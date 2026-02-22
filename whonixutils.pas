{
  Copyright: 2026 - 2026 ENCRYPTED SUPPORT LLC <adrelanos@whonix.org>
  See the file COPYING for copying conditions.
}

unit WhonixUtils;

{$mode ObjFPC}{$H+}
{$ZeroBasedStrings On}

interface

uses
  Classes, SysUtils, Process;

function VMNameFromLine(VMLine: String) : String;
function RunCaptureCommand(ProgramName: String;
  Parameters: array of String; out CmdResult: String): Boolean;

implementation

function VMNameFromLine(VMLine: String) : String;
var
  LineIndex: Integer;
  EndSpaceIndex: Integer;
begin
  EndSpaceIndex := -1;
  for LineIndex := 0 to Length(VMLine) - 1 do
  begin
    if VMLine[LineIndex] = ' ' then
    begin
      EndSpaceIndex := LineIndex
    end;
  end;

  {
    There must be at least three characters before the end space, to accomodate
    two double quotes and a one-character VM name.
  }

  if EndSpaceIndex < 3 then
  begin
    VMNameFromLine := '';
    Exit;
  end;

  VMNameFromLine := VMLine.Substring(1, EndSpaceIndex - 2);
end;

{
  We need to run VBoxManage commands in order to do our job. In a perfect world,
  we could simply use the RunCommand() function built into Free Pascal to do
  this. Unfortunately, Free Pascal 3.2.2's RunCommand() function has a bug, and
  is unable to capture the standard output of an executed application under
  Windows, although it works fine on Linux. See
  https://forum.lazarus.freepascal.org/index.php/topic,73508.msg576725.html#msg576725.
  Debian 13 ships with Free Pascal 3.2.2, so we're stuck with it.

  The other way of doing this is to execute applications asynchronously using
  TProcess, gather up their output into a stream, and then convert that stream
  to a string at the end. This solution works quite well under Windows, but on
  Linux, it inexplicably hangs for multiple seconds before returning, and there
  doesn't seem to be a straightforward way to prevent this. See
  https://forum.lazarus.freepascal.org/index.php/topic,73508.msg576725.html#msg576725.

  To make sure both Linux and Windows work, abstract both methods away and use
  the working method for each platform. Hopefully we can get rid of this kludge
  once Debian 14 releases.
}

{$ifdef windows}
function RunCaptureCommand(ProgramName: String;
  Parameters: array of String; out CmdResult: String): Boolean;
type
  T4KReadBuf = array[0..4095] of Byte;
var
  CmdProcess: TProcess;
  CmdParam: String;
  CmdReadBuf: T4KReadBuf;
  CmdReadLen: Integer;
  CmdOutput: TStringStream;
begin
  CmdReadBuf := Default(T4KReadBuf);
  CmdProcess := TProcess.Create(nil);
  CmdProcess.Executable := ProgramName;
  with CmdProcess.Parameters do
  begin
    for CmdParam in Parameters do
    begin
      Add(CmdParam);
    end;
  end;
  CmdProcess.Options := [poUsePipes, poNoConsole];
  try
    CmdProcess.Execute;
  except
    CmdProcess.Free;
    RunCaptureCommand := False;
    Exit;
  end;

  CmdOutput := TStringStream.Create;
  repeat
    CmdReadLen := CmdProcess.Output.Read(CmdReadBuf, 4096);
    CmdOutput.Write(CmdReadBuf, CmdReadLen);
  until CmdReadLen = 0;
  CmdResult := CmdOutput.DataString;

  if CmdProcess.ExitCode = 0 then RunCaptureCommand := True
  else RunCaptureCommand := False;
  CmdProcess.Free;
  CmdOutput.Free;
end;
{$else}
function RunCaptureCommand(ProgramName: String;
  Parameters: array of String; out CmdResult: String): Boolean;
begin
  RunCaptureCommand := RunCommand(ProgramName, Parameters, CmdResult,
    [poNoConsole]);
end;
{$endif}

end.

