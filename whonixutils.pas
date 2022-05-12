(*
 * Whonix Starter ( whonixutils.pas )
 *
 * Copyright: 2012 - 2019 ENCRYPTED SUPPORT LP <adrelanos@riseup.net>
 * Author: einsiedler90@protonmail.com
 * License: GPL-3+-with-additional-terms-1
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * .
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * .
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *)

unit WhonixUtils;

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF WINDOWS}
    Windows, ShellApi,
  {$ENDIF}

  Classes, SysUtils, Forms, Process;

{$IFDEF WINDOWS}
procedure RunAsAdmin(const Handle: Hwnd; const Path, Params: string; Output: TStrings = nil);
{$ENDIF}

procedure Execute(CommandLine: String; Output: TStrings = nil);

implementation

{$IFDEF WINDOWS}
procedure RunAsAdmin(const Handle: Hwnd; const Path, Params: string; Output: TStrings = nil);
var
  sei: TShellExecuteInfoA;
begin
  FillChar(sei, SizeOf(sei), 0);
  sei.cbSize := SizeOf(sei);
  sei.Wnd := 0; // Handle
  sei.fMask := SEE_MASK_NOCLOSEPROCESS; //SEE_MASK_FLAG_DDEWAIT or SEE_MASK_FLAG_NO_UI;
  sei.lpVerb := 'runas';
  sei.lpFile := PAnsiChar(Path);
  sei.lpParameters := PAnsiChar(Params);
  sei.nShow := SW_SHOW; //SW_SHOWNORMAL;
  sei.hInstApp := 0;

  if Output <> nil then begin
    Output.Append('Execute: ' + sei.lpFile + ' ' + sei.lpParameters);
  end;

  if ShellExecuteExA(@sei) then begin
    while WaitForSingleObject(sei.hProcess, 10) <> 0 do begin // INFINITE
      Application.ProcessMessages;
    end;
    CloseHandle(sei.hProcess);
  end;
end;
{$ENDIF}

procedure Execute(CommandLine: String; Output: TStrings = nil);
var
  Process: TProcess;
  StrStream : TStringStream;
begin
  Process := TProcess.Create(nil);
  Process.CommandLine := CommandLine;
  Process.Options := Process.Options + [poNoConsole];

  if Output <> nil then begin
    Process.Options := Process.Options + [poUsePipes, poStderrToOutPut];
    Output.Append('Execute: ' + Process.CommandLine);
  end;

  Process.Execute;

  StrStream := TStringStream.Create;
  repeat
    Sleep(10);
    Application.ProcessMessages;

    while (Process.Output.NumBytesAvailable > 0) do
    begin
      StrStream.WriteByte(Process.Output.ReadByte);
    end;
  until not Process.Running;

  if Output <> nil then begin
    Output.Append(StrStream.DataString);
  end;

  StrStream.Free;
  Process.Free;
end;

end.

