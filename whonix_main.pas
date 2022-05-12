(*
 * Whonix Starter ( whonix_main.pas )
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

unit Whonix_Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Process,
  WhonixAppConfig, WhonixUtils;

type

  { TMainForm }

  TMainForm = class(TForm)
    ButtonStartStop: TButton;
    ButtonAdvanced: TButton;
    procedure ButtonAdvancedClick(Sender: TObject);
    procedure ButtonStartStopClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  MainForm: TMainForm;

implementation

uses Whonix_Error, Whonix_License, Whonix_Status;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormShow(Sender: TObject);
var
  ova_path: string;
  Output: TStringList;
begin
  if AppConfig.ShowLicense then
  begin
    LicenseForm.ShowModal;
    if AppConfig.ShowLicense then
    begin
      Halt;
    end;
    SaveWhonixAppConfig;
  end;

  StatusForm.Show;

  {$IFDEF WINDOWS}
  if (AppConfig.VirtualBoxPath = '') or (AppConfig.VBoxManagePath = '') then
  begin
    StatusForm.NextStatus('step 1/4: execute virtualbox installer');
    RunAsAdmin(MainForm.Handle, ExtractFilePath(Application.ExeName) +
      'vbox.exe', '--silent --ignore-reboot', StatusForm.MemoOutput.Lines);
    //Execute('cmd.exe /c ""' + ExtractFilePath(Application.ExeName) +
    //  'vbox.exe"" --silent --ignore-reboot', StatusForm.MemoOutput.Lines);

    StatusForm.NextStatus('step 2/4: reload whonix application config');
    LoadWhonixAppConfig;
  end;
  {$ENDIF}

  if (AppConfig.VirtualBoxPath = '') or (AppConfig.VBoxManagePath = '') then
  begin
    StatusForm.Hide;
    ErrorForm.MemoError.Lines.Text := 'VirtualBox or VBoxManage not found';
    ErrorForm.ShowModal;
  end;

  StatusForm.NextStatus('step 3/4: install whonix gateway and workstation');
  Output := TStringList.Create;
  Execute(AppConfig.VBoxManagePath + ' list vms', Output);
  StatusForm.MemoOutput.Lines.AddStrings(Output);

  ova_path := ExtractFilePath(Application.ExeName) + 'Whonix.ova';
  if FileExists(ova_path) and not ContainsStr(Output.Text, 'Whonix-Gateway-XFCE') and
    not ContainsStr(Output.Text, 'Whonix-Workstation-XFCE') then
  begin
    Execute(AppConfig.VBoxManagePath + ' import "' + ova_path +
      '" --vsys 0 --eula accept --vsys 1 --eula accept', StatusForm.MemoOutput.Lines);
  end;

  {$IFDEF WINDOWS}
  if FileExists(AppConfig.MsiInstallerPath)
    and (ExtractFileName(AppConfig.MsiInstallerPath) = 'Whonix.msi') then
  begin
    StatusForm.NextStatus('step 4/4: remove whonix ova from install dir');
    Execute('msiexec /i "' + AppConfig.MsiInstallerPath + '" REMOVE="Data"',
      StatusForm.MemoOutput.Lines);
  end;
  {$ENDIF}

  Output.Free;

  StatusForm.Hide;
end;

procedure TMainForm.ButtonAdvancedClick(Sender: TObject);
var
  Process: TProcess;
begin
  if not FileExists(AppConfig.VirtualBoxPath) then
  begin
    ErrorForm.MemoError.Lines.Text := 'binary VirtualBox not found';
    ErrorForm.ShowModal;
  end;

  Process := TProcess.Create(nil);
  Process.CommandLine := AppConfig.VirtualBoxPath;
  Process.Execute;
  Process.Free;
end;

procedure TMainForm.ButtonStartStopClick(Sender: TObject);
var
  ProcessA, ProcessB: TProcess;
begin
  if not FileExists(AppConfig.VBoxManagePath) then
  begin
    ErrorForm.MemoError.Lines.Text := 'binary VBoxManage not found';
    ErrorForm.ShowModal;
  end;

  if (ButtonStartStop.Caption = 'Start Whonix') then
  begin
    ProcessA := TProcess.Create(nil);
    ProcessA.CommandLine := AppConfig.VBoxManagePath +
      ' startvm Whonix-Workstation-XFCE';
    ProcessA.Execute;
    ProcessB := TProcess.Create(nil);
    ProcessB.CommandLine := AppConfig.VBoxManagePath +
      ' startvm Whonix-Gateway-XFCE';
    ProcessB.Execute;
    ButtonStartStop.Caption := 'Stop Whonix';
  end
  else
  if (ButtonStartStop.Caption = 'Stop Whonix') then
  begin
    ProcessA := TProcess.Create(nil);
    ProcessA.CommandLine := AppConfig.VBoxManagePath +
      ' controlvm Whonix-Workstation-XFCE poweroff';
    ProcessA.Execute;
    ProcessB := TProcess.Create(nil);
    ProcessB.CommandLine := AppConfig.VBoxManagePath +
      ' controlvm Whonix-Gateway-XFCE poweroff';
    ProcessB.Execute;
    ButtonStartStop.Caption := 'Start Whonix';
  end;
end;

end.
