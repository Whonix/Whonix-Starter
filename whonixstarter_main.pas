(*
 * Whonix Starter ( whonixstarter_main.pas )
 *
 * Copyright: 2012 - 2023 ENCRYPTED SUPPORT LP <adrelanos@riseup.net>
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

unit WhonixStarter_Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, Process, WhonixStarterAppConfig;

type

  { TMainForm }

  TMainForm = class(TForm)
    ButtonStartStop: TButton;
    ButtonAdvanced: TButton;
    procedure ButtonAdvancedClick(Sender: TObject);
    procedure ButtonStartStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  MainForm: TMainForm;

implementation

uses WhonixStarter_Error;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.ButtonAdvancedClick(Sender: TObject);
var
  Process: TProcess;
begin
  if not FileExists(AppConfig.VirtualBoxPath) then
  begin
    ErrorForm.MemoError.Lines.Text := 'binary "VirtualBox" not found';
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
    ErrorForm.MemoError.Lines.Text := 'binary "VBoxManage" not found';
    ErrorForm.ShowModal;
  end;

  if (ButtonStartStop.Caption = 'Start Whonix') then
  begin
    ProcessA := TProcess.Create(nil);
    ProcessA.CommandLine := AppConfig.VBoxManagePath +
      ' startvm Whonix-Workstation-Xfce';
    ProcessA.Execute;
    ProcessB := TProcess.Create(nil);
    ProcessB.CommandLine := AppConfig.VBoxManagePath +
      ' startvm Whonix-Gateway-Xfce';
    ProcessB.Execute;
    ButtonStartStop.Caption := 'Stop Whonix';
  end
  else
  if (ButtonStartStop.Caption = 'Stop Whonix') then
  begin
    ProcessA := TProcess.Create(nil);
    ProcessA.CommandLine := AppConfig.VBoxManagePath +
      ' controlvm Whonix-Workstation-Xfce poweroff';
    ProcessA.Execute;
    ProcessB := TProcess.Create(nil);
    ProcessB.CommandLine := AppConfig.VBoxManagePath +
      ' controlvm Whonix-Gateway-Xfce poweroff';
    ProcessB.Execute;
    ButtonStartStop.Caption := 'Start Whonix';
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  MainForm.Icon.LoadFromResourceName(Hinstance, 'MAINICON');
end;

end.
