(*
 * Whonix Starter ( whonix_status.pas )
 *
 * Copyright: 2012 - 2022 ENCRYPTED SUPPORT LP <adrelanos@riseup.net>
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

unit Whonix_Status;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  Arrow, ExtCtrls;

type

  { TStatusForm }

  TStatusForm = class(TForm)
    ButtonClose: TButton;
    CheckBoxOutput: TCheckBox;
    MemoOutput: TMemo;
    PanelStatus: TPanel;
    ProgressBar: TProgressBar;
    procedure ButtonCloseClick(Sender: TObject);
    procedure CheckBoxOutputChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public
    procedure NextStatus(Status: string; Output: TStrings = nil);
  end;

var
  StatusForm: TStatusForm;

implementation

{$R *.lfm}

{ TStatusForm }

procedure TStatusForm.CheckBoxOutputChange(Sender: TObject);
begin
  if CheckBoxOutput.Checked then
  begin
    StatusForm.Height := 500;
    MemoOutput.Show;
  end
  else
  begin
    MemoOutput.Hide;
    StatusForm.Height := CheckBoxOutput.Top + CheckBoxOutput.Height + 10;
  end;
end;

procedure TStatusForm.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TStatusForm.FormCreate(Sender: TObject);
begin
  StatusForm.Icon.LoadFromResourceName(Hinstance,'MAINICON');
  MemoOutput.Hide;
  StatusForm.Height := CheckBoxOutput.Top + CheckBoxOutput.Height + 10;
end;

procedure TStatusForm.NextStatus(Status: string; Output: TStrings = nil);
var
  i: integer;
begin
  if not StatusForm.Showing then
  begin
    StatusForm.Show;
  end;

  PanelStatus.Caption := 'Status: ' + Status;
  MemoOutput.Append('Status:' + Status);

  if Output <> nil then
  begin
    for i := 0 to Output.Count - 1 do
    begin
      MemoOutput.Append(Output.Strings[i]);
    end;
  end;

  MemoOutput.Lines.SaveToFile(GetAppConfigDir(False) + 'Whonix.log');

  // wait 2 seconds to make status reading possible
  for i := 1 to 20 do
  begin
    Sleep(100);
    Application.ProcessMessages;
  end;
end;

end.
