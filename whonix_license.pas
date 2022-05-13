(*
 * Whonix Starter ( whonix_license.pas )
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

unit Whonix_License;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  WhonixAppConfig;

type

  { TLicenseForm }

  TLicenseForm = class(TForm)
    ButtonDecline: TButton;
    ButtonAccept: TButton;
    MemoLicense: TMemo;
    Panel: TPanel;
    procedure ButtonAcceptClick(Sender: TObject);
    procedure ButtonDeclineClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  LicenseForm: TLicenseForm;

implementation

{$R *.lfm}

{ TLicenseForm }

procedure TLicenseForm.FormCreate(Sender: TObject);
begin
  MemoLicense.Lines.LoadFromFile(ExtractFilePath(Application.ExeName) + 'license.txt');
end;

procedure TLicenseForm.ButtonDeclineClick(Sender: TObject);
begin
  LicenseForm.Close;
end;

procedure TLicenseForm.ButtonAcceptClick(Sender: TObject);
begin
  AppConfig.ShowLicense := False;
  LicenseForm.Close;
end;

end.
