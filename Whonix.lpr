(*
 * Whonix Starter ( Whonix.lpr )
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

program Whonix;

{$mode objfpc}{$H+}

uses
 {$IFDEF UNIX}
  cthreads,
       {$ENDIF} {$IFDEF HASAMIGA}
  athreads,
       {$ENDIF}
  SysUtils,
  Interfaces, // this includes the LCL widgetset
  Forms,
  Dialogs,
  Controls,
  Process,
  Whonix_Main,
  Whonix_Error,
  Whonix_License,
  Whonix_Status,
  WhonixAppConfig { you can add units after this };

{$R *.res}

var
  i: integer;
  msi_path: string = '';
begin
  if Application.ParamCount > 0 then
  begin
    for i := 1 to Application.ParamCount do
    begin
      msi_path := msi_path + ' ' + Application.Params[i];
    end;
    AppConfig.MsiInstallerPath := TrimLeft(msi_path);
  end;

  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TErrorForm, ErrorForm);
  Application.CreateForm(TLicenseForm, LicenseForm);
  Application.CreateForm(TStatusForm, StatusForm);
  Application.Run;
end.
