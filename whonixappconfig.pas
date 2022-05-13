(*
 * Whonix Starter ( whonixappconfig.pas )
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

unit WhonixAppConfig;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, WhonixUtils;

type
  TAppConfig = record
    ShowLicense: boolean;
    VirtualBoxPath: string;
    VBoxManagePath: string;
    MsiInstallerPath: string;
  end;

procedure LoadWhonixAppConfig;
procedure SaveWhonixAppConfig;

var
  AppConfig: TAppConfig;

implementation

uses IniFiles;

var
  ini: TIniFile;

procedure EnsureValidExePath(var TargetPath: string; DefaultPath: string);
var
  filename: string;
  sl: TStringList;
begin
  if FileExists(TargetPath) then
  begin
    Exit;
  end;

  if (TargetPath <> DefaultPath) and FileExists(DefaultPath) then
  begin
    TargetPath := DefaultPath;
    Exit;
  end;

  filename := ExtractFileName(DefaultPath);
  TargetPath := FindDefaultExecutablePath(filename);
  if FileExists(TargetPath) then
  begin
    Exit;
  end;

  sl := TStringList.Create;
  {$IFDEF WINDOWS}
  Execute('where /r C:\ ' + filename, sl);
  {$ELSE}
  Execute('which ' + filename, sl);
  {$ENDIF}

  if (sl.Count > 0) and FileExists(sl.Strings[0]) then
  begin
    TargetPath := sl.Strings[0];
  end
  else
  begin
    TargetPath := '';
  end;

  sl.Free;
end;

const
  {$IFDEF WINDOWS}
  defaultVirtualBoxPath = 'C:\Program Files\Oracle\VirtualBox\VirtualBox.exe';
  defaultVBoxManagePath = 'C:\Program Files\Oracle\VirtualBox\VBoxManage.exe';
  {$ELSE}
  defaultVirtualBoxPath = '/usr/bin/VirtualBox';
  defaultVBoxManagePath = '/usr/bin/VBoxManage';
  {$ENDIF}

procedure LoadWhonixAppConfig;
begin
  AppConfig.ShowLicense := ini.ReadBool('AppConfig', 'ShowLicense', True);
  AppConfig.VirtualBoxPath := ini.ReadString('AppConfig', 'VirtualBoxPath',
    defaultVirtualBoxPath);
  AppConfig.VBoxManagePath := ini.ReadString('AppConfig', 'VBoxManagePath',
    defaultVBoxManagePath);

  EnsureValidExePath(AppConfig.VirtualBoxPath, defaultVirtualBoxPath);
  EnsureValidExePath(AppConfig.VBoxManagePath, defaultVBoxManagePath);
end;

procedure SaveWhonixAppConfig;
begin
  ini.WriteBool('AppConfig', 'ShowLicense', AppConfig.ShowLicense);
  ini.WriteString('AppConfig', 'VirtualBoxPath', AppConfig.VirtualBoxPath);
  ini.WriteString('AppConfig', 'VBoxManagePath', AppConfig.VBoxManagePath);
end;

initialization
  ini := TIniFile.Create(GetAppConfigDir(False) + 'Whonix.ini');
  LoadWhonixAppConfig;

finalization
  SaveWhonixAppConfig;
  ini.Free;

end.
