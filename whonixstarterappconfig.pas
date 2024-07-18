(*
 * Whonix Starter ( whonixstarterappconfig.pas )
 *
 * Copyright: 2012 - 2023 ENCRYPTED SUPPORT LP <adrelanos@riseup.net>
 * Author: einsiedler90@protonmail.com
 * License: See the file COPYING for copying conditions.
 *)

unit WhonixStarterAppConfig;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, WhonixUtils;

type
  TAppConfig = record
    VirtualBoxPath: string;
    VBoxManagePath: string;
  end;

procedure LoadWhonixStarterAppConfig;
procedure SaveWhonixStarterAppConfig;

var
  AppConfig: TAppConfig;

implementation

uses IniFiles;

var
  ini: TIniFile;

const
  {$IFDEF WINDOWS}
  defaultVirtualBoxPath = 'C:\Program Files\Oracle\VirtualBox\VirtualBox.exe';
  defaultVBoxManagePath = 'C:\Program Files\Oracle\VirtualBox\VBoxManage.exe';
  {$ELSE}
  defaultVirtualBoxPath = '/usr/bin/VirtualBox';
  defaultVBoxManagePath = '/usr/bin/VBoxManage';

  {$ENDIF}

procedure LoadWhonixStarterAppConfig;
begin
  AppConfig.VirtualBoxPath := ini.ReadString('AppConfig', 'VirtualBoxPath',
    defaultVirtualBoxPath);
  AppConfig.VBoxManagePath := ini.ReadString('AppConfig', 'VBoxManagePath',
    defaultVBoxManagePath);

  EnsureValidExePath(AppConfig.VirtualBoxPath, defaultVirtualBoxPath);
  EnsureValidExePath(AppConfig.VBoxManagePath, defaultVBoxManagePath);
end;

procedure SaveWhonixStarterAppConfig;
begin
  ini.WriteString('AppConfig', 'VirtualBoxPath', AppConfig.VirtualBoxPath);
  ini.WriteString('AppConfig', 'VBoxManagePath', AppConfig.VBoxManagePath);
end;

initialization
  ini := TIniFile.Create(GetAppConfigDir(False) + 'Whonix.ini');
  LoadWhonixStarterAppConfig;

finalization
  SaveWhonixStarterAppConfig;
  ini.Free;

end.
