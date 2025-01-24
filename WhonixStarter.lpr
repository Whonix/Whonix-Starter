(*
 * Whonix Starter ( WhonixStarter.lpr )
 *
 * Copyright: 2012 - 2025 ENCRYPTED SUPPORT LLC <adrelanos@riseup.net>
 * Author: einsiedler90@protonmail.com
 * License: See the file COPYING for copying conditions.
 *)

program WhonixStarter;

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
  WhonixStarter_Main,
  WhonixStarter_Error;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TErrorForm, ErrorForm);
  Application.Run;
end.
