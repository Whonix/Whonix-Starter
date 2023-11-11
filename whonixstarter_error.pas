(*
 * Whonix Starter ( whonixstarter_error.pas )
 *
 * Copyright: 2012 - 2023 ENCRYPTED SUPPORT LP <adrelanos@riseup.net>
 * Author: einsiedler90@protonmail.com
 * License: See the file COPYING for copying conditions.

unit WhonixStarter_Error;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TErrorForm }

  TErrorForm = class(TForm)
    ButtonErrorOK: TButton;
    MemoError: TMemo;
    procedure ButtonErrorOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
  private

  public

  end;

var
  ErrorForm: TErrorForm;

implementation

{$R *.lfm}

{ TErrorForm }

procedure TErrorForm.ButtonErrorOKClick(Sender: TObject);
begin
  Halt;
end;

procedure TErrorForm.FormCreate(Sender: TObject);
begin
  ErrorForm.Icon.LoadFromResourceName(Hinstance, 'MAINICON');
end;

procedure TErrorForm.FormHide(Sender: TObject);
begin
  Halt;
end;

end.
