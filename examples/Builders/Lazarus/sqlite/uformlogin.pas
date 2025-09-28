unit UFormLogin;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, OPCB, StdCtrls, Buttons, Controls;

type

  { TFormLogin }

  TFormLogin = class(TForm)
  private
    FEditPassword: TEdit;
    FEditUserName: TEdit;
    procedure SetupButtons(AControl: TControl);
    procedure SetupEditPassword(AControl: TControl);
  public
    constructor CreateNew(AOwner: TComponent);
    property EditUserName: TEdit read FEditUserName;
    property EditPassword: TEdit read FEditPassword;
  end;

procedure CallFormLogin;

implementation

uses
  UUserSession, Dialogs;

procedure CallFormLogin;
var
  Form: TFormLogin;
begin
  Form := TFormLogin.CreateNew(nil);
  try
    while Form.ShowModal = mrOk do
      if not UserSession.Login(Form.EditUserName.Text, Form.EditPassword.Text) then
        MessageDlg('Senha inválida.', mtError, [mbOk], 0)
      else
        break;
  finally
    Form.Free;
  end;
end;

{ TFormLogin }

constructor TFormLogin.CreateNew(AOwner: TComponent);
var
  ControlBuilder: TControlsBuilder;
begin
  inherited CreateNew(AOwner, 0);
  Self.Name := 'FormLogin';
  Self.Caption := 'Login';
  Self.BorderStyle := bsDialog;
  Self.Position := poMainFormCenter;

  ControlBuilder := TControlsBuilder.Create(Self.Name);
  try
    ControlBuilder
      .WithOwnerAndParent(Self, Self)
      .SetTopLeft(10, 10)
      .SetSpace(5, 5)
      .SetDirection(cpdVertical)
      .AddControl(TControlBuilder.Create(TLabel).WithCaption('Nome de usuário'))
      .AddControl(TControlBuilder.Create(TEdit, 'EditUserName', FEditUserName).WithText('').WithWidth(250))
      .AddControl(TControlBuilder.Create(TLabel).WithCaption('Senha'))
      .AddControl(TControlBuilder.Create(TEdit, 'EditPassword', FEditPassword).WithText('').WithWidth(250).Setup(@SetupEditPassword))
      .SetDirection(cpdHorizontal)
      .IncTop(20)
      .AddControl(TControlBuilder.Create(TBitBtn, 'ButtonOk').WithWidth(75).Setup(@SetupButtons))
      .AddControl(TControlBuilder.Create(TBitBtn, 'ButtonCancel').WithWidth(75).Setup(@SetupButtons))
      .AlignControlsRight(['ButtonOk', 'ButtonCancel'], ['EditUserName', 'EditPassword'])
    ;

    Self.Width := Trunc(ControlBuilder.ContentWidth) + 20;
    Self.Height := Trunc(ControlBuilder.ContentHeight) + 20;
  finally
    ControlBuilder.Free;
  end;
end;

procedure TFormLogin.SetupEditPassword(AControl: TControl);
begin
  (AControl as TEdit).PasswordChar := '*';
end;

procedure TFormLogin.SetupButtons(AControl: TControl);
var
  BitBtn: TBitBtn;
begin
  BitBtn := (AControl as TBitBtn);
  if BitBtn.Name = 'ButtonOk' then
    BitBtn.Kind := bkOK;
  if BitBtn.Name = 'ButtonCancel' then
    BitBtn.Kind := bkCancel;
end;

end.

