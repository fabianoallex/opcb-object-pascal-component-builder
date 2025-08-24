unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Populators, Vcl.ExtCtrls, Vcl.StdCtrls;

type

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    FButtonLogin: TButton;
    FEditPassword: TEdit;
    FCheckBoxConnected: TCheckBox;
    FEditEmail: TEdit;
    procedure SetupBevel(AControl: TControl);
    procedure SetupLabelLogin(AControl: TControl);
    procedure SetupSenha(AControl: TControl);
    procedure SetupLoginButton(AControl: TControl);
    procedure PopulateLoginControls(Populator: TControlPopulator);
    procedure SetButtonLogin(const Value: TButton);
    procedure SetCheckBoxConnected(const Value: TCheckBox);
    procedure SetEditEmail(const Value: TEdit);
    procedure SetEditPassword(const Value: TEdit);
    procedure LoginButtonClick(ASender: TObject);
    procedure SetupCheckBox(AControl: TControl);
  public
    property EditEmail: TEdit read FEditEmail write SetEditEmail;
    property EditPassword: TEdit read FEditPassword write SetEditPassword;
    property ButtonLogin: TButton read FButtonLogin write SetButtonLogin;
    property CheckBoxConnected: TCheckBox read FCheckBoxConnected write SetCheckBoxConnected;
  end;

var
  Form1: TForm1;

implementation


{$R *.dfm}

procedure TForm1.SetButtonLogin(const Value: TButton);
begin
  FButtonLogin := Value;
  FButtonLogin.OnClick := LoginButtonClick;
end;

procedure TForm1.SetCheckBoxConnected(const Value: TCheckBox);
begin
  FCheckBoxConnected := Value;
end;

procedure TForm1.SetEditEmail(const Value: TEdit);
begin
  FEditEmail := Value;
end;

procedure TForm1.SetEditPassword(const Value: TEdit);
begin
  FEditPassword := Value;
end;

procedure TForm1.SetupBevel(AControl: TControl);
var
  Bevel: TBevel;
begin
  Bevel := TBevel(AControl);
  Bevel.Shape := bsBottomLine;
  Bevel.Height := 5;
  Bevel.Width := 250;
end;

procedure TForm1.SetupLabelLogin(AControl: TControl);
var
  Lbl: TLabel;
begin
  Lbl := TLabel(AControl);
  lbl.Caption := 'Login';
  Lbl.Font.Size := 25;
end;

procedure TForm1.SetupSenha(AControl: TControl);
var
  Edit: TEdit;
begin
  Edit := TEdit(AControl);
  Edit.PasswordChar := '*';
end;

procedure TForm1.SetupLoginButton(AControl: TControl);
var
  Button: TButton;
begin
  Button := TButton(AControl);
  Button.Width := 250;
  Button.Height := 70;
  Button.Caption := 'Logar';
end;

procedure TForm1.SetupCheckBox(AControl: TControl);
var
  CheckBox: TCheckBox;
  CaptionWidth: Integer;
begin
  CheckBox := TCheckBox(AControl);
  CheckBox.Caption := 'Manter Conectado';

  CaptionWidth := Canvas.TextWidth(CheckBox.Caption);
  CheckBox.Width := CaptionWidth + 20;
end;

procedure TForm1.PopulateLoginControls(Populator: TControlPopulator);
var
  CILabel: TControlInfo;
  CIEdit: TControlInfo;
begin
  CILabel := TControlInfo.Create(TLabel);
  CIEdit := TControlInfo.Create(TEdit).WithWidth(250);

  Populator
    .NextLevel(TControlInfo.Create(TPanel, 'PanelLogin').WithCaption(''), cpdVertical)
      .SetTopLeft(10, 10)
      .AddControl(CILabel.Setup(SetupLabelLogin).WithName('LabelLogin'))
      .IncTop(20)
      .AddControl(TControlInfo.Create(TBevel).Setup(SetupBevel))
      .IncTop(20)
      .AddControl(CILabel.WithCaption('e-mail'))
      .AddControl(CIEdit.WithName('EditEmail').WithText(''))
      .AddControl(CILabel.WithCaption('Senha'))
      .AddControl(CIEdit.WithName('EditPassword').WithText('').Setup(SetupSenha))
      .AddControl(TControlInfo.Create(TCheckBox, 'CheckBoxConnected').Setup(SetupCheckBox))
      .IncTop(20)
      .AddControl(TControlInfo.Create(TButton).WithName('LoginButton').Setup(SetupLoginButton))
      .NextLevel(
        TControlInfo.Create(TPanel, 'PanelRodape')
          .WithAlign(alBottom)
          .WithHeight(100)
          .WithCaption(''),
          cpdHorizontal
        )
         .AddControl(CILabel.WithName('LabelConta').WithCaption('Ainda não tem conta?'))
         .AddControl(TControlInfo.Create(TButton, 'ButtonConta').WithCaption('Cadastra-se'))
      .PreviousLevel

      .RecalcParentSize(20, 10)
      .CenterControlsInParentHorizontally(['LabelLogin'])
      .CenterControlsVertically(['LabelConta'], ['ButtonConta'])
      .CenterControlsInParentVertically(['LabelConta', 'ButtonConta'])
      .CenterControlsInParentHorizontally(['LabelConta', 'ButtonConta'])
      .AlignControlsRight(['CheckBoxConnected'], ['EditPassword'])
    .PreviousLevel
  ;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  Populator: TControlPopulator;
begin
  Populator := TControlPopulator.Create(Self.Name);

  try
    Populator
      .WithOwnerAndParent(Self, Self)
      .SetTopLeft(10, 10)
      .SetSpace(5, 5)
    ;

    PopulateLoginControls(Populator);

    Populator
      .AddControl(TControlInfo.Create(TPanel, 'PanelRight'))
      .CopySize(['PanelRight'], ['PanelLogin'])
    ;

    Form1.ButtonLogin := TButton(Populator.NamedControls['LoginButton']);
    Form1.EditPassword := TEdit(Populator.NamedControls['EditPassword']);
    Form1.EditEmail := TEdit(Populator.NamedControls['EditEmail']);
    Form1.CheckBoxConnected := TCheckBox(Populator.NamedControls['CheckBoxConnected']);
  finally
    Populator.Free;
  end;
end;

procedure TForm1.LoginButtonClick(ASender: TObject);
begin
  Showmessage('Clicou Login: ' + EditEmail.Text + ' / ' + EditPassword.Text);
end;

end.
