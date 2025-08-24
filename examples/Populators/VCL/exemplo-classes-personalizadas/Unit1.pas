unit Unit1;

interface

uses
  ulayout.controls,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    procedure SetupAutoClickButton(AControl: TControl);
    procedure AutoClickButtonClick(ASender: TObject);
  end;

var
  Form1: TForm1;

implementation

uses
  Vcl.ExtCtrls, AutoClickButton, Vcl.StdCtrls;

{$R *.dfm}

procedure TForm1.AutoClickButtonClick(ASender: TObject);
begin
  showmessage('Eu sei clicar sozinho');
end;

procedure TForm1.SetupAutoClickButton(AControl: TControl);
var
  AutoClickButton: TAutoClickButton;
begin
  AutoClickButton := TAutoClickButton(AControl);
  AutoClickButton.OnClick := AutoClickButtonClick;
  AutoClickButton.StartDelay := 3000;
  AutoClickButton.Countdown := 10;
  AutoClickButton.Caption := 'Opção 3';
  AutoClickButton.Reset;
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
      .AddControl(TControlInfo.Create(TButton).WithCaption('Opção 1'))
      .AddControl(TControlInfo.Create(TButton).WithCaption('Opção 2'))
      .AddControl(TControlInfo.Create(TAutoClickButton).Setup(SetupAutoClickButton))
    ;

  finally
    Populator.Free;
  end;
end;

end.
