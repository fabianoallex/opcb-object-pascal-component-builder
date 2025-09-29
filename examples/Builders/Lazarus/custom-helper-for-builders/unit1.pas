unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, OPCB,
  OPCB.Builders;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

  { TButtonBuilderHelper }

  { TFontSizeSetup }

  TFontSizeSetup = class
  public
    Size: Integer;
    procedure Apply(AControl: TControl);
  end;

  { TButtonVisibleSetup }

  TButtonVisibleSetup = class
    Visible: Boolean;
    procedure Apply(AControl: TControl);
  end;

  TButtonBuilderHelper = class helper for TButtonBuilder
  public
    function WithFontSize(ASize: Integer): TButtonBuilder;
    function WithVisible(AVisible: Boolean): TButtonBuilder;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  Creator: TControlCreator;
  B: TButton;
begin
  Creator := TControlCreator.Create;
  try
    Creator
      .WithOwnerAndParent(Self, Self)
      .GridInit(2, 2)
        .GridSetCellWidthAndHeight(250, 60)
        .AddControl(TButtonBuilder.Create(TButton, B).WithCaption('Teste').WithFontSize(22))
        .AddControl(TButtonBuilder.Create.WithCaption('Teste 2').WithEnabled(False))
        .AddControl(TButtonBuilder.Create.WithCaption('Teste 3').WithVisible(False))
        .AddControl(TButtonBuilder.Create.WithCaption('Teste 4'))
      .GridFinish
    ;
  finally
    Creator.Free;
  end;
end;

{ TFontSizeSetup }

procedure TFontSizeSetup.Apply(AControl: TControl);
begin
  TButton(AControl).Font.Size := Size;
  Self.Free;
end;

{ TButtonVisibleSetup }

procedure TButtonVisibleSetup.Apply(AControl: TControl);
begin
  TButton(AControl).Visible := Visible;
  Self.Free;
end;

{ TButtonBuilderHelper }

function TButtonBuilderHelper.WithFontSize(ASize: Integer): TButtonBuilder;
var
  FS: TFontSizeSetup;
begin
  // como no lazarus não temos opção de procedures anonimas
  // é necessario criar uma classe e instanciar um objeto que
  // guarde o valor da propriedade a ser setada
  Result := Self;
  FS := TFontSizeSetup.Create;
  FS.Size := ASize;
  Setup(@FS.Apply);
end;

function TButtonBuilderHelper.WithVisible(AVisible: Boolean): TButtonBuilder;
var
  SetupObj: TButtonVisibleSetup;
begin
  // como no lazarus não temos opção de procedures anonimas
  // é necessario criar uma classe e instanciar um objeto que
  // guarde o valor da propriedade a ser setada
  Result := Self;
  SetupObj := TButtonVisibleSetup.Create;
  SetupObj.Visible := AVisible;
  Setup(@SetupObj.Apply);
end;

end.

