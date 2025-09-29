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

  TFontSizeSetup = class(specialize TPropertySetup<Integer>)
  public
    procedure Apply(AControl: TControl); override;
  end;

  TButtonVisibleSetup = class(specialize TPropertySetup<Boolean>)
    procedure Apply(AControl: TControl); override;
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
        .AddControl(TButtonBuilder.Create.WithCaption('Teste 4').WithFontSize(20))
      .GridFinish
    ;
  finally
    Creator.Free;
  end;
end;

{ TFontSizeSetup }

procedure TFontSizeSetup.Apply(AControl: TControl);
begin
  TButton(AControl).Font.Size := Self.FValue;
  Self.Free;
end;

{ TButtonVisibleSetup }

procedure TButtonVisibleSetup.Apply(AControl: TControl);
begin
  TButton(AControl).Visible := FValue;
  Self.Free;
end;

{ TButtonBuilderHelper }

function TButtonBuilderHelper.WithFontSize(ASize: Integer): TButtonBuilder;
begin
  Result := Self;
  with TFontSizeSetup.Create(ASize) do Setup(@Apply);
end;

function TButtonBuilderHelper.WithVisible(AVisible: Boolean): TButtonBuilder;
begin
  Result := Self;
  with TButtonVisibleSetup.Create(AVisible) do Setup(@Apply);
end;

end.

