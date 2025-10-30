unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  ExtCtrls, OPCB, OPCB.Builders;

type

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

  { TButtonBuilderHelper }

  { TFontSizeSetup }

  TFontSizeSetup = class(specialize TPropertySetup<Integer, TButton>)
  public
    procedure Apply(AButton: TButton); override;
  end;

  TButtonVisibleSetup = class(specialize TPropertySetup<Boolean, TButton>)
    procedure Apply(AButton: TButton); override;
  end;

  TButtonBuilderHelper = class helper for TButtonBuilder
  public
    function WithFontSize(ASize: Integer): TButtonBuilder;    // usa classe auxiliar + setup
    function WithVisible(AVisible: Boolean): TButtonBuilder;  // usa classe auxiliar + setup
    function WithCursor(ACursor: Integer): TButtonBuilder;    // usa RTTI: recomendação. criar teste unitário para esse método.
  end;

  { TControlBuilderHelper }

  TControlBuilderHelper = class helper for TControlBuilder
    function WithBorderStyle(ABorderStyle: TBorderStyle): TControlBuilder;
  end;

var
  Form1: TForm1;

implementation

uses
  RTTI;

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
      .SetOwnerAndParent(Self, Self)
      .GridInit(4, 4)
        .GridSetCellWidthAndHeight(250, 60)
        .specialize Add<TButton>(TButtonBuilder.Create(TButton, B).WithCaption('Teste').WithFontSize(22))  // helper method: class + setup
        .specialize Add<TButton>(TButtonBuilder.Create.WithCaption('Teste 2').WithEnabled(False))
        .specialize Add<TButton>(TButtonBuilder.Create.WithCaption('Teste 3').WithVisible(False))          // helper method: class + setup
        .specialize Add<TButton>(TButtonBuilder.Create.WithCaption('Teste 4').WithCursor(crHandPoint))     // helper method: RTTI
        .Add(TControlBuilder.Create(TPanel).WithBorderStyle(bsSingle))
      .GridFinish
    ;
  finally
    Creator.Free;
  end;
end;

{ TFontSizeSetup }

procedure TFontSizeSetup.Apply(AButton: TButton);
begin
  AButton.Font.Size := Self.FValue;
  Self.Free;
end;

{ TButtonVisibleSetup }

procedure TButtonVisibleSetup.Apply(AButton: TButton);
begin
  TButton(AButton).Visible := FValue;
  Self.Free;
end;

{ TButtonBuilderHelper }

function TButtonBuilderHelper.WithFontSize(ASize: Integer): TButtonBuilder;
begin
  Result := Self;
  with TFontSizeSetup.Create(ASize) do
    Setup(@Apply);
end;

function TButtonBuilderHelper.WithVisible(AVisible: Boolean): TButtonBuilder;
begin
  Result := Self;
  with TButtonVisibleSetup.Create(AVisible) do
    Setup(@Apply);
end;

function TButtonBuilderHelper.WithCursor(ACursor: Integer): TButtonBuilder;
begin
  Result := Self.WithProp('Cursor', ACursor);
end;

{ TControlBuilderHelper }

function TControlBuilderHelper.WithBorderStyle(ABorderStyle: TBorderStyle
  ): TControlBuilder;
begin
  Result := Self.WithProp(
    'BorderStyle',
    TValue.specialize From<TBorderStyle>(ABorderStyle)
  );
end;

end.

