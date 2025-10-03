unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, OPCB, OPCB.Builders, Vcl.StdCtrls,
  Vcl.Menus, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TButtonBuilderHelper = class helper for TButtonBuilder
  public
    function WithFontSize(ASize: Integer): TButtonBuilder;
    function WithVisible(AVisible: Boolean): TButtonBuilder;
    function WithCursor(ACursor: Integer): TButtonBuilder;    // usa RTTI: recomendação. criar teste unitário para esse método.
  end;

  TControlBuilderHelper = class helper for TControlBuilder
    function WithBorderStyle(ABorderStyle: TBorderStyle): TControlBuilder;
  end;

var
  Form1: TForm1;

implementation

uses
  System.Rtti;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  Creator: TControlCreator;
  B: TButton;
begin
  Creator := TControlCreator.Create;
  try
    Creator
      .WithOwnerAndParent(Self, Self)
      .GridInit(4, 4)
        .GridSetCellWidthAndHeight(250, 60)
        .AddControl(TButtonBuilder.Create(TButton, B).WithCaption('Teste').WithFontSize(22))  // metodo definido no helper
        .AddControl(TButtonBuilder.Create.WithCaption('Teste 2').WithEnabled(False))
        .AddControl(TButtonBuilder.Create.WithCaption('Teste 3').WithVisible(False))          // metodo definido no helper
        .AddControl(TButtonBuilder.Create.WithCaption('Test 4').WithCursor(crHandPoint))      // medoto definido no helper (com rtti)
        .AddControl(TControlBuilder.Create(TPanel).WithBorderStyle(bsSingle))
      .GridFinish
    ;
  finally
    Creator.Free;
  end;
end;

{ TButtonBuilderHelper }

function TButtonBuilderHelper.WithCursor(ACursor: Integer): TButtonBuilder;
begin
  Result := Self.WithProp('Cursor', ACursor);
end;

function TButtonBuilderHelper.WithFontSize(ASize: Integer): TButtonBuilder;
begin
  Result := Self;
  Setup(
    procedure(AControl: TControl)
    begin
      TButton(AControl).Font.Size := ASize;
    end
  );
end;

function TButtonBuilderHelper.WithVisible(AVisible: Boolean): TButtonBuilder;
begin
  Result := Self;
  Setup(
    procedure(AControl: TControl)
    begin
      TButton(AControl).Visible := AVisible;
    end
  );
end;

{ TControlBuilderHelper }

function TControlBuilderHelper.WithBorderStyle(
  ABorderStyle: TBorderStyle): TControlBuilder;
begin
  Result := Self.WithProp(
    'BorderStyle',
    TValue.From<TBorderStyle>(ABorderStyle)
  );
end;

end.
