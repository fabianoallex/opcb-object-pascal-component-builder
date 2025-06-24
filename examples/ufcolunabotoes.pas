unit UFColunaBotoes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls,
  ExtCtrls, SpinEx, ULayout, ULayout.controls;

type

  { TColumnButtons }

  TColumnButtons = class
  private
    FButtonHeight: Integer;
    FButtonMaxWidth: Integer;
    FButtonMinWidth: Integer;
    FButtonWidth: Integer;
    FGrid: TGridLayout;
    FSpacing: Integer;
    FOwner: TComponent;
    FParent: TWinControl;
    procedure SetButtonHeight(AValue: Integer);
    procedure SetButtonMaxWidth(AValue: Integer);
    procedure SetButtonMinWidth(AValue: Integer);
    procedure SetButtonWidth(AValue: Integer);
    procedure SetSpacing(AValue: Integer);
  public
    constructor Create(AOwner: TComponent; AParent: TWinControl);
    destructor Destroy; override;
    function AddButton: TButton;
    property ButtonWidth: Integer read FButtonWidth write SetButtonWidth;
    property ButtonHeight: Integer read FButtonHeight write SetButtonHeight;
    property ButtonMaxWidth: Integer read FButtonMaxWidth write SetButtonMaxWidth;
    property ButtonMinWidth: Integer read FButtonMinWidth write SetButtonMinWidth;
    property Spacing: Integer read FSpacing write SetSpacing;
    procedure Update;
  end;

  { TFColunaBotoes }

  TFColunaBotoes = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    ScrollBox: TScrollBox;
    SpinEditEx1: TSpinEditEx;
    SpinEditEx2: TSpinEditEx;
    Splitter1: TSplitter;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ScrollBoxResize(Sender: TObject);
    procedure SpinEditEx1Change(Sender: TObject);
    procedure SpinEditEx2Change(Sender: TObject);
  private
    FColunaBotoes: TColumnButtons;
    procedure SetColunaBotoes(AValue: TColumnButtons);
  public
    property ColunaBotoes: TColumnButtons read FColunaBotoes write SetColunaBotoes;
  end;

var
  FColunaBotoes: TFColunaBotoes;

implementation

uses
  Math;

{$R *.lfm}

{ TColumnButtons }

procedure TColumnButtons.SetButtonWidth(AValue: Integer);
begin
  if FButtonWidth = AValue then Exit;
  FButtonWidth := AValue;
  FGrid.ColumnWidths := FButtonWidth;
end;

procedure TColumnButtons.SetSpacing(AValue: Integer);
begin
  if FSpacing = AValue then Exit;
  FSpacing := AValue;
end;

procedure TColumnButtons.SetButtonHeight(AValue: Integer);
begin
  if FButtonHeight = AValue then Exit;
  FButtonHeight := AValue;
end;

procedure TColumnButtons.SetButtonMaxWidth(AValue: Integer);
begin
  if FButtonMaxWidth = AValue then Exit;
  FButtonMaxWidth := AValue;
end;

procedure TColumnButtons.SetButtonMinWidth(AValue: Integer);
begin
  if FButtonMinWidth = AValue then Exit;
  FButtonMinWidth := AValue;
end;

constructor TColumnButtons.Create(AOwner: TComponent; AParent: TWinControl);
begin
  FOwner := AOwner;
  FParent := AParent;
  FGrid := TGridLayout.Create;
  FGrid.Columns := 1;
  FGrid.Rows := 0;
end;

destructor TColumnButtons.Destroy;
begin
  FGrid.Free;
  inherited Destroy;
end;

function TColumnButtons.AddButton: TButton;
var
  Btn: TButton;
begin
  Btn := TButton.Create(FOwner);
  Btn.Parent := FParent;
  FGrid.AddItem(Btn, TGridCellSettings.Create(FGrid.Rows, 0));
  FGrid.Rows := FGrid.Rows + 1;
  Update;
  Result := Btn;
end;

procedure TColumnButtons.Update;
begin
  if FGrid.ColumnWidths > ButtonMaxWidth then
    FGrid.ColumnWidths := ButtonMaxWidth
  else if FGrid.ColumnWidths < ButtonMinWidth then
    FGrid.ColumnWidths := ButtonMinWidth;

  FGrid.RowHeights := ButtonHeight;
  FGrid.VerticalSpacings := Spacing;
  FGrid.ArrangeItems;
end;

{ TFColunaBotoes }

procedure TFColunaBotoes.Button1Click(Sender: TObject);
begin
  with ColunaBotoes.AddButton do
    Caption := TimeToStr(now);
end;

procedure TFColunaBotoes.FormCreate(Sender: TObject);
begin
  ColunaBotoes := TColumnButtons.Create(Self, ScrollBox);
  ColunaBotoes.ButtonWidth := ScrollBox.ClientWidth;
  ColunaBotoes.ButtonMaxWidth := 300;
  ColunaBotoes.ButtonMinWidth := 100;
  ColunaBotoes.Spacing := 5;
end;

procedure TFColunaBotoes.FormDestroy(Sender: TObject);
begin
  ColunaBotoes.Free;
end;

procedure TFColunaBotoes.ScrollBoxResize(Sender: TObject);
begin
  ColunaBotoes.ButtonWidth := ScrollBox.ClientWidth;
  ColunaBotoes.Update;
end;

procedure TFColunaBotoes.SpinEditEx1Change(Sender: TObject);
begin
  ColunaBotoes.ButtonHeight := SpinEditEx1.Value;
  ColunaBotoes.Update;
end;

procedure TFColunaBotoes.SpinEditEx2Change(Sender: TObject);
begin
  ColunaBotoes.Spacing := SpinEditEx2.Value;
  ColunaBotoes.Update;
end;

procedure TFColunaBotoes.SetColunaBotoes(AValue: TColumnButtons);
begin
  if FColunaBotoes = AValue then Exit;
  FColunaBotoes := AValue;
end;

end.

