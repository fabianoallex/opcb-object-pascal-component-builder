unit uffullresizergridlayout;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ULayout,
  UGridaLayoutResizer, Builders;

type

  { TFFullResizerGridLayout }

  TFFullResizerGridLayout = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FGrid: TGridLayout;
    FResizer: IGridFullResizer;
    procedure CreateGrid;
    procedure SetGrid(AValue: TGridLayout);
    procedure SetResizer(AValue: IGridFullResizer);
  public
    property Grid: TGridLayout read FGrid write SetGrid;
    property Resizer: IGridFullResizer read FResizer write SetResizer;
  end;

var
  FFullResizerGridLayout: TFFullResizerGridLayout;

implementation

{$R *.lfm}

{ TFFullResizerGridLayout }

procedure TFFullResizerGridLayout.SetResizer(AValue: IGridFullResizer);
begin
  if FResizer = AValue then Exit;
  FResizer := AValue;
end;

procedure TFFullResizerGridLayout.FormCreate(Sender: TObject);
begin
  CreateGrid;
end;

procedure TFFullResizerGridLayout.FormResize(Sender: TObject);
begin
  Resizer
    .WithGridHeight(Self.Height)
    .WithGridWidth(Self.Width)
    .Resize(Grid);
  Grid.ArrangeItems;
end;

procedure TFFullResizerGridLayout.CreateGrid;
var
  I: Integer;
  Btn: TButton;
begin
  Grid := TGridLayout.Create;
  Grid.Rows := 4;
  Grid.Columns := 4;
  Grid.ColumnWidths := 80;
  Grid.RowHeights := 50;
  Grid.HorizontalSpacings := 5;
  Grid.VerticalSpacings := 5;
  Grid.Margins.All := 5;

  for I := 0 to 14 do
  begin
    Btn := TButton.Create(Self);
    Btn.Parent := Self;
    Btn.Caption := 'Botão ' + IntToStr(I + 1);

    Grid.AddItem(Btn, TGridCellSettings.Create(I div 4, I mod 4));
  end;

  Resizer := TGridFullResizer
    .Create
    .WithFixedRows([0])
    .WithMinAndMaxRowHeight(1, 150, 250)
    .WithMinAndMaxGridHeight(350, 800)
    .WithGridHeight(Self.Height)
    .WithGridWidth(Self.Width);

  Resizer.Resize(Grid);

  Grid.ArrangeItems;
end;

procedure TFFullResizerGridLayout.SetGrid(AValue: TGridLayout);
begin
  if FGrid = AValue then Exit;
  FGrid := AValue;
end;

end.

