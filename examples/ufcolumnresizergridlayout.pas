unit UFColumnResizerGridLayout;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ULayout,
  UGridaLayoutResizer;

type

  { TFColumnResizerGridLayout }

  TFColumnResizerGridLayout = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FGrid: TGridLayout;
    FResizer: IGridLayoutWidthResizer;
    procedure SetGrid(AValue: TGridLayout);
    procedure SetResizer(AValue: IGridLayoutWidthResizer);
  public
    procedure CreateGrid;
    property Grid: TGridLayout read FGrid write SetGrid;
    property Resizer: IGridLayoutWidthResizer read FResizer write SetResizer;
  end;

var
  FColumnResizerGridLayout: TFColumnResizerGridLayout;

implementation

{$R *.lfm}

{ TFColumnResizerGridLayout }

procedure TFColumnResizerGridLayout.FormCreate(Sender: TObject);
begin
  CreateGrid;
end;

procedure TFColumnResizerGridLayout.Button1Click(Sender: TObject);
begin
  Resizer
    .WithGridWidth(Self.Width)
    .WithFixedColumns([])
    .Resize(Grid);
  Grid.ArrangeItems;
end;

procedure TFColumnResizerGridLayout.Button2Click(Sender: TObject);
begin
  Resizer
    .WithGridWidth(Self.Width)
    .EnableFixedColumn(0)
    .Resize(Grid);
  Grid.ArrangeItems;
end;

procedure TFColumnResizerGridLayout.Button3Click(Sender: TObject);
begin
  Resizer
    .WithGridWidth(Self.Width)
    .EnableFixedColumn(1)
    .Resize(Grid);
  Grid.ArrangeItems;
end;

procedure TFColumnResizerGridLayout.Button4Click(Sender: TObject);
begin
  Resizer
    .WithGridWidth(Self.Width)
    .EnableFixedColumn(2)
    .Resize(Grid);
  Grid.ArrangeItems;
end;

procedure TFColumnResizerGridLayout.Button5Click(Sender: TObject);
begin
  Resizer
    .WithGridWidth(Self.Width)
    .EnableFixedColumn(3)
    .Resize(Grid);
  Grid.ArrangeItems;
end;

procedure TFColumnResizerGridLayout.FormResize(Sender: TObject);
begin
  if Resizer.GridWidth <> Self.Width then
  begin
    Resizer
      .WithGridWidth(Self.Width)
      .Resize(Grid);
    Grid.ArrangeItems;
  end;
end;

procedure TFColumnResizerGridLayout.CreateGrid;
var
  I: Integer;
  Btn: TButton;
begin
  Grid := TGridLayout.Create;
  Grid.Top := 50;
  Grid.Rows := 4;
  Grid.Columns := 4;
  Grid.ColumnWidths := 80;
  Grid.RowHeights := 50;
  Grid.HorizontalSpacings := 5;
  Grid.VerticalSpacings := 5;
  Grid.Margins.All := 5;

  for I := 0 to 10 do
  begin
    Btn := TButton.Create(Self);
    Btn.Parent := Self;
    Btn.Caption := 'Botão ' + IntToStr(I + 1);

    if I = 10 then
      Grid.AddItem(
        Btn,
        TGridCellSettings.Create(I div 4, I mod 4)
          .WithColumnSpan(2)
      )
    else
      Grid.AddItem(Btn, TGridCellSettings.Create(I div 4, I mod 4));
  end;

  Resizer := TGridLayoutWidthResizer
    .Create
    .WithFixedColumns([0])
    .WithMinAndMaxColumnWidth(1, 150, 250)
    .WithMinAndMaxGridWidth(350, 1200)
    .WithGridWidth(Self.Width);

  Resizer.Resize(Grid);

  Grid.ArrangeItems;
end;

procedure TFColumnResizerGridLayout.SetGrid(AValue: TGridLayout);
begin
  if FGrid = AValue then Exit;
  FGrid := AValue;
end;

procedure TFColumnResizerGridLayout.SetResizer(AValue: IGridLayoutWidthResizer);
begin
  if FResizer = AValue then Exit;
  FResizer := AValue;
end;

end.

