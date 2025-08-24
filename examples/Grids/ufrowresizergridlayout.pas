unit UFRowResizerGridLayout;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ULayout,
  UGridaLayoutResizer, ULayout.controls;

type

  { TFRowResizerGridLayout }

  TFRowResizerGridLayout = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FGrid: TGridLayout;
    FResizer: IGridHeightResizer;
    procedure SetGrid(AValue: TGridLayout);
    procedure SetResizer(AValue: IGridHeightResizer);
  public
    procedure CreateGrid;
    property Grid: TGridLayout read FGrid write SetGrid;
    property Resizer: IGridHeightResizer read FResizer write SetResizer;
  end;

var
  FRowResizerGridLayout: TFRowResizerGridLayout;

implementation

{$R *.lfm}

{ TFRowResizerGridLayout }

procedure TFRowResizerGridLayout.FormResize(Sender: TObject);
begin
  if Resizer.GridHeight <> Self.Height then
  begin
    Resizer
      .WithGridHeight(Self.Height)
      .Resize(Grid);
    Grid.ArrangeItems;
  end;
end;

procedure TFRowResizerGridLayout.FormCreate(Sender: TObject);
begin
  CreateGrid;
end;

procedure TFRowResizerGridLayout.Button1Click(Sender: TObject);
begin
  Resizer
    .WithGridHeight(Self.Height)
    .WithFixedRows([])
    .Resize(Grid);
  Grid.ArrangeItems;
end;

procedure TFRowResizerGridLayout.Button2Click(Sender: TObject);
begin
  Resizer
    .WithGridHeight(Self.Height)
    .EnableFixedRow(0)
    .Resize(Grid);
  Grid.ArrangeItems;
end;

procedure TFRowResizerGridLayout.Button3Click(Sender: TObject);
begin
  Resizer
    .WithGridHeight(Self.Height)
    .EnableFixedRow(1)
    .Resize(Grid);
  Grid.ArrangeItems;
end;

procedure TFRowResizerGridLayout.Button4Click(Sender: TObject);
begin
  Resizer
    .WithGridHeight(Self.Height)
    .EnableFixedRow(2)
    .Resize(Grid);
  Grid.ArrangeItems;
end;

procedure TFRowResizerGridLayout.Button5Click(Sender: TObject);
begin
  Resizer
    .WithGridHeight(Self.Height)
    .EnableFixedRow(3)
    .Resize(Grid);
  Grid.ArrangeItems;
end;

procedure TFRowResizerGridLayout.Button6Click(Sender: TObject);
begin
  Grid.InsertRow(2);

  Resizer
    .WithGridHeight(Self.Height)
    .Resize(Grid);

  Grid.ArrangeItems;
end;

procedure TFRowResizerGridLayout.SetGrid(AValue: TGridLayout);
begin
  if FGrid = AValue then Exit;
  FGrid := AValue;
end;

procedure TFRowResizerGridLayout.SetResizer(AValue: IGridHeightResizer);
begin
  if FResizer = AValue then Exit;
  FResizer := AValue;
end;

procedure TFRowResizerGridLayout.CreateGrid;
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

  Resizer := TGridHeightResizer
    .Create
    .WithFixedRows([0])
    .WithMinAndMaxRowHeight(1, 150, 250)
    .WithMinAndMaxGridHeight(350, 800)
    .WithGridHeight(Self.Height);

  Resizer.Resize(Grid);

  Grid.ArrangeItems;
end;

end.

