unit UVirtualKeyBlockLayouts;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, UVirtualKeyboardClasses, Generics.Collections;

type
  TIntIntDictionay = specialize TDictionary<Integer, Integer>;

  { TVirtualKeyBlockGridLayout }

  IVirtualKeyGridCell = interface
    property Key: TVirtualKey;
    property ColSpan: Integer;
    property RowSpan: Integer;
  end;

  { TVirtualKeyGridCell }

  TVirtualKeyGridCell = class(TInterfacedObject, IVirtualKeyGridCell)
  private
    FColSpan: Integer;
    FKey: TVirtualKey;
    FRowSpan: Integer;
  public
    constructor Create(const AKey: TVirtualKey); overload;
    constructor Create; overload;
    destructor Destroy; override;
    property Key: TVirtualKey read FKey;
    property ColSpan: Integer read FColSpan write FColSpan;
    property RowSpan: Integer read FRowSpan write FRowSpan;
  end;

  TVirtualKeyBlockGridLayout = class(TInterfacedObject, IVirtualKeyBlockLayout)
  private
    FCells: array of array of TVirtualKeyGridCell;
    FDefaultColumnWidth: Integer;
    FDefaultRowHeight: Integer;
    FHorizontalSpacing: Integer;
    FKeyHeight: Integer;
    FKeyWidth: Integer;
    FRows: Integer;
    FColumns: Integer;
    FVerticalSpacing: Integer;
    FCustomRowHeights: TIntIntDictionay;
    FCustomColumnWidths: TIntIntDictionay;

    procedure CreateCells;
    procedure SetDefaultColumnWidth(AValue: Integer);
    procedure SetDefaultRowHeight(AValue: Integer);

    function GetColumnWidth(ACol: Integer): Integer;
    function GetRowHeight(ARow: Integer): Integer;
    procedure SetColumnWidth(ACol, AWidth: Integer);
    procedure SetRowHeight(ARow, AHeight: Integer);
  public
    constructor Create(const ARows, AColumns: Integer); overload;
    constructor Create(const ARows, AColumns, AVerticalSpacing,
      AHorizontalSpacing: Integer); overload;
    destructor Destroy; override;
    procedure SetCell(ARow, ACol: Integer; AKey: TVirtualKey;
      AColSpan: Integer = 1; ARowSpan: Integer = 1);
    procedure ArrangeKeys(ABlock: TVirtualKeyBlock);

    property Rows: Integer read FRows;
    property Columns: Integer read FColumns;
    property VerticalSpacing: Integer read FVerticalSpacing;
    property HorizontalSpacing: Integer read FHorizontalSpacing;

    property DefaultRowHeight: Integer read FDefaultRowHeight write SetDefaultRowHeight;
    property DefaultColumnWidth: Integer read FDefaultColumnWidth write SetDefaultColumnWidth;

    property RowHeight[Index: Integer]: Integer read GetRowHeight write SetRowHeight;
    property ColumnWidth[Index: Integer]: Integer read GetColumnWidth write SetColumnWidth;
  end;

implementation

uses
  Math;

{ TVirtualKeyGridCell }

constructor TVirtualKeyGridCell.Create(const AKey: TVirtualKey);
begin
  FKey := AKey;
end;

constructor TVirtualKeyGridCell.Create;
begin
  FKey := nil;
end;

destructor TVirtualKeyGridCell.Destroy;
begin
  inherited Destroy;
end;

{ TVirtualKeyBlockGridLayout }

function TVirtualKeyBlockGridLayout.GetColumnWidth(ACol: Integer): Integer;
begin
  if not FCustomColumnWidths.TryGetValue(ACol, Result) then
    Result := FDefaultColumnWidth;
end;

function TVirtualKeyBlockGridLayout.GetRowHeight(ARow: Integer): Integer;
begin
  if not FCustomRowHeights.TryGetValue(ARow, Result) then
    Result := FDefaultRowHeight;
end;

procedure TVirtualKeyBlockGridLayout.SetColumnWidth(ACol, AWidth: Integer);
begin
  FCustomColumnWidths.AddOrSetValue(ACol, AWidth);
end;

procedure TVirtualKeyBlockGridLayout.SetRowHeight(ARow, AHeight: Integer);
begin
  FCustomRowHeights.AddOrSetValue(ARow, AHeight);
end;

procedure TVirtualKeyBlockGridLayout.CreateCells;
var
  I, Row, Col: Integer;
begin
  SetLength(FCells, FRows);
  for I := 0 to FRows-1 do
    SetLength(FCells[I], FColumns);
end;

procedure TVirtualKeyBlockGridLayout.SetDefaultColumnWidth(AValue: Integer);
begin
  if FDefaultColumnWidth = AValue then
    Exit;
  FDefaultColumnWidth := AValue;
end;


procedure TVirtualKeyBlockGridLayout.SetDefaultRowHeight(AValue: Integer);
begin
  if FDefaultRowHeight = AValue then
    Exit;
  FDefaultRowHeight := AValue;
end;


constructor TVirtualKeyBlockGridLayout.Create(const ARows, AColumns: Integer);
begin
  inherited Create;
  Create(ARows, AColumns, 2, 2);
end;

constructor TVirtualKeyBlockGridLayout.Create(const ARows, AColumns,
  AVerticalSpacing, AHorizontalSpacing: Integer);
var
  I: Integer;
begin
  inherited Create;

  FRows := Max(1, ARows);
  FColumns := Max(1, AColumns);
  FVerticalSpacing := Max(1, AVerticalSpacing);
  FHorizontalSpacing := Max(1, AHorizontalSpacing);

  FKeyHeight := 80;
  FKeyWidth := 80;

  FCustomColumnWidths := TIntIntDictionay.Create;
  FCustomRowHeights := TIntIntDictionay.Create;

  CreateCells;
end;

destructor TVirtualKeyBlockGridLayout.Destroy;
var
  Row, Col: Integer;
begin
  for Row := 0 to FRows - 1 do
    for Col := 0 to FColumns - 1 do
      if Assigned(FCells[Row][Col]) and Assigned(FCells[Row][Col].Key) then
        FCells[Row][Col].Free;

  FCustomColumnWidths.Free;
  FCustomRowHeights.Free;

  inherited Destroy;
end;

procedure TVirtualKeyBlockGridLayout.SetCell(ARow, ACol: Integer;
  AKey: TVirtualKey; AColSpan: Integer; ARowSpan: Integer);

begin
  if (ARow >= FRows) or (ACol >= FColumns) then
    Exit;

  FCells[ARow][ACol] := TVirtualKeyGridCell.Create(AKey);
  FCells[ARow][ACol].ColSpan := Max(1, AColSpan);
  FCells[ARow][ACol].RowSpan := Max(1, ARowSpan);
end;

procedure TVirtualKeyBlockGridLayout.ArrangeKeys(ABlock: TVirtualKeyBlock);
var
  Row, Col: Integer;
  Cell: TVirtualKeyGridCell;
  Used: array of array of Boolean;
  StartX, StartY: Integer;
  X, Y: Integer;
  PosX, PosY, TotalWidth, TotalHeight: Integer;
  BlockWidth, BlockHeigth: Integer;
begin
  SetLength(Used, FRows, FColumns);
  StartX := ABlock.MarginLeft;
  StartY := ABlock.MarginTop;

  for Row := 0 to FRows - 1 do
    for Col := 0 to FColumns - 1 do
    begin
      if Used[Row][Col] then
        Continue;

      Cell := FCells[Row][Col];
      if not Assigned(Cell) or not Assigned(Cell.Key) then
        Continue;

      // Marcar células ocupadas
      for Y := Row to Row + Cell.RowSpan - 1 do
        for X := Col to Col + Cell.ColSpan - 1 do
          if (Y < FRows) and (X < FColumns) then
            Used[Y][X] := True;

      // Calcula posição inicial (PosX, PosY)
      PosX := StartX;
      for X := 0 to Col - 1 do
        Inc(PosX, ColumnWidth[X] + HorizontalSpacing);

      PosY := StartY;
      for Y := 0 to Row - 1 do
        Inc(PosY, RowHeight[Y] + VerticalSpacing);

      // Calcula tamanho total (TotalWidth, TotalHeight)
      TotalWidth := 0;
      for X := Col to Col + Cell.ColSpan - 1 do
        if X < FColumns then
          Inc(TotalWidth, ColumnWidth[X]);
      Inc(TotalWidth, (Cell.ColSpan - 1) * HorizontalSpacing);

      TotalHeight := 0;
      for Y := Row to Row + Cell.RowSpan - 1 do
        if Y < FRows then
          Inc(TotalHeight, RowHeight[Y]);
      Inc(TotalHeight, (Cell.RowSpan - 1) * VerticalSpacing);

      // Posicionar a tecla
      Cell.Key.SetBounds(PosX, PosY, TotalWidth, TotalHeight);
      Cell.Key.Parent := ABlock;
    end;

  // Agora calcular a largura total do bloco
  BlockWidth := StartX;
  for Col := 0 to FColumns - 1 do
    Inc(BlockWidth, ColumnWidth[Col] + HorizontalSpacing);
  Dec(BlockWidth, HorizontalSpacing); // remove o último espaçamento
  Inc(BlockWidth, ABlock.MarginRight);
  ABlock.Width := BlockWidth;

  // E a altura total do bloco
  BlockHeigth := StartY;
  for Row := 0 to FRows - 1 do
    Inc(BlockHeigth, RowHeight[Row] + VerticalSpacing);
  Dec(BlockHeigth, VerticalSpacing); // remove o último espaçamento
  Inc(BlockHeigth, ABlock.MarginBottom);
  ABlock.Height := BlockHeigth;
end;

{
procedure TVirtualKeyBlockGridLayout.ArrangeKeys(ABlock: TVirtualKeyBlock);
var
  Row, Col: Integer;
  Cell: TVirtualKeyGridCell;
  Used: array of array of Boolean;
  StartX, StartY: Integer;
  X, Y, W, H: Integer;
begin
  SetLength(Used, FRows, FColumns);
  StartX := ABlock.MarginLeft;
  StartY := ABlock.MarginTop;

  for Row := 0 to FRows - 1 do
    for Col := 0 to FColumns - 1 do
    begin
      if Used[Row][Col] then
        Continue;

      Cell := FCells[Row][Col];
      if not Assigned(Cell) or not Assigned(Cell.Key) then
        Continue;

      W := Cell.ColSpan;
      H := Cell.RowSpan;

      // Marcar células ocupadas
      for Y := Row to Row + H - 1 do
        for X := Col to Col + W - 1 do
          if (Y < FRows) and (X < FColumns) then
            Used[Y][X] := True;

      // Posicionar tecla
      Cell.Key.SetBounds(
        StartX + Col * (KeyWidth + HorizontalSpacing),
        StartY + Row * (KeyHeight + VerticalSpacing),
        W * KeyWidth + (W - 1) * HorizontalSpacing,
        H * KeyHeight + (H - 1) * VerticalSpacing
      );
      Cell.Key.Parent := ABlock;
    end;

  ABlock.Width := StartX
    + FColumns * (KeyWidth + HorizontalSpacing)
    + ABlock.MarginRight;

  ABlock.Height := StartY
    + FRows * (KeyHeight + VerticalSpacing)
    + ABlock.MarginBottom;
end;
}

{procedure TVirtualKeyBlockGridLayout.ArrangeKeys(ABlock: TVirtualKeyBlock);
var
  Row, Col, i: Integer;
  Key: TVirtualKey;
  StartX, StartY: Integer;
  TotalWidth, TotalHeight: Integer;
begin
  StartX := ABlock.MarginLeft;
  StartY := ABlock.MarginTop;

  for i := 0 to ABlock.KeyCount - 1 do
  begin
    Row := i div FColumns;
    Col := i mod FColumns;

    Key := ABlock.Keys.Data[i];
    Key.Left := StartX + Col * (KeyWidth + VerticalSpacing);
    Key.Top := StartY + Row * (KeyHeight + HorizontalSpacing);
    Key.Width := KeyWidth;
    Key.Height := KeyHeight;
  end;

  // Cálculo do tamanho do bloco com base nas dimensões das teclas
  TotalWidth := StartX
    + FColumns * KeyWidth
    + (FColumns - 1) * HorizontalSpacing
    + ABlock.MarginRight;

  TotalHeight := StartY
    + (
        (ABlock.KeyCount + FColumns - 1) div FColumns
      ) * KeyHeight
    + (
        ((ABlock.KeyCount + FColumns - 1) div FColumns) - 1
      ) * VerticalSpacing
    + ABlock.MarginBottom;

  ABlock.Width := TotalWidth;
  ABlock.Height := TotalHeight;
end;
}

end.

