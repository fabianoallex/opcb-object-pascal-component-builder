unit UGridaLayoutResizer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ULayout, Generics.Collections;

type
  TIntList = specialize TList<Integer>;
  TIntIntDictionary = specialize TDictionary<Integer, Integer>;

  IGridLayoutResizer = interface
    ['{CDDFEEED-72B4-4699-99AA-23714658C2DE}']
    procedure Resize(AGrid: TGridLayout);
  end;

  { IGridLayoutWidthResizer }

  IGridLayoutWidthResizer = interface(IGridLayoutResizer)
    ['{8F19CDBF-F294-4F4E-B1EE-3A844BEDA43C}']
    function GetGridWidth: Integer;
    function WithGridWidth(ANewWidth: Integer): IGridLayoutWidthResizer;
    function WithFixedColumns(const AFixedColumns: array of Integer): IGridLayoutWidthResizer;
    function DisableFixedColumn(AColIndex: Integer): IGridLayoutWidthResizer; overload;
    function EnableFixedColumn(AColIndex: Integer): IGridLayoutWidthResizer; overload;
    function DisableFixedColumn(const AFixedColumns: array of Integer): IGridLayoutWidthResizer; overload;
    function EnableFixedColumn(const AFixedColumns: array of Integer): IGridLayoutWidthResizer; overload;
    function WithMaxColumnWidth(AColIndex, AMax: Integer): IGridLayoutWidthResizer;
    function WithMinColumnWidth(AColIndex, AMin: Integer): IGridLayoutWidthResizer;
    function WithMinAndMaxColumnWidth(AColIndex, AMin, AMax: Integer): IGridLayoutWidthResizer;
    function WithMaxGridWidth(AMax: Integer): IGridLayoutWidthResizer;
    function WithMinGridWidth(AMin: Integer): IGridLayoutWidthResizer;
    function WithMinAndMaxGridWidth(Amin, AMax: Integer): IGridLayoutWidthResizer;
    property GridWidth: Integer read GetGridWidth;
  end;

  { IGridLayoutHeightResizer }

  IGridLayoutHeightResizer = interface(IGridLayoutResizer)
    ['{F8FC03F3-843B-4D84-8CDE-2B1F77425078}']
    function GetGridHeight: Integer;
    function WithGridHeight(ANewHeight: Integer): IGridLayoutHeightResizer;
    function WithFixedRows(const AFixedRows: array of Integer): IGridLayoutHeightResizer;
    function DisableFixedRow(ARowIndex: Integer): IGridLayoutHeightResizer; overload;
    function EnableFixedRow(ARowIndex: Integer): IGridLayoutHeightResizer; overload;
    function DisableFixedRow(const AFixedRows: array of Integer): IGridLayoutHeightResizer; overload;
    function EnableFixedRow(const AFixedRows: array of Integer): IGridLayoutHeightResizer; overload;
    function WithMaxRowHeight(ARowIndex, AMax: Integer): IGridLayoutHeightResizer;
    function WithMinRowHeight(ARowIndex, AMin: Integer): IGridLayoutHeightResizer;
    function WithMinAndMaxRowHeight(ARowIndex, AMin, AMax: Integer): IGridLayoutHeightResizer;
    function WithMaxGridHeight(AMax: Integer): IGridLayoutHeightResizer;
    function WithMinGridHeight(AMin: Integer): IGridLayoutHeightResizer;
    function WithMinAndMaxGridHeight(Amin, AMax: Integer): IGridLayoutHeightResizer;
    property GridHeight: Integer read GetGridHeight ;
  end;

  IGridLayoutFullResizer = interface(IGridLayoutResizer)
    function WithGridWidth(ANewWidth: Integer): IGridLayoutFullResizer;
    function WithFixedColumns(const AFixedColumns: array of Integer): IGridLayoutFullResizer;
    function DisableFixedColumn(AColIndex: Integer): IGridLayoutFullResizer; overload;
    function EnableFixedColumn(AColIndex: Integer): IGridLayoutFullResizer; overload;
    function DisableFixedColumn(const AFixedColumns: array of Integer): IGridLayoutFullResizer; overload;
    function EnableFixedColumn(const AFixedColumns: array of Integer): IGridLayoutFullResizer; overload;
    function WithMaxColumnWidth(AColIndex, AMax: Integer): IGridLayoutFullResizer;
    function WithMinColumnWidth(AColIndex, AMin: Integer): IGridLayoutFullResizer;
    function WithMinAndMaxColumnWidth(AColIndex, AMin, AMax: Integer): IGridLayoutFullResizer;
    function WithMaxGridWidth(AMax: Integer): IGridLayoutFullResizer;
    function WithMinGridWidth(AMin: Integer): IGridLayoutFullResizer;
    function WithMinAndMaxGridWidth(Amin, AMax: Integer): IGridLayoutFullResizer;

    function WithGridHeight(ANewHeight: Integer): IGridLayoutFullResizer;
    function WithFixedRows(const AFixedRows: array of Integer): IGridLayoutFullResizer;
    function DisableFixedRow(ARowIndex: Integer): IGridLayoutFullResizer; overload;
    function EnableFixedRow(ARowIndex: Integer): IGridLayoutFullResizer; overload;
    function DisableFixedRow(const AFixedRows: array of Integer): IGridLayoutFullResizer; overload;
    function EnableFixedRow(const AFixedRows: array of Integer): IGridLayoutFullResizer; overload;
    function WithMaxRowHeight(ARowIndex, AMax: Integer): IGridLayoutFullResizer;
    function WithMinRowHeight(ARowIndex, AMin: Integer): IGridLayoutFullResizer;
    function WithMinAndMaxRowHeight(ARowIndex, AMin, AMax: Integer): IGridLayoutFullResizer;
    function WithMaxGridHeight(AMax: Integer): IGridLayoutFullResizer;
    function WithMinGridHeight(AMin: Integer): IGridLayoutFullResizer;
    function WithMinAndMaxGridHeight(Amin, AMax: Integer): IGridLayoutFullResizer;
  end;

  { TGridLayoutWidthResizer }

  TGridLayoutWidthResizer = class(TInterfacedObject, IGridLayoutWidthResizer)
  private
    FMinGridWidth: Integer;
    FMaxGridWidth: Integer;
    FGridWidth: Integer;
    FFixedColumns: TIntList;
    FMinWidths: TIntIntDictionary;
    FMaxWidths: TIntIntDictionary;
    procedure ApplyColumnLimitsAndRedistribute(AGrid: TGridLayout;
      AFlexibleCols: TIntList);
  public
    constructor Create;
    destructor Destroy; override;
    function GetGridWidth: Integer;
    procedure Resize(AGrid: TGridLayout);
    function WithGridWidth(ANewWidth: Integer): IGridLayoutWidthResizer;
    function WithFixedColumns(const AFixedColumns: array of Integer): IGridLayoutWidthResizer;
    function DisableFixedColumn(AColIndex: Integer): IGridLayoutWidthResizer; overload;
    function EnableFixedColumn(AColIndex: Integer): IGridLayoutWidthResizer; overload;
    function DisableFixedColumn(const AFixedColumns: array of Integer): IGridLayoutWidthResizer; overload;
    function EnableFixedColumn(const AFixedColumns: array of Integer): IGridLayoutWidthResizer; overload;
    function WithMaxColumnWidth(AColIndex, AMax: Integer): IGridLayoutWidthResizer;
    function WithMinColumnWidth(AColIndex, AMin: Integer): IGridLayoutWidthResizer;
    function WithMinAndMaxColumnWidth(AColIndex, AMin, AMax: Integer): IGridLayoutWidthResizer;
    function WithMaxGridWidth(AMax: Integer): IGridLayoutWidthResizer;
    function WithMinGridWidth(AMin: Integer): IGridLayoutWidthResizer;
    function WithMinAndMaxGridWidth(Amin, AMax: Integer): IGridLayoutWidthResizer;
    property GridWidth: Integer read GetGridWidth write FGridWidth;
  end;

  { TGridLayoutHeightResizer }

  TGridLayoutHeightResizer = class(TInterfacedObject, IGridLayoutHeightResizer)
  private
    FMinGridHeight: Integer;
    FMaxGridHeight: Integer;
    FGridHeight: Integer;
    FFixedRows: TIntList;
    FMinHeights: TIntIntDictionary;
    FMaxHeights: TIntIntDictionary;
    procedure ApplyRowLimitsAndRedistribute(AGrid: TGridLayout;
      AFlexibleRows: TIntList);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Resize(AGrid: TGridLayout);
    function GetGridHeight: Integer;
    function WithGridHeight(ANewHeight: Integer): IGridLayoutHeightResizer;
    function WithFixedRows(const AFixedRows: array of Integer): IGridLayoutHeightResizer;
    function DisableFixedRow(ARowIndex: Integer): IGridLayoutHeightResizer; overload;
    function EnableFixedRow(ARowIndex: Integer): IGridLayoutHeightResizer; overload;
    function DisableFixedRow(const AFixedRows: array of Integer): IGridLayoutHeightResizer; overload;
    function EnableFixedRow(const AFixedRows: array of Integer): IGridLayoutHeightResizer; overload;
    function WithMaxRowHeight(ARowIndex, AMax: Integer): IGridLayoutHeightResizer;
    function WithMinRowHeight(ARowIndex, AMin: Integer): IGridLayoutHeightResizer;
    function WithMinAndMaxRowHeight(ARowIndex, AMin, AMax: Integer): IGridLayoutHeightResizer;
    function WithMaxGridHeight(AMax: Integer): IGridLayoutHeightResizer;
    function WithMinGridHeight(AMin: Integer): IGridLayoutHeightResizer;
    function WithMinAndMaxGridHeight(Amin, AMax: Integer): IGridLayoutHeightResizer;
    property GridHeight: Integer read GetGridHeight write FGridHeight;
  end;

  { TGridLayoutFullResizer }

  TGridLayoutFullResizer = class(TInterfacedObject, IGridLayoutFullResizer)
  private
    FGridHeight: Integer;
    FGridWidth: Integer;
    FWidthResizer: IGridLayoutWidthResizer;
    FHeightResizer: IGridLayoutHeightResizer;
  public
    constructor Create;
    procedure Resize(AGrid: TGridLayout);

    function GetGridHeight: Integer;
    function WithGridHeight(ANewHeight: Integer): IGridLayoutFullResizer; reintroduce;
    function WithFixedRows(const AFixedRows: array of Integer): IGridLayoutFullResizer; reintroduce;
    function DisableFixedRow(ARowIndex: Integer): IGridLayoutFullResizer; overload; reintroduce;
    function EnableFixedRow(ARowIndex: Integer): IGridLayoutFullResizer; overload; reintroduce;
    function DisableFixedRow(const AFixedRows: array of Integer): IGridLayoutFullResizer; reintroduce; overload;
    function EnableFixedRow(const AFixedRows: array of Integer): IGridLayoutFullResizer; reintroduce; overload;
    function WithMaxRowHeight(ARowIndex, AMax: Integer): IGridLayoutFullResizer; reintroduce;
    function WithMinRowHeight(ARowIndex, AMin: Integer): IGridLayoutFullResizer; reintroduce;
    function WithMinAndMaxRowHeight(ARowIndex, AMin, AMax: Integer): IGridLayoutFullResizer; reintroduce;
    function WithMaxGridHeight(AMax: Integer): IGridLayoutFullResizer; reintroduce;
    function WithMinGridHeight(AMin: Integer): IGridLayoutFullResizer; reintroduce;
    function WithMinAndMaxGridHeight(Amin, AMax: Integer): IGridLayoutFullResizer; reintroduce;
    property GridHeight: Integer read GetGridHeight write FGridHeight;


    function GetGridWidth: Integer;
    function WithGridWidth(ANewWidth: Integer): IGridLayoutFullResizer; reintroduce;
    function WithFixedColumns(const AFixedColumns: array of Integer): IGridLayoutFullResizer; reintroduce;
    function DisableFixedColumn(AColIndex: Integer): IGridLayoutFullResizer; overload; reintroduce;
    function EnableFixedColumn(AColIndex: Integer): IGridLayoutFullResizer; overload; reintroduce;
    function DisableFixedColumn(const AFixedColumns: array of Integer): IGridLayoutFullResizer; reintroduce; overload;
    function EnableFixedColumn(const AFixedColumns: array of Integer): IGridLayoutFullResizer; reintroduce; overload;
    function WithMaxColumnWidth(AColIndex, AMax: Integer): IGridLayoutFullResizer; reintroduce;
    function WithMinColumnWidth(AColIndex, AMin: Integer): IGridLayoutFullResizer; reintroduce;
    function WithMinAndMaxColumnWidth(AColIndex, AMin, AMax: Integer): IGridLayoutFullResizer; reintroduce;
    function WithMaxGridWidth(AMax: Integer): IGridLayoutFullResizer; reintroduce;
    function WithMinGridWidth(AMin: Integer): IGridLayoutFullResizer; reintroduce;
    function WithMinAndMaxGridWidth(Amin, AMax: Integer): IGridLayoutFullResizer; reintroduce;
    property GridWidth: Integer read GetGridWidth write FGridWidth;
  end;

implementation

uses
  Math;

{ TGridLayoutWidthResizer }

constructor TGridLayoutWidthResizer.Create;
begin
  FMinGridWidth := 0;
  FMaxGridWidth := MaxInt;
  FFixedColumns := TIntList.Create;
  FMinWidths := TIntIntDictionary.Create;
  FMaxWidths := TIntIntDictionary.Create;
end;

destructor TGridLayoutWidthResizer.Destroy;
begin
  FMaxWidths.Free;
  FMinWidths.Free;
  FFixedColumns.Free;
  inherited Destroy;
end;

function TGridLayoutWidthResizer.GetGridWidth: Integer;
begin
  Result := FGridWidth;
end;

procedure TGridLayoutWidthResizer.Resize(AGrid: TGridLayout);
var
  FixedWidth: Integer;
  FlexibleCols: TIntList;
  AvailableWidth, NewColWidth: Integer;
  I: Integer;

  function CalculateTotalSpacingWithMargins: Integer;
  var
    I, LastVisibleCol: Integer;
  begin
    LastVisibleCol := -1;
    for I := AGrid.Columns - 1 downto 0 do
      if AGrid.VisibleColumn[I] then
      begin
        LastVisibleCol := I;
        Break;
      end;

    if LastVisibleCol = -1 then
    begin
      Result := 0;   // se nao tem nenhuma coluna visivel, retorna sem margens
      Exit;
    end;

    Result := AGrid.Margins.Left + AGrid.Margins.Right;

    for I := 0 to LastVisibleCol - 1 do
      if AGrid.VisibleColumn[I] then
        Inc(Result, AGrid.HorizontalSpacing[I]);
  end;

begin
  if (AGrid.Columns = 0) or (FGridWidth <= 0) then
    Exit;

  // Calcular a largura já ocupada pelas colunas fixas
  FixedWidth := 0;
  FlexibleCols := TIntList.Create;
  try
    for I := 0 to AGrid.Columns - 1 do
    begin
      if not AGrid.VisibleColumn[I] then
        Continue;

      if (FFixedColumns.IndexOf(I) >= 0) then
        Inc(FixedWidth, AGrid.ColumnWidth[I])
      else
        FlexibleCols.Add(I);
    end;

    AvailableWidth := FGridWidth
      - FixedWidth
      - CalculateTotalSpacingWithMargins;

    if AvailableWidth <= 0 then
      Exit;

    if FlexibleCols.Count = 0 then
      Exit;

    NewColWidth := AvailableWidth div FlexibleCols.Count;

    for I in FlexibleCols do
      AGrid.ColumnWidth[I] := Max(NewColWidth, 1);

    ApplyColumnLimitsAndRedistribute(AGrid, FlexibleCols);
  finally
    FlexibleCols.Free;
  end;
end;

procedure TGridLayoutWidthResizer.ApplyColumnLimitsAndRedistribute(
  AGrid: TGridLayout; AFlexibleCols: TIntList);
var
  Col, MinW, MaxW: Integer;
  AdjustedCols: TIntList;
  Excess: Integer;
  DistWidth: Integer;
begin
  AdjustedCols := TIntList.Create;
  try
    Excess := 0;

    // Aplicar limites e acumular excesso
    for Col in AFlexibleCols do
    begin
      if not FMinWidths.TryGetValue(Col, MinW) then
        MinW := 1;
      if not FMaxWidths.TryGetValue(Col, MaxW) then
        MaxW := MaxInt-1;

      if AGrid.ColumnWidth[Col] < MinW then
      begin
        Excess := Excess - (MinW - AGrid.ColumnWidth[Col]); // agora é negativo
        AGrid.ColumnWidth[Col] := MinW;
      end
      else if AGrid.ColumnWidth[Col] > MaxW then
      begin
        Excess := Excess + (AGrid.ColumnWidth[Col] - MaxW); // positivo como antes
        AGrid.ColumnWidth[Col] := MaxW;
      end
      else
        AdjustedCols.Add(Col);
    end;

    if (Excess <> 0) and (AdjustedCols.Count > 0) then
    begin
      // Redistribuir o excesso entre colunas que ainda não atingiram o limite
      DistWidth := Excess div AdjustedCols.Count;
      for Col in AdjustedCols do
        AGrid.ColumnWidth[Col] := Max(AGrid.ColumnWidth[Col] + DistWidth, 0);
    end;
  finally
    AdjustedCols.Free;
  end;
end;

function TGridLayoutWidthResizer.WithGridWidth(ANewWidth: Integer
  ): IGridLayoutWidthResizer;
begin
  Result := Self;
  if ANewWidth < FMinGridWidth then
    FGridWidth := FMinGridWidth
  else if ANewWidth > FMaxGridWidth then
    FGridWidth := FMaxGridWidth
  else
    FGridWidth := ANewWidth;
end;

function TGridLayoutWidthResizer.WithFixedColumns
  (const AFixedColumns: array of Integer): IGridLayoutWidthResizer;
var
  I: Integer;
begin
  Result := Self;
  FFixedColumns.Clear;
  for I := 0 to High(AFixedColumns) do
    if FFixedColumns.IndexOf(I) = -1 then
      FFixedColumns.Add(AFixedColumns[I]);
end;

function TGridLayoutWidthResizer.EnableFixedColumn(
  AColIndex: Integer): IGridLayoutWidthResizer;
begin
  Result := Self;
  if FFixedColumns.IndexOf(AColIndex) = -1 then
    FFixedColumns.Add(AColIndex);
end;

function TGridLayoutWidthResizer.DisableFixedColumn
  (const AFixedColumns: array of Integer): IGridLayoutWidthResizer;
var
  I: Integer;
begin
  Result := Self;
  for I := 0 to High(AFixedColumns) do
    FFixedColumns.Remove(AFixedColumns[I]);
end;

function TGridLayoutWidthResizer.EnableFixedColumn
  (const AFixedColumns: array of Integer): IGridLayoutWidthResizer;
var
  I: Integer;
begin
  Result := Self;
  for I := 0 to High(AFixedColumns) do
    if FFixedColumns.IndexOf(I) = -1 then
      FFixedColumns.Add(AFixedColumns[I]);
end;

function TGridLayoutWidthResizer.DisableFixedColumn(
  AColIndex: Integer): IGridLayoutWidthResizer;
begin
  Result := Self;
  FFixedColumns.Remove(AColIndex);
end;

function TGridLayoutWidthResizer.WithMinColumnWidth(AColIndex, AMin: Integer
  ): IGridLayoutWidthResizer;
begin
  Result := Self;
  if not FMinWidths.ContainsKey(AColIndex) then
    FMinWidths.Add(AColIndex, AMin);
end;

function TGridLayoutWidthResizer.WithMinAndMaxColumnWidth
  (AColIndex, AMin, AMax: Integer): IGridLayoutWidthResizer;
begin
  Result := Self
    .WithMinColumnWidth(AColIndex, AMin)
    .WithMaxColumnWidth(AColIndex, AMax);
end;

function TGridLayoutWidthResizer.WithMaxGridWidth(AMax: Integer
  ): IGridLayoutWidthResizer;
begin
  Result := Self;
  FMaxGridWidth := AMax;
end;

function TGridLayoutWidthResizer.WithMinGridWidth(AMin: Integer
  ): IGridLayoutWidthResizer;
begin
  Result := Self;
  FMinGridWidth := AMin;
end;

function TGridLayoutWidthResizer.WithMinAndMaxGridWidth
  (Amin, AMax: Integer): IGridLayoutWidthResizer;
begin
  Result := Self
    .WithMinGridWidth(Amin)
    .WithMaxGridWidth(AMax)
end;

function TGridLayoutWidthResizer.WithMaxColumnWidth(AColIndex, AMax: Integer): IGridLayoutWidthResizer;
begin
  Result := Self;
  if not FMaxWidths.ContainsKey(AColIndex) then
    FMaxWidths.Add(AColIndex, AMax);
end;

{ TGridLayoutHeightResizer }

procedure TGridLayoutHeightResizer.ApplyRowLimitsAndRedistribute
  (AGrid: TGridLayout; AFlexibleRows: TIntList);
var
  Row, MinH, MaxH: Integer;
  AdjustedRows: TIntList;
  Excess: Integer;
  DistHeight: Integer;
begin
  AdjustedRows := TIntList.Create;
  try
    Excess := 0;

    // Aplicar limites e acumular excesso
    for Row in AFlexibleRows do
    begin
      if not FMinHeights.TryGetValue(Row, MinH) then
        MinH := 1;
      if not FMaxHeights.TryGetValue(Row, MaxH) then
        MaxH := MaxInt-1;

      if AGrid.RowHeight[Row] < MinH then
      begin
        Excess := Excess - (MinH - AGrid.RowHeight[Row]); // agora é negativo
        AGrid.RowHeight[Row] := MinH;
      end
      else if AGrid.RowHeight[Row] > MaxH then
      begin
        Excess := Excess + (AGrid.RowHeight[Row] - MaxH); // positivo como antes
        AGrid.RowHeight[Row] := MaxH;
      end
      else
        AdjustedRows.Add(Row);
    end;

    if (Excess <> 0) and (AdjustedRows.Count > 0) then
    begin
      // Redistribuir o excesso entre colunas que ainda não atingiram o limite
      DistHeight := Excess div AdjustedRows.Count;
      for Row in AdjustedRows do
        AGrid.RowHeight[Row] := Max(AGrid.RowHeight[Row] + DistHeight, 0);
    end;
  finally
    AdjustedRows.Free;
  end;
end;

constructor TGridLayoutHeightResizer.Create;
begin
  FMinGridHeight := 0;
  FMaxGridHeight := MaxInt;
  FFixedRows := TIntList.Create;
  FMinHeights := TIntIntDictionary.Create;
  FMaxHeights := TIntIntDictionary.Create;
end;

destructor TGridLayoutHeightResizer.Destroy;
begin
  FMaxHeights.Free;
  FMinHeights.Free;
  FFixedRows.Free;
  inherited Destroy;
end;


procedure TGridLayoutHeightResizer.Resize(AGrid: TGridLayout);
var
  FixedHeight: Integer;
  FlexibleRows: TIntList;
  AvailableHeight, NewRowHeight: Integer;
  I: Integer;

  function CalculateTotalSpacingWithMargins: Integer;
  var
    I, LastVisibleRow: Integer;
  begin
    LastVisibleRow := -1;
    for I := AGrid.Rows - 1 downto 0 do
      if AGrid.VisibleRow[I] then
      begin
        LastVisibleRow := I;
        Break;
      end;

    if LastVisibleRow = -1 then
    begin
      Result := 0;   // se nao tem nenhuma coluna visivel, retorna sem margens
      Exit;
    end;

    Result := AGrid.Margins.Left + AGrid.Margins.Right;

    for I := 0 to LastVisibleRow - 1 do
      if AGrid.VisibleRow[I] then
        Inc(Result, AGrid.VerticalSpacing[I]);
  end;

begin
  if (AGrid.Rows = 0) or (FGridHeight <= 0) then
    Exit;

  // Calcular a largura já ocupada pelas colunas fixas
  FixedHeight := 0;
  FlexibleRows := TIntList.Create;
  try
    for I := 0 to AGrid.Rows - 1 do
    begin
      if not AGrid.VisibleRow[I] then
        Continue;

      if (FFixedRows.IndexOf(I) >= 0) then
        Inc(FixedHeight, AGrid.RowHeight[I])
      else
        FlexibleRows.Add(I);
    end;

    AvailableHeight := FGridHeight
      - FixedHeight
      - CalculateTotalSpacingWithMargins;

    if AvailableHeight <= 0 then
      Exit;

    if FlexibleRows.Count = 0 then
      Exit;

    NewRowHeight := AvailableHeight div FlexibleRows.Count;

    for I in FlexibleRows do
      AGrid.RowHeight[I] := Max(NewRowHeight, 1);

    ApplyRowLimitsAndRedistribute(AGrid, FlexibleRows);
  finally
    FlexibleRows.Free;
  end;
end;

function TGridLayoutHeightResizer.GetGridHeight: Integer;
begin
  Result := FGridHeight;
end;

function TGridLayoutHeightResizer.WithGridHeight(ANewHeight: Integer
  ): IGridLayoutHeightResizer;
begin
  Result := Self;
  if ANewHeight < FMinGridHeight then
    FGridHeight := FMinGridHeight
  else if ANewHeight > FMaxGridHeight then
    FGridHeight := FMaxGridHeight
  else
    FGridHeight := ANewHeight;
end;

function TGridLayoutHeightResizer.WithFixedRows
  (const AFixedRows: array of Integer): IGridLayoutHeightResizer;
var
  I: Integer;
begin
  Result := Self;
  FFixedRows.Clear;
  for I := 0 to High(AFixedRows) do
    if FFixedRows.IndexOf(I) = -1 then
      FFixedRows.Add(AFixedRows[I]);
end;

function TGridLayoutHeightResizer.DisableFixedRow(ARowIndex: Integer
  ): IGridLayoutHeightResizer;
begin
  Result := Self;
  FFixedRows.Remove(ARowIndex);
end;

function TGridLayoutHeightResizer.EnableFixedRow(ARowIndex: Integer
  ): IGridLayoutHeightResizer;
begin
  Result := Self;
  if FFixedRows.IndexOf(ARowIndex) = -1 then
    FFixedRows.Add(ARowIndex);
end;

function TGridLayoutHeightResizer.DisableFixedRow
  (const AFixedRows: array of Integer): IGridLayoutHeightResizer;
var
  I: Integer;
begin
  Result := Self;
  for I := 0 to High(AFixedRows) do
    FFixedRows.Remove(AFixedRows[I]);
end;

function TGridLayoutHeightResizer.EnableFixedRow
  (const AFixedRows: array of Integer): IGridLayoutHeightResizer;
var
  I: Integer;
begin
  Result := Self;
  for I := 0 to High(AFixedRows) do
    if FFixedRows.IndexOf(I) = -1 then
      FFixedRows.Add(AFixedRows[I]);
end;

function TGridLayoutHeightResizer.WithMaxRowHeight(ARowIndex, AMax: Integer
  ): IGridLayoutHeightResizer;
begin
  Result := Self;
  if not FMaxHeights.ContainsKey(ARowIndex) then
    FMaxHeights.Add(ARowIndex, AMax);
end;

function TGridLayoutHeightResizer.WithMinRowHeight(ARowIndex, AMin: Integer
  ): IGridLayoutHeightResizer;
begin
  Result := Self;
  if not FMinHeights.ContainsKey(ARowIndex) then
    FMinHeights.Add(ARowIndex, AMin);
end;

function TGridLayoutHeightResizer.WithMinAndMaxRowHeight
  (ARowIndex, AMin, AMax: Integer): IGridLayoutHeightResizer;
begin
  Result := Self
    .WithMinRowHeight(ARowIndex, AMin)
    .WithMaxRowHeight(ARowIndex, AMax);
end;

function TGridLayoutHeightResizer.WithMaxGridHeight(AMax: Integer
  ): IGridLayoutHeightResizer;
begin
  Result := Self;
  FMaxGridHeight := AMax;
end;

function TGridLayoutHeightResizer.WithMinGridHeight(AMin: Integer
  ): IGridLayoutHeightResizer;
begin
  Result := Self;
  FMinGridHeight := AMin;
end;

function TGridLayoutHeightResizer.WithMinAndMaxGridHeight(Amin, AMax: Integer
  ): IGridLayoutHeightResizer;
begin
  Result := Self
    .WithMinGridHeight(Amin)
    .WithMaxGridHeight(AMax)
end;

{ TGridLayoutFullResizer }

constructor TGridLayoutFullResizer.Create;
begin
  FHeightResizer := TGridLayoutHeightResizer.Create;
  FWidthResizer := TGridLayoutWidthResizer.Create;
end;

procedure TGridLayoutFullResizer.Resize(AGrid: TGridLayout);
begin
  FWidthResizer.Resize(AGrid);
  FHeightResizer.Resize(AGrid);
end;

function TGridLayoutFullResizer.GetGridHeight: Integer;
begin
  Result := FGridHeight;
end;

function TGridLayoutFullResizer.WithGridHeight(ANewHeight: Integer
  ): IGridLayoutFullResizer;
begin
  Result := Self;
  FHeightResizer.WithGridHeight(ANewHeight)
end;

function TGridLayoutFullResizer.WithFixedRows(const AFixedRows: array of Integer
  ): IGridLayoutFullResizer;
begin
  Result := Self;
  FHeightResizer.WithFixedRows(AFixedRows);
end;

function TGridLayoutFullResizer.DisableFixedRow(ARowIndex: Integer
  ): IGridLayoutFullResizer;
begin
  Result := Self;
  FHeightResizer.DisableFixedRow(ARowIndex);
end;

function TGridLayoutFullResizer.EnableFixedRow(ARowIndex: Integer
  ): IGridLayoutFullResizer;
begin
  Result := Self;
  FHeightResizer.EnableFixedRow(ARowIndex);
end;

function TGridLayoutFullResizer.DisableFixedRow
  (const AFixedRows: array of Integer): IGridLayoutFullResizer;
begin
  Result := Self;
  FHeightResizer.DisableFixedRow(AFixedRows);
end;

function TGridLayoutFullResizer.EnableFixedRow
  (const AFixedRows: array of Integer): IGridLayoutFullResizer;
begin
  Result := Self;
  FHeightResizer.EnableFixedRow(AFixedRows);
end;

function TGridLayoutFullResizer.WithMaxRowHeight(ARowIndex, AMax: Integer
  ): IGridLayoutFullResizer;
begin
  Result := Self;
  FHeightResizer.WithMaxRowHeight(ARowIndex, AMax);
end;

function TGridLayoutFullResizer.WithMinRowHeight(ARowIndex, AMin: Integer
  ): IGridLayoutFullResizer;
begin
  Result := Self;
  FHeightResizer.WithMinRowHeight(ARowIndex, AMin);
end;

function TGridLayoutFullResizer.WithMinAndMaxRowHeight
  (ARowIndex, AMin, AMax: Integer): IGridLayoutFullResizer;
begin
  Result := Self;
  FHeightResizer.WithMinAndMaxRowHeight(ARowIndex, AMin, AMax);
end;

function TGridLayoutFullResizer.WithMaxGridHeight(AMax: Integer
  ): IGridLayoutFullResizer;
begin
  Result := Self;
  FHeightResizer.WithMaxGridHeight(AMax);
end;

function TGridLayoutFullResizer.WithMinGridHeight(AMin: Integer
  ): IGridLayoutFullResizer;
begin
  Result := Self;
  FHeightResizer.WithMinGridHeight(AMin);
end;

function TGridLayoutFullResizer.WithMinAndMaxGridHeight(Amin, AMax: Integer
  ): IGridLayoutFullResizer;
begin
  Result := Self;
  FHeightResizer.WithMinAndMaxGridHeight(Amin, AMax);
end;

function TGridLayoutFullResizer.GetGridWidth: Integer;
begin
  Result := FGridWidth;
end;

function TGridLayoutFullResizer.WithGridWidth(ANewWidth: Integer
  ): IGridLayoutFullResizer;
begin
  Result := Self;
  FWidthResizer.WithGridWidth(ANewWidth);
end;

function TGridLayoutFullResizer.WithFixedColumns
  (const AFixedColumns: array of Integer): IGridLayoutFullResizer;
begin
  Result := Self;
  FWidthResizer.WithFixedColumns(AFixedColumns);
end;

function TGridLayoutFullResizer.DisableFixedColumn(AColIndex: Integer
  ): IGridLayoutFullResizer;
begin
  Result := Self;
  FWidthResizer.DisableFixedColumn(AColIndex);
end;

function TGridLayoutFullResizer.EnableFixedColumn(AColIndex: Integer
  ): IGridLayoutFullResizer;
begin
  Result := Self;
  FWidthResizer.EnableFixedColumn(AColIndex);
end;

function TGridLayoutFullResizer.DisableFixedColumn
  (const AFixedColumns: array of Integer): IGridLayoutFullResizer;
begin
  Result := Self;
  FWidthResizer.DisableFixedColumn(AFixedColumns);
end;

function TGridLayoutFullResizer.EnableFixedColumn
  (const AFixedColumns: array of Integer): IGridLayoutFullResizer;
begin
  Result := Self;
  FWidthResizer.EnableFixedColumn(AFixedColumns);
end;

function TGridLayoutFullResizer.WithMaxColumnWidth(AColIndex, AMax: Integer
  ): IGridLayoutFullResizer;
begin
  Result := Self;
  FWidthResizer.WithMaxColumnWidth(AColIndex, AMax);
end;

function TGridLayoutFullResizer.WithMinColumnWidth(AColIndex, AMin: Integer
  ): IGridLayoutFullResizer;
begin
  Result := Self;
  FWidthResizer.WithMinColumnWidth(AColIndex, AMin);
end;

function TGridLayoutFullResizer.WithMinAndMaxColumnWidth
  (AColIndex, AMin, AMax: Integer): IGridLayoutFullResizer;
begin
  Result := Self;
  FWidthResizer.WithMinAndMaxColumnWidth(AColIndex, AMin, AMax);
end;

function TGridLayoutFullResizer.WithMaxGridWidth(AMax: Integer
  ): IGridLayoutFullResizer;
begin
  Result := Self;
  FWidthResizer.WithMaxGridWidth(AMax);
end;

function TGridLayoutFullResizer.WithMinGridWidth(AMin: Integer
  ): IGridLayoutFullResizer;
begin
  Result := Self;
  FWidthResizer.WithMinGridWidth(AMin);
end;

function TGridLayoutFullResizer.WithMinAndMaxGridWidth(Amin, AMax: Integer
  ): IGridLayoutFullResizer;
begin
  Result := Self;
  FWidthResizer.WithMinAndMaxGridWidth(Amin, AMax);
end;

end.

