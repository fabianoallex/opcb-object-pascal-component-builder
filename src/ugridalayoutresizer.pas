unit UGridaLayoutResizer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ULayout, Generics.Collections;

type
  TIntList = specialize TList<Integer>;
  TIntIntDictionary = specialize TDictionary<Integer, Integer>;

  { TValueRange }

  TValueRangeMode = (vrmFloor, vrmCeil, vrmNearest);
  TValueRange = class
  private
    FValues: TIntList;
    FMode: TValueRangeMode;
    FFreeOnSelect: Boolean;
    function GetFloor(Value: Integer): Integer;
    function GeCeil(Value: Integer): Integer;
    function GetNearest(Value: Integer): Integer;
  public
    constructor Create(AValues: TIntList; AMode: TValueRangeMode;
      AFreeOnSelect: Boolean=True);
    destructor Destroy; override;
    function Select(AValue: Integer): Integer;
    property Values: TIntList read FValues;
    property Mode: TValueRangeMode read FMode;
  end;

  TIntTValueRangeDictionary = specialize TDictionary<Integer, TValueRange>;

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
    function WithWidthRange(AColIndex: Integer; const AValues: array of Integer;
      ASelectRangeMode: TValueRangeMode): IGridLayoutWidthResizer;
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
    function WithHeightRange(ARowIndex: Integer; const AValues: array of Integer;
      ASelectRangeMode: TValueRangeMode): IGridLayoutHeightResizer;
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

  { TGridLayoutResizerBase }

  TGridLayoutResizerBase = class abstract(TInterfacedObject, IGridLayoutResizer)
  protected
    FMinTotalSize: Integer;
    FMaxTotalSize: Integer;
    FTotalSize: Integer;
    FFixedIndexes: TIntList;
    FMinSizes: TIntIntDictionary;
    FMaxSizes: TIntIntDictionary;
    FSizesRanges: TIntTValueRangeDictionary;
    procedure ApplyLimitsAndRedistribute(AGrid: TGridLayout; AFlexibleIndexes: TIntList);
    function GetItemCount(AGrid: TGridLayout): Integer; virtual; abstract;
    function GetVisibleItem(AGrid: TGridLayout; AIndex: Integer): Boolean; virtual; abstract;
    function GetItemSize(AGrid: TGridLayout; AIndex: Integer): Integer; virtual; abstract;
    procedure SetItemSize(AGrid: TGridLayout; AIndex, ASize: Integer); virtual; abstract;
    function GetSpacing(AGrid: TGridLayout; AIndex: Integer): Integer; virtual; abstract;
    function GetTotalMargin(AGrid: TGridLayout): Integer; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Resize(AGrid: TGridLayout);
  end;

  { TGridLayoutWidthResizer }

  TGridLayoutWidthResizer = class(TGridLayoutResizerBase, IGridLayoutWidthResizer)
  protected
    function GetItemCount(AGrid: TGridLayout): Integer; override;
    function GetVisibleItem(AGrid: TGridLayout; AIndex: Integer): Boolean; override;
    function GetItemSize(AGrid: TGridLayout; AIndex: Integer): Integer; override;
    procedure SetItemSize(AGrid: TGridLayout; AIndex, ASize: Integer); override;
    function GetSpacing(AGrid: TGridLayout; AIndex: Integer): Integer; override;
    function GetTotalMargin(AGrid: TGridLayout): Integer; override;
  public
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
    function WithWidthRange(AColIndex: Integer; const AValues: array of Integer;
      ASelectRangeMode: TValueRangeMode): IGridLayoutWidthResizer;
    property GridWidth: Integer read GetGridWidth write FTotalSize;
  end;




  { TGridLayoutHeightResizer }

  TGridLayoutHeightResizer = class(TGridLayoutResizerBase, IGridLayoutHeightResizer)
  protected
    function GetItemCount(AGrid: TGridLayout): Integer; override;
    function GetVisibleItem(AGrid: TGridLayout; AIndex: Integer): Boolean; override;
    function GetItemSize(AGrid: TGridLayout; AIndex: Integer): Integer; override;
    procedure SetItemSize(AGrid: TGridLayout; AIndex, ASize: Integer); override;
    function GetSpacing(AGrid: TGridLayout; AIndex: Integer): Integer; override;
    function GetTotalMargin(AGrid: TGridLayout): Integer; override;
  public
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
    function WithHeightRange(ARowIndex: Integer; const AValues: array of Integer;
      ASelectRangeMode: TValueRangeMode): IGridLayoutHeightResizer;

    property GridHeight: Integer read GetGridHeight write FTotalSize;
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

{ TValueRange }

constructor TValueRange.Create(AValues: TIntList; AMode: TValueRangeMode;
  AFreeOnSelect: Boolean);
begin
  inherited Create;
  FMode := AMode;
  FValues := AValues;
  FFreeOnSelect := AFreeOnSelect;
end;

destructor TValueRange.Destroy;
begin
  inherited Destroy;
end;

function TValueRange.GetFloor(Value: Integer): Integer;
var
  I: Integer;
begin
  for I := 0 to FValues.Count-1 do
    if FValues[I] >= Value then
      Exit(FValues[I]);

  Result := FValues.Last;
end;

function TValueRange.GeCeil(Value: Integer): Integer;
var
  I: Integer;
begin
  for I := FValues.Count - 1 downto 0 do
    if FValues[I] <= Value then
      Exit(FValues[I]);

  Result := FValues.First;
end;

function TValueRange.GetNearest(Value: Integer): Integer;
var
  Lower, Upper: Integer;
begin
  if Value <= FValues.First then
    Exit(FValues.First)
  else if Value >= FValues.Last then
    Exit(FValues.Last);

  Lower := GeCeil(Value);
  Upper := GetFloor(Value);

  if Abs(Upper - Value) < Abs(Lower - Value) then
    Result := Upper
  else
    Result := Lower;
end;

function TValueRange.Select(AValue: Integer): Integer;
begin
  case FMode of
    vrmNearest:
      Result := GetNearest(AValue);
    vrmFloor:
      Result := GeCeil(AValue);
    vrmCeil:
      Result := GetFloor(AValue);
  else
    Result := AValue; // fallback
  end;

  if FFreeOnSelect then
    Free;
end;

{ TGridLayoutResizerBase }

procedure TGridLayoutResizerBase.ApplyLimitsAndRedistribute(AGrid: TGridLayout;
  AFlexibleIndexes: TIntList);
var
  Index, MinS, MaxS, Range: Integer;
  AdjustedIndexes: TIntList;
  Excess: Integer;
  Dist: Integer;

  function ApplyRange(Index: Integer): Integer;
  begin
    if FSizesRanges.ContainsKey(Index) then
      Result := FSizesRanges[Index].Select(GetItemSize(AGrid, Index))  // AGrid.ColumnWidth[Col]
    else
      Result := GetItemSize(AGrid, Index); // AGrid.ColumnWidth[Col];
  end;

  function ApplyMaxValue(Index: Integer): Integer;
  begin
    if not FMaxSizes.TryGetValue(Index, Result) then  // if not FMaxWidths.TryGetValue(Col, Result) then
      Result := MaxInt;
  end;

  function ApplyMinValue(Index: Integer): Integer;
  begin
    if not FMinSizes.TryGetValue(Index, Result) then  // if not FMinWidths.TryGetValue(Col, Result) then
      Result := 0;
  end;

begin
  AdjustedIndexes := TIntList.Create;
  try
    Excess := 0;

    // Aplicar limites e acumular excesso
    for Index in AFlexibleIndexes do
    begin
      MinS := ApplyMinValue(Index);
      MaxS := ApplyMaxValue(Index);
      Range := ApplyRange(Index);

      if GetItemSize(AGrid, Index) < MinS then    // if AGrid.ColumnWidth[Col] < MinW then
      begin
        Excess := Excess - (MinS - GetItemSize(AGrid, Index));  // Excess := Excess - (MinS - AGrid.ColumnWidth[Col]);
        SetItemSize(AGrid, Index, MinS);  //AGrid.ColumnWidth[Col] := MinW;
      end
      else if GetItemSize(AGrid, Index) > MaxS then  // else if AGrid.ColumnWidth[Col] > MaxW then
      begin
        Excess := Excess + (GetItemSize(AGrid, Index) - MaxS);  // Excess := Excess + (AGrid.ColumnWidth[Col] - MaxW);
        SetItemSize(AGrid, Index, MaxS);   // AGrid.ColumnWidth[Col] := MaxW;
      end
      else if GetItemSize(AGrid, Index) <> Range then    // else if AGrid.ColumnWidth[Col] <> Range then
      begin
        Excess := Excess + (GetItemSize(AGrid, Index) - Range);  // Excess := Excess + (AGrid.ColumnWidth[Col] - Range);
        SetItemSize(AGrid, Index, Range);  // AGrid.ColumnWidth[Col] := Range;
      end
      else
        AdjustedIndexes.Add(Index);
    end;

    if (Excess <> 0) and (AdjustedIndexes.Count > 0) then
    begin
      Dist := Excess div AdjustedIndexes.Count;
      for Index in AdjustedIndexes do
        SetItemSize(
          AGrid,
          Index,
          Max(GetItemSize(AGrid, Index) + Dist, 0)
        );                                         // AGrid.ColumnWidth[Col] := Max(AGrid.ColumnWidth[Col] + DistWidth, 0);
    end;
  finally
    AdjustedIndexes.Free;
  end;
end;

constructor TGridLayoutResizerBase.Create;
begin
  FMinTotalSize := 0;
  FMaxTotalSize := MaxInt;
  FFixedIndexes := TIntList.Create;
  FMinSizes := TIntIntDictionary.Create;
  FMaxSizes := TIntIntDictionary.Create;
  FSizesRanges := TIntTValueRangeDictionary.Create;
end;

destructor TGridLayoutResizerBase.Destroy;
var
  Pair: specialize TPair<Integer, TValueRange>;
begin
  FMaxSizes.Free;
  FMinSizes.Free;
  FFixedIndexes.Free;
  for Pair in FSizesRanges do
    Pair.Value.Free;
  FSizesRanges.Free;
  inherited Destroy;
end;

procedure TGridLayoutResizerBase.Resize(AGrid: TGridLayout);
var
  FixedSize: Integer;
  FlexibleIndexes: TIntList;
  AvailableSize, NewSize: Integer;
  I: Integer;

  function CalculateTotalSpacingWithMargins: Integer;
  var
    I, LastVisibleIndex: Integer;
  begin
    LastVisibleIndex := -1;
    for I := GetItemCount(AGrid) downto 0 do   // for I := AGrid.Columns - 1 downto 0 do
      if GetVisibleItem(AGrid, I) then   // if AGrid.VisibleColumn[I] then
      begin
        LastVisibleIndex := I;
        Break;
      end;

    if LastVisibleIndex = -1 then
    begin
      Result := 0;   // se nao tem nenhuma coluna visivel, retorna sem margens
      Exit;
    end;

    Result := GetTotalMargin(AGrid);  // AGrid.Margins.Left + AGrid.Margins.Right;

    for I := 0 to LastVisibleIndex - 1 do
      if GetVisibleItem(AGrid, I) then  //  if AGrid.VisibleColumn[I] then
        Inc(Result, GetSpacing(AGrid, I));  //Inc(Result, AGrid.HorizontalSpacing[I]);
  end;

begin
  if (GetItemCount(AGrid) = 0) or (FTotalSize <= 0) then  // if (AGrid.Columns = 0) or (FGridWidth <= 0) then
    Exit;

  // Calcular a largura já ocupada pelas colunas fixas
  FixedSize := 0;
  FlexibleIndexes := TIntList.Create;
  try
    for I := 0 to GetItemCount(AGrid) - 1 do   // for I := 0 to AGrid.Columns - 1 do
    begin
      if not GetVisibleItem(AGrid, I) then   // if not AGrid.VisibleColumn[I] then
        Continue;

      if (FlexibleIndexes.IndexOf(I) >= 0) then
        Inc(FixedSize, GetItemSize(AGrid, I))  // AGrid.ColumnWidth[I]
      else
        FlexibleIndexes.Add(I);
    end;

    AvailableSize := FTotalSize
      - FixedSize
      - CalculateTotalSpacingWithMargins;

    if AvailableSize <= 0 then
      Exit;

    if FlexibleIndexes.Count = 0 then
      Exit;

    NewSize := AvailableSize div FlexibleIndexes.Count;

    for I in FlexibleIndexes do
      SetItemSize(AGrid, I, Max(NewSize, 1));  // AGrid.ColumnWidth[I] := Max(NewColWidth, 1);

    ApplyLimitsAndRedistribute(AGrid, FlexibleIndexes);
  finally
    FlexibleIndexes.Free;
  end;
end;

{ TGridLayoutWidthResizer }

function TGridLayoutWidthResizer.GetItemCount(AGrid: TGridLayout): Integer;
begin
  Result := AGrid.Columns;
end;

function TGridLayoutWidthResizer.GetVisibleItem(AGrid: TGridLayout;
  AIndex: Integer): Boolean;
begin
  Result := AGrid.VisibleColumn[AIndex];
end;

function TGridLayoutWidthResizer.GetItemSize(AGrid: TGridLayout;
  AIndex: Integer): Integer;
begin
  Result := AGrid.ColumnWidth[AIndex];
end;

procedure TGridLayoutWidthResizer.SetItemSize(AGrid: TGridLayout;
  AIndex, ASize: Integer);
begin
  AGrid.ColumnWidth[AIndex] := ASize;
end;

function TGridLayoutWidthResizer.GetSpacing(AGrid: TGridLayout;
  AIndex: Integer): Integer;
begin
  Result := AGrid.HorizontalSpacing[AIndex];
end;

function TGridLayoutWidthResizer.GetTotalMargin(AGrid: TGridLayout
  ): Integer;
begin
  Result := AGrid.Margins.Left + AGrid.Margins.Right;
end;

function TGridLayoutWidthResizer.GetGridWidth: Integer;
begin
  Result := FTotalSize;
end;

function TGridLayoutWidthResizer.WithGridWidth(ANewWidth: Integer
  ): IGridLayoutWidthResizer;
begin
  Result := Self;
  if ANewWidth < FMinTotalSize then
    FTotalSize := FMinTotalSize
  else if ANewWidth > FMaxTotalSize then
    FTotalSize := FMaxTotalSize
  else
    FTotalSize := ANewWidth;
end;

function TGridLayoutWidthResizer.WithFixedColumns
  (const AFixedColumns: array of Integer): IGridLayoutWidthResizer;
var
  I: Integer;
begin
  Result := Self;
  FFixedIndexes.Clear;
  for I := 0 to High(AFixedColumns) do
    if FFixedIndexes.IndexOf(I) = -1 then
      FFixedIndexes.Add(AFixedColumns[I]);
end;

function TGridLayoutWidthResizer.DisableFixedColumn(AColIndex: Integer
  ): IGridLayoutWidthResizer;
begin
  Result := Self;
  FFixedIndexes.Remove(AColIndex);
end;

function TGridLayoutWidthResizer.EnableFixedColumn(AColIndex: Integer
  ): IGridLayoutWidthResizer;
begin
  Result := Self;
  if FFixedIndexes.IndexOf(AColIndex) = -1 then
    FFixedIndexes.Add(AColIndex);
end;

function TGridLayoutWidthResizer.DisableFixedColumn
  (const AFixedColumns: array of Integer): IGridLayoutWidthResizer;
var
  I: Integer;
begin
  Result := Self;
  for I := 0 to High(AFixedColumns) do
    FFixedIndexes.Remove(AFixedColumns[I]);
end;

function TGridLayoutWidthResizer.EnableFixedColumn
  (const AFixedColumns: array of Integer): IGridLayoutWidthResizer;
var
  I: Integer;
begin
  Result := Self;
  for I := 0 to High(AFixedColumns) do
    if FFixedIndexes.IndexOf(I) = -1 then
      FFixedIndexes.Add(AFixedColumns[I]);
end;

function TGridLayoutWidthResizer.WithMaxColumnWidth(AColIndex, AMax: Integer
  ): IGridLayoutWidthResizer;
begin
  Result := Self;
  if not FMaxSizes.ContainsKey(AColIndex) then
    FMaxSizes.Add(AColIndex, AMax);
end;

function TGridLayoutWidthResizer.WithMinColumnWidth(AColIndex, AMin: Integer
  ): IGridLayoutWidthResizer;
begin
  Result := Self;
  if not FMinSizes.ContainsKey(AColIndex) then
    FMinSizes.Add(AColIndex, AMin);
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
  FMaxTotalSize := AMax;
end;

function TGridLayoutWidthResizer.WithMinGridWidth(AMin: Integer
  ): IGridLayoutWidthResizer;
begin
  Result := Self;
  FMinTotalSize := AMin;
end;

function TGridLayoutWidthResizer.WithMinAndMaxGridWidth(Amin, AMax: Integer
  ): IGridLayoutWidthResizer;
begin
  Result := Self
    .WithMinGridWidth(Amin)
    .WithMaxGridWidth(AMax);
end;

function TGridLayoutWidthResizer.WithWidthRange(AColIndex: Integer;
  const AValues: array of Integer; ASelectRangeMode: TValueRangeMode
  ): IGridLayoutWidthResizer;
var
  I: Integer;
  List: TIntList;
begin
  Result := Self;

  List := TIntList.Create;
  for I:=Low(AValues) to High(AValues) do
    if not List.Contains(AValues[I]) then
      List.Add(AValues[I]);
  List.Sort;

  FSizesRanges.AddOrSetValue(
    AColIndex,
    TValueRange.Create(List, ASelectRangeMode, False)
  );
end;

{ TGridLayoutHeightResizer }

function TGridLayoutHeightResizer.GetItemCount(AGrid: TGridLayout): Integer;
begin
  Result := AGrid.Rows;
end;

function TGridLayoutHeightResizer.GetVisibleItem(AGrid: TGridLayout;
  AIndex: Integer): Boolean;
begin
  Result := AGrid.VisibleRow[AIndex];
end;

function TGridLayoutHeightResizer.GetItemSize(AGrid: TGridLayout;
  AIndex: Integer): Integer;
begin
  Result := AGrid.RowHeight[AIndex];
end;

procedure TGridLayoutHeightResizer.SetItemSize(AGrid: TGridLayout;
  AIndex, ASize: Integer);
begin
  AGrid.RowHeight[AIndex] := ASize;
end;

function TGridLayoutHeightResizer.GetSpacing(AGrid: TGridLayout;
  AIndex: Integer): Integer;
begin
  Result := AGrid.VerticalSpacing[AIndex];
end;

function TGridLayoutHeightResizer.GetTotalMargin(AGrid: TGridLayout
  ): Integer;
begin
  Result := AGrid.Margins.Top + AGrid.Margins.Bottom;
end;

function TGridLayoutHeightResizer.GetGridHeight: Integer;
begin
  Result := FTotalSize;
end;

function TGridLayoutHeightResizer.WithGridHeight(ANewHeight: Integer
  ): IGridLayoutHeightResizer;
begin
  Result := Self;
  if ANewHeight < FMinTotalSize then
    FTotalSize := FMinTotalSize
  else if ANewHeight > FMaxTotalSize then
    FTotalSize := FMaxTotalSize
  else
    FTotalSize := ANewHeight;
end;

function TGridLayoutHeightResizer.WithFixedRows
  (const AFixedRows: array of Integer): IGridLayoutHeightResizer;
var
  I: Integer;
begin
  Result := Self;
  FFixedIndexes.Clear;
  for I := 0 to High(AFixedRows) do
    if FFixedIndexes.IndexOf(I) = -1 then
      FFixedIndexes.Add(AFixedRows[I]);
end;

function TGridLayoutHeightResizer.DisableFixedRow(ARowIndex: Integer
  ): IGridLayoutHeightResizer;
begin
  Result := Self;
  FFixedIndexes.Remove(ARowIndex);
end;

function TGridLayoutHeightResizer.EnableFixedRow(ARowIndex: Integer
  ): IGridLayoutHeightResizer;
begin
  Result := Self;
  if FFixedIndexes.IndexOf(ARowIndex) = -1 then
    FFixedIndexes.Add(ARowIndex);
end;

function TGridLayoutHeightResizer.DisableFixedRow
  (const AFixedRows: array of Integer): IGridLayoutHeightResizer;
var
  I: Integer;
begin
  Result := Self;
  for I := 0 to High(AFixedRows) do
    FFixedIndexes.Remove(AFixedRows[I]);
end;

function TGridLayoutHeightResizer.EnableFixedRow
  (const AFixedRows: array of Integer): IGridLayoutHeightResizer;
var
  I: Integer;
begin
  Result := Self;
  for I := 0 to High(AFixedRows) do
    if FFixedIndexes.IndexOf(I) = -1 then
      FFixedIndexes.Add(AFixedRows[I]);
end;

function TGridLayoutHeightResizer.WithMaxRowHeight(ARowIndex, AMax: Integer
  ): IGridLayoutHeightResizer;
begin
  Result := Self;
  if not FMaxSizes.ContainsKey(ARowIndex) then
    FMaxSizes.Add(ARowIndex, AMax);
end;

function TGridLayoutHeightResizer.WithMinRowHeight(ARowIndex, AMin: Integer
  ): IGridLayoutHeightResizer;
begin
  Result := Self;
  if not FMinSizes.ContainsKey(ARowIndex) then
    FMinSizes.Add(ARowIndex, AMin);
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
  FMaxTotalSize := AMax;
end;

function TGridLayoutHeightResizer.WithMinGridHeight(AMin: Integer
  ): IGridLayoutHeightResizer;
begin
  Result := Self;
  FMinTotalSize := AMin;
end;

function TGridLayoutHeightResizer.WithMinAndMaxGridHeight
  (Amin, AMax: Integer): IGridLayoutHeightResizer;
begin
  Result := Self
    .WithMinGridHeight(AMin)
    .WithMaxGridHeight(AMax);
end;

function TGridLayoutHeightResizer.WithHeightRange(ARowIndex: Integer;
  const AValues: array of Integer; ASelectRangeMode: TValueRangeMode
  ): IGridLayoutHeightResizer;
var
  I: Integer;
  List: TIntList;
begin
  Result := Self;

  List := TIntList.Create;
  for I:=Low(AValues) to High(AValues) do
    if not List.Contains(AValues[I]) then
      List.Add(AValues[I]);
  List.Sort;

  FSizesRanges.AddOrSetValue(
    ARowIndex,
    TValueRange.Create(List, ASelectRangeMode, False)
  );
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

