unit UGridaLayoutResizer;

{$IFDEF FPC}
{$mode ObjFPC}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, ULayout, Generics.Collections;

type
  TIntList = {$IFDEF FPC}specialize{$ENDIF} TList<Integer>;
  TSingleList = {$IFDEF FPC}specialize{$ENDIF} TList<Single>;
  TIntIntDictionary = {$IFDEF FPC}specialize{$ENDIF} TDictionary<Integer, Integer>;
  TIntSingleDictionary = {$IFDEF FPC}specialize{$ENDIF} TDictionary<Integer, Single>;

  { TValueRange }

  TValueRangeMode = (vrmFloor, vrmCeil, vrmNearest);
  TValueRange = class
  private
    FValues: TSingleList;
    FMode: TValueRangeMode;
    FFreeOnSelect: Boolean;
    function GetFloor(Value: Single): Single;
    function GeCeil(Value: Single): Single;
    function GetNearest(Value: Single): Single;
  public
    constructor Create(AValues: TSingleList; AMode: TValueRangeMode;
      AFreeOnSelect: Boolean=True);
    destructor Destroy; override;
    function Select(AValue: Single): Single;
    property Values: TSingleList read FValues;
    property Mode: TValueRangeMode read FMode;
  end;

  TIntTValueRangeDictionary = {$IFDEF FPC}specialize{$ENDIF} TDictionary<Integer, TValueRange>;
  TSingleTValueRangeDictionary = {$IFDEF FPC}specialize{$ENDIF} TDictionary<Single, TValueRange>;

  IGridResizer = interface
    ['{CDDFEEED-72B4-4699-99AA-23714658C2DE}']
    procedure Resize(AGrid: TGridLayout);
  end;

  { IGridWidthResizer }

  IGridWidthResizer = interface(IGridResizer)
    ['{8F19CDBF-F294-4F4E-B1EE-3A844BEDA43C}']
    function GetGridWidth: Single;
    function WithGridWidth(ANewWidth: Single): IGridWidthResizer;
    function WithFixedColumns(const AFixedColumns: array of Integer): IGridWidthResizer;
    function DisableFixedColumn(AColIndex: Integer): IGridWidthResizer; overload;
    function EnableFixedColumn(AColIndex: Integer): IGridWidthResizer; overload;
    function DisableFixedColumn(const AFixedColumns: array of Integer): IGridWidthResizer; overload;
    function EnableFixedColumn(const AFixedColumns: array of Integer): IGridWidthResizer; overload;
    function WithMaxColumnWidth(AColIndex: Integer; AMax: Single): IGridWidthResizer;
    function WithMinColumnWidth(AColIndex: Integer; AMin: Single): IGridWidthResizer;
    function WithMinAndMaxColumnWidth(AColIndex: Integer; AMin, AMax: Single): IGridWidthResizer;
    function WithMaxGridWidth(AMax: Single): IGridWidthResizer;
    function WithMinGridWidth(AMin: Single): IGridWidthResizer;
    function WithMinAndMaxGridWidth(Amin, AMax: Single): IGridWidthResizer;
    function WithWidthRange(AColIndex: Integer; const AValues: array of Single;
      ASelectRangeMode: TValueRangeMode): IGridWidthResizer;
    property GridWidth: Single read GetGridWidth;
  end;

  { IGridHeightResizer }

  IGridHeightResizer = interface(IGridResizer)
    ['{F8FC03F3-843B-4D84-8CDE-2B1F77425078}']
    function GetGridHeight: Single;
    function WithGridHeight(ANewHeight: Single): IGridHeightResizer;
    function WithFixedRows(const AFixedRows: array of Integer): IGridHeightResizer;
    function DisableFixedRow(ARowIndex: Integer): IGridHeightResizer; overload;
    function EnableFixedRow(ARowIndex: Integer): IGridHeightResizer; overload;
    function DisableFixedRow(const AFixedRows: array of Integer): IGridHeightResizer; overload;
    function EnableFixedRow(const AFixedRows: array of Integer): IGridHeightResizer; overload;
    function WithMaxRowHeight(ARowIndex: Integer; AMax: Single): IGridHeightResizer;
    function WithMinRowHeight(ARowIndex: Integer; AMin: Single): IGridHeightResizer;
    function WithMinAndMaxRowHeight(ARowIndex: Integer; AMin, AMax: Single): IGridHeightResizer;
    function WithMaxGridHeight(AMax: Single): IGridHeightResizer;
    function WithMinGridHeight(AMin: Single): IGridHeightResizer;
    function WithMinAndMaxGridHeight(Amin, AMax: Single): IGridHeightResizer;
    function WithHeightRange(ARowIndex: Integer; const AValues: array of Single;
      ASelectRangeMode: TValueRangeMode): IGridHeightResizer;
    property GridHeight: Single read GetGridHeight;
  end;

  IGridFullResizer = interface(IGridResizer)
    function WithGridWidth(ANewWidth: Single): IGridFullResizer;
    function WithFixedColumns(const AFixedColumns: array of Integer): IGridFullResizer;
    function DisableFixedColumn(AColIndex: Integer): IGridFullResizer; overload;
    function EnableFixedColumn(AColIndex: Integer): IGridFullResizer; overload;
    function DisableFixedColumn(const AFixedColumns: array of Integer): IGridFullResizer; overload;
    function EnableFixedColumn(const AFixedColumns: array of Integer): IGridFullResizer; overload;
    function WithMaxColumnWidth(AColIndex: Integer; AMax: Single): IGridFullResizer;
    function WithMinColumnWidth(AColIndex: Integer; AMin: Single): IGridFullResizer;
    function WithMinAndMaxColumnWidth(AColIndex: Integer; AMin, AMax: Single): IGridFullResizer;
    function WithMaxGridWidth(AMax: Single): IGridFullResizer;
    function WithMinGridWidth(AMin: Single): IGridFullResizer;
    function WithMinAndMaxGridWidth(Amin, AMax: Single): IGridFullResizer;
    function WithGridHeight(ANewHeight: Single): IGridFullResizer;
    function WithFixedRows(const AFixedRows: array of Integer): IGridFullResizer;
    function DisableFixedRow(ARowIndex: Integer): IGridFullResizer; overload;
    function EnableFixedRow(ARowIndex: Integer): IGridFullResizer; overload;
    function DisableFixedRow(const AFixedRows: array of Integer): IGridFullResizer; overload;
    function EnableFixedRow(const AFixedRows: array of Integer): IGridFullResizer; overload;
    function WithMaxRowHeight(ARowIndex: Integer; AMax: Single): IGridFullResizer;
    function WithMinRowHeight(ARowIndex: Integer; AMin: Single): IGridFullResizer;
    function WithMinAndMaxRowHeight(ARowIndex: Integer; AMin, AMax: Single): IGridFullResizer;
    function WithMaxGridHeight(AMax: Single): IGridFullResizer;
    function WithMinGridHeight(AMin: Single): IGridFullResizer;
    function WithMinAndMaxGridHeight(Amin, AMax: Single): IGridFullResizer;
  end;

  { TGridResizerBase }

  TGridResizerBase = class abstract(TInterfacedObject, IGridResizer)
  protected
    FMinTotalSize: Single;
    FMaxTotalSize: Single;
    FTotalSize: Single;
    FFixedIndexes: TIntList;
    FMinSizes: TIntSingleDictionary;
    FMaxSizes: TIntSingleDictionary;
    FSizesRanges: TSingleTValueRangeDictionary;
    procedure ApplyLimitsAndRedistribute(AGrid: TGridLayout; AFlexibleIndexes: TIntList);
    function GetItemCount(AGrid: TGridLayout): Integer; virtual; abstract;
    function GetVisibleItem(AGrid: TGridLayout; AIndex: Integer): Boolean; virtual; abstract;
    function GetItemSize(AGrid: TGridLayout; AIndex: Integer): Single; virtual; abstract;
    procedure SetItemSize(AGrid: TGridLayout; AIndex: Integer; ASize: Single); virtual; abstract;
    function GetSpacing(AGrid: TGridLayout; AIndex: Integer): Single; virtual; abstract;
    function GetTotalMargin(AGrid: TGridLayout): Single; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Resize(AGrid: TGridLayout);
  end;

  { TGridWidthResizer }

  TGridWidthResizer = class(TGridResizerBase, IGridWidthResizer)
  protected
    function GetItemCount(AGrid: TGridLayout): Integer; override;
    function GetVisibleItem(AGrid: TGridLayout; AIndex: Integer): Boolean; override;
    function GetItemSize(AGrid: TGridLayout; AIndex: Integer): Single; override;
    procedure SetItemSize(AGrid: TGridLayout; AIndex: Integer; ASize: Single); override;
    function GetSpacing(AGrid: TGridLayout; AIndex: Integer): Single; override;
    function GetTotalMargin(AGrid: TGridLayout): Single; override;
  public
    function GetGridWidth: Single;
    function WithGridWidth(ANewWidth: Single): IGridWidthResizer;
    function WithFixedColumns(const AFixedColumns: array of Integer): IGridWidthResizer;
    function DisableFixedColumn(AColIndex: Integer): IGridWidthResizer; overload;
    function EnableFixedColumn(AColIndex: Integer): IGridWidthResizer; overload;
    function DisableFixedColumn(const AFixedColumns: array of Integer): IGridWidthResizer; overload;
    function EnableFixedColumn(const AFixedColumns: array of Integer): IGridWidthResizer; overload;
    function WithMaxColumnWidth(AColIndex: Integer; AMax: Single): IGridWidthResizer;
    function WithMinColumnWidth(AColIndex: Integer; AMin: Single): IGridWidthResizer;
    function WithMinAndMaxColumnWidth(AColIndex: Integer; AMin, AMax: Single): IGridWidthResizer;
    function WithMaxGridWidth(AMax: Single): IGridWidthResizer;
    function WithMinGridWidth(AMin: Single): IGridWidthResizer;
    function WithMinAndMaxGridWidth(Amin, AMax: Single): IGridWidthResizer;
    function WithWidthRange(AColIndex: Integer; const AValues: array of Single;
      ASelectRangeMode: TValueRangeMode): IGridWidthResizer;
    property GridWidth: Single read GetGridWidth write FTotalSize;
  end;

  { TGridHeightResizer }

  TGridHeightResizer = class(TGridResizerBase, IGridHeightResizer)
  protected
    function GetItemCount(AGrid: TGridLayout): Integer; override;
    function GetVisibleItem(AGrid: TGridLayout; AIndex: Integer): Boolean; override;
    function GetItemSize(AGrid: TGridLayout; AIndex: Integer): Single; override;
    procedure SetItemSize(AGrid: TGridLayout; AIndex: Integer; ASize: Single); override;
    function GetSpacing(AGrid: TGridLayout; AIndex: Integer): Single; override;
    function GetTotalMargin(AGrid: TGridLayout): Single; override;
  public
    function GetGridHeight: Single;
    function WithGridHeight(ANewHeight: Single): IGridHeightResizer;
    function WithFixedRows(const AFixedRows: array of Integer): IGridHeightResizer;
    function DisableFixedRow(ARowIndex: Integer): IGridHeightResizer; overload;
    function EnableFixedRow(ARowIndex: Integer): IGridHeightResizer; overload;
    function DisableFixedRow(const AFixedRows: array of Integer): IGridHeightResizer; overload;
    function EnableFixedRow(const AFixedRows: array of Integer): IGridHeightResizer; overload;
    function WithMaxRowHeight(ARowIndex: Integer; AMax: Single): IGridHeightResizer;
    function WithMinRowHeight(ARowIndex: Integer; AMin: Single): IGridHeightResizer;
    function WithMinAndMaxRowHeight(ARowIndex: Integer; AMin, AMax: Single): IGridHeightResizer;
    function WithMaxGridHeight(AMax: Single): IGridHeightResizer;
    function WithMinGridHeight(AMin: Single): IGridHeightResizer;
    function WithMinAndMaxGridHeight(Amin, AMax: Single): IGridHeightResizer;
    function WithHeightRange(ARowIndex: Integer; const AValues: array of Single;
      ASelectRangeMode: TValueRangeMode): IGridHeightResizer;
    property GridHeight: Single read GetGridHeight write FTotalSize;
  end;

  { TGridFullResizer }

  TGridFullResizer = class(TInterfacedObject, IGridFullResizer)
  private
    FGridHeight: Single;
    FGridWidth: Single;
    FWidthResizer: IGridWidthResizer;
    FHeightResizer: IGridHeightResizer;
  public
    constructor Create;
    procedure Resize(AGrid: TGridLayout);
    function GetGridHeight: Single;
    function WithGridHeight(ANewHeight: Single): IGridFullResizer; reintroduce;
    function WithFixedRows(const AFixedRows: array of Integer): IGridFullResizer; reintroduce;
    function DisableFixedRow(ARowIndex: Integer): IGridFullResizer; reintroduce; overload;
    function EnableFixedRow(ARowIndex: Integer): IGridFullResizer; reintroduce; overload;
    function DisableFixedRow(const AFixedRows: array of Integer): IGridFullResizer; reintroduce; overload;
    function EnableFixedRow(const AFixedRows: array of Integer): IGridFullResizer; reintroduce; overload;
    function WithMaxRowHeight(ARowIndex: Integer; AMax: Single): IGridFullResizer; reintroduce;
    function WithMinRowHeight(ARowIndex: Integer; AMin: Single): IGridFullResizer; reintroduce;
    function WithMinAndMaxRowHeight(ARowIndex: Integer; AMin, AMax: Single): IGridFullResizer; reintroduce;
    function WithMaxGridHeight(AMax: Single): IGridFullResizer; reintroduce;
    function WithMinGridHeight(AMin: Single): IGridFullResizer; reintroduce;
    function WithMinAndMaxGridHeight(Amin, AMax: Single): IGridFullResizer; reintroduce;
    property GridHeight: Single read GetGridHeight write FGridHeight;
    function GetGridWidth: Single;
    function WithGridWidth(ANewWidth: Single): IGridFullResizer; reintroduce;
    function WithFixedColumns(const AFixedColumns: array of Integer): IGridFullResizer; reintroduce;
    function DisableFixedColumn(AColIndex: Integer): IGridFullResizer; reintroduce; overload;
    function EnableFixedColumn(AColIndex: Integer): IGridFullResizer; reintroduce; overload;
    function DisableFixedColumn(const AFixedColumns: array of Integer): IGridFullResizer; reintroduce; overload;
    function EnableFixedColumn(const AFixedColumns: array of Integer): IGridFullResizer; reintroduce; overload;
    function WithMaxColumnWidth(AColIndex: Integer; AMax: Single): IGridFullResizer; reintroduce;
    function WithMinColumnWidth(AColIndex: Integer; AMin: Single): IGridFullResizer; reintroduce;
    function WithMinAndMaxColumnWidth(AColIndex: Integer; AMin, AMax: Single): IGridFullResizer; reintroduce;
    function WithMaxGridWidth(AMax: Single): IGridFullResizer; reintroduce;
    function WithMinGridWidth(AMin: Single): IGridFullResizer; reintroduce;
    function WithMinAndMaxGridWidth(Amin, AMax: Single): IGridFullResizer; reintroduce;
    property GridWidth: Single read GetGridWidth write FGridWidth;
  end;

implementation

uses
  Math;

{ TValueRange }

constructor TValueRange.Create(AValues: TSingleList; AMode: TValueRangeMode;
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

function TValueRange.GetFloor(Value: Single): Single;
var
  I: Integer;
begin
  for I := 0 to FValues.Count-1 do
    if FValues[I] >= Value then
      Exit(FValues[I]);

  Result := FValues.Last;
end;

function TValueRange.GeCeil(Value: Single): Single;
var
  I: Integer;
begin
  for I := FValues.Count - 1 downto 0 do
    if FValues[I] <= Value then
      Exit(FValues[I]);

  Result := FValues.First;
end;

function TValueRange.GetNearest(Value: Single): Single;
var
  Lower, Upper: Single;
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

function TValueRange.Select(AValue: Single): Single;
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

{ TGridResizerBase }

procedure TGridResizerBase.ApplyLimitsAndRedistribute(AGrid: TGridLayout;
  AFlexibleIndexes: TIntList);
var
  Index: Integer;
  MinS, MaxS, Range: Single;
  AdjustedIndexes: TIntList;
  Excess: Single;
  Dist: Single;

  function ApplyRange(Index: Integer): Single;
  begin
    if FSizesRanges.ContainsKey(Index) then
      Result := FSizesRanges[Index].Select(GetItemSize(AGrid, Index))  // AGrid.ColumnWidth[Col]
    else
      Result := GetItemSize(AGrid, Index); // AGrid.ColumnWidth[Col];
  end;

  function ApplyMaxValue(Index: Integer): Single;
  begin
    if not FMaxSizes.TryGetValue(Index, Result) then  // if not FMaxWidths.TryGetValue(Col, Result) then
      Result := MaxSingle;
  end;

  function ApplyMinValue(Index: Integer): Single;
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
      Dist := Excess / AdjustedIndexes.Count;
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

constructor TGridResizerBase.Create;
begin
  FMinTotalSize := 0;
  FMaxTotalSize := MaxInt;
  FFixedIndexes := TIntList.Create;
  FMinSizes := TIntSingleDictionary.Create;
  FMaxSizes := TIntSingleDictionary.Create;
  FSizesRanges := TSingleTValueRangeDictionary.Create;
end;

destructor TGridResizerBase.Destroy;
var
  Pair: {$IFDEF FPC}specialize{$ENDIF} TPair<Single, TValueRange>;
begin
  FMaxSizes.Free;
  FMinSizes.Free;
  FFixedIndexes.Free;
  for Pair in FSizesRanges do
    Pair.Value.Free;
  FSizesRanges.Free;
  inherited Destroy;
end;

procedure TGridResizerBase.Resize(AGrid: TGridLayout);
var
  FixedSize: Single;
  FlexibleIndexes: TIntList;
  AvailableSize, NewSize: Single;
  I: Integer;

  function CalculateTotalSpacingWithMargins: Single;
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
        Result := Result + GetSpacing(AGrid, I); // Inc(Result, GetSpacing(AGrid, I));  //Inc(Result, AGrid.HorizontalSpacing[I]);
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

      if (FFixedIndexes.IndexOf(I) >= 0) then
        FixedSize := FixedSize + GetItemSize(AGrid, I) // Inc(FixedSize, GetItemSize(AGrid, I))  // AGrid.ColumnWidth[I]
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

    NewSize := AvailableSize / FlexibleIndexes.Count;

    for I in FlexibleIndexes do
      SetItemSize(AGrid, I, Max(NewSize, 1));  // AGrid.ColumnWidth[I] := Max(NewColWidth, 1);

    ApplyLimitsAndRedistribute(AGrid, FlexibleIndexes);
  finally
    FlexibleIndexes.Free;
  end;
end;

{ TGridWidthResizer }

function TGridWidthResizer.GetItemCount(AGrid: TGridLayout): Integer;
begin
  Result := AGrid.Columns;
end;

function TGridWidthResizer.GetVisibleItem(AGrid: TGridLayout;
  AIndex: Integer): Boolean;
begin
  Result := AGrid.VisibleColumn[AIndex];
end;

function TGridWidthResizer.GetItemSize(AGrid: TGridLayout;
  AIndex: Integer): Single;
begin
  Result := AGrid.ColumnWidth[AIndex];
end;

procedure TGridWidthResizer.SetItemSize(AGrid: TGridLayout; AIndex: Integer;
  ASize: Single);
begin
  AGrid.ColumnWidth[AIndex] := ASize;
end;

function TGridWidthResizer.GetSpacing(AGrid: TGridLayout;
  AIndex: Integer): Single;
begin
  Result := AGrid.HorizontalSpacing[AIndex];
end;

function TGridWidthResizer.GetTotalMargin(AGrid: TGridLayout
  ): Single;
begin
  Result := AGrid.Margins.Left + AGrid.Margins.Right;
end;

function TGridWidthResizer.GetGridWidth: Single;
begin
  Result := FTotalSize;
end;

function TGridWidthResizer.WithGridWidth(ANewWidth: Single): IGridWidthResizer;
begin
  Result := Self;
  if ANewWidth < FMinTotalSize then
    FTotalSize := FMinTotalSize
  else if ANewWidth > FMaxTotalSize then
    FTotalSize := FMaxTotalSize
  else
    FTotalSize := ANewWidth;
end;

function TGridWidthResizer.WithFixedColumns
  (const AFixedColumns: array of Integer): IGridWidthResizer;
var
  I: Integer;
begin
  Result := Self;
  FFixedIndexes.Clear;
  for I := 0 to High(AFixedColumns) do
    if FFixedIndexes.IndexOf(I) = -1 then
      FFixedIndexes.Add(AFixedColumns[I]);
end;

function TGridWidthResizer.DisableFixedColumn(AColIndex: Integer
  ): IGridWidthResizer;
begin
  Result := Self;
  FFixedIndexes.Remove(AColIndex);
end;

function TGridWidthResizer.EnableFixedColumn(AColIndex: Integer
  ): IGridWidthResizer;
begin
  Result := Self;
  if FFixedIndexes.IndexOf(AColIndex) = -1 then
    FFixedIndexes.Add(AColIndex);
end;

function TGridWidthResizer.DisableFixedColumn
  (const AFixedColumns: array of Integer): IGridWidthResizer;
var
  I: Integer;
begin
  Result := Self;
  for I := 0 to High(AFixedColumns) do
    FFixedIndexes.Remove(AFixedColumns[I]);
end;

function TGridWidthResizer.EnableFixedColumn
  (const AFixedColumns: array of Integer): IGridWidthResizer;
var
  I: Integer;
begin
  Result := Self;
  for I := 0 to High(AFixedColumns) do
    if FFixedIndexes.IndexOf(I) = -1 then
      FFixedIndexes.Add(AFixedColumns[I]);
end;

function TGridWidthResizer.WithMaxColumnWidth(AColIndex: Integer;
  AMax: Single): IGridWidthResizer;
begin
  Result := Self;
  if not FMaxSizes.ContainsKey(AColIndex) then
    FMaxSizes.Add(AColIndex, AMax);
end;

function TGridWidthResizer.WithMinColumnWidth(AColIndex: Integer;
  AMin: Single): IGridWidthResizer;
begin
  Result := Self;
  if not FMinSizes.ContainsKey(AColIndex) then
    FMinSizes.Add(AColIndex, AMin);
end;

function TGridWidthResizer.WithMinAndMaxColumnWidth
  (AColIndex: Integer; AMin, AMax: Single): IGridWidthResizer;
begin
  Result := Self
    .WithMinColumnWidth(AColIndex, AMin)
    .WithMaxColumnWidth(AColIndex, AMax);
end;

function TGridWidthResizer.WithMaxGridWidth(AMax: Single): IGridWidthResizer;
begin
  Result := Self;
  FMaxTotalSize := AMax;
end;

function TGridWidthResizer.WithMinGridWidth(AMin: Single): IGridWidthResizer;
begin
  Result := Self;
  FMinTotalSize := AMin;
end;

function TGridWidthResizer.WithMinAndMaxGridWidth(Amin, AMax: Single): IGridWidthResizer;
begin
  Result := Self
    .WithMinGridWidth(Amin)
    .WithMaxGridWidth(AMax);
end;

function TGridWidthResizer.WithWidthRange(AColIndex: Integer;
  const AValues: array of Single; ASelectRangeMode: TValueRangeMode
  ): IGridWidthResizer;
var
  I: Integer;
  List: TSingleList;
begin
  Result := Self;

  List := TSingleList.Create;
  for I:=Low(AValues) to High(AValues) do
    if not List.Contains(AValues[I]) then
      List.Add(AValues[I]);
  List.Sort;

  FSizesRanges.AddOrSetValue(
    AColIndex,
    TValueRange.Create(List, ASelectRangeMode, False)
  );
end;

{ TGridHeightResizer }

function TGridHeightResizer.GetItemCount(AGrid: TGridLayout): Integer;
begin
  Result := AGrid.Rows;
end;

function TGridHeightResizer.GetVisibleItem(AGrid: TGridLayout;
  AIndex: Integer): Boolean;
begin
  Result := AGrid.VisibleRow[AIndex];
end;

function TGridHeightResizer.GetItemSize(AGrid: TGridLayout;
  AIndex: Integer): Single;
begin
  Result := AGrid.RowHeight[AIndex];
end;

procedure TGridHeightResizer.SetItemSize(AGrid: TGridLayout;
  AIndex: Integer; ASize: Single);
begin
  AGrid.RowHeight[AIndex] := ASize;
end;

function TGridHeightResizer.GetSpacing(AGrid: TGridLayout;
  AIndex: Integer): Single;
begin
  Result := AGrid.VerticalSpacing[AIndex];
end;

function TGridHeightResizer.GetTotalMargin(AGrid: TGridLayout
  ): Single;
begin
  Result := AGrid.Margins.Top + AGrid.Margins.Bottom;
end;

function TGridHeightResizer.GetGridHeight: Single;
begin
  Result := FTotalSize;
end;

function TGridHeightResizer.WithGridHeight(ANewHeight: Single): IGridHeightResizer;
begin
  Result := Self;
  if ANewHeight < FMinTotalSize then
    FTotalSize := FMinTotalSize
  else if ANewHeight > FMaxTotalSize then
    FTotalSize := FMaxTotalSize
  else
    FTotalSize := ANewHeight;
end;

function TGridHeightResizer.WithFixedRows
  (const AFixedRows: array of Integer): IGridHeightResizer;
var
  I: Integer;
begin
  Result := Self;
  FFixedIndexes.Clear;
  for I := 0 to High(AFixedRows) do
    if FFixedIndexes.IndexOf(I) = -1 then
      FFixedIndexes.Add(AFixedRows[I]);
end;

function TGridHeightResizer.DisableFixedRow(ARowIndex: Integer
  ): IGridHeightResizer;
begin
  Result := Self;
  FFixedIndexes.Remove(ARowIndex);
end;

function TGridHeightResizer.EnableFixedRow(ARowIndex: Integer
  ): IGridHeightResizer;
begin
  Result := Self;
  if FFixedIndexes.IndexOf(ARowIndex) = -1 then
    FFixedIndexes.Add(ARowIndex);
end;

function TGridHeightResizer.DisableFixedRow
  (const AFixedRows: array of Integer): IGridHeightResizer;
var
  I: Integer;
begin
  Result := Self;
  for I := 0 to High(AFixedRows) do
    FFixedIndexes.Remove(AFixedRows[I]);
end;

function TGridHeightResizer.EnableFixedRow
  (const AFixedRows: array of Integer): IGridHeightResizer;
var
  I: Integer;
begin
  Result := Self;
  for I := 0 to High(AFixedRows) do
    if FFixedIndexes.IndexOf(I) = -1 then
      FFixedIndexes.Add(AFixedRows[I]);
end;

function TGridHeightResizer.WithMaxRowHeight(ARowIndex: Integer;
  AMax: Single): IGridHeightResizer;
begin
  Result := Self;
  if not FMaxSizes.ContainsKey(ARowIndex) then
    FMaxSizes.Add(ARowIndex, AMax);
end;

function TGridHeightResizer.WithMinRowHeight(ARowIndex: Integer;
  AMin: Single): IGridHeightResizer;
begin
  Result := Self;
  if not FMinSizes.ContainsKey(ARowIndex) then
    FMinSizes.Add(ARowIndex, AMin);
end;

function TGridHeightResizer.WithMinAndMaxRowHeight(ARowIndex: Integer;
  AMin, AMax: Single): IGridHeightResizer;
begin
  Result := Self
    .WithMinRowHeight(ARowIndex, AMin)
    .WithMaxRowHeight(ARowIndex, AMax);
end;

function TGridHeightResizer.WithMaxGridHeight(AMax: Single): IGridHeightResizer;
begin
  Result := Self;
  FMaxTotalSize := AMax;
end;

function TGridHeightResizer.WithMinGridHeight(AMin: Single): IGridHeightResizer;
begin
  Result := Self;
  FMinTotalSize := AMin;
end;

function TGridHeightResizer.WithMinAndMaxGridHeight(
  Amin, AMax: Single): IGridHeightResizer;
begin
  Result := Self
    .WithMinGridHeight(AMin)
    .WithMaxGridHeight(AMax);
end;

function TGridHeightResizer.WithHeightRange(ARowIndex: Integer;
  const AValues: array of Single; ASelectRangeMode: TValueRangeMode
  ): IGridHeightResizer;
var
  I: Integer;
  List: TSingleList;
begin
  Result := Self;

  List := TSingleList.Create;
  for I:=Low(AValues) to High(AValues) do
    if not List.Contains(AValues[I]) then
      List.Add(AValues[I]);
  List.Sort;

  FSizesRanges.AddOrSetValue(
    ARowIndex,
    TValueRange.Create(List, ASelectRangeMode, False)
  );
end;

{ TGridFullResizer }

constructor TGridFullResizer.Create;
begin
  FHeightResizer := TGridHeightResizer.Create;
  FWidthResizer := TGridWidthResizer.Create;
end;

procedure TGridFullResizer.Resize(AGrid: TGridLayout);
begin
  FWidthResizer.Resize(AGrid);
  FHeightResizer.Resize(AGrid);
end;

function TGridFullResizer.GetGridHeight: Single;
begin
  Result := FGridHeight;
end;

function TGridFullResizer.WithGridHeight(ANewHeight: Single): IGridFullResizer;
begin
  Result := Self;
  FHeightResizer.WithGridHeight(ANewHeight)
end;

function TGridFullResizer.WithFixedRows(const AFixedRows: array of Integer
  ): IGridFullResizer;
begin
  Result := Self;
  FHeightResizer.WithFixedRows(AFixedRows);
end;

function TGridFullResizer.DisableFixedRow(ARowIndex: Integer
  ): IGridFullResizer;
begin
  Result := Self;
  FHeightResizer.DisableFixedRow(ARowIndex);
end;

function TGridFullResizer.EnableFixedRow(ARowIndex: Integer
  ): IGridFullResizer;
begin
  Result := Self;
  FHeightResizer.EnableFixedRow(ARowIndex);
end;

function TGridFullResizer.DisableFixedRow
  (const AFixedRows: array of Integer): IGridFullResizer;
begin
  Result := Self;
  FHeightResizer.DisableFixedRow(AFixedRows);
end;

function TGridFullResizer.EnableFixedRow
  (const AFixedRows: array of Integer): IGridFullResizer;
begin
  Result := Self;
  FHeightResizer.EnableFixedRow(AFixedRows);
end;

function TGridFullResizer.WithMaxRowHeight(ARowIndex: Integer;
  AMax: Single): IGridFullResizer;
begin
  Result := Self;
  FHeightResizer.WithMaxRowHeight(ARowIndex, AMax);
end;

function TGridFullResizer.WithMinRowHeight(ARowIndex: Integer;
  AMin: Single): IGridFullResizer;
begin
  Result := Self;
  FHeightResizer.WithMinRowHeight(ARowIndex, AMin);
end;

function TGridFullResizer.WithMinAndMaxRowHeight
  (ARowIndex: Integer; AMin, AMax: Single): IGridFullResizer;
begin
  Result := Self;
  FHeightResizer.WithMinAndMaxRowHeight(ARowIndex, AMin, AMax);
end;

function TGridFullResizer.WithMaxGridHeight(AMax: Single): IGridFullResizer;
begin
  Result := Self;
  FHeightResizer.WithMaxGridHeight(AMax);
end;

function TGridFullResizer.WithMinGridHeight(AMin: Single): IGridFullResizer;
begin
  Result := Self;
  FHeightResizer.WithMinGridHeight(AMin);
end;

function TGridFullResizer.WithMinAndMaxGridHeight(Amin, AMax: Single): IGridFullResizer;
begin
  Result := Self;
  FHeightResizer.WithMinAndMaxGridHeight(Amin, AMax);
end;

function TGridFullResizer.GetGridWidth: Single;
begin
  Result := FGridWidth;
end;

function TGridFullResizer.WithGridWidth(ANewWidth: Single): IGridFullResizer;
begin
  Result := Self;
  FWidthResizer.WithGridWidth(ANewWidth);
end;

function TGridFullResizer.WithFixedColumns
  (const AFixedColumns: array of Integer): IGridFullResizer;
begin
  Result := Self;
  FWidthResizer.WithFixedColumns(AFixedColumns);
end;

function TGridFullResizer.DisableFixedColumn(AColIndex: Integer
  ): IGridFullResizer;
begin
  Result := Self;
  FWidthResizer.DisableFixedColumn(AColIndex);
end;

function TGridFullResizer.EnableFixedColumn(AColIndex: Integer
  ): IGridFullResizer;
begin
  Result := Self;
  FWidthResizer.EnableFixedColumn(AColIndex);
end;

function TGridFullResizer.DisableFixedColumn
  (const AFixedColumns: array of Integer): IGridFullResizer;
begin
  Result := Self;
  FWidthResizer.DisableFixedColumn(AFixedColumns);
end;

function TGridFullResizer.EnableFixedColumn
  (const AFixedColumns: array of Integer): IGridFullResizer;
begin
  Result := Self;
  FWidthResizer.EnableFixedColumn(AFixedColumns);
end;

function TGridFullResizer.WithMaxColumnWidth(AColIndex: Integer;
  AMax: Single): IGridFullResizer;
begin
  Result := Self;
  FWidthResizer.WithMaxColumnWidth(AColIndex, AMax);
end;

function TGridFullResizer.WithMinColumnWidth(AColIndex: Integer;
  AMin: Single): IGridFullResizer;
begin
  Result := Self;
  FWidthResizer.WithMinColumnWidth(AColIndex, AMin);
end;

function TGridFullResizer.WithMinAndMaxColumnWidth
  (AColIndex: Integer; AMin, AMax: Single): IGridFullResizer;
begin
  Result := Self;
  FWidthResizer.WithMinAndMaxColumnWidth(AColIndex, AMin, AMax);
end;

function TGridFullResizer.WithMaxGridWidth(AMax: Single): IGridFullResizer;
begin
  Result := Self;
  FWidthResizer.WithMaxGridWidth(AMax);
end;

function TGridFullResizer.WithMinGridWidth(AMin: Single): IGridFullResizer;
begin
  Result := Self;
  FWidthResizer.WithMinGridWidth(AMin);
end;

function TGridFullResizer.WithMinAndMaxGridWidth(Amin, AMax: Single): IGridFullResizer;
begin
  Result := Self;
  FWidthResizer.WithMinAndMaxGridWidth(Amin, AMax);
end;

end.

