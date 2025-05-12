unit ULayout;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, Generics.Collections;

type
  ILayoutItem = interface
    ['{C28F4715-03C4-428B-AEBA-E2CDE48378B7}']
    function GetControl: TControl;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
  end;

  TLayoutAlignment = (laStretch, laCenter, laStart, laEnd);

  { TMargins }

  TMargins = class
  private
    FBottom: Integer;
    FLeft: Integer;
    FOnChange: TNotifyEvent;
    FRight: Integer;
    FTop: Integer;
    procedure DoChange;
    procedure SetAll(AValue: Integer);
    procedure SetBottom(AValue: Integer);
    procedure SetLeft(AValue: Integer);
    procedure SetOnChange(AValue: TNotifyEvent);
    procedure SetRight(AValue: Integer);
    procedure SetTop(AValue: Integer);
  public
    constructor Create(ATop, ARight, ABottom, ALeft: Integer); overload;
    constructor Create; overload;
    constructor Create(const AMargin: Integer); overload;
    constructor Create(ATopBottom, ALeftRight: Integer); overload;
    property Top: Integer read FTop write SetTop;
    property Right: Integer read FRight write SetRight;
    property Bottom: Integer read FBottom write SetBottom;
    property Left: Integer read FLeft write SetLeft;
    property All: Integer write SetAll;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;

  IGridPosition = interface
    ['{B4B7F671-9464-4F79-94A5-657B84C30B59}']
    function GetRow: Integer;
    function GetColumn: Integer;
    procedure SetRow(Value: Integer);
    procedure SetColumn(Value: Integer);
    property Row: Integer read GetRow write SetRow;
    property Column: Integer read GetColumn write SetColumn;
  end;

  { TGridPosition }

  TGridPosition = class(TInterfacedObject, IGridPosition)
  private
    FRow: Integer;
    FColumn: Integer;
  public
    constructor Create(ARow, AColumn: Integer);
    function GetRow: Integer;
    function GetColumn: Integer;
    procedure SetRow(Value: Integer);
    procedure SetColumn(Value: Integer);
    property Row: Integer read GetRow write SetRow;
    property Column: Integer read GetColumn write SetColumn;
  end;

  { TGridCellSettings }

  TGridCellSettings = class
  private
    FColSpan: Integer;
    FColumn: Integer;
    FExtraHeight: Integer;
    FExtraWidth: Integer;
    FHorizontalAlignment: TLayoutAlignment;
    FOffsetX: Integer;
    FOffsetY: Integer;
    FRow: Integer;
    FRowSpan: Integer;
    FVerticalAlignment: TLayoutAlignment;
  public
    constructor Create(ARow, AColumn: Integer); reintroduce;
    function WithRow(ARow: Integer): TGridCellSettings;
    function WithColumn(AColum: Integer): TGridCellSettings;
    function WithRowSpan(ARowSpan: Integer): TGridCellSettings;
    function WithColumnSpan(AColSpan: Integer): TGridCellSettings;
    function WithOffsetX(AOffsetX: Integer): TGridCellSettings;
    function WithOffsetY(AOffsetY: Integer): TGridCellSettings;
    function WithHorizontalAlignment(AHorizontalAlignment: TLayoutAlignment): TGridCellSettings;
    function WithVerticalAlignment(AVerticalAlignment: TLayoutAlignment): TGridCellSettings;
    function WithAlignment(AHorizontalAlignment: TLayoutAlignment;
      AVerticalAlignment: TLayoutAlignment): TGridCellSettings;
    function WithExtraWidth(AExtraWidth: Integer): TGridCellSettings;
    function WithExtraHeight(AExtraHeight: Integer): TGridCellSettings;
    property Row: Integer read FRow;
    property Column: Integer read FColumn;
    property ColSpan: Integer read FColSpan;
    property RowSpan: Integer read FRowSpan;
    property OffsetX: Integer read FOffsetX;
    property OffsetY: Integer read FOffsetY;
    property HorizontalAlignment: TLayoutAlignment read FHorizontalAlignment;
    property VerticalAlignment: TLayoutAlignment read FVerticalAlignment;
    property ExtraWidth: Integer read FExtraWidth;
    property ExtraHeight: Integer read FExtraHeight;
  end;

  { TGridCell }

  TGridCell = class
  private
    FExtraHeight: Integer;
    FExtraWidth: Integer;
    FItem: ILayoutItem;
    FRow: Integer;
    FColumn: Integer;
    FRowSpan: Integer;
    FColSpan: Integer;
    FOffsetX: Integer;
    FOffsetY: Integer;
    FHorizontalAlignment: TLayoutAlignment;
    FVerticalAlignment: TLayoutAlignment;
  public
    constructor Create(AControl: ILayoutItem; ASettings:
      TGridCellSettings); reintroduce; overload;
    property Item: ILayoutItem read FItem;
    property Row: Integer read FRow;
    property Column: Integer read FColumn;
    property RowSpan: Integer read FRowSpan;
    property ColSpan: Integer read FColSpan;
    property OffsetX: Integer read FOffsetX write FOffsetX;
    property OffsetY: Integer read FOffsetY write FOffsetY;
    property ExtraWidth: Integer read FExtraWidth write FExtraWidth;
    property ExtraHeight: Integer read FExtraHeight write FExtraWidth;
    property HorizontalAlignment: TLayoutAlignment read FHorizontalAlignment write FHorizontalAlignment;
    property VerticalAlignment: TLayoutAlignment read FVerticalAlignment write FVerticalAlignment;
  end;

  IGridFill = interface;
  TGridLayout = class;

  TGridFillBeforePlaceEvent = procedure(
    AGridFill: IGridFill;
    AGrid: TGridLayout;
    AItem: ILayoutItem;
    APos: IGridPosition;
    var ASettings: TGridCellSettings;
    var ACanPlace: Boolean
  ) of object;

  TGridFillAfterPlaceEvent = procedure(
    AGridFill: IGridFill;
    AGrid: TGridLayout;
    AItem: ILayoutItem;
    var APosition: IGridPosition
  ) of object;

  { IGridFill }

  IGridFill = interface
    ['{C36045EF-5E7F-453D-88A3-218A480781D5}']
    function GetOnAfterPlaceItem: TGridFillAfterPlaceEvent;
    function GetOnBeforePlaceItem: TGridFillBeforePlaceEvent;
    function GetPosition: IGridPosition;
    procedure SetOnAfterPlaceItem(AValue: TGridFillAfterPlaceEvent);
    procedure SetOnBeforePlaceItem(AValue: TGridFillBeforePlaceEvent);
    function GetGrid: TGridLayout;
    procedure PlaceItem(AItem: ILayoutItem); overload;
    procedure PlaceItem(AItem: TControl); overload;
    procedure Skip(ACount: Integer=1);
    procedure InitialPos(APos: IGridPosition);
    function NextPosition: IGridPosition;
    property Grid: TGridLayout read GetGrid;
    property OnBeforePlaceItem: TGridFillBeforePlaceEvent read GetOnBeforePlaceItem write SetOnBeforePlaceItem;
    property OnAfterPlaceItem: TGridFillAfterPlaceEvent read GetOnAfterPlaceItem write SetOnAfterPlaceItem;
    property Position: IGridPosition read GetPosition;
  end;

  TGridCellList = specialize TObjectList<TGridCell>;
  TGridCellSettingsList = specialize TObjectList<TGridCellSettings>;
  TIntIntDictionary = specialize TDictionary<Integer, Integer>;
  TIntList = specialize TList<Integer>;

  { TGridLayout }

  TGridLayout = class
  private
    FCells: TGridCellList;
    FCellSettingsList: TGridCellSettingsList;
    FLeft: Integer;
    FMargins: TMargins;
    FRows: Integer;
    FColumns: Integer;
    FTop: Integer;
    FVerticalSpacings: Integer;
    FHorizontalSpacings: Integer;
    FRowHeights: Integer;
    FColumnWidths: Integer;
    FRowHeightDic: TIntIntDictionary;
    FColumnWidthDic: TIntIntDictionary;
    FHorizontalSpacingDic: TIntIntDictionary;
    FVerticalSpacingDic: TIntIntDictionary;
    FRowShift: TIntIntDictionary;
    FColumnShift: TIntIntDictionary;
    FHiddenRows: TIntList;
    FHiddenColumns: TIntList;
    function CalculateCellWidth(Cell: TGridCell): Integer;
    function CalculateCellHeight(Cell: TGridCell): Integer;
    function CalculateCellLeft(Cell: TGridCell): Integer;
    function CalculateCellTop(Cell: TGridCell): Integer;
    function CreateDefaultSettings(ARow, AColumn: Integer): TGridCellSettings;
    function GetColumnShift(Index: Integer): Integer;
    function GetContentHeight: Integer;
    function GetContentWidth: Integer;
    function GetHorizontalSpacing(Index: Integer): Integer;
    function GetRowHeight(Index: Integer): Integer;
    function GetRowShift(Index: Integer): Integer;
    function GetVerticalSpacing(Index: Integer): Integer;
    function GetVisibleColumn(Index: Integer): Boolean;
    function GetVisibleRow(Index: Integer): Boolean;
    procedure SetColumnShift(Index: Integer; AValue: Integer);
    procedure SetHorizontalSpacing(Index: Integer; AValue: Integer);
    procedure SetLeft(AValue: Integer);
    procedure SetRowHeight(Index: Integer; Value: Integer);
    function GetColumnWidth(Index: Integer): Integer;
    procedure SetColumnWidth(Index: Integer; Value: Integer);
    procedure SetRowShift(Index: Integer; AValue: Integer);
    procedure SetTop(AValue: Integer);
    procedure SetVerticalSpacing(Index: Integer; AValue: Integer);
    procedure SetVisibleColumn(Index: Integer; AValue: Boolean);
    procedure SetVisibleRow(Index: Integer; AValue: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddItem(AItem: ILayoutItem; ASettings: TGridCellSettings); overload;
    procedure AddItem(AItem: TControl; ASettings: TGridCellSettings); overload;
    procedure AddItem(AItem: TGridLayout; ASettings: TGridCellSettings); overload;
    procedure ArrangeItems; overload;
    procedure ArrangeItems(ALeft, ATop: Integer); overload;
    procedure ApplyCellsVisibility;
    function IsVisibleCell(Cell: TGridCell): Boolean;
    procedure Clear;
    procedure ClearRowAndColumnShifts;
    procedure ResetColumnWidthsToDefault;
    function IsCellOccupied(ARow, ACol: Integer): Boolean;
    function IsColumnWidthCustomized(ACol: Integer): Boolean;
    function IsVerticalSpacingCustomized(AIndex: Integer): Boolean;
    function IsHorizontalSpacingCustomized(AIndex: Integer): Boolean;
    property Rows: Integer read FRows write FRows;
    property Columns: Integer read FColumns write FColumns;
    property VerticalSpacings: Integer read FVerticalSpacings write FVerticalSpacings;
    property HorizontalSpacings: Integer read FHorizontalSpacings write FHorizontalSpacings;
    property RowHeights: Integer read FRowHeights write FRowHeights;
    property ColumnWidths: Integer read FColumnWidths write FColumnWidths;
    property RowHeight[Index: Integer]: Integer read GetRowHeight write SetRowHeight;
    property ColumnWidth[Index: Integer]: Integer read GetColumnWidth write SetColumnWidth;
    property VerticalSpacing[Index: Integer]: Integer read GetVerticalSpacing write SetVerticalSpacing;
    property HorizontalSpacing[Index: Integer]: Integer read GetHorizontalSpacing write SetHorizontalSpacing;
    property RowShift[Index: Integer]: Integer read GetRowShift write SetRowShift;
    property ColumnShift[Index: Integer]: Integer read GetColumnShift write SetColumnShift;
    property VisibleRow[Index: Integer]: Boolean read GetVisibleRow write SetVisibleRow;
    property VisibleColumn[Index: Integer]: Boolean read GetVisibleColumn write SetVisibleColumn;
    property Margins: TMargins read FMargins;
    property ContentWidth: Integer read GetContentWidth;
    property ContentHeight: Integer read GetContentHeight;
    property Top: Integer read FTop write SetTop;
    property Left: Integer read FLeft write SetLeft;
  end;

  { TControlLayoutItem }

  TControlLayoutItem = class(TInterfacedObject, ILayoutItem)
  protected
    FControl: TControl;
    procedure AfterSetBounds; virtual;
  public
    constructor Create(AControl: TControl);
    function GetControl: TControl;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
  end;

  { TVirtualContainer }

  TVirtualContainer = class(TWinControl)
  end;

  { TSubGridLayoutItem }

  TSubGridLayoutItem = class(TControlLayoutItem)
  private
    FLayout: TGridLayout;
    procedure ContainerResize(Sender: TObject);
  protected
    FContainer: TWinControl;
    procedure AfterSetBounds; override;
  public
    constructor Create(ALayout: TGridLayout);
    constructor CreateWithContainerClass(ALayout: TGridLayout;
      AOwner: TWinControl; AContainerClass: TWinControlClass);
    destructor Destroy; override;
    property Layout: TGridLayout read FLayout;
    property Container: TWinControl read FContainer;
  end;

implementation

uses
  Math;

{ TGridCell }

constructor TGridCell.Create(AControl: ILayoutItem; ASettings: TGridCellSettings);
begin
  Assert(AControl <> nil, 'AControl cannot be nil');
  Assert(ASettings <> nil, 'ASettings cannot be nil');
  FItem := AControl;
  FRow := ASettings.Row;
  FColumn := ASettings.Column;
  FRowSpan := ASettings.RowSpan;
  FColSpan := ASettings.ColSpan;
  FOffsetX := ASettings.OffsetX;
  FOffsetY := ASettings.OffsetY;
  FExtraHeight := ASettings.ExtraHeight;
  FExtraWidth := ASettings.ExtraWidth;
  FHorizontalAlignment := ASettings.HorizontalAlignment;
  FVerticalAlignment := ASettings.VerticalAlignment;
end;

{ TMargins }

procedure TMargins.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TMargins.SetAll(AValue: Integer);
begin
  FTop := AValue;
  FRight := AValue;
  FBottom := AValue;
  FLeft := AValue;
end;

procedure TMargins.SetBottom(AValue: Integer);
begin
  if FBottom = AValue then
    Exit;
  FBottom := AValue;
end;

procedure TMargins.SetLeft(AValue: Integer);
begin
  if FLeft = AValue then
    Exit;
  FLeft := AValue;
end;

procedure TMargins.SetOnChange(AValue: TNotifyEvent);
begin
  if FOnChange = AValue then
    Exit;
  FOnChange := AValue;
end;

procedure TMargins.SetRight(AValue: Integer);
begin
  if FRight = AValue then
    Exit;
  FRight := AValue;
end;

procedure TMargins.SetTop(AValue: Integer);
begin
  if FTop = AValue then
    Exit;
  FTop := AValue;
end;

constructor TMargins.Create(ATop, ARight, ABottom, ALeft: Integer);
begin
  inherited Create;
  FTop := ATop;
  FRight := ARight;
  FBottom := ABottom;
  FLeft := ALeft;
end;

constructor TMargins.Create;
begin
  Create(0, 0, 0, 0);
end;

constructor TMargins.Create(const AMargin: Integer);
begin
  Create(AMargin, AMargin, AMargin, AMargin);
end;

constructor TMargins.Create(ATopBottom, ALeftRight: Integer);
begin
  Create(ATopBottom, ALeftRight, ATopBottom, ALeftRight);
end;

{ TGridPosition }

function TGridPosition.GetRow: Integer;
begin
  Result := FRow;
end;

function TGridPosition.GetColumn: Integer;
begin
  Result := FColumn;
end;

procedure TGridPosition.SetRow(Value: Integer);
begin
  FRow := Value;
end;

procedure TGridPosition.SetColumn(Value: Integer);
begin
  FColumn := Value;
end;

constructor TGridPosition.Create(ARow, AColumn: Integer);
begin
  FRow := ARow;
  FColumn := AColumn;
end;

{ TGridCellSettings }

constructor TGridCellSettings.Create(ARow, AColumn: Integer);
begin
  inherited Create;
  FRow := Max(0, ARow);
  FColumn := Max(0, AColumn);
  FColSpan := 1;
  FRowSpan := 1;
  FOffsetX := 0;
  FOffsetY := 0;
  FExtraHeight := 0;
  FExtraWidth := 0;
  FHorizontalAlignment := laStretch;
  FVerticalAlignment := laStretch;
end;

function TGridCellSettings.WithRow(ARow: Integer): TGridCellSettings;
begin
  FRow := Max(0, ARow);
  Result := Self;
end;

function TGridCellSettings.WithColumn(AColum: Integer): TGridCellSettings;
begin
  FColumn := Max(0, AColum);
  Result := Self;
end;

function TGridCellSettings.WithRowSpan(ARowSpan: Integer): TGridCellSettings;
begin
  FRowSpan := Max(1, ARowSpan);
  Result := Self;
end;

function TGridCellSettings.WithColumnSpan(AColSpan: Integer): TGridCellSettings;
begin
  FColSpan := Max(1, AColSpan);
  Result := Self;
end;

function TGridCellSettings.WithOffsetX(AOffsetX: Integer): TGridCellSettings;
begin
  FOffsetX := AOffsetX;
  Result := Self;
end;

function TGridCellSettings.WithOffsetY(AOffsetY: Integer): TGridCellSettings;
begin
  FOffsetY := AOffsetY;
  Result := Self;
end;

function TGridCellSettings.WithHorizontalAlignment(
  AHorizontalAlignment: TLayoutAlignment): TGridCellSettings;
begin
  FHorizontalAlignment := AHorizontalAlignment;
  Result := Self;
end;

function TGridCellSettings.WithVerticalAlignment(
  AVerticalAlignment: TLayoutAlignment): TGridCellSettings;
begin
  FVerticalAlignment := AVerticalAlignment;
  Result := Self;
end;

function TGridCellSettings.WithExtraWidth(AExtraWidth: Integer
  ): TGridCellSettings;
begin
  FExtraWidth := AExtraWidth;
  Result := Self;
end;

function TGridCellSettings.WithExtraHeight(AExtraHeight: Integer
  ): TGridCellSettings;
begin
  FExtraHeight := AExtraHeight;
  Result := Self;
end;

function TGridCellSettings.WithAlignment(AHorizontalAlignment: TLayoutAlignment;
  AVerticalAlignment: TLayoutAlignment): TGridCellSettings;
begin
  FHorizontalAlignment := AHorizontalAlignment;
  FVerticalAlignment := AVerticalAlignment;
  Result := Self;
end;

{ TGridLayout }

constructor TGridLayout.Create;
begin
  FCells := TGridCellList.Create(True);
  FCellSettingsList := TGridCellSettingsList.Create(True);
  FRowHeightDic := TIntIntDictionary.Create;
  FColumnWidthDic := TIntIntDictionary.Create;
  FVerticalSpacingDic := TIntIntDictionary.Create;
  FHorizontalSpacingDic := TIntIntDictionary.Create;
  FRowShift := TIntIntDictionary.Create;
  FColumnShift := TIntIntDictionary.Create;
  FHiddenRows := TIntList.Create;
  FHiddenColumns := TIntList.Create;
  FMargins := TMargins.Create;
  FVerticalSpacings := 0;
  FHorizontalSpacings := 0;
  FRowHeights := 10;
  FColumnWidths := 10;
  FTop := 0;
  FLeft := 0;
end;

destructor TGridLayout.Destroy;
begin
  FCells.Free;
  FCellSettingsList.Free;
  FRowHeightDic.Free;
  FColumnWidthDic.Free;
  FVerticalSpacingDic.Free;
  FHorizontalSpacingDic.Free;
  FRowShift.Free;
  FColumnShift.Free;
  FHiddenRows.Free;
  FHiddenColumns.Free;
  FMargins.Free;
  inherited;
end;

procedure TGridLayout.AddItem(AItem: ILayoutItem; ASettings: TGridCellSettings);
var
  GridCell: TGridCell;
begin
  Assert(ASettings <> nil, 'Settings cannot be nil');
  GridCell := TGridCell.Create(AItem, ASettings);
  FCells.Add(GridCell);
  FCellSettingsList.Add(ASettings);
end;

procedure TGridLayout.AddItem(AItem: TControl; ASettings: TGridCellSettings);
begin
  Self.AddItem(TControlLayoutItem.Create(AItem), ASettings);
end;

procedure TGridLayout.AddItem(AItem: TGridLayout; ASettings: TGridCellSettings);
begin
  Self.AddItem(TSubGridLayoutItem.Create(AItem), ASettings);
end;

function TGridLayout.CreateDefaultSettings(ARow, AColumn: Integer): TGridCellSettings;
begin
  Result := TGridCellSettings.Create(ARow, AColumn);
  Result.WithColumnSpan(1);
  Result.WithRowSpan(1);
  Result.WithOffsetX(0);
  Result.WithOffsetY(0);
  Result.WithHorizontalAlignment(laStretch);
  Result.WithVerticalAlignment(laStretch);
end;

function TGridLayout.GetColumnShift(Index: Integer): Integer;
begin
  if not FColumnShift.TryGetValue(Index, Result) then
    Result := 0;
end;

function TGridLayout.GetContentHeight: Integer;
var
  I: Integer;
begin
  Result := FMargins.Top + FMargins.Bottom;
  for I := 0 to Rows - 1 do
    Result := Result + GetRowHeight(I);
  for I := 0 to Rows - 2 do
    Result := Result + GetVerticalSpacing(I);
end;

function TGridLayout.GetContentWidth: Integer;
var
  I: Integer;
begin
  Result := FMargins.Left + FMargins.Right;
  for I := 0 to Columns - 1 do
    Result := Result + GetColumnWidth(I);
  for I := 0 to Columns - 2 do
    Result := Result + GetHorizontalSpacing(I);
end;

function TGridLayout.GetHorizontalSpacing(Index: Integer): Integer;
begin
  if not FHorizontalSpacingDic.TryGetValue(Index, Result) then
    Result := FHorizontalSpacings;
end;

procedure TGridLayout.Clear;
begin
  FCells.Clear;
  FCellSettingsList.Clear;
end;

procedure TGridLayout.ClearRowAndColumnShifts;
begin
  FRowShift.Clear;
  FColumnShift.Clear;
end;

procedure TGridLayout.ResetColumnWidthsToDefault;
begin
  FColumnWidthDic.Clear;
end;

function TGridLayout.IsCellOccupied(ARow, ACol: Integer): Boolean;
var
  Cell: TGridCell;
  R, C: Integer;
begin
  for Cell in FCells do
    for R := Cell.Row to Cell.Row + Cell.RowSpan - 1 do
      for C := Cell.Column to Cell.Column + Cell.ColSpan - 1 do
        if (R = ARow) and (C = ACol) then
          Exit(True);

  Result := False;
end;

function TGridLayout.IsColumnWidthCustomized(ACol: Integer): Boolean;
begin
  Result := FColumnWidthDic.ContainsKey(ACol);
end;

function TGridLayout.IsVerticalSpacingCustomized(AIndex: Integer): Boolean;
begin
  Result := FVerticalSpacingDic.ContainsKey(AIndex);
end;

function TGridLayout.IsHorizontalSpacingCustomized(AIndex: Integer): Boolean;
begin
  Result := FHorizontalSpacingDic.ContainsKey(AIndex);
end;

function TGridLayout.GetRowHeight(Index: Integer): Integer;
begin
  if not FRowHeightDic.TryGetValue(Index, Result) then
    Result := FRowHeights;
end;

function TGridLayout.GetRowShift(Index: Integer): Integer;
begin
  if not FRowShift.TryGetValue(Index, Result) then
    Result := 0;
end;

function TGridLayout.GetVerticalSpacing(Index: Integer): Integer;
begin
  if not FVerticalSpacingDic.TryGetValue(Index, Result) then
    Result := FVerticalSpacings;
end;

function TGridLayout.GetVisibleColumn(Index: Integer): Boolean;
begin
  Result := not FHiddenColumns.Contains(Index);
end;

function TGridLayout.GetVisibleRow(Index: Integer): Boolean;
begin
  Result := not FHiddenRows.Contains(Index);
end;

procedure TGridLayout.SetColumnShift(Index: Integer; AValue: Integer);
begin
  FColumnShift.AddOrSetValue(Index, AValue);
end;

procedure TGridLayout.SetHorizontalSpacing(Index: Integer; AValue: Integer);
begin
  FHorizontalSpacingDic.AddOrSetValue(Index, AValue);
end;

procedure TGridLayout.SetLeft(AValue: Integer);
begin
  if FLeft = AValue then
    Exit;
  FLeft := AValue;
end;

procedure TGridLayout.SetRowHeight(Index: Integer; Value: Integer);
begin
  FRowHeightDic.AddOrSetValue(Index, Value);
end;

function TGridLayout.GetColumnWidth(Index: Integer): Integer;
begin
  if not FColumnWidthDic.TryGetValue(Index, Result) then
    Result := FColumnWidths;
end;

procedure TGridLayout.SetColumnWidth(Index: Integer; Value: Integer);
begin
  FColumnWidthDic.AddOrSetValue(Index, Value);
end;

procedure TGridLayout.SetRowShift(Index: Integer; AValue: Integer);
begin
  FRowShift.AddOrSetValue(Index, AValue);
end;

procedure TGridLayout.SetTop(AValue: Integer);
begin
  if FTop = AValue then Exit;
  FTop := AValue;
end;

procedure TGridLayout.SetVerticalSpacing(Index: Integer; AValue: Integer);
begin
  FVerticalSpacingDic.AddOrSetValue(Index, AValue);
end;

procedure TGridLayout.SetVisibleColumn(Index: Integer; AValue: Boolean);
begin
  if AValue then
  begin
    if FHiddenColumns.Contains(Index) then
      FHiddenColumns.Remove(Index);
  end
  else
  begin
    if not FHiddenColumns.Contains(Index) then
      FHiddenColumns.Add(Index);
  end;
end;

procedure TGridLayout.SetVisibleRow(Index: Integer; AValue: Boolean);
begin
  if AValue then
  begin
    if FHiddenRows.Contains(Index) then
      FHiddenRows.Remove(Index);
  end
  else
  begin
    if not FHiddenRows.Contains(Index) then
      FHiddenRows.Add(Index);
  end;
end;

function TGridLayout.CalculateCellLeft(Cell: TGridCell): Integer;
var
  I: Integer;
begin
  Result := Self.RowShift[Cell.Row] + Margins.Left;
  for I := 0 to Cell.Column - 1 do
    if VisibleColumn[I] then
      Result := Result
        + GetColumnWidth(I)
        + GetHorizontalSpacing(I);
end;

function TGridLayout.CalculateCellHeight(Cell: TGridCell): Integer;
  function GetLastVisibleRow: Integer;
  var
    I: Integer;
  begin
    Result := -1;
    for I := Cell.Row + Cell.RowSpan - 1 downto Cell.Row do
      if VisibleRow[I] then
        Exit(I);
  end;

  function CalculateTotalSpacings: Integer;
  var
    I, LastVisibleRow: Integer;
  begin
    Result := 0;
    LastVisibleRow := GetLastVisibleRow;
    for I := Cell.Row to LastVisibleRow - 1 do
      if VisibleRow[I] then
        Inc(Result, VerticalSpacing[I]);
  end;

  function CalculateTotalHeight: Integer;
  var
    I: Integer;
  begin
    Result := 0;

    for I := Cell.Row to Cell.Row + Cell.RowSpan - 1 do
      if not VisibleRow[I] then
        Continue
      else
        Result := Result + GetRowHeight(I);
  end;
begin
  Result := 0;

  if Cell.Row >= FRows then
    Exit;

  Result := Result
    + CalculateTotalHeight
    + CalculateTotalSpacings;
end;

function TGridLayout.CalculateCellWidth(Cell: TGridCell): Integer;
//     horizontal spacing Index
//     |               Margin
//     0   1   2   3   |
// \---|---|---|---|---/
//   0   1   2   3   4
//   |
//   Column Width Index

  function GetLastVisibleCol: Integer;
  var
    I: Integer;
  begin
    Result := -1;
    for I := Cell.Column + Cell.ColSpan - 1 downto Cell.Column do
      if VisibleColumn[I] then
        Exit(I);
  end;

  function CalculateTotalSpacings: Integer;
  var
    I, LastVisibleCol: Integer;
  begin
    Result := 0;
    LastVisibleCol := GetLastVisibleCol;
    for I := Cell.Column to LastVisibleCol - 1 do
      if VisibleColumn[I] then
        Inc(Result, HorizontalSpacing[I]);
  end;

  function CalculateTotalWidth: Integer;
  var
    I: Integer;
  begin
    Result := 0;

    for I := Cell.Column to Cell.Column + Cell.ColSpan - 1 do
      if not VisibleColumn[I] then
        Continue
      else
        Result := Result + GetColumnWidth(I);
  end;

begin
  Result := 0;

  if Cell.Column >= FColumns then
    Exit;

  Result := Result
    + CalculateTotalWidth
    + CalculateTotalSpacings;
end;

function TGridLayout.CalculateCellTop(Cell: TGridCell): Integer;
var
  Row, I: Integer;
begin
  Result := Self.ColumnShift[Cell.Column] + Margins.Top;
  for I := 0 to Cell.Row - 1 do
    if VisibleRow[I] then
      Result := Result
        + GetRowHeight(I)
        + GetVerticalSpacing(I);
end;

procedure TGridLayout.ArrangeItems(ALeft, ATop: Integer);
var
  Cell: TGridCell;
  Item: ILayoutItem;
  X, Y, W, H: Integer;
  Control: TControl;
begin
  ApplyCellsVisibility;

  for Cell in FCells do
  begin
    Item := Cell.Item;
    if Item = nil then
      Continue;

    Control := Item.GetControl;
    if not Assigned(Control) then
      Continue;

    if not IsVisibleCell(Cell) then
      Continue;

    // Posição e tamanho baseados na célula
    X := CalculateCellLeft(Cell);
    Y := CalculateCellTop(Cell);
    W := CalculateCellWidth(Cell);
    H := CalculateCellHeight(Cell);

    // Ajuste Horizontal
    case Cell.HorizontalAlignment of
      laStretch:
        W := W + Cell.ExtraWidth;
      laCenter:
        begin
          W := Control.Width;
          X := X + (CalculateCellWidth(Cell) - W) div 2;
        end;
      laStart:
        begin
          W := Control.Width;
        end;
      laEnd:
        begin
          W := Control.Width;
          X := X + (CalculateCellWidth(Cell) - W);
        end;
    end;

    // Ajuste Vertical
    case Cell.VerticalAlignment of
      laStretch:
        H := H + Cell.ExtraHeight;
      laCenter:
        begin
          H := Control.Height;
          Y := Y + (CalculateCellHeight(Cell) - H) div 2;
        end;
      laStart:
        begin
          H := Control.Height;
        end;
      laEnd:
        begin
          H := Control.Height;
          Y := Y + (CalculateCellHeight(Cell) - H);
        end;
    end;

    Item.SetBounds(
      FLeft + ALeft + X + Cell.OffsetX,
      FTop + ATop + Y + Cell.OffsetY,
      W,
      H
    );
  end;
end;

procedure TGridLayout.ApplyCellsVisibility;
var
  Cell: TGridCell;
begin
  for Cell in FCells do
    Cell.Item.GetControl.Visible := IsVisibleCell(Cell);
end;

function TGridLayout.IsVisibleCell(Cell: TGridCell): Boolean;
begin
  Result := VisibleRow[Cell.Row] and VisibleColumn[Cell.Column];
end;

procedure TGridLayout.ArrangeItems;
begin
  Self.ArrangeItems(0, 0);
end;

{ TControlLayoutItem }

procedure TControlLayoutItem.AfterSetBounds;
begin
  // Nessa classe não faz nada
end;

constructor TControlLayoutItem.Create(AControl: TControl);
begin
  inherited Create;
  FControl := AControl;
end;

function TControlLayoutItem.GetControl: TControl;
begin
  Result := FControl;
end;

procedure TControlLayoutItem.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  FControl.SetBounds(ALeft, ATop, AWidth, AHeight);
  AfterSetBounds;
end;

{ TSubGridLayoutItem }

procedure TSubGridLayoutItem.AfterSetBounds;
var
  IsVirtual: Boolean;
begin
  IsVirtual := (Self.FControl is TVirtualContainer);

  if Assigned(FLayout) then
    if IsVirtual then
      FLayout.ArrangeItems(Self.FContainer.Left, Self.FContainer.Top)
    else
      FLayout.ArrangeItems;
end;

procedure TSubGridLayoutItem.ContainerResize(Sender: TObject);
begin
  FLayout.RowHeights := TVirtualContainer(Sender).Height div FLayout.Rows;
  FLayout.ColumnWidths := TVirtualContainer(Sender).Width div FLayout.Columns;
end;

constructor TSubGridLayoutItem.Create(ALayout: TGridLayout);
begin
  inherited Create(nil);
  FContainer := TVirtualContainer.Create(nil);
  FLayout := ALayout;
  FControl := FContainer;

  FContainer.OnResize := @ContainerResize;
end;

constructor TSubGridLayoutItem.CreateWithContainerClass(ALayout: TGridLayout;
  AOwner: TWinControl; AContainerClass: TWinControlClass);
begin
  inherited Create(nil); // Nenhum controle visível é passado diretamente

  FLayout := ALayout;

  FContainer := AContainerClass.Create(AOwner);
  FContainer.Parent := AOwner;
  FContainer.Align := alNone;
  FContainer.Caption := '';
  FContainer.Visible := True;

  // Define o item de layout como sendo o container
  FControl := FContainer;
end;

destructor TSubGridLayoutItem.Destroy;
begin
  if FControl is TVirtualContainer then
    FControl.Free;
  inherited Destroy;
end;

end.

