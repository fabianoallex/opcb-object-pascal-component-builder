unit ULayout;

{$IFDEF FPC}
{$mode ObjFPC}{$H+}
{$MODESWITCH ADVANCEDRECORDS}
{$ENDIF}

interface

uses
  Classes, SysUtils, Generics.Collections, Generics.Defaults;

type
  IGridItem = interface
    ['{7A972D12-00D4-4113-96C3-880C95E3FCD1}']
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
    function GetWidth: Integer;
    procedure SetWidth(AValue: Integer);
    function GetHeight: Integer;
    procedure SetHeight(AValue: Integer);
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
    function GetLeft: Integer;
    function GetTop: Integer;
    procedure Redraw;
  end;

  TItemAlignment = (laStretch, laCenter, laStart, laEnd);

  { TOptionalInt }

  TOptionalInt = record
    HasValue: Boolean;
    Value: Integer;
    {$IFDEF FPC}
    class operator :=(AValue: Integer): TOptionalInt;
    {$ELSE}
    class operator Implicit(AValue: Integer): TOptionalInt;
    {$ENDIF}
    class function Some(AValue: Integer): TOptionalInt; static;
    class function None: TOptionalInt; static;
  end;

  { TGridTrackInfo}

  TGridTrackInfo = record      // col      | row
    Size: TOptionalInt;        // width    | hight
    Spacing: TOptionalInt;     // vertical | horizontal
    Shift: TOptionalInt;       // vertical | horizontal
    Hidden: Boolean;
    class function Default: TGridTrackInfo; static;
  end;

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
    FHorizontalAlignment: TItemAlignment;
    FOffsetX: Integer;
    FOffsetY: Integer;
    FRow: Integer;
    FRowSpan: Integer;
    FVerticalAlignment: TItemAlignment;
  public
    constructor Create; overload;
    constructor Create(ARow, AColumn: Integer); overload;
    function WithRow(ARow: Integer): TGridCellSettings;
    function WithColumn(AColum: Integer): TGridCellSettings;
    function WithRowSpan(ARowSpan: Integer): TGridCellSettings;
    function WithColumnSpan(AColSpan: Integer): TGridCellSettings;
    function WithOffsetX(AOffsetX: Integer): TGridCellSettings;
    function WithOffsetY(AOffsetY: Integer): TGridCellSettings;
    function WithHorizontalAlignment(AHorizontalAlignment: TItemAlignment): TGridCellSettings;
    function WithVerticalAlignment(AVerticalAlignment: TItemAlignment): TGridCellSettings;
    function WithAlignment(AHorizontalAlignment: TItemAlignment;
      AVerticalAlignment: TItemAlignment): TGridCellSettings;
    function WithExtraWidth(AExtraWidth: Integer): TGridCellSettings;
    function WithExtraHeight(AExtraHeight: Integer): TGridCellSettings;
    property Row: Integer read FRow;
    property Column: Integer read FColumn;
    property ColSpan: Integer read FColSpan;
    property RowSpan: Integer read FRowSpan;
    property OffsetX: Integer read FOffsetX;
    property OffsetY: Integer read FOffsetY;
    property HorizontalAlignment: TItemAlignment read FHorizontalAlignment;
    property VerticalAlignment: TItemAlignment read FVerticalAlignment;
    property ExtraWidth: Integer read FExtraWidth;
    property ExtraHeight: Integer read FExtraHeight;
  end;

  { TGridCell }

  TGridCell = class
  private
    FExtraHeight: Integer;
    FExtraWidth: Integer;
    FItem: IGridItem;
    FRow: Integer;
    FColumn: Integer;
    FRowSpan: Integer;
    FColSpan: Integer;
    FOffsetX: Integer;
    FOffsetY: Integer;
    FHorizontalAlignment: TItemAlignment;
    FVerticalAlignment: TItemAlignment;
  public
    constructor Create(AGridItem: IGridItem; ASettings:
      TGridCellSettings); reintroduce; overload;
    property Item: IGridItem read FItem;
    property Row: Integer read FRow;
    property Column: Integer read FColumn;
    property RowSpan: Integer read FRowSpan;
    property ColSpan: Integer read FColSpan;
    property OffsetX: Integer read FOffsetX write FOffsetX;
    property OffsetY: Integer read FOffsetY write FOffsetY;
    property ExtraWidth: Integer read FExtraWidth write FExtraWidth;
    property ExtraHeight: Integer read FExtraHeight write FExtraWidth;
    property HorizontalAlignment: TItemAlignment read FHorizontalAlignment write FHorizontalAlignment;
    property VerticalAlignment: TItemAlignment read FVerticalAlignment write FVerticalAlignment;
  end;

  IGridFill = interface;
  TGridLayout = class;

  IGridLayoutListener = interface
    ['{16C54C27-A5F5-4651-8ACA-B804C36787E8}']
    procedure LayoutChanged(AGrid: TGridLayout);
  end;

  TGridFillBeforePlaceEvent = procedure(
    AGridFill: IGridFill;
    AGrid: TGridLayout;
    AItem: IGridItem;
    APos: IGridPosition;
    var ASettings: TGridCellSettings;
    var ACanPlace: Boolean
  ) of object;

  TGridFillAfterPlaceEvent = procedure(
    AGridFill: IGridFill;
    AGrid: TGridLayout;
    AItem: IGridItem;
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
    procedure PlaceItem(AItem: IGridItem); overload;
    procedure PlaceItem(AItem: IGridItem; ASettings: TGridCellSettings); overload;
    procedure Skip(ACount: Integer=1);
    procedure InitialPos(APos: IGridPosition);
    function NextPosition: IGridPosition;
    property Grid: TGridLayout read GetGrid;
    property OnBeforePlaceItem: TGridFillBeforePlaceEvent read GetOnBeforePlaceItem write SetOnBeforePlaceItem;
    property OnAfterPlaceItem: TGridFillAfterPlaceEvent read GetOnAfterPlaceItem write SetOnAfterPlaceItem;
    property Position: IGridPosition read GetPosition;
  end;

  TGridCellList = {$IFDEF FPC}specialize{$ENDIF} TObjectList<TGridCell>;

  TIntList = {$IFDEF FPC}specialize{$ENDIF} TList<Integer>;

  { TIntegerKeyDictionary }

  {$IFDEF FPC}
  generic TIntegerKeyDictionary<T> = class(specialize TDictionary<Integer, T>)
  {$ELSE}
  TIntegerKeyDictionary<T> = class(TDictionary<Integer, T>)
  {$ENDIF}
  public
    procedure MoveKey(const FromIndex, ToIndex: Integer);
    procedure AddWithShiftAt(AIndex: Integer; const AValue: T);
  end;

  { TGridTrackInfoDictionary }

  TGridTrackInfoDictionary = class({$IFDEF FPC}specialize{$ENDIF} TIntegerKeyDictionary<TGridTrackInfo>)
    function GetSizeOrDefault(Index: Integer; const ADefault: Integer): Integer;
    procedure SetSize(AIndex: Integer; ASize: Integer);
    function IsSizeDefined(AIndex: Integer): Boolean;
    procedure ClearSizes;
    function GetSpacingOrDefault(Index: Integer; const ADefault: Integer): Integer;
    procedure SetSpacing(AIndex: Integer; ASpacing: Integer);
    function IsSpacingDefined(AIndex: Integer): Boolean;
    procedure ClearSpacings;
    function GetShiftOrDefault(Index: Integer; const ADefault: Integer): Integer;
    procedure SetShift(AIndex: Integer; AShift: Integer);
    function IsShiftDefined(AIndex: Integer): Boolean;
    procedure ClearShifts;
    function GetHidden(Index: Integer): Boolean;
    procedure SetHidden(AIndex: Integer; AValue: Boolean);
    procedure InsertTrackAt(AIndex: Integer);
    procedure MoveTrack(AFrom, ATo: Integer);
  end;

  { TGridLayout }

  TGridLayout = class
  private
    FLeft: Integer;
    FMargins: TMargins;
    FRows: Integer;
    FColumns: Integer;
    FTop: Integer;
    FVerticalSpacings: Integer;
    FHorizontalSpacings: Integer;
    FRowHeights: Integer;
    FColumnWidths: Integer;
    FCells: TGridCellList;
    FRowsInfo: TGridTrackInfoDictionary;
    FColumnsInfo: TGridTrackInfoDictionary;
    FListeners: TInterfaceList;
    function CalculateCellWidth(Cell: TGridCell): Integer;
    function CalculateCellHeight(Cell: TGridCell): Integer;
    function CalculateCellLeft(Cell: TGridCell): Integer;
    function CalculateCellTop(Cell: TGridCell): Integer;
    function CreateDefaultSettings(ARow, AColumn: Integer): TGridCellSettings;
    function GetColumnShift(ACol: Integer): Integer;
    function GetContentHeight: Integer;
    function GetContentWidth: Integer;
    function GetHorizontalSpacing(AIndex: Integer): Integer;
    function GetRowHeight(AIndex: Integer): Integer;
    function GetRowShift(ARow: Integer): Integer;
    function GetVerticalSpacing(ARow: Integer): Integer;
    function GetVisibleColumn(ACol: Integer): Boolean;
    function GetVisibleRow(ARow: Integer): Boolean;
    procedure SetColumnShift(ACol: Integer; AValue: Integer);
    procedure SetHorizontalSpacing(ACol: Integer; AValue: Integer);
    procedure SetLeft(AValue: Integer);
    procedure SetRowHeight(AIndex: Integer; AValue: Integer);
    function GetColumnWidth(AIndex: Integer): Integer;
    procedure SetColumnWidth(AIndex: Integer; AValue: Integer);
    procedure SetRowShift(ARow: Integer; AValue: Integer);
    procedure SetTop(AValue: Integer);
    procedure SetVerticalSpacing(ARow: Integer; AValue: Integer);
    procedure SetVisibleColumn(ACol: Integer; AValue: Boolean);
    procedure SetVisibleRow(ARow: Integer; AValue: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddItem(AItem: IGridItem; ASettings: TGridCellSettings); overload;
    procedure ArrangeItems; overload;
    procedure ArrangeItems(ALeft, ATop: Integer); overload;
    procedure ApplyCellsVisibility;
    function IsVisibleCell(Cell: TGridCell): Boolean;
    procedure Clear;
    procedure ClearRowAndColumnShifts;
    procedure ResetColumnWidthsToDefault;
    procedure ResetRowHeightsToDefault;
    procedure ResetColumnShift;
    procedure ResetRowShift;
    procedure ResetVerticalSpacing;
    procedure ResetHorizontalSpacing;
    function IsCellOccupied(ARow, ACol: Integer): Boolean;
    function IsCellSpan(ARow, ACol: Integer): Boolean;
    function IsColumnWidthCustomized(ACol: Integer): Boolean;
    function IsVerticalSpacingCustomized(ARow: Integer): Boolean;
    function IsHorizontalSpacingCustomized(ACol: Integer): Boolean;
    procedure InsertRow(ARow: Integer);
    procedure InsertColumn(AColumn: Integer);
    function IsInTopMargin(Y: Integer): Boolean;
    function IsInBottomMargin(Y: Integer): Boolean;
    function IsInLeftMargin(X: Integer): Boolean;
    function IsInRightMargin(X: Integer): Boolean;
    function IsInVerticalSpacing(X, Y: Integer): Boolean;
    function IsInHorizontalSpacing(X, Y: Integer): Boolean;
    function GetCell(ARow, AColumn: Integer): TGridCell;
    procedure AddListener(const AListener: IGridLayoutListener);
    procedure RemoveListener(const AListener: IGridLayoutListener);
    procedure NotifyLayoutChange;
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

  TGridLayoutCompositeOrientation = (gcoHorizontal, gcoVertical);

  { TGridLayoutNode }

  TGridLayoutNode = class
  private
    FGrid: TGridLayout;
    FNext: TGridLayoutNode;
    procedure SetGrid(AValue: TGridLayout);
    procedure SetNext(AValue: TGridLayoutNode);
  public
    constructor Create(AGrid: TGridLayout);
    property Grid: TGridLayout read FGrid write SetGrid;
    property Next: TGridLayoutNode read FNext write SetNext;
  end;

  TIntIntDictionary = {$IFDEF FPC}specialize{$ENDIF} TDictionary<Integer, Integer>;

  { TGridLayoutComposite }

  TGridLayoutComposite = class(TInterfacedObject, IGridLayoutListener)
  private
    FOrientation: TGridLayoutCompositeOrientation;
    FHead: TGridLayoutNode;
    FDefaultSpacing: Integer;
    FSpacing: TIntIntDictionary;
    FTail: TGridLayoutNode;
    function ContainsGrid(AGrid: TGridLayout): Boolean;
    function GetContentHeight: Integer;
    function GetContentWidth: Integer;
    function GetSpacing(AIndex: Integer): Integer;
    procedure SetSpacing(AIndex: Integer; AValue: Integer);
    procedure SetDefaultSpacing(AValue: Integer);
  public
    constructor Create(AOrientation: TGridLayoutCompositeOrientation);
    destructor Destroy; override;
    procedure LayoutChanged(AGrid: TGridLayout);
    procedure AddGrids(AGrids: array of TGridLayout);
    procedure AddGrid(AGrid: TGridLayout);
    procedure AddGridAtBegin(AGrid: TGridLayout);
    procedure AddGridAfter(AGrid, AGridReference: TGridLayout);
    procedure AddGridBefore(AGrid, AGridReference: TGridLayout);
    procedure ArrangeGrids(ATop: Integer; ALeft: Integer);
    function GetNextGrid(AGrid: TGridLayout): TGridLayout;
    function GetGridIndex(AGrid: TGridLayout): Integer;
    property DefaultSpacing: Integer read FDefaultSpacing write SetDefaultSpacing;
    property Spacing[Index: Integer]: Integer read GetSpacing write SetSpacing;
    property ContentWidth: Integer read GetContentWidth;
    property ContentHeight: Integer read GetContentHeight;
  end;

implementation

uses
  Math;

{ TGridCell }

constructor TGridCell.Create(AGridItem: IGridItem; ASettings: TGridCellSettings);
begin
  Assert(AGridItem <> nil, 'AControl cannot be nil');
  Assert(ASettings <> nil, 'ASettings cannot be nil');
  FItem := AGridItem;
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

{ TOptionalInt }
{$IFDEF FPC}
class operator TOptionalInt.:=(AValue: Integer): TOptionalInt;
{$ELSE}
class operator TOptionalInt.Implicit(AValue: Integer): TOptionalInt;
{$ENDIF}
begin
  Result := TOptionalInt.Some(AValue);
end;

class function TOptionalInt.Some(AValue: Integer): TOptionalInt;
begin
  Result.HasValue := True;
  Result.Value := AValue;
end;

class function TOptionalInt.None: TOptionalInt;
begin
  Result.Value := -1;
  Result.HasValue := False;
end;

{ TGridTrackInfo }

class function TGridTrackInfo.Default: TGridTrackInfo;
begin
  Result.Size := TOptionalInt.None;
  Result.Shift := TOptionalInt.None;
  Result.Spacing := TOptionalInt.None;
  Result.Hidden := False;
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

constructor TGridCellSettings.Create;
begin
  Create(0, 0);
end;

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
  AHorizontalAlignment: TItemAlignment): TGridCellSettings;
begin
  FHorizontalAlignment := AHorizontalAlignment;
  Result := Self;
end;

function TGridCellSettings.WithVerticalAlignment(
  AVerticalAlignment: TItemAlignment): TGridCellSettings;
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

function TGridCellSettings.WithAlignment(AHorizontalAlignment: TItemAlignment;
  AVerticalAlignment: TItemAlignment): TGridCellSettings;
begin
  FHorizontalAlignment := AHorizontalAlignment;
  FVerticalAlignment := AVerticalAlignment;
  Result := Self;
end;

{ TGridLayout }

constructor TGridLayout.Create;
begin
  FListeners := TInterfaceList.Create;
  FCells := TGridCellList.Create(True);
  FRowsInfo := TGridTrackInfoDictionary.Create;
  FColumnsInfo := TGridTrackInfoDictionary.Create;
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
  FListeners.Free;
  FCells.Free;
  FRowsInfo.Free;
  FColumnsInfo.Free;
  FMargins.Free;
  inherited;
end;

procedure TGridLayout.AddItem(AItem: IGridItem; ASettings: TGridCellSettings);
begin
  Assert(ASettings <> nil, 'Settings cannot be nil');
  FCells.Add(TGridCell.Create(AItem, ASettings));
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

function TGridLayout.GetColumnShift(ACol: Integer): Integer;
begin
  Result := FColumnsInfo.GetShiftOrDefault(ACol, 0);
end;

function TGridLayout.GetContentHeight: Integer;
var
  I: Integer;
begin
  Result := FMargins.Top + FMargins.Bottom;
  for I := 0 to Rows - 1 do
    if VisibleRow[I] then
      Result := Result + GetRowHeight(I);

  for I := 0 to Rows - 2 do
    if VisibleRow[I] then
      Result := Result + GetVerticalSpacing(I);
end;

function TGridLayout.GetContentWidth: Integer;
var
  I: Integer;
begin
  Result := FMargins.Left + FMargins.Right;

  for I := 0 to Columns - 1 do
    if VisibleColumn[I] then
      Result := Result + GetColumnWidth(I);

  for I := 0 to Columns - 2 do
    if VisibleColumn[I] then
      Result := Result + GetHorizontalSpacing(I);
end;

function TGridLayout.GetHorizontalSpacing(AIndex: Integer): Integer;
begin
  Result := FColumnsInfo.GetSpacingOrDefault(AIndex, FHorizontalSpacings);
end;

procedure TGridLayout.Clear;
begin
  FCells.Clear;
end;

procedure TGridLayout.ClearRowAndColumnShifts;
begin
  FRowsInfo.ClearShifts;
  FColumnsInfo.ClearShifts;
end;

procedure TGridLayout.ResetColumnWidthsToDefault;
begin
  FColumnsInfo.ClearSizes;
end;

procedure TGridLayout.ResetRowHeightsToDefault;
begin
  FRowsInfo.ClearSizes;
end;

procedure TGridLayout.ResetColumnShift;
begin
  FColumnsInfo.ClearShifts;
end;

procedure TGridLayout.ResetRowShift;
begin
  FRowsInfo.ClearShifts;
end;

procedure TGridLayout.ResetVerticalSpacing;
begin
  FRowsInfo.ClearSpacings;
end;

procedure TGridLayout.ResetHorizontalSpacing;
begin
  FColumnsInfo.ClearSpacings;
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

function TGridLayout.IsCellSpan(ARow, ACol: Integer): Boolean;
var
  Cell: TGridCell;
  R, C: Integer;
begin
  for Cell in FCells do
    for R := Cell.Row to Cell.Row + Cell.RowSpan - 1 do
      for C := Cell.Column to Cell.Column + Cell.ColSpan - 1 do
        // Verifica se (ARow, ACol) está dentro da área de span,
        // mas ignora a célula original (Cell.Row, Cell.Column)
        if (R = ARow) and (C = ACol) and
           not ((R = Cell.Row) and (C = Cell.Column)) then
          Exit(True);

  Result := False;
end;

function TGridLayout.IsColumnWidthCustomized(ACol: Integer): Boolean;
begin
  Result := FColumnsInfo.IsSizeDefined(ACol);
end;

function TGridLayout.IsVerticalSpacingCustomized(ARow: Integer): Boolean;
begin
  Result := FRowsInfo.IsSpacingDefined(ARow);
end;

function TGridLayout.IsHorizontalSpacingCustomized(ACol: Integer): Boolean;
begin
  Result := FColumnsInfo.IsSpacingDefined(ACol);
end;

procedure TGridLayout.InsertRow(ARow: Integer);
var
  Cell: TGridCell;
begin
  for Cell in FCells do
    if Cell.Row >= ARow then
      Cell.FRow := Cell.FRow + 1;

  if ARow > FRows then
    FRows := ARow;

  FRows := FRows + 1;
  FRowsInfo.InsertTrackAt(ARow);
end;

procedure TGridLayout.InsertColumn(AColumn: Integer);
var
  Cell: TGridCell;
begin
  for Cell in FCells do
    if Cell.Column >= AColumn then
      Cell.FColumn := Cell.FColumn + 1;

  if AColumn > FColumns then
    FColumns := AColumn;

  FColumns := FColumns + 1;
  FColumnsInfo.InsertTrackAt(AColumn);
end;

function TGridLayout.GetRowHeight(AIndex: Integer): Integer;
begin
  Result := FRowsInfo.GetSizeOrDefault(AIndex, FRowHeights);
end;

function TGridLayout.GetRowShift(ARow: Integer): Integer;
begin
  Result := FRowsInfo.GetShiftOrDefault(ARow, 0);
end;

function TGridLayout.GetVerticalSpacing(ARow: Integer): Integer;
begin
  Result := FRowsInfo.GetSpacingOrDefault(ARow, FVerticalSpacings);
end;

function TGridLayout.GetVisibleColumn(ACol: Integer): Boolean;
begin
  if (ACol < 0) or (ACol >= Columns) then
    Result := False
  else
    Result := not FColumnsInfo.GetHidden(ACol);
end;

function TGridLayout.GetVisibleRow(ARow: Integer): Boolean;
begin
  if (ARow < 0) or (ARow >= Rows) then
    Result := False
  else
    Result := not FRowsInfo.GetHidden(ARow);
end;

procedure TGridLayout.SetColumnShift(ACol: Integer; AValue: Integer);
begin
  FColumnsInfo.SetShift(ACol, AValue);
end;

procedure TGridLayout.SetHorizontalSpacing(ACol: Integer; AValue: Integer);
begin
  FColumnsInfo.SetSpacing(ACol, AValue);
end;

procedure TGridLayout.SetLeft(AValue: Integer);
begin
  if FLeft = AValue then
    Exit;
  FLeft := AValue;
end;

procedure TGridLayout.SetRowHeight(AIndex: Integer; AValue: Integer);
begin
  FRowsInfo.SetSize(AIndex, AValue);
end;

function TGridLayout.GetColumnWidth(AIndex: Integer): Integer;
begin
  Result := FColumnsInfo.GetSizeOrDefault(AIndex, FColumnWidths);
end;

procedure TGridLayout.SetColumnWidth(AIndex: Integer; AValue: Integer);
begin
  FColumnsInfo.SetSize(AIndex, AValue);
end;

procedure TGridLayout.SetRowShift(ARow: Integer; AValue: Integer);
begin
  FRowsInfo.SetShift(ARow, AValue);
end;

procedure TGridLayout.SetTop(AValue: Integer);
begin
  if FTop = AValue then Exit;
  FTop := AValue;
end;

procedure TGridLayout.SetVerticalSpacing(ARow: Integer; AValue: Integer);
begin
  FRowsInfo.SetSpacing(ARow, AValue);
end;

procedure TGridLayout.SetVisibleColumn(ACol: Integer; AValue: Boolean);
begin
  FColumnsInfo.SetHidden(ACol, not AValue);
end;

procedure TGridLayout.SetVisibleRow(ARow: Integer; AValue: Boolean);
begin
  FRowsInfo.SetHidden(ARow, not AValue);
end;

function TGridLayout.GetCell(ARow, AColumn: Integer): TGridCell;
var
  Cell: TGridCell;
begin
  Result := nil;
  for Cell in FCells do
  begin
    if (Cell.Row = ARow) and (Cell.Column = AColumn) then
    begin
      Result := Cell;
      Break;
    end;
  end;
end;

procedure TGridLayout.AddListener(const AListener: IGridLayoutListener);
begin
  if FListeners.IndexOf(AListener) = -1 then
    FListeners.Add(AListener);
end;

procedure TGridLayout.RemoveListener(const AListener: IGridLayoutListener);
begin
  FListeners.Remove(AListener);
end;

procedure TGridLayout.NotifyLayoutChange;
var
  I: Integer;
begin
  for I := 0 to FListeners.Count - 1 do
    IGridLayoutListener(FListeners[I]).LayoutChanged(Self);
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

function TGridLayout.IsInTopMargin(Y: Integer): Boolean;
begin
  Result :=
    (Y >= Top)
    and
    (Y < Top + Margins.Top);
end;

function TGridLayout.IsInBottomMargin(Y: Integer): Boolean;
var
  BottomStart: Integer;
begin
  BottomStart := Top + GetContentHeight - Margins.Bottom;
  Result :=
    (Y >= BottomStart)
    and
    (Y < Top + GetContentHeight);
end;

function TGridLayout.IsInLeftMargin(X: Integer): Boolean;
begin
  Result :=
    (X >= Left)
    and
    (X < Left + Margins.Left);
end;

function TGridLayout.IsInRightMargin(X: Integer): Boolean;
begin
  Result :=
    (X >= Left + ContentWidth - Margins.Right)
    and
    (X <  Left + ContentWidth);
end;

function TGridLayout.IsInVerticalSpacing(X, Y: Integer): Boolean;
var
  RowIndex, CurrentY, SpacingHeight: Integer;
begin
  Result := False;
  CurrentY := Top + Margins.Top;

  for RowIndex := 0 to Rows - 1 do
  begin
    if not VisibleRow[RowIndex] then
      Continue;

    CurrentY := CurrentY + GetRowHeight(RowIndex);

    // Verifica espaçamento, exceto após a última linha
    if RowIndex < Rows - 1 then
    begin
      SpacingHeight := GetVerticalSpacing(RowIndex);
      if (Y >= CurrentY) and (Y < CurrentY + SpacingHeight) then
      begin
        Result := True;
        Exit;
      end;

      CurrentY := CurrentY + SpacingHeight;
    end;
  end;
end;

function TGridLayout.IsInHorizontalSpacing(X, Y: Integer): Boolean;
var
  ColIndex, CurrentX, SpacingWidth: Integer;
begin
  Result := False;
  CurrentX := Left + Margins.Left;

  for ColIndex := 0 to Columns - 1 do
  begin
    if not VisibleColumn[ColIndex] then
      Continue;

    CurrentX := CurrentX + GetColumnWidth(ColIndex);

    // Verifica espaçamento, exceto após a última coluna
    if ColIndex < Columns - 1 then
    begin
      SpacingWidth := GetHorizontalSpacing(ColIndex);
      if (X >= CurrentX) and (X < CurrentX + SpacingWidth) then
      begin
        Result := True;
        Exit;
      end;

      CurrentX := CurrentX + SpacingWidth;
    end;
  end;
end;

procedure TGridLayout.ArrangeItems(ALeft, ATop: Integer);
var
  Cell: TGridCell;
  Item: IGridItem;
  X, Y, W, H: Integer;
begin
  ApplyCellsVisibility;

  for Cell in FCells do
  begin
    Item := Cell.Item;
    if not Assigned(Item) then
      Continue;

    if not IsVisibleCell(Cell) then
      Continue;

    X := CalculateCellLeft(Cell);
    Y := CalculateCellTop(Cell);
    W := CalculateCellWidth(Cell);
    H := CalculateCellHeight(Cell);

    case Cell.HorizontalAlignment of
      laStretch:
        W := W + Cell.ExtraWidth;
      laCenter:
        begin
          W := Item.GetWidth;
          X := X + (CalculateCellWidth(Cell) - W) div 2;
        end;
      laStart:
        begin
          W := Item.GetWidth;
        end;
      laEnd:
        begin
          W := Item.GetWidth;
          X := X + (CalculateCellWidth(Cell) - W);
        end;
    end;

    case Cell.VerticalAlignment of
      laStretch:
        H := H + Cell.ExtraHeight;
      laCenter:
        begin
          H := Item.GetHeight;
          Y := Y + (CalculateCellHeight(Cell) - H) div 2;
        end;
      laStart:
        begin
          H := Item.GetHeight;
        end;
      laEnd:
        begin
          H := Item.GetHeight;
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

  NotifyLayoutChange;
end;

procedure TGridLayout.ApplyCellsVisibility;
var
  Cell: TGridCell;
  Item: IGridItem;
begin
  for Cell in FCells do
  begin
    if Assigned(Cell) then
      Item := Cell.Item;

    if Assigned(Item) then
      Item.SetVisible(IsVisibleCell(Cell));
  end;
end;

function TGridLayout.IsVisibleCell(Cell: TGridCell): Boolean;
begin
  Result := VisibleRow[Cell.Row] and VisibleColumn[Cell.Column];
end;

procedure TGridLayout.ArrangeItems;
begin
  Self.ArrangeItems(0, 0);
end;

{ TGridLayoutNode }

procedure TGridLayoutNode.SetGrid(AValue: TGridLayout);
begin
  if FGrid = AValue then
    Exit;
  FGrid := AValue;
end;

procedure TGridLayoutNode.SetNext(AValue: TGridLayoutNode);
begin
  if FNext = AValue then
    Exit;
  FNext := AValue;
end;

constructor TGridLayoutNode.Create(AGrid: TGridLayout);
begin
  FGrid := AGrid;
end;

{ TGridLayoutComposite }

constructor TGridLayoutComposite.Create
  (AOrientation: TGridLayoutCompositeOrientation);
begin
  FOrientation := AOrientation;
  FDefaultSpacing := 0;
  FSpacing := TIntIntDictionary.Create;
  FHead := nil;
  FTail := nil;
end;

destructor TGridLayoutComposite.Destroy;
var
  Node, Temp: TGridLayoutNode;
begin
  FSpacing.Free;

  Node := FHead;
  while Node <> nil do
  begin
    Temp := Node;
    Node := Node.Next;
    Temp.Free;
  end;
  inherited Destroy;
end;

procedure TGridLayoutComposite.LayoutChanged(AGrid: TGridLayout);
var
  NextGrid: TGridLayout;
  Index: Integer;
begin
  NextGrid := GetNextGrid(AGrid);
  Index := GetGridIndex(AGrid);

  if not Assigned(NextGrid) then
    Exit;

  if FOrientation = gcoHorizontal then
  begin
    NextGrid.Top := AGrid.Top;
    NextGrid.Left := AGrid.Left + AGrid.ContentWidth + Spacing[Index];
  end;

  if FOrientation = gcoVertical then
  begin
    NextGrid.Top := AGrid.Top + AGrid.ContentHeight + Spacing[Index];
    NextGrid.Left := AGrid.Left;
  end;

  NextGrid.ArrangeItems;
end;

procedure TGridLayoutComposite.AddGrids(AGrids: array of TGridLayout);
var
  I: Integer;
begin
  for I:=0 to High(AGrids) do
    AddGrid(AGrids[I]);
end;

function TGridLayoutComposite.ContainsGrid(AGrid: TGridLayout): Boolean;
// foi usado um algoritmo de verificação O(n) pois geralmente são poucos grids.
// pode ser substituido por dictionary caso necessário.
var
  Node: TGridLayoutNode;
begin
  Node := FHead;
  while Node <> nil do
  begin
    if Node.Grid = AGrid then
      Exit(True);
    Node := Node.Next;
  end;
  Result := False;
end;

function TGridLayoutComposite.GetContentHeight: Integer;
var
  Node: TGridLayoutNode;
  Index: Integer;

  function CalcSpacing: Integer;
  begin
    Result := 0;
    if Node.Next <> nil then       // o ultimo espaço não conta
      Result := GetSpacing(Index)
  end;

begin
  Result := 0;
  Node := FHead;
  Index := 0;
  while Node <> nil do
  begin
    if FOrientation = gcoVertical then
      Result := Result + Node.Grid.ContentHeight + CalcSpacing
    else
      Result := Max(Result, Node.Grid.ContentHeight);

    Inc(Index);
    Node := Node.Next;
  end;
end;

function TGridLayoutComposite.GetContentWidth: Integer;
var
  Node: TGridLayoutNode;
  Index: Integer;

  function CalcSpacing: Integer;
  begin
    Result := 0;
    if Node.Next <> nil then       // o ultimo espaço não conta
      Result := GetSpacing(Index)
  end;

begin
  Result := 0;
  Node := FHead;
  while Node <> nil do
  begin
    if FOrientation = gcoHorizontal then
      Result := Result + Node.Grid.ContentWidth + CalcSpacing
    else
      Result := Max(Result, Node.Grid.ContentWidth);

    Inc(Index);
    Node := Node.Next;
  end;
end;

function TGridLayoutComposite.GetSpacing(AIndex: Integer): Integer;
begin
  if not FSpacing.TryGetValue(AIndex, Result) then
    Result := FDefaultSpacing;
end;

procedure TGridLayoutComposite.SetSpacing(AIndex: Integer; AValue: Integer);
begin
  FSpacing.AddOrSetValue(AIndex, AValue);
end;

procedure TGridLayoutComposite.SetDefaultSpacing(AValue: Integer);
begin
  if FDefaultSpacing = AValue then Exit;
  FDefaultSpacing := AValue;
end;

procedure TGridLayoutComposite.AddGrid(AGrid: TGridLayout);
var
  Node: TGridLayoutNode;
begin
  if ContainsGrid(AGrid) then
    Exit;

  AGrid.AddListener(Self);

  Node := TGridLayoutNode.Create(AGrid);

  if FHead = nil then
    FHead := Node
  else
    FTail.Next := Node;

  FTail := Node;
end;

procedure TGridLayoutComposite.AddGridAtBegin(AGrid: TGridLayout);
var
  NewNode: TGridLayoutNode;
begin
  if ContainsGrid(AGrid) then
    Exit;

  NewNode := TGridLayoutNode.Create(AGrid);
  AGrid.AddListener(Self);

  NewNode.Next := FHead;
  FHead := NewNode;

  if FTail = nil then
    FTail := NewNode;
end;

procedure TGridLayoutComposite.AddGridAfter(AGrid, AGridReference: TGridLayout);
var
  Current: TGridLayoutNode;
  NewNode: TGridLayoutNode;
begin
  if ContainsGrid(AGrid) then
    Exit;

  Current := FHead;
  while (Current <> nil) and (Current.Grid <> AGridReference) do
    Current := Current.Next;

  if Current = nil then
    Exit;

  NewNode := TGridLayoutNode.Create(AGrid);
  AGrid.AddListener(Self);

  NewNode.Next := Current.Next;
  Current.Next := NewNode;

  if Current = FTail then
    FTail := NewNode;
end;

procedure TGridLayoutComposite.AddGridBefore(AGrid, AGridReference: TGridLayout);
var
  Current, Prev: TGridLayoutNode;
  NewNode: TGridLayoutNode;
begin
  if ContainsGrid(AGrid) then
    Exit;

  Current := FHead;
  Prev := nil;

  while (Current <> nil) and (Current.Grid <> AGridReference) do
  begin
    Prev := Current;
    Current := Current.Next;
  end;

  if Current = nil then
    Exit;

  NewNode := TGridLayoutNode.Create(AGrid);
  AGrid.AddListener(Self);

  NewNode.Next := Current;

  if Prev = nil then
    FHead := NewNode
  else
    Prev.Next := NewNode;
end;

procedure TGridLayoutComposite.ArrangeGrids(ATop: Integer; ALeft: Integer);
begin
  if not Assigned(FHead.Grid) then
    Exit;

  FHead.Grid.Top := ATop;
  FHead.Grid.Left := ALeft;
  FHead.Grid.ArrangeItems;
end;

function TGridLayoutComposite.GetNextGrid(AGrid: TGridLayout): TGridLayout;
// foi usado um algoritmo de verificação O(n) pois geralmente são poucos grids.
// pode ser substituido por dictionary caso necessário.
var
  Node: TGridLayoutNode;
begin
  Node := FHead;
  while Node <> nil do
  begin
    if Node.Grid = AGrid then
    begin
      if Node.Next <> nil then
        Exit(Node.Next.Grid)
      else
        Exit(nil);
    end;
    Node := Node.Next;
  end;
  Result := nil;
end;

function TGridLayoutComposite.GetGridIndex(AGrid: TGridLayout): Integer;
// foi usado um algoritmo de verificação O(n) pois geralmente são poucos grids.
// pode ser substituido por dictionary caso necessário.
var
  Node: TGridLayoutNode;
begin
  Result := -1;
  Node := FHead;
  while Node <> nil do
  begin
    Inc(Result);
    if Node.Grid = AGrid then
      Exit;
    Node := Node.Next;
  end;
end;

{ TIntegerKeyDictionary }

{$IFDEF FPC}
procedure TIntegerKeyDictionary.MoveKey(const FromIndex, ToIndex: Integer);
{$ELSE}
procedure TIntegerKeyDictionary<T>.MoveKey(const FromIndex, ToIndex: Integer);
{$ENDIF}
var
  MovedValue: T;
  KeysToShift: {$IFDEF FPC}specialize{$ENDIF} TList<Integer>;
  Key: Integer;
  Direction: Integer;
  ExistFrom: Boolean;
begin
  if FromIndex = ToIndex then
    Exit;

  ExistFrom := Self.TryGetValue(FromIndex, MovedValue);

  KeysToShift := {$IFDEF FPC}specialize{$ENDIF} TList<Integer>.Create;
  try
    // Determine direção
    if FromIndex < ToIndex then
    begin
      // Mover para frente → deslocar [FromIndex+1 .. ToIndex] para trás
      for Key in Self.Keys do
        if (Key > FromIndex) and (Key <= ToIndex) then
          KeysToShift.Add(Key);
      KeysToShift.Sort;         // ordem crescente
      Direction := -1;
    end
    else if FromIndex > ToIndex then
    begin
      // Mover para trás ← deslocar [ToIndex .. FromIndex-1] para frente
      for Key in Self.Keys do
        if (Key >= ToIndex) and (Key < FromIndex) then
          KeysToShift.Add(Key);
      KeysToShift.Sort;
      KeysToShift.Reverse;      // ordem decrescente
      Direction := +1;
    end;

    if ExistFrom then
      Self.Remove(FromIndex);

    for Key in KeysToShift do
    begin
      Self.Add(Key + Direction, Self[Key]);
      Self.Remove(Key);
    end;

    if ExistFrom then
      Self.Add(ToIndex, MovedValue);
  finally
    KeysToShift.Free;
  end;
end;

{$IFDEF FPC}
procedure TIntegerKeyDictionary.AddWithShiftAt(AIndex: Integer; const AValue: T);
{$ELSE}
procedure TIntegerKeyDictionary<T>.AddWithShiftAt(AIndex: Integer; const AValue: T);
{$ENDIF}
var
  KeysToShift: {$IFDEF FPC}specialize{$ENDIF} TList<Integer>;
  Key: Integer;
  Obj: T;
begin
  KeysToShift := {$IFDEF FPC}specialize{$ENDIF} TList<Integer>.Create;
  try
    for Key in Self.Keys do
      if Key >= AIndex then
        KeysToShift.add(Key);

    KeysToShift.Sort;
    KeysToShift.Reverse;

    for Key in KeysToShift do
    begin
      Self.TryGetValue(Key, Obj);
      Self.Add(Key + 1, Obj);
      Self.Remove(Key);
    end;

    Self.AddOrSetValue(AIndex, AValue);
  finally
    KeysToShift.Free;
  end;
end;

{ TGridTrackInfoDictionary }

function TGridTrackInfoDictionary.GetSizeOrDefault(Index: Integer;
  const ADefault: Integer): Integer;
var
  Info: TGridTrackInfo;
begin
  if Self.TryGetValue(Index, Info) and Info.Size.HasValue then
    Result := Info.Size.Value
  else
    Result := ADefault;
end;

procedure TGridTrackInfoDictionary.SetSize(AIndex: Integer;
  ASize: Integer);
var
  Info: TGridTrackInfo;
begin
  if not Self.TryGetValue(AIndex, Info) then
    Info := TGridTrackInfo.Default;
  Info.Size := TOptionalInt.Some(ASize);
  Self.AddOrSetValue(AIndex, Info);
end;

procedure TGridTrackInfoDictionary.ClearSizes;
var
  Key: Integer;
  Info: TGridTrackInfo;
begin
  for Key in Self.Keys do
  begin
    if Self.TryGetValue(Key, Info) then
    begin
      Info.Size := TOptionalInt.None;
      Self[Key] := Info;
    end;
  end;
end;

function TGridTrackInfoDictionary.IsSizeDefined(AIndex: Integer
  ): Boolean;
var
  Info: TGridTrackInfo;
begin
  Result := Self.TryGetValue(AIndex, Info) and Info.Size.HasValue;
end;

function TGridTrackInfoDictionary.GetSpacingOrDefault
  (Index: Integer; const ADefault: Integer): Integer;
var
  Info: TGridTrackInfo;
begin
  if Self.TryGetValue(Index, Info) and Info.Spacing.HasValue then
    Result := Info.Spacing.Value
  else
    Result := ADefault;
end;

procedure TGridTrackInfoDictionary.SetSpacing(AIndex: Integer;
  ASpacing: Integer);
var
  Info: TGridTrackInfo;
begin
  if not Self.TryGetValue(AIndex, Info) then
    Info := TGridTrackInfo.Default;
  Info.Spacing := TOptionalInt.Some(ASpacing);
  Self.AddOrSetValue(AIndex, Info);
end;

function TGridTrackInfoDictionary.IsSpacingDefined(AIndex: Integer
  ): Boolean;
var
  Info: TGridTrackInfo;
begin
  Result := Self.TryGetValue(AIndex, Info) and Info.Spacing.HasValue;
end;

procedure TGridTrackInfoDictionary.ClearSpacings;
var
  Key: Integer;
  Info: TGridTrackInfo;
begin
  for Key in Self.Keys do
  begin
    if Self.TryGetValue(Key, Info) then
    begin
      Info.Spacing := TOptionalInt.None;
      Self[Key] := Info;
    end;
  end;
end;

function TGridTrackInfoDictionary.GetShiftOrDefault
  (Index: Integer; const ADefault: Integer): Integer;
var
  Info: TGridTrackInfo;
begin
  if Self.TryGetValue(Index, Info) and Info.Shift.HasValue then
    Result := Info.Shift.Value
  else
    Result := ADefault;
end;

procedure TGridTrackInfoDictionary.SetShift(AIndex: Integer;
  AShift: Integer);
var
  Info: TGridTrackInfo;
begin
  if not Self.TryGetValue(AIndex, Info) then
    Info := TGridTrackInfo.Default;
  Info.Shift := TOptionalInt.Some(AShift);
  Self.AddOrSetValue(AIndex, Info);
end;

function TGridTrackInfoDictionary.IsShiftDefined(AIndex: Integer
  ): Boolean;
var
  Info: TGridTrackInfo;
begin
  Result := Self.TryGetValue(AIndex, Info) and Info.Shift.HasValue;
end;

procedure TGridTrackInfoDictionary.ClearShifts;
var
  Key: Integer;
  Info: TGridTrackInfo;
begin
  for Key in Self.Keys do
  begin
    if Self.TryGetValue(Key, Info) then
    begin
      Info.Shift := TOptionalInt.None;
      Self[Key] := Info;
    end;
  end;
end;

function TGridTrackInfoDictionary.GetHidden(Index: Integer
  ): Boolean;
var
  Info: TGridTrackInfo;
begin
  if Self.TryGetValue(Index, Info) then
    Result := Info.Hidden
  else
    Result := False;
end;

procedure TGridTrackInfoDictionary.SetHidden(AIndex: Integer;
  AValue: Boolean);
var
  Info: TGridTrackInfo;
begin
  if not Self.TryGetValue(AIndex, Info) then
    Info := TGridTrackInfo.Default;
  Info.Hidden := AValue;
  Self.AddOrSetValue(AIndex, Info);
end;

procedure TGridTrackInfoDictionary.InsertTrackAt(AIndex: Integer);
begin
  Self.AddWithShiftAt(AIndex, TGridTrackInfo.Default);
end;

procedure TGridTrackInfoDictionary.MoveTrack(AFrom, ATo: Integer);
begin
  Self.MoveKey(AFrom, ATo);
end;

end.
