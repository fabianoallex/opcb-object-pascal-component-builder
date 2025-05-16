unit ULayout;

{$mode ObjFPC}{$H+}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils, Controls, Generics.Collections, Generics.Defaults;

type
  IVisualElement = interface
    ['{02F693DD-5377-477F-9B17-05906737F1F1}']
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
    function GetWidth: Integer;
    procedure SetWidth(AValue: Integer);
    function GetHeight: Integer;
    procedure SetHeight(AValue: Integer);
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
    function IsOfType(AClass: TClass): Boolean;
  end;

  { TControlVisualElement }

  TControlVisualElement = class(TInterfacedObject, IVisualElement)
  private
    FControl: TControl;
  public
    constructor Create(AControl: TControl);
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
    function GetWidth: Integer;
    procedure SetWidth(AValue: Integer);
    function GetHeight: Integer;
    procedure SetHeight(AValue: Integer);
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
    function IsOfType(AClass: TClass): Boolean;
    function GetControl: TControl;
  end;

  IGridItem = interface
    ['{7A972D12-00D4-4113-96C3-880C95E3FCD1}']
    function GetElement: IVisualElement;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
  end;

  TItemAlignment = (laStretch, laCenter, laStart, laEnd);

  { TOptionalInt }

  TOptionalInt = record
    HasValue: Boolean;
    Value: Integer;
    class operator :=(AValue: Integer): TOptionalInt;
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
    constructor Create(ARow, AColumn: Integer); reintroduce;
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
    constructor Create(AControl: IGridItem; ASettings:
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
  TIntList = specialize TList<Integer>;

  { TIntegerKeyDictionary }

  generic TIntegerKeyDictionary<T> = class(specialize TDictionary<Integer, T>)
  public
    procedure AddWithShiftAt(AIndex: Integer; const AValue: T);
    procedure MoveKey(const FromIndex, ToIndex: Integer);
  end;

  { TGridTrackInfoDictionary }

  TGridTrackInfoDictionary = class(specialize TIntegerKeyDictionary<TGridTrackInfo>)
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
    procedure AddItem(AItem: TControl; ASettings: TGridCellSettings); overload;
    procedure AddItem(AItem: TGridLayout; ASettings: TGridCellSettings); overload;
    procedure ArrangeItems; overload;
    procedure ArrangeItems(ALeft, ATop: Integer); overload;
    procedure ApplyCellsVisibility;
    function IsVisibleCell(Cell: TGridCell): Boolean;
    procedure Clear;
    procedure ClearRowAndColumnShifts;
    procedure ResetColumnWidthsToDefault;
    procedure ResetRowHeightsToDefault;
    function IsCellOccupied(ARow, ACol: Integer): Boolean;
    function IsColumnWidthCustomized(ACol: Integer): Boolean;
    function IsVerticalSpacingCustomized(ARow: Integer): Boolean;
    function IsHorizontalSpacingCustomized(ACol: Integer): Boolean;
    procedure InsertRow(ARow: Integer);
    procedure InsertColumn(AColumn: Integer);
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

  { TControlGridItem }

  TControlGridItem = class(TInterfacedObject, IGridItem)
  protected
    FControlElement: IVisualElement;
    procedure AfterSetBounds; virtual;
  public
    constructor Create(AControl: TControl);
    function GetElement: IVisualElement;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
  end;

  { TVirtualContainer }

  TVirtualContainer = class(TWinControl)
  end;

  { TSubGridItem }

  TSubGridItem = class(TControlGridItem)
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

constructor TGridCell.Create(AControl: IGridItem; ASettings: TGridCellSettings);
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

{ TControlVisualElement }

constructor TControlVisualElement.Create(AControl: TControl);
begin
  inherited Create;
  FControl := AControl;
end;

function TControlVisualElement.GetVisible: Boolean;
begin
  if not Assigned(FControl) then
    Exit;
  Result := FControl.Visible;
end;

procedure TControlVisualElement.SetVisible(AValue: Boolean);
begin
  if not Assigned(FControl) then
    Exit;
  FControl.Visible := AValue;
end;

function TControlVisualElement.GetWidth: Integer;
begin
  if not Assigned(FControl) then
    Exit;
  Result := FControl.Width;
end;

procedure TControlVisualElement.SetWidth(AValue: Integer);
begin
  if not Assigned(FControl) then
    Exit;
  FControl.Width := AValue;
end;

function TControlVisualElement.GetHeight: Integer;
begin
  if not Assigned(FControl) then
    Exit;
  Result := FControl.Height;
end;

procedure TControlVisualElement.SetHeight(AValue: Integer);
begin
  if not Assigned(FControl) then
    Exit;
  FControl.Height := AValue;
end;

procedure TControlVisualElement.SetBounds(ALeft, ATop, AWidth, AHeight: Integer
  );
begin
  if not Assigned(FControl) then
    Exit;
  FControl.SetBounds(ALeft, ATop, AWidth, AHeight);
end;

function TControlVisualElement.IsOfType(AClass: TClass): Boolean;
begin
  if not Assigned(FControl) then
    Exit;
  Result := FControl is AClass;
end;

function TControlVisualElement.GetControl: TControl;
begin
  Result := FControl;
end;

{ TOptionalInt }

class operator TOptionalInt.:=(AValue: Integer): TOptionalInt;
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

procedure TGridLayout.AddItem(AItem: TControl; ASettings: TGridCellSettings);
begin
  Self.AddItem(TControlGridItem.Create(AItem), ASettings);
end;

procedure TGridLayout.AddItem(AItem: TGridLayout; ASettings: TGridCellSettings);
begin
  Self.AddItem(TSubGridItem.Create(AItem), ASettings);
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
  Item: IGridItem;
  X, Y, W, H: Integer;
  Element: IVisualElement;
begin
  ApplyCellsVisibility;

  for Cell in FCells do
  begin
    Item := Cell.Item;
    if not Assigned(Item) then
      Continue;

    Element := Item.GetElement;
    if not Assigned(Element) then
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
          W := Element.GetWidth;
          X := X + (CalculateCellWidth(Cell) - W) div 2;
        end;
      laStart:
        begin
          W := Element.GetWidth;
        end;
      laEnd:
        begin
          W := Element.GetWidth;
          X := X + (CalculateCellWidth(Cell) - W);
        end;
    end;

    case Cell.VerticalAlignment of
      laStretch:
        H := H + Cell.ExtraHeight;
      laCenter:
        begin
          H := Element.GetHeight;
          Y := Y + (CalculateCellHeight(Cell) - H) div 2;
        end;
      laStart:
        begin
          H := Element.GetHeight;
        end;
      laEnd:
        begin
          H := Element.GetHeight;
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
  Item: IGridItem;
  Element: IVisualElement;
begin
  for Cell in FCells do
  begin
    begin
      if Assigned(Cell) then
        Item := Cell.Item;
      if Assigned(Item) then
        Element := Item.GetElement;
      if Assigned(Element) then
        Element.SetVisible(IsVisibleCell(Cell));
    end;
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

{ TIntegerKeyDictionary }

procedure TIntegerKeyDictionary.MoveKey(const FromIndex, ToIndex: Integer);
type
  TKeyList = specialize TList<Integer>;
var
  MovedValue: T;
  KeysToShift: TKeyList;
  Key: Integer;
  Direction: Integer;
  ExistFrom: Boolean;
begin
  if FromIndex = ToIndex then
    Exit;

  ExistFrom := Self.TryGetValue(FromIndex, MovedValue);

  KeysToShift := TKeyList.Create;
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

procedure TIntegerKeyDictionary.AddWithShiftAt(AIndex: Integer; const AValue: T);
type
  TKeyToShift = specialize TList<Integer>;
var
  KeysToShift: TKeyToShift;
  Key: Integer;
  Obj: T;
begin
  KeysToShift := TKeyToShift.Create;
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

{ TControlGridItem }

procedure TControlGridItem.AfterSetBounds;
begin
  // Nessa classe não faz nada
end;

constructor TControlGridItem.Create(AControl: TControl);
begin
  inherited Create;
  FControlElement := TControlVisualElement.Create(AControl);
end;

function TControlGridItem.GetElement: IVisualElement;
begin
  Result := FControlElement;
end;

procedure TControlGridItem.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  FControlElement.SetBounds(ALeft, ATop, AWidth, AHeight);
  AfterSetBounds;
end;

{ TSubGridItem }

procedure TSubGridItem.AfterSetBounds;
var
  IsVirtual: Boolean;
begin
  IsVirtual := Self.FControlElement.IsOfType(TVirtualContainer);

  if Assigned(FLayout) then
    if IsVirtual then
      FLayout.ArrangeItems(Self.FContainer.Left, Self.FContainer.Top)
    else
      FLayout.ArrangeItems;
end;

procedure TSubGridItem.ContainerResize(Sender: TObject);
begin
  FLayout.RowHeights := TVirtualContainer(Sender).Height div FLayout.Rows;
  FLayout.ColumnWidths := TVirtualContainer(Sender).Width div FLayout.Columns;
end;

constructor TSubGridItem.Create(ALayout: TGridLayout);
begin
  inherited Create(nil);
  FContainer := TVirtualContainer.Create(nil);
  FLayout := ALayout;
  FControlElement := TControlVisualElement.Create(FContainer);

  FContainer.OnResize := @ContainerResize;
end;

constructor TSubGridItem.CreateWithContainerClass(ALayout: TGridLayout;
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
  FControlElement := TControlVisualElement.Create(FContainer);
end;

destructor TSubGridItem.Destroy;
begin
  // if FControlElement.IsOfType(TVirtualContainer) then
  //  FControlElement.Free;
  inherited Destroy;
end;

end.

