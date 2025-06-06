unit GridLayoutComponent;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ULayout, Controls;

type
  TControlScope = (csForm, csParent, csCustom);
  TGridFillerType = (gftManual, gftRowFirst, gftColumnFirst);

  { TMargins }

  TMargins = class(TPersistent)
  private
    FLeft, FTop, FRight, FBottom: Integer;
    FOnChange: TNotifyEvent;
    procedure SetLeft(AValue: Integer);
    procedure SetTop(AValue: Integer);
    procedure SetRight(AValue: Integer);
    procedure SetBottom(AValue: Integer);
    procedure DoChange;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Left: Integer read FLeft write SetLeft default 0;
    property Top: Integer read FTop write SetTop default 0;
    property Right: Integer read FRight write SetRight default 0;
    property Bottom: Integer read FBottom write SetBottom default 0;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TGridLayoutComponent = class;

  { TControlItem }

  TControlItem = class(TCollectionItem)
  private
    FColSpan: Integer;
    FColumn: Integer;
    FControl: TControl;
    FHorizontalAlignment: TItemAlignment;
    FRow: Integer;
    FRowSpan: Integer;
    FVerticalAlignment: TItemAlignment;
    function GetOwnerComponent: TGridLayoutComponent;
    procedure SetColSpan(AValue: Integer);
    procedure SetColumn(AValue: Integer);
    procedure SetControl(AValue: TControl);
    procedure SetHorizontalAlignment(AValue: TItemAlignment);
    procedure SetRow(AValue: Integer);
    procedure SetRowSpan(AValue: Integer);
    procedure SetVerticalAlignment(AValue: TItemAlignment);
  protected
    procedure Changed;
  public
    constructor Create(ACollection: TCollection); override;
  published
    property Control: TControl read FControl write SetControl;
    property Row: Integer read FRow write SetRow;
    property Column: Integer read FColumn write SetColumn;
    property RowSpan: Integer read FRowSpan write SetRowSpan default 1;
    property ColSpan: Integer read FColSpan write SetColSpan default 1;
    property OwnerComponent: TGridLayoutComponent read GetOwnerComponent;
    property HorizontalAlignment: TItemAlignment read FHorizontalAlignment write SetHorizontalAlignment;
    property VerticalAlignment: TItemAlignment read FVerticalAlignment write SetVerticalAlignment;
  end;

  { TControlCollection }

  TControlCollection = class(TOwnedCollection)
  public
    function Add: TControlItem;
  end;

  { TGridTrackItem }

  TGridTrackItem = class(TCollectionItem)
  private
    FEnableShift: Boolean;
    FEnableSize: Boolean;
    FEnableSpacing: Boolean;
    FIndex: Integer;
    FSize, FSpacing, FShift: Integer;
    FHidden: Boolean;
    function GetOwnerComponent: TGridLayoutComponent;
    procedure SetEnableShift(AValue: Boolean);
    procedure SetEnableSize(AValue: Boolean);
    procedure SetEnableSpacing(AValue: Boolean);
    procedure SetHidden(AValue: Boolean);
    procedure SetShift(AValue: Integer);
    procedure SetSize(AValue: Integer);
    procedure SetSpacing(AValue: Integer);
    procedure SetTrackIndex(AValue: Integer);
  protected
    procedure Changed;
  published
    property Index: Integer read FIndex write SetTrackIndex; // linha/coluna
    property Size: Integer read FSize write SetSize;
    property Spacing: Integer read FSpacing write SetSpacing;
    property Shift: Integer read FShift write SetShift;
    property Hidden: Boolean read FHidden write SetHidden default False;
    property EnableSize: Boolean read FEnableSize write SetEnableSize ;
    property EnableSpacing: Boolean read FEnableSpacing write SetEnableSpacing ;
    property EnableShift: Boolean read FEnableShift write SetEnableShift ;
    property OwnerComponent: TGridLayoutComponent read GetOwnerComponent;
  end;

  { TGridTrackCollection }

  TGridTrackCollection = class(TOwnedCollection)
  public
    function GetItemByIndex(AIndex: Integer): TGridTrackItem;
  end;

  { TGridLayoutComponent }

  TGridLayoutComponent = class(TComponent)
  private
    FAutoArrange: Boolean;
    FColTracks: TGridTrackCollection;
    FContainer: TWinControl;
    FControlScope: TControlScope;
    FFillerType: TGridFillerType;
    FGridLayout: TGridLayout;
    FColumns: Integer;
    FColumnWidth: Integer;
    FControls: TControlCollection;
    FGridLeft: Integer;
    FHorizontalSpacing: Integer;
    FMargins: TMargins;
    FRowHeight: Integer;
    FRows: Integer;
    FGridTop: Integer;
    FRowTracks: TGridTrackCollection;
    FVerticalSpacing: Integer;
    function GetContentHeight: Integer;
    function GetContentWidth: Integer;
    procedure SetAutoArrange(AValue: Boolean);
    procedure SetColTracks(AValue: TGridTrackCollection);
    procedure SetColumns(AValue: Integer);
    procedure SetColumnWidth(AValue: Integer);
    procedure SetControls(AValue: TControlCollection);
    procedure SetFillerType(AValue: TGridFillerType);
    procedure SetGridLayout(AValue: TGridLayout);
    procedure SetGridLeft(AValue: Integer);
    procedure SetHorizontalSpacing(AValue: Integer);
    procedure SetMargins(AValue: TMargins);
    procedure SetRowHeight(AValue: Integer);
    procedure SetRows(AValue: Integer);
    procedure SetGridTop(AValue: Integer);
    procedure MarginsChanged(Sender: TObject);
    procedure SetRowTracks(AValue: TGridTrackCollection);
    procedure SetVerticalSpacing(AValue: Integer);
    procedure PopulateGrid;
  protected
    procedure UpdateGrid;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property ControlScope: TControlScope read FControlScope write FControlScope default csForm;
    property Container: TWinControl read FContainer write FContainer;
    property GridTop: Integer read FGridTop write SetGridTop;
    property GridLeft: Integer read FGridLeft write SetGridLeft;
    property ColumnWidth: Integer read FColumnWidth write SetColumnWidth;
    property RowHeight: Integer read FRowHeight write SetRowHeight;
    property Rows: Integer read FRows write SetRows;
    property Columns: Integer read FColumns write SetColumns;
    property GridLayout: TGridLayout read FGridLayout write SetGridLayout;
    property Controls: TControlCollection read FControls write SetControls;
    property AutoArrange: Boolean read FAutoArrange write SetAutoArrange default False;
    property Margins: TMargins read FMargins write SetMargins;
    property ColTracks: TGridTrackCollection read FColTracks write SetColTracks;
    property RowTracks: TGridTrackCollection read FRowTracks write SetRowTracks;
    property VerticalSpacing: Integer read FVerticalSpacing write SetVerticalSpacing;
    property HorizontalSpacing: Integer read FHorizontalSpacing write SetHorizontalSpacing;
    property ContentWidth: Integer read GetContentWidth;
    property ContentHeight: Integer read GetContentHeight;
    property FillerType: TGridFillerType read FFillerType write SetFillerType;
  end;

procedure Register;

implementation

uses
  UGridLayoutFillerFactory, ControlPropertyEditor, PropEdits;

procedure Register;
begin
  RegisterComponents('Layouts',[TGridLayoutComponent]);
  RegisterPropertyEditor(TypeInfo(TControl), TControlItem, 'Control', TControlPropertyEditor);
end;

{ TMargins }

procedure TMargins.SetLeft(AValue: Integer);
begin
  if FLeft <> AValue then
  begin
    FLeft := AValue;
    DoChange;
  end;
end;

procedure TMargins.SetTop(AValue: Integer);
begin
  if FTop <> AValue then
  begin
    FTop := AValue;
    DoChange;
  end;
end;

procedure TMargins.SetRight(AValue: Integer);
begin
  if FRight <> AValue then
  begin
    FRight := AValue;
    DoChange;
  end;
end;

procedure TMargins.SetBottom(AValue: Integer);
begin
  if FBottom <> AValue then
  begin
    FBottom := AValue;
    DoChange;
  end;
end;

procedure TMargins.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TMargins.Assign(Source: TPersistent);
begin
  if Source is TMargins then
  begin
    Left := TMargins(Source).Left;
    Top := TMargins(Source).Top;
    Right := TMargins(Source).Right;
    Bottom := TMargins(Source).Bottom;
  end
  else
    inherited Assign(Source);
end;

{ TControlItem }

procedure TControlItem.SetColumn(AValue: Integer);
begin
  if FColumn = AValue then Exit;
  FColumn := AValue;
  Changed;
end;

procedure TControlItem.SetControl(AValue: TControl);
begin
  if FControl <> AValue then
  begin
    if Assigned(FControl) then
      FControl.RemoveFreeNotification(OwnerComponent);

    FControl := AValue;

    if Assigned(FControl) then
      FControl.FreeNotification(OwnerComponent);

    Changed;
  end;

  {if FControl = AValue then Exit;
  FControl := AValue;
  Changed;}
end;

procedure TControlItem.SetHorizontalAlignment(AValue: TItemAlignment);
begin
  if FHorizontalAlignment = AValue then Exit;
  FHorizontalAlignment := AValue;
  Changed;
end;

procedure TControlItem.SetColSpan(AValue: Integer);
begin
  if FColSpan = AValue then Exit;
  FColSpan := AValue;
  Changed;
end;

function TControlItem.GetOwnerComponent: TGridLayoutComponent;
begin
  if Collection <> nil then
    Result := TGridLayoutComponent(Collection.Owner)
  else
    Result := nil;
end;

procedure TControlItem.SetRow(AValue: Integer);
begin
  if FRow = AValue then Exit;
  FRow := AValue;
  Changed;
end;

procedure TControlItem.SetRowSpan(AValue: Integer);
begin
  if FRowSpan = AValue then Exit;
  FRowSpan := AValue;
  Changed;
end;

procedure TControlItem.SetVerticalAlignment(AValue: TItemAlignment);
begin
  if FVerticalAlignment = AValue then Exit;
  FVerticalAlignment := AValue;
  Changed;
end;

procedure TControlItem.Changed;
begin
  OwnerComponent.UpdateGrid;
end;

constructor TControlItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FColSpan := 1;
  FRowSpan := 1;
end;

{ TControlCollection }

function TControlCollection.Add: TControlItem;
begin
  Result := TControlItem(inherited Add);
end;

{ TGridTrackItem }

procedure TGridTrackItem.SetEnableShift(AValue: Boolean);
begin
  if FEnableShift = AValue then Exit;
  FEnableShift := AValue;
  Changed;
end;

function TGridTrackItem.GetOwnerComponent: TGridLayoutComponent;
begin
  if Collection <> nil then
    Result := TGridLayoutComponent(Collection.Owner)
  else
    Result := nil;
end;

procedure TGridTrackItem.SetEnableSize(AValue: Boolean);
begin
  if FEnableSize = AValue then Exit;
  FEnableSize := AValue;
  Changed;
end;

procedure TGridTrackItem.SetEnableSpacing(AValue: Boolean);
begin
  if FEnableSpacing = AValue then Exit;
  FEnableSpacing := AValue;
  Changed;
end;

procedure TGridTrackItem.SetHidden(AValue: Boolean);
begin
  if FHidden = AValue then Exit;
  FHidden := AValue;
  Changed;
end;

procedure TGridTrackItem.SetShift(AValue: Integer);
begin
  if FShift = AValue then Exit;
  FShift := AValue;
  If EnableShift then
    Changed;
end;

procedure TGridTrackItem.SetSize(AValue: Integer);
begin
  if FSize = AValue then Exit;
  FSize := AValue;
  if EnableSize then
    Changed;
end;

procedure TGridTrackItem.SetSpacing(AValue: Integer);
begin
  if FSpacing = AValue then Exit;
  FSpacing := AValue;
  if EnableSpacing then
    Changed;
end;

procedure TGridTrackItem.SetTrackIndex(AValue: Integer);
begin
  if FIndex = AValue then Exit;
  FIndex := AValue;
  Changed;
end;

procedure TGridTrackItem.Changed;
begin
  OwnerComponent.UpdateGrid;
end;

{ TGridTrackCollection }

function TGridTrackCollection.GetItemByIndex(AIndex: Integer): TGridTrackItem;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if TGridTrackItem(Items[i]).Index = AIndex then
      Exit(TGridTrackItem(Items[i]));
  Result := nil;
end;

{ TGridLayoutComponent }

procedure TGridLayoutComponent.SetColumns(AValue: Integer);
begin
  if FColumns = AValue then Exit;
  FColumns := AValue;

  if AutoArrange then
    UpdateGrid;
end;

procedure TGridLayoutComponent.SetAutoArrange(AValue: Boolean);
begin
  if FAutoArrange = AValue then Exit;
  FAutoArrange := AValue;

  if AutoArrange then
    UpdateGrid;
end;

function TGridLayoutComponent.GetContentHeight: Integer;
begin
  Result := GridLayout.ContentHeight;
end;

function TGridLayoutComponent.GetContentWidth: Integer;
begin
  Result := GridLayout.ContentWidth;
end;

procedure TGridLayoutComponent.SetColTracks(AValue: TGridTrackCollection);
begin
  FColTracks.Assign(AValue);
end;

procedure TGridLayoutComponent.SetColumnWidth(AValue: Integer);
begin
  if FColumnWidth = AValue then Exit;
  FColumnWidth := AValue;

  if AutoArrange then
    UpdateGrid;
end;

procedure TGridLayoutComponent.SetControls(AValue: TControlCollection);
begin
  if FControls = AValue then Exit;
  FControls := AValue;
end;

procedure TGridLayoutComponent.SetFillerType(AValue: TGridFillerType);
begin
  if FFillerType = AValue then Exit;
  FFillerType := AValue;
  if AutoArrange then
    UpdateGrid;
end;

procedure TGridLayoutComponent.SetGridLayout(AValue: TGridLayout);
begin
  if FGridLayout = AValue then Exit;
  FGridLayout := AValue;
end;

procedure TGridLayoutComponent.SetGridLeft(AValue: Integer);
begin
  if FGridLeft = AValue then Exit;
  FGridLeft := AValue;
  if AutoArrange then
    UpdateGrid;
end;

procedure TGridLayoutComponent.SetHorizontalSpacing(AValue: Integer);
begin
  if FHorizontalSpacing = AValue then Exit;
  FHorizontalSpacing := AValue;
  if AutoArrange then
    UpdateGrid;
end;

procedure TGridLayoutComponent.SetMargins(AValue: TMargins);
begin
  if FMargins = AValue then Exit;
  FMargins := AValue;
end;

procedure TGridLayoutComponent.SetRowHeight(AValue: Integer);
begin
  if FRowHeight = AValue then Exit;
  FRowHeight := AValue;

  if AutoArrange then
    UpdateGrid;
end;

procedure TGridLayoutComponent.SetRows(AValue: Integer);
begin
  if FRows = AValue then Exit;
  FRows := AValue;
  if AutoArrange then
    UpdateGrid;
end;

procedure TGridLayoutComponent.SetGridTop(AValue: Integer);
begin
  if FGridTop = AValue then Exit;
  FGridTop := AValue;
  if AutoArrange then
    UpdateGrid;
end;

procedure TGridLayoutComponent.MarginsChanged(Sender: TObject);
begin
  if AutoArrange then
    UpdateGrid;
end;

procedure TGridLayoutComponent.SetRowTracks(AValue: TGridTrackCollection);
begin
  FRowTracks.Assign(AValue);
end;

procedure TGridLayoutComponent.SetVerticalSpacing(AValue: Integer);
begin
  if FVerticalSpacing = AValue then Exit;
  FVerticalSpacing := AValue;
  if AutoArrange then
    UpdateGrid;
end;

procedure TGridLayoutComponent.PopulateGrid;
var
  ControItem: TControlItem;
  Filler: IGridFill;
  I: Integer;

  procedure ConfigFiller;
  const
    FillerTypeClasses: array[TGridFillerType] of TFillerType = (
      ftRowFirst,     // manual
      ftRowFirst,
      ftColumnFirst
    );
  begin
    Filler := nil;

    if FFillerType <> gftManual then
    begin
      Filler := TGridLayoutFillerFactory.Create
        .CreateFiller(
          FillerTypeClasses[FFillerType],
          FGridLayout
        );
    end;
  end;

  procedure AddItem(Item: IGridItem; Settings: TGridCellSettings);
  begin
    if FFillerType = gftManual then
      FGridLayout.AddItem(Item, Settings)
    else
      Filler.PlaceItem(Item, Settings);
  end;

begin
  FGridLayout.Clear;

  ConfigFiller;

  for I:=0 to FControls.Count-1 do
  begin
    ControItem := TControlItem(FControls.Items[I]);

    if Assigned(Filler) and (not Assigned(ControItem.Control)) then
    begin
      Filler.Skip;
      Continue;
    end;

    AddItem(
      TControlGridItem.Create(ControItem.Control),
      TGridCellSettings.Create(ControItem.Row, ControItem.Column)
        .WithColumnSpan(ControItem.ColSpan)
        .WithRowSpan(ControItem.RowSpan)
        .WithAlignment(
          ControItem.HorizontalAlignment,
          ControItem.VerticalAlignment
        )
    );
  end;
end;

procedure TGridLayoutComponent.UpdateGrid;
var
  RowTrack: TGridTrackItem;
  ColTrack: TGridTrackItem;
  I: Integer;
begin
  FGridLayout.Rows := Rows;
  FGridLayout.Columns := Columns;
  FGridLayout.RowHeights := RowHeight;
  FGridLayout.ColumnWidths := ColumnWidth;
  FGridLayout.Margins.Top := FMargins.Top;
  FGridLayout.Margins.Right := FMargins.Right;
  FGridLayout.Margins.Bottom := FMargins.Bottom;
  FGridLayout.Margins.Left := FMargins.Left;
  FGridLayout.VerticalSpacings := VerticalSpacing;
  FGridLayout.HorizontalSpacings := FHorizontalSpacing;

  PopulateGrid;

  FGridLayout.ResetRowHeightsToDefault;
  FGridLayout.ResetRowShift;
  FGridLayout.ResetVerticalSpacing;
  FGridLayout.ResetColumnWidthsToDefault;
  FGridLayout.ResetColumnShift;
  FGridLayout.ResetHorizontalSpacing;

  for I:=0 to FRowTracks.Count-1 do
  begin
    RowTrack := TGridTrackItem(FRowTracks.Items[I]);
    if RowTrack.EnableSize then
      FGridLayout.RowHeight[RowTrack.Index] := RowTrack.Size;
    if RowTrack.EnableShift then
      FGridLayout.RowShift[RowTrack.Index] := RowTrack.Shift;
    if RowTrack.EnableSpacing then
      FGridLayout.VerticalSpacing[RowTrack.Index] := RowTrack.Spacing;
    FGridLayout.VisibleRow[RowTrack.Index] := not RowTrack.Hidden;
  end;

  for I:=0 to FColTracks.Count-1 do
  begin
    ColTrack := TGridTrackItem(FColTracks.Items[I]);
    if ColTrack.EnableSize then
      FGridLayout.ColumnWidth[ColTrack.Index] := ColTrack.Size;
    if ColTrack.EnableShift then
      FGridLayout.ColumnShift[ColTrack.Index] := ColTrack.Shift;
    if ColTrack.EnableSpacing then
      FGridLayout.HorizontalSpacing[ColTrack.Index] := ColTrack.Spacing;
    FGridLayout.VisibleColumn[ColTrack.Index] := not ColTrack.Hidden;
  end;

  FGridLayout.ArrangeItems(GridLeft, GridTop);
end;

constructor TGridLayoutComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRows := 3;
  FColumns := 3;
  FRowHeight := 35;
  FColumnWidth := 150;
  FVerticalSpacing := 5;
  FHorizontalSpacing := 5;
  FGridLayout := TGridLayout.Create;
  FControls := TControlCollection.Create(Self, TControlItem);
  FMargins := TMargins.Create;
  FMargins.OnChange := @MarginsChanged;
  FColTracks := TGridTrackCollection.Create(Self, TGridTrackItem);
  FRowTracks := TGridTrackCollection.Create(Self, TGridTrackItem);
end;

destructor TGridLayoutComponent.Destroy;
begin
  FColTracks.Free;
  FRowTracks.Free;
  FMargins.Free;
  FControls.Free;
  FGridLayout.Free;
  inherited Destroy;
end;

procedure TGridLayoutComponent.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  I: Integer;
begin
  if Operation = opRemove then
    for I := 0 to FControls.Count - 1 do
      if (FControls.Items[I] is TControlItem)
         and (TControlItem(FControls.Items[I]).Control = AComponent) then
        TControlItem(FControls.Items[I]).Control := nil;
end;

end.
