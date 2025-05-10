unit UGridLayoutFill;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ULayout, Controls;

type
  TNextPositionFunc = function : IGridPosition of object;

  { TGridFillBase }

  TGridFillBase = class(TInterfacedObject, IGridFill)
  protected
    FGrid: TGridLayout;
    function GetOnAfterPlaceItem: TGridFillAfterPlaceEvent; virtual; abstract;
    function GetOnBeforePlaceItem: TGridFillBeforePlaceEvent; virtual; abstract;
    function GetPosition: IGridPosition; virtual; abstract;
    procedure SetOnAfterPlaceItem(AValue: TGridFillAfterPlaceEvent); virtual; abstract;
    procedure SetOnBeforePlaceItem(AValue: TGridFillBeforePlaceEvent); virtual; abstract;
    function GetGrid: TGridLayout; virtual; abstract;
    procedure PlaceItem(AItem: ILayoutItem); overload; virtual; abstract;
    procedure PlaceItem(AItem: TControl); overload; virtual; abstract;
    procedure Skip(ACount: Integer = 1); virtual; abstract;
    procedure InitialPos(APos: IGridPosition); virtual; abstract;
    function NextPosition: IGridPosition; virtual; abstract;
    function SafeFindNextPosition(AFunc: TNextPositionFunc): IGridPosition;
  public
    constructor Create(AGrid: TGridLayout); virtual; abstract;
  end;

  { TGridFillRowFirst }

  TGridFillRowFirst = class(TGridFillBase)
  private
    function SafeNextPosition: IGridPosition;
  protected
    FOnAfterPlaceItem: TGridFillAfterPlaceEvent;
    FOnBeforePlaceItem: TGridFillBeforePlaceEvent;
    FPosition: IGridPosition;
    function GetPosition: IGridPosition; override;
    function GetGrid: TGridLayout; override;
    function GetOnAfterPlaceItem: TGridFillAfterPlaceEvent; override;
    function GetOnBeforePlaceItem: TGridFillBeforePlaceEvent; override;
    procedure SetOnAfterPlaceItem(AValue: TGridFillAfterPlaceEvent); override;
    procedure SetOnBeforePlaceItem(AValue: TGridFillBeforePlaceEvent); override;
  public
    constructor Create(AGrid: TGridLayout); override; overload;
    function NextPosition: IGridPosition; override;
    procedure PlaceItem(AItem: ILayoutItem); override; overload;
    procedure PlaceItem(AItem: TControl); override; overload;
    procedure Skip(ACount: Integer=1); override;
    procedure InitialPos(APos: IGridPosition); override;
    property Grid: TGridLayout read FGrid;
    property OnBeforePlaceItem: TGridFillBeforePlaceEvent read GetOnBeforePlaceItem write SetOnBeforePlaceItem;
    property OnAfterPlaceItem: TGridFillAfterPlaceEvent read GetOnAfterPlaceItem write SetOnAfterPlaceItem;
    property Position: IGridPosition read GetPosition;
  end;

  { TGridFillColumnFirst }

  TGridFillColumnFirst = class(TGridFillBase)
  private
    function SafeNextPosition: IGridPosition;
  protected
    FOnAfterPlaceItem: TGridFillAfterPlaceEvent;
    FOnBeforePlaceItem: TGridFillBeforePlaceEvent;
    FPosition: IGridPosition;
    function GetPosition: IGridPosition; override;
    function GetGrid: TGridLayout; override;
    function GetOnAfterPlaceItem: TGridFillAfterPlaceEvent; override;
    function GetOnBeforePlaceItem: TGridFillBeforePlaceEvent; override;
    procedure SetOnAfterPlaceItem(AValue: TGridFillAfterPlaceEvent); override;
    procedure SetOnBeforePlaceItem(AValue: TGridFillBeforePlaceEvent); override;
  public
    constructor Create(AGrid: TGridLayout); override;
    function NextPosition: IGridPosition; override;
    procedure PlaceItem(AItem: ILayoutItem); override;
    procedure PlaceItem(AItem: TControl); override;
    procedure Skip(ACount: Integer=1); override;
    procedure InitialPos(APos: IGridPosition); override;
    property Grid: TGridLayout read FGrid;
    property OnBeforePlaceItem: TGridFillBeforePlaceEvent read GetOnBeforePlaceItem write SetOnBeforePlaceItem;
    property OnAfterPlaceItem: TGridFillAfterPlaceEvent read GetOnAfterPlaceItem write SetOnAfterPlaceItem;
    property Position: IGridPosition read GetPosition;
  end;

implementation

{ TGridFillBase }

function TGridFillBase.SafeFindNextPosition(AFunc: TNextPositionFunc
  ): IGridPosition;
var
  MaxCols, MaxRows, CheckedCells: Integer;
  R, C: Integer;
begin
  CheckedCells := 0;
  MaxCols := FGrid.Columns;
  MaxRows := FGrid.Rows;

  repeat
    Inc(CheckedCells);
    if CheckedCells > (MaxCols * MaxRows) then
      raise Exception.Create('Grid is full. No available position.');

    Result := AFunc();
    R := Result.Row;
    C := Result.Column;

  until not FGrid.IsCellOccupied(R, C);
end;

{ TGridFillRowFirst }

function TGridFillRowFirst.GetPosition: IGridPosition;
begin
  Result := FPosition;
end;

function TGridFillRowFirst.GetGrid: TGridLayout;
begin
  Result := FGrid;
end;

function TGridFillRowFirst.GetOnAfterPlaceItem: TGridFillAfterPlaceEvent;
begin
  Result := FOnAfterPlaceItem;
end;

function TGridFillRowFirst.GetOnBeforePlaceItem: TGridFillBeforePlaceEvent;
begin
  Result := FOnBeforePlaceItem;
end;

function TGridFillRowFirst.SafeNextPosition: IGridPosition;
begin
  Result := TGridPosition.Create(FPosition.Row, FPosition.Column);
  FPosition.Column := FPosition.Column + 1;
  if FPosition.Column >= Grid.Columns then
  begin
    FPosition.Column := 0;
    FPosition.Row := FPosition.Row + 1;
    if FPosition.Row >= Grid.Rows then
      FPosition.Row := 0;
  end;
end;

function TGridFillRowFirst.NextPosition: IGridPosition;
begin
  Result := SafeFindNextPosition(@SafeNextPosition);
end;

procedure TGridFillRowFirst.SetOnAfterPlaceItem(AValue: TGridFillAfterPlaceEvent
  );
begin
  if FOnAfterPlaceItem = AValue then
    Exit;
  FOnAfterPlaceItem := AValue;
end;

procedure TGridFillRowFirst.SetOnBeforePlaceItem(
  AValue: TGridFillBeforePlaceEvent);
begin
  if FOnBeforePlaceItem = AValue then
    Exit;
  FOnBeforePlaceItem := AValue;
end;

constructor TGridFillRowFirst.Create(AGrid: TGridLayout);
begin
  FPosition := TGridPosition.Create(0, 0);
  FGrid := AGrid;
end;

procedure TGridFillRowFirst.PlaceItem(AItem: ILayoutItem);
var
  Pos: IGridPosition;
  Settings: TGridCellSettings;
  Accept: Boolean;
begin
  if not Assigned(Grid) then
    raise Exception.Create('Grid not assigned.');

  Pos := NextPosition;
  Accept := True;
  Settings := TGridCellSettings.Create(Pos.Row, Pos.Column);

  if Assigned(FOnBeforePlaceItem) then
    FOnBeforePlaceItem(
      Self, Self.Grid, AItem, Pos,
      Settings, Accept
    );

  if not Accept then
    Exit;

  Grid.AddItem(AItem, Settings);

  if Assigned(FOnAfterPlaceItem) then
    FOnAfterPlaceItem(Self, Self.Grid, AItem, Pos);
end;

procedure TGridFillRowFirst.PlaceItem(AItem: TControl);
begin
  Self.PlaceItem(TControlLayoutItem.Create(AItem));
end;

procedure TGridFillRowFirst.Skip(ACount: Integer);
var
  I: Integer;
begin
  for I := 1 to ACount do
    NextPosition;
end;

procedure TGridFillRowFirst.InitialPos(APos: IGridPosition);
begin
  FPosition := APos;
end;

{ TGridFillColumnFirst }

function TGridFillColumnFirst.GetPosition: IGridPosition;
begin
  Result := FPosition;
end;

function TGridFillColumnFirst.GetGrid: TGridLayout;
begin
  Result := FGrid;
end;

function TGridFillColumnFirst.GetOnAfterPlaceItem: TGridFillAfterPlaceEvent;
begin
  Result := FOnAfterPlaceItem;
end;

function TGridFillColumnFirst.GetOnBeforePlaceItem: TGridFillBeforePlaceEvent;
begin
  Result := FOnBeforePlaceItem;
end;

procedure TGridFillColumnFirst.SetOnAfterPlaceItem(AValue: TGridFillAfterPlaceEvent);
begin
  if FOnAfterPlaceItem = AValue then Exit;
  FOnAfterPlaceItem := AValue;
end;

procedure TGridFillColumnFirst.SetOnBeforePlaceItem(AValue: TGridFillBeforePlaceEvent);
begin
  if FOnBeforePlaceItem = AValue then Exit;
  FOnBeforePlaceItem := AValue;
end;

constructor TGridFillColumnFirst.Create(AGrid: TGridLayout);
begin
  FPosition := TGridPosition.Create(0, 0);
  FGrid := AGrid;
end;

function TGridFillColumnFirst.SafeNextPosition: IGridPosition;
begin
  Result := TGridPosition.Create(FPosition.Row, FPosition.Column);
  FPosition.Row := FPosition.Row + 1;
  if FPosition.Row >= Grid.Rows then
  begin
    FPosition.Row := 0;
    FPosition.Column := FPosition.Column + 1;
    if FPosition.Column >= Grid.Columns then
      FPosition.Column := 0;
  end;
end;

function TGridFillColumnFirst.NextPosition: IGridPosition;
begin
  Result := SafeFindNextPosition(@SafeNextPosition);
end;

procedure TGridFillColumnFirst.PlaceItem(AItem: ILayoutItem);
var
  Pos: IGridPosition;
  Settings: TGridCellSettings;
  Accept: Boolean;
begin
  if not Assigned(Grid) then
    raise Exception.Create('Grid not assigned.');

  Pos := NextPosition;
  Accept := True;
  Settings := TGridCellSettings.Create(Pos.Row, Pos.Column);

  if Assigned(FOnBeforePlaceItem) then
    FOnBeforePlaceItem(Self, Grid, AItem, Pos, Settings, Accept);

  if not Accept then
    Exit;

  Grid.AddItem(AItem, Settings);

  if Assigned(FOnAfterPlaceItem) then
    FOnAfterPlaceItem(Self, Grid, AItem, Pos);
end;

procedure TGridFillColumnFirst.PlaceItem(AItem: TControl);
begin
  Self.PlaceItem(TControlLayoutItem.Create(AItem));
end;

procedure TGridFillColumnFirst.Skip(ACount: Integer);
var
  I: Integer;
begin
  for I := 1 to ACount do
    NextPosition;
end;

procedure TGridFillColumnFirst.InitialPos(APos: IGridPosition);
begin
  FPosition := APos;
end;

end.

