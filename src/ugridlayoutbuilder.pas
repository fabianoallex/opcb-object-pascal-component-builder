unit UGridLayoutBuilder;

{$mode Objfpc}{$H+}

interface

uses
  ULayout, Controls, UGridLayoutFillerFactory;

type
  TGridFillClass = class of TInterfacedObject;
  TItemBuilderProc = function(AGridPos: IGridPosition; AOwner: TWinControl): ILayoutItem of object;

  { TGridLayoutBuilder }

  TGridLayoutBuilder = class
  private
    FParentBuilder: TGridLayoutBuilder;
    FSettingsToApply: TGridCellSettings;

    FCurrentItem: ILayoutItem;
    FGridLayout: TGridLayout;
    FFiller: IGridFill;
    FFreeOnDone: Boolean;
  public
    constructor Create; overload;
    constructor Create(AGridLayout: TGridLayout); overload;
    destructor Destroy; override;
    function EnableAutoFreeOnDone: TGridLayoutBuilder;
    function DisableAutoFreeOnDone: TGridLayoutBuilder;
    function WithDimensions(ARows, AColumns: Integer): TGridLayoutBuilder;
    function WithColumnsWidth(AWidth: Integer): TGridLayoutBuilder;
    function WithRowsHeight(AHight: Integer): TGridLayoutBuilder; overload;
    function WithRowsHeight(ARowsIndex: array of Integer; AHeight: Integer): TGridLayoutBuilder; overload;
    function WithRowAndColumnSizes(AHight, AWidth: Integer): TGridLayoutBuilder;
    function WithHorizontalSpacings(ASpacing: Integer): TGridLayoutBuilder;
    function WithVerticalSpacings(ASpacing: Integer): TGridLayoutBuilder;
    function WithSpacings(ASpacing: Integer): TGridLayoutBuilder; overload;
    function WithSpacings(AHSpacing, AVSpacing: Integer): TGridLayoutBuilder; overload;
    function WithMargins(ATop, ARight, ABottom, ALeft: Integer): TGridLayoutBuilder; overload;
    function WithMargins(AAll: Integer): TGridLayoutBuilder; overload;
    function AddItem(AItem: ILayoutItem; ASettings: TGridCellSettings): TGridLayoutBuilder; overload;
    function AddItem(AItem: TControl; ASettings: TGridCellSettings): TGridLayoutBuilder; overload;
    function UsingFiller(AFillerType: TFillerType): TGridLayoutBuilder; overload;
    function AddItem(AItem: ILayoutItem): TGridLayoutBuilder; overload;
    function FillItems(AControls: array of TControl;
      AInitialPosition: IGridPosition=nil): TGridLayoutBuilder;

    function BeginSubGrid(ASettings: TGridCellSettings): TGridLayoutBuilder;
    function EndSubGrid: TGridLayoutBuilder;

    function Done: TGridLayout;
  end;


implementation

uses
  Classes, SysUtils, ExtCtrls;

{ TGridLayoutBuilder }

constructor TGridLayoutBuilder.Create;
var
  AGridLayout: TGridLayout;
begin
  inherited Create;
  FFreeOnDone := True;
  if not Assigned(AGridLayout) then
    AGridLayout := TGridLayout.Create;
  FGridLayout := AGridLayout;

  //Create(TGridLayout.Create);
end;

constructor TGridLayoutBuilder.Create(AGridLayout: TGridLayout);
begin
  inherited Create;
  FFreeOnDone := True;
  if not Assigned(AGridLayout) then
    AGridLayout := TGridLayout.Create;
  FGridLayout := AGridLayout;
end;

destructor TGridLayoutBuilder.Destroy;
begin
  inherited Destroy;
end;

function TGridLayoutBuilder.EnableAutoFreeOnDone: TGridLayoutBuilder;
begin
  Result := Self;
  FFreeOnDone := True;
end;

function TGridLayoutBuilder.DisableAutoFreeOnDone: TGridLayoutBuilder;
begin
  Result := Self;
  FFreeOnDone := False;
end;

function TGridLayoutBuilder.WithDimensions(ARows, AColumns: Integer
  ): TGridLayoutBuilder;
begin
  FGridLayout.Rows := ARows;
  FGridLayout.Columns := AColumns;
  Result := Self;
end;

function TGridLayoutBuilder.WithColumnsWidth(AWidth: Integer
  ): TGridLayoutBuilder;
begin
  FGridLayout.ColumnWidths := AWidth;
  Result := Self;
end;

function TGridLayoutBuilder.WithRowsHeight(AHight: Integer
  ): TGridLayoutBuilder;
begin
  Result := Self;
  FGridLayout.RowHeights := AHight;
end;

function TGridLayoutBuilder.WithRowsHeight(ARowsIndex: array of Integer;
  AHeight: Integer): TGridLayoutBuilder;
var
  I: Integer;
begin
  Result := Self;
  for I := Low(ARowsIndex) to High(ARowsIndex) do
    FGridLayout.RowHeight[ARowsIndex[I]] := AHeight;
end;

function TGridLayoutBuilder.WithRowAndColumnSizes(AHight, AWidth: Integer
  ): TGridLayoutBuilder;
begin
  FGridLayout.RowHeights := AHight;
  FGridLayout.ColumnWidths := AWidth;
  Result := Self;
end;

function TGridLayoutBuilder.WithHorizontalSpacings(ASpacing: Integer
  ): TGridLayoutBuilder;
begin
  FGridLayout.HorizontalSpacings := ASpacing;
  Result := Self;
end;

function TGridLayoutBuilder.WithVerticalSpacings(ASpacing: Integer
  ): TGridLayoutBuilder;
begin
  FGridLayout.VerticalSpacings := ASpacing;
  Result := Self;
end;

function TGridLayoutBuilder.WithSpacings(ASpacing: Integer): TGridLayoutBuilder;
begin
  FGridLayout.VerticalSpacings := ASpacing;
  FGridLayout.HorizontalSpacings := ASpacing;
  Result := Self;
end;

function TGridLayoutBuilder.WithSpacings(AHSpacing, AVSpacing: Integer): TGridLayoutBuilder;
begin
  FGridLayout.HorizontalSpacings := AHSpacing;
  FGridLayout.VerticalSpacings := AVSpacing;
  Result := Self;
end;

function TGridLayoutBuilder.WithMargins(ATop, ARight, ABottom, ALeft: Integer
  ): TGridLayoutBuilder;
begin
  FGridLayout.Margins.Top := ATop;
  FGridLayout.Margins.Right := ARight;
  FGridLayout.Margins.Bottom := ABottom;
  FGridLayout.Margins.Left := ALeft;
  Result := Self;
end;

function TGridLayoutBuilder.WithMargins(AAll: Integer): TGridLayoutBuilder;
begin
  FGridLayout.Margins.All := AAll;
  Result := Self;
end;

function TGridLayoutBuilder.AddItem(AItem: ILayoutItem;
  ASettings: TGridCellSettings): TGridLayoutBuilder;
begin
  FCurrentItem := AItem;
  FGridLayout.AddItem(FCurrentItem, ASettings);
  Result := Self;
end;

function TGridLayoutBuilder.AddItem(AItem: TControl;
  ASettings: TGridCellSettings): TGridLayoutBuilder;
begin
  FCurrentItem := TControlLayoutItem.Create(AItem);
  FGridLayout.AddItem(FCurrentItem, ASettings);
  Result := Self;
end;

function TGridLayoutBuilder.UsingFiller(AFillerType: TFillerType): TGridLayoutBuilder;
begin
  Result := Self;
  FFiller := TGridLayoutFillerFactory.CreateFiller(AFillerType, FGridLayout);
end;

function TGridLayoutBuilder.AddItem(AItem: ILayoutItem): TGridLayoutBuilder;
begin
  Result := Self;
  if Assigned(FFiller) then
    FFiller.PlaceItem(AItem);
end;

function TGridLayoutBuilder.FillItems(AControls: array of TControl;
  AInitialPosition: IGridPosition): TGridLayoutBuilder;
var
  I: Integer;
  Control: TControl;
begin
  Result := Self;

  if Assigned(AInitialPosition) then
    FFiller.InitialPos(AInitialPosition);

  for I := Low(AControls) to High(AControls) do
  begin
    Control := AControls[I];

    if Assigned(Control) then
      FFiller.PlaceItem(Control)
    else
      FFiller.Skip;
  end;
end;

function TGridLayoutBuilder.BeginSubGrid(ASettings: TGridCellSettings
  ): TGridLayoutBuilder;
var
  SubBuilder: TGridLayoutBuilder;
begin
  SubBuilder := TGridLayoutBuilder.Create;
  SubBuilder.FParentBuilder := Self;
  SubBuilder.FSettingsToApply := ASettings;
  Result := SubBuilder;
end;

function TGridLayoutBuilder.EndSubGrid: TGridLayoutBuilder;
var
  SubGridItem: TSubGridLayoutItem;
begin
  Result := FParentBuilder;
  SubGridItem := TSubGridLayoutItem.Create(FGridLayout);
  Result.AddItem(SubGridItem, FSettingsToApply);
  SubGridItem.Container.Height := FGridLayout.ContentHeight;
  SubGridItem.Container.Width := FGridLayout.ContentWidth;

  Self.Done;  //auto destroy. sempre chamar no final
end;

function TGridLayoutBuilder.Done: TGridLayout;
begin
  Result := FGridLayout;
  if FFreeOnDone then
    Free;
end;



end.

