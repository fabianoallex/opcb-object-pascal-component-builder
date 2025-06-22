unit UGridLayoutBuilder;

{$IFDEF FPC}
{$mode Objfpc}{$H+}
{$ENDIF}

interface

uses
  ULayout, UGridLayoutFillerFactory;

type
  TGridFillClass = class of TInterfacedObject;

  { TGridLayoutBuilder }

  TGridLayoutBuilder = class
  private
    FCurrentItem: IGridItem;
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
    function WithTopLeft(ATop, ALeft: Integer): TGridLayoutBuilder;
    function WithColumnsWidth(AWidth: Integer): TGridLayoutBuilder; overload;
    function WithColumnsWidth(AColsIndex: array of Integer; AWidth: Integer): TGridLayoutBuilder; overload;
    function WithRowAndColSizes(AHeight, AWidth: Integer): TGridLayoutBuilder;
    function WithRowsHeight(AHeight: Integer): TGridLayoutBuilder; overload;
    function WithRowsHeight(ARowsIndex: array of Integer; AHeight: Integer): TGridLayoutBuilder; overload;
    function WithRowAndColumnSizes(AHight, AWidth: Integer): TGridLayoutBuilder;
    function WithHorizontalSpacings(ASpacing: Integer): TGridLayoutBuilder;
    function WithVerticalSpacings(ASpacing: Integer): TGridLayoutBuilder;
    function WithSpacings(ASpacing: Integer): TGridLayoutBuilder; overload;
    function WithSpacings(AHSpacing, AVSpacing: Integer): TGridLayoutBuilder; overload;
    function WithMargins(ATop, ARight, ABottom, ALeft: Integer): TGridLayoutBuilder; overload;
    function WithMargins(AAll: Integer): TGridLayoutBuilder; overload;
    function AddItem(AItem: IGridItem; ASettings: TGridCellSettings): TGridLayoutBuilder; overload;
    function UsingFiller(AFillerType: TFillerType): TGridLayoutBuilder; overload;
    function AddItem(AItem: IGridItem): TGridLayoutBuilder; overload;
    function Build: TGridLayout;
    property Filler: IGridFill read FFiller;
    property GridLayout: TGridLayout read FGridLayout;
  end;

implementation

uses
  Classes, SysUtils, UGridItemFactory;

{ TGridLayoutBuilder }

constructor TGridLayoutBuilder.Create;
begin
  inherited Create;
  Create(TGridLayout.Create);
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

function TGridLayoutBuilder.WithTopLeft(ATop, ALeft: Integer
  ): TGridLayoutBuilder;
begin
  Result := Self;
  FGridLayout.Top := ATop;
  FGridLayout.Left := ALeft;
end;

function TGridLayoutBuilder.WithColumnsWidth(AWidth: Integer
  ): TGridLayoutBuilder;
begin
  Result := Self;
  FGridLayout.ColumnWidths := AWidth;
end;

function TGridLayoutBuilder.WithColumnsWidth(AColsIndex: array of Integer;
  AWidth: Integer): TGridLayoutBuilder;
var
  I: Integer;
begin
  Result := Self;
  for I := Low(AColsIndex) to High(AColsIndex) do
    FGridLayout.ColumnWidth[AColsIndex[I]] := AWidth;
end;

function TGridLayoutBuilder.WithRowAndColSizes(AHeight, AWidth: Integer
  ): TGridLayoutBuilder;
begin
  Result := Self;
  FGridLayout.RowHeights := AHeight;
  FGridLayout.ColumnWidths := AWidth;
end;

function TGridLayoutBuilder.WithRowsHeight(AHeight: Integer
  ): TGridLayoutBuilder;
begin
  Result := Self;
  FGridLayout.RowHeights := AHeight;
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

function TGridLayoutBuilder.AddItem(AItem: IGridItem;
  ASettings: TGridCellSettings): TGridLayoutBuilder;
begin
  FCurrentItem := AItem;
  FGridLayout.AddItem(FCurrentItem, ASettings);
  Result := Self;
end;

function TGridLayoutBuilder.UsingFiller(AFillerType: TFillerType): TGridLayoutBuilder;
begin
  Result := Self;
  FFiller := TGridLayoutFillerFactory.CreateFiller(AFillerType, FGridLayout);
end;

function TGridLayoutBuilder.AddItem(AItem: IGridItem): TGridLayoutBuilder;
begin
  Result := Self;
  if Assigned(FFiller) then
    FFiller.PlaceItem(AItem);
end;

function TGridLayoutBuilder.Build: TGridLayout;
begin
  Result := FGridLayout;
  if FFreeOnDone then
    Free;
end;

end.

