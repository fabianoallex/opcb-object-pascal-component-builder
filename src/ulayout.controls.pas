unit ulayout.controls;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$LONGSTRINGS ON}{$MODESWITCH TYPEHELPERS}{$MODESWITCH ADVANCEDRECORDS}
{$ENDIF}

interface

uses
  {$IFDEF FPC}Controls, StdCtrls,
  {$ELSE}
    {$IFDEF FRAMEWORK_FMX}
    FMX.Controls, FMX.StdCtrls, Fmx.Types,
    {$ELSE}
    Vcl.Controls, Vcl.StdCtrls,
    {$ENDIF}
  {$ENDIF}
  Classes, SysUtils, ULayout, UGridLayoutBuilder,
  UGridLayoutFillerFactory, Generics.Collections, Generics.Defaults;

type
  {$IFNDEF FPC}
    {$IFDEF FRAMEWORK_FMX}
    TWinControl = TFmxObject;
    {$ENDIF}
  {$ENDIF}

  TControlClass = class of TControl;
  TControlPopulateProc = procedure(AControl: TControl; AIndex: Integer;
    ASettings: TGridCellSettings) of object;

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
    function GetLeft: Integer;
    function GetTop: Integer;
    function IsControlOfType(AClass: TClass): Boolean;
    function GetControl: TControl;
    procedure Redraw;
  end;

  { TControlGridItem }

  TControlGridItem = class(TInterfacedObject, IGridItem)
  protected
    FControlElement: IVisualElement;
    procedure AfterSetBounds; virtual;
  public
    constructor Create(AControl: TControl);
    function GetVisualElement: IVisualElement;
    function GetRenderer: IGridItemRenderer;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
  end;

  { TControlGridItemRenderer }

  TControlGridItemRenderer = class(TInterfacedObject, IGridItemRenderer)
  private
    FGridItem: IGridItem;
  public
    constructor Create(AGridItem: TControlGridItem);
    procedure Render;
  end;

  { TControlInfo }

  TControlInfo = record
    ControlClass: TControlClass;
    ControlName: string;

    class function Create(AClass: TControlClass; const AName: string): TControlInfo; static;
  end;

  TStrControlDictionary = {$IFDEF FPC}specialize{$ENDIF} TDictionary<string, TControl>;

  { TControlGridPopulator }

  TControlGridPopulator = class
  private
    FGrid: TGridLayout;
    FFiller: IGridFill;
    FFillerType: TFillerType;
    FOwner: TComponent;
    FParent: TWinControl;
    FControls: {$IFDEF FPC}specialize{$ENDIF} TList<TControl>;
    FNamedControls: TStrControlDictionary;
    FOnControlPopulate: TControlPopulateProc;
    function GetNamedControl(const AName: string): TControl;
    procedure SetControls(AValue: {$IFDEF FPC}specialize{$ENDIF} TList<TControl>);
    procedure ConfigControl(AControl: TControl);
  public
    constructor Create;
    destructor Destroy; override;
    function WithOwnerAndParentControl(AOwner: TComponent; AParent: TWinControl
      ): TControlGridPopulator;
    procedure SetGrid(AGrid: TGridLayout);
    function UsingFiller(AFillerType: TFillerType; ARow: Integer=0;
      AColumn: Integer=0): TControlGridPopulator;
    function FillerSkip(ACount: Integer=1): TControlGridPopulator;
    function FillerSetPosition(ARow, AColumn: Integer): TControlGridPopulator;

    function AddControl(AControl: TControl;
      AProc: TControlPopulateProc=nil): TControlGridPopulator;

    function AddControls(AControls: array of TControl;
      AProc: TControlPopulateProc=nil): TControlGridPopulator; overload;

    function CreateControl(AControlInfo: TControlInfo;
      AProc: TControlPopulateProc=nil): TControlGridPopulator; overload;

    function CreateControl(AControlClass: TControlClass;
      AProc: TControlPopulateProc=nil): TControlGridPopulator; overload;

    function CretaeControls(ACount: Integer; AControlClass: TControlClass;
      AProc: TControlPopulateProc=nil): TControlGridPopulator; overload;

    function CretaeControls(ACount: Integer; AControlInfo: TControlInfo;
      AProc: TControlPopulateProc=nil): TControlGridPopulator; overload;

    function CreateControls(ACount: Integer;
      AControlClasses: array of TControlClass;
      AProc: TControlPopulateProc=nil): TControlGridPopulator; overload;

    function CreateControls(ACount: Integer;
      AControlCreateInfos: array of TControlInfo;
      AProc: TControlPopulateProc=nil): TControlGridPopulator; overload;

    function CreateControls(AControlCreateInfos: array of TControlInfo;
      AProc: TControlPopulateProc=nil): TControlGridPopulator; overload;

    function OnControlCreate(AProc: TControlPopulateProc): TControlGridPopulator;
    property Controls: {$IFDEF FPC}specialize{$ENDIF} TList<TControl> read FControls write SetControls;
    property NamedControls[const AName: string]: TControl read GetNamedControl;
    property Grid: TGridLayout read FGrid;
  end;

  { TGridLayoutHelper }

  TGridLayoutHelper = class helper for TGridLayout
  public
    function GetControl(ARow, ACol: Integer): TControl;
    procedure AddItem(AItem: TControl; ASettings: TGridCellSettings); overload;
  end;

  { TGridLayoutBuilderHelper }

  TGridLayoutBuilderHelper = class helper for TGridLayoutBuilder
  public
    function AddItem(AItem: TControl; ASettings: TGridCellSettings):
      TGridLayoutBuilder; overload;
    function FillItems(AControls: array of TControl;
      AInitialPosition: IGridPosition=nil): TGridLayoutBuilder;
    function BuildAndPopulate(var AGrid: TGridLayout;
      APopulator: TControlGridPopulator): TControlGridPopulator;
    function Build(var AGrid: TGridLayout): TGridLayoutBuilder; overload;
    function UsePopulator(APopulator: TControlGridPopulator): TControlGridPopulator;
  end;

implementation

uses
  {$IFDEF FPC}Graphics
  {$ELSE}
    {$IFDEF FRAMEWORK_FMX} Fmx.Graphics
    {$ELSE} Vcl.Graphics
    {$ENDIF}
  {$ENDIF} ;

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
  {$IFDEF FPC}
  Result := FControl.Width;
  {$ELSE}
    {$IFDEF FRAMEWORK_FMX}
    Result := Trunc(FControl.Width);
    {$ELSE}
    Result := FControl.Width;
    {$ENDIF}
  {$ENDIF}
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
  {$IFDEF FPC}
  Result := FControl.Height;
  {$ELSE}
    {$IFDEF FRAMEWORK_FMX}
    Result := Trunc(FControl.Height);
    {$ELSE}
    Result := FControl.Height;
    {$ENDIF}
  {$ENDIF}
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

function TControlVisualElement.GetLeft: Integer;
begin
  if not Assigned(FControl) then
    Exit;
  {$IFDEF FPC}
  Result := FControl.Left;
  {$ELSE}
    {$IFDEF FRAMEWORK_FMX}
    Result := Trunc(FControl.Position.X);
    {$ELSE}
    Result := FControl.Left;
    {$ENDIF}
  {$ENDIF}
end;

function TControlVisualElement.GetTop: Integer;
begin
  if not Assigned(FControl) then
    Exit;
  {$IFDEF FPC}
    Result := FControl.Top;
  {$ELSE}
    {$IFDEF FRAMEWORK_FMX}
    Result := Trunc(FControl.Position.Y);
    {$ELSE}
    Result := FControl.Top;
    {$ENDIF}
  {$ENDIF}
end;

function TControlVisualElement.IsControlOfType(AClass: TClass): Boolean;
begin
  if not Assigned(FControl) then
    Exit;
  Result := FControl is AClass;
end;

function TControlVisualElement.GetControl: TControl;
begin
  Result := FControl;
end;

procedure TControlVisualElement.Redraw;
begin

end;

{ TControlGridItemRenderer }

constructor TControlGridItemRenderer.Create(AGridItem: TControlGridItem);
begin
  FGridItem := AGridItem;
end;

procedure TControlGridItemRenderer.Render;
begin
  {FGridItem.GetVisualElement.SetBounds(
    AContext.Left,
    AContext.Top,
    AContext.Width,
    AContext.Height
  );}
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

function TControlGridItem.GetVisualElement: IVisualElement;
begin
  Result := FControlElement;
end;

procedure TControlGridItem.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  GetVisualElement.SetBounds(ALeft, ATop, AWidth, AHeight);
  GetRenderer.Render;
end;

function TControlGridItem.GetRenderer: IGridItemRenderer;
begin
  Result := TControlGridItemRenderer.Create(Self);
end;

{ TControlInfo }

class function TControlInfo.Create(AClass: TControlClass;
  const AName: string): TControlInfo;
begin
  Result.ControlClass := AClass;
  Result.ControlName := AName;
end;

{ TControlGridPopulator }

procedure TControlGridPopulator.SetControls(
  AValue: {$IFDEF FPC}specialize{$ENDIF} TList<TControl>);
begin
  if FControls = AValue then Exit;
  FControls := AValue;
end;

function TControlGridPopulator.GetNamedControl(const AName: string): TControl;
begin
  if not FNamedControls.TryGetValue(AName, Result) then
    Result := nil;
end;

function TControlGridPopulator.AddControls(AControls: array of TControl;
  AProc: TControlPopulateProc): TControlGridPopulator;
var
  I: Integer;
begin
  for I := Low(AControls) to High(AControls) do
    AddControl(AControls[I], AProc);
end;

procedure TControlGridPopulator.ConfigControl(AControl: TControl);
begin
  if AControl is TLabel then
    with (AControl as TLabel) do
    begin
      {$IFNDEF FRAMEWORK_FMX}
      Layout := tlCenter;
      {$ENDIF}
      AutoSize := False;
    end;

  if AControl is TCheckBox then
    with (AControl as TCheckBox) do
    begin
      {$IFDEF FPC}
      AutoSize := False;
      {$ENDIF}
    end;

  {$IFNDEF FRAMEWORK_FMX}
  if AControl is TEdit then
    with (AControl as TEdit) do
    begin
      AutoSize := False;
    end;
  {$ENDIF}
end;

constructor TControlGridPopulator.Create;
begin
  FGrid := nil;
  FFiller := nil;
  FControls := {$IFDEF FPC}specialize{$ENDIF} TList<TControl>.Create;
  FNamedControls := TStrControlDictionary.Create;
end;

destructor TControlGridPopulator.Destroy;
begin
  FControls.Free;
  FNamedControls.Free;
  inherited;
end;

function TControlGridPopulator.UsingFiller(AFillerType: TFillerType;
  ARow: Integer; AColumn: Integer): TControlGridPopulator;
begin
  Result := Self;
  FFillerType := AFillerType;
  FFiller := TGridLayoutFillerFactory.CreateFiller(AFillerType, FGrid);
  FillerSetPosition(ARow, AColumn);
end;

function TControlGridPopulator.FillerSkip(ACount: Integer): TControlGridPopulator;
begin
  Result := Self;
  FFiller.Skip(ACount);
end;

function TControlGridPopulator.FillerSetPosition(ARow, AColumn: Integer
  ): TControlGridPopulator;
begin
  Result := Self;
  FFiller.InitialPos(TGridPosition.Create(ARow, AColumn));
end;

function TControlGridPopulator.AddControl(AControl: TControl;
  AProc: TControlPopulateProc): TControlGridPopulator;
var
  ControlGridItem: TControlGridItem;
  Settings: TGridCellSettings;
begin
  FControls.Add(AControl);

  if not string(AControl.Name).IsEmpty then
    FNamedControls.Add(AControl.Name, AControl);

  ControlGridItem := TControlGridItem.Create(AControl);
  Settings := TGridCellSettings.Create(0, 0);

  try
    if Assigned(FOnControlPopulate) then
      FOnControlPopulate(AControl, FControls.Count-1, Settings);
    if Assigned(AProc) then
      AProc(AControl, FControls.Count-1, Settings);

    FFiller.PlaceItem(ControlGridItem, Settings);
  finally
    Settings.Free;
  end;
end;

function TControlGridPopulator.CreateControl(AControlInfo: TControlInfo;
  AProc: TControlPopulateProc): TControlGridPopulator;
var
  Control: TControl;

  function UniqueName(const ABaseName: string): string;
  var
    Index: Integer;
    Candidate: string;
  begin
    if ABaseName.IsEmpty then
      Exit('');

    Candidate := ABaseName;
    Index := 1;

    while FNamedControls.ContainsKey(Candidate) do
    begin
      Candidate := ABaseName + IntToStr(Index);
      Inc(Index);
    end;

    Result := Candidate;
  end;

begin
  Result := Self;

  Control := AControlInfo
    .ControlClass
    .Create(FOwner);

  Control.Parent := FParent;

  if not AControlInfo.ControlName.IsEmpty then
    Control.Name := UniqueName(AControlInfo.ControlName);

  AddControl(Control, AProc);
end;

function TControlGridPopulator.CreateControl(AControlClass: TControlClass;
  AProc: TControlPopulateProc): TControlGridPopulator;
var
  ControlInfo: TControlInfo;
begin
  ControlInfo.ControlClass := AControlClass;
  ControlInfo.ControlName := '';
  Result := CreateControl(ControlInfo, AProc);
end;

function TControlGridPopulator.CretaeControls(ACount: Integer;
  AControlClass: TControlClass; AProc: TControlPopulateProc
  ): TControlGridPopulator;
var
  I: Integer;
begin
  Result := Self;
  for I:=1 to ACount do
    CreateControl(AControlClass, AProc);
end;

function TControlGridPopulator.CretaeControls(ACount: Integer;
  AControlInfo: TControlInfo; AProc: TControlPopulateProc
  ): TControlGridPopulator;
var
  I: Integer;
begin
  Result := Self;
  for I:=1 to ACount do
    CreateControl(AControlInfo, AProc);
end;

function TControlGridPopulator.CreateControls(ACount: Integer;
  AControlClasses: array of TControlClass;
  AProc: TControlPopulateProc): TControlGridPopulator;
var
  I: Integer;
  ControlClass: TControlClass;
begin
  Result := Self;
  for I := 0 to ACount - 1 do
  begin
    ControlClass := AControlClasses[I mod Length(AControlClasses)];
    CreateControl(ControlClass, AProc);
  end;
end;

function TControlGridPopulator.CreateControls(ACount: Integer;
  AControlCreateInfos: array of TControlInfo; AProc: TControlPopulateProc
  ): TControlGridPopulator;
var
  I: Integer;
  ControlCreateInfo: TControlInfo;
begin
  Result := Self;
  for I := 0 to ACount - 1 do
  begin
    ControlCreateInfo := AControlCreateInfos[I mod Length(AControlCreateInfos)];
    CreateControl(ControlCreateInfo, AProc);
  end;
end;

function TControlGridPopulator.CreateControls
  (AControlCreateInfos: array of TControlInfo; AProc: TControlPopulateProc
  ): TControlGridPopulator;
begin
  Result := CreateControls(
    Length(AControlCreateInfos), AControlCreateInfos, AProc);
end;

function TControlGridPopulator.OnControlCreate(AProc: TControlPopulateProc
  ): TControlGridPopulator;
begin
  Result := Self;
  FOnControlPopulate := AProc;
end;

function TControlGridPopulator.WithOwnerAndParentControl
  (AOwner: TComponent; AParent: TWinControl): TControlGridPopulator;
begin
  Result := Self;
  FOwner := AOwner;
  FParent := AParent;
end;

procedure TControlGridPopulator.SetGrid(AGrid: TGridLayout);
begin
  FGrid := AGrid;
  UsingFiller(ftRowFirst);
end;

{ TGridLayoutHelper }

procedure TGridLayoutHelper.AddItem(AItem: TControl;
  ASettings: TGridCellSettings);
begin
  Self.AddItem(TControlGridItem.Create(AItem), ASettings);
end;

function TGridLayoutHelper.GetControl(ARow, ACol: Integer): TControl;
var
  Cell: TGridCell;
begin
  Result := nil;

  Cell := Self.GetCell(ARow, ACol);

  if (not Assigned(Cell))
    or (not Assigned(Cell.Item))
    or (not Assigned(Cell.Item.GetVisualElement)) then
    Exit;

  Result := (Cell.Item.GetVisualElement as TControlVisualElement).GetControl;
end;

{ TGridLayoutBuilderHelper }

function TGridLayoutBuilderHelper.AddItem(AItem: TControl;
  ASettings: TGridCellSettings): TGridLayoutBuilder;
var
  CurrentItem: TControlGridItem;
begin
  CurrentItem := TControlGridItem.Create(AItem);
  Self.AddItem(CurrentItem, ASettings);
  Result := Self;
end;

function TGridLayoutBuilderHelper.Build(
  var AGrid: TGridLayout): TGridLayoutBuilder;
begin
  Result := Self;
  AGrid := Self.Build;
end;

function TGridLayoutBuilderHelper.BuildAndPopulate(var AGrid: TGridLayout;
  APopulator: TControlGridPopulator): TControlGridPopulator;
begin
  AGrid := Self.Build;
  APopulator.SetGrid(AGrid);
  Result := APopulator;
end;

function TGridLayoutBuilderHelper.FillItems(AControls: array of TControl;
  AInitialPosition: IGridPosition): TGridLayoutBuilder;
var
  I: Integer;
  Control: TControl;
begin
  Result := Self;

  if Assigned(AInitialPosition) then
    Filler.InitialPos(AInitialPosition);

  for I := Low(AControls) to High(AControls) do
  begin
    Control := AControls[I];

    if Assigned(Control) then
      Filler.PlaceItem(TControlGridItem.Create(Control))
    else
      Filler.Skip;
  end;
end;

function TGridLayoutBuilderHelper.UsePopulator(
  APopulator: TControlGridPopulator): TControlGridPopulator;
begin
  APopulator.SetGrid(Self.GridLayout);
  Result := APopulator;
end;

end.

