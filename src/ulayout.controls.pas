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

  { TControlGridItem }

  TControlGridItem = class(TInterfacedObject, IGridItem)
  private
    FControl: TControl;
  protected
    procedure AfterSetBounds; virtual;
  public
    constructor Create(AControl: TControl);
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Single);
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
    function GetWidth: Single;
    procedure SetWidth(AValue: Single);
    function GetHeight: Single;
    procedure SetHeight(AValue: Single);
    function GetLeft: Single;
    function GetTop: Single;
    function IsControlOfType(AClass: TClass): Boolean;
    function GetControl: TControl;
    procedure Redraw;
  end;

  TControlSetupProc = procedure(AControl: TControl) of object;

  { TControlInfo }

  TControlInfo = record
    Control: TControl;
    ControlClass: TControlClass;
    SetupProc: TControlSetupProc;
    Name: string;
    Align: TAlign;
    Width: Single;
    Height: Single;
    Top: TOptionalSingle;
    Left: TOptionalSingle;
    function WithSetup(AProc: TControlSetupProc): TControlInfo;
    function WithAlign(AAlign: TAlign): TControlInfo;
    function WithWidth(AWidth: Single): TControlInfo;
    function WithHeight(AHeight: Single): TControlInfo;
    function WithWidthAndHeight(AWidth: Single; AHeight: Single): TControlInfo;
    function WithTop(ATop: Single): TControlInfo;
    function WithLeft(ALeft: Single): TControlInfo;
    class function Create(AClass: TControlClass; const AName: string=''): TControlInfo; overload; static;
    class function Create(AControl: TControl): TControlInfo; overload; static;
  end;

  TStrControlDictionary = {$IFDEF FPC}specialize{$ENDIF} TDictionary<string, TControl>;
  TControlList = {$IFDEF FPC}specialize{$ENDIF} TList<TControl>;
  TControlGroupMap = {$IFDEF FPC}specialize{$ENDIF} TDictionary<string, TControlList>;

  { TControlGridPopulator }

  TControlGridPopulator = class
  private
    FGrid: TGridLayout;
    FFiller: IGridFill;
    FFillerType: TFillerType;
    FOwner: TComponent;
    FParent: TWinControl;
    FControls: TControlList;
    FNamedControls: TStrControlDictionary;
    FOnControlPopulate: TControlPopulateProc;
    function GetNamedControl(const AName: string): TControl;
    procedure SetControls(AValue: TControlList);
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
    property Controls: TControlList read FControls write SetControls;
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

  TControlGroupBounds = record
    Left: Single;
    Top: Single;
    Right: Single;
    Bottom: Single;
    procedure Include(Control: TControl);
    procedure Reset;
    function Width: Single;
    function Height: Single;
  end;

  TControlPopulatorDirection = (cpdHorizontal, cpdVertical);
  TRelativePosition = (rpRight, rpBelow);

  TControlPopulatorLevel = class
  private
    class var FGroupCounter: Integer;
  public
    Direction: TControlPopulatorDirection;
    InitialTop, InitialLeft: Single;
    CurrentTop, CurrentLeft: Single;
    MaxControlHeight: Single;
    GroupName: string;
    constructor Create;
    function Clone: TControlPopulatorLevel;
  end;

  TControlPopulatorLevelStack = TObjectList<TControlPopulatorLevel>;

  TControlPopulator = class
  private
    FOwner: TComponent;
    FParent: TWinControl;
    FControls: TControlList;
    FNamedControls: TStrControlDictionary;
    FGroups: TControlGroupMap;
    FVerticalSpace: Single;
    FHorizontalSpace: Single;
    FLevelStack: TControlPopulatorLevelStack;
    FContentWidth: Single;
    FContentHeight: Single;
    procedure MoveTopLeftAfterControl(AControl: TControl);
    procedure AddControlToGroups(AControl: TControl; const AGroups: array of string);
    function GetGroupBounds(const AGroupName: string): TControlGroupBounds;
    function GetCurrenteLevel: TControlPopulatorLevel;
  public
    constructor Create;
    destructor Destroy; override;
    function GetControlsBounds(AControlsNames: array of string): TControlGroupBounds;
    function SetSpace(AVerticalSpace, AHorizontalSpace: Single): TControlPopulator;
    function BeginLevel: TControlPopulator; overload;
    function BeginLevel(ADirection: TControlPopulatorDirection): TControlPopulator; overload;
    function EndLevel: TControlPopulator;
    function NextLevel: TControlPopulator; overload;
    function NextLevel(ADirection: TControlPopulatorDirection): TControlPopulator; overload;
    function SetVerticalSpace(AVerticalSpace: Single): TControlPopulator;
    function SetHorizontalSpace(AHorizontalSpace: Single): TControlPopulator;
    function SetTopLeft(ATop, ALeft: Single): TControlPopulator;
    function SetTopLeftNearControl(AControlName: string; APosition: TRelativePosition): TControlPopulator;
    function SetTopLeftNearControls(AControlsNames: array of string; APosition: TRelativePosition): TControlPopulator;
    function SetTopLeftNearGroup(const AGroupName: string; APosition: TRelativePosition): TControlPopulator;
    function SetTop(ATop: Single): TControlPopulator; overload;
    function SetLeft(ALeft: Single): TControlPopulator; overload;
    function SetTop(AControlName: string): TControlPopulator; overload;
    function SetLeft(AControlName: string): TControlPopulator; overload;
    function IncTop(AIncTop: Single): TControlPopulator;
    function IncLeft(AIncLeft: Single): TControlPopulator;
    function IncTopLeft(AIncTop, AIncLeft: Single): TControlPopulator;
    function SetDirection(ADirection: TControlPopulatorDirection): TControlPopulator;
    function BreakLine: TControlPopulator; overload;
    function BreakLine(AIncTop: Single): TControlPopulator; overload;
    function WithOwnerAndParent(AOwner: TComponent; AParent: TWinControl): TControlPopulator;
    function WithParent(AParent: TWinControl): TControlPopulator;
    function AddControl(AControlInfo: TControlInfo;
      const AGroups: array of string): TControlPopulator; overload;
    function AddControl(AControlInfo: TControlInfo): TControlPopulator; overload;
    function AddControls(AControlCreateInfos: array of TControlInfo): TControlPopulator; overload;
    function AddControls(AControlCreateInfos: array of TControlInfo;
      const AGroups: array of string): TControlPopulator; overload;
    function AddInLevel(const AControls: array of TControlInfo;
      ADirection: TControlPopulatorDirection): TControlPopulator;
    function GetNamedControl(const AName: string): TControl;
    function MoveControls(const AControlNames: array of string;
      const ADX, ADY: Single): TControlPopulator;
    function AlignControlsRight(const AControlNames, AReferenceGroup: array of string;
      const ARightPadding: Single = 0): TControlPopulator;
    function CenterControlsHorizontally(const AControlNames, AReferenceGroup:
      array of string): TControlPopulator;
    function CenterControlsVertically(const AControlNames, AReferenceGroup:
      array of string): TControlPopulator;
    property NamedControls[const AName: string]: TControl read GetNamedControl;
    property ContentWidth: Single read FContentWidth;
    property ContentHeight: Single read FContentHeight;
    property CurrentLevel: TControlPopulatorLevel read GetCurrenteLevel;
  end;

implementation

uses
  {$IFDEF FPC}Graphics,
  {$ELSE}
    {$IFDEF FRAMEWORK_FMX} Fmx.Graphics,
    {$ELSE} Vcl.Graphics,
    {$ENDIF}
  {$ENDIF} Math;

{ TControlGridItem }

procedure TControlGridItem.AfterSetBounds;
begin
  // Nessa classe não faz nada
end;

constructor TControlGridItem.Create(AControl: TControl);
begin
  inherited Create;
  FControl := AControl;
end;

procedure TControlGridItem.SetBounds(ALeft, ATop, AWidth, AHeight: Single);
begin
  {$IFDEF FRAMEWORK_FMX}
  FControl.SetBounds(ALeft, ATop, AWidth, AHeight);
  {$ELSE}
  FControl.SetBounds(Trunc(ALeft), Trunc(ATop), Trunc(AWidth), Trunc(AHeight));
  {$ENDIF}
end;

function TControlGridItem.GetControl: TControl;
begin
  Result := FControl;
end;

function TControlGridItem.GetHeight: Single;
begin
  {$IFDEF FRAMEWORK_FMX}
  Result := FControl.Height;
  {$ELSE}
  Result := FControl.Height;
  {$ENDIF}
end;

function TControlGridItem.GetLeft: Single;
begin
  {$IFDEF FRAMEWORK_FMX}
  Result := FControl.Position.X;
  {$ELSE}
  Result := FControl.Left;
  {$ENDIF}
end;

procedure TControlGridItem.SetHeight(AValue: Single);
begin
  {$IFDEF FRAMEWORK_FMX}
  FControl.Height := AValue;
  {$ELSE}
  FControl.Height := Trunc(AValue);
  {$ENDIF}
end;

procedure TControlGridItem.SetVisible(AValue: Boolean);
begin
  FControl.Visible := AValue;
end;

procedure TControlGridItem.SetWidth(AValue: Single);
begin
  {$IFDEF FRAMEWORK_FMX}
  FControl.Width := AValue;
  {$ELSE}
  FControl.Width := Trunc(AValue);
  {$ENDIF}
end;

function TControlGridItem.GetTop: Single;
begin
  {$IFDEF FRAMEWORK_FMX}
  Result := FControl.Position.Y;
  {$ELSE}
  Result := FControl.Top;
  {$ENDIF}
end;

function TControlGridItem.GetVisible: Boolean;
begin
  Result := FControl.Visible;
end;

function TControlGridItem.GetWidth: Single;
begin
  {$IFDEF FRAMEWORK_FMX}
  Result := FControl.Width;
  {$ELSE}
  Result := FControl.Width;
  {$ENDIF}
end;

function TControlGridItem.IsControlOfType(AClass: TClass): Boolean;
begin
  if not Assigned(FControl) then
    Exit;
  Result := FControl is AClass;
end;

procedure TControlGridItem.Redraw;
begin

end;

{ TControlInfo }

class function TControlInfo.Create(AClass: TControlClass;
  const AName: string): TControlInfo;
begin
  Result.Control := nil;
  Result.ControlClass := AClass;
  Result.Name := AName;
  Result.Height := -1;
  Result.Width := -1;
end;

class function TControlInfo.Create(AControl: TControl): TControlInfo;
begin
  Result.Control := AControl;
  Result.ControlClass := TControlClass(AControl.ClassType);
  Result.Name := AControl.Name;
  Result.Height := -1;
  Result.Width := -1;
end;

function TControlInfo.WithAlign(AAlign: TAlign): TControlInfo;
begin
  Result := Self;
  Result.Align := AAlign;
end;

function TControlInfo.WithHeight(AHeight: Single): TControlInfo;
begin
  Result := Self;
  Result.Height := AHeight;
end;

function TControlInfo.WithLeft(ALeft: Single): TControlInfo;
begin
  Result := Self;
  Result.Left := ALeft;
end;

function TControlInfo.WithSetup(AProc: TControlSetupProc): TControlInfo;
begin
  Result := Self;
  Result.SetupProc := AProc;
end;

function TControlInfo.WithTop(ATop: Single): TControlInfo;
begin
  Result := Self;
  Result.Top := ATop;
end;

function TControlInfo.WithWidth(AWidth: Single): TControlInfo;
begin
  Result := Self;
  Result.Width := AWidth;
end;

function TControlInfo.WithWidthAndHeight(AWidth, AHeight: Single): TControlInfo;
begin
  Result := Self;
  Result.Width := AWidth;
  Result.Height := AHeight;
end;

{ TControlGridPopulator }

procedure TControlGridPopulator.SetControls(AValue: TControlList);
begin
  if FControls = AValue then
    Exit;
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
  FControls := TControlList.Create;
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

  if not AControlInfo.Name.IsEmpty then
    Control.Name := UniqueName(AControlInfo.Name);

  AddControl(Control, AProc);
end;

function TControlGridPopulator.CreateControl(AControlClass: TControlClass;
  AProc: TControlPopulateProc): TControlGridPopulator;
var
  ControlInfo: TControlInfo;
begin
  ControlInfo.ControlClass := AControlClass;
  ControlInfo.Name := '';
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

  if (not Assigned(Cell)) or (not Assigned(Cell.Item)) then
    Exit;

  Result := (Cell.Item as TControlGridItem).GetControl;
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

{ TControlPopulator }

function TControlPopulator.BreakLine: TControlPopulator;
begin
  Result := Self;
  CurrentLevel.CurrentTop := CurrentLevel.CurrentTop + CurrentLevel.MaxControlHeight; // FCurrentMaxControlHeight;
  CurrentLevel.CurrentLeft := CurrentLevel.InitialLeft;
  CurrentLevel.MaxControlHeight := 0;
end;

function TControlPopulator.CenterControlsVertically(const AControlNames,
  AReferenceGroup: array of string): TControlPopulator;
var
  RefBounds: TControlGroupBounds;
  TargetBounds: TControlGroupBounds;
  RefCenterY, TargetCenterY, DeltaY: Single;
begin
  Result := Self;

  RefBounds := GetControlsBounds(AReferenceGroup);
  TargetBounds := GetControlsBounds(AControlNames);

  RefCenterY := RefBounds.Top + (RefBounds.Height / 2);
  TargetCenterY := TargetBounds.Top + (TargetBounds.Height / 2);

  DeltaY := RefCenterY - TargetCenterY;
  MoveControls(AControlNames, 0, DeltaY);
end;

constructor TControlPopulator.Create;
begin
  FControls := TControlList.Create;
  FNamedControls := TStrControlDictionary.Create;
  FGroups := TControlGroupMap.Create;

  FLevelStack := TControlPopulatorLevelStack.Create(True);
  FLevelStack.Add(TControlPopulatorLevel.Create);

  FVerticalSpace := 0;
  FHorizontalSpace := 0;
end;

function TControlPopulator.AddControl(AControlInfo: TControlInfo;
  const AGroups: array of string): TControlPopulator;
var
  Control: TControl;
  ControlRight, ControlBottom: Single;

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

  if Assigned(AControlInfo.Control) then
    Control := AControlInfo.Control
  else
    Control := AControlInfo.ControlClass.Create(FOwner);

  Control.Parent := FParent;

  if not AControlInfo.Name.IsEmpty then
    Control.Name := UniqueName(AControlInfo.Name);

  Control.Align := AControlInfo.Align;

  if AControlInfo.Width >=0  then
    Control.Width := Trunc(AControlInfo.Width);

  if AControlInfo.Height >= 0 then
    Control.Height := Trunc(AControlInfo.Height);

  if AControlInfo.Top.HasValue then
    Control.Top := Trunc(AControlInfo.Top.Value)
  else
    Control.Top := Trunc(CurrentLevel.CurrentTop);

  if AControlInfo.Left.HasValue then
    Control.Left := Trunc(AControlInfo.Left.Value)
  else
    Control.Left := Trunc(CurrentLevel.CurrentLeft);

  if Assigned(AControlInfo.SetupProc) then
    AControlInfo.SetupProc(Control);

  MoveTopLeftAfterControl(Control);

  if Control.Align = alNone then
  begin
    ControlRight := Control.Left + Control.Width;
    ControlBottom := Control.Top + Control.Height;

    if ControlRight > FContentWidth then
      FContentWidth := ControlRight;

    if ControlBottom > FContentHeight then
      FContentHeight := ControlBottom;
  end;

  FControls.Add(Control);

  if not string(Control.Name).IsEmpty then
    FNamedControls.Add(Control.Name, Control);

  if not CurrentLevel.GroupName.IsEmpty then
    AddControlToGroups(Control, [CurrentLevel.GroupName]);

  AddControlToGroups(Control, AGroups);
end;

function TControlPopulator.AddControl(
  AControlInfo: TControlInfo): TControlPopulator;
begin
  Result := AddControl(AControlInfo, []);
end;

function TControlPopulator.AddControls(
  AControlCreateInfos: array of TControlInfo;
  const AGroups: array of string): TControlPopulator;
var
  I: Integer;
begin
  Result := Self;
  for I := Low(AControlCreateInfos) to High(AControlCreateInfos) do
    AddControl(AControlCreateInfos[I], AGroups);
end;

function TControlPopulator.AddControls(
  AControlCreateInfos: array of TControlInfo): TControlPopulator;
var
  I: Integer;
begin
  Result := Self;
  for I := Low(AControlCreateInfos) to High(AControlCreateInfos) do
    AddControl(AControlCreateInfos[I], []);
end;

procedure TControlPopulator.AddControlToGroups(AControl: TControl;
  const AGroups: array of string);
var
  Group: string;
  List: TControlList;
begin
  for Group in AGroups do
  begin
    if not FGroups.TryGetValue(Group, List) then
    begin
      List := TControlList.Create;
      FGroups.Add(Group, List);
    end;
    List.Add(AControl);
  end;
end;

function TControlPopulator.AddInLevel(const AControls: array of TControlInfo;
  ADirection: TControlPopulatorDirection): TControlPopulator;
begin
  Result := Self;
  BeginLevel(ADirection);
  AddControls(AControls);
  EndLevel;
end;

function TControlPopulator.CenterControlsHorizontally(const AControlNames,
  AReferenceGroup: array of string): TControlPopulator;
var
  RefBounds: TControlGroupBounds;
  TargetBounds: TControlGroupBounds;
  RefCenterX, TargetCenterX, DeltaX: Single;
begin
  Result := Self;

  RefBounds := GetControlsBounds(AReferenceGroup);
  TargetBounds := GetControlsBounds(AControlNames);

  RefCenterX := RefBounds.Left + (RefBounds.Width / 2);
  TargetCenterX := TargetBounds.Left + (TargetBounds.Width / 2);

  DeltaX := RefCenterX - TargetCenterX;
  MoveControls(AControlNames, DeltaX, 0);
end;

function TControlPopulator.AlignControlsRight(const AControlNames,
  AReferenceGroup: array of string; const ARightPadding: Single = 0): TControlPopulator;
var
  RefBounds: TControlGroupBounds;
  GroupBounds: TControlGroupBounds;
  DeltaX: Single;
begin
  Result := Self;

  RefBounds := GetControlsBounds(AReferenceGroup);
  GroupBounds := GetControlsBounds(AControlNames);

  DeltaX := (RefBounds.Left + RefBounds.Width)
    - GroupBounds.Width
    - GroupBounds.Left
    - ARightPadding;

  MoveControls(AControlNames, DeltaX, 0);
end;

function TControlPopulator.BeginLevel: TControlPopulator;
begin
  Result := Self;
  FLevelStack.Add(CurrentLevel.Clone);
end;

function TControlPopulator.BeginLevel(
  ADirection: TControlPopulatorDirection): TControlPopulator;
begin
  Result := BeginLevel;
  SetDirection(ADirection);
end;

function TControlPopulator.BreakLine(AIncTop: Single): TControlPopulator;
begin
  Result := BreakLine;
  IncTop(AIncTop);
end;

destructor TControlPopulator.Destroy;
var
  GroupList: TControlList;
begin
  FControls.Free;
  FNamedControls.Free;
  for GroupList in FGroups.Values do
    GroupList.Free;
  FGroups.Free;
  FLevelStack.Free;
  inherited;
end;

function TControlPopulator.EndLevel: TControlPopulator;
var
  SubLevel, SuperLevel: TControlPopulatorLevel;
  Bounds: TControlGroupBounds;
begin
  if FLevelStack.Count <= 1 then
    raise Exception.Create('EndLevel chamado no nível raiz');

  SubLevel := FLevelStack.Last;
  SuperLevel := FLevelStack[FLevelStack.Count - 2];

  Bounds := GetGroupBounds(SubLevel.GroupName);

  case SuperLevel.Direction of
    cpdHorizontal:
      begin
        SuperLevel.CurrentLeft := Bounds.Right + FHorizontalSpace;
        SuperLevel.MaxControlHeight := Max(SuperLevel.MaxControlHeight, Bounds.Height + FVerticalSpace);
      end;
    cpdVertical:
      begin
        SuperLevel.CurrentTop := Bounds.Bottom + FVerticalSpace;
      end;
  end;

  FLevelStack.Delete(FLevelStack.Count - 1); // remove nível atual
  Result := Self;
end;

function TControlPopulator.GetNamedControl(const AName: string): TControl;
begin
  if not FNamedControls.TryGetValue(AName, Result) then
    Result := nil;
end;

function TControlPopulator.IncLeft(AIncLeft: Single): TControlPopulator;
begin
  Result := Self;
  CurrentLevel.CurrentLeft := CurrentLevel.CurrentLeft + AIncLeft;
end;

function TControlPopulator.IncTop(AIncTop: Single): TControlPopulator;
begin
  Result := Self;
  CurrentLevel.CurrentTop := CurrentLevel.CurrentTop + AIncTop;
end;

function TControlPopulator.IncTopLeft(AIncTop,
  AIncLeft: Single): TControlPopulator;
begin
  Result := Self;
  IncTop(AIncTop);
  IncLeft(AIncLeft);
end;

function TControlPopulator.GetControlsBounds(
  AControlsNames: array of string): TControlGroupBounds;
var
  I: Integer;
  Control: TControl;
begin
  Result.Reset;

  for I := Low(AControlsNames) to High(AControlsNames) do
  begin
    Control := GetNamedControl(AControlsNames[I]);
    Result.Include(Control);
  end;
end;

function TControlPopulator.GetCurrenteLevel: TControlPopulatorLevel;
begin
  Result := FLevelStack.Last;
end;

function TControlPopulator.GetGroupBounds(
  const AGroupName: string): TControlGroupBounds;
var
  Control: TControl;
begin
  if not FGroups.ContainsKey(AGroupName) then
    raise Exception.CreateFmt('Grupo "%s" não encontrado.', [AGroupName]);

  Result.Reset;

  for Control in FGroups[AGroupName] do
    Result.Include(Control);
end;

function TControlPopulator.MoveControls(const AControlNames: array of string;
  const ADX, ADY: Single): TControlPopulator;
var
  Name: string;
  Ctrl: TControl;
begin
  Result := Self;

  for Name in AControlNames do
  begin
    if not FNamedControls.TryGetValue(Name, Ctrl) then
      raise Exception.CreateFmt('Controle "%s" não encontrado.', [Name]);

    Ctrl.Left := Round(Ctrl.Left + ADX);
    Ctrl.Top := Round(Ctrl.Top + ADY);
  end;
end;

procedure TControlPopulator.MoveTopLeftAfterControl(AControl: TControl);
begin
  if AControl.Align = alNone then
  begin
    if CurrentLevel.Direction = cpdHorizontal then
    begin
      CurrentLevel.CurrentLeft := CurrentLevel.CurrentLeft + AControl.Width + FHorizontalSpace;
      CurrentLevel.MaxControlHeight := Max(CurrentLevel.MaxControlHeight, AControl.Height + FVerticalSpace);
    end;

    if CurrentLevel.Direction = cpdVertical then
      CurrentLevel.CurrentTop := CurrentLevel.CurrentTop + AControl.Height + FVerticalSpace;
  end;
end;

function TControlPopulator.NextLevel(
  ADirection: TControlPopulatorDirection): TControlPopulator;
begin
  Result := EndLevel.BeginLevel(ADirection);
end;

function TControlPopulator.NextLevel: TControlPopulator;
begin
  Result := EndLevel.BeginLevel;
end;

function TControlPopulator.SetDirection(
  ADirection: TControlPopulatorDirection): TControlPopulator;
begin
  Result := Self;
  CurrentLevel.Direction := ADirection;
end;

function TControlPopulator.SetHorizontalSpace(
  AHorizontalSpace: Single): TControlPopulator;
begin
  Result := Self;
  FHorizontalSpace := AHorizontalSpace;
end;

function TControlPopulator.SetLeft(AControlName: string): TControlPopulator;
begin
  Result := SetLeft(NamedControls[AControlName].Left);
end;

function TControlPopulator.SetLeft(ALeft: Single): TControlPopulator;
begin
  Result := Self;
  CurrentLevel.CurrentLeft := ALeft;
  CurrentLevel.InitialLeft := ALeft;
  CurrentLevel.MaxControlHeight := 0;
end;

function TControlPopulator.SetSpace(AVerticalSpace, AHorizontalSpace: Single): TControlPopulator;
begin
  Result := Self;
  FVerticalSpace := AVerticalSpace;
  FHorizontalSpace := AHorizontalSpace;
end;

function TControlPopulator.SetTop(ATop: Single): TControlPopulator;
begin
  Result := Self;
  CurrentLevel.CurrentTop := ATop;
  CurrentLevel.InitialTop := ATop;
  CurrentLevel.MaxControlHeight := 0;
end;

function TControlPopulator.SetTop(AControlName: string): TControlPopulator;
begin
  Result := SetTop(NamedControls[AControlName].Top);
end;

function TControlPopulator.SetTopLeft(ATop, ALeft: Single): TControlPopulator;
begin
  Result := Self;
  SetTop(ATop);
  SetLeft(ALeft);
end;

function TControlPopulator.SetTopLeftNearControl(AControlName: string;
  APosition: TRelativePosition): TControlPopulator;
var
  Control: TControl;
begin
  Result := Self;

  Control := NamedControls[AControlName];
  SetTop(Control.Top);
  SetLeft(Control.Left);

  if APosition = rpBelow then
    SetTop(CurrentLevel.CurrentTop + Control.Height + FVerticalSpace);

  if APosition = rpRight then
    SetLeft(CurrentLevel.CurrentLeft + Control.Width + FHorizontalSpace);
end;

function TControlPopulator.SetTopLeftNearControls(
  AControlsNames: array of string;
  APosition: TRelativePosition): TControlPopulator;
var
  I: Integer;
  Ctrl: TControl;
  Bounds: TControlGroupBounds;
begin
  Result := Self;

  Bounds.Reset;

  for I := Low(AControlsNames) to High(AControlsNames) do
  begin
    Ctrl := GetNamedControl(AControlsNames[I]);
    Bounds.Include(Ctrl);
  end;

  case APosition of
    rpBelow:
      begin
        SetLeft(Bounds.Left);
        SetTop(Bounds.Bottom + FVerticalSpace);
      end;
    rpRight:
      begin
        SetLeft(Bounds.Right + FHorizontalSpace);
        SetTop(Bounds.Top);
      end;
  end;
end;

function TControlPopulator.SetTopLeftNearGroup(const AGroupName: string;
  APosition: TRelativePosition): TControlPopulator;
var
  Bounds: TControlGroupBounds;
begin
  Result := Self;

  Bounds := GetGroupBounds(AGroupName);
  SetTop(Bounds.Top);
  SetLeft(Bounds.Left);

  if APosition = rpBelow then
    SetTop(CurrentLevel.CurrentTop + Bounds.Height + FVerticalSpace);

  if APosition = rpRight then
    SetLeft(CurrentLevel.CurrentLeft + Bounds.Width + FHorizontalSpace);
end;

function TControlPopulator.SetVerticalSpace(
  AVerticalSpace: Single): TControlPopulator;
begin
  Result := Self;
  FVerticalSpace := AVerticalSpace;
end;

function TControlPopulator.WithOwnerAndParent(AOwner: TComponent;
  AParent: TWinControl): TControlPopulator;
begin
  Result := Self;
  FOwner := AOwner;
  FParent := AParent;
end;

function TControlPopulator.WithParent(AParent: TWinControl): TControlPopulator;
begin
  Result := Self;
  FParent := AParent;
end;

{ TControlGroupBounds }

function TControlGroupBounds.Height: Single;
begin
  Result := Bottom - Top;
end;

procedure TControlGroupBounds.Include(Control: TControl);
var
  CtrlRight, CtrlBottom: Single;
begin
  if Control = nil then
    Exit;

  CtrlRight := Control.Left + Control.Width;
  CtrlBottom := Control.Top + Control.Height;

  if Left > Control.Left then
    Left := Control.Left;
  if Top > Control.Top then
    Top := Control.Top;
  if Right < CtrlRight then
    Right := CtrlRight;
  if Bottom < CtrlBottom then
    Bottom := CtrlBottom;
end;

procedure TControlGroupBounds.Reset;
begin
  Left := MaxSingle;
  Top := MaxSingle;
  Right := -MaxSingle;
  Bottom := -MaxSingle;
end;

function TControlGroupBounds.Width: Single;
begin
  Result := Right - Left;
end;

{ TControlPopulatorLevel }

function TControlPopulatorLevel.Clone: TControlPopulatorLevel;
begin
  Result := TControlPopulatorLevel.Create;
  Result.Direction := Direction;
  Result.InitialTop := InitialTop;
  Result.InitialLeft := InitialLeft;
  Result.CurrentTop := CurrentTop;
  Result.CurrentLeft := CurrentLeft;
  Result.MaxControlHeight := MaxControlHeight;
end;

constructor TControlPopulatorLevel.Create;
begin
  Direction := cpdHorizontal;
  InitialTop := 0;
  InitialLeft := 0;
  CurrentTop := 0;
  CurrentLeft := 0;
  MaxControlHeight := 0;
  Inc(FGroupCounter);
  GroupName := '__LEVEL_GROUP_' + IntToStr(FGroupCounter) + '__';
end;

end.

