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
  UGridLayoutFillerFactory, Generics.Collections, Generics.Defaults,
  Vcl.Graphics, Vcl.ExtCtrls;

type
  {$IFNDEF FPC}
    {$IFDEF FRAMEWORK_FMX}
    TWinControl = TFmxObject;
    {$ENDIF}
  {$ENDIF}

  TProtectedControl = class(TControl);
  TControlClass = class of TControl;
  TWinControlClass = class of TWinControl;
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
  TWinControlSetupProc = procedure(AWinControl: TWinControl) of object;

  TWinControlInfo = record
    WinControl: TWinControl;
    WinControlClass: TWinControlClass;
    SetupProc: TWinControlSetupProc;
    Name: string;
    Caption: TOptionalString;
    Align: TOptionalAlign;
    Width: Single;
    Height: Single;
    Top: TOptionalSingle;
    Left: TOptionalSingle;
    function Setup(AProc: TWinControlSetupProc): TWinControlInfo;
    function WithAlign(AAlign: TAlign): TWinControlInfo;
    function WithWidth(AWidth: Single): TWinControlInfo;
    function WithHeight(AHeight: Single): TWinControlInfo;
    function WithWidthAndHeight(AWidth: Single; AHeight: Single): TWinControlInfo;
    function WithTop(ATop: Single): TWinControlInfo;
    function WithLeft(ALeft: Single): TWinControlInfo;
    function WithCaption(ACaption: string): TWinControlInfo;
    function CreateWinControl(AOwner: TComponent; AParent: TWinControl;
      const AName: string): TWinControl;
    class function Create(AClass: TWinControlClass; const AName: string=''): TWinControlInfo; overload; static;
    class function Create(AWinControl: TWinControl): TWinControlInfo; overload; static;
  end;

  { TControlInfo }

  TControlInfo = record
    Control: TControl;
    ControlClass: TControlClass;
    SetupProc: TControlSetupProc;
    Name: string;
    Caption: TOptionalString;
    Text: TOptionalString;
    Align: TOptionalAlign;
    Width: Single;
    Height: Single;
    Top: TOptionalSingle;
    Left: TOptionalSingle;
    function Setup(AProc: TControlSetupProc): TControlInfo;
    function WithAlign(AAlign: TAlign): TControlInfo;
    function WithWidth(AWidth: Single): TControlInfo;
    function WithHeight(AHeight: Single): TControlInfo;
    function WithWidthAndHeight(AWidth: Single; AHeight: Single): TControlInfo;
    function WithTop(ATop: Single): TControlInfo;
    function WithLeft(ALeft: Single): TControlInfo;
    function WithCaption(ACaption: string): TControlInfo;
    function WithText(AText: string): TControlInfo;
    function CreateControl(AOwner: TComponent; AParent: TWinControl;
      const AControlName: string): TControl;
    class function Create(AClass: TControlClass; const AName: string=''): TControlInfo; overload; static;
    class function Create(AControl: TControl): TControlInfo; overload; static;
  end;

  TControlRegistry = class;

  TControlRegistryEntry = record
    Registry: TControlRegistry;
    RefCount: Integer;
  end;

  TStrControlRegistryEntryDictionary = {$IFDEF FPC}specialize{$ENDIF} TDictionary<string, TControlRegistryEntry>;
  TStrControlDictionary = {$IFDEF FPC}specialize{$ENDIF} TDictionary<string, TControl>;
  TControlList = {$IFDEF FPC}specialize{$ENDIF} TList<TControl>;
  TControlGroupMap = {$IFDEF FPC}specialize{$ENDIF} TDictionary<string, TControlList>;
  TStrGridDictionary = {$IFDEF FPC}specialize{$ENDIF} TDictionary<string, TGridLayout>;

  TControlRegistry = class
  private
    class var FInstances: TStrControlRegistryEntryDictionary;
  public
    class function ForContext(const AKey: string): TControlRegistry; static;
    class procedure ReleaseContext(const AKey: string); static;
    class procedure ClearAll; static;
  private
    FControls: TControlList;
    FNamedControls: TStrControlDictionary;
    constructor CreatePrivate;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddControl(AControl: TControl; const AName: string = '');
    function GetControl(const AName: string): TControl;
    function TryGetControl(const AName: string; out AControl: TControl): Boolean;
    property Controls: TControlList read FControls;
    property NamedControls: TStrControlDictionary read FNamedControls;
  end;

  { TControlGridPopulator }

  TControlGridPopulator = class
  private
    FGrid: TGridLayout;
    FFiller: IGridFill;
    FFillerType: TFillerType;
    FOwner: TComponent;
    FParent: TWinControl;
    // FControls: TControlList;
    // FNamedControls: TStrControlDictionary;
    FControlRegistry: TControlRegistry;
    FControlRegistryName: string;
    FOnControlPopulate: TControlPopulateProc;
    function GetNamedControl(const AName: string): TControl;
    // procedure SetControls(AValue: TControlList);
    function GetControls: TControlList;
    procedure ConfigControl(AControl: TControl);
  public
    constructor Create(AControlRegistryName: string);
    destructor Destroy; override;
    function WithOwnerAndParentControl(AOwner: TComponent; AParent: TWinControl): TControlGridPopulator;
    procedure SetGrid(AGrid: TGridLayout);
    function UsingFiller(AFillerType: TFillerType; ARow: Integer=0; AColumn: Integer=0): TControlGridPopulator;
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
    property Controls: TControlList read GetControls;   // property Controls: TControlList read GetControls write SetControls;
    property NamedControls[const AName: string]: TControl read GetNamedControl;
    property Grid: TGridLayout read FGrid;

    property ControlRegistry: TControlRegistry read FControlRegistry;
    property ControlRegistryName: string read FControlRegistryName;
  end;

  { TGridLayoutHelper }

  TGridLayoutHelper = class helper for TGridLayout
  public
    function GetControl(ARow, ACol: Integer): TControl;
    procedure AddItem(AItem: TControl; ASettings: TGridCellSettings); overload;
  end;

  TAutoSizeContainer = class(TPanel)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TGridContainer = class(TAutoSizeContainer)
  private
    FGrid: TGridLayout;
    FGridPopulator: TControlGridPopulator;
  public
    constructor Create(AOwner: TComponent; AControlRegistryName: string);
    destructor Destroy; override;
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
    Parent: TWinControl;
    GroupName: string;
    Direction: TControlPopulatorDirection;
    InitialTop: Single;
    InitialLeft: Single;
    CurrentTop: Single;
    CurrentLeft: Single;
    MaxControlHeight: Single;
    constructor Create;
    function Clone: TControlPopulatorLevel;
  end;

  TControlPopulatorLevelStack = TObjectList<TControlPopulatorLevel>;

  TControlPopulator = class;

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

  TControlPopulator = class
  private
    FOwner: TComponent;
    // FControls: TControlList;
    // FNamedControls: TStrControlDictionary;

    FControlRegistryName: string;
    FControlRegistry: TControlRegistry;

    FGrids: TStrGridDictionary;
    FGroups: TControlGroupMap;
    FVerticalSpace: Single;
    FHorizontalSpace: Single;
    FLevelStack: TControlPopulatorLevelStack;
    function GetControls: TControlList;
    procedure MoveTopLeftAfterControl(AControl: TControl);
    procedure MoveTopLeftAfterBound(ABounds: TControlGroupBounds);
    procedure AddControlToGroups(AControl: TControl; const AGroups: array of string);
    function GetGroupBounds(const AGroupName: string): TControlGroupBounds;
    function GetCurrenteLevel: TControlPopulatorLevel;
    function GetContentWidth: Single;
    function GetFContentHeight: Single;
  public
    constructor Create(AControlRegistryName: string);
    destructor Destroy; override;
    function GetControlsBounds(AControlsNames: array of string): TControlGroupBounds;
    function SetSpace(AVerticalSpace, AHorizontalSpace: Single): TControlPopulator;
    function NextLevel(AGroupName: string=''): TControlPopulator; overload;
    function NextLevel(ADirection: TControlPopulatorDirection;
      AGroupName: string=''): TControlPopulator; overload;
    function PreviousLevel: TControlPopulator;
    function NextSiblingLevel(AGroupName: string='';
      ABreakLine: Boolean=False): TControlPopulator; overload;
    function NextSiblingLevel(ADirection: TControlPopulatorDirection;
      AGroupName: string=''; ABreakLine: Boolean=False): TControlPopulator; overload;
    function NextSiblingLevel(ABreakLine: Boolean=False): TControlPopulator; overload;
    function NextSiblingLevel(ADirection: TControlPopulatorDirection;
      ABreakLine: Boolean): TControlPopulator; overload;
    function NextSiblingLevelWithBreak(AGroupName: string=''): TControlPopulator; overload;
    function NextSiblingLevelWithBreak(ADirection: TControlPopulatorDirection;
      AGroupName: string=''): TControlPopulator; overload;
    function NextLevel(AWinControlInfo: TWinControlInfo;
      AGroupName: string=''): TControlPopulator; overload;
    function NextLevel(AWinControlInfo: TWinControlInfo;
      ADirection: TControlPopulatorDirection; AGroupName: string=''): TControlPopulator; overload;
    function NextSiblingLevel(AWinControlInfo: TWinControlInfo;
      AGroupName: string=''; ABreakLine: Boolean=False): TControlPopulator; overload;
    function NextSiblingLevel(AWinControlInfo: TWinControlInfo;
      ADirection: TControlPopulatorDirection;
      AGroupName: string=''; ABreakLine: Boolean=False): TControlPopulator; overload;
    function NextSiblingLevel(AWinControlInfo: TWinControlInfo;
      ABreakLine: Boolean=False): TControlPopulator; overload;
    function NextSiblingLevel(AWinControlInfo: TWinControlInfo;
      ADirection: TControlPopulatorDirection;
      ABreakLine: Boolean): TControlPopulator; overload;
    function NextSiblingLevelWithBreak(AWinControlInfo: TWinControlInfo; AGroupName: string=''): TControlPopulator; overload;
    function NextSiblingLevelWithBreak(AWinControlInfo: TWinControlInfo; ADirection: TControlPopulatorDirection;
      AGroupName: string=''): TControlPopulator; overload;

    function NextLevelGrid(AGridName: string; ABuilder: TGridLayoutBuilder): TControlPopulator; overload;



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
    property ContentWidth: Single read GetContentWidth;
    property ContentHeight: Single read GetFContentHeight;
    property CurrentLevel: TControlPopulatorLevel read GetCurrenteLevel;
    property Controls: TControlList read GetControls;
  end;

implementation

uses
  {$IFDEF FPC}Graphics,
  {$ELSE}
    {$IFDEF FRAMEWORK_FMX} Fmx.Graphics,
    {$ENDIF}
  {$ENDIF} Math, System.Types, Winapi.Messages, Vcl.ComCtrls;

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

function TControlInfo.CreateControl(AOwner: TComponent; AParent: TWinControl;
  const AControlName: string): TControl;
begin
  if Assigned(Control) then
    Result := Control
  else
    Result := ControlClass.Create(AOwner);

  if not AControlName.IsEmpty then
    Result.Name := AControlName;

  Result.Parent := AParent;

  if Caption.HasValue then
    TProtectedControl(Result).Caption := Caption.Value;

  if Text.HasValue then
    TProtectedControl(Result).Text := Text.Value;

  if Align.HasValue then
    Result.Align := Align.Value;

  if Width >= 0 then
    Result.Width := Trunc(Width);

  if Height >= 0 then
    Result.Height := Trunc(Height);

  if Top.HasValue then
    Result.Top := Trunc(Top.Value);

  if Left.HasValue then
    Result.Left := Trunc(Left.Value);

  if Assigned(SetupProc) then
    SetupProc(Result);
end;

function TControlInfo.WithAlign(AAlign: TAlign): TControlInfo;
begin
  Result := Self;
  Result.Align := AAlign;
end;

function TControlInfo.WithCaption(ACaption: string): TControlInfo;
begin
  Result := Self;
  Result.Caption := ACaption;
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

function TControlInfo.Setup(AProc: TControlSetupProc): TControlInfo;
begin
  Result := Self;
  Result.SetupProc := AProc;
end;

function TControlInfo.WithText(AText: string): TControlInfo;
begin
  Result := Self;
  Result.Text := AText;
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

{procedure TControlGridPopulator.SetControls(AValue: TControlList);
begin
  if FControls = AValue then
    Exit;
  FControls := AValue;
end;}

function TControlGridPopulator.GetControls: TControlList;
begin
  Result := FControlRegistry.FControls;
end;

function TControlGridPopulator.GetNamedControl(const AName: string): TControl;
begin
  if not FControlRegistry.TryGetControl(AName, Result) then // if not FNamedControls.TryGetValue(AName, Result) then
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

constructor TControlGridPopulator.Create(AControlRegistryName: string);
begin
  FGrid := nil;
  FFiller := nil;
  // FControls := TControlList.Create;
  // FNamedControls := TStrControlDictionary.Create;

  FControlRegistryName := AControlRegistryName;
  FControlRegistry := TControlRegistry.ForContext(AControlRegistryName);
end;

destructor TControlGridPopulator.Destroy;
begin
  FControlRegistry.ReleaseContext(FControlRegistryName);
  // FControls.Free;
  // FNamedControls.Free;
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
  // FControls.Add(AControl);
  // if not string(AControl.Name).IsEmpty then
  //  FNamedControls.Add(AControl.Name, AControl);

  FControlRegistry.AddControl(AControl, AControl.Name);

  ControlGridItem := TControlGridItem.Create(AControl);
  Settings := TGridCellSettings.Create(0, 0);

  try
    if Assigned(FOnControlPopulate) then
      FOnControlPopulate(AControl, Controls.Count-1, Settings);   // FOnControlPopulate(AControl, FControls.Count-1, Settings);
    if Assigned(AProc) then
      AProc(AControl, Controls.Count-1, Settings);   // AProc(AControl, FControls.Count-1, Settings);

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

    while ControlRegistry.FNamedControls.ContainsKey(Candidate) do  // while FNamedControls.ContainsKey(Candidate) do
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
  CurrentLevel.CurrentTop := CurrentLevel.CurrentTop + CurrentLevel.MaxControlHeight;
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

constructor TControlPopulator.Create(AControlRegistryName: string);
begin
  // FControls := TControlList.Create;
  // FNamedControls := TStrControlDictionary.Create;

  FControlRegistryName := AControlRegistryName;
  FControlRegistry := TControlRegistry.ForContext(FControlRegistryName);

  FGrids := TStrGridDictionary.Create;
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
  Level: TControlPopulatorLevel;

  function UniqueName(const ABaseName: string): string;
  var
    Index: Integer;
    Candidate: string;
  begin
    if ABaseName.IsEmpty then
      Exit('');

    Candidate := ABaseName;
    Index := 1;

    while FControlRegistry.NamedControls.ContainsKey(Candidate) do  // while FNamedControls.ContainsKey(Candidate) do
    begin
      Candidate := ABaseName + IntToStr(Index);
      Inc(Index);
    end;

    Result := Candidate;
  end;

  function CreateControl(Info: TControlInfo): TControl;
  var
    ControlName: string;
  begin
    ControlName := '';
    if not AControlInfo.Name.IsEmpty then
      ControlName := UniqueName(AControlInfo.Name);

    Info := AControlInfo;

    if not Info.Top.HasValue then
      Info := Info.WithTop(CurrentLevel.CurrentTop);

    if not Info.Left.HasValue then
      Info := Info.WithLeft(CurrentLevel.CurrentLeft);

    Result := Info.CreateControl(FOwner, CurrentLevel.Parent, ControlName);
  end;

begin
  Result := Self;

  Control := CreateControl(AControlInfo);

  FControlRegistry.AddControl(Control); // FControls.Add(Control);

  // caso especial: TTabSheet / TPageControl
  if (Control is TTabSheet) and (CurrentLevel.Parent is TPageControl) then
  begin
    TTabSheet(Control).Parent := nil;
    TTabSheet(Control).PageControl := TPageControl(CurrentLevel.Parent);
  end;

  // if not string(Control.Name).IsEmpty then
  //   FNamedControls.Add(Control.Name, Control);

  for Level in FLevelStack do
    if not Level.GroupName.IsEmpty then
      AddControlToGroups(Control, [Level.GroupName]);

  AddControlToGroups(Control, AGroups);

  MoveTopLeftAfterControl(Control);
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
  NextLevel(ADirection);
  AddControls(AControls);
  PreviousLevel;
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

function TControlPopulator.NextLevel(AGroupName: string): TControlPopulator;
begin
  Result := Self;
  FLevelStack.Add(CurrentLevel.Clone);
  CurrentLevel.InitialTop := CurrentLevel.CurrentTop;
  CurrentLevel.InitialLeft := CurrentLevel.CurrentLeft;
  CurrentLevel.MaxControlHeight := 0;

  if not AGroupName.IsEmpty then
    CurrentLevel.GroupName := AGroupName;
end;

function TControlPopulator.NextLevel(
  ADirection: TControlPopulatorDirection; AGroupName: string): TControlPopulator;
begin
  Result := NextLevel(AGroupName);
  SetDirection(ADirection);
end;

function TControlPopulator.NextSiblingLevel(ADirection: TControlPopulatorDirection;
  ABreakLine: Boolean): TControlPopulator;
begin
  Result := NextSiblingLevel(ADirection, '', ABreakLine);
end;

function TControlPopulator.NextSiblingLevelWithBreak(
  ADirection: TControlPopulatorDirection;
  AGroupName: string): TControlPopulator;
begin
  Result := NextSiblingLevel(ADirection, AGroupName, True);
end;

function TControlPopulator.NextSiblingLevelWithBreak(
  AGroupName: string): TControlPopulator;
begin
  Result := NextSiblingLevel(AGroupName, True);
end;

function TControlPopulator.NextSiblingLevel( ABreakLine: Boolean): TControlPopulator;
begin
  Result := NextSiblingLevel('', ABreakLine);
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
  // FControls.Free;
  // FNamedControls.Free;
  TControlRegistry.ReleaseContext(FControlRegistryName);

  FGrids.Free;
  for GroupList in FGroups.Values do
    GroupList.Free;
  FGroups.Free;
  FLevelStack.Free;
  inherited;
end;

function TControlPopulator.PreviousLevel: TControlPopulator;
var
  SubLevel, SuperLevel: TControlPopulatorLevel;
  Bounds: TControlGroupBounds;

  function GetSubLevelBounds: TControlGroupBounds;
  begin
    if (SubLevel.Parent = SuperLevel.Parent) then
      Result := GetGroupBounds(SubLevel.GroupName)
    else
    begin
      // quando o TPanel for o container e estiver configurado para
      // AutoSize, o TPanel ainda não estará redimensionado, sendo
      // necessário força-lo a se atualizar.
      if (SubLevel.Parent is TPanel) and TPanel(SubLevel.Parent).AutoSize then
      begin
        SubLevel.Parent.HandleNeeded;
        SubLevel.Parent.Invalidate;
        SubLevel.Parent.Update;
      end;

      Result.Reset;
      Result.Include(SubLevel.Parent);
    end;
  end;

begin
  if FLevelStack.Count <= 1 then
    raise Exception.Create('PreviousLevel chamado no nível raiz');

  SubLevel := FLevelStack.Last;
  SuperLevel := FLevelStack[FLevelStack.Count - 2];

  Bounds := GetSubLevelBounds;

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

  MoveTopLeftAfterBound(GetGroupBounds(SuperLevel.GroupName));

  FLevelStack.Delete(FLevelStack.Count - 1); // remove nível atual
  Result := Self;
end;

function TControlPopulator.GetNamedControl(const AName: string): TControl;
begin
  if not FControlRegistry.NamedControls.TryGetValue(AName, Result) then   // if not FNamedControls.TryGetValue(AName, Result) then
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

function TControlPopulator.GetContentWidth: Single;
begin
  Result := GetGroupBounds(FLevelStack.First.GroupName).Width;
end;

function TControlPopulator.GetControls: TControlList;
begin
  Result := FControlRegistry.Controls;
end;

function TControlPopulator.GetControlsBounds(
  AControlsNames: array of string): TControlGroupBounds;
var
  I: Integer;
  Name: string;
  Control: TControl;
  Group: TControlList;
begin
  Result.Reset;

  for I := Low(AControlsNames) to High(AControlsNames) do
  begin
    Name := AControlsNames[I];

    if FControlRegistry.NamedControls.TryGetValue(Name, Control) then  // if FNamedControls.TryGetValue(Name, Control) then
      Result.Include(Control)
    else if FGroups.TryGetValue(Name, Group) then
      for Control in Group do
        Result.Include(Control);
  end;
end;

function TControlPopulator.GetCurrenteLevel: TControlPopulatorLevel;
begin
  Result := FLevelStack.Last;
end;

function TControlPopulator.GetFContentHeight: Single;
begin
  Result := GetGroupBounds(FLevelStack.First.GroupName).Height;
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
    if not FControlRegistry.NamedControls.TryGetValue(Name, Ctrl) then   // if not FNamedControls.TryGetValue(Name, Ctrl) then
      raise Exception.CreateFmt('Controle "%s" não encontrado.', [Name]);

    Ctrl.Left := Round(Ctrl.Left + ADX);
    Ctrl.Top := Round(Ctrl.Top + ADY);
  end;
end;

procedure TControlPopulator.MoveTopLeftAfterBound(ABounds: TControlGroupBounds);
begin
  if CurrentLevel.Direction = cpdHorizontal then
  begin
    CurrentLevel.CurrentLeft := CurrentLevel.CurrentLeft + ABounds.Width + FHorizontalSpace;
    CurrentLevel.MaxControlHeight := Max(CurrentLevel.MaxControlHeight, ABounds.Height + FVerticalSpace);
  end;

  if CurrentLevel.Direction = cpdVertical then
    CurrentLevel.CurrentTop := CurrentLevel.CurrentTop + ABounds.Height + FVerticalSpace;
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

  if AControl.Align = alTop then
    CurrentLevel.CurrentTop := CurrentLevel.CurrentTop + AControl.Height + FVerticalSpace;

  if AControl.Align = alLeft then
    SetTopLeft(
      CurrentLevel.CurrentTop,
      CurrentLevel.CurrentLeft + AControl.Width + FHorizontalSpace
    )
end;

function TControlPopulator.NextSiblingLevel(ADirection: TControlPopulatorDirection;
  AGroupName: string; ABreakLine: Boolean): TControlPopulator;
begin
  Result := PreviousLevel;
  if ABreakLine then
    BreakLine;
  NextLevel(ADirection, AGroupName);
end;

function TControlPopulator.NextSiblingLevel(AGroupName: string;
  ABreakLine: Boolean): TControlPopulator;
begin
  Result := PreviousLevel;
  if ABreakLine then
    BreakLine;
  NextLevel(AGroupName);
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
  CurrentLevel.Parent := AParent;
end;

function TControlPopulator.WithParent(AParent: TWinControl): TControlPopulator;
begin
  Result := Self;
  CurrentLevel.Parent := AParent;
end;

function TControlPopulator.NextLevel(AWinControlInfo: TWinControlInfo;
  AGroupName: string): TControlPopulator;
var
  WinControl: TWinControl;
begin
  Result := Self;

  if (AWinControlInfo.WinControlClass.InheritsFrom(TTabSheet))
      and (CurrentLevel.Parent is TPageControl) then
    WinControl := AWinControlInfo.CreateWinControl(
      CurrentLevel.Parent, CurrentLevel.Parent, AWinControlInfo.Name)
  else
    WinControl := AWinControlInfo.CreateWinControl(
      FOwner, CurrentLevel.Parent, AWinControlInfo.Name);

  AddControl(TControlInfo.Create(WinControl));
  NextLevel(AGroupName);
  WithParent(WinControl);
  SetTopLeft(0, 0);
end;

function TControlPopulator.NextLevel(AWinControlInfo: TWinControlInfo;
  ADirection: TControlPopulatorDirection; AGroupName: string): TControlPopulator;
begin
  Result := NextLevel(AWinControlInfo, AGroupName);
  SetDirection(ADirection);
end;

function TControlPopulator.NextLevelGrid(AGridName: string;
  ABuilder: TGridLayoutBuilder): TControlPopulator;
var
  Grid: TGridLayout;
begin
  Grid := ABuilder.Build;
  Result := NextLevel(AGridName);
  FGrids.Add(AGridName, Grid);
end;

function TControlPopulator.NextSiblingLevel(AWinControlInfo: TWinControlInfo;
  AGroupName: string; ABreakLine: Boolean): TControlPopulator;
begin
  Result := PreviousLevel;
  if ABreakLine then
    BreakLine;
  NextLevel(AWinControlInfo, AGroupName);
end;

function TControlPopulator.NextSiblingLevel(AWinControlInfo: TWinControlInfo;
  ADirection: TControlPopulatorDirection; AGroupName: string;
  ABreakLine: Boolean): TControlPopulator;
begin
  Result := PreviousLevel;
  if ABreakLine then
    BreakLine;
  NextLevel(AWinControlInfo, ADirection, AGroupName);
end;

function TControlPopulator.NextSiblingLevel(AWinControlInfo: TWinControlInfo;
  ABreakLine: Boolean): TControlPopulator;
begin
  Result := NextSiblingLevel(AWinControlInfo, '', ABreakLine);
end;

function TControlPopulator.NextSiblingLevel(AWinControlInfo: TWinControlInfo;
  ADirection: TControlPopulatorDirection;
  ABreakLine: Boolean): TControlPopulator;
begin
  Result := NextSiblingLevel(AWinControlInfo, ADirection, '', ABreakLine);
end;

function TControlPopulator.NextSiblingLevelWithBreak(
  AWinControlInfo: TWinControlInfo; ADirection: TControlPopulatorDirection;
  AGroupName: string): TControlPopulator;
begin
  Result := NextSiblingLevel(AWinControlInfo, ADirection, AGroupName, True);
end;

function TControlPopulator.NextSiblingLevelWithBreak(
  AWinControlInfo: TWinControlInfo; AGroupName: string): TControlPopulator;
begin
  Result := NextSiblingLevel(AWinControlInfo, AGroupName, True);
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
  Result.Parent := Parent;
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
  GroupName := '__LEVEL_GROUP_' + IntToStr(FGroupCounter - 1) + '__';
end;

{ TWinControlInfo }

class function TWinControlInfo.Create(AClass: TWinControlClass;
  const AName: string): TWinControlInfo;
begin
  Result.WinControl := nil;
  Result.WinControlClass := AClass;
  Result.Name := AName;
  Result.Height := -1;
  Result.Width := -1;
end;

class function TWinControlInfo.Create(AWinControl: TWinControl): TWinControlInfo;
begin
  Result.WinControl := AWinControl;
  Result.WinControlClass := TWinControlClass(AWinControl.ClassType);
  Result.Name := AWinControl.Name;
  Result.Height := -1;
  Result.Width := -1;
end;

function TWinControlInfo.CreateWinControl(AOwner: TComponent;
  AParent: TWinControl; const AName: string): TWinControl;
begin
  if Assigned(WinControl) then
    Result := WinControl
  else
    Result := WinControlClass.Create(AOwner);

  if not AName.IsEmpty then
    Result.Name := AName;

  Result.Parent := AParent;

  if Caption.HasValue then
    TProtectedControl(Result).Caption := Caption.Value;

  if Align.HasValue then
    Result.Align := Align.Value;

  if Width >= 0 then
    Result.Width := Trunc(Width);

  if Height >= 0 then
    Result.Height := Trunc(Height);

  if Top.HasValue then
    Result.Top := Trunc(Top.Value);

  if Left.HasValue then
    Result.Left := Trunc(Left.Value);

  if Assigned(SetupProc) then
    SetupProc(Result);
end;

function TWinControlInfo.Setup(AProc: TWinControlSetupProc): TWinControlInfo;
begin
  Result := Self;
  Result.SetupProc := AProc;
end;

function TWinControlInfo.WithAlign(AAlign: TAlign): TWinControlInfo;
begin
  Result := Self;
  Result.Align := AAlign;
end;

function TWinControlInfo.WithCaption(ACaption: string): TWinControlInfo;
begin
  Result := Self;
  Result.Caption := ACaption;
end;

function TWinControlInfo.WithHeight(AHeight: Single): TWinControlInfo;
begin
  Result := Self;
  Result.Height := AHeight;
end;

function TWinControlInfo.WithLeft(ALeft: Single): TWinControlInfo;
begin
  Result := Self;
  Result.Left := ALeft;
end;

function TWinControlInfo.WithTop(ATop: Single): TWinControlInfo;
begin
  Result := Self;
  Result.Top := ATop;
end;

function TWinControlInfo.WithWidth(AWidth: Single): TWinControlInfo;
begin
  Result := Self;
  Result.Width := AWidth;
end;

function TWinControlInfo.WithWidthAndHeight(AWidth,
  AHeight: Single): TWinControlInfo;
begin
  Result := Self;
  Result.Width := AWidth;
  Result.Height := AHeight;
end;

{ TAutoSizeContainer }

constructor TAutoSizeContainer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BevelOuter := bvNone;
  AutoSize := True;
end;

{ TGridContainer }

constructor TGridContainer.Create(AOwner: TComponent; AControlRegistryName: string);
begin
  inherited Create(AOwner);
  FGrid := nil;
  FGridPopulator := TControlGridPopulator.Create(AControlRegistryName);
end;

destructor TGridContainer.Destroy;
begin
  FGridPopulator.Free;

  inherited;
end;

{ TControlRegistry }

procedure TControlRegistry.AddControl(AControl: TControl; const AName: string);
begin
  if AControl = nil then
    Exit;

  FControls.Add(AControl);

  if AName <> '' then
  begin
    if FNamedControls.ContainsKey(AName) then
      raise Exception.CreateFmt('Já existe um controle registrado com o nome "%s".', [AName]);
    FNamedControls.Add(AName, AControl);
  end;
end;

class procedure TControlRegistry.ClearAll;
var
  Entry: TControlRegistryEntry;
begin
  if not Assigned(FInstances) then
    Exit;

  for Entry in FInstances.Values do
    Entry.Registry.Free;

  FInstances.Free;
  FInstances := nil;
end;

constructor TControlRegistry.Create;
begin
  raise Exception.Create('Use TControlRegistry.ForContext');
end;

constructor TControlRegistry.CreatePrivate;
begin
  inherited Create;
  FControls := TControlList.Create;
  FNamedControls := TStrControlDictionary.Create;
end;

destructor TControlRegistry.Destroy;
begin
  FControls.Free;
  FNamedControls.Free;
  inherited;
end;

class function TControlRegistry.ForContext(const AKey: string): TControlRegistry;
var
  Entry: TControlRegistryEntry;
begin
  if FInstances = nil then
    FInstances := TStrControlRegistryEntryDictionary.Create; // TDictionary<string, TControlRegistry>.Create;

  if FInstances.TryGetValue(AKey, Entry) then
  begin
    Inc(Entry.RefCount);
    FInstances[AKey] := Entry;
    Exit(Entry.Registry);
  end;

  Entry.Registry := TControlRegistry.CreatePrivate;
  Entry.RefCount := 1;
  FInstances.Add(AKey, Entry);
  Result := Entry.Registry;
end;

function TControlRegistry.GetControl(const AName: string): TControl;
begin
  if not FNamedControls.TryGetValue(AName, Result) then
    raise Exception.CreateFmt('Controle com o nome "%s" não encontrado.', [AName]);
end;

class procedure TControlRegistry.ReleaseContext(const AKey: string);
var
  Entry: TControlRegistryEntry;
begin
  if not Assigned(FInstances) then
    Exit;

  if FInstances.TryGetValue(AKey, Entry) then
  begin
    Dec(Entry.RefCount);
    if Entry.RefCount <= 0 then
    begin
      Entry.Registry.Free;
      FInstances.Remove(AKey);
    end
    else
    begin
      FInstances[AKey] := Entry;
    end;
  end;

  if FInstances.Count = 0 then
  begin
    FInstances.Free;
    FInstances := nil;
  end;
end;

function TControlRegistry.TryGetControl(const AName: string;
  out AControl: TControl): Boolean;
begin
  Result := FNamedControls.TryGetValue(AName, AControl);
end;

end.

