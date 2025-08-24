unit ulayout.controls;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$LONGSTRINGS ON}{$MODESWITCH TYPEHELPERS}{$MODESWITCH ADVANCEDRECORDS}
{$ENDIF}

interface

uses
  {$IFDEF FPC}Controls, StdCtrls, ExtCtrls,
  {$ELSE}
    {$IFDEF FRAMEWORK_FMX}
    FMX.Controls, FMX.StdCtrls, Fmx.Types, FMX.ExtCtrls, FMX.TabControl, FMX.Forms,
    {$ELSE}
    Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls,
    {$ENDIF}
  {$ENDIF}
  Classes, SysUtils, ULayout, UGridLayoutBuilder,
  UGridLayoutFillerFactory, Generics.Collections, Generics.Defaults;

type
  {$IFNDEF FPC}
    {$IFDEF FRAMEWORK_FMX}
    TWinControl = TControl;
    {$ENDIF}
  {$ENDIF}

  TProtectedControl = class(TControl);
  TControlClass = class of TControl;
  TWinControlClass = class of TWinControl;
  TControlPopulateProc = procedure(AControl: TControl; AIndex: Integer;
    ASettings: TGridCellSettings) of object;

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
  TComponentSetupProc = procedure(AComponent: TComponent) of object;
  TWinControlSetupProc = procedure(AWinControl: TWinControl) of object;

  TComponentInfo = record
    Component: TComponent;
    ComponentClass: TComponentClass;
    SetupProc: TComponentSetupProc;
    Name: string;
    function Setup(AProc: TComponentSetupProc): TComponentInfo;
    function WithName(AName: string): TComponentInfo;
    function CreateComponent(AOwner: TComponent; const AControlName: string): TComponent;
    class function Create(AClass: TComponentClass; const AName: string=''): TComponentInfo; overload; static;
    class function Create(AComponent: TComponent): TComponentInfo; overload; static;
  end;

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
    OnClick: TNotifyEvent;
    function Setup(AProc: TControlSetupProc): TControlInfo;
    function WithAlign(
      AAlign: {$IFDEF FRAMEWORK_FMX}TAlignLayout{$ELSE}TAlign{$ENDIF}): TControlInfo;
    function WithName(AName: string): TControlInfo;
    function WithWidth(AWidth: Single): TControlInfo;
    function WithHeight(AHeight: Single): TControlInfo;
    function WithWidthAndHeight(AWidth: Single; AHeight: Single): TControlInfo;
    function WithTop(ATop: Single): TControlInfo;
    function WithLeft(ALeft: Single): TControlInfo;
    function WithCaption(ACaption: string): TControlInfo;
    function WithText(AText: string): TControlInfo;
    function WithOnClick(AOnClick: TNotifyEvent): TControlInfo;
    function CreateControl(AOwner: TComponent; AParent: TWinControl;
      const AControlName: string): TControl;
    class function Create(AClass: TControlClass; const AName: string=''): TControlInfo; overload; static;
    class function Create(AControl: TControl): TControlInfo; overload; static;
  end;

  TComponentRegistry = class;

  IRegistryContextHandle = interface
    ['{A7D78704-853A-43D2-B3D4-770B3B3143D1}']
    function GetRegistry: TComponentRegistry;
    property Registry: TComponentRegistry read GetRegistry;
  end;

  TRegistryContextHandle = class(TInterfacedObject, IRegistryContextHandle)
  private
    FContextKey: string;
    FRegistry: TComponentRegistry;
  public
    constructor Create(const AContextKey: string);
    destructor Destroy; override;
    function GetRegistry: TComponentRegistry;
  end;

  TComponentRegistryEntry = record
    Registry: TComponentRegistry;
    RefCount: Integer;
  end;

  TStrComponentRegistryEntryDictionary = {$IFDEF FPC}specialize{$ENDIF} TDictionary<string, TComponentRegistryEntry>;
  TStrComponentDictionary = {$IFDEF FPC}specialize{$ENDIF} TDictionary<string, TComponent>;
  TStrControlDictionary = {$IFDEF FPC}specialize{$ENDIF} TDictionary<string, TControl>;
  TControlList = {$IFDEF FPC}specialize{$ENDIF} TList<TControl>;
  TComponentList = {$IFDEF FPC}specialize{$ENDIF} TList<TComponent>;
  TControlGroupMap = {$IFDEF FPC}specialize{$ENDIF} TDictionary<string, TControlList>;
  TStrGridDictionary = {$IFDEF FPC}specialize{$ENDIF} TDictionary<string, TGridLayout>;

  TRegistryNotifier = class(TComponent)
  private
    FOwnerRegistry: TComponentRegistry;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponentRegistry);
  end;

  TComponentRegistry = class
  private
    class var FInstances: TStrComponentRegistryEntryDictionary;
    class function GetContextComponents(const AContext, AName: string): TComponent; static;
  protected
    class function ForContext(const AKey: string): TComponentRegistry; static;
    class procedure ReleaseContext(const AKey: string); static;
  public
    class function GetContextHandle(AKey: string): IRegistryContextHandle;
    class procedure ClearAll; static;
    {$IFDEF FPC}generic{$ENDIF}
    class function GetControlFromContext<T: TControl>(const AContextKey: string; const AControlName: string): T; overload;
    class function GetControlFromContext(const AContextKey: string; const AControlName: string): TControl; overload;
    {$IFDEF FPC}generic{$ENDIF}
    class function GetComponentFromContext<T: TComponent>(const AContextKey: string; const AComponentlName: string): T; overload;
    class function GetComponentFromContext(const AContextKey: string; const AComponentlName: string): TComponent; overload;
    class property ContextComponents[const AContext, AName: string]: TComponent read GetContextComponents;
  private
    FNotifier: TRegistryNotifier;
    FComponents: TComponentList;
    FControls: TControlList;
    FNamedComponents: TStrComponentDictionary;
    FNamedControls: TStrControlDictionary;
    constructor CreatePrivate;
    function GetItem(ACompName: string): TComponent;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddControl(AControl: TControl; const AName: string = '');
    procedure AddComponent(AComponent: TComponent; const AName: string = '');
    {$IFDEF FPC}generic{$ENDIF}
    function GetComponent<T: TComponent>(const AName: string): T; overload;
    function GetComponent(const AName: string): TComponent; overload;
    {$IFDEF FPC}generic{$ENDIF}
    function TryGetComponent<T: TComponent>(const AName: string; out AComponent: T): Boolean; overload;
    function TryGetComponent(const AName: string; out AComponent: TComponent): Boolean; overload;
    {$IFDEF FPC}generic{$ENDIF}
    function GetControl<T: TControl>(const AName: string): T; overload;
    function GetControl(const AName: string): TControl; overload;
    {$IFDEF FPC}generic{$ENDIF}
    function TryGetControl<T: TControl>(const AName: string; out AControl: T): Boolean; overload;
    function TryGetControl(const AName: string; out AControl: TControl): Boolean; overload;
    procedure RegisterComponentForNotification(AComp: TComponent);
    procedure UnregisterComponentForNotification(AComp: TComponent);
    property Components: TComponentList read FComponents;
    property Controls: TControlList read FControls;
    property NamedComponents: TStrComponentDictionary read FNamedComponents;
    property Items[ACompName: string]: TComponent read GetItem; default;
  end;

  TControlGridPopulator = class
  private
    FGrid: TGridLayout;
    FFiller: IGridFill;
    FFillerType: TFillerType;
    FOwner: TComponent;
    FParent: TWinControl;
    FComponentRegistry: TComponentRegistry;
    FComponentRegistryName: string;
    FOnControlPopulate: TControlPopulateProc;
    function GetNamedControl(const AName: string): TControl;
    function GetControls: TControlList;
    procedure ConfigControl(AControl: TControl);
  public
    constructor Create(AComponentRegistryName: string);
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
    property Controls: TControlList read GetControls;
    property NamedControls[const AName: string]: TControl read GetNamedControl;
    property Grid: TGridLayout read FGrid;
    property ComponentRegistry: TComponentRegistry read FComponentRegistry;
    property ComponentRegistryName: string read FComponentRegistryName;
  end;

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
    constructor Create(AOwner: TComponent; AComponentRegistryName: string);
    destructor Destroy; override;
  end;

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
    procedure Include(Control: TControl); overload;
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
    MaxControlWidth: Single;
    VerticalSpace: Single;
    HorizontalSpace: Single;
    constructor Create;
    function Clone: TControlPopulatorLevel;
  end;

  TControlPopulatorLevelStack = {$IFDEF FPC}specialize{$ENDIF} TObjectList<TControlPopulatorLevel>;

  TComponentPopulator = class
  private
    FOwner: TComponent;
    FRegistryContextHandle: IRegistryContextHandle;
    function GetComponentRegistry: TComponentRegistry;
    function GetComponents: TComponentList;
    function GetItem(const AName: string): TComponent;
  public
    constructor Create(AComponentRegistryName: string);
    destructor Destroy; override;
    {$IFDEF FPC}generic{$ENDIF}
    function GetComponent<T: TComponent>(const AName: string): T; overload;
    function GetComponent(const AName: string): TComponent; overload;
    function WithOwner(AOwner: TComponent): TComponentPopulator;
    function Add(AComponentInfo: TComponentInfo): TComponentPopulator; overload;
    property Registry: TComponentRegistry read GetComponentRegistry;
    property Items[const AName: string]: TComponent read GetItem; default;
  end;

  TControlPopulator = class
  private
    FOwner: TComponent;
    FRegistryContextHandle: IRegistryContextHandle;
    FGroups: TControlGroupMap;
    FLevelStack: TControlPopulatorLevelStack;
    function GetControls: TControlList;
    procedure MoveTopLeftAfterControl(AControl: TControl);
    procedure MoveTopLeftAfterBound(ABounds: TControlGroupBounds);
    procedure AddControlToGroups(AControl: TControl; const AGroups: array of string);
    function GetGroupBounds(const AGroupName: string): TControlGroupBounds;
    function GetCurrenteLevel: TControlPopulatorLevel;
    function GetContentWidth: Single;
    function GetFContentHeight: Single;
    function GetComponentRegistry: TComponentRegistry;
    function GetItem(const AName: string): TControl;
  public
    constructor Create(ARegistryContext: string);
    destructor Destroy; override;
    {$IFDEF FPC}generic{$ENDIF}
    function GetControl<T: TControl>(const AName: string): T; overload;
    function GetControl(const AName: string): TControl; overload;
    function GetControlsBounds(AControlsNames: array of string): TControlGroupBounds;
    function SetSpace(AVerticalSpace, AHorizontalSpace: Single): TControlPopulator;
    function NextLevel(AGroupName: string=''): TControlPopulator; overload;
    function NextLevel(ADirection: TControlPopulatorDirection;
      AGroupName: string=''): TControlPopulator; overload;
    function PreviousLevel: TControlPopulator;
    function NextSiblingLevel(AGroupName: string='';
      ABreak: Boolean=False): TControlPopulator; overload;
    function NextSiblingLevel(ADirection: TControlPopulatorDirection;
      AGroupName: string=''; ABreak: Boolean=False): TControlPopulator; overload;
    function NextSiblingLevel(ABreak: Boolean=False): TControlPopulator; overload;
    function NextSiblingLevel(ADirection: TControlPopulatorDirection;
      ABreak: Boolean): TControlPopulator; overload;
    function NextSiblingLevelWithBreak(AGroupName: string=''): TControlPopulator; overload;
    function NextSiblingLevelWithBreak(ADirection: TControlPopulatorDirection;
      AGroupName: string=''): TControlPopulator; overload;
    function NextLevel(AControlInfo: TControlInfo;
      AGroupName: string=''): TControlPopulator; overload;
    function NextLevel(AControlInfo: TControlInfo;
      ADirection: TControlPopulatorDirection; AGroupName: string=''): TControlPopulator; overload;
    function NextSiblingLevel(AControlInfo: TControlInfo;
      AGroupName: string=''; ABreak: Boolean=False): TControlPopulator; overload;
    function NextSiblingLevel(AControlInfo: TControlInfo;
      ADirection: TControlPopulatorDirection;
      AGroupName: string=''; ABreak: Boolean=False): TControlPopulator; overload;
    function NextSiblingLevel(AControlInfo: TControlInfo;
      ABreak: Boolean=False): TControlPopulator; overload;
    function NextSiblingLevel(AControlInfo: TControlInfo;
      ADirection: TControlPopulatorDirection;
      ABreak: Boolean): TControlPopulator; overload;
    function NextSiblingLevelWithBreak(AControlInfo: TControlInfo; AGroupName: string=''): TControlPopulator; overload;
    function NextSiblingLevelWithBreak(AControlInfo: TControlInfo; ADirection: TControlPopulatorDirection;
      AGroupName: string=''): TControlPopulator; overload;
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
    function BreakColumn: TControlPopulator; overload;
    function Break: TControlPopulator; overload;
    function Break(AIncTopOrLeft: Single): TControlPopulator; overload;
    function BreakLine(AIncTop: Single): TControlPopulator; overload;
    function BreakColumn(AIncLeft: Single): TControlPopulator; overload;
    {$IFDEF FRAMEWORK_FMX}function WithOwnerAndParent(AOwner: TComponent; AParent: TFmxObject): TControlPopulator;
    {$ELSE}function WithOwnerAndParent(AOwner: TComponent; AParent: TWinControl): TControlPopulator;
    {$ENDIF}
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
    function CenterControlsInParentVertically(
      const AControlNames: array of string): TControlPopulator;
    function CenterControlsInParentHorizontally(
      const AControlNames: array of string): TControlPopulator;
    function RecalcParentHeight(AExtraHeight: Single = 0): TControlPopulator;
    function RecalcParentWidth(AExtraWidth: Single = 0): TControlPopulator;
    function RecalcParentSize(AExtraHeight: Single = 0; AExtraWidth: Single = 0): TControlPopulator;
    function CopyHeight(const AControlNames,
      AReferenceGroup: array of string): TControlPopulator;
    function CopyWidth(const AControlNames,
      AReferenceGroup: array of string): TControlPopulator;
    function CopySize(const AControlNames,
      AReferenceGroup: array of string): TControlPopulator;
    property NamedControls[const AName: string]: TControl read GetNamedControl;
    property ContentWidth: Single read GetContentWidth;
    property ContentHeight: Single read GetFContentHeight;
    property CurrentLevel: TControlPopulatorLevel read GetCurrenteLevel;
    property Controls: TControlList read GetControls;
    property Registry: TComponentRegistry read GetComponentRegistry;
    property Items[const AName: string]: TControl read GetItem; default;
  end;

implementation

uses
  {$IFDEF FPC}Graphics, ComCtrls,
  {$ELSE}
    {$IFDEF FRAMEWORK_FMX} Fmx.Graphics,
    {$ELSE}
    Vcl.ComCtrls,
    {$ENDIF}
  {$ENDIF} Math, Types, Messages;

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
  Result.Align := TOptionalAlign.None;
  Result.Caption := TOptionalString.None;
  Result.Text := TOptionalString.None;
  Result.Top := TOptionalSingle.None;
  Result.Left := TOptionalSingle.None;
  Result.SetupProc := nil;
  Result.OnClick := nil;
end;

class function TControlInfo.Create(AControl: TControl): TControlInfo;
begin
  Result.Control := AControl;
  Result.ControlClass := TControlClass(AControl.ClassType);
  Result.Name := AControl.Name;
  Result.Height := -1;
  Result.Width := -1;
  Result.Align := TOptionalAlign.None;
  Result.Caption := TOptionalString.None;
  Result.Text := TOptionalString.None;
  Result.Top := TOptionalSingle.None;
  Result.Left := TOptionalSingle.None;
  Result.SetupProc := nil;
  Result.OnClick := nil;
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
  begin
    {$IFDEF FRAMEWORK_FMX}
    if Result is TPresentedTextControl then
      TPresentedTextControl(Result).Text := Caption.Value;
    if Result is TTextControl then
      TTextControl(Result).Text := Caption.Value;
    {$ELSE}
    TProtectedControl(Result).Caption := Caption.Value;
    {$ENDIF}
  end;

  if Text.HasValue then
  begin
    {$IFDEF FRAMEWORK_FMX}
    if Result is TPresentedTextControl then
      TPresentedTextControl(Result).Text := Text.Value;
    if Result is TTextControl then
      TTextControl(Result).Text := Text.Value;
    {$ELSE}
    TProtectedControl(Result).Text := Text.Value;
    {$ENDIF}
  end;

  if Align.HasValue then
    Result.Align := Align.Value;

  if Width >= 0 then
    Result.Width := {$IFDEF FRAMEWORK_FMX}Width{$ELSE}Trunc(Width){$ENDIF};

  if Height >= 0 then
    Result.Height := {$IFDEF FRAMEWORK_FMX}Height{$ELSE}Trunc(Height){$ENDIF};

  if Top.HasValue then
  begin
    {$IFDEF FRAMEWORK_FMX}
    Result.Position.Y := Top.Value;
    {$ELSE}
    Result.Top := Trunc(Top.Value);
    {$ENDIF}
  end;

  if Left.HasValue then
  begin
    {$IFDEF FRAMEWORK_FMX}
    Result.Position.X := Left.Value;
    {$ELSE}
    Result.Left := Trunc(Left.Value);
    {$ENDIF}
  end;

  TProtectedControl(Result).OnClick := OnClick;

  if Assigned(SetupProc) then
    SetupProc(Result);
end;

function TControlInfo.WithAlign(
  AAlign: {$IFDEF FRAMEWORK_FMX}TAlignLayout{$ELSE}TAlign{$ENDIF}): TControlInfo;
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

function TControlInfo.WithName(AName: string): TControlInfo;
begin
  Result := Self;
  Result.Name := AName;
end;

function TControlInfo.WithOnClick(AOnClick: TNotifyEvent): TControlInfo;
begin
  Result := Self;
  Result.OnClick := AOnClick;
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

function TControlGridPopulator.GetControls: TControlList;
begin
  Result := FComponentRegistry.FControls;
end;

function TControlGridPopulator.GetNamedControl(const AName: string): TControl;
begin
  if not FComponentRegistry.TryGetControl(AName, Result) then
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

constructor TControlGridPopulator.Create(AComponentRegistryName: string);
begin
  FGrid := nil;
  FFiller := nil;
  FComponentRegistryName := AComponentRegistryName;
  FComponentRegistry := TComponentRegistry.ForContext(AComponentRegistryName);
end;

destructor TControlGridPopulator.Destroy;
begin
  FComponentRegistry.ReleaseContext(FComponentRegistryName);
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
  FComponentRegistry.AddComponent(AControl, AControl.Name);

  ControlGridItem := TControlGridItem.Create(AControl);
  Settings := TGridCellSettings.Create(0, 0);

  try
    if Assigned(FOnControlPopulate) then
      FOnControlPopulate(AControl, Controls.Count-1, Settings);
    if Assigned(AProc) then
      AProc(AControl, Controls.Count-1, Settings);

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

    while ComponentRegistry.FNamedComponents.ContainsKey(Candidate) do
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

function TControlPopulator.Break: TControlPopulator;
begin
  Result := Self;
  if CurrentLevel.Direction = cpdHorizontal then
    BreakLine;
  if CurrentLevel.Direction = cpdVertical then
    BreakColumn;
end;

function TControlPopulator.Break(AIncTopOrLeft: Single): TControlPopulator;
begin
  Result := Break;
  if CurrentLevel.Direction = cpdHorizontal then
    IncTop(AIncTopOrLeft);
  if CurrentLevel.Direction = cpdVertical then
    IncLeft(AIncTopOrLeft);
end;

function TControlPopulator.BreakColumn(AIncLeft: Single): TControlPopulator;
begin
  Result := BreakColumn;
  IncLeft(AIncLeft);
end;

function TControlPopulator.BreakColumn: TControlPopulator;
begin
  Result := Self;
  CurrentLevel.CurrentLeft := CurrentLevel.CurrentLeft + CurrentLevel.MaxControlWidth;
  CurrentLevel.CurrentTop := CurrentLevel.InitialTop;
  CurrentLevel.MaxControlWidth := 0;
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

function TControlPopulator.CenterControlsInParentVertically(
  const AControlNames: array of string): TControlPopulator;
var
  TargetBounds: TControlGroupBounds;
  ParentCtrl: TControl;
  ParentHeight: Single;
  TargetCenterY, ParentCenterY, DeltaY: Single;

  {$IFDEF FRAMEWORK_FMX}
  function GetParentClientHeight(AControl: TControl): Single;
  begin
    if AControl.Parent is TForm then
      Result := TForm(AControl.Parent).ClientHeight
    else if AControl.Parent is TControl then
      Result :=
        TControl(AControl.Parent).Height
        - TControl(AControl.Parent).Padding.Top
        + TControl(AControl.Parent).Padding.Bottom
    else
      Result := 0; // não tem dimensão
  end;
  {$ENDIF}

begin
  Result := Self;

  if Length(AControlNames) = 0 then
    Exit;

  // pega o parent do primeiro controle da lista
  ParentCtrl := NamedControls[AControlNames[0]];
  if not Assigned(ParentCtrl) or not Assigned(ParentCtrl.Parent) then
    Exit;

  TargetBounds := GetControlsBounds(AControlNames);
  {$IFDEF FRAMEWORK_FMX}
  ParentHeight := GetParentClientHeight(ParentCtrl);
  {$ELSE}
  ParentHeight := ParentCtrl.Parent.ClientHeight;
  {$ENDIF}

  TargetCenterY := TargetBounds.Top + (TargetBounds.Height / 2);
  ParentCenterY := ParentHeight / 2;

  DeltaY := ParentCenterY - TargetCenterY;

  MoveControls(AControlNames, 0, DeltaY);
end;

function TControlPopulator.CenterControlsInParentHorizontally(
  const AControlNames: array of string): TControlPopulator;
var
  TargetBounds: TControlGroupBounds;
  ParentCtrl: TControl;
  ParentWidth: Single;
  TargetCenterX, ParentCenterX, DeltaX: Single;

  {$IFDEF FRAMEWORK_FMX}
  function GetParentClientWidth(AControl: TControl): Single;
  begin
    if AControl.Parent is TForm then
      Result := TForm(AControl.Parent).ClientHeight
    else if AControl.Parent is TControl then
      Result :=
        TControl(AControl.Parent).Height
        - TControl(AControl.Parent).Padding.Top
        + TControl(AControl.Parent).Padding.Bottom
    else
      Result := 0; // não tem dimensão
  end;
  {$ENDIF}

begin
  Result := Self;

  if Length(AControlNames) = 0 then
    Exit;

  ParentCtrl := NamedControls[AControlNames[0]];
  if not Assigned(ParentCtrl) or not Assigned(ParentCtrl.Parent) then
    Exit;

  TargetBounds := GetControlsBounds(AControlNames);
  {$IFDEF FRAMEWORK_FMX}
  ParentWidth := GetParentClientWidth(ParentCtrl);
  {$ELSE}
  ParentWidth := ParentCtrl.Parent.ClientWidth;
  {$ENDIF}

  TargetCenterX := TargetBounds.Left + (TargetBounds.Width / 2);
  ParentCenterX := ParentWidth / 2;

  DeltaX := ParentCenterX - TargetCenterX;

  MoveControls(AControlNames, DeltaX, 0);
end;

constructor TControlPopulator.Create(ARegistryContext: string);
begin
  FRegistryContextHandle := TRegistryContextHandle.Create(ARegistryContext);
  FGroups := TControlGroupMap.Create;
  FLevelStack := TControlPopulatorLevelStack.Create(True);
  FLevelStack.Add(TControlPopulatorLevel.Create);
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

    while Registry.FNamedComponents.ContainsKey(Candidate) do
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

  Registry.AddComponent(Control, Control.Name);

  // caso especial: TTabSheet / TPageControl
  {$IFDEF FRAMEWORK_FMX}
  if (Control is TTabItem) and (CurrentLevel.Parent is TTabControl) then
  begin
    TTabItem(Control).Parent := TTabControl(CurrentLevel.Parent);
  end;
  {$ELSE}
  if (Control is TTabSheet) and (CurrentLevel.Parent is TPageControl) then
  begin
    TTabSheet(Control).Parent := nil;
    TTabSheet(Control).PageControl := TPageControl(CurrentLevel.Parent);
  end;
  {$ENDIF}

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
  CurrentLevel.MaxControlWidth := 0;

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
  ABreak: Boolean): TControlPopulator;
begin
  Result := NextSiblingLevel(ADirection, '', ABreak);
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

function TControlPopulator.NextSiblingLevel(ABreak: Boolean): TControlPopulator;
begin
  Result := NextSiblingLevel('', ABreak);
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
      {$IFNDEF FRAMEWORK_FMX}
      if (SubLevel.Parent is TPanel) and TPanel(SubLevel.Parent).AutoSize then
      begin
        SubLevel.Parent.HandleNeeded;
        SubLevel.Parent.Invalidate;
        SubLevel.Parent.Update;
      end;
      {$ENDIF}

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
        SuperLevel.CurrentLeft := Bounds.Right + SuperLevel.HorizontalSpace;
        SuperLevel.MaxControlHeight := Max(SuperLevel.MaxControlHeight, Bounds.Height + SuperLevel.VerticalSpace);
      end;
    cpdVertical:
      begin
        SuperLevel.CurrentTop := Bounds.Bottom + SuperLevel.VerticalSpace;
        SuperLevel.MaxControlWidth := Max(SuperLevel.MaxControlWidth, Bounds.Width + SuperLevel.HorizontalSpace);
      end;
  end;

  MoveTopLeftAfterBound(GetGroupBounds(SuperLevel.GroupName));

  FLevelStack.Delete(FLevelStack.Count - 1); // remove nível atual
  Result := Self;
end;

function TControlPopulator.RecalcParentHeight(AExtraHeight: Single): TControlPopulator;
begin
  Result := Self;
  {$IFDEF FRAMEWORK_FMX}
  CurrentLevel.Parent.Height :=
    GetControlsBounds([CurrentLevel.GroupName]).Height
    + AExtraHeight;
  {$ELSE}
  CurrentLevel.Parent.Height := Trunc(
    GetControlsBounds([CurrentLevel.GroupName]).Height
    + AExtraHeight
  );
  {$ENDIF}
end;

function TControlPopulator.RecalcParentSize(AExtraHeight,
  AExtraWidth: Single): TControlPopulator;
begin
  Result := Self;
  RecalcParentHeight(AExtraHeight);
  RecalcParentWidth(AExtraWidth);
end;

function TControlPopulator.RecalcParentWidth(
  AExtraWidth: Single): TControlPopulator;
begin
  Result := Self;
  {$IFDEF FRAMEWORK_FMX}
  CurrentLevel.Parent.Width :=
    GetControlsBounds([CurrentLevel.GroupName]).Width
    + AExtraWidth;
  {$ELSE}
  CurrentLevel.Parent.Width := Trunc(
    GetControlsBounds([CurrentLevel.GroupName]).Width
    + AExtraWidth
  );
  {$ENDIF}
end;

function TControlPopulator.CopyHeight(const AControlNames, AReferenceGroup: array of string): TControlPopulator;
var
  RefBounds: TControlGroupBounds;
  Name: string;
  Ctrl: TControl;
begin
  Result := Self;

  if (Length(AControlNames) = 0) or (Length(AReferenceGroup) = 0) then
    Exit;

  // calcula o bounds dos controles de referência
  RefBounds := GetControlsBounds(AReferenceGroup);

  for Name in AControlNames do
  begin
    if not Registry.NamedComponents.TryGetValue(Name, TComponent(Ctrl)) then
      raise Exception.CreateFmt('Controle "%s" não encontrado.', [Name]);

    {$IFDEF FRAMEWORK_FMX}
    Ctrl.Height := RefBounds.Height;
    {$ELSE}
    Ctrl.Height := Round(RefBounds.Height);
    {$ENDIF}
  end;
end;

function TControlPopulator.CopyWidth(const AControlNames, AReferenceGroup: array of string): TControlPopulator;
var
  RefBounds: TControlGroupBounds;
  Name: string;
  Ctrl: TControl;
begin
  Result := Self;

  if (Length(AControlNames) = 0) or (Length(AReferenceGroup) = 0) then
    Exit;

  // calcula o bounds dos controles de referência
  RefBounds := GetControlsBounds(AReferenceGroup);

  for Name in AControlNames do
  begin
    if not Registry.NamedComponents.TryGetValue(Name, TComponent(Ctrl)) then
      raise Exception.CreateFmt('Controle "%s" não encontrado.', [Name]);

    {$IFDEF FRAMEWORK_FMX}
    Ctrl.Width := RefBounds.Width;
    {$ELSE}
    Ctrl.Width := Round(RefBounds.Width);
    {$ENDIF}
  end;
end;

function TControlPopulator.CopySize(const AControlNames, AReferenceGroup: array of string): TControlPopulator;
var
  RefBounds: TControlGroupBounds;
  Name: string;
  Ctrl: TControl;
begin
  Result := Self;

  if (Length(AControlNames) = 0) or (Length(AReferenceGroup) = 0) then
    Exit;

  // calcula o bounds dos controles de referência
  RefBounds := GetControlsBounds(AReferenceGroup);

  for Name in AControlNames do
  begin
    if not Registry.NamedComponents.TryGetValue(Name, TComponent(Ctrl)) then
      raise Exception.CreateFmt('Controle "%s" não encontrado.', [Name]);

    {$IFDEF FRAMEWORK_FMX}
    Ctrl.Width  := RefBounds.Width;
    Ctrl.Height := RefBounds.Height;
    {$ELSE}
    Ctrl.Width  := Round(RefBounds.Width);
    Ctrl.Height := Round(RefBounds.Height);
    {$ENDIF}
  end;
end;

function TControlPopulator.GetNamedControl(const AName: string): TControl;
begin
  if not Registry.NamedComponents.TryGetValue(AName, TComponent(Result)) then
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

function TControlPopulator.GetComponentRegistry: TComponentRegistry;
begin
  Result := FRegistryContextHandle.GetRegistry;
end;

function TControlPopulator.GetContentWidth: Single;
begin
  Result := GetGroupBounds(FLevelStack.First.GroupName).Width;
end;

function TControlPopulator.GetControl(const AName: string): TControl;
begin
  Result := Registry.GetControl(AName);
end;

{$IFDEF FPC}generic{$ENDIF}
function TControlPopulator.GetControl<T>(const AName: string): T;
begin
  Result := Registry.GetControl<T>(AName);
end;

function TControlPopulator.GetControls: TControlList;
begin
  Result := Registry.Controls;
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

    if Registry.NamedComponents.TryGetValue(Name, TComponent(Control)) then
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

function TControlPopulator.GetItem(const AName: string): TControl;
begin
  Result := Self.GetControl(AName);
end;

function TControlPopulator.MoveControls(const AControlNames: array of string;
  const ADX, ADY: Single): TControlPopulator;
var
  Name: string;
  Ctrl: TControl;
  L, T: Single;
begin
  Result := Self;

  for Name in AControlNames do
  begin
    if not Registry.NamedComponents.TryGetValue(Name, TComponent(Ctrl)) then
      raise Exception.CreateFmt('Controle "%s" não encontrado.', [Name]);

    {$IFDEF FRAMEWORK_FMX}
    L := Ctrl.Position.X;
    T := Ctrl.Position.Y;
    {$ELSE}
    L := Ctrl.Left;
    T := Ctrl.Top;
    {$ENDIF}

    L := L + ADX;
    T := T + ADY;

    {$IFDEF FRAMEWORK_FMX}
    Ctrl.Position.X := L;
    Ctrl.Position.Y := T;
    {$ELSE}
    Ctrl.Left := Round(L);
    Ctrl.Top := Round(T);
    {$ENDIF}
  end;
end;

procedure TControlPopulator.MoveTopLeftAfterBound(ABounds: TControlGroupBounds);
begin
  if CurrentLevel.Direction = cpdHorizontal then
  begin
    CurrentLevel.CurrentLeft := CurrentLevel.CurrentLeft + ABounds.Width + CurrentLevel.HorizontalSpace;
    CurrentLevel.MaxControlHeight := Max(CurrentLevel.MaxControlHeight, ABounds.Height + CurrentLevel.VerticalSpace);
  end;

  if CurrentLevel.Direction = cpdVertical then
  begin
    CurrentLevel.CurrentTop := CurrentLevel.CurrentTop + ABounds.Height + CurrentLevel.VerticalSpace;
    CurrentLevel.MaxControlWidth := Max(CurrentLevel.MaxControlWidth, ABounds.Width + CurrentLevel.HorizontalSpace);
  end;
end;

procedure TControlPopulator.MoveTopLeftAfterControl(AControl: TControl);
var
  W, H: Single;
  {$IFDEF FRAMEWORK_FMX}
  AlignNone, AlignTop, AlignLeft: TAlignLayout;
  {$ELSE}
  AlignNone, AlignTop, AlignLeft: TAlign;
  {$ENDIF}
begin
  {$IFDEF FRAMEWORK_FMX}
  AlignNone := TAlignLayout.None;
  AlignTop  := TAlignLayout.Top;
  AlignLeft := TAlignLayout.Left;
  {$ELSE}
  AlignNone := alNone;
  AlignTop  := alTop;
  AlignLeft := alLeft;
  {$ENDIF}

  // largura/altura como Single para unificar cálculo
  W := AControl.Width;
  H := AControl.Height;

  if AControl.Align = AlignNone then
  begin
    if CurrentLevel.Direction = cpdHorizontal then
    begin
      CurrentLevel.CurrentLeft := CurrentLevel.CurrentLeft + W + CurrentLevel.HorizontalSpace;
      CurrentLevel.MaxControlHeight := Max(CurrentLevel.MaxControlHeight, H + CurrentLevel.VerticalSpace);
    end;

    if CurrentLevel.Direction = cpdVertical then
    begin
      CurrentLevel.CurrentTop := CurrentLevel.CurrentTop + H + CurrentLevel.VerticalSpace;
      CurrentLevel.MaxControlWidth := Max(CurrentLevel.MaxControlWidth, W + CurrentLevel.HorizontalSpace);
    end;
  end;

  if AControl.Align = AlignTop then
    CurrentLevel.CurrentTop := CurrentLevel.CurrentTop + H + CurrentLevel.VerticalSpace;

  if AControl.Align = AlignLeft then
    SetTopLeft(
      CurrentLevel.CurrentTop,
      CurrentLevel.CurrentLeft + W + CurrentLevel.HorizontalSpace
    );
end;

function TControlPopulator.NextSiblingLevel(ADirection: TControlPopulatorDirection;
  AGroupName: string; ABreak: Boolean): TControlPopulator;
begin
  Result := PreviousLevel;
  if ABreak then
    Break;
  NextLevel(ADirection, AGroupName);
end;

function TControlPopulator.NextSiblingLevel(AGroupName: string;
  ABreak: Boolean): TControlPopulator;
begin
  Result := PreviousLevel;
  if ABreak then
    Break;
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
  CurrentLevel.HorizontalSpace := AHorizontalSpace;
end;

function TControlPopulator.SetLeft(AControlName: string): TControlPopulator;
var
  L: Single;
begin
  {$IFDEF FRAMEWORK_FMX}
  L := NamedControls[AControlName].Position.X;
  {$ELSE}
  L := NamedControls[AControlName].Left;
  {$ENDIF}

  Result := SetLeft(L);
end;

function TControlPopulator.SetLeft(ALeft: Single): TControlPopulator;
begin
  Result := Self;
  CurrentLevel.CurrentLeft := ALeft;
  CurrentLevel.InitialLeft := ALeft;
  CurrentLevel.MaxControlHeight := 0;
  CurrentLevel.MaxControlWidth := 0;
end;

function TControlPopulator.SetSpace(AVerticalSpace, AHorizontalSpace: Single): TControlPopulator;
begin
  Result := Self;
  CurrentLevel.VerticalSpace := AVerticalSpace;
  CurrentLevel.HorizontalSpace := AHorizontalSpace;
end;

function TControlPopulator.SetTop(ATop: Single): TControlPopulator;
begin
  Result := Self;
  CurrentLevel.CurrentTop := ATop;
  CurrentLevel.InitialTop := ATop;
  CurrentLevel.MaxControlHeight := 0;
  CurrentLevel.MaxControlWidth := 0;
end;

function TControlPopulator.SetTop(AControlName: string): TControlPopulator;
var
  T: Single;
begin
  {$IFDEF FRAMEWORK_FMX}
  T := NamedControls[AControlName].Position.Y;
  {$ELSE}
  T := NamedControls[AControlName].Top;
  {$ENDIF}

  Result := SetTop(T);
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
  L, T, W, H: Single;
begin
  Result := Self;

  Control := NamedControls[AControlName];

  {$IFDEF FRAMEWORK_FMX}
  L := Control.Position.X;
  T := Control.Position.Y;
  W := Control.Width;
  H := Control.Height;
  {$ELSE}
  L := Control.Left;
  T := Control.Top;
  W := Control.Width;
  H := Control.Height;
  {$ENDIF}

  SetTop(T);
  SetLeft(L);

  if APosition = rpBelow then
    SetTop(CurrentLevel.CurrentTop + H + CurrentLevel.VerticalSpace);

  if APosition = rpRight then
    SetLeft(CurrentLevel.CurrentLeft + W + CurrentLevel.HorizontalSpace);
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
        SetTop(Bounds.Bottom + CurrentLevel.VerticalSpace);
      end;
    rpRight:
      begin
        SetLeft(Bounds.Right + CurrentLevel.HorizontalSpace);
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
    SetTop(CurrentLevel.CurrentTop + Bounds.Height + CurrentLevel.VerticalSpace);

  if APosition = rpRight then
    SetLeft(CurrentLevel.CurrentLeft + Bounds.Width + CurrentLevel.HorizontalSpace);
end;

function TControlPopulator.SetVerticalSpace(
  AVerticalSpace: Single): TControlPopulator;
begin
  Result := Self;
  CurrentLevel.VerticalSpace := AVerticalSpace
end;

{$IFNDEF FRAMEWORK_FMX}
function TControlPopulator.WithOwnerAndParent(AOwner: TComponent;
  AParent: TWinControl): TControlPopulator;
begin
  Result := Self;
  FOwner := AOwner;
  CurrentLevel.Parent := AParent;
end;
{$ENDIF}

{$IFDEF FRAMEWORK_FMX}
function TControlPopulator.WithOwnerAndParent(AOwner: TComponent;
  AParent: TFmxObject): TControlPopulator;
begin
  Result := Self;
  FOwner := AOwner;
  CurrentLevel.Parent := TControl(AParent);
end;
{$ENDIF}

function TControlPopulator.WithParent(AParent: TWinControl): TControlPopulator;
begin
  Result := Self;
  CurrentLevel.Parent := AParent;
end;

function TControlPopulator.NextLevel(
  AControlInfo: TControlInfo;
  AGroupName: string
): TControlPopulator;
var
  Control: TControl;
  OwnerToUse: TComponent;
  IsTabChild: Boolean;
begin
  Result := Self;

  IsTabChild :=
    AControlInfo.ControlClass.InheritsFrom(
      {$IFDEF FRAMEWORK_FMX}TTabItem{$ELSE}TTabSheet{$ENDIF}
    ) and
    (CurrentLevel.Parent is
      {$IFDEF FRAMEWORK_FMX}TTabControl{$ELSE}TPageControl{$ENDIF}
    );

  if IsTabChild then
    OwnerToUse := CurrentLevel.Parent
  else
    OwnerToUse := FOwner;

  Control := AControlInfo.CreateControl(OwnerToUse, CurrentLevel.Parent, AControlInfo.Name);
  AddControl(TControlInfo.Create(Control));
  NextLevel(AGroupName);

  WithParent(
    {$IFDEF FRAMEWORK_FMX}Control
    {$ELSE}TWinControl(Control)
    {$ENDIF}
  );

  SetTopLeft(0, 0);
end;

function TControlPopulator.NextLevel(AControlInfo: TControlInfo;
  ADirection: TControlPopulatorDirection; AGroupName: string): TControlPopulator;
begin
  Result := NextLevel(AControlInfo, AGroupName);
  SetDirection(ADirection);
end;

function TControlPopulator.NextSiblingLevel(AControlInfo: TControlInfo;
  AGroupName: string; ABreak: Boolean): TControlPopulator;
begin
  Result := PreviousLevel;
  if ABreak then
    Break;
  NextLevel(AControlInfo, AGroupName);
end;

function TControlPopulator.NextSiblingLevel(AControlInfo: TControlInfo;
  ADirection: TControlPopulatorDirection; AGroupName: string;
  ABreak: Boolean): TControlPopulator;
begin
  Result := PreviousLevel;
  if ABreak then
    Break;
  NextLevel(AControlInfo, ADirection, AGroupName);
end;

function TControlPopulator.NextSiblingLevel(AControlInfo: TControlInfo;
  ABreak: Boolean): TControlPopulator;
begin
  Result := NextSiblingLevel(AControlInfo, '', ABreak);
end;

function TControlPopulator.NextSiblingLevel(AControlInfo: TControlInfo;
  ADirection: TControlPopulatorDirection;
  ABreak: Boolean): TControlPopulator;
begin
  Result := NextSiblingLevel(AControlInfo, ADirection, '', ABreak);
end;

function TControlPopulator.NextSiblingLevelWithBreak(
  AControlInfo: TControlInfo; ADirection: TControlPopulatorDirection;
  AGroupName: string): TControlPopulator;
begin
  Result := NextSiblingLevel(AControlInfo, ADirection, AGroupName, True);
end;

function TControlPopulator.NextSiblingLevelWithBreak(
  AControlInfo: TControlInfo; AGroupName: string): TControlPopulator;
begin
  Result := NextSiblingLevel(AControlInfo, AGroupName, True);
end;

{ TControlGroupBounds }

function TControlGroupBounds.Height: Single;
begin
  Result := Bottom - Top;
end;

procedure TControlGroupBounds.Include(Control: TControl);
var
  L, T, W, H: Single;
begin
  if Control = nil then
    Exit;

  W := Control.Width;
  H := Control.Height;

  {$IFDEF FRAMEWORK_FMX}
  L := Control.Position.X;
  T := Control.Position.Y;
  {$ELSE}
  L := Control.Left;
  T := Control.Top;
  {$ENDIF}

  if Left > L then
    Left := L;
  if Top > T then
    Top := T;
  if Right < L + W then
    Right := L + W;
  if Bottom < T + H then
    Bottom := T + H;
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
  Result.VerticalSpace := VerticalSpace;
  Result.HorizontalSpace := HorizontalSpace;
  Result.MaxControlHeight := MaxControlHeight;
  Result.MaxControlWidth := MaxControlWidth;
end;

constructor TControlPopulatorLevel.Create;
begin
  Direction := cpdHorizontal;
  InitialTop := 0;
  InitialLeft := 0;
  CurrentTop := 0;
  CurrentLeft := 0;
  MaxControlHeight := 0;
  MaxControlWidth := 0;
  VerticalSpace := 0;
  HorizontalSpace := 0;
  Inc(FGroupCounter);
  GroupName := '__LEVEL_GROUP_' + IntToStr(FGroupCounter - 1) + '__';
end;

{ TAutoSizeContainer }

constructor TAutoSizeContainer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFNDEF FRAMEWORK_FMX}
  BevelOuter := bvNone;
  AutoSize := True;
  {$ENDIF}
end;

{ TGridContainer }

constructor TGridContainer.Create(AOwner: TComponent; AComponentRegistryName: string);
begin
  inherited Create(AOwner);
  FGrid := nil;
  FGridPopulator := TControlGridPopulator.Create(AComponentRegistryName);
end;

destructor TGridContainer.Destroy;

begin
  FGridPopulator.Free;
  inherited;
end;

{ TComponentRegistry }

procedure TComponentRegistry.AddComponent(AComponent: TComponent; const AName: string);
begin
  if AComponent = nil then
    Exit;

  Self.RegisterComponentForNotification(AComponent);

  FComponents.Add(AComponent);

  if AComponent is TControl then
    FControls.Add(TControl(AComponent));

  if AName <> '' then
  begin
    if FNamedComponents.ContainsKey(AName) then
      raise Exception.CreateFmt('Já existe um componente registrado com o nome "%s".', [AName]);
    FNamedComponents.Add(AName, AComponent);

    if AComponent is TControl then
      FNamedControls.Add(AName, TControl(AComponent))
  end;
end;

procedure TComponentRegistry.AddControl(AControl: TControl;
  const AName: string);
begin
  AddComponent(AControl, AName);
end;

class procedure TComponentRegistry.ClearAll;
var
  Entry: TComponentRegistryEntry;
begin
  if not Assigned(FInstances) then
    Exit;

  for Entry in FInstances.Values do
    Entry.Registry.Free;

  FInstances.Free;
  FInstances := nil;
end;

constructor TComponentRegistry.Create;
begin
  raise Exception.Create('Use TComponentRegistry.ForContext');
end;

constructor TComponentRegistry.CreatePrivate;
begin
  inherited Create;
  FNotifier := TRegistryNotifier.Create(Self);
  FComponents := TComponentList.Create;
  FControls := TControlList.Create;
  FNamedComponents := TStrComponentDictionary.Create;
  FNamedControls := TStrControlDictionary.Create;
end;

destructor TComponentRegistry.Destroy;
begin
  FComponents.Free;
  FControls.Free;
  FNamedComponents.Free;
  FNamedControls.Free;
  FNotifier.Free;
  inherited;
end;

// ATENÇÃO: Ao chamar ForContext, é obrigatório chamar TComponentRegistry.ReleaseContext
// ao final do uso do objeto, para garantir a liberação da memória se não
// houver mais referencias ao objeto na lista
class function TComponentRegistry.ForContext(const AKey: string): TComponentRegistry;
var
  Entry: TComponentRegistryEntry;
begin
  if FInstances = nil then
    FInstances := TStrComponentRegistryEntryDictionary.Create;

  if FInstances.TryGetValue(AKey, Entry) then
  begin
    Inc(Entry.RefCount);
    FInstances[AKey] := Entry;
    Exit(Entry.Registry);
  end;

  Entry.Registry := TComponentRegistry.CreatePrivate;
  Entry.RefCount := 1;
  FInstances.Add(AKey, Entry);
  Result := Entry.Registry;
end;

function TComponentRegistry.GetComponent(const AName: string): TComponent;
begin
  if not FNamedComponents.TryGetValue(AName, Result) then
    raise Exception.CreateFmt('Componente com o nome "%s" não encontrado.', [AName]);
end;

{$IFDEF FPC}generic{$ENDIF}
function TComponentRegistry.GetComponent<T>(const AName: string): T;
begin
  Result := T(GetComponent(AName));
end;

class function TComponentRegistry.GetComponentFromContext(const AContextKey,
  AComponentlName: string): TComponent;
var
  Registry: TComponentRegistry;
begin
  Registry := TComponentRegistry.ForContext(AContextKey);
  try
    Result := Registry.GetComponent(AComponentlName);
  finally
    Registry.ReleaseContext(AContextKey);
  end;
end;

{$IFDEF FPC}generic{$ENDIF}
class function TComponentRegistry.GetComponentFromContext<T>(const AContextKey,
  AComponentlName: string): T;
begin
  Result := T(GetComponentFromContext(AContextKey, AComponentlName));
end;

class function TComponentRegistry.GetControlFromContext(const AContextKey,
  AControlName: string): TControl;
var
  Registry: TComponentRegistry;
begin
  Registry := TComponentRegistry.ForContext(AContextKey);
  try
    Result := Registry.GetControl(AControlName);
  finally
    Registry.ReleaseContext(AContextKey);
  end;
end;

{$IFDEF FPC}generic{$ENDIF}
class function TComponentRegistry.GetControlFromContext<T>(const AContextKey,
  AControlName: string): T;
begin
  Result := T(TComponentRegistry.GetControlFromContext(AContextKey, AControlName));
end;

function TComponentRegistry.GetItem(ACompName: string): TComponent;
begin
  Result := GetControl(ACompName);
end;

class function TComponentRegistry.GetContextComponents(const AContext,
  AName: string): TComponent;
begin
  Result := TComponentRegistry.GetComponentFromContext(AContext, AName);
end;

function TComponentRegistry.GetControl(const AName: string): TControl;
begin
  if not FNamedControls.TryGetValue(AName, Result) then
    raise Exception.CreateFmt('Controle com o nome "%s" não encontrado.', [AName]);
end;

{$IFDEF FPC}generic{$ENDIF}
function TComponentRegistry.GetControl<T>(const AName: string): T;
begin
  Result := T(GetControl(AName));
end;

procedure TComponentRegistry.RegisterComponentForNotification(AComp: TComponent);
begin
  AComp.FreeNotification(FNotifier);
end;

class procedure TComponentRegistry.ReleaseContext(const AKey: string);
var
  Entry: TComponentRegistryEntry;
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
      FInstances[AKey] := Entry;
  end;

  if FInstances.Count = 0 then
  begin
    FInstances.Free;
    FInstances := nil;
  end;
end;

function TComponentRegistry.TryGetComponent(const AName: string;
  out AComponent: TComponent): Boolean;
begin
  Result := FNamedComponents.TryGetValue(AName, AComponent);
end;

{$IFDEF FPC}generic{$ENDIF}
function TComponentRegistry.TryGetComponent<T>(const AName: string;
  out AComponent: T): Boolean;
begin
  Result := TryGetComponent(AName, AComponent);
end;

function TComponentRegistry.TryGetControl(const AName: string;
  out AControl: TControl): Boolean;
begin
  Result := FNamedControls.TryGetValue(AName, AControl);
end;

{$IFDEF FPC}generic{$ENDIF}
function TComponentRegistry.TryGetControl<T>(const AName: string;
  out AControl: T): Boolean;
begin
  Result := TryGetControl(AName, AControl);
end;

procedure TComponentRegistry.UnregisterComponentForNotification(AComp: TComponent);
begin
  FComponents.Remove(AComp);

  if AComp is TControl then
    FControls.Remove(TControl(AComp));

  if AComp.Name <> '' then
  begin
    FNamedComponents.Remove(AComp.Name);
    FNamedControls.Remove(AComp.Name);
  end;

  AComp.RemoveFreeNotification(FNotifier);
end;

class function TComponentRegistry.GetContextHandle(
  AKey: string): IRegistryContextHandle;
begin
  Result := TRegistryContextHandle.Create(AKey);
end;

{ TComponentPopulator }

function TComponentPopulator.Add(
  AComponentInfo: TComponentInfo): TComponentPopulator;
var
  Component: TComponent;

  function UniqueName(const ABaseName: string): string;
  var
    Index: Integer;
    Candidate: string;
  begin
    if ABaseName.IsEmpty then
      Exit('');

    Candidate := ABaseName;
    Index := 1;

    while Registry.FNamedComponents.ContainsKey(Candidate) do
    begin
      Candidate := ABaseName + IntToStr(Index);
      Inc(Index);
    end;

    Result := Candidate;
  end;

  function CreateComponent(Info: TComponentInfo): TComponent;
  var
    ComponentName: string;
  begin
    ComponentName := '';
    if not AComponentInfo.Name.IsEmpty then
      ComponentName := UniqueName(AComponentInfo.Name);

    Info := AComponentInfo;

    Result := Info.CreateComponent(FOwner, ComponentName);
  end;

begin
  Result := Self;
  Component := CreateComponent(AComponentInfo);
  Registry.AddComponent(Component, Component.Name);
end;

constructor TComponentPopulator.Create(AComponentRegistryName: string);
begin
  FRegistryContextHandle := TRegistryContextHandle.Create(AComponentRegistryName);
end;

destructor TComponentPopulator.Destroy;
begin
  inherited;
end;

function TComponentPopulator.GetComponent(const AName: string): TComponent;
begin
  Result := Registry.GetComponent(AName);
end;

{$IFDEF FPC}generic{$ENDIF}
function TComponentPopulator.GetComponent<T>(const AName: string): T;
begin
  Result := Registry.GetComponent<T>(AName);
end;

function TComponentPopulator.GetComponentRegistry: TComponentRegistry;
begin
  Result := FRegistryContextHandle.GetRegistry;
end;

function TComponentPopulator.GetComponents: TComponentList;
begin
  Result := Registry.FComponents;
end;

function TComponentPopulator.GetItem(const AName: string): TComponent;
begin
  Result := Self.GetComponent(AName);
end;

function TComponentPopulator.WithOwner(AOwner: TComponent): TComponentPopulator;
begin
  Result := Self;
  FOwner := AOwner;
end;

{ TComponentInfo }

class function TComponentInfo.Create(AComponent: TComponent): TComponentInfo;
begin
  Result.Component := AComponent;
  Result.ComponentClass := TComponentClass(AComponent.ClassType);
  Result.Name := AComponent.Name;
  Result.SetupProc := nil;
end;

function TComponentInfo.CreateComponent(AOwner: TComponent;
  const AControlName: string): TComponent;
begin
  if Assigned(Component) then
    Result := Component
  else
    Result := ComponentClass.Create(AOwner);

  if not AControlName.IsEmpty then
    Result.Name := AControlName;

  if Assigned(SetupProc) then
    SetupProc(Result);
end;

function TComponentInfo.Setup(AProc: TComponentSetupProc): TComponentInfo;
begin
  Result := Self;
  Result.SetupProc := AProc;
end;

function TComponentInfo.WithName(AName: string): TComponentInfo;
begin
  Result := Self;
  Result.Name := AName;
end;

class function TComponentInfo.Create(AClass: TComponentClass;
  const AName: string): TComponentInfo;
begin
  Result.Component := nil;
  Result.ComponentClass := AClass;
  Result.Name := AName;
  Result.SetupProc := nil;
end;

{ TRegistryNotifier }

constructor TRegistryNotifier.Create(AOwner: TComponentRegistry);
begin
  inherited Create(nil);
  FOwnerRegistry := AOwner;
end;

procedure TRegistryNotifier.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    FOwnerRegistry.UnregisterComponentForNotification(AComponent);
end;

{ TContextHandle }

constructor TRegistryContextHandle.Create(const AContextKey: string);
begin
  inherited Create;
  FContextKey := AContextKey;
  FRegistry := TComponentRegistry.ForContext(AContextKey);
end;

destructor TRegistryContextHandle.Destroy;
begin
  TComponentRegistry.ReleaseContext(FContextKey);
  inherited;
end;

function TRegistryContextHandle.GetRegistry: TComponentRegistry;
begin
  Result := FRegistry;
end;

end.

