unit OPCB;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$LONGSTRINGS ON}{$MODESWITCH TYPEHELPERS}{$MODESWITCH ADVANCEDRECORDS}
{$ENDIF}

interface

uses
  {$IFDEF FPC}Controls, ExtCtrls, Menus,
  {$ELSE}
    {$IFDEF FRAMEWORK_FMX}
    FMX.Controls, FMX.StdCtrls, Fmx.Types, FMX.ExtCtrls, FMX.TabControl, FMX.Forms, FMX.Menus, System.Types,
    {$ELSE}
    Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Menus, Types,
    {$ENDIF}
  {$ENDIF}
  Classes, SysUtils, ULayout, Generics.Collections, Generics.Defaults, OPCB.Optionals;

type
  {$IFNDEF FPC}
    {$IFDEF FRAMEWORK_FMX}
    TWinControl = TControl;
    TMenu = TFmxObject;
    {$ENDIF}
  {$ENDIF}

  TProtectedControl = class(TControl);
  TControlClass = class of TControl;
  TWinControlClass = class of TWinControl;
  TMenuClass = class of TMenu;
  TMenuItemClass = class of TMenuItem;
  TControlSetupProc = procedure(AControl: TControl) of object;
  TComponentSetupProc = procedure(AComponent: TComponent) of object;
  TWinControlSetupProc = procedure(AWinControl: TWinControl) of object;
  TMenuSetupProc = procedure(AMenu: TMenu) of object;
  TMenuItemSetupProc = procedure(AMenuItem: TMenuItem) of object;

  TControlSetupProcList = {$IFDEF FPC}specialize{$ENDIF} TList<TControlSetupProc>;

  { TComponentInfo }

  TComponentInfo = class
  private
    FComponent: TComponent;
    FComponentClass: TComponentClass;
    FSetupProc: TComponentSetupProc;
    FName: string;
    FTag: NativeInt;
    FTargetField: Pointer;
  public
    constructor Create(AComponent: TComponent); overload;
    constructor Create(AClass: TComponentClass; const AName: string=''); overload;
    constructor Create(AClass: TComponentClass; const AName: string; out Reference); overload;
    constructor Create(AComponentClass: TComponentClass; out Reference); overload;
    function Assign(out Reference): TComponentInfo; overload;
    function Setup(AProc: TComponentSetupProc): TComponentInfo;
    function WithName(AName: string): TComponentInfo;
    function WithTag(ATag: NativeInt): TComponentInfo;
    function CreateComponent(AOwner: TComponent; const AComponentName: string): TComponent;
    property Component: TComponent read FComponent;
    property ComponentClass: TComponentClass read FComponentClass;
    property SetupProc: TComponentSetupProc read FSetupProc;
    property Name: string read FName;
  end;

  TComponentInfoArray = array of TComponentInfo;

  TComponentInfoHelper = class helper for TComponentInfo
    class function CreateArray(AClass: TComponentClass;
      const ANames: array of string): TComponentInfoArray; overload; static;
  end;

  { TMenuInfo }

  TMenuInfo = class
  private
    FMenu: TMenu;
    FMenuClass: TMenuClass;
    FSetupProc: TMenuSetupProc;
    FName: string;
    FTag: NativeInt;
    FTargetField: Pointer;
  public
    constructor Create(AMenu: TMenu); overload;
    constructor Create(AClass: TMenuClass; const AName: string=''); overload;
    constructor Create(AClass: TMenuClass; const AName: string; out Reference); overload;
    constructor Create(AClass: TMenuClass; out Reference); overload;
    function Assign(out Reference): TMenuInfo; overload;
    function Setup(AProc: TMenuSetupProc): TMenuInfo;
    function WithName(AName: string): TMenuInfo;
    function WithTag(ATag: NativeInt): TMenuInfo;
    function CreateMenu(AOwner: TComponent; const AMenuName: string): TMenu;
    property Menu: TMenu read FMenu;
    property MenuClass: TMenuClass read FMenuClass;
    property SetupProc: TMenuSetupProc read FSetupProc;
    property Name: string read FName;
    property TargetField: Pointer read FTargetField;
  end;

  { TMenuItemInfo }

  TMenuItemInfo = class
  private
    FMenuItem: TMenuItem;
    FMenuItemClass: TMenuItemClass;
    FOnClick: TNotifyEvent;
    FSetupProc: TMenuItemSetupProc;
    FName: string;
    FTag: NativeInt;
    FCaption: TOptionalString;
    FImageIndex: TOptionalInteger;
    FTargetField: Pointer;
  public
    constructor Create(AMenuItem: TMenuItem); overload;
    constructor Create(AClass: TMenuItemClass; const AName: string=''); overload;
    constructor Create(AClass: TMenuItemClass; const AName: string; out Reference); overload;
    constructor Create(AClass: TMenuItemClass; out Reference); overload;
    constructor Create; overload;
    constructor Create(out Reference); overload;
    function Assign(out Reference): TMenuItemInfo; overload;
    function Setup(AProc: TMenuItemSetupProc): TMenuItemInfo;
    function WithName(AName: string): TMenuItemInfo;
    function WithTag(ATag: NativeInt): TMenuItemInfo;
    function WithCaption(ACaption: string): TMenuItemInfo;
    function WithImageIndex(AImageIndex: Integer): TMenuItemInfo;
    function WithOnClick(AOnClick: TNotifyEvent): TMenuItemInfo;
    function CreateMenuItem(AOwner: TComponent; const AMenuItemName: string): TMenuItem;
    property MenuItem: TMenuItem read FMenuItem;
    property MenuItemClass: TMenuItemClass read FMenuItemClass;
    property SetupProc: TMenuItemSetupProc read FSetupProc;
    property Name: string read FName;
    property Caption: TOptionalString read FCaption;
    property ImageIndex: TOptionalInteger read FImageIndex;
    property OnClick: TNotifyEvent read FOnClick;
  end;

  { TCustomControlInfo }

  TCustomControlInfo = class abstract
  protected
    FControl: TControl;
    FControlClass: TControlClass;
    FSetupProcList: TControlSetupProcList;
    FName: string;
    FTag: NativeInt;
    FCaption: TOptionalString;
    FText: TOptionalString;
    FAlign: TOptionalAlign;
    FWidth: Single;
    FHeight: Single;
    FTop: TOptionalSingle;
    FLeft: TOptionalSingle;
    FOnClick: TNotifyEvent;
    FTargetField: Pointer;
  public
    function CreateControl(AOwner: TComponent; AParent: TWinControl;
      const AControlName: string): TControl; virtual; abstract;
    property Control: TControl read FControl;
    property ControlClass: TControlClass read FControlClass;
    property Name: string read FName;
    property Caption: TOptionalString read FCaption;
    property Text: TOptionalString read FText write FText;
    property Align: TOptionalAlign read FAlign write FAlign;
    property Width: Single read FWidth write FWidth;
    property Height: Single read FHeight write FHeight;
    property Top: TOptionalSingle read FTop write FTop;
    property Left: TOptionalSingle read FLeft write FLeft;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

  { TControlInfoBase }

  {$IFDEF FPC}generic{$ENDIF}
  TControlInfoBase<TSelf: class> = class(TCustomControlInfo)
  public
    constructor Create(AControl: TControl); overload;
    constructor Create(AClass: TControlClass; const AName: string=''); overload;
    constructor Create(AClass: TControlClass; const AName: string; out Reference); overload;
    constructor Create(AClass: TControlClass; out Reference); overload;
    destructor Destroy; override;
    function Assign(out Reference): TSelf;
    function Setup(AProc: TControlSetupProc): TSelf; overload;
    function Setup(const AProcs: array of TControlSetupProc): TSelf;  overload;
    function WithAlign(
      AAlign: {$IFDEF FRAMEWORK_FMX}TAlignLayout{$ELSE}TAlign{$ENDIF}): TSelf;
    function WithName(AName: string): TSelf;
    function WithTag(ATag: NativeInt): TSelf;
    function WithWidth(AWidth: Single): TSelf;
    function WithHeight(AHeight: Single): TSelf;
    function WithWidthAndHeight(AWidth: Single; AHeight: Single): TSelf;
    function WithTop(ATop: Single): TSelf;
    function WithLeft(ALeft: Single): TSelf;
    function WithCaption(ACaption: string): TSelf;
    function WithText(AText: string): TSelf;
    function WithOnClick(AOnClick: TNotifyEvent): TSelf;
    function CreateControl(AOwner: TComponent; AParent: TWinControl;
      const AControlName: string): TControl; override;
  end;

  TControlInfo = class;
  TControlInfo =
    class({$IFDEF FPC}specialize{$ENDIF} TControlInfoBase<TControlInfo>)
  end;

  TComponentRegistry = class;

  IRegistryContextHandle = interface
    ['{6EEC3518-D8A0-4E8E-A92A-D5D34E5838C3}']
    function GetRegistry: TComponentRegistry;
    property Registry: TComponentRegistry read GetRegistry;
    procedure ReleaseContext;
  end;

  { TRegistryContextHandle }

  TRegistryContextHandle = class(TInterfacedObject, IRegistryContextHandle)
  private
    FIsReleased: Boolean;
    FContextKey: string;
    FRegistry: TComponentRegistry;
    class var FAutoCounter: Integer;
    class function GenerateAutoKey: string; static;
  public
    constructor Create(const AContextKey: string); overload;
    constructor Create; overload;
    destructor Destroy; override;
    function GetRegistry: TComponentRegistry;
    procedure ReleaseContext;
    property Registry: TComponentRegistry read GetRegistry;
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

  TRegistryNotifier = class(TComponent)
  private
    FOwnerRegistry: TComponentRegistry;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponentRegistry);
  end;

  TRegistryLifetime = (rlTransient, rlPersistent);  // rlTimeout (future)

  TComponentRegistry = class
  private
    class var FInstances: TStrComponentRegistryEntryDictionary;
    class function GetContextComponents(const AContext, AName: string): TComponent; static;
  protected
    class function ForContext(const AKey: string): TComponentRegistry; static;
    class procedure ReleaseContext(const AKey: string); static;
    class procedure Finalize; static;
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
    FRegistryLifetime: TRegistryLifetime;
    constructor CreatePrivate;
    function GetItem(ACompName: string): TComponent;
    procedure SetRegistryLifetime(const Value: TRegistryLifetime);
    procedure CheckRelease;
    function UniqueName(const ABaseName: string): string;
    function GetContextKey: string;
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
    property RegistryLifetime: TRegistryLifetime read FRegistryLifetime write SetRegistryLifetime;
    property ContextKey: string read GetContextKey;
  end;

  TAutoSizeContainer = class(TPanel)
  public
    constructor Create(AOwner: TComponent); override;
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

  TControlsBuilderDirection = (cpdHorizontal, cpdVertical);
  TRelativePosition = (rpRight, rpBelow);
  TGridFillDirection = (gfdRowFirst, gfdColFirst);
  TGridCellStatus = (csEmpty, csOccupied);

  { TGridCellCoord }

  TGridCellCoord = record
    Row: Integer;
    Col: Integer;
    class function Create(ARow, ACol: Integer): TGridCellCoord; static;
  end;

  TCellCordStatusDictionary
    = {$IFDEF FPC}specialize{$ENDIF} TDictionary<TGridCellCoord, TGridCellStatus>;

  TGridAutoExpand = set of (gaeRows, gaeCols);
  TCellStrech = set of (csVertical, csHorizontal);
  TCellPosition = (cpCenter, cpTop, cpRight, cpBottom, cpLeft,
    cpTopRight, cpTopLeft, cpBottomRight, cpBottomLeft);

  { TGridMode }

  TGridMode = class
  private
    FActive: Boolean;
    FAutoExpand: TGridAutoExpand;
    FCellHeight: Single;
    FCellWidth: Single;
    FColSpan: Integer;
    FRows: Integer;
    FCols: Integer;
    FOriginLeft: Single;
    FOriginTop: Single;
    FCurrentCol: Integer;
    FCurrentRow: Integer;
    FColWidths: TIntSingleDictionary;
    FRowHeights: TIntSingleDictionary;
    FColOffsets: TIntSingleDictionary;
    FRowOffsets: TIntSingleDictionary;
    FOccupation: TCellCordStatusDictionary;
    FRowSpan: Integer;
    FDirection: TGridFillDirection;
    FCellStrech: TCellStrech;
    FCellPosition: TCellPosition;
    function GetNextCol: Integer;
    function GetNextRow: Integer;
    procedure SetCellHeight(AValue: Single);
    procedure SetCellPosition(AValue: TCellPosition);
    procedure SetCellStrech(AValue: TCellStrech);
    procedure SetCellWidth(AValue: Single);
    procedure SetColSpan(AValue: Integer);
    procedure SetDirection(AValue: TGridFillDirection);
    procedure SetRowSpan(AValue: Integer);
    function GetColSpanForFill: Integer;
    function GetRowSpanForFill: Integer;
    procedure ExpandIfNeeded(ARow, ACol: Integer);
    procedure ExpandForSpan(ARow, ACol, ARowSpan, AColSpan: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetColWidth(ACol: Integer; AWidth: Single);
    procedure SetRowHeight(ARow: Integer; AHeight: Single);
    procedure SetColOffset(ACol: Integer; AOffset: Single);
    procedure SetRowOffset(ARow: Integer; AOffset: Single);
    function GetColWidth(ACol: Integer): Single;
    function GetRowHeight(ARow: Integer): Single;
    function GetColOffset(ACol: Integer): Single;
    function GetRowOffset(ARow: Integer): Single;
    procedure Activate(ARows, ACols: Integer; AOriginLeft, AOriginTop: Single);
    procedure Inactivate;
    function PeekNext(out NextRow, NextCol: Integer): Boolean;
    function Next: Boolean;
    function Step(out ARowSpan, AColSpan: Integer; out ARow, ACol: Integer; AMark: Boolean = True): Boolean;
    function CalcSpanRect(ARow, ACol, ARowSpan, AColSpan: Integer;
      AHorizontalSpace, AVerticalSpace: Single;
      out R: {$IFDEF FRAMEWORK_FMX}TRectF{$ELSE}TRect{$ENDIF}): Boolean;
    procedure ResetOccupation;
    procedure MarkOccupied(ARow, ACol, ARowSpan, AColSpan: Integer);
    function IsCellFree(ARow, ACol: Integer): Boolean;
    function AdjustRectForCellLayout(
      const CellRect: {$IFDEF FRAMEWORK_FMX}TRectF{$ELSE}TRect{$ENDIF};
        AControlWidth, AControlHeight: Single
      ): {$IFDEF FRAMEWORK_FMX}TRectF{$ELSE}TRect{$ENDIF};
    function CalcCellRectAbsolute(
      ARow, ACol, ARowSpan, AColSpan: Integer;
      AHorizontalSpace, AVerticalSpace: Single;
      out R: {$IFDEF FRAMEWORK_FMX}TRectF{$ELSE}TRect{$ENDIF}
    ): Boolean;
    procedure ResetSpans;
    property Active: Boolean read FActive;
    property Rows: Integer read FRows;
    property Cols: Integer read FCols;
    property CellWidth: Single read FCellWidth write SetCellWidth;
    property CellHeight: Single read FCellHeight write SetCellHeight;
    property RowSpan: Integer write SetRowSpan;
    property ColSpan: Integer write SetColSpan;
    property CurrentRow: Integer read FCurrentRow;
    property CurrentCol: Integer read FCurrentCol;
    property NextRow: Integer read GetNextRow;
    property NextCol: Integer read GetNextCol;
    property Direction: TGridFillDirection read FDirection write SetDirection;
    property AutoExpand: TGridAutoExpand read FAutoExpand write FAutoExpand;
    property CellStrech: TCellStrech read FCellStrech write SetCellStrech;
    property CellPosition: TCellPosition read FCellPosition write SetCellPosition;
  end;

  { TControlsBuilderLevel }

  TControlsBuilderLevel = class
  private
    class var FGroupCounter: Integer;
  public
    Parent: TWinControl;
    GroupName: string;
    Direction: TControlsBuilderDirection;
    InitialTop: Single;
    InitialLeft: Single;
    CurrentTop: Single;
    CurrentLeft: Single;
    MaxControlHeight: Single;
    MaxControlWidth: Single;
    VerticalSpace: Single;
    HorizontalSpace: Single;
    ControlHeight: TOptionalSingle;
    ControlWidth: TOptionalSingle;
    GridMode: TGridMode;
    constructor Create;
    destructor Destroy; override;
    function Clone: TControlsBuilderLevel;
  end;

  TControlsBuilderLevelStack = {$IFDEF FPC}specialize{$ENDIF} TObjectList<TControlsBuilderLevel>;

  TMenuBuilderLevel = class
  public
    {$IFDEF FRAMEWORK_FMX}
    Parent: TFmxObject;
    {$ELSE}
    Parent: TMenuItem;
    {$ENDIF}
  end;

  TMenuBuilderLevelStack = {$IFDEF FPC}specialize{$ENDIF} TObjectList<TMenuBuilderLevel>;

  TComponentBuilder = class;
  TComponentBuilderObjProc = procedure(const ABuiler: TComponentBuilder) of object;
  TComponentBuilderProc = procedure(const ABuiler: TComponentBuilder);

  TComponentBuilder = class
  private
    FOwner: TComponent;
    FRegistryContextHandle: IRegistryContextHandle;
    function GetComponentRegistry: TComponentRegistry;
    function GetComponents: TComponentList;
    function GetItem(const AName: string): TComponent;
  public
    constructor Create(ARegistryContextKey: string=''); overload;
    constructor Create(ARegistryContextHandle: IRegistryContextHandle); overload;
    destructor Destroy; override;
    function External(const AProc: TComponentBuilderObjProc): TComponentBuilder; overload;
    function External(const AProc: TComponentBuilderProc): TComponentBuilder; overload;
    {$IFDEF FPC}generic{$ENDIF}
    function GetComponent<T: TComponent>(const AName: string): T; overload;
    function GetComponent(const AName: string): TComponent; overload;
    function WithOwner(AOwner: TComponent): TComponentBuilder;
    function Add(AComponentInfo: TComponentInfo): TComponentBuilder; overload;
    function Add(AComponentInfos: TComponentInfoArray): TComponentBuilder; overload;
    property Registry: TComponentRegistry read GetComponentRegistry;
    property Items[const AName: string]: TComponent read GetItem; default;
  end;

  TMenuBuilder = class;
  TMenuBuilderObjProc = procedure(const ABuiler: TMenuBuilder) of object;
  TMenuBuilderProc = procedure(const ABuiler: TMenuBuilder);

  TMenuBuilder = class
  private
    FOwner: TComponent;
    FRegistryContextHandle: IRegistryContextHandle;
    FLevelStack: TMenuBuilderLevelStack;
    function GetComponentRegistry: TComponentRegistry;
    function GetCurrenteLevel: TMenuBuilderLevel;
  public
    constructor Create(ARegistryContextKey: string=''); overload;
    constructor Create(ARegistryContextHandle: IRegistryContextHandle); overload;
    destructor Destroy; override;
    function WithOwner(AOwner: TComponent): TMenuBuilder;
    function External(const AProc: TMenuBuilderObjProc): TMenuBuilder; overload;
    function External(const AProc: TMenuBuilderProc): TMenuBuilder; overload;
    function AddMenu(AMenuInfo: TMenuInfo): TMenuBuilder;
    function AddMenuItem(AMenuItemInfo: TMenuItemInfo): TMenuBuilder;
    function SubLevel(AMenuItemInfo: TMenuItemInfo): TMenuBuilder;
    function SuperLevel: TMenuBuilder;
    {$IFDEF FPC}generic{$ENDIF}
    function GetMenu<T: TMenu>(const AName: string): T; overload;
    function GetMenu(const AName: string): TMenu; overload;
    {$IFDEF FPC}generic{$ENDIF}
    function GetMenuItem<T: TMenuItem>(const AName: string): T; overload;
    function GetMenuItem(const AName: string): TMenuItem; overload;
    property CurrentLevel: TMenuBuilderLevel read GetCurrenteLevel;
    property Registry: TComponentRegistry read GetComponentRegistry;
  end;

  TControlsBuilder = class;
  TControlsBuilderObjProc = procedure(const ABuiler: TControlsBuilder) of object;
  TControlsBuilderProc = {$IFNDEF FPC}reference to{$ENDIF} procedure(ABuiler: TControlsBuilder);

  TControlsBuilder = class
  private
    FOwner: TComponent;
    FRegistryContextHandle: IRegistryContextHandle;
    FGroups: TControlGroupMap;
    FLevelStack: TControlsBuilderLevelStack;
    function GetControls: TControlList;
    procedure MoveTopLeftAfterControl(AControl: TControl);
    procedure MoveTopLeftAfterRect(
      const ARect: {$IFDEF FRAMEWORK_FMX}TRectF{$ELSE}TRect{$ENDIF};
      AAlign: {$IFDEF FRAMEWORK_FMX}TAlignLayout{$ELSE}TAlign{$ENDIF});
    procedure MoveTopLeftAfterBound(ABounds: TControlGroupBounds);
    procedure AddControlToGroups(AControl: TControl; const AGroups: array of string);
    function GetGroupBounds(const AGroupName: string): TControlGroupBounds;
    function GetCurrenteLevel: TControlsBuilderLevel;
    function GetContentWidth: Single;
    function GetFContentHeight: Single;
    function GetComponentRegistry: TComponentRegistry;
    function GetItem(const AName: string): TControl;
    procedure SetupControlInfoForGridMode(AControlInfo: TCustomControlInfo);
    function CreateControl(Info: TCustomControlInfo; AOwner: TComponent = nil): TControl;
  public
    constructor Create(ARegistryContextKey: string=''); overload;
    constructor Create(ARegistryContextHandle: IRegistryContextHandle); overload;
    destructor Destroy; override;
    {$IFDEF FPC}generic{$ENDIF}
    function GetControl<T: TControl>(const AName: string): T; overload;
    function GetControl(const AName: string): TControl; overload;
    function GetControlsBounds(AControlsNames: array of string): TControlGroupBounds;
    function External(const AProc: TControlsBuilderObjProc): TControlsBuilder; overload;
    function External(const AProc: TControlsBuilderProc): TControlsBuilder; overload;
    function SetSpace(AVerticalSpace, AHorizontalSpace: Single): TControlsBuilder;
    function SubLevel(AGroupName: string=''): TControlsBuilder; overload;  // xx
    function SubLevel(ADirection: TControlsBuilderDirection;
      AGroupName: string=''): TControlsBuilder; overload;
    function SuperLevel: TControlsBuilder;
    function SiblingSubLevel(AGroupName: string='';
      ABreak: Boolean=False): TControlsBuilder; overload;
    function SiblingSubLevel(ADirection: TControlsBuilderDirection;
      AGroupName: string=''; ABreak: Boolean=False): TControlsBuilder; overload;
    function SiblingSubLevel(ABreak: Boolean=False): TControlsBuilder; overload;
    function SiblingSubLevel(ADirection: TControlsBuilderDirection;
      ABreak: Boolean): TControlsBuilder; overload;
    function SiblingSubLevelWithBreak(AGroupName: string=''): TControlsBuilder; overload;
    function SiblingSubLevelWithBreak(ADirection: TControlsBuilderDirection;
      AGroupName: string=''): TControlsBuilder; overload;
    function SubLevel(AControlInfo: TControlInfo;        // main
      AGroupName: string=''): TControlsBuilder; overload;
    function SubLevel(AControlInfo: TControlInfo;
      ADirection: TControlsBuilderDirection; AGroupName: string=''): TControlsBuilder; overload;
    function SiblingSubLevel(AControlInfo: TControlInfo;
      AGroupName: string=''; ABreak: Boolean=False): TControlsBuilder; overload;
    function SiblingSubLevel(AControlInfo: TControlInfo;
      ADirection: TControlsBuilderDirection;
      AGroupName: string=''; ABreak: Boolean=False): TControlsBuilder; overload;
    function SiblingSubLevel(AControlInfo: TControlInfo;
      ABreak: Boolean=False): TControlsBuilder; overload;
    function SiblingSubLevel(AControlInfo: TControlInfo;
      ADirection: TControlsBuilderDirection;
      ABreak: Boolean): TControlsBuilder; overload;
    function SiblingSubLevelWithBreak(AControlInfo: TControlInfo; AGroupName: string=''): TControlsBuilder; overload;
    function SiblingSubLevelWithBreak(AControlInfo: TControlInfo; ADirection: TControlsBuilderDirection;
      AGroupName: string=''): TControlsBuilder; overload;
    function SetVerticalSpace(AVerticalSpace: Single): TControlsBuilder;
    function SetHorizontalSpace(AHorizontalSpace: Single): TControlsBuilder;
    function SetTopLeft(ATop, ALeft: Single): TControlsBuilder;
    function SetTopLeftNearControl(AControlName: string; APosition: TRelativePosition): TControlsBuilder;
    function SetTopLeftNearControls(AControlsNames: array of string; APosition: TRelativePosition): TControlsBuilder;
    function SetTopLeftNearGroup(const AGroupName: string; APosition: TRelativePosition): TControlsBuilder;
    function SetTop(ATop: Single): TControlsBuilder; overload;
    function SetLeft(ALeft: Single): TControlsBuilder; overload;
    function SetTop(AControlName: string): TControlsBuilder; overload;
    function SetLeft(AControlName: string): TControlsBuilder; overload;
    function IncTop(AIncTop: Single): TControlsBuilder;
    function IncLeft(AIncLeft: Single): TControlsBuilder;
    function IncTopLeft(AIncTop, AIncLeft: Single): TControlsBuilder;
    function SetDirection(ADirection: TControlsBuilderDirection): TControlsBuilder;
    function SetControlHeight(AHeight: Single): TControlsBuilder;
    function SetControlWidth(AWidth: Single): TControlsBuilder;
    function SetControlWidthAndHeight(AWidth, AHeight: Single): TControlsBuilder;
    function UnsetControlHeight: TControlsBuilder;
    function UnsetControlWidth: TControlsBuilder;
    function UnsetControlWidthAndHeight: TControlsBuilder;
    function GridInit(ARows, ACols: Integer): TControlsBuilder;
    function GridFinish: TControlsBuilder;
    function GridCellSpan(ACellSpan: Integer): TControlsBuilder;
    function GridRowSpan(ARowSpan: Integer): TControlsBuilder;
    function GridColSpan(AColSpan: Integer): TControlsBuilder;
    function GridSetCellWidthAndHeight(AWidth, AHeight: Integer): TControlsBuilder;
    function GridSetColWidth(ACol: Integer; AWidth: Single): TControlsBuilder;
    function GridSetRowHeight(ARow: Integer; AHeight: Single): TControlsBuilder;
    function GridSetColOffset(ACol: Integer; AOffset: Single): TControlsBuilder;
    function GridSetRowOffset(ARow: Integer; AOffset: Single): TControlsBuilder;
    function GridSkipCell: TControlsBuilder;
    function GridSkipCells(ANumCells: Integer): TControlsBuilder;
    function GridGotoCell(ARow, ACol: Integer): TControlsBuilder;
    function GridAutoExpand: TControlsBuilder;
    function GridAutoExpandRows: TControlsBuilder;
    function GridAutoExpandCols: TControlsBuilder;
    function GridCellPosition(ACellPosition: TCellPosition): TControlsBuilder;
    function GridCellNoStrech: TControlsBuilder;
    function GridCellStrechAll: TControlsBuilder;
    function GridCellStrechHorizontal: TControlsBuilder;
    function GridCellStrechVertical: TControlsBuilder;
    function GridReturnNumberOfRows(out ARows: Integer): TControlsBuilder;
    function GridReturnNumberOfCols(out ACols: Integer): TControlsBuilder;
    function BreakLine: TControlsBuilder; overload;
    function BreakColumn: TControlsBuilder; overload;
    function Break: TControlsBuilder; overload;
    function Break(AIncTopOrLeft: Single): TControlsBuilder; overload;
    function BreakLine(AIncTop: Single): TControlsBuilder; overload;
    function BreakColumn(AIncLeft: Single): TControlsBuilder; overload;
    {$IFDEF FRAMEWORK_FMX}function WithOwnerAndParent(AOwner: TComponent; AParent: TFmxObject): TControlsBuilder;
    {$ELSE}function WithOwnerAndParent(AOwner: TComponent; AParent: TWinControl): TControlsBuilder;
    {$ENDIF}
    function WithParent(AParent: TWinControl): TControlsBuilder;
    function AddControl(AControlInfo: TCustomControlInfo; // main
      const AGroups: array of string): TControlsBuilder; overload;
    function AddControl(AControlInfo: TCustomControlInfo): TControlsBuilder; overload;
    function AddControls(AControlCreateInfos: array of TCustomControlInfo): TControlsBuilder; overload;
    function AddControls(AControlCreateInfos: array of TCustomControlInfo;
      const AGroups: array of string): TControlsBuilder; overload;
    function AddControl(AClass: TControlClass; const AName: string=''): TControlsBuilder; overload;
    function AddControl(AClass: TControlClass; const AName: string; out Reference): TControlsBuilder; overload;
    function AddControl(AClass: TControlClass; out Reference): TControlsBuilder; overload;
    function AddControl(AClass: TControlClass; const AName: string; AProc: TControlSetupProc): TControlsBuilder; overload;
    function AddControl(AClass: TControlClass; const AName: string; out Reference; AProc: TControlSetupProc): TControlsBuilder; overload;
    function AddControl(AClass: TControlClass; out Reference; AProc: TControlSetupProc): TControlsBuilder; overload;
    function AddInLevel(const AControls: array of TCustomControlInfo;
      ADirection: TControlsBuilderDirection): TControlsBuilder;
    function GetNamedControl(const AName: string): TControl;
    function MoveControls(const AControl: TControl; const ADX,
      ADY: Single): TControlsBuilder; overload;
    function MoveControls(const AControlNames: array of string;
      const ADX, ADY: Single): TControlsBuilder; overload;
    function AlignControlsRight(const AControlNames, AReferenceGroup: array of string;
      const ARightPadding: Single = 0): TControlsBuilder;
    function CenterControlsHorizontally(const AControlNames, AReferenceGroup:
      array of string): TControlsBuilder;
    function CenterControlsVertically(const AControlNames, AReferenceGroup:
      array of string): TControlsBuilder;
    function CenterControlsInParentVertically(
      const AControlNames: array of string): TControlsBuilder;
    function CenterControlsInParentHorizontally(
      const AControlNames: array of string): TControlsBuilder;
    function CenterControlInParentHorizontally: TControlsBuilder;
    function RecalcParentHeight(AExtraHeight: Single = 0): TControlsBuilder;
    function RecalcParentWidth(AExtraWidth: Single = 0): TControlsBuilder;
    function RecalcParentSize(AExtraHeight: Single = 0; AExtraWidth: Single = 0): TControlsBuilder;
    function CopyHeight(const AControlNames,
      AReferenceGroup: array of string): TControlsBuilder;
    function CopyWidth(const AControlNames,
      AReferenceGroup: array of string): TControlsBuilder;
    function CopySize(const AControlNames,
      AReferenceGroup: array of string): TControlsBuilder;

    function ReturnCurrentLevel(var ACurrentLevel: TControlsBuilderLevel): TControlsBuilder;
    function ReturnLastControl(out AControl: TControl): TControlsBuilder;


    property NamedControls[const AName: string]: TControl read GetNamedControl;
    property ContentWidth: Single read GetContentWidth;
    property ContentHeight: Single read GetFContentHeight;
    property CurrentLevel: TControlsBuilderLevel read GetCurrenteLevel;
    property Controls: TControlList read GetControls;
    property Registry: TComponentRegistry read GetComponentRegistry;
    property Items[const AName: string]: TControl read GetItem; default;
  end;

  TOPCBBuilders = class
  private
    FRegistryContextHandle: IRegistryContextHandle;
    FComponentBuilder: TComponentBuilder;
    FControlBuilder: TControlsBuilder;
    FMenuBuilder: TMenuBuilder;
  public
    constructor Create(const ARegistryContextKey: string='');
    destructor Destroy; override;
    function AsComponentBuilder: TComponentBuilder;
    function AsControlBuilder: TControlsBuilder;
    function AsMenuBuilder: TMenuBuilder;
  end;

implementation

uses
  {$IFDEF FPC}Graphics, ComCtrls,
  {$ELSE}
    {$IFDEF FRAMEWORK_FMX} Fmx.Graphics,
    {$ELSE}
    Vcl.ComCtrls,
    {$ENDIF}
  {$ENDIF}

  Math;

{ TControlInfo }

constructor TControlInfoBase{$IFNDEF FPC}<TSelf>{$ENDIF}.Create(AClass: TControlClass; const AName: string='');
begin
  FControl := nil;
  FControlClass := AClass;
  FName := AName;
  FTag := 0;
  FHeight := -1;
  FWidth := -1;
  FAlign := TOptionalAlign.None;
  FCaption := TOptionalString.None;
  FText := TOptionalString.None;
  FTop := TOptionalSingle.None;
  FLeft := TOptionalSingle.None;
  FSetupProcList := nil;
  FOnClick := nil;
  FTargetField := nil;
end;

constructor TControlInfoBase{$IFNDEF FPC}<TSelf>{$ENDIF}.Create(
  AClass: TControlClass; const AName: string; out Reference);
begin
  Create(AClass, AName);
  Assign(Reference);
end;

constructor TControlInfoBase{$IFNDEF FPC}<TSelf>{$ENDIF}.Create(
  AClass: TControlClass; out Reference);
begin
  Create(AClass, '');
  Assign(Reference);
end;

constructor TControlInfoBase{$IFNDEF FPC}<TSelf>{$ENDIF}.Create(AControl: TControl);
begin
  FControl := AControl;
  FControlClass := TControlClass(AControl.ClassType);
  FName := AControl.Name;
  FTag := 0;
  FHeight := -1;
  FWidth := -1;
  FAlign := TOptionalAlign.None;
  FCaption := TOptionalString.None;
  FText := TOptionalString.None;
  FTop := TOptionalSingle.None;
  FLeft := TOptionalSingle.None;
  FSetupProcList := nil;
  FOnClick := nil;
  FTargetField := nil;
end;

function TControlInfoBase{$IFNDEF FPC}<TSelf>{$ENDIF}.CreateControl(
  AOwner: TComponent; AParent: TWinControl; const AControlName: string): TControl;
var
  Proc: TControlSetupProc;
begin
  try
    if Assigned(Control) then
      Result := Control
    else
      Result := ControlClass.Create(AOwner);

    if not AControlName.IsEmpty then
      Result.Name := AControlName;

    Result.Tag := FTag;
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

    if Assigned(FTargetField) then
      PPointer(FTargetField)^ := Result;

    if Assigned(FSetupProcList) then
      for Proc in FSetupProcList do
        Proc(Result);
  finally
    Free;
  end;
end;

destructor TControlInfoBase{$IFNDEF FPC}<TSelf>{$ENDIF}.Destroy;
begin
  if Assigned(FSetupProcList) then
    FSetupProcList.Free;
  inherited;
end;

function TControlInfoBase{$IFNDEF FPC}<TSelf>{$ENDIF}.Assign(out Reference): TSelf;
begin
  Result := TSelf(Self);
  FTargetField := @Reference;
end;

function TControlInfoBase{$IFNDEF FPC}<TSelf>{$ENDIF}.WithAlign(
  AAlign: {$IFDEF FRAMEWORK_FMX}TAlignLayout{$ELSE}TAlign{$ENDIF}): TSelf;
begin
  Result := TSelf(Self);
  FAlign := AAlign;
end;

function TControlInfoBase{$IFNDEF FPC}<TSelf>{$ENDIF}.WithCaption(ACaption: string): TSelf;
begin
  Result := TSelf(Self);
  FCaption := ACaption;
end;

function TControlInfoBase{$IFNDEF FPC}<TSelf>{$ENDIF}.WithHeight(AHeight: Single): TSelf;
begin
  Result := TSelf(Self);
  FHeight := AHeight;
end;

function TControlInfoBase{$IFNDEF FPC}<TSelf>{$ENDIF}.WithLeft(ALeft: Single): TSelf;
begin
  Result := TSelf(Self);
  FLeft := ALeft;
end;

function TControlInfoBase{$IFNDEF FPC}<TSelf>{$ENDIF}.WithName(AName: string): TSelf;
begin
  Result := TSelf(Self);
  FName := AName;
end;

function TControlInfoBase{$IFNDEF FPC}<TSelf>{$ENDIF}.WithOnClick(AOnClick: TNotifyEvent): TSelf;
begin
  Result := TSelf(Self);
  FOnClick := AOnClick;
end;

function TControlInfoBase{$IFNDEF FPC}<TSelf>{$ENDIF}.Setup(AProc: TControlSetupProc): TSelf;
begin
  Result := TSelf(Self);

  if not Assigned(aproc) then
    Exit;

  if not Assigned(FSetupProcList) then
    FSetupProcList := TControlSetupProcList.Create;
  FSetupProcList.Add(AProc);
end;

function TControlInfoBase{$IFNDEF FPC}<TSelf>{$ENDIF}.Setup(const AProcs: array of TControlSetupProc): TSelf;
var
  Proc: TControlSetupProc;
begin
  if not Assigned(FSetupProcList) then
    FSetupProcList := TControlSetupProcList.Create;
  for Proc in AProcs do
    FSetupProcList.Add(Proc);
  Result := TSelf(Self);
end;

function TControlInfoBase{$IFNDEF FPC}<TSelf>{$ENDIF}.WithTag(ATag: NativeInt): TSelf;
begin
  Result := TSelf(Self);
  FTag := ATag;
end;

function TControlInfoBase{$IFNDEF FPC}<TSelf>{$ENDIF}.WithText(AText: string): TSelf;
begin
  Result := TSelf(Self);
  FText := AText;
end;

function TControlInfoBase{$IFNDEF FPC}<TSelf>{$ENDIF}.WithTop(ATop: Single): TSelf;
begin
  Result := TSelf(Self);
  FTop := ATop;
end;

function TControlInfoBase{$IFNDEF FPC}<TSelf>{$ENDIF}.WithWidth(AWidth: Single): TSelf;
begin
  Result := TSelf(Self);
  FWidth := AWidth;
end;

function TControlInfoBase{$IFNDEF FPC}<TSelf>{$ENDIF}.WithWidthAndHeight(AWidth: Single; AHeight: Single
  ): TSelf;
begin
  Result := TSelf(Self);
  FWidth := AWidth;
  FHeight := AHeight;
end;

{ TControlsBuilder }

function TControlsBuilder.BreakLine: TControlsBuilder;
begin
  Result := Self;
  if CurrentLevel.GridMode.Active then
  begin
    Self.GridGotoCell(CurrentLevel.GridMode.CurrentRow + 1, 0);
  end
  else
  begin
    CurrentLevel.CurrentTop := CurrentLevel.CurrentTop + CurrentLevel.MaxControlHeight;
    CurrentLevel.CurrentLeft := CurrentLevel.InitialLeft;
    CurrentLevel.MaxControlHeight := 0;
  end;
end;

function TControlsBuilder.Break: TControlsBuilder;
begin
  Result := Self;
  if CurrentLevel.Direction = cpdHorizontal then
    BreakLine;
  if CurrentLevel.Direction = cpdVertical then
    BreakColumn;
end;

function TControlsBuilder.Break(AIncTopOrLeft: Single): TControlsBuilder;
begin
  Result := Self;
  Self.Break;
  if CurrentLevel.Direction = cpdHorizontal then
    IncTop(AIncTopOrLeft);
  if CurrentLevel.Direction = cpdVertical then
    IncLeft(AIncTopOrLeft);
end;

function TControlsBuilder.BreakColumn(AIncLeft: Single): TControlsBuilder;
begin
  Result := Self;
  Self.BreakColumn;
  IncLeft(AIncLeft);
end;

function TControlsBuilder.BreakColumn: TControlsBuilder;
begin
  Result := Self;
  if CurrentLevel.GridMode.Active then
  begin
    Self.GridGotoCell(0, CurrentLevel.GridMode.CurrentCol + 1);
  end
  else
  begin
    CurrentLevel.CurrentLeft := CurrentLevel.CurrentLeft + CurrentLevel.MaxControlWidth;
    CurrentLevel.CurrentTop := CurrentLevel.InitialTop;
    CurrentLevel.MaxControlWidth := 0;
  end;
end;

function TControlsBuilder.CenterControlsVertically(const AControlNames,
  AReferenceGroup: array of string): TControlsBuilder;
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

function TControlsBuilder.CenterControlsInParentVertically(
  const AControlNames: array of string): TControlsBuilder;
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

function TControlsBuilder.CenterControlsInParentHorizontally(
  const AControlNames: array of string): TControlsBuilder;
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

function TControlsBuilder.CenterControlInParentHorizontally: TControlsBuilder;
var
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
        TControl(AControl.Parent).Width
        - TControl(AControl.Parent).Padding.Top
        + TControl(AControl.Parent).Padding.Bottom
    else
      Result := 0; // não tem dimensão
  end;
  {$ENDIF}

begin
  Result := Self;

  ParentCtrl := Self.GetControls.Last;
  if not Assigned(ParentCtrl) or not Assigned(ParentCtrl.Parent) then
    Exit;

  {$IFDEF FRAMEWORK_FMX}
  ParentWidth := GetParentClientWidth(ParentCtrl);
  {$ELSE}
  ParentWidth := ParentCtrl.Parent.ClientWidth;
  {$ENDIF}

  TargetCenterX := ParentCtrl.BoundsRect.Left + (ParentCtrl.BoundsRect.Width / 2);
  ParentCenterX := ParentWidth / 2;

  DeltaX := ParentCenterX - TargetCenterX;

  MoveControls(ParentCtrl, DeltaX, 0);
end;

constructor TControlsBuilder.Create(ARegistryContextKey: string);
begin
  Create(TRegistryContextHandle.Create(ARegistryContextKey));
end;

constructor TControlsBuilder.Create(ARegistryContextHandle: IRegistryContextHandle);
begin
  FRegistryContextHandle := ARegistryContextHandle;
  FGroups := TControlGroupMap.Create;
  FLevelStack := TControlsBuilderLevelStack.Create(True);
  FLevelStack.Add(TControlsBuilderLevel.Create);
end;

procedure TControlsBuilder.SetupControlInfoForGridMode(AControlInfo: TCustomControlInfo);
var
  Row, Col: Integer;
  RowSpan, ColSpan: Integer;
  RCell: {$IFDEF FRAMEWORK_FMX}TRectF{$ELSE}TRect{$ENDIF};
  RControl: {$IFDEF FRAMEWORK_FMX}TRectF{$ELSE}TRect{$ENDIF};
begin
  if not CurrentLevel.GridMode.Active then
    Exit;

  if CurrentLevel.Direction = cpdHorizontal then
    CurrentLevel.GridMode.Direction := gfdRowFirst
  else
    CurrentLevel.GridMode.Direction := gfdColFirst;

  if not CurrentLevel.GridMode.Step(RowSpan, ColSpan, Row, Col) then
    Exit; // fim do grid

  if CurrentLevel.GridMode.CalcCellRectAbsolute(
     Row, Col, RowSpan, ColSpan,
     CurrentLevel.HorizontalSpace, CurrentLevel.VerticalSpace, RCell) then
  begin
    RControl := CurrentLevel.GridMode.AdjustRectForCellLayout(
      RCell,
      AControlInfo.Width,
      AControlInfo.Height
    );

    AControlInfo.Align := {$IFDEF FRAMEWORK_FMX}TAlignLayout.None{$ELSE}alNone{$ENDIF};
    AControlInfo.Left := RControl.Left;
    AControlInfo.Top  := RControl.Top;
    AControlInfo.Height := RControl.Bottom - RControl.Top;
    AControlInfo.Width := RControl.Right - RControl.Left;
  end;

  CurrentLevel.GridMode.ResetSpans;
end;

function TControlsBuilder.CreateControl(Info: TCustomControlInfo; AOwner: TComponent = nil): TControl;
var
  ControlName: string;
begin
  ControlName := '';
  if not Info.Name.IsEmpty then
    ControlName := Registry.UniqueName(Info.Name);

  if not Info.Top.HasValue then
    Info.Top := CurrentLevel.CurrentTop;

  if not Info.Left.HasValue then
    Info.Left := CurrentLevel.CurrentLeft;

  if not Assigned(AOwner) then
    AOwner := FOwner;

  Result := Info.CreateControl(AOwner, CurrentLevel.Parent, ControlName);
end;

function TControlsBuilder.AddControl(AControlInfo: TCustomControlInfo;
  const AGroups: array of string): TControlsBuilder;
var
  Control: TControl;
  Level: TControlsBuilderLevel;

  procedure ApplyDefaultControlSize;
  begin
    {$IFDEF FRAMEWORK_FMX}
    if CurrentLevel.ControlHeight.HasValue then
      Control.Height := CurrentLevel.ControlHeight.Value;
    if CurrentLevel.ControlWidth.HasValue then
      Control.Width := CurrentLevel.ControlWidth.Value;
    {$ELSE}
    if CurrentLevel.ControlHeight.HasValue then
      Control.Height := Trunc(CurrentLevel.ControlHeight.Value);
    if CurrentLevel.ControlWidth.HasValue then
      Control.Width := Trunc(CurrentLevel.ControlWidth.Value);
    {$ENDIF}
  end;

  function CanAddToGrid: Boolean;
  var
    R, C: Integer;
  begin
    Result := False;

    if CurrentLevel.GridMode.IsCellFree(
      CurrentLevel.GridMode.CurrentRow,
      CurrentLevel.GridMode.CurrentCol
    ) then
    begin
      Result := True;
      Exit;
    end;


    if (CurrentLevel.GridMode.Direction = gfdRowFirst) and (gaeRows in CurrentLevel.GridMode.AutoExpand)
       or
       (CurrentLevel.GridMode.Direction = gfdColFirst) and (gaeCols in CurrentLevel.GridMode.AutoExpand)
    then
    begin
      Result := True;
      Exit;
    end;

    Result := CurrentLevel.GridMode.PeekNext(R, C);
  end;

  procedure AdjustControlToCell(AControl: TControl; ACellRect: TRect);
  var
    RControl: {$IFDEF FRAMEWORK_FMX}TRectF{$ELSE}TRect{$ENDIF};
  begin

    RControl := CurrentLevel.GridMode.AdjustRectForCellLayout(
      ACellRect,
      AControl.Width,
      AControl.Height
    );

    TProtectedControl(AControl).Left := RControl.Left;
    TProtectedControl(AControl).Top := RControl.Top;
  end;

  function GetRectFromControlInfo(AControlInfo: TControlInfo): TRect;
  begin
    Result := TRect.Create(
      TPoint.Create(Trunc(AControlInfo.Top.Value), Trunc(AControlInfo.Left.Value)),
      Trunc(AControlInfo.Width),
      Trunc(AControlInfo.Height)
    );
  end;

begin
  Result := Self;

  if CurrentLevel.GridMode.Active then
  begin
    if not CanAddToGrid then
      Exit;

    SetupControlInfoForGridMode(AControlInfo);
    Control := CreateControl(AControlInfo);
  end
  else
  begin
    Control := CreateControl(AControlInfo);
  end;

  ApplyDefaultControlSize;

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
    {$IFNDEF FPC}TTabSheet(Control).Parent := nil;{$ENDIF}
    TTabSheet(Control).PageControl := TPageControl(CurrentLevel.Parent);
  end;
  {$ENDIF}

  for Level in FLevelStack do
    if not Level.GroupName.IsEmpty then
      AddControlToGroups(Control, [Level.GroupName]);

  AddControlToGroups(Control, AGroups);

  MoveTopLeftAfterControl(Control);
end;

function TControlsBuilder.AddControl(AClass: TControlClass;
  const AName: string=''): TControlsBuilder;
begin
  Result := AddControl(TControlInfo.Create(AClass, AName));
end;

function TControlsBuilder.AddControl(AClass: TControlClass; const AName: string;
  out Reference): TControlsBuilder;
begin
  Result := AddControl(TControlInfo.Create(AClass, AName, Reference));
end;

function TControlsBuilder.AddControl(AClass: TControlClass;
  out Reference): TControlsBuilder;
begin
  Result := AddControl(TControlInfo.Create(AClass, Reference));
end;

function TControlsBuilder.AddControl(AClass: TControlClass; const AName: string;
  AProc: TControlSetupProc): TControlsBuilder;
begin
  Result := AddControl(TControlInfo.Create(AClass, AName).Setup(AProc));
end;

function TControlsBuilder.AddControl(AClass: TControlClass; const AName: string;
  out Reference; AProc: TControlSetupProc): TControlsBuilder;
begin
  Result := AddControl(TControlInfo.Create(AClass, AName, Reference).Setup(AProc));
end;

function TControlsBuilder.AddControl(AClass: TControlClass; out Reference;
  AProc: TControlSetupProc): TControlsBuilder;
begin
  Result := AddControl(TControlInfo.Create(AClass, Reference).Setup(AProc));
end;

function TControlsBuilder.AddControl(AControlInfo: TCustomControlInfo): TControlsBuilder;
begin
  Result := AddControl(AControlInfo, []);
end;

function TControlsBuilder.AddControls(AControlCreateInfos: array of TCustomControlInfo;
  const AGroups: array of string): TControlsBuilder;
var
  I: Integer;
begin
  Result := Self;
  for I := Low(AControlCreateInfos) to High(AControlCreateInfos) do
    AddControl(AControlCreateInfos[I], AGroups);
end;

function TControlsBuilder.AddControls(
  AControlCreateInfos: array of TCustomControlInfo): TControlsBuilder;
var
  I: Integer;
begin
  Result := Self;
  for I := Low(AControlCreateInfos) to High(AControlCreateInfos) do
    AddControl(AControlCreateInfos[I], []);
end;

procedure TControlsBuilder.AddControlToGroups(AControl: TControl;
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

function TControlsBuilder.AddInLevel(const AControls: array of TCustomControlInfo;
  ADirection: TControlsBuilderDirection): TControlsBuilder;
begin
  Result := Self;
  SubLevel(ADirection);
  AddControls(AControls);
  SuperLevel;
end;

function TControlsBuilder.CenterControlsHorizontally(const AControlNames,
  AReferenceGroup: array of string): TControlsBuilder;
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

function TControlsBuilder.AlignControlsRight(const AControlNames,
  AReferenceGroup: array of string; const ARightPadding: Single = 0): TControlsBuilder;
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

function TControlsBuilder.SubLevel(AGroupName: string): TControlsBuilder;
var
  R: {$IFDEF FRAMEWORK_FMX}TRectF{$ELSE}TRect{$ENDIF};
begin
  Result := Self;

  if CurrentLevel.GridMode.Active then
  begin
    CurrentLevel.GridMode.CalcCellRectAbsolute(
      CurrentLevel.GridMode.NextRow, CurrentLevel.GridMode.NextCol,
      1, 1,
      Self.CurrentLevel.HorizontalSpace, Self.CurrentLevel.VerticalSpace,
      R
    );

    FLevelStack.Add(CurrentLevel.Clone);
    CurrentLevel.InitialTop := R.top;
    CurrentLevel.InitialLeft := R.Left;
    CurrentLevel.CurrentTop := R.top;
    CurrentLevel.CurrentLeft := R.Left;
    CurrentLevel.MaxControlHeight := 0;
    CurrentLevel.MaxControlWidth := 0;
  end
  else
  begin
    FLevelStack.Add(CurrentLevel.Clone);
    CurrentLevel.InitialTop := CurrentLevel.CurrentTop;
    CurrentLevel.InitialLeft := CurrentLevel.CurrentLeft;
    CurrentLevel.MaxControlHeight := 0;
    CurrentLevel.MaxControlWidth := 0;
  end;

  if not AGroupName.IsEmpty then
    CurrentLevel.GroupName := AGroupName;
end;

function TControlsBuilder.SubLevel(
  ADirection: TControlsBuilderDirection; AGroupName: string): TControlsBuilder;
begin
  Result := SubLevel(AGroupName);
  SetDirection(ADirection);
end;

function TControlsBuilder.SiblingSubLevel(ADirection: TControlsBuilderDirection;
  ABreak: Boolean): TControlsBuilder;
begin
  Result := SiblingSubLevel(ADirection, '', ABreak);
end;

function TControlsBuilder.SiblingSubLevelWithBreak(
  ADirection: TControlsBuilderDirection;
  AGroupName: string): TControlsBuilder;
begin
  Result := SiblingSubLevel(ADirection, AGroupName, True);
end;

function TControlsBuilder.SiblingSubLevelWithBreak(
  AGroupName: string): TControlsBuilder;
begin
  Result := SiblingSubLevel(AGroupName, True);
end;

function TControlsBuilder.SiblingSubLevel(ABreak: Boolean): TControlsBuilder;
begin
  Result := SiblingSubLevel('', ABreak);
end;

function TControlsBuilder.BreakLine(AIncTop: Single): TControlsBuilder;
begin
  Result := Self;
  Self.BreakLine;
  IncTop(AIncTop);
end;

destructor TControlsBuilder.Destroy;
var
  GroupList: TControlList;
begin
  for GroupList in FGroups.Values do
    GroupList.Free;
  FGroups.Free;
  FLevelStack.Free;
  inherited;
end;

function TControlsBuilder.SuperLevel: TControlsBuilder;
var
  SubL, SuperL: TControlsBuilderLevel;
  Bounds: TControlGroupBounds;
  R: {$IFDEF FRAMEWORK_FMX}TRectF{$ELSE}TRect{$ENDIF};
  Row, Col, RowSpan, ColSpan: Integer;

  function GetSubLevelBounds: TControlGroupBounds;
  begin
    if (SubL.Parent = SuperL.Parent) then
      Result := GetGroupBounds(SubL.GroupName)
    else
    begin
      {$IFNDEF FRAMEWORK_FMX}
      if (SubL.Parent is TPanel) and TPanel(SubL.Parent).AutoSize then
      begin
        SubL.Parent.HandleNeeded;
        SubL.Parent.Invalidate;
        SubL.Parent.Update;
      end;
      {$ENDIF}

      Result.Reset;
      Result.Include(SubL.Parent);
    end;
  end;

begin
  if FLevelStack.Count <= 1 then
    raise Exception.Create('PreviousLevel chamado no nível raiz');

  SubL := FLevelStack.Last;
  SuperL := FLevelStack[FLevelStack.Count - 2];

  if SuperL.GridMode.Active then
  begin
    if (SubL.Parent = SuperL.Parent) then  // sublevel sem container
    begin
      if CurrentLevel.Direction = cpdHorizontal then
        SuperL.GridMode.Direction := gfdRowFirst
      else
        SuperL.GridMode.Direction := gfdColFirst;

      RowSpan := SuperL.GridMode.GetRowSpanForFill;
      ColSpan := SuperL.GridMode.GetColSpanForFill;

      if SuperL.GridMode.Step(RowSpan, ColSpan, Row, Col) then
      begin
        if SuperL.GridMode.CalcCellRectAbsolute(
          SuperL.GridMode.CurrentRow,
          SuperL.GridMode.CurrentCol,
          RowSpan,
          ColSpan,
          SuperL.HorizontalSpace,
          SuperL.VerticalSpace,
          R
        ) then
        begin
          // move o cursor para depois da célula
          MoveTopLeftAfterRect(R,
            {$IFDEF FRAMEWORK_FMX} TAlignLayout.None {$ELSE} alNone {$ENDIF});
        end;
      end;
    end
    else   // sublevel com container
    begin
      if SuperL.GridMode.CalcCellRectAbsolute(
          SuperL.GridMode.CurrentRow,
          SuperL.GridMode.CurrentCol,
          RowSpan,
          ColSpan,
          SuperL.HorizontalSpace,
          SuperL.VerticalSpace,
          R
        ) then
        begin
          // move o cursor para depois da célula
          MoveTopLeftAfterRect(R,
            {$IFDEF FRAMEWORK_FMX} TAlignLayout.None {$ELSE} alNone {$ENDIF});
        end;
    end;
  end
  else
  begin
    // Comportamento antigo (não-grid): usa bounds reais dos controles
    if FGroups.ContainsKey(SubL.GroupName) then
    begin
      Bounds := GetSubLevelBounds;

      case SuperL.Direction of
        cpdHorizontal:
          begin
            SuperL.CurrentLeft := Bounds.Right + SuperL.HorizontalSpace;
            SuperL.MaxControlHeight :=
              Max(SuperL.MaxControlHeight, Bounds.Height + SuperL.VerticalSpace);
          end;
        cpdVertical:
          begin
            SuperL.CurrentTop := Bounds.Bottom + SuperL.VerticalSpace;
            SuperL.MaxControlWidth :=
              Max(SuperL.MaxControlWidth, Bounds.Width + SuperL.HorizontalSpace);
          end;
      end;
    end;

    // só move o top/left se existir o GroupName do level,
    // ou seja foi inserido pelo menos 1 controle

    if FGroups.ContainsKey(SuperL.GroupName) then
      MoveTopLeftAfterBound(GetGroupBounds(SuperL.GroupName));
  end;

  FLevelStack.Delete(FLevelStack.Count - 1); // remove nível atual
  Result := Self;
end;


function TControlsBuilder.RecalcParentHeight(AExtraHeight: Single): TControlsBuilder;
begin
  Result := Self;
  {$IFDEF FRAMEWORK_FMX}
  CurrentLevel.Parent.Height :=
    GetControlsBounds([CurrentLevel.GroupName]).Bottom
    + AExtraHeight;
  {$ELSE}
  CurrentLevel.Parent.Height := Trunc(
    GetControlsBounds([CurrentLevel.GroupName]).Bottom
    + AExtraHeight
  );
  {$ENDIF}
end;

function TControlsBuilder.RecalcParentSize(AExtraHeight,
  AExtraWidth: Single): TControlsBuilder;
begin
  Result := Self;
  RecalcParentHeight(AExtraHeight);
  RecalcParentWidth(AExtraWidth);
end;

function TControlsBuilder.RecalcParentWidth(
  AExtraWidth: Single): TControlsBuilder;
begin
  Result := Self;
  {$IFDEF FRAMEWORK_FMX}
  CurrentLevel.Parent.Width :=
    GetControlsBounds([CurrentLevel.GroupName]).Right
    + AExtraWidth;
  {$ELSE}
  CurrentLevel.Parent.Width := Trunc(
    GetControlsBounds([CurrentLevel.GroupName]).Right
    + AExtraWidth
  );
  {$ENDIF}
end;

function TControlsBuilder.CopyHeight(const AControlNames,
  AReferenceGroup: array of string): TControlsBuilder;
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

function TControlsBuilder.CopyWidth(const AControlNames,
  AReferenceGroup: array of string): TControlsBuilder;
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

function TControlsBuilder.CopySize(const AControlNames,
  AReferenceGroup: array of string): TControlsBuilder;
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

function TControlsBuilder.ReturnCurrentLevel(var ACurrentLevel: TControlsBuilderLevel): TControlsBuilder;
begin
  Result := Self;
  ACurrentLevel := Self.CurrentLevel;
end;

function TControlsBuilder.ReturnLastControl(out AControl: TControl): TControlsBuilder;
begin
  Result := Self;
  AControl := nil;
  if Self.Controls.Count > 0 then;
    AControl := Self.Controls.Last;
end;

function TControlsBuilder.GetNamedControl(const AName: string): TControl;
begin
  if not Registry.NamedComponents.TryGetValue(AName, TComponent(Result)) then
    Result := nil;
end;

function TControlsBuilder.IncLeft(AIncLeft: Single): TControlsBuilder;
begin
  Result := Self;
  CurrentLevel.CurrentLeft := CurrentLevel.CurrentLeft + AIncLeft;
end;

function TControlsBuilder.IncTop(AIncTop: Single): TControlsBuilder;
begin
  Result := Self;
  CurrentLevel.CurrentTop := CurrentLevel.CurrentTop + AIncTop;
end;

function TControlsBuilder.IncTopLeft(AIncTop,
  AIncLeft: Single): TControlsBuilder;
begin
  Result := Self;
  IncTop(AIncTop);
  IncLeft(AIncLeft);
end;

function TControlsBuilder.GetComponentRegistry: TComponentRegistry;
begin
  Result := FRegistryContextHandle.GetRegistry;
end;

function TControlsBuilder.GetContentWidth: Single;
begin
  Result := GetGroupBounds(FLevelStack.First.GroupName).Width;
end;

function TControlsBuilder.GetControl(const AName: string): TControl;
begin
  Result := Registry.GetControl(AName);
end;

{$IFDEF FPC}generic{$ENDIF}
function TControlsBuilder.GetControl<T>(const AName: string): T;
begin
  Result := Registry.GetControl<T>(AName);
end;

function TControlsBuilder.GetControls: TControlList;
begin
  Result := Registry.Controls;
end;

function TControlsBuilder.GetControlsBounds(
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

function TControlsBuilder.GetCurrenteLevel: TControlsBuilderLevel;
begin
  Result := FLevelStack.Last;
end;

function TControlsBuilder.GetFContentHeight: Single;
begin
  Result := GetGroupBounds(FLevelStack.First.GroupName).Height;
end;

function TControlsBuilder.GetGroupBounds(
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

function TControlsBuilder.GetItem(const AName: string): TControl;
begin
  Result := Self.GetControl(AName);
end;

function TControlsBuilder.MoveControls(const AControl: TControl;
  const ADX, ADY: Single): TControlsBuilder;
var
    L, T: Single;
begin
  Result := Self;

  {$IFDEF FRAMEWORK_FMX}
  L := AControl.Position.X;
  T := AControl.Position.Y;
  {$ELSE}
  L := AControl.Left;
  T := AControl.Top;
  {$ENDIF}

  L := L + ADX;
  T := T + ADY;

  {$IFDEF FRAMEWORK_FMX}
  AControl.Position.X := L;
  AControl.Position.Y := T;
  {$ELSE}
  AControl.Left := Round(L);
  AControl.Top := Round(T);
  {$ENDIF}
end;

function TControlsBuilder.MoveControls(const AControlNames: array of string;
  const ADX, ADY: Single): TControlsBuilder;
var
  Name: string;
  Ctrl: TControl;
begin
  Result := Self;

  for Name in AControlNames do
  begin
    if not Registry.NamedComponents.TryGetValue(Name, TComponent(Ctrl)) then
      raise Exception.CreateFmt('Controle "%s" não encontrado.', [Name]);

    MoveControls(Ctrl, ADX, ADY);
  end;
end;

procedure TControlsBuilder.MoveTopLeftAfterBound(ABounds: TControlGroupBounds);
begin
  if CurrentLevel.Direction = cpdHorizontal then
  begin
    CurrentLevel.CurrentLeft := CurrentLevel.CurrentLeft
      + ABounds.Width
      + CurrentLevel.HorizontalSpace;
    CurrentLevel.MaxControlHeight :=
      Max(CurrentLevel.MaxControlHeight, ABounds.Height + CurrentLevel.VerticalSpace);
  end;

  if CurrentLevel.Direction = cpdVertical then
  begin
    CurrentLevel.CurrentTop := CurrentLevel.CurrentTop
      + ABounds.Height
      + CurrentLevel.VerticalSpace;
    CurrentLevel.MaxControlWidth :=
      Max(CurrentLevel.MaxControlWidth, ABounds.Width + CurrentLevel.HorizontalSpace);
  end;
end;

procedure TControlsBuilder.MoveTopLeftAfterRect(
  const ARect: {$IFDEF FRAMEWORK_FMX}TRectF{$ELSE}TRect{$ENDIF};
  AAlign: {$IFDEF FRAMEWORK_FMX}TAlignLayout{$ELSE}TAlign{$ENDIF}
);
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

  W := ARect.Width;
  H := ARect.Height;

  if AAlign = AlignNone then
  begin
    if CurrentLevel.Direction = cpdHorizontal then
    begin
      CurrentLevel.CurrentLeft := CurrentLevel.CurrentLeft
        + W
        + CurrentLevel.HorizontalSpace;
      CurrentLevel.MaxControlHeight :=
        Max(CurrentLevel.MaxControlHeight, H + CurrentLevel.VerticalSpace);
    end;

    if CurrentLevel.Direction = cpdVertical then
    begin
      CurrentLevel.CurrentTop := CurrentLevel.CurrentTop
        + H
        + CurrentLevel.VerticalSpace;
      CurrentLevel.MaxControlWidth :=
        Max(CurrentLevel.MaxControlWidth, W + CurrentLevel.HorizontalSpace);
    end;
  end;

  if AAlign = AlignTop then
    CurrentLevel.CurrentTop := CurrentLevel.CurrentTop
      + H
      + CurrentLevel.VerticalSpace;

  if AAlign = AlignLeft then
    SetTopLeft(
      CurrentLevel.CurrentTop,
      CurrentLevel.CurrentLeft + W + CurrentLevel.HorizontalSpace
    );
end;

procedure TControlsBuilder.MoveTopLeftAfterControl(AControl: TControl);
var
  R: {$IFDEF FRAMEWORK_FMX}TRectF{$ELSE}TRect{$ENDIF};
begin
  {$IFDEF FRAMEWORK_FMX}
  R := RectF(0, 0, AControl.Width, AControl.Height);
  {$ELSE}
  R := Rect(0, 0, AControl.Width, AControl.Height);
  {$ENDIF}

  MoveTopLeftAfterRect(R, AControl.Align);
end;

function TControlsBuilder.SiblingSubLevel(ADirection: TControlsBuilderDirection;
  AGroupName: string; ABreak: Boolean): TControlsBuilder;
begin
  Result := SuperLevel;
  if ABreak then
    Break;
  SubLevel(ADirection, AGroupName);
end;

function TControlsBuilder.SiblingSubLevel(AGroupName: string;
  ABreak: Boolean): TControlsBuilder;
begin
  Result := SuperLevel;
  if ABreak then
    Break;
  SubLevel(AGroupName);
end;

function TControlsBuilder.SetControlHeight(AHeight: Single): TControlsBuilder;
begin
  Result := Self;
  CurrentLevel.ControlHeight := AHeight;
end;

function TControlsBuilder.SetControlWidth(AWidth: Single): TControlsBuilder;
begin
  Result := Self;
  CurrentLevel.ControlWidth := AWidth;
end;

function TControlsBuilder.SetControlWidthAndHeight(AWidth,
  AHeight: Single): TControlsBuilder;
begin
  Result := Self;
  CurrentLevel.ControlHeight := AHeight;
  CurrentLevel.ControlWidth := AWidth;
end;

function TControlsBuilder.UnsetControlHeight: TControlsBuilder;
begin
  Result := Self;
  CurrentLevel.ControlHeight := TOptionalSingle.None;
end;

function TControlsBuilder.UnsetControlWidth: TControlsBuilder;
begin
  Result := Self;
  CurrentLevel.ControlWidth := TOptionalSingle.None;
end;

function TControlsBuilder.UnsetControlWidthAndHeight: TControlsBuilder;
begin
  Result := Self;
  CurrentLevel.ControlHeight := TOptionalSingle.None;
  CurrentLevel.ControlWidth := TOptionalSingle.None;
end;

function TControlsBuilder.GridInit(ARows, ACols: Integer): TControlsBuilder;
begin
  Result := Self;
  SubLevel;
  CurrentLevel.GridMode.Activate(
    ARows,
    ACols,
    CurrentLevel.CurrentLeft,
    CurrentLevel.CurrentTop
  );
end;

function TControlsBuilder.GridCellSpan(ACellSpan: Integer): TControlsBuilder;
begin
  Result := Self;
  CurrentLevel.GridMode.ResetSpans;

  if CurrentLevel.Direction = cpdHorizontal then
    CurrentLevel.GridMode.ColSpan := ACellSpan
  else
    CurrentLevel.GridMode.RowSpan := ACellSpan;
end;

function TControlsBuilder.GridRowSpan(ARowSpan: Integer): TControlsBuilder;
begin
  Result := Self;
  CurrentLevel.GridMode.RowSpan := ARowSpan;
end;

function TControlsBuilder.GridColSpan(AColSpan: Integer): TControlsBuilder;
begin
  Result := Self;
  CurrentLevel.GridMode.ColSpan := AColSpan;
end;

function TControlsBuilder.GridSetCellWidthAndHeight(AWidth,
  AHeight: Integer): TControlsBuilder;
begin
  Result := Self;
  CurrentLevel.GridMode.CellWidth := AWidth;
  CurrentLevel.GridMode.CellHeight := AHeight;
end;

function TControlsBuilder.GridSetColWidth(ACol: Integer;
  AWidth: Single): TControlsBuilder;
begin
  Result := Self;
  CurrentLevel.GridMode.SetColWidth(ACol, AWidth);
end;

function TControlsBuilder.GridSetRowHeight(ARow: Integer;
  AHeight: Single): TControlsBuilder;
begin
  Result := Self;
  CurrentLevel.GridMode.SetRowHeight(ARow, AHeight);
end;

function TControlsBuilder.GridSetColOffset(ACol: Integer; AOffset: Single): TControlsBuilder;
begin
  Result := Self;
  CurrentLevel.GridMode.SetColOffset(ACol, AOffset);
end;

function TControlsBuilder.GridSetRowOffset(ARow: Integer; AOffset: Single): TControlsBuilder;
begin
  Result := Self;
  CurrentLevel.GridMode.SetRowOffset(ARow, AOffset);
end;

function TControlsBuilder.GridAutoExpand: TControlsBuilder;
begin
  Result := Self;
  CurrentLevel.GridMode.AutoExpand := [gaeRows, gaeCols];
end;

function TControlsBuilder.GridAutoExpandRows: TControlsBuilder;
begin
  Result := Self;
  CurrentLevel.GridMode.AutoExpand := [gaeRows];
end;

function TControlsBuilder.GridAutoExpandCols: TControlsBuilder;
begin
  Result := Self;
  CurrentLevel.GridMode.AutoExpand := [gaeCols];
end;

function TControlsBuilder.GridCellPosition(ACellPosition: TCellPosition): TControlsBuilder;
begin
  Result := Self;
  Self.CurrentLevel.GridMode.CellPosition := ACellPosition;
end;

function TControlsBuilder.GridCellNoStrech: TControlsBuilder;
begin
  Result := Self;
  Self.CurrentLevel.GridMode.CellStrech := [];
end;

function TControlsBuilder.GridCellStrechAll: TControlsBuilder;
begin
  Result := Self;
  Self.CurrentLevel.GridMode.CellStrech := [csVertical, csHorizontal];
end;

function TControlsBuilder.GridCellStrechHorizontal: TControlsBuilder;
begin
  Result := Self;
  Self.CurrentLevel.GridMode.CellStrech := [csHorizontal];
end;

function TControlsBuilder.GridCellStrechVertical: TControlsBuilder;
begin
  Result := Self;
  Self.CurrentLevel.GridMode.CellStrech := [csVertical];
end;

function TControlsBuilder.GridReturnNumberOfRows(out ARows: Integer): TControlsBuilder;
begin
  Result := Self;
  ARows := Self.CurrentLevel.GridMode.Rows;
end;

function TControlsBuilder.GridReturnNumberOfCols(out ACols: Integer): TControlsBuilder;
begin
  Result := Self;
  ACols := Self.CurrentLevel.GridMode.Cols;
end;

function TControlsBuilder.GridSkipCell: TControlsBuilder;
var
  Row, Col, RowSpan, ColSpan: Integer;
  Dir: TGridFillDirection;
  R: {$IFDEF FRAMEWORK_FMX}TRectF{$ELSE}TRect{$ENDIF};
begin
  Result := Self;
  CurrentLevel.GridMode.ResetSpans;  // skip ira resetar qualquer configuração de span

  if CurrentLevel.Direction = cpdHorizontal then
    CurrentLevel.GridMode.Direction := gfdRowFirst
  else
    CurrentLevel.GridMode.Direction := gfdColFirst;

  if CurrentLevel.GridMode.Step(RowSpan, ColSpan, Row, Col, False) then
  begin
    if CurrentLevel.GridMode.CalcCellRectAbsolute(
      Row, Col, 1, 1,
      CurrentLevel.HorizontalSpace,
      CurrentLevel.VerticalSpace,
      R) then
    begin
      // atualiza a posição corrente do builder
      CurrentLevel.CurrentLeft := R.Left;
      CurrentLevel.CurrentTop  := R.Top;
    end;
  end;
end;

function TControlsBuilder.GridSkipCells(ANumCells: Integer): TControlsBuilder;
var
  I: Integer;
begin
  if not CurrentLevel.GridMode.Active then
    Exit;

  if ANumCells <= 0 then
    Exit;

  Result := Self;
  for I:=1 to ANumCells do
    GridSkipCell;
end;

function TControlsBuilder.GridGotoCell(ARow, ACol: Integer): TControlsBuilder;
var
  R: {$IFDEF FRAMEWORK_FMX}TRectF{$ELSE}TRect{$ENDIF};
begin
  Result := Self;

  if (ARow < 0) or (ARow >= CurrentLevel.GridMode.Rows) then
    raise Exception.CreateFmt('GridGotoCell: linha %d fora dos limites', [ARow]);

  if (ACol < 0) or (ACol >= CurrentLevel.GridMode.Cols) then
    raise Exception.CreateFmt('GridGotoCell: coluna %d fora dos limites', [ACol]);

  CurrentLevel.GridMode.FCurrentRow := ARow;
  CurrentLevel.GridMode.FCurrentCol := ACol;

  if CurrentLevel.GridMode.CalcCellRectAbsolute(
    ARow, ACol, 1, 1,
    CurrentLevel.HorizontalSpace,
    CurrentLevel.VerticalSpace,
    R) then
  begin
    CurrentLevel.CurrentLeft := R.Left;
    CurrentLevel.CurrentTop  := R.Top;
  end;
end;

function TControlsBuilder.GridFinish: TControlsBuilder;
begin
  Result := Self;
  if CurrentLevel.GridMode.Active then
  begin
    CurrentLevel.GridMode.Inactivate;
    SuperLevel;
  end;
end;

function TControlsBuilder.SetDirection(
  ADirection: TControlsBuilderDirection): TControlsBuilder;
begin
  Result := Self;
  CurrentLevel.Direction := ADirection;
end;

function TControlsBuilder.SetHorizontalSpace(
  AHorizontalSpace: Single): TControlsBuilder;
begin
  Result := Self;
  CurrentLevel.HorizontalSpace := AHorizontalSpace;
end;

function TControlsBuilder.SetLeft(AControlName: string): TControlsBuilder;
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

function TControlsBuilder.SetLeft(ALeft: Single): TControlsBuilder;
begin
  Result := Self;
  CurrentLevel.CurrentLeft := ALeft;
  CurrentLevel.InitialLeft := ALeft;
  CurrentLevel.MaxControlHeight := 0;
  CurrentLevel.MaxControlWidth := 0;
end;

function TControlsBuilder.External(const AProc: TControlsBuilderObjProc): TControlsBuilder;
begin
  Result := Self;
  if Assigned(AProc) then
    AProc(Self);
end;

function TControlsBuilder.External(const AProc: TControlsBuilderProc): TControlsBuilder;
begin
  Result := Self;
  if Assigned(AProc) then
    AProc(Self);
end;

function TControlsBuilder.SetSpace(AVerticalSpace,
  AHorizontalSpace: Single): TControlsBuilder;
begin
  Result := Self;
  CurrentLevel.VerticalSpace := AVerticalSpace;
  CurrentLevel.HorizontalSpace := AHorizontalSpace;
end;

function TControlsBuilder.SetTop(ATop: Single): TControlsBuilder;
begin
  Result := Self;
  CurrentLevel.CurrentTop := ATop;
  CurrentLevel.InitialTop := ATop;
  CurrentLevel.MaxControlHeight := 0;
  CurrentLevel.MaxControlWidth := 0;
end;

function TControlsBuilder.SetTop(AControlName: string): TControlsBuilder;
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

function TControlsBuilder.SetTopLeft(ATop, ALeft: Single): TControlsBuilder;
begin
  Result := Self;
  SetTop(ATop);
  SetLeft(ALeft);
end;

function TControlsBuilder.SetTopLeftNearControl(AControlName: string;
  APosition: TRelativePosition): TControlsBuilder;
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

function TControlsBuilder.SetTopLeftNearControls(
  AControlsNames: array of string;
  APosition: TRelativePosition): TControlsBuilder;
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

function TControlsBuilder.SetTopLeftNearGroup(const AGroupName: string;
  APosition: TRelativePosition): TControlsBuilder;
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

function TControlsBuilder.SetVerticalSpace(
  AVerticalSpace: Single): TControlsBuilder;
begin
  Result := Self;
  CurrentLevel.VerticalSpace := AVerticalSpace
end;

{$IFNDEF FRAMEWORK_FMX}
function TControlsBuilder.WithOwnerAndParent(AOwner: TComponent;
  AParent: TWinControl): TControlsBuilder;
begin
  Result := Self;
  FOwner := AOwner;
  CurrentLevel.Parent := AParent;
end;
{$ENDIF}

{$IFDEF FRAMEWORK_FMX}
function TControlsBuilder.WithOwnerAndParent(AOwner: TComponent;
  AParent: TFmxObject): TControlsBuilder;
begin
  Result := Self;
  FOwner := AOwner;
  CurrentLevel.Parent := TControl(AParent);
end;
{$ENDIF}

function TControlsBuilder.WithParent(AParent: TWinControl): TControlsBuilder;
begin
  Result := Self;
  CurrentLevel.Parent := AParent;
end;

function TControlsBuilder.SubLevel(
  AControlInfo: TControlInfo;
  AGroupName: string
): TControlsBuilder;
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

  if CurrentLevel.GridMode.Active then
    SetupControlInfoForGridMode(AControlInfo);

  Control := CreateControl(AControlInfo, OwnerToUse);

  if not CurrentLevel.GridMode.Active then
    AddControl(TControlInfo.Create(Control))
  else
    MoveTopLeftAfterControl(Control);

  SubLevel(AGroupName);

  WithParent(
    {$IFDEF FRAMEWORK_FMX}Control
    {$ELSE}TWinControl(Control)
    {$ENDIF}
  );

  SetTopLeft(0, 0);
end;

function TControlsBuilder.SubLevel(AControlInfo: TControlInfo;
  ADirection: TControlsBuilderDirection; AGroupName: string): TControlsBuilder;
begin
  Result := SubLevel(AControlInfo, AGroupName);
  SetDirection(ADirection);
end;

function TControlsBuilder.SiblingSubLevel(AControlInfo: TControlInfo;
  AGroupName: string; ABreak: Boolean): TControlsBuilder;
begin
  Result := SuperLevel;
  if ABreak then
    Break;
  SubLevel(AControlInfo, AGroupName);
end;

function TControlsBuilder.SiblingSubLevel(AControlInfo: TControlInfo;
  ADirection: TControlsBuilderDirection; AGroupName: string;
  ABreak: Boolean): TControlsBuilder;
begin
  Result := SuperLevel;
  if ABreak then
    Break;
  SubLevel(AControlInfo, ADirection, AGroupName);
end;

function TControlsBuilder.SiblingSubLevel(AControlInfo: TControlInfo;
  ABreak: Boolean): TControlsBuilder;
begin
  Result := SiblingSubLevel(AControlInfo, '', ABreak);
end;

function TControlsBuilder.SiblingSubLevel(AControlInfo: TControlInfo;
  ADirection: TControlsBuilderDirection;
  ABreak: Boolean): TControlsBuilder;
begin
  Result := SiblingSubLevel(AControlInfo, ADirection, '', ABreak);
end;

function TControlsBuilder.SiblingSubLevelWithBreak(
  AControlInfo: TControlInfo; ADirection: TControlsBuilderDirection;
  AGroupName: string): TControlsBuilder;
begin
  Result := SiblingSubLevel(AControlInfo, ADirection, AGroupName, True);
end;

function TControlsBuilder.SiblingSubLevelWithBreak(
  AControlInfo: TControlInfo; AGroupName: string): TControlsBuilder;
begin
  Result := SiblingSubLevel(AControlInfo, AGroupName, True);
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

{ TGridCellCoord }

class function TGridCellCoord.Create(ARow, ACol: Integer): TGridCellCoord;
begin
  Result.Row := ARow;
  Result.Col := ACol;
end;

{ TGridMode }

procedure TGridMode.SetCellHeight(AValue: Single);
begin
  if FCellHeight = AValue then Exit;
  FCellHeight := AValue;
end;

procedure TGridMode.SetCellPosition(AValue: TCellPosition);
begin
  if FCellPosition = AValue then Exit;
  FCellPosition := AValue;
end;

procedure TGridMode.SetCellStrech(AValue: TCellStrech);
begin
  if FCellStrech = AValue then Exit;
  FCellStrech := AValue;
end;

function TGridMode.GetNextCol: Integer;
var
  Row: Integer;
begin
  PeekNext(Row, Result);
end;

function TGridMode.GetNextRow: Integer;
var
  Col: Integer;
begin
  PeekNext(Result, Col);
end;

procedure TGridMode.SetCellWidth(AValue: Single);
begin
  if FCellWidth = AValue then Exit;
  FCellWidth := AValue;
end;

function TGridMode.PeekNext(out NextRow, NextCol: Integer): Boolean;
begin
  Result := True;

  NextRow := FCurrentRow;
  NextCol := FCurrentCol;

  case FDirection of
    gfdRowFirst:
      begin
        Inc(NextCol);
        if NextCol >= Cols then
        begin
          NextCol := 0;
          Inc(NextRow);
        end;
      end;

    gfdColFirst:
      begin
        Inc(NextRow);
        if NextRow >= Rows then
        begin
          NextRow := 0;
          Inc(NextCol);
        end;
      end;
  end;

  // fora dos limites
  if (NextRow >= Rows) or (NextCol >= Cols) then
    Result := False;
end;

procedure TGridMode.ExpandIfNeeded(ARow, ACol: Integer);
begin
  if AutoExpand = [] then
    Exit;

  case Direction of
    gfdRowFirst:
      begin
        if (gaeRows in AutoExpand) and (ARow >= (FRows - 1)) then
          Inc(FRows);
      end;

    gfdColFirst:
      begin
        if (gaeCols in AutoExpand) and (ACol >= (FCols - 1)) then
          Inc(FCols);
      end;
  end;
end;


procedure TGridMode.ExpandForSpan(ARow, ACol, ARowSpan, AColSpan: Integer);
var
  RequiredRows, RequiredCols: Integer;
begin
  if AutoExpand = [] then
    Exit;

  RequiredRows := ARow + ARowSpan;
  RequiredCols := ACol + AColSpan;

  if (gaeRows in AutoExpand) and (RequiredRows > FRows) then
    FRows := RequiredRows;

  if (gaeCols in AutoExpand) and (RequiredCols > FCols) then
    FCols := RequiredCols;
end;


function TGridMode.Next: Boolean;
var
  Row, Col: Integer;
begin
  Result := PeekNext(Row, Col);

  if not Result then
  begin
    ExpandIfNeeded(CurrentRow, CurrentCol);
    Result := PeekNext(Row, Col);
  end;

  if not Result then
    Exit;

  FCurrentRow := Row;
  FCurrentCol := Col;

  Result := True;
end;

procedure TGridMode.SetColOffset(ACol: Integer; AOffset: Single);
begin
  if not Active then
    Exit;
  FColOffsets.AddOrSetValue(ACol, AOffset);
end;

procedure TGridMode.SetColSpan(AValue: Integer);
begin
  if FColSpan = AValue then Exit;
  FColSpan := AValue;
end;

procedure TGridMode.SetDirection(AValue: TGridFillDirection);
begin
  if FDirection = AValue then Exit;
  FDirection := AValue;
end;

function TGridMode.GetRowSpanForFill: Integer;
var
  MaxSpan: Integer;
begin
  if (gaeRows in AutoExpand) then
  begin
    // com autoexpand em linhas não limita pelo tamanho atual
    if FRowSpan < 1 then
      Result := 1
    else
      Result := FRowSpan;
  end
  else
  begin
    MaxSpan := (FRows - CurrentRow);

    if FRowSpan > MaxSpan then
      Result := MaxSpan
    else if FRowSpan < 1 then
      Result := 1
    else
      Result := FRowSpan;
  end;
end;


function TGridMode.GetColOffset(ACol: Integer): Single;
begin
  Result := 0;
  if not Active then
    Exit;
  if not FColOffsets.TryGetValue(ACol, Result) then
    Result := 0;
end;

function TGridMode.GetColSpanForFill: Integer;
var
  MaxSpan: Integer;
begin
  if (gaeCols in AutoExpand) then
  begin
    if FColSpan < 1 then
      Result := 1
    else
      Result := FColSpan;
  end
  else
  begin

    MaxSpan := (FCols - CurrentCol);

    if FColSpan > MaxSpan then
      Result := MaxSpan
    else if FColSpan < 1 then
      Result := 1
    else
      Result := FColSpan;
  end;
end;

procedure TGridMode.SetRowSpan(AValue: Integer);
begin
  if FRowSpan = AValue then Exit;
  FRowSpan := AValue;
end;

constructor TGridMode.Create;
begin
  FActive := False;
  FOccupation := TCellCordStatusDictionary.Create;
  FAutoExpand := [];
  FCellStrech := [csVertical, csHorizontal];
  FCellPosition := cpCenter;
end;

function TGridMode.Step(out ARowSpan, AColSpan: Integer; out ARow, ACol: Integer; AMark: Boolean): Boolean;
var
  I: Integer;
  AdvanceSpan: Integer;
begin
  Result := False;

  while not IsCellFree(CurrentRow, CurrentCol) do
    if not Next then
      Exit;

  ARowSpan := GetRowSpanForFill;
  AColSpan := GetColSpanForFill;

  ExpandForSpan(CurrentRow, CurrentCol, ARowSpan, AColSpan);


  if FDirection = gfdRowFirst then
    AdvanceSpan := AColSpan
  else
    AdvanceSpan := ARowSpan;

  if AMark then
  begin
    MarkOccupied(CurrentRow, CurrentCol, ARowSpan, AColSpan);

    ARow := CurrentRow;
    ACol := CurrentCol;

    for I := 1 to AdvanceSpan - 1 do
      if not Next then
        Exit;
  end
  else
  begin
    if not Next then
      Exit;

    ARow := CurrentRow;
    ACol := CurrentCol;
  end;

  Result := True;
end;


function TGridMode.CalcSpanRect(ARow, ACol, ARowSpan, AColSpan: Integer;
      AHorizontalSpace, AVerticalSpace: Single;
      out R: {$IFDEF FRAMEWORK_FMX}TRectF{$ELSE}TRect{$ENDIF}): Boolean;
var
  W, H: Single;
  I: Integer;
begin
  if (ARow < 0) or (ACol < 0) then
    Exit(False);

  H := AVerticalSpace * (ARowSpan - 1);
  for I := 0 to ARowSpan - 1 do
    H := H + GetRowHeight(ARow + I);

  W := AHorizontalSpace * (AColSpan - 1);
  for I := 0 to AColSpan - 1 do
    W := W + GetColWidth(ACol + I);

  {$IFDEF FRAMEWORK_FMX}
  R := RectF(0, 0, W, H);
  {$ELSE}
  R := Rect(0, 0, Trunc(W), Trunc(H));
  {$ENDIF}

  Result := True;
end;

procedure TGridMode.ResetOccupation;
begin
  FOccupation.Clear;
end;

procedure TGridMode.MarkOccupied(ARow, ACol, ARowSpan, AColSpan: Integer);
var
  R, C: Integer;
begin
  for R := ARow to ARow + ARowSpan - 1 do
    for C := ACol to ACol + AColSpan - 1 do
      FOccupation.AddOrSetValue(TGridCellCoord.Create(R, C), csOccupied);
end;

function TGridMode.IsCellFree(ARow, ACol: Integer): Boolean;
var
  Status: TGridCellStatus;
begin
  Result := True;
  if not FOccupation.TryGetValue(TGridCellCoord.Create(ARow, ACol), Status) then
    Exit;

  Result := Status = csEmpty;
end;

procedure TGridMode.ResetSpans;
begin
  RowSpan := 1;
  ColSpan := 1;
end;

function TGridMode.AdjustRectForCellLayout(
  const CellRect: {$IFDEF FRAMEWORK_FMX}TRectF{$ELSE}TRect{$ENDIF};
  AControlWidth, AControlHeight: Single
): {$IFDEF FRAMEWORK_FMX}TRectF{$ELSE}TRect{$ENDIF};
var
  FinalW, FinalH: Single;
  X, Y: Single;
begin
  // --- largura ---
  if csHorizontal in FCellStrech then
    FinalW := CellRect.Width
  else
    FinalW := AControlWidth;

  // --- altura ---
  if csVertical in FCellStrech then
    FinalH := CellRect.Height
  else
    FinalH := AControlHeight;

  // --- posição horizontal ---
  case FCellPosition of
    cpLeft, cpTopLeft, cpBottomLeft:
      X := CellRect.Left;
    cpRight, cpTopRight, cpBottomRight:
      X := CellRect.Right - FinalW;
  else
    // centraliza (default)
    X := CellRect.Left + (CellRect.Width - FinalW) / 2;
  end;

  // --- posição vertical ---
  case FCellPosition of
    cpTop, cpTopLeft, cpTopRight:
      Y := CellRect.Top;
    cpBottom, cpBottomLeft, cpBottomRight:
      Y := CellRect.Bottom - FinalH;
  else
    // centraliza (default)
    Y := CellRect.Top + (CellRect.Height - FinalH) / 2;
  end;

  {$IFDEF FRAMEWORK_FMX}
    Result := TRectF.Create(X, Y, X + FinalW, Y + FinalH);
  {$ELSE}
    Result := TRect.Create(Trunc(X), Trunc(Y), Trunc(X + FinalW), Trunc(Y + FinalH));
  {$ENDIF}
end;

function TGridMode.CalcCellRectAbsolute(
  ARow, ACol, ARowSpan, AColSpan: Integer;
  AHorizontalSpace, AVerticalSpace: Single;
  out R: {$IFDEF FRAMEWORK_FMX}TRectF{$ELSE}TRect{$ENDIF}
): Boolean;
var
  Col, Row: Integer;
  LeftPos, TopPos, W, H: Single;
begin
  Result := False;

  if (ARow < 0) or (ACol < 0) then
    Exit;
  if (ARow >= Rows) or (ACol >= Cols) then
    Exit;
  if ARowSpan < 1 then
    ARowSpan := 1;
  if AColSpan < 1 then
    AColSpan := 1;

  if AutoExpand = [] then
    if (ARow + ARowSpan - 1 >= Rows) or (ACol + AColSpan - 1 >= Cols) then
      Exit;

  // calcula Left: soma larguras+espacos das colunas anteriores
  leftPos := FOriginLeft + GetRowOffset(ARow);
  for Col := 0 to ACol - 1 do
    LeftPos := LeftPos + GetColWidth(col) + AHorizontalSpace;

  // calcula Top: soma alturas+espacos das linhas anteriores
  TopPos := FOriginTop + GetColOffset(ACol);
  for Row := 0 to ARow - 1 do
    TopPos := TopPos + GetRowHeight(row) + AVerticalSpace;

  // calcula Width (colspan)
  W := 0.0;
  for col := ACol to ACol + AColSpan - 1 do
    W := W + GetColWidth(col);
  W := W + AHorizontalSpace * (AColSpan - 1);

  // calcula Height (rowspan)
  H := 0.0;
  for row := ARow to ARow + ARowSpan - 1 do
    H := H + GetRowHeight(row);
  H := H + AVerticalSpace * (ARowSpan - 1);

  {$IFDEF FRAMEWORK_FMX}
  R := RectF(LeftPos, TopPos, LeftPos + W, TopPos + H);
  {$ELSE}
  R := Rect(Trunc(LeftPos), Trunc(TopPos), Trunc(LeftPos + W), Trunc(TopPos + H));
  {$ENDIF}

  Result := True;
end;

destructor TGridMode.Destroy;
begin
  FOccupation.Free;
  if Assigned(FColWidths) then
    FColWidths.Free;
  if Assigned(FRowHeights) then
    FRowHeights.Free;
  if Assigned(FColOffsets) then
    FColOffsets.Free;
  if Assigned(FRowOffsets) then
    FRowOffsets.Free;
  inherited;
end;

procedure TGridMode.SetColWidth(ACol: Integer; AWidth: Single);
begin
  if not Active then
    Exit;
  FColWidths.AddOrSetValue(ACol, AWidth);
end;

procedure TGridMode.SetRowHeight(ARow: Integer; AHeight: Single);
begin
  if not Active then
    Exit;
  FRowHeights.AddOrSetValue(ARow, AHeight);
end;

procedure TGridMode.SetRowOffset(ARow: Integer; AOffset: Single);
begin
  if not Active then
    Exit;
  FRowOffsets.AddOrSetValue(ARow, AOffset);
end;

function TGridMode.GetColWidth(ACol: Integer): Single;
begin
  Result := 0;
  if not Active then
    Exit;
  if not FColWidths.TryGetValue(ACol, Result) then
    Result := CellWidth;
end;

function TGridMode.GetRowHeight(ARow: Integer): Single;
begin
  Result := 0;
  if not Active then
    Exit;
  if not FRowHeights.TryGetValue(ARow, Result) then
    Result := CellHeight;
end;

function TGridMode.GetRowOffset(ARow: Integer): Single;
begin
  Result := 0;
  if not Active then
    Exit;
  if not FRowOffsets.TryGetValue(ARow, Result) then
    Result := 0;
end;

procedure TGridMode.Activate(ARows, ACols: Integer; AOriginLeft, AOriginTop: Single);
begin
  if Active then
    Exit;
  FActive := True;
  FRows := ARows;
  FCols := ACols;
  FOriginLeft := AOriginLeft;
  FOriginTop := AOriginTop;
  CellWidth := 0;
  CellHeight := 0;
  RowSpan := 1;
  ColSpan := 1;
  FCurrentRow := 0;
  FCurrentCol := 0;
  FColWidths := TIntSingleDictionary.Create;
  FRowHeights := TIntSingleDictionary.Create;
  FColOffsets := TIntSingleDictionary.Create;
  FRowOffsets := TIntSingleDictionary.Create;
  ResetOccupation;
end;

procedure TGridMode.Inactivate;
begin
  if not Active then
    Exit;
  FActive := False;
  if Assigned(FColWidths) then
    FreeAndNil(FColWidths);
  if Assigned(FRowHeights) then
    FreeAndNil(FRowHeights);
end;

{ TControlsBuilderLevel }

function TControlsBuilderLevel.Clone: TControlsBuilderLevel;
begin
  Result := TControlsBuilderLevel.Create;
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
  Result.ControlWidth := TOptionalSingle.None;
  Result.ControlHeight := TOptionalSingle.None;
end;

constructor TControlsBuilderLevel.Create;
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
  ControlWidth := TOptionalSingle.None;
  ControlHeight := TOptionalSingle.None;
  GridMode := TGridMode.Create;
  GridMode.Inactivate;
  Inc(FGroupCounter);
  GroupName := '__LEVEL_GROUP_' + IntToStr(FGroupCounter - 1) + '__';
end;

destructor TControlsBuilderLevel.Destroy;
begin
  GridMode.Free;
  inherited Destroy;
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

procedure TComponentRegistry.CheckRelease;
var
  Entry: TComponentRegistryEntry;
  Key: string;
begin
  for Key in FInstances.Keys do
    if FInstances[Key].Registry = Self then
    begin
      Entry := FInstances[Key];

      if (Entry.RefCount <= 0)
        and (Entry.Registry.RegistryLifetime = rlTransient) then
      begin
        Free; // vai destruir a si mesmo
        FInstances.Remove(Key);
      end;

      Break;
    end;

  if FInstances.Count = 0 then
  begin
    FInstances.Free;
    FInstances := nil;
  end;
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
  FRegistryLifetime := rlTransient;
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

class procedure TComponentRegistry.Finalize;
var
  Entry: TComponentRegistryEntry;
begin
  if Assigned(TComponentRegistry.FInstances) then
  begin
    for Entry in TComponentRegistry.FInstances.Values do
      Entry.Registry.Free;
    TComponentRegistry.FInstances.Free;
  end;
end;

function TComponentRegistry.UniqueName(const ABaseName: string): string;
var
  Index: Integer;
  Candidate: string;
begin
  if ABaseName.IsEmpty then
    Exit('');

  Candidate := ABaseName;
  Index := 1;

  while FNamedComponents.ContainsKey(Candidate) do
  begin
    Candidate := ABaseName + IntToStr(Index);
    Inc(Index);
  end;

  Result := Candidate;
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
    FInstances[AKey] := Entry;  // reinsere Entry com o novo RefCont

    if Entry.RefCount <= 0 then
      Entry.Registry.CheckRelease;
  end;
end;

procedure TComponentRegistry.SetRegistryLifetime(
  const Value: TRegistryLifetime);
begin
  if FRegistryLifetime <> Value then
  begin
    FRegistryLifetime := Value;
    CheckRelease; // garante destruição se precisar
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

function TComponentRegistry.GetContextKey: string;
var
  Pair: {$IFDEF FPC}specialize{$ENDIF} TPair<string, TComponentRegistryEntry>;
begin
  Result := '';
  for Pair in FInstances do
  begin
    if Pair.Value.Registry = Self then
    begin
      Result := Pair.Key;
      Exit;
    end;
  end;
end;

{ TComponentBuilder }

function TComponentBuilder.Add(
  AComponentInfo: TComponentInfo): TComponentBuilder;
var
  Component: TComponent;
  ComponentName: string;
begin
  Result := Self;
  ComponentName := AComponentInfo.Name;
  if not ComponentName.IsEmpty then
    ComponentName := Registry.UniqueName(AComponentInfo.Name);
  Component := AComponentInfo.CreateComponent(FOwner, ComponentName);
  Registry.AddComponent(Component, Component.Name);
end;

function TComponentBuilder.Add(
  AComponentInfos: TComponentInfoArray): TComponentBuilder;
var
  I: Integer;
begin
  Result := Self;
  for I := 0 to High(AComponentInfos) do
    Add(AComponentInfos[I]);
end;

constructor TComponentBuilder.Create(ARegistryContextKey: string);
begin
  Create(TRegistryContextHandle.Create(ARegistryContextKey))
end;

constructor TComponentBuilder.Create(ARegistryContextHandle: IRegistryContextHandle);
begin
  FRegistryContextHandle := ARegistryContextHandle;
end;

destructor TComponentBuilder.Destroy;
begin
  inherited;
end;

function TComponentBuilder.GetComponent(const AName: string): TComponent;
begin
  Result := Registry.GetComponent(AName);
end;

function TComponentBuilder.External(const AProc: TComponentBuilderObjProc): TComponentBuilder;
begin
  Result := Self;
  if Assigned(AProc) then
    AProc(Self);
end;

function TComponentBuilder.External(const AProc: TComponentBuilderProc): TComponentBuilder;
begin
  Result := Self;
  if Assigned(AProc) then
    AProc(Self);
end;

{$IFDEF FPC}generic{$ENDIF}
function TComponentBuilder.GetComponent<T>(const AName: string): T;
begin
  Result := Registry.GetComponent<T>(AName);
end;

function TComponentBuilder.GetComponentRegistry: TComponentRegistry;
begin
  Result := FRegistryContextHandle.GetRegistry;
end;

function TComponentBuilder.GetComponents: TComponentList;
begin
  Result := Registry.FComponents;
end;

function TComponentBuilder.GetItem(const AName: string): TComponent;
begin
  Result := Self.GetComponent(AName);
end;

function TComponentBuilder.WithOwner(AOwner: TComponent): TComponentBuilder;
begin
  Result := Self;
  FOwner := AOwner;
end;

{ TComponentInfo }

function TComponentInfo.CreateComponent(AOwner: TComponent;
  const AComponentName: string): TComponent;
begin
  try
    if Assigned(Component) then
      Result := Component
    else
      Result := ComponentClass.Create(AOwner);

    if not AComponentName.IsEmpty then
      Result.Name := AComponentName;

    Result.Tag := FTag;

    if Assigned(FTargetField) then
      PPointer(FTargetField)^ := Result;

    if Assigned(SetupProc) then
      SetupProc(Result);
  finally
    Free;
  end;
end;

constructor TComponentInfo.Create(AClass: TComponentClass; const AName: string);
begin
  FComponent := nil;
  FComponentClass := AClass;
  FName := AName;
  FTag := 0;
  FSetupProc := nil;
end;

constructor TComponentInfo.Create(AClass: TComponentClass; const AName: string;
  out Reference);
begin
  Create(AClass, AName);
  Assign(Reference);
end;

constructor TComponentInfo.Create(AComponent: TComponent);
begin
  FComponent := AComponent;
  FComponentClass := TComponentClass(AComponent.ClassType);
  FName := AComponent.Name;
  FTag := 0;
  FSetupProc := nil;
end;

constructor TComponentInfo.Create(AComponentClass: TComponentClass; out Reference);
begin
  Create(AComponentClass);
  Assign(Reference);
end;

function TComponentInfo.Assign(out Reference): TComponentInfo;
begin
  Result := Self;
  FTargetField := @Reference;
end;

function TComponentInfo.Setup(AProc: TComponentSetupProc): TComponentInfo;
begin
  Result := Self;
  FSetupProc := AProc;
end;

function TComponentInfo.WithName(AName: string): TComponentInfo;
begin
  Result := Self;
  FName := AName;
end;

function TComponentInfo.WithTag(ATag: NativeInt): TComponentInfo;
begin
  Result := Self;
  FTag := ATag;
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

class function TRegistryContextHandle.GenerateAutoKey: string;
begin
  Inc(FAutoCounter);
  Result := 'Auto_' + IntToStr(FAutoCounter);
end;

constructor TRegistryContextHandle.Create(const AContextKey: string);
begin
  inherited Create;
  FIsReleased := False;
  FContextKey := AContextKey;
  FRegistry := TComponentRegistry.ForContext(AContextKey);
end;

constructor TRegistryContextHandle.Create;
begin
  Create(GenerateAutoKey);
end;

destructor TRegistryContextHandle.Destroy;
begin
  if not FIsReleased then
    TComponentRegistry.ReleaseContext(FContextKey);
  inherited;
end;

function TRegistryContextHandle.GetRegistry: TComponentRegistry;
begin
  Result := nil;
  if not FIsReleased then
    Result := FRegistry;
end;

procedure TRegistryContextHandle.ReleaseContext;
begin
  if not FIsReleased then
    FRegistry.ReleaseContext(FContextKey)
end;

{ TOPCBBuilders }

function TOPCBBuilders.AsComponentBuilder: TComponentBuilder;
begin
  if not Assigned(FComponentBuilder) then
    FComponentBuilder := TComponentBuilder.Create(FRegistryContextHandle);

  Result := FComponentBuilder;
end;

function TOPCBBuilders.AsControlBuilder: TControlsBuilder;
begin
  if not Assigned(FControlBuilder) then
    FControlBuilder := TControlsBuilder.Create(FRegistryContextHandle);

  Result := FControlBuilder;
end;

function TOPCBBuilders.AsMenuBuilder: TMenuBuilder;
begin
  if not Assigned(FMenuBuilder) then
    FMenuBuilder := TMenuBuilder.Create(FRegistryContextHandle);
  Result := FMenuBuilder;
end;

constructor TOPCBBuilders.Create(const ARegistryContextKey: string);
begin
  FRegistryContextHandle := TRegistryContextHandle.Create(ARegistryContextKey);
  FComponentBuilder := nil;
  FControlBuilder := nil;
  FMenuBuilder := nil;
end;

destructor TOPCBBuilders.Destroy;
begin
  if Assigned(FComponentBuilder) then
    FComponentBuilder.Free;

  if Assigned(FControlBuilder) then
    FControlBuilder.Free;

  if Assigned(FMenuBuilder) then
    FMenuBuilder.Free;

  inherited;
end;

{ TComponentInfoHelper }

class function TComponentInfoHelper.CreateArray(AClass: TComponentClass;
  const ANames: array of string): TComponentInfoArray;
var
  I: Integer;
begin
  Result := [];
  SetLength(Result, Length(ANames));
  for I := 0 to High(ANames) do
    Result[I] := TComponentInfo.Create(AClass, ANames[I]);
end;

{ TMenuBuilder }

function TMenuBuilder.AddMenu(AMenuInfo: TMenuInfo): TMenuBuilder;
var
  Menu: TMenu;
  MenuName: string;
begin
  Result := Self;
  MenuName := AMenuInfo.Name;
  if not MenuName.IsEmpty then
    MenuName := Registry.UniqueName(AMenuInfo.Name);
  Menu := AMenuInfo.CreateMenu(FOwner, MenuName);
  Registry.AddComponent(Menu, Menu.Name);

  {$IFDEF FRAMEWORK_FMX}
  CurrentLevel.Parent := Menu;
  if FOwner is TFmxObject then
    (FOwner as TFmxObject).AddObject(Menu);
  {$ELSE}
  CurrentLevel.Parent := Menu.Items;
  {$ENDIF}
end;

function TMenuBuilder.AddMenuItem(AMenuItemInfo: TMenuItemInfo): TMenuBuilder;
var
  MenuItem: TMenuItem;
  MenuItemName: string;
begin
  Result := Self;
  MenuItemName := AMenuItemInfo.Name;
  if not MenuItemName.IsEmpty then
    MenuItemName := Registry.UniqueName(AMenuItemInfo.Name);
  MenuItem := AMenuItemInfo.CreateMenuItem(FOwner, MenuItemName);
  Registry.AddComponent(MenuItem, MenuItem.Name);

  {$IFDEF FRAMEWORK_FMX}
  CurrentLevel.Parent.AddObject(MenuItem);
  {$ELSE}
  CurrentLevel.Parent.Add(MenuItem);
  {$ENDIF}
end;

constructor TMenuBuilder.Create(ARegistryContextKey: string);
begin
  Create(TRegistryContextHandle.Create(ARegistryContextKey));
end;

constructor TMenuBuilder.Create(ARegistryContextHandle: IRegistryContextHandle);
begin
  FRegistryContextHandle := ARegistryContextHandle;
  FLevelStack := TMenuBuilderLevelStack.Create(True);
  FLevelStack.Add(TMenuBuilderLevel.Create);
end;

destructor TMenuBuilder.Destroy;
begin
  FLevelStack.Free;
  inherited;
end;

function TMenuBuilder.GetComponentRegistry: TComponentRegistry;
begin
  Result := FRegistryContextHandle.GetRegistry;
end;

function TMenuBuilder.GetCurrenteLevel: TMenuBuilderLevel;
begin
  Result := FLevelStack.Last;
end;

function TMenuBuilder.GetMenu(const AName: string): TMenu;
begin
  Result := Registry.GetComponent(AName) as TMenu;
end;

{$IFDEF FPC}generic{$ENDIF}
function TMenuBuilder.GetMenu<T>(const AName: string): T;
begin
  Result := Registry.GetComponent<T>(AName);
end;

function TMenuBuilder.GetMenuItem(const AName: string): TMenuItem;
begin
  Result := Registry.GetComponent(AName) as TMenuItem;
end;

{$IFDEF FPC}generic{$ENDIF}
function TMenuBuilder.GetMenuItem<T>(const AName: string): T;
begin
  Result := Registry.GetComponent<T>(AName);
end;

function TMenuBuilder.SubLevel(AMenuItemInfo: TMenuItemInfo): TMenuBuilder;
var
  MenuItem: TMenuItem;
  MenuItemName: string;
begin
  Result := Self;
  MenuItemName := AMenuItemInfo.Name;
  if not MenuItemName.IsEmpty then
    MenuItemName := Registry.UniqueName(AMenuItemInfo.Name);
  MenuItem := AMenuItemInfo.CreateMenuItem(FOwner, MenuItemName);
  Registry.AddComponent(MenuItem, MenuItem.Name);

  {$IFDEF FRAMEWORK_FMX}
  CurrentLevel.Parent.AddObject(MenuItem);
  {$ELSE}
  CurrentLevel.Parent.Add(MenuItem);
  {$ENDIF}

  FLevelStack.Add(TMenuBuilderLevel.Create);
  CurrentLevel.Parent := MenuItem;
end;

function TMenuBuilder.SuperLevel: TMenuBuilder;
begin
  if FLevelStack.Count <= 1 then
    raise Exception.Create('PreviousLevel chamado no nível raiz');

  FLevelStack.Delete(FLevelStack.Count - 1); // remove nível atual
  Result := Self;
end;

function TMenuBuilder.WithOwner(AOwner: TComponent): TMenuBuilder;
begin
  Result := Self;
  FOwner := AOwner;
end;

function TMenuBuilder.External(const AProc: TMenuBuilderObjProc): TMenuBuilder;
begin
  Result := Self;
  if Assigned(AProc) then
    AProc(Self);
end;

function TMenuBuilder.External(const AProc: TMenuBuilderProc): TMenuBuilder;
begin
  Result := Self;
  if Assigned(AProc) then
    AProc(Self);
end;

{ TMenuInfo }

function TMenuInfo.CreateMenu(AOwner: TComponent; const AMenuName: string): TMenu;
begin
  try
    if Assigned(Menu) then
      Result := Menu
    else
      Result := MenuClass.Create(AOwner);

    if not AMenuName.IsEmpty then
      Result.Name := AMenuName;

    Result.Tag := FTag;

    if Assigned(FTargetField) then
      PPointer(FTargetField)^ := Result;

    if Assigned(SetupProc) then
      SetupProc(Result);
  finally
    Free;
  end;
end;

constructor TMenuInfo.Create(AClass: TMenuClass; const AName: string);
begin
  FMenu := nil;
  FMenuClass := AClass;
  FName := AName;
  FTag := 0;
  FSetupProc := nil;
  FTargetField := nil;
end;

constructor TMenuInfo.Create(AClass: TMenuClass; const AName: string; out
  Reference);
begin
  Create(AClass, AName);
  Assign(Reference);
end;

constructor TMenuInfo.Create(AMenu: TMenu);
begin
  FMenu := AMenu;
  FMenuClass := TMenuClass(AMenu.ClassType);
  FName := AMenu.Name;
  FTag := 0;
  FSetupProc := nil;
  FTargetField := nil;
end;

constructor TMenuInfo.Create(AClass: TMenuClass; out Reference);
begin
  Create(AClass, '');
  Assign(Reference);
end;

function TMenuInfo.Setup(AProc: TMenuSetupProc): TMenuInfo;
begin
  Result := Self;
  FSetupProc := AProc;
end;

function TMenuInfo.WithName(AName: string): TMenuInfo;
begin
  Result := Self;
  FName := AName;
end;

function TMenuInfo.WithTag(ATag: NativeInt): TMenuInfo;
begin
  Result := Self;
  FTag := ATag;
end;

function TMenuInfo.Assign(out Reference): TMenuInfo;
begin
  Result := Self;
  FTargetField := @Reference;
end;

{ TMenuItemInfo }

constructor TMenuItemInfo.Create(AMenuItem: TMenuItem);
begin
  FMenuItem := AMenuItem;
  FMenuItemClass := TMenuItemClass(AMenuItem.ClassType);
  FName := AMenuItem.Name;
  FTag := 0;
  FCaption := TOptionalString.None;
  FImageIndex := TOptionalInteger.None;
  FSetupProc := nil;
  FTargetField := nil;
end;

constructor TMenuItemInfo.Create(AClass: TMenuItemClass; out Reference);
begin
  Create(AClass, '');
  Assign(Reference);
end;

constructor TMenuItemInfo.Create;
begin
  Create(TMenuItem);
end;

constructor TMenuItemInfo.Create(out Reference);
begin
  Create(TMenuItem, Reference);
end;

constructor TMenuItemInfo.Create(AClass: TMenuItemClass; const AName: string);
begin
  FMenuItem := nil;
  FMenuItemClass := AClass;
  FName := AName;
  FTag := 0;
  FCaption := TOptionalString.None;;
  FImageIndex := TOptionalInteger.None;
  FSetupProc := nil;
  FTargetField := nil;
end;

constructor TMenuItemInfo.Create(AClass: TMenuItemClass; const AName: string;
  out Reference);
begin
  Create(AClass, AName);
  Assign(Reference);
end;

function TMenuItemInfo.CreateMenuItem(AOwner: TComponent;
  const AMenuItemName: string): TMenuItem;
begin
  try
    if Assigned(MenuItem) then
      Result := MenuItem
    else
      Result := MenuItemClass.Create(AOwner);

    if not AMenuItemName.IsEmpty then
      Result.Name := AMenuItemName;

    Result.Tag := FTag;

    if Caption.HasValue then
      {$IFDEF FRAMEWORK_FMX}
      Result.Text := Caption.Value;
      {$ELSE}
      Result.Caption := Caption.Value;
      {$ENDIF}

    if ImageIndex.HasValue then
      Result.ImageIndex := ImageIndex.Value;

    Result.OnClick := OnClick;

    if Assigned(FTargetField) then
      PPointer(FTargetField)^ := Result;

    if Assigned(SetupProc) then
      SetupProc(Result);
  finally
    Free;
  end;
end;

function TMenuItemInfo.Setup(AProc: TMenuItemSetupProc): TMenuItemInfo;
begin
  Result := Self;
  FSetupProc := AProc;
end;

function TMenuItemInfo.WithCaption(ACaption: string): TMenuItemInfo;
begin
  Result := Self;
  FCaption := ACaption;
end;

function TMenuItemInfo.WithImageIndex(AImageIndex: Integer): TMenuItemInfo;
begin
  Result := Self;
  FImageIndex := AImageIndex;
end;

function TMenuItemInfo.WithOnClick(AOnClick: TNotifyEvent): TMenuItemInfo;
begin
  Result := Self;
  FOnClick := AOnClick;
end;

function TMenuItemInfo.WithTag(ATag: NativeInt): TMenuItemInfo;
begin
  Result := Self;
  FTag := ATag;
end;

{$IFDEF FPC}generic{$ENDIF}
function TMenuItemInfo.Assign(out Reference): TMenuItemInfo;
begin
  Result := Self;
  FTargetField := @Reference;
end;

function TMenuItemInfo.WithName(AName: string): TMenuItemInfo;
begin
  Result := Self;
  FName := AName;
end;

initialization

finalization
  TComponentRegistry.Finalize;

end.

