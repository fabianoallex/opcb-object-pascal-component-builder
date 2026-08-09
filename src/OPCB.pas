unit OPCB;

{**************************************************************************
  This file is part of OPCB - Object Pascal Component Builder
  Copyright (c) 2025 Fabiano Arndt

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
**************************************************************************}

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$LONGSTRINGS ON}{$MODESWITCH TYPEHELPERS}{$MODESWITCH ADVANCEDRECORDS}
{$ENDIF}

interface

uses
  {$IFDEF FPC}Controls, ExtCtrls, Menus,
  {$ELSE}
    {$IFDEF FRAMEWORK_FMX}
    FMX.Controls, FMX.StdCtrls, Fmx.Types, FMX.ExtCtrls, FMX.TabControl,
    FMX.Forms, FMX.Menus, System.Types,
    {$ELSE}
    Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Menus, Types,
    {$ENDIF}
  {$ENDIF}
  Classes, SysUtils, Generics.Collections, Generics.Defaults, RTTI,
  TypInfo, OPCB.Optionals;

type
  {$IFNDEF FPC}
    {$IFDEF FRAMEWORK_FMX}
    TWinControl = TControl;
    TMenu = TFmxObject;
    {$ENDIF}
  {$ENDIF}

  TProtectedControl = class(TControl);

  TObjectClass = class of TObject;
  TControlClass = class of TControl;
  TWinControlClass = class of TWinControl;
  TMenuClass = class of TMenu;
  TMenuItemClass = class of TMenuItem;

  {$IFDEF FPC}generic{$ENDIF} TSetupProcObj<TBuild> = procedure(AObj: TBuild) of object;
  {$IFDEF FPC}generic{$ENDIF} TSetupRefProc<TBuild> = {$IFNDEF FPC}reference to{$ENDIF} procedure(AObj: TBuild);

  TObjectSetupProcObj = {$IFDEF FPC}specialize{$ENDIF} TSetupProcObj<TObject>;
  TObjectSetupRefProc = {$IFDEF FPC}specialize{$ENDIF} TSetupRefProc<TObject>;
  TComponentSetupProcObj = {$IFDEF FPC}specialize{$ENDIF} TSetupProcObj<TComponent>;
  TComponentSetupRefProc = {$IFDEF FPC}specialize{$ENDIF} TSetupRefProc<TComponent>;
  TMenuSetupProcObj = {$IFDEF FPC}specialize{$ENDIF} TSetupProcObj<TMenu>;
  TMenuSetupRefProc = {$IFDEF FPC}specialize{$ENDIF} TSetupRefProc<TMenu>;
  TMenuItemSetupProcObj = {$IFDEF FPC}specialize{$ENDIF} TSetupProcObj<TMenuItem>;
  TMenuItemSetupRefProc = {$IFDEF FPC}specialize{$ENDIF} TSetupRefProc<TMenuItem>;
  TControlSetupProcObj = {$IFDEF FPC}specialize{$ENDIF} TSetupProcObj<TControl>;
  TControlSetupRefProc = {$IFDEF FPC}specialize{$ENDIF} TSetupRefProc<TControl>;

  { TPropertySetup }

  {$IFDEF FPC}generic{$ENDIF}
  TPropertySetup<T, TControlType> = class abstract
  protected
    FValue: T;
  public
    constructor Create(const AValue: T);
    procedure Apply(AControl: TControlType); virtual; abstract;
  end;

  { TPropValue }

  TPropertyValue = record
    PropName: string;
    Value: TValue;
    {$IFDEF FPC}generic{$ENDIF}
    class function Create<E>(const APropName: string; const AValue: E): TPropertyValue; static;
  end;

  TPropValueList = {$IFDEF FPC}specialize{$ENDIF} TList<TPropertyValue>;

  TEventValue = record
    EventName: string;
    Method: TMethod;
  end;

  TEventValueList = {$IFDEF FPC}specialize{$ENDIF} TList<TEventValue>;

  {$IFDEF FPC}generic{$ENDIF}
  IObjectBuilder<TBuild> = interface
    ['{1FD2411F-05F7-4132-A10D-C06A2CA95781}']
    function Build: TBuild;
    procedure AssignReference(out Reference);
  end;

  { TObjectBuilderBase }

  // ATENÇÃO - posse de memória: esta classe (e todos os *Builder que dela
  // descendem: TObjectBuilder, TComponentBuilder, TControlBuilder,
  // TMenuBuilder, TMenuItemBuilder etc.) herda de TInterfacedObject e por
  // isso é contada por referência quando atribuída a uma variável de
  // interface (ex: IControlBuilder<T>) - que é exatamente o tipo de
  // parâmetro recebido por TControlCreator.Add/SubLevel/SiblingSubLevel e
  // equivalentes em TComponentCreator/TMenuCreator.
  //
  // Use o builder SEMPRE de forma efêmera, criado inline no próprio
  // parâmetro da chamada:
  //   Creator.Add(TControlBuilder.Create(TButton).WithCaption('OK'));
  // Esse é o único padrão seguro. Se, em vez disso, o builder for guardado
  // numa variável e passado para um desses métodos, ele é destruído
  // silenciosamente ao sair da chamada (a passagem por parâmetro de
  // interface derruba o refcount a zero) - qualquer uso posterior dessa
  // variável é acesso a memória já liberada, e chamar .Free nela depois
  // é liberar algo que já foi liberado. Não guarde um builder para reuso.
  {$IFDEF FPC}generic{$ENDIF}
  TObjectBuilderBase<TBuild: {$IFDEF FPC}TObject{$ELSE}class{$ENDIF}; TSelf: class> =
    class abstract(TInterfacedObject, {$IFDEF FPC}specialize{$ENDIF} IObjectBuilder<TBuild>)
  type
    TSetupProcObjBuild = {$IFDEF FPC}specialize{$ENDIF} TSetupProcObj<TBuild>;
    TSetupRefProcBuild = {$IFDEF FPC}specialize{$ENDIF} TSetupRefProc<TBuild>;
    TSetupProcObjList = {$IFDEF FPC}specialize{$ENDIF} TList<TSetupProcObjBuild>;
    TSetupRefProcList = {$IFDEF FPC}specialize{$ENDIF} TList<TSetupRefProcBuild>;
    TPointerList = {$IFDEF FPC}specialize{$ENDIF} TList<Pointer>;
  protected
    FObject: TObject;
    FObjectClass: TObjectClass;
    FTargetFields: TPointerList;
    FSetupProcList: TSetupProcObjList;
    FSetupRefProcList: TSetupRefProcList;
    FProperties: TPropValueList;
    FEvents: TEventValueList;
    function CreateObject: TBuild; virtual; abstract;
    procedure ConfigureObject(AObject: TBuild); virtual;
    procedure ApplyPropPath(Instance: TObject; AProp: TPropertyValue);
    procedure ApplyPendingProps(Instance: TObject);
    procedure ApplyPendingEvents(Instance: TObject);
    procedure SetupProc(AProcObj: {$IFDEF FPC}specialize{$ENDIF} TSetupProcObj<TBuild>); overload;
    procedure SetupProc(ARefProc: {$IFDEF FPC}specialize{$ENDIF} TSetupRefProc<TBuild>); overload;
  public
    constructor Create; overload;
    constructor Create(AClass: TObjectClass); overload;
    constructor Create(AClass: TObjectClass; out Reference); overload;
    destructor Destroy; override;
    procedure AssignReference(out Reference);
    procedure ResetReferences;
    function Assign(out Reference): TSelf; overload;
    function WithProp(const APropName: string; const AValue: TValue): TSelf; overload;
    function WithProp(const APropValue: TPropertyValue): TSelf; overload;
    function WithPropObj(const APropName: string; AObj: TObject): TSelf; overload;  // for lazarus
    function WithPropSet(const APropName: string; const AValue: Integer): TSelf;
    function WithEvent(const AEventName: string; const AMethod: TMethod): TSelf; overload;
    function WithEvent(const AEventName: string; const AInstance: TObject;
      const AMethod: Pointer): TSelf; overload;
    function Setup(AProc: TSetupProcObjBuild): TSelf; overload;
    function Setup(AProc: TSetupRefProcBuild): TSelf; overload;
    function Build: TBuild;
    property ObjectClass: TObjectClass read FObjectClass;
  end;

  { TComponentBuilder }

  TObjectBuilder = class;
  TObjectBuilder = class({$IFDEF FPC}specialize{$ENDIF} TObjectBuilderBase<TObject, TObjectBuilder>)
  protected
    function CreateObject: TObject; override;
  public
    constructor Create; overload;
    constructor Create(out Reference); overload;
    constructor Create(AClass: TObjectClass); overload;
    constructor Create(AClass: TObjectClass; out Reference); overload;
  end;

  { IComponentBuilder }

  {$IFDEF FPC}generic{$ENDIF}
  IComponentBuilder<TBuild> = interface({$IFDEF FPC}specialize{$ENDIF} IObjectBuilder<TBuild>)
    function GetName: string;
    function GetOwner: TComponent;
    function GetTag: NativeInt;
    procedure SetName(AValue: string);
    procedure SetOwner(AValue: TComponent);
    procedure SetTag(AValue: NativeInt);
    property Name: string read GetName write SetName;
    property Tag: NativeInt read GetTag write SetTag;
    property Owner: TComponent read GetOwner write SetOwner;
  end;

  { TComponentBuilderBase }

  {$IFDEF FPC}generic{$ENDIF}
  TComponentBuilderBase<TBuild: TComponent; TSelf: class> = class abstract (
    {$IFDEF FPC}specialize{$ENDIF} TObjectBuilderBase<TBuild, TSelf>,
    {$IFDEF FPC}specialize{$ENDIF} IComponentBuilder<TBuild>
  )
  private
  protected
    FName: string;
    FTag: NativeInt;
    FOwner: TComponent;
    procedure ConfigureObject(AObject: TBuild); override;
  public
    constructor Create(AClass: TComponentClass; const AName: string=''); overload;
    constructor Create(AClass: TComponentClass; const AName: string; out Reference); overload;
    constructor Create(AClass: TComponentClass; out Reference); overload;
    function GetName: string;
    function GetOwner: TComponent;
    function GetTag: NativeInt;
    procedure SetName(AValue: string);
    procedure SetOwner(AValue: TComponent);
    procedure SetTag(AValue: NativeInt);
    function WithOwner(AOwner: TComponent): TSelf;
    function WithName(AName: string): TSelf;
    function WithTag(ATag: NativeInt): TSelf;
    property Name: string read FName write FName;
    property Tag: NativeInt read FTag write FTag;
    property Owner: TComponent read FOwner write FOwner;
  end;

  { TComponentBuilder }

  TComponentBuilder = class;
  TComponentBuilder = class({$IFDEF FPC}specialize{$ENDIF} TComponentBuilderBase<TComponent, TComponentBuilder>)
  protected
    function CreateObject: TComponent; override;
  public
    constructor Create; overload;
    constructor Create(AClass: TComponentClass; const AName: string=''); overload;
    constructor Create(AClass: TComponentClass; const AName: string; out Reference); overload;
    constructor Create(AClass: TComponentClass; out Reference); overload;
  end;

  {$IFDEF FPC}generic{$ENDIF}
  IMenuBuilder<TBuild> = interface({$IFDEF FPC}specialize{$ENDIF} IComponentBuilder<TBuild>)
  end;

  { TMenuBuilderBase }

  {$IFDEF FPC}generic{$ENDIF}
  TMenuBuilderBase<TBuild: TMenu; TSelf: class> =
    class abstract (
       {$IFDEF FPC}specialize{$ENDIF}
       TComponentBuilderBase<TBuild, TSelf>,
       {$IFDEF FPC}specialize{$ENDIF} IMenuBuilder<TBuild>
     )
  end;

  { TMenuBuilder }

  TMenuBuilder = class;
  TMenuBuilder = class({$IFDEF FPC}specialize{$ENDIF} TMenuBuilderBase<TMenu, TMenuBuilder>)
  protected
    function CreateObject: TMenu; override;
  public
    constructor Create; overload;
    constructor Create(AClass: TMenuClass; const AName: string=''); overload;
    constructor Create(AClass: TMenuClass; const AName: string; out Reference); overload;
    constructor Create(AClass: TMenuClass; out Reference); overload;
  end;

  { IMenuItemBuilder }

  {$IFDEF FPC}generic{$ENDIF}
  IMenuItemBuilder<TBuild> = interface({$IFDEF FPC}specialize{$ENDIF} IObjectBuilder<TBuild>)
    function GetCaption: TOptionalString;
    function GetImageIndex: TOptionalInteger;
    function GetOnClick: TNotifyEvent;
    procedure SetCaption(AValue: TOptionalString);
    procedure SetImageIndex(AValue: TOptionalInteger);
    procedure SetOnClick(AValue: TNotifyEvent);
    property Caption: TOptionalString read GetCaption write SetCaption;
    property ImageIndex: TOptionalInteger read GetImageIndex write SetImageIndex;
    property OnClick: TNotifyEvent read GetOnClick write SetOnClick;
  end;

  { TMenuItemBuilderBase }

  {$IFDEF FPC}generic{$ENDIF}
  TMenuItemBuilderBase<TBuild: TMenuItem; TSelf: class> = class abstract (
    {$IFDEF FPC}specialize{$ENDIF}
    TComponentBuilderBase<TBuild, TSelf>,
    {$IFDEF FPC}specialize{$ENDIF} IMenuItemBuilder<TBuild>
  )
  protected
    FOnClick: TNotifyEvent;
    FCaption: TOptionalString;
    FImageIndex: TOptionalInteger;
    procedure ConfigureObject(AObject: TBuild); override;
  public
    constructor Create; overload;
    function WithCaption(ACaption: string): TSelf;
    function WithImageIndex(AImageIndex: Integer): TSelf;
    function WithOnClick(AOnClick: TNotifyEvent): TSelf;
    function GetCaption: TOptionalString;
    function GetImageIndex: TOptionalInteger;
    function GetOnClick: TNotifyEvent;
    procedure SetCaption(AValue: TOptionalString);
    procedure SetImageIndex(AValue: TOptionalInteger);
    procedure SetOnClick(AValue: TNotifyEvent);
    property Caption: TOptionalString read GetCaption write SetCaption;
    property ImageIndex: TOptionalInteger read GetImageIndex write SetImageIndex;
    property OnClick: TNotifyEvent read GetOnClick write SetOnClick;
  end;

  { TMenuItemBuilder }

  TMenuItemBuilder = class;
  TMenuItemBuilder = class({$IFDEF FPC}specialize{$ENDIF} TMenuItemBuilderBase<TMenuItem, TMenuItemBuilder>)
  protected
    function CreateObject: TMenuItem; override;
  public
    constructor Create; overload;
    constructor Create(AClass: TMenuItemClass; const AName: string=''); overload;
    constructor Create(AClass: TMenuItemClass; const AName: string; out Reference); overload;
    constructor Create(AClass: TMenuItemClass; out Reference); overload;
  end;

  { IControlBuilder }

  {$IFDEF FPC}generic{$ENDIF}
  IControlBuilder<TBuild> = interface({$IFDEF FPC}specialize{$ENDIF} IComponentBuilder<TBuild>)
    ['{96747920-C27C-4846-BE48-76D1763D45B3}']
    function GetAlign: TOptionalAlign;
    function GetCaption: TOptionalString;
    function GetHeight: TOptionalSingle;
    function GetLeft: TOptionalSingle;
    function GetOnClick: TNotifyEvent;
    function GetParent: TWinControl;
    function GetText: TOptionalString;
    function GetTop: TOptionalSingle;
    function GetWidth: TOptionalSingle;
    procedure SetAlign(AValue: TOptionalAlign);
    procedure SetCaption(AValue: TOptionalString);
    procedure SetHeight(AValue: TOptionalSingle);
    procedure SetLeft(AValue: TOptionalSingle);
    procedure SetOnClick(AValue: TNotifyEvent);
    procedure SetParent(AValue: TWinControl);
    procedure SetText(AValue: TOptionalString);
    procedure SetTop(AValue: TOptionalSingle);
    procedure SetWidth(AValue: TOptionalSingle);
    property Caption: TOptionalString read GetCaption write SetCaption;
    property Text: TOptionalString read GetText write SetText;
    property Align: TOptionalAlign read GetAlign write SetAlign;
    property Width: TOptionalSingle read GetWidth write SetWidth;
    property Height: TOptionalSingle read GetHeight write SetHeight;
    property Top: TOptionalSingle read GetTop write SetTop;
    property Left: TOptionalSingle read GetLeft write SetLeft;
    property OnClick: TNotifyEvent read GetOnClick write SetOnClick;
    property Parent: TWinControl read GetParent write SetParent;
  end;

  { TControlBuilderBase }

  {$IFDEF FPC}generic{$ENDIF}
  TControlBuilderBase<TBuild: TControl; TSelf: class> =
    class abstract (
      {$IFDEF FPC}specialize{$ENDIF}
      TComponentBuilderBase<TBuild, TSelf>,
      {$IFDEF FPC}specialize{$ENDIF} IControlBuilder<TBuild>
    )
  protected
    FParent: TWinControl;
    FCaption: TOptionalString;
    FText: TOptionalString;
    FAlign: TOptionalAlign;
    FWidth: TOptionalSingle;
    FHeight: TOptionalSingle;
    FTop: TOptionalSingle;
    FLeft: TOptionalSingle;
    FOnClick: TNotifyEvent;
    procedure ConfigureObject(AObject: TBuild); override;
  public
    constructor Create; overload;
    function GetParent: TWinControl;
    procedure SetParent(AValue: TWinControl);
    function GetAlign: TOptionalAlign;
    function GetCaption: TOptionalString;
    function GetHeight: TOptionalSingle;
    function GetLeft: TOptionalSingle;
    function GetOnClick: TNotifyEvent;
    function GetText: TOptionalString;
    function GetTop: TOptionalSingle;
    function GetWidth: TOptionalSingle;
    procedure SetAlign(AValue: TOptionalAlign);
    procedure SetCaption(AValue: TOptionalString);
    procedure SetHeight(AValue: TOptionalSingle);
    procedure SetLeft(AValue: TOptionalSingle);
    procedure SetOnClick(AValue: TNotifyEvent);
    procedure SetText(AValue: TOptionalString);
    procedure SetTop(AValue: TOptionalSingle);
    procedure SetWidth(AValue: TOptionalSingle);
    function WithAlign(AAlign: {$IFDEF FRAMEWORK_FMX}TAlignLayout{$ELSE}TAlign{$ENDIF}): TSelf;
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
    function WithParent(AParent: TWinControl): TSelf;
    function WithOwnerAndParent(AOwner: TComponent; AParent: TWinControl): TSelf;
    property Parent: TWinControl read GetParent write SetParent;
    property Caption: TOptionalString read FCaption write FCaption;
    property Text: TOptionalString read FText write FText;
    property Align: TOptionalAlign read FAlign write FAlign;
    property Width: TOptionalSingle read FWidth write FWidth;
    property Height: TOptionalSingle read FHeight write FHeight;
    property Top: TOptionalSingle read FTop write FTop;
    property Left: TOptionalSingle read FLeft write FLeft;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

  TControlBuilder = class;
  TControlBuilder = class({$IFDEF FPC}specialize{$ENDIF} TControlBuilderBase<TControl, TControlBuilder>)
  protected
    function CreateObject: TControl; override;
  public
    constructor Create; overload;
    constructor Create(AClass: TControlClass; const AName: string=''); overload;
    constructor Create(AClass: TControlClass; const AName: string; out Reference); overload;
    constructor Create(AClass: TControlClass; out Reference); overload;
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
  protected
    class var FInstances: TStrComponentRegistryEntryDictionary;
    class function GetContextComponents(const AContext, AName: string): TComponent; static;
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
    procedure Add(AControl: TControl; const AName: string = '');
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

  TControlCreatorDirection = (cpdHorizontal, cpdVertical);
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

  TIntSingleDictionary = {$IFDEF FPC}specialize{$ENDIF} TDictionary<Integer, Single>;

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

  { TControlCreatorLevel }

  TControlCreatorLevel = class
  private
    class var FGroupCounter: Integer;
  public
    Parent: TWinControl;
    GroupName: string;
    Direction: TControlCreatorDirection;
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
    function Clone: TControlCreatorLevel;
  end;

  TControlCreatorLevelStack = {$IFDEF FPC}specialize{$ENDIF} TObjectList<TControlCreatorLevel>;

  TMenuCreatorLevel = class
  public
    {$IFDEF FRAMEWORK_FMX}
    Parent: TFmxObject;
    {$ELSE}
    Parent: TMenuItem;
    {$ENDIF}
  end;

  TMenuCreatorLevelStack = {$IFDEF FPC}specialize{$ENDIF} TObjectList<TMenuCreatorLevel>;
  TComponentCreator = class;
  TComponentCreatorObjProc = procedure(const ABuiler: TComponentCreator) of object;
  TComponentCreatorProc = procedure(const ABuiler: TComponentCreator);

  TComponentCreator = class
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
    function External(const AProc: TComponentCreatorObjProc): TComponentCreator; overload;
    function External(const AProc: TComponentCreatorProc): TComponentCreator; overload;
    {$IFDEF FPC}generic{$ENDIF} function GetComponent<T: TComponent>(const AName: string): T; overload;
    function GetComponent(const AName: string): TComponent; overload;
    function WithOwner(AOwner: TComponent): TComponentCreator; deprecated 'Use SetOwner instead';
    function SetOwner(AOwner: TComponent): TComponentCreator;
    function Add(AComponentBuilder: {$IFDEF FPC}specialize{$ENDIF} IComponentBuilder<TComponent>): TComponentCreator; overload;
    property Registry: TComponentRegistry read GetComponentRegistry;
    property Items[const AName: string]: TComponent read GetItem; default;
  end;

  TMenuCreator = class;
  TMenuCreatorObjProc = procedure(const ABuiler: TMenuCreator) of object;
  TMenuCreatorProc = procedure(const ABuiler: TMenuCreator);

  TMenuCreator = class
  private
    FOwner: TComponent;
    FRegistryContextHandle: IRegistryContextHandle;
    FLevelStack: TMenuCreatorLevelStack;
    function GetComponentRegistry: TComponentRegistry;
    function GetCurrenteLevel: TMenuCreatorLevel;
  public
    constructor Create(ARegistryContextKey: string=''); overload;
    constructor Create(ARegistryContextHandle: IRegistryContextHandle); overload;
    destructor Destroy; override;
    function WithOwner(AOwner: TComponent): TMenuCreator; deprecated 'Use SetOwner instead';
    function SetOwner(AOwner: TComponent): TMenuCreator;
    function External(const AProc: TMenuCreatorObjProc): TMenuCreator; overload;
    function External(const AProc: TMenuCreatorProc): TMenuCreator; overload;
    function AddMenu(AMenuBuilder: TMenuBuilder): TMenuCreator;
    function AddMenuItem(AMenuItemBuilder: TMenuItemBuilder): TMenuCreator;
    function SubLevel(AMenuItemBuilder: TMenuItemBuilder): TMenuCreator;
    function SuperLevel: TMenuCreator;
    {$IFDEF FPC}generic{$ENDIF}
    function GetMenu<T: TMenu>(const AName: string): T; overload;
    function GetMenu(const AName: string): TMenu; overload;
    {$IFDEF FPC}generic{$ENDIF}
    function GetMenuItem<T: TMenuItem>(const AName: string): T; overload;
    function GetMenuItem(const AName: string): TMenuItem; overload;
    property CurrentLevel: TMenuCreatorLevel read GetCurrenteLevel;
    property Registry: TComponentRegistry read GetComponentRegistry;
  end;

  TControlCreator = class;
  TControlCreatorObjProc = procedure(const ABuiler: TControlCreator) of object;
  TControlCreatorProc = {$IFNDEF FPC}reference to{$ENDIF} procedure(ABuiler: TControlCreator);

  TControlCreator = class
  private
    FOwner: TComponent;
    FRegistryContextHandle: IRegistryContextHandle;
    FGroups: TControlGroupMap;
    FLevelStack: TControlCreatorLevelStack;
    FOwnControls: TControlList;
    function GetControls: TControlList;
    procedure MoveTopLeftAfterControl(AControl: TControl);
    procedure MoveTopLeftAfterRect(
      const ARect: {$IFDEF FRAMEWORK_FMX}TRectF{$ELSE}TRect{$ENDIF};
      AAlign: {$IFDEF FRAMEWORK_FMX}TAlignLayout{$ELSE}TAlign{$ENDIF});
    procedure MoveTopLeftAfterBound(ABounds: TControlGroupBounds);
    procedure AddToGroups(AControl: TControl; const AGroups: array of string);
    function GetGroupBounds(const AGroupName: string): TControlGroupBounds;
    function GetCurrenteLevel: TControlCreatorLevel;
    function GetContentWidth: Single;
    function GetFContentHeight: Single;
    function GetComponentRegistry: TComponentRegistry;
    function GetItem(const AName: string): TControl;
    procedure ApplyDefaultControlSize(AControl: TControl);
    function CanAddToGrid: Boolean;
    procedure AdjustControlToCell(AControl: TControl; ACellRect: TRect);
  public
    constructor Create(ARegistryContextKey: string=''); overload;
    constructor Create(ARegistryContextHandle: IRegistryContextHandle); overload;
    destructor Destroy; override;
    {$IFDEF FPC}generic{$ENDIF} function GetControl<T: TControl>(const AName: string): T; overload;
    function GetControl(const AName: string): TControl; overload;
    function GetControlsBounds(AControlsNames: array of string): TControlGroupBounds;
    function GetNamedControl(const AName: string): TControl;
    property NamedControls[const AName: string]: TControl read GetNamedControl;
    property ContentWidth: Single read GetContentWidth;
    property ContentHeight: Single read GetFContentHeight;
    property CurrentLevel: TControlCreatorLevel read GetCurrenteLevel;
    property Controls: TControlList read GetControls;
    property Registry: TComponentRegistry read GetComponentRegistry;
    property Items[const AName: string]: TControl read GetItem; default;
  end;

  TControlCreatorHelper = class helper for TControlCreator
  public
    {$IFDEF FPC}generic{$ENDIF}
    procedure SetupControlBuilderForGridMode<TBuild>(AControlBuilder: {$IFDEF FPC}specialize{$ENDIF} IControlBuilder<TBuild>);
    {$IFDEF FPC}generic{$ENDIF}
    function CreateControl<TBuild>(ABuilder: {$IFDEF FPC}specialize{$ENDIF} IControlBuilder<TBuild>; AOwner: TComponent = nil): TBuild;
    function External(const AProc: TControlCreatorObjProc): TControlCreator; overload;
    function External(const AProc: TControlCreatorProc): TControlCreator; overload;
    function SetSpace(AVerticalSpace, AHorizontalSpace: Single): TControlCreator;
    function SubLevel(AGroupName: string=''): TControlCreator; overload;  // xx
    function SubLevel(ADirection: TControlCreatorDirection;
      AGroupName: string=''): TControlCreator; overload;
    function SuperLevel: TControlCreator;
    function SiblingSubLevel(AGroupName: string='';
      ABreak: Boolean=False): TControlCreator; overload;
    function SiblingSubLevel(ADirection: TControlCreatorDirection;
      AGroupName: string=''; ABreak: Boolean=False): TControlCreator; overload;
    function SiblingSubLevel(ABreak: Boolean=False): TControlCreator; overload;
    function SiblingSubLevel(ADirection: TControlCreatorDirection;
      ABreak: Boolean): TControlCreator; overload;
    function SiblingSubLevelWithBreak(AGroupName: string=''): TControlCreator; overload;
    function SiblingSubLevelWithBreak(ADirection: TControlCreatorDirection;
      AGroupName: string=''): TControlCreator; overload;
    // main
    {$IFDEF FPC}generic{$ENDIF}
    function SubLevel<TBuild: class>(AControlBuilder: {$IFDEF FPC}specialize{$ENDIF} IControlBuilder<TBuild>; AGroupName: string=''): TControlCreator; overload;
    function SubLevel(AControlBuilder: TControlBuilder; AGroupName: string=''): TControlCreator; overload;
    {$IFDEF FPC}generic{$ENDIF}
    function SubLevel<TBuild: class>(AControlBuilder: {$IFDEF FPC}specialize{$ENDIF} IControlBuilder<TBuild>; ADirection: TControlCreatorDirection; AGroupName: string=''): TControlCreator; overload;
    function SubLevel(AControlBuilder: TControlBuilder; ADirection: TControlCreatorDirection; AGroupName: string=''): TControlCreator; overload;
    {$IFDEF FPC}generic{$ENDIF}
    function SiblingSubLevel<TBuild: class>(AControlBuilder: {$IFDEF FPC}specialize{$ENDIF} IControlBuilder<TBuild>; AGroupName: string=''; ABreak: Boolean=False): TControlCreator; overload;
    function SiblingSubLevel(AControlBuilder: TControlBuilder; AGroupName: string=''; ABreak: Boolean=False): TControlCreator; overload;
    {$IFDEF FPC}generic{$ENDIF}
    function SiblingSubLevel<TBuild: class>(AControlBuilder: {$IFDEF FPC}specialize{$ENDIF} IControlBuilder<TBuild>; ADirection: TControlCreatorDirection; AGroupName: string=''; ABreak: Boolean=False): TControlCreator; overload;
    function SiblingSubLevel(AControlBuilder: TControlBuilder; ADirection: TControlCreatorDirection; AGroupName: string=''; ABreak: Boolean=False): TControlCreator; overload;
    {$IFDEF FPC}generic{$ENDIF}
    function SiblingSubLevel<TBuild: class>(AControlBuilder: {$IFDEF FPC}specialize{$ENDIF} IControlBuilder<TBuild>; ABreak: Boolean=False): TControlCreator; overload;
    function SiblingSubLevel(AControlBuilder: TControlBuilder; ABreak: Boolean=False): TControlCreator; overload;
    {$IFDEF FPC}generic{$ENDIF}
    function SiblingSubLevel<TBuild: class>(AControlBuilder: {$IFDEF FPC}specialize{$ENDIF} IControlBuilder<TBuild>; ADirection: TControlCreatorDirection; ABreak: Boolean): TControlCreator; overload;
    function SiblingSubLevel(AControlBuilder: TControlBuilder; ADirection: TControlCreatorDirection; ABreak: Boolean): TControlCreator; overload;
    {$IFDEF FPC}generic{$ENDIF}
    function SiblingSubLevelWithBreak<TBuild: class>(AControlBuilder: {$IFDEF FPC}specialize{$ENDIF} IControlBuilder<TBuild>; AGroupName: string=''): TControlCreator; overload;
    function SiblingSubLevelWithBreak(AControlBuilder: TControlBuilder; AGroupName: string=''): TControlCreator; overload;
    {$IFDEF FPC}generic{$ENDIF}
    function SiblingSubLevelWithBreak<TBuild: class>(AControlBuilder: {$IFDEF FPC}specialize{$ENDIF} IControlBuilder<TBuild>; ADirection: TControlCreatorDirection; AGroupName: string=''): TControlCreator; overload;
    function SiblingSubLevelWithBreak(AControlBuilder: TControlBuilder; ADirection: TControlCreatorDirection; AGroupName: string=''): TControlCreator; overload;
    function SetVerticalSpace(AVerticalSpace: Single): TControlCreator;
    function SetHorizontalSpace(AHorizontalSpace: Single): TControlCreator;
    function SetTopLeft(ATop, ALeft: Single): TControlCreator;
    function SetTopLeftNearControl(AControlName: string; APosition: TRelativePosition): TControlCreator;
    function SetTopLeftNearControls(AControlsNames: array of string; APosition: TRelativePosition): TControlCreator;
    function SetTopLeftNearGroup(const AGroupName: string; APosition: TRelativePosition): TControlCreator;
    function SetTop(ATop: Single): TControlCreator; overload;
    function SetLeft(ALeft: Single): TControlCreator; overload;
    function SetTop(AControlName: string): TControlCreator; overload;
    function SetLeft(AControlName: string): TControlCreator; overload;
    function IncTop(AIncTop: Single): TControlCreator;
    function IncLeft(AIncLeft: Single): TControlCreator;
    function IncTopLeft(AIncTop, AIncLeft: Single): TControlCreator;
    function SetDirection(ADirection: TControlCreatorDirection): TControlCreator;
    function SetControlHeight(AHeight: Single): TControlCreator;
    function SetControlWidth(AWidth: Single): TControlCreator;
    function SetControlWidthAndHeight(AWidth, AHeight: Single): TControlCreator;
    function UnsetControlHeight: TControlCreator;
    function UnsetControlWidth: TControlCreator;
    function UnsetControlWidthAndHeight: TControlCreator;
    function GridInit(ARows, ACols: Integer): TControlCreator;
    function GridFinish: TControlCreator;
    function GridCellSpan(ACellSpan: Integer): TControlCreator;
    function GridRowSpan(ARowSpan: Integer): TControlCreator;
    function GridColSpan(AColSpan: Integer): TControlCreator;
    function GridSetCellWidthAndHeight(AWidth, AHeight: Integer): TControlCreator;
    function GridSetColWidth(ACol: Integer; AWidth: Single): TControlCreator;
    function GridSetRowHeight(ARow: Integer; AHeight: Single): TControlCreator;
    function GridSetColOffset(ACol: Integer; AOffset: Single): TControlCreator;
    function GridSetRowOffset(ARow: Integer; AOffset: Single): TControlCreator;
    function GridSkipCell: TControlCreator;
    function GridSkipCells(ANumCells: Integer): TControlCreator;
    function GridGotoCell(ARow, ACol: Integer): TControlCreator;
    function GridAutoExpand: TControlCreator;
    function GridAutoExpandRows: TControlCreator;
    function GridAutoExpandCols: TControlCreator;
    function GridCellPosition(ACellPosition: TCellPosition): TControlCreator;
    function GridCellNoStrech: TControlCreator;
    function GridCellStrechAll: TControlCreator;
    function GridCellStrechHorizontal: TControlCreator;
    function GridCellStrechVertical: TControlCreator;
    function GridReturnNumberOfRows(out ARows: Integer): TControlCreator;
    function GridReturnNumberOfCols(out ACols: Integer): TControlCreator;
    function BreakLine: TControlCreator; overload;
    function BreakColumn: TControlCreator; overload;
    function Break: TControlCreator; overload;
    function Break(AIncTopOrLeft: Single): TControlCreator; overload;
    function BreakLine(AIncTop: Single): TControlCreator; overload;
    function BreakColumn(AIncLeft: Single): TControlCreator; overload;
    {$IFDEF FRAMEWORK_FMX}
    function WithOwnerAndParent(AOwner: TComponent; AParent: TFmxObject): TControlCreator; deprecated 'Use SetOwnerAndParent instead';
    function SetOwnerAndParent(AOwner: TComponent; AParent: TFmxObject): TControlCreator;
    {$ELSE}
    function WithOwnerAndParent(AOwner: TComponent; AParent: TWinControl): TControlCreator; deprecated 'Use SetOwnerAndParent instead';
    function SetOwnerAndParent(AOwner: TComponent; AParent: TWinControl): TControlCreator;
    {$ENDIF}
    function WithParent(AParent: TWinControl): TControlCreator; deprecated 'Use SetParent instead';
    function SetParent(AParent: TWinControl): TControlCreator;
    // main
    {$IFDEF FPC}generic{$ENDIF}
    function Add<TBuild: class>(AControlBuilder: {$IFDEF FPC}specialize{$ENDIF} IControlBuilder<TBuild>; const AGroups: array of string): TControlCreator; overload;
    {$IFDEF FPC}generic{$ENDIF}
    function Add<TBuild: class>(AControlBuilder: {$IFDEF FPC}specialize{$ENDIF} IControlBuilder<TBuild>): TControlCreator; overload;
    function Add(AControlBuilder: TControlBuilder): TControlCreator; overload;
    function Add(AClass: TControlClass; const AName: string=''): TControlCreator; overload;
    function Add(AClass: TControlClass; const AName: string; out Reference): TControlCreator; overload;
    function Add(AClass: TControlClass; out Reference): TControlCreator; overload;
    function Add(AClass: TControlClass; const AName: string; AProc: TControlSetupProcObj): TControlCreator; overload;
    function Add(AClass: TControlClass; const AName: string; out Reference; AProc: TControlSetupProcObj): TControlCreator; overload;
    function Add(AClass: TControlClass; out Reference; AProc: TControlSetupProcObj): TControlCreator; overload;
    function MoveControls(const AControl: TControl; const ADX,
      ADY: Single): TControlCreator; overload;
    function MoveControls(const AControlNames: array of string;
      const ADX, ADY: Single): TControlCreator; overload;
    function AlignControlsRight(const AControlNames, AReferenceGroup: array of string;
      const ARightPadding: Single = 0): TControlCreator;
    function CenterControlsHorizontally(const AControlNames, AReferenceGroup:
      array of string): TControlCreator;
    function CenterControlsVertically(const AControlNames, AReferenceGroup:
      array of string): TControlCreator;
    function CenterControlsInParentVertically(
      const AControlNames: array of string): TControlCreator;
    function CenterControlsInParentHorizontally(
      const AControlNames: array of string): TControlCreator;
    function CenterControlInParentHorizontally: TControlCreator;
    function RecalcParentHeight(AExtraHeight: Single = 0): TControlCreator;
    function RecalcParentWidth(AExtraWidth: Single = 0): TControlCreator;
    function RecalcParentSize(AExtraHeight: Single = 0; AExtraWidth: Single = 0): TControlCreator;
    function CopyHeight(const AControlNames,
      AReferenceGroup: array of string): TControlCreator;
    function CopyWidth(const AControlNames,
      AReferenceGroup: array of string): TControlCreator;
    function CopySize(const AControlNames,
      AReferenceGroup: array of string): TControlCreator;
    function ReturnCurrentLevel(var ACurrentLevel: TControlCreatorLevel): TControlCreator;
    function ReturnLastControl(out AControl: TControl): TControlCreator;
  end;

  TOPCBCreators = class
  private
    FRegistryContextHandle: IRegistryContextHandle;
    FComponentCreator: TComponentCreator;
    FControlCreator: TControlCreator;
    FMenuCreator: TMenuCreator;
  public
    constructor Create(const ARegistryContextKey: string='');
    destructor Destroy; override;
    function AsComponentCreator: TComponentCreator;
    function AsControlCreator: TControlCreator;
    function AsMenuCreator: TMenuCreator;
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

{ TControlBuilder }

function TControlBuilderBase{$IFNDEF FPC}<TBuild; TSelf>{$ENDIF}.WithAlign(
  AAlign: {$IFDEF FRAMEWORK_FMX}TAlignLayout{$ELSE}TAlign{$ENDIF}): TSelf;
begin
  Result := TSelf(Self);
  FAlign := AAlign;
end;

function TControlBuilderBase{$IFNDEF FPC}<TBuild; TSelf>{$ENDIF}.WithCaption(
  ACaption: string): TSelf;
begin
  Result := TSelf(Self);
  FCaption := ACaption;
end;

function TControlBuilderBase{$IFNDEF FPC}<TBuild; TSelf>{$ENDIF}.WithHeight(
  AHeight: Single): TSelf;
begin
  Result := TSelf(Self);
  FHeight := AHeight;
end;

function TControlBuilderBase{$IFNDEF FPC}<TBuild; TSelf>{$ENDIF}.WithLeft(
  ALeft: Single): TSelf;
begin
  Result := TSelf(Self);
  FLeft := ALeft;
end;

function TControlBuilderBase{$IFNDEF FPC}<TBuild; TSelf>{$ENDIF}.WithName(
  AName: string): TSelf;
begin
  Result := TSelf(Self);
  FName := AName;
end;

function TControlBuilderBase{$IFNDEF FPC}<TBuild; TSelf>{$ENDIF}.WithOnClick(
  AOnClick: TNotifyEvent): TSelf;
begin
  Result := TSelf(Self);
  FOnClick := AOnClick;
end;

function TControlBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.WithOwnerAndParent(
  AOwner: TComponent; AParent: TWinControl): TSelf;
begin
  Result := TSelf(Self);
  WithOwner(AOwner);
  WithParent(AParent);
end;

function TControlBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.WithParent(
  AParent: TWinControl): TSelf;
begin
  Result := TSelf(Self);
  FParent := AParent;
end;

function TControlBuilderBase{$IFNDEF FPC}<TBuild; TSelf>{$ENDIF}.WithTag(
  ATag: NativeInt): TSelf;
begin
  Result := TSelf(Self);
  FTag := ATag;
end;

function TControlBuilderBase{$IFNDEF FPC}<TBuild; TSelf>{$ENDIF}.WithText(
  AText: string): TSelf;
begin
  Result := TSelf(Self);
  FText := AText;
end;

function TControlBuilderBase{$IFNDEF FPC}<TBuild; TSelf>{$ENDIF}.WithTop(
  ATop: Single): TSelf;
begin
  Result := TSelf(Self);
  FTop := ATop;
end;

function TControlBuilderBase{$IFNDEF FPC}<TBuild; TSelf>{$ENDIF}.WithWidth(
  AWidth: Single): TSelf;
begin
  Result := TSelf(Self);
  FWidth := AWidth;
end;

function TControlBuilderBase{$IFNDEF FPC}<TBuild; TSelf>{$ENDIF}.WithWidthAndHeight(
  AWidth: Single; AHeight: Single): TSelf;
begin
  Result := TSelf(Self);
  FWidth := AWidth;
  FHeight := AHeight;
end;

{ TControlCreator }

function TControlCreatorHelper.BreakLine: TControlCreator;
begin
  Result := Self;
  if CurrentLevel.GridMode.Active then
  begin
    Self.GridGotoCell(CurrentLevel.GridMode.CurrentRow + 1, 0);
  end
  else
  begin
    CurrentLevel.CurrentTop :=
      CurrentLevel.CurrentTop + CurrentLevel.MaxControlHeight;
    CurrentLevel.CurrentLeft := CurrentLevel.InitialLeft;
    CurrentLevel.MaxControlHeight := 0;
  end;
end;

function TControlCreatorHelper.Break: TControlCreator;
begin
  Result := Self;
  if CurrentLevel.Direction = cpdHorizontal then
    BreakLine;
  if CurrentLevel.Direction = cpdVertical then
    BreakColumn;
end;

function TControlCreatorHelper.Break(AIncTopOrLeft: Single): TControlCreator;
begin
  Result := Self;
  Self.Break;
  if CurrentLevel.Direction = cpdHorizontal then
    IncTop(AIncTopOrLeft);
  if CurrentLevel.Direction = cpdVertical then
    IncLeft(AIncTopOrLeft);
end;

function TControlCreatorHelper.BreakColumn(AIncLeft: Single): TControlCreator;
begin
  Result := Self;
  Self.BreakColumn;
  IncLeft(AIncLeft);
end;

function TControlCreatorHelper.BreakColumn: TControlCreator;
begin
  Result := Self;
  if CurrentLevel.GridMode.Active then
  begin
    Self.GridGotoCell(0, CurrentLevel.GridMode.CurrentCol + 1);
  end
  else
  begin
    CurrentLevel.CurrentLeft :=
      CurrentLevel.CurrentLeft + CurrentLevel.MaxControlWidth;
    CurrentLevel.CurrentTop := CurrentLevel.InitialTop;
    CurrentLevel.MaxControlWidth := 0;
  end;
end;

function TControlCreatorHelper.CenterControlsVertically(const AControlNames,
  AReferenceGroup: array of string): TControlCreator;
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

function TControlCreatorHelper.CenterControlsInParentVertically(
  const AControlNames: array of string): TControlCreator;
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

function TControlCreatorHelper.CenterControlsInParentHorizontally(
  const AControlNames: array of string): TControlCreator;
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

function TControlCreatorHelper.CenterControlInParentHorizontally: TControlCreator;
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

constructor TControlCreator.Create(ARegistryContextKey: string);
begin
  Create(TRegistryContextHandle.Create(ARegistryContextKey));
end;

constructor TControlCreator.Create(ARegistryContextHandle: IRegistryContextHandle);
begin
  FRegistryContextHandle := ARegistryContextHandle;
  FGroups := TControlGroupMap.Create;
  FLevelStack := TControlCreatorLevelStack.Create(True);
  FLevelStack.Add(TControlCreatorLevel.Create);
  FOwnControls := TControlList.Create;
end;

{$IFDEF FPC}generic{$ENDIF}
procedure TControlCreatorHelper.SetupControlBuilderForGridMode<TBuild>(AControlBuilder: {$IFDEF FPC}specialize{$ENDIF} IControlBuilder<TBuild>);
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
      IfThen(AControlBuilder.Width.HasValue, AControlBuilder.Width.Value, 0),
      IfThen(AControlBuilder.Height.HasValue, AControlBuilder.Height.Value,0)
    );

    AControlBuilder.Align := {$IFDEF FRAMEWORK_FMX}TAlignLayout.None{$ELSE}alNone{$ENDIF};
    AControlBuilder.Left := RControl.Left;
    AControlBuilder.Top  := RControl.Top;
    AControlBuilder.Height := RControl.Bottom - RControl.Top;
    AControlBuilder.Width := RControl.Right - RControl.Left;
  end;

  CurrentLevel.GridMode.ResetSpans;
end;

{$IFDEF FPC}generic{$ENDIF}
function TControlCreatorHelper.CreateControl<TBuild>(
  ABuilder: {$IFDEF FPC}specialize{$ENDIF} IControlBuilder<TBuild>;
  AOwner: TComponent = nil): TBuild;
var
  ControlName: string;
begin
  ControlName := '';
  if not ABuilder.Name.IsEmpty then
    ControlName := Registry.UniqueName(ABuilder.Name);

  if not ABuilder.Top.HasValue then
    ABuilder.Top := CurrentLevel.CurrentTop;

  if not ABuilder.Left.HasValue then
    ABuilder.Left := CurrentLevel.CurrentLeft;

  if not Assigned(AOwner) then
    AOwner := FOwner;

  ABuilder.Owner := AOwner;
  ABuilder.Parent := CurrentLevel.Parent;
  ABuilder.Name := ControlName;

  Result := ABuilder.Build;
end;

{$IFDEF FPC}generic{$ENDIF}
function TControlCreatorHelper.Add<TBuild>(
  AControlBuilder: {$IFDEF FPC}specialize{$ENDIF} IControlBuilder<TBuild>;
  const AGroups: array of string): TControlCreator;
var
  Control: TBuild;
  Level: TControlCreatorLevel;
begin
  Result := Self;

  if CurrentLevel.GridMode.Active then
  begin
    if not CanAddToGrid then
      Exit;

    {$IFDEF FPC}specialize{$ENDIF} SetupControlBuilderForGridMode<TBuild>(AControlBuilder);
    Control := {$IFDEF FPC}specialize{$ENDIF} CreateControl<TBuild>(AControlBuilder);
  end
  else
  begin
    Control := {$IFDEF FPC}specialize{$ENDIF} CreateControl<TBuild>(AControlBuilder);
  end;

  ApplyDefaultControlSize(Control as TControl);

  Registry.AddComponent(Control as TControl, (Control as TControl).Name);
  FOwnControls.Add(Control as TControl);

  // caso especial: TTabSheet / TPageControl
  {$IFDEF FRAMEWORK_FMX}
  if (Control is TTabItem) and (CurrentLevel.Parent is TTabControl) then
  begin
    TTabItem(Control).Parent := TTabControl(CurrentLevel.Parent);
  end;
  {$ELSE}
  if (TControl(Control) is TTabSheet) and (CurrentLevel.Parent is TPageControl) then
  begin
    {$IFNDEF FPC}TTabSheet(Control).Parent := nil;{$ENDIF}
    TTabSheet(Control).PageControl := TPageControl(CurrentLevel.Parent);
  end;
  {$ENDIF}

  for Level in FLevelStack do
    if not Level.GroupName.IsEmpty then
      AddToGroups(Control as TControl, [Level.GroupName]);

  AddToGroups(Control as TControl, AGroups);

  MoveTopLeftAfterControl(Control as TControl);
end;

procedure TControlCreator.ApplyDefaultControlSize(AControl: TControl);
begin
  {$IFDEF FRAMEWORK_FMX}
  if CurrentLevel.ControlHeight.HasValue then
    AControl.Height := CurrentLevel.ControlHeight.Value;
  if CurrentLevel.ControlWidth.HasValue then
    AControl.Width := CurrentLevel.ControlWidth.Value;
  {$ELSE}
  if CurrentLevel.ControlHeight.HasValue then
    AControl.Height := Trunc(CurrentLevel.ControlHeight.Value);
  if CurrentLevel.ControlWidth.HasValue then
    AControl.Width := Trunc(CurrentLevel.ControlWidth.Value);
  {$ENDIF}
end;

function TControlCreator.CanAddToGrid: Boolean;
var
  R, C: Integer;
begin
  if CurrentLevel.GridMode.IsCellFree(
    CurrentLevel.GridMode.CurrentRow,
    CurrentLevel.GridMode.CurrentCol
  ) then
  begin
    Result := True;
    Exit;
  end;

  if ((CurrentLevel.GridMode.Direction = gfdRowFirst) and (gaeRows in CurrentLevel.GridMode.AutoExpand))
     or
     ((CurrentLevel.GridMode.Direction = gfdColFirst) and (gaeCols in CurrentLevel.GridMode.AutoExpand)) then
  begin
    Result := True;
    Exit;
  end;

  Result := CurrentLevel.GridMode.PeekNext(R, C);
end;

procedure TControlCreator.AdjustControlToCell(AControl: TControl; ACellRect: TRect);
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

function TControlCreatorHelper.Add(AControlBuilder: TControlBuilder): TControlCreator;
begin
  Result := {$IFDEF FPC}specialize{$ENDIF} Add<TControl>(AControlBuilder);
end;

function TControlCreatorHelper.Add(AClass: TControlClass;
  const AName: string=''): TControlCreator;
begin
  Result := {$IFDEF FPC}specialize{$ENDIF} Add<TControl>(TControlBuilder.Create(AClass, AName));
end;

function TControlCreatorHelper.Add(AClass: TControlClass; const AName: string;
  out Reference): TControlCreator;
begin
  Result := {$IFDEF FPC}specialize{$ENDIF} Add<TControl>(TControlBuilder.Create(AClass, AName, Reference));
end;

function TControlCreatorHelper.Add(AClass: TControlClass;
  out Reference): TControlCreator;
begin
  Result := {$IFDEF FPC}specialize{$ENDIF} Add<TControl>(TControlBuilder.Create(AClass, Reference));
end;

function TControlCreatorHelper.Add(AClass: TControlClass; const AName: string;
  AProc: TControlSetupProcObj): TControlCreator;
begin
  Result := {$IFDEF FPC}specialize{$ENDIF} Add<TControl>(TControlBuilder.Create(AClass, AName).Setup(AProc));
end;

function TControlCreatorHelper.Add(AClass: TControlClass; const AName: string;
  out Reference; AProc: TControlSetupProcObj): TControlCreator;
begin
  Result := {$IFDEF FPC}specialize{$ENDIF} Add<TControl>(TControlBuilder.Create(AClass, AName, Reference).Setup(AProc));
end;

function TControlCreatorHelper.Add(AClass: TControlClass; out Reference;
  AProc: TControlSetupProcObj): TControlCreator;
begin
  Result := {$IFDEF FPC}specialize{$ENDIF} Add<TControl>(TControlBuilder.Create(AClass, Reference).Setup(AProc));
end;

{$IFDEF FPC}generic{$ENDIF} function TControlCreatorHelper.Add<TBuild>(
  AControlBuilder: {$IFDEF FPC}specialize{$ENDIF} IControlBuilder<TBuild>): TControlCreator;
begin
  Result := {$IFDEF FPC}specialize{$ENDIF} Add<TBuild>(AControlBuilder, []);
end;

procedure TControlCreator.AddToGroups(AControl: TControl;
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

function TControlCreatorHelper.CenterControlsHorizontally(const AControlNames,
  AReferenceGroup: array of string): TControlCreator;
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

function TControlCreatorHelper.AlignControlsRight(const AControlNames,
  AReferenceGroup: array of string; const ARightPadding: Single = 0): TControlCreator;
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

function TControlCreatorHelper.SubLevel(AGroupName: string): TControlCreator;
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

function TControlCreatorHelper.SubLevel(
  ADirection: TControlCreatorDirection; AGroupName: string): TControlCreator;
begin
  Result := SubLevel(AGroupName);
  SetDirection(ADirection);
end;

function TControlCreatorHelper.SiblingSubLevel(ADirection: TControlCreatorDirection;
  ABreak: Boolean): TControlCreator;
begin
  Result := SiblingSubLevel(ADirection, '', ABreak);
end;

function TControlCreatorHelper.SiblingSubLevelWithBreak(
  ADirection: TControlCreatorDirection;
  AGroupName: string): TControlCreator;
begin
  Result := SiblingSubLevel(ADirection, AGroupName, True);
end;

function TControlCreatorHelper.SiblingSubLevelWithBreak(
  AGroupName: string): TControlCreator;
begin
  Result := SiblingSubLevel(AGroupName, True);
end;

function TControlCreatorHelper.SiblingSubLevel(ABreak: Boolean): TControlCreator;
begin
  Result := SiblingSubLevel('', ABreak);
end;

function TControlCreatorHelper.BreakLine(AIncTop: Single): TControlCreator;
begin
  Result := Self;
  Self.BreakLine;
  IncTop(AIncTop);
end;

destructor TControlCreator.Destroy;
var
  GroupList: TControlList;
begin
  for GroupList in FGroups.Values do
    GroupList.Free;
  FGroups.Free;
  FLevelStack.Free;
  FOwnControls.Free;
  inherited;
end;

function TControlCreatorHelper.SuperLevel: TControlCreator;
var
  SubL, SuperL: TControlCreatorLevel;
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
      RowSpan := SuperL.GridMode.GetRowSpanForFill;
      ColSpan := SuperL.GridMode.GetColSpanForFill;

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

function TControlCreatorHelper.RecalcParentHeight(AExtraHeight: Single): TControlCreator;
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

function TControlCreatorHelper.RecalcParentSize(AExtraHeight,
  AExtraWidth: Single): TControlCreator;
begin
  Result := Self;
  RecalcParentHeight(AExtraHeight);
  RecalcParentWidth(AExtraWidth);
end;

function TControlCreatorHelper.RecalcParentWidth(
  AExtraWidth: Single): TControlCreator;
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

function TControlCreatorHelper.CopyHeight(const AControlNames,
  AReferenceGroup: array of string): TControlCreator;
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

function TControlCreatorHelper.CopyWidth(const AControlNames,
  AReferenceGroup: array of string): TControlCreator;
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

function TControlCreatorHelper.CopySize(const AControlNames,
  AReferenceGroup: array of string): TControlCreator;
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

function TControlCreatorHelper.ReturnCurrentLevel(var ACurrentLevel:
  TControlCreatorLevel): TControlCreator;
begin
  Result := Self;
  ACurrentLevel := Self.CurrentLevel;
end;

function TControlCreatorHelper.ReturnLastControl(out AControl: TControl): TControlCreator;
begin
  Result := Self;
  AControl := nil;
  if Self.Controls.Count > 0 then
    AControl := Self.Controls.Last;
end;

function TControlCreator.GetNamedControl(const AName: string): TControl;
begin
  if not Registry.NamedComponents.TryGetValue(AName, TComponent(Result)) then
    Result := nil;
end;

function TControlCreatorHelper.IncLeft(AIncLeft: Single): TControlCreator;
begin
  Result := Self;
  CurrentLevel.CurrentLeft := CurrentLevel.CurrentLeft + AIncLeft;
end;

function TControlCreatorHelper.IncTop(AIncTop: Single): TControlCreator;
begin
  Result := Self;
  CurrentLevel.CurrentTop := CurrentLevel.CurrentTop + AIncTop;
end;

function TControlCreatorHelper.IncTopLeft(AIncTop,
  AIncLeft: Single): TControlCreator;
begin
  Result := Self;
  IncTop(AIncTop);
  IncLeft(AIncLeft);
end;

function TControlCreator.GetComponentRegistry: TComponentRegistry;
begin
  Result := FRegistryContextHandle.GetRegistry;
end;

function TControlCreator.GetContentWidth: Single;
begin
  Result := GetGroupBounds(FLevelStack.First.GroupName).Width;
end;

function TControlCreator.GetControl(const AName: string): TControl;
begin
  Result := Registry.GetControl(AName);
end;

{$IFDEF FPC}generic{$ENDIF}
function TControlCreator.GetControl<T>(const AName: string): T;
begin
  Result := Registry.{$IFDEF FPC}specialize{$ENDIF}GetControl<T>(AName);
end;

function TControlCreator.GetControls: TControlList;
var
  I: Integer;
begin
  // Remove referências a controles já destruídos (removidos do Registry
  // por notificação de free), evitando devolver ponteiros obsoletos, e
  // restringe o resultado aos controles criados por ESTE creator (e não
  // por outros creators que compartilhem o mesmo contexto de registry).
  for I := FOwnControls.Count - 1 downto 0 do
    if Registry.Controls.IndexOf(FOwnControls[I]) < 0 then
      FOwnControls.Delete(I);

  Result := FOwnControls;
end;

function TControlCreator.GetControlsBounds(
  AControlsNames: array of string): TControlGroupBounds;
var
  I: Integer;
  Name: string;
  Control: TControl;
  Group: TControlList;
  NamesStr: string;
begin
  Result.Reset;

  for I := Low(AControlsNames) to High(AControlsNames) do
  begin
    Name := AControlsNames[I];

    if Registry.NamedComponents.TryGetValue(Name, TComponent(Control)) then
      Result.Include(Control)
    else
      if FGroups.TryGetValue(Name, Group) then
        for Control in Group do
          Result.Include(Control);
  end;

  if Result.Left > Result.Right then
  begin
    NamesStr := '';
    for I := Low(AControlsNames) to High(AControlsNames) do
    begin
      if I > Low(AControlsNames) then
        NamesStr := NamesStr + ', ';
      NamesStr := NamesStr + AControlsNames[I];
    end;

    raise Exception.CreateFmt(
      'Nenhum controle encontrado para calcular os limites (bounds) do(s) nome(s)/grupo(s) "%s".',
      [NamesStr]);
  end;
end;

function TControlCreator.GetCurrenteLevel: TControlCreatorLevel;
begin
  Result := FLevelStack.Last;
end;

function TControlCreator.GetFContentHeight: Single;
begin
  Result := GetGroupBounds(FLevelStack.First.GroupName).Height;
end;

function TControlCreator.GetGroupBounds(
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

function TControlCreator.GetItem(const AName: string): TControl;
begin
  Result := Self.GetControl(AName);
end;

function TControlCreatorHelper.MoveControls(const AControl: TControl;
  const ADX, ADY: Single): TControlCreator;
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

function TControlCreatorHelper.MoveControls(const AControlNames: array of string;
  const ADX, ADY: Single): TControlCreator;
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

procedure TControlCreator.MoveTopLeftAfterBound(ABounds: TControlGroupBounds);
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

procedure TControlCreator.MoveTopLeftAfterRect(
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

procedure TControlCreator.MoveTopLeftAfterControl(AControl: TControl);
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

function TControlCreatorHelper.SiblingSubLevel(ADirection: TControlCreatorDirection;
  AGroupName: string; ABreak: Boolean): TControlCreator;
begin
  Result := SuperLevel;
  if ABreak then
    Break;
  SubLevel(ADirection, AGroupName);
end;

function TControlCreatorHelper.SiblingSubLevel(AGroupName: string;
  ABreak: Boolean): TControlCreator;
begin
  Result := SuperLevel;
  if ABreak then
    Break;
  SubLevel(AGroupName);
end;

function TControlCreatorHelper.SetControlHeight(AHeight: Single): TControlCreator;
begin
  Result := Self;
  CurrentLevel.ControlHeight := AHeight;
end;

function TControlCreatorHelper.SetControlWidth(AWidth: Single): TControlCreator;
begin
  Result := Self;
  CurrentLevel.ControlWidth := AWidth;
end;

function TControlCreatorHelper.SetControlWidthAndHeight(AWidth,
  AHeight: Single): TControlCreator;
begin
  Result := Self;
  CurrentLevel.ControlHeight := AHeight;
  CurrentLevel.ControlWidth := AWidth;
end;

function TControlCreatorHelper.UnsetControlHeight: TControlCreator;
begin
  Result := Self;
  CurrentLevel.ControlHeight := TOptionalSingle.None;
end;

function TControlCreatorHelper.UnsetControlWidth: TControlCreator;
begin
  Result := Self;
  CurrentLevel.ControlWidth := TOptionalSingle.None;
end;

function TControlCreatorHelper.UnsetControlWidthAndHeight: TControlCreator;
begin
  Result := Self;
  CurrentLevel.ControlHeight := TOptionalSingle.None;
  CurrentLevel.ControlWidth := TOptionalSingle.None;
end;

function TControlCreatorHelper.GridInit(ARows, ACols: Integer): TControlCreator;
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

function TControlCreatorHelper.GridCellSpan(ACellSpan: Integer): TControlCreator;
begin
  Result := Self;
  CurrentLevel.GridMode.ResetSpans;

  if CurrentLevel.Direction = cpdHorizontal then
    CurrentLevel.GridMode.ColSpan := ACellSpan
  else
    CurrentLevel.GridMode.RowSpan := ACellSpan;
end;

function TControlCreatorHelper.GridRowSpan(ARowSpan: Integer): TControlCreator;
begin
  Result := Self;
  CurrentLevel.GridMode.RowSpan := ARowSpan;
end;

function TControlCreatorHelper.GridColSpan(AColSpan: Integer): TControlCreator;
begin
  Result := Self;
  CurrentLevel.GridMode.ColSpan := AColSpan;
end;

function TControlCreatorHelper.GridSetCellWidthAndHeight(AWidth,
  AHeight: Integer): TControlCreator;
begin
  Result := Self;
  CurrentLevel.GridMode.CellWidth := AWidth;
  CurrentLevel.GridMode.CellHeight := AHeight;
end;

function TControlCreatorHelper.GridSetColWidth(ACol: Integer;
  AWidth: Single): TControlCreator;
begin
  Result := Self;
  CurrentLevel.GridMode.SetColWidth(ACol, AWidth);
end;

function TControlCreatorHelper.GridSetRowHeight(ARow: Integer;
  AHeight: Single): TControlCreator;
begin
  Result := Self;
  CurrentLevel.GridMode.SetRowHeight(ARow, AHeight);
end;

function TControlCreatorHelper.GridSetColOffset(ACol: Integer; AOffset: Single): TControlCreator;
begin
  Result := Self;
  CurrentLevel.GridMode.SetColOffset(ACol, AOffset);
end;

function TControlCreatorHelper.GridSetRowOffset(ARow: Integer; AOffset: Single): TControlCreator;
begin
  Result := Self;
  CurrentLevel.GridMode.SetRowOffset(ARow, AOffset);
end;

function TControlCreatorHelper.GridAutoExpand: TControlCreator;
begin
  Result := Self;
  CurrentLevel.GridMode.AutoExpand := [gaeRows, gaeCols];
end;

function TControlCreatorHelper.GridAutoExpandRows: TControlCreator;
begin
  Result := Self;
  CurrentLevel.GridMode.AutoExpand := [gaeRows];
end;

function TControlCreatorHelper.GridAutoExpandCols: TControlCreator;
begin
  Result := Self;
  CurrentLevel.GridMode.AutoExpand := [gaeCols];
end;

function TControlCreatorHelper.GridCellPosition(ACellPosition: TCellPosition): TControlCreator;
begin
  Result := Self;
  Self.CurrentLevel.GridMode.CellPosition := ACellPosition;
end;

function TControlCreatorHelper.GridCellNoStrech: TControlCreator;
begin
  Result := Self;
  Self.CurrentLevel.GridMode.CellStrech := [];
end;

function TControlCreatorHelper.GridCellStrechAll: TControlCreator;
begin
  Result := Self;
  Self.CurrentLevel.GridMode.CellStrech := [csVertical, csHorizontal];
end;

function TControlCreatorHelper.GridCellStrechHorizontal: TControlCreator;
begin
  Result := Self;
  Self.CurrentLevel.GridMode.CellStrech := [csHorizontal];
end;

function TControlCreatorHelper.GridCellStrechVertical: TControlCreator;
begin
  Result := Self;
  Self.CurrentLevel.GridMode.CellStrech := [csVertical];
end;

function TControlCreatorHelper.GridReturnNumberOfRows(out ARows: Integer): TControlCreator;
begin
  Result := Self;
  ARows := Self.CurrentLevel.GridMode.Rows;
end;

function TControlCreatorHelper.GridReturnNumberOfCols(out ACols: Integer): TControlCreator;
begin
  Result := Self;
  ACols := Self.CurrentLevel.GridMode.Cols;
end;

function TControlCreatorHelper.GridSkipCell: TControlCreator;
var
  Row, Col, RowSpan, ColSpan: Integer;
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

function TControlCreatorHelper.GridSkipCells(ANumCells: Integer): TControlCreator;
var
  I: Integer;
begin
  Result := Self;

  if not CurrentLevel.GridMode.Active then
    Exit;

  if ANumCells <= 0 then
    Exit;

  for I:=1 to ANumCells do
    GridSkipCell;
end;

function TControlCreatorHelper.GridGotoCell(ARow, ACol: Integer): TControlCreator;
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

function TControlCreatorHelper.GridFinish: TControlCreator;
begin
  Result := Self;
  if CurrentLevel.GridMode.Active then
  begin
    CurrentLevel.GridMode.Inactivate;
    SuperLevel;
  end;
end;

function TControlCreatorHelper.SetDirection(
  ADirection: TControlCreatorDirection): TControlCreator;
begin
  Result := Self;
  CurrentLevel.Direction := ADirection;
end;

function TControlCreatorHelper.SetHorizontalSpace(
  AHorizontalSpace: Single): TControlCreator;
begin
  Result := Self;
  CurrentLevel.HorizontalSpace := AHorizontalSpace;
end;

function TControlCreatorHelper.SetLeft(AControlName: string): TControlCreator;
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

function TControlCreatorHelper.SetLeft(ALeft: Single): TControlCreator;
begin
  Result := Self;
  CurrentLevel.CurrentLeft := ALeft;
  CurrentLevel.InitialLeft := ALeft;
  CurrentLevel.MaxControlHeight := 0;
  CurrentLevel.MaxControlWidth := 0;
end;

function TControlCreatorHelper.External(const AProc: TControlCreatorObjProc): TControlCreator;
begin
  Result := Self;
  if Assigned(AProc) then
    AProc(Self);
end;

function TControlCreatorHelper.External(const AProc: TControlCreatorProc): TControlCreator;
begin
  Result := Self;
  if Assigned(AProc) then
    AProc(Self);
end;

function TControlCreatorHelper.SetSpace(AVerticalSpace,
  AHorizontalSpace: Single): TControlCreator;
begin
  Result := Self;
  CurrentLevel.VerticalSpace := AVerticalSpace;
  CurrentLevel.HorizontalSpace := AHorizontalSpace;
end;

function TControlCreatorHelper.SetTop(ATop: Single): TControlCreator;
begin
  Result := Self;
  CurrentLevel.CurrentTop := ATop;
  CurrentLevel.InitialTop := ATop;
  CurrentLevel.MaxControlHeight := 0;
  CurrentLevel.MaxControlWidth := 0;
end;

function TControlCreatorHelper.SetTop(AControlName: string): TControlCreator;
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

function TControlCreatorHelper.SetTopLeft(ATop, ALeft: Single): TControlCreator;
begin
  Result := Self;
  SetTop(ATop);
  SetLeft(ALeft);
end;

function TControlCreatorHelper.SetTopLeftNearControl(AControlName: string;
  APosition: TRelativePosition): TControlCreator;
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

function TControlCreatorHelper.SetTopLeftNearControls(
  AControlsNames: array of string;
  APosition: TRelativePosition): TControlCreator;
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

function TControlCreatorHelper.SetTopLeftNearGroup(const AGroupName: string;
  APosition: TRelativePosition): TControlCreator;
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

function TControlCreatorHelper.SetVerticalSpace(
  AVerticalSpace: Single): TControlCreator;
begin
  Result := Self;
  CurrentLevel.VerticalSpace := AVerticalSpace
end;

{$IFNDEF FRAMEWORK_FMX}
function TControlCreatorHelper.WithOwnerAndParent(AOwner: TComponent;
  AParent: TWinControl): TControlCreator;
begin
  Result := SetOwnerAndParent(AOwner, AParent);
end;

function TControlCreatorHelper.SetOwnerAndParent(AOwner: TComponent;
  AParent: TWinControl): TControlCreator;
begin
  Result := Self;
  FOwner := AOwner;
  CurrentLevel.Parent := AParent;
end;

{$ENDIF}

{$IFDEF FRAMEWORK_FMX}
function TControlCreatorHelper.WithOwnerAndParent(AOwner: TComponent;
  AParent: TFmxObject): TControlCreator;
begin
  Result := SetOwnerAndParent(AOwner, AParent);
end;

function TControlCreatorHelper.SetOwnerAndParent(AOwner: TComponent;
  AParent: TFmxObject): TControlCreator;
begin
  Result := Self;
  FOwner := AOwner;
  CurrentLevel.Parent := TControl(AParent);
end;
{$ENDIF}

function TControlCreatorHelper.WithParent(AParent: TWinControl): TControlCreator;
begin
  Result := SetParent(AParent);
end;

function TControlCreatorHelper.SetParent(AParent: TWinControl): TControlCreator;
begin
  Result := Self;
  CurrentLevel.Parent := AParent;
end;

{$IFDEF FPC}generic{$ENDIF}
function TControlCreatorHelper.SubLevel<TBuild>(
  AControlBuilder: {$IFDEF FPC}specialize{$ENDIF} IControlBuilder<TBuild>;
  AGroupName: string
): TControlCreator;
var
  Control: TBuild;
  OwnerToUse: TComponent;
  IsTabChild: Boolean;
begin
  Result := Self;

  IsTabChild := False;

  // todo: precisa rever essa checagem
  if AControlBuilder is TControlBuilder then
    IsTabChild :=
      (AControlBuilder as TControlBuilder).ObjectClass.InheritsFrom(
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
    {$IFDEF FPC}specialize{$ENDIF} SetupControlBuilderForGridMode<TBuild>(AControlBuilder);

  // Control := specialize CreateControl<TBuild>(AControlBuilder, OwnerToUse);

  if not CurrentLevel.GridMode.Active then
  begin
    AControlBuilder.AssignReference(Control);
    {$IFDEF FPC}specialize{$ENDIF} Add<TBuild>(AControlBuilder);
  end
  else
  begin
    Control := {$IFDEF FPC}specialize{$ENDIF} CreateControl<TBuild>(AControlBuilder, OwnerToUse);
    MoveTopLeftAfterControl(Control as TControl);
  end;

  SubLevel(AGroupName);

  SetParent(
    {$IFDEF FRAMEWORK_FMX}TWinControl(Control)
    {$ELSE}TWinControl(Control)
    {$ENDIF}
  );

  SetTopLeft(0, 0);
end;

function TControlCreatorHelper.SubLevel(AControlBuilder: TControlBuilder; AGroupName: string=''): TControlCreator;
begin
  Result := {$IFDEF FPC}specialize{$ENDIF} SubLevel<TControl>(AControlBuilder, AGroupName);
end;

function TControlCreatorHelper.SubLevel(AControlBuilder: TControlBuilder; ADirection: TControlCreatorDirection; AGroupName: string=''): TControlCreator;
begin
  Result := {$IFDEF FPC}specialize{$ENDIF} SubLevel<TControl>(AControlBuilder, ADirection, AGroupName);
end;

{$IFDEF FPC}generic{$ENDIF} function TControlCreatorHelper.SubLevel<TBuild>(
  AControlBuilder: {$IFDEF FPC}specialize{$ENDIF} IControlBuilder<TBuild>;
  ADirection: TControlCreatorDirection; AGroupName: string): TControlCreator;
begin
  Result := {$IFDEF FPC}specialize{$ENDIF} SubLevel<TBuild>(AControlBuilder, AGroupName);
  SetDirection(ADirection);
end;

{$IFDEF FPC}generic{$ENDIF} function TControlCreatorHelper.SiblingSubLevel<TBuild>(
  AControlBuilder: {$IFDEF FPC}specialize{$ENDIF} IControlBuilder<TBuild>; AGroupName: string; ABreak: Boolean): TControlCreator;
begin
  Result := SuperLevel;
  if ABreak then
    Break;

  {$IFDEF FPC}specialize{$ENDIF}
  SubLevel<TBuild>(AControlBuilder, AGroupName);
end;

function TControlCreatorHelper.SiblingSubLevel(AControlBuilder: TControlBuilder; ADirection: TControlCreatorDirection; ABreak: Boolean): TControlCreator;
begin
  Result := {$IFDEF FPC}specialize{$ENDIF} SiblingSubLevel<TControl>(AControlBuilder, ADirection, ABreak);
end;

function TControlCreatorHelper.SiblingSubLevel(AControlBuilder: TControlBuilder; ABreak: Boolean=False): TControlCreator;
begin
  Result := {$IFDEF FPC}specialize{$ENDIF} SiblingSubLevel<TControl>(AControlBuilder, ABreak);
end;

function TControlCreatorHelper.SiblingSubLevel(AControlBuilder: TControlBuilder; ADirection: TControlCreatorDirection; AGroupName: string=''; ABreak: Boolean=False): TControlCreator;
begin
  Result := {$IFDEF FPC}specialize{$ENDIF} SiblingSubLevel<TControl>(AControlBuilder, ADirection, AGroupName, ABreak);
end;

function TControlCreatorHelper.SiblingSubLevel(AControlBuilder: TControlBuilder; AGroupName: string=''; ABreak: Boolean=False): TControlCreator;
begin
  Result := {$IFDEF FPC}specialize{$ENDIF} SiblingSubLevel<TControl>(AControlBuilder, AGroupName, ABreak);
end;

{$IFDEF FPC}generic{$ENDIF}
function TControlCreatorHelper.SiblingSubLevel<TBuild>(AControlBuilder: {$IFDEF FPC}specialize{$ENDIF} IControlBuilder<TBuild>; ADirection: TControlCreatorDirection; AGroupName: string; ABreak: Boolean): TControlCreator;
begin
  Result := SuperLevel;
  if ABreak then
    Break;
  {$IFDEF FPC}specialize{$ENDIF} SubLevel<TBuild>(AControlBuilder, ADirection, AGroupName);
end;

{$IFDEF FPC}generic{$ENDIF} function TControlCreatorHelper.SiblingSubLevel<TBuild>(
  AControlBuilder: {$IFDEF FPC}specialize{$ENDIF} IControlBuilder<TBuild>;
  ABreak: Boolean): TControlCreator;
begin
  Result := {$IFDEF FPC}specialize{$ENDIF} SiblingSubLevel<TBuild>(AControlBuilder, '', ABreak);
end;

{$IFDEF FPC}generic{$ENDIF} function TControlCreatorHelper.SiblingSubLevel<TBuild>(
  AControlBuilder: {$IFDEF FPC}specialize{$ENDIF} IControlBuilder<TBuild>;
  ADirection: TControlCreatorDirection; ABreak: Boolean): TControlCreator;
begin
  Result := {$IFDEF FPC}specialize{$ENDIF} SiblingSubLevel<TBuild>(AControlBuilder, ADirection, '', ABreak);
end;

{$IFDEF FPC}generic{$ENDIF} function TControlCreatorHelper.SiblingSubLevelWithBreak<TBuild>(
  AControlBuilder: {$IFDEF FPC}specialize{$ENDIF} IControlBuilder<TBuild>;
  ADirection: TControlCreatorDirection; AGroupName: string): TControlCreator;
begin
  Result := {$IFDEF FPC}specialize{$ENDIF} SiblingSubLevel<TBuild>(AControlBuilder, ADirection, AGroupName, True);
end;

function TControlCreatorHelper.SiblingSubLevelWithBreak(AControlBuilder: TControlBuilder; ADirection: TControlCreatorDirection; AGroupName: string=''): TControlCreator;
begin
  Result := {$IFDEF FPC}specialize{$ENDIF} SiblingSubLevelWithBreak<TControl>(AControlBuilder, ADirection, AGroupName);
end;

function TControlCreatorHelper.SiblingSubLevelWithBreak(AControlBuilder: TControlBuilder; AGroupName: string=''): TControlCreator;
begin
  Result := {$IFDEF FPC}specialize{$ENDIF} SiblingSubLevelWithBreak<TControl>(AControlBuilder, AGroupName);
end;

{$IFDEF FPC}generic{$ENDIF} function TControlCreatorHelper.SiblingSubLevelWithBreak<TBuild>(
  AControlBuilder: {$IFDEF FPC}specialize{$ENDIF} IControlBuilder<TBuild>;
  AGroupName: string): TControlCreator;
begin
  Result := {$IFDEF FPC}specialize{$ENDIF} SiblingSubLevel<TBuild>(AControlBuilder, AGroupName, True);
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
      if (gaeRows in AutoExpand) and (ARow >= (FRows - 1)) then
        Inc(FRows);
    gfdColFirst:
      if (gaeCols in AutoExpand) and (ACol >= (FCols - 1)) then
        Inc(FCols);
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
  begin
    Result := False;
    Exit;
  end;

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

{ TControlCreatorLevel }

function TControlCreatorLevel.Clone: TControlCreatorLevel;
begin
  Result := TControlCreatorLevel.Create;
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

constructor TControlCreatorLevel.Create;
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

destructor TControlCreatorLevel.Destroy;
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

  RegisterComponentForNotification(AComponent);
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

procedure TComponentRegistry.Add(AControl: TControl;
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
  begin
    Result := '';
    Exit;
  end;

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
    Result := Entry.Registry;
    Exit;
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
    if Pair.Value.Registry = Self then
    begin
      Result := Pair.Key;
      Exit;
    end;
end;

{ TComponentsBuilder }

function TComponentCreator.Add(AComponentBuilder: {$IFDEF FPC}specialize{$ENDIF}IComponentBuilder<TComponent>): TComponentCreator;
var
  Component: TComponent;
  ComponentName: string;
begin
  Result := Self;
  ComponentName := AComponentBuilder.Name;
  if not ComponentName.IsEmpty then
    ComponentName := Registry.UniqueName(AComponentBuilder.Name);

  AComponentBuilder.Owner := FOwner;
  AComponentBuilder.Name := ComponentName;
  Component := AComponentBuilder.Build;
  Registry.AddComponent(Component, Component.Name);
end;

constructor TComponentCreator.Create(ARegistryContextKey: string);
begin
  Create(TRegistryContextHandle.Create(ARegistryContextKey))
end;

constructor TComponentCreator.Create(ARegistryContextHandle: IRegistryContextHandle);
begin
  FRegistryContextHandle := ARegistryContextHandle;
end;

destructor TComponentCreator.Destroy;
begin
  inherited;
end;

function TComponentCreator.GetComponent(const AName: string): TComponent;
begin
  Result := Registry.GetComponent(AName);
end;

function TComponentCreator.External(const AProc: TComponentCreatorObjProc): TComponentCreator;
begin
  Result := Self;
  if Assigned(AProc) then
    AProc(Self);
end;

function TComponentCreator.External(const AProc: TComponentCreatorProc): TComponentCreator;
begin
  Result := Self;
  if Assigned(AProc) then
    AProc(Self);
end;

{$IFDEF FPC}generic{$ENDIF}
function TComponentCreator.GetComponent<T>(const AName: string): T;
begin
  Result := Registry.{$IFDEF FPC}specialize{$ENDIF} GetComponent<T>(AName);
end;

function TComponentCreator.GetComponentRegistry: TComponentRegistry;
begin
  Result := FRegistryContextHandle.GetRegistry;
end;

function TComponentCreator.GetComponents: TComponentList;
begin
  Result := Registry.FComponents;
end;

function TComponentCreator.GetItem(const AName: string): TComponent;
begin
  Result := Self.GetComponent(AName);
end;

function TComponentCreator.SetOwner(AOwner: TComponent): TComponentCreator;
begin
  Result := Self;
  FOwner := AOwner;
end;

function TComponentCreator.WithOwner(AOwner: TComponent): TComponentCreator;
begin
  Result := SetOwner(AOwner);
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
  if AContextKey.Trim.IsEmpty then
    FContextKey := GenerateAutoKey
  else
    FContextKey := AContextKey;
  FRegistry := TComponentRegistry.ForContext(FContextKey);
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

{ TOPCBCreators }

function TOPCBCreators.AsComponentCreator: TComponentCreator;
begin
  if not Assigned(FComponentCreator) then
    FComponentCreator := TComponentCreator.Create(FRegistryContextHandle);

  Result := FComponentCreator;
end;

function TOPCBCreators.AsControlCreator: TControlCreator;
begin
  if not Assigned(FControlCreator) then
    FControlCreator := TControlCreator.Create(FRegistryContextHandle);

  Result := FControlCreator;
end;

function TOPCBCreators.AsMenuCreator: TMenuCreator;
begin
  if not Assigned(FMenuCreator) then
    FMenuCreator := TMenuCreator.Create(FRegistryContextHandle);
  Result := FMenuCreator;
end;

constructor TOPCBCreators.Create(const ARegistryContextKey: string);
begin
  FRegistryContextHandle := TRegistryContextHandle.Create(ARegistryContextKey);
  FComponentCreator := nil;
  FControlCreator := nil;
  FMenuCreator := nil;
end;

destructor TOPCBCreators.Destroy;
begin
  if Assigned(FComponentCreator) then
    FComponentCreator.Free;

  if Assigned(FControlCreator) then
    FControlCreator.Free;

  if Assigned(FMenuCreator) then
    FMenuCreator.Free;

  inherited;
end;

{ TMenuCreator }

function TMenuCreator.AddMenu(AMenuBuilder: TMenuBuilder): TMenuCreator;
var
  Menu: TMenu;
  MenuName: string;
begin
  Result := Self;
  MenuName := AMenuBuilder.Name;
  if not MenuName.IsEmpty then
    MenuName := Registry.UniqueName(AMenuBuilder.Name);

  AMenuBuilder.Owner := FOwner;
  AMenuBuilder.Name := MenuName;

  Menu := AMenuBuilder.Build;
  Registry.AddComponent(Menu, Menu.Name);

  {$IFDEF FRAMEWORK_FMX}
  CurrentLevel.Parent := Menu;
  if FOwner is TFmxObject then
    (FOwner as TFmxObject).AddObject(Menu);
  {$ELSE}
  CurrentLevel.Parent := Menu.Items;
  {$ENDIF}
end;

function TMenuCreator.AddMenuItem(AMenuItemBuilder: TMenuItemBuilder): TMenuCreator;
var
  MenuItem: TMenuItem;
  MenuItemName: string;
begin
  Result := Self;
  MenuItemName := AMenuItemBuilder.Name;
  if not MenuItemName.IsEmpty then
    MenuItemName := Registry.UniqueName(AMenuItemBuilder.Name);

  AMenuItemBuilder.Owner := FOwner;
  AMenuItemBuilder.Name := MenuItemName;

  MenuItem := AMenuItemBuilder.Build;
  Registry.AddComponent(MenuItem, MenuItem.Name);

  {$IFDEF FRAMEWORK_FMX}
  CurrentLevel.Parent.AddObject(MenuItem);
  {$ELSE}
  CurrentLevel.Parent.Add(MenuItem);
  {$ENDIF}
end;

constructor TMenuCreator.Create(ARegistryContextKey: string);
begin
  Create(TRegistryContextHandle.Create(ARegistryContextKey));
end;

constructor TMenuCreator.Create(ARegistryContextHandle: IRegistryContextHandle);
begin
  FRegistryContextHandle := ARegistryContextHandle;
  FLevelStack := TMenuCreatorLevelStack.Create(True);
  FLevelStack.Add(TMenuCreatorLevel.Create);
end;

destructor TMenuCreator.Destroy;
begin
  FLevelStack.Free;
  inherited;
end;

function TMenuCreator.GetComponentRegistry: TComponentRegistry;
begin
  Result := FRegistryContextHandle.GetRegistry;
end;

function TMenuCreator.GetCurrenteLevel: TMenuCreatorLevel;
begin
  Result := FLevelStack.Last;
end;

function TMenuCreator.GetMenu(const AName: string): TMenu;
begin
  Result := Registry.GetComponent(AName) as TMenu;
end;

{$IFDEF FPC}generic{$ENDIF}
function TMenuCreator.GetMenu<T>(const AName: string): T;
begin
  Result := Registry.{$IFDEF FPC}specialize{$ENDIF} GetComponent<T>(AName);
end;

function TMenuCreator.GetMenuItem(const AName: string): TMenuItem;
begin
  Result := Registry.GetComponent(AName) as TMenuItem;
end;

{$IFDEF FPC}generic{$ENDIF}
function TMenuCreator.GetMenuItem<T>(const AName: string): T;
begin
  Result := Registry.{$IFDEF FPC}specialize{$ENDIF} GetComponent<T>(AName);
end;

function TMenuCreator.SetOwner(AOwner: TComponent): TMenuCreator;
begin
  Result := Self;
  FOwner := AOwner;
end;

function TMenuCreator.SubLevel(AMenuItemBuilder: TMenuItemBuilder): TMenuCreator;
var
  MenuItem: TMenuItem;
  MenuItemName: string;
begin
  Result := Self;
  MenuItemName := AMenuItemBuilder.Name;
  if not MenuItemName.IsEmpty then
    MenuItemName := Registry.UniqueName(AMenuItemBuilder.Name);

  AMenuItemBuilder.Owner := FOwner;
  AMenuItemBuilder.Name := MenuItemName;
  MenuItem := AMenuItemBuilder.Build;
  Registry.AddComponent(MenuItem, MenuItem.Name);

  {$IFDEF FRAMEWORK_FMX}
  CurrentLevel.Parent.AddObject(MenuItem);
  {$ELSE}
  CurrentLevel.Parent.Add(MenuItem);
  {$ENDIF}

  FLevelStack.Add(TMenuCreatorLevel.Create);
  CurrentLevel.Parent := MenuItem;
end;

function TMenuCreator.SuperLevel: TMenuCreator;
begin
  if FLevelStack.Count <= 1 then
    raise Exception.Create('PreviousLevel chamado no nível raiz');

  FLevelStack.Delete(FLevelStack.Count - 1); // remove nível atual
  Result := Self;
end;

function TMenuCreator.WithOwner(AOwner: TComponent): TMenuCreator;
begin
  Result := SetOwner(AOwner);
end;

function TMenuCreator.External(const AProc: TMenuCreatorObjProc): TMenuCreator;
begin
  Result := Self;
  if Assigned(AProc) then
    AProc(Self);
end;

function TMenuCreator.External(const AProc: TMenuCreatorProc): TMenuCreator;
begin
  Result := Self;
  if Assigned(AProc) then
    AProc(Self);
end;

{ TPropertySetup }

constructor TPropertySetup{$IFNDEF FPC}<T, TControlType>{$ENDIF}.Create(const AValue: T);
begin
  FValue := AValue;
end;

{ TPropEnum }

{$IFDEF FPC}generic{$ENDIF}
class function TPropertyValue.Create<E>(const APropName: string; const AValue: E): TPropertyValue;
begin
  Result.PropName := APropName;
  Result.Value := TValue.{$IFDEF FPC}specialize{$ENDIF}From<E>(AValue);
end;

{ TObjectBuilderBase }

procedure TObjectBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.ApplyPendingProps(Instance: TObject);
var
  Prop: TPropertyValue;
begin
  for Prop in FProperties do
    ApplyPropPath(Instance, Prop);
end;

procedure TObjectBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.ApplyPendingEvents(
  Instance: TObject);
var
  Ctx: TRttiContext;
  RttiType: TRttiType;
  Prop: TRttiProperty;
  Ev: TEventValue;
begin
  Ctx := TRttiContext.Create;
  try
    RttiType := Ctx.GetType(Instance.ClassType);

    for Ev in FEvents do
    begin
      Prop := RttiType.GetProperty(Ev.EventName);
      if Prop = nil then
        Continue;

      if not (Prop.PropertyType is TRttiMethodType) then
        Continue;

      SetMethodProp(Instance, ev.EventName, Ev.Method);
    end;
  finally
    Ctx.Free;
  end;
end;

procedure TObjectBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.ConfigureObject(AObject: TBuild);
var
  Ref: Pointer;
begin
  for Ref in FTargetFields do
    PPointer(Ref)^ := (AObject as TObject);
end;

procedure TObjectBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}
  .ApplyPropPath(Instance: TObject; AProp: TPropertyValue);
var
  Context: TRttiContext;
  RType: TRttiType;
  RProp: TRttiProperty;
  Parts: TStringList;
  i: Integer;
  CurrentObj: TObject;
  PropInfo: PPropInfo;
begin
  CurrentObj := Instance;
  Parts := TStringList.Create;
  try
    Parts.Delimiter := '.';
    Parts.StrictDelimiter := True;
    Parts.DelimitedText := Aprop.PropName;

    Context := TRttiContext.Create;
    for i := 0 to Parts.Count - 1 do
    begin
      RType := Context.GetType(CurrentObj.ClassType);
      RProp := RType.GetProperty(Parts[i]);
      if not Assigned(RProp) then
        raise EPropertyError.CreateFmt('Propriedade "%s" não encontrada em %s',
          [Parts[i], CurrentObj.ClassName]);

      if i < Parts.Count - 1 then
      begin
        CurrentObj := RProp.GetValue(CurrentObj).AsObject;
        if not Assigned(CurrentObj) then
          Exit;
      end
      else
        if RProp.IsWritable then
        begin
          PropInfo := GetPropInfo(CurrentObj.ClassInfo, Parts[i]);
          if RProp.PropertyType.TypeKind = tkSet then
            SetOrdProp(CurrentObj, PropInfo, Integer(AProp.Value.AsOrdinal))
          else
            RProp.SetValue(CurrentObj, AProp.Value);
        end;
    end;
  finally
    Parts.Free;
  end;
end;

constructor TObjectBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.Create;
begin
  FSetupProcList := TSetupProcObjList.Create;
  FSetupRefProcList := TSetupRefProcList.Create;
  FProperties := TPropValueList.Create;
  FEvents := TEventValueList.Create;
  FTargetFields := TPointerList.Create;
end;

constructor TObjectBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.Create(AClass: TObjectClass);
begin
  Create;
  FObjectClass := AClass;
end;

constructor TObjectBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.Create(AClass: TObjectClass; out Reference);
begin
  Create(AClass);
  Assign(Reference);
end;

destructor TObjectBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.Destroy;
begin
  FSetupProcList.Free;
  FSetupRefProcList.Free;
  FProperties.Free;
  FEvents.Free;
  FTargetFields.Free;
  inherited;
end;

procedure TObjectBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.AssignReference(out Reference);
begin
  Assign(Reference);
end;

procedure TObjectBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}
  .ResetReferences;
begin
  FTargetFields.Clear;
end;

function TObjectBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.Assign(out Reference): TSelf;
begin
  Result := TSelf(Self);
  FTargetFields.Add(@Reference);
end;

function TObjectBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.WithProp(
  const APropName: string; const AValue: TValue): TSelf;
var
  P: TPropertyValue;
begin
  Result := TSelf(Self);
  P.PropName := APropName;
  P.Value := AValue;
  FProperties.Add(P);
end;

function TObjectBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.WithEvent(const AEventName: string;
  const AInstance: TObject; const AMethod: Pointer): TSelf;
var
  E: TEventValue;
begin
  Result := TSelf(Self);
  E.EventName := AEventName;
  E.Method.Code := AMethod;
  E.Method.Data := AInstance;
  FEvents.Add(E);
end;

function TObjectBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.WithProp(const APropValue: TPropertyValue): TSelf;
begin
  Result := Self.WithProp(APropValue.PropName, APropValue.Value);
end;

function TObjectBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.WithPropObj(
  const APropName: string; AObj: TObject): TSelf;
begin
  Result := WithProp(
    TPropertyValue.{$IFDEF FPC}specialize{$ENDIF}Create<TObject>(APropName, AObj)
  );
end;

function TObjectBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.WithPropSet(const APropName: string; const AValue: Integer): TSelf;
begin
  Result := WithProp(APropName, TValue.{$IFDEF FPC}specialize{$ENDIF}From<NativeInt>(AValue));
end;

function TObjectBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.WithEvent(
  const AEventName: string; const AMethod: TMethod): TSelf;
var
  E: TEventValue;
begin
  Result := TSelf(Self);
  E.EventName := AEventName;
  E.Method := AMethod;
  FEvents.Add(E);
end;

procedure TObjectBuilderBase{$IFNDEF FPC}<TBuild,  TSelf>{$ENDIF}.SetupProc(
  AProcObj: {$IFDEF FPC}specialize{$ENDIF} TSetupProcObj<TBuild>);
begin
  FSetupProcList.Add(AProcObj);
end;

procedure TObjectBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.SetupProc(
  ARefProc: {$IFDEF FPC}specialize{$ENDIF} TSetupRefProc<TBuild>);
begin
  FSetupRefProcList.Add(ARefProc);
end;

function TObjectBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.Build: TBuild;
var
  Proc: TSetupProcObjBuild;
  RefProc: TSetupRefProcBuild;
  Ref: Pointer;
begin
  Result := CreateObject;
  try
    ConfigureObject(Result);
    ApplyPendingProps(Result);
    ApplyPendingEvents(Result);

    for Proc in FSetupProcList do
      Proc(Result);

    for RefProc in FSetupRefProcList do
      RefProc(Result);
  except
    // Se algo falhar apos a criacao (prop/evento invalido, setup do
    // usuario), o objeto ja foi criado por CreateObject e precisa ser
    // destruido aqui - do contrario ele vaza, pois a excecao impede o
    // Result de chegar ate quem chamou Build e nenhum outro codigo tem
    // referencia a ele. As variaveis externas associadas via Assign/out
    // Reference tambem ja podem apontar para ele (ConfigureObject roda
    // antes de ApplyPendingProps/ApplyPendingEvents/Setup), entao sao
    // zeradas primeiro para nao ficarem com ponteiro pendente.
    for Ref in FTargetFields do
      PPointer(Ref)^ := nil;
    (Result as TObject).Free;
    raise;
  end;
end;

function TObjectBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.Setup(
  AProc: TSetupRefProcBuild): TSelf;
begin
  Result := TSelf(Self);
  Self.SetupProc(AProc);
end;

function TObjectBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.Setup(
  AProc: TSetupProcObjBuild): TSelf;
begin
  Result := TSelf(Self);
  Self.SetupProc(AProc);
end;

function TObjectBuilder.CreateObject: TObject;
begin
  Result := TObjectClass(FObjectClass).Create;
end;

constructor TObjectBuilder.Create;
begin
  inherited Create(TObject);
end;

constructor TObjectBuilder.Create(out Reference);
begin
  inherited Create(TObject, Reference);
end;

constructor TObjectBuilder.Create(AClass: TObjectClass);
begin
  inherited Create(AClass);
end;

constructor TObjectBuilder.Create(AClass: TObjectClass; out Reference);
begin
  inherited Create(AClass, Reference);
end;

{ TComponentBuilderBase<TSelf> }

constructor TComponentBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.Create(AClass: TComponentClass; out Reference);
begin
  Create(AClass);
  Assign(Reference);
end;

function TComponentBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.GetName: string;
begin
  Result := FName;
end;

function TComponentBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.GetOwner: TComponent;
begin
  Result := FOwner;
end;

function TComponentBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.GetTag: NativeInt;
begin
  Result := FTag;
end;

procedure TComponentBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.SetName(AValue: string);
begin
  FName := AValue;
end;

procedure TComponentBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.SetOwner(AValue: TComponent);
begin
  FOwner := AValue;
end;

procedure TComponentBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.SetTag(AValue: NativeInt);
begin
  FTag := AValue;
end;

procedure TComponentBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.ConfigureObject(AObject: TBuild);
begin
  inherited ConfigureObject(AObject);
  if not Self.Name.IsEmpty then
    AObject.Name := Self.Name;
  AObject.Tag := FTag;
end;

constructor TComponentBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.Create(AClass: TComponentClass;
  const AName: string; out Reference);
begin
  Create(AClass, AName);
  Assign(Reference);
end;

constructor TComponentBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.Create(AClass: TComponentClass;
  const AName: string);
begin
  inherited Create;
  FObjectClass := AClass;
  FName := AName;
  FTag := 0;
end;

function TComponentBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.WithName(AName: string): TSelf;
begin
  Result := TSelf(Self);
  FName := AName;
end;

function TComponentBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.WithOwner(
  AOwner: TComponent): TSelf;
begin
  Result := TSelf(Self);
  FOwner := AOwner;
end;

function TComponentBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.WithTag(ATag: NativeInt): TSelf;
begin
  Result := TSelf(Self);
  FTag := ATag;
end;

{ TMenuBuilderBase<TSelf> }

constructor TControlBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.Create;
begin
  Create(TControl);
end;

{ TMenuItemBuilderBase<TSelf> }

function TMenuItemBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.GetCaption: TOptionalString;
begin
  Result := FCaption;
end;

function TMenuItemBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.GetImageIndex: TOptionalInteger;
begin
  Result := FImageIndex;
end;

function TMenuItemBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.GetOnClick: TNotifyEvent;
begin
  Result := FOnClick;
end;

procedure TMenuItemBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.SetCaption(AValue: TOptionalString);
begin
  FCaption := AValue;
end;

procedure TMenuItemBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.SetImageIndex(AValue: TOptionalInteger);
begin
  FImageIndex := AValue;
end;

procedure TMenuItemBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.SetOnClick(AValue: TNotifyEvent);
begin
  FOnClick := AValue;
end;

constructor TMenuItemBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.Create;
begin
  Create(TMenuItem);
end;

procedure TMenuItemBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.ConfigureObject(AObject: TBuild);
begin
  inherited ConfigureObject(AObject);

  if Caption.HasValue then
    {$IFDEF FRAMEWORK_FMX}
    AObject.Text := Caption.Value;
    {$ELSE}
    AObject.Caption := Caption.Value;
    {$ENDIF}

  if ImageIndex.HasValue then
    AObject.ImageIndex := ImageIndex.Value;

  AObject.OnClick := OnClick;
end;

function TMenuItemBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.WithCaption(ACaption: string): TSelf;
begin
  Result := TSelf(Self);
  FCaption := ACaption;
end;

function TMenuItemBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.WithImageIndex(
  AImageIndex: Integer): TSelf;
begin
  Result := TSelf(Self);
  FImageIndex := AImageIndex;
end;

function TMenuItemBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.WithOnClick(AOnClick: TNotifyEvent): TSelf;
begin
  Result := TSelf(Self);
  FOnClick := AOnClick;
end;

procedure TControlBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.ConfigureObject(AObject: TBuild);
begin
  inherited ConfigureObject(AObject);
  AObject.Parent := Self.Parent;

  if Caption.HasValue then
  begin
    {$IFDEF FRAMEWORK_FMX}
    if AObject is TPresentedTextControl then
      TPresentedTextControl(AObject).Text := Caption.Value;
    if AObject is TTextControl then
      TTextControl(AObject).Text := Caption.Value;
    {$ELSE}
      {$IFDEF FPC}
      AObject.Caption := Caption.Value;
      {$ELSE}
      TProtectedControl(AObject).Caption := Caption.Value;
      {$ENDIF}
    {$ENDIF}
  end;

  if Text.HasValue then
  begin
    {$IFDEF FRAMEWORK_FMX}
    if AObject is TPresentedTextControl then
      TPresentedTextControl(AObject).Text := Text.Value;
    if AObject is TTextControl then
      TTextControl(AObject).Text := Text.Value;
    {$ELSE}
      {$IFDEF FPC}
        AObject.Caption := Text.Value;
      {$ELSE}
        TProtectedControl(AObject).Text := Text.Value;
      {$ENDIF}
    {$ENDIF}
  end;

  if Align.HasValue then
    AObject.Align := Align.Value;

  if Width.HasValue then
    AObject.Width := {$IFDEF FRAMEWORK_FMX}Width.Value{$ELSE}Trunc(Width.Value){$ENDIF};

  if Height.HasValue then
    AObject.Height := {$IFDEF FRAMEWORK_FMX}Height.Value{$ELSE}Trunc(Height.Value){$ENDIF};

  if Top.HasValue then
  begin
    {$IFDEF FRAMEWORK_FMX}
    AObject.Position.Y := Top.Value;
    {$ELSE}
    AObject.Top := Trunc(Top.Value);
    {$ENDIF}
  end;

  if Left.HasValue then
  begin
    {$IFDEF FRAMEWORK_FMX}
    AObject.Position.X := Left.Value;
    {$ELSE}
    AObject.Left := Trunc(Left.Value);
    {$ENDIF}
  end;

  TProtectedControl(AObject).OnClick := OnClick;
end;

function TControlBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.GetParent: TWinControl;
begin
  Result := FParent;
end;

procedure TControlBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.SetParent(AValue: TWinControl);
begin
  FParent := AValue;
end;

function TControlBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.GetAlign: TOptionalAlign;
begin
  Result := FAlign;
end;

function TControlBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.GetCaption: TOptionalString;
begin
  Result := FCaption;
end;

function TControlBuilderBase{$IFNDEF FPC}<TBuild,  TSelf>{$ENDIF}.GetHeight: TOptionalSingle;
begin
  Result := FHeight;
end;

function TControlBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.GetLeft: TOptionalSingle;
begin
  Result := FLeft;
end;

function TControlBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.GetOnClick: TNotifyEvent;
begin
  Result := FOnClick;
end;

function TControlBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.GetText: TOptionalString;
begin
  Result := FText;
end;

function TControlBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.GetTop: TOptionalSingle;
begin
  Result := FTop;
end;

function TControlBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.GetWidth: TOptionalSingle;
begin
  Result := FWidth;
end;

procedure TControlBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.SetAlign(AValue: TOptionalAlign);
begin
  FAlign := AValue;
end;

procedure TControlBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.SetCaption(AValue: TOptionalString);
begin
  FCaption := AValue;
end;

procedure TControlBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.SetHeight(AValue: TOptionalSingle);
begin
  FHeight := AValue;
end;

procedure TControlBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.SetLeft(AValue: TOptionalSingle);
begin
  FLeft := AValue
end;

procedure TControlBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.SetOnClick(AValue: TNotifyEvent);
begin
  FOnClick := AValue;
end;

procedure TControlBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.SetText(AValue: TOptionalString);
begin
  FText := AValue;
end;

procedure TControlBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.SetTop(AValue: TOptionalSingle);
begin
  FTop := AValue;
end;

procedure TControlBuilderBase{$IFNDEF FPC}<TBuild, TSelf>{$ENDIF}.SetWidth(AValue: TOptionalSingle);
begin
  FWidth := AValue;
end;

function TControlBuilder.CreateObject: TControl;
begin
  Result := TControlClass(FObjectClass).Create(Owner);
end;

constructor TControlBuilder.Create;
begin
  inherited Create(TControl);
end;

constructor TControlBuilder.Create(AClass: TControlClass; const AName: string);
begin
  inherited Create(AClass, AName);
end;

constructor TControlBuilder.Create(AClass: TControlClass; const AName: string;
  out Reference);
begin
  inherited Create(AClass, AName, Reference);
end;

constructor TControlBuilder.Create(AClass: TControlClass; out Reference);
begin
  inherited Create(AClass, Reference);
end;

{ TComponentBuilder }

constructor TComponentBuilder.Create;
begin
  inherited Create(TComponent);
end;

constructor TComponentBuilder.Create(AClass: TComponentClass;
  const AName: string);
begin
  inherited Create(AClass, AName);
end;

constructor TComponentBuilder.Create(AClass: TComponentClass;
  const AName: string; out Reference);
begin
  inherited Create(AClass, AName, Reference);
end;

constructor TComponentBuilder.Create(AClass: TComponentClass; out Reference);
begin
  inherited Create(AClass, Reference);
end;

function TComponentBuilder.CreateObject: TComponent;
begin
  Result := TComponentClass(FObjectClass).Create(Owner);
end;

{ TMenuBuilder }

constructor TMenuBuilder.Create;
begin
  inherited Create(TMenu);
end;

constructor TMenuBuilder.Create(AClass: TMenuClass; const AName: string);
begin
  inherited Create(AClass, AName);
end;

constructor TMenuBuilder.Create(AClass: TMenuClass; const AName: string;
  out Reference);
begin
  inherited Create(AClass, AName, Reference);
end;

constructor TMenuBuilder.Create(AClass: TMenuClass; out Reference);
begin
  inherited Create(AClass, Reference);
end;

function TMenuBuilder.CreateObject: TMenu;
begin
  Result := TMenuClass(FObjectClass).Create(Owner);
end;

{ TMenuItemBuilder }

constructor TMenuItemBuilder.Create;
begin
  inherited Create(TMenuItem);
end;

constructor TMenuItemBuilder.Create(AClass: TMenuItemClass; const AName: string);
begin
  inherited Create(AClass, AName);
end;

constructor TMenuItemBuilder.Create(AClass: TMenuItemClass; const AName: string;
  out Reference);
begin
  inherited Create(AClass, AName, Reference);
end;

constructor TMenuItemBuilder.Create(AClass: TMenuItemClass; out Reference);
begin
  inherited Create(AClass, Reference);
end;

function TMenuItemBuilder.CreateObject: TMenuItem;
begin
  Result := TMenuItemClass(FObjectClass).Create(Owner);
end;

initialization

finalization
  TComponentRegistry.Finalize;

end.

