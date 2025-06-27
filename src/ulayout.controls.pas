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

  { TControlInfo }

  TControlInfo = record
    ControlClass: TControlClass;
    Name: string;
    Align: TAlign;
    Width: Single;
    Height: Single;
    Top: TOptionalSingle;
    Left: TOptionalSingle;
    function WithAlign(AAlign: TAlign): TControlInfo;
    function WithWidth(AWidth: Single): TControlInfo;
    function WithHeight(AHeight: Single): TControlInfo;
    function WithTop(ATop: Single): TControlInfo;
    function WithLeft(ALeft: Single): TControlInfo;
    class function Create(AClass: TControlClass; const AName: string): TControlInfo; static;
  end;

  TStrControlDictionary =
    {$IFDEF FPC}specialize{$ENDIF} TDictionary<string, TControl>;

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
    procedure SetControls(
      AValue: {$IFDEF FPC}specialize{$ENDIF} TList<TControl>);
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

  TControlPopulatorDirection = (cpdHorizontal, cpdVertical);

  TControlPopulator = class
  private
    FOwner: TComponent;
    FParent: TWinControl;
    FControls: {$IFDEF FPC}specialize{$ENDIF} TList<TControl>;
    FNamedControls: TStrControlDictionary;
    FDirection: TControlPopulatorDirection;
    FVerticalSpace: Single;
    FHorizontalSpace: Single;
    FInitialLeft: Single;
    FInitialTop: Single;
    FCurrentTop: Single;
    FCurrentLeft: Single;
    FCurrentMaxControlHeight: Single;
    FCurrentRow: Integer;
    FCurrentCol: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function SetSpace(AVerticalSpace, AHorizontalSpace: Single): TControlPopulator;
    function SetVerticalSpace(AVerticalSpace: Single): TControlPopulator;
    function SetHorizontalSpace(AHorizontalSpace: Single): TControlPopulator;
    function SetTopLeft(ATop, ALeft: Single): TControlPopulator;
    function SetTop(ATop: Single): TControlPopulator;
    function SetLeft(ALeft: Single): TControlPopulator;
    function SetDirection(ADirection: TControlPopulatorDirection): TControlPopulator;
    function BreakLine: TControlPopulator;
    function WithOwnerAndParent(AOwner: TComponent; AParent: TWinControl): TControlPopulator;
    function WithParent(AParent: TWinControl): TControlPopulator;
    function AddControl(AControl: TControl): TControlPopulator;
    function CreateControl(AControlInfo: TControlInfo): TControlPopulator; overload;
    function CreateControls(AControlCreateInfos: array of TControlInfo): TControlPopulator;
    function GetNamedControl(const AName: string): TControl;
    property NamedControls[const AName: string]: TControl read GetNamedControl;
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
  Result.ControlClass := AClass;
  Result.Name := AName;
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

function TControlPopulator.AddControl(AControl: TControl): TControlPopulator;
var
  ControlGridItem: TControlGridItem;
  Settings: TGridCellSettings;
begin
  FControls.Add(AControl);

  if not string(AControl.Name).IsEmpty then
    FNamedControls.Add(AControl.Name, AControl);
end;

function TControlPopulator.BreakLine: TControlPopulator;
begin
  Result := Self;
  FCurrentTop := FCurrentTop + FCurrentMaxControlHeight;
  FCurrentLeft := FInitialLeft;
  FCurrentMaxControlHeight := 0;
  FCurrentRow := FCurrentRow + 1;
  FCurrentCol := 0;
end;

constructor TControlPopulator.Create;
begin
  FControls := {$IFDEF FPC}specialize{$ENDIF} TList<TControl>.Create;
  FNamedControls := TStrControlDictionary.Create;
  FDirection := cpdHorizontal;
  FVerticalSpace := 0;
  FHorizontalSpace := 0;
  FInitialLeft := 0;
  FCurrentTop := 0;
  FCurrentLeft := 0;
  FCurrentMaxControlHeight := 0;
end;

function TControlPopulator.CreateControl(
  AControlInfo: TControlInfo): TControlPopulator;
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

  function GetVerticalSpace: Single;
  begin
    if FCurrentRow = 0 then
      Result := 0
    else
      Result := FVerticalSpace;
  end;

  function GetHorizontalSpace: Single;
  begin
    if FCurrentCol = 0 then
      Result := 0
    else
      Result := FHorizontalSpace;
  end;

begin
  Result := Self;

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
    Control.Top := Trunc(AControlInfo.Top.Value + GetVerticalSpace)
  else
    Control.Top := Trunc(FCurrentTop + GetVerticalSpace);

  if AControlInfo.Left.HasValue then
    Control.Left := Trunc(AControlInfo.Left.Value + GetHorizontalSpace)
  else
    Control.Left := Trunc(FCurrentLeft + GetHorizontalSpace);

  if AControlInfo.Align = alNone then
  begin
    if FDirection = cpdHorizontal then
    begin
      FCurrentLeft := FCurrentLeft + Control.Width + GetHorizontalSpace;
      FCurrentMaxControlHeight := Max(FCurrentMaxControlHeight, Control.Height + GetVerticalSpace);
      FCurrentCol := FCurrentCol + 1;
    end;

    if FDirection = cpdVertical then
    begin
      FCurrentTop := FCurrentTop + Control.Height + GetVerticalSpace;
      FCurrentRow := FCurrentRow + 1;
    end;
  end;

  AddControl(Control);
end;

function TControlPopulator.CreateControls(
  AControlCreateInfos: array of TControlInfo): TControlPopulator;
var
  I: Integer;
begin
  Result := Self;
  for I := Low(AControlCreateInfos) to High(AControlCreateInfos) do
    CreateControl(AControlCreateInfos[I]);
end;

destructor TControlPopulator.Destroy;
begin
  FControls.Free;
  FNamedControls.Free;
  FCurrentCol := 0;
  FCurrentRow := 0;
  inherited;
end;

function TControlPopulator.GetNamedControl(const AName: string): TControl;
begin
  if not FNamedControls.TryGetValue(AName, Result) then
    Result := nil;
end;

function TControlPopulator.SetDirection(
  ADirection: TControlPopulatorDirection): TControlPopulator;
begin
  Result := Self;
  FDirection := ADirection;
end;

function TControlPopulator.SetHorizontalSpace(
  AHorizontalSpace: Single): TControlPopulator;
begin
  Result := Self;
  FHorizontalSpace := AHorizontalSpace;
end;

function TControlPopulator.SetLeft(ALeft: Single): TControlPopulator;
begin
  Result := Self;
  FCurrentLeft := ALeft;
  FInitialLeft := ALeft;
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
  FCurrentTop := ATop;
  FInitialTop := ATop;
end;

function TControlPopulator.SetTopLeft(ATop, ALeft: Single): TControlPopulator;
begin
  Result := Self;
  SetTop(ATop);
  SetLeft(ALeft);
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

end.

