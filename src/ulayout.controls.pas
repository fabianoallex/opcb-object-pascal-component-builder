unit ulayout.controls;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$LONGSTRINGS ON}{$MODESWITCH TYPEHELPERS}{$MODESWITCH ADVANCEDRECORDS}
{$ENDIF}

interface

uses
  Controls, StdCtrls, Classes, SysUtils, ULayout, UGridLayoutBuilder,
  UGridLayoutFillerFactory, Generics.Collections, Generics.Defaults;

type

  TControlClass = class of TControl;
  TControlCreateProc = procedure(AControl: TControl; AIndex: Integer;
    ASettings: TGridCellSettings) of object;

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
    FOnControlCreate: TControlCreateProc;
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

    function CreateControl(AControlInfo: TControlInfo;
      AProc: TControlCreateProc=nil): TControlGridPopulator; overload;

    function CreateControl(AControlClass: TControlClass;
      AProc: TControlCreateProc=nil): TControlGridPopulator; overload;

    function CretaeControls(ACount: Integer; AControlClass: TControlClass;
      AProc: TControlCreateProc=nil): TControlGridPopulator; overload;

    function CretaeControls(ACount: Integer; AControlInfo: TControlInfo;
      AProc: TControlCreateProc=nil): TControlGridPopulator; overload;

    function CreateControls(ACount: Integer;
      AControlClasses: array of TControlClass;
      AProc: TControlCreateProc=nil): TControlGridPopulator; overload;

    function CreateControls(ACount: Integer;
      AControlCreateInfos: array of TControlInfo;
      AProc: TControlCreateProc=nil): TControlGridPopulator; overload;

    function CreateControls(AControlCreateInfos: array of TControlInfo;
      AProc: TControlCreateProc=nil): TControlGridPopulator; overload;

    function OnControlCreate(AProc: TControlCreateProc): TControlGridPopulator;
    property Controls: {$IFDEF FPC}specialize{$ENDIF} TList<TControl> read FControls write SetControls;
    property NamedControls[const AName: string]: TControl read GetNamedControl;
    property Grid: TGridLayout read FGrid;
  end;

  { TGridLayoutHelper }

  TGridLayoutHelper = class helper for TGridLayout
  public
    function GetControl(ARow, ACol: Integer): TControl;
  end;

  { TGridLayoutBuilderHelper }

  TGridLayoutBuilderHelper = class helper for TGridLayoutBuilder
  public
    function BuildAndPopulate(var AGrid: TGridLayout;
      APopulator: TControlGridPopulator): TControlGridPopulator;
  end;

implementation

uses
  Graphics;

{ TControlInfo }

class function TControlInfo.Create(AClass: TControlClass;
  const AName: string): TControlInfo;
begin
  Result.ControlClass := AClass;
  Result.ControlName := AName;
end;

{ TControlGridPopulator }

procedure TControlGridPopulator.SetControls(AValue: {$IFDEF FPC}specialize{$ENDIF} TList<TControl
  >);
begin
  if FControls = AValue then Exit;
  FControls := AValue;
end;

function TControlGridPopulator.GetNamedControl(const AName: string): TControl;
begin
  if not FNamedControls.TryGetValue(AName, Result) then
    Result := nil; //raise Exception.CreateFmt('Controle com nome "%s" não encontrado.', [AName]);
end;

procedure TControlGridPopulator.ConfigControl(AControl: TControl);
begin
  if AControl is TLabel then
    with (AControl as TLabel) do
    begin
      Layout := tlCenter;
      AutoSize := False;
    end;

  if AControl is TCheckBox then
    with (AControl as TCheckBox) do
    begin
      {$IFDEF FPC}
      AutoSize := False;
      {$ENDIF}
    end;

  if AControl is TEdit then
    with (AControl as TEdit) do
    begin
      AutoSize := False;
    end;
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

function TControlGridPopulator.CreateControl(AControlInfo: TControlInfo;
  AProc: TControlCreateProc): TControlGridPopulator;
var
  Control: TControl;
  ControlGridItem: TControlGridItem;
  Settings: TGridCellSettings;

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
  Control := AControlInfo.ControlClass.Create(FOwner);
  Control.Parent := FParent;

  FControls.Add(Control);

  if not AControlInfo.ControlName.IsEmpty then
  begin
    Control.Name := UniqueName(AControlInfo.ControlName);
    FNamedControls.Add(Control.Name, Control);
  end;

  ControlGridItem := TControlGridItem.Create(Control);
  Settings := TGridCellSettings.Create(0, 0);

  try
    if Assigned(FOnControlCreate) then
      FOnControlCreate(Control, FControls.Count-1, Settings);
    if Assigned(AProc) then
      AProc(Control, FControls.Count-1, Settings);

    FFiller.PlaceItem(ControlGridItem, Settings);
  finally
    Settings.Free;
  end;
end;

function TControlGridPopulator.CreateControl(AControlClass: TControlClass;
  AProc: TControlCreateProc): TControlGridPopulator;
var
  ControlInfo: TControlInfo;
begin
  ControlInfo.ControlClass := AControlClass;
  ControlInfo.ControlName := '';
  Result := CreateControl(ControlInfo, AProc);
end;

function TControlGridPopulator.CretaeControls(ACount: Integer;
  AControlClass: TControlClass; AProc: TControlCreateProc
  ): TControlGridPopulator;
var
  I: Integer;
begin
  Result := Self;
  for I:=1 to ACount do
    CreateControl(AControlClass, AProc);
end;

function TControlGridPopulator.CretaeControls(ACount: Integer;
  AControlInfo: TControlInfo; AProc: TControlCreateProc
  ): TControlGridPopulator;
var
  I: Integer;
begin
  Result := Self;
  for I:=1 to ACount do
    CreateControl(AControlInfo, AProc);
end;

function TControlGridPopulator.CreateControls(ACount: Integer;
  AControlClasses: array of TControlClass; AProc: TControlCreateProc
  ): TControlGridPopulator;
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
  AControlCreateInfos: array of TControlInfo; AProc: TControlCreateProc
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
  (AControlCreateInfos: array of TControlInfo; AProc: TControlCreateProc
  ): TControlGridPopulator;
begin
  Result := CreateControls(Length(AControlCreateInfos), AControlCreateInfos, AProc);
end;

function TControlGridPopulator.OnControlCreate(AProc: TControlCreateProc
  ): TControlGridPopulator;
begin
  Result := Self;
  FOnControlCreate := AProc;
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

function TGridLayoutHelper.GetControl(ARow, ACol: Integer): TControl;
var
  Cell: TGridCell;
begin
  Result := nil;

  Cell := Self.GetCell(0,0);

  if (not Assigned(Cell))
    or (not Assigned(Cell.Item))
    or (not Assigned(Cell.Item.GetVisualElement)) then
    Exit;

  Result := (Cell.Item.GetVisualElement as TControlVisualElement).GetControl;
end;

{ TGridLayoutBuilderHelper }

function TGridLayoutBuilderHelper.BuildAndPopulate(var AGrid: TGridLayout;
  APopulator: TControlGridPopulator): TControlGridPopulator;
begin
  AGrid := Self.Build;
  APopulator.SetGrid(AGrid);
  Result := APopulator;
end;

end.

