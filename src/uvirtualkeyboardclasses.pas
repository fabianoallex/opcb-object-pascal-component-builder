unit UVirtualKeyboardClasses;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, Buttons, fgl;

type
  { TVirtualKey }
  TVirtualKey = class(TSpeedButton)
  private
    FInternalCode: string;
    FMappedKeyCode: Word;
    procedure SetInternalCode(AValue: string);
    procedure SetMappedKeyCode(AValue: Word);
  public
    constructor Create(AOwner: TComponent; AInternalCode: string); reintroduce;
  published
    property InternalCode: string read FInternalCode;
    property MappedKeyCode: Word read FMappedKeyCode write SetMappedKeyCode;
  end;

  TVirtualKeyList = specialize TFPGmap<string, TVirtualKey>;

  TVirtualKeyBlock = class;

  { IVirtualKeyBlockLayout }

  IVirtualKeyBlockLayout = interface
    ['{EF269299-1BA5-4670-92C1-4C4B48D8B1D1}']
    procedure ArrangeKeys(ABlock: TVirtualKeyBlock);
  end;

  { TVirtualKeyBlock }

  TVirtualKeyBlock = class(TWinControl)
  private
    FBlockName: string;
    FKeys: TVirtualKeyList;
    FLayout: IVirtualKeyBlockLayout;
    FMarginBottom: Integer;
    FMarginLeft: Integer;
    FMarginRight: Integer;
    FMarginTop: Integer;
    procedure SetBlockName(AValue: string);
    procedure SetMarginBottom(AValue: Integer);
    procedure SetMarginLeft(AValue: Integer);
    procedure SetMarginRight(AValue: Integer);
    procedure SetMarginTop(AValue: Integer);
  public
    constructor Create(AOwner: TComponent; const ABlockName: string;
      const ALayout: IVirtualKeyBlockLayout); reintroduce;
    destructor Destroy; override;
    procedure AddKey(AKey: TVirtualKey);
    procedure AddKey(const AKey: string);
    function KeyCount: Integer;
    function FindKeyByInternalCode(const AInternalCode: string;
      out AIndex: Integer): Boolean;
    procedure ArrangeKeys;
    property BlockName: string read FBlockName;
    property Keys: TVirtualKeyList read FKeys;
    property MarginTop: Integer read FMarginTop write SetMarginTop;
    property MarginLeft: Integer read FMarginLeft write SetMarginLeft;
    property MarginBottom: Integer read FMarginBottom write SetMarginBottom;
    property MarginRight: Integer read FMarginRight write SetMarginRight;
  end;

  TVirtualKeyBlockList = specialize TFPGmap<string, TVirtualKeyBlock>;

  { TVirtualKeyboardPage }

  TVirtualKeyboardPage = class(TWinControl)
  private
    FBlocks: TVirtualKeyBlockList;
    FPageName: string;
  public
    constructor Create(AOwner: TComponent; const APageName: string); reintroduce;
    destructor Destroy; override;
    procedure AddBlock(const ABlock: TVirtualKeyBlock);
    function CountBlocks: Integer;
    function FindBlockByName(const AName: string; out AIndex: Integer): Boolean;
    property PageName: string read FPageName;
  end;

  TVirtualKeyboardPageList = specialize TFPGmap<string, TVirtualKeyboardPage>;

  { TVirtualKeyboard }

  TVirtualKeyboard = class(TWinControl)
  private
    FPages: TVirtualKeyboardPageList;
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
    procedure AddPage(const APage: TVirtualKeyboardPage);
    function CountPages: Integer;
    function FindPageByName(const AName: string; out AIndex: Integer): Boolean;
  end;

implementation

uses
  UVirtualKeyRegistry, Graphics;

{ TVirtualKey }

procedure TVirtualKey.SetInternalCode(AValue: string);
begin
  if FInternalCode = AValue then
    Exit;
  FInternalCode := AValue;
end;

procedure TVirtualKey.SetMappedKeyCode(AValue: Word);
begin
  if FMappedKeyCode = AValue then
    Exit;
  FMappedKeyCode := AValue;
end;

constructor TVirtualKey.Create(AOwner: TComponent; AInternalCode: string);
var
  Def: TVirtualKeyDef;
begin
  inherited Create(AOwner);
  Def := FindKeyDef(AInternalCode);

  Width := 50;
  Height :=50;

  FInternalCode := Def.InternalCode;
  Caption := Def.Caption;
  FMappedKeyCode := Def.MappedKeyCode;
end;

{ TVirtualKeyBlock }

procedure TVirtualKeyBlock.SetBlockName(AValue: string);
begin
  if FBlockName=AValue then Exit;
  FBlockName:=AValue;
end;

procedure TVirtualKeyBlock.SetMarginBottom(AValue: Integer);
begin
  if FMarginBottom=AValue then Exit;
  FMarginBottom:=AValue;
end;

procedure TVirtualKeyBlock.SetMarginLeft(AValue: Integer);
begin
  if FMarginLeft=AValue then Exit;
  FMarginLeft:=AValue;
end;

procedure TVirtualKeyBlock.SetMarginRight(AValue: Integer);
begin
  if FMarginRight=AValue then Exit;
  FMarginRight:=AValue;
end;

procedure TVirtualKeyBlock.SetMarginTop(AValue: Integer);
begin
  if FMarginTop=AValue then Exit;
  FMarginTop:=AValue;
end;

constructor TVirtualKeyBlock.Create(AOwner: TComponent;
  const ABlockName: string; const ALayout: IVirtualKeyBlockLayout);
begin
  inherited Create(AOwner);
  Self.Width := 500;
  Self.Height := 500;

  Self.Color := clYellow;

  FLayout := ALayout;
  FBlockName := ABlockName;
  FKeys := TVirtualKeyList.Create;
  FKeys.Sorted := True;
end;

destructor TVirtualKeyBlock.Destroy;
begin
  FKeys.Free;
  inherited Destroy;
end;

procedure TVirtualKeyBlock.AddKey(AKey: TVirtualKey);
begin
  AKey.Parent := Self;
  FKeys.Add(AKey.InternalCode, AKey);
end;

procedure TVirtualKeyBlock.AddKey(const AKey: string);
var
  Key: TVirtualKey;
begin
  Key := TVirtualKey.Create(Self, AKey);
end;

function TVirtualKeyBlock.KeyCount: Integer;
begin
  Result := Keys.Count;
end;

function TVirtualKeyBlock.FindKeyByInternalCode(const AInternalCode: string;
  out AIndex: Integer): Boolean;
begin
  Result := Keys.Find(AInternalCode, AIndex);
end;

procedure TVirtualKeyBlock.ArrangeKeys;
begin
  if Assigned(FLayout) then
    FLayout.ArrangeKeys(Self);
end;

{ TVirtualKeyboardPage }

constructor TVirtualKeyboardPage.Create(AOwner: TComponent; const APageName: string);
begin
  inherited Create(AOwner);
  FPageName := APageName;
  FBlocks := TVirtualKeyBlockList.Create;
  FBlocks.Sorted := True;
end;

destructor TVirtualKeyboardPage.Destroy;
begin
  FBlocks.Free;
  inherited Destroy;
end;

procedure TVirtualKeyboardPage.AddBlock(const ABlock: TVirtualKeyBlock);
begin
  FBlocks.Add(ABlock.BlockName, ABlock);
end;

function TVirtualKeyboardPage.CountBlocks: Integer;
begin
  Result := FBlocks.Count;
end;

function TVirtualKeyboardPage.FindBlockByName(const AName: string; out
  AIndex: Integer): Boolean;
begin
  Result := FBlocks.Find(AName, AIndex);
end;

{ TVirtualKeyboard }

constructor TVirtualKeyboard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPages := TVirtualKeyboardPageList.Create;
  FPages.Sorted := True;
end;

destructor TVirtualKeyboard.Destroy;
begin
  FPages.Free;
  inherited Destroy;
end;

procedure TVirtualKeyboard.AddPage(const APage: TVirtualKeyboardPage);
begin
  FPages.Add(APage.PageName, APage);
end;

function TVirtualKeyboard.CountPages: Integer;
begin
  Result := FPages.Count;
end;

function TVirtualKeyboard.FindPageByName(const AName: string; out
  AIndex: Integer): Boolean;
begin
  Result := FPages.Find(AName, AIndex);
end;

end.

