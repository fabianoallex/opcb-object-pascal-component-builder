unit HtmlBuilder;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  { TCSSBuilder }

  TCSSBuilder = class
    private
      FSelector: string;
      FSelectors: TStringList; // armazenará .class { prop: val; }
    public
      constructor Create;
      destructor Destroy; override;
      procedure AddRule(const ASelector, AProperty, AValue: string);
      function GetCSS: string;
      function UsingRule(const ASelector: string): TCSSBuilder;
      function Add(AProperty, AValue: string): TCSSBuilder;
    end;

  { THTMLAttributeList }

  THTMLAttributeList = class
  private
    FData: TStringList;
    function GetAsString: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetAttribute(const AName, AValue: string);
    procedure AddClass(const AClass: string);
    procedure AddStyle(const AStyleName, AValue: string);
    procedure AppendStyle(const AStyle: string);
    property AsString: string read GetAsString;
  end;

  { THTMLElement }

  THTMLElement = class
  private
    FTag: string;
    FAttributes: THTMLAttributeList;
    FChildren: TList;
    FText: string;
  public
    constructor Create(const ATag: string);
    destructor Destroy; override;

    procedure AddChild(AChild: THTMLElement);
    function CreateChild(const ATag: string): THTMLElement;
    procedure SetText(const AText: string);

    function Render: string;

    property Attributes: THTMLAttributeList read FAttributes;
  end;

implementation

uses
  strutils;

{ TCSSBuilder }

constructor TCSSBuilder.Create;
begin
  FSelectors := TStringList.Create;
  FSelectors.CaseSensitive := False;
end;

destructor TCSSBuilder.Destroy;
begin
  FSelectors.Free;
  inherited Destroy;
end;

procedure TCSSBuilder.AddRule(const ASelector, AProperty, AValue: string);
var
  Index: Integer;
  ExistingBlock, NewBlock: string;
begin
  Index := FSelectors.IndexOfName(ASelector);
  if Index = -1 then
    FSelectors.Values[ASelector] := Format('%s:%s;', [AProperty, AValue])
  else
  begin
    ExistingBlock := FSelectors.ValueFromIndex[Index];
    NewBlock := ExistingBlock + Format('%s:%s;', [AProperty, AValue]);
    FSelectors.ValueFromIndex[Index] := NewBlock;
  end;
end;

function TCSSBuilder.GetCSS: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to FSelectors.Count - 1 do
    Result := Result + Format('%s {%s}', [FSelectors.Names[I], FSelectors.ValueFromIndex[I]]);
end;

function TCSSBuilder.UsingRule(const ASelector: string): TCSSBuilder;
begin
  Result := Self;
  FSelector := ASelector;
end;

function TCSSBuilder.Add(AProperty, AValue: string): TCSSBuilder;
begin
  Result := Self;
  AddRule(FSelector, AProperty, AValue);
end;

{ THTMLAttributeList }

function THTMLAttributeList.GetAsString: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to FData.Count - 1 do
    Result := Result + Format(' %s="%s"', [FData.Names[I], FData.ValueFromIndex[I]]);
end;

constructor THTMLAttributeList.Create;
begin
  FData := TStringList.Create;
  FData.NameValueSeparator := '=';
  FData.CaseSensitive := False;
end;

destructor THTMLAttributeList.Destroy;
begin
  FData.Free;
  inherited Destroy;
end;

procedure THTMLAttributeList.SetAttribute(const AName, AValue: string);
begin
  FData.Values[AName] := AValue;
end;

procedure THTMLAttributeList.AddClass(const AClass: string);
var
  Existing: string;
begin
  Existing := FData.Values['class'];
  if Existing = '' then
    FData.Values['class'] := AClass
  else if not AnsiContainsText(Existing, AClass) then
    FData.Values['class'] := Existing + ' ' + AClass;
end;

procedure THTMLAttributeList.AddStyle(const AStyleName, AValue: string);
var
  ExistingStyle, NewStyle: string;
begin
  ExistingStyle := FData.Values['style'];

  if (ExistingStyle <> '') and (ExistingStyle[Length(ExistingStyle)] <> ';') then
    ExistingStyle := ExistingStyle + ';';

  NewStyle := Format('%s:%s;', [AStyleName, AValue]);
  FData.Values['style'] := ExistingStyle + NewStyle;
end;

procedure THTMLAttributeList.AppendStyle(const AStyle: string);
var
  Existing: string;
begin
  Existing := FData.Values['style'];
  if Existing = '' then
    FData.Values['style'] := AStyle
  else
    FData.Values['style'] := Existing + ';' + AStyle;
end;

{ THTMLElement }

constructor THTMLElement.Create(const ATag: string);
begin
  FTag := ATag;
  FAttributes := THTMLAttributeList.Create;
  FChildren := TList.Create;
end;

destructor THTMLElement.Destroy;
var
  I: Integer;
begin
  for I := 0 to FChildren.Count - 1 do
    TObject(FChildren[I]).Free;
  FChildren.Free;
  FAttributes.Free;
  inherited Destroy;
end;

procedure THTMLElement.AddChild(AChild: THTMLElement);
begin
  FChildren.Add(AChild);
end;

function THTMLElement.CreateChild(const ATag: string): THTMLElement;
begin
  Result := THTMLElement.Create(ATag);
  AddChild(Result);
end;

procedure THTMLElement.SetText(const AText: string);
begin
  FText := AText;
end;

function THTMLElement.Render: string;
var
  I: Integer;
  ChildHTML: string;
begin
  ChildHTML := '';
  for I := 0 to FChildren.Count - 1 do
    ChildHTML := ChildHTML + THTMLElement(FChildren[I]).Render;

  // Se não há conteúdo nem filhos, gera tag auto-fechada
  if (Trim(FText) = '') and (Trim(ChildHTML) = '') then
    Result := Format(
      '<%s%s />',
      [FTag, FAttributes.AsString]
    )
  else
    Result := Format(
      '<%s%s>%s%s</%s>',
      [FTag, FAttributes.AsString, FText, ChildHTML, FTag]
    );
end;

end.

