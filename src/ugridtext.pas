unit UGridText;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ULayout;

type

  { TCharMatrix }

  TCharMatrix = class
  private
    FBuffer: array of array of Char;
    FEmptyChar: Char;
    FWidth, FHeight: Integer;
    procedure SetEmptyChar(AValue: Char);
  public
    constructor Create(AWidth, AHeight: Integer);
    procedure Clear;
    procedure WriteTextAt(x, y: Integer; const AText: string);
    procedure WriteCharAt(x, y: Integer; ch: Char);
    function GetAsString: string;
    property EmptyChar: Char read FEmptyChar write SetEmptyChar;
    property Height: Integer read FHeight;
    property Width: Integer read FWidth;
  end;

  { TTextGridRenderer }

  TTextGridRenderer = class
  private
    FGrid: TGridLayout;
    FCharMatrix: TCharMatrix;
    FHorizontaSpancingChar: Char;
    FIntersectionChar: Char;
    FMarginBottomChar: Char;
    FMarginLeftChar: Char;
    FMarginRightChar: Char;
    FMarginTopChar: Char;
    FVerticalSpacingChar: Char;
    procedure SetHorizontaSpancingChar(AValue: Char);
    procedure SetIntersectionSpacingChar(AValue: Char);
    procedure SetMarginBottomChar(AValue: Char);
    procedure SetMarginLeftChar(AValue: Char);
    procedure SetMarginRightChar(AValue: Char);
    procedure SetMarginTopChar(AValue: Char);
    procedure SetVerticalSpacingChar(AValue: Char);
  public
    constructor Create(AGrid: TGridLayout);
    destructor Destroy; override;
    procedure WriteTextAt(x, y: Integer; const AText: string);
    function GetAsString: string;
    procedure Clear;
    property MarginTopChar: Char read FMarginTopChar write SetMarginTopChar;
    property MarginBottomChar: Char read FMarginBottomChar write SetMarginBottomChar;
    property MarginLeftChar: Char read FMarginLeftChar write SetMarginLeftChar;
    property MarginRightChar: Char read FMarginRightChar write SetMarginRightChar;
    property VerticalSpacingChar: Char read FVerticalSpacingChar write SetVerticalSpacingChar;
    property HorizontaSpancingChar: Char read FHorizontaSpancingChar write SetHorizontaSpancingChar;
    property IntersectionChar: Char read FIntersectionChar write SetIntersectionSpacingChar;
  end;

  TTextAlignHorizontal = (tahLeft, tahCenter, tahRight);
  TTextAlignVertical = (tavTop, tavMiddle, tavBottom);

  { TTextVisualElement }

  TTextVisualElement = class(TInterfacedObject, IVisualElement)
    FRenderer: TTextGridRenderer;
    FLines: TStringList;
    FLeft, FTop, FWidth, FHeight: Integer;
    FVisible: Boolean;
  private
    FHorizontalAlign: TTextAlignHorizontal;
    FVerticalAlign: TTextAlignVertical;
    procedure AdjustSize;
    function GetAlignedLines: specialize TArray<string>;
  public
    constructor Create(const ARenderer: TTextGridRenderer);
    destructor Destroy; override;
    procedure Redraw(AContext: TGriItemRenderContext);
    function GetHeight: Integer;
    function GetLeft: Integer;
    function GetTop: Integer;
    function GetVisible: Boolean;
    function GetWidth: Integer;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
    procedure SetHeight(AValue: Integer);
    procedure SetVisible(AValue: Boolean);
    procedure SetWidth(AValue: Integer);
    procedure SetText(const AText: string);
    function GetTextLines: TStringList;
    property HorizontalAlign: TTextAlignHorizontal read FHorizontalAlign write FHorizontalAlign;
    property VerticalAlign: TTextAlignVertical read FVerticalAlign write FVerticalAlign;
    property Renderer: TTextGridRenderer read FRenderer;
  end;

  { TTextGridItem }

  TTextGridItem = class(TInterfacedObject, IGridItem)
  protected
    FGridRenderer: TTextGridRenderer;
    FElement: IVisualElement;
    procedure AfterSetBounds; virtual;
  public
    constructor Create(AElement: TTextVisualElement);
    function GetVisualElement: IVisualElement;
    function GetRenderer: IGridItemRenderer;
  end;

  { TTextGridItemRenderer }

  TTextGridItemRenderer = class(TInterfacedObject, IGridItemRenderer)
  private
    FGridRenderer: TTextGridRenderer;
    FGridItem: TTextGridItem;
  public
    constructor Create(AGridRenderer: TTextGridRenderer; AGridItem: TTextGridItem);
    procedure RenderTo(AContext: TGriItemRenderContext);
  end;

implementation

{ TCharMatrix }

procedure TCharMatrix.SetEmptyChar(AValue: Char);
begin
  if FEmptyChar = AValue then Exit;
  FEmptyChar := AValue;
end;

constructor TCharMatrix.Create(AWidth, AHeight: Integer);
begin
  inherited Create;
  FEmptyChar := ' ';
  FWidth := AWidth;
  FHeight := AHeight;
  SetLength(FBuffer, FHeight, FWidth);
  Clear;
end;

procedure TCharMatrix.Clear;
var
  y, x: Integer;
begin
  for y := 0 to FHeight - 1 do
    for x := 0 to FWidth - 1 do
      FBuffer[y][x] := EmptyChar;
end;

procedure TCharMatrix.WriteTextAt(x, y: Integer; const AText: string);
var
  i: Integer;
begin
  for i := 1 to Length(AText) do
    WriteCharAt(x + i - 1, y, AText[i]);
end;

procedure TCharMatrix.WriteCharAt(x, y: Integer; ch: Char);
begin
  if (x >= 0) and (x < FWidth) and (y >= 0) and (y < FHeight) then
    FBuffer[y][x] := ch;
end;

function TCharMatrix.GetAsString: string;
var
  y, x: Integer;
begin
  Result := '';
  for y := 0 to FHeight - 1 do
  begin
    for x := 0 to FWidth - 1 do
      Result := Result + string(FBuffer[y][x]);
    Result := Result + sLineBreak;
  end;
end;

{ TTextGridRenderer }

procedure TTextGridRenderer.SetHorizontaSpancingChar(AValue: Char);
begin
  if FHorizontaSpancingChar = AValue then Exit;
  FHorizontaSpancingChar := AValue;
end;

procedure TTextGridRenderer.SetIntersectionSpacingChar(AValue: Char);
begin
  if FIntersectionChar = AValue then Exit;
  FIntersectionChar := AValue;
end;

procedure TTextGridRenderer.SetMarginBottomChar(AValue: Char);
begin
  if FMarginBottomChar = AValue then Exit;
  FMarginBottomChar := AValue;
end;

procedure TTextGridRenderer.SetMarginLeftChar(AValue: Char);
begin
  if FMarginLeftChar = AValue then Exit;
  FMarginLeftChar := AValue;
end;

procedure TTextGridRenderer.SetMarginRightChar(AValue: Char);
begin
  if FMarginRightChar = AValue then Exit;
  FMarginRightChar := AValue;
end;

procedure TTextGridRenderer.SetMarginTopChar(AValue: Char);
begin
  if FMarginTopChar = AValue then Exit;
  FMarginTopChar := AValue;
end;

procedure TTextGridRenderer.SetVerticalSpacingChar(AValue: Char);
begin
  if FVerticalSpacingChar = AValue then Exit;
  FVerticalSpacingChar := AValue;
end;

constructor TTextGridRenderer.Create(AGrid: TGridLayout);
begin
  inherited Create;
  FGrid := AGrid;
  FCharMatrix := TCharMatrix.Create(AGrid.ContentWidth, AGrid.ContentHeight);
  FCharMatrix.FEmptyChar := ' ';
  MarginTopChar := '-';
  MarginBottomChar := '-';
  MarginLeftChar := '|';
  MarginRightChar := '|';
  VerticalSpacingChar := '-';
  HorizontaSpancingChar := '|';
  IntersectionChar := '+';
  Clear;
end;

destructor TTextGridRenderer.Destroy;
begin
  FCharMatrix.Free;
  inherited Destroy;
end;

procedure TTextGridRenderer.WriteTextAt(x, y: Integer; const AText: string);
begin
  FCharMatrix.WriteTextAt(x, y, AText);
end;

function TTextGridRenderer.GetAsString: string;
begin
  Result := FCharMatrix.GetAsString;
end;

procedure TTextGridRenderer.Clear;
var
  X, Y: Integer;
  IsVerticalSpacing: Boolean;
  IsHorizontalSpacing: Boolean;
  IsTopMargin: Boolean;
  IsRightMargin: Boolean;
  IsBottomMargin: Boolean;
  IsLeftMargin: Boolean;

  function IsIntersection: Boolean;
  var
    Count: Integer;
  begin
    Count := 0;
    if IsVerticalSpacing then Inc(Count);
    if IsHorizontalSpacing then Inc(Count);
    if IsTopMargin then Inc(Count);
    if IsBottomMargin then Inc(Count);
    if IsLeftMargin then Inc(Count);
    if IsRightMargin then Inc(Count);

    Result := Count >= 2;
  end;

begin
  FCharMatrix.Clear;

  for X:=0 to FCharMatrix.FWidth-1 do
    for Y:=0 to FCharMatrix.FHeight-1 do
    begin
      IsVerticalSpacing := FGrid.IsInVerticalSpacing(X, Y);
      IsHorizontalSpacing := FGrid.IsInHorizontalSpacing(X, Y);
      IsTopMargin := FGrid.IsInTopMargin(Y);
      IsBottomMargin := FGrid.IsInBottomMargin(Y);
      IsLeftMargin := FGrid.IsInLeftMargin(X);
      IsRightMargin := FGrid.IsInRightMargin(X);

      if IsIntersection then
        FCharMatrix.WriteCharAt(X, Y, FIntersectionChar)
      else if IsVerticalSpacing then
        FCharMatrix.WriteCharAt(X, Y, FVerticalSpacingChar)
      else if IsHorizontalSpacing then
        FCharMatrix.WriteCharAt(X, Y, FHorizontaSpancingChar)
      else if IsTopMargin then
        FCharMatrix.WriteCharAt(X, Y, FMarginTopChar)
      else if IsBottomMargin then
        FCharMatrix.WriteCharAt(X, Y, FMarginBottomChar)
      else if IsLeftMargin then
        FCharMatrix.WriteCharAt(X, Y, FMarginLeftChar)
      else if IsRightMargin then
        FCharMatrix.WriteCharAt(X, Y, FMarginRightChar);
    end;
end;

{ TTextVisualElement }

function TTextVisualElement.GetAlignedLines: specialize TArray<string>;
var
  I, PadTop, LineIndex: Integer;
  Line: string;
  LeftPad, RightPad: Integer;
begin
  SetLength(Result, FHeight);

  // Calcular padding superior (vertical alignment)
  case FVerticalAlign of
    tavTop: PadTop := 0;
    tavMiddle: PadTop := (FHeight - FLines.Count) div 2;
    tavBottom: PadTop := FHeight - FLines.Count;
  else
    PadTop := 0;
  end;

  // Preenche com linhas em branco
  for I := 0 to FHeight - 1 do
    Result[I] := StringOfChar(' ', FWidth);

  // Aplica alinhamento horizontal em cada linha
  for I := 0 to FLines.Count - 1 do
  begin
    if PadTop + I >= FHeight then
      Break;

    Line := FLines[I];
    case FHorizontalAlign of
      tahLeft:
        Line := Line + StringOfChar(' ', FWidth - Length(Line));
      tahCenter:
        begin
          LeftPad := (FWidth - Length(Line)) div 2;
          RightPad := FWidth - Length(Line) - LeftPad;
          Line := StringOfChar(' ', LeftPad) + Line + StringOfChar(' ', RightPad);
        end;
      tahRight:
        Line := StringOfChar(' ', FWidth - Length(Line)) + Line;
    end;

    Result[PadTop + I] := Copy(Line, 1, FWidth); // Garante corte se necessário
  end;
end;

procedure TTextVisualElement.Redraw(AContext: TGriItemRenderContext);
var
  I: Integer;
  Str: string;
  AlignedLines: specialize TArray<string>;
begin
  if (not Assigned(FRenderer)) or (not FVisible) then
    Exit;

  AlignedLines := GetAlignedLines;
  for I := 0 to High(AlignedLines) do
    FRenderer.WriteTextAt(FLeft, FTop + I, AlignedLines[I]);
end;

constructor TTextVisualElement.Create(const ARenderer: TTextGridRenderer);
begin
  inherited Create;
  FRenderer := ARenderer;
  FLines := TStringList.Create;
  FVisible := True;
end;

destructor TTextVisualElement.Destroy;
begin
  FLines.Free;
  inherited Destroy;
end;

function TTextVisualElement.GetHeight: Integer;
begin
  Result := FHeight;
end;

function TTextVisualElement.GetLeft: Integer;
begin
  Result := FLeft;
end;

function TTextVisualElement.GetTop: Integer;
begin
  Result := FTop;
end;

function TTextVisualElement.GetVisible: Boolean;
begin
  Result := FVisible;
end;

function TTextVisualElement.GetWidth: Integer;
begin
  Result := FWidth;
end;

procedure TTextVisualElement.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  FLeft := ALeft;
  FTop := ATop;
  FWidth := AWidth;
  FHeight := AHeight;
end;

procedure TTextVisualElement.SetHeight(AValue: Integer);
begin
  FHeight := AValue;
end;

procedure TTextVisualElement.SetVisible(AValue: Boolean);
begin
  FVisible := AValue;
end;

procedure TTextVisualElement.SetWidth(AValue: Integer);
begin
  FWidth := AValue;
end;

procedure TTextVisualElement.AdjustSize;
var
  I, MaxLineLength: Integer;
begin
  MaxLineLength := 0;
  for I := 0 to FLines.Count - 1 do
    if Length(FLines[I]) > MaxLineLength then
      MaxLineLength := Length(FLines[I]);

  FWidth := MaxLineLength;
  FHeight := FLines.Count;
end;

procedure TTextVisualElement.SetText(const AText: string);
begin
  FLines.Text := AText;
  AdjustSize;
end;

function TTextVisualElement.GetTextLines: TStringList;
begin
  Result := FLines;
end;

{ TTextGridItem }

procedure TTextGridItem.AfterSetBounds;
begin
  // nessa classe não faz nada
end;

constructor TTextGridItem.Create(AElement: TTextVisualElement);
begin
  inherited Create;
  FGridRenderer := AElement.Renderer;
  FElement := AElement;
end;

function TTextGridItem.GetVisualElement: IVisualElement;
begin
  Result := FElement;
end;

function TTextGridItem.GetRenderer: IGridItemRenderer;
begin
  Result := TTextGridItemRenderer.Create(Self.FGridRenderer, Self);
end;

{ TTextGridItemRenderer }

constructor TTextGridItemRenderer.Create(AGridRenderer: TTextGridRenderer;
  AGridItem: TTextGridItem);
begin
  FGridRenderer := AGridRenderer;
  FGridItem := AGridItem;
end;

procedure TTextGridItemRenderer.RenderTo(AContext: TGriItemRenderContext);
begin
  FGridItem.GetVisualElement.SetBounds(
    AContext.Left,
    AContext.Top,
    AContext.Width,
    AContext.Height
  );

  FGridItem.GetVisualElement.Redraw(AContext);
end;

end.

