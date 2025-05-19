unit UGridHtmlTable;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ULayout;

type

  { TGridHtmlTableRenderer }

  TGridHtmlTableRenderer = class
  private
    FGrid: TGridLayout;
  public
    constructor Create(AGrid: TGridLayout);
    destructor Destroy; override;
    function GetAsString: string;
  end;

  { TTextVisualElement }

  { THtmlTableVisualElement }

  THtmlTableVisualElement = class(TInterfacedObject, IVisualElement)
  private
    FRenderer: TGridHtmlTableRenderer;
    FLeft, FTop, FWidth, FHeight: Integer;
    FStrContent: string;
    FVisible: Boolean;
    procedure Redraw;
    procedure SetStrContent(AValue: string);
  public
    constructor Create(const ARenderer: TGridHtmlTableRenderer);
    destructor Destroy; override;
    function GetHeight: Integer;
    function GetLeft: Integer;
    function GetTop: Integer;
    function GetVisible: Boolean;
    function GetWidth: Integer;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
    procedure SetHeight(AValue: Integer);
    procedure SetVisible(AValue: Boolean);
    procedure SetWidth(AValue: Integer);
    property StrContent: string read FStrContent write SetStrContent;
  end;

  { THtmlTableGridItem }

  THtmlTableGridItem = class(TInterfacedObject, IGridItem)
  protected
    FElement: IVisualElement;
    procedure AfterSetBounds; virtual;
  public
    constructor Create(AElement: THtmlTableVisualElement);
    function GetElement: IVisualElement;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
  end;

implementation

{ TGridHtmlTableRenderer }

constructor TGridHtmlTableRenderer.Create(AGrid: TGridLayout);
begin
  FGrid := AGrid;
end;

destructor TGridHtmlTableRenderer.Destroy;
begin
  inherited Destroy;
end;

function TGridHtmlTableRenderer.GetAsString: string;
var
  Row, Col: Integer;
  Cell: TGridCell;
  Item: IGridItem;
  StrRowSpan, StrColSpan: string;

  function CalcColumnWidth(AColumn: Integer): Integer;
  begin
    Result := FGrid.ColumnWidth[AColumn];
    if AColumn = 0 then
      Result := Result + FGrid.Margins.Left;

    if AColumn = FGrid.Columns-1 then
      Result := Result + FGrid.Margins.Right
    else
      Result := Result + FGrid.HorizontalSpacing[AColumn];
  end;

  function CalcRowHeight(ARow: Integer): Integer;
  begin
    Result := FGrid.RowHeight[ARow];
    if ARow = 0 then
      Result := Result + FGrid.Margins.Top;

    if ARow = FGrid.Rows - 1 then
      Result := Result + FGrid.Margins.Bottom
    else
      Result := Result + FGrid.VerticalSpacing[ARow];
  end;

  function ColGroup: string;
  var
    C: Integer;
    W: Integer;
  begin
    Result := '  <colgroup>' + sLineBreak;

    for C:=0 to FGrid.Columns-1 do
      Result := Result
        + Format('    <col width="%dpx"/>', [CalcColumnWidth(C)])
        + sLineBreak;

    Result := Result + '  </colgroup>';
  end;

  function CreateStyle(AItens: array of string): string;
  var
    I: Integer;
    Comma: string;
  begin
    Result := ' style="';
    Comma := '';
    for I:= Low(AItens) to High(AItens) do
    begin
      if Trim(AItens[I]) = EmptyStr then
        Continue;

      Result := Result + Comma + AItens[I];
      Comma := '; ';
    end;
    Result := Result + '"';
  end;

  function HorizontalAligmentToCellStyle(AAlign: TItemAlignment): string;
  const
    AlignMap: array[TItemAlignment] of string = ('left', 'center', 'left', 'right');
  begin
    Result := Format('text-align: %s', [AlignMap[AAlign]]);
  end;

  function VerticalAligmentToCellStyle(AAlign: TItemAlignment): string;
  const
    AlignMap: array[TItemAlignment] of string = ('top', 'middle', 'top', 'bottom');
  begin
    Result := Format('vertical-align: %s', [AlignMap[AAlign]]);
  end;

  function GetTextHorizontalAlignStyle(ARow, ACol: Integer): string;
  var
    Cell: TGridCell;
  begin
    Result := '';
    Cell := FGrid.GetCell(ARow, ACol);

    if not Assigned(Cell) then
      Exit;

    Result := HorizontalAligmentToCellStyle(Cell.HorizontalAlignment);
  end;

  function GetTextVerticalAlignStyle(ARow, ACol: Integer): string;
  var
    Cell: TGridCell;
  begin
    Result := '';
    Cell := FGrid.GetCell(ARow, ACol);

    if not Assigned(Cell) then
      Exit;

    Result := VerticalAligmentToCellStyle(Cell.VerticalAlignment);
  end;

  function GetCellPaddingStyle(ARow, ACol: Integer): string;
  var
    PadTop, PadRight, PadBottom, PadLeft: Integer;
    HCol, VRow: Integer;
  begin
    PadTop := 0;
    PadRight := 0;
    PadBottom := 0;
    PadLeft := 0;

    // Margens do grid (aplicadas apenas nas bordas)
    if ARow = 0 then
      PadTop := FGrid.Margins.Top;
    if ACol = 0 then
      PadLeft := FGrid.Margins.Left;
    if ARow = FGrid.Rows - 1 then
      PadBottom := FGrid.Margins.Bottom;
    if ACol = FGrid.Columns - 1 then
      PadRight := FGrid.Margins.Right;

    HCol := ACol;
    VRow := ARow;

    if Assigned(FGrid.GetCell(ARow, ACol)) then
    begin
      HCol := ACol + FGrid.GetCell(ARow, ACol).ColSpan - 1;
      VRow := ARow + FGrid.GetCell(ARow, ACol).RowSpan - 1;
    end;

    // Espaçamento entre células (aplicado na parte inferior/direita das células internas)
    if (HCol < FGrid.Columns - 1) then
      PadRight := PadRight + FGrid.HorizontalSpacing[HCol];
    if VRow < FGrid.Rows - 1 then
      PadBottom := PadBottom + FGrid.VerticalSpacing[VRow];

    Result := Format('padding: %dpx %dpx %dpx %dpx',
      [PadTop, PadRight, PadBottom, PadLeft]);
  end;

begin
  Result :=
    '<style>' + sLineBreak
      + 'td {' + sLineBreak
      + '  padding: 35px;' + sLineBreak
      + '  background-image: linear-gradient(to bottom, rgba(240, 255, 40, 1) 0%, rgba(240, 255, 40, 1) 100%), linear-gradient(to bottom, rgba(240, 40, 40, 1) 0%, rgba(240, 40, 40, 1) 100%);' + sLineBreak
      + '  background-clip: content-box, padding-box;' + sLineBreak
      + '}' + sLineBreak
      + '</style>' + sLineBreak +

    '<table style="border: 0px">'
    + sLineBreak
    + ColGroup
    + sLineBreak;

  for Row := 0 to FGrid.Rows - 1 do
  begin
    Result := Result
      + Format('  <tr height="%dpx">', [CalcRowHeight(Row)])
      + sLineBreak;

    for Col := 0 to FGrid.Columns - 1 do
    begin
      Cell := FGrid.GetCell(Row, Col);

      if not Assigned(Cell) then
      begin
        if not FGrid.IsCellSpan(Row, Col) then
           Result := Result
             + Format(
               '    <td%s></td>',
               [
                 CreateStyle([
                   GetCellPaddingStyle(Row, Col)
                 ])
               ])
             + sLineBreak;
        Continue;
      end;

      if (not Assigned(Cell.Item)) or (not Assigned(Cell.Item.GetElement))  then
        Continue;

      StrRowSpan := '';
      StrColSpan := '';

      if Cell.RowSpan > 1 then
        StrRowSpan := Format(' rowspan="%d" ', [Cell.RowSpan]);
      if Cell.ColSpan > 1 then
        StrColSpan := Format(' colspan="%d" ', [Cell.ColSpan]);

      Item := Cell.Item;

      Result := Result +
        Format(
          '    <td%s%s%s>%s</td>%s',
          [
            StrRowSpan,
            StrColSpan,
            CreateStyle([
              GetCellPaddingStyle(Row, Col),
              GetTextHorizontalAlignStyle(Row, Col),
              GetTextVerticalAlignStyle(Row, Col)
            ]),
            (Item.GetElement as THtmlTableVisualElement).StrContent,
            sLineBreak
          ]
        );
    end;

    Result := Result + '  </tr>' + sLineBreak;
  end;

  Result := Result + '</table>' + sLineBreak;
end;


{ TTextVisualElement }

procedure THtmlTableVisualElement.Redraw;
begin

end;

procedure THtmlTableVisualElement.SetStrContent(AValue: string);
begin
  if FStrContent = AValue then Exit;
  FStrContent := AValue;
end;

constructor THtmlTableVisualElement.Create(const ARenderer: TGridHtmlTableRenderer);
begin
  FRenderer := ARenderer;
end;

destructor THtmlTableVisualElement.Destroy;
begin
  inherited Destroy;
end;

function THtmlTableVisualElement.GetHeight: Integer;
begin
  Result := FHeight;
end;

function THtmlTableVisualElement.GetLeft: Integer;
begin
  Result := FLeft;
end;

function THtmlTableVisualElement.GetTop: Integer;
begin
  Result := FTop;
end;

function THtmlTableVisualElement.GetVisible: Boolean;
begin
  Result := FVisible;
end;

function THtmlTableVisualElement.GetWidth: Integer;
begin
  Result := FWidth;
end;

procedure THtmlTableVisualElement.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  FLeft := ALeft;
  FTop := ATop;
  FWidth := AWidth;
  FHeight := AHeight;
end;

procedure THtmlTableVisualElement.SetHeight(AValue: Integer);
begin
  FHeight := AValue;
end;

procedure THtmlTableVisualElement.SetVisible(AValue: Boolean);
begin
  FVisible := AValue;
end;

procedure THtmlTableVisualElement.SetWidth(AValue: Integer);
begin
  FWidth := AValue;
end;

{ THtmlTableGridItem }

procedure THtmlTableGridItem.AfterSetBounds;
begin
  // nessa classe não faz nada
end;

constructor THtmlTableGridItem.Create(AElement: THtmlTableVisualElement);
begin
  inherited Create;
  FElement := AElement;
end;

function THtmlTableGridItem.GetElement: IVisualElement;
begin
  Result := FElement;
end;

procedure THtmlTableGridItem.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  FElement.SetBounds(ALeft, ATop, AWidth, AHeight);
  AfterSetBounds;
end;

end.

