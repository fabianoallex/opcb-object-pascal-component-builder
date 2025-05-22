unit UGridHtml;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ULayout;

type

  TPadding = record
    Top: Integer;
    Right: Integer;
    Bottom: Integer;
    Left: Integer;
  end;

  { IHtmlGridRenderer }

  IHtmlGridRenderer = interface
    function GetAsString: string;
  end;

  THtmlTableGridRenderer = class(TInterfacedObject, IHtmlGridRenderer)
  private
    FGrid: TGridLayout;
  public
    constructor Create(AGrid: TGridLayout);
    destructor Destroy; override;
    function GetAsString: string;
  end;

  { THtmlDivGridRenderer }

  THtmlDivGridRenderer = class(TInterfacedObject, IHtmlGridRenderer)
  private
    FGrid: TGridLayout;
    function GetStyleTag: string;
  public
    constructor Create(AGrid: TGridLayout);
    destructor Destroy; override;
    function GetAsString: string;
  end;

  { THtmlVisualElement }

  THtmlVisualElement = class(TInterfacedObject, IVisualElement)
  private
    FRenderer: IHtmlGridRenderer;
    FLeft, FTop, FWidth, FHeight: Integer;
    FStrContent: string;
    FVisible: Boolean;
    procedure Redraw(AContext: TGriItemRenderContext);
    procedure SetStrContent(AValue: string);
  public
    constructor Create(ARenderer: IHtmlGridRenderer);
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
    property Renderer: IHtmlGridRenderer read FRenderer;
  end;

  { THtmlGridItem }

  THtmlGridItem = class(TInterfacedObject, IGridItem)
  protected
    FGridRenderer: IHtmlGridRenderer;
    FElement: IVisualElement;
    procedure AfterSetBounds; virtual;
  public
    constructor Create(AElement: THtmlVisualElement);
    function GetVisualElement: IVisualElement;
    function GetRenderer: IGridItemRenderer;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
  end;

  { THtmlGridItemRenderer }

  THtmlGridItemRenderer = class(TInterfacedObject, IGridItemRenderer)
  private
    FGridRenderer: IHtmlGridRenderer;
    FGridItem: THtmlGridItem;
  public
    constructor Create(AGridRenderer: IHtmlGridRenderer; AGridItem: THtmlGridItem);
    procedure RenderTo(AContext: TGriItemRenderContext);
  end;

implementation

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

function VerticalAligmentItemsStyle(AAlign: TItemAlignment): string;
const
  AlignMap: array[TItemAlignment] of string = ('start', 'center', 'start', 'end');
begin
  Result := Format('align-items: %s', [AlignMap[AAlign]]);
end;

function GetTextHorizontalAlignStyle(Cell: TGridCell): string;
begin
  Result := '';
  if not Assigned(Cell) then
    Exit;
  Result := HorizontalAligmentToCellStyle(Cell.HorizontalAlignment);
end;

function GetTextVerticalAlignStyle(Cell: TGridCell): string;
begin
  Result := '';
  if not Assigned(Cell) then
    Exit;
  Result := VerticalAligmentToCellStyle(Cell.VerticalAlignment);
end;

function GetTextVerticalAligeItemsStyle(Cell: TGridCell): string;
begin
  Result := '';
  if not Assigned(Cell) then
    Exit;
  Result := VerticalAligmentItemsStyle(Cell.VerticalAlignment);
end;

function CalcPaddingOfCell(AGrid: TGridLayout; ACell: TGridCell): TPadding;
var
  HCol, VRow: Integer;
begin
  With Result do
  begin
    Top := 0;
    Right := 0;
    Bottom := 0;
    Left := 0;

    if not Assigned(ACell) then
      Exit;

    // Margens do grid (aplicadas apenas nas bordas)
    if ACell.Row = 0 then
      Top := AGrid.Margins.Top;

    if ACell.Column = 0 then
      Left := AGrid.Margins.Left;

    if ACell.Row = AGrid.Rows - ACell.RowSpan then
      Bottom := AGrid.Margins.Bottom;

    if ACell.Column = AGrid.Columns - ACell.ColSpan then
      Right := AGrid.Margins.Right;

    HCol := ACell.Column;
    VRow := ACell.Row;

    if Assigned(ACell) then
    begin
      HCol := ACell.Column + ACell.ColSpan - 1;
      VRow := ACell.Row + ACell.RowSpan - 1;
    end;

    // Espaçamento entre células (aplicado na parte inferior/direita das células internas)
    if HCol < AGrid.Columns - 1 then
      Right := Right + AGrid.HorizontalSpacing[HCol];

    if VRow < AGrid.Rows - 1 then
      Bottom := Bottom + AGrid.VerticalSpacing[VRow];
  end;
end;

function GetCellPaddingStyle(AGrid: TGridLayout; ACell: TGridCell): string;
var
  Padding: TPadding;
begin
  Padding := CalcPaddingOfCell(AGrid, ACell);
  Result := Format('padding: %dpx %dpx %dpx %dpx',
    [Padding.Top, Padding.Right, Padding.Bottom, Padding.Left]);
end;

{ IHtmlGridRenderer }

constructor THtmlTableGridRenderer.Create(AGrid: TGridLayout);
begin
  FGrid := AGrid;
end;

destructor THtmlTableGridRenderer.Destroy;
begin
  inherited Destroy;
end;

function THtmlTableGridRenderer.GetAsString: string;
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
  begin
    Result := '  <colgroup>' + sLineBreak;

    for C:=0 to FGrid.Columns-1 do
      Result := Result
        + Format('    <col width="%dpx"/>', [CalcColumnWidth(C)])
        + sLineBreak;

    Result := Result + '  </colgroup>';
  end;

  function CreateStyle(AItems: array of string): string;
  var
    I: Integer;
    Semicolon: string;
  begin
    Result := ' style="';
    Semicolon := '';
    for I:= Low(AItems) to High(AItems) do
    begin
      if Trim(AItems[I]) = EmptyStr then
        Continue;

      Result := Result + Semicolon + AItems[I];
      Semicolon := '; ';
    end;
    Result := Result + '"';
  end;



begin
  Result :=
    // o style é temporário, apenas para facilitar visualização durante testes
    '<style>' + sLineBreak
      + 'td {' + sLineBreak
      + '  padding: 35px;' + sLineBreak
      + '  background-image: '
        + 'linear-gradient('
          + 'to bottom, rgba(240, 255, 40, 1) 0%'
          + ', rgba(240, 255, 40, 1) 100%)'
        + ', linear-gradient('
          + 'to bottom, rgba(240, 40, 40, 1) 0%'
          + ', rgba(240, 40, 40, 1) 100%);'
        + sLineBreak
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
             + Format('    <td%s></td>', [
                 CreateStyle([
                   GetCellPaddingStyle(FGrid, FGrid.GetCell(Row, Col))
                 ])
               ])
             + sLineBreak;
        Continue;
      end;

      if (not Assigned(Cell.Item)) or (not Assigned(Cell.Item.GetVisualElement))  then
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
              GetCellPaddingStyle(FGrid, FGrid.GetCell(Row, Col)),
              GetTextHorizontalAlignStyle(FGrid.GetCell(Row, Col)),
              GetTextVerticalAlignStyle(FGrid.GetCell(Row, Col))
            ]),
            (Item.GetVisualElement as THtmlVisualElement).StrContent,
            sLineBreak
          ]
        );
    end;

    Result := Result + '  </tr>' + sLineBreak;
  end;

  Result := Result + '</table>' + sLineBreak;
end;

{ THtmlDivGridRenderer }

constructor THtmlDivGridRenderer.Create(AGrid: TGridLayout);
begin
  inherited Create;
  FGrid := AGrid;
end;

destructor THtmlDivGridRenderer.Destroy;
begin
  inherited Destroy;
end;

function THtmlDivGridRenderer.GetStyleTag: string;
  function CalcGridTemplateColumns: string;
  var
    C: Integer;
    Padding: TPadding;
  begin
    Result := '';
    for C := 0 to FGrid.Columns - 1 do
    begin
      Padding := CalcPaddingOfCell(FGrid, FGrid.GetCell(0, C));
      Result := Result + ' '
        + ( FGrid.ColumnWidth[C]
            + Padding.Left
            + Padding.Right
          ).ToString + 'px';
    end;
    Result := Result + ';';
  end;

  function CalcCurrentHight(Row: Integer): Integer;
  var
    Padding: TPadding;
  begin
    Padding := CalcPaddingOfCell(FGrid, FGrid.GetCell(Row, 0));
    Result := FGrid.RowHeight[Row]
      + Padding.Top
      + Padding.Bottom;
  end;

  function CalcGridTemplateRows: string;
  var
    R, Count: Integer;
    CurrentHeight: Integer;
  begin
    Result := '';
    R := 0;
    while R < FGrid.Rows do
    begin
      CurrentHeight := CalcCurrentHight(R); //FGrid.RowHeight[R] ;
      Count := 1;

      while (R + Count < FGrid.Rows)
            and
            (CalcCurrentHight(R + Count) = CurrentHeight) do
        Inc(Count);

      if Count >= 3 then
      begin
        if Result <> '' then
          Result := Result + ' ';
        Result := Result + Format('repeat(%d, %dpx)', [Count, CurrentHeight]);
        Inc(R, Count);
      end
      else
      begin
        if Result <> '' then
          Result := Result + ' ';
        Result := Result + Format('%dpx', [CurrentHeight]);
        Inc(R);
      end;
    end;

    Result := Result + ';';
  end;

  function CalcGridWidht: string;
  var
    I, Sum: Integer;
  begin
    Sum := 0;
    for I:=0 to FGrid.Columns-1 do
      Sum := Sum + FGrid.ColumnWidth[I];
    Result := IntToStr(Sum) + 'px;';
  end;

  function CalcGridPadding: string;
  begin
    Result := Format(
      '%dpx %dpx %dpx %dpx;', [
        FGrid.Margins.Top,
        FGrid.Margins.Right,
        FGrid.Margins.Bottom,
        FGrid.Margins.Left
      ]);
  end;

begin
  Result := '<style>' + sLineBreak
    + '  .grid-container {' + sLineBreak
    + '    display: grid;' + sLineBreak
    + '    grid-template-columns:' + CalcGridTemplateColumns + sLineBreak
    + '    grid-template-rows: ' + CalcGridTemplateRows + sLineBreak
    + '    width: ' + CalcGridWidht + sLineBreak
//    + '    border: 1px solid #ccc; ' + sLineBreak
    + '    padding: ' + CalcGridPadding + sLineBreak
    + '  }' + sLineBreak
    + '  .grid-item{' + sLineBreak

          + '  background-image: '
            + 'linear-gradient('
              + 'to bottom, rgba(240, 255, 40, 1) 0%'
              + ', rgba(240, 255, 40, 1) 100%)'
            + ', linear-gradient('
              + 'to bottom, rgba(240, 40, 40, 1) 0%'
              + ', rgba(240, 40, 40, 1) 100%);'
            + sLineBreak
          + '  background-clip: content-box, padding-box;' + sLineBreak


    + '    display: grid; ' + sLineBreak
    + '    align-items: center;  ' + sLineBreak
    + '    background-color: #e0e0e0;' + sLineBreak
    + '    padding: 0px;' + sLineBreak
    + '    text-align: center;' + sLineBreak
//    + '    border: 1px solid #aaa;' + sLineBreak
    + '    --offsetx: 0px;' + sLineBreak
    + '    margin-left: var(--offsetx);' + sLineBreak
    + '    margin-right: calc(-1 * var(--offsetx));' + sLineBreak
    + '    --offsety: 0px;' + sLineBreak
    + '    margin-top: var(--offsety);' + sLineBreak
    + '    margin-bottom: calc(-1 * var(--offsety));' + sLineBreak
    + '  }' + sLineBreak
    + '</style>';
end;

function THtmlDivGridRenderer.GetAsString: string;
var
  I, Row, Col: Integer;
  Cell: TGridCell;

  function GetGridItemStyle: string;
  begin
    Result := '';

    if Cell.ColSpan > 1 then
      Result := Result + 'grid-column: span ' + Cell.ColSpan.ToString + '; ';

    if Cell.RowSpan > 1 then
      Result := Result + 'grid-row: span ' + Cell.RowSpan.ToString + '; ';

    Result := Result + GetTextHorizontalAlignStyle(FGrid.GetCell(Row, Col)) + '; ';
    Result := Result + GetTextVerticalAligeItemsStyle(FGrid.GetCell(Row, Col)) + '; ';
    Result := Result + GetCellPaddingStyle(FGrid, FGrid.GetCell(Row, Col)) + ';';

    Result := TrimRight(Result);
  end;

begin
  Result :=
    GetStyleTag + sLineBreak
    + '<div class="grid-container">' + sLineBreak;

  for Row := 0 to FGrid.Rows - 1 do
  begin
    for Col := 0 to FGrid.Columns - 1 do
    begin
      Cell := FGrid.GetCell(Row, Col);

      if not Assigned(Cell) then
        Continue;

      Result := Result
        + '<div class="grid-item" style="' + GetGridItemStyle + '" '
        +   'row="' + Row.ToString
        +   '" col="' + Col.ToString + '">'
        + (Cell.Item.GetVisualElement as THtmlVisualElement).StrContent
        + '</div>'
        + sLineBreak;
    end;
  end;

  Result := Result
    + '</div>';
end;


{ THtmlVisualElement }

procedure THtmlVisualElement.Redraw(AContext: TGriItemRenderContext);
begin

end;

procedure THtmlVisualElement.SetStrContent(AValue: string);
begin
  if FStrContent = AValue then Exit;
  FStrContent := AValue;
end;

constructor THtmlVisualElement.Create(ARenderer: IHtmlGridRenderer);
begin
  FRenderer := ARenderer;
end;

destructor THtmlVisualElement.Destroy;
begin
  inherited Destroy;
end;

function THtmlVisualElement.GetHeight: Integer;
begin
  Result := FHeight;
end;

function THtmlVisualElement.GetLeft: Integer;
begin
  Result := FLeft;
end;

function THtmlVisualElement.GetTop: Integer;
begin
  Result := FTop;
end;

function THtmlVisualElement.GetVisible: Boolean;
begin
  Result := FVisible;
end;

function THtmlVisualElement.GetWidth: Integer;
begin
  Result := FWidth;
end;

procedure THtmlVisualElement.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  FLeft := ALeft;
  FTop := ATop;
  FWidth := AWidth;
  FHeight := AHeight;
end;

procedure THtmlVisualElement.SetHeight(AValue: Integer);
begin
  FHeight := AValue;
end;

procedure THtmlVisualElement.SetVisible(AValue: Boolean);
begin
  FVisible := AValue;
end;

procedure THtmlVisualElement.SetWidth(AValue: Integer);
begin
  FWidth := AValue;
end;

{ THtmlGridItem }

procedure THtmlGridItem.AfterSetBounds;
begin
  // nessa classe não faz nada
end;

constructor THtmlGridItem.Create(AElement: THtmlVisualElement);
begin
  inherited Create;
  FGridRenderer := AElement.Renderer;
  FElement := AElement;
end;

function THtmlGridItem.GetVisualElement: IVisualElement;
begin
  Result := FElement;
end;

function THtmlGridItem.GetRenderer: IGridItemRenderer;
begin
  Result := THtmlGridItemRenderer.Create(Self.FGridRenderer, Self);
end;

procedure THtmlGridItem.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  FElement.SetBounds(ALeft, ATop, AWidth, AHeight);
  AfterSetBounds;
end;

{ THtmlGridItemRenderer }

constructor THtmlGridItemRenderer.Create(AGridRenderer: IHtmlGridRenderer;
  AGridItem: THtmlGridItem);
begin
  FGridRenderer := AGridRenderer;
  FGridItem := AGridItem;
end;

procedure THtmlGridItemRenderer.RenderTo(AContext: TGriItemRenderContext);
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

