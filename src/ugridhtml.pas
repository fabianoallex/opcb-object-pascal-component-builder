unit UGridHtml;

{$IFDEF FPC}
{$mode ObjFPC}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, ULayout, HtmlBuilder;

type
  TPadding = record
    Top: Integer;
    Right: Integer;
    Bottom: Integer;
    Left: Integer;
  end;

  { IHtmlGridRenderer }

  IHtmlGridRenderer = interface
    function GetAsString(AGrid: TGridLayout): string;
  end;

  { THtmlTableGridRenderer }

  THtmlTableGridRenderer = class(TInterfacedObject, IHtmlGridRenderer)
  private
    function CreateStyleElement: THTMLElement;
  public
    function GetAsString(AGrid: TGridLayout): string;
  end;

  { THtmlDivGridRenderer }

  THtmlDivGridRenderer = class(TInterfacedObject, IHtmlGridRenderer)
  private
    function CreateStyleElement(AGrid: TGridLayout): THTMLElement;
  public
    function GetAsString(AGrid: TGridLayout): string;
  end;

  { THtmlVisualElement }

  THtmlVisualElement = class(TInterfacedObject, IVisualElement)
  private
    FRenderer: IHtmlGridRenderer;
    FLeft, FTop, FWidth, FHeight: Integer;
    FStrContent: string;
    FVisible: Boolean;
    procedure Redraw;
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
    procedure Render;
  end;

implementation

function CalcCellPadding(AGrid: TGridLayout; ACell: TGridCell): TPadding;
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

    if HCol < AGrid.Columns - 1 then
      Right := Right + AGrid.HorizontalSpacing[HCol];

    if VRow < AGrid.Rows - 1 then
      Bottom := Bottom + AGrid.VerticalSpacing[VRow];
  end;
end;

{ IHtmlGridRenderer }

function THtmlTableGridRenderer.CreateStyleElement: THTMLElement;
var
  CssBuilder: TCSSBuilder;
begin
  CSSBuilder := TCSSBuilder.Create;
  Result := THTMLElement.Create('style');

  try
    CssBuilder
      .UsingRule('td')
        .Add('paddging', '35px')
        .Add('background-image',
          'linear-gradient(to bottom, rgba(240, 255, 40, 1) 0%, rgba(240, 255, 40, 1) 100%), ' +
          'linear-gradient(to bottom, rgba(240, 40, 40, 1) 0%, rgba(240, 40, 40, 1) 100%)'
        )
        .Add('background-clip', 'content-box, padding-box')
      .UsingRule('.lt').Add('text-align', 'left').Add('vertical-align', 'top')
      .UsingRule('.ct').Add('text-align', 'center').Add('vertical-align', 'top')
      .UsingRule('.rt').Add('text-align', 'right').Add('vertical-align', 'top')
      .UsingRule('.lc').Add('text-align', 'left').Add('vertical-align', 'middle')
      .UsingRule('.cc').Add('text-align', 'center').Add('vertical-align', 'middle')
      .UsingRule('.rc').Add('text-align', 'right').Add('vertical-align', 'middle')
      .UsingRule('.lb').Add('text-align', 'left').Add('vertical-align', 'bottom')
      .UsingRule('.cb').Add('text-align', 'center').Add('vertical-align', 'bottom')
      .UsingRule('.rb').Add('text-align', 'right').Add('vertical-align', 'bottom')
    ;

    Result.SetText(CssBuilder.GetCSS);
  finally
    CssBuilder.Free;
  end;
end;

function THtmlTableGridRenderer.GetAsString(AGrid: TGridLayout): string;
var
  Row, Col: Integer;
  Cell: TGridCell;
  StyleElement: THTMLElement;

  TableElement: THTMLElement;
  TrElement: THTMLElement;
  TdElement: THTMLElement;
  Padding: TPadding;

  function CalcColumnWidth(AColumn: Integer): string;
  var
    Width: Integer;
  begin
    Width := AGrid.ColumnWidth[AColumn];
    if AColumn = 0 then
      Width := Width + AGrid.Margins.Left;

    if AColumn = AGrid.Columns-1 then
      Width := Width + AGrid.Margins.Right
    else
      Width := Width + AGrid.HorizontalSpacing[AColumn];

    Result := Width.ToString + 'px';
  end;

  function CalcRowHeight(ARow: Integer): string;
  var
    Height: Integer;
  begin
    Height := AGrid.RowHeight[ARow];
    if ARow = 0 then
      Height := Height + AGrid.Margins.Top;

    if ARow = AGrid.Rows - 1 then
      Height := Height + AGrid.Margins.Bottom
    else
      Height := Height + AGrid.VerticalSpacing[ARow];

    Result := Height.ToString + 'px';
  end;

  function CreateColGroupElement: THTMLElement;
  var
    C: Integer;
    Element: THTMLElement;
  begin
    Result := THTMLElement.Create('colgroup');
    for C := 0 to AGrid.Columns - 1 do
    begin
      Element := Result.CreateChild('col');
      Element.Attributes.SetAttribute('width', CalcColumnWidth(C));
    end;
  end;

  function GetAlignmentClass(ACell: TGridCell): string;
  const
    HAlignMap: array[TItemAlignment] of string = ('l', 'c', 'l', 'r');
    VAlignMap: array[TItemAlignment] of string = ('t', 'c', 't', 'b');
  begin
    Result :=
      HAlignMap[ACell.HorizontalAlignment] +
      VAlignMap[ACell.VerticalAlignment];
  end;

begin
  TableElement := THTMLElement.Create('table');
  StyleElement := CreateStyleElement;
  try
    TableElement.Attributes.AddStyle('border', '0px');
    TableElement.AddChild(CreateColGroupElement);

    for Row := 0 to AGrid.Rows - 1 do
    begin
      TrElement := TableElement.CreateChild('tr');
      TrElement.Attributes.SetAttribute('height', CalcRowHeight(Row));

      for Col := 0 to AGrid.Columns - 1 do
      begin
        Cell := AGrid.GetCell(Row, Col);

        if not Assigned(Cell) then
        begin
          if not AGrid.IsCellSpan(Row, Col) then
          begin
            TdElement := TrElement.CreateChild('td');

            Padding := CalcCellPadding(AGrid, Cell);
            TdElement.Attributes.AddStyle('padding',
              Format(
                '%dpx %dpx %dpx %dpx',
                [Padding.Top, Padding.Right, Padding.Bottom, Padding.Left]
              )
            );
          end;

          Continue;
        end;

        if (not Assigned(Cell.Item)) or (not Assigned(Cell.Item.GetVisualElement))  then
          Continue;

        TdElement := TrElement.CreateChild('td');

        if Cell.RowSpan > 1 then
          TdElement.Attributes.SetAttribute('rowspan', Cell.RowSpan.ToString);
        if Cell.ColSpan > 1 then
          TdElement.Attributes.SetAttribute('colspan', Cell.ColSpan.ToString);

        Padding := CalcCellPadding(AGrid, Cell);
        TdElement.Attributes.AddStyle(
          'padding',
          Format(
            '%dpx %dpx %dpx %dpx',
            [Padding.Top, Padding.Right, Padding.Bottom, Padding.Left]
          )
        );

        TdElement.Attributes.AddClass(GetAlignmentClass(cell));
        TdElement.SetText((Cell.Item.GetVisualElement as THtmlVisualElement).StrContent);
      end;
    end;

    Result := StyleElement.Render + TableElement.Render;
  finally
    StyleElement.Free;
    TableElement.Free;
  end;
end;

{ THtmlDivGridRenderer }

function THtmlDivGridRenderer.CreateStyleElement(AGrid: TGridLayout): THTMLElement;
  function CalcGridTemplateColumns: string;
  var
    C: Integer;
    Padding: TPadding;
  begin
    Result := '';
    for C := 0 to AGrid.Columns - 1 do
    begin
      Padding := CalcCellPadding(AGrid, AGrid.GetCell(0, C));
      Result := Result + ' '
        + ( AGrid.ColumnWidth[C]
            + Padding.Left
            + Padding.Right
          ).ToString + 'px';
    end;
  end;

  function CalcCurrentHight(Row: Integer): Integer;
  var
    Padding: TPadding;
  begin
    Padding := CalcCellPadding(AGrid, AGrid.GetCell(Row, 0));
    Result := AGrid.RowHeight[Row]
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
    while R < AGrid.Rows do
    begin
      CurrentHeight := CalcCurrentHight(R);
      Count := 1;

      while (R + Count < AGrid.Rows)
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
  end;

  function CalcGridWidht: string;
  var
    I, Sum: Integer;
  begin
    Sum := 0;
    for I:=0 to AGrid.Columns-1 do
      Sum := Sum + AGrid.ColumnWidth[I];
    Result := IntToStr(Sum) + 'px';
  end;

  function CalcGridPadding: string;
  begin
    Result := Format(
      '%dpx %dpx %dpx %dpx', [
        AGrid.Margins.Top,
        AGrid.Margins.Right,
        AGrid.Margins.Bottom,
        AGrid.Margins.Left
      ]);
  end;

var
  CssBuilder: TCSSBuilder;
begin
  Result := THTMLElement.Create('style');
  CssBuilder := TCssBuilder.Create;
  try
    CssBuilder
      .UsingRule('.grid-container')
        .Add('display', 'grid')
        .Add('grid-template-columns', CalcGridTemplateColumns)
        .Add('grid-template-rows', CalcGridTemplateRows)
        .Add('width', CalcGridWidht)
        .Add('padding', CalcGridPadding)
      .UsingRule('.grid-item')
        .Add(
          'background-image',
          'linear-gradient(to bottom, rgba(240, 255, 40, 1) 0%, rgba(240, 255, 40, 1) 100%), ' +
          'linear-gradient(to bottom, rgba(240, 40, 40, 1) 0%, rgba(240, 40, 40, 1) 100%)'
        )
        .Add('background-clip', 'content-box, padding-box')
        .Add('display', 'grid')
        .Add('align-items', 'center')
        .Add('background-color', '#e0e0e0')
        .Add('padding', '0px')
        .Add('text-align', 'center')
        .Add('--offsetx', '0px')
        .Add('margin-left', 'var(--offsetx)')
        .Add('margin-right', 'calc(-1 * var(--offsetx))')
        .Add('--offsety', '0px')
        .Add('margin-top', 'var(--offsety)')
        .Add('margin-bottom', 'calc(-1 * var(--offsety))')
      .UsingRule('.lt').Add('text-align', 'left').Add('align-items', 'start')
      .UsingRule('.ct').Add('text-align', 'center').Add('align-items', 'start')
      .UsingRule('.rt').Add('text-align', 'right').Add('align-items', 'start')
      .UsingRule('.lc').Add('text-align', 'left').Add('align-items', 'center')
      .UsingRule('.cc').Add('text-align', 'center').Add('align-items', 'center')
      .UsingRule('.rc').Add('text-align', 'right').Add('align-items', 'center')
      .UsingRule('.lb').Add('text-align', 'left').Add('align-items', 'end')
      .UsingRule('.cb').Add('text-align', 'center').Add('align-items', 'end')
      .UsingRule('.rb').Add('text-align', 'right').Add('align-items', 'end')
    ;
    Result.SetText(CssBuilder.GetCSS);
  finally
    CssBuilder.Free;
  end;
end;

function THtmlDivGridRenderer.GetAsString(AGrid: TGridLayout): string;
var
  Row, Col: Integer;
  Cell: TGridCell;
  DivContainer: THTMLElement;
  DivStyle: THTMLElement;
  Padding: TPadding;

  function GetAlignmentClass(ACell: TGridCell): string;
  const
    HAlignMap: array[TItemAlignment] of string = ('l', 'c', 'l', 'r');
    VAlignMap: array[TItemAlignment] of string = ('t', 'c', 't', 'b');
  begin
    Result := HAlignMap[ACell.HorizontalAlignment]
      + VAlignMap[ACell.VerticalAlignment];
  end;

  function CreateDivItem(ACell: TGridCell): THTMLElement;
  begin
    Result := THTMLElement.Create('div');
    Result.Attributes.AddClass('grid-item');
    Result.Attributes.AddClass(GetAlignmentClass(Acell));

    Padding := CalcCellPadding(AGrid, ACell);
    Result.Attributes.AddStyle(
      'padding',
      Format(
        '%dpx %dpx %dpx %dpx',
        [Padding.Top, Padding.Right, Padding.Bottom, Padding.Left]
      )
    );

    if ACell.ColSpan > 1 then
      Result.Attributes.AddStyle('grid-column', ' span ' + ACell.ColSpan.ToString);
    if ACell.RowSpan > 1 then
      Result.Attributes.AddStyle('grid-row', ' span ' + ACell.RowSpan.ToString);

     Result.SetText(
       (ACell.Item.GetVisualElement as THtmlVisualElement).StrContent
     );
  end;

begin
  DivContainer := THTMLElement.Create('div');
  DivStyle := CreateStyleElement(AGrid);

  try
    DivContainer.Attributes.AddClass('grid-container');

    for Row := 0 to AGrid.Rows - 1 do
      for Col := 0 to AGrid.Columns - 1 do
      begin
        Cell := AGrid.GetCell(Row, Col);

        if not Assigned(Cell) then
          Continue;

        DivContainer.AddChild(CreateDivItem(Cell));
      end;

    Result := DivStyle.Render + DivContainer.Render;
  finally
    DivStyle.Free;
    DivContainer.Free;
  end;
end;

{ THtmlVisualElement }

procedure THtmlVisualElement.Redraw;
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

procedure THtmlGridItemRenderer.Render;
begin
{  FGridItem.GetVisualElement.SetBounds(
    AVisualElement.GetLeft,
    AVisualElement.GetTop,
    AVisualElement.GetWidth,
    AVisualElement.GetHeight
  );}

  FGridItem.GetVisualElement.Redraw;
end;

end.

