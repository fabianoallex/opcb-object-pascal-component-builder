unit UFGridText;

{$mode ObjFPC}{$H+}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ULayout,
  UGridText, UGridHtml;

type
  TVendaItem = record
    Descricao: string;
    Unidade: string;
    ValorUnitario: Double;
    Quantidade: Double;
    function Total: Double;
  end;

  { TFGridText }

  TFGridText = class(TForm)
    Button2: TButton;
    Button3: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    procedure GerarRelatorioVenda;
    procedure GerarRelatorioVendaHtml;

  public

  end;

var
  FGridText: TFGridText;

implementation

uses
  UGridItemFactory;

{$R *.lfm}

{ TFGridText }


procedure TFGridText.GerarRelatorioVenda;
const
  COL_DESC = 0;
  COL_UNID = 1;
  COL_VLR_UNIT = 2;
  COL_TOTAL = 3;
var
  Grid: TGridLayout;
  Renderer: TTextGridRenderer;
  I, Row: Integer;
  Item: TVendaItem;
  TextElement: TTextVisualElement;
  Items: array of TVendaItem;
  TotalGeral: Double;

  procedure AddHeaderCell(const AText: string; ACol: Integer);
  begin
    TGridItemFactory.Create
      .BuildTextItem(Renderer)
      .WithText(AText)
      .WithAlignment(tahCenter, tavMiddle)
      .AddToGrid(Grid, 0, ACol);
  end;

  procedure AddItemCell(const AText: string; ACol: Integer; Align: TTextAlignHorizontal = tahLeft);
  begin
    TGridItemFactory.Create
      .BuildTextItem(Renderer)
      .WithText(AText)
      .WithAlignment(Align, tavMiddle)
      .AddToGrid(Grid, Row, ACol);
  end;

begin
  // Simulando lista de itens
  SetLength(Items, 4);
  Items[0].Descricao := 'Arroz Branco';
  Items[0].Unidade := 'kg';
  Items[0].ValorUnitario := 5.40;
  Items[0].Quantidade := 2;

  Items[1].Descricao := 'Feijao Preto';
  Items[1].Unidade := 'kg';
  Items[1].ValorUnitario := 7.3;
  Items[1].Quantidade := 1;

  Items[2].Descricao := 'Oleo de Soja';
  Items[2].Unidade := 'lt';
  Items[2].ValorUnitario := 6.90;
  Items[2].Quantidade := 3;

  Items[3].Descricao := 'Sabao em po';
  Items[3].Unidade := 'un';
  Items[3].ValorUnitario := 10.90;
  Items[3].Quantidade := 1;

  Grid := TGridLayout.Create;
  try
    Grid.Columns := 4;
    Grid.Rows := Length(Items) + 4; // +1 para cabeçalho, +3 para total
    Grid.RowHeights := 1;
    Grid.ColumnWidths := 20;
    Grid.ColumnWidth[COL_DESC] := 30;
    Grid.ColumnWidth[COL_UNID] := 10;
    Grid.ColumnWidth[COL_VLR_UNIT] := 12;
    Grid.ColumnWidth[COL_TOTAL] := 12;
    Grid.HorizontalSpacings := 1;
    Grid.VerticalSpacings := 0;
    Grid.VerticalSpacing[0] := 1;
    Grid.VerticalSpacing[Length(Items)] := 1;  // ultimo item
    Grid.Margins.All := 1;

    Renderer := TTextGridRenderer.Create(Grid);

    AddHeaderCell('Descricao', COL_DESC);
    AddHeaderCell('Unidade', COL_UNID);
    AddHeaderCell('Vlr Unit.', COL_VLR_UNIT);
    AddHeaderCell('Total', COL_TOTAL);

    // Itens
    TotalGeral := 0;
    for I := 0 to High(Items) do
    begin
      Row := I + 1;
      Item := Items[I];
      TotalGeral := TotalGeral + Item.Total;

      AddItemCell(Item.Descricao, COL_DESC);
      AddItemCell(Item.Unidade, COL_UNID, tahCenter);
      AddItemCell(FormatFloat('0.00', Item.ValorUnitario) + ' ', COL_VLR_UNIT, tahRight);
      AddItemCell(FormatFloat('0.00', Item.Total) + ' ', COL_TOTAL, tahRight);
    end;

    TextElement := TTextVisualElement.Create(Renderer);
    TextElement.SetText('Total Geral  ');
    TextElement.HorizontalAlign := tahRight;
    TextElement.VerticalAlign := tavMiddle;
    Grid.AddItem(
      TTextGridItem.Create(TextElement),
      TGridCellSettings.Create(Row+1, 0)
        .WithColumnSpan(3)
        .WithRowSpan(3)
    );

    TextElement := TTextVisualElement.Create(Renderer);
    TextElement.SetText(FormatFloat('0.00', TotalGeral) + ' ');
    TextElement.HorizontalAlign := tahRight;
    TextElement.VerticalAlign := tavMiddle;
    Grid.AddItem(
      TTextGridItem.Create(TextElement),
      TGridCellSettings.Create(Row+1, 3)
        .WithRowSpan(3)
    );

    Grid.ArrangeItems;
    Memo1.Text := Renderer.GetAsString;

  finally
    Renderer.Free;
    Grid.Free;
  end;
end;



procedure TFGridText.GerarRelatorioVendaHtml;
const
  COL_DESC = 0;
  COL_UNID = 1;
  COL_VLR_UNIT = 2;
  COL_TOTAL = 3;
var
  Grid: TGridLayout;
  Renderer: IHtmlGridRenderer;
  I, Row: Integer;
  Item: TVendaItem;
  Element: THtmlVisualElement;
  Items: array of TVendaItem;
  TotalGeral: Double;

  procedure AddHeaderCell(const AText: string; ACol: Integer);
  begin
    TGridItemFactory.Create
      .BuildHtmlTableItem(Renderer)
      .WithStrContent(AText)
      .WithCellSettings(
        TGridCellSettings.Create(0, ACol)
          .WithAlignment(laCenter, laStart)
      )
      .AddToGrid(Grid);
  end;

  procedure AddItemCell(const AText: string; ACol: Integer; Align: TItemAlignment = laStart);
  begin
    TGridItemFactory.Create
      .BuildHtmlTableItem(Renderer)
      .WithStrContent(AText)
      .WithCellSettings(
        TGridCellSettings.Create(Row, ACol)
          .WithAlignment(Align, laCenter)
      )
      .AddToGrid(Grid);
  end;

begin
  // Simulando lista de itens
  SetLength(Items, 4);
  Items[0].Descricao := 'Arroz Branco';
  Items[0].Unidade := 'kg';
  Items[0].ValorUnitario := 5.40;
  Items[0].Quantidade := 2;

  Items[1].Descricao := 'Feijao Preto';
  Items[1].Unidade := 'kg';
  Items[1].ValorUnitario := 7.3;
  Items[1].Quantidade := 1;

  Items[2].Descricao := 'Oleo de Soja';
  Items[2].Unidade := 'lt';
  Items[2].ValorUnitario := 6.90;
  Items[2].Quantidade := 3;

  Items[3].Descricao := 'Sabao em po';
  Items[3].Unidade := 'un';
  Items[3].ValorUnitario := 10.90;
  Items[3].Quantidade := 1;

  Grid := TGridLayout.Create;
  try
    Grid.Columns := 4;
    Grid.Rows := Length(Items) + 4; // +1 para cabeçalho, +3 para total
    Grid.RowHeights := 50;
    Grid.ColumnWidths := 200;
    Grid.ColumnWidth[COL_DESC] := 300;
    Grid.ColumnWidth[COL_UNID] := 100;
    Grid.ColumnWidth[COL_VLR_UNIT] := 120;
    Grid.ColumnWidth[COL_TOTAL] := 120;
    Grid.HorizontalSpacings := 3;
    Grid.VerticalSpacings := 5;
    Grid.VerticalSpacing[0] := 10;
    Grid.VerticalSpacing[Length(Items)] := 8;  // ultimo item

    Grid.HorizontalSpacing[2] := 50;

    Grid.Margins.All := 5;

    Renderer := THtmlDivGridRenderer.Create(Grid);

    AddHeaderCell('Descricao', COL_DESC);
    AddHeaderCell('Unidade', COL_UNID);
    AddHeaderCell('Vlr Unit.', COL_VLR_UNIT);
    AddHeaderCell('Total', COL_TOTAL);

    // Itens
    TotalGeral := 0;
    for I := 0 to High(Items) do
    begin
      Row := I + 1;
      Item := Items[I];
      TotalGeral := TotalGeral + Item.Total;

      AddItemCell(Item.Descricao, COL_DESC);
      AddItemCell(Item.Unidade, COL_UNID, laCenter);
      AddItemCell(FormatFloat('0.00', Item.ValorUnitario) + ' ', COL_VLR_UNIT, laEnd);
      AddItemCell(FormatFloat('0.00', Item.Total) + ' ', COL_TOTAL, laEnd);
    end;

    Element := THtmlVisualElement.Create(Renderer);
    Element.StrContent := 'Total Geral  ';
    Grid.AddItem(
      THtmlGridItem.Create(Element),
      TGridCellSettings.Create(Row+1, 0)
        .WithAlignment(laEnd, laCenter)
        .WithColumnSpan(3)
        .WithRowSpan(3)
    );

    Element := THtmlVisualElement.Create(Renderer);
    Element.StrContent := FormatFloat('0.00', TotalGeral) + ' ';
    Grid.AddItem(
      THtmlGridItem.Create(Element),
      TGridCellSettings.Create(Row+1, 3)
        .WithRowSpan(3)
        .WithAlignment(laEnd, laCenter)
    );

    Grid.ArrangeItems;
    Memo1.Text := Renderer.GetAsString;

  finally
    Grid.Free;
  end;
end;

function TVendaItem.Total: Double;
begin
  Result := Quantidade * ValorUnitario;
end;

procedure TFGridText.Button1Click(Sender: TObject);
begin

end;

procedure TFGridText.Button2Click(Sender: TObject);
begin
  GerarRelatorioVenda;
end;

procedure TFGridText.Button3Click(Sender: TObject);
begin
  GerarRelatorioVendaHtml;
end;


end.

