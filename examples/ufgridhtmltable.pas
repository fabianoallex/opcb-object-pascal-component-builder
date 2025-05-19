unit UFGridHtmlTable;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, LazHelpHTML,
  IpHtml, SynHighlighterHTML, ULayout, UGridHtmlTable, UGridItemFactory;

type

  { TFGridHtmlTable }

  TFGridHtmlTable = class(TForm)
    Button2: TButton;
    Memo1: TMemo;
    procedure Button2Click(Sender: TObject);
  private

  public

  end;

var
  FGridHtmlTable: TFGridHtmlTable;

implementation

{$R *.lfm}

{ TFGridHtmlTable }

procedure TFGridHtmlTable.Button2Click(Sender: TObject);
var
  Grid: TGridLayout;
  Renderer: TGridHtmlTableRenderer;
begin
  Grid := TGridLayout.Create;

  try
    Grid.Rows := 3;
    Grid.Columns := 3;
    Grid.ColumnWidths := 200;
    Grid.RowHeights := 60;
    Grid.RowHeight[2] := 100;

    Grid.VerticalSpacings := 1;  //20
    Grid.HorizontalSpacings := 1;  //20

    Grid.HorizontalSpacing[1] := 5;
    Grid.VerticalSpacing[0] := 5;

    Grid.Margins.All := 5;
    Grid.Margins.Top := 15;
    Grid.Margins.Bottom := 10;

    Renderer := TGridHtmlTableRenderer.Create(Grid);

    TGridItemFactory.Create
      .BuildHtmlTableItem(Renderer)
        .WithStrContent('<input type="text">')
        .WithCellSettings(
          TGridCellSettings.Create(0, 0)
           .WithRowSpan(2)
           .WithColumnSpan(2)
           .WithAlignment(laCenter, laCenter)
        )
        .AddToGrid(Grid);


    TGridItemFactory.Create
      .BuildHtmlTableItem(Renderer)
        .WithStrContent('456')
        .WithCellSettings(
          TGridCellSettings.Create(0, 2)
          .WithAlignment(laEnd, laCenter)
        )
        .AddToGrid(Grid);

    TGridItemFactory.Create
      .BuildHtmlTableItem(Renderer)
        .WithStrContent('789')
        .WithCellSettings(
          TGridCellSettings.Create(2, 0)
        )
        .AddToGrid(Grid);


    TGridItemFactory.Create
      .BuildHtmlTableItem(Renderer)
        .WithStrContent('333')
        .WithCellSettings(
          TGridCellSettings.Create(2, 2)
          .WithAlignment(laEnd, laEnd)
        )
        .AddToGrid(Grid);

    memo1.Text := Renderer.GetAsString;
  finally
    Renderer.Free;
    Grid.Free;
  end;
end;

end.

