unit UFGridText;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ULayout,
  UGridText;

type

  { TFGridText }

  TFGridText = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  FGridText: TFGridText;

implementation

{$R *.lfm}

{ TFGridText }

procedure TFGridText.Button1Click(Sender: TObject);
var
  Grid: TGridLayout;
  Renderer: TGridTextRenderer;
  TextItem: TTextVisualElement;
begin
  Grid := TGridLayout.Create;

  try
    Grid.Rows := 4;
    Grid.Columns := 4;
    Grid.ColumnWidths := 10;
    Grid.ColumnWidth[0] := 35;
    Grid.RowHeights := 1;  // testar caso com mais de 1 dá erro
    Grid.HorizontalSpacings := 1;
    Grid.VerticalSpacings := 1;
    Grid.Margins.All := 0;

    Grid.VisibleColumn[3] := True;

    Grid.Top := 0;

    Renderer := TGridTextRenderer.Create(Grid);

    TextItem := TTextVisualElement.Create(Renderer);
    TextItem.SetText('TESTE');

    Grid.AddItem(
     TTextGridItem.Create(TextItem),
     TGridCellSettings.Create(0, 1)
    );

    Grid.AddItem(
     TTextGridItem.Create(TextItem),
     TGridCellSettings.Create(0, 0)
    );

    Grid.AddItem(
     TTextGridItem.Create(TextItem),
     TGridCellSettings.Create(1, 0)
    );

    Grid.AddItem(
     TTextGridItem.Create(TextItem),
     TGridCellSettings.Create(1, 1)
    );

    Grid.ArrangeItems;

    Memo1.Text := Renderer.GetAsString;
  finally
    Renderer.Free;
    Grid.Free;
  end;
end;

end.

