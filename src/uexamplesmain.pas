unit UExamplesMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, ULayout, UGridLayoutBuilder, UGridLayoutFillerFactory;

type

  { TFExamplesMain }

  TFExamplesMain = class(TForm)
    Button1: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    Button14: TButton;
    Button15: TButton;
    Button16: TButton;
    Button17: TButton;
    Button18: TButton;
    Button19: TButton;
    Button2: TButton;
    Button20: TButton;
    Button21: TButton;
    Button22: TButton;
    Button23: TButton;
    Button24: TButton;
    Button25: TButton;
    Button26: TButton;
    Button27: TButton;
    Button28: TButton;
    Button29: TButton;
    Button3: TButton;
    Button30: TButton;
    Button31: TButton;
    Button32: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    PageControl: TPageControl;
    Panel3: TPanel;
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);
    procedure Button16Click(Sender: TObject);
    procedure Button17Click(Sender: TObject);
    procedure Button18Click(Sender: TObject);
    procedure Button19Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button20Click(Sender: TObject);
    procedure Button21Click(Sender: TObject);
    procedure Button22Click(Sender: TObject);
    procedure Button23Click(Sender: TObject);
    procedure Button24Click(Sender: TObject);
    procedure Button25Click(Sender: TObject);
    procedure Button26Click(Sender: TObject);
    procedure Button27Click(Sender: TObject);
    procedure Button28Click(Sender: TObject);
    procedure Button29Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button30Click(Sender: TObject);
    procedure Button31Click(Sender: TObject);
    procedure Button32Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
  private
    procedure AddExample_BasicColumnResizerGridLayout(AOwner: TForm;
      APageControl: TPageControl);
    procedure AddExample_BasicGridLayout(AOwner: TForm; APageControl: TPageControl);
    procedure AddExample_BasicGridLayout2(AOwner: TForm;
      APageControl: TPageControl);
    procedure AddExample_BasicGridLayoutBuilder(AOwner: TForm;
      APageControl: TPageControl);
    procedure AddExample_ColumnShift(AOwner: TForm; APageControl: TPageControl);
    procedure AddExample_ControlPanel(AOwner: TForm; APageControl: TPageControl
      );
    procedure AddExample_CustomSpacing(AOwner: TForm; APageControl: TPageControl);
    procedure AddExample_Dashboard(AOwner: TForm; APageControl: TPageControl);
    procedure AddExample_DistinctRowColumnSizes(AOwner: TForm;
      APageControl: TPageControl);
    procedure AddExample_FormLayout(AOwner: TForm; APageControl: TPageControl);
    procedure AddExample_GridFill(AOwner: TForm;
      APageControl: TPageControl);
    procedure AddExample_GridFillBuilder(AOwner: TForm;
      APageControl: TPageControl);
    procedure AddExample_GridFillBuilderUsedCell(AOwner: TForm;
      APageControl: TPageControl);
    procedure AddExample_GridFillKeyboard(AOwner: TForm;
      APageControl: TPageControl);
    procedure AddExample_GridFillKeyboardAlpha(AOwner: TForm;
      APageControl: TPageControl);
    procedure AddExample_GridFillWithEvents(AOwner: TForm;
      APageControl: TPageControl);
    procedure AddExample_GridFillWithSpansBeforePlaceItem(
      AGridFill: IGridFill;
      AGrid: TGridLayout;
      AItem: ILayoutItem;
      APos: IGridPosition;
      var ASettings: TGridCellSettings;
      var ACanPlace: Boolean);
    procedure AddExample_GridFillWithSpansAfterPlaceItem(AGridFill: IGridFill;
      AGrid: TGridLayout; AItem: ILayoutItem; var APos: IGridPosition);
    procedure AddExample_ReportGrid(AOwner: TForm; APageControl: TPageControl);
    procedure AddExample_RowAndColumnShift(AOwner: TForm;
      APageControl: TPageControl);
    procedure AddExample_RowAndColumnShift2(AOwner: TForm;
      APageControl: TPageControl);
    procedure AddExample_RowColSpan(AOwner: TForm; APageControl: TPageControl);
    procedure AddExample_RowColSpanBuilder(AOwner: TForm;
      APageControl: TPageControl);
    procedure AddExample_RowShift(AOwner: TForm; APageControl: TPageControl);
    procedure AddExample_Spreadsheet(AOwner: TForm; APageControl: TPageControl);
    procedure AddExample_SpreadsheetEditable(AOwner: TForm;
      APageControl: TPageControl);
    procedure AddExample_NestedGrid(AOwner: TForm; APageControl: TPageControl);
    procedure AddExample_NestedGridNoContainer(AOwner: TForm; APageControl: TPageControl);
    procedure AddExample_WeeklyAgenda(AOwner: TForm; APageControl: TPageControl);
    procedure AddExample_NestedGridNoContainerSubGridBuilder(AOwner: TForm; APageControl: TPageControl);
    function CreateButtonFactory(APosition: IGridPosition; AOwner: TWinControl
      ): ILayoutItem;
  public

  end;

var
  FExamplesMain: TFExamplesMain;

implementation

uses
  StrUtils, UGridLayoutFill, UGridaLayoutResizer, UFColumnResizerGridLayout,
  UFRowResizerGridLayout, UFFullResizerGridLayout;

procedure TFExamplesMain.Button1Click(Sender: TObject);
begin
  AddExample_BasicGridLayout(Self, PageControl);
end;

procedure TFExamplesMain.Button20Click(Sender: TObject);
begin
  AddExample_NestedGrid(Self, PageControl);
end;

procedure TFExamplesMain.Button21Click(Sender: TObject);
begin
  AddExample_NestedGridNoContainer(Self, PageControl);
end;

procedure TFExamplesMain.Button22Click(Sender: TObject);
begin
  AddExample_BasicGridLayout2(Self, PageControl);
end;

procedure TFExamplesMain.Button23Click(Sender: TObject);
begin
  AddExample_BasicGridLayoutBuilder(Self, PageControl);
end;

procedure TFExamplesMain.Button24Click(Sender: TObject);
begin
  AddExample_RowColSpanBuilder(Self, PageControl);
end;

procedure TFExamplesMain.Button25Click(Sender: TObject);
begin
  AddExample_GridFillBuilder(Self, PageControl);
end;

procedure TFExamplesMain.Button26Click(Sender: TObject);
begin
  AddExample_GridFillBuilderUsedCell(Self, PageControl);
end;

procedure TFExamplesMain.Button27Click(Sender: TObject);
begin
  AddExample_NestedGridNoContainerSubGridBuilder(Self, PageControl);
end;

procedure TFExamplesMain.Button28Click(Sender: TObject);
begin

end;

procedure TFExamplesMain.Button29Click(Sender: TObject);
begin
  AddExample_BasicColumnResizerGridLayout(Self, PageControl);
end;

procedure TFExamplesMain.Button10Click(Sender: TObject);
begin
  AddExample_GridFillWithEvents(Self, PageControl);
end;

procedure TFExamplesMain.Button11Click(Sender: TObject);
begin
  AddExample_GridFillKeyboard(Self, PageControl);
end;

procedure TFExamplesMain.Button12Click(Sender: TObject);
begin
  AddExample_GridFillKeyboardAlpha(Self, PageControl);
end;

procedure TFExamplesMain.Button13Click(Sender: TObject);
begin
  AddExample_ControlPanel(Self, PageControl);
end;

procedure TFExamplesMain.Button14Click(Sender: TObject);
begin
  AddExample_FormLayout(Self, PageControl);
end;

procedure TFExamplesMain.Button15Click(Sender: TObject);
begin
  AddExample_WeeklyAgenda(Self, PageControl);
end;

procedure TFExamplesMain.Button16Click(Sender: TObject);
begin
  AddExample_Dashboard(Self, PageControl);
end;

procedure TFExamplesMain.Button17Click(Sender: TObject);
begin
  AddExample_ReportGrid(Self, PageControl);
end;

procedure TFExamplesMain.Button18Click(Sender: TObject);
begin
  AddExample_Spreadsheet(Self, PageControl);
end;

procedure TFExamplesMain.Button19Click(Sender: TObject);
begin
  AddExample_SpreadsheetEditable(Self, PageControl);
end;

procedure TFExamplesMain.Button2Click(Sender: TObject);
begin
  AddExample_RowColSpan(Self, PageControl);
end;

procedure TFExamplesMain.Button30Click(Sender: TObject);
begin
  FColumnResizerGridLayout.ShowModal;
end;

procedure TFExamplesMain.Button31Click(Sender: TObject);
begin
  FRowResizerGridLayout.ShowModal;
end;

procedure TFExamplesMain.Button32Click(Sender: TObject);
begin
  FFullResizerGridLayout.ShowModal;
end;

procedure TFExamplesMain.Button3Click(Sender: TObject);
begin
  AddExample_DistinctRowColumnSizes(Self, PageControl)
end;

procedure TFExamplesMain.Button4Click(Sender: TObject);
begin
  AddExample_CustomSpacing(Self, PageControl)
end;

procedure TFExamplesMain.Button5Click(Sender: TObject);
begin
  AddExample_RowShift(Self, PageControl);
end;

procedure TFExamplesMain.Button6Click(Sender: TObject);
begin
  AddExample_ColumnShift(Self, PageControl);
end;

procedure TFExamplesMain.Button7Click(Sender: TObject);
begin
  AddExample_RowAndColumnShift(Self, PageControl);
end;

procedure TFExamplesMain.Button8Click(Sender: TObject);
begin
  AddExample_RowAndColumnShift2(Self, PageControl);
end;

procedure TFExamplesMain.Button9Click(Sender: TObject);
begin
  AddExample_GridFill(Self, PageControl);
end;

procedure TFExamplesMain.AddExample_BasicGridLayout(AOwner: TForm;
  APageControl: TPageControl);
var
  Tab: TTabSheet;
  Grid: TGridLayout;
  I: Integer;
  Btn: TButton;
begin
  Tab := TTabSheet.Create(PageControl);
  Tab.PageControl := PageControl;
  Tab.Caption := 'Grid Simples';

  Grid := TGridLayout.Create;
  Grid.Rows := 3;
  Grid.Columns := 3;
  Grid.ColumnWidths := 80;
  Grid.RowHeights := 50;
  Grid.HorizontalSpacings := 10;
  Grid.VerticalSpacings := 10;
  Grid.Margins.All := 10;

  for I := 0 to 8 do
  begin
    Btn := TButton.Create(Tab);
    Btn.Parent := Tab;
    Btn.Caption := 'Botão ' + IntToStr(I + 1);

    Grid.AddItem(Btn, TGridCellSettings.Create(I div 3, I mod 3));
  end;

  Grid.ArrangeItems;
end;

procedure TFExamplesMain.AddExample_BasicColumnResizerGridLayout(AOwner: TForm;
  APageControl: TPageControl);
var
  Tab: TTabSheet;
  Grid: TGridLayout;
  I: Integer;
  Btn: TButton;
  Resizer: IGridLayoutWidthResizer;
begin
  Tab := TTabSheet.Create(PageControl);
  Tab.PageControl := PageControl;
  Tab.Caption := 'Grid Simples';

  Resizer := TGridLayoutWidthResizer
    .Create
    .WithGridWidth(Tab.Width);

  Grid := TGridLayout.Create;
  Grid.Rows := 3;
  Grid.Columns := 3;
  Grid.ColumnWidths := 80;
  Grid.RowHeights := 50;
  Grid.HorizontalSpacings := 10;
  Grid.VerticalSpacings := 0;
  Grid.Margins.All := 10;

  for I := 0 to 8 do
  begin
    Btn := TButton.Create(Tab);
    Btn.Parent := Tab;
    Btn.Caption := 'Botão ' + IntToStr(I + 1);

    Grid.AddItem(Btn, TGridCellSettings.Create(I div 3, I mod 3));
  end;

  Grid.ArrangeItems;

  Application.ProcessMessages;

  Sleep(2000);

  Resizer.Resize(Grid);

  Grid.ArrangeItems;
end;

procedure TFExamplesMain.AddExample_RowColSpan(AOwner: TForm;
  APageControl: TPageControl);
var
  Tab: TTabSheet;
  Grid: TGridLayout;
  Btn1, Btn2, Btn3, Btn4: TButton;
begin
  Tab := TTabSheet.Create(PageControl);
  Tab.PageControl := PageControl;
  Tab.Caption := 'RowSpan / ColSpan';

  Grid := TGridLayout.Create;
  try
    Grid.Rows := 3;
    Grid.Columns := 3;
    Grid.ColumnWidths := 80;
    Grid.RowHeights := 50;
    Grid.HorizontalSpacings := 10;
    Grid.VerticalSpacings := 10;
    Grid.Margins.All := 10;

    // Botão que ocupa 2 colunas
    Btn1 := TButton.Create(Tab);
    Btn1.Parent := Tab;
    Btn1.Caption := 'ColSpan 2';

    Grid.AddItem(
      Btn1,
      TGridCellSettings
        .Create(0, 0)
        .WithColumnSpan(2)
    );

    // Botão que ocupa 2 linhas
    Btn2 := TButton.Create(Tab);
    Btn2.Parent := Tab;
    Btn2.Caption := 'RowSpan 2';
    Grid.AddItem(
      Btn2,
      TGridCellSettings.Create(1, 2)
        .WithRowSpan(2)
        .WithAlignment(laStretch, laStretch)
    );

    // Botão normal
    Btn3 := TButton.Create(Tab);
    Btn3.Parent := Tab;
    Btn3.Caption := 'Normal';
    Grid.AddItem(Btn3, TGridCellSettings.Create(0, 2));

    // Botão normal
    Btn4 := TButton.Create(Tab);
    Btn4.Parent := Tab;
    Btn4.Caption := 'Normal';
    Grid.AddItem(Btn4, TGridCellSettings.Create(2, 0));

    Grid.ArrangeItems;
  finally
    Grid.Free;
  end;
end;

procedure TFExamplesMain.AddExample_RowColSpanBuilder(AOwner: TForm;
  APageControl: TPageControl);
var
  Tab: TTabSheet;
  Grid: TGridLayout;
  Btn1, Btn2, Btn3, Btn4: TButton;

  function CreateButton(Caption: TCaption): TButton;
  begin
    Result := TButton.Create(Tab);
    Result.Parent := Tab;
    Result.Caption := Caption;
  end;

begin
  Tab := TTabSheet.Create(PageControl);
  Tab.PageControl := PageControl;
  Tab.Caption := 'RowSpan / ColSpan';

  Btn1 := CreateButton('ColSpan 2');
  Btn2 := CreateButton('RowSpan 2');
  Btn3 := CreateButton('Normal');
  Btn4 := CreateButton('Normal');

  Grid := TGridLayoutBuilder.Create
    .WithDimensions(3, 3)
    .WithRowAndColumnSizes(50, 80)
    .WithSpacings(10)
    .WithMargins(10)
    .AddItem(Btn1, TGridCellSettings.Create(0, 0).WithColumnSpan(2))
    .AddItem(Btn2, TGridCellSettings.Create(1, 2).WithRowSpan(2).WithAlignment(laStretch, laStretch))
    .AddItem(Btn3, TGridCellSettings.Create(0, 2))
    .AddItem(Btn4, TGridCellSettings.Create(2, 0))
    .Done;

  try
    Grid.ArrangeItems;
  finally
    Grid.Free;
  end;
end;

procedure TFExamplesMain.AddExample_DistinctRowColumnSizes(AOwner: TForm;
  APageControl: TPageControl);
var
  Tab: TTabSheet;
  Grid: TGridLayout;
  Btn: TButton;
  I: Integer;
begin
  Tab := TTabSheet.Create(PageControl);
  Tab.PageControl := PageControl;
  Tab.Caption := 'Tamanhos distintos';

  Grid := TGridLayout.Create;
  try
    Grid.Rows := 3;
    Grid.Columns := 3;
    Grid.ColumnWidths := 60;
    Grid.RowHeights := 40;
    Grid.Margins.All := 10;
    Grid.HorizontalSpacings := 10;
    Grid.VerticalSpacings := 10;

    // Alterações específicas
    Grid.ColumnWidth[0] := 150;
    Grid.ColumnWidth[2] := 100;
    Grid.RowHeight[1] := 90;
    Grid.RowHeight[2] := 120;

    for I := 0 to 24 do
    begin
      Btn := TButton.Create(Tab);
      Btn.Parent := Tab;
      Btn.Caption := 'Botão ' + IntToStr(I + 1);

      Grid.AddItem(Btn, TGridCellSettings.Create(I div 5, I mod 5));
    end;

    Grid.ArrangeItems;
  finally
    Grid.Free;
  end;
end;

procedure TFExamplesMain.AddExample_CustomSpacing(AOwner: TForm;
  APageControl: TPageControl);
var
  Tab: TTabSheet;
  Grid: TGridLayout;
  Btn: TButton;
  I: Integer;
begin
  Tab := TTabSheet.Create(PageControl);
  Tab.PageControl := PageControl;
  Tab.Caption := 'Espaçamento customizado';

  Grid := TGridLayout.Create;
  try
    Grid.Rows := 5;
    Grid.Columns := 5;
    Grid.ColumnWidths := 80;
    Grid.RowHeights := 50;
    Grid.Margins.All := 10;
    Grid.HorizontalSpacings := 2;
    Grid.VerticalSpacings := 2;
    Grid.HorizontalSpacing[0] := 30; // entre coluna 0 e 1
    Grid.VerticalSpacing[0] := 20; // entre linha 0 e 1

    for I := 0 to 24 do
    begin
      Btn := TButton.Create(Tab);
      Btn.Parent := Tab;
      Btn.Caption := 'Botão ' + IntToStr(I + 1);
      Grid.AddItem(Btn, TGridCellSettings.Create(I div 5, I mod 5));
    end;

    Grid.ArrangeItems;
  finally
    Grid.Free;
  end;
end;

procedure TFExamplesMain.AddExample_RowShift(AOwner: TForm;
  APageControl: TPageControl);
var
  Tab: TTabSheet;
  Grid: TGridLayout;
  Btn: TButton;
  I: Integer;
begin
  Tab := TTabSheet.Create(APageControl);
  Tab.PageControl := APageControl;
  Tab.Caption := 'Row Shift';

  Grid := TGridLayout.Create;
  try
    Grid.Rows := 5;
    Grid.Columns := 5;
    Grid.ColumnWidths := 80;
    Grid.RowHeights := 50;
    Grid.Margins.All := 10;
    Grid.HorizontalSpacings := 2;
    Grid.VerticalSpacings := 2;

    for I := 0 to 24 do
    begin
      Btn := TButton.Create(Tab);
      Btn.Parent := Tab;
      Btn.Caption := 'Botão ' + IntToStr(I + 1);
      Grid.AddItem(Btn, TGridCellSettings.Create(I div 5, I mod 5));
      Grid.RowShift[I div 5] := 10 * (I div 5);
    end;

    Grid.ArrangeItems;
  finally
    Grid.Free;
  end;
end;

procedure TFExamplesMain.AddExample_ColumnShift(AOwner: TForm;
  APageControl: TPageControl);
var
  Tab: TTabSheet;
  Grid: TGridLayout;
  Btn: TButton;
  I: Integer;
begin
  Tab := TTabSheet.Create(APageControl);
  Tab.PageControl := APageControl;
  Tab.Caption := 'Col Shift';

  Grid := TGridLayout.Create;
  try
    Grid.Rows := 5;
    Grid.Columns := 5;
    Grid.ColumnWidths := 80;
    Grid.RowHeights := 50;
    Grid.Margins.All := 10;
    Grid.HorizontalSpacings := 2;
    Grid.VerticalSpacings := 2;

    for I := 0 to 24 do
    begin
      Btn := TButton.Create(Tab);
      Btn.Parent := Tab;
      Btn.Caption := 'Botão ' + IntToStr(I + 1);
      Grid.AddItem(Btn, TGridCellSettings.Create(I div 5, I mod 5));
      Grid.ColumnShift[I mod 5] := 10 * (I mod 5);
    end;

    Grid.ArrangeItems;
  finally
    Grid.Free;
  end;
end;

procedure TFExamplesMain.AddExample_RowAndColumnShift(AOwner: TForm;
  APageControl: TPageControl);
var
  Tab: TTabSheet;
  Grid: TGridLayout;
  Btn: TButton;
  I: Integer;
begin
  Tab := TTabSheet.Create(APageControl);
  Tab.PageControl := APageControl;
  Tab.Caption := 'Row/Col Shift';

  Grid := TGridLayout.Create;
  try
    Grid.Rows := 5;
    Grid.Columns := 5;
    Grid.ColumnWidths := 80;
    Grid.RowHeights := 50;
    Grid.Margins.All := 10;
    Grid.HorizontalSpacings := 5;
    Grid.VerticalSpacings := 5;

    for I := 0 to 24 do
    begin
      Btn := TButton.Create(Tab);
      Btn.Parent := Tab;
      Btn.Caption := 'Botão ' + IntToStr(I + 1);
      Grid.AddItem(Btn, TGridCellSettings.Create(I div 5, I mod 5));
      Grid.ColumnShift[I mod 5] := 10 * (I mod 5);
      Grid.RowShift[I div 5] := 10 * (I div 5);
    end;

    Grid.ArrangeItems;
  finally
    Grid.Free;
  end;
end;

procedure TFExamplesMain.AddExample_RowAndColumnShift2(AOwner: TForm;
  APageControl: TPageControl);
var
  Tab: TTabSheet;
  Grid: TGridLayout;
  Btn: TButton;
  I: Integer;
  Settings: TGridCellSettings;
begin
  Tab := TTabSheet.Create(APageControl);
  Tab.PageControl := APageControl;
  Tab.Caption := 'Cell Offset';

  Grid := TGridLayout.Create;
  try
    Grid.Rows := 5;
    Grid.Columns := 5;
    Grid.ColumnWidths := 80;
    Grid.RowHeights := 50;
    Grid.Margins.All := 10;
    Grid.HorizontalSpacings := 15;
    Grid.VerticalSpacings := 15;

    for I := 0 to 24 do
    begin
      Btn := TButton.Create(Tab);
      Btn.Parent := Tab;
      Btn.Caption := 'Botão ' + IntToStr(I + 1);

      Settings := TGridCellSettings.Create(I div 5, I mod 5);

      if (I = 6) then
      begin
        Settings.WithOffsetX(10);
        Btn.Caption := 'Offset X';
      end;

      if (I = 12) then
      begin
        Settings.WithOffsetY(10);
        Btn.Caption := 'Offset Y';
      end;

      if (I = 18) then
      begin
        Settings.WithOffsetX(10);
        Settings.WithOffsetY(10);
        Btn.Caption := 'Offset XY';
      end;

      if (I = 24) then
      begin
        Settings.WithOffsetX(-10);
        Settings.WithOffsetY(-10);
        Btn.Caption := 'Offset -X-Y';
      end;

      Grid.AddItem(Btn, Settings);
    end;

    Grid.ArrangeItems;
  finally
    Grid.Free;
  end;
end;

procedure TFExamplesMain.AddExample_GridFillWithSpansBeforePlaceItem(
  AGridFill: IGridFill; AGrid: TGridLayout; AItem: ILayoutItem; APos: IGridPosition;
  var ASettings: TGridCellSettings; var ACanPlace: Boolean);
begin
  if (AItem.GetControl is TButton) and (TButton(AItem.GetControl).Tag = 4) then
  begin
    ASettings.WithColumnSpan(2);
  end;
end;

procedure TFExamplesMain.AddExample_GridFillWithSpansAfterPlaceItem(
  AGridFill: IGridFill; AGrid: TGridLayout; AItem: ILayoutItem;
  var APos: IGridPosition);
begin
  if (AItem.GetControl is TButton) and (TButton(AItem.GetControl).Tag = 4) then
  begin
    APos := AGridFill.NextPosition;
  end;

  if (AItem.GetControl is TButton) and (TButton(AItem.GetControl).Tag = 8) then
  begin
    APos := AGridFill.NextPosition;
  end;
end;

procedure TFExamplesMain.AddExample_GridFill(AOwner: TForm;
  APageControl: TPageControl);
var
  Tab: TTabSheet;
  Grid: TGridLayout;
  GridFill: IGridFill;
  I: Integer;
  Btn: TButton;
begin
  Tab := TTabSheet.Create(APageControl);
  Tab.PageControl := APageControl;
  Tab.Caption := 'GridFill';

  Grid := TGridLayout.Create;
  try
    Grid.Rows := 3;
    Grid.Columns := 4;
    Grid.ColumnWidths := 70;
    Grid.RowHeights := 40;
    Grid.HorizontalSpacings := 8;
    Grid.VerticalSpacings := 8;
    Grid.Margins.All := 10;

    GridFill := TGridFillRowFirst.Create(Grid);

    for I := 0 to 9 do
    begin
      Btn := TButton.Create(Tab);
      Btn.Parent := Tab;
      Btn.Caption := Format('Btn %d', [I + 1]);
      Btn.Tag := I;

      if I = 5 then
        GridFill.Skip(2);

      GridFill.PlaceItem(Btn);
    end;

    Grid.ArrangeItems;
  finally
    Grid.Free;
  end;
end;


function TFExamplesMain.CreateButtonFactory(APosition: IGridPosition;
  AOwner: TWinControl): ILayoutItem;
var
  Btn: TButton;
begin
  Btn := TButton.Create(AOwner);
  Btn.Parent := AOwner;
  Btn.Caption := Format('Btn %d', [APosition.Row * APosition.Column + APosition.Column]);
  Result := TControlLayoutItem.Create(Btn);
end;


procedure TFExamplesMain.AddExample_GridFillBuilder(AOwner: TForm;
  APageControl: TPageControl);
var
  Tab: TTabSheet;
  I, CountSkip: Integer;
  Btns: array of TControl;
  Btn: TButton;
begin
  Tab := TTabSheet.Create(APageControl);
  Tab.PageControl := APageControl;
  Tab.Caption := 'GridFill';

  CountSkip := 0;
  SetLength(Btns, 12);
  for I := 0 to 11 do
  begin
    if I in [5, 6] then
    begin
      Inc(CountSkip);
      Btns[I] := nil;  //Skip
      Continue;
    end;
    Btn := TButton.Create(Tab);
    Btn.Parent := Tab;
    Btn.Caption := Format('Btn %d', [I-CountSkip + 1]);
    Btn.Tag := I-CountSkip;
    Btns[I] := Btn;
  end;

  with TGridLayoutBuilder.Create
    .WithDimensions(3, 4)
    .WithRowAndColumnSizes(40, 70)
    .WithSpacings(8)
    .WithMargins(10)
    .UsingFiller(ftColumnFirst)
    .FillItems(Btns)
    .Done do
  begin
    ArrangeItems;
    Free;
  end;
end;

procedure TFExamplesMain.AddExample_GridFillBuilderUsedCell(AOwner: TForm;
  APageControl: TPageControl);
var
  Tab: TTabSheet;
  I, CountSkip: Integer;
  Btns: array of TControl;
  Btn: TButton;
begin
  Tab := TTabSheet.Create(APageControl);
  Tab.PageControl := APageControl;
  Tab.Caption := 'GridFill';

  CountSkip := 0;
  SetLength(Btns, 10);
  for I := Low(Btns) to High(Btns) do
  begin
    Btn := TButton.Create(Tab);
    Btn.Parent := Tab;
    Btn.Caption := Format('Btn %d', [I-CountSkip + 1]);
    Btn.Tag := I-CountSkip;
    Btns[I] := Btn;
  end;

  Btn := TButton.Create(Tab);
  Btn.Parent := Tab;
  Btn.Caption := ' OCUPADAS ';

  with TGridLayoutBuilder.Create
    .WithDimensions(3, 4)
    .WithRowAndColumnSizes(40, 70)
    .WithSpacings(8)
    .WithMargins(10)
    .AddItem(Btn, TGridCellSettings.Create(0, 1).WithColumnSpan(2)) // adiciona manualmente
    .UsingFiller(ftColumnFirst) // define um preechedor
    .FillItems(Btns, TGridPosition.Create(0, 3))            // preenche o array de botões
    .Done do
  begin
    ArrangeItems;
    Free;
  end;
end;

procedure TFExamplesMain.AddExample_GridFillWithEvents(AOwner: TForm;
  APageControl: TPageControl);
var
  Tab: TTabSheet;
  Grid: TGridLayout;
  GridFill: IGridFill;
  I: Integer;
  Btn: TButton;
begin
  Tab := TTabSheet.Create(APageControl);
  Tab.PageControl := APageControl;
  Tab.Caption := 'GridFill Events';

  Grid := TGridLayout.Create;
  try
    Grid.Rows := 3;
    Grid.Columns := 4;
    Grid.ColumnWidths := 70;
    Grid.RowHeights := 40;
    Grid.HorizontalSpacings := 8;
    Grid.VerticalSpacings := 8;
    Grid.Margins.All := 10;

    GridFill := TGridFillRowFirst.Create(Grid);
    GridFill.OnBeforePlaceItem := @AddExample_GridFillWithSpansBeforePlaceItem;
    GridFill.OnAfterPlaceItem := @AddExample_GridFillWithSpansAfterPlaceItem;

    for I := 0 to 9 do
    begin
      Btn := TButton.Create(Tab);
      Btn.Parent := Tab;
      Btn.Caption := Format('Btn %d', [I + 1]);
      Btn.Tag := I;
      GridFill.PlaceItem(Btn);
    end;

    Grid.ArrangeItems;
  finally
    Grid.Free;
  end;
end;

procedure TFExamplesMain.AddExample_GridFillKeyboard(AOwner: TForm; APageControl: TPageControl);
const
  Keys: array[0..11] of string = ('1','2','3','4','5','6','7','8','9','←','0','⏎');
var
  Tab: TTabSheet;
  Grid: TGridLayout;
  Fill: IGridFill;
  Btn: TButton;
  I: Integer;
begin
  Tab := TTabSheet.Create(APageControl);
  Tab.PageControl := APageControl;
  Tab.Caption := 'GridFill - Teclado';

  Grid := TGridLayout.Create;
  try
    Grid.Rows := 4;
    Grid.Columns := 3;
    Grid.ColumnWidths := 60;
    Grid.RowHeights := 60;
    Grid.HorizontalSpacings := 10;
    Grid.VerticalSpacings := 10;
    Grid.Margins.All := 20;

    Fill := TGridFillRowFirst.Create(Grid);

    for I := 0 to High(Keys) do
    begin
      Btn := TButton.Create(Tab);
      Btn.Parent := Tab;
      Btn.Caption := Keys[I];
      Btn.Font.Size := 14;

      Fill.PlaceItem(Btn);
    end;

    Grid.ArrangeItems;
  finally
    Grid.Free;
  end;
end;

procedure TFExamplesMain.AddExample_GridFillKeyboardAlpha(AOwner: TForm; APageControl: TPageControl);
const
  Row0: array[0..9] of string = ('F1', 'F2', 'F3', 'F4', 'F5', 'F6', 'F7', 'F8', 'F9', 'F10');
  Row1: array[0..9] of string = ('Q', 'W', 'E', 'R', 'T', 'Y', 'U', 'I', 'O', 'P');
  Row2: array[0..8] of string = ('A', 'S', 'D', 'F', 'G', 'H', 'J', 'K', 'L');
  Row3: array[0..7] of string = ('Z', 'X', 'C', 'V', 'B', 'N', 'M', '←');
var
  Tab: TTabSheet;
  Grid: TGridLayout;
  Fill: IGridFill;
  Btn: TButton;
  I: Integer;
begin
  Tab := TTabSheet.Create(APageControl);
  Tab.PageControl := APageControl;
  Tab.Caption := 'Teclado QWERTY';

  Grid := TGridLayout.Create;
  try
    Grid.Rows := 5;
    Grid.Columns := 10;
    Grid.ColumnWidths := 50;
    Grid.RowHeights := 50;
    Grid.HorizontalSpacings := 5;
    Grid.VerticalSpacings := 5;
    Grid.Margins.All := 10;
    Grid.RowShift[2] := 20;
    Grid.RowShift[3] := 45;
    Grid.RowHeight[0] := 35;
    Grid.VerticalSpacing[0] := 20;

    Fill := TGridFillRowFirst.Create(Grid);
    Fill.InitialPos(TGridPosition.Create(0, 0));

    for I := 0 to High(Row0) do
    begin
      Btn := TButton.Create(Tab);
      Btn.Parent := Tab;
      Btn.Caption := Row0[I];
      Fill.PlaceItem(Btn);
    end;

    for I := 0 to High(Row1) do
    begin
      Btn := TButton.Create(Tab);
      Btn.Parent := Tab;
      Btn.Caption := Row1[I];
      Fill.PlaceItem(Btn);
    end;

    for I := 0 to High(Row2) do
    begin
      Btn := TButton.Create(Tab);
      Btn.Parent := Tab;
      Btn.Caption := Row2[I];
      Fill.PlaceItem(Btn);
    end;

    Fill.Skip;

    for I := 0 to High(Row3) do
    begin
      Btn := TButton.Create(Tab);
      Btn.Parent := Tab;
      Btn.Caption := Row3[I];
      Fill.PlaceItem(Btn);
    end;

    Fill.Skip(2);

    // Espaço na última linha
    Btn := TButton.Create(Tab);
    Btn.Parent := Tab;
    Btn.Caption := 'Espaço';
    Btn.Font.Size := 12;

    Grid.AddItem(
      Btn,
      TGridCellSettings
        .Create(4, 2)
        .WithColumnSpan(6)
        .WithAlignment(laStretch, laStretch)
    );

    Grid.ArrangeItems;
  finally
    Grid.Free;
  end;
end;

procedure TFExamplesMain.AddExample_ControlPanel(AOwner: TForm;
  APageControl: TPageControl);
var
  Tab: TTabSheet;
  Grid: TGridLayout;
  BtnStart, BtnStop: TButton;
  LedOn, LedOff: TShape;
  SliderVolume, SliderBrightness: TTrackBar;
begin
  Tab := TTabSheet.Create(APageControl);
  Tab.PageControl := APageControl;
  Tab.Caption := 'Painel de Controle';

  Grid := TGridLayout.Create;
  try
    Grid.Rows := 3;
    Grid.Columns := 4;
    Grid.ColumnWidths := 100;
    Grid.RowHeights := 60;
    Grid.HorizontalSpacings := 15;
    Grid.VerticalSpacings := 15;
    Grid.Margins.All := 20;

    // Botão Start
    BtnStart := TButton.Create(Tab);
    BtnStart.Parent := Tab;
    BtnStart.Caption := 'Iniciar';
    Grid.AddItem(
      BtnStart,
      TGridCellSettings.Create(0, 0)
        .WithAlignment(laStretch, laStretch)
    );

    // Botão Stop
    BtnStop := TButton.Create(Tab);
    BtnStop.Parent := Tab;
    BtnStop.Caption := 'Parar';
    Grid.AddItem(
      BtnStop,
      TGridCellSettings.Create(0, 1)
        .WithAlignment(laStretch, laStretch)
    );

    // LED Ligado
    LedOn := TShape.Create(Tab);
    LedOn.Parent := Tab;
    LedOn.Shape := stCircle;
    LedOn.Brush.Color := clLime;
    LedOn.Width := 20;
    LedOn.Height := 20;
    Grid.AddItem(
      LedOn,
      TGridCellSettings.Create(1, 0)
        .WithAlignment(laCenter, laCenter)
    );

    // LED Desligado
    LedOff := TShape.Create(Tab);
    LedOff.Parent := Tab;
    LedOff.Shape := stCircle;
    LedOff.Brush.Color := clRed;
    LedOff.Width := 20;
    LedOff.Height := 20;
    Grid.AddItem(
      LedOff,
      TGridCellSettings.Create(1, 1)
        .WithAlignment(laCenter, laCenter)
    );

    // Slider Volume
    SliderVolume := TTrackBar.Create(Tab);
    SliderVolume.Parent := Tab;
    SliderVolume.Min := 0;
    SliderVolume.Max := 100;
    SliderVolume.Position := 50;
    Grid.AddItem(
      SliderVolume,
      TGridCellSettings.Create(2, 0)
        .WithColumnSpan(2)
        .WithAlignment(laStretch, laCenter)
    );

    // Slider Brilho
    SliderBrightness := TTrackBar.Create(Tab);
    SliderBrightness.Parent := Tab;
    SliderBrightness.Min := 0;
    SliderBrightness.Max := 100;
    SliderBrightness.Position := 75;
    Grid.AddItem(
      SliderBrightness,
      TGridCellSettings.Create(2, 2)
        .WithColumnSpan(2)
        .WithAlignment(laStretch, laCenter)
    );

    Grid.ArrangeItems;
  finally
    Grid.Free;
  end;
end;

procedure TFExamplesMain.AddExample_FormLayout(AOwner: TForm;
  APageControl: TPageControl);
var
  Tab: TTabSheet;
  Grid: TGridLayout;
  LblName, LblEmail, LblPhone, LblDOB: TLabel;
  EdtName, EdtEmail, EdtPhone, EdtDOB: TEdit;
  BtnSubmit: TButton;
begin
  Tab := TTabSheet.Create(APageControl);
  Tab.PageControl := APageControl;
  Tab.Caption := 'Formulário Dinâmico';

  Grid := TGridLayout.Create;
  try
    Grid.Rows := 5;
    Grid.Columns := 2;
    Grid.ColumnWidths := 200;
    Grid.ColumnWidth[1] := 500;
    Grid.RowHeights := 30;
    Grid.HorizontalSpacings := 10;
    Grid.VerticalSpacings := 10;
    Grid.Margins.All := 20;

    // Nome
    LblName := TLabel.Create(Tab);
    LblName.Parent := Tab;
    LblName.Caption := 'Nome:';
    Grid.AddItem(
      LblName,
      TGridCellSettings.Create(0, 0).WithAlignment(laEnd, laCenter)
    );

    EdtName := TEdit.Create(Tab);
    EdtName.Parent := Tab;
    Grid.AddItem(
      EdtName,
      TGridCellSettings.Create(0, 1).WithAlignment(laStretch, laCenter)
    );

    // Email
    LblEmail := TLabel.Create(Tab);
    LblEmail.Parent := Tab;
    LblEmail.Caption := 'Email:';
    Grid.AddItem(
      LblEmail,
      TGridCellSettings.Create(1, 0).WithAlignment(laEnd, laCenter)
    );

    EdtEmail := TEdit.Create(Tab);
    EdtEmail.Parent := Tab;
    Grid.AddItem(
      EdtEmail,
      TGridCellSettings.Create(1, 1).WithAlignment(laStretch, laCenter)
    );

    // Telefone
    LblPhone := TLabel.Create(Tab);
    LblPhone.Parent := Tab;
    LblPhone.Caption := 'Telefone:';
    Grid.AddItem(
      LblPhone,
      TGridCellSettings.Create(2, 0).WithAlignment(laEnd, laCenter)
    );

    EdtPhone := TEdit.Create(Tab);
    EdtPhone.Parent := Tab;
    Grid.AddItem(
      EdtPhone,
      TGridCellSettings.Create(2, 1).WithAlignment(laStretch, laCenter)
    );

    // Data de Nascimento
    LblDOB := TLabel.Create(Tab);
    LblDOB.Parent := Tab;
    LblDOB.Caption := 'Nascimento:';
    Grid.AddItem(
      LblDOB,
      TGridCellSettings.Create(3, 0).WithAlignment(laEnd, laCenter)
    );

    EdtDOB := TEdit.Create(Tab);
    EdtDOB.Parent := Tab;
    Grid.AddItem(
      EdtDOB,
      TGridCellSettings.Create(3, 1).WithAlignment(laStretch, laCenter)
    );

    // Botão de Enviar
    BtnSubmit := TButton.Create(Tab);
    BtnSubmit.Parent := Tab;
    BtnSubmit.Caption := 'Enviar';
    Grid.AddItem(
      BtnSubmit,
      TGridCellSettings.Create(4, 0)
        .WithColumnSpan(2)
        .WithAlignment(laCenter, laCenter)
    );

    Grid.ArrangeItems;
  finally
    Grid.Free;
  end;
end;

procedure TFExamplesMain.AddExample_WeeklyAgenda(AOwner: TForm;
  APageControl: TPageControl);
const
  DaysOfWeek: array[0..6] of string = (
    'Domingo', 'Segunda', 'Terça', 'Quarta', 'Quinta', 'Sexta', 'Sábado'
  );
  HourLabels: array[1..4] of string = ('08:00', '10:00', '14:00', '16:00');
var
  Tab: TTabSheet;
  Grid: TGridLayout;
  Col, Row: Integer;
  LabelDay, LabelHour: TLabel;
  Panel: TPanel;
begin
  Tab := TTabSheet.Create(APageControl);
  Tab.PageControl := APageControl;
  Tab.Caption := 'Agenda Semanal';

  Grid := TGridLayout.Create;
  try
    Grid.Rows := 5; // 1 linha para cabeçalho + 4 horários
    Grid.Columns := 8; // 1 coluna para horários + 7 dias da semana
    Grid.ColumnWidths := 100;
    Grid.RowHeights := 40;
    Grid.HorizontalSpacings := 4;
    Grid.VerticalSpacings := 4;
    Grid.Margins.All := 10;
    Grid.HorizontalSpacing[0] := 20;

    // Cabeçalho da primeira célula (vazia no topo-esquerdo)
    LabelDay := TLabel.Create(Tab);
    LabelDay.Parent := Tab;
    LabelDay.Caption := '';
    Grid.AddItem(
      LabelDay,
      TGridCellSettings.Create(0, 0)
        .WithAlignment(laCenter, laCenter)
    );

    // Cabeçalhos dos dias da semana
    for Col := 1 to 7 do
    begin
      LabelDay := TLabel.Create(Tab);
      LabelDay.Parent := Tab;
      LabelDay.Caption := DaysOfWeek[Col - 1];
      LabelDay.Font.Style := [fsBold];
      LabelDay.Alignment := taCenter;
      Grid.AddItem(
        LabelDay,
        TGridCellSettings.Create(0, Col)
          .WithAlignment(laCenter, laCenter)
      );
    end;

    // Linhas de horário
    for Row := 1 to 4 do
    begin
      // Primeira coluna: hora
      LabelHour := TLabel.Create(Tab);
      LabelHour.Parent := Tab;
      LabelHour.Caption := HourLabels[Row];
      LabelHour.Font.Style := [fsBold];
      Grid.AddItem(
        LabelHour,
        TGridCellSettings.Create(Row, 0)
          .WithAlignment(laEnd, laCenter)
      );

      // Colunas da semana (slots de agenda)
      for Col := 1 to 7 do
      begin
        Panel := TPanel.Create(Tab);
        Panel.Parent := Tab;
        Panel.Caption := '';
        Grid.AddItem(
          Panel,
          TGridCellSettings.Create(Row, Col)
            .WithAlignment(laStretch, laStretch)
        );
      end;
    end;

    Grid.ArrangeItems;
  finally
    Grid.Free;
  end;
end;

procedure TFExamplesMain.AddExample_NestedGridNoContainerSubGridBuilder
  (AOwner: TForm; APageControl: TPageControl);
var
  Tab: TTabSheet;
  I: Integer;
  Btns: array of TControl;
  Btn: TButton;
  Btn1, Btn2, Btn3: TButton;
const
  Rows = 3;
  Columns = 5;
  CWidht = 70;
  Spacing = 3;
  RHeight = 40;

  SubGridRow = 0;
  SubGridRows = 1;
  SubGridColumns = 3;
  SubGridSpacing = 10;
  SubGridRHeight = RHeight * 2;
begin
  Tab := TTabSheet.Create(APageControl);
  Tab.PageControl := APageControl;
  Tab.Caption := 'GridFill';

  SetLength(Btns, 10);
  for I := Low(Btns) to High(Btns) do
  begin
    Btn := TButton.Create(Tab);
    Btn.Parent := Tab;
    Btn.Caption := Format('Btn %d', [I + 1]);
    Btn.Tag := I;
    Btns[I] := Btn;
  end;

  Btn1 := TButton.Create(Tab);
  Btn1.Parent := Tab;
  Btn1.Caption := ' SUB-GRID A ';

  Btn2 := TButton.Create(Tab);
  Btn2.Parent := Tab;
  Btn2.Caption := ' SUB-GRID B ';

  Btn3 := TButton.Create(Tab);
  Btn3.Parent := Tab;
  Btn3.Caption := ' SUB-GRID C ';

  with TGridLayoutBuilder.Create
    .WithDimensions(Rows, Columns)
    .WithRowsHeight(RHeight)
    .WithColumnsWidth(CWidht)
    .WithSpacings(Spacing)
    .WithMargins(10)
    .WithRowsHeight(
      [SubGridRow],
      SubGridRows*SubGridRHeight
      + (SubGridRows-1)*SubGridSpacing
    )
    .BeginSubGrid(
      TGridCellSettings.Create(SubGridRow, 0)
        .WithColumnSpan(Columns)
    )
      .WithDimensions(SubGridRows, SubGridColumns)
      .WithSpacings(SubGridSpacing)
      .WithRowsHeight(SubGridRHeight)
      .WithColumnsWidth((
        CWidht*Columns
          + Spacing*(Columns-1)
          - SubGridSpacing*(SubGridColumns-1)
        ) div SubGridColumns
      )
      .UsingFiller(ftColumnFirst)
      .FillItems([Btn1, Btn2, Btn3])
    .EndSubGrid
    .UsingFiller(ftRowFirst)
    .FillItems(Btns)
    .Done do
  begin
    ArrangeItems;
    Free;
  end;
end;

procedure TFExamplesMain.AddExample_Dashboard(AOwner: TForm;
  APageControl: TPageControl);
var
  Tab: TTabSheet;
  Grid: TGridLayout;
  Card1, Card2, Card3, ChartPanel: TPanel;
  LogMemo: TMemo;
begin
  Tab := TTabSheet.Create(APageControl);
  Tab.PageControl := APageControl;
  Tab.Caption := 'Dashboard';

  Grid := TGridLayout.Create;
  try
    Grid.Rows := 3;
    Grid.Columns := 3;
    Grid.ColumnWidths := 200;
    Grid.RowHeights := 120;
    Grid.HorizontalSpacings := 8;
    Grid.VerticalSpacings := 8;
    Grid.Margins.All := 10;

    // Cartão 1 - Total de Vendas
    Card1 := TPanel.Create(Tab);
    Card1.Parent := Tab;
    Card1.WordWrap:=True;
    Card1.Caption := '💰 Total de Vendas: '+#13+'R$ 123.456';
    Card1.Font.Size := 10;
    Card1.Font.Style := [fsBold];
    Grid.AddItem(
      Card1,
      TGridCellSettings.Create(0, 0)
        .WithAlignment(laStretch, laStretch)
    );

    // Cartão 2 - Clientes Ativos
    Card2 := TPanel.Create(Tab);
    Card2.Parent := Tab;
    Card2.WordWrap:=True;
    Card2.Caption := '👥 Clientes Ativos: '+#13+'350';
    Card2.Font.Size := 10;
    Card2.Font.Style := [fsBold];
    Grid.AddItem(
      Card2,
      TGridCellSettings.Create(0, 1)
        .WithAlignment(laStretch, laStretch)
    );

    // Cartão 3 - Pedidos Pendentes
    Card3 := TPanel.Create(Tab);
    Card3.Parent := Tab;
    Card3.WordWrap:=True;
    Card3.Caption := '📦 Pedidos Pendentes: '+#13+'24';
    Card3.Font.Size := 10;
    Card3.Font.Style := [fsBold];
    Grid.AddItem(
      Card3,
      TGridCellSettings.Create(0, 2)
        .WithAlignment(laStretch, laStretch)
    );

    // Gráfico (fictício)
    ChartPanel := TPanel.Create(Tab);
    ChartPanel.Parent := Tab;
    ChartPanel.WordWrap:=True;
    ChartPanel.Caption := '📈 Gráfico de '+#13+'desempenho';
    ChartPanel.Font.Size := 9;
    ChartPanel.Font.Style := [fsBold];
    Grid.AddItem(
      ChartPanel,
      TGridCellSettings.Create(1, 0)
        .WithColumnSpan(2)
        .WithRowSpan(2)
        .WithAlignment(laStretch, laStretch)
    );

    // Log ou Atividades Recentes
    LogMemo := TMemo.Create(Tab);
    LogMemo.Parent := Tab;
    LogMemo.Lines.Text := '• Venda confirmada'#13'• Novo cliente registrado'#13'• Pedido cancelado';
    Grid.AddItem(
      LogMemo,
      TGridCellSettings.Create(1, 2)
        .WithRowSpan(2)
        .WithAlignment(laStretch, laStretch)
    );

    Grid.ArrangeItems;
  finally
    Grid.Free;
  end;
end;

procedure TFExamplesMain.AddExample_ReportGrid(AOwner: TForm; APageControl: TPageControl);
const
  ColProduct = 0;
  ColQty = 1;
  ColPrice = 2;
  ColTotal = 3;
var
  Tab: TTabSheet;
  Grid: TGridLayout;
  procedure AddCell(const AText: string; ARow, ACol: Integer; ABold: Boolean = False; AAlignH: TLayoutAlignment = laStart; AAlignV: TLayoutAlignment = laCenter);
  var
    Lbl: TLabel;
  begin
    Lbl := TLabel.Create(Tab);
    Lbl.Parent := Tab;
    Lbl.Caption := AText;
    if ABold then
      Lbl.Font.Style := [fsBold];

    Grid.AddItem(
      Lbl,
      TGridCellSettings.Create(ARow, ACol)
        .WithAlignment(AAlignH, AAlignV)
    );
  end;

  procedure AddRow(AIdx: Integer; const AProduct: string; AQty: Integer; APrice: Currency);
  var
    Total: Currency;
  begin
    Total := AQty * APrice;
    AddCell(AProduct, AIdx, ColProduct);
    AddCell(IntToStr(AQty), AIdx, ColQty, False, laEnd);
    AddCell(FormatFloat('0.00', APrice), AIdx, ColPrice, False, laEnd);
    AddCell(FormatFloat('0.00', Total), AIdx, ColTotal, False, laEnd);
  end;

var
  Row: Integer;
  GrandTotal: Currency;
begin
  Tab := TTabSheet.Create(APageControl);
  Tab.PageControl := APageControl;
  Tab.Caption := 'Relatório de Vendas';

  Grid := TGridLayout.Create;
  try
    Grid.Columns := 4;
    Grid.ColumnWidths := 120;
    Grid.RowHeights := 30;
    Grid.VerticalSpacings := 4;
    Grid.HorizontalSpacings := 10;
    Grid.Margins.All := 10;

    // Cabeçalhos
    AddCell('Produto', 0, ColProduct, True);
    AddCell('Qtd',     0, ColQty, True, laEnd);
    AddCell('Preço',   0, ColPrice, True, laEnd);
    AddCell('Total',   0, ColTotal, True, laEnd);

    Row := 1;
    GrandTotal := 0;

    // Linhas de exemplo
    AddRow(Row, 'Maçã', 3, 2.5);
    Inc(Row);
    GrandTotal := GrandTotal + 3 * 2.5;

    AddRow(Row, 'Banana', 5, 1.8);
    Inc(Row);
    GrandTotal := GrandTotal + 5 * 1.8;

    AddRow(Row, 'Uva', 2, 3.2);
    Inc(Row);
    GrandTotal := GrandTotal + 2 * 3.2;

    // Linha Total
    AddCell('Total Geral:', Row, 0, True);
    Grid.AddItem(ILayoutItem(nil), TGridCellSettings.Create(Row, 1).WithColumnSpan(2)); // espaço vazio
    AddCell(FormatFloat('0.00', GrandTotal), Row, 3, True, laEnd);

    Grid.ArrangeItems;
  finally
    Grid.Free;
  end;
end;

procedure TFExamplesMain.AddExample_Spreadsheet(AOwner: TForm; APageControl: TPageControl);
const
  ColCount = 6;
  RowCount = 10;
var
  Tab: TTabSheet;
  Grid: TGridLayout;
  Col, Row: Integer;

  procedure AddCell(ARow, ACol: Integer; const AText: string; ABold: Boolean = False; AColor: TColor = clNone);
  var
    Lbl: TLabel;
  begin
    Lbl := TLabel.Create(Tab);
    Lbl.Parent := Tab;
    Lbl.Caption := AText;
    Lbl.Alignment := taCenter;
    Lbl.Layout := tlCenter;
    Lbl.AutoSize := False;
    Lbl.Width := 80;
    Lbl.Height := 30;
    if ABold then
      Lbl.Font.Style := [fsBold];
    if AColor <> clNone then
      Lbl.Color := AColor
    else
      Lbl.Color := clBtnFace;
    Lbl.Transparent := False;

    Grid.AddItem(
      Lbl,
      TGridCellSettings.Create(ARow, ACol)
        .WithAlignment(laStretch, laStretch)
    );
  end;

begin
  Tab := TTabSheet.Create(APageControl);
  Tab.PageControl := APageControl;
  Tab.Caption := 'Planilha';

  Grid := TGridLayout.Create;
  try
    Grid.Columns := ColCount + 1; // +1 para a coluna dos números de linha
    Grid.Rows := RowCount + 1;    // +1 para o cabeçalho
    Grid.ColumnWidths := 80;
    Grid.RowHeights := 30;
    Grid.VerticalSpacings := 2;
    Grid.HorizontalSpacings := 2;
    Grid.Margins.All := 10;

    // Cabeçalho de colunas (A, B, C, ...)
    for Col := 1 to ColCount do
      AddCell(0, Col, Chr(64 + Col), True, clSilver);

    // Cabeçalho de linhas (1, 2, 3, ...)
    for Row := 1 to RowCount do
      AddCell(Row, 0, IntToStr(Row), True, clSilver);

    // Conteúdo das células
    for Row := 1 to RowCount do
      for Col := 1 to ColCount do
        AddCell(Row, Col, Format('R%dC%d', [Row, Col]));

    Grid.ArrangeItems;
  finally
    Grid.Free;
  end;
end;

procedure TFExamplesMain.AddExample_SpreadsheetEditable(AOwner: TForm; APageControl: TPageControl);
const
  ColCount = 6;
  RowCount = 10;
var
  Tab: TTabSheet;
  Grid: TGridLayout;
  Col, Row: Integer;

  procedure AddHeaderCell(ARow, ACol: Integer; const AText: string);
  var
    Lbl: TLabel;
  begin
    Lbl := TLabel.Create(Tab);
    Lbl.Parent := Tab;
    Lbl.Caption := AText;
    Lbl.Alignment := taCenter;
    Lbl.Layout := tlCenter;
    Lbl.AutoSize := False;
    Lbl.Font.Style := [fsBold];
    Lbl.Color := clSilver;
    Lbl.Transparent := False;
    Lbl.Width := 80;
    Lbl.Height := 30;

    Grid.AddItem(
      Lbl,
      TGridCellSettings.Create(ARow, ACol)
        .WithAlignment(laStretch, laStretch)
    );
  end;

  procedure AddEditableCell(ARow, ACol: Integer);
  var
    Edt: TEdit;
  begin
    Edt := TEdit.Create(Tab);
    Edt.Parent := Tab;
    Edt.Text := Format('%d,%d', [ARow, ACol]);
    Edt.Width := 80;
    Edt.Height := 30;
    Edt.BorderStyle := bsSingle;

    Grid.AddItem(
      Edt,
      TGridCellSettings.Create(ARow, ACol)
        .WithAlignment(laStretch, laStretch)
    );
  end;

begin
  Tab := TTabSheet.Create(APageControl);
  Tab.PageControl := APageControl;
  Tab.Caption := 'Planilha Editável';

  Grid := TGridLayout.Create;
  try
    Grid.Columns := ColCount + 1;
    Grid.Rows := RowCount + 1;
    Grid.ColumnWidths := 80;
    Grid.RowHeights := 30;
    Grid.HorizontalSpacings := 1;
    Grid.VerticalSpacings := 1;
    Grid.Margins.All := 10;

    // Cabeçalho das colunas (A, B, C...)
    for Col := 1 to ColCount do
      AddHeaderCell(0, Col, Chr(64 + Col));

    // Cabeçalho das linhas (1, 2, 3...)
    for Row := 1 to RowCount do
      AddHeaderCell(Row, 0, IntToStr(Row));

    // Células editáveis
    for Row := 1 to RowCount do
      for Col := 1 to ColCount do
        AddEditableCell(Row, Col);

    Grid.ArrangeItems;
  finally
    Grid.Free;
  end;
end;

procedure TFExamplesMain.AddExample_NestedGrid(AOwner: TForm; APageControl: TPageControl);
var
  Tab: TTabSheet;
  MainGrid, SubGrid: TGridLayout;
  SubItem: TSubGridLayoutItem;
  BtnMain, Btn: TButton;
  I: Integer;
begin
  Tab := TTabSheet.Create(APageControl);
  Tab.PageControl := APageControl;
  Tab.Caption := 'Nested Grid';

  // Grid principal
  MainGrid := TGridLayout.Create;
  try
    MainGrid.Rows := 2;
    MainGrid.Columns := 2;
    MainGrid.RowHeights := 80;
    MainGrid.ColumnWidths := 250;
    MainGrid.Margins.All := 10;

    // Botão normal
    BtnMain := TButton.Create(Tab);
    BtnMain.Parent := Tab;
    BtnMain.Caption := 'Principal 1';
    MainGrid.AddItem(BtnMain, TGridCellSettings.Create(0, 0));

    try
      // Subgrid configurado
      SubGrid := TGridLayout.Create;
      SubGrid.Rows := 3;
      SubGrid.Columns := 3;
      SubGrid.ColumnWidths := 50;
      SubGrid.RowHeights := 30;
      SubGrid.HorizontalSpacings := 5;
      SubGrid.Margins.All := 10;

      // Criando SubGrid
      SubItem := TSubGridLayoutItem.CreateWithContainerClass(
        SubGrid,
        Tab,
        TGroupBox
      );

      SubItem.Container.Parent := Tab;
      TGroupBox(SubItem.Container).Caption := 'Item do Grid Aninhado';
      SubItem.Container.Color := clYellow;

      for I := 0 to 8 do
      begin
        Btn := TButton.Create(Tab);
        Btn.Parent := SubItem.Container;
        Btn.Caption := IntToStr(I + 1);
        SubGrid.AddItem(Btn, TGridCellSettings.Create(I div 3, I mod 3));
      end;

      SubGrid.ArrangeItems; //quando o subgrid tem seu proprio container, precisa chamar ArrangeItems Manualmente.

      SubItem.Container.Height := SubGrid.ContentHeight + 25;
      SubItem.Container.Width := SubGrid.ContentWidth;

      // Adicionando subgrid ao grid principal
      MainGrid.AddItem(
        SubItem,
        TGridCellSettings.Create(0, 1)
          .WithRowSpan(2)
          .WithAlignment(laCenter, laCenter)
      );
    finally
      //SubGrid.Free;
    end;

    // Outro botão normal
    BtnMain := TButton.Create(Tab);
    BtnMain.Parent := Tab;
    BtnMain.Caption := 'Principal 2';
    MainGrid.AddItem(BtnMain, TGridCellSettings.Create(1, 0));

    MainGrid.ArrangeItems;
  finally
    MainGrid.Free;
  end;
end;

procedure TFExamplesMain.AddExample_NestedGridNoContainer(AOwner: TForm;
  APageControl: TPageControl);
var
  Tab: TTabSheet;
  MainGrid, SubGrid: TGridLayout;
  SubItem: TSubGridLayoutItem;
  BtnMain, Btn: TButton;
  I: Integer;
begin
  Tab := TTabSheet.Create(APageControl);
  Tab.PageControl := APageControl;
  Tab.Caption := 'Nested Grid';

  // Grid principal
  MainGrid := TGridLayout.Create;
  try
    MainGrid.Rows := 2;
    MainGrid.Columns := 2;
    MainGrid.RowHeights := 80;
    MainGrid.ColumnWidths := 250;
    MainGrid.Margins.All := 10;

    // Botão normal
    BtnMain := TButton.Create(Tab);
    BtnMain.Parent := Tab;
    BtnMain.Caption := 'Principal 1';
    MainGrid.AddItem(BtnMain, TGridCellSettings.Create(0, 0));

    try
      // Subgrid configurado
      SubGrid := TGridLayout.Create;
      SubGrid.Rows := 3;
      SubGrid.Columns := 3;
      SubGrid.ColumnWidths := 50;
      SubGrid.RowHeights := 30;
      SubGrid.HorizontalSpacings := 5;
      SubGrid.Margins.All := 10;

      // Criando SubGrid
      SubItem := TSubGridLayoutItem.Create(SubGrid);

      for I := 0 to 8 do
      begin
        Btn := TButton.Create(Tab);
        Btn.Parent := Tab; // Importante!
        Btn.Caption := IntToStr(I + 1);
        SubGrid.AddItem(Btn, TGridCellSettings.Create(I div 3, I mod 3));
      end;

      SubItem.Container.Height := SubGrid.ContentHeight;
      SubItem.Container.Width := SubGrid.ContentWidth;

      // não chamar ArrangeItems aqui no sub grid.
      // No ArrangeItems do Main será chamado
      // pois qualquer chamada agora não conseguirá
      // determinar onde a célula será posicionada.
      // a celula só terá sua posição definida no ArrangeItem
      // do MainGrid.

      //SubGrid.ArrangeItems;

      // Adicionando subgrid ao grid principal
      MainGrid.AddItem(
        SubItem,
        TGridCellSettings.Create(0, 1)
          .WithRowSpan(2)
          .WithAlignment(laCenter, laCenter)
      );
    finally
      // não chamar Free para subGrids.
      // SubGrid´s são destruídos junto ao Main
      //SubGrid.Free;
    end;

    // Outro botão normal
    BtnMain := TButton.Create(Tab);
    BtnMain.Parent := Tab;
    BtnMain.Caption := 'Principal 2';
    MainGrid.AddItem(BtnMain, TGridCellSettings.Create(1, 0));

    MainGrid.ArrangeItems;
  finally
    MainGrid.Free;  // aqui também está destruíndo subGrid
  end;
end;

procedure TFExamplesMain.AddExample_BasicGridLayout2(AOwner: TForm;
  APageControl: TPageControl);
var
  Tab: TTabSheet;
  Grid, SubGrid: TGridLayout;
  I: Integer;
  Btn: TButton;
begin
  Tab := TTabSheet.Create(PageControl);
  Tab.PageControl := PageControl;
  Tab.Caption := 'Grid Simples';

  Grid := TGridLayout.Create;
  try
    Grid.Rows := 3;
    Grid.Columns := 3;
    Grid.ColumnWidths := 80;
    Grid.RowHeights := 50;
    Grid.HorizontalSpacings := 10;
    Grid.VerticalSpacings := 10;
    Grid.Margins.All := 10;

    SubGrid := TGridLayout.Create;
    SubGrid.Rows := 1;
    SubGrid.Columns := 2;
    SubGrid.ColumnWidths := 125;
    SubGrid.RowHeights := 50;
    SubGrid.HorizontalSpacings := 10;
    SubGrid.VerticalSpacings := 10;
    SubGrid.Margins.All := 0;

    Btn := TButton.Create(Tab);
    Btn.Parent := Tab;
    Btn.Caption := 'A';
    SubGrid.AddItem(Btn, TGridCellSettings.Create(0, 0));

    Btn := TButton.Create(Tab);
    Btn.Parent := Tab;
    Btn.Caption := 'B';
    SubGrid.AddItem(Btn, TGridCellSettings.Create(0, 1));

    Grid.AddItem(
      SubGrid,
      TGridCellSettings.Create(0, 0)
        .WithColumnSpan(3)
    );

    for I := 3 to 8 do
    begin
      Btn := TButton.Create(Tab);
      Btn.Parent := Tab;
      Btn.Caption := 'Botão ' + IntToStr(I + 1);

      Grid.AddItem(Btn, TGridCellSettings.Create(I div 3, I mod 3));
    end;

    Grid.ArrangeItems;
  finally
    Grid.Free;
  end;
end;

procedure TFExamplesMain.AddExample_BasicGridLayoutBuilder(AOwner: TForm;
  APageControl: TPageControl);
var
  Tab: TTabSheet;
  Grid: TGridLayout;
  I: Integer;
  Btn: TButton;
begin
  Tab := TTabSheet.Create(PageControl);
  Tab.PageControl := PageControl;
  Tab.Caption := 'Builder Simples';

  Grid := TGridLayoutBuilder.Create
    .WithDimensions(3, 3)
    .WithColumnsWidth(80)
    .WithRowsHeight(50)
    .WithHorizontalSpacings(10)
    .WithVerticalSpacings(10)
    .WithMargins(10)
    .Done;

  for I := 0 to 8 do
  begin
    Btn := TButton.Create(Tab);
    Btn.Parent := Tab;
    Btn.Caption := 'Botão ' + IntToStr(I + 1);

    Grid.AddItem(Btn, TGridCellSettings.Create(I div 3, I mod 3));
  end;

  Grid.ArrangeItems;
end;

{$R *.lfm}

end.

