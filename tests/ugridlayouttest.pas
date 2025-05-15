unit UGridLayoutTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, ULayout;

type
  TSubGridItemTest = class (TSubGridItem)
  end;

  { TGridLayoutTest }

  TGridLayoutTest= class(TTestCase)
  private
  protected
  published
    procedure TestPosicaoComponente;
    procedure TestPosicaoComponente_ComRowShift;
    procedure TestPosicaoComponente_ComColumnShift;
    procedure TestPosicaoComponente_ComHorizontalSpacing;
    procedure TestPosicaoComponente_ComVerticalSpacing;
    procedure TestPosicaoComponente_ComEspacamentoHorizontalEVertical;
    procedure TestPosicaoComponente_ComEspacamentoCustomizado;
    procedure TestPosicaoComponente_ComMargens;
    procedure TestPosicaoComponente_ComTopLeftGrid;
    procedure TestPosicaoComponente_ComColunaOculta;
    procedure TestPosicaoComponente_ComLinhaOculta;
    procedure TestTamanhoComponente_ComColSpanExcedeUltimaColuna;
    procedure TestTamanhoComponente_ComRowSpanExcedeUltimaLinha;

    procedure TestTamanhoComponente_ComRowColSpan_EspacamentoCustomizado;
    procedure TestTamanhoComponente;
    procedure TestTamanhoComponente_ComLarguraEAlturaVariaveis;
    procedure TestTamanhoComponente_ComRowSpan;
    procedure TestTamanhoComponente_ComColumnSpan;
    procedure TestTamanhoComponente_ComColumnSpan_ComColunaOculta;
    procedure TestTamanhoComponente_ComRowSpanEColumnSpan;


    procedure TestVisibilidadeComponente_ComColunaOculta;
    procedure TestVisibilidadeComponente_AdicionadoForaDosLimitesDoGrid;
    procedure TestVisibilidadeComponente_AdicionadoDentroDosLimitesDoGrid;


    procedure TestMargensAfetamContentSize;
    procedure TestMargensComMultiplasCelulas;
    procedure TestMargensComRowSpanEColSpan;
    procedure TestMargensEContentSizeComMultiplasCelulas;

    procedure TestOffsetSimples;
    procedure TestOffsetComMargensEGrid;
    procedure TestOffsetComSpan;

    procedure TestLayoutAlignmentHorizontal;
    procedure TestLayoutAlignmentVertical;
    procedure TestLayoutVerticalAndHorizontaAlignment;
    procedure TestLayoutAlignmentVertical_ComRowSpan;
    procedure TestComponenteMaiorQueCelula_RespeitaTamanhoComAlignment;
    procedure TestExtraSize_StretchOnly;

    procedure TestExtraSize_NoEffectOnNonStretch;
    procedure TestExtraSize_WithOffset_StretchOnly;

    procedure TestNestedGrid_PositionAndSize;
    procedure TestSubGrid_WithVirtualContainer_PositionedCorrectly;

    procedure TestInsertAt_TGridTrackInfoDictionaryHelper;
    procedure TestMoveKey_From1To3;
    procedure TestMoveKey_From3To1;
    procedure TestMoveKey_From3To1_DontExistFrom;
  end;

implementation

uses
  Buttons, Controls, StdCtrls, ExtCtrls;

procedure TGridLayoutTest.TestPosicaoComponente;
(*
Grade esperada (cada célula tem 10px de largura e 20px de altura):

  Colunas →
  0     1      2      3
+------+------+------+------+
| (0,0)|      |      |      | ← Row 0 (Top = 0)
+------+------+------+------+
|      |(1,1) |      |      | ← Row 1 (Top = 20)
+------+------+------+------+
|      |      |(2,2) | (2,3)| ← Row 2 (Top = 40)
+------+------+------+------+

Posições esperadas:
(0,0) → Left = 0,  Top = 0
(1,1) → Left = 10, Top = 20
(2,2) → Left = 20, Top = 40
(2,3) → Left = 30, Top = 40
*)
type
  TCaso = record
    Row, Col,
    LeftExpected, TopExpected: Integer;
  end;
var
  FGrid: TGridLayout;
  SB: TSpeedButton;
  I: Integer;
const
  Casos: array[0..3] of TCaso = (
    (Row: 0; Col: 0; LeftExpected: 0; TopExpected: 0),
    (Row: 1; Col: 1; LeftExpected: 10; TopExpected: 20),
    (Row: 2; Col: 2; LeftExpected: 20; TopExpected: 40),
    (Row: 2; Col: 3; LeftExpected: 30; TopExpected: 40)
  );
begin
  for I := Low(Casos) to High(Casos) do
  begin
    FGrid := TGridLayout.Create;
    SB := TSpeedButton.Create(nil);

    FGrid.Rows := 3;
    FGrid.Columns := 4;
    FGrid.ColumnWidths := 10;
    FGrid.RowHeights := 20;

    try
      FGrid.AddItem(
        TControlGridItem.Create(SB),
        TGridCellSettings.Create(Casos[I].Row, Casos[I].Col)
      );

      FGrid.ArrangeItems;
      AssertEquals(
        Format(
          'LeftExpected: Erro no caso %d: Posição(%d, %d)',
          [I, Casos[I].Row, Casos[I].Col]
        ),
        Casos[I].LeftExpected,
        SB.Left
      );

      AssertEquals(
        Format(
          'TopExpected: Erro no caso %d: Posição(%d, %d)',
          [I, Casos[I].Row, Casos[I].Col]
        ),
        Casos[I].TopExpected,
        SB.Top
      );
    finally
      FGrid.Free;
      SB.Free;
    end;
  end;
end;

procedure TGridLayoutTest.TestVisibilidadeComponente_ComColunaOculta;
type
  TCaso = record
    HiddenCol,
    Row, Col: Integer;
    VisibilityExpected: Boolean;
  end;
var
  FGrid: TGridLayout;
  SB: TSpeedButton;
  I: Integer;
const
  Casos: array[0..3] of TCaso = (
    (HiddenCol: 0; Row: 0; Col: 0; VisibilityExpected: False),
    (HiddenCol: 0; Row: 1; Col: 1; VisibilityExpected: True),
    (HiddenCol: 2; Row: 2; Col: 2; VisibilityExpected: False),
    (HiddenCol: 3; Row: 2; Col: 3; VisibilityExpected: False)
  );
begin
  for I := Low(Casos) to High(Casos) do
  begin
    FGrid := TGridLayout.Create;
    SB := TSpeedButton.Create(nil);

    FGrid.Rows := 3;
    FGrid.Columns := 4;
    FGrid.ColumnWidths := 10;
    FGrid.RowHeights := 20;
    FGrid.VisibleColumn[Casos[I].HiddenCol] := False;

    try
      FGrid.AddItem(
        TControlGridItem.Create(SB),
        TGridCellSettings.Create(Casos[I].Row, Casos[I].Col)
      );

      FGrid.ArrangeItems;
      AssertEquals(
        Format(
          'VisibilityExpected: Erro no caso %d: Posição(%d, %d)',
          [I, Casos[I].Row, Casos[I].Col]
        ),
        Casos[I].VisibilityExpected,
        SB.Visible
      );
    finally
      FGrid.Free;
      SB.Free;
    end;
  end;
end;


procedure TGridLayoutTest.TestPosicaoComponente_ComLinhaOculta;
type
  TCaso = record
    HiddenRow,
    Row, Col,
    LeftExpected, TopExpected: Integer;
  end;
var
  FGrid: TGridLayout;
  SB: TSpeedButton;
  I: Integer;
const
  Casos: array[0..3] of TCaso = (
    (HiddenRow: 1; Row: 0; Col: 0; LeftExpected: 0; TopExpected: 0),
    (HiddenRow: 0; Row: 1; Col: 1; LeftExpected: 10; TopExpected: 0),
    (HiddenRow: 1; Row: 2; Col: 2; LeftExpected: 20; TopExpected: 20),
    (HiddenRow: 1; Row: 2; Col: 3; LeftExpected: 30; TopExpected: 20)
  );
begin
  for I := Low(Casos) to High(Casos) do
  begin
    FGrid := TGridLayout.Create;
    SB := TSpeedButton.Create(nil);

    FGrid.Rows := 3;
    FGrid.Columns := 4;
    FGrid.ColumnWidths := 10;
    FGrid.RowHeights := 20;
    FGrid.VisibleRow[Casos[I].HiddenRow] := False;

    try
      FGrid.AddItem(
        TControlGridItem.Create(SB),
        TGridCellSettings.Create(Casos[I].Row, Casos[I].Col)
      );

      FGrid.ArrangeItems;
      AssertEquals(
        Format(
          'LeftExpected: Erro no caso %d: Posição(%d, %d)',
          [I, Casos[I].Row, Casos[I].Col]
        ),
        Casos[I].LeftExpected,
        SB.Left
      );

      AssertEquals(
        Format(
          'TopExpected: Erro no caso %d: Posição(%d, %d)',
          [I, Casos[I].Row, Casos[I].Col]
        ),
        Casos[I].TopExpected,
        SB.Top
      );
    finally
      FGrid.Free;
      SB.Free;
    end;
  end;
end;

procedure TGridLayoutTest.TestPosicaoComponente_ComColunaOculta;
type
  TCaso = record
    HiddenCol,
    Row, Col,
    LeftExpected, TopExpected: Integer;
  end;
var
  FGrid: TGridLayout;
  SB: TSpeedButton;
  I: Integer;
const
  Casos: array[0..3] of TCaso = (
    (HiddenCol: 1; Row: 0; Col: 0; LeftExpected: 0; TopExpected: 0),
    (HiddenCol: 0; Row: 1; Col: 1; LeftExpected: 0; TopExpected: 20),
    (HiddenCol: 1; Row: 2; Col: 2; LeftExpected: 10; TopExpected: 40),
    (HiddenCol: 2; Row: 2; Col: 3; LeftExpected: 20; TopExpected: 40)
  );
begin
  for I := Low(Casos) to High(Casos) do
  begin
    FGrid := TGridLayout.Create;
    SB := TSpeedButton.Create(nil);

    FGrid.Rows := 3;
    FGrid.Columns := 4;
    FGrid.ColumnWidths := 10;
    FGrid.RowHeights := 20;
    FGrid.VisibleColumn[Casos[I].HiddenCol] := False;

    try
      FGrid.AddItem(
        TControlGridItem.Create(SB),
        TGridCellSettings.Create(Casos[I].Row, Casos[I].Col)
      );

      FGrid.ArrangeItems;
      AssertEquals(
        Format(
          'LeftExpected: Erro no caso %d: Posição(%d, %d)',
          [I, Casos[I].Row, Casos[I].Col]
        ),
        Casos[I].LeftExpected,
        SB.Left
      );

      AssertEquals(
        Format(
          'TopExpected: Erro no caso %d: Posição(%d, %d)',
          [I, Casos[I].Row, Casos[I].Col]
        ),
        Casos[I].TopExpected,
        SB.Top
      );
    finally
      FGrid.Free;
      SB.Free;
    end;
  end;
end;

procedure TGridLayoutTest.TestPosicaoComponente_ComTopLeftGrid;
type
  TCaso = record
    Top, Left,   // Top e Left do Grid
    Row, Col,
    LeftExpected, TopExpected: Integer;
  end;
var
  FGrid: TGridLayout;
  SB: TSpeedButton;
  I: Integer;
const
  Casos: array[0..4] of TCaso = (
    (Top: 0; Left: 0; Row: 0; Col: 0; LeftExpected: 0; TopExpected: 0),
    (Top: 5; Left: 7; Row: 1; Col: 1; LeftExpected: 17; TopExpected: 25),
    (Top: -10; Left: -8; Row: 2; Col: 2; LeftExpected: 12; TopExpected: 30),
    (Top: 5; Left: 0; Row: 2; Col: 3; LeftExpected: 30; TopExpected: 45),
    (Top: 0; Left: 5; Row: 2; Col: 3; LeftExpected: 35; TopExpected: 40)
  );
begin
  for I := Low(Casos) to High(Casos) do
  begin
    FGrid := TGridLayout.Create;
    SB := TSpeedButton.Create(nil);

    FGrid.Rows := 3;
    FGrid.Top := Casos[I].Top;
    FGrid.Left := Casos[I].Left;
    FGrid.Columns := 4;
    FGrid.ColumnWidths := 10;
    FGrid.RowHeights := 20;

    try
      FGrid.AddItem(
        TControlGridItem.Create(SB),
        TGridCellSettings.Create(Casos[I].Row, Casos[I].Col)
      );

      FGrid.ArrangeItems;
      AssertEquals(
        Format(
          'LeftExpected: Erro no caso %d: Posição(%d, %d)',
          [I, Casos[I].Row, Casos[I].Col]
        ),
        Casos[I].LeftExpected,
        SB.Left
      );

      AssertEquals(
        Format(
          'TopExpected: Erro no caso %d: Posição(%d, %d)',
          [I, Casos[I].Row, Casos[I].Col]
        ),
        Casos[I].TopExpected,
        SB.Top
      );
    finally
      FGrid.Free;
      SB.Free;
    end;
  end;
end;

procedure TGridLayoutTest.TestPosicaoComponente_ComRowShift;
(*
Grade esperada com RowShift (colunas com largura 10px, linhas com altura 20px)
RowShift[0] = 5
RowShift[2] = 7

Visualização:

  Colunas →
   0      1      2      3
+------+------+------+------+
| (0,0)|      |      |      | --> Row 0 (Left = 0 + Shift = 5) = 5
+------+------+------+------+
|      |(1,1) |      |      | --> Row 1 (Left = 10) *sem Shift
+------+------+------+------+
|      |      |(2,2) | (2,3)|
+------+------+------+------+
                 |       |------> Row 2 (left = 30 + Shift = 7) = 37
                 |
                 |--------------> Row 2 (left = 20 + Shift = 7) = 27

Posições esperadas:
(0,0) → Left = 5,  Top = 0
(1,1) → Left = 10, Top = 20
(2,2) → Left = 27, Top = 40
(2,3) → Left = 37, Top = 40
*)
type
  TCaso = record
    Row,
    Col,
    LeftExpected,
    TopExpected: Integer;
  end;
var
  FGrid: TGridLayout;
  SB: TSpeedButton;
  I: Integer;
const
  Casos: array[0..3] of TCaso = (
    (Row: 0; Col: 0; LeftExpected: 5; TopExpected: 0),
    (Row: 1; Col: 1; LeftExpected: 10; TopExpected: 20),
    (Row: 2; Col: 2; LeftExpected: 27; TopExpected: 40),
    (Row: 2; Col: 3; LeftExpected: 37; TopExpected: 40)
  );
begin
  for I := Low(Casos) to High(Casos) do
  begin
    FGrid := TGridLayout.Create;
    SB := TSpeedButton.Create(nil);

    FGrid.Rows := 3;
    FGrid.Columns := 4;
    FGrid.ColumnWidths := 10;
    FGrid.RowHeights := 20;
    FGrid.RowShift[0] := 5;
    FGrid.RowShift[2] := 7;

    try
      FGrid.AddItem(
        TControlGridItem.Create(SB),
        TGridCellSettings.Create(Casos[I].Row, Casos[I].Col)
      );

      FGrid.ArrangeItems;
      AssertEquals(
        Format(
          'LeftExpected: Erro no caso %d: Posição(%d, %d)',
          [I, Casos[I].Row, Casos[I].Col]
        ),
        Casos[I].LeftExpected,
        SB.Left
      );

      AssertEquals(
        Format(
          'TopExpected: Erro no caso %d: Posição(%d, %d)',
          [I, Casos[I].Row, Casos[I].Col]
        ),
        Casos[I].TopExpected,
        SB.Top
      );
    finally
      FGrid.Free;
      SB.Free;
    end;
  end;
end;

procedure TGridLayoutTest.TestPosicaoComponente_ComColumnShift;
(*
Testa a posição dos componentes em uma grade 3x4 com ColumnShift aplicado.
  Cada célula da grade é de 10x20 pixels (largura x altura).
  O deslocamento vertical (Top) é afetado pelos ColumnShift definidos:

    ColumnShift[0] := 5   // Coluna 0 inteira é deslocada 5 pixels para baixo
    ColumnShift[2] := 7   // Coluna 2 inteira é deslocada 7 pixels para baixo

  Representação visual (valores esperados de Top por célula):

        Col 0   Col 1   Col 2   Col 3
      +-------+-------+-------+-------+
Row 0 |   5   |   0   |   7   |   0   |  <-- Cada valor representa o deslocamento no eixo Y
Row 1 |  25   |  20   |  27   |  20   |      (Top) após aplicar o shift e a altura da linha
Row 2 |  45   |  40   |  47   |  40   |
      +-------+-------+-------+-------+

  Casos testados:
    (0,0) → Top = 5   (shift da coluna 0)
    (1,1) → Top = 20  (sem shift)
    (2,2) → Top = 47  (40 + shift de 7 da coluna 2)
    (2,3) → Top = 40  (sem shift)
*)
type
  TCaso = record
    Row,
    Col,
    LeftExpected,
    TopExpected: Integer;
  end;
var
  FGrid: TGridLayout;
  SB: TSpeedButton;
  I: Integer;
const
  Casos: array[0..3] of TCaso = (
    (Row: 0; Col: 0; LeftExpected: 0; TopExpected: 5),
    (Row: 1; Col: 1; LeftExpected: 10; TopExpected: 20),
    (Row: 2; Col: 2; LeftExpected: 20; TopExpected: 47),
    (Row: 2; Col: 3; LeftExpected: 30; TopExpected: 40)
  );
begin
  for I := Low(Casos) to High(Casos) do
  begin
    FGrid := TGridLayout.Create;
    SB := TSpeedButton.Create(nil);

    FGrid.Rows := 3;
    FGrid.Columns := 4;
    FGrid.ColumnWidths := 10;
    FGrid.RowHeights := 20;
    FGrid.ColumnShift[0] := 5;
    FGrid.ColumnShift[2] := 7;

    try
      FGrid.AddItem(
        TControlGridItem.Create(SB),
        TGridCellSettings.Create(Casos[I].Row, Casos[I].Col)
      );

      FGrid.ArrangeItems;
      AssertEquals(
        Format(
          'LeftExpected: Erro no caso %d: Posição(%d, %d)',
          [I, Casos[I].Row, Casos[I].Col]
        ),
        Casos[I].LeftExpected,
        SB.Left
      );

      AssertEquals(
        Format(
          'TopExpected: Erro no caso %d: Posição(%d, %d)',
          [I, Casos[I].Row, Casos[I].Col]
        ),
        Casos[I].TopExpected,
        SB.Top
      );
    finally
      FGrid.Free;
      SB.Free;
    end;
  end;
end;

procedure TGridLayoutTest.TestTamanhoComponente;
type
  TCaso = record
    Row, Col: Integer;
  end;
var
  FGrid: TGridLayout;
  SB: TSpeedButton;
  I: Integer;
const
  Casos: array[0..3] of TCaso = (
    (Row: 0; Col: 0),
    (Row: 1; Col: 1),
    (Row: 2; Col: 2),
    (Row: 2; Col: 3)
  );

  DefaultWidth = 10;
  DefaultHeight = 20;
begin
  for I := Low(Casos) to High(Casos) do
  begin
    FGrid := TGridLayout.Create;
    SB := TSpeedButton.Create(nil);

    FGrid.Rows := 3;
    FGrid.Columns := 4;
    FGrid.ColumnWidths := DefaultWidth;
    FGrid.RowHeights := DefaultHeight;

    try
      FGrid.AddItem(
        TControlGridItem.Create(SB),
        TGridCellSettings.Create(Casos[I].Row, Casos[I].Col)
      );

      FGrid.ArrangeItems;

      AssertEquals(
        Format('WidthExpected: Erro no caso %d: Posição(%d, %d)',
          [I, Casos[I].Row, Casos[I].Col]),
        DefaultWidth,
        SB.Width
      );

      AssertEquals(
        Format('HeightExpected: Erro no caso %d: Posição(%d, %d)',
          [I, Casos[I].Row, Casos[I].Col]),
        DefaultHeight,
        SB.Height
      );
    finally
      FGrid.Free;
      SB.Free;
    end;
  end;
end;

procedure TGridLayoutTest.TestTamanhoComponente_ComLarguraEAlturaVariaveis;
type
  TCaso = record
    Row, Col: Integer;
    ExpectedWidth, ExpectedHeight: Integer;
  end;
var
  FGrid: TGridLayout;
  SB: TSpeedButton;
  I: Integer;
const
  Casos: array[0..3] of TCaso = (
    (Row: 0; Col: 0; ExpectedWidth: 15; ExpectedHeight: 25),
    (Row: 1; Col: 1; ExpectedWidth: 30; ExpectedHeight: 20),
    (Row: 2; Col: 2; ExpectedWidth: 10; ExpectedHeight: 35),
    (Row: 2; Col: 3; ExpectedWidth: 40; ExpectedHeight: 40)
  );
begin
  for I := Low(Casos) to High(Casos) do
  begin
    FGrid := TGridLayout.Create;
    SB := TSpeedButton.Create(nil);

    FGrid.Rows := 3;
    FGrid.Columns := 4;

    // Define tamanhos personalizados por coluna e linha
    FGrid.ColumnWidth[Casos[I].Col] := Casos[I].ExpectedWidth;
    FGrid.RowHeight[Casos[I].Row] := Casos[I].ExpectedHeight;

    try
      FGrid.AddItem(
        TControlGridItem.Create(SB),
        TGridCellSettings.Create(Casos[I].Row, Casos[I].Col)
      );

      FGrid.ArrangeItems;

      AssertEquals(
        Format('WidthExpected: Erro no caso %d: Posição(%d, %d)',
          [I, Casos[I].Row, Casos[I].Col]),
        Casos[I].ExpectedWidth,
        SB.Width
      );

      AssertEquals(
        Format('HeightExpected: Erro no caso %d: Posição(%d, %d)',
          [I, Casos[I].Row, Casos[I].Col]),
        Casos[I].ExpectedHeight,
        SB.Height
      );
    finally
      FGrid.Free;
      SB.Free;
    end;
  end;
end;

procedure TGridLayoutTest.TestTamanhoComponente_ComRowSpan;
 {
  Grid de 3 linhas, com as seguintes alturas:
  Row 0: 20px
  Row 1: 20px
  Row 2: 25px

  Casos testados:
  Caso 0: ocupa só a linha 0                  → altura esperada: 20
  Caso 1: ocupa as linhas 1 e 2              → altura esperada: 20 + 25 = 45
  Caso 2: ocupa linhas 0, 1 e 2              → altura esperada: 20 + 20 + 25 = 65

  Visual (linhas x colunas):
    C0     C1
  +-----+------+
  | C0  | C2   |  ← Row 0
  |     |      |
  +-----+------+
  | C1  |      |  ← Row 1
  |     |      |
  +-----+------+
  |     |      |  ← Row 2
  +-----+------+
 }
type
  TCaso = record
    Row, Col, RowSpan: Integer;
    ExpectedHeight: Integer;
  end;
var
  FGrid: TGridLayout;
  SB: TSpeedButton;
  I: Integer;
  Settings: TGridCellSettings;
const
  Casos: array[0..2] of TCaso = (
    (Row: 0; Col: 0; RowSpan: 1; ExpectedHeight: 20),
    (Row: 1; Col: 0; RowSpan: 2; ExpectedHeight: 45),
    (Row: 0; Col: 1; RowSpan: 3; ExpectedHeight: 65)
  );
begin
  for I := Low(Casos) to High(Casos) do
  begin
    FGrid := TGridLayout.Create;
    SB := TSpeedButton.Create(nil);

    FGrid.Rows := 3;
    FGrid.Columns := 2;
    FGrid.RowHeight[0] := 20;
    FGrid.RowHeight[1] := 20;
    FGrid.RowHeight[2] := 25;
    FGrid.ColumnWidths := 30;

    try
      Settings := TGridCellSettings.Create(Casos[I].Row, Casos[I].Col);
      Settings.WithRowSpan(Casos[I].RowSpan);
      FGrid.AddItem(TControlGridItem.Create(SB), Settings);

      FGrid.ArrangeItems;

      AssertEquals(
        Format('Erro no caso %d: Height esperado com RowSpan=%d em (Row=%d):',
          [I, Casos[I].RowSpan, Casos[I].Row]),
        Casos[I].ExpectedHeight,
        SB.Height
      );
    finally
      FGrid.Free;
      SB.Free;
    end;
  end;
end;

procedure TGridLayoutTest.TestTamanhoComponente_ComColumnSpan;
{
  Grid de 3 colunas, com as seguintes larguras:
  Col 0: 10px
  Col 1: 15px
  Col 2: 20px

  Casos testados:
  Caso 0: ocupa só a coluna 0                  → largura esperada: 10
  Caso 1: ocupa as colunas 1 e 2              → largura esperada: 15 + 20 = 35
  Caso 2: ocupa colunas 0, 1 e 2              → largura esperada: 10 + 15 + 20 = 45

  Visual (linhas x colunas):
       C0     C1     C2
    +------+------+------+
 R0 | C0  | C2           |
    |     |              |
    +------+------+------+
 R1 |      C1            |
    |                    |
    +------+------+------+
}
type
  TCaso = record
    Row, Col, ColumnSpan: Integer;
    ExpectedWidth: Integer;
  end;
var
  FGrid: TGridLayout;
  SB: TSpeedButton;
  I: Integer;
  Settings: TGridCellSettings;
const
  Casos: array[0..2] of TCaso = (
    (Row: 0; Col: 0; ColumnSpan: 1; ExpectedWidth: 10),
    (Row: 1; Col: 1; ColumnSpan: 2; ExpectedWidth: 35),
    (Row: 0; Col: 0; ColumnSpan: 3; ExpectedWidth: 45)
  );
begin
  for I := Low(Casos) to High(Casos) do
  begin
    FGrid := TGridLayout.Create;
    SB := TSpeedButton.Create(nil);

    FGrid.Rows := 2;
    FGrid.Columns := 3;
    FGrid.ColumnWidth[0] := 10;
    FGrid.ColumnWidth[1] := 15;
    FGrid.ColumnWidth[2] := 20;
    FGrid.RowHeights := 25;

    try
      Settings := TGridCellSettings.Create(Casos[I].Row, Casos[I].Col);
      Settings.WithColumnSpan(Casos[I].ColumnSpan);
      FGrid.AddItem(TControlGridItem.Create(SB), Settings);

      FGrid.ArrangeItems;

      AssertEquals(
        Format('Erro no caso %d: Width esperado com ColumnSpan=%d em (Col=%d):',
          [I, Casos[I].ColumnSpan, Casos[I].Col]),
        Casos[I].ExpectedWidth,
        SB.Width
      );
    finally
      FGrid.Free;
      SB.Free;
    end;
  end;
end;

procedure TGridLayoutTest.TestTamanhoComponente_ComColumnSpan_ComColunaOculta;
type
  TCaso = record
    HiddenCol,
    Row, Col, ColumnSpan: Integer;
    ExpectedWidth: Integer;
  end;
var
  FGrid: TGridLayout;
  SB: TSpeedButton;
  I: Integer;
  Settings: TGridCellSettings;
const
  Casos: array[0..2] of TCaso = (
    (HiddenCol: 1; Row: 0; Col: 0; ColumnSpan: 1; ExpectedWidth: 10),
    (HiddenCol: 1; Row: 0; Col: 0; ColumnSpan: 2; ExpectedWidth: 10),
    (HiddenCol: 2; Row: 0; Col: 0; ColumnSpan: 3; ExpectedWidth: 25)
  );
begin
  for I := Low(Casos) to High(Casos) do
  begin
    FGrid := TGridLayout.Create;
    SB := TSpeedButton.Create(nil);

    FGrid.Rows := 2;
    FGrid.Columns := 3;
    FGrid.ColumnWidth[0] := 10;
    FGrid.ColumnWidth[1] := 15;
    FGrid.ColumnWidth[2] := 20;
    FGrid.RowHeights := 25;

    FGrid.VisibleColumn[Casos[I].HiddenCol] := False;

    try
      Settings := TGridCellSettings.Create(Casos[I].Row, Casos[I].Col);
      Settings.WithColumnSpan(Casos[I].ColumnSpan);
      FGrid.AddItem(TControlGridItem.Create(SB), Settings);

      FGrid.ArrangeItems;

      AssertEquals(
        Format('Erro no caso %d: Width esperado com ColumnSpan=%d em (Col=%d):',
          [I, Casos[I].ColumnSpan, Casos[I].Col]),
        Casos[I].ExpectedWidth,
        SB.Width
      );
    finally
      FGrid.Free;
      SB.Free;
    end;
  end;
end;

procedure TGridLayoutTest.TestTamanhoComponente_ComRowSpanEColumnSpan;
{
  Grid de 3 linhas e 3 colunas, com:
  - Alturas das linhas:    [20, 25, 30]
  - Larguras das colunas:  [10, 15, 20]

  Casos testados:
  Caso 0: ocupa (0, 0) com RowSpan=1 e ColSpan=1  → tamanho: 10 x 20
  Caso 1: ocupa (1, 1) com RowSpan=2 e ColSpan=2  → tamanho: (15+20) x (25+30) = 35 x 55
  Caso 2: ocupa (0, 0) com RowSpan=3 e ColSpan=3  → tamanho: (10+15+20) x (20+25+30) = 45 x 75

  Visual:
       C0     C1     C2
    +------+------+------+
 R0 | C0  |              |
    |     |              |
    +------+------+------+
 R1 |      C1            |
    |                    |
    +------+------+------+
 R2 |                    |
    +------+------+------+
}
type
  TCaso = record
    Row, Col, RowSpan, ColSpan: Integer;
    ExpectedWidth, ExpectedHeight: Integer;
  end;
var
  FGrid: TGridLayout;
  SB: TSpeedButton;
  I: Integer;
  Settings: TGridCellSettings;
const
  Casos: array[0..2] of TCaso = (
    (Row: 0; Col: 0; RowSpan: 1; ColSpan: 1; ExpectedWidth: 10; ExpectedHeight: 20),
    (Row: 1; Col: 1; RowSpan: 2; ColSpan: 2; ExpectedWidth: 35; ExpectedHeight: 55),
    (Row: 0; Col: 0; RowSpan: 3; ColSpan: 3; ExpectedWidth: 45; ExpectedHeight: 75)
  );
begin
  for I := Low(Casos) to High(Casos) do
  begin
    FGrid := TGridLayout.Create;
    SB := TSpeedButton.Create(nil);

    FGrid.Rows := 3;
    FGrid.Columns := 3;
    FGrid.RowHeight[0] := 20;
    FGrid.RowHeight[1] := 25;
    FGrid.RowHeight[2] := 30;
    FGrid.ColumnWidth[0] := 10;
    FGrid.ColumnWidth[1] := 15;
    FGrid.ColumnWidth[2] := 20;

    try
      Settings := TGridCellSettings.Create(Casos[I].Row, Casos[I].Col);
      Settings.WithRowSpan(Casos[I].RowSpan).WithColumnSpan(Casos[I].ColSpan);

      FGrid.AddItem(TControlGridItem.Create(SB), Settings);
      FGrid.ArrangeItems;

      AssertEquals(
        Format('Erro no caso %d: Width esperado (%d x colunas) em (Col=%d, Span=%d)',
          [I, Casos[I].ExpectedWidth, Casos[I].Col, Casos[I].ColSpan]),
        Casos[I].ExpectedWidth,
        SB.Width
      );

      AssertEquals(
        Format('Erro no caso %d: Height esperado (%d x linhas) em (Row=%d, Span=%d)',
          [I, Casos[I].ExpectedHeight, Casos[I].Row, Casos[I].RowSpan]),
        Casos[I].ExpectedHeight,
        SB.Height
      );
    finally
      FGrid.Free;
      SB.Free;
    end;
  end;
end;

procedure TGridLayoutTest.TestPosicaoComponente_ComHorizontalSpacing;
{
  Grid de 3 linhas e 4 colunas, com:
  - Largura padrão das colunas: 10
  - Altura padrão das linhas: 20
  - Espaçamento horizontal: 3px entre colunas

  Casos testados:
  Caso 0: célula (0, 0) → Left = 0
  Caso 1: célula (1, 1) → Left = (10 + 3) * 1 = 13
  Caso 2: célula (2, 2) → Left = (10 + 3) * 2 = 26
  Caso 3: célula (2, 3) → Left = (10 + 3) * 3 = 39
}
type
  TCaso = record
    Row, Col,
    LeftExpected, TopExpected: Integer;
  end;
const
  Casos: array[0..3] of TCaso = (
    (Row: 0; Col: 0; LeftExpected: 0; TopExpected: 0),
    (Row: 1; Col: 1; LeftExpected: 13; TopExpected: 20),
    (Row: 2; Col: 2; LeftExpected: 26; TopExpected: 40),
    (Row: 2; Col: 3; LeftExpected: 39; TopExpected: 40)
  );
var
  FGrid: TGridLayout;
  SB: TSpeedButton;
  I: Integer;
begin
  for I := Low(Casos) to High(Casos) do
  begin
    FGrid := TGridLayout.Create;
    SB := TSpeedButton.Create(nil);

    FGrid.Rows := 3;
    FGrid.Columns := 4;
    FGrid.ColumnWidths := 10;
    FGrid.RowHeights := 20;
    FGrid.HorizontalSpacings := 3;

    try
      FGrid.AddItem(
        TControlGridItem.Create(SB),
        TGridCellSettings.Create(Casos[I].Row, Casos[I].Col)
      );

      FGrid.ArrangeItems;

      AssertEquals(
        Format(
          'LeftExpected: Erro no caso %d: Posição(%d, %d)',
          [I, Casos[I].Row, Casos[I].Col]
        ),
        Casos[I].LeftExpected,
        SB.Left
      );

      AssertEquals(
        Format(
          'TopExpected: Erro no caso %d: Posição(%d, %d)',
          [I, Casos[I].Row, Casos[I].Col]
        ),
        Casos[I].TopExpected,
        SB.Top
      );
    finally
      FGrid.Free;
      SB.Free;
    end;
  end;
end;

procedure TGridLayoutTest.TestPosicaoComponente_ComVerticalSpacing;
{
  Grid de 3 linhas e 4 colunas, com:
  - Altura padrão das linhas: 20
  - Largura padrão das colunas: 10
  - Espaçamento vertical: 4px entre linhas

  Casos testados:
  Caso 0: célula (0, 0) → Top = 0
  Caso 1: célula (1, 1) → Top = (20 + 4) * 1 = 24
  Caso 2: célula (2, 2) → Top = (20 + 4) * 2 = 48
  Caso 3: célula (2, 3) → Top = (20 + 4) * 2 = 48
}
type
  TCaso = record
    Row, Col,
    LeftExpected, TopExpected: Integer;
  end;
const
  Casos: array[0..3] of TCaso = (
    (Row: 0; Col: 0; LeftExpected: 0; TopExpected: 0),
    (Row: 1; Col: 1; LeftExpected: 10; TopExpected: 24),
    (Row: 2; Col: 2; LeftExpected: 20; TopExpected: 48),
    (Row: 2; Col: 3; LeftExpected: 30; TopExpected: 48)
  );
var
  FGrid: TGridLayout;
  SB: TSpeedButton;
  I: Integer;
begin
  for I := Low(Casos) to High(Casos) do
  begin
    FGrid := TGridLayout.Create;
    SB := TSpeedButton.Create(nil);

    FGrid.Rows := 3;
    FGrid.Columns := 4;
    FGrid.ColumnWidths := 10;
    FGrid.RowHeights := 20;
    FGrid.VerticalSpacings := 4;

    try
      FGrid.AddItem(
        TControlGridItem.Create(SB),
        TGridCellSettings.Create(Casos[I].Row, Casos[I].Col)
      );

      FGrid.ArrangeItems;

      AssertEquals(
        Format(
          'LeftExpected: Erro no caso %d: Posição(%d, %d)',
          [I, Casos[I].Row, Casos[I].Col]
        ),
        Casos[I].LeftExpected,
        SB.Left
      );

      AssertEquals(
        Format(
          'TopExpected: Erro no caso %d: Posição(%d, %d)',
          [I, Casos[I].Row, Casos[I].Col]
        ),
        Casos[I].TopExpected,
        SB.Top
      );
    finally
      FGrid.Free;
      SB.Free;
    end;
  end;
end;

procedure TGridLayoutTest.TestPosicaoComponente_ComEspacamentoHorizontalEVertical;
{
  Grid de 3 linhas e 4 colunas com:
  - Altura padrão das linhas: 20
  - Largura padrão das colunas: 10
  - Espaçamento horizontal: 3px entre colunas
  - Espaçamento vertical: 4px entre linhas

  Casos testados:
  Caso 0: célula (0, 0) → Left = 0, Top = 0
  Caso 1: célula (1, 1) → Left = (10 + 3) * 1 = 13, Top = (20 + 4) * 1 = 24
  Caso 2: célula (2, 2) → Left = (10 + 3) * 2 = 26, Top = (20 + 4) * 2 = 48
  Caso 3: célula (2, 3) → Left = (10 + 3) * 3 = 39, Top = 48
}
type
  TCaso = record
    Row, Col,
    LeftExpected, TopExpected: Integer;
  end;
const
  Casos: array[0..3] of TCaso = (
    (Row: 0; Col: 0; LeftExpected: 0; TopExpected: 0),
    (Row: 1; Col: 1; LeftExpected: 13; TopExpected: 24),
    (Row: 2; Col: 2; LeftExpected: 26; TopExpected: 48),
    (Row: 2; Col: 3; LeftExpected: 39; TopExpected: 48)
  );
var
  FGrid: TGridLayout;
  SB: TSpeedButton;
  I: Integer;
begin
  for I := Low(Casos) to High(Casos) do
  begin
    FGrid := TGridLayout.Create;
    SB := TSpeedButton.Create(nil);

    FGrid.Rows := 3;
    FGrid.Columns := 4;
    FGrid.ColumnWidths := 10;
    FGrid.RowHeights := 20;
    FGrid.HorizontalSpacings := 3;
    FGrid.VerticalSpacings := 4;

    try
      FGrid.AddItem(
        TControlGridItem.Create(SB),
        TGridCellSettings.Create(Casos[I].Row, Casos[I].Col)
      );

      FGrid.ArrangeItems;

      AssertEquals(
        Format(
          'LeftExpected: Erro no caso %d: Posição(%d, %d)',
          [I, Casos[I].Row, Casos[I].Col]
        ),
        Casos[I].LeftExpected,
        SB.Left
      );

      AssertEquals(
        Format(
          'TopExpected: Erro no caso %d: Posição(%d, %d)',
          [I, Casos[I].Row, Casos[I].Col]
        ),
        Casos[I].TopExpected,
        SB.Top
      );
    finally
      FGrid.Free;
      SB.Free;
    end;
  end;
end;

procedure TGridLayoutTest.TestPosicaoComponente_ComEspacamentoCustomizado;
{
  Grid de 3 linhas e 4 colunas com:
  - Altura padrão das linhas: 20
  - Largura padrão das colunas: 10
  - Espaçamento entre colunas:
      Coluna 0 → 3px
      Coluna 1 → 4px
      Coluna 2 → 5px
  - Espaçamento entre linhas:
      Linha 0 → 2px
      Linha 1 → 4px

  Casos testados:
  Caso 0: célula (0, 0) → Left = 0, Top = 0
  Caso 1: célula (1, 1) →
    Left = 10 + 3 = 13
    Top  = 20 + 2 = 22
  Caso 2: célula (2, 2) →
    Left = 10 + 3 + 10 + 4 = 27
    Top  = 20 + 2 + 20 + 4 = 46
  Caso 3: célula (2, 3) →
    Left = 10 + 3 + 10 + 4 + 10 + 5 = 42
    Top  = 46
}
type
  TCaso = record
    Row, Col,
    LeftExpected, TopExpected: Integer;
  end;
const
  Casos: array[0..3] of TCaso = (
    (Row: 0; Col: 0; LeftExpected: 0; TopExpected: 0),
    (Row: 1; Col: 1; LeftExpected: 13; TopExpected: 22),
    (Row: 2; Col: 2; LeftExpected: 27; TopExpected: 46),
    (Row: 2; Col: 3; LeftExpected: 42; TopExpected: 46)
  );
var
  FGrid: TGridLayout;
  SB: TSpeedButton;
  I: Integer;
begin
  for I := Low(Casos) to High(Casos) do
  begin
    FGrid := TGridLayout.Create;
    SB := TSpeedButton.Create(nil);

    FGrid.Rows := 3;
    FGrid.Columns := 4;
    FGrid.ColumnWidths := 10;
    FGrid.RowHeights := 20;

    // Define espaçamentos personalizados entre colunas
    FGrid.HorizontalSpacing[0] := 3;
    FGrid.HorizontalSpacing[1] := 4;
    FGrid.HorizontalSpacing[2] := 5;

    // Define espaçamentos personalizados entre linhas
    FGrid.VerticalSpacing[0] := 2;
    FGrid.VerticalSpacing[1] := 4;

    try
      FGrid.AddItem(
        TControlGridItem.Create(SB),
        TGridCellSettings.Create(Casos[I].Row, Casos[I].Col)
      );

      FGrid.ArrangeItems;

      AssertEquals(
        Format(
          'LeftExpected: Erro no caso %d: Posição(%d, %d)',
          [I, Casos[I].Row, Casos[I].Col]
        ),
        Casos[I].LeftExpected,
        SB.Left
      );

      AssertEquals(
        Format(
          'TopExpected: Erro no caso %d: Posição(%d, %d)',
          [I, Casos[I].Row, Casos[I].Col]
        ),
        Casos[I].TopExpected,
        SB.Top
      );
    finally
      FGrid.Free;
      SB.Free;
    end;
  end;
end;

procedure TGridLayoutTest.TestTamanhoComponente_ComRowColSpan_EspacamentoCustomizado;
{
  Grid: 3 linhas, 3 colunas
  - Alturas das linhas: 20, 25, 30
  - Larguras das colunas: 10, 15, 20
  - Espaçamento vertical:
      Linha 0 → 3px
      Linha 1 → 5px
  - Espaçamento horizontal:
      Coluna 0 → 2px
      Coluna 1 → 4px

  Componente ocupa:
  - Row 0 a Row 2 (RowSpan=3)
  - Col 0 a Col 2 (ColumnSpan=3)

  Altura esperada = 20 + 3 + 25 + 5 + 30 = 83
  Largura esperada = 10 + 2 + 15 + 4 + 20 = 51
}
var
  FGrid: TGridLayout;
  SB: TSpeedButton;
  Settings: TGridCellSettings;
begin
  FGrid := TGridLayout.Create;
  SB := TSpeedButton.Create(nil);

  try
    // Definições básicas
    FGrid.Rows := 3;
    FGrid.Columns := 3;
    FGrid.RowHeight[0] := 20;
    FGrid.RowHeight[1] := 25;
    FGrid.RowHeight[2] := 30;

    FGrid.ColumnWidth[0] := 10;
    FGrid.ColumnWidth[1] := 15;
    FGrid.ColumnWidth[2] := 20;

    // Espaçamentos customizados
    FGrid.VerticalSpacing[0] := 3;
    FGrid.VerticalSpacing[1] := 5;

    FGrid.HorizontalSpacing[0] := 2;
    FGrid.HorizontalSpacing[1] := 4;

    // Item com RowSpan + ColumnSpan
    Settings :=
      TGridCellSettings.Create(0, 0)
        .WithRowSpan(3)
        .WithColumnSpan(3);

    FGrid.AddItem(TControlGridItem.Create(SB), Settings);

    FGrid.ArrangeItems;

    // Verificações
    AssertEquals('Altura incorreta com RowSpan+Espaçamento customizado', 83, SB.Height);
    AssertEquals('Largura incorreta com ColumnSpan+Espaçamento customizado', 51, SB.Width);
  finally
    FGrid.Free;
    SB.Free;
  end;
end;

procedure TGridLayoutTest.TestPosicaoComponente_ComMargens;
var
  Grid: TGridLayout;
  SB: TSpeedButton;
begin
  Grid := TGridLayout.Create;
  SB := TSpeedButton.Create(nil);

  Grid.Rows := 1;
  Grid.Columns := 1;
  Grid.ColumnWidths := 50;
  Grid.RowHeights := 30;
  Grid.Margins.Top := 10;
  Grid.Margins.Left := 15;

  try
    Grid.AddItem(
      TControlGridItem.Create(SB),
      TGridCellSettings.Create(0, 0)
    );
    Grid.ArrangeItems;

    AssertEquals('Left do controle deve respeitar Margin.Left', 15, SB.Left);
    AssertEquals('Top do controle deve respeitar Margin.Top', 10, SB.Top);
  finally
    Grid.Free;
    SB.Free;
  end;
end;

procedure TGridLayoutTest.TestMargensAfetamContentSize;
var
  Grid: TGridLayout;
  Rows: Integer;
begin
  Grid := TGridLayout.Create;
  try
    Grid.Rows := 1;
    Grid.Columns := 1;
    Grid.ColumnWidths := 50;
    Grid.RowHeights := 30;
    Grid.Margins.All := 5;

    AssertEquals('ContentWidth deve incluir Margins.Left e Margins.Right',
      50 + 5 + 5, Grid.ContentWidth);

    AssertEquals('ContentHeight deve incluir Margins.Top e Margins.Bottom',
      30 + 5 + 5, Grid.ContentHeight);
  finally
    Grid.Free;
  end;
end;

procedure TGridLayoutTest.TestMargensComMultiplasCelulas;
var
  Grid: TGridLayout;
  SB: TSpeedButton;
begin
  Grid := TGridLayout.Create;
  SB := TSpeedButton.Create(nil);

  Grid.Rows := 2;
  Grid.Columns := 2;
  Grid.ColumnWidths := 40;
  Grid.RowHeights := 20;
  Grid.HorizontalSpacings := 5;
  Grid.VerticalSpacings := 10;
  Grid.Margins.Top := 7;
  Grid.Margins.Left := 3;

  try
    Grid.AddItem(
      TControlGridItem.Create(SB),
      TGridCellSettings.Create(1, 1)
    );
    Grid.ArrangeItems;

    // Cálculo esperado:
    // Top: Margem.Top (7) + Altura linha 0 (20) + Espaçamento (10) = 37
    // Left: Margem.Left (3) + Largura coluna 0 (40) + Espaçamento (5) = 48
    AssertEquals('Top esperado com margens, linha 1', 37, SB.Top);
    AssertEquals('Left esperado com margens, coluna 1', 48, SB.Left);
  finally
    Grid.Free;
    SB.Free;
  end;
end;

procedure TGridLayoutTest.TestMargensEContentSizeComMultiplasCelulas;
var
  Grid: TGridLayout;
begin
  Grid := TGridLayout.Create;
  try
    Grid.Rows := 2;
    Grid.Columns := 3;
    Grid.ColumnWidths := 50;
    Grid.RowHeights := 25;
    Grid.HorizontalSpacings := 4;
    Grid.VerticalSpacings := 3;
    Grid.Margins.Top := 2;
    Grid.Margins.Bottom := 8;
    Grid.Margins.Left := 6;
    Grid.Margins.Right := 10;

    // Largura:
    // 3 colunas * 50 + 2 espaçamentos (entre 3 colunas) = 150 + 8 = 158
    // + margens: 6 + 10 = 174
    AssertEquals('ContentWidth com margens', 174, Grid.ContentWidth);

    // Altura:
    // 2 linhas * 25 + 1 espaçamento (entre 2 linhas) = 50 + 3 = 53
    // + margens: 2 + 8 = 63
    AssertEquals('ContentHeight com margens', 63, Grid.ContentHeight);
  finally
    Grid.Free;
  end;
end;

procedure TGridLayoutTest.TestMargensComRowSpanEColSpan;
var
  Grid: TGridLayout;
  SB: TSpeedButton;
  Settings: TGridCellSettings;
begin
  Grid := TGridLayout.Create;
  SB := TSpeedButton.Create(nil);
  Grid.Rows := 2;
  Grid.Columns := 2;
  Grid.RowHeight[0] := 20;
  Grid.RowHeight[1] := 30;
  Grid.ColumnWidth[0] := 25;
  Grid.ColumnWidth[1] := 35;
  Grid.HorizontalSpacings := 4;
  Grid.VerticalSpacings := 6;
  Grid.Margins.Left := 10;
  Grid.Margins.Top := 5;

  try
    Settings := TGridCellSettings.Create(0, 0)
      .WithRowSpan(2)
      .WithColumnSpan(2);

    Grid.AddItem(TControlGridItem.Create(SB), Settings);
    Grid.ArrangeItems;

    // Esperado:
    // Width = 25 + 35 + 1 espaçamento horizontal (4) = 64
    // Height = 20 + 30 + 1 espaçamento vertical (6) = 56
    // Top/Left = Margens
    AssertEquals('Left com margens', 10, SB.Left);
    AssertEquals('Top com margens', 5, SB.Top);
    AssertEquals('Width com spans e margens', 64, SB.Width);
    AssertEquals('Height com spans e margens', 56, SB.Height);
  finally
    Grid.Free;
    SB.Free;
  end;
end;

procedure TGridLayoutTest.TestOffsetSimples;
var
  Grid: TGridLayout;
  SB: TSpeedButton;
  Settings: TGridCellSettings;
begin
  Grid := TGridLayout.Create;
  SB := TSpeedButton.Create(nil);

  Grid.Rows := 1;
  Grid.Columns := 1;
  Grid.ColumnWidths := 50;
  Grid.RowHeights := 30;
  Grid.Margins.All := 0;
  Grid.HorizontalSpacings := 0;
  Grid.VerticalSpacings := 0;

  try
    Settings := TGridCellSettings.Create(0, 0);
    Settings.WithOffsetX(5);
    Settings.WithOffsetY(7);

    Grid.AddItem(TControlGridItem.Create(SB), Settings);
    Grid.ArrangeItems;

    // Esperado: posição inicial é 0,0 (sem margens), + offset aplicado
    AssertEquals('Left com OffsetX=5', 5, SB.Left);
    AssertEquals('Top com OffsetY=7', 7, SB.Top);
  finally
    Grid.Free;
    SB.Free;
  end;
end;

procedure TGridLayoutTest.TestOffsetComMargensEGrid;
var
  Grid: TGridLayout;
  SB: TSpeedButton;
  Settings: TGridCellSettings;
begin
  Grid := TGridLayout.Create;
  SB := TSpeedButton.Create(nil);

  Grid.Rows := 2;
  Grid.Columns := 2;
  Grid.RowHeight[0] := 20;
  Grid.ColumnWidth[0] := 40;
  Grid.HorizontalSpacings := 4;
  Grid.VerticalSpacings := 6;
  Grid.Margins.Left := 10;
  Grid.Margins.Top := 15;

  try
    Settings := TGridCellSettings.Create(1, 1); // Segunda linha e coluna
    Settings.WithOffsetX(-2);
    Settings.WithOffsetY(3);

    Grid.AddItem(TControlGridItem.Create(SB), Settings);
    Grid.ArrangeItems;

    // Esperado:
    // Left = margem esquerda (10) + col0 (40) + espaçamento (4) = 54 → + offset (-2) = 52
    // Top = margem topo (15) + row0 (20) + espaçamento (6) = 41 → + offset (3) = 44
    AssertEquals('Left com Offset e margens', 52, SB.Left);
    AssertEquals('Top com Offset e margens', 44, SB.Top);
  finally
    Grid.Free;
    SB.Free;
  end;
end;

procedure TGridLayoutTest.TestOffsetComSpan;
var
  Grid: TGridLayout;
  SB: TSpeedButton;
  Settings: TGridCellSettings;
begin
  Grid := TGridLayout.Create;
  SB := TSpeedButton.Create(nil);

  Grid.Rows := 2;
  Grid.Columns := 2;
  Grid.RowHeight[0] := 25;
  Grid.RowHeight[1] := 35;
  Grid.ColumnWidth[0] := 30;
  Grid.ColumnWidth[1] := 50;
  Grid.HorizontalSpacings := 3;
  Grid.VerticalSpacings := 4;
  Grid.Margins.All := 0;

  try
    Settings := TGridCellSettings.Create(0, 0);
    Settings
      .WithRowSpan(2)
      .WithColumnSpan(2)
      .WithOffsetX(6)
      .WithOffsetY(-2);

    Grid.AddItem(TControlGridItem.Create(SB), Settings);
    Grid.ArrangeItems;

    // Width = 30 + 50 + spacing (3) = 83
    // Height = 25 + 35 + spacing (4) = 64
    // Pos X = 0 + offset (6) = 6
    // Pos Y = 0 + offset (-2) = -2

    AssertEquals('Left com offset no span', 6, SB.Left);
    AssertEquals('Top com offset no span', -2, SB.Top);
    AssertEquals('Width com span', 83, SB.Width);
    AssertEquals('Height com span', 64, SB.Height);
  finally
    Grid.Free;
    SB.Free;
  end;
end;

procedure TGridLayoutTest.TestLayoutAlignmentHorizontal;
var
  Grid: TGridLayout;
  Btn: TSpeedButton;
  Settings: TGridCellSettings;
begin
  Grid := TGridLayout.Create;
  Btn := TSpeedButton.Create(nil);

  Grid.Rows := 1;
  Grid.Columns := 1;
  Grid.ColumnWidth[0] := 100;
  Grid.RowHeights := 30;
  Grid.Margins.All := 0;
  Btn.Width := 40;
  Btn.Height := 20;

  try
    Settings := TGridCellSettings.Create(0, 0);
    Settings.WithHorizontalAlignment(laCenter);

    Grid.AddItem(TControlGridItem.Create(Btn), Settings);
    Grid.ArrangeItems;

    // 100 de largura de célula → 40 de controle → centralizado: Left = (100 - 40) div 2 = 30
    AssertEquals('Left com AlignH=Center', 30, Btn.Left);
  finally
    Grid.Free;
    Btn.Free;
  end;
end;

procedure TGridLayoutTest.TestLayoutAlignmentVertical;
var
  Grid: TGridLayout;
  Btn: TSpeedButton;
  Settings: TGridCellSettings;
begin
  Grid := TGridLayout.Create;
  Btn := TSpeedButton.Create(nil);

  Grid.Rows := 1;
  Grid.Columns := 1;
  Grid.RowHeight[0] := 40;
  Grid.ColumnWidths := 60;
  Grid.Margins.All := 0;
  Btn.Width := 30;
  Btn.Height := 20;

  try
    Settings := TGridCellSettings.Create(0, 0);
    Settings.WithVerticalAlignment(laEnd);

    Grid.AddItem(TControlGridItem.Create(Btn), Settings);
    Grid.ArrangeItems;

    // 40 de altura de célula → 20 de controle → alinhado ao fundo: Top = 40 - 20 = 20
    AssertEquals('Top com AlignV=End', 20, Btn.Top);
  finally
    Grid.Free;
    Btn.Free;
  end;
end;

procedure TGridLayoutTest.TestLayoutVerticalAndHorizontaAlignment;
var
  Grid: TGridLayout;
  Btn: TSpeedButton;
  Settings: TGridCellSettings;
begin
  Grid := TGridLayout.Create;
  Btn := TSpeedButton.Create(nil);

  Grid.Rows := 1;
  Grid.Columns := 1;
  Grid.RowHeight[0] := 25;
  Grid.ColumnWidth[0] := 80;
  Grid.Margins.All := 0;
  Btn.Height := 15;
  Btn.Width := 30;

  try
    Settings := TGridCellSettings.Create(0, 0);
    Settings.WithHorizontalAlignment(laEnd);
    Settings.WithVerticalAlignment(laCenter);

    Grid.AddItem(TControlGridItem.Create(Btn), Settings);
    Grid.ArrangeItems;

    AssertEquals('Btn Left', 50, Btn.Left);  // 50
    AssertEquals('Btn Top', 5, Btn.Top);  // 5
  finally
    Grid.Free;
    Btn.Free;
  end;
end;

procedure TGridLayoutTest.TestLayoutAlignmentVertical_ComRowSpan;
var
  Grid: TGridLayout;
  Btn: TSpeedButton;
  Settings: TGridCellSettings;
begin
  Grid := TGridLayout.Create;
  Btn := TSpeedButton.Create(nil);

  Grid.Rows := 3;
  Grid.Columns := 1;
  Grid.RowHeight[0] := 30;
  Grid.RowHeight[1] := 40;
  Grid.ColumnWidths := 100;
  Grid.Margins.All := 0;
  Grid.VerticalSpacings := 10;

  Btn.Height := 30; // menor que o total do span (30 + 40 + spacing = 80)

  try
    Settings := TGridCellSettings.Create(0, 0);
    Settings.WithRowSpan(2);
    Settings.WithVerticalAlignment(laCenter);

    Grid.AddItem(TControlGridItem.Create(Btn), Settings);
    Grid.ArrangeItems;

    // Altura da célula estendida: 30 + 10 (spacing) + 40 = 80
    // Altura do controle: 30
    // Top esperado: centralizado → (80 - 30) div 2 = 25
    AssertEquals('Top com RowSpan=2 e AlignV=Center', 25, Btn.Top);
  finally
    Grid.Free;
    Btn.Free;
  end;
end;

procedure TGridLayoutTest.TestComponenteMaiorQueCelula_RespeitaTamanhoComAlignment;
var
  FGrid: TGridLayout;
  SB: TSpeedButton;
  Settings: TGridCellSettings;
begin
  FGrid := TGridLayout.Create;
  SB := TSpeedButton.Create(nil);
  try
    // Definindo grid com célula pequena
    FGrid.Rows := 1;
    FGrid.Columns := 1;
    FGrid.RowHeights := 30;
    FGrid.ColumnWidths := 30;

    // Definindo controle propositalmente maior que a célula
    SB.Width := 50;
    SB.Height := 40;

    // Alinhamento centralizado (sem stretch)
    Settings := TGridCellSettings.Create(0, 0);
    Settings.WithHorizontalAlignment(laCenter);
    Settings.WithVerticalAlignment(laCenter);

    FGrid.AddItem(TControlGridItem.Create(SB), Settings);
    FGrid.ArrangeItems;

    // Verificações:
    AssertEquals('Controle mantém Width original', 50, SB.Width);
    AssertEquals('Controle mantém Height original', 40, SB.Height);

    // Está centralizado, mesmo excedendo o limite da célula
    AssertEquals('Left centralizado dentro da célula', (30 - 50) div 2, SB.Left);
    AssertEquals('Top centralizado dentro da célula', (30 - 40) div 2, SB.Top);
  finally
    FGrid.Free;
    SB.Free;
  end;
end;

procedure TGridLayoutTest.TestExtraSize_StretchOnly;
var
  Grid: TGridLayout;
  Btn: TSpeedButton;
  Settings: TGridCellSettings;
begin
  Grid := TGridLayout.Create;
  Btn := TSpeedButton.Create(nil);

  Grid.Rows := 1;
  Grid.Columns := 1;
  Grid.RowHeight[0] := 30;
  Grid.ColumnWidths := 40;

  Settings := TGridCellSettings.Create(0, 0);
  Settings.WithExtraWidth(10);
  Settings.WithExtraHeight(5);
  Settings.WithVerticalAlignment(laStretch);
  Settings.WithHorizontalAlignment(laStretch);

  Grid.AddItem(TControlGridItem.Create(Btn), Settings);
  Grid.ArrangeItems;

  // Quando alinhamento é laStretch, o ExtraWidth/ExtraHeight devem ser somados
  AssertEquals('Largura incorreta com Stretch + ExtraWidth', 50, Btn.Width);
  AssertEquals('Altura incorreta com Stretch + ExtraHeight', 35, Btn.Height);

  // Teste com alinhamento laCenter: não deve aplicar os extras
  Settings := TGridCellSettings.Create(0, 0);
  Settings.WithExtraWidth(10);
  Settings.WithVerticalAlignment(laCenter);
  Settings.WithHorizontalAlignment(laCenter);
  Btn.Width := 20;
  Btn.Height := 20;

  Grid := TGridLayout.Create;
  Grid.Rows := 1;
  Grid.Columns := 1;
  Grid.RowHeight[0] := 30;
  Grid.ColumnWidths := 40;

  Grid.AddItem(TControlGridItem.Create(Btn), Settings);
  Grid.ArrangeItems;

  AssertEquals('Largura incorreta com Center (sem aplicar ExtraWidth)', 20, Btn.Width);
  AssertEquals('Altura incorreta com Center (sem aplicar ExtraHeight)', 20, Btn.Height);

  Btn.Free;
  Grid.Free;
end;


procedure TGridLayoutTest.TestTamanhoComponente_ComColSpanExcedeUltimaColuna;
var
  Grid: TGridLayout;
  Btn: TSpeedButton;
  Settings: TGridCellSettings;
begin
  Grid := TGridLayout.Create;
  Btn := TSpeedButton.Create(nil);

  Grid.Rows := 1;
  Grid.Columns := 1;
  Grid.RowHeights := 30;
  Grid.ColumnWidths := 40;

  Settings := TGridCellSettings.Create(0, 0);
  Settings.WithColumnSpan(2);
  Grid.AddItem(TControlGridItem.Create(Btn), Settings);
  Grid.ArrangeItems;

  AssertEquals('Largura incorreta com ColSpan ultrapassando ultima coluna', 40, Btn.Width);

  Btn.Free;
  Grid.Free;
end;

procedure TGridLayoutTest.TestTamanhoComponente_ComRowSpanExcedeUltimaLinha;
var
  Grid: TGridLayout;
  Btn: TSpeedButton;
  Settings: TGridCellSettings;
begin
  Grid := TGridLayout.Create;
  Btn := TSpeedButton.Create(nil);

  Grid.Rows := 2;
  Grid.Columns := 2;
  Grid.RowHeights := 30;
  Grid.ColumnWidths := 40;

  Settings := TGridCellSettings.Create(0, 0);
  Settings.WithRowSpan(3);
  Grid.AddItem(TControlGridItem.Create(Btn), Settings);
  Grid.ArrangeItems;

  AssertEquals('Altura incorreta com RowSpan ultrapassando ultima linha', 60, Btn.Height);

  Btn.Free;
  Grid.Free;
end;

procedure TGridLayoutTest.TestVisibilidadeComponente_AdicionadoForaDosLimitesDoGrid;
var
  Grid: TGridLayout;
  Btn: TSpeedButton;
  Settings: TGridCellSettings;
begin
  Grid := TGridLayout.Create;
  Btn := TSpeedButton.Create(nil);

  Grid.Rows := 1;
  Grid.Columns := 1;
  Grid.RowHeights := 30;
  Grid.ColumnWidths := 40;

  Settings := TGridCellSettings.Create(2, 2);
  Grid.AddItem(TControlGridItem.Create(Btn), Settings);
  Grid.ArrangeItems;

  AssertEquals('Componente fora dos limites do grid deve ficar invisível', False, Btn.Visible);

  Btn.Free;
  Grid.Free;
end;

procedure TGridLayoutTest.TestVisibilidadeComponente_AdicionadoDentroDosLimitesDoGrid;
var
  Grid: TGridLayout;
  Btn: TSpeedButton;
  Settings: TGridCellSettings;
begin
  Grid := TGridLayout.Create;
  Btn := TSpeedButton.Create(nil);

  Grid.Rows := 1;
  Grid.Columns := 1;
  Grid.RowHeights := 30;
  Grid.ColumnWidths := 40;

  Settings := TGridCellSettings.Create(0, 0);
  Grid.AddItem(TControlGridItem.Create(Btn), Settings);
  Grid.ArrangeItems;

  AssertEquals('Componente dentro dos limites do grid deve ficar visível',
    True, Btn.Visible);

  Btn.Free;
  Grid.Free;
end;

procedure TGridLayoutTest.TestExtraSize_NoEffectOnNonStretch;
var
  Grid: TGridLayout;
  Btn: TSpeedButton;
  Settings: TGridCellSettings;
begin
  Btn := TSpeedButton.Create(nil);
  Btn.Width := 20;
  Btn.Height := 20;

  Grid := TGridLayout.Create;
  Grid.Rows := 1;
  Grid.Columns := 1;
  Grid.RowHeight[0] := 30;
  Grid.ColumnWidths := 40;

  // Alinhamento Start
  Settings := TGridCellSettings.Create(0, 0);
  Settings.WithExtraWidth(15);
  Settings.WithExtraHeight(8);
  Settings.WithVerticalAlignment(laStart);
  Settings.WithHorizontalAlignment(laStart);

  Grid.AddItem(TControlGridItem.Create(Btn), Settings);
  Grid.ArrangeItems;

  AssertEquals('laStart - largura deve ignorar ExtraWidth', 20, Btn.Width);
  AssertEquals('laStart - altura deve ignorar ExtraHeight', 20, Btn.Height);

  // Alinhamento End
  Settings := TGridCellSettings.Create(0, 0);
  Settings.WithExtraWidth(15);
  Settings.WithExtraHeight(8);
  Settings.WithVerticalAlignment(laEnd);
  Settings.WithHorizontalAlignment(laEnd);

  Btn.Width := 25;
  Btn.Height := 25;
  Grid := TGridLayout.Create;
  Grid.Rows := 1;
  Grid.Columns := 1;
  Grid.RowHeight[0] := 30;
  Grid.ColumnWidths := 40;
  Grid.AddItem(TControlGridItem.Create(Btn), Settings);
  Grid.ArrangeItems;

  AssertEquals('laEnd - largura deve ignorar ExtraWidth', 25, Btn.Width);
  AssertEquals('laEnd - altura deve ignorar ExtraHeight', 25, Btn.Height);

  Btn.Free;
  Grid.Free;
end;

procedure TGridLayoutTest.TestExtraSize_WithOffset_StretchOnly;
var
  Grid: TGridLayout;
  Btn: TSpeedButton;
  Settings: TGridCellSettings;
begin
  Btn := TSpeedButton.Create(nil);

  Grid := TGridLayout.Create;
  Grid.Rows := 1;
  Grid.Columns := 1;
  Grid.RowHeight[0] := 40;
  Grid.ColumnWidths := 50;

  Settings := TGridCellSettings.Create(0, 0);
  Settings.WithExtraWidth(10);
  Settings.WithExtraHeight(5);
  Settings.WithOffsetX(3);
  Settings.WithOffsetY(4);
  Settings.WithVerticalAlignment(laStretch);
  Settings.WithHorizontalAlignment(laStretch);

  Grid.AddItem(TControlGridItem.Create(Btn), Settings);
  Grid.ArrangeItems;

  AssertEquals('Stretch + OffsetX + ExtraWidth: largura', 60, Btn.Width);
  AssertEquals('Stretch + OffsetY + ExtraHeight: altura', 45, Btn.Height);
  AssertEquals('Stretch + OffsetX: posição X', 3, Btn.Left);
  AssertEquals('Stretch + OffsetY: posição Y', 4, Btn.Top);

  Btn.Free;
  Grid.Free;
end;

procedure TGridLayoutTest.TestNestedGrid_PositionAndSize;
var
  MainGrid, SubGrid: TGridLayout;
  MainContainer,
  SubContainer: TWinControl;
  SubItem: TSubGridItem;
  Btn: TSpeedButton;
begin
  MainContainer := TPanel.Create(nil); // ou TPanel
  SubContainer := TGroupBox.Create(nil);
  try
    MainGrid := TGridLayout.Create;
    MainGrid.Rows := 2;
    MainGrid.Columns := 2;
    MainGrid.ColumnWidths := 200;
    MainGrid.RowHeights := 100;

    SubGrid := TGridLayout.Create;
    SubGrid.Rows := 2;
    SubGrid.Columns := 2;
    SubGrid.ColumnWidths := 80;
    SubGrid.RowHeights := 30;

    // Criar botão dentro do subgrid
    Btn := TSpeedButton.Create(SubContainer);
    Btn.Parent := SubContainer;
    Btn.Caption := 'A';
    SubGrid.AddItem(Btn, TGridCellSettings.Create(1, 1));

    SubGrid.ArrangeItems;

    SubItem := TSubGridItem.CreateWithContainerClass(SubGrid, MainContainer, TGroupBox);
    SubItem.Container.Parent := MainContainer;
    TGroupBox(SubItem.Container).Caption := 'SubGrid';

    SubItem.Container.Width := SubGrid.ContentWidth + 20;
    SubItem.Container.Height := SubGrid.ContentHeight + 20;

    MainGrid.AddItem(SubItem, TGridCellSettings.Create(1, 1));
    MainGrid.ArrangeItems;

    AssertEquals('SubContainer X', 200, SubItem.Container.Left);
    AssertEquals('SubContainer Y', 100, SubItem.Container.Top);
    AssertTrue('SubContainer Width > 0', SubItem.Container.Width > 0);
    AssertTrue('SubContainer Height > 0', SubItem.Container.Height > 0);
    AssertEquals('Button position X', 80, Btn.Left);
    AssertEquals('Button position Y', 30, Btn.Top);

    // Liberação
    Btn.Free;
    SubGrid.Free;
    MainGrid.Free;
  finally
    SubContainer.Free;
    MainContainer.Free;
  end;
end;

procedure TGridLayoutTest.TestSubGrid_WithVirtualContainer_PositionedCorrectly;
var
  MainGrid, SubGrid: TGridLayout;
  SubItem: TSubGridItemTest;
  Virtual: TVirtualContainer;
  SubButton: TSpeedButton;
begin
  // SubGrid aninhado
  SubGrid := TGridLayout.Create;
  SubGrid.Rows := 1;
  SubGrid.Columns := 1;
  SubGrid.RowHeights := 20;
  SubGrid.ColumnWidths := 50;

  // Virtual container
  Virtual := TVirtualContainer.Create(nil);
  Virtual.SetBounds(100, 200, 0, 0); // posição esperada do subgrid

  // SubItem com container virtual
  SubItem := TSubGridItemTest.Create(SubGrid);
  SubItem.FControl := Virtual; // simulando uso interno
  SubItem.FContainer := Virtual;

  // Adiciona botão ao subgrid
  SubButton := TSpeedButton.Create(nil);
  SubGrid.AddItem(SubButton, TGridCellSettings.Create(0, 0));

  // Simula posicionamento do SubGrid no grid principal
  SubItem.SetBounds(100, 200, 50, 20);

  // Checagens
  AssertEquals('SubButton.Left deve estar na posição virtual X', 100, SubButton.Left);
  AssertEquals('SubButton.Top deve estar na posição virtual Y', 200, SubButton.Top);
  AssertEquals('SubButton.Width deve refletir o tamanho do SubGrid', 50, SubButton.Width);
  AssertEquals('SubButton.Height deve refletir o tamanho do SubGrid', 20, SubButton.Height);

  // Cleanup
  SubButton.Free;
  SubItem.Free;
  SubGrid.Free;
end;

procedure TGridLayoutTest.TestMoveKey_From1To3;
type
  TDict = specialize TIntegerKeyDictionary<Integer>;
var
  Dict: TDict;
  I: Integer;
begin
  Dict := TDict.Create;
  try
    // Dados iniciais: [1→10, 2→20, 3→30, 4→40, 5→50]
    Dict.Add(1, 10);
    Dict.Add(2, 20);
    Dict.Add(3, 30);
    Dict.Add(4, 40);
    Dict.Add(5, 50);

    Dict.MoveKey(1, 3);

    // Esperado: [1→20, 2→30, 3→10, 4→40, 5→50]
    AssertTrue(Dict[1] = 20);
    AssertTrue(Dict[2] = 30);
    AssertTrue(Dict[3] = 10);
    AssertTrue(Dict[4] = 40);
    AssertTrue(Dict[5] = 50);
  finally
    Dict.Free;
  end;
end;

procedure TGridLayoutTest.TestMoveKey_From3To1;
type
  TDict = specialize TIntegerKeyDictionary<Integer>;
var
  Dict: TDict;
begin
  Dict := TDict.Create;
  try
    // Dados iniciais: [1→10, 2→20, 3→30, 4→40, 5→50]
    Dict.Add(1, 10);
    Dict.Add(2, 20);
    Dict.Add(3, 30);
    Dict.Add(4, 40);
    Dict.Add(5, 50);

    Dict.MoveKey(3, 1);

    // Esperado: [1→30, 2→10, 3→20, 4→40, 5→50]
    AssertTrue(Dict[1] = 30);
    AssertTrue(Dict[2] = 10);
    AssertTrue(Dict[3] = 20);
    AssertTrue(Dict[4] = 40);
    AssertTrue(Dict[5] = 50);
  finally
    Dict.Free;
  end;
end;


procedure TGridLayoutTest.TestMoveKey_From3To1_DontExistFrom;
type
  TDict = specialize TIntegerKeyDictionary<Integer>;
var
  Dict: TDict;
  I: Integer;
begin
  Dict := TDict.Create;
  try
    // Dados iniciais: [1→10, 2→20, 3→30, 4→40, 5→50]
    Dict.Add(1, 10);
    Dict.Add(2, 20);
    // Dict.Add(3, 30);  // não usa a chave 3, mas mesmo assim tem que poder 'mover' como se existisse
    Dict.Add(4, 40);
    Dict.Add(5, 50);
    Dict.Add(6, 60);

    Dict.MoveKey(3, 1);

    // Esperado: [1→X, 2→10, 3→20, 4→40, 5→50]
    AssertTrue(not Dict.TryGetValue(1, I)); // o 3 que não existia sobrepoem 1 (remove)
    AssertTrue(Dict[2] = 10);
    AssertTrue(Dict[3] = 20);
    AssertTrue(Dict[4] = 40);
    AssertTrue(Dict[5] = 50);
    AssertTrue(Dict[6] = 60);
  finally
    Dict.Free;
  end;
end;

procedure TGridLayoutTest.TestInsertAt_TGridTrackInfoDictionaryHelper;
var
  Dic: TGridTrackInfoDictionary;
  InfoA, InfoB, InfoC, InfoD, InfoRes:  TGridTrackInfo;
begin
  InfoA := TGridTrackInfo.Default;
  InfoB := TGridTrackInfo.Default;
  InfoC := TGridTrackInfo.Default;
  InfoD := TGridTrackInfo.Default;
  InfoRes := TGridTrackInfo.Default;

  Dic := TGridTrackInfoDictionary.Create;
  try
    InfoA.Size := 10;
    Dic.AddOrSetValue(0, InfoA);          // 0    // não deve mover

    InfoB.Shift := 3;
    Dic.AddOrSetValue(1, InfoB);          // 1    // não deve mover

    InfoC.Size := 8;
    InfoC.Shift := 2;
    Dic.AddOrSetValue(4, InfoC);          // 4    // Deve mover para pos 5

    InfoD.Spacing := 9;
    Dic.AddOrSetValue(7, InfoD);          // 7    // Deve mover para pos 8

    Dic.InsertTrackAt(3);                      // 3: move 4, 7 to 5, 8

    // Testa se o item 0 continua na posição 0
    if Dic.TryGetValue(0, InfoRes) then
      AssertEquals(
        'Item na chave 0 não tem o ''Size'' esperado',
        10,
        InfoRes.Size.Value
      )
    else
      Fail('Deveria existir item na chave 0');

    // Testa se o item 1 continua na posição 0
    if Dic.TryGetValue(1, InfoRes) then
      AssertEquals(
        'Item na chave 1 não tem o ''Shift'' esperado',
        3,
        InfoRes.Shift.Value
      )
    else
      Fail('Deveria existir item na chave 0');

    // Testa se a chave 4 não existe mais (pois foi movido)
    AssertEquals(
      'Chave 4 deveria ter sido movida para posição 5',
      False,
      Dic.TryGetValue(4, InfoRes)
    );

    // Testa se a chave 5 existe (pois espera-se que 4 seja movido para 5)
    AssertEquals(
      'Chave 5 deveria existir',
      True,
      Dic.TryGetValue(5, InfoRes)
    );

    // Testa se o valor atribuido na chave 4, é o mesmo depois de movido para a chave 5
    Dic.TryGetValue(5, InfoRes);
    AssertEquals(
      'Item na chave 5 diferente do esperado',
      8,
      InfoRes.Size.Value
    );
  finally
    Dic.Free;
  end;
end;

initialization
  RegisterTest(TGridLayoutTest);

end.

