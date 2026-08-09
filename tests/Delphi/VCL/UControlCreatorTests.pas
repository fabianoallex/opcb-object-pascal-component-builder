unit UControlCreatorTests;

interface

uses
  DUnitX.TestFramework, Vcl.Forms, Vcl.Controls, Vcl.ExtCtrls, Vcl.StdCtrls, OPCB;

type
  [TestFixture]
  TControlCreatorTest = class
  private
    FForm: TForm;
    procedure ExternalMethod(const ACreator: TControlCreator);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test] procedure TestTopLeft;
    [Test] procedure TestTopLeftHorizontally;
    [Test] procedure TestTopLeftVertically;
    [Test] procedure TestIncTop;
    [Test] procedure TestIncLeft;
    [Test] procedure TestBreakHorizontally;
    [Test] procedure TestBreakVertically;
    [Test] procedure TestBreakHorizontallyWithIncrement;
    [Test] procedure TestBreakVerticallyWithIncrement;
    [Test] procedure TestBreakLineWithIncrement;
    [Test] procedure TestBreakColumnWithIncrement;
    [Test] procedure TestBreakAfterTopLeft;
    [Test] procedure TestTopAfterDiferentControlsHightsAndBreak;
    [Test] procedure TestLeftAfterDiferentControlsWidthsAndBreak;
    [Test] procedure TestSpaceHorizontally;
    [Test] procedure TestSpaceVertically;
    [Test] procedure TestSetControlHeight;
    [Test] procedure TestUnsetControlHeight;
    [Test] procedure TestSetControlWidth;
    [Test] procedure TestUnsetControlWidth;
    [Test] procedure TestExternal;
    [Test] procedure TestAddToRegistry;
    [Test] procedure TestRemoveControlFromRegistryOnDestroy;
    [Test] procedure TestReturnLastControlOnEmptyCreatorDoesNotRaise;
    [Test] procedure TestControlsIsolatedPerCreatorInSharedContext;
    [Test] procedure TestSubLevel;
    [Test] procedure TestSubLevelSubLevel;
    [Test] procedure TestSubLevelSuperLevel;
    [Test] procedure TestSubLevelEmpty;
    [Test] procedure TestSubLevelDirectionSuperLevelDirection;
    [Test] procedure TestRecalcParentSize;
    [Test] procedure TestRecalcParentSizeWithExtraSizes;
    [Test] procedure TestGridMode;
    [Test] procedure TestGridModeIgnoreAddAfterEndOfGrid;
    [Test] procedure TestGridModeNotIgnoreAddAfterEndOfGridWhenAutoExpand;
    [Test] procedure TestGridModeAutoExpandRows;
    [Test] procedure TestGridModeAutoExpandCols;
    [Test] procedure TestGridModeAutoExpandOnlyRows;
    [Test] procedure TestGridModeAutoExpandOnlyCols;
    [Test] procedure TestGridModeWithSubLevel;
    [Test] procedure TestGridSkipCell;
    [Test] procedure TestGridSkipCells;
    [Test] procedure TestGridRowHeight;
    [Test] procedure TestGridColWidth;
    [Test] procedure TestGridRowSpan;
    [Test] procedure TestGridColSpan;
    [Test] procedure TestGridRowSpanOnlyOnce;
    [Test] procedure TestGridColSpanOnlyOnce;
    [Test] procedure TestGridRowSpanOutOfBounds;
    [Test] procedure TestGridColSpanOutOfBounds;
    [Test] procedure TestGridRowSpanExpandGridRows;
    [Test] procedure TestGridColSpanExpandGridCols;
    [Test] procedure TestGridGotoCell;
    [Test] procedure TestGridRowSpanWithSpace;
    [Test] procedure TestGridColSpanWithSpace;
    [Test] procedure TestCellStrechHorizontal;
    [Test] procedure TestCellStrechVertical;
    [Test] procedure TestCellStrechAll;
    [Test] procedure TestCellNoStrech;
    [Test] procedure TestCellNoStrechCenter;
    [Test] procedure TestCellNoStrechTop;
    [Test] procedure TestCellNoStrechTopRight;
    [Test] procedure TestCellNoStrechRight;
    [Test] procedure TestCellNoStrechBottomRight;
    [Test] procedure TestCellNoStrechBottom;
    [Test] procedure TestCellNoStrechBottomLeft;
    [Test] procedure TestCellNoStrechLeft;
    [Test] procedure TestCellNoStrechTopLeft;
    [Test] procedure TestGridRowOffset;
    [Test] procedure TestGridColOffset;
    [Test] procedure TestGridBreakLine;
    [Test] procedure TestGridBreakColumn;
    [Test] procedure TestGrid1x1;
    [Test] procedure TestGridGoToLastCell;
    [Test] procedure TestGridSpanOverlapRaises;
    [Test] procedure TestBreakLineOnLastRowIsNoOp;
    [Test] procedure TestBreakColumnOnLastColIsNoOp;
    [Test] procedure TestSubLevelWithNonWinControlRaises;
    [Test] procedure TestAddWithGridFullDiscardsOutReference;
    [Test] procedure TestAlignControlsRight;
    [Test] procedure TestAlignControlsRightWithPadding;
    [Test] procedure TestSetOwnerAndParentDirectly;
    [Test] procedure TestSetParentDirectly;
    [Test] procedure TestGetControlGeneric;
  end;

implementation

uses
  System.SysUtils;

procedure TControlCreatorTest.ExternalMethod(const ACreator: TControlCreator);
var
  ControlBuilder: TControlBuilder;
begin
  ControlBuilder := TControlBuilder.Create(TPanel, 'PanelTest');
  ControlBuilder.WithCaption('EXTERNAL-TEST');
  ACreator.Add<TControl>(ControlBuilder);
end;

procedure TControlCreatorTest.Setup;
begin
  FForm := TForm.Create(nil);
end;

procedure TControlCreatorTest.TearDown;
begin
  FForm.Free;
end;

procedure TControlCreatorTest.TestTopLeft;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .SetTopLeft(10, 20)
      .Add(TControlBuilder.Create(TPanel, P));

    Assert.AreEqual(10, P.Top, 'Propriedade Top diferente da esperada');
    Assert.AreEqual(20, P.Left, 'Propriedade Left diferente da esperada');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestTopLeftHorizontally;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .SetTopLeft(10, 20)
      .SetDirection(cpdHorizontal)
      .Add(TControlBuilder.Create(TPanel).WithWidth(15).WithHeight(15))
      .Add(TControlBuilder.Create(TPanel, P))
    ;
    Assert.AreEqual(10 + 0, P.Top, 'Propriedade Top diferente da esperada');
    Assert.AreEqual(20 + 15, P.Left, 'Propriedade Left diferente da esperada');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestTopLeftVertically;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .SetTopLeft(10, 20)
      .SetDirection(cpdVertical)
      .Add(TControlBuilder.Create(TPanel).WithWidth(15).WithHeight(15))
      .Add(TControlBuilder.Create(TPanel, P))
    ;
    Assert.AreEqual(10 + 15, P.Top, 'Propriedade Top diferente da esperada');
    Assert.AreEqual(20 + 0, P.Left, 'Propriedade Left diferente da esperada');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestIncTop;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .SetTopLeft(10, 20)
      .IncTop(13)
      .Add(TControlBuilder.Create(TPanel, P))
    ;

    Assert.AreEqual(10 + 13, P.Top, 'Propriedade Top diferente da esperada');
    Assert.AreEqual(20, P.Left, 'Propriedade Left diferente da esperada');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestIncLeft;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .SetTopLeft(10, 20)
      .IncLeft(13)
      .Add(TControlBuilder.Create(TPanel, P))
    ;

    Assert.AreEqual(10, P.Top, 'Propriedade Top diferente da esperada');
    Assert.AreEqual(20 + 13, P.Left, 'Propriedade Left diferente da esperada');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestBreakHorizontally;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(15, 10))
      .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(15, 10))
      .Break
      .Add(TControlBuilder.Create(TPanel, P))
    ;
    Assert.AreEqual(10, P.Top, 'Propriedade Top diferente da esperada');
    Assert.AreEqual(0, P.Left, 'Propriedade Left diferente da esperada');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestBreakVertically;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .SetDirection(cpdVertical)
      .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(15, 10))
      .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(15, 10))
      .Break
      .Add(TControlBuilder.Create(TPanel, P))
    ;
    Assert.AreEqual(0, P.Top, 'Propriedade Top diferente da esperada');
    Assert.AreEqual(15, P.Left, 'Propriedade Left diferente da esperada');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestBreakHorizontallyWithIncrement;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(15, 10))
      .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(15, 10))
      .Break(5)
      .Add(TControlBuilder.Create(TPanel, P))
    ;
    Assert.AreEqual(10 + 5, P.Top, 'Propriedade Top diferente da esperada');
    Assert.AreEqual(0, P.Left, 'Propriedade Left diferente da esperada');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestBreakVerticallyWithIncrement;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .SetDirection(cpdVertical)
      .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(15, 10))
      .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(15, 10))
      .Break(5)
      .Add(TControlBuilder.Create(TPanel, P))
    ;
    Assert.AreEqual(0, P.Top, 'Propriedade Top diferente da esperada');
    Assert.AreEqual(15 + 5, P.Left, 'Propriedade Left diferente da esperada');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestBreakLineWithIncrement;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(15, 10))
      .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(15, 10))
      .BreakLine(5)
      .Add(TControlBuilder.Create(TPanel, P))
    ;
    Assert.AreEqual(10 + 5, P.Top, 'Propriedade Top diferente da esperada');
    Assert.AreEqual(0, P.Left, 'Propriedade Left diferente da esperada');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestBreakColumnWithIncrement;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .SetDirection(cpdVertical)
      .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(15, 10))
      .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(15, 10))
      .BreakColumn(5)
      .Add(TControlBuilder.Create(TPanel, P))
    ;
    Assert.AreEqual(0, P.Top, 'Propriedade Top diferente da esperada');
    Assert.AreEqual(15 + 5, P.Left, 'Propriedade Left diferente da esperada');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestBreakAfterTopLeft;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(15, 10))
      .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(15, 10))
      .SetTopLeft(40, 30) // muda o local de referencia do break
      .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(5, 5))
      .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(5, 5))
      .Break
      .Add(TControlBuilder.Create(TPanel, P))
    ;
    Assert.AreEqual(40 + 5, P.Top, 'Propriedade Top diferente da esperada');
    Assert.AreEqual(30, P.Left, 'Propriedade Left diferente da esperada');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestTopAfterDiferentControlsHightsAndBreak;
var
  ControlCreator: TControlCreator;
  P1, P2: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .Add(TControlBuilder.Create(TPanel).WithHeight(15))
      .Add(TControlBuilder.Create(TPanel).WithHeight(25)) // maior incluido primeiro
      .Break
      .Add(TControlBuilder.Create(TPanel, P1).WithHeight(5))
    ;
    Assert.AreEqual(25, P1.Top, 'Propriedade Top diferente da esperada para P1');

    ControlCreator
      .Add(TControlBuilder.Create(TPanel).WithHeight(25)) // maior incluido segundo
      .Add(TControlBuilder.Create(TPanel).WithHeight(15))
      .Break
      .Add(TControlBuilder.Create(TPanel, P2))
    ;
    Assert.AreEqual(25 + 25, P2.Top, 'Propriedade Top diferente da esperada para P2');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestLeftAfterDiferentControlsWidthsAndBreak;
var
  ControlCreator: TControlCreator;
  P1, P2: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .SetDirection(cpdVertical)
      .Add(TControlBuilder.Create(TPanel).WithWidth(15))
      .Add(TControlBuilder.Create(TPanel).WithWidth(25)) // maior incluido primeiro
      .Break
      .Add(TControlBuilder.Create(TPanel, P1).WithWidth(5))
    ;
    Assert.AreEqual(25, P1.Left, 'Propriedade Left diferente da esperada para P1');

    ControlCreator
      .Add(TControlBuilder.Create(TPanel).WithWidth(25)) // maior incluido segundo
      .Add(TControlBuilder.Create(TPanel).WithWidth(15))
      .Break
      .Add(TControlBuilder.Create(TPanel, P2))
    ;
    Assert.AreEqual(25 + 25, P2.Left, 'Propriedade Left diferente da esperada para P2');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestSpaceHorizontally;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .SetSpace(7, 8)
      .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(15, 10))
      .Add(TControlBuilder.Create(TPanel, P))
    ;
    Assert.AreEqual(0, P.Top, 'Propriedade Top diferente da esperada');
    Assert.AreEqual(15 + 8, P.Left, 'Propriedade Left diferente da esperada');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestSpaceVertically;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .SetDirection(cpdVertical)
      .SetSpace(7, 8)
      .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(15, 10))
      .Add(TControlBuilder.Create(TPanel, P))
    ;
    Assert.AreEqual(10 + 7, P.Top, 'Propriedade Top diferente da esperada');
    Assert.AreEqual(0, P.Left, 'Propriedade Left diferente da esperada');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestSetControlHeight;
var
  ControlCreator: TControlCreator;
  P1, P2: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .SetTopLeft(10, 20)
      .SetControlHeight(33)
      .Add(TControlBuilder.Create(TPanel, P1).WithHeight(44)) // tem que sobrepor com 33
      .Add(TControlBuilder.Create(TPanel, P2))
    ;

    Assert.AreEqual(33, P1.Height, 'Propriedade Height de P1 diferente da esperada');
    Assert.AreEqual(33, P2.Height, 'Propriedade Height de P2 diferente da esperada');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestUnsetControlHeight;
var
  ControlCreator: TControlCreator;
  P1, P2: TPanel;
  DefaultPanel: TPanel;
begin
  // O valor default de Height de um TPanel recem-criado difere entre VCL e
  // LCL, entao comparamos contra um TPanel de referencia em vez de um
  // literal fixo.
  DefaultPanel := TPanel.Create(nil);
  try
    ControlCreator := TControlCreator.Create;
    try
      ControlCreator
        .SetOwnerAndParent(FForm, FForm)
        .SetTopLeft(10, 20)
        .SetControlHeight(33)
        .Add(TControlBuilder.Create(TPanel).WithHeight(44))
        .Add(TControlBuilder.Create(TPanel))
        .UnsetControlHeight
        .Add(TControlBuilder.Create(TPanel, P1).WithHeight(44)) // agora tem que ser considerado os 44
        .Add(TControlBuilder.Create(TPanel, P2))
      ;

      Assert.AreEqual(44, P1.Height, 'Propriedade Height de P1 diferente da esperada');
      Assert.AreEqual(DefaultPanel.Height, P2.Height, 'Propriedade Height de P2 diferente da esperada');
    finally
      ControlCreator.Free;
    end;
  finally
    DefaultPanel.Free;
  end;
end;

procedure TControlCreatorTest.TestSetControlWidth;
var
  ControlCreator: TControlCreator;
  P1, P2: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .SetTopLeft(10, 20)
      .SetControlWidth(33)
      .Add(TControlBuilder.Create(TPanel, P1).WithWidth(44)) // tem que sobrepor com 33
      .Add(TControlBuilder.Create(TPanel, P2))
    ;

    Assert.AreEqual(33, P1.Width, 'Propriedade Width de P1 diferente da esperada');
    Assert.AreEqual(33, P2.Width, 'Propriedade Width de P2 diferente da esperada');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestUnsetControlWidth;
var
  ControlCreator: TControlCreator;
  P1, P2: TPanel;
  DefaultPanel: TPanel;
begin
  // Ver comentario em TestUnsetControlHeight: Width default tambem difere
  // entre VCL e LCL.
  DefaultPanel := TPanel.Create(nil);
  try
    ControlCreator := TControlCreator.Create;
    try
      ControlCreator
        .SetOwnerAndParent(FForm, FForm)
        .SetTopLeft(10, 20)
        .SetControlWidth(33)
        .Add(TControlBuilder.Create(TPanel).WithWidth(44))
        .Add(TControlBuilder.Create(TPanel))
        .UnsetControlWidth
        .Add(TControlBuilder.Create(TPanel, P1).WithWidth(44)) // agora tem que ser considerado os 44
        .Add(TControlBuilder.Create(TPanel, P2))
      ;

      Assert.AreEqual(44, P1.Width, 'Propriedade Width de P1 diferente da esperada');
      Assert.AreEqual(DefaultPanel.Width, P2.Width, 'Propriedade Width de P2 diferente da esperada');
    finally
      ControlCreator.Free;
    end;
  finally
    DefaultPanel.Free;
  end;
end;

procedure TControlCreatorTest.TestExternal;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    P := nil;

    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .External(ExternalMethod) // inclui um Panel com Caption 'EXTERNAL-TEST'
    ;

    P := ControlCreator.GetControl('PanelTest') as TPanel;

    Assert.IsNotNull(P, 'Variavel P nao deveria ser nil');
    Assert.AreEqual('EXTERNAL-TEST', P.Caption, 'Propriedade Caption diferente da esperada');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestAddToRegistry;
var
  ControlCreator: TControlCreator;
begin
  ControlCreator := TControlCreator.Create;
  try
    Assert.AreEqual(0, ControlCreator.Controls.Count,
      'A quantidade de controles no registro diferente do esperado');

    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .Add(TControlBuilder.Create(TPanel))
    ;

    Assert.AreEqual(1, ControlCreator.Controls.Count,
      'A quantidade de controles no registro diferente do esperado');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestRemoveControlFromRegistryOnDestroy;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    Assert.AreEqual(0, ControlCreator.Controls.Count,
      'A quantidade de controles no registro diferente do esperado');

    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .Add(TControlBuilder.Create(TPanel, 'panel_test'))
    ;

    P := ControlCreator.GetControl('panel_test') as TPanel;
    Assert.AreEqual(1, ControlCreator.Controls.Count,
      'A quantidade de controles no registro diferente do esperado');

    // ao destruir o objeto, automaticamente deve ser removido do registro
    P.Free;

    Assert.AreEqual(0, ControlCreator.Controls.Count,
      'A quantidade de controles no registro diferente do esperado');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestReturnLastControlOnEmptyCreatorDoesNotRaise;
var
  ControlCreator: TControlCreator;
  Ctrl: TControl;
begin
  // Regressao: "if Self.Controls.Count > 0 then;" tinha um ";" solto que
  // tornava a guarda inerte, causando excecao ao acessar .Last de uma
  // lista vazia.
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator.ReturnLastControl(Ctrl);
    Assert.IsNull(Ctrl, 'Ctrl deveria ser nil quando nao ha nenhum controle criado');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestControlsIsolatedPerCreatorInSharedContext;
var
  CreatorA, CreatorB: TControlCreator;
begin
  // Regressao: Controls devolvia Registry.Controls (todo o contexto
  // compartilhado), entao um creator enxergava controles criados por outro
  // creator que usasse a mesma chave de contexto.
  CreatorA := TControlCreator.Create('SharedContextTest');
  CreatorB := TControlCreator.Create('SharedContextTest');
  try
    CreatorA
      .SetOwnerAndParent(FForm, FForm)
      .Add(TControlBuilder.Create(TPanel))
    ;
    CreatorB
      .SetOwnerAndParent(FForm, FForm)
      .Add(TControlBuilder.Create(TPanel))
      .Add(TControlBuilder.Create(TPanel))
    ;

    Assert.AreEqual(1, CreatorA.Controls.Count,
      'CreatorA deveria ver so os controles que ele mesmo criou');
    Assert.AreEqual(2, CreatorB.Controls.Count,
      'CreatorB deveria ver so os controles que ele mesmo criou');
  finally
    CreatorA.Free;
    CreatorB.Free;
  end;
end;

procedure TControlCreatorTest.TestSubLevel;
var
  ControlCreator: TControlCreator;
  InitialLevel, NewLevel: TControlCreatorLevel;
begin
  ControlCreator := TControlCreator.Create;
  try
    InitialLevel := ControlCreator.CurrentLevel;

    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .Add(TControlBuilder.Create(TPanel).WithHeight(50).WithLeft(60))
      .SubLevel;

    NewLevel := ControlCreator.CurrentLevel;

    Assert.IsTrue(InitialLevel <> NewLevel,
      Format('Level inicial deve ser diferente do level atual. [%s, %s]',
        [InitialLevel.GroupName, NewLevel.GroupName]));
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestSubLevelSubLevel;
var
  ControlCreator: TControlCreator;
  InitialLevel, NewLevel_01, NewLevel_02: TControlCreatorLevel;
begin
  ControlCreator := TControlCreator.Create;
  try
    InitialLevel := ControlCreator.CurrentLevel;

    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .Add(TControlBuilder.Create(TPanel).WithHeight(50).WithLeft(60))
      .SubLevel;

    NewLevel_01 := ControlCreator.CurrentLevel;

    ControlCreator.SubLevel;

    NewLevel_02 := ControlCreator.CurrentLevel;

    Assert.IsTrue(InitialLevel <> NewLevel_01,
      Format('Level inicial deve ser diferente do novo level 01. [%s, %s]',
        [InitialLevel.GroupName, NewLevel_01.GroupName]));

    Assert.IsTrue(NewLevel_01 <> NewLevel_02,
      Format('Novo Level 01 deve ser diferente do novo level 02. [%s, %s]',
        [NewLevel_01.GroupName, NewLevel_02.GroupName]));
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestSubLevelSuperLevel;
var
  ControlCreator: TControlCreator;
  InitialLevel, InitialLevelBack: TControlCreatorLevel;
begin
  ControlCreator := TControlCreator.Create;
  try
    InitialLevel := ControlCreator.CurrentLevel;

    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .SubLevel
      .SuperLevel;

    InitialLevelBack := ControlCreator.CurrentLevel;

    Assert.IsTrue(InitialLevel = InitialLevelBack,
      Format('Level inicial deve ser igual ao level atual. [%s, %s]',
        [InitialLevel.GroupName, InitialLevelBack.GroupName]));
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestSubLevelEmpty;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .SetSpace(5, 6)
      .SubLevel
      .SuperLevel
      .Add(TControlBuilder.Create(TPanel, P));

    Assert.AreEqual(0, P.Top, 'Propriedade Top diferente da esperada');
    Assert.AreEqual(0, P.Left, 'Propriedade Left diferente da esperada');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestSubLevelDirectionSuperLevelDirection;
var
  ControlCreator: TControlCreator;
  P1, P2: TPanel;
  InitialDirection, SubLevelDirection, SuperLevelDirection: TControlCreatorDirection;
begin
  ControlCreator := TControlCreator.Create;
  try
    InitialDirection := ControlCreator.CurrentLevel.Direction;

    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .SetControlWidth(80)
      .SetControlHeight(50)
      .SetDirection(cpdHorizontal)
      .Add(TControlBuilder.Create(TPanel))
      .SubLevel
        .SetControlWidth(80)
        .SetControlHeight(50)
        .SetDirection(cpdVertical)
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel, P1))
    ;

    SubLevelDirection := ControlCreator.CurrentLevel.Direction;

    ControlCreator
      .SuperLevel
      .Add(TControlBuilder.Create(TPanel, P2))
    ;

    SuperLevelDirection := ControlCreator.CurrentLevel.Direction;

    Assert.IsTrue(SuperLevelDirection = cpdHorizontal, 'InitialLevel com direcao diferente da esperada.');
    Assert.IsTrue(SubLevelDirection = cpdVertical, 'SubLevel com direcao diferente da esperada.');
    Assert.IsTrue(SuperLevelDirection = cpdHorizontal, 'SuperLevel com direcao diferente da esperada.');
    Assert.IsTrue(InitialDirection = SuperLevelDirection,
      'InitialLevel e SuperLevel deveriam ter a mesma direcao');

    Assert.AreEqual(50, P1.Top, 'Propriedade Top do Panel diferente da esperada');
    Assert.AreEqual(80 + 80, P2.Left, 'Propriedade Left do Panel diferente da esperada');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestRecalcParentSize;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .SubLevel(TControlBuilder.Create(TPanel, P).WithHeight(10).WithLeft(15))
        .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(20, 25))
        .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(20, 25))
        .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(20, 25))
        .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(20, 25))
        .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(20, 25))
        .Break
        .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(20, 25))
        .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(20, 25))
        .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(20, 25))
        .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(20, 25))
        .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(20, 25))
        .RecalcParentSize
      .SuperLevel
    ;

    Assert.AreEqual(25 * 2, P.Height, 'Propriedade Height diferente da esperada');
    Assert.AreEqual(20 * 5, P.Width, 'Propriedade Width diferente da esperada');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestRecalcParentSizeWithExtraSizes;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .SubLevel(TControlBuilder.Create(TPanel, P).WithHeight(10).WithLeft(15))
        .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(20, 25))
        .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(20, 25))
        .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(20, 25))
        .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(20, 25))
        .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(20, 25))
        .Break
        .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(20, 25))
        .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(20, 25))
        .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(20, 25))
        .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(20, 25))
        .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(20, 25))
        .RecalcParentSize(12, 17)
      .SuperLevel
    ;

    Assert.AreEqual(25 * 2 + 12, P.Height, 'Propriedade Height diferente da esperada');
    Assert.AreEqual(20 * 5 + 17, P.Width, 'Propriedade Width diferente da esperada');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestGridMode;
var
  ControlCreator: TControlCreator;
  P1, P2, P3, P4: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .SetDirection(cpdHorizontal)
      .GridInit(2, 2)
        .GridSetCellWidthAndHeight(6, 7)
        .Add(TControlBuilder.Create(TPanel, P1))
        .Add(TControlBuilder.Create(TPanel, P2))
        .Add(TControlBuilder.Create(TPanel, P3))
        .Add(TControlBuilder.Create(TPanel, P4))
      .GridFinish
    ;

    Assert.AreEqual(0, P1.Top, 'Propriedade Top diferente da esperada para P1');
    Assert.AreEqual(0, P2.Top, 'Propriedade Top diferente da esperada para P2');
    Assert.AreEqual(7, P3.Top, 'Propriedade Top diferente da esperada para P3');
    Assert.AreEqual(7, P4.Top, 'Propriedade Top diferente da esperada para P4');

    Assert.AreEqual(0, P1.Left, 'Propriedade Left diferente da esperada para P1');
    Assert.AreEqual(6, P2.Left, 'Propriedade Left diferente da esperada para P2');
    Assert.AreEqual(0, P3.Left, 'Propriedade Left diferente da esperada para P3');
    Assert.AreEqual(6, P4.Left, 'Propriedade Left diferente da esperada para P4');

    Assert.AreEqual(7, P1.Height, 'Propriedade Height diferente da esperada para P1');
    Assert.AreEqual(7, P2.Height, 'Propriedade Height diferente da esperada para P2');
    Assert.AreEqual(7, P3.Height, 'Propriedade Height diferente da esperada para P3');
    Assert.AreEqual(7, P4.Height, 'Propriedade Height diferente da esperada para P4');

    Assert.AreEqual(6, P1.Width, 'Propriedade Width diferente da esperada para P1');
    Assert.AreEqual(6, P2.Width, 'Propriedade Width diferente da esperada para P2');
    Assert.AreEqual(6, P3.Width, 'Propriedade Width diferente da esperada para P3');
    Assert.AreEqual(6, P4.Width, 'Propriedade Width diferente da esperada para P4');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestGridModeIgnoreAddAfterEndOfGrid;
var
  ControlCreator: TControlCreator;
  P1, P2, P3, P4, P5: TPanel;
begin
  P1 := nil;
  P2 := nil;
  P3 := nil;
  P4 := nil;
  P5 := nil;

  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .SetDirection(cpdHorizontal)
      .GridInit(2, 2)
        .GridSetCellWidthAndHeight(6, 7)
        .Add(TControlBuilder.Create(TPanel, P1))
        .Add(TControlBuilder.Create(TPanel, P2))
        .Add(TControlBuilder.Create(TPanel, P3))
        .Add(TControlBuilder.Create(TPanel, P4))
        .Add(TControlBuilder.Create(TPanel, P5)) // deve ignorar pois ja finalizou o grid
      .GridFinish
    ;

    Assert.IsNotNull(P1, 'P1 nao deveria ser nil');
    Assert.IsNotNull(P2, 'P2 nao deveria ser nil');
    Assert.IsNotNull(P3, 'P3 nao deveria ser nil');
    Assert.IsNotNull(P4, 'P4 nao deveria ser nil');
    Assert.IsNull(P5, 'P5 deveria ser nil');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestGridModeNotIgnoreAddAfterEndOfGridWhenAutoExpand;
var
  ControlCreator: TControlCreator;
  P1, P2, P3, P4, P5: TPanel;
begin
  P1 := nil;
  P2 := nil;
  P3 := nil;
  P4 := nil;
  P5 := nil;

  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .SetDirection(cpdHorizontal)
      .GridInit(2, 2)
        .GridAutoExpand
        .GridSetCellWidthAndHeight(6, 7)
        .Add(TControlBuilder.Create(TPanel, P1))
        .Add(TControlBuilder.Create(TPanel, P2))
        .Add(TControlBuilder.Create(TPanel, P3))
        .Add(TControlBuilder.Create(TPanel, P4))
        .Add(TControlBuilder.Create(TPanel, P5)) // ao adicionar esse registro grid se expande
      .GridFinish
    ;

    Assert.IsNotNull(P1, 'P1 nao deveria ser nil');
    Assert.IsNotNull(P2, 'P2 nao deveria ser nil');
    Assert.IsNotNull(P3, 'P3 nao deveria ser nil');
    Assert.IsNotNull(P4, 'P4 nao deveria ser nil');
    Assert.IsNotNull(P5, 'P5 nao deveria ser nil');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestGridModeAutoExpandRows;
var
  ControlCreator: TControlCreator;
  RowsBeforeExpand: Integer;
  RowsAfterExpand: Integer;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .SetDirection(cpdHorizontal)
      .GridInit(2, 2)
        .GridAutoExpand
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .GridReturnNumberOfRows(RowsBeforeExpand)
        .Add(TControlBuilder.Create(TPanel)) // ao adicionar esse registro grid se expande
        .GridReturnNumberOfRows(RowsAfterExpand)
      .GridFinish
    ;

    Assert.AreEqual(2, RowsBeforeExpand,
      'O numero de linhas do grid antes de expandir esta diferente do esperado');
    Assert.AreEqual(3, RowsAfterExpand,
      'O numero de linhas do grid depois de expandir esta diferente do esperado');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestGridModeAutoExpandCols;
var
  ControlCreator: TControlCreator;
  ColsBeforeExpand: Integer;
  ColsAfterExpand: Integer;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .SetDirection(cpdVertical)
      .GridInit(2, 2)
        .GridAutoExpand
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .GridReturnNumberOfCols(ColsBeforeExpand)
        .Add(TControlBuilder.Create(TPanel)) // ao adicionar esse registro grid se expande
        .GridReturnNumberOfCols(ColsAfterExpand)
      .GridFinish
    ;

    Assert.AreEqual(2, ColsBeforeExpand,
      'O numero de colunas do grid antes de expandir esta diferente do esperado');
    Assert.AreEqual(3, ColsAfterExpand,
      'O numero de colunas do grid depois de expandir esta diferente do esperado');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestGridModeAutoExpandOnlyRows;
var
  ControlCreator: TControlCreator;
  RowsBeforeExpand: Integer;
  RowsAfterExpand: Integer;
  ColsBeforeExpand: Integer;
  ColsAfterExpand: Integer;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  P := nil;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .SetDirection(cpdHorizontal)
      .GridInit(2, 2)
        .GridAutoExpandRows // aqui define que apenas expande Linhas
        .Add(TControlBuilder.Create(TPanel).WithCaption('1'))
        .Add(TControlBuilder.Create(TPanel).WithCaption('2'))
        .Add(TControlBuilder.Create(TPanel).WithCaption('3'))
        .Add(TControlBuilder.Create(TPanel).WithCaption('4'))

        .GridReturnNumberOfRows(RowsBeforeExpand)
        .Add(TControlBuilder.Create(TPanel).WithCaption('5')) // aqui expande automaticamente linhas para 3
        .GridReturnNumberOfRows(RowsAfterExpand)

        .SetDirection(cpdVertical) // ao mudar de direcao, apos preencher a ultima celula da coluna, tenta criar nova coluna

        .Add(TControlBuilder.Create(TPanel).WithCaption('6'))
        .GridReturnNumberOfCols(ColsBeforeExpand)
        .Add(TControlBuilder.Create(TPanel, P).WithCaption('7')) // aqui ignora pois nao expande colunas
        .GridReturnNumberOfCols(ColsAfterExpand)
      .GridFinish
    ;

    Assert.AreEqual(2, RowsBeforeExpand, 'O numero de linhas do grid antes de expandir esta diferente do esperado');
    Assert.AreEqual(3, RowsAfterExpand, 'O numero de linhas do grid depois de expandir esta diferente do esperado');
    Assert.AreEqual(2, ColsBeforeExpand, 'O numero de colunas do grid antes de expandir esta diferente do esperado');
    Assert.AreEqual(2, ColsAfterExpand, 'O numero de colunas do grid depois de expandir esta diferente do esperado');

    Assert.IsNull(P, 'P deveria ser nil, pois o grid pode expandir apenas linhas e nao colunas');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestGridModeAutoExpandOnlyCols;
var
  ControlCreator: TControlCreator;
  RowsBeforeExpand: Integer;
  RowsAfterExpand: Integer;
  ColsBeforeExpand: Integer;
  ColsAfterExpand: Integer;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  P := nil;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .SetDirection(cpdVertical)
      .GridInit(2, 2)
        .GridAutoExpandCols // aqui define que apenas expande Colunas
        .Add(TControlBuilder.Create(TPanel).WithCaption('1'))
        .Add(TControlBuilder.Create(TPanel).WithCaption('2'))
        .Add(TControlBuilder.Create(TPanel).WithCaption('3'))
        .Add(TControlBuilder.Create(TPanel).WithCaption('4'))

        .GridReturnNumberOfCols(ColsBeforeExpand)
        .Add(TControlBuilder.Create(TPanel).WithCaption('5')) // aqui expande automaticamente colunas para 3
        .GridReturnNumberOfCols(ColsAfterExpand)

        .SetDirection(cpdHorizontal)

        .Add(TControlBuilder.Create(TPanel).WithCaption('6'))
        .GridReturnNumberOfRows(RowsBeforeExpand)
        .Add(TControlBuilder.Create(TPanel, P).WithCaption('7')) // aqui ignora pois nao expande linhas
        .GridReturnNumberOfRows(RowsAfterExpand)
      .GridFinish
    ;

    Assert.AreEqual(2, ColsBeforeExpand, 'O numero de colunas do grid antes de expandir esta diferente do esperado');
    Assert.AreEqual(3, ColsAfterExpand, 'O numero de colunas do grid depois de expandir esta diferente do esperado');
    Assert.AreEqual(2, RowsBeforeExpand, 'O numero de linhas do grid antes de expandir esta diferente do esperado');
    Assert.AreEqual(2, RowsAfterExpand, 'O numero de linhas do grid depois de expandir esta diferente do esperado');

    Assert.IsNull(P, 'P deveria ser nil, pois o grid pode expandir apenas colunas e nao linhas');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestGridModeWithSubLevel;
var
  ControlCreator: TControlCreator;
  P1, P2: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .SetDirection(cpdHorizontal)
      .GridInit(4, 4)
        .GridSetCellWidthAndHeight(20, 25)
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        // sublevel na segunda linha e na segunda coluna
        .SubLevel
          // P1 e P2 estao em um sublevel dentro de uma celula, mas nao
          // devem se comportar como conteudo da celula
          .Add(TControlBuilder.Create(TPanel, P1).WithWidthAndHeight(6, 7))
          .Add(TControlBuilder.Create(TPanel, P2))
        .SuperLevel
      .GridFinish
    ;

    Assert.AreEqual(25, P1.Top, 'Propriedade Top de P1 diferente da esperada');
    Assert.AreEqual(20, P1.Left, 'Propriedade Left de P1 diferente da esperada');

    Assert.AreEqual(25, P2.Top, 'Propriedade Top de P2 diferente da esperada');
    Assert.AreEqual(20 + 6, P2.Left, 'Propriedade Left de P2 diferente da esperada');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestGridSkipCell;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .SetDirection(cpdHorizontal)
      .GridInit(2, 3)
        .GridSetCellWidthAndHeight(10, 15)
        .Add(TControlBuilder.Create(TPanel))
        .GridSkipCell
        .Add(TControlBuilder.Create(TPanel, P))
      .GridFinish
    ;

    Assert.AreEqual(10 + 10, P.Left, 'Propriedade Left diferente do esperado');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestGridSkipCells;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .SetDirection(cpdHorizontal)
      .GridInit(1, 4)
        .GridSetCellWidthAndHeight(10, 15)
        .Add(TControlBuilder.Create(TPanel))
        .GridSkipCells(2)
        .Add(TControlBuilder.Create(TPanel, P))
      .GridFinish
    ;

    Assert.AreEqual(10 * 3, P.Left, 'Propriedade Left diferente do esperado');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestGridRowHeight;
var
  ControlCreator: TControlCreator;
  P1, P2: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .SetDirection(cpdHorizontal)
      .GridInit(3, 3)
        .GridSetCellWidthAndHeight(50, 60) // altura padrao da linha = 60
        .GridSetRowHeight(0, 100) // altera a linha 0 para altura = 100

        .Add(TControlBuilder.Create(TPanel, P1)) // p1 esta na linha 0
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel, P2)) // p2 esta na linha 2
      .GridFinish
    ;

    Assert.AreEqual(100, P1.Height, 'Propriedade Height de P1 diferente do esperada');
    Assert.AreEqual(60, P2.Height, 'Propriedade Height de P2 diferente do esperada');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestGridColWidth;
var
  ControlCreator: TControlCreator;
  P1, P2: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .SetDirection(cpdHorizontal)
      .GridInit(3, 3)
        .GridSetCellWidthAndHeight(50, 60) // largura padrao da coluna = 50
        .GridSetColWidth(0, 90) // altera a coluna 0 para largura = 90

        .Add(TControlBuilder.Create(TPanel, P1)) // p1 esta na coluna 0
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel, P2)) // p2 esta na coluna 2
      .GridFinish
    ;

    Assert.AreEqual(90, P1.Width, 'Propriedade Width de P1 diferente do esperada');
    Assert.AreEqual(50, P2.Width, 'Propriedade Width de P2 diferente do esperada');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestGridRowSpan;
var
  ControlCreator: TControlCreator;
  P1, P2: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .SetDirection(cpdVertical)
      .GridInit(3, 3)
        .GridSetCellWidthAndHeight(50, 60)
        .Add(TControlBuilder.Create(TPanel, P1)) // sem rowspan
        .GridRowSpan(2)
        .Add(TControlBuilder.Create(TPanel, P2)) // com rowspan=2
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
      .GridFinish
    ;

    Assert.AreEqual(60, P1.Height, 'Propriedade Height de P1 diferente da esperada');
    Assert.AreEqual(60 * 2, P2.Height, 'Propriedade Height de P2 diferente da esperada');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestGridColSpan;
var
  ControlCreator: TControlCreator;
  P1, P2: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .SetDirection(cpdHorizontal)
      .GridInit(3, 3)
        .GridSetCellWidthAndHeight(50, 60)
        .Add(TControlBuilder.Create(TPanel, P1)) // sem colspan
        .GridColSpan(2)
        .Add(TControlBuilder.Create(TPanel, P2)) // com colspan=2
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
      .GridFinish
    ;

    Assert.AreEqual(50, P1.Width, 'Propriedade Width de P1 diferente da esperada');
    Assert.AreEqual(50 * 2, P2.Width, 'Propriedade Width de P2 diferente da esperada');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestGridRowSpanOnlyOnce;
var
  ControlCreator: TControlCreator;
  P1, P2, P3: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .SetDirection(cpdVertical)
      .GridInit(4, 2)
        .GridSetCellWidthAndHeight(50, 60)
        .Add(TControlBuilder.Create(TPanel, P1)) // sem rowspan
        .GridRowSpan(2)
        .Add(TControlBuilder.Create(TPanel, P2)) // com rowspan=2
        .Add(TControlBuilder.Create(TPanel, P3)) // sem rowspan automaticamente
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
      .GridFinish
    ;

    Assert.AreEqual(60, P1.Height, 'Propriedade Height de P1 diferente da esperada');
    Assert.AreEqual(60 * 2, P2.Height, 'Propriedade Height de P2 diferente da esperada');
    Assert.AreEqual(60, P3.Height, 'Propriedade Height de P3 diferente da esperada');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestGridColSpanOnlyOnce;
var
  ControlCreator: TControlCreator;
  P1, P2, P3: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .SetDirection(cpdHorizontal)
      .GridInit(2, 4)
        .GridSetCellWidthAndHeight(50, 60)
        .Add(TControlBuilder.Create(TPanel, P1)) // sem colspan
        .GridColSpan(2)
        .Add(TControlBuilder.Create(TPanel, P2)) // com colspan=2
        .Add(TControlBuilder.Create(TPanel, P3)) // sem colspan automaticamente
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
      .GridFinish
    ;

    Assert.AreEqual(50, P1.Width, 'Propriedade Width de P1 diferente da esperada');
    Assert.AreEqual(50 * 2, P2.Width, 'Propriedade Width de P2 diferente da esperada');
    Assert.AreEqual(50, P3.Width, 'Propriedade Width de P3 diferente da esperada');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestGridRowSpanOutOfBounds;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .SetDirection(cpdVertical)
      .GridInit(3, 3)
        .GridSetCellWidthAndHeight(50, 60)
        .Add(TControlBuilder.Create(TPanel)) // sem rowspan

        // span 3 vai ultrapassar o limite da ultima linha do grid;
        // espera-se que limite a 2
        .GridRowSpan(3)

        .Add(TControlBuilder.Create(TPanel, P))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
      .GridFinish
    ;

    Assert.AreEqual(60 * 2, P.Height, 'Propriedade Height de P diferente do esperada');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestGridColSpanOutOfBounds;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .SetDirection(cpdHorizontal)
      .GridInit(3, 3)
        .GridSetCellWidthAndHeight(50, 60)
        .Add(TControlBuilder.Create(TPanel))

        // span 3 vai ultrapassar o limite da ultima coluna do grid;
        // espera-se que limite a 2
        .GridColSpan(3)

        .Add(TControlBuilder.Create(TPanel, P))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
      .GridFinish
    ;

    Assert.AreEqual(50 * 2, P.Width, 'Propriedade Width de P diferente do esperada');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestGridRowSpanExpandGridRows;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .SetDirection(cpdVertical)
      .GridInit(3, 3)
        .GridAutoExpand
        .GridSetCellWidthAndHeight(50, 60)
        .Add(TControlBuilder.Create(TPanel)) // sem rowspan

        // span 3 vai ultrapassar o limite da ultima linha do grid;
        // espera-se que expanda o grid para 4 linhas
        .GridRowSpan(3)

        .Add(TControlBuilder.Create(TPanel, P)) // com rowspan=3
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
      .GridFinish
    ;

    Assert.AreEqual(60 * 3, P.Height, 'Propriedade Height de P diferente do esperada');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestGridColSpanExpandGridCols;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .SetDirection(cpdHorizontal)
      .GridInit(3, 3)
        .GridAutoExpand
        .GridSetCellWidthAndHeight(50, 60)
        .Add(TControlBuilder.Create(TPanel))

        // span 3 vai ultrapassar o limite da ultima coluna do grid;
        // espera-se que expanda as colunas do grid
        .GridColSpan(3)

        .Add(TControlBuilder.Create(TPanel, P)) // com colspan=3
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
      .GridFinish
    ;

    Assert.AreEqual(50 * 3, P.Width, 'Propriedade Width de P diferente da esperada');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestGridGotoCell;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .SetDirection(cpdHorizontal)
      .GridInit(5, 5)
        .GridSetCellWidthAndHeight(30, 42)
        .GridGotoCell(2, 3)
        .Add(TControlBuilder.Create(TPanel, P))
      .GridFinish
    ;

    Assert.AreEqual(42 * 2, P.Top, 'Propriedade Top diferente da esperada');
    Assert.AreEqual(30 * 3, P.Left, 'Propriedade Left diferente da esperada');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestGridRowSpanWithSpace;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .SetDirection(cpdVertical)
      .SetVerticalSpace(7) // espacamento entre duas linhas
      .GridInit(3, 3)
        .GridSetCellWidthAndHeight(50, 60)
        .Add(TControlBuilder.Create(TPanel))
        .GridRowSpan(2)
        .Add(TControlBuilder.Create(TPanel, P)) // com rowspan=2
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
      .GridFinish
    ;

    Assert.AreEqual(60 * 2 + 7, P.Height, 'Propriedade Height diferente da esperada');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestGridColSpanWithSpace;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .SetDirection(cpdHorizontal)
      .SetHorizontalSpace(9) // espacamento entre as colunas
      .GridInit(3, 3)
        .GridSetCellWidthAndHeight(50, 60)
        .Add(TControlBuilder.Create(TPanel))
        .GridColSpan(2)
        .Add(TControlBuilder.Create(TPanel, P)) // com colspan=2
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
      .GridFinish
    ;

    Assert.AreEqual(50 * 2 + 9, P.Width, 'Propriedade Width diferente da esperada');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestCellStrechHorizontal;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .GridInit(1, 1)
        .GridSetCellWidthAndHeight(50, 60)
        .GridCellStrechHorizontal // strech na horizontal = largura da celula
        .Add(TControlBuilder.Create(TPanel, P).WithWidthAndHeight(30, 25))
      .GridFinish
    ;

    Assert.AreEqual(50, P.Width, 'Propriedade Width diferente da esperada'); // usa largura da celula (streched)
    Assert.AreEqual(25, P.Height, 'Propriedade Height diferente da esperada'); // usa altura definida no builder (nao streched)
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestCellStrechVertical;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .GridInit(1, 1)
        .GridSetCellWidthAndHeight(50, 60)
        .GridCellStrechVertical // strech na vertical = altura da celula
        .Add(TControlBuilder.Create(TPanel, P).WithWidthAndHeight(30, 25))
      .GridFinish
    ;

    Assert.AreEqual(30, P.Width, 'Propriedade Width diferente da esperada'); // usa largura definida no builder (nao streched)
    Assert.AreEqual(60, P.Height, 'Propriedade Height diferente da esperada'); // usa altura da celula (streched)
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestCellStrechAll;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .GridInit(1, 1)
        .GridSetCellWidthAndHeight(50, 60)
        .GridCellStrechAll // strech na horizontal e vertical
        .Add(TControlBuilder.Create(TPanel, P).WithWidthAndHeight(30, 25)) // ignorados
      .GridFinish
    ;

    Assert.AreEqual(50, P.Width, 'Propriedade Width diferente da esperada');
    Assert.AreEqual(60, P.Height, 'Propriedade Height diferente da esperada');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestCellNoStrech;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .GridInit(1, 1)
        .GridSetCellWidthAndHeight(50, 60)
        .GridCellNoStrech // sem strech
        .Add(TControlBuilder.Create(TPanel, P).WithWidthAndHeight(30, 25))
      .GridFinish
    ;

    Assert.AreEqual(30, P.Width, 'Propriedade Width diferente da esperada');
    Assert.AreEqual(25, P.Height, 'Propriedade Height diferente da esperada');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestCellNoStrechCenter;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .GridInit(1, 1)
        .GridSetCellWidthAndHeight(60, 50)
        .GridCellNoStrech
        .GridCellPosition(cpCenter)
        .Add(TControlBuilder.Create(TPanel, P).WithWidthAndHeight(10, 14))
      .GridFinish
    ;

    Assert.AreEqual((60 div 2) - (10 div 2), P.Left, 'Propriedade Left diferente da esperada');
    Assert.AreEqual((50 div 2) - (14 div 2), P.Top, 'Propriedade Top diferente da esperada');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestCellNoStrechTop;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .GridInit(1, 1)
        .GridSetCellWidthAndHeight(60, 50)
        .GridCellNoStrech
        .GridCellPosition(cpTop)
        .Add(TControlBuilder.Create(TPanel, P).WithWidthAndHeight(10, 14))
      .GridFinish
    ;

    Assert.AreEqual((60 div 2) - (10 div 2), P.Left, 'Propriedade Left diferente da esperada');
    Assert.AreEqual(0, P.Top, 'Propriedade Top diferente da esperada');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestCellNoStrechTopRight;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .GridInit(1, 1)
        .GridSetCellWidthAndHeight(60, 50)
        .GridCellNoStrech
        .GridCellPosition(cpTopRight)
        .Add(TControlBuilder.Create(TPanel, P).WithWidthAndHeight(10, 14))
      .GridFinish
    ;

    Assert.AreEqual(60 - 10, P.Left, 'Propriedade Left diferente da esperada');
    Assert.AreEqual(0, P.Top, 'Propriedade Top diferente da esperada');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestCellNoStrechRight;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .GridInit(1, 1)
        .GridSetCellWidthAndHeight(60, 50)
        .GridCellNoStrech
        .GridCellPosition(cpRight)
        .Add(TControlBuilder.Create(TPanel, P).WithWidthAndHeight(10, 14))
      .GridFinish
    ;

    Assert.AreEqual(60 - 10, P.Left, 'Propriedade Left diferente da esperada');
    Assert.AreEqual((50 div 2) - (14 div 2), P.Top, 'Propriedade Top diferente da esperada');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestCellNoStrechBottomRight;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .GridInit(1, 1)
        .GridSetCellWidthAndHeight(60, 50)
        .GridCellNoStrech
        .GridCellPosition(cpBottomRight)
        .Add(TControlBuilder.Create(TPanel, P).WithWidthAndHeight(10, 14))
      .GridFinish
    ;

    Assert.AreEqual(60 - 10, P.Left, 'Propriedade Left diferente da esperada');
    Assert.AreEqual(50 - 14, P.Top, 'Propriedade Top diferente da esperada');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestCellNoStrechBottom;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .GridInit(1, 1)
        .GridSetCellWidthAndHeight(60, 50)
        .GridCellNoStrech
        .GridCellPosition(cpBottom)
        .Add(TControlBuilder.Create(TPanel, P).WithWidthAndHeight(10, 14))
      .GridFinish
    ;

    Assert.AreEqual((60 div 2) - (10 div 2), P.Left, 'Propriedade Left diferente da esperada');
    Assert.AreEqual(50 - 14, P.Top, 'Propriedade Top diferente da esperada');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestCellNoStrechBottomLeft;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .GridInit(1, 1)
        .GridSetCellWidthAndHeight(60, 50)
        .GridCellNoStrech
        .GridCellPosition(cpBottomLeft)
        .Add(TControlBuilder.Create(TPanel, P).WithWidthAndHeight(10, 14))
      .GridFinish
    ;

    Assert.AreEqual(0, P.Left, 'Propriedade Left diferente da esperada');
    Assert.AreEqual(50 - 14, P.Top, 'Propriedade Top diferente da esperada');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestCellNoStrechLeft;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .GridInit(1, 1)
        .GridSetCellWidthAndHeight(60, 50)
        .GridCellNoStrech
        .GridCellPosition(cpLeft)
        .Add(TControlBuilder.Create(TPanel, P).WithWidthAndHeight(10, 14))
      .GridFinish
    ;

    Assert.AreEqual(0, P.Left, 'Propriedade Left diferente da esperada');
    Assert.AreEqual((50 div 2) - (14 div 2), P.Top, 'Propriedade Top diferente da esperada');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestCellNoStrechTopLeft;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .GridInit(1, 1)
        .GridSetCellWidthAndHeight(60, 50)
        .GridCellNoStrech
        .GridCellPosition(cpTopLeft)
        .Add(TControlBuilder.Create(TPanel, P).WithWidthAndHeight(10, 14))
      .GridFinish
    ;

    Assert.AreEqual(0, P.Left, 'Propriedade Left diferente da esperada');
    Assert.AreEqual(0, P.Top, 'Propriedade Top diferente da esperada');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestGridRowOffset;
var
  ControlCreator: TControlCreator;
  P1, P2, P3, P4, P5, P6, P7, P8, P9: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .SetDirection(cpdHorizontal)
      .GridInit(3, 3)
        .GridSetCellWidthAndHeight(50, 60)
        .GridSetRowOffset(1, 15) // segunda linha com offset positivo de 15
        .Add(TControlBuilder.Create(TPanel, P1)) // linha 1
        .Add(TControlBuilder.Create(TPanel, P2)) // linha 1
        .Add(TControlBuilder.Create(TPanel, P3)) // linha 1

        .Add(TControlBuilder.Create(TPanel, P4)) // linha 2
        .Add(TControlBuilder.Create(TPanel, P5)) // linha 2
        .Add(TControlBuilder.Create(TPanel, P6)) // linha 2

        .Add(TControlBuilder.Create(TPanel, P7)) // linha 3
        .Add(TControlBuilder.Create(TPanel, P8)) // linha 3
        .Add(TControlBuilder.Create(TPanel, P9)) // linha 3
      .GridFinish
    ;

    // primeira coluna
    Assert.AreEqual(0, P1.Left, 'Propriedade Left de P1 diferente da esperada');
    Assert.AreEqual(15, P4.Left, 'Propriedade Left de P4 diferente da esperada'); // linha com offset
    Assert.AreEqual(0, P7.Left, 'Propriedade Left de P7 diferente da esperada');

    // segunda coluna
    Assert.AreEqual(50, P2.Left, 'Propriedade Left de P2 diferente da esperada');
    Assert.AreEqual(50 + 15, P5.Left, 'Propriedade Left de P5 diferente da esperada'); // linha com offset
    Assert.AreEqual(50, P8.Left, 'Propriedade Left de P8 diferente da esperada');

    // terceira coluna
    Assert.AreEqual(50 * 2, P3.Left, 'Propriedade Left de P3 diferente da esperada');
    Assert.AreEqual(50 * 2 + 15, P6.Left, 'Propriedade Left de P6 diferente da esperada'); // linha com offset
    Assert.AreEqual(50 * 2, P9.Left, 'Propriedade Left de P9 diferente da esperada');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestGridColOffset;
var
  ControlCreator: TControlCreator;
  P1, P2, P3, P4, P5, P6, P7, P8, P9: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .SetDirection(cpdVertical)
      .GridInit(3, 3)
        .GridSetCellWidthAndHeight(50, 60)
        .GridSetColOffset(1, 15) // segunda coluna com offset positivo de 15
        .Add(TControlBuilder.Create(TPanel, P1)) // coluna 1
        .Add(TControlBuilder.Create(TPanel, P2)) // coluna 1
        .Add(TControlBuilder.Create(TPanel, P3)) // coluna 1

        .Add(TControlBuilder.Create(TPanel, P4)) // coluna 2
        .Add(TControlBuilder.Create(TPanel, P5)) // coluna 2
        .Add(TControlBuilder.Create(TPanel, P6)) // coluna 2

        .Add(TControlBuilder.Create(TPanel, P7)) // coluna 3
        .Add(TControlBuilder.Create(TPanel, P8)) // coluna 3
        .Add(TControlBuilder.Create(TPanel, P9)) // coluna 3
      .GridFinish
    ;

    // primeira linha
    Assert.AreEqual(0, P1.Top, 'Propriedade Top de P1 diferente da esperada');
    Assert.AreEqual(15, P4.Top, 'Propriedade Top de P4 diferente da esperada'); // coluna com offset
    Assert.AreEqual(0, P7.Top, 'Propriedade Top de P7 diferente da esperada');

    // segunda linha
    Assert.AreEqual(60, P2.Top, 'Propriedade Top de P2 diferente da esperada');
    Assert.AreEqual(60 + 15, P5.Top, 'Propriedade Top de P5 diferente da esperada'); // coluna com offset
    Assert.AreEqual(60, P8.Top, 'Propriedade Top de P8 diferente da esperada');

    // terceira linha
    Assert.AreEqual(60 * 2, P3.Top, 'Propriedade Top de P3 diferente da esperada');
    Assert.AreEqual(60 * 2 + 15, P6.Top, 'Propriedade Top de P6 diferente da esperada'); // coluna com offset
    Assert.AreEqual(60 * 2, P9.Top, 'Propriedade Top de P9 diferente da esperada');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestGridBreakLine;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .SetDirection(cpdHorizontal)
      .GridInit(2, 4)
        .GridSetCellWidthAndHeight(10, 15)
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .BreakLine
        .Add(TControlBuilder.Create(TPanel, P)) // segunda linha, primeira coluna
      .GridFinish
    ;

    Assert.AreEqual(15, P.Top, 'Propriedade Top diferente da esperada');
    Assert.AreEqual(0, P.Left, 'Propriedade Left diferente da esperada');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestGridBreakColumn;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .SetDirection(cpdVertical)
      .GridInit(4, 2)
        .GridSetCellWidthAndHeight(10, 15)
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .BreakColumn
        .Add(TControlBuilder.Create(TPanel, P)) // segunda coluna, primeira linha
      .GridFinish
    ;

    Assert.AreEqual(0, P.Top, 'Propriedade Top diferente da esperada');
    Assert.AreEqual(10, P.Left, 'Propriedade Left diferente da esperada');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestGrid1x1;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    P := nil;
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .GridInit(1, 1)
        .GridSetCellWidthAndHeight(50, 50)
        .GridCellStrechAll
        .Add(TControlBuilder.Create(TPanel, P))
      .GridFinish
    ;

    Assert.IsNotNull(P, 'P nao deveria ser nil');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestGridGoToLastCell;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    P := nil;
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .GridInit(2, 2)
        .GridGotoCell(1, 1)
        .GridSetCellWidthAndHeight(50, 50)
        .GridCellStrechAll
        .Add(TControlBuilder.Create(TPanel, P))
      .GridFinish
    ;

    Assert.IsNotNull(P, 'P nao deveria ser nil');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestGridSpanOverlapRaises;
// Regressao: Step so conferia se a celula INICIAL estava livre; o resto do
// span (via RowSpan/ColSpan) era marcado ocupado sem checar sobreposicao.
// Aqui a celula (0,0) esta livre, mas o span de 2 linhas invade (1,0), que
// ja foi ocupada por outro controle - deve lancar erro, nao sobrepor
// silenciosamente.
var
  ControlCreator: TControlCreator;
  P1, P2: TPanel;
  Raised: Boolean;
begin
  Raised := False;
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .GridInit(3, 3)
        .GridGotoCell(1, 0)
        .Add(TControlBuilder.Create(TPanel, P1)) // ocupa (1,0)
    ;

    try
      ControlCreator
        .GridGotoCell(0, 0)
        .GridRowSpan(2)
        .Add(TControlBuilder.Create(TPanel, P2)) // (0,0) livre, mas span invade (1,0) ja ocupada
      ;
    except
      on E: Exception do
        Raised := True;
    end;

    Assert.IsTrue(Raised, 'Sobreposicao de span deveria lancar excecao');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestBreakLineOnLastRowIsNoOp;
// Regressao: BreakLine na ultima linha do grid chamava GridGotoCell(Rows,0),
// que lancava "GridGotoCell: linha fora dos limites" - um metodo que quem
// chamou BreakLine nunca invocou diretamente. Deve ser no-op, igual ao
// BreakLine fora do modo grid (que nunca lanca).
var
  ControlCreator: TControlCreator;
  Raised: Boolean;
begin
  Raised := False;
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .GridInit(2, 2)
        .GridGotoCell(1, 0); // ja na ultima linha

    try
      ControlCreator.BreakLine;
    except
      on E: Exception do
        Raised := True;
    end;

    Assert.IsFalse(Raised, 'BreakLine na ultima linha nao deveria lancar excecao');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestBreakColumnOnLastColIsNoOp;
var
  ControlCreator: TControlCreator;
  Raised: Boolean;
begin
  Raised := False;
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .GridInit(2, 2)
        .GridGotoCell(0, 1); // ja na ultima coluna

    try
      ControlCreator.BreakColumn;
    except
      on E: Exception do
        Raised := True;
    end;

    Assert.IsFalse(Raised, 'BreakColumn na ultima coluna nao deveria lancar excecao');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestSubLevelWithNonWinControlRaises;
// Regressao: SubLevel fazia TWinControl(Control) via cast direto, sem
// checar o tipo. TLabel nao e TWinControl (e TGraphicControl) - passar um
// builder de TLabel para SubLevel deveria lancar um erro claro em vez de
// corromper o proximo Add silenciosamente. Tambem verifica que a excecao
// nao deixa FLevelStack com um nivel extra orfao (a checagem de tipo
// precisa rodar ANTES do SubLevel(AGroupName) empilhar um nivel novo) - um
// Add logo depois deve continuar criando no nivel original, com FForm
// como Parent direto.
var
  ControlCreator: TControlCreator;
  Raised: Boolean;
  P: TPanel;
begin
  Raised := False;
  ControlCreator := TControlCreator.Create;
  try
    try
      ControlCreator
        .SetOwnerAndParent(FForm, FForm)
        .SubLevel(TControlBuilder.Create(TLabel));
    except
      on E: Exception do
        Raised := True;
    end;

    Assert.IsTrue(Raised, 'SubLevel com um controle que nao e TWinControl deveria lancar excecao');

    P := nil;
    ControlCreator.Add(TControlBuilder.Create(TPanel, P));
    Assert.IsNotNull(P, 'P nao deveria ser nil');
    Assert.IsTrue(FForm = P.Parent,
      'Parent do controle criado depois deveria ser FForm diretamente (nivel nao deveria ter ficado corrompido)');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestAddWithGridFullDiscardsOutReference;
// Regressao: quando o grid ja esta cheio, Add saia antes de chamar Build,
// entao o "out Reference" do builder nunca era escrito e a variavel ficava
// com o valor anterior (lixo) em vez de nil.
var
  ControlCreator: TControlCreator;
  Dummy: TPanel;
  P1, P2: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  Dummy := TPanel.Create(nil);
  try
    P2 := Dummy; // valor nao-nil conhecido, simulando "lixo" de uma referencia anterior
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .GridInit(1, 1)
        .Add(TControlBuilder.Create(TPanel, P1))
        .Add(TControlBuilder.Create(TPanel, P2)) // grid ja cheio: nao deveria criar nem manter o valor antigo
    ;

    Assert.IsNotNull(P1, 'P1 deveria ter sido criado normalmente');
    Assert.IsNull(P2, 'P2 deveria ser nil (grid cheio, Build nunca chegou a ser chamado)');
  finally
    Dummy.Free;
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestAlignControlsRight;
var
  ControlCreator: TControlCreator;
  RefPanel, Btn1, Btn2: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .SetTopLeft(0, 10)
      .Add(TControlBuilder.Create(TPanel, 'ref_panel', RefPanel).WithWidth(250))
      .SetTopLeft(0, 0)
      .Add(TControlBuilder.Create(TPanel, 'btn1', Btn1).WithWidth(50))
      .Add(TControlBuilder.Create(TPanel, 'btn2', Btn2).WithLeft(50).WithWidth(60))
      .AlignControlsRight(['btn1', 'btn2'], ['ref_panel'])
    ;

    // borda direita do ref_panel: Left(10) + Width(250) = 260
    // grupo [btn1, btn2] tem largura total 110, deslocado para que a borda
    // direita do grupo coincida com a do ref_panel
    Assert.AreEqual(150, Btn1.Left, 'Left de btn1 diferente do esperado');
    Assert.AreEqual(200, Btn2.Left, 'Left de btn2 diferente do esperado');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestAlignControlsRightWithPadding;
var
  ControlCreator: TControlCreator;
  RefPanel, Btn1: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .SetTopLeft(0, 10)
      .Add(TControlBuilder.Create(TPanel, 'ref_panel', RefPanel).WithWidth(250))
      .SetTopLeft(0, 0)
      .Add(TControlBuilder.Create(TPanel, 'btn1', Btn1).WithWidth(50))
      .AlignControlsRight(['btn1'], ['ref_panel'], 10) // com 10px de padding
    ;

    // borda direita do ref_panel (260) - largura de btn1 (50) - padding (10)
    Assert.AreEqual(260 - 50 - 10, Btn1.Left, 'Left de btn1 diferente do esperado');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestSetOwnerAndParentDirectly;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .Add(TControlBuilder.Create(TPanel, P))
    ;

    Assert.IsTrue(FForm = P.Owner, 'Owner do controle diferente do esperado');
    Assert.IsTrue(FForm = P.Parent, 'Parent do controle diferente do esperado');
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestSetParentDirectly;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetParent(FForm)
      .Add(TControlBuilder.Create(TPanel, P))
    ;

    Assert.IsTrue(FForm = P.Parent, 'Parent do controle diferente do esperado');
  finally
    // SetParent (diferente de SetOwnerAndParent) nao define Owner, entao P
    // nao e liberado automaticamente pelo FForm.Free do TearDown.
    P.Free;
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTest.TestGetControlGeneric;
var
  ControlCreator: TControlCreator;
begin
  // Regressao: GetControl<T> chamava Registry.GetControl<T> internamente
  // sem "specialize" (necessario apenas no FPC); no Delphi a chamada
  // generica direta ja e o padrao correto.
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .Add(TControlBuilder.Create(TPanel, 'painel_generico'))
    ;

    Assert.IsNotNull(ControlCreator.GetControl<TPanel>('painel_generico'),
      'GetControl<T> nao deveria devolver nil');
  finally
    ControlCreator.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TControlCreatorTest);

end.
