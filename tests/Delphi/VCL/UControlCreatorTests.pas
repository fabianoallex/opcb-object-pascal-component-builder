unit UControlCreatorTests;

interface

uses
  DUnitX.TestFramework, Vcl.Forms, Vcl.Controls, Vcl.ExtCtrls, OPCB;

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
    [Test] procedure TestSpaceHorizontally;
    [Test] procedure TestSpaceVertically;
    [Test] procedure TestSetControlHeight;
    [Test] procedure TestSetControlWidth;
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
    [Test] procedure TestGridModeAutoExpandRows;
    [Test] procedure TestGridModeAutoExpandCols;
    [Test] procedure TestGridSkipCell;
    [Test] procedure TestGridSkipCells;
    [Test] procedure TestGridRowSpan;
    [Test] procedure TestGridColSpan;
    [Test] procedure TestGridGotoCell;
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
