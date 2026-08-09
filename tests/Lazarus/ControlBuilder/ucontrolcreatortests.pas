unit UControlCreatorTests;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$LONGSTRINGS ON}{$MODESWITCH TYPEHELPERS}{$MODESWITCH ADVANCEDRECORDS}
{$ENDIF}


interface

uses
  Classes, SysUtils, fpcunit, testregistry, Forms, StdCtrls, OPCB;

type

  { TControlCreatorTests }

  TControlCreatorTests = class(TTestCase)
  private
    procedure ExternalMethod(const ACreator: TControlCreator);
  protected
    FForm: TForm;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTopLeft;
    procedure TestTopLeftHorizontally;
    procedure TestTopLeftVertically;
    procedure TestIncTop;
    procedure TestIncLeft;
    procedure TestBreakHorizontally;
    procedure TestBreakVertically;
    procedure TestBreakAfterTopLeft;
    procedure TestTopAfterDiferentControlsHightsAndBreak;
    procedure TestLeftAfterDiferentControlsWidthsAndBreak;
    procedure TestSpaceHorizontally;
    procedure TestSpaceVertically;
    procedure TestSetControlHeight;
    procedure TestUnsetControlHeight;
    procedure TestSetControlWidth;
    procedure TestUnsetControlWidth;
    procedure TestExternal;
    procedure TestAddToRegistry;                // testa se um conrole ao ser criado é inserido no registro
    procedure TestRemoveControlFromRegistryOnDestroy;  // testa se um controle quando destruído será removido do registro
    procedure TestReturnLastControlOnEmptyCreatorDoesNotRaise;
    procedure TestControlsIsolatedPerCreatorInSharedContext;
    procedure TestSubLevel;
    procedure TestSubLevelSubLevel;    // mais de um nivel de profunidade
    procedure TestSubLevelSuperLevel;  // testa se volta ao nivel inicial
    procedure TestSubLevelEmpty;       // testa sublevel vazio. deve permitir sem mover top/left.
    procedure TestSubLevelDirectionSuperLevelDirection;
    procedure TestSiblingSubLevelWithOptionsMatchesPositionalOverload;
    procedure TestSiblingSubLevelWithOptionsWithoutDirectionKeepsInheritedDirection;
    procedure TestSiblingSubLevelWithOptionsAndBuilderMatchesPositionalOverload;
    procedure TestRecalcParentSize;
    procedure TestRecalcParentSizeWithExtraSizes;
    procedure TestGridMode;
    procedure TestGridModeIgnoreAddAfterEndOfGrid;
    procedure TestGridModeNotIgnoreAddAfterEndOfGridWhenAutoExpand;
    procedure TestGridModeAutoExpandRows;
    procedure TestGridModeAutoExpandCols;
    procedure TestGridModeAutoExpandOnlyRows;
    procedure TestGridModeAutoExpandOnlyCols;
    procedure TestGridModeWithSubLevel;
    procedure TestGridSkipCell;
    procedure TestGridRowHeight;
    procedure TestGridColWidth;
    procedure TestGridRowSpan;
    procedure TestGridColSpan;
    procedure TestGridRowSpanOnlyOnce;
    procedure TestGridColSpanOnlyOnce;
    procedure TestGridRowSpanOutOfBounds;
    procedure TestGridColSpanOutOfBounds;
    procedure TestGridRowSpanExpandGridRows;
    procedure TestGridColSpanExpandGridCols;
    procedure TestGridGotoCell;
    procedure TestGridRowSpanWithSpace;
    procedure TestGridColSpanWithSpace;
    procedure TestGrid1x1;
    procedure TestGridGoToLastCell;
    procedure TestGridSpanOverlapRaises;
    procedure TestBreakLineOnLastRowIsNoOp;
    procedure TestBreakColumnOnLastColIsNoOp;
    procedure TestSubLevelWithNonWinControlRaises;
    procedure TestAddWithGridFullDiscardsOutReference;
    procedure TestCellStrechHorizontal;
    procedure TestCellStrechVertical;
    procedure TestCellStrechAll;
    procedure TestCellNoStrech;
    procedure TestCellNoStrechCenter;
    procedure TestCellNoStrechTop;
    procedure TestCellNoStrechTopRight;
    procedure TestCellNoStrechRight;
    procedure TestCellNoStrechBottomRight;
    procedure TestCellNoStrechBottom;
    procedure TestCellNoStrechBottomLeft;
    procedure TestCellNoStrechLeft;
    procedure TestCellNoStrechTopLeft;
    procedure TestGridRowOffset;
    procedure TestGridColOffset;
    procedure TestGridBreakLine;
    procedure TestGridBreakColumn;
    procedure TestBreakHorizontallyWithIncrement;
    procedure TestBreakVerticallyWithIncrement;
    procedure TestBreakLineWithIncrement;
    procedure TestBreakColumnWithIncrement;
    procedure TestGridSkipCells;
    procedure TestAlignControlsRight;
    procedure TestAlignControlsRightWithPadding;
    procedure TestMoveControlsSingleControlAppliesDelta;
    procedure TestMoveControlsByNamesAppliesDeltaToEach;
    procedure TestMoveControlsByNamesRaisesWhenControlNotFound;
    procedure TestSetTopLeftNearControlBelow;
    procedure TestSetTopLeftNearControlRight;
    procedure TestSetTopLeftNearControlsBelow;
    procedure TestSetTopLeftNearControlsRight;
    procedure TestSetTopLeftNearGroupBelow;
    procedure TestSetTopLeftNearGroupRight;
    procedure TestRecalcParentHeightOnly;
    procedure TestRecalcParentWidthOnly;
    procedure TestCenterControlsHorizontally;
    procedure TestCenterControlsVertically;
    procedure TestCenterControlsInParentHorizontally;
    procedure TestCenterControlsInParentVertically;
    procedure TestCenterControlInParentHorizontally;
    procedure TestCopyHeightAppliesReferenceHeight;
    procedure TestCopyWidthAppliesReferenceWidth;
    procedure TestCopySizeAppliesReferenceWidthAndHeight;
    procedure TestCopyHeightRaisesWhenControlNotFound;
    procedure TestCopyWidthRaisesWhenControlNotFound;
    procedure TestCopySizeRaisesWhenControlNotFound;
    procedure TestReturnCurrentLevel;
    procedure TestSetOwnerAndParentDirectly;
    procedure TestSetParentDirectly;
    procedure TestGetControlGeneric;
    procedure TestAddControlBuilderIsFreedNotLeaked;
    procedure TestAddControlBuilderExceptionDuringBuildStillFreesBuilder;
    procedure TestAddControlBuilderExceptionDuringBuildNullsOutReference;
  end;

implementation

uses
  ExtCtrls, Controls, TypInfo;

type
  { TCountingControlBuilder }

  TCountingControlBuilder = class(TControlBuilder)
  public
    destructor Destroy; override;
  end;

var
  GControlBuilderDestroyCount: Integer = 0;

destructor TCountingControlBuilder.Destroy;
begin
  Inc(GControlBuilderDestroyCount);
  inherited;
end;

procedure TControlCreatorTests.ExternalMethod(const ACreator: TControlCreator);
var
  ControlBuilder: TControlBuilder;
begin
  ControlBuilder := TControlBuilder.Create(TPanel, 'PanelTest');
  ControlBuilder.WithCaption('EXTERNAL-TEST');
  ACreator.specialize Add<TControl>(ControlBuilder);
end;

procedure TControlCreatorTests.SetUp;
begin
  FForm := TForm.Create(nil);
end;

procedure TControlCreatorTests.TearDown;
begin
  FForm.Free;
end;

procedure TControlCreatorTests.TestTopLeft;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    with ControlCreator do
    begin
      WithOwnerAndParent(FForm, FForm);
      SetTopLeft(10, 20);
      Add(TControlBuilder.Create(TPanel, P));
    end;

    AssertEquals('Propriedade Top diferente da esperada', 10, P.Top);
    AssertEquals('Propriedade Left diferente da esperada', 20, P.Left);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestTopLeftHorizontally;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .SetTopLeft(10, 20)
      .SetDirection(cpdHorizontal)
      .Add(TControlBuilder.Create(TPanel).WithWidth(15).WithHeight(15))
      .Add(TControlBuilder.Create(TPanel, P))
    ;
    AssertEquals('Propriedade Top diferente da esperada', 10 + 0, P.Top);
    AssertEquals('Propriedade Left diferente da esperada', 20 + 15, P.Left);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestTopLeftVertically;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .SetTopLeft(10, 20)
      .SetDirection(cpdVertical)
      .Add(TControlBuilder.Create(TPanel).WithWidth(15).WithHeight(15))
      .Add(TControlBuilder.Create(TPanel, P))
    ;
    AssertEquals('Propriedade Top diferente da esperada', 10 + 15, P.Top);
    AssertEquals('Propriedade Left diferente da esperada', 20 + 0, P.Left);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestIncTop;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .SetTopLeft(10, 20)
      .IncTop(13)
      .Add(TControlBuilder.Create(TPanel, P))
    ;

    AssertEquals('Propriedade Top diferente da esperada', 10 + 13, P.Top);
    AssertEquals('Propriedade Left diferente da esperada', 20, P.Left);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestIncLeft;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .SetTopLeft(10, 20)
      .IncLeft(17)
      .Add(TControlBuilder.Create(TPanel, P))
    ;

    AssertEquals('Propriedade Top diferente da esperada', 10, P.Top);
    AssertEquals('Propriedade Left diferente da esperada', 20 + 17, P.Left);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestBreakHorizontally;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(15, 10))
      .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(15, 10))
      .Break
      .Add(TControlBuilder.Create(TPanel, P))
    ;
    AssertEquals('Propriedade Top diferente da esperada', 10, P.Top);
    AssertEquals('Propriedade Left diferente da esperada', 0, P.Left);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestBreakVertically;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdVertical)
      .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(15, 10))
      .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(15, 10))
      .Break
      .Add(TControlBuilder.Create(TPanel, P))
    ;
    AssertEquals('Propriedade Top diferente da esperada', 0, P.Top);
    AssertEquals('Propriedade Left diferente da esperada', 15, P.Left);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestBreakHorizontallyWithIncrement;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(15, 10))
      .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(15, 10))
      .Break(5) // igual ao Break, mas soma 5 ao Top depois
      .Add(TControlBuilder.Create(TPanel, P))
    ;
    AssertEquals('Propriedade Top diferente da esperada', 10 + 5, P.Top);
    AssertEquals('Propriedade Left diferente da esperada', 0, P.Left);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestBreakVerticallyWithIncrement;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdVertical)
      .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(15, 10))
      .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(15, 10))
      .Break(5) // igual ao Break, mas soma 5 ao Left depois
      .Add(TControlBuilder.Create(TPanel, P))
    ;
    AssertEquals('Propriedade Top diferente da esperada', 0, P.Top);
    AssertEquals('Propriedade Left diferente da esperada', 15 + 5, P.Left);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestBreakLineWithIncrement;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(15, 10))
      .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(15, 10))
      .BreakLine(5)
      .Add(TControlBuilder.Create(TPanel, P))
    ;
    AssertEquals('Propriedade Top diferente da esperada', 10 + 5, P.Top);
    AssertEquals('Propriedade Left diferente da esperada', 0, P.Left);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestBreakColumnWithIncrement;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdVertical)
      .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(15, 10))
      .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(15, 10))
      .BreakColumn(5)
      .Add(TControlBuilder.Create(TPanel, P))
    ;
    AssertEquals('Propriedade Top diferente da esperada', 0, P.Top);
    AssertEquals('Propriedade Left diferente da esperada', 15 + 5, P.Left);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestBreakAfterTopLeft;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(15, 10))
      .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(15, 10))
      .SetTopLeft(40, 30)  // muda o local de referencia do break
      .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(5, 5))
      .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(5, 5))
      .Break
      .Add(TControlBuilder.Create(TPanel, P))
    ;
    AssertEquals('Propriedade Top diferente da esperada', 40 + 5, P.Top);
    AssertEquals('Propriedade Left diferente da esperada', 30, P.Left);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestTopAfterDiferentControlsHightsAndBreak;
{
  ---  ---
  | |  | |
  ---  | |
       ---  .break
 --  ---  ---  . ---> top of P1
 --  | |  | |
     | |  ---
     ---       .break
  . --- top of P2
}
var
  ControlCreator: TControlCreator;
  P1, P2: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .Add(TControlBuilder.Create(TPanel).WithHeight(15))
      .Add(TControlBuilder.Create(TPanel).WithHeight(25)) // maior incluido primeiro
      .Break
      .Add(TControlBuilder.Create(TPanel, P1).WithHeight(5))
    ;
    AssertEquals('Propriedade Top diferente da esperada para P1', 25, P1.Top);

    ControlCreator
      .Add(TControlBuilder.Create(TPanel).WithHeight(25)) // maior incluido segundo
      .Add(TControlBuilder.Create(TPanel).WithHeight(15))
      .Break
      .Add(TControlBuilder.Create(TPanel, P2))
    ;
    AssertEquals('Propriedade Top diferente da esperada para P2', 25 + 25, P2.Top);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestLeftAfterDiferentControlsWidthsAndBreak;
{
        +-> Left P1
        |     +-> Left P2
        |     |
        .     .
  ---   --    --
  | |   --    --
  ---   -----
  ----- |   |
  |   | |   |
  |   | -----
  -----.---         . --> break
        | |
        | |
        ---
}
var
  ControlCreator: TControlCreator;
  P1, P2: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdVertical)
      .Add(TControlBuilder.Create(TPanel).WithWidth(15))
      .Add(TControlBuilder.Create(TPanel).WithWidth(25)) // maior incluido primeiro
      .Break
      .Add(TControlBuilder.Create(TPanel, P1).WithWidth(5))
    ;
    AssertEquals('Propriedade Left diferente da esperada para P1', 25, P1.Left);

    ControlCreator
      .Add(TControlBuilder.Create(TPanel).WithWidth(25)) // maior incluido segundo
      .Add(TControlBuilder.Create(TPanel).WithWidth(15))
      .Break
      .Add(TControlBuilder.Create(TPanel, P2))
    ;
    AssertEquals('Propriedade Left diferente da esperada  para P2', 25 + 25, P2.Left);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestSpaceHorizontally;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .SetSpace(7, 8)
      .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(15, 10))
      .Add(TControlBuilder.Create(TPanel, P))
    ;
    AssertEquals('Propriedade Top diferente da esperada', 0, P.Top);
    AssertEquals('Propriedade Left diferente da esperada', 15 + 8, P.Left);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestSpaceVertically;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdVertical)
      .SetSpace(7, 8)
      .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(15, 10))
      .Add(TControlBuilder.Create(TPanel, P))
    ;
    AssertEquals('Propriedade Top diferente da esperada', 10 + 7, P.Top);
    AssertEquals('Propriedade Left diferente da esperada', 0, P.Left);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestSetControlHeight;
var
  ControlCreator: TControlCreator;
  P1, P2: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .SetTopLeft(10, 20)
      .SetControlHeight(33)
      .Add(TControlBuilder.Create(TPanel, P1).WithHeight(44)) // tem que sobrepor com 33, mesmo passando 44
      .Add(TControlBuilder.Create(TPanel, P2))
    ;

    AssertEquals('Propriedade Height de P1 diferente da esperada', 33, P1.Height);
    AssertEquals('Propriedade Height de P2 diferente da esperada', 33, P2.Height);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestUnsetControlHeight;
var
  ControlCreator: TControlCreator;
  P1, P2: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .SetTopLeft(10, 20)
      .SetControlHeight(33)
      .Add(TControlBuilder.Create(TPanel).WithHeight(44))
      .Add(TControlBuilder.Create(TPanel))
      .UnsetControlHeight
      .Add(TControlBuilder.Create(TPanel, P1).WithHeight(44)) // agora tem que ser considerado os 44
      .Add(TControlBuilder.Create(TPanel, P2))
    ;

    AssertEquals('Propriedade Height de P1 diferente da esperada', 44, P1.Height);
    AssertEquals('Propriedade Height de P2 diferente da esperada', 50, P2.Height);  // 50: default panel height
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestSetControlWidth;
var
  ControlCreator: TControlCreator;
  P1, P2: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .SetTopLeft(10, 20)
      .SetControlWidth(33)
      .Add(TControlBuilder.Create(TPanel, P1).WithWidth(44)) // tem que sobrepor com 33, mesmo passando 44
      .Add(TControlBuilder.Create(TPanel, P2))
    ;

    AssertEquals('Propriedade Width de P1 diferente da esperada', 33, P1.Width);
    AssertEquals('Propriedade Width de P2 diferente da esperada', 33, P2.Width);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestUnsetControlWidth;
var
  ControlCreator: TControlCreator;
  P1, P2: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .SetTopLeft(10, 20)
      .SetControlWidth(33)
      .Add(TControlBuilder.Create(TPanel).WithWidth(44))
      .Add(TControlBuilder.Create(TPanel))
      .UnsetControlWidth
      .Add(TControlBuilder.Create(TPanel, P1).WithWidth(44)) // agora tem que ser considerado os 44
      .Add(TControlBuilder.Create(TPanel, P2))
    ;

    AssertEquals('Propriedade Width de P1 diferente da esperada', 44, P1.Width);
    AssertEquals('Propriedade Width de P2 diferente da esperada', 170, P2.Width);  // 170: default panel width
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestExternal;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    P := nil;

    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .External(@ExternalMethod)             // Irá incluir um Panel com Caption 'EXTERNAL-TEST'
    ;

    P := ControlCreator.GetControl('PanelTest') as TPanel;

    AssertNotNull('Variável P não deveria ser nil', P);
    AssertEquals('Propreiedade Caption diferente da esperada', 'EXTERNAL-TEST', P.Caption);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestAddToRegistry;
var
  ControlCreator: TControlCreator;
begin
  ControlCreator := TControlCreator.Create;
  try
    AssertEquals('A quantidade de controles no registro diferente do esperado',
      0, ControlCreator.Controls.Count);

    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .Add(TControlBuilder.Create(TPanel))
    ;

    AssertEquals('A quantidade de controles no registro diferente do esperado',
      1, ControlCreator.Controls.Count);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestRemoveControlFromRegistryOnDestroy;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    AssertEquals('A quantidade de controles no registro diferente do esperado',
      0, ControlCreator.Controls.Count);

    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .Add(TControlBuilder.Create(TPanel, 'panel_test'))
    ;

    P := ControlCreator.GetControl('panel_test') as TPanel;
    AssertEquals('A quantidade de controles no registro diferente do esperado',
      1, ControlCreator.Controls.Count);

    // ao destruir o objeto, automaticamente o objeto deve ser removido do
    // registro interno de controles do builder

    P.Free;

    AssertEquals('A quantidade de controles no registro diferente do esperado',
      0, ControlCreator.Controls.Count);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestReturnLastControlOnEmptyCreatorDoesNotRaise;
var
  ControlCreator: TControlCreator;
  Ctrl: TControl;
begin
  // Regressão: "if Self.Controls.Count > 0 then;" tinha um ";" solto que
  // tornava a guarda inerte, causando exceção ao acessar .Last de uma
  // lista vazia.
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator.ReturnLastControl(Ctrl);
    AssertNull('Ctrl deveria ser nil quando não há nenhum controle criado', Ctrl);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestControlsIsolatedPerCreatorInSharedContext;
var
  CreatorA, CreatorB: TControlCreator;
begin
  // Regressão: Controls devolvia Registry.Controls (todo o contexto
  // compartilhado), então um creator enxergava controles criados por outro
  // creator que usasse a mesma chave de contexto.
  CreatorA := TControlCreator.Create('SharedContextTest');
  CreatorB := TControlCreator.Create('SharedContextTest');
  try
    CreatorA
      .WithOwnerAndParent(FForm, FForm)
      .Add(TControlBuilder.Create(TPanel))
    ;
    CreatorB
      .WithOwnerAndParent(FForm, FForm)
      .Add(TControlBuilder.Create(TPanel))
      .Add(TControlBuilder.Create(TPanel))
    ;

    AssertEquals('CreatorA deveria ver só os controles que ele mesmo criou',
      1, CreatorA.Controls.Count);
    AssertEquals('CreatorB deveria ver só os controles que ele mesmo criou',
      2, CreatorB.Controls.Count);
  finally
    CreatorA.Free;
    CreatorB.Free;
  end;
end;

procedure TControlCreatorTests.TestSubLevel;
var
  ControlCreator: TControlCreator;
  InitialLevel, NewLevel: TControlCreatorLevel;
begin
  ControlCreator := TControlCreator.Create;
  try
    InitialLevel := ControlCreator.CurrentLevel;

    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .Add(TControlBuilder.Create(TPanel).WithHeight(50).WithLeft(60))
      .SubLevel;

    NewLevel := ControlCreator.CurrentLevel;

    AssertNotSame(
      Format('Level inicial deve ser difente do level atual. [%s, %s]',
        [InitialLevel.GroupName, NewLevel.GroupName]),
      InitialLevel, NewLevel
    );
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestSubLevelSubLevel;
var
  ControlCreator: TControlCreator;
  InitialLevel, NewLevel_01, NewLevel_02: TControlCreatorLevel;
begin
  ControlCreator := TControlCreator.Create;
  try
    InitialLevel := ControlCreator.CurrentLevel;

    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .Add(TControlBuilder.Create(TPanel).WithHeight(50).WithLeft(60))
      .SubLevel;

    NewLevel_01 := ControlCreator
      .CurrentLevel;

    ControlCreator
        .SubLevel;

    NewLevel_02 := ControlCreator
        .CurrentLevel;

    AssertNotSame(
      Format('Level inicial deve ser difente do novo level 01. [%s, %s]',
        [InitialLevel.GroupName, NewLevel_01.GroupName]),
      InitialLevel, NewLevel_01
    );

    AssertNotSame(
      Format('Novo Level 01 deve ser difente do novo level 02. [%s, %s]',
        [NewLevel_01.GroupName, NewLevel_02.GroupName]),
      NewLevel_01, NewLevel_02
    );
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestSubLevelSuperLevel;
var
  ControlCreator: TControlCreator;
  InitialLevel, InitialLevelBack: TControlCreatorLevel;
begin
  ControlCreator := TControlCreator.Create;
  try
    InitialLevel := ControlCreator.CurrentLevel;

    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .SubLevel
      .SuperLevel;

    InitialLevelBack := ControlCreator.CurrentLevel;

    AssertSame(
      Format('Level inicial deve ser difente do level atual. [%s, %s]',
        [InitialLevel.GroupName, InitialLevelBack.GroupName]),
      InitialLevel, InitialLevelBack
    );
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestSubLevelEmpty;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .SetSpace(5, 6)
      .SubLevel
        // Sublevel vazio. Ao sair e inserir novo controle no level raiz,
        // deve ser como se esse sublevel não existisse.
        // Não pode levantar exceção.
        // Não pode considerar o espaçamento na inclusão do item
        // após o SuperLevel.
      .SuperLevel
      .Add(TControlBuilder.Create(TPanel, P));

    AssertEquals('Propriedade Top diferente da esperada', 0, P.Top);
    AssertEquals('Propriedade Left diferente da esperada', 0, P.Left);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestSubLevelDirectionSuperLevelDirection;
var
  ControlCreator: TControlCreator;
  P1, P2: TPanel;
  InitialDirection, SubLevelDirection, SuperLevelDirection: TControlCreatorDirection;
begin
  ControlCreator := TControlCreator.Create;
  try
    InitialDirection := ControlCreator.CurrentLevel.Direction;

    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .SetControlWidth(80)
      .SetControlHeight(50)
      .SetDirection(cpdHorizontal)
      .Add(TControlBuilder.Create(TPanel))          // Width 80 (A)
      .SubLevel
        .SetControlWidth(80)
        .SetControlHeight(50)
        .SetDirection(cpdVertical)
        .Add(TControlBuilder.Create(TPanel))       // Width 80 (B)
        .Add(TControlBuilder.Create(TPanel, P1))
    ;

    SubLevelDirection := ControlCreator.CurrentLevel.Direction;

    ControlCreator
      .SuperLevel
      .Add(TControlBuilder.Create(TPanel, P2))    // <-- left 160 (80A + 80B)
    ;

    SuperLevelDirection := ControlCreator.CurrentLevel.Direction;

    AssertTrue('InitialLevel com direção diferente da esperada.',
      SuperLevelDirection = cpdHorizontal
    );

    AssertTrue('SubLevel com direção diferente da esperada.',
      SubLevelDirection = cpdVertical
    );

    AssertTrue('SuperLevel com direção diferente da esperada.',
      SuperLevelDirection = cpdHorizontal
    );

    AssertTrue('InitialLevel e SuperLevel deveriam ser o mesmo objeto',
      InitialDirection = SuperLevelDirection);

    AssertEquals('Propriedade Top do Panel diferente da esperada', 50, P1.Top);

    AssertEquals('Propriedade Left do Panel diferente da esperada', 80 + 80, P2.Left);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestSiblingSubLevelWithOptionsMatchesPositionalOverload;
var
  CreatorOptions, CreatorPositional: TControlCreator;
  POptions, PPositional: TPanel;
begin
  // O overload TControlCreatorSiblingSubLevelOptions existe só pra evitar a
  // explosão combinatória de overloads posicionais (Direction/GroupName/
  // Break); ele precisa produzir exatamente o mesmo resultado que o
  // overload posicional equivalente.
  CreatorOptions := TControlCreator.Create;
  CreatorPositional := TControlCreator.Create;
  try
    CreatorOptions
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdHorizontal)
      .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(15, 10))
      .SubLevel('first')
        .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(15, 10))
      .SiblingSubLevel(
        TControlCreatorSiblingSubLevelOptions.Create
          .WithDirection(cpdVertical)
          .WithGroup('second')
          .WithBreak
      )
      .Add(TControlBuilder.Create(TPanel, POptions))
    ;

    CreatorPositional
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdHorizontal)
      .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(15, 10))
      .SubLevel('first')
        .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(15, 10))
      .SiblingSubLevel(cpdVertical, 'second', True)
      .Add(TControlBuilder.Create(TPanel, PPositional))
    ;

    AssertEquals('Top diferente entre o overload de Options e o posicional equivalente',
      PPositional.Top, POptions.Top);
    AssertEquals('Left diferente entre o overload de Options e o posicional equivalente',
      PPositional.Left, POptions.Left);
    AssertEquals('GroupName do level diferente entre Options e posicional',
      CreatorPositional.CurrentLevel.GroupName, CreatorOptions.CurrentLevel.GroupName);
    AssertTrue('Direction do level diferente entre Options e posicional',
      CreatorPositional.CurrentLevel.Direction = CreatorOptions.CurrentLevel.Direction);
  finally
    CreatorOptions.Free;
    CreatorPositional.Free;
  end;
end;

procedure TControlCreatorTests.TestSiblingSubLevelWithOptionsWithoutDirectionKeepsInheritedDirection;
var
  ControlCreator: TControlCreator;
  P: TPanel;
  DirectionBefore, DirectionAfter: TControlCreatorDirection;
begin
  // Quando Options não chama WithDirection, HasDirection fica False e o
  // level novo deve herdar a direção do level pai, igual ao overload
  // posicional SiblingSubLevel(AGroupName, ABreak) (sem ADirection).
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdVertical)
      .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(15, 10))
      .SubLevel('first')
        .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(15, 10))
    ;
    DirectionBefore := ControlCreator.CurrentLevel.Direction;

    // SiblingSubLevel sai do level 'first' (SuperLevel) antes de abrir o
    // novo level 'g' - por isso precisa estar dentro de algum SubLevel
    // antes de ser chamado, senão levanta "SuperLevel chamado no nível raiz".
    ControlCreator
      .SiblingSubLevel(TControlCreatorSiblingSubLevelOptions.Create.WithGroup('g'))
      .Add(TControlBuilder.Create(TPanel, P))
    ;
    DirectionAfter := ControlCreator.CurrentLevel.Direction;

    AssertTrue('Direção deveria ser herdada do level pai quando Options não chama WithDirection',
      DirectionBefore = DirectionAfter);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestSiblingSubLevelWithOptionsAndBuilderMatchesPositionalOverload;
var
  CreatorOptions, CreatorPositional: TControlCreator;
  ContainerOptions, ContainerPositional: TPanel;
  POptions, PPositional: TPanel;
begin
  // Mesma equivalência de TestSiblingSubLevelWithOptionsMatchesPositionalOverload,
  // mas para o overload de SiblingSubLevel que recebe um TControlBuilder
  // (o caminho que também cobre a especialização genérica IControlBuilder<TBuild>,
  // já que o overload concreto delega pra "specialize SiblingSubLevel<TControl>").
  CreatorOptions := TControlCreator.Create;
  CreatorPositional := TControlCreator.Create;
  try
    CreatorOptions
      .WithOwnerAndParent(FForm, FForm)
      .SubLevel(TControlBuilder.Create(TPanel, ContainerOptions).WithWidthAndHeight(100, 100))
        .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(20, 25))
      .SiblingSubLevel(
        TControlBuilder.Create(TPanel),
        TControlCreatorSiblingSubLevelOptions.Create.WithDirection(cpdVertical).WithBreak
      )
      .Add(TControlBuilder.Create(TPanel, POptions))
    ;

    CreatorPositional
      .WithOwnerAndParent(FForm, FForm)
      .SubLevel(TControlBuilder.Create(TPanel, ContainerPositional).WithWidthAndHeight(100, 100))
        .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(20, 25))
      .SiblingSubLevel(TControlBuilder.Create(TPanel), cpdVertical, True)
      .Add(TControlBuilder.Create(TPanel, PPositional))
    ;

    AssertEquals('Top diferente entre o overload de Options com builder e o posicional equivalente',
      PPositional.Top, POptions.Top);
    AssertEquals('Left diferente entre o overload de Options com builder e o posicional equivalente',
      PPositional.Left, POptions.Left);
  finally
    CreatorOptions.Free;
    CreatorPositional.Free;
  end;
end;

procedure TControlCreatorTests.TestRecalcParentSize;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
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

    AssertEquals('Propriedade Hight diferente da esperada', 25 * 2, P.Height);
    AssertEquals('Propriedade Width diferente da esperada', 20 * 5, P.Width);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestRecalcParentSizeWithExtraSizes;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
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

    AssertEquals('Propriedade Hight diferente da esperada', 25 * 2 + 12, P.Height);
    AssertEquals('Propriedade Width diferente da esperada', 20 * 5 + 17, P.Width);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestGridMode;
var
  ControlCreator: TControlCreator;
  P1, P2, P3, P4: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdHorizontal)
      .GridInit(2, 2)
        .GridSetCellWidthAndHeight(6, 7)
        .Add(TControlBuilder.Create(TPanel, P1))
        .Add(TControlBuilder.Create(TPanel, P2))
        .Add(TControlBuilder.Create(TPanel, P3))
        .Add(TControlBuilder.Create(TPanel, P4))
      .GridFinish
    ;

    AssertEquals('Propriedade Top diferente da esperada para P1', 0, P1.Top);
    AssertEquals('Propriedade Top diferente da esperada para P2', 0, P2.Top);
    AssertEquals('Propriedade Top diferente da esperada para P3', 7, P3.Top);
    AssertEquals('Propriedade Top diferente da esperada para P4', 7, P4.Top);

    AssertEquals('Propriedade Left diferente da esperada para P1', 0, P1.Left);
    AssertEquals('Propriedade Left diferente da esperada para P2', 6, P2.Left);
    AssertEquals('Propriedade Left diferente da esperada para P3', 0, P3.Left);
    AssertEquals('Propriedade Left diferente da esperada para P4', 6, P4.Left);

    AssertEquals('Propriedade Height diferente da esperada para P1', 7, P1.Height);
    AssertEquals('Propriedade Height diferente da esperada para P2', 7, P2.Height);
    AssertEquals('Propriedade Height diferente da esperada para P3', 7, P3.Height);
    AssertEquals('Propriedade Height diferente da esperada para P4', 7, P4.Height);

    AssertEquals('Propriedade Width diferente da esperada para P1', 6, P1.Width);
    AssertEquals('Propriedade Width diferente da esperada para P2', 6, P2.Width);
    AssertEquals('Propriedade Width diferente da esperada para P3', 6, P3.Width);
    AssertEquals('Propriedade Width diferente da esperada para P4', 6, P4.Width);

  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestGridModeIgnoreAddAfterEndOfGrid;
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
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdHorizontal)
      .GridInit(2, 2)
        .GridSetCellWidthAndHeight(6, 7)
        .Add(TControlBuilder.Create(TPanel, P1))
        .Add(TControlBuilder.Create(TPanel, P2))
        .Add(TControlBuilder.Create(TPanel, P3))
        .Add(TControlBuilder.Create(TPanel, P4))
        .Add(TControlBuilder.Create(TPanel, P5))  // deve ignorar pois já finalizou o grid
      .GridFinish
    ;

    AssertNotNull('P1 não deverfia ser nil', P1);
    AssertNotNull('P1 não deverfia ser nil', P2);
    AssertNotNull('P1 não deverfia ser nil', P3);
    AssertNotNull('P1 não deverfia ser nil', P4);
    AssertNull('P5 deverfia ser nil', P5);

  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestGridModeNotIgnoreAddAfterEndOfGridWhenAutoExpand;
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
      .WithOwnerAndParent(FForm, FForm)
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

    AssertNotNull('P1 não deverfia ser nil', P1);
    AssertNotNull('P1 não deverfia ser nil', P2);
    AssertNotNull('P1 não deverfia ser nil', P3);
    AssertNotNull('P1 não deverfia ser nil', P4);
    AssertNotNull('P5 não deverfia ser nil', P5);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestGridModeAutoExpandRows;
var
  ControlCreator: TControlCreator;
  RowsBeforeExpand: Integer;
  RowsAfterExpand: Integer;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
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

    AssertEquals('O número de linhas do grid antes de expandir está diferente do esperado', 2, RowsBeforeExpand);
    AssertEquals('O número de linhas do grid depois de expandir está diferente do esperado', 3, RowsAfterExpand);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestGridModeAutoExpandCols;
var
  ControlCreator: TControlCreator;
  ColsBeforeExpand: Integer;
  ColsAfterExpand: Integer;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
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

    AssertEquals('O número de colunas do grid antes de expandir está diferente do esperado', 2, ColsBeforeExpand);
    AssertEquals('O número de dolunas do grid depois de expandir está diferente do esperado', 3, ColsAfterExpand);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestGridModeAutoExpandOnlyRows;
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
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdHorizontal)
      .GridInit(2, 2)
        .GridAutoExpandRows                                         // aqui define que apenas expande Linhas
        .Add(TControlBuilder.Create(TPanel).WithCaption('1'))
        .Add(TControlBuilder.Create(TPanel).WithCaption('2'))
        .Add(TControlBuilder.Create(TPanel).WithCaption('3'))
        .Add(TControlBuilder.Create(TPanel).WithCaption('4'))

        .GridReturnNumberOfRows(RowsBeforeExpand)
        .Add(TControlBuilder.Create(TPanel).WithCaption('5'))  // aqui expande automaticamente linhas para 3
        .GridReturnNumberOfRows(RowsAfterExpand)

        .SetDirection(cpdVertical) // ao mudar pra direção vertical, apos preencher a ultima celula da coluna, tentará criar uma nova coluna

        .Add(TControlBuilder.Create(TPanel).WithCaption('6'))
        .GridReturnNumberOfCols(ColsBeforeExpand)
        .Add(TControlBuilder.Create(TPanel, P).WithCaption('7'))  // aqui ignora pois não expande colunas
        .GridReturnNumberOfCols(ColsAfterExpand)
      .GridFinish
    ;


    AssertEquals('O número de linhas do grid antes de expandir está diferente do esperado', 2, RowsBeforeExpand);
    AssertEquals('O número de linhas do grid depois de expandir está diferente do esperado', 3, RowsAfterExpand);
    AssertEquals('O número de colunas do grid antes de expandir está diferente do esperado', 2, ColsBeforeExpand);
    AssertEquals('O número de colunas do grid depois de expandir está diferente do esperado', 2, ColsAfterExpand);

    AssertNull('P deveria ser nil, pois o grid pode expandir apenas linhas e não colunas', P);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestGridModeAutoExpandOnlyCols;
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
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdVertical)
      .GridInit(2, 2)
        .GridAutoExpandCols                                         // aqui define que apenas expande Colunas
        .Add(TControlBuilder.Create(TPanel).WithCaption('1'))
        .Add(TControlBuilder.Create(TPanel).WithCaption('2'))
        .Add(TControlBuilder.Create(TPanel).WithCaption('3'))
        .Add(TControlBuilder.Create(TPanel).WithCaption('4'))

        .GridReturnNumberOfCols(ColsBeforeExpand)
        .Add(TControlBuilder.Create(TPanel).WithCaption('5'))  // aqui expande automaticamente Colunas para 3
        .GridReturnNumberOfCols(ColsAfterExpand)

        .SetDirection(cpdHorizontal) // ao mudar pra direção vertical, apos preencher a ultima celula da linha, tentará criar uma nova linha

        .Add(TControlBuilder.Create(TPanel).WithCaption('6'))
        .GridReturnNumberOfRows(RowsBeforeExpand)
        .Add(TControlBuilder.Create(TPanel, P).WithCaption('7'))  // aqui ignora pois não expande colunas
        .GridReturnNumberOfRows(RowsAfterExpand)
      .GridFinish
    ;

    AssertEquals('O número de colunas do grid antes de expandir está diferente do esperado', 2, ColsBeforeExpand);
    AssertEquals('O número de colunas do grid depois de expandir está diferente do esperado', 3, ColsAfterExpand);
    AssertEquals('O número de linhas do grid antes de expandir está diferente do esperado', 2, RowsBeforeExpand);
    AssertEquals('O número de linhas do grid depois de expandir está diferente do esperado', 2, RowsAfterExpand);

    AssertNull('P deveria ser nil, pois o grid pode expandir apenas linhas e não colunas', P);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestGridModeWithSubLevel;
var
  ControlCreator: TControlCreator;
  P1, P2: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
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
          // P1 e P2 estão em um sublevel dentro de uma celula, mas eles
          // não devem se comportar como conteudo da celula
          .Add(TControlBuilder.Create(TPanel, P1).WithWidthAndHeight(6, 7))
          .Add(TControlBuilder.Create(TPanel, P2))
        .SuperLevel
      .GridFinish
    ;

    AssertEquals('Propriedade Top de P1 diferente da esperada', 25, P1.Top);
    AssertEquals('Propriedade Left de P1 diferente da esperada', 20, P1.Left);

    AssertEquals('Propriedade Top de P2 diferente da esperada', 25, P2.Top);
    AssertEquals('Propriedade Left de P2 diferente da esperada', 20 + 6, P2.Left);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestGridSkipCell;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdHorizontal)
      .GridInit(2, 3)
        .GridSetCellWidthAndHeight(10, 15)

        .Add(TControlBuilder.Create(TPanel))
        .GridSkipCell
        .Add(TControlBuilder.Create(TPanel, P))
      .GridFinish
    ;

    AssertEquals('Propriedade Left diferente do esperado', 10 + 10, P.Left);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestGridSkipCells;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdHorizontal)
      .GridInit(1, 4)
        .GridSetCellWidthAndHeight(10, 15)
        .Add(TControlBuilder.Create(TPanel))
        .GridSkipCells(2)
        .Add(TControlBuilder.Create(TPanel, P))
      .GridFinish
    ;

    AssertEquals('Propriedade Left diferente do esperado', 10 * 3, P.Left);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestGridRowHeight;
var
  ControlCreator: TControlCreator;
  P1, P2: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdHorizontal)
      .GridInit(3, 3)
        .GridSetCellWidthAndHeight(50, 60)  // altura padrão da linha = 60
        .GridSetRowHeight(0, 100)           // altera a linha 0 para altura = 100

        .Add(TControlBuilder.Create(TPanel, P1))  // p1 esta na linha 0
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

    AssertEquals('propriedade Height de P1 diferente do esperada', 100, P1.Height);
    AssertEquals('Propriedade Height de P2 diferente do esperada', 60, P2.Height);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestGridColWidth;
var
  ControlCreator: TControlCreator;
  P1, P2: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdHorizontal)
      .GridInit(3, 3)
        .GridSetCellWidthAndHeight(50, 60)  // largura padrão da linha = 50
        .GridSetColWidth(0, 90)           // altera a coluna 0 para largura = 90

        .Add(TControlBuilder.Create(TPanel, P1))  // p1 esta na coluna 0
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

    AssertEquals('propriedade Width de P1 diferente do esperada', 90, P1.Width);
    AssertEquals('Propriedade Width de P2 diferente do esperada', 50, P2.Width);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestGridRowSpan;
var
  ControlCreator: TControlCreator;
  P1, P2: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdVertical)
      .GridInit(3, 3)
        .GridSetCellWidthAndHeight(50, 60)  // altura da celula

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

    AssertEquals('propriedade Height de P1 diferente da esperada', 60, P1.Height);
    AssertEquals('Propriedade Height de P2 diferente da esperada', 60*2, P2.Height);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestGridColSpan;
var
  ControlCreator: TControlCreator;
  P1, P2: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
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

    AssertEquals('propriedade Width de P1 diferente da esperada', 50, P1.Width);
    AssertEquals('Propriedade Width de P2 diferente da esperada', 50*2, P2.Width);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestGridRowSpanOnlyOnce;
var
  ControlCreator: TControlCreator;
  P1, P2, P3: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdVertical)
      .GridInit(4, 2)
        .GridSetCellWidthAndHeight(50, 60)  // altura da celula

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

    AssertEquals('propriedade Height de P1 diferente da esperada', 60, P1.Height);
    AssertEquals('Propriedade Height de P2 diferente da esperada', 60*2, P2.Height);
    AssertEquals('propriedade Height de P3 diferente da esperada', 60, P3.Height);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestGridColSpanOnlyOnce;
var
  ControlCreator: TControlCreator;
  P1, P2, P3: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
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

    AssertEquals('propriedade Width de P1 diferente da esperada', 50, P1.Width);
    AssertEquals('Propriedade Width de P2 diferente da esperada', 50*2, P2.Width);
    AssertEquals('propriedade Width de P3 diferente da esperada', 50, P3.Width);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestGridRowSpanOutOfBounds;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdVertical)
      .GridInit(3, 3)
        .GridSetCellWidthAndHeight(50, 60)  // altura da celula

        .Add(TControlBuilder.Create(TPanel)) // sem rowspan

        // span 3 irá ultrapassar limite da ultima linha do grid.
        // espera-se que limite a 2
        .GridRowSpan(3)

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

    AssertEquals('Propriedade Height de P2 diferente do esperada', 60*2, P.Height);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestGridColSpanOutOfBounds;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdHorizontal)
      .GridInit(3, 3)
        .GridSetCellWidthAndHeight(50, 60)  // altura da celula

        .Add(TControlBuilder.Create(TPanel))

        // span 3 irá ultrapassar limite da ultima coluna do grid.
        // espera-se que limite a 2
        .GridColSpan(3)

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

    AssertEquals('Propriedade Width de P2 diferente do esperada', 50*2, P.Width);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestGridRowSpanExpandGridRows;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdVertical)
      .GridInit(3, 3)
        .GridAutoExpand
        .GridSetCellWidthAndHeight(50, 60)  // altura da celula

        .Add(TControlBuilder.Create(TPanel)) // sem rowspan

        // span 3 irá ultrapassar limite da ultima linha do grid.
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

    AssertEquals('Propriedade Height de P2 diferente do esperada', 60*3, P.Height);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestGridColSpanExpandGridCols;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdHorizontal)
      .GridInit(3, 3)
        .GridAutoExpand
        .GridSetCellWidthAndHeight(50, 60)  // altura da celula

        .Add(TControlBuilder.Create(TPanel))

        // span 3 irá ultrapassar limite da ultima coluna do grid.
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

    AssertEquals('Propriedade Width de P diferente da esperada', 50*3, P.Width);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestGridGotoCell;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdHorizontal)
      .GridInit(5, 5)
        .GridSetCellWidthAndHeight(30, 42)
        .GridGotoCell(2, 3)
        .Add(TControlBuilder.Create(TPanel, P))
      .GridFinish
    ;

    AssertEquals('Propriedade Top diferente da esperada', 42*2, P.Top);
    AssertEquals('Propriedade Left diferente da esperada', 30*3, P.Left);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestGridRowSpanWithSpace;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdVertical)
      .SetVerticalSpace(7)                  // define o espacamento entre duas linhas
      .GridInit(3, 3)
        .GridSetCellWidthAndHeight(50, 60)  // altura da celula

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

    AssertEquals('propriedade Height diferente da esperada', 60*2 + 7, P.Height);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestGridColSpanWithSpace;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdHorizontal)
      .SetHorizontalSpace(9)            // define o espaçaento entre as colunas
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

    AssertEquals('Propriedade Width diferente da esperada', 50*2+9, P.Width);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestCellStrechHorizontal;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .GridInit(1, 1)
        .GridSetCellWidthAndHeight(50, 60)
        .GridCellStrechHorizontal                                                // strech na horizontal = largura da celula
        .Add(TControlBuilder.Create(TPanel, P).WithWidthAndHeight(30, 25))
      .GridFinish
    ;

    AssertEquals('Propriedade Width diferente da esperada', 50, P.Width);        // usa largura da celula (streched)
    AssertEquals('Propriedade Height diferente da esperada', 25, P.Height);      // usa altura definida TControlBuilder (not streched)
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestCellStrechVertical;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .GridInit(1, 1)
        .GridSetCellWidthAndHeight(50, 60)
        .GridCellStrechVertical                                                // strech na horizontal = largura da celula
        .Add(TControlBuilder.Create(TPanel, P).WithWidthAndHeight(30, 25))
      .GridFinish
    ;

    AssertEquals('Propriedade Width diferente da esperada', 30, P.Width);        // usa largura definida TControlBuilder (not streched)
    AssertEquals('Propriedade Height diferente da esperada', 60, P.Height);      // usa altura da celula (streched)
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestCellStrechAll;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .GridInit(1, 1)
        .GridSetCellWidthAndHeight(50, 60)
        .GridCellStrechAll                                                // strech na horizontal e vertical
        .Add(TControlBuilder.Create(TPanel, P).WithWidthAndHeight(30, 25))   // 30 e 25: esses valores sao ignorados
      .GridFinish
    ;

    AssertEquals('Propriedade Width diferente da esperada', 50, P.Width);        // usa largura da celula (streched)
    AssertEquals('Propriedade Height diferente da esperada', 60, P.Height);      // usa altura da celula (streched)
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestCellNoStrech;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .GridInit(1, 1)
        .GridSetCellWidthAndHeight(50, 60)
        .GridCellNoStrech                                                     // sem strech
        .Add(TControlBuilder.Create(TPanel, P).WithWidthAndHeight(30, 25))
      .GridFinish
    ;

    AssertEquals('Propriedade Width diferente da esperada', 30, P.Width);        // usa largura definida no TControlBuilder
    AssertEquals('Propriedade Height diferente da esperada', 25, P.Height);      // usa altura definida no TControlBuilder
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestCellNoStrechCenter;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .GridInit(1, 1)
        .GridSetCellWidthAndHeight(60, 50)
        .GridCellNoStrech
        .GridCellPosition(cpCenter)
        .Add(TControlBuilder.Create(TPanel, P).WithWidthAndHeight(10, 14))
      .GridFinish
    ;

    AssertEquals('Propriedade Left diferente da esperada', (60 div 2) - (10 div 2), P.Left);
    AssertEquals('Propriedade Top diferente da esperada', (50 div 2) - (14 div 2), P.Top);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestCellNoStrechTop;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .GridInit(1, 1)
        .GridSetCellWidthAndHeight(60, 50)
        .GridCellNoStrech
        .GridCellPosition(cpTop)
        .Add(TControlBuilder.Create(TPanel, P).WithWidthAndHeight(10, 14))
      .GridFinish
    ;

    AssertEquals('Propriedade Left diferente da esperada', (60 div 2) - (10 div 2), P.Left);
    AssertEquals('Propriedade Top diferente da esperada', 0, P.Top);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestCellNoStrechTopRight;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .GridInit(1, 1)
        .GridSetCellWidthAndHeight(60, 50)
        .GridCellNoStrech
        .GridCellPosition(cpTopRight)
        .Add(TControlBuilder.Create(TPanel, P).WithWidthAndHeight(10, 14))
      .GridFinish
    ;

    AssertEquals('Propriedade Left diferente da esperada', 60 - 10, P.Left);
    AssertEquals('Propriedade Top diferente da esperada', 0, P.Top);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestCellNoStrechRight;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .GridInit(1, 1)
        .GridSetCellWidthAndHeight(60, 50)
        .GridCellNoStrech
        .GridCellPosition(cpRight)
        .Add(TControlBuilder.Create(TPanel, P).WithWidthAndHeight(10, 14))
      .GridFinish
    ;

    AssertEquals('Propriedade Left diferente da esperada', 60 - 10, P.Left);
    AssertEquals('Propriedade Top diferente da esperada', (50 div 2) - (14 div 2), P.Top);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestCellNoStrechBottomRight;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .GridInit(1, 1)
        .GridSetCellWidthAndHeight(60, 50)
        .GridCellNoStrech
        .GridCellPosition(cpBottomRight)
        .Add(TControlBuilder.Create(TPanel, P).WithWidthAndHeight(10, 14))
      .GridFinish
    ;

    AssertEquals('Propriedade Left diferente da esperada', 60 - 10, P.Left);
    AssertEquals('Propriedade Top diferente da esperada', 50 - 14, P.Top);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestCellNoStrechBottom;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .GridInit(1, 1)
        .GridSetCellWidthAndHeight(60, 50)
        .GridCellNoStrech
        .GridCellPosition(cpBottom)
        .Add(TControlBuilder.Create(TPanel, P).WithWidthAndHeight(10, 14))
      .GridFinish
    ;

    AssertEquals('Propriedade Left diferente da esperada', (60 div 2) - (10 div 2), P.Left);
    AssertEquals('Propriedade Top diferente da esperada', 50 - 14, P.Top);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestCellNoStrechBottomLeft;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .GridInit(1, 1)
        .GridSetCellWidthAndHeight(60, 50)
        .GridCellNoStrech
        .GridCellPosition(cpBottomLeft)
        .Add(TControlBuilder.Create(TPanel, P).WithWidthAndHeight(10, 14))
      .GridFinish
    ;

    AssertEquals('Propriedade Left diferente da esperada', 0, P.Left);
    AssertEquals('Propriedade Top diferente da esperada', 50 - 14, P.Top);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestCellNoStrechLeft;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .GridInit(1, 1)
        .GridSetCellWidthAndHeight(60, 50)
        .GridCellNoStrech
        .GridCellPosition(cpLeft)
        .Add(TControlBuilder.Create(TPanel, P).WithWidthAndHeight(10, 14))
      .GridFinish
    ;

    AssertEquals('Propriedade Left diferente da esperada', 0, P.Left);
    AssertEquals('Propriedade Top diferente da esperada', (50 div 2) - (14 div 2), P.Top);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestCellNoStrechTopLeft;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .GridInit(1, 1)
        .GridSetCellWidthAndHeight(60, 50)
        .GridCellNoStrech
        .GridCellPosition(cpTopLeft)
        .Add(TControlBuilder.Create(TPanel, P).WithWidthAndHeight(10, 14))
      .GridFinish
    ;

    AssertEquals('Propriedade Left diferente da esperada', 0, P.Left);
    AssertEquals('Propriedade Top diferente da esperada', 0, P.Top);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestGridRowOffset;
var
  ControlCreator: TControlCreator;
  P1, P2, P3, P4, P5, P6, P7, P8, P9: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdHorizontal)
      .GridInit(3, 3)
        .GridSetCellWidthAndHeight(50, 60)
        .GridSetRowOffset(1, 15)                      // segunda linha com offset positivo de 15
        .Add(TControlBuilder.Create(TPanel, P1))  // linha 1
        .Add(TControlBuilder.Create(TPanel, P2))  // linha 1
        .Add(TControlBuilder.Create(TPanel, P3))  // linha 1

        .Add(TControlBuilder.Create(TPanel, P4))  // linha 2
        .Add(TControlBuilder.Create(TPanel, P5))  // linha 2
        .Add(TControlBuilder.Create(TPanel, P6))  // linha 2

        .Add(TControlBuilder.Create(TPanel, P7))  // linha 3
        .Add(TControlBuilder.Create(TPanel, P8))  // linha 3
        .Add(TControlBuilder.Create(TPanel, P9))  // linha 3
      .GridFinish
    ;

    // primeira coluna
    AssertEquals('propriedade Left de P1 diferente da esperada', 0, P1.Left);
    AssertEquals('propriedade Left de P4 diferente da esperada', 15, P4.Left);  // linha com offset
    AssertEquals('propriedade Left de P7 diferente da esperada', 0, P7.Left);

    // segunda coluna
    AssertEquals('propriedade Left de P2 diferente da esperada', 50, P2.Left);
    AssertEquals('propriedade Left de P5 diferente da esperada', 50 + 15, P5.Left);  // linha com offset
    AssertEquals('propriedade Left de P8 diferente da esperada', 50, P8.Left);

    // terceira coluna
    AssertEquals('propriedade Left de P3 diferente da esperada', 50*2, P3.Left);
    AssertEquals('propriedade Left de P6 diferente da esperada', 50*2 + 15, P6.Left);  // linha com offset
    AssertEquals('propriedade Left de P9 diferente da esperada', 50*2, P9.Left);

  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestGridColOffset;
var
  ControlCreator: TControlCreator;
  P1, P2, P3, P4, P5, P6, P7, P8, P9: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdVertical)
      .GridInit(3, 3)
        .GridSetCellWidthAndHeight(50, 60)
        .GridSetColOffset(1, 15)                      // segunda coluna com offset positivo de 15
        .Add(TControlBuilder.Create(TPanel, P1))  // coluna 1
        .Add(TControlBuilder.Create(TPanel, P2))  // coluna 1
        .Add(TControlBuilder.Create(TPanel, P3))  // coluna 1

        .Add(TControlBuilder.Create(TPanel, P4))  // coluna 2
        .Add(TControlBuilder.Create(TPanel, P5))  // coluna 2
        .Add(TControlBuilder.Create(TPanel, P6))  // coluna 2

        .Add(TControlBuilder.Create(TPanel, P7))  // coluna 3
        .Add(TControlBuilder.Create(TPanel, P8))  // coluna 3
        .Add(TControlBuilder.Create(TPanel, P9))  // coluna 3
      .GridFinish
    ;

    // primeira linha
    AssertEquals('propriedade Top de P1 diferente da esperada', 0, P1.Top);
    AssertEquals('propriedade Top de P4 diferente da esperada', 15, P4.Top);  // coluna com offset
    AssertEquals('propriedade Top de P7 diferente da esperada', 0, P7.Top);

    // segunda linha
    AssertEquals('propriedade Top de P2 diferente da esperada', 60, P2.Top);
    AssertEquals('propriedade Top de P5 diferente da esperada', 60 + 15, P5.Top);  // coluna com offset
    AssertEquals('propriedade Top de P8 diferente da esperada', 60, P8.Top);

    // terceira linha
    AssertEquals('propriedade Top de P3 diferente da esperada', 60*2, P3.Top);
    AssertEquals('propriedade Top de P6 diferente da esperada', 60*2 + 15, P6.Top);  // coluna com offset
    AssertEquals('propriedade Top de P9 diferente da esperada', 60*2, P9.Top);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestGridBreakLine;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdHorizontal)
      .GridInit(2, 4)
        .GridSetCellWidthAndHeight(10, 15)
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .BreakLine
        .Add(TControlBuilder.Create(TPanel, P)) // segunda linha, primeira coluna
      .GridFinish
    ;

    AssertEquals('Propriedade Top diferente da esperada', 15, P.Top);
    AssertEquals('Propriedade Left diferente da esperada', 0, P.Left);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestGridBreakColumn;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdVertical)
      .GridInit(4, 2)
        .GridSetCellWidthAndHeight(10, 15)
        .Add(TControlBuilder.Create(TPanel))
        .Add(TControlBuilder.Create(TPanel))
        .BreakColumn
        .Add(TControlBuilder.Create(TPanel, P)) // segunda coluna, primeira linha
      .GridFinish
    ;

    AssertEquals('Propriedade Top diferente da esperada', 0, P.Top);
    AssertEquals('Propriedade Left diferente da esperada', 10, P.Left);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestGrid1x1;
{ testa bug corrigido. não incluia controle em grid 1x1 }
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    P := nil;
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .GridInit(1, 1)
        .GridSetCellWidthAndHeight(50, 50)
        .GridCellStrechAll
        .Add(TControlBuilder.Create(TPanel, P))
      .GridFinish
    ;

    AssertNotNull('P não deveria ser null', P);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestGridGoToLastCell;
{ testa bug corrigido. não incluia controle ao posicionar na última celula. }
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    P := nil;
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .GridInit(2, 2)
        .GridGotoCell(1, 1)
        .GridSetCellWidthAndHeight(50, 50)
        .GridCellStrechAll
        .Add(TControlBuilder.Create(TPanel, P))
      .GridFinish
    ;

    AssertNotNull('P não deveria ser null', P);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestGridSpanOverlapRaises;
{ regressão: Step só conferia se a célula INICIAL estava livre; o resto do
  span (via RowSpan/ColSpan) era marcado ocupado sem checar sobreposição.
  Aqui a célula (0,0) está livre, mas o span de 2 linhas invade (1,0), que
  já foi ocupada por outro controle - deve levantar erro, não sobrepor
  silenciosamente. }
var
  ControlCreator: TControlCreator;
  P1, P2: TPanel;
  Raised: Boolean;
begin
  Raised := False;
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .GridInit(3, 3)
        .GridGotoCell(1, 0)
        .Add(TControlBuilder.Create(TPanel, P1))  // ocupa (1,0)
    ;

    try
      ControlCreator
        .GridGotoCell(0, 0)
        .GridRowSpan(2)
        .Add(TControlBuilder.Create(TPanel, P2))  // (0,0) livre, mas span invade (1,0) já ocupada
      ;
    except
      on E: Exception do
        Raised := True;
    end;

    AssertTrue('Sobreposição de span deveria levantar exceção', Raised);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestBreakLineOnLastRowIsNoOp;
{ regressão: BreakLine na última linha do grid chamava GridGotoCell(Rows,0),
  que levantava "GridGotoCell: linha fora dos limites" - um método que quem
  chamou BreakLine nunca invocou diretamente. Deve ser no-op, igual ao
  BreakLine fora do modo grid (que nunca lança). }
var
  ControlCreator: TControlCreator;
  Raised: Boolean;
begin
  Raised := False;
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .GridInit(2, 2)
        .GridGotoCell(1, 0); // já na última linha

    try
      ControlCreator.BreakLine;
    except
      on E: Exception do
        Raised := True;
    end;

    AssertFalse('BreakLine na última linha não deveria lançar exceção', Raised);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestBreakColumnOnLastColIsNoOp;
var
  ControlCreator: TControlCreator;
  Raised: Boolean;
begin
  Raised := False;
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .GridInit(2, 2)
        .GridGotoCell(0, 1); // já na última coluna

    try
      ControlCreator.BreakColumn;
    except
      on E: Exception do
        Raised := True;
    end;

    AssertFalse('BreakColumn na última coluna não deveria lançar exceção', Raised);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestSubLevelWithNonWinControlRaises;
{ regressão: SubLevel fazia TWinControl(Control) via cast direto, sem
  checar o tipo. TLabel não é TWinControl (é TGraphicControl) - passar um
  builder de TLabel para SubLevel deveria levantar um erro claro em vez de
  corromper o próximo Add silenciosamente. Também verifica que a exceção
  não deixa FLevelStack com um nível extra órfão (a checagem de tipo
  precisa rodar ANTES do SubLevel(AGroupName) empilhar um nível novo) -
  um Add logo depois deve continuar criando no nível original, com FForm
  como Parent direto. }
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
        .WithOwnerAndParent(FForm, FForm)
        .SubLevel(TControlBuilder.Create(TLabel));
    except
      on E: Exception do
        Raised := True;
    end;

    AssertTrue('SubLevel com um controle que não é TWinControl deveria levantar exceção', Raised);

    P := nil;
    ControlCreator.Add(TControlBuilder.Create(TPanel, P));
    AssertNotNull('P não deveria ser nil', P);
    AssertSame('Parent do controle criado depois deveria ser FForm diretamente (nível não deveria ter ficado corrompido)',
      FForm, P.Parent);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestAddWithGridFullDiscardsOutReference;
{ regressão: quando o grid já está cheio, Add saía antes de chamar Build,
  então o "out Reference" do builder nunca era escrito e a variável ficava
  com o valor anterior (lixo) em vez de nil. }
var
  ControlCreator: TControlCreator;
  Dummy: TPanel;
  P1, P2: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  Dummy := TPanel.Create(nil);
  try
    P2 := Dummy; // valor não-nil conhecido, simulando "lixo" de uma referência anterior
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .GridInit(1, 1)
        .Add(TControlBuilder.Create(TPanel, P1))
        .Add(TControlBuilder.Create(TPanel, P2))  // grid já cheio: não deveria criar nem manter o valor antigo
      .GridFinish
    ;

    AssertNotNull('P1 deveria ter sido criado normalmente', P1);
    AssertNull('P2 deveria ser nil (grid cheio, Build nunca chegou a ser chamado)', P2);
  finally
    Dummy.Free;
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestAlignControlsRight;
var
  ControlCreator: TControlCreator;
  RefPanel, Btn1, Btn2: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .SetTopLeft(0, 10)
      .Add(TControlBuilder.Create(TPanel, 'ref_panel', RefPanel).WithWidth(250))
      .SetTopLeft(0, 0)
      .Add(TControlBuilder.Create(TPanel, 'btn1', Btn1).WithWidth(50))
      .Add(TControlBuilder.Create(TPanel, 'btn2', Btn2).WithLeft(50).WithWidth(60))
      .AlignControlsRight(['btn1', 'btn2'], ['ref_panel'])
    ;

    // borda direita do ref_panel: Left(10) + Width(250) = 260
    // grupo [btn1, btn2] tem largura total 110 (50 + 60), deslocado para
    // que a borda direita do grupo coincida com a do ref_panel
    AssertEquals('Left de btn1 diferente do esperado', 150, Btn1.Left);
    AssertEquals('Left de btn2 diferente do esperado', 200, Btn2.Left);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestAlignControlsRightWithPadding;
var
  ControlCreator: TControlCreator;
  RefPanel, Btn1: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .SetTopLeft(0, 10)
      .Add(TControlBuilder.Create(TPanel, 'ref_panel', RefPanel).WithWidth(250))
      .SetTopLeft(0, 0)
      .Add(TControlBuilder.Create(TPanel, 'btn1', Btn1).WithWidth(50))
      .AlignControlsRight(['btn1'], ['ref_panel'], 10) // com 10px de padding
    ;

    // borda direita do ref_panel (260) - largura de btn1 (50) - padding (10)
    AssertEquals('Left de btn1 diferente do esperado', 260 - 50 - 10, Btn1.Left);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestMoveControlsSingleControlAppliesDelta;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .Add(TControlBuilder.Create(TPanel, P).WithTop(10).WithLeft(20))
      .MoveControls(P, 5, 7)
    ;

    AssertEquals('Propriedade Left diferente da esperada', 20 + 5, P.Left);
    AssertEquals('Propriedade Top diferente da esperada', 10 + 7, P.Top);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestMoveControlsByNamesAppliesDeltaToEach;
var
  ControlCreator: TControlCreator;
  P1, P2: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .Add(TControlBuilder.Create(TPanel, 'p1', P1).WithTop(10).WithLeft(20))
      .Add(TControlBuilder.Create(TPanel, 'p2', P2).WithTop(30).WithLeft(40))
      .MoveControls(['p1', 'p2'], 5, 7)
    ;

    AssertEquals('Propriedade Left de P1 diferente da esperada', 20 + 5, P1.Left);
    AssertEquals('Propriedade Top de P1 diferente da esperada', 10 + 7, P1.Top);
    AssertEquals('Propriedade Left de P2 diferente da esperada', 40 + 5, P2.Left);
    AssertEquals('Propriedade Top de P2 diferente da esperada', 30 + 7, P2.Top);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestMoveControlsByNamesRaisesWhenControlNotFound;
var
  ControlCreator: TControlCreator;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator.WithOwnerAndParent(FForm, FForm);

    try
      ControlCreator.MoveControls(['inexistente'], 1, 1);
      Fail('Deveria ter levantado exceção para controle inexistente');
    except
      on E: Exception do
        AssertTrue('Mensagem da exceção deveria mencionar o nome do controle',
          Pos('inexistente', E.Message) > 0);
    end;
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestSetTopLeftNearControlBelow;
var
  ControlCreator: TControlCreator;
  Ref, P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .SetSpace(5, 6)
      .Add(TControlBuilder.Create(TPanel, 'ref', Ref).WithTop(10).WithLeft(20).WithWidthAndHeight(50, 30))
      .SetTopLeftNearControl('ref', rpBelow)
      .Add(TControlBuilder.Create(TPanel, P))
    ;

    AssertEquals('Propriedade Left diferente da esperada', 20, P.Left);
    AssertEquals('Propriedade Top diferente da esperada', 10 + 30 + 5, P.Top);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestSetTopLeftNearControlRight;
var
  ControlCreator: TControlCreator;
  Ref, P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .SetSpace(5, 6)
      .Add(TControlBuilder.Create(TPanel, 'ref', Ref).WithTop(10).WithLeft(20).WithWidthAndHeight(50, 30))
      .SetTopLeftNearControl('ref', rpRight)
      .Add(TControlBuilder.Create(TPanel, P))
    ;

    AssertEquals('Propriedade Top diferente da esperada', 10, P.Top);
    AssertEquals('Propriedade Left diferente da esperada', 20 + 50 + 6, P.Left);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestSetTopLeftNearControlsBelow;
var
  ControlCreator: TControlCreator;
  Ref1, Ref2, P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .SetSpace(5, 6)
      .Add(TControlBuilder.Create(TPanel, 'ref1', Ref1).WithTop(10).WithLeft(20).WithWidthAndHeight(50, 30))
      .Add(TControlBuilder.Create(TPanel, 'ref2', Ref2).WithTop(10).WithLeft(80).WithWidthAndHeight(50, 60))
      .SetTopLeftNearControls(['ref1', 'ref2'], rpBelow)
      .Add(TControlBuilder.Create(TPanel, P))
    ;

    // Bounds envelope de [ref1, ref2]: Left=20 (min), Bottom=max(40, 70)=70
    AssertEquals('Propriedade Left diferente da esperada', 20, P.Left);
    AssertEquals('Propriedade Top diferente da esperada', 70 + 5, P.Top);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestSetTopLeftNearControlsRight;
var
  ControlCreator: TControlCreator;
  Ref1, Ref2, P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .SetSpace(5, 6)
      .Add(TControlBuilder.Create(TPanel, 'ref1', Ref1).WithTop(10).WithLeft(20).WithWidthAndHeight(50, 30))
      .Add(TControlBuilder.Create(TPanel, 'ref2', Ref2).WithTop(10).WithLeft(80).WithWidthAndHeight(50, 60))
      .SetTopLeftNearControls(['ref1', 'ref2'], rpRight)
      .Add(TControlBuilder.Create(TPanel, P))
    ;

    // Bounds envelope de [ref1, ref2]: Right=max(70, 130)=130
    AssertEquals('Propriedade Top diferente da esperada', 10, P.Top);
    AssertEquals('Propriedade Left diferente da esperada', 130 + 6, P.Left);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestSetTopLeftNearGroupBelow;
var
  ControlCreator: TControlCreator;
  G1, G2, P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .SetSpace(5, 6)
      .SubLevel('g')
        .Add(TControlBuilder.Create(TPanel, G1).WithTop(10).WithLeft(20).WithWidthAndHeight(50, 30))
        .Add(TControlBuilder.Create(TPanel, G2).WithTop(10).WithLeft(80).WithWidthAndHeight(50, 60))
      .SuperLevel
      .SetTopLeftNearGroup('g', rpBelow)
      .Add(TControlBuilder.Create(TPanel, P))
    ;

    AssertEquals('Propriedade Left diferente da esperada', 20, P.Left);
    AssertEquals('Propriedade Top diferente da esperada', 70 + 5, P.Top);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestSetTopLeftNearGroupRight;
var
  ControlCreator: TControlCreator;
  G1, G2, P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .SetSpace(5, 6)
      .SubLevel('g')
        .Add(TControlBuilder.Create(TPanel, G1).WithTop(10).WithLeft(20).WithWidthAndHeight(50, 30))
        .Add(TControlBuilder.Create(TPanel, G2).WithTop(10).WithLeft(80).WithWidthAndHeight(50, 60))
      .SuperLevel
      .SetTopLeftNearGroup('g', rpRight)
      .Add(TControlBuilder.Create(TPanel, P))
    ;

    AssertEquals('Propriedade Top diferente da esperada', 10, P.Top);
    AssertEquals('Propriedade Left diferente da esperada', 130 + 6, P.Left);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestRecalcParentHeightOnly;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .SubLevel(TControlBuilder.Create(TPanel, P).WithWidth(999).WithHeight(10).WithLeft(15))
        .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(20, 25))
        .RecalcParentHeight
      .SuperLevel
    ;

    AssertEquals('Propriedade Height diferente da esperada', 25, P.Height);
    AssertEquals('Propriedade Width não deveria ser alterada por RecalcParentHeight', 999, P.Width);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestRecalcParentWidthOnly;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .SubLevel(TControlBuilder.Create(TPanel, P).WithHeight(999).WithWidth(10).WithLeft(15))
        .Add(TControlBuilder.Create(TPanel).WithWidthAndHeight(20, 25))
        .RecalcParentWidth
      .SuperLevel
    ;

    AssertEquals('Propriedade Width diferente da esperada', 20, P.Width);
    AssertEquals('Propriedade Height não deveria ser alterada por RecalcParentWidth', 999, P.Height);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestCenterControlsHorizontally;
var
  ControlCreator: TControlCreator;
  Ref, P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .Add(TControlBuilder.Create(TPanel, 'ref', Ref).WithTop(0).WithLeft(0).WithWidthAndHeight(100, 10))
      .Add(TControlBuilder.Create(TPanel, 'p', P).WithTop(50).WithLeft(0).WithWidthAndHeight(20, 10))
      .CenterControlsHorizontally(['p'], ['ref'])
    ;

    // Centro X do ref: 0 + 100/2 = 50. Left de P para centralizar: 50 - 20/2 = 40
    AssertEquals('Propriedade Left diferente da esperada', 40, P.Left);
    AssertEquals('Propriedade Top não deveria mudar', 50, P.Top);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestCenterControlsVertically;
var
  ControlCreator: TControlCreator;
  Ref, P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .Add(TControlBuilder.Create(TPanel, 'ref', Ref).WithTop(0).WithLeft(0).WithWidthAndHeight(10, 100))
      .Add(TControlBuilder.Create(TPanel, 'p', P).WithTop(0).WithLeft(50).WithWidthAndHeight(10, 20))
      .CenterControlsVertically(['p'], ['ref'])
    ;

    // Centro Y do ref: 0 + 100/2 = 50. Top de P para centralizar: 50 - 20/2 = 40
    AssertEquals('Propriedade Top diferente da esperada', 40, P.Top);
    AssertEquals('Propriedade Left não deveria mudar', 50, P.Left);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestCenterControlsInParentHorizontally;
var
  ControlCreator: TControlCreator;
  P: TPanel;
  ExpectedCenterX: Single;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .Add(TControlBuilder.Create(TPanel, 'p', P).WithTop(0).WithLeft(0).WithWidthAndHeight(20, 10))
      .CenterControlsInParentHorizontally(['p'])
    ;

    // Não fixamos o ClientWidth de FForm (depende do ambiente), então
    // comparamos contra o valor real em runtime em vez de um literal.
    ExpectedCenterX := FForm.ClientWidth / 2;
    AssertEquals('Centro horizontal de P diferente do esperado',
      ExpectedCenterX, P.Left + (P.Width / 2), 0.5);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestCenterControlsInParentVertically;
var
  ControlCreator: TControlCreator;
  P: TPanel;
  ExpectedCenterY: Single;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .Add(TControlBuilder.Create(TPanel, 'p', P).WithTop(0).WithLeft(0).WithWidthAndHeight(10, 20))
      .CenterControlsInParentVertically(['p'])
    ;

    ExpectedCenterY := FForm.ClientHeight / 2;
    AssertEquals('Centro vertical de P diferente do esperado',
      ExpectedCenterY, P.Top + (P.Height / 2), 0.5);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestCenterControlInParentHorizontally;
var
  ControlCreator: TControlCreator;
  P: TPanel;
  ExpectedCenterX: Single;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .Add(TControlBuilder.Create(TPanel, P).WithTop(0).WithLeft(0).WithWidthAndHeight(20, 10))
      .CenterControlInParentHorizontally
    ;

    ExpectedCenterX := FForm.ClientWidth / 2;
    AssertEquals('Centro horizontal de P diferente do esperado',
      ExpectedCenterX, P.Left + (P.Width / 2), 0.5);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestCopyHeightAppliesReferenceHeight;
var
  ControlCreator: TControlCreator;
  Ref, P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .Add(TControlBuilder.Create(TPanel, 'ref', Ref).WithWidthAndHeight(50, 77))
      .Add(TControlBuilder.Create(TPanel, 'p', P).WithWidthAndHeight(50, 10))
      .CopyHeight(['p'], ['ref'])
    ;

    AssertEquals('Propriedade Height diferente da esperada', 77, P.Height);
    AssertEquals('Propriedade Width não deveria mudar', 50, P.Width);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestCopyWidthAppliesReferenceWidth;
var
  ControlCreator: TControlCreator;
  Ref, P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .Add(TControlBuilder.Create(TPanel, 'ref', Ref).WithWidthAndHeight(88, 30))
      .Add(TControlBuilder.Create(TPanel, 'p', P).WithWidthAndHeight(10, 30))
      .CopyWidth(['p'], ['ref'])
    ;

    AssertEquals('Propriedade Width diferente da esperada', 88, P.Width);
    AssertEquals('Propriedade Height não deveria mudar', 30, P.Height);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestCopySizeAppliesReferenceWidthAndHeight;
var
  ControlCreator: TControlCreator;
  Ref, P: TPanel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .Add(TControlBuilder.Create(TPanel, 'ref', Ref).WithWidthAndHeight(88, 77))
      .Add(TControlBuilder.Create(TPanel, 'p', P).WithWidthAndHeight(10, 10))
      .CopySize(['p'], ['ref'])
    ;

    AssertEquals('Propriedade Width diferente da esperada', 88, P.Width);
    AssertEquals('Propriedade Height diferente da esperada', 77, P.Height);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestCopyHeightRaisesWhenControlNotFound;
var
  ControlCreator: TControlCreator;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .Add(TControlBuilder.Create(TPanel, 'ref').WithWidthAndHeight(50, 77))
    ;

    try
      ControlCreator.CopyHeight(['inexistente'], ['ref']);
      Fail('Deveria ter levantado exceção para controle inexistente');
    except
      on E: Exception do
        AssertTrue('Mensagem da exceção deveria mencionar o nome do controle',
          Pos('inexistente', E.Message) > 0);
    end;
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestCopyWidthRaisesWhenControlNotFound;
var
  ControlCreator: TControlCreator;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .Add(TControlBuilder.Create(TPanel, 'ref').WithWidthAndHeight(50, 77))
    ;

    try
      ControlCreator.CopyWidth(['inexistente'], ['ref']);
      Fail('Deveria ter levantado exceção para controle inexistente');
    except
      on E: Exception do
        AssertTrue('Mensagem da exceção deveria mencionar o nome do controle',
          Pos('inexistente', E.Message) > 0);
    end;
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestCopySizeRaisesWhenControlNotFound;
var
  ControlCreator: TControlCreator;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .Add(TControlBuilder.Create(TPanel, 'ref').WithWidthAndHeight(50, 77))
    ;

    try
      ControlCreator.CopySize(['inexistente'], ['ref']);
      Fail('Deveria ter levantado exceção para controle inexistente');
    except
      on E: Exception do
        AssertTrue('Mensagem da exceção deveria mencionar o nome do controle',
          Pos('inexistente', E.Message) > 0);
    end;
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestReturnCurrentLevel;
var
  ControlCreator: TControlCreator;
  Level: TControlCreatorLevel;
begin
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .SubLevel('g')
      .ReturnCurrentLevel(Level)
    ;

    AssertSame('Level devolvido deveria ser o mesmo objeto do CurrentLevel',
      ControlCreator.CurrentLevel, Level);
    AssertEquals('GroupName do level devolvido diferente do esperado', 'g', Level.GroupName);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestSetOwnerAndParentDirectly;
var
  ControlCreator: TControlCreator;
  P: TPanel;
begin
  // Regressão: usa SetOwnerAndParent diretamente (não o WithOwnerAndParent
  // deprecated).
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .Add(TControlBuilder.Create(TPanel, P))
    ;

    AssertSame('Owner do controle diferente do esperado', FForm, P.Owner);
    AssertSame('Parent do controle diferente do esperado', FForm, P.Parent);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestSetParentDirectly;
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

    AssertSame('Parent do controle diferente do esperado', FForm, P.Parent);
  finally
    // SetParent (diferente de SetOwnerAndParent) não define Owner, então P
    // não é liberado automaticamente pelo FForm.Free do TearDown.
    P.Free;
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestGetControlGeneric;
var
  ControlCreator: TControlCreator;
begin
  // Regressão: GetControl<T> chamava Registry.GetControl<T> internamente
  // sem "specialize", o que travava o compilador FPC (erro interno
  // 2015071704) assim que o método genérico era de fato instanciado -
  // nunca havia sido exercitado por nenhum teste antes.
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .Add(TControlBuilder.Create(TPanel, 'painel_generico'))
    ;

    AssertNotNull('GetControl<T> não deveria devolver nil',
      ControlCreator.specialize GetControl<TPanel>('painel_generico'));
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestAddControlBuilderIsFreedNotLeaked;
var
  ControlCreator: TControlCreator;
begin
  // Simetria com TestAddMenuBuilderIsFreedNotLeaked (umenucreatortests.pas):
  // o Creator assume posse do builder em Add e deve liberá-lo exatamente
  // uma vez.
  GControlBuilderDestroyCount := 0;
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .Add(TCountingControlBuilder.Create(TPanel))
    ;

    AssertEquals('O builder do controle deveria ter sido liberado exatamente uma vez', 1, GControlBuilderDestroyCount);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestAddControlBuilderExceptionDuringBuildStillFreesBuilder;
var
  ControlCreator: TControlCreator;
  Raised: Boolean;
begin
  // Vazamento-em-exceção é uma classe recorrente de bug nesta lib (ver
  // memória feedback_opcb_leak_on_exception) - o builder precisa ser
  // liberado mesmo quando WithProp aponta pra uma propriedade inexistente
  // e Build lança EPropertyError, não só no caminho feliz.
  GControlBuilderDestroyCount := 0;
  Raised := False;
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator.WithOwnerAndParent(FForm, FForm);

    try
      ControlCreator.Add(TCountingControlBuilder.Create(TPanel).WithProp('FooBar', 123));
    except
      on E: EPropertyError do
        Raised := True;
    end;

    AssertTrue('Deveria ter lançado EPropertyError para propriedade inexistente', Raised);
    AssertEquals('O builder do controle deveria ter sido liberado mesmo com Build lançando exceção', 1, GControlBuilderDestroyCount);
  finally
    ControlCreator.Free;
  end;
end;

procedure TControlCreatorTests.TestAddControlBuilderExceptionDuringBuildNullsOutReference;
var
  ControlCreator: TControlCreator;
  Dummy: TPanel;
  P: TPanel;
  Raised: Boolean;
begin
  // DiscardReferences já era exercitado indiretamente pelo caminho de grid
  // cheio (TestAddWithGridFullDiscardsOutReference), mas o OUTRO ponto que
  // a chama - dentro do except de TObjectBuilderBase.Build, quando
  // CreateObject já criou o objeto mas ApplyPendingProps falha - ainda não
  // tinha teste direto verificando que a referência externa fica nil em vez
  // de apontar pro objeto já liberado.
  ControlCreator := TControlCreator.Create;
  Dummy := TPanel.Create(nil);
  Raised := False;
  try
    P := Dummy; // valor não-nil conhecido, simulando "lixo" de uma referência anterior
    ControlCreator.WithOwnerAndParent(FForm, FForm);

    try
      ControlCreator.Add(TControlBuilder.Create(TPanel, P).WithProp('FooBar', 123));
    except
      on E: EPropertyError do
        Raised := True;
    end;

    AssertTrue('Deveria ter lançado EPropertyError para propriedade inexistente', Raised);
    AssertNull('P deveria ser nil (Build falhou depois de criar o objeto, referência não pode ficar pendurada)', P);
  finally
    Dummy.Free;
    ControlCreator.Free;
  end;
end;

initialization
  RegisterTest(TControlCreatorTests);

end.

