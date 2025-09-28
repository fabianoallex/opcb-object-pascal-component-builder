unit UControlCreatorTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, Forms, OPCB;

type

  { TControlCreatorTests }

  TControlCreatorTests = class(TTestCase)
  private
    procedure ExternalMethod(const ABuiler: TControlCreator);
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
    procedure TestAddControlToRegistry;                // testa se um conrole ao ser criado é inserido no registro
    procedure TestRemoveControlFromRegistryOnDestroy;  // testa se um controle quando destruído será removido do registro
    procedure TestSubLevel;
    procedure TestSubLevelSubLevel;    // mais de um nivel de profunidade
    procedure TestSubLevelSuperLevel;  // testa se volta ao nivel inicial
    procedure TestSubLevelEmpty;       // testa sublevel vazio. deve permitir sem mover top/left.
    procedure TestSubLevelDirectionSuperLevelDirection;
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
  end;

implementation

uses
  ExtCtrls;

procedure TControlCreatorTests.ExternalMethod(const ABuiler: TControlCreator);
begin
  ABuiler.AddControl(TControlBuilder.Create(TPanel, 'PanelTest').WithCaption('EXTERNAL-TEST'));
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
  ci: TControlBuilder;
begin
  ControlCreator := TControlCreator.Create;
  try
    with ControlCreator do
    begin
      WithOwnerAndParent(FForm, FForm);
      SetTopLeft(10, 20);

      ci := TControlBuilder.Create(TPanel, P);

      AddControl(
        ci
      );
    end;
     {
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .SetTopLeft(10, 20)
      .AddControl(TControlBuilder.Create(TPanel, P))
    ;
    }

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
      .AddControl(TControlBuilder.Create(TPanel).WithWidth(15).WithHeight(15))
      .AddControl(TControlBuilder.Create(TPanel, P))
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
      .AddControl(TControlBuilder.Create(TPanel).WithWidth(15).WithHeight(15))
      .AddControl(TControlBuilder.Create(TPanel, P))
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
      .AddControl(TControlBuilder.Create(TPanel, P))
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
      .AddControl(TControlBuilder.Create(TPanel, P))
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
      .AddControl(TControlBuilder.Create(TPanel).WithWidthAndHeight(15, 10))
      .AddControl(TControlBuilder.Create(TPanel).WithWidthAndHeight(15, 10))
      .Break
      .AddControl(TControlBuilder.Create(TPanel, P))
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
      .AddControl(TControlBuilder.Create(TPanel).WithWidthAndHeight(15, 10))
      .AddControl(TControlBuilder.Create(TPanel).WithWidthAndHeight(15, 10))
      .Break
      .AddControl(TControlBuilder.Create(TPanel, P))
    ;
    AssertEquals('Propriedade Top diferente da esperada', 0, P.Top);
    AssertEquals('Propriedade Left diferente da esperada', 15, P.Left);
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
      .AddControl(TControlBuilder.Create(TPanel).WithWidthAndHeight(15, 10))
      .AddControl(TControlBuilder.Create(TPanel).WithWidthAndHeight(15, 10))
      .SetTopLeft(40, 30)  // muda o local de referencia do break
      .AddControl(TControlBuilder.Create(TPanel).WithWidthAndHeight(5, 5))
      .AddControl(TControlBuilder.Create(TPanel).WithWidthAndHeight(5, 5))
      .Break
      .AddControl(TControlBuilder.Create(TPanel, P))
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
      .AddControl(TControlBuilder.Create(TPanel).WithHeight(15))
      .AddControl(TControlBuilder.Create(TPanel).WithHeight(25)) // maior incluido primeiro
      .Break
      .AddControl(TControlBuilder.Create(TPanel, P1).WithHeight(5))
    ;
    AssertEquals('Propriedade Top diferente da esperada para P1', 25, P1.Top);

    ControlCreator
      .AddControl(TControlBuilder.Create(TPanel).WithHeight(25)) // maior incluido segundo
      .AddControl(TControlBuilder.Create(TPanel).WithHeight(15))
      .Break
      .AddControl(TControlBuilder.Create(TPanel, P2))
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
      .AddControl(TControlBuilder.Create(TPanel).WithWidth(15))
      .AddControl(TControlBuilder.Create(TPanel).WithWidth(25)) // maior incluido primeiro
      .Break
      .AddControl(TControlBuilder.Create(TPanel, P1).WithWidth(5))
    ;
    AssertEquals('Propriedade Left diferente da esperada para P1', 25, P1.Left);

    ControlCreator
      .AddControl(TControlBuilder.Create(TPanel).WithWidth(25)) // maior incluido segundo
      .AddControl(TControlBuilder.Create(TPanel).WithWidth(15))
      .Break
      .AddControl(TControlBuilder.Create(TPanel, P2))
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
      .AddControl(TControlBuilder.Create(TPanel).WithWidthAndHeight(15, 10))
      .AddControl(TControlBuilder.Create(TPanel, P))
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
      .AddControl(TControlBuilder.Create(TPanel).WithWidthAndHeight(15, 10))
      .AddControl(TControlBuilder.Create(TPanel, P))
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
      .AddControl(TControlBuilder.Create(TPanel, P1).WithHeight(44)) // tem que sobrepor com 33, mesmo passando 44
      .AddControl(TControlBuilder.Create(TPanel, P2))
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
      .AddControl(TControlBuilder.Create(TPanel).WithHeight(44))
      .AddControl(TControlBuilder.Create(TPanel))
      .UnsetControlHeight
      .AddControl(TControlBuilder.Create(TPanel, P1).WithHeight(44)) // agora tem que ser considerado os 44
      .AddControl(TControlBuilder.Create(TPanel, P2))
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
      .AddControl(TControlBuilder.Create(TPanel, P1).WithWidth(44)) // tem que sobrepor com 33, mesmo passando 44
      .AddControl(TControlBuilder.Create(TPanel, P2))
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
      .AddControl(TControlBuilder.Create(TPanel).WithWidth(44))
      .AddControl(TControlBuilder.Create(TPanel))
      .UnsetControlWidth
      .AddControl(TControlBuilder.Create(TPanel, P1).WithWidth(44)) // agora tem que ser considerado os 44
      .AddControl(TControlBuilder.Create(TPanel, P2))
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

procedure TControlCreatorTests.TestAddControlToRegistry;
var
  ControlCreator: TControlCreator;
begin
  ControlCreator := TControlCreator.Create;
  try
    AssertEquals('A quantidade de controles no registro diferente do esperado',
      0, ControlCreator.Controls.Count);

    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .AddControl(TControlBuilder.Create(TPanel))
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
      .AddControl(TControlBuilder.Create(TPanel, 'panel_test'))
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
      .AddControl(TControlBuilder.Create(TPanel).WithHeight(50).WithLeft(60))
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
      .AddControl(TControlBuilder.Create(TPanel).WithHeight(50).WithLeft(60))
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
      .AddControl(TControlBuilder.Create(TPanel, P));

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
      .AddControl(TControlBuilder.Create(TPanel))          // Width 80 (A)
      .SubLevel
        .SetControlWidth(80)
        .SetControlHeight(50)
        .SetDirection(cpdVertical)
        .AddControl(TControlBuilder.Create(TPanel))       // Width 80 (B)
        .AddControl(TControlBuilder.Create(TPanel, P1))
    ;

    SubLevelDirection := ControlCreator.CurrentLevel.Direction;

    ControlCreator
      .SuperLevel
      .AddControl(TControlBuilder.Create(TPanel, P2))    // <-- left 160 (80A + 80B)
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
        .AddControl(TControlBuilder.Create(TPanel).WithWidthAndHeight(20, 25))
        .AddControl(TControlBuilder.Create(TPanel).WithWidthAndHeight(20, 25))
        .AddControl(TControlBuilder.Create(TPanel).WithWidthAndHeight(20, 25))
        .AddControl(TControlBuilder.Create(TPanel).WithWidthAndHeight(20, 25))
        .AddControl(TControlBuilder.Create(TPanel).WithWidthAndHeight(20, 25))
        .Break
        .AddControl(TControlBuilder.Create(TPanel).WithWidthAndHeight(20, 25))
        .AddControl(TControlBuilder.Create(TPanel).WithWidthAndHeight(20, 25))
        .AddControl(TControlBuilder.Create(TPanel).WithWidthAndHeight(20, 25))
        .AddControl(TControlBuilder.Create(TPanel).WithWidthAndHeight(20, 25))
        .AddControl(TControlBuilder.Create(TPanel).WithWidthAndHeight(20, 25))
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
        .AddControl(TControlBuilder.Create(TPanel).WithWidthAndHeight(20, 25))
        .AddControl(TControlBuilder.Create(TPanel).WithWidthAndHeight(20, 25))
        .AddControl(TControlBuilder.Create(TPanel).WithWidthAndHeight(20, 25))
        .AddControl(TControlBuilder.Create(TPanel).WithWidthAndHeight(20, 25))
        .AddControl(TControlBuilder.Create(TPanel).WithWidthAndHeight(20, 25))
        .Break
        .AddControl(TControlBuilder.Create(TPanel).WithWidthAndHeight(20, 25))
        .AddControl(TControlBuilder.Create(TPanel).WithWidthAndHeight(20, 25))
        .AddControl(TControlBuilder.Create(TPanel).WithWidthAndHeight(20, 25))
        .AddControl(TControlBuilder.Create(TPanel).WithWidthAndHeight(20, 25))
        .AddControl(TControlBuilder.Create(TPanel).WithWidthAndHeight(20, 25))
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
        .AddControl(TControlBuilder.Create(TPanel, P1))
        .AddControl(TControlBuilder.Create(TPanel, P2))
        .AddControl(TControlBuilder.Create(TPanel, P3))
        .AddControl(TControlBuilder.Create(TPanel, P4))
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
        .AddControl(TControlBuilder.Create(TPanel, P1))
        .AddControl(TControlBuilder.Create(TPanel, P2))
        .AddControl(TControlBuilder.Create(TPanel, P3))
        .AddControl(TControlBuilder.Create(TPanel, P4))
        .AddControl(TControlBuilder.Create(TPanel, P5))  // deve ignorar pois já finalizou o grid
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
        .AddControl(TControlBuilder.Create(TPanel, P1))
        .AddControl(TControlBuilder.Create(TPanel, P2))
        .AddControl(TControlBuilder.Create(TPanel, P3))
        .AddControl(TControlBuilder.Create(TPanel, P4))
        .AddControl(TControlBuilder.Create(TPanel, P5)) // ao adicionar esse registro grid se expande
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
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .GridReturnNumberOfRows(RowsBeforeExpand)
        .AddControl(TControlBuilder.Create(TPanel)) // ao adicionar esse registro grid se expande
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
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .GridReturnNumberOfCols(ColsBeforeExpand)
        .AddControl(TControlBuilder.Create(TPanel)) // ao adicionar esse registro grid se expande
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
        .AddControl(TControlBuilder.Create(TPanel).WithCaption('1'))
        .AddControl(TControlBuilder.Create(TPanel).WithCaption('2'))
        .AddControl(TControlBuilder.Create(TPanel).WithCaption('3'))
        .AddControl(TControlBuilder.Create(TPanel).WithCaption('4'))

        .GridReturnNumberOfRows(RowsBeforeExpand)
        .AddControl(TControlBuilder.Create(TPanel).WithCaption('5'))  // aqui expande automaticamente linhas para 3
        .GridReturnNumberOfRows(RowsAfterExpand)

        .SetDirection(cpdVertical) // ao mudar pra direção vertical, apos preencher a ultima celula da coluna, tentará criar uma nova coluna

        .AddControl(TControlBuilder.Create(TPanel).WithCaption('6'))
        .GridReturnNumberOfCols(ColsBeforeExpand)
        .AddControl(TControlBuilder.Create(TPanel, P).WithCaption('7'))  // aqui ignora pois não expande colunas
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
        .AddControl(TControlBuilder.Create(TPanel).WithCaption('1'))
        .AddControl(TControlBuilder.Create(TPanel).WithCaption('2'))
        .AddControl(TControlBuilder.Create(TPanel).WithCaption('3'))
        .AddControl(TControlBuilder.Create(TPanel).WithCaption('4'))

        .GridReturnNumberOfCols(ColsBeforeExpand)
        .AddControl(TControlBuilder.Create(TPanel).WithCaption('5'))  // aqui expande automaticamente Colunas para 3
        .GridReturnNumberOfCols(ColsAfterExpand)

        .SetDirection(cpdHorizontal) // ao mudar pra direção vertical, apos preencher a ultima celula da linha, tentará criar uma nova linha

        .AddControl(TControlBuilder.Create(TPanel).WithCaption('6'))
        .GridReturnNumberOfRows(RowsBeforeExpand)
        .AddControl(TControlBuilder.Create(TPanel, P).WithCaption('7'))  // aqui ignora pois não expande colunas
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
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        // sublevel na segunda linha e na segunda coluna
        .SubLevel
          // P1 e P2 estão em um sublevel dentro de uma celula, mas eles
          // não devem se comportar como conteudo da celula
          .AddControl(TControlBuilder.Create(TPanel, P1).WithWidthAndHeight(6, 7))
          .AddControl(TControlBuilder.Create(TPanel, P2))
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

        .AddControl(TControlBuilder.Create(TPanel))
        .GridSkipCell
        .AddControl(TControlBuilder.Create(TPanel, P))
      .GridFinish
    ;

    AssertEquals('Propriedade Left diferente do esperado', 10 + 10, P.Left);
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

        .AddControl(TControlBuilder.Create(TPanel, P1))  // p1 esta na linha 0
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel, P2)) // p2 esta na linha 2
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

        .AddControl(TControlBuilder.Create(TPanel, P1))  // p1 esta na coluna 0
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel, P2)) // p2 esta na coluna 2
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

        .AddControl(TControlBuilder.Create(TPanel, P1)) // sem rowspan
        .GridRowSpan(2)
        .AddControl(TControlBuilder.Create(TPanel, P2)) // com rowspan=2
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
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
        .AddControl(TControlBuilder.Create(TPanel, P1)) // sem colspan
        .GridColSpan(2)
        .AddControl(TControlBuilder.Create(TPanel, P2)) // com colspan=2
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
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

        .AddControl(TControlBuilder.Create(TPanel, P1)) // sem rowspan
        .GridRowSpan(2)
        .AddControl(TControlBuilder.Create(TPanel, P2)) // com rowspan=2
        .AddControl(TControlBuilder.Create(TPanel, P3)) // sem rowspan automaticamente
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
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
        .AddControl(TControlBuilder.Create(TPanel, P1)) // sem colspan
        .GridColSpan(2)
        .AddControl(TControlBuilder.Create(TPanel, P2)) // com colspan=2
        .AddControl(TControlBuilder.Create(TPanel, P3)) // sem colspan automaticamente
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
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

        .AddControl(TControlBuilder.Create(TPanel)) // sem rowspan

        // span 3 irá ultrapassar limite da ultima linha do grid.
        // espera-se que limite a 2
        .GridRowSpan(3)

        .AddControl(TControlBuilder.Create(TPanel, P)) // com rowspan=2
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
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

        .AddControl(TControlBuilder.Create(TPanel))

        // span 3 irá ultrapassar limite da ultima coluna do grid.
        // espera-se que limite a 2
        .GridColSpan(3)

        .AddControl(TControlBuilder.Create(TPanel, P)) // com colspan=2
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
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

        .AddControl(TControlBuilder.Create(TPanel)) // sem rowspan

        // span 3 irá ultrapassar limite da ultima linha do grid.
        // espera-se que expanda o grid para 4 linhas
        .GridRowSpan(3)

        .AddControl(TControlBuilder.Create(TPanel, P)) // com rowspan=3
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
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

        .AddControl(TControlBuilder.Create(TPanel))

        // span 3 irá ultrapassar limite da ultima coluna do grid.
        // espera-se que expanda as colunas do grid
        .GridColSpan(3)

        .AddControl(TControlBuilder.Create(TPanel, P)) // com colspan=3
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
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
        .AddControl(TControlBuilder.Create(TPanel, P))
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

        .AddControl(TControlBuilder.Create(TPanel))
        .GridRowSpan(2)
        .AddControl(TControlBuilder.Create(TPanel, P)) // com rowspan=2
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
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
        .AddControl(TControlBuilder.Create(TPanel))
        .GridColSpan(2)
        .AddControl(TControlBuilder.Create(TPanel, P)) // com colspan=2
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
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
        .AddControl(TControlBuilder.Create(TPanel, P).WithWidthAndHeight(30, 25))
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
        .AddControl(TControlBuilder.Create(TPanel, P).WithWidthAndHeight(30, 25))
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
        .AddControl(TControlBuilder.Create(TPanel, P).WithWidthAndHeight(30, 25))   // 30 e 25: esses valores sao ignorados
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
        .AddControl(TControlBuilder.Create(TPanel, P).WithWidthAndHeight(30, 25))
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
        .AddControl(TControlBuilder.Create(TPanel, P).WithWidthAndHeight(10, 14))
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
        .AddControl(TControlBuilder.Create(TPanel, P).WithWidthAndHeight(10, 14))
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
        .AddControl(TControlBuilder.Create(TPanel, P).WithWidthAndHeight(10, 14))
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
        .AddControl(TControlBuilder.Create(TPanel, P).WithWidthAndHeight(10, 14))
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
        .AddControl(TControlBuilder.Create(TPanel, P).WithWidthAndHeight(10, 14))
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
        .AddControl(TControlBuilder.Create(TPanel, P).WithWidthAndHeight(10, 14))
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
        .AddControl(TControlBuilder.Create(TPanel, P).WithWidthAndHeight(10, 14))
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
        .AddControl(TControlBuilder.Create(TPanel, P).WithWidthAndHeight(10, 14))
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
        .AddControl(TControlBuilder.Create(TPanel, P).WithWidthAndHeight(10, 14))
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
        .AddControl(TControlBuilder.Create(TPanel, P1))  // linha 1
        .AddControl(TControlBuilder.Create(TPanel, P2))  // linha 1
        .AddControl(TControlBuilder.Create(TPanel, P3))  // linha 1

        .AddControl(TControlBuilder.Create(TPanel, P4))  // linha 2
        .AddControl(TControlBuilder.Create(TPanel, P5))  // linha 2
        .AddControl(TControlBuilder.Create(TPanel, P6))  // linha 2

        .AddControl(TControlBuilder.Create(TPanel, P7))  // linha 3
        .AddControl(TControlBuilder.Create(TPanel, P8))  // linha 3
        .AddControl(TControlBuilder.Create(TPanel, P9))  // linha 3
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
        .AddControl(TControlBuilder.Create(TPanel, P1))  // coluna 1
        .AddControl(TControlBuilder.Create(TPanel, P2))  // coluna 1
        .AddControl(TControlBuilder.Create(TPanel, P3))  // coluna 1

        .AddControl(TControlBuilder.Create(TPanel, P4))  // coluna 2
        .AddControl(TControlBuilder.Create(TPanel, P5))  // coluna 2
        .AddControl(TControlBuilder.Create(TPanel, P6))  // coluna 2

        .AddControl(TControlBuilder.Create(TPanel, P7))  // coluna 3
        .AddControl(TControlBuilder.Create(TPanel, P8))  // coluna 3
        .AddControl(TControlBuilder.Create(TPanel, P9))  // coluna 3
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
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .BreakLine
        .AddControl(TControlBuilder.Create(TPanel, P)) // segunda linha, primeira coluna
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
        .AddControl(TControlBuilder.Create(TPanel))
        .AddControl(TControlBuilder.Create(TPanel))
        .BreakColumn
        .AddControl(TControlBuilder.Create(TPanel, P)) // segunda coluna, primeira linha
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
        .AddControl(TControlBuilder.Create(TPanel, P))
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
        .AddControl(TControlBuilder.Create(TPanel, P))
      .GridFinish
    ;

    AssertNotNull('P não deveria ser null', P);
  finally
    ControlCreator.Free;
  end;
end;

initialization
  RegisterTest(TControlCreatorTests);

end.

