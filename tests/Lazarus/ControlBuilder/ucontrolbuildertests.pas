unit UControlBuilderTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, Forms, OPCB;

type

  { TControlBuilderTests }

  TControlBuilderTests = class(TTestCase)
  private
    procedure ExternalMethod(const ABuiler: TControlBuilder);
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

    procedure TestGridMode;
    procedure TestGridModeIgnoreAddAfterEndOfGrid;
    procedure TestGridModeNotIgnoreAddAfterEndOfGridWhenAutoExpand;
    procedure TestGridModeAutoExpandRows;
    procedure TestGridModeAutoExpandCols;
    procedure TestGridModeAutoExpandOnlyRows;
    procedure TestGridModeAutoExpandOnlyCols;
    procedure TestGridSkipCell;
    procedure TestGridRowHeight;
    procedure TestGridColWidth;
    procedure TestGridRowSpan;
    procedure TestGridColSpan;
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
  end;

implementation

uses
  ExtCtrls;

procedure TControlBuilderTests.ExternalMethod(const ABuiler: TControlBuilder);
begin
  ABuiler.AddControl(TControlInfo.Create(TPanel, 'PanelTest').WithCaption('EXTERNAL-TEST'));
end;

procedure TControlBuilderTests.SetUp;
begin
  FForm := TForm.Create(nil);
end;

procedure TControlBuilderTests.TearDown;
begin
  FForm.Free;
end;

procedure TControlBuilderTests.TestTopLeft;
var
  ControlBuilder: TControlBuilder;
  P: TPanel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .SetTopLeft(10, 20)
      .AddControl(TControlInfo.Create(TPanel, P))
    ;

    AssertEquals('Propriedade Top diferente da esperada', 10, P.Top);
    AssertEquals('Propriedade Left diferente da esperada', 20, P.Left);
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestTopLeftHorizontally;
var
  ControlBuilder: TControlBuilder;
  P: TPanel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .SetTopLeft(10, 20)
      .SetDirection(cpdHorizontal)
      .AddControl(TControlInfo.Create(TPanel).WithWidth(15).WithHeight(15))
      .AddControl(TControlInfo.Create(TPanel, P))
    ;
    AssertEquals('Propriedade Top diferente da esperada', 10 + 0, P.Top);
    AssertEquals('Propriedade Left diferente da esperada', 20 + 15, P.Left);
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestTopLeftVertically;
var
  ControlBuilder: TControlBuilder;
  P: TPanel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .SetTopLeft(10, 20)
      .SetDirection(cpdVertical)
      .AddControl(TControlInfo.Create(TPanel).WithWidth(15).WithHeight(15))
      .AddControl(TControlInfo.Create(TPanel, P))
    ;
    AssertEquals('Propriedade Top diferente da esperada', 10 + 15, P.Top);
    AssertEquals('Propriedade Left diferente da esperada', 20 + 0, P.Left);
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestIncTop;
var
  ControlBuilder: TControlBuilder;
  P: TPanel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .SetTopLeft(10, 20)
      .IncTop(13)
      .AddControl(TControlInfo.Create(TPanel, P))
    ;

    AssertEquals('Propriedade Top diferente da esperada', 10 + 13, P.Top);
    AssertEquals('Propriedade Left diferente da esperada', 20, P.Left);
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestIncLeft;
var
  ControlBuilder: TControlBuilder;
  P: TPanel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .SetTopLeft(10, 20)
      .IncLeft(17)
      .AddControl(TControlInfo.Create(TPanel, P))
    ;

    AssertEquals('Propriedade Top diferente da esperada', 10, P.Top);
    AssertEquals('Propriedade Left diferente da esperada', 20 + 17, P.Left);
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestBreakHorizontally;
var
  ControlBuilder: TControlBuilder;
  P: TPanel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .AddControl(TControlInfo.Create(TPanel).WithWidthAndHeight(15, 10))
      .AddControl(TControlInfo.Create(TPanel).WithWidthAndHeight(15, 10))
      .Break
      .AddControl(TControlInfo.Create(TPanel, P))
    ;
    AssertEquals('Propriedade Top diferente da esperada', 10, P.Top);
    AssertEquals('Propriedade Left diferente da esperada', 0, P.Left);
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestBreakVertically;
var
  ControlBuilder: TControlBuilder;
  P: TPanel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdVertical)
      .AddControl(TControlInfo.Create(TPanel).WithWidthAndHeight(15, 10))
      .AddControl(TControlInfo.Create(TPanel).WithWidthAndHeight(15, 10))
      .Break
      .AddControl(TControlInfo.Create(TPanel, P))
    ;
    AssertEquals('Propriedade Top diferente da esperada', 0, P.Top);
    AssertEquals('Propriedade Left diferente da esperada', 15, P.Left);
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestBreakAfterTopLeft;
var
  ControlBuilder: TControlBuilder;
  P: TPanel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .AddControl(TControlInfo.Create(TPanel).WithWidthAndHeight(15, 10))
      .AddControl(TControlInfo.Create(TPanel).WithWidthAndHeight(15, 10))
      .SetTopLeft(40, 30)  // muda o local de referencia do break
      .AddControl(TControlInfo.Create(TPanel).WithWidthAndHeight(5, 5))
      .AddControl(TControlInfo.Create(TPanel).WithWidthAndHeight(5, 5))
      .Break
      .AddControl(TControlInfo.Create(TPanel, P))
    ;
    AssertEquals('Propriedade Top diferente da esperada', 40 + 5, P.Top);
    AssertEquals('Propriedade Left diferente da esperada', 30, P.Left);
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestTopAfterDiferentControlsHightsAndBreak;
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
  ControlBuilder: TControlBuilder;
  P1, P2: TPanel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .AddControl(TControlInfo.Create(TPanel).WithHeight(15))
      .AddControl(TControlInfo.Create(TPanel).WithHeight(25)) // maior incluido primeiro
      .Break
      .AddControl(TControlInfo.Create(TPanel, P1).WithHeight(5))
    ;
    AssertEquals('Propriedade Top diferente da esperada para P1', 25, P1.Top);

    ControlBuilder
      .AddControl(TControlInfo.Create(TPanel).WithHeight(25)) // maior incluido segundo
      .AddControl(TControlInfo.Create(TPanel).WithHeight(15))
      .Break
      .AddControl(TControlInfo.Create(TPanel, P2))
    ;
    AssertEquals('Propriedade Top diferente da esperada para P2', 25 + 25, P2.Top);
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestLeftAfterDiferentControlsWidthsAndBreak;
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
  ControlBuilder: TControlBuilder;
  P1, P2: TPanel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdVertical)
      .AddControl(TControlInfo.Create(TPanel).WithWidth(15))
      .AddControl(TControlInfo.Create(TPanel).WithWidth(25)) // maior incluido primeiro
      .Break
      .AddControl(TControlInfo.Create(TPanel, P1).WithWidth(5))
    ;
    AssertEquals('Propriedade Left diferente da esperada para P1', 25, P1.Left);

    ControlBuilder
      .AddControl(TControlInfo.Create(TPanel).WithWidth(25)) // maior incluido segundo
      .AddControl(TControlInfo.Create(TPanel).WithWidth(15))
      .Break
      .AddControl(TControlInfo.Create(TPanel, P2))
    ;
    AssertEquals('Propriedade Left diferente da esperada  para P2', 25 + 25, P2.Left);
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestSpaceHorizontally;
var
  ControlBuilder: TControlBuilder;
  P: TPanel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .SetSpace(7, 8)
      .AddControl(TControlInfo.Create(TPanel).WithWidthAndHeight(15, 10))
      .AddControl(TControlInfo.Create(TPanel, P))
    ;
    AssertEquals('Propriedade Top diferente da esperada', 0, P.Top);
    AssertEquals('Propriedade Left diferente da esperada', 15 + 8, P.Left);
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestSpaceVertically;
var
  ControlBuilder: TControlBuilder;
  P: TPanel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdVertical)
      .SetSpace(7, 8)
      .AddControl(TControlInfo.Create(TPanel).WithWidthAndHeight(15, 10))
      .AddControl(TControlInfo.Create(TPanel, P))
    ;
    AssertEquals('Propriedade Top diferente da esperada', 10 + 7, P.Top);
    AssertEquals('Propriedade Left diferente da esperada', 0, P.Left);
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestSetControlHeight;
var
  ControlBuilder: TControlBuilder;
  P1, P2: TPanel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .SetTopLeft(10, 20)
      .SetControlHeight(33)
      .AddControl(TControlInfo.Create(TPanel, P1).WithHeight(44)) // tem que sobrepor com 33, mesmo passando 44
      .AddControl(TControlInfo.Create(TPanel, P2))
    ;

    AssertEquals('Propriedade Height de P1 diferente da esperada', 33, P1.Height);
    AssertEquals('Propriedade Height de P2 diferente da esperada', 33, P2.Height);
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestUnsetControlHeight;
var
  ControlBuilder: TControlBuilder;
  P1, P2: TPanel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .SetTopLeft(10, 20)
      .SetControlHeight(33)
      .AddControl(TControlInfo.Create(TPanel).WithHeight(44))
      .AddControl(TControlInfo.Create(TPanel))
      .UnsetControlHeight
      .AddControl(TControlInfo.Create(TPanel, P1).WithHeight(44)) // agora tem que ser considerado os 44
      .AddControl(TControlInfo.Create(TPanel, P2))
    ;

    AssertEquals('Propriedade Height de P1 diferente da esperada', 44, P1.Height);
    AssertEquals('Propriedade Height de P2 diferente da esperada', 50, P2.Height);  // 50: default panel height
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestSetControlWidth;
var
  ControlBuilder: TControlBuilder;
  P1, P2: TPanel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .SetTopLeft(10, 20)
      .SetControlWidth(33)
      .AddControl(TControlInfo.Create(TPanel, P1).WithWidth(44)) // tem que sobrepor com 33, mesmo passando 44
      .AddControl(TControlInfo.Create(TPanel, P2))
    ;

    AssertEquals('Propriedade Width de P1 diferente da esperada', 33, P1.Width);
    AssertEquals('Propriedade Width de P2 diferente da esperada', 33, P2.Width);
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestUnsetControlWidth;
var
  ControlBuilder: TControlBuilder;
  P1, P2: TPanel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .SetTopLeft(10, 20)
      .SetControlWidth(33)
      .AddControl(TControlInfo.Create(TPanel).WithWidth(44))
      .AddControl(TControlInfo.Create(TPanel))
      .UnsetControlWidth
      .AddControl(TControlInfo.Create(TPanel, P1).WithWidth(44)) // agora tem que ser considerado os 44
      .AddControl(TControlInfo.Create(TPanel, P2))
    ;

    AssertEquals('Propriedade Width de P1 diferente da esperada', 44, P1.Width);
    AssertEquals('Propriedade Width de P2 diferente da esperada', 170, P2.Width);  // 170: default panel width
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestExternal;
var
  ControlBuilder: TControlBuilder;
  P: TPanel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    P := nil;

    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .External(@ExternalMethod)             // Irá incluir um Panel com Caption 'EXTERNAL-TEST'
    ;

    P := ControlBuilder.GetControl('PanelTest') as TPanel;

    AssertNotNull('Variável P não deveria ser nil', P);
    AssertEquals('Propreiedade Caption diferente da esperada', 'EXTERNAL-TEST', P.Caption);
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestAddControlToRegistry;
var
  ControlBuilder: TControlBuilder;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    AssertEquals('A quantidade de controles no registro diferente do esperado',
      0, ControlBuilder.Controls.Count);

    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .AddControl(TControlInfo.Create(TPanel))
    ;

    AssertEquals('A quantidade de controles no registro diferente do esperado',
      1, ControlBuilder.Controls.Count);
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestRemoveControlFromRegistryOnDestroy;
var
  ControlBuilder: TControlBuilder;
  P: TPanel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    AssertEquals('A quantidade de controles no registro diferente do esperado',
      0, ControlBuilder.Controls.Count);

    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .AddControl(TControlInfo.Create(TPanel, 'panel_test'))
    ;

    P := ControlBuilder.GetControl('panel_test') as TPanel;
    AssertEquals('A quantidade de controles no registro diferente do esperado',
      1, ControlBuilder.Controls.Count);

    // ao destruir o objeto, automaticamente o objeto deve ser removido do
    // registro interno de controles do builder

    P.Free;

    AssertEquals('A quantidade de controles no registro diferente do esperado',
      0, ControlBuilder.Controls.Count);
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestSubLevel;
var
  ControlBuilder: TControlBuilder;
  InitialLevel, NewLevel: TControlBuilderLevel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    InitialLevel := ControlBuilder.CurrentLevel;

    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .AddControl(TControlInfo.Create(TPanel).WithHeight(50).WithLeft(60))
      .SubLevel;

    NewLevel := ControlBuilder.CurrentLevel;

    AssertNotSame(
      Format('Level inicial deve ser difente do level atual. [%s, %s]',
        [InitialLevel.GroupName, NewLevel.GroupName]),
      InitialLevel, NewLevel
    );
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestSubLevelSubLevel;
var
  ControlBuilder: TControlBuilder;
  InitialLevel, NewLevel_01, NewLevel_02: TControlBuilderLevel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    InitialLevel := ControlBuilder.CurrentLevel;

    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .AddControl(TControlInfo.Create(TPanel).WithHeight(50).WithLeft(60))
      .SubLevel;

    NewLevel_01 := ControlBuilder.CurrentLevel;

    ControlBuilder.SubLevel;

    NewLevel_02 := ControlBuilder.CurrentLevel;

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
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestSubLevelSuperLevel;
var
  ControlBuilder: TControlBuilder;
  InitialLevel, InitialLevelBack: TControlBuilderLevel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    InitialLevel := ControlBuilder.CurrentLevel;

    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .SubLevel
      .SuperLevel;

    InitialLevelBack := ControlBuilder.CurrentLevel;

    AssertSame(
      Format('Level inicial deve ser difente do level atual. [%s, %s]',
        [InitialLevel.GroupName, InitialLevelBack.GroupName]),
      InitialLevel, InitialLevelBack
    );
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestSubLevelEmpty;
var
  ControlBuilder: TControlBuilder;
  P: TPanel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .SetSpace(5, 6)
      .SubLevel
        // Sublevel vazio. Ao sair e inserir novo controle no level raiz,
        // deve ser como se esse sublevel não existisse.
        // Não pode levantar exceção.
        // Não pode considerar o espaçamento na inclusão do item
        // após o SuperLevel.
      .SuperLevel
      .AddControl(TControlInfo.Create(TPanel, P));

    AssertEquals('Propriedade Top diferente da esperada', 0, P.Top);
    AssertEquals('Propriedade Left diferente da esperada', 0, P.Left);
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestSubLevelDirectionSuperLevelDirection;
var
  ControlBuilder: TControlBuilder;
  P1, P2: TPanel;
  InitialDirection, SubLevelDirection, SuperLevelDirection: TControlBuilderDirection;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    InitialDirection := ControlBuilder.CurrentLevel.Direction;

    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .SetControlWidth(80)
      .SetControlHeight(50)
      .SetDirection(cpdHorizontal)
      .AddControl(TControlInfo.Create(TPanel))          // Width 80 (A)
      .SubLevel
        .SetControlWidth(80)
        .SetControlHeight(50)
        .SetDirection(cpdVertical)
        .AddControl(TControlInfo.Create(TPanel))       // Width 80 (B)
        .AddControl(TControlInfo.Create(TPanel, P1))
    ;

    SubLevelDirection := ControlBuilder.CurrentLevel.Direction;

    ControlBuilder
      .SuperLevel
      .AddControl(TControlInfo.Create(TPanel, P2))    // <-- left 160 (80A + 80B)
    ;

    SuperLevelDirection := ControlBuilder.CurrentLevel.Direction;

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
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestGridMode;
var
  ControlBuilder: TControlBuilder;
  P1, P2, P3, P4: TPanel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdHorizontal)
      .GridInit(2, 2)
        .GridSetCellWidthAndHeight(6, 7)
        .AddControl(TControlInfo.Create(TPanel, P1))
        .AddControl(TControlInfo.Create(TPanel, P2))
        .AddControl(TControlInfo.Create(TPanel, P3))
        .AddControl(TControlInfo.Create(TPanel, P4))
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
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestGridModeIgnoreAddAfterEndOfGrid;
var
  ControlBuilder: TControlBuilder;
  P1, P2, P3, P4, P5: TPanel;
begin
  P1 := nil;
  P2 := nil;
  P3 := nil;
  P4 := nil;
  P5 := nil;

  ControlBuilder := TControlBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdHorizontal)
      .GridInit(2, 2)
        .GridSetCellWidthAndHeight(6, 7)
        .AddControl(TControlInfo.Create(TPanel, P1))
        .AddControl(TControlInfo.Create(TPanel, P2))
        .AddControl(TControlInfo.Create(TPanel, P3))
        .AddControl(TControlInfo.Create(TPanel, P4))
        .AddControl(TControlInfo.Create(TPanel, P5))  // deve ignorar pois já finalizou o grid
      .GridFinish
    ;

    AssertNotNull('P1 não deverfia ser nil', P1);
    AssertNotNull('P1 não deverfia ser nil', P2);
    AssertNotNull('P1 não deverfia ser nil', P3);
    AssertNotNull('P1 não deverfia ser nil', P4);
    AssertNull('P5 deverfia ser nil', P5);

  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestGridModeNotIgnoreAddAfterEndOfGridWhenAutoExpand;
var
  ControlBuilder: TControlBuilder;
  P1, P2, P3, P4, P5: TPanel;
begin
  P1 := nil;
  P2 := nil;
  P3 := nil;
  P4 := nil;
  P5 := nil;

  ControlBuilder := TControlBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdHorizontal)
      .GridInit(2, 2)
        .GridAutoExpand
        .GridSetCellWidthAndHeight(6, 7)
        .AddControl(TControlInfo.Create(TPanel, P1))
        .AddControl(TControlInfo.Create(TPanel, P2))
        .AddControl(TControlInfo.Create(TPanel, P3))
        .AddControl(TControlInfo.Create(TPanel, P4))
        .AddControl(TControlInfo.Create(TPanel, P5)) // ao adicionar esse registro grid se expande
      .GridFinish
    ;

    AssertNotNull('P1 não deverfia ser nil', P1);
    AssertNotNull('P1 não deverfia ser nil', P2);
    AssertNotNull('P1 não deverfia ser nil', P3);
    AssertNotNull('P1 não deverfia ser nil', P4);
    AssertNotNull('P5 não deverfia ser nil', P5);
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestGridModeAutoExpandRows;
var
  ControlBuilder: TControlBuilder;
  RowsBeforeExpand: Integer;
  RowsAfterExpand: Integer;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdHorizontal)
      .GridInit(2, 2)
        .GridAutoExpand
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .GridReturnNumberOfRows(RowsBeforeExpand)
        .AddControl(TControlInfo.Create(TPanel)) // ao adicionar esse registro grid se expande
        .GridReturnNumberOfRows(RowsAfterExpand)
      .GridFinish
    ;

    AssertEquals('O número de linhas do grid antes de expandir está diferente do esperado', 2, RowsBeforeExpand);
    AssertEquals('O número de linhas do grid depois de expandir está diferente do esperado', 3, RowsAfterExpand);
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestGridModeAutoExpandCols;
var
  ControlBuilder: TControlBuilder;
  ColsBeforeExpand: Integer;
  ColsAfterExpand: Integer;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdVertical)
      .GridInit(2, 2)
        .GridAutoExpand
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .GridReturnNumberOfCols(ColsBeforeExpand)
        .AddControl(TControlInfo.Create(TPanel)) // ao adicionar esse registro grid se expande
        .GridReturnNumberOfCols(ColsAfterExpand)
      .GridFinish
    ;

    AssertEquals('O número de colunas do grid antes de expandir está diferente do esperado', 2, ColsBeforeExpand);
    AssertEquals('O número de dolunas do grid depois de expandir está diferente do esperado', 3, ColsAfterExpand);
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestGridModeAutoExpandOnlyRows;
var
  ControlBuilder: TControlBuilder;
  RowsBeforeExpand: Integer;
  RowsAfterExpand: Integer;
  ColsBeforeExpand: Integer;
  ColsAfterExpand: Integer;
  P: TPanel;
begin
  ControlBuilder := TControlBuilder.Create;
  P := nil;
  try
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdHorizontal)
      .GridInit(2, 2)
        .GridAutoExpandRows                                         // aqui define que apenas expande Linhas
        .AddControl(TControlInfo.Create(TPanel).WithCaption('1'))
        .AddControl(TControlInfo.Create(TPanel).WithCaption('2'))
        .AddControl(TControlInfo.Create(TPanel).WithCaption('3'))
        .AddControl(TControlInfo.Create(TPanel).WithCaption('4'))

        .GridReturnNumberOfRows(RowsBeforeExpand)
        .AddControl(TControlInfo.Create(TPanel).WithCaption('5'))  // aqui expande automaticamente linhas para 3
        .GridReturnNumberOfRows(RowsAfterExpand)

        .SetDirection(cpdVertical) // ao mudar pra direção vertical, apos preencher a ultima celula da coluna, tentará criar uma nova coluna

        .AddControl(TControlInfo.Create(TPanel).WithCaption('6'))
        .GridReturnNumberOfCols(ColsBeforeExpand)
        .AddControl(TControlInfo.Create(TPanel, P).WithCaption('7'))  // aqui ignora pois não expande colunas
        .GridReturnNumberOfCols(ColsAfterExpand)
      .GridFinish
    ;


    AssertEquals('O número de linhas do grid antes de expandir está diferente do esperado', 2, RowsBeforeExpand);
    AssertEquals('O número de linhas do grid depois de expandir está diferente do esperado', 3, RowsAfterExpand);
    AssertEquals('O número de colunas do grid antes de expandir está diferente do esperado', 2, ColsBeforeExpand);
    AssertEquals('O número de colunas do grid depois de expandir está diferente do esperado', 2, ColsAfterExpand);

    AssertNull('P deveria ser nil, pois o grid pode expandir apenas linhas e não colunas', P);
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestGridModeAutoExpandOnlyCols;
var
  ControlBuilder: TControlBuilder;
  RowsBeforeExpand: Integer;
  RowsAfterExpand: Integer;

  ColsBeforeExpand: Integer;
  ColsAfterExpand: Integer;

  P: TPanel;
begin
  ControlBuilder := TControlBuilder.Create;
  P := nil;
  try
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdVertical)
      .GridInit(2, 2)
        .GridAutoExpandCols                                         // aqui define que apenas expande Colunas
        .AddControl(TControlInfo.Create(TPanel).WithCaption('1'))
        .AddControl(TControlInfo.Create(TPanel).WithCaption('2'))
        .AddControl(TControlInfo.Create(TPanel).WithCaption('3'))
        .AddControl(TControlInfo.Create(TPanel).WithCaption('4'))

        .GridReturnNumberOfCols(ColsBeforeExpand)
        .AddControl(TControlInfo.Create(TPanel).WithCaption('5'))  // aqui expande automaticamente Colunas para 3
        .GridReturnNumberOfCols(ColsAfterExpand)

        .SetDirection(cpdHorizontal) // ao mudar pra direção vertical, apos preencher a ultima celula da linha, tentará criar uma nova linha

        .AddControl(TControlInfo.Create(TPanel).WithCaption('6'))
        .GridReturnNumberOfRows(RowsBeforeExpand)
        .AddControl(TControlInfo.Create(TPanel, P).WithCaption('7'))  // aqui ignora pois não expande colunas
        .GridReturnNumberOfRows(RowsAfterExpand)
      .GridFinish
    ;


    AssertEquals('O número de colunas do grid antes de expandir está diferente do esperado', 2, ColsBeforeExpand);
    AssertEquals('O número de colunas do grid depois de expandir está diferente do esperado', 3, ColsAfterExpand);
    AssertEquals('O número de linhas do grid antes de expandir está diferente do esperado', 2, RowsBeforeExpand);
    AssertEquals('O número de linhas do grid depois de expandir está diferente do esperado', 2, RowsAfterExpand);

    AssertNull('P deveria ser nil, pois o grid pode expandir apenas linhas e não colunas', P);
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestGridSkipCell;
var
  ControlBuilder: TControlBuilder;
  P: TPanel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdHorizontal)
      .GridInit(2, 3)
        .GridSetCellWidthAndHeight(10, 15)

        .AddControl(TControlInfo.Create(TPanel))
        .GridSkipCell
        .AddControl(TControlInfo.Create(TPanel, P))
      .GridFinish
    ;

    AssertEquals('Propriedade Left diferente do esperado', 10 + 10, P.Left);
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestGridRowHeight;
var
  ControlBuilder: TControlBuilder;
  P1, P2: TPanel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdHorizontal)
      .GridInit(3, 3)
        .GridSetCellWidthAndHeight(50, 60)  // altura padrão da linha = 60
        .GridSetRowHeight(0, 100)           // altera a linha 0 para altura = 100

        .AddControl(TControlInfo.Create(TPanel, P1))  // p1 esta na linha 0
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel, P2)) // p2 esta na linha 2
      .GridFinish
    ;

    AssertEquals('propriedade Height de P1 diferente do esperada', 100, P1.Height);
    AssertEquals('Propriedade Height de P2 diferente do esperada', 60, P2.Height);
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestGridColWidth;
var
  ControlBuilder: TControlBuilder;
  P1, P2: TPanel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdHorizontal)
      .GridInit(3, 3)
        .GridSetCellWidthAndHeight(50, 60)  // largura padrão da linha = 50
        .GridSetColWidth(0, 90)           // altera a coluna 0 para largura = 90

        .AddControl(TControlInfo.Create(TPanel, P1))  // p1 esta na coluna 0
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel, P2)) // p2 esta na coluna 2
      .GridFinish
    ;

    AssertEquals('propriedade Width de P1 diferente do esperada', 90, P1.Width);
    AssertEquals('Propriedade Width de P2 diferente do esperada', 50, P2.Width);
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestGridRowSpan;
var
  ControlBuilder: TControlBuilder;
  P1, P2: TPanel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdVertical)
      .GridInit(3, 3)
        .GridSetCellWidthAndHeight(50, 60)  // altura da celula

        .AddControl(TControlInfo.Create(TPanel, P1)) // sem rowspan
        .GridRowSpan(2)
        .AddControl(TControlInfo.Create(TPanel, P2)) // com rowspan=2
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
      .GridFinish
    ;

    AssertEquals('propriedade Height de P1 diferente do esperada', 60, P1.Height);
    AssertEquals('Propriedade Height de P2 diferente do esperada', 60*2, P2.Height);
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestGridColSpan;
var
  ControlBuilder: TControlBuilder;
  P1, P2: TPanel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdHorizontal)
      .GridInit(3, 3)
        .GridSetCellWidthAndHeight(50, 60)
        .AddControl(TControlInfo.Create(TPanel, P1)) // sem colspan
        .GridColSpan(2)
        .AddControl(TControlInfo.Create(TPanel, P2)) // com colspan=2
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
      .GridFinish
    ;

    AssertEquals('propriedade Width de P1 diferente do esperada', 50, P1.Width);
    AssertEquals('Propriedade Width de P2 diferente do esperada', 50*2, P2.Width);
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestGridRowSpanOutOfBounds;
var
  ControlBuilder: TControlBuilder;
  P: TPanel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdVertical)
      .GridInit(3, 3)
        .GridSetCellWidthAndHeight(50, 60)  // altura da celula

        .AddControl(TControlInfo.Create(TPanel)) // sem rowspan

        // span 3 irá ultrapassar limite da ultima linha do grid.
        // espera-se que limite a 2
        .GridRowSpan(3)

        .AddControl(TControlInfo.Create(TPanel, P)) // com rowspan=2
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
      .GridFinish
    ;

    AssertEquals('Propriedade Height de P2 diferente do esperada', 60*2, P.Height);
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestGridColSpanOutOfBounds;
var
  ControlBuilder: TControlBuilder;
  P: TPanel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdHorizontal)
      .GridInit(3, 3)
        .GridSetCellWidthAndHeight(50, 60)  // altura da celula

        .AddControl(TControlInfo.Create(TPanel))

        // span 3 irá ultrapassar limite da ultima coluna do grid.
        // espera-se que limite a 2
        .GridColSpan(3)

        .AddControl(TControlInfo.Create(TPanel, P)) // com colspan=2
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
      .GridFinish
    ;

    AssertEquals('Propriedade Width de P2 diferente do esperada', 50*2, P.Width);
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestGridRowSpanExpandGridRows;
var
  ControlBuilder: TControlBuilder;
  P: TPanel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdVertical)
      .GridInit(3, 3)
        .GridAutoExpand
        .GridSetCellWidthAndHeight(50, 60)  // altura da celula

        .AddControl(TControlInfo.Create(TPanel)) // sem rowspan

        // span 3 irá ultrapassar limite da ultima linha do grid.
        // espera-se que expanda o grid para 4 linhas
        .GridRowSpan(3)

        .AddControl(TControlInfo.Create(TPanel, P)) // com rowspan=3
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
      .GridFinish
    ;

    AssertEquals('Propriedade Height de P2 diferente do esperada', 60*3, P.Height);
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestGridColSpanExpandGridCols;
var
  ControlBuilder: TControlBuilder;
  P: TPanel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdHorizontal)
      .GridInit(3, 3)
        .GridAutoExpand
        .GridSetCellWidthAndHeight(50, 60)  // altura da celula

        .AddControl(TControlInfo.Create(TPanel))

        // span 3 irá ultrapassar limite da ultima coluna do grid.
        // espera-se que expanda as colunas do grid
        .GridColSpan(3)

        .AddControl(TControlInfo.Create(TPanel, P)) // com colspan=3
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
      .GridFinish
    ;

    AssertEquals('Propriedade Width de P diferente da esperada', 50*3, P.Width);
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestGridGotoCell;
var
  ControlBuilder: TControlBuilder;
  P: TPanel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdHorizontal)
      .GridInit(5, 5)
        .GridSetCellWidthAndHeight(30, 42)
        .GridGotoCell(2, 3)
        .AddControl(TControlInfo.Create(TPanel, P))
      .GridFinish
    ;

    AssertEquals('Propriedade Top diferente da esperada', 42*2, P.Top);
    AssertEquals('Propriedade Left diferente da esperada', 30*3, P.Left);
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestGridRowSpanWithSpace;
var
  ControlBuilder: TControlBuilder;
  P: TPanel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdVertical)
      .SetVerticalSpace(7)                  // define o espacamento entre duas linhas
      .GridInit(3, 3)
        .GridSetCellWidthAndHeight(50, 60)  // altura da celula

        .AddControl(TControlInfo.Create(TPanel))
        .GridRowSpan(2)
        .AddControl(TControlInfo.Create(TPanel, P)) // com rowspan=2
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
      .GridFinish
    ;

    AssertEquals('propriedade Height diferente da esperada', 60*2 + 7, P.Height);
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestGridColSpanWithSpace;
var
  ControlBuilder: TControlBuilder;
  P: TPanel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .SetDirection(cpdHorizontal)
      .SetHorizontalSpace(9)            // define o espaçaento entre as colunas
      .GridInit(3, 3)
        .GridSetCellWidthAndHeight(50, 60)
        .AddControl(TControlInfo.Create(TPanel))
        .GridColSpan(2)
        .AddControl(TControlInfo.Create(TPanel, P)) // com colspan=2
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
        .AddControl(TControlInfo.Create(TPanel))
      .GridFinish
    ;

    AssertEquals('Propriedade Width diferente da esperada', 50*2+9, P.Width);
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestCellStrechHorizontal;
var
  ControlBuilder: TControlBuilder;
  P: TPanel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .GridInit(1, 1)
        .GridSetCellWidthAndHeight(50, 60)
        .GridCellStrechHorizontal                                                // strech na horizontal = largura da celula
        .AddControl(TControlInfo.Create(TPanel, P).WithWidthAndHeight(30, 25))
      .GridFinish
    ;

    AssertEquals('Propriedade Width diferente da esperada', 50, P.Width);        // usa largura da celula (streched)
    AssertEquals('Propriedade Height diferente da esperada', 25, P.Height);      // usa altura definida TControlInfo (not streched)
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestCellStrechVertical;
var
  ControlBuilder: TControlBuilder;
  P: TPanel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .GridInit(1, 1)
        .GridSetCellWidthAndHeight(50, 60)
        .GridCellStrechVertical                                                // strech na horizontal = largura da celula
        .AddControl(TControlInfo.Create(TPanel, P).WithWidthAndHeight(30, 25))
      .GridFinish
    ;

    AssertEquals('Propriedade Width diferente da esperada', 30, P.Width);        // usa largura definida TControlInfo (not streched)
    AssertEquals('Propriedade Height diferente da esperada', 60, P.Height);      // usa altura da celula (streched)
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestCellStrechAll;
var
  ControlBuilder: TControlBuilder;
  P: TPanel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .GridInit(1, 1)
        .GridSetCellWidthAndHeight(50, 60)
        .GridCellStrechAll                                                // strech na horizontal e vertical
        .AddControl(TControlInfo.Create(TPanel, P).WithWidthAndHeight(30, 25))   // 30 e 25: esses valores sao ignorados
      .GridFinish
    ;

    AssertEquals('Propriedade Width diferente da esperada', 50, P.Width);        // usa largura da celula (streched)
    AssertEquals('Propriedade Height diferente da esperada', 60, P.Height);      // usa altura da celula (streched)
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestCellNoStrech;
var
  ControlBuilder: TControlBuilder;
  P: TPanel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .GridInit(1, 1)
        .GridSetCellWidthAndHeight(50, 60)
        .GridCellNoStrech                                                     // sem strech
        .AddControl(TControlInfo.Create(TPanel, P).WithWidthAndHeight(30, 25))
      .GridFinish
    ;

    AssertEquals('Propriedade Width diferente da esperada', 30, P.Width);        // usa largura definida no TControlInfo
    AssertEquals('Propriedade Height diferente da esperada', 25, P.Height);      // usa altura definida no TControlInfo
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestCellNoStrechCenter;
var
  ControlBuilder: TControlBuilder;
  P: TPanel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .GridInit(1, 1)
        .GridSetCellWidthAndHeight(60, 50)
        .GridCellNoStrech
        .GridCellPosition(cpCenter)
        .AddControl(TControlInfo.Create(TPanel, P).WithWidthAndHeight(10, 14))
      .GridFinish
    ;

    AssertEquals('Propriedade Left diferente da esperada', (60 div 2) - (10 div 2), P.Left);
    AssertEquals('Propriedade Top diferente da esperada', (50 div 2) - (14 div 2), P.Top);
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestCellNoStrechTop;
var
  ControlBuilder: TControlBuilder;
  P: TPanel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .GridInit(1, 1)
        .GridSetCellWidthAndHeight(60, 50)
        .GridCellNoStrech
        .GridCellPosition(cpTop)
        .AddControl(TControlInfo.Create(TPanel, P).WithWidthAndHeight(10, 14))
      .GridFinish
    ;

    AssertEquals('Propriedade Left diferente da esperada', (60 div 2) - (10 div 2), P.Left);
    AssertEquals('Propriedade Top diferente da esperada', 0, P.Top);
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestCellNoStrechTopRight;
var
  ControlBuilder: TControlBuilder;
  P: TPanel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .GridInit(1, 1)
        .GridSetCellWidthAndHeight(60, 50)
        .GridCellNoStrech
        .GridCellPosition(cpTopRight)
        .AddControl(TControlInfo.Create(TPanel, P).WithWidthAndHeight(10, 14))
      .GridFinish
    ;

    AssertEquals('Propriedade Left diferente da esperada', 60 - 10, P.Left);
    AssertEquals('Propriedade Top diferente da esperada', 0, P.Top);
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestCellNoStrechRight;
var
  ControlBuilder: TControlBuilder;
  P: TPanel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .GridInit(1, 1)
        .GridSetCellWidthAndHeight(60, 50)
        .GridCellNoStrech
        .GridCellPosition(cpRight)
        .AddControl(TControlInfo.Create(TPanel, P).WithWidthAndHeight(10, 14))
      .GridFinish
    ;

    AssertEquals('Propriedade Left diferente da esperada', 60 - 10, P.Left);
    AssertEquals('Propriedade Top diferente da esperada', (50 div 2) - (14 div 2), P.Top);
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestCellNoStrechBottomRight;
var
  ControlBuilder: TControlBuilder;
  P: TPanel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .GridInit(1, 1)
        .GridSetCellWidthAndHeight(60, 50)
        .GridCellNoStrech
        .GridCellPosition(cpBottomRight)
        .AddControl(TControlInfo.Create(TPanel, P).WithWidthAndHeight(10, 14))
      .GridFinish
    ;

    AssertEquals('Propriedade Left diferente da esperada', 60 - 10, P.Left);
    AssertEquals('Propriedade Top diferente da esperada', 50 - 14, P.Top);
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestCellNoStrechBottom;
var
  ControlBuilder: TControlBuilder;
  P: TPanel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .GridInit(1, 1)
        .GridSetCellWidthAndHeight(60, 50)
        .GridCellNoStrech
        .GridCellPosition(cpBottom)
        .AddControl(TControlInfo.Create(TPanel, P).WithWidthAndHeight(10, 14))
      .GridFinish
    ;

    AssertEquals('Propriedade Left diferente da esperada', (60 div 2) - (10 div 2), P.Left);
    AssertEquals('Propriedade Top diferente da esperada', 50 - 14, P.Top);
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestCellNoStrechBottomLeft;
var
  ControlBuilder: TControlBuilder;
  P: TPanel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .GridInit(1, 1)
        .GridSetCellWidthAndHeight(60, 50)
        .GridCellNoStrech
        .GridCellPosition(cpBottomLeft)
        .AddControl(TControlInfo.Create(TPanel, P).WithWidthAndHeight(10, 14))
      .GridFinish
    ;

    AssertEquals('Propriedade Left diferente da esperada', 0, P.Left);
    AssertEquals('Propriedade Top diferente da esperada', 50 - 14, P.Top);
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestCellNoStrechLeft;
var
  ControlBuilder: TControlBuilder;
  P: TPanel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .GridInit(1, 1)
        .GridSetCellWidthAndHeight(60, 50)
        .GridCellNoStrech
        .GridCellPosition(cpLeft)
        .AddControl(TControlInfo.Create(TPanel, P).WithWidthAndHeight(10, 14))
      .GridFinish
    ;

    AssertEquals('Propriedade Left diferente da esperada', 0, P.Left);
    AssertEquals('Propriedade Top diferente da esperada', (50 div 2) - (14 div 2), P.Top);
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestCellNoStrechTopLeft;
var
  ControlBuilder: TControlBuilder;
  P: TPanel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .GridInit(1, 1)
        .GridSetCellWidthAndHeight(60, 50)
        .GridCellNoStrech
        .GridCellPosition(cpTopLeft)
        .AddControl(TControlInfo.Create(TPanel, P).WithWidthAndHeight(10, 14))
      .GridFinish
    ;

    AssertEquals('Propriedade Left diferente da esperada', 0, P.Left);
    AssertEquals('Propriedade Top diferente da esperada', 0, P.Top);
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestGrid1x1;
{ testa bug corrigido. não incluia controle em grid 1x1 }
var
  ControlBuilder: TControlBuilder;
  P: TPanel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    P := nil;
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .GridInit(1, 1)
        .GridSetCellWidthAndHeight(50, 50)
        .GridCellStrechAll
        .AddControl(TControlInfo.Create(TPanel, P))
      .GridFinish
    ;

    AssertNotNull('P não deveria ser null', P);
  finally
    ControlBuilder.Free;
  end;
end;

procedure TControlBuilderTests.TestGridGoToLastCell;
{ testa bug corrigido. não incluia controle ao posicionar na última celula. }
var
  ControlBuilder: TControlBuilder;
  P: TPanel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    P := nil;
    ControlBuilder
      .WithOwnerAndParent(FForm, FForm)
      .GridInit(2, 2)
        .GridGotoCell(1, 1)
        .GridSetCellWidthAndHeight(50, 50)
        .GridCellStrechAll
        .AddControl(TControlInfo.Create(TPanel, P))
      .GridFinish
    ;

    AssertNotNull('P não deveria ser null', P);
  finally
    ControlBuilder.Free;
  end;
end;

initialization
  RegisterTest(TControlBuilderTests);

end.

