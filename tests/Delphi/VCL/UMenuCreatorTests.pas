unit UMenuCreatorTests;

interface

uses
  System.SysUtils, DUnitX.TestFramework, Vcl.Forms, Vcl.Menus, OPCB;

type
  [TestFixture]
  TMenuCreatorTest = class
  private
    FForm: TForm;
    FClickCount: Integer;
    procedure MenuItemClick(Sender: TObject);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test] procedure TestAddMenu;
    [Test] procedure TestAddMenuItem;
    [Test] procedure TestMenuItemBuilderWithCaptionImageIndexAndOnClick;
    [Test] procedure TestSubLevelAddsItemsToSubmenu;
    [Test] procedure TestSubLevelSuperLevelReturnsToRootLevel;
    [Test] procedure TestSuperLevelAtRootLevelRaises;
    [Test] procedure TestSetOwnerDirectly;
    [Test] procedure TestGetMenuGeneric;
    [Test] procedure TestAddMenuBuilderIsFreedNotLeaked;
    [Test] procedure TestAddMenuItemBeforeAddMenuRaises;
  end;

implementation

type
  TCountingMenuBuilder = class(TMenuBuilder)
  public
    destructor Destroy; override;
  end;

var
  GMenuBuilderDestroyCount: Integer = 0;

destructor TCountingMenuBuilder.Destroy;
begin
  Inc(GMenuBuilderDestroyCount);
  inherited;
end;

procedure TMenuCreatorTest.MenuItemClick(Sender: TObject);
begin
  Inc(FClickCount);
end;

procedure TMenuCreatorTest.Setup;
begin
  FForm := TForm.Create(nil);
  FClickCount := 0;
end;

procedure TMenuCreatorTest.TearDown;
begin
  FForm.Free;
end;

procedure TMenuCreatorTest.TestAddMenu;
var
  MenuCreator: TMenuCreator;
  MainMenu: TMainMenu;
begin
  MenuCreator := TMenuCreator.Create;
  try
    MenuCreator
      .SetOwner(FForm)
      .AddMenu(TMenuBuilder.Create(TMainMenu, 'MainMenu'))
    ;

    MainMenu := MenuCreator.GetMenu('MainMenu') as TMainMenu;
    Assert.IsNotNull(MainMenu, 'MainMenu nao deveria ser nil');
    Assert.IsTrue(FForm = MainMenu.Owner, 'Owner do menu diferente do esperado');
  finally
    MenuCreator.Free;
  end;
end;

procedure TMenuCreatorTest.TestAddMenuItem;
var
  MenuCreator: TMenuCreator;
  MainMenu: TMainMenu;
begin
  MenuCreator := TMenuCreator.Create;
  try
    MenuCreator
      .SetOwner(FForm)
      .AddMenu(TMenuBuilder.Create(TMainMenu, 'MainMenu'))
        .AddMenuItem(TMenuItemBuilder.Create(TMenuItem, 'Item1').WithCaption('Item 1'))
        .AddMenuItem(TMenuItemBuilder.Create(TMenuItem, 'Item2').WithCaption('Item 2'))
    ;

    MainMenu := MenuCreator.GetMenu('MainMenu') as TMainMenu;
    Assert.AreEqual(2, MainMenu.Items.Count, 'Quantidade de itens do menu diferente da esperada');
    Assert.AreEqual('Item 1', MainMenu.Items[0].Caption, 'Caption do primeiro item diferente da esperada');
    Assert.AreEqual('Item 2', MainMenu.Items[1].Caption, 'Caption do segundo item diferente da esperada');
  finally
    MenuCreator.Free;
  end;
end;

procedure TMenuCreatorTest.TestMenuItemBuilderWithCaptionImageIndexAndOnClick;
var
  MenuCreator: TMenuCreator;
  Item: TMenuItem;
begin
  MenuCreator := TMenuCreator.Create;
  try
    MenuCreator
      .SetOwner(FForm)
      .AddMenu(TMenuBuilder.Create(TMainMenu, 'MainMenu'))
        .AddMenuItem(
          TMenuItemBuilder.Create(TMenuItem, 'ItemComEvento')
            .WithCaption('Com Evento')
            .WithImageIndex(3)
            .WithOnClick(MenuItemClick)
        )
    ;

    Item := MenuCreator.GetMenuItem('ItemComEvento') as TMenuItem;
    Assert.IsNotNull(Item, 'Item nao deveria ser nil');
    Assert.AreEqual('Com Evento', Item.Caption, 'Caption diferente da esperada');
    Assert.AreEqual(3, Item.ImageIndex, 'ImageIndex diferente do esperado');

    Item.Click; // simula o clique
    Assert.AreEqual(1, FClickCount, 'OnClick nao foi disparado o numero de vezes esperado');
  finally
    MenuCreator.Free;
  end;
end;

procedure TMenuCreatorTest.TestSubLevelAddsItemsToSubmenu;
var
  MenuCreator: TMenuCreator;
  FileMenu: TMenuItem;
  MainMenu: TMainMenu;
begin
  MenuCreator := TMenuCreator.Create;
  try
    MenuCreator
      .SetOwner(FForm)
      .AddMenu(TMenuBuilder.Create(TMainMenu, 'MainMenu'))
        .SubLevel(TMenuItemBuilder.Create(TMenuItem, 'FileMenu').WithCaption('&File'))
          .AddMenuItem(TMenuItemBuilder.Create(TMenuItem, 'FileNew').WithCaption('&New'))
          .AddMenuItem(TMenuItemBuilder.Create(TMenuItem, 'FileOpen').WithCaption('&Open'))
        .SuperLevel
    ;

    MainMenu := MenuCreator.GetMenu('MainMenu') as TMainMenu;
    FileMenu := MenuCreator.GetMenuItem('FileMenu') as TMenuItem;

    Assert.AreEqual(1, MainMenu.Items.Count, 'MainMenu deveria ter so o FileMenu na raiz');
    Assert.AreEqual(2, FileMenu.Count, 'FileMenu deveria ter 2 subitens');
    Assert.AreEqual('&New', FileMenu.Items[0].Caption, 'Caption do primeiro subitem diferente da esperada');
    Assert.AreEqual('&Open', FileMenu.Items[1].Caption, 'Caption do segundo subitem diferente da esperada');
  finally
    MenuCreator.Free;
  end;
end;

procedure TMenuCreatorTest.TestSubLevelSuperLevelReturnsToRootLevel;
var
  MenuCreator: TMenuCreator;
  MainMenu: TMainMenu;
begin
  MenuCreator := TMenuCreator.Create;
  try
    MenuCreator
      .SetOwner(FForm)
      .AddMenu(TMenuBuilder.Create(TMainMenu, 'MainMenu'))
        .SubLevel(TMenuItemBuilder.Create(TMenuItem, 'FileMenu').WithCaption('&File'))
          .AddMenuItem(TMenuItemBuilder.Create(TMenuItem, 'FileNew').WithCaption('&New'))
        .SuperLevel
        .AddMenuItem(TMenuItemBuilder.Create(TMenuItem, 'EditMenu').WithCaption('&Edit'))
    ;

    MainMenu := MenuCreator.GetMenu('MainMenu') as TMainMenu;

    // depois do SuperLevel, o proximo AddMenuItem deve voltar a inserir na
    // raiz do menu (irmao do FileMenu), nao dentro do FileMenu
    Assert.AreEqual(2, MainMenu.Items.Count, 'MainMenu deveria ter FileMenu e EditMenu na raiz');
    Assert.AreEqual('&Edit', MainMenu.Items[1].Caption, 'Segundo item da raiz diferente do esperado');
  finally
    MenuCreator.Free;
  end;
end;

procedure TMenuCreatorTest.TestSuperLevelAtRootLevelRaises;
var
  MenuCreator: TMenuCreator;
  Raised: Boolean;
begin
  Raised := False;
  MenuCreator := TMenuCreator.Create;
  try
    MenuCreator.SetOwner(FForm).AddMenu(TMenuBuilder.Create(TMainMenu, 'MainMenu'));
    try
      MenuCreator.SuperLevel; // nao ha SubLevel correspondente
    except
      on E: Exception do
        Raised := True;
    end;
    Assert.IsTrue(Raised, 'SuperLevel no nivel raiz deveria lancar uma excecao');
  finally
    MenuCreator.Free;
  end;
end;

procedure TMenuCreatorTest.TestSetOwnerDirectly;
var
  MenuCreator: TMenuCreator;
  Item: TMenuItem;
begin
  // Regressao: usa SetOwner diretamente (nao o WithOwner deprecated).
  MenuCreator := TMenuCreator.Create;
  try
    MenuCreator
      .SetOwner(FForm)
      .AddMenu(TMenuBuilder.Create(TMainMenu, 'MainMenu'))
        .AddMenuItem(TMenuItemBuilder.Create(TMenuItem, 'Item1'))
    ;

    Item := MenuCreator.GetMenuItem('Item1') as TMenuItem;
    Assert.IsTrue(FForm = Item.Owner, 'Owner do item de menu diferente do esperado');
  finally
    MenuCreator.Free;
  end;
end;

procedure TMenuCreatorTest.TestGetMenuGeneric;
var
  MenuCreator: TMenuCreator;
begin
  MenuCreator := TMenuCreator.Create;
  try
    MenuCreator
      .SetOwner(FForm)
      .AddMenu(TMenuBuilder.Create(TMainMenu, 'MainMenu'))
        .AddMenuItem(TMenuItemBuilder.Create(TMenuItem, 'Item1'))
    ;

    Assert.IsNotNull(MenuCreator.GetMenu<TMainMenu>('MainMenu'), 'GetMenu<T> nao deveria devolver nil');
    Assert.IsNotNull(MenuCreator.GetMenuItem<TMenuItem>('Item1'), 'GetMenuItem<T> nao deveria devolver nil');
  finally
    MenuCreator.Free;
  end;
end;

procedure TMenuCreatorTest.TestAddMenuBuilderIsFreedNotLeaked;
// Regressao: TMenuBuilder e recebido por AddMenu como parametro de classe
// concreta (nao de interface), entao o refcounting de TInterfacedObject
// nunca entrava em acao nesse caminho - e como ninguem chamava Free, todo
// AddMenu vazava o builder. Agora o Creator assume a posse e libera
// explicitamente logo apos Build.
var
  MenuCreator: TMenuCreator;
begin
  GMenuBuilderDestroyCount := 0;
  MenuCreator := TMenuCreator.Create;
  try
    MenuCreator
      .SetOwner(FForm)
      .AddMenu(TCountingMenuBuilder.Create(TMainMenu, 'MainMenu'))
    ;

    Assert.AreEqual(1, GMenuBuilderDestroyCount, 'O builder do menu deveria ter sido liberado exatamente uma vez');
  finally
    MenuCreator.Free;
  end;
end;

procedure TMenuCreatorTest.TestAddMenuItemBeforeAddMenuRaises;
// Regressao: o nivel raiz do menu creator comeca com Parent = nil;
// AddMenuItem nao checava antes de usar, causando acesso a nil (AV) ao
// chamar AddMenuItem sem antes um AddMenu. Agora levanta uma excecao clara
// explicando o pre-requisito.
var
  MenuCreator: TMenuCreator;
  Raised: Boolean;
begin
  Raised := False;
  MenuCreator := TMenuCreator.Create;
  try
    try
      MenuCreator
        .SetOwner(FForm)
        .AddMenuItem(TMenuItemBuilder.Create(TMenuItem, 'Item1')); // sem AddMenu antes
    except
      on E: Exception do
        Raised := True;
    end;

    Assert.IsTrue(Raised, 'AddMenuItem sem AddMenu anterior deveria levantar excecao');
  finally
    MenuCreator.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TMenuCreatorTest);

end.
