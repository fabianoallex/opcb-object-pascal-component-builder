unit UMenuCreatorTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, Forms, Menus, OPCB;

type

  { TMenuCreatorTests }

  TMenuCreatorTests = class(TTestCase)
  private
    FForm: TForm;
    FClickCount: Integer;
    procedure MenuItemClick(Sender: TObject);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddMenu;
    procedure TestAddMenuItem;
    procedure TestMenuItemBuilderWithCaptionImageIndexAndOnClick;
    procedure TestSubLevelAddsItemsToSubmenu;
    procedure TestSubLevelSuperLevelReturnsToRootLevel;
    procedure TestSuperLevelAtRootLevelRaises;
    procedure TestSetOwnerDirectly;
    procedure TestGetMenuGeneric;
    procedure TestAddMenuBuilderIsFreedNotLeaked;
    procedure TestAddMenuItemBeforeAddMenuRaises;
  end;

implementation

type
  { TCountingMenuBuilder }

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

procedure TMenuCreatorTests.MenuItemClick(Sender: TObject);
begin
  Inc(FClickCount);
end;

procedure TMenuCreatorTests.SetUp;
begin
  FForm := TForm.Create(nil);
  FClickCount := 0;
end;

procedure TMenuCreatorTests.TearDown;
begin
  FForm.Free;
end;

procedure TMenuCreatorTests.TestAddMenu;
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
    AssertNotNull('MainMenu não deveria ser nil', MainMenu);
    AssertSame('Owner do menu diferente do esperado', FForm, MainMenu.Owner);
  finally
    MenuCreator.Free;
  end;
end;

procedure TMenuCreatorTests.TestAddMenuItem;
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
    AssertEquals('Quantidade de itens do menu diferente da esperada', 2, MainMenu.Items.Count);
    AssertEquals('Caption do primeiro item diferente da esperada', 'Item 1', MainMenu.Items[0].Caption);
    AssertEquals('Caption do segundo item diferente da esperada', 'Item 2', MainMenu.Items[1].Caption);
  finally
    MenuCreator.Free;
  end;
end;

procedure TMenuCreatorTests.TestMenuItemBuilderWithCaptionImageIndexAndOnClick;
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
            .WithOnClick(@MenuItemClick)
        )
    ;

    Item := MenuCreator.GetMenuItem('ItemComEvento') as TMenuItem;
    AssertNotNull('Item não deveria ser nil', Item);
    AssertEquals('Caption diferente da esperada', 'Com Evento', Item.Caption);
    AssertEquals('ImageIndex diferente do esperado', 3, Item.ImageIndex);

    Item.Click; // simula o clique
    AssertEquals('OnClick não foi disparado o número de vezes esperado', 1, FClickCount);
  finally
    MenuCreator.Free;
  end;
end;

procedure TMenuCreatorTests.TestSubLevelAddsItemsToSubmenu;
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

    AssertEquals('MainMenu deveria ter só o FileMenu na raiz', 1, MainMenu.Items.Count);
    AssertEquals('FileMenu deveria ter 2 subitens', 2, FileMenu.Count);
    AssertEquals('Caption do primeiro subitem diferente da esperada', '&New', FileMenu.Items[0].Caption);
    AssertEquals('Caption do segundo subitem diferente da esperada', '&Open', FileMenu.Items[1].Caption);
  finally
    MenuCreator.Free;
  end;
end;

procedure TMenuCreatorTests.TestSubLevelSuperLevelReturnsToRootLevel;
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

    // depois do SuperLevel, o próximo AddMenuItem deve voltar a inserir na
    // raiz do menu (irmão do FileMenu), não dentro do FileMenu
    AssertEquals('MainMenu deveria ter FileMenu e EditMenu na raiz', 2, MainMenu.Items.Count);
    AssertEquals('Segundo item da raiz diferente do esperado', '&Edit', MainMenu.Items[1].Caption);
  finally
    MenuCreator.Free;
  end;
end;

procedure TMenuCreatorTests.TestSuperLevelAtRootLevelRaises;
var
  MenuCreator: TMenuCreator;
  Raised: Boolean;
begin
  Raised := False;
  MenuCreator := TMenuCreator.Create;
  try
    MenuCreator.SetOwner(FForm).AddMenu(TMenuBuilder.Create(TMainMenu, 'MainMenu'));
    try
      MenuCreator.SuperLevel; // não há SubLevel correspondente
    except
      on E: Exception do
        Raised := True;
    end;
    AssertTrue('SuperLevel no nível raiz deveria lançar uma exceção', Raised);
  finally
    MenuCreator.Free;
  end;
end;

procedure TMenuCreatorTests.TestSetOwnerDirectly;
var
  MenuCreator: TMenuCreator;
  Item: TMenuItem;
begin
  // Regressão: usa SetOwner diretamente (não o WithOwner deprecated).
  MenuCreator := TMenuCreator.Create;
  try
    MenuCreator
      .SetOwner(FForm)
      .AddMenu(TMenuBuilder.Create(TMainMenu, 'MainMenu'))
        .AddMenuItem(TMenuItemBuilder.Create(TMenuItem, 'Item1'))
    ;

    Item := MenuCreator.GetMenuItem('Item1') as TMenuItem;
    AssertSame('Owner do item de menu diferente do esperado', FForm, Item.Owner);
  finally
    MenuCreator.Free;
  end;
end;

procedure TMenuCreatorTests.TestGetMenuGeneric;
var
  MenuCreator: TMenuCreator;
begin
  // Regressão: GetMenu<T>/GetMenuItem<T> chamavam Registry.GetComponent<T>
  // internamente sem "specialize", o que travava o compilador FPC (erro
  // interno 2015071704) assim que o método genérico era de fato
  // instanciado - nunca havia sido exercitado por nenhum teste antes.
  MenuCreator := TMenuCreator.Create;
  try
    MenuCreator
      .SetOwner(FForm)
      .AddMenu(TMenuBuilder.Create(TMainMenu, 'MainMenu'))
        .AddMenuItem(TMenuItemBuilder.Create(TMenuItem, 'Item1'))
    ;

    AssertNotNull('GetMenu<T> não deveria devolver nil',
      MenuCreator.specialize GetMenu<TMainMenu>('MainMenu'));
    AssertNotNull('GetMenuItem<T> não deveria devolver nil',
      MenuCreator.specialize GetMenuItem<TMenuItem>('Item1'));
  finally
    MenuCreator.Free;
  end;
end;

procedure TMenuCreatorTests.TestAddMenuBuilderIsFreedNotLeaked;
var
  MenuCreator: TMenuCreator;
begin
  // Regressão: TMenuBuilder é recebido por AddMenu como parâmetro de classe
  // concreta (não de interface), então o refcounting de TInterfacedObject
  // nunca entrava em ação nesse caminho - e como ninguém chamava Free,
  // todo AddMenu vazava o builder. Agora o Creator assume a posse e libera
  // explicitamente logo após Build.
  GMenuBuilderDestroyCount := 0;
  MenuCreator := TMenuCreator.Create;
  try
    MenuCreator
      .SetOwner(FForm)
      .AddMenu(TCountingMenuBuilder.Create(TMainMenu, 'MainMenu'))
    ;

    AssertEquals('O builder do menu deveria ter sido liberado exatamente uma vez', 1, GMenuBuilderDestroyCount);
  finally
    MenuCreator.Free;
  end;
end;

procedure TMenuCreatorTests.TestAddMenuItemBeforeAddMenuRaises;
{ Regressão: o nível raiz do menu creator começa com Parent = nil;
  AddMenuItem não checava antes de usar, causando acesso a nil (AV) ao
  chamar AddMenuItem sem antes um AddMenu. Agora levanta uma exceção clara
  explicando o pré-requisito. }
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

    AssertTrue('AddMenuItem sem AddMenu anterior deveria levantar exceção', Raised);
  finally
    MenuCreator.Free;
  end;
end;

initialization

  RegisterTest(TMenuCreatorTests);
end.
