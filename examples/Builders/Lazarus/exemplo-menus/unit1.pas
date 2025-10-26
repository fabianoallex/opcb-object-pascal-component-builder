unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, OPCB;

type

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    FContextHandle: IRegistryContextHandle;
    FEditMenu: TMenuItem;
    FFileCloseMenu: TMenuItem;
    FFileMenu: TMenuItem;
    FFileNewMenu: TMenuItem;
    FFileOpenMenu: TMenuItem;
    FMainMenu: TMainMenu;
    FPopupMenu: TPopupMenu;
    FSearchMenu: TMenuItem;
    procedure SetContextHandle(AValue: IRegistryContextHandle);
    procedure SetEditMenu(AValue: TMenuItem);
    procedure SetFileCloseMenu(AValue: TMenuItem);
    procedure SetFileMenu(AValue: TMenuItem);
    procedure SetFileNewMenu(AValue: TMenuItem);
    procedure SetFileOpenMenu(AValue: TMenuItem);
    procedure SetMainMenu(AValue: TMainMenu);
    procedure SetPopupMenu(AValue: TPopupMenu);
    procedure SetSearchMenu(AValue: TMenuItem);
  public
    procedure ConfigMenus;
    property PopupMenuX: TPopupMenu read FPopupMenu write SetPopupMenu;
    property MainMenuX: TMainMenu read FMainMenu write SetMainMenu;
    property FileMenu: TMenuItem read FFileMenu write SetFileMenu;
    property FileNewMenu: TMenuItem read FFileNewMenu write SetFileNewMenu;
    property FileOpenMenu: TMenuItem read FFileOpenMenu write SetFileOpenMenu;
    property FileCloseMenu: TMenuItem read FFileCloseMenu write SetFileCloseMenu;
    property EditMenu: TMenuItem read FEditMenu write SetEditMenu;
    property SearchMenu: TMenuItem read FSearchMenu write SetSearchMenu;
    property ContextHandle: IRegistryContextHandle read FContextHandle write SetContextHandle;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ConfigMenus;
begin
  PopupMenuX := ContextHandle.Registry.GetComponent('PopupMenu') as TPopupMenu;
  MainMenuX := ContextHandle.Registry.GetComponent('MainMenu') as TMainMenu;
  FileMenu := ContextHandle.Registry.GetComponent('FileMenu') as TMenuItem;
  FileNewMenu := ContextHandle.Registry.GetComponent('FileNewMenu') as TMenuItem;
  FileOpenMenu := ContextHandle.Registry.GetComponent('FileOpenMenu') as TMenuItem;
  FileCloseMenu := ContextHandle.Registry.GetComponent('FileCloseMenu') as TMenuItem;
  EditMenu := ContextHandle.Registry.GetComponent('EditMenu') as TMenuItem;
  SearchMenu := ContextHandle.Registry.GetComponent('SearchMenu') as TMenuItem;

  FileMenu.Caption := '&File';
  FileNewMenu.Caption := '&New';
  FileOpenMenu.Caption := '&Open';
  FileCloseMenu.Caption := '&Close';

  EditMenu.Caption := '&Edit';
  SearchMenu.Caption := '&Search';

  Self.Menu := MainMenuX;
  Self.PopupMenu := PopupMenuX;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  Creators: TOPCBCreators;
begin
  ContextHandle := TComponentRegistry.GetContextHandle(Self.Name);

  Creators := TOPCBCreators.Create(Self.Name);
  try
    Creators.AsMenuCreator
      .WithOwner(Self)
      .AddMenu(TMenuBuilder.Create(TMainMenu, 'MainMenu')) // menu principal por primeiro
        .SubLevel(TMenuItemBuilder.Create(TMenuItem, 'FileMenu'))
          .AddMenuItem(TMenuItemBuilder.Create(TMenuItem, 'FileNewMenu'))
          .AddMenuItem(TMenuItemBuilder.Create(TMenuItem, 'FileOpenMenu'))
          .AddMenuItem(TMenuItemBuilder.Create(TMenuItem, 'FileCloseMenu'))
        .SuperLevel
        .SubLevel(TMenuItemBuilder.Create(TMenuItem, 'EditMenu'))
          .AddMenuItem(TMenuItemBuilder.Create(TMenuItem, 'Edit1').WithCaption('Edit 1'))
          .AddMenuItem(TMenuItemBuilder.Create(TMenuItem, 'Edit2').WithCaption('Edit 2'))
          .AddMenuItem(TMenuItemBuilder.Create(TMenuItem, 'Edit3').WithCaption('Edit 3'))
        .SuperLevel
        .SubLevel(TMenuItemBuilder.Create(TMenuItem, 'SearchMenu'))
          .AddMenuItem(TMenuItemBuilder.Create(TMenuItem, 'SearchMenu1').WithCaption('SearchMenu 1'))
          .AddMenuItem(TMenuItemBuilder.Create(TMenuItem, 'SearchMenu2').WithCaption('SearchMenu 2'))
          .AddMenuItem(TMenuItemBuilder.Create(TMenuItem, 'SearchMenu3').WithCaption('SearchMenu 3'))
        .SuperLevel
      .AddMenu(TMenuBuilder.Create(TPopupMenu, 'PopupMenu'))
        .AddMenuItem(TMenuItemBuilder.Create(TMenuItem, 'Popup1').WithCaption('Popup1'))
        .AddMenuItem(TMenuItemBuilder.Create(TMenuItem, 'Popup2').WithCaption('Popup2'))
        .AddMenuItem(TMenuItemBuilder.Create(TMenuItem, 'Popup3').WithCaption('Popup3'))
    ;

    Creators.AsControlCreator
      .WithOwnerAndParent(Self, Self)
      .SetSpace(5, 5)
      .SetTopLeft(10, 10)
    ;
  finally
    Creators.Free;
  end;

  ConfigMenus;
end;

procedure TForm1.SetContextHandle(AValue: IRegistryContextHandle);
begin
  if FContextHandle = AValue then Exit;
  FContextHandle := AValue;
end;

procedure TForm1.SetEditMenu(AValue: TMenuItem);
begin
  if FEditMenu = AValue then Exit;
  FEditMenu := AValue;
end;

procedure TForm1.SetFileCloseMenu(AValue: TMenuItem);
begin
  if FFileCloseMenu = AValue then Exit;
  FFileCloseMenu := AValue;
end;

procedure TForm1.SetFileMenu(AValue: TMenuItem);
begin
  if FFileMenu = AValue then Exit;
  FFileMenu := AValue;
end;

procedure TForm1.SetFileNewMenu(AValue: TMenuItem);
begin
  if FFileNewMenu = AValue then Exit;
  FFileNewMenu := AValue;
end;

procedure TForm1.SetFileOpenMenu(AValue: TMenuItem);
begin
  if FFileOpenMenu = AValue then Exit;
  FFileOpenMenu := AValue;
end;

procedure TForm1.SetMainMenu(AValue: TMainMenu);
begin
  if FMainMenu = AValue then Exit;
  FMainMenu := AValue;
end;

procedure TForm1.SetPopupMenu(AValue: TPopupMenu);
begin
  if FPopupMenu = AValue then Exit;
  FPopupMenu := AValue;
end;

procedure TForm1.SetSearchMenu(AValue: TMenuItem);
begin
  if FSearchMenu = AValue then Exit;
  FSearchMenu := AValue;
end;

end.

