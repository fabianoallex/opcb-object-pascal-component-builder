unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, OPCB, FMX.Menus,
  FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
  private
    FFileNewMenu: TMenuItem;
    FFileOpenMenu: TMenuItem;
    FSearchMenu: TMenuItem;
    FFileCloseMenu: TMenuItem;
    FEditMenu: TMenuItem;
    FPopupMenu: TPopupMenu;
    FFileMenu: TMenuItem;
    FContextHandle: IRegistryContextHandle;
    FMainMenu: TMainMenu;
    procedure SetContextHandle(const Value: IRegistryContextHandle);
    procedure SetEditMenu(const Value: TMenuItem);
    procedure SetFileCloseMenu(const Value: TMenuItem);
    procedure SetFileMenu(const Value: TMenuItem);
    procedure SetFileNewMenu(const Value: TMenuItem);
    procedure SetFileOpenMenu(const Value: TMenuItem);
    procedure SetMainMenu(const Value: TMainMenu);
    procedure SetPopupMenu(const Value: TPopupMenu);
    procedure SetSearchMenu(const Value: TMenuItem);
    procedure ConfigMenus;
  public
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

{$R *.fmx}

{ TForm1 }

procedure TForm1.ConfigMenus;
begin
  PopupMenuX := ContextHandle.Registry.GetComponent<TPopupMenu>('PopupMenu');
  MainMenuX := ContextHandle.Registry.GetComponent<TMainMenu>('MainMenu');
  FileMenu := ContextHandle.Registry.GetComponent<TMenuItem>('FileMenu');
  FileNewMenu := ContextHandle.Registry.GetComponent<TMenuItem>('FileNewMenu');
  FileOpenMenu := ContextHandle.Registry.GetComponent<TMenuItem>('FileOpenMenu');
  FileCloseMenu := ContextHandle.Registry.GetComponent<TMenuItem>('FileCloseMenu');
  EditMenu := ContextHandle.Registry.GetComponent<TMenuItem>('EditMenu');
  SearchMenu := ContextHandle.Registry.GetComponent<TMenuItem>('SearchMenu');

  FileMenu.Text := '&File';
  FileNewMenu.Text := '&New';
  FileOpenMenu.Text := '&Open';
  FileCloseMenu.Text := '&Close';

  EditMenu.Text := '&Edit';
  SearchMenu.Text := '&Search';
end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  vPoint : TPointF;
begin
  if Button = TMouseButton.mbRight then
  begin
    vPoint := Self.ClientToScreen(PointF (X,Y));
    PopupMenuX.Popup(vPoint.X, vPoint.Y);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  Builders: TOPCBBuilders;
begin
  ContextHandle := TComponentRegistry.GetContextHandle(Self.Name);

  Builders := TOPCBBuilders.Create(Self.Name);
  try
    Builders.AsMenusBuilder
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

    Builders.AsControlCreator
      .WithOwnerAndParent(Self, Self)
      .SetSpace(5, 5)
      .SetTopLeft(10, 10)
    ;
  finally
    Builders.Free;
  end;

  ConfigMenus;
end;

procedure TForm1.SetContextHandle(const Value: IRegistryContextHandle);
begin
  FContextHandle := Value;
end;

procedure TForm1.SetEditMenu(const Value: TMenuItem);
begin
  FEditMenu := Value;
end;

procedure TForm1.SetFileCloseMenu(const Value: TMenuItem);
begin
  FFileCloseMenu := Value;
end;

procedure TForm1.SetFileMenu(const Value: TMenuItem);
begin
  FFileMenu := Value;
end;

procedure TForm1.SetFileNewMenu(const Value: TMenuItem);
begin
  FFileNewMenu := Value;
end;

procedure TForm1.SetFileOpenMenu(const Value: TMenuItem);
begin
  FFileOpenMenu := Value;
end;

procedure TForm1.SetMainMenu(const Value: TMainMenu);
begin
  FMainMenu := Value;
end;

procedure TForm1.SetPopupMenu(const Value: TPopupMenu);
begin
  FPopupMenu := Value;
end;

procedure TForm1.SetSearchMenu(const Value: TMenuItem);
begin
  FSearchMenu := Value;
end;

end.
