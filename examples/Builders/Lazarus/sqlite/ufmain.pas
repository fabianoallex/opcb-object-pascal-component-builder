unit UFMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3conn, sqldb, db, Forms, Controls, Graphics, Dialogs,
  Buttons, ComCtrls, ExtCtrls, Menus, ActnList, StdCtrls, OPCB, UFormUsers;

type

  { TFMain }

  TFMain = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FFormUsers: TFormUsers2;
    FMenuImages: TImageList;
    FPageControl: TPageControl;
    FStatusPanelConnected: TStatusPanel;
    FStatusPanelUser: TStatusPanel;
    procedure MenuItemCloseClick(ASender: TObject);
    procedure MenuItemLogoffClick(ASender: TObject);
    procedure MenuItemUsersClick(ASender: TObject);
    procedure SetFormUsers(AValue: TFormUsers2);
    procedure SetupMainMenu(AMenu: TMenu);
    procedure SetupMenuImages(AComponent: TComponent);
    procedure UpdateStatusBar;
    function AddTabSheet: TTabSheet;
  private
    procedure BuildForm;
    procedure SetupStatusBar(AControl: TControl);
    procedure ShowLogin;
    property PageControl: TPageControl read FPageControl;
    property StatusPanelUser: TStatusPanel read FStatusPanelUser;
    property StatusPanelConnected: TStatusPanel read FStatusPanelConnected;
    property FormUsers: TFormUsers2 read FFormUsers write SeTFormUsers;
    property MenuImages: TImageList read FMenuImages;
  public
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  end;

var
  FMain: TFMain;

implementation

uses
  UUserSession, UFormLogin, StrUtils;

procedure TFMain.ShowLogin;
begin
  CallFormLogin;
  if not UserSession.Logged then
    Application.Terminate;
end;

procedure TFMain.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FFormUsers) then
    FFormUsers := nil;
end;

procedure TFMain.SetupStatusBar(AControl: TControl);
var
  StatusBar: TStatusBar;
begin
  StatusBar := (AControl as TStatusBar);
  StatusBar.SimplePanel := False;

  FStatusPanelUser := StatusBar.Panels.Add;
  FStatusPanelUser.Width := 150;

  FStatusPanelConnected := StatusBar.Panels.Add;
  FStatusPanelConnected.Width := 150;
end;

procedure TFMain.SetupMenuImages(AComponent: TComponent);
var
  ImageList: TImageList;
  Picture: TPicture;

  procedure AddFromFile(AFileName: string);
  var
    srcBmp: TBitmap;
  begin
    Picture.LoadFromFile(AFileName);
    SrcBmp := TBitmap.Create;
    try
      SrcBmp.Assign(Picture.Graphic);
      ImageList.Add(SrcBmp, nil);
    finally
      SrcBmp.Free;
    end;
  end;

begin
  ImageList := (AComponent as TImageList);
  ImageList.Width := 16;
  ImageList.Height := 16;

  Picture := TPicture.Create;
  try
    AddFromFile('img.user.png');
    AddFromFile('img.logout.png');
    AddFromFile('img.close.png');
    AddFromFile('img.info.png');
  finally
    Picture.Free;
  end;
end;

procedure TFMain.SetupMainMenu(AMenu: TMenu);
begin
  AMenu.Images := FMenuImages;
end;

procedure TFMain.BuildForm;
var
  Builders: TOPCBBuilders;
begin
  Builders := TOPCBBuilders.Create;

  try
    Builders.AsComponentBuilder
      .WithOwner(Self)
      .Add(TComponentInfo.Create(TImageList, FMenuImages).Setup(@SetupMenuImages))
    ;

    Builders.AsMenuBuilder
      .WithOwner(Self)
      .AddMenu(TMenuInfo.Create(TMainMenu).Setup(@SetupMainMenu))
        .SubLevel(TMenuItemInfo.Create.WithCaption('Aplicação'))
          .AddMenuItem(TMenuItemInfo.Create.WithCaption('Logoff').WithOnClick(@MenuItemLogoffClick).WithImageIndex(1))
          .AddMenuItem(TMenuItemInfo.Create.WithCaption('-'))
          .AddMenuItem(TMenuItemInfo.Create.WithCaption('Fechar').WithOnClick(@MenuItemCloseClick).WithImageIndex(2))
        .SuperLevel
        .SubLevel(TMenuItemInfo.Create.WithCaption('Cadastros'))
          .AddMenuItem(TMenuItemInfo.Create.WithCaption('Usuários').WithOnClick(@MenuItemUsersClick).WithImageIndex(0))
        .SuperLevel
        .SubLevel(TMenuItemInfo.Create.WithCaption('Ajuda'))
          .AddMenuItem(TMenuItemInfo.Create.WithCaption('Sobre').WithImageIndex(3))
        .SuperLevel
    ;

    Builders.AsControlsBuilder
      .WithOwnerAndParent(Self, Self)
      .SetSpace(5, 5)
      .SetTopLeft(10, 10)
      .AddControl(TControlInfo.Create(TStatusBar).Setup(@SetupStatusBar))
      .SubLevel(TControlInfo.Create(TPageControl, FPageControl).WithAlign(alClient))
        .SubLevel(TControlInfo.Create(TTabSheet).WithCaption('Dashboard'))
        .SuperLevel
      .SuperLevel
    ;
  finally
    Builders.Free;
  end;
end;

procedure TFMain.MenuItemUsersClick(ASender: TObject);
var
  TabSheet: TTabSheet;
begin
  if not Assigned(FormUsers) then
  begin
    FormUsers := TFormUsers2.CreateNew(Application);
    TabSheet := AddTabSheet;
    FormUsers.Parent := TabSheet;
    TabSheet.Caption := FormUsers.Caption;
    FormUsers.Show;
  end
  else
  begin
    (FormUsers.Parent as TTabSheet).PageControl.ActivePage := (FormUsers.Parent as TTabSheet);
  end;
end;

procedure TFMain.SetFormUsers(AValue: TFormUsers2);
begin
  if FFormUsers = AValue then Exit;
  FFormUsers := AValue;
end;

procedure TFMain.MenuItemLogoffClick(ASender: TObject);
begin
  if MessageDlg('Deseja realmente deslogar?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    UserSession.Logout;
    UpdateStatusBar;
    CallFormLogin;
    UpdateStatusBar;

    if not UserSession.Logged then
      Application.Terminate;
  end;
end;

procedure TFMain.MenuItemCloseClick(ASender: TObject);
begin
  if MessageDlg('Deseja realmente sair da aplicação?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    Application.Terminate;
end;

procedure TFMain.UpdateStatusBar;
begin
  Self.StatusPanelUser.Text := 'Usuário: ' + UserSession.UserName;
  Self.StatusPanelConnected.Text := 'Status: ' + IfThen(UserSession.Logged, 'Conectado', 'Desconectado');
end;

function TFMain.AddTabSheet: TTabSheet;
var
  ControlsBuilder: TControlsBuilder;
begin
  ControlsBuilder := TControlsBuilder.Create(Self.Name);
  try
    ControlsBuilder
      .WithOwnerAndParent(Self, PageControl)
      .AddControl(TControlInfo.Create(TTabSheet).WithCaption('Nova aba'))
    ;
    Result := ControlsBuilder.Controls.Last as TTabSheet;
    PageControl.ActivePage := Result;
  finally
    ControlsBuilder.Free;
  end;
end;

procedure TFMain.FormCreate(Sender: TObject);
begin
  BuildForm;
  UpdateStatusBar;
end;

procedure TFMain.FormShow(Sender: TObject);
begin
  ShowLogin;
  UpdateStatusBar;
end;

{$R *.lfm}

end.

