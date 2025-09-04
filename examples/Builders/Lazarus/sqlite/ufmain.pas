unit UFMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3conn, sqldb, db, Forms, Controls, Graphics, Dialogs,
  Buttons, ComCtrls, ExtCtrls, Menus, ActnList, OPCB, UFormUsers;

type

  { TFMain }

  TFMain = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FFormUsers: TFormUsers2;
    FPageControl: TPageControl;
    FStatusPanelConnected: TStatusPanel;
    FStatusPanelUser: TStatusPanel;
    procedure MenuItemCloseClick(ASender: TObject);
    procedure MenuItemLogoffClick(ASender: TObject);
    procedure MenuItemUsersClick(ASender: TObject);
    procedure SetFormUsers(AValue: TFormUsers2);
    procedure SetPageControl(AValue: TPageControl);
    procedure SetStatusPanelConnected(AValue: TStatusPanel);
    procedure SetStatusPanelUser(AValue: TStatusPanel);
    procedure UpdateStatusBar;
    function AddTabSheet: TTabSheet;
  private
    procedure BuildForm;
    procedure SetupStatusBar(AControl: TControl);
    procedure ShowLogin;
    property PageControl: TPageControl read FPageControl write SetPageControl;
    property StatusPanelUser: TStatusPanel read FStatusPanelUser write SetStatusPanelUser;
    property StatusPanelConnected: TStatusPanel read FStatusPanelConnected write SetStatusPanelConnected;
    property FormUsers: TFormUsers2 read FFormUsers write SeTFormUsers;
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

  Self.StatusPanelUser := StatusBar.Panels.Add;
  Self.StatusPanelUser.Width := 150;

  Self.StatusPanelConnected := StatusBar.Panels.Add;
  Self.StatusPanelConnected.Width := 150;
end;

procedure TFMain.BuildForm;
var
  ControlBuilder: TControlBuilder;
  MenuBuilder: TMenuBuilder;
begin
  ControlBuilder := TControlBuilder.Create(Self.Name);
  try
    ControlBuilder
      .WithOwnerAndParent(Self, Self)
      .SetSpace(5, 5)
      .SetTopLeft(10, 10)
      .AddControl(TControlInfo.Create(TStatusBar).Setup(@SetupStatusBar))
      .NextLevel(TControlInfo.Create(TPageControl, FPageControl).WithAlign(alClient))
        .AddControl(TControlInfo.Create(TTabSheet).WithCaption('Dashboard'))
      .PreviousLevel
    ;
  finally
    ControlBuilder.Free;
  end;

  MenuBuilder := TMenuBuilder.Create(Self.Name);
  try
    MenuBuilder
      .WithOwner(Self)
      .AddMenu(TMenuInfo.Create(TMainMenu))
        .NextLevel(TMenuItemInfo.Create.WithCaption('Aplicação'))
          .AddMenuItem(TMenuItemInfo.Create.WithCaption('Logoff').WithOnClick(@MenuItemLogoffClick))
          .AddMenuItem(TMenuItemInfo.Create.WithCaption('-'))
          .AddMenuItem(TMenuItemInfo.Create.WithCaption('Fechar').WithOnClick(@MenuItemCloseClick))
        .PreviousLevel
        .NextLevel(TMenuItemInfo.Create.WithCaption('Cadastros'))
          .AddMenuItem(TMenuItemInfo.Create.WithCaption('Usuários').WithOnClick(@MenuItemUsersClick))
        .PreviousLevel
        .NextLevel(TMenuItemInfo.Create.WithCaption('Ajuda'))
          .AddMenuItem(TMenuItemInfo.Create.WithCaption('Sobre'))
        .PreviousLevel
    ;
  finally
    MenuBuilder.Free;
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

procedure TFMain.SeTFormUsers(AValue: TFormUsers2);
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
  ControlBuilder: TControlBuilder;
begin
  ControlBuilder := TControlBuilder.Create(Self.Name);
  try
    ControlBuilder
      .WithOwnerAndParent(Self, PageControl)
      .AddControl(TControlInfo.Create(TTabSheet).WithCaption('Nova aba'))
    ;
    Result := ControlBuilder.Controls.Last as TTabSheet;
    PageControl.ActivePage := Result;
  finally
    ControlBuilder.Free;
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

procedure TFMain.SetStatusPanelConnected(AValue: TStatusPanel);
begin
  if FStatusPanelConnected = AValue then Exit;
  FStatusPanelConnected := AValue;
end;

procedure TFMain.SetPageControl(AValue: TPageControl);
begin
  if FPageControl = AValue then Exit;
  FPageControl := AValue;
end;

procedure TFMain.SetStatusPanelUser(AValue: TStatusPanel);
begin
  if FStatusPanelUser = AValue then Exit;
  FStatusPanelUser := AValue;
end;

{$R *.lfm}

end.

