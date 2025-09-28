unit UFormUsers;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, OPCB, StdCtrls, ExtCtrls, Buttons, Controls, DB,
  UFormCRUD;

type

  { TFormUsers2 }

  TFormUsers2 = class(TFormCRUDBase)
  private
    procedure SetupDBEditID(AControl: TControl);
    procedure SetupDBEditPassword(AControl: TControl);
    procedure SetupDBEditUserName(AControl: TControl);
  protected
    procedure SetupDBGrid(AControl: TControl); override;
    procedure BuildContents(const ABuiler: TControlsBuilder); override;
  public
    constructor CreateNew(AOwner: TComponent);
  end;

implementation

uses
  UDBUser, DBCtrls, DBGrids, ComCtrls, Dialogs;

{ TFormUsers2 }

procedure TFormUsers2.SetupDBGrid(AControl: TControl);
begin
  inherited SetupDBGrid(AControl);

  with DBGrid.Columns.Add do
  begin
    FieldName := 'ID';
    Title.Caption := 'ID';
  end;

  with DBGrid.Columns.Add do
  begin
    FieldName := 'user_name';
    Title.Caption := 'Usuário';
  end;
end;

procedure TFormUsers2.BuildContents(const ABuiler: TControlsBuilder);
begin
  ABuiler
    .SetTopLeft(5, 5)
    .IncTopLeft(10, 10)
    {
    .NextLevel(cpdVertical)
      .AddControl(TControlInfo.Create(TLabel).WithCaption('ID'))
      .AddControl(TControlInfo.Create(TDBEdit).Setup(@SetupDBEditID))
    .PreviousLevel
    .NextLevel(cpdVertical)
      .AddControl(TControlInfo.Create(TLabel).WithCaption('Nome'))
      .AddControl(TControlInfo.Create(TDBEdit).WithWidth(250).Setup(@SetupDBEditUserName))
    .PreviousLevel
    .NextLevel(cpdVertical)
      .AddControl(TControlInfo.Create(TLabel).WithCaption('Senha'))
      .AddControl(TControlInfo.Create(TDBEdit).WithWidth(250).Setup(@SetupDBEditPassword))
    .PreviousLevel
    }

    .SetDirection(cpdVertical)
    .SetVerticalSpace(5)
    .AddControl(TControlInfo.Create(TLabel).WithCaption('ID').WithWidth(50))
    .AddControl(TControlInfo.Create(TLabel).WithCaption('Nome').WithWidth(50))
    .AddControl(TControlInfo.Create(TLabel).WithCaption('Senha').WithWidth(50))
    .Break
    .AddControl(TControlInfo.Create(TDBEdit).Setup(@SetupDBEditID))
    .AddControl(TControlInfo.Create(TDBEdit).WithWidth(250).Setup(@SetupDBEditUserName))
    .AddControl(TControlInfo.Create(TDBEdit).WithWidth(250).Setup(@SetupDBEditPassword))

    .RecalcParentHeight(25)
  ;
end;

constructor TFormUsers2.CreateNew(AOwner: TComponent);
begin
  inherited CreateNew(AOwner, TDBUser.GetInstance);
  Self.Name := 'FormUsers';
  Self.Caption := 'Usuários';
end;

procedure TFormUsers2.SetupDBEditID(AControl: TControl);
begin
  (AControl as TDBEdit).DataSource := DataSourceCRUD;
  (AControl as TDBEdit).DataField := 'ID';
end;

procedure TFormUsers2.SetupDBEditUserName(AControl: TControl);
begin
  (AControl as TDBEdit).DataSource := DataSourceCRUD;
  (AControl as TDBEdit).DataField := 'user_name';
end;

procedure TFormUsers2.SetupDBEditPassword(AControl: TControl);
begin
  (AControl as TDBEdit).DataSource := DataSourceCRUD;
  (AControl as TDBEdit).DataField := 'password';
  (AControl as TDBEdit).PasswordChar := '*';
end;

end.

