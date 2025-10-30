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
    procedure BuildContents(const ACreator: TControlCreator); override;
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

procedure TFormUsers2.BuildContents(const ACreator: TControlCreator);
begin
  ACreator
    .SetTopLeft(5, 5)
    .IncTopLeft(10, 10)
    .SetDirection(cpdVertical)
    .SetVerticalSpace(5)
    .Add(TControlBuilder.Create(TLabel).WithCaption('ID').WithWidth(50))
    .Add(TControlBuilder.Create(TLabel).WithCaption('Nome').WithWidth(50))
    .Add(TControlBuilder.Create(TLabel).WithCaption('Senha').WithWidth(50))
    .Break
    .Add(TControlBuilder.Create(TDBEdit).Setup(@SetupDBEditID))
    .Add(TControlBuilder.Create(TDBEdit).WithWidth(250).Setup(@SetupDBEditUserName))
    .Add(TControlBuilder.Create(TDBEdit).WithWidth(250).Setup(@SetupDBEditPassword))
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

