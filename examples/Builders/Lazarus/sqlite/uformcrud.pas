unit UFormCRUD;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, OPCB, StdCtrls, ExtCtrls, Buttons, Controls, DB,
  DBGrids, UDBEntity;

type

  { TFormCRUDBase }

  TFormCRUDBase = class(TForm)
  private
    FButtonAdd, FButtonEdit, FButtonRemove,
    FButtonPost, FButtonCancel, FButtonRefresh,
    FButtonNext, FButtonPrevious, FButtonClose: TButton;
    FDataSourceCRUD: TDataSource;
    FDBGrid: TDBGrid;
    FEntity: TDBEntity;
    procedure ButtonAddClick(ASender: TObject);
    procedure ButtonCancelClick(ASender: TObject);
    procedure ButtonCloseClick(ASender: TObject);
    procedure ButtonEditClick(ASender: TObject);
    procedure ButtonNextClick(ASender: TObject);
    procedure ButtonPostClick(ASender: TObject);
    procedure ButtonPreviousClick(ASender: TObject);
    procedure ButtonRefreshClick(ASender: TObject);
    procedure ButtonRemoveClick(ASender: TObject);
    procedure FormClose(ASender: TObject; var ACloseAction: TCloseAction);
    procedure FormShow(ASender: TObject);
    procedure DataSourceCRUDStateChange(ASender: TObject);
    procedure SetupDataSourceCRUD(AComponent: TComponent);
  protected
    procedure SetupDBGrid(AControl: TControl); virtual;
    procedure BuildContents(const ABuiler: TControlCreator); virtual; abstract;
    procedure DataSourceStateChange(Sender: TObject);
  public
    constructor CreateNew(AOwner: TComponent);
    constructor CreateNew(AOwner: TComponent; AEntity: TDBEntity);
    property ButtonAdd: TButton read FButtonAdd;
    property ButtonEdit: TButton read FButtonEdit;
    property ButtonRemove: TButton read FButtonRemove;
    property ButtonPost: TButton read FButtonPost;
    property ButtonCancel: TButton read FButtonCancel;
    property ButtonRefresh: TButton read FButtonRefresh;
    property ButtonNext: TButton read FButtonNext;
    property ButtonPrevious: TButton read FButtonPrevious;
    property ButtonClose: TButton read FButtonClose;
    property DataSourceCRUD: TDataSource read FDataSourceCRUD;
    property DBGrid: TDBGrid read FDBGrid;
    property Entity: TDBEntity read FEntity;
  end;

implementation

uses
  ComCtrls, Dialogs;

{ TFormCRUDBase }

procedure TFormCRUDBase.ButtonAddClick(ASender: TObject);
begin
  Entity.QueryCRUD.Insert;
end;

procedure TFormCRUDBase.ButtonCancelClick(ASender: TObject);
begin
  Entity.QueryCRUD.Cancel;
end;

procedure TFormCRUDBase.ButtonCloseClick(ASender: TObject);
begin
  Self.Close;
end;

procedure TFormCRUDBase.ButtonEditClick(ASender: TObject);
begin
  Entity.QueryCRUD.Edit;
end;

procedure TFormCRUDBase.ButtonNextClick(ASender: TObject);
begin
  Entity.QueryCRUD.Next;
end;

procedure TFormCRUDBase.ButtonPostClick(ASender: TObject);
begin
  try
    Entity.QueryCRUD.Post;
    Entity.QueryCRUD.ApplyUpdates;
    Entity.QueryCRUD.SQLTransaction.CommitRetaining;
  except
    on E: Exception do
    begin
      Entity.QueryCRUD.SQLTransaction.RollbackRetaining;
      raise;
    end;
  end;
end;

procedure TFormCRUDBase.ButtonPreviousClick(ASender: TObject);
begin
  Entity.QueryCRUD.Prior;
end;

procedure TFormCRUDBase.ButtonRefreshClick(ASender: TObject);
begin
  Entity.QueryCRUD.Refresh;
end;

procedure TFormCRUDBase.ButtonRemoveClick(ASender: TObject);
begin
  if MessageDlg('Deseja realmente excluir esse registro?', mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Exit;
  try
    Entity.QueryCRUD.Delete;
    Entity.QueryCRUD.ApplyUpdates;
    Entity.QueryCRUD.SQLTransaction.CommitRetaining;
  except
    on E: Exception do
    begin
      Entity.QueryCRUD.SQLTransaction.RollbackRetaining;
      raise;
    end;
  end;
end;

procedure TFormCRUDBase.FormClose(ASender: TObject;
  var ACloseAction: TCloseAction);
begin
  if Self.Parent is TTabSheet then
    Self.Parent.Free;

  ACloseAction := caFree;
end;

procedure TFormCRUDBase.FormShow(ASender: TObject);
begin
  DataSourceCRUDStateChange(DataSourceCRUD);
end;

procedure TFormCRUDBase.DataSourceCRUDStateChange(ASender: TObject);
begin
  ButtonAdd.Enabled := not (DataSourceCRUD.State in [dsInsert, dsEdit]);
  ButtonEdit.Enabled := not (DataSourceCRUD.State in [dsInsert, dsEdit]);
  ButtonRemove.Enabled := not (DataSourceCRUD.State in [dsInsert, dsEdit]);
  ButtonPost.Enabled := (DataSourceCRUD.State in [dsInsert, dsEdit]);
  ButtonCancel.Enabled := (DataSourceCRUD.State in [dsInsert, dsEdit]);
  ButtonRefresh.Enabled := not (DataSourceCRUD.State in [dsInsert, dsEdit]);
end;

procedure TFormCRUDBase.SetupDataSourceCRUD(AComponent: TComponent);
begin
  DataSourceCRUD.DataSet := Entity.QueryCRUD;
  DataSourceCRUD.OnStateChange := @DataSourceCRUDStateChange;
  DataSourceCRUD.AutoEdit := False;
end;

procedure TFormCRUDBase.SetupDBGrid(AControl: TControl);
begin
  DBGrid.Align := alClient;
  DBGrid.DataSource := Self.DataSourceCRUD;
  DBGrid.Options := DBGrid.Options + [dgRowSelect, dgAlwaysShowSelection];
end;

procedure TFormCRUDBase.DataSourceStateChange(Sender: TObject);
begin
  ButtonAdd.Enabled := not (DataSourceCRUD.State in [dsInsert, dsEdit]);
  ButtonEdit.Enabled := not (DataSourceCRUD.State in [dsInsert, dsEdit]);
  ButtonRemove.Enabled := not (DataSourceCRUD.State in [dsInsert, dsEdit]);
  ButtonPost.Enabled := (DataSourceCRUD.State in [dsInsert, dsEdit]);
  ButtonCancel.Enabled := (DataSourceCRUD.State in [dsInsert, dsEdit]);
  ButtonRefresh.Enabled := not (DataSourceCRUD.State in [dsInsert, dsEdit]);
end;

constructor TFormCRUDBase.CreateNew(AOwner: TComponent);
begin
  inherited CreateNew(AOwner);
end;

constructor TFormCRUDBase.CreateNew(AOwner: TComponent; AEntity: TDBEntity);
var
  Creators: TOPCBCreators;
begin
  inherited CreateNew(AOwner);

  Self.BorderStyle := bsNone;
  Self.WindowState := wsMaximized;
  Self.Position := poMainFormCenter;
  Self.OnClose := @FormClose;
  Self.OnShow := @FormShow;
  Self.FEntity := AEntity;

  Creators := TOPCBCreators.Create;

  try
    Creators.AsComponentCreator
      .SetOwner(Self)
      .Add(TComponentBuilder.Create(TDataSource, FDataSourceCRUD).Setup(@SetupDataSourceCRUD))
    ;

    Creators.AsControlCreator
      .SetOwnerAndParent(Self, Self)
      .SetSpace(2, 5)
      .SubLevel(TControlBuilder.Create(TPanel).WithCaption('').WithAlign(alTop))
        .SetTopLeft(5, 5)
        .Add(TControlBuilder.Create(TButton, FButtonAdd).WithCaption('Novo').WithOnClick(@ButtonAddClick))
        .Add(TControlBuilder.Create(TButton, FButtonEdit).WithCaption('Alterar').WithOnClick(@ButtonEditClick))
        .Add(TControlBuilder.Create(TButton, FButtonRemove).WithCaption('Excluir').WithOnClick(@ButtonRemoveClick))
        .Add(TControlBuilder.Create(TButton, FButtonPost).WithCaption('Gravar').WithOnClick(@ButtonPostClick))
        .Add(TControlBuilder.Create(TButton, FButtonCancel).WithCaption('Cancelar').WithOnClick(@ButtonCancelClick))
        .Add(TControlBuilder.Create(TButton, FButtonRefresh).WithCaption('Recarregar').WithOnClick(@ButtonRefreshClick))
        .IncLeft(20)
        .Add(TControlBuilder.Create(TButton, FButtonPrevious).WithCaption('Anterior').WithOnClick(@ButtonPreviousClick))
        .Add(TControlBuilder.Create(TButton, FButtonNext).WithCaption('Próximo').WithOnClick(@ButtonNextClick))
        .IncLeft(20)
        .Add(TControlBuilder.Create(TButton, FButtonClose).WithCaption('Fechar').WithOnClick(@ButtonCloseClick))
        .RecalcParentHeight(10)
      .SuperLevel
      .SubLevel(TControlBuilder.Create(TPanel).WithCaption('').WithAlign(altop))
         .External(@BuildContents) // aqui injeta a continuacao do build
      .SuperLevel
      .SubLevel(TControlBuilder.Create(TPanel).WithCaption('').WithAlign(alClient))
        .Add(TControlBuilder.Create(TDBGrid, FDBGrid).Setup(@SetupDBGrid))
      .SuperLevel
    ;

    FEntity.QueryCRUD.Open;
  finally
    Creators.Free;
  end;
end;

end.

