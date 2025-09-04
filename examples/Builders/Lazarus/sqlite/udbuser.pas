unit UDBUser;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, UDMDatabase, sqlite3conn, sqldb, db, OPCB, UDBEntity;

type

  { TDBUser }

  TDBUser = class(TDBEntity)
  private
    class var FInstance: TDBUser;
  public
    class function GetInstance: TDBUser;
  protected
    procedure SetupQuerySearch(AComponent: TComponent); override;
    procedure SetupQueryCRUD(AComponent: TComponent); override;
  public
    procedure CheckTableUsers;
    function ExistsUsers: Boolean;
    function CheckLogin(const AUserName, APassword: string; var UserId: Integer): boolean;
  end;

implementation

{ TDBUser }

procedure TDBUser.SetupQueryCRUD(AComponent: TComponent);
begin
  inherited SetupQueryCRUD(AComponent);
  Self.QueryCRUD.SQL.Add('select * from users where (:ID is null or ID = :ID)');
end;

procedure TDBUser.SetupQuerySearch(AComponent: TComponent);
begin
  inherited SetupQueryCRUD(AComponent);
  Self.QuerySearch.SQL.Add(
    'select * from users where (:user_name is null or user_name = :user_name )'
  );
end;

class function TDBUser.GetInstance: TDBUser;
begin
  if not Assigned(FInstance) then
    FInstance := TDBUser.Create;
  Result := FInstance;
end;

procedure TDBUser.CheckTableUsers;
begin
  if not DMDatabase.ExistsTable('users') then
  begin
    try
      DMDatabase.Transaction.StartTransaction;
      DMDatabase.DBConnection.ExecuteDirect(
        'CREATE TABLE IF NOT EXISTS users ('+
        'id INTEGER PRIMARY KEY AUTOINCREMENT,'+
        'user_name VARCHAR(50) NOT NULL,'+
        'password VARCHAR(100) NOT NULL)'
      );
      DMDatabase.Transaction.Commit;
    except
      DMDatabase.Transaction.Rollback;
    end;
  end;

  if not ExistsUsers then
  begin
    try
      DMDatabase.Transaction.StartTransaction;
      DMDatabase.DBConnection.ExecuteDirect(
        'INSERT INTO users (user_name, password) VALUES (''admin'', ''admin'')'
      );
      DMDatabase.Transaction.Commit;
    except
      DMDatabase.Transaction.Rollback;
    end;
  end;
end;

function TDBUser.ExistsUsers: Boolean;
var
  Q: TSQLQuery;
begin
  Result := False;
  Q := TSQLQuery.Create(nil);
  try
    Q.DataBase := DMDatabase.DBConnection;
    Q.Transaction := DMDatabase.DBConnection.Transaction;
    Q.SQL.Text := 'SELECT ID from users';
    Q.Open;
    Result := not Q.IsEmpty;
  finally
    DMDatabase.DBConnection.Transaction.Commit;
    Q.Free;
  end;
end;

function TDBUser.CheckLogin(const AUserName, APassword: string; var UserId: Integer): boolean;
var
  Q: TSQLQuery;
begin
  Result := False;
  Q := TSQLQuery.Create(nil);
  try
    Q.DataBase := DMDatabase.DBConnection;
    Q.Transaction := DMDatabase.Transaction;
    Q.SQL.Text :=
      'SELECT id FROM users '+
      'WHERE user_name = :user_name AND password = :password';
    Q.ParamByName('user_name').AsString := AUserName;
    Q.ParamByName('password').AsString := APassword;
    Q.Open;
    Result := not Q.IsEmpty;

    if Result then
      UserId := Q.FieldByName('id').AsInteger;
  finally
    Q.Free;
  end;
end;

initialization
  TDBUser.FInstance := nil;

end.

