unit UDMDatabase;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, sqlite3conn, sqldb, db, Forms, OPCB;

type

  { TDMDatabase }

  TDMDatabase = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    FDBConnection: TSQLite3Connection;
    FTransaction: TSQLTransaction;
    procedure CheckDatabaseStructure;
    procedure SetupConnection(AComponent: TComponent);
    procedure SetupTransaction(AComponent: TComponent);
    function GetPathDB: string;
    procedure SetDBConnection(AValue: TSQLite3Connection);
    procedure SetTransaction(AValue: TSQLTransaction);
  public
    function ExistsColumn(const ATableName, AColumnName: string): Boolean;
    function ExistsTable(const ATableName: string): Boolean;
    property DBConnection: TSQLite3Connection read FDBConnection write SetDBConnection;
    property Transaction: TSQLTransaction read FTransaction write SetTransaction;
    property PathDB: string read GetPathDB;
  end;

var
  DMDatabase: TDMDatabase;

implementation

uses
  UDBUser;

const DBPATH = 'meubanco.db';

{$R *.lfm}

{ TDMDatabase }

function TDMDatabase.ExistsColumn(const ATableName, AColumnName: string): Boolean;
var
  Q: TSQLQuery;
begin
  Result := False;
  Q := TSQLQuery.Create(nil);
  try
    Q.DataBase := DBConnection;
    Q.Transaction := DBConnection.Transaction;
    Q.SQL.Text := Format(
      'SELECT name FROM pragma_table_info(%s) WHERE name = %s',
      [QuotedStr(ATableName), QuotedStr(AColumnName)]
    );
    Q.Open;
    Result := not Q.IsEmpty;
  finally
    DBConnection.Transaction.Commit;
    Q.Free;
  end;
end;

function TDMDatabase.ExistsTable(const ATableName: string): Boolean;
var
  Q: TSQLQuery;
begin
  Result := False;
  Q := TSQLQuery.Create(nil);
  try
    Q.DataBase := DBConnection;
    Q.Transaction := DBConnection.Transaction;
    Q.SQL.Text := Format(
      'SELECT name FROM sqlite_master WHERE type=''table'' AND name=%s',
      [QuotedStr(ATableName)]
    );
    Q.Open;
    Result := not Q.IsEmpty;
  finally
    DBConnection.Transaction.Commit;
    Q.Free;
  end;
end;

procedure TDMDatabase.SetupTransaction(AComponent: TComponent);
begin
  Transaction := (AComponent as TSQLTransaction);
end;

procedure TDMDatabase.CheckDatabaseStructure;
begin
  TDBUser.GetInstance.CheckTableUsers;
end;

procedure TDMDatabase.SetupConnection(AComponent: TComponent);
begin
  DBConnection := (AComponent as TSQLite3Connection);
  DBConnection.DatabaseName := Self.PathDB;
  DBConnection.Transaction := Self.Transaction;
  DBConnection.Open;

  CheckDatabaseStructure;
end;

procedure TDMDatabase.DataModuleCreate(Sender: TObject);
var
  ComponentsBuilder: TComponentsBuilder;
begin
  ComponentsBuilder := TComponentsBuilder.Create(Self.Name);
  try
    ComponentsBuilder
      .WithOwner(Self)
      .Add(TComponentInfo.Create(TSQLTransaction).Setup(@SetupTransaction))
      .Add(TComponentInfo.Create(TSQLite3Connection).Setup(@SetupConnection))
    ;
  finally
    ComponentsBuilder.Free;
  end;
end;

procedure TDMDatabase.DataModuleDestroy(Sender: TObject);
begin
  if Assigned(DBConnection) then
    DBConnection.Free;
  if Assigned(Transaction) then
    Transaction.Free;
end;

function TDMDatabase.GetPathDB: string;
begin
  Result := ExtractFilePath(Application.ExeName) + DBPATH;
end;

procedure TDMDatabase.SetDBConnection(AValue: TSQLite3Connection);
begin
  if FDBConnection = AValue then Exit;
  FDBConnection := AValue;
end;

procedure TDMDatabase.SetTransaction(AValue: TSQLTransaction);
begin
  if FTransaction = AValue then Exit;
  FTransaction := AValue;
end;

end.

