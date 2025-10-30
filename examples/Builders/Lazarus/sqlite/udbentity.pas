unit UDBEntity;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, UDMDatabase, sqlite3conn, sqldb, db, OPCB;

type

  { TDBEntity }

  TDBEntity = class
  private
    FQueryCRUD: TSQLQuery;
    FQuerySearch: TSQLQuery;
  protected
    procedure SetupQueryCRUD(AComponent: TComponent); virtual;
    procedure SetupQuerySearch(AComponent: TComponent); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property QueryCRUD: TSQLQuery read FQueryCRUD;
    property QuerySearch: TSQLQuery read FQuerySearch;
  end;

implementation

{ TDBEntity }

procedure TDBEntity.SetupQueryCRUD(AComponent: TComponent);
begin
  Self.QueryCRUD.DataBase := DMDatabase.DBConnection;
  Self.QueryCRUD.Transaction := DMDatabase.DBConnection.Transaction;
end;

procedure TDBEntity.SetupQuerySearch(AComponent: TComponent);
begin
  Self.QuerySearch.DataBase := DMDatabase.DBConnection;
  Self.QuerySearch.Transaction := DMDatabase.DBConnection.Transaction;
end;

constructor TDBEntity.Create;
var
  Creator: TComponentCreator;
begin
  Creator := TComponentCreator.Create;
  try
    Creator
      .SetOwner(DMDatabase)
      .Add(TComponentBuilder.Create(TSQLQuery, FQueryCRUD).Setup(@SetupQueryCRUD))
      .Add(TComponentBuilder.Create(TSQLQuery, FQuerySearch).Setup(@SetupQuerySearch))
    ;
  finally
    Creator.Free;
  end;
end;

destructor TDBEntity.Destroy;
begin
  inherited Destroy;
end;

end.

