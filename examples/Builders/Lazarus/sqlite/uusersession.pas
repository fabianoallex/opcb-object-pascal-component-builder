unit UUserSession;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TUserSession }

  TUserSession = class
  private
    FLogged: Boolean;
    FLoggedAt: TDateTime;
    FUserName: string;
    FUserID: Integer;
    procedure SetLogged(AValue: Boolean);
    procedure SetLoggedAt(AValue: TDateTime);
    procedure SetUserName(AValue: string);
    procedure SetUserID(AValue: Integer);
  public
    constructor Create;
    function Login(const AUserName, APassword: string): Boolean;
    procedure Logout;
    property UserID: Integer read FUserID write SetUserID;
    property UserName: string read FUserName write SetUserName;
    property LoggedAt: TDateTime read FLoggedAt write SetLoggedAt;
    property Logged: Boolean read FLogged write SetLogged;
  end;

var
  UserSession: TUserSession;

implementation

uses
  UDBUser;

{ TUserSession }

procedure TUserSession.SetLoggedAt(AValue: TDateTime);
begin
  if FLoggedAt = AValue then Exit;
  FLoggedAt := AValue;
end;

procedure TUserSession.SetLogged(AValue: Boolean);
begin
  if FLogged = AValue then Exit;
  FLogged := AValue;
end;

procedure TUserSession.SetUserName(AValue: string);
begin
  if FUserName = AValue then Exit;
  FUserName := AValue;
end;

procedure TUserSession.SetUserID(AValue: Integer);
begin
  if FUserID = AValue then Exit;
  FUserID := AValue;
end;

function TUserSession.Login(const AUserName, APassword: string): Boolean;
var
  Id: Integer;
begin
  Result := False;
  Id := 0;

  if TDBUser.GetInstance.CheckLogin(AUserName, APassword, Id) then
  begin
    UserSession.Logout;
    Result := True;

    UserSession.Logged := True;
    UserSession.LoggedAt := Now;
    UserSession.UserID := Id;
    UserSession.UserName := AUserName;
  end;
end;

procedure TUserSession.Logout;
begin
  UserSession.Logged := False;
end;

constructor TUserSession.Create;
begin
  FLogged := False;
end;

initialization
  UserSession := TUserSession.Create;

finalization
  UserSession.Free;

end.

