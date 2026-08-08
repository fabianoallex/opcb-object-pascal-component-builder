program OPCB.Lazarus.Sqlite;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, UFMain, UDMDatabase, UDBUser, UUserSession, UFormLogin, UFormUsers,
  UFormCRUD, UDBEntity;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  {$PUSH}{$WARN 5044 OFF}
  Application.MainFormOnTaskbar := True;
  {$POP}
  Application.Initialize;
  Application.CreateForm(TDMDatabase, DMDatabase);
  Application.CreateForm(TFMain, FMain);
  Application.Run;
end.

