program consoletestproject;

{$mode objfpc}{$H+}

uses
  Interfaces, consoletestrunner, UControlCreatorTests, UControlBuilderTests,
  UObjectBuilderTests, UComponentBuilderTests, UComponentRegistryTest,
  UMenuCreatorTests, UComponentCreatorTests, UButtonBuilderTests;

var
  Application: TTestRunner;

begin
  Application := TTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'OPCB console test runner';
  Application.Run;
  Application.Free;
end.
