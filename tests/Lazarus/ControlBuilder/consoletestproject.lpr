program consoletestproject;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, consoletestrunner, UControlCreatorTests, UControlBuilderTests,
  UObjectBuilderTests, UComponentBuilderTests, UComponentRegistryTest,
  UMenuCreatorTests, UComponentCreatorTests, UButtonBuilderTests;

var
  Application: TTestRunner;

begin
  // Initializes the LCL widgetset (window classes), so tests that need a
  // handle (HandleNeeded, a real ClientRect) work in the console runner.
  Forms.Application.Initialize;
  Application := TTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'OPCB console test runner';
  Application.Run;
  Application.Free;
end.
