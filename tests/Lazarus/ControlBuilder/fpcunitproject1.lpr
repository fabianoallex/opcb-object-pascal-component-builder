program fpcunitproject1;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, UControlCreatorTests, UControlBuilderTests,
  UObjectBuilderTests, UComponentBuilderTests, UComponentRegistryTest,
  UMenuCreatorTests, UComponentCreatorTests, UButtonBuilderTests;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

