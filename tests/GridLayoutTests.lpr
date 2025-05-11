program GridLayoutTests;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner,
  ULayout in '../src/Ulayout.pas',
  UGridLayoutTest;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

