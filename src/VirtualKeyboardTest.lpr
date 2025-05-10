program VirtualKeyboardTest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, UKeyTest, UVirtualKeyboardClasses,
  UVirtualKeyBlockGridLayoutTest, UVirtualKeyBlockLayouts, ULayout,
  UGridLayoutTest;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

