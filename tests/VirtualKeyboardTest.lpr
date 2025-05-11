program VirtualKeyboardTest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, UVirtualKeyboardClasses,
  UVirtualKeyBlockGridLayoutTest, UVirtualKeyBlockLayouts, ULayout,
  UGridLayoutTest;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

