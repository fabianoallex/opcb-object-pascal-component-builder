{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pckgridlayout;

{$warn 5023 off : no warning about unused units}
interface

uses
  GridLayoutComponent, UGridLayoutFill, UGridLayoutFillerFactory, 
  ControlPropertyEditor, HtmlBuilder, UGridaLayoutResizer, UGridHtml, 
  UGridItemFactory, UGridLayoutBuilder, UGridText, ulayout.controls, ULayout, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('GridLayoutComponent', @GridLayoutComponent.Register);
end;

initialization
  RegisterPackage('pckgridlayout', @Register);
end.
