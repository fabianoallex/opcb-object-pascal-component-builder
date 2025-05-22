program GridLayoutExamples;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  ULayout, UGridLayoutBuilder, UGridLayoutFillerFactory, UGridLayoutFill,
  UGridaLayoutResizer, Forms, lazcontrols, UExamplesMain,
  UFColumnResizerGridLayout, UFRowResizerGridLayout, uffullresizergridlayout,
  UFColunaBotoes, UGridText, UFGridText, UGridItemFactory, UGridHtml,
  UFGridHtmlTable;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TFExamplesMain, FExamplesMain);
  Application.CreateForm(TFColumnResizerGridLayout, FColumnResizerGridLayout);
  Application.CreateForm(TFRowResizerGridLayout, FRowResizerGridLayout);
  Application.CreateForm(TFFullResizerGridLayout, FFullResizerGridLayout);
  Application.CreateForm(TFColunaBotoes, FColunaBotoes);
  Application.CreateForm(TFGridText, FGridText);
  Application.CreateForm(TFGridHtmlTable, FGridHtmlTable);
  Application.Run;
end.

