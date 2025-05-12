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
  ULayout in '../src/ULayout.pas',
  UGridLayoutBuilder in '../src/UGridLayoutBuilder.pas',
  UGridLayoutFillerFactory in '../src/UGridLayoutFillerFactory.pas',
  UGridLayoutFill in '../src/UGridLayoutFill.pas',
  UGridaLayoutResizer in '../src/UGridaLayoutResizer.pas',
  Forms, UExamplesMain,
  UFColumnResizerGridLayout,
  UFRowResizerGridLayout,
  uffullresizergridlayout;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TFExamplesMain, FExamplesMain);
  Application.CreateForm(TFColumnResizerGridLayout, FColumnResizerGridLayout);
  Application.CreateForm(TFRowResizerGridLayout, FRowResizerGridLayout);
  Application.CreateForm(TFFullResizerGridLayout, FFullResizerGridLayout);
  Application.Run;
end.

