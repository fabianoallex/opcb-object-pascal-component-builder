program DelphiProject;

uses
  Vcl.Forms,
  UMainForm in 'UMainForm.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);  // TMainForm does not have an associated .dfm
  Application.Run;
end.
