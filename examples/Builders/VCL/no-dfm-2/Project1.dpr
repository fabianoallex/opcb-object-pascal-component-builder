program Project1;

uses
  Vcl.Forms,
  OPCB,
  Vcl.StdCtrls;

var
  MainForm: TForm;

procedure ConfigMainForm;
var
  Creator: TControlCreator;
begin
  MainForm.Caption := 'I am the Main Form';

  Creator := TControlCreator.Create;
  try
    Creator
      .SetOwnerAndParent(MainForm, MainForm)
      .SetSpace(5, 5)
      .SetTopLeft(10, 10)
      .Add(TControlBuilder.Create(TButton).WithCaption('Button 1'))
      .Add(TControlBuilder.Create(TButton).WithCaption('Button 2'))
      .Add(TControlBuilder.Create(TButton).WithCaption('Button 3'))
      .Add(TControlBuilder.Create(TButton).WithCaption('Button 4'))
    ;
  finally
    Creator.Free;
  end;
end;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm, MainForm);
  ConfigMainForm;
  Application.Run;
end.
