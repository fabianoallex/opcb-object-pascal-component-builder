unit UMainForm;

{ This file can be compiled in Delphi (VCL) and Lazarus. }

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  {$IFDEF FPC}
  Forms, Classes, StdCtrls,
  {$ELSE}
  Forms, System.Classes, Vcl.StdCtrls,
  {$ENDIF}
  OPCB;

type
  {
    This TMainForm will be automatically instantiated by the Application
    at startup through the following call:

      Application.CreateForm(TMainForm, MainForm);

    Since CreateForm internally uses the default TForm.Create constructor,
    we must override it to redirect creation to CreateNew instead.
    The CreateNew method does not load any resources from a .dfm file
    (which does not exist in this case), allowing the form to be
    created entirely at runtime.
  }
  TMainForm = class(TForm)
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  MainForm: TMainForm;

implementation

{ TMainForm }

constructor TMainForm.Create(AOwner: TComponent);
var
  Creator: TControlCreator;
begin
  // If we called inherited Create, the constructor would attempt
  // to load the associated .dfm file, which does not exist in this case.

  inherited CreateNew(AOwner);
  WindowState := wsMaximized;

  Self.Caption := 'I am the main Form. I can be compiled in Delphi or Lazarus.';

  Creator := TControlCreator.Create;

  try
    Creator
      .SetOwnerAndParent(Self, Self)
      .SetTopLeft(10, 10)
      .SetSpace(5, 5)
      .Add(TControlBuilder.Create(TButton).WithCaption('Button 1'))
      .Add(TControlBuilder.Create(TButton).WithCaption('Button 2'))
      .Add(TControlBuilder.Create(TButton).WithCaption('Button 3'))
      .Add(TControlBuilder.Create(TButton).WithCaption('Button 4'))
      .Add(TControlBuilder.Create(TButton).WithCaption('Button 5'))
    ;
  finally
    Creator.Free;
  end;
end;

end.
