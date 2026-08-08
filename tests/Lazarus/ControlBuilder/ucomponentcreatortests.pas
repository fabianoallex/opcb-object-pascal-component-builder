unit UComponentCreatorTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, Forms, Dialogs, OPCB;

type

  { TComponentCreatorTests }

  TComponentCreatorTests = class(TTestCase)
  private
    FForm: TForm;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAdd;
    procedure TestSetOwnerDirectly;
    procedure TestGetComponent;
  end;

implementation

procedure TComponentCreatorTests.SetUp;
begin
  FForm := TForm.Create(nil);
end;

procedure TComponentCreatorTests.TearDown;
begin
  FForm.Free;
end;

procedure TComponentCreatorTests.TestAdd;
var
  ComponentCreator: TComponentCreator;
  Dialog: TOpenDialog;
begin
  ComponentCreator := TComponentCreator.Create;
  try
    ComponentCreator
      .SetOwner(FForm)
      .Add(TComponentBuilder.Create(TOpenDialog, 'OpenDialog1'))
    ;

    Dialog := ComponentCreator.GetComponent('OpenDialog1') as TOpenDialog;
    AssertNotNull('Dialog não deveria ser nil', Dialog);
  finally
    ComponentCreator.Free;
  end;
end;

procedure TComponentCreatorTests.TestSetOwnerDirectly;
var
  ComponentCreator: TComponentCreator;
  Dialog: TOpenDialog;
begin
  // Regressão: usa SetOwner diretamente (não o WithOwner deprecated).
  ComponentCreator := TComponentCreator.Create;
  try
    ComponentCreator
      .SetOwner(FForm)
      .Add(TComponentBuilder.Create(TOpenDialog, 'OpenDialog1'))
    ;

    Dialog := ComponentCreator.GetComponent('OpenDialog1') as TOpenDialog;
    AssertSame('Owner do componente diferente do esperado', FForm, Dialog.Owner);
  finally
    ComponentCreator.Free;
  end;
end;

procedure TComponentCreatorTests.TestGetComponent;
var
  ComponentCreator: TComponentCreator;
begin
  ComponentCreator := TComponentCreator.Create;
  try
    ComponentCreator
      .SetOwner(FForm)
      .Add(TComponentBuilder.Create(TOpenDialog, 'OpenDialog1'))
    ;

    AssertNotNull('GetComponent<T> não deveria devolver nil',
      ComponentCreator.specialize GetComponent<TOpenDialog>('OpenDialog1'));
  finally
    ComponentCreator.Free;
  end;
end;

initialization

  RegisterTest(TComponentCreatorTests);
end.
