unit UButtonBuilderTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, Forms, StdCtrls, Controls, OPCB, OPCB.Builders;

type
  TMyCustomButton = class(TButton)
  end;

  { TButtonBuilderTests }

  TButtonBuilderTests = class(TTestCase)
  private
    FForm: TForm;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreateWithClassRespectsAClass;
    procedure TestConfigureObjectAppliesModalResultAndEnabled;
  end;

implementation

procedure TButtonBuilderTests.SetUp;
begin
  FForm := TForm.Create(nil);
end;

procedure TButtonBuilderTests.TearDown;
begin
  FForm.Free;
end;

procedure TButtonBuilderTests.TestCreateWithClassRespectsAClass;
{ Regressão: dois dos três overloads de TButtonBuilder.Create(AClass, ...)
  descartavam o parâmetro AClass e sempre criavam TButton, mesmo quando
  outra TButtonClass era explicitamente pedida. }
var
  ControlCreator: TControlCreator;
  Button: TButton;
begin
  Button := nil;
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .specialize Add<TButton>(TButtonBuilder.Create(TMyCustomButton, 'Btn1', Button))
    ;

    AssertNotNull('Button não deveria ser nil', Button);
    AssertTrue('Deveria ter criado um TMyCustomButton, não um TButton genérico',
      Button is TMyCustomButton);
  finally
    ControlCreator.Free;
  end;
end;

procedure TButtonBuilderTests.TestConfigureObjectAppliesModalResultAndEnabled;
{ Regressão: TButtonBuilder não tinha override de ConfigureObject, então
  WithModalResult/WithEnabled gravavam campos que nunca chegavam a ser
  aplicados ao TButton criado. }
var
  ControlCreator: TControlCreator;
  Button: TButton;
begin
  Button := nil;
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .specialize Add<TButton>(
        TButtonBuilder.Create(TButton, 'Btn1', Button)
          .WithModalResult(mrOk)
          .WithEnabled(False)
      )
    ;

    AssertNotNull('Button não deveria ser nil', Button);
    AssertEquals('ModalResult diferente do esperado', Integer(mrOk), Integer(Button.ModalResult));
    AssertFalse('Enabled deveria ser False', Button.Enabled);
  finally
    ControlCreator.Free;
  end;
end;

initialization

  RegisterTest(TButtonBuilderTests);
end.
