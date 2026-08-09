unit UButtonBuilderTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, Forms, StdCtrls, Controls, OPCB, OPCB.Builders;

type
  TMyCustomButton = class(TButton)
  end;

  TMyOkButton = class(TButton)
  public
    constructor Create(AOwner: TComponent); override;
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
    procedure TestConfigureObjectDoesNotOverrideModalResultWhenNotSet;
  end;

implementation

constructor TMyOkButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ModalResult := mrOk;
end;

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

procedure TButtonBuilderTests.TestConfigureObjectDoesNotOverrideModalResultWhenNotSet;
{ Regressão: ConfigureObject aplicava FModalResult incondicionalmente
  (campo Integer comum, default 0/mrNone) mesmo quando WithModalResult
  nunca foi chamado, sobrescrevendo um ModalResult que uma subclasse já
  define no próprio construtor. Corrigido usando TOptionalInteger, como o
  resto da lib faz (ex: FEnabled: TOptionalBoolean, no mesmo builder). }
var
  ControlCreator: TControlCreator;
  Button: TButton;
begin
  Button := nil;
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .WithOwnerAndParent(FForm, FForm)
      .specialize Add<TButton>(TButtonBuilder.Create(TMyOkButton, 'Btn1', Button))
      // sem WithModalResult
    ;

    AssertNotNull('Button não deveria ser nil', Button);
    AssertEquals('ModalResult definido pela subclasse não deveria ter sido sobrescrito',
      Integer(mrOk), Integer(Button.ModalResult));
  finally
    ControlCreator.Free;
  end;
end;

initialization

  RegisterTest(TButtonBuilderTests);
end.
