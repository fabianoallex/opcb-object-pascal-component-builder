unit UButtonBuilderTests;

interface

uses
  System.Classes, DUnitX.TestFramework, Vcl.Forms, Vcl.StdCtrls, Vcl.Controls, OPCB, OPCB.Builders;

type
  TMyCustomButton = class(TButton)
  end;

  TMyOkButton = class(TButton)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  [TestFixture]
  TButtonBuilderTest = class
  private
    FForm: TForm;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test] procedure TestCreateWithClassRespectsAClass;
    [Test] procedure TestConfigureObjectAppliesModalResultAndEnabled;
    [Test] procedure TestConfigureObjectDoesNotOverrideModalResultWhenNotSet;
  end;

implementation

constructor TMyOkButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ModalResult := mrOk;
end;

procedure TButtonBuilderTest.Setup;
begin
  FForm := TForm.Create(nil);
end;

procedure TButtonBuilderTest.TearDown;
begin
  FForm.Free;
end;

procedure TButtonBuilderTest.TestCreateWithClassRespectsAClass;
// Regressao: dois dos tres overloads de TButtonBuilder.Create(AClass, ...)
// descartavam o parametro AClass e sempre criavam TButton, mesmo quando
// outra TButtonClass era explicitamente pedida.
var
  ControlCreator: TControlCreator;
  Button: TButton;
begin
  Button := nil;
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .Add<TButton>(TButtonBuilder.Create(TMyCustomButton, 'Btn1', Button))
    ;

    Assert.IsNotNull(Button, 'Button nao deveria ser nil');
    Assert.IsTrue(Button is TMyCustomButton,
      'Deveria ter criado um TMyCustomButton, nao um TButton generico');
  finally
    ControlCreator.Free;
  end;
end;

procedure TButtonBuilderTest.TestConfigureObjectAppliesModalResultAndEnabled;
// Regressao: TButtonBuilder nao tinha override de ConfigureObject, entao
// WithModalResult/WithEnabled gravavam campos que nunca chegavam a ser
// aplicados ao TButton criado.
var
  ControlCreator: TControlCreator;
  Button: TButton;
begin
  Button := nil;
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .Add<TButton>(
        TButtonBuilder.Create(TButton, 'Btn1', Button)
          .WithModalResult(mrOk)
          .WithEnabled(False)
      )
    ;

    Assert.IsNotNull(Button, 'Button nao deveria ser nil');
    Assert.AreEqual(Integer(mrOk), Integer(Button.ModalResult), 'ModalResult diferente do esperado');
    Assert.IsFalse(Button.Enabled, 'Enabled deveria ser False');
  finally
    ControlCreator.Free;
  end;
end;

procedure TButtonBuilderTest.TestConfigureObjectDoesNotOverrideModalResultWhenNotSet;
// Regressao: ConfigureObject aplicava FModalResult incondicionalmente
// (campo Integer comum, default 0/mrNone) mesmo quando WithModalResult
// nunca foi chamado, sobrescrevendo um ModalResult que uma subclasse ja
// define no proprio construtor. Corrigido usando TOptionalInteger, como o
// resto da lib faz (ex: FEnabled: TOptionalBoolean, no mesmo builder).
var
  ControlCreator: TControlCreator;
  Button: TButton;
begin
  Button := nil;
  ControlCreator := TControlCreator.Create;
  try
    ControlCreator
      .SetOwnerAndParent(FForm, FForm)
      .Add<TButton>(TButtonBuilder.Create(TMyOkButton, 'Btn1', Button))
      // sem WithModalResult
    ;

    Assert.IsNotNull(Button, 'Button nao deveria ser nil');
    Assert.AreEqual(Integer(mrOk), Integer(Button.ModalResult),
      'ModalResult definido pela subclasse nao deveria ter sido sobrescrito');
  finally
    ControlCreator.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TButtonBuilderTest);

end.
