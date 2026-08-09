unit UButtonBuilderTests;

interface

uses
  DUnitX.TestFramework, Vcl.Forms, Vcl.StdCtrls, Vcl.Controls, OPCB, OPCB.Builders;

type
  TMyCustomButton = class(TButton)
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
  end;

implementation

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

initialization
  TDUnitX.RegisterTestFixture(TButtonBuilderTest);

end.
