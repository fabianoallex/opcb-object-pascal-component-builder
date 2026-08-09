unit UComponentCreatorTests;

interface

uses
  DUnitX.TestFramework, Vcl.Forms, Vcl.Dialogs, OPCB;

type
  [TestFixture]
  TComponentCreatorTest = class
  private
    FForm: TForm;
    procedure ExternalMethod(const ACreator: TComponentCreator);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test] procedure TestAdd;
    [Test] procedure TestSetOwnerDirectly;
    [Test] procedure TestGetComponent;
    [Test] procedure TestAddComponentBuilderIsFreedNotLeaked;
    [Test] procedure TestExternalObjProc;
    [Test] procedure TestExternalProc;
    [Test] procedure TestWithOwnerDeprecated;
  end;

implementation

type
  TCountingComponentBuilder = class(TComponentBuilder)
  public
    destructor Destroy; override;
  end;

var
  GComponentBuilderDestroyCount: Integer = 0;

destructor TCountingComponentBuilder.Destroy;
begin
  Inc(GComponentBuilderDestroyCount);
  inherited;
end;

procedure TComponentCreatorTest.ExternalMethod(const ACreator: TComponentCreator);
begin
  ACreator.Add(TComponentBuilder.Create(TOpenDialog, 'ExternalMethodDialog'));
end;

procedure ExternalComponentCreatorProc(const ACreator: TComponentCreator);
begin
  ACreator.Add(TComponentBuilder.Create(TOpenDialog, 'ExternalProcDialog'));
end;

procedure TComponentCreatorTest.Setup;
begin
  FForm := TForm.Create(nil);
end;

procedure TComponentCreatorTest.TearDown;
begin
  FForm.Free;
end;

procedure TComponentCreatorTest.TestAdd;
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
    Assert.IsNotNull(Dialog, 'Dialog nao deveria ser nil');
  finally
    ComponentCreator.Free;
  end;
end;

procedure TComponentCreatorTest.TestSetOwnerDirectly;
var
  ComponentCreator: TComponentCreator;
  Dialog: TOpenDialog;
begin
  // Regressao: usa SetOwner diretamente (nao o WithOwner deprecated).
  ComponentCreator := TComponentCreator.Create;
  try
    ComponentCreator
      .SetOwner(FForm)
      .Add(TComponentBuilder.Create(TOpenDialog, 'OpenDialog1'))
    ;

    Dialog := ComponentCreator.GetComponent('OpenDialog1') as TOpenDialog;
    Assert.IsTrue(FForm = Dialog.Owner, 'Owner do componente diferente do esperado');
  finally
    ComponentCreator.Free;
  end;
end;

procedure TComponentCreatorTest.TestGetComponent;
var
  ComponentCreator: TComponentCreator;
begin
  ComponentCreator := TComponentCreator.Create;
  try
    ComponentCreator
      .SetOwner(FForm)
      .Add(TComponentBuilder.Create(TOpenDialog, 'OpenDialog1'))
    ;

    Assert.IsNotNull(ComponentCreator.GetComponent<TOpenDialog>('OpenDialog1'),
      'GetComponent<T> nao deveria devolver nil');
  finally
    ComponentCreator.Free;
  end;
end;

procedure TComponentCreatorTest.TestAddComponentBuilderIsFreedNotLeaked;
var
  ComponentCreator: TComponentCreator;
begin
  // Simetria com TestAddMenuBuilderIsFreedNotLeaked (UMenuCreatorTests.pas):
  // o Creator assume posse do builder em Add e deve libera-lo exatamente
  // uma vez.
  GComponentBuilderDestroyCount := 0;
  ComponentCreator := TComponentCreator.Create;
  try
    ComponentCreator
      .SetOwner(FForm)
      .Add(TCountingComponentBuilder.Create(TOpenDialog, 'OpenDialog1'))
    ;

    Assert.AreEqual(1, GComponentBuilderDestroyCount, 'O builder do componente deveria ter sido liberado exatamente uma vez');
  finally
    ComponentCreator.Free;
  end;
end;

procedure TComponentCreatorTest.TestExternalObjProc;
var
  ComponentCreator: TComponentCreator;
begin
  ComponentCreator := TComponentCreator.Create;
  try
    ComponentCreator
      .SetOwner(FForm)
      .External(ExternalMethod)
    ;

    Assert.IsNotNull(ComponentCreator.GetComponent('ExternalMethodDialog'),
      'Componente criado dentro do External (of object) nao deveria ser nil');
  finally
    ComponentCreator.Free;
  end;
end;

procedure TComponentCreatorTest.TestExternalProc;
var
  ComponentCreator: TComponentCreator;
begin
  ComponentCreator := TComponentCreator.Create;
  try
    ComponentCreator
      .SetOwner(FForm)
      .External(ExternalComponentCreatorProc)
    ;

    Assert.IsNotNull(ComponentCreator.GetComponent('ExternalProcDialog'),
      'Componente criado dentro do External (procedure simples) nao deveria ser nil');
  finally
    ComponentCreator.Free;
  end;
end;

procedure TComponentCreatorTest.TestWithOwnerDeprecated;
var
  ComponentCreator: TComponentCreator;
  Dialog: TOpenDialog;
begin
  ComponentCreator := TComponentCreator.Create;
  try
    ComponentCreator
      .WithOwner(FForm)
      .Add(TComponentBuilder.Create(TOpenDialog, 'OpenDialog1'))
    ;

    Dialog := ComponentCreator.GetComponent('OpenDialog1') as TOpenDialog;
    Assert.IsTrue(FForm = Dialog.Owner, 'Owner do componente diferente do esperado');
  finally
    ComponentCreator.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TComponentCreatorTest);

end.
