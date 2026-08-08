unit UObjectBuilderTests;

interface

uses
  DUnitX.TestFramework, OPCB;

type
  TObjectDescendent = class
  public
    FFoo: Integer;
    FBar: Integer;
  published
    property Foo: Integer read FFoo write FFoo;
    property Bar: Integer read FBar write FBar;
  end;

  [TestFixture]
  TObjectBuilderTest = class
  protected
    procedure SetupObject_1(AObject: TObject);
    procedure SetupObject_2(AObject: TObject);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test] procedure TestObjectBuilder;
    [Test] procedure TestObjectBuilderDescendent;
    [Test] procedure TestObjectBuilderTPanel;
    [Test] procedure TestObjectBuilderAssignsReference;
    [Test] procedure TestObjectBuilderAssignsMultiReferences;
    [Test] procedure TestObjectBuilderMultiplesBuilds;
    [Test] procedure TestObjectBuilderMultiplesBuildsWithReferences;
    [Test] procedure TestObjectBuilderResetReferences;
    [Test] procedure TestObjectBuilderSetup;
    [Test] procedure TestObjectBuilderMultipleSetups;
    [Test] procedure TestObjectBuilderWithProp;
  end;

implementation

uses
  Vcl.ExtCtrls;

procedure TObjectBuilderTest.Setup;
begin
end;

procedure TObjectBuilderTest.SetupObject_1(AObject: TObject);
var
  O: TObjectDescendent;
begin
  O := (AObject as TObjectDescendent);
  O.FFoo := 188;
end;

procedure TObjectBuilderTest.SetupObject_2(AObject: TObject);
var
  O: TObjectDescendent;
begin
  O := (AObject as TObjectDescendent);
  O.FBar := 456;
end;

procedure TObjectBuilderTest.TearDown;
begin
end;

procedure TObjectBuilderTest.TestObjectBuilder;
var
  Builder: TObjectBuilder;
  Obj: TObject;
begin
  Obj := nil;
  Builder := TObjectBuilder.Create;
  try
    Obj := Builder.Build;
    Assert.IsNotNull(Obj, 'Obj não deveria ser nil');
  finally
    Obj.Free;
  end;
end;

procedure TObjectBuilderTest.TestObjectBuilderAssignsMultiReferences;
var
  Builder: TObjectBuilder;
  Object_1, Object_2, Object_3, Object_Build: TObject;
begin
  Object_1 := nil;
  Object_2 := nil;
  Object_3 := nil;
  Object_Build := nil;

  Builder := TObjectBuilder.Create(TObject, Object_1);
  Builder.Assign(Object_2);
  Builder.Assign(Object_3);

  try
    Object_Build := Builder.Build; // capturando retorno do Build

    // Validar não-nil
    Assert.IsNotNull(Object_1, 'Object_1 não deveria ser nil');
    Assert.IsNotNull(Object_2, 'Object_2 não deveria ser nil');
    Assert.IsNotNull(Object_3, 'Object_3 não deveria ser nil');
    Assert.IsNotNull(Object_Build, 'Object_Build não deveria ser nil');

    // Validar classe exata
    Assert.AreEqual(TObject, Object_1.ClassType, 'Object_1 deveria ser TObject');
    Assert.AreEqual(TObject, Object_2.ClassType, 'Object_2 deveria ser TObject');
    Assert.AreEqual(TObject, Object_3.ClassType, 'Object_3 deveria ser TObject');
    Assert.AreEqual(TObject, Object_Build.ClassType, 'Object_Build deveria ser TObject');

    // Validar que todas apontam para a mesma instância
    Assert.IsTrue(
      (Object_1 = Object_2) and
      (Object_2 = Object_3) and
      (Object_3 = Object_Build),
      'Todas as referências devem apontar para o mesmo objeto'
    );

  finally
    Object_1.Free; // apenas uma vez, pois todas são a mesma instância
  end;
end;
procedure TObjectBuilderTest.TestObjectBuilderAssignsReference;
var
  Builder: TObjectBuilder;
  Obj: TObject;
begin
  Obj := nil;
  Builder := TObjectBuilder.Create(Obj);
  try
    Builder.Build;
    Assert.IsNotNull(Obj, 'Obj não deveria ser nil');
  finally
    Obj.Free;
  end;
end;

procedure TObjectBuilderTest.TestObjectBuilderDescendent;
var
  Builder: TObjectBuilder;
  Obj: TObjectDescendent;
begin
  Obj := nil;
  Builder := TObjectBuilder.Create(TObjectDescendent);
  try
    Obj := Builder.Build as TObjectDescendent;
    Assert.isNotNull(Obj, 'Obj não deveria ser nil');
    Assert.AreEqual(
      TObjectDescendent, Obj.ClassType,
      'Obj deveria ser exatamente da classe TObjectDescendent'
      );
  finally
    Obj.Free;
  end;
end;

procedure TObjectBuilderTest.TestObjectBuilderMultiplesBuilds;
var
  Builder: TObjectBuilder;
  Object1, Object2: TObject;
begin
  Builder := TObjectBuilder.Create(TObject);

  Object1 := Builder.Build;
  Object2 := Builder.Build;

  Assert.isNotNull(Object1);
  Assert.isNotNull(Object2);
  Assert.isTrue(Object1 <> Object2, 'Cada Build deve retornar uma nova instância');

  Object1.Free;
  Object2.Free;
  Builder.Free;
end;

procedure TObjectBuilderTest.TestObjectBuilderMultiplesBuildsWithReferences;
var
  Builder: TObjectBuilder;
  FirstInstance, Object_1, Object_2: TObject;
  Object_Build: TObject;
begin
  Object_1 := nil;
  Object_2 := nil;

  Builder := TObjectBuilder.Create(TObjectDescendent, Object_1);
  Builder.Assign(Object_2);

  // primeira build
  Object_Build := Builder.Build;
  Assert.isNotNull(Object_Build);
  Assert.isNotNull(Object_1);
  Assert.isNotNull(Object_2);
  Assert.isTrue(
    (Object_Build = Object_1) and (Object_1 = Object_2),
    'Todas as referências devem apontar para a mesma instância'
  );

  // salvar referência da primeira instância
  FirstInstance := Object_Build;

  // segunda build
  Object_Build := Builder.Build;
  Assert.isNotNull(Object_Build);
  Assert.isNotNull(Object_1);
  Assert.isNotNull(Object_2);

  // validar que é uma nova instância
  Assert.isTrue(Object_Build <> FirstInstance, 'Cada Build deve retornar uma nova instância');

  // validar que todas as referências foram atualizadas
  Assert.isTrue(
    (Object_Build = Object_1) and (Object_1 = Object_2),
    'Todas as referências devem apontar para a nova instância',
  );
end;

procedure TObjectBuilderTest.TestObjectBuilderMultipleSetups;
var
  Builder: TObjectBuilder;
  Obj: TObjectDescendent;
begin
  Obj := nil;
  Builder := TObjectBuilder.Create(TObjectDescendent);
  Builder.Setup(SetupObject_1);
  Builder.Setup(SetupObject_2);
  try
    Obj := Builder.Build as TObjectDescendent;
    Assert.IsNotNull(Obj, 'Obj não deveria ser nil');
    Assert.AreEqual(188, Obj.FFoo, 'Propriedade FFoo de Obj diferente da esperada');
    Assert.AreEqual(456, Obj.FBar, 'Propriedade FBar de Obj diferente da esperada');
  finally
    Obj.Free;
  end;
end;

procedure TObjectBuilderTest.TestObjectBuilderResetReferences;
var
  Builder: TObjectBuilder;
  Object_1, Object_2, Object_3: TObject;
begin
  Object_1 := nil;
  Object_2 := nil;
  Object_3 := nil;

  Builder := TObjectBuilder.Create(TObject, Object_1);

  Builder.Assign(Object_2).Build;

  // Resetar referências e usar novas
  Builder.ResetReferences;
  Builder.Assign(Object_3).Build;

  // validar que a primeira build não foi sobrescrita
  Assert.IsNotNull(Object_1, 'Object_1 não deveria ser nil');
  Assert.IsNotNull(Object_2, 'Object_2 não deveria ser nil');

  // validar que a segunda build foi atribuída apenas à nova referência
  Assert.IsNotNull(Object_3, 'Object_3 não deveria ser nil');
  Assert.IsTrue(Object_3 <> Object_1, 'Object_3 deveria ser diferente de Object_1');
  Assert.IsTrue(Object_3 <> Object_2, 'Object_3 deveria ser diferente de Object_2');
end;

procedure TObjectBuilderTest.TestObjectBuilderSetup;
var
  Builder: TObjectBuilder;
  Obj: TObjectDescendent;
begin
  Obj := nil;
  Builder := TObjectBuilder.Create(TObjectDescendent);
  Builder.Setup(SetupObject_1);
  try
    Obj := Builder.Build as TObjectDescendent;
    Assert.IsNotNull(Obj, 'Obj não deveria ser nil');
    Assert.AreEqual(188, Obj.FFoo, 'Propriedade FFoo de Obj diferente da esperada');
  finally
    Obj.Free;
  end;
end;

procedure TObjectBuilderTest.TestObjectBuilderTPanel;
var
  Builder: TObjectBuilder;
  Obj: TPanel;
begin
  Obj := nil;
  Builder := TObjectBuilder.Create(TPanel);
  try
    Obj := Builder.Build as TPanel; // vai criar o Panel usando o construtor sem parametro;
    Assert.isNotNull(Obj, 'Obj não deveria ser nil');
    Assert.AreEqual(
      TPanel, Obj.ClassType,
      'Obj deveria ser exatamente da classe TPanel');
  finally
    Obj.Free;
  end;
end;

procedure TObjectBuilderTest.TestObjectBuilderWithProp;
var
  Builder: TObjectBuilder;
  Obj: TObjectDescendent;
begin
  Obj := nil;
  Builder := TObjectBuilder.Create(TObjectDescendent);
  Builder.WithProp('Foo', 251); // altera via rtti
  Builder.WithProp('Bar', 938);  // altera via rtti
  try
    Obj := Builder.Build as TObjectDescendent;  // auto free Builder
    Assert.IsNotNull(Obj, 'Obj não deveria ser nil');
    Assert.AreEqual(251, Obj.Foo, 'Propriedade Foo de Obj diferente da esperada');
    Assert.AreEqual(938, Obj.Bar, 'Propriedade Bar de Obj diferente da esperada');
  finally
    Obj.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TObjectBuilderTest);

end.
