unit UObjectBuilderTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, OPCB;

type
  TObjectDescendent = class
  public
    FFoo: Integer;
    FBar: Integer;
  published
    property Foo: Integer read FFoo write FFoo;
    property Bar: Integer read FBar write FBar;
  end;

  { TObjectBuilderTests }

  TObjectBuilderTests = class(TTestCase)
  private
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure SetupObject_1(AObject: TObject);
    procedure SetupObject_2(AObject: TObject);
  published
    procedure TestObjectBuilder;
    procedure TestObjectBuilderDescendent;
    procedure TestObjectBuilderTPanel;
    procedure TestObjectBuilderAssignsReference;
    procedure TestObjectBuilderAssignsMultiReferences;
    procedure TestObjectBuilderMultiplesBuilds;
    procedure TestObjectBuilderMultiplesBuildsWithReferences;
    procedure TestObjectBuilderResetReferences;
    procedure TestObjectBuilderSetup;
    procedure TestObjectBuilderMultipleSetups;
    procedure TestObjectBuilderWithProp;
  end;

implementation

uses
  ExtCtrls;

procedure TObjectBuilderTests.TestObjectBuilder;
var
  Builder: TObjectBuilder;
  Obj: TObject;
begin
  Obj := nil;
  Builder := TObjectBuilder.Create;
  try
    Obj := Builder.Build;
    AssertNotNull('Obj não deveria ser nil', Obj);
  finally
    Obj.Free;
  end;
end;

procedure TObjectBuilderTests.TestObjectBuilderDescendent;
var
  Builder: TObjectBuilder;
  Obj: TObjectDescendent;
begin
  Obj := nil;
  Builder := TObjectBuilder.Create(TObjectDescendent);
  try
    Obj := Builder.Build as TObjectDescendent;
    AssertNotNull('Obj não deveria ser nil', Obj);
    AssertEquals(
      'Obj deveria ser exatamente da classe TObjectDescendent',
      TObjectDescendent, Obj.ClassType);
  finally
    Obj.Free;
  end;
end;

procedure TObjectBuilderTests.TestObjectBuilderTPanel;
var
  Builder: TObjectBuilder;
  Obj: TPanel;
begin
  Obj := nil;
  Builder := TObjectBuilder.Create(TPanel);
  try
    Obj := Builder.Build as TPanel; // vai criar o Panel usando o construtor sem parametro;
    AssertNotNull('Obj não deveria ser nil', Obj);
    AssertEquals(
      'Obj deveria ser exatamente da classe TPanel',
      TPanel, Obj.ClassType);
  finally
    Obj.Free;
  end;
end;

procedure TObjectBuilderTests.TestObjectBuilderAssignsReference;
var
  Builder: TObjectBuilder;
  Obj: TObject;
begin
  Obj := nil;
  Builder := TObjectBuilder.Create(Obj);
  try
    Builder.Build;
    AssertNotNull('Obj não deveria ser nil', Obj);
  finally
    Obj.Free;
  end;
end;

procedure TObjectBuilderTests.TestObjectBuilderAssignsMultiReferences;
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
    AssertNotNull('Object_1 não deveria ser nil', Object_1);
    AssertNotNull('Object_2 não deveria ser nil', Object_2);
    AssertNotNull('Object_3 não deveria ser nil', Object_3);
    AssertNotNull('Object_Build não deveria ser nil', Object_Build);

    // Validar classe exata
    AssertEquals('Object_1 deveria ser TObject', TObject, Object_1.ClassType);
    AssertEquals('Object_2 deveria ser TObject', TObject, Object_2.ClassType);
    AssertEquals('Object_3 deveria ser TObject', TObject, Object_3.ClassType);
    AssertEquals('Object_Build deveria ser TObject', TObject, Object_Build.ClassType);

    // Validar que todas apontam para a mesma instância
    AssertTrue('Todas as referências devem apontar para o mesmo objeto',
               (Object_1 = Object_2) and
               (Object_2 = Object_3) and
               (Object_3 = Object_Build));

  finally
    Object_1.Free; // apenas uma vez, pois todas são a mesma instância
  end;
end;

procedure TObjectBuilderTests.TestObjectBuilderMultiplesBuilds;
var
  Builder: TObjectBuilder;
  Object1, Object2: TObject;
begin
  Builder := TObjectBuilder.Create(TObject);

  Object1 := Builder.Build;
  Object2 := Builder.Build;

  AssertNotNull(Object1);
  AssertNotNull(Object2);
  AssertTrue('Cada Build deve retornar uma nova instância', Object1 <> Object2);

  Object1.Free;
  Object2.Free;
  Builder.Free;
end;

procedure TObjectBuilderTests.TestObjectBuilderMultiplesBuildsWithReferences;
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
  AssertNotNull(Object_Build);
  AssertNotNull(Object_1);
  AssertNotNull(Object_2);
  AssertTrue('Todas as referências devem apontar para a mesma instância',
             (Object_Build = Object_1) and (Object_1 = Object_2));

  // salvar referência da primeira instância
  FirstInstance := Object_Build;

  // segunda build
  Object_Build := Builder.Build;
  AssertNotNull(Object_Build);
  AssertNotNull(Object_1);
  AssertNotNull(Object_2);

  // validar que é uma nova instância
  AssertTrue('Cada Build deve retornar uma nova instância', Object_Build <> FirstInstance);

  // validar que todas as referências foram atualizadas
  AssertTrue('Todas as referências devem apontar para a nova instância',
             (Object_Build = Object_1) and (Object_1 = Object_2));
end;

procedure TObjectBuilderTests.TestObjectBuilderResetReferences;
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
  AssertNotNull('Object_1 não deveria ser nil', Object_1);
  AssertNotNull('Object_2 não deveria ser nil', Object_2);

  // validar que a segunda build foi atribuída apenas à nova referência
  AssertNotNull('Object_3 não deveria ser nil', Object_3);
  AssertTrue('Object_3 deveria ser diferente de Object_1', Object_3 <> Object_1);
  AssertTrue('Object_3 deveria ser diferente de Object_2', Object_3 <> Object_2);
end;

procedure TObjectBuilderTests.TestObjectBuilderSetup;
var
  Builder: TObjectBuilder;
  Obj: TObjectDescendent;
begin
  Obj := nil;
  Builder := TObjectBuilder.Create(TObjectDescendent);
  Builder.Setup(TObjectSetupProcObj(@SetupObject_1));
  try
    Obj := Builder.Build as TObjectDescendent;
    AssertNotNull('Obj não deveria ser nil', Obj);
    AssertEquals('Propriedade FFoo de Obj diferente da esperada', 188, Obj.FFoo);
  finally
    Obj.Free;
  end;
end;

procedure TObjectBuilderTests.TestObjectBuilderMultipleSetups;
var
  Builder: TObjectBuilder;
  Obj: TObjectDescendent;
begin
  Obj := nil;
  Builder := TObjectBuilder.Create(TObjectDescendent);
  Builder.Setup(TObjectSetupProcObj(@SetupObject_1));
  Builder.Setup(TObjectSetupProcObj(@SetupObject_2));
  try
    Obj := Builder.Build as TObjectDescendent;
    AssertNotNull('Obj não deveria ser nil', Obj);
    AssertEquals('Propriedade FFoo de Obj diferente da esperada', 188, Obj.FFoo);
    AssertEquals('Propriedade FBar de Obj diferente da esperada', 456, Obj.FBar);
  finally
    Obj.Free;
  end;
end;

procedure TObjectBuilderTests.TestObjectBuilderWithProp;
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
    AssertNotNull('Obj não deveria ser nil', Obj);
    AssertEquals('Propriedade Foo de Obj diferente da esperada', 251, Obj.Foo);
    AssertEquals('Propriedade Bar de Obj diferente da esperada', 938, Obj.Bar);
  finally
    Obj.Free;
  end;
end;

procedure TObjectBuilderTests.SetUp;
begin

end;

procedure TObjectBuilderTests.TearDown;
begin

end;

procedure TObjectBuilderTests.SetupObject_1(AObject: TObject);
var
  O: TObjectDescendent;
begin
  O := (AObject as TObjectDescendent);
  O.FFoo := 188;
end;

procedure TObjectBuilderTests.SetupObject_2(AObject: TObject);
var
  O: TObjectDescendent;
begin
  O := (AObject as TObjectDescendent);
  O.FBar := 456;
end;

initialization
  RegisterTest(TObjectBuilderTests);

end.

