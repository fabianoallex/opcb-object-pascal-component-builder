unit UComponentRegistryTest;

interface

uses
  DUnitX.TestFramework, System.Classes, Vcl.ExtCtrls, OPCB;

type
  TComponentRegistry_ = class(TComponentRegistry)

  end;

  [TestFixture]
  TComponentRegistryTest = class
  public
    [Test] procedure TestForContext;
    [Test] procedure TestForContextDiferentInstance;
    [Test] procedure TestForContextSameInstance;
    [Test] procedure TestReleaseContext;
    [Test] procedure TestGetItemFindsNonVisualComponent;
    [Test] procedure TestRenamedComponentLeavesNoPhantomEntryOnDestroy;
  end;

implementation

procedure TComponentRegistryTest.TestForContext;
var
  Registry: TComponentRegistry;
begin
  Registry := TComponentRegistry_.ForContext('test');
  try
    Assert.IsNotNull(Registry, 'Registry nao deveria ser nil');
  finally
    TComponentRegistry_.ReleaseContext('test'); //each ForContext need a Release
  end;
end;

procedure TComponentRegistryTest.TestForContextDiferentInstance;
var
  Registry_1: TComponentRegistry;
  Registry_2: TComponentRegistry;
begin
  Registry_1 := TComponentRegistry_.ForContext('test_1');
  Registry_2 := TComponentRegistry_.ForContext('test_2');
  try
    Assert.IsTrue(Registry_1 <> Registry_2, 'Registry_1 deveria ser diferente de Registry_2');
  finally
    TComponentRegistry_.ReleaseContext('test_1');
    TComponentRegistry_.ReleaseContext('test_2');
  end;
end;

procedure TComponentRegistryTest.TestForContextSameInstance;
var
  Registry_1: TComponentRegistry;
  Registry_2: TComponentRegistry;
begin
  Registry_1 := TComponentRegistry_.ForContext('test');
  Registry_2 := TComponentRegistry_.ForContext('test');
  try
    Assert.IsTrue(Registry_1 = Registry_2, 'Registry_1 should be the same that Registry_2');
  finally
    TComponentRegistry_.ReleaseContext('test');
    TComponentRegistry_.ReleaseContext('test');
  end;
end;

procedure TComponentRegistryTest.TestReleaseContext;
var
  Registry: TComponentRegistry;
  InitialCount: Integer;
  FinalCount: Integer;
  Context: string;
begin
  Context := 'test_bpdoeiduuskiokdloowlslsw'; // unique context for this test

  Registry := TComponentRegistry_.ForContext(Context);

  InitialCount := TComponentRegistry_.FInstances.Count;

  TComponentRegistry_.ReleaseContext(Context);

  FinalCount := 0;
  if Assigned(TComponentRegistry_.FInstances) then
    FinalCount := TComponentRegistry_.FInstances.Count;

  Assert.AreEqual(InitialCount - 1, FinalCount, 'FInstances.Count com valor diferente do esperado');
end;

procedure TComponentRegistryTest.TestGetItemFindsNonVisualComponent;
// Regressao: o indexador default Items[]/GetItem so buscava em
// FNamedControls (via GetControl), nunca em FNamedComponents - um
// componente nao-visual (ex: TTimer) registrado via AddComponent nunca era
// achado por Registry['Nome'], mesmo existindo no registry.
var
  Registry: TComponentRegistry;
  OwnerComp: TComponent;
  Timer: TTimer;
begin
  Registry := TComponentRegistry_.ForContext('test_getitem_nonvisual');
  OwnerComp := TComponent.Create(nil);
  try
    Timer := TTimer.Create(OwnerComp);
    Registry.AddComponent(Timer, 'MeuTimer');

    Assert.IsTrue(TComponent(Timer) = Registry['MeuTimer'],
      'Registry[''MeuTimer''] deveria devolver o TTimer registrado');
  finally
    OwnerComp.Free;
    TComponentRegistry_.ReleaseContext('test_getitem_nonvisual');
  end;
end;

procedure TComponentRegistryTest.TestRenamedComponentLeavesNoPhantomEntryOnDestroy;
// Regressao: o registro e feito pelo nome unico calculado na criacao, mas a
// remocao (ao destruir o componente) usava o .Name ATUAL - se o nome
// tivesse sido trocado depois do registro, a remocao nao achava a entrada
// original pelo nome antigo, deixando-a no registry apontando para memoria
// ja liberada.
var
  Registry: TComponentRegistry;
  OwnerComp: TComponent;
  Timer: TTimer;
  Found: TComponent;
begin
  Registry := TComponentRegistry_.ForContext('test_rename_phantom');
  OwnerComp := TComponent.Create(nil);
  try
    Timer := TTimer.Create(OwnerComp);
    Registry.AddComponent(Timer, 'NomeOriginal');
    Timer.Name := 'NomeRenomeado';

    Timer.Free; // dispara a notificacao de remocao do registry

    Assert.IsFalse(Registry.TryGetComponent('NomeOriginal', Found),
      'Entrada fantasma sob o nome original nao deveria mais existir');
    Assert.IsFalse(Registry.TryGetComponent('NomeRenomeado', Found),
      'Tambem nao deveria existir entrada sob o nome novo');
  finally
    OwnerComp.Free;
    TComponentRegistry_.ReleaseContext('test_rename_phantom');
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TComponentRegistryTest);

end.
