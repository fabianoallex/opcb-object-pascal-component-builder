unit UComponentRegistryTest;

interface

uses
  DUnitX.TestFramework, System.SysUtils, System.Classes, Vcl.Controls, Vcl.ExtCtrls, OPCB;

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
    [Test] procedure TestCreateRaisesGuardException;
    [Test] procedure TestGetComponentRaisesWhenNotFound;
    [Test] procedure TestGetControlRaisesWhenNotFound;
    [Test] procedure TestTryGetComponentReturnsFalseWhenNotFound;
    [Test] procedure TestTryGetComponentReturnsTrueWhenFound;
    [Test] procedure TestTryGetControlReturnsFalseWhenNotFound;
    [Test] procedure TestTryGetControlReturnsTrueWhenFound;
    [Test] procedure TestGetComponentFromContext;
    [Test] procedure TestGetControlFromContext;
    [Test] procedure TestClearAllRemovesAllContexts;
    [Test] procedure TestGetContextHandleWrapsForContext;
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

procedure TComponentRegistryTest.TestCreateRaisesGuardException;
var
  Raised: Boolean;
  Registry: TComponentRegistry;
begin
  Raised := False;
  try
    Registry := TComponentRegistry.Create;
    Registry.Free; // nao deveria chegar aqui
  except
    on E: Exception do
      Raised := (E.Message = 'Use TComponentRegistry.ForContext');
  end;
  Assert.IsTrue(Raised, 'TComponentRegistry.Create deveria orientar a usar ForContext');
end;

procedure TComponentRegistryTest.TestGetComponentRaisesWhenNotFound;
var
  Registry: TComponentRegistry;
  Raised: Boolean;
begin
  Registry := TComponentRegistry_.ForContext('test_getcomponent_notfound');
  Raised := False;
  try
    try
      Registry.GetComponent('inexistente');
    except
      on E: Exception do
        Raised := Pos('inexistente', E.Message) > 0;
    end;
    Assert.IsTrue(Raised, 'GetComponent deveria levantar excecao para nome inexistente');
  finally
    TComponentRegistry_.ReleaseContext('test_getcomponent_notfound');
  end;
end;

procedure TComponentRegistryTest.TestGetControlRaisesWhenNotFound;
var
  Registry: TComponentRegistry;
  Raised: Boolean;
begin
  Registry := TComponentRegistry_.ForContext('test_getcontrol_notfound');
  Raised := False;
  try
    try
      Registry.GetControl('inexistente');
    except
      on E: Exception do
        Raised := Pos('inexistente', E.Message) > 0;
    end;
    Assert.IsTrue(Raised, 'GetControl deveria levantar excecao para nome inexistente');
  finally
    TComponentRegistry_.ReleaseContext('test_getcontrol_notfound');
  end;
end;

procedure TComponentRegistryTest.TestTryGetComponentReturnsFalseWhenNotFound;
var
  Registry: TComponentRegistry;
  Found: TComponent;
begin
  Registry := TComponentRegistry_.ForContext('test_trygetcomponent');
  try
    Assert.IsFalse(Registry.TryGetComponent('inexistente', Found),
      'TryGetComponent deveria devolver False para nome inexistente');
  finally
    TComponentRegistry_.ReleaseContext('test_trygetcomponent');
  end;
end;

procedure TComponentRegistryTest.TestTryGetComponentReturnsTrueWhenFound;
var
  Registry: TComponentRegistry;
  OwnerComp: TComponent;
  Timer: TTimer;
  Found: TComponent;
begin
  Registry := TComponentRegistry_.ForContext('test_trygetcomponent_found');
  OwnerComp := TComponent.Create(nil);
  try
    Timer := TTimer.Create(OwnerComp);
    Registry.AddComponent(Timer, 'MeuTimer');

    Assert.IsTrue(Registry.TryGetComponent('MeuTimer', Found),
      'TryGetComponent deveria devolver True para nome existente');
    Assert.IsTrue(TComponent(Timer) = Found, 'Componente devolvido diferente do esperado');
  finally
    OwnerComp.Free;
    TComponentRegistry_.ReleaseContext('test_trygetcomponent_found');
  end;
end;

procedure TComponentRegistryTest.TestTryGetControlReturnsFalseWhenNotFound;
var
  Registry: TComponentRegistry;
  Found: TControl;
begin
  Registry := TComponentRegistry_.ForContext('test_trygetcontrol');
  try
    Assert.IsFalse(Registry.TryGetControl('inexistente', Found),
      'TryGetControl deveria devolver False para nome inexistente');
  finally
    TComponentRegistry_.ReleaseContext('test_trygetcontrol');
  end;
end;

procedure TComponentRegistryTest.TestTryGetControlReturnsTrueWhenFound;
var
  Registry: TComponentRegistry;
  P: TPanel;
  Found: TControl;
begin
  Registry := TComponentRegistry_.ForContext('test_trygetcontrol_found');
  P := TPanel.Create(nil);
  try
    Registry.Add(P, 'MeuPainel');

    Assert.IsTrue(Registry.TryGetControl('MeuPainel', Found),
      'TryGetControl deveria devolver True para nome existente');
    Assert.IsTrue(TControl(P) = Found, 'Controle devolvido diferente do esperado');
  finally
    P.Free;
    TComponentRegistry_.ReleaseContext('test_trygetcontrol_found');
  end;
end;

procedure TComponentRegistryTest.TestGetComponentFromContext;
var
  Registry: TComponentRegistry;
  OwnerComp: TComponent;
  Timer: TTimer;
  Found: TComponent;
begin
  // Atalho de classe: abre o contexto, busca e libera internamente - nao
  // precisa que o chamador segure um ForContext/ReleaseContext proprio.
  Registry := TComponentRegistry_.ForContext('test_getcomponentfromcontext');
  OwnerComp := TComponent.Create(nil);
  try
    Timer := TTimer.Create(OwnerComp);
    Registry.AddComponent(Timer, 'MeuTimer');

    Found := TComponentRegistry.GetComponentFromContext('test_getcomponentfromcontext', 'MeuTimer');
    Assert.IsTrue(TComponent(Timer) = Found, 'Componente devolvido diferente do esperado');
  finally
    OwnerComp.Free;
    TComponentRegistry_.ReleaseContext('test_getcomponentfromcontext');
  end;
end;

procedure TComponentRegistryTest.TestGetControlFromContext;
var
  Registry: TComponentRegistry;
  P: TPanel;
  Found: TControl;
begin
  Registry := TComponentRegistry_.ForContext('test_getcontrolfromcontext');
  P := TPanel.Create(nil);
  try
    Registry.Add(P, 'MeuPainel');

    Found := TComponentRegistry.GetControlFromContext('test_getcontrolfromcontext', 'MeuPainel');
    Assert.IsTrue(TControl(P) = Found, 'Controle devolvido diferente do esperado');
  finally
    P.Free;
    TComponentRegistry_.ReleaseContext('test_getcontrolfromcontext');
  end;
end;

procedure TComponentRegistryTest.TestClearAllRemovesAllContexts;
var
  Registry: TComponentRegistry;
begin
  // ClearAll e global para o processo (libera TODO contexto vivo em
  // FInstances, nao so o desta chave) - seguro aqui porque nenhum outro
  // teste desta suite mantem um contexto aberto entre metodos (cada um
  // fecha o proprio no finally antes de retornar). Se algum teste futuro
  // passar a manter contexto aberto entre chamadas, este teste passaria a
  // liberar objetos que esse outro teste ainda espera vivos.
  Registry := TComponentRegistry_.ForContext('test_clearall');
  Assert.IsNotNull(Registry, 'Registry nao deveria ser nil antes do ClearAll');

  TComponentRegistry_.ClearAll;

  Assert.IsFalse(Assigned(TComponentRegistry_.FInstances),
    'FInstances deveria estar nil depois de ClearAll');
end;

procedure TComponentRegistryTest.TestGetContextHandleWrapsForContext;
var
  Handle: IRegistryContextHandle;
  Registry: TComponentRegistry;
begin
  Handle := TComponentRegistry.GetContextHandle('test_contexthandle');
  try
    Registry := TComponentRegistry_.ForContext('test_contexthandle');
    try
      Assert.IsTrue(Registry = Handle.Registry,
        'Registry do handle deveria ser o mesmo devolvido por ForContext para a mesma chave');
    finally
      TComponentRegistry_.ReleaseContext('test_contexthandle');
    end;
  finally
    Handle := nil; // dispara ReleaseContext via destructor da interface
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TComponentRegistryTest);

end.
