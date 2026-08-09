unit UComponentRegistryTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, fpcunit, testutils, testregistry, OPCB;

type
  TComponentRegistry_ = class(TComponentRegistry)

  end;

  { TComponentRegistryTest }

  TComponentRegistryTest = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestForContext;
    procedure TestForContextDiferentInstance;
    procedure TestForContextSameInstance;
    procedure TestReleaseContext;
    procedure TestGetItemFindsNonVisualComponent;
    procedure TestRenamedComponentLeavesNoPhantomEntryOnDestroy;
    procedure TestCreateRaisesGuardException;
    procedure TestGetComponentRaisesWhenNotFound;
    procedure TestGetControlRaisesWhenNotFound;
    procedure TestTryGetComponentReturnsFalseWhenNotFound;
    procedure TestTryGetComponentReturnsTrueWhenFound;
    procedure TestTryGetControlReturnsFalseWhenNotFound;
    procedure TestTryGetControlReturnsTrueWhenFound;
    procedure TestGetComponentFromContext;
    procedure TestGetControlFromContext;
    procedure TestClearAllRemovesAllContexts;
    procedure TestGetContextHandleWrapsForContext;
  end;

implementation

procedure TComponentRegistryTest.TestForContext;
var
  Registry: TComponentRegistry;
begin
  Registry := TComponentRegistry_.ForContext('test');
  try
    AssertNotNull('Registry não deveria ser nil', Registry);
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
    AssertNotSame('Registry_1 deveria ser diferente de Registry_2', Registry_1, Registry_2);
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
    AssertSame('Registry_1 should be the same that Registry_2', Registry_1, Registry_2);
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

  AssertEquals('FInstances.Count com valor diferente do esperado', InitialCount-1, FinalCount);
end;

procedure TComponentRegistryTest.TestGetItemFindsNonVisualComponent;
{ regressão: o indexador default Items[]/GetItem só buscava em
  FNamedControls (via GetControl), nunca em FNamedComponents - um
  componente não-visual (ex: TTimer) registrado via AddComponent nunca era
  achado por Registry['Nome'], mesmo existindo no registry. }
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

    AssertSame('Registry[''MeuTimer''] deveria devolver o TTimer registrado',
      TComponent(Timer), Registry['MeuTimer']);
  finally
    OwnerComp.Free;
    TComponentRegistry_.ReleaseContext('test_getitem_nonvisual');
  end;
end;

procedure TComponentRegistryTest.TestRenamedComponentLeavesNoPhantomEntryOnDestroy;
{ regressão: o registro é feito pelo nome único calculado na criação, mas a
  remoção (ao destruir o componente) usava o .Name ATUAL - se o nome
  tivesse sido trocado depois do registro, a remoção não achava a entrada
  original pelo nome antigo, deixando-a no registry apontando para memória
  já liberada. }
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

    Timer.Free; // dispara a notificação de remoção do registry

    AssertFalse('Entrada fantasma sob o nome original não deveria mais existir',
      Registry.TryGetComponent('NomeOriginal', Found));
    AssertFalse('Também não deveria existir entrada sob o nome novo',
      Registry.TryGetComponent('NomeRenomeado', Found));
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
    Registry.Free; // não deveria chegar aqui
  except
    on E: Exception do
      Raised := (E.Message = 'Use TComponentRegistry.ForContext');
  end;
  AssertTrue('TComponentRegistry.Create deveria orientar a usar ForContext', Raised);
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
    AssertTrue('GetComponent deveria levantar exceção para nome inexistente', Raised);
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
    AssertTrue('GetControl deveria levantar exceção para nome inexistente', Raised);
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
    AssertFalse('TryGetComponent deveria devolver False para nome inexistente',
      Registry.TryGetComponent('inexistente', Found));
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

    AssertTrue('TryGetComponent deveria devolver True para nome existente',
      Registry.TryGetComponent('MeuTimer', Found));
    AssertSame('Componente devolvido diferente do esperado', TComponent(Timer), Found);
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
    AssertFalse('TryGetControl deveria devolver False para nome inexistente',
      Registry.TryGetControl('inexistente', Found));
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

    AssertTrue('TryGetControl deveria devolver True para nome existente',
      Registry.TryGetControl('MeuPainel', Found));
    AssertSame('Controle devolvido diferente do esperado', TControl(P), Found);
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
  // Atalho de classe: abre o contexto, busca e libera internamente - não
  // precisa que o chamador segure um ForContext/ReleaseContext próprio.
  Registry := TComponentRegistry_.ForContext('test_getcomponentfromcontext');
  OwnerComp := TComponent.Create(nil);
  try
    Timer := TTimer.Create(OwnerComp);
    Registry.AddComponent(Timer, 'MeuTimer');

    Found := TComponentRegistry.GetComponentFromContext('test_getcomponentfromcontext', 'MeuTimer');
    AssertSame('Componente devolvido diferente do esperado', TComponent(Timer), Found);
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
    AssertSame('Controle devolvido diferente do esperado', TControl(P), Found);
  finally
    P.Free;
    TComponentRegistry_.ReleaseContext('test_getcontrolfromcontext');
  end;
end;

procedure TComponentRegistryTest.TestClearAllRemovesAllContexts;
var
  Registry: TComponentRegistry;
begin
  // ClearAll é global para o processo (libera TODO contexto vivo em
  // FInstances, não só o desta chave) - seguro aqui porque nenhum outro
  // teste desta suíte mantém um contexto aberto entre métodos (cada um
  // fecha o próprio no finally antes de retornar). Se algum teste futuro
  // passar a manter contexto aberto entre chamadas, este teste passaria a
  // liberar objetos que esse outro teste ainda espera vivos.
  Registry := TComponentRegistry_.ForContext('test_clearall');
  AssertNotNull('Registry não deveria ser nil antes do ClearAll', Registry);

  TComponentRegistry_.ClearAll;

  AssertFalse('FInstances deveria estar nil depois de ClearAll',
    Assigned(TComponentRegistry_.FInstances));
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
      AssertSame('Registry do handle deveria ser o mesmo devolvido por ForContext para a mesma chave',
        Registry, Handle.Registry);
    finally
      TComponentRegistry_.ReleaseContext('test_contexthandle');
    end;
  finally
    Handle := nil; // dispara ReleaseContext via destructor da interface
  end;
end;

procedure TComponentRegistryTest.SetUp;
begin

end;

procedure TComponentRegistryTest.TearDown;
begin

end;

initialization

  RegisterTest(TComponentRegistryTest);
end.

