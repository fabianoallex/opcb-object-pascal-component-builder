unit UComponentRegistryTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, fpcunit, testutils, testregistry, OPCB;

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

procedure TComponentRegistryTest.SetUp;
begin

end;

procedure TComponentRegistryTest.TearDown;
begin

end;

initialization

  RegisterTest(TComponentRegistryTest);
end.

