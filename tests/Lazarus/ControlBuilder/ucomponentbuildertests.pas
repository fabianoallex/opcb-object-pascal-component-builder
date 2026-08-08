unit UComponentBuilderTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, OPCB, Dialogs, Menus;

type

  { TComponentBuilderHelper }

  TComponentBuilderHelper = class helper for TComponentBuilder
  public
    function WithOpenFileOptions(AValue: TOpenOptions): TComponentBuilder;
  end;

  { TComponentBuilderTests }

  TComponentBuilderTests = class(TTestCase)
  private
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestComponentBuilder;
    procedure TestComponentBuilderDescendent;
    procedure TestComponentBuilderWithName;
    procedure TestComponentBuilderWithProp;
    procedure TestComponentBuilderFluentWithCalls;
    procedure TestMenuItemBuilderCreateWithNameAndReference;
  end;

implementation

{ TComponentBuilderHelper }

function TComponentBuilderHelper.WithOpenFileOptions(AValue: TOpenOptions
  ): TComponentBuilder;
begin
  Result := Self;
  WithPropSet('Options', Integer(AValue));
end;

procedure TComponentBuilderTests.TestComponentBuilder;
var
  Builder: TComponentBuilder;
  Obj: TComponent;
begin
  Obj := nil;
  Builder := TComponentBuilder.Create;
  try
    Obj := Builder.Build;
    AssertNotNull('Obj não deveria ser nil', Obj);
  finally
    Obj.Free;
  end;
end;

procedure TComponentBuilderTests.TestComponentBuilderDescendent;
var
  Builder: TComponentBuilder;
  Obj: TOpenDialog;
begin
  Obj := nil;
  Builder := TComponentBuilder.Create(TOpenDialog);
  try
    Obj := Builder.Build as TOpenDialog;
    AssertNotNull('Obj não deveria ser nil', Obj);
    AssertEquals(
      'Obj deveria ser exatamente da classe TObjectDescendent',
      TOpenDialog, Obj.ClassType);
  finally
    Obj.Free;
  end;
end;

procedure TComponentBuilderTests.TestComponentBuilderWithName;
var
  Builder: TComponentBuilder;
  Obj: TOpenDialog;
begin
  Obj := nil;
  Builder := TComponentBuilder.Create(TOpenDialog);
  Builder.WithName('MyOpenDialog');
  try
    Obj := Builder.Build as TOpenDialog;
    AssertNotNull('Obj não deveria ser nil', Obj);
    AssertEquals('Propriedade Name de Obj diferente da esperada', 'MyOpenDialog', Obj.Name);
  finally
    Obj.Free;
  end;
end;

procedure TComponentBuilderTests.TestComponentBuilderWithProp;
var
  Builder: TComponentBuilder;
  Obj: TOpenDialog;
begin
  Obj := nil;
  Builder := TComponentBuilder.Create(TOpenDialog);
  Builder.WithProp('DefaultExt', '.exe'); // altera via rtti
  try
    Obj := Builder.Build as TOpenDialog;
    AssertNotNull('Obj não deveria ser nil', Obj);
    AssertEquals('Propriedade DefaultExt de Obj diferente da esperada', '.exe', Obj.DefaultExt);
  finally
    Obj.Free;
  end;
end;

procedure TComponentBuilderTests.TestComponentBuilderFluentWithCalls;
var
  Builder: TComponentBuilder;
  Obj: TOpenDialog;
begin
  Obj := nil;
  Builder := TComponentBuilder.Create(TOpenDialog);
  Builder
    .WithName('MyOpenDialog')
    .WithProp('DefaultExt', '.exe')
      // definido no helper TComponentBuilderHelper declara aqui nos testes
    .WithOpenFileOptions([ofOverwritePrompt, ofHideReadOnly, ofShowHelp])
    .WithTag(55)
  ;
  try
    Obj := Builder.Build as TOpenDialog;
    AssertNotNull('Obj não deveria ser nil', Obj);
    AssertEquals('Propriedade Name de Obj diferente da esperada', 'MyOpenDialog', Obj.Name);
    AssertEquals('Propriedade Tag de Obj diferente da esperada', 55, Obj.Tag);
    AssertEquals('Propriedade DefaultExt de Obj diferente da esperada', '.exe', Obj.DefaultExt);
    AssertEquals(
      'Propriedade Options de Obj diferente da esperada',
      Integer([ofOverwritePrompt, ofHideReadOnly, ofShowHelp]),
      Integer(Obj.Options)
    );
  finally
    Obj.Free;
  end;
end;

procedure TComponentBuilderTests.TestMenuItemBuilderCreateWithNameAndReference;
var
  Builder: TMenuItemBuilder;
  Item: TMenuItem;
begin
  Item := nil;
  // Regressão: este overload (AClass, AName, out Reference) chamava a si mesmo
  // em vez de "inherited Create", causando recursão infinita (stack overflow).
  Builder := TMenuItemBuilder.Create(TMenuItem, 'MyMenuItem', Item);
  try
    Builder.Build;
    AssertNotNull('Item (out Reference) deveria ter sido preenchido', Item);
    AssertEquals('Nome do item diferente do esperado', 'MyMenuItem', Item.Name);
  finally
    Item.Free;
  end;
end;

procedure TComponentBuilderTests.SetUp;
begin

end;

procedure TComponentBuilderTests.TearDown;
begin

end;

initialization

  RegisterTest(TComponentBuilderTests);
end.

