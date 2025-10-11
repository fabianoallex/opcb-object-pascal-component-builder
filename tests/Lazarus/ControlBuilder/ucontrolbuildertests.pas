unit UControlBuilderTests;

{$mode objfpc}{$H+}

interface

uses
  Controls, Classes, SysUtils, fpcunit, testregistry, OPCB, ExtCtrls;

type

  { TPanelWithReadOnlyProp }

  TPanelWithReadOnlyProp = class(TPanel)
  private
    function GetReadOnlyProp: Integer;
  published
    property ReadOnlyProp: Integer read GetReadOnlyProp;
  end;

  { TControlBuilderTests }

  TControlBuilderTests = class(TTestCase)
  protected
    FDestroyed: Boolean;
    FClicked: Boolean;
    procedure SetUp; override;
    procedure TearDown; override;
    procedure BuilderDestroy(Sender: TObject);
    procedure SetupControl_1(AControl: TControl);
    procedure SetupControl_2(AControl: TControl);
    procedure ButtonClick(Self: TObject);
  published
    procedure TestControlBuilder;
    procedure TestControlBuilderDescendetControlBuild;
    procedure TestControlBuilderAssignsReference;
    procedure TestControlBuilderAssignsMultiReferences;
    procedure TestControlBuilderAssignsName;
    procedure TestControlBuilderMultiplesBuilds;
    procedure TestControlBuilderMultiplesBuildsWithReferences;
    procedure TestControlBuilderResetReferences;
    procedure TestControlBuilderSetup;
    procedure TestControlBuilderMultipleSetups;
    procedure TestControlBuilderWithProp;
    procedure TestControlBuilderWithPropBoolean;
    procedure TestControlBuilderWithPropPath;
    procedure TestControlBuilderWithPropPathSetOf;
    procedure TestControlBuilderWithPropPathColor;
    procedure TestControlBuilderWithPropObj;
    procedure TestControlBuilderWithPropObjWithPropValue;
    procedure TestControlBuilderWithPropObjNegative;
    procedure TestControlBuilderWithPropEnum;
    procedure TestControlBuilderWithPropSetOf;
    procedure TestControlBuilderWithName;
    procedure TestControlBuilderWithTag;
    procedure TestControlBuilderWithWidth;
    procedure TestControlBuilderWithHeight;
    procedure TestControlBuilderWithTop;
    procedure TestControlBuilderWithLeft;
    procedure TestControlBuilderWithCaption;
    procedure TestControlBuilderWithText;
    procedure TestControlBuilderWithTextAsCaption;
    procedure TestControlBuilderWithOnClick;
    procedure TestControlBuilderWithOwner;
    procedure TestControlBuilderWithParent;
    procedure TestControlBuilderWithConstraints;
    procedure TestControlBuilderWithInexistentProp;
    procedure TestControlBuilderWithInvalidType;
    procedure TestControlBuilderWithOutOfRangeValue;
    procedure TestControlBuilderReadOnlyProp;
    procedure TestControlBuilderSetupAndWithName;
  end;

implementation

uses
  StdCtrls, Menus, Graphics, Forms, RTTI, TypInfo;

{ TPanelWithReadOnlyProp }

function TPanelWithReadOnlyProp.GetReadOnlyProp: Integer;
begin
  Result := 42;
end;

procedure TControlBuilderTests.SetUp;
begin
  FClicked := False;
end;

procedure TControlBuilderTests.TearDown;
begin

end;


procedure TControlBuilderTests.BuilderDestroy(Sender: TObject);
begin
  FDestroyed := True;
end;

procedure TControlBuilderTests.SetupControl_1(AControl: TControl);
begin
  (AControl as TPanel).Height := 299;
end;

procedure TControlBuilderTests.SetupControl_2(AControl: TControl);
begin
  (AControl as TPanel).Width := 177;
end;

procedure TControlBuilderTests.ButtonClick(Self: TObject);
begin
  FClicked := True;
end;

procedure TControlBuilderTests.TestControlBuilder;
var
  Builder: TControlBuilder;
  Control: TControl;
begin
  Control := nil;
  Builder := TControlBuilder.Create;
  try
    Control := Builder.Build;
    AssertNotNull('Control não deveria ser nil', Control);
  finally
    Control.Free;
  end;
end;

procedure TControlBuilderTests.TestControlBuilderDescendetControlBuild;
var
  Builder: TControlBuilder;
  Control: TControl;
begin
  Control := nil;
  Builder := TControlBuilder.Create(TPanel);  // Instancia um TPanel
  try
    Control := Builder.Build;
    AssertNotNull('Control não deveria ser nil', Control);
    AssertEquals(
      'Control deveria ser exatamente da classe TPanel',
      TPanel, Control.ClassType);
  finally
    Control.Free;
  end;
end;

procedure TControlBuilderTests.TestControlBuilderAssignsReference;
var
  Builder: TControlBuilder;
  Control: TControl;
begin
  Control := nil;
  Builder := TControlBuilder.Create(TPanel, Control);  // Passa a variável que ira receber a instancia como referencia
  try
    Builder.Build;  // não atribui o build diretamente a variavel
    AssertNotNull('Control não deveria ser nil', Control);
  finally
    Control.Free;
  end;
end;

procedure TControlBuilderTests.TestControlBuilderAssignsMultiReferences;
var
  Builder: TControlBuilder;
  Control_1, Control_2, Control_3, Control_Build: TControl;
begin
  Control_1 := nil;
  Control_2 := nil;
  Control_3 := nil;
  Control_Build := nil;

  Builder := TControlBuilder.Create(TPanel, Control_1);
  Builder.Assign(Control_2);
  Builder.Assign(Control_3);

  try
    Control_Build := Builder.Build; // capturando retorno do Build

    // Validar não-nil
    AssertNotNull('Control_1 não deveria ser nil', Control_1);
    AssertNotNull('Control_2 não deveria ser nil', Control_2);
    AssertNotNull('Control_3 não deveria ser nil', Control_3);
    AssertNotNull('Control_Build não deveria ser nil', Control_Build);

    // Validar classe exata
    AssertEquals('Control_1 deveria ser TPanel', TPanel, Control_1.ClassType);
    AssertEquals('Control_2 deveria ser TPanel', TPanel, Control_2.ClassType);
    AssertEquals('Control_3 deveria ser TPanel', TPanel, Control_3.ClassType);
    AssertEquals('Control_Build deveria ser TPanel', TPanel, Control_Build.ClassType);

    // Validar que todas apontam para a mesma instância
    AssertTrue('Todas as referências devem apontar para o mesmo objeto',
               (Control_1 = Control_2) and
               (Control_2 = Control_3) and
               (Control_3 = Control_Build));

  finally
    Control_1.Free; // apenas uma vez, pois todas são a mesma instância
  end;
end;

procedure TControlBuilderTests.TestControlBuilderAssignsName;
var
  Builder: TControlBuilder;
  Control: TControl;
begin
  Control := nil;
  Builder := TControlBuilder.Create(TPanel, 'PanelTeste');  // Instancia um TPanel
  try
    Control := Builder.Build;  // auto free Builder
    AssertNotNull('Control não deveria ser nil', Control);
    AssertEquals('Control com nome diferente do esperado', 'PanelTeste', Control.Name);
  finally
    Control.Free;
  end;
end;

procedure TControlBuilderTests.TestControlBuilderMultiplesBuilds;
var
  Builder: TControlBuilder;
  Control1, Control2: TControl;
begin
  Builder := TControlBuilder.Create(TPanel);

  Control1 := Builder.Build;
  Control2 := Builder.Build;

  AssertNotNull(Control1);
  AssertNotNull(Control2);
  AssertTrue('Cada Build deve retornar uma nova instância', Control1 <> Control2);

  Control1.Free;
  Control2.Free;
  Builder.Free;
end;

procedure TControlBuilderTests.TestControlBuilderMultiplesBuildsWithReferences;
var
  Builder: TControlBuilder;
  FirstInstance, Control_1, Control_2: TControl;
  Control_Build: TControl;
begin
  Control_1 := nil;
  Control_2 := nil;

  Builder := TControlBuilder.Create(TPanel, Control_1);
  Builder.Assign(Control_2);

  // primeira build
  Control_Build := Builder.Build;
  AssertNotNull(Control_Build);
  AssertNotNull(Control_1);
  AssertNotNull(Control_2);
  AssertTrue('Todas as referências devem apontar para a mesma instância',
             (Control_Build = Control_1) and (Control_1 = Control_2));

  // salvar referência da primeira instância
  FirstInstance := Control_Build;

  // segunda build
  Control_Build := Builder.Build;
  AssertNotNull(Control_Build);
  AssertNotNull(Control_1);
  AssertNotNull(Control_2);

  // validar que é uma nova instância
  AssertTrue('Cada Build deve retornar uma nova instância', Control_Build <> FirstInstance);

  // validar que todas as referências foram atualizadas
  AssertTrue('Todas as referências devem apontar para a nova instância',
             (Control_Build = Control_1) and (Control_1 = Control_2));
end;

procedure TControlBuilderTests.TestControlBuilderResetReferences;
var
  Builder: TControlBuilder;
  Control_1, Control_2, Control_3: TControl;
begin
  Control_1 := nil;
  Control_2 := nil;
  Control_3 := nil;

  Builder := TControlBuilder.Create(TPanel, Control_1);

  Builder.Assign(Control_2).Build;

  // Resetar referências e usar novas
  Builder.ResetReferences;
  Builder.Assign(Control_3).Build;

  // validar que a primeira build não foi sobrescrita
  AssertNotNull('Control_1 não deveria ser nil', Control_1);
  AssertNotNull('Control_2 não deveria ser nil', Control_2);

  // validar que a segunda build foi atribuída apenas à nova referência
  AssertNotNull('Control_3 não deveria ser nil', Control_3);
  AssertTrue('Control_3 deveria ser diferente de Control_1', Control_3 <> Control_1);
  AssertTrue('Control_3 deveria ser diferente de Control_2', Control_3 <> Control_2);
end;

procedure TControlBuilderTests.TestControlBuilderSetup;
var
  Builder: TControlBuilder;
  Control: TControl;
begin
  Control := nil;
  Builder := TControlBuilder.Create(TPanel);  // Instancia um TPanel
  Builder.Setup(TControlSetupProcObj(@SetupControl_1)); // altera hight pra 299
  try
    Control := Builder.Build;  // auto free Builder
    AssertNotNull('Control não deveria ser nil', Control);
    AssertEquals('Propriedade Height de Control diferente da esperada', 299, Control.Height);
  finally
    Control.Free;
  end;
end;

procedure TControlBuilderTests.TestControlBuilderMultipleSetups;
var
  Builder: TControlBuilder;
  Control: TControl;
begin
  Control := nil;
  Builder := TControlBuilder.Create(TPanel);  // Instancia um TPanel
  Builder.Setup(@SetupControl_1); // altera hight pra 299
  Builder.Setup(@SetupControl_2); // altera width para 177
  try
    Control := Builder.Build;  // auto free Builder
    AssertNotNull('Control não deveria ser nil', Control);
    AssertEquals('Propriedade Height de Control diferente da esperada', 299, Control.Height);
    AssertEquals('Propriedade Width de Control diferente da esperada', 177, Control.Width);
  finally
    Control.Free;
  end;
end;

procedure TControlBuilderTests.TestControlBuilderWithProp;
var
  Builder: TControlBuilder;
  Control: TControl;
begin
  Control := nil;
  Builder := TControlBuilder.Create(TPanel);  // Instancia um TPanel
  Builder.WithProp('Height', 288); // altera via rtti
  Builder.WithProp('Width', 344);  // altera via rtti
  try
    Control := Builder.Build;  // auto free Builder
    AssertNotNull('Control não deveria ser nil', Control);
    AssertEquals('Propriedade Height de Control diferente da esperada', 288, Control.Height);
    AssertEquals('Propriedade Width de Control diferente da esperada', 344, Control.Width);
  finally
    Control.Free;
  end;
end;

procedure TControlBuilderTests.TestControlBuilderWithPropBoolean;
var
  Builder: TControlBuilder;
  Button: TButton;
begin
  Button := nil;
  Builder := TControlBuilder.Create(TButton);  // Instancia um TPanel
  Builder.WithProp('Enabled', False);  // altera via rtti
  try
    Button := Builder.Build as TButton;  // auto free Builder
    AssertNotNull('Button não deveria ser nil', Button);
    AssertEquals('Propriedade Width de Button diferente da esperada', False, Button.Enabled);
  finally
    Button.Free;
  end;
end;

procedure TControlBuilderTests.TestControlBuilderWithPropPath;
var
  Builder: TControlBuilder;
  Control: TControl;
begin
  Control := nil;
  Builder := TControlBuilder.Create(TPanel);  // Instancia um TPanel
  Builder.WithProp('Font.size', 25); // altera via rtti
  try
    Control := Builder.Build;  // auto free Builder
    AssertNotNull('Control não deveria ser nil', Control);
    AssertEquals('Propriedade Font Size de Control diferente da esperada', 25, Control.Font.Size);
  finally
    Control.Free;
  end;
end;

procedure TControlBuilderTests.TestControlBuilderWithPropPathSetOf;
var
  Builder: TControlBuilder;
  Control: TControl;
begin
  Control := nil;
  Builder := TControlBuilder.Create(TPanel);  // Instancia um TPanel
  // seta via RTTI o conjunto Style (set of TFontStyle)
  Builder.WithPropSet('Font.Style', Integer([fsBold, fsItalic]));
  try
    Control := Builder.Build;
    AssertNotNull('Control não deveria ser nil', Control);
    AssertTrue('Font.Style deveria conter fsBold', fsBold in Control.Font.Style);
    AssertTrue('Font.Style deveria conter fsItalic', fsItalic in Control.Font.Style);
    AssertFalse('Font.Style não deveria conter fsUnderline', fsUnderline in Control.Font.Style);
  finally
    Control.Free;
  end;
end;

procedure TControlBuilderTests.TestControlBuilderWithPropPathColor;
var
  Builder: TControlBuilder;
  Control: TControl;
begin
  Control := nil;
  Builder := TControlBuilder.Create(TPanel);  // Instancia um TPanel
  try
    // seta via RTTI o Color da fonte
    Builder.WithProp('Font.Color', clRed);

    Control := Builder.Build;

    AssertNotNull('Control não deveria ser nil', Control);
    AssertEquals('Font.Color diferente do esperado', clRed, Control.Font.Color);
  finally
    Control.Free;
  end;
end;

procedure TControlBuilderTests.TestControlBuilderWithPropObj;
var
  Builder: TControlBuilder;
  Panel: TPanel;
  PopupMenu: TPopupMenu;
begin
  PopupMenu := TPopupMenu.Create(nil);

  Panel := nil;
  Builder := TControlBuilder.Create(TPanel);  // Instancia um TPanel
  Builder.WithPropObj('PopupMenu', PopupMenu); // altera via rtti

  try
    Panel := Builder.Build as TPanel;  // auto free Builder
    AssertNotNull('Panel não deveria ser nil', Panel);
    AssertSame('Propriedade PopupMenu de Panel diferente da esperada', PopupMenu, Panel.PopupMenu);
  finally
    Panel.Free;
    PopupMenu.Free;
  end;
end;

procedure TControlBuilderTests.TestControlBuilderWithPropObjWithPropValue;
var
  Builder: TControlBuilder;
  Panel: TPanel;
  PopupMenu: TPopupMenu;
begin
  PopupMenu := TPopupMenu.Create(nil);
  Panel := nil;
  Builder := TControlBuilder.Create(TPanel);  // Instancia um TPanel
  Builder.WithProp(TPropertyValue.specialize Create<TPopupMenu>('PopupMenu', PopupMenu)); // altera via rtti
  try
    Panel := Builder.Build as TPanel;
    AssertNotNull('Panel não deveria ser nil', Panel);
    AssertSame('Propriedade PopupMenu de Panel diferente da esperada', PopupMenu, Panel.PopupMenu);
  finally
    Panel.Free;
    PopupMenu.Free;
  end;
end;

procedure TControlBuilderTests.TestControlBuilderWithPropObjNegative;
var
  Builder: TControlBuilder;
  Panel: TPanel;
  PopupMenu1, PopupMenu2: TPopupMenu;
begin
  PopupMenu1 := TPopupMenu.Create(nil);
  PopupMenu2 := TPopupMenu.Create(nil);

  Panel := nil;
  Builder := TControlBuilder.Create(TPanel);
  Builder.WithPropObj('PopupMenu', PopupMenu1);
  try
    Panel := Builder.Build as TPanel;
    AssertNotNull('Panel não deveria ser nil', Panel);
    AssertSame('PopupMenu deveria ser exatamente o PopupMenu1', PopupMenu1, Panel.PopupMenu); // Deve ser o PopupMenu1
    AssertNotSame('PopupMenu não deveria ser o PopupMenu2', PopupMenu2, Panel.PopupMenu);     // Não pode ser o PopupMenu2
  finally
    Panel.Free;
    PopupMenu1.Free;
    PopupMenu2.Free;
  end;
end;

procedure TControlBuilderTests.TestControlBuilderWithPropEnum;
var
  Builder: TControlBuilder;
  Panel: TPanel;
begin
  Panel := nil;
  Builder := TControlBuilder.Create(TPanel);

  // seta via RTTI o enum Align
  Builder.WithProp(TPropertyValue.specialize Create<TAlign>('Align', alRight));

  try
    Panel := Builder.Build as TPanel;

    AssertNotNull('Panel não deveria ser nil', Panel);
    AssertEquals(
      'Propriedade Align de Panel diferente da esperada',
      Ord(alRight),  // Enum convertido pra inteiro
      Ord(Panel.Align)
    );
  finally
    Panel.Free;
  end;
end;

procedure TControlBuilderTests.TestControlBuilderWithPropSetOf;
var
  Builder: TControlBuilder;
  Panel: TPanel;
  AnchorsValue: TAnchors;
begin
  Panel := nil;
  Builder := TControlBuilder.Create(TPanel);  // Instancia um TPanel
  try
    // seta via RTTI o conjunto Anchors
    AnchorsValue := [akBottom, akLeft];
    Builder.WithPropSet('Anchors', Integer(AnchorsValue)); // não existe um tipo especifico pra set, entao converte pra integer
    Panel := Builder.Build as TPanel;

    AssertNotNull('Panel não deveria ser nil', Panel);
    AssertTrue('Anchors deveria conter akLeft', akLeft in Panel.Anchors);
    AssertFalse('Anchors deveria conter akTop', akTop in Panel.Anchors);
    AssertFalse('Anchors não deveria conter akRight', akRight in Panel.Anchors);
    AssertTrue('Anchors não deveria conter akBottom', akBottom in Panel.Anchors);
  finally
    Panel.Free;
  end;
end;

procedure TControlBuilderTests.TestControlBuilderWithName;
var
  Builder: TControlBuilder;
  Panel: TPanel;
begin
  Panel := nil;
  Builder := TControlBuilder.Create(TPanel);
  Builder.WithName('PanelTeste');
  try
    Panel := Builder.Build as TPanel;
    AssertNotNull('Panel não deveria ser nil', Panel);
    AssertEquals('Propriedade Name de Panel diferente da esperada', 'PanelTeste', Panel.Name);
  finally
    Panel.Free;
  end;
end;

procedure TControlBuilderTests.TestControlBuilderWithTag;
var
  Builder: TControlBuilder;
  Panel: TPanel;
begin
  Panel := nil;
  Builder := TControlBuilder.Create(TPanel);
  Builder.WithTag(77);
  try
    Panel := Builder.Build as TPanel;
    AssertNotNull('Panel não deveria ser nil', Panel);
    AssertEquals('Propriedade Tag diferente da esperada', 77, Panel.Tag);
  finally
    Panel.Free;
  end;
end;

procedure TControlBuilderTests.TestControlBuilderWithWidth;
var
  Builder: TControlBuilder;
  Panel: TPanel;
begin
  Panel := nil;
  Builder := TControlBuilder.Create(TPanel);
  Builder.WithWidth(205);
  try
    Panel := Builder.Build as TPanel;
    AssertNotNull('Panel não deveria ser nil', Panel);
    AssertEquals('Propriedade Width diferente da esperada', 205, Panel.Width);
  finally
    Panel.Free;
  end;
end;

procedure TControlBuilderTests.TestControlBuilderWithHeight;
var
  Builder: TControlBuilder;
  Panel: TPanel;
begin
  Panel := nil;
  Builder := TControlBuilder.Create(TPanel);
  Builder.WithHeight(154);
  try
    Panel := Builder.Build as TPanel;
    AssertNotNull('Panel não deveria ser nil', Panel);
    AssertEquals('Propriedade Height diferente da esperada', 154, Panel.Height);
  finally
    Panel.Free;
  end;
end;

procedure TControlBuilderTests.TestControlBuilderWithTop;
var
  Builder: TControlBuilder;
  Panel: TPanel;
begin
  Panel := nil;
  Builder := TControlBuilder.Create(TPanel);
  Builder.WithTop(25);
  try
    Panel := Builder.Build as TPanel;
    AssertNotNull('Panel não deveria ser nil', Panel);
    AssertEquals('Propriedade Top diferente da esperada', 25, Panel.Top);
  finally
    Panel.Free;
  end;
end;

procedure TControlBuilderTests.TestControlBuilderWithLeft;
var
  Builder: TControlBuilder;
  Panel: TPanel;
begin
  Panel := nil;
  Builder := TControlBuilder.Create(TPanel);
  Builder.WithLeft(32);
  try
    Panel := Builder.Build as TPanel;
    AssertNotNull('Panel não deveria ser nil', Panel);
    AssertEquals('Propriedade Left diferente da esperada', 32, Panel.Left);
  finally
    Panel.Free;
  end;
end;

procedure TControlBuilderTests.TestControlBuilderWithCaption;
var
  Builder: TControlBuilder;
  Panel: TPanel;
begin
  Panel := nil;
  Builder := TControlBuilder.Create(TPanel);
  Builder.WithCaption('TesteCaption');
  try
    Panel := Builder.Build as TPanel;
    AssertNotNull('Panel não deveria ser nil', Panel);
    AssertEquals('Propriedade Caption diferente da esperada', 'TesteCaption', Panel.Caption);
  finally
    Panel.Free;
  end;
end;

procedure TControlBuilderTests.TestControlBuilderWithText;
var
  Builder: TControlBuilder;
  Edit: TEdit;
begin
  Edit := nil;
  Builder := TControlBuilder.Create(TEdit);
  Builder.WithText('TesteText');
  try
    Edit := Builder.Build as TEdit;
    AssertNotNull('Edit não deveria ser nil', Edit);
    AssertEquals('Propriedade Text diferente da esperada', 'TesteText', Edit.Text);
  finally
    Edit.Free;
  end;
end;

procedure TControlBuilderTests.TestControlBuilderWithTextAsCaption;
var
  Builder: TControlBuilder;
  Panel: TPanel;
begin
  Panel := nil;
  Builder := TControlBuilder.Create(TPanel);
  Builder.WithText('TesteCaption');  // deve atribuir ao Caption, pois Panel não tem Text
  try
    Panel := Builder.Build as TPanel;
    AssertNotNull('Panel não deveria ser nil', Panel);
    AssertEquals('Propriedade Caption diferente da esperada', 'TesteCaption', Panel.Caption);
  finally
    Panel.Free;
  end;
end;

procedure TControlBuilderTests.TestControlBuilderWithOnClick;
var
  Builder: TControlBuilder;
  Button: TButton;
begin
  Button := nil;
  Builder := TControlBuilder.Create(TButton);
  Builder.WithOnClick(@ButtonClick);
  try
    Button := Builder.Build as TButton;
    Button.Click; //simula o click
    AssertNotNull('Button não deveria ser nil', Button);
    AssertEquals('Propriedade FClicked diferente da esperada', True, FClicked);
  finally
    Button.Free;
  end;
end;

procedure TControlBuilderTests.TestControlBuilderWithOwner;
var
  Builder: TControlBuilder;
  Panel: TPanel;
  Form: TForm;
begin
  Form := TForm.CreateNew(nil);
  Builder := TControlBuilder.Create(TPanel);
  Builder.Owner := Form;
  try
    Panel := Builder.Build as TPanel;

    AssertNotNull('Panel não deveria ser nil', Panel);
    AssertSame('Owner do Panel deveria ser o Form', Form, Panel.Owner);
  finally
    Form.Free;  // já libera o Panel também
  end;
end;

procedure TControlBuilderTests.TestControlBuilderWithParent;
var
  Builder: TControlBuilder;
  Form: TForm;
  ParentPanel, ChildPanel: TPanel;
begin
  Form := TForm.CreateNew(nil);
  try
    ParentPanel := TPanel.Create(Form);
    ParentPanel.Parent := Form;

    Builder := TControlBuilder.Create(TPanel);
    Builder.Owner := Form;
    Builder.Parent := ParentPanel; // aqui testamos o Parent
    ChildPanel := Builder.Build as TPanel;

    AssertNotNull('ChildPanel não deveria ser nil', ChildPanel);
    AssertSame('Owner do ChildPanel deveria ser o Form', Form, ChildPanel.Owner);
    AssertSame('Parent do ChildPanel deveria ser o ParentPanel', ParentPanel, ChildPanel.Parent);
  finally
    Form.Free; // libera ParentPanel e ChildPanel também
  end;
end;

procedure TControlBuilderTests.TestControlBuilderWithConstraints;
var
  Builder: TControlBuilder;
  Form: TForm;
  Panel: TPanel;
begin
  Form := TForm.CreateNew(nil);
  try
    Builder := TControlBuilder.Create(TPanel);
    Builder.Owner := Form;
    Builder.WithProp('Constraints.MinWidth', 100);
    Builder.WithProp('Constraints.MinHeight', 80);
    Builder.WithProp('Constraints.MaxWidth', 200);
    Builder.WithProp('Constraints.MaxHeight', 150);

    Panel := Builder.Build as TPanel;

    AssertNotNull('Panel não deveria ser nil', Panel);
    AssertSame('Owner deveria ser o Form', Form, Panel.Owner);

    AssertEquals('Constraints.MinWidth não aplicado corretamente', 100, Panel.Constraints.MinWidth);
    AssertEquals('Constraints.MinHeight não aplicado corretamente', 80, Panel.Constraints.MinHeight);
    AssertEquals('Constraints.MaxWidth não aplicado corretamente', 200, Panel.Constraints.MaxWidth);
    AssertEquals('Constraints.MaxHeight não aplicado corretamente', 150, Panel.Constraints.MaxHeight);
  finally
    Form.Free;  // libera Panel também
  end;
end;

procedure TControlBuilderTests.TestControlBuilderWithInexistentProp;
var
  Builder: TControlBuilder;
  Control: TControl;
  Raised: Boolean;
begin
  Raised := False;
  Builder := TControlBuilder.Create(TPanel);
  Control := nil;
  try
    Builder.WithProp('FooBar', 123); // propriedade inexistente
    try
      Control := Builder.Build;  // aqui que deve lançar
    except
      on E: EPropertyError do
        Raised := True;
    end;

    AssertTrue('Deveria lançar EPropertyError ao tentar setar propriedade inexistente', Raised);
  finally
    Control.Free;
  end;
end;

procedure TControlBuilderTests.TestControlBuilderWithInvalidType;
var
  Builder: TControlBuilder;
  Control: TControl;
  Raised: Boolean;
begin
  Raised := False;
  Builder := TControlBuilder.Create(TPanel);
  Control := nil;
  try
    // Passando string em propriedade Integer
    Builder.WithProp('Height', 'abc');

    try
      Control := Builder.Build;
    except
      on E: EPropertyError do
        Raised := True;  // exceção personalizada
      on E: Exception do
        Raised := True;  // ou outra exceção do RTTI
    end;

    AssertTrue('Deveria lançar exceção ao tentar setar tipo inválido', Raised);
  finally
    Control.Free;
  end;
end;

procedure TControlBuilderTests.TestControlBuilderWithOutOfRangeValue;
var
  Builder: TControlBuilder;
  Panel: TPanel;
begin
  Panel := nil;
  Builder := TControlBuilder.Create(TPanel);
  try
    // Passando valor negativo para Height (normalmente não faria sentido)
    Builder.WithProp('Height', -50);

    Panel := Builder.Build as TPanel;

    AssertNotNull('Panel não deveria ser nil', Panel);
    AssertEquals('Height não deveria aceitar valor fora do range', 0, Panel.Height);
  finally
    Panel.Free;
  end;
end;

procedure TControlBuilderTests.TestControlBuilderReadOnlyProp;
var
  Builder: TControlBuilder;
  Panel: TPanelWithReadOnlyProp;
begin
  Panel := nil;
  Builder := TControlBuilder.Create(TPanelWithReadOnlyProp);
  try
    Builder.WithProp('ReadOnlyProp', 99);  // ReadOnlyProp retorna sempre 42
    Panel := Builder.Build as TPanelWithReadOnlyProp;
    AssertEquals(
      'Propriedade read-only ReadOnlyProp não deveria ser alterada',
      42, Panel.ReadOnlyProp
    );
  finally
    Panel.Free;
  end;
end;

procedure TControlBuilderTests.TestControlBuilderSetupAndWithName;
var
  Builder: TControlBuilder;
  Control: TControl;
begin
  Control := nil;
  Builder := TControlBuilder.Create;
  Builder
    .WithProp('Tag', 7)
    .WithName('ABC')
  ;

  try
    Control := Builder.Build;  // auto free Builder
    AssertNotNull('Control não deveria ser nil', Control);
  finally
    Control.Free;
  end;
end;


initialization
  RegisterTest(TControlBuilderTests);

end.

