unit UControlBuilderTests;

interface

uses
  DUnitX.TestFramework, Vcl.Controls, Vcl.ExtCtrls, OPCB;

type
  TPanelWithReadOnlyProp = class(TPanel)
  private
    function GetReadOnlyProp: Integer;
  published
    property ReadOnlyProp: Integer read GetReadOnlyProp;
  end;

  [TestFixture]
  TControlBuilderTest = class
  protected
    FDestroyed: Boolean;
    FClicked: Boolean;
    procedure BuilderDestroy(Sender: TObject);
    procedure SetupControl_1(AControl: TControl);
    procedure SetupControl_2(AControl: TControl);
    procedure ButtonClick(Sender: TObject);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test] procedure TestControlBuilder;
    [Test] procedure TestControlBuilderDescendetControlBuild;
    [Test] procedure TestControlBuilderAssignsReference;
    [Test] procedure TestControlBuilderAssignsMultiReferences;
    [Test] procedure TestControlBuilderAssignsName;
    [Test] procedure TestControlBuilderMultiplesBuilds;
    [Test] procedure TestControlBuilderMultiplesBuildsWithReferences;
    [Test] procedure TestControlBuilderResetReferences;
    [Test] procedure TestControlBuilderSetup;
    [Test] procedure TestControlBuilderMultipleSetups;
    [Test] procedure TestControlBuilderWithProp;
    [Test] procedure TestControlBuilderWithPropBoolean;
    [Test] procedure TestControlBuilderWithPropPath;
    [Test] procedure TestControlBuilderWithPropPathSetOf;
    [Test] procedure TestControlBuilderWithPropPathColor;
    [Test] procedure TestControlBuilderWithPropObj;
    [Test] procedure TestControlBuilderWithPropObjWithPropValue;
    [Test] procedure TestControlBuilderWithPropObjNegative;
    [Test] procedure TestControlBuilderWithPropEnum;
    [Test] procedure TestControlBuilderWithPropSetOf;
    [Test] procedure TestControlBuilderWithName;
    [Test] procedure TestControlBuilderWithTag;
    [Test] procedure TestControlBuilderWithWidth;
    [Test] procedure TestControlBuilderWithHeight;
    [Test] procedure TestControlBuilderWithTop;
    [Test] procedure TestControlBuilderWithLeft;
    [Test] procedure TestControlBuilderWithCaption;
    [Test] procedure TestControlBuilderWithText;
    [Test] procedure TestControlBuilderWithTextAsCaption;
    [Test] procedure TestControlBuilderWithOnClick;
    [Test] procedure TestControlBuilderWithOwner;
    [Test] procedure TestControlBuilderWithParent;
    [Test] procedure TestControlBuilderWithConstraints;
    [Test] procedure TestControlBuilderWithInexistentProp;
    [Test] procedure TestControlBuilderWithInvalidType;
    [Test] procedure TestControlBuilderWithOutOfRangeValue;
    [Test] procedure TestControlBuilderReadOnlyProp;
    [Test] procedure TestControlBuilderSetupAndWithName;
    [Test] procedure TestControlBuilderWithEvent;
  end;

implementation

uses
  Vcl.StdCtrls, Vcl.Menus, Vcl.Graphics, Vcl.Forms, System.RTTI, System.TypInfo,
  System.SysUtils;

{ TPanelWithReadOnlyProp }

function TPanelWithReadOnlyProp.GetReadOnlyProp: Integer;
begin
  Result := 42;
end;

procedure TControlBuilderTest.Setup;
begin
  FClicked := False;
end;

procedure TControlBuilderTest.TearDown;
begin
end;

procedure TControlBuilderTest.BuilderDestroy(Sender: TObject);
begin
  FDestroyed := True;
end;

procedure TControlBuilderTest.SetupControl_1(AControl: TControl);
begin
  (AControl as TPanel).Height := 299;
end;

procedure TControlBuilderTest.SetupControl_2(AControl: TControl);
begin
  (AControl as TPanel).Width := 177;
end;

procedure TControlBuilderTest.ButtonClick(Sender: TObject);
begin
  FClicked := True;
end;

procedure TControlBuilderTest.TestControlBuilder;
var
  Builder: TControlBuilder;
  Control: TControl;
begin
  Control := nil;
  Builder := TControlBuilder.Create;
  try
    Control := Builder.Build;
    Assert.IsNotNull(Control, 'Control nao deveria ser nil');
  finally
    Control.Free;
    Builder.Free;
  end;
end;

procedure TControlBuilderTest.TestControlBuilderDescendetControlBuild;
var
  Builder: TControlBuilder;
  Control: TControl;
begin
  Control := nil;
  Builder := TControlBuilder.Create(TPanel);
  try
    Control := Builder.Build;
    Assert.IsNotNull(Control, 'Control nao deveria ser nil');
    Assert.AreEqual(TPanel, Control.ClassType,
      'Control deveria ser exatamente da classe TPanel');
  finally
    Control.Free;
    Builder.Free;
  end;
end;

procedure TControlBuilderTest.TestControlBuilderAssignsReference;
var
  Builder: TControlBuilder;
  Control: TControl;
begin
  Control := nil;
  Builder := TControlBuilder.Create(TPanel, Control);
  try
    Builder.Build;
    Assert.IsNotNull(Control, 'Control nao deveria ser nil');
  finally
    Control.Free;
    Builder.Free;
  end;
end;

procedure TControlBuilderTest.TestControlBuilderAssignsMultiReferences;
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
    Control_Build := Builder.Build;

    Assert.IsNotNull(Control_1, 'Control_1 nao deveria ser nil');
    Assert.IsNotNull(Control_2, 'Control_2 nao deveria ser nil');
    Assert.IsNotNull(Control_3, 'Control_3 nao deveria ser nil');
    Assert.IsNotNull(Control_Build, 'Control_Build nao deveria ser nil');

    Assert.AreEqual(TPanel, Control_1.ClassType, 'Control_1 deveria ser TPanel');
    Assert.AreEqual(TPanel, Control_2.ClassType, 'Control_2 deveria ser TPanel');
    Assert.AreEqual(TPanel, Control_3.ClassType, 'Control_3 deveria ser TPanel');
    Assert.AreEqual(TPanel, Control_Build.ClassType, 'Control_Build deveria ser TPanel');

    Assert.IsTrue(
      (Control_1 = Control_2) and
      (Control_2 = Control_3) and
      (Control_3 = Control_Build),
      'Todas as referencias devem apontar para o mesmo objeto'
    );
  finally
    Control_1.Free;
    Builder.Free;
  end;
end;

procedure TControlBuilderTest.TestControlBuilderAssignsName;
var
  Builder: TControlBuilder;
  Control: TControl;
begin
  Control := nil;
  Builder := TControlBuilder.Create(TPanel, 'PanelTeste');
  try
    Control := Builder.Build;
    Assert.IsNotNull(Control, 'Control nao deveria ser nil');
    Assert.AreEqual('PanelTeste', Control.Name, 'Control com nome diferente do esperado');
  finally
    Control.Free;
    Builder.Free;
  end;
end;

procedure TControlBuilderTest.TestControlBuilderMultiplesBuilds;
var
  Builder: TControlBuilder;
  Control1, Control2: TControl;
begin
  Builder := TControlBuilder.Create(TPanel);

  Control1 := Builder.Build;
  Control2 := Builder.Build;

  Assert.IsNotNull(Control1);
  Assert.IsNotNull(Control2);
  Assert.IsTrue(Control1 <> Control2, 'Cada Build deve retornar uma nova instancia');

  Control1.Free;
  Control2.Free;
  Builder.Free;
end;

procedure TControlBuilderTest.TestControlBuilderMultiplesBuildsWithReferences;
var
  Builder: TControlBuilder;
  FirstInstance, Control_1, Control_2: TControl;
  Control_Build: TControl;
begin
  Control_1 := nil;
  Control_2 := nil;

  Builder := TControlBuilder.Create(TPanel, Control_1);
  Builder.Assign(Control_2);

  try
    Control_Build := Builder.Build;
    Assert.IsNotNull(Control_Build);
    Assert.IsNotNull(Control_1);
    Assert.IsNotNull(Control_2);
    Assert.IsTrue(
      (Control_Build = Control_1) and (Control_1 = Control_2),
      'Todas as referencias devem apontar para a mesma instancia'
    );

    FirstInstance := Control_Build;

    Control_Build := Builder.Build;
    Assert.IsNotNull(Control_Build);
    Assert.IsNotNull(Control_1);
    Assert.IsNotNull(Control_2);

    Assert.IsTrue(Control_Build <> FirstInstance, 'Cada Build deve retornar uma nova instancia');

    Assert.IsTrue(
      (Control_Build = Control_1) and (Control_1 = Control_2),
      'Todas as referencias devem apontar para a nova instancia'
    );
  finally
    FirstInstance.Free; // instancia da primeira build, ja substituida em Control_1/Control_2
    Control_Build.Free; // instancia da segunda build (= Control_1 = Control_2)
    Builder.Free;
  end;
end;

procedure TControlBuilderTest.TestControlBuilderResetReferences;
var
  Builder: TControlBuilder;
  Control_1, Control_2, Control_3: TControl;
begin
  Control_1 := nil;
  Control_2 := nil;
  Control_3 := nil;

  Builder := TControlBuilder.Create(TPanel, Control_1);

  try
    Builder.Assign(Control_2).Build;

    Builder.ResetReferences;
    Builder.Assign(Control_3).Build;

    Assert.IsNotNull(Control_1, 'Control_1 nao deveria ser nil');
    Assert.IsNotNull(Control_2, 'Control_2 nao deveria ser nil');

    Assert.IsNotNull(Control_3, 'Control_3 nao deveria ser nil');
    Assert.IsTrue(Control_3 <> Control_1, 'Control_3 deveria ser diferente de Control_1');
    Assert.IsTrue(Control_3 <> Control_2, 'Control_3 deveria ser diferente de Control_2');
  finally
    Control_1.Free; // = Control_2 (mesma instancia da primeira build)
    Control_3.Free;
    Builder.Free;
  end;
end;

procedure TControlBuilderTest.TestControlBuilderSetup;
var
  Builder: TControlBuilder;
  Control: TControl;
begin
  Control := nil;
  Builder := TControlBuilder.Create(TPanel);
  Builder.Setup(SetupControl_1);
  try
    Control := Builder.Build;
    Assert.IsNotNull(Control, 'Control nao deveria ser nil');
    Assert.AreEqual(299, Control.Height, 'Propriedade Height de Control diferente da esperada');
  finally
    Control.Free;
    Builder.Free;
  end;
end;

procedure TControlBuilderTest.TestControlBuilderMultipleSetups;
var
  Builder: TControlBuilder;
  Control: TControl;
begin
  Control := nil;
  Builder := TControlBuilder.Create(TPanel);
  Builder.Setup(SetupControl_1);
  Builder.Setup(SetupControl_2);
  try
    Control := Builder.Build;
    Assert.IsNotNull(Control, 'Control nao deveria ser nil');
    Assert.AreEqual(299, Control.Height, 'Propriedade Height de Control diferente da esperada');
    Assert.AreEqual(177, Control.Width, 'Propriedade Width de Control diferente da esperada');
  finally
    Control.Free;
    Builder.Free;
  end;
end;

procedure TControlBuilderTest.TestControlBuilderWithProp;
var
  Builder: TControlBuilder;
  Control: TControl;
begin
  Control := nil;
  Builder := TControlBuilder.Create(TPanel);
  Builder.WithProp('Height', 288);
  Builder.WithProp('Width', 344);
  try
    Control := Builder.Build;
    Assert.IsNotNull(Control, 'Control nao deveria ser nil');
    Assert.AreEqual(288, Control.Height, 'Propriedade Height de Control diferente da esperada');
    Assert.AreEqual(344, Control.Width, 'Propriedade Width de Control diferente da esperada');
  finally
    Control.Free;
    Builder.Free;
  end;
end;

procedure TControlBuilderTest.TestControlBuilderWithPropBoolean;
var
  Builder: TControlBuilder;
  Button: TButton;
begin
  Button := nil;
  Builder := TControlBuilder.Create(TButton);
  Builder.WithProp('Enabled', False);
  try
    Button := Builder.Build as TButton;
    Assert.IsNotNull(Button, 'Button nao deveria ser nil');
    Assert.AreEqual(False, Button.Enabled, 'Propriedade Enabled de Button diferente da esperada');
  finally
    Button.Free;
    Builder.Free;
  end;
end;

procedure TControlBuilderTest.TestControlBuilderWithPropPath;
var
  Builder: TControlBuilder;
  Panel: TPanel;
begin
  Panel := nil;
  Builder := TControlBuilder.Create(TPanel);
  Builder.WithProp('Font.size', 25);
  try
    Panel := Builder.Build as TPanel;
    Assert.IsNotNull(Panel, 'Panel nao deveria ser nil');
    Assert.AreEqual(25, Panel.Font.Size, 'Propriedade Font Size de Panel diferente da esperada');
  finally
    Panel.Free;
    Builder.Free;
  end;
end;

procedure TControlBuilderTest.TestControlBuilderWithPropPathSetOf;
var
  Builder: TControlBuilder;
  Panel: TPanel;
  FontStyleValue: TFontStyles;
  FontStyleAsInt: Integer;
begin
  Panel := nil;
  Builder := TControlBuilder.Create(TPanel);
  FontStyleValue := [fsBold, fsItalic];
  // TFontStyles nao tem o mesmo tamanho em bytes de Integer no Delphi
  // (diferente do FPC), entao um typecast direto Integer(FontStyleValue)
  // falha com "Invalid typecast". Move copia apenas os bytes reais do set,
  // zero-extendendo o resto - equivalente ao cast quando os tamanhos batem.
  FontStyleAsInt := 0;
  Move(FontStyleValue, FontStyleAsInt, SizeOf(FontStyleValue));
  Builder.WithPropSet('Font.Style', FontStyleAsInt);
  try
    Panel := Builder.Build as TPanel;
    Assert.IsNotNull(Panel, 'Panel nao deveria ser nil');
    Assert.IsTrue(fsBold in Panel.Font.Style, 'Font.Style deveria conter fsBold');
    Assert.IsTrue(fsItalic in Panel.Font.Style, 'Font.Style deveria conter fsItalic');
    Assert.IsFalse(fsUnderline in Panel.Font.Style, 'Font.Style nao deveria conter fsUnderline');
  finally
    Panel.Free;
    Builder.Free;
  end;
end;

procedure TControlBuilderTest.TestControlBuilderWithPropPathColor;
var
  Builder: TControlBuilder;
  Panel: TPanel;
begin
  Panel := nil;
  Builder := TControlBuilder.Create(TPanel);
  try
    Builder.WithProp('Font.Color', clRed);

    Panel := Builder.Build as TPanel;

    Assert.IsNotNull(Panel, 'Panel nao deveria ser nil');
    Assert.AreEqual(Integer(clRed), Integer(Panel.Font.Color), 'Font.Color diferente do esperado');
  finally
    Panel.Free;
    Builder.Free;
  end;
end;

procedure TControlBuilderTest.TestControlBuilderWithPropObj;
var
  Builder: TControlBuilder;
  Panel: TPanel;
  PopupMenu: TPopupMenu;
begin
  PopupMenu := TPopupMenu.Create(nil);

  Panel := nil;
  Builder := TControlBuilder.Create(TPanel);
  Builder.WithPropObj('PopupMenu', PopupMenu);

  try
    Panel := Builder.Build as TPanel;
    Assert.IsNotNull(Panel, 'Panel nao deveria ser nil');
    Assert.IsTrue(PopupMenu = Panel.PopupMenu, 'Propriedade PopupMenu de Panel diferente da esperada');
  finally
    Panel.Free;
    PopupMenu.Free;
    Builder.Free;
  end;
end;

procedure TControlBuilderTest.TestControlBuilderWithPropObjWithPropValue;
var
  Builder: TControlBuilder;
  Panel: TPanel;
  PopupMenu: TPopupMenu;
begin
  PopupMenu := TPopupMenu.Create(nil);
  Panel := nil;
  Builder := TControlBuilder.Create(TPanel);
  Builder.WithProp(TPropertyValue.Create<TPopupMenu>('PopupMenu', PopupMenu));
  try
    Panel := Builder.Build as TPanel;
    Assert.IsNotNull(Panel, 'Panel nao deveria ser nil');
    Assert.IsTrue(PopupMenu = Panel.PopupMenu, 'Propriedade PopupMenu de Panel diferente da esperada');
  finally
    Panel.Free;
    PopupMenu.Free;
    Builder.Free;
  end;
end;

procedure TControlBuilderTest.TestControlBuilderWithPropObjNegative;
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
    Assert.IsNotNull(Panel, 'Panel nao deveria ser nil');
    Assert.IsTrue(PopupMenu1 = Panel.PopupMenu, 'PopupMenu deveria ser exatamente o PopupMenu1');
    Assert.IsTrue(PopupMenu2 <> Panel.PopupMenu, 'PopupMenu nao deveria ser o PopupMenu2');
  finally
    Panel.Free;
    PopupMenu1.Free;
    PopupMenu2.Free;
    Builder.Free;
  end;
end;

procedure TControlBuilderTest.TestControlBuilderWithPropEnum;
var
  Builder: TControlBuilder;
  Panel: TPanel;
begin
  Panel := nil;
  Builder := TControlBuilder.Create(TPanel);

  Builder.WithProp(TPropertyValue.Create<TAlign>('Align', alRight));

  try
    Panel := Builder.Build as TPanel;

    Assert.IsNotNull(Panel, 'Panel nao deveria ser nil');
    Assert.AreEqual(
      Ord(alRight),
      Ord(Panel.Align),
      'Propriedade Align de Panel diferente da esperada'
    );
  finally
    Panel.Free;
    Builder.Free;
  end;
end;

procedure TControlBuilderTest.TestControlBuilderWithPropSetOf;
var
  Builder: TControlBuilder;
  Panel: TPanel;
  AnchorsValue: TAnchors;
  AnchorsAsInt: Integer;
begin
  Panel := nil;
  Builder := TControlBuilder.Create(TPanel);
  try
    AnchorsValue := [akBottom, akLeft];
    // Ver comentario em TestControlBuilderWithPropPathSetOf: TAnchors nao
    // tem o mesmo tamanho em bytes de Integer no Delphi.
    AnchorsAsInt := 0;
    Move(AnchorsValue, AnchorsAsInt, SizeOf(AnchorsValue));
    Builder.WithPropSet('Anchors', AnchorsAsInt);
    Panel := Builder.Build as TPanel;

    Assert.IsNotNull(Panel, 'Panel nao deveria ser nil');
    Assert.IsTrue(akLeft in Panel.Anchors, 'Anchors deveria conter akLeft');
    Assert.IsFalse(akTop in Panel.Anchors, 'Anchors nao deveria conter akTop');
    Assert.IsFalse(akRight in Panel.Anchors, 'Anchors nao deveria conter akRight');
    Assert.IsTrue(akBottom in Panel.Anchors, 'Anchors deveria conter akBottom');
  finally
    Panel.Free;
    Builder.Free;
  end;
end;

procedure TControlBuilderTest.TestControlBuilderWithName;
var
  Builder: TControlBuilder;
  Panel: TPanel;
begin
  Panel := nil;
  Builder := TControlBuilder.Create(TPanel);
  Builder.WithName('PanelTeste');
  try
    Panel := Builder.Build as TPanel;
    Assert.IsNotNull(Panel, 'Panel nao deveria ser nil');
    Assert.AreEqual('PanelTeste', Panel.Name, 'Propriedade Name de Panel diferente da esperada');
  finally
    Panel.Free;
    Builder.Free;
  end;
end;

procedure TControlBuilderTest.TestControlBuilderWithTag;
var
  Builder: TControlBuilder;
  Panel: TPanel;
begin
  Panel := nil;
  Builder := TControlBuilder.Create(TPanel);
  Builder.WithTag(77);
  try
    Panel := Builder.Build as TPanel;
    Assert.IsNotNull(Panel, 'Panel nao deveria ser nil');
    Assert.AreEqual(NativeInt(77), Panel.Tag, 'Propriedade Tag diferente da esperada');
  finally
    Panel.Free;
    Builder.Free;
  end;
end;

procedure TControlBuilderTest.TestControlBuilderWithWidth;
var
  Builder: TControlBuilder;
  Panel: TPanel;
begin
  Panel := nil;
  Builder := TControlBuilder.Create(TPanel);
  Builder.WithWidth(205);
  try
    Panel := Builder.Build as TPanel;
    Assert.IsNotNull(Panel, 'Panel nao deveria ser nil');
    Assert.AreEqual(205, Panel.Width, 'Propriedade Width diferente da esperada');
  finally
    Panel.Free;
    Builder.Free;
  end;
end;

procedure TControlBuilderTest.TestControlBuilderWithHeight;
var
  Builder: TControlBuilder;
  Panel: TPanel;
begin
  Panel := nil;
  Builder := TControlBuilder.Create(TPanel);
  Builder.WithHeight(154);
  try
    Panel := Builder.Build as TPanel;
    Assert.IsNotNull(Panel, 'Panel nao deveria ser nil');
    Assert.AreEqual(154, Panel.Height, 'Propriedade Height diferente da esperada');
  finally
    Panel.Free;
    Builder.Free;
  end;
end;

procedure TControlBuilderTest.TestControlBuilderWithTop;
var
  Builder: TControlBuilder;
  Panel: TPanel;
begin
  Panel := nil;
  Builder := TControlBuilder.Create(TPanel);
  Builder.WithTop(25);
  try
    Panel := Builder.Build as TPanel;
    Assert.IsNotNull(Panel, 'Panel nao deveria ser nil');
    Assert.AreEqual(25, Panel.Top, 'Propriedade Top diferente da esperada');
  finally
    Panel.Free;
    Builder.Free;
  end;
end;

procedure TControlBuilderTest.TestControlBuilderWithLeft;
var
  Builder: TControlBuilder;
  Panel: TPanel;
begin
  Panel := nil;
  Builder := TControlBuilder.Create(TPanel);
  Builder.WithLeft(32);
  try
    Panel := Builder.Build as TPanel;
    Assert.IsNotNull(Panel, 'Panel nao deveria ser nil');
    Assert.AreEqual(32, Panel.Left, 'Propriedade Left diferente da esperada');
  finally
    Panel.Free;
    Builder.Free;
  end;
end;

procedure TControlBuilderTest.TestControlBuilderWithCaption;
var
  Builder: TControlBuilder;
  Panel: TPanel;
begin
  Panel := nil;
  Builder := TControlBuilder.Create(TPanel);
  Builder.WithCaption('TesteCaption');
  try
    Panel := Builder.Build as TPanel;
    Assert.IsNotNull(Panel, 'Panel nao deveria ser nil');
    Assert.AreEqual('TesteCaption', Panel.Caption, 'Propriedade Caption diferente da esperada');
  finally
    Panel.Free;
    Builder.Free;
  end;
end;

procedure TControlBuilderTest.TestControlBuilderWithText;
var
  Builder: TControlBuilder;
  Edit: TEdit;
begin
  Edit := nil;
  Builder := TControlBuilder.Create(TEdit);
  Builder.WithText('TesteText');
  try
    Edit := Builder.Build as TEdit;
    Assert.IsNotNull(Edit, 'Edit nao deveria ser nil');
    Assert.AreEqual('TesteText', Edit.Text, 'Propriedade Text diferente da esperada');
  finally
    Edit.Free;
    Builder.Free;
  end;
end;

procedure TControlBuilderTest.TestControlBuilderWithTextAsCaption;
var
  Builder: TControlBuilder;
  Panel: TPanel;
begin
  Panel := nil;
  Builder := TControlBuilder.Create(TPanel);
  Builder.WithText('TesteCaption');
  try
    Panel := Builder.Build as TPanel;
    Assert.IsNotNull(Panel, 'Panel nao deveria ser nil');
    Assert.AreEqual('TesteCaption', Panel.Caption, 'Propriedade Caption diferente da esperada');
  finally
    Panel.Free;
    Builder.Free;
  end;
end;

procedure TControlBuilderTest.TestControlBuilderWithOnClick;
var
  Builder: TControlBuilder;
  Button: TButton;
begin
  Button := nil;
  Builder := TControlBuilder.Create(TButton);
  Builder.WithOnClick(ButtonClick);
  try
    Button := Builder.Build as TButton;
    Button.Click;
    Assert.IsNotNull(Button, 'Button nao deveria ser nil');
    Assert.AreEqual(True, FClicked, 'Propriedade FClicked diferente da esperada');
  finally
    Button.Free;
    Builder.Free;
  end;
end;

procedure TControlBuilderTest.TestControlBuilderWithOwner;
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

    Assert.IsNotNull(Panel, 'Panel nao deveria ser nil');
    Assert.IsTrue(Form = Panel.Owner, 'Owner do Panel deveria ser o Form');
  finally
    Form.Free;
    Builder.Free;
  end;
end;

procedure TControlBuilderTest.TestControlBuilderWithParent;
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
    Builder.Parent := ParentPanel;
    ChildPanel := Builder.Build as TPanel;

    Assert.IsNotNull(ChildPanel, 'ChildPanel nao deveria ser nil');
    Assert.IsTrue(Form = ChildPanel.Owner, 'Owner do ChildPanel deveria ser o Form');
    Assert.IsTrue(ParentPanel = ChildPanel.Parent, 'Parent do ChildPanel deveria ser o ParentPanel');
  finally
    Form.Free;
    Builder.Free;
  end;
end;

procedure TControlBuilderTest.TestControlBuilderWithConstraints;
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

    Assert.IsNotNull(Panel, 'Panel nao deveria ser nil');
    Assert.IsTrue(Form = Panel.Owner, 'Owner deveria ser o Form');

    // Constraints.MinWidth/etc sao do tipo TConstraintSize (nao Integer
    // puro); o cast explicito evita falha de inferencia do generic AreEqual.
    Assert.AreEqual(100, Integer(Panel.Constraints.MinWidth), 'Constraints.MinWidth nao aplicado corretamente');
    Assert.AreEqual(80, Integer(Panel.Constraints.MinHeight), 'Constraints.MinHeight nao aplicado corretamente');
    Assert.AreEqual(200, Integer(Panel.Constraints.MaxWidth), 'Constraints.MaxWidth nao aplicado corretamente');
    Assert.AreEqual(150, Integer(Panel.Constraints.MaxHeight), 'Constraints.MaxHeight nao aplicado corretamente');
  finally
    Form.Free;
    Builder.Free;
  end;
end;

procedure TControlBuilderTest.TestControlBuilderWithInexistentProp;
var
  Builder: TControlBuilder;
  Control: TControl;
  Raised: Boolean;
begin
  Raised := False;
  Builder := TControlBuilder.Create(TPanel);
  Control := nil;
  try
    Builder.WithProp('FooBar', 123);
    try
      Control := Builder.Build;
    except
      on E: EPropertyError do
        Raised := True;
    end;

    Assert.IsTrue(Raised, 'Deveria lancar EPropertyError ao tentar setar propriedade inexistente');
  finally
    Control.Free;
    Builder.Free;
  end;
end;

procedure TControlBuilderTest.TestControlBuilderWithInvalidType;
var
  Builder: TControlBuilder;
  Control: TControl;
  Raised: Boolean;
begin
  Raised := False;
  Builder := TControlBuilder.Create(TPanel);
  Control := nil;
  try
    Builder.WithProp('Height', 'abc');

    try
      Control := Builder.Build;
    except
      on E: EPropertyError do
        Raised := True;
      on E: Exception do
        Raised := True;
    end;

    Assert.IsTrue(Raised, 'Deveria lancar excecao ao tentar setar tipo invalido');
  finally
    Control.Free;
    Builder.Free;
  end;
end;

procedure TControlBuilderTest.TestControlBuilderWithOutOfRangeValue;
var
  Builder: TControlBuilder;
  Panel: TPanel;
begin
  Panel := nil;
  Builder := TControlBuilder.Create(TPanel);
  try
    Builder.WithProp('Height', -50);

    Panel := Builder.Build as TPanel;

    // Diferente do LCL (que zera Height negativo), o VCL aceita o valor
    // negativo como esta, sem clamping - a lib so repassa o valor via RTTI,
    // quem decide o comportamento e o proprio widgetset.
    Assert.IsNotNull(Panel, 'Panel nao deveria ser nil');
    Assert.AreEqual(-50, Panel.Height, 'Height deveria aceitar o valor repassado pelo WithProp sem clamping no VCL');
  finally
    Panel.Free;
    Builder.Free;
  end;
end;

procedure TControlBuilderTest.TestControlBuilderReadOnlyProp;
var
  Builder: TControlBuilder;
  Panel: TPanelWithReadOnlyProp;
begin
  Panel := nil;
  Builder := TControlBuilder.Create(TPanelWithReadOnlyProp);
  try
    Builder.WithProp('ReadOnlyProp', 99);
    Panel := Builder.Build as TPanelWithReadOnlyProp;
    Assert.AreEqual(42, Panel.ReadOnlyProp,
      'Propriedade read-only ReadOnlyProp nao deveria ser alterada');
  finally
    Panel.Free;
    Builder.Free;
  end;
end;

procedure TControlBuilderTest.TestControlBuilderSetupAndWithName;
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
    Control := Builder.Build;
    Assert.IsNotNull(Control, 'Control nao deveria ser nil');
  finally
    Control.Free;
    Builder.Free;
  end;
end;

procedure TControlBuilderTest.TestControlBuilderWithEvent;
var
  Builder: TControlBuilder;
  Button: TControl;
begin
  Button := nil;
  Builder := TControlBuilder.Create(TButton);
  Builder.WithEvent('OnClick', Self, @TControlBuilderTest.ButtonClick);
  try
    Button := Builder.Build;
    TButton(Button).Click;
    Assert.IsNotNull(Button, 'Button nao deveria ser nil');
    Assert.AreEqual(True, FClicked, 'Propriedade FClicked diferente da esperada');
  finally
    Button.Free;
    Builder.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TControlBuilderTest);

end.
