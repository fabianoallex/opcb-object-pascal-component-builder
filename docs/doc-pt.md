# Documentação

* [Visão geral](#visão-geral)
    * [Sobre a biblioteca](#sobre-a-biblioteca)
    * [Benefícios](#benefícios)
    * [Licença - Direitos autorais](#licença---direitos-autorais)
* [Instalação e configuração](#instalação-e-configuração)
* [Classes e Interfaces](#classes-e-interfaces)
    * [Builders](#builders)
        * [IObjectBuilder](#iobjectbuilder)
            * [TObjectBuilderBase](#tobjectbuilderbase)
            * [TObjectBuilder](#tobjectbuilder)
        * [IComponentBuilder](#icomponentbuilder)
            * [TComponentBuilderBase](#tcomponentbuilderbase)
            * [TComponentBuilder](#tcomponentbuilder)
        * [IControlBuilder](#icontrolbuilder)
            * [TControlBuilderBase](#tcontrolbuilderbase)
            * [TControlBuilder](#tcontrolbuilder)
        * [IMenuBuilder](#imenubuilder)
            * [TMenuBuilderBase](#tmenubuilderbase)
            * [TMenuBuilder](#tmenubuilder)
        * [IMenuItemBuilder](#imenuitembuilder)
            * [TMenuItemBuilderBase](#tmenuitembuilderbase)
            * [TMenuItemBuilder](#tmenuitembuilder)
    * [Creators](#creators)
        * [TComponentCreator](#tcomponentcreator)
        * [TControlCreator](#tcontrolcreator)
        * [TMenuCreator](#tmenucreator)
        * [TControlCreator](#tcontrolcreator-1)
        * [TOPCBCreators](#topcbcreators)


## Visão geral

### Sobre a biblioteca

A OPCB (Object Pascal Component Builder) é uma biblioteca desenvolvida em Object Pascal, criada para simplificar a criação e configuração de componentes visuais e não-visuais em aplicações Delphi e Lazarus.
Seu objetivo é reduzir a complexidade do código, eliminar repetição e fornecer uma abordagem fluente para a construção de componentes, tornando os projetos mais fáceis de manter, escalar e evoluir.

### Benefícios

Uma das principais vantagens da OPCB é permitir a criação de componentes diretamente no código, dispensando o uso de arquivos .dfm (Delphi) ou .lfm (Lazarus). Dessa forma, toda a lógica e configuração dos componentes ficam centralizadas em unidades .pas, o que oferece vários benefícios práticos:

* Compatibilidade entre Delphi e Lazarus: facilita a portabilidade de projetos entre as duas IDEs.
* Substituição de componentes simplificada;
* Facilidade de criar novos componentes com base nos existentes sem precisar instalá-los na IDE.
* Redução de conflitos em sistemas de versionamento: evita problemas comuns com arquivos .dfm ou .lfm em repositórios Git.
* Código mais organizado e centralizado: todas as definições de componentes e layouts ficam em um único lugar, facilitando manutenção e revisão.

Em resumo, a OPCB torna o desenvolvimento de interfaces mais rápido, consistente e confiável, oferecendo flexibilidade e controle total sobre a criação de componentes.

---

### Licença - Direitos autorais

A OPCB (Object Pascal Component Builder) é distribuída sob a Apache License, Version 2.0 (Janeiro de 2004).

Isso significa que você pode usar, modificar e distribuir a biblioteca livremente, inclusive em projetos comerciais, desde que respeite os seguintes pontos:

1. **Notificação de copyright e licença**
    * Todos os arquivos da biblioteca devem manter a indicação de copyright e a referência à licença.
2. **Redistribuição de modificações**
    * Se você alterar a biblioteca e redistribuí-la, deve informar as mudanças realizadas.
3. **Sem garantias**
    * A biblioteca é fornecida “no estado em que se encontra”, sem qualquer garantia de funcionamento ou responsabilidade por danos.

**Recursos adicionais**

Você pode consultar o texto completo da licença no arquivo LICENSE do repositório.

A licença permite tanto uso em projetos open source quanto proprietários, oferecendo flexibilidade máxima para integração em diferentes contextos.

---

### Instalação e configuração

#### 🔹 Opção 1 – Clonar o repositório

```bash
git clone https://github.com/fabianoallex/opcb-object-pascal-component-builder.git
```

#### 🔹 Opção 2 – Download manual

Baixe o arquivo ZIP do repositório no GitHub e extraia o conteúdo em uma pasta de sua preferência.


#### 💠 Delphi

1. Abra o **Delphi**.  
2. Vá em **Tools ▸ Options ▸ Language ▸ Delphi Options ▸ Library**.  
3. No campo **Library Path**, adicione o caminho completo da pasta `src`.  
   Exemplo:  
   `C:\opcb\src`


#### 💠 Lazarus

1. Abra o **Lazarus**.  
2. Vá em **Project ▸ Project Options ▸ Compiler Options ▸ Paths**.  
3. Em **Other unit files (-Fu)**, adicione o caminho da pasta `src`.  
   Exemplo:  
   `C:\opcb\src`

### 🧰 Pronto!

Após isso, as units do projeto estarão disponíveis para uso em seus projetos Delphi ou Lazarus.

---

### Classes e Interfaces

#### Builders

Builders são classes utilizadas para facilitar a instanciação de objetos.


![Diagrama de classes](img/builder-class-diagram.png)

##### IObjectBuilder

Interface base que define as operações de construção de objetos genéricos.

| Método | Retorno | Descrição |
|--------|----------|------------|
| `Build: TBuild` | `TBuild` | Cria e retorna uma nova instância do tipo genérico `TBuild`. |
| `AssignReference(var AReference: TBuild)` | `void` | Atribui a instância criada por `.Build` à variável informada. |

---
#### `TObjectBuilderBase`

Classe abstrata que implementa a interface `IObjectBuilder<TBuild>`
 e fornece a infraestrutura principal para construção de objetos no padrão Builder.
É responsável por criar, configurar e preparar instâncias do tipo genérico `TBuild`, permitindo o uso de métodos fluentes e operações de configuração customizadas.


| Método | Retorno | Descrição |
|--------|----------|------------|
`CreateObject: TBuild; virtual; abstract;`|TBuild|Método abstrato a ser implementado pelas classes derivadas. Deve criar uma nova instância através do construtor definido em `ObjectClass`. 
`ConfigureObject(AObject: TBuild); virtual;`||Atribui as propriedades definidas pelo builder ao objeto instanciado após o `.Build`. Deve ser sobrescrito pelas classes que estendem `TObjectBuilderBase`, mas sempre chamando `inherited ConfigureObject(AObject);`.
`ApplyPropPath(Instance: TObject; AProp: TPropertyValue);` ||	Aplica ao objeto instanciado com `.Build` um valor de propriedade via RTTI usando caminho hierárquico (`'Prop.SubProp'`). Exemplo: `'Font.Size'`.
`ApplyPendindProps(Instance: TObject);`	||Aplica ao objeto instanciado com `.Build` todas as propriedades pendentes armazenadas em FProperties via RTTI.
`SetupProc(AProcObj: TSetupProcObj<TBuild>); overload;`||Adiciona uma referencia a um procedimento de configuração de objeto definido pelo usuário. Procedimento será executado durante a execução do método `.Build`.
`SetupProc(ARefProc: TSetupRefProc<TBuild>); overload;`||Adiciona uma referencia a um procedimento de configuração de objeto definido pelo usuário. Procedimento será executado durante a execução do método `.Build`.
`Create; overload;`	||Construtor.
`Create(AClass: TObjectClass); overload;`	||Construtor que recebe como parâmetro a classe a ser instanciada.
`Create(AClass: TObjectClass; out Reference); overload;`	||Construtor que recebe como parâmetro a classe a ser instanciada. Recebe também como parâmetro uma referência de uma variável para receber a instância criada.
`Destroy; override;`	||Destrutor.
`AssignReference(out Reference);`	||Atribui o objeto criado por `.Build` ao parâmetro recebido. Mantém a referencia em uma lista.
`ResetReferences;`	||Limpa lista de referências.
`Assign(out Reference): TSelf; overload;`	|`TSelf`|Atribui a referência e retorna a própria instância para uso fluente.
`WithProp(const APropName: string; const AValue: TValue): TSelf; overload;`	|`TSelf`|Define o valor de uma propriedade simples via RTTI.
`WithProp(const APropValue: TPropertyValue): TSelf; overload;`	|`TSelf`|Define uma propriedade do objeto via RTTI usando um registro TPropertyValue.
`WithPropObj(const APropName: string; AObj: TObject): TSelf; overload;`	|`TSelf`|Define uma propriedade do objeto via RTTI.
`WithPropSet(const APropName: string; const AValue: Integer): TSelf;`	|`TSelf`|Define uma propriedade de tipo set do objeto via RTTI.
`WithEvent(const AEventName: string; const AMethod: TMethod): TSelf; overload;`|`TSelf`|Define um evento do objeto via RTTI.
`WithEvent(const AEventName: string; const AInstance: TObject; const AMethod: Pointer): TSelf; overload;`|`TSelf`|Define um evento do objeto via RTTI.
`Setup(AProc: TSetupProcObjBuild): TSelf; overload;`	|`TSelf`|Adiciona um procedimento de configuração de objeto.
`Setup(AProc: TSetupRefProcBuild): TSelf; overload;`	|`TSelf`|Adiciona um procedimento de configuração de objecto.
`Build: TBuild;` |`TBuild`	|Cria, configura e retorna o objeto final do tipo `TBuild`.
`property ObjectClass:	TObjectClass`	||Retorna a classe associada à construção atual.

---
##### `TObjectBuilder`

Classe concreta que herda `TObjectBuilderbase`. Define tipo genérico `TBuild` como `TComponent`.

Exemplo de uso:

```pascal
uses
  OPCB;
  
type
  TMyClass = class
  private
    FBar: string;
    FFoo: Integer;
    procedure SetBar(const Value: string);
    procedure SetFoo(const Value: Integer);
  published
    property Foo: Integer read FFoo write SetFoo;
    property Bar: string read FBar write SetBar;
  end;

procedure TForm1.FormCreate(Sender: TObject);
var
  Builder: TObjectBuilder;
  MyObject: TMyClass;
begin
  Builder := TObjectBuilder.Create(TMyClass);

  try
    MyObject := Builder
      .WithProp('Foo', 10)
      .WithProp('Bar', 'bar')
      .Build as TMyClass;
  finally
    Builder.Free;
  end;

  with MyObject do
    ShowMessage(Format('%d - %s', [Foo, Bar]));
end;
```
---
#### `TObjectBuilder`

Classe concreta que herda de `TObjectBuilderBase<TObject, TObjectBuilder>`.

Fornece uma implementação genérica para construção de instâncias de `TObject`, permitindo criar objetos de qualquer classe derivada de `TObject` sem a necessidade de definir um tipo genérico específico.

É útil como builder universal para cenários onde o tipo concreto do objeto é determinado em tempo de execução.

Herança

    TObjectBuilder = class(TObjectBuilderBase<TObject, TObjectBuilder>)


| Método | Retorno | Descrição |
|--------|----------|------------|
`CreateObject: TObject; override;`|`TObject`|Cria uma nova instância do tipo `TObject` com base na classe definida em `FObjectClass`. Este método sobrescreve a implementação abstrata da classe base.
`Create; overload;`||Cria uma nova instância de TObjectBuilder associada à classe `TObject`.
`Create(out Reference); overload;`||Cria uma nova instância e atribui a referência do objeto construído à variável fornecida.
`Create(AClass: TObjectClass); overload;`	||Inicializa o builder associando-o a uma classe específica de objeto (`TObjectClass`).
`Create(AClass: TObjectClass; out Reference); overload;`	||Cria uma instância associada a uma classe e já atribui a referência de saída.

---

#### `IComponentBuilder`

A interface `IComponentBuilder<TBuild>` estende `IObjectBuilder<TBuild>`
 e define as operações específicas para construção de componentes (`TComponent` e descendentes).

Ela adiciona suporte ao gerenciamento de propriedades comuns de componentes — como `Name`, `Owner` e `Tag` —, permitindo que o processo de criação seja totalmente controlado de forma fluente e consistente.

Herança

    IComponentBuilder<TBuild> = interface(IObjectBuilder<TBuild>)

| Propriedade | Tipo | Descrição |
|--------|----------|------------|
`Name`|`string`|Define o nome do componente, equivalente à propriedade Name do TComponent.
`Tag`|	`NativeInt`|	Define o valor do identificador livre (Tag) associado ao componente.
`Owner`|`TComponent`|Define ou obtém o componente responsável por gerenciar o ciclo de vida do componente construído.

Métodos

| Propriedade | Tipo | Descrição |
|--------|----------|------------|
`GetName`|`string`|Retorna o nome a ser atribuído ao objeto a ser instanciado 
`SetName(AValue: string);`||Define o nome do componente a ser instanciado pelo `.Build`.
`GetTag`	|`NativeInt`|Retorna o valor da propriedade `Tag` a ser atribuída ao objeto instanciado.
`SetTag(AValue: NativeInt);`||Define o valor da propriedade `Tag` do objeto a ser instanciado.
`GetOwner`|`TComponent`|Retorna o `Owner` associado ao componente a ser instanciado.
`SetOwner(AValue: TComponent);`||Define o `Owner` responsável pelo ciclo de vida do componente a ser instanciado.

---

#### `TComponentBuilderBase`

Classe abstrata que estende `TObjectBuilderBase<TBuild, TSelf>`
 e implementa a interface `IComponentBuilder<TBuild>`.

Serve como base para todos os builders voltados à criação e configuração de componentes (TComponent e descendentes), adicionando suporte a propriedades típicas como `Name`, `Owner` e `Tag`.

Além disso, `TComponentBuilderBase` mantém a fluência dos métodos herdados da classe base (`TObjectBuilderBase`), preservando o retorno do tipo genérico `TSelf`.
Isso permite encadear chamadas entre os métodos herdados e os métodos específicos de componentes, garantindo uma experiência fluente, por exemplo:

```pascal
Builder
  .WithName('BtnOk')
  .WithTag(10)
  .WithProp('Caption', 'Confirmar')
  .Setup(SetupProc)
  .Build;
```

Herança

    TComponentBuilderBase<TBuild, TSelf> = class(TObjectBuilderBase<TBuild, TSelf>, IComponentBuilder<TBuild>)

Campos protegidos
| Propriedade | Tipo | Descrição |
|--------|----------|------------|
`FName`	|string|	Armazena o nome do componente.
`FTag`	|NativeInt|	Armazena o valor de identificação livre do componente.
`FOwner`	|TComponent|	Armazena o componente que será o owner da instância criada.

Métodos protegidos
| Propriedade | Descrição |
|----------|------------|
`ConfigureObject(AObject: TBuild); override;`|	Executa a configuração adicional do componente após sua criação, aplicando `Name`, `Tag` e `Owner` conforme definidos.

Métodos públicos
| Método | Tipo | Descrição |
|--------|----------|------------|
`Create(AClass: TComponentClass; const AName: string=''); overload;`	||Construtor. Cria o builder para um tipo de componente e opcionalmente define o nome inicial.
`Create(AClass: TComponentClass; const AName: string; out Reference); overload;`	||Construtor. Cria o builder para um tipo de componente, define o nome e atribui a referência do objeto criado.
`Create(AClass: TComponentClass; out Reference); overload;`	||Construtor. Cria o builder e atribui a referência sem definir o nome.
`GetName`|`string`|Retorna o nome atual configurado para o componente.
`GetOwner: TComponent;`	||Retorna o owner atualmente associado ao componente.
`GetTag`	|`NativeInt`|Retorna o valor atual configurado para a propriedade `Tag`.
`SetName(AValue: string);`	||Define o nome do componente que será atribuído durante a construção.
`SetOwner(AValue: TComponent);`	||Define o owner responsável pelo ciclo de vida do componente.
`SetTag(AValue: NativeInt);`	||Define o valor da propriedade `Tag` do componente.
`WithName(AName: string)`|`TSelf`|	Define o nome do componente e retorna a própria instância (método fluente).
`WithTag(ATag: NativeInt)`|`TSelf`|	Define o valor da propriedade `Tag` e retorna a própria instância (método fluente).

Propriedades
| Propriedade | Tipo | Descrição |
|--------|----------|------------|
`Name`	|`string`|	Nome do componente a ser criado.
`Tag`|	`NativeInt`|	Valor numérico associado ao componente.
`Owner`	|`TComponent`|	Componente owner responsável pela instância criada.

---
#### `TComponentBuilder`

A classe `TComponentBuilder` é uma especialização concreta da classe genérica `TComponentBuilderBase`, projetada para facilitar a criação de instâncias de `TComponent` (ou de qualquer descendente) de forma fluente e configurável.

Ela oferece construtores sobrecarregados que permitem:

* Criar o componente sem nome nem referência.
* Criar o componente com nome definido.
* Criar o componente atribuindo automaticamente a instância a uma variável externa.

O método protegido `CreateObject` é sobrescrito para realizar a instanciação efetiva do componente, utilizando o tipo informado no construtor.

Assim como as demais classes Builder da biblioteca, `TComponentBuilder` mantém a fluência herdada dos métodos da classe base (`TComponentBuilderBase`), permitindo encadeamento de chamadas com retorno do tipo `TSelf`.

Isso significa que métodos herdados e específicos podem ser combinados livremente, proporcionando uma construção legível e expressiva.

| Construtor | Descrição |
|--------|------------|
`Create`|	Cria o builder sem parâmetros, utilizando como classe a ser instanciada `TComponent`.
`Create(AClass: TComponentClass; const AName: string = '')`|	Cria o builder para a classe informada, opcionalmente definindo o nome do componente.
`Create(AClass: TComponentClass; const AName: string; out Reference)`|Cria o builder e atribui a instância criada à variável `Reference`.
`Create(AClass: TComponentClass; out Reference)`|Cria o builder atribuindo a instância criada à variável `Reference`, sem definir nome.

Método
| Método | Tipo | Descrição |
|--------|----------|------------|
`CreateObject: TComponent`|`TComponent`|Sobrescreve o método base para instanciar o componente informado em `AClass`.

---
#### `IControlBuilder`

A interface `IControlBuilder<TBuild>` estende `IComponentBuilder<TBuild>`
, acrescentando suporte às propriedades e eventos típicos de controles visuais (`TControl` e descendentes).

Ela define os métodos de acesso (`get/set`) e propriedades opcionais que permitem configurar dinamicamente aspectos visuais, como posição, dimensões, alinhamento, texto e eventos, sem necessidade de interação direta com o formulário ou arquivos .dfm/.lfm.

O uso de tipos opcionais (`TOptionalString`, `TOptionalSingle`, `TOptionalAlign`) permite distinguir entre valores definidos explicitamente e valores padrão herdados do controle, tornando o builder mais seguro e previsível em cenários de inicialização parcial.

**Principais propriedades**
| Propriedade | Tipo | Descrição |
|--------|----------|------------|
`Caption`	|`TOptionalString`|	Define o texto de exibição do controle (quando aplicável).
`Text`	|`TOptionalString`|	Define o conteúdo textual, usado em controles de entrada como `TEdit` ou `TMemo`.
`Align`	|`TOptionalAlign`|	Define o alinhamento do controle dentro do container (`alTop`, `alLeft`, etc.).
`Width`	|`TOptionalSingle`|	Largura do controle em pixels.
`Height`	|`TOptionalSingle`|	Altura do controle em pixels.
`Top`	|`TOptionalSingle`|	Posição vertical relativa ao container.
`Left`	|`TOptionalSingle`|	Posição horizontal relativa ao container.
`Parent`	|`TWinControl`|	Container pai onde o controle será inserido.
`OnClick`	|`TNotifyEvent`|	Define o evento de clique do controle.

**Métodos definidos**
| Propriedade | Descrição |
|--------|---------------|
`Get/SetAlign`	|Obtém ou define o alinhamento do controle.
`Get/SetCaption`	|Obtém ou define o texto de exibição.
`Get/SetText`	|Obtém ou define o conteúdo textual do controle.
`Get/SetHeight`	|Obtém ou define a altura.
`Get/SetWidth`	|Obtém ou define a largura.
`Get/SetTop`	|Obtém ou define a posição vertical.
`Get/SetLeft`	|Obtém ou define a posição horizontal.
`Get/SetParent`	|Define o container pai (`TWinControl`).
`Get/SetOnClick`	|Define o manipulador do evento `OnClick`.

---

#### `TControlBuilderBase`

A classe `TControlBuilderBase<TBuild, TSelf>` é uma classe abstrata que implementa `IControlBuilder<TBuild>` e estende `TComponentBuilderBase<TBuild, TSelf>`.

Ela fornece uma implementação fluente e genérica para a construção e configuração de controles visuais (`TControl` e descendentes), tanto no Delphi quanto no Lazarus.

Por meio de métodos encadeáveis (`With`), é possível definir propriedades como posição, tamanho, alinhamento, texto, e eventos sem depender de formulários ou arquivos .dfm/.lfm.
Essa classe mantém a fluência de chamadas herdada da hierarquia base — todos os métodos retornam TSelf, permitindo composições como:
```pascal
TControlBuilder.Create(TButton)
  .WithName('BtnSave')
  .WithCaption('Salvar')
  .WithAlign(alBottom)
  .WithOnClick(@OnSaveClick)
  .Build;
```  

**Principais propriedades**
| Propriedade | Tipo |Descrição |
|--------|--------|---------------|
`Parent`	|`TWinControl`|	Define o container onde o controle será inserido.
`Caption`	|`TOptionalString`|	Texto de exibição do controle (quando aplicável).
`Text`	|`TOptionalString`|	Conteúdo textual (ex.: `TEdit.Text`).
`Align`	|`TOptionalAlign`|	Alinhamento do controle no container (`alTop`, `alClient`, etc.).
`Width`	|`TOptionalSingle`|	Largura do controle em pixels.
`Height`	|`TOptionalSingle`|	Altura do controle em pixels.
`Top`	|`TOptionalSingle`|	Posição vertical relativa ao container.
`Left`	|`TOptionalSingle`|	Posição horizontal relativa ao container.
`OnClick`	|`TNotifyEvent`|	Evento disparado quando o controle é clicado.

**Métodos de configuração (fluentes)**
| Método | Descrição |
|--------|---------------|
`WithAlign(AAlign)`	|Define o alinhamento (`TAlign` ou `TAlignLayout`, conforme o framework).
`WithName(AName)`	|Define o nome (`Name`) do controle.
`WithTag(ATag)`	|Define o identificador numérico (`Tag`).
`WithWidth(AWidth)`	|Define a largura.
`WithHeight(AHeight)`	|Define a altura.
`WithWidthAndHeight(AWidth, AHeight)`	|Define largura e altura simultaneamente.
`WithTop(ATop)`	|Define a posição vertical.
`WithLeft(ALeft)`	|Define a posição horizontal.
`WithCaption(ACaption)`	|Define o texto de exibição (`Caption`).
`WithText(AText)`	|Define o conteúdo textual (`Text`).
`WithOnClick(AOnClick)`	|Define o evento de clique (`OnClick`).

**Métodos herdados e sobrescritos**
| Método | Descrição |
|--------|---------------|
`ConfigureObject(AObject: TBuild)`	|Sobrescreve o método base para aplicar as propriedades visuais configuradas.
`Create`	|Construtor padrão, inicializa a instância definindo `TControl` como Classe a ser instanciada.
`Get/Set*`	|Implementações dos acessores da interface `IControlBuilder<TBuild>`.

**Observações**

* Mantém fluência total com os métodos da classe base (TComponentBuilderBase e TObjectBuilderBase), graças ao uso do tipo genérico TSelf.
* É compatível com Delphi (VCL/FM) e Lazarus (LCL), adaptando o tipo Align conforme o framework ativo (`TAlign` ou `TAlignLayout`).
* Permite que componentes sejam criados inteiramente via código, reduzindo a dependência de formulários visuais.
* É a base para builders concretos como TControlBuilder e outros builders especializados (`TButtonBuilder`, `TPanelBuilder`, etc.).

---
#### `TControlBuilder`

A classe `TControlBuilder` é a implementação concreta de `TControlBuilderBase<TBuild, TSelf>`, especializada para o tipo TControl.

Ela serve como um builder universal de controles visuais, permitindo instanciar dinamicamente qualquer classe descendente de TControl (como `TButton`, `TPanel`, `TEdit`, etc.) com configuração fluente.

Estrutura 

```pascal
TControlBuilder = class(TControlBuilderBase<TControl, TControlBuilder>)
protected
  function CreateObject: TControl; override;
public
  constructor Create; overload;
  constructor Create(AClass: TControlClass; const AName: string=''); overload;
  constructor Create(AClass: TControlClass; const AName: string; out Reference); overload;
  constructor Create(AClass: TControlClass; out Reference); overload;
end;
```


**Descrição**

A classe implementa o método `CreateObject`, responsável por instanciar o controle informado no construtor. É ideal para cenários em que se deseja criar e configurar controles sem precisar declarar uma classe de builder específica, mantendo a flexibilidade e o estilo fluente da biblioteca OPCB.

Principais Construtores
| Método | Descrição |
|--------|---------------|
`Create`	|Cria um builder genérico, definindo a classe de instanciação como `TControl`.
`Create(AClass: TControlClass; const AName: string='')`	|Cria um builder para a classe especificada (`TButton`, `TPanel`, etc.), opcionalmente atribuindo um nome.
`Create(AClass: TControlClass; const AName: string; out Reference)`	|Igual ao anterior, mas retorna a referência do controle criado.
`Create(AClass: TControlClass; out Reference)`	|Variante sem nome inicial, mas com retorno por referência.

**Exemplo de uso**
```pascal
uses
  OPCB.ControlBuilder;

var
  Btn: TButton;
begin
  TControlBuilder.Create(TButton, 'BtnOk', Btn)
    .WithCaption('OK')
    .WithAlign(alRight)
    .WithWidthAndHeight(100, 30)
    .WithOnClick(@OnOkClick)
    .Build;
end;
```

**Observações**

* Mantém fluência completa com os métodos herdados de TControlBuilderBase
 e classes anteriores na hierarquia.
* Evita a necessidade de classes específicas quando não há comportamento adicional a sobrescrever.
* Ideal para criação dinâmica de UI em frameworks compatíveis (VCL, LCL e FMX).

---
#### `IMenuBuilder`

A interface `IMenuBuilder\<TBuild\>` estende `IComponentBuilder\<TBuild\>`
, fornecendo a estrutura necessária para builders voltados a componentes do tipo `TMenu` (como `TMainMenu`, `TPopupMenu` e derivados).

Atualmente, não adiciona novos membros além dos herdados, mas serve como ponto de extensão para futuras especializações.

**Observação**

Mesmo sem declarar métodos adicionais, o uso de uma interface separada traz benefícios de tipagem genérica e semântica, permitindo que outros builders e creators possam reconhecer menus de forma específica (por exemplo, `TMenuItemBuilder`).

---

#### `TMenuBuilderBase`

A classe abstrata `TMenuBuilderBase\<TBuild, TSelf\>` é a implementação base da interface `IMenuBuilder\<TBuild\>`.
Ela herda toda a infraestrutura de TComponentBuilderBase
, mantendo o estilo fluente e os mecanismos de configuração e construção já padronizados na hierarquia da biblioteca.

---

#### `TMenuBuilder` 

A classe TMenuBuilder é a implementação concreta de TMenuBuilderBase<TBuild, TSelf>, especializada para o tipo TMenu.


| Construtor                             | Uso                                                                |
| -------------------------------------- | ------------------------------------------------------------------ |
| `Create`                               | Cria o builder com o tipo padrão `TMenu`.                          |
| `Create(AClass: TMenuClass)`           | Cria um menu de uma classe específica (por exemplo, `TPopupMenu`). |
| `Create(AClass, AName)`                | Define o nome do componente ao criá-lo.                            |
| `Create(AClass, AName, out Reference)` | Cria e já fornece uma referência para uso externo.                 |
| `Create(AClass, out Reference)`        | Cria com referência, sem nome.                                     |

---

#### `IMenuItemBuilder`


Essa interface define o contrato para a construção de itens de menu, como `TMenuItem`.

| Propriedade  | Tipo               | Função                                                          |
| ------------ | ------------------ | --------------------------------------------------------------- |
| `Caption`    | `TOptionalString`  | Define o texto exibido no item de menu.                         |
| `ImageIndex` | `TOptionalInteger` | Define o índice da imagem associada, se houver um `TImageList`. |
| `OnClick`    | `TNotifyEvent`     | Define o evento de clique do item.                              |

---

#### `TMenuItemBuilderBase`

`TMenuItemBuilderBase` fornece uma base genérica para construção fluente de itens de menu (`TMenuItem`), herdando toda a infraestrutura de criação e configuração de componentes oferecida por `TComponentBuilderBase`.

| Campo         | Tipo               | Descrição                                   |
| ------------- | ------------------ | ------------------------------------------- |
| `FCaption`    | `TOptionalString`  | Armazena o texto do item de menu.           |
| `FImageIndex` | `TOptionalInteger` | Define o índice da imagem associada.        |
| `FOnClick`    | `TNotifyEvent`     | Armazena o manipulador do evento de clique. |

**Métodos**

| Método                        | Descrição                                 |
| ----------------------------- | ----------------------------------------- |
| `WithCaption(ACaption)`       | Define o texto a ser exibido.             |
| `WithImageIndex(AImageIndex)` | Define o índice da imagem associada.      |
| `WithOnClick(AOnClick)`       | Define o manipulador de evento `OnClick`. |

---

#### `TMenuItemBuilder`

`TMenuItemBuilder` é a implementação concreta e pronta para uso do builder para objetos `TMenuItem` e descendentes.
Ela herda toda a lógica de configuração da classe base `TMenuItemBuilderBase`, oferecendo um ponto de entrada simples e direto para a criação fluente de itens de menu no Delphi ou Lazarus.

A classe segue a mesma hierarquia dos outros builders da biblioteca:
```
TObjectBuilderBase
  └── TComponentBuilderBase
        └── TMenuItemBuilderBase
              └── TMenuItemBuilder ← implementação concreta
```

**Construtores**

| Construtor                             | Uso típico                                                          |
| -------------------------------------- | ------------------------------------------------------------------- |
| `Create`                               | Criação padrão com `TMenuItem` como classe base.                    |
| `Create(AClass: TMenuItemClass)`       | Permite construir descendentes de `TMenuItem` (ex.: `TMenuItemEx`). |
| `Create(AClass, AName)`                | Define nome de design-time ao instanciar.                           |
| `Create(AClass, AName, out Reference)` | Cria e retorna a referência do objeto instanciado.                  |
| `Create(AClass, out Reference)`        | Cria e retorna referência sem nome pré-definido.                    |

---









#### TComponentCreator

`TComponentCreator` permite centralizar a criação de múltiplos componentes e manter um registro interno das instâncias criadas, possibilitando recuperá-las posteriormente.

---

**Campos privados**

| Campo | Tipo | Descrição |
|-------|------|------------|
| `FOwner` | `TComponent` | Define o *Owner* dos componentes adicionados por meio do método `Add`. |
| `FRegistryContextHandle` | `IRegistryContextHandle` | Indica o *ContextHandle* utilizado para registrar os componentes criados. Permite compartilhar o registro com outros *Creators*, como `TControlCreator`, `TMenuCreator`, etc. |

---

**Métodos privados**

| Método | Retorno | Descrição |
|---------|----------|-----------|
| `GetComponentRegistry` | `TComponentRegistry` | Retorna o registro de componentes associado ao `FRegistryContextHandle`. |
| `GetComponents` | `TComponentList` | Retorna a lista de componentes vinculada ao registro a partir do `FRegistryContextHandle`. |
| `GetItem(const AName: string)` | `TComponent` | Retorna um componente da lista pelo nome, desde que ele tenha sido nomeado. |

---

**Construtores**

| Construtor | Uso típico |
|-------------|-------------|
| `Create(ARegistryContextKey: string = '')` | Instancia um novo `TComponentCreator`. Se `ARegistryContextKey` for informado, utilizará o registro existente com o mesmo nome; caso contrário, criará um novo. Se a chave não for informada, uma chave exclusiva aleatória será gerada, resultando em um novo registro. |
| `Create(ARegistryContextHandle: IRegistryContextHandle)` | Instancia um `TComponentCreator` reutilizando um registro de componentes existente, obtido via `ARegistryContextHandle`. |

---

**Métodos públicos**

| Método | Retorno | Descrição |
|---------|----------|-----------|
| `External(const AProc: TComponentCreatorObjProc)` | `TComponentCreator` | Permite a injeção de código externo por meio de uma *procedure* do tipo `TComponentCreatorObjProc`. |
| `External(const AProc: TComponentCreatorProc)` | `TComponentCreator` | Permite a injeção de código externo por meio de uma *procedure* do tipo `TComponentCreatorProc`. |
| `GetComponent<T: TComponent>(const AName: string): T; overload;` | `T` | Método genérico que tenta retornar um componente pelo seu nome. |
| `GetComponent(const AName: string): TComponent;` | `TComponent` | Retorna um componente pelo nome informado. |
| `SetOwner(AOwner: TComponent)` | `TComponentCreator` | Método fluente que define o *Owner* a ser utilizado nos componentes adicionados via `.Add`. |
| `Add(AComponentBuilder: IComponentBuilder<TComponent>)` | `TComponentCreator` | Método fluente que adiciona um novo componente a partir de um `TComponentBuilder`. |

---

**Propriedades**

| Propriedade | Tipo | Função |
|--------------|------|---------|
| `Registry` | `TComponentRegistry` | Retorna o `TComponentRegistry` utilizado para armazenar os componentes criados. |
| `Items[const AName: string]` | `TComponent` | Retorna um componente do registro a partir do seu nome. |

---

**Exemplo**
```pascal
var
  Creator: TComponentCreator;
  CDS: TClientDataSet;
  DS: TDataSource;
begin
  Creator := TComponentCreator.Create;

  try
    Creator
      .SetOwner(Self)
      .Add(TComponentBuilder.Create(TClientDataSet, CDS))
      .Add(TComponentBuilder.Create(TDataSource, DS));

    DS.DataSet := CDS;
  finally
    Creator.Free;
  end;
end;
```
---

#### TControlCreator

TControlCreator permite centralizar a criação de múltiplos Controles e manter um registro interno das instâncias criadas, possibilitando recuperá-las posteriormente.

TControlCreator foi desenhada para facilitar a criação de constroles visuais e posicioná-los na tela sem a necessidade de arrastar componentes na tela através da IDE em tempo de design.

**Campos privados**

| Campo | Tipo | Descrição |
|-------|------|------------|
| `FOwner` | `TComponent` | Define o *Owner* dos controles adicionados por meio do método `.Add`. |
| `FRegistryContextHandle` | `IRegistryContextHandle` | Indica o *ContextHandle* utilizado para registrar os componentes criados. Permite compartilhar o registro com outros *Creators*, como `TControlCreator`, `TMenuCreator`, etc. |
|`FGroups`|`TControlGroupMap`|`TControlGroupMap = TDictionary<string, TControlList>;`. <br> Permite adicionar os controles em grupos específicos para posterior recuperação de controle com base nos grupos pertencentes. Também são utilizados para determinar os limites (*Bounds*) ocupados por um grupo de controles. 
|`FLevelStack`|`TControlCreatorLevelStack`|`TControlCreatorLevelStack = TObjectList<TControlCreatorLevel>;` <br>Pilha que armazena informações dos níveis (*levels*). Chamadas aos métodos `.SubLevel` e `.SuperLevel` adicionam e removem, respectivamente,  *levels* na pilha. O *level* no topo é o *level* corrente. <br><br>Um level é um objeto da classe `TControlCreatorLevel` que contém as informações de `Direction`, `CurrentTop`, `CurrentLeft`, `VerticalSpace`, `HorizontalSpace`, `MaxControlHeight`, `MaxControlWidht`, entre outras propriedades utilizadas para controlar o posicionamento e tamanho dos controles conforme são adicionados através dos métodos `.Add`.|

**Métodos Privados**
| Método | Retorno | Descrição |
|---------|----------|-----------|
|`GetControls`|`TControlList`|Retorna lista de controles adicionados via `.Add`|
|`MoveTopLeftAfterControl (AControl: TControl)`||Move `CurrentTop` e `CurrentLeft` do `level` corrente após o `AControl`. Se `Direction` for `cpdVertical` `Top` e `Left` ficarão a direita do controle, se for `cpdHorizontal` ficarão abaixo do controle. <br>O cálculo do posicionamento leva em consideração também as propriedades `VerticalSpace` e `HorizontalSpace` do *level* corrente e também a propriedade `Align` de `AControl`. |
|`MoveTopLeftAfterRect(const ARect: TRect;AAlign: TAlign);`||Mesmo do anterior, mas considerando `ARect`. <br>`AAlign` = `alTop` aplica a mudança apenas para `Top`. <br> `AAlign` = `alLeft` aplica a mudança apenas para `Left`. <br> `AAlign` = `alNone` aplica a mudança apenas para `Top` e `Left`. <br> No `FMX` os tipos de `AAlign` são tratados conforme o *framework*. |
|`MoveTopLeftAfterBound(ABounds: TControlGroupBounds)`||Mesmo do Anterior, mas considerando objeto do tipo `TControlGroupBounds`.|
|`AddToGroups(AControl: TControl; const AGroups: array of string)`||Adiciona o controle a um ou mais grupos definidos em `AGroups`.|
|`GetGroupBounds(const AGroupName: string)`|`TControlGroupBounds`|Retorna um `record` do tipo `TControlGroupBounds` com informações de dimensões de um grupo de controles.|
|`GetCurrenteLevel`|`TControlCreatorLevel`|Retorna o nível (*level*) atual|
|`GetContentWidth`|`Single`|Retorna a largura ocupada por todos os controles adicionados. Utiliza tipo `Single` para melhor compatiblidade com o *framework Firemonkey*.|
|`GetFContentHeight`|`Single`|Retorna a altura ocupada por todos os controles adicionados. Utiliza tipo `Single` para melhor compatiblidade com o *framework Firemonkey*.|
|`GetComponentRegistry`|`TComponentRegistry`|Retorna o objeto de registro dos componentes adicionados. Esse objeto é definido na chamada do construtor pelo parâmetro `ARegistryContextKey` ou pelo parâmetro `ARegistryContextHandle`. Diferentes *Creators* podem compartilhar um mesmo Registro de seus componentes.|


---
**DOCUMENTAÇÃO EM CONSTRUÇÃO**