# Como Usar o `TControlBuilder`

`TControlBuilder` é uma implementação do padrão *Builder* voltada à instanciação de objetos que herdam de `TControl`.

Hierarquia:  
```
TObjectBuilderBase<TBuild, TSelf> : IObjectBuilder<TBuild>
  └── TComponentBuilderBase<TBuild, TSelf> : IComponentBuilder<TBuild>
        └── TControlBuilderBase<TBuild, TSelf> : IControlBuilder<TBuild>
              └── TControlBuilder <-- classe concreta

```

Com o `TControlBuilder`, é possível instanciar qualquer componente descendente de `TControl`, como `TButton`, `TPanel` ou `TForm`.

***Exemplo***
```pascal
uses
  OPCB;

var
  Builder: TControlBuilder;
  Button: TButton;
begin
  Builder := TControlBuilder.Create(TButton); //TButton indica a classe do objeto a ser criado.

  try
    Builder
      .WithOwnerAndParent(Self, Self)
      .WithTop(10)
      .WithLeft(10)
      .WithWidth(150)
      .WithHeight(50)
      .WithName('Button1')
      .WithOnClick(ButtonClick)  //ButtonClick deve ser um método com a implementação do clique
      .WithCaption('Clique aqui')
    ;

    Button := Builder.Build as TButton;

  finally
    Builder.Free;
  end;
end;
```

---

### 💡 Conceito

O *Builder* permite configurar um componente **antes de sua criação efetiva**, por meio de métodos como `WithTop`, `WithLeft`, `WithName` e outros que acessam propriedades definidas em `TControl` ou em suas classes ancestrais.

No entanto, esses métodos não abrangem propriedades **específicas da classe concreta** sendo instanciada.  
Por exemplo, ao criar um `TButton`, a propriedade `ModalResult` não possui um método dedicado (`WithModalResult`).  

Para lidar com esse tipo de situação — ou seja, quando precisamos configurar propriedades que não possuem um método fluente específico — o `TControlBuilder` oferece alternativas flexíveis, descritas a seguir.

---

### ⚙️ Método `Setup`

O método `Setup` permite associar um ou mais procedimentos de configuração ao componente que será criado.  
Esses procedimentos são executados automaticamente durante a chamada ao método `.Build`.

Exemplo:
```pascal
uses
  OPCB;

var
  Builder: TControlBuilder;
  Button: TButton;
begin
  Builder := TControlBuilder.Create(TButton); //TButton indica a classe do objeto a ser criado.

  try
    Builder
      .WithOwnerAndParent(Self, Self)
      .WithTop(10)
      .WithLeft(10)
      .WithWidth(150)
      .WithHeight(50)
      .WithName('Button1')
      .WithOnClick(ButtonClick)  //ButtonClick deve ser um método com a implementação do clique
      .WithCaption('Clique aqui')
      .Setup(procedure (AControl: TControl)
             var
               Button: TButton;
             begin
               Button := TButton(AControl);
               Button.Font.Color := clBlue;
               Button.Font.Size := 22;
             end)
    ;

    Button := Builder.Build as TButton;

  finally
    Builder.Free;
  end;
end;
```

> A *procedure* passada como parâmetro em `Setup` será executada no momento da construção do objeto, permitindo aplicar configurações personalizadas após todas as definições fluentes.

---

### 🧩 Métodos `WithProp` e `WithEvent`

Os métodos `WithProp` e `WithEvent` permitem definir propriedades e eventos de um objeto por meio de **RTTI** (*Runtime Type Information*).

Eles oferecem uma forma dinâmica de configurar propriedades que não são suportadas diretamente pelos métodos `With` padrão do *Builder*.

No entanto, é importante ter atenção:  
erros relacionados a nomes de propriedades inexistentes, tipos incompatíveis ou valores inválidos **serão detectados apenas em tempo de execução**.

Exemplo:
```pascal
uses
  OPCB;

var
  Builder: TControlBuilder;
  Button: TButton;
begin
  Builder := TControlBuilder.Create(TButton); 

  try
    Builder
      .WithOwnerAndParent(Self, Self)
      .WithTop(10)
      .WithLeft(10)
      .WithWidth(150)
      .WithHeight(50)
      .WithName('Button1')
      .WithOnClick(ButtonClick)  
      .WithCaption('Clique aqui')
      .WithProp('Font.size', 22)   //define a propriedade Font.size através de RTTI
    ;

    Button := Builder.Build as TButton;

  finally
    Builder.Free;
  end;
end;
```

---

### ✅ Resumo

| Cenário | Método recomendado |
|----------|--------------------|
| Propriedades comuns de `TControl` ou ancestrais | `With...` |
| Propriedades específicas da classe concreta | `Setup` ou `WithProp` |
| Atribuição de eventos via código | `WithEvent` |

