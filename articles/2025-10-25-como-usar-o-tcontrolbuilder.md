# Como Usar o TControlBuilder

TControlBuilder é a implementação de um Builder para instanciação de Objetos que herdem de `TControl`.

Hierarquia:
```
TObjectBuilderBase<TBuild, TSelf> : IObjectBuilder<TBuild>
  └── TComponentBuilderBase<TBuild, TSelf> : IComponentBuilder<TBuild>
        └── TControlBuilderBase<TBuild, TSelf> : IControlBuilder<TBuild>
              └── TControlBuilder <-- classe concreta

```

Com o TControlBuilder é possível instanciar qualquer componente que herde de `TControl`, como `TButton`, `TPanel`, `TForm`.

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
    ;

    Button := Builder.Build as TButton;

  finally
    Builder.Free;
  end;
end;
```

O Builder permite que configuremos um componente antes de criarmos ele efetivamente, através dos métodos, `WithTop`, `WithLeft`, `WithName`, e demais métodos que acessa as propriedades definidas em TControl e nas classes das quais TControl decende, porém não conseguimos usar métodos específicos para propriedades definidas na classe que estamos instanciando, como no caso TButton. Um exemplo é a propriedade ModalResult que encontramos em TButton. Nesse caso nós não teremos um método chamado WithModalResult, que permitisse definir o ModalResult desse botão. Para essa situação existem algumas possibilidades para permitir configurar o componente com todas as propriedades que queremos.

**Método Setup**

Esse método permite vincular um ou mais métodos de configuração do componente a ser instanciado como pode ser visto no exemplo abaixo:

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

A procedure passada como parâmetro em setup, será executada durante a chamada ao método `.Build`.

**WithProp e WithEvent**

WithProp e WithEvent são métodos que permitem definir uma propridade do objeto que estamos criando, através de RTTI.

Esses métodos oferecem possibilidades de configurar diversas propriedades que não são suportadas diretamente pelos métodos With do componente. Porém é preciso estar atento, pois erros de propriedades  ou valores inválidos serão retornados apenas em tempo de execução.

Exemplo

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
      .WithProp('Font.size', 22)
    ;

    Button := Builder.Build as TButton;

  finally
    Builder.Free;
  end;
end;
```