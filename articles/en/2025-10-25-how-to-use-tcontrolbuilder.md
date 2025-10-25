# How to Use `TControlBuilder`

`TControlBuilder` is an implementation of the *Builder* pattern designed for instantiating objects that inherit from `TControl`.

Hierarchy:  
```
TObjectBuilderBase<TBuild, TSelf> : IObjectBuilder<TBuild>
  └── TComponentBuilderBase<TBuild, TSelf> : IComponentBuilder<TBuild>
        └── TControlBuilderBase<TBuild, TSelf> : IControlBuilder<TBuild>
              └── TControlBuilder <-- classe concreta

```

With `TControlBuilder`, you can instantiate any component that descends from `TControl`, such as `TButton`, `TPanel`, or `TForm`.

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
      .WithCaption('Click here')
    ;

    Button := Builder.Build as TButton;

  finally
    Builder.Free;
  end;
end;
```

---

### 💡 Concept

The *Builder* allows you to configure a component **before it is actually created**, through methods like `WithTop`, `WithLeft`, `WithName`, and others that access properties defined in `TControl` or its ancestor classes.

However, these methods do not cover properties **specific to the concrete class** being instantiated.  
For example, when creating a `TButton`, the `ModalResult` property does not have a dedicated method (`WithModalResult`).  

To handle this type of scenario — when you need to configure properties that don’t have a specific fluent method — `TControlBuilder` provides flexible alternatives, described below.

---

### ⚙️ The `Setup` Method

The `Setup` method allows you to associate one or more configuration procedures with the component that will be created.  
These procedures are automatically executed during the `.Build` method call.

Example:
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
      .WithCaption('Click here')
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

> The *procedure* passed as a parameter to `Setup` will be executed at object construction time, allowing you to apply custom configurations after all fluent definitions have been processed.

---

### 🧩 The `WithProp` and `WithEvent` Methods

The `WithProp` and `WithEvent` methods allow you to define object properties and events using **RTTI** (*Runtime Type Information*).

They provide a dynamic way to configure properties that are not directly supported by the component’s standard `With` methods.

However, it’s important to note that errors related to non-existent property names, incompatible types, or invalid values **will only be detected at runtime**.

Example:
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
      .WithCaption('Clique here')
      .WithProp('Font.size', 22)   
	  .WithEvent('OnClick', Self, @TForm1.ButtonClick)  
    ;

    Button := Builder.Build as TButton;

  finally
    Builder.Free;
  end;
end;
```

---

### ✅ Summary

| Scenario | Recommended Method |
|-----------|--------------------|
| Common properties from `TControl` or ancestor classes | `With...` |
| Properties specific to the concrete class | `Setup` or `WithProp` |
| Assigning events via code | `WithEvent` |
