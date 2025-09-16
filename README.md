# OPCB – Object Pascal Component Builder  

🚀 Instancie e configure componentes Delphi e Lazarus de forma fluente, expressiva e reutilizável.  

O **OPCB (Object Pascal Component Builder)** é uma biblioteca que facilita a construção de componente em tempo de execução em **Delphi** e **Lazarus**, permitindo criar e configurar componentes visuais com uma API fluente, clara e organizada.  

---

## ✨ Benefícios  

- 🔹 **API fluente** – Configure propriedades e eventos de forma encadeada.  
- 🔹 **Menos código repetitivo**.
- 🔹 **Organização em níveis hierárquicos** – Facilita a criação de layouts aninhados.  
- 🔹 **Suporte a grids automáticos** – Adicione controles em células com `RowSpan` e `ColSpan`.  
- 🔹 **Compatível com VCL, LCL e FMX** .  
- 🔹 **Extensível** – Crie seus próprios builders especializados.  

---

## 🚀 Exemplos de uso  

Criação simples de alguns controles:  

```pascal
uses
  OPCB;

procedure TForm1.FormCreate(Sender: TObject);
var
  ControlBuilder: TControlBuilder;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(Self, Self)
      .SetSpace(5, 5)
      .SetTopLeft(10, 10)
      .SetDirection(cpdVertical)
      .AddControl(TControlInfo.Create(TLabel).WithCaption('Nome'))
      .AddControl(TControlInfo.Create(TEdit, 'edit_name').WithWidth(250).WithCaption(''))
      .IncTop(15)
      .AddControl(TControlInfo.Create(TLabel).WithCaption('Idade'))
      .AddControl(TControlInfo.Create(TNumberBox).WithWidth(50))
      .IncTop(20)
      .SetDirection(cpdHorizontal)
      .AddControl(TControlInfo.Create(TButton, 'button_enviar').WithCaption('Enviar'))
      .AddControl(TControlInfo.Create(TButton, 'button_cancelar').WithCaption('Cancelar'))
      .AlignControlsRight(['button_enviar', 'button_cancelar'], ['edit_name'])
    ;
  finally
    ControlBuilder.Free;;
  end;
end;
```
![Exemplo 01](docs/img/img-01.png)

---

Criação de controles aninhados:  

```pascal
uses
  OPCB;

procedure TForm1.FormCreate(Sender: TObject);
var
  ControlBuilder: TControlBuilder;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(Self, Self)
      .SetTopLeft(10, 10)
      .SetSpace(5, 5)
      .SubLevel(TControlInfo.Create(TGroupBox).WithCaption('Opções').WithWidthAndHeight(250, 250))
        .SetDirection(cpdVertical)
        .SetTopLeft(20, 20)
        .AddControl(TControlInfo.Create(TCheckBox).WithCaption('Opção 1'))
        .AddControl(TControlInfo.Create(TCheckBox).WithCaption('Opção 2'))
        .AddControl(TControlInfo.Create(TCheckBox).WithCaption('Opção 3'))
        .AddControl(TControlInfo.Create(TCheckBox).WithCaption('Opção 4'))
        .AddControl(TControlInfo.Create(TCheckBox).WithCaption('Opção 5'))
        .AddControl(TControlInfo.Create(TCheckBox).WithCaption('Opção 6'))
      .SuperLevel
      .SubLevel(TControlInfo.Create(TPanel).WithWidthAndHeight(250, 250))
        .SetDirection(cpdVertical)
        .SetTopLeft(20, 20)
        .AddControl(TControlInfo.Create(TLabel).WithCaption('Informação 1'))
        .AddControl(TControlInfo.Create(TLabel).WithCaption('Informação 2'))
        .AddControl(TControlInfo.Create(TLabel).WithCaption('Informação 3'))
        .AddControl(TControlInfo.Create(TLabel).WithCaption('Informação 4'))
        .AddControl(TControlInfo.Create(TLabel).WithCaption('Informação 5'))
        .AddControl(TControlInfo.Create(TLabel).WithCaption('Informação 6'))
        .IncTop(50)
        .AddControl(TControlInfo.Create(TButton).WithCaption('Copiar'))
      .SuperLevel
    ;
  finally
    ControlBuilder.Free;
  end;
end;  

```
![Exemplo 01](docs/img/img-02.png)
