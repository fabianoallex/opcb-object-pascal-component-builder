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

## 🚀 Exemplo de uso  

Criação simples de um painel com botões:  

```pascal
uses
  uOPCB.Builder;

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

