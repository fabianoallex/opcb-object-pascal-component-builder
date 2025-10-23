# OPCB – Object Pascal Component Builder  

Documentantion: [docs/doc-pt.md](docs/doc-pt.md)

🚀 Instancie e configure componentes Delphi e Lazarus de forma fluente, expressiva e reutilizável.  

O **OPCB (Object Pascal Component Builder)** é uma biblioteca que facilita a construção de componente em tempo de execução em **Delphi** e **Lazarus**, permitindo criar e configurar componentes visuais com uma API fluente, clara e organizada.  

---

## ✨ Benefícios  

- 🔹 **API fluente** – Configure propriedades e eventos de forma encadeada.  
- 🔹 **Menos código repetitivo**.
- 🔹 **Organização em níveis hierárquicos** – Facilita a criação de layouts aninhados.  
- 🔹 **Suporte a grids automáticos** – Adicione controles em células com `RowSpan` e `ColSpan`.  
- 🔹 **Compatível com VCL, LCL e FMX** . 

---

## 🚀 Exemplos de uso  

Criação simples de alguns controles:  

```pascal
uses
  OPCB;

procedure TForm1.FormCreate(Sender: TObject);
var
  Creator: TControlCreator;
begin
  Creator := TControlCreator.Create;
  try
    Creator
      .WithOwnerAndParent(Self, Self)
      .SetSpace(5, 5)
      .SetTopLeft(10, 10)
      .SetDirection(cpdVertical)
      .Add(TControlBuilder.Create(TLabel).WithCaption('Nome'))
      .Add(TControlBuilder.Create(TEdit, 'edit_name').WithWidth(250).WithCaption(''))
      .IncTop(15)
      .Add(TControlBuilder.Create(TLabel).WithCaption('Idade'))
      .Add(TControlBuilder.Create(TNumberBox).WithWidth(50))
      .IncTop(20)
      .SetDirection(cpdHorizontal)
      .Add(TControlBuilder.Create(TButton, 'button_enviar').WithCaption('Enviar'))
      .Add(TControlBuilder.Create(TButton, 'button_cancelar').WithCaption('Cancelar'))
      .AlignControlsRight(['button_enviar', 'button_cancelar'], ['edit_name'])
    ;
  finally
    Creator.Free;;
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
  Creator: TControlCreator;
begin
  Creator := TControlCreator.Create;
  try
    Creator
      .WithOwnerAndParent(Self, Self)
      .SetTopLeft(10, 10)
      .SetSpace(5, 5)
      .SubLevel(TControlBuilder.Create(TGroupBox).WithCaption('Opções').WithWidthAndHeight(250, 250))
        .SetDirection(cpdVertical)
        .SetTopLeft(20, 20)
        .Add(TControlBuilder.Create(TCheckBox).WithCaption('Opção 1'))
        .Add(TControlBuilder.Create(TCheckBox).WithCaption('Opção 2'))
        .Add(TControlBuilder.Create(TCheckBox).WithCaption('Opção 3'))
        .Add(TControlBuilder.Create(TCheckBox).WithCaption('Opção 4'))
        .Add(TControlBuilder.Create(TCheckBox).WithCaption('Opção 5'))
        .Add(TControlBuilder.Create(TCheckBox).WithCaption('Opção 6'))
      .SuperLevel
      .SubLevel(TControlBuilder.Create(TPanel).WithWidthAndHeight(250, 250))
        .SetDirection(cpdVertical)
        .SetTopLeft(20, 20)
        .Add(TControlBuilder.Create(TLabel).WithCaption('Informação 1'))
        .Add(TControlBuilder.Create(TLabel).WithCaption('Informação 2'))
        .Add(TControlBuilder.Create(TLabel).WithCaption('Informação 3'))
        .Add(TControlBuilder.Create(TLabel).WithCaption('Informação 4'))
        .Add(TControlBuilder.Create(TLabel).WithCaption('Informação 5'))
        .Add(TControlBuilder.Create(TLabel).WithCaption('Informação 6'))
        .IncTop(50)
        .Add(TControlBuilder.Create(TButton).WithCaption('Copiar'))
      .SuperLevel
    ;
  finally
    Creator.Free;
  end;
end;  

```
![Exemplo 02](docs/img/img-02.png)

---

Modo Grid:  

```pascal
uses
  OPCB;

procedure TForm1.FormCreate(Sender: TObject);
var
  Creator: TControlCreator;
begin
  Creator := TControlCreator.Create;
  try
    Creator
      .WithOwnerAndParent(Self, Self)
      .SetTopLeft(20, 20)
      .SetSpace(5, 5)
      .SetDirection(cpdVertical)
      .GridInit(4, 6)
        .GridSetCellWidthAndHeight(70, 70)
        .GridSetColWidth(0, 200)
        .GridSetRowHeight(1, 150)
        .Add(TControlBuilder.Create(TPanel).WithCaption('1'))
        .Add(TControlBuilder.Create(TPanel).WithCaption('2'))
        .SubLevel(TControlBuilder.Create(TPanel).WithCaption('3'))
          .Add(TControlBuilder.Create(TButton).WithCaption('B1'))
          .Add(TControlBuilder.Create(TButton).WithCaption('B2'))
        .SuperLevel
        .Add(TControlBuilder.Create(TPanel).WithCaption('4'))
        .Add(TControlBuilder.Create(TPanel).WithCaption('5'))
        .SetDirection(cpdHorizontal)
        .GridColSpan(2)
        .GridRowSpan(2)
        .Add(TControlBuilder.Create(TPanel).WithCaption('6'))
        .Add(TControlBuilder.Create(TPanel).WithCaption('7'))
        .Add(TControlBuilder.Create(TPanel).WithCaption('8'))
        .Add(TControlBuilder.Create(TPanel).WithCaption('9'))
        .Add(TControlBuilder.Create(TPanel).WithCaption('10'))
        .Add(TControlBuilder.Create(TPanel).WithCaption('11'))
        .GridSkipCell
        .Add(TControlBuilder.Create(TPanel).WithCaption('12'))
        .Add(TControlBuilder.Create(TPanel).WithCaption('13'))
        .GridRowSpan(2)
        .Add(TControlBuilder.Create(TPanel).WithCaption('14'))
        .Add(TControlBuilder.Create(TPanel).WithCaption('15'))
        .Add(TControlBuilder.Create(TPanel).WithCaption('16'))
        .Add(TControlBuilder.Create(TPanel).WithCaption('17'))
        .Add(TControlBuilder.Create(TPanel).WithCaption('18'))
        .Add(TControlBuilder.Create(TPanel).WithCaption('19'))
      .GridFinish
    ;
  finally
    Creator.Free;
  end;
end;   
```
![Exemplo 03](docs/img/img-03.png)


## 🚀 Exemplo: Construindo um Teclado Virtual

O exemplo abaixo demonstra como montar um **teclado virtual** completo usando a biblioteca.

```pascal
procedure TForm1.FormCreate(Sender: TObject);
var
  Creator: TControlCreator;
begin
  Creator := TControlCreator.Create;
  try
    Creator
      .WithOwnerAndParent(Self, Self)
      .SetSpace(2, 2)
      .SetTopLeft(20, 20)
      .SubLevel(TControlBuilder.Create(TPanel))
        .SetTopLeft(10, 10)
        .GridInit(4, 10)
          .GridSetCellWidthAndHeight(80, 80)
          .GridSetRowOffset(1, 22)
          .GridSetRowOffset(2, 65)
          .External(procedure (ACreator: TControlCreator)
            const KeyRows: array[0..1] of string = ('QWERTYUIOPASDFGHJKL', 'ZXCVBNM');
            var Key: Char;
            begin
              for Key in KeyRows[0] do
                ACreator.Add(TControlBuilder.Create(TSpeedButton).WithCaption(Key));
              ACreator.Break;
              for Key in KeyRows[1] do
                ACreator.Add(TControlBuilder.Create(TSpeedButton).WithCaption(Key));
              ACreator.Break;
              ACreator.GridSkipCells(2);
              ACreator.GridColSpan(5);
              ACreator.Add(TControlBuilder.Create(TSpeedButton).WithCaption('[ SPACE ]'));
            end
          )
        .GridFinish
        .IncLeft(20)
        .GridInit(4, 3)
          .GridSetCellWidthAndHeight(80, 80)
          .External(procedure(ACreator: TControlCreator)
            const Keys = '789456123';
            var Key: Char;
            begin
              for Key in Keys do
                ACreator.Add(TControlBuilder.Create(TSpeedButton).WithCaption(Key));
              ACreator.GridColSpan(2);
              ACreator.Add(TControlBuilder.Create(TSpeedButton).WithCaption('0'));
              ACreator.Add(TControlBuilder.Create(TSpeedButton).WithCaption(','));
            end
          )
        .GridFinish
        .RecalcParentSize(10, 10)
      .SuperLevel
    ;
  finally
    Creator.Free;
  end;
end;
```
![Exemplo 04](docs/img/img-04.png)

---

## 🔑 Funcionalidades Demonstradas

- **Criação fluente de controles**
  - `.WithOwnerAndParent(Self, Self)` define o owner e parent.
  - `.SetSpace(2, 2)` / `.SetTopLeft(20, 20)` controlam espaçamento e posição inicial.

- **Hierarquia de níveis**
  - `.SubLevel(...)` abre um novo escopo de construção (ex.: um `TPanel`).
  - `.SuperLevel` retorna ao nível anterior.

- **Layouts em grid**
  - `.GridInit(Cols, Rows)` inicia um grid.
  - `.GridSetCellWidthAndHeight(W, H)` define as dimensões das células.
  - `.GridSetRowOffset(Row, Offset)` aplica deslocamento em linhas específicas.

- **Inserção dinâmica de controles**
  - `.External(procedure(ACreator: TControlCreator) ...)`  
    permite criar controles em lote, como teclas, a partir de arrays/strings.

- **Controle de posicionamento no grid**
  - `.Break` → quebra de linha/coluna.  
  - `.GridSkipCells(N)` → pula células.  
  - `.GridColSpan(N)` → mescla colunas (ex.: tecla **SPACE**).

- **Ajuste automático de container**
  - `.RecalcParentSize(PaddingX, PaddingY)` redimensiona automaticamente o container pai.

---

## 🎹 Resultado

O exemplo cria:

- Um **teclado QWERTY** com duas linhas de letras e a tecla **[ SPACE ]** expandida.  
- Um **teclado numérico** com dígitos `0–9` e vírgula.  
- Ambos organizados dentro de um painel (`TPanel`) que se ajusta automaticamente ao conteúdo.

## Instalação

### 🔹 Opção 1 – Clonar o repositório

```bash
git clone https://github.com/fabianoallex/opcb-object-pascal-component-builder.git
```

### 🔹 Opção 2 – Download manual

Baixe o arquivo ZIP do repositório no GitHub e extraia o conteúdo em uma pasta de sua preferência.

---

### 💠 Delphi

1. Abra o **Delphi**.  
2. Vá em **Tools ▸ Options ▸ Language ▸ Delphi Options ▸ Library**.  
3. No campo **Library Path**, adicione o caminho completo da pasta `src`.  
   Exemplo:  
   `C:\opcb\src`

---

### 💠 Lazarus

1. Abra o **Lazarus**.  
2. Vá em **Project ▸ Project Options ▸ Compiler Options ▸ Paths**.  
3. Em **Other unit files (-Fu)**, adicione o caminho da pasta `src`.  
   Exemplo:  
   `C:\opcb\src`

---

### 🧰 Pronto!

Após isso, as units do projeto estarão disponíveis para uso em seus projetos Delphi ou Lazarus.

---
