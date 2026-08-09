# OPCB – Object Pascal Component Builder

**English version**: see [README.md](README.md)

---

🚀 Instancie e configure componentes Delphi e Lazarus de forma fluente, expressiva e reutilizável — **inteiramente em tempo de execução, sem precisar de `.dfm`/`.lfm`**.

O **OPCB (Object Pascal Component Builder)** é uma biblioteca para **Delphi** e **Lazarus** que permite criar e configurar componentes visuais em código, com uma API fluente, encadeada e legível. Ela ataca um caso que o designer de formulários visual não cobre bem: telas cuja forma só é conhecida em runtime — dirigida por dado, por um schema, por escolhas do usuário, ou por qualquer outra coisa que o designer não consegue prever com antecedência.

---

## Sumário

- [Documentação e Artigos](#-documentação)
- [Benefícios](#-benefícios)
- [Exemplos de uso](#-exemplos-de-uso)
- [Funcionalidades Demonstradas](#-funcionalidades-demonstradas)
- [Galeria de Exemplos](#-galeria-de-exemplos)
- [Testado](#-testado)
- [Requisitos](#-requisitos)
- [Instalação](#instalação)
- [Licença](#-licença)

---

## 📖 Documentação
[docs/doc-pt.md](docs/doc-pt.md)
---

## 📖 Artigos
[artigos](articles/pt/index.md)
---

## ✨ Benefícios

- 🔹 **API fluente** – Configure propriedades e eventos de forma encadeada.
- 🔹 **Menos código repetitivo**.
- 🔹 **Organização em níveis hierárquicos** – Facilita a criação de layouts aninhados.
- 🔹 **Suporte a grids automáticos** – Adicione controles em células com `RowSpan` e `ColSpan`.
- 🔹 **Três builders, um único estilo de API** – `TControlCreator` para controles visuais, `TComponentCreator` para componentes não-visuais (diálogos, timers, datasets...), `TMenuCreator` para árvores de menu.
- 🔹 **Atribuição de propriedades/eventos via RTTI** – Defina propriedades (`WithProp`) e eventos (`WithEvent`) por nome, mesmo em classes que a biblioteca não conhece de antemão.
- 🔹 **Compatível com VCL, LCL e FMX**.
- 🔹 **Amplamente testada** – veja [Testado](#-testado) abaixo.

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
      .SetOwnerAndParent(Self, Self)
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
    Creator.Free;
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
      .SetOwnerAndParent(Self, Self)
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
      .SetOwnerAndParent(Self, Self)
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
      .SetOwnerAndParent(Self, Self)
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
  - `.SetOwnerAndParent(Self, Self)` define o owner e parent.
  - `.SetSpace(2, 2)` / `.SetTopLeft(20, 20)` controlam espaçamento e posição inicial.

- **Hierarquia de níveis**
  - `.SubLevel(...)` abre um novo escopo de construção (ex.: um `TPanel`).
  - `.SuperLevel` retorna ao nível anterior.

- **Layouts em grid**
  - `.GridInit(Rows, Cols)` inicia um grid.
  - `.GridSetCellWidthAndHeight(W, H)` define as dimensões das células.
  - `.GridSetRowOffset(Row, Offset)` aplica deslocamento em linhas específicas.

- **Inserção dinâmica de controles**
  - `.External(procedure(ACreator: TControlCreator) ...)`
    permite criar controles em lote, como teclas, a partir de arrays/strings.

- **Controle de posicionamento no grid**
  - `.Break` → quebra de linha/coluna *(depende da `Direction` do level atual — veja a [documentação](docs/doc-pt.md) antes de usar esperando um simples "próxima linha abaixo"; `.BreakLine`/`.BreakColumn` são as versões incondicionais)*.
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

---

## 🧪 Galeria de Exemplos

Além dos trechos acima, a pasta [`examples/Builders`](examples/Builders) tem projetos completos e executáveis - uma subpasta por plataforma (`Lazarus`, `VCL`, `FMX`, mais alguns `VCL-Lazarus` multi-compilador). Cada um é um projeto Delphi/Lazarus independente que dá pra abrir e rodar direto.

| Plataforma | Exemplo | O que demonstra |
|---|---|---|
| Lazarus | [`schema-driven-ui`](examples/Builders/Lazarus/schema-driven-ui) | Um único gerador renderiza um schema de campos ora como grade de 2 colunas, ora como pilha de 1 coluna, e lê os valores de volta pelo nome do campo - UI cuja forma só existe em runtime. |
| Lazarus | [`book-search`](examples/Builders/Lazarus/book-search) | Tela de busca de livros na Open Library com campo de busca, paginação, barra de progresso e cards de livro gerados dinamicamente. |
| Lazarus | [`sqlite`](examples/Builders/Lazarus/sqlite) | App com SQLite: tela de login, menu principal/barra de status e um formulário de CRUD de usuários aberto dinamicamente em aba. |
| Lazarus | [`virtual-keyboard`](examples/Builders/Lazarus/virtual-keyboard) | Teclado virtual na tela (QWERTY + numérico) construído com botões estilizados num grid. |
| Lazarus | [`grid-mode`](examples/Builders/Lazarus/grid-mode) | Modo grid com mesclagem de linhas/colunas e células puladas, organizando 18 painéis/botões de forma irregular. |
| Lazarus | [`dialogs`](examples/Builders/Lazarus/dialogs) | Diálogos modais construídos via builder: lista de estados, telefone com máscara, seletor de cor, spin edit, grid de strings, trackbar, movedor de itens entre listas, e um grid de banco de dados pesquisável. |
| Lazarus | [`exemplo-menus`](examples/Builders/Lazarus/exemplo-menus) | Menu principal e menu de contexto (Arquivo/Editar/Buscar) construídos via `TMenuCreator`. |
| Lazarus | [`custom-helper-for-builders`](examples/Builders/Lazarus/custom-helper-for-builders) | Como escrever seus próprios métodos via `class helper` (ex.: `WithFontSize`, `WithVisible`) para as classes builder da lib. |
| VCL | [`exemplo-populator-tela-login`](examples/Builders/VCL/exemplo-populator-tela-login) | Tela de login completa (campos, checkbox, botão, rodapé) construída e alinhada inteiramente via builder. |
| VCL | [`virtual-keyboard`](examples/Builders/VCL/virtual-keyboard) | Teclado virtual na tela (QWERTY + numérico) com grids aninhados, offsets de linha e mesclagens. |
| VCL | [`dialogs`](examples/Builders/VCL/dialogs) | Diálogos modais: telefone com máscara, seletor de cor, calendário, grid de strings, trackbar, movedor de itens entre listas, e um grid de `ClientDataSet` pesquisável. |
| VCL | [`grid-autoexpand`](examples/Builders/VCL/grid-autoexpand) | Modo grid (`GridInit`/`GridFinish`) num exemplo mínimo 2×2. |
| VCL | [`exemplo-menu`](examples/Builders/VCL/exemplo-menu) | Menu principal e menu de contexto (Arquivo/Editar/Buscar) construídos via `TMenuCreator`. |
| VCL | [`exemplo-panelflow-card`](examples/Builders/VCL/exemplo-panelflow-card) | Adiciona cards de perfil (imagem + nome/contato) a um painel de fluxo sob demanda, um clique por vez. |
| VCL | [`exemplo-classes-personalizadas`](examples/Builders/VCL/exemplo-classes-personalizadas) | Como construir sua própria classe de controle customizada (um botão que se auto-clica) junto com controles normais. |
| VCL | [`custom-helper-for-builders`](examples/Builders/VCL/custom-helper-for-builders) | Como escrever seus próprios métodos via `class helper` para as classes builder da lib. |
| VCL | [`no-dfm`](examples/Builders/VCL/no-dfm) / [`no-dfm-2`](examples/Builders/VCL/no-dfm-2) | Formulários mínimos construídos **sem nenhum arquivo `.dfm`** - todo controle criado em código. |
| VCL-Lazarus | [`no-dfm`](examples/Builders/VCL-Lazarus/no-dfm) | A mesma ideia de "sem `.dfm`/`.lfm`", escrita uma vez e compilada sem alteração tanto no Delphi quanto no Lazarus. |
| FMX | [`dialogs`](examples/Builders/FMX/dialogs) | Diálogos modais: entrada de senha, seletor de data em grid de strings, seletor via trackbar, seletor de estado, e movedor de itens entre listas. |
| FMX | [`exemplo-menu`](examples/Builders/FMX/exemplo-menu) | Menu principal e menu de contexto (Arquivo/Editar/Buscar) construídos via `TMenuCreator`. |
| FMX | [`flow-card`](examples/Builders/FMX/flow-card) | Adiciona cards de perfil a um layout de fluxo sob demanda, um clique por vez. |
| FMX | [`CRUD1`](examples/Builders/FMX/CRUD1) | Demo mínima de layout com `TOPCBCreators` (um ponto de partida pequeno, não um CRUD completo apesar do nome). |

---

## ✅ Testado

A OPCB tem uma suíte de regressão automatizada extensa, rodada a cada mudança contra **os dois** compiladores que a lib suporta - porque cada um pega uma classe diferente de bug (a suíte Delphi/DUnitX, por exemplo, é a única que detecta vazamento de memória de forma confiável, via FastMM):

- **213 testes** na suíte Lazarus/FPC ([`tests/Lazarus/ControlBuilder`](tests/Lazarus/ControlBuilder)), fpcunit.
- **207 testes** na suíte Delphi/VCL ([`tests/Delphi/VCL`](tests/Delphi/VCL)), DUnitX.
- 0 falhas, 0 erros, 0 vazamentos de memória nas duas, na última rodada completa.

---

## 🧩 Requisitos

- **Delphi**: uma versão com suporte a generics e interfaces (recomendado 10.x ou mais recente). Compatível com **VCL** e **FMX**.
- **Lazarus / Free Pascal**: FPC 3.2+ (a lib depende de generics e advanced records). Compatível com a **LCL**.

---

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

## 📄 Licença

A OPCB é licenciada sob a **Apache License, Version 2.0** — veja [LICENSE](LICENSE) e [NOTICE](NOTICE) para o texto completo e detalhes de atribuição.

---
