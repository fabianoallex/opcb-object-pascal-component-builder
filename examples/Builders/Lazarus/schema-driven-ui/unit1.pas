unit Unit1;

{ Exemplo: UI orientada a schema.

  A ideia central deste exemplo não é "como criar um formulário de
  cadastro" - dá pra fazer isso com um .dfm/.lfm normal. A ideia é
  demonstrar algo que só faz sentido porque a tela é montada em runtime:
  UM ÚNICO gerador (RenderForm) interpreta um schema de campos (TFieldDef)
  e produz a tela - e o MESMO schema, sem nenhuma alteração, pode ser
  renderizado de formas visuais bem diferentes (grade de 2 colunas ou
  pilha de 1 coluna) trocando um único parâmetro. Com formulários
  desenhados em tempo de design isso exigiria duas telas duplicadas.

  O botão "Enviar" fecha o ciclo: percorre o mesmo schema e lê os valores
  de volta via GetControl<T> pelo nome do campo, provando que o
  round-trip funciona, não é só cosmético. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin, OPCB;

type
  TFieldType = (ftText, ftNumber, ftBoolean, ftDate, ftChoice);

  TFieldDef = record
    Name: string;
    Caption: string;
    Section: string;
    FieldType: TFieldType;
    Choices: array of string;
    Required: Boolean;
  end;

  TFieldDefArray = array of TFieldDef;

  { TForm1 }

  TForm1 = class(TForm)
    BtnSubmit: TButton;
    BtnToggleLayout: TButton;
    PanelContent: TPanel;
    PanelToolbar: TPanel;
    procedure BtnSubmitClick(Sender: TObject);
    procedure BtnToggleLayoutClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FCreator: TControlCreator;
    FSchema: TFieldDefArray;
    FTwoColumns: Boolean;
    procedure RenderForm;
    procedure ClearContent;
    procedure CloseSection;
    function CreateFieldLabel(const AField: TFieldDef): TControlBuilder;
    function ReadFieldValueAsText(const AField: TFieldDef): string;
    procedure UpdateToggleButtonCaption;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

function MakeField(const AName, ACaption, ASection: string;
  AFieldType: TFieldType; ARequired: Boolean): TFieldDef;
begin
  Result.Name := AName;
  Result.Caption := ACaption;
  Result.Section := ASection;
  Result.FieldType := AFieldType;
  Result.Required := ARequired;
end;

function MakeChoiceField(const AName, ACaption, ASection: string;
  const AChoices: array of string; ARequired: Boolean): TFieldDef;
var
  I: Integer;
begin
  Result := MakeField(AName, ACaption, ASection, ftChoice, ARequired);
  SetLength(Result.Choices, Length(AChoices));
  for I := 0 to High(AChoices) do
    Result.Choices[I] := AChoices[I];
end;

function BuildCustomerSchema: TFieldDefArray;
{ Este schema poderia vir de qualquer lugar decidido em runtime - um
  arquivo, uma resposta de API, metadado de tabela do banco. Aqui ele está
  fixo no código só pra manter o exemplo autocontido; o ponto é que
  RenderForm não sabe nem precisa saber disso. }
begin
  SetLength(Result, 7);
  Result[0] := MakeField('nome', 'Nome completo', 'Dados pessoais', ftText, True);
  Result[1] := MakeField('nascimento', 'Data de nascimento', 'Dados pessoais', ftDate, False);
  Result[2] := MakeField('newsletter', 'Quero receber novidades', 'Dados pessoais', ftBoolean, False);
  Result[3] := MakeChoiceField('faixa_etaria', 'Faixa etária', 'Dados pessoais',
    ['18-25', '26-40', '41-60', '60+'], False);
  Result[4] := MakeField('cidade', 'Cidade', 'Endereço', ftText, True);
  Result[5] := MakeField('cep', 'CEP', 'Endereço', ftText, False);
  Result[6] := MakeField('renda', 'Renda mensal (R$)', 'Endereço', ftNumber, False);
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FSchema := BuildCustomerSchema;
  FTwoColumns := True;
  UpdateToggleButtonCaption;
  RenderForm;
end;

procedure TForm1.UpdateToggleButtonCaption;
begin
  if FTwoColumns then
    BtnToggleLayout.Caption := 'Alternar para 1 coluna'
  else
    BtnToggleLayout.Caption := 'Alternar para 2 colunas';
end;

procedure TForm1.ClearContent;
begin
  // Os controles gerados pertencem (Owner) ao form, não ao TControlCreator -
  // liberar o Creator não os destrói. Quem monta a UI em runtime também
  // precisa desmontar em runtime: antes de renderizar de novo, é preciso
  // liberar explicitamente o que já existe em PanelContent.
  while PanelContent.ControlCount > 0 do
    PanelContent.Controls[0].Free;

  FreeAndNil(FCreator);
end;

function TForm1.CreateFieldLabel(const AField: TFieldDef): TControlBuilder;
var
  LabelCaption: string;
begin
  LabelCaption := AField.Caption;
  if AField.Required then
    LabelCaption := LabelCaption + ' *';
  Result := TControlBuilder.Create(TLabel).WithCaption(LabelCaption);
end;

procedure TForm1.CloseSection;
begin
  // GridFinish não faz nada se o level atual não tiver grid ativo (modo 1
  // coluna nunca ativa grid), então é seguro chamar sempre, sem checar
  // FTwoColumns aqui.
  FCreator.GridFinish.SuperLevel;
end;

procedure TForm1.RenderForm;
var
  Field: TFieldDef;
  CurrentSection: string;
  HeaderLabel: TLabel;
  ComboBox: TComboBox;
  I: Integer;
begin
  ClearContent;

  FCreator := TControlCreator.Create;
  FCreator
    .SetOwnerAndParent(Self, PanelContent)
    .SetTopLeft(10, 10)
    .SetSpace(6, 10)
    .SetDirection(cpdVertical)
  ;

  CurrentSection := '';

  for Field in FSchema do
  begin
    if Field.Section <> CurrentSection then
    begin
      if CurrentSection <> '' then
        CloseSection;

      CurrentSection := Field.Section;

      // Caption precisa estar setado ANTES do Add/Build - o layout usa o
      // tamanho do controle no momento em que é posicionado. Setar o
      // Caption depois (num label antes vazio) reserva espaço de menos e
      // o cabeçalho acaba sobrepondo o campo seguinte.
      FCreator.Add(TControlBuilder.Create(TLabel, HeaderLabel).WithCaption(CurrentSection));
      HeaderLabel.Font.Style := [fsBold];

      // Break() decide entre BreakLine/BreakColumn conforme a Direction do
      // level atual (linha abaixo quando horizontal, coluna ao lado quando
      // vertical - pense em "quebra ortogonal ao fluxo"). Como a raiz é
      // vertical, Break() aqui chamaria BreakColumn e jogaria a seção pro
      // lado em vez de para baixo. BreakLine força "próxima linha, abaixo"
      // independente da Direction, que é o que se quer depois de um
      // cabeçalho.
      FCreator
        .BreakLine
        .SubLevel(CurrentSection)
      ;

      if FTwoColumns then
        FCreator
          .SetDirection(cpdHorizontal) // preenche a grade linha-a-linha (label, input)
          .GridInit(1, 2)
            .GridAutoExpandRows
            // CellHeight fica 0 se nunca setado, e GetRowHeight cai nesse
            // default quando a linha não tem altura própria - sem isso
            // todas as linhas ficam com altura zero e os campos se
            // empilham exatamente na mesma posição Y.
            .GridSetCellWidthAndHeight(160, 28)
            .GridSetColWidth(0, 160)
            .GridSetColWidth(1, 220)
        ;
      // else: a seção herda Direction=cpdVertical da raiz, então cada
      // Add() já empilha o controle abaixo do anterior sozinho - não
      // precisa (e não deve) chamar Break entre label e input aqui.
    end;

    FCreator.Add(CreateFieldLabel(Field));

    case Field.FieldType of
      ftText:
        FCreator.Add(TControlBuilder.Create(TEdit, Field.Name));
      ftNumber:
        FCreator.Add(TControlBuilder.Create(TFloatSpinEdit, Field.Name));
      ftBoolean:
        FCreator.Add(TControlBuilder.Create(TCheckBox, Field.Name));
      ftDate:
        FCreator.Add(TControlBuilder.Create(TEdit, Field.Name)
          .WithProp('TextHint', 'dd/mm/aaaa'));
      ftChoice:
        begin
          FCreator.Add(TComboBox, Field.Name, ComboBox);
          ComboBox.Style := csDropDownList;
          for I := 0 to High(Field.Choices) do
            ComboBox.Items.Add(Field.Choices[I]);
          if ComboBox.Items.Count > 0 then
            ComboBox.ItemIndex := 0;
        end;
    end;
    // Sem Break aqui: no modo 1 coluna a seção também herda Direction
    // vertical, então label e input já empilham sozinhos (ver comentário
    // acima). No modo grade, GridMode cuida de avançar pra próxima
    // célula/linha sozinho.
  end;

  if CurrentSection <> '' then
    CloseSection;
end;

function TForm1.ReadFieldValueAsText(const AField: TFieldDef): string;
begin
  case AField.FieldType of
    ftText:
      Result := FCreator.specialize GetControl<TEdit>(AField.Name).Text;
    ftNumber:
      Result := FloatToStr(FCreator.specialize GetControl<TFloatSpinEdit>(AField.Name).Value);
    ftBoolean:
      if FCreator.specialize GetControl<TCheckBox>(AField.Name).Checked then
        Result := 'Sim'
      else
        Result := 'Não';
    ftDate:
      Result := FCreator.specialize GetControl<TEdit>(AField.Name).Text;
    ftChoice:
      Result := FCreator.specialize GetControl<TComboBox>(AField.Name).Text;
  else
    Result := '';
  end;
end;

procedure TForm1.BtnToggleLayoutClick(Sender: TObject);
begin
  // Reforça o ponto do exemplo: o MESMO FSchema, sem alteração nenhuma, é
  // renderizado de novo de um jeito visualmente diferente. Valores
  // digitados antes do toggle são perdidos de propósito - persisti-los
  // entre re-renderizações ficou fora do escopo deste exemplo.
  FTwoColumns := not FTwoColumns;
  UpdateToggleButtonCaption;
  RenderForm;
end;

procedure TForm1.BtnSubmitClick(Sender: TObject);
var
  Field: TFieldDef;
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    for Field in FSchema do
      Lines.Add(Field.Caption + ': ' + ReadFieldValueAsText(Field));

    ShowMessage(
      'Valores lidos de volta do schema (via GetControl<T> pelo nome do campo):'
      + LineEnding + LineEnding + Lines.Text
    );
  finally
    Lines.Free;
  end;
end;

end.
