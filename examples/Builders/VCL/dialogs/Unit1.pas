unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, OPCB, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.ExtCtrls, Vcl.ButtonGroup, Vcl.WinXCalendars, Vcl.CheckLst, Vcl.Grids,
  Data.DB, Vcl.DBGrids, Datasnap.DBClient;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
  private
    procedure SetupListBox(AControl: TControl);
    procedure SetupMaskEdit(AControl: TControl);
    procedure SetupStringGrid(AControl: TControl);
    procedure SetupTrackBar(AControl: TControl);
    procedure SetupListBoxLeft(AControl: TControl);
    procedure SetupListBoxRight(AControl: TControl);
    procedure ButtonMoveToLeftClick(ASender: TObject);
    procedure ButtonMoveToRightClick(ASender: TObject);
    procedure SetupCDS(AComponent: TComponent);
    procedure SetupDS(AComponent: TComponent);
    procedure SetupDBGrid(AControl: TControl);
    procedure SetupEditSearch(AControl: TControl);
    procedure EditSearchChange(ASender: TObject);
  public
  end;

var
  Form1: TForm1;

implementation

uses
  UDialogs, Vcl.Mask;

{$R *.dfm}

procedure TForm1.SetupMaskEdit(AControl: TControl);
var
  MaskEdit: TMaskEdit;
begin
  MaskEdit := (AControl as TMaskEdit);
  MaskEdit.EditMask := '!\(99\) 00000-0000;0;_';
  MaskEdit.Clear;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  ControlDialog: TControlDialog;
  MaskEdit: TMaskEdit;
begin
  ControlDialog := TControlDialog.CreateNew(
    Self,
    'Informe o Celular',
    TControlBuilder.Create(TMaskEdit, 'MaskEditCelular').WithWidth(250).Setup(SetupMaskEdit)
  );

  try
    if ControlDialog.ShowModal = mrOk then
    begin
      MaskEdit := ControlDialog.ControlCreator.GetControl<TMaskEdit>('MaskEditCelular');
      ShowMessage('Informou ' + MaskEdit.Text);
    end;
  finally
    ControlDialog.Free;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  ControlDialog: TControlDialog;
  ColorBox: TColorBox;
begin
  ControlDialog := TControlDialog.CreateNew(
    Self,
    'Selecione uma cor',
    TControlBuilder.Create(TColorBox, 'ColorBox').WithWidth(250)
  );

  try
    if ControlDialog.ShowModal = mrOk then
    begin
      ColorBox := ControlDialog.ControlCreator.GetControl<TColorBox>('ColorBox');
      Panel1.Color := ColorBox.Selected;
    end;
  finally
    ControlDialog.Free;
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  ControlDialog: TControlDialog;
  CalendarView: TCalendarView;
begin
  ControlDialog := TControlDialog.CreateNew(
    Self,
    'Selecione uma data',
    TControlBuilder.Create(TCalendarView, 'CalendarView')
  );

  try
    if ControlDialog.ShowModal = mrOk then
    begin
      CalendarView := ControlDialog.ControlCreator.GetControl<TCalendarView>('CalendarView');
      ShowMessage('Selecionou a data ' + DateToStr(CalendarView.Date));
    end;
  finally
    ControlDialog.Free;
  end;
end;

procedure TForm1.SetupStringGrid(AControl: TControl);
var
  StringGrid: TStringGrid;
begin
  StringGrid := (AControl as TStringGrid);

  StringGrid.ColCount := 3;
  StringGrid.RowCount := 6;

  StringGrid.ColWidths[0] := 50;
  StringGrid.ColWidths[1] := 150;
  StringGrid.ColWidths[2] := 100;

  StringGrid.Cells[0, 0] := 'ID';
  StringGrid.Cells[1, 0] := 'Nome';
  StringGrid.Cells[2, 0] := 'Idade';

  StringGrid.Cells[0, 1] := '1';
  StringGrid.Cells[1, 1] := 'Maria';
  StringGrid.Cells[2, 1] := '23';

  StringGrid.Cells[0, 2] := '2';
  StringGrid.Cells[1, 2] := 'João';
  StringGrid.Cells[2, 2] := '35';

  StringGrid.Cells[0, 3] := '3';
  StringGrid.Cells[1, 3] := 'Pedro';
  StringGrid.Cells[2, 3] := '29';

  StringGrid.Cells[0, 4] := '4';
  StringGrid.Cells[1, 4] := 'Ana';
  StringGrid.Cells[2, 4] := '41';

  StringGrid.Cells[0, 5] := '5';
  StringGrid.Cells[1, 5] := 'Lucas';
  StringGrid.Cells[2, 5] := '18';
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  ControlDialog: TControlDialog;
  StringGrid: TStringGrid;
  ACol, ARow: Integer;
  Valor: string;
begin
  ControlDialog := TControlDialog.CreateNew(
    Self,
    'Selecione uma data',
    TControlBuilder.Create(TStringGrid, 'StringGrid').WithWidthAndHeight(350, 400).Setup(SetupStringGrid)
  );

  try
    if ControlDialog.ShowModal = mrOk then
    begin
      StringGrid := ControlDialog.ControlCreator.GetControl<TStringGrid>('StringGrid');

      ACol := StringGrid.Col;
      ARow := StringGrid.Row;
      Valor := StringGrid.Cells[ACol, ARow];

      ShowMessage(Format('Coluna: %d | Linha: %d | Valor: %s', [ACol, ARow, Valor]));
    end;
  finally
    ControlDialog.Free;
  end;
end;

procedure TForm1.SetupTrackBar(AControl: TControl);
var
  TrackBar: TTrackBar;
begin
  TrackBar := (AControl as TTrackBar);
  TrackBar.Min := 0;
  TrackBar.Max := 100;
end;

procedure TForm1.Button6Click(Sender: TObject);
var
  ControlDialog: TControlDialog;
  TrackBar: TTrackBar;
begin
  ControlDialog := TControlDialog.CreateNew(
    Self,
    'Selecione a posição',
    TControlBuilder.Create(TTrackBar, 'TrackBar').WithWidth(500).Setup(SetupTrackBar)
  );

  try
    if ControlDialog.ShowModal = mrOk then
    begin
      TrackBar := ControlDialog.ControlCreator.GetControl<TTrackBar>('TrackBar');
      ShowMessage('Selecionou a posição' + TrackBar.Position.ToString);
    end;
  finally
    ControlDialog.Free;
  end;
end;

procedure TForm1.SetupListBoxLeft(AControl: TControl);
begin
  (AControl as TListBox).Items.Add('A');
  (AControl as TListBox).Items.Add('B');
  (AControl as TListBox).Items.Add('C');
end;

procedure TForm1.SetupListBoxRight(AControl: TControl);
begin
  (AControl as TListBox).Items.Add('D');
  (AControl as TListBox).Items.Add('E');
  (AControl as TListBox).Items.Add('F');
end;

procedure TForm1.ButtonMoveToLeftClick(ASender: TObject);
var
  I: Integer;
  ListBoxRight, ListBoxLeft: TListBox;
begin
  ListBoxRight := TComponentRegistry.GetComponentFromContext<TListBox>('ContextKey-Exemplo-Panel', 'ListBoxRight');
  ListBoxLeft := TComponentRegistry.GetComponentFromContext<TListBox>('ContextKey-Exemplo-Panel', 'ListBoxLeft');

  for I := ListBoxRight.Items.Count - 1 downto 0 do
  begin
    if ListBoxRight.Selected[I] then
    begin
      ListBoxLeft.Items.Add(ListBoxRight.Items[I]);
      ListBoxRight.Items.Delete(I);
    end;
  end;
end;

procedure TForm1.ButtonMoveToRightClick(ASender: TObject);
var
  I: Integer;
  ListBoxRight, ListBoxLeft: TListBox;
begin
  ListBoxRight := TComponentRegistry.GetComponentFromContext<TListBox>('ContextKey-Exemplo-Panel', 'ListBoxRight');
  ListBoxLeft := TComponentRegistry.GetComponentFromContext<TListBox>('ContextKey-Exemplo-Panel', 'ListBoxLeft');

  for I := ListBoxLeft.Items.Count - 1 downto 0 do
  begin
    if ListBoxLeft.Selected[I] then
    begin
      ListBoxRight.Items.Add(ListBoxLeft.Items[I]);
      ListBoxLeft.Items.Delete(I);
    end;
  end;
end;

procedure TForm1.Button7Click(Sender: TObject);
var
  ControlDialog: TControlDialog;

  procedure ConfigPanelMain;
  var
    ControlCreator: TControlCreator;
  begin
    ControlCreator := TControlCreator.Create(ControlDialog.ControlCreator.Registry.ContextKey); // usa o mesmo context do dialog
    try
      ControlCreator
        .WithOwnerAndParent(
          ControlDialog,
          ControlDialog.ControlCreator.GetControl<TPanel>('PanelMain')
        )
        .SetTopLeft(10, 10)
        .SetSpace(20, 20)
        .AddControl(TControlBuilder.Create(TListBox, 'ListBoxLeft').WithWidthAndHeight(180, 280).Setup(SetupListBoxLeft))
        .SubLevel(cpdVertical)
          .AddControl(TControlBuilder.Create(TButton, 'ButtonMoveToRight').WithCaption('>').WithOnClick(ButtonMoveToRightClick))
          .AddControl(TControlBuilder.Create(TButton, 'ButtonMoveToLeft').WithCaption('<').WithOnClick(ButtonMoveToLeftClick))
        .SuperLevel
        .AddControl(TControlBuilder.Create(TListBox, 'ListBoxRight').WithWidthAndHeight(180, 280).Setup(SetupListBoxRight))
        .CenterControlsInParentVertically(['ButtonMoveToRight', 'ButtonMoveToLeft'])
    finally
      ControlCreator.Free;
    end;
  end;

begin
  ControlDialog := TControlDialog.CreateNew(
    Self,
    'Mova os itens',
    TControlBuilder.Create(TPanel, 'PanelMain').WithWidthAndHeight(500, 300).WithCaption(''),
    'ContextKey-Exemplo-Panel'
  );

  ConfigPanelMain; // Adiciona outros controles no Panel

  try
    if ControlDialog.ShowModal = mrOk then
    begin
      ShowMessage(
        'Left: ' + #13 + ControlDialog.ControlCreator.GetControl<TListBox>('ListBoxLeft').Items.Text + #13 +
        'Right: ' + #13 + ControlDialog.ControlCreator.GetControl<TListBox>('ListBoxRight').Items.Text
      );
    end;
  finally
    ControlDialog.Free;
  end;
end;

procedure TForm1.SetupCDS(AComponent: TComponent);
var
  CDS: TClientDataSet;
begin
  CDS := AComponent as TClientDataSet;

  with CDS.FieldDefs do
  begin
    Clear;
    Add('ID', ftInteger);
    Add('Cidade', ftString, 100);
    Add('Estado', ftString, 50);
    Add('Populacao', ftInteger);
  end;

  CDS.CreateDataSet;

  CDS.AppendRecord([1, 'São Paulo', 'SP', 12300000]);
  CDS.AppendRecord([2, 'Rio de Janeiro', 'RJ', 6748000]);
  CDS.AppendRecord([3, 'Belo Horizonte', 'MG', 2528000]);
  CDS.AppendRecord([4, 'Curitiba', 'PR', 1960000]);
  CDS.AppendRecord([5, 'Salvador', 'BA', 2880000]);
  CDS.AppendRecord([6, 'Fortaleza', 'CE', 2687000]);
  CDS.AppendRecord([7, 'Manaus', 'AM', 2219000]);
  CDS.AppendRecord([8, 'Recife', 'PE', 1650000]);
  CDS.AppendRecord([9, 'Porto Alegre', 'RS', 1480000]);
  CDS.AppendRecord([10, 'Goiânia', 'GO', 1530000]);
  CDS.AppendRecord([11, 'Belém', 'PA', 1500000]);
  CDS.AppendRecord([12, 'Brasília', 'DF', 3050000]);
  CDS.AppendRecord([13, 'Campinas', 'SP', 1214000]);
  CDS.AppendRecord([14, 'São Luís', 'MA', 1100000]);
  CDS.AppendRecord([15, 'Cuiabá', 'MT', 618000]);
  CDS.AppendRecord([16, 'João Pessoa', 'PB', 817000]);
  CDS.AppendRecord([17, 'Teresina', 'PI', 868000]);
  CDS.AppendRecord([18, 'Maceió', 'AL', 1010000]);
  CDS.AppendRecord([19, 'Natal', 'RN', 890000]);
  CDS.AppendRecord([20, 'Vitória', 'ES', 365000]);
end;

procedure TForm1.SetupDS(AComponent: TComponent);
var
  I: Integer;
  CDS: TClientDataSet;
  DS: TDataSource;
begin
  DS := (AComponent as TDataSource);
  CDS := TComponentRegistry.GetComponentFromContext<TClientDataSet>('ContextKey-Exemplo-Panel', 'CDS');
  DS.DataSet := CDS;
end;

procedure TForm1.SetupDBGrid(AControl: TControl);
var
  DBGrid: TDBGrid;
  DS: TDataSource;
begin
  DBGrid := (AControl as TDBGrid);
  DS := TComponentRegistry.GetComponentFromContext<TDataSource>('ContextKey-Exemplo-Panel', 'DS');
  DBGrid.DataSource := DS;
  DBGrid.Options := DBGrid.Options + [dgRowSelect, dgAlwaysShowSelection];
  DBGrid.Columns[0].Width := 50;
  DBGrid.Columns[1].Width := 150;
  DBGrid.Columns[2].Width := 70;
  DBGrid.Columns[2].Width := 100;
end;

procedure TForm1.EditSearchChange(ASender: TObject);
var
  Edit: TEdit;
  CDS: TClientDataSet;
begin
  Edit := (ASender as TEdit);
  CDS := TComponentRegistry.GetComponentFromContext<TClientDataSet>('ContextKey-Exemplo-Panel', 'CDS');
  CDS.Locate('Cidade', Edit.Text, [loCaseInsensitive, loPartialKey]);
end;

procedure TForm1.SetupEditSearch(AControl: TControl);
var
  Edit: TEdit;
  CDS: TClientDataSet;
begin
  Edit := (AControl as TEdit);
  CDS := TComponentRegistry.GetComponentFromContext<TClientDataSet>('ContextKey-Exemplo-Panel', 'CDS');
  Edit.OnChange := EditSearchChange;
end;

procedure TForm1.Button8Click(Sender: TObject);
var
  ControlDialog: TControlDialog;

  procedure ConfigPanelMain;
  var
    Creators: TOPCBCreators;
  begin
    Creators := TOPCBCreators.Create(ControlDialog.ControlCreator.Registry.ContextKey); // usa o mesmo context do dialog
    try
      Creators.AsComponentCreator
        .WithOwner(ControlDialog)
        .Add(TComponentBuilder.Create(TClientDataSet, 'CDS').Setup(SetupCDS))
        .Add(TComponentBuilder.Create(TDataSource, 'DS').Setup(SetupDS))
      ;

      Creators.AsControlCreator
        .WithOwnerAndParent(
          ControlDialog,
          ControlDialog.ControlCreator.GetControl<TPanel>('PanelMain')
        )
        .SetTopLeft(10, 10)
        .SetSpace(20, 20)
        .SubLevel(TControlBuilder.Create(TPanel, 'Px').WithAlign(alTop).WithCaption(''))
          .SetTopLeft(5, 5)
          .AddControl(TControlBuilder.Create(TEdit, 'eee').WithWidth(480).Setup(SetupEditSearch))
        .SuperLevel
        .AddControl(TControlBuilder.Create(TDBGrid).WithAlign(alClient).Setup(SetupDBGrid))
    finally
      Creators.Free;
    end;
  end;

begin
  ControlDialog := TControlDialog.CreateNew(
    Self,
    'Mova os itens',
    TControlBuilder.Create(TPanel, 'PanelMain').WithWidthAndHeight(500, 300).WithCaption(''),
    'ContextKey-Exemplo-Panel'
  );

  ConfigPanelMain; // Adiciona outros controles no Panel

  try
    if ControlDialog.ShowModal = mrOk then
    begin
      var CDS := ControlDialog.ControlCreator.Registry.GetComponent<TClientDataSet>('CDS');
      if not CDS.Eof then
        ShowMessage('Selecionou: ' + CDS.FieldByName('Cidade').AsString);
    end;
  finally
    ControlDialog.Free;
  end;
end;

procedure TForm1.SetupListBox(AControl: TControl);
var
  ListBox: TListBox;
begin
  ListBox := (AControl as TListBox);
  ListBox.Items.Clear;
  ListBox.Items.Add('Acre - AC');
  ListBox.Items.Add('Alagoas - AL');
  ListBox.Items.Add('Amapá - AP');
  ListBox.Items.Add('Amazonas - AM');
  ListBox.Items.Add('Bahia - BA');
  ListBox.Items.Add('Ceará - CE');
  ListBox.Items.Add('Distrito Federal - DF');
  ListBox.Items.Add('Espírito Santo - ES');
  ListBox.Items.Add('Goiás - GO');
  ListBox.Items.Add('Maranhão - MA');
  ListBox.Items.Add('Mato Grosso - MT');
  ListBox.Items.Add('Mato Grosso do Sul - MS');
  ListBox.Items.Add('Minas Gerais - MG');
  ListBox.Items.Add('Pará - PA');
  ListBox.Items.Add('Paraíba - PB');
  ListBox.Items.Add('Paraná - PR');
  ListBox.Items.Add('Pernambuco - PE');
  ListBox.Items.Add('Piauí - PI');
  ListBox.Items.Add('Rio de Janeiro - RJ');
  ListBox.Items.Add('Rio Grande do Norte - RN');
  ListBox.Items.Add('Rio Grande do Sul - RS');
  ListBox.Items.Add('Rondônia - RO');
  ListBox.Items.Add('Roraima - RR');
  ListBox.Items.Add('Santa Catarina - SC');
  ListBox.Items.Add('São Paulo - SP');
  ListBox.Items.Add('Sergipe - SE');
  ListBox.Items.Add('Tocantins - TO');
  ListBox.ItemIndex := 0;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  ControlDialog: TControlDialog;
  ListBoxCidades: TListBox;
begin
  ControlDialog := TControlDialog.CreateNew(
    Self,
    'Selecione um estado:',
    TControlBuilder.Create(TListBox, 'ListBoxCidades').WithWidthAndHeight(350, 400).Setup(SetupListBox)
  );

  try
    if ControlDialog.ShowModal = mrOk then
    begin
      ListBoxCidades := ControlDialog.ControlCreator.GetControl<TListBox>('ListBoxCidades');
      if ListBoxCidades.ItemIndex >= 0 then
        ShowMessage('Selecionou ' + ListBoxCidades.Items[ListBoxCidades.ItemIndex]);
    end;
  finally
    ControlDialog.Free;
  end;
end;

end.
