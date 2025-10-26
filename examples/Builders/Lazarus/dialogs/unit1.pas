unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ActnList, ExtCtrls, OPCB,
  Buttons, StdCtrls, UDialogs, MaskEdit, ColorBox, Spin, Grids, ComCtrls, memds, DB,
  DBGrids;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
  private
    procedure ButtonMoveToLeftClick(ASender: TObject);
    procedure ButtonMoveToRightClick(ASender: TObject);
    procedure EditSearchChange(ASender: TObject);
    procedure SetupDBGrid(AControl: TControl);
    procedure SetupDS(AComponent: TComponent);
    procedure SetupEditSearch(AControl: TControl);
    procedure SetupListBox(AControl: TControl);
    procedure SetupListBoxLeft(AControl: TControl);
    procedure SetupListBoxRight(AControl: TControl);
    procedure SetupMaskEdit(AControl: TControl);
    procedure SetupMDS(AComponent: TComponent);
    procedure SetupStringGrid(AControl: TControl);
    procedure SetupTrackBar(AControl: TControl);

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

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
    TControlBuilder.Create(TListBox, 'ListBoxCidades').WithWidthAndHeight(350, 400).Setup(@SetupListBox)
  );

  try
    if ControlDialog.ShowModal = mrOk then
    begin
      ListBoxCidades := ControlDialog.ControlCreator.GetControl('ListBoxCidades') as TListBox;
      if ListBoxCidades.ItemIndex >= 0 then
        ShowMessage('Selecionou ' + ListBoxCidades.Items[ListBoxCidades.ItemIndex]);
    end;
  finally
    ControlDialog.Free;
  end;
end;

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
    TControlBuilder.Create(TMaskEdit, 'MaskEditCelular').WithWidth(250).Setup(@SetupMaskEdit)
  );

  try
    if ControlDialog.ShowModal = mrOk then
    begin
      MaskEdit := ControlDialog.ControlCreator.GetControl('MaskEditCelular') as TMaskEdit;
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
      ColorBox := ControlDialog.ControlCreator.GetControl('ColorBox') as TColorBox;
      Panel1.Color := ColorBox.Selected;
    end;
  finally
    ControlDialog.Free;
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  ControlDialog: TControlDialog;
  FloatSpinEdit: TFloatSpinEdit;
begin
  ControlDialog := TControlDialog.CreateNew(
    Self,
    'Informe o valor',
    TControlBuilder.Create(TFloatSpinEdit, 'FloatSpinEdit').WithWidth(250)
  );

  try
    if ControlDialog.ShowModal = mrOk then
    begin
      FloatSpinEdit := ControlDialog.ControlCreator.GetControl('FloatSpinEdit') as TFloatSpinEdit;
      ShowMessage('Informou ' + FloatSpinEdit.Value.ToString);
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
    TControlBuilder.Create(TStringGrid, 'StringGrid').WithWidthAndHeight(350, 400).Setup(@SetupStringGrid)
  );

  try
    if ControlDialog.ShowModal = mrOk then
    begin
      StringGrid := ControlDialog.ControlCreator.GetControl('StringGrid') as TStringGrid;

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
    TControlBuilder.Create(TTrackBar, 'TrackBar').WithWidth(500).Setup(@SetupTrackBar)
  );

  try
    if ControlDialog.ShowModal = mrOk then
    begin
      TrackBar := ControlDialog.ControlCreator.GetControl('TrackBar') as TTrackBar;
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
  ListBoxRight := TComponentRegistry.GetComponentFromContext('ContextKey-Exemplo-Panel', 'ListBoxRight') as TListBox;
  ListBoxLeft := TComponentRegistry.GetComponentFromContext('ContextKey-Exemplo-Panel', 'ListBoxLeft') as TListBox;

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
  ListBoxRight := TComponentRegistry.GetComponentFromContext('ContextKey-Exemplo-Panel', 'ListBoxRight') as TListBox;
  ListBoxLeft := TComponentRegistry.GetComponentFromContext('ContextKey-Exemplo-Panel', 'ListBoxLeft') as TListBox;

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
          ControlDialog.ControlCreator.GetControl('PanelMain') as TPanel
        )
        .SetTopLeft(10, 10)
        .SetSpace(20, 20)
        .Add(TControlBuilder.Create(TListBox, 'ListBoxLeft').WithWidthAndHeight(180, 280).Setup(@SetupListBoxLeft))
        .SubLevel(cpdVertical)
          .Add(TControlBuilder.Create(TButton, 'ButtonMoveToRight').WithCaption('>').WithOnClick(@ButtonMoveToRightClick))
          .Add(TControlBuilder.Create(TButton, 'ButtonMoveToLeft').WithCaption('<').WithOnClick(@ButtonMoveToLeftClick))
        .SuperLevel
        .Add(TControlBuilder.Create(TListBox, 'ListBoxRight').WithWidthAndHeight(180, 280).Setup(@SetupListBoxRight))
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
        'Left: ' + #13 + (ControlDialog.ControlCreator.GetControl('ListBoxLeft') as TListBox).Items.Text + #13 +
        'Right: ' + #13 + (ControlDialog.ControlCreator.GetControl('ListBoxRight') as TListBox).Items.Text
      );
    end;
  finally
    ControlDialog.Free;
  end;
end;

procedure TForm1.SetupMDS(AComponent: TComponent);
var
  MDS: TMemDataSet;
begin
  MDS := (AComponent as TMemDataSet);
  with MDS.FieldDefs do
  begin
    Clear;
    Add('ID', ftInteger);
    Add('Cidade', ftString, 100);
    Add('UF', ftString, 2);
  end;

  MDS.CreateTable; // CreateDataset;
  MDS.Open;

  // Inserindo alguns dados de cidades
  MDS.AppendRecord([1, 'São Paulo', 'SP']);
  MDS.AppendRecord([2, 'Rio de Janeiro', 'RJ']);
  MDS.AppendRecord([3, 'Belo Horizonte', 'MG']);
  MDS.AppendRecord([4, 'Curitiba', 'PR']);
  MDS.AppendRecord([5, 'Porto Alegre', 'RS']);
  MDS.AppendRecord([6, 'Recife', 'PE']);
  MDS.AppendRecord([7, 'Fortaleza', 'CE']);
  MDS.AppendRecord([8, 'Manaus', 'AM']);
  MDS.AppendRecord([9, 'Salvador', 'BA']);
  MDS.AppendRecord([10, 'Brasília', 'DF']);
end;

procedure TForm1.SetupDS(AComponent: TComponent);
var
  I: Integer;
  MemDS_: TMemDataSet;
  DS: TDataSource;
begin
  DS := (AComponent as TDataSource);
  MemDS_ := TComponentRegistry.GetComponentFromContext('ContextKey-Exemplo-Panel', 'MDS') as TMemDataset;
  DS.DataSet := MemDS_;
end;

procedure TForm1.SetupDBGrid(AControl: TControl);
var
  DBGrid: TDBGrid;
  DataSource: TDataSource;
begin
  DBGrid := (AControl as TDBGrid);
  DataSource := TComponentRegistry.GetComponentFromContext('ContextKey-Exemplo-Panel', 'DS') as TDataSource;
  DBGrid.DataSource := DataSource;
  DBGrid.Options := DBGrid.Options + [dgRowSelect, dgAlwaysShowSelection];
  DBGrid.Columns[0].Width := 50;
  DBGrid.Columns[1].Width := 150;
  DBGrid.Columns[2].Width := 70;
  DBGrid.Columns[2].Width := 100;
end;

procedure TForm1.EditSearchChange(ASender: TObject);
var
  EditSearch_: TEdit;
  MemDS: TMemDataSet;
begin
  EditSearch_ := (ASender as TEdit);
  MemDS := TComponentRegistry.GetComponentFromContext('ContextKey-Exemplo-Panel', 'MDS') as TMemDataset;
  MemDS.Locate('Cidade', EditSearch_.Text, [loCaseInsensitive, loPartialKey]);
end;

procedure TForm1.SetupEditSearch(AControl: TControl);
var
  Edit_: TEdit;
begin
  Edit_ := (AControl as TEdit);
  Edit_.OnChange := @EditSearchChange;
end;

procedure TForm1.Button8Click(Sender: TObject);
var
  ControlDialog: TControlDialog;
  MDS: TMemDataset;

  procedure ConfigPanelMain;
  var
    Creators: TOPCBCreators;
  begin
    Creators := TOPCBCreators.Create(ControlDialog.ControlCreator.Registry.ContextKey); // usa o mesmo context do dialog
    try
      Creators.AsComponentCreator
        .WithOwner(ControlDialog)
        .Add(TComponentBuilder.Create(TMemDataset, 'MDS').Setup(@SetupMDS))
        .Add(TComponentBuilder.Create(TDataSource, 'DS').Setup(@SetupDS))
      ;
      Creators.AsControlCreator
        .WithOwnerAndParent(
          ControlDialog,
          ControlDialog.ControlCreator.GetControl('PanelMain') as TPanel
        )
        .SetTopLeft(10, 10)
        .SetSpace(20, 20)
        .SubLevel(TControlBuilder.Create(TPanel).WithAlign(alTop).WithHeight(70).WithCaption(''), cpdVertical)
          .SetTopLeft(5, 5)
          .SetSpace(5, 5)
          .Add(TControlBuilder.Create(TLabel).WithCaption('Buscar por cidade').WithWidth(460))
          .Add(TControlBuilder.Create(TEdit).WithWidth(480).Setup(@SetupEditSearch))
        .SuperLevel
        .Add(TControlBuilder.Create(TDBGrid).WithAlign(alClient).Setup(@SetupDBGrid))
    finally
      Creators.Free;
    end;
  end;

begin
  ControlDialog := TControlDialog.CreateNew(
    Self, '',
    TControlBuilder.Create(TPanel, 'PanelMain').WithWidthAndHeight(500, 300).WithCaption(''),
    'ContextKey-Exemplo-Panel'
  );

  ConfigPanelMain; // Adiciona outros controles no Panel

  try
    if ControlDialog.ShowModal = mrOk then
    begin
      MDS := ControlDialog.ControlCreator.Registry.GetComponent('MDS') as TMemDataSet;
      if not MDS.Eof then
        ShowMessage('Selecionou: ' + MDS.FieldByName('Cidade').AsString);
    end;
  finally
    ControlDialog.Free;
  end;
end;

end.

