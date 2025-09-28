unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, OPCB, UDialogs, FMX.Layouts, FMX.ListBox, FMX.Edit,
  System.Rtti, FMX.Grid.Style, FMX.ScrollBox, FMX.Grid;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    SpeedButton1: TSpeedButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    procedure SetupListBox(AControl: TControl);
    procedure SetupEdit(AControl: TControl);
    procedure SetupStringGrid(AControl: TControl);
    procedure SetupTrackBar(AControl: TControl);
    procedure ButtonMoveToLeftClick(ASender: TObject);
    procedure ButtonMoveToRightClick(ASender: TObject);
    procedure SetupListBoxLeft(AControl: TControl);
    procedure SetupListBoxRight(AControl: TControl);
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.SetupEdit(AControl: TControl);
var
  Edit: TEdit;
begin
  Edit := (AControl as TEdit);
  Edit.Password := True;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  ControlDialog: TControlDialog;
  Edit: TEdit;
begin
  ControlDialog := TControlDialog.CreateNew(
    Self,
    'Informe a senha',
    TControlInfo.Create(TEdit, 'EditSenha').WithWidth(250).Setup(SetupEdit)
  );

  ControlDialog.ShowModal(
    procedure(ModalResult: TModalResult)
    begin
      if ModalResult <> mrOk then
        Exit;

      Edit := ControlDialog.ControlBuilder.GetControl<TEdit>('EditSenha');
      ShowMessage('Informou ' + Edit.Text);
    end
  );
end;

procedure TForm1.SetupStringGrid(AControl: TControl);
var
  Grid: TStringGrid;
  Col: TStringColumn;
begin
  Grid := AControl as TStringGrid;

  Grid.BeginUpdate;
  try
    Grid.ClearColumns;

    Col := TStringColumn.Create(Grid);
    Col.Header := 'ID';
    Col.Width  := 60;
    Col.Parent := Grid;

    Col := TStringColumn.Create(Grid);
    Col.Header := 'Nome';
    Col.Width  := 150;
    Col.Parent := Grid;

    Col := TStringColumn.Create(Grid);
    Col.Header := 'Idade';
    Col.Width  := 80;
    Col.Parent := Grid;

    Grid.RowCount := 5;

    Grid.Cells[0, 0] := '1';
    Grid.Cells[1, 0] := 'Maria';
    Grid.Cells[2, 0] := '23';

    Grid.Cells[0, 1] := '2';
    Grid.Cells[1, 1] := 'João';
    Grid.Cells[2, 1] := '35';

    Grid.Cells[0, 2] := '3';
    Grid.Cells[1, 2] := 'Pedro';
    Grid.Cells[2, 2] := '29';

    Grid.Cells[0, 3] := '4';
    Grid.Cells[1, 3] := 'Ana';
    Grid.Cells[2, 3] := '41';

    Grid.Cells[0, 4] := '5';
    Grid.Cells[1, 4] := 'Lucas';
    Grid.Cells[2, 4] := '18';
  finally
    Grid.EndUpdate;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  ControlDialog: TControlDialog;
  StringGrid: TStringGrid;
  ACol, ARow: Integer;
  Valor: string;
begin
  ControlDialog := TControlDialog.CreateNew(
    Self,
    'Selecione uma data',
    TControlInfo.Create(TStringGrid, 'StringGrid').WithWidthAndHeight(350, 400).Setup(SetupStringGrid)
  );

  ControlDialog.ShowModal(
    procedure(ModalResult: TModalResult)
    begin
      if ModalResult <> mrOk then
        Exit;

      StringGrid := ControlDialog.ControlBuilder.GetControl<TStringGrid>('StringGrid');

      ACol := StringGrid.Col;
      ARow := StringGrid.Row;
      Valor := StringGrid.Cells[ACol, ARow];

      ShowMessage(Format('Coluna: %d | Linha: %d | Valor: %s', [ACol, ARow, Valor]));
    end
  );
end;

procedure TForm1.SetupTrackBar(AControl: TControl);
var
  TrackBar: TTrackBar;
begin
  TrackBar := (AControl as TTrackBar);
  TrackBar.Min := 0;
  TrackBar.Max := 100;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  ControlDialog: TControlDialog;
  TrackBar: TTrackBar;
begin
  ControlDialog := TControlDialog.CreateNew(
    Self,
    'Selecione a posição',
    TControlInfo.Create(TTrackBar, 'TrackBar').WithWidth(300).Setup(SetupTrackBar)
  );

  ControlDialog.ShowModal(
    procedure(ModalResult: TModalResult)
    begin
      if ModalResult <> mrOk then
        Exit;

      TrackBar := ControlDialog.ControlBuilder.GetControl<TTrackBar>('TrackBar');
      ShowMessage('Selecionou a posição' + TrackBar.Value.ToString);
    end
  );
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
  Item: TListBoxItem;
  ListBoxRight, ListBoxLeft: TListBox;
begin
  ListBoxRight := TComponentRegistry.GetComponentFromContext<TListBox>('ContextKey-Exemplo-Panel', 'ListBoxRight');
  ListBoxLeft  := TComponentRegistry.GetComponentFromContext<TListBox>('ContextKey-Exemplo-Panel', 'ListBoxLeft');

  for I := ListBoxRight.Count - 1 downto 0 do
  begin
    Item := ListBoxRight.ListItems[I];
    if Item.IsSelected then
      Item.Parent := ListBoxLeft;
  end;
end;

procedure TForm1.ButtonMoveToRightClick(ASender: TObject);
var
  I: Integer;
  Item: TListBoxItem;
  ListBoxRight, ListBoxLeft: TListBox;
begin
  ListBoxRight := TComponentRegistry.GetComponentFromContext<TListBox>('ContextKey-Exemplo-Panel', 'ListBoxRight');
  ListBoxLeft := TComponentRegistry.GetComponentFromContext<TListBox>('ContextKey-Exemplo-Panel', 'ListBoxLeft');

  for I := ListBoxLeft.Items.Count - 1 downto 0 do
  begin
    Item := ListBoxLeft.ListItems[I];
    if Item.IsSelected then
      Item.Parent := ListBoxRight;
  end;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  ControlDialog: TControlDialog;

  procedure ConfigPanelMain;
  var
    ControlBuilder: TControlsBuilder;
  begin
    ControlBuilder := TControlsBuilder.Create(ControlDialog.ControlBuilder.Registry.ContextKey); // usa o mesmo context do dialog
    try
      ControlBuilder
        .WithOwnerAndParent(
          ControlDialog,
          ControlDialog.ControlBuilder.GetControl<TPanel>('PanelMain')
        )
        .SetTopLeft(10, 10)
        .SetSpace(20, 20)
        .AddControl(TControlInfo.Create(TListBox, 'ListBoxLeft').WithWidthAndHeight(80, 280).Setup(SetupListBoxLeft))
        .SubLevel(cpdVertical)
          .AddControl(TControlInfo.Create(TButton, 'ButtonMoveToRight').WithCaption('>').WithOnClick(ButtonMoveToRightClick))
          .AddControl(TControlInfo.Create(TButton, 'ButtonMoveToLeft').WithCaption('<').WithOnClick(ButtonMoveToLeftClick))
        .SuperLevel
        .AddControl(TControlInfo.Create(TListBox, 'ListBoxRight').WithWidthAndHeight(80, 280).Setup(SetupListBoxRight))
        .CenterControlsInParentVertically(['ButtonMoveToRight', 'ButtonMoveToLeft'])
    finally
      ControlBuilder.Free;
    end;
  end;

begin
  ControlDialog := TControlDialog.CreateNew(
    Self,
    'Mova os itens',
    TControlInfo.Create(TPanel, 'PanelMain').WithWidthAndHeight(300, 300).WithCaption(''),
    'ContextKey-Exemplo-Panel'
  );

  ConfigPanelMain; // Adiciona outros controles no Panel

  ControlDialog.ShowModal(
    procedure(ModalResult: TModalResult)
    begin
      if ModalResult <> mrOk then
        Exit;
      ShowMessage(
        'Left: ' + #13 + ControlDialog.ControlBuilder.GetControl<TListBox>('ListBoxLeft').Items.Text + #13 +
        'Right: ' + #13 + ControlDialog.ControlBuilder.GetControl<TListBox>('ListBoxRight').Items.Text
      );
    end
  );
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
    TControlInfo.Create(TListBox, 'ListBoxCidades').WithWidthAndHeight(350, 400).Setup(SetupListBox)
  );

  ControlDialog.ShowModal(
    procedure(ModalResult: TModalResult)
    begin
      if ModalResult <> mrOk then
        Exit;

      ListBoxCidades := ControlDialog.ControlBuilder.GetControl('ListBoxCidades') as TListBox;
      if ListBoxCidades.ItemIndex >= 0 then
        ShowMessage('Selecionou ' + ListBoxCidades.Items[ListBoxCidades.ItemIndex]);
    end
  );
end;

end.
