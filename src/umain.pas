unit UMain;

{$apptype console}
{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, Controls, Dialogs, StdCtrls, ExtCtrls, Buttons,
  UVirtualKeyboardClasses, Classes, ULayout;

type

  { TForm1 }

  { TFocusSpeedButton }


  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BeforePlaceItem(Sender: TObject; AItem: ILayoutItem; var APosition: IGridPosition; var Accept: Boolean);
  private
    procedure CreateButtonGrid;
    procedure CreateVirtualKeyBlock;
    procedure CreateLayout;
    procedure CreateLayout2;
    procedure teste;
  public

  end;

var
  Form1: TForm1;

implementation

uses
  UVirtualKeyBlockLayouts;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  CreateVirtualKeyBlock;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  CreateLayout;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  CreateLayout2;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  CreateButtonGrid;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  teste;
end;


procedure TForm1.teste;
var
  FGrid: TGridLayout;
  SB: TSpeedButton;
  Settings: TGridCellSettings;
begin
  FGrid := TGridLayout.Create;
  SB := TSpeedButton.Create(Panel1);
  SB.Parent := Panel1;

  try
    // Definições básicas
    FGrid.Rows := 3;
    FGrid.Columns := 3;
    FGrid.RowHeight[0] := 20;
    FGrid.RowHeight[1] := 25;
    FGrid.RowHeight[2] := 30;

    FGrid.ColumnWidth[0] := 10;
    FGrid.ColumnWidth[1] := 15;
    FGrid.ColumnWidth[2] := 20;

    // Espaçamentos customizados
    FGrid.VerticalSpacing[0] := 13;
    FGrid.VerticalSpacing[1] := 15;

    FGrid.HorizontalSpacing[0] := 2;
    FGrid.HorizontalSpacing[1] := 4;

    // Item com RowSpan + ColumnSpan
    Settings :=
      TGridCellSettings.Create(0, 0)
        .WithRowSpan(3)
        .WithColumnSpan(3);

    FGrid.AddItem(TControlLayoutItem.Create(SB), Settings);


    {SB := TSpeedButton.Create(Panel1);
    SB.Parent := Panel1;
    Settings := TGridCellSettings.Create(3, 0);

    FGrid.AddItem(TControlLayoutItem.Create(SB), Settings);
    }



    FGrid.ArrangeItems;

    Panel1.Height := FGrid.ContentHeight;
    Panel1.Width := FGrid.ContentWidth;

  finally
    FGrid.Free;
  end;
end;

procedure TForm1.BeforePlaceItem(Sender: TObject; AItem: ILayoutItem; var APosition: IGridPosition; var Accept: Boolean);
begin
  //tratar evento
end;

procedure TForm1.CreateButtonGrid;
var
  GridLayout: TGridLayout;
  Btn: TButton;
  i: Integer;
  GridFill: IGridFill;
begin
  GridLayout := TGridLayout.Create;

  try
    GridLayout.Columns := 3;
    GridLayout.Rows := 3;
    GridLayout.DefaultHorizontalSpacing := 0;
    GridLayout.DefaultVerticalSpacing := 0;
    GridLayout.DefaultColumnWidth := 80;
    GridLayout.DefaultRowHeight := 40;

    GridLayout.RowHeight[1] := 80;
    GridLayout.ColumnWidth[1] := 50;

    GridLayout.VerticalSpacing[0] := 50;
    GridLayout.HorizontalSpacing[0] := 50;

    //GridLayout.RowShift[0] := 5;
    GridLayout.ColumnShift[0] := 5;
    GridLayout.ColumnShift[2] := -5;
    GridLayout.Margins.All := 7;
    GridLayout.Margins.Left := 50;

    GridFill := TGridFillRowFirst.Create(GridLayout);
    GridFill.InitialPos(TGridPosition.Create(0, 0));
    GridFill.OnBeforePlaceItem := @BeforePlaceItem;

    // Adiciona os botões
    for i := 1 to 9 do
    begin
      Btn := TButton.Create(Panel1);
      Btn.Parent := Panel1;
      Btn.Caption := 'Botão ' + IntToStr(i);
      GridFill.PlaceItem(TControlLayoutItem.Create(Btn));
    end;

    Panel1.Height := GridLayout.ContentHeight;
    Panel1.Width := GridLayout.ContentWidth;

    GridLayout.ArrangeItems;
  finally
    GridLayout.Free;
  end;
end;

procedure TForm1.CreateVirtualKeyBlock;
var
  Block: TVirtualKeyBlock;
  Layout: TVirtualKeyBlockGridLayout;
  Key: TVirtualKey;
  I: Integer;
const
  COLS = 4;
  ROWS = 5;
begin
  Layout := TVirtualKeyBlockGridLayout.Create(ROWS, COLS, 15, 20);

  Layout.DefaultRowHeight := 50;
  Layout.DefaultColumnWidth := 50;

  Layout.RowHeight[1] := 150;
  Layout.ColumnWidth[1] := 150;

  Block := TVirtualKeyBlock.Create(Self, 'Bloco', Layout);
  Block.Parent := Self;
  Block.Left := 10;
  Block.Top := 10;
  Block.MarginLeft := 50;
  Block.MarginTop := 50;
  Block.MarginBottom := 50;
  Block.MarginRight := 50;

  Key := TVirtualKey.Create(Block, 'KEY_' + Chr(Ord('A')));
  Layout.SetCell(0, 0, Key, 2, 1);

  Key := TVirtualKey.Create(Block, 'KEY_' + Chr(Ord('B')));
  Layout.SetCell(0, 2, Key, 1, 1);

  Key := TVirtualKey.Create(Block, 'KEY_' + Chr(Ord('C')));
  Layout.SetCell(0, 3, Key, 1, 2);

  Key := TVirtualKey.Create(Block, 'KEY_' + Chr(Ord('D')));
  Layout.SetCell(1, 0, Key, 1, 1);

  Key := TVirtualKey.Create(Block, 'KEY_' + Chr(Ord('E')));
  Layout.SetCell(1, 1, Key, 1, 1);

  Key := TVirtualKey.Create(Block, 'KEY_' + Chr(Ord('F')));
  Layout.SetCell(1, 2, Key, 1, 1);

  Key := TVirtualKey.Create(Block, 'KEY_' + Chr(Ord('G')));
  Layout.SetCell(2, 2, Key, 1, 1);

  Block.ArrangeKeys;
end;

procedure TForm1.CreateLayout;
var
  Layout: TGridLayout;
begin
  Layout := TGridLayout.Create;

  try
    Layout.Rows := 3;
    Layout.Columns := 3;
    Layout.DefaultRowHeight := 60;
    Layout.DefaultColumnWidth := 80;
    Layout.DefaultHorizontalSpacing := 1;
    Layout.DefaultVerticalSpacing := 20;

    Layout.RowHeight[1] := 25;

    Layout.AddItem(
      TControlLayoutItem.Create(BitBtn1),
      TGridCellSettings.Create(0, 0)
    );

    Layout.AddItem(
      TControlLayoutItem.Create(BitBtn2),
      TGridCellSettings.Create(0, 1)
        .WithColumnSpan(2)
    );

    Layout.AddItem(
      TControlLayoutItem.Create(BitBtn3),
      TGridCellSettings.Create(1, 0)
        .WithRowSpan(2)
    );

    Layout.AddItem(
      TControlLayoutItem.Create(BitBtn4),
      TGridCellSettings.Create(1, 1)
    );

    Layout.AddItem(
      TControlLayoutItem.Create(BitBtn5),
      TGridCellSettings.Create(2, 2)
    );

    // Organizar
    Layout.ArrangeItems;
  finally
    Layout.Free;
  end;
end;

procedure TForm1.CreateLayout2;
var
  Layout: TGridLayout;
begin
  Layout := TGridLayout.Create;

  try
    Layout.Rows := 3;
    Layout.Columns := 3;
    Layout.DefaultRowHeight := Edit1.Height * 5;
    Layout.DefaultColumnWidth := Edit2.Width;
    Layout.DefaultHorizontalSpacing := 0;
    Layout.DefaultVerticalSpacing := 8;

    Layout.ColumnWidth[2] := 55;
    Layout.HorizontalSpacing[0] := 10;

    Layout.AddItem(
      TControlLayoutItem.Create(Edit1),
      TGridCellSettings.Create(0, 0)
        .WithVerticalAlignment(laCenter)
    );

    Layout.AddItem(
      TControlLayoutItem.Create(BitBtn2),
      TGridCellSettings.Create(0, 1)
        .WithColumnSpan(2)
        .WithHorizontalAlignment(laCenter)
    );

    Layout.AddItem(
      TControlLayoutItem.Create(Edit3),
      TGridCellSettings.Create(1, 0)
        .WithRowSpan(2)
    );

    Layout.AddItem(
      TControlLayoutItem.Create(Edit4),
      TGridCellSettings.Create(1, 1)
    );

    Layout.AddItem(
      TControlLayoutItem.Create(Edit5),
      TGridCellSettings.Create(2, 2)
    );

    Layout.ArrangeItems;
  finally
    Layout.Free;
  end;
end;

end.

