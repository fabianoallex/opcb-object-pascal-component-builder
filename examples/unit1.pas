unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  ulayout.controls, ULayout, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  DBCtrls, Grids, ColorBox, ExtCtrls, Buttons, ListViewFilterEdit,
  SynHighlighterPas, TAGraph;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure ButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure OnControlCreate(AControl: TControl; AIndex: Integer;
      ASettings: TGridCellSettings);
    procedure OnControlCreate2(AControl: TControl; AIndex: Integer;
      ASettings: TGridCellSettings);
    procedure OnControlCreateForm(AControl: TControl; AIndex: Integer;
      ASettings: TGridCellSettings);
  public
  end;

var
  Form1: TForm1;

implementation

uses
  UGridLayoutBuilder, UGridLayoutFillerFactory;

{$R *.lfm}

{ TForm1 }

procedure TForm1.OnControlCreate(AControl: TControl; AIndex: Integer;
  ASettings: TGridCellSettings);
var
  L: TLabel;
begin
  if AControl is TLabel then
  begin
    ASettings.WithAlignment(laEnd, laCenter);
    L := (AControl as TLabel);
    L.Caption := 'Teste ' + AIndex.ToString;
    L.AutoSize := True;

    if AIndex = 0 then
      ASettings.WithColumnSpan(2);
  end;

  if AControl is TEdit then
  begin
    ASettings.WithAlignment(laStretch, laCenter);
  end;

  if AControl is TButton then
    AControl.OnClick := @ButtonClick;

  if AControl is TListViewFilterEdit then
    ASettings.WithAlignment(laStretch, laCenter);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  Grid: TGridLayout;
  ControlGridPopulator: TControlGridPopulator;
begin
  Grid := TGridLayoutBuilder.Create
    .WithDimensions(5, 5)
    .WithRowAndColSizes(80, 40)
    .WithMargins(10)
    .WithSpacings(6, 6)
    .WithRowsHeight([0, 1], 35)
    .WithColumnsWidth([0], 100)
    .WithColumnsWidth([1], 100)
    .Build;

  ControlGridPopulator := TControlGridPopulator.Create(Grid);

  try
    ControlGridPopulator
      .WithOwnerAndParentControl(Self, Self)
      .UsingFiller(ftColumnFirst)
      .CreateControls(5, [TLabel], @OnControlCreate)
      .UsingFiller(ftRowFirst)
      .CreateControls(19, [TMemo, TButton, TCheckBox, TEdit], @OnControlCreate)
    ;
  finally
    ControlGridPopulator.Free;
  end;

  Grid.ArrangeItems;
end;

procedure TForm1.OnControlCreate2(AControl: TControl; AIndex: Integer;
  ASettings: TGridCellSettings);
begin
  if (AControl is TLabel) then
    ASettings.WithAlignment(laStart, laCenter);

  if AIndex in [0, 1, 2, 3] then
    ASettings.WithColumnSpan(3);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  Grid: TGridLayout;
  Populator: TControlGridPopulator;

  procedure ConfigControls;
  begin
    Populator.NamedControls['LabelEdit'].Caption := 'LabelEdit';
    Populator.NamedControls['LabelMemo'].Caption := 'LabelMemo';

    (Populator.NamedControls['BtnOk'] as TBitBtn).Kind := bkOk;
    (Populator.NamedControls['BtnCancel'] as TBitBtn).Kind := bkCancel;
  end;

begin
  Grid := TGridLayoutBuilder.Create
    .WithDimensions(7, 3)
    .WithRowAndColSizes(35, 150)
    .WithMargins(10)
    .WithSpacings(6, 6)
    .WithRowsHeight([0, 2], 17)
    .WithRowsHeight([3], 200)
    .Build;

  Populator := TControlGridPopulator.Create(Grid);

  try
    Populator
      .WithOwnerAndParentControl(Self, Self)
      .UsingFiller(ftRowFirst)
      .CreateControls(4, [
          TControlInfo.Create(TLabel, 'LabelEdit'),
          TControlInfo.Create(TEdit, 'Edit'),
          TControlInfo.Create(TLabel, 'LabelMemo'),
          TControlInfo.Create(TMemo, 'Memo')
        ],
        @OnControlCreate2
      )
      .FillerSetPosition(4, 1)
      .CreateControls(2, [
        TControlInfo.Create(TBitBtn, 'BtnOk'),
        TControlInfo.Create(TBitBtn, 'BtnCancel')
      ])
    ;

    ConfigControls;

    Grid.ArrangeItems;
  finally
    Populator.Free;
    Grid.Free;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  Grid: TGridLayout;
  Populator: TControlGridPopulator;
begin
  Grid := TGridLayoutBuilder.Create
    .WithDimensions(7, 7)
    .WithRowAndColSizes(45, 45)
    .WithMargins(10)
    .WithSpacings(3, 3)
    .Build;

  Populator := TControlGridPopulator.Create(Grid);

  try
    Populator
      .WithOwnerAndParentControl(Self, Self)
      .UsingFiller(ftColumnFirst, 5, 5)
      .CreateControls(2, [TControlInfo.Create(TLabel, 'Label')])
      .FillerSetPosition(0, 0)
      .CreateControls(7, [TControlInfo.Create(TLabel, 'Label')])
      .UsingFiller(ftRowFirst)
      .CreateControls(40, [TControlInfo.Create(TSpeedButton, 'SB')])
    ;

    Populator.NamedControls['SB'].Caption := '01';

    Grid.ArrangeItems;
  finally
    Populator.Free;
    Grid.Free;
  end;
end;

procedure TForm1.OnControlCreateForm(AControl: TControl; AIndex: Integer;
  ASettings: TGridCellSettings);
begin
  if AIndex in [0, 1] then
    ASettings.WithColumnSpan(2);
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  Form: TForm;
  Grid: TGridLayout;
  Populator: TControlGridPopulator;
begin
  Grid := nil;
  Populator := nil;

  Form := TForm.Create(Application);
  Form.Position := poMainFormCenter;

  try
    TGridLayoutBuilder.Create
      .WithDimensions(3, 2)
      .WithRowAndColSizes(25, 180)
      .WithMargins(15)
      .WithSpacings(8, 8)
      .WithRowsHeight([1], 185)
      .WithRowsHeight([2], 35)
      .BuildAndPopulate(Grid, Populator)
      .WithOwnerAndParentControl(Form, Form)
      .UsingFiller(ftRowFirst)
      .CreateControls(4, [
          TControlInfo.Create(TLabel, 'Label'),
          TControlInfo.Create(TListBox, 'ListBox'),
          TControlInfo.Create(TBitBtn, 'BtnOk'),
          TControlInfo.Create(TBitBtn, 'BtnCancel')
        ], @OnControlCreateForm
      );

    Grid.RowHeight[0] := (Populator.NamedControls['Label'] as TLabel).Height;
    (Populator.NamedControls['Label'] as TLabel).Caption := 'Escolha o item';
    (Populator.NamedControls['ListBox'] as TListBox).Items.AddCommaText('Item1,Item2,Item3');
    (Populator.NamedControls['BtnOk'] as TBitBtn).Kind := bkOK;
    (Populator.NamedControls['BtnCancel'] as TBitBtn).Kind := bkCancel;

    Grid.ArrangeItems;

    Form.Height := Grid.ContentHeight;
    Form.Width := Grid.ContentWidth;

    Form.ShowModal;
  finally
    if Assigned(Form) then
      Form.Free;
    if Assigned(Grid) then
      Grid.Free;
    if Assigned(Populator) then
      Populator.Free;
  end;
end;

procedure TForm1.ButtonClick(Sender: TObject);
begin
  showmessage('click');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

end.

