unit UDialogs;


interface

uses
  OPCB, FMX.Forms, FMX.Controls, System.Classes, System.UITypes;

type
  TControlDialog = class(TForm)
  private
    FControlBuilder: TControlsBuilder;
    FControlInfo: TControlInfo;
    procedure SeTControlsBuilder(const Value: TControlsBuilder);
    procedure SetupButton(AControl: TControl);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SetupLabel(AControl: TControl);
  public
    constructor CreateNew(AOwner: TComponent; AMsg: string; AControlInfo: TControlInfo; AContextKey: string='');
    destructor Destroy; override;
    property ControlBuilder: TControlsBuilder read FControlBuilder write SeTControlsBuilder;
  end;

implementation

uses
  FMX.StdCtrls, FMX.Layouts;

{ TControlDialog }

constructor TControlDialog.CreateNew(AOwner: TComponent; AMsg: string; AControlInfo: TControlInfo; AContextKey: string='');
var
  ControlName: string;
begin
  inherited CreateNew(AOwner, 0);

  Self.BorderStyle := TFmxFormBorderStyle.Single;
  Self.Caption := 'ControlDialog Example';
  Self.Position := TFormPosition.MainFormCenter;
  Self.OnClose := FormClose;

  if AContextKey = '' then
    AContextKey := 'DIALOGS';

  ControlBuilder := TControlsBuilder.Create(AContextKey);

  if AControlInfo.Name = '' then
    AControlInfo.WithName('Control');

  ControlName := AControlInfo.Name;

  ControlBuilder
    .WithOwnerAndParent(Self, Self)
    .SetSpace(5, 5)
    .SetTopLeft(20, 20)
    .SubLevel(TControlInfo.Create(TLayout))
      .SubLevel(cpdVertical)
        .AddControl(TControlInfo.Create(TLabel, 'LabelMessage').WithCaption(AMsg).Setup(SetupLabel))
        .AddControl(AControlInfo)
      .SuperLevel
      .Break
      .IncTop(10)
      .AddControl(TControlInfo.Create(TButton, 'ButtonOk').WithCaption('Ok').WithHeight(30).Setup(SetupButton))
      .AddControl(TControlInfo.Create(TButton, 'ButtonCancel').WithCaption('Cancelar').WithWidthAndHeight(100, 30).Setup(SetupButton))
      .AlignControlsRight(['ButtonOk', 'ButtonCancel'], [ControlName])
    .SuperLevel
  ;

  Self.Width := Trunc(ControlBuilder.ContentWidth) + 40;
  Self.Height := Trunc(ControlBuilder.ContentHeight) + 75;
end;

procedure TControlDialog.SetupLabel(AControl: TControl);
var
  Lbl: TLabel;
begin
  Lbl := (AControl as TLabel);
  Lbl.AutoSize := True;
  Lbl.Width := 300;
end;

procedure TControlDialog.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction.caFree;
end;

procedure TControlDialog.SetupButton(AControl: TControl);
var
  Button: TButton;
begin
  Button := (AControl as TButton);
  if Button.Name = 'ButtonOk' then
  begin
    Button.ModalResult := mrOk;
    Button.Default := True;
  end;

  if Button.Name = 'ButtonCancel' then
  begin
    Button.ModalResult := mrCancel;
    Button.Cancel := True;
  end;
end;

destructor TControlDialog.Destroy;
begin
  FControlBuilder.Free;
  inherited;
end;

procedure TControlDialog.SeTControlsBuilder(const Value: TControlsBuilder);
begin
  FControlBuilder := Value;
end;

end.
