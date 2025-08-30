unit UDialogs;

interface

uses
  OPCB, Forms, System.Classes, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  Vcl.Buttons, Vcl.Controls;

type
  TControlDialog = class(TForm)
  private
    FControlBuilder: TControlBuilder;
    FControlInfo: TControlInfo;
    procedure SetControlBuilder(const Value: TControlBuilder);
    procedure SetupButton(AControl: TControl);
  public
    constructor CreateNew(AOwner: TComponent; AMsg: string; AControlInfo: TControlInfo; AContextKey: string='');
    destructor Destroy; override;
    property ControlBuilder: TControlBuilder read FControlBuilder write SetControlBuilder;
  end;

implementation

{ TControlDialog }

constructor TControlDialog.CreateNew(AOwner: TComponent; AMsg: string; AControlInfo: TControlInfo; AContextKey: string='');
var
  ControlName: string;
begin
  inherited CreateNew(AOwner, 0);

  Self.BorderStyle := bsDialog;
  Self.Caption := 'ControlDialog Example';
  Self.Position := poMainFormCenter;

  if AContextKey = '' then
    AContextKey := 'DIALOGS';

  ControlBuilder := TControlBuilder.Create(AContextKey);

  if AControlInfo.Name.IsEmpty then
    AControlInfo.Name := 'Control';

  ControlName := AControlInfo.Name;

  ControlBuilder
    .WithOwnerAndParent(Self, Self)
    .SetSpace(5, 5)
    .SetTopLeft(20, 20)
    .NextLevel(cpdVertical)
      .AddControl(TControlInfo.Create(TLabel, 'LabelMessage').WithCaption(AMsg))
      .AddControl(AControlInfo)
    .PreviousLevel
    .Break
    .IncTop(10)
    .AddControl(TControlInfo.Create(TBitBtn, 'ButtonOk').WithCaption('Ok').WithHeight(30).Setup(SetupButton))
    .AddControl(TControlInfo.Create(TBitBtn, 'ButtonCancel').WithCaption('Cancelar').WithHeight(30).Setup(SetupButton))
    .AlignControlsRight(['ButtonOk', 'ButtonCancel'], [ControlName])
  ;

  Self.Width := Trunc(ControlBuilder.ContentWidth) + 50;
  Self.Height := Trunc(ControlBuilder.ContentHeight) + 75;
end;

procedure TControlDialog.SetupButton(AControl: TControl);
var
  Button: TBitBtn;
begin
  Button := (AControl as TBitBtn);
  if Button.Name = 'ButtonOk' then
    Button.Kind := bkOK;

  if Button.Name = 'ButtonCancel' then
  begin
    Button.Kind := bkCancel;
    Button.Caption := 'Cancelar';
    Button.Width := 100;
  end;
end;

destructor TControlDialog.Destroy;
begin
  FControlBuilder.Free;
  inherited;
end;

procedure TControlDialog.SetControlBuilder(const Value: TControlBuilder);
begin
  FControlBuilder := Value;
end;

end.
