unit UDialogs;

interface

uses
  OPCB, Forms, System.Classes,
  {$IFDEF FPC}
  StdCtrls, ExtCtrls, ComCtrls, Buttons, Controls
  {$ELSE}
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.Buttons, Vcl.Controls
  {$ENDIF}
  ;

type
  TControlDialog = class(TForm)
  private
    FControlBuilder: TControlsBuilder;
    FControlInfo: TControlInfo;
    procedure SeTControlsBuilder(const Value: TControlsBuilder);
    procedure SetupButton(AControl: TControl);
  public
    constructor CreateNew(AOwner: TComponent; AMsg: string; AControlInfo: TControlInfo; AContextKey: string='');
    destructor Destroy; override;
    property ControlBuilder: TControlsBuilder read FControlBuilder write SeTControlsBuilder;
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

  ControlBuilder := TControlsBuilder.Create(AContextKey);

  if AControlInfo.Name = '' then
    AControlInfo.WithName('Control');

  ControlName := AControlInfo.Name;

  ControlBuilder
    .WithOwnerAndParent(Self, Self)
    .SetSpace(5, 5)
    .SetTopLeft(20, 20)
    .SubLevel(cpdVertical)
      .AddControl(TControlInfo.Create(TLabel, 'LabelMessage').WithCaption(AMsg))
      .AddControl(AControlInfo)
    .SuperLevel
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

procedure TControlDialog.SeTControlsBuilder(const Value: TControlsBuilder);
begin
  FControlBuilder := Value;
end;

end.
