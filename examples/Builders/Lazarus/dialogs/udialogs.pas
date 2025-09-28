unit UDialogs;

{$mode ObjFPC}{$H+}

interface

uses
  OPCB, Forms,
  {$IFDEF FPC}
  Classes, StdCtrls, ExtCtrls, ComCtrls, Buttons, Controls
  {$ELSE}
  System.Classes, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.Buttons, Vcl.Controls
  {$ENDIF}
  ;

type
  TControlDialog = class(TForm)
  private
    FControlsBuilder: TControlsBuilder;
    FControlInfo: TControlInfo;
    procedure SeTControlsBuilder(const Value: TControlsBuilder);
    procedure SetupButton(AControl: TControl);
  public
    constructor CreateNew(AOwner: TComponent; AMsg: string; AControlInfo: TControlInfo; AContextKey: string='');
    destructor Destroy; override;
    property ControlsBuilder: TControlsBuilder read FControlsBuilder write SeTControlsBuilder;
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

  ControlsBuilder := TControlsBuilder.Create(AContextKey);

  if AControlInfo.Name = '' then
    AControlInfo.WithName('Control');

  ControlName := AControlInfo.Name;

  ControlsBuilder
    .WithOwnerAndParent(Self, Self)
    .SetSpace(5, 5)
    .SetTopLeft(20, 20)
    .SubLevel(cpdVertical);

      if AMsg <> '' then
        ControlsBuilder.AddControl(TControlInfo.Create(TLabel, 'LabelMessage').WithCaption(AMsg));

   ControlsBuilder
      .AddControl(AControlInfo)
    .SuperLevel
    .Break
    .IncTop(10)
    .AddControl(TControlInfo.Create(TBitBtn, 'ButtonOk').WithCaption('Ok').WithHeight(30).Setup(@SetupButton))
    .AddControl(TControlInfo.Create(TBitBtn, 'ButtonCancel').WithCaption('Cancelar').WithHeight(30).Setup(@SetupButton))
    .AlignControlsRight(['ButtonOk', 'ButtonCancel'], [ControlName])
    .RecalcParentSize(10, 10)
  ;

  Self.Width := Trunc(ControlsBuilder.ContentWidth) + 40;
  Self.Height := Trunc(ControlsBuilder.ContentHeight) + 40;
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
  FControlsBuilder.Free;
  inherited;
end;

procedure TControlDialog.SeTControlsBuilder(const Value: TControlsBuilder);
begin
  FControlsBuilder := Value;
end;

end.
