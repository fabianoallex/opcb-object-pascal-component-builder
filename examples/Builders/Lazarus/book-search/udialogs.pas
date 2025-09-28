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
    FControlBuilder: TControlCreator;
    FControlInfo: TControlBuilder;
    procedure SeTControlCreator(const Value: TControlCreator);
    procedure SetupButton(AControl: TControl);
  public
    constructor CreateNew(AOwner: TComponent; AMsg: string; AControlBuilder: TControlBuilder; AContextKey: string='');
    destructor Destroy; override;
    property ControlBuilder: TControlCreator read FControlBuilder write SeTControlCreator;
  end;

implementation

{ TControlDialog }

constructor TControlDialog.CreateNew(AOwner: TComponent; AMsg: string; AControlBuilder: TControlBuilder; AContextKey: string='');
var
  ControlName: string;
begin
  inherited CreateNew(AOwner, 0);

  Self.BorderStyle := bsDialog;
  Self.Caption := 'Dialog';
  Self.Position := poMainFormCenter;

  if AContextKey = '' then
    AContextKey := 'DIALOGS';

  ControlBuilder := TControlCreator.Create(AContextKey);

  if AControlBuilder.Name = '' then
    AControlBuilder.WithName('Control');

  ControlName := AControlBuilder.Name;

  ControlBuilder
    .WithOwnerAndParent(Self, Self)
    .SetSpace(5, 5)
    .SetTopLeft(20, 20)
    .SubLevel(cpdVertical);

      if AMsg <> '' then
        ControlBuilder.AddControl(TControlBuilder.Create(TLabel, 'LabelMessage').WithCaption(AMsg));

   ControlBuilder
      .AddControl(AControlBuilder)
    .SuperLevel
    .Break
    .IncTop(10)
    .AddControl(TControlBuilder.Create(TBitBtn, 'ButtonOk').WithCaption('Ok').WithHeight(30).Setup(@SetupButton))
    .AddControl(TControlBuilder.Create(TBitBtn, 'ButtonCancel').WithCaption('Cancelar').WithHeight(30).Setup(@SetupButton))
    .AlignControlsRight(['ButtonOk', 'ButtonCancel'], [ControlName])
  ;

  Self.Width := Trunc(ControlBuilder.ContentWidth) + 40;
  Self.Height := Trunc(ControlBuilder.ContentHeight) + 40;
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

procedure TControlDialog.SeTControlCreator(const Value: TControlCreator);
begin
  FControlBuilder := Value;
end;

end.
