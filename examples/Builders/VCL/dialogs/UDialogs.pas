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
    FControlCreator: TControlCreator;
    FControlInfo: TControlBuilder;
    procedure SeTControlCreator(const Value: TControlCreator);
    procedure SetupButton(AControl: TControl);
  public
    constructor CreateNew(AOwner: TComponent; AMsg: string; AControlBuilder: TControlBuilder; AContextKey: string='');
    destructor Destroy; override;
    property ControlCreator: TControlCreator read FControlCreator write SeTControlCreator;
  end;

implementation

{ TControlDialog }

constructor TControlDialog.CreateNew(AOwner: TComponent; AMsg: string; AControlBuilder: TControlBuilder; AContextKey: string='');
var
  ControlName: string;
begin
  inherited CreateNew(AOwner, 0);

  Self.BorderStyle := bsDialog;
  Self.Caption := 'ControlDialog Example';
  Self.Position := poMainFormCenter;

  if AContextKey = '' then
    AContextKey := 'DIALOGS';

  ControlCreator := TControlCreator.Create(AContextKey);

  if AControlBuilder.Name = '' then
    AControlBuilder.WithName('Control');

  ControlName := AControlBuilder.Name;

  ControlCreator
    .WithOwnerAndParent(Self, Self)
    .SetSpace(5, 5)
    .SetTopLeft(20, 20)
    .SubLevel(cpdVertical)
      .Add(TControlBuilder.Create(TLabel, 'LabelMessage').WithCaption(AMsg))
      .Add(AControlBuilder)
    .SuperLevel
    .Break
    .IncTop(10)
    .Add(TControlBuilder.Create(TBitBtn, 'ButtonOk').WithCaption('Ok').WithHeight(30).Setup(SetupButton))
    .Add(TControlBuilder.Create(TBitBtn, 'ButtonCancel').WithCaption('Cancelar').WithHeight(30).Setup(SetupButton))
    .AlignControlsRight(['ButtonOk', 'ButtonCancel'], [ControlName])
  ;

  Self.Width := Trunc(ControlCreator.ContentWidth) + 50;
  Self.Height := Trunc(ControlCreator.ContentHeight) + 75;
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
  FControlCreator.Free;
  inherited;
end;

procedure TControlDialog.SeTControlCreator(const Value: TControlCreator);
begin
  FControlCreator := Value;
end;

end.
