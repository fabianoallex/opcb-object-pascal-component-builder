unit OPCB.Controls;

interface

uses
  OPCB,
  {$IFDEF FPC}Controls, ExtCtrls, Menus,
  {$ELSE}
    {$IFDEF FRAMEWORK_FMX}
    FMX.Controls, FMX.StdCtrls, Fmx.Types, FMX.ExtCtrls, FMX.TabControl, FMX.Forms, FMX.Menus, System.Types,
    {$ELSE}
    Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Menus, Types,
    {$ENDIF}
  {$ENDIF}
  Classes, SysUtils, ULayout, Generics.Collections, Generics.Defaults, OPCB.Optionals;

type
  { TButtonInfo }

  TButtonBuilder = class;
  TButtonInfo = class({$IFDEF FPC}specialize{$ENDIF} TControlBuilderBase<TButtonInfo>)
  private
    FModalResult: Integer;
  public
    function WithModalResult(AModalResult: Integer): TButtonInfo;
    function CreateControl(AOwner: TComponent; AParent: TWinControl;
      const AControlName: string): TControl; override;
  end;

implementation

{ TButtonInfo }

function TButtonInfo.WithModalResult(AModalResult: Integer): TButtonInfo;
begin
  Result := Self;
  FModalResult := AModalResult;
end;

function TButtonInfo.CreateControl(AOwner: TComponent; AParent: TWinControl;
  const AControlName: string): TControl;
begin
  Result := inherited CreateControl(AOwner, AParent, AControlName);
  (Result as TButton).ModalResult := FModalResult;
end;

end.
