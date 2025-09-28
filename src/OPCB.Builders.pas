unit OPCB.Builders;

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
  { TButtonBuilder }

  TButtonBuilder = class;
  TButtonBuilder = class({$IFDEF FPC}specialize{$ENDIF} TControlBuilderBase<TButtonBuilder>)
  private
    FModalResult: Integer;
  public
    function WithModalResult(AModalResult: Integer): TButtonBuilder;
    function Build(AOwner: TComponent; AParent: TWinControl;
      const AControlName: string): TControl; override;
  end;

implementation

{ TButtonBuilder }

function TButtonBuilder.WithModalResult(AModalResult: Integer): TButtonBuilder;
begin
  Result := Self;
  FModalResult := AModalResult;
end;

function TButtonBuilder.Build(AOwner: TComponent; AParent: TWinControl;
  const AControlName: string): TControl;
begin
  Result := inherited Build(AOwner, AParent, AControlName);
  (Result as TButton).ModalResult := FModalResult;
end;

end.
