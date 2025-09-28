unit OPCB.Builders;

interface

uses
  OPCB,
  {$IFDEF FPC}Controls, ExtCtrls, Menus, StdCtrls,
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

  TButtonClass = class of TButton;
  TButtonBuilder = class;
  TButtonBuilder = class({$IFDEF FPC}specialize{$ENDIF} TControlBuilderBase<TButtonBuilder>)
  private
    FModalResult: Integer;
    FEnabled: TOptionalBoolean;
  public
    constructor Create(const AName: string=''); overload;
    constructor Create(AClass: TButtonClass; const AName: string=''); overload;
    constructor Create(AClass: TButtonClass; const AName: string; out Reference); overload;
    constructor Create(AClass: TButtonClass; out Reference); overload;
    function WithModalResult(AModalResult: Integer): TButtonBuilder;
    function WithEnabled(AEnabled: Boolean): TButtonBuilder;
    function Build(AOwner: TComponent; AParent: TWinControl;
      const AControlName: string): TControl; override;
  end;

implementation

{ TButtonBuilder }

constructor TButtonBuilder.Create(AClass: TButtonClass; const AName: string);
begin
  inherited Create(TButton, AName);
end;

constructor TButtonBuilder.Create(AClass: TButtonClass; const AName: string;
  out Reference);
begin
  inherited Create(TButton, AName, Reference);
end;

constructor TButtonBuilder.Create(AClass: TButtonClass; out Reference);
begin
  inherited Create(AClass, Reference);
end;

constructor TButtonBuilder.Create(const AName: string);
begin
  inherited Create(TButton, AName);
end;

function TButtonBuilder.WithEnabled(AEnabled: Boolean): TButtonBuilder;
begin
  Result := Self;
  FEnabled := AEnabled;
end;

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
  if FEnabled.HasValue then
    (Result as TButton).Enabled := FEnabled.Value;
end;

end.
