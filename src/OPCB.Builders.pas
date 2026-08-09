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
  Classes, SysUtils, Generics.Collections, Generics.Defaults, OPCB.Optionals;

type
  { TButtonBuilder }

  TButtonClass = class of TButton;
  TButtonBuilder = class;
  TButtonBuilder = class({$IFDEF FPC}specialize{$ENDIF} TControlBuilderBase<TButton, TButtonBuilder>)
  private
    FModalResult: TOptionalInteger;
    FEnabled: TOptionalBoolean;
  protected
    function CreateObject: TButton; override;
    procedure ConfigureObject(AObject: TButton); override;
  public
    constructor Create(const AName: string=''); overload;
    constructor Create(AClass: TButtonClass; const AName: string=''); overload;
    constructor Create(AClass: TButtonClass; const AName: string; out Reference); overload;
    constructor Create(AClass: TButtonClass; out Reference); overload;
    function WithModalResult(AModalResult: Integer): TButtonBuilder;
    function WithEnabled(AEnabled: Boolean): TButtonBuilder;
  end;

implementation

{ TButtonBuilder }

constructor TButtonBuilder.Create(AClass: TButtonClass; const AName: string);
begin
  inherited Create(AClass, AName);
end;

constructor TButtonBuilder.Create(AClass: TButtonClass; const AName: string;
  out Reference);
begin
  inherited Create(AClass, AName, Reference);
end;

constructor TButtonBuilder.Create(AClass: TButtonClass; out Reference);
begin
  inherited Create(AClass, Reference);
end;

function TButtonBuilder.CreateObject: TButton;
begin
  Result := TButtonClass(FObjectClass).Create(Owner);
end;

procedure TButtonBuilder.ConfigureObject(AObject: TButton);
begin
  inherited ConfigureObject(AObject);
  if FModalResult.HasValue then
    AObject.ModalResult := FModalResult.Value;
  if FEnabled.HasValue then
    AObject.Enabled := FEnabled.Value;
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

end.
