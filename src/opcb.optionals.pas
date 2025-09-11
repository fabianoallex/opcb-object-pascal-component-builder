unit OPCB.Optionals;

{$IFDEF FPC}
{$mode ObjFPC}{$H+}
{$MODESWITCH ADVANCEDRECORDS}
{$ENDIF}

interface

uses
  {$IFDEF FPC}Controls, Graphics,
  {$ELSE}
    {$IFDEF FRAMEWORK_FMX}
    FMX.Controls, Fmx.Types, Fmx.Graphics,
    {$ELSE}
    Vcl.Controls, Vcl.Graphics,
    {$ENDIF}
  {$ENDIF}
  Classes, SysUtils;

type
  TOptionalString = record
    HasValue: Boolean;
    Value: string;
    {$IFDEF FPC}
    class operator :=(AValue: string): TOptionalString;
    {$ELSE}
    class operator Implicit(AValue: string): TOptionalString;
    {$ENDIF}
    class function Some(AValue: string): TOptionalString; static;
    class function None: TOptionalString; static;
  end;

  TOptionalAlign = record
    HasValue: Boolean;
    Value: {$IFDEF FRAMEWORK_FMX}TAlignLayout{$ELSE}TAlign{$ENDIF};
    {$IFDEF FPC}
    class operator :=(AValue: TAlign): TOptionalAlign;
    {$ELSE}
    class operator Implicit(
      AValue: {$IFDEF FRAMEWORK_FMX}TAlignLayout{$ELSE}TAlign{$ENDIF}): TOptionalAlign;
    {$ENDIF}
    class function Some(
      AValue: {$IFDEF FRAMEWORK_FMX}TAlignLayout{$ELSE}TAlign{$ENDIF}): TOptionalAlign; static;
    class function None: TOptionalAlign; static;
  end;

  { TOptionalSingle }

  TOptionalSingle = record
    HasValue: Boolean;
    Value: Single;
    {$IFDEF FPC}
    class operator :=(AValue: Single): TOptionalSingle;
    {$ELSE}
    class operator Implicit(AValue: Single): TOptionalSingle;
    {$ENDIF}
    class function Some(AValue: Single): TOptionalSingle; static;
    class function None: TOptionalSingle; static;
  end;

  { TOptionalInteger }

  TOptionalInteger = record
    HasValue: Boolean;
    Value: Integer;
    {$IFDEF FPC}
    class operator :=(AValue: Integer): TOptionalInteger;
    {$ELSE}
    class operator Implicit(AValue: Integer): TOptionalInteger;
    {$ENDIF}
    class function Some(AValue: Integer): TOptionalInteger; static;
    class function None: TOptionalInteger; static;
  end;

  { TOptionalColor }

  {$IFNDEF FRAMEWORK_FMX}
  TOptionalColor = record
    HasValue: Boolean;
    Value: TColor;
    {$IFDEF FPC}
    class operator :=(AValue: TColor): TOptionalColor;
    {$ELSE}
    class operator Implicit(AValue: TColor): TOptionalColor;
    {$ENDIF}
    class function Some(AValue: TColor): TOptionalColor; static;
    class function None: TOptionalColor; static;
  end;
  {$ENDIF}

  { TOptionalBoolean }

  TOptionalBoolean = record
    HasValue: Boolean;
    Value: Boolean;
    {$IFDEF FPC}
    class operator :=(AValue: Boolean): TOptionalBoolean;
    {$ELSE}
    class operator Implicit(AValue: Boolean): TOptionalBoolean;
    {$ENDIF}
    class function Some(AValue: Boolean): TOptionalBoolean; static;
    class function None: TOptionalBoolean; static;
  end;

  { TOptionalFontStyles }

  {$IFNDEF FRAMEWORK_FMX}
  TOptionalFontStyles = record
    HasValue: Boolean;
    Value: TFontStyles;
    {$IFDEF FPC}
    class operator :=(AValue: TFontStyles): TOptionalFontStyles;
    {$ELSE}
    class operator Implicit(AValue: TFontStyles): TOptionalFontStyles;
    {$ENDIF}
    class function Some(AValue: TFontStyles): TOptionalFontStyles; static;
    class function None: TOptionalFontStyles; static;
  end;
  {$ENDIF}

implementation

{ TOptionalString }

{$IFDEF FPC}
class operator TOptionalString.:=(AValue: string): TOptionalString;
{$ELSE}
class operator TOptionalString.Implicit(AValue: string): TOptionalString;
{$ENDIF}
begin
  Result := TOptionalString.Some(AValue);
end;

class function TOptionalString.None: TOptionalString;
begin
  Result.Value := EmptyStr;
  Result.HasValue := False;
end;

class function TOptionalString.Some(AValue: string): TOptionalString;
begin
  Result.HasValue := True;
  Result.Value := AValue;
end;

{ TOptionalAlign }

{$IFDEF FPC}
class operator TOptionalAlign.:=(AValue: TAlign): TOptionalAlign;
{$ELSE}
class operator TOptionalAlign.Implicit(
  AValue: {$IFDEF FRAMEWORK_FMX}TAlignLayout{$ELSE}TAlign{$ENDIF}): TOptionalAlign;
{$ENDIF}
begin
  Result := TOptionalAlign.Some(AValue);
end;

class function TOptionalAlign.None: TOptionalAlign;
begin
  {$IFDEF FRAMEWORK_FMX}
  Result.Value := TAlignLayout.None;
  {$ELSE}
  Result.Value := alNone;
  {$ENDIF}
  Result.HasValue := False;
end;

class function TOptionalAlign.Some(
  AValue: {$IFDEF FRAMEWORK_FMX}TAlignLayout{$ELSE}TAlign{$ENDIF}): TOptionalAlign;
begin
  Result.HasValue := True;
  Result.Value := AValue;
end;

{ TOptionalSingle }

{$IFDEF FPC}
class operator TOptionalSingle.:=(AValue: Single): TOptionalSingle;
{$ELSE}
class operator TOptionalSingle.Implicit(AValue: Single): TOptionalSingle;
{$ENDIF}
begin
  Result := TOptionalSingle.Some(AValue);
end;

class function TOptionalSingle.Some(AValue: Single): TOptionalSingle;
begin
  Result.HasValue := True;
  Result.Value := AValue;
end;

class function TOptionalSingle.None: TOptionalSingle;
begin
  Result.Value := -1;
  Result.HasValue := False;
end;

{ TOptionalInteger }

{$IFDEF FPC}
class operator TOptionalInteger.:=(AValue: Integer): TOptionalInteger;
{$ELSE}
class operator TOptionalInteger.Implicit(AValue: Integer): TOptionalInteger;
{$ENDIF}
begin
  Result := TOptionalInteger.Some(AValue);
end;

class function TOptionalInteger.Some(AValue: Integer): TOptionalInteger;
begin
  Result.HasValue := True;
  Result.Value := AValue;
end;

class function TOptionalInteger.None: TOptionalInteger;
begin
  Result.Value := -1;
  Result.HasValue := False;
end;

{ TOptionalColor }

{$IFNDEF FRAMEWORK_FMX}
{$IFDEF FPC}
class operator TOptionalColor.:=(AValue: TColor): TOptionalColor;
{$ELSE}
class operator TOptionalColor.Implicit(AValue: TColor): TOptionalColor;
{$ENDIF}
begin
  Result := TOptionalColor.Some(AValue);
end;

class function TOptionalColor.Some(AValue: TColor): TOptionalColor;
begin
  Result.HasValue := True;
  Result.Value := AValue;
end;

class function TOptionalColor.None: TOptionalColor;
begin
  Result.Value := clNone;
  Result.HasValue := False;
end;
{$ENDIF}

{ TOptionalBoolean }

{$IFDEF FPC}
class operator TOptionalBoolean.:=(AValue: Boolean): TOptionalBoolean;
{$ELSE}
class operator TOptionalBoolean.Implicit(AValue: Boolean): TOptionalBoolean;
{$ENDIF}
begin
  Result := TOptionalBoolean.Some(AValue);
end;

class function TOptionalBoolean.Some(AValue: Boolean): TOptionalBoolean;
begin
  Result.HasValue := True;
  Result.Value := AValue;
end;

class function TOptionalBoolean.None: TOptionalBoolean;
begin
  Result.Value := False;
  Result.HasValue := False;
end;

{ TOptionalFontStyles }

{$IFNDEF FRAMEWORK_FMX}
{$IFDEF FPC}
class operator TOptionalFontStyles.:=(AValue: TFontStyles): TOptionalFontStyles;
{$ELSE}
class operator TOptionalFontStyles.Implicit(AValue: TFontStyles): TOptionalFontStyles;
{$ENDIF}
begin
  Result := TOptionalFontStyles.Some(AValue);
end;

class function TOptionalFontStyles.Some(AValue: TFontStyles): TOptionalFontStyles;
begin
  Result.HasValue := True;
  Result.Value := AValue;
end;

class function TOptionalFontStyles.None: TOptionalFontStyles;
begin
  Result.Value := [];
  Result.HasValue := False;
end;
{$ENDIF}

end.

