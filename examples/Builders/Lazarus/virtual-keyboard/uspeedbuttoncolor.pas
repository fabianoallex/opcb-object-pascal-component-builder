unit USpeedButtonColor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Buttons, Graphics, LMessages, Controls, Generics.Collections,
  OPCB.Optionals;

type
  TButtonStyleType = (
    ttDefault,
    ttBlueLight,
    ttGreenHighlight,   // botões de destaque positivo
    ttRedAlert,         // botões de alerta/ação crítica
    ttGrayNeutral,      // botões comuns
    ttDarkGray
  );
  TButtonStyleName = string;

  TSpeedButtonColor = class;

  TButtonStyleItemKind = (bkEnum, bkName);

  TButtonStyleItem = record
    Kind: TButtonStyleItemKind;
    EnumValue: TButtonStyleType;
    Name: string;
  end;

  IButtonStyleSet = interface
    ['{D15D7E8A-71F0-4D3A-9C34-B327F69A1F7E}'] // GUID único
    function Add(AEnum: TButtonStyleType): IButtonStyleSet; overload;
    function Add(const AName: string): IButtonStyleSet; overload;
    function ApplyTo(ABtn: TSpeedButtonColor): IButtonStyleSet;
  end;

  { TButtonStyleSet }

  TButtonStyleSet = class(TInterfacedObject, IButtonStyleSet)
  private
    FItems: array of TButtonStyleItem;
  public
    function Add(AEnum: TButtonStyleType): IButtonStyleSet; overload;
    function Add(const AName: string): IButtonStyleSet; overload;
    function ApplyTo(ABtn: TSpeedButtonColor): IButtonStyleSet;
    class function CreateNew: TButtonStyleSet; static;
  end;

  { TButtonStyle }

  TButtonStyle = class
  private
    FBorderRadius: TOptionalInteger;
    FColor: TOptionalColor;
    FDisabledColor: TOptionalColor;
    FDisabledFontColor: TOptionalColor;
    FDownBorderRadius: TOptionalInteger;
    FDownColor: TOptionalColor;
    FDownOutlineColor: TOptionalColor;
    FDownOutlineWidth: TOptionalInteger;
    FFontColor: TOptionalColor;
    FFontSize: TOptionalInteger;
    FFontStyle: TOptionalFontStyles;
    FHoverBorderRadius: TOptionalInteger;
    FHoverColor: TOptionalColor;
    FHoverHighlight: TOptionalBoolean;
    FHoverOutlineColor: TOptionalColor;
    FHoverOutlineWidth: TOptionalInteger;
    FOutline: TOptionalBoolean;
    FOutlineColor: TOptionalColor;
    FOutlineWidth: TOptionalInteger;
    FSpacing: TOptionalInteger;
    procedure SetBorderRadius(AValue: TOptionalInteger);
    procedure SetColor(AValue: TOptionalColor);
    procedure SetDisabledColor(AValue: TOptionalColor);
    procedure SetDisabledFontColor(AValue: TOptionalColor);
    procedure SetDownBorderRadius(AValue: TOptionalInteger);
    procedure SetDownColor(AValue: TOptionalColor);
    procedure SetDownOutlineColor(AValue: TOptionalColor);
    procedure SetDownOutlineWidth(AValue: TOptionalInteger);
    procedure SetFontColor(AValue: TOptionalColor);
    procedure SetFontSize(AValue: TOptionalInteger);
    procedure SetFontStyle(AValue: TOptionalFontStyles);
    procedure SetHoverBorderRadius(AValue: TOptionalInteger);
    procedure SetHoverColor(AValue: TOptionalColor);
    procedure SetHoverHighlight(AValue: TOptionalBoolean);
    procedure SetHoverOutlineColor(AValue: TOptionalColor);
    procedure SetHoverOutlineWidth(AValue: TOptionalInteger);
    procedure SetOutline(AValue: TOptionalBoolean);
    procedure SetOutlineColor(AValue: TOptionalColor);
    procedure SetOutlineWidth(AValue: TOptionalInteger);
    procedure SetSpacing(AValue: TOptionalInteger);
  public
    // aplica no botão
    procedure Apply(ABtn: TSpeedButtonColor);
    function Clone: TButtonStyle;


    function WithColor(AValue: TColor): TButtonStyle;
    function WithDisabledColor(AValue: TColor): TButtonStyle;
    function WithHoverColor(AValue: TColor): TButtonStyle;
    function WithDownColor(AValue: TColor): TButtonStyle;

    function WithOutline(AValue: Boolean): TButtonStyle;
    function WithOutlineColor(AValue: TColor): TButtonStyle;
    function WithHoverOutlineColor(AValue: TColor): TButtonStyle;
    function WithDownOutlineColor(AValue: TColor): TButtonStyle;
    function WithOutlineWidth(AValue: Integer): TButtonStyle;
    function WithHoverOutlineWidth(AValue: Integer): TButtonStyle;
    function WithDownOutlineWidth(AValue: Integer): TButtonStyle;

    function WithHoverHighlight(AValue: Boolean): TButtonStyle;
    function WithBorderRadius(AValue: Integer): TButtonStyle;
    function WithHoverBorderRadius(AValue: Integer): TButtonStyle;
    function WithDownBorderRadius(AValue: Integer): TButtonStyle;

    function WithFontColor(AValue: TColor): TButtonStyle;
    function WithDisabledFontColor(AValue: TColor): TButtonStyle;
    function WithFontStyle(AValue: TFontStyles): TButtonStyle;
    function WithFontSize(AValue: Integer): TButtonStyle;

    function WithSpacing(AValue: Integer): TButtonStyle;



    // fundo
    property Color: TOptionalColor read FColor write SetColor;
    property DisabledColor: TOptionalColor read FDisabledColor write SetDisabledColor;
    property HoverColor: TOptionalColor read FHoverColor write SetHoverColor;
    property DownColor: TOptionalColor read FDownColor write SetDownColor;

    // contorno
    property Outline: TOptionalBoolean read FOutline write SetOutline;
    property OutlineColor: TOptionalColor read FOutlineColor write SetOutlineColor;
    property HoverOutlineColor: TOptionalColor read FHoverOutlineColor write SetHoverOutlineColor;
    property DownOutlineColor: TOptionalColor read FDownOutlineColor write SetDownOutlineColor;
    property OutlineWidth: TOptionalInteger read FOutlineWidth write SetOutlineWidth;
    property HoverOutlineWidth: TOptionalInteger read FHoverOutlineWidth write SetHoverOutlineWidth;
    property DownOutlineWidth: TOptionalInteger read FDownOutlineWidth write SetDownOutlineWidth;
    // borda
    property HoverHighlight: TOptionalBoolean read FHoverHighlight write SetHoverHighlight;
    property BorderRadius: TOptionalInteger read FBorderRadius write SetBorderRadius;
    property HoverBorderRadius: TOptionalInteger read FHoverBorderRadius write SetHoverBorderRadius;
    property DownBorderRadius: TOptionalInteger read FDownBorderRadius write SetDownBorderRadius;
    // texto
    property FontColor: TOptionalColor read FFontColor write SetFontColor;
    property DisabledFontColor: TOptionalColor read FDisabledFontColor write SetDisabledFontColor;
    property FontStyle: TOptionalFontStyles read FFontStyle write SetFontStyle;
    property FontSize: TOptionalInteger read FFontSize write SetFontSize;
    property Spacing: TOptionalInteger read FSpacing write SetSpacing;
  end;

  { TButtonStyleManager }

  TButtonStyleManager = class
  private
    class var FButtonStyles: specialize TDictionary<TButtonStyleType, TButtonStyle>;
    class var FCustomButtonStyles: specialize TDictionary<TButtonStyleName, TButtonStyle>;
    class function DefaultButtonStyle: TButtonStyle;
    class function BlueLightButtonStyle: TButtonStyle;
    class function GreenButtonStyle: TButtonStyle;
    class function RedButtonStyle: TButtonStyle;
    class function GrayButtonStyle: TButtonStyle;
    class function DarkGrayButtonStyle: TButtonStyle;
    class procedure InitDefaults;
  public
    class constructor Create;
    class destructor Destroy;
    class function CloneButtonStyle(AButtonStyleType: TButtonStyleType): TButtonStyle;
    class function CloneCustomButtonStyle(const AName: string): TButtonStyle;
    class procedure RegisterCustomButtonStyle(const AName: string; AButtonStyleObj: TButtonStyle);
    class procedure ApplyTo(AButtonStyleType: TButtonStyleType; ABtn: TSpeedButtonColor);
    class procedure ApplyTo(AButtonStyleName: TButtonStyleName; ABtn: TSpeedButtonColor);
    class function NewSet: IButtonStyleSet;
    class function NewButtonStyle: TButtonStyle;
  end;

  { TSpeedButtonColor }

  TSpeedButtonColor = class(TSpeedButton)
  private
    FCustomButtonStyle: TButtonStyleName;
    FDisabledColor: TColor;
    FDisabledFontColor: TColor;
    FDownBorderRadius: Integer;
    FDownOutlineColor: TColor;
    FDownOutlineWidth: Integer;
    FFontColor: TColor;
    FHoverBorderRadius: Integer;
    FHoverOutlineColor: TColor;
    FHoverOutlineWidth: Integer;
    FOutline: Boolean;
    FOutlineColor: TColor;
    FOutlineWidth: Integer;
    FBorderRadius: Integer;
    FHover: Boolean;
    FHoverHighlight: Boolean;
    FPressed: Boolean;
    FDownColor: TColor;
    FHoverColor: TColor;
    FTextGlyphSpacing: Integer;
    FButtonStyle: TButtonStyleType;
    procedure SetCustomButtonStyle(AValue: TButtonStyleName);
    procedure SetDisabledColor(AValue: TColor);
    procedure SetDisabledFontColor(AValue: TColor);
    procedure SetDownBorderRadius(AValue: Integer);
    procedure SetDownOutlineColor(AValue: TColor);
    procedure SetDownOutlineWidth(AValue: Integer);
    procedure SetFontColor(AValue: TColor);
    procedure SetHoverBorderRadius(AValue: Integer);
    procedure SetHoverOutlineColor(AValue: TColor);
    procedure SetHoverOutlineWidth(AValue: Integer);
    procedure SetOutline(AValue: Boolean);
    procedure SetOutlineColor(AValue: TColor);
    procedure SetOutlineWidth(AValue: Integer);
    procedure SetBorderRadius(AValue: Integer);
    procedure SetDownColor(AValue: TColor);
    procedure SetHoverColor(AValue: TColor);
    procedure SetHoverHighlight(AValue: Boolean);
    procedure SetTextGlyphSpacing(AValue: Integer);
    procedure SetButtonStyle(AValue: TButtonStyleType);
  protected
    procedure Paint; override;
    procedure CMMouseEnter(var Message: TLMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TLMessage); message CM_MOUSELEAVE;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property DisabledColor: TColor read FDisabledColor write SetDisabledColor;

    property Outline: Boolean read FOutline write SetOutline;
    property OutlineColor: TColor read FOutlineColor write SetOutlineColor;
    property OutlineWidth: Integer read FOutlineWidth write SetOutlineWidth;
    property BorderRadius: Integer read FBorderRadius write SetBorderRadius;

    property HoverHighlight: Boolean read FHoverHighlight write SetHoverHighlight;
    property HoverColor: TColor read FHoverColor write SetHoverColor default clBtnShadow;
    property HoverOutlineColor: TColor read FHoverOutlineColor write SetHoverOutlineColor;
    property HoverOutlineWidth: Integer read FHoverOutlineWidth write SetHoverOutlineWidth;
    property HoverBorderRadius: Integer read FHoverBorderRadius write SetHoverBorderRadius;

    property DownColor: TColor read FDownColor write SetDownColor default clBtnShadow;
    property DownOutlineColor: TColor read FDownOutlineColor write SetDownOutlineColor;
    property DownOutlineWidth: Integer read FDownOutlineWidth write SetDownOutlineWidth;
    property DownBorderRadius: Integer read FDownBorderRadius write SetDownBorderRadius;

    property FontColor: TColor read FFontColor write SetFontColor;
    property DisabledFontColor: TColor read FDisabledFontColor write SetDisabledFontColor;
  end;

implementation

{ TButtonStyleManager }

class procedure TButtonStyleManager.InitDefaults;
begin
  FButtonStyles.Add(TButtonStyleType.ttDefault, DefaultButtonStyle);
  FButtonStyles.Add(TButtonStyleType.ttBlueLight, BlueLightButtonStyle);
  FButtonStyles.Add(TButtonStyleType.ttGreenHighlight, GreenButtonStyle);
  FButtonStyles.Add(TButtonStyleType.ttRedAlert, RedButtonStyle);
  FButtonStyles.Add(TButtonStyleType.ttGrayNeutral, GrayButtonStyle);
  FButtonStyles.Add(TButtonStyleType.ttDarkGray, DarkGrayButtonStyle);
end;

class constructor TButtonStyleManager.Create;
begin
  FButtonStyles := specialize TDictionary<TButtonStyleType, TButtonStyle>.create;
  FCustomButtonStyles := specialize TDictionary<TButtonStyleName, TButtonStyle>.create;
  InitDefaults;
end;

class destructor TButtonStyleManager.Destroy;
var
  Entry: TButtonStyle;
begin
  for Entry in FButtonStyles.Values do
    Entry.Free;

  for Entry in FCustomButtonStyles.Values do
    Entry.Free;

  FButtonStyles.Free;
  FCustomButtonStyles.Free;
end;

class function TButtonStyleManager.CloneButtonStyle(AButtonStyleType: TButtonStyleType): TButtonStyle;
begin
  if FButtonStyles.ContainsKey(AButtonStyleType) then
    if not FButtonStyles.TryGetValue(AButtonStyleType, Result) then
      Result := nil;

  if Assigned(Result) then
    Result := Result.Clone;
end;

class function TButtonStyleManager.CloneCustomButtonStyle(const AName: string): TButtonStyle;
begin
  if FCustomButtonStyles.ContainsKey(AName) then
    if not FCustomButtonStyles.TryGetValue(AName, Result) then
      Result := nil;

  if Assigned(Result) then
    Result := Result.Clone;
end;

class procedure TButtonStyleManager.RegisterCustomButtonStyle(const AName: string;
  AButtonStyleObj: TButtonStyle);
var
  ButtonStyleTemp: TButtonStyle;
begin
  if FCustomButtonStyles.ContainsKey(AName) then
    if FCustomButtonStyles.TryGetValue(AName, ButtonStyleTemp) then
      ButtonStyleTemp.Free;

  FCustomButtonStyles.AddOrSetValue(AName, AButtonStyleObj);
end;

class procedure TButtonStyleManager.ApplyTo(AButtonStyleType: TButtonStyleType;
  ABtn: TSpeedButtonColor);
var
  AButtonStyle: TButtonStyle;
begin
  if FButtonStyles.ContainsKey(AButtonStyleType) then
    if FButtonStyles.TryGetValue(AButtonStyleType, AButtonStyle) then
      AButtonStyle.Apply(ABtn);
end;

class procedure TButtonStyleManager.ApplyTo(AButtonStyleName: TButtonStyleName;
  ABtn: TSpeedButtonColor);
var
  AButtonStyle: TButtonStyle;
begin
  if FCustomButtonStyles.ContainsKey(AButtonStyleName) then
    if FCustomButtonStyles.TryGetValue(AButtonStyleName, AButtonStyle) then
      AButtonStyle.Apply(ABtn);
end;

class function TButtonStyleManager.NewSet: IButtonStyleSet;
begin
  Result := TButtonStyleSet.CreateNew;
end;

class function TButtonStyleManager.NewButtonStyle: TButtonStyle;
begin
  Result := TButtonStyle.Create;
end;

class function TButtonStyleManager.DefaultButtonStyle: TButtonStyle;
begin
  Result := TButtonStyle.Create;
  Result.Color := clBtnFace;
  Result.FontColor := clWindowText;
  Result.Outline := False;
end;

class function TButtonStyleManager.BlueLightButtonStyle: TButtonStyle;
begin
  Result := TButtonStyle.Create;
  Result.BorderRadius := 18;;

  // === Cores de fundo ===
  Result.Color := $00FFAA40;       // azul claro
  Result.HoverColor := $00ED9564;  // Azul médio (#6495ED)
  Result.DownColor  := $00CD0000;  // Azul escuro (#0000CD)

  Result.Outline := True;
  Result.OutlineColor := $00D47F00;    // contorno sutil
  Result.OutlineWidth := 1;

  Result.HoverOutlineColor := $00FFB050; // contorno um pouco mais forte no hover
  Result.HoverOutlineWidth := 2;

  Result.DownOutlineColor := $00CC6600;  // contorno no estado down
  Result.DownOutlineWidth := 1;

  Result.HoverHighlight := True;         // ativa efeito visual no hover
  Result.HoverBorderRadius := 18;
  Result.DownBorderRadius  := 18;

  Result.Spacing := 10;
  Result.FontColor := clWhite;

  Result.FontStyle := Result.FontStyle.Value + [fsBold];
  Result.FontSize := 14;
end;

class function TButtonStyleManager.GreenButtonStyle: TButtonStyle;
begin
  Result := TButtonStyle.Create;
  Result.BorderRadius := 20;

  // Fundo
  Result.Color := $0099CC66;       // verde médio
  Result.HoverColor := $00A6D28C;  // verde mais claro no hover
  Result.DownColor := $0073A055;   // verde escuro quando clicado

  // Contorno
  Result.Outline := True;
  Result.OutlineColor := $0080B360;      // contorno suave
  Result.OutlineWidth := 3;

  Result.HoverOutlineColor := $00A0D2A0; // contorno no hover
  Result.HoverOutlineWidth := 3;

  Result.DownOutlineColor := $00669944;  // contorno no down
  Result.DownOutlineWidth := 3;

  Result.HoverHighlight := True;
  Result.HoverBorderRadius := 20;
  Result.DownBorderRadius := 20;

  Result.Spacing := 10;
  Result.FontColor := clWhite;
  Result.FontStyle := Result.FontStyle.Value + [fsBold];
  Result.FontSize := 14;
end;

class function TButtonStyleManager.RedButtonStyle: TButtonStyle;
begin
  Result := TButtonStyle.Create;
  Result.BorderRadius := 18;

  // Fundo
  Result.Color := RGBToColor(224, 102, 102);
  Result.HoverColor := RGBToColor(240, 128, 128);
  Result.DownColor := RGBToColor(176, 48, 48);

  // Contorno
  Result.Outline := True;
  Result.OutlineWidth := 2;
  Result.HoverOutlineWidth := 2;
  Result.DownOutlineWidth := 2;

  Result.OutlineColor := RGBToColor(192, 80, 80);
  Result.HoverOutlineColor := RGBToColor(240, 144, 144);
  Result.DownOutlineColor := RGBToColor(128, 64, 64);

  Result.HoverHighlight := True;
  Result.HoverBorderRadius := 18;
  Result.DownBorderRadius := 18;

  Result.Spacing := 10;
  Result.FontColor := clWhite;
  Result.FontStyle := Result.FontStyle.Value + [fsBold];
  Result.FontSize := 14;
end;

class function TButtonStyleManager.GrayButtonStyle: TButtonStyle;
begin
  Result := TButtonStyle.Create;
  Result.BorderRadius := 18;

  // Fundo
  Result.Color := $00D3D3D3;       // cinza claro
  Result.HoverColor := $00E0E0E0;  // cinza mais claro no hover
  Result.DownColor := $00B0B0B0;   // cinza escuro ao clicar

  // Contorno
  Result.Outline := True;
  Result.OutlineColor := $00999999;      // contorno suave
  Result.OutlineWidth := 1;

  Result.HoverOutlineColor := $00AAAAAA; // contorno no hover
  Result.HoverOutlineWidth := 1;

  Result.DownOutlineColor := $00777777;  // contorno no down
  Result.DownOutlineWidth := 1;

  Result.HoverHighlight := True;
  Result.HoverBorderRadius := 18;
  Result.DownBorderRadius := 18;

  Result.Spacing := 8;
  Result.FontColor := clBlack;
  Result.FontStyle := [];
  Result.FontSize := 12;
end;

class function TButtonStyleManager.DarkGrayButtonStyle: TButtonStyle;
begin
  Result := TButtonStyle.Create;
  Result.BorderRadius := 12;

  // Fundo
  Result.Color := $00A9A9A9;       // cinza escuro
  Result.HoverColor := $00BFBFBF;  // cinza médio no hover
  Result.DownColor := $00808080;   // cinza mais escuro ao clicar

  // Contorno
  Result.Outline := True;
  Result.OutlineColor := $00606060;      // contorno sutil
  Result.OutlineWidth := 1;

  Result.HoverOutlineColor := $00808080; // contorno no hover
  Result.HoverOutlineWidth := 1;

  Result.DownOutlineColor := $00404040;  // contorno no down
  Result.DownOutlineWidth := 1;

  Result.HoverHighlight := True;
  Result.HoverBorderRadius := 12;
  Result.DownBorderRadius := 12;

  Result.Spacing := 8;

  Result.FontColor := clWhite;
  Result.FontStyle := [];
  Result.FontSize := 12;
end;

{ TSpeedButtonColor }

procedure TSpeedButtonColor.SetDownColor(AValue: TColor);
begin
  if FDownColor = AValue then Exit;
  FDownColor := AValue;
  Invalidate;
end;

procedure TSpeedButtonColor.SetOutline(AValue: Boolean);
begin
  if FOutline = AValue then Exit;
  FOutline := AValue;
  Invalidate;
end;

procedure TSpeedButtonColor.SetDownBorderRadius(AValue: Integer);
begin
  if FDownBorderRadius = AValue then Exit;
  FDownBorderRadius := AValue;
  Invalidate;
end;

procedure TSpeedButtonColor.SetCustomButtonStyle(AValue: TButtonStyleName);
begin
  if FCustomButtonStyle = AValue then Exit;
  FCustomButtonStyle := AValue;
  TButtonStyleManager.ApplyTo(FCustomButtonStyle, Self);
  Invalidate;
end;

procedure TSpeedButtonColor.SetDisabledColor(AValue: TColor);
begin
  if FDisabledColor = AValue then Exit;
  FDisabledColor := AValue;
  Invalidate;
end;

procedure TSpeedButtonColor.SetDisabledFontColor(AValue: TColor);
begin
  if FDisabledFontColor = AValue then Exit;
  FDisabledFontColor := AValue;
  Invalidate;
end;

procedure TSpeedButtonColor.SetDownOutlineColor(AValue: TColor);
begin
  if FDownOutlineColor = AValue then Exit;
  FDownOutlineColor := AValue;
  Invalidate;
end;

procedure TSpeedButtonColor.SetDownOutlineWidth(AValue: Integer);
begin
  if FDownOutlineWidth = AValue then Exit;
  FDownOutlineWidth := AValue;
  Invalidate;
end;

procedure TSpeedButtonColor.SetFontColor(AValue: TColor);
begin
  if FFontColor = AValue then Exit;
  FFontColor := AValue;
  Invalidate;
end;

procedure TSpeedButtonColor.SetHoverBorderRadius(AValue: Integer);
begin
  if FHoverBorderRadius = AValue then Exit;
  FHoverBorderRadius := AValue;
  Invalidate;
end;

procedure TSpeedButtonColor.SetHoverOutlineColor(AValue: TColor);
begin
  if FHoverOutlineColor = AValue then Exit;
  FHoverOutlineColor := AValue;
  Invalidate;
end;

procedure TSpeedButtonColor.SetHoverOutlineWidth(AValue: Integer);
begin
  if FHoverOutlineWidth = AValue then Exit;
  FHoverOutlineWidth := AValue;
  Invalidate;
end;

procedure TSpeedButtonColor.SetOutlineColor(AValue: TColor);
begin
  if FOutlineColor = AValue then Exit;
  FOutlineColor := AValue;
  Invalidate;
end;

procedure TSpeedButtonColor.SetOutlineWidth(AValue: Integer);
begin
  if FOutlineWidth = AValue then Exit;
  FOutlineWidth := AValue;
  Invalidate;
end;

procedure TSpeedButtonColor.SetBorderRadius(AValue: Integer);
begin
  if FBorderRadius = AValue then Exit;
  FBorderRadius := AValue;
  Invalidate;
end;

procedure TSpeedButtonColor.SetHoverColor(AValue: TColor);
begin
 if FHoverColor = AValue then Exit;
 FHoverColor := AValue;
 invalidate;
end;

procedure TSpeedButtonColor.SetHoverHighlight(AValue: Boolean);
begin
  if FHoverHighlight = AValue then Exit;
  FHoverHighlight := AValue;
end;

procedure TSpeedButtonColor.SetTextGlyphSpacing(AValue: Integer);
begin
  if FTextGlyphSpacing = AValue then Exit;
  FTextGlyphSpacing := AValue;
  Invalidate;
end;

procedure TSpeedButtonColor.SetButtonStyle(AValue: TButtonStyleType);
begin
  if FButtonStyle = AValue then Exit;
  FButtonStyle := AValue;
  TButtonStyleManager.ApplyTo(FButtonStyle, Self);
  Invalidate;
end;

procedure TSpeedButtonColor.Paint;
var
  R: TRect;
  Txt: string;
  TxtW, TxtH: Integer;
  ImgW, ImgH: Integer;
  Radius: Integer;
  GlyphRect, TextRect: TRect;
  FillCol, PenCol, FontCol: TColor;
  PenW: Integer;
  InnerRect: TRect;
begin
  R := ClientRect;
  Canvas.Brush.Style := bsSolid;

  FontCol := FontColor;

  if not Enabled then
  begin
    FillCol := DisabledColor;
    PenCol := DisabledColor;
    PenW := 0;
    Radius := BorderRadius;
    FontCol := DisabledFontColor;
  end
  else if FPressed then
  begin
    FillCol := FDownColor;
    if Outline then
      PenCol := FDownOutlineColor
    else
      PenCol := FDownColor;
    PenW := OutlineWidth;
    Radius := FDownBorderRadius;
  end
  else if FHover and FHoverHighlight then
  begin
    FillCol := FHoverColor;
    if Outline then
      PenCol := FHoverOutlineColor
    else
      PenCol := FHoverColor;
    PenW := HoverOutlineWidth;
    Radius := HoverBorderRadius;
  end
  else
  begin
    FillCol := Color;
    if Outline then
      PenCol := OutlineColor
    else
      PenCol := Color;
    PenW := OutlineWidth;
    Radius := BorderRadius;
  end;

  // Desenha contorno (externo)
  Canvas.Brush.Color := PenCol;
  Canvas.Pen.Style := psClear; // só preencher
  Canvas.RoundRect(R.Left, R.Top, R.Right, R.Bottom, Radius, Radius);

  // Desenha interior (interno)
  InnerRect := Rect(
    R.Left + PenW,
    R.Top + PenW,
    R.Right - PenW,
    R.Bottom - PenW
  );

  Canvas.Brush.Color := FillCol;

  Canvas.RoundRect(
    InnerRect.Left,
    InnerRect.Top,
    InnerRect.Right,
    InnerRect.Bottom,
    Radius - PenW,
    Radius - PenW
  );

  Self.Font.Color := FontCol;

  // === Preparar Texto ===
  Canvas.Font := Self.Font;
  Txt := Caption;
  TxtW := Canvas.TextWidth(Txt);
  TxtH := Canvas.TextHeight(Txt);

  // === Preparar Ícone (Glyph ou Images) ===
  ImgW := 0;
  ImgH := 0;
  if (Images <> nil) and (ImageIndex >= 0) and (ImageIndex < Images.Count) then
  begin
    ImgW := Images.Width;
    ImgH := Images.Height;
  end
  else if (Glyph <> nil) and (not Glyph.Empty) then
  begin
    ImgW := Glyph.Width div NumGlyphs;
    ImgH := Glyph.Height;
  end;

  // === Definir posições conforme Layout ===
  case Layout of
    blGlyphLeft, blGlyphRight:
      begin
        if (Txt <> '') and (ImgW > 0) then
        begin
          if Layout = blGlyphLeft then
          begin
            GlyphRect.Left := (R.Width - (ImgW + Spacing + TxtW)) div 2;
            TextRect.Left  := GlyphRect.Left + ImgW + Spacing;
          end
          else
          begin
            TextRect.Left  := (R.Width - (TxtW + Spacing + ImgW)) div 2;
            GlyphRect.Left := TextRect.Left + TxtW + Spacing;
          end;
          GlyphRect.Top := (R.Height - ImgH) div 2;
          TextRect.Top  := (R.Height - TxtH) div 2;
        end
        else if ImgW > 0 then
        begin
          GlyphRect.Left := (R.Width - ImgW) div 2;
          GlyphRect.Top  := (R.Height - ImgH) div 2;
        end
        else
        begin
          TextRect.Left := (R.Width - TxtW) div 2;
          TextRect.Top  := (R.Height - TxtH) div 2;
        end;
      end;

    blGlyphTop, blGlyphBottom:
      begin
        if (Txt <> '') and (ImgH > 0) then
        begin
          if Layout = blGlyphTop then
          begin
            GlyphRect.Top := (R.Height - (ImgH + Spacing + TxtH)) div 2;
            TextRect.Top  := GlyphRect.Top + ImgH + Spacing;
          end
          else
          begin
            TextRect.Top  := (R.Height - (TxtH + Spacing + ImgH)) div 2;
            GlyphRect.Top := TextRect.Top + TxtH + Spacing;
          end;
          GlyphRect.Left := (R.Width - ImgW) div 2;
          TextRect.Left  := (R.Width - TxtW) div 2;
        end
        else if ImgH > 0 then
        begin
          GlyphRect.Left := (R.Width - ImgW) div 2;
          GlyphRect.Top  := (R.Height - ImgH) div 2;
        end
        else
        begin
          TextRect.Left := (R.Width - TxtW) div 2;
          TextRect.Top  := (R.Height - TxtH) div 2;
        end;
      end;

    else
      begin
        if ImgW > 0 then
        begin
          GlyphRect.Left := (R.Width - ImgW) div 2;
          GlyphRect.Top  := (R.Height - ImgH) div 2;
        end
        else
        begin
          TextRect.Left := (R.Width - TxtW) div 2;
          TextRect.Top  := (R.Height - TxtH) div 2;
        end;
      end;
  end;

  if FPressed then
  begin
    TextRect.Top := TextRect.Top + 2;
    GlyphRect.Top := GlyphRect.Top + 2;
  end;

  // === Desenhar Glyph ===
  if (ImgW > 0) then
  begin
    if (Images <> nil) and (ImageIndex >= 0) then
      Images.Draw(Canvas, GlyphRect.Left, GlyphRect.Top, ImageIndex, Enabled)
    else if (Glyph <> nil) and (not Glyph.Empty) then
      Canvas.Draw(GlyphRect.Left, GlyphRect.Top, Glyph);
  end;

  // === Desenhar Texto ===
  if (Txt <> '') then
    Canvas.TextOut(TextRect.Left, TextRect.Top, Txt);
end;

procedure TSpeedButtonColor.CMMouseEnter(var Message: TLMessage);
begin
  inherited;
  if HoverHighlight then
  begin
    FHover := True;
    Invalidate;
  end;
end;

procedure TSpeedButtonColor.CMMouseLeave(var Message: TLMessage);
begin
  inherited;
  FHover := False;
  Invalidate;
end;

procedure TSpeedButtonColor.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  FPressed := True;
  Invalidate;
end;

procedure TSpeedButtonColor.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
 Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  FPressed := False;
  Invalidate;
end;

constructor TSpeedButtonColor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHover := False;
  FPressed := False;
  FHoverColor := Color;
  FDownColor := clBtnShadow;
  Transparent := False;
  ParentColor := False;
  FTextGlyphSpacing := 4;
  FDisabledColor := clBtnFace;
  FDisabledFontColor := clBtnShadow;
end;

{ TButtonStyleSet }

function TButtonStyleSet.Add(AEnum: TButtonStyleType): IButtonStyleSet;
var
  LIndex: Integer;
  Item: TButtonStyleItem;
begin
  Item.Kind := bkEnum;
  Item.EnumValue := AEnum;
  Item.Name := '';

  LIndex := Length(FItems);
  SetLength(FItems, LIndex + 1);
  FItems[LIndex] := Item;

  Result := Self;
end;

function TButtonStyleSet.Add(const AName: string): IButtonStyleSet;
var
  LIndex: Integer;
  Item: TButtonStyleItem;
begin
  Item.Kind := bkName;
  Item.EnumValue := ttDefault;
  Item.Name := AName;

  LIndex := Length(FItems);
  SetLength(FItems, LIndex + 1);
  FItems[LIndex] := Item;

  Result := Self;
end;

function TButtonStyleSet.ApplyTo(ABtn: TSpeedButtonColor): IButtonStyleSet;
var
  item: TButtonStyleItem;
begin
  Result := Self;
  for item in FItems do
  begin
    case item.Kind of
      bkEnum:
        TButtonStyleManager.ApplyTo(item.EnumValue, ABtn);
      bkName:
        TButtonStyleManager.ApplyTo(item.Name, ABtn);
    end;
  end;
end;

class function TButtonStyleSet.CreateNew: TButtonStyleSet;
begin
  Result := TButtonStyleSet.Create;
end;

procedure TButtonStyle.SetBorderRadius(AValue: TOptionalInteger);
begin
  FBorderRadius := AValue;
end;

procedure TButtonStyle.SetColor(AValue: TOptionalColor);
begin
  FColor := AValue;
end;

procedure TButtonStyle.SetDisabledColor(AValue: TOptionalColor);
begin
  FDisabledColor := AValue;
end;

procedure TButtonStyle.SetDisabledFontColor(AValue: TOptionalColor);
begin
  FDisabledFontColor := AValue;
end;

procedure TButtonStyle.SetDownBorderRadius(AValue: TOptionalInteger);
begin
  FDownBorderRadius := AValue;
end;

procedure TButtonStyle.SetDownColor(AValue: TOptionalColor);
begin
  FDownColor := AValue;
end;

procedure TButtonStyle.SetDownOutlineColor(AValue: TOptionalColor);
begin
  FDownOutlineColor := AValue;
end;

procedure TButtonStyle.SetDownOutlineWidth(AValue: TOptionalInteger);
begin
  FDownOutlineWidth := AValue;
end;

procedure TButtonStyle.SetFontColor(AValue: TOptionalColor);
begin
  FFontColor := AValue;
end;

procedure TButtonStyle.SetFontSize(AValue: TOptionalInteger);
begin
  FFontSize := AValue;
end;

procedure TButtonStyle.SetFontStyle(AValue: TOptionalFontStyles);
begin
  FFontStyle := AValue;
end;

procedure TButtonStyle.SetHoverBorderRadius(AValue: TOptionalInteger);
begin
  FHoverBorderRadius := AValue;
end;

procedure TButtonStyle.SetHoverColor(AValue: TOptionalColor);
begin
  FHoverColor := AValue;
end;

procedure TButtonStyle.SetHoverHighlight(AValue: TOptionalBoolean);
begin
  FHoverHighlight := AValue;
end;

procedure TButtonStyle.SetHoverOutlineColor(AValue: TOptionalColor);
begin
  FHoverOutlineColor := AValue;
end;

procedure TButtonStyle.SetHoverOutlineWidth(AValue: TOptionalInteger);
begin
  FHoverOutlineWidth := AValue;
end;

procedure TButtonStyle.SetOutline(AValue: TOptionalBoolean);
begin
  FOutline := AValue;
end;

procedure TButtonStyle.SetOutlineColor(AValue: TOptionalColor);
begin
  FOutlineColor := AValue;
end;

procedure TButtonStyle.SetOutlineWidth(AValue: TOptionalInteger);
begin
  FOutlineWidth := AValue;
end;

procedure TButtonStyle.SetSpacing(AValue: TOptionalInteger);
begin
  FSpacing := AValue;
end;

procedure TButtonStyle.Apply(ABtn: TSpeedButtonColor);
begin
  if not Assigned(ABtn) then
    Exit;

  // Fundo
  if Color.HasValue then
    ABtn.Color := Color.Value;
  if DisabledColor.HasValue then
    ABtn.DisabledColor := DisabledColor.Value;
  if HoverColor.HasValue then
    ABtn.HoverColor := HoverColor.Value;
  if DownColor.HasValue then
    ABtn.DownColor := DownColor.Value;

  // Contorno
  if Outline. HasValue then
    ABtn.Outline := Outline.Value;
  if OutlineColor.HasValue then
    ABtn.OutlineColor := OutlineColor.Value;
  if HoverOutlineColor.HasValue then
    ABtn.HoverOutlineColor := HoverOutlineColor.Value;
  if DownOutlineColor.HasValue then
    ABtn.DownOutlineColor := DownOutlineColor.Value;

  if OutlineWidth.HasValue then
    ABtn.OutlineWidth := OutlineWidth.Value;
  if HoverOutlineWidth.HasValue then
    ABtn.HoverOutlineWidth := HoverOutlineWidth.Value;
  if DownOutlineWidth.HasValue then
    ABtn.DownOutlineWidth := DownOutlineWidth.Value;

  // Borda
  if HoverHighlight.HasValue then
    ABtn.HoverHighlight := HoverHighlight.Value;
  if BorderRadius.HasValue then
    ABtn.BorderRadius := BorderRadius.Value;
  if HoverBorderRadius.HasValue then
    ABtn.HoverBorderRadius := HoverBorderRadius.Value;
  if DownBorderRadius.HasValue then
    ABtn.DownBorderRadius := DownBorderRadius.Value;

  // Texto
  if Spacing.HasValue then
    ABtn.Spacing := Spacing.Value;
  if FontColor.HasValue then
    ABtn.FontColor := FontColor.Value;
  if DisabledFontColor.HasValue then
    ABtn.DisabledFontColor := DisabledFontColor.Value;
  if FontStyle.HasValue then
    ABtn.Font.Style := FontStyle.Value;
  if FontSize.HasValue then
    ABtn.Font.Size := FontSize.Value;
end;


function TButtonStyle.Clone: TButtonStyle;
begin
  Result := TButtonStyle.Create;
  Result.Color := Color;
  Result.HoverColor := HoverColor;
  Result.DownColor := DownColor;

  Result.Outline := Outline;
  Result.OutlineColor := OutlineColor;
  Result.HoverOutlineColor := HoverOutlineColor;
  Result.DownOutlineColor := DownOutlineColor;
  Result.OutlineWidth := OutlineWidth;
  Result.HoverOutlineWidth := HoverOutlineWidth;
  Result.DownOutlineWidth := DownOutlineWidth;

  Result.HoverHighlight := HoverHighlight;
  Result.BorderRadius := BorderRadius;
  Result.HoverBorderRadius := HoverBorderRadius;
  Result.DownBorderRadius := DownBorderRadius;

  Result.FontColor := FontColor;
  Result.FontStyle := FontStyle;
  Result.FontSize := FontSize;
  Result.Spacing := Spacing;
end;

function TButtonStyle.WithColor(AValue: TColor): TButtonStyle;
begin
  Result := Self;
  FColor.Value := AValue;
end;

function TButtonStyle.WithDisabledColor(AValue: TColor): TButtonStyle;
begin
  Result := Self;
  FDisabledColor.Value := AValue;
end;

function TButtonStyle.WithHoverColor(AValue: TColor): TButtonStyle;
begin
  Result := Self;
  FHoverColor.Value := AValue;
end;

function TButtonStyle.WithDownColor(AValue: TColor): TButtonStyle;
begin
  Result := Self;
  FDownColor.Value := AValue;
end;

function TButtonStyle.WithOutline(AValue: Boolean): TButtonStyle;
begin
  Result := Self;
  FOutline.Value := AValue;
end;

function TButtonStyle.WithOutlineColor(AValue: TColor): TButtonStyle;
begin
  Result := Self;
  FOutlineColor.Value := AValue;
end;

function TButtonStyle.WithHoverOutlineColor(AValue: TColor): TButtonStyle;
begin
  Result := Self;
  FHoverOutlineColor.Value := AValue;
end;

function TButtonStyle.WithDownOutlineColor(AValue: TColor): TButtonStyle;
begin
  Result := Self;
  FDownOutlineColor.Value := AValue;
end;

function TButtonStyle.WithOutlineWidth(AValue: Integer): TButtonStyle;
begin
  Result := Self;
  FOutlineWidth.Value := AValue;
end;

function TButtonStyle.WithHoverOutlineWidth(AValue: Integer): TButtonStyle;
begin
  Result := Self;
  FHoverOutlineWidth.Value := AValue;
end;

function TButtonStyle.WithDownOutlineWidth(AValue: Integer): TButtonStyle;
begin
  Result := Self;
  FDownOutlineWidth.Value := AValue;
end;

function TButtonStyle.WithHoverHighlight(AValue: Boolean): TButtonStyle;
begin
  Result := Self;
  FHoverHighlight.Value := AValue;
end;

function TButtonStyle.WithBorderRadius(AValue: Integer): TButtonStyle;
begin
  Result := Self;
  FBorderRadius.Value := AValue;
end;

function TButtonStyle.WithHoverBorderRadius(AValue: Integer): TButtonStyle;
begin
  Result := Self;
  FHoverBorderRadius.Value := AValue;
end;

function TButtonStyle.WithDownBorderRadius(AValue: Integer): TButtonStyle;
begin
  Result := Self;
  FDownBorderRadius.Value := AValue;
end;

function TButtonStyle.WithFontColor(AValue: TColor): TButtonStyle;
begin
  Result := Self;
  FFontColor.Value := AValue;
end;

function TButtonStyle.WithDisabledFontColor(AValue: TColor): TButtonStyle;
begin
  Result := Self;
  FDisabledFontColor.Value := AValue;
end;

function TButtonStyle.WithFontStyle(AValue: TFontStyles): TButtonStyle;
begin
  Result := Self;
  FFontStyle.Value := AValue;
end;

function TButtonStyle.WithFontSize(AValue: Integer): TButtonStyle;
begin
  Result := Self;
  FFontSize.Value := AValue;
end;

function TButtonStyle.WithSpacing(AValue: Integer): TButtonStyle;
begin
  Result := Self;
  FSpacing.Value := AValue;
end;

end.

