unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, OPCB,
  LMessages, StdCtrls, USpeedButtonColor;

type
  { TForm1 }

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton1MouseEnter(Sender: TObject);
    procedure SpeedButton1MouseLeave(Sender: TObject);
  private
    FImages: TImageList;
  private
    procedure SetupMenuImages(AComponent: TComponent);
    procedure SetupMySpeedButton(AControl: TControl);
    procedure SetupMySpeedButtonVerde(AControl: TControl);
    procedure SetupMySpeedButtonVermelho(AControl: TControl);
    procedure SetupMySpeedButtonGrayLight(AControl: TControl);
    procedure SetupMySpeedButtonGrayDark(AControl: TControl);
    property Images: TImageList read FImages;
  public

  end;

var
  Form1: TForm1;

implementation

uses
  LCLIntf;

{$R *.lfm}

{ TForm1 }

procedure TForm1.SetupMenuImages(AComponent: TComponent);
var
  ImageList: TImageList;
  Picture: TPicture;

  procedure AddFromFile(AFileName: string);
  var
    srcBmp: TBitmap;
  begin
    Picture.LoadFromFile(AFileName);
    SrcBmp := TBitmap.Create;
    try
      SrcBmp.Assign(Picture.Graphic);
      ImageList.Add(SrcBmp, nil);
    finally
      SrcBmp.Free;
    end;
  end;

begin
  ImageList := (AComponent as TImageList);
  ImageList.Width := 32;
  ImageList.Height := 32;

  Picture := TPicture.Create;
  try
    AddFromFile('img.user.png');
    AddFromFile('img.logout.png');
    AddFromFile('img.close.png');
    AddFromFile('img.info.png');
  finally
    Picture.Free;
  end;
end;

procedure TForm1.SetupMySpeedButton(AControl: TControl);
var
  Btn: TSpeedButtonColor;
begin
  Btn := AControl as TSpeedButtonColor;
  Btn.Height := 128;
  Btn.Width  := 128;
  TButtonStyleManager.NewSet.Add(ttBlueLight).ApplyTo(Btn);
  Btn.Images := Images;        // ImageList já carregado no Form
  Btn.ImageIndex := 0;         // índice do ícone
  Btn.Layout := blGlyphTop;    // ícone acima do texto
end;

procedure TForm1.SetupMySpeedButtonVerde(AControl: TControl);
var
  Btn: TSpeedButtonColor;
begin
  Btn := AControl as TSpeedButtonColor;
  Btn.Height := 128;
  Btn.Width := 261;

  TButtonStyleManager
    .NewSet
    .Add(ttGreenHighlight)
    .Add('fonte-amarela')
    .ApplyTo(Btn);

  Btn.Images := Images;
  Btn.ImageIndex := 1;
  Btn.Layout := blGlyphTop;
end;

procedure TForm1.SetupMySpeedButtonVermelho(AControl: TControl);
var
  Btn: TSpeedButtonColor;
begin
  Btn := AControl as TSpeedButtonColor;
  Btn.Height := 128;
  Btn.Width := 128;
  TButtonStyleManager.NewSet.Add(ttRedAlert).ApplyTo(Btn);
  Btn.Images := Images;
  Btn.ImageIndex := 2;
  Btn.Layout := blGlyphTop;
end;

procedure TForm1.SetupMySpeedButtonGrayLight(AControl: TControl);
var
  Btn: TSpeedButtonColor;
begin
  Btn := AControl as TSpeedButtonColor;

  TButtonStyleManager
    .NewSet
    .Add(ttGrayNeutral)
    .Add('big-font')
    .ApplyTo(Btn);

  if Btn.Caption = '5' then
    Btn.Enabled := False;
end;

procedure TForm1.SetupMySpeedButtonGrayDark(AControl: TControl);
var
  Btn: TSpeedButtonColor;
begin
  Btn := AControl as TSpeedButtonColor;
  Btn.Height := 128;
  Btn.Width := 128;
  TButtonStyleManager.NewSet.Add(ttGrayNeutral).ApplyTo(Btn);
  Btn.Caption := 'Teste 123';
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  Creators: TOPCBCreators;

  procedure ConfigCustomButtonStyle;
  var
    Style: TButtonStyle;
  begin
    Style := TButtonStyleManager
      .NewButtonStyle
      .WithFontColor(clYellow);

    TButtonStyleManager.RegisterCustomButtonStyle('fonte-amarela', Style);

    Style := TButtonStyleManager
      .NewButtonStyle
      .WithFontSize(18);

    TButtonStyleManager.RegisterCustomButtonStyle('big-font', Style);
  end;

begin
  ConfigCustomButtonStyle;

  Creators := TOPCBCreators.Create;
  try
    Creators.AsComponentCreator
      .SetOwner(Self)
      .Add(TComponentBuilder.Create(TImageList, FImages).Setup(@SetupMenuImages))
    ;

    Creators.AsControlCreator
      .SetTopLeft(5, 5)
      .SetSpace(5, 5)
      .SetControlWidthAndHeight(64, 64)
      .SetOwnerAndParent(Self, Self)
      .Add(TControlBuilder.Create(TSpeedButtonColor).Setup(@SetupMySpeedButtonGrayLight).WithCaption('1'))
      .Add(TControlBuilder.Create(TSpeedButtonColor).Setup(@SetupMySpeedButtonGrayLight).WithCaption('2'))
      .Add(TControlBuilder.Create(TSpeedButtonColor).Setup(@SetupMySpeedButtonGrayLight).WithCaption('3'))
      .Break
      .Add(TControlBuilder.Create(TSpeedButtonColor).Setup(@SetupMySpeedButtonGrayLight).WithCaption('4'))
      .Add(TControlBuilder.Create(TSpeedButtonColor).Setup(@SetupMySpeedButtonGrayLight).WithCaption('5'))
      .Add(TControlBuilder.Create(TSpeedButtonColor).Setup(@SetupMySpeedButtonGrayLight).WithCaption('6'))
      .Break
      .Add(TControlBuilder.Create(TSpeedButtonColor).Setup(@SetupMySpeedButtonGrayLight).WithCaption('7'))
      .Add(TControlBuilder.Create(TSpeedButtonColor).Setup(@SetupMySpeedButtonGrayLight).WithCaption('8'))
      .Add(TControlBuilder.Create(TSpeedButtonColor).Setup(@SetupMySpeedButtonGrayLight).WithCaption('9'))
      .Break
      .Add(TControlBuilder.Create(TSpeedButtonColor).Setup(@SetupMySpeedButtonGrayLight).WithCaption('0'))
      .UnsetControlWidthAndHeight
      .Break
      .Add(TControlBuilder.Create(TSpeedButtonColor).Setup(@SetupMySpeedButtonVermelho).WithCaption('Cancelar'))
      .Add(TControlBuilder.Create(TSpeedButtonColor).Setup(@SetupMySpeedButtonVerde).WithCaption('Enter'))
    ;
  finally
    Creators.Free;
  end;
end;

procedure TForm1.SpeedButton1MouseEnter(Sender: TObject);
begin

end;

procedure TForm1.SpeedButton1MouseLeave(Sender: TObject);
begin

end;

end.

