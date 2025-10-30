unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, OPCB;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FCardsCreator: TControlCreator;
    procedure SetupImage(AControl: TControl);
    procedure AddCard;
    procedure ButtonAddCardClick(ASender: TObject);
    procedure SetCardsCreator(const Value: TControlCreator);
  public
    property CardsCreator: TControlCreator read FCardsCreator write SetCardsCreator;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  System.Net.HttpClientComponent, Vcl.Imaging.pngimage;

procedure LoadImageFromURL(const AURL: string; AImage: TImage);
var
  Http: TNetHTTPClient;
  Stream: TMemoryStream;
  Png: TPngImage;
begin
  Http := TNetHTTPClient.Create(nil);
  Stream := TMemoryStream.Create;
  try
    Http.Get(AURL, Stream);
    Stream.Position := 0;

    if LowerCase(ExtractFileExt(AURL)) = '.png' then
    begin
      Png := TPngImage.Create;
      try
        Png.LoadFromStream(Stream);
        AImage.Picture.Assign(Png);
      finally
        Png.Free;
      end;
    end
    else
      AImage.Picture.LoadFromStream(Stream);

  finally
    Stream.Free;
    Http.Free;
  end;
end;

procedure TForm1.SetCardsCreator(const Value: TControlCreator);
begin
  FCardsCreator := Value;
end;

procedure TForm1.SetupImage(AControl: TControl);
var
  Image: TImage;
begin
  Image := (AControl as TImage);
  Image.Stretch := True;
  Image.Width := 200;
  Image.Height := 200;
  LoadImageFromURL('https://i.pravatar.cc/300', Image);
end;

procedure TForm1.AddCard;
begin
  CardsCreator
    .SubLevel(TControlBuilder.Create(TPanel).WithWidthAndHeight(250, 350), cpdVertical)
      .SetVerticalSpace(2)
      .Add(TControlBuilder.Create(TImage).Setup(SetupImage))
      .CenterControlInParentHorizontally
      .Add(TControlBuilder.Create(TLabel).WithCaption('Nome'))
      .CenterControlInParentHorizontally
      .Add(TControlBuilder.Create(TLabel).WithCaption('Contato'))
      .CenterControlInParentHorizontally
    .SuperLevel
  ;
end;

procedure TForm1.ButtonAddCardClick(ASender: TObject);
begin
  AddCard;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  Creators: TOPCBCreators;
begin
  Creators := TOPCBCreators.Create(Self.Name);
  try
    Creators.AsControlCreator
      .SetOwnerAndParent(Self, Self)
      .SetTopLeft(10, 10)
      .SetSpace(5, 5)
      .SetDirection(cpdVertical)
      .SubLevel(TControlBuilder.Create(TPanel).WithAlign(alTop))
        .SetTopLeft(10, 10)
        .Add(TControlBuilder.Create(TButton)
          .WithCaption('Novo Card')
          .WithOnClick(ButtonAddCardClick)
        )
      .SuperLevel
      .SetDirection(cpdHorizontal)
      .Add(TControlBuilder.Create(TFlowPanel, 'FlowCards').WithAlign(alClient).WithCaption(''))
    ;

    CardsCreator := TControlCreator.Create(Self.Name);

    CardsCreator
      .SetOwnerAndParent(
        Self,
        Creators.AsControlCreator.GetControl<TFlowPanel>('FlowCards')
      )
    ;
  finally
    Creators.Free;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  CardsCreator.Free;
end;

end.
