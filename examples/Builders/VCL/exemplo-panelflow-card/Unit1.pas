unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, OPCB;

type
  TForm1 = class(TForm)
    FlowPanel1: TFlowPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FBuilderCards: TControlBuilder;
    procedure SetupImage(AControl: TControl);
    procedure AddCard;
    procedure ButtonAddCardClick(ASender: TObject);
    procedure SetBuilderCards(const Value: TControlBuilder);
  public
    property BuilderCards: TControlBuilder read FBuilderCards write SetBuilderCards;
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

procedure TForm1.SetBuilderCards(const Value: TControlBuilder);
begin
  FBuilderCards := Value;
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
  BuilderCards
    .NextLevel(TControlInfo.Create(TPanel).WithWidthAndHeight(250, 350), cpdVertical)
      .SetVerticalSpace(2)
      .AddControl(TControlInfo.Create(TImage).Setup(SetupImage))
      .CenterControlInParentHorizontally
      .AddControl(TControlInfo.Create(TLabel).WithCaption('Nome'))
      .CenterControlInParentHorizontally
      .AddControl(TControlInfo.Create(TLabel).WithCaption('Contato'))
      .CenterControlInParentHorizontally
    .PreviousLevel
  ;
end;

procedure TForm1.ButtonAddCardClick(ASender: TObject);
begin
  AddCard;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  OPCBBuilders: TOPCBBuilders;
begin
  OPCBBuilders := TOPCBBuilders.Create(Self.Name);
  try
    OPCBBuilders.AsControlBuilder
      .WithOwnerAndParent(Self, Self)
      .SetTopLeft(10, 10)
      .SetSpace(5, 5)
      .SetDirection(cpdVertical)
      .NextLevel(TControlInfo.Create(TPanel).WithAlign(alTop))
        .SetTopLeft(10, 10)
        .AddControl(TControlInfo.Create(TButton)
          .WithCaption('Novo Card')
          .WithOnClick(ButtonAddCardClick)
        )
      .PreviousLevel
      .SetDirection(cpdHorizontal)
      .AddControl(TControlInfo.Create(TFlowPanel, 'FlowCards').WithAlign(alClient).WithCaption(''))
    ;

    BuilderCards := TControlBuilder.Create(Self.Name);

    BuilderCards
      .WithOwnerAndParent(
        Self,
        OPCBBuilders.AsControlBuilder.GetControl<TFlowPanel>('FlowCards')
      )
    ;
  finally
    OPCBBuilders.Free;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  BuilderCards.Free;
end;

end.
