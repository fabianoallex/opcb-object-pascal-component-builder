unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, OPCB,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FBuilderCards: TControlBuilder;
    procedure SetBuilderCards(const Value: TControlBuilder);
    procedure SetupImage(AControl: TControl);
    procedure AddCard;
    procedure ButtonAddCardClick(ASender: TObject);
    procedure SetupLabel(AControl: TControl);
  public
    property BuilderCards: TControlBuilder read FBuilderCards write SetBuilderCards;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

{ TForm1 }

uses
  System.Net.HttpClientComponent, FMX.Objects;

procedure LoadImageFromURL(const AURL: string; AImage: TImage);
var
  Http: TNetHTTPClient;
  Stream: TMemoryStream;
begin
  Http := TNetHTTPClient.Create(nil);
  Stream := TMemoryStream.Create;
  try
    Http.Get(AURL, Stream);
    Stream.Position := 0;
    AImage.Bitmap.LoadFromStream(Stream); // no FMX é sempre Bitmap
  finally
    Stream.Free;
    Http.Free;
  end;
end;

procedure TForm1.SetupImage(AControl: TControl);
var
  Image: TImage;
begin
  Image := (AControl as TImage);
  Image.Width := 200;
  Image.Height := 200;
  LoadImageFromURL('https://i.pravatar.cc/300', Image);
end;

procedure TForm1.SetupLabel(AControl: TControl);
var
  Lbl: TLabel;
begin
  Lbl := AControl as TLabel;
  Lbl.AutoSize := True;
  Lbl.TextAlign := TTextAlign.Center;
end;

procedure TForm1.AddCard;
begin
  BuilderCards
    .NextLevel(TControlInfo.Create(TPanel).WithWidthAndHeight(250, 350), cpdVertical)
      .SetVerticalSpace(2)
      .AddControl(TControlInfo.Create(TImage).Setup(SetupImage))
      .CenterControlInParentHorizontally
      .AddControl(TControlInfo.Create(TLabel).WithCaption('Nome').Setup(SetupLabel))
      .CenterControlInParentHorizontally
      .AddControl(TControlInfo.Create(TLabel).WithCaption('Contato').Setup(SetupLabel))
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
      .NextLevel(TControlInfo.Create(TPanel).WithAlign(TAlignLayout.Top).WithHeight(50))
        .SetTopLeft(10, 10)
        .AddControl(TControlInfo.Create(TButton)
          .WithCaption('Novo Card')
          .WithOnClick(ButtonAddCardClick)
        )
      .PreviousLevel
      .SetDirection(cpdHorizontal)
      .AddControl(TControlInfo.Create(TFlowLayout, 'FlowCards').WithAlign(TAlignLayout.Client).WithCaption(''))
    ;

    BuilderCards := TControlBuilder.Create(Self.Name);

    BuilderCards
      .WithOwnerAndParent(
        Self,
        OPCBBuilders.AsControlBuilder.GetControl<TFlowLayout>('FlowCards')
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

procedure TForm1.SetBuilderCards(const Value: TControlBuilder);
begin
  FBuilderCards := Value;
end;

end.
