unit UBookCard;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, StdCtrls, OPCB, Controls, Forms;

type

  { TImageLoaderThread }

  TImageLoaderThread = class(TThread)
  private
    FURL: string;
    FImage: TImage;
    FStream: TMemoryStream;
  protected
    procedure Execute; override;
    procedure DoLoadImage;
  public
    constructor Create(const AURL: string; AImage: TImage);
    destructor Destroy; override;
  end;

  { TBookCard }

  TBookCard = class(TScrollBox)
  private
    FImage: TImage;
    FLabelAuthor: TLabel;
    FLabelBookURL: TLabel;
    FLabelFirstEditionYear: TLabel;
    FLabelTitle: TLabel;
    procedure LabelBookURLClick(ASender: TObject);
    procedure SetupImage(AControl: TControl);
    procedure SetupLabel(AControl: TControl);
  private
    FAuthor: string;
    FBookURL: string;
    FFirstEditionYear: Integer;
    FTitle: string;
    FImageURL: string;
    procedure SetAuthor(AValue: string);
    procedure SetBookURL(AValue: string);
    procedure SetFirstEditionYear(AValue: Integer);
    procedure SetTitle(AValue: string);
    procedure SetImageURL(AValue: string);
    property Image: TImage read FImage;
    property LabelTitle: TLabel read FLabelTitle;
    property LabelAuthor: TLabel read FLabelAuthor;
    property LabelBookURL: TLabel read FLabelBookURL;
    property LabelFirstEditionYear: TLabel read FLabelFirstEditionYear;
  public
    constructor Create(AOwner: TComponent); override;
    property Title: string read FTitle write SetTitle;
    property Author: string read FAuthor write SetAuthor;
    property FirstEditionYear: Integer read FFirstEditionYear write SetFirstEditionYear;
    property ImageURL: string read FImageURL write SetImageURL;
    property BookURL: string read FBookURL write SetBookURL;
  end;

implementation

uses
  Windows, WinInet, LCLType, LCLProc, LCLIntf, Graphics;

{ TImageLoaderThread }

function DownloadURL(const Url: string; Stream: TStream): Boolean;
var
  hInet, hFile: HINTERNET;
  Buffer: array[0..1023] of Byte;
  BytesRead: DWORD;
begin
  Result := False;
  hInet := InternetOpen('LazarusApp', INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  if Assigned(hInet) then
  try
    hFile := InternetOpenUrl(hInet, PChar(Url), nil, 0, INTERNET_FLAG_RELOAD, 0);
    if Assigned(hFile) then
    try
      repeat
        InternetReadFile(hFile, @Buffer, SizeOf(Buffer), BytesRead);
        if BytesRead > 0 then
          Stream.WriteBuffer(Buffer, BytesRead);
      until BytesRead = 0;
      Result := True;
    finally
      InternetCloseHandle(hFile);
    end;
  finally
    InternetCloseHandle(hInet);
  end;
end;

procedure LoadImageFromURL(const AURL: string; AImage: TImage);
var
  MS: TMemoryStream;
begin
  MS := TMemoryStream.Create;
  try
    if DownloadURL(AURL, MS) then
    begin
      MS.Position := 0;
      AImage.Picture.LoadFromStream(MS);
    end;
  finally
    MS.Free;
  end;
end;

procedure TImageLoaderThread.Execute;
begin
  if DownloadURL(FURL, FStream) then
  begin
    FStream.Position := 0;
    Synchronize(@DoLoadImage);
  end;
end;

procedure TImageLoaderThread.DoLoadImage;
begin
  if Assigned(FImage) then
    FImage.Picture.LoadFromStream(FStream);
end;

constructor TImageLoaderThread.Create(const AURL: string; AImage: TImage);
begin
  inherited Create(False); // já inicia a thread
  FreeOnTerminate := True; // libera memória automaticamente
  FURL := AURL;
  FImage := AImage;
  FStream := TMemoryStream.Create;
end;

destructor TImageLoaderThread.Destroy;
begin
  FStream.Free;
  inherited Destroy;
end;

{ TBookCard }

procedure TBookCard.SetAuthor(AValue: string);
begin
  if FAuthor = AValue then Exit;
  FAuthor := AValue;
  LabelAuthor.Caption := AValue;
end;

procedure TBookCard.SetBookURL(AValue: string);
begin
  if FBookURL = AValue then Exit;
  FBookURL := AValue;
end;

procedure TBookCard.SetFirstEditionYear(AValue: Integer);
begin
  if FFirstEditionYear = AValue then Exit;
  FFirstEditionYear := AValue;
  LabelFirstEditionYear.Caption := 'Primeira Edição: ' + AValue.ToString;
end;

procedure TBookCard.SetTitle(AValue: string);
begin
  if FTitle = AValue then Exit;
  FTitle := AValue;
  LabelTitle.Caption := AValue;
end;

procedure TBookCard.SetImageURL(AValue: string);
begin
  if FImageURL = AValue then Exit;
  FImageURL := AValue;

  //LoadImageFromURL(AValue, Image);    // sem thread
  TImageLoaderThread.Create(AValue, Image);
end;

procedure TBookCard.LabelBookURLClick(ASender: TObject);
begin
  if FBookURL <> '' then
    OpenURL(FBookURL);
end;

procedure TBookCard.SetupLabel(AControl: TControl);
begin
  (AControl as TLabel).WordWrap := True;
  (AControl as TLabel).BorderSpacing.Around := 10;

  if (AControl = FLabelTitle) then
    AControl.Font.Size := 15
  else
    AControl.Font.Size := 12;

  if (AControl = FLabelBookURL) then
  begin
    (AControl as TLabel).Caption := '🔗 Ver mais';
    (AControl as TLabel).Font.Color := clBlue;
    (AControl as TLabel).Font.Style := [fsUnderline];
    (AControl as TLabel).Cursor := crHandPoint;
    (AControl as TLabel).OnClick := @LabelBookURLClick;
  end;
end;

procedure TBookCard.SetupImage(AControl: TControl);
var
  Img: TImage;
begin
  Img := (AControl as TImage);
  Img.Proportional := True;
  Img.Align := alClient;
  Img.BorderSpacing.Around := 10;
  Img.Stretch := True;
end;

constructor TBookCard.Create(AOwner: TComponent);
var
  ControlBuilder: TControlsBuilder;
begin
  inherited Create(AOwner);
  Self.HorzScrollBar.Visible := False;
  ControlBuilder := TControlsBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(Self, Self)
      .SetTopLeft(10, 10)
      .SetSpace(5, 5)
      .SubLevel(TControlInfo.Create(TPanel).WithAlign(alLeft).WithWidth(150))
        .AddControl(TControlInfo.Create(TImage, 'Image', FImage).Setup(@SetupImage))
      .SuperLevel
      .SubLevel(TControlInfo.Create(TPanel, 'PanelClient').WithAlign(alClient).WithCaption(''))
        .SetTopLeftNearControl('Image', rpRight)
        .SetDirection(cpdVertical)
        .AddControl(TControlInfo.Create(TLabel, FLabelTitle).WithAlign(alTop).Setup(@SetupLabel))
        .AddControl(TControlInfo.Create(TLabel, FLabelAuthor).WithAlign(alTop).Setup(@SetupLabel))
        .AddControl(TControlInfo.Create(TLabel, FLabelFirstEditionYear).WithAlign(alTop).Setup(@SetupLabel))
        .AddControl(TControlInfo.Create(TLabel, FLabelBookURL).WithAlign(alTop).Setup(@SetupLabel))
        .RecalcParentHeight()
      .SuperLevel
    ;
  finally
    ControlBuilder.Free;
  end;
end;

end.

