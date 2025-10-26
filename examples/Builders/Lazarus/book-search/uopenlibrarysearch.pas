unit UOpenLibrarySearch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, {$IFDEF WINDOWS}Windows, WinInet,{$ENDIF} fpjson, fphttpclient, jsonparser;

type
  // Eventos
  TOnSearchStartEvent = procedure(Sender: TObject; const AQuery: string) of object;
  TOnSearchReturn = procedure(Sender: TObject; const AResultsFound: Integer) of object;
  TOnDocFoundEvent = procedure(Sender: TObject; ADoc: TJSONObject) of object;
  TOnSearchFinishedEvent = procedure(Sender: TObject; const ATotal: Integer) of object;

  { TOpenLibrarySearch }

  TOpenLibrarySearch = class(TComponent)
  private
    FLimit: Integer;
    FOffset: Integer;
    FOnSearchReturn: TOnSearchReturn;
    FOnSearchStart: TOnSearchStartEvent;
    FOnDocFound: TOnDocFoundEvent;
    FOnSearchFinished: TOnSearchFinishedEvent;
    FQuery: string;
    function HttpGet(const AUrl: string): string;
    procedure SetOnSearchReturn(AValue: TOnSearchReturn);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Search(const q: string; AOffset: Integer; ALimit: Integer);
  published
    property OnSearchStart: TOnSearchStartEvent read FOnSearchStart write FOnSearchStart;
    property OnDocFound: TOnDocFoundEvent read FOnDocFound write FOnDocFound;
    property OnSearchFinished: TOnSearchFinishedEvent read FOnSearchFinished write FOnSearchFinished;
    property OnSearchReturn: TOnSearchReturn read FOnSearchReturn write SetOnSearchReturn;
    property Query: string read FQuery;
    property Offset: Integer read FOffset;
    property Limit: Integer read FLimit;
  end;

implementation

const
  BASE_URL = 'https://openlibrary.org/search.json?q=%s&offset=%d&limit=%d';

{ TOpenLibrarySearch }

constructor TOpenLibrarySearch.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

function TOpenLibrarySearch.HttpGet(const AUrl: string): string;
{$IFDEF WINDOWS}
var
  hInet, hUrl: HINTERNET;
  Buffer: array[0..1023] of Byte;
  BytesRead: DWORD;
  S: TStringStream;
begin
  Result := '';
  hInet := InternetOpen('LazarusWinInet', INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  if hInet = nil then
    RaiseLastOSError;

  try
    hUrl := InternetOpenUrl(hInet, PChar(AUrl), nil, 0, INTERNET_FLAG_RELOAD, 0);
    if hUrl = nil then
      RaiseLastOSError;

    S := TStringStream.Create('');
    try
      repeat
        if not InternetReadFile(hUrl, @Buffer, SizeOf(Buffer), BytesRead) then
          RaiseLastOSError;
        if BytesRead > 0 then
          S.Write(Buffer, BytesRead);
      until BytesRead = 0;

      Result := S.DataString;
    finally
      S.Free;
      InternetCloseHandle(hUrl);
    end;
  finally
    InternetCloseHandle(hInet);
  end;
{$ELSE}
var
  HTTPClient: TFPHttpClient;
  S: TStringStream;
begin
  Result := '';
  HTTPClient := TFPHttpClient.Create(nil);
  S := TStringStream.Create('');
  try
    try
      HTTPClient.AllowRedirect := True;
      HTTPClient.Get(AUrl, S);
      Result := S.DataString;
    except
      on E: Exception do
        raise;
    end;
  finally
    S.Free;
    HTTPClient.Free;
  end;
{$ENDIF}
end;

procedure TOpenLibrarySearch.SetOnSearchReturn(AValue: TOnSearchReturn);
begin
  if FOnSearchReturn = AValue then Exit;
  FOnSearchReturn := AValue;
end;

procedure TOpenLibrarySearch.Search(const q: string; AOffset: Integer; ALimit: Integer);
var
  url, rawJson: string;
  jsonData: TJSONData;
  jsonObj: TJSONObject;
  docs: TJSONArray;
  i, totalProcessed: Integer;
begin
  FQuery := q;
  FOffset := AOffset;
  FLimit := ALimit;

  if Assigned(FOnSearchStart) then
    FOnSearchStart(Self, q);

  url := Format(BASE_URL, [EncodeURLElement(q), AOffset, ALimit]);
  rawJson := HttpGet(url);

  if (rawJson = '') or (rawJson[1] <> '{') then
    raise Exception.Create('Resposta inesperada: ' + Copy(rawJson, 1, 200));

  totalProcessed := 0;
  jsonData := GetJSON(rawJson);
  try
    if not (jsonData is TJSONObject) then
      raise Exception.Create('Resposta não é um objeto JSON');

    jsonObj := TJSONObject(jsonData);

    if Assigned(FOnSearchReturn) then
      FOnSearchReturn(Self, jsonObj.FindPath('numFound').AsInteger);

    docs := TJSONArray(jsonObj.FindPath('docs'));
    if docs <> nil then
    begin
      for i := 0 to docs.Count - 1 do
      begin
        if Assigned(FOnDocFound) then
          FOnDocFound(Self, TJSONObject(docs.Items[i]));
      end;
      totalProcessed := docs.Count;
    end;
  finally
    jsonData.Free;
  end;

  if Assigned(FOnSearchFinished) then
    FOnSearchFinished(Self, totalProcessed);
end;

end.

