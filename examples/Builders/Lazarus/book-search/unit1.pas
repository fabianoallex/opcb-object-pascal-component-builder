unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, OPCB, ExtCtrls,
  StdCtrls, RTTICtrls, USearchPageNavigation, UOpenLibrarySearch, fpjson,
  jsonparser, UBookCard, ComCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
  private
    FComboBoxPageSize: TComboBox;
    FControlBuilderPage: TControlCreator;
    FEditSearch: TEdit;
    FMemo: TMemo;
    FOpenLibrarySearch: TOpenLibrarySearch;
    FPanelCards: TPanel;
    FProgressBar: TProgressBar;
    FScrollboxPage: TScrollbox;
    FSearchPageNavigation: TSearchPageNavigation;
    procedure OnDoc(Sender: TObject; ADoc: TJSONObject);
    procedure OnFinish(Sender: TObject; const ATotal: Integer);
    procedure OnStart(Sender: TObject; const AQuery: string);
    procedure OnReturn(Sender: TObject; const AFound: Integer);
    procedure SeTControlCreatorPage(AValue: TControlCreator);
    procedure SetOpenLibrarySearch(AValue: TOpenLibrarySearch);
    procedure SetupComboBoxPageSize(AControl: TControl);
    procedure SetupProgressBar(AControl: TControl);
    procedure SetupScrollBoxPage(AControl: TControl);
  published
    procedure FormCreate(Sender: TObject);
    property OpenLibrarySearch: TOpenLibrarySearch read FOpenLibrarySearch write SetOpenLibrarySearch;
    property SearchPageNavigation: TSearchPageNavigation read FSearchPageNavigation;
    property EditSearch: TEdit read FEditSearch;
    property Memo: TMemo read FMemo;
    property ComboBoxPageSize: TComboBox read FComboBoxPageSize;
    property PanelCards: TPanel read FPanelCards;
    property ScrollboxPage: TScrollbox read FScrollboxPage;
    property ProgressBar: TProgressBar read FProgressBar;
    property ControlBuilderPage: TControlCreator read FControlBuilderPage write SeTControlCreatorPage;
  private
    procedure ButtonSearchClick(ASender: TObject);
    procedure SearchNavigationPageChange(ANavigation: TSearchPageNavigation);
    procedure SetupButtonSearch(AControl: TControl);
    procedure SetupSearchNavigation(AControl: TControl);
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.SearchNavigationPageChange(ANavigation: TSearchPageNavigation);
begin
  if not OpenLibrarySearch.Query.Trim.IsEmpty then
  begin
    OpenLibrarySearch.Search(
      OpenLibrarySearch.Query,
      OpenLibrarySearch.Limit * (ANavigation.CurrentPage-1),
      OpenLibrarySearch.Limit
    );
  end;
end;

procedure TForm1.SetupSearchNavigation(AControl: TControl);
begin
  (AControl as TSearchPageNavigation).Results := 1;
  (AControl as TSearchPageNavigation).PageSize := 1;
  (AControl as TSearchPageNavigation).OnPageChange := @SearchNavigationPageChange;
  (AControl as TSearchPageNavigation).BevelOuter := bvNone;
end;

procedure TForm1.ButtonSearchClick(ASender: TObject);
begin
  SearchPageNavigation.Results := 0;
  SearchPageNavigation.CurrentPage := 1;
  OpenLibrarySearch.Search(
    EditSearch.Text, 0,
    ComboBoxPageSize.Items[ComboBoxPageSize.ItemIndex].ToInteger
  );
end;

procedure TForm1.SetupButtonSearch(AControl: TControl);
begin
  (AControl as TButton).OnClick := @ButtonSearchClick;
end;

procedure TForm1.SetOpenLibrarySearch(AValue: TOpenLibrarySearch);
begin
  if FOpenLibrarySearch = AValue then Exit;
  FOpenLibrarySearch := AValue;
end;

procedure TForm1.OnStart(Sender: TObject; const AQuery: string);
begin
  Memo.Lines.Add('🔍 Iniciando busca por: ' + AQuery);
  ProgressBar.Position := 0;
  ProgressBar.Visible := True;
end;

procedure TForm1.OnReturn(Sender: TObject; const AFound: Integer);
begin
  Memo.Lines.Add('🔍 Encontrou : ' + AFound.ToString + ' Livro(s)');

  SearchPageNavigation.Results := AFound;
  SearchPageNavigation.PageSize := OpenLibrarySearch.Limit;

  if Assigned(ControlBuilderPage) then
    ControlBuilderPage.Free;

  // ao gerar uma nova página, destroi a anterior.
  if Assigned(FScrollboxPage) then
    FScrollboxPage.Free;

  // o builder é instanciado (aqui) no evento OnSearchReturn do objeto OpenLibrarySearch
  // e destruido no evento OnSearchFinished.
  // Durante os eventos OnDocFound serão instanciados os Cards

  ControlBuilderPage := TControlCreator.Create;
  ControlBuilderPage
    .WithOwnerAndParent(Self, FPanelCards)
    .SubLevel(TControlBuilder.Create(TScrollBox, FScrollboxPage).WithAlign(alClient).Setup(@SetupScrollBoxPage))
  ;
end;

procedure TForm1.OnDoc(Sender: TObject; ADoc: TJSONObject);
var
  Card: TBookCard;

  function GetAuthors: string;
  var
    arr: TJSONArray;
    i: Integer;
    authors: TStringList;
  begin
    Result := '';
    if not Assigned(ADoc.FindPath('author_name')) then
      Exit;

    arr := TJSONArray(ADoc.FindPath('author_name'));
    if arr <> nil then
    begin
      authors := TStringList.Create;
      try
        for i := 0 to arr.Count - 1 do
          authors.Add(arr.Strings[i]);
        Result := StringReplace(authors.CommaText, ',', ', ', [rfReplaceAll]);
      finally
        authors.Free;
      end;
    end;
  end;

  procedure ConfiguCard;
  begin
    if Assigned(ADoc.FindPath('title')) then
      Card.Title := ADoc.FindPath('title').AsString;

    Card.Author := GetAuthors;

    if Assigned(ADoc.FindPath('first_publish_year')) then
      Card.FirstEditionYear := ADoc.FindPath('first_publish_year').AsInteger;
    if Assigned(ADoc.FindPath('cover_i')) then
      Card.ImageURL := Format('https://covers.openlibrary.org/b/id/%d-M.jpg', [ADoc.FindPath('cover_i').AsInteger]);
    if Assigned(ADoc.FindPath('key')) then
      Card.BookURL := 'https://openlibrary.org' + ADoc.FindPath('key').AsString;
  end;

begin
  Memo.Lines.Add('📚 Livro: ' + ADoc.FindPath('title').AsString);
  ControlBuilderPage
    .SubLevel(TControlBuilder.Create(TBookCard, Card).WithCaption(ADoc.FindPath('title').AsString).WithAlign(alTop).WithHeight(180))
    .SuperLevel
  ;
  ConfiguCard;

  ProgressBar.Position := ProgressBar.Position
    + ProgressBar.Max div OpenLibrarySearch.Limit;
end;

procedure TForm1.OnFinish(Sender: TObject; const ATotal: Integer);
begin
  Memo.Lines.Add('✅ Busca finalizada. Total de docs recebidos: ' + ATotal.ToString);

  ControlBuilderPage.Free;
  ControlBuilderPage := nil;

  ProgressBar.Visible := False;
end;

procedure TForm1.SeTControlCreatorPage(AValue: TControlCreator);
begin
  if FControlBuilderPage = AValue then Exit;
  FControlBuilderPage := AValue;
end;

procedure TForm1.SetupScrollBoxPage(AControl: TControl);
begin
  FScrollboxPage.VertScrollBar.Increment := 128;
  FScrollboxPage.VertScrollBar.Smooth := True;
end;

procedure TForm1.SetupComboBoxPageSize(AControl: TControl);
begin
  ComboBoxPageSize.Items.AddCommaText('3,5,10,20,50,100');
  ComboBoxPageSize.ItemIndex := 0;
  ComboBoxPageSize.Style := csDropDownList;
end;

procedure TForm1.SetupProgressBar(AControl: TControl);
begin
  ProgressBar.Smooth := True;
  ProgressBar.Height := 4;
  ProgressBar.Visible := False;;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  ControlBuilder: TControlCreator;
begin
  OpenLibrarySearch := TOpenLibrarySearch.Create(Self);
  OpenLibrarySearch.OnSearchStart := @OnStart;
  OpenLibrarySearch.OnDocFound := @OnDoc;
  OpenLibrarySearch.OnSearchFinished := @OnFinish;
  OpenLibrarySearch.OnSearchReturn := @OnReturn;

  ControlBuilder := TControlCreator.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(Self, Self)
      .SetSpace(5, 5)
      .SubLevel(TControlBuilder.Create(TPanel).WithCaption('').WithAlign(alTop))
        .SetTopLeft(10, 10)
        .SubLevel(cpdVertical)
          .AddControl(TControlBuilder.Create(TLabel).WithCaption('Pesquisa de Livros na Open Library'))
          .SetDirection(cpdHorizontal)
          .AddControl(TControlBuilder.Create(TEdit, FEditSearch).WithWidth(220).WithText('Object Pascal'))
          .AddControl(TControlBuilder.Create(TComboBox, FComboBoxPageSize).WithWidth(70).Setup(@SetupComboBoxPageSize))
          .AddControl(TControlBuilder.Create(TButton).WithCaption('Pesquisar').Setup(@SetupButtonSearch))
        .SuperLevel
        .IncLeft(50)
        .AddControl(TControlBuilder.Create(TSearchPageNavigation, FSearchPageNavigation).WithWidth(300).Setup(@SetupSearchNavigation))
        .RecalcParentHeight(10)
      .SuperLevel
      .SubLevel(TControlBuilder.Create(TPanel).WithCaption('').WithAlign(alClient))
        .AddControl(TControlBuilder.Create(TPanel, FPanelCards).WithAlign(alClient))
        .AddControl(TControlBuilder.Create(TMemo, FMemo).WithAlign(alBottom))
        .AddControl(TControlBuilder.Create(TProgressBar, FProgressBar).WithAlign(alBottom).Setup(@SetupProgressBar))
      .SuperLevel
    ;
  finally
    ControlBuilder.Free;
  end;
end;

end.

