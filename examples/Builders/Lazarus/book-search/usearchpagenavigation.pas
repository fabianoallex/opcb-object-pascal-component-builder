unit USearchPageNavigation;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, StdCtrls, OPCB, Controls, UDialogs, Spin;

type
  TSearchPageNavigation = class;
  TSearchPageChangeEvent = procedure(ASearchPageNavigation: TSearchPageNavigation) of object;

  { TSearchPageNavigation }

  TSearchPageNavigation = class(TPanel)
  private
    FButtonFirst: TButton;
    FButtonLast: TButton;
    FButtonNext: TButton;
    FButtonPage: TButton;
    FButtonPrior: TButton;
    FLabelResults: TLabel;
    procedure SetupButton(AControl: TControl);
    procedure SetupSpinEdit(AControl: TControl);
  private
    FCurrentPage: Integer;
    FOnPageChange: TSearchPageChangeEvent;
    FPages: Integer;
    FPageSize: Integer;
    FResults: Integer;
    procedure SetCurrentPage(AValue: Integer);
    procedure SetOnPageChange(AValue: TSearchPageChangeEvent);
    procedure SetPageSize(AValue: Integer);
    procedure SetResults(AValue: Integer);
    procedure CalcPages;
    procedure ButtonFirstClick(ASender: TObject);
    procedure ButtonPriorClick(ASender: TObject);
    procedure ButtonPageClick(ASender: TObject);
    procedure ButtonNextClick(ASender: TObject);
    procedure ButtonLastClick(ASender: TObject);
    property LabelResults: TLabel read FLabelResults;
    property ButtonFirst: TButton read FButtonFirst;
    property ButtonPrior: TButton read FButtonPrior;
    property ButtonPage: TButton read FButtonPage;
    property ButtonNext: TButton read FButtonNext;
    property ButtonLast: TButton read FButtonLast;
  public
    constructor Create(AOwner: TComponent); override;
    property Results: Integer read FResults write SetResults;
    property PageSize: Integer read FPageSize write SetPageSize;
    property CurrentPage: Integer read FCurrentPage write SetCurrentPage;
    property Pages: Integer read FPages;
    property OnPageChange: TSearchPageChangeEvent read FOnPageChange write SetOnPageChange;
  end;

implementation

{ TSearchPageNavigation }

procedure TSearchPageNavigation.SetCurrentPage(AValue: Integer);
begin
  if AValue <= 0 then
    AValue := 1;

  if AValue > Pages then
    AValue := Pages;

  if FCurrentPage = AValue then
    Exit;

  FCurrentPage := AValue;
  ButtonPage.Caption := FCurrentPage.ToString;

  if Assigned(FOnPageChange) then
    FOnPageChange(Self);
end;

procedure TSearchPageNavigation.SetOnPageChange(AValue: TSearchPageChangeEvent);
begin
  if FOnPageChange = AValue then Exit;
  FOnPageChange := AValue;
end;

procedure TSearchPageNavigation.SetPageSize(AValue: Integer);
begin
  if FPageSize = AValue then Exit;
  FPageSize := AValue;

  CalcPages;
end;

procedure TSearchPageNavigation.SetResults(AValue: Integer);
begin
  if FResults < 0 then
    FResults := 0;

  if FResults = AValue then
    Exit;

  FResults := AValue;

  LabelResults.Caption := 'Resultados: ' + FResults.ToString;
  CalcPages;
end;

procedure TSearchPageNavigation.CalcPages;
begin
  if FPageSize <= 0 then
    FPageSize := 1;
  FPages := (FResults div FPageSize);
  if (FResults mod FPageSize) <> 0 then
    Inc(FPages);
  if FResults = 0 then
    FPages := 1;

  FButtonLast.Caption := FPages.ToString;
end;

procedure TSearchPageNavigation.ButtonFirstClick(ASender: TObject);
begin
  SetCurrentPage(1);
end;

procedure TSearchPageNavigation.ButtonPriorClick(ASender: TObject);
begin
  SetCurrentPage(CurrentPage-1);
end;

procedure TSearchPageNavigation.SetupSpinEdit(AControl: TControl);
begin
  (AControl as TSpinEdit).MinValue := 1;
  (AControl as TSpinEdit).MaxValue := Pages;
  (AControl as TSpinEdit).Value := CurrentPage;
end;

procedure TSearchPageNavigation.ButtonPageClick(ASender: TObject);
var
  Dialog: TControlDialog;
  SpinEdit: TSpinEdit;
begin
  Dialog := TControlDialog.CreateNew(
    Self,
    'Informe a página',
    TControlInfo.Create(TSpinEdit, SpinEdit).WithWidth(250).Setup(@SetupSpinEdit)
  );

  try
    if Dialog.ShowModal <> mrOk then
      Exit;
    CurrentPage := SpinEdit.Value;
  finally
    Dialog.Free;
  end;
end;

procedure TSearchPageNavigation.ButtonNextClick(ASender: TObject);
begin
  SetCurrentPage(CurrentPage+1);
end;

procedure TSearchPageNavigation.ButtonLastClick(ASender: TObject);
begin
  SetCurrentPage(Pages);
end;

procedure TSearchPageNavigation.SetupButton(AControl: TControl);
begin
  if Assigned(FButtonLast) then
  begin
    FButtonLast.OnClick := @ButtonLastClick;
    Exit;
  end;

  if Assigned(FButtonNext) then
  begin
    FButtonNext.OnClick := @ButtonNextClick;
    Exit;
  end;

  if Assigned(FButtonPage) then
  begin
    FButtonPage.OnClick := @ButtonPageClick;
    Exit;
  end;

  if Assigned(FButtonPrior) then
  begin
    FButtonPrior.OnClick := @ButtonPriorClick;
    Exit;
  end;

  if Assigned(FButtonFirst) then
  begin
    FButtonFirst.OnClick := @ButtonFirstClick;
    Exit;
  end;
end;

constructor TSearchPageNavigation.Create(AOwner: TComponent);
var
  ControlBuilder: TControlBuilder;
begin
  inherited Create(AOwner);
  ControlBuilder := TControlBuilder.Create;

  try
    ControlBuilder
      .WithOwnerAndParent(Self, Self)
      .SetSpace(5, 5)
      .AddControl(TControlInfo.Create(TLabel, FLabelResults).WithWidth(40).WithCaption('Resultados'))
      .Break
      .AddControl(TControlInfo.Create(TButton, FButtonFirst).WithWidth(40).WithCaption('1').Setup(@SetupButton))
      .AddControl(TControlInfo.Create(TButton, FButtonPrior).WithWidth(40).WithCaption('<').Setup(@SetupButton))
      .IncLeft(10)
      .AddControl(TControlInfo.Create(TButton, FButtonPage).WithWidth(40).WithCaption('1').Setup(@SetupButton))
      .IncLeft(10)
      .AddControl(TControlInfo.Create(TButton, FButtonNext).WithWidth(40).WithCaption('>').Setup(@SetupButton))
      .AddControl(TControlInfo.Create(TButton, FButtonLast).WithWidth(40).WithCaption('99').Setup(@SetupButton))
    ;
  finally
    ControlBuilder.Free;
  end;

  FPageSize := 1;
  FCurrentPage := 1;
  FResults := 0;
  CalcPages;
end;

end.

