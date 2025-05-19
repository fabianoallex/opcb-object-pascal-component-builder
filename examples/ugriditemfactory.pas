unit UGridItemFactory;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ULayout, Controls, UGridText, UGridHtmlTable;

type
  IHtmlTableGridItemBuilder = interface
    ['{47B2A165-22AC-457E-A5C7-73F7DFFE0492}']
    function WithStrContent(AContent: string): IHtmlTableGridItemBuilder;
    function WithCellSettings(ACellSettings: TGridCellSettings): IHtmlTableGridItemBuilder;
    function AddToGrid(AGrid: TGridLayout): IHtmlTableGridItemBuilder;
    function AddWithFiller(AFiller: IGridFill): IHtmlTableGridItemBuilder;
  end;

  IControlGridItemBuilder = interface
    ['{47B2A165-22AC-457E-A5C7-73F7DFFE0492}']
    function WithControl(AControl: TControl): IControlGridItemBuilder;
    function AddToGrid(AGrid: TGridLayout): IControlGridItemBuilder;
    function AddWithFiller(AFiller: IGridFill): IControlGridItemBuilder;
  end;

  ITextGridItemBuilder = interface
    ['{1B5F445E-0561-4130-AB0F-D8187D29C43F}']
    function NewTextItem: ITextGridItemBuilder;
    function WithText(AText: string): ITextGridItemBuilder;
    function WithHorizontalAlign(AHorizontalAlign: TTextAlignHorizontal): ITextGridItemBuilder;
    function WithVerticalAlign(AVerticalAlign: TTextAlignVertical): ITextGridItemBuilder;
    function WithCellSettings(ACellSettings: TGridCellSettings): ITextGridItemBuilder;
    function WithAlignment(AHorizontal: TTextAlignHorizontal; AVertical: TTextAlignVertical): ITextGridItemBuilder;
    function AddToGrid(AGrid: TGridLayout): ITextGridItemBuilder; overload;
    function AddToGrid(AGrid: TGridLayout; ARow, AColumn: Integer): ITextGridItemBuilder; overload;
  end;

  IGridItemFactory = interface
    ['{BE27E7F4-BFD1-4BD8-8CAD-0C1A7B2B99E1}']
    function BuildTextItem(ARenderer: TGridTextRenderer): ITextGridItemBuilder;
    function BuildControlItem: IControlGridItemBuilder;
    function BuildHtmlTableItem(ARenderer: TGridHtmlTableRenderer): IHtmlTableGridItemBuilder;
  end;

  { TGridItemFactory }

  TGridItemFactory = class(TInterfacedObject, IGridItemFactory)
  public
    function BuildTextItem(ARenderer: TGridTextRenderer): ITextGridItemBuilder;
    function BuildControlItem: IControlGridItemBuilder;
    function BuildHtmlTableItem(ARenderer: TGridHtmlTableRenderer): IHtmlTableGridItemBuilder;
  end;

  { TControlGridItemBuilder }

  TControlGridItemBuilder = class(TInterfacedObject, IControlGridItemBuilder)
  private
    FControl: TControl;
    FCellSettings: TGridCellSettings;
  protected
    constructor Create;
  public
    function WithControl(AControl: TControl): IControlGridItemBuilder;
    function WithCellSettings(ACellSettings: TGridCellSettings): IControlGridItemBuilder;
    function AddToGrid(AGrid: TGridLayout): IControlGridItemBuilder;
    function AddWithFiller(AFiller: IGridFill): IControlGridItemBuilder;
  end;

  { THtmlTableGridItemBuilder }

  THtmlTableGridItemBuilder = class(TInterfacedObject, IHtmlTableGridItemBuilder)
  private
    FRenderer: TGridHtmlTableRenderer;
    FElement: THtmlTableVisualElement;
    FCellSettings: TGridCellSettings;
  protected
    constructor Create(ARenderer: TGridHtmlTableRenderer);
  public
    function WithStrContent(AContent: string): IHtmlTableGridItemBuilder;
    function WithCellSettings(ACellSettings: TGridCellSettings): IHtmlTableGridItemBuilder;
    function AddToGrid(AGrid: TGridLayout): IHtmlTableGridItemBuilder;
    function AddWithFiller(AFiller: IGridFill): IHtmlTableGridItemBuilder;
  end;

  { TTextGridItemBuilder }

  TTextGridItemBuilder = class(TInterfacedObject, ITextGridItemBuilder)
  private
    FRenderer: TGridTextRenderer;
    FTextElement: TTextVisualElement;
    FCellSettings: TGridCellSettings;
  protected
    constructor Create(ARenderer: TGridTextRenderer);
  public
    function NewTextItem: ITextGridItemBuilder;
    function WithText(AText: string): ITextGridItemBuilder;
    function WithHorizontalAlign(AHorizontalAlign: TTextAlignHorizontal): ITextGridItemBuilder;
    function WithVerticalAlign(AVerticalAlign: TTextAlignVertical): ITextGridItemBuilder;
    function WithAlignment(AHorizontal: TTextAlignHorizontal; AVertical: TTextAlignVertical): ITextGridItemBuilder;
    function WithCellSettings(ACellSettings: TGridCellSettings): ITextGridItemBuilder;
    function AddToGrid(AGrid: TGridLayout): ITextGridItemBuilder; overload;
    function AddToGrid(AGrid: TGridLayout; ARow, AColumn: Integer): ITextGridItemBuilder; overload;
  end;


implementation

{ TTextGridItemBuilder }

constructor TTextGridItemBuilder.Create(ARenderer: TGridTextRenderer);
begin
  FRenderer := ARenderer;
  NewTextItem;
end;

function TTextGridItemBuilder.NewTextItem: ITextGridItemBuilder;
begin
  Result := Self;
  FTextElement := TTextVisualElement.Create(FRenderer);
  FCellSettings := nil;
end;

function TTextGridItemBuilder.WithText(AText: string): ITextGridItemBuilder;
begin
  Result := Self;
  FTextElement.SetText(AText);
end;

function TTextGridItemBuilder.WithHorizontalAlign
  (AHorizontalAlign: TTextAlignHorizontal): ITextGridItemBuilder;
begin
  Result := Self;
  FTextElement.HorizontalAlign := AHorizontalAlign;
end;

function TTextGridItemBuilder.WithVerticalAlign
  (AVerticalAlign: TTextAlignVertical): ITextGridItemBuilder;
begin
  Result := Self;
  FTextElement.VerticalAlign := AVerticalAlign;
end;

function TTextGridItemBuilder.WithAlignment(AHorizontal: TTextAlignHorizontal;
  AVertical: TTextAlignVertical): ITextGridItemBuilder;
begin
  Result := Self;
  FTextElement.HorizontalAlign := AHorizontal;
  FTextElement.VerticalAlign := AVertical;
end;

function TTextGridItemBuilder.WithCellSettings(ACellSettings: TGridCellSettings
  ): ITextGridItemBuilder;
begin
  Result := Self;
  FCellSettings := ACellSettings;
end;

function TTextGridItemBuilder.AddToGrid(AGrid: TGridLayout
  ): ITextGridItemBuilder;
begin
  Result := Self;
  AGrid.AddItem(TTextGridItem.Create(FTextElement), FCellSettings);
end;

function TTextGridItemBuilder.AddToGrid(AGrid: TGridLayout;
  ARow, AColumn: Integer): ITextGridItemBuilder;
begin
  Result := Self;
  AGrid.AddItem(
    TTextGridItem.Create(FTextElement),
    TGridCellSettings.Create(ARow, AColumn)
  );
end;

{ TGridItemFactory }

function TGridItemFactory.BuildTextItem(ARenderer: TGridTextRenderer
  ): ITextGridItemBuilder;
begin
  Result := TTextGridItemBuilder.Create(ARenderer);
end;

function TGridItemFactory.BuildControlItem: IControlGridItemBuilder;
begin
  Result := TControlGridItemBuilder.Create;
end;

function TGridItemFactory.BuildHtmlTableItem(ARenderer: TGridHtmlTableRenderer
  ): IHtmlTableGridItemBuilder;
begin
  Result := THtmlTableGridItemBuilder.Create(ARenderer);
end;

{ TControlGridItemBuilder }

constructor TControlGridItemBuilder.Create;
begin
  inherited Create;
end;

function TControlGridItemBuilder.WithControl(AControl: TControl
  ): IControlGridItemBuilder;
begin
  Result := Self;
  FControl := AControl;
end;

function TControlGridItemBuilder.WithCellSettings
  (ACellSettings: TGridCellSettings): IControlGridItemBuilder;
begin
  Result := Self;
  FCellSettings := ACellSettings;
end;

function TControlGridItemBuilder.AddToGrid(AGrid: TGridLayout
  ): IControlGridItemBuilder;
begin
  Result := Self;
  AGrid.AddItem(
    TControlGridItem.Create(FControl),
    FCellSettings
  );
end;

function TControlGridItemBuilder.AddWithFiller(AFiller: IGridFill
  ): IControlGridItemBuilder;
begin
  Result := Self;
  AFiller.PlaceItem(
    TControlGridItem.Create(FControl)
  );
end;

{ THtmlTableGridItemBuilder }

constructor THtmlTableGridItemBuilder.Create
  (ARenderer: TGridHtmlTableRenderer);
begin
  FElement := THtmlTableVisualElement.Create(FRenderer);
  FCellSettings := nil;
end;

function THtmlTableGridItemBuilder.WithStrContent(AContent: string
  ): IHtmlTableGridItemBuilder;
begin
  Result := Self;
  FElement.StrContent := AContent;
end;

function THtmlTableGridItemBuilder.WithCellSettings
  (ACellSettings: TGridCellSettings): IHtmlTableGridItemBuilder;
begin
  Result := Self;
  FCellSettings := ACellSettings;
end;

function THtmlTableGridItemBuilder.AddToGrid(AGrid: TGridLayout
  ): IHtmlTableGridItemBuilder;
begin
  Result := Self;
  AGrid.AddItem(
    THtmlTableGridItem.Create(FElement),
    FCellSettings
  );
end;

function THtmlTableGridItemBuilder.AddWithFiller(AFiller: IGridFill
  ): IHtmlTableGridItemBuilder;
begin
  Result := Self;
  AFiller.PlaceItem(
    THtmlTableGridItem.Create(FElement)
  );
end;

end.

