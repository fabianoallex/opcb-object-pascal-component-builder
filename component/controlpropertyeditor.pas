unit ControlPropertyEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PropEdits, ComponentEditors, ObjectInspector, Controls,
  Dialogs, GridLayoutComponent, ULayout;

type
  { TControlPropertyEditor }

  TControlPropertyEditor = class(TComponentPropertyEditor)
  private
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetValue: string; override;
    procedure Edit; override;
  end;


implementation

uses
  ComponentReg, LCLType, LCLProc, LCLIntf, TypInfo, StdCtrls, Forms, Buttons,
  UGridLayoutBuilder;

{ TControlPropertyEditor }

function TControlPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paValueList, paSortList, paMultiSelect, paRevertable];
end;

function TControlPropertyEditor.GetValue: string;
var
  ControlItem: TControlItem;
  Control: TControl;
begin
  if GetComponent(0) is TControlItem then
  begin
    Control := nil;
    ControlItem := TControlItem(GetComponent(0));

    if Assigned(ControlItem.Control) then
      Control := ControlItem.Control;

    if Assigned(Control) then
      Result := Control.Name
    else
      Result := '';
  end
  else
    Result := inherited GetValue;
end;

procedure TControlPropertyEditor.Edit;
type
  TDialogOptions = record
    ClassRef: TControlClass;
  end;

  TDialog = record
    Form: TForm;
    ListBox: TListBox;
  end;

var
  Control: TControl;
  Form: TCustomForm;
  Container: TWinControl;
  Item: TControlItem;
  GridLayoutComp: TGridLayoutComponent;
  Options: TDialogOptions;

  function CreateDialog: TDialog;
  var
    GridLayout: TGridLayout;
    ListBox: TListBox;
    BtnOk, BtnCancel: TButton;
    I: Integer;
    CommonControls: array of record
      Name: string;
      ClassRef: TControlClass;
    end = (
      (Name: 'TButton'; ClassRef: TButton),
      (Name: 'TSpeedButton'; ClassRef: TSpeedButton),
      (Name: 'TEdit'; ClassRef: TEdit),
      (Name: 'TLabel'; ClassRef: TLabel),
      (Name: 'TCheckBox'; ClassRef: TCheckBox),
      (Name: 'TRadioButton'; ClassRef: TRadioButton),
      (Name: 'TComboBox'; ClassRef: TComboBox)
    );
  begin
    Result.Form := TForm.Create(nil);
    Result.Form.Caption := 'Select Control';
    Result.Form.Position := poScreenCenter;
    Result.Form.BorderStyle := bsDialog;

    BtnOk := TButton.Create(Result.Form);
    BtnOk.Parent := Result.Form;
    BtnOk.Caption := 'OK';
    BtnOk.ModalResult := mrOk;

    BtnCancel := TButton.Create(Result.Form);
    BtnCancel.Parent := Result.Form;
    BtnCancel.Caption := 'Cancelar';
    BtnCancel.ModalResult := mrCancel;

    ListBox := TListBox.Create(Result.Form);
    ListBox.Parent := Result.Form;
    Result.ListBox := ListBox;

    GridLayout := TGridLayout.Create;
    GridLayout.Rows := 2;
    GridLayout.Columns := 3;
    GridLayout.ColumnWidths := 170;
    GridLayout.RowHeights := 35;
    GridLayout.Margins.All := 10;
    GridLayout.RowHeight[0] := 200;
    GridLayout.HorizontalSpacings := 10;
    GridLayout.VerticalSpacings := 10;

    TGridLayoutBuilder.Create(GridLayout)
      .AddItem(
        ListBox,
        TGridCellSettings.Create(0, 0)
        .WithColumnSpan(3)
      )
      .AddItem(BtnOk, TGridCellSettings.Create(1, 1))
      .AddItem(BtnCancel, TGridCellSettings.Create(1, 2))
    ;

    GridLayout.ArrangeItems;
    Result.Form.Width := GridLayout.ContentWidth;
    Result.Form.Height := GridLayout.ContentHeight;

    for I := 0 to High(CommonControls) do
      ListBox.Items.AddObject(
        CommonControls[I].Name,
        TObject(CommonControls[I].ClassRef)
      );
  end;

  function ShowDialog(var Options: TDialogOptions): Integer;
  var
    Dialog: TDialog;
  begin
    Dialog := CreateDialog;
    try
      Result := Dialog.Form.ShowModal;
      Options.ClassRef := TControlClass(
        Dialog.ListBox.Items.Objects[Dialog.ListBox.ItemIndex]
      );
    finally
      Dialog.Form.Free;
    end;
  end;

  function GenerateUniqueName(ABaseName: string): string;
  var
    I: Integer;
  begin
    I := 1;
    Result := ABaseName + IntToStr(I);
    while Form.FindComponent(Result) <> nil do
    begin
      Result := ABaseName + IntToStr(I);
      Inc(I);
    end;
  end;

begin
  Options.ClassRef := nil;
  if ShowDialog(Options) = mrOk then
  begin
    Item := TControlItem(GetComponent(0));
    if not Assigned(Item) then
      Exit;

    GridLayoutComp := Item.Collection.Owner as TGridLayoutComponent;
    Form := GridLayoutComp.Owner as TCustomForm;

    if Assigned(GridLayoutComp.Container) then
      Container := GridLayoutComp.Container
    else
      Container := Form;

    if Assigned(Options.ClassRef) then
    begin
      Control := Options.ClassRef.Create(Form);
      Control.Name := GenerateUniqueName(
        Copy(Options.ClassRef.ClassName, 2, Length(Options.ClassRef.ClassName))
      );
      Control.Parent := Container;
      Control.Caption := Control.Name;

      SetPtrValue(Pointer(Control));
    end;
  end;
end;

procedure TControlPropertyEditor.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Comp: TComponent;
  LayoutOwner: TGridLayoutComponent;
  CtrlItem: TControlItem;
begin
  if GetComponent(0) is TControlItem then
  begin
    CtrlItem := TControlItem(GetComponent(0));
    if CtrlItem.Collection is TControlCollection then
    begin
      if CtrlItem.Collection.Owner is TGridLayoutComponent then
        LayoutOwner := TGridLayoutComponent(CtrlItem.Collection.Owner)
      else
        Exit;
    end
    else
      Exit;
  end
  else
    Exit;

  for I := 0 to LayoutOwner.Owner.ComponentCount - 1 do
  begin
    Comp := LayoutOwner.Owner.Components[I];
    if Assigned(Comp) and (Comp is TControl) then
    begin
      // Aqui pode aplicar o filtro do ControlScope, por exemplo:
      if LayoutOwner.ControlScope = TControlScope.csForm then
        Proc(Comp.Name)
      else if LayoutOwner.ControlScope = csParent then
      begin
        if (Comp is TControl) and (TControl(Comp).Parent = LayoutOwner.Container) then
          Proc(Comp.Name);
      end;
    end;
  end;
end;

end.

