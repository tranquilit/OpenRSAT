unit ufrmpropertyattributes;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  StdCtrls,
  Buttons, ExtCtrls, ActnList,
  tis.ui.grid.core,
  mormot.core.base,
  mormot.core.log, mormot.core.variants, mormot.net.ldap,
  uproperty,
  upropertyframe, VirtualTrees, Graphics, Menus;

type

  { TFrmPropertyAttributes }

  TFrmPropertyAttributes = class(TPropertyFrame)
    Action_Filter: TAction;
    Action_Modify: TAction;
    ActionList1: TActionList;
    BitBtn_Modify: TBitBtn;
    BitBtn_Filter: TBitBtn;
    Label_Attributes: TLabel;
    List_Attributes: TTisGrid;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    Timer_SearchInGrid: TTimer;
    procedure Action_ModifyExecute(Sender: TObject);
    procedure Action_ModifyUpdate(Sender: TObject);
    procedure BitBtn_FilterClick(Sender: TObject);
    function List_AttributesCompareByRow(aSender: TTisGrid;
      const aPropertyName: RawUtf8; const aRow1, aRow2: PDocVariantData;
      var aHandled: Boolean): PtrInt;
    procedure List_AttributesDblClick(Sender: TObject);
    procedure List_AttributesDrawText(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      const CellText: String; const CellRect: TRect; var DefaultDraw: Boolean);
    procedure List_AttributesGetText(aSender: TBaseVirtualTree;
      aNode: PVirtualNode; const aCell: TDocVariantData; aColumn: TColumnIndex;
      aTextType: TVSTTextType; var aText: string);
    procedure List_AttributesKeyPress(Sender: TObject; var Key: char);
    procedure MenuItem1Click(Sender: TObject);
    procedure Timer_SearchInGridTimer(Sender: TObject);
  private
    fLog: TSynLog;
    fProperty: TProperty;
    fSearchWord: RawUtf8;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Update(Props: TProperty); override;
  end;

implementation
uses
  mormot.core.text,
  ucommon,
  uhelpers,
  uvisattributeeditor;

{$R *.lfm}

{ TFrmPropertyAttributes }

procedure TFrmPropertyAttributes.Timer_SearchInGridTimer(Sender: TObject);
begin
  Timer_SearchInGrid.Enabled := False;
end;

procedure TFrmPropertyAttributes.List_AttributesKeyPress(Sender: TObject;
  var Key: char);
begin
  SearchInGrid(Timer_SearchInGrid, List_Attributes, fSearchWord, Key);
end;

procedure TFrmPropertyAttributes.MenuItem1Click(Sender: TObject);
begin
  Update(fProperty);
end;

procedure TFrmPropertyAttributes.List_AttributesDblClick(Sender: TObject);
begin
  Action_Modify.Execute;
end;

procedure TFrmPropertyAttributes.Action_ModifyUpdate(Sender: TObject);
begin
  Action_Modify.Enabled := Assigned(List_Attributes.FocusedRow);
end;

procedure TFrmPropertyAttributes.BitBtn_FilterClick(Sender: TObject);
var
  P: TPoint;
begin
  P := Point(BitBtn_Filter.ClientRect.Left, BitBtn_Filter.ClientRect.Bottom);
  P := BitBtn_Filter.ControlToScreen(P);
  PopupMenu1.PopUp(P.X, P.Y);
end;

function TFrmPropertyAttributes.List_AttributesCompareByRow(aSender: TTisGrid;
  const aPropertyName: RawUtf8; const aRow1, aRow2: PDocVariantData;
  var aHandled: Boolean): PtrInt;
begin
  aHandled := Assigned(aRow1) and Assigned(aRow2) and (aPropertyName <> '') and aRow1^.Exists(aPropertyName) and aRow2^.Exists(aPropertyName);
  if aHandled then
    result := String.Compare(aRow1^.S[aPropertyName], aRow2^.S[aPropertyName], True);
end;

procedure TFrmPropertyAttributes.Action_ModifyExecute(Sender: TObject);
var
  vis: TVisAttributeEditor;
  Attribute: TLdapAttribute;
  i: Integer;
begin
  Attribute := fProperty.Get(List_Attributes.FocusedRow^.S['attribute']);

  vis := TVisAttributeEditor.Create(self, fProperty.Core, Attribute, List_Attributes.FocusedRow^.S['attribute']);
  try
    if (vis.ShowModal <> mrOK) then
      Exit;

    if Vis.Attr.Count <= 0 then
    begin
      fProperty.Add(Vis.Attr.AttributeName, '');
      Exit;
    end;
    fProperty.Add(Vis.Attr.AttributeName, Vis.Attr.GetReadable(0));
    for i := 1 to Vis.Attr.Count - 1 do
      fProperty.Add(Vis.Attr.AttributeName, Vis.Attr.GetReadable(i), aoAlways);
  finally
    FreeAndNil(vis);
  end;
end;

procedure TFrmPropertyAttributes.List_AttributesDrawText(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; const CellText: String; const CellRect: TRect;
  var DefaultDraw: Boolean);
var
  NodeData: PDocVariantData;
  AttributeName: RawUtf8;
begin
  if Column <> 1 then
    Exit;

  NodeData := List_Attributes.GetNodeAsPDocVariantData(Node);
  if not Assigned(NodeData) then
    Exit;
  AttributeName := NodeData^.U['attribute'];


  if not fProperty.IsModified(AttributeName) then // check if diff exists
    Exit;

  if fProperty.GetReadable(AttributeName) = '' then
    TargetCanvas.Font.Style := [TFontStyle.fsItalic, TFontStyle.fsStrikeOut]
  else
    TargetCanvas.Font.Style := [TFontStyle.fsItalic, TFontStyle.fsBold];
end;

procedure TFrmPropertyAttributes.List_AttributesGetText(
  aSender: TBaseVirtualTree; aNode: PVirtualNode; const aCell: TDocVariantData;
  aColumn: TColumnIndex; aTextType: TVSTTextType; var aText: string);
var
  NodeData: PDocVariantData;
  AttributeName: RawUtf8;
  AttributeValues: TRawUtf8DynArray;
  len: SizeInt;
  GridColumn: TTisGridColumn;
begin
  if aColumn <> 1 then
    Exit;

  NodeData := List_Attributes.GetNodeAsPDocVariantData(aNode);
  if not Assigned(NodeData) then
    Exit;
  AttributeName := NodeData^.U['attribute'];

  AttributeValues := fProperty.GetAllReadable(AttributeName);

  aText := String.Join('; ', TStringArray(AttributeValues));
  GridColumn := List_Attributes.FindColumnByIndex(aColumn);
  if GridColumn.Width <= List_Attributes.Canvas.TextWidth(aText) then
  begin
    len := Length(AttributeValues);
    aText := FormatUtf8('%; range=0-%', [AttributeValues[0], Len]);
  end;
end;

constructor TFrmPropertyAttributes.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);

  Caption := 'Attributes';
end;

procedure TFrmPropertyAttributes.Update(Props: TProperty);
var
  Row: TDocVariantData;
  Attributes, value: TRawUtf8DynArray;
  Attribute: RawUtf8;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Update', Self);

  fProperty := Props;
  Attributes := fProperty.AttributesFromSchema;

  Row.Init();
  List_Attributes.Clear;
  List_Attributes.BeginUpdate;
  try
    for Attribute in Attributes do
    begin
      value := fProperty.GetAllReadable(Attribute);
      if (not MenuItem1.Checked) or (MenuItem1.Checked and Assigned(value)) then
      begin
        Row.AddOrUpdateValue('attribute', Attribute);
        List_Attributes.Data.AddItem(Row);
        Row.Clear;
      end;
    end;
  finally
    List_Attributes.EndUpdate;
    List_Attributes.LoadData();
  end;
end;

end.

