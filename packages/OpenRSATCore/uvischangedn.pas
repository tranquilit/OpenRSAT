unit uvischangedn;

{$mode ObjFPC}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ComCtrls,
  ExtCtrls,
  Buttons,
  mormot.core.base,
  mormot.core.variants,
  mormot.net.ldap;

type

  //TLdapTreeNode = class(TTreeNode)
  //public
  //  DistinguishedName: RawUtf8;
  //end;

  { TVisChangeDN }

  TVisChangeDN = class(TForm)
    BitBtn_Ok: TBitBtn;
    BitBtn_Cancel: TBitBtn;
    Panel_Bottom: TPanel;
    TreeView1: TTreeView;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure TreeView1CreateNodeClass(Sender: TCustomTreeView;
      var NodeClass: TTreeNodeClass);
  private
    fSelectedDN: RawUtf8;
    fLdap: TLdapClient;
    fBaseDN: RawUtf8;

    procedure ChangeNode(node: TTreeNode);
  public
    constructor Create(TheOwner: TComponent; _Ldap: TLdapClient; _CurrentDN,
      abaseDN: RawUtf8); reintroduce;

    property SelectedDN: RawUtf8 read fSelectedDN;
  end;

implementation

uses
  mormot.core.log,
  mormot.core.text,
  uaductreeview,
  ucoredatamodule,
  ucommon,
  ursatldapclient;
{$R *.lfm}

const
  TreeViewLdapFilter: RawUtf8 = '|(objectClass=organizationalUnit)(objectClass=container)(objectClass=builtinDomain)';
{ TVisChangeDN }

procedure TVisChangeDN.TreeView1Change(Sender: TObject; Node: TTreeNode);
var
  NodeData: TADUCTreeNodeObject;
begin
  fSelectedDN := '';
  if Assigned(Node) then
  begin
    NodeData := (Node as TADUCTreeNode).GetNodeDataObject;
    if Assigned(NodeData) then
      fSelectedDN := NodeData.DistinguishedName;
  end;
  ChangeNode(Node);
end;

procedure TVisChangeDN.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    27: Close;
  end;
end;

procedure TVisChangeDN.TreeView1CreateNodeClass(Sender: TCustomTreeView;
  var NodeClass: TTreeNodeClass);
begin
  NodeClass := TADUCTreeNode;
end;

procedure TVisChangeDN.ChangeNode(node: TTreeNode);
var
  data: TDocVariantData;
  item: TLdapResult;
  child, nextChild: TTreeNode;
  obj: PDocVariantData;
  bak_cursor: TCursor;
  ObjectClassArray: TRawUtf8DynArray;
  NodeData, ChildData: TADUCTreeNodeObject;
begin
  bak_cursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;

  // Fast Exit
  if not Assigned(Node) then
    Exit;

  // Get NodeData
  NodeData := (Node as TADUCTreeNode).GetNodeDataObject;
  if not Assigned(NodeData) then
    Exit;

  data.init();
  TreeView1.BeginUpdate;
  try
    fLdap.SearchBegin({VisMain.Storage.Options.SearchPageSize});
    try
      fLdap.SearchScope := lssSingleLevel;
      repeat
        if not fLdap.Search(NodeData.DistinguishedName, False, TreeViewLdapFilter, ['distinguishedName', 'objectClass']) then
        begin
          ShowLdapSearchError(fLdap);
          Exit;
        end;

        for item in fLdap.SearchResult.Items do
        begin
          if not Assigned(item) then
            continue;
          data.O_[item.ObjectName]^.S['distinguishedName'] := item.Find('distinguishedName').GetReadable();
          ObjectClassArray := item.Find('objectClass').GetAllReadable;
          data.O_[item.ObjectName]^.S['objectClass'] := ObjectClassArray[High(ObjectClassArray)];
        end;
      until fLdap.SearchCookie = '';
    finally
      fLdap.SearchEnd;
    end;

    nextChild := Node.GetFirstChild;
    while Assigned(nextChild) do
    begin
      child := nextChild;
      nextChild := nextChild.GetNextChild(nextChild);
      ChildData := (child as TADUCTreeNode).GetNodeDataObject;
      if not Assigned(ChildData) or not data.Exists(ChildData.DistinguishedName) then
      begin
        TreeView1.Items.Delete(child);
        continue;
      end;
      data.Delete(ChildData.DistinguishedName);
    end;

    for obj in data.Objects do
    begin
      child := TreeView1.Items.AddChild(node, obj^.S['distinguishedName'].Split(',')[0].Split('=')[1]);
      (child as TADUCTreeNode).NodeType := atntObject;
      ChildData := (Child as TADUCTreeNode).GetNodeDataObject;
      if not Assigned(ChildData) then
        continue;
      ChildData.DistinguishedName := obj^.S['distinguishedName'];
      child.ImageIndex := CoreDataModule.objectClassToImageIndex(obj^.S['objectClass']);
      child.SelectedIndex := child.ImageIndex;
    end;
    node.Expand(False);
  finally
    Screen.Cursor := bak_cursor;
    TreeView1.EndUpdate;
  end;
end;

constructor TVisChangeDN.Create(TheOwner: TComponent; _Ldap: TLdapClient;
  _CurrentDN, abaseDN: RawUtf8);
var
  res: TLdapResult;
  distinguishedName, objectClass: RawUtf8;
  node: TTreeNode;
  i: Integer;
  ObjectClassArray: TRawUtf8DynArray;
  NodeData, NodeItemData: TADUCTreeNodeObject;
begin
  inherited Create(TheOwner);

  fBaseDN := abaseDN;
  fLdap := _Ldap;
  fSelectedDN := _CurrentDN;

  res := fLdap.SearchObject(fLdap.DefaultDN(fBaseDN), '', ['distinguishedName', 'objectClass']);
  if not Assigned(res) then
  begin
    ShowLdapSearchError(fLdap);
    Exit;
  end;

  distinguishedName := res.Find('distinguishedName').GetReadable();
  ObjectClassArray := res.Find('objectClass').GetAllReadable;
  objectClass := ObjectClassArray[High(ObjectClassArray)];

  node := TreeView1.Items.AddFirst(nil, String(DNToCN(distinguishedName)).Split('/')[0]);
  if not Assigned(Node) then
    Exit;
  (node as TADUCTreeNode).NodeType := atntObject;
  NodeData := (Node as TADUCTreeNode).GetNodeDataObject;
  if not Assigned(NodeData) then
    Exit;
  NodeData.DistinguishedName := distinguishedName;
  node.ImageIndex := CoreDataModule.objectClassToImageIndex(objectClass);
  node.SelectedIndex := node.ImageIndex;

  node.Selected := True;
  while Assigned(node) and (NodeData.DistinguishedName <> fSelectedDN) do
  begin
    ChangeNode(node);
    if node.Count <= 0 then
      break;
    for i := 0 to node.Count - 1 do
    begin
      NodeItemData := (Node.Items[i] as TADUCTreeNode).GetNodeDataObject;
      if not Assigned(NodeItemData) then
        continue;
      if String(fSelectedDN).EndsWith(NodeItemData.DistinguishedName) then
      begin
        node := node.Items[i];
        node.Selected := True;
        break;
      end;
      if i = node.Count - 1 then
        node := nil;
    end;
  end;

  UnifyButtonsWidth([BitBtn_Cancel, BitBtn_Ok]);
end;

end.

