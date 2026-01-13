unit uvistaskmove;

{$mode ObjFPC}{$H+}

interface

uses
  ActnList,
  Buttons,
  Classes,
  ComCtrls,
  ExtCtrls,
  Forms,
  StdCtrls,
  mormot.core.base,
  mormot.core.variants,
  mormot.net.ldap,
  ursatldapclient,
  uaductreeview,
  Controls;

type

  { TMoveTreeNode }

  TMoveTreeNode = class(TTreeNode)
  private
    fDistinguishedName: RawUtf8;
    fName: RawUtf8;
    fObjectClass: TRawUtf8DynArray;

    function GetLastObjectClass: RawUtf8;
    procedure SetDistinguishedName(AValue: RawUtf8);
    procedure SetName(AValue: RawUtf8);
    procedure SetObjectClass(AValue: TRawUtf8DynArray);
  public
    property Name: RawUtf8 read fName write SetName;
    property DistinguishedName: RawUtf8 read fDistinguishedName write SetDistinguishedName;
    property ObjectClass: TRawUtf8DynArray read fObjectClass write SetObjectClass;
    property LastObjectClass: RawUtf8 read GetLastObjectClass;
  end;

  { TVisTaskMove }

  TVisTaskMove = class(TForm)
    Action_OK: TAction;
    ActionList: TActionList;
    Btn_Cancel: TBitBtn;
    Btn_Ok: TBitBtn;
    Edit_DN: TEdit;
    Edit_RDN: TEdit;
    Label_RDN: TLabel;
    Label_DN: TLabel;
    PanelBottom: TPanel;
    TreeView: TTreeView;
    {$push}{$warn 5024 off}
    procedure Action_OKExecute(Sender: TObject);
    procedure Action_OKUpdate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure TreeViewCreateNodeClass(Sender: TCustomTreeView;
      var NodeClass: TTreeNodeClass);
    {$pop}
  private
    Ldap: TRsatLdapClient;
    DN, RDN: String;

    fDomainNode: TMoveTreeNode;

    procedure Tree_Images();
  public
    constructor Create(TheOwner: TComponent; _Ldap: TRsatLdapClient; _DN: String); reintroduce;
  end;

implementation

uses
  Dialogs,
  SysUtils,
  System.UITypes,
  mormot.core.text,
  ucommon,
  ucommonui,
  ursatldapclientui,
  ucoredatamodule;
{$R *.lfm}

{ TMoveTreeNode }

procedure TMoveTreeNode.SetDistinguishedName(AValue: RawUtf8);
begin
  if fDistinguishedName = AValue then
    Exit;
  fDistinguishedName := AValue;
end;

function TMoveTreeNode.GetLastObjectClass: RawUtf8;
var
  c: SizeInt;
begin
  result := '';
  c := Length(ObjectClass);
  if (c <= 0) then
    Exit;
  result := ObjectClass[c - 1];
end;

procedure TMoveTreeNode.SetName(AValue: RawUtf8);
begin
  if fName = AValue then
    Exit;
  fName := AValue;

  Text := fName;
end;

procedure TMoveTreeNode.SetObjectClass(AValue: TRawUtf8DynArray);
begin
  if fObjectClass = AValue then
    Exit;
  fObjectClass := AValue;

  ImageIndex := ObjectClassToImageIndex(LastObjectClass);
  SelectedIndex := ImageIndex;
end;

{ TVisTaskMove }

constructor TVisTaskMove.Create(TheOwner: TComponent; _Ldap: TRsatLdapClient;
  _DN: String);
var
  DNs: TNameValueDNs;
begin
  Inherited Create(TheOwner);
  Ldap := _Ldap;
  DN := _DN;
  ParseDN(_DN, DNs);
  RDN := DNs[0].Value;
end;

procedure TVisTaskMove.FormShow(Sender: TObject);
var
  Obj: TLdapResult;
begin
  Edit_DN.Text := DNToCN(DN);
  Edit_RDN.Text := RDN;

  Obj := Ldap.SearchObject(Ldap.DefaultDN(), '', ['name', 'distinguishedName', 'objectClass']);

  if not Assigned(Obj) then
    Exit;

  fDomainNode := (TreeView.Items.Add(nil, Obj.Find('name').GetReadable()) as TMoveTreeNode);
  fDomainNode.DistinguishedName := Obj.Find('distinguishedName').GetReadable();
  fDomainNode.ObjectClass := Obj.Find('objectClass').GetAllReadable;

  fDomainNode.Selected := True;

  UnifyButtonsWidth([Btn_Ok, Btn_Cancel]);
end;

procedure TVisTaskMove.TreeViewChange(Sender: TObject; Node: TTreeNode);
var
  i: Integer;
  NodeBackup: TDocVariantData;
  MoveNode, NewMoveNode: TMoveTreeNode;
  SearchResult: TLdapResult;
  ItemDistinguishedName: RawUtf8;
begin
  MoveNode := (Node as TMoveTreeNode);
  NodeBackup.Init();

  // List existing nodes
  for i := 0 to MoveNode.Count - 1 do
    NodeBackup.I[(MoveNode.Items[i] as TMoveTreeNode).distinguishedName] := i;

  // Add missing nodes
  Ldap.SearchBegin();
  TreeView.BeginUpdate;
  try
    Ldap.SearchScope := lssSingleLevel;
    repeat
      if not Ldap.Search(MoveNode.distinguishedName, False, '', ['name', 'distinguishedName', 'objectClass']) then
        Exit;

      for SearchResult in Ldap.SearchResult.Items do
      begin
        if not Assigned(SearchResult) then
          continue;

        ItemDistinguishedName := SearchResult.Find('distinguishedName').GetReadable();
        if NodeBackup.Exists(ItemDistinguishedName) then
        begin
          (MoveNode.Items[NodeBackup.I[ItemDistinguishedName]] as TMoveTreeNode).name := SearchResult.Find('name').GetReadable();
          (MoveNode.Items[NodeBackup.I[ItemDistinguishedName]] as TMoveTreeNode).ObjectClass := SearchResult.Find('objectClass').GetAllReadable;
          NodeBackup.Delete(ItemDistinguishedName);
        end
        else
        begin
          NewMoveNode := (TreeView.Items.AddChild(MoveNode, '') as TMoveTreeNode);
          NewMoveNode.DistinguishedName := ItemDistinguishedName;
          NewMoveNode.Name := SearchResult.Find('name').GetReadable();
          NewMoveNode.ObjectClass := SearchResult.Find('objectClass').GetAllReadable;
        end;
      end;
    until Ldap.SearchCookie = '';

    // Delete useless nodes
    i := MoveNode.Count - 1;
    while i >= 0 do
    begin
      if NodeBackup.Exists((MoveNode.Items[i] as TMoveTreeNode).DistinguishedName) then
        TreeView.Items.Delete(MoveNode.Items[i]);
      Dec(i);
    end;
  finally
    TreeView.EndUpdate;
    Ldap.SearchEnd;
  end;
end;

procedure TVisTaskMove.TreeViewCreateNodeClass(Sender: TCustomTreeView;
  var NodeClass: TTreeNodeClass);
begin
  NodeClass := TMoveTreeNode;
end;

procedure TVisTaskMove.Tree_Images();
var
  Node: TTreeNode;
  NodeData: PDocVariantData;
begin
  Node := TreeView.Items.GetFirstNode();
  while Assigned(Node) do
  begin
    NodeData := PDocVariantData(Node.Data)^.A['objectClass'];
    if NodeData^.Count > 0 then
    begin
      Node.ImageIndex    := ObjectClassToImageIndex(NodeData^.ToRawUtf8DynArray[NodeData^.Count - 1]);
      Node.SelectedIndex := ObjectClassToImageIndex(NodeData^.ToRawUtf8DynArray[NodeData^.Count - 1]);
    end;
    Node := Node.GetNext();
  end;
end;

procedure TVisTaskMove.Action_OKExecute(Sender: TObject);
var
  DNs: TNameValueDNs;
  newDN: String;
  newRDN: RawUtf8 = '';
begin
  ParseDN(DN, DNs);
  if not LdapEscapeName(Edit_RDN.Text, newRDN) then
  begin
    ModalResult := mrNone;
    Exit;
  end;
  newDN := DNs[0].Name + '=' + newRDN;
  newDn += ',' + PDocVariantData(TreeView.Selected.Data)^.S['distinguishedName'];
  if not Ldap.MoveLdapEntry(DN, newDn) then
    Exit;
end;

procedure TVisTaskMove.Action_OKUpdate(Sender: TObject);
begin
  Action_OK.Enabled := TreeView.Selected <> nil;
  Btn_Ok.Default := TreeView.Selected <> nil;
  Btn_Cancel.Default := not Btn_Ok.Default;
end;

procedure TVisTaskMove.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    27: Close;
  end;
end;

end.

