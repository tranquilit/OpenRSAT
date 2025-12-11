unit ufrmmoduledns;

{$mode objfpc}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  ComCtrls,
  ExtCtrls,
  ActnList,
  Menus,
  Graphics,
  Dialogs,
  StdCtrls,
  mormot.net.ldap,
  mormot.core.base,
  tis.ui.grid.core,
  ucoredatamodule,
  mormot.core.variants,
  mormot.core.log,
  utreeselectionhistory,
  VirtualTrees,
  ufrmmodule,
  ufrmoption,
  umodule,
  umoduleaddns;

type

  TDNSTreeNodeType = (dtntRoot, dtntCustom, dtntZone, dtntDNSNode);

  { TDNSTreeNode }

  TDNSTreeNode = class(TTreeNode)
  private
    fType: TDNSTreeNodeType;

    // dtntZone, dtntDNSNode
    fAttributes: TLdapAttributeList;
    fDistinguishedName: String;
    fRetrieved: Boolean;

    //fChildren: TDocVariantData;
    //fOwnerZone: TDNSTreeNode;
  public
    constructor Create(AnOwner: TTreeNodes); override;
    destructor Destroy; override;

    function HasVisibleChildren: Boolean;
    property NodeType: TDNSTreeNodeType read fType write fType default dtntRoot;
    property Retrieved: Boolean read fRetrieved default False;
    property DistinguishedName: String read fDistinguishedName;
  end;

  { TFrmModuleDNS }

  TFrmModuleDNS = class(TFrameModule)
    Action_NewZone: TAction;
    Action_OtherNewRecords: TAction;
    Action_Previous: TAction;
    Action_Next: TAction;
    Action_Parent: TAction;
    Action_Property: TAction;
    Action_Delete: TAction;
    Action_Refresh: TAction;
    ActionList_DNS: TActionList;
    Image1: TImage;
    Image2: TImage;
    Label1: TLabel;
    MenuItem10: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    Panel1: TPanel;
    PopupMenu_DNS: TPopupMenu;
    Separator1: TMenuItem;
    Splitter1: TSplitter;
    GridDNS: TTisGrid;
    Timer_TreeChangeNode: TTimer;
    Timer_SearchInGrid: TTimer;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton_Previous: TToolButton;
    ToolButton10: TToolButton;
    ToolButton_Delete: TToolButton;
    ToolButton_Next: TToolButton;
    ToolButton_Parent: TToolButton;
    ToolButton4: TToolButton;
    ToolButton_Property: TToolButton;
    ToolButton6: TToolButton;
    TreeDNS: TTreeView;
    procedure Action_DeleteExecute(Sender: TObject);
    procedure Action_NewZoneExecute(Sender: TObject);
    procedure Action_NewZoneUpdate(Sender: TObject);
    procedure Action_NextExecute(Sender: TObject);
    procedure Action_OtherNewRecordsExecute(Sender: TObject);
    procedure Action_OtherNewRecordsUpdate(Sender: TObject);
    procedure Action_ParentExecute(Sender: TObject);
    procedure Action_PreviousExecute(Sender: TObject);
    procedure Action_PropertyExecute(Sender: TObject);
    procedure Action_RefreshExecute(Sender: TObject);
    procedure GridDNSDblClick(Sender: TObject);
    procedure GridDNSGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure GridDNSKeyPress(Sender: TObject; var Key: char);
    procedure GridDNSMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Timer_SearchInGridTimer(Sender: TObject);
    procedure Timer_TreeChangeNodeTimer(Sender: TObject);
    procedure TreeDNSChange(Sender: TObject; Node: TTreeNode);
    procedure TreeDNSCreateNodeClass(Sender: TCustomTreeView;
      var NodeClass: TTreeNodeClass);
    procedure TreeDNSExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure TreeDNSMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    fLog: TSynLog;
    fTreeSelectionHistory: TTreeSelectionHistory;

    fModule: TModuleADDNS;
    fFrmOption: TFrameOption;

    fRootNode: TDNSTreeNode;
    fForwardLookupZonesNode: TDNSTreeNode;
    fReverseLookupZonesNode: TDNSTreeNode;

    fSearchWord: RawUtf8;

    procedure UpdateGridColumns(Names: TStringArray);
    procedure UpdateNodeRoot(Node: TDNSTreeNode);
    procedure UpdateNodeCustom(Node: TDNSTreeNode);
    procedure UpdateNodeDNSNode(Node: TDNSTreeNode);
    procedure UpdateNodeZone(Node: TDNSTreeNode);

    procedure UpdateNode(Node: TDNSTreeNode);

    procedure RetrieveNodeDNSNode(Node: TDNSTreeNode);
    procedure RetrieveNodeZone(Node: TDNSTreeNode);

    procedure RetrieveNode(Node: TDNSTreeNode);

    function GetCurrentZone: TDNSTreeNode;

    procedure OnLdapClientConnect(LdapClient: TLdapClient);
    procedure OnLdapClientClose(LdapClient: TLdapClient);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

  protected
    function GetFrmOption: TFrameOption; override;
    function GetModule: TModule; override;
    function GetFrmOptionClass: TFrameOptionClass; override;
  published
    ////////////////
    /// TFrameModule
    procedure Refresh; override;
    procedure Load; override;
    ///
    ////////////////
  end;

implementation

uses
  mormot.net.dns,
  mormot.core.text,
  uvisselectnewrecordtype,
  ucommon,
  ucommonui,
  uDarkStyleParams,
  uvisnewzonewizard,
  uvisnewresourcerecord,
  ursatldapclientui,
  udns,
  ufrmrsat;

{$R *.lfm}

const
  DOMAIN_DNS_ZONES: String = 'CN=MicrosoftDNS,DC=DomainDnsZones';
  FOREST_DNS_ZONES: String = 'CN=MicrosoftDNS,DC=ForestDnsZones';

{ TDNSTreeNode }

constructor TDNSTreeNode.Create(AnOwner: TTreeNodes);
begin
  inherited Create(AnOwner);

  ImageIndex := Ord(ileADOU);
  SelectedIndex := Ord(ileADOU);

  fRetrieved := False;
  fDistinguishedName := '';
  fAttributes := nil;
end;

destructor TDNSTreeNode.Destroy;
begin
  FreeAndNil(fAttributes);

  inherited Destroy;
end;

function TDNSTreeNode.HasVisibleChildren: Boolean;
var
  i: Integer;
begin
  result := False;

  for i := 0 to Count - 1 do
    if Items[i].Visible then
      Exit(True);
end;

{ TFrmModuleDNS }

procedure TFrmModuleDNS.TreeDNSCreateNodeClass(Sender: TCustomTreeView;
  var NodeClass: TTreeNodeClass);
begin
  NodeClass := TDNSTreeNode;
end;

procedure TFrmModuleDNS.TreeDNSExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
begin
  Node.Selected := True;
end;

procedure TFrmModuleDNS.TreeDNSMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  GridDNS.ClearSelection;
  GridDNS.FocusedNode := nil;
end;

procedure TFrmModuleDNS.UpdateGridColumns(Names: TStringArray);
var
  Column: TTisGridColumn;
  i: Integer;

  function ArrayContains(Names: TStringArray; str: String): Boolean;
  var
    N: String;
  begin
    result := False;

    for N in Names do
      if (N = str) then
        Exit(True);
  end;

begin
  i := 0;
  Column := GridDNS.FindColumnByIndex(i);
  while Assigned(Column) do
  begin
    if ArrayContains(Names, Column.PropertyName) then
      Column.Options := Column.Options + [coVisible]
    else
      Column.Options := Column.Options - [coVisible];
    Inc(i);
    Column := GridDNS.FindColumnByIndex(i);
  end;

end;

procedure TFrmModuleDNS.UpdateNodeRoot(Node: TDNSTreeNode);
var
  newRaw: TDocVariantData;
  i: Integer;
begin
  newRaw.Init();
  UpdateGridColumns(['name']);

  for i := 0 to Node.Count - 1 do
  begin
    newRaw.AddValue('name', Node.Items[i].Text);
    GridDNS.Data.AddItem(newRaw);
    newRaw.Clear;
  end;
end;

procedure TFrmModuleDNS.UpdateNodeCustom(Node: TDNSTreeNode);
var
  newRaw: TDocVariantData;
  i: Integer;
begin
  newRaw.Init();
  UpdateGridColumns(['name', 'type', 'status', 'dnssec', 'keymaster']);

  for i := 0 to Node.Count - 1 do
  begin
    newRaw.AddValue('name', Node.Items[i].Text);
    GridDNS.Data.AddItem(newRaw);
    newRaw.Clear;
  end;
end;

procedure TFrmModuleDNS.UpdateNodeDNSNode(Node: TDNSTreeNode);
var
  newRaw: TDocVariantData;
  i, j: Integer;
  DNSRecordAttribute: TLdapAttribute;
  RecordName: String;
  DNSRecord: TDNSRecord;
begin
  newRaw.Init();
  UpdateGridColumns(['name', 'type', 'data', 'timestamp']);

  // Self information
  DNSRecordAttribute := Node.fAttributes.Find('dnsRecord');
  if Assigned(DNSRecordAttribute) then
  begin
    RecordName := '(Same as parent folder)';
    for j := 0 to DNSRecordAttribute.Count - 1 do
    begin
      if not DNSRecordBytesToRecord(DNSRecord, PByteArray(DNSRecordAttribute.GetRaw(j))^) then
        continue;
      newRaw.AddValue('name', RecordName);
      newRaw.AddValue('type', DnsResourceRecordToStr(TDnsResourceRecord(DNSRecord.RecType)));
      newRaw.AddValue('data', DNSRecordDataToString(DNSRecord));
      newRaw.AddValue('timestamp', '');
      newRaw.AddValue('rawdata', DNSRecordAttribute.GetRaw(j));
      newRaw.AddValue('distinguishedName', Node.fAttributes.Find('distinguishedName').GetReadable());
      GridDNS.Data.AddItem(newRaw);
      NewRaw.Clear;
    end;
  end;
  for i := 0 to Node.Count - 1 do
  begin
    RecordName := Node.Items[i].Text;
    DNSRecordAttribute := (Node.Items[i] as TDNSTreeNode).fAttributes.Find('dnsRecord');
    if not Assigned(DNSRecordAttribute) or (Node.Items[i] as TDNSTreeNode).HasVisibleChildren then
    begin
      newRaw.AddValue('name', RecordName);
      GridDNS.Data.AddItem(newRaw);
      newRaw.Clear;
      continue;
    end;
    for j := 0 to DNSRecordAttribute.Count - 1 do
    begin
      if not DNSRecordBytesToRecord(DNSRecord, PByteArray(DNSRecordAttribute.getRaw(j))^) then
        continue;
      newRaw.AddValue('name', RecordName);
      newRaw.AddValue('type', DnsResourceRecordToStr(TDnsResourceRecord(DNSRecord.RecType)));
      newRaw.AddValue('data', DNSRecordDataToString(DNSRecord));
      newRaw.AddValue('timestamp', '');
      newRaw.AddValue('rawdata', DNSRecordAttribute.GetRaw(j));
      newRaw.AddValue('distinguishedName', (Node.Items[i] as TDNSTreeNode).fAttributes.Find('distinguishedName').GetReadable());
      GridDNS.Data.AddItem(newRaw);
      NewRaw.Clear;
    end;
  end;
end;

procedure TFrmModuleDNS.UpdateNodeZone(Node: TDNSTreeNode);
var
  newRaw: TDocVariantData;
  i, j: Integer;
  DNSRecordAttribute: TLdapAttribute;
  DNSRecord: TDNSRecord;
  RecordName: String;
begin
  newRaw.Init();
  UpdateGridColumns(['name', 'type', 'data', 'timestamp']);

  for i := 0 to Node.Count - 1 do
  begin
    RecordName := Node.Items[i].Text;
    if RecordName = '@' then
      RecordName := '(Same as parent folder)';
    DNSRecordAttribute := (Node.Items[i] as TDNSTreeNode).fAttributes.Find('dnsRecord');
    if not Assigned(DNSRecordAttribute) or (Node.Items[i] as TDNSTreeNode).HasVisibleChildren then
    begin
      newRaw.AddValue('name', RecordName);
      GridDNS.Data.AddItem(newRaw);
      newRaw.Clear;
      continue;
    end;
    for j := 0 to DNSRecordAttribute.Count - 1 do
    begin
      if not DNSRecordBytesToRecord(DNSRecord, PByteArray(DNSRecordAttribute.GetRaw(j))^) then
        continue;
      newRaw.AddValue('name', RecordName);
      newRaw.AddValue('type', DnsResourceRecordToStr(TDnsResourceRecord(dnsRecord.RecType)));
      newRaw.AddValue('data', DNSRecordDataToString(dnsRecord));
      newRaw.AddValue('timestamp', '');
      newRaw.AddValue('rawdata', DNSRecordAttribute.GetRaw(j));
      newRaw.AddValue('distinguishedName', (Node.Items[i] as TDNSTreeNode).fAttributes.Find('distinguishedName').GetReadable());
      GridDNS.Data.AddItem(newRaw);
      newRaw.Clear;
    end;
  end;
end;

procedure TFrmModuleDNS.UpdateNode(Node: TDNSTreeNode);
begin
  GridDNS.Clear;
  if not Node.Retrieved then
    RetrieveNode(Node);
  GridDNS.BeginUpdate;
  try
    case Node.fType of
      dtntRoot: UpdateNodeRoot(Node);
      dtntCustom: UpdateNodeCustom(Node);
      dtntDNSNode: UpdateNodeDNSNode(Node);
      dtntZone: UpdateNodeZone(Node);
    end;
  finally
    GridDNS.EndUpdate;
    GridDNS.LoadData;
  end;
end;

procedure TFrmModuleDNS.RetrieveNodeDNSNode(Node: TDNSTreeNode);
begin
  while Assigned(Node) and (Node.fType = dtntDNSNode) do
    Node := (Node.Parent as TDNSTreeNode);
  Node.fRetrieved := False;
  Node.Selected := True;
end;

procedure TFrmModuleDNS.RetrieveNodeZone(Node: TDNSTreeNode);
var
  SearchResult: TLdapResult;
  NewNode: TDNSTreeNode;

  function AddDNSNode(Node: TDNSTreeNode; SplittedName: TStringArray): TDNSTreeNode;
  var
    NewNode: TDNSTreeNode;
    idx: Integer;
  begin
    idx := high(SplittedName);
    NewNode := (node.FindNode(SplittedName[idx]) as TDNSTreeNode);
    if not Assigned(NewNode) then
      NewNode := (TreeDNS.Items.AddChild(Node, SplittedName[idx]) as TDNSTreeNode);
    NewNode.Visible := (idx > 0) or (NewNode.Count > 0);
    NewNode.fType := dtntDNSNode;
    NewNode.fRetrieved := True;
    Delete(SplittedName, idx, 1);
    if idx > 0 then
      newNode := AddDNSNode((NewNode as TDNSTreeNode), SplittedName);
    result := newNode;
  end;

begin
  Node.DeleteChildren;
  FrmRSAT.LdapClient.SearchBegin();
  try
    FrmRSAT.LdapClient.SearchScope := lssSingleLevel;

    repeat
      if not FrmRSAT.LdapClient.Search(Node.DistinguishedName, False, '', ['*']) then
      begin
        if Assigned(fLog) then
          fLog.Log(sllError, '% - Ldap Search Error: %', [Self.Name, FrmRSAT.LdapClient.ResultString]);
        ShowLdapSearchError(FrmRSAT.LdapClient);
        Exit;
      end;

      for SearchResult in FrmRSAT.LdapClient.SearchResult.Items do
      begin
        if not Assigned(SearchResult) then
          continue;

        NewNode := AddDNSNode(Node, String(SearchResult.Find('dc').GetReadable()).Split('.'));
        NewNode.fDistinguishedName := SearchResult.ObjectName;
        NewNode.fAttributes := TLdapAttributeList(SearchResult.Attributes.Clone);
      end;
    until FrmRSAT.LdapClient.SearchCookie = '';
  finally
    FrmRSAT.LdapClient.SearchEnd;
  end;
  Node.fRetrieved := True;
end;

procedure TFrmModuleDNS.RetrieveNode(Node: TDNSTreeNode);
begin
  case Node.fType of
    dtntZone: RetrieveNodeZone(Node);
    dtntDNSNode: RetrieveNodeDNSNode(Node);
  end;
end;

function TFrmModuleDNS.GetCurrentZone: TDNSTreeNode;
var
  dcPrefix: String;
begin
  result := nil;
  dcPrefix := '';
  case (TreeDNS.Selected as TDNSTreeNode).fType of
    dtntZone: result := (TreeDNS.Selected as TDNSTreeNode);
    dtntDNSNode:
    begin
      result := (TreeDNS.Selected as TDNSTreeNode);
      while Assigned(result) and (result.fType <> dtntZone) do
      begin
        dcPrefix := Format('.%s%s', [result.Text, dcPrefix]);
        result := (result.Parent as TDNSTreeNode);
      end;
    end;
  end;
end;

procedure TFrmModuleDNS.OnLdapClientConnect(LdapClient: TLdapClient);
begin
  Refresh;
end;

procedure TFrmModuleDNS.OnLdapClientClose(LdapClient: TLdapClient);
begin
  GridDNS.Clear;
end;

procedure TFrmModuleDNS.TreeDNSChange(Sender: TObject; Node: TTreeNode);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Change Node', Self);

  if Timer_TreeChangeNode.Enabled then
    Timer_TreeChangeNode.Enabled := False;
  Timer_TreeChangeNode.Enabled := True;
end;

procedure TFrmModuleDNS.Action_RefreshExecute(Sender: TObject);
var
  dcCache: TDocVariantData;

  procedure UpdateZones(distinguishedName: String; dcCache: PDocVariantData);
  var
    SearchResult: TLdapResult;
    Node: TDNSTreeNode;
    dc: String;
  begin
    FrmRSAT.LdapClient.SearchBegin();
    try
      FrmRSAT.LdapClient.SearchScope := lssSingleLevel;
      repeat
        FrmRSAT.LdapClient.SearchRangeBegin;
        try
          if not FrmRSAT.LdapClient.Search(distinguishedName, False, '', ['*']) then
          begin
            if Assigned(fLog) then
              fLog.Log(sllError, '% - Ldap Search Error: %', [Self.Name, FrmRSAT.LdapClient.ResultString]);
            ShowLdapSearchError(FrmRSAT.LdapClient);
            Exit;
          end;
        finally
          FrmRSAT.LdapClient.SearchRangeEnd;
        end;

        for SearchResult in FrmRSAT.LdapClient.SearchResult.Items do
        begin
          if not Assigned(SearchResult) then
            continue;
          dc := SearchResult.find('dc').GetReadable();
          if dc = 'RootDNSServers' then
            continue;
          if dc.EndsWith('in-addr.arpa') then
          begin
            Node := (fReverseLookupZonesNode.FindNode(dc) as TDNSTreeNode);
            if not Assigned(Node) then
            begin
              Node := (TreeDNS.Items.AddChild(fReverseLookupZonesNode, dc) as TDNSTreeNode);
              Node.NodeType := dtntZone;
              Node.HasChildren := True;
              Node.fDistinguishedName := SearchResult.ObjectName;
            end;
          end
          else
          begin
            Node := (fForwardLookupZonesNode.FindNode(dc) as TDNSTreeNode);
            if not Assigned(Node) then
            begin
              Node := (TreeDNS.Items.AddChild(fForwardLookupZonesNode, dc) as TDNSTreeNode);
              Node.NodeType := dtntZone;
              Node.HasChildren := True;
              Node.fDistinguishedName := SearchResult.ObjectName;
            end;
          end;
          if Assigned(Node.fAttributes) then
            FreeAndNil(Node.fAttributes);
          Node.fAttributes := TLdapAttributeList(SearchResult.Attributes.Clone);
          dcCache^.B[dc] := True;
        end;
      until FrmRSAT.LdapClient.SearchCookie = '';
    finally
      FrmRSAT.LdapClient.SearchEnd;
    end;
  end;

  procedure RemoveOldNodes(ZonesNode: TDNSTreeNode; dcCache: PDocVariantData);
  var
    i: Integer;
  begin
    i := ZonesNode.Count - 1;
    while i >= 0 do
    begin
      if not dcCache^.Exists(ZonesNode.Items[i].Text) then
        ZonesNode.Items[i].Delete;
      Dec(i);
    end;
  end;

begin
  dcCache.Init();
  TreeDNS.BeginUpdate;
  try
    UpdateZones(Format('%s,%s', [DOMAIN_DNS_ZONES, FrmRSAT.LdapClient.DefaultDN()]), @dcCache);
    UpdateZones(Format('%s,%s', [FOREST_DNS_ZONES, FrmRSAT.LdapClient.RootDN]), @dcCache);

    RemoveOldNodes(fReverseLookupZonesNode, @dcCache);
    RemoveOldNodes(fForwardLookupZonesNode, @dcCache);
    if not Assigned(TreeDNS.Selected) then
      TreeDNS.Selected := fRootNode;
    (TreeDNS.Selected as TDNSTreeNode).fRetrieved := False;
    UpdateNode((TreeDNS.Selected as TDNSTreeNode));
  finally
    TreeDNS.EndUpdate;
  end;
end;

procedure TFrmModuleDNS.GridDNSDblClick(Sender: TObject);
var
  NodeData: PDocVariantData;
  Node: TTreeNode;
begin
  if not Assigned(GridDNS.FocusedNode) then
    Exit;

  NodeData := GridDNS.GetNodeAsPDocVariantData(GridDNS.FocusedNode);
  if not Assigned(NodeData) then
    Exit;

  if Assigned(TreeDNS.Selected) then
  begin
    if NodeData^.S['name'] = '(Same as parent folder)' then
      Node := TreeDNS.Selected.FindNode('@')
    else
      Node := TreeDNS.Selected.FindNode(NodeData^.S['name']);
    if Assigned(Node) then
    begin
      if Node.Visible then
      begin
        TreeDNS.Selected.Expand(False);
        Node.Selected := True;
      end
      else
      begin
        Action_Property.Execute;
      end;
    end;
  end;
end;

procedure TFrmModuleDNS.GridDNSGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  NodeData: PDocVariantData;
  NodeFound: TDNSTreeNode;
begin
  if GridDNS.FindColumnByIndex(Column).PropertyName = 'name' then
  begin
    NodeFound := nil;
    NodeData := GridDNS.GetNodeAsPDocVariantData(Node);
    if Assigned(NodeData) then
      NodeFound := (TreeDNS.Selected.FindNode(NodeData^.S['name']) as TDNSTreeNode);
    if Assigned(TreeDNS.Selected) and Assigned(NodeFound) and NodeFound.HasChildren then
      ImageIndex := Ord(ileADOU)
    else
      ImageIndex := Ord(ileADUnknown);
  end;
end;

procedure TFrmModuleDNS.GridDNSKeyPress(Sender: TObject; var Key: char);
begin
  SearchInGrid(Timer_SearchInGrid, GridDNS, fSearchWord, Key);
end;

procedure TFrmModuleDNS.GridDNSMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if not Assigned(GridDNS.GetNodeAt(X, Y)) then
  begin
    GridDNS.ClearSelection;
    GridDNS.FocusedNode := nil;
  end;
end;

procedure TFrmModuleDNS.Timer_SearchInGridTimer(Sender: TObject);
begin
  Timer_SearchInGrid.Enabled := False;
end;

procedure TFrmModuleDNS.Timer_TreeChangeNodeTimer(Sender: TObject);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Timer Tree Change', Self);

  Timer_TreeChangeNode.Enabled := False;

  if not Assigned(TreeDNS.Selected) then
  begin
    if Assigned(fLog) then
      fLog.Log(sllInfo, 'No node Assigned', Self);
    Exit;
  end;

  UpdateNode((TreeDNS.Selected as TDNSTreeNode));
end;

procedure TFrmModuleDNS.Action_PreviousExecute(Sender: TObject);
var
  NewNode: TTreeNode;
begin
  NewNode := fTreeSelectionHistory.Previous;
  if Assigned(NewNode) then
    TreeDNS.Select(NewNode);
end;

procedure TFrmModuleDNS.Action_PropertyExecute(Sender: TObject);
var
  DNSNode: TDNSTreeNode;
  NodeData: PDocVariantData;
begin
  DNSNode := nil;
  if Assigned(GridDNS.FocusedNode) then
  begin
    NodeData := GridDNS.GetNodeAsPDocVariantData(GridDNS.FocusedNode);
    if Assigned(NodeData) and Assigned(TreeDNS.Selected) then
    begin
      if NodeData^.S['name'] = '(Same as parent folder)' then
        DNSNode := (TreeDNS.Selected.FindNode('@') as TDNSTreeNode)
      else
        DNSNode := (TreeDNS.Selected.FindNode(NodeData^.S['name']) as TDNSTreeNode);
    end;
  end;
  if not Assigned(DNSNode) and Assigned(TreeDNS.Selected) then
    DNSNode := (TreeDNS.Selected as TDNSTreeNode);

  if Assigned(DNSNode) and Assigned(DNSNode.fAttributes) then
    FrmRSAT.OpenProperty(DNSNode.fAttributes.Find('distinguishedName').GetReadable(), DNSNode.fAttributes.Find('name').GetReadable());
end;

procedure TFrmModuleDNS.Action_NextExecute(Sender: TObject);
var
  NewNode: TTreeNode;
begin
  NewNode := fTreeSelectionHistory.Next;
  if Assigned(NewNode) then
    TreeDNS.Select(NewNode);
end;

procedure TFrmModuleDNS.Action_DeleteExecute(Sender: TObject);
var
  RowData, Item: PDocVariantData;
  data: TDocVariantData;
  distinguishedName: RawUtf8;
  AttributeToRemove: TLdapAttribute;
  SearchResult: TLdapResult;
  Filter: String;

  procedure InnerDelete(DC: String);
  begin
    repeat
      if not FrmRSAT.LdapClient.Search(DC, False, Filter, ['dnsRecord']) then
      begin
        if Assigned(fLog) then
          fLog.Log(sllError, '% - Ldap Search Error: %', [Action_Delete.Caption, FrmRSAT.LdapClient.ResultString]);
        ShowLdapSearchError(FrmRSAT.LdapClient);
        Exit;
      end;

      for SearchResult in FrmRSAT.LdapClient.SearchResult.Items do
      begin
        if not Assigned(SearchResult) or not data.Exists(SearchResult.ObjectName) then
          continue;
        for Item in data.A[distinguishedName]^.Objects do
        begin
          if not Assigned(Item) then
            continue;
          AttributeToRemove.Add(Item^.S['rawdata']);
        end;
        if AttributesEquals(AttributeToRemove, SearchResult.Find('dnsRecord')) then
        begin
          if Assigned(fLog) then
            fLog.Log(sllInfo, '% - Delete "%"', [Action_Delete.Caption, SearchResult.ObjectName]);
          if not FrmRSAT.LdapClient.Delete(SearchResult.ObjectName) then
          begin
            if Assigned(fLog) then
              fLog.Log(sllError, '% - Ldap Delete Error: %', [Action_Delete.Caption, FrmRSAT.LdapClient.ResultString]);
            ShowLdapDeleteError(FrmRSAT.LdapClient);
            Exit;
          end;
        end
        else
        begin
          if Assigned(fLog) then
            fLog.Log(sllInfo, '% - Modify "%"', [Action_Delete.Caption, SearchResult.ObjectName]);
          if not FrmRSAT.LdapClient.Modify(SearchResult.ObjectName, lmoDelete, AttributeToRemove) then
          begin
            if Assigned(fLog) then
              fLog.Log(sllError, '% - Ldap Modify Error: %', [Action_Delete.Caption, FrmRSAT.LdapClient.ResultString]);
            ShowLdapModifyError(FrmRSAT.LdapClient);
            Exit;
          end;
        end;
      end;
    until FrmRSAT.LdapClient.SearchCookie = '';
  end;

begin
  data.Init();
  for RowData in GridDNS.SelectedRows.Objects do
  begin
    if not Assigned(RowData) or not RowData^.Exists('rawdata') then
      continue;
    data.A_[RowData^.S['distinguishedName']]^.AddItem(RowData^);
  end;

  Filter := '';
  if data.Count > 0 then
  begin
    for DistinguishedName in data.GetNames do
      Filter := FormatUtf8('%(distinguishedName=%)', [Filter, distinguishedName]);
    Filter := FormatUtf8('(|%)', [Filter]);
  end;

  FrmRSAT.LdapClient.SearchBegin();
  AttributeToRemove := TLdapAttribute.Create('dnsRecord', atUndefined);
  try
    FrmRSAT.LdapClient.SearchScope := lssWholeSubtree;

    InnerDelete(FormatUtf8('%,%', [DOMAIN_DNS_ZONES, FrmRSAT.LdapClient.DefaultDN]));
    InnerDelete(FormatUtf8('%,%', [FOREST_DNS_ZONES, FrmRSAT.LdapClient.RootDN]));
  finally
    FrmRSAT.LdapClient.SearchEnd;
    FreeAndNilSafe(AttributeToRemove);
    Action_Refresh.Execute;
  end;
end;

procedure TFrmModuleDNS.Action_NewZoneExecute(Sender: TObject);
var
  NewZone: TVisNewZoneWizard;
begin
  NewZone := TVisNewZoneWizard.Create(Self);

  try
    if NewZone.ShowModal <> mrOK then
      Exit;
    NewZone.Apply(FrmRSAT.LdapClient);
  finally
    FreeAndNil(NewZone);
  end;
  Action_Refresh.Execute;
end;

procedure TFrmModuleDNS.Action_NewZoneUpdate(Sender: TObject);
begin
  {$ifdef DEVMODE}
  Action_NewZone.Enabled := True;
  {$else}
  Action_NewZone.Enabled := False;
  {$endif}
end;

procedure TFrmModuleDNS.Action_OtherNewRecordsExecute(Sender: TObject);
var
  DNSNodeZone, DNSNode: TDNSTreeNode;
  AttributeDNSRecord: TLdapAttribute;
  dcPrefix: String;
  DNSRecord: TDNSRecord;
  i: Integer;
  Serial: Cardinal;
  Found: Boolean;
begin
  dcPrefix := '';
  DNSNodeZone := GetCurrentZone;

  if not Assigned(DNSNodeZone) then
    Exit;

  DNSNode := (DNSNodeZone.FindNode('@') as TDNSTreeNode);
  if Assigned(DNSNode) and Assigned(DNSNode.fAttributes) then
    AttributeDNSRecord := DNSNode.fAttributes.Find('dnsRecord');
  if not Assigned(AttributeDNSRecord) then
    Exit;

  Found := False;
  for i := 0 to AttributeDNSRecord.Count - 1 do
  begin
    if not DNSRecordBytesToRecord(DNSRecord, PByteArray(AttributeDNSRecord.GetRaw(i))^) then
      continue;
    if not (DNSRecord.RecType = Ord(drrSOA)) then
      continue;
    Found := True;
    Serial := DNSRecord.Serial;
    Break;
  end;

  if not Found then
    Exit;


  With TVisSelectNewRecordType.Create(Self, Serial, FrmRSAT.LdapClient, DNSNodeZone.DistinguishedName, dcPrefix) do
  begin
    ShowModal;
  end;
end;

procedure TFrmModuleDNS.Action_OtherNewRecordsUpdate(Sender: TObject);
begin
  Action_OtherNewRecords.Enabled := Assigned(GetCurrentZone);
end;

procedure TFrmModuleDNS.Action_ParentExecute(Sender: TObject);
var
  NewNode: TTreeNode;
begin
  if not Assigned(TreeDNS.Selected) then
    Exit;

  NewNode := TreeDNS.Selected.Parent;
  if not Assigned(NewNode) then
    Exit;

  TreeDNS.Select(NewNode);
end;

constructor TFrmModuleDNS.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Create', [Self.Name]);

  fModule := TModuleADDNS.Create;
  fFrmOption := nil;

  FrmRSAT.LdapClient.RegisterObserverConnect(@OnLdapClientConnect);
  FrmRSAT.LdapClient.RegisterObserverClose(@OnLdapClientClose);

  {$IFDEF WINDOWS}
  Image1.Visible := not IsDarkModeEnabled;
  Image2.Visible := IsDarkModeEnabled;
  {$ELSE}
  Image1.Visible := True;
  Image2.Visible := False;
  {$ENDIF}

end;

destructor TFrmModuleDNS.Destroy;
begin
  FreeAndNil(fTreeSelectionHistory);
  FreeAndNil(fModule);

  inherited Destroy;
end;

procedure TFrmModuleDNS.Refresh;
begin
  fRootNode.Text := FrmRSAT.LdapClient.Settings.TargetHost;
  Action_Refresh.Execute;
end;

procedure TFrmModuleDNS.Load;
begin
  fTreeSelectionHistory := TTreeSelectionHistory.Create;

  fRootNode := (TreeDNS.Items.Add(nil, '') as TDNSTreeNode);
  fForwardLookupZonesNode := (TreeDNS.Items.AddChild(fRootNode, 'Forward Lookup Zones') as TDNSTreeNode);
  fForwardLookupZonesNode.NodeType := dtntCustom;
  fReverseLookupZonesNode := (TreeDNS.Items.AddChild(fRootNode, 'Reverse Lookup Zones') as TDNSTreeNode);
  fReverseLookupZonesNode.NodeType := dtntCustom;
  fRootNode.Expand(False);
  fRootNode.Selected := True;
end;

function TFrmModuleDNS.GetModule: TModule;
begin
  result := fModule;
end;

function TFrmModuleDNS.GetFrmOption: TFrameOption;
begin
  result := fFrmOption;
end;

function TFrmModuleDNS.GetFrmOptionClass: TFrameOptionClass;
begin
  result := nil;
end;

end.

