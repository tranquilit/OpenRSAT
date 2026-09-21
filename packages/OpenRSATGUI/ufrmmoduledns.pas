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
  tis.ui.searchedit,
  ucoredatamodule,
  mormot.core.variants,
  mormot.core.log,
  utreeselectionhistory,
  VirtualTrees,
  ufrmmodule,
  ufrmoption,
  umodule,
  umoduleaddns,
  ursatldapclient,
  uopenrsatuicontextinterface,
  udnsservice,
  ulog;

type

  /// defines node kinds used to determine a tree node kind
  TDnsTreeNodeKind = (
    /// root node, corresponding to a DC
    dtnkRoot,
    /// represents the forward and reverse lookup zone folders
    dtnkZonesFolder,
    /// represent a LDAP dnsZone
    dtnkZone,
    /// represent a folder of a dnsNode
    dtnkNodesFolder
  );

  /// stores the data of a dns tree node
  TDnsTreeNodeData = class
  public
    /// tree node kind
    Kind: TDnsTreeNodeKind;
    /// full distinguished name of the node. Used to retrieve LDAP data from a node
    DistinguishedName: RawUtf8;
    /// not used
    Loaded: Boolean;
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
    Label_StatusMessage: TLabel;
    Label_NodeCountMessage: TLabel;
    Label_RecordCountMessage: TLabel;
    Label_ErrorMessage: TLabel;
    Label_Error: TLabel;
    Label_RecordCount: TLabel;
    Label_Status: TLabel;
    Label_NodeCount: TLabel;
    MenuItem10: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel_Error: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel_Status: TPanel;
    Panel_NodeCount: TPanel;
    Panel_RecordCount: TPanel;
    PopupMenu_DNS: TPopupMenu;
    Separator1: TMenuItem;
    Splitter1: TSplitter;
    GridDNS: TTisGrid;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Timer_TreeChangeNode: TTimer;
    Timer_SearchInGrid: TTimer;
    TisSearchEdit_GridDNS: TTisSearchEdit;
    TisSearchEdit_TreeDNS: TTisSearchEdit;
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
    procedure Action_DeleteUpdate(Sender: TObject);
    procedure Action_NewZoneExecute(Sender: TObject);
    procedure Action_NewZoneUpdate(Sender: TObject);
    procedure Action_NextExecute(Sender: TObject);
    procedure Action_OtherNewRecordsExecute(Sender: TObject);
    procedure Action_OtherNewRecordsUpdate(Sender: TObject);
    procedure Action_ParentExecute(Sender: TObject);
    procedure Action_PreviousExecute(Sender: TObject);
    procedure Action_PropertyExecute(Sender: TObject);
    procedure Action_PropertyUpdate(Sender: TObject);
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
    procedure TisSearchEdit_GridDNSSearch(Sender: TObject; const aText: string);
    procedure TisSearchEdit_TreeDNSSearch(Sender: TObject; const aText: string);
    procedure TreeDNSDeletion(Sender: TObject; Node: TTreeNode);
    procedure TreeDNSExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure TreeDNSMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TreeDNSSelectionChanged(Sender: TObject);
  private
    fLog: TSynLogClass;
    fIContext: IOpenRSATUIContext;
    fTreeSelectionHistory: TTreeSelectionHistory;

    fModule: TModuleADDNS;

    fSearchWord: RawUtf8;

    function GetLdapClient: TRsatLdapClient;
    /// set the columns visibility in grid
    procedure UpdateGridColumns(Names: TStringArray);
    /// create the root node
    procedure SetupRootNode;
    /// ldap connect event callback
    procedure LdapConnectEvent(Sender: TObject);
    /// ldap close event callback
    procedure LdapCloseEvent(Sender: TObject);
  private
    /// get the focused object full distinguished name
    // - look for a focused object in the grid, then fallback to the tree
    function GetFocusedObject: RawUtf8;
    /// ask user confirmation before object deletion
    procedure DoBeforeDelete(out Allow: Boolean);
  public
    /// delete the selected object
    procedure DoDelete;
    /// open the new zone wizard
    procedure DoNewZone;
    /// open the new record wizard
    procedure DoNewRecord;
    /// open the property of the selected object
    procedure DoOpenProperty;
    /// refresh the selected node
    procedure DoRefresh;
  private
    fDnsService: TDNSService;

    /// delete children of selected node before an updateNode
    procedure RefreshNode(const SelectedNode: TTreeNode);
    /// update the selected node
    procedure UpdateNode(const SelectedNode: TTreeNode);
    /// update the selected root node
    procedure UpdateRootNode(const SelectedNode: TTreeNode);
    /// update the selected zone folder node
    procedure UpdateZonesFolderNode(const SelectedNode: TTreeNode);
    /// update the selected zone node
    procedure UpdateZoneNode(const SelectedNode: TTreeNode);
    /// update the nodes folder node
    procedure UpdateNodesFolderNode(const SelectedNode: TTreeNode);

    /// display the dns zones in the grid and tree
    procedure DisplayZones(const SelectedNode: TTreeNode; const DNSZones: TDNSZoneDynArray);
    /// display the dns nodes in the grid and tree
    procedure DisplayNodes(const SelectedNode: TTreeNode; const DNSNodes: TDNSNodeDynArray);
    /// display the dns nodes in the grid
    // - the tree is updated by the DisplayNodes
    procedure DisplayNodesFolder(const SelectedNode: TTreeNode; const DNSNodes: TDNSNodeDynArray);
  public
    constructor Create(Context: IOpenRSATUIContext); reintroduce;
    destructor Destroy; override;

    property LdapClient: TRsatLdapClient read GetLdapClient;

  protected
    function GetModule: TModule; override;
    function GetFrmOptionClass: TFrameOptionClass; override;
    function GetOnLdapConnect: TNotifyEvent; override;
    function GetOnLdapClose: TNotifyEvent; override;
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
  utheme,
  uvisnewzonewizard,
  ursatldapclientui,
  udns,
  uhelpers;

{$R *.lfm}

{ TFrmModuleDNS }

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

procedure TFrmModuleDNS.TreeDNSSelectionChanged(Sender: TObject);
begin
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, 'Node selection change', Self);

  GridDNS.Clear;

  if Timer_TreeChangeNode.Enabled then
    Timer_TreeChangeNode.Enabled := False;
  Timer_TreeChangeNode.Enabled := True;
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

procedure TFrmModuleDNS.SetupRootNode;
var
  RootNode: TTreeNode;
  Data: TDnsTreeNodeData;
begin
  RootNode := TreeDNS.Items.Add(nil, rsEmpty);

  Data := TDnsTreeNodeData.Create;
  Data.Kind := dtnkRoot;
  RootNode.Data := Data;

  RootNode.ImageIndex := Ord(ileADContainer);
  RootNode.SelectedIndex := RootNode.ImageIndex;
  RootNode.Expand(False);
  RootNode.Selected := True;
end;

function TFrmModuleDNS.GetLdapClient: TRsatLdapClient;
begin
  result := fModule.RSAT.LdapClient;
end;

procedure TFrmModuleDNS.LdapConnectEvent(Sender: TObject);
begin
  fModule.NeedRefresh := True;
  TreeDNS.Items.GetFirstNode.Text := (Sender as TLdapClient).Settings.TargetHost;
end;

procedure TFrmModuleDNS.LdapCloseEvent(Sender: TObject);
begin
  GridDNS.Clear;
  TreeDNS.Items.GetFirstNode.Text := rsEmpty;
end;

function TFrmModuleDNS.GetFocusedObject: RawUtf8;
begin
  result := '';
  try
    if GridDNS.Focused and (GridDNS.SelectedCount > 0) then
      result := GridDNS.SelectedRows._[0]^.S['objectName']
    else
      result := TDnsTreeNodeData(TreeDNS.Selected.Data).DistinguishedName;
  except
    on E: Exception do
      result := '';
  end;
end;

procedure TFrmModuleDNS.DoBeforeDelete(out Allow: Boolean);
begin
  if MessageDlg(rsTitleDeleteObject, rsDeleteObjectsConfirmation, mtConfirmation, mbYesNoCancel, 0) <> mrYes then
  begin
    Allow := False;
    if Assigned(fLog) then
      fLog.Add.Log(sllInfo, 'Action cancelled by user.', Self);
    Exit;
  end;
  Allow := True;
end;

procedure TFrmModuleDNS.DoDelete;
var
  Allow: Boolean;
  RowData: PDocVariantData;
begin
  DoBeforeDelete(Allow);
  if not Allow then
    Exit;

  for RowData in GridDNS.SelectedRows.Objects do
  begin
    if not Assigned(RowData) then
      continue;

    if RowData^.Exists('rawdata') then
      LdapClient.Modify(RowData^.S['objectName'], lmoDelete, 'dnsRecord', RowData^.S['rawdata'])
    else
      LdapClient.Delete(RowData^.S['objectName'], True);
  end;

  DoRefresh;
end;

procedure TFrmModuleDNS.DoNewZone;
var
  Vis: TVisNewZoneWizard;
begin
  Vis := TVisNewZoneWizard.Create(Self);
  try
    if Vis.ShowModal <> mrOK then
      Exit;
    Vis.Apply(LdapClient);
  finally
    FreeAndNil(Vis);
  end;
  DoRefresh;
end;

procedure TFrmModuleDNS.DoNewRecord;
var
  Vis: TVisSelectNewRecordType;
  ParentNode: TTreeNode;
  DNSRecordAttr: TLdapAttribute;
  i: Integer;
  DNSRecord: TDNSRecord;
  SOA: TRRSOA;
  Serial: Cardinal;
  dcPrefix: RawUtf8;
begin
  ParentNode := TreeDNS.Selected;
  dcPrefix := '';
  while TDnsTreeNodeData(ParentNode.Data).Kind = dtnkNodesFolder do
  begin
    ParentNode := ParentNode.Parent;
  end;
  if TDnsTreeNodeData(ParentNode.Data).Kind <> dtnkZone then
    Exit;

  DNSRecordAttr := LdapClient.SearchObject(TDnsTreeNodeData(ParentNode.Data).DistinguishedName, '(&(objectClass=dnsNode)(dc=@))', 'dnsRecord', lssSingleLevel);
  if not Assigned(DNSRecordAttr) then
    Exit;
  for i := 0 to DNSRecordAttr.Count - 1 do
  begin
    if not DNSRecordBytesToRecord(DNSRecord, PByteArray(DNSRecordAttr.GetRaw(i))^) then
      Continue;
    if DNSRecord.RecType <> Ord(drrSOA) then
      Continue;
    if not DNSRRSOABytesToRecord(SOA, PByteArray(@DNSRecord.RData)^) then
      Continue;
    Serial := SOA.Serial;
    Break;
  end;

  Vis := TVisSelectNewRecordType.Create(Self, Serial, LdapClient, TDnsTreeNodeData(ParentNode.Data).DistinguishedName, dcPrefix);
  try
    Vis.ShowModal;
  finally
    FreeAndNil(Vis);
    DoRefresh;
  end;
end;

procedure TFrmModuleDNS.DoOpenProperty;
var
  DistinguishedName: RawUtf8;
begin
  DistinguishedName := GetFocusedObject;

  if DistinguishedName <> '' then
    fIContext.OpenProperty(DistinguishedName);
end;

procedure TFrmModuleDNS.DoRefresh;
begin
  RefreshNode(TreeDNS.Selected);
end;

procedure TFrmModuleDNS.RefreshNode(const SelectedNode: TTreeNode);
var
  Data: TDnsTreeNodeData;
begin
  if not Assigned(SelectedNode) then
    Exit;
  Data := TDnsTreeNodeData(SelectedNode.Data);
  if not Assigned(Data) then
    Exit;
  SelectedNode.DeleteChildren;
  UpdateNode(SelectedNode);
  SelectedNode.Expand(False);
end;

procedure TFrmModuleDNS.UpdateNode(const SelectedNode: TTreeNode);
var
  Data: TDnsTreeNodeData;
  Bak: TCursor;
begin
  if not Assigned(SelectedNode) then
    Exit;

  Data := TDnsTreeNodeData(SelectedNode.Data);

  if not Assigned(Data) then
    Exit;

  Bak := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
    case Data.Kind of
      dtnkRoot: UpdateRootNode(SelectedNode);
      dtnkZonesFolder: UpdateZonesFolderNode(SelectedNode);
      dtnkZone: UpdateZoneNode(SelectedNode);
      dtnkNodesFolder: UpdateNodesFolderNode(SelectedNode);
    end;
  finally
    Screen.Cursor := Bak;
  end;
end;

procedure TFrmModuleDNS.UpdateRootNode(const SelectedNode: TTreeNode);
var
  ForwardLookupZones, ReverseLookupZones: TTreeNode;
  Data: TDnsTreeNodeData;
  NewRow: TDocVariantData;
  GridData: TDocVariantData;
begin
  if not Assigned(SelectedNode) then
    Exit;

  ForwardLookupZones := SelectedNode.FindNode(rsDNSForwardLookupZones);
  if not Assigned(ForwardLookupZones) then
  begin
    ForwardLookupZones := TreeDNS.Items.AddChild(SelectedNode, rsDNSForwardLookupZones);
    ForwardLookupZones.ImageIndex := Ord(ileADContainer);
    ForwardLookupZones.SelectedIndex := ForwardLookupZones.ImageIndex;
    ForwardLookupZones.HasChildren := True;
    Data := TDnsTreeNodeData.Create;
    Data.Kind := dtnkZonesFolder;
    ForwardLookupZones.Data := Data;
  end;

  ReverseLookupZones := SelectedNode.FindNode(rsDNSReverseLookupZones);
  if not Assigned(ReverseLookupZones) then
  begin
    ReverseLookupZones := TreeDNS.Items.AddChild(SelectedNode, rsDNSReverseLookupZones);
    ReverseLookupZones.ImageIndex := Ord(ileADContainer);
    ReverseLookupZones.SelectedIndex := ReverseLookupZones.ImageIndex;
    ReverseLookupZones.HasChildren := True;
    Data := TDnsTreeNodeData.Create;
    Data.Kind := dtnkZonesFolder;
    ReverseLookupZones.Data := Data;
  end;

  UpdateGridColumns(['name']);
  GridData.Init(JSON_FAST);
  NewRow.Init(JSON_FAST);
  NewRow.AddValue('name', rsDNSForwardLookupZones);
  GridData.AddItem(NewRow);
  NewRow.Clear;
  NewRow.Init(JSON_FAST);
  NewRow.AddValue('name', rsDNSReverseLookupZones);
  GridData.AddItem(NewRow);

  GridDNS.LoadData(@GridData);
  GridDNS.UpdateSelectedAndTotalLabel;
end;

procedure TFrmModuleDNS.UpdateZonesFolderNode(const SelectedNode: TTreeNode);
var
  DNSZones: TDNSZoneDynArray;
begin
  if not Assigned(SelectedNode) then
    Exit;

  fDnsService.OnSearchPageZones := nil; // @OnSearchPageZones;
  if SelectedNode.Text = rsDNSForwardLookupZones then
    DNSZones := fDnsService.SearchForwardZones
  else if SelectedNode.Text = rsDNSReverseLookupZones then
    DNSZones := fDnsService.SearchReverseZones
  else
    Exit;

  DisplayZones(SelectedNode, DNSZones);
end;

procedure TFrmModuleDNS.UpdateZoneNode(const SelectedNode: TTreeNode);
var
  Data: TDnsTreeNodeData;
  DNSNodes: TDNSNodeDynArray;
begin
  Data := TDnsTreeNodeData(SelectedNode.Data);
  fDnsService.OnSearchPageNodes := nil; // @OnSearchPageNodes;
  DNSNodes := fDnsService.SearchNodes(Data.DistinguishedName);

  DisplayNodes(SelectedNode, DNSNodes);
end;

procedure TFrmModuleDNS.UpdateNodesFolderNode(const SelectedNode: TTreeNode);
var
  Data: TDnsTreeNodeData;
  DNSNodes: TDNSNodeDynArray;
begin
  Data := TDnsTreeNodeData(SelectedNode.Data);
  fDnsService.OnSearchPageNodes := nil; // @OnSearchPageNodes;
  DNSNodes := fDnsService.SearchNodes(GetParentDN(Data.DistinguishedName));

  DisplayNodesFolder(SelectedNode, DNSNodes);
end;

procedure TFrmModuleDNS.DisplayZones(const SelectedNode: TTreeNode;
  const DNSZones: TDNSZoneDynArray);
var
  DNSZone: TDNSZone;
  Child: TTreeNode;
  Data: TDnsTreeNodeData;
  NewRow: TDocVariantData;
  GridData: TDocVariantData;
begin
  GridData.Init(JSON_FAST);
  for DNSZone in DNSZones do
  begin
    NewRow.Init(JSON_FAST);
    NewRow.AddValue('name', DNSZone.Name);
    NewRow.AddValue('objectName', DNSZone.DistinguishedName);
    GridData.AddItem(NewRow);
    NewRow.Clear;

    Child := SelectedNode.FindNode(DNSZone.Name);
    if Assigned(Child) then
      Continue;
    Child := TreeDNS.Items.AddChild(SelectedNode, DNSZone.Name);
    Child.ImageIndex := 59;
    Child.SelectedIndex := Child.ImageIndex;
    Child.HasChildren := True;
    Data := TDnsTreeNodeData.Create;
    Data.Kind := dtnkZone;
    Data.DistinguishedName := DNSZone.DistinguishedName;
    Child.Data := Data;
  end;

  UpdateGridColumns(['name', 'type', 'status', 'dnssec', 'keymaster']);
  GridDNS.LoadData(@GridData);
  GridDNS.UpdateSelectedAndTotalLabel;
end;

function ReverseDnsToIP(const AZone, ARecord: RawUtf8): RawUtf8;
var
  Parts: TRawUtf8DynArray;
  i: Integer;

  function EndsWithText(const Value, Suffix: RawUtf8): Boolean;
  begin
    result := False;
    if Length(Value) < Length(Suffix) then
      Exit;
    result := Copy(Value, Length(Value) - Length(Suffix) + 1, Length(Suffix)) = Suffix;
  end;

  procedure AddParts(const Value: RawUtf8);
  begin
    Parts := Concat(Parts ,TRawUtf8DynArray(String(Value).Split('.')));
  end;

begin
  result := '';
  Parts := nil;
  if EndsWithText(AZone, 'in-addr.arpa') then
  begin
    AddParts(ARecord);
    AddParts(Copy(AZone, 0, Length(AZone) - Length('in-addr.arpa')));

    for i := 0 to 3 do
    begin
      if i > 0 then
        result := FormatUtf8('.%', [result]);
      result := FormatUtf8('%%', [parts[i], result]);
    end;
  end
  else if EndsWithText(AZone, 'ip6.arpa') then
  begin

  end;
end;

procedure DNSRecordToData(out D: TDocVariantData; const ZoneN, NodeN, ObjectN: RawUtf8; const R: RawByteString);
var
  DNSRecord: TDNSRecord;
begin
  D.Init(JSON_FAST);
  if not DNSRecordBytesToRecord(DNSRecord, PByteArray(R)^) then
    Exit;
  if TDnsResourceRecord(DNSRecord.RecType) = drrPTR then
    D.AddValue('name', ReverseDnsToIP(ZoneN, NodeN))
  else
    D.AddValue('name', NodeN);
  D.AddValue('data', DNSRecordDataToString(DNSRecord));
  D.AddValue('_type', DNSRecord.RecType);
  D.AddValue('type',  DnsResourceRecordToStr(TDnsResourceRecord(dnsRecord.RecType)));
  D.AddValue('timestamp', '');
  D.AddValue('rawdata', R);
  D.AddValue('objectName', ObjectN);
end;

procedure TFrmModuleDNS.DisplayNodes(const SelectedNode: TTreeNode;
  const DNSNodes: TDNSNodeDynArray);
var
  DNSNode: TDNSNode;
  Paths: TAnsiStringArray;
  ParentNode, ChildNode: TTreeNode;
  i: Integer;
  Data: TDnsTreeNodeData;
  GridData, NewRow: TDocVariantData;
begin
  GridData.Init(JSON_FAST);
  for i := 0 to SelectedNode.Count - 1 do
  begin
    NewRow.Init(JSON_FAST);
    NewRow.AddValue('name', SelectedNode.Items[i].Text);
    GridData.AddItem(NewRow);
    NewRow.Clear;
  end;

  for DNSNode in DNSNodes do
  begin
    if DNSNode.Name = '@' then
    begin
      for i := 0 to Length(DNSNode.DnsRecords) - 1 do
      begin
        DNSRecordToData(NewRow, '', rsDNSSameAsParentFolder, DNSNode.DistinguishedName, DNSNode.DnsRecords[i]);
        GridData.AddItem(NewRow);
        NewRow.Clear;
      end;
      Continue;
    end;

    if SelectedNode.Parent.Text = rsDNSReverseLookupZones then
    begin
      for i := 0 to Length(DNSNode.DnsRecords) - 1 do
      begin
        DNSRecordToData(NewRow, SelectedNode.Text, DNSNode.Name, DNSNode.DistinguishedName, DNSNode.DnsRecords[i]);
        GridData.AddItem(NewRow);
        NewRow.Clear;
      end;
      Continue;
    end;

    Paths := String(DNSNode.Name).Split('.');
    if Length(Paths) = 1 then
    begin
      for i := 0 to Length(DNSNode.DnsRecords) - 1 do
      begin
        DNSRecordToData(NewRow, '', DNSNode.Name, DNSNode.DistinguishedName, DNSNode.DnsRecords[i]);
        GridData.AddItem(NewRow);
        NewRow.Clear;
      end;
      Continue;
    end;
    ParentNode := SelectedNode;
    for i := High(Paths) downto 1 do
    begin
      ChildNode := ParentNode.FindNode(Paths[i]);
      if not Assigned(ChildNode) then
      begin
        ChildNode := TreeDNS.Items.AddChild(ParentNode, Paths[i]);
        ChildNode.ImageIndex := Ord(ileADContainer);
        ChildNode.SelectedIndex := ChildNode.ImageIndex;
        ChildNode.HasChildren := True;
        Data := TDnsTreeNodeData.Create;
        Data.Kind := dtnkNodesFolder;
        Data.DistinguishedName := DNSNode.DistinguishedName;
        ChildNode.Data := Data;
      end;
      ParentNode := ChildNode;
    end;
  end;
  UpdateGridColumns(['name', 'type', 'data', 'timestamp']);
  GridDNS.LoadData(@GridData);
  GridDNS.UpdateSelectedAndTotalLabel;
end;

procedure TFrmModuleDNS.DisplayNodesFolder(const SelectedNode: TTreeNode;
  const DNSNodes: TDNSNodeDynArray);
var
  ParentNode: TTreeNode;
  Path, RelativeName: RawUtf8;
  DNSNode: TDNSNode;
  i: Integer;
  GridData, NewRow: TDocVariantData;
  p: SizeInt;
begin
  Path := '';
  ParentNode := SelectedNode;
  while (TDnsTreeNodeData(ParentNode.Data).Kind = dtnkNodesFolder) do
  begin
    if Path = '' then
      Path := ParentNode.Text
    else
      Path := FormatUtf8('%.%', [Path, ParentNode.Text]);
    ParentNode := ParentNode.Parent;
  end;
  GridData.Init(JSON_FAST);
  /// Add folders
  for i := 0 to SelectedNode.Count - 1 do
  begin
    NewRow.Init(JSON_FAST);
    NewRow.AddValue('name', SelectedNode.Items[i].Text);
    GridData.AddItem(NewRow);
    NewRow.Clear;
  end;
  /// Add records
  for DNSNode in DNSNodes do
  begin
    if not String(DNSNode.Name).EndsWith(Path) then
      Continue;
    if DNSNode.Name = Path then
    begin
      for i := 0 to Length(DNSNode.DnsRecords) - 1 do
      begin
        DNSRecordToData(NewRow, '', rsDNSSameAsParentFolder, DNSNode.DistinguishedName, DNSNode.DnsRecords[i]);
        GridData.AddItem(NewRow);
        NewRow.Clear;
      end;
      Continue;
    end;
    RelativeName := Copy(DNSNode.Name, 0, Length(DNSNode.Name) - Length(Path) - 1);
    p := Pos('.', RelativeName);
    if p = 0 then
    begin
      for i := 0 to Length(DNSNode.DnsRecords) - 1 do
      begin
        DNSRecordToData(NewRow, '', RelativeName, DNSNode.DistinguishedName, DNSNode.DnsRecords[i]);
        GridData.AddItem(NewRow);
        NewRow.Clear;
      end;
    end;
  end;
  UpdateGridColumns(['name', 'type', 'data', 'timestamp']);
  GridDNS.LoadData(@GridData);
  GridDNS.UpdateSelectedAndTotalLabel;
end;

procedure TFrmModuleDNS.Action_RefreshExecute(Sender: TObject);
begin
  DoRefresh;
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
    if NodeData^.S['name'] = rsDNSSameAsParentFolder then
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
        DoOpenProperty;
    end;
  end;
end;

procedure TFrmModuleDNS.GridDNSGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  NodeData: PDocVariantData;
  NodeIsFolder: Boolean;
  Data: TDnsTreeNodeData;
begin
  if GridDNS.FindColumnByIndex(Column).PropertyName = 'name' then
  begin
    ImageIndex := Ord(ileADUnknown);
    if not Assigned(TreeDNS.Selected) then
      Exit;

    Data := TDNSTreeNodeData(TreeDNS.Selected.Data);
    case Data.Kind of
      dtnkZonesFolder: ImageIndex := 59;
      dtnkRoot: ImageIndex := Ord(ileADContainer);
      dtnkZone, dtnkNodesFolder:
      begin
        NodeData := GridDNS.GetNodeAsPDocVariantData(Node);
        if not Assigned(NodeData) then
          Exit;
        NodeIsFolder := Assigned(TreeDNS.Selected.FindNode(NodeData^.U['name']));
        if NodeIsFolder then
        begin
          ImageIndex := Ord(ileADContainer);
          Exit;
        end;
        if NodeData^.Exists('_type') and (NodeData^.I['_type'] <> 0) then
          ImageIndex := 57;
      end;
    end;
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
    fLog.Add.Log(sllTrace, 'Timer node changed', Self);

  Timer_TreeChangeNode.Enabled := False;

  UpdateNode(TreeDNS.Selected);
end;

procedure TFrmModuleDNS.TisSearchEdit_GridDNSSearch(Sender: TObject;
  const aText: string);
var
  NodeData: PDocVariantData;
  Node: PVirtualNode;
  lowerText: String;
  FieldName: RawUtf8;
  Filtered: Boolean;
begin
  lowerText := aText.ToLower;
  GridDNS.BeginUpdate;
  Node := GridDNS.GetFirst();
  while Assigned(Node) do
  begin
    if (aText = '') then
    begin
      GridDNS.IsFiltered[Node] := False;
      Node := GridDNS.GetNext(Node);
      Continue;
    end;

    NodeData := GridDNS.GetNodeAsPDocVariantData(Node);
    if not Assigned(NodeData) then
    begin
      GridDNS.IsFiltered[Node] := True;
      Node := GridDNS.GetNext(Node);
      Continue;
    end;

    with NodeData^ do
    begin
      Filtered := True;
      for FieldName in Names do
      begin
        if not GridDNS.IsVisibleColumnByPropertyName(FieldName) or not Exists(FieldName) then
          Continue;
        Filtered := not S[FieldName].ToLower.Contains(lowerText);
        if not Filtered then
          Break;
      end;
      GridDNS.IsFiltered[Node] := Filtered;
    end;
    Node := GridDNS.GetNext(Node);
  end;
  GridDNS.EndUpdate;
end;

procedure TFrmModuleDNS.TisSearchEdit_TreeDNSSearch(Sender: TObject;
  const aText: string);
var
  Node: TTreeNode;
  LowerText: String;

  function HasVisibleChild(Node: TTreeNode; LowerText: String): Boolean;
  var
    ChildNode: TTreeNode;
  begin
    result := False;
    ChildNode := Node.GetFirstChild;
    if Assigned(ChildNode) then
    begin
      repeat
        ChildNode.Visible := HasVisibleChild(ChildNode, LowerText) or (LowerText = '') or (ChildNode.Text.ToLower.Contains(LowerText));
        if ChildNode.Visible and (LowerText <> '') then
          ChildNode.Expand(False);
        result := ChildNode.Visible or result;
        ChildNode := Node.GetNextChild(ChildNode);
      until not Assigned(ChildNode);
    end;
  end;

begin
  Node := TreeDNS.Items.GetFirstNode;
  if not Assigned(Node) then
    Exit;

  LowerText := aText.ToLower;

  repeat
    Node.Visible := (HasVisibleChild(Node, LowerText)) or (LowerText = '') or (Node.Text.ToLower.Contains(LowerText));
    Node := Node.GetNextSibling;
  until not Assigned(Node);
end;

procedure TFrmModuleDNS.TreeDNSDeletion(Sender: TObject; Node: TTreeNode);
var
  Data: TDnsTreeNodeData;
begin
  Data := TDnsTreeNodeData(Node.Data);
  if Assigned(Data) then
    FreeAndNil(Data);
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
begin
  DoOpenProperty;
end;

procedure TFrmModuleDNS.Action_PropertyUpdate(Sender: TObject);
begin
  Action_Property.Enabled := Assigned(LdapClient) and LdapClient.Connected and (GridDNS.SelectedCount > 0)
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
begin
  DoDelete;
end;

procedure TFrmModuleDNS.Action_DeleteUpdate(Sender: TObject);
begin
  Action_Delete.Enabled := (GridDNS.Focused) and (GridDNS.SelectedCount > 0);
end;

procedure TFrmModuleDNS.Action_NewZoneExecute(Sender: TObject);
begin
  DoNewZone;
end;

procedure TFrmModuleDNS.Action_NewZoneUpdate(Sender: TObject);
begin
  Action_NewZone.Enabled := Assigned(TreeDNS.Selected) and (TDnsTreeNodeData(TreeDNS.Selected.Data).Kind = dtnkZonesFolder);
end;

procedure TFrmModuleDNS.Action_OtherNewRecordsExecute(Sender: TObject);
begin
  DoNewRecord;
end;

procedure TFrmModuleDNS.Action_OtherNewRecordsUpdate(Sender: TObject);
begin
  Action_OtherNewRecords.Enabled := Assigned(TreeDNS.Selected) and ((TDnsTreeNodeData(TreeDNS.Selected.Data).Kind = dtnkZone) or (TDnsTreeNodeData(TreeDNS.Selected.Data).Kind = dtnkNodesFolder));
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

constructor TFrmModuleDNS.Create(Context: IOpenRSATUIContext);
begin
  inherited Create(Context.ComponentOwner);

  fLog := TADDNSLog;
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, '% - Create', [Self.Name]);

  fIContext := Context;
  fModule := TModuleADDNS.Create(Context.RSAT);

  Image1.Visible := not IsDarkMode;
  Image2.Visible := not Image1.Visible;

  SetupRootNode;

  PageControl1.ActivePageIndex := 0;

  fDnsService := TDNSService.Create(LdapClient);
end;

destructor TFrmModuleDNS.Destroy;
begin
  FreeAndNil(fTreeSelectionHistory);
  FreeAndNil(fModule);
  FreeAndNil(fDnsService);

  inherited Destroy;
end;

procedure TFrmModuleDNS.Refresh;
begin
  Action_Refresh.Execute;
end;

procedure TFrmModuleDNS.Load;
begin
  fTreeSelectionHistory := TTreeSelectionHistory.Create;
end;

function TFrmModuleDNS.GetModule: TModule;
begin
  result := fModule;
end;

function TFrmModuleDNS.GetFrmOptionClass: TFrameOptionClass;
begin
  result := nil;
end;

function TFrmModuleDNS.GetOnLdapConnect: TNotifyEvent;
begin
  result := @LdapConnectEvent;
end;

function TFrmModuleDNS.GetOnLdapClose: TNotifyEvent;
begin
  result := @LdapCloseEvent;
end;

end.

