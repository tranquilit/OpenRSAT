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
  ursatldapclient;

type

  TDNSTreeNodeType = (dtntRoot, dtntCustom, dtntZone, dtntZoneFolder);

  { TDNSTreeNode }

  TDNSTreeNode = class(TTreeNode)
  private
    fType: TDNSTreeNodeType;
    fAttributes: TLdapAttributeList;
    fDistinguishedName: String;
    fRetrieved: Boolean;
    procedure SetNodeType(AValue: TDNSTreeNodeType);
  public
    constructor Create(AnOwner: TTreeNodes); override;
    destructor Destroy; override;

    function ZoneFolderPath: RawUtf8;

    function HasVisibleChildren: Boolean;
    property NodeType: TDNSTreeNodeType read fType write SetNodeType default dtntRoot;
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
    Label2: TLabel;
    Label3: TLabel;
    MenuItem10: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    PopupMenu_DNS: TPopupMenu;
    ProgressBar1: TProgressBar;
    Separator1: TMenuItem;
    Splitter1: TSplitter;
    GridDNS: TTisGrid;
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

    fRootNode: TDNSTreeNode;
    fForwardLookupZonesNode: TDNSTreeNode;
    fReverseLookupZonesNode: TDNSTreeNode;

    fSearchWord: RawUtf8;

    procedure UpdateGridColumns(Names: TStringArray);
    procedure UpdateNodeRoot(Node: TDNSTreeNode);
    procedure UpdateNodeCustom(Node: TDNSTreeNode);
    procedure UpdateNodeZoneFolder(Node: TDNSTreeNode);
    procedure UpdateNodeZone(Node: TDNSTreeNode);
    procedure UpdateZoneOnStatus(const status: String);
    procedure UpdateZoneOnFinished();

    /// Update data about a TreeNode.
    procedure UpdateNode(Node: TDNSTreeNode = nil);
    /// Update grid from data on a TreeNode.
    procedure UpdateGrid(Node: TDNSTreeNode = nil);

    procedure ReloadZones;
    procedure TreeUpdateZones;

    procedure LdapConnectEvent(Sender: TObject);
    procedure LdapCloseEvent(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

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
  uvisnewresourcerecord,
  ursatldapclientui,
  udns,
  uhelpers,
  ufrmrsat;

{$R *.lfm}

{ TDNSTreeNode }

procedure TDNSTreeNode.SetNodeType(AValue: TDNSTreeNodeType);
begin
  if fType = AValue then
    Exit;
  fType := AValue;
  case fType of
    dtntRoot, dtntCustom: ImageIndex := Ord(ileADContainer);
    dtntZoneFolder: ImageIndex := 57;
    dtntZone: ImageIndex := 59;
  end;
  SelectedIndex := ImageIndex;
end;

constructor TDNSTreeNode.Create(AnOwner: TTreeNodes);
begin
  inherited Create(AnOwner);

  ImageIndex := Ord(ileADContainer);
  SelectedIndex := Ord(ileADContainer);

  fRetrieved := False;
  fDistinguishedName := '';
  fAttributes := nil;
end;

destructor TDNSTreeNode.Destroy;
begin
  FreeAndNil(fAttributes);

  inherited Destroy;
end;

function TDNSTreeNode.ZoneFolderPath: RawUtf8;
var
  ParentNode: TDNSTreeNode;
  ResultPath: TRawUtf8DynArray;
begin
  ResultPath := nil;
  result := '';
  ParentNode := Self;
  repeat
    Insert(ParentNode.Text, ResultPath, Length(ResultPath));
    ParentNode := (ParentNode.Parent as TDNSTreeNode);
  until ParentNode.fType <> dtntZoneFolder;
  if ParentNode.fType = dtntZone then
    result := FormatUtf8('.%', [String.Join('.', TStringArray(ResultPath))]);
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
  GridDNS.LoadData;
end;

procedure TFrmModuleDNS.UpdateNodeCustom(Node: TDNSTreeNode);
begin
  UpdateGridColumns(['name', 'type', 'status', 'dnssec', 'keymaster']);
  ReloadZones;
  UpdateGrid();
end;

procedure TFrmModuleDNS.UpdateNodeZoneFolder(Node: TDNSTreeNode);
var
  newRaw: TDocVariantData;
  i, j: Integer;
  DNSRecordAttribute: TLdapAttribute;
  RecordName: String;
  DNSRecord: TDNSRecord;
begin
  newRaw.Init();
  UpdateGridColumns(['name', 'type', 'data', 'timestamp']);

  UpdateGrid();
end;

procedure TFrmModuleDNS.UpdateNodeZone(Node: TDNSTreeNode);
begin
  UpdateGridColumns(['name', 'type', 'data', 'timestamp']);

  Panel5.Visible := True;
  GridDNS.Visible := False;
  Label3.Caption := 'Starting...';
  fModule.UpdateZoneOnStatus := @UpdateZoneOnStatus;
  fModule.UpdateZoneOnFinished := @UpdateZoneOnFinished;
  fModule.UpdateZone(Node.DistinguishedName);
end;

procedure TFrmModuleDNS.UpdateZoneOnStatus(const status: String);
begin
  Label3.Caption := status;
end;

procedure TFrmModuleDNS.UpdateZoneOnFinished;
begin
  Label3.Caption := 'Terminated';

  UpdateGrid();

  GridDNS.Visible := True;
  Panel5.Visible := False;
end;

procedure TFrmModuleDNS.UpdateNode(Node: TDNSTreeNode);
begin
  if not Assigned(Node) then
    Node := (TreeDNS.Selected as TDNSTreeNode);
  if not Assigned(Node) then
    Exit;
  fModule.StopPreviousUpdate;

  case Node.fType of
    dtntRoot: UpdateNodeRoot(Node);
    dtntCustom: UpdateNodeCustom(Node);
    dtntZoneFolder: UpdateNodeZoneFolder(Node);
    dtntZone: UpdateNodeZone(Node);
  end;
end;

procedure TFrmModuleDNS.UpdateGrid(Node: TDNSTreeNode);
begin
  if not Assigned(Node) then
    Node := (TreeDNS.Selected as TDNSTreeNode);
  if not Assigned(Node) then
    Exit;

  GridDNS.Clear;
  case Node.fType of
    dtntRoot: ;
    dtntCustom: GridDNS.LoadData(fModule.ZoneStoragesToDocVariantData((Node = fReverseLookupZonesNode)));
    dtntZone: GridDNS.LoadData(fModule.GetZoneDnsStorage(Node.DistinguishedName).ToDocVariantData);
  end;
end;


procedure TFrmModuleDNS.ReloadZones;
begin
  fModule.ReloadZones;
  TreeUpdateZones();
end;

procedure TFrmModuleDNS.TreeUpdateZones;
var
  ZoneNames: TRawUtf8DynArray;
  i: Integer;
  ZoneName, ZoneDC: RawUtf8;
  Node: TDNSTreeNode;
  ZoneStorage: TZoneDnsStorage;
begin
  ZoneNames := fModule.GetZoneNames;

  for i := Pred(fForwardLookupZonesNode.Count) downto 0 do
    if not ZoneNames.Contains(fForwardLookupZonesNode.Items[i].Text) then
      TreeDNS.Items.Delete(fForwardLookupZonesNode.Items[i]);

  for i := Pred(fReverseLookupZonesNode.Count) downto 0 do
    if not ZoneNames.Contains(fReverseLookupZonesNode.Items[i].Text) then
      TreeDNS.Items.Delete(fReverseLookupZonesNode.Items[i]);

  TreeDNS.BeginUpdate;
  try
    for ZoneName in ZoneNames do
    begin
      ZoneStorage := fModule.GetZoneDnsStorageByName(ZoneName);
      if not Assigned(ZoneStorage) then
        Continue;
      ZoneDC := ZoneStorage.DC;
      if Assigned(fReverseLookupZonesNode.FindNode(ZoneDC)) or Assigned(fForwardLookupZonesNode.FindNode(ZoneDC)) then
        Continue;
      if (ZoneDC = 'RootDNSServers') then
        continue;
      if String(ZoneDC).EndsWith('in-addr.arpa') then
        Node := (TreeDNS.Items.AddChild(fReverseLookupZonesNode, ZoneDC) as TDNSTreeNode)
      else
        Node := (TreeDNS.Items.AddChild(fForwardLookupZonesNode, ZoneDC) as TDNSTreeNode);
      Node.NodeType := dtntZone;
      Node.HasChildren := True;
      Node.fDistinguishedName := ZoneStorage.ZoneObjectName;
    end;
  finally
    TreeDNS.EndUpdate;
  end;
end;

procedure TFrmModuleDNS.LdapConnectEvent(Sender: TObject);
var
  LdapClient: TLdapClient;
begin
  LdapClient := (Sender as TLdapClient);
  if Assigned(fForwardLookupZonesNode) then
    FreeAndNil(fForwardLookupZonesNode);
  if Assigned(fReverseLookupZonesNode) then
    FreeAndNil(fReverseLookupZonesNode);
  if Assigned(fRootNode) then
    FreeAndNil(fRootNode);
  fRootNode := (TreeDNS.Items.Add(nil, LdapClient.Settings.TargetHost) as TDNSTreeNode);

  fForwardLookupZonesNode := (TreeDNS.Items.AddChild(fRootNode, 'Forward Lookup Zones') as TDNSTreeNode);
  fForwardLookupZonesNode.NodeType := dtntCustom;

  fReverseLookupZonesNode := (TreeDNS.Items.AddChild(fRootNode, 'Reverse Lookup Zones') as TDNSTreeNode);
  fReverseLookupZonesNode.NodeType := dtntCustom;

  fRootNode.Expand(False);
  fRootNode.Selected := True;
  Refresh;
end;

procedure TFrmModuleDNS.LdapCloseEvent(Sender: TObject);
begin
  FreeAndNil(fForwardLookupZonesNode);
  FreeAndNil(fReverseLookupZonesNode);
  FreeAndNil(fRootNode);
  GridDNS.Clear;
end;

procedure TFrmModuleDNS.TreeDNSChange(Sender: TObject; Node: TTreeNode);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Change Node', Self);

  GridDNS.Clear;
  if Timer_TreeChangeNode.Enabled then
    Timer_TreeChangeNode.Enabled := False;
  Timer_TreeChangeNode.Enabled := True;
end;

procedure TFrmModuleDNS.Action_RefreshExecute(Sender: TObject);
begin
  ReloadZones;
  UpdateNode();
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
    if Assigned(NodeData) and Assigned(TreeDNS.Selected) then
      NodeFound := (TreeDNS.Selected.FindNode(NodeData^.S['name']) as TDNSTreeNode);
    if Assigned(TreeDNS.Selected) and Assigned(NodeFound) and NodeFound.HasChildren then
      ImageIndex := Ord(ileADOU)
    else if Assigned(nodeData) then
    begin
      if NodeData^.Exists('_type') then
      begin
        case NodeData^.I['_type'] of
          0: ImageIndex := Ord(ileADUnknown);
          else
            ImageIndex := 57;
        end;
      end
      else
        ImageIndex := Ord(ileADUnknown);
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
    fLog.Log(sllTrace, 'Timer Tree Change', Self);

  Timer_TreeChangeNode.Enabled := False;

  GridDNS.Clear;

  if not Assigned(TreeDNS.Selected) then
  begin
    if Assigned(fLog) then
      fLog.Log(sllInfo, 'No node Assigned', Self);
    Exit;
  end;

  if not (TreeDNS.Selected as TDNSTreeNode).Retrieved then
    UpdateNode((TreeDNS.Selected as TDNSTreeNode))
  else
    UpdateGrid((TreeDNS.Selected as TDNSTreeNode));
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
  NodeData: PDocVariantData;
  DistinguishedName: RawUtf8;
begin
  DistinguishedName := '';
  if Assigned(GridDNS.FocusedNode) then
  begin
    NodeData := GridDNS.GetNodeAsPDocVariantData(GridDNS.FocusedNode);
    if Assigned(NodeData) then
      DistinguishedName := NodeData^.S['objectName'];
  end;
  if (DistinguishedName = '') and Assigned(TreeDNS.Selected) then
    DistinguishedName := (TreeDNS.Selected as TDNSTreeNode).DistinguishedName;

  if DistinguishedName <> '' then
    FrmRSAT.OpenProperty(DistinguishedName);
end;

procedure TFrmModuleDNS.Action_PropertyUpdate(Sender: TObject);
begin
  Action_Property.Enabled := Assigned(FrmRSAT.LdapClient) and FrmRSAT.LdapClient.Connected and (GridDNS.SelectedCount > 0)
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
  Filter, Message: String;

  procedure InnerDelete(DC: String);
  begin
    repeat
      if not FrmRSAT.LdapClient.Search(DC, False, Filter, ['dnsRecord']) then
      begin
        if Assigned(fLog) then
          fLog.Log(sllError, '% - Ldap Search Error: %', [Action_Delete.Caption, FrmRSAT.LdapClient.ResultString]);
        Exit;
      end;

      for SearchResult in FrmRSAT.LdapClient.SearchResult.Items do
      begin
        if not Assigned(SearchResult) or not data.Exists(SearchResult.ObjectName) then
          continue;
        for Item in data.A[SearchResult.ObjectName]^.Objects do
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
            Exit;
          end;
        end;
        AttributeToRemove.Clear;
      end;
    until FrmRSAT.LdapClient.SearchCookie = '';
  end;

begin

  if MessageDlg(rsTitleDeleteObject, rsDeleteObjectsConfirmation, mtConfirmation, mbYesNoCancel, 0) <> mrYes then
  begin
    if Assigned(fLog) then
      fLog.Log(sllInfo, 'Action cancelled by user.', Self);
    Exit;
  end;


  for RowData in GridDNS.SelectedRows.Objects do
  begin
    if not Assigned(RowData) then
      continue;

    if RowData^.Exists('rawdata') then
      fModule.RSAT.LdapClient.Modify(RowData^.S['objectName'], lmoDelete, 'dnsRecord', RowData^.S['rawdata'])
    else
      fModule.RSAT.LdapClient.Delete(RowData^.S['objectName'], True);
  end;
  Refresh;
end;

procedure TFrmModuleDNS.Action_DeleteUpdate(Sender: TObject);
begin
  Action_Delete.Enabled := (GridDNS.Focused) and (GridDNS.SelectedCount > 0);
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
  Action_NewZone.Enabled := Assigned(TreeDNS.Selected) and ((TreeDNS.Selected as TDNSTreeNode).fType = dtntCustom);
end;

procedure TFrmModuleDNS.Action_OtherNewRecordsExecute(Sender: TObject);
var
  DNSNodeZone: TDNSTreeNode;
  dcPrefix: String;
  DNSRecord: TDNSRecord;
  idx: Integer;
  Serial: Cardinal;
  Found: Boolean;
  ZoneStorage: TZoneDnsStorage;
  RawDNSRecord: RawByteString;
begin
  dcPrefix := '';
  DNSNodeZone := (TreeDNS.Selected as TDNSTreeNode);

  if not Assigned(DNSNodeZone) then
    Exit;

  Found := False;
  ZoneStorage := fModule.GetZoneDnsStorage(DNSNodeZone.DistinguishedName);
  idx := ZoneStorage.GetByName('@');
  for RawDNSRecord in ZoneStorage.GetDnsRecord(idx) do
  begin
    if not DNSRecordBytesToRecord(DNSRecord, PByteArray(RawDNSRecord)^) then
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
  Action_OtherNewRecords.Enabled := Assigned(TreeDNS.Selected) and ((TreeDNS.Selected as TDNSTreeNode).fType = dtntZone);
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

  fModule := TModuleADDNS.Create(FrmRSAT.RSAT);


  fRootNode := nil;
  fForwardLookupZonesNode := nil;
  fReverseLookupZonesNode := nil;

  Image1.Visible := not IsDarkMode;
  Image2.Visible := not Image1.Visible;

end;

destructor TFrmModuleDNS.Destroy;
begin
  FreeAndNil(fTreeSelectionHistory);
  FreeAndNil(fModule);

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

