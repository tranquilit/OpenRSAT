unit ufrmmodulesitesandservices;

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
  StdCtrls,
  ActnList,
  Dialogs,
  Menus,
  VirtualTrees,
  tis.ui.grid.core,
  mormot.core.base,
  mormot.core.log,
  mormot.core.variants,
  mormot.net.ldap,
  ufrmmoduleadssoptions,
  ucoredatamodule,
  uinterfacecore,
  uinterfacemodule;

type

  { TSSNode }

  TADSSTreeNode = class(TTreeNode)
  private
    fAttributes: TLdapAttributeList;

    function GetDistinguishedName: String;
    function GetName: String;
    function GetObjectType: String;
    procedure SetDistinguishedName(AValue: String);
    procedure SetObjectTypes(AValue: TRawUtf8DynArray);
  public
    destructor Destroy; override;
  published
    property DistinguishedName: String read GetDistinguishedName write SetDistinguishedName;
    property ObjectType: String read GetObjectType;
    property Name: String read GetName;
  end;

  { TFrmModuleSitesAndServices }

  TFrmModuleSitesAndServices = class(TFrameModule)
    Action_Delete: TAction;
    Action_Property: TAction;
    Action_NewSubnet: TAction;
    Action_NewSite: TAction;
    Action_Previous: TAction;
    Action_Next: TAction;
    Action_Parent: TAction;
    Action_Refresh: TAction;
    ActionList1: TActionList;
    Label1: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    Timer_TreeChangeNode: TTimer;
    Timer_SearchInGrid: TTimer;
    TisGrid1: TTisGrid;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    TreeView1: TTreeView;
    {$push}{$warn 5024 off}
    procedure Action_DeleteExecute(Sender: TObject);
    procedure Action_DeleteUpdate(Sender: TObject);
    procedure Action_NewSiteExecute(Sender: TObject);
    procedure Action_NewSiteUpdate(Sender: TObject);
    procedure Action_NewSubnetExecute(Sender: TObject);
    procedure Action_NewSubnetUpdate(Sender: TObject);
    procedure Action_PropertyExecute(Sender: TObject);
    procedure Action_PropertyUpdate(Sender: TObject);
    procedure Action_RefreshExecute(Sender: TObject);
    procedure Action_RefreshUpdate(Sender: TObject);
    procedure Timer_TreeChangeNodeTimer(Sender: TObject);
    procedure Timer_SearchInGridTimer(Sender: TObject);
    function TisGrid1CompareByRow(aSender: TTisGrid;
      const aPropertyName: RawUtf8; const aRow1, aRow2: PDocVariantData;
      var aHandled: Boolean): PtrInt;
    procedure TisGrid1DblClick(Sender: TObject);
    procedure TisGrid1GetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure TisGrid1KeyPress(Sender: TObject; var Key: char);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure TreeView1CreateNodeClass(Sender: TCustomTreeView;
      var NodeClass: TTreeNodeClass);
    procedure TreeView1Expanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure TreeView1GetImageIndex(Sender: TObject; Node: TTreeNode);
    {$pop}
  private
    fCore: ICore;
    fLog: TSynLog;
    fEnabled: Boolean;
    fUpdating: Integer;

    fSearchWord: RawUtf8;

    fADSSSiteNode: TADSSTreeNode;
    fADSSServiceNode: TADSSTreeNode;

    fOptions: TModuleADSSOptions;

    procedure RefreshLdapNode(Node: TADSSTreeNode = nil);

    procedure UpdateGrid(Node: TADSSTreeNode);
    procedure UpdateGridColumns(Names: TStringArray);

    procedure BackupNodes(BackupData: PDocVariantData);
    procedure RestoreNodes(BackupData: PDocVariantData; RootNode: TADSSTreeNode);
    procedure BeginUpdate;
    procedure EndUpdate;

    procedure OnLdapClientConnect(LdapClient: TLdapClient);
    procedure OnLdapClientClose(LdapClient: TLdapClient);

    procedure OnADSSOptionsChanged(Options: TOptions);

    function CustomSortSitesNode(Node1, Node2: TTreeNode): integer;
    function CustomSortSubnetsNode(Node1, Node2: TTreeNode): integer;
  public
    constructor Create(TheOwner: TComponent; ACore: ICore); reintroduce;
    destructor Destroy; override;

  published
    ////////////////
    /// TFrameModule
    function GetModuleEnabled: Boolean; override;
    procedure SetModuleEnabled(AValue: Boolean); override;
    function GetModuleName: String; override;
    function GetModuleDisplayName: String; override;
    function GetOptions: TOptions; override;
    procedure Refresh; override;
    procedure Load; override;
    ///
    ////////////////
  end;

implementation
uses
  math,
  mormot.core.text,
  ucommon,
  ursatldapclient,
  uvisnewobject;

{$R *.lfm}

type
  TIPAddress = record
    IsIPV6: Boolean;
    Parts: array[0..7] of Integer;
  end;

function CompareIP(const A, B: TIPAddress): Integer;
var
  i: Integer;
begin
  for i := 0 to High(A.Parts) do
  begin
    if A.Parts[i] < B.Parts[i] then
    begin
      Result := -1;
      Exit;
    end
    else if A.Parts[i] > B.Parts[i] then
    begin
      Result := 1;
      Exit;
    end;
  end;
  Result := 0;
end;

function ParseIP(const IP: string): TIPAddress;
var
  parts: TStringArray;
  i: Integer;
begin
  if Pos(':', IP) > 0 then // IPv6
  begin
    Result.IsIPv6 := True;
    parts := IP.Split(':');
    for i := 0 to 7 do
      Result.Parts[i] := StrToIntDef(parts[Min(i, High(parts))], 0);
  end
  else // IPv4
  begin
    Result.IsIPv6 := False;
    parts := IP.Split('.');
    for i := 0 to 3 do
      Result.Parts[i] := StrToIntDef(parts[i], 0);
    for i := 4 to 7 do
      Result.Parts[i] := 0;
  end;
end;

function IsIP(const IP: String): Boolean;
begin
  result := (Pos(':', IP) > 0) or ((Pos('.', IP) > 0) and (Length(IP.Split('.')) = 4));
end;

{ TSSNode }

function TADSSTreeNode.GetDistinguishedName: String;
begin
  result := fAttributes.Find('distinguishedName').GetReadable();
end;

function TADSSTreeNode.GetName: String;
begin
  result := fAttributes.Find('name').GetReadable();
end;

function TADSSTreeNode.GetObjectType: String;
var
  Attribute: TLdapAttribute;
begin
  result := '';
  Attribute := fAttributes.Find('objectClass');
  if not Assigned(Attribute) then
    Exit;
  result := Attribute.GetReadable(Attribute.Count - 1);
end;

procedure TADSSTreeNode.SetDistinguishedName(AValue: String);
var
  Attribute: TLdapAttribute;
begin
  if not Assigned(fAttributes) then
    fAttributes := TLdapAttributeList.Create;
  Attribute := fAttributes.Find('distinguishedName');
  if not Assigned(Attribute) then
    Attribute := fAttributes.Add('distinguishedName');
  Attribute.Clear;
    Attribute.Add(AValue);
end;

procedure TADSSTreeNode.SetObjectTypes(AValue: TRawUtf8DynArray);
var
  Attribute: TLdapAttribute;
  AV: RawUtf8;
begin
  if not Assigned(fAttributes) then
    fAttributes := TLdapAttributeList.Create;
  Attribute := fAttributes.Find('objectClass');
  if not Assigned(Attribute) then
    Attribute := fAttributes.Add('objectClass');
  Attribute.Clear;
  for AV in AValue do
    Attribute.Add(AV);
end;

destructor TADSSTreeNode.Destroy;
begin
  inherited Destroy;
  FreeAndNil(fAttributes);
end;

{ TFrmModuleSitesAndServices }

procedure TFrmModuleSitesAndServices.Action_RefreshExecute(Sender: TObject);
begin
  RefreshLdapNode();
end;

procedure TFrmModuleSitesAndServices.Action_RefreshUpdate(Sender: TObject);
begin
  Action_Refresh.Enabled := Assigned(fCore) and Assigned(fCore.LdapClient) and fCore.LdapClient.Connected;
end;

procedure TFrmModuleSitesAndServices.Timer_TreeChangeNodeTimer(Sender: TObject);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Timer Tree Change', Self);

  Timer_TreeChangeNode.Enabled := False;

  RefreshLdapNode((TreeView1.Selected as TADSSTreeNode));
  UpdateGrid((TreeView1.Selected as TADSSTreeNode));
end;

procedure TFrmModuleSitesAndServices.Timer_SearchInGridTimer(Sender: TObject);
begin
  Timer_SearchInGrid.Enabled := False;
end;

function TFrmModuleSitesAndServices.TisGrid1CompareByRow(aSender: TTisGrid;
  const aPropertyName: RawUtf8; const aRow1, aRow2: PDocVariantData;
  var aHandled: Boolean): PtrInt;
begin
  if (aPropertyName = 'name') then
  begin
    if not Assigned(aRow1) or not Assigned(aRow2) or
       not aRow1^.Exists('name') or not aRow2^.Exists('name') or
       not IsIP(aRow1^.S['name']) or not IsIP(aRow2^.S['name']) then
      Exit;

    aHandled := True;
    result := CompareIP(ParseIP(aRow1^.S['name'].Split('/')[0]), ParseIP(aRow2^.S['name'].Split('/')[0]))
  end;
end;

procedure TFrmModuleSitesAndServices.Action_NewSiteExecute(Sender: TObject);
var
  vis: TVisNewObject;
begin
  vis := TVisNewObject.Create(Self, vnotSite, Format('CN=Sites,%s', [fCore.LdapClient.ConfigDN]), fCore.LdapClient.ConfigDN);
  try
    vis.Ldap := fCore.LdapClient;
    vis.ShowModal;
    RefreshLdapNode();
  finally
    FreeAndNil(vis);
  end;
end;

procedure TFrmModuleSitesAndServices.Action_DeleteExecute(Sender: TObject);
var
  RowData: PDocVariantData;
  mResult, mResultNonLeaf: TModalResult;

  procedure InnerDelete(distinguishedName: String);
  begin
    if (mResult <> mrYesToAll) then
      mResult := MessageDlg('Delete object', FormatUtf8('Do you want to delete "%"?', [distinguishedName]), mtConfirmation, [mbYesToAll, mbYes, mbNo, mbNoToAll, mbCancel], 0);

    if (mResult <> mrYes) and (mResult <> mrYesToAll) then
      Exit;

    if not fCore.LdapClient.Delete(distinguishedName) then
    begin
      case fCore.LdapClient.ResultError of
        leNotAllowedOnNonLeaf:
        begin
          if (mResultNonLeaf = mrNoToAll) or (mResultNonLeaf = mrCancel) then
            Exit;
          if (mResultNonLeaf <> mrYesToAll) then
            mResultNonLeaf := MessageDlg('Non leaf object', 'Object has children. Do you want to delete them ?', mtConfirmation, [mbYesToAll, mbYes, mbNo, mbNoToAll, mbCancel], 0);

          if (mResultNonLeaf <> mrYes) and (mResultNonLeaf <> mrYesToAll) then
            Exit;
          if not fCore.LdapClient.Delete((TreeView1.Selected as TADSSTreeNode).DistinguishedName, True) then
          begin
            ShowLdapDeleteError(fCore.LdapClient);
            Exit;
          end;
        end;
      end;
    end;
  end;

begin
  mResult := mrNone;
  mResultNonLeaf := mrNone;

  if TisGrid1.Focused then
  begin
    for RowData in TisGrid1.SelectedRows.Objects do
    begin
      if not Assigned(RowData) or not RowData^.Exists('distinguishedName') then
        continue;
      InnerDelete(RowData^.S['distinguishedName']);
      if (mResult = mrNoToAll) or (mResult = mrCancel) then
        Exit;
    end;
  end
  else if TreeView1.Focused then
  begin
    InnerDelete((TreeView1.Selected as TADSSTreeNode).DistinguishedName);
  end;
  RefreshLdapNode();
end;

procedure TFrmModuleSitesAndServices.Action_DeleteUpdate(Sender: TObject);
var
  Allowed: Boolean;
  Node: PVirtualNode;
  NodeData: PDocVariantData;

  function TypeIsAllowed(ObjectType: String): Boolean;
  begin
    case ObjectType of
      'subnetContainer',
      'interSiteTransportContainer': Result := False;
      else
        Result := True;
    end;
  end;

begin
  Allowed := True;
  //if TisGrid1.Focused then
  //begin
  //  Allowed := True;
  //  Node := TisGrid1.GetFirstSelected();
  //  while Assigned(Node) do
  //  begin
  //    NodeData := TisGrid1.GetNodeAsPDocVariantData(Node);
  //    Node := TisGrid1.GetNextSelected(Node);
  //    if not Assigned(NodeData) then
  //      continue;
  //    Allowed := Allowed and TypeIsAllowed(NodeData^.S['type']);
  //  end;
  //end
  //else if TreeView1.Focused then
  //begin
  //  Allowed := TypeIsAllowed((TreeView1.Selected as TSSNode).ObjectType);
  //end;
  Action_Delete.Enabled := Allowed;
end;

procedure TFrmModuleSitesAndServices.Action_NewSiteUpdate(Sender: TObject);
begin
  Action_NewSite.Enabled := Assigned(fCore.LdapClient) and fCore.LdapClient.Connected;
end;

procedure TFrmModuleSitesAndServices.Action_NewSubnetExecute(Sender: TObject);
var
  vis: TVisNewObject;
begin
  vis := TVisNewObject.Create(Self, vnotSubnet, Format('CN=Subnets,CN=Sites,%s', [fCore.LdapClient.ConfigDN]), fCore.LdapClient.ConfigDN);
  try
    vis.Ldap := fCore.LdapClient;
    vis.ShowModal;
    RefreshLdapNode();
  finally
    FreeAndNil(vis);
  end;
end;

procedure TFrmModuleSitesAndServices.Action_NewSubnetUpdate(Sender: TObject);
begin
  Action_NewSubnet.Enabled := Assigned(fCore.LdapClient) and fCore.LdapClient.Connected;
end;

procedure TFrmModuleSitesAndServices.Action_PropertyExecute(Sender: TObject);
var
  PropertyName, DistinguishedName: String;
begin
  if TreeView1.Focused and Assigned(TreeView1.Selected) then
  begin
    PropertyName := (TreeView1.Selected as TADSSTreeNode).Name;
    DistinguishedName := (TreeView1.Selected as TADSSTreeNode).DistinguishedName;
  end
  else if Assigned(TisGrid1.FocusedRow) then
  begin
    PropertyName := TisGrid1.FocusedRow^.S['name'];
    DistinguishedName := TisGrid1.FocusedRow^.S['distinguishedName'];
  end
  else if (TisGrid1.SelectedCount = 1) then
  begin
    PropertyName := TisGrid1.SelectedRows._[0]^.S['name'];
    DistinguishedName := TisGrid1.SelectedRows._[0]^.S['distinguishedName'];
  end;
  fCore.OpenProperty(PropertyName, DistinguishedName);
end;

procedure TFrmModuleSitesAndServices.Action_PropertyUpdate(Sender: TObject);
begin
  Action_Property.Enabled := Assigned(fCore) and Assigned(fCore.LdapClient) and fCore.LdapClient.Connected;
end;

procedure TFrmModuleSitesAndServices.TisGrid1DblClick(Sender: TObject);
var
  n: String;
  Node: TTreeNode;
begin
  if not Assigned(TisGrid1.FocusedRow) then
    Exit;
  n := TisGrid1.FocusedRow^.S['name'];
  Node := TreeView1.Selected.FindNode(n);
  if Assigned(Node) then
    Node.Selected := True
  else
    fCore.OpenProperty(TisGrid1.FocusedRow^.S['name'], TisGrid1.FocusedRow^.S['distinguishedName']);
end;

procedure TFrmModuleSitesAndServices.TisGrid1GetImageIndex(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
var
  NodeData: PDocVariantData;
begin
  NodeData := TisGrid1.GetNodeAsPDocVariantData(Node);
  if not Assigned(NodeData) then
    Exit;

  case TisGrid1.FindColumnByIndex(Column).PropertyName of
    'name':
    begin
      if ImageIndex < 0 then
        ImageIndex := CoreDataModule.objectClassToImageIndex(NodeData^.S['type']);
    end;
  end;
end;

procedure TFrmModuleSitesAndServices.TisGrid1KeyPress(Sender: TObject;
  var Key: char);
begin
  SearchInGrid(Timer_SearchInGrid, TisGrid1, fSearchWord, Key);
end;

procedure TFrmModuleSitesAndServices.TreeView1Change(Sender: TObject;
  Node: TTreeNode);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Tree Change', Self);

  if Timer_TreeChangeNode.Enabled then
    Timer_TreeChangeNode.Enabled := False;
  Timer_TreeChangeNode.Enabled := True;
end;

procedure TFrmModuleSitesAndServices.TreeView1CreateNodeClass(
  Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
begin
  NodeClass := TADSSTreeNode;
end;

procedure TFrmModuleSitesAndServices.TreeView1Expanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
begin
  RefreshLdapNode((Node as TADSSTreeNode));
end;

procedure TFrmModuleSitesAndServices.TreeView1GetImageIndex(Sender: TObject;
  Node: TTreeNode);
begin
  if Node.ImageIndex >= 0 then
    Exit;
  Node.ImageIndex := CoreDataModule.objectClassToImageIndex((Node as TADSSTreeNode).ObjectType);
  Node.SelectedIndex := Node.ImageIndex;
end;

procedure TFrmModuleSitesAndServices.RefreshLdapNode(Node: TADSSTreeNode);
var
  SearchResult: TLdapResult;
  ChildNode: TADSSTreeNode;
  cn: RawUtf8;
  ChildCache: TDocVariantData;
  i: Integer;
  c: TCursor;
  Filter: String;
begin
  if not Assigned(Node) then
    Node := (TreeView1.Selected as TADSSTreeNode);

  if not Assigned(Node) or (fUpdating > 0) then
    Exit;
  ChildCache.Init;
  fCore.LdapClient.SearchBegin();
  try
    c := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    fCore.LdapClient.SearchScope := lssSingleLevel;
    Filter := '(&(!(objectClass=nTDSSiteSettings))(!(objectClass=nTDSConnection)))';
    repeat
      if not fCore.LdapClient.Search(Node.DistinguishedName, False, Filter, ['*']) then
      begin
        ShowLdapSearchError(fCore.LdapClient);
        Exit;
      end;

      for SearchResult in fCore.LdapClient.SearchResult.Items do
      begin
        if not Assigned(SearchResult) then
          Continue;
        cn := SearchResult.Find('cn').GetReadable();
        ChildCache.B[cn] := True;
        ChildNode := (Node.FindNode(cn) as TADSSTreeNode);
        if not Assigned(ChildNode) then
        begin
          ChildNode := (TreeView1.Items.AddChild(Node, SearchResult.Find('cn').GetReadable()) as TADSSTreeNode);
          ChildNode.HasChildren := True;
        end;
        if Assigned(ChildNode.fAttributes) then
          FreeAndNil(ChildNode.fAttributes);
        ChildNode.fAttributes := TLdapAttributeList(SearchResult.Attributes.Clone);
      end;
    until fCore.LdapClient.SearchCookie = '';
    for i := 0 to Node.Count - 1 do
    begin
      if not ChildCache.Exists(Node.Items[Node.Count - 1 - i].Text) then
        TreeView1.Items.Delete(node.Items[Node.Count - 1 - i]);
    end;
    Node.HasChildren := Node.Count > 0;
  finally
    fCore.LdapClient.SearchEnd;
    Screen.Cursor := c;
  end;
  if node = fADSSSiteNode then
    Node.CustomSort(@CustomSortSitesNode);
  if node.Text = 'Subnets' then
  begin
    Node.CustomSort(@CustomSortSubnetsNode);
  end;
end;

procedure TFrmModuleSitesAndServices.UpdateGrid(Node: TADSSTreeNode);
var
  SearchResult: TLdapResult;
  RowData: TDocVariantData;
  Attribute: TLdapAttribute;
  c: TCursor;
  Filter: String;
  options: RawUtf8;
  optionsValue: Longint;

  procedure FillFromServer(RowData: PDocVariantData; Attribute: TLdapAttribute);
  var
    fromServer, fromServerCN: RawUtf8;
    fromServerCNSplitted: TStringArray;
  begin
    if not Assigned(Attribute) then
      Exit;

    fromServer := Attribute.GetReadable();
    if (fromServer = '') then
      Exit;
    fromServerCN := DNToCN(fromServer);
    if (fromServerCN = '') then
      Exit;
    fromServerCNSplitted := String(fromServerCN).Split('/');
    if (Length(fromServerCNSplitted) < 6) then
      Exit;
    RowData^.AddOrUpdateValue('fromServer', fromServerCNSplitted[5]);
    RowData^.AddOrUpdateValue('fromSite', fromServerCNSplitted[3]);
  end;

  procedure FillSiteObject(RowData: PDocVariantData; Attribute: TLdapAttribute);
  var
    site, siteCN: RawUtf8;
    siteCNSplitted: TStringArray;
  begin
    if not Assigned(Attribute) then
      Exit;

    site := Attribute.GetReadable();
    if (site = '') then
      Exit;
    siteCN := DNToCN(site);
    if (siteCN = '') then
      Exit;
    siteCNSplitted := String(siteCN).Split('/');
    RowData^.AddOrUpdateValue('site', siteCNSplitted[high(siteCNSplitted)]);
  end;

begin
  TisGrid1.Clear;
  RowData.Init();

  if not Assigned(Node) then
    Exit;
  case Node.ObjectType of
    'siteContainer': UpdateGridColumns(['name', 'type', 'description', 'location']);
    'subnetContainer': UpdateGridColumns(['name', 'site', 'location', 'type', 'description']);
    'interSiteTransport': UpdateGridColumns(['name', 'type', 'description', 'cost', 'replicationInterval']);
    'serverContainer': UpdateGridColumns(['name', 'domain', 'bridgehead', 'dcType', 'description']);
    'nTDSDSA': UpdateGridColumns(['name', 'fromServer', 'fromSite', 'type', 'description']);
    else
      UpdateGridColumns(['name', 'type', 'description']);
  end;
  TisGrid1.BeginUpdate;
  fCore.LdapClient.SearchBegin();
  try
    c := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    fCore.LdapClient.SearchScope := lssSingleLevel;
    Filter := '';
    repeat
      if not fCore.LdapClient.Search(Node.DistinguishedName, False, Filter, ['*']) then
      begin
        ShowLdapSearchError(fCore.LdapClient);
        Exit;
      end;

      for SearchResult in fCore.LdapClient.SearchResult.Items do
      begin
        if not Assigned(SearchResult) then
          continue;
        RowData.AddOrUpdateValue('distinguishedName', SearchResult.Find('distinguishedName').GetReadable());
        Attribute := SearchResult.Find('objectClass');
        if Assigned(Attribute) then
          RowData.AddOrUpdateValue('type', Attribute.GetReadable(Attribute.Count - 1));
        options := SearchResult.Find('options').GetReadable();
        if (RowData.S['type'] = 'nTDSConnection') and (options <> '') and TryStrToInt(options, optionsValue) and ((optionsValue and 1) = 1) then
          RowData.AddOrUpdateValue('name', '<automatically generated>')
        else
          RowData.AddOrUpdateValue('name', SearchResult.Find('cn').GetReadable());
        RowData.AddOrUpdateValue('description', SearchResult.Find('description').GetReadable());
        RowData.AddOrUpdateValue('cost', SearchResult.Find('cost').GetReadable());
        RowData.AddOrUpdateValue('replicationInterval', SearchResult.Find('replInterval').GetReadable());

        FillSiteObject(@RowData, SearchResult.Find('siteObject'));
        FillFromServer(@RowData, SearchResult.Find('fromServer'));

        TisGrid1.Data.AddItem(RowData);
        RowData.Clear;
      end;
    until fCore.LdapClient.SearchCookie = '';
  finally
    fcore.LdapClient.SearchEnd;
    TisGrid1.EndUpdate;
    TisGrid1.LoadData();
    Screen.Cursor := c;
  end;
end;

procedure TFrmModuleSitesAndServices.UpdateGridColumns(Names: TStringArray);
var
  i: Integer;
  Column: TTisGridColumn;

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
  Column := TisGrid1.FindColumnByIndex(i);
  while Assigned(Column) do
  begin
    Inc(i);
    if ArrayContains(Names, Column.PropertyName) then
      Column.Options := Column.Options + [coVisible]
    else
      Column.Options := Column.Options - [coVisible];
    Column := TisGrid1.FindColumnByIndex(i);
  end;
  for i := 0 to High(Names) do
  begin
    Column := TisGrid1.FindColumnByPropertyName(Names[i]);
    Column.Position := i;
  end;
end;

procedure TFrmModuleSitesAndServices.BackupNodes(BackupData: PDocVariantData);
var
  Item: TTreeNode;
  BackupRow: PDocVariantData;
begin
  if not Assigned(BackupData) then
    Exit;

  for Item in TreeView1.Items do
  begin
    if not Assigned(Item.Parent) then
      continue;
    BackupRow := BackupData^.O_[(Item.Parent as TADSSTreeNode).DistinguishedName]^.O_[(Item as TADSSTreeNode).DistinguishedName];
    BackupRow^.AddOrUpdateValue('name', Item.Text);
    BackupRow^.AddOrUpdateValue('selected', Item.Selected);
    BackupRow^.AddOrUpdateValue('focused', Item.Focused);
    BackupRow^.AddOrUpdateValue('hasChildren', Item.HasChildren);
    BackupRow^.AddOrUpdateValue('expanded', Item.Expanded);
    BackupRow^.AddOrUpdateValue('imageIndex', Item.ImageIndex);
    BackupRow^.AddOrUpdateValue('selectedIndex', Item.SelectedIndex);
    BackupRow^.AddOrUpdateValue('distinguishedName', (Item as TADSSTreeNode).DistinguishedName);
    BackupRow^.AddOrUpdateValue('objectClass', (Item as TADSSTreeNode).fAttributes.Find('objectClass').GetVariant());
  end;
end;

procedure TFrmModuleSitesAndServices.RestoreNodes(BackupData: PDocVariantData;
  RootNode: TADSSTreeNode);
  procedure RestoreNode(Node: TADSSTreeNode);
  var
    NewNode: TADSSTreeNode;
    BackupRow: PDocVariantData;
  begin
    if not BackupData^.Exists(Node.DistinguishedName) then
      Exit;

    for BackupRow in BackupData^.O[Node.DistinguishedName]^.Objects do
    begin
      NewNode := (TreeView1.Items.AddChild(Node, '') as TADSSTreeNode);
      NewNode.DistinguishedName := BackupRow^.S['distinguishedName'];
      NewNode.SetObjectTypes(BackupRow^.A['objectClass']^.ToRawUtf8DynArray);
      NewNode.Text := BackupRow^.S['name'];
      RestoreNode(NewNode);
      NewNode.Selected := BackupRow^.B['selected'];
      NewNode.HasChildren := BackupRow^.B['hasChildren'];
      NewNode.Focused := BackupRow^.B['focused'];
      if BackupRow^.B['expanded'] then
        NewNode.Expand(False);
      NewNode.ImageIndex := BackupRow^.I['imageIndex'];
      NewNode.SelectedIndex := BackupRow^.I['selectedIndex'];
    end;
  end;

begin
  RestoreNode(RootNode);
end;

procedure TFrmModuleSitesAndServices.BeginUpdate;
begin
  TreeView1.BeginUpdate;
  Inc(fUpdating);
end;

procedure TFrmModuleSitesAndServices.EndUpdate;
begin
  TreeView1.EndUpdate;
  Dec(fUpdating);
end;

procedure TFrmModuleSitesAndServices.OnLdapClientConnect(LdapClient: TLdapClient
  );
var
  SearchResult: TLdapResult;
  Filter, cn: RawUtf8;
  BackupCursor: TCursor;
begin
  fADSSSiteNode := (TreeView1.Items.Add(nil, 'Sites') as TADSSTreeNode);
  fADSSSiteNode.HasChildren := True;
  fADSSServiceNode := (TreeView1.Items.Add(nil, 'Services') as TADSSTreeNode);
  fADSSServiceNode.HasChildren := True;
  fADSSServiceNode.Visible := fOptions.ShowService;

  LdapClient.SearchBegin();
  BeginUpdate;
  try
    BackupCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;

    LdapClient.SearchScope := lssSingleLevel;

    Filter := '(|(cn=Sites)(cn=Services))';
    repeat
      if not LdapClient.Search(LdapClient.ConfigDN, False, Filter, ['*']) then
      begin
        ShowLdapSearchError(LdapClient);
        Exit;
      end;
      for SearchResult in LdapClient.SearchResult.Items do
      begin
        if not Assigned(SearchResult) then
          continue;

        cn := SearchResult.Find('cn').GetReadable();

        if (cn = fADSSServiceNode.Text) then
          fADSSServiceNode.fAttributes := TLdapAttributeList(SearchResult.Attributes.Clone)
        else if (cn = fADSSSiteNode.Text) then
          fADSSSiteNode.fAttributes := TLdapAttributeList(SearchResult.Attributes.Clone);

      end;
    until LdapClient.SearchCookie = '';
  finally
    EndUpdate;
    LdapClient.SearchEnd;
    Screen.Cursor := BackupCursor;
  end;
  fADSSSiteNode.Expand(False);
end;

procedure TFrmModuleSitesAndServices.OnLdapClientClose(LdapClient: TLdapClient);
begin
  TreeView1.Items.Clear;
end;

procedure TFrmModuleSitesAndServices.OnADSSOptionsChanged(Options: TOptions);
var
  ADSSOptions: TModuleADSSOptions absolute Options;
begin
  if Assigned(fADSSSiteNode) then
  begin
    fADSSSiteNode.DeleteChildren;
    fADSSSiteNode.Expand(False);
  end;

  if Assigned(fADSSServiceNode) then
  begin
    fADSSServiceNode.DeleteChildren;
    fADSSServiceNode.Visible := ADSSOptions.ShowService;
  end;
end;

function TFrmModuleSitesAndServices.CustomSortSitesNode(Node1, Node2: TTreeNode
  ): integer;
begin
  if Node1.Text = 'Subnets' then
  begin
    result := -10;
    Exit;
  end;
  if Node2.Text = 'Subnets' then
  begin
    result := 10;
    Exit;
  end;
  if Node1.Text = 'Inter-Site Transports' then
  begin
    result := -10;
    Exit;
  end;
  if Node2.Text = 'Inter-Site Transports' then
  begin
    result := 10;
    Exit;
  end;
  result := Node1.DefaultTreeViewSort(Node1, Node2);
end;

function TFrmModuleSitesAndServices.CustomSortSubnetsNode(Node1,
  Node2: TTreeNode): integer;
var
  IP1, IP2: String;
begin
  IP1 := Node1.Text.Split('/')[0];
  IP2 := Node2.Text.Split('/')[0];

  result := CompareIP(ParseIP(IP1), ParseIP(IP2));
end;

constructor TFrmModuleSitesAndServices.Create(TheOwner: TComponent; ACore: ICore
  );
begin
  inherited Create(TheOwner);

  fEnabled := True;
  fCore := ACore;
  fUpdating := 0;
  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);

  fOptions := TModuleADSSOptions.Create;

  fCore.LdapClient.RegisterObserverConnect(@OnLdapClientConnect);
  fCore.LdapClient.RegisterObserverClose(@OnLdapClientClose);

  fOptions.RegisterObserver(@OnADSSOptionsChanged);
end;

destructor TFrmModuleSitesAndServices.Destroy;
begin
  FreeAndNil(fOptions);

  inherited Destroy;
end;

function TFrmModuleSitesAndServices.GetModuleEnabled: Boolean;
begin
  result := fEnabled;
end;

procedure TFrmModuleSitesAndServices.SetModuleEnabled(AValue: Boolean);
begin
  fEnabled := AValue;
end;

function TFrmModuleSitesAndServices.GetModuleName: String;
begin
  result := rsModuleADSSName;
end;

function TFrmModuleSitesAndServices.GetModuleDisplayName: String;
begin
  result := rsModuleADSSDisplayName;
end;

function TFrmModuleSitesAndServices.GetOptions: TOptions;
begin
  result := fOptions;
end;

procedure TFrmModuleSitesAndServices.Refresh;
begin
  Action_Refresh.Execute;
end;

procedure TFrmModuleSitesAndServices.Load;
begin

end;

end.

