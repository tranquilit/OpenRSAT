unit ufrmmoduleserviceinterfaces;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Dialogs,
  Forms,
  Controls,
  ComCtrls,
  StdCtrls,
  ExtCtrls,
  ActnList,
  Menus,
  tis.ui.grid.core,
  tis.ui.searchedit,
  mormot.core.log,
  mormot.core.base,
  mormot.core.text,
  mormot.core.variants,
  mormot.net.ldap,
  ucoredatamodule,
  VirtualTrees,
  ufrmmodule,
  ufrmoption,
  umodule,
  uopenrsatuicontextinterface,
  ursatldapclient,
  umoduleadsi,
  ursat,
  ulog;

type

  { TADSITreeNode }

  TADSITreeNode = class(TTreeNode)
  private
    fData: TLdapEntryData;
    fUpdated: Boolean;

    function GetDescription: RawUtf8;
    function GetDisplayName: RawUtf8;
    function GetDistinguishedName: RawUtf8;
    function GetObjectClass: TRawUtf8DynArray;
    function GetObjectType: RawUtf8;
  public
    destructor Destroy; override;

    function FindDN(const DistinguishedName: RawUtf8): TTreeNode;
    procedure SetEntry(const Entry: TLdapEntryData);
    procedure StartUpdateChildren;
    procedure EndUpdateChildren;
  published
    property DistinguishedName: RawUtf8 read GetDistinguishedName;
    property ObjectType: RawUtf8 read GetObjectType;
    property ObjectClass: TRawUtf8DynArray read GetObjectClass;
    property DisplayName: RawUtf8 read GetDisplayName;
    property Description: RawUtf8 read GetDescription;
    property Updated: Boolean read fUpdated write fUpdated;
  end;

  { TFrmModuleADSI }

  TFrmModuleADSI = class(TFrameModule)
    Action_Property: TAction;
    Action_NewObject: TAction;
    Action_DeleteObject: TAction;
    Action_Previous: TAction;
    Action_Next: TAction;
    Action_Parent: TAction;
    Action_Refresh: TAction;
    ActionList1: TActionList;
    Image1: TImage;
    Image2: TImage;
    Label1: TLabel;
    MenuItem1: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    PopupMenu1: TPopupMenu;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    StatusBar1: TStatusBar;
    Timer_TreeChangeNode: TTimer;
    Timer_SearchInGrid: TTimer;
    TisGrid1: TTisGrid;
    TisGrid2: TTisGrid;
    TisSearchEdit_ADSI: TTisSearchEdit;
    TisSearchEdit_TreeADSI: TTisSearchEdit;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolSeparator1: TToolButton;
    ToolButton_Property: TToolButton;
    ToolButton_NewObject: TToolButton;
    ToolButton_DeleteObject: TToolButton;
    ToolButton_Previous: TToolButton;
    ToolButton_Next: TToolButton;
    ToolButton_Parent: TToolButton;
    TreeView1: TTreeView;
    procedure Action_DeleteObjectExecute(Sender: TObject);
    procedure Action_DeleteObjectUpdate(Sender: TObject);
    procedure Action_NewObjectExecute(Sender: TObject);
    procedure Action_NewObjectUpdate(Sender: TObject);
    procedure Action_PropertyExecute(Sender: TObject);
    procedure Action_PropertyUpdate(Sender: TObject);
    procedure Action_RefreshExecute(Sender: TObject);
    procedure Timer_TreeChangeNodeTimer(Sender: TObject);
    procedure Timer_SearchInGridTimer(Sender: TObject);
    procedure TisGrid1Change(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TisGrid1Click(Sender: TObject);
    procedure TisGrid1DblClick(Sender: TObject);
    procedure TisGrid1GetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure TisGrid1KeyPress(Sender: TObject; var Key: char);
    procedure TisGrid1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TisGrid2KeyPress(Sender: TObject; var Key: char);
    procedure TisSearchEdit_ADSISearch(Sender: TObject; const aText: string);
    procedure TisSearchEdit_TreeADSISearch(Sender: TObject; const aText: string);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure TreeView1Click(Sender: TObject);
    procedure TreeView1CreateNodeClass(Sender: TCustomTreeView;
      var NodeClass: TTreeNodeClass);
    procedure TreeView1Expanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure TreeView1GetImageIndex(Sender: TObject; Node: TTreeNode);
  private
    fLog: TSynLogClass;
    fIContext: IOpenRSATUIContext;
    fModule: TModuleADSI;

    fSearchWord: RawUtf8;

    function GetLdap: ILdapConnection;
    procedure RefreshNode(Node: TADSITreeNode);
    procedure UpdateGrid(Node: TADSITreeNode);
    procedure UpdateGridAttribute(Node: TADSITreeNode); overload;
    procedure UpdateGridAttribute(DistinguishedName: String); overload;

    procedure LdapConnectEvent(Sender: TObject);
    procedure LdapCloseEvent(Sender: TObject);
  public
    constructor Create(Context: IOpenRSATUIContext);
    destructor Destroy; override;

    property Ldap: ILdapConnection read GetLdap;
  protected
    function GetModule: TModule; override;
    function GetFrmOptionClass: TFrameOptionClass; override;
    function GetOnLdapConnect: TNotifyEvent; override;
    function GetOnLdapClose: TNotifyEvent; override;
  published
    procedure Refresh; override;
    procedure Load; override;
  end;

implementation
uses
  ucommon,
  ucommonui,
  utheme,
  ursatldapclientui,
  uvisnewobject;

{$R *.lfm}

{ TADSITreeNode }

function TADSITreeNode.GetDistinguishedName: RawUtf8;
begin
  result := GetLdapEntryReadable(fData, 'distinguishedName', 0);
end;

function TADSITreeNode.GetObjectClass: TRawUtf8DynArray;
begin
  result := GetLdapEntryAllReadable(fData, 'objectClass');
end;

function TADSITreeNode.GetDisplayName: RawUtf8;
begin
  result := GetLdapEntryReadable(fData, 'name', 0);
end;

function TADSITreeNode.GetDescription: RawUtf8;
begin
  result := GetLdapEntryReadable(fData, 'description', 0);
end;

function TADSITreeNode.GetObjectType: RawUtf8;
var
  Values: TRawUtf8DynArray;
begin
  result := '';
  Values := GetLdapEntryAllReadable(fData, 'objectClass');
  if Assigned(Values) then
    result := Values[High(Values)];
end;

destructor TADSITreeNode.Destroy;
begin
  inherited Destroy;
end;

function TADSITreeNode.FindDN(const DistinguishedName: RawUtf8): TTreeNode;
begin
  result := GetFirstChild;
  while (Result <> nil) and not (EqualBuf((result as TADSITreeNode).DistinguishedName, DistinguishedName)) do
    result := result.GetNextSibling;
end;

procedure TADSITreeNode.SetEntry(const Entry: TLdapEntryData);
begin
  fData := Entry;
  fUpdated := True;
end;

procedure TADSITreeNode.StartUpdateChildren;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    (Items[i] as TADSITreeNode).Updated := False;
end;

procedure TADSITreeNode.EndUpdateChildren;
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
    if not (Items[i] as TADSITreeNode).Updated then
      TreeView.Items.Delete(Items[i]);
end;

{ TFrmModuleADSI }

procedure TFrmModuleADSI.Action_RefreshExecute(Sender: TObject);
var
  NamingContext: RawUtf8;
  RootNode: TADSITreeNode;
  res: TLdapSearchResult;
  SR: TLdapSearchRequest;
  c: TCursor;
begin
  c := Screen.Cursor;
  Screen.Cursor := crHourGlass;

  TisGrid1.Clear;
  TisGrid2.Clear;
  TreeView1.Items.Clear;
  TreeView1.BeginUpdate;
  try
    SR.Options := DefaultSearchRequestOptions;
    for NamingContext in Ldap.Context.NamingContexts do
    begin
      SearchRequest(SR, NamingContext, '', ['description', 'distinguishedName', 'name', 'objectClass'], lssBaseObject);
      res := LDAP.Search(SR);
      if not res.OperationResult.Success then
        Exit;
      if res.ReturnedCount <> 1 then
        Exit;
      RootNode := (TreeView1.Items.Add(nil, GetLdapEntryReadable(res.Entries[0], 'name', 0)) as TADSITreeNode);
      RootNode.HasChildren := True;
      RootNode.SetEntry(res.Entries[0]);
    end;
  finally
    TreeView1.AlphaSort;
    TreeView1.EndUpdate;
    Screen.Cursor := c;
  end;
end;

procedure TFrmModuleADSI.Timer_TreeChangeNodeTimer(Sender: TObject);
begin
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, 'Timer Tree Change', Self);

  Timer_TreeChangeNode.Enabled := False;

  RefreshNode((TreeView1.Selected as TADSITreeNode));
  UpdateGrid((TreeView1.Selected as TADSITreeNode));
  UpdateGridAttribute((TreeView1.Selected as TADSITreeNode));
end;

procedure TFrmModuleADSI.Timer_SearchInGridTimer(Sender: TObject);
begin
  Timer_SearchInGrid.Enabled := False;
end;

procedure TFrmModuleADSI.Action_PropertyExecute(Sender: TObject);
var
  NodeData: PDocVariantData;
begin
  if TisGrid1.SelectedCount = 1 then
  begin
    NodeData := TisGrid1.GetNodeAsPDocVariantData(TisGrid1.GetFirstSelected());
    fIContext.OpenProperty(NodeData^.S['distinguishedName']);
  end
  else if Assigned(TreeView1.Selected) then
  begin
    fIContext.OpenProperty((TreeView1.Selected as TADSITreeNode).DistinguishedName);
  end;
end;

procedure TFrmModuleADSI.Action_PropertyUpdate(Sender: TObject);
begin
  Action_Property.Enabled := (TisGrid1.SelectedCount > 0) and Ldap.IsConnected;
end;

procedure TFrmModuleADSI.Action_NewObjectExecute(Sender: TObject);
//var
//  vis: TVisNewObject;
begin
  //vis := TVisNewObject.Create(Self, vnotNone, LdapClient.DefaultDN, LdapClient.DefaultDN);
  //
  //try
  //  vis.Ldap := LdapClient;
  //  vis.ShowModal;
  //finally
  //  FreeAndNil(vis);
  //end;
end;

procedure TFrmModuleADSI.Action_NewObjectUpdate(Sender: TObject);
begin
  Action_NewObject.Enabled := False;
end;

procedure TFrmModuleADSI.Action_DeleteObjectExecute(Sender: TObject);
var
  SelectedObjects: TDocVariantData;
  SelectedObject: PDocVariantData;
  Res: TLdapOperationResult;
  Request: TLdapDeleteRequest;
begin
  SelectedObjects := TisGrid1.SelectedRows;

  if (mrYes <> MessageDlg(rsGridDeleteRows, Format(rsGridConfDeleteRow, [TisGrid1.SelectedCount]), mtConfirmation, mbYesNoCancel, 0)) then
    Exit;

  for SelectedObject in SelectedObjects.Objects do
  begin
    if not Assigned(SelectedObject) then
      continue;
    if not SelectedObject^.Exists('distinguishedName') then
    begin
      if Assigned(fLog) then
        fLog.Add.Log(sllWarning, 'No distinguishedName');
      continue;
    end;

    Request.DistinguishedName := SelectedObject^.S['distinguishedName'];
    Request.DeleteChildren := True;
    Res := Ldap.Delete(Request);
    if not Res.Success then
      Exit;
  end;
  TisGrid1.DeleteRows(@SelectedObjects);
end;

procedure TFrmModuleADSI.Action_DeleteObjectUpdate(Sender: TObject);
begin
  Action_DeleteObject.Enabled := (TisGrid1.SelectedCount > 0) and Ldap.IsConnected;
end;

procedure TFrmModuleADSI.TisGrid1Change(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  NodeData: PDocVariantData;
begin
  if not Assigned(Node) then
    Exit;
  NodeData := TisGrid1.GetNodeAsPDocVariantData(Node);
  if not Assigned(NodeData) then
    Exit;
  if not NodeData^.Exists('distinguishedName') then
    Exit;
  UpdateGridAttribute(NodeData^.S['distinguishedName']);
end;

procedure TFrmModuleADSI.TisGrid1Click(Sender: TObject);
begin
  ;
end;

procedure TFrmModuleADSI.TisGrid1DblClick(Sender: TObject);
var
  NodeData: PDocVariantData;
  Node: TTreeNode;
begin
  NodeData := TisGrid1.FocusedRow;
  if not Assigned(NodeData) then
    Exit;
  Node := TreeView1.Selected.FindNode(NodeData^.S['name']);
  if Assigned(Node) then
  begin
    TreeView1.Select(Node);
  end
  else
  begin
    fIContext.OpenProperty(NodeData^.S['distinguishedName']);
  end;
end;

procedure TFrmModuleADSI.TisGrid1GetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  NodeData: PDocVariantData;
begin
  NodeData := TisGrid1.GetNodeAsPDocVariantData(Node);
  if not Assigned(NodeData) then
    Exit;

  case TisGrid1.FindColumnByIndex(Column).PropertyName of
    'name':
    begin
      if (ImageIndex < 0) and NodeData^.Exists('type') then
        ImageIndex := ObjectClassToImageIndex(NodeData^.S['type']);
    end;
  end;
end;

procedure TFrmModuleADSI.TisGrid1KeyPress(Sender: TObject; var Key: char);
begin
  SearchInGrid(Timer_SearchInGrid, TisGrid1, fSearchWord, Key);
end;

procedure TFrmModuleADSI.TisGrid1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not Assigned(TisGrid1.GetNodeAt(X, Y)) then
    TisGrid1.ClearSelection;
end;

procedure TFrmModuleADSI.TisGrid2KeyPress(Sender: TObject; var Key: char);
begin
  SearchInGrid(Timer_SearchInGrid, TisGrid2, fSearchWord, Key);
end;

procedure TFrmModuleADSI.TisSearchEdit_ADSISearch(Sender: TObject;
  const aText: string);
var
  lowerText: String;
  Node: PVirtualNode;
  NodeData: PDocVariantData;
  Filtered: Boolean;
  FieldName: RawUtf8;
begin
  lowerText := aText.ToLower;
  Node := TisGrid1.GetFirst();
  while Assigned(Node) do
  begin
    if (aText = '') then
    begin
      TisGrid1.IsFiltered[Node] := False;
      Node := TisGrid1.GetNext(Node);
      Continue;
    end;

    NodeData := TisGrid1.GetNodeAsPDocVariantData(Node);
    if not Assigned(NodeData) then
    begin
      TisGrid1.IsFiltered[Node] := True;
      Node := TisGrid1.GetNext(Node);
      Continue;
    end;

    with NodeData^ do
    begin
      Filtered := True;
      for FieldName in Names do
      begin
        if not TisGrid1.IsVisibleColumnByPropertyName(FieldName) or not Exists(FieldName) then
          Continue;
        Filtered := not S[FieldName].ToLower.Contains(lowerText);
        if not Filtered then
          Break;
      end;
      TisGrid1.IsFiltered[Node] := Filtered;
    end;
    Node := TisGrid1.GetNext(Node);
  end;
end;

procedure TFrmModuleADSI.TisSearchEdit_TreeADSISearch(Sender: TObject;
  const aText: string);
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

var
  Node: TTreeNode;
  LowerText: String;
begin
  Node := TreeView1.Items.GetFirstNode;
  if not Assigned(Node) then
    Exit;

  LowerText := aText.ToLower;

  repeat
    Node.Visible := (HasVisibleChild(Node, LowerText)) or (LowerText = '') or (Node.Text.ToLower.Contains(LowerText));
    Node := Node.GetNextSibling;
  until not Assigned(Node);
end;

procedure TFrmModuleADSI.TreeView1Change(Sender: TObject; Node: TTreeNode);
begin
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, 'Tree Change', Self);

  if Timer_TreeChangeNode.Enabled then
    Timer_TreeChangeNode.Enabled := False;
  Timer_TreeChangeNode.Enabled := True;
end;

procedure TFrmModuleADSI.TreeView1Click(Sender: TObject);
begin
  TisGrid1.ClearSelection;
end;

procedure TFrmModuleADSI.TreeView1CreateNodeClass(Sender: TCustomTreeView;
  var NodeClass: TTreeNodeClass);
begin
  NodeClass := TADSITreeNode;
end;

procedure TFrmModuleADSI.TreeView1Expanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
begin
  RefreshNode((Node as TADSITreeNode));
end;

procedure TFrmModuleADSI.TreeView1GetImageIndex(Sender: TObject; Node: TTreeNode
  );
begin
  if node.ImageIndex >= 0 then
    Exit;
  Node.ImageIndex := ObjectClassToImageIndex((Node as TADSITreeNode).ObjectType);
  Node.SelectedIndex := Node.ImageIndex;
end;

procedure TFrmModuleADSI.RefreshNode(Node: TADSITreeNode);
var
  ChildNode: TADSITreeNode;
  EntryName: RawUtf8;
  i: Integer;
  c: TCursor;
  Req: TLdapSearchRequest;
  Res: TLdapSearchResult;
  PEntry: PLdapEntryData;
  Timer: TPrecisionTimer;
begin
  if not Assigned(Node) or (Node.DistinguishedName = '') then
    Exit;

  c := Screen.Cursor;
  Screen.Cursor := crHourGlass;

  Req.Options := DefaultSearchRequestOptions;
  SearchRequest(Req, Node.DistinguishedName, '', ['description', 'distinguishedName', 'name', 'objectClass']);
  Res := Ldap.Search(Req);

  fLog.Add.Log(sllTrace, 'Ldap Search (%)', [Timer.Time]);
  Timer.Resume;
  TreeView1.Items.BeginUpdate;
  Node.StartUpdateChildren;
  try
    for i := 0 to Res.ReturnedCount - 1 do
    begin
      PEntry := @Res.Entries[i];
      EntryName := GetLdapEntryReadable(PEntry^, 'name', 0);
      if EntryName = '' then
        Continue;
      ChildNode := (Node.FindDN(PEntry^.DistinguishedName) as TADSITreeNode);
      if not Assigned(ChildNode) then
      begin
        ChildNode := (TreeView1.Items.AddChild(Node, EntryName) as TADSITreeNode);
        ChildNode.HasChildren := True;
      end;
      ChildNode.SetEntry(PEntry^);
    end;
    Node.AlphaSort;
    Node.HasChildren := Node.Count > 0;
  finally
    Node.EndUpdateChildren;
    TreeView1.Items.EndUpdate;
    Screen.Cursor := c;
  end;
end;

function TFrmModuleADSI.GetLdap: ILdapConnection;
begin
  result := fModule.RSAT.LdapConnection;
end;

procedure TFrmModuleADSI.UpdateGrid(Node: TADSITreeNode);
var
  data: TDocVariantData;
  i: Integer;
  ChildNode: TADSITreeNode;
begin
  if not Assigned(Node) then
    Exit;

  TisGrid1.Clear;
  TisGrid1.BeginUpdate;
  try
    for i := 0 to Node.Count - 1 do
    begin
      ChildNode := (Node.Items[i] as TADSITreeNode);
      data.init(JSON_FAST);
      data.U['description'] := ChildNode.Description;
      data.U['distinguishedName'] := ChildNode.DistinguishedName;
      data.U['name'] := ChildNode.DisplayName;
      data.U['type'] := ChildNode.ObjectType;
      TisGrid1.Data.AddItem(data);
      data.Clear;
    end;
  finally
    TisGrid1.EndUpdate;
    TisGrid1.LoadData();
  end;
end;

procedure TFrmModuleADSI.UpdateGridAttribute(Node: TADSITreeNode);
begin
  if not Assigned(Node) then
    Exit;

  UpdateGridAttribute(Node.DistinguishedName);
end;

procedure TFrmModuleADSI.UpdateGridAttribute(DistinguishedName: String);
var
  data: TDocVariantData;
  i, j: Integer;
  Request: TLdapSearchRequest;
  Res: TLdapSearchResult;
  PEntry: PLdapEntryData;
  v: RawUtf8;
begin
  SearchRequestOptions(Request.Options, 1, 5, 1, []);
  SearchRequest(Request, DistinguishedName, '', ['*'], lssBaseObject);
  Res := Ldap.Search(Request);
  if not Res.OperationResult.Success then
    Exit;

  if Res.ReturnedCount <> 1 then
    Exit;

  TisGrid2.Clear;
  TisGrid2.BeginUpdate;
  try
    PEntry := @Res.Entries[0];
    for i := 0 to PEntry^.AttributeCount - 1 do
    begin
      Data.init(JSON_FAST);
      Data.AddOrUpdateValue('attribute', PEntry^.Attributes[i].Name);
      for j := 0 to High(PEntry^.Attributes[i].Values) do
      begin
        v := PEntry^.Attributes[i].Values[j];
        AttributeValueMakeReadable(v, AttrTypeStorage[AttributeNameType(PEntry^.Attributes[i].Name)]);
        Data.AddOrUpdateValue('value',  v);
        TisGrid2.Data.AddItem(data);
      end;
      Data.Clear;
    end;
  finally
    TisGrid2.EndUpdate;
    TisGrid2.LoadData();
  end;
end;

procedure TFrmModuleADSI.LdapConnectEvent(Sender: TObject);
var
  Settings: TLdapConnectionSettings;
  Res: TLdapOperationResult;
  Credentials: TLdapCredentials;
begin
  Settings.UseCldapDiscovery := True;
  Settings.DiscoverWhenHostEmpty := True;
  Settings.DiscoveryDelayMS := 5000;
  Settings.AutoReconnect := True;
  Settings.SelectClosestServer := True;
  Settings.UseCldapDiscovery := True;

  Res := Ldap.Connect(Settings);

  Credentials.AllowUnsafePasswordBind := True;
  Credentials.Authentication := ldamKerberos;
  Res := Ldap.Bind(Credentials);
  Action_Refresh.Execute;
end;

procedure TFrmModuleADSI.LdapCloseEvent(Sender: TObject);
begin
  TreeView1.Items.Clear;
  TisGrid1.Clear;
end;

constructor TFrmModuleADSI.Create(Context: IOpenRSATUIContext);
begin
  Inherited Create(Context.ComponentOwner);

  fLog := TADSILog;
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, '% - Create', [Self.Name]);

  fIContext := Context;

  fModule := TModuleADSI.Create(Context.RSAT);

  fLog.Add.Log(sllTrace, 'Created');
  Image1.Visible := not IsDarkMode;
  fLog.Add.Log(sllTrace, 'visible1');
  Image2.Visible := not Image1.Visible;
  fLog.Add.Log(sllTrace, 'visible2');
end;

destructor TFrmModuleADSI.Destroy;
begin
  FreeAndNil(fModule);

  inherited Destroy;
end;

procedure TFrmModuleADSI.Refresh;
begin
  Action_Refresh.Execute;
end;

procedure TFrmModuleADSI.Load;
begin

end;

function TFrmModuleADSI.GetModule: TModule;
begin
  result := fModule;
end;

function TFrmModuleADSI.GetFrmOptionClass: TFrameOptionClass;
begin
  result := nil;
end;

function TFrmModuleADSI.GetOnLdapConnect: TNotifyEvent;
begin
  result := @LdapConnectEvent;
end;

function TFrmModuleADSI.GetOnLdapClose: TNotifyEvent;
begin
  result := @LdapCloseEvent;
end;

end.

