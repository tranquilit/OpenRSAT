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
  uinterfacemodule,
  uinterfacecore,
  tis.ui.grid.core,
  mormot.core.log,
  mormot.core.base,
  mormot.net.ldap,
  ucoredatamodule,
  VirtualTrees;

type

  { TADSITreeNode }

  TADSITreeNode = class(TTreeNode)
  private
    fAttributes: TLdapAttributeList;

    function GetDistinguishedName: String;
    function GetObjectType: String;
  public
    destructor Destroy; override;
  published
    property DistinguishedName: String read GetDistinguishedName;
    property ObjectType: String read GetObjectType;
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
    Label1: TLabel;
    MenuItem1: TMenuItem;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    StatusBar1: TStatusBar;
    Timer_TreeChangeNode: TTimer;
    Timer_SearchInGrid: TTimer;
    TisGrid1: TTisGrid;
    TisGrid2: TTisGrid;
    ToolBar1: TToolBar;
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
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure TreeView1Click(Sender: TObject);
    procedure TreeView1CreateNodeClass(Sender: TCustomTreeView;
      var NodeClass: TTreeNodeClass);
    procedure TreeView1Expanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure TreeView1GetImageIndex(Sender: TObject; Node: TTreeNode);
  private
    fCore: ICore;
    fLog: TSynLog;
    fEnabled: Boolean;

    fSearchWord: RawUtf8;

    procedure RefreshNode(Node: TADSITreeNode);
    procedure UpdateGrid(Node: TADSITreeNode);
    procedure UpdateGridAttribute(Node: TADSITreeNode); overload;
    procedure UpdateGridAttribute(DistinguishedName: String); overload;

    procedure OnLdapClientConnect(LdapClient: TLdapClient);
    procedure OnLdapClientClose(LdapClient: TLdapClient);
  public
    constructor Create(TheOwner: TComponent; ACore: ICore); reintroduce;
  published
    /// IModule
    function GetModuleEnabled: Boolean; override;
    procedure SetModuleEnabled(AValue: Boolean); override;
    function GetModuleName: String; override;
    function GetModuleDisplayName: String; override;
    function GetOptions: TOptions; override;
    procedure Refresh; override;
    procedure Load; override;
  end;

implementation
uses
  mormot.core.variants,
  ucommon,
  uvisnewobject;

{$R *.lfm}

{ TADSITreeNode }

function TADSITreeNode.GetDistinguishedName: String;
begin
  result := fAttributes.Find('distinguishedName').GetReadable();
end;

function TADSITreeNode.GetObjectType: String;
var
  Attribute: TLdapAttribute;
begin
  result := '';
  Attribute := fAttributes.Find('objectClass');
  if not Assigned(Attribute) then
    Exit;
  result := Attribute.GetReadable(Attribute.Count - 1);
end;

destructor TADSITreeNode.Destroy;
begin
  inherited Destroy;
  FreeAndNil(fAttributes);
end;

{ TFrmModuleADSI }

procedure TFrmModuleADSI.Action_RefreshExecute(Sender: TObject);
var
  Context, NodeName: RawUtf8;
  RootNode: TADSITreeNode;
  SearchResult: TLdapResult;
  c: TCursor;
begin
  c := Screen.Cursor;
  Screen.Cursor := crHourGlass;

  TisGrid1.Clear;
  TisGrid2.Clear;
  TreeView1.Items.Clear;
  TreeView1.BeginUpdate;
  try
    RootNode := (TreeView1.Items.Add(nil, 'RootDSE') as TADSITreeNode);
    for Context in fCore.LdapClient.NamingContexts do
    begin
      SearchResult := fCore.LdapClient.SearchObject(Context, '', ['description', 'distinguishedName', 'name', 'objectClass']);
      if not Assigned(SearchResult) then
        continue;
      NodeName := DNToCN(SearchResult.ObjectName);
      RootNode := (TreeView1.Items.Add(nil, NodeName) as TADSITreeNode);
      RootNode.fAttributes := TLdapAttributeList(SearchResult.Attributes.Clone);
      RefreshNode(RootNode);
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
    fLog.Log(sllTrace, 'Timer Tree Change', Self);

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
    fCore.OpenProperty(NodeData^.S['name'], NodeData^.S['distinguishedName']);
  end
  else if Assigned(TreeView1.Selected) then
  begin
    fCore.OpenProperty(TreeView1.Selected.Text, (TreeView1.Selected as TADSITreeNode).DistinguishedName);
  end;
end;

procedure TFrmModuleADSI.Action_NewObjectExecute(Sender: TObject);
var
  vis: TVisNewObject;
begin
  vis := TVisNewObject.Create(Self, vnotNone, fCore.LdapClient.DefaultDN, fCore.LdapClient.DefaultDN);

  try
    vis.Ldap := fCore.LdapClient;
    vis.ShowModal;
  finally
    FreeAndNil(vis);
  end;
end;

procedure TFrmModuleADSI.Action_NewObjectUpdate(Sender: TObject);
begin
  Action_NewObject.Enabled := False;
end;

procedure TFrmModuleADSI.Action_DeleteObjectExecute(Sender: TObject);
var
  SelectedObjects: TDocVariantData;
  SelectedObject: PDocVariantData;
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
        fLog.Log(sllWarning, 'No distinguishedName');
      continue;
    end;

    if not fCore.LdapClient.Delete(SelectedObject^.S['distinguishedName']) then
    begin
      if Assigned(fLog) then
        fLog.Log(sllError, fCore.LdapClient.ResultString);
      continue;
    end;
  end;
  TisGrid1.DeleteRows(@SelectedObjects);
end;

procedure TFrmModuleADSI.Action_DeleteObjectUpdate(Sender: TObject);
begin
  Action_DeleteObject.Enabled := Assigned(fCore.LdapClient) and fCore.LdapClient.Connected and (TisGrid1.SelectedCount > 0);
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
    fCore.OpenProperty(NodeData^.S['name'], NodeData^.S['distinguishedName']);
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
        ImageIndex := CoreDataModule.objectClassToImageIndex(NodeData^.S['type']);
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

procedure TFrmModuleADSI.TreeView1Change(Sender: TObject; Node: TTreeNode);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Tree Change', Self);

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
  Node.ImageIndex := CoreDataModule.objectClassToImageIndex((Node as TADSITreeNode).ObjectType);
  Node.SelectedIndex := Node.ImageIndex;
end;

procedure TFrmModuleADSI.RefreshNode(Node: TADSITreeNode);
var
  SearchResult: TLdapResult;
  ChildNode: TADSITreeNode;
  NodeCache: TDocVariantData;
  NodeName: RawUtf8;
  i, Count: Integer;
  c: TCursor;
  NodeNameAttribute: TLdapAttribute;
begin
  if not Assigned(Node) or (Node.DistinguishedName = '') then
    Exit;

  c := Screen.Cursor;
  Screen.Cursor := crHourGlass;

  NodeCache.Init();
  TreeView1.Items.BeginUpdate;
  fCore.LdapClient.SearchBegin();
  try
    fCore.LdapClient.SearchScope := lssSingleLevel;

    repeat
      if not fCore.LdapClient.Search(Node.DistinguishedName, False, '', ['description', 'distinguishedName', 'name', 'objectClass']) then
      begin
        if Assigned(fLog) then
          fLog.Log(sllError, '% - Ldap Search Error: "%"', [Self.Name, fCore.LdapClient.ResultString]);
        Exit;
      end;

      for SearchResult in fCore.LdapClient.SearchResult.Items do
      begin
        if not Assigned(SearchResult) then
          continue;

        NodeNameAttribute := SearchResult.Find('name');
        if not Assigned(NodeNameAttribute) then
          continue;

        NodeName := NodeNameAttribute.GetReadable();
        if NodeName = '' then
          continue;

        ChildNode := (Node.FindNode(NodeName) as TADSITreeNode);
        if not Assigned(ChildNode) then
        begin
          ChildNode := (TreeView1.Items.AddChild(Node, NodeName) as TADSITreeNode);
          ChildNode.HasChildren := True;
        end
        else
          FreeAndNil(ChildNode.fAttributes);
        NodeCache.B[NodeName] := True;
        ChildNode.fAttributes := TLdapAttributeList(SearchResult.Attributes.Clone);
      end;
    until fCore.LdapClient.SearchCookie = '';

    Count := Node.Count;
    for i := 0 to Node.Count - 1 do
    begin
      if not NodeCache.Exists(Node.Items[Count - 1 - i].Text) then
        TreeView1.Items.Delete(Node.Items[Count - 1 - i]);
    end;
    Node.AlphaSort;
    Node.HasChildren := Node.Count > 0;
  finally
    fCore.LdapClient.SearchEnd;
    TreeView1.Items.EndUpdate;
    Screen.Cursor := c;
  end;
end;

procedure TFrmModuleADSI.UpdateGrid(Node: TADSITreeNode);
var
  SearchResult: TLdapResult;
  data: TDocVariantData;
  Attribute: TLdapAttribute;
  c: TCursor;
begin
  if not Assigned(Node) then
    Exit;

  c := Screen.Cursor;
  Screen.Cursor := crHourGlass;

  data.Init;

  TisGrid1.Clear;
  TisGrid1.BeginUpdate;
  fCore.LdapClient.SearchBegin();
  try
    if (Node.DistinguishedName = '') then
      Exit;
    fCore.LdapClient.SearchScope := lssSingleLevel;

    repeat
      if not fCore.LdapClient.Search(Node.DistinguishedName, False, '', ['description', 'distinguishedName', 'name', 'objectClass']) then
      begin
        if Assigned(fLog) then
          fLog.Log(sllError, '% - Ldap Search Error: "%"', [Self.Name, fCore.LdapClient.ResultString]);
        Exit;
      end;

      for SearchResult in fCore.LdapClient.SearchResult.Items do
      begin
        if not Assigned(SearchResult) then
          continue;

        Attribute := SearchResult.Find('name');
        if not Assigned(Attribute) then
          continue;
        if (Attribute.GetReadable() = '') then
          continue;

        data.AddOrUpdateValue('name', Attribute.GetReadable());

        Attribute := SearchResult.Find('objectClass');
        if Assigned(Attribute) and (Attribute.Count > 0) then
          data.AddOrUpdateValue('type', Attribute.GetReadable(Attribute.Count - 1));
        Attribute := SearchResult.Find('description');
        if Assigned(Attribute) then
          data.AddOrUpdateValue('description', Attribute.GetReadable());
        Attribute := SearchResult.Find('distinguishedName');
        if Assigned(Attribute) then
          data.AddOrUpdateValue('distinguishedName', Attribute.GetReadable())
        else
          data.AddOrUpdateValue('distinguishedName', SearchResult.ObjectName);
        TisGrid1.Data.AddItem(data);
        data.Clear;
      end;
    until fCore.LdapClient.SearchCookie = '';
  finally
    fCore.LdapClient.SearchEnd;
    TisGrid1.EndUpdate;
    TisGrid1.LoadData();
    TisGrid1.ClearSelection;
    Screen.Cursor := c;
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
  c: TCursor;
  Attribute: TLdapAttribute;
  Attributes: TLdapResult;
  data: TDocVariantData;
  i: Integer;
begin
  c := Screen.Cursor;
  Screen.Cursor := crHourGlass;

  data.Init;
  TisGrid2.Clear;
  TisGrid2.BeginUpdate;
  try
    Attributes := fCore.LdapClient.SearchObject(DistinguishedName, '', ['*']);
    if not Assigned(Attributes) then
    begin
      if Assigned(fLog) then
        fLog.Log(sllError, '% - Ldap Search Object Error: "%"', [Self.Name, fCore.LdapClient.ResultString]);
      Exit;
    end;

    for Attribute in Attributes.Attributes.Items do
    begin
      if not Assigned(Attribute) then
        continue;
      data.AddOrUpdateValue('attribute', Attribute.AttributeName);
      for i := 0 to Attribute.Count - 1 do
      begin
        data.AddOrUpdateValue('value', Attribute.GetReadable(i));
        TisGrid2.Data.AddItem(data);
      end;
      data.Clear;
    end;
  finally
    TisGrid2.EndUpdate;
    TisGrid2.LoadData();
    Screen.Cursor := c;
  end;
end;

procedure TFrmModuleADSI.OnLdapClientConnect(LdapClient: TLdapClient);
begin
  Action_Refresh.Execute;
end;

procedure TFrmModuleADSI.OnLdapClientClose(LdapClient: TLdapClient);
begin
  TreeView1.Items.Clear;
end;

constructor TFrmModuleADSI.Create(TheOwner: TComponent; ACore: ICore);
begin
  Inherited Create(TheOwner);

  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Create', [Self.Name]);

  fCore := ACore;
  fEnabled := True;

  fCore.LdapClient.RegisterObserverConnect(@OnLdapClientConnect);
  fCore.LdapClient.RegisterObserverClose(@OnLdapClientClose);
end;

function TFrmModuleADSI.GetModuleEnabled: Boolean;
begin
  result := fEnabled;
end;

procedure TFrmModuleADSI.SetModuleEnabled(AValue: Boolean);
begin
  fEnabled := AValue;
end;

function TFrmModuleADSI.GetModuleName: String;
begin
  result := rsModuleADSIName;
end;

function TFrmModuleADSI.GetModuleDisplayName: String;
begin
  result := rsModuleADSIDisplayName;
end;

function TFrmModuleADSI.GetOptions: TOptions;
begin
  result := nil;
end;

procedure TFrmModuleADSI.Refresh;
begin
  Action_Refresh.Execute;
end;

procedure TFrmModuleADSI.Load;
begin

end;

end.

