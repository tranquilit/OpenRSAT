unit ufrmmoduleaduc;

{$mode objfpc}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
  {$ifdef windows}
  ActiveX,
  {$else}
  FakeActiveX,
  {$endif}
  Classes,
  SysUtils,
  Forms,
  Controls,
  ComCtrls,
  ExtCtrls,
  StdCtrls,
  ActnList,
  Menus,
  tis.ui.grid.core,
  VirtualTrees,
  mormot.core.log,
  umoduleADUC,
  uinterfacemodule,
  uinterfacecore,
  IniFiles,
  Types,
  mormot.core.variants,
  mormot.core.text,
  mormot.core.base,
  mormot.net.ldap,
  uaductreeview,
  utreeselectionhistory,
  uvisproperties,
  ucoredatamodule,
  ufrmmoduleaducoptions,
  ursatldapclient;

type

  { TFrmModuleADUC }

  TFrmModuleADUC = class(TFrameModule)
    ActionList_ADUC: TActionList;
    Action_ChangePartition: TAction;
    Action_Copy: TAction;
    Action_Cut: TAction;
    Action_DelegateControl: TAction;
    Action_Delete: TAction;
    Action_Filter: TAction;
    Action_NewAll: TAction;
    Action_NewComputer: TAction;
    Action_NewContact: TAction;
    Action_NewDefault: TAction;
    Action_NewGroup: TAction;
    Action_NewInetOrgPerson: TAction;
    Action_NewOU: TAction;
    Action_NewSharedFolder: TAction;
    Action_NewUser: TAction;
    Action_Next: TAction;
    Action_Parent: TAction;
    Action_Paste: TAction;
    Action_Previous: TAction;
    Action_Properties: TAction;
    Action_Refresh: TAction;
    Action_Search: TAction;
    Action_SearchRoot: TAction;
    Action_ShowGPO: TAction;
    Action_SitesAndServices: TAction;
    Action_SwitchToolbarCaption: TAction;
    Action_TaskAddToAGroup: TAction;
    Action_TaskCopy: TAction;
    Action_TaskDelegateControl: TAction;
    Action_TaskDisableAccount: TAction;
    Action_TaskManage: TAction;
    Action_TaskMove: TAction;
    Action_TaskNameMappings: TAction;
    Action_TaskOpenHomePage: TAction;
    Action_TaskResetPassword: TAction;
    Action_TaskSendMail: TAction;
    Action_TreeNewAll: TAction;
    Action_UsersAndComputers: TAction;
    MenuItem_NewSubnet: TMenuItem;
    MenuItem_NewUser: TMenuItem;
    MenuItem_NewSharedFolder: TMenuItem;
    MenuItem_NewSite: TMenuItem;
    MenuItem_NewSiteLink: TMenuItem;
    MenuItem_NewSiteLinkBridge: TMenuItem;
    MenuItem_NewSiteContainer: TMenuItem;
    MenuItem_NewSiteSettings: TMenuItem;
    MenuItem_NewOU: TMenuItem;
    MenuItem_NewPrinter: TMenuItem;
    MenuItem_NewServer: TMenuItem;
    MenuItem_NewServersContainer: TMenuItem;
    MenuItem_NewComputer: TMenuItem;
    MenuItem_NewMsDSAuthNPolicy: TMenuItem;
    MenuItem_NewMsDSAuthNPolicySilo: TMenuItem;
    MenuItem_NewMsDSClaimType: TMenuItem;
    MenuItem_NewMsDSKeyCredential: TMenuItem;
    MenuItem_NewMsDSResourceProperty: TMenuItem;
    MenuItem_NewMsDSResourcePropertyList: TMenuItem;
    MenuItem_NewMsDSShadowPrincipal: TMenuItem;
    MenuItem_NewMsDSShadowPrincipalContainer: TMenuItem;
    MenuItem_NewMsImagingPSPs: TMenuItem;
    MenuItem_NewMsImagingPostScanProcess: TMenuItem;
    MenuItem_NewContact: TMenuItem;
    MenuItem_NewMSMQQueueAlias: TMenuItem;
    MenuItem_NewConnection: TMenuItem;
    MenuItem_NewGroup: TMenuItem;
    MenuItem_NewInetOrgPerson: TMenuItem;
    MenuItem_NewMSMQQueue: TMenuItem;
    MenuItem_NewMSMQRoutingLink: TMenuItem;
    MenuItem_NewMsAuthzCentralAccessPolicy: TMenuItem;
    MenuItem_NewMsAuthzCentralAccessRule: TMenuItem;
    MenuItem_NewMsDNSServerSettings: TMenuItem;
    MenuItem_SwitchToolbarSize: TMenuItem;
    MenuItem_Copy: TMenuItem;
    MenuItem_Cut: TMenuItem;
    MenuItem_Paste: TMenuItem;
    MenuItem_DelegateControl: TMenuItem;
    MenuItem_SendMail: TMenuItem;
    MenuItem_Manage: TMenuItem;
    MenuItem_Find: TMenuItem;
    MenuItem_ChangePartition: TMenuItem;
    MenuItem_: TMenuItem;
    MenuItem_AddToAGroup: TMenuItem;
    MenuItem_NameMapping: TMenuItem;
    MenuItem_DisableAccount: TMenuItem;
    MenuItem_ResetPassword: TMenuItem;
    MenuItem_Move: TMenuItem;
    MenuItem_OpenHomePage: TMenuItem;
    MenuItem_Delete: TMenuItem;
    MenuItem_Refresh: TMenuItem;
    MenuItem_Properties: TMenuItem;
    MenuItem_New: TMenuItem;
    PopupMenu1: TPopupMenu;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    Separator3: TMenuItem;
    Separator4: TMenuItem;
    Timer_TreeChangeNode: TTimer;
    Timer_SearchInGrid: TTimer;
    Label1: TLabel;
    MenuItem_Search: TMenuItem;
    Splitter1: TSplitter;
    GridADUC: TTisGrid;
    ToolBar1: TToolBar;
    ToolButton_AddToGroup: TToolButton;
    ToolButton_Copy: TToolButton;
    ToolButton_Cut: TToolButton;
    ToolButton_Delete: TToolButton;
    ToolButton_Divider1: TToolButton;
    ToolButton_Divider2: TToolButton;
    ToolButton_Divider3: TToolButton;
    ToolButton_Filter: TToolButton;
    ToolButton_Group: TToolButton;
    ToolButton_Next: TToolButton;
    ToolButton_OU: TToolButton;
    ToolButton_Parent: TToolButton;
    ToolButton_Paste: TToolButton;
    ToolButton_Previous: TToolButton;
    ToolButton_Property: TToolButton;
    ToolButton_Search: TToolButton;
    ToolButton_User: TToolButton;
    TreeADUC: TTreeView;
    procedure Action_CopyExecute(Sender: TObject);
    procedure Action_CopyUpdate(Sender: TObject);
    procedure Action_CutExecute(Sender: TObject);
    procedure Action_CutUpdate(Sender: TObject);
    procedure Action_DelegateControlExecute(Sender: TObject);
    procedure Action_DelegateControlUpdate(Sender: TObject);
    procedure Action_DeleteExecute(Sender: TObject);
    procedure Action_DeleteUpdate(Sender: TObject);
    procedure Action_NewAllUpdate(Sender: TObject);
    procedure Action_NewComputerExecute(Sender: TObject);
    procedure Action_NewComputerUpdate(Sender: TObject);
    procedure Action_NewGroupExecute(Sender: TObject);
    procedure Action_NewGroupUpdate(Sender: TObject);
    procedure Action_NewOUExecute(Sender: TObject);
    procedure Action_NewOUUpdate(Sender: TObject);
    procedure Action_NewUserExecute(Sender: TObject);
    procedure Action_NewUserUpdate(Sender: TObject);
    procedure Action_NextExecute(Sender: TObject);
    procedure Action_NextUpdate(Sender: TObject);
    procedure Action_ParentExecute(Sender: TObject);
    procedure Action_ParentUpdate(Sender: TObject);
    procedure Action_PasteExecute(Sender: TObject);
    procedure Action_PasteUpdate(Sender: TObject);
    procedure Action_PreviousExecute(Sender: TObject);
    procedure Action_PreviousUpdate(Sender: TObject);
    procedure Action_PropertiesExecute(Sender: TObject);
    procedure Action_PropertiesUpdate(Sender: TObject);
    procedure Action_RefreshExecute(Sender: TObject);
    procedure Action_RefreshUpdate(Sender: TObject);
    procedure Action_SearchExecute(Sender: TObject);
    procedure Action_SearchUpdate(Sender: TObject);
    procedure Action_SwitchToolbarCaptionExecute(Sender: TObject);
    procedure Action_TaskAddToAGroupExecute(Sender: TObject);
    procedure Action_TaskAddToAGroupUpdate(Sender: TObject);
    procedure Action_TaskMoveExecute(Sender: TObject);
    procedure Action_TaskMoveUpdate(Sender: TObject);
    procedure Action_TaskResetPasswordExecute(Sender: TObject);
    procedure Action_TaskResetPasswordUpdate(Sender: TObject);
    procedure GridADUCKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure Timer_SearchInGridTimer(Sender: TObject);
    procedure Timer_TreeChangeNodeTimer(Sender: TObject);
    procedure TreeADUCChange(Sender: TObject; Node: TTreeNode);
    procedure TreeADUCContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure TreeADUCCreateNodeClass(Sender: TCustomTreeView;
      var NodeClass: TTreeNodeClass);
    procedure TreeADUCDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure TreeADUCDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure TreeADUCEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure TreeADUCExpanded(Sender: TObject; Node: TTreeNode);
    procedure TreeADUCGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure GridADUCAfterDataChange(Sender: TObject);
    procedure GridADUCContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure GridADUCDblClick(Sender: TObject);
    procedure GridADUCDragDrop(Sender: TBaseVirtualTree; Source: TObject;
      DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
      const Pt: TPoint; var Effect: LongWord; Mode: TDropMode);
    procedure GridADUCDragOver(Sender: TBaseVirtualTree; Source: TObject;
      Shift: TShiftState; State: TDragState; const Pt: TPoint; Mode: TDropMode;
      var Effect: LongWord; var Accept: Boolean);
    procedure GridADUCEdited(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure GridADUCEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure GridADUCGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure GridADUCGetText(aSender: TBaseVirtualTree; aNode: PVirtualNode;
      const aCell: TDocVariantData; aColumn: TColumnIndex;
      aTextType: TVSTTextType; var aText: string);
    procedure GridADUCKeyPress(Sender: TObject; var Key: char);
    procedure GridADUCMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    // Define if the module is enabled
    fEnabled: Boolean;
    // Options linked to the module
    fModuleOptions: TModuleADUCOptions;

    // Selection history
    fTreeSelectionHistory: TTreeSelectionHistory;

    // Mormot2 logs
    fLog: TSynLog;
    // Reference to the Core module
    fCore: ICore;

    // Search word for grid
    fSearchWord: RawUtf8;

    fADUCRootNode: TADUCTreeNode;
    fADUCQueryNode: TADUCTreeNode;
    fADUCDomainNode: TADUCTreeNode;

    fUpdating: Integer;

    procedure UpdateTreeImages(ANode: TADUCTreeNode);

    procedure InsertGPLink(ANode: TADUCTreeNode);
    procedure RefreshADUCTreeNode(Node: TADUCTreeNode);
    procedure UpdateGridADUC(ANode: TADUCTreeNode);

    function GetSelectedObjects: TRawUtf8DynArray;
    function GetFocusedObject(OnlyContainer: Boolean = False): RawUtf8;
    function GetFocusedObjectClass: RawUtf8;

    procedure ObserverRsatOptions(Options: TOptions);

    procedure OnLdapClientConnect(LdapClient: TLdapClient);
    procedure OnLdapClientClose(LdapClient: TLdapClient);
  public
    constructor Create(TheOwner: TComponent; ACore: ICore); overload;
    destructor Destroy; override;

    procedure Focus(DistinguishedName: String);

    procedure BeginUpdate;
    procedure EndUpdate;

    property Core: ICore read fCore;
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
  dialogs,
  uvissearch,
  uvisnewobject,
  uOmniselect,
  uvisdelegatecontrol,
  uvischangedn,
  uvistaskresetpassword,
  ucommon,
  ufrmrsatoptions;

{$R *.lfm}

type
  // Enumeration of actions new on ldap
  TNewAction = (
    naComputer,                      // New Computer
    naContact,                       // New Contact
    naGroup,                         // New Group
    naInetOrgPerson,                 // New InetOrgPerson
    naMSMQQueue,                     // New MSMQ Queue
    naMSMQRoutingLink,               // New MSMQ Routing
    naMsAuthzCentralAccessPolicy,    // New MS AuthzCentralAccessPolicy
    naMsAuthzCentralAccessRule,      // New Ms AuthzCentralAccessRule
    naMsDNSServerSettings,           // New Ms-DS ServerSettings
    naMsDSAuthNPolicy,               // New Ms-DS AuthNPolicy
    naMsDSAuthNPolicySilo,           // New Ms-DS AuthNPolicySilo
    naMsDSClaimType,                 // New Ms-DS ClaimType
    naMsDSKeyCredential,             // New Ms-DS KeyCredential
    naMsDSResourceProperty,          // New Ms-DS ResourceProperty
    naMsDSResourcePropertyList,      // New Ms-DS ResourcePropertyList
    naMsDSShadowPrincipal,           // New Ms-DS ShadowPrincipal
    naMsDSShadowPrincipalContainer,  // New Ms-DS ShadowPrincipalContainer
    naMsImagingPSPs,                 // New Ms-DS ImagingPSPs
    naMsImagingPostScanProcess,      // New Ms-DS ImagingPostScanProcess
    naMSMQQueueAlias,                // New MSMQ QueueAlias
    naConnection,                    // New Connection
    naSiteSettings,                  // New Site Settings
    naOrganizationalUnit,            // New Organizational Unit
    naPrinter,                       // New Printer
    naServer,                        // New Server
    naServersContainer,              // New Server Container
    naSite,                          // New Site
    naSiteLink,                      // New Site Link
    naSiteLinkBridge,                // New Site Link Bridge
    naSitesContainer,                // New Sites Container
    naSubnet,                        // New Subnet
    naUser,                          // New User
    naSharedFolder                   // New Shared Folder
  );

function TRawUtf8DynArrayContains(Values: TRawUtf8DynArray; Contain: String): Boolean;
var
  Value: RawUtf8;
begin
  result := True;
  for Value in Values do
    if Value = contain then
      Exit;
  result := False;
end;

{ TFrmModuleADUC }

procedure TFrmModuleADUC.Action_RefreshExecute(Sender: TObject);
var
  c: TCursor;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Execute', [Action_Refresh.Name]);

  c := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    fTreeSelectionHistory.Clear;
    RefreshADUCTreeNode((TreeADUC.Selected as TADUCTreeNode));
  finally
    Screen.Cursor := c;
  end;
end;

procedure TFrmModuleADUC.Action_CopyExecute(Sender: TObject);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Execute', [Action_Copy.Name]);
end;

procedure TFrmModuleADUC.Action_CopyUpdate(Sender: TObject);
begin
  Action_Copy.Enabled := Core.Active;
end;

procedure TFrmModuleADUC.Action_CutExecute(Sender: TObject);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Execute', [Action_Cut.Name]);
end;

procedure TFrmModuleADUC.Action_CutUpdate(Sender: TObject);
begin
  Action_Cut.Enabled := Core.Active;
end;

procedure TFrmModuleADUC.Action_DelegateControlExecute(Sender: TObject);
var
  NodeData: TADUCTreeNodeObject;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Execute', Action_DelegateControl);

  // Fast Exit
  if not Assigned(TreeADUC.Selected) then
  begin
    if Assigned(fLog) then
      fLog.Log(sllWarning, 'No node assigned', Action_DelegateControl);
    Exit;
  end;

  With TVisDelegateControl.Create(Self, fCore) do
  try
    BaseDN := fCore.LdapClient.DefaultDN;
    NodeData := (TreeADUC.Selected as TADUCTreeNode).GetNodeDataObject;
    if not Assigned(NodeData) then
    begin
      if Assigned(fLog) then
        fLog.Log(sllWarning, 'No data assigned to node.', Action_DelegateControl);
      Exit;
    end;
    SelectedObject := NodeData.DistinguishedName;
    if ShowModal <> mrOK then
    begin
      if Assigned(fLog) then
        fLog.Log(sllInfo, 'Canceled', Action_DelegateControl);
      Exit;
    end;
    Execute;
  finally
    Free;
  end;
end;

procedure TFrmModuleADUC.Action_DelegateControlUpdate(Sender: TObject);
begin

end;

procedure TFrmModuleADUC.Action_DeleteExecute(Sender: TObject);
var
  SelectedObject: RawUtf8;
  SelectedObjects: TRawUtf8DynArray;
  SelectedObjectCount: SizeInt;
  MessageResult: TModalResult;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Execute', Action_Delete);

  SelectedObjects := GetSelectedObjects;

  SelectedObjectCount := Length(SelectedObjects);

  // No object
  if (SelectedObjectCount <= 0) then
  begin
    if Assigned(fLog) then
      fLog.Log(sllTrace, 'No object to delete', Action_Delete);
    Exit;
  end;

  MessageResult := mrNone;
  for SelectedObject in SelectedObjects do
  begin
    if MessageResult <> mrYesToAll then
    begin
      MessageResult := MessageDlg(rsDelete, FormatUtf8(rsDeleteConfirm, [SelectedObject]), mtConfirmation, [mbYesToAll, mbYes, mbNo, mbNoToAll, mbCancel], 0);

      // Cancel action
      if (MessageResult = mrNoToAll) or (MessageResult = mrCancel) then
      begin
        if Assigned(fLog) then
          fLog.Log(sllInfo, 'User cancel action', Action_Delete);
        Exit;
      end;
    end;

    // Delete object
    if (MessageResult = mrYesToAll) or (MessageResult = mrYes) then
    begin
      if not Core.LdapClient.Delete(SelectedObject, True) then
      begin
        if Assigned(fLog) then
          fLog.Log(sllError, 'Ldap deletion failed: "%"', [Core.LdapClient.ResultString], Action_Delete);
        Exit;
      end;
    end;
  end;

  // Select parent node
  if TreeADUC.Focused then
    TreeADUC.Selected := TreeADUC.Selected.Parent;

  Action_Refresh.Execute;
end;

procedure TFrmModuleADUC.Action_DeleteUpdate(Sender: TObject);
begin
  Action_delete.Enabled := Core.Active and Assigned(Core.LdapClient) and Core.LdapClient.Connected and ((GridADUC.SelectedCount > 0) or (Assigned(TreeADUC.Selected)));
end;

procedure TFrmModuleADUC.Action_NewAllUpdate(Sender: TObject);
var
  menuItems: Array of TMenuItem;
  newList: Set of TNewAction;
  item, i: Integer;
  filter: UInt64;
  nodeData: PDocVariantData;
  listItem: TNewAction;
  ObjectClass: String;
const
  // Allowed new buttons for objectClass 'domainDNS'
  DomainDNSNew = [
    naComputer,
    naContact,
    naGroup,
    naInetOrgPerson,
    naMsDSShadowPrincipalContainer,
    naMsImagingPSPs,
    naMSMQQueueAlias,
    naOrganizationalUnit,
    naPrinter,
    naUser,
    naSharedFolder
  ];

  // Allowed new buttons for objectClass 'builtInDomain'
  BuiltInDomainNew = [
    naComputer,
    naGroup,
    naInetOrgPerson,
    naUser
  ];

  // Allowed new buttons for objectClass 'container'
  ContainerNew = [
    naComputer,
    naContact,
    naGroup,
    naInetOrgPerson,
    naMsDSKeyCredential,
    naMsDSResourcePropertyList,
    naMsDSShadowPrincipalContainer,
    naMsImagingPSPs,
    naMSMQQueueAlias,
    naPrinter,
    naUser,
    naSharedFolder
  ];

  // Allowed new buttons for objectClass 'organizationalUnit'
  OUNew = ContainerNew;

  // Allowed new buttons for objectClass 'lostAndFound'
  LostAndFoundNew = [
    naComputer,
    naContact,
    naGroup,
    naInetOrgPerson,
    naMSMQQueue,
    naMSMQRoutingLink,
    naMsAuthzCentralAccessPolicy,
    naMsAuthzCentralAccessRule,
    naMsDNSServerSettings,
    naMsDSAuthNPolicy,
    naMsDSAuthNPolicySilo,
    naMsDSClaimType,
    naMsDSKeyCredential,
    naMsDSResourceProperty,
    naMsDSResourcePropertyList,
    naMsDSShadowPrincipal,
    naMsDSShadowPrincipalContainer,
    naMsImagingPSPs,
    naMsImagingPostScanProcess,
    naMSMQQueueAlias,
    naConnection,
    naSiteSettings,
    naOrganizationalUnit,
    naPrinter,
    naServer,
    naServersContainer,
    naSite,
    naSiteLink,
    naSiteLinkBridge,
    naSitesContainer,
    naSubnet,
    naUser,
    nasharedFolder
  ];
begin
  // List of MenuItem_TreeNew that can be enabled as new buttons
  // Must follow the size and order of enum TNewAction
  menuItems := [
    MenuItem_NewComputer,                       // naComputer
    MenuItem_NewContact,                        // naContact
    MenuItem_NewGroup,                          // naGroup
    MenuItem_NewInetOrgPerson,                  // naInetOrgPerson
    MenuItem_NewMSMQQueue,                      // naMSMQQueue
    MenuItem_NewMSMQRoutingLink,                // naMSMQRoutingLink
    MenuItem_NewMsAuthzCentralAccessPolicy,     // naMsAuthzCentralAccessPolicy
    MenuItem_NewMsAuthzCentralAccessRule,       // naMsAuthzCentralAccessRule
    MenuItem_NewMsDNSServerSettings,            // naMsDNSServerSettings
    MenuItem_NewMsDSAuthNPolicy,                // naMsDSAuthNPolicy
    MenuItem_NewMsDSAuthNPolicySilo,            // naMsDSAuthNPolicySilo
    MenuItem_NewMsDSClaimType,                  // naMsDSClaimType
    MenuItem_NewMsDSKeyCredential,              // naMsDSKeyCredential
    MenuItem_NewMsDSResourceProperty,           // naMsDSResourceProperty
    MenuItem_NewMsDSResourcePropertyList,       // naMsDSResourcePropertyList
    MenuItem_NewMsDSShadowPrincipal,            // naMsDSShadowPrincipal
    MenuItem_NewMsDSShadowPrincipalContainer,   // naMsDSShadowPrincipalContainer
    MenuItem_NewMsImagingPSPs,                  // naMsImagingPSPs
    MenuItem_NewMsImagingPostScanProcess,       // naMsImagingPostScanContainer
    MenuItem_NewMSMQQueueAlias,                 // naMSMQQueueAlias
    MenuItem_NewConnection,                     // naConnection
    MenuItem_NewSiteSettings,                   // naSiteSettings
    MenuItem_NewOU,                             // naOrganizationalUnit
    MenuItem_NewPrinter,                        // naPrinter
    MenuItem_NewServer,                         // naServer
    MenuItem_NewServersContainer,               // naServersContainer
    MenuItem_NewSite,                           // naSite
    MenuItem_NewSiteLink,                       // naSiteLink
    MenuItem_NewSiteLinkBridge,                 // naSiteLinkBridge
    MenuItem_NewSiteContainer,                  // naSitesContainer
    MenuItem_NewSubnet,                         // naSubnet
    MenuItem_NewUser,                           // naUser
    MenuItem_NewSharedFolder                    // naSharedFolder
  ];
  filter := 0;
  newList := [];

  ObjectClass := GetFocusedObjectClass;

  case ObjectClass of
    'builtinDomain': newList := BuiltInDomainNew;
    'container': newList := ContainerNew;
    'organizationalUnit': newList := OUNew;
    'lostAndFound': newList := LostAndFoundNew;
    'domainDNS': newList := DomainDNSNew;
    else
      TSynLog.Add.Log(sllWarning, FormatUtf8('"objectClass" not yet implemented: %', [ObjectClass]));
  end;

  // Create filter to allow MenuItem_TreeNew visibility
  for listItem in newList do
    filter := filter or (UInt64(1) shl Ord(listItem));

  // Set MenuItem_TreeNew visibility
  for item := 0 to High(menuItems) do
  begin
    menuItems[item].Visible := ((filter and (UInt64(1) shl item)) > 0);
    menuItems[item].Enabled := Assigned(menuItems[item].Action) and Assigned(menuItems[item].Action.OnExecute);
  end;

  // Hide new MenuItem_TreeNew if no new action allowed
  for i := 0 to MenuItem_New.Count - 1 do
  begin
    if MenuItem_New.Items[i].Visible then
    begin
      Action_NewAll.Visible := True;
      Exit;
    end;
  end;
  Action_NewAll.Visible := False;
end;

procedure TFrmModuleADUC.Action_NewComputerExecute(Sender: TObject);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Execute', [Action_NewComputer.Caption]);

  With TVisNewObject.Create(Self, vnotComputer, GetFocusedObject(True), Core.LdapClient.DefaultDN) do
  begin
    PageCount := 1;
    Ldap := Core.LdapClient;
    if ShowModal = mrOK then
      Action_Refresh.Execute;
  end;
end;

procedure TFrmModuleADUC.Action_NewComputerUpdate(Sender: TObject);
begin
  Action_NewComputer.Enabled := Assigned(Core.LdapClient) and Core.LdapClient.Connected;
end;

procedure TFrmModuleADUC.Action_NewGroupExecute(Sender: TObject);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Execute', [Action_NewGroup.Caption]);

  With TVisNewObject.Create(Self, vnotGroup, GetFocusedObject(True), Core.LdapClient.DefaultDN) do
  begin
    PageCount := 1;
    Ldap := Core.LdapClient;
    if ShowModal = mrOK then
      Action_Refresh.Execute;
  end;
end;

procedure TFrmModuleADUC.Action_NewGroupUpdate(Sender: TObject);
begin
  Action_NewGroup.Enabled := Assigned(Core.LdapClient) and Core.LdapClient.Connected;
end;

procedure TFrmModuleADUC.Action_NewOUExecute(Sender: TObject);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Execute', [Action_NewOU.Caption]);

  With TVisNewObject.Create(Self, vnotOrganizationalUnit, GetFocusedObject(True), Core.LdapClient.DefaultDN()) do
  begin
    PageCount := 1;
    Ldap := Core.LdapClient;
    if ShowModal = mrOK then
      Action_Refresh.Execute;
  end;
end;

procedure TFrmModuleADUC.Action_NewOUUpdate(Sender: TObject);
begin
  Action_NewOU.Enabled := Assigned(Core.LdapClient) and Core.LdapClient.Connected;
end;

procedure TFrmModuleADUC.Action_NewUserExecute(Sender: TObject);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Execute', [Action_NewUser.Caption]);

  With TVisNewObject.Create(Self, vnotUser, GetFocusedObject(True), Core.LdapClient.DefaultDN) do
  begin
    PageCount := 3;
    Ldap := Core.LdapClient;
    if ShowModal = mrOK then
      Action_Refresh.Execute;
  end;
end;

procedure TFrmModuleADUC.Action_NewUserUpdate(Sender: TObject);
begin
  Action_NewUser.Enabled := Assigned(Core.LdapClient) and Core.LdapClient.Connected;
end;

procedure TFrmModuleADUC.Action_NextExecute(Sender: TObject);
var
  Node: TTreeNode;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Execute', [Action_Next.Name]);

  Node := fTreeSelectionHistory.Next;
  if Assigned(Node) then
    Node.Selected := True;
end;

procedure TFrmModuleADUC.Action_NextUpdate(Sender: TObject);
begin

end;

procedure TFrmModuleADUC.Action_ParentExecute(Sender: TObject);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Execute', [Action_Parent.Name]);

  if Assigned(TreeADUC.Selected.Parent) then
    TreeADUC.Selected.Parent.Selected := True;
end;

procedure TFrmModuleADUC.Action_ParentUpdate(Sender: TObject);
begin

end;

procedure TFrmModuleADUC.Action_PasteExecute(Sender: TObject);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Execute', [Action_Paste.Name]);
end;

procedure TFrmModuleADUC.Action_PasteUpdate(Sender: TObject);
begin
  Action_Paste.Enabled := Core.Active;
end;

procedure TFrmModuleADUC.Action_PreviousExecute(Sender: TObject);
var
  Node: TTreeNode;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Execute', [Action_Previous.Name]);

  Node := fTreeSelectionHistory.Previous;
  if Assigned(Node) then
    Node.Selected := True;
end;

procedure TFrmModuleADUC.Action_PreviousUpdate(Sender: TObject);
begin

end;

procedure TFrmModuleADUC.Action_PropertiesExecute(Sender: TObject);
var
  SelectedText, DistinguishedName: String;

  procedure InnerGetSelectedNodeInfo(var SelectedText: String; var DistinguishedName: String);
  var
    NodeData: TADUCTreeNodeObject;
  begin
    if not Assigned(TreeADUC.Selected) then
      Exit;
    NodeData := (TreeADUC.Selected as TADUCTreeNode).GetNodeDataObject;
    if not Assigned(NodeData) then
      Exit;
    SelectedText := NodeData.Find('name').GetReadable();
    DistinguishedName := NodeData.DistinguishedName;
  end;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Execute', [Action_Properties.Name]);

  if GridADUC.Focused then
  begin
    if (GridADUC.SelectedCount <= 0) then
    begin // No selected Row
      InnerGetSelectedNodeInfo(SelectedText, DistinguishedName);
    end
    else
    begin // Missing name or distinguishedName
      if not GridADUC.SelectedObjects[0]^.Exists('name') or
         not GridADUC.SelectedObjects[0]^.Exists('distinguishedName') then
      begin
        Exit;
      end;
      SelectedText := GridADUC.SelectedObjects[0]^.U['name'];
      DistinguishedName := GridADUC.SelectedObjects[0]^.U['distinguishedName'];
    end;
  end
  else if TreeADUC.Focused then
  begin
    InnerGetSelectedNodeInfo(SelectedText, DistinguishedName);
  end
  else
  begin
    Exit;
  end;

  fCore.OpenProperty(SelectedText, DistinguishedName);
end;

procedure TFrmModuleADUC.Action_PropertiesUpdate(Sender: TObject);
begin
  Action_Properties.Enabled := Core.Active and Assigned(Core.LdapClient) and Core.LdapClient.Connected and ((GridADUC.SelectedCount > 0) or (Assigned(TreeADUC.Selected)));
end;

procedure TFrmModuleADUC.Action_RefreshUpdate(Sender: TObject);
begin
  Action_Refresh.Enabled := {Active and} Assigned(Core.LdapClient) and Core.LdapClient.Connected();
end;

procedure TFrmModuleADUC.Action_SearchExecute(Sender: TObject);
var
  NodeData: TADUCTreeNodeObject;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Execute', [Action_Search.Caption]);

  // Fast Exit
  if not Assigned(TreeADUC.Selected) then
    Exit;

  NodeData := (TreeADUC.Selected as TADUCTreeNode).GetNodeDataObject;
  if not Assigned(NodeData) then
    Exit;

  With TVisSearch.Create(Self, Self) do
  begin
    Edit_Path.Caption := NodeData.DistinguishedName;
    ShowModal;
  end;
end;

procedure TFrmModuleADUC.Action_SearchUpdate(Sender: TObject);
begin
  Action_Search.Enabled := Core.Active and Assigned(Core.LdapClient) and Core.LdapClient.Connected;
end;

procedure TFrmModuleADUC.Action_SwitchToolbarCaptionExecute(Sender: TObject);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Execute', [Action_SwitchToolbarCaption.Caption]);

  ToolBar1.ShowCaptions := not ToolBar1.ShowCaptions;
  if ToolBar1.ShowCaptions then
  begin
    ToolBar1.ButtonHeight := 40;
    UnifyButtonsWidth([ToolButton_Previous, ToolButton_Next, ToolButton_Parent]);
    UnifyButtonsWidth([ToolButton_Property, ToolButton_Search, ToolButton_Filter]);
    UnifyButtonsWidth([ToolButton_User, ToolButton_Group, ToolButton_OU]);
    UnifyButtonsWidth([ToolButton_Copy, ToolButton_Cut, ToolButton_Paste, ToolButton_Delete, ToolButton_AddToGroup]);
  end else
  begin
    ToolBar1.ButtonHeight := 24;
    UnifyButtonsWidth([
      ToolButton_Previous,
      ToolButton_Next,
      ToolButton_Parent,
      ToolButton_Property,
      ToolButton_Search,
      ToolButton_Filter,
      ToolButton_User,
      ToolButton_Group,
      ToolButton_OU,
      ToolButton_Copy,
      ToolButton_Cut,
      ToolButton_Paste,
      ToolButton_Delete,
      ToolButton_AddToGroup
    ], 24);
  end;
  ToolBar1.Refresh;
end;

procedure TFrmModuleADUC.Action_TaskAddToAGroupExecute(Sender: TObject);
var
  DistinguishedName, SelectedDistinguishedName: String;
  selectedDistinguishedNameArray: TStringArray;
  LdapObject: TLdapResult;
  AttributeMember: TLdapAttribute;

  procedure ModifyMembers;
  begin
    AttributeMember.Add(DistinguishedName);
    if not Core.LdapClient.Modify(SelectedDistinguishedName, lmoAdd, AttributeMember) then
    begin
      if Assigned(fLog) then
        fLog.Log(sllError, '% - Ldap modify error: "%"', [Action_TaskAddToAGroup.Caption, Core.LdapClient.ResultString]);
      MessageDlg(rsLdapError, Core.LdapClient.ResultString, mtError, [mbOK], 0);
    end;
  end;

begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Execute', [Action_TaskAddToAGroup.Caption]);

  DistinguishedName := GridADUC.FocusedRow^.S['distinguishedName'];

  With TVisOmniselect.Create(Self, Core.LdapClient, ['group']) do
  try
    Caption := rsTitleSelectGroups;
    if (ShowModal <> mrOK) then
      Exit;
    selectedDistinguishedNameArray := SelectedObjects;
  finally
    Free;
  end;

  for SelectedDistinguishedName in selectedDistinguishedNameArray do
  begin
    LdapObject := Core.LdapClient.SearchObject(SelectedDistinguishedName, '', ['member']);
    if not Assigned(LdapObject) then
    begin
      if Assigned(fLog) then
        fLog.Log(sllError, '% - Ldap search error: "%"', [Action_TaskAddToAGroup.Caption, Core.LdapClient.ResultString]);
      MessageDlg(rsLdapError, FormatUtf8(rsLdapSearchFailed, [Core.LdapClient.ResultString]), mtError, [mbOK], 0);
      Exit;
    end;

    AttributeMember := LdapObject.Attributes.Find('member');
    if not Assigned(AttributeMember) then
    begin
      AttributeMember := TLdapAttribute.Create('member', atMember);
      try
        ModifyMembers;
      finally
        FreeAndNil(AttributeMember);
      end;
    end
    else
    begin
      ModifyMembers;
    end;
  end;
end;

procedure TFrmModuleADUC.Action_TaskAddToAGroupUpdate(Sender: TObject);
begin
  Action_TaskAddToAGroup.Enabled := Assigned(GridADUC.FocusedRow) and GridADUC.FocusedRow^.Exists('distinguishedName');
end;

procedure TFrmModuleADUC.Action_TaskMoveExecute(Sender: TObject);
var
  DistinguishedName: String;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Execute', [Action_TaskMove.Caption]);

  DistinguishedName := GridADUC.FocusedRow^.U['distinguishedName'];
  With TVisChangeDN.Create(Self, fCore.LdapClient, DistinguishedName, fCore.LdapClient.DefaultDN) do
  try
    if (mrOK <> ShowModal) then
    begin
      if Assigned(fLog) then
        fLog.Log(sllInfo, '% - Task canceled.');
      Exit;
    end;
    if not fCore.LdapClient.MoveLdapEntry(DistinguishedName, Join([DistinguishedName.Split(',')[0], ',', SelectedDN])) then
    begin
      if Assigned(fLog) then
        fLog.Log(sllError, '% - Failed to move ldap entry "%" to "%": %', [DistinguishedName, SelectedDN, fCore.LdapClient.ResultString]);
      Exit;
    end;
  finally
    Free;
  end;
end;

procedure TFrmModuleADUC.Action_TaskMoveUpdate(Sender: TObject);
begin
  Action_TaskMove.Enabled := Assigned(GridADUC.FocusedRow) and GridADUC.FocusedRow^.Exists('distinguishedName');
end;

procedure TFrmModuleADUC.Action_TaskResetPasswordExecute(Sender: TObject);
var
  userDN: String;
  userData: TLdapResult;
  res, attr: TLdapAttribute;
  userAccountControl: LongInt;
  accountIsLocked: Boolean;
  ObjectClassArr: TRawUtf8DynArray;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Execute', [Action_TaskResetPassword.Caption]);

  With TVisTaskResetPassword.Create(self) do
  try
    // Query object to modify
    userDN := GridADUC.FocusedRow^.U['distinguishedName'];
    userData := fCore.LdapClient.SearchObject(userDN, '', ['userAccountControl', 'objectClass']);
    if not Assigned(userData) then
    begin
      if Assigned(fLog) then
        fLog.Log(sllError, '% - Cannot retrieve information about "%". (%)', [Action_TaskResetPassword.Caption, userDN, fCore.LdapClient.ResultString]);
      Exit;
    end;
    res := userData.Attributes.Find('objectClass');
    if not Assigned(res) then
    begin
      if Assigned(fLog) then
        fLog.Log(sllError, '% - Cannot retrieve objectClass about "%".', [Action_TaskResetPassword.Caption, userDN]);
      Exit;
    end;
    ObjectClassArr := res.GetAllReadable;
    if (ObjectClassArr[High(ObjectClassArr)] = 'computer') then
      if MessageDlg('Dangererous action', 'Be carefull, this action is dangerous. Do you want to perform a reset password on a computer ?', mtWarning, mbYesNoCancel, 0) <> mrYes then
      begin
        if Assigned(fLog) then
          fLog.Log(sllInfo, '% - User cancel', [Action_TaskResetPassword.Caption]);
        Exit;
      end;
    res := userData.Attributes.Find('userAccountControl');
    userAccountControl := StrToInt(res.GetRaw());
    if (userAccountControl < 0) then
    begin
      if Assigned(fLog) then
        fLog.Log(sllError, '% - Invalid userAccountControl(%)', [Action_TaskResetPassword.Caption, IntToStr(userAccountControl)]);
      Exit;
    end;
    accountIsLocked := ((userAccountControl and 16 {uacLockedOut}) > 0);
    if accountIsLocked then
      Label_Status.Caption := Label_Status.Caption + 'Locked'
    else
      Label_Status.Caption := Label_Status.Caption + 'Unlocked';
    // Check password conformity
    repeat
      if (ShowModal <> mrOk) then
        Exit;
      if (Edit_NewPassword.Text <> Edit_ConfirmPassword.Text) then
        ShowMessage(rsNewConfirmPassDifferent);
    until (Edit_NewPassword.Text = Edit_ConfirmPassword.Text);
    // modify userAccountControl
    if accountIsLocked and CheckBox_Unlock.Checked then
    begin
      attr := TLdapAttribute.Create('userAccountControl', atUserAccountControl);
      try
        attr.Add(IntToString(userAccountControl or 16 {uacLockedOut}));
        if not fCore.LdapClient.Modify(userDN, lmoReplace, attr) then
        begin
          if Assigned(fLog) then
            fLog.Log(sllError, '% - Ldap Modify Error: %', [Action_TaskResetPassword.Caption, fCore.LdapClient.ResultString]);
          MessageDlg(rsLdapError, FormatUtf8(rsLdapModifyFailed, [fCore.LdapClient.ResultString]), mtError, [mbOK], 0);
        end;
      finally
        FreeAndNil(attr);
      end;

    end;
    attr := TLdapAttribute.Create('unicodePwd', atUndefined);
    try
      attr.add(LdapUnicodePwd(Edit_NewPassword.Text));
      if not fCore.LdapClient.Modify(userDN, lmoReplace, attr) then
      begin
        if Assigned(fLog) then
          fLog.Log(sllError, '% - Ldap Modify Error: %', [Action_TaskResetPassword.Caption, fCore.LdapClient.ResultString]);
        Dialogs.MessageDlg(rsLdapError, FormatUtf8(rsLdapModifyFailed, [fCore.LdapClient.ResultString]), mtError, [mbOK], 0);
      end;
    finally
      FreeAndNil(attr);
    end;


    // Set reset password
    if CheckBox_Change.Checked then
    begin
      attr := TLdapAttribute.Create('pwdLastSet', atPwdLastSet);
      try
        attr.Add(IntToString(0));
        if not fCore.LdapClient.Modify(userDN, lmoReplace, attr) then
        begin
          if Assigned(fLog) then
            fLog.Log(sllError, '% - Ldap Modify Error: %', [Action_TaskResetPassword.Caption, fCore.LdapClient.ResultString]);
          Dialogs.MessageDlg(rsLdapError, FormatUtf8(rsLdapModifyFailed, [fcore.LdapClient.ResultString]), mtError, [mbOK], 0);
        end;
      finally
        FreeAndNil(attr);
      end;
    end;
  finally
    Free;
  end;
end;

procedure TFrmModuleADUC.Action_TaskResetPasswordUpdate(Sender: TObject);
begin
  Action_TaskResetPassword.Enabled := Assigned(GridADUC.FocusedRow) and (GridADUC.FocusedRow^.Exists('distinguishedName'));
end;

procedure TFrmModuleADUC.GridADUCKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (key = 113) and (GridADUC.FocusedColumnObject.PropertyName = 'name') then // F2
  begin
    GridADUC.TreeOptions.MiscOptions := GridADUC.TreeOptions.MiscOptions - [toReadOnly] + [toEditable];
    GridADUC.EditNode(GridADUC.FocusedNode, GridADUC.FocusedColumn);
    GridADUC.TreeOptions.MiscOptions := GridADUC.TreeOptions.MiscOptions + [toReadOnly] - [toEditable];
  end;
end;

procedure TFrmModuleADUC.Timer_SearchInGridTimer(Sender: TObject);
begin
  Timer_SearchInGrid.Enabled := False;
end;

procedure TFrmModuleADUC.Timer_TreeChangeNodeTimer(Sender: TObject);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Tree Change Timer', Self);

  Timer_TreeChangeNode.Enabled := False;

  RefreshADUCTreeNode((TreeADUC.Selected as TADUCTreeNode));
  UpdateTreeImages((TreeADUC.Selected as TADUCTreeNode));
  UpdateGridADUC((TreeADUC.Selected as TADUCTreeNode));
end;

procedure TFrmModuleADUC.TreeADUCChange(Sender: TObject; Node: TTreeNode);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Tree Change', Self);

  if Timer_TreeChangeNode.Enabled then
    Timer_TreeChangeNode.Enabled := False;
  Timer_TreeChangeNode.Enabled := True;
end;

procedure TFrmModuleADUC.TreeADUCContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
var
  VisibleItems: Array of TMenuItem;
  Item: TMenuItem;

  function Contains(Items: Array of TMenuItem; Item: TMenuItem): Boolean;
  var
    i: Integer;
  begin
    result := True;
    for i := 0 to High(Items) do
      if Items[i] = Item then
        Exit;
    result := False;
  end;

begin
  VisibleItems := [
    MenuItem_DelegateControl,
    MenuItem_ChangePartition,
    MenuItem_New,
    MenuItem_SwitchToolbarSize,
    MenuItem_Search,
    MenuItem_Delete,
    MenuItem_Refresh,
    MenuItem_Properties
  ];

  Handled := not Assigned(Core.LdapClient) or not Core.LdapClient.Connected;

  for Item in PopupMenu1.Items do
    Item.Visible := Contains(VisibleItems, Item);
end;

procedure TFrmModuleADUC.TreeADUCCreateNodeClass(Sender: TCustomTreeView;
  var NodeClass: TTreeNodeClass);
begin
  NodeClass := TADUCTreeNode;
end;

procedure TFrmModuleADUC.TreeADUCDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  TreeNode: TTreeNode;
  Node: PVirtualNode;
  BaseDN, oldDN, newDN: String;
  oldDNS, newDNS: Array of String;
  NodeData: TADUCTreeNodeObject;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - DragDrop', [Self.Name]);
  NewDNS := [];
  OldDNS := [];
  TreeNode := TreeADUC.GetNodeAt(X, Y);
  if not Assigned(TreeNode) then
  begin
    if Assigned(fLog) then
      fLog.Log(sllWarning, '% - No node at (%,%)', [Self.Name, IntToStr(X), IntToStr(Y)]);
    Exit;
  end;
  NodeData := (TreeNode as TADUCTreeNode).GetNodeDataObject;
  if not Assigned(NodeData) then
  begin
    Exit;
  end;
  BaseDN := NodeData.DistinguishedName;

  Node := GridADUC.GetFirstSelected();
  if not Assigned(Node) then
  begin
    if Assigned(fLog) then
      fLog.Log(sllWarning, '% - No node selected in %', [Self.Name, GridADUC.Name]);
    Exit;
  end;
  repeat
    oldDN := GridADUC.GetNodeAsPDocVariantData(Node)^.U['distinguishedName'];
    newDN := String.Join(',', [oldDN.Split(',')[0], BaseDN]);
    Insert(oldDN, oldDNS, Length(oldDNS));
    Insert(newDN, newDNS, Length(newDNS));
    Node := GridADUC.GetNextSelected(Node);
  until Node = nil;
  Core.LdapClient.MoveLdapEntries(oldDNS, newDNS);
  if Assigned(fLog) then
    fLog.Log(sllInfo, '% - % entries moved.', [Self.Name, IntToStr(Length(OldDN))]);
  Action_Refresh.Execute;
end;

procedure TFrmModuleADUC.TreeADUCDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  Node: TTreeNode;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - DragOver', [Self.Name]);
  Accept := False;
  if Source <> GridADUC then
  begin
    if Assigned(fLog) then
      fLog.Log(sllWarning, '% - Invalid source (%).', [Self.Name, Source.ClassName]);
    Exit;
  end;
  TreeADUC.HotTrack := True;
  Node := TreeADUC.GetNodeAt(x, y);
  if not Assigned(Node) then
  begin
    if Assigned(fLog) then
      fLog.Log(sllWarning, '% - No node at (%,%)', [Self.Name, IntToStr(X), IntToStr(Y)]);
    Exit;
  end;
  Node.Expand(False);
  Accept := True;
end;


procedure TFrmModuleADUC.TreeADUCEndDrag(Sender, Target: TObject; X,
  Y: Integer);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - EndDrag', [Self.Name]);

  TreeADUC.HotTrack := False;
end;

procedure TFrmModuleADUC.TreeADUCExpanded(Sender: TObject; Node: TTreeNode);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Expanded', [Self.Name]);

  RefreshADUCTreeNode((Node as TADUCTreeNode));
  if not (Node.Count > 0) then
  begin
    Node.Collapse(False);
    Node.HasChildren := False;
  end;
end;

procedure TFrmModuleADUC.TreeADUCGetImageIndex(Sender: TObject;
  Node: TTreeNode);
var
  NodeData: TADUCTreeNodeObject;
begin
  // Fast Exit
  if not Assigned(Node) or (Node.ImageIndex <> -1) then
    Exit;

  NodeData := (Node as TADUCTreeNode).GetNodeDataObject;
  if not Assigned(NodeData) then
    Exit;

  Node.ImageIndex := CoreDataModule.objectClassToImageIndex(NodeData.LastObjectClass);
  Node.SelectedIndex := Node.ImageIndex;
end;

procedure TFrmModuleADUC.GridADUCAfterDataChange(Sender: TObject);
begin
  GridADUC.FindColumnByPropertyName('name').ReadOnly := True;
end;

procedure TFrmModuleADUC.GridADUCContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
var
  VisibleItems: Array of TMenuItem;
  Item: TMenuItem;

  function Contains(Items: Array of TMenuItem; Item: TMenuItem): Boolean;
  var
    i: Integer;
  begin
    result := True;
    for i := 0 to High(Items) do
      if Items[i] = Item then
        Exit;
    result := False;
  end;

begin
  VisibleItems := [
    MenuItem_DelegateControl,
    MenuItem_,
    MenuItem_AddToAGroup,
    MenuItem_NameMapping,
    MenuItem_DisableAccount,
    MenuItem_ResetPassword,
    MenuItem_Move,
    MenuItem_OpenHomePage,
    MenuItem_SendMail,
    MenuItem_Manage,
    MenuItem_Find,
    MenuItem_New,
    MenuItem_Copy,
    MenuItem_Cut,
    MenuItem_Paste,
    MenuItem_SwitchToolbarSize,
    MenuItem_Search,
    MenuItem_Delete,
    MenuItem_Refresh,
    MenuItem_Properties
  ];
  Handled := not Assigned(Core.LdapClient) or not Core.LdapClient.Connected;
  for Item in PopupMenu1.Items do
    Item.Visible := Contains(VisibleItems, Item);
end;

procedure TFrmModuleADUC.GridADUCDblClick(Sender: TObject);
var
  Data: PDocVariantData;
  i: Integer;
  NodeData: TADUCTreeNodeObject;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - DoubleClick', [Self.Name]);

  if not Assigned(GridADUC.FocusedNode) then
  begin
    if Assigned(fLog) then
      fLog.Log(sllWarning, '% - No focused node.', [Self.Name]);
    Exit;
  end;

  Data := GridADUC.GetNodeAsPDocVariantData(GridADUC.FocusedNode);
  if not Assigned(Data) then
  begin
    if Assigned(fLog) then
      fLog.Log(sllWarning, '% - No data in selected node (#%)', [Self.Name, IntToStr(GridADUC.FocusedNode^.Index)]);
    Exit;
  end;

  if not Assigned(TreeADUC.Selected) then
    Exit;

  for i := 0 to TreeADUC.Selected.Count - 1 do
  begin
    if not Assigned(TreeADUC.Selected.Items[i]) then
      continue;

    NodeData := (TreeADUC.Selected.Items[i] as TADUCTreeNode).GetNodeDataObject;
    if not Assigned(TreeADUC.Selected.Items[i]) then
      continue;

    if NodeData.DistinguishedName = Data^.U['distinguishedName'] then
    begin
      if Assigned(fLog) then
        fLog.Log(sllInfo, '% - Found object in % (%)', [Self.Name, TreeADUC.Name, TreeADUC.Selected.Items[i].Text]);
      TreeADUC.Select(TreeADUC.Selected.Items[i]);
      Exit;
    end;
  end;

  Action_Properties.Execute;
end;

procedure TFrmModuleADUC.GridADUCDragDrop(Sender: TBaseVirtualTree;
  Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
  Shift: TShiftState; const Pt: TPoint; var Effect: LongWord; Mode: TDropMode);
var
  HitInfo: THitInfo;
  Data: PDocVariantData;
  BaseDN, oldDN, newDN: String;
  Node: PVirtualNode;
  newDNS, oldDNS: Array of String;
begin
  oldDNS := [];
  newDNS := [];

  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - DragDrop', [Self.Name]);

  GridADUC.GetHitTestInfoAt(Pt.x, Pt.y, True, HitInfo);

  Data := GridADUC.GetNodeAsPDocVariantData(HitInfo.HitNode);
  BaseDN := Data^.U['distinguishedName'];

  Node := GridADUC.GetFirstSelected();
  if not Assigned(Node) then
  begin
    if Assigned(fLog) then
      fLog.Log(sllWarning, '% - No selected node.', [Self.Name]);
    Exit;
  end;

  repeat
    Data := GridADUC.GetNodeAsPDocVariantData(Node);
    oldDN := data^.U['distinguishedName'];
    newDN := String.Join(',', [oldDN.split(',')[0], BaseDN]);
    Insert(newDN, newDNS, Length(newDNS));
    Insert(oldDN, oldDNS, Length(oldDNS));
    Node := GridADUC.GetNextSelected(Node);
  until Node = nil;

  Core.LdapClient.MoveLdapEntries(oldDNS, newDNS);
  if Assigned(fLog) then
    fLog.Log(sllInfo, '% - % entries moved.', [Self.Name, IntToStr(Length(oldDNS))]);
  Action_Refresh.Execute;
end;

procedure TFrmModuleADUC.GridADUCDragOver(Sender: TBaseVirtualTree;
  Source: TObject; Shift: TShiftState; State: TDragState; const Pt: TPoint;
  Mode: TDropMode; var Effect: LongWord; var Accept: Boolean);
var
  HitInfo: THitInfo;
  Data: PDocVariantData;
begin
  Accept := False;
  if (Source <> GridADUC) then
    Exit;

  GridADUC.TreeOptions.PaintOptions := GridADUC.TreeOptions.PaintOptions + [toHotTrack];
  HitInfo.HitNode := nil;
  GridADUC.GetHitTestInfoAt(Pt.x, Pt.y, True, HitInfo);
  if not Assigned(HitInfo.HitNode) or (hiNowhere in HitInfo.HitPositions) then
    Exit;
  Data := GridADUC.GetNodeAsPDocVariantData(HitInfo.HitNode);
  if not Assigned(data) then
    Exit;
  if TRawUtf8DynArrayContains(Data^.A['objectClass']^.ToRawUtf8DynArray, 'organizationalUnit') then
    Accept := HitInfo.HitNode <> GridADUC.FocusedNode;
end;

procedure TFrmModuleADUC.GridADUCEdited(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  DistinguishedName: String;
  DistinguishedNameParsed: TNameValueDNs;
  i: Integer;
  newRDN: RawUtf8;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Edited', [Self.Name]);

  if not Assigned(Node) then
  begin
    if Assigned(fLog) then
      fLog.Log(sllWarning, '% - No node assigned.', [Self.Name]);
    Exit;
  end;

  DistinguishedName := GridADUC.GetNodeAsPDocVariantData(Node)^.U['distinguishedName'];
  ParseDN(DistinguishedName, DistinguishedNameParsed);
  newRDN := GridADUC.GetNodeAsPDocVariantData(Node)^.U['name'];
  if newRDN = DistinguishedNameParsed[0].Value then
  begin
    if Assigned(fLog) then
      fLog.Log(sllWarning, '% - Name "%" haven''t changed.', [Self.Name, newRDN]);
    Exit;
  end;

  if not LdapEscapeName(newRDN, newRDN) then
  begin
    if Assigned(fLog) then
      fLog.Log(sllWarning, '% - Name "%" is invalid.', [Self.Name, newRDN]);
    Exit;
  end;
  newRDN := Join([DistinguishedNameParsed[0].Name + '=' + newRDN]);
  Core.LdapClient.RenameLdapEntry(DistinguishedName, newRDN);

  DistinguishedName := newRDN;
  for i := 1 to High(DistinguishedNameParsed) do
    DistinguishedName := Format('%s,%s=%s', [DistinguishedName, DistinguishedNameParsed[i].Name, DistinguishedNameParsed[i].Value]);
end;

procedure TFrmModuleADUC.GridADUCEndDrag(Sender, Target: TObject; X, Y: Integer
  );
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - EndDrag', [Self.Name]);
  GridADUC.TreeOptions.PaintOptions := GridADUC.TreeOptions.PaintOptions - [toHotTrack];
  TreeADUC.HotTrack := False;
end;

procedure TFrmModuleADUC.GridADUCGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
begin
  if GridADUC.FindColumnByIndex(Column).PropertyName <> 'name' then
    Exit;
  try
    ImageIndex := CoreDataModule.objectClassToImageIndex(GridADUC.GetNodeAsPDocVariantData(Node)^.S['objectClass']);
  except
    ImageIndex := Ord(ileADUnknown);
  end;
end;

procedure TFrmModuleADUC.GridADUCGetText(aSender: TBaseVirtualTree;
  aNode: PVirtualNode; const aCell: TDocVariantData; aColumn: TColumnIndex;
  aTextType: TVSTTextType; var aText: string);
var
  row: PDocVariantData;
  PropertyName: RawUtf8;
  PropertyValues: TRawUtf8DynArray;
begin
  row := GridADUC.GetNodeAsPDocVariantData(aNode);
  if not Assigned(Row) then
    Exit;
  PropertyName := GridADUC.FindColumnByIndex(aColumn).PropertyName;
  if (PropertyName = '') then
    Exit;
  PropertyValues := Row^.A[PropertyName]^.ToRawUtf8DynArray;
  if not Assigned(PropertyValues) or (Length(PropertyValues) <= 0) then
    Exit;
  aText := PropertyValues[High(PropertyValues)];
end;

procedure TFrmModuleADUC.GridADUCKeyPress(Sender: TObject; var Key: char);
begin
  SearchInGrid(Timer_SearchInGrid, GridADUC, fSearchWord, Key);
end;

procedure TFrmModuleADUC.GridADUCMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not Assigned(GridADUC.GetNodeAt(X, Y)) then
  begin
    GridADUC.ClearSelection;
    GridADUC.FocusedNode := nil;
    Exit;
  end;
  if (GridADUC.SelectedCount <= 0) then
    Exit;
  if Button = mbLeft then
    GridADUC.BeginDrag(False);
end;

procedure TFrmModuleADUC.UpdateTreeImages(ANode: TADUCTreeNode);
var
  i: Integer;
  NodeData: TADUCTreeNodeObject;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - UpdateTreeImages', [Self.Name]);

  if not Assigned(ANode) then
  begin
    if Assigned(fLog) then
      fLog.Log(sllWarning, '% - Node not assigned.', [Self.Name]);
    Exit;
  end;

  NodeData := ANode.GetNodeDataObject;
  if not Assigned(NodeData) then
  begin
    if Assigned(fLog) then
      fLog.Log(sllWarning, 'No node data');
    Exit;
  end;

  TreeADUC.BeginUpdate;
  try
    ANode.ImageIndex := CoreDataModule.objectClassToImageIndex(NodeData.LastObjectClass);
    ANode.SelectedIndex := ANode.ImageIndex;
    for i := 0 to ANode.Count - 1 do
    begin
      if not Assigned(ANode.Items[i]) then
        continue;

      NodeData := (ANode.Items[i] as TADUCTreeNode).GetNodeDataObject;
      if not Assigned(NodeData) then
        continue;

      ANode.Items[i].ImageIndex := CoreDataModule.objectClassToImageIndex(NodeData.LastObjectClass);
      ANode.Items[i].SelectedIndex := ANode.Items[i].ImageIndex;
    end;
  finally
    TreeADUC.EndUpdate;
  end;
  if Assigned(fLog) then
    fLog.Log(sllInfo, '% - Node "%" and subnodes (%) have been updated.', [Self.Name, ANode.Text, IntToStr(ANode.Count)]);
end;

procedure TFrmModuleADUC.InsertGPLink(ANode: TADUCTreeNode);
begin
  if not Assigned(ANode) or not Assigned(ANode.GetNodeDataObject) or not Assigned(ANode.GetNodeDataObject.GPLinks) then
    Exit;
end;

procedure TFrmModuleADUC.RefreshADUCTreeNode(Node: TADUCTreeNode);
var
  BackupItems: TDocVariantData;
  Ldap: TRsatLdapClient;
  SearchResult: TLdapResult;
  NodeDN: RawUtf8;
  RefreshNode: TADUCTreeNode;
  NodeData, ItemNodeData: TADUCTreeNodeObject;
  i: Integer;
  Obj: TLdapResult;

  function BuildFilter: RawUtf8;
  var
    i: Integer;
  begin
    result := '';
    if (Length(fModuleOptions.TreeObjectClasses) < 1) then
      Exit;

    result := '(&';
    if not fCore.RsatOptions.AdvancedView then
      result := FormatUtf8('%(|(!(showInAdvancedViewOnly=*))(showInAdvancedViewOnly=FALSE))', [result]);
    result := FormatUtf8('%%(|', [result, fModuleOptions.TreeFilter]);
    for i := 0 to High(fModuleOptions.TreeObjectClasses) do
      result := FormatUtf8('%(objectClass=%)', [result, fModuleOptions.TreeObjectClasses[i]]);
    result := FormatUtf8('%))', [result]);
  end;

begin
  // Fast Exit
  if not Assigned(fCore) or not Assigned(fCore.LdapClient) or (fUpdating > 0) then
    Exit;

  // Fallback to domainNode
  if not Assigned(Node) then
    Node := fADUCDomainNode;

  // Get Ldap instance
  Ldap := fCore.LdapClient;

  // Retrieve domainNode
  if not Assigned(Node) then
  begin
    Obj := Ldap.SearchObject(Ldap.DefaultDN(), '', ['distinguishedName', 'objectClass', 'gPLink', 'name']);
    if not Assigned(Obj) then
    begin
      if Assigned(fLog) then
        fLog.Log(sllError, 'Ldap search object failed: "%"', [Ldap.ResultString], Self);
      Exit;
    end;

    TreeADUC.BeginUpdate;
    try
      Node := (TreeADUC.Items.Add(nil, DNToCN(Obj.Find('distinguishedName').GetReadable())) as TADUCTreeNode);
      Node.NodeType := atntObject;
      Node.GetNodeDataObject.DistinguishedName := Obj.Find('distinguishedName').GetReadable();
      Node.GetNodeDataObject.ObjectClass := Obj.Find('objectClass').GetAllReadable;
      Node.GetNodeDataObject.GPLink := Obj.Find('gPLink').GetReadable();
      if not Assigned(fADUCDomainNode) then
      begin
        Node.Selected := True;
        fADUCDomainNode := Node;
        Exit;
      end;
    finally
      TreeADUC.EndUpdate;
    end;
  end;

  NodeData := Node.GetNodeDataObject;
  if not Assigned(NodeData) then
    Exit;

  // Backup Items node
  BackupItems.Init();
  for i := 0 to Node.Count - 1 do
  begin
    if not Assigned(Node.Items[i]) then
      continue;

    ItemNodeData := (Node.Items[i] as TADUCTreeNode).GetNodeDataObject;
    if not Assigned(ItemNodeData) then
      continue;

    BackupItems.I[ItemNodeData.DistinguishedName] := i;
  end;

  // Retrieve object from AD
  Ldap.SearchBegin(fModuleOptions.SearchPageSize);
  TreeADUC.BeginUpdate;
  try
    Ldap.SearchScope := lssSingleLevel;
    repeat
      if not Ldap.Search(NodeData.DistinguishedName, False, BuildFilter, ['distinguishedName', 'objectClass', 'gPLink', 'name']) then
      begin
        if Assigned(fLog) then
          fLog.Log(sllError, 'Fail to refresh ADUCTreeNode(%): %', [Node.Text, Ldap.ResultString], Self);
        Exit;
      end;

      for SearchResult in Ldap.SearchResult.Items do
      begin
        // Empty Result item
        if not Assigned(SearchResult) then
          continue;

        // No name for Result item
        NodeDN := SearchResult.Find('distinguishedName').GetReadable();
        if NodeDN = '' then
          continue;

        if BackupItems.Exists(NodeDN) then
        begin // Found in backup
          RefreshNode := (Node.Items[BackupItems.I[NodeDN]] as TADUCTreeNode);
          BackupItems.Delete(NodeDN);
        end
        else
        begin // New node
          RefreshNode := (TreeADUC.Items.AddChild(Node, '') as TADUCTreeNode);
          RefreshNode.NodeType := atntObject;
          RefreshNode.HasChildren := True;
        end;

        ItemNodeData := (RefreshNode as TADUCTreeNode).GetNodeDataObject;
        if not Assigned(ItemNodeData) then
          continue;
        ItemNodeData.DistinguishedName := NodeDN;
        ItemNodeData.Name := SearchResult.Find('name').GetReadable();
        RefreshNode.Text := ItemNodeData.Name;
        ItemNodeData.ObjectClass := SearchResult.Find('objectClass').GetAllReadable;
        ItemNodeData.GPLink := SearchResult.Find('gPLink').GetReadable();
      end;
    until Ldap.SearchCookie = '';
  finally
    TreeADUC.EndUpdate;
    Ldap.SearchEnd;
  end;

  // Remove non-existing nodes
  TreeADUC.BeginUpdate;
  try
    i := Node.Count;
    while i > 0 do
    begin
      Dec(i);
      if not Assigned(Node.Items[i]) then
        continue;

      ItemNodeData := (Node.Items[i] as TADUCTreeNode).GetNodeDataObject;
      if not Assigned(ItemNodeData) then
        continue;

      if BackupItems.Exists(ItemNodeData.DistinguishedName) then
        TreeADUC.Items.Delete(Node.Items[i])
    end;
  finally
    TreeADUC.EndUpdate;
  end;

  if (Node = fADUCDomainNode) then
  begin
    BeginUpdate;
    try
      Node.Expand(False);
    finally
      EndUpdate;
    end;
  end;

  // Sort nodes
  Node.AlphaSort;
  if fModuleOptions.ShowGPO then
    InsertGPLink(Node);
end;

procedure TFrmModuleADUC.UpdateGridADUC(ANode: TADUCTreeNode);
var
  DocResult, Data, SelectedRows: TDocVariantData;
  RowData, Row, GridRow: PDocVariantData;
  RowName: RawUtf8;
  GridNode: PVirtualNode;
  DistinguishedName, FocusedRowDN, Filter: String;
  PropertyValues: TRawUtf8DynArray;
  NodeData: TADUCTreeNodeObject;
begin
  if not Assigned(ANode) then
    ANode := (TreeADUC.Selected as TADUCTreeNode);
  if not Assigned(ANode) then
  begin
    if Assigned(fLog) then
      fLog.Log(sllError, '% - No node assigned.', [GridADUC.Name]);
    Exit;
  end;

  NodeData := ANode.GetNodeDataObject;
  if not Assigned(NodeData) then
  begin
    if Assigned(fLog) then
      fLog.Log(sllError, 'No node data', Self);
    Exit;
  end;

  DistinguishedName := NodeData.DistinguishedName;

  SelectedRows := GridADUC.SelectedRows;
  FocusedRowDN := '';
  if Assigned(GridADUC.FocusedRow) then
    FocusedRowDN := GridADUC.FocusedRow^.U['distinguishedName'];

  GridADUC.Clear;
  Data.init();
  DocResult.init();

  Core.LdapClient.SearchBegin(fModuleOptions.SearchPageSize);
  GridADUC.BeginUpdate;
  try
    Core.LdapClient.SearchPagingBegin(fModuleOptions.SearchPageNumber);
    Core.LdapClient.SearchScope := lssSingleLevel;
    Filter := '(&';
    if not Core.RsatOptions.AdvancedView then
      Filter := FormatUtf8('%(|(!(showInAdvancedViewOnly=*))(showInAdvancedViewOnly=FALSE))', [Filter]);
    Filter := FormatUtf8('%%)', [Filter, fModuleOptions.GridFilter]);
    if not Core.LdapClient.SearchAllDocPaged(@DocResult, DistinguishedName, False, Filter, ['distinguishedName', 'objectClass', 'name', 'description']) then
    begin
      if Assigned(fLog) then
        fLog.Log(sllError, '% - Ldap search error: "%"', [GridADUC.Name, Core.LdapClient.ResultString]);
      Exit;
    end;

    for RowData in DocResult.Objects do
    begin
      for RowName in RowData^.Names do
      begin
        if RowName = '' then
          continue;
        PropertyValues := RowData^.A[RowName]^.ToRawUtf8DynArray;
        if Assigned(PropertyValues) then
          Data.AddOrUpdateValue(RowName, PropertyValues[High(PropertyValues)])
        else
          Data.AddOrUpdateValue(RowName, RowData^.S[RowName]);
      end;
      GridADUC.Data.AddItem(Data);
      Data.Clear;
    end;
    GridADUC.LoadData();
    GridADUC.CreateColumnsFromData(True, True);
  finally
    Core.LdapClient.SearchPagingEnd;
    Core.LdapClient.SearchEnd;
    GridADUC.EndUpdate;
  end;
  GridADUC.ClearSelection;
  GridADUC.FocusedNode := nil;
  GridNode := GridADUC.GetFirst();
  while Assigned(GridNode) do
  begin
    GridRow := GridADUC.GetNodeAsPDocVariantData(GridNode);

    for Row in SelectedRows.Objects do
    begin
      if (Row^.S['distinguishedName'] = GridRow^.S['distinguishedName']) then
        GridADUC.AddToSelection(GridNode);
    end;
    if (FocusedRowDN <> '') and (GridRow^.S['distinguishedName'] = FocusedRowDN) then
      GridADUC.FocusedNode := GridNode;

    GridNode := GridADUC.GetNext(GridNode);
  end;
  GridADUC.Refresh;
end;

function TFrmModuleADUC.GetSelectedObjects: TRawUtf8DynArray;
var
  SelectedRows: TDocVariantData;
  Row: PDocVariantData;
  NodeData: TADUCTreeNodeObject;

  function GetSelectedObjectsInGrid: TRawUtf8DynArray;
  begin
    if Assigned(fLog) then
      fLog.Log(sllTrace, 'Focus on Grid', Self);

    if (GridADUC.SelectedCount <= 0) then
    begin
      if Assigned(fLog) then
        fLog.Log(sllTrace, 'No selected item', Self);
      Exit;
    end;

    SelectedRows := GridADUC.SelectedRows;
    SetLength(result, SelectedRows.Count);

    for Row in SelectedRows.Objects do
    begin
      // Invalid row
      if not Assigned(Row) then
        continue;

      if Row^.Exists('distinguishedName') then
        Insert(Row^.U['distinguishedName'], result, Length(result));
    end;
  end;

  function GetSelectedObjectsInTree: TRawUtf8DynArray;
  begin
    if Assigned(fLog) then
      fLog.Log(sllTrace, 'Focus on TreeView', Self);

    if not Assigned(TreeADUC.Selected) then
    begin
      if Assigned(fLog) then
        fLog.Log(sllTrace, 'No selected node.', Self);
      Exit;
    end;

    NodeData := (TreeADUC.Selected as TADUCTreeNode).GetNodeDataObject;
    if not Assigned(NodeData) then
    begin
      if Assigned(fLog) then
        fLog.Log(sllTrace, 'No node data', Self);
      Exit;
    end;
    Insert(NodeData.DistinguishedName, result, 0);
  end;

begin
  result := [];

  if GridADUC.Focused then
  begin
    result := GetSelectedObjectsInGrid;
    if (Length(result) = 0) then // If no result, fallback to tree selection
      result := GetSelectedObjectsInTree;
  end
  else if TreeADUC.Focused then
  begin
    result := GetSelectedObjectsInTree;
  end
  else
  begin
    if Assigned(fLog) then
      fLog.Log(sllWarning, 'Focus is not on grid nor tree.', Self);
  end;
end;

function TFrmModuleADUC.GetFocusedObject(OnlyContainer: Boolean): RawUtf8;
var
  ObjectClass: RawUtf8;

  function GetFocusedObjectInGrid(var ObjectClass: RawUtf8): RawUtf8;
  var
    Row: PDocVariantData;
  begin
    Row := GridADUC.FocusedRow;
    if not Assigned(Row) or not Row^.Exists('distinguishedName') then
    begin
      if Assigned(fLog) then
        fLog.Log(sllTrace, 'No focused Row on Grid', Self);
      Exit;
    end;

    result := Row^.U['distinguishedName'];
    ObjectClass := Row^.U['objectClass'];
  end;

  function GetFocusedObjectInTree(var ObjectClass: RawUtf8): RawUtf8;
  var
    NodeData: TADUCTreeNodeObject;
  begin
    if not Assigned(TreeADUC.Selected) then
    begin
      if Assigned(fLog) then
        fLog.Log(sllTrace, 'No node selected on TreeView', Self);
      Exit;
    end;

    NodeData := (TreeADUC.Selected as TADUCTreeNode).GetNodeDataObject;
    if not Assigned(NodeData) then
    begin
      if Assigned(fLog) then
        fLog.Log(sllTrace, 'No node data on TreeView', Self);
      Exit;
    end;

    result := NodeData.DistinguishedName;
    ObjectClass := NodeData.LastObjectClass;
  end;

begin
  result := '';

  if GridADUC.Focused then
  begin
    result := GetFocusedObjectInGrid(ObjectClass);
    if (result = '') then // If no result, fallback to tree selection
      result := GetFocusedObjectInTree(ObjectClass);
  end
  else if TreeADUC.Focused then
  begin
    result := GetFocusedObjectInTree(ObjectClass);
  end
  else
  begin
    if Assigned(fLog) then
      fLog.Log(sllWarning, 'Focus is not on grid nor tree.', Self);
    Exit;
  end;

  if OnlyContainer and not IsContainer(ObjectClass) then
    result := GetParentDN(result);
end;

function TFrmModuleADUC.GetFocusedObjectClass: RawUtf8;
var
  Row: PDocVariantData;
  NodeData: TADUCTreeNodeObject;
begin
  result := '';

  if GridADUC.Focused then
  begin
    Row := GridADUC.FocusedRow;
    if not Assigned(Row) or not Row^.Exists('objectClass') then
    begin
      if Assigned(fLog) then
        fLog.Log(sllTrace, 'Not assigned Row in grid or no objectClass', Self);
      Exit;
    end;
    Result := Row^.U['objectClass'];
  end
  else if TreeADUC.Focused then
  begin
    if not Assigned(TreeADUC.Selected) then
    begin
      if Assigned(fLog) then
        fLog.Log(sllTrace, 'No node selected', Self);
      Exit;
    end;

    NodeData := (TreeADUC.Selected as TADUCTreeNode).GetNodeDataObject;
    if not Assigned(NodeData) then
    begin
      if Assigned(fLog) then
        fLog.Log(sllTrace, 'No data assigned to selected node', Self);
      Exit;
    end;

    result := NodeData.LastObjectClass;
  end
  else
  begin
    if Assigned(fLog) then
      fLog.Log(sllWarning, 'Focus is not on grid nor tree.', Self);
    Exit;
  end;
end;

procedure TFrmModuleADUC.ObserverRsatOptions(Options: TOptions);
var
  RsatOptions: TRsatOptions absolute Options;
begin
  if Assigned(fADUCDomainNode) then
  begin
    try
      BeginUpdate;
      fADUCDomainNode.Selected := True;
      fADUCDomainNode.Collapse(True);
      fADUCDomainNode.Expand(False);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TFrmModuleADUC.OnLdapClientConnect(LdapClient: TLdapClient);
begin
  RefreshADUCTreeNode(fADUCDomainNode);
end;

procedure TFrmModuleADUC.OnLdapClientClose(LdapClient: TLdapClient);
begin
  TreeADUC.BeginUpdate;
  try
    if Assigned(fADUCDomainNode) and fADUCDomainNode.HasChildren then
      fADUCDomainNode.DeleteChildren;
    FreeAndNil(fADUCDomainNode);
  finally
    TreeADUC.EndUpdate;
  end;
end;

constructor TFrmModuleADUC.Create(TheOwner: TComponent; ACore: ICore);
begin
  inherited Create(TheOwner);

  fCore := ACore;

  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Create', [Self.Name]);

  fEnabled := True;

  fADUCRootNode := (TreeADUC.Items.Add(nil, 'Active Directory Users and Computers') as TADUCTreeNode);
  fADUCRootNode.ImageIndex := Ord(ileAppIcon);
  fADUCRootNode.SelectedIndex := fADUCRootNode.ImageIndex;

  fADUCQueryNode := (TreeADUC.Items.Add(nil, 'Saved Query') as TADUCTreeNode);
  fADUCQueryNode.ImageIndex := Ord(ileADContainer);
  fADUCQueryNode.SelectedIndex := fADUCQueryNode.ImageIndex;

  fADUCDomainNode := nil;

  fCore.LdapClient.RegisterObserverConnect(@OnLdapClientConnect);
  fCore.LdapClient.RegisterObserverClose(@OnLdapClientClose);
end;

destructor TFrmModuleADUC.Destroy;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Destroy', [Self.Name]);

  FreeAndNil(fModuleOptions);
  FreeAndNil(fTreeSelectionHistory);

  inherited Destroy;
end;

procedure TFrmModuleADUC.Focus(DistinguishedName: String);
var
  parentDN: String;
  node: PVirtualNode;
  row: PDocVariantData;
  FoundNode: TADUCTreeNode;

  function Find(DN: String): TADUCTreeNode;
  var
    i: Integer;
    ItemNodeData: TADUCTreeNodeObject;
  begin
    result := nil;
    for i := 0 to fADUCDomainNode.Count - 1 do
    begin
      if not Assigned(fADUCDomainNode.Items[i]) then
        continue;

      ItemNodeData := (fADUCDomainNode.Items[i] as TADUCTreeNode).GetNodeDataObject;
      if not Assigned(ItemNodeData) then
        continue;

      if (ItemNodeData.DistinguishedName = DN) then
      begin
        result := (fADUCDomainNode.Items[i] as TADUCTreeNode);
        Break;
      end;
    end;
  end;

  function Search(DN: String): TADUCTreeNode;
  var
    SplittedCN: TStringArray;
    Node: TADUCTreeNode;
    i: Integer;
    NodeData: TADUCTreeNodeObject;
    NodeDistinguishedName: String;
  begin
    result := nil;

    SplittedCN := String(DNToCN(DN)).Split('/');

    // No node assigned
    Node := fADUCDomainNode;
    if not Assigned(Node) then
      Exit;

    // No data assigned to node
    NodeData := Node.GetNodeDataObject;
    if not Assigned(NodeData) then
      Exit;

    // Get distinguishedName
    NodeDistinguishedName := NodeData.DistinguishedName;

    for i := 1 to Length(SplittedCN) - 1 do
    begin
      RefreshADUCTreeNode(Node);
      Node := Find(FormatUtf8('OU=%,%', [SplittedCN[i], NodeDistinguishedName]));
      if not Assigned(Node) then
        break;
    end;
    result := Node;
  end;

begin
  if (DistinguishedName <> Core.LdapClient.DefaultDN()) then
    parentDN := GetParentDN(DistinguishedName);

  FoundNode := Find(DistinguishedName);
  if not Assigned(FoundNode) then
    TreeADUC.Select(Search(parentDN));
  UpdateGridADUC((TreeADUC.Selected as TADUCTreeNode));

  GridADUC.FocusedNode := nil;
  GridADUC.ClearSelection;
  node := GridADUC.GetFirst();
  while Assigned(node) do
  begin
    row := GridADUC.GetNodeAsPDocVariantData(node);
    if (row^.U['distinguishedName'] = DistinguishedName) then
      GridADUC.AddToSelection(node);
    node := GridADUC.GetNext(node);
  end;
end;

procedure TFrmModuleADUC.BeginUpdate;
begin
  Inc(fUpdating);
end;

procedure TFrmModuleADUC.EndUpdate;
begin
  Dec(fUpdating);
end;

function TFrmModuleADUC.GetModuleEnabled: Boolean;
begin
  result := fEnabled;
end;

procedure TFrmModuleADUC.SetModuleEnabled(AValue: Boolean);
begin
  if AValue = fEnabled then
    Exit;

  fEnabled := AValue;
end;

function TFrmModuleADUC.GetModuleName: String;
begin
  result := rsModuleADUCName;
end;

function TFrmModuleADUC.GetModuleDisplayName: String;
begin
  result := rsModuleADUCDisplayName;
end;

function TFrmModuleADUC.GetOptions: TOptions;
begin
  result := fModuleOptions;
end;

procedure TFrmModuleADUC.Refresh;
begin
  Action_Refresh.Execute;
end;

procedure TFrmModuleADUC.Load;
begin
  fModuleOptions := TModuleADUCOptions.Create;

  fTreeSelectionHistory := TTreeSelectionHistory.Create;

  Constraints.MinWidth := GridADUC.Constraints.MinWidth + Splitter1.Width + TreeADUC.Constraints.MinWidth;

  fCore.RsatOptions.RegisterObserver(@ObserverRsatOptions);
end;

end.

