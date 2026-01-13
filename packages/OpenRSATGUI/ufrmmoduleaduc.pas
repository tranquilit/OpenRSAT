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
  ufrmmoduleaducoption,
  ufrmmodule,
  ufrmoption,
  umodule,
  uoption,
  ursatldapclient;

type

  { TFrmModuleADUC }

  TFrmModuleADUC = class(TFrameModule)
    Action_BlockGPOInheritance: TAction;
    Action_EnableGPO: TAction;
    Action_DisableGPO: TAction;
    Action_EnforceGPO: TAction;
    Action_ModifyGPLink: TAction;
    Action_OperationsMasters: TAction;
    ActionList_ADUC: TActionList;
    Action_ChangeDomainController: TAction;
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
    Image1: TImage;
    Image2: TImage;
    MenuItem_BlockGPOInheritance: TMenuItem;
    MenuItem_EnableGPO: TMenuItem;
    MenuItem_DisableGPO: TMenuItem;
    MenuItem_EnforceGPO: TMenuItem;
    MenuItem_OperationsMasters: TMenuItem;
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
    MenuItem_ChangeDomainController: TMenuItem;
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
    Panel3: TPanel;
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
    ToolButton1: TToolButton;
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
    procedure Action_BlockGPOInheritanceExecute(Sender: TObject);
    procedure Action_BlockGPOInheritanceUpdate(Sender: TObject);
    procedure Action_ChangeDomainControllerExecute(Sender: TObject);
    procedure Action_ChangeDomainControllerUpdate(Sender: TObject);
    procedure Action_CopyExecute(Sender: TObject);
    procedure Action_CopyUpdate(Sender: TObject);
    procedure Action_CutExecute(Sender: TObject);
    procedure Action_CutUpdate(Sender: TObject);
    procedure Action_DelegateControlExecute(Sender: TObject);
    procedure Action_DelegateControlUpdate(Sender: TObject);
    procedure Action_DeleteExecute(Sender: TObject);
    procedure Action_DeleteUpdate(Sender: TObject);
    procedure Action_DisableGPOExecute(Sender: TObject);
    procedure Action_EnableGPOExecute(Sender: TObject);
    procedure Action_EnforceGPOExecute(Sender: TObject);
    procedure Action_ModifyGPLinkExecute(Sender: TObject);
    procedure Action_NewAllUpdate(Sender: TObject);
    procedure Action_NewComputerExecute(Sender: TObject);
    procedure Action_NewComputerUpdate(Sender: TObject);
    procedure Action_NewContactExecute(Sender: TObject);
    procedure Action_NewContactUpdate(Sender: TObject);
    procedure Action_NewGroupExecute(Sender: TObject);
    procedure Action_NewGroupUpdate(Sender: TObject);
    procedure Action_NewOUExecute(Sender: TObject);
    procedure Action_NewOUUpdate(Sender: TObject);
    procedure Action_NewUserExecute(Sender: TObject);
    procedure Action_NewUserUpdate(Sender: TObject);
    procedure Action_NextExecute(Sender: TObject);
    procedure Action_NextUpdate(Sender: TObject);
    procedure Action_OperationsMastersExecute(Sender: TObject);
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

    // Self core
    fModuleAduc: TModuleADUC;

    // Selection history
    fTreeSelectionHistory: TTreeSelectionHistory;

    // Mormot2 logs
    fLog: TSynLog;

    // Search word for grid
    fSearchWord: RawUtf8;

    fADUCRootNode: TADUCTreeNode;
    fADUCQueryNode: TADUCTreeNode;
    fADUCDomainNode: TADUCTreeNode;

    fUpdating: Integer;

    procedure UpdateTreeImages(ANode: TADUCTreeNode);

    procedure UpdateGPLink(ANode: TADUCTreeNode; Flag: Integer);
    procedure RefreshGPLinks(ANode: TADUCTreeNode; Recursive: Boolean = True);
    procedure RemoveGPLinks(ANode: TADUCTreeNode);
    procedure RefreshADUCTreeNode(Node: TADUCTreeNode);
    procedure UpdateGridADUC(ANode: TADUCTreeNode);

    function GetSelectedObjects: TRawUtf8DynArray;
    function GetFocusedObject(OnlyContainer: Boolean = False): RawUtf8;
    function GetFocusedObjectClass: RawUtf8;

    procedure ObserverRsatOptions(Option: TOption);

    procedure OnLdapClientConnect(LdapClient: TLdapClient);
    procedure OnLdapClientClose(LdapClient: TLdapClient);
    procedure LdapConnectEvent(Sender: TObject);
    procedure LdapCloseEvent(Sender: TObject);

    procedure OnModifyEventAccountUnlock(Sender: TObject);
    procedure OnModifyEventPasswordChanged(Sender: TObject);

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure Focus(DistinguishedName: String);

    procedure BeginUpdate;
    procedure EndUpdate;

  protected
    function GetModule: TModule; override;
    function GetFrmOptionClass: TFrameOptionClass; override;
    function GetOnLdapConnect: TNotifyEvent; override;
    function GetOnLdapClose: TNotifyEvent; override;
  published
    procedure Load; override;
    procedure Refresh; override;
  end;

implementation
uses
  dialogs,
  uvissearch,
  uDarkStyleParams,
  uvisnewobject,
  uOmniselect,
  uvisdelegatecontrol,
  uvischangedn,
  uvistaskresetpassword,
  uvischangedomaincontroller,
  uvisoperationmasters,
  uvismodifygplink,
  ucommon,
  ucommonui,
  ursatldapclientui,
  ufrmrsatoptions,
  ursatoption,
  ufrmrsat,
  ugplink;

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
    UpdateGridADUC((TreeADUC.Selected as TADUCTreeNode));
  finally
    Screen.Cursor := c;
  end;
end;

procedure TFrmModuleADUC.Action_CopyExecute(Sender: TObject);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Execute', [Action_Copy.Name]);
end;

procedure TFrmModuleADUC.Action_ChangeDomainControllerUpdate(Sender: TObject);
begin

end;

procedure TFrmModuleADUC.Action_ChangeDomainControllerExecute(Sender: TObject);
var
  Vis: TVisChangeDomainController;
  mr: Integer;
begin
  Vis := TVisChangeDomainController.Create(Self);

  try
    mr := Vis.ShowModal;
    if (mr <> mrOK) or (Vis.DomainController = '') then
      Exit;
    FrmRSAT.ChangeDomainController(Vis.DomainController);
  finally
    FreeAndNil(Vis);
  end;
end;

procedure TFrmModuleADUC.Action_BlockGPOInheritanceExecute(Sender: TObject);
var
  NodeData: TADUCTreeNodeObject;
  BakGPOptions: RawUtf8;
begin
  NodeData := (TreeADUC.Selected as TADUCTreeNode).GetNodeDataObject;
  if not Assigned(NodeData) then
    Exit;
  BakGPOptions := NodeData.GPOptions;
  if NodeData.GPOptions = '1' then
    NodeData.GPOptions := '0'
  else
    NodeData.GPOptions := '1';
  if not FrmRSAT.LdapClient.Modify(NodeData.DistinguishedName, lmoReplace, 'gPOptions', NodeData.GPOptions) then
  begin
    NodeData.GPOptions := BakGPOptions;
    Exit;
  end;
  TreeADUC.OnGetImageIndex(Self, TreeADUC.Selected);
end;

procedure TFrmModuleADUC.Action_BlockGPOInheritanceUpdate(Sender: TObject);
var
  NodeData: TADUCTreeNodeObject;
begin
  NodeData := (TreeADUC.Selected as TADUCTreeNode).GetNodeDataObject;
  if not Assigned(NodeData) then
    Exit;

  Action_BlockGPOInheritance.Checked := NodeData.GPOptions = '1';
end;

procedure TFrmModuleADUC.Action_CopyUpdate(Sender: TObject);
begin

end;

procedure TFrmModuleADUC.Action_CutExecute(Sender: TObject);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Execute', [Action_Cut.Name]);
end;

procedure TFrmModuleADUC.Action_CutUpdate(Sender: TObject);
begin

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

  With TVisDelegateControl.Create(Self) do
  try
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
    if (SelectedObject = '') then
      continue;
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
      if not FrmRSAT.LdapClient.Delete(SelectedObject, True) then
      begin
        if Assigned(fLog) then
          fLog.Log(sllError, 'Ldap deletion failed: "%"', [FrmRSAT.LdapClient.ResultString], Action_Delete);
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
  Action_delete.Enabled := Assigned(FrmRSAT.LdapClient) and FrmRSAT.LdapClient.Connected and ((GridADUC.SelectedCount > 0) or (Assigned(TreeADUC.Selected)));
end;

procedure TFrmModuleADUC.Action_DisableGPOExecute(Sender: TObject);
begin
  UpdateGPLink((TreeADUC.Selected as TADUCTreeNode), 1);
end;

procedure TFrmModuleADUC.Action_EnableGPOExecute(Sender: TObject);
begin
  UpdateGPLink((TreeADUC.Selected as TADUCTreeNode), 0);
end;

procedure TFrmModuleADUC.Action_EnforceGPOExecute(Sender: TObject);
begin
  UpdateGPLink((TreeADUC.Selected as TADUCTreeNode), 2);
end;

procedure TFrmModuleADUC.Action_ModifyGPLinkExecute(Sender: TObject);
var
  Vis: TVisModifyGPLink;
begin
  Vis := TVisModifyGPLink.Create(Self, GetFocusedObject(True));

  try
    Vis.ShowModal;
    RefreshADUCTreeNode((TreeADUC.Selected as TADUCTreeNode));
  finally
    FreeAndNil(Vis);
  end;
end;

procedure TFrmModuleADUC.Action_NewAllUpdate(Sender: TObject);
var
  menuItems: Array of TMenuItem;
  newList: Set of TNewAction;
  item, i: Integer;
  filter: UInt64;
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
    naOrganizationalUnit,
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

  With TVisNewObject.Create(Self, vnotComputer, GetFocusedObject(True), FrmRSAT.LdapClient.DefaultDN) do
  begin
    PageCount := 1;
    Ldap := FrmRSAT.LdapClient;
    if ShowModal = mrOK then
      Action_Refresh.Execute;
  end;
end;

procedure TFrmModuleADUC.Action_NewComputerUpdate(Sender: TObject);
begin
  Action_NewComputer.Enabled := Assigned(FrmRSAT.LdapClient) and FrmRSAT.LdapClient.Connected;
end;

procedure TFrmModuleADUC.Action_NewContactExecute(Sender: TObject);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Execute', [Action_NewContact.Caption]);

  With TVisNewObject.Create(Self, vnotContact, GetFocusedObject(True), FrmRSAT.LdapClient.DefaultDN) do
  begin
    PageCount := 1;
    Ldap := FrmRSAT.LdapClient;
    if ShowModal = mrOK then
      Action_Refresh.Execute;
  end;
end;

procedure TFrmModuleADUC.Action_NewContactUpdate(Sender: TObject);
begin
  Action_NewContact.Enabled := Assigned(FrmRSAT.LdapClient) and FrmRSAT.LdapClient.Connected;
end;

procedure TFrmModuleADUC.Action_NewGroupExecute(Sender: TObject);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Execute', [Action_NewGroup.Caption]);

  With TVisNewObject.Create(Self, vnotGroup, GetFocusedObject(True), FrmRSAT.LdapClient.DefaultDN) do
  begin
    PageCount := 1;
    Ldap := FrmRSAT.LdapClient;
    if ShowModal = mrOK then
      Action_Refresh.Execute;
  end;
end;

procedure TFrmModuleADUC.Action_NewGroupUpdate(Sender: TObject);
begin
  Action_NewGroup.Enabled := Assigned(FrmRSAT.LdapClient) and FrmRSAT.LdapClient.Connected;
end;

procedure TFrmModuleADUC.Action_NewOUExecute(Sender: TObject);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Execute', [Action_NewOU.Caption]);

  With TVisNewObject.Create(Self, vnotOrganizationalUnit, GetFocusedObject(True), FrmRSAT.LdapClient.DefaultDN()) do
  begin
    PageCount := 1;
    Ldap := FrmRSAT.LdapClient;
    if ShowModal = mrOK then
      Action_Refresh.Execute;
  end;
end;

procedure TFrmModuleADUC.Action_NewOUUpdate(Sender: TObject);
begin
  Action_NewOU.Enabled := Assigned(FrmRSAT.LdapClient) and FrmRSAT.LdapClient.Connected;
end;

procedure TFrmModuleADUC.Action_NewUserExecute(Sender: TObject);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Execute', [Action_NewUser.Caption]);

  With TVisNewObject.Create(Self, vnotUser, GetFocusedObject(True), FrmRSAT.LdapClient.DefaultDN) do
  begin
    PageCount := 3;
    Ldap := FrmRSAT.LdapClient;
    if ShowModal = mrOK then
      Action_Refresh.Execute;
  end;
end;

procedure TFrmModuleADUC.Action_NewUserUpdate(Sender: TObject);
begin
  Action_NewUser.Enabled := Assigned(FrmRSAT.LdapClient) and FrmRSAT.LdapClient.Connected;
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

procedure TFrmModuleADUC.Action_OperationsMastersExecute(Sender: TObject);
var
  Vis: TVisOperationMasters;
begin
  Vis := TVisOperationMasters.Create(Self);
  try
    Vis.ShowModal();
  finally
    FreeAndNil(Vis);
  end;
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
    if (SelectedText = '') and (DistinguishedName <> '') then
      SelectedText := DNToCN(DistinguishedName);
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

  FrmRSAT.OpenProperty(DistinguishedName, SelectedText);
end;

procedure TFrmModuleADUC.Action_PropertiesUpdate(Sender: TObject);
begin
  Action_Properties.Enabled := Assigned(FrmRSAT.LdapClient) and FrmRSAT.LdapClient.Connected and ((GridADUC.SelectedCount > 0) or (Assigned(TreeADUC.Selected)));
end;

procedure TFrmModuleADUC.Action_RefreshUpdate(Sender: TObject);
begin
  Action_Refresh.Enabled := {Active and} Assigned(FrmRSAT.LdapClient) and FrmRSAT.LdapClient.Connected();
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
  Action_Search.Enabled := Assigned(FrmRSAT.LdapClient) and FrmRSAT.LdapClient.Connected;
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
  selectedDistinguishedNameArray: TRawUtf8DynArray;

  // Create an attribute to add a member to a group.
  // It does a modify ldap request with an Add operation.
  // In case of error, it shows a message to the user.
  procedure AddMemberToGroup(Member, Group: RawUtf8);
  var
    AttributeMember: TLdapAttribute;
  begin
    AttributeMember := TLdapAttribute.Create('member', atMember);
    try
      AttributeMember.Add(Member);

      if not FrmRSAT.LdapClient.Modify(Group, lmoAdd, AttributeMember) then
      begin
        if Assigned(fLog) then
          fLog.Log(sllError, 'Ldap modify error: "%"', [FrmRSAT.LdapClient.ResultString]);
      end;
    finally
      FreeAndNil(AttributeMember);
    end;
  end;

  // Search for groups that contains the focused object.
  // If so, create a ldap filter to exclude those groups.
  // It prevents the addition of a member in a group that already contains this member.
  function ExclusionFilter(Member: RawUtf8): RawUtf8;
  var
    SearchResult: TLdapResult;
    memberOf: RawUtf8;
  begin
    Result := '';
    FrmRSAT.LdapClient.SearchBegin();
    try
      FrmRSAT.LdapClient.SearchScope := lssWholeSubtree;

      repeat
        if not FrmRSAT.LdapClient.Search(FrmRSAT.LdapClient.DefaultDN, False, FormatUtf8('(member=%)', [LdapEscape(Member)]), ['distinguishedName']) then
        begin
          if Assigned(fLog) then
            fLog.Log(sllError, 'Ldap Search Error: "%"', [FrmRSAT.LdapClient.ResultString]);
          Exit;
        end;

        for SearchResult in FrmRSAT.LdapClient.SearchResult.Items do
        begin
          if not Assigned(SearchResult) then
            continue;

          memberOf := SearchResult.Find('distinguishedName').GetReadable();
          Result := FormatUtf8('%(distinguishedName=%)', [Result, memberOf]);
        end;
      until FrmRSAT.LdapClient.SearchCookie = '';
    finally
      FrmRSAT.LdapClient.SearchEnd();
    end;
    if (Result <> '') then
      Result := FormatUtf8('(!(|%))', [Result]);
  end;

begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Execute', [Action_TaskAddToAGroup.Caption]);

  // Retrieve Focused object distinguishedName
  DistinguishedName := GridADUC.FocusedRow^.S['distinguishedName'];

  // Open the vis to let the user select groups
  With TVisOmniselect.Create(Self, FrmRSAT.LdapClient, ['group'], FrmRSAT.LdapClient.DefaultDN, True, ExclusionFilter(DistinguishedName)) do
  try
    Caption := rsTitleSelectGroups;
    if (ShowModal <> mrOK) then
      Exit;
    selectedDistinguishedNameArray := SelectedObjects;
  finally
    Free;
  end;

  // Add the member to the selected groups
  for SelectedDistinguishedName in selectedDistinguishedNameArray do
  begin
    if SelectedDistinguishedName = '' then
      continue;
    AddMemberToGroup(DistinguishedName, SelectedDistinguishedName);
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
  With TVisChangeDN.Create(Self, FrmRSAT.LdapClient, DistinguishedName, FrmRSAT.LdapClient.DefaultDN) do
  try
    if (mrOK <> ShowModal) then
    begin
      if Assigned(fLog) then
        fLog.Log(sllInfo, '% - Task canceled.');
      Exit;
    end;
    if not FrmRSAT.LdapClient.MoveLdapEntry(DistinguishedName, Join([DistinguishedName.Split(',')[0], ',', SelectedDN])) then
    begin
      if Assigned(fLog) then
        fLog.Log(sllError, '% - Failed to move ldap entry "%" to "%": %', [DistinguishedName, SelectedDN, FrmRSAT.LdapClient.ResultString]);
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
    userData := FrmRSAT.LdapClient.SearchObject(userDN, '', ['userAccountControl', 'objectClass']);
    if not Assigned(userData) then
    begin
      if Assigned(fLog) then
        fLog.Log(sllError, '% - Cannot retrieve information about "%". (%)', [Action_TaskResetPassword.Caption, userDN, FrmRSAT.LdapClient.ResultString]);
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
        FrmRSAT.LdapClient.OnModify := @OnModifyEventAccountUnlock;
        if not FrmRSAT.LdapClient.Modify(userDN, lmoReplace, attr) then
        begin
          if Assigned(fLog) then
            fLog.Log(sllError, '% - Ldap Modify Error: %', [Action_TaskResetPassword.Caption, FrmRSAT.LdapClient.ResultString]);
          Exit;
        end;
      finally
        FreeAndNil(attr);
      end;
    end;
    attr := TLdapAttribute.Create('unicodePwd', atUndefined);
    try
      attr.add(LdapUnicodePwd(Edit_NewPassword.Text));
      FrmRSAT.LdapClient.OnModify := @OnModifyEventPasswordChanged;
      if not FrmRSAT.LdapClient.Modify(userDN, lmoReplace, attr) then
      begin
        if Assigned(fLog) then
          fLog.Log(sllError, '% - Ldap Modify Error: %', [Action_TaskResetPassword.Caption, FrmRSAT.LdapClient.ResultString]);
        Exit;
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
        if not FrmRSAT.LdapClient.Modify(userDN, lmoReplace, attr) then
        begin
          if Assigned(fLog) then
            fLog.Log(sllError, '% - Ldap Modify Error: %', [Action_TaskResetPassword.Caption, FrmRSAT.LdapClient.ResultString]);
          Exit;
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
  VisibleItems := [];

  case (TreeADUC.Selected as TADUCTreeNode).NodeType of
    atntObject:
    begin
      VisibleItems := Concat(VisibleItems, [MenuItem_DelegateControl,
        MenuItem_ChangeDomainController,MenuItem_OperationsMasters,MenuItem_New,
        MenuItem_Search,MenuItem_Refresh,MenuItem_Delete,MenuItem_Properties]);
      if fModuleAduc.ADUCOption.ShowGPO then
        VisibleItems := Concat(VisibleItems, [MenuItem_BlockGPOInheritance]);
    end;
    atntGPO: VisibleItems := Concat(VisibleItems, [MenuItem_EnableGPO,
      MenuItem_DisableGPO, MenuItem_EnforceGPO]);
  end;

  Handled := not Assigned(FrmRSAT.LdapClient) or not FrmRSAT.LdapClient.Connected;

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
  FrmRSAT.LdapClient.MoveLdapEntries(oldDNS, newDNS);
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
  ObjectClass: RawUtf8;
begin
  // Fast Exit

  case (Node as TADUCTreeNode).NodeType of
    atntObject:
    begin
      ObjectClass := (Node as TADUCTreeNode).GetNodeDataObject.LastObjectClass;
      Node.ImageIndex := ObjectClassToImageIndex(ObjectClass);
      if fModuleAduc.ADUCOption.ShowGPO and ((Node as TADUCTreeNode).GetNodeDataObject.GPOptions = '1') then
      case TImageListEnum(Node.ImageIndex) of
        ileADOU: Node.ImageIndex := Ord(ileADOUGPOInheritanceDisable);
        ileADContainer: Node.ImageIndex := Ord(ileADContainerGPOInheritanceDisabled);
      end;
    end;
    atntGPO:
    begin
      case (Node as TADUCTreeNode).GetNodeDataGPO.Flag of
        0: Node.ImageIndex := Ord(ileADGPOEnabled);
        1: Node.ImageIndex := Ord(ileADGPODisabled);
        2: Node.ImageIndex := Ord(ileADGPOEnforced);
        else
          Exit;
      end;
    end;
    else
      Exit;
  end;

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
  Handled := not Assigned(FrmRSAT.LdapClient) or not FrmRSAT.LdapClient.Connected;
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

  FrmRSAT.LdapClient.MoveLdapEntries(oldDNS, newDNS);
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
  if not FrmRSAT.LdapClient.RenameLdapEntry(DistinguishedName, newRDN) then
  begin
    if Assigned(fLog) then
      fLog.Log(sllError, 'Rename Ldap Entry: "%"', [FrmRSAT.LdapClient.ResultString], Self);
    Exit;
  end;

  DistinguishedName := newRDN;
  for i := 1 to High(DistinguishedNameParsed) do
    DistinguishedName := Format('%s,%s=%s', [DistinguishedName, DistinguishedNameParsed[i].Name, DistinguishedNameParsed[i].Value]);
  GridADUC.GetNodeAsPDocVariantData(Node)^.U['distinguishedName'] := DistinguishedName;
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
    ImageIndex := ObjectClassToImageIndex(GridADUC.GetNodeAsPDocVariantData(Node)^.S['objectClass']);
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
      fLog.Log(sllWarning, 'No node data', Self);
    Exit;
  end;

  TreeADUC.BeginUpdate;
  try
    TreeADUC.OnGetImageIndex(Self, ANode);
    for i := 0 to ANode.Count - 1 do
    begin
      if not Assigned(ANode.Items[i]) then
        continue;

      NodeData := (ANode.Items[i] as TADUCTreeNode).GetNodeDataObject;
      if not Assigned(NodeData) then
        continue;

      TreeADUC.OnGetImageIndex(Self, ANode.Items[i]);
    end;
  finally
    TreeADUC.EndUpdate;
  end;
  if Assigned(fLog) then
    fLog.Log(sllInfo, '% - Node "%" and subnodes (%) have been updated.', [Self.Name, ANode.Text, IntToStr(ANode.Count)]);
end;

procedure TFrmModuleADUC.UpdateGPLink(ANode: TADUCTreeNode; Flag: Integer);
var
  NodeDataGPO: TADUCTreeNodeGPO;
  ParentNode, ChildNodeGPO: TADUCTreeNode;
  Count, BackupFlag: Integer;
  GPLinkArr: TGPLinkDynArr;
  GPLink: RawUtf8;
begin
  NodeDataGPO := ANode.GetNodeDataGPO;
  if not Assigned(NodeDataGPO) then
    Exit;
  BackupFlag := NodeDataGPO.Flag;
  NodeDataGPO.Flag := Flag;
  ParentNode := (ANode.Parent as TADUCTreeNode);

  Count := 0;
  ChildNodeGPO := ParentNode.GetFirstChildGPO;
  while Assigned(ChildNodeGPO) do
  begin
    Insert(ChildNodeGPO.GetNodeDataGPO.GPLink, GPLinkArr, Count);
    Inc(Count);
    ChildNodeGPO := ParentNode.GetNextChildGPO(ChildNodeGPO);
  end;
  GPLink := GPLinkArrToGPLink(GPLinkArr);
  if not FrmRSAT.LdapClient.Modify(ParentNode.GetNodeDataObject.DistinguishedName, lmoReplace, 'gPLink', GPLink) then
  begin
    NodeDataGPO.Flag := BackupFlag;
    Exit;
  end;
  RefreshADUCTreeNode(ParentNode);
end;

procedure TFrmModuleADUC.RefreshGPLinks(ANode: TADUCTreeNode; Recursive: Boolean
  );
var
  GPLinkArr: TGPLinkDynArr;
  GPLink: TGPLink;
  GPOItems: TLdapResultObjArray;
  NodeGPO, NodeToDelete: TADUCTreeNode;
  Filter: RawUtf8;
  pairs: TNameValueDNs;
  GPOItem: TLdapResult;
  Found: Boolean;
  i: Integer;
begin
  if not Assigned(ANode) then
    ANode := fADUCDomainNode;

  if not Assigned(ANode) or not Assigned(ANode.GetNodeDataObject) then
    Exit;
  GPLinkArr := GPLinkToGPLinkArr(ANode.GetNodeDataObject.GPLink);

  if Length(GPLinkArr) <= 0 then
    Exit;

  GPOItems := nil;

  // Retrieve existing GPO Nodes
  // Compare GPLinkArr with existing nodes
  Filter := '';
  for GPLink in GPLinkArr do
    Filter := FormatUtf8('%(distinguishedName=%)', [Filter, LdapEscape(GPLink.DistinguishedName)]);
  if Filter <> '' then
    Filter := FormatUtf8('(|%)', [Filter]);

  FrmRSAT.LdapClient.SearchBegin();
  try
    FrmRSAT.LdapClient.SearchScope := lssWholeSubtree;
    repeat
      if not FrmRSAT.LdapClient.Search(FrmRSAT.LdapClient.DefaultDN, False, Filter, ['displayName']) then
      begin
        if Assigned(fLog) then
          fLog.Log(sllError, 'Ldap Search Error: "%"', [FrmRSAT.LdapClient.ResultString]);
        Exit;
      end;
      GPOItems := Concat(GPOItems, FrmRSAT.LdapClient.SearchResult.Items);
    until FrmRSAT.LdapClient.SearchCookie = '';
  finally
    FrmRSAT.LdapClient.SearchEnd;
  end;

  TreeADUC.BeginUpdate;
  try
    for GPLink in GPLinkArr do
    begin
      NodeGPO := ANode.GetFirstChildGPO();
      while Assigned(NodeGPO) do
      begin
        if NodeGPO.GetNodeDataGPO.DistinguishedName = GPLink.DistinguishedName then
          break;
        NodeGPO := ANode.GetNextChildGPO(NodeGPO);
      end;
      if Assigned(NodeGPO) then
      begin
        NodeGPO.GetNodeDataGPO.GPLink := GPLink;
        for GPOItem in GPOItems do
          if String(GPOItem.ObjectName).ToLower = String(GPLink.DistinguishedName).ToLower then
          begin
            NodeGPO.Text := GPOItem.Find('displayName').GetReadable();
            break;
          end;
      end
      else
      begin
        ParseDN(GPLink.DistinguishedName, pairs);
        NodeGPO := (TreeADUC.Items.AddChild(ANode, pairs[0].Value) as TADUCTreeNode);
        NodeGPO.NodeType := atntGPO;
        NodeGPO.GetNodeDataGPO.GPLink := GPLink;
        for GPOItem in GPOItems do
          if String(GPOItem.ObjectName).ToLower = String(GPLink.DistinguishedName).ToLower then
          begin
            NodeGPO.Text := GPOItem.Find('displayName').GetReadable();
            break;
          end;
      end;
      TreeADUC.OnGetImageIndex(Self, NodeGPO);
    end;
  finally
    TreeADUC.EndUpdate;
  end;

  NodeToDelete := nil;
  TreeADUC.BeginUpdate;
  try
    NodeGPO := ANode.GetFirstChildGPO();
    while Assigned(NodeGPO) do
    begin
      Found := False;
      for GPLink in GPLinkArr do
      begin
        if String(GPLink.DistinguishedName).ToLower = String(NodeGPO.GetNodeDataGPO.DistinguishedName).ToLower then
        begin
          Found := True;
          break;
        end;
      end;
      if not Found then
        NodeToDelete := NodeGPO;
      NodeGPO := ANode.GetNextChildGPO(NodeGPO);
      if Assigned(NodeToDelete) then
      begin
        TreeADUC.Items.Delete(NodeToDelete);
        NodeToDelete := nil;
      end;
    end;
  finally
    TreeADUC.EndUpdate;
  end;

  if Recursive then
    for i := 0 to Pred(ANode.Count) do
      RefreshGPLinks((ANode.Items[i] as TADUCTreeNode), Recursive);

end;

procedure TFrmModuleADUC.RemoveGPLinks(ANode: TADUCTreeNode);
var
  Node, PrevNode: TTreeNode;
begin
  if not Assigned(ANode) then
    ANode := fADUCDomainNode;

  if not Assigned(ANode) then
    Exit;

  TreeADUC.Items.BeginUpdate;
  try
    Node := ANode.GetFirstChild;
    while Assigned(Node) do
    begin
      PrevNode := Node;
      Node := ANode.GetNextChild(Node);

      case (PrevNode as TADUCTreeNode).NodeType of
      atntGPO: TreeADUC.Items.Delete(PrevNode);
      atntObject: if PrevNode.HasChildren then RemoveGPLinks((PrevNode as TADUCTreeNode))
      end;
    end;
  finally
    TreeADUC.Items.EndUpdate;
  end;
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
    if (Length(fModuleAduc.ADUCOption.TreeObjectClasses) < 1) then
      Exit;

    result := '(&';
    if not FrmRSAT.RSATOption.AdvancedView then
      result := FormatUtf8('%(|(!(showInAdvancedViewOnly=*))(showInAdvancedViewOnly=FALSE))', [result]);
    result := FormatUtf8('%%(|', [result, fModuleAduc.ADUCOption.TreeFilter]);
    for i := 0 to High(fModuleAduc.ADUCOption.TreeObjectClasses) do
      result := FormatUtf8('%(objectClass=%)', [result, fModuleAduc.ADUCOption.TreeObjectClasses[i]]);
    result := FormatUtf8('%))', [result]);
  end;

begin
  // Fast Exit
  if not Assigned(FrmRSAT) or
     not Assigned(FrmRSAT.LdapClient) or
     not FrmRSAT.LdapClient.Connected or
     (fUpdating > 0) then
    Exit;

  // Fallback to domainNode
  if not Assigned(Node) then
    Node := fADUCDomainNode;

  // Get Ldap instance
  Ldap := FrmRSAT.LdapClient;

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
      if not Assigned(fADUCDomainNode) then
      begin
        Node.Selected := True;
        fADUCDomainNode := Node;
        Exit;
      end;
    finally
      TreeADUC.EndUpdate;
    end;
  end
  else
  begin
    Obj := Ldap.SearchObject(Node.GetNodeDataObject.DistinguishedName, '', ['distinguishedName', 'objectClass', 'gPLink', 'name']);
    if not Assigned(Obj) then
    begin
      if Assigned(fLog) then
        fLog.Log(sllError, 'Ldap search object failed: "%"', [Ldap.ResultString], Self);
      Exit;
    end;
  end;
  Node.GetNodeDataObject.GPLink := Obj.Find('gPLink').GetReadable();

  NodeData := Node.GetNodeDataObject;
  if not Assigned(NodeData) then
    Exit;

  // Backup Items node
  BackupItems.Init();
  for i := 0 to Pred(Node.Count) do
  begin
    if not Assigned(Node.Items[i]) then
      continue;
    Node.Items[i].ImageIndex := -1;
    ItemNodeData := (Node.Items[i] as TADUCTreeNode).GetNodeDataObject;
    if not Assigned(ItemNodeData) then
      continue;

    BackupItems.I[ItemNodeData.DistinguishedName] := i;
  end;

  // Retrieve object from AD
  Ldap.SearchBegin(fModuleAduc.ADUCOption.SearchPageSize);
  TreeADUC.BeginUpdate;
  try
    Ldap.SearchScope := lssSingleLevel;
    repeat
      if not Ldap.Search(NodeData.DistinguishedName, False, BuildFilter, ['distinguishedName', 'objectClass', 'gPLink', 'name', 'gPOptions']) then
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

        TreeADUC.OnGetImageIndex(Self, RefreshNode);
        ItemNodeData := (RefreshNode as TADUCTreeNode).GetNodeDataObject;
        if not Assigned(ItemNodeData) then
          continue;
        ItemNodeData.DistinguishedName := NodeDN;
        ItemNodeData.Name := SearchResult.Find('name').GetReadable();
        RefreshNode.Text := ItemNodeData.Name;
        ItemNodeData.ObjectClass := SearchResult.Find('objectClass').GetAllReadable;
        ItemNodeData.GPLink := SearchResult.Find('gPLink').GetReadable();
        ItemNodeData.GPOptions := SearchResult.Find('gPOptions').GetReadable();
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
  TreeADUC.OnGetImageIndex(Self, Node);

  // Refresh GPO
  if fModuleAduc.ADUCOption.ShowGPO then
    RefreshGPLinks(Node, False);

  // Sort nodes
  Node.CustomSort(@Node.ADUCNodeCompare);
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
      fLog.Log(sllWarning, 'No node data', Self);
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

  FrmRSAT.LdapClient.SearchBegin(fModuleAduc.ADUCOption.SearchPageSize);
  GridADUC.BeginUpdate;
  try
    FrmRSAT.LdapClient.SearchPagingBegin(fModuleAduc.ADUCOption.SearchPageNumber);
    FrmRSAT.LdapClient.SearchScope := lssSingleLevel;
    Filter := '(&';
    if not FrmRSAT.RSATOption.AdvancedView then
      Filter := FormatUtf8('%(|(!(showInAdvancedViewOnly=*))(showInAdvancedViewOnly=FALSE))', [Filter]);
    Filter := FormatUtf8('%%)', [Filter, fModuleAduc.ADUCOption.GridFilter]);
    if not FrmRSAT.LdapClient.SearchAllDocPaged(@DocResult, DistinguishedName, False, Filter, ['distinguishedName', 'objectClass', 'name', 'description']) then
    begin
      if Assigned(fLog) then
        fLog.Log(sllError, '% - Ldap search error: "%"', [GridADUC.Name, FrmRSAT.LdapClient.ResultString]);
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
    FrmRSAT.LdapClient.SearchPagingEnd;
    FrmRSAT.LdapClient.SearchEnd;
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
  var
    Count: Integer;
  begin
    result := nil;
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

    Count := 0;
    for Row in SelectedRows.Objects do
    begin
      // Invalid row
      if not Assigned(Row) then
        continue;

      if Row^.Exists('distinguishedName') then
      begin
        Insert(Row^.U['distinguishedName'], result, Count);
        Inc(Count);
      end;
    end;
  end;

  function GetSelectedObjectsInTree: TRawUtf8DynArray;
  begin
    result := nil;
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

  function GetFocusedObjectClassInTree: RawUtf8;
  var
    NodeData: TADUCTreeNodeObject;
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
  end;

begin
  result := '';

  if GridADUC.Focused then
  begin
    Row := GridADUC.FocusedRow;
    if not Assigned(Row) or not Row^.Exists('objectClass') then
    begin
      if Assigned(fLog) then
        fLog.Log(sllTrace, 'Not assigned Row in grid or no objectClass', Self);
      result := GetFocusedObjectClassInTree;
    end
    else
      Result := Row^.U['objectClass'];
  end
  else if TreeADUC.Focused then
  begin
    result := GetFocusedObjectClassInTree;
  end
  else
  begin
    if Assigned(fLog) then
      fLog.Log(sllWarning, 'Focus is not on grid nor tree.', Self);
    Exit;
  end;
end;

procedure TFrmModuleADUC.ObserverRsatOptions(Option: TOption);
var
  ARsatOption: TRsatOption absolute Option;
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
    GridADUC.Clear;
    FreeAndNil(fADUCDomainNode);
  finally
    TreeADUC.EndUpdate;
  end;
end;

procedure TFrmModuleADUC.LdapConnectEvent(Sender: TObject);
begin
  RefreshADUCTreeNode(fADUCDomainNode);
end;

procedure TFrmModuleADUC.LdapCloseEvent(Sender: TObject);
begin
  TreeADUC.BeginUpdate;
  try
    if Assigned(fADUCDomainNode) and fADUCDomainNode.HasChildren then
      fADUCDomainNode.DeleteChildren;
    GridADUC.Clear;
    FreeAndNil(fADUCDomainNode);
  finally
    TreeADUC.EndUpdate;
  end;
end;

procedure TFrmModuleADUC.OnModifyEventAccountUnlock(Sender: TObject);
begin
  MessageDlg(rsAccountUnlocked, rsAccountUnlockedMessage, mtInformation, [mbOK], 0);
  (Sender as TRsatLdapClient).OnModify := nil;
end;

procedure TFrmModuleADUC.OnModifyEventPasswordChanged(Sender: TObject);
begin
  MessageDlg(rsResetPassword, rsResetPasswordMessage, mtInformation, [mbOK], 0);
  (Sender as TRsatLdapClient).OnModify := nil;
end;

constructor TFrmModuleADUC.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Create', [Self.Name]);

  fEnabled := True;

  fModuleAduc := TModuleADUC.Create;
  fTreeSelectionHistory := TTreeSelectionHistory.Create;

  fADUCRootNode := (TreeADUC.Items.Add(nil, 'Active Directory Users and Computers') as TADUCTreeNode);
  fADUCRootNode.ImageIndex := Ord(ileAppIcon);
  fADUCRootNode.SelectedIndex := fADUCRootNode.ImageIndex;

  fADUCQueryNode := (TreeADUC.Items.Add(nil, 'Saved Query') as TADUCTreeNode);
  fADUCQueryNode.ImageIndex := Ord(ileADContainer);
  fADUCQueryNode.SelectedIndex := fADUCQueryNode.ImageIndex;

  fADUCDomainNode := nil;

  {$IFDEF WINDOWS}
  Image1.Visible := not IsDarkModeEnabled;
  Image2.Visible := IsDarkModeEnabled;
  {$ELSE}
  Image1.Visible := True;
  Image2.Visible := False;
  {$ENDIF}
end;

destructor TFrmModuleADUC.Destroy;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Destroy', [Self.Name]);

  FreeAndNil(fTreeSelectionHistory);
  FreeAndNil(fModuleAduc);

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
  if (DistinguishedName <> FrmRSAT.LdapClient.DefaultDN()) then
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

function TFrmModuleADUC.GetModule: TModule;
begin
  result := fModuleAduc;
end;

function TFrmModuleADUC.GetFrmOptionClass: TFrameOptionClass;
begin
  result := TFrmModuleADUCOption;
end;

function TFrmModuleADUC.GetOnLdapConnect: TNotifyEvent;
begin
  result := @LdapConnectEvent;
end;

function TFrmModuleADUC.GetOnLdapClose: TNotifyEvent;
begin
  result := @LdapCloseEvent;
end;

procedure TFrmModuleADUC.Load;
begin

end;

procedure TFrmModuleADUC.Refresh;
begin
  if fModuleAduc.ADUCOption.ShowGPO then
    RefreshGPLinks(nil)
  else
    RemoveGPLinks(nil);

  RefreshADUCTreeNode((TreeADUC.Selected as TADUCTreeNode));
end;

end.

