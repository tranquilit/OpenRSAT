unit uvisadvancedsecurity;

{$mode ObjFPC}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
  ActnList,
  Buttons,
  Classes,
  Controls,
  Dialogs,
  ExtCtrls,
  Forms,
  tis.ui.grid.core,
  StdCtrls,
  Menus,
  VirtualTrees,
  mormot.core.base,
  mormot.core.log,
  mormot.core.os.security,
  mormot.core.variants,
  mormot.net.ldap,
  ucommon;

type

  { TVisAdvancedSecurity }

  TVisAdvancedSecurity = class(TForm)
    Action_Restore: TAction;
    Action_OK: TAction;
    Action_Cancel: TAction;
    Action_Apply: TAction;
    BitBtn_Cancel: TBitBtn;
    BitBtn_Apply: TBitBtn;
    BitBtn_OK: TBitBtn;
    BitBtn4: TBitBtn;
    CheckBox_SR: TCheckBox;
    CheckBox_DT: TCheckBox;
    CheckBox_SD: TCheckBox;
    CheckBox_SP: TCheckBox;
    CheckBox_DD: TCheckBox;
    CheckBox_DP: TCheckBox;
    CheckBox_GD: TCheckBox;
    CheckBox_OD: TCheckBox;
    CheckBox_RM: TCheckBox;
    CheckBox_PS: TCheckBox;
    CheckBox_PD: TCheckBox;
    CheckBox_SI: TCheckBox;
    CheckBox_DI: TCheckBox;
    CheckBox_SC: TCheckBox;
    CheckBox_DC: TCheckBox;
    CheckBox_SS: TCheckBox;
    CheckGroup_ACLFlags: TCheckGroup;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    Panel1: TPanel;
    PanelTop: TPanel;
      Edit_Group: TEdit;
      Label_Group: TLabel;
      Label_Owner: TLabel;
      Edit_Owner: TEdit;
      CheckBox_Raw: TCheckBox;
      PopupMenu1: TPopupMenu;
      Timer_SearchInGrid: TTimer;
    TisGrid_ACL: TTisGrid;
    PanelBottom: TPanel;
      BitBtn_AddAce: TBitBtn;
      BitBtn_DeleteAce: TBitBtn;
      BitBtn_EditAce: TBitBtn;
    ActionList: TActionList;
      Action_AddAce: TAction;
      Action_DeleteAce: TAction;
      Action_EditAce: TAction;
      procedure Action_AddAceExecute(Sender: TObject);
      procedure Action_ApplyExecute(Sender: TObject);
      procedure Action_ApplyUpdate(Sender: TObject);
      procedure Action_CancelExecute(Sender: TObject);
      procedure Action_DeleteAceExecute(Sender: TObject);
      procedure Action_DeleteAceUpdate(Sender: TObject);
      procedure Action_EditAceExecute(Sender: TObject);
      procedure Action_EditAceUpdate(Sender: TObject);
      procedure Action_OKExecute(Sender: TObject);
      procedure Action_RestoreExecute(Sender: TObject);
      procedure CheckBox_RawChange(Sender: TObject);
      procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
      procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
      procedure Timer_SearchInGridTimer(Sender: TObject);
      procedure TisGrid_ACLDblClick(Sender: TObject);
      procedure TisGrid_ACLGetImageIndex(Sender: TBaseVirtualTree;
        Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
        var Ghosted: Boolean; var ImageIndex: Integer);
      procedure TisGrid_ACLGetText(aSender: TBaseVirtualTree;
        aNode: PVirtualNode; const aCell: TDocVariantData;
        aColumn: TColumnIndex; aTextType: TVSTTextType; var aText: string);
      procedure TisGrid_ACLKeyPress(Sender: TObject; var Key: char);
  private
    SecurityDescriptor: PSecurityDescriptor;
    SampleSD: TSecurityDescriptor;
    Ldap: TLdapClient;
    DN: RawUtf8;
    fBaseDN: RawUtf8;
    fSearchWord: RawUtf8;
    fLog: TSynLog;

    /// Cache of GUID names for fast retrieve.
    GUIDName: TDocVariantData;

    /// Find name of a GUID.
    /// It looks into a name cache storage first, and if it not exists in storage,
    /// it looks into ldap configuration and then into ldap schema.
    /// It still not found, use the string representation of the guid.
    /// Argument:
    /// - guid(TGuid): Guid to convert.
    /// Return:
    /// Guid representation as RawUtf8.
    function FindGuidName(guid: TGuid): RawUtf8;

    /// Find the names of a TSecAccess array.
    /// Argument:
    /// - mask(TSecAccessMask): List of access to convert.
    /// Return:
    /// A RawUtf8 representation of the access list.
    function AccessNamesFromMask(mask: TSecAccessMask; Raw: Boolean): RawUtf8;

    /// Find the names of a TSecAceFlags.
    /// Argument:
    /// - flags(TSecAceFlags): List of flag to convert.
    /// Return:
    /// A RawUtf8 representation of the flag list.
    function AppliesToFromFlags(flags: TSecAceFlags; Raw: Boolean): RawUtf8;

    procedure UpdateGridACL();

    procedure ResolveOwnerAndGroup;
  public
    constructor Create(TheOwner: TComponent; _Ldap: TLdapClient;
      _SD: PSecurityDescriptor; _dn, abaseDN: RawUtf8); reintroduce;
  end;

const
  SEC_ACCESS: Array of TSecAccess = (
    samCreateChild,  // CC
    samDeleteChild,  // DC
    samListChildren, // LC
    samSelfWrite,    // SW
    samReadProp,     // RP
    samWriteProp,    // WP
    samDeleteTree,   // DT
    samListObject,   // LO
    samControlAccess,// CR
    samDelete,       // SD
    samReadControl,  // RC
    samWriteDAC,     // WD
    samWriteOwner    // WO
  );

  SAF_NAMES: array[TSecAceFlag] of String = (
    rsSafObjectInherit,    // 'OI' // safObjectInherit
    rsSafContainerInherit, // 'CI' // safContainerInherit
    rsSafNoPropagate,      // 'NP' // safNoPropagateInherit
    rsSafInheritOnly,      // 'IO' // safInheritOnly
    rsSafInherited,        // 'ID' // safInherited
    '',                    // '',  // saf5
    rsSafAuditSuccess,     // 'SA' // safSuccessfulAccess
    rsSafAuditFailure      // 'FA' // safFailedAccess
  );

    WELL_KNOWN_SID_NAMES: array[TWellKnownSid] of String = (
    rsWellKnownSidNull,                                            // wksNull,
    rsWellKnownSidWorld,                                           // wksWorld,
    rsWellKnownSidLocal,                                           // wksLocal,
    rsWellKnownSidConsoleLogon,                                    // wksConsoleLogon,
    rsWellKnownSidCreatorOwner,                                    // wksCreatorOwner,
    rsWellKnownSidCreatorGroup,                                    // wksCreatorGroup,
    rsWellKnownSidCreatorOwnerServer,                              // wksCreatorOwnerServer,
    rsWellKnownSidCreatorGroupServer,                              // wksCreatorGroupServer,
    rsWellKnownSidCreatorOwnerRights,                              // wksCreatorOwnerRights,
    rsWellKnownSidIntegrityUntrusted,                              // wksIntegrityUntrusted,
    rsWellKnownSidIntegrityLow,                                    // wksIntegrityLow,
    rsWellKnownSidIntegrityMedium,                                 // wksIntegrityMedium,
    rsWellKnownSidIntegrityMediumPlus,                             // wksIntegrityMediumPlus,
    rsWellKnownSidIntegrityHigh,                                   // wksIntegrityHigh,
    rsWellKnownSidIntegritySystem,                                 // wksIntegritySystem,
    rsWellKnownSidIntegrityProtectedProcess,                       // wksIntegrityProtectedProcess,
    rsWellKnownSidIntegritySecureProcess,                          // wksIntegritySecureProcess,
    rsWellKnownSidAuthenticationAuthorityAsserted,                 // wksAuthenticationAuthorityAsserted,
    rsWellKnownSidAuthenticationServiceAsserted,                   // wksAuthenticationServiceAsserted,
    rsWellKnownSidAuthenticationFreshKeyAuth,                      // wksAuthenticationFreshKeyAuth,
    rsWellKnownSidAuthenticationKeyTrust,                          // wksAuthenticationKeyTrust,
    rsWellKnownSidAuthenticationKeyPropertyMfa,                    // wksAuthenticationKeyPropertyMfa,
    rsWellKnownSidAuthenticationKeyPropertyAttestation,            // wksAuthenticationKeyPropertyAttestation,
    rsWellKnownSidNtAuthority,                                     // wksNtAuthority,
    rsWellKnownSidDialup,                                          // wksDialup,
    rsWellKnownSidNetwork,                                         // wksNetwork,
    rsWellKnownSidBatch,                                           // wksBatch,
    rsWellKnownSidInteractive,                                     // wksInteractive,
    rsWellKnownSidService,                                         // wksService,
    rsWellKnownSidAnonymous,                                       // wksAnonymous,
    rsWellKnownSidProxy,                                           // wksProxy,
    rsWellKnownSidEnterpriseControllers,                           // wksEnterpriseControllers,
    rsWellKnownSidSelf,                                            // wksSelf,
    rsWellKnownSidAuthenticatedUser,                               // wksAuthenticatedUser,
    rsWellKnownSidRestrictedCode,                                  // wksRestrictedCode,
    rsWellKnownSidTerminalServer,                                  // wksTerminalServer,
    rsWellKnownSidRemoteLogonId,                                   // wksRemoteLogonId,
    rsWellKnownSidThisOrganisation,                                // wksThisOrganisation,
    rsWellKnownSidIisUser,                                         // wksIisUser,
    rsWellKnownSidLocalSystem,                                     // wksLocalSystem,
    rsWellKnownSidLocalService,                                    // wksLocalService,
    rsWellKnownSidNetworkService,                                  // wksNetworkService,
    rsWellKnownSidLocalAccount,                                    // wksLocalAccount,
    rsWellKnownSidLocalAccountAndAdministrator,                    // wksLocalAccountAndAdministrator,
    rsWellKnownSidBuiltinDomain,                                   // wksBuiltinDomain,
    rsWellKnownSidBuiltinAdministrators,                           // wksBuiltinAdministrators,
    rsWellKnownSidBuiltinUsers,                                    // wksBuiltinUsers,
    rsWellKnownSidBuiltinGuests,                                   // wksBuiltinGuests,
    rsWellKnownSidBuiltinPowerUsers,                               // wksBuiltinPowerUsers,
    rsWellKnownSidBuiltinAccountOperators,                         // wksBuiltinAccountOperators,
    rsWellKnownSidBuiltinSystemOperators,                          // wksBuiltinSystemOperators,
    rsWellKnownSidBuiltinPrintOperators,                           // wksBuiltinPrintOperators,
    rsWellKnownSidBuiltinBackupOperators,                          // wksBuiltinBackupOperators,
    rsWellKnownSidBuiltinReplicator,                               // wksBuiltinReplicator,
    rsWellKnownSidBuiltinRasServers,                               // wksBuiltinRasServers,
    rsWellKnownSidBuiltinPreWindows2000CompatibleAccess,           // wksBuiltinPreWindows2000CompatibleAccess,
    rsWellKnownSidBuiltinRemoteDesktopUsers,                       // wksBuiltinRemoteDesktopUsers,
    rsWellKnownSidBuiltinNetworkConfigurationOperators,            // wksBuiltinNetworkConfigurationOperators,
    rsWellKnownSidBuiltinIncomingForestTrustBuilders,              // wksBuiltinIncomingForestTrustBuilders,
    rsWellKnownSidBuiltinPerfMonitoringUsers,                      // wksBuiltinPerfMonitoringUsers,
    rsWellKnownSidBuiltinPerfLoggingUsers,                         // wksBuiltinPerfLoggingUsers,
    rsWellKnownSidBuiltinAuthorizationAccess,                      // wksBuiltinAuthorizationAccess,
    rsWellKnownSidBuiltinTerminalServerLicenseServers,             // wksBuiltinTerminalServerLicenseServers,
    rsWellKnownSidBuiltinDcomUsers,                                // wksBuiltinDcomUsers,
    rsWellKnownSidBuiltinIUsers,                                   // wksBuiltinIUsers,
    rsWellKnownSidBuiltinCryptoOperators,                          // wksBuiltinCryptoOperators,
    rsWellKnownSidBuiltinUnknown,                                  // wksBuiltinUnknown,
    rsWellKnownSidBuiltinCacheablePrincipalsGroups,                // wksBuiltinCacheablePrincipalsGroups,
    rsWellKnownSidBuiltinNonCacheablePrincipalsGroups,             // wksBuiltinNonCacheablePrincipalsGroups,
    rsWellKnownSidBuiltinEventLogReadersGroup,                     // wksBuiltinEventLogReadersGroup,
    rsWellKnownSidBuiltinCertSvcDComAccessGroup,                   // wksBuiltinCertSvcDComAccessGroup,
    rsWellKnownSidBuiltinRdsRemoteAccessServers,                   // wksBuiltinRdsRemoteAccessServers,
    rsWellKnownSidBuiltinRdsEndpointServers,                       // wksBuiltinRdsEndpointServers,
    rsWellKnownSidBuiltinRdsManagementServers,                     // wksBuiltinRdsManagementServers,
    rsWellKnownSidBuiltinHyperVAdmins,                             // wksBuiltinHyperVAdmins,
    rsWellKnownSidBuiltinAccessControlAssistanceOperators,         // wksBuiltinAccessControlAssistanceOperators,
    rsWellKnownSidBuiltinRemoteManagementUsers,                    // wksBuiltinRemoteManagementUsers,
    rsWellKnownSidBuiltinDefaultSystemManagedGroup,                // wksBuiltinDefaultSystemManagedGroup,
    rsWellKnownSidBuiltinStorageReplicaAdmins,                     // wksBuiltinStorageReplicaAdmins,
    rsWellKnownSidBuiltinDeviceOwners,                             // wksBuiltinDeviceOwners,
    rsWellKnownSidBuiltinWriteRestrictedCode,                      // wksBuiltinWriteRestrictedCode,
    rsWellKnownSidBuiltinUserModeDriver,                           // wksBuiltinUserModeDriver,
    rsWellKnownSidCapabilityInternetClient,                        // wksCapabilityInternetClient,
    rsWellKnownSidCapabilityInternetClientServer,                  // wksCapabilityInternetClientServer,
    rsWellKnownSidCapabilityPrivateNetworkClientServer,            // wksCapabilityPrivateNetworkClientServer,
    rsWellKnownSidCapabilityPicturesLibrary,                       // wksCapabilityPicturesLibrary,
    rsWellKnownSidCapabilityVideosLibrary,                         // wksCapabilityVideosLibrary,
    rsWellKnownSidCapabilityMusicLibrary,                          // wksCapabilityMusicLibrary,
    rsWellKnownSidCapabilityDocumentsLibrary,                      // wksCapabilityDocumentsLibrary,
    rsWellKnownSidCapabilityEnterpriseAuthentication,              // wksCapabilityEnterpriseAuthentication,
    rsWellKnownSidCapabilitySharedUserCertificates,                // wksCapabilitySharedUserCertificates,
    rsWellKnownSidCapabilityRemovableStorage,                      // wksCapabilityRemovableStorage,
    rsWellKnownSidCapabilityAppointments,                          // wksCapabilityAppointments,
    rsWellKnownSidCapabilityContacts,                              // wksCapabilityContacts,
    rsWellKnownSidBuiltinAnyPackage,                               // wksBuiltinAnyPackage,
    rsWellKnownSidBuiltinAnyRestrictedPackage,                     // wksBuiltinAnyRestrictedPackage,
    rsWellKnownSidNtlmAuthentication,                              // wksNtlmAuthentication,
    rsWellKnownSidSChannelAuthentication,                          // wksSChannelAuthentication,
    rsWellKnownSidDigestAuthentication                             // wksDigestAuthentication,
  );

implementation

uses
  SysUtils,
  mormot.core.data,
  mormot.core.text,
  ucoredatamodule,
  ursatldapclient,
  uvisaddaces;

{$R *.lfm}


// Can be optimised, but it's fine
function GUIDsToNames(Ldap: TLdapClient; const GuidArr: array of TGuid): TRawUtf8DynArray;
var
  i: Integer;
  Filter, G: RawUtf8;
  res: TLdapResult;
  toSearch: array of RawUtf8;
begin
  result := [];
  SetLength(result, Length(GuidArr));

  toSearch := [];

  for i := 0 to High(GuidArr) do
  begin
    if IsNullGuid(GuidArr[i]) then
      result[i] := 'NULL'
    else
      result[i] := ATTR_TXT[UuidToKnownAttribute(GuidArr[i])]; // Find WellKnownGUID

    if result[i] <> '' then
      continue;
    result[i] := ToUtf8(GuidArr[i]); // Set Default Value
    Insert(result[i], toSearch, Length(toSearch)); // Add to Search
  end;

  // LDAP Search
  Filter := '|';
  for i := 0 to High(toSearch) do
    Filter += FormatUtf8('(schemaIDGUID=%)', [toSearch[i]]);
  if Filter = '|' then
    Exit; // Nothing to do
  Ldap.SearchScope := lssWholeSubtree;
  if not Ldap.Search(FormatUtf8('%,%', [CN_SCHEMA, Ldap.ConfigDN]), False, Filter, ['name', 'schemaIDGUID']) then
  begin
    ShowLdapSearchError(Ldap);
    Exit;
  end;
  for res in Ldap.SearchResult.Items do
  begin
    G := ToUtf8(PGuid(res.Attributes.Find('schemaIDGUID').GetRaw())^);
    for i := 0 to High(GuidArr) do // Find corresponding Guid
      if result[i] = G then
        result[i] := res.Attr[atName]; // Set new value
  end;

  Filter := '|';
  for i := 0 to High(toSearch) do
    Filter += FormatUtf8('(rightsGuid=%)', [toSearch[i]]);
  Ldap.SearchScope := lssWholeSubtree;
  if not Ldap.Search(FormatUtf8('%,%', [CN_EXTENDED_RIGHTS, Ldap.ConfigDN]), False, Filter, ['name', 'rightsGuid']) then
  begin
    ShowLdapSearchError(Ldap);
    Exit;
  end;
  for res in Ldap.SearchResult.Items do
  begin
    G := ToUtf8(PGuid(res.Attributes.Find('rightsGuid').GetRaw())^);
    for i := 0 to High(GuidArr) do // Find corresponding Guid
      if result[i] = G then
        result[i] := res.Attr[atName]; // Set new value
  end;
end;

function ACEsToInheritanceRoot(Ldap: TLdapClient; arr: PSecAcl; DN, fBaseDN: RawUtf8): TRawUtf8DynArray;
var
  Filter: RawUtf8;
  SecDescArr: array of TSecurityDescriptor;
  DNParentArr: Array of RawUtf8;
  res: TLdapResult;
  start, i, j, found: Integer;
  pairs: TNameValueDNs;
begin
  result := [];
  SetLength(result, Length(arr^));

  Filter := '|';

  while DN <> Ldap.DefaultDN(fBaseDN) do
  begin
    DN := GetParentDN(DN);
    Filter += FormatUtf8('(distinguishedName=%)', [DN]);
  end;

  // LDAP Search
  if Filter = '|' then
    Exit; // Nothing to do
  Ldap.SearchScope := lssWholeSubtree;
  if not Ldap.Search([atNTSecurityDescriptor], Filter) then
  begin
    ShowLdapSearchError(Ldap);
    Exit;
  end;

  SecDescArr  := []; // Sort & fill SecDescArr by level of inheritance (closest-first)
  DNParentArr := [];
  SetLength(SecDescArr,  Ldap.SearchResult.Count);
  SetLength(DNParentArr, Ldap.SearchResult.Count);
  ParseDN(Ldap.DefaultDN(fBaseDN), pairs); // Furthest Parent
  start := Length(pairs);
  for i := 0 to Ldap.SearchResult.Count - 1 do
  begin
    res := Ldap.SearchResult.Items[i];
    ParseDN(res.ObjectName, pairs);
    SecDescArr[High(SecDescArr) - (Length(pairs) - start)].FromBinary(res.Attributes.Find(atNTSecurityDescriptor).GetRaw());
    DNParentArr[High(SecDescArr) - (Length(pairs) - start)] := res.ObjectName;
  end;

  start := 0; // Skip all non-inherited
  while (start < Length(result)) and not (safInherited in arr^[start].Flags) do
  begin
    result[start] := '';
    Inc(start);
  end;

  for i := 0 to High(SecDescArr) do // Loop over parents
    for j := start to High(result) do // Loop over ACES
    begin
      found := SecDescFindACE(@SecDescArr[i], arr^[start].AceType, arr^[start].Sid, arr^[start].Mask,   @arr^[start].ObjectType, arr^[start].Flags - [safInherited], @arr^[start].InheritedObjectType); // Find non-inherited self in parent
      if found <> -1 then // Is oldest parent
      begin
        result[j] := DNParentArr[i]; // Set inherited DN
        start := j + 1; // Optimise loop
      end;
    end;
end;

{ TVisAdvancedSecurity }

constructor TVisAdvancedSecurity.Create(TheOwner: TComponent;
  _Ldap: TLdapClient; _SD: PSecurityDescriptor; _dn, abaseDN: RawUtf8);
var
  SearchResult: TLdapResult;
begin
  inherited Create(TheOwner);

  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Create', [Self.Name]);

  // Init
  Ldap := _Ldap;
  SecurityDescriptor := _SD;
  SampleSD := _SD^;
  DN := _dn;
  fBaseDN := aBaseDN;

  // Unify button width
  UnifyButtonsWidth([BitBtn_AddAce, BitBtn_DeleteAce, BitBtn_EditAce]);
  UnifyButtonsWidth([BitBtn_Cancel, BitBtn_Apply, BitBtn_OK]);

  // Update Grid
  UpdateGridACL();
end;

procedure TVisAdvancedSecurity.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TVisAdvancedSecurity.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    27: Close;
  end;
end;

procedure TVisAdvancedSecurity.Timer_SearchInGridTimer(Sender: TObject);
begin
  Timer_SearchInGrid.Enabled := True;
end;


procedure TVisAdvancedSecurity.TisGrid_ACLDblClick(Sender: TObject);
begin
  Action_EditAce.Execute;
end;

// CheckBox
procedure TVisAdvancedSecurity.CheckBox_RawChange(Sender: TObject);
begin
  CheckGroup_ACLFlags.Visible := CheckBox_Raw.Checked;
  TisGrid_ACL.Refresh();
end;

procedure TVisAdvancedSecurity.Action_AddAceExecute(Sender: TObject);
var
  add: TVisAddACEs;
begin
  add := TVisAddACEs.Create(self, Ldap, fBaseDN);
  try
    add.Caption := 'Add ACE';
    if (add.ShowModal() <> mrOK) or (Length(add.Acl) <= 0) then
    begin
      FreeAndNil(add);
      Exit;
    end;
    SecurityDescriptor^.Dacl := Concat(SecurityDescriptor^.Dacl, add.Acl);
    OrderAcl(Ldap, DN, fBaseDN, @SecurityDescriptor^.Dacl);
    UpdateGridACL();
  finally
    FreeAndNil(add);
  end;
end;

procedure TVisAdvancedSecurity.Action_ApplyExecute(Sender: TObject);
var
  attr: TLdapAttribute;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Execute', [Action_Apply.Caption]);

  if (mrYes <> MessageDlg(rsConfirmation, 'Are you sure you want to apply the changes?', mtWarning, [mbYes, mbNo], 0)) then
  begin
    if Assigned(fLog) then
      fLog.Log(sllInfo, '% - User cancel action.', [Action_Apply.Caption]);
    Exit;
  end;
  attr := TLdapAttribute.Create('nTSecurityDescriptor', atNTSecurityDescriptor);
  try
    attr.Add(SecurityDescriptor^.ToBinary);
    if not Ldap.Modify(dn, lmoReplace, attr) then
    begin
      if Assigned(fLog) then
        fLog.Log(sllError, '% - Ldap Modify Error: "%"', [Action_Apply.Caption, Ldap.ResultString]);
      ShowLdapModifyError(Ldap);
      Exit;
    end;
  finally
    FreeAndNil(attr);
  end;
  attr := Ldap.SearchObject(DN, '', 'nTSecurityDescriptor');
  if not Assigned(attr) then
  begin
    if Assigned(fLog) then
      fLog.Log(sllError, '% - Ldap Search Object Error: "%"', [Action_Apply.Caption, Ldap.ResultString]);
    ShowLdapSearchError(Ldap);
    Exit;
  end;
  SecurityDescriptor^.FromBinary(attr.GetRaw());
  SampleSD := SecurityDescriptor^;
  UpdateGridACL();
end;

procedure TVisAdvancedSecurity.Action_ApplyUpdate(Sender: TObject);
begin
  Action_Apply.Enabled := not SecurityDescriptor^.IsEqual(SampleSD);
end;

procedure TVisAdvancedSecurity.Action_CancelExecute(Sender: TObject);
begin
  if (not Action_Apply.Enabled) then
    Exit;
  if (mrYes <> MessageDlg(rsConfirmation, 'Are you sure you want to cancel the changes?', mtWarning, [mbYes, mbNo], 0)) then
  begin
    ModalResult := mrNone;
    Exit;
  end;
  SecurityDescriptor^ := SampleSD;
end;

procedure TVisAdvancedSecurity.Action_DeleteAceExecute(Sender: TObject);
var
  Node: PVirtualNode;
  NodesIndex: Array of Integer;
  Index: Integer;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Execute', [Action_DeleteAce.Caption]);

  Node := TisGrid_ACL.GetFirstSelected();
  NodesIndex := [];
  while Assigned(Node) do
  begin
    Insert(Node^.Index, NodesIndex, 0);
    Node := TisGrid_ACL.GetNextSelected(Node);
  end;
  for Index in NodesIndex do
    Delete(SecurityDescriptor^.Dacl, Index, 1);

  TisGrid_ACL.DeleteSelectedNodes;
end;

procedure TVisAdvancedSecurity.Action_DeleteAceUpdate(Sender: TObject);
begin
  Action_DeleteAce.Enabled := (TisGrid_ACL.SelectedCount > 0);
end;

procedure TVisAdvancedSecurity.Action_EditAceExecute(Sender: TObject);
var
  add: TVisAddACEs;
  data: TDocVariantData;
  pdata: PDocVariantData;
  value: Integer;
  _name, DistinguishedName: RawUtf8;
  SearchResult: TLdapResult;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Execute', [Action_EditAce.Caption]);

  pdata := TisGrid_ACL.GetNodeAsPDocVariantData(TisGrid_ACL.GetFirstSelected());
  if not Assigned(pdata) then
  begin
    if Assigned(fLog) then
      fLog.Log(sllWarning, '% - Cannot get first selected row', [Action_EditAce.Caption]);
    Exit;
  end;

  assert(Assigned(pdata), 'No focused node.');
  assert(pdata^.Exists('_type'), 'Focused node does not have type.');
  assert(pdata^.Exists('_sid'), 'Focused node does not have sid.');
  assert(pdata^.Exists('_mask'), 'Focused node does not have mask.');
  assert(pdata^.Exists('_flags'), 'Focused node does not have flags.');
  assert(pdata^.Exists('_objectType'), 'Focused node does not have objectType.');
  assert(pdata^.Exists('_iObjectType'), 'Focused node does not have iObjectType.');

  add := TVisAddACEs.Create(self, Ldap, fBaseDN);
  try
    data.Init([]);

    add.Caption := 'Edit ACE';
    // Get type
    value := pdata^.I['_type'];
    if (TSecAceType(value) in [satObjectAccessAllowed, satAccessAllowed]) then
      data.AddOrUpdateValue('type', 0)
    else if (TSecAceType(value) in [satObjectAccessDenied, satAccessDenied]) then
      data.AddOrUpdateValue('type', 1)
    else
    begin
      if Assigned(fLog) then
        fLog.Log(sllError, '% - Invalid ace value type.', [Action_EditAce.Caption]);
      Exit;
    end;

    // Get sid
    // Retrieve from domain, if not retrieve from known sid
    Ldap.SearchBegin();
    try
      Ldap.SearchScope := lssWholeSubtree;
      repeat
        if not Ldap.Search(Ldap.DefaultDN(fBaseDN), False, FormatUtf8('objectSid=%', [LdapEscape(pdata^.S['_sid'])]), ['distinguishedName']) then
        begin
          if Assigned(fLog) then
            fLog.Log(sllError, '% - Ldap Search Error: "%"', [Action_EditAce.Caption, Ldap.ResultString]);
          ShowLdapSearchError(Ldap);
          Exit;
        end;

        for SearchResult in Ldap.SearchResult.Items do
        begin
          if not Assigned(SearchResult) then
            Continue;
          DistinguishedName := SearchResult.Find('distinguishedName').GetReadable();
        end;
      until Ldap.SearchCookie = '';
    finally
      Ldap.SearchEnd;
    end;

    Ldap.SearchBegin();
    try
      Ldap.SearchScope := lssWholeSubtree;
      repeat
        if not Ldap.Search(FormatUtf8('CN=WellKnown Security Principals,%', [Ldap.ConfigDN]), False, FormatUtf8('objectSid=%', [LdapEscape(pdata^.S['_sid'])]), ['distinguishedName']) then
        begin
          if Assigned(fLog) then
            fLog.Log(sllTrace, '% - Ldap Search Error: "%"', [Action_EditAce.Caption, Ldap.ResultString]);
          ShowLdapSearchError(Ldap);
          Exit;
        end;

        for SearchResult in Ldap.SearchResult.Items do
        begin
          if not Assigned(SearchResult) then
            Continue;
          DistinguishedName := SearchResult.Find('distinguishedName').GetReadable();
        end;
      until Ldap.SearchCookie = '';

    finally
      Ldap.SearchEnd;
    end;
    assert(Ldap.SearchResult.Count = 1, FormatUtf8('Invalid result number (%).', [Ldap.SearchResult.Count]));
    assert(Assigned(Ldap.SearchResult.Items[0].Find('distinguishedName')), 'Cannot find attribute (distinguishedName).');
    data.AddOrUpdateValue('sid', DistinguishedName);

    // Get mask
    data.AddOrUpdateValue('mask', pdata^.I['_mask']);

    // Get flags
    data.AddOrUpdateValue('flags', pdata^.I['_flags']);

    // Get objectType
    if not IsNullGuid(RawUtf8ToGuid(pdata^.S['_objectType'])) then
    begin
      _name := Ldap.SearchObject(FormatUtf8('CN=Schema,%', [Ldap.ConfigDN]), FormatUtf8('(schemaIDGUID=%)', [LdapEscape(pdata^.S['_objectType'])]), 'lDAPDisplayName', lssWholeSubtree).GetReadable();
      if _name = '' then
        _name := Ldap.SearchObject(FormatUtf8('%,%', [CN_EXTENDED_RIGHTS, Ldap.ConfigDN]), FormatUtf8('rightsGuid=%', [LdapEscape(pdata^.S['_objectType'])]), 'displayName', lssWholeSubtree).GetReadable();
      data.AddOrUpdateValue('objectType', _name);
    end;

    // Get iObjectType
    if not IsNullGuid(RawUtf8ToGuid(pdata^.S['_iObjectType'])) then
    begin
      _name := Ldap.SearchObject(FormatUtf8('CN=Schema,%', [Ldap.ConfigDN]), FormatUtf8('(schemaIDGUID=%)', [LdapEscape(pdata^.S['_iObjectType'])]), 'lDAPDisplayName', lssWholeSubtree).GetReadable();
      data.AddOrUpdateValue('iObjectType', _name);
    end;

    // Add object to grid
    add.TisGrid1.BeginUpdate;
    add.TisGrid1.Data.AddItem(data);
    add.TisGrid1.EndUpdate;
    add.TisGrid1.LoadData();
    add.BitBtn_Add.Action := Nil;
    add.BitBtn_Add.Enabled := False;

    if add.ShowModal() <> mrOK then
    begin
      if Assigned(fLog) then
        fLog.Log(sllInfo, '% - User cancel action', [Action_EditAce.Caption]);
      Exit;
    end;
    Delete(SecurityDescriptor^.Dacl, TisGrid_ACL.FocusedNode^.Index, 1);
    SecurityDescriptor^.Dacl := Concat(SecurityDescriptor^.Dacl, add.Acl);
    OrderAcl(Ldap, DN, fBaseDN, @SecurityDescriptor^.Dacl);
    UpdateGridACL();
  finally
    FreeAndNil(add);
  end;
end;

procedure TVisAdvancedSecurity.Action_EditAceUpdate(Sender: TObject);
begin
  Action_EditAce.Enabled := (TisGrid_ACL.SelectedCount = 1);
end;

procedure TVisAdvancedSecurity.Action_OKExecute(Sender: TObject);
begin
  if Action_Apply.Enabled then
    Action_ApplyExecute(Sender);
end;

procedure TVisAdvancedSecurity.Action_RestoreExecute(Sender: TObject);
var
  sdText: RawUtf8;
  sd: TSecurityDescriptor;
  ObjectClass, Attribute: TLdapAttribute;
  ace: TSecAce;
  i: Integer;
  SearchResult: TLdapResult;
  ObjectClassFilter: String;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Execute', [Action_Restore.Caption]);

  ObjectClass := Ldap.SearchObject(DN, '', 'objectClass');
  if not Assigned(ObjectClass) then
  begin
    if Assigned(fLog) then
      fLog.Log(sllError, '% - Ldap Search Object Error: "%"', [Action_Restore.Caption, Ldap.ResultString]);
    ShowLdapSearchError(Ldap);
    Exit;
  end;

  SecurityDescriptor^.Flags := [scDaclAutoInheritReq, scOwnerDefaulted, scGroupDefaulted, scDaclAutoInherit, scSelfRelative, scDaclPresent];
  SecurityDescriptor^.Dacl := [];


  ObjectClassFilter := '(|';
  for i := 0 to objectClass.Count - 1 do
    ObjectClassFilter := Format('%s(lDAPDisplayName=%s)', [ObjectClassFilter, LdapEscape(ObjectClass.GetReadable(i))]);

  if ObjectClassFilter = '(|' then
    ObjectClassFilter := ''
  else
    ObjectClassFilter := Format('%s)', [ObjectClassFilter]);

  Ldap.SearchBegin();
  try
    Ldap.SearchScope := lssSingleLevel;

    repeat
      if not Ldap.Search(Format('CN=Schema,%s', [Ldap.ConfigDN]), False, ObjectClassFilter, ['defaultSecurityDescriptor']) then
      begin
        if Assigned(fLog) then
          fLog.Log(sllError, '% - Ldap Search Error: "%"', [Action_Restore.Caption, Ldap.ResultString]);
        ShowLdapSearchError(Ldap);
        Exit;
      end;

      for SearchResult in Ldap.SearchResult.Items do
      begin
        if not Assigned(SearchResult) then
          continue;

        Attribute := SearchResult.Find('defaultSecurityDescriptor');
        if not Assigned(Attribute) then
          continue;
        sdText := Attribute.GetReadable();
        sd.FromText(sdText, RawSidToText(Ldap.DomainSid));
        for ace in sd.Dacl do
        begin
          if not AceInDacl(SecurityDescriptor^.Dacl, ace) then
            Insert(ace, SecurityDescriptor^.Dacl, 0);
        end;
      end;
    until Ldap.SearchCookie = '';
  finally
    Ldap.SearchEnd;
  end;
  OrderAcl(Ldap, DN, fBaseDN, @SecurityDescriptor^.Dacl);
  UpdateGridACL;
end;

// TisGrid
procedure TVisAdvancedSecurity.TisGrid_ACLGetText(aSender: TBaseVirtualTree;
  aNode: PVirtualNode; const aCell: TDocVariantData; aColumn: TColumnIndex;
  aTextType: TVSTTextType; var aText: string);
var
  PropName: RawUtf8;
begin
  if CheckBox_Raw.Checked then
    PropName := 'Raw_' + TisGrid_ACL.FindColumnByIndex(aColumn).PropertyName
  else
    PropName := 'Hum_' + TisGrid_ACL.FindColumnByIndex(aColumn).PropertyName;

  aText := aCell.S[PropName];
  case TisGrid_ACL.FindColumnByIndex(aColumn).PropertyName of
  'iObjectType':
  begin
    if CheckBox_Raw.Checked then
    begin
      if aText = '00000000-0000-0000-0000-000000000000' then
        aText := '';
    end
    else
    begin
      if aText = 'NULL' then
        aText := '';
    end;
  end;
  'objectType':
    if not CheckBox_Raw.Checked and (aText = 'NULL') then
      aText := '';
  end;
end;

procedure TVisAdvancedSecurity.TisGrid_ACLKeyPress(Sender: TObject;
  var Key: char);
begin
  SearchInGrid(Timer_SearchInGrid, TisGrid_ACL, fSearchWord, Key);
end;

procedure TVisAdvancedSecurity.TisGrid_ACLGetImageIndex(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
begin
  if TisGrid_ACL.FindColumnByIndex(Column).PropertyName <> 'type' then
    Exit;
  case TisGrid_ACL.GetNodeAsPDocVariantData(Node)^.S['Raw_type'] of
  'A', 'OA':
    ImageIndex := Ord(ileValid);
  'D', 'OD':
    ImageIndex := Ord(ileToolDelete);
  end;
end;

function TVisAdvancedSecurity.FindGuidName(guid: TGuid): RawUtf8;
var
  sguid: String;
begin
  result := '';

  if IsNullGuid(guid) then
    Exit;

  sguid := guid.ToString(true);

  if not GUIDName.Exists(sguid) then
  begin
    /// Search in Domain Configuration
    GUIDName.S[sguid] := Ldap.SearchObject(Ldap.ConfigDN, Format('(rightsGuid=%s)', [sguid]), 'displayName', lssWholeSubtree).GetReadable();
    if GUIDName.S[sguid] = '' then
      /// Search in Domain Schema
      GUIDName.S[sguid] := Ldap.SearchObject(FormatUtf8('%,%', [CN_SCHEMA, Ldap.ConfigDN]), Format('(schemaIDGUID=%s)', [sguid]), 'lDAPDisplayName', lssWholeSubtree).GetReadable();
    if GUIDName.S[sguid] = '' then
      /// guid as string
      GUIDName.S[sguid] := sguid;
  end;
  result := GUIDName.S[sguid];
end;

function TVisAdvancedSecurity.AccessNamesFromMask(mask: TSecAccessMask; Raw: Boolean): RawUtf8;
var
  m: TSecAccess;
begin
  result := '';

  if not Raw then
  begin
    // Full control
    if mask >= [samCreateChild, samDeleteChild, samListChildren, samSelfWrite, samReadProp, samWriteProp,
      samDeleteTree, samListObject,samControlAccess, samDelete, samReadControl, samWriteDac, samWriteOwner] then
    begin
      result := rsSecAccessFullControl;
      mask -= [samCreateChild, samDeleteChild, samListChildren, samSelfWrite, samReadProp, samWriteProp,
        samDeleteTree, samListObject, samControlAccess, samDelete, samReadControl, samWriteDac, samWriteOwner];
    end;

    // List content
    if mask >= [samListChildren, samListObject] then
    begin
      result := rsSecAccessListContent;
      mask -= [samListChildren, samListObject];
    end;
  end;

  for m in mask do
    if result = '' then
      if Raw then
        result := SAM_SDDL[m]
      else
        result := SEC_ACCESS_NAMES[m]
    else
      if Raw then
        result := FormatUtf8('%, %', [result, SAM_SDDL[m]])
      else
        result := FormatUtf8('%, %', [result, SEC_ACCESS_NAMES[m]]);
end;

function TVisAdvancedSecurity.AppliesToFromFlags(flags: TSecAceFlags; Raw: Boolean): RawUtf8;
var
  f: TSecAceFlag;
begin
  result := '';

  if Raw then
    for f in flags do
      if result = '' then
        result := SAF_SDDL[f]
      else
        result := FormatUtf8('%, %', [result, SAF_SDDL[f]])
  else
  begin
    if safNoPropagateInherit in flags then
        result := rsFlagDirectDesendants
    else if (safObjectInherit in flags) or (safContainerInherit in flags) then
        result := rsFlagAllDescendants;

    if not (safInheritOnly in flags) then
      if result = '' then
        result := rsFlagThisObject
      else
        result := FormatUtf8(rsAnd, [rsFlagThisObject, result]);

    if result <> '' then
      result[1] := UpCase(result[1]); // propper upper casing
  end;
end;

procedure TVisAdvancedSecurity.UpdateGridACL;
var
  i: Integer;
  ace: PSecAce;
  rowAce, Data, SidCache: TDocVariantData;
  HumanReadableData: TRawUtf8DynArray;
  SidArr: array of RawUtf8;
  GuidArr: array of TGuid;

  procedure ResolveSid(PSidCache: PDocVariantData; SecDesc: PSecurityDescriptor; LdapClient: TLdapClient);
  var
    Filter: String;
    idx: Integer;
    SearchResult: TLdapResult;

  begin
    if Assigned(fLog) then
      fLog.Log(sllTrace, '% - Resolve SIDs', [Self.Name]);
    Filter := '(|';

    // Owner filter
    if SidToKnown(PSid(SecDesc^.Owner)) <> wksNull then
      PSidCache^.S[RawSidToText(SecDesc^.Owner)] := WELL_KNOWN_SID_NAMES[SidToKnown(PSid(SecDesc^.Owner))]
    else
      Filter := FormatUtf8('%(objectSid=%)', [Filter, RawSidToText(SecDesc^.Owner)]);

    // Group filter
    if SidToKnown(PSid(SecDesc^.Group)) <> wksNull then
      PSidCache^.S[RawSidToText(SecDesc^.Group)] := WELL_KNOWN_SID_NAMES[SidToKnown(PSid(SecDesc^.Group))]
    else
      Filter := FormatUtf8('%(objectSid=%)', [Filter, RawSidToText(SecDesc^.Group)]);

    // ACL filter
    for idx := 0 to High(SecDesc^.Dacl) do
    begin
      if SidToKnown(PSid(SecDesc^.Dacl[idx].Sid)) <> wksNull then
      begin
        PSidCache^.S[RawSidToText(SecDesc^.Dacl[idx].Sid)] := WELL_KNOWN_SID_NAMES[SidToKnown(PSid(SecDesc^.Dacl[idx].Sid))];
        continue;
      end;
      Filter := FormatUtf8('%(objectSid=%)', [Filter, RawSidToText(SecDesc^.Dacl[idx].Sid)]);
    end;
    Filter := FormatUtf8('%)', [Filter]);

    // Resolve SIDs
    LdapClient.SearchBegin();
    try
      LdapClient.SearchScope := lssWholeSubtree;

      repeat
        if not LdapClient.Search(LdapClient.DefaultDN(), False, Filter, ['name', 'objectSid']) then
        begin
          if Assigned(fLog) then
            fLog.Log(sllError, '% - Ldap Search Error: "%"', [Self.Name, Ldap.ResultString]);
          ShowLdapSearchError(LdapClient);
          Exit;
        end;

        for SearchResult in LdapClient.SearchResult.Items do
        begin
          if not Assigned(SearchResult) then
            continue;
          PSidCache^.S[SearchResult.Find('objectSid').GetReadable()] := SearchResult.Find('name').GetReadable();
        end;
      until LdapClient.SearchCookie = '';
    finally
      LdapClient.SearchEnd;
    end;

    LdapClient.SearchBegin();
    try
      LdapClient.SearchScope := lssWholeSubtree;

      repeat
        if not LdapClient.Search(LdapClient.ConfigDN, False, Filter, ['name', 'objectSid']) then
        begin
          if Assigned(fLog) then
            fLog.Log(sllError, '% - Ldap Search Error: "%"', [Self.Name, Ldap.ResultString]);
          ShowLdapSearchError(LdapClient);
          Exit;
        end;

        for SearchResult in LdapClient.SearchResult.Items do
        begin
          if not Assigned(SearchResult) then
            continue;
          PSidCache^.S[SearchResult.Find('objectSid').GetReadable()] := SearchResult.Find('name').GetReadable();
        end;
      until LdapClient.SearchCookie = '';
    finally
      LdapClient.SearchEnd;
    end;
  end;

begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Update Grid ACL', [Self.Name]);

  SidArr := [];
  SetLength(SidArr, Length(SecurityDescriptor^.Dacl));
  GuidArr := [];
  SetLength(GuidArr, 2 * Length(SecurityDescriptor^.Dacl)); // obj type + inherited

  TisGrid_ACL.BeginUpdate();
  try
    Data.InitArray([]);
    SidCache.Init();
    ResolveSid(@SidCache, SecurityDescriptor, Ldap);

    if SidCache.Exists(RawSidToText(SecurityDescriptor^.Group)) then
      Edit_Group.Text := SidCache.S[RawSidToText(SecurityDescriptor^.Group)]
    else
      Edit_Group.Text := RawSidToText(SecurityDescriptor^.Group);

    if SidCache.Exists(RawSidToText(SecurityDescriptor^.Owner)) then
      Edit_Owner.Text := SidCache.S[RawSidToText(SecurityDescriptor^.Owner)]
    else
      Edit_Owner.Text := RawSidToText(SecurityDescriptor^.Owner);

    for i := 0 to High(SecurityDescriptor^.Dacl) do
    begin
      ace := @SecurityDescriptor^.Dacl[i];
      // Raw
      rowAce.Init([dvoCheckForDuplicatedNames, dvoReturnNullForUnknownProperty]);
      rowAce.AddValue('_type', Ord(ace^.AceType));
      rowAce.AddValue('_sid', RawSidToText(ace^.Sid));
      rowAce.AddValue('_mask', Integer(ace^.Mask));
      rowAce.AddValue('_flags', Int8(ace^.Flags));
      rowAce.AddValue('_objectType', ace^.ObjectType.ToString(True));
      rowAce.AddValue('_iObjectType', ace^.InheritedObjectType.ToString(True));
      rowAce.AddValue('Raw_type',        SAT_SDDL[ace^.AceType]);
      rowAce.AddValue('Raw_sid',         ace^.SidText());
      rowAce.AddValue('Raw_mask',        AccessNamesFromMask(ace^.Mask, True));
      rowAce.AddValue('Raw_flags',       AppliesToFromFlags(ace^.flags, True));
      rowAce.AddValue('Raw_objectType',  ace^.ObjectType.ToString());
      //rowAce.AddValue('Raw_iFrom',       'None');
      rowAce.AddValue('Raw_iObjectType', ace^.InheritedObjectType.ToString());
      // Human-readable
      case ace^.AceType of
      satAccessAllowed, satObjectAccessAllowed:
        rowAce.AddValue('Hum_type', rsAllow);
      satAccessDenied, satObjectAccessDenied:
        rowAce.AddValue('Hum_type', rsDeny);
      else
        rowAce.AddValue('Hum_type', SAT_SDDL[ace^.AceType]);
      end;
      SidArr[i] := RawSidToText(ace^.Sid);
      rowAce.AddValue('Hum_mask',        AccessNamesFromMask(ace^.Mask, False));
      rowAce.AddValue('Hum_flags',       AppliesToFromFlags(ace^.flags, False));
      GuidArr[i] := ace^.ObjectType;
      GuidArr[Length(SecurityDescriptor^.Dacl) + i] := ace^.InheritedObjectType;
      // Add
      Data.AddItem(rowAce);
      rowAce.clear();
    end;
    // Finalise Human-readable data
    // SID
    //HumanReadableData := VisMain.Storage.SidCache.SidsNamesFromSids(SidArr);
    for i := 0 to Data.Count - 1 do
      if SidCache.Exists(Data._[i]^.S['_sid']) then
        Data._[i]^.AddValue('Hum_sid', SidCache.S[Data._[i]^.S['_sid']]);
    // GUID
    HumanReadableData := GUIDsToNames(Ldap, GuidArr);
    for i := 0 to high(HumanReadableData) div 2 do
    begin
      Data._[i]^.AddValue('Hum_objectType',  HumanReadableData[i]);
      Data._[i]^.AddValue('Hum_iObjectType', HumanReadableData[Length(HumanReadableData) div 2 + i]);
    end;
    // Inherited From
    HumanReadableData := ACEsToInheritanceRoot(Ldap, @SecurityDescriptor^.Dacl, DN, fBaseDN);
    for i := 0 to High(HumanReadableData) do
    begin
      Data._[i]^.AddValue('Hum_iFrom', HumanReadableData[i]);
      Data._[i]^.AddValue('Raw_iFrom', HumanReadableData[i]);
    end;

    TisGrid_ACL.Data := Data;
  finally
    TisGrid_ACL.EndUpdate();
  end;

  CheckBox_SR.Checked := scSelfRelative in SecurityDescriptor^.Flags;
  CheckBox_RM.Checked := scRmControlValid in SecurityDescriptor^.Flags;
  CheckBox_PS.Checked := scSaclProtected in SecurityDescriptor^.Flags;
  CheckBox_PD.Checked := scDaclProtected in SecurityDescriptor^.Flags;
  CheckBox_SI.Checked := scSaclAutoInherit in SecurityDescriptor^.Flags;
  CheckBox_DI.Checked := scDaclAutoInherit in SecurityDescriptor^.Flags;
  CheckBox_SC.Checked := scSaclAutoInheritReq in SecurityDescriptor^.Flags;
  CheckBox_DC.Checked := scDaclAutoInheritReq in SecurityDescriptor^.Flags;
  CheckBox_SS.Checked := scServerSecurity in SecurityDescriptor^.Flags;
  CheckBox_DT.Checked := scDaclTrusted in SecurityDescriptor^.Flags;
  CheckBox_SD.Checked := scSaclDefaulted in SecurityDescriptor^.Flags;
  CheckBox_SP.Checked := scSaclPresent in SecurityDescriptor^.Flags;
  CheckBox_DD.Checked := scDaclDefaulted in SecurityDescriptor^.Flags;
  CheckBox_DP.Checked := scDaclPresent in SecurityDescriptor^.Flags;
  CheckBox_GD.Checked := scGroupDefaulted in SecurityDescriptor^.Flags;
  CheckBox_OD.Checked := scOwnerDefaulted in SecurityDescriptor^.Flags;
end;

procedure TVisAdvancedSecurity.ResolveOwnerAndGroup;
var
  SearchResult: TLdapResult;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Resolve Owner and Group SIDs.', [Self.Name]);

  Ldap.SearchBegin();
  try
    if not Ldap.Search(Ldap.DefaultDN(), False, FormatUtf8('(|(objectSid=%)(objectSid=%))', [RawSidToText(SecurityDescriptor^.Owner), RawSidToText(SecurityDescriptor^.Group)]), ['name', 'objectSid']) then
    begin
      if Assigned(fLog) then
        fLog.Log(sllError, '% - Ldap Search Error: "%"', [Self.Name, Ldap.ResultString]);
      ShowLdapSearchError(Ldap);
      Exit;
    end;

    for SearchResult in Ldap.SearchResult.Items do
    begin
      if not Assigned(SearchResult) then
        continue;
      if (SearchResult.Find('objectSid').GetRaw() = SecurityDescriptor^.Owner) then
        Edit_Owner.Text := SearchResult.Find('name').GetReadable();

      if (SearchResult.Find('objectSid').GetRaw() = SecurityDescriptor^.Group) then
        Edit_Group.Text := SearchResult.Find('name').GetReadable();
    end;
  finally
    Ldap.SearchEnd;
  end;
end;

end.

