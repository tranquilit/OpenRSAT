unit ucommon;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  controls,
  ExtCtrls,
  mormot.core.base,
  mormot.net.ldap,
  mormot.core.os.security,
  mormot.core.variants,
  tis.ui.grid.core,
  VirtualTrees;

type
  PSecurityDescriptor = ^TSecurityDescriptor;

resourcestring
  // Ldap SDDL names
  rsSecAccessCreateChild    = 'Create child';
  rsSecAccessDeleteChild    = 'Delete child';
  rsSecAccessListChildren   = 'List Children';
  rsSecAccessSelfWrite      = 'Self Write';
  rsSecAccessReadProp       = 'Read';  // Read Prop
  rsSecAccessWriteProp      = 'Write'; // Write Prop
  rsSecAccessDeleteTree     = 'Delete Tree';
  rsSecAccessListObject     = 'List Object';
  rsSecAccessControlAccess  = 'Control Access';
  rsSecAccessDelete         = 'Delete';
  rsSecAccessReadControl    = 'Read Control';
  rsSecAccessWriteDac       = 'Write Dac';
  rsSecAccessWriteOwner     = 'Write Owner';
  rsSecAccessSynchronize    = 'Synchronize';
  rsSecAccessGenericAll     = 'Generic All';
  rsSecAccessGenericExecute = 'Generic Execute';
  rsSecAccessGenericWrite   = 'Generic Write';
  rsSecAccessGenericRead    = 'Generic Read';

  rsConfirmation = 'Confirmation';
  rsWarning = 'Warning';
  rsError = 'Error';
  rsDelete = 'Delete';
  rsRename = 'Rename';

  rsLdapError = 'Ldap Error';
  rsLdapAddFailed = 'Ldap add failed: "%"';
  rsLdapDeleteFailed = 'Ldap delete failed: "%"';
  rsLdapConnectFailed = 'Ldap connect failed: "%"';
  rsLdapSearchFailed = 'Ldap search failed: "%"';
  rsLdapModifyFailed = 'Ldap modify failed: "%"';

  rsInsufficientAccessRights = 'Insufficient rights access.';
  rsConstraintViolation = 'The requests violates some constraint defined within the server.';

  rsACEpaadParent = 'A Deny Delete-Tree right will be added to the ACL of the parent of this object. Do you wish to continue?';

  rsDeleteConfirm = 'Are you sure you want to delete the following object: %';

  rsTitleSelectOwner = 'Select owner';

  rsNewConfirmPassDifferent = 'The New and Confirm passwords must match. Please re-type them.';

  // New Objects
  rsNewObjectComputer = 'New object - Computer';
  rsNewObjectContact = 'New object - Contact';
  rsNewObjectGroup = 'New object - Group';
  rsNewObjectOU = 'New object - OU';
  rsNewObjectSite = 'New object - Site';
  rsNewObjectSharedFolder = 'New object - Shared Folder';
  rsNewObjectSubnet = 'New object - Subnet';
  rsNewObjectUser = 'New object - User';

  rsNewObjectBtnBack = '< Back';
  rsNewObjectBtnNext = 'Next >';
  rsNewObjectBtnOK = 'OK';

  rsNewUserFullName = 'Full name: %';
  rsNewUserLogonName = 'User logon name: %';
  rsNewUserChangePassword = 'The user will have to change his password at next logon.';
  rsNewUserNOChangePassword = 'The user cannot change his password.';
  rsNewUserPasswordNoExpire = 'The Password never expires.';
  rsNewUserDisabled = 'The account is disabled.';

  rsTitleParsing = 'Parsing error';
  rsACEParsing = 'Failed to parse ACL.';

  rsLdapMoveWarningMessage = 'Moving objects in Active Directory Domain Services can prevent your existing system from working the way it was designed. For exemple, moving an organizational unit (OU) can affect the way that group policies are applied to the accounts within the OU.' + LineEnding +
  'Are you sure you want to move this object?';

  // Well Known Sid
  rsWellKnownSidNull = '';
  rsWellKnownSidWorld = 'Everyone';
  rsWellKnownSidLocal = 'Local';
  rsWellKnownSidConsoleLogon = 'Console Logon';
  rsWellKnownSidCreatorOwner = 'Creator Owner';
  rsWellKnownSidCreatorGroup = 'Creator Group';
  rsWellKnownSidCreatorOwnerServer = 'Creator Owner Server';
  rsWellKnownSidCreatorGroupServer = 'Creator Group Server';
  rsWellKnownSidCreatorOwnerRights = 'Creator Owner Rights';
  rsWellKnownSidIntegrityUntrusted = 'Integrity Untrusted';
  rsWellKnownSidIntegrityLow = 'Integrity Low';
  rsWellKnownSidIntegrityMedium = 'Integrity Medium';
  rsWellKnownSidIntegrityMediumPlus = 'Integrity Medium Plus';
  rsWellKnownSidIntegrityHigh = 'Integrity High';
  rsWellKnownSidIntegritySystem = 'Integrity System';
  rsWellKnownSidIntegrityProtectedProcess = 'Integrity Protected Process';
  rsWellKnownSidIntegritySecureProcess = 'Integrity Secure Process';
  rsWellKnownSidAuthenticationAuthorityAsserted = 'Authentication Authority Asserted';
  rsWellKnownSidAuthenticationServiceAsserted = 'Authentication Service Asserted';
  rsWellKnownSidAuthenticationFreshKeyAuth = 'Authentication Fresh Key Auth';
  rsWellKnownSidAuthenticationKeyTrust = 'Authentication Key Trust';
  rsWellKnownSidAuthenticationKeyPropertyMfa = 'Authentication Key Property Mfa';
  rsWellKnownSidAuthenticationKeyPropertyAttestation = 'Authentication Key Property Attestation';
  rsWellKnownSidNtAuthority = 'Nt Authority';
  rsWellKnownSidDialup = 'Dialup';
  rsWellKnownSidNetwork = 'Network';
  rsWellKnownSidBatch = 'Batch';
  rsWellKnownSidInteractive = 'Interactive';
  rsWellKnownSidService = 'Service';
  rsWellKnownSidAnonymous = 'Anonymous';
  rsWellKnownSidProxy = 'Proxy';
  rsWellKnownSidEnterpriseControllers = 'Enterprise Controllers';
  rsWellKnownSidSelf = 'Self';
  rsWellKnownSidAuthenticatedUser = 'Authenticated User';
  rsWellKnownSidRestrictedCode = 'Restricted Code';
  rsWellKnownSidTerminalServer = 'Terminal Server';
  rsWellKnownSidRemoteLogonId = 'Remote Logon Id';
  rsWellKnownSidThisOrganisation = 'This Organisation';
  rsWellKnownSidIisUser = 'Iis User';
  rsWellKnownSidLocalSystem = 'Local System';
  rsWellKnownSidLocalService = 'Local Service';
  rsWellKnownSidNetworkService = 'Network Service';
  rsWellKnownSidLocalAccount = 'Local Account';
  rsWellKnownSidLocalAccountAndAdministrator = 'Local Account And Administrator';
  rsWellKnownSidBuiltinDomain = 'Builtin Domain';
  rsWellKnownSidBuiltinAdministrators = 'Builtin Administrators';
  rsWellKnownSidBuiltinUsers = 'Builtin Users';
  rsWellKnownSidBuiltinGuests = 'Builtin Guests';
  rsWellKnownSidBuiltinPowerUsers = 'Builtin Power Users';
  rsWellKnownSidBuiltinAccountOperators = 'Builtin Account Operators';
  rsWellKnownSidBuiltinSystemOperators = 'Builtin System Operators';
  rsWellKnownSidBuiltinPrintOperators = 'Builtin Print Operators';
  rsWellKnownSidBuiltinBackupOperators = 'Builtin Backup Operators';
  rsWellKnownSidBuiltinReplicator = 'Builtin Replicator';
  rsWellKnownSidBuiltinRasServers = 'Builtin Ras Servers';
  rsWellKnownSidBuiltinPreWindows2000CompatibleAccess = 'Builtin Pre Windows 200 Compatible Access';
  rsWellKnownSidBuiltinRemoteDesktopUsers = 'Builtin Remote Desktop Users';
  rsWellKnownSidBuiltinNetworkConfigurationOperators = 'Builtin Network Configuration Operators';
  rsWellKnownSidBuiltinIncomingForestTrustBuilders = 'Builtin Incoming Forest Trust Builders';
  rsWellKnownSidBuiltinPerfMonitoringUsers = 'Builtin Perf Monitoring Users';
  rsWellKnownSidBuiltinPerfLoggingUsers = 'Builtin Perf Logging Users';
  rsWellKnownSidBuiltinAuthorizationAccess = 'Builtin Authorization Access';
  rsWellKnownSidBuiltinTerminalServerLicenseServers = 'Builtin Terminal Server License Servers';
  rsWellKnownSidBuiltinDcomUsers = 'Builtin Dcom Users';
  rsWellKnownSidBuiltinIUsers = 'Builtin IUsers';
  rsWellKnownSidBuiltinCryptoOperators = 'Builtin Crypto Operators';
  rsWellKnownSidBuiltinUnknown = 'Builtin Unknown';
  rsWellKnownSidBuiltinCacheablePrincipalsGroups = 'Builtin Cacheable Principals Groups';
  rsWellKnownSidBuiltinNonCacheablePrincipalsGroups = 'Builtin Non Cacheable Principals Groups';
  rsWellKnownSidBuiltinEventLogReadersGroup = 'Builtin Event Log Readers Group';
  rsWellKnownSidBuiltinCertSvcDComAccessGroup = 'Builtin Cert Svc DCom Access Group';
  rsWellKnownSidBuiltinRdsRemoteAccessServers = 'Builtin Rds Remote Access Servers';
  rsWellKnownSidBuiltinRdsEndpointServers = 'Builtin Rds Endpoint Servers';
  rsWellKnownSidBuiltinRdsManagementServers = 'Builtin Rds Management Servers';
  rsWellKnownSidBuiltinHyperVAdmins = 'Builtin Hyper VAdmins';
  rsWellKnownSidBuiltinAccessControlAssistanceOperators = 'Builtin Access Control Assistance Operators';
  rsWellKnownSidBuiltinRemoteManagementUsers = 'Builtin Remote Management Users';
  rsWellKnownSidBuiltinDefaultSystemManagedGroup = 'Builtin Default System Managed Group';
  rsWellKnownSidBuiltinStorageReplicaAdmins = 'Builtin Storage Replica Admins';
  rsWellKnownSidBuiltinDeviceOwners = 'Builtin Device Owners';
  rsWellKnownSidBuiltinWriteRestrictedCode = 'Builtin Write Restricted Code';
  rsWellKnownSidBuiltinUserModeDriver = 'Builtin User Mode Driver';
  rsWellKnownSidCapabilityInternetClient = 'Capability Internet Client';
  rsWellKnownSidCapabilityInternetClientServer = 'Capability Internet Client Server';
  rsWellKnownSidCapabilityPrivateNetworkClientServer = 'Capability Private Network Client Server';
  rsWellKnownSidCapabilityPicturesLibrary = 'Capability Pictures Library';
  rsWellKnownSidCapabilityVideosLibrary = 'Capability Videos Library';
  rsWellKnownSidCapabilityMusicLibrary = 'Capability Music Library';
  rsWellKnownSidCapabilityDocumentsLibrary = 'Capability Documents Library';
  rsWellKnownSidCapabilityEnterpriseAuthentication = 'Capability Enterprise Authentication';
  rsWellKnownSidCapabilitySharedUserCertificates = 'Capability Shared User Certificates';
  rsWellKnownSidCapabilityRemovableStorage = 'Capability Removable Storage';
  rsWellKnownSidCapabilityAppointments = 'Capability Appointments';
  rsWellKnownSidCapabilityContacts = 'Capability Contacts';
  rsWellKnownSidBuiltinAnyPackage = 'Builtin Any Package';
  rsWellKnownSidBuiltinAnyRestrictedPackage = 'Builtin Any Restricted Package';
  rsWellKnownSidNtlmAuthentication = 'Ntlm Authentication';
  rsWellKnownSidSChannelAuthentication = 'SChannel Authentication';
  rsWellKnownSidDigestAuthentication = 'Digest Authentication';

  rsACE_mask =
    'Full Control' + LineEnding +
    'List Content' + LineEnding +
    'Read (WP)' + LineEnding +
    'Write (RP)' + LineEnding +
    'Create Child (CC)' + LineEnding +
    'Delete Child (DC)' + LineEnding +
    'List Child (LC)' + LineEnding +
    'List Object (be visible) (LO)' + LineEnding +
    'Delete (SD)' + LineEnding +
    'Delete Tree (DT)' + LineEnding +
    'Read Security (RC)' + LineEnding +
    'Write Security (WD)' + LineEnding +
    'Write Owner (WO)' + LineEnding +
    'Validated Write (SW)' + LineEnding +
    'Control Access (CR)';

  rsACE_flags =
    'Container Inherit (CI)' + LineEnding +
    'Object Inherit (OI)' + LineEnding +
    'No Propagate (NP)' + LineEnding +
    'Inherit Only (E/S)';

  // SecAceFlag - mormot.core.os.security
  rsSafObjectInherit = 'Object Inherit';
  rsSafContainerInherit = 'Container Inherit';
  rsSafNoPropagate = 'No Propagate';
  rsSafInheritOnly = 'Inherit Only';
  rsSafInherited = 'Inherited';
  rsSafAuditSuccess = 'Audit Success';
  rsSafAuditFailure = 'Audit Failure';

  rsSecAccessFullControl = 'Full control';
  rsSecAccessListContent = 'List content';

  rsAnd = '% and %';
  rsFlagDirectDesendants = 'direct descendants';
  rsFlagThisObject       = 'this object';
  rsFlagAllDescendants   = 'all descendants';

  rsAllow = 'Allow';
  rsDeny = 'Deny';

  rsSingleStringEditor = 'Single String Editor';
  rsMultiStringEditor = 'Multi-valued String Editor';
  rsSingleTimeEditor = 'Single Time Editor';
  rsMultiTimeEditor = 'Multi-valued Time Editor';

  rsAnonymousConnection = 'Anonymous Connection';
  rsUseCurrentCreds = 'Current user credentials';
  rsTitleConnectOrBind = 'Connect or bind error.';
  rsTitleConnectSuccess = 'Successful connection';
  rsConnectSuccess = 'Successfully connected to domain "%"';
  rsConnectedOnDomainAs = 'You are connected as "%".';
  rsConnectedOnDomainWithDC = 'The DC of this domain is "%".';

  rsDelegateControlFolder = 'You chose to delegate control of objects in the following Active Directory folder:';
  rsDelegateControlObjects = 'The groups, users, or computers to which you have given control are:';
  rsDelegateControlTasks = 'You chose to delegate the following tasks:';

  // Week days
  rsMonday    = 'Monday';
  rsThursday  = 'Thursday';
  rsWednesday = 'Wednesday';
  rsTuesday   = 'Tuesday';
  rsFriday    = 'Friday';
  rsSaturday  = 'Saturday';
  rsSunday    = 'Sunday';

  // UAC
  rsUACWorkstation = 'Workstation or server';
  rsUACServer      = 'Domain controller';

  rsUnsavedChangeQuit = 'There are unsaved changes.'#13#10'Are you sure you want to quit?';

  rsTitleSelectNewManager = 'Select new Manager';
  rsTitleSelectGroups = 'Select groups';
  rsDeleteMemberFromGroups = 'Are you sure that you want to delete % from the selected group(s)?';
  rsTitleDeleteMember = 'Delete a group user';
  rsDeletePrimaryGroup = 'The primary group cannot be deleted. You need to define another if you wish to delete this one.';
  rsTitleNotFound = 'Error not found';
  rsACENotFound = 'ACE for object % not found in ACL.';
  rsACECreateFailure = 'Failed to create ACE for object % in ACL.';
  rsTitleAdvancedSecurity = 'Advanced Security Settings for %';
  rsTitlePermissionsFor = 'Permissions for %';

  // Search Scope
  rsSearchScopeBaseObject = 'Base object';
  rsSearchScopeSingleLevel = 'Single level';
  rsSearchScopeWholeSubtree = 'Whole subtree';

  // DNS Record names
  rsDNSRecordA = 'Host (A or AAAA)';
  rsDNSRecordCNAME = 'Alias (CNAME)';
  rsDNSRecordMX = 'Mail Exchanger (MX)';
  rsDNSRecordPTR = 'Pointer (PTR)';
  rsDNSRecordSRV = 'Service Location (SRV)';

  // DNS Record description
  rsDNSRecordADescription = 'Host address (A or AAAA) record. Maps a DNS domain name to a 32-bit IP version 4 address (RFC 1035) or a 128-bit IP version 6 address (RFC 1886).';
  rsDNSRecordCNAMEDescription = 'Alias record. Indicates an alternate or alias DNS domain name for a name already specified in other resource record types used in this zone. The record is also known as the canonical name (CNAME) record type. (RFC 1035)';
  rsDNSRecordMXDescription = 'Mail exchanger (MX) record. Provides message routing to a specified mail exchange host that is acting as a mail exchanger for a specified DNS domain name. MX records use a 16-bit integer to indicate host priority in message routing where multiple mail exchange hosts are specified. For each mail exchange host specified in this record type, a corresponding host address (A) type record is needed. (RFC 1035)';
  rsDNSRecordPTRDescription = 'Pointer (PTR) record. Points to a location in the domain name space. PTR records are typically used in special domains to perform reverse lookups of address-to-name mappings. Each record provides simple data that points to some other location in the domain name space (usually a forward lookup zone). Where PTR records are used, no additional section processing is implied or caused by their presence. (RFC 1035)';
  rsDNSRecordSRVDescription = 'Service (SRV) record. Allows administrators to use several servers for a single DNS domain, to easily move a TCP/IP service from one host to another host with administration, and to designate some service provider hosts as primary servers for a service and other hosts as backups. DNS clients that use a SRV-type query ask for a specific TCP/IP service and protocol mapped to a specific DNS domain and receive the names of any available servers. (RFC 2052)';

  // DNS PROPERTY ZONE TYPE
  rsDNSZoneTypeCache = 'Cache';
  rsDNSZoneTypePrimary = 'Primary';
  rsDNSZoneTypeSecondary = 'Secondary';
  rsDNSZoneTypeStub = 'Stub';
  rsDNSZoneTypeForwarder = 'Forwarder';
  rsDNSZoneTypeSecondaryCache = 'Secondary cache';

  // DNS PROPERTY ZONE ALLOW UPDATE
  rsDNSZoneUpdateOff = 'OFF';
  rsDNSZoneUpdateUnsecure = 'Unsecure';
  rsDNSZoneUpdateSecure = 'Secure';

  // DNS PROPERTY ZONE DC PROMO CONVERT
  rsDNSZoneDCPromoConvertNone = 'None';
  rsDNSZoneDCPromoConvertDomain = 'Domain';
  rsDNSZoneDCPromoConvertForest = 'Forest';

  rsDNSPropertyZoneType = 'Type';
  rsDNSPropertyZoneAllowUpdate = 'Allow Update';
  rsDNSPropertyZoneSecureTime = 'Secure Time';
  rsDNSPropertyZoneNoRefreshInterval = 'No Refresh Interval';
  rsDNSPropertyZoneRefreshInterval = 'Refresh Interval';
  rsDNSPropertyZoneAgingState = 'Aging state';
  rsDNSPropertyZoneScavengingServers = 'Scavenging servers';
  rsDNSPropertyZoneAgingEnabledTime = 'Aging enabled time';
  rsDNSPropertyZoneDeletedFromHostname = 'Deleted from hostname';
  rsDNSPropertyZoneMasterServers = 'Master servers';
  rsDNSPropertyZoneAutoNSServers = 'Auto NS servers';
  rsDNSPropertyZoneDCPromoConvert = 'DC promo convert';
  rsDNSPropertyZoneScavengingServersDA = 'Scavenging servers DA';
  rsDNSPropertyZoneMasterServersDA = 'Master servers DA';
  rsDNSPropertyZoneAutoNSServersDA = 'Auto NS servers DA';
  rsDNSPropertyZoneNodeDBFlags = 'Node DB flags';

  // Delete rows
  rsGridDeleteRows = 'Delete selected rows';
  rsGridConfDeleteRow = 'Confirm the deletion of the %d selected rows?';

  // Module names
  rsModuleADUCName = 'UsersAndComputers';
  rsModuleADUCDisplayName = 'Users and Computers';
  rsModuleDNSName = 'DomainNameSystem';
  rsModuleDNSDisplayName = 'Domain Name System';
  rsModuleADSSName = 'SitesAndServices';
  rsModuleADSSDisplayName = 'Sites and Services';
  rsModuleADSIName = 'ServicesAndInterfaces';
  rsModuleADSIDisplayName = 'Services and Interfaces';

const
  DAYS_BETWEEN_1601_AND_1900 = 109205;
  HUNDRED_OF_MS_IN_A_DAY = 864000000000;

  SEC_ACCESS_NAMES: array[TSecAccess] of String = (
    rsSecAccessCreateChild,       // samCreateChild
    rsSecAccessDeleteChild,       // samDeleteChild
    rsSecAccessListChildren,      // samListChildren
    rsSecAccessSelfWrite,         // samSelfWrite
    rsSecAccessReadProp,          // samReadProp
    rsSecAccessWriteProp,         // samWriteProp
    rsSecAccessDeleteTree,        // samDeleteTree
    rsSecAccessListObject,        // samListObject
    rsSecAccessControlAccess,     // samControlAccess
    '',                           // sam9
    '',                           // sam10
    '',                           // sam11
    '',                           // sam12
    '',                           // sam13
    '',                           // sam14
    '',                           // sam15
    rsSecAccessDelete,            // samDelete
    rsSecAccessReadControl,       // samReadControl
    rsSecAccessWriteDac,          // samWriteDac
    rsSecAccessWriteOwner,        // samWriteOwner
    rsSecAccessSynchronize,       // samSynchronize
    '',                           // sam21
    '',                           // sam22
    '',                           // sam23
    '',                           // samAccessSystemSecurity
    '',                           // samMaximumAllowed
    '',                           // sam26
    '',                           // sam27
    rsSecAccessGenericAll,        // samGenericAll
    rsSecAccessGenericExecute,    // samGenericExecute
    rsSecAccessGenericWrite,      // samGenericWrite
    rsSecAccessGenericRead        // samGenericRead
  );

function AceInDacl(Dacl: TSecAcl; Ace: TSecAce): Boolean;
function AceIsEqual(a, b: TSecAce): Boolean;
function AceIsInherited(Ace: PSecAce): Boolean;
function AttributesEquals(a1, a2: TLdapAttribute): Boolean;
function DateTimeToMSTime(const t: TDateTime): Int64;
function GetAceParentCount(Ace: TSecAce; sdArr: Array of TSecurityDescriptor; count: Integer = 0): Integer;
function GetParentDN(DN: RawUtf8): RawUtf8;
function IsContainer(ObjectClass: RawUtf8): Boolean;
function IsLocalPath(path: RawUtf8): Boolean;
function ISOToTimeFormat(str: String): String;
function IsServerPath(path: RawUtf8): Boolean;
function MSTimeToDateTime(mst: Int64): TDateTime;
function MSTimeEqual(mst1: Int64; mst2: Int64): Boolean;
function ProtectAgainstAccidentalDeletion(Ldap: TLdapClient; DN, BaseDN: RawUtf8; out NewSecDesc: TSecurityDescriptor; Enabled: Boolean): Boolean;
procedure OrderAcl(Ldap: TLdapClient; DN, baseDN: RawUtf8; Acl: PSecAcl);
procedure SearchInGrid(const Timer: TTimer; const TisGrid: TTisGrid; var SearchWord: RawUtf8; const Key: Char);
function SecDescAddACE(PSecDesc: PSecurityDescriptor;
    GUID: TGuid; Sid: RawSid; SecType: TSecAceType; mask: TSecAccessMask): PSecAce;
function SecDescAddOrUpdateACE(PSecDesc: PSecurityDescriptor; GUID: TGuid;
  Sid: RawSid; SecType: TSecAceType; mask: TSecAccessMask): PSecAce;
function SecDescDeleteACE(PSecDesc: PSecurityDescriptor;
    GUID: TGuid; Sid: RawSid; SecType: TSecAceType; mask: TSecAccessMask): Boolean;
function SecDescFindACE(PSecDesc: PSecurityDescriptor;
  SecType: TSecAceType = TSecAceType.satUnknown;
  const Sid: RawSid    = '';
  mask: TSecAccessMask = [];
  GUID: PGuid          = nil;
  Flags: TSecAceFlags  = [];
  iGUID: PGuid         = nil
  ): Integer;
procedure TTisGridClearSelection(grid: TTisGrid);
function TTisGridGetNextUnselected(grid: TTisGrid; Node: PVirtualNode): PVirtualNode;
procedure UnifyButtonsWidth(Butons: array of TControl; default: Integer = -1);

implementation
uses
  DateUtils,
  Dialogs,
  RegExpr,
  mormot.core.log,
  mormot.core.text,
  ursatldapclient;

function AceInDacl(Dacl: TSecAcl; Ace: TSecAce): Boolean;
var
  Item: TSecAce;
begin
  result := True;
  for Item in Dacl do
    if AceIsEqual(Ace, Item) then
      Exit;
  result := False;
end;

// ntSecurityDescriptor
function AceIsInherited(Ace: PSecAce): Boolean;
begin
  result := safInherited in Ace^.Flags;
end;

function GetParentDN(DN: RawUtf8): RawUtf8;
var
  dns: TNameValueDNs;
  i: Integer;
begin
  result := '';
  ParseDN(Dn, dns, true);
  for i := 1 to High(dns) do
  begin
    if i <> 1 then
      result += ',';
    result += dns[i].Name + '=' + dns[i].Value;
  end;
end;

function AceIsUseless(Ace: PSecAce): Boolean;
begin
  result := Ace^.Mask = [];
end;

function AceIsEqual(a, b: TSecAce): Boolean;
begin
  result := (a.AceType = b.AceType) and
              (a.RawType = b.RawType) and
              (a.Mask = b.Mask) and
              (a.Sid = b.Sid) and
              (a.Opaque = b.Opaque) and
              IsEqualGuid(a.ObjectType, b.ObjectType) and
              IsEqualGuid(a.InheritedObjectType, b.InheritedObjectType);
end;

function GetAceRoot(Ldap: TLdapClient; DN: RawUtf8; Ace: TSecAce): RawUtf8;
var
  sd: TSecurityDescriptor;
  pace: TSecAce;
  pDN: RawUtf8;
  LdapObject: TLdapAttribute;
begin
  pDN := GetParentDN(DN);
  LdapObject := Ldap.SearchObject(atNTSecurityDescriptor, pDN, '');
  if not Assigned(LdapObject) then
  begin
    ShowLdapSearchError(Ldap);
    Exit;
  end;
  sd.FromBinary(LdapObject.GetRaw());
  for pace in sd.Dacl do
  begin
    if (TSecAceFlag.safInherited in Ace.Flags) and (AceIsEqual(ace, pace)) then
    begin
      result := GetAceRoot(Ldap, pDN, ace);
      Exit;
    end;
  end;
  result := DN;
end;

function AttributesEquals(a1, a2: TLdapAttribute): Boolean;
var
  i: Integer;
begin
  result := False;

  if a1.AttributeName <> a2.AttributeName then
    Exit;

  if a1.Count <> a2.Count then
    Exit;

  for i := 0 to a1.Count - 1 do
    if a1.GetRaw(i) <> a2.GetRaw(i) then
      Exit;

  result := True;
end;

function DateTimeToMSTime(const t: TDateTime): Int64;
begin
  result := round((t + DAYS_BETWEEN_1601_AND_1900) * HUNDRED_OF_MS_IN_A_DAY)
end;

function GetAceParentCount(Ace: TSecAce; sdArr: array of TSecurityDescriptor;
  count: Integer): Integer;
var
  pace: TSecAce;
begin
  result := count;
  if not AceIsInherited(@ace) or (count >= Length(sdArr)) then
    Exit;

  // Look for ace in parent acl
  for pace in sdArr[count].Dacl do
    if AceIsEqual(pace, ace) then
    begin
      // look for ace in grandparent acl
      result := GetAceParentCount(Ace, sdArr, count + 1);
      Exit;
    end;

  result := count;
end;

function CompareAce(p1, p2: PSecAce; sdArr: array of TSecurityDescriptor
  ): Integer;
const
  DENY: set of TSecAceType = [satAccessDenied, satObjectAccessDenied, satCallbackAccessDenied, satCallbackObjectAccessDenied];
begin
  result := 0;

  if not Assigned(p1) or not Assigned(p2) then
    Exit;

  // Compare inheritance
  result := GetAceParentCount(p1^, sdArr) - GetAceParentCount(p2^, sdArr);
  if result <> 0 then
    Exit;

  // Compare deny / allow
  if (p1^.AceType in DENY) = (p2^.AceType in DENY) then
    result := Ord(p1^.AceType) - Ord(p2^.AceType)
  else if p1^.AceType in DENY then
    result := -1
  else
    result := 1;
  if result <> 0 then
    Exit;

  // Compare global or object access
  if IsNullGuid(p1^.ObjectType) then
    result := -1;
  if IsNullGuid(p2^.ObjectType) then
    result := result + 1;
  if result <> 0 then
    Exit;

  // Compare sid
  result := CompareStr(RawSidToText(p1^.sid), RawSidToText(p2^.Sid));
end;

procedure InnerOrderAcl(Acl: PSecAcl; sdArr: Array of TSecurityDescriptor);
var
  ace: TSecAce;
  idx, j, lowest: Integer;
begin
  idx := 0;
  while idx < Length(acl^) do // select sort
  begin
    if AceIsUseless(@acl^[idx]) then
    begin
      Delete(acl^, idx, 1);
      continue;
    end;

    lowest := idx;
    for j := idx to High(acl^) do
      if CompareAce(@acl^[j], @acl^[lowest], sdArr) < 0 then
        lowest := j;

    if lowest > idx then
    begin
      ace := acl^[lowest];
      Delete(acl^, lowest, 1);
      Insert(ace, acl^, idx);
    end;
    Inc(idx);
  end;
end;

function IsContainer(objectClass: RawUtf8): Boolean;
const
  CONTAINERS: Array of String = ('organizationalUnit', 'container', 'domainDNS');
var
  Container: String;
begin
  result := True;

  for Container in CONTAINERS do
    if Container = objectClass then
      Exit;

  result := False;
end;

function IsLocalPath(path: RawUtf8): Boolean;
var
  re: TRegExpr;
begin
  result := false;
  if path = '' then
    Exit;
  re := TRegExpr.Create('^[a-zA-Z]:[\\\/]((?:[\w\-\s]+[\\\/])*[\w\-\s]+)?$');
  try
    result := re.Exec(path);
  finally
    FreeAndNil(re);
  end;
end;

function ISOToTimeFormat(str: String): String;
var
  d: TDateTime;
begin
  DefaultFormatSettings.DateSeparator := '/';
  if not TryISOStrToDateTime(str, d) then
  begin
    str := '00000000000000.0Z';
    TryISOStrToDateTime(str, d);
  end;
  d := UniversalTimeToLocal(d);
  result := DateTimetoStr(d);
end;

function IsServerPath(path: RawUtf8): Boolean;
var
  re: TRegExpr;
begin
  result := false;
  if path = '' then
    Exit;
  re := TRegExpr.Create('^\\\\\w+(\\\w+)+$');
  try
    result := re.exec(path);
  finally
    FreeAndNil(re);
  end;
end;

function MSTimeToDateTime(mst: Int64): TDateTime;
begin
  result := mst / HUNDRED_OF_MS_IN_A_DAY - DAYS_BETWEEN_1601_AND_1900;
end;

function MSTimeEqual(mst1: Int64; mst2: Int64): Boolean;
begin
  result := Abs(mst1 - mst2) < 10000000; // one second
end;

function ProtectAgainstAccidentalDeletion(Ldap: TLdapClient; DN,
  BaseDN: RawUtf8; out NewSecDesc: TSecurityDescriptor; Enabled: Boolean
  ): Boolean;
var
  Sid: RawSid;
  SecDescParent: TSecurityDescriptor;
  data: TLdapAttribute;
  i: Integer;
begin
  result := False;
  Sid := KnownRawSid(wksWorld);

  // Parent
  if Enabled then
  begin
    DN := GetParentDN(DN); // Get Parent SecDesc
    data := Ldap.SearchObject(atNTSecurityDescriptor, DN, '');
    if not Assigned(data) then
    begin
      ShowLdapSearchError(Ldap);
      Exit; // Failure
    end;
    if not SecDescParent.FromBinary(data.GetRaw()) then
    begin
      //Dialogs.MessageDlg(rsTitleParsing, rsACEParsing, mtError, [mbOK], 0);
      Exit; // Failure
    end;

    if SecDescFindACE(@SecDescParent, satAccessDenied, Sid, [samDeleteChild], @ATTR_UUID[kaNull]) = -1 then // Search if ACE already exists
    begin
      if MessageDlg(rsConfirmation, rsACEpaadParent, mtConfirmation, mbYesNo, 0) <> mrYes then // User Confirmation
        Exit; // Failure

      if not Assigned(SecDescAddOrUpdateACE(@SecDescParent, ATTR_UUID[kaNull], Sid, satAccessDenied, [samDeleteChild])) then // Add ACE
      begin
        //Dialogs.MessageDlg(rsTitleParsing, FormatUtf8(rsACECreateFailure, [DN]), mtError, [mbOK], 0);
        Exit; // Failure
      end;

      OrderAcl(Ldap, DN, BaseDN, @SecDescParent.Dacl); // Order

      if not Ldap.Modify(DN, lmoReplace, atNTSecurityDescriptor, SecDescParent.ToBinary()) then // Modify
      begin
        ShowLdapModifyError(Ldap);
        Exit; // Failure
      end;
    end;
  end;

  // Self
  if Enabled then
  begin // Add ACE
    if not Assigned(SecDescAddOrUpdateACE(@NewSecDesc, ATTR_UUID[kaNull], Sid, satAccessDenied, [samDelete, samDeleteTree])) then
    begin
      //Dialogs.MessageDlg(rsTitleParsing, FormatUtf8(rsACECreateFailure, [DN]), mtError, [mbOK], 0);
      Exit; // Failure
    end
  end
  else
  begin // Remove ACE
    i := SecDescFindACE(@NewSecDesc, satAccessDenied, Sid, [samDelete, samDeleteTree], @ATTR_UUID[kaNull]);
    if i = -1 then
    begin
      //Dialogs.MessageDlg(rsTitleNotFound, FormatUtf8(rsACENotFound, [DN]), mtError, [mbOK], 0);
      Exit; // Failure
    end;
    NewSecDesc.Dacl[i].Mask -= [samDelete, samDeleteTree];
  end;

  OrderAcl(Ldap, DN, BaseDN, @NewSecDesc.Dacl); // Order

  result := True; // Success
end;

// https://learn.microsoft.com/en-us/windows/win32/secauthz/order-of-aces-in-a-dacl
procedure OrderAcl(Ldap: TLdapClient; DN, baseDN: RawUtf8; Acl: PSecAcl);
var
  sdArr: Array of TSecurityDescriptor;
  sd: TSecurityDescriptor;
  parent, filter: RawUtf8;
  res: TLdapResult;
begin
  TSynLog.Add.Log(sllDebug, FormatUtf8('Start ordering ACL for %...', [DN]));
  sdArr := [];

  parent := DN;
  filter := '';
  while not (parent = ldap.DefaultDN(baseDN)) and not (parent = '') do
  begin
    if (filter = '') then
      filter := '|';
    parent := GetParentDN(parent);
    filter := FormatUtf8('%(distinguishedName=%)', [filter, LdapEscape(parent)]);
  end;

  // Get SDs
  Ldap.SearchBegin();
  try
    repeat
      Ldap.SearchScope := lssWholeSubtree;
      if not Ldap.Search([atNTSecurityDescriptor], filter) then
      begin
        ShowLdapSearchError(Ldap);
        Exit;
      end;
      for res in Ldap.SearchResult.Items do
      begin
        if not sd.FromBinary(res.Attributes.Find(atNTSecurityDescriptor).GetRaw()) then
          continue;
        Insert(sd, sdArr, Length(sdArr));
      end;
    until (Ldap.SearchCookie = '');
  finally
    Ldap.SearchEnd();
  end;

  // Order acl
  InnerOrderAcl(acl, sdArr);
  TSynLog.Add.Log(sllDebug, 'End ordering ACL for.');
end;

procedure SearchInGrid(const Timer: TTimer;
  const TisGrid: TTisGrid; var SearchWord: RawUtf8; const Key: Char);
var
  RowData: PDocVariantData;
  ColumnName: RawUtf8;
  Node, NextNode: PVirtualNode;
  PropertyValues: TRawUtf8DynArray;
begin
  case Timer.Enabled of
    True: Timer.Enabled := False;
    False: SearchWord := '';
  end;

  Insert(Key, SearchWord, Length(SearchWord) + 1);
  ColumnName := TisGrid.FocusedColumnObject.PropertyName;
  NextNode := TisGrid.GetFirst();
  TisGrid.FocusedNode := nil;
  TisGrid.ClearSelection;
  while Assigned(NextNode) do
  begin
    RowData := TisGrid.GetNodeAsPDocVariantData(NextNode);
    Node := NextNode;
    NextNode := TisGrid.GetNext(NextNode);
    if not Assigned(RowData) or not RowData^.Exists(ColumnName) then
      continue;

    if RowData^.S[ColumnName].StartsWith(SearchWord, True) then
    begin
      TisGrid.FocusedNode := Node;
      TisGrid.AddToSelection(Node);
      Break;
    end;
  end;
  Timer.Enabled := True;
  TisGrid.ScrollIntoView(TisGrid.FocusedNode, True);
end;

function SecDescAddACE(PSecDesc: PSecurityDescriptor; GUID: TGuid; Sid: RawSid;
  SecType: TSecAceType; mask: TSecAccessMask): PSecAce;
var
  s: ShortString = '';
  m: ShortString = '';
begin
  SddlAppendSid(s, PSid(Sid), nil);
  SddlAppendMask(m, mask);

  result := PSecDesc^.Add(SecType, s, m);
  if not Assigned(result) then
    Exit;

  result^.ObjectType := GUID;
end;

function SecDescAddOrUpdateACE(PSecDesc: PSecurityDescriptor; GUID: TGuid;
  Sid: RawSid; SecType: TSecAceType; mask: TSecAccessMask): PSecAce;
  var
    s: ShortString = '';
    m: ShortString = '';
    i: Integer;
  begin
    SddlAppendSid(s, PSid(Sid), nil);
    SddlAppendMask(m, mask);

    i := SecDescFindACE(PSecDesc, SecType, Sid, [], @GUID);
    if (i = -1) or AceIsInherited(@PSecDesc^.Dacl[i]) then
    begin
      result := PSecDesc^.Add(SecType, s, m);
      if not Assigned(result) then
        Exit;
      result^.ObjectType := GUID;
    end
    else
    begin
      result := @PSecDesc^.Dacl[i];
      result^.Mask += mask;
    end;
end;

function SecDescDeleteACE(PSecDesc: PSecurityDescriptor; GUID: TGuid;
  Sid: RawSid; SecType: TSecAceType; mask: TSecAccessMask): Boolean;
var
  i: Integer;
begin
  result := False;

  i := SecDescFindACE(PSecDesc, SecType, Sid, mask, @GUID);
  if i = -1 then
    Exit;

  PSecDesc^.Delete(i);
  result := True;
end;

function SecDescFindACE(PSecDesc: PSecurityDescriptor;
  SecType: TSecAceType = TSecAceType.satUnknown;
  const Sid: RawSid    = '';
  mask: TSecAccessMask = [];
  GUID: PGuid          = nil;
  Flags: TSecAceFlags  = [];
  iGUID: PGuid         = nil
  ): Integer;
var
  ace: PSecAce;
begin
  result := 0;

  while result < Length(PSecDesc^.Dacl) do
  begin
    ace := @PSecDesc^.Dacl[result];
    if not (safInherited in Flags) and AceIsInherited(ace) then // Outide of non-inherited range
    begin
      result := -1;
      Exit;
    end;
    if
      (ace^.Flags   >= Flags) and // Placed first to go a litle faster
      ((SecType = TSecAceType.satUnknown) or (ace^.AceType = SecType)) and
      ((Sid     = '')                     or (ace^.Sid     = Sid)) and
                                             (ace^.Mask   >= mask) and
      ((GUID    = nil)                    or isEqualGuid(@ace^.ObjectType, GUID)) and
      ((iGUID   = nil)                    or isEqualGuid(@ace^.InheritedObjectType, iGUID)) then
      Exit;
    Inc(result);
  end;

  if result = Length(PSecDesc^.Dacl) then
    result := -1;
end;

procedure TTisGridClearSelection(grid: TTisGrid);
var
  i: Integer;
  Selection: TNodeArray;
begin
  Selection := grid.GetSortedSelection(True);
  for i := Length(Selection) - 1 downto 0 do
    grid.RemoveFromSelection(Selection[i]);
  grid.FocusedNode := grid.RootNode;
end;

function TTisGridGetNextUnselected(grid: TTisGrid; Node: PVirtualNode
  ): PVirtualNode;
begin
  repeat
    Node := grid.GetNext(Node);
  until not grid.Selected[Node];
  result := Node;
end;

// Laz
procedure UnifyButtonsWidth(Butons: array of TControl; default: Integer);
var
  BtnWidth, i: Integer;
begin
  BtnWidth := 0;
  if default = -1 then
  begin
    for i := 0 to Length(Butons) - 1 do
      if BtnWidth < Butons[i].Width then
        BtnWidth := Butons[i].Width;
  end else
    BtnWidth := default;
  for i := 0 to Length(Butons) - 1 do
  begin
    Butons[i].Constraints.MinWidth := BtnWidth;
    Butons[i].Width := BtnWidth;
  end;
end;

end.

