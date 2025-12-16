unit ucommon;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.base,
  mormot.net.ldap,
  {$IFDEF OPENRSATTESTS}
  mormot.core.test,
  {$ENDIF OPENRSATTESTS}
  mormot.core.os.security,
  mormot.core.variants;

type
  PSecurityDescriptor = ^TSecurityDescriptor;

  {$IFDEF OPENRSATTESTS}

  { TCommonTests }

  TCommonTests = class(TSynTestCase)
  published
    procedure FuncExpandIPv6;
    procedure FuncIsValidIP6;
    procedure FuncIsValidIP4;
  end;

  {$ENDIF OPENRSATTESTS}

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

  rsLdapSuccess = 'Ldap Success';
  rsLdapError = 'Ldap Error';
  rsLdapAddFailed = 'Ldap add failed: "%"';
  rsLdapDeleteFailed = 'Ldap delete failed: "%"';
  rsLdapConnectFailed = 'Ldap connect failed: "%"';
  rsLdapSearchFailed = 'Ldap search failed: "%"';
  rsLdapModifyFailed = 'Ldap modify failed: "%"';

  rsSecurityDescriptorInvalid = 'Fail to read security descriptor';

  // https://ldap.com/ldap-result-code-reference-core-ldapv3-result-codes/#rc-constraintViolation
  rsOperationsError = 'Operation occurs in a wrong order.'; // leOperationsError (1)
  rsProtocolError = 'Malformed LDAP message. Unrecognized operation.'; // leProtocolError (2)
  rsTimeLimitExceeded = 'The request time limit has been reached.'; // leTimeLimitExceeded (3)
  rsSizeLimitExceeded = 'The result retrieved more objects than the server allowed.'; // leSizeLimitExceeded (4)
  rsAuthMethodNotSupported = 'The server does not allow this kind of auth method.'; // leAuthMethodNotSupported (7)
  rsStrongerAuthRequired = 'The server requires the client to authenticate with a stronger authentication.'; // leStrongerAuthRequired (8)
  rsReferral = 'This should not be an error. Client should follow referral. Please try the action on a different server.'; // leReferral (9)
  rsAdminLimitExceeded = '';
  rsUnavailableCriticalExtension = '';
  rsConfidentialityRequired = 'The server do not allow your operation, cause your connection is not secured enought.'; // leConfidentialityRequired (13)
  rsSaslBindInProgress = 'The server is working on a bind request. Try again later.'; // leSaslBindInProgress (14)
  rsNoSuchAttribute = 'The targeted attribute does not exist in the specified entry.'; // leNoSuchAttribute (16)
  rsUndefinedAttributeType = 'The targeted attribute does not exist in the server schema.'; // leUndefinedAttributeType (17)
  rsInappropriateMatching = 'The matching condition is not supported for this attributeType.'; // leInappropriateMatching (18)
  rsConstraintViolation = 'The requests violates some constraint defined within the server.'; // leConstraintViolation (19)
  rsInsufficientAccessRights = 'Insufficient rights access.';

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
  rsConnectSuccess = 'Successfully connected to domain controller "%"';
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

  rsRestartTitle = 'Restart confirmation';
  rsChangeOptionRequiredRestart = 'The changes you have made require a console restart. Restart now?';

  rsTitleInvalidFormat = 'Invalid format';
  rsInvalidIpFormat = 'Invalid IP format';

  rsTitleDeleteObject = 'Delete object';
  rsDeleteObjectConfirmation = 'Do you want to delete the record % from the server?';
  rsDeleteObjectsConfirmation = 'Do you want to delete the selected records from the server?';

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

  PREFIX_REGEX = '(\/(12[0-8]|(1[0-1]|[1-9]|)\d))';
  IPV4_REGEX = '((25[0-5]|(2[0-4]|1\d|[1-9]|)\d)\.?\b){4}';
  IPV6_REGEX = '((?:[0-9A-Fa-f]{1,4}:){7}[0-9A-Fa-f]{1,4}|(?:[0-9A-Fa-f]{1,4}:){1,7}:|:(?::[0-9A-Fa-f]{1,4}){1,7}|(?:[0-9A-Fa-f]{1,4}:){1,6}:[0-9A-Fa-f]{1,4}|(?:[0-9A-Fa-f]{1,4}:){1,5}(?::[0-9A-Fa-f]{1,4}){1,2}|(?:[0-9A-Fa-f]{1,4}:){1,4}(?::[0-9A-Fa-f]{1,4}){1,3}|(?:[0-9A-Fa-f]{1,4}:){1,3}(?::[0-9A-Fa-f]{1,4}){1,4}|(?:[0-9A-Fa-f]{1,4}:){1,2}(?::[0-9A-Fa-f]{1,4}){1,5}|[0-9A-Fa-f]{1,4}:(?:(?::[0-9A-Fa-f]{1,4}){1,6})|:(?:(?::[0-9A-Fa-f]{1,4}){1,6}))';

function AceInDacl(Dacl: TSecAcl; Ace: TSecAce): Boolean;
function AceIsEqual(a, b: TSecAce): Boolean;
function AceIsInherited(Ace: PSecAce): Boolean;
function AttributesEquals(a1, a2: TLdapAttribute): Boolean;
function DateTimeToMSTime(const t: TDateTime): Int64;
function ExpandIPv6(ShortIPv6: RawUtf8): RawUtf8;
function GetAceParentCount(Ace: TSecAce; sdArr: Array of TSecurityDescriptor; count: Integer = 0): Integer;
function GetDNName(DistinguishedName: RawUtf8): RawUtf8;
function GetParentDN(DN: RawUtf8): RawUtf8;
function IsContainer(ObjectClass: RawUtf8): Boolean;
function IsLocalPath(path: RawUtf8): Boolean;
function ISOToTimeFormat(str: String): String;
function IsServerPath(path: RawUtf8): Boolean;
function IsValidIP(IP: RawUtf8): Boolean;
function IsValidIP4(IP4: RawUtf8): Boolean;
function IsValidIP6(IP6: RawUtf8): Boolean;
function MSTimeToDateTime(mst: Int64): TDateTime;
function MSTimeEqual(mst1: Int64; mst2: Int64): Boolean;
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

implementation
uses
  DateUtils,
  RegExpr,
  StrUtils,
  mormot.core.log,
  mormot.core.text,
  mormot.net.sock;

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

function GetDNName(DistinguishedName: RawUtf8): RawUtf8;
var
  CanonicalName: RawUtf8;
  Splitted: TStringArray;
  Len: SizeInt;
begin
  result := '';
  CanonicalName := DNToCN(DistinguishedName);
  Splitted := String(CanonicalName).Split('/');
  if not Assigned(Splitted) then
    Exit;
  Len := Length(Splitted);
  if Len <= 0 then
    Exit;
  result := Splitted[Len - 1];
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

function ExpandIPv6(ShortIPv6: RawUtf8): RawUtf8;
var
  IPv6Parts: TStringArray;
  SubPos, MissingParts, j, i: Integer;
begin
  IPv6Parts := String(ShortIPv6).Split(':');

  // Find substitution
  SubPos := -1;
  for i := 0 to High(IPv6Parts) do
  begin
    if IPv6Parts[i] = '' then
    begin
      SubPos := i;
      break;
    end;
  end;

  // Add missing parts
  MissingParts := 7 - Length(IPv6Parts);
  for i := 0 to MissingParts do
    Insert('', IPv6Parts, SubPos);

  // Fill missing zero
  for i := 0 to High(IPv6Parts) do
  begin
    for j := 0 to 3 - Length(IPv6Parts[i]) do
      Insert('0', IPv6Parts[i], 0);
  end;

  result := String.Join(':', IPv6Parts);
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

function IsContainer(ObjectClass: RawUtf8): Boolean;
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

function IsValidIP(IP: RawUtf8): Boolean;
var
  IPAddr: TNetAddr;
begin
  result := False;
  if IPAddr.SetFrom(IP, '0', nlTcp) <> nrOK then
    Exit;
  case IPAddr.Family of
    nfIP4: result := IsValidIP4(IP);
    nfIP6: result := IsValidIP6(IP);
  end;
end;

function IsValidIP4(IP4: RawUtf8): Boolean;
var
  Regex: RawUtf8;
  re: TRegExpr;
begin
  result := False;

  try
    try
      Regex := FormatUtf8('^%$', [IPV4_REGEX]);
      re := TRegExpr.Create(Regex);
      result := re.Exec(IP4);
    except
      on E: Exception do
        Exit;
    end;
  finally
    FreeAndNil(re);
  end;
end;

function IsValidIP6(IP6: RawUtf8): Boolean;
var
  regex: RawUtf8;
  re: TRegExpr;
begin
  result := False;

  try
    try
      regex := FormatUtf8('^%$', [IPV6_REGEX]);
      re := TRegExpr.Create(regex);
      result := re.Exec(IP6);
    except
      on E: Exception do
        Exit;
    end;
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

{$IFDEF OPENRSATTESTS}

{ TCommonTests }

procedure TCommonTests.FuncExpandIPv6;
begin
  Check(ExpandIPv6('0000:0000:0000:0000:0000:0000:0000:0000') = '0000:0000:0000:0000:0000:0000:0000:0000', 'Full zero IP6');
  Check(ExpandIPv6('0000::') = '0000:0000:0000:0000:0000:0000:0000:0000', 'Ultra short zero IP6');
  Check(ExpandIPv6('0000::00') = '0000:0000:0000:0000:0000:0000:0000:0000', 'Very short zero IP6');
  Check(ExpandIPv6('2001:0db8:85a3:0000:0000:8a2e:0370:7334') = '2001:0db8:85a3:0000:0000:8a2e:0370:7334', 'Random IP6');
  Check(ExpandIPv6('2001:db8::1') = '2001:0db8:0000:0000:0000:0000:0000:0001', 'IP6 fill middle');
  Check(ExpandIPv6('::1') = '0000:0000:0000:0000:0000:0000:0000:0001', 'IP6 fill before');
  Check(ExpandIPv6('fe80::') = 'fe80:0000:0000:0000:0000:0000:0000:0000', 'IP6 fill after');
  Check(ExpandIPv6('0:0:0:0:0:0:0:1') = '0000:0000:0000:0000:0000:0000:0000:0001');
end;

procedure TCommonTests.FuncIsValidIP6;
begin
  Check(IsValidIP6('2001:0db8:85a3:0000:0000:8a2e:0370:7334'), 'Valid test');
  Check(IsValidIP6('2001:db8::1'), 'Valid test');
  Check(IsValidIP6('::1'), 'Valid test');
  Check(IsValidIP6('fe80::'), 'Valid test');
  Check(IsValidIP6('0:0:0:0:0:0:0:1'), 'Valid test');
  Check(not IsValidIP6('2001::85a3::8a2e'), 'Double "::" not allowed');
  Check(not IsValidIP6('2001:db8:85a3'), 'Too few groups');
  Check(not IsValidIP6('12345::abcd'), 'Invalid hex (12345 is too long)');
  Check(not IsValidIP6('::1::'), 'Double compression');
  Check(not IsValidIP6('fe80:::1'), 'triple colon not valid');
end;

procedure TCommonTests.FuncIsValidIP4;
begin
  Check(IsValidIP4('192.168.1.255'), 'Valid test');
  Check(IsValidIP4('10.0.0.255'), 'Valid test');
  Check(IsValidIP4('172.16.254.3'), 'Valid test');
  Check(IsValidIP4('255.255.255.255'), 'Valid test');
  Check(IsValidIP4('0.0.0.0'), 'Valid test');
  Check(not IsValidIP4('192.168.1'), 'Only three octets');
  Check(not IsValidIP4('192.168.01.1'), 'Leading zero');
  Check(not IsValidIP4('192.168.1.1.1'), 'Too many octets');
  Check(not IsValidIP4('192.168.1.'), 'Trailing dot');
  Check(not IsValidIP4('256.100.100.100'), '256 is outside valid range');
end;


{$ENDIF OPENRSATTESTS}
end.

