unit ustrconsts;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

resourcestring
  rsVisProfileManagerCaption = 'Profile manager';
  rsVisViewKeyTabCaption = 'View Keytab';
  rsVisTaskResetPassword = 'Reset password';
  rsVisTaskMove = 'Move';
  rsVisSelectObjectSID = 'Select object SID';
  rsVisSelectObjectGUID = 'Select object GUID';
  rsVisResourceRecordType = 'Resource record type';
  rsVisRootDSEInfos = 'DC infos';
  rsVisSearch = 'Search';
  rsVisProfileConfiguration = 'Profile configuration';
  rsVisOptions = 'Options';
  rsVisOperationMasters = 'Operations masters';
  rsVisNewZoneWizard = 'New Zone Wizard';
  rsVisLogonWorkstations = 'Logon workstations';
  rsVisLogonHours = 'Logon hours';
  rsVisChangeDirectoryServer = 'Change directory server';
  rsVisOpenRSATAuthentication = 'OpenRSAT Authentication';

  rsAdd = 'Add';
  rsDelete = 'Delete';
  rsEdit = 'Edit';

  rsLargeIcons = 'Large Icons';
  rsSmallIcons = 'Small Icons';
  rsList = 'List';
  rsTable = 'Table';

  rsOK = 'OK';
  rsClose = 'Close';
  rsCancel = 'Cancel';

  rsName = 'Name';
  rsName_1 = 'Name: ';
  rsValue = 'Value';
  rsDomain = 'Domain';
  rsDomainController = 'Domain controller';
  rsUsername = 'Username';

  rsCopyToClipboardBase64 = 'Copy to clipboard (base64)';
  rsSaveToFile = 'Save to file';
  rsLoadFromFile = 'Load from file';
  rsSelectFolder = 'SelectFolder';

  rsFilename = 'Filename';
  rsFolder = 'Folder';

  rsKVNO = 'KVNO';
  rsTimestamp = 'Timestamp';
  rsPrincipal = 'Principal';
  rsEncryptionType = 'Encryption type';
  rsKey = 'Key';

  rsNewPassword = 'New password: ';
  rsConfirmPassword = 'Confirm password: ';

  rsUserMustChangePasswordAtNextLogon = 'User must change password at next logon';
  rsTheUserMustLogOffAndThenLogOnAgain = 'The user must logoff and then logon again for the change to take effect.';
  rsUnlockTheUserAccount = 'Account Lockout Status on this Domain Controller: ';
  rsAccountLockoutStatusOnThisDomainController = 'Unlock the user''s account';

  rsObjectLocation = 'Object location: ';

  rsSearch = 'Search';
  rsFilter = 'Filter';
  rsFilter_1 = 'Filter: ';

  rsDistinguishedName = 'Distinguished name';
  rsSID = 'SID';

  rsSelectAResourceRecordType = 'Select a resource record type: ';
  rsDescription = 'Description';
  rsDescription_1 = 'Description: ';
  rsCreateRecord = 'Create record';

  rsServerInformationsAvailableFromRootDSE = 'Server informations (available from RootDSE Object)';

  rsType = 'Type';
  rsBasic = 'Basic';
  rsAdvanced = 'Advanced';
  rsExpert = 'Expert';
  rsOptions = 'Options';
  rsPath = 'Path: ';
  rsChange = 'Change';
  rsSearchAName = 'Search a name: ';
  rsAllConditions = 'All conditions';
  rsAtLeastOneCondition = 'At least one condition';
  rsCondition = 'Condition';
  rsAddCondition = 'Add condition';
  rsDeleteCondition = 'Delete condition';
  rsSearchUsingLdapFilter = 'Search using ldap filter: ';
  rsSearchScope = 'Search scope';
  rsPageSize = 'Page size';
  rsPageCount = 'Page count';
  rsSearchFrom = 'Search from';
  rsSearchNew = 'Search new';
  rsShowInView = 'Show in view';
  rsProperties = 'Properties';
  rsAdvancedAdd = 'Advanced add';
  rsAdvancedDelete = 'Advanced delete';

  rsApply = 'Apply';
  rsClear = 'Clear';
  rsManagerCanUpdateMembershipList = 'Manager can update membership list';
  rsShowOnlyAttributesThatHaveValues = 'Show only attributes that have values';
  rsShowOnlyWritableAttributes = 'Show only writable attributes';
  rsMandatory = 'Mandatory';
  rsOptional = 'Optional';
  rsConstructed = 'Constructed';
  rsBacklinks = 'Backlinks';
  rsSystemOnly = 'System-only';
  rsRemove = 'Remove';
  rsCopyToFile = 'Copy to file';
  rsViewCertificate = 'View certificate';
  rsExpireNow = 'Expire now';
  rsCopyPassword = 'Copy password';
  rsShowPassword = 'Show password';
  rsModify = 'Modify';

  rsAuthMethod = 'Auth method';
  rsAccount = 'Account';
  rsTLS = 'TLS';
  rsTimeout = 'Timeout';
  rsUnsafeConnection = 'Unsafe connection';
  rsAnonymous = 'Anonymous';
  rsSimple = 'Simple';
  rsDigest = 'Digest';
  rsKerberos = 'Kerberos';
  rsChannelBinding = 'Channel binding';
  rsCurrentUsername = 'Current username';
  rsAlgorithm = 'Algorithm';
  rsPassword = 'Password';
  rsPassword_1 = 'Password:';
  rsAt = '@';
  rsTestConnection = 'Test connection';
  rsTestAuthentication = 'Test authentication';
  rsShowDCInfos = 'Show DC Infos';

  rsSchemaMaster = 'Schema master';
  rsDomainNamingMaster = 'Domain naming master';
  rsPDC = 'PDC';
  rsRID = 'RID';
  rsInfrastructureMaster = 'Infrastructure master';

  rsNewZoneWizardWelcome = 'Welcome to the New Zone Wizard';
  rsNewZoneWizardDescription1 = 'This wizard helps you create a new zone for your DNS server.';
  rsNewZoneWizardDescription2 = 'A zone translates DNS names to related data, such as IP addresses or network services.';
  rsNewZoneWizardDescription3 = 'To continue, click Next.';
  rsNewZoneWizardZoneType = 'Zone Type';
  rsNewZoneWizardZoneTypeDescription = 'The DNS server supports various types of zones and storage.';
  rsNewZoneWizardZoneTypeSelect = 'Select the type of zone you want to create:';
  rsNewZoneWizardZoneTypePrimaryZoneDescription = 'Creates a copy of a zone that can be updated directly on this server.';
  rsNewZoneWizardZoneTypeSecondaryZoneDescription = 'Create a copy of a zone that exists on another server. This option helps balance the processing load of primary servers and provides fault tolerance.';
  rsNewZoneWizardZoneTypeStubZoneDescription = 'Create a copy of a zone containing inly Name Server (NS), Start of Authority (SOA), and possibly glue Host (A) records. A server containing a stub zone is not authoritative for that zone.';
  rsNewZoneWizardZoneTypeStoreTheZone = 'Store the zone in Active Directory (available only if DNS server is a writeable domain controller)';
  rsNewZoneWizardZoneReplication = 'Active Directory Zone Replication Scope';
  rsNewZoneWizardZoneReplicationDescription = 'You can select how you want DNS data replicated throughout your network.';
  rsNewZoneWizardZoneReplicationSelect = 'Select how you want zone data replicated:';
  rsNewZoneWizardZoneReplicationChoice1 = 'To all DNS servers running on domain controllers in this forest:';
  rsNewZoneWizardZoneReplicationChoice2 = 'To all DNS servers running on domain controllers in this domain:';
  rsNewZoneWizardZoneReplicationChoice3 = 'To all domain controllers in this domain (for Windows 2000 compatibility):';
  rsNewZoneWizardZoneReplicationChoice4 = 'To all domain controllers specified in the scoper of this directory partition:';
  rsNewZoneWizardZoneName = 'Zone Name';
  rsNewZoneWizardZoneNameDescription = 'What is the name of the new zone?';
  rsNewZoneWizardZoneNameMessage = 'The zone name specifies the portion of the DNS namespace for which this server is autoritative. It might be your organization''s domain name (for example, example.com) or a portion of the domain name (for example, newzone.example.com). The zone name is not the name of the DNS server.';
  rsNewZoneWizardZoneNameZoneName = 'Zone name:';
  rsNewZoneWizardZoneFile = 'Zone File';
  rsNewZoneWizardZoneFileDescription = 'You can create a new zone file or use a file copied from another DNS server.';
  rsNewZoneWizardZoneFileQuestion = 'Do you want to create a new zone file or use an existing file that you have copied from another DNS server?';
  rsNewZoneWizardZoneFileMessage = 'To use this existing file, ensure that it has been copied to the folder %SystemRoot%\system32\dns on this server, and then click Next.';
  rsNewZoneWizardZoneFileChoice1 = 'Create a new file with this file name:';
  rsNewZoneWizardZoneFileChoice2 = 'Use this existing file:';
  rsNewZoneWizardDynamicUpdateDescription = 'You can specify that this DNS zone accepts secure, nonsecure, or no dynamic updates.';
  rsNewZoneWizardDynamicUpdate = 'Dynamic Update';
  rsNewZoneWizardDynamicUpdateMessage = 'Dynamic updates enable DNS client computers to register and dynamically update their resource records with a DNS server whenever changes occur.';
  rsNewZoneWizardDynamicUpdateSelect = 'Select the type of dynamic updates you want to allow:';
  rsNewZoneWizardDynamicUpdateChoice1 = 'Allow only secure dynamic updates (recommanded for Active Directory)';
  rsNewZoneWizardDynamicUpdateChoice11 = 'This option is available only for Active Directory-integrated zones.';
  rsNewZoneWizardDynamicUpdateChoice2 = 'Allow both nonsecure and secure dynamic updates';
  rsNewZoneWizardDynamicUpdateChoice21 = 'Dynamic updates of resource records are accepted from any client.';
  rsNewZoneWizardDynamicUpdateChoice22 = 'This option is a significant security vulnerability because updates can be accepted from untrusted sources.';
  rsNewZoneWizardDynamicUpdateChoice3 = 'Do not allow dynamic updates';
  rsNewZoneWizardDynamicUpdateChoice31 = 'Dynamic updates of resource records are not accepted by this zone. You must update these records manually.';
  rsNewZoneWizardComplete = 'Completing the New Zone Wizard';
  rsNewZoneWizardCompleteMessage = 'You have successfully completed the New Zone Wizard. You specified the following settings:';
  rsNewZoneWizardCompleteNote = 'Note: You should now add records to the zone or ensure that records are updated dynamically. You can then verify name resolution using nslookup.';
  rsNewZoneWizardCompleteClose = 'To close this wizard and create the new zone, click Finish.';

  rsNext = 'Next';
  rsBack = 'Back';
  rsPrimaryZone = 'Primary zone';
  rsSecondaryZone = 'Secondary zone';
  rsStubZone = 'Stub zone';
  rsFinish = 'Finish';

  rsCreateIn = 'Create in: ';

  rsLogonWorkstationsInfo = 'In Computer name, type the computer''s NetBIOS or Domain Name system (DNS) name.';
  rsLogonWorkstationsSelect = 'This user can logon to: ';
  rsComputerName = 'Computer name: ';
  rsLogonWorkstationsChoice1 = 'All computers';
  rsLogonWorkstationsChoice2 = 'The following computers';

  rsLogonHoursSelectUTC = 'Select the allowed logon period in UTC';
  rsLogonAllowed = 'Logon allowed';
  rsLogonDenied = 'Logon denied';

  rsCurrentDirectoryServer = 'Current directory server: ';
  rsEmpty = '<empty>';
  rsChangeTo = 'Change to: ';
  rsAnyWritableDomainController = 'Any writable Domain Controller';
  rsThisDomainControllerOrADLDSInstance = 'This Domain Controller or AD LDS instance';
  rsSaveThisSettingForTheCurrentConsole = 'Save this setting for the current console';
  rsSite = 'Site';
  rsDCType = 'DC Type';
  rsDCVersion = 'DC Version';
  rsStatus = 'Status';

  rsProfile = 'Profile';
  rsServer = 'Server';

  rsComputerName2000 = 'Computer name (pre-Windows 2000): ';
  rsTheFollowingUserOrGroupCanJoinThisComputerToADomain = 'The following user or group can join this computer to a domain.';
  rsUserOrGroup = 'User or group: ';
  rsAssignedThisComputerAccountAsPre2000Computer = 'Assign this computer account as a pre-Windows 2000 computer';

  rsFirstName = 'First name: ';
  rsInitials = 'Initials: ';
  rsLastName = 'Last name: ';
  rsFullName = 'Full name: ';
  rsDisplayName = 'Display name: ';

  rsGroupName = 'Group name: ';
  rsGroupName2000 = 'Group name (pre-Windows 2000): ';
  rsGroupScope = 'Group scope';
  rsGroupType = 'Group type';
  rsDomainLocal = 'Domain local';
  rsGlobal = 'Global';
  rsUniversal = 'Universal';
  rsSecurity = 'Security';
  rsDistribution = 'Distribution';

  rsUserLogonName = 'User logon name: ';
  rsUserLogonNamePreWindows2000 = 'User logon name (pre-Windows 2000): ';
  rsWhenYouClickOKTheFollowingObjectWillBeCreated = 'When you click OK, the following object will be created: ';
  rsConfirm = 'Confirm:';
  rsUserCannotChangePassword = 'User cannot change password';
  rsPasswordNeverExpires = 'Password never expires';
  rsAccountIsDisabled = 'Account is disabled';

  rsCn = 'cn: ';

implementation

end.

