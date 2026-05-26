unit ustrconsts;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

type

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

implementation

end.

