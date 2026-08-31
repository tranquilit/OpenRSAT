unit ugpocore;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.base,
  mormot.core.log,
  mormot.core.text,
  mormot.net.ldap,
  ucommon,
  ulog;

const
  // Group Policy Container (GPC) LDAP constants
  GPO_OBJECTCLASS = 'groupPolicyContainer';
  GPO_POLICIES_DN = 'CN=Policies,CN=System';

  GPO_ATTR_DISPLAYNAME = 'displayName';
  GPO_ATTR_DESCRIPTION = 'description';
  GPO_ATTR_FILESYSPATH = 'gPCFileSysPath';
  GPO_ATTR_VERSIONNUMBER = 'versionNumber';
  GPO_ATTR_FUNCTIONALITYVERSION = 'gPCFunctionalityVersion';
  GPO_ATTR_FLAGS = 'flags';
  GPO_ATTR_USEREXTENSIONNAMES = 'gPCUserExtensionNames';
  GPO_ATTR_MACHINEEXTENSIONNAMES = 'gPCMachineExtensionNames';
  GPO_ATTR_WQLFILTER = 'gPCWQLFilter';
  GPO_ATTR_WHENCREATED = 'whenCreated';
  GPO_ATTR_WHENCHANGED = 'whenChanged';

  // gpcFlags attribute values
  GPO_FLAG_ALLENABLED = 0;
  GPO_FLAG_USERDISABLED = 1;
  GPO_FLAG_MACHINEDISABLED = 2;
  GPO_FLAG_ALLDISABLED = 3;

  // Default gPCFunctionalityVersion of a new GPO (Windows 2008 and above)
  GPO_FUNCTIONALITYVERSION_DEFAULT = 2;

  // versionNumber attribute: low word is the user version, high word is the machine version
  GPO_USER_VERSION_MASK = $0000FFFF;
  GPO_MACHINE_VERSION_SHIFT = 16;

  // Attributes of the GPC that OpenRSAT knows and can manage.
  GPO_KNOWN_ATTRIBUTES: array of RawUtf8 = (
    'displayName', 'description', 'gPCFileSysPath', 'versionNumber',
    'gPCFunctionalityVersion', 'flags', 'gPCWQLFilter',
    'gPCUserExtensionNames', 'gPCMachineExtensionNames',
    'whenCreated', 'whenChanged', 'cn', 'distinguishedName',
    'showInAdvancedViewOnly');

  // Attributes copied from a GPO to its duplicate.
  DUPLICATED_GPO_ATTRIBUTES: array of RawUtf8 = (
    'description', 'gPCWQLFilter', 'gPCUserExtensionNames',
    'gPCMachineExtensionNames');

type

  { EGPOException }

  EGPOException = class(Exception)
  public
    constructor Create(const fmt: RawUtf8; const args: array of const); overload;
  end;

  { TGPO }

  TGPO = class
  private
    fAttributes: TLdapAttributeList;

    function GetDisplayName: RawUtf8;
    procedure SetDisplayName(AValue: RawUtf8);
    function GetDescription: RawUtf8;
    procedure SetDescription(AValue: RawUtf8);
    function GetDistinguishedName: RawUtf8;
    function GetFileSysPath: RawUtf8;
    function GetFlags: Integer;
    function GetFunctionalityVersion: Integer;
    function GetMachineExtensionNames: RawUtf8;
    function GetMachineVersion: Word;
    function GetName: RawUtf8;
    function GetUserExtensionNames: RawUtf8;
    function GetUserVersion: Word;
    function GetVersionNumber: Cardinal;
    procedure SetVersionNumber(AValue: Cardinal);
    function GetWhenChanged: RawUtf8;
    function GetWhenCreated: RawUtf8;
    function GetWQLFilter: RawUtf8;
    procedure SetWQLFilter(AValue: RawUtf8);
  public
    constructor Create; overload;
    constructor Create(AAttributes: TLdapAttributeList); overload;
    destructor Destroy; override;

    /// Rebuild the GPO values from its GPC attributes.
    procedure Refresh;

    // Raw GPC attributes
    property Attributes: TLdapAttributeList read fAttributes;
    // Readable helpers
    property DistinguishedName: RawUtf8 read GetDistinguishedName;
    property Name: RawUtf8 read GetName;
    property DisplayName: RawUtf8 read GetDisplayName write SetDisplayName;
    property Description: RawUtf8 read GetDescription write SetDescription;
    property FileSysPath: RawUtf8 read GetFileSysPath;
    property VersionNumber: Cardinal read GetVersionNumber write SetVersionNumber;
    property UserVersion: Word read GetUserVersion;
    property MachineVersion: Word read GetMachineVersion;
    property FunctionalityVersion: Integer read GetFunctionalityVersion;
    property Flags: Integer read GetFlags;
    property WQLFilter: RawUtf8 read GetWQLFilter write SetWQLFilter;
    property UserExtensionNames: RawUtf8 read GetUserExtensionNames;
    property MachineExtensionNames: RawUtf8 read GetMachineExtensionNames;
    property WhenCreated: RawUtf8 read GetWhenCreated;
    property WhenChanged: RawUtf8 read GetWhenChanged;
  end;

  TGPOList = array of TGPO;

  { TGPOModification }

  /// A single attribute change, staged before being applied to a GPO.
  /// An empty Value means the attribute should be removed.
  TGPOModification = record
    AttributeName: RawUtf8;
    Value: RawUtf8;
  end;
  TGPOModificationDynArray = array of TGPOModification;

  { TGPOCatalogItem }

  /// An attribute available in the GPO catalog, that can be added to a GPO.
  TGPOCatalogItem = record
    AttributeName: RawUtf8;
    Description: RawUtf8;
  end;
  TGPOCatalogItemDynArray = array of TGPOCatalogItem;

  { TGPOLogic }

  TGPOLogic = class
  private
    fLog: TSynLogClass;
    fLdapClient: TLdapClient;

    function GetDomainName: RawUtf8;
  public
    constructor Create(ALdapClient: TLdapClient);
    destructor Destroy; override;

    /// Compute the distinguished name of the Policies container of a domain.
    function GetPoliciesDN(ADomainDN: RawUtf8): RawUtf8;

    /// Generate the CN of a new GPO container, based on a new GUID.
    function NewGPCName: RawUtf8;

    /// Retrieve every GPO of a domain. The caller owns the returned objects.
    function List(ADomainDN: RawUtf8): TGPOList;

    /// Retrieve a GPO by its displayName, or nil when not found.
    /// The caller owns the returned object.
    function FindByName(ADomainDN, ADisplayName: RawUtf8): TGPO;

    /// Create a new GPO in a domain and return its distinguished name.
    function Create(ADomainDN, ADisplayName: RawUtf8): RawUtf8;

    /// Delete a GPO container and its children.
    function Delete(AGPO: TGPO): Boolean; overload;
    function Delete(ADistinguishedName: RawUtf8): Boolean; overload;

    /// Rename the displayName of a GPO.
    function Rename(AGPO: TGPO; ANewDisplayName: RawUtf8): Boolean;

    /// Duplicate a GPO (GPC part) under a new display name, and return the
    /// distinguished name of the new GPO. The caller owns no object.
    function Duplicate(AGPO: TGPO; ANewDisplayName: RawUtf8): RawUtf8;

    /// Update the configuration attributes of a GPO (flags, versions,
    /// description and WMI filter) in a single LDAP modify operation.
    function UpdateConfiguration(AGPO: TGPO; AFlags: Integer;
      AUserVersion, AMachineVersion: Word; ADescription, AWQLFilter: RawUtf8): Boolean;

    /// Apply a list of staged attribute changes to a GPO in a single LDAP
    /// modify operation. An empty Value removes the attribute.
    function ApplyModifications(AGPO: TGPO;
      const AModifications: array of TGPOModification): Boolean;
  end;

/// Pack the user and machine versions of a GPO into its versionNumber value.
function GPOPartsToVersionNumber(AUserVersion, AMachineVersion: Word): Cardinal;

/// Unpack the user and machine versions from a versionNumber value.
procedure GPOVersionToParts(AVersionNumber: Cardinal; out AUserVersion,
  AMachineVersion: Word);

/// Return the human readable status of a GPO, based on its flags value.
function GPOFlagsToText(AFlags: Integer): RawUtf8;

/// Extract the DNS domain name (e.g. openrsat.lan) from a domain DN
/// (e.g. DC=openrsat,DC=lan).
function GetDomainNameFromDN(const ADomainDN: RawUtf8): RawUtf8;

/// Extract the domain DN (e.g. DC=openrsat,DC=lan) from a GPO distinguished
/// name (e.g. cn={GUID},CN=Policies,CN=System,DC=openrsat,DC=lan).
function GetDomainDNFromGPODN(const AGPODN: RawUtf8): RawUtf8;

/// Return True when the attribute is part of the known GPC attribute catalog.
function IsKnownGPOAttribute(const AAttributeName: RawUtf8): Boolean;

/// Return the catalog of the GPC attributes that can be managed on a GPO.
function GPOAttributeCatalog: TGPOCatalogItemDynArray;

/// Count the extensions (technologies) declared in a gPC*ExtensionNames value.
function GPOExtensionsCount(const AExtensionNames: RawUtf8): Integer;

/// Count the number of occurrences of a pattern in a string.
function CountOccurrences(const AText, APattern: RawUtf8): Integer;

implementation

{ EGPOException }

constructor EGPOException.Create(const fmt: RawUtf8; const args: array of const);
begin
  inherited Create(FormatUtf8(fmt, args));
end;

{ TGPO }

function TGPO.GetDisplayName: RawUtf8;
var
  Attribute: TLdapAttribute;
begin
  result := '';
  Attribute := fAttributes.Find(GPO_ATTR_DISPLAYNAME);
  if Assigned(Attribute) then
    result := Attribute.GetReadable();
end;

procedure TGPO.SetDisplayName(AValue: RawUtf8);
var
  Attribute: TLdapAttribute;
begin
  Attribute := fAttributes.Find(GPO_ATTR_DISPLAYNAME);
  if not Assigned(Attribute) then
    Attribute := fAttributes.Add(GPO_ATTR_DISPLAYNAME);
  Attribute.Clear;
  Attribute.Add(AValue);
end;

function TGPO.GetDescription: RawUtf8;
var
  Attribute: TLdapAttribute;
begin
  result := '';
  Attribute := fAttributes.Find(GPO_ATTR_DESCRIPTION);
  if Assigned(Attribute) then
    result := Attribute.GetReadable();
end;

procedure TGPO.SetDescription(AValue: RawUtf8);
var
  Attribute: TLdapAttribute;
begin
  Attribute := fAttributes.Find(GPO_ATTR_DESCRIPTION);
  if not Assigned(Attribute) then
    Attribute := fAttributes.Add(GPO_ATTR_DESCRIPTION);
  Attribute.Clear;
  Attribute.Add(AValue);
end;

function TGPO.GetWQLFilter: RawUtf8;
var
  Attribute: TLdapAttribute;
begin
  result := '';
  Attribute := fAttributes.Find(GPO_ATTR_WQLFILTER);
  if Assigned(Attribute) then
    result := Attribute.GetReadable();
end;

procedure TGPO.SetWQLFilter(AValue: RawUtf8);
var
  Attribute: TLdapAttribute;
begin
  Attribute := fAttributes.Find(GPO_ATTR_WQLFILTER);
  if not Assigned(Attribute) then
    Attribute := fAttributes.Add(GPO_ATTR_WQLFILTER);
  Attribute.Clear;
  Attribute.Add(AValue);
end;

function TGPO.GetUserExtensionNames: RawUtf8;
var
  Attribute: TLdapAttribute;
begin
  result := '';
  Attribute := fAttributes.Find(GPO_ATTR_USEREXTENSIONNAMES);
  if Assigned(Attribute) then
    result := Attribute.GetReadable();
end;

function TGPO.GetMachineExtensionNames: RawUtf8;
var
  Attribute: TLdapAttribute;
begin
  result := '';
  Attribute := fAttributes.Find(GPO_ATTR_MACHINEEXTENSIONNAMES);
  if Assigned(Attribute) then
    result := Attribute.GetReadable();
end;

function TGPO.GetDistinguishedName: RawUtf8;
var
  Attribute: TLdapAttribute;
begin
  result := '';
  Attribute := fAttributes.Find('distinguishedName');
  if Assigned(Attribute) then
    result := Attribute.GetReadable();
end;

function TGPO.GetFileSysPath: RawUtf8;
var
  Attribute: TLdapAttribute;
begin
  result := '';
  Attribute := fAttributes.Find(GPO_ATTR_FILESYSPATH);
  if Assigned(Attribute) then
    result := Attribute.GetReadable();
end;

function TGPO.GetFlags: Integer;
var
  Attribute: TLdapAttribute;
begin
  result := GPO_FLAG_ALLENABLED;
  Attribute := fAttributes.Find(GPO_ATTR_FLAGS);
  if Assigned(Attribute) then
    TryStrToInt(Attribute.GetReadable(), result);
end;

function TGPO.GetFunctionalityVersion: Integer;
var
  Attribute: TLdapAttribute;
begin
  result := 0;
  Attribute := fAttributes.Find(GPO_ATTR_FUNCTIONALITYVERSION);
  if Assigned(Attribute) then
    TryStrToInt(Attribute.GetReadable(), result);
end;

function TGPO.GetMachineVersion: Word;
var
  AUserVersion: Word;
begin
  GPOVersionToParts(GetVersionNumber, AUserVersion, result);
end;

function TGPO.GetName: RawUtf8;
var
  Attribute: TLdapAttribute;
begin
  result := '';
  Attribute := fAttributes.Find('cn');
  if Assigned(Attribute) then
    result := Attribute.GetReadable();
end;

function TGPO.GetUserVersion: Word;
var
  AMachineVersion: Word;
begin
  GPOVersionToParts(GetVersionNumber, result, AMachineVersion);
end;

function TGPO.GetVersionNumber: Cardinal;
var
  Attribute: TLdapAttribute;
begin
  result := 0;
  Attribute := fAttributes.Find(GPO_ATTR_VERSIONNUMBER);
  if Assigned(Attribute) then
    TryStrToDWord(Attribute.GetReadable(), result);
end;

procedure TGPO.SetVersionNumber(AValue: Cardinal);
var
  Attribute: TLdapAttribute;
begin
  Attribute := fAttributes.Find(GPO_ATTR_VERSIONNUMBER);
  if not Assigned(Attribute) then
    Attribute := fAttributes.Add(GPO_ATTR_VERSIONNUMBER);
  Attribute.Clear;
  Attribute.Add(FormatUtf8('%', [AValue]));
end;

function TGPO.GetWhenChanged: RawUtf8;
var
  Attribute: TLdapAttribute;
begin
  result := '';
  Attribute := fAttributes.Find(GPO_ATTR_WHENCHANGED);
  if Assigned(Attribute) then
    result := Attribute.GetReadable();
end;

function TGPO.GetWhenCreated: RawUtf8;
var
  Attribute: TLdapAttribute;
begin
  result := '';
  Attribute := fAttributes.Find(GPO_ATTR_WHENCREATED);
  if Assigned(Attribute) then
    result := Attribute.GetReadable();
end;

constructor TGPO.Create;
begin
  fAttributes := TLdapAttributeList.Create;
end;

constructor TGPO.Create(AAttributes: TLdapAttributeList);
begin
  if not Assigned(AAttributes) then
    raise EGPOException.Create('Attributes of the GPO not assigned', []);

  fAttributes := TLdapAttributeList(AAttributes.Clone);
end;

destructor TGPO.Destroy;
begin
  FreeAndNil(fAttributes);

  inherited Destroy;
end;

procedure TGPO.Refresh;
begin
  // Getters read the values from the attributes at each call.
end;

{ TGPOLogic }

function GetDomainNameFromDN(const ADomainDN: RawUtf8): RawUtf8;
var
  Pairs: TNameValueDNs;
  i: Integer;
begin
  result := '';
  if not ParseDN(ADomainDN, Pairs) then
    Exit;

  for i := 0 to High(Pairs) do
  begin
    if SameText(Pairs[i].Name, 'dc') then
    begin
      if (result <> '') then
        result := result + '.';
      result := result + Pairs[i].Value;
    end;
  end;
end;

function GetDomainDNFromGPODN(const AGPODN: RawUtf8): RawUtf8;
var
  CommaPos: Integer;
  i: Integer;
begin
  // A GPO DN looks like: cn={GUID},CN=Policies,CN=System,DC=domain,DC=lan
  // The domain DN is everything after the three first components.
  result := '';
  CommaPos := 0;
  for i := 1 to 3 do
  begin
    CommaPos := PosEx(',', AGPODN, CommaPos + 1);
    if (CommaPos = 0) then
      Exit;
  end;
  result := Copy(AGPODN, CommaPos + 1, MaxInt);
end;

function TGPOLogic.GetDomainName: RawUtf8;
begin
  // Build the domain name from the default naming context.
  result := GetDomainNameFromDN(fLdapClient.DefaultDN);
end;

constructor TGPOLogic.Create(ALdapClient: TLdapClient);
begin
  fLog := TOpenRSATLog;
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, 'Create', Self);

  fLdapClient := ALdapClient;
end;

destructor TGPOLogic.Destroy;
begin
  inherited Destroy;
end;

function TGPOLogic.GetPoliciesDN(ADomainDN: RawUtf8): RawUtf8;
begin
  result := FormatUtf8('%,%', [GPO_POLICIES_DN, ADomainDN]);
end;

function TGPOLogic.NewGPCName: RawUtf8;
var
  Guid: TGuid;
begin
  if (CreateGUID(Guid) <> 0) then
    raise EGPOException.Create('Unable to generate a new GUID', []);

  result := UpperCase(GUIDToString(Guid));
end;

function TGPOLogic.List(ADomainDN: RawUtf8): TGPOList;
var
  SearchResult: TLdapResult;
  Filter: RawUtf8;
begin
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, 'List GPOs of "%"', [ADomainDN], Self);

  result := nil;

  fLdapClient.SearchBegin();
  try
    fLdapClient.SearchScope := lssSingleLevel;
    Filter := FormatUtf8('(objectClass=%)', [GPO_OBJECTCLASS]);
    repeat
      if not fLdapClient.Search(GetPoliciesDN(ADomainDN), False, Filter, ['*']) then
        Exit;

      for SearchResult in fLdapClient.SearchResult.Items do
      begin
        if not Assigned(SearchResult) then
          Continue;
        SetLength(result, Length(result) + 1);
        result[High(result)] := TGPO.Create(SearchResult.Attributes);
      end;
    until fLdapClient.SearchCookie = '';
  finally
    fLdapClient.SearchEnd;
  end;
end;

function TGPOLogic.FindByName(ADomainDN, ADisplayName: RawUtf8): TGPO;
var
  GPOs: TGPOList;
  i: Integer;
begin
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, 'Find GPO by name "%"', [ADisplayName], Self);

  result := nil;

  GPOs := List(ADomainDN);
  try
    for i := 0 to High(GPOs) do
    begin
      if SameText(GPOs[i].DisplayName, ADisplayName) then
      begin
        result := GPOs[i];
        GPOs[i] := nil;
        Exit;
      end;
    end;
  finally
    for i := 0 to High(GPOs) do
      GPOs[i].Free;
  end;
end;

function TGPOLogic.Create(ADomainDN, ADisplayName: RawUtf8): RawUtf8;
var
  Attributes: TLdapAttributeList;
  Attr: TLdapAttribute;
  GPCName, PoliciesDN, DomainName: RawUtf8;
begin
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, 'Create GPO "%" in "%"', [ADisplayName, ADomainDN], Self);

  result := '';

  if (ADisplayName = '') then
    raise EGPOException.Create('Display name of the GPO not assigned', []);

  if Assigned(FindByName(ADomainDN, ADisplayName)) then
    raise EGPOException.Create('A GPO with the display name "%" already exists', [ADisplayName]);

  GPCName := NewGPCName;
  PoliciesDN := GetPoliciesDN(ADomainDN);
  result := FormatUtf8('cn=%,%', [GPCName, PoliciesDN]);

  Attributes := TLdapAttributeList.Create;
  try
    Attr := Attributes.Add('objectClass', 'top');
    Attr.Add('container');
    Attr.Add(GPO_OBJECTCLASS);
    Attributes.Add('cn', GPCName);
    Attributes.Add(GPO_ATTR_DISPLAYNAME, ADisplayName);

    DomainName := GetDomainName;
    Attributes.Add(GPO_ATTR_FILESYSPATH,
      FormatUtf8('\\%\%\Policies\%', [DomainName, DomainName, GPCName]));

    Attributes.Add(GPO_ATTR_VERSIONNUMBER, '0');
    Attributes.Add(GPO_ATTR_FUNCTIONALITYVERSION,
      FormatUtf8('%', [GPO_FUNCTIONALITYVERSION_DEFAULT]));
    Attributes.Add(GPO_ATTR_FLAGS, '0');
    Attributes.Add('showInAdvancedViewOnly', 'TRUE');

    if not fLdapClient.Add(result, Attributes) then
      result := '';
  finally
    Attributes.Free;
  end;
end;

function TGPOLogic.Delete(AGPO: TGPO): Boolean;
begin
  result := False;
  if not Assigned(AGPO) then
    Exit;
  result := Delete(AGPO.DistinguishedName);
end;

function TGPOLogic.Delete(ADistinguishedName: RawUtf8): Boolean;
begin
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, 'Delete GPO "%"', [ADistinguishedName], Self);

  result := False;
  if (ADistinguishedName = '') then
    Exit;

  // A GPC contains cn=Machine and cn=User children: delete them too.
  result := fLdapClient.Delete(ADistinguishedName, True);
end;

function TGPOLogic.Rename(AGPO: TGPO; ANewDisplayName: RawUtf8): Boolean;
begin
  result := False;

  if not Assigned(AGPO) then
    Exit;

  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, 'Rename GPO "%" to "%"', [AGPO.DisplayName, ANewDisplayName], Self);

  if (ANewDisplayName = '') then
    raise EGPOException.Create('Display name of the GPO not assigned', []);

  if (SameText(AGPO.DisplayName, ANewDisplayName)) then
  begin
    result := True;
    Exit;
  end;

  result := fLdapClient.Modify(AGPO.DistinguishedName, lmoReplace,
    GPO_ATTR_DISPLAYNAME, ANewDisplayName);
end;

function TGPOLogic.Duplicate(AGPO: TGPO; ANewDisplayName: RawUtf8): RawUtf8;
var
  Attributes: TLdapAttributeList;
  Attr: TLdapAttribute;
  GPCName, PoliciesDN, DomainName: RawUtf8;
  Source: TLdapAttribute;
  i: Integer;
begin
  result := '';

  if not Assigned(AGPO) then
    raise EGPOException.Create('GPO not assigned', []);

  if (ANewDisplayName = '') then
    raise EGPOException.Create('Display name of the GPO not assigned', []);

  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, 'Duplicate GPO "%" to "%"', [AGPO.DisplayName,
      ANewDisplayName], Self);

  GPCName := NewGPCName;
  PoliciesDN := GetPoliciesDN(GetDomainDNFromGPODN(AGPO.DistinguishedName));
  result := FormatUtf8('cn=%,%', [GPCName, PoliciesDN]);

  Attributes := TLdapAttributeList.Create;
  try
    Attr := Attributes.Add('objectClass', 'top');
    Attr.Add('container');
    Attr.Add(GPO_OBJECTCLASS);
    Attributes.Add('cn', GPCName);
    Attributes.Add(GPO_ATTR_DISPLAYNAME, ANewDisplayName);

    DomainName := GetDomainName;
    Attributes.Add(GPO_ATTR_FILESYSPATH,
      FormatUtf8('\\%\%\Policies\%', [DomainName, DomainName, GPCName]));

    // Copy the configuration attributes that make sense on the copy.
    Attributes.Add(GPO_ATTR_VERSIONNUMBER, '0');
    Attributes.Add(GPO_ATTR_FUNCTIONALITYVERSION,
      FormatUtf8('%', [AGPO.FunctionalityVersion]));
    Attributes.Add(GPO_ATTR_FLAGS, FormatUtf8('%', [AGPO.Flags]));
    Attributes.Add('showInAdvancedViewOnly', 'TRUE');

    for i := 0 to High(DUPLICATED_GPO_ATTRIBUTES) do
    begin
      Source := AGPO.Attributes.Find(DUPLICATED_GPO_ATTRIBUTES[i]);
      if Assigned(Source) then
        Attributes.Add(Source.AttributeName, Source.GetReadable());
    end;

    if not fLdapClient.Add(result, Attributes) then
      result := '';
  finally
    Attributes.Free;
  end;
end;

function TGPOLogic.ApplyModifications(AGPO: TGPO;
  const AModifications: array of TGPOModification): Boolean;
var
  Modifications: array of RawByteString;
  i: Integer;
  Attribute: TLdapAttribute;

  procedure AddModification(AOp: TLdapModifyOp; const AAttribute: RawByteString);
  begin
    SetLength(Modifications, Length(Modifications) + 1);
    Modifications[High(Modifications)] := Modifier(AOp, AAttribute);
  end;

  procedure AddModification(AOp: TLdapModifyOp; const AName: RawUtf8;
    const AValue: RawByteString);
  begin
    SetLength(Modifications, Length(Modifications) + 1);
    Modifications[High(Modifications)] := Modifier(AOp, AName, AValue);
  end;

  procedure UpdateAttribute(const AName, ANewValue, ACurrentValue: RawUtf8);
  var
    Attribute: TLdapAttribute;
  begin
    if (ANewValue = ACurrentValue) then
      Exit;

    Attribute := AGPO.Attributes.Find(AName);
    if (ANewValue = '') then
    begin
      // Removing the value: delete the whole attribute when it exists,
      // otherwise there is nothing to do. (AD rejects Replace on a missing
      // attribute with noSuchAttribute, and empty values are invalid for
      // DN-syntax attributes such as gPCWQLFilter.)
      if Assigned(Attribute) then
        AddModification(lmoDelete, Attribute.ExportToAsnSeq);
      Exit;
    end;

    // Setting a value: Add creates a missing attribute, Replace updates an
    // existing one. (AD rejects Replace on a missing attribute.)
    if Assigned(Attribute) then
      AddModification(lmoReplace, AName, ANewValue)
    else
      AddModification(lmoAdd, AName, ANewValue);
  end;
begin
  result := False;

  if not Assigned(AGPO) then
    Exit;

  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, 'Apply % modification(s) on GPO "%"',
      [Length(AModifications), AGPO.DisplayName], Self);

  for i := 0 to High(AModifications) do
  begin
    if (AModifications[i].AttributeName = '') then
      Continue;
    Attribute := AGPO.Attributes.Find(AModifications[i].AttributeName);
    if Assigned(Attribute) then
      UpdateAttribute(AModifications[i].AttributeName, AModifications[i].Value,
        Attribute.GetReadable())
    else
      UpdateAttribute(AModifications[i].AttributeName, AModifications[i].Value, '');
  end;

  if (Length(Modifications) = 0) then
  begin
    // Nothing changed: nothing to send.
    result := True;
    Exit;
  end;

  result := fLdapClient.Modify(AGPO.DistinguishedName, Modifications);
end;

function TGPOLogic.UpdateConfiguration(AGPO: TGPO; AFlags: Integer;
  AUserVersion, AMachineVersion: Word; ADescription, AWQLFilter: RawUtf8): Boolean;
var
  Modifications: TGPOModificationDynArray;
begin
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, 'Update configuration of GPO "%"', [AGPO.DisplayName], Self);

  SetLength(Modifications, 4);
  Modifications[0].AttributeName := GPO_ATTR_FLAGS;
  Modifications[0].Value := FormatUtf8('%', [AFlags]);
  Modifications[1].AttributeName := GPO_ATTR_VERSIONNUMBER;
  Modifications[1].Value := FormatUtf8('%',
    [GPOPartsToVersionNumber(AUserVersion, AMachineVersion)]);
  Modifications[2].AttributeName := GPO_ATTR_DESCRIPTION;
  Modifications[2].Value := ADescription;
  Modifications[3].AttributeName := GPO_ATTR_WQLFILTER;
  Modifications[3].Value := AWQLFilter;

  result := ApplyModifications(AGPO, Modifications);
end;

{ GPO helpers }

function CountOccurrences(const AText, APattern: RawUtf8): Integer;
var
  Offset: Integer;
begin
  result := 0;
  if (APattern = '') then
    Exit;

  Offset := 1;
  while (Offset <= Length(AText)) do
  begin
    Offset := PosEx(APattern, AText, Offset);
    if (Offset = 0) then
      Break;
    Inc(result);
    Inc(Offset, Length(APattern));
  end;
end;

function GPOExtensionsCount(const AExtensionNames: RawUtf8): Integer;
begin
  result := CountOccurrences(AExtensionNames, '{');
end;

function IsKnownGPOAttribute(const AAttributeName: RawUtf8): Boolean;
var
  i: Integer;
begin
  result := False;
  for i := 0 to High(GPO_KNOWN_ATTRIBUTES) do
    if SameText(GPO_KNOWN_ATTRIBUTES[i], AAttributeName) then
      Exit(True);
end;

function GPOAttributeCatalog: TGPOCatalogItemDynArray;
var
  i: Integer;
begin
  SetLength(result, Length(GPO_KNOWN_ATTRIBUTES));
  for i := 0 to High(GPO_KNOWN_ATTRIBUTES) do
  begin
    result[i].AttributeName := GPO_KNOWN_ATTRIBUTES[i];
    case LowerCase(GPO_KNOWN_ATTRIBUTES[i]) of
      'displayname': result[i].Description := rsGPOCatalogDisplayName;
      'description': result[i].Description := rsGPOCatalogDescription;
      'gpcfilesyspath': result[i].Description := rsGPOCatalogFileSysPath;
      'versionnumber': result[i].Description := rsGPOCatalogVersionNumber;
      'gpcfunctionalityversion': result[i].Description := rsGPOCatalogFunctionalityVersion;
      'flags': result[i].Description := rsGPOCatalogFlags;
      'gpcwqlfilter': result[i].Description := rsGPOCatalogWQLFilter;
      'gpcuserextensionnames': result[i].Description := rsGPOCatalogUserExtensions;
      'gpcmachineextensionnames': result[i].Description := rsGPOCatalogMachineExtensions;
      'whencreated': result[i].Description := rsGPOCatalogWhenCreated;
      'whenchanged': result[i].Description := rsGPOCatalogWhenChanged;
      'cn': result[i].Description := rsGPOCatalogCN;
      'distinguishedname': result[i].Description := rsGPOCatalogDistinguishedName;
      'showinadvancedviewonly': result[i].Description := rsGPOCatalogShowInAdvanced;
    end;
  end;
end;

{ GPO helpers }

function GPOPartsToVersionNumber(AUserVersion, AMachineVersion: Word): Cardinal;
begin
  result := (Cardinal(AMachineVersion) shl GPO_MACHINE_VERSION_SHIFT) or
    (Cardinal(AUserVersion) and GPO_USER_VERSION_MASK);
end;

procedure GPOVersionToParts(AVersionNumber: Cardinal; out AUserVersion,
  AMachineVersion: Word);
begin
  AUserVersion := Word(AVersionNumber and GPO_USER_VERSION_MASK);
  AMachineVersion := Word((AVersionNumber shr GPO_MACHINE_VERSION_SHIFT) and
    GPO_USER_VERSION_MASK);
end;

function GPOFlagsToText(AFlags: Integer): RawUtf8;
begin
  case AFlags of
    GPO_FLAG_USERDISABLED: result := rsGPOStatusUserDisabled;
    GPO_FLAG_MACHINEDISABLED: result := rsGPOStatusMachineDisabled;
    GPO_FLAG_ALLDISABLED: result := rsGPOStatusAllDisabled;
    else
      result := rsGPOStatusAllEnabled;
  end;
end;

end.
