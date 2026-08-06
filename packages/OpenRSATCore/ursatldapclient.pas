unit ursatldapclient;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  System.UITypes,
  mormot.core.base,
  mormot.core.os.security,
  mormot.core.variants,
  mormot.net.ldap,
  ucommon,
  ulog;

type

  TLdapConnectionTransport = (
    /// LDAPS direct, généralement sur le port 636.
    ldctTls,

    /// LDAP non chiffré, généralement sur le port 389.
    ldctPlain
  );

  TLdapAuthenticationMode = (
    /// Bind anonyme.
    ldamAnonymous,

    /// Simple Bind avec UserName/Password.
    ldamSimple,

    /// Bind SASL DIGEST.
    ldamSaslDigest,

    /// Bind SASL Kerberos/GSSAPI.
    ldamKerberos
  );

  TLdapCredentials = record
    UserName: RawUtf8;
    Password: SpiUtf8;
    Authentication: TLdapAuthenticationMode;
    AllowUnsafePasswordBind: Boolean;
    KerberosDisableChannelBinding: Boolean;
    KerberosSignSeal: TLdapKerberosSignSeal;
    KerberosCredentialFile: RawUtf8;
    KerberosAuthIdentity: RawUtf8;
  end;

  TLdapConnectionSettings = record
    TargetHost: RawUtf8;
    Port: RawUtf8;
    Transport: TLdapConnectionTransport;
    IgnoreCertificateErrors: Boolean;
    DiscoverWhenHostEmpty: Boolean;
    UseCldapDiscovery: Boolean;
    SelectClosestServer: Boolean;
    TryTlsFirst: Boolean;
    DiscoveryDelayMS: Integer;
    TimeoutMS: Integer;
    PingIdleSeconds: Integer;
    AutoReconnect: Boolean;
    KerberosDN: RawUtf8;
    KerberosSPN: RawUtf8;
  end;

  TLdapErrorKind = (
    lekNone,
    lekNotFound,
    lekAuthentication,
    lekAuthorization,
    lekValidation,
    lekNetwork,
    lekTls,
    lekServer,
    lekCancelled,
    lekUnknown
  );

  TLdapOperationResult = record
    Success: Boolean;
    ErrorKind: TLdapErrorKind;
    LdapCode: Integer;
    Message: RawUtf8;
    DistinguishedName: RawUtf8;
    ElapsedTimeMS: Int64;
  end;

  TLdapSearchRequestOptions = record
    SizeLimit: Integer;
    TimeLimitSeconds: Integer;
    PageSize: Integer;
    SearchSDFlags: TLdapSearchSDFlags;
  end;

  TLdapSearchRequest = record
    Scope: TLdapSearchScope;
    BaseDN: RawUtf8;
    Attributes: TRawUtf8DynArray;
    Filter: RawUtf8;
    Options: TLdapSearchRequestOptions;
  end;

  TLdapAttributeData = record
    Name: RawUtf8;
    Values: TRawByteStringDynArray;
  end;

  PLdapAttributeData = ^TLdapAttributeData;
  TLdapAttributeDataDynArray = Array of TLdapAttributeData;

  TLdapEntryData = record
    DistinguishedName: RawUtf8;
    AttributeCount: Integer;
    Attributes: TLdapAttributeDataDynArray;
  end;

  PLdapEntryData = ^TLdapEntryData;
  TLdapEntryDataDynArray = Array of TLdapEntryData;

  TLdapSearchStatus = (
    lssOk,
    lssPartial,
    lssInvalidRequest,
    lssConnectionError,
    lssLdapError,
    lssInternalError
  );

  TLdapSearchResult = record
    Status: TLdapSearchStatus;
    OperationResult: TLdapOperationResult;
    Entries: TLdapEntryDataDynArray;
    ReturnedCount: Integer;
  end;

  TLdapAddRequest = record
    DistinguishedName: RawUtf8;
    Attributes: TLdapAttributeDataDynArray;
  end;

  TLdapModifyChange = record
    Operation: TLdapModifyOp;
    Attribute: TLdapAttributeData;
  end;

  TLdapModifyChanges = Array of TLdapModifyChange;

  TLdapModifyRequest = record
    DistinguishedName: RawUtf8;
    Changes: TLdapModifyChanges;
  end;

  TLdapDeleteRequest = record
    DistinguishedName: RawUtf8;
    DeleteChildren: Boolean;
  end;

  TLdapModifyDNRequest = record
    DistinguishedName: RawUtf8;
    NewRDN: RawUtf8;
    NewSuperior: RawUtf8;
    DeleteOldRDN: Boolean;
  end;

  ILdapConnectionContext = Interface
    function NamingContexts: TRawUtf8DynArray;
    function DefaultNamingContext: RawUtf8;
    function RootNamingContext: RawUtf8;
    function ConfigNamingContext: RawUtf8;
    function SchemaNamingContext: RawUtf8;
  end;

  { TMormotLdapConnectionContext }

  TMormotLdapConnectionContext = class(TInterfacedObject, ILdapConnectionContext)
  private
    fLdapClient: TLdapClient;
  public
    constructor Create(const LdapClient: TLdapClient);

    function NamingContexts: TRawUtf8DynArray;
    function DefaultNamingContext: RawUtf8;
    function RootNamingContext: RawUtf8;
    function ConfigNamingContext: RawUtf8;
    function SchemaNamingContext: RawUtf8;
  end;

  { ILdapConnection }

  ILdapConnection = Interface
    function Connect(const ASettings: TLdapConnectionSettings): TLdapOperationResult;
    function Bind(const ACredentials: TLdapCredentials): TLdapOperationResult;
    procedure Disconnect;
    function IsConnected: Boolean;
    function Search(const ARequest: TLdapSearchRequest): TLdapSearchResult;
    function Add(const ARequest: TLdapAddRequest): TLdapOperationResult;
    function Modify(const ARequest: TLdapModifyRequest): TLdapOperationResult;
    function Delete(const ARequest: TLdapDeleteRequest): TLdapOperationResult;
    function ModifyDN(const ARequest: TLdapModifyDNRequest): TLdapOperationResult;
    function Context: ILdapConnectionContext;
  end;

  { TMormotLdapConnection }

  TMormotLdapConnection = class(TInterfacedObject, ILdapConnection)
  private
    fLdapClient: TLdapClient;
    fContext: ILdapConnectionContext;

    procedure LdapOperationResult(out r: TLdapOperationResult; Success: Boolean;
      const DistinguishedName: RawUtf8; Code: Integer; const Message: RawUtf8;
      Error: TLdapError; ElapsedTimeMS: Int64);
  public
    constructor Create;
    destructor destroy; override;

    function Connect(const ASettings: TLdapConnectionSettings
  ): TLdapOperationResult;
    function Bind(const ACredentials: TLdapCredentials): TLdapOperationResult;
    procedure Disconnect;
    function IsConnected: Boolean;
    function Search(const ARequest: TLdapSearchRequest): TLdapSearchResult;
    function Add(const ARequest: TLdapAddRequest): TLdapOperationResult;
    function Modify(const ARequest: TLdapModifyRequest): TLdapOperationResult;
    function Delete(const ARequest: TLdapDeleteRequest): TLdapOperationResult;
    function ModifyDN(const ARequest: TLdapModifyDNRequest): TLdapOperationResult;
    function Context: ILdapConnectionContext;
  end;

  TLdapSessionManager = class
  private
    fLdapConnection: ILdapConnection;
  public

  end;

  { ILdapAttribute }

  //ILdapAttribute = Interface
  //  function GetName: RawUtf8;
  //  function IsModified: Boolean;
  //  procedure Add(const Value: RawByteString);
  //  procedure Replace(const Value: RawByteString);
  //  procedure Delete;
  //  function GetRaw(Idx: Integer = 0): RawByteString;
  //  function GetReadable(Idx: Integer = 0): RawUtf8;
  //  function GetAllRaw: TRawByteStringDynArray;
  //  function GetAllReadable: TRawUtf8DynArray;
  //
  //  property Name: RawUtf8 read GetName write SetName;
  //end;

  { TMormotLdapAttribute }

  //TMormotLdapAttribute = class(TInterfacedObjectClass, ILdapAttribute)
  //private
  //  fName: RawUtf8;
  //  fCurrentValues: TRawByteStringDynArray;
  //  fOriginalValues: TRawByteStringDynArray;
  //
  //  function GetName: RawUtf8;
  //public
  //  constructor Create(const AttributeName: RawUtf8; const AttributeValues: TRawByteStringDynArray);
  //
  //  procedure Add(const Value: RawByteString);
  //  procedure Replace(const Value: RawByteString);
  //  procedure Delete;
  //end;

  { ILdapObject }

  //ILdapObject = Interface
  //  procedure LoadAttributes(const Attributes: TRawUtf8DynArray);
  //  procedure Refresh;
  //  procedure Commit;
  //
  //  procedure Delete;
  //  procedure MoveTo(const AParentDN: RawUtf8);
  //  procedure Rename(const ANewRDN: RawUtf8);
  //  procedure DiscardChanges;
  //  function HasObjectClass(const AObjectClass: RawUtf8): Boolean;
  //  function IsNew: Boolean;
  //  function IsDeleted: Boolean;
  //  function IsDirty: Boolean;
  //
  //  function GetRaw(const Name: RawUtf8; Idx: Integer = 0): RawByteString;
  //  function GetReadable(const Name: RawUt8; Idx: Integer = 0): RawUtf8;
  //  function GetAllRaw(const Name: RawUtf8): TRawByteStringDynArray;
  //  function GetAllReadable(const Name: RawUtf8): TRawUtf8DynArray;
  //end;

  { TMormotLdapObject }

  //TMormotLdapObject = class(TInterfacedObjectClass, ILdapObject)
  //private
  //  fLdap: ILdapConnection;
  //  fIdentity: RawUtf8;
  //public
  //  constructor Create(const Ldap: ILdapConnection; const DistinguishedName: RawUtf8);
  //
  //  procedure LoadAttributes(const Attributes: TRawUtf8DynArray);
  //  procedure Refresh;
  //  procedure Commit;
  //end;

  TProcLdapClientObject = procedure(LdapClient: TLdapClient) of Object;

  { TRsatLdapClient }

  TRsatLdapClient = class(TLdapClient)
  private
    function AddProtection(PSecDesc: PSecurityDescriptor; Sid: RawSid): Boolean;
    function DelProtection(PSecDesc: PSecurityDescriptor; Sid: RawSid): Boolean;
  protected
    fPageNumber: Integer;
    fSearchAllResult: TLdapResultObjArray;
  public
    procedure SearchPagingBegin(PageNumber: Integer);
    function SearchAllDocPaged(DocResult: PDocVariantData; const BaseDN: RawUtf8;
      TypesOnly: boolean; const Filter: RawUtf8;
      const Attributes: array of RawUtf8): Boolean;
    procedure SearchPagingEnd;
    function MoveLdapEntry(oldDN, newDN: string): Boolean;
    function RenameLdapEntry(DN, newName: string): Boolean;

    procedure ChangeSettings(ASettings: TLdapClientSettings; AutoConnect: Boolean = True);

    function CreateOrganizationalUnit(OUName, ParentDN: RawUtf8; Protected: Boolean
      ): RawUtf8;
    function CreateGroup(GroupName, ParentDN: RawUtf8; JoinGroups: TRawUtf8DynArray = nil; AddMembers: TRawUtf8DynArray = nil): RawUtf8;
    function CreateUser(UserName, ParentDN: RawUtf8; JoinGroups: TRawUtf8DynArray = nil): RawUtf8;
    function SetOUProtection(DistinguishedName: RawUtf8; Protected: Boolean): Boolean;

    procedure OrderAcl(DN, BaseDN: RawUtf8; Acl: PSecAcl);
  public
    function Search(const Attributes: TLdapAttributeTypes; const Filter: RawUtf8='';
      const BaseDN: RawUtf8=''; TypesOnly: boolean=false): boolean; overload;
    function Search(const BaseDN: RawUtf8; TypesOnly: boolean; const Filter: RawUtf8;
      const Attributes: array of RawUtf8): boolean; overload;
    function SearchObject(const ObjectDN, Filter, Attribute: RawUtf8;
  Scope: TLdapSearchScope=lssBaseObject): TLdapAttribute; overload;
    function SearchObject(const ObjectDN, Filter: RawUtf8;
      const Attributes: array of RawUtf8; Scope: TLdapSearchScope=lssBaseObject
      ): TLdapResult; overload;
    function SearchObject(Attribute: TLdapAttributeType; const ObjectDN,
      Filter: RawUtf8; Scope: TLdapSearchScope=lssBaseObject): TLdapAttribute;
      overload;
    function SearchObject(const Attributes: TLdapAttributeTypes;
      const ObjectDN, Filter: RawUtf8; Scope: TLdapSearchScope=lssBaseObject
      ): TLdapResult; overload;
    function Modify(const Obj: RawUtf8; const Modifications: array of TAsnObject
      ): boolean; overload;
    function Modify(const Obj: RawUtf8; Op: TLdapModifyOp;
      const Types: array of TLdapAttributeType;
      const Values: array of const): boolean; overload;
    function Modify(const Obj: RawUtf8; Op: TLdapModifyOp; const AttrName: RawUtf8;
  const AttrValue: RawByteString): boolean; overload;
    function Modify(const Obj: RawUtf8; Op: TLdapModifyOp; Attribute: TLdapAttribute
  ): boolean; overload;
    function Modify(const Obj: RawUtf8; Op: TLdapModifyOp;
      AttrType: TLdapAttributeType; const AttrValue: RawByteString): boolean;
      overload;
    function ModifyDN(const Obj, NewRdn, NewSuperior: RawUtf8;
      DeleteOldRdn: boolean): boolean;
    function ModifyUserPassword(const UserDN: RawUtf8; const OldPassword,
      NewPassword: SpiUtf8): boolean;
    function Add(const Obj: RawUtf8; Value: TLdapAttributeList): boolean;
    function Delete(const Obj: RawUtf8; DeleteChildren: boolean=false): boolean;
    function Connect(DiscoverMode: TLdapClientConnect=[lccCldap, lccTlsFirst];
      DelayMS: integer=500): boolean;
    function Close: boolean;
  private
    fOnConnect: TNotifyEvent;
    fOnClose: TNotifyEvent;
    fOnError: TNotifyEvent;

    procedure SetOnConnect(AValue: TNotifyEvent);
    procedure SetOnClose(AValue: TNotifyEvent);
    procedure SetOnError(AValue: TNotifyEvent);
  published
    property OnConnect: TNotifyEvent read fOnConnect write SetOnConnect;
    property OnClose: TNotifyEvent read fOnClose write SetOnClose;
    property OnError: TNotifyEvent read fOnError write SetOnError;
  end;

function GetLdapEntryAttribute(const Entry: TLdapEntryData; const AttributeName: RawUtf8): PLdapAttributeData;
function GetLdapEntryReadable(const Entry: TLdapEntryData; const AttributeName: RawUtf8; Idx: Integer): RawUtf8;
function GetLdapEntryAllReadable(const Entry: TLdapEntryData; const AttributeName: RawUtf8): TRawUtf8DynArray;

function DefaultSearchRequestOptions: TLdapSearchRequestOptions;
procedure SearchRequestOptions(var RequestOptions: TLdapSearchRequestOptions; SizeLimit: Integer; TimeLimitSeconds: Integer; PageSize: Integer; SearchSDFlags: TLdapSearchSDFlags);
procedure SearchRequest(var Request: TLdapSearchRequest; BaseDN: RawUtf8; Filter: RawUtf8; Attributes: TRawUtf8DynArray; Scope: TLdapSearchScope = lssSingleLevel);

function GetLdapErrorCustomMessage(LdapClient: TLdapClient): RawUtf8;

function ConcatACL(ADacls: Array of TSecAcl; AAllowDuplicated: Boolean = False): TSecAcl;

function GetDefaultACLFromObjectClass(ALdapClient: TLdapClient; AObjectClass: TRawUtf8DynArray): TSecAcl;

procedure OrderACL(aLdapClient: TLdapClient; aDN: RawUtf8; aACL: PSecAcl);

function ChangeLdapSettings(LdapClient: TRsatLdapClient; Settings: TLdapClientSettings; AutoConnect: Boolean = True): Boolean;

const
  LDAP_ERROR_CUSTOM_MESSAGE: Array[leOperationsError..leOther] of RawUtf8 = (
    rsOperationsError,
    rsProtocolError,
    rsTimeLimitExceeded,
    rsSizeLimitExceeded,
    '',
    '',
    rsAuthMethodNotSupported,
    rsStrongerAuthRequired,
    rsReferral,
    rsAdminLimitExceeded,
    rsUnavailableCriticalExtension,
    rsConfidentialityRequired,
    rsSaslBindInProgress,
    rsNoSuchAttribute,
    rsUndefinedAttributeType,
    rsInappropriateMatching,
    rsConstraintViolation,
    rsAttributeOrValueExists,
    rsInvalidAttributeSyntax,
    rsNoSuchObject,
    rsAliasProblem,
    rsInvalidDNSyntax,
    '',
    rsAliasDereferencingProblem,
    rsInappropriateAuthentication,
    rsInvalidCredentials,
    rsInsufficientAccessRights,
    rsBusy,
    rsUnavailable,
    rsUnwillingToPerform,
    rsLoopDetect,
    '',
    '',
    rsNamingViolation,
    rsObjectClassViolation,
    rsNotAllowedOnNonLeaf,
    rsNotAllowedOnRDN,
    rsEntryAlreadyExists,
    rsObjectModsProhibited,
    '',
    rsAffectMultipleDSAs,
    '',
    rsOther);

implementation

uses
  Math,
  DateUtils,
  mormot.core.log,
  mormot.core.text,
  mormot.core.rtti;

function GetLdapEntryAttribute(const Entry: TLdapEntryData;
  const AttributeName: RawUtf8): PLdapAttributeData;
var
  i: Integer;
begin
  for i := 0 to Entry.AttributeCount - 1 do
    if EqualBuf(Entry.Attributes[i].Name, AttributeName) then
    begin
      result := @Entry.Attributes[i];
      Exit;
    end;
  result := nil;
end;

function GetLdapEntryReadable(const Entry: TLdapEntryData;
  const AttributeName: RawUtf8; Idx: Integer): RawUtf8;
var
  A: PLdapAttributeData;
begin
  A := GetLdapEntryAttribute(Entry, AttributeName);
  if Assigned(A) and (PtrUInt(Idx) < Length(A^.Values)) then
    result := A^.Values[Idx]
  else
    result := '';
end;

function GetLdapEntryAllReadable(const Entry: TLdapEntryData;
  const AttributeName: RawUtf8): TRawUtf8DynArray;
var
  A: PLdapAttributeData;
begin
  A := GetLdapEntryAttribute(Entry, AttributeName);
  if Assigned(A) then
    result := TRawUtf8DynArray(A^.Values)
  else
    result := nil;
end;

function DefaultSearchRequestOptions: TLdapSearchRequestOptions;
begin
  SearchRequestOptions(result, 2000, 5, 1000, []);
end;

procedure SearchRequestOptions(var RequestOptions: TLdapSearchRequestOptions;
  SizeLimit: Integer; TimeLimitSeconds: Integer; PageSize: Integer;
  SearchSDFlags: TLdapSearchSDFlags);
begin
  RequestOptions.PageSize := PageSize;
  RequestOptions.SearchSDFlags := SearchSDFlags;
  RequestOptions.TimeLimitSeconds := TimeLimitSeconds;
  RequestOptions.SizeLimit := SizeLimit;
end;

procedure SearchRequest(var Request: TLdapSearchRequest; BaseDN: RawUtf8;
  Filter: RawUtf8; Attributes: TRawUtf8DynArray; Scope: TLdapSearchScope);
begin
  Request.BaseDN := BaseDN;
  Request.Attributes := Attributes;
  Request.Filter := Filter;
  Request.Scope := Scope;
end;

function GetLdapErrorCustomMessage(LdapClient: TLdapClient): RawUtf8;
begin
  result := '';

  if (LdapClient.ResultError >= Low(LDAP_ERROR_CUSTOM_MESSAGE)) and (LdapClient.ResultError <= High(LDAP_ERROR_CUSTOM_MESSAGE)) then
    result := LDAP_ERROR_CUSTOM_MESSAGE[LdapClient.ResultError];

  if (result = '') then
    result := LdapClient.ResultString;
end;

function AceIsUseless(Ace: PSecAce): Boolean;
begin
  result := Ace^.Mask = [];
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

function ConcatACL(ADacls: array of TSecAcl; AAllowDuplicated: Boolean): TSecAcl;
var
  i, c, j: Integer;
begin
  result := nil;

  c := 0;
  for i := 0 to High(ADacls) do
  begin
    for j := 0 to High(ADacls[i]) do
      if AAllowDuplicated or not AceInDacl(result, ADacls[i][j]) then
      begin
        Insert(ADacls[i][j], result, c);
        Inc(c);
      end;
  end;
end;

function GetDefaultACLFromObjectClass(ALdapClient: TLdapClient; AObjectClass: TRawUtf8DynArray): TSecAcl;
var
  Filter: RawUtf8;
  i: Integer;
  SD: TSecurityDescriptor;
  LdapResult: TLdapResult;
  DomainSID: RawUtf8;
begin
  result := nil;

  Filter := '';
  for i := 0 to High(AObjectClass) do
    Filter := FormatUtf8('%(lDAPDisplayName=%)', [Filter, LdapEscape(AObjectClass[i])]);
  if Filter = '' then
    Exit;
  Filter := FormatUtf8('(|%)', [Filter]);

  DomainSID := RawSidToText(ALdapClient.DomainSid);
  ALdapClient.SearchBegin();
  try
    ALdapClient.SearchScope := lssWholeSubtree;
    repeat
      if not ALdapClient.Search(ALdapClient.SchemaDN, False, Filter, ['defaultSecurityDescriptor']) then
        Exit;

      for LdapResult in ALdapClient.SearchResult.Items do
      begin
        if not Assigned(LdapResult) then
          Continue;
        SD.Clear;
        SD.FromText(LdapResult.Find('defaultSecurityDescriptor').GetReadable(), DomainSID);

        result := ConcatACL([result, SD.Dacl]);
      end;
    until ALdapClient.SearchCookie = '';
  finally
    ALdapClient.SearchEnd;
  end;
end;

procedure OrderACL(aLdapClient: TLdapClient; aDN: RawUtf8; aACL: PSecAcl);
var
  sdArr: Array of TSecurityDescriptor;
  sd: TSecurityDescriptor;
  parent, filter: RawUtf8;
  res: TLdapResult;
begin
  TOpenRSATLog.Add.Log(sllDebug, FormatUtf8('Start ordering ACL for %...', [aDN]));
  sdArr := [];

  parent := aDN;
  filter := '';
  while not (parent = aLdapClient.DefaultDN()) and not (parent = '') do
  begin
    if (filter = '') then
      filter := '|';
    parent := GetParentDN(parent);
    filter := FormatUtf8('%(distinguishedName=%)', [filter, LdapEscape(parent)]);
  end;

  // Get SDs
  aLdapClient.SearchBegin();
  try
    repeat
      aLdapClient.SearchScope := lssWholeSubtree;
      if not aLdapClient.Search([atNTSecurityDescriptor], filter) then
        Exit;
      for res in aLdapClient.SearchResult.Items do
      begin
        if not sd.FromBinary(res.Attributes.Find(atNTSecurityDescriptor).GetRaw()) then
          continue;
        Insert(sd, sdArr, Length(sdArr));
      end;
    until (aLdapClient.SearchCookie = '');
  finally
    aLdapClient.SearchEnd();
  end;

  // Order acl
  InnerOrderAcl(aACL, sdArr);
  TOpenRSATLog.Add.Log(sllDebug, 'End ordering ACL for.');
end;

function ChangeLdapSettings(LdapClient: TRsatLdapClient;
  Settings: TLdapClientSettings; AutoConnect: Boolean): Boolean;
begin
  result := LdapClient.Close;
  CopyObject(Settings, LdapClient.Settings);
  {$IFDEF DEBUG}
  LdapClient.Log := TLdapLog;
  {$ENDIF DEBUG}
  LdapClient.SearchSDFlags := [lsfOwnerSecurityInformation, lsfGroupSecurityInformation, lsfDaclSecurityInformation];
  if AutoConnect then
    result := LdapClient.Connect();
  Exit;
end;

{ TMormotLdapConnectionContext }

constructor TMormotLdapConnectionContext.Create(const LdapClient: TLdapClient);
begin
  fLdapClient := LdapClient;
end;

function TMormotLdapConnectionContext.NamingContexts: TRawUtf8DynArray;
begin
  result := fLdapClient.NamingContexts;
end;

function TMormotLdapConnectionContext.DefaultNamingContext: RawUtf8;
begin
  result := fLdapClient.DefaultDN();
end;

function TMormotLdapConnectionContext.RootNamingContext: RawUtf8;
begin
  result := fLdapClient.RootDN;
end;

function TMormotLdapConnectionContext.ConfigNamingContext: RawUtf8;
begin
  result := fLdapClient.ConfigDN;
end;

function TMormotLdapConnectionContext.SchemaNamingContext: RawUtf8;
begin
  result := fLdapClient.SchemaDN;
end;

{ TMormotLdapConnection }

procedure TMormotLdapConnection.LdapOperationResult(out
  r: TLdapOperationResult; Success: Boolean; const DistinguishedName: RawUtf8;
  Code: Integer; const Message: RawUtf8; Error: TLdapError; ElapsedTimeMS: Int64
  );
begin
  r.Success := Success;
  r.DistinguishedName := DistinguishedName;
  r.LdapCode := Code;
  r.Message := Message;
  case Error of
    leSuccess: r.ErrorKind := lekNone;
    else
      r.ErrorKind := lekUnknown;
  end;
  r.ElapsedTimeMS := ElapsedTimeMS;
end;

constructor TMormotLdapConnection.Create;
begin
  fLdapClient := TLdapClient.Create;

  fContext := TMormotLdapConnectionContext.Create(fLdapClient);
end;

destructor TMormotLdapConnection.destroy;
begin
  FreeAndNil(fLdapClient);

  inherited destroy;
end;

function TMormotLdapConnection.Connect(const ASettings: TLdapConnectionSettings
  ): TLdapOperationResult;
var
  Start: TDateTime;
  DiscoverMode: TLdapClientConnect;
begin
  With fLdapClient.Settings do
  begin
    TargetHost := ASettings.TargetHost;
    if ASettings.Port = '' then
    begin
      if ASettings.Transport = ldctTls then
        TargetPort := '636'
      else
        TargetPort := '389';
    end
    else
      TargetPort := ASettings.Port;
    Tls := ASettings.Transport = ldctTls;
    KerberosDN := ASettings.KerberosDN;
    fLdapClient.TlsContext^.IgnoreCertificateErrors := ASettings.IgnoreCertificateErrors;
    PingIdleSeconds := ASettings.PingIdleSeconds;
    Timeout := ASettings.TimeoutMS;
    AutoReconnect := ASettings.AutoReconnect;
    KerberosSpn := ASettings.KerberosSPN;
  end;
  Start := Now();

  DiscoverMode := [lccNoDiscovery];
  if ASettings.DiscoverWhenHostEmpty and (ASettings.TargetHost = '') then
    Exclude(DiscoverMode, lccNoDiscovery);
  if ASettings.SelectClosestServer then
    Include(DiscoverMode, lccClosest);
  if ASettings.UseCldapDiscovery then
    Include(DiscoverMode, lccCldap);
  if ASettings.TryTlsFirst then
    Include(DiscoverMode, lccTlsFirst);


  result.Success := fLdapClient.Connect(DiscoverMode, ASettings.DiscoveryDelayMS);
  LdapOperationResult(result, result.Success, '', fLdapClient.ResultCode, fLdapClient.ResultString, fLdapClient.ResultError, MilliSecondsBetween(Start, Now));
end;

function TMormotLdapConnection.Bind(const ACredentials: TLdapCredentials
  ): TLdapOperationResult;
var
  Start: TDateTime;
begin
  Start := Now;
  With fLdapClient.Settings do
  begin
    AllowUnsafePasswordBind := ACredentials.AllowUnsafePasswordBind;
    KerberosSignSeal := ACredentials.KerberosSignSeal;
    KerberosDisableChannelBinding := ACredentials.KerberosDisableChannelBinding;
    UserName := ACredentials.UserName;
    Password := ACredentials.Password;
  end;
  case ACredentials.Authentication of
    ldamAnonymous:
    begin
      fLdapClient.Settings.UserName := '';
      fLdapClient.Settings.Password := '';
      result.Success := fLdapClient.Bind;
    end;
    ldamSimple:
    begin
      result.Success := fLdapClient.Bind;
    end;
    ldamSaslDigest:
    begin
      result.Success := fLdapClient.BindSaslDigest();
    end;
    ldamKerberos:
    begin
      result.Success := fLdapClient.BindSaslKerberos(ACredentials.KerberosAuthIdentity);
    end;
  end;
  LdapOperationResult(result, result.Success, '', fLdapClient.ResultCode, fLdapClient.ResultString, fLdapClient.ResultError, MilliSecondsBetween(Start, Now));
end;

procedure TMormotLdapConnection.Disconnect;
begin
  fLdapClient.Close;
end;

function TMormotLdapConnection.IsConnected: Boolean;
begin
  result := fLdapClient.Connected;
end;

function TMormotLdapConnection.Search(const ARequest: TLdapSearchRequest
  ): TLdapSearchResult;
var
  SearchResult: TLdapResult;
  Attribute: TLdapAttribute;
  i: Integer;
  PEntry: PLdapEntryData;
  PAttribute: PLdapAttributeData;
  Start: TDateTime;
begin
  Start := Now;
  result := Default(TLdapSearchResult);
  fLdapClient.SearchBegin(ARequest.Options.PageSize);
  try
    fLdapClient.SearchScope := ARequest.Scope;
    fLdapClient.SearchSDFlags := ARequest.Options.SearchSDFlags;
    fLdapClient.SearchTimeLimit := ARequest.Options.TimeLimitSeconds;
    repeat
      fLdapClient.SearchPageSize := Min(ARequest.Options.PageSize, ARequest.Options.SizeLimit - Result.returnedCount);
      fLdapClient.SearchRangeBegin;
      try
        result.OperationResult.Success := fLdapClient.Search(ARequest.BaseDN, False, ARequest.Filter, ARequest.Attributes);
        if not result.OperationResult.Success then
          Exit;
      finally
        fLdapClient.SearchRangeEnd;
      end;
      SetLength(result.Entries, result.ReturnedCount + fLdapClient.SearchResult.Count);
      for SearchResult in fLdapClient.SearchResult.Items do
      begin
        if not Assigned(SearchResult) then
          continue;
        PEntry := @result.Entries[result.ReturnedCount];
        PEntry^.DistinguishedName := SearchResult.ObjectName;
        SetLength(PEntry^.Attributes, SearchResult.Attributes.Count);
        for Attribute in SearchResult.Attributes.Items do
        begin
          if not Assigned(Attribute) then
            Continue;
          PAttribute := @PEntry^.Attributes[PEntry^.AttributeCount];
          PAttribute^.Name := Attribute.AttributeName;
          SetLength(PAttribute^.Values, Attribute.Count);
          for i := 0 to Attribute.Count - 1 do
            PAttribute^.Values[i] := Attribute.GetRaw(i);
          Inc(PEntry^.AttributeCount);
        end;
        Inc(result.ReturnedCount);
      end;
    until (fLdapClient.SearchCookie = '') or (result.ReturnedCount >= ARequest.Options.SizeLimit);
  finally
    fLdapClient.SearchEnd;
    LdapOperationResult(result.OperationResult, result.OperationResult.Success,
    '', fLdapClient.ResultCode, fLdapClient.ResultString, fLdapClient.ResultError,
    MilliSecondsBetween(Start, Now));
  end;
end;

function TMormotLdapConnection.Add(const ARequest: TLdapAddRequest
  ): TLdapOperationResult;
var
  Attributes: TLdapAttributeList;
  Attribute: TLdapAttributeData;
  A: TLdapAttribute;
  v: RawByteString;
  Start: TDateTime;
begin
  Attributes := TLdapAttributeList.Create;
  try
    for Attribute in ARequest.Attributes do
    begin
      A := Attributes.Add(Attribute.Name);
      for v in Attribute.Values do
        A.Add(v);
    end;
    Start := Now;
    Result.Success := fLdapClient.Add(ARequest.DistinguishedName, Attributes);
  finally
    FreeAndNil(Attributes);
    LdapOperationResult(result,
      result.Success,
      ARequest.DistinguishedName,
      fLdapClient.ResultCode,
      fLdapClient.ResultString,
      fLdapClient.ResultError,
      MilliSecondsBetween(Start, Now));
  end;
end;

function TMormotLdapConnection.Modify(const ARequest: TLdapModifyRequest
  ): TLdapOperationResult;
var
  Modifications: array of TAsnObject;
  i: Integer;
  Attribute: TLdapAttribute;
  Start: TDateTime;
  v: RawByteString;
begin
  Start := Now();
  SetLength(Modifications, Length(ARequest.Changes));
  for i := 0 to High(ARequest.Changes) do
  begin
    Attribute := TLdapAttribute.Create(ARequest.Changes[i].Attribute.Name, atUndefined);
    try
      for v in ARequest.Changes[i].Attribute.Values do
        Attribute.Add(v);
      Modifications[i] := Modifier(ARequest.Changes[i].Operation, Attribute.ExportToAsnSeq);
    finally
      FreeAndNil(Attribute);
    end;
  end;
  result.Success := fLdapClient.Modify(ARequest.DistinguishedName, Modifications);

  LdapOperationResult(result, result.Success, ARequest.DistinguishedName, fLdapClient.ResultCode,
    fLdapClient.ResultString, fLdapClient.ResultError, MilliSecondsBetween(Start, Now));
end;

function TMormotLdapConnection.Delete(const ARequest: TLdapDeleteRequest
  ): TLdapOperationResult;
var
  Start: TDateTime;
begin
  Start := Now();

  result.Success := fLdapClient.Delete(ARequest.DistinguishedName, ARequest.DeleteChildren);

  LdapOperationResult(result, result.Success, ARequest.DistinguishedName, fLdapClient.ResultCode,
    fLdapClient.ResultString, fLdapClient.ResultError, MilliSecondsBetween(Start, Now));
end;

function TMormotLdapConnection.ModifyDN(const ARequest: TLdapModifyDNRequest
  ): TLdapOperationResult;
var
  Start: TDateTime;
begin
  Start := Now();

  result.Success := fLdapClient.ModifyDN(ARequest.DistinguishedName, ARequest.NewRDN, ARequest.NewSuperior, ARequest.DeleteOldRDN);

  LdapOperationResult(result, result.Success, ARequest.DistinguishedName, fLdapClient.ResultCode,
    fLdapClient.ResultString, fLdapClient.ResultError, MilliSecondsBetween(Start, Now));
end;

function TMormotLdapConnection.Context: ILdapConnectionContext;
begin
  result := fContext;
end;

{ TMormotLdapAttribute }

//function TMormotLdapAttribute.GetName: RawUtf8;
//begin
//  result := fName;
//end;
//
//constructor TMormotLdapAttribute.Create(const AttributeName: RawUtf8;
//  const AttributeValues: TRawByteStringDynArray);
//begin
//  fName := AttributeName;
//  fOriginalValues := AttributeValues;
//  fCurrentValues := fOriginalValues;
//end;
//
//procedure TMormotLdapAttribute.Add(const Value: RawByteString);
//var
//  C: SizeInt;
//begin
//  C := Length(fCurrentValues);
//  SetLength(fCurrentValues, C + 1);
//  fCurrentValues[C] := Value;
//end;
//
//procedure TMormotLdapAttribute.Replace(const Value: RawByteString);
//begin
//  Delete;
//  Add(Value);
//end;
//
//procedure TMormotLdapAttribute.Delete;
//begin
//  fCurrentValues := nil;
//end;

{ TMormotLdapObject }

//constructor TMormotLdapObject.Create(const Ldap: ILdapConnection;
//  const DistinguishedName: RawUtf8);
//begin
//  fLdap := Ldap;
//  fIdentity := DistinguishedName;
//end;
//
//procedure TMormotLdapObject.LoadAttributes(const Attributes: TRawUtf8DynArray);
//var
//  Request: TLdapSearchRequest;
//  Res: TLdapSearchResult;
//begin
//  SearchRequestOptions(Request.Options, 1, 5, 1, [lsfOwnerSecurityInformation, lsfGroupSecurityInformation, lsfDaclSecurityInformation]);
//  SearchRequest(Request, fIdentity, '', Attributes, lssBaseObject);
//
//  Res := fLdap.Search(Request);
//  if not Res.OperationResult.Success then
//    Exit;
//
//  // Manage Attributes
//end;
//
//procedure TMormotLdapObject.Refresh;
//begin
//  // LoadAttributes with already loaded attributes
//end;
//
//procedure TMormotLdapObject.Commit;
//var
//  DR: TLdapDeleteRequest;
//  Res: TLdapOperationResult;
//begin
//  // Retrieve changes, and perform operations
//  if IsDeleted then
//  begin
//    DR.DistinguishedName := fIdentity;
//    DR.DeleteChildren := True;
//    Res := fLdap.Delete(DR);
//    Exit;
//  end;
//  if IsNew then
//  begin
//    DR.DistinguishedName:=;
//  end;
//end;

{ TRsatLdapClient }

function TRsatLdapClient.AddProtection(PSecDesc: PSecurityDescriptor;
  Sid: RawSid): Boolean;
begin
  result := False;

  if not Assigned(PSecDesc) then
    Exit;

  if not Assigned(SecDescAddOrUpdateACE(PSecDesc, ATTR_UUID[kaNull], Sid, satAccessDenied, [samDelete, samDeleteTree])) then
  begin
    if Assigned(fLog) then
      fLog.add.Log(sllWarning, 'Cannot add ACE', Self);
    Exit;
  end;
  result := True;
end;

function TRsatLdapClient.DelProtection(PSecDesc: PSecurityDescriptor;
  Sid: RawSid): Boolean;
var
  i: Integer;
begin
  result := False;

  if not Assigned(PSecDesc) then
    Exit;

  i := SecDescFindACE(PSecDesc, satAccessDenied, Sid, [samDelete, samDeleteTree], @ATTR_UUID[kaNull]);
  if i < 0 then
  begin
    if Assigned(fLog) then
      fLog.add.Log(sllWarning, 'Cannot find ACE', Self);
    //Dialogs.MessageDlg(rsTitleNotFound, 'Cannot find ACE', mtError, [mbOK], 0);
    Exit;
  end;
  PSecDesc^.Dacl[i].Mask -= [samDelete, samDeleteTree];
  result := True;
end;

procedure TRsatLdapClient.SearchPagingBegin(PageNumber: Integer);
begin
  fPageNumber := PageNumber;
end;

function TRsatLdapClient.SearchAllDocPaged(DocResult: PDocVariantData; const BaseDN: RawUtf8;
  TypesOnly: boolean; const Filter: RawUtf8; const Attributes: array of RawUtf8
  ): Boolean;
var
  PageCount: Integer;
  item: TLdapResult;
  Attribute: TLdapAttribute;
begin
  PageCount := 0;
  repeat
    result := Search(BaseDN, TypesOnly, Filter, Attributes);

    for item in fSearchResult.Items do
    begin
      if not Assigned(item) then
        continue;
      for Attribute in item.Attributes.Items do
      begin
        if not Assigned(Attribute) then
          continue;
        DocResult^.O_[item.ObjectName]^.AddOrUpdateValue(Attribute.AttributeName, Attribute.GetVariant());
      end;
    end;
    Inc(PageCount);
  until (SearchCookie = '') or (fPageNumber = PageCount) or not result;
end;

procedure TRsatLdapClient.SearchPagingEnd;
begin
  fPageNumber := 0;
end;

function TRsatLdapClient.MoveLdapEntry(oldDN, newDN: string): Boolean;
var
  DNs: TNameValueDNs;
  newRdn, newParentDN: String;
  i: Integer;
  aLog: ISynLog;
begin
  result := False;
  aLog := TOpenRSATLog.Enter('Move Ldap Entry', []);

  assert(oldDN <> '', 'OldDN is empty.');
  assert(newDN <> '', 'NewDN is empty.');
  assert(Assigned(Self), 'Ldap instance is null.');

  if (oldDN = '') or (newDN = '') then
  begin
    if Assigned(aLog) then
      aLog.Log(sllDebug, 'oldDN or newDN is empty');
    Exit;
  end;
  ParseDN(newDN, DNs);
  newRdn := DNs[0].Name + '=' + DNs[0].Value;
  newParentDN := DNs[1].Name + '=' + DNs[1].Value;
  for i := 2 to High(DNs) do
    newParentDN += ',' + DNs[i].Name + '=' + DNs[i].Value;
  if Assigned(aLog) then
    aLog.Log(sllDebug, FormatUtf8('Moving Ldap entry "%" as "%" to "%".', [oldDN, newRDN, newParentDN]));

  result := ModifyDN(oldDN, newRdn, newParentDN, True);
end;

function TRsatLdapClient.RenameLdapEntry(DN, newName: string): Boolean;
var
  aLog: ISynLog;
begin
  result := False;
  aLog := TOpenRSATLog.Enter('Rename Ldap Entry', []);

  assert(DN <> '', 'DN is empty');
  assert(newName <> '', 'newName is empty');
  assert(Assigned(Self), 'Ldap instance is null.');

  if (DN = '') or (newName = '') then
  begin
    if Assigned(aLog) then
      aLog.Log(sllDebug, 'DN or NewName is empty');
    Exit;
  end;
  if Assigned(aLog) then
    aLog.Log(sllDebug, FormatUtf8('Renaming Ldap entry "%" as "%".', [DN, newName]));

  result := ModifyDN(DN, newName, '', True);
end;

procedure TRsatLdapClient.ChangeSettings(ASettings: TLdapClientSettings;
  AutoConnect: Boolean);
begin
  if Assigned(fSettings) then
    FreeAndNil(fSettings);
  Close;
  fSettings := TLdapClientSettings.Create;

  CopyObject(ASettings, fSettings);
  TlsContext^.IgnoreCertificateErrors := Settings.AllowUnsafePasswordBind;
  if AutoConnect then
    Connect;
end;

function TRsatLdapClient.CreateOrganizationalUnit(OUName, ParentDN: RawUtf8;
  Protected: Boolean): RawUtf8;
var
  Attrs: TLdapAttributeList;
  Attr: TLdapAttribute;
begin
  result := FormatUtf8('ou=%,%', [OUName, ParentDN]);
  Attrs := TLdapAttributeList.Create;
  try
    Attr := Attrs.Add('objectClass', 'top');
    Attr.Add('organizationalUnit');
    if not Add(result, Attrs) then
    begin
      result := '';
      Exit;
    end;

  finally
    Attrs.Free;
  end;
end;

function TRsatLdapClient.CreateGroup(GroupName, ParentDN: RawUtf8;
  JoinGroups: TRawUtf8DynArray; AddMembers: TRawUtf8DynArray): RawUtf8;
var
  Attrs: TLdapAttributeList;
  Attr: TLdapAttribute;
  member, group: RawUtf8;
begin
  result := FormatUtf8('cn=%,%', [GroupName, ParentDN]);
  Attrs := TLdapAttributeList.Create;
  try
    Attr := Attrs.Add('objectClass', 'top');
    Attr.Add('group');
    if Length(AddMembers) > 0 then
    begin
      Attr := Attrs.Add('member');
      for member in AddMembers do
        Attr.Add(member, aoNoDuplicateValue);
    end;
    attrs.Add('sAMAccountName', GroupName);
    if not Add(result, Attrs) then
    begin
      result := '';
      Exit;
    end;
  finally
    Attrs.Free;
  end;
  for group in JoinGroups do
    Modify(group, lmoAdd, 'member', result);
end;

function TRsatLdapClient.CreateUser(UserName, ParentDN: RawUtf8;
  JoinGroups: TRawUtf8DynArray): RawUtf8;
var
  Attrs: TLdapAttributeList;
  Attr: TLdapAttribute;
  group: RawUtf8;
begin
  result := FormatUtf8('cn=%,%', [UserName, ParentDN]);
  Attrs := TLdapAttributeList.Create;
  try
    Attr := Attrs.Add('objectClass', 'top');
    Attr.Add('person');
    Attr.Add('organizationalPerson');
    Attr.Add('user');
    if not Add(result, Attrs) then
    begin
      result := '';
      Exit;
    end;
  finally
    Attrs.Free;
  end;
  for group in JoinGroups do
    Modify(group, lmoAdd, 'member', result);
end;

function TRsatLdapClient.SetOUProtection(DistinguishedName: RawUtf8;
  Protected: Boolean): Boolean;
var
  Attribute: TLdapAttribute;
  SecDesc: TSecurityDescriptor;
  Sid: RawSid;
begin
  result := False;

  Attribute := SearchObject(DistinguishedName, '(objectClass=organizationalUnit)', 'nTSecurityDescriptor');
  if not Assigned(Attribute) then
    Exit;
  if not SecDesc.FromBinary(Attribute.GetRaw()) then
    Exit;
  Sid := KnownRawSid(wksWorld);

  if Protected then
    result := AddProtection(@SecDesc, Sid)
  else
    result := DelProtection(@SecDesc, Sid);

  if not result then
    Exit;

  OrderAcl(Attribute.AttributeName, DefaultDN, @SecDesc);

  if not Modify(Attribute.AttributeName, lmoReplace, 'nTSecurityDescriptor', SecDesc.ToBinary) then
    Exit;
end;

// https://learn.microsoft.com/en-us/windows/win32/secauthz/order-of-aces-in-a-dacl
procedure TRsatLdapClient.OrderAcl(DN, BaseDN: RawUtf8; Acl: PSecAcl);
var
  sdArr: Array of TSecurityDescriptor;
  sd: TSecurityDescriptor;
  parent, filter: RawUtf8;
  res: TLdapResult;
begin
  TOpenRSATLog.Add.Log(sllDebug, FormatUtf8('Start ordering ACL for %...', [DN]));
  sdArr := [];

  parent := DN;
  filter := '';
  while not (parent = Self.DefaultDN(baseDN)) and not (parent = '') do
  begin
    if (filter = '') then
      filter := '|';
    parent := GetParentDN(parent);
    filter := FormatUtf8('%(distinguishedName=%)', [filter, LdapEscape(parent)]);
  end;

  // Get SDs
  Self.SearchBegin();
  try
    repeat
      Self.SearchScope := lssWholeSubtree;
      if not Self.Search([atNTSecurityDescriptor], filter) then
        Exit;
      for res in Self.SearchResult.Items do
      begin
        if not sd.FromBinary(res.Attributes.Find(atNTSecurityDescriptor).GetRaw()) then
          continue;
        Insert(sd, sdArr, Length(sdArr));
      end;
    until (Self.SearchCookie = '');
  finally
    Self.SearchEnd();
  end;

  // Order acl
  InnerOrderAcl(acl, sdArr);
  TOpenRSATLog.Add.Log(sllDebug, 'End ordering ACL for.');
end;

function TRsatLdapClient.Search(const Attributes: TLdapAttributeTypes;
  const Filter: RawUtf8; const BaseDN: RawUtf8; TypesOnly: boolean): boolean;
begin
  Result := inherited Search(Attributes, Filter, BaseDN, TypesOnly);

  if not result and Assigned(fOnError) then
    fOnError(Self);
end;

function TRsatLdapClient.Search(const BaseDN: RawUtf8; TypesOnly: boolean;
  const Filter: RawUtf8; const Attributes: array of RawUtf8): boolean;
begin
  Result := inherited Search(BaseDN, TypesOnly, Filter, Attributes);

  if not result and Assigned(fOnError) then
    fOnError(Self);
end;

function TRsatLdapClient.SearchObject(const ObjectDN, Filter,
  Attribute: RawUtf8; Scope: TLdapSearchScope): TLdapAttribute;
begin
  result := inherited SearchObject(ObjectDN, Filter, Attribute, Scope);

  if not Assigned(result) and Assigned(fOnError) then
    fOnError(Self);
end;

function TRsatLdapClient.SearchObject(const ObjectDN, Filter: RawUtf8;
  const Attributes: array of RawUtf8; Scope: TLdapSearchScope): TLdapResult;
begin
  result := inherited SearchObject(ObjectDN, Filter, Attributes, Scope);

  if not Assigned(result) and Assigned(fOnError) then
    fOnError(Self);
end;

function TRsatLdapClient.SearchObject(Attribute: TLdapAttributeType;
  const ObjectDN, Filter: RawUtf8; Scope: TLdapSearchScope): TLdapAttribute;
begin
  result := inherited SearchObject(Attribute, ObjectDN, Filter, Scope);

  if not Assigned(result) and Assigned(fOnError) then
    fOnError(Self);
end;

function TRsatLdapClient.SearchObject(const Attributes: TLdapAttributeTypes;
  const ObjectDN, Filter: RawUtf8; Scope: TLdapSearchScope): TLdapResult;
begin
  result := inherited SearchObject(Attributes, ObjectDN, Filter, Scope);

  if not Assigned(result) and Assigned(fOnError) then
    fOnError(Self);
end;

function TRsatLdapClient.Modify(const Obj: RawUtf8;
  const Modifications: array of TAsnObject): boolean;
begin
  result := inherited Modify(Obj, Modifications);

  if not result and Assigned(fOnError) then
    fOnError(Self);
end;

function TRsatLdapClient.Modify(const Obj: RawUtf8; Op: TLdapModifyOp;
  const Types: array of TLdapAttributeType;
  const Values: array of const): boolean;
begin
  result := inherited Modify(Obj, Op, Types, Values);

  if not result and Assigned(fOnError) then
    fOnError(Self);
end;

function TRsatLdapClient.Modify(const Obj: RawUtf8; Op: TLdapModifyOp;
  const AttrName: RawUtf8; const AttrValue: RawByteString): boolean;
begin
  result := inherited Modify(Obj, Op, AttrName, AttrValue);

  if not result and Assigned(fOnError) then
    fOnError(Self);
end;

function TRsatLdapClient.Modify(const Obj: RawUtf8; Op: TLdapModifyOp;
  Attribute: TLdapAttribute): boolean;
begin
  result := inherited Modify(Obj, Op, Attribute);

  if not result and Assigned(fOnError) then
    fOnError(Self);
end;

function TRsatLdapClient.Modify(const Obj: RawUtf8; Op: TLdapModifyOp;
  AttrType: TLdapAttributeType; const AttrValue: RawByteString): boolean;
begin
  result := inherited Modify(Obj, Op, AttrType, AttrValue);

  if not result and Assigned(fOnError) then
    fOnError(Self);
end;

function TRsatLdapClient.ModifyDN(const Obj, NewRdn, NewSuperior: RawUtf8;
  DeleteOldRdn: boolean): boolean;
begin
  result := inherited ModifyDN(Obj, NewRdn, NewSuperior, DeleteOldRdn);

  if not result and Assigned(fOnError) then
    fOnError(Self);
end;

function TRsatLdapClient.ModifyUserPassword(const UserDN: RawUtf8;
  const OldPassword, NewPassword: SpiUtf8): boolean;
begin
  result := inherited ModifyUserPassword(UserDN, OldPassword, NewPassword);

  if not result and Assigned(fOnError) then
    fOnError(Self);
end;

function TRsatLdapClient.Add(const Obj: RawUtf8; Value: TLdapAttributeList
  ): boolean;
begin
  result := inherited Add(Obj, Value);

  if not result and Assigned(fOnError) then
    fOnError(Self);
end;

function TRsatLdapClient.Delete(const Obj: RawUtf8; DeleteChildren: boolean
  ): boolean;
begin
  result := inherited Delete(Obj, DeleteChildren);

  if not result and Assigned(fOnError) then
    fOnError(Self);
end;

function TRsatLdapClient.Connect(DiscoverMode: TLdapClientConnect;
  DelayMS: integer): boolean;
begin
  Result := inherited Connect(DiscoverMode, DelayMS);

  if Result and Connected then
  begin
    if Assigned(fOnConnect) then
      fOnConnect(Self);
  end
  else
  begin
    if Assigned(fOnError) then
      fOnError(Self);
    Close;
  end;
end;

function TRsatLdapClient.Close: boolean;
begin
  Result := inherited Close;

  if result then
  begin
    if Assigned(fOnClose) then
      fOnClose(Self);
  end
  else
    if Assigned(fOnError) then
      fOnError(Self);
end;

procedure TRsatLdapClient.SetOnClose(AValue: TNotifyEvent);
begin
  if fOnClose=AValue then Exit;
  fOnClose:=AValue;
end;

procedure TRsatLdapClient.SetOnConnect(AValue: TNotifyEvent);
begin
  if fOnConnect=AValue then Exit;
  fOnConnect:=AValue;
end;

procedure TRsatLdapClient.SetOnError(AValue: TNotifyEvent);
begin
  if fOnError=AValue then Exit;
  fOnError:=AValue;
end;

end.

