unit uproperty;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.base,
  mormot.core.os.security,
  mormot.core.variants,
  {$IFDEF OPENRSATTESTS}
  mormot.core.test,
  {$ENDIF OPENRSATTESTS}
  mormot.net.ldap,
  ucommon,
  ursatldapclient,
  ursat;

type

  {$IFDEF OPENRSATTESTS}

  { TPropertyTests }

  TPropertyTests = class(TSynTestCase)
  private
    function GetFilledAttributes: TLdapAttributeList;
  published
    procedure TestCreate;
    procedure TestAttributes;
    procedure TestCanonicalName;
    procedure TestDistinguishedName;
    procedure TestAdd;
    procedure TestRestore;
    procedure TestGet;
    procedure TestGetRaw;
    procedure TestGetReadable;
    procedure TestGetAllReadable;
    procedure TestDCTypeFromUAC;
    procedure TestDirectReportsNames;
    procedure TestManagerName;
    procedure TestSubnetsFromSiteObject;
    procedure TestSiteFromServerReference;
  end;
  {$ENDIF OPENRSATTESTS}


  TFVERecoveryInformation = record
    cn: RawUtf8;
    msFVERecoveryPassword: RawUtf8;
  end;

  PFVERecoveryInformation = ^TFVERecoveryInformation;

  TFVERecoveryInformationDynArray = Array of TFVERecoveryInformation;

  { TProperty }

  TProperty = class
  private
    fModifiedAttributes, fAttributes: TLdapAttributeList;
    fTempModify: TDocVariantData;

    fRSAT: TRSAT;

    function GetDCTypeFromUAC: RawUtf8;
    function GetDirectReportsNames: TRawUtf8DynArray;
    function GetManagerName: RawUtf8;
    function GetSiteFromServerReference: RawUtf8;
    function GetSubnetsFromSiteObject: TRawUtf8DynArray;
    procedure UpdateMemberOf(MemberOfList: TRawUtf8DynArray; AddModify: Boolean = True);

    function ApplyAttributeDifference: Boolean;
    function ApplyAttributeDifference(AttributeName: RawUtf8): Boolean;
    function ApplyTempModification: Boolean;
  public
    constructor Create(ARSAT: TRSAT = nil);
    destructor Destroy; override;

    function IsModified: Boolean;
    function IsModified(AttributeName: RawUtf8): Boolean;
    function ApplyModification: Boolean;

    function GetAllReadable(Name: RawUtf8): TRawUtf8DynArray;
    function GetReadable(Name: RawUtf8; index: Integer = 0): RawUtf8;
    function GetRaw(Name: RawUtf8; index: Integer = 0): RawByteString;
    function Get(Name: RawUtf8): TLdapAttribute;

    procedure Add(Name: RawUtf8; Value: RawUtf8; Option: TLdapAddOption = aoReplaceValue);
    procedure Restore(Name: RawUtf8);
    procedure SearchObject(Attributes: TRawUtf8DynArray);

    procedure AddMemberOf(MemberOfList: TRawUtf8DynArray);
    procedure DeleteMemberOf(MemberOfList: TRawUtf8DynArray);

    procedure Subnets(Data: PDocVariantData);
    function UPNSuffixes: TRawUtf8DynArray;

    property SiteFromServerReference: RawUtf8 read GetSiteFromServerReference;
    property SubnetsFromSiteObject: TRawUtf8DynArray read GetSubnetsFromSiteObject;
    property DCTypeFromUAC: RawUtf8 read GetDCTypeFromUAC;
    property ManagerName: RawUtf8 read GetManagerName;
    property DirectReportsNames: TRawUtf8DynArray read GetDirectReportsNames;
    function CannotChangePassword: Boolean;
    function msFVERecoveryInformation: TFVERecoveryInformationDynArray;
    function AttributesFromSchema: TRawUtf8DynArray;

    procedure UserAccountControlExclude(UserAccountControl: TUserAccountControl);
    procedure UserAccountControlInclude(UserAccountControl: TUserAccountControl);
    procedure msDSSupportedEncryptionTypeInclude(msDSSupportedEncrytionType: TMsdsSupportedEncryptionType);
    procedure msDSSupportedEncryptionTypeExclude(msDSSupportedEncrytionType: TMsdsSupportedEncryptionType);
  private
    fSecurityDescriptor: TSecurityDescriptor;

    function GetcanonicalName: RawUtf8;
    function GetCN: RawUtf8;
    function Getdescription: RawUtf8;
    function GetdistinguishedName: RawUtf8;
    function GetLdapClient: TRsatLdapClient;
    function GetmanagedBy: RawUtf8;
    function Getname: RawUtf8;
    function GetobjectClass: TRawUtf8DynArray;
    function GetobjectGuid: RawUtf8;
    function GetobjectSid: RawUtf8;
    function GetobjectType: RawUtf8;
    function GetsAMAccountName: RawUtf8;
    function GetSecurityDescriptor: PSecurityDescriptor;
    function GetwhenChanged: TDateTime;
    function GetwhenCreated: TDateTime;
    procedure SetAttributes(AValue: TLdapAttributeList);
    procedure SetCN(AValue: RawUtf8);
    procedure Setdescription(AValue: RawUtf8);
    procedure SetdistinguishedName(AValue: RawUtf8);
    procedure SetmanagedBy(AValue: RawUtf8);
    procedure Setname(AValue: RawUtf8);
    procedure SetobjectClass(AValue: TRawUtf8DynArray);
    procedure SetobjectGuid(AValue: RawUtf8);
    procedure SetobjectSid(AValue: RawUtf8);
    procedure SetsAMAccountName(AValue: RawUtf8);
    procedure SetSecurityDescriptor(AValue: PSecurityDescriptor);
    procedure SetwhenChanged(AValue: TDateTime);
    procedure SetwhenCreated(AValue: TDateTime);
  public
    property RSAT: TRSAT read fRSAT write fRSAT;
    property LdapClient: TRsatLdapClient read GetLdapClient;

    property Attributes: TLdapAttributeList read fAttributes write SetAttributes;
    property sAMAccountName: RawUtf8 read GetsAMAccountName write SetsAMAccountName;
    property distinguishedName: RawUtf8 read GetdistinguishedName write SetdistinguishedName;
    property canonicalName: RawUtf8 read GetcanonicalName;
    property name: RawUtf8 read Getname write Setname;
    property CN: RawUtf8 read GetCN write SetCN;
    property description: RawUtf8 read Getdescription write Setdescription;
    property managedBy: RawUtf8 read GetmanagedBy write SetmanagedBy;
    property objectClass: TRawUtf8DynArray read GetobjectClass write SetobjectClass;
    property objectType: RawUtf8 read GetobjectType;
    property objectSid: RawUtf8 read GetobjectSid write SetobjectSid;
    property objectGuid: RawUtf8 read GetobjectGuid write SetobjectGuid;
    property SecurityDescriptor: PSecurityDescriptor read GetSecurityDescriptor write SetSecurityDescriptor;
    property whenCreated: TDateTime read GetwhenCreated write SetwhenCreated;
    property whenChanged: TDateTime read GetwhenChanged write SetwhenChanged;
  end;

implementation

uses
  mormot.core.datetime,
  mormot.core.text;

{ TPropertyTests }

function TPropertyTests.GetFilledAttributes: TLdapAttributeList;
var
  a: TLdapAttribute;
begin
  result := TLdapAttributeList.Create;

  result.Add('cn', 'cn');
  result.Add('description', 'description');
  result.Add('distinguishedName', 'CN=distinguishedName,DC=test,DC=lan');
  result.Add('managedBy', 'CN=managedBy,DC=test,DC=lan');
  result.Add('name', 'name');
  result.Add('objectClass', 'objectClass').Add('objectClassBis');
  result.Add('objectGuid', 'objectGUID');
  result.Add('objectSid', 'objectSID');
  result.Add('objectType', 'objectType');
  result.Add('sAMAccountName', 'sAMAccountName');
end;

procedure TPropertyTests.TestCreate;
var
  P: TProperty;
  RSAT, R: TRSAT;
begin

  P := TProperty.Create();
  try
    Check(Assigned(P));
    Check(not Assigned(P.fAttributes));
    Check(not Assigned(P.fModifiedAttributes));
    Check(not Assigned(P.fRSAT));
    Check(P.fTempModify.Count = 0);
  finally
    FreeAndNil(P);
  end;

  R := TRSAT.Create(nil);
  try
    P := TProperty.Create(R);
    try
      Check(Assigned(P));
      Check(not Assigned(P.fAttributes));
      Check(not Assigned(P.fModifiedAttributes));
      Check(Assigned(P.fRSAT));
      Check(P.fRSAT = R);
      Check(P.fTempModify.Count = 0);
    finally
      FreeAndNil(P);
    end;
  finally
    FreeAndNil(R);
  end;
end;

procedure TPropertyTests.TestAttributes;
var
  P: TProperty;
  A, B: TLdapAttributeList;
begin
  P := TProperty.Create();
  A := TLdapAttributeList.Create;
  B := GetFilledAttributes;
  try
    Check(not Assigned(P.Attributes));
    P.Attributes := A;
    Check(Assigned(P.Attributes));
    Check(P.Attributes.Count = A.Count);
    P.Attributes := nil;
    Check(not Assigned(P.Attributes));
    P.Attributes := B;
    Check(P.Attributes.Count = B.Count);
  finally
    FreeAndNil(B);
    FreeAndNil(A);
    FreeAndNil(P);
  end;
end;

procedure TPropertyTests.TestCanonicalName;
var
  P: TProperty;
begin
  P := TProperty.Create();
  try
    Check(P.canonicalName = '');
    P.distinguishedName := 'CN=distinguishedName,DC=test,DC=lan';
    Check(P.canonicalName = 'test.lan/distinguishedName');
  finally
    FreeAndNil(P);
  end;
end;

procedure TPropertyTests.TestDistinguishedName;
var
  P: TProperty;
  A: TLdapAttributeList;
begin
  P := TProperty.Create();
  A := GetFilledAttributes;
  try
    Check(P.distinguishedName = '');
    Check(P.Attributes.Find('distinguishedName').GetReadable() = '');
    Check(P.fModifiedAttributes.Find('distinguishedName').GetReadable() = '');

    P.distinguishedName := 'CN=distinguishedName,DC=test,DC=lan';
    Check(P.distinguishedName = 'CN=distinguishedName,DC=test,DC=lan');
    Check(P.Attributes.Find('distinguishedName').GetReadable() = '');
    Check(P.fModifiedAttributes.Find('distinguishedName').GetReadable() = 'CN=distinguishedName,DC=test,DC=lan');

    P.Attributes := A;
    Check(P.distinguishedName = 'CN=distinguishedName,DC=test,DC=lan');
    Check(P.Attributes.Find('distinguishedName').GetReadable() = 'CN=distinguishedName,DC=test,DC=lan');
    Check(P.fModifiedAttributes.Find('distinguishedName').GetReadable() = '');

    P.distinguishedName := 'CN=distinguishedNameModified,DC=test,DC=lan';
    Check(P.distinguishedName = 'CN=distinguishedNameModified,DC=test,DC=lan');
    Check(P.Attributes.Find('distinguishedName').GetReadable() = 'CN=distinguishedName,DC=test,DC=lan');
    Check(P.fModifiedAttributes.Find('distinguishedName').GetReadable() = 'CN=distinguishedNameModified,DC=test,DC=lan');
  finally
    FreeAndNil(A);
    FreeAndNil(P);
  end;
end;

procedure TPropertyTests.TestAdd;
const
  Key: RawUtf8 = 'distinguishedName';
var
  P: TProperty;
begin
  P := TProperty.Create();
  try
    P.Add(key, 'value');
    Check(not Assigned(P.Attributes));
    Check(P.fModifiedAttributes.Find(Key).Count = 1);

    P.Add(key, 'valuebis');
    Check(not Assigned(P.Attributes));
    Check(P.fModifiedAttributes.Find(Key).Count = 1);

    P.Add(key, 'valuebisbis', aoAlways);
    Check(not Assigned(P.Attributes));
    Check(P.fModifiedAttributes.Find(Key).Count = 2);

    P.Add(key, 'newvalue', aoKeepExisting);
    Check(not Assigned(P.Attributes));
    Check(P.fModifiedAttributes.Find(Key).Count = 2);

    P.Add(key, 'newvalue', aoNoDuplicateValue);
    Check(not Assigned(P.Attributes));
    Check(P.fModifiedAttributes.Find(Key).Count = 3);

    P.Add(key, 'newvalue', aoNoDuplicateValue);
    Check(not Assigned(P.Attributes));
    Check(P.fModifiedAttributes.Find(Key).Count = 3);

    P.Add(key, 'newvalue', aoReplaceValue);
    Check(not Assigned(P.Attributes));
    Check(P.fModifiedAttributes.Find(Key).Count = 1);
  finally
    FreeAndNil(P);
  end;
end;

procedure TPropertyTests.TestRestore;
const
  Key: RawUtf8 = 'objectClass';
var
  P: TProperty;
begin
  P := TProperty.Create;
  try
    P.Add(Key, 'top');
    P.Add(Key, 'user', aoAlways);
    Check(Assigned(P.Get(Key)));
    P.Restore(Key);
    Check(not Assigned(P.Get(Key)))
  finally
    FreeAndNil(P);
  end;
end;

procedure TPropertyTests.TestGet;
const
  Key: RawUtf8 = 'objectClass';
var
  P: TProperty;
begin
  P := TProperty.Create();
  try
    Check(not Assigned(P.Get(Key)));

    P.Add(Key, 'top');
    P.Add(Key, 'user', aoAlways);
    P.Add(Key, 'person', aoAlways);

    Check(Assigned(P.Get(Key)));
    P.Restore(Key);
    Check(not Assigned(P.Get(Key)));
  finally
    FreeAndNil(P);
  end;
end;

procedure TPropertyTests.TestGetRaw;
const
  Key: RawUtf8 = 'objectClass';
var
  P: TProperty;
begin
  P := TProperty.Create();
  try
    Check(P.GetRaw(Key) = '');
    Check(P.GetRaw(Key, -1) = '');
    Check(P.GetRaw(Key, 152) = '');

    P.Add(Key, 'top');
    P.Add(Key, 'user', aoAlways);
    P.Add(Key, 'person', aoAlways);

    Check(P.GetRaw(Key) = 'top');
    Check(P.GetRaw(Key, 1) = 'user');
    Check(P.GetRaw(Key, 2) = 'person');
    Check(P.GetRaw(Key, 3) = '');

    P.Restore(Key);

    Check(P.GetRaw(Key) = '');
  finally
    FreeAndNil(P);
  end;
end;

procedure TPropertyTests.TestGetReadable;
const
  Key: RawUtf8 = 'objectClass';
var
  P: TProperty;
begin
  P := TProperty.Create();
  try
    Check(P.GetReadable(Key) = '');
    Check(P.GetReadable(Key, -1) = '');
    Check(P.GetReadable(Key, 152) = '');

    P.Add(Key, 'top');
    P.Add(Key, 'user', aoAlways);
    P.Add(Key, 'person', aoAlways);

    Check(P.GetReadable(Key) = 'top');
    Check(P.GetReadable(Key, 1) = 'user');
    Check(P.GetReadable(Key, 2) = 'person');
    Check(P.GetReadable(Key, 3) = '');

    P.Restore(Key);

    Check(P.GetReadable(Key) = '');
  finally
    FreeAndNil(P);
  end;
end;

procedure TPropertyTests.TestGetAllReadable;
const
  Key: RawUtf8 = 'objectClass';
var
  P: TProperty;
begin
  P := TProperty.Create();
  try
    Check(Length(P.GetAllReadable(Key)) = 0);

    P.Add(Key, 'top');
    P.Add(Key, 'user', aoAlways);
    P.Add(Key, 'person', aoAlways);

    Check(Length(P.GetAllReadable(Key)) = 3);

    P.Restore(Key);

    Check(Length(P.GetAllReadable(Key)) = 0);
  finally
    FreeAndNil(P);
  end;
end;

procedure TPropertyTests.TestDCTypeFromUAC;
var
  P: TProperty;
begin
  P := TProperty.Create();
  try
    Check(P.DCTypeFromUAC = '');

    P.UserAccountControlInclude(uacServerTrusted);
    Check(P.DCTypeFromUAC = rsUACServer);
    P.UserAccountControlExclude(uacServerTrusted);

    P.UserAccountControlInclude(uacWorkstationTrusted);
    Check(P.DCTypeFromUAC = rsUACWorkstation);
    P.UserAccountControlExclude(uacWorkstationTrusted);
  finally
    FreeAndNil(P);
  end;
end;

procedure TPropertyTests.TestDirectReportsNames;
var
  P: TProperty;
begin
  P := TProperty.Create();
  try
    Check(Length(P.DirectReportsNames) = 0);
  finally
    FreeAndNil(P);
  end;
end;

procedure TPropertyTests.TestManagerName;
var
  P: TProperty;
begin
  P := TProperty.Create();
  try
    Check(P.ManagerName = '');
  finally
    FreeAndNil(P);
  end;
end;

procedure TPropertyTests.TestSubnetsFromSiteObject;
var
  P: TProperty;
begin
  P := TProperty.Create();
  try
    Check(Length(P.SubnetsFromSiteObject) = 0);
  finally
    FreeAndNil(P);
  end;
end;

procedure TPropertyTests.TestSiteFromServerReference;
var
  P: TProperty;
begin
  P := TProperty.Create();
  try
    Check(P.SiteFromServerReference = '');
  finally
    FreeAndNil(P);
  end;
end;

{ TProperty }

function TProperty.GetcanonicalName: RawUtf8;
begin
  result := DNToCN(distinguishedName);
end;

function TProperty.GetCN: RawUtf8;
begin
  result := GetReadable('cn');
end;

function TProperty.Getdescription: RawUtf8;
begin
  result := GetReadable('description');
end;

function TProperty.GetdistinguishedName: RawUtf8;
begin
  result := GetReadable('distinguishedName');
end;

function TProperty.GetLdapClient: TRsatLdapClient;
begin
  result := nil;

  if Assigned(fRSAT) then
    result := fRSAT.LdapClient;
end;

function TProperty.GetmanagedBy: RawUtf8;
begin
  result := GetReadable('managedBy');
end;

function TProperty.Getname: RawUtf8;
begin
  result := GetReadable('name');
end;

function TProperty.GetobjectClass: TRawUtf8DynArray;
begin
  result := GetAllReadable('objectClass');
end;

function TProperty.GetobjectGuid: RawUtf8;
begin
  result := GetReadable('objectGuid');
end;

function TProperty.GetobjectSid: RawUtf8;
begin
  result := GetReadable('objectSid');
end;

function TProperty.GetobjectType: RawUtf8;
var
  Attribute: TLdapAttribute;
begin
  result := '';
  Attribute := Get('objectClass');
  if not Assigned(Attribute) or (Attribute.Count <= 0) then
    Exit;
  result := Attribute.GetReadable(Attribute.Count - 1);
end;

function TProperty.GetsAMAccountName: RawUtf8;
begin
  result := GetReadable('sAMAccountName');
end;

function TProperty.GetSecurityDescriptor: PSecurityDescriptor;
begin
  result := nil;
  fSecurityDescriptor.Clear;
  if not fSecurityDescriptor.FromBinary(GetRaw('nTSecurityDescriptor')) then
    Exit;
  result := @fSecurityDescriptor;
end;

function TProperty.GetwhenChanged: TDateTime;
begin
  result := LdapToDate(GetReadable('whenChanged'));
end;

function TProperty.GetwhenCreated: TDateTime;
begin
  result := LdapToDate(GetReadable('whenCreated'));
end;

procedure TProperty.SetAttributes(AValue: TLdapAttributeList);
begin
  if Assigned(fModifiedAttributes) then
    FreeAndNil(fModifiedAttributes);
  if Assigned(fAttributes) then
    FreeAndNil(fAttributes);

  if not Assigned(AValue) then
    Exit;

  fModifiedAttributes := TLdapAttributeList.Create;
  fAttributes := TLdapAttributeList(AValue.Clone);
end;

procedure TProperty.SetCN(AValue: RawUtf8);
begin
  Add('cn', AValue);
end;

procedure TProperty.Setdescription(AValue: RawUtf8);
begin
  Add('description', AValue);
end;

procedure TProperty.SetdistinguishedName(AValue: RawUtf8);
begin
  Add('distinguishedName', AValue);
end;

procedure TProperty.SetmanagedBy(AValue: RawUtf8);
begin
  Add('managedBy', AValue);
end;

procedure TProperty.Setname(AValue: RawUtf8);
begin
  Add('name', AValue);
end;

procedure TProperty.SetobjectClass(AValue: TRawUtf8DynArray);
var
  Count, i: Integer;
begin
  Count := Length(AValue);
  if (Count <= 0) then
  begin
    Add('objectClass', '');
    Exit;
  end;
  Add('objectClass', AValue[0]);
  for i := 1 to Count - 1 do
    Add('objectClass', AValue[i], aoAlways);
end;

procedure TProperty.SetobjectGuid(AValue: RawUtf8);
begin
  Add('objectGuid', AValue);
end;

procedure TProperty.SetobjectSid(AValue: RawUtf8);
begin
  Add('objectSid', AValue);
end;

procedure TProperty.SetsAMAccountName(AValue: RawUtf8);
begin
  Add('sAMAccountName', AValue);
end;

procedure TProperty.SetSecurityDescriptor(AValue: PSecurityDescriptor);
begin
  if not Assigned(AValue) then
    Exit;

  Add('nTSecurityDescriptor', AValue^.ToBinary);
end;

procedure TProperty.SetwhenChanged(AValue: TDateTime);
begin
  Add('whenChanged', DateTimeToIso8601(AValue, False));
end;

procedure TProperty.SetwhenCreated(AValue: TDateTime);
begin
  Add('whenCreated', DateTimeToIso8601(AValue, False));
end;

function TProperty.GetReadable(Name: RawUtf8; index: Integer): RawUtf8;
var
  Attribute: TLdapAttribute;
begin
  result := '';
  Attribute := fModifiedAttributes.Find(Name);
  if Assigned(Attribute) then
    Attribute.GetReadable(index, result)
  else
    fAttributes.Find(Name).GetReadable(index, result);
end;

function TProperty.GetRaw(Name: RawUtf8; index: Integer): RawByteString;
var
  Attribute: TLdapAttribute;
begin
  Attribute := fModifiedAttributes.Find(Name);
  if Assigned(Attribute) then
    result := Attribute.GetRaw(index)
  else
    result := fAttributes.Find(Name).GetRaw(index);
end;

function TProperty.Get(Name: RawUtf8): TLdapAttribute;
begin
  result := fModifiedAttributes.Find(Name);
  if not Assigned(result) then
    result := fAttributes.Find(Name);
end;

procedure TProperty.Add(Name: RawUtf8; Value: RawUtf8; Option: TLdapAddOption);
var
  Attribute, BaseAttribute: TLdapAttribute;
  i: Integer;
begin
  // Create missing attribute list
  if not Assigned(fModifiedAttributes) then
    fModifiedAttributes := TLdapAttributeList.Create;

  // Get Attribute from name
  Attribute := fModifiedAttributes.Find(Name);
  // If Attribute is missing, copy from base Attribute value
  if not Assigned(Attribute) then
  begin
    Attribute := fModifiedAttributes.Add(Name);
    BaseAttribute := fAttributes.Find(Name);
    if Assigned(BaseAttribute) then
      for i := 0 to BaseAttribute.Count - 1 do
        Attribute.Add(BaseAttribute.GetReadable(i));
  end;

  Attribute.Add(Value, Option);
end;

procedure TProperty.Restore(Name: RawUtf8);
begin
  if Assigned(fModifiedAttributes) then
    fModifiedAttributes.Delete(Name);
end;

procedure TProperty.SearchObject(Attributes: TRawUtf8DynArray);
var
  Obj: TLdapResult;
  Attribute: TLdapAttribute;
  i: Integer;
begin
  if not Assigned(Attributes) or (Length(Attributes) = 0) then
    Exit;
  Obj := LdapClient.SearchObject(distinguishedName, '', Attributes);

  for Attribute in Obj.Attributes.Items do
  begin
    if not Assigned(Attribute) then
      continue;
    fAttributes.Add(Attribute.AttributeName, Attribute.GetReadable(), aoReplaceValue);
    for i := 1 to Attribute.Count - 1 do
      fAttributes.Add(Attribute.AttributeName, Attribute.GetReadable(i));
  end;
end;

procedure TProperty.UpdateMemberOf(MemberOfList: TRawUtf8DynArray;
  AddModify: Boolean);
const
  MODIFY_VALUE: Array[Boolean] of String = (
    'delete',
    'add'
  );
var
  MemberOf: RawUtf8;
  MemberObject: PDocVariantData;
begin
  for MemberOf in MemberOfList do
  begin
    if MemberOf = '' then
      continue;
    // Update member
    if not fTempModify.Exists(MemberOf) or not fTempModify.O[MemberOf]^.Exists('member') then
    begin
      fTempModify.O_[MemberOf]^.O_['member']^.A_[MODIFY_VALUE[AddModify]]^.AddItem(distinguishedName);
      continue;
    end;
    MemberObject := fTempModify.O[MemberOf]^.O['member'];
    // Error: Should exists
    if not Assigned(MemberObject) then
      Exit;
    // Already Exists
    if MemberObject^.Exists(MODIFY_VALUE[AddModify]) then
      continue;
    // Cancel previous modification
    if MemberObject^.Exists(MODIFY_VALUE[not AddModify]) then
      fTempModify.O_[MemberOf]^.Delete('member');
  end;
end;

function TProperty.GetSiteFromServerReference: RawUtf8;
var
  Attribute: TLdapAttribute;
  Pairs: TNameValueDNs;
begin
  result := '';
  if not Assigned(LdapClient) then
    Exit;
  Attribute := LdapClient.SearchObject(LdapClient.ConfigDN, FormatUtf8('(serverReference=%)', [distinguishedName]), 'distinguishedName', lssWholeSubtree);
  if not Assigned(Attribute) then
    Exit;
  if not ParseDN(Attribute.GetReadable(), Pairs, True) then
    Exit;
  if Length(Pairs) < 3 then
    Exit;
  result := Pairs[2].Value;
end;

function TProperty.GetSubnetsFromSiteObject: TRawUtf8DynArray;
var
  SearchResult: TLdapResult;
  Count: Integer;
begin
  result := nil;
  Count := 0;

  if not Assigned(LdapClient) then
    Exit;

  LdapClient.SearchBegin();
  try
    LdapClient.SearchScope := lssWholeSubtree;
    repeat
      if not LdapClient.Search(LdapClient.ConfigDN, False, FormatUtf8('(siteObject=%)', [LdapEscape(distinguishedName)]), ['cn']) then
        Exit;
      SetLength(result, Count + LdapClient.SearchResult.Count);
      for SearchResult in LdapClient.SearchResult.Items do
      begin
        if not Assigned(SearchResult) then
          continue;
        Insert(SearchResult.Find('cn').GetReadable(), result, Count);
        Inc(Count);
      end;
    until LdapClient.SearchCookie = '';
  finally
    LdapClient.SearchEnd;
  end;
end;

function TProperty.GetDCTypeFromUAC: RawUtf8;
var
  UserAccountControls: TUserAccountControls;
begin
  result := '';

  UserAccountControls := UserAccountControlsFromText(GetReadable('userAccountControl'));
  if uacWorkstationTrusted in UserAccountControls then
    result := rsUACWorkstation
  else if uacServerTrusted in UserAccountControls then
    result := rsUACServer
end;

function TProperty.GetDirectReportsNames: TRawUtf8DynArray;
var
  DirectReports: TRawUtf8DynArray;
  DirectReport, Filter: RawUtf8;
  Count: Integer;
  SearchResult: TLdapResult;
begin
  result := nil;
  DirectReports := GetAllReadable('directReports');
  Filter := '';
  for DirectReport in DirectReports do
    Filter := FormatUtf8('%(distinguishedName=%)', [Filter, LdapEscape(DirectReport)]);
  if Filter = '' then
    Exit;
  Filter := FormatUtf8('(|%)', [Filter]);
  Count := 0;

  LdapClient.SearchBegin();
  try
    LdapClient.SearchScope := lssWholeSubtree;
    repeat
      if not LdapClient.Search(LdapClient.DefaultDN, False, Filter, ['name']) then
        Exit;
      SetLength(result, Count + LdapClient.SearchResult.Count);
      for SearchResult in LdapClient.SearchResult.Items do
      begin
        if not Assigned(SearchResult) then
          Continue;
        Insert(SearchResult.Find('name').GetReadable(), result, Count);
        Inc(Count);
      end;
    until LdapClient.SearchCookie = '';
  finally
    LdapClient.SearchEnd;
  end;
end;

function TProperty.GetManagerName: RawUtf8;
var
  Manager: RawUtf8;
begin
  result := '';
  Manager := GetReadable('manager');
  if Manager <> '' then
    result := LdapClient.SearchObject(Manager, '', 'name').GetReadable();
end;

function TProperty.ApplyAttributeDifference: Boolean;
var
  i: Integer;
begin
  result := not Assigned(fModifiedAttributes);
  if result then
    Exit;

  result := True;
  for i := 0 to fModifiedAttributes.Count - 1 do
  begin
    if fModifiedAttributes.Items[i].AttributeName = '' then
      continue;
    result := result and ApplyAttributeDifference(fModifiedAttributes.Items[i].AttributeName);
    if not result and (LdapClient.ResultString <> '') then
      break;
  end;
  FreeAndNil(fModifiedAttributes);
end;

function TProperty.ApplyAttributeDifference(AttributeName: RawUtf8): Boolean;
var
  ModifiedAttribute, Attribute: TLdapAttribute;
  ToAdd, ToDelete: TRawUtf8DynArray;
  i, idx: Integer;
  Value: RawByteString;
begin
  result := False;
  if AttributeName = '' then
    Exit;
  ModifiedAttribute := fModifiedAttributes.Find(AttributeName);
  Attribute := fAttributes.Find(AttributeName);

  // No value, should not happen
  if not Assigned(Attribute) and (not Assigned(ModifiedAttribute) or (ModifiedAttribute.GetReadable() = '')) then
    Exit;

  result := True;

  // New value, add it
  if not Assigned(Attribute) and Assigned(ModifiedAttribute) then
  begin
    result := LdapClient.Modify(distinguishedName, lmoAdd, ModifiedAttribute);
    Exit;
  end;

  // No more value, delete it
  if Assigned(Attribute) and (not Assigned(ModifiedAttribute) or (ModifiedAttribute.GetReadable() = '')) then
  begin
    result := LdapClient.Modify(distinguishedName, lmoDelete, Attribute);
    Exit;
  end;

  // Unique value changed, replace it
  if (Attribute.Count = 1) and (Attribute.Count = ModifiedAttribute.Count) then
  begin
    result := LdapClient.Modify(distinguishedName, lmoReplace, ModifiedAttribute);
    Exit;
  end;

  // Array updated. List new and old values
  ToAdd := [];
  ToDelete := [];
  for i := 0 to Attribute.Count - 1 do
  begin
    Value := Attribute.GetRaw(i);
    idx := ModifiedAttribute.FindIndex(Value);
    if idx < 0 then
      Insert(Value, ToDelete, 0);
  end;
  for i := 0 to ModifiedAttribute.Count - 1 do
  begin
    Value := ModifiedAttribute.GetRaw(i);
    idx := Attribute.FindIndex(Value);
    if idx < 0 then
      Insert(Value, ToAdd, 0);
  end;

  // Add new values
  Attribute := TLdapAttribute.Create(AttributeName, atUndefined);
  try
    if Length(ToAdd) > 0 then
    begin
      for Value in ToAdd do
        Attribute.Add(Value);
      result := LdapClient.Modify(distinguishedName, lmoAdd, Attribute);
    end;
  finally
    FreeAndNil(Attribute);
  end;

  // Remove old values
  Attribute := TLdapAttribute.Create(AttributeName, atUndefined);
  try
    if Length(ToDelete) > 0 then
    begin
      for Value in ToDelete do
        Attribute.Add(Value);
      result := LdapClient.Modify(distinguishedName, lmoDelete, Attribute);
    end;
  finally
    FreeAndNil(Attribute);
  end;
end;

function TProperty.ApplyTempModification: Boolean;
var
  DistinguishedNameArray, AttributeNameArray: TRawUtf8DynArray;
  ADistinguishedName, AttributeName, Value: RawUtf8;

  function InnerModify(DistinguishedName, AttributeName: RawUtf8; Modifier: TLdapModifyOp): Boolean;
  var
    ModifierString: RawUtf8;
    Attribute: TLdapAttribute;
    AttributeObject: PDocVariantData;
  begin
    result := False;
    case Modifier of
      lmoAdd: ModifierString := 'add';
      lmoDelete: ModifierString := 'delete';
      lmoReplace: ModifierString := 'replace';
      else
        Exit;
    end;

    AttributeObject := fTempModify.O[DistinguishedName]^.O[AttributeName];
    if AttributeObject^.Exists(ModifierString) then
    begin
      Attribute := TLdapAttribute.Create(AttributeName, atUndefined);
      try
        for Value in AttributeObject^.A[ModifierString]^.ToRawUtf8DynArray do
          Attribute.Add(Value);
        result := LdapClient.Modify(DistinguishedName, Modifier, Attribute);
        if not result then
          Exit;
      finally
        FreeAndNil(Attribute);
      end;
    end;
  end;

begin
  result := True;
  DistinguishedNameArray := fTempModify.GetNames;

  for ADistinguishedName in DistinguishedNameArray do
  begin
    AttributeNameArray := fTempModify.O[ADistinguishedName]^.GetNames;
    for AttributeName in AttributeNameArray do
    begin
      InnerModify(ADistinguishedName, AttributeName, lmoAdd);
      InnerModify(ADistinguishedName, AttributeName, lmoDelete);
      InnerModify(ADistinguishedName, AttributeName, lmoReplace);
    end;
  end;
  fTempModify.Clear;
end;

constructor TProperty.Create(ARSAT: TRSAT);
begin
  fRSAT := ARSAT;

  fTempModify.Init();
end;

destructor TProperty.Destroy;
begin
  FreeAndNil(fAttributes);
  FreeAndNil(fModifiedAttributes);

  inherited Destroy;
end;

function TProperty.IsModified: Boolean;
var
  Attribute: TLdapAttribute;
begin
  result := (fTempModify.Count > 0);
  if result then
    Exit;
  if not Assigned(fModifiedAttributes) then
    Exit;
  for Attribute in fModifiedAttributes.Items do
  begin
    if not Assigned(Attribute) then
      continue;
    result := IsModified(Attribute.AttributeName);
    if result then
      Exit;
  end;
end;

function TProperty.IsModified(AttributeName: RawUtf8): Boolean;
var
  ModifiedAttribute, BaseAttribute: TLdapAttribute;
  i: Integer;
begin
  result := False;
  ModifiedAttribute := fModifiedAttributes.Find(AttributeName);
  if not Assigned(ModifiedAttribute) then
    Exit;

  result := True;
  BaseAttribute := fAttributes.Find(AttributeName);
  if not Assigned(BaseAttribute) or (BaseAttribute.Count <> ModifiedAttribute.Count) then
    Exit;
  for i := 0 to BaseAttribute.Count - 1 do
    if BaseAttribute.GetReadable(i) <> ModifiedAttribute.GetReadable(i) then
      Exit;
  result := False;
end;

function TProperty.ApplyModification: Boolean;
var
  LdapObject: TLdapResult;
begin
  result := ApplyAttributeDifference and ApplyTempModification;

  LdapObject := LdapClient.SearchObject(distinguishedName, '', ['*']);
  if Assigned(LdapObject) then
    Attributes := LdapObject.Attributes;
end;

function TProperty.GetAllReadable(Name: RawUtf8): TRawUtf8DynArray;
var
  Attribute: TLdapAttribute;
begin
  result := nil;
  Attribute := fModifiedAttributes.Find(Name);
  if Assigned(Attribute) then
    result := Attribute.GetAllReadable
  else
    result := fAttributes.Find(Name).GetAllReadable;
end;

procedure TProperty.AddMemberOf(MemberOfList: TRawUtf8DynArray);
begin
  UpdateMemberOf(MemberOfList, True);
end;

procedure TProperty.DeleteMemberOf(MemberOfList: TRawUtf8DynArray);
begin
  UpdateMemberOf(MemberOfList, False);
end;

procedure TProperty.Subnets(Data: PDocVariantData);
var
  SearchResult: TLdapResult;
  SiteName, SiteDistinguishedName: RawUtf8;
begin
  LdapClient.SearchBegin();
  try
    LdapClient.SearchScope := lssWholeSubtree;
    repeat
      if not LdapClient.Search(LdapClient.ConfigDN, False, '(objectClass=site)', ['name', 'distinguishedName']) then
        Exit;

      for SearchResult in LdapClient.SearchResult.Items do
      begin
        if not Assigned(SearchResult) then
          continue;
        SiteName := SearchResult.Find('name').GetReadable();
        SiteDistinguishedName := SearchResult.Find('distinguishedName').GetReadable();
        Data^.O_['name']^.S[SiteName] := SiteDistinguishedName;
        Data^.O_['dn']^.S[SiteDistinguishedName] := SiteName;
      end;
    until LdapClient.SearchCookie = '';
  finally
    LdapClient.SearchEnd;
  end;
end;

function TProperty.UPNSuffixes: TRawUtf8DynArray;
var
  Attribute: TLdapAttribute;
begin
  LdapClient.SearchRangeBegin;
  try
    Attribute := LdapClient.SearchObject(FormatUtf8('CN=Partitions,%', [LdapClient.ConfigDN]), '', 'uPNSuffixes');
    if not Assigned(Attribute) and (LdapClient.ResultCode > 0) then
      Exit;
  finally
    LdapClient.SearchRangeEnd;
  end;
  result := Attribute.GetAllReadable;
  Insert(DNToCN(LdapClient.DefaultDN), result, 0);
  if (LdapClient.DefaultDN <> LdapClient.RootDN) then
    Insert(DNToCN(LdapClient.RootDN), result, 1);
end;

function TProperty.CannotChangePassword: Boolean;
var
  PSecDesc: PSecurityDescriptor;
  AceSelf, AceWorld: Integer;
begin
  // https://learn.microsoft.com/fr-fr/windows/win32/adsi/reading-user-cannot-change-password-ldap-provider
  result := False;
  PSecDesc := SecurityDescriptor;

  if not Assigned(PSecDesc) then
    Exit;

  AceSelf  := SecDescFindACE(PSecDesc,
    satObjectAccessDenied, KnownRawSid(wksSelf),
    [samControlAccess], @ATTR_UUID[kaUserChangePassword]);
  AceWorld := SecDescFindACE(PSecDesc,
    satObjectAccessDenied, KnownRawSid(wksWorld),
    [samControlAccess], @ATTR_UUID[kaUserChangePassword]);

  result := (AceSelf <> -1) and (AceWorld <> -1);
end;

function TProperty.msFVERecoveryInformation: TFVERecoveryInformationDynArray;
var
  SearchResult: TLdapResult;
  Count: Integer;
begin
  result := nil;
  Count := 0;

  LdapClient.SearchBegin();
  try
    LdapClient.SearchScope := lssSingleLevel;
    repeat
      if not LdapClient.Search(DistinguishedName, False, '(objectClass=msFVE-RecoveryInformation)', ['cn', 'msFVE-RecoveryPassword']) then
        Exit;

      SetLength(result, Count + LdapClient.SearchResult.Count);
      for SearchResult in LdapClient.SearchResult.Items do
      begin
        if not Assigned(SearchResult) then
          continue;
        result[Count].cn := SearchResult.Find('cn').GetReadable();
        result[Count].msFVERecoveryPassword := SearchResult.Find('msFVE-RecoveryPassword').GetReadable();
        Inc(Count);
      end;
    until (LdapClient.SearchCookie = '');
  finally
    LdapClient.SearchEnd;
  end;
end;

function TProperty.AttributesFromSchema: TRawUtf8DynArray;
var
  AObjectClass: TRawUtf8DynArray;
  ObjClass: RawUtf8;
  SearchResult: TLdapResult;
  Attribute: TLdapAttribute;
  Filter, Value, r: RawUtf8;
  Count: Integer;
  found: Boolean;
begin
  result := [];
  AObjectClass := ObjectClass;
  Count := 0;

  repeat
    if not assigned(AObjectClass) or (Length(AObjectClass) <= 0) then
      Exit;
    Filter := '';
    for ObjClass in AObjectClass do
      Filter := FormatUtf8('%(lDAPDisplayName=%)', [Filter, LdapEscape(ObjClass)]);
    if Filter <> '' then
      Filter := FormatUtf8('(|%)', [Filter]);
    AObjectClass := [];

    RSAT.LdapClient.SearchBegin();
    try
      RSAT.LdapClient.SearchScope := lssWholeSubtree;
      repeat
        if not RSAT.LdapClient.Search(RSAT.LdapClient.SchemaDN, False, Filter, ['mustContain', 'systemMustContain', 'mayContain', 'systemMayContain', 'auxiliaryClass', 'systemAuxiliaryClass']) then
          Exit;

        for SearchResult in RSAT.LdapClient.SearchResult.Items do
        begin
          if not Assigned(SearchResult) then
            continue;
          for Attribute in SearchResult.Attributes.Items do
          begin
            if not Assigned(Attribute) then
              continue;

            case Attribute.AttributeName of
              'mustContain', 'systemMustContain', 'mayContain', 'systemMayContain':
              begin
                for Value in Attribute.GetAllReadable do
                begin
                  found := False;
                  for r in result do
                  begin
                    found := (r = value);
                    if found then
                      break;
                  end;
                  if not found then
                  begin
                    Insert(value, result, Count);
                    Inc(Count);
                  end;
                end;
              end;
              'auxiliaryClass', 'systemAuxiliaryClass': AObjectClass := Concat(AObjectClass, Attribute.GetAllReadable);
            end;
          end;
        end;
      until RSAT.LdapClient.SearchCookie = '';
    finally
      RSAT.LdapClient.SearchEnd;
    end;
  until not Assigned(AObjectClass) or (Length(AObjectClass) <= 0);
end;

procedure TProperty.UserAccountControlExclude(
  UserAccountControl: TUserAccountControl);
var
  UserAccountControls: TUserAccountControls;
begin
  UserAccountControls := UserAccountControlsFromText(GetReadable('userAccountControl'));
  Exclude(UserAccountControls, UserAccountControl);
  Add('userAccountControl', IntToStr(UserAccountControlsValue(UserAccountControls)));
end;

procedure TProperty.UserAccountControlInclude(
  UserAccountControl: TUserAccountControl);
var
  UserAccountControls: TUserAccountControls;
begin
  UserAccountControls := UserAccountControlsFromText(GetReadable('userAccountControl'));
  Include(UserAccountControls, UserAccountControl);
  Add('userAccountControl', IntToStr(UserAccountControlsValue(UserAccountControls)));
end;

procedure TProperty.msDSSupportedEncryptionTypeInclude(
  msDSSupportedEncrytionType: TMsdsSupportedEncryptionType);
var
  msDSSupportedEncrytionTypes: TMsdsSupportedEncryptionTypes;
begin
  msDSSupportedEncrytionTypes := MsdsSupportedEncryptionTypesFromText(GetReadable('msDS-SupportedEncryptionTypes'));
  Include(msDSSupportedEncrytionTypes, msDSSupportedEncrytionType);
  Add('msDS-SupportedEncryptionTypes', IntToStr(MsdsSupportedEncryptionTypesValue(msDSSupportedEncrytionTypes)));
end;

procedure TProperty.msDSSupportedEncryptionTypeExclude(
  msDSSupportedEncrytionType: TMsdsSupportedEncryptionType);
var
  msDSSupportedEncrytionTypes: TMsdsSupportedEncryptionTypes;
begin
  msDSSupportedEncrytionTypes := MsdsSupportedEncryptionTypesFromText(GetReadable('msDS-SupportedEncryptionTypes'));
  Exclude(msDSSupportedEncrytionTypes, msDSSupportedEncrytionType);
  Add('msDS-SupportedEncryptionTypes', IntToStr(MsdsSupportedEncryptionTypesValue(msDSSupportedEncrytionTypes)));
end;

{$IFDEF OPENRSATTESTS}

{$ENDIF OPENRSATTESTS}

end.

