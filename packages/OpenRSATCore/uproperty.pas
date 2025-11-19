unit uproperty;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.base,
  mormot.core.os.security,
  mormot.core.variants,
  mormot.net.ldap,
  ucommon,
  uinterfacecore,
  ursatldapclient;

type
  { TProperty }

  TProperty = class
  private
    fModifiedAttributes, fAttributes: TLdapAttributeList;
    fTempModify: TDocVariantData;

    fCore: ICore;

    function GetDCTypeFromUAC: RawUtf8;
    function GetSiteFromServerReference: RawUtf8;
    function GetSubnetsFromSiteObject: TRawUtf8DynArray;
    procedure UpdateMemberOf(MemberOfList: TRawUtf8DynArray; AddModify: Boolean = True);

    function ApplyAttributeDifference: Boolean;
    function ApplyAttributeDifference(AttributeName: RawUtf8): Boolean;
    function ApplyTempModification: Boolean;
  public
    constructor Create(Core: ICore = nil);
    destructor Destroy; override;

    function IsModified: Boolean;
    function IsModified(AttributeName: RawUtf8): Boolean;
    function ApplyModification: Boolean;

    function GetAllReadable(Name: RawUtf8): TRawUtf8DynArray;
    function GetReadable(Name: RawUtf8; index: Integer = 0): RawUtf8;
    function GetRaw(Name: RawUtf8; index: Integer = 0): RawByteString;

    procedure Add(Name: RawUtf8; Value: RawUtf8; Option: TLdapAddOption = aoReplaceValue);

    procedure AddMemberOf(MemberOfList: TRawUtf8DynArray);
    procedure DeleteMemberOf(MemberOfList: TRawUtf8DynArray);

    procedure Subnets(Data: PDocVariantData);

    property SiteFromServerReference: RawUtf8 read GetSiteFromServerReference;
    property SubnetsFromSiteObject: TRawUtf8DynArray read GetSubnetsFromSiteObject;
    property DCTypeFromUAC: RawUtf8 read GetDCTypeFromUAC;
  private
    fSecurityDescriptor: TSecurityDescriptor;

    function GetcanonicalName: RawUtf8;
    function GetCN: RawUtf8;
    function Getdescription: RawUtf8;
    function GetdistinguishedName: RawUtf8;
    function GetLdapClient: TRsatLdapClient;
    function GetmanagedBy: RawUtf8;
    function Getname: RawUtf8;
    function GetobjectGuid: RawUtf8;
    function GetobjectSid: RawUtf8;
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
    procedure SetobjectGuid(AValue: RawUtf8);
    procedure SetobjectSid(AValue: RawUtf8);
    procedure SetsAMAccountName(AValue: RawUtf8);
    procedure SetSecurityDescriptor(AValue: PSecurityDescriptor);
    procedure SetwhenChanged(AValue: TDateTime);
    procedure SetwhenCreated(AValue: TDateTime);
  public
    property Core: ICore read fCore write fCore;
    property LdapClient: TRsatLdapClient read GetLdapClient;

    property Attributes: TLdapAttributeList read fAttributes write SetAttributes;
    property sAMAccountName: RawUtf8 read GetsAMAccountName write SetsAMAccountName;
    property distinguishedName: RawUtf8 read GetdistinguishedName write SetdistinguishedName;
    property canonicalName: RawUtf8 read GetcanonicalName;
    property name: RawUtf8 read Getname write Setname;
    property CN: RawUtf8 read GetCN write SetCN;
    property description: RawUtf8 read Getdescription write Setdescription;
    property managedBy: RawUtf8 read GetmanagedBy write SetmanagedBy;
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

  if Assigned(fCore) then
    result := fCore.LdapClient;
end;

function TProperty.GetmanagedBy: RawUtf8;
begin
  result := GetReadable('managedBy');
end;

function TProperty.Getname: RawUtf8;
begin
  result := GetReadable('name');
end;

function TProperty.GetobjectGuid: RawUtf8;
begin
  result := GetReadable('objectGuid');
end;

function TProperty.GetobjectSid: RawUtf8;
begin
  result := GetReadable('objectSid');
end;

function TProperty.GetsAMAccountName: RawUtf8;
begin
  result := GetReadable('sAMAccountName');
end;

function TProperty.GetSecurityDescriptor: PSecurityDescriptor;
var
  Attribute: TLdapAttribute;
begin
  result := nil;
  Attribute := Attributes.Find('nTSecurityDescriptor');
  if not Assigned(Attribute) then
    Exit;
  fSecurityDescriptor.Clear;
  if not fSecurityDescriptor.FromBinary(Attribute.GetRaw()) then
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

  LdapClient.SearchBegin();
  try
    LdapClient.SearchScope := lssWholeSubtree;
    repeat
      if not LdapClient.Search(LdapClient.ConfigDN, False, FormatUtf8('(siteObject=%)', [LdapEscape(distinguishedName)]), ['cn']) then
      begin
        ShowLdapSearchError(LdapClient);
        Exit;
      end;
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
    begin
      ShowLdapModifyError(LdapClient);
      break;
    end;
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
  if not Assigned(Attribute) and not Assigned(ModifiedAttribute) then
    Exit;

  result := True;

  // New value, add it
  if not Assigned(Attribute) and Assigned(ModifiedAttribute) then
  begin
    result := LdapClient.Modify(distinguishedName, lmoAdd, ModifiedAttribute);
    Exit;
  end;

  // No more value, delete it
  if Assigned(Attribute) and not Assigned(ModifiedAttribute) then
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
  Attribute := TLdapAttribute.Create;
  try
    for Value in ToAdd do
      Attribute.Add(Value);
    result := LdapClient.Modify(distinguishedName, lmoAdd, Attribute);
  finally
    FreeAndNil(Attribute);
  end;

  // Remove old values
  Attribute := TLdapAttribute.Create;
  try
    for Value in ToDelete do
      Attribute.Add(Value);
    result := LdapClient.Modify(distinguishedName, lmoDelete, Attribute);
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
        begin
          ShowLdapModifyError(LdapClient);
          Exit;
        end;
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

constructor TProperty.Create(Core: ICore);
begin
  fCore := Core;

  fTempModify.Init();
end;

destructor TProperty.Destroy;
begin
  FreeAndNil(fAttributes);
  FreeAndNil(fModifiedAttributes);

  inherited Destroy;
end;

function TProperty.IsModified: Boolean;
begin
  result := (Assigned(fModifiedAttributes) and (fModifiedAttributes.Count > 0)) or (fTempModify.Count > 0);
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
  if BaseAttribute.Count <> ModifiedAttribute.Count then
    Exit;
  for i := 0 to BaseAttribute.Count - 1 do
    if BaseAttribute.GetReadable(i) <> ModifiedAttribute.GetReadable(i) then
      Exit;
  result := False;
end;

function TProperty.ApplyModification: Boolean;
begin
  ApplyAttributeDifference;
  ApplyTempModification;
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
      begin
        ShowLdapSearchError(LdapClient);
        Exit;
      end;

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

end.

