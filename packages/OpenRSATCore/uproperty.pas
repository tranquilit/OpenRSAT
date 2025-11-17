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
    fBaseAttributes, fAttributes: TLdapAttributeList;
    fTempModify: TDocVariantData;

    fCore: ICore;

    function GetReadable(Name: RawUtf8; index: Integer = 0): RawUtf8;
    function GetRaw(Name: RawUtf8; index: Integer = 0): RawByteString;
    procedure Add(Name: RawUtf8; Value: RawUtf8; Option: TLdapAddOption = aoReplaceValue);

    procedure UpdateMemberOf(MemberOfList: TRawUtf8DynArray; AddModify: Boolean = True);
  public
    constructor Create(Core: ICore = nil);
    destructor Destroy; override;

    function IsModified(Fast: Boolean = True): Boolean;
    function IsModified(AttributeName: RawUtf8): Boolean;

    procedure AddMemberOf(MemberOfList: TRawUtf8DynArray);
    procedure DeleteMemberOf(MemberOfList: TRawUtf8DynArray);
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
  mormot.core.datetime;

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
  if Assigned(fBaseAttributes) then
    FreeAndNil(fBaseAttributes);
  if Assigned(fAttributes) then
    FreeAndNil(fAttributes);

  fBaseAttributes := TLdapAttributeList(AValue.Clone);
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
var
  Attribute: TLdapAttribute;
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
begin
  fAttributes.Find(Name).GetReadable(index, result);
end;

function TProperty.GetRaw(Name: RawUtf8; index: Integer): RawByteString;
begin
  result := fAttributes.Find(Name).GetRaw(index);
end;

procedure TProperty.Add(Name: RawUtf8; Value: RawUtf8; Option: TLdapAddOption);
var
  Attribute: TLdapAttribute;
begin
  if not Assigned(fAttributes) then
    fAttributes := TLdapAttributeList.Create;

  Attribute := fAttributes.Find(Name);
  if not Assigned(Attribute) then
    Attribute := fAttributes.Add(Name);

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

constructor TProperty.Create(Core: ICore);
begin
  fCore := Core;

  fTempModify.Init();
end;

destructor TProperty.Destroy;
begin
  FreeAndNil(fAttributes);
  FreeAndNil(fBaseAttributes);

  inherited Destroy;
end;

function TProperty.IsModified(Fast: Boolean): Boolean;
var
  i: Integer;
begin
  result := fAttributes.Count <> fBaseAttributes.Count;
  if result then
    Exit;

  result := True;

  for i := 0 to fAttributes.Count - 1 do
  begin
    if IsModified(fAttributes.Items[i].AttributeName) then
      Exit;
  end;
  result := False;
end;

function TProperty.IsModified(AttributeName: RawUtf8): Boolean;
var
  Attribute, BaseAttribute: TLdapAttribute;
  i: Integer;
  ModifiedDistinguishedName, ModifiedAttribute: RawUtf8;
begin
  Attribute := fAttributes.Find(AttributeName);
  BaseAttribute := fBaseAttributes.Find(AttributeName);

  result := False;
  if not Assigned(Attribute) and not Assigned(BaseAttribute) then
    Exit;

  result := (not Assigned(Attribute) and Assigned(BaseAttribute)) or
            (Assigned(Attribute) and not Assigned(BaseAttribute)) or
            (Attribute.Count <> BaseAttribute.Count);
  if result then
    Exit;

  result := True;
  for i := 0 to Attribute.Count - 1 do
    if Attribute.GetReadable(i) <> BaseAttribute.GetReadable(i) then
      Exit;

  for ModifiedDistinguishedName in fTempModify.GetNames do
  begin
    if ModifiedDistinguishedName = '' then
      continue;
    for ModifiedAttribute in fTempModify.O[ModifiedDistinguishedName]^.GetNames do
    begin
      if ModifiedAttribute = '' then
        continue;
      Exit;
    end;
  end;
  result := False;
end;

procedure TProperty.AddMemberOf(MemberOfList: TRawUtf8DynArray);
begin
  UpdateMemberOf(MemberOfList, True);
end;

procedure TProperty.DeleteMemberOf(MemberOfList: TRawUtf8DynArray);
begin
  UpdateMemberOf(MemberOfList, False);
end;

end.

