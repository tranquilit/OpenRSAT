unit uproperty;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.base,
  mormot.net.ldap,
  uinterfacecore,
  uinterfaceproperty,
  ursatldapclient;

type

  { TProperty }

  TProperty = class
  private
    fBaseAttributes, fAttributes: TLdapAttributeList;

    fCore: ICore;

    function GetReadable(Name: RawUtf8; index: Integer = 0): RawUtf8;
    function GetRaw(Name: RawUtf8; index: Integer = 0): RawByteString;
    procedure Add(Name: RawUtf8; Value: RawUtf8; Option: TLdapAddOption = aoReplaceValue);
  public
    constructor Create(Core: ICore = nil);
    destructor Destroy; override;
  private
    function GetcanonicalName: RawUtf8;
    function GetCN: RawUtf8;
    function Getdescription: RawUtf8;
    function GetdistinguishedName: RawUtf8;
    function GetLdapClient: TRsatLdapClient;
    function Getname: RawUtf8;
    function GetobjectGuid: RawUtf8;
    function GetobjectSid: RawUtf8;
    function GetsAMAccountName: RawUtf8;
    function GetwhenChanged: TDateTime;
    function GetwhenCreated: TDateTime;
    procedure SetAttributes(AValue: TLdapAttributeList);
    procedure SetCN(AValue: RawUtf8);
    procedure Setdescription(AValue: RawUtf8);
    procedure SetdistinguishedName(AValue: RawUtf8);
    procedure Setname(AValue: RawUtf8);
    procedure SetobjectGuid(AValue: RawUtf8);
    procedure SetobjectSid(AValue: RawUtf8);
    procedure SetsAMAccountName(AValue: RawUtf8);
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
    property objectSid: RawUtf8 read GetobjectSid write SetobjectSid;
    property objectGuid: RawUtf8 read GetobjectGuid write SetobjectGuid;
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

constructor TProperty.Create(Core: ICore);
begin

end;

destructor TProperty.Destroy;
begin
  FreeAndNil(fAttributes);
  FreeAndNil(fBaseAttributes);

  inherited Destroy;
end;

end.

