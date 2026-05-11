unit fixture.ldapclient;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.base,
  mormot.core.text,
  mormot.net.ldap;

function GetBaseDN(DefaultDN: RawUtf8): RawUtf8;
function SetupLdapClient: TLdapClient;
function BaseDNExists(LdapClient: TLdapClient; BaseDN: RawUtf8): Boolean;
function SetupTestUser(LdapClient: TLdapClient; UserName: RawUtf8; out DN: RawUtf8): Boolean;
function SetupTestGroup(LdapClient: TLdapClient; GroupName: RawUtf8; out DN: RawUtf8): Boolean;

implementation

function GetBaseDN(DefaultDN: RawUtf8): RawUtf8;
begin
  result := GetEnvironmentVariable('DELEGATION_OU');
  if result = '' then
    result := FormatUtf8('OU=test-openrsat,%', [DefaultDN]);
end;

function SetupLdapClient: TLdapClient;
begin
  result := TLdapClient.Create;
  result.Settings.UserName := GetEnvironmentVariable('LDAP_USERNAME');
  result.Settings.Password := GetEnvironmentVariable('LDAP_PASSWORD');
  result.Settings.KerberosDN := GetEnvironmentVariable('LDAP_DOMAIN');
  result.Settings.TargetHost := GetEnvironmentVariable('LDAP_DOMAINCONTROLLER');
  result.Settings.AutoBind := lcbKerberos;
  result.Settings.AutoReconnect := True;
  if not result.Connect() or not result.Connected then
    raise Exception.Create('Failed to connect on LdapClient setup.');

  if not BaseDNExists(result, GetBaseDN(result.DefaultDN())) then
    raise Exception.Create('BaseDN for tests does not exists.');
end;

function BaseDNExists(LdapClient: TLdapClient; BaseDN: RawUtf8): Boolean;
var
  ObjectClassAttr: TLdapAttribute;
begin
  result := False;
  ObjectClassAttr := LdapClient.SearchObject(BaseDN, '', 'objectClass');
  result := Assigned(ObjectClassAttr) and
    (ObjectClassAttr.GetReadable(Pred(ObjectClassAttr.Count)) = 'organizationalUnit');
end;

function SetupTestUser(LdapClient: TLdapClient; UserName: RawUtf8; out DN: RawUtf8): Boolean;
var
  Attributes: TLdapAttributeList;
  Attr: TLdapAttribute;
begin
  result := False;

  Attributes := TLdapAttributeList.create;
  try
    Attr := Attributes.Add('objectClass', 'top');
    Attr.Add('person');
    Attr.Add('organizationalPerson');
    Attr.Add('user');

    DN := FormatUtf8('CN=%_u,%', [UserName, GetBaseDN(LdapClient.DefaultDN())]);
    if not LdapClient.Add(DN, Attributes) then
    begin
      if LdapClient.ResultError <> leEntryAlreadyExists then
      begin
        DN := '';
        Exit;
      end;
    end;
    result := True;
  finally
    FreeAndNil(Attributes);
  end;
end;

function SetupTestGroup(LdapClient: TLdapClient; GroupName: RawUtf8; out DN: RawUtf8): Boolean;
var
  Attributes: TLdapAttributeList;
  Attr: TLdapAttribute;
begin
  result := False;

  Attributes := TLdapAttributeList.Create;
  try
    Attr := Attributes.Add('objectClass', 'top');
    Attr.Add('group');

    DN := FormatUtf8('CN=%_g,%', [GroupName, GetBaseDN(LdapClient.DefaultDN())]);
    if not LdapClient.Add(DN, Attributes) then
    begin
      if LdapClient.ResultError <> leEntryAlreadyExists then
      begin
        DN := '';
        Exit;
      end;
    end;
    result := True;
  finally
    FreeAndNil(Attributes);
  end;
end;

end.

