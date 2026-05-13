unit unittest.ugeneratekeytab;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.test,
  mormot.core.base,
  mormot.core.os.security,
  mormot.crypt.core,
  mormot.crypt.secure,
  mormot.net.ldap,
  ucommon;

type

  { TUnitTestGenerateKeyTab }

  TUnitTestGenerateKeyTab = class(TSynTestCase)
  published
    procedure GenerateKeyTab_MissingLdapClient;
    procedure PrepareKeyTab_Valid_User;
    procedure PrepareKeyTab_Valid_Computer;
    procedure PrepareKeyTab_EmptyPassword;
    procedure PrepareKeyTab_EmptyEncryptionTypes;
    procedure PrepareKeyTab_EmptyEntryObject;
    procedure PrepareKeyTab_EmptyRealm;
    //procedure GenerateKeyTab_InvalidDistinguishedName;
    //procedure GenerateKeyTab_WithPassword;
    //procedure GenerateKeyTab_WithoutPassword;
    //procedure GenerateKeyTab_WithoutEncryptionTypes;
    //procedure GenerateKeyTab_MissingReadRightsOnDistinguishedName;
    //procedure GenerateKeyTab_MissingWriteRightsOnDistinguishedName;
    //procedure GenerateKeyTab_Computer;
    //procedure GenerateKeyTab_User;
  end;

implementation

{ TUnitTestGenerateKeyTab }

procedure TUnitTestGenerateKeyTab.GenerateKeyTab_MissingLdapClient;
var
  KeyTabGenerator: TKerberosKeyTabGenerator;
  LdapClient: TLdapClient;
begin
  try
    KeyTabGenerator := GenerateKeyTab(nil, '');
    Check(False, 'Should raise a missing ldap error.');
  except
    on E: TGenerateKeyTabException do Check(True, 'Cannot generate KeyTabGenerator without LdapClient.');
  end;

  LdapClient := TLdapClient.Create;
  try
    try
      KeyTabGenerator := GenerateKeyTab(LdapClient, '');
      Check(False, 'should raise a not connected ldap error.');
    except
      on E: TGenerateKeyTabException do Check(True, 'Cannot generate KeyTabGenerator without established connection with LdapClient.');
    end;
  finally
    FreeAndNil(LdapClient);
  end;
end;

procedure TUnitTestGenerateKeyTab.PrepareKeyTab_Valid_User;
var
  Password: SpiUtf8;
  EntryObject: TLdapResult;
  Attr: TLdapAttribute;
  KeyTabGenerator: TKerberosKeyTabGenerator;
begin
  Password := TAesPrng.Main.RandomPassword(64);
  EntryObject := TLdapResult.Create;
  try
    Attr := EntryObject.FindOrAdd('objectClass');
    Attr.Add('top');
    Attr.Add('organizationalPerson');
    Attr.Add('person');
    Attr.Add('user');
    Attr := EntryObject.FindOrAdd('sAMAccountName');
    Attr.Add('test_user');
    Attr := EntryObject.FindOrAdd('userPrincipalName');
    Attr.Add('test_user@test.lan');
    KeyTabGenerator := PrepareKeyTab(Password, [ENCTYPE_AES128_CTS_HMAC_SHA1_96], EntryObject, 'test.lan');
    Check(Assigned(KeyTabGenerator), 'KeyTab has been created.');
    Check(Length(KeyTabGenerator.Entry) = 1, 'KeyTab contains one entry.');
    Check(KeyTabGenerator.Entry[0].EncType = ENCTYPE_AES128_CTS_HMAC_SHA1_96, 'KeyTab Entry[0] encryption type is ENCTYPE_AES128_CTS_HMAC_SHA1_96.');
    Check(KeyTabGenerator.Entry[0].Principal = 'test_user@TEST.LAN', 'KeyTab Entry[0] principal name is test_user@TEST.LAN.');
    Check(KeyTabGenerator.Entry[0].Key <> '', 'KeyTab Entry[0] key has been generated.');
  finally
    FreeAndNil(EntryObject);
  end;
end;

procedure TUnitTestGenerateKeyTab.PrepareKeyTab_Valid_Computer;
var
  Password: SpiUtf8;
  EntryObject: TLdapResult;
  Attr: TLdapAttribute;
  KeyTabGenerator: TKerberosKeyTabGenerator;
begin
  Password := TAesPrng.Main.RandomPassword(64);
  EntryObject := TLdapResult.Create;
  try
    Attr := EntryObject.FindOrAdd('objectClass');
    Attr.Add('top');
    Attr.Add('organizationalPerson');
    Attr.Add('person');
    Attr.Add('user');
    Attr.Add('computer');
    Attr := EntryObject.FindOrAdd('sAMAccountName');
    Attr.Add('test_computer$');
    Attr := EntryObject.FindOrAdd('servicePrincipalName');
    Attr.Add('HTTP/test_computer.test.lan');
    Attr.Add('host/test_computer.test.lan');
    Attr.Add('host/test_computer');
    KeyTabGenerator := PrepareKeyTab(Password, [ENCTYPE_AES128_CTS_HMAC_SHA1_96], EntryObject, 'test.lan');
    Check(Assigned(KeyTabGenerator), 'KeyTab has been created.');
    Check(Length(KeyTabGenerator.Entry) = 4, 'KeyTab contains 4 entries.');
    Check(KeyTabGenerator.Entry[0].EncType = ENCTYPE_AES128_CTS_HMAC_SHA1_96, 'KeyTab Entry[0] encryption type is ENCTYPE_AES128_CTS_HMAC_SHA1_96.');
    Check(KeyTabGenerator.Entry[0].Principal = 'test_computer$@TEST.LAN', 'KeyTab Entry[0] principal name is test_computer$@TEST.LAN.');
    Check(KeyTabGenerator.Entry[0].Key <> '', 'KeyTab Entry[0] key has been generated.');
    Check(KeyTabGenerator.Entry[1].EncType = ENCTYPE_AES128_CTS_HMAC_SHA1_96, 'KeyTab Entry[1] encryption type is ENCTYPE_AES128_CTS_HMAC_SHA1_96.');
    Check(KeyTabGenerator.Entry[1].Principal = 'HTTP/test_computer.test.lan@TEST.LAN', 'KeyTab Entry[1] principal name is HTTP/test_computer.test.lan@TEST.LAN.');
    Check(KeyTabGenerator.Entry[1].Key <> '', 'KeyTab Entry[1] key has been generated.');
    Check(KeyTabGenerator.Entry[2].EncType = ENCTYPE_AES128_CTS_HMAC_SHA1_96, 'KeyTab Entry[2] encryption type is ENCTYPE_AES128_CTS_HMAC_SHA1_96.');
    Check(KeyTabGenerator.Entry[2].Principal = 'host/test_computer.test.lan@TEST.LAN', 'KeyTab Entry[2] principal name is host/test_computer.test.lan@TEST.LAN.');
    Check(KeyTabGenerator.Entry[2].Key <> '', 'KeyTab Entry[2] key has been generated.');
    Check(KeyTabGenerator.Entry[3].EncType = ENCTYPE_AES128_CTS_HMAC_SHA1_96, 'KeyTab Entry[3] encryption type is ENCTYPE_AES128_CTS_HMAC_SHA1_96.');
    Check(KeyTabGenerator.Entry[3].Principal = 'host/test_computer@TEST.LAN', 'KeyTab Entry[3] principal name is host/test_computer@TEST.LAN.');
    Check(KeyTabGenerator.Entry[3].Key <> '', 'KeyTab Entry[3] key has been generated.');
  finally
    FreeAndNil(EntryObject);
  end;
end;

procedure TUnitTestGenerateKeyTab.PrepareKeyTab_EmptyPassword;
var
  EntryObject: TLdapResult;
  Attr: TLdapAttribute;
  KeyTabGenerator: TKerberosKeyTabGenerator;
begin
  EntryObject := TLdapResult.Create;
  try
    Attr := EntryObject.FindOrAdd('objectClass');
    Attr.Add('top');
    Attr.Add('organizationalPerson');
    Attr.Add('person');
    Attr.Add('user');
    Attr := EntryObject.FindOrAdd('sAMAccountName');
    Attr.Add('test_user');
    Attr := EntryObject.FindOrAdd('userPrincipalName');
    Attr.Add('test_user@test.lan');
    KeyTabGenerator := PrepareKeyTab('', [ENCTYPE_AES128_CTS_HMAC_SHA1_96], EntryObject, 'test.lan');
    Check(not Assigned(KeyTabGenerator), 'KeyTab cannot be created without password.');
  finally
    FreeAndNil(EntryObject);
  end;
end;

procedure TUnitTestGenerateKeyTab.PrepareKeyTab_EmptyEncryptionTypes;
var
  Password: SpiUtf8;
  EntryObject: TLdapResult;
  Attr: TLdapAttribute;
  KeyTabGenerator: TKerberosKeyTabGenerator;
begin
  Password := TAesPrng.Main.RandomPassword(64);
  EntryObject := TLdapResult.Create;
  try
    Attr := EntryObject.FindOrAdd('objectClass');
    Attr.Add('top');
    Attr.Add('organizationalPerson');
    Attr.Add('person');
    Attr.Add('user');
    Attr := EntryObject.FindOrAdd('sAMAccountName');
    Attr.Add('test_user');
    Attr := EntryObject.FindOrAdd('userPrincipalName');
    Attr.Add('test_user@test.lan');
    KeyTabGenerator := PrepareKeyTab(Password, [], EntryObject, 'test.lan');
    Check(not Assigned(KeyTabGenerator), 'KeyTab cannot be created without EncryptionType.');
  finally
    FreeAndNil(EntryObject);
  end;
end;

procedure TUnitTestGenerateKeyTab.PrepareKeyTab_EmptyEntryObject;
var
  Password: SpiUtf8;
  KeyTabGenerator: TKerberosKeyTabGenerator;
  EntryObject: TLdapResult;
  Attr: TLdapAttribute;
begin
  Password := TAesPrng.Main.RandomPassword(64);
  // No EntryObject
  KeyTabGenerator := PrepareKeyTab(Password, [ENCTYPE_AES128_CTS_HMAC_SHA1_96], nil, 'test.lan');
  Check(not Assigned(KeyTabGenerator), 'KeyTab cannot be created without EntryObject.');

  // Missing ObjectClass
  EntryObject := TLdapResult.Create;
  try
    Attr := EntryObject.FindOrAdd('sAMAccountName');
    Attr.Add('test_user');
    Attr := EntryObject.FindOrAdd('userPrincipalName');
    Attr.Add('test_user@test.lan');
    KeyTabGenerator := PrepareKeyTab(Password, [ENCTYPE_AES128_CTS_HMAC_SHA1_96], EntryObject, 'test.lan');
    Check(not Assigned(KeyTabGenerator), 'KeyTab cannot be created without objectClass in EntryObject.');
  finally
    FreeAndNil(EntryObject);
  end;

  // Missing sAMAccountName
  EntryObject := TLdapResult.Create;
  try
    Attr := EntryObject.FindOrAdd('objectClass');
    Attr.Add('top');
    Attr.Add('organizationalPerson');
    Attr.Add('person');
    Attr.Add('user');
    Attr := EntryObject.FindOrAdd('userPrincipalName');
    Attr.Add('test_user@test.lan');
    KeyTabGenerator := PrepareKeyTab(Password, [ENCTYPE_AES128_CTS_HMAC_SHA1_96], EntryObject, 'test.lan');
    Check(not Assigned(KeyTabGenerator), 'KeyTab cannot be created without sAMAccountName in EntryObject.');
  finally
    FreeAndNil(EntryObject);
  end;
end;

procedure TUnitTestGenerateKeyTab.PrepareKeyTab_EmptyRealm;
var
  Password: SpiUtf8;
  EntryObject: TLdapResult;
  Attr: TLdapAttribute;
  KeyTabGenerator: TKerberosKeyTabGenerator;
begin
  Password := TAesPrng.Main.RandomPassword(64);
  EntryObject := TLdapResult.Create;
  try
    Attr := EntryObject.FindOrAdd('objectClass');
    Attr.Add('top');
    Attr.Add('organizationalPerson');
    Attr.Add('person');
    Attr.Add('user');
    Attr := EntryObject.FindOrAdd('sAMAccountName');
    Attr.Add('test_user');
    Attr := EntryObject.FindOrAdd('userPrincipalName');
    Attr.Add('test_user@test.lan');
    KeyTabGenerator := PrepareKeyTab(Password, [ENCTYPE_AES128_CTS_HMAC_SHA1_96], EntryObject, '');
    Check(not Assigned(KeyTabGenerator), 'KeyTab cannot be created without realm.');
  finally
    FreeAndNil(EntryObject);
  end;
end;

end.

