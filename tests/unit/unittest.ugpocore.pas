unit unittest.ugpocore;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.test,
  mormot.core.base,
  mormot.core.unicode,
  mormot.net.ldap,
  ucommon,
  ugpocore,
  ugptcore,
  ugpregpol;

type

  { TUnitTestGPOCore }

  TUnitTestGPOCore = class(TSynTestCase)
  published
    procedure GPOPartsToVersionNumber_Zero;
    procedure GPOPartsToVersionNumber_UserOnly;
    procedure GPOPartsToVersionNumber_MachineOnly;
    procedure GPOPartsToVersionNumber_Full;
    procedure GPOVersionToParts_Full;
    procedure GPOFlagsToText_AllEnabled;
    procedure GPOFlagsToText_UserDisabled;
    procedure GPOFlagsToText_MachineDisabled;
    procedure GPOFlagsToText_AllDisabled;
    procedure GPOFlagsToText_Unknown;
    procedure GPOGetters_FromAttributes;
    procedure GPOGetters_EmptyAttributes;
    procedure GPOSetDisplayName_ExistingAttribute;
    procedure GPOSetDisplayName_MissingAttribute;
    procedure GPOSetDescription;
    procedure GPOSetWQLFilter;
    procedure GPOSetVersionNumber;
    procedure GetDomainNameFromDN_Valid;
    procedure GetDomainNameFromDN_Invalid;
    procedure GetDomainDNFromGPODN_Valid;
    procedure GetDomainDNFromGPODN_Invalid;
    procedure CountOccurrences_Valid;
    procedure CountOccurrences_EmptyPattern;
    procedure GPOExtensionsCount_Empty;
    procedure GPOExtensionsCount_TwoExtensions;
    procedure IsKnownGPOAttribute_Valid;
    procedure IsKnownGPOAttribute_Unknown;
    procedure GPOAttributeCatalog_NotEmpty;
    procedure ParseSmbListOutput_Valid;
    procedure ParseSmbListOutput_Empty;
    procedure ParseGptIni_Valid;
    procedure ParseGptIni_MissingFields;
    procedure ParseGptIni_Empty;
    procedure GptIniToText_Valid;
    procedure RegPol_LoadValid;
    procedure RegPol_LoadInvalidMagic;
    procedure RegPol_LoadTruncated;
    procedure RegPol_RoundTrip;
    procedure RegPol_EditAndSave;
    procedure RegPol_Utf16LeStrings;
  end;

implementation

{ TUnitTestGPOCore }

procedure TUnitTestGPOCore.GPOPartsToVersionNumber_Zero;
begin
  CheckEqual(GPOPartsToVersionNumber(0, 0), 0);
end;

procedure TUnitTestGPOCore.GPOPartsToVersionNumber_UserOnly;
begin
  CheckEqual(GPOPartsToVersionNumber(1, 0), 1);
  CheckEqual(GPOPartsToVersionNumber($FFFF, 0), $0000FFFF);
end;

procedure TUnitTestGPOCore.GPOPartsToVersionNumber_MachineOnly;
begin
  CheckEqual(GPOPartsToVersionNumber(0, 1), 1 shl GPO_MACHINE_VERSION_SHIFT);
  CheckEqual(GPOPartsToVersionNumber(0, $FFFF), $FFFF0000);
end;

procedure TUnitTestGPOCore.GPOPartsToVersionNumber_Full;
begin
  CheckEqual(GPOPartsToVersionNumber(1, 1), $00010001);
  CheckEqual(GPOPartsToVersionNumber($1234, $ABCD), $ABCD1234);
end;

procedure TUnitTestGPOCore.GPOVersionToParts_Full;
var
  UserVersion, MachineVersion: Word;
begin
  GPOVersionToParts($ABCD1234, UserVersion, MachineVersion);
  CheckEqual(UserVersion, $1234);
  CheckEqual(MachineVersion, $ABCD);
end;

procedure TUnitTestGPOCore.GPOFlagsToText_AllEnabled;
begin
  CheckEqual(GPOFlagsToText(GPO_FLAG_ALLENABLED), RawUtf8(rsGPOStatusAllEnabled));
end;

procedure TUnitTestGPOCore.GPOFlagsToText_UserDisabled;
begin
  CheckEqual(GPOFlagsToText(GPO_FLAG_USERDISABLED), RawUtf8(rsGPOStatusUserDisabled));
end;

procedure TUnitTestGPOCore.GPOFlagsToText_MachineDisabled;
begin
  CheckEqual(GPOFlagsToText(GPO_FLAG_MACHINEDISABLED), RawUtf8(rsGPOStatusMachineDisabled));
end;

procedure TUnitTestGPOCore.GPOFlagsToText_AllDisabled;
begin
  CheckEqual(GPOFlagsToText(GPO_FLAG_ALLDISABLED), RawUtf8(rsGPOStatusAllDisabled));
end;

procedure TUnitTestGPOCore.GPOFlagsToText_Unknown;
begin
  CheckEqual(GPOFlagsToText(42), RawUtf8(rsGPOStatusAllEnabled));
end;

procedure TUnitTestGPOCore.GPOGetters_FromAttributes;
var
  Attributes: TLdapAttributeList;
  GPO: TGPO;
begin
  Attributes := TLdapAttributeList.Create;
  try
    Attributes.Add('cn', '{31B2F340-016D-11D2-945F-00C04FB984F9}');
    Attributes.Add(GPO_ATTR_DISPLAYNAME, 'Default Domain Policy');
    Attributes.Add(GPO_ATTR_DESCRIPTION, 'Default Domain Policy Description');
    Attributes.Add(GPO_ATTR_FILESYSPATH,
      '\\example.com\SysVol\example.com\Policies\{31B2F340-016D-11D2-945F-00C04FB984F9}');
    Attributes.Add(GPO_ATTR_VERSIONNUMBER, '65537');
    Attributes.Add(GPO_ATTR_FUNCTIONALITYVERSION, '2');
    Attributes.Add(GPO_ATTR_FLAGS, '0');
    Attributes.Add(GPO_ATTR_WQLFILTER, 'CN=MyFilter,CN=SOM,CN=WMIPolicy,CN=System');
    Attributes.Add(GPO_ATTR_USEREXTENSIONNAMES,
      '[{D02B1F72-3407-48AE-BA88-E8213C6761F1}{40B66650-4972-11D0-AABD-00AA006C2EBB}]');
    Attributes.Add(GPO_ATTR_MACHINEEXTENSIONNAMES,
      '[{35378EAC-683F-11D2-A89A-00C04FBBCFA2}{D02B1F73-3407-48AE-BA88-E8213C6761F1}]');
    Attributes.Add(GPO_ATTR_WHENCREATED, '20260101000000.0Z');
    Attributes.Add(GPO_ATTR_WHENCHANGED, '20260201000000.0Z');

    GPO := TGPO.Create(Attributes);
    try
      CheckEqual(GPO.Name, '{31B2F340-016D-11D2-945F-00C04FB984F9}');
      CheckEqual(GPO.DisplayName, 'Default Domain Policy');
      CheckEqual(GPO.Description, 'Default Domain Policy Description');
      CheckEqual(GPO.FileSysPath,
        '\\example.com\SysVol\example.com\Policies\{31B2F340-016D-11D2-945F-00C04FB984F9}');
      CheckEqual(GPO.VersionNumber, 65537);
      CheckEqual(GPO.UserVersion, 1);
      CheckEqual(GPO.MachineVersion, 1);
      CheckEqual(GPO.FunctionalityVersion, 2);
      CheckEqual(GPO.Flags, GPO_FLAG_ALLENABLED);
      CheckEqual(GPO.WQLFilter, 'CN=MyFilter,CN=SOM,CN=WMIPolicy,CN=System');
      CheckEqual(GPO.UserExtensionNames,
        '[{D02B1F72-3407-48AE-BA88-E8213C6761F1}{40B66650-4972-11D0-AABD-00AA006C2EBB}]');
      CheckEqual(GPO.MachineExtensionNames,
        '[{35378EAC-683F-11D2-A89A-00C04FBBCFA2}{D02B1F73-3407-48AE-BA88-E8213C6761F1}]');
      CheckEqual(GPO.WhenCreated, '2026-01-01');
      CheckEqual(GPO.WhenChanged, '2026-02-01');
    finally
      GPO.Free;
    end;
  finally
    Attributes.Free;
  end;
end;

procedure TUnitTestGPOCore.GPOGetters_EmptyAttributes;
var
  GPO: TGPO;
begin
  GPO := TGPO.Create;
  try
    CheckEqual(GPO.Name, '');
    CheckEqual(GPO.DisplayName, '');
    CheckEqual(GPO.Description, '');
    CheckEqual(GPO.FileSysPath, '');
    CheckEqual(GPO.VersionNumber, 0);
    CheckEqual(GPO.UserVersion, 0);
    CheckEqual(GPO.MachineVersion, 0);
    CheckEqual(GPO.FunctionalityVersion, 0);
    CheckEqual(GPO.Flags, GPO_FLAG_ALLENABLED);
    CheckEqual(GPO.WQLFilter, '');
    CheckEqual(GPO.UserExtensionNames, '');
    CheckEqual(GPO.MachineExtensionNames, '');
    CheckEqual(GPO.WhenCreated, '');
    CheckEqual(GPO.WhenChanged, '');
  finally
    GPO.Free;
  end;
end;

procedure TUnitTestGPOCore.GPOSetDisplayName_ExistingAttribute;
var
  GPO: TGPO;
begin
  GPO := TGPO.Create;
  try
    GPO.Attributes.Add(GPO_ATTR_DISPLAYNAME, 'Old Name');
    GPO.DisplayName := 'New Name';

    CheckEqual(GPO.DisplayName, 'New Name');
    CheckEqual(GPO.Attributes.Find(GPO_ATTR_DISPLAYNAME).Count, 1);
  finally
    GPO.Free;
  end;
end;

procedure TUnitTestGPOCore.GPOSetDisplayName_MissingAttribute;
var
  GPO: TGPO;
begin
  GPO := TGPO.Create;
  try
    GPO.DisplayName := 'Brand New';

    CheckEqual(GPO.DisplayName, 'Brand New');
  finally
    GPO.Free;
  end;
end;

procedure TUnitTestGPOCore.GPOSetDescription;
var
  GPO: TGPO;
begin
  GPO := TGPO.Create;
  try
    GPO.Attributes.Add(GPO_ATTR_DESCRIPTION, 'Old Description');
    GPO.Description := 'New Description';

    CheckEqual(GPO.Description, 'New Description');
    CheckEqual(GPO.Attributes.Find(GPO_ATTR_DESCRIPTION).Count, 1);
  finally
    GPO.Free;
  end;
end;

procedure TUnitTestGPOCore.GPOSetWQLFilter;
var
  GPO: TGPO;
begin
  GPO := TGPO.Create;
  try
    GPO.WQLFilter := 'CN=MyFilter,CN=SOM,CN=WMIPolicy,CN=System';

    CheckEqual(GPO.WQLFilter, 'CN=MyFilter,CN=SOM,CN=WMIPolicy,CN=System');
    CheckEqual(GPO.Attributes.Find(GPO_ATTR_WQLFILTER).Count, 1);

    GPO.WQLFilter := '';
    CheckEqual(GPO.WQLFilter, '');
  finally
    GPO.Free;
  end;
end;

procedure TUnitTestGPOCore.GPOSetVersionNumber;
var
  GPO: TGPO;
begin
  GPO := TGPO.Create;
  try
    GPO.VersionNumber := GPOPartsToVersionNumber(3, 7);

    CheckEqual(GPO.VersionNumber, $00070003);
    CheckEqual(GPO.UserVersion, 3);
    CheckEqual(GPO.MachineVersion, 7);
  finally
    GPO.Free;
  end;
end;

procedure TUnitTestGPOCore.GetDomainNameFromDN_Valid;
begin
  CheckEqual(GetDomainNameFromDN('DC=openrsat,DC=lan'), 'openrsat.lan');
  CheckEqual(GetDomainNameFromDN('OU=Test,DC=example,DC=com'), 'example.com');
end;

procedure TUnitTestGPOCore.GetDomainNameFromDN_Invalid;
begin
  CheckEqual(GetDomainNameFromDN(''), '');
  CheckEqual(GetDomainNameFromDN('CN=Test'), '');
end;

procedure TUnitTestGPOCore.GetDomainDNFromGPODN_Valid;
begin
  CheckEqual(GetDomainDNFromGPODN(
    'cn={31B2F340-016D-11D2-945F-00C04FB984F9},CN=Policies,CN=System,DC=openrsat,DC=lan'),
    'DC=openrsat,DC=lan');
end;

procedure TUnitTestGPOCore.GetDomainDNFromGPODN_Invalid;
begin
  CheckEqual(GetDomainDNFromGPODN(''), '');
  CheckEqual(GetDomainDNFromGPODN('CN=Policies,CN=System'), '');
end;

procedure TUnitTestGPOCore.CountOccurrences_Valid;
begin
  CheckEqual(CountOccurrences('ababab', 'ab'), 3);
  CheckEqual(CountOccurrences('aaa', 'aa'), 1);
  CheckEqual(CountOccurrences('hello', 'x'), 0);
end;

procedure TUnitTestGPOCore.CountOccurrences_EmptyPattern;
begin
  CheckEqual(CountOccurrences('hello', ''), 0);
end;

procedure TUnitTestGPOCore.GPOExtensionsCount_Empty;
begin
  CheckEqual(GPOExtensionsCount(''), 0);
end;

procedure TUnitTestGPOCore.GPOExtensionsCount_TwoExtensions;
begin
  CheckEqual(GPOExtensionsCount('[{A}{B}]'), 2);
  CheckEqual(GPOExtensionsCount('[{D02B1F72-3407-48AE-BA88-E8213C6761F1}' +
    '{40B66650-4972-11D0-AABD-00AA006C2EBB}]'), 2);
end;

procedure TUnitTestGPOCore.IsKnownGPOAttribute_Valid;
begin
  Check(IsKnownGPOAttribute(GPO_ATTR_DISPLAYNAME));
  Check(IsKnownGPOAttribute('gPCWQLFilter'));
  Check(IsKnownGPOAttribute('flags'));
end;

procedure TUnitTestGPOCore.IsKnownGPOAttribute_Unknown;
begin
  Check(not IsKnownGPOAttribute('unknownAttribute'));
  Check(not IsKnownGPOAttribute(''));
end;

procedure TUnitTestGPOCore.GPOAttributeCatalog_NotEmpty;
var
  Catalog: TGPOCatalogItemDynArray;
  i: Integer;
begin
  Catalog := GPOAttributeCatalog;

  Check(Length(Catalog) > 0, 'Catalog should not be empty');
  for i := 0 to High(Catalog) do
  begin
    Check(Catalog[i].AttributeName <> '', 'Catalog item name should not be empty');
    Check(Catalog[i].Description <> '', 'Catalog item description should not be empty');
  end;
end;

procedure TUnitTestGPOCore.ParseSmbListOutput_Valid;
var
  Files: TGPTFileInfoDynArray;
  Output: RawUtf8;
begin
  Output :=
    '  .                                   D        0  Sat Aug 29 10:00:00 2026' + #10 +
    '  ..                                  D        0  Sat Aug 29 10:00:00 2026' + #10 +
    '  GPT.INI                             A      204  Sat Aug 29 10:00:00 2026' + #10 +
    '  User                                D        0  Sat Aug 29 10:00:00 2026' + #10 +
    '  User\Registry.pol                   A    12345  Mon Sep 1 08:30:00 2026' + #10 +
    '  Machine\Registry.pol                A     9999  Tue Sep 2 09:00:00 2026';

  Files := ParseSmbListOutput(Output);

  CheckEqual(Length(Files), 4);
  if (Length(Files) < 4) then
    Exit;
  CheckEqual(Files[0].Path, 'GPT.INI');
  CheckEqual(Files[0].Size, 204);
  CheckEqual(Files[1].Path, 'User');
  CheckEqual(Files[1].Size, 0);
  CheckEqual(Files[2].Path, 'User\Registry.pol');
  CheckEqual(Files[2].Size, 12345);
  CheckEqual(Files[3].Path, 'Machine\Registry.pol');
  CheckEqual(Files[3].Size, 9999);
end;

procedure TUnitTestGPOCore.ParseSmbListOutput_Empty;
var
  Files: TGPTFileInfoDynArray;
begin
  Files := ParseSmbListOutput('');
  CheckEqual(Length(Files), 0);

  Files := ParseSmbListOutput('smbclient: no share name' + #10 + 'Usage: smbclient ...');
  CheckEqual(Length(Files), 0);
end;

procedure TUnitTestGPOCore.ParseGptIni_Valid;
var
  Ini: TGptIni;
begin
  Ini := ParseGptIni('[General]' + #13#10 +
    'Version=65537' + #13#10 +
    'DisplayName=Default Domain Policy' + #13#10 +
    'Options=0' + #13#10);

  CheckEqual(Ini.Version, 65537);
  CheckEqual(Ini.DisplayName, 'Default Domain Policy');
  CheckEqual(Ini.Options, 0);
  Check(Ini.HasVersion);
  Check(Ini.HasDisplayName);
  Check(Ini.HasOptions);
end;

procedure TUnitTestGPOCore.ParseGptIni_MissingFields;
var
  Ini: TGptIni;
begin
  Ini := ParseGptIni('[General]' + #10 + 'Version=2' + #10);

  CheckEqual(Ini.Version, 2);
  CheckEqual(Ini.DisplayName, '');
  CheckEqual(Ini.Options, 0);
  Check(Ini.HasVersion);
  Check(not Ini.HasDisplayName);
  Check(not Ini.HasOptions);
end;

procedure TUnitTestGPOCore.ParseGptIni_Empty;
var
  Ini: TGptIni;
begin
  Ini := ParseGptIni('');

  CheckEqual(Ini.Version, 0);
  Check(not Ini.HasVersion);
  Check(not Ini.HasDisplayName);
  Check(not Ini.HasOptions);
end;

procedure TUnitTestGPOCore.GptIniToText_Valid;
var
  Ini: TGptIni;
  TextValue: RawUtf8;
begin
  Ini.Version := 65537;
  Ini.DisplayName := 'Test GPO';
  Ini.Options := 2;
  Ini.HasVersion := True;
  Ini.HasDisplayName := True;
  Ini.HasOptions := True;

  TextValue := GptIniToText(Ini);
  CheckEqual(TextValue, '[General]' + #13#10 +
    'Version=65537' + #13#10 +
    'DisplayName=Test GPO' + #13#10 +
    'Options=2' + #13#10);

  // Round trip.
  Ini := ParseGptIni(TextValue);
  CheckEqual(Ini.Version, 65537);
  CheckEqual(Ini.DisplayName, 'Test GPO');
  CheckEqual(Ini.Options, 2);
end;

function Utf16(const S: RawUtf8): RawByteString;
var
  W: SynUnicode;
begin
  W := Utf8ToSynUnicode(S);
  SetLength(result, Length(W) * 2);
  if (Length(W) > 0) then
    Move(W[1], result[1], Length(W) * 2);
end;

function BuildSampleRegPol: RawByteString;
begin
  // 24-byte REGF header: PRegf, 0, 1, 1, flags, reserved, machine=0, reserved.
  // Nested key records, as written by the GPMC:
  //   Software (size 95 = 0x5F)
  //     Policies (size 71 = 0x47)
  //       Test (size 47 = 0x2F)
  //         Enabled = REG_DWORD 1 (value record, size 31 = 0x1F)
  result := 'PRegf' + #0 + #1 + #1 + #0#0 + #0#0 + #0#0#0#0 +
    #0#0#0#0 + #0#0#0#0 +
    // Key "Software": 3 + 18 + 71 + 3 = 95
    #$6C + #$5F#$00 + Utf16('Software') + #0#0 +
    // Key "Policies": 3 + 18 + 47 + 3 = 71
    #$6C + #$47#$00 + Utf16('Policies') + #0#0 +
    // Key "Test": 3 + 10 + 31 + 3 = 47
    #$6C + #$2F#$00 + Utf16('Test') + #0#0 +
    // Value "Enabled" = REG_DWORD 1: 3 + 16 + 4 + 4 + 4 = 31
    #$76 + #$1F#$00 + Utf16('Enabled') + #0#0 +
    #$04#$00#$00#$00 + #$04#$00#$00#$00 + #$01#$00#$00#$00 +
    // END records
    #$00#$03#$00 + #$00#$03#$00 + #$00#$03#$00;
end;

procedure TUnitTestGPOCore.RegPol_LoadValid;
var
  Pol: TGPRegPol;
  Key: TGPRegPolKey;
begin
  Pol := TGPRegPol.LoadFromBytes(BuildSampleRegPol);
  try
    Check(not Pol.Machine);

    Key := Pol.FindKey('Software\Policies\Test');
    Check(Assigned(Key), 'Key should be found');
    if not Assigned(Key) then
      Exit;

    Check(Assigned(Key.GetValue('Enabled')));
    CheckEqual(Key.GetValue('Enabled').ValueType, REG_DWORD);
    CheckEqual(Key.GetValue('Enabled').AsDWord, 1);
  finally
    Pol.Free;
  end;
end;

procedure TUnitTestGPOCore.RegPol_LoadInvalidMagic;
var
  RaisedException: Boolean;
begin
  RaisedException := False;
  try
    TGPRegPol.LoadFromBytes('XXXX' + StringOfChar(#0, 40)).Free;
  except
    on E: EGPOException do
      RaisedException := True;
  end;
  Check(RaisedException, 'Invalid signature should raise EGPOException');
end;

procedure TUnitTestGPOCore.RegPol_LoadTruncated;
var
  RaisedException: Boolean;
begin
  RaisedException := False;
  try
    TGPRegPol.LoadFromBytes('PRegf' + #0 + #1).Free;
  except
    on E: EGPOException do
      RaisedException := True;
  end;
  Check(RaisedException, 'Truncated file should raise EGPOException');
end;

procedure TUnitTestGPOCore.RegPol_RoundTrip;
var
  Pol: TGPRegPol;
  Bytes: RawByteString;
begin
  Pol := TGPRegPol.LoadFromBytes(BuildSampleRegPol);
  try
    Bytes := Pol.SaveToBytes;
  finally
    Pol.Free;
  end;

  // The serialized file must parse back to the same content.
  Pol := TGPRegPol.LoadFromBytes(Bytes);
  try
    CheckEqual(Pol.FindKey('Software\Policies\Test').GetValue('Enabled').AsDWord, 1);
    CheckEqual(Pol.FindKey('Software\Policies\Test').GetValue('Enabled').ValueType,
      REG_DWORD);
  finally
    Pol.Free;
  end;
end;

procedure TUnitTestGPOCore.RegPol_EditAndSave;
var
  Pol: TGPRegPol;
  Bytes: RawByteString;
  Key: TGPRegPolKey;
begin
  Pol := TGPRegPol.LoadFromBytes(BuildSampleRegPol);
  try
    Key := Pol.AddKey('Software\Policies\Test\Sub');
    Key.SetDWordValue('Enabled', 2);
    Key.SetStringValue('Name', 'Hello');

    Pol.AddKey('Software\Policies\New').SetStringValue('Text', 'World');

    Pol.Machine := True;
    Bytes := Pol.SaveToBytes;
  finally
    Pol.Free;
  end;

  Pol := TGPRegPol.LoadFromBytes(Bytes);
  try
    Check(Pol.Machine, 'Machine flag should be saved');

    Key := Pol.FindKey('Software\Policies\Test\Sub');
    Check(Assigned(Key), 'Sub key should be found');
    CheckEqual(Key.GetValue('Enabled').AsDWord, 2);
    CheckEqual(Key.GetValue('Name').AsString, 'Hello');

    Key := Pol.FindKey('Software\Policies\New');
    Check(Assigned(Key), 'New key should be found');
    CheckEqual(Key.GetValue('Text').AsString, 'World');

    // The original key must still be there.
    Key := Pol.FindKey('Software\Policies\Test');
    Check(Assigned(Key));
    CheckEqual(Key.GetValue('Enabled').AsDWord, 1);
  finally
    Pol.Free;
  end;
end;

procedure TUnitTestGPOCore.RegPol_Utf16LeStrings;
var
  OLA_UTF8: RawUtf8;
  OLA_UNICODE: SynUnicode;
  Pol: TGPRegPol;
  Key: TGPRegPolKey;
  Bytes: RawByteString;
begin
  // 'Olá' built from code units (keeps the test encoding-independent:
  // string literals with chars above #$7F may be re-encoded by the compiler).
  OLA_UNICODE := 'Ol';
  SetLength(OLA_UNICODE, 3);
  OLA_UNICODE[3] := WideChar($00E1);
  OLA_UTF8 := SynUnicodeToUtf8(OLA_UNICODE);
  // Windows-written Registry.pol: string values are stored as UTF-16LE.
  //   Key "Software" (size 98 = 0x62)
  //     Value "Name"  = REG_SZ 'Olá'       (size 29 = 0x1D)
  //     Value "Paths" = REG_MULTI_SZ       (size 45 = 0x2D)
  Bytes := 'PRegf' + #0 + #1 + #1 + #0#0 + #0#0 + #0#0#0#0 +
    #0#0#0#0 + #0#0#0#0 +
    #$6C + #$62#$00 + Utf16('Software') + #0#0 +
    #$76 + #$1D#$00 + Utf16('Name') + #0#0 +
    #$01#$00#$00#$00 + #$08#$00#$00#$00 +
    'O'#0'l'#0 + AnsiChar($E1) + #0 + #0#0 +
    #$76 + #$2D#$00 + Utf16('Paths') + #0#0 +
    #$07#$00#$00#$00 + #$16#$00#$00#$00 +
    Utf16('C:\a') + #0#0 + Utf16('D:\b') + #0#0#0#0 +
    #$00#$03#$00;

  Pol := TGPRegPol.LoadFromBytes(Bytes);
  try
    Key := Pol.FindKey('Software');
    Check(Assigned(Key), 'Key should be found');
    if not Assigned(Key) then
      Exit;

    CheckEqual(Key.GetValue('Name').AsString, OLA_UTF8);
    CheckEqual(Key.GetValue('Paths').AsString, 'C:\a' + #10 + 'D:\b');
  finally
    Pol.Free;
  end;

  // Round trip: strings are stored back as UTF-16LE and read again.
  Pol := TGPRegPol.Create;
  try
    Key := Pol.AddKey('Software');
    Key.SetStringValue('Name', OLA_UTF8);
    Key.SetValueData('Paths', REG_MULTI_SZ,
      Utf16('C:\a') + #0#0 + Utf16('D:\b') + #0#0#0#0);
    Bytes := Pol.SaveToBytes;
  finally
    Pol.Free;
  end;

  Pol := TGPRegPol.LoadFromBytes(Bytes);
  try
    Key := Pol.FindKey('Software');
    Check(Assigned(Key));
    if not Assigned(Key) then
      Exit;

    CheckEqual(Key.GetValue('Name').AsString, OLA_UTF8);
    CheckEqual(Key.GetValue('Paths').AsString, 'C:\a' + #10 + 'D:\b');
  finally
    Pol.Free;
  end;
end;

end.
