unit ufrmpropertyaccount;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  Buttons,
  DateTimePicker,
  SysUtils,
  Forms,
  Controls,
  ExtCtrls,
  StdCtrls, ActnList,
  mormot.core.base,
  mormot.core.log,
  mormot.net.ldap,
  uproperty,
  upropertyframe;

type

  { TFrmPropertyAccount }

  TFrmPropertyAccount = class(TPropertyFrame)
    Action_LogonHours: TAction;
    Action_LogonTo: TAction;
    ActionList1: TActionList;
    Btn_LogonHours: TBitBtn;
    Btn_LogOnTo: TBitBtn;
    CheckBox_CannotChange: TCheckBox;
    CheckBox_Disabled: TCheckBox;
    CheckBox_KerberosAES128Encryption: TCheckBox;
    CheckBox_KerberosAES256Encryption: TCheckBox;
    CheckBox_KerberosDESEncryption: TCheckBox;
    CheckBox_MustChange: TCheckBox;
    CheckBox_NeverExpires: TCheckBox;
    CheckBox_NoKerberosPreauth: TCheckBox;
    CheckBox_ReversibleEncryption: TCheckBox;
    CheckBox_Sensitive: TCheckBox;
    CheckBox_SmartCard: TCheckBox;
    CheckBox_Unlock: TCheckBox;
    ComboBox_Domain: TComboBox;
    DateTimePicker_Expires: TDateTimePicker;
    Edit_Name: TEdit;
    Edit_SAMAccountName: TEdit;
    Edit_SAMDomain: TEdit;
    GoupBox_Expires: TGroupBox;
    Label_Options: TLabel;
    Label_UserLogonName: TLabel;
    Label_UserSAMAccountName: TLabel;
    Panel_LogonTime: TPanel;
    Panel_LogonName: TPanel;
    Panel_SAMAccountName: TPanel;
    RadioButton_EndOf: TRadioButton;
    RadioButton_Never: TRadioButton;
    ScrollBox_Options: TScrollBox;
    procedure Action_LogonHoursExecute(Sender: TObject);
    procedure Action_LogonToExecute(Sender: TObject);
    procedure CheckBox_CannotChangeChange(Sender: TObject);
    procedure CheckBox_DisabledChange(Sender: TObject);
    procedure CheckBox_KerberosAES128EncryptionChange(Sender: TObject);
    procedure CheckBox_KerberosAES256EncryptionChange(Sender: TObject);
    procedure CheckBox_KerberosDESEncryptionChange(Sender: TObject);
    procedure CheckBox_MustChangeChange(Sender: TObject);
    procedure CheckBox_NeverExpiresChange(Sender: TObject);
    procedure CheckBox_NoKerberosPreauthChange(Sender: TObject);
    procedure CheckBox_ReversibleEncryptionChange(Sender: TObject);
    procedure CheckBox_SensitiveChange(Sender: TObject);
    procedure CheckBox_SmartCardChange(Sender: TObject);
    procedure CheckBox_UnlockChange(Sender: TObject);
    procedure ComboBox_DomainChange(Sender: TObject);
    procedure DateTimePicker_ExpiresChange(Sender: TObject);
    procedure Edit_NameChange(Sender: TObject);
    procedure Edit_SAMAccountNameChange(Sender: TObject);
    procedure RadioButton_EndOfChange(Sender: TObject);
    procedure RadioButton_NeverChange(Sender: TObject);
  private
    fLog: TSynLog;
    fProperty: TProperty;

    procedure UpdateLogonName;
    procedure UpdateSamaccountName;
    procedure UpdateOptions;
    procedure UpdateExpires;

    procedure UserAccountControlUpdate(UserAccountControl: TUserAccountControl; Include: Boolean);
    procedure msDSSupportedEncryptionTypeUpdate(
      msDSSupportedEncryptionType: TMsdsSupportedEncryptionType;
  Include: Boolean);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Update(Props: TProperty); override;
  end;

implementation
uses
  mormot.core.os.security,
  mormot.core.text,
  ucommon,
  uhelpers,
  uvislogonhours,
  uvislogonworkstation;

{$R *.lfm}

{ TFrmPropertyAccount }

procedure TFrmPropertyAccount.Edit_NameChange(Sender: TObject);
begin
  fProperty.Add('userPrincipalName', FormatUtf8('%%', [Edit_Name.Text, ComboBox_Domain.Text]));
end;

procedure TFrmPropertyAccount.Edit_SAMAccountNameChange(Sender: TObject);
begin
  fProperty.sAMAccountName := Edit_SAMAccountName.Text;
end;

procedure TFrmPropertyAccount.RadioButton_EndOfChange(Sender: TObject);
begin
  if not RadioButton_EndOf.Checked then
    Exit;
  DateTimePicker_Expires.Enabled := True;
  fProperty.Add('accountExpires', IntToStr(DateTimeToMSTime(DateTimePicker_Expires.DateTime)));
end;

procedure TFrmPropertyAccount.RadioButton_NeverChange(Sender: TObject);
begin
  if not RadioButton_Never.Checked then
    Exit;
  DateTimePicker_Expires.Enabled := False;
  fProperty.Add('accountExpires', '0');
end;

procedure TFrmPropertyAccount.ComboBox_DomainChange(Sender: TObject);
begin
  fProperty.Add('userPrincipalName', FormatUtf8('%%', [Edit_Name.Text, ComboBox_Domain.Text]));
end;

procedure TFrmPropertyAccount.DateTimePicker_ExpiresChange(Sender: TObject);
begin
  if not RadioButton_EndOf.Checked then
    Exit;

  fProperty.Add('accountExpires', IntToStr(DateTimeToMSTime(DateTimePicker_Expires.DateTime)));
end;

procedure TFrmPropertyAccount.Action_LogonHoursExecute(Sender: TObject);
var
  default: RawByteString = '';
  LogonHours: TVisLogonHours;
  Hours: RawByteString = '';
begin
  // Get Latest version
  SetLength(default, 21);  // 21bytes = (7days * 24hours) / 8bits
  FillByte(default[1], 21, $00);

  Hours := fProperty.GetRaw('logonHours');
  if Hours = '' then
    Hours := default;

  // LogonHours Form
  LogonHours := TVisLogonHours.Create(self, @Hours);
  try
    if LogonHours.ShowModal() <> mrOK then
      Exit;
  finally
    FreeAndNil(LogonHours);
  end;

  fProperty.Add('logonHours', Hours);
end;

procedure TFrmPropertyAccount.Action_LogonToExecute(Sender: TObject);
var
  LogonWorkstation: TVisLogonWorkstation;
  Workstations: String = '';
begin
  Workstations := fProperty.GetRaw('userWorkstations');

  // LogonWorkstation Form
  LogonWorkstation := TVisLogonWorkstation.Create(self, @Workstations);
  try
    if LogonWorkstation.ShowModal() <> mrOK then
      Exit;
  finally
    FreeAndNil(LogonWorkstation);
  end;

  fProperty.Add('userWorkstations', Workstations);
end;

procedure TFrmPropertyAccount.CheckBox_CannotChangeChange(Sender: TObject);
const
  SECACETYPE: Array[Boolean] of TSecAceType = (
    satObjectAccessAllowed,
    satObjectAccessDenied
  );
var
  PSecDesc: PSecurityDescriptor;
  AceSelf, AceWorld: PSecAce;
  ModSecAceType: TSecAceType;
begin
  // https://learn.microsoft.com/en-us/windows/win32/adsi/modifying-user-cannot-change-password-ldap-provider

  // Retrieve Security Descriptor
  PSecDesc := fProperty.SecurityDescriptor;
  if not Assigned(PSecDesc) then
    Exit;

  // Get ACE Self and World with previous AceType
  ModSecAceType := SECACETYPE[not CheckBox_CannotChange.Checked];
  AceSelf := SecDescAddOrUpdateACE(PSecDesc, ATTR_UUID[kaUserChangePassword],
    KnownRawSid(wksSelf), ModSecAceType, [samControlAccess]);
  AceWorld := SecDescAddOrUpdateACE(PSecDesc, ATTR_UUID[kaUserChangePassword],
    KnownRawSid(wksWorld), ModSecAceType, [samControlAccess]);

  // Modify ACE Self and World with new AceType
  ModSecAceType := SECACETYPE[CheckBox_CannotChange.Checked];
  AceSelf^.AceType  := ModSecAceType;
  AceWorld^.AceType := ModSecAceType;
  OrderAcl(fProperty.LdapClient, fProperty.DistinguishedName, fProperty.LdapClient.DefaultDN(), @PSecDesc^.Dacl);
  fProperty.SecurityDescriptor := PSecDesc;
end;

procedure TFrmPropertyAccount.CheckBox_DisabledChange(Sender: TObject);
begin
  UserAccountControlUpdate(uacAccountDisable, CheckBox_Disabled.Checked);
end;

procedure TFrmPropertyAccount.CheckBox_KerberosAES128EncryptionChange(
  Sender: TObject);
begin
  msDSSupportedEncryptionTypeUpdate(metAes128CtsHmacSha1, CheckBox_KerberosAES128Encryption.Checked);
end;

procedure TFrmPropertyAccount.CheckBox_KerberosAES256EncryptionChange(
  Sender: TObject);
begin
  msDSSupportedEncryptionTypeUpdate(metAes256CtsHmacSha1, CheckBox_KerberosAES256Encryption.Checked);
end;

procedure TFrmPropertyAccount.CheckBox_KerberosDESEncryptionChange(
  Sender: TObject);
begin
  UserAccountControlUpdate(uacKerberosDesOnly, CheckBox_KerberosDESEncryption.Checked);
end;

procedure TFrmPropertyAccount.CheckBox_MustChangeChange(Sender: TObject);
begin
  if CheckBox_MustChange.Checked then
    fProperty.Add('pwdLastSet', '0')
  else
    fProperty.Add('pwdLastSet', '-1');
end;

procedure TFrmPropertyAccount.CheckBox_NeverExpiresChange(Sender: TObject);
begin
  UserAccountControlUpdate(uacPasswordDoNotExpire, CheckBox_NeverExpires.Checked);
end;

procedure TFrmPropertyAccount.CheckBox_NoKerberosPreauthChange(Sender: TObject);
begin
  UserAccountControlUpdate(uacKerberosRequirePreAuth, CheckBox_NoKerberosPreauth.Checked);
end;

procedure TFrmPropertyAccount.CheckBox_ReversibleEncryptionChange(
  Sender: TObject);
begin
  UserAccountControlUpdate(uacPasswordUnencrypted, CheckBox_ReversibleEncryption.Checked);
end;

procedure TFrmPropertyAccount.CheckBox_SensitiveChange(Sender: TObject);
begin
  UserAccountControlUpdate(uacKerberosNotDelegated, CheckBox_Sensitive.Checked);
end;

procedure TFrmPropertyAccount.CheckBox_SmartCardChange(Sender: TObject);
begin
  UserAccountControlUpdate(uacSmartcardRequired, CheckBox_SmartCard.Checked);
end;

procedure TFrmPropertyAccount.CheckBox_UnlockChange(Sender: TObject);
begin
  if CheckBox_Unlock.Checked then
    fProperty.Add('lockoutTime', '0')
  else
    fProperty.Restore('lockoutTime');
end;

procedure TFrmPropertyAccount.UpdateLogonName;
var
  UserPrincipalName: RawUtf8;
  Splitted: TStringArray;
  i: Integer;
  UPNSuffixes: TRawUtf8DynArray;
begin
  // Default value
  Edit_Name.CaptionNoChange := '';
  ComboBox_Domain.ItemIndex := -1;

  // Fill UPN Suffixes
  ComboBox_Domain.Items.BeginUpdate;
  try
    UPNSuffixes := fProperty.UPNSuffixes;
    for i := 0 to High(UPNSuffixes) do
      ComboBox_Domain.Items.Add(FormatUtf8('@%', [UPNSuffixes[i]]));
  finally
    ComboBox_Domain.Items.EndUpdate;
  end;

  // Get userPrincipalName
  UserPrincipalName := fProperty.GetReadable('userPrincipalName');
  if UserPrincipalName = '' then
    Exit;

  // Split userPrincipalName
  Splitted := String(UserPrincipalName).Split('@');

  if Length(Splitted) <> 2 then
    Exit;

  // Fill values
  ComboBox_Domain.ItemIndex := ComboBox_Domain.Items.IndexOf(FormatUtf8('@%', [Splitted[1]]));
  Edit_Name.CaptionNoChange := Splitted[0];
end;

procedure TFrmPropertyAccount.UpdateSamaccountName;
begin
  Edit_SAMAccountName.CaptionNoChange := fProperty.sAMAccountName;
  Edit_SAMDomain.CaptionNoChange := FormatUtf8('%\', [fProperty.LdapClient.NetbiosDN]);
end;

procedure TFrmPropertyAccount.UpdateOptions;
const
  AES128 = $08;
  AES256 = $10;
var
  UserAccountControl: TUserAccountControls;
  pwdLastSet: RawUtf8;
  MsdsSupportedEncryptionTypes: TMsdsSupportedEncryptionTypes;
begin
  pwdLastSet := fProperty.GetReadable('pwdLastSet');
  CheckBox_MustChange.CheckedNoChange := (pwdLastSet = '') or (pwdLastSet = '0');

  // https://ldapwiki.com/wiki/Wiki.jsp?page=User-Account-Control%20Attribute
  UserAccountControl := UserAccountControlsFromText(fProperty.GetReadable('userAccountControl'));

  CheckBox_CannotChange.CheckedNoChange := uacPasswordCannotChange in UserAccountControl;
  CheckBox_NeverExpires.CheckedNoChange := uacPasswordDoNotExpire in UserAccountControl;
  CheckBox_ReversibleEncryption.CheckedNoChange := uacPasswordUnencrypted in UserAccountControl;
  CheckBox_Disabled.CheckedNoChange := uacAccountDisable in UserAccountControl;
  CheckBox_SmartCard.CheckedNoChange := uacSmartcardRequired in UserAccountControl;
  CheckBox_Sensitive.CheckedNoChange := uacKerberosNotDelegated in UserAccountControl;
  CheckBox_KerberosDESEncryption.CheckedNoChange := uacKerberosDesOnly in UserAccountControl;
  CheckBox_NoKerberosPreauth.CheckedNoChange := uacKerberosRequirePreAuth in UserAccountControl;

  // https://ldapwiki.com/wiki/Wiki.jsp?page=MsDS-SupportedEncryptionTypes
  MsdsSupportedEncryptionTypes := MsdsSupportedEncryptionTypesFromText(fProperty.GetReadable('msDS-SupportedEncryptionTypes'));

  CheckBox_KerberosAES128Encryption.CheckedNoChange := metAes128CtsHmacSha1 in MsdsSupportedEncryptionTypes;
  CheckBox_KerberosAES256Encryption.CheckedNoChange := metAes256CtsHmacSha1 in MsdsSupportedEncryptionTypes;
end;

procedure TFrmPropertyAccount.UpdateExpires;
var
  AccountExpires: RawUtf8;
  Value: int64;
begin
  // 0 and 9223372036854775807 (0x7FFFFFFFFFFFFFFF) mean "never expire"
  // 0 will be used as default value
  AccountExpires := fProperty.GetRaw('accountExpires');

  if (AccountExpires = '0') or (AccountExpires = '9223372036854775807') then
  begin
    RadioButton_Never.CheckedNoChange := True;
    DateTimePicker_Expires.DateTime := Now();
  end
  else
  begin
    RadioButton_EndOf.CheckedNoChange := True;
    if not TryStrToInt64(AccountExpires, Value) then
      if Assigned(fLog) then
        fLog.Log(sllError, 'Cannot convert str(%) to int.', [AccountExpires], Self);
    DateTimePicker_Expires.DateTimeNoChange := MSTimeToDateTime(Value);
  end;
  DateTimePicker_Expires.Enabled := RadioButton_EndOf.Checked;

  if not CheckBox_CannotChange.Checked then
    CheckBox_CannotChange.CheckedNoChange := fProperty.CannotChangePassword;
end;

procedure TFrmPropertyAccount.UserAccountControlUpdate(
  UserAccountControl: TUserAccountControl; Include: Boolean);
begin
  if Include then
    fProperty.UserAccountControlInclude(UserAccountControl)
  else
    fProperty.UserAccountControlExclude(UserAccountControl);
end;

procedure TFrmPropertyAccount.msDSSupportedEncryptionTypeUpdate(
  msDSSupportedEncryptionType: TMsdsSupportedEncryptionType; Include: Boolean);
begin
  if Include then
    fProperty.msDSSupportedEncryptionTypeInclude(msDSSupportedEncryptionType)
  else
    fProperty.msDSSupportedEncryptionTypeExclude(msDSSupportedEncryptionType);
end;

constructor TFrmPropertyAccount.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);

  Caption := 'Account';
end;

procedure TFrmPropertyAccount.Update(Props: TProperty);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Update', Self);

  fProperty := Props;

  UpdateLogonName;
  UpdateSamaccountName;

  CheckBox_Unlock.CheckedNoChange  := False;

  UpdateOptions;
  UpdateExpires;
end;

end.

