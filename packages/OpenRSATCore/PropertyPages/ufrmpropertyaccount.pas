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
  uproperty,
  upropertyframe;

type

  { TFrmPropertyAccount }

  TFrmPropertyAccount = class(TPropertyFrame)
    Action1: TAction;
    Action2: TAction;
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
    procedure Action1Execute(Sender: TObject);
    procedure Action2Execute(Sender: TObject);
    procedure CheckBox_MustChangeChange(Sender: TObject);
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
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Update(Props: TProperty); override;
  end;

implementation
uses
  mormot.core.text,
  mormot.net.ldap,
  ucommon,
  uhelpers,
  uvislogonhours,
  uvislogonworkstation;

{$R *.lfm}

{ TFrmPropertyAccount }

procedure TFrmPropertyAccount.Edit_NameChange(Sender: TObject);
begin
  fProperty.Add('userPrincipalName', FormatUtf8('%%', [Edit_Name, ComboBox_Domain.Text]));
end;

procedure TFrmPropertyAccount.Edit_SAMAccountNameChange(Sender: TObject);
begin
  fProperty.sAMAccountName := Edit_SAMAccountName.Text;
end;

procedure TFrmPropertyAccount.RadioButton_EndOfChange(Sender: TObject);
begin
  if not RadioButton_EndOf.Checked then
    Exit;
  fProperty.Add('accountExpires', IntToStr(DateTimeToMSTime(DateTimePicker_Expires.DateTime)));
end;

procedure TFrmPropertyAccount.RadioButton_NeverChange(Sender: TObject);
begin
  if not RadioButton_Never.Checked then
    Exit;
  fProperty.Add('accountExpires', '0');
end;

procedure TFrmPropertyAccount.ComboBox_DomainChange(Sender: TObject);
begin
  fProperty.Add('userPrincipalName', FormatUtf8('%%', [Edit_Name, ComboBox_Domain.Text]));
end;

procedure TFrmPropertyAccount.DateTimePicker_ExpiresChange(Sender: TObject);
begin
  if not RadioButton_EndOf.Checked then
    Exit;

  fProperty.Add('accountExpires', IntToStr(DateTimeToMSTime(DateTimePicker_Expires.DateTime)));
end;

procedure TFrmPropertyAccount.Action1Execute(Sender: TObject);
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

procedure TFrmPropertyAccount.Action2Execute(Sender: TObject);
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

procedure TFrmPropertyAccount.CheckBox_MustChangeChange(Sender: TObject);
begin

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
begin
  // Default value
  Edit_Name.CaptionNoChange := '';
  ComboBox_Domain.ItemIndex := -1;

  // Fill UPN Suffixes
  ComboBox_Domain.Items.BeginUpdate;
  try
    ComboBox_Domain.Items.AddStrings(TStringDynArray(fProperty.UPNSuffixes));
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
  msDSSETstr, pwdLastSet: RawUtf8;
  msDSSET: Longint;
begin
  pwdLastSet := fProperty.GetReadable('pwdLastSet');
  CheckBox_MustChange.Checked := (pwdLastSet = '') or (pwdLastSet = '0');

  // https://ldapwiki.com/wiki/Wiki.jsp?page=User-Account-Control%20Attribute
  UserAccountControl := UserAccountControlsFromText(fProperty.GetReadable('userAccountControl'));

  CheckBox_CannotChange.Checked := uacPasswordCannotChange in UserAccountControl;
  CheckBox_NeverExpires.Checked := uacPasswordDoNotExpire in UserAccountControl;
  CheckBox_ReversibleEncryption.Checked := uacPasswordUnencrypted in UserAccountControl;
  CheckBox_Disabled.Checked := uacAccountDisable in UserAccountControl;
  CheckBox_SmartCard.Checked := uacSmartcardRequired in UserAccountControl;
  CheckBox_Sensitive.Checked := uacKerberosNotDelegated in UserAccountControl;
  CheckBox_KerberosDESEncryption.Checked := uacKerberosDesOnly in UserAccountControl;
  CheckBox_NoKerberosPreauth.Checked := uacKerberosRequirePreAuth in UserAccountControl;

  // https://ldapwiki.com/wiki/Wiki.jsp?page=MsDS-SupportedEncryptionTypes
  msDSSETstr := fProperty.GetReadable('msDS-SupportedEncryptionTypes');

  if not TryStrToInt(msDSSETstr, msDSSET) then
    msDSSET := 0;
  CheckBox_KerberosAES128Encryption.Checked := (msDSSET and AES128) > 0;
  CheckBox_KerberosAES256Encryption.Checked := (msDSSET and AES256) > 0;
end;

procedure TFrmPropertyAccount.UpdateExpires;
var
  AccountExpires: RawUtf8;
  Value: Longint;
begin
  // 0 and 9223372036854775807 (0x7FFFFFFFFFFFFFFF) mean "never expire"
  // 0 will be used as default value
  AccountExpires := fProperty.GetReadable('accountExpires');

  if (AccountExpires = '') or (AccountExpires = '9223372036854775807') then
  begin
    RadioButton_Never.Checked := True;
    DateTimePicker_Expires.DateTime := Now();
  end
  else
  begin
    RadioButton_EndOf.Checked := True;
    if not TryStrToInt(AccountExpires, Value) then
      if Assigned(fLog) then
        fLog.Log(sllError, 'Cannot convert str(%) to int.', [AccountExpires], Self);
    DateTimePicker_Expires.DateTime := MSTimeToDateTime(Value);
  end;
  DateTimePicker_Expires.Enabled := RadioButton_EndOf.Checked;

  if not CheckBox_CannotChange.Checked then
    CheckBox_CannotChange.Checked := fProperty.CannotChangePassword;
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

  CheckBox_Unlock.Checked  := False;

  UpdateOptions;
  UpdateExpires;
end;

end.

