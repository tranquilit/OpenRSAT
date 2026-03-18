unit ufrmpropertylaps;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  DateTimePicker,
  Buttons,
  Controls,
  StdCtrls,
  ActnList,
  ExtCtrls,
  mormot.core.base,
  mormot.core.log,
  uproperty,
  upropertyframe;

type

  { TFrmPropertyLAPS }

  TFrmPropertyLAPS = class(TPropertyFrame)
    Action_v2_EncryptedCopyPassword: TAction;
    Action_v2_EncryptedShowPassword: TAction;
    Action_v1_ExpireNow: TAction;
    Action_v1_CopyPassword: TAction;
    Action_v1_ShowPassword: TAction;
    Action_v2_ExpireNow: TAction;
    Action_v2_CopyPassword: TAction;
    Action_v2_ShowPassword: TAction;
    ActionList1: TActionList;
    BitBtn_v2_EncryptedCopyPassword: TBitBtn;
    BitBtn_v2_EncryptedShowPassword: TBitBtn;
    BitBtn_v1_ExpireNow: TBitBtn;
    BitBtn_v1_ShowPassword: TBitBtn;
    BitBtn_v1_CopyPassword: TBitBtn;
    BitBtn_v2_ExpireNow: TBitBtn;
    BitBtn_v2_CopyPassword: TBitBtn;
    BitBtn_v2_ShowPassword: TBitBtn;
    DateTimePicker_v1_NewExpiration: TDateTimePicker;
    DateTimePicker_v2_NewExpiration: TDateTimePicker;
    Edit_v2_EncryptedPasswordData: TEdit;
    Edit_v2_EncryptedLocalAdminAccountName: TEdit;
    Edit_v2_EncryptedLocalAdminAccountPassword: TEdit;
    Edit_v1_CurrentExpiration: TEdit;
    Edit_v1_Password: TEdit;
    Edit_v2_CurrentExpiration: TEdit;
    Edit_v2_LocalAdminAccountName: TEdit;
    Edit_v2_LocalAdminAccountPassword: TEdit;
    GroupBox_LAPSEncrypted: TGroupBox;
    GroupBox_LAPSv2: TGroupBox;
    GroupBox_LAPSv1: TGroupBox;
    Label_v2_EncryptedPasswordData: TLabel;
    Label_v2_EncryptedLocalAdminAccountName: TLabel;
    Label_v2_EncryptedLocalAdminPassword: TLabel;
    Label_v1_CurrentExpiration: TLabel;
    Label_v1_NewExpiration: TLabel;
    Label_v1_Password: TLabel;
    Label_v2_CurrentExpiration: TLabel;
    Label_v2_NewExpiration: TLabel;
    Label_v2_LocalAdminAccountName: TLabel;
    Label_v2_LocalAdminAccountPassword: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    procedure Action_v1_CopyPasswordExecute(Sender: TObject);
    procedure Action_v1_ExpireNowExecute(Sender: TObject);
    procedure Action_v1_ShowPasswordExecute(Sender: TObject);
    procedure Action_v2_CopyPasswordExecute(Sender: TObject);
    procedure Action_v2_EncryptedCopyPasswordExecute(Sender: TObject);
    procedure Action_v2_EncryptedShowPasswordExecute(Sender: TObject);
    procedure Action_v2_ExpireNowExecute(Sender: TObject);
    procedure Action_v2_ShowPasswordExecute(Sender: TObject);
  private
    fLog: TSynLog;
    fProperty: TProperty;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Update(Props: TProperty); override;
  end;

implementation
uses
  mormot.core.datetime,
  mormot.core.os,
  mormot.core.variants,
  mormot.net.ldap,
  ucommon,
  DateUtils,
  uhelpersui;

{$R *.lfm}

{ TFrmPropertyLAPS }
function DateTimeToFileTime(ADateTime: TDateTime): QWord;
begin
  result := UnixMSTimeToWindowsFileTime64(DateTimeToUnixMSTime(ADateTime));
end;

procedure TFrmPropertyLAPS.Action_v2_ExpireNowExecute(Sender: TObject);
begin
  DateTimePicker_v2_NewExpiration.DateTime := Now;
  fProperty.Add('msLAPS-PasswordExpirationTime', IntToStr(DateTimeToFileTime(LocalTimeToUniversal(DateTimePicker_v2_NewExpiration.DateTime))));
end;

procedure ShowPassword(AEdit: TEdit; AAction: TAction);
begin
  if (AEdit.EchoMode = emPassword) then
  begin
    AEdit.EchoMode := emNormal;
    AAction.Caption := rsHidePassword;
  end
  else
  begin
    AEdit.EchoMode := emPassword;
    AAction.Caption := rsShowPassword;
  end;
end;

procedure TFrmPropertyLAPS.Action_v2_ShowPasswordExecute(Sender: TObject);
begin
  ShowPassword(Edit_v2_LocalAdminAccountPassword, Action_v2_ShowPassword);
end;

procedure TFrmPropertyLAPS.Action_v2_CopyPasswordExecute(Sender: TObject);
begin
  Edit_v2_LocalAdminAccountPassword.CopyToClipboard;
end;

procedure TFrmPropertyLAPS.Action_v2_EncryptedCopyPasswordExecute(
  Sender: TObject);
begin
  Edit_v2_EncryptedLocalAdminAccountPassword.CopyToClipboard;
end;

procedure TFrmPropertyLAPS.Action_v2_EncryptedShowPasswordExecute(
  Sender: TObject);
begin
  ShowPassword(Edit_v2_EncryptedLocalAdminAccountPassword, Action_v2_EncryptedShowPassword);
end;

procedure TFrmPropertyLAPS.Action_v1_ExpireNowExecute(Sender: TObject);
begin
  DateTimePicker_v1_NewExpiration.DateTime := Now;
  fProperty.Add('ms-Mcs-AdmPwdExpirationTime', IntToStr(DateTimeToFileTime(LocalTimeToUniversal(DateTimePicker_v1_NewExpiration.DateTime))));
end;

procedure TFrmPropertyLAPS.Action_v1_ShowPasswordExecute(Sender: TObject);
begin
  ShowPassword(Edit_v1_Password, Action_v1_ShowPassword);
end;

procedure TFrmPropertyLAPS.Action_v1_CopyPasswordExecute(Sender: TObject);
begin
  Edit_v1_Password.CopyToClipboard;
end;

constructor TFrmPropertyLAPS.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);

  Caption := 'LAPS';
end;

procedure TFrmPropertyLAPS.Update(Props: TProperty);
var
  LAPSInformation: PLAPSInformation;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Update', Self);

  fProperty := Props;

  fProperty.SearchObject(['msLAPS-Password', 'ms-Mcs-AdmPwd', 'msLAPS-PasswordExpirationTime', 'ms-Mcs-AdmPwdExpirationTime', 'msLAPS-EncryptedPassword']);

  LAPSInformation := fProperty.LAPSInformation;
  if not Assigned(LAPSInformation) then
  begin
    if Assigned(fLog) then
      fLog.Log(sllTrace, 'No laps information.', Self);
    Exit;
  end;

  GroupBox_LAPSv1.Visible := (LAPSInformation^.LAPSV1.Password <> '');
  if GroupBox_LAPSv1.Visible then
  begin
    Edit_v1_CurrentExpiration.CaptionNoChange := DateTimeToStr(UniversalTimeToLocal(LAPSInformation^.LAPSV1.Expiration));
    DateTimePicker_v1_NewExpiration.DateTimeNoChange := UniversalTimeToLocal(LAPSInformation^.LAPSV1.Expiration);
    Edit_v1_Password.CaptionNoChange := LAPSInformation^.LAPSV1.Password;
  end;

  GroupBox_LAPSEncrypted.Visible := (LAPSInformation^.LAPSV2.EncryptedPassword <> '');
  GroupBox_LAPSv2.Visible := (LAPSInformation^.LAPSV2.Password <> '') or GroupBox_LAPSEncrypted.Visible;
  if GroupBox_LAPSv2.Visible then
  begin
    Edit_v2_CurrentExpiration.CaptionNoChange := DateTimeToStr(UniversalTimeToLocal(LAPSInformation^.LAPSV2.Expiration));
    DateTimePicker_v2_NewExpiration.DateTimeNoChange := UniversalTimeToLocal(LAPSInformation^.LAPSV2.Expiration);
    Edit_v2_LocalAdminAccountName.CaptionNoChange := LAPSInformation^.LAPSV2.Account;
    Edit_v2_LocalAdminAccountPassword.CaptionNoChange := LAPSInformation^.LAPSV2.Password;
    if GroupBox_LAPSEncrypted.Visible then
    begin
      Edit_v2_EncryptedPasswordData.CaptionNoChange := LAPSInformation^.LAPSV2.EncryptedPassword;
    end;
  end;
end;

end.
