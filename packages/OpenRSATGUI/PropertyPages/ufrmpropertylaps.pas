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
  mormot.core.base,
  mormot.core.log,
  uproperty,
  upropertyframe;

type

  { TFrmPropertyLAPS }

  TFrmPropertyLAPS = class(TPropertyFrame)
    Action_ExpireNow: TAction;
    Action_CopyPassword: TAction;
    Action_ShowPassword: TAction;
    ActionList1: TActionList;
    BitBtn_ExpireNow: TBitBtn;
    BitBtn_CopyPassword: TBitBtn;
    BitBtn_ShowPassword: TBitBtn;
    DateTimePicker_NewExpiration: TDateTimePicker;
    Edit_CurrentExpiration: TEdit;
    Edit_LocalAdminAccountName: TEdit;
    Edit_LocalAdminAccountPassword: TEdit;
    GroupBox1: TGroupBox;
    Label_CurrentExpiration: TLabel;
    Label_NewExpiration: TLabel;
    Label_LocalAdminAccountName: TLabel;
    Label_LocalAdminAccountPassword: TLabel;
    procedure Action_CopyPasswordExecute(Sender: TObject);
    procedure Action_ExpireNowExecute(Sender: TObject);
    procedure Action_ShowPasswordExecute(Sender: TObject);
    procedure DateTimePicker_NewExpirationChange(Sender: TObject);
  private
    fLog: TSynLog;
    fProperty: TProperty;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Update(Props: TProperty); override;
  end;

implementation
uses
  mormot.core.variants,
  mormot.net.ldap,
  ucommon,
  uhelpersui;

{$R *.lfm}

{ TFrmPropertyLAPS }

procedure TFrmPropertyLAPS.Action_ExpireNowExecute(Sender: TObject);
begin
  DateTimePicker_NewExpiration.DateTime := Now;
end;

procedure TFrmPropertyLAPS.Action_ShowPasswordExecute(Sender: TObject);
begin
  if (Edit_LocalAdminAccountPassword.EchoMode = emPassword) then
  begin
    Edit_LocalAdminAccountPassword.EchoMode := emNormal;
    Action_ShowPassword.Caption := 'Hide password';
  end
  else
  begin
    Edit_LocalAdminAccountPassword.EchoMode := emPassword;
    Action_ShowPassword.Caption := 'Show password';
  end;
end;

procedure TFrmPropertyLAPS.Action_CopyPasswordExecute(Sender: TObject);
begin
  Edit_LocalAdminAccountPassword.CopyToClipboard
end;

procedure TFrmPropertyLAPS.DateTimePicker_NewExpirationChange(Sender: TObject);
var
  AttributeName: RawUtf8;
begin
  AttributeName := 'ms-Mcs-AdmPwdExpirationTime';
  if fProperty.GetReadable(AttributeName) = '' then
  begin
    AttributeName := 'msLAPS-PasswordExpirationTime';
    if fProperty.GetReadable(AttributeName) = '' then
      Exit;
  end;
  fProperty.Add(AttributeName, DateTimeToStr(DateTimePicker_NewExpiration.DateTime));
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
  PasswordData: TDocVariantData;
  Password: RawUtf8;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Update', Self);

  fProperty := Props;

  fProperty.SearchObject(['msLAPS-Password', 'ms-Mcs-AdmPwd', 'msLAPS-PasswordExpirationTime', 'ms-Mcs-AdmPwdExpirationTime']);

  Edit_CurrentExpiration.CaptionNoChange := fProperty.GetReadable('msLAPS-PasswordExpirationTime');
  if Edit_CurrentExpiration.Caption = '' then
    Edit_CurrentExpiration.CaptionNoChange := fProperty.GetReadable('ms-Mcs-AdmPwdExpirationTime');
  if Edit_CurrentExpiration.Caption <> '' then
    Edit_CurrentExpiration.Caption := DateTimeToIsoString(LdapToDate(Edit_CurrentExpiration.Caption));

  Password := fProperty.GetReadable('msLAPS-Password');
  if Password <> '' then
  begin
    PasswordData.InitJson(Password);
    if PasswordData.Exists('n') then
      Edit_LocalAdminAccountName.CaptionNoChange := PasswordData.U['n'];
    if PasswordData.Exists('p') then
      Edit_LocalAdminAccountPassword.CaptionNoChange := PasswordData.U['p'];
  end
  else
    Edit_LocalAdminAccountPassword.CaptionNoChange := fProperty.GetReadable('ms-Mcs-AdmPwd');
end;

end.
