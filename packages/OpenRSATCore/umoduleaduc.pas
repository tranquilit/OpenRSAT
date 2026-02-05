unit umoduleaduc;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.base,
  mormot.core.log,
  umoduleaducoption,
  uoption,
  umodule,
  ursat;

type

  TResetPasswordCallback = function(const LockerAccount: Boolean; out ANewPassword: RawUtf8; out AUserMustChangePassword: Boolean; out AUnlockAccount: Boolean): Boolean of object;

  { TModuleADUC }

  TModuleADUC = class(TModule)
  private
    fModuleADUCOption: TModuleADUCOption;

    fEnabled: Boolean;
    fLog: TSynLog;

    fRSAT: TRSAT;
  public
    constructor Create(ARSAT: TRSAT);
    destructor Destroy; override;

    property ADUCOption: TModuleADUCOption read fModuleADUCOption;

    function ResetPassword(AResetPasswordCallback: TResetPasswordCallback; AObjectDN: RawUtf8; AComputerWarning, AOnUnlockedAccount, AOnPasswordChanged: TNotifyEvent): Boolean;

    /// TModule
  protected
    function GetEnabled: Boolean; override;
    procedure SetEnabled(AValue: Boolean); override;
    function GetName: RawUtf8; override;
    function GetDisplayName: RawUtf8; override;
    function GetOption: TOption; override;
  end;

implementation
uses
  ucommon,
  mormot.net.ldap;

{ TModuleADUC }

constructor TModuleADUC.Create(ARSAT: TRSAT);
begin
  fRSAT := ARSAT;
  fEnabled := True;
  fLog := TSynLog.Add;
  fModuleADUCOption := TModuleADUCOption.Create;
  fModuleADUCOption.Load;
end;

destructor TModuleADUC.Destroy;
begin
  FreeAndNil(fModuleADUCOption);

  inherited Destroy;
end;

function TModuleADUC.ResetPassword(
  AResetPasswordCallback: TResetPasswordCallback; AObjectDN: RawUtf8;
  AComputerWarning, AOnUnlockedAccount, AOnPasswordChanged: TNotifyEvent
  ): Boolean;
var
  NewPassword: RawUtf8;
  UserMustChangePassword, UnlockAccount: Boolean;
  Attribute, ObjectClassAttr, UserAccountControlAttr: TLdapAttribute;
  UserData: TLdapResult;
  UserAccountControls: TUserAccountControls;
begin

  UserData := RSAT.LdapClient.SearchObject(AObjectDN, '', ['userAccountControl', 'objectClass']);
  if not Assigned(UserData) then
  begin
    if Assigned(fLog) then
      fLog.Log(sllError, 'Cannot retrieve information about "%". (%)', [AObjectDN, RSAT.LdapClient.ResultString], Self);
    Exit;
  end;

  ObjectClassAttr := UserData.Attributes.Find('objectClass');
  if not Assigned(ObjectClassAttr) then
  begin
    if Assigned(fLog) then
      fLog.Log(sllError, 'Cannot retrieve objectClass about "%".', [AObjectDN], Self);
    Exit;
  end;

  if Assigned(fLog) then
    fLog.Log(sllInfo, 'Reset password on %.', [ObjectClassAttr.GetReadable(Pred(ObjectClassAttr.Count))], Self);

  if Assigned(AComputerWarning) then
    AComputerWarning(ObjectClassAttr);

  UserAccountControlAttr := UserData.Attributes.Find('userAccountControl');
  if not Assigned(UserAccountControlAttr) then
  begin
    if Assigned(fLog) then
      fLog.Log(sllError, 'Cannot retrieve userAccountControl about "%".', [AObjectDN], Self);
    Exit;
  end;

  UserAccountControls := UserAccountControlsFromText(UserAccountControlAttr.GetRaw());

  result := AResetPasswordCallback((uacLockedOut in UserAccountControls), NewPassword, UserMustChangePassword, UnlockAccount);

  // modify userAccountControl
  if UnlockAccount then
  begin
    Attribute := TLdapAttribute.Create('userAccountControl', atUserAccountControl);
    try
      Include(UserAccountControls, uacLockedOut);
      Attribute.Add(IntToStr(UserAccountControlsValue(UserAccountControls)));
      RSAT.LdapClient.OnModify := AOnUnlockedAccount;
      if not RSAT.LdapClient.Modify(AObjectDN, lmoReplace, Attribute) then
      begin
        if Assigned(fLog) then
          fLog.Log(sllError, 'Ldap Modify Error: %', [RSAT.LdapClient.ResultString], Self);
        Exit;
      end;
    finally
      FreeAndNil(Attribute);
    end;
  end;
  Attribute := TLdapAttribute.Create('unicodePwd', atUndefined);
  try
    Attribute.add(LdapUnicodePwd(NewPassword));
    RSAT.LdapClient.OnModify := AOnPasswordChanged;
    if not RSAT.LdapClient.Modify(AObjectDN, lmoReplace, Attribute) then
    begin
      if Assigned(fLog) then
        fLog.Log(sllError, 'Ldap Modify Error: %', [RSAT.LdapClient.ResultString], Self);
      Exit;
    end;
  finally
    FreeAndNil(Attribute);
  end;

  // Set reset password
  if UserMustChangePassword then
  begin
    Attribute := TLdapAttribute.Create('pwdLastSet', atPwdLastSet);
    try
      Attribute.Add(IntToStr(0));
      if not RSAT.LdapClient.Modify(AObjectDN, lmoReplace, Attribute) then
      begin
        if Assigned(fLog) then
          fLog.Log(sllError, 'Ldap Modify Error: %', [RSAT.LdapClient.ResultString], Self);
        Exit;
      end;
    finally
      FreeAndNil(Attribute);
    end;
  end;
end;

function TModuleADUC.GetEnabled: Boolean;
begin
  result := fEnabled;
end;

procedure TModuleADUC.SetEnabled(AValue: Boolean);
begin
  if AValue = fEnabled then
    Exit;
  fEnabled := AValue;
end;

function TModuleADUC.GetName: RawUtf8;
begin
  result := rsModuleADUCName;
end;

function TModuleADUC.GetDisplayName: RawUtf8;
begin
  result := rsModuleADUCDisplayName;
end;

function TModuleADUC.GetOption: TOption;
begin
  result := fModuleADUCOption;
end;

end.

