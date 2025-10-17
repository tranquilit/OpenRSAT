unit ufrmnewuser;

{$mode ObjFPC}{$H+}

interface

uses
  // Lazarus / fpc
  ActnList,
  Classes,
  ExtCtrls,
  Forms,
  Graphics,
  StdCtrls,
  // Submodules
  tis.ui.searchedit;

type

  { TFrmNewUser }

  TFrmNewUser = class(TFrame)

    Panel_Page0: TPanel;
      Label_FirstName: TLabel;
      Label_Initials: TLabel;
      Label_LastName: TLabel;
      Label_FullName: TLabel;
      Edit_FirstName: TEdit;
      Edit_Initials: TEdit;
      Edit_LastName: TEdit;
      Edit_FullName: TEdit;
      Label_UserLogon: TLabel;
      Edit_UserLogon: TEdit;
      TisSearchEdit_UserLogonDomain: TTisSearchEdit;
      Label_WinUserLogonName: TLabel;
      Edit_nETBIOSDomain: TEdit;
      Edit_nETBIOSName: TEdit;
    Panel_Page1: TPanel;
      Label_Password: TLabel;
      Edit_Password: TEdit;
      Label_Confirm: TLabel;
      Edit_Confirm: TEdit;
      CheckBox_MustChangePassword: TCheckBox;
      CheckBox_CannotChangePassword: TCheckBox;
      CheckBox_PasswordNeverExpires: TCheckBox;
      CheckBox_AccountDisabled: TCheckBox;
    Panel_Page2: TPanel;
      Label_Resume: TLabel;
      ListBox_Resume: TMemo;
    ActionList: TActionList;
      Action_Next: TAction;
      Action_Back: TAction;
    procedure Action_NextExecute(Sender: TObject);
    procedure Action_BackExecute(Sender: TObject);
    procedure Action_BackUpdate(Sender: TObject);
    procedure Action_NextUpdate(Sender: TObject);
    procedure CheckBox_PwdChange(Sender: TObject);
    procedure Edit_ConfirmChange(Sender: TObject);
    procedure NameChange(Sender: TObject);
    procedure Edit_PasswordChange(Sender: TObject);
    procedure Edit_UserLogonChange(Sender: TObject);
  private
    procedure OKBtn();
    procedure Load;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

implementation
uses
  // Lazarus / fpc
  Controls,
  Dialogs,
  SysUtils,
  // Submodules
  mormot.core.text,
  mormot.core.os.security,
  mormot.net.ldap,
  // Rsat
  ucommon,
  ucoredatamodule,
  ursatldapclient,
  uvisnewobject;
{$R *.lfm}

{ TFrmNewUser - private }

procedure TFrmNewUser.OKBtn();
var
  NewObject: TVisNewObject;
  AttList: TLdapAttributeList;
  Att: TLdapAttribute;
  UAC: TUserAccountControls = [uacNormalAccount];
  DN: String;
  SecDesc: TSecurityDescriptor;
  AceSelf, AceWorld: PSecAce;
begin
  NewObject := (owner as TVisNewObject);
  Dec(NewObject.PageIdx);

  AttList := TLdapAttributeList.Create();
  try
    Att := AttList.Add(atObjectClass, 'top');
    Att.Add('person');
    Att.Add('organizationalPerson');
    Att.Add('user');

    if Edit_FirstName.Text <> '' then
      AttList.Add(atGivenName, Edit_FirstName.Text);
    if Edit_LastName.Text <> '' then
      AttList.Add(atSurName, Edit_LastName.Text);
    if Edit_FullName.Text <> '' then
      AttList.Add(atDisplayName, Edit_FullName.Text);
    if Edit_Initials.Text <> '' then
      AttList.Add(atInitials, Edit_Initials.Text);
    if Edit_nETBIOSName.Text <> '' then
      AttList.Add(atSAMAccountName, Edit_nETBIOSName.Text);
    if (Edit_UserLogon.Text <> '') and (TisSearchEdit_UserLogonDomain.Text <> '') then
      AttList.Add(atUserPrincipalName, Edit_UserLogon.Text + TisSearchEdit_UserLogonDomain.Text);

    if Edit_Password.Text <> '' then
      AttList.AddUnicodePwd(Edit_Password.Text);

    if CheckBox_MustChangePassword.Checked then
      AttList.Add(atPwdLastSet, '0');

    if CheckBox_PasswordNeverExpires.Checked then
      Include(UAC, uacPasswordDoNotExpire);
    if CheckBox_AccountDisabled.Checked then
      Include(UAC, uacAccountDisable);
    if UAC <> [] then
      AttList.Add(atUserAccountControl, UserAccountControlsValue(UAC).ToString());

    DN := FormatUtf8('CN=%,%', [Edit_FullName.Text, NewObject.ObjectOU]);
    if not NewObject.Ldap.Add(DN, AttList) then
    begin
      ShowLdapAddError(NewObject.Ldap);
      Exit;
    end;
  finally
    FreeAndNil(AttList);
  end;

  if CheckBox_CannotChangePassword.Checked then
  begin
    AttList.Clear();
    // https://learn.microsoft.com/en-us/windows/win32/adsi/modifying-user-cannot-change-password-ldap-provider
    Att := NewObject.Ldap.SearchObject(atNTSecurityDescriptor, DN, '');
    if not Assigned(Att) then
    begin
      ShowLdapSearchError(NewObject.Ldap);
      Exit;
    end;

    SecDesc.FromBinary(Att.GetRaw());
    AceSelf  := SecDescAddOrUpdateACE(@SecDesc, ATTR_UUID[kaUserChangePassword],
      KnownRawSid(wksSelf),  satObjectAccessAllowed, [samControlAccess]);
    AceWorld := SecDescAddOrUpdateACE(@SecDesc, ATTR_UUID[kaUserChangePassword],
      KnownRawSid(wksWorld), satObjectAccessAllowed, [samControlAccess]);

    AceSelf^.AceType  := satObjectAccessDenied;
    AceWorld^.AceType := satObjectAccessDenied;

    OrderAcl(NewObject.Ldap, DN, (Owner as TVisNewObject).BaseDN, @SecDesc.Dacl);

    if not NewObject.Ldap.Modify(DN, lmoReplace, atNTSecurityDescriptor, SecDesc.ToBinary()) then
    begin
      ShowLdapModifyError(NewObject.Ldap);
      Exit;
    end;
  end;

  NewObject.ModalResult := mrOK;
end;

procedure TFrmNewUser.Load;
begin
  Edit_FirstName.SetFocus;
end;

constructor TFrmNewUser.Create(TheOwner: TComponent);
var
  OwnerNewObject: TVisNewObject absolute TheOwner;
  SearchObject: TLdapResult;
  Item: String;
begin
  inherited Create(TheOwner);

  OwnerNewObject.Caption := rsNewObjectUser;
  OwnerNewObject.Btn_Next.Action := ActionList.ActionByName('Action_Next');
  OwnerNewObject.Btn_Next.Default := True;
  OwnerNewObject.Btn_Back.Visible := True;
  OwnerNewObject.Btn_Back.Action := ActionList.ActionByName('Action_Back');
  OwnerNewObject.Btn_Back.Caption := rsNewObjectBtnBack;
  OwnerNewObject.Image_Object.ImageIndex := Ord(ileADUser);
  OwnerNewObject.CallBack := @Load;

  TisSearchEdit_UserLogonDomain.Items.Add('@' + DNToCN((TheOwner as TVisNewObject).Ldap.DefaultDN));
  if (TheOwner as TVisNewObject).Ldap.DefaultDN <> (TheOwner as TVisNewObject).Ldap.RootDN then
    TisSearchEdit_UserLogonDomain.Items.Add('@' + DNToCN((TheOwner as TVisNewObject).Ldap.RootDN));

  TisSearchEdit_UserLogonDomain.ItemIndex := 0;
  Edit_nETBIOSDomain.Caption := (TheOwner as TVisNewObject).Ldap.NetbiosDN + '\';

  SearchObject := (TheOwner as TVisNewObject).Ldap.SearchObject(FormatUtf8('CN=Partitions,%', [(TheOwner as TVisNewObject).Ldap.ConfigDN]), '', ['uPNSuffixes']);
  if Assigned(SearchObject) then
  begin
    for Item in SearchObject.Find('uPNSuffixes').GetAllReadable do
      TisSearchEdit_UserLogonDomain.Items.Add('@' + Item);
  end;
end;

{ TFrmNewUser - public }

procedure TFrmNewUser.Edit_UserLogonChange(Sender: TObject);
begin
  Edit_nETBIOSName.Text := UpperCase(Edit_UserLogon.Text);
end;

procedure TFrmNewUser.NameChange(Sender: TObject);
var
  values: TStringArray;
begin
  values := [];

  if Edit_FirstName.Text <> '' then
    Insert(Edit_FirstName.Text, values, Length(values));
  if Edit_Initials.Text <> '' then
    Insert(Edit_Initials.Text + '.', values, Length(values));
  if Edit_LastName.Text <> '' then
    Insert(Edit_LastName.Text, values, Length(values));

  Edit_FullName.Text := String.Join(' ', values);
end;

procedure TFrmNewUser.Edit_PasswordChange(Sender: TObject);
begin
  if Edit_Password.Text <> Edit_Confirm.Text then
    Edit_Confirm.Font.Color := clRed
  else
    Edit_Confirm.Font.Color := clDefault;
end;

procedure TFrmNewUser.Edit_ConfirmChange(Sender: TObject);
begin
  if Edit_Password.Text <> Edit_Confirm.Text then
    Edit_Confirm.Font.Color := clRed
  else
    Edit_Confirm.Font.Color := clDefault;
end;

procedure TFrmNewUser.CheckBox_PwdChange(Sender: TObject);
begin
  if not (Sender as TCheckBox).Checked then
    Exit;

  case (Sender as TCheckBox).Name of
    'CheckBox_MustChange':
    begin
      CheckBox_CannotChangePassword.Checked   := False;
      CheckBox_PasswordNeverExpires.Checked      := False;
    end;
    'CheckBox_NoChange',
    'CheckBox_Never':
      CheckBox_MustChangePassword.Checked := False;
  end;
end;

// Action
procedure TFrmNewUser.Action_NextExecute(Sender: TObject);
var
  NewObject: TVisNewObject;
begin
  NewObject := (owner as TVisNewObject);
  Inc(NewObject.PageIdx);
  if NewObject.PageIdx = NewObject.PageCount then
  begin
    OKBtn();
    Exit;
  end;

  Panel_Page0.Visible := False;
  Panel_Page1.Visible := False;
  Panel_Page2.Visible := False;
  case NewObject.PageIdx of
    1:
    begin
      Panel_Page1.Visible := True;
      Edit_Password.SetFocus;
    end;
    2:
    begin
      Panel_Page2.Visible := True;
      ListBox_Resume.Clear();
      ListBox_Resume.Lines.Add(FormatUtf8(rsNewUserFullName, [Edit_FullName.Text]));
      ListBox_Resume.Lines.Add(FormatUtf8(rsNewUserLogonName, [Edit_UserLogon.Text]));

      if CheckBox_MustChangePassword.Checked then
        ListBox_Resume.Lines.Add(rsNewUserChangePassword);
      if CheckBox_CannotChangePassword.Checked then
        ListBox_Resume.Lines.Add(rsNewUserNOChangePassword);
      if CheckBox_PasswordNeverExpires.Checked then
        ListBox_Resume.Lines.Add(rsNewUserPasswordNoExpire);
      if CheckBox_AccountDisabled.Checked then
        ListBox_Resume.Lines.Add(rsNewUserDisabled);
    end;
  end;
end;

procedure TFrmNewUser.Action_NextUpdate(Sender: TObject);
begin
  case (owner as TVisNewObject).PageIdx of
    0:
    begin
      (owner as TVisNewObject).Btn_Next.Caption := rsNewObjectBtnNext;
      Action_Next.Enabled := (Trim(Edit_UserLogon.Text) <> '') and (Trim(Edit_FullName.Text) <> '') and (Trim(Edit_nETBIOSName.Text) <> '');          // FullName && UserLogon && nETBIOSName
    end;
    1:
    begin
      (owner as TVisNewObject).Btn_Next.Caption := rsNewObjectBtnNext;
      Action_Next.Enabled := True;
    end;
    2:
    begin
      (owner as TVisNewObject).Btn_Next.Caption := rsNewObjectBtnOK;
      Action_Next.Enabled := True;
    end;
  end;
end;

procedure TFrmNewUser.Action_BackExecute(Sender: TObject);
var
  NewObject: TVisNewObject;
begin
  NewObject := (owner as TVisNewObject);
  Dec(NewObject.PageIdx);

  Panel_Page0.Visible := False;
  Panel_Page1.Visible := False;
  Panel_Page2.Visible := False;
  case NewObject.PageIdx of
    0:
    begin
      Panel_Page0.Visible := True;
      Edit_FirstName.SetFocus;
    end;
    1:
    begin
      Panel_Page1.Visible := True;
      Edit_Password.SetFocus;
    end;
  end;
end;

procedure TFrmNewUser.Action_BackUpdate(Sender: TObject);
begin
  Action_Back.Enabled := (owner as TVisNewObject).PageIdx <> 0;
end;

end.
