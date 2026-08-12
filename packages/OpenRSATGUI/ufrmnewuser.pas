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
  mormot.net.ldap,
  mormot.core.base;

type

  { TFrmNewUser }

  TFrmNewUser = class(TFrame)
    ComboBox1: TComboBox;

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
    BaseObj: TLdapAttributeList;

    procedure OKBtn();
    procedure Load;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Copy(const DistinguishedName: RawUtf8);
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
  // Rsat
  ucommon,
  ucoredatamodule,
  ursatldapclient,
  ursatldapclientui,
  uvisnewobject;
{$R *.lfm}

{ TFrmNewUser - private }

procedure TFrmNewUser.OKBtn();
var
  Ldap: TLdapClient;
  DN: String;
  ObjectOU, MemberOf: RawUtf8;
  NewUser: TLdapAttributeList;
  MemberOfAttr: TLdapAttribute;

  procedure CopyAttribute(Src, Dest: TLdapAttributeList; const AttributeName: RawUtf8);
  var
    A, B: TLdapAttribute;
    i: Integer;
  begin
    A := Src.Find(AttributeName);
    if not Assigned(A) then
      Exit;

    B := Dest.Add(AttributeName);
    B.AddFrom(A);
  end;

  procedure CopyAttributes(Src, Dest: TLdapAttributeList; const AttributeNames: TRawUtf8DynArray);
  var
    i: Integer;
  begin
    for i := 0 to High(AttributeNames) do
      CopyAttribute(Src, Dest, AttributeNames[i]);
  end;

begin
  Ldap := (Owner as TVisNewObject).Ldap;
  ObjectOU := (Owner as TVisNewObject).ObjectOU;
  Dec((Owner as TVisNewObject).PageIdx);

  NewUser := PrepareNewUser(
    Edit_FirstName.Text,
    Edit_LastName.Text,
    Edit_FullName.Text,
    Edit_Initials.Text,
    Edit_nETBIOSName.Text,
    FormatUtf8('%%', [Edit_UserLogon.Text, ComboBox1.Text])
  );

  try
    if Assigned(BaseObj) then
    begin
      CopyAttributes(BaseObj, NewUser, [
        'c',
        'l',
        'st',
        'postalCode',
        'postOfficeBox',
        'co',
        'department',
        'company',
        'codePage',
        'countryCode',
        'logonHours',
        'accountExpires',
        'userAccountControl']);
    end;

    ChangePassword(NewUser, Edit_Password.Text);
    MustChangePassword(NewUser, CheckBox_MustChangePassword.Checked);
    PasswordNeverExpires(NewUser, CheckBox_PasswordNeverExpires.Checked);
    DisableAccount(NewUser, CheckBox_AccountDisabled.Checked);

    DN := FormatUtf8('CN=%,%', [Edit_FullName.Text, ObjectOU]);
    if not Ldap.Add(DN, NewUser) then
      Exit;
    if Assigned(BaseObj) then
    begin
      MemberOfAttr := BaseObj.Find('memberOf');
      if Assigned(MemberOfAttr) then
      begin
        for MemberOf in MemberOfAttr.GetAllReadable do
        begin
          if MemberOf = '' then
            Continue;
          if not Ldap.Modify(MemberOf, lmoAdd, 'member', DN) then
            Exit;
        end;
      end;
    end;
  finally
    FreeAndNil(NewUser);
  end;

  if CheckBox_CannotChangePassword.Checked then
    CannotChangePassword(Ldap, DN);

  (Owner as TVisNewObject).ModalResult := mrOK;
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

  BaseObj := nil;

  OwnerNewObject.Caption := rsNewObjectUser;
  OwnerNewObject.Btn_Next.Action := ActionList.ActionByName('Action_Next');
  OwnerNewObject.Btn_Next.Default := True;
  OwnerNewObject.Btn_Back.Visible := True;
  OwnerNewObject.Btn_Back.Action := ActionList.ActionByName('Action_Back');
  OwnerNewObject.Btn_Back.Caption := rsNewObjectBtnBack;
  OwnerNewObject.Image_Object.ImageIndex := Ord(ileADUser);
  OwnerNewObject.CallBack := @Load;

  ComboBox1.Items.Add('@' + DNToCN((TheOwner as TVisNewObject).Ldap.DefaultDN));
  if (TheOwner as TVisNewObject).Ldap.DefaultDN <> (TheOwner as TVisNewObject).Ldap.RootDN then
    ComboBox1.Items.Add('@' + DNToCN((TheOwner as TVisNewObject).Ldap.RootDN));

  ComboBox1.ItemIndex := 0;
  Edit_nETBIOSDomain.Caption := (TheOwner as TVisNewObject).Ldap.NetbiosDN + '\';

  SearchObject := (TheOwner as TVisNewObject).Ldap.SearchObject(FormatUtf8('CN=Partitions,%', [(TheOwner as TVisNewObject).Ldap.ConfigDN]), '', ['uPNSuffixes']);
  if Assigned(SearchObject) then
  begin
    for Item in SearchObject.Find('uPNSuffixes').GetAllReadable do
      ComboBox1.Items.Add('@' + Item);
  end;
end;

destructor TFrmNewUser.Destroy;
begin
  if Assigned(BaseObj) then
    FreeAndNil(BaseObj);
  inherited Destroy;
end;

procedure TFrmNewUser.Copy(const DistinguishedName: RawUtf8);
var
  UserPrincipalName, PwdLastSet: RawUtf8;
  UAC: TUserAccountControls;
  Idx: SizeInt;
  suffix: String;
  Obj: TLdapResult;
begin
  Obj := (Owner as TVisNewObject).Ldap.SearchObject(DistinguishedName, '', ['*']);
  if not Assigned(Obj) then
    Exit;

  BaseObj := TLdapAttributeList(Obj.Attributes.Clone);
  UserPrincipalName := Obj.Find('userPrincipalName').GetReadable();
  UAC := UserAccountControlsFromText(Obj.Find('userAccountControl').GetReadable());
  PwdLastSet := Obj.Find('pwdLastSet').GetReadable();

  Idx := Pos('@', UserPrincipalName);
  suffix := String(UserPrincipalName).Substring(Idx - 1);
  ComboBox1.Caption := suffix;
  CheckBox_AccountDisabled.Checked := (uacAccountDisable in UAC);
  CheckBox_CannotChangePassword.Checked := (uacPasswordCannotChange in UAC);
  CheckBox_MustChangePassword.Checked := PwdLastSet = '0';
  CheckBox_PasswordNeverExpires.Checked := (uacPasswordDoNotExpire in UAC);
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
