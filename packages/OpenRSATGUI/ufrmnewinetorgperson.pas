unit ufrmnewinetorgperson;

{$mode ObjFPC}{$H+}

interface

uses
  ActnList,
  Classes,
  ExtCtrls,
  Forms,
  Graphics,
  StdCtrls,
  tis.ui.searchedit;

type

  { TFrmNewInetOrgPerson }

  TFrmNewInetOrgPerson = class(TFrame)
    Action_Back: TAction;
    Action_Next: TAction;
    ActionList: TActionList;
    CheckBox_AccountDisabled: TCheckBox;
    CheckBox_NeverExpires: TCheckBox;
    CheckBox_CannotChange: TCheckBox;
    CheckBox_NextLogon: TCheckBox;
    ComboBox_UserLogonName: TComboBox;
    Edit_Confirm: TEdit;
    Edit_Password: TEdit;
    Edit_PreWindowsSuffix: TEdit;
    Edit_PreWindowsPrefix: TEdit;
    Edit_UserLogonName: TEdit;
    Edit_FullName: TEdit;
    Edit_LastName: TEdit;
    Edit_Initials: TEdit;
    Edit_FirstName: TEdit;
    Label_Validation: TLabel;
    Label_Password: TLabel;
    Label_Confirm: TLabel;
    Label_PreWindows: TLabel;
    Label_UserLogonName: TLabel;
    Label_FullName: TLabel;
    Label_LastName: TLabel;
    Label_Initials: TLabel;
    Label_FirstName: TLabel;
    Memo_Properties: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    procedure Action_BackExecute(Sender: TObject);
    procedure Action_BackUpdate(Sender: TObject);
    procedure Action_NextExecute(Sender: TObject);
    procedure Action_NextUpdate(Sender: TObject);
    procedure CheckBox_CannotChangeClick(Sender: TObject);
    procedure CheckBox_NeverExpiresClick(Sender: TObject);
    procedure CheckBox_NextLogonChange(Sender: TObject);
    procedure Edit_ConfirmChange(Sender: TObject);
    procedure Edit_FirstNameChange(Sender: TObject);
    procedure Edit_InitialsChange(Sender: TObject);
    procedure Edit_LastNameChange(Sender: TObject);
    procedure Edit_PasswordKeyPress(Sender: TObject; var Key: char);
    procedure Edit_UserLogonNameChange(Sender: TObject);
  private
    PageID: Integer;
    procedure BuildRecapProperties;
    procedure BtnOK;
    procedure Load;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

implementation

uses
  Controls,
  Dialogs,
  SysUtils,
  mormot.core.text,
  mormot.core.os.security,
  mormot.core.base,
  mormot.net.ldap,
  ucommon,
  ucoredatamodule,
  ursatldapclient,
  ursatldapclientui,
  uvisnewobject;

{$R *.lfm}

procedure TFrmNewInetOrgPerson.BuildRecapProperties;
begin
  Memo_Properties.Lines.Clear;
  Memo_Properties.Lines.Add(FormatUtf8('Full name: %%', [Edit_FullName.Text, LineEnding]));
  Memo_Properties.Lines.Add(FormatUtf8('User logon name: %%', [Edit_UserLogonName.Text, LineEnding]));
  if CheckBox_NextLogon.Checked then
    Memo_Properties.Lines.Add(CheckBox_NextLogon.Caption);
  if CheckBox_CannotChange.Checked then
    Memo_Properties.Lines.Add(CheckBox_CannotChange.Caption);
  if CheckBox_NeverExpires.Checked then
    Memo_Properties.Lines.Add(CheckBox_NeverExpires.Caption);
  if CheckBox_AccountDisabled.Checked then
    Memo_Properties.Lines.Add(CheckBox_AccountDisabled.Caption);
end;

procedure TFrmNewInetOrgPerson.BtnOK();
var
  Ldap: TLdapClient;
  NewPerson: TLdapAttributeList;
  DN, ObjectOU: RawUtf8;
begin
  Ldap := (Owner as TVisNewObject).Ldap;
  ObjectOU := (Owner as TVisNewObject).ObjectOU;

  NewPerson := PrepareInetOrgPerson(
    Edit_FirstName.Text,
    Edit_LastName.Text,
    Edit_FullName.Text,
    Edit_Initials.Text,
    Edit_UserLogonName.Text,
    FormatUtf8('%%', [Edit_UserLogonName.Text, ComboBox_UserLogonName.Text])
  );
  try
    ChangePassword(NewPerson, Edit_Password.Text);
    MustChangePassword(NewPerson, CheckBox_NextLogon.Checked);
    PasswordNeverExpires(NewPerson, CheckBox_NeverExpires.Checked);
    DisableAccount(NewPerson, CheckBox_AccountDisabled.Checked);

    DN := FormatUtf8('CN=%,%', [Edit_FullName.Text, ObjectOU]);
    if not Ldap.Add(DN, NewPerson) then
      Exit;
  finally
    FreeAndNil(NewPerson);
  end;

  if CheckBox_CannotChange.Checked then
    CannotChangePassword(Ldap, DN);

  (Owner as TVisNewObject).ModalResult := mrOK;
end;

procedure TFrmNewInetOrgPerson.Edit_FirstNameChange(Sender: TObject);
var
  FullName: RawUtf8;
begin
  FullName := Edit_FirstName.Text;
  if Edit_Initials.Text <> '' then
    FullName := FormatUtf8('% %.', [FullName, Edit_Initials.Text]);
  if Edit_LastName.Text <> '' then
    FullName := FormatUtf8('% %', [FullName, Edit_LastName.Text]);
  Edit_FullName.Text := FullName;
end;

procedure TFrmNewInetOrgPerson.Action_NextExecute(Sender: TObject);
begin
  case (PageID) of
    0:
    begin
      PageID += 1;
      Panel7.Visible := False;
      Panel8.Visible := True;
      Edit_Password.SetFocus;
      (owner as TVisNewObject).Btn_Next.Caption := rsNewObjectBtnNext;
    end;
    1:
    begin
      PageID += 1;
      BuildRecapProperties;
      Panel8.Visible := False;
      Panel9.Visible := True;
      (owner as TVisNewObject).Btn_Next.Caption := rsNewObjectBtnOK;
    end;
    2: BtnOK();
  end;
end;

procedure TFrmNewInetOrgPerson.Action_BackExecute(Sender: TObject);
begin
  PageID -= 1;
  case (PageId) of
    0:
    begin
      Panel7.Visible := True;
      Panel8.Visible := False;
      Edit_FullName.SetFocus;
      (owner as TVisNewObject).Btn_Next.Caption := rsNewObjectBtnNext;
    end;
    1:
    begin
      Panel8.Visible := True;
      Panel9.Visible := False;
      Edit_Password.SetFocus;
      (owner as TVisNewObject).Btn_Next.Caption := rsNewObjectBtnNext;
    end;
  end;
end;

procedure TFrmNewInetOrgPerson.Action_BackUpdate(Sender: TObject);
begin
  if PageID = 0 then
    Action_Back.Enabled := False
  else
    Action_Back.Enabled := True;
end;

procedure TFrmNewInetOrgPerson.Action_NextUpdate(Sender: TObject);
begin
  if PageID = 0 then
    Action_Next.Enabled := (Edit_FullName.Text <> '') and (Edit_UserLogonName.Text <> '')
  else if PageID = 1 then
    Action_Next.Enabled := (Edit_Password.Text = Edit_Confirm.Text) and (Edit_Password.Text <> '');
end;

procedure TFrmNewInetOrgPerson.CheckBox_CannotChangeClick(Sender: TObject);
begin
  if not CheckBox_CannotChange.Checked then
    exit;

  if CheckBox_NextLogon.Checked then
  begin
    CheckBox_CannotChange.Checked := False;
    MessageDlg(
      rsPasswordShouldNeverExpire + LineEnding + rsNotRequiredToChangePassword,
      mtError,
      [mbOK],
      0
    );
  end;
end;

procedure TFrmNewInetOrgPerson.CheckBox_NeverExpiresClick(Sender: TObject);
begin
  if CheckBox_NextLogon.Checked then
  begin
    CheckBox_NextLogon.Checked := False;
    MessageDlg(
      rsUserMustChangePasswordAndCannotChange,
      MtError,
      [mbOK],
      0
    );
  end;
end;

procedure TFrmNewInetOrgPerson.CheckBox_NextLogonChange(Sender: TObject);
begin
  if not CheckBox_NextLogon.Checked then
     exit;

  if CheckBox_NeverExpires.Checked then
  begin
    CheckBox_NextLogon.Checked := False;
    MessageDlg(
      rsPasswordShouldNeverExpire + LineEnding + rsNotRequiredToChangePassword,
      mtError,
      [mbOK],
      0
    );
  end
  else if CheckBox_CannotChange.Checked then
  begin
    CheckBox_CannotChange.Checked := False;
    MessageDlg(
      rsUserMustChangePasswordAndCannotChange,
      mtError,
      [mbOK],
      0
    );
  end;
end;

procedure TFrmNewInetOrgPerson.Edit_ConfirmChange(Sender: TObject);
begin
  if Edit_Confirm.Text <> Edit_Password.Text then
    Edit_Confirm.Font.Color := clRed
  else
    Edit_Confirm.Font.Color := clDefault;
end;

procedure TFrmNewInetOrgPerson.Edit_InitialsChange(Sender: TObject);
var
  FullName: RawUtf8;
begin
  if Edit_FirstName.Text <> '' then
    FullName := Edit_FirstName.Text;
  FullName := FormatUtf8('% %.', [FullName, Edit_Initials.Text]);
  if Edit_LastName.Text <> '' then
    FullName := FormatUtf8('% %', [FullName, Edit_LastName.Text]);
  Edit_FullName.Text := FullName;
end;

procedure TFrmNewInetOrgPerson.Edit_LastNameChange(Sender: TObject);
var
  FullName: RawUtf8;
begin
  if Edit_FirstName.Text <> '' then
    FullName := Edit_FirstName.Text;
  if Edit_Initials.Text <> '' then
    FullName := FormatUtf8('% %.', [FullName, Edit_Initials.Text]);
  FullName := FormatUtf8('% %', [FullName, Edit_LastName.Text]);
  Edit_FullName.Text := FullName;
end;

procedure TFrmNewInetOrgPerson.Edit_PasswordKeyPress(Sender: TObject;
  var Key: char);
begin
  if Key = #13 then
  begin
    Edit_Confirm.SetFocus;
    Key := #0
  end;
end;

procedure TFrmNewInetOrgPerson.Edit_UserLogonNameChange(Sender: TObject);
begin
  Edit_PreWindowsSuffix.Text := Edit_UserLogonName.Text
end;

procedure TFrmNewInetOrgPerson.Load;
begin
  Edit_FirstName.SetFocus;
end;

constructor TFrmNewInetOrgPerson.Create(TheOwner: TComponent);
var
  OwnerNewObject: TVisNewObject absolute TheOwner;
  Ldap: TLdapClient;
  SearchObject: TLdapResult;
  Item: RawUtf8;
begin
  inherited Create(TheOwner);

  OwnerNewObject.Caption := rsNewObjectInetOrgPerson;
  OwnerNewObject.Btn_Next.Action := ActionList.ActionByName('Action_Next');
  OwnerNewObject.Btn_Next.Default := True;
  OwnerNewObject.Btn_Back.Action := ActionList.ActionByName('Action_Back');
  OwnerNewObject.Btn_Back.Caption := rsNewObjectBtnBack;
  PageID := 0;

  Ldap := OwnerNewObject.Ldap;
  ComboBox_UserLogonName.Items.BeginUpdate;
  try
    ComboBox_UserLogonName.Items.Add(FormatUtf8('@%', [DNToCN(Ldap.DefaultDN)]));
    if Ldap.DefaultDN <> Ldap.RootDN then
      ComboBox_UserLogonName.Items.Add(FormatUtf8('@%', [DNToCN(Ldap.RootDN)]));
    SearchObject := Ldap.SearchObject(FormatUtf8('CN=Partitions,%', [Ldap.ConfigDN]), '', ['uPNSuffixes']);
    if Assigned(SearchObject) then
      for Item in SearchObject.Find('uPNSuffixes').GetAllReadable do
        ComboBox_UserLogonName.Items.Add(FormatUtf8('@%', [Item]));
  finally
    ComboBox_UserLogonName.Items.EndUpdate;
    ComboBox_UserLogonName.ItemIndex := 0;
  end;

  Edit_PreWindowsPrefix.Text := FormatUtf8('%/', [Ldap.NetbiosDN]);
  OwnerNewObject.CallBack := @Load;
end;

end.

