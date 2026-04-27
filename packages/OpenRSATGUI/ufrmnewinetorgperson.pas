unit ufrmnewinetorgperson;

{$mode ObjFPC}{$H+}

interface

uses
  ActnList,
  Classes,
  ExtCtrls,
  Forms,
  StdCtrls;

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
    procedure Action_BackExecute(Sender: TObject);
    procedure Action_BackUpdate(Sender: TObject);
    procedure Action_NextExecute(Sender: TObject);
    procedure Action_NextUpdate(Sender: TObject);
    procedure CheckBox_CannotChangeClick(Sender: TObject);
    procedure CheckBox_NeverExpiresClick(Sender: TObject);
    procedure CheckBox_NextLogonChange(Sender: TObject);
    procedure Edit_FirstNameChange(Sender: TObject);
    procedure Edit_InitialsChange(Sender: TObject);
    procedure Edit_LastNameChange(Sender: TObject);
    procedure Edit_UserLogonNameChange(Sender: TObject); 
  private
    PageID: Integer;
    procedure BuildRecapProperties;
    procedure BtnOK;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

implementation

uses
  Controls,
  Dialogs,
  SysUtils,
  mormot.core.text,
  mormot.net.ldap,
  ucommon,
  ucoredatamodule,
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
  NewObject: TVisNewObject;
  AttList: TLdapAttributeList;
  Att: TLdapAttribute;
  DN: String;
  UAC: TUserAccountControls = [uacNormalAccount]; 
begin
  NewObject := (owner as TVisNewObject);
  AttList := TLdapAttributeList.Create();
  try
    Att := AttList.Add(atObjectClass, 'top');
    Att.Add('person');
    Att.Add('organizationalPerson');
    Att.Add('user');
    Att.Add('inetOrgPerson');
    
    AttList.Add(atDisplayName, Edit_FullName.Text);
    if Edit_FirstName.Text <> '' then
       AttList.Add(atGivenName, Edit_FirstName.Text);
    if Edit_Initials.Text <> '' then
       AttList.Add(atInitials, Edit_Initials.Text);
    if Edit_LastName.Text <> '' then
       AttList.Add(atSurname, Edit_LastName.Text);
    AttList.Add(atUserPrincipalName, FormatUtf8('%%', [Edit_UserLogonName.Text, ComboBox_UserLogonName.Text]));
    AttList.Add(atSamAccountName, Edit_UserLogonName.Text);
    
    AttList.AddUnicodePwd(Edit_Password.Text);
    if CheckBox_NextLogon.Checked then
      AttList.Add(atPwdLastSet, '0');
    if CheckBox_NeverExpires.Checked then
      Include(UAC, uacPasswordDoNotExpire);
    if CheckBox_CannotChange.Checked then
      Include(UAC, uacPasswordCannotChange);
    if CheckBox_AccountDisabled.Checked then
      Include(UAC, uacAccountDisable);
    if UAC <> [] then
      AttList.Add(atUserAccountControl, UserAccountControlsValue(UAC).ToString());
    
    DN := FormatUtf8('CN=%,%', [Edit_FullName.Text, NewObject.ObjectOU]);
    if not NewObject.Ldap.Add(DN, AttList) then
      Exit;
  finally
    FreeAndNil(Att);
  end;
  NewObject.ModalResult := mrOK;    
end;

procedure TFrmNewInetOrgPerson.Edit_FirstNameChange(Sender: TObject);
var
  FullName: String;
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
      Panel1.Visible := False;
      Panel2.Visible := False;
      Panel3.Visible := False;
      Panel4.Visible := True;
      Panel5.Visible := True;
    end;
    1:
    begin
      if Edit_Password.Text <> Edit_Confirm.Text then
      begin
        ShowMessage('The passwords do not match.');
        Exit
      end;
      PageID += 1;
      BuildRecapProperties;
      Panel4.Visible := False;
      Panel5.Visible := False;
      Panel6.Visible := True; 
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
      Panel1.Visible := True;
      Panel2.Visible := True;
      Panel3.Visible := True;
      Panel4.Visible := False;
      Panel5.Visible := False;
    end;
    1:
    begin
      Panel4.Visible := True;
      Panel5.Visible := True;
      Panel6.Visible := False;
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
    Action_Next.Enabled := True
end;

procedure TFrmNewInetOrgPerson.CheckBox_CannotChangeClick(Sender: TObject);
begin
  if not CheckBox_CannotChange.Checked then
    exit;
  
  if CheckBox_NextLogon.Checked then
  begin
    CheckBox_CannotChange.Checked := False;
    ShowMessage('You specified that the password should never expire.' + LineEnding + 'The user will not be required to change the password at next logon.');
  end;
end;

procedure TFrmNewInetOrgPerson.CheckBox_NeverExpiresClick(Sender: TObject);
begin
  if CheckBox_NextLogon.Checked then
  begin
    CheckBox_NextLogon.Checked := False;
    ShowMessage('You cannot check both User must change password at next logon and User cannot change password for the same user.');
  end;
end;

procedure TFrmNewInetOrgPerson.CheckBox_NextLogonChange(Sender: TObject);
begin
  if not CheckBox_NextLogon.Checked then
     exit;
  
  if CheckBox_NeverExpires.Checked then
  begin
    CheckBox_NextLogon.Checked := False;
    ShowMessage('You specified that the password should never expire.' + LineEnding + 'The user will not be required to change the password at next logon.');
  end
  else if CheckBox_CannotChange.Checked then
  begin
    CheckBox_CannotChange.Checked := False;
    ShowMessage('You cannot check both User must change password at next logon and User cannot change password for the same user.');
  end;
end;

procedure TFrmNewInetOrgPerson.Edit_InitialsChange(Sender: TObject);
var
  FullName: String;
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
  FullName: String;
begin
  if Edit_FirstName.Text <> '' then
    FullName := Edit_FirstName.Text;
  if Edit_Initials.Text <> '' then
    FullName := FormatUtf8('% %.', [FullName, Edit_Initials.Text]);
  FullName := FormatUtf8('% %', [FullName, Edit_LastName.Text]);
  Edit_FullName.Text := FullName;
end;

procedure TFrmNewInetOrgPerson.Edit_UserLogonNameChange(Sender: TObject);
begin
  Edit_PreWindowsSuffix.Text := Edit_UserLogonName.Text
end;

constructor TFrmNewInetOrgPerson.Create(TheOwner: TComponent);
var
  OwnerNewObject: TVisNewObject absolute TheOwner;
  function UserLogonFormatDN(Text, Separator: String): string;
  var
    Position: Integer;
  begin
    result := '';
    Position := Pos(Separator, Text);
    if Position > 0 then
      result := Copy(Text, 1, Position - 1)
    else
      result := Text;
  end;
begin
  inherited Create(TheOwner);
  
  OwnerNewObject.Caption := rsNewObjectInetOrgPerson;
  OwnerNewObject.Btn_Next.Action := ActionList.ActionByName('Action_Next'); 
  OwnerNewObject.Btn_Next.Caption := rsNewObjectBtnNext;
  OwnerNewObject.Btn_Back.Action := ActionList.ActionByName('Action_Back');
  OwnerNewObject.Btn_Back.Caption := rsNewObjectBtnBack;
  PageID := 0;
  ComboBox_UserLogonName.Items.Add(FormatUtf8('@%', [UserLogonFormatDN(OwnerNewObject.Edit_DN.Text, '/')]));
  ComboBox_UserLogonName.ItemIndex := 0;
  Edit_PreWindowsPrefix.Text := FormatUtf8('%/', [UpperCase(UserLogonFormatDN(OwnerNewObject.Edit_DN.Text, '.lan'))]);
end;

end.

