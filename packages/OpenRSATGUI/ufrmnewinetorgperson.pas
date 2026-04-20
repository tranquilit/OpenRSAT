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
    Label_Password: TLabel;
    Label_Confirm: TLabel;
    Label_PreWindows: TLabel;
    Label_UserLogonName: TLabel;
    Label_FullName: TLabel;
    Label_LastName: TLabel;
    Label_Initials: TLabel;
    Label_FirstName: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    procedure Action_BackExecute(Sender: TObject);
    procedure Action_BackUpdate(Sender: TObject);
    procedure Action_NextExecute(Sender: TObject);
    procedure Action_NextUpdate(Sender: TObject);
    procedure CheckBox_CannotChangeClick(Sender: TObject);
    procedure CheckBox_NeverExpiresClick(Sender: TObject);
    procedure CheckBox_NextLogonClick(Sender: TObject);
    procedure Edit_FirstNameChange(Sender: TObject);
    procedure Edit_InitialsChange(Sender: TObject);
    procedure Edit_LastNameChange(Sender: TObject);
    procedure Edit_UserLogonNameChange(Sender: TObject); 
  private
    PageID: Integer;
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
  mormot.net.ldap,
  ucommon,
  ucoredatamodule,
  ursatldapclientui,
  uvisnewobject;

{$R *.lfm}

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
  if PageID = 0 then
  begin
    PageID += 1;
    Panel1.Visible := False;
    Panel2.Visible := False;
    Panel3.Visible := False;
    Panel4.Visible := True;
    Panel5.Visible := True;
  end
  else if PageID = 1 then
  begin
    if Edit_Password.Text <> Edit_Confirm.Text then
    begin
      ShowMessage('The passwords do not match.');
      Exit
    end;
    PageID += 1;
    Panel4.Visible := False;
    Panel5.Visible := False;
  end;
end;

procedure TFrmNewInetOrgPerson.Action_BackExecute(Sender: TObject);
begin
  PageID -= 1;
  if PageID = 0 then
  begin
    Panel1.Visible := True;
    Panel2.Visible := True;
    Panel3.Visible := True;
    Panel4.Visible := False;
    Panel5.Visible := False;
  end
  else if PageID = 1 then
  begin
    Panel4.Visible := True;
    Panel5.Visible := True;
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
    Action_Next.Enabled := True;
end;

procedure TFrmNewInetOrgPerson.CheckBox_CannotChangeClick(Sender: TObject);
begin
  if CheckBox_NextLogon.Checked then
  begin
    CheckBox_CannotChange.Checked := False;
    ShowMessage('You cannot check both User must change password at next logon and User cannot change password for the same user.');
  end;
end;

procedure TFrmNewInetOrgPerson.CheckBox_NeverExpiresClick(Sender: TObject);
begin
  if CheckBox_NextLogon.Checked then
  begin
    CheckBox_NextLogon.Checked := False;
    ShowMessage('You specified that the password should never expire.' + LineEnding + 'The user will not be required to change the password at next logon.');
  end 
end;

procedure TFrmNewInetOrgPerson.CheckBox_NextLogonClick(Sender: TObject);
begin
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

procedure TFrmNewInetOrgPerson.Load;
begin
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

