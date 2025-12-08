unit ufrmnewcomputer;

{$mode ObjFPC}{$H+}

interface

uses
  // Lazarus / fpc
  ActnList,
  Buttons,
  Classes,
  Controls,
  Forms,
  StdCtrls,
  // Submodules
  mormot.core.os.security;

type

  { TFrmNewComputer }

  TFrmNewComputer = class(TFrame)
    Action_Next: TAction;
    ActionList1: TActionList;
    BitBtn_ChangeUserOrGroup: TBitBtn;
    CheckBox_AssignComputer: TCheckBox;
    Edit_ComputerName: TEdit;
    Edit_ComputerName2000: TEdit;
    Edit_UserOrGroup: TEdit;
    Label_ComputerName: TLabel;
    Label_ComputerName2000: TLabel;
    Label1: TLabel;
    Label_UserOrGroup: TLabel;
    procedure Action_NextExecute(Sender: TObject);
    procedure Action_NextUpdate(Sender: TObject);
    procedure BitBtn_ChangeUserOrGroupClick(Sender: TObject);
    procedure Edit_ComputerNameChange(Sender: TObject);
  private
    Sid: RawSid;

    procedure Load;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

implementation

uses
  // Lazarus / fpc
  dialogs,
  SysUtils,
  // Submodules
  mormot.core.base,
  mormot.core.text,
  mormot.crypt.core,
  mormot.net.ldap,
  // Rsat
  uOmniselect,
  ucommon,
  ucoredatamodule,
  ursatldapclient,
  uvisnewobject;
{$R *.lfm}

{ TFrmNewComputer }
constructor TFrmNewComputer.Create(TheOwner: TComponent);
var
  OwnerNewObject: TVisNewObject absolute TheOwner;
begin
  inherited Create(TheOwner);

  Sid := '';
  OwnerNewObject.Caption := rsNewObjectComputer;
  OwnerNewObject.Btn_Next.Action := ActionList1.ActionByName('Action_Next');
  OwnerNewObject.Btn_Next.Caption := rsNewObjectBtnOK;
  OwnerNewObject.Btn_Next.Default := True;
  OwnerNewObject.Btn_Back.Visible := False;
  OwnerNewObject.Image_Object.ImageIndex := Ord(ileADComputer);
  OwnerNewObject.CallBack := @Load;
end;

procedure TFrmNewComputer.Action_NextUpdate(Sender: TObject);
begin
  Action_Next.Enabled := ((Edit_ComputerName.Text <> '') and (Edit_ComputerName.Text <> ''));
  if Action_Next.Enabled then
    (owner as TVisNewObject).Btn_Next.Default := True
  else
    (owner as TVisNewObject).Btn_Cancel.Default := True;
end;

procedure TFrmNewComputer.BitBtn_ChangeUserOrGroupClick(Sender: TObject);
var
  DNarr: TRawUtf8DynArray;
  Omniselect: TVisOmniselect;
  Attr: TLdapAttribute;
begin
  // Omniselect
  DNarr := [''];
  Omniselect := TVisOmniselect.Create(self, (owner as TVisNewObject).Ldap, ['user', 'group'], (Owner as TVisNewObject).BaseDN, False, '');
  try
    Omniselect.Caption := rsTitleSelectOwner;
    if Omniselect.ShowModal() <> mrOK then
      Exit;
    DNarr := Omniselect.SelectedObjects;
  finally
    FreeAndNil(Omniselect);
  end;

  Attr := (owner as TVisNewObject).Ldap.SearchObject(atObjectSid, DNarr[0], '');
  if not Assigned(Attr) then
  begin
    ShowLdapSearchError((owner as TVisNewObject).Ldap);
    Exit;
  end;
  Edit_UserOrGroup.Text := DNarr[0];
  Sid := Attr.GetRaw();
end;

procedure TFrmNewComputer.Edit_ComputerNameChange(Sender: TObject);
begin
  Edit_ComputerName2000.Text := UpperCase(Edit_ComputerName.Text);
end;

procedure TFrmNewComputer.Load;
begin
  Edit_ComputerName.SetFocus;
end;

procedure TFrmNewComputer.Action_NextExecute(Sender: TObject);
var
  VisNewObject: TVisNewObject;
  Attr: TLdapAttributeList;
  Att: TLdapAttribute;
  DN: RawUtf8;
  SecDesc: TSecurityDescriptor;
  ace: PSecAce;
begin
  VisNewObject := (owner as TVisNewObject);

  Attr := TLdapAttributeList.Create();
  try
    Att := Attr.Add(atObjectClass, 'top');
    Att.Add('person');
    Att.Add('organizationalPerson');
    Att.Add('user');
    Att.Add('computer');
    if (Edit_ComputerName2000.Text <> '') then
      Attr.Add(atSAMAccountName, Edit_ComputerName2000.Text + '$');

    Attr.UserAccountControl := [uacPasswordNotRequired, uacWorkstationTrusted];

    if (CheckBox_AssignComputer.Checked) then
      Attr.AddUnicodePwd(String(Edit_ComputerName2000.Text).ToLower)
    else
      Attr.AddUnicodePwd(TAesPrng.Main.RandomPassword(16));

    DN := FormatUtf8('CN=%,%', [Edit_ComputerName.Text, VisNewObject.ObjectOU]);
    if not VisNewObject.Ldap.Add(DN, Attr) then
    begin
      ShowLdapAddError(VisNewObject.Ldap);
      Exit;
    end;
  finally
    FreeAndNil(Attr);
  end;

  // Owner
  if Sid = '' then
  begin
    VisNewObject.ModalResult := mrOK;
    Exit;
  end;

  Att := VisNewObject.Ldap.SearchObject(atNTSecurityDescriptor, DN, '');
  if not Assigned(Att) then
  begin
    ShowLdapSearchError(VisNewObject.Ldap);
    Exit;
  end;

  SecDesc.FromBinary(Att.GetRaw());

  SecDescAddACE(@SecDesc, ATTR_UUID[kaNull],                    Sid, satAccessAllowed,
    [samListChildren, samReadProp, samDeleteTree, samListObject, samControlAccess, samDelete, samReadControl]);
  SecDescAddACE(@SecDesc, ATTR_UUID[kaUserAccountRestrictions], Sid, satObjectAccessAllowed, [samWriteProp]);
  SecDescAddACE(@SecDesc, ATTR_UUID[kaDnsHostName],             Sid, satObjectAccessAllowed, [samSelfWrite]);
  SecDescAddACE(@SecDesc, ATTR_UUID[kaServicePrincipalName],    Sid, satObjectAccessAllowed, [samSelfWrite]);
  ace := SecDescAddACE(@SecDesc, ATTR_UUID[kaSamAccountName],   Sid, satObjectAccessAllowed, [samWriteProp]);
  ace^.InheritedObjectType := ATTR_UUID[kaComputer];
  ace := SecDescAddACE(@SecDesc, ATTR_UUID[kaUserLogon],        Sid, satObjectAccessAllowed, [samWriteProp]);
  ace^.InheritedObjectType := ATTR_UUID[kaComputer];
  ace := SecDescAddACE(@SecDesc, ATTR_UUID[kaDescription],      Sid, satObjectAccessAllowed, [samWriteProp]);
  ace^.InheritedObjectType := ATTR_UUID[kaComputer];
  ace := SecDescAddACE(@SecDesc, ATTR_UUID[kaDisplayName],      Sid, satObjectAccessAllowed, [samWriteProp]);
  ace^.InheritedObjectType := ATTR_UUID[kaComputer];

  OrderAcl(VisNewObject.Ldap, DN, (owner as TVisNewObject).BaseDN, @SecDesc.Dacl);

  if not VisNewObject.Ldap.Modify(DN, lmoReplace, atNTSecurityDescriptor, SecDesc.ToBinary()) then
  begin
    ShowLdapModifyError(VisNewObject.Ldap);
    Exit;
  end;

  VisNewObject.ModalResult := mrOK;
end;

end.

