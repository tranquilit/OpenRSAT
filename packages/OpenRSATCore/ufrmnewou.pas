unit ufrmnewou;

{$mode ObjFPC}{$H+}

interface

uses
  // Lazarus / fpc
  ActnList,
  Classes,
  Forms,
  StdCtrls;

type

  { TFrmNewOU }

  TFrmNewOU = class(TFrame)
    CheckBox_Protected: TCheckBox;
    Edit_Name: TEdit;
    Label_Name: TLabel;
    ActionList: TActionList;
      Action_Next: TAction;
    procedure Action_NextExecute(Sender: TObject);
    procedure Action_NextUpdate(Sender: TObject);
    procedure Load;
  private
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
  mormot.core.base,
  mormot.core.os.security,
  mormot.core.text,
  mormot.net.ldap,
  // Rsat
  uvisnewobject,
  ucommon,
  ucoredatamodule;
{$R *.lfm}

{ TFrmNewOU }

//todo: protect against accidental deletion
procedure TFrmNewOU.Action_NextExecute(Sender: TObject);
var
  NewObject: TVisNewObject;
  Att: TLdapAttributeList;
  DN: RawUtf8;
  SecDesc: TSecurityDescriptor;
  data: TLdapAttribute;

begin
  NewObject := (Owner as TVisNewObject);

  DN := FormatUtf8('OU=%,%', [Edit_Name.Text, NewObject.ObjectOU]);
  Att := TLdapAttributeList.Create();
  try
    Att.Add('objectClass', 'top').Add('organizationalUnit');
    if not NewObject.Ldap.Add(DN, Att) then
    begin
      Dialogs.MessageDlg(rsLdapError, FormatUtf8(rsLdapAddFailed, [NewObject.Ldap.ResultString]), mtError, [mbOK], 0);
      Exit;
    end;
  finally
    FreeAndNil(Att);
  end;

  // Protect against accidental deletion
  if not CheckBox_Protected.Checked then
  begin
    NewObject.ModalResult := mrOK;
    Exit;
  end;

  // Get SecDesc
  data := NewObject.Ldap.SearchObject(atNTSecurityDescriptor, DN, '');
  if not Assigned(data) then
  begin
    Dialogs.MessageDlg(rsLdapError, (owner as TVisNewObject).Ldap.ResultString, mtError, [mbOK], 0);
    Exit;
  end;
  if not SecDesc.FromBinary(data.GetRaw()) then
  begin
    Dialogs.MessageDlg(rsTitleParsing, rsACEParsing, mtError, [mbOK], 0);
    Exit;
  end;

  if not ProtectAgainstAccidentalDeletion(NewObject.Ldap, DN, (owner as TVisNewObject).BaseDN, SecDesc, True) then
    Exit;

  if not NewObject.Ldap.Modify(DN, lmoReplace, atNTSecurityDescriptor, SecDesc.ToBinary()) then // Modify
  begin
    Dialogs.MessageDlg(rsLdapError, NewObject.Ldap.ResultString, mtError, [mbOK], 0);
    Exit;
  end;

  NewObject.ModalResult := mrOK;
end;

procedure TFrmNewOU.Action_NextUpdate(Sender: TObject);
begin
  Action_Next.Enabled := Edit_Name.Text <> '' ;
end;

procedure TFrmNewOU.Load;
begin
  Edit_Name.SetFocus;
end;

constructor TFrmNewOU.Create(TheOwner: TComponent);
var
  OwnerNewObject: TVisNewObject absolute TheOwner;
begin
  inherited Create(TheOwner);

  OwnerNewObject.Caption := rsNewObjectOU;
  OwnerNewObject.Btn_Next.Action := ActionList.ActionByName('Action_Next');
  OwnerNewObject.Btn_Next.Caption := rsNewObjectBtnOK;
  OwnerNewObject.Btn_Next.Default := True;
  OwnerNewObject.Btn_Back.Visible := False;
  OwnerNewObject.Image_Object.ImageIndex := Ord(ileADOU);
  OwnerNewObject.CallBack := @Load;
end;

end.
