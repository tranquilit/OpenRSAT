unit ufrmnewcontact;

{$mode ObjFPC}{$H+}

interface

uses
  // Lazarus / fpc
  ActnList,
  Controls,
  Classes,
  Forms,
  StdCtrls;

type

  { TFrmNewContact }

  TFrmNewContact = class(TFrame)
    ActionList1: TActionList;
    Action_Next: TAction;
    Edit_DisplayName: TEdit;
    Edit_FullName: TEdit;
    Edit_LastName: TEdit;
    Edit_FirstName: TEdit;
    Edit_Initial: TEdit;
    Label_DisplayName: TLabel;
    Label_FirstName: TLabel;
    Label_FullName: TLabel;
    Label_Initial: TLabel;
    Label_LastName: TLabel;
    procedure Action_NextExecute(Sender: TObject);
    procedure Action_NextUpdate(Sender: TObject);
    procedure Edit_ContactChange(Sender: TObject);
  private

    procedure Load;
  public
    constructor Create(TheOwner: TComponent); override;

  end;

implementation
uses
  // Lazarus / fpc
  Dialogs,
  SysUtils,
  // Submodules
  mormot.core.text,
  mormot.core.base,
  mormot.net.ldap,
  // Rsat
  ucommon,
  ursatldapclient,
  uvisnewobject;
{$R *.lfm}

{ TFrmNewContact }

procedure TFrmNewContact.Action_NextExecute(Sender: TObject);
var
  VisNewObject: TVisNewObject;
  Attr: TLdapAttributeList;
  Att: TLdapAttribute;
begin
  VisNewObject := (owner as TVisNewObject);

  Attr := TLdapAttributeList.Create();
  try
    Att := Attr.Add(atObjectClass, 'top'); // objectClass
    Att.Add('person');
    Att.Add('organizationalPerson');
    Att.Add('contact');

    if (Edit_FirstName.Text <> '') then // givenName
      Attr.Add(atGivenName, Edit_FirstName.Text);

    if (Edit_LastName.Text <> '') then // sn
      Attr.Add(atSurName, Edit_LastName.Text);

    if (Edit_DisplayName.Text <> '') then // displayName
      Attr.Add(atDisplayName, Edit_DisplayName.Text);

    if (Edit_Initial.Text <> '') then // initials
      Attr.Add(atInitials, Edit_Initial.Text);

    if not VisNewObject.Ldap.Add('CN=' + Edit_FullName.Text + ',' + VisNewObject.ObjectOU, Attr) then
    begin
      ShowLdapAddError(VisNewObject.Ldap);
      Exit;
    end;
    VisNewObject.ModalResult := mrOK;
  finally
    FreeAndNil(Attr);
  end;
end;

procedure TFrmNewContact.Action_NextUpdate(Sender: TObject);
begin
  Action_Next.Enabled := Edit_FullName.Text <> '';
end;

procedure TFrmNewContact.Edit_ContactChange(Sender: TObject);
var
  i1, i2: String;
  values: Array of String;
begin
  // Initials
  i1 := '';
  i2 := '';
  if Edit_FirstName.Text <> '' then
    i1 := String.UpperCase(Copy(Edit_FirstName.Text, 1, 1));
  if Edit_LastName.Text <> '' then
    i2 := String.UpperCase(Copy(Edit_LastName.Text, 1, 1));
  Edit_Initial.Text := i1 + i2;

  // FullName
  values := [];
  if Edit_FirstName.Text <> '' then
    Insert(Edit_FirstName.Text, values, Length(values));
  if Edit_Initial.Text <> '' then
    Insert(Edit_Initial.Text + '.', values, Length(values));
  if Edit_LastName.Text <> '' then
    Insert(Edit_LastName.Text, values, Length(values));
  Edit_FullName.Text := String.Join(' ', values);

  // DisplayName
  values := [];
  if Edit_FirstName.Text <> '' then
    Insert(Edit_FirstName.Text, values, Length(values));
  if Edit_LastName.Text <> '' then
    Insert(Edit_LastName.Text, values, Length(values));
  Edit_DisplayName.Text := String.Join(' ', values);
end;

procedure TFrmNewContact.Load;
begin
  Edit_FirstName.SetFocus;
end;

constructor TFrmNewContact.Create(TheOwner: TComponent);
var
  OwnerNewObject: TVisNewObject absolute TheOwner;
begin
  inherited Create(TheOwner);

  OwnerNewObject.Caption := rsNewObjectContact;
  OwnerNewObject.Btn_Next.Action := ActionList1.ActionByName('Action_Next');
  OwnerNewObject.Btn_Next.Caption := rsNewObjectBtnOK;
  OwnerNewObject.Btn_Next.Default := True;
  OwnerNewObject.Btn_Back.Visible := False;
  OwnerNewObject.Image_Object.ImageIndex := -1{Im_Contact};
  OwnerNewObject.CallBack := @Load;
end;

end.

