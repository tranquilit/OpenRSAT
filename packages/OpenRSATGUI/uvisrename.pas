unit uvisrename;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ComCtrls,
  ExtCtrls,
  StdCtrls,
  Buttons,
  ActnList,
  mormot.core.base,
  mormot.core.text,
  mormot.net.ldap;

type

  { TVisRename }

  TVisRename = class(TForm)
    Action_OK: TAction;
    ActionList1: TActionList;
    BitBtn_OK: TBitBtn;
    BitBtn_Cancel: TBitBtn;
    ComboBox_UserLogonName: TComboBox;
    Edit_FullName: TEdit;
    Edit_FirstName: TEdit;
    Edit_LastName: TEdit;
    Edit_DisplayName: TEdit;
    Edit_UserLogonName: TEdit;
    Edit_UserLogonName2000_prefix: TEdit;
    Edit_UserLogonName2000_suffix: TEdit;
    Label_FullName: TLabel;
    Label_FirstName: TLabel;
    Label_LastName: TLabel;
    Label_DisplayName: TLabel;
    Label_UserLogonName: TLabel;
    Label_UserLogonName2000: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel_FullName: TPanel;
    Panel_DisplayName: TPanel;
    Panel_LastName: TPanel;
    Panel_FirstName: TPanel;
    Panel_UserLogonName: TPanel;
    Panel_UserLogonName2000: TPanel;
    TabSheet1: TTabSheet;
    procedure Action_OKExecute(Sender: TObject);
  private
    function GetDisplayName: RawUtf8;
    function GetFirstName: RawUtf8;
    function GetFullName: RawUtf8;
    function GetLastName: RawUtf8;
    function GetSAMAccountName: RawUtf8;
    function GetUserPrincipalName: RawUtf8;
    procedure SetDisplayName(AValue: RawUtf8);
    procedure SetFirstName(AValue: RawUtf8);
    procedure SetFullName(AValue: RawUtf8);
    procedure SetLastName(AValue: RawUtf8);
    procedure SetSAMAccountName(AValue: RawUtf8);
    procedure SetUserPrincipalName(AValue: RawUtf8);
  private
    fDistinguishedName: RawUtf8;
    fLdapClient: TLdapClient;
    fNewName: RawUtf8;

    procedure SetDistinguishedName(AValue: RawUtf8);
    procedure SetLdapClient(AValue: TLdapClient);
    procedure RefreshValues;
    function RenameDistinguishedName(const LdapClient: TLdapClient; const DistinguishedName, NewName: RawUtf8): RawUtf8;

    property FullName: RawUtf8 read GetFullName write SetFullName;
    property DisplayName: RawUtf8 read GetDisplayName write SetDisplayName;
    property LastName: RawUtf8 read GetLastName write SetLastName;
    property FirstName: RawUtf8 read GetFirstName write SetFirstName;
    property SAMAccountName: RawUtf8 read GetSAMAccountName write SetSAMAccountName;
    property UserPrincipalName: RawUtf8 read GetUserPrincipalName write SetUserPrincipalName;
  public

    property NewName: RawUtf8 read fNewName write fNewName;
    property DistinguishedName: RawUtf8 read fDistinguishedName write SetDistinguishedName;
    property LdapClient: TLdapClient read fLdapClient write SetLdapClient;
  end;

implementation

{$R *.lfm}

{ TVisRename }

procedure TVisRename.SetLdapClient(AValue: TLdapClient);
begin
  if fLdapClient = AValue then
    Exit;
  fLdapClient := AValue;

  RefreshValues;
end;

procedure TVisRename.Action_OKExecute(Sender: TObject);
var
  Attributes: TLdapAttributeList;
begin
  Attributes := TLdapAttributeList.Create;
  Attributes.Add('displayName', DisplayName);
  Attributes.Add('sn', LastName);
  Attributes.Add('givenName', FirstName);
  Attributes.Add('userPrincipalName', UserPrincipalName);
  Attributes.Add('sAMAccountName', SAMAccountName);

  if not LdapClient.Modify(DistinguishedName, lmoReplace, Attributes) then
    Exit;

  DistinguishedName := RenameDistinguishedName(LdapClient, DistinguishedName, FullName);
end;

function TVisRename.GetDisplayName: RawUtf8;
begin
  result := Edit_DisplayName.Text;
end;

function TVisRename.GetFirstName: RawUtf8;
begin
  result := Edit_FirstName.Text;
end;

function TVisRename.GetFullName: RawUtf8;
begin
  result := Edit_FullName.Text;
end;

function TVisRename.GetLastName: RawUtf8;
begin
  result := Edit_LastName.Text;
end;

function TVisRename.GetSAMAccountName: RawUtf8;
begin
  result := Edit_UserLogonName2000_suffix.Text;
end;

function TVisRename.GetUserPrincipalName: RawUtf8;
begin
  result := FormatUtf8('%%', [Edit_UserLogonName.Text, ComboBox_UserLogonName.Text]);
end;

procedure TVisRename.SetDisplayName(AValue: RawUtf8);
begin
  Edit_DisplayName.Text := AValue;
end;

procedure TVisRename.SetFirstName(AValue: RawUtf8);
begin
  Edit_FirstName.Text := AValue;
end;

procedure TVisRename.SetFullName(AValue: RawUtf8);
begin
  Edit_FullName.Text := AValue;
end;

procedure TVisRename.SetLastName(AValue: RawUtf8);
begin
  Edit_LastName.Text := AValue;
end;

procedure TVisRename.SetSAMAccountName(AValue: RawUtf8);
begin
  Edit_UserLogonName2000_suffix.Text := AValue;
end;

procedure TVisRename.SetUserPrincipalName(AValue: RawUtf8);
var
  Splitted: TStringArray;
begin
  Splitted := String(AValue).Split('@');

  if Length(Splitted) <> 2 then
    Exit;

  Edit_UserLogonName.Text := Splitted[0];
  ComboBox_UserLogonName.Text := FormatUtf8('@%', [Splitted[1]]);
end;

procedure TVisRename.SetDistinguishedName(AValue: RawUtf8);
begin
  if fDistinguishedName = AValue then
    Exit;
  fDistinguishedName := AValue;

  RefreshValues;
end;

procedure TVisRename.RefreshValues;
var
  LdapObject: TLdapResult;
begin
  if not Assigned(LdapClient) or (DistinguishedName = '') then
    Exit;

  LdapObject := LdapClient.SearchObject(FormatUtf8('CN=Partitions,%', [LdapClient.ConfigDN]), '', ['uPNSuffixes']);
  if not Assigned(LdapObject) then
    Exit;

  ComboBox_UserLogonName.Clear;
  ComboBox_UserLogonName.Items.Add('@' + DNToCN(LdapClient.DefaultDN));
  if LdapClient.DefaultDN <> LdapClient.RootDN then
    ComboBox_UserLogonName.Items.Add('@' + DNToCN(LdapClient.RootDN));
  ComboBox_UserLogonName.Items.AddStrings(TStringArray(LdapObject.Find('uPNSuffixes').GetAllReadable));

  LdapObject := LdapClient.SearchObject(DistinguishedName, '', ['distinguishedName', 'name', 'displayName', 'sn', 'givenName', 'userPrincipalName', 'sAMAccountName']);
  if not Assigned(LdapObject) then
    Exit;

  DisplayName := LdapObject.Find('displayName').GetReadable();
  FirstName := LdapObject.Find('givenName').GetReadable();
  if NewName <> '' then
    FullName := NewName
  else
    FullName := LdapObject.Find('name').GetReadable();
  LastName := LdapObject.Find('sn').GetReadable();
  SAMAccountName := LdapObject.Find('sAMAccountName').GetReadable();
  UserPrincipalName := LdapObject.Find('userPrincipalName').GetReadable();
  Edit_UserLogonName2000_prefix.Text := FormatUtf8('%\', [LdapClient.NetbiosDN]);
end;

function TVisRename.RenameDistinguishedName(const LdapClient: TLdapClient;
  const DistinguishedName, NewName: RawUtf8): RawUtf8;
var
  Pairs: TNameValueDNs;
  NewRdn, NewSuperior: RawUtf8;
  DeleteOldRdn: boolean;
  i: Integer;
begin
  result := '';
  NewSuperior := '';
  DeleteOldRdn := True;

  if not ParseDN(DistinguishedName, Pairs) then
    Exit;

  Pairs[0].Value := NewName;
  NewRdn := FormatUtf8('%=%', [Pairs[0].Name, Pairs[0].Value]);

  if not LdapClient.ModifyDN(DistinguishedName, NewRdn, NewSuperior, DeleteOldRdn) then
    Exit;

  result := NewRdn;
  for i := 1 to High(Pairs) do
    result := FormatUtf8('%,%=%', [result, Pairs[i].Name, Pairs[i].Value]);
end;

end.

