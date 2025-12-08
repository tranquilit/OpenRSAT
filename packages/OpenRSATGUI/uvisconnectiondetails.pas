unit uvisconnectiondetails;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs, StdCtrls, ExtCtrls, Grids, ComCtrls, Buttons,
  mormot.net.ldap, tis.ui.grid.core;

type

  { TVisConnectionDetails }

  TVisConnectionDetails = class(TForm)
    BitBtn1: TBitBtn;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    ComboBox1: TComboBox;
    Edit_Username: TEdit;
    Edit_Password: TEdit;
    Edit_Timeout: TEdit;
    Edit_KerberosSPN: TEdit;
    Edit_Domain: TEdit;
    Edit_DomainController: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label_Tls: TLabel;
    Label_AllowUnsafePasswordBind: TLabel;
    Label_KerberosDisableChannelBinding: TLabel;
    Label_AutoReconnect: TLabel;
    Label_AutoBind: TLabel;
    Label_Domain: TLabel;
    Label_DomainController: TLabel;
    Label_Username: TLabel;
    Label_Password: TLabel;
    Label_Timeout: TLabel;
    Label_KerberosSPN: TLabel;
    Memo1: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    ScrollBox1: TScrollBox;
    TabSheet_Formatted: TTabSheet;
    TabSheet_Advanced: TTabSheet;
    TisGrid1: TTisGrid;
    procedure BitBtn1Click(Sender: TObject);
  private
    fLdapClient: TLdapClient;
    procedure SetLdapClient(AValue: TLdapClient);

    procedure ClearLdapInformation;
    procedure UpdateLdapInformation;
  public
    ShouldClose: Boolean;

    constructor Create(TheOwner: TComponent; ALdapClient: TLdapClient = nil); reintroduce;

    property LdapClient: TLdapClient read fLdapClient write SetLdapClient;
  end;

implementation
uses
  mormot.core.base,
  mormot.core.text,
  mormot.core.variants,
  ucommon,
  ursatldapclient;

{$R *.lfm}

{ TVisConnectionDetails }

procedure TVisConnectionDetails.BitBtn1Click(Sender: TObject);
begin
  if Edit_Password.EchoMode = emPassword then
    Edit_Password.EchoMode := emNormal
  else
    Edit_Password.EchoMode := emPassword;
end;

procedure TVisConnectionDetails.SetLdapClient(AValue: TLdapClient);
begin
  if fLdapClient = AValue then
    Exit;

  fLdapClient := AValue;
  UpdateLdapInformation;
end;

procedure TVisConnectionDetails.ClearLdapInformation;
begin
  Edit_Domain.Clear;
  Edit_DomainController.Clear;

  TisGrid1.Clear;

  Edit_Username.Clear;
  Edit_Password.Clear;
  Edit_Timeout.Clear;
  Edit_KerberosSPN.Clear;
  CheckBox1.Checked := False;
  CheckBox2.Checked := False;
  CheckBox3.Checked := False;
  CheckBox4.Checked := False;
  ComboBox1.ClearSelection;
end;

procedure TVisConnectionDetails.UpdateLdapInformation;
var
  RootDSEObject: TLdapResult;
  Item: TLdapAttribute;
  NewRow: TDocVariantData;
  Value: RawUtf8;
begin
  ClearLdapInformation;

  if not Assigned(LdapClient) then
    Exit;

  // Server info
  // Label_Domain and Label_Domain controller

  Edit_Domain.Text := LdapClient.Settings.KerberosDN;
  Edit_DomainController.Text := LdapClient.Settings.TargetHost;

  // Connection information (ldap settings)
  // Username
  // Password (hidden/visible)
  // Authentication type
  Edit_Username.Text := LdapClient.Settings.UserName;
  Edit_Password.Text := LdapClient.Settings.Password;
  Edit_Timeout.Text := IntToStr(LdapClient.Settings.Timeout);

  CheckBox1.Checked := LdapClient.Settings.Tls;
  CheckBox2.Checked := LdapClient.Settings.AllowUnsafePasswordBind;
  CheckBox3.Checked := LdapClient.Settings.KerberosDisableChannelBinding;
  CheckBox4.Checked := LdapClient.Settings.AutoReconnect;

  ComboBox1.ItemIndex := Ord(LdapClient.Settings.AutoBind);

  // Retrieve RootDSE information
  LdapClient.Settings.UserName := '';
  LdapClient.Settings.Password := '';
  LdapClient.Connect();

  RootDSEObject := LdapClient.SearchObject('', '', ['*']);

  if not Assigned(RootDSEObject) then
  begin
    ShowLdapConnectError(LdapClient);
    Panel5.Visible := True;
    Memo1.Text := LdapClient.ResultString;
    Exit;
  end;

  LdapClient.Settings.UserName := Edit_Username.Text;
  LdapClient.Settings.Password := Edit_Password.Text;

  NewRow.Init();
  for Item in RootDSEObject.Attributes.Items do
  begin
    if not Assigned(Item) then
      continue;

    for Value in Item.GetAllReadable do
    begin
      NewRow.AddValue('name', Item.AttributeName);
      NewRow.AddValue('value', Value);
      TisGrid1.Data.AddItem(NewRow);
      NewRow.Clear;
    end;
  end;
  TisGrid1.LoadData();

  LdapClient.Close;
  if (not LdapClient.Connect()) then
  begin
    ShowLdapConnectError(LdapClient);
    Panel5.Visible := True;
    Memo1.Text := LdapClient.ResultString;
  end
  else
  begin
    if mrOk <> MessageDlg(rsLdapSuccess, FormatUtf8(rsConnectSuccess, [LdapClient.Settings.TargetUri]), mtInformation, [mbClose, mbOK], 0) then
    begin
      ShouldClose := True;
      Exit;
    end;

  end;
  Edit_KerberosSPN.Text := LdapClient.Settings.KerberosSpn;
end;

constructor TVisConnectionDetails.Create(TheOwner: TComponent;
  ALdapClient: TLdapClient);
begin
  inherited Create(TheOwner);

  ShouldClose := False;
  Edit_Password.EchoMode := emPassword;
  LdapClient := ALdapClient;
end;

end.

