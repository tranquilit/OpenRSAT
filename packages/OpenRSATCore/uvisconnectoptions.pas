unit uvisconnectoptions;

{$mode ObjFPC}{$H+}

interface

uses
  ActnList,
  Buttons,
  Forms,
  ComCtrls,
  Classes,
  ExtCtrls,
  StdCtrls,
  tis.ui.searchedit,
  mormot.core.base,
  mormot.net.ldap,
  mormot.crypt.core,
  Controls,
  uldapconfigs;

type

  { TVisConnectOptions }

  TVisConnectOptions = class(TForm)
    Action_Check: TAction;
    Action_OK: TAction;
    Action_Cancel: TAction;
    Action_Domain: TAction;
    Action_DomainController: TAction;
    ActionList1: TActionList;
    BitBtn_RefreshDomain: TBitBtn;
    BitBtn_RefreshDC: TBitBtn;
    cbIgnoreCertErrors: TCheckBox;
    CheckBox_TLS: TCheckBox;
    CheckBox_SASL: TCheckBox;
    Label_DC: TLabel;
    PageControl: TPanel;
    Panel1: TPanel;
    RadioButton_SimpleAuth: TRadioButton;
    RadioButton_GSSAPI: TRadioButton;
        Group_Connection: TGroupBox;
          Label_TargetHost: TLabel;
          Edit_Domain: TTisSearchEdit;
          Label_Port: TLabel;
          Edit_Port: TEdit;
        Group_Account: TGroupBox;
          Label_User: TLabel;
          Edit_User: TEdit;
          Label_Password: TLabel;
          Edit_Password: TEdit;
        CheckBox_Anonyme: TCheckBox;
    Panel_Bottom: TPanel;
      Button_Check: TBitBtn;
      Btn_OK: TBitBtn;
      Btn_Cancel: TBitBtn;
      Edit_DomainController: TTisSearchEdit;
    procedure Action_CheckExecute(Sender: TObject);
    procedure Action_DomainControllerExecute(Sender: TObject);
    procedure Action_DomainExecute(Sender: TObject);
    procedure Action_OKExecute(Sender: TObject);
    procedure CheckBox_TLSChange(Sender: TObject);
    procedure CheckBox_AccountChange(Sender: TObject);
    procedure CheckBox_AnonymeChange(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure RadioButtonChange(Sender: TObject);
  private
    fSettings: TMLdapClientSettings;

    procedure GetSettings(AValue: TMLdapClientSettings);
    procedure SetSettings(AValue: TMLdapClientSettings);
  public
    constructor Create(TheOwner: TComponent); override;

    property Settings: TMLdapClientSettings write SetSettings;
  end;

implementation
uses
  Dialogs,
  SysUtils,
  mormot.core.text,
  mormot.net.dns,
  mormot.net.sock,
  ursatldapclient,
  ucommon;

{$R *.lfm}

constructor TVisConnectOptions.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

// Account
procedure TVisConnectOptions.CheckBox_AccountChange(Sender: TObject);
begin
  Group_Account.Enabled := (Sender as TCheckBox).Checked
end;

procedure TVisConnectOptions.CheckBox_AnonymeChange(Sender: TObject);
begin
  Edit_Password.Enabled := not CheckBox_Anonyme.Checked;
  Edit_User.Enabled := not CheckBox_Anonyme.Checked;
end;

procedure TVisConnectOptions.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    27: Close;
  end;
end;

procedure TVisConnectOptions.RadioButtonChange(Sender: TObject);
begin
  if (RadioButton_SimpleAuth.Checked) then
  begin
    CheckBox_Anonyme.Caption := rsAnonymousConnection;
    CheckBox_SASL.Enabled := False;
  end
  else
  begin
    CheckBox_Anonyme.Caption := rsUseCurrentCreds;
    CheckBox_SASL.Enabled := True;
  end;
end;

procedure TVisConnectOptions.GetSettings(AValue: TMLdapClientSettings);
begin
  AValue.TargetHost := Edit_DomainController.Text;
  AValue.TargetPort := Edit_Port.Text;
  AValue.KerberosDN := Edit_Domain.Text;

  AValue.AllowUnsafePasswordBind := cbIgnoreCertErrors.Checked;
  AValue.KerberosDisableChannelBinding := not CheckBox_SASL.Checked;
  AValue.Tls := CheckBox_TLS.Checked;
  AValue.UseCredentials := not CheckBox_Anonyme.Checked;

  if RadioButton_GSSAPI.Checked then
    AValue.AutoBind := lcbKerberos
  else
    AValue.AutoBind := lcbPlain;
  //AValue.AllowUnsafePasswordBind := (AValue.AutoBind = lcbPlain) and (not AValue.Tls);
  if AValue.UseCredentials then
  begin
    AValue.UserName := Edit_User.Text;
    AValue.Password := Edit_Password.Text;
  end
  else
  begin
    AValue.UserName := '';
    AValue.Password := '';
  end;
end;

procedure TVisConnectOptions.SetSettings(AValue: TMLdapClientSettings);
begin
  fSettings := AValue;

  Edit_DomainController.Caption := fSettings.TargetHost;
  Edit_Domain.Text := fSettings.KerberosDN;
  Edit_Port.Text := fSettings.TargetPort;

  RadioButton_GSSAPI.Checked := fSettings.AutoBind = lcbKerberos;
  RadioButton_SimpleAuth.Checked := fSettings.AutoBind <> lcbKerberos;

  CheckBox_TLS.Checked := fSettings.Tls;
  CheckBox_SASL.Checked := not fSettings.KerberosDisableChannelBinding;
  cbIgnoreCertErrors.Checked := fSettings.AllowUnsafePasswordBind;

  Edit_User.Text := fSettings.UserName;
  Edit_Password.Text := fSettings.Password;
  RadioButtonChange(Self);
  CheckBox_Anonyme.Checked := not fSettings.UseCredentials;
end;

procedure TVisConnectOptions.CheckBox_TLSChange(Sender: TObject);
begin
  if CheckBox_TLS.Checked then
    Edit_Port.Text := '636'
  else
    Edit_Port.Text := '389';
end;

procedure TVisConnectOptions.Action_CheckExecute(Sender: TObject);
var
  fLdap: TLdapClient;
  ASettings: TMLdapClientSettings;
begin
  ASettings := TMLdapClientSettings.Create();
  try
    GetSettings(ASettings);
    fLdap := TLdapClient.Create(ASettings);
    try
      fLdap.TlsContext^.IgnoreCertificateErrors := fLdap.Settings.AllowUnsafePasswordBind;
      if not fLdap.Connect() then
      begin
        ShowLdapConnectError(fLdap);
        Exit;
      end;
      MessageDlg(rsTitleConnectSuccess, FormatUtf8(rsConnectSuccess, [fLdap.Settings.TargetUri]) + LineEnding +
        FormatUtf8(rsConnectedOnDomainAs, [fLdap.BoundUser]) + LineEnding +
        FormatUtf8(rsConnectedOnDomainWithDC, [fLdap.DefaultDN()]),
      mtInformation, [mbOK], 0);
      fLdap.Close;
    finally
      FreeAndNil(fLdap);
    end;
  finally
    FreeAndNil(ASettings);
  end;
end;

procedure TVisConnectOptions.Action_DomainControllerExecute(Sender: TObject);
var
  PreviousDC: String;
  hostnames: TRawUtf8DynArray;
  dc: RawUtf8;
begin
  PreviousDC := Edit_DomainController.Caption;
  Edit_DomainController.Items.Clear;
  Edit_DomainController.Caption := '';

  hostnames := DnsLdapServices(Edit_Domain.Caption);

  // Sort the DC
  if Length(hostnames) > 0 then
  begin
    Screen.Cursor := crHourGlass;
    try
      CldapSortHosts(hostnames, 3000, MaxInt);
    finally
      Screen.Cursor := crDefault;
    end;

    for dc in hostnames do
      Edit_DomainController.Items.Add(String(dc).Split(':')[0]);
    Edit_DomainController.ItemIndex := 0;
  end
  else
  begin
    Edit_DomainController.Caption := PreviousDC;
  end;

end;

procedure TVisConnectOptions.Action_DomainExecute(Sender: TObject);
var
  Servers: TCldapServers;
  domains: TRawUtf8DynArray;
  PreviousDomain, domainName: String;
  srv: TCldapServer;
begin
  PreviousDomain := Edit_Domain.Caption;
  Edit_Domain.Clear;
  Edit_Domain.Caption := '';
  Edit_DomainController.Clear;
  Edit_DomainController.Caption := '';
  Servers := [];

  /// Local domain
  domains := GetDomainNames;
  if Length(domains) > 0 then
    Edit_Domain.items.Add(domains[0]);

  // Broadcast domains
  Screen.Cursor := crHourGlass;
  try
    CldapBroadcast(Servers, 3000);
    CldapBroadcast(Servers, 1000, cLocalHost);
  finally
    Screen.Cursor := crDefault;
  end;

  for srv in Servers do
  begin
    domainName := String(srv.ServiceName).Split(':')[0];
    if Edit_Domain.Items.IndexOf(domainName) = -1 then
      Edit_Domain.items.Add(domainName);
  end;

  // Set current domain to first one
  if Edit_Domain.Items.Count > 0 then
  begin
    Edit_Domain.ItemIndex := 0;
    if Edit_Domain.Items.Count > 1 then
      Edit_Domain.DroppedDown := True;
    Action_DomainController.Execute;
  end else
  begin
    Edit_Domain.Caption := PreviousDomain;
  end;
end;

procedure TVisConnectOptions.Action_OKExecute(Sender: TObject);
begin
  GetSettings(fSettings);
end;

end.
