unit uvisprofileconfiguration;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  StdCtrls,
  Buttons,
  ExtCtrls,
  ButtonPanel,
  ActnList,
  uldapconfigs,
  mormot.core.base;

type

  { TVisProfileConfiguration }

  TVisProfileConfiguration = class(TForm)
    Action_ShowDCInfos: TAction;
    Action_TestConnection: TAction;
    Action_TestAuthentication: TAction;
    Action_SearchDomains: TAction;
    Action_SearchDomainControllers: TAction;
    ActionList1: TActionList;
    BitBtn_DCInfos: TBitBtn;
    BitBtn_SearchDomains: TBitBtn;
    BitBtn_SearchDomainController: TBitBtn;
    BitBtn_TestConnection: TBitBtn;
    BitBtn_TestAuthentication: TBitBtn;
    ButtonPanel1: TButtonPanel;
    CheckBox_CurrentUsername: TCheckBox;
    CheckBox_TLS: TCheckBox;
    CheckBox_Unsafe: TCheckBox;
    CheckBox_ChannelBinding: TCheckBox;
    ComboBox_Domains: TComboBox;
    ComboBox_DomainControllers: TComboBox;
    ComboBox_Algorithm: TComboBox;
    Edit_Timeout: TEdit;
    Edit_Username: TEdit;
    Edit_Password: TEdit;
    Edit_UsernameDomain: TEdit;
    GroupBox_Domain: TGroupBox;
    GroupBox_Account: TGroupBox;
    GroupBox_AuthMethods: TGroupBox;
    Label_CurrentUsername: TLabel;
    Label_Domain: TLabel;
    Label_Unsafe: TLabel;
    Label_Algorithm: TLabel;
    Label_ChannelBinding: TLabel;
    Label_Username: TLabel;
    Label_Password: TLabel;
    Label_at: TLabel;
    Label_DomainController: TLabel;
    Label_TLS: TLabel;
    Label_Timeout: TLabel;
    Label_Anonymous: TLabel;
    Label_Simple: TLabel;
    Label_Digest: TLabel;
    Label_Kerberos: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel_AuthMethodSelectorInner: TPanel;
    Panel_Kerberos: TPanel;
    Panel_Digest: TPanel;
    Panel_AuthMethodSelector: TPanel;
    RadioButton_Anonymous: TRadioButton;
    RadioButton_Simple: TRadioButton;
    RadioButton_Digest: TRadioButton;
    RadioButton_Kerberos: TRadioButton;
    procedure Action_SearchDomainControllersExecute(Sender: TObject);
    procedure Action_SearchDomainsExecute(Sender: TObject);
    procedure Action_ShowDCInfosExecute(Sender: TObject);
    procedure Action_TestAuthenticationExecute(Sender: TObject);
    procedure Action_TestConnectionExecute(Sender: TObject);
    procedure CheckBox_CurrentUsernameChange(Sender: TObject);
    procedure ComboBox_DomainsEditingDone(Sender: TObject);
    procedure ComboBox_DomainsSelect(Sender: TObject);
    procedure Edit_UsernameKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure RadioButton_AnonymousChange(Sender: TObject);
    procedure RadioButton_DigestChange(Sender: TObject);
    procedure RadioButton_KerberosChange(Sender: TObject);
    procedure RadioButton_SimpleChange(Sender: TObject);
  private
    fSettings: TMLdapClientSettings;

    procedure UpdateAuthMethodsPanel;
    procedure SettingsToGUI(ASettings: TMLdapClientSettings = nil);
    procedure GUIToSettings(ASettings: TMLdapClientSettings = nil);
  public
    constructor Create(TheOwner: TComponent; ASettings: TMLdapClientSettings); reintroduce;

    property Settings: TMLdapClientSettings read fSettings;
  end;

implementation
uses
  mormot.core.log,
  mormot.core.text,
  mormot.net.dns,
  mormot.net.ldap,
  mormot.net.sock,
  mormot.crypt.secure,
  ucommon,
  ucoredatamodule,
  uvisrootdseinfos,
  ursatldapclient,
  ursatldapclientui;

{$R *.lfm}

{ TVisProfileConfiguration }

procedure TVisProfileConfiguration.CheckBox_CurrentUsernameChange(
  Sender: TObject);
begin
  UpdateAuthMethodsPanel;
end;

procedure TVisProfileConfiguration.ComboBox_DomainsEditingDone(Sender: TObject);
begin
  Edit_UsernameDomain.Text := ComboBox_Domains.Text;
end;

procedure TVisProfileConfiguration.ComboBox_DomainsSelect(Sender: TObject);
begin
  Edit_UsernameDomain.Text := ComboBox_Domains.Text;
end;

procedure TVisProfileConfiguration.Edit_UsernameKeyPress(Sender: TObject;
  var Key: char);
begin
  if (Key = '@') then
  begin
    Key := Char(0);
    Edit_UsernameDomain.SetFocus;
  end;
end;

procedure TVisProfileConfiguration.Action_SearchDomainsExecute(Sender: TObject);
var
  BroadcastDomains: TCldapServers;
  LocalDomains: TRawUtf8DynArray;
  LocalDomain, BakDomain, DomainName: RawUtf8;
  BroadcastDomain: TCldapServer;
begin
  try
    Screen.Cursor := crHourGlass;
    BakDomain := ComboBox_Domains.Text;
    ComboBox_Domains.Clear;
    ComboBox_Domains.Text := '';
    ComboBox_Domains.Items.BeginUpdate;

    // Local domains
    LocalDomains := GetDomainNames;

    // Broadcast domains
    CldapBroadcast(BroadcastDomains, 3000);
    CldapBroadcast(BroadcastDomains, 1000, cLocalHost);

    for LocalDomain in LocalDomains do
      ComboBox_Domains.Items.Add(LocalDomain);

    for BroadcastDomain in BroadcastDomains do
    begin
      DomainName := String(BroadcastDomain.ServiceName).Split(':')[0];
      if ComboBox_Domains.Items.IndexOf(DomainName) = -1 then
        ComboBox_Domains.Items.Add(DomainName);
    end;

    if ComboBox_Domains.Items.Count > 0 then
    begin
      ComboBox_Domains.ItemIndex := 0;
      Edit_UsernameDomain.Text := ComboBox_Domains.Text;
      if ComboBox_Domains.Items.Count > 1 then
        ComboBox_Domains.DroppedDown := True
      else
        Action_SearchDomainControllers.Execute;
    end
    else
      ComboBox_Domains.Text := BakDomain;
  finally
    ComboBox_Domains.Items.EndUpdate;
    Screen.Cursor := crDefault;
  end;
end;

procedure TVisProfileConfiguration.Action_ShowDCInfosExecute(Sender: TObject);
var
  VisRootDSEInfos: TVisRootDSEInfos;
begin
  VisRootDSEInfos := TVisRootDSEInfos.Create(Self, fSettings);
  try
    VisRootDSEInfos.ShowModal;
  finally
    FreeAndNil(VisRootDSEInfos);
  end;
end;

procedure TVisProfileConfiguration.Action_TestAuthenticationExecute(
  Sender: TObject);
var
  ASettings: TMLdapClientSettings;
  LdapTest: TRsatLdapClient;
begin
  try
    ASettings := TMLdapClientSettings.Create();
    GUIToSettings(ASettings);
    LdapTest := TRsatLdapClient.Create(ASettings);
    LdapTest.TlsContext^.IgnoreCertificateErrors := LdapTest.Settings.AllowUnsafePasswordBind;
    if LdapTest.Connect() then
      MessageDlg(rsLdapSuccess, FormatUtf8(rsLdapSuccessAuthMessage, [LdapTest.BoundUser]), mtConfirmation, [mbOK], 0)
    else
      ShowLdapConnectError(LdapTest);
  finally
    FreeAndNil(LdapTest);
    FreeAndNil(ASettings);
  end;
end;

procedure TVisProfileConfiguration.Action_TestConnectionExecute(Sender: TObject
  );
var
  ASettings: TMLdapClientSettings;
  LdapTest: TRsatLdapClient;
  res: Boolean;
begin
  try
    ASettings := TMLdapClientSettings.Create();
    GUIToSettings(ASettings);
    LdapTest := TRsatLdapClient.Create(ASettings);
    LdapTest.Settings.AutoBind := lcbNone;
    res := LdapTest.Connect();
    if not LdapTest.Settings.Tls then
      MessageDlg(rsLdapWarning, FormatUtf8(rsLdapMissingTLS, []), mtWarning, [mbOK], 0)
    else if LdapTest.Settings.Tls and (LdapTest.TlsContext^.LastError = 'not verified') then
      MessageDlg(rsLdapWarning, FormatUtf8(rsLdapTLSInvalidCert, []), mtWarning, [mbOK], 0);

      if res then
        MessageDlg(rsLdapSuccess, FormatUtf8(rsLdapSuccessConnMessage, [LdapTest.Settings.KerberosDN, LdapTest.Settings.TargetHost, LdapTest.Settings.TargetPort]), mtConfirmation, [mbOK], 0)
      else
        ShowLdapConnectError(LdapTest);
  finally
    FreeAndNil(LdapTest);
    FreeAndNil(ASettings);
  end;
end;

procedure TVisProfileConfiguration.Action_SearchDomainControllersExecute(
  Sender: TObject);
var
  BakDomainController, Hostname: RawUtf8;
  Hostnames: TRawUtf8DynArray;
begin
  try
    Screen.Cursor := crHourGlass;
    BakDomainController := ComboBox_DomainControllers.Text;
    ComboBox_DomainControllers.Clear;
    ComboBox_DomainControllers.Text := '';
    ComboBox_DomainControllers.Items.BeginUpdate;

    Hostnames := DnsLdapServices(ComboBox_Domains.Text);

    if Length(Hostnames) > 0 then
    begin
      CldapSortHosts(Hostnames, 3000, MaxInt);

      for Hostname in Hostnames do
        ComboBox_DomainControllers.Items.Add(String(Hostname).Split(':')[0]);
      ComboBox_DomainControllers.ItemIndex := 0;
      if ComboBox_DomainControllers.Items.Count > 1 then
        ComboBox_DomainControllers.DroppedDown := True;
    end
    else
      ComboBox_DomainControllers.Text := BakDomainController;
  finally
    ComboBox_DomainControllers.Items.EndUpdate;
    Screen.Cursor := crDefault;
  end;
end;

procedure TVisProfileConfiguration.FormShow(Sender: TObject);
var
  DigestAlgo: TDigestAlgo;
begin
  ComboBox_Algorithm.Items.BeginUpdate;
  try
    ComboBox_Algorithm.Items.Add('');
    for DigestAlgo := daMD5 to High(TDigestAlgo) do
      ComboBox_Algorithm.Items.Add(DIGEST_NAME[DigestAlgo]);
  finally
    ComboBox_Algorithm.Items.EndUpdate;
  end;
  UpdateAuthMethodsPanel;
  SettingsToGUI(Settings);
end;

procedure TVisProfileConfiguration.OKButtonClick(Sender: TObject);
begin
  GUIToSettings(Settings);
end;

procedure TVisProfileConfiguration.RadioButton_AnonymousChange(Sender: TObject);
begin
  UpdateAuthMethodsPanel;
end;

procedure TVisProfileConfiguration.RadioButton_DigestChange(Sender: TObject);
begin
  UpdateAuthMethodsPanel;
end;

procedure TVisProfileConfiguration.RadioButton_KerberosChange(Sender: TObject);
begin
  UpdateAuthMethodsPanel;
end;

procedure TVisProfileConfiguration.RadioButton_SimpleChange(Sender: TObject);
begin
  UpdateAuthMethodsPanel;
end;

procedure TVisProfileConfiguration.UpdateAuthMethodsPanel;
begin
  Panel_Digest.Visible := RadioButton_Digest.Checked;
  Panel_Kerberos.Visible := RadioButton_Kerberos.Checked;
  GroupBox_Account.Visible := RadioButton_Digest.Checked or RadioButton_Simple.Checked or (RadioButton_Kerberos.Checked and not CheckBox_CurrentUsername.Checked);
  AutoSize := True;
  AutoSize := False;
end;

procedure TVisProfileConfiguration.SettingsToGUI(ASettings: TMLdapClientSettings
  );
begin
  if not Assigned(ASettings) then
    ASettings := Settings;
  if not Assigned(ASettings) then
    raise Exception.Create('No settings to load to GUI.');

  ComboBox_Domains.Text := ASettings.KerberosDN;
  if (ASettings.Tls and (ASettings.TargetPort <> LDAP_TLS_PORT) and (not ASettings.Tls and (ASettings.TargetPort <> LDAP_PORT))) then
    ComboBox_DomainControllers.Text := FormatUtf8('%:%', [ASettings.TargetHost, ASettings.TargetPort])
  else
    ComboBox_DomainControllers.Text := ASettings.TargetHost;

  CheckBox_TLS.Checked := ASettings.Tls;
  Edit_Timeout.Text := IntToStr(ASettings.Timeout);

  RadioButton_Anonymous.Checked := False;
  RadioButton_Simple.Checked := (ASettings.AutoBind = lcbPlain);
  RadioButton_Digest.Checked := (ASettings.AutoBind = lcbDigest);
  RadioButton_Kerberos.Checked := (ASettings.AutoBind = lcbKerberos) or (ASettings.AutoBind = lcbNone);

  CheckBox_Unsafe.Checked := ASettings.AllowUnsafePasswordBind;
  CheckBox_ChannelBinding.Checked := not ASettings.KerberosDisableChannelBinding;
  CheckBox_CurrentUsername.Checked := not ASettings.UseCredentials;

  if (Pos('@', ASettings.UserName) > 0) then
  begin
    Edit_Username.Text := String(ASettings.UserName).Substring(0, Pos('@', ASettings.UserName) - 1);
    Edit_UsernameDomain.Text := String(ASettings.UserName).Substring(Pos('@', ASettings.UserName));
  end
  else
    Edit_Username.Text := ASettings.UserName;
  Edit_Password.Text := ASettings.Password;
end;

procedure TVisProfileConfiguration.GUIToSettings(ASettings: TMLdapClientSettings
  );
var
  Timeout: Longint;
begin
  if not Assigned(ASettings) then
    ASettings := Settings;
  if not Assigned(ASettings) then
    raise Exception.Create('No settings to update from GUI.');

  ASettings.Tls := CheckBox_TLS.Checked;
  ASettings.KerberosDN := ComboBox_Domains.Text;
  if (Pos(':', ComboBox_DomainControllers.Text) > 0) then
  begin
    ASettings.TargetHost := String(ComboBox_DomainControllers.Text).Substring(0, Pos(':', ComboBox_DomainControllers.Text) - 1);
    ASettings.TargetPort := String(ComboBox_DomainControllers.Text).Substring(Pos(':', ComboBox_DomainControllers.Text));
  end
  else
  begin
    ASettings.TargetHost := ComboBox_DomainControllers.Text;
    if ASettings.Tls then
      ASettings.TargetPort := LDAP_TLS_PORT
    else
      ASettings.TargetPort := LDAP_PORT;
  end;

  if not TryStrToInt(Edit_Timeout.Text, Timeout) then
    Timeout := 5000;

  ASettings.Timeout := Timeout;
  if RadioButton_Kerberos.Checked then
    ASettings.AutoBind := lcbKerberos
  else if RadioButton_Simple.Checked then
    ASettings.AutoBind := lcbPlain
  else
    ASettings.AutoBind := lcbNone;

  ASettings.AllowUnsafePasswordBind := CheckBox_Unsafe.Checked;
  ASettings.KerberosDisableChannelBinding := not CheckBox_ChannelBinding.Checked;
  ASettings.UseCredentials := not CheckBox_CurrentUsername.Checked;
  if (RadioButton_Kerberos.Checked and CheckBox_CurrentUsername.Checked) then
  begin
    ASettings.UseCredentials := False;
    ASettings.UserName := '';
    ASettings.Password := '';
    Exit;
  end;
  ASettings.UseCredentials := True;
  if Edit_UsernameDomain.Text <> '' then
    ASettings.UserName := FormatUtf8('%@%', [Edit_Username.Text, Edit_UsernameDomain.Text])
  else
    ASettings.UserName := Edit_Username.Text;
  ASettings.Password := Edit_Password.Text;
end;

constructor TVisProfileConfiguration.Create(TheOwner: TComponent;
  ASettings: TMLdapClientSettings);
begin
  inherited Create(TheOwner);

  fSettings := ASettings;

  ActionList1.Images := CoreDataModule.ImageList1;
end;

end.

