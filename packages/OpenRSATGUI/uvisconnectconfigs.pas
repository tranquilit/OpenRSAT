unit uvisconnectconfigs;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  StdCtrls,
  FileUtil,
  LazFileUtils,
  Buttons,
  ActnList,
  ExtCtrls,
  tis.ui.searchedit,
  mormot.core.log,
  uldapconfigs;

type

  { TFormConnectConfigs }

  TFormConnectConfigs = class(TForm)
    Action_AddConfig: TAction;
    Action_DeleteConfig: TAction;
    Action_OK: TAction;
    Action_Cancel: TAction;
    Action_EditConfig: TAction;
    ActionList1: TActionList;
    BitBtn_DeleteConfig: TBitBtn;
    BitBtn_OK: TBitBtn;
    BitBtn_Cancel: TBitBtn;
    BitBtn_EditConfig: TBitBtn;
    CheckBox_AutoConnect: TCheckBox;
    Edit_Server: TEdit;
    Edit_Username: TEdit;
    Edit_Password: TEdit;
    Image1: TImage;
    Image2: TImage;
    Label1: TLabel;
    Label_Config: TLabel;
    Label_Server: TLabel;
    Label_Username: TLabel;
    Label_Password: TLabel;
    Panel1: TPanel;
    Panel_Config: TPanel;
    Panel_Image: TPanel;
    Panel_Client: TPanel;
    Panel_Bottom: TPanel;
    TisSearchEdit_Configs: TTisSearchEdit;
    procedure Action_EditConfigExecute(Sender: TObject);
    procedure Action_OKExecute(Sender: TObject);
    procedure Action_OKUpdate(Sender: TObject);
    procedure CheckBox_AutoConnectChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TisSearchEdit_ConfigsSelect(Sender: TObject);
  private
    fLog: ISynLog;
    fLdapConfigs: TLdapConfigs;

    procedure CreateDefaultConfig;
    function RetrieveConfigs(out LastConfig: String): Integer;

  public
    constructor Create(TheOwner: TComponent; ALdapConfigs: TLdapConfigs; aLog: ISynLog = nil); reintroduce;
  end;

implementation

uses
  mormot.core.base,
  mormot.core.text,
  mormot.core.rtti,
  tisinifiles,
  uDarkStyleParams,
  uvisconnectoptions,
  ucoredatamodule,
  uresourcestring,
  utranslation,
  ucommonui,
  uconfig;

{$R *.lfm}

{ TFormConnectConfigs }

procedure TFormConnectConfigs.Action_EditConfigExecute(Sender: TObject);
var
  connectOptions: TVisConnectOptions;
begin
  connectOptions := TVisConnectOptions.Create(Self);
  try
    connectOptions.Settings := fLdapConfigs.LdapConnectionSettings;
    if (connectOptions.ShowModal <> mrOK) then
      Exit;

    TisSearchEdit_Configs.Items.Add(TisSearchEdit_Configs.Text);
    fLdapConfigs.SaveConfig(TisSearchEdit_Configs.Text, fLdapConfigs.LdapConnectionSettings);

    Edit_UserName.Enabled := fLdapConfigs.LdapConnectionSettings.UseCredentials;
    Edit_Password.Enabled := fLdapConfigs.LdapConnectionSettings.UseCredentials;

    if fLdapConfigs.LdapConnectionSettings.UseCredentials then
    begin
      Edit_Username.text := connectOptions.Edit_User.Text;
      if Edit_Password.Text = '' then
        Edit_Password.SetFocus;

      Edit_Password.text := connectOptions.Edit_Password.Text;
      if Edit_Username.Text = '' then
        Edit_Username.SetFocus;
    end;

    Edit_Server.Text := connectOptions.Edit_DomainController.Text;
    if Edit_Server.Text = '' then
      BitBtn_EditConfig.SetFocus;
  finally
    FreeAndNil(connectOptions);
  end;
end;

procedure TFormConnectConfigs.Action_OKExecute(Sender: TObject);
begin
  fLdapConfigs.LdapConnectionSettings.TargetHost := Edit_Server.Text;
  fLdapConfigs.LdapConnectionSettings.password := Edit_Password.Text;
  fLdapConfigs.LdapConnectionSettings.userName := Edit_Username.Text;
  fLdapConfigs.SaveConfig(TisSearchEdit_Configs.Text);
end;

procedure TFormConnectConfigs.Action_OKUpdate(Sender: TObject);
begin
  Edit_Username.Enabled := fLdapConfigs.LdapConnectionSettings.UseCredentials;
  Edit_Password.Enabled := Edit_Username.Enabled;
  Action_OK.Enabled := (TisSearchEdit_Configs.Items.IndexOf(TisSearchEdit_Configs.Text) >= 0);
end;

procedure TFormConnectConfigs.CheckBox_AutoConnectChange(Sender: TObject);
begin
  fLdapConfigs.AutoConnect := CheckBox_AutoConnect.Checked;
end;

procedure TFormConnectConfigs.FormShow(Sender: TObject);
var
  index, ConfigCount: Integer;
  LastConfig: String;
begin
  TisSearchEdit_Configs.Items.Clear;

  ConfigCount := RetrieveConfigs(LastConfig);

  if ConfigCount <= 0 then
    CreateDefaultConfig;

  index := TisSearchEdit_Configs.Items.IndexOf(TisSearchEdit_Configs.Text);
  if index < 0 then
    index := 0;
  TisSearchEdit_Configs.ItemIndex := index;
  TisSearchEdit_ConfigsSelect(Sender);
  CheckBox_AutoConnect.Checked := fLdapConfigs.AutoConnect;

  UnifyButtonsWidth([BitBtn_Cancel, BitBtn_OK]);
end;

procedure TFormConnectConfigs.TisSearchEdit_ConfigsSelect(Sender: TObject);
begin
  fLdapConfigs.LdapConnectionSettings.Password := '';
  fLdapConfigs.LoadConfig(TisSearchEdit_Configs.Text);

  if Assigned(fLog) then
    fLog.Log(sllDebug, 'Config selected: %', [ConfigFilePath]);

  Edit_Password.Enabled := True;
  Edit_Username.Enabled := True;
  Edit_Server.Text := fLdapConfigs.LdapConnectionSettings.TargetHost;
  if fLdapConfigs.LdapConnectionSettings.UseCredentials then
  begin
    Edit_Username.Text := fLdapConfigs.LdapConnectionSettings.UserName;
    Edit_Password.Text := fLdapConfigs.LdapConnectionSettings.password;
    if Edit_Password.Text = '' then
      Edit_Password.SetFocus;
    if Edit_Username.Text = '' then
      Edit_Username.SetFocus;
  end
  else
  begin
    Edit_Username.Text := '';
    Edit_Password.Text := '';
    Edit_Username.Enabled := False;
    Edit_Password.Enabled := False;
  end;
  if  Edit_Server.Text = '' then
    BitBtn_EditConfig.SetFocus;
end;

procedure TFormConnectConfigs.CreateDefaultConfig;
const
  DEFAULT_NAME = 'default';
var
  connectOptions: TVisConnectOptions;
begin
  TisSearchEdit_Configs.Items.Add(DEFAULT_NAME);
  TisSearchEdit_Configs.ItemIndex := TisSearchEdit_Configs.Items.IndexOf(DEFAULT_NAME);

  connectOptions := TVisConnectOptions.Create(Self);
  try
    connectOptions.Settings := fLdapConfigs.LdapConnectionSettings;
    if (connectOptions.ShowModal <> mrOK) then
      Exit;
    fLdapConfigs.SaveConfig(DEFAULT_NAME);
  finally
    FreeAndNil(connectOptions);
  end;
end;

function TFormConnectConfigs.RetrieveConfigs(out LastConfig: String): Integer;
var
  ini: TTisInifiles;
  sections: TStringArray;
  section: String;
begin
  if Assigned(fLog) then
    fLog.Log(sllDebug, 'Retrieve config files from "%".', [ConfigFilePath]);
  result := 0;
  TisSearchEdit_Configs.Items.BeginUpdate;
  ini := TTisIniFiles.Create(ConfigFilePath);
  try
    sections := ini.GetSections;
    result := Length(sections);
    LastConfig := fLdapConfigs.LastConfig;
    if LastConfig = '' then
      LastConfig := ini.ReadString('global', 'lastConfig', '');
    for section in sections do
    begin
      if (section = 'global') then
        continue;
      TisSearchEdit_Configs.Items.add(section);
    end;
    TisSearchEdit_Configs.Text := LastConfig;
  finally
    FreeAndNil(ini);
    TisSearchEdit_Configs.Items.EndUpdate;
  end;
end;

constructor TFormConnectConfigs.Create(TheOwner: TComponent;
  ALdapConfigs: TLdapConfigs; aLog: ISynLog);
begin
  inherited Create(TheOwner);

  fLog := aLog;
  fLdapConfigs := ALdapConfigs;

  if Assigned(fLog) then
    fLog.Log(sllInfo, 'Vis Connect Config');

  Label_Config.Caption := rsConfiguration;
  Label_Server.Caption := rsServer;
  Label_Username.Caption := rsUsername;
  Label_Password.Caption := rsPassword;
  Self.Caption := rsVisConnectConfigsTitle;
  CheckBox_AutoConnect.Caption := rsAutoConnect;
  Action_Cancel.Caption := rsCancel;
  Action_OK.Caption := rsOK;
  {$IFDEF WINDOWS}
  Image1.Visible := not IsDarkModeEnabled;
  Image2.Visible := IsDarkModeEnabled;
  {$ENDIF}
end;

end.

