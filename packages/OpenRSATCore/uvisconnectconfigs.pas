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
    Action_OK: TAction;
    Action_Cancel: TAction;
    Action_EditConfig: TAction;
    ActionList1: TActionList;
    BitBtn_OK: TBitBtn;
    BitBtn_Cancel: TBitBtn;
    BitBtn_EditConfig: TBitBtn;
    CheckBox_AutoConnect: TCheckBox;
    Edit_Server: TEdit;
    Edit_Username: TEdit;
    Edit_Password: TEdit;
    Label_Config: TLabel;
    Label_Server: TLabel;
    Label_Username: TLabel;
    Label_Password: TLabel;
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

  public
    constructor Create(TheOwner: TComponent; ALdapConfigs: TLdapConfigs; aLog: ISynLog = nil); overload;
  end;

implementation

uses
  mormot.core.base,
  mormot.core.text,
  mormot.core.rtti,
  tisinifiles,
  uvisconnectoptions,
  ucoredatamodule,
  uresourcestring,
  utranslation;
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
  index: Integer;
  ini: TTisInifiles;
  sections: TStringArray;
  lastConfig, section: String;
  connectOptions: TVisConnectOptions;
begin
  TisSearchEdit_Configs.Items.BeginUpdate;
  try
    TisSearchEdit_Configs.Items.Clear;
    if Assigned(fLog) then
      fLog.Log(sllDebug, 'Retrieve config files from "' + CoreDataModule.ConfigFilePath + '".');
    ini := TTisIniFiles.Create(CoreDataModule.ConfigFilePath);
    try
      sections := ini.GetSections;
      lastConfig := fLdapConfigs.LastConfig;
      if lastConfig = '' then
        lastConfig := ini.ReadString('global', 'lastConfig', '');
      for section in sections do
      begin
        if (section = 'global') then
          continue;
        if (lastConfig = section) then
          TisSearchEdit_Configs.Text := lastConfig;
        TisSearchEdit_Configs.Items.add(section);
      end;
    finally
      FreeAndNil(ini);
    end;
    // No configs
    if TisSearchEdit_Configs.Items.Count <= 0 then
    begin
      TisSearchEdit_Configs.Items.Add('default');
      connectOptions := TVisConnectOptions.Create(Self);
      try
        connectOptions.Settings := fLdapConfigs.LdapConnectionSettings;
        connectOptions.Action_Domain.Execute;
        connectOptions.Action_OK.Execute;
      finally
        FreeAndNil(connectOptions);
      end;
      fLdapConfigs.SaveConfig('default');
      BitBtn_OK.Click;
    end;
  finally
    TisSearchEdit_Configs.Items.EndUpdate;
  end;

  index := TisSearchEdit_Configs.Items.IndexOf(TisSearchEdit_Configs.Text);
  if index < 0 then
    index := 0;
  TisSearchEdit_Configs.ItemIndex := index;
  TisSearchEdit_ConfigsSelect(Sender);
  CheckBox_AutoConnect.Checked := fLdapConfigs.AutoConnect;
  //UnifyButtonsWidth([BitBtn_Cancel, BitBtn_OK]);
end;

procedure TFormConnectConfigs.TisSearchEdit_ConfigsSelect(Sender: TObject);
begin
  fLdapConfigs.LdapConnectionSettings.Password := '';
  fLdapConfigs.LoadConfig(TisSearchEdit_Configs.Text);

  if Assigned(fLog) then
    fLog.Log(sllDebug, 'Config selected: %', [CoreDataModule.ConfigFilePath]);

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
end;

end.

