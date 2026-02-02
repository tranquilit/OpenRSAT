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
  mormot.core.base,
  mormot.core.text,
  mormot.core.log,
  uldapconfigs;

type

  { TFormConnectConfigs }

  TFormConnectConfigs = class(TForm)
    Action_OK: TAction;
    Action_Cancel: TAction;
    Action_ProfileManager: TAction;
    ActionList1: TActionList;
    BitBtn_OK: TBitBtn;
    BitBtn_Cancel: TBitBtn;
    BitBtn_ProfileManager: TBitBtn;
    CheckBox_AutoConnect: TCheckBox;
    Edit_Server: TEdit;
    Edit_Username: TEdit;
    Edit_Password: TEdit;
    Image1: TImage;
    Image2: TImage;
    Label_AutoConnect: TLabel;
    Label_Profile: TLabel;
    Label_Server: TLabel;
    Label_Username: TLabel;
    Label_Password: TLabel;
    Panel_Profile: TPanel;
    Panel_Image: TPanel;
    Panel_Client: TPanel;
    Panel_Bottom: TPanel;
    TisSearchEdit_Profile: TTisSearchEdit;
    procedure Action_ProfileManagerExecute(Sender: TObject);
    procedure Action_OKExecute(Sender: TObject);
    procedure Action_OKUpdate(Sender: TObject);
    procedure CheckBox_AutoConnectChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TisSearchEdit_ProfileSearch(Sender: TObject; const aText: string);
    procedure TisSearchEdit_ProfileSelect(Sender: TObject);
  private
    fLog: ISynLog;
    fLdapConfigs: TLdapConfigs;

    procedure CreateDefaultConfig;
    function RetrieveConfigs(out LastConfig: RawUtf8): Integer;

  public
    constructor Create(TheOwner: TComponent; ALdapConfigs: TLdapConfigs; aLog: ISynLog = nil); reintroduce;
  end;

implementation

uses
  mormot.core.rtti,
  tisinifiles,
  uDarkStyleParams,
  uvisprofilemanager,
  uvisprofileconfiguration,
  ucoredatamodule,
  utranslation,
  ucommonui,
  uconfig;

{$R *.lfm}

{ TFormConnectConfigs }

procedure TFormConnectConfigs.Action_ProfileManagerExecute(Sender: TObject);
var
  ProfileManager: TVisProfileManager;
  LastConfig: RawUtf8;
begin
  ProfileManager := TVisProfileManager.Create(Self, fLdapConfigs, TisSearchEdit_Profile.Text);
  try
    if ProfileManager.ShowModal <> mrOK then
      Exit;

    RetrieveConfigs(LastConfig);
    if Assigned(ProfileManager.ListView1.Selected) then
    begin
      TisSearchEdit_Profile.Text := ProfileManager.ListView1.Selected.Caption;
      TisSearchEdit_Profile.OnSelect(TisSearchEdit_Profile);
    end;
  finally
    FreeAndNil(ProfileManager);
  end;
end;

procedure TFormConnectConfigs.Action_OKExecute(Sender: TObject);
begin
  fLdapConfigs.LdapConnectionSettings.TargetHost := Edit_Server.Text;
  fLdapConfigs.LdapConnectionSettings.password := Edit_Password.Text;
  fLdapConfigs.LdapConnectionSettings.userName := Edit_Username.Text;
  fLdapConfigs.SaveConfig(TisSearchEdit_Profile.Text);
end;

procedure TFormConnectConfigs.Action_OKUpdate(Sender: TObject);
begin
  Edit_Username.Enabled := fLdapConfigs.LdapConnectionSettings.UseCredentials;
  Edit_Password.Enabled := Edit_Username.Enabled;
  Action_OK.Enabled := (TisSearchEdit_Profile.Items.IndexOf(TisSearchEdit_Profile.Text) >= 0);
end;

procedure TFormConnectConfigs.CheckBox_AutoConnectChange(Sender: TObject);
begin
  fLdapConfigs.AutoConnect := CheckBox_AutoConnect.Checked;
end;

procedure TFormConnectConfigs.FormShow(Sender: TObject);
var
  index, ConfigCount: Integer;
  LastConfig: RawUtf8;
begin
  TisSearchEdit_Profile.Items.Clear;

  ConfigCount := RetrieveConfigs(LastConfig);

  if ConfigCount <= 0 then
    CreateDefaultConfig;

  index := TisSearchEdit_Profile.Items.IndexOf(TisSearchEdit_Profile.Text);
  if index < 0 then
    index := 0;
  TisSearchEdit_Profile.ItemIndex := index;
  TisSearchEdit_ProfileSelect(Sender);
  CheckBox_AutoConnect.Checked := fLdapConfigs.AutoConnect;

  UnifyButtonsWidth([BitBtn_Cancel, BitBtn_OK]);
end;

procedure TFormConnectConfigs.TisSearchEdit_ProfileSearch(Sender: TObject;
  const aText: string);
begin
  if TisSearchEdit_Profile.Items.IndexOf(aText) > 0 then
    TisSearchEdit_Profile.OnSelect(Sender);
end;

procedure TFormConnectConfigs.TisSearchEdit_ProfileSelect(Sender: TObject);
begin
  fLdapConfigs.LdapConnectionSettings.Password := '';
  fLdapConfigs.LoadConfig(TisSearchEdit_Profile.Text);

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
    BitBtn_ProfileManager.SetFocus;
end;

procedure TFormConnectConfigs.CreateDefaultConfig;
const
  DEFAULT_NAME = 'default';
var
  ASettings: TMLdapClientSettings;
  ProfileConfiguration: TVisProfileConfiguration;
begin
  TisSearchEdit_Profile.Items.Add(DEFAULT_NAME);
  TisSearchEdit_Profile.ItemIndex := TisSearchEdit_Profile.Items.IndexOf(DEFAULT_NAME);

  ASettings := TMLdapClientSettings.Create();
  try
    ProfileConfiguration := TVisProfileConfiguration.Create(Self, ASettings);
    try
      if (ProfileConfiguration.ShowModal <> mrOK) then
        Exit;
    finally
      FreeAndNil(ProfileConfiguration);
    end;
    fLdapConfigs.SaveConfig(DEFAULT_NAME, ASettings);
  finally
    FreeAndNil(ASettings);
  end;
end;

function TFormConnectConfigs.RetrieveConfigs(out LastConfig: RawUtf8): Integer;
var
  ini: TTisInifiles;
  sections: TRawUtf8DynArray;
  section: RawUtf8;
begin
  if Assigned(fLog) then
    fLog.Log(sllDebug, 'Retrieve config files from "%".', [ConfigFilePath]);
  result := 0;
  TisSearchEdit_Profile.Clear;
  TisSearchEdit_Profile.Items.BeginUpdate;
  ini := TTisIniFiles.Create(ConfigFilePath);
  try
    sections := TRawUtf8DynArray(ini.GetSections);
    result := Length(sections);
    LastConfig := fLdapConfigs.LastConfig;
    if LastConfig = '' then
      LastConfig := ini.ReadString('global', 'lastConfig', '');
    for section in sections do
    begin
      if (section = 'global') then
        continue;
      TisSearchEdit_Profile.Items.add(section);
    end;
    TisSearchEdit_Profile.Text := LastConfig;
  finally
    FreeAndNil(ini);
    TisSearchEdit_Profile.Items.EndUpdate;
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

  {$IFDEF WINDOWS}
  Image1.Visible := not IsDarkModeEnabled;
  Image2.Visible := IsDarkModeEnabled;
  {$ENDIF}
end;

end.

