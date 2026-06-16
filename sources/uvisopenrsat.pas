unit uvisopenrsat;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  Menus,
  ComCtrls,
  ActnList,
  LResources,
  IniPropStorage,
  ExtCtrls,
  Translations,
  {$IFDEF WINDOWS} // Adds dark mode support, must be included after the LCL widgetset
    uDarkStyleParams,
    uMetaDarkStyle,
    uDarkStyleSchemes,
  {$ENDIF}
  mormot.core.base,
  mormot.core.log,
  mormot.net.ldap,
  uopenrsatuicontextinterface,
  ursat,
  utranslation,
  umoduleaduc,
  ursatldapclient,
  uVisOptions,
  uvisconnectconfigs,
  uvispropertieslist,
  uldapconfigs,
  ufrmmodule,
  ufrmmodules,
  ursatoption,
  ursatldapclientui,
  uoption,
  ulog;

type
  TOpenRSATArgs = record

  end;

  { TVisRsatConsole }

  { TVisOpenRSAT }

  TVisOpenRSAT = class(TForm, IOpenRSATUIContext)
    Action_Properties: TAction;
    Action_Settings: TAction;
    Action_LdapOptions: TAction;
    Action_LdapDisconnect: TAction;
    Action_LdapConnect: TAction;
    Action_AdvancedFeatures: TAction;
    Action_ShowGPO: TAction;
    Action_About: TAction;
    Action_NewWindow: TAction;
    Action_OpenedProperties: TAction;
    ActionList: TActionList;
    Action_Quit: TAction;
    IniPropStorage1: TIniPropStorage;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem_ShowGPO: TMenuItem;
    MenuItem_RetrieveOldConfiguration: TMenuItem;
    MenuItem_AdvancedFeatures: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem_DEBUG: TMenuItem;
    MenuItem_File: TMenuItem;
    MenuItem_FileOptions: TMenuItem;
    MenuItem_FileQuit: TMenuItem;
    MenuItem_Help: TMenuItem;
    MenuItem_LdapConnect: TMenuItem;
    MenuItem_LdapConnectOption: TMenuItem;
    MenuItem_LdapDisconnect: TMenuItem;
    MenuItem_View: TMenuItem;
    MenuItem_ViewTheme: TMenuItem;
    MenuItem_ViewThemeDark: TMenuItem;
    MenuItem_ViewThemeLight: TMenuItem;
    MenuItem_ViewThemeSystem: TMenuItem;
    MenuItem_ViewWindows: TMenuItem;
    PageControl1: TPageControl;
    Separator4: TMenuItem;
    Separator_File: TMenuItem;
    StatusBar1: TStatusBar;
    Timer_AutoConnect: TTimer;
    procedure Action_AboutExecute(Sender: TObject);
    procedure Action_AdvancedFeaturesExecute(Sender: TObject);
    procedure Action_AdvancedFeaturesUpdate(Sender: TObject);
    procedure Action_LdapConnectExecute(Sender: TObject);
    procedure Action_LdapDisconnectExecute(Sender: TObject);
    procedure Action_LdapDisconnectUpdate(Sender: TObject);
    procedure Action_LdapOptionsExecute(Sender: TObject);
    procedure Action_NewWindowExecute(Sender: TObject);
    procedure Action_OpenedPropertiesUpdate(Sender: TObject);
    procedure Action_QuitExecute(Sender: TObject);
    procedure Action_SettingsExecute(Sender: TObject);
    procedure Action_ShowGPOExecute(Sender: TObject);
    procedure Action_ShowGPOUpdate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItem_ViewThemeDarkClick(Sender: TObject);
    procedure MenuItem_ViewThemeLightClick(Sender: TObject);
    procedure MenuItem_ViewThemeSystemClick(Sender: TObject);
    procedure Timer_AutoConnectTimer(Sender: TObject);
  private
    fLog: TSynLogClass;
    fArgs: TOpenRSATArgs;

    fRSAT: TRSAT;
    fModules: TFrmModules;

    fVisPropertiesList: TVisPropertiesList;

    function GetLdapClient: TRsatLdapClient;
    function GetLdapConfigs: TLdapConfigs;
    procedure OnOptionChange(Option: TOption);
    procedure RestoreOldConfig;
    procedure UpdateViewThemeButtons;

    procedure UpdateLang;
    procedure RegisterModule(Module: TFrameModule);
    procedure CreateTabFromRegisteredModules;
    procedure CreateTabFromModule(Module: TFrameModule);

    procedure OnLdapConnect(Sender: TObject);
    procedure OnLdapClose(Sender: TObject);
    procedure OnLdapError(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure ViewMenuItemClick(Sender: TObject);
    procedure SetStatusBar(aIndex: Integer; const aText: RawUtf8);

    property Args: TOpenRSATArgs read fArgs write fArgs;
    property LdapConfigs: TLdapConfigs read GetLdapConfigs;
    property LdapClient: TRsatLdapClient read GetLdapClient;
  protected
    function GetIniPropStorage: TIniPropStorage;
    function GetRSAT: TRSAT;
    function GetComponentOwner: TComponent;
  public
    procedure OpenProperty(DistinguishedName: RawUtf8);
    procedure CloseProperty(DistinguishedName: RawUtf8);
    procedure ChangeDomainController(DomainController: RawUtf8);
  end;

{$I version.inc}

var
  VisOpenRSAT: TVisOpenRSAT;

implementation
uses
  process,
  mormot.core.os,
  mormot.core.text,
  uconfig,
  ufrmrsatoptions,
  ucommon,
  uvisabout,
  ufrmmoduleaduc,
  ufrmmoduledns,
  ufrmmodulesitesandservices,
  ufrmmoduleserviceinterfaces;

{$R *.lfm}

{ TVisRsatConsole }

procedure TVisOpenRSAT.Action_QuitExecute(Sender: TObject);
begin
  Close;
end;

procedure TVisOpenRSAT.Action_SettingsExecute(Sender: TObject);
begin
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, '% - Execute');

  With TVisOptions.Create(Self, TFrmRSATOption, fRSAT.RsatOption, fModules) do
  try
    ShowModal;
    fModules.RefreshAll;
  finally
    Free;
  end;
end;

procedure TVisOpenRSAT.Action_ShowGPOExecute(Sender: TObject);
var
  Module: TFrameModule;
begin
  Module := fModules.Get('UsersAndComputers');
  if not Assigned(Module) then
    Exit;

  ((Module as TFrmModuleADUC).Module as TModuleADUC).ADUCOption.ShowGPO := not ((Module as TFrmModuleADUC).Module as TModuleADUC).ADUCOption.ShowGPO;
  Module.Refresh;
end;

procedure TVisOpenRSAT.Action_ShowGPOUpdate(Sender: TObject);
var
  Module: TFrameModule;
begin
  Module := fModules.Get('UsersAndComputers');
  if not Assigned(Module) then
    Exit;

  Action_ShowGPO.Checked := ((Module as TFrmModuleADUC).Module as TModuleADUC).ADUCOption.ShowGPO;
end;

procedure TVisOpenRSAT.Action_OpenedPropertiesUpdate(Sender: TObject);
var
  AName: String;
  i: Integer;
  NewMenuItem: TMenuItem;
  found: Boolean;
  ItemsFound: Array of TMenuItem;

  function Contains(Items: Array of TMenuItem; Item: TMenuItem): Boolean;
  var
    v: TMenuItem;
  begin
    result := True;
    for v in Items do
      if v = Item then
        Exit;
    result := False;
  end;

begin
  if fVisPropertiesList.Count = MenuItem_ViewWindows.Count then
    Exit;
  ItemsFound := [];

  for AName in fVisPropertiesList.GetNames do
  begin
    found := False;
    for i := 0 to MenuItem_ViewWindows.Count - 1 do
    begin
      Found := (MenuItem_ViewWindows.Items[i].Caption = AName);
      if Found then
      begin
        Insert(MenuItem_ViewWindows.Items[i], ItemsFound, 0);
        Break;
      end;
    end;
    if Found then
      continue;
    NewMenuItem := TMenuItem.Create(Self);
    NewMenuItem.Caption := AName;
    NewMenuItem.OnClick := @ViewMenuItemClick;
    NewMenuitem.ImageIndex := -1;
    MenuItem_ViewWindows.Add(NewMenuItem);
    Insert(NewMenuItem, ItemsFound, 0);
  end;
  i := MenuItem_ViewWindows.Count - 1;
  while i >= 0 do
  begin
    if not Contains(ItemsFound, MenuItem_ViewWindows.Items[i]) then
      MenuItem_ViewWindows.Remove(MenuItem_ViewWindows.Items[i]);
    Dec(i);
  end;

end;

procedure TVisOpenRSAT.Action_NewWindowExecute(Sender: TObject);
var
  CheminExecutable: String;
begin
  CheminExecutable := ParamStr(0);

  with TProcess.Create(nil) do
  try
    Executable := CheminExecutable;
    Execute;
  finally
    Free;
  end;
end;

procedure TVisOpenRSAT.Action_AboutExecute(Sender: TObject);
var
  Vis: TVisAbout;
begin
  Vis := TVisAbout.Create(Self);
  try
    Vis.Version := VERSION;
    Vis.ShowModal;
  finally
    FreeAndNil(Vis);
  end;
end;

procedure TVisOpenRSAT.Action_AdvancedFeaturesExecute(Sender: TObject);
begin
  fRSAT.RsatOption.AdvancedView := not fRSAT.RsatOption.AdvancedView;
  fModules.Refresh;
end;

procedure TVisOpenRSAT.Action_AdvancedFeaturesUpdate(Sender: TObject);
begin
  Action_AdvancedFeatures.Checked := fRSAT.RsatOption.AdvancedView;
end;

procedure TVisOpenRSAT.Action_LdapConnectExecute(Sender: TObject);
var
  pwd: TFormConnectConfigs;
  conf: TConnectionSettings;
begin
  if Assigned(LdapClient) and LdapClient.Connected then
    LdapClient.Close;

  if (LdapConfigs.LastConfig = '') then
  begin
    if Assigned(fLog) then
      fLog.Add.Log(sllInfo, 'No last config. Open options...');
    Action_LdapOptions.Execute;
    Exit;
  end;

  LdapConfigs.LoadConfig();

  if not LdapConfigs.IsConfigValid then
  begin
    pwd := TFormConnectConfigs.Create(Self, LdapConfigs);
    try
      if (pwd.ShowModal <> mrOK) then
      begin
        if Assigned(fLog) then
          fLog.Add.Log(sllError, 'Invalid connection entries.');
        Exit;
      end;
    finally
      FreeAndNil(pwd);
    end;
  end;

  Screen.Cursor := crHourGlass;
  try
    // TLdapClientSettings
    conf := LdapConfigs.LdapConnectionSettings;
    LdapClient.ChangeSettings(conf);

    {$IFDEF DEBUG}
      LdapClient.Log := TLdapLog;
    {$ENDIF DEBUG}

    LdapClient.TlsContext^.IgnoreCertificateErrors := conf.AllowUnsafePasswordBind;
    // lsfSaclSecurityInformation sould not be used.
    // A regular user will not be able to get the NTSecurityDesriptor is Ldap is trying to fetch SACL
    LdapClient.SearchSDFlags := [lsfOwnerSecurityInformation, lsfGroupSecurityInformation, lsfDaclSecurityInformation];

    // Connection & bind
    if not LdapClient.Connect then
    begin
      Screen.Cursor := crDefault;
      if Assigned(fLog) then
        fLog.Add.Log(sllError, 'Ldap connection failed: %', [LdapClient.ResultString]);
      Action_LdapOptions.Execute();
      Exit;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TVisOpenRSAT.Action_LdapDisconnectExecute(Sender: TObject);
begin
  LdapClient.Close;
end;

procedure TVisOpenRSAT.Action_LdapDisconnectUpdate(Sender: TObject);
begin
  Action_LdapDisconnect.Enabled := Assigned(LdapClient) and LdapClient.Connected();
end;

procedure TVisOpenRSAT.Action_LdapOptionsExecute(Sender: TObject);
var
  ConnectionConfig: TFormConnectConfigs;
begin
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, '% - Execute', [Action_LdapOptions.Name]);

  ConnectionConfig := TFormConnectConfigs.Create(self, LdapConfigs);
  try
    if (ConnectionConfig.ShowModal() = mrOK) then
    begin
      Action_LdapDisconnect.Execute();
      if LdapConfigs.AutoConnect then
        Action_LdapConnect.Execute();
    end;
  finally
    FreeAndNil(ConnectionConfig);
  end;
end;

procedure TVisOpenRSAT.FormShow(Sender: TObject);
begin
  Timer_AutoConnect.Enabled := LdapConfigs.AutoConnect;
  UpdateViewThemeButtons;
  IniPropStorage1.Restore;
  MakeFullyVisible();
  {$IFDEF WINDOWS}
  if (WindowState = wsMaximized) and (OSVersion >= wTen) then
  begin
    WindowState := wsNormal;
    WindowState := wsMaximized;
  end;
  {$ENDIF WINDOWS}
  SetStatusBar(2, FormatUtf8('Version: %', [VERSION]));
end;

procedure TVisOpenRSAT.MenuItem_ViewThemeDarkClick(Sender: TObject);
begin
  fRSAT.RsatOption.Theme := tmDark;
  UpdateViewThemeButtons;
end;

procedure TVisOpenRSAT.MenuItem_ViewThemeLightClick(Sender: TObject);
begin
  fRSAT.RsatOption.Theme := tmLight;
  UpdateViewThemeButtons;
end;

procedure TVisOpenRSAT.MenuItem_ViewThemeSystemClick(Sender: TObject);
begin
  fRSAT.RsatOption.Theme := tmSystem;
  UpdateViewThemeButtons;
end;

procedure TVisOpenRSAT.Timer_AutoConnectTimer(Sender: TObject);
begin
  Timer_AutoConnect.Enabled := False;
  Action_LdapConnect.Execute;
end;

procedure TVisOpenRSAT.RestoreOldConfig;
var
  Paths: TStringArray;
  oldPath: TFileName;
  newPath: String;
begin
  newPath := GetAppConfigDir(False);
  Paths := newPath.Split(PathDelim);
  if Paths[high(Paths)] = '' then
    Delete(Paths, high(Paths), 1);
  Paths[high(Paths)] := 'RsatConsole';
  oldPath := String.Join(PathDelim, Paths);
  oldPath := MakePath([oldPath]);
  if DirectoryExists(oldPath) then
  begin
    oldPath := IncludeTrailingPathDelimiter(oldPath);
    newPath := IncludeTrailingPathDelimiter(newPath);
    RenameFile(oldPath, newPath);
  end;
end;

function TVisOpenRSAT.GetLdapClient: TRsatLdapClient;
begin
  result := fRSAT.LdapClient;
end;

function TVisOpenRSAT.GetLdapConfigs: TLdapConfigs;
begin
  result := fRSAT.LdapConfigs;
end;

procedure TVisOpenRSAT.OnOptionChange(Option: TOption);
begin
  UpdateLang;
end;

procedure TVisOpenRSAT.UpdateViewThemeButtons;
begin
  MenuItem_ViewThemeDark.Checked := fRsat.RsatOption.Theme = tmDark;
  MenuItem_ViewThemeLight.Checked := fRsat.RsatOption.Theme = tmLight;
  MenuItem_ViewThemeSystem.Checked := fRsat.RsatOption.Theme = tmSystem;
end;

procedure TVisOpenRSAT.UpdateLang;
var
  Lang: RawUtf8;
begin
  try
    Lang := fRSAT.RsatOption.Lang;
    if Lang = '' then
      Lang := GetLanguageID.CountryCode;
    TranslateFromResource(Lang);
  except
    on E: EResNotFound do
      if Assigned(fLog) then
        fLog.Add.Log(sllWarning, 'Translation resource not found', Self);
    on E: Exception do
      raise E;
  end;
end;

procedure TVisOpenRSAT.RegisterModule(Module: TFrameModule);
begin
  fModules.RegisterModule(Module);
end;

procedure TVisOpenRSAT.CreateTabFromRegisteredModules;
var
  Module: TFrameModule;
begin
  for Module in fModules.Items do
    CreateTabFromModule(Module);

  PageControl1.ShowTabs := (PageControl1.PageCount > 1);
end;

procedure TVisOpenRSAT.CreateTabFromModule(Module: TFrameModule);
var
  NewTab: TTabSheet;
begin
  if not Module.ModuleEnabled then
    Exit;

  NewTab := PageControl1.AddTabSheet;
  NewTab.Caption := Module.ModuleDisplayName;
  NewTab.Name := Module.ModuleName;

  Module.Parent := NewTab;
  Module.Align := alClient;
end;

procedure TVisOpenRSAT.OnLdapConnect(Sender: TObject);
var
  Module: TFrameModule;
begin
  SetStatusBar(0, FormatUtf8(rsStatusBarUsername, [LdapClient.BoundUser]));
  SetStatusBar(1, FormatUtf8(rsStatusBarDC, [LdapClient.Settings.TargetHost]));

  for Module in fModules.Items do
    Module.OnLdapConnect(LdapClient);
end;

procedure TVisOpenRSAT.OnLdapClose(Sender: TObject);
var
  Module: TFrameModule;
begin
  SetStatusBar(0, '');
  SetStatusBar(1, '');

  for Module in fModules.Items do
    Module.OnLdapClose(LdapClient);
end;

procedure TVisOpenRSAT.OnLdapError(Sender: TObject);
begin
  ShowLdapError((Sender as TLdapClient));
end;

constructor TVisOpenRSAT.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fLog := TOpenRSATLog;
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, '% - Create', [Self.Name]);

  fRSAT := TRSAT.Create(@OnOptionChange);
  fModules := TFrmModules.Create;
  frsat.Load;
  fVisPropertiesList := TVisPropertiesList.Create(Self);

  RegisterModule(TFrmModuleADUC.Create(Self));
  RegisterModule(TFrmModuleDNS.Create(Self));
  RegisterModule(TFrmModuleSitesAndServices.Create(Self));
  RegisterModule(TFrmModuleADSI.Create(Self));
  CreateTabFromRegisteredModules;

  // Setup theme for windows
  {$IFDEF WINDOWS}
  {$IFDEF LCLWIN32}
  if fRSAT.RsatOption.Theme = tmDark then
    PreferredAppMode := pamForceDark
  else if fRSAT.RsatOption.Theme = tmLight then
    PreferredAppMode := pamForceLight
  else
    PreferredAppMode := pamAllowDark;
  uMetaDarkStyle.ApplyMetaDarkStyle(DefaultDark);
  {$ENDIF}
  {$ENDIF}

  // Check for old config
  if not DirectoryExists(GetAppConfigDir(False)) then
    RestoreOldConfig;

  StatusBar1.Canvas.Font.Assign(StatusBar1.Font);

  LdapClient.OnConnect := @OnLdapConnect;
  LdapClient.OnClose := @OnLdapClose;
  LdapClient.OnError := @OnLdapError;

  IniPropStorage1.IniFileName := VisBakFilePath;
end;

destructor TVisOpenRSAT.Destroy;
begin
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, '% - Destroy', [Self.Name]);

  FreeAndNil(fVisPropertiesList);
  FreeAndNil(fRSAT);
  FreeAndNil(fModules);
  inherited Destroy;
end;

procedure TVisOpenRSAT.ViewMenuItemClick(Sender: TObject);
var
  i: Integer;
begin
  i := MenuItem_ViewWindows.IndexOf(sender as TMenuItem);
  if i = -1 then
    Exit;
  fVisPropertiesList.Focus(i);
end;

procedure TVisOpenRSAT.SetStatusBar(aIndex: Integer; const aText: RawUtf8);
const
  OFFSET: Integer = 8;
begin
  StatusBar1.Panels.Items[aIndex].Text := aText;
  StatusBar1.Panels.Items[aIndex].Width := StatusBar1.Canvas.TextWidth(StatusBar1.Panels.Items[aIndex].Text) + OFFSET;
end;

function TVisOpenRSAT.GetIniPropStorage: TIniPropStorage;
begin
  result := IniPropStorage1;
end;

function TVisOpenRSAT.GetRSAT: TRSAT;
begin
  result := fRSAT;
end;

function TVisOpenRSAT.GetComponentOwner: TComponent;
begin
  result := Self;
end;

procedure TVisOpenRSAT.OpenProperty(DistinguishedName: RawUtf8);
begin
  fVisPropertiesList.Open(DistinguishedName);
end;

procedure TVisOpenRSAT.CloseProperty(DistinguishedName: RawUtf8);
begin
  fVisPropertiesList.Close(DistinguishedName);
end;

procedure TVisOpenRSAT.ChangeDomainController(DomainController: RawUtf8);
begin
  if LdapClient.Settings.TargetHost = DomainController then
    Exit;
  LdapClient.Close;
  LdapClient.Settings.TargetHost := DomainController;
  LdapClient.Settings.KerberosSpn := '';
  LdapClient.Connect();
end;

end.

