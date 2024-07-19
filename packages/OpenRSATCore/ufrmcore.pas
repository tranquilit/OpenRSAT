unit ufrmcore;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  ComCtrls,
  ActnList, ExtCtrls,
  mormot.core.log,
  uldapconfigs,
  ursatldapclient,
  ursatmodules,
  uinterfacemodule,
  uinterfacecore,
  uvisproperties,
  ufrmrsatoptions,
  mormot.core.base;

type

  { TFrmCore }

  TFrmCore = class(TFrame, ICore)
    Action_AdvancedFeatures: TAction;
    ActionList_Core: TActionList;
    Action_LdapConnect: TAction;
    Action_LdapDisconnect: TAction;
    Action_LdapOptions: TAction;
    Action_Options: TAction;
    Action_Properties: TAction;
    PageControl1: TPageControl;
    Timer_AutoConnect: TTimer;
    {$push}{$warn 5024 off}
    procedure Action_AdvancedFeaturesExecute(Sender: TObject);
    procedure Action_AdvancedFeaturesUpdate(Sender: TObject);
    procedure Action_LdapConnectExecute(Sender: TObject);
    procedure Action_LdapConnectUpdate(Sender: TObject);
    procedure Action_LdapDisconnectExecute(Sender: TObject);
    procedure Action_LdapDisconnectUpdate(Sender: TObject);
    procedure Action_LdapOptionsExecute(Sender: TObject);
    procedure Action_LdapOptionsUpdate(Sender: TObject);
    procedure Action_OptionsExecute(Sender: TObject);
    procedure Action_OptionsUpdate(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure Timer_AutoConnectTimer(Sender: TObject);
    {$pop}
  private
    fLog: TSynLog;
    fActive: Boolean;

    /// Centralized LdapClient connexion.
    fLdapClient: TRsatLdapClient;

    /// Manage all options in the project. Usefull to easely save and load all
    /// tool options.
    fOptions: TRsatOptions;

    fLdapConfigs: TLdapConfigs;

    /// Manage all rsat modules in the tool.
    fModules: TRsatModules;

    fVisPropertiesList: TVisPropertiesList;

    /// Register all modules to the core.
    procedure LoadModules;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    /// Register a new module to the core.
    function RegisterModule(AModule: IModule {TAbstractModule}): boolean;
    property VisPropertiesList: TVisPropertiesList read fVisPropertiesList;

    //////////
    /// ICore
    function GetActive: Boolean;
    function GetLdapClient: TRsatLdapClient;
    function GetLdapConfigs: TLdapConfigs;
    function GetModules: TRsatModules;
    function GetRsatOptions: TRsatOptions;

    procedure Activate;
    procedure CloseProperty(VisProperty: TForm);
    procedure Deactivate;
    procedure OpenProperty(AName, DistinguishedName: String);
    procedure Load;

    property Active: Boolean read GetActive;
    property LdapClient: TRsatLdapClient read GetLdapClient;
    property LdapConfigs: TLdapConfigs read GetLdapConfigs;
    property Modules: TRsatModules read GetModules;
    property RsatOptions: TRsatOptions read GetRsatOptions;
    /// ICore
    //////////
  end;

implementation

uses
  Dialogs,
  mormot.core.text,
  mormot.net.ldap,
  ufrmmoduleaduc,
  ufrmmoduledns,
  ufrmmodulesitesandservices,
  ufrmmoduleserviceinterfaces,
  ucoredatamodule,
  uVisOptions,
  uvisconnectconfigs,
  mormot.net.dns,
  ucommon;

{$R *.lfm}

{ TFrmCore }

procedure TFrmCore.Action_LdapConnectExecute(Sender: TObject);
var
  conf: TConnectionSettings;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Execute', [Action_LdapConnect.Name]);

  // Close old connection
  if Assigned(LdapClient) and LdapClient.Connected then
    LdapClient.Close;

  if (LdapConfigs.LastConfig = '') then
  begin
    if Assigned(fLog) then
      fLog.Log(sllInfo, 'No last config. Open options...');
    Action_LdapOptions.Execute;
    Exit;
  end;

  LdapConfigs.LoadConfig();

  if not LdapConfigs.IsConfigValid then
  begin
    if Assigned(fLog) then
      fLog.Log(sllError, 'Invalid connection entries.');
    Exit;
  end;

  Screen.Cursor := crHourGlass;
  try
    // TLdapClientSettings
    conf := LdapConfigs.LdapConnectionSettings;
    if conf.UseCredentials and not conf.Tls then
      {$IfDef darwin}
      conf.UserName := conf.UserName;
      {$Else}
      conf.UserName := String(conf.UserName).Split('@')[0];
      {$EndIf}
    LdapClient.ChangeSettings(Conf);

    {$IFDEF DEBUG}
      LdapClient.Log := TSynLog;
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
        fLog.Log(sllError, 'Ldap connection failed: %', [LdapClient.ResultString]);
      if String(LdapClient.ResultString).ToLower.Contains('52e') then
        MessageDlg(rsLdapError, FormatUtf8(rsLdapConnectFailed, ['Invalid credentials']), mtError, [mbOK], 0)
      else
        MessageDlg(rsLdapError, FormatUtf8(rsLdapConnectFailed, [LdapClient.ResultString]), mtError, [mbOK], 0);
      Action_LdapOptions.Execute();
      Exit;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFrmCore.Action_AdvancedFeaturesExecute(Sender: TObject);
begin
  RsatOptions.AdvancedView := not RsatOptions.AdvancedView;
  Modules.Refresh;
end;

procedure TFrmCore.Action_AdvancedFeaturesUpdate(Sender: TObject);
begin
  Action_AdvancedFeatures.Checked := RsatOptions.AdvancedView;
end;

procedure TFrmCore.Action_LdapConnectUpdate(Sender: TObject);
begin

end;

procedure TFrmCore.Action_LdapDisconnectExecute(Sender: TObject);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Execute', [Action_LdapDisconnect.Name]);

  LdapClient.Close;
end;

procedure TFrmCore.Action_LdapDisconnectUpdate(Sender: TObject);
begin
  Action_LdapDisconnect.Enabled := Assigned(LdapClient) and LdapClient.Connected();
end;

procedure TFrmCore.Action_LdapOptionsExecute(Sender: TObject);
var
  ConnectionConfig: TFormConnectConfigs;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Execute', [Action_LdapOptions.Name]);

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

procedure TFrmCore.Action_LdapOptionsUpdate(Sender: TObject);
begin
  Action_LdapOptions.Enabled := True;
end;

procedure TFrmCore.Action_OptionsExecute(Sender: TObject);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Execute');

  With TVisOptions.Create(Self, Self.RsatOptions) do
  try
    ShowModal;
    Modules.RefreshAll;
  finally
    Free;
  end;
end;

procedure TFrmCore.Action_OptionsUpdate(Sender: TObject);
begin
  Action_Options.Enabled := True;
end;

procedure TFrmCore.PageControl1Change(Sender: TObject);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Change', [Self.Name]);

  Modules.Change(PageControl1.ActivePage.Name);
end;

procedure TFrmCore.Timer_AutoConnectTimer(Sender: TObject);
begin
  Timer_AutoConnect.Enabled := False;
  Action_LdapConnect.Execute;
end;

procedure TFrmCore.LoadModules;
var
  AModule: TFrameModule;
  aLog: ISynLog;

  procedure LoadModule(AModule: TFrameModule);
  var
    NewTab: TTabSheet;
  begin
    if not AModule.GetModuleEnabled then
    begin
      if Assigned(fLog) then
        fLog.Log(sllInfo, '% - Module "%" not enabled.', [Self.Name, AModule.GetModuleName]);
      Exit;
    end;

    AModule.Load;
    if Assigned(AModule.GetOptions) then
      RsatOptions.AddOptions(AModule.GetOptions);
    NewTab := PageControl1.AddTabSheet;
    NewTab.Caption := AModule.GetModuleDisplayName;
    NewTab.Name := AModule.GetModuleName;

    AModule.Parent := NewTab;
    AModule.Align := alClient;

    PageControl1.ShowTabs := (PageControl1.PageCount <> 1);

    if Assigned(fLog) then
      fLog.Log(sllInfo, '% - Module "%" loaded.', [Self.Name, AModule.GetModuleName]);
  end;

begin
  if Assigned(fLog) then
    aLog := fLog.Enter('% - LoadModules', [Self.Name]);

  AModule := TFrmModuleADUC.Create(Self, Self);
  if RegisterModule(AModule) then
    LoadModule(AModule);
  Modules.Change(AModule.GetModuleName);

  AModule := TFrmModuleDNS.Create(Self, Self);
  if RegisterModule(AModule) then
    LoadModule(AModule);

  AModule := TFrmModuleSitesAndServices.Create(Self, Self);
  if RegisterModule(AModule) then
    LoadModule(AModule);

  AModule := TFrmModuleADSI.Create(Self, Self);
  if RegisterModule(AModule) then
    LoadModule(AModule);

  fOptions.Load;

  if fLdapConfigs.AutoConnect then
    Timer_AutoConnect.Enabled := True;
end;

constructor TFrmCore.Create(TheOwner: TComponent);
var
  aLog: ISynLog;
begin
  inherited Create(TheOwner);

  fLog := TSynLog.Add;

  if Assigned(fLog) then
    aLog := fLog.Enter('% - Create', [Self.Name]);

  Application.CreateForm(TCoreDataModule, CoreDataModule);

  fLdapClient := TRsatLdapClient.Create;
  fOptions := TRsatOptions.Create;
  fLdapConfigs := TLdapConfigs.Create;
  fModules := TRsatModules.Create;

  fLdapConfigs.LoadConfig();
  RsatOptions.Load;

  fVisPropertiesList := TVisPropertiesList.Create(Self);
end;

destructor TFrmCore.Destroy;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Destroy', [Self.Name]);

  FreeAndNil(fLdapClient);
  FreeAndNil(fOptions);
  FreeAndNil(fLdapConfigs);
  FreeAndNil(fModules);

  FreeAndNil(fVisPropertiesList);

  inherited Destroy;
end;

function TFrmCore.RegisterModule(AModule: IModule): boolean;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Register Module', [Self.Name]);

  result := fModules.RegisterModule(AModule);
end;

function TFrmCore.GetActive: Boolean;
begin
  result := fActive;
end;

function TFrmCore.GetLdapClient: TRsatLdapClient;
begin
  result := fLdapClient;
end;

function TFrmCore.GetLdapConfigs: TLdapConfigs;
begin
  result := fLdapConfigs;
end;

function TFrmCore.GetModules: TRsatModules;
begin
  result := fModules;
end;

function TFrmCore.GetRsatOptions: TRsatOptions;
begin
  result := fOptions;
end;

procedure TFrmCore.Activate;
begin
  fActive := True;
end;

procedure TFrmCore.CloseProperty(VisProperty: TForm);
begin
  fVisPropertiesList.Close((VisProperty as TVisProperties));
end;

procedure TFrmCore.Deactivate;
begin
  fActive := False;
end;

procedure TFrmCore.OpenProperty(AName, DistinguishedName: String);
begin
  fVisPropertiesList.Open(AName, DistinguishedName);
end;

procedure TFrmCore.Load;
begin
  LoadModules;
end;

end.

