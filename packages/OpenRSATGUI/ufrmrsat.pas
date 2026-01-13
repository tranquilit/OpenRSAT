unit ufrmrsat;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  ComCtrls,
  ActnList,
  ExtCtrls,
  StdCtrls,
  mormot.core.base,
  mormot.core.log,
  mormot.core.test,
  uldapconfigs,
  uoption,
  ursatldapclient,
  uvisproperties,
  uvispropertieslist,
  ufrmrsatoptions,
  ufrmmodule,
  ufrmmodules,
  ufrmoption,
  ursat,
  ursatoption;

type

  TFrmRSATOptionClass = class of TFrmRSATOption;

  { TFrmRSAT }

  TFrmRSAT = class(TFrame)
    Action_AdvancedFeatures: TAction;
    ActionList_Core: TActionList;
    Action_LdapConnect: TAction;
    Action_LdapDisconnect: TAction;
    Action_LdapOptions: TAction;
    Action_Options: TAction;
    Action_Properties: TAction;
    PageControl1: TPageControl;
    StatusBar1: TStatusBar;
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

    // Self core
    fRSAT: TRSAT;

    // Frame for option.
    fFrmRSATOption: TFrmRSATOption;

    // Frame for modules.
    fFrmModules: TFrmModules;

    // Opened properties
    fVisPropertiesList: TVisPropertiesList;

    procedure LoadModules;

    procedure OnOptionChange(Option: TOption);

    procedure OnLdapConnect(Sender: TObject);
    procedure OnLdapClose(Sender: TObject);
    procedure OnLdapError(Sender: TObject);
  public
    FrmRSATOptionClass: TFrameOptionClass;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    /// Register a new module to the core.
    function RegisterModule(FrameModule: TFrameModule): boolean;
    property VisPropertiesList: TVisPropertiesList read fVisPropertiesList;

    procedure SetStatusBarText(ItemIndex: Integer; ItemText: RawUtf8);
    procedure UpdateLang;
    procedure Restart;

    // Self core
    property RSAT: TRSAT read fRSAT;

    // Frame for option.
    property FrmRSATOption: TFrmRSATOption read fFrmRSATOption;

    property FrmModules: TFrmModules read fFrmModules;

    procedure CloseProperty(VisProperty: TForm);
    function OpenProperty(DistinguishedName: RawUtf8; AName: RawUtf8 = ''): TForm;
    procedure Load;
    procedure ChangeDomainController(DomainController: RawUtf8);

  // Expose RSAT
  private
    function GetLdapClient: TRsatLdapClient;
    function GetLdapConfigs: TLdapConfigs;
    function GetRsatOption: TRsatOption;
  public
    property LdapClient: TRsatLdapClient read GetLdapClient;
    property LdapConfigs: TLdapConfigs read GetLdapConfigs;
    property RsatOption: TRsatOption read GetRsatOption;
  end;

  {$IFDEF OPENRSATTESTS}

  { TTestFrmCore }

  { TTestModule }

  TTestModule = class(TInterfacedObject, IModule)
    function GetModuleEnabled: Boolean;
    procedure SetModuleEnabled(AValue: Boolean);
    function GetModuleName: String;
    function GetModuleDisplayName: String;
    function GetOptions: TOptions;
    procedure Refresh;
    procedure Load;
  end;

  TTestFrmCore = class(TSynTestCase)
  published
    procedure MethodCreate;
    procedure MethodRegisterModule;
    procedure MethodGetLdapClient;
    procedure MethodGetLdapConfigs;
    procedure MethodGetModules;
    procedure MethodGetRsatOptions;
    procedure MethodCloseProperty;
    procedure MethodOpenProperty;
  end;
  {$ENDIF}

var
  FrmRSAT: TFrmRSAT;

implementation

uses
  Dialogs,
  Translations,
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
  ucommon,
  ursatldapclientui,
  utranslation;

{$R *.lfm}

{ TFrmRSAT }

procedure TFrmRSAT.Action_LdapConnectExecute(Sender: TObject);
var
  conf: TConnectionSettings;
  pwd: TFormConnectConfigs;
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
    pwd := TFormConnectConfigs.Create(CoreDataModule, LdapConfigs);
    try
      if (pwd.ShowModal <> mrOK) then
      begin
        if Assigned(fLog) then
          fLog.Log(sllError, 'Invalid connection entries.');
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
    LdapClient.ChangeSettings(Conf, False);

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
      ShowLdapConnectError(LdapClient);
      Action_LdapOptions.Execute();
      Exit;
    end;
  finally
    Screen.Cursor := crDefault;
  end;

  SetStatusBarText(0, FormatUtf8('User: %', [LdapClient.BoundUser]));
  SetStatusBarText(1, FormatUtf8('DC: %', [LdapClient.Settings.TargetHost]));
end;

procedure TFrmRSAT.Action_AdvancedFeaturesExecute(Sender: TObject);
begin
  RSAT.RsatOption.AdvancedView := not RSAT.RsatOption.AdvancedView;
  fFrmModules.Refresh;
end;

procedure TFrmRSAT.Action_AdvancedFeaturesUpdate(Sender: TObject);
begin
  Action_AdvancedFeatures.Checked := RSAT.RsatOption.AdvancedView;
end;

procedure TFrmRSAT.Action_LdapConnectUpdate(Sender: TObject);
begin

end;

procedure TFrmRSAT.Action_LdapDisconnectExecute(Sender: TObject);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Execute', [Action_LdapDisconnect.Name]);

  LdapClient.Close;

  SetStatusBarText(0, 'User: NA');
end;

procedure TFrmRSAT.Action_LdapDisconnectUpdate(Sender: TObject);
begin
  Action_LdapDisconnect.Enabled := Assigned(LdapClient) and LdapClient.Connected();
end;

procedure TFrmRSAT.Action_LdapOptionsExecute(Sender: TObject);
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

procedure TFrmRSAT.Action_LdapOptionsUpdate(Sender: TObject);
begin
  Action_LdapOptions.Enabled := True;
end;

procedure TFrmRSAT.Action_OptionsExecute(Sender: TObject);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Execute');

  With TVisOptions.Create(Self) do
  try
    ShowModal;
    fFrmModules.RefreshAll;
  finally
    Free;
  end;
end;

procedure TFrmRSAT.Action_OptionsUpdate(Sender: TObject);
begin
  Action_Options.Enabled := True;
end;

procedure TFrmRSAT.PageControl1Change(Sender: TObject);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Change', [Self.Name]);

  fFrmModules.Change(PageControl1.ActivePage.Name);
end;

procedure TFrmRSAT.Timer_AutoConnectTimer(Sender: TObject);
begin
  Timer_AutoConnect.Enabled := False;
  Action_LdapConnect.Execute;
end;

procedure TFrmRSAT.SetStatusBarText(ItemIndex: Integer; ItemText: RawUtf8);
begin
  StatusBar1.Panels.Items[ItemIndex].Text := ItemText;
  StatusBar1.Panels.Items[ItemIndex].Width := StatusBar1.Canvas.TextWidth(StatusBar1.Panels.Items[ItemIndex].Text) + 8;
end;

procedure TFrmRSAT.UpdateLang;
var
  Lang: RawUtf8;
begin
  try
    Lang := RsatOption.Lang;
    if Lang = '' then
      Lang := GetLanguageID.CountryCode;
    TranslateFromResource(Lang);
  except
    on E: EResNotFound do
      if Assigned(fLog) then
        fLog.Log(sllWarning, 'Translation resource not found', Self);
    on E: Exception do
      raise E;
  end;
end;

procedure TFrmRSAT.Restart;
begin
  (Parent as TForm).Close;
end;

procedure TFrmRSAT.LoadModules;
var
  aLog: ISynLog;
  FrameModule: TFrameModule;

  procedure LoadModule(FrameModule: TFrameModule);
  var
    NewTab: TTabSheet;
  begin
    if not FrameModule.ModuleEnabled then
    begin
      if Assigned(fLog) then
        fLog.Log(sllInfo, '% - Module "%" not enabled.', [Self.Name, FrameModule.ModuleName]);
      Exit;
    end;

    FrameModule.Load;
    NewTab := PageControl1.AddTabSheet;
    NewTab.Caption := FrameModule.ModuleDisplayName;
    NewTab.Name := FrameModule.ModuleName;

    FrameModule.Parent := NewTab;
    FrameModule.Align := alClient;

    PageControl1.ShowTabs := (PageControl1.PageCount <> 1);

    if Assigned(fLog) then
      fLog.Log(sllInfo, '% - Module "%" loaded.', [Self.Name, FrameModule.ModuleName]);
  end;

begin
  if Assigned(fLog) then
    aLog := fLog.Enter('% - LoadModules', [Self.Name]);

  FrameModule := TFrmModuleADUC.Create(Self);
  if RegisterModule(FrameModule) then
    LoadModule(FrameModule);
  FrmModules.Change(FrameModule.ModuleName);

  FrameModule := TFrmModuleDNS.Create(Self);
  if RegisterModule(FrameModule) then
    LoadModule(FrameModule);

  FrameModule := TFrmModuleSitesAndServices.Create(Self);
  if RegisterModule(FrameModule) then
    LoadModule(FrameModule);

  FrameModule := TFrmModuleADSI.Create(Self);
  if RegisterModule(FrameModule) then
    LoadModule(FrameModule);

  if LdapConfigs.AutoConnect then
    Timer_AutoConnect.Enabled := True;
end;

procedure TFrmRSAT.OnOptionChange(Option: TOption);
begin
  UpdateLang;
end;

procedure TFrmRSAT.OnLdapConnect(Sender: TObject);
var
  Module: TFrameModule;
begin
  for Module in fFrmModules.Items do
    Module.OnLdapConnect(Sender);
end;

procedure TFrmRSAT.OnLdapClose(Sender: TObject);
var
  Module: TFrameModule;
begin
  for Module in fFrmModules.Items do
    Module.OnLdapClose(Sender);
end;

procedure TFrmRSAT.OnLdapError(Sender: TObject);
begin
  ShowLdapError((Sender as TLdapClient));
end;

constructor TFrmRSAT.Create(TheOwner: TComponent);
var
  aLog: ISynLog;
begin
  inherited Create(TheOwner);

  fLog := TSynLog.Add;

  if Assigned(fLog) then
    aLog := fLog.Enter('% - Create', [Self.Name]);

  Application.CreateForm(TCoreDataModule, CoreDataModule);

  FrmRSATOptionClass := TFrmRSATOption;

  fRSAT := TRSAT.Create(@OnOptionChange);
  fFrmRSATOption := TFrmRSATOption.Create(Self);
  fFrmModules := TFrmModules.Create;

  fRSAT.Load;

  fVisPropertiesList := TVisPropertiesList.Create;

  StatusBar1.Canvas.Font.Assign(StatusBar1.Font);

  fRSAT.LdapClient.OnConnect := @OnLdapConnect;
  fRSAT.LdapClient.OnClose := @OnLdapClose;
  fRSAT.LdapClient.OnError := @OnLdapError;
end;

destructor TFrmRSAT.Destroy;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Destroy', [Self.Name]);

  FreeAndNil(fRSAT);
  FreeAndNil(fFrmRSATOption);
  FreeAndNil(fFrmModules);

  FreeAndNil(fVisPropertiesList);

  inherited Destroy;
end;

function TFrmRSAT.RegisterModule(FrameModule: TFrameModule): boolean;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Register Module', [Self.Name]);

  result := FrmModules.RegisterModule(FrameModule);
end;

function TFrmRSAT.GetLdapClient: TRsatLdapClient;
begin
  result := RSAT.LdapClient;
end;

function TFrmRSAT.GetLdapConfigs: TLdapConfigs;
begin
  result := RSAT.LdapConfigs;
end;

function TFrmRSAT.GetRsatOption: TRsatOption;
begin
  result := RSAT.RsatOption;
end;

procedure TFrmRSAT.CloseProperty(VisProperty: TForm);
begin
  fVisPropertiesList.Close((VisProperty as TVisProperties));
end;

function TFrmRSAT.OpenProperty(DistinguishedName: RawUtf8; AName: RawUtf8): TForm;
begin
  if AName = '' then
    AName := GetDNName(DistinguishedName);
  result := fVisPropertiesList.Open(AName, DistinguishedName);
end;

procedure TFrmRSAT.Load;
begin
  LoadModules;
end;

procedure TFrmRSAT.ChangeDomainController(DomainController: RawUtf8);
begin
  LdapClient.DomainControllerName := DomainController;
  SetStatusBarText(1, FormatUtf8('DC: %', [DomainController]));
end;

{$IFDEF OPENRSATTESTS}

{ TTestModule }

function TTestModule.GetModuleEnabled: Boolean;
begin
  result := True;
end;

procedure TTestModule.SetModuleEnabled(AValue: Boolean);
begin

end;

function TTestModule.GetModuleName: String;
begin
  result := 'TestModule';
end;

function TTestModule.GetModuleDisplayName: String;
begin
  result := 'Test Module';
end;

function TTestModule.GetOptions: TOptions;
begin
  result := nil;
end;

procedure TTestModule.Refresh;
begin

end;

procedure TTestModule.Load;
begin

end;

{ TTestFrmCore }

procedure TTestFrmCore.MethodCreate;
var
  Core: TFrmRSAT;
begin
  Core := TFrmRSAT.Create(nil);
  try
    Check(Assigned(Core));
    Check(Assigned(Core.fLog));
    Check(Assigned(Core.fLdapClient));
    Check(Assigned(Core.fOptions));
    Check(Assigned(Core.fLdapConfigs));
    Check(Assigned(Core.fModules));
    Check(Assigned(Core.fVisPropertiesList));
  finally
    FreeAndNil(Core);
  end;
end;

procedure TTestFrmCore.MethodRegisterModule;
var
  Core: TFrmRSAT;
  Module: IModule;
begin
  Core := TFrmRSAT.Create(nil);
  try
    Check(not Core.RegisterModule(nil));
  finally
    FreeAndNil(Core);
  end;

  Module := TTestModule.Create;
  Core := TFrmRSAT.Create(nil);
  try
    Check(Core.RegisterModule(Module));
  finally
    FreeAndNil(Core);
  end;
end;

procedure TTestFrmCore.MethodGetLdapClient;
var
  Core: TFrmRSAT;
begin
  Core := TFrmRSAT.Create(nil);
  try
    Check(Assigned(Core.GetLdapClient));
  finally
    FreeAndNil(Core);
  end;
end;

procedure TTestFrmCore.MethodGetLdapConfigs;
var
  Core: TFrmRSAT;
begin
  Core := TFrmRSAT.Create(nil);
  try
    Check(Assigned(Core.GetLdapConfigs));
  finally
    FreeAndNil(Core);
  end;
end;

procedure TTestFrmCore.MethodGetModules;
var
  Core: TFrmRSAT;
begin
  Core := TFrmRSAT.Create(nil);
  try
    Check(Assigned(Core.GetModules));
  finally
    FreeAndNil(Core);
  end;
end;

procedure TTestFrmCore.MethodGetRsatOptions;
var
  Core: TFrmRSAT;
begin
  Core := TFrmRSAT.Create(nil);
  try
    Check(Assigned(Core.GetRsatOptions));
  finally
    FreeAndNil(Core);
  end;
end;

procedure TTestFrmCore.MethodCloseProperty;
var
  Core: TFrmRSAT;
  VisProperties: TForm;
begin
  Core := TFrmRSAT.Create(nil);
  try
    Check(Core.VisPropertiesList.Count = 0);
    VisProperties := Core.OpenProperty('test', 'testtest');
    Check(Core.VisPropertiesList.Count = 1);
    Core.CloseProperty(VisProperties);
    Check(Core.VisPropertiesList.Count = 0);
  finally
    FreeAndNil(Core);
  end;
end;

procedure TTestFrmCore.MethodOpenProperty;
var
  Core: TFrmRSAT;
  VisProperties: TForm;
begin
  Core := TFrmRSAT.Create(nil);
  try
    Check(Core.VisPropertiesList.Count = 0);
    VisProperties := Core.OpenProperty('test', 'testtest');
    Check(Assigned(VisProperties));
  finally
    FreeAndNil(Core);
  end;
end;

{$ENDIF}

end.

