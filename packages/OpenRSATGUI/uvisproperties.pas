unit uvisproperties;

{$mode ObjFPC}{$H+}
{$modeSwitch advancedRecords}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
  ActnList,
  Buttons,
  Classes,
  ComCtrls,
  DateTimePicker,
  Dialogs,
  ExtCtrls,
  Forms,
  Menus,
  Graphics,
  StdCtrls,
  ShellCtrls,
  PairSplitter,
  IniPropStorage,
  SysUtils,
  tis.ui.grid.core,
  VirtualTrees,
  mormot.core.base,
  mormot.core.log,
  mormot.core.os.security,
  mormot.core.variants,
  mormot.crypt.x509,
  mormot.net.ldap,
  Controls,
  ucommon,
  ucoredatamodule,
  upropertyframe,
  uproperty;

type

  TAttributEditorFilter = (
    aefOnlyAvailableValues,
    aefOnlyWritableValues,
    aefMandatory,
    aefOptional,
    aefConstructed,
    aefBacklinks,
    aefSystemOnly
  );

  TAttributEditorFilters = set of TAttributEditorFilter;

  { TVisProperties }

  TVisProperties = class(TForm)
    Action_Cancel: TAction;
    Action_AttributesModify: TAction;
    Action_LAPSCopyPassword: TAction;
    Action_LAPSShowPassword: TAction;
    Action_LAPSExpireNow: TAction;
    IniPropStorage1: TIniPropStorage;
    PageControl: TPageControl;
    // Bottom Panel ---------------------------------------------------------
    Panel_Bottom: TPanel;
      Btn_BottomApply: TBitBtn;
      Btn_BottomCancel: TBitBtn;
      Btn_BottomOK: TBitBtn;
    // Actions
    ActionList: TActionList;
      Action_AttributesOptional: TAction;
      Action_AttributesConstructed: TAction;
      Action_AttributesBacklinks: TAction;
      Action_AttributesSystemOnly: TAction;
      Action_AttributesMandatory: TAction;
      Action_AttributesShowWritable: TAction;
      Action_AttributesShowValues: TAction;
      Action_AttributesFilter: TAction;
      Action_CertificateView: TAction;
      Action_CertificateCopy: TAction;
      Action_CertificateRemove: TAction;
      Action_MemberOfAdd: TAction;
      Action_MemberOfDelete: TAction;
      Action_Apply: TAction;
      Action_OK: TAction;
      Action_MembersDelete: TAction;
      Action_MembersAdd: TAction;
      Action_ManagedByCheckBox: TAction;
      Action_ManagedByClear: TAction;
      Action_ManagedByProperties: TAction;
      Action_ManagedByChange: TAction;
      Action_OrganizationClear: TAction;
      Action_OrganizationProperties: TAction;
      Action_SecurityDeleteUser: TAction;
      Action_SecurityAddUser: TAction;
      Action_SecurityAdvanced: TAction;
    procedure Action_ApplyExecute(Sender: TObject);
    procedure Action_ApplyUpdate(Sender: TObject);
    procedure Action_CancelExecute(Sender: TObject);
    procedure Action_OKExecute(Sender: TObject);

    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    function FormHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean
      ): Boolean;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);

    procedure PageControlChange(Sender: TObject);

    procedure Show; reintroduce;
    procedure SetFocus; reintroduce;
  private
    fProperty: TProperty;
    fPropertyFrameList: Array of TPropertyFrame;
    fDistinguishedName: RawUtf8;

    function LoadAttributes: Boolean;
    procedure OnSearchEventFillAttributes(Sender: TObject);
    procedure LoadView;
    procedure UpdateTabs;
    procedure NewTab(NewFrameClass: TPropertyFrameClass);
  public
    constructor Create(TheOwner: TComponent; ADistinguishedName: RawUtf8); reintroduce;
    destructor Destroy(); override;

    property DistinguishedName: RawUtf8 read fDistinguishedName;
  end;

const
  ACCOUNTDISABLE             = $000002;
  PASSWD_CANT_CHANGE         = $000040;
  ENCRYPTED_TEXT_PWD_ALLOWED = $000080;
  DONT_EXPIRE_PASSWORD       = $010000;
  SMARTCARD_REQUIRED         = $040000;
  NOT_DELEGATED              = $100000;
  USE_DES_KEY_ONLY           = $200000;
  DONT_REQUIRE_PREAUTH       = $400000;
  AES128 = $08;
  AES256 = $10;

  /// standard long identifier of Certificate usage
  // - i.e. match OpenSSL PX509.ExtendedKeyUsage/KeyUsage text
  CEKU_FULLTEXT: array[TXExtendedKeyUsage] of RawUtf8 = (
    'None',                                       //xkuNone
    'Server Authentification',                    //xkuServerAuth
    'Client Authentification',                    //xkuClientAuth
    'Code Signing',                               //xkuCodeSigning
    'Email Protection',                           //xkuEmailProtection
    'Time Stamping',                              //xkuTimeStamping
    'Online Certificate Status Protocol Signing', //xkuOcspSigning
    'Ms Publisher');                              //xkuMsPublisher

  SELF_MEMBERSHIP_TEXT_GUID = '{bf9679c0-0de6-11d0-a285-00aa003049e2}';

  WELL_KNOWN_SID_NAMES: array[TWellKnownSid] of String = (
    rsWellKnownSidNull,                                            // wksNull,
    rsWellKnownSidWorld,                                           // wksWorld,
    rsWellKnownSidLocal,                                           // wksLocal,
    rsWellKnownSidConsoleLogon,                                    // wksConsoleLogon,
    rsWellKnownSidCreatorOwner,                                    // wksCreatorOwner,
    rsWellKnownSidCreatorGroup,                                    // wksCreatorGroup,
    rsWellKnownSidCreatorOwnerServer,                              // wksCreatorOwnerServer,
    rsWellKnownSidCreatorGroupServer,                              // wksCreatorGroupServer,
    rsWellKnownSidCreatorOwnerRights,                              // wksCreatorOwnerRights,
    rsWellKnownSidIntegrityUntrusted,                              // wksIntegrityUntrusted,
    rsWellKnownSidIntegrityLow,                                    // wksIntegrityLow,
    rsWellKnownSidIntegrityMedium,                                 // wksIntegrityMedium,
    rsWellKnownSidIntegrityMediumPlus,                             // wksIntegrityMediumPlus,
    rsWellKnownSidIntegrityHigh,                                   // wksIntegrityHigh,
    rsWellKnownSidIntegritySystem,                                 // wksIntegritySystem,
    rsWellKnownSidIntegrityProtectedProcess,                       // wksIntegrityProtectedProcess,
    rsWellKnownSidIntegritySecureProcess,                          // wksIntegritySecureProcess,
    rsWellKnownSidAuthenticationAuthorityAsserted,                 // wksAuthenticationAuthorityAsserted,
    rsWellKnownSidAuthenticationServiceAsserted,                   // wksAuthenticationServiceAsserted,
    rsWellKnownSidAuthenticationFreshKeyAuth,                      // wksAuthenticationFreshKeyAuth,
    rsWellKnownSidAuthenticationKeyTrust,                          // wksAuthenticationKeyTrust,
    rsWellKnownSidAuthenticationKeyPropertyMfa,                    // wksAuthenticationKeyPropertyMfa,
    rsWellKnownSidAuthenticationKeyPropertyAttestation,            // wksAuthenticationKeyPropertyAttestation,
    rsWellKnownSidNtAuthority,                                     // wksNtAuthority,
    rsWellKnownSidDialup,                                          // wksDialup,
    rsWellKnownSidNetwork,                                         // wksNetwork,
    rsWellKnownSidBatch,                                           // wksBatch,
    rsWellKnownSidInteractive,                                     // wksInteractive,
    rsWellKnownSidService,                                         // wksService,
    rsWellKnownSidAnonymous,                                       // wksAnonymous,
    rsWellKnownSidProxy,                                           // wksProxy,
    rsWellKnownSidEnterpriseControllers,                           // wksEnterpriseControllers,
    rsWellKnownSidSelf,                                            // wksSelf,
    rsWellKnownSidAuthenticatedUser,                               // wksAuthenticatedUser,
    rsWellKnownSidRestrictedCode,                                  // wksRestrictedCode,
    rsWellKnownSidTerminalServer,                                  // wksTerminalServer,
    rsWellKnownSidRemoteLogonId,                                   // wksRemoteLogonId,
    rsWellKnownSidThisOrganisation,                                // wksThisOrganisation,
    rsWellKnownSidIisUser,                                         // wksIisUser,
    rsWellKnownSidLocalSystem,                                     // wksLocalSystem,
    rsWellKnownSidLocalService,                                    // wksLocalService,
    rsWellKnownSidNetworkService,                                  // wksNetworkService,
    rsWellKnownSidLocalAccount,                                    // wksLocalAccount,
    rsWellKnownSidLocalAccountAndAdministrator,                    // wksLocalAccountAndAdministrator,
    rsWellKnownSidBuiltinDomain,                                   // wksBuiltinDomain,
    rsWellKnownSidBuiltinAdministrators,                           // wksBuiltinAdministrators,
    rsWellKnownSidBuiltinUsers,                                    // wksBuiltinUsers,
    rsWellKnownSidBuiltinGuests,                                   // wksBuiltinGuests,
    rsWellKnownSidBuiltinPowerUsers,                               // wksBuiltinPowerUsers,
    rsWellKnownSidBuiltinAccountOperators,                         // wksBuiltinAccountOperators,
    rsWellKnownSidBuiltinSystemOperators,                          // wksBuiltinSystemOperators,
    rsWellKnownSidBuiltinPrintOperators,                           // wksBuiltinPrintOperators,
    rsWellKnownSidBuiltinBackupOperators,                          // wksBuiltinBackupOperators,
    rsWellKnownSidBuiltinReplicator,                               // wksBuiltinReplicator,
    rsWellKnownSidBuiltinRasServers,                               // wksBuiltinRasServers,
    rsWellKnownSidBuiltinPreWindows2000CompatibleAccess,           // wksBuiltinPreWindows2000CompatibleAccess,
    rsWellKnownSidBuiltinRemoteDesktopUsers,                       // wksBuiltinRemoteDesktopUsers,
    rsWellKnownSidBuiltinNetworkConfigurationOperators,            // wksBuiltinNetworkConfigurationOperators,
    rsWellKnownSidBuiltinIncomingForestTrustBuilders,              // wksBuiltinIncomingForestTrustBuilders,
    rsWellKnownSidBuiltinPerfMonitoringUsers,                      // wksBuiltinPerfMonitoringUsers,
    rsWellKnownSidBuiltinPerfLoggingUsers,                         // wksBuiltinPerfLoggingUsers,
    rsWellKnownSidBuiltinAuthorizationAccess,                      // wksBuiltinAuthorizationAccess,
    rsWellKnownSidBuiltinTerminalServerLicenseServers,             // wksBuiltinTerminalServerLicenseServers,
    rsWellKnownSidBuiltinDcomUsers,                                // wksBuiltinDcomUsers,
    rsWellKnownSidBuiltinIUsers,                                   // wksBuiltinIUsers,
    rsWellKnownSidBuiltinCryptoOperators,                          // wksBuiltinCryptoOperators,
    rsWellKnownSidBuiltinUnknown,                                  // wksBuiltinUnknown,
    rsWellKnownSidBuiltinCacheablePrincipalsGroups,                // wksBuiltinCacheablePrincipalsGroups,
    rsWellKnownSidBuiltinNonCacheablePrincipalsGroups,             // wksBuiltinNonCacheablePrincipalsGroups,
    rsWellKnownSidBuiltinEventLogReadersGroup,                     // wksBuiltinEventLogReadersGroup,
    rsWellKnownSidBuiltinCertSvcDComAccessGroup,                   // wksBuiltinCertSvcDComAccessGroup,
    rsWellKnownSidBuiltinRdsRemoteAccessServers,                   // wksBuiltinRdsRemoteAccessServers,
    rsWellKnownSidBuiltinRdsEndpointServers,                       // wksBuiltinRdsEndpointServers,
    rsWellKnownSidBuiltinRdsManagementServers,                     // wksBuiltinRdsManagementServers,
    rsWellKnownSidBuiltinHyperVAdmins,                             // wksBuiltinHyperVAdmins,
    rsWellKnownSidBuiltinAccessControlAssistanceOperators,         // wksBuiltinAccessControlAssistanceOperators,
    rsWellKnownSidBuiltinRemoteManagementUsers,                    // wksBuiltinRemoteManagementUsers,
    rsWellKnownSidBuiltinDefaultSystemManagedGroup,                // wksBuiltinDefaultSystemManagedGroup,
    rsWellKnownSidBuiltinStorageReplicaAdmins,                     // wksBuiltinStorageReplicaAdmins,
    rsWellKnownSidBuiltinDeviceOwners,                             // wksBuiltinDeviceOwners,
    rsWellKnownSidBuiltinWriteRestrictedCode,                      // wksBuiltinWriteRestrictedCode,
    rsWellKnownSidBuiltinUserModeDriver,                           // wksBuiltinUserModeDriver,
    rsWellKnownSidCapabilityInternetClient,                        // wksCapabilityInternetClient,
    rsWellKnownSidCapabilityInternetClientServer,                  // wksCapabilityInternetClientServer,
    rsWellKnownSidCapabilityPrivateNetworkClientServer,            // wksCapabilityPrivateNetworkClientServer,
    rsWellKnownSidCapabilityPicturesLibrary,                       // wksCapabilityPicturesLibrary,
    rsWellKnownSidCapabilityVideosLibrary,                         // wksCapabilityVideosLibrary,
    rsWellKnownSidCapabilityMusicLibrary,                          // wksCapabilityMusicLibrary,
    rsWellKnownSidCapabilityDocumentsLibrary,                      // wksCapabilityDocumentsLibrary,
    rsWellKnownSidCapabilityEnterpriseAuthentication,              // wksCapabilityEnterpriseAuthentication,
    rsWellKnownSidCapabilitySharedUserCertificates,                // wksCapabilitySharedUserCertificates,
    rsWellKnownSidCapabilityRemovableStorage,                      // wksCapabilityRemovableStorage,
    rsWellKnownSidCapabilityAppointments,                          // wksCapabilityAppointments,
    rsWellKnownSidCapabilityContacts,                              // wksCapabilityContacts,
    rsWellKnownSidBuiltinAnyPackage,                               // wksBuiltinAnyPackage,
    rsWellKnownSidBuiltinAnyRestrictedPackage,                     // wksBuiltinAnyRestrictedPackage,
    rsWellKnownSidNtlmAuthentication,                              // wksNtlmAuthentication,
    rsWellKnownSidSChannelAuthentication,                          // wksSChannelAuthentication,
    rsWellKnownSidDigestAuthentication                             // wksDigestAuthentication,
  );

implementation
uses
  DateUtils,
  LCLIntf,
  LCLType,
  variants,
  mormot.core.data,
  mormot.core.text,
  mormot.core.os,
  mormot.crypt.secure,
  uOmniselect,
  uvispropertieslist,
  uvisadvancedsecurity,
  uvislogonhours,
  uvislogonworkstation,
  uvislistother,
  uvisattributeeditor,
  ufrmrsat,
  ufrmpropertyattributes,
  ufrmpropertyaccount,
  ufrmpropertyaddress,
  ufrmpropertybitlocker,
  ufrmpropertygeneraldefault,
  ufrmpropertygeneralsubnet,
  ufrmpropertygeneralcomputer,
  ufrmpropertygeneralgroup,
  ufrmpropertygeneralou,
  ufrmpropertygeneralsite,
  ufrmpropertygeneraluser,
  ufrmpropertygeneralvolume,
  ufrmpropertylaps,
  ufrmpropertylocation,
  ufrmpropertymanagedby,
  ufrmpropertymember,
  ufrmpropertymemberof,
  ufrmpropertyobject,
  ufrmpropertyoperatingsystem,
  ufrmpropertyorganization,
  ufrmpropertypublishedcertificates,
  ufrmpropertyprofile,
  ufrmpropertysecurity,
  ufrmpropertytelephone,
  ursatldapclient,
  ursatldapclientui,
  uconfig,
  ucommonui,
  udns;

{$R *.lfm}

const
  PROPERTY_USER: Array of TPropertyFrameClass = (
    TFrmPropertyGeneralUser,
    TFrmPropertyAddress,
    TFrmPropertyAccount,
    TFrmPropertyProfile,
    TFrmPropertyTelephone,
    TFrmPropertyOrganization,
    TFrmPropertyPublishedCertificates,
    TFrmPropertyMemberOf,
    TFrmPropertyObject,
    TFrmPropertySecurity,
    TFrmPropertyAttributes
  );

  PROPERTY_GROUP: Array of TPropertyFrameClass = (
    TFrmPropertyGeneralGroup,
    TFrmPropertyMember,
    TFrmPropertyMemberOf,
    TFrmPropertyManagedBy,
    TFrmPropertyObject,
    TFrmPropertySecurity,
    TFrmPropertyAttributes
  );

  PROPERTY_COMPUTER: Array of TPropertyFrameClass = (
    TFrmPropertyGeneralComputer,
    TFrmPropertyOperatingSystem,
    TFrmPropertyMemberOf,
    TFrmPropertyLAPS,
    TFrmPropertyLocation,
    TFrmPropertyManagedBy,
    TFrmPropertyObject,
    TFrmPropertySecurity,
    TFrmPropertyAttributes,
    TFrmPropertyBitLocker
  );

  PROPERTY_ORGANIZATIONAL_UNIT: Array of TPropertyFrameClass = (
    TFrmPropertyGeneralOU,
    TFrmPropertyManagedBy,
    TFrmPropertyObject,
    TFrmPropertySecurity,
    TFrmPropertyAttributes
  );

  PROPERTY_CONTACT: Array of TPropertyFrameClass = (
    TFrmPropertyGeneralUser,
    TFrmPropertyAddress,
    TFrmPropertyTelephone,
    TFrmPropertyOrganization,
    TFrmPropertyMemberOf,
    TFrmPropertyObject,
    TFrmPropertySecurity,
    TFrmPropertyAttributes
  );

  PROPERTY_VOLUME: Array of TPropertyFrameClass = (
    TFrmPropertyGeneralVolume,
    TFrmPropertyManagedBy,
    TFrmPropertyObject,
    TFrmPropertySecurity,
    TFrmPropertyAttributes
  );

  PROPERTY_SITE: Array of TPropertyFrameClass = (
    TFrmPropertyGeneralSite,
    TFrmPropertyLocation,
    TFrmPropertyObject,
    TFrmPropertySecurity,
    TFrmPropertyAttributes
  );

  PROPERTY_SUBNET: Array of TPropertyFrameClass = (
    TFrmPropertyGeneralSubnet,
    TFrmPropertyLocation,
    TFrmPropertyObject,
    TFrmPropertySecurity,
    TFrmPropertyAttributes
  );

  PROPERTY_DEFAULT: Array of TPropertyFrameClass = (
    TFrmPropertyGeneralDefault,
    TFrmPropertyObject,
    TFrmPropertySecurity,
    TFrmPropertyAttributes
  );

//procedure TVisProperties.InitPanelDnsProperties();
//var
//  dnsPropertyAttr: TLdapAttribute;
//  i: Integer;
//  DNSProperty: TDNSProperty;
//  data: TDocVariantData;
//begin
//  Tab_dnsProperties.TabVisible := True;
//
//  dnsPropertyAttr := Attributes.Find('dNSProperty');
//
//  if not Assigned(dnsPropertyAttr) then
//    Exit;
//
//  data.init();
//  TisGrid2.Clear;
//  TisGrid2.BeginUpdate;
//  try
//    for i := 0 to dnsPropertyAttr.Count - 1 do
//    begin
//      if not DNSPropertyBytesToRecord(DNSProperty, PByteArray(dnsPropertyAttr.GetRaw(i))^) then
//        Exit;
//      data.AddOrUpdateValue('dataLength', DNSProperty.DataLength);
//      data.AddOrUpdateValue('nameLength', DNSProperty.NameLength);
//      data.AddOrUpdateValue('flag', DNSProperty.Flag);
//      data.AddOrUpdateValue('version', DNSProperty.Version);
//      data.AddOrUpdateValue('_id', DNSProperty.Id);
//      data.AddOrUpdateValue('id', DnsPropertyIdToString(TDnsPropertyId(DNSProperty.Id)));
//      data.AddOrUpdateValue('data', DNSPropertyDataToString(DNSProperty));
//      data.AddOrUpdateValue('name', DNSProperty.Name);
//      TisGrid2.Data.AddItem(data);
//      data.clear;
//    end;
//  finally
//    TisGrid2.EndUpdate;
//    TisGrid2.LoadData();
//  end;
//end;

//procedure TVisProperties.InitPanelDnsRecord();
//var
//  dnsRecordsAttr: TLdapAttribute;
//  i: Integer;
//  DNSRecord: TDNSRecord;
//  data: TDocVariantData;
//begin
//  Tab_dnsRecords.TabVisible := True;
//
//  dnsRecordsAttr := Attributes.Find('dnsRecord');
//  if not Assigned(dnsRecordsAttr) then
//    Exit;
//
//  data.init();
//  TisGrid3.Clear;
//  TisGrid3.BeginUpdate;
//  try
//    for i := 0 to dnsRecordsAttr.Count - 1 do
//    begin
//      DNSRecordBytesToRecord(DNSRecord, PByteArray(dnsRecordsAttr.GetRaw(i))^);
//      data.AddOrUpdateValue('dataLength', DNSRecord.DataLength);
//      data.AddOrUpdateValue('type', DNSRecord.RecType);
//      data.AddOrUpdateValue('version', DNSRecord.Version);
//      data.AddOrUpdateValue('rank', DNSRecord.Rank);
//      data.AddOrUpdateValue('flags', DNSRecord.Flags);
//      data.AddOrUpdateValue('serial', DNSRecord.Serial);
//      data.AddOrUpdateValue('ttlSeconds', DNSRecord.TtlSeconds);
//      data.AddOrUpdateValue('reserved', DNSRecord.Reserved);
//      data.AddOrUpdateValue('timestamp', DNSRecord.Timestamp);
//      data.AddOrUpdateValue('data', DNSRecordDataToString(DNSRecord));
//      TisGrid3.Data.AddItem(data);
//      data.Clear;
//    end;
//  finally
//    TisGrid3.EndUpdate;
//    TisGrid3.LoadData();
//  end;
//end;

{ TVisProperties }

constructor TVisProperties.Create(TheOwner: TComponent;
  ADistinguishedName: RawUtf8);
begin
  Inherited Create(TheOwner);

  fDistinguishedName := ADistinguishedName;

  if not Assigned(FrmRSAT) or
     not Assigned(FrmRSAT.LdapClient) or
     not FrmRSAT.LdapClient.Connected or
     (fDistinguishedName = '') then
    Exit;

  IniPropStorage1.IniFileName := VisBakFilePath;
  UnifyButtonsWidth([Btn_BottomApply, Btn_BottomCancel, Btn_BottomOK]);

  fProperty := TProperty.Create(FrmRSAT.RSAT);
end;

destructor TVisProperties.Destroy();
begin
  FreeAndNil(fProperty);

  Inherited;
end;

procedure TVisProperties.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  FrmRSAT.CloseProperty(Self);
end;

procedure TVisProperties.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if (ModalResult = mrCancel) and fProperty.IsModified then
  begin
    CanClose := Dialogs.MessageDlg(rsWarning, rsUnsavedChangeQuit, mtWarning, mbYesNoCancel, 0) = mrYes;
    Exit;
   end;
  CanClose := True;
end;

function TVisProperties.FormHelp(Command: Word; Data: PtrInt;
  var CallHelp: Boolean): Boolean;
begin
  result := CallHelp;
  ShowMessage('Help!');
end;

procedure TVisProperties.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    27: Close;
  end;
end;

procedure TVisProperties.FormShow(Sender: TObject);
begin
  if LoadAttributes then
    LoadView;
  MakeFullyVisible;
end;

procedure TVisProperties.Show;
begin
  {$ifndef OPENRSATTESTS}
  inherited Show();
  {$endif}
end;

procedure TVisProperties.SetFocus;
begin
  {$ifndef OPENRSATTESTS}
  inherited SetFocus;
  {$endif}
end;

function TVisProperties.LoadAttributes: Boolean;
var
  LdapObject: TLdapResult;
begin
  result := False;
  // Fetch data
  fProperty.RSAT.LdapClient.SearchRangeBegin;
  try
    fProperty.RSAT.LdapClient.OnSearch := @OnSearchEventFillAttributes;
    LdapObject := fProperty.RSAT.LdapClient.SearchObject(DistinguishedName, '', ['*']);
  finally
    fProperty.RSAT.LdapClient.SearchRangeEnd;
  end;
  if not Assigned(LdapObject) then
  begin
    Close();
    Exit;
  end;
  result := True;
end;

procedure TVisProperties.OnSearchEventFillAttributes(Sender: TObject);
var
  LdapClient: TRsatLdapClient;
begin
  LdapClient := (Sender as TRsatLdapClient);

  // Fill Attributes
  fProperty.Attributes := LdapClient.SearchResult.Items[0].Attributes;
  fProperty.RSAT.LdapClient.OnSearch := nil;
end;

procedure TVisProperties.LoadView;
var
  property_tabs: Array of TPropertyFrameClass;
  property_tab: TPropertyFrameClass;
begin
  property_tabs := PROPERTY_DEFAULT;
  case fProperty.objectType of
  'user', 'inetOrgPerson': property_tabs := PROPERTY_USER;
  'group': property_tabs := PROPERTY_GROUP;
  'computer': property_tabs := PROPERTY_COMPUTER;
  'organizationalUnit': property_tabs := PROPERTY_ORGANIZATIONAL_UNIT;
  'contact': property_tabs := PROPERTY_CONTACT;
  'volume': property_tabs := PROPERTY_VOLUME;
  {$ifdef DEVMODE}
  'dnsZone':
  begin
    //InitPanelDefaultGeneral();
    //InitPanelObject();
    //InitPanelSecurity();
    //InitPanelAttributes();
    //InitPanelDnsProperties();
  end;
  'dnsNode':
  begin
    //InitPanelDefaultGeneral();
    //InitPanelObject();
    //InitPanelSecurity();
    //InitPanelAttributes();
    //InitPanelDnsRecord();
  end;
  {$endif}
  'site': property_tabs := PROPERTY_SITE;
  'subnet': property_tabs := PROPERTY_SUBNET;
  end;

  for property_tab in property_tabs do
    NewTab(property_tab);
  UpdateTabs;
end;

procedure TVisProperties.UpdateTabs;
var
  PropertyFrame: TPropertyFrame;
begin
  for PropertyFrame in fPropertyFrameList do
    PropertyFrame.Update(fProperty);
end;

procedure TVisProperties.NewTab(NewFrameClass: TPropertyFrameClass);
var
  Tab: TTabSheet;
  Frame: TPropertyFrame;
begin
  Tab := PageControl.AddTabSheet;
  Frame := NewFrameClass.Create(Tab);
  Frame.Parent := Tab;
  Frame.Align := alClient;
  Tab.Caption := Frame.Caption;
  Insert(Frame, fPropertyFrameList, High(fPropertyFrameList));
end;

procedure TVisProperties.PageControlChange(Sender: TObject);
begin
  TSynLog.Add.Log(sllDebug, FormatUtf8('TVisProperties: PageControlChange: %', [PageControl.ActivePage.Name]));
end;

procedure TVisProperties.Action_ApplyExecute(Sender: TObject);
begin
  fProperty.ApplyModification;
end;

procedure TVisProperties.Action_ApplyUpdate(Sender: TObject);
begin
  Action_Apply.Enabled := fProperty.IsModified;
end;

procedure TVisProperties.Action_CancelExecute(Sender: TObject);
begin
  Close();
end;

procedure TVisProperties.Action_OKExecute(Sender: TObject);
begin
  Action_Apply.Execute();
  Close();
end;

end.
