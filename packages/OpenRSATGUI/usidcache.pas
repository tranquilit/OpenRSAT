unit usidcache;

{$mode ObjFPC}{$H+}

interface

uses
  // Lazarus / fpc
  Classes,
  SysUtils,
  // Submodules
  mormot.core.base,
  mormot.core.log,
  mormot.core.os.security,
  mormot.core.text,
  mormot.core.variants,
  mormot.net.ldap,
  // Rsat
  ucommon;

type

  { TSidCache }

  TSidCache = class
    cache: TDocVariantData;
    Ldap: TLdapClient;
    fBaseDN: RawUtf8;
  public
    /// Find the common name of a RawSid.
    /// It first looks into TWellKnownSid, then query ldap,
    /// and finally just transform it to text.
    /// Argument:
    /// - sid(RawSid): SID to convert.
    /// Return:
    /// The SID name as RawUtf8.
    function SidNameFromRawSid(sid: RawSid): RawUtf8;
    function SidsNamesFromRawSids(sids: Array of RawSid): TRawUtf8DynArray;
    function SidsNamesFromSids(sids: TRawUtf8DynArray): TRawUtf8DynArray;
    constructor Create(_Ldap: TLdapClient; aBaseDN: RawUtf8);
  end;

const
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

{ TSidCache }

constructor TSidCache.Create(_Ldap: TLdapClient; aBaseDN: RawUtf8);
begin
  cache.Init();
  Ldap := _Ldap;
  fBaseDN := aBaseDN;
end;

function TSidCache.SidNameFromRawSid(sid: RawSid): RawUtf8;
var
  Utf8Sid: RawUtf8;
begin
  Utf8Sid := RawSidToText(sid);

  // Get from cache and fast exit
  if cache.Exists(Utf8Sid) then
  begin
    result := cache.S[Utf8Sid];
    if (result <> '') then
      Exit;
  end;

  // Get name from WELL_KNOWN_SID_NAMES
  result := WELL_KNOWN_SID_NAMES[SidToKnown(Utf8Sid)];
  if (result = '') then
    // Get name from ldap
    result := Ldap.SearchObject(atName, Ldap.DefaultDN(fBaseDN), Format('(objectSid=%s)', [RawSidToText(sid)]), lssWholeSubtree).GetReadable();
  if (result = '') then
    // Get name as raw sid
    result := RawSidToText(sid);
  cache.S[Utf8Sid] := result;
end;

function TSidCache.SidsNamesFromRawSids(sids: array of RawSid): TRawUtf8DynArray;
var
  i, len: Integer;
begin
  result := [];
  len := Length(sids);

  for i := 0 to len - 1 do
    Insert(RawSidToText(sids[i]), result, i);
  result := SidsNamesFromSids(result);
end;

function TSidCache.SidsNamesFromSids(sids: TRawUtf8DynArray): TRawUtf8DynArray;
var
  aLog: ISynLog;
  Filter: RawUtf8;
  len, i: Integer;
  item: TLdapResult;
begin
  aLog := TSynLog.Enter('SidsNamesFromRawSids', []);
  result := [];
  Filter := '(|';

  // Retrieve missing sids
  len := Length(Sids);
  for i := 0 to len - 1 do
  begin
    if cache.Exists(sids[i]) then
      continue;
    if (SidToKnown(sids[i]) <> wksNull) then
    begin
      cache.S[sids[i]] := WELL_KNOWN_SID_NAMES[SidToKnown(sids[i])];
      continue;
    end;
    if Assigned(aLog) then
      aLog.Log(sllDebug, FormatUtf8('Missing sid: %', [sids[i]]));
    Filter := FormatUtf8('%(objectSid=%)', [Filter, sids[i]]);
  end;
  Filter := FormatUtf8('%)', [Filter]);

  if (Filter <> '(|)') then
  begin
    Ldap.SearchBegin({VisMain.Storage.Options.SearchPageSize});
    Ldap.SearchScope := lssWholeSubtree;
    repeat
      if not Ldap.Search(Ldap.DefaultDN(fBaseDN), False, Filter, ['name', 'objectSid']) then
        raise Exception.Create(Ldap.ResultString);
      for item in Ldap.SearchResult.Items do
        cache.S[item.Find('objectSid').GetReadable()] := item.Find('name').GetReadable();
    until (Ldap.SearchCookie = '');
    Ldap.SearchEnd;
  end;

  if Assigned(aLog) then
    aLog.Log(sllDebug, 'Retrieve from cache.');
  len := Length(sids);
  for i := 0 to len - 1 do
  begin
    if not cache.Exists(sids[i]) then
      cache.S[sids[i]] := sids[i];
    Insert(cache.S[sids[i]], result, i);
  end;
end;

end.

