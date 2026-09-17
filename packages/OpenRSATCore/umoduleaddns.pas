unit umoduleaddns;

{$mode objfpc}{$H+}
{$WARN 6058 OFF}

interface

uses
  Classes,
  SysUtils,
  mormot.core.base,
  mormot.net.ldap,
  mormot.core.variants,
  umodule,
  umoduleaddnsoption,
  uoption,
  udns,
  ursat;

type

  TZoneDnsStorage = class;

  TThreadRefreshDNSZoneEvent = procedure(const ZoneStorage: TZoneDnsStorage) of object;

  { TThreadRefreshDNSZone }

  /// This thread retrieve all children of a dnsZone.
  /// It's usefull to provide GUI feedback while retrieving all nodes on huge domains.
  TThreadRefreshDNSZone = class(TThread)
  private
    /// Ldap Client instance to perform LDAP operations.
    fLdapClient: TLdapClient;
    /// Zone Storage instance where dnsZone children data are located.
    fCurrentZoneStorage: TZoneDnsStorage;

    fOnStart: TThreadRefreshDNSZoneEvent;
    fOnFinish: TThreadRefreshDNSZoneEvent;
    fOnUpdate: TThreadRefreshDNSZoneEvent;
    fOnError: TThreadRefreshDNSZoneEvent;

    procedure OnSearchPage(Sender: TLdapClient);
  protected
    procedure Execute; override;

    procedure DoNotifyStart;
    procedure DoNotifyFinish;
    procedure DoNotifyUpdate;
    procedure DoNotifyError;
  public
    constructor Create(ALdapClient: TLdapClient; ACurrentZone: TZoneDnsStorage); reintroduce;

    property OnStart: TThreadRefreshDNSZoneEvent read fOnStart write fOnStart;
    property OnFinish: TThreadRefreshDNSZoneEvent read fOnFinish write fOnFinish;
    property OnUpdate: TThreadRefreshDNSZoneEvent read fOnUpdate write fOnUpdate;
    property OnError: TThreadRefreshDNSZoneEvent read fOnError write fOnError;
  end;

  { TZoneDnsStorage }

  /// This class provide a fast storage for DNSZone children.
  /// It stores data in array for fast iteration.
  /// To retrieve all data of a dnsNode, you need to know its index in the storage.
  /// To retrieve the index of a dnsNode, you need its name or objectName.
  TZoneDnsStorage = class
  private
    fErrorMessage: RawUtf8;
    /// DNSZone objectName.
    fZoneObjectName: RawUtf8;
    /// DNSZone dc
    fDC: RawUtf8;
    /// DNSZone dnsProperty
    fDnsProperties: TRawByteStringDynArray;
    /// DNSZone name
    fName: RawUtf8;

    /// Item count in storage.
    fCount: Integer;
    /// Capacity of the storage.
    fCapacity: Integer;

    /// Storage for objectNames.
    fObjectNames: TRawUtf8DynArray;
    /// Storage for Names.
    fNames: TRawUtf8DynArray;
    /// Storage for objectClasses.
    fObjectClasses: TRawUtf8DynArrayDynArray;
    /// Storage for DNSRecords.
    fDnsRecords: Array of TRawByteStringDynArray;
    /// Storage for WhenChanged.
    fWhenChanged: TRawUtf8DynArray;

    fDocVariantData: TDocVariantData;
    function GetCountRecords: Integer;
    function GetIsReverseZone: Boolean;
  public
    constructor Create(AZoneObjectName: RawUtf8);

    /// Increase the storage capacity, to prevent iteration on memory allocation.
    /// It is mostly used to increase the storage capacity by the number of objects retrieve on a LDAP search.
    procedure IncreaseStorageCapacity(ACount: Integer);
    /// Empty the storage.
    procedure Clear;

    function ToDocVariantData(): PDocVariantData; overload;
    function ToDocVariantData(const Path: RawUtf8): PDocVariantData; overload;

    /// Get a dnsNode index in storage from its objectName.
    function GetByObjectName(AObjectName: RawUtf8): Integer;
    /// Get a dnsNode index in storage from its name.
    function GetByName(AName: RawUtf8): Integer;
    /// Add a dnsNode in storage from its objectName.
    function AddByObjectName(AObjectName: RawUtf8): Integer;
    /// Add a dnsNode in storage from its name.
    function AddByName(AName: RawUtf8): Integer;
    /// Get or add a dnsNode in storage. May be slow.
    function GetOrAddByObjectName(AObjectName: RawUtf8): Integer;
    /// Get or add a dnsNode in storage. May be slow.
    function GetOrAddByName(AName: RawUtf8): Integer;
    /// Not implemented.
    function DelByObjectName(AObjectName: RawUtf8): Integer;
    /// Not implemented.
    function DelByName(AName: RawUtf8): Integer;
    /// Update the dnsNode name at index.
    procedure UpdateName(AIndex: Integer; AName: RawUtf8);
    /// Update the dnsNode objectClass at index.
    procedure UpdateObjectClass(AIndex: Integer; AObjectClass: TRawUtf8DynArray);
    /// Update the dnsNode dnsRecords at index.
    procedure UpdateDnsRecord(AIndex: Integer; ADnsRecord: TLdapAttribute);
    /// Update the dnsNode whenChanged at index.
    procedure UpdateWhenChanged(AIndex: Integer; AWhenChanged: RawUtf8);
    /// Get the dnsNode object name at index.
    function GetObjectName(AIndex: Integer): RawUtf8;
    /// Get the dnsNode name at index.
    function GetName(AIndex: Integer): RawUtf8;
    /// Get the dnsNode objectClass at index.
    function GetObjectClass(AIndex: Integer): TRawUtf8DynArray;
    /// Get the dnsNode dnsRecords at index.
    function GetDnsRecord(AIndex: Integer): TRawByteStringDynArray;
    /// Get the dnsNode whenChanged at index.
    function GetWhenChanged(AIndex: Integer): RawUtf8;

    /// Read-only property to know the number of object in the storage.
    property Count: Integer read fCount;
    property CountRecords: Integer read GetCountRecords;
    /// Read-only property to know the storage objectName.
    property ZoneObjectName: RawUtf8 read fZoneObjectName;

    property DC: RawUtf8 read fDC write fDC;
    property DnsProperties: TRawByteStringDynArray read fDnsProperties write fDnsProperties;
    property Name: RawUtf8 read fName write fName;
    property IsReverseZone: Boolean read GetIsReverseZone;
    property ErrorMessage: RawUtf8 read fErrorMessage write fErrorMessage;
  end;

  TZoneDnsStorageDynArray = Array of TZoneDnsStorage;

  { TModuleADDNS }

  TModuleADDNS = class(TModule)
  private
    fZoneStorages: TZoneDnsStorageDynArray;

    fUpdateZoneThread: TThreadRefreshDNSZone;
    fCurrentZoneStorage: TZoneDnsStorage;

    fDocVariantData: TDocVariantData;

    fOnStart: TThreadRefreshDNSZoneEvent;
    fOnFinish: TThreadRefreshDNSZoneEvent;
    fOnUpdate: TThreadRefreshDNSZoneEvent;
    fOnError: TThreadRefreshDNSZoneEvent;

    function GetADDNSOption: TModuleADDNSOption;
  public
    constructor Create(ARSAT: TRSAT); reintroduce;
    destructor Destroy; override;

    function GetZoneDnsStorage(ADistinguishedName: RawUtf8): TZoneDnsStorage;
    function GetZoneDnsStorageByName(AName: RawUtf8): TZoneDnsStorage;
    function AddZoneDnsStorage(ADistinguishedName: RawUtf8): TZoneDnsStorage;
    function GetOrAddZoneDnsStorage(ADistinguishedName: RawUtf8): TZoneDnsStorage;
    procedure UpdateZone(ADistinguishedName: RawUtf8);
    function ZoneStorage(ADistinguishedName: RawUtf8): TZoneDnsStorage;
    function ZoneStoragesToDocVariantData(AReverseZone: Boolean): PDocVariantData;
    procedure StopPreviousUpdate;

    procedure ReloadZones;
    procedure UpdateZoneStorage(DistinguishedName: RawUtf8; var UpdatedStorages: TRawUtf8DynArray);
    procedure AfterUpdateZoneStorage(UpdatedStorages: TRawUtf8DynArray);
    function GetZoneNames: TRawUtf8DynArray;

    property OnStart: TThreadRefreshDNSZoneEvent read fOnStart write fOnStart;
    property OnFinish: TThreadRefreshDNSZoneEvent read fOnFinish write fOnFinish;
    property OnUpdate: TThreadRefreshDNSZoneEvent read fOnUpdate write fOnUpdate;
    property OnError: TThreadRefreshDNSZoneEvent read fOnError write fOnError;

    property CurrentZoneStorage: TZoneDnsStorage read fCurrentZoneStorage;
    property ADDNSOption: TModuleADDNSOption read GetADDNSOption;
  end;

implementation
uses
  mormot.core.rtti,
  mormot.core.text,
  mormot.net.dns,
  ucommon;

const
  DOMAIN_DNS_ZONES: String = 'CN=MicrosoftDNS,DC=DomainDnsZones';
  FOREST_DNS_ZONES: String = 'CN=MicrosoftDNS,DC=ForestDnsZones';

{ TThreadRefreshDNSZone }

procedure TThreadRefreshDNSZone.OnSearchPage(Sender: TLdapClient);
var
  Item: TLdapResult;
  idx: Integer;
begin
  if (sender.SearchPageCount = 0) then
    Exit;
  if Terminated then
    Sender.SearchAllAbort;

  fCurrentZoneStorage.IncreaseStorageCapacity(Sender.SearchResult.Count);
  for Item in Sender.SearchResult.Items do
  begin
    if not Assigned(Item) then
      continue;

    idx := fCurrentZoneStorage.AddByObjectName(Item.ObjectName);
    fCurrentZoneStorage.UpdateName(idx, Item.Find('name').GetReadable());
    fCurrentZoneStorage.UpdateObjectClass(idx, Item.Find('objectClass').GetAllReadable);
    fCurrentZoneStorage.UpdateWhenChanged(idx, Item.Find('whenChanged').GetReadable());
    fCurrentZoneStorage.UpdateDnsRecord(idx, Item.Find('dnsRecord'));
  end;
  Synchronize(@DoNotifyUpdate);
end;

procedure TThreadRefreshDNSZone.Execute;
var
  Bak: TOnLdapClientEvent;
  SearchResult: TDocVariantData;
begin
  Synchronize(@DoNotifyStart);
  try
    fCurrentZoneStorage.Clear;

    Bak := fLdapClient.OnSearchPage;
    try
      fLdapClient.SearchScope := lssSingleLevel;
      fLdapClient.OnSearchPage := @OnSearchPage;
      if not fLdapClient.SearchAllDocRaw(SearchResult, fCurrentZoneStorage.ZoneObjectName, '', ['name', 'dnsRecord', 'whenChanged'], [roAutoRange, roKnownValuesAsArray, roObjectNameAtRoot, roRawValues]) then
      begin
        fCurrentZoneStorage.ErrorMessage := FormatUtf8('LDAP: Search (%): %', [fLdapClient.ResultCode, fLdapClient.ResultString]);
        Synchronize(@DoNotifyError);
        Exit;
      end;
    finally
      fLdapClient.OnSearchPage := Bak;
    end;
  finally
    Synchronize(@DoNotifyFinish);
  end;
end;

procedure TThreadRefreshDNSZone.DoNotifyStart;
begin
  if Assigned(fOnStart) then
    fOnStart(fCurrentZoneStorage);
end;

procedure TThreadRefreshDNSZone.DoNotifyFinish;
begin
  if Assigned(fOnFinish) then
    fOnFinish(fCurrentZoneStorage);
end;

procedure TThreadRefreshDNSZone.DoNotifyUpdate;
begin
  if Assigned(fOnUpdate) then
    fOnUpdate(fCurrentZoneStorage);
end;

procedure TThreadRefreshDNSZone.DoNotifyError;
begin
  if Assigned(fOnError) then
    fOnError(fCurrentZoneStorage);;
end;

constructor TThreadRefreshDNSZone.Create(ALdapClient: TLdapClient;
  ACurrentZone: TZoneDnsStorage);
begin
  inherited Create(True);

  FreeOnTerminate := False;
  fLdapClient := ALdapClient;
  fCurrentZoneStorage := ACurrentZone;
end;

{ TZoneDnsStorage }

function TZoneDnsStorage.GetIsReverseZone: Boolean;
begin
  result := String(Name).EndsWith('in-addr.arpa');
end;

function TZoneDnsStorage.GetCountRecords: Integer;
var
  i: Integer;
begin
  result := 0;
  for i := 0 to Count - 1 do
    Inc(result, Length(fDnsRecords[i]));
end;

constructor TZoneDnsStorage.Create(AZoneObjectName: RawUtf8);
begin
  fZoneObjectName := AZoneObjectName;
  fDC := '';
  fDnsProperties := nil;
  fName := '';

  fCount := 0;
  fCapacity := 0;

  fObjectNames := nil;
  fNames := nil;
  fObjectClasses := nil;
  fDnsRecords := nil;
  fWhenChanged := nil;

  fDocVariantData.Init();
end;

procedure TZoneDnsStorage.IncreaseStorageCapacity(ACount: Integer);
begin
  Inc(fCapacity, ACount);
  SetLength(fObjectNames, fCapacity);
  SetLength(fNames, fCapacity);
  SetLength(fObjectClasses, fCapacity);
  SetLength(fDnsRecords, fCapacity);
  SetLength(fWhenChanged, fCapacity);
end;

procedure TZoneDnsStorage.Clear;
begin
  fCapacity := 0;
  fCount := 0;

  fObjectNames := nil;
  fNames := nil;
  fObjectClasses := nil;
  fDnsRecords := nil;
  fWhenChanged := nil;
end;

function ReverseDnsToIP(const AZone, ARecord: RawUtf8): RawUtf8;
var
  Parts: TRawUtf8DynArray;
  i: Integer;

  function EndsWithText(const Value, Suffix: RawUtf8): Boolean;
  begin
    result := False;
    if Length(Value) < Length(Suffix) then
      Exit;
    result := Copy(Value, Length(Value) - Length(Suffix) + 1, Length(Suffix)) = Suffix;
  end;

  procedure AddParts(const Value: RawUtf8);
  begin
    Parts := Concat(Parts ,TRawUtf8DynArray(String(Value).Split('.')));
  end;

begin
  result := '';
  Parts := nil;
  if EndsWithText(AZone, 'in-addr.arpa') then
  begin
    AddParts(ARecord);
    AddParts(Copy(AZone, 0, Length(AZone) - Length('in-addr.arpa')));

    for i := 0 to 3 do
    begin
      if i > 0 then
        result := FormatUtf8('.%', [result]);
      result := FormatUtf8('%%', [parts[i], result]);
    end;
  end
  else if EndsWithText(AZone, 'ip6.arpa') then
  begin

  end;
end;

procedure DNSRecordToData(out D: TDocVariantData; const ZoneN, NodeN, ObjectN: RawUtf8; const R: RawByteString);
var
  DNSRecord: TDNSRecord;
begin
  D.Init(JSON_FAST);
  if not DNSRecordBytesToRecord(DNSRecord, PByteArray(R)^) then
    Exit;
  if TDnsResourceRecord(DNSRecord.RecType) = drrPTR then
    D.AddValue('name', ReverseDnsToIP(ZoneN, NodeN))
  else
    D.AddValue('name', NodeN);
  D.AddValue('data', DNSRecordDataToString(DNSRecord));
  D.AddValue('_type', DNSRecord.RecType);
  D.AddValue('type',  DnsResourceRecordToStr(TDnsResourceRecord(dnsRecord.RecType)));
  D.AddValue('timestamp', '');
  D.AddValue('rawdata', R);
  D.AddValue('objectName', ObjectN);
end;

function TZoneDnsStorage.ToDocVariantData(): PDocVariantData;
var
  i: Integer;
  RawDnsRecord: RawByteString;
  newRaw: TDocVariantData;
  DNSName, DNSObjectName: RawUtf8;
begin
  newRaw.Init(JSON_FAST);
  fDocVariantData.Clear;
  for i := 0 to Pred(Count) do
  begin
    DNSName := GetName(i);
    if DNSName = '@' then
      DNSName := '(Same as parent folder)';
    DNSObjectName := GetObjectName(i);
    for RawDnsRecord in GetDnsRecord(i) do
    begin
      DNSRecordToData(NewRaw, Name, DNSName, DNSObjectName, RawDnsRecord);
      fDocVariantData.AddItem(newRaw);
      newRaw.Clear;
    end;
  end;
  result := @fDocVariantData;
end;

function TZoneDnsStorage.ToDocVariantData(const Path: RawUtf8): PDocVariantData;
var
  RawDnsRecord: RawByteString;
  i, p: Integer;
  DNSObjectName, DNSFullName, DNSRelativeName: RawUtf8;
  NewRaw, Folders: TDocVariantData;
  tmp: String;

  function GetParentDNSName(const DNSName: RawUtf8): RawUtf8;
  var
    Idx: SizeInt;
  begin
    Idx := Pos('.', DNSName);
    result := Copy(DNSName, Idx + 1, Length(DNSName) - Idx);
  end;

begin
  if IsReverseZone then
  begin
    result := ToDocVariantData();
    Exit;
  end;
  Folders.Init(JSON_FAST);
  fDocVariantData.Clear;
  for i := 0 to Count - 1 do
  begin
    DNSFullName := GetName(i);
    if (DNSFullName = '@') or not String(DNSFullName).EndsWith(Path) then
    begin
      tmp := Copy(Path, 2, Length(Path) - 1);
      if (DNSFullName = '@') or (DNSFullName = tmp) then
      begin
        DNSObjectName := GetObjectName(i);
        for RawDnsRecord in GetDnsRecord(i) do
        begin
          DNSRecordToData(NewRaw, Name, '(Same as parent folder)', DNSObjectName, RawDnsRecord);
          fDocVariantData.AddItem(newRaw);
          newRaw.Clear;
        end;
      end;
      Continue;
    end;

    DNSRelativeName := Copy(DNSFullName, 0, Length(DNSFullName) - Length(Path));
    p := Pos('.', DNSRelativeName);
    if p <> 0 then
    begin
      p := Length(DNSRelativeName);
      while (DNSRelativeName[p] <> '.') and (P > 0) do
        Dec(p);
      DNSRelativeName := Copy(DNSRelativeName, p + 1, Length(DNSRelativeName) - p);
      if not Folders.Exists(DNSRelativeName) then
      begin
        Folders.AddValue(DNSRelativeName, True);
        NewRaw.Init(JSON_FAST);
        NewRaw.AddValue('name', DNSRelativeName);
        fDocVariantData.AddItem(newRaw);
        newRaw.Clear;
      end;
      Continue;
    end;

    DNSObjectName := GetObjectName(i);
    for RawDnsRecord in GetDnsRecord(i) do
    begin
      DNSRecordToData(NewRaw, Name, DNSRelativeName, DNSObjectName, RawDnsRecord);
      fDocVariantData.AddItem(newRaw);
      newRaw.Clear;
    end;
  end;
  result := @fDocVariantData;
end;

function TZoneDnsStorage.GetByObjectName(AObjectName: RawUtf8): Integer;
begin
  for result := 0 to Pred(fCount) do
    if fObjectNames[result] = AObjectName then
      Exit;
  result := -1;
end;

function TZoneDnsStorage.GetByName(AName: RawUtf8): Integer;
begin
  for result := 0 to Pred(fCount) do
    if fNames[result] = AName then
      Exit;
  result := -1;
end;

function TZoneDnsStorage.AddByObjectName(AObjectName: RawUtf8): Integer;
begin
  if fCount = fCapacity then
    IncreaseStorageCapacity(1);
  result := fCount;
  fObjectNames[result] := AObjectName;
  Inc(fCount);
end;

function TZoneDnsStorage.AddByName(AName: RawUtf8): Integer;
begin
  if fCount = fCapacity then
    IncreaseStorageCapacity(1);
  result := fCount;
  fObjectNames[result] := AName;
  Inc(fCount);
end;

function TZoneDnsStorage.GetOrAddByObjectName(AObjectName: RawUtf8): Integer;
begin
  result := GetByObjectName(AObjectName);
  if result < 0 then
    result := AddByObjectName(AObjectName);
end;

function TZoneDnsStorage.GetOrAddByName(AName: RawUtf8): Integer;
begin
  result := GetByName(AName);
  if result < 0 then
    result := AddByName(AName);
end;

function TZoneDnsStorage.DelByObjectName(AObjectName: RawUtf8): Integer;
begin
  result := -1;
end;

function TZoneDnsStorage.DelByName(AName: RawUtf8): Integer;
begin
  result := -1;
end;

procedure TZoneDnsStorage.UpdateName(AIndex: Integer; AName: RawUtf8);
begin
  fNames[AIndex] := AName;
end;

procedure TZoneDnsStorage.UpdateObjectClass(AIndex: Integer;
  AObjectClass: TRawUtf8DynArray);
begin
  DynArrayCopy(@fObjectClasses[AIndex], @AObjectClass, TypeInfo(TRawUtf8DynArray));
end;

procedure TZoneDnsStorage.UpdateDnsRecord(AIndex: Integer;
  ADnsRecord: TLdapAttribute);
var
  i: Integer;
begin
  if not Assigned(ADnsRecord) then
    Exit;
  SetLength(fDnsRecords[AIndex], ADnsRecord.Count);
  for i := 0 to Pred(ADnsRecord.Count) do
    fDnsRecords[AIndex][i] := ADnsRecord.GetRaw(i);
end;

procedure TZoneDnsStorage.UpdateWhenChanged(AIndex: Integer; AWhenChanged: RawUtf8
  );
begin
  fWhenChanged[AIndex] := AWhenChanged;
end;

function TZoneDnsStorage.GetObjectName(AIndex: Integer): RawUtf8;
begin
  result := fObjectNames[AIndex];
end;

function TZoneDnsStorage.GetName(AIndex: Integer): RawUtf8;
begin
  result := fNames[AIndex];
end;

function TZoneDnsStorage.GetObjectClass(AIndex: Integer): TRawUtf8DynArray;
begin
  result := fObjectClasses[AIndex];
end;

function TZoneDnsStorage.GetDnsRecord(AIndex: Integer): TRawByteStringDynArray;
begin
  result := fDnsRecords[AIndex];
end;

function TZoneDnsStorage.GetWhenChanged(AIndex: Integer): RawUtf8;
begin
  result := fWhenChanged[AIndex];
end;

{ TModuleADDNS }

function TModuleADDNS.GetADDNSOption: TModuleADDNSOption;
begin
  result := (fOption as TModuleADDNSOption);
end;

constructor TModuleADDNS.Create(ARSAT: TRSAT);
begin
  inherited Create('DomainNameSystem', rsModuleDNSDisplayName);

  fOption := TModuleADDNSOption.Create;
  fRSAT := ARSAT;

  fZoneStorages := nil;
  fUpdateZoneThread := nil;
end;

destructor TModuleADDNS.Destroy;
var
  i: Integer;
begin
  StopPreviousUpdate;
  FreeAndNil(fOption);
  for i := 0 to High(fZoneStorages) do
    FreeAndNil(fZoneStorages[i]);

  inherited Destroy;
end;

function TModuleADDNS.GetZoneDnsStorage(ADistinguishedName: RawUtf8
  ): TZoneDnsStorage;
var
  i: Integer;
begin
  result := nil;
  for i := 0 to High(fZoneStorages) do
    if Assigned(fZoneStorages[i]) and (fZoneStorages[i].ZoneObjectName = ADistinguishedName) then
    begin
      result := fZoneStorages[i];
      Exit;
    end;
end;

function TModuleADDNS.GetZoneDnsStorageByName(AName: RawUtf8): TZoneDnsStorage;
var
  i: Integer;
begin
  result := nil;
  for i := 0 to High(fZoneStorages) do
    if Assigned(fZoneStorages[i]) and (fZoneStorages[i].Name = AName) then
    begin
      result := fZoneStorages[i];
      Exit;
    end;
end;

function TModuleADDNS.AddZoneDnsStorage(ADistinguishedName: RawUtf8
  ): TZoneDnsStorage;
var
  i: Integer;
begin
  result := TZoneDnsStorage.Create(ADistinguishedName);
  for i := 0 to High(fZoneStorages) do
    if not Assigned(fZoneStorages[i]) then
    begin
      fZoneStorages[i] := result;
      Exit;
    end;
  Insert(result, fZoneStorages, Length(fZoneStorages));
end;

function TModuleADDNS.GetOrAddZoneDnsStorage(ADistinguishedName: RawUtf8
  ): TZoneDnsStorage;
begin
  result := GetZoneDnsStorage(ADistinguishedName);
  if not Assigned(result) then
    result := AddZoneDnsStorage(ADistinguishedName);
end;

procedure TModuleADDNS.UpdateZone(ADistinguishedName: RawUtf8);
begin
  if not Assigned(fRSAT) then
    Exit;

  StopPreviousUpdate;
  fCurrentZoneStorage := GetOrAddZoneDnsStorage(ADistinguishedName);
  if not Assigned(fCurrentZoneStorage) then
    Exit;
  fUpdateZoneThread := TThreadRefreshDNSZone.Create(fRSAT.LdapClient, fCurrentZoneStorage);
  fUpdateZoneThread.OnStart := fOnStart;
  fUpdateZoneThread.OnFinish := fOnFinish;
  fUpdateZoneThread.OnUpdate := fOnUpdate;
  fUpdateZoneThread.OnError := fOnError;
  fUpdateZoneThread.Start;
end;

function TModuleADDNS.ZoneStorage(ADistinguishedName: RawUtf8): TZoneDnsStorage;
begin
  result := GetZoneDnsStorage(ADistinguishedName);
end;

function TModuleADDNS.ZoneStoragesToDocVariantData(AReverseZone: Boolean
  ): PDocVariantData;
var
  i: Integer;
  NewRow: TDocVariantData;
  IsReverseZone: Boolean;
begin
  fDocVariantData.Clear;
  NewRow.Init();
  for i := 0 to Pred(Length(fZoneStorages)) do
  begin
    if not Assigned(fZoneStorages[i]) then
      continue;
    if (fZoneStorages[i].Name = 'RootDNSServers') then
      continue;
    IsReverseZone := String(fZoneStorages[i].DC).EndsWith('in-addr.arpa') or String(fZoneStorages[i].DC).EndsWith('ip6.arpa');
    if not (IsReverseZone = AReverseZone) then
      continue;
    NewRow.AddValue('name', fZoneStorages[i].Name);
    NewRow.AddValue('objectName', fZoneStorages[i].ZoneObjectName);
    fDocVariantData.AddItem(NewRow);
    NewRow.Clear;
  end;
  result := @fDocVariantData;
end;

procedure TModuleADDNS.StopPreviousUpdate;
begin
  if Assigned(fUpdateZoneThread) then
  begin
    if not fUpdateZoneThread.Finished then
    begin
      fUpdateZoneThread.Terminate;
      fUpdateZoneThread.WaitFor;
    end;
    FreeAndNil(fUpdateZoneThread);
  end;
end;

procedure TModuleADDNS.ReloadZones;
var
  UpdatedStorages: TRawUtf8DynArray;
begin
  UpdatedStorages := nil;
  UpdateZoneStorage(FormatUtf8('%,%', [DOMAIN_DNS_ZONES, fRSAT.LdapClient.DefaultDN]), UpdatedStorages);
  UpdateZoneStorage(FormatUtf8('%,%', [FOREST_DNS_ZONES, fRSAT.LdapClient.RootDN]), UpdatedStorages);

  AfterUpdateZoneStorage(UpdatedStorages);
end;

procedure TModuleADDNS.UpdateZoneStorage(DistinguishedName: RawUtf8;
  var UpdatedStorages: TRawUtf8DynArray);
var
  SearchResult: TLdapResult;
  ZoneDNSStorage: TZoneDnsStorage;
  Attribute: TLdapAttribute;
  i: Integer;
  DnsProperties: TRawByteStringDynArray;
begin
  fRSAT.LdapClient.SearchBegin();
  try
    fRSAT.LdapClient.SearchScope := lssSingleLevel;
    repeat
      fRSAT.LdapClient.SearchRangeBegin;
      try
        if not fRSAT.LdapClient.Search(DistinguishedName, False, '', ['dc', 'name', 'dNSProperty']) then
          Exit;
      finally
        fRSAT.LdapClient.SearchRangeEnd;
      end;

      for SearchResult in fRSAT.LdapClient.SearchResult.Items do
      begin
        if not Assigned(SearchResult) then
          continue;
        Insert(SearchResult.ObjectName, UpdatedStorages, Length(UpdatedStorages));
        ZoneDNSStorage := GetOrAddZoneDnsStorage(SearchResult.ObjectName);
        ZoneDNSStorage.DC := SearchResult.Find('dc').GetReadable();
        ZoneDNSStorage.Name := SearchResult.Find('name').GetReadable();
        Attribute := SearchResult.Find('dNSProperty');
        if Assigned(Attribute) then
        begin
          DnsProperties := nil;
          SetLength(DnsProperties, Attribute.Count);
          for i := 0 to Pred(Attribute.Count) do
            DnsProperties[i] := Attribute.GetRaw(i);
          ZoneDNSStorage.DnsProperties := DnsProperties;
        end;
      end;

    until fRSAT.LdapClient.SearchCookie = '';
  finally
    fRSAT.LdapClient.SearchEnd;
  end;
end;

procedure TModuleADDNS.AfterUpdateZoneStorage(UpdatedStorages: TRawUtf8DynArray
  );
var
  i, j: Integer;
  Found: Boolean;
begin
  for i := Pred(Length(fZoneStorages)) downto 0 do
  begin
    Found := False;
    for j := 0 to Pred(Length(UpdatedStorages)) do
    begin
      Found := UpdatedStorages[j] = fZoneStorages[i].ZoneObjectName;
      if Found then
        Break;
    end;
    if not Found then
    begin
      FreeAndNil(fZoneStorages[i]);
      Delete(fZoneStorages, i, 1);
    end;
  end;
end;

function TModuleADDNS.GetZoneNames: TRawUtf8DynArray;
var
  i: Integer;
  Len: SizeInt;
begin
  result := nil;
  Len := Length(fZoneStorages);
  SetLength(result, Len);
  for i := 0 to Pred(Len) do
    result[i] := fZoneStorages[i].Name;
end;

end.

