unit udns;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.base,
  mormot.core.test,
  mormot.net.dns;

type

  // The structure is used to represent an array of IPv4 addresses. This structure cannot represent IPv6 addresses.
  // https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-dnsp/588ae296-71bf-402f-9996-86ecee39dc29
  TDNSIPArray = packed record
    // The number of IPv4 addresses present in the AddrArray member.
    AddrCount: Cardinal;
    // An array of IPv4 addresses. An IPv4 address is represented as a 32-bit unsigned integer in network byte order.
    AddrArray: Array of Cardinal;
  end;

  TDNSAddr = packed record
    MaxSa: packed record
      AddressFamily: Word;
      PortNumber: Word;
      IPv4Address: Cardinal;
      IPv6Address: Array[0..3] of Cardinal;
      Padding: QWord;
    end;
    DnsAddrUserDword: packed record
      SockAddrLength: Cardinal;
      SubnetLength: Cardinal;
      Flags: Cardinal;
      //packed record
      //  T: Boolean;
      //  Zero: Byte;
      //  RTT: Word;
      //  ValidationStatus: Word;
      //end;
      Padding: Array[0..4] of Cardinal;
    end;
  end;

  TDNSAddrArray = packed record
    MaxCount: Cardinal;
    AddrCount: Cardinal;
    Tag: Cardinal;
    Family: Word;
    WordReserved: Word;
    Flags: Cardinal;
    MatchFlag: Cardinal;
    Reserved1: Cardinal;
    Reserved2: Cardinal;
    AddrArray: Array of TDNSAddr;
  end;

  // https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-dnsp/445c7843-e4a1-4222-8c0f-630c230a4c80
  TDNSProperty = record
    // DataLength (4 bytes): An unsigned binary integer containing the length, in bytes, of the Data field. If this value is 0, default values are assigned to Data.
    DataLength: Cardinal;
    // NameLength (4 bytes): Not Used. The value MUST be ignored and assumed to be 0x00000001.
    NameLength: Cardinal;
    // Flag (4 bytes): This field is reserved for future use. The value MUST be 0x00000000.
    Flag: Cardinal;
    // Version (4 bytes): The version number associated with the property attribute. The value MUST be 0x00000001.
    Version: Cardinal;
    // Id (4 bytes): The property attribute's type.
    Id: Cardinal;
    // Data (variable): The data associated with an Id.
    Data: Array[0..$ffff] of Byte;
    // Name (1 byte): Not used. The value MUST be of length 1 byte and MUST be ignored.
    Name: Cardinal;
  end;

  // https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-dnsp/3af63871-0cc4-4179-916c-5caade55a8f3
  TDnsPropertyId = (
    dpidPropertyZoneType = $1, // https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-dnsp/27e138a7-110c-44a4-afcb-b95f35f00306
      // Default: DNS_ZONE_TYPE_PRIMARY
      //
      // DNS_ZONE_TYPE_CACHE = $0,
      // DNS_ZONE_TYPE_PRIMARY = $1,
      // DNS_ZONE_TYPE_SECONDARY = $2,
      // DNS_ZONE_TYPE_STUB = $3,
      // DNS_ZONE_TYPE_FORWARDER = $4,
      // DNS_ZONE_TYPE_SECONDARY_CACHE = $5
    dpidPropertyZoneAllowUpdate = $2, // https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-dnsp/e8651544-0fbb-4038-8232-375ff2d8a55e  ->  fAllowUpdate
      // ZONE_UPDATE_OFF = $0,
      // ZONE_UPDATE_UNSECURE = $1
      // ZONE_UPDATE_SECURE = $2
    dpidPropertyZoneSecureTime = $8, // https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-dnsp/e72286df-3e4c-4535-9a0c-e9cc944248c2  ->  Time Zone Secured
      // Default: 0
    dpidPropertyZoneNoRefreshInterval = $10, // https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-dnsp/e8651544-0fbb-4038-8232-375ff2d8a55e  ->  dwNoRefreshInterval
      // Default: 168
      // The time interval, in hours, that is configured as NoRefresh interval value for this zone.
    dpidPropertyZoneScavengingServers = $11, // https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-dnsp/588ae296-71bf-402f-9996-86ecee39dc29
      // IP4_ARRAY = record
      //   AddrCount: Cardinal;
      //   AddrArray: Array of Cardinal
    dpidPropertyZoneAgingEnabledTime = $12,
      // Default: 0
      // The time interval, in hours, that is available before the scheduled next scavenging cycle for this zone.
    dpidPropertyZoneRefreshInterval = $20, // https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-dnsp/e8651544-0fbb-4038-8232-375ff2d8a55e  ->  dwRefreshInterval
      // Default: 168
      // The time interval, in hours, that is configured as the refresh interval value for this zone.
    dpidPropertyZoneAgingState = $40, // https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-dnsp/e8651544-0fbb-4038-8232-375ff2d8a55e  ->  fAging
      // Default: 0
      // FALSE = $0,
      // TRUE = $1
    dpidPropertyZoneDeletedFromHostname = $80,
    dpidPropertyZoneMasterServers = $81,
    dpidPropertyZoneAutoNSServers = $82,
    dpidPropertyZoneDCPromoConvert = $83,
    dpidPropertyZoneScavengingServersDA = $90,
    dpidPropertyZoneMasterServersDA = $91,
    dpidPropertyZoneAutoNSServersDA = $92,
    dpidPropertyZoneNodeDBFlags = $100
  );

  // The zone type.
  // Default: dpztDnsZoneTypePrimary
  // https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-dnsp/27e138a7-110c-44a4-afcb-b95f35f00306
  // TDNSPropertyID.dpidPropertyZoneType
  TDNSPropertyZoneType = (
    dpztDnsZoneTypeCache,
    dpztDnsZoneTypePrimary,
    dpztDnsZoneTypeSecondary,
    dpztDnsZoneTypeStub,
    dpztDnsZoneTypeForwarder,
    dpztDnsZoneTypeSecondaryCache
  );

  // Whether dynamic updates are allowed.
  // https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-dnsp/e8651544-0fbb-4038-8232-375ff2d8a55e
  // fAllowUpdate
  // TDNSPropertyId.dpidPropertyZoneAllowUpdate
  TDNSPropertyZoneAllowUpdate = (
    dpzauDnsZoneUpdateOff,
    dpzauDnsZoneUpdateUnsecure,
    dpzauDnsZoneUpdateSecure
  );

  // The time at which the zone became secure.
  // Default: 0
  // https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-dnsp/e72286df-3e4c-4535-9a0c-e9cc944248c2
  // Time Zone Secured
  // TDNSProperty.dpidPropertyZoneSecureTime
  TDNSPropertyZoneSecureTime = QWord;

  // The zone no refresh interval.
  // Default: 168 hours / 7 days
  // https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-dnsp/e8651544-0fbb-4038-8232-375ff2d8a55e
  // dwNoRefreshInterval
  // TDNSProperty.dpidPropertyZoneNoRefreshInterval
  TDNSPropertyZoneNoRefreshInterval = Cardinal;

  // The zone refresh interval.
  // Default: 168 hours / 7 days
  // https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-dnsp/e8651544-0fbb-4038-8232-375ff2d8a55e
  // dwRefreshInterval
  // TDNSProperty.dpidPropertyZoneRefreshInterval
  TDNSPropertyZoneRefreshInterval = Cardinal;

  // Whether aging is enabled.
  // Default: 0
  // https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-dnsp/e8651544-0fbb-4038-8232-375ff2d8a55e
  // fAging
  // TDNSProperty.dpidPropertyZoneAgingState
  TDNSPropertyZoneAgingState = Boolean;

  // A list of DNS servers that will perform scavenging. The list is formatted as an IP4 ARRAY.
  // This value is applicable for zones of type DNS_ZONE_TYPE_PRIMARY (section DNS_ZONE_TYPE) only. If this value is NULL,
  // there are no restrictions on which DNS server can perform scavenging for this zone.
  // Default: Empty array
  // https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-dnsp/e8651544-0fbb-4038-8232-375ff2d8a55e
  // aipScavengeServers
  //
  // IP ARRAY: https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-dnsp/588ae296-71bf-402f-9996-86ecee39dc29
  TDNSPropertyZoneScavengingServers = TDNSIPArray;

  // The time interval before the next scavenging cycle.
  // The time interval, in hours, that is available before the scheduled next scavenging cycle for this zone.
  // Default: 0
  // https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-dnsp/e8651544-0fbb-4038-8232-375ff2d8a55e
  // dwAvailForScavengeTime
  TDNSPropertyZoneAgingEnabledTime = Cardinal;

  // The name of the server that deleted the zone. The value is a null-terminated Unicode string. The server SHOULD ignore this value.
  TDNSPropertyZoneDeletedFromHostname = String;

  // A list of DNS server that will perform zone transfers. The list is formatted as an IP4 array.
  // Default: Empty array
  TDNSPropertyZoneMasterServers = TDNSIPArray;

  // A list of servers which MAY autocreate a delegation. The list is formatted as an IP4 array.
  // Default: Empty array
  TDNSPropertyZoneAutoNSServers = TDNSIPArray;

  // https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-dnsp/4ec7bdf7-1807-4179-96af-ce1c1cd448b7
  TDNSPropertyZoneDCPromoConvert = (
    // No change to existing zone storage.
    dpzdcpcConvertNone,
    // Zone is to be moved to the DNS domain partition.
    dpzdcpcConvertDomain,
    // Zone is to be moved to the DNS forest partition.
    dpzdcpcConvertForest
  );

  TDNSPropertyZoneScavengingSeversDA = TDNSAddrArray;

  { TDNSRecord }

  // https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-dnsp/6912b338-5472-4f59-b912-0edb536b6ed8
  TDNSRecord = record
    DataLength: Word;
    RecType: Word;
    Version: Byte;
    Rank: Byte;
    Flags: Word;
    Serial: cardinal;
    TtlSeconds: cardinal;
    Reserved: cardinal;
    Timestamp: cardinal;
    RData: Array[0..$ffff] of Byte;
  end;

  // https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-dnsp/3fd41adc-c69e-407b-979e-721251403132
  // https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-dnsp/2af86306-3bd9-4c86-b763-fd33e16e3e5b
  TDNSCOUNTNAME = record
    Length: Byte;
    LabelCount: Byte;
    RawName: Array of record
      Length: Byte;
      Name: String;
    end;
  end;

  // Standard Resource Record

  TRRCname = TDNSCOUNTNAME;

  TRRHinfo = record
    CPU: String;
    OS: String;
  end;

  TRRMB = TDNSCOUNTNAME;

  TRRMD = TDNSCOUNTNAME;

  TRRMF = TDNSCOUNTNAME;

  TRRMG = TDNSCOUNTNAME;

  TRRMinfo = record
    RMailBX: TDNSCOUNTNAME;
    EMailBX: TDNSCOUNTNAME;
  end;

  TRRMR = TDNSCOUNTNAME;

  TRRMX = record
    Preference: Cardinal;
    Exchange: TDNSCOUNTNAME;
  end;

  TRRNS = TDNSCOUNTNAME;

  TRRPTR = TDNSCOUNTNAME;

  TRRSOA = record
    Serial: Cardinal;
    Refresh: Cardinal;
    Retry: Cardinal;
    Expire: Cardinal;
    Minimum: Cardinal;
    MName: TDNSCOUNTNAME;
    RName: TDNSCOUNTNAME;
  end;

  TRRTxt = String;

  // Internet Resource Record

  TRRA = Cardinal;

  TRRWKS = record
    Address: Cardinal;
    Protocol: Byte;
    BitMap: PByte;
  end;

  // Other ?

  // https://www.rfc-editor.org/rfc/rfc2782
  // https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-dnsp/db37cab7-f121-43ba-81c5-ca0e198d4b9a
  TRRSRV = record
    wPriority: Word;
    wWeight: Word;
    wPort: Word;
    nameTarget: TDNSCOUNTNAME;
  end;

  ExceptionInvalidIPFormat = class(Exception) end;

// Convert a TDnsResourceRecord enum into human readable.
function DnsResourceRecordToStr(AValue: TDnsResourceRecord): RawUtf8;

function DnsPropertyIdToString(AValue: TDnsPropertyId): RawUtf8;

operator = (Destination: TDNSCOUNTNAME; Source: TDNSCOUNTNAME): Boolean;

function DNSIPArrayBytesToRecord(out DNSIPArray: TDNSIPArray; const Buffer: TByteArray): Boolean;
function DNSIPArrayRecordToBytes(out Buffer: TByteArray; const DNSIPArray: TDNSIPArray): Integer;
function DNSIPArrayToString(const DNSIPArray: TDNSIPArray): RawUtf8;
function DNSIPArrayToStringFromBytes(const Buffer: TByteArray): RawUtf8;
procedure DNSIPArrayBuild(out DNSIPArray: TDNSIPArray; const IPARRAY: TStringArray);

function DNSAddrBytesToRecord(out DNSAddr: TDNSAddr; const Buffer: TByteArray): Boolean;
function DNSAddrRecordToBytes(out Buffer: TByteArray; const DNSAddr: TDNSAddr): Boolean;
function DNSAddrToString(const DNSAddr: TDNSAddr): RawUtf8;
function DNSAddrToStringFromBytes(const Buffer: TByteArray): RawUtf8;

function DNSAddrArrayBytesToRecord(out DNSAddrArray: TDNSAddrArray; const Buffer: TByteArray): Boolean;
function DNSAddrArrayRecordToBytes(out Buffer: TByteArray; const DNSAddrArray: TDNSAddrArray): Integer;
function DNSAddrArrayToString(const DNSAddrArray: TDNSAddrArray): RawUtf8;
function DNSAddrArrayToStringFromBytes(const Buffer: TByteArray): RawUtf8;

function DNSPropertyBytesToRecord(out DNSProperty: TDNSProperty; const Buffer: TByteArray): Boolean;
function DNSPropertyRecordToBytes(out Buffer: TByteArray; const DNSProperty: TDNSProperty): Integer;
function DNSPropertyToString(const DNSProperty: TDNSProperty): RawUtf8;
function DNSPropertyToStringFromBytes(const Buffer: TByteArray): RawUtf8;
function DNSPropertyDataToString(const DNSProperty: TDNSProperty): RawUtf8;

function DNSPropertyZoneTypeBytesToRecord(out DNSPropertyZoneType: TDNSPropertyZoneType; const Buffer: TByteArray): Boolean;
function DNSPropertyZoneTypeRecordToBytes(out Buffer: TByteArray; const DNSPropertyZoneType: TDNSPropertyZoneType): Integer;
function DNSPropertyZoneTypeToString(const DNSPropertyZoneType: TDNSPropertyZoneType): RawUtf8;
function DNSPropertyZoneTypeToStringFromBytes(const Buffer: TByteArray): RawUtf8;

function DNSPropertyZoneAllowUpdateBytesToRecord(out DNSPropertyZoneAllowUpdate: TDNSPropertyZoneAllowUpdate; const Buffer: TByteArray): Boolean;
function DNSPropertyZoneAllowUpdateRecordToBytes(out Buffer: TByteArray; const DNSPropertyZoneAllowUpdate: TDNSPropertyZoneAllowUpdate): Integer;
function DNSPropertyZoneAllowUpdateToString(const DNSPropertyZoneAllowUpdate: TDNSPropertyZoneAllowUpdate): RawUtf8;
function DNSPropertyZoneAllowUpdateToStringFromBytes(const Buffer: TByteArray): RawUtf8;

function DNSPropertyZoneSecureTimeBytesToRecord(out DNSPropertyZoneSecureTime: TDNSPropertyZoneSecureTime; const Buffer: TByteArray): Boolean;
function DNSPropertyZoneSecureTimeRecordToBytes(out Buffer: TByteArray; const DNSPropertyZoneSecureTime: TDNSPropertyZoneSecureTime): Integer;
function DNSPropertyZoneSecureTimeToString(const DNSPropertyZoneSecureTime: TDNSPropertyZoneSecureTime): RawUtf8;
function DNSPropertyZoneSecureTimeToStringFromBytes(const Buffer: TByteArray): RawUtf8;

function DNSPropertyZoneNoRefreshIntervalBytesToRecord(out DNSPropertyZoneNoRefreshInterval: TDNSPropertyZoneNoRefreshInterval; const Buffer: TByteArray): Boolean;
function DNSPropertyZoneNoRefreshIntervalRecordToBytes(out Buffer: TByteArray; const DNSPropertyZoneNoRefreshInterval: TDNSPropertyZoneNoRefreshInterval): Integer;
function DNSPropertyZoneNoRefreshIntervalToString(const DNSPropertyZoneNoRefreshInterval: TDNSPropertyZoneNoRefreshInterval): RawUtf8;
function DNSPropertyZoneNoRefreshIntervalToStringFromBytes(const Buffer: TByteArray): RawUtf8;

function DNSPropertyZoneRefreshIntervalBytesToRecord(out DNSPropertyZoneRefreshInterval: TDNSPropertyZoneRefreshInterval; const Buffer: TByteArray): Boolean;
function DNSPropertyZoneRefreshIntervalRecordToBytes(out Buffer: TByteArray; const DNSPropertyZoneRefreshInterval: TDNSPropertyZoneRefreshInterval): Integer;
function DNSPropertyZoneRefreshIntervalToString(const DNSPropertyZoneRefreshInterval: TDNSPropertyZoneRefreshInterval): RawUtf8;
function DNSPropertyZoneRefreshIntervalToStringFromBytes(const Buffer: TByteArray): RawUtf8;

function DNSPropertyZoneAgingStateBytesToRecord(out DNSPropertyZoneAgingState: TDNSPropertyZoneAgingState; const Buffer: TByteArray): Boolean;
function DNSPropertyZoneAgingStateRecordToBytes(out Buffer: TByteArray; const DNSPropertyZoneAgingState: TDNSPropertyZoneAgingState): Integer;
function DNSPropertyZoneAgingStateToString(const DNSPropertyZoneAgingState: TDNSPropertyZoneAgingState): RawUtf8;
function DNSPropertyZoneAgingStateToStringFromBytes(const Buffer: TByteArray): RawUtf8;

function DNSPropertyZoneScavengingServersBytesToRecord(out DNSPropertyZoneScavengingServers: TDNSPropertyZoneScavengingServers; const Buffer: TByteArray): Boolean;
function DNSPropertyZoneScavengingServersRecordToBytes(out Buffer: TByteArray; const DNSPropertyZoneScavengingServers: TDNSPropertyZoneScavengingServers): Integer;
function DNSPropertyZoneScavengingServersToString(const DNSPropertyZoneScavengingServers: TDNSPropertyZoneScavengingServers): RawUtf8;
function DNSPropertyZoneScavengingServersToStringFromBytes(const Buffer: TByteArray): RawUtf8;

function DNSPropertyZoneAgingEnabledTimeBytesToRecord(out DNSPropertyZoneAgingEnabledTime: TDNSPropertyZoneAgingEnabledTime; const Buffer: TByteArray): Boolean;
function DNSPropertyZoneAgingEnabledTimeRecordToBytes(out Buffer: TByteArray; const DNSPropertyZoneAgingEnabledTime: TDNSPropertyZoneAgingEnabledTime): Integer;
function DNSPropertyZoneAgingEnabledTimeToString(const DNSPropertyZoneAgingEnabledTime: TDNSPropertyZoneAgingEnabledTime): RawUtf8;
function DNSPropertyZoneAgingEnabledTimeToStringFromBytes(const Buffer: TByteArray): RawUtf8;

function DNSPropertyZoneDeletedFromHostnameBytesToRecord(out DNSPropertyZoneDeletedFromHostname: TDNSPropertyZoneDeletedFromHostname; const Buffer: TByteArray): Boolean;
function DNSPropertyZoneDeletedFromHostnameRecordToBytes(out Buffer: TByteArray; const DNSPropertyZoneDeletedFromHostname: TDNSPropertyZoneDeletedFromHostname): Boolean;
function DNSPropertyZoneDeletedFromHostnameToString(const DNSPropertyZoneDeletedFromHostname: TDNSPropertyZoneDeletedFromHostname): RawUtf8;
function DNSPropertyZoneDeletedFromHostnameToStringFromBytes(const Buffer: TByteArray): RawUtf8;

function DNSPropertyZoneMasterServersBytesToRecord(out DNSPropertyZoneMasterServers: TDNSPropertyZoneMasterServers; const Buffer: TByteArray): Boolean;
function DNSPropertyZoneMasterServersRecordToBytes(out Buffer: TByteArray; const DNSPropertyZoneMasterServers: TDNSPropertyZoneMasterServers): Integer;
function DNSPropertyZoneMasterServersToString(const DNSPropertyZoneMasterServers: TDNSPropertyZoneMasterServers): RawUtf8;
function DNSPropertyZoneMasterServersToStringFromBytes(const Buffer: TByteArray): RawUtf8;

function DNSPropertyZoneAutoNSServersBytesToRecord(out DNSPropertyZoneAutoNSServers: TDNSPropertyZoneAutoNSServers; const Buffer: TByteArray): Boolean;
function DNSPropertyZoneAutoNSServersRecordToBytes(out Buffer: TByteArray; const DNSPropertyZoneAutoNSServers: TDNSPropertyZoneAutoNSServers): Integer;
function DNSPropertyZoneAutoNSServersToString(const DNSPropertyZoneAutoNSServers: TDNSPropertyZoneAutoNSServers): RawUtf8;
function DNSPropertyZoneAutoNSServersToStringFromBytes(const Buffer: TByteArray): RawUtf8;

function DNSPropertyZoneDCPromoConvertBytesToRecord(out DNSPropertyZoneDCPromoConvert: TDNSPropertyZoneDCPromoConvert; const Buffer: TByteArray): Boolean;
function DNSPropertyZoneDCPromoConvertRecordToBytes(out Buffer: TByteArray; const DNSPropertyZoneDCPromoConvert: TDNSPropertyZoneDCPromoConvert): Boolean;
function DNSPropertyZoneDCPromoConvertToString(const DNSPropertyZoneDCPromoConvert: TDNSPropertyZoneDCPromoConvert): RawUtf8;
function DNSPropertyZoneDCPromoConvertToStringFromBytes(const Buffer: TByteArray): RawUtf8;

// Decode a dnsRecord bytes array into a TDNSRecord record.
// https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-dnsp/6912b338-5472-4f59-b912-0edb536b6ed8
function DNSRecordBytesToRecord(out DNSRecord: TDNSRecord; const Buffer: TByteArray): Boolean;
// Encode a TDNSRecord record into a dnsRecord bytes array.
// https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-dnsp/6912b338-5472-4f59-b912-0edb536b6ed8
function DNSRecordRecordToBytes(out Buffer: TByteArray; const DNSRecord: TDNSRecord): Integer;
// Make a TDNSRecord record human readable.
// https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-dnsp/6912b338-5472-4f59-b912-0edb536b6ed8
function DNSRecordToString(const DNSRecord: TDNSRecord): RawUtf8;
// Make a TDNSRecord.RData record human readable.
// https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-dnsp/6912b338-5472-4f59-b912-0edb536b6ed8
function DNSRecordDataToString(const DNSRecord: TDNSRecord): RawUtf8;

// Decode a DNS_COUNT_NAME bytes array into a TDNSCOUNTNAME record.
// https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-dnsp/2af86306-3bd9-4c86-b763-fd33e16e3e5b
function DNSCOUNTNAMEBytesToRecord(out DNSCOUNTNAME: TDNSCOUNTNAME; const Buffer: TByteArray): Boolean;
// Encode a TDNSCOUNTNAME record into a DNS_COUNT_NAME bytes array.
// https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-dnsp/2af86306-3bd9-4c86-b763-fd33e16e3e5b
function DNSCOUNTNAMERecordToBytes(out Buffer: TByteArray; const DNSCOUNTNAME: TDNSCOUNTNAME): Integer;
// Make a DNSCOUNTNAME human readable.
// https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-dnsp/2af86306-3bd9-4c86-b763-fd33e16e3e5b
function DNSCOUNTNAMEToString(const DNSCOUNTNAME: TDNSCOUNTNAME): RawUtf8;
procedure DNSCOUNTNAMEBuild(var DNSCOUNTNAME: TDNSCOUNTNAME; const AValue: RawUtf8);

// Decode a DNS record CNAME bytes array into a TRRCname record.
function DNSRRCNAMEBytesToRecord(out CNAME: TRRCname; const Buffer: TByteArray): Boolean;
// Encode a TRRCname record into a DNS record CNAME bytes array.
function DNSRRCNAMERecordToBytes(out Buffer: TByteArray; const CNAME: TRRCname): Integer;
// Make a TRRCname record human readable.
function DNSRRCNAMEToString(const CNAME: TRRCname): String;
// Make a DNS record CNAME bytes array human readable.
function DNSRRCNAMEToStringFromBytes(const Buffer: TByteArray): RawUtf8;
procedure DNSRRCNAMEBuild(var CNAME: TRRCname; const AValue: RawUtf8);

// Decode a DNS record A bytes array into a TRRA record.
function DNSRRABytesToRecord(out A: TRRA; const Buffer: TByteArray): Boolean;
// Encode a TRRA record into a DNS record A bytes array.
function DNSRRARecordToBytes(out Buffer: TByteArray; const A: TRRA): Integer;
// Make a TRRA record human readable.
function DNSRRAToString(const A: TRRA): RawUtf8;
// Make a DNS record A bytes array human readable.
function DNSRRAToStringFromBytes(const Buffer: TByteArray): RawUtf8;
procedure DNSRRABuild(var A: TRRA; const AValue: RawUtf8);

// Decode a DNS record SOA bytes array into a TRRSOA record.
function DNSRRSOABytesToRecord(out SOA: TRRSOA; const Buffer: TByteArray): Boolean;
// Encode a TRRSOA record into a DNS record A bytes array.
function DNSRRSOARecordToBytes(out Buffer: TByteArray; const SOA: TRRSOA): Integer;
// Make a TRRSOA human readable.
function DNSRRSOAToString(const SOA: TRRSOA): RawUtf8;
// Make a DNS record SOA bytes array human readable.
function DNSRRSOAToStringFromBytes(const Buffer: TByteArray): RawUtf8;
procedure DNSRRSOABuild(var SOA: TRRSOA;
  const Serial: Cardinal;
  const Refresh: Cardinal;
  const Retry: Cardinal;
  const Expire: Cardinal;
  const Minimum: Cardinal;
  const MName: RawUtf8;
  const RName: RawUtf8
);

// Decode a DNS record PTR bytes array into a TRRPTR record.
function DNSRRPTRBytesToRecord(out APTR: TRRPTR; const Buffer: TByteArray): Boolean;
// Encode a TRRPTR record into a DNS record PTR bytes array.
function DNSRRPTRRecordToBytes(out Buffer: TByteArray; const APTR: TRRPTR): Integer;
// Make a TRRPTR human readable.
function DNSRRPTRToString(const APTR: TRRPTR): String;
// Make a DNS record PTR bytes array human readable.
function DNSRRPTRToStringFromBytes(const Buffer: TByteArray): RawUtf8;
// Build a TRRPTR from arguments.
procedure DNSRRPTRBuild(var APTR: TRRPTR; const AValue: RawUtf8);

// Decode a DNS record MX bytes array into a TRRMX record.
function DNSRRMXBytesToRecord(out MX: TRRMX; const Buffer: TByteArray): Boolean;
// Encode a TRRMX record into a DNS record MX bytes array.
function DNSRRMXRecordToBytes(out Buffer: TByteArray; const MX: TRRMX): Integer;
// Make a TRRMX record human readable.
function DNSRRMXToString(const MX: TRRMX): RawUtf8;
// Make a DNS record MX human readable.
function DNSRRMXToStringFromBytes(const Buffer: TByteArray): RawUtf8;
// Build a TRRMX from arguments.
procedure DNSRRMXBuild(var MX: TRRMX; const Exchange: RawUtf8; const Preference: Cardinal);

// Decode a DNS record SRV bytes array into a TRRSRV record.
function DNSRRSRVBytesToRecord(out SRV: TRRSRV; const Buffer: TByteArray): Boolean;
// Encode a TRRSRV record into a DNS record SRV bytes array.
function DNSRRSRVRecordToBytes(out Buffer: TByteArray; const SRV: TRRSRV): Integer;
// Make a TRRSRV record human readable.
function DNSRRSRVToString(const SRV: TRRSRV): RawUtf8;
// Make a DNS record SRV human readable.
function DNSRRSRVToStringFromBytes(const Buffer: TByteArray): RawUtf8;
// Build a TRRSRV from arguments.
procedure DNSRRSRVBuild(var SRV: TRRSRV; const Priority: Word; const Weight: Word; const Port: Word; const NameTarget: RawUtf8);

function DNSRRNSBytesToRecord(out NS: TRRNS; const Buffer: TByteArray): Boolean;
function DNSRRNSRecordToBytes(out Buffer: TByteArray; const NS: TRRNS): Integer;
function DNSRRNSToString(const NS: TRRNS): RawUtf8;
function DNSRRNSToStringFromBytes(const Buffer: TByteArray): RawUtf8;
procedure DNSRRNSBuild(var NS: TRRNS; const AValue: RawUtf8);

implementation
uses
  mormot.net.sock,
  DateUtils,
  ucommon;


function DNSIPArrayBytesToRecord(out DNSIPArray: TDNSIPArray;
  const Buffer: TByteArray): Boolean;
var
  i: Integer;
begin
  result := False;
  try
    Move(Buffer, DNSIPArray.AddrCount, SizeOf(Cardinal));
    SetLength(DNSIPArray.AddrArray, DNSIPArray.AddrCount);
    for i := 0 to Pred(DNSIPArray.AddrCount) do
      Move(Buffer[SizeOf(Cardinal) + SizeOf(Cardinal) * i], DNSIPArray.AddrArray[i], SizeOf(Cardinal));
    result := True;
  finally
  end;
end;

function DNSIPArrayRecordToBytes(out Buffer: TByteArray;
  const DNSIPArray: TDNSIPArray): Integer;
var
  i: Integer;
begin
  result := 0;
  try
    Move(DNSIPArray.AddrCount, Buffer, SizeOf(Cardinal));
    for i := 0 to Pred(DNSIPArray.AddrCount) do
      Move(DNSIPArray.AddrArray[i], Buffer[4 + SizeOf(Cardinal) * i], SizeOf(Cardinal));
    result := SizeOf(Cardinal) + SizeOf(Cardinal) * DNSIPArray.AddrCount;
  finally
  end;
end;

function DNSIPArrayToString(const DNSIPArray: TDNSIPArray): RawUtf8;
var
  i: Integer;
begin
  result := '';
  if Length(DNSIPArray.AddrArray) <= 0 then
    Exit;
  result := Format('[%s', [DNSIPArray.AddrArray[0]]);
  for i := 1 to Pred(DNSIPArray.AddrCount) do
    result := Format('%s, %s', [result, DNSIPArray.AddrArray[i]]);
  result := Format('%s]', [result]);
end;

function DNSIPArrayToStringFromBytes(const Buffer: TByteArray): RawUtf8;
var
  DNSIPArray: TDNSIPArray;
begin
  result := '';
  if DNSIPArrayBytesToRecord(DNSIPArray, Buffer) then
    result := DNSIPArrayToString(DNSIPArray);
end;

procedure DNSIPArrayBuild(out DNSIPArray: TDNSIPArray;
  const IPARRAY: TStringArray);
var
  netAddr: TNetAddr;
  i: Integer;
begin
  DNSIPArray.AddrCount := Length(IPARRAY);
  SetLength(DNSIPArray.AddrArray, DNSIPArray.AddrCount);
  for i := 0 to Pred(DNSIPArray.AddrCount) do
  begin
    if not netAddr.SetFromIP4(IPARRAY[i], True) then
      Exit;
    DNSIPArray.AddrArray[i] := netAddr.IP4;
    netAddr.Clear;
  end;
end;

function DNSAddrBytesToRecord(out DNSAddr: TDNSAddr; const Buffer: TByteArray
  ): Boolean;
var
  idx: Integer;
begin
  result := False;
  try
    Move(Buffer, DNSAddr.MaxSa.AddressFamily, SizeOf(Word));
    Move(Buffer[SizeOf(Word)], DNSAddr.MaxSa.PortNumber, SizeOf(Word));
    Move(Buffer[SizeOf(Word) * 2], DNSAddr.MaxSa.IPv4Address, SizeOf(Cardinal));
    Move(Buffer[SizeOf(Word) * 2 + SizeOf(Cardinal)], DNSAddr.MaxSa.IPv6Address, SizeOf(Cardinal) * 4);
    Move(Buffer[SizeOf(Word) * 2 + SizeOf(Cardinal) * 5], DNSAddr.MaxSa.Padding, SizeOf(QWord));
    idx := SizeOf(Word) * 2 + SizeOf(Cardinal) * 5 + SizeOf(QWord);
    Move(Buffer[idx], DNSAddr.DnsAddrUserDword.SockAddrLength, SizeOf(Cardinal));
    Move(Buffer[idx + SizeOf(Cardinal)], DNSAddr.DnsAddrUserDword.SubnetLength, SizeOf(Cardinal));
    Move(Buffer[idx + SizeOf(Cardinal) * 2], DNSAddr.DnsAddrUserDword.Flags, SizeOf(Cardinal));
    Move(Buffer[idx + SizeOf(Cardinal) * 3], DNSAddr.DnsAddrUserDword.Padding, SizeOf(Cardinal) * 5);
    result := True;
  finally
  end;
end;

function DNSAddrRecordToBytes(out Buffer: TByteArray; const DNSAddr: TDNSAddr
  ): Boolean;
var
  idx: Integer;
begin
  result := False;
  try
    Move(DNSAddr.MaxSa.AddressFamily, Buffer, SizeOf(Word));
    Move(DNSAddr.MaxSa.PortNumber, Buffer[SizeOf(Word)], SizeOf(Word));
    Move(DNSAddr.MaxSa.IPv4Address, Buffer[SizeOf(Word) * 2], SizeOf(Cardinal));
    Move(DNSAddr.MaxSa.IPv6Address, Buffer[SizeOf(Word) * 2 + SizeOf(Cardinal)], SizeOf(Cardinal) * 4);
    Move(DNSAddr.MaxSa.Padding, Buffer[SizeOf(Word) * 2 + SizeOf(Cardinal) * 5], SizeOf(QWord));
    idx := SizeOf(Word) * 2 + SizeOf(Cardinal) * 5 + SizeOf(QWord);
    Move(DNSAddr.DnsAddrUserDword.SockAddrLength, Buffer[idx], SizeOf(Cardinal));
    Move(DNSAddr.DnsAddrUserDword.SubnetLength, Buffer[idx + SizeOf(Cardinal)], SizeOf(Cardinal));
    Move(DNSAddr.DnsAddrUserDword.Flags, Buffer[idx + SizeOf(Cardinal) * 2], SizeOf(Cardinal));
    Move(DNSAddr.DnsAddrUserDword.Padding, Buffer[idx + SizeOf(Cardinal) * 3], SizeOf(Cardinal) * 5);
    result := True;
  finally
  end;
end;

function DNSAddrToString(const DNSAddr: TDNSAddr): RawUtf8;
begin
  result := '';
end;

function DNSAddrToStringFromBytes(const Buffer: TByteArray): RawUtf8;
var
  DNSAddr: TDNSAddr;
begin
  result := '';
  if DNSAddrBytesToRecord(DNSAddr, Buffer) then
    result := DNSAddrToString(DNSAddr);
end;

function DNSAddrArrayBytesToRecord(out DNSAddrArray: TDNSAddrArray;
  const Buffer: TByteArray): Boolean;
var
  idx, i: Integer;
begin
  result := False;
  try
    Move(Buffer, DNSAddrArray.MaxCount, SizeOf(Cardinal));
    Move(Buffer[SizeOf(Cardinal)], DNSAddrArray.AddrCount, SizeOf(Cardinal));
    Move(Buffer[SizeOf(Cardinal) * 2], DNSAddrArray.Tag, SizeOf(Cardinal));
    Move(Buffer[SizeOf(Cardinal) * 3], DNSAddrArray.Family, SizeOf(Word));
    Move(Buffer[SizeOf(Cardinal) * 3 + SizeOf(Word)], DNSAddrArray.WordReserved, SizeOf(Word));
    Move(Buffer[SizeOf(Cardinal) * 3 + SizeOf(Word) * 2], DNSAddrArray.Flags, SizeOf(Cardinal));
    Move(Buffer[SizeOf(Cardinal) * 4 + SizeOf(Word) * 2], DNSAddrArray.MatchFlag, SizeOf(Cardinal));
    Move(Buffer[SizeOf(Cardinal) * 5 + SizeOf(Word) * 2], DNSAddrArray.Reserved1, SizeOf(Cardinal));
    Move(Buffer[SizeOf(Cardinal) * 6 + SizeOf(Word) * 2], DNSAddrArray.Reserved2, SizeOf(Cardinal));
    idx := SizeOf(Cardinal) * 7 + SizeOf(Word) * 2;
    SetLength(DNSAddrArray.AddrArray, DNSAddrArray.MaxCount);
    for i := 0 to Pred(DNSAddrArray.MaxCount) do
      DNSAddrBytesToRecord(DNSAddrArray.AddrArray[i], PByteArray(@Buffer[idx + i * SizeOf(TDNSAddr)])^);
    result := True;
  finally
  end;
end;

function DNSAddrArrayRecordToBytes(out Buffer: TByteArray;
  const DNSAddrArray: TDNSAddrArray): Integer;
var
  i: Integer;
begin
  result := -1;
  try
    Move(DNSAddrArray.MaxCount, Buffer, SizeOf(Cardinal));
    Move(DNSAddrArray.AddrCount, Buffer[SizeOf(Cardinal)], SizeOf(Cardinal));
    Move(DNSAddrArray.Tag, Buffer[SizeOf(Cardinal) * 2], SizeOf(Cardinal));
    Move(DNSAddrArray.Family, Buffer[SizeOf(Cardinal) * 3], SizeOf(Word));
    Move(DNSAddrArray.WordReserved, Buffer[SizeOf(Cardinal) * 3 + SizeOf(Word)], SizeOf(Word));
    Move(DNSAddrArray.Flags, Buffer[SizeOf(Cardinal) * 3 + SizeOf(Word) * 2], SizeOf(Cardinal));
    Move(DNSAddrArray.MatchFlag, Buffer[SizeOf(Cardinal) * 4 + SizeOf(Word) * 2], SizeOf(Cardinal));
    Move(DNSAddrArray.Reserved1, Buffer[SizeOf(Cardinal) * 5 + SizeOf(Word) * 2], SizeOf(Cardinal));
    Move(DNSAddrArray.Reserved2, Buffer[SizeOf(Cardinal) * 6 + SizeOf(Word) * 2], SizeOf(Cardinal));
    result := SizeOf(Cardinal) * 7 + SizeOf(Word) * 2;
    for i := 0 to Pred(DNSAddrArray.MaxCount) do
      DNSAddrRecordToBytes(PByteArray(@Buffer[result + SizeOf(TDNSAddr) * i])^, DNSAddrArray.AddrArray[i]);
    result := result + DNSAddrArray.MaxCount * SizeOf(TDNSAddr);
  finally
  end;
end;

function DNSAddrArrayToString(const DNSAddrArray: TDNSAddrArray): RawUtf8;
begin
  result := '';
end;

function DNSAddrArrayToStringFromBytes(const Buffer: TByteArray): RawUtf8;
var
  DNSAddrArray: TDNSAddrArray;
begin
  result := '';
  if DNSAddrArrayBytesToRecord(DNSAddrArray, Buffer) then
    result := DNSAddrArrayToString(DNSAddrArray);
end;

function DNSPropertyBytesToRecord(out DNSProperty: TDNSProperty;
  const Buffer: TByteArray): Boolean;
var
  CardinalSize: Integer;
begin
  result := False;
  try
    CardinalSize := SizeOf(Cardinal);
    Move(Buffer, DNSProperty.DataLength, CardinalSize);
    Move(Buffer[CardinalSize], DNSProperty.NameLength, CardinalSize);
    Move(Buffer[CardinalSize * 2], DNSProperty.Flag, CardinalSize);
    Move(Buffer[CardinalSize * 3], DNSProperty.Version, CardinalSize);
    Move(Buffer[CardinalSize * 4], DNSProperty.Id, CardinalSize);
    Move(Buffer[CardinalSize * 5], DNSProperty.Data, DNSProperty.DataLength);
    Move(Buffer[CardinalSize * 5 + DNSProperty.DataLength], DNSProperty.Name, SizeOf(Cardinal));
    result := True;
  finally
  end;
end;

function DNSPropertyRecordToBytes(out Buffer: TByteArray;
  const DNSProperty: TDNSProperty): Integer;
var
  CardinalSize: Integer;
begin
  result := 0;
  try
    CardinalSize := SizeOf(Cardinal);
    Move(DNSProperty.DataLength, Buffer, CardinalSize);
    Move(DNSProperty.NameLength, Buffer[CardinalSize], CardinalSize);
    Move(DNSProperty.Flag, Buffer[CardinalSize * 2], CardinalSize);
    Move(DNSProperty.Version, Buffer[CardinalSize * 3], CardinalSize);
    Move(DNSProperty.Id, Buffer[CardinalSize * 4], CardinalSize);
    Move(DNSProperty.Data, Buffer[CardinalSize * 5], DNSProperty.DataLength);
    Move(DNSProperty.Name, Buffer[CardinalSize * 5 + DNSProperty.DataLength], SizeOf(Cardinal));
    result := CardinalSize * 5 + DNSProperty.DataLength + SizeOf(Cardinal);
  finally
  end;
end;

function DNSPropertyToString(const DNSProperty: TDNSProperty): RawUtf8;
begin
  result := Format('[%d][%d][%d][%d][%d][%s][%d]', [DNSProperty.DataLength, DNSProperty.NameLength, DNSProperty.Flag, DNSProperty.Version, DNSProperty.Id, DNSPropertyDataToString(DNSProperty), DNSProperty.Name]);
end;

function DNSPropertyToStringFromBytes(const Buffer: TByteArray): RawUtf8;
var
  DNSProperty: TDNSProperty;
begin
  result := '';
  if DNSPropertyBytesToRecord(DNSProperty, Buffer) then
    result := DNSPropertyToString(DNSProperty);
end;

function DNSPropertyDataToString(const DNSProperty: TDNSProperty): RawUtf8;
var
  pdata: PByteArray;
begin
  result := '';
  pdata := PByteArray(@DNSProperty.Data);
  case TDnsPropertyId(DNSProperty.Id) of
    dpidPropertyZoneType: result := DNSPropertyZoneTypeToStringFromBytes(pdata^);
    dpidPropertyZoneAllowUpdate: result := DNSPropertyZoneAllowUpdateToStringFromBytes(pdata^);
    dpidPropertyZoneSecureTime: result := DNSPropertyZoneSecureTimeToStringFromBytes(pdata^);
    dpidPropertyZoneNoRefreshInterval: result := DNSPropertyZoneNoRefreshIntervalToStringFromBytes(pdata^);
    dpidPropertyZoneRefreshInterval: result := DNSPropertyZoneRefreshIntervalToStringFromBytes(pdata^);
    dpidPropertyZoneAgingState: result := DNSPropertyZoneAgingStateToStringFromBytes(pdata^);
    dpidPropertyZoneScavengingServers: result := DNSPropertyZoneScavengingServersToStringFromBytes(pdata^);
    dpidPropertyZoneAgingEnabledTime: result := DNSPropertyZoneAgingEnabledTimeToStringFromBytes(pdata^);
    dpidPropertyZoneDeletedFromHostname: result := DNSPropertyZoneDeletedFromHostnameToStringFromBytes(pdata^);
    dpidPropertyZoneMasterServers: result := DNSPropertyZoneMasterServersToStringFromBytes(pdata^);
    dpidPropertyZoneAutoNSServers: result := DNSPropertyZoneAutoNSServersToStringFromBytes(pdata^);
    dpidPropertyZoneDCPromoConvert: result := DNSPropertyZoneDCPromoConvertToStringFromBytes(pdata^);
  end;
end;

function DNSPropertyZoneTypeBytesToRecord(out
  DNSPropertyZoneType: TDNSPropertyZoneType; const Buffer: TByteArray): Boolean;
begin
  result := False;
  if (Buffer[0] >= Ord(Low(TDNSPropertyZoneType))) and (Buffer[0] <= Ord(High(TDNSPropertyZoneType))) then
  begin
    DNSPropertyZoneType := TDNSPropertyZoneType(Buffer[0]);
    result := True;
  end;
end;

function DNSPropertyZoneTypeRecordToBytes(out Buffer: TByteArray;
  const DNSPropertyZoneType: TDNSPropertyZoneType): Integer;
begin
  Move(DNSPropertyZoneType, Buffer, 4);
  //Buffer[0] := Ord(DNSPropertyZoneType);
  result := 4;
end;

function DNSPropertyZoneTypeToString(
  const DNSPropertyZoneType: TDNSPropertyZoneType): RawUtf8;
begin
  result := '';
  case DNSPropertyZoneType of
    dpztDnsZoneTypeCache: result := rsDNSZoneTypeCache;
    dpztDnsZoneTypePrimary: result := rsDNSZoneTypePrimary;
    dpztDnsZoneTypeSecondary: result := rsDNSZoneTypeSecondary;
    dpztDnsZoneTypeStub: result := rsDNSZoneTypeStub;
    dpztDnsZoneTypeForwarder: result := rsDNSZoneTypeForwarder;
    dpztDnsZoneTypeSecondaryCache: result := rsDNSZoneTypeSecondaryCache;
  end;
end;

function DNSPropertyZoneTypeToStringFromBytes(const Buffer: TByteArray
  ): RawUtf8;
var
  DNSPropertyZoneType: TDNSPropertyZoneType;
begin
  result := '';
  if DNSPropertyZoneTypeBytesToRecord(DNSPropertyZoneType, Buffer) then
    result := DNSPropertyZoneTypeToString(DNSPropertyZoneType);
end;

function DNSPropertyZoneAllowUpdateBytesToRecord(out
  DNSPropertyZoneAllowUpdate: TDNSPropertyZoneAllowUpdate;
  const Buffer: TByteArray): Boolean;
begin
  result := False;
  if (Buffer[0] >= Ord(Low(TDNSPropertyZoneAllowUpdate))) and (Buffer[0] <= Ord(High(TDNSPropertyZoneAllowUpdate))) then
  begin
    DNSPropertyZoneAllowUpdate := TDNSPropertyZoneAllowUpdate(Buffer[0]);
    result := True;
  end;
end;

function DNSPropertyZoneAllowUpdateRecordToBytes(out Buffer: TByteArray;
  const DNSPropertyZoneAllowUpdate: TDNSPropertyZoneAllowUpdate): Integer;
begin
  Buffer[0] := Ord(DNSPropertyZoneAllowUpdate);
  result := 1;
end;

function DNSPropertyZoneAllowUpdateToString(
  const DNSPropertyZoneAllowUpdate: TDNSPropertyZoneAllowUpdate): RawUtf8;
begin
  result := '';
  case DNSPropertyZoneAllowUpdate of
    dpzauDnsZoneUpdateOff: result := rsDNSZoneUpdateOff;
    dpzauDnsZoneUpdateUnsecure: result := rsDNSZoneUpdateUnsecure;
    dpzauDnsZoneUpdateSecure: result := rsDNSZoneUpdateSecure;
  end;
end;

function DNSPropertyZoneAllowUpdateToStringFromBytes(const Buffer: TByteArray
  ): RawUtf8;
var
  DNSPropertyZoneAllowUpdate: TDNSPropertyZoneAllowUpdate;
begin
  result := '';
  if DNSPropertyZoneAllowUpdateBytesToRecord(DNSPropertyZoneAllowUpdate, Buffer) then
    result := DNSPropertyZoneAllowUpdateToString(DNSPropertyZoneAllowUpdate);
end;

function DNSPropertyZoneSecureTimeBytesToRecord(out
  DNSPropertyZoneSecureTime: TDNSPropertyZoneSecureTime;
  const Buffer: TByteArray): Boolean;
begin
  Move(Buffer, DNSPropertyZoneSecureTime, SizeOf(TDNSPropertyZoneSecureTime));
  result := True;
end;

function DNSPropertyZoneSecureTimeRecordToBytes(out Buffer: TByteArray;
  const DNSPropertyZoneSecureTime: TDNSPropertyZoneSecureTime): Integer;
begin
  Move(DNSPropertyZoneSecureTime, Buffer, SizeOf(TDNSPropertyZoneSecureTime));
  result := SizeOf(TDNSPropertyZoneSecureTime);
end;

function DNSPropertyZoneSecureTimeToString(
  const DNSPropertyZoneSecureTime: TDNSPropertyZoneSecureTime): RawUtf8;
var
  BaseDate: TDateTime;
begin
  // 01/01/1601
  BaseDate := TDateTime(-109205);
  result := DateTimeToStr(IncSecond(BaseDate, DNSPropertyZoneSecureTime));
end;

function DNSPropertyZoneSecureTimeToStringFromBytes(const Buffer: TByteArray
  ): RawUtf8;
var
  DNSPropertyZoneSecureTime: TDNSPropertyZoneSecureTime;
begin
  result := '';
  if DNSPropertyZoneSecureTimeBytesToRecord(DNSPropertyZoneSecureTime, Buffer) then
    result := DNSPropertyZoneSecureTimeToString(DNSPropertyZoneSecureTime);
end;

function DNSPropertyZoneNoRefreshIntervalBytesToRecord(out
  DNSPropertyZoneNoRefreshInterval: TDNSPropertyZoneNoRefreshInterval;
  const Buffer: TByteArray): Boolean;
begin
  result := False;
  try
    Move(Buffer, DNSPropertyZoneNoRefreshInterval, SizeOf(Cardinal));
    result := True;
  finally
  end;
end;

function DNSPropertyZoneNoRefreshIntervalRecordToBytes(out Buffer: TByteArray;
  const DNSPropertyZoneNoRefreshInterval: TDNSPropertyZoneNoRefreshInterval
  ): Integer;
begin
  result := 0;
  try
    Move(DNSPropertyZoneNoRefreshInterval, Buffer, SizeOf(Cardinal));
    result := SizeOf(Cardinal);
  finally
  end;
end;

function DNSPropertyZoneNoRefreshIntervalToString(
  const DNSPropertyZoneNoRefreshInterval: TDNSPropertyZoneNoRefreshInterval
  ): RawUtf8;
begin
  result := Format('%dh', [DNSPropertyZoneNoRefreshInterval]);
end;

function DNSPropertyZoneNoRefreshIntervalToStringFromBytes(
  const Buffer: TByteArray): RawUtf8;
var
  DNSPropertyZoneNoRefreshInterval: TDNSPropertyZoneNoRefreshInterval;
begin
  result := '';
  if DNSPropertyZoneNoRefreshIntervalBytesToRecord(DNSPropertyZoneNoRefreshInterval, Buffer) then
    result := DNSPropertyZoneNoRefreshIntervalToString(DNSPropertyZoneNoRefreshInterval);
end;

function DNSPropertyZoneRefreshIntervalBytesToRecord(out
  DNSPropertyZoneRefreshInterval: TDNSPropertyZoneRefreshInterval;
  const Buffer: TByteArray): Boolean;
begin
  result := False;
  try
    Move(Buffer, DNSPropertyZoneRefreshInterval, SizeOf(Cardinal));
    result := True;
  finally
  end;
end;

function DNSPropertyZoneRefreshIntervalRecordToBytes(out Buffer: TByteArray;
  const DNSPropertyZoneRefreshInterval: TDNSPropertyZoneRefreshInterval
  ): Integer;
begin
  result := 0;
  try
    Move(DNSPropertyZoneRefreshInterval, Buffer, SizeOf(Cardinal));
    result := SizeOf(Cardinal);
  finally
  end;
end;

function DNSPropertyZoneRefreshIntervalToString(
  const DNSPropertyZoneRefreshInterval: TDNSPropertyZoneRefreshInterval
  ): RawUtf8;
begin
  result := Format('%dh', [DNSPropertyZoneRefreshInterval]);
end;

function DNSPropertyZoneRefreshIntervalToStringFromBytes(
  const Buffer: TByteArray): RawUtf8;
var
  DNSPropertyZoneRefreshInterval: TDNSPropertyZoneRefreshInterval;
begin
  result := '';
  if DNSPropertyZoneRefreshIntervalBytesToRecord(DNSPropertyZoneRefreshInterval, Buffer) then
    result := DNSPropertyZoneRefreshIntervalToString(DNSPropertyZoneRefreshInterval);
end;

function DNSPropertyZoneAgingStateBytesToRecord(out
  DNSPropertyZoneAgingState: TDNSPropertyZoneAgingState;
  const Buffer: TByteArray): Boolean;
begin
  result := True;
  case Buffer[0] of
    $0: DNSPropertyZoneAgingState := False;
    $1: DNSPropertyZoneAgingState := True;
    else
      result := False;
  end;
end;

function DNSPropertyZoneAgingStateRecordToBytes(out Buffer: TByteArray;
  const DNSPropertyZoneAgingState: TDNSPropertyZoneAgingState): Integer;
begin
  result := 4;
  case DNSPropertyZoneAgingState of
    False: Buffer[0] := $0;
    True: Buffer[0] := $1;
    else
      result := 0;
  end;
end;

function DNSPropertyZoneAgingStateToString(
  const DNSPropertyZoneAgingState: TDNSPropertyZoneAgingState): RawUtf8;
begin
  result := Format('%s', [BoolToStr(DNSPropertyZoneAgingState, 'TRUE', 'FALSE')]);
end;

function DNSPropertyZoneAgingStateToStringFromBytes(const Buffer: TByteArray
  ): RawUtf8;
var
  DNSPropertyZoneAgingState: TDNSPropertyZoneAgingState;
begin
  result := '';
  if DNSPropertyZoneAgingStateBytesToRecord(DNSPropertyZoneAgingState, Buffer) then
    result := DNSPropertyZoneAgingStateToString(DNSPropertyZoneAgingState);
end;

function DNSPropertyZoneScavengingServersBytesToRecord(out
  DNSPropertyZoneScavengingServers: TDNSPropertyZoneScavengingServers;
  const Buffer: TByteArray): Boolean;
begin
  result := DNSIPArrayBytesToRecord(DNSPropertyZoneScavengingServers, Buffer);
end;

function DNSPropertyZoneScavengingServersRecordToBytes(out Buffer: TByteArray;
  const DNSPropertyZoneScavengingServers: TDNSPropertyZoneScavengingServers
  ): Integer;
begin
  result := DNSIPArrayRecordToBytes(Buffer, DNSPropertyZoneScavengingServers);
end;

function DNSPropertyZoneScavengingServersToString(
  const DNSPropertyZoneScavengingServers: TDNSPropertyZoneScavengingServers
  ): RawUtf8;
begin
  result := DNSIPArrayToString(DNSPropertyZoneScavengingServers);
end;

function DNSPropertyZoneScavengingServersToStringFromBytes(
  const Buffer: TByteArray): RawUtf8;
begin
  result := DNSIPArrayToStringFromBytes(Buffer);
end;

function DNSPropertyZoneAgingEnabledTimeBytesToRecord(out
  DNSPropertyZoneAgingEnabledTime: TDNSPropertyZoneAgingEnabledTime;
  const Buffer: TByteArray): Boolean;
begin
  result := False;
  try
    Move(Buffer, DNSPropertyZoneAgingEnabledTime, SizeOf(Cardinal));
    result := True;
  finally
  end;
end;

function DNSPropertyZoneAgingEnabledTimeRecordToBytes(out Buffer: TByteArray;
  const DNSPropertyZoneAgingEnabledTime: TDNSPropertyZoneAgingEnabledTime
  ): Integer;
begin
  result := 0;
  try
    Move(DNSPropertyZoneAgingEnabledTime, Buffer, SizeOf(Cardinal));
    result := SizeOf(Cardinal);
  finally
  end;
end;

function DNSPropertyZoneAgingEnabledTimeToString(
  const DNSPropertyZoneAgingEnabledTime: TDNSPropertyZoneAgingEnabledTime
  ): RawUtf8;
begin
  result := Format('%d', [DNSPropertyZoneAgingEnabledTime]);
end;

function DNSPropertyZoneAgingEnabledTimeToStringFromBytes(
  const Buffer: TByteArray): RawUtf8;
var
  DNSPropertyZoneAgingEnabledTime: TDNSPropertyZoneAgingEnabledTime;
begin
  result := '';
  if DNSPropertyZoneAgingEnabledTimeBytesToRecord(DNSPropertyZoneAgingEnabledTime, Buffer) then
    result := DNSPropertyZoneAgingEnabledTimeToString(DNSPropertyZoneAgingEnabledTime);
end;

function DNSPropertyZoneDeletedFromHostnameBytesToRecord(out
  DNSPropertyZoneDeletedFromHostname: TDNSPropertyZoneDeletedFromHostname;
  const Buffer: TByteArray): Boolean;
begin
  result := False;
  try
    DNSPropertyZoneDeletedFromHostname := String(PByteArray(@Buffer));
    result := True;
  finally
  end;
end;

function DNSPropertyZoneDeletedFromHostnameRecordToBytes(out
  Buffer: TByteArray;
  const DNSPropertyZoneDeletedFromHostname: TDNSPropertyZoneDeletedFromHostname
  ): Boolean;
begin
  result := False;
  try
    Buffer := PByteArray(DNSPropertyZoneDeletedFromHostname)^;
    result := True;
  finally
  end;
end;

function DNSPropertyZoneDeletedFromHostnameToString(
  const DNSPropertyZoneDeletedFromHostname: TDNSPropertyZoneDeletedFromHostname
  ): RawUtf8;
begin
  result := DNSPropertyZoneDeletedFromHostname;
end;

function DNSPropertyZoneDeletedFromHostnameToStringFromBytes(
  const Buffer: TByteArray): RawUtf8;
begin
  result := String(PByteArray(@Buffer));
end;

function DNSPropertyZoneMasterServersBytesToRecord(out
  DNSPropertyZoneMasterServers: TDNSPropertyZoneMasterServers;
  const Buffer: TByteArray): Boolean;
begin
  result := DNSIPArrayBytesToRecord(DNSPropertyZoneMasterServers, Buffer);
end;

function DNSPropertyZoneMasterServersRecordToBytes(out Buffer: TByteArray;
  const DNSPropertyZoneMasterServers: TDNSPropertyZoneMasterServers): Integer;
begin
  result := DNSIPArrayRecordToBytes(Buffer, DNSPropertyZoneMasterServers);
end;

function DNSPropertyZoneMasterServersToString(
  const DNSPropertyZoneMasterServers: TDNSPropertyZoneMasterServers): RawUtf8;
begin
  result := DNSIPArrayToString(DNSPropertyZoneMasterServers);
end;

function DNSPropertyZoneMasterServersToStringFromBytes(const Buffer: TByteArray
  ): RawUtf8;
begin
  result := DNSIPArrayToStringFromBytes(Buffer);
end;

function DNSPropertyZoneAutoNSServersBytesToRecord(out
  DNSPropertyZoneAutoNSServers: TDNSPropertyZoneAutoNSServers;
  const Buffer: TByteArray): Boolean;
begin
  result := DNSIPArrayBytesToRecord(DNSPropertyZoneAutoNSServers, Buffer);
end;

function DNSPropertyZoneAutoNSServersRecordToBytes(out Buffer: TByteArray;
  const DNSPropertyZoneAutoNSServers: TDNSPropertyZoneAutoNSServers): Integer;
begin
  result := DNSIPArrayRecordToBytes(Buffer, DNSPropertyZoneAutoNSServers);
end;

function DNSPropertyZoneAutoNSServersToString(
  const DNSPropertyZoneAutoNSServers: TDNSPropertyZoneAutoNSServers): RawUtf8;
begin
  result := DNSIPArrayToString(DNSPropertyZoneAutoNSServers);
end;

function DNSPropertyZoneAutoNSServersToStringFromBytes(const Buffer: TByteArray
  ): RawUtf8;
begin
  result := DNSIPArrayToStringFromBytes(Buffer);
end;

function DNSPropertyZoneDCPromoConvertBytesToRecord(out
  DNSPropertyZoneDCPromoConvert: TDNSPropertyZoneDCPromoConvert;
  const Buffer: TByteArray): Boolean;
begin
  result := False;
  if (Buffer[0] < Ord(Low(TDNSPropertyZoneDCPromoConvert))) or (Buffer[0] > Ord(High(TDNSPropertyZoneDCPromoConvert))) then
    Exit;
  DNSPropertyZoneDCPromoConvert := TDNSPropertyZoneDCPromoConvert(Buffer[0]);
  result := True;
end;

function DNSPropertyZoneDCPromoConvertRecordToBytes(out Buffer: TByteArray;
  const DNSPropertyZoneDCPromoConvert: TDNSPropertyZoneDCPromoConvert): Boolean;
begin
  result := False;
  Buffer[0] := Ord(DNSPropertyZoneDCPromoConvert);
  result := True;
end;

function DNSPropertyZoneDCPromoConvertToString(
  const DNSPropertyZoneDCPromoConvert: TDNSPropertyZoneDCPromoConvert): RawUtf8;
begin
  result := '';
  case DNSPropertyZoneDCPromoConvert of
    dpzdcpcConvertNone: result := rsDNSZoneDCPromoConvertNone;
    dpzdcpcConvertDomain: result := rsDNSZoneDCPromoConvertDomain;
    dpzdcpcConvertForest: result := rsDNSZoneDCPromoConvertForest;
  end;
end;

function DNSPropertyZoneDCPromoConvertToStringFromBytes(const Buffer: TByteArray
  ): RawUtf8;
var
  DNSPropertyZoneDCPromoConvert: TDNSPropertyZoneDCPromoConvert;
begin
  result := '';
  if DNSPropertyZoneDCPromoConvertBytesToRecord(DNSPropertyZoneDCPromoConvert, Buffer) then
    result := DNSPropertyZoneDCPromoConvertToString(DNSPropertyZoneDCPromoConvert);
end;

function DNSRecordBytesToRecord(out DNSRecord: TDNSRecord;
  const Buffer: TByteArray): Boolean;
var
  i: Integer;
begin
  result := False;

  DNSRecord.DataLength := (Buffer[1] shl 8) or Buffer[0];
  DNSRecord.RecType := (Buffer[3] shl 8) or Buffer[2];
  DNSRecord.Version := Buffer[4];
  DNSRecord.Rank := Buffer[5];
  DNSRecord.Flags := (Buffer[7] shl 8) or Buffer[6];
  DNSRecord.Serial := (Buffer[11] shl 24) or (Buffer[10] shl 16) or (Buffer[9] shl 8) or Buffer[8];
  DNSRecord.TtlSeconds := (Buffer[12] shl 24) or (Buffer[13] shl 16) or (Buffer[14] shl 8) or Buffer[15];
  DNSRecord.Reserved := (Buffer[19] shl 24) or (Buffer[18] shl 16) or (Buffer[17] shl 8) or Buffer[16];
  DNSRecord.Timestamp := (Buffer[23] shl 24) or (Buffer[22] shl 16) or (Buffer[21] shl 8) or Buffer[20];

  for i := 0 to Pred(DNSRecord.DataLength) do
    DNSRecord.RData[i] := Buffer[24 + i];
  result := True;
end;

function DNSCOUNTNAMEBytesToRecord(out DNSCOUNTNAME: TDNSCOUNTNAME;
  const Buffer: TByteArray): Boolean;
var
  idx, i: Integer;
begin
  result := False;
  try
    DNSCOUNTNAME.Length := Buffer[0];
    DNSCOUNTNAME.LabelCount := Buffer[1];
    SetLength(DNSCOUNTNAME.RawName, DNSCOUNTNAME.LabelCount);

    idx := 2;
    for i := 0 to Pred(DNSCOUNTNAME.LabelCount) do
    begin
      DNSCOUNTNAME.RawName[i].Length := Buffer[idx];
      DNSCOUNTNAME.RawName[i].Name := Copy(String(@Buffer[1]), idx + 1, DNSCOUNTNAME.RawName[i].Length);
      idx := idx + DNSCOUNTNAME.RawName[i].Length + 1;
    end;
    result := True;
  finally
  end;
end;

function DNSCOUNTNAMERecordToBytes(out Buffer: TByteArray;
  const DNSCOUNTNAME: TDNSCOUNTNAME): Integer;
var
  idx, i: Integer;
begin
  Buffer[0] := DNSCOUNTNAME.Length;
  Buffer[1] := DNSCOUNTNAME.LabelCount;

  idx := 2;
  for i := 0 to Pred(DNSCOUNTNAME.LabelCount) do
  begin
    Buffer[idx] := DNSCOUNTNAME.RawName[i].Length;
    Move(DNSCOUNTNAME.RawName[i].Name[1], Buffer[idx + 1], DNSCOUNTNAME.RawName[i].Length);
    idx := idx + DNSCOUNTNAME.RawName[i].Length + 1;
  end;
  Buffer[idx] := 0;
  result := idx + 1;
end;

function DNSCOUNTNAMEToString(const DNSCOUNTNAME: TDNSCOUNTNAME): RawUtf8;
var
  i: Integer;
begin
  result := '';
  for i := 0 to Pred(DNSCOUNTNAME.LabelCount) do
  begin
    if (i = 0) then
      result := DNSCOUNTNAME.RawName[i].Name
    else
      result := Format('%s.%s', [result, DNSCOUNTNAME.RawName[i].Name]);
  end;
end;

procedure DNSCOUNTNAMEBuild(var DNSCOUNTNAME: TDNSCOUNTNAME;
  const AValue: RawUtf8);
var
  AValueArray: TStringArray;
  i, Count: Integer;
begin
  AValueArray := String(AValue).Split('.');

  Count := 0;
  DNSCOUNTNAME.LabelCount := Length(AValueArray);
  SetLength(DNSCOUNTNAME.RawName, DNSCOUNTNAME.LabelCount);
  for i := 0 to Pred(DNSCOUNTNAME.LabelCount) do
  begin
    DNSCOUNTNAME.RawName[i].Length := Length(AValueArray[i]);
    DNSCOUNTNAME.RawName[i].Name := AValueArray[i];
    Count := Count + 1 + DNSCOUNTNAME.RawName[i].Length;
  end;
  DNSCOUNTNAME.Length := Count + 1;
end;

function DNSRecordRecordToBytes(out Buffer: TByteArray;
  const DNSRecord: TDNSRecord): Integer;
var
  i: Integer;
begin
  result := -1;

  Buffer[0] := DNSRecord.DataLength and $ff;
  Buffer[1] := (DNSRecord.DataLength shr 8) and $ff;

  Buffer[2] := DNSRecord.RecType and $ff;
  Buffer[3] := (DNSRecord.RecType shr 8) and $ff;

  Buffer[4] := DNSRecord.Version;

  Buffer[5] := DNSRecord.Rank;

  Buffer[6] := DNSRecord.Flags and $ff;
  Buffer[7] := (DNSRecord.Flags shr 8) and $ff;

  Buffer[8] := DNSRecord.Serial and $ff;
  Buffer[9] := (DNSRecord.Serial shr 8) and $ff;
  Buffer[10] := (DNSRecord.Serial shr 16) and $ff;
  Buffer[11] := (DNSRecord.Serial shr 24) and $ff;

  Buffer[12] := (DNSRecord.TtlSeconds shr 24) and $ff;
  Buffer[13] := (DNSRecord.TtlSeconds shr 16) and $ff;
  Buffer[14] := (DNSRecord.TtlSeconds shr 8) and $ff;
  Buffer[15] := DNSRecord.TtlSeconds and $ff;

  Buffer[16] := DNSRecord.Reserved and $ff;
  Buffer[17] := (DNSRecord.Reserved shr 8) and $ff;
  Buffer[18] := (DNSRecord.Reserved shr 16) and $ff;
  Buffer[19] := (DNSRecord.Reserved shr 24) and $ff;

  Buffer[20] := DNSRecord.Timestamp and $ff;
  Buffer[21] := (DNSRecord.Timestamp shr 8) and $ff;
  Buffer[22] := (DNSRecord.Timestamp shr 16) and $ff;
  Buffer[23] := (DNSRecord.Timestamp shr 24) and $ff;

  for i := 0 to Pred(DNSRecord.DataLength) do
    Buffer[24 + i] := DNSRecord.RData[i];
  Buffer[24 + i + 1] := 0;
  result := 24 + i + 1;
end;

function DNSRecordToString(const DNSRecord: TDNSRecord): RawUtf8;
begin
  result := DNSRecordDataToString(DNSRecord);
end;

function DNSRecordDataToString(const DNSRecord: TDNSRecord): RawUtf8;
var
  BytesData: PByteArray;
begin
  result := '';
  BytesData := PByteArray(@DNSRecord.RData[0]);

  case TDnsResourceRecord(DNSRecord.RecType) of
    drrA: result := DNSRRAToStringFromBytes(BytesData^);
    drrCNAME: result := DNSRRCNAMEToStringFromBytes(BytesData^);
    drrSOA: result := DNSRRSOAToStringFromBytes(BytesData^);
    drrMX: result := DNSRRMXToStringFromBytes(BytesData^);
    drrPTR: result := DNSRRPTRToStringFromBytes(BytesData^);
    drrSRV: result := DNSRRSRVToStringFromBytes(BytesData^);
  end;
end;

function DNSRRCNAMEBytesToRecord(out CNAME: TRRCname; const Buffer: TByteArray
  ): Boolean;
begin
  result := DNSCOUNTNAMEBytesToRecord(CNAME, Buffer);
end;

function DNSRRCNAMERecordToBytes(out Buffer: TByteArray; const CNAME: TRRCname
  ): Integer;
begin
  result := DNSCOUNTNAMERecordToBytes(Buffer, CNAME);
end;

function DNSRRCNAMEToString(const CNAME: TRRCname): String;
begin
  result := DNSCOUNTNAMEToString(CNAME);
end;

function DNSRRCNAMEToStringFromBytes(const Buffer: TByteArray): RawUtf8;
var
  cname: TDNSCOUNTNAME;
begin
  result := '';
  if DNSCOUNTNAMEBytesToRecord(cname, Buffer) then
    result := DNSCOUNTNAMEToString(cname);
end;

procedure DNSRRCNAMEBuild(var CNAME: TRRCname; const AValue: RawUtf8);
begin
  DNSCOUNTNAMEBuild(CNAME, AValue);
end;

function DNSRRABytesToRecord(out A: TRRA; const Buffer: TByteArray): Boolean;
begin
  result := False;
  try
    Move(Buffer, A, SizeOf(TRRA));
    result := True;
  finally
  end;
end;

function DNSRRARecordToBytes(out Buffer: TByteArray; const A: TRRA): Integer;
begin
  result := -1;
  try
    Move(A, Buffer, SizeOf(TRRA));
    result := SizeOf(TRRA);
  finally
  end;
end;

function DNSRRAToString(const A: TRRA): RawUtf8;
var
  v: Array[0..3] of String;
begin
  result := '';
  v[3] := IntToStr((A shr 24) and $ff);
  v[2] := IntToStr((A shr 16) and $ff);
  v[1] := IntToStr((A shr 8) and $ff);
  v[0] := IntToStr(A and $ff);
  result := String.Join('.', v);
end;

function DNSRRAToStringFromBytes(const Buffer: TByteArray): RawUtf8;
var
  A: TRRA;
begin
  result := '';
  if DNSRRABytesToRecord(A, Buffer) then
    result := DNSRRAToString(A);
end;

procedure DNSRRABuild(var A: TRRA; const AValue: RawUtf8);
var
  AValueArray: TStringArray;
begin
  AValueArray := String(AValue).Split('.');

  if (Length(AValueArray) <> 4) then
    raise ExceptionInvalidIPFormat.Create('Invalid IP4 format.');

  A := ((StrToInt(AValueArray[3]) and $ff) shl 24) or ((StrToInt(AValueArray[2]) and $ff) shl 16) or ((StrToInt(AValueArray[1]) and $ff) shl 8) or (StrToInt(AValueArray[0]) and $ff);
end;

function DNSRRSOABytesToRecord(out SOA: TRRSOA; const Buffer: TByteArray
  ): Boolean;
begin
  result := False;
  try
    MoveSwap(PByte(@SOA.Serial), @Buffer[0], SizeOf(Cardinal));
    MoveSwap(PByte(@SOA.Refresh), @Buffer[4], SizeOf(Cardinal));
    MoveSwap(PByte(@SOA.Retry), @Buffer[8], SizeOf(Cardinal));
    MoveSwap(PByte(@SOA.Expire), @Buffer[12], SizeOf(Cardinal));
    MoveSwap(PByte(@SOA.Minimum), @Buffer[16], SizeOf(Cardinal));

    DNSCOUNTNAMEBytesToRecord(SOA.MName, PByteArray(@Buffer[20])^);
    DNSCOUNTNAMEBytesToRecord(SOA.RName, PByteArray(@Buffer[22 + Buffer[20]])^);
    result := True;
  finally
  end;
end;

function DNSRRSOARecordToBytes(out Buffer: TByteArray; const SOA: TRRSOA
  ): Integer;
begin
  result := 0;
  try
    MoveSwap(@Buffer[0], PByte(@SOA.Serial), SizeOf(Cardinal));
    MoveSwap(@Buffer[4], PByte(@SOA.Refresh), SizeOf(Cardinal));
    MoveSwap(@Buffer[8], PByte(@SOA.Retry), SizeOf(Cardinal));
    MoveSwap(@Buffer[12], PByte(@SOA.Expire), SizeOf(Cardinal));
    MoveSwap(@Buffer[16], PByte(@SOA.Minimum), SizeOf(Cardinal));

    result := SizeOf(Cardinal) * 5;
    result := result + DNSCOUNTNAMERecordToBytes(PByteArray(@Buffer[20])^, SOA.MName);
    result := result + DNSCOUNTNAMERecordToBytes(PByteArray(@Buffer[22 + Buffer[20]])^, SOA.RName);
  finally
  end;
end;

function DNSRRSOAToString(const SOA: TRRSOA): RawUtf8;
begin
  result := Format('[%d],%s,%s', [SOA.Serial, DNSCOUNTNAMEToString(SOA.MName), DNSCOUNTNAMEToString(SOA.RName)]);
end;

function DNSRRSOAToStringFromBytes(const Buffer: TByteArray): RawUtf8;
var
  soa: TRRSOA;
begin
  result := '';

  if DNSRRSOABytesToRecord(soa, Buffer) then
    result := DNSRRSOAToString(soa);
end;

procedure DNSRRSOABuild(var SOA: TRRSOA; const Serial: Cardinal;
  const Refresh: Cardinal; const Retry: Cardinal; const Expire: Cardinal;
  const Minimum: Cardinal; const MName: RawUtf8; const RName: RawUtf8);
begin
  SOA.Serial := Serial;
  SOA.Refresh := Refresh;
  SOA.Retry := Retry;
  SOA.Expire := Expire;
  SOA.Minimum := Minimum;
  DNSCOUNTNAMEBuild(SOA.MName, MName);
  DNSCOUNTNAMEBuild(SOA.RName, RName);
end;

function DNSRRPTRBytesToRecord(out APTR: TRRPTR; const Buffer: TByteArray
  ): Boolean;
begin
  result := DNSCOUNTNAMEBytesToRecord(APTR, Buffer);
end;

function DNSRRPTRRecordToBytes(out Buffer: TByteArray; const APTR: TRRPTR
  ): Integer;
begin
  result := DNSCOUNTNAMERecordToBytes(Buffer, APTR);
end;

function DNSRRPTRToString(const APTR: TRRPTR): String;
begin
  result := DNSCOUNTNAMEToString(APTR);
end;

function DNSRRPTRToStringFromBytes(const Buffer: TByteArray): RawUtf8;
var
  APTR: TRRPTR;
begin
  result := '';
  if DNSRRPTRBytesToRecord(APTR, Buffer) then
    result := DNSRRPTRToString(APTR);
end;

procedure DNSRRPTRBuild(var APTR: TRRPTR; const AValue: RawUtf8);
begin
  DNSCOUNTNAMEBuild(APTR, AValue);
end;

function DNSRRMXBytesToRecord(out MX: TRRMX; const Buffer: TByteArray): Boolean;
begin
  result := False;
  try
    MX.Preference := (Buffer[0] shl 8) or Buffer[1];

    result := DNSCOUNTNAMEBytesToRecord(MX.Exchange, PByteArray(@Buffer[2])^);
  finally
  end;
end;

function DNSRRMXRecordToBytes(out Buffer: TByteArray; const MX: TRRMX): Integer;
begin
  Buffer[1] := MX.Preference and $ff;
  Buffer[0] := (MX.Preference shr 8) and $ff;

  result := DNSCOUNTNAMERecordToBytes(PByteArray(@Buffer[2])^, MX.Exchange) + 2;
end;

function DNSRRMXToString(const MX: TRRMX): RawUtf8;
begin
  result := Format('[%d] %s', [mx.Preference, DNSCOUNTNAMEToString(mx.Exchange)]);
end;

function DNSRRMXToStringFromBytes(const Buffer: TByteArray): RawUtf8;
var
  MX: TRRMX;
begin
  result := '';
  if DNSRRMXBytesToRecord(MX, Buffer) then
    result := DNSRRMXToString(MX);
end;

procedure DNSRRMXBuild(var MX: TRRMX; const Exchange: RawUtf8;
  const Preference: Cardinal);
begin
  MX.Preference := Preference;
  DNSCOUNTNAMEBuild(MX.Exchange, Exchange);
end;

function DNSRRSRVBytesToRecord(out SRV: TRRSRV; const Buffer: TByteArray
  ): Boolean;
begin
  result := False;
  try
    SRV.wPriority := (Buffer[0] shl 8) and Buffer[1];
    SRV.wWeight := (Buffer[2] shl 8) and Buffer[3];
    SRV.wPort := (Buffer[4] shl 8) and Buffer[5];
    result := DNSCOUNTNAMEBytesToRecord(SRV.nameTarget, PByteArray(@Buffer[6])^);
  finally
  end;
end;

function DNSRRSRVRecordToBytes(out Buffer: TByteArray; const SRV: TRRSRV
  ): Integer;
begin
  Buffer[0] := (SRV.wPriority shr 8) and $ff;
  Buffer[1] := SRV.wPriority and $ff;

  Buffer[2] := (SRV.wWeight shr 8) and $ff;
  Buffer[3] := SRV.wWeight and $ff;

  Buffer[4] := (SRV.wPort shr 8) and $ff;
  Buffer[5] := SRV.wPort and $ff;

  result := DNSCOUNTNAMERecordToBytes(PByteArray(@Buffer[6])^, SRV.nameTarget) + 6;
end;

function DNSRRSRVToString(const SRV: TRRSRV): RawUtf8;
begin
  result := Format('[%d][%d][%d] %s', [SRV.wPriority, SRV.wWeight, SRV.wPort, DNSCOUNTNAMEToString(SRV.nameTarget)]);
end;

function DNSRRSRVToStringFromBytes(const Buffer: TByteArray): RawUtf8;
var
  SRV: TRRSRV;
begin
  result := '';
  if DNSRRSRVBytesToRecord(SRV, Buffer) then
    result := DNSRRSRVToString(SRV);
end;

procedure DNSRRSRVBuild(var SRV: TRRSRV; const Priority: Word;
  const Weight: Word; const Port: Word; const NameTarget: RawUtf8);
begin
  SRV.wPriority := Priority;
  SRV.wWeight := Weight;
  SRV.wPort := Port;
  DNSCOUNTNAMEBuild(SRV.nameTarget, NameTarget);
end;

function DNSRRNSBytesToRecord(out NS: TRRNS; const Buffer: TByteArray): Boolean;
begin
  result := DNSCOUNTNAMEBytesToRecord(NS, Buffer);
end;

function DNSRRNSRecordToBytes(out Buffer: TByteArray; const NS: TRRNS): Integer;
begin
  result := DNSCOUNTNAMERecordToBytes(Buffer, NS);
end;

function DNSRRNSToString(const NS: TRRNS): RawUtf8;
begin
  result := DNSCOUNTNAMEToString(NS);
end;

function DNSRRNSToStringFromBytes(const Buffer: TByteArray): RawUtf8;
var
  NS: TRRNS;
begin
  result := '';
  if DNSRRNSBytesToRecord(NS, Buffer) then
    result := DNSRRNSToString(NS);
end;

procedure DNSRRNSBuild(var NS: TRRNS; const AValue: RawUtf8);
begin
  DNSCOUNTNAMEBuild(NS, AValue);
end;

function DnsResourceRecordToStr(AValue: TDnsResourceRecord): RawUtf8;
begin
  result := '';
  case AValue of
    drrA: result := 'Host (A)';
    drrNS: result := 'Name Server (NS)';
    drrMD: result := 'MD';
    drrMF: result := 'MF';
    drrCNAME: result := 'CNAME';
    drrSOA: result := 'Start of Authority (SOA)';
    drrMB: result := 'MB';
    drrSRV: result := 'Service Location (SRV)';
    drrMX: result := 'Mail Exchanger (MX)';
  end;
end;

function DnsPropertyIdToString(AValue: TDnsPropertyId): RawUtf8;
begin
  result := '';
  case AValue of
    dpidPropertyZoneType: result := rsDNSPropertyZoneType;
    dpidPropertyZoneAllowUpdate: result := rsDNSPropertyZoneAllowUpdate;
    dpidPropertyZoneSecureTime: result := rsDNSPropertyZoneSecureTime;
    dpidPropertyZoneNoRefreshInterval: result := rsDNSPropertyZoneNoRefreshInterval;
    dpidPropertyZoneRefreshInterval: result := rsDNSPropertyZoneRefreshInterval;
    dpidPropertyZoneAgingState: result := rsDNSPropertyZoneAgingState;
    dpidPropertyZoneScavengingServers: result := rsDNSPropertyZoneScavengingServers;
    dpidPropertyZoneAgingEnabledTime: result := rsDNSPropertyZoneAgingEnabledTime;
    dpidPropertyZoneDeletedFromHostname: result := rsDNSPropertyZoneDeletedFromHostname;
    dpidPropertyZoneMasterServers: result := rsDNSPropertyZoneMasterServers;
    dpidPropertyZoneAutoNSServers: result := rsDNSPropertyZoneAutoNSServers;
    dpidPropertyZoneDCPromoConvert: result := rsDNSPropertyZoneDCPromoConvert;
    dpidPropertyZoneScavengingServersDA: result := rsDNSPropertyZoneScavengingServersDA;
    dpidPropertyZoneMasterServersDA: result := rsDNSPropertyZoneMasterServersDA;
    dpidPropertyZoneAutoNSServersDA: result := rsDNSPropertyZoneAutoNSServersDA;
    dpidPropertyZoneNodeDBFlags: result := rsDNSPropertyZoneNodeDBFlags;
  end;
end;

operator=(Destination: TDNSCOUNTNAME; Source: TDNSCOUNTNAME): Boolean;
var
  i: Integer;
begin
  result := (Destination.Length = Source.Length) and (Destination.LabelCount = Source.LabelCount);
  if result then
    for i := 0 to Pred(Destination.LabelCount) do
    begin
      result := result and (Destination.RawName[i].Length = Source.RawName[i].Length) and (Destination.RawName[i].Name = Source.RawName[i].Name);
      if not result then
        Exit;
    end;
end;

end.

