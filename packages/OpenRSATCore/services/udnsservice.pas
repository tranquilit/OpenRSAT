unit udnsservice;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.base,
  mormot.core.text,
  mormot.net.ldap, // should be removed
  uldapclient;

type

  /// stores required information for a dnsZone
  TDNSZone = record
    /// full distinguished name of the LDAP entry
    DistinguishedName: RawUtf8;
    /// dc attribute for this entry
    DC: RawUtf8;
    /// dnsProperty attribute for this entry
    DNSProperties: TRawByteStringDynArray;
    /// name attribute for this entry
    Name: RawUtf8;
  end;

  /// dynamic array of a dnsZone entry
  TDNSZoneDynArray = Array of TDNSZone;

  /// stores required information for a dnsNode
  TDNSNode = record
    /// full distinguished name of the LDAP entry
    DistinguishedName: RawUtf8;
    /// name attribute for this entry
    Name: RawUtf8;
    /// dnsRecord attribute for this entry
    DnsRecords: TRawByteStringDynArray;
    /// whenChanged attribute for this entry
    WhenChanged: RawUtf8;
  end;

  /// dynamic array of a dnsNode entry
  TDNSNodeDynArray = Array of TDNSNode;

  /// event used on LDAP search page for a dnsZone search
  TOnSearchPageZonesEvent = procedure(const DNSZones: TDNSZoneDynArray) of object;
  /// event used on LDAP search page for a dnsNode search
  TOnSearchPageNodesEvent = procedure(const DNSNodes: TDNSNodeDynArray) of object;

  { TDNSService }

  /// provide a service to work on AD DNS
  // isolates the DNS work from the UI and the LDAP protocol
  // TODO: Replace the fLdapConnection by the ILdapConnection interface
  // TODO: Also replace all references from mormot2 ldap so it can be used
  // without any network references
  TDNSService = class
  private
    fLdapConnection: TLdapClient;
    /// store the event used on zone search page
    fOnSearchPageZones: TOnSearchPageZonesEvent;
    /// store the event used on node search page
    fOnSearchPageNodes: TOnSearchPageNodesEvent;

    /// convert a TLdapResultList from mormot2 into a TDNSZoneDynArray
    function SearchResultListToDNSZones(const SearchResults: TLdapResultList
      ): TDNSZoneDynArray;
    /// convert a TLdapResult from mormot2 into a TDNSZone
    function SearchResultToDNSZone(const SearchResult: TLdapResult): TDNSZone;
    /// convert a TLdapResultList from mormot2 into a TDNSNodeDynArray
    function SearchResultListToDNSNodes(const SearchResults: TLdapResultList
      ): TDNSNodeDynArray;
    /// convert a TLdapResult from mormot2 into a TDNSNode
    function SearchResultToDNSNode(const SearchResult: TLdapResult): TDNSNode;
    /// generic function to search zones
    // - zones can be found on both domain and forest partitions, and can be forward or reverse zone
    function SearchZones(const DistinguishedName, Filter: RawUtf8): TDNSZoneDynArray;
  public
    constructor Create(const LdapConnection: TLdapClient);
    destructor Destroy; override;

    /// search forward zones
    // - forward zone can be found on both domain and forest partitions.
    function SearchForwardZones: TDNSZoneDynArray;
    /// search reverse zones
    // - reverse zone can be found on both domain and forest partitions.
    function SearchReverseZones: TDNSZoneDynArray;
    /// search nodes based on a dnsZone
    function SearchNodes(const DNSZone: TDNSZone): TDNSNodeDynArray; overload;
    /// search nodes based on a distinguishedName
    function SearchNodes(const DistinguishedName: RawUtf8): TDNSNodeDynArray; overload;

    /// callback for the search zones function on ldap search page
    property OnSearchPageZones: TOnSearchPageZonesEvent read fOnSearchPageZones write fOnSearchPageZones;
    /// callback for the search nodes function on ldap search page
    property OnSearchPageNodes: TOnSearchPageNodesEvent read fOnSearchPageNodes write fOnSearchPageNodes;
  end;

implementation

const
  /// store the path of domain dns zones
  // - must be extanded with the domain distinguished name
  DOMAIN_DNS_ZONES: RawUtf8 = 'CN=MicrosoftDNS,DC=DomainDnsZones';
  /// store the path of forest dns zones
  // - must be extanded with the forest distinguished name
  FOREST_DNS_ZONES: RawUtf8 = 'CN=MicrosoftDNS,DC=ForestDnsZones';

  /// store the LDAP filter to retrieve only reverse zones
  REVERSE_ZONES_FILTER: RawUtf8 = '(&(objectClass=dnsZone)(|(dc=*.in-addr.arpa)(dc=*.ip6.arpa)))';
  /// store the LDAP filter to retrieve only forward zones
  FORWARD_ZONES_FILTER: RawUtf8 = '(&(objectClass=dnsZone)(!(|(dc=*.in-addr.arpa)(dc=*.ip6.arpa))))';

{ TDNSService }

function TDNSService.SearchResultListToDNSZones(
  const SearchResults: TLdapResultList): TDNSZoneDynArray;
var
  i: Integer;
  SearchResult: TLdapResult;
begin
  result := nil;
  SetLength(result, SearchResults.Count);
  for i := 0 to SearchResults.Count - 1 do
  begin
    SearchResult := SearchResults.Items[i];
    if not Assigned(SearchResult) then
      Continue;

    result[i] := SearchResultToDNSZone(SearchResult);
  end;
end;

function TDNSService.SearchResultToDNSZone(const SearchResult: TLdapResult
  ): TDNSZone;
var
  DNSProperty: TLdapAttribute;
  i: Integer;
begin
  result := Default(TDNSZone);
  if not Assigned(SearchResult) then
    Exit;

  result.DistinguishedName := SearchResult.ObjectName;
  result.Name := SearchResult.Find('name').GetReadable();
  result.DC := SearchResult.Find('dc').GetReadable();
  DNSProperty := SearchResult.Find('dNSProperty');
  SetLength(result.DNSProperties, DNSProperty.Count);
  for i := 0 to DNSProperty.Count - 1 do
    result.DNSProperties[i] := DNSProperty.GetRaw(i);
end;

function TDNSService.SearchResultListToDNSNodes(const SearchResults: TLdapResultList
  ): TDNSNodeDynArray;
var
  i: Integer;
  SearchResult: TLdapResult;
begin
  result := nil;
  SetLength(result, SearchResults.Count);
  for i := 0 to SearchResults.Count - 1 do
  begin
    SearchResult := SearchResults.Items[i];
    if not Assigned(SearchResult) then
      Continue;

    result[i] := SearchResultToDNSNode(SearchResult);
  end;
end;

function TDNSService.SearchResultToDNSNode(const SearchResult: TLdapResult
  ): TDNSNode;
var
  DNSRecord: TLdapAttribute;
  i: Integer;
begin
  result := Default(TDNSNode);
  if not Assigned(SearchResult) then
    Exit;

  result.DistinguishedName := SearchResult.ObjectName;
  result.Name := SearchResult.Find('name').GetReadable();
  result.WhenChanged := SearchResult.Find('whenChanged').GetReadable();
  DNSRecord := SearchResult.Find('dnsRecord');
  SetLength(result.DnsRecords, DNSRecord.Count);
  for i := 0 to DNSRecord.Count - 1 do
    result.DnsRecords[i] := DNSRecord.GetRaw(i);
end;

function TDNSService.SearchZones(const DistinguishedName, Filter: RawUtf8
  ): TDNSZoneDynArray;
var
  DNSZones: TDNSZoneDynArray;
begin
  result := nil;

  fLdapConnection.SearchRangeBegin;
  fLdapConnection.SearchBegin();
  try
    fLdapConnection.SearchScope := lssSingleLevel;
    repeat
      if not fLdapConnection.Search(DistinguishedName, False, Filter, ['dc', 'name', 'dNSProperty']) then
        Exit;

      DNSZones := SearchResultListToDNSZones(fLdapConnection.SearchResult);
      if Assigned(OnSearchPageZones) then
        OnSearchPageZones(DNSZones);
      result := Concat(result, DNSZones);
    until (fLdapConnection.SearchCookie = '');
  finally
    fLdapConnection.SearchEnd;
    fLdapConnection.SearchRangeEnd;
  end;
end;

constructor TDNSService.Create(const LdapConnection: TLdapClient);
begin
  fLdapConnection := LdapConnection;
end;

destructor TDNSService.Destroy;
begin
  fLdapConnection := nil;

  inherited Destroy;
end;

function TDNSService.SearchForwardZones: TDNSZoneDynArray;
begin
  result := Concat(
    SearchZones(FormatUtf8('%,%', [DOMAIN_DNS_ZONES, fLdapConnection.DefaultDN]), FORWARD_ZONES_FILTER),
    SearchZones(FormatUtf8('%,%', [FOREST_DNS_ZONES, fLdapConnection.RootDN]), FORWARD_ZONES_FILTER)
  );
end;

function TDNSService.SearchReverseZones: TDNSZoneDynArray;
begin
  result := Concat(
    SearchZones(FormatUtf8('%,%', [DOMAIN_DNS_ZONES, fLdapConnection.DefaultDN]), REVERSE_ZONES_FILTER),
    SearchZones(FormatUtf8('%,%', [FOREST_DNS_ZONES, fLdapConnection.RootDN]), REVERSE_ZONES_FILTER)
  );
end;

function TDNSService.SearchNodes(const DNSZone: TDNSZone): TDNSNodeDynArray;
begin
  result := SearchNodes(DNSZone.DistinguishedName);
end;

function TDNSService.SearchNodes(const DistinguishedName: RawUtf8
  ): TDNSNodeDynArray;
var
  DNSNodes: TDNSNodeDynArray;
begin
  result := nil;

  fLdapConnection.SearchRangeBegin;
  fLdapConnection.SearchBegin();
  try
    fLdapConnection.SearchScope := lssSingleLevel;
    repeat
      if not fLdapConnection.Search(DistinguishedName, False, '', ['name', 'dnsRecord', 'whenChanged']) then
        Exit;

      DNSNodes := SearchResultListToDNSNodes(fLdapConnection.SearchResult);
      if Assigned(OnSearchPageNodes) then
        OnSearchPageNodes(DNSNodes);
      result := Concat(result, DNSNodes);
    until (fLdapConnection.SearchCookie = '');
  finally
    fLdapConnection.SearchEnd;
    fLdapConnection.SearchRangeEnd;
  end;
end;

end.

