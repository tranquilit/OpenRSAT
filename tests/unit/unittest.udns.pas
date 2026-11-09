unit unittest.udns;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.test,
  mormot.core.base,
  mormot.net.dns,
  udns;

type

  { TUnitTestDNS }

  TUnitTestDNS = class(TSynTestCase)
  published
    procedure ResourceRecordToStr_AAAA;
    procedure RecordDataToString_A;
    procedure RecordDataToString_AAAA;
    procedure RecordDataToString_AAAAInvalidLength;
  end;

implementation

// Build a raw dnsRecord attribute value (DNS_RPC_RECORD) with the given data.
// https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-dnsp/6912b338-5472-4f59-b912-0edb536b6ed8
function MakeRawDNSRecord(RecType: TDnsResourceRecord; const Data: array of Byte): RawByteString;
var
  i: Integer;
begin
  SetLength(result, 24 + Length(Data));
  FillChar(result[1], Length(result), 0);
  result[1] := AnsiChar(Length(Data) and $ff);
  result[2] := AnsiChar((Length(Data) shr 8) and $ff);
  result[3] := AnsiChar(Ord(RecType) and $ff);
  result[4] := AnsiChar((Ord(RecType) shr 8) and $ff);
  result[5] := #5; // Version
  result[6] := #$f0; // Rank: RANK_ZONE
  for i := 0 to High(Data) do
    result[25 + i] := AnsiChar(Data[i]);
end;

function RawDNSRecordDataToString(const Raw: RawByteString): RawUtf8;
var
  DNSRecord: TDNSRecord;
begin
  result := '';
  if DNSRecordBytesToRecord(DNSRecord, PByteArray(Raw)^) then
    result := DNSRecordDataToString(DNSRecord);
end;

{ TUnitTestDNS }

procedure TUnitTestDNS.ResourceRecordToStr_AAAA;
begin
  CheckEqual(DnsResourceRecordToStr(drrAAAA), 'IPv6 Host (AAAA)');
end;

procedure TUnitTestDNS.RecordDataToString_A;
begin
  CheckEqual(RawDNSRecordDataToString(MakeRawDNSRecord(drrA, [192, 0, 2, 250])), '192.0.2.250');
end;

procedure TUnitTestDNS.RecordDataToString_AAAA;
begin
  // Loopback, as in the issue #137 reproduction steps.
  CheckEqual(RawDNSRecordDataToString(MakeRawDNSRecord(drrAAAA,
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1])), '::1');
  // Unspecified address.
  CheckEqual(RawDNSRecordDataToString(MakeRawDNSRecord(drrAAAA,
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0])), '::');
  // Documentation prefix, compressed zeros in the middle.
  CheckEqual(RawDNSRecordDataToString(MakeRawDNSRecord(drrAAAA,
    [$20, $01, $0d, $b8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1])), '2001:db8::1');
  // A single zero group is not compressed (RFC 5952).
  CheckEqual(RawDNSRecordDataToString(MakeRawDNSRecord(drrAAAA,
    [$20, $01, $0d, $b8, 0, 0, 0, 1, 0, 2, 0, 3, 0, 4, 0, 5])), '2001:db8:0:1:2:3:4:5');
  // Link-local, leading zeros are trimmed.
  CheckEqual(RawDNSRecordDataToString(MakeRawDNSRecord(drrAAAA,
    [$fe, $80, 0, 0, 0, 0, 0, 0, $01, $ff, $fe, $23, $45, $67, $89, $0a])), 'fe80::1ff:fe23:4567:890a');
end;

procedure TUnitTestDNS.RecordDataToString_AAAAInvalidLength;
begin
  CheckEqual(RawDNSRecordDataToString(MakeRawDNSRecord(drrAAAA, [0, 0, 0, 1])), '');
end;

end.
