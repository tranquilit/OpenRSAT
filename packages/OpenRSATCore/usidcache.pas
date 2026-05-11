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

  TSidCache = Class
  private
    fSIDS: TRawUtf8DynArray;
    fNames: TRawUtf8DynArray;

    function BuildFilter: RawUtf8;
    function GetCount: Integer;
    procedure ResolveKnownSIDS;
    procedure UpdateSIDS(ALdapResult: TLdapResultList);
    function IndexOfSID(ASID: RawUtf8): Integer;
    function IndexOfName(AName: RawUtf8): Integer;
  public
    procedure ResolveSIDS(ALdapClient: TLdapClient);
    procedure AddSID(ASID: RawUtf8);
    procedure SetSIDName(ASID, AName: RawUtf8);
    function SIDToName(ASID: RawUtf8): RawUtf8;
    function NameToSID(AName: RawUtf8): RawUtf8;
    function ExistsSID(ASID: RawUtf8): Boolean;

    procedure Clear;
    property Count: Integer read GetCount;
  end;


implementation

{ TSidCache }

function TSidCache.BuildFilter: RawUtf8;
var
  i: Integer;
begin
  result := '';

  for i := 0 to High(fSIDS) do
    if (SidToKnown(fSIDS[i]) = wksNull) then
      result := FormatUtf8('%(objectSid=%)', [result, LdapEscape(fSIDS[i])]);
  if result <> '' then
    result := FormatUtf8('(|%)', [result]);
end;

function TSidCache.GetCount: Integer;
begin
  result := Length(fSIDS);
end;

procedure TSidCache.ResolveKnownSIDS;
var
  i: Integer;
begin
  for i := 0 to High(fSIDS) do
    if SidToKnown(fSIDS[i]) <> wksNull then
      fNames[i] := WELL_KNOWN_SID_NAMES[SidToKnown(fSIDS[i])];
end;

function TSidCache.IndexOfSID(ASID: RawUtf8): Integer;
var
  i: Integer;
  lSID: RawUtf8;
begin
  result := -1;
  if ASid = '' then
    Exit;
  lSID := UpperCase(ASID);
  for i := 0 to High(fSIDS) do
    if fSIDS[i] = lSID then
    begin
      result := i;
      Exit;
    end;
end;

function TSidCache.IndexOfName(AName: RawUtf8): Integer;
var
  i: Integer;
begin
  result := -1;
  if AName = '' then
    Exit;

  for i := 0 to High(fNames) do
    if fNames[i] = AName then
    begin
      result := i;
      Exit;
    end;
end;

procedure TSidCache.ResolveSIDS(ALdapClient: TLdapClient);
var
  Filter: RawUtf8;
begin
  // Fill Well known sid
  ResolveKnownSIDS;

  // Ignore Well known sid for ldap query

  if not Assigned(ALdapClient) then
    Exit;

  Filter := BuildFilter;
  if Filter = '' then
    Exit;

  ALdapClient.SearchBegin();
  try
    ALdapClient.SearchScope := lssWholeSubtree;
    repeat
      if not ALdapClient.Search(ALdapClient.DefaultDN, False, Filter, ['name', 'objectSid']) then
        Exit;
      UpdateSIDS(ALdapClient.SearchResult);
    until ALdapClient.SearchCookie = '';
  finally
    ALdapClient.SearchEnd;
  end;
end;

procedure TSidCache.UpdateSIDS(ALdapResult: TLdapResultList);
var
  SearchResult: TLdapResult;
  SID, Name: RawUtf8;
begin
  for SearchResult in ALdapResult.Items do
  begin
    if not Assigned(SearchResult) then
      continue;
    SID := SearchResult.Find('objectSid').GetReadable();
    Name := SearchResult.Find('name').GetReadable();
    SetSIDName(SID, Name);
  end;
end;

procedure TSidCache.AddSID(ASID: RawUtf8);
var
  c: SizeInt;
begin
  if (ASID = '') then
    Exit;
  c := IndexOfSID(ASID);
  if c >= 0 then
    Exit;

  c := Length(fSIDS);

  SetLength(fSIDS, c + 1);
  SetLength(fNames, c + 1);

  fSIDS[c] := UpperCase(ASID);
  fNames[c] := '';
end;

procedure TSidCache.SetSIDName(ASID, AName: RawUtf8);
var
  c: Integer;
begin
  c := IndexOfSID(ASID);
  if c < 0 then
    Exit;

  fNames[c] := AName;
end;

function TSidCache.SIDToName(ASID: RawUtf8): RawUtf8;
var
  c: Integer;
begin
  result := '';

  try
    c := IndexOfSID(ASID);
    if c < 0 then
      Exit;

    result := fNames[c];
  finally
    if result = '' then
      result := ASID;
  end;
end;

function TSidCache.NameToSID(AName: RawUtf8): RawUtf8;
var
  c: Integer;
begin
  result := '';
  if AName = '' then
    Exit;

  c := IndexOfName(AName);
  if c < 0 then
  begin
    c := IndexOfSID(AName);
    if c < 0 then
      Exit;
  end;

  result := fSIDS[c];
end;

function TSidCache.ExistsSID(ASID: RawUtf8): Boolean;
begin
  result := IndexOfSID(ASID) >= 0;
end;

procedure TSidCache.Clear;
begin
  fSIDS := nil;
  fNames := nil;
end;

end.

