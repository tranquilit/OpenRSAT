unit uguidcache;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.base,
  mormot.core.text,
  mormot.net.ldap;

type

  { TGUIDCache }

  TGUIDCache = class
  private
    fGUIDS: TRawUtf8DynArray;
    fNames: TRawUtf8DynArray;
    fTypes: TRawUtf8DynArray;

    function GetCount: Integer;
    procedure UpdateFromClassSchema(ALdapResult: TLdapResultList);
    procedure UpdateFromAttributeSchema(ALdapResult: TLdapResultList);
    procedure UpdateFromExtendedRights(ALdapResult: TLdapResultList);
    function IndexOfGUID(AGUID, AType: RawUtf8): Integer;
    function IndexOfName(AName: RawUtf8): Integer;
  public
    procedure ResolveGUIDS(ALdapClient: TLdapClient);
    function AddGUID(AGUID, AType: RawUtf8): Integer;
    procedure SetGUIDName(AGUID, AName, AType: RawUtf8);
    function GUIDToName(AGUID, AType: RawUtf8): RawUtf8;
    function NameToGUID(AName: RawUtf8): RawUtf8;

    procedure Clear;
    function Dump: RawUtf8;
    property Count: Integer read GetCount;
  end;

implementation

{ TGUIDCache }

procedure TGUIDCache.UpdateFromClassSchema(ALdapResult: TLdapResultList);
var
  SearchResult: TLdapResult;
  Name, GUID: RawUtf8;
begin
  for SearchResult in ALdapResult.Items do
  begin
    if not Assigned(SearchResult) then
      continue;

    Name := SearchResult.Find('cn').GetReadable();
    GUID := ToUtf8(PGuid(SearchResult.Find('schemaIDGUID').GetRaw())^);
    SetGUIDName(GUID, Name, 'class');
  end;
end;

function TGUIDCache.GetCount: Integer;
begin
  result := Length(fGUIDS);
end;

procedure TGUIDCache.UpdateFromAttributeSchema(ALdapResult: TLdapResultList);
var
  SearchResult: TLdapResult;
  Name, GUID: RawUtf8;
begin
  for SearchResult in ALdapResult.Items do
  begin
    if not Assigned(SearchResult) then
      continue;

    Name := SearchResult.Find('cn').GetReadable();
    GUID := ToUtf8(PGuid(SearchResult.Find('schemaIDGUID').GetRaw())^);
    SetGUIDName(GUID, Name, 'attribute');
  end;
end;

procedure TGUIDCache.UpdateFromExtendedRights(ALdapResult: TLdapResultList);
var
  SearchResult: TLdapResult;
  Name, GUID, T: RawUtf8;
  ValidAccesses: Longint;
begin
  for SearchResult in ALdapResult.Items do
  begin
    if not Assigned(SearchResult) then
      continue;

    Name := SearchResult.Find('cn').GetReadable();
    GUID := SearchResult.Find('rightsGuid').GetReadable();
    ValidAccesses := 0;
    TryStrToInt(SearchResult.Find('validAccesses').GetReadable(), ValidAccesses);

    T := 'controlAccessRight';
    if ((ValidAccesses and 256) <> 0) then
      T := 'extendedRight'
    else if ((ValidAccesses and 8) <> 0) then
      T := 'validateRight'
    else if ((ValidAccesses and 16) <> 0) then
      T := 'propertySet';

    SetGUIDName(GUID, Name, T);
  end;
end;

function TGUIDCache.IndexOfGUID(AGUID, AType: RawUtf8): Integer;
var
  uGUID: RawUtf8;
  i: Integer;
begin
  result := -1;

  uGUID := UpperCase(AGUID);
  for i := 0 to High(fGUIDS) do
    if (fGUIDS[i] = uGUID) and ((AType = '') or (fTypes[i] = AType)) then
    begin
      result := i;
      Exit;
    end;
end;

function TGUIDCache.IndexOfName(AName: RawUtf8): Integer;
var
  i: Integer;
begin
  result := -1;

  for i := 0 to High(fNames) do
    if (fNames[i] = AName) then
    begin
      result := i;
      Exit;
    end;
end;

procedure TGUIDCache.ResolveGUIDS(ALdapClient: TLdapClient);
var
  ExtendedRightsDN: RawUtf8;
begin
  if not Assigned(ALdapClient) then
    Exit;

  ALdapClient.SearchBegin();
  try
    ALdapClient.SearchScope := lssSingleLevel;
    repeat
      if not ALdapClient.Search(ALdapClient.SchemaDN, False, '(objectClass=classSchema)', ['cn', 'lDAPDisplayName', 'governsID', 'schemaIDGUID']) then
        Exit;
      UpdateFromClassSchema(ALdapClient.SearchResult);
    until ALdapClient.SearchCookie = '';
  finally
    ALdapClient.SearchEnd;
  end;

  ALdapClient.SearchBegin();
  try
    ALdapClient.SearchScope := lssSingleLevel;
    repeat
      if not ALdapClient.Search(ALdapClient.SchemaDN, False, '(objectClass=attributeSchema)', ['cn', 'lDAPDisplayName', 'attributeID', 'schemaIDGUID']) then
        Exit;
      UpdateFromAttributeSchema(ALdapClient.SearchResult);
    until ALdapClient.SearchCookie = '';
  finally
    ALdapClient.SearchEnd;
  end;

  ExtendedRightsDN := FormatUtf8('CN=Extended-Rights,%', [ALdapClient.ConfigDN]);
  ALdapClient.SearchBegin();
  try
    ALdapClient.SearchScope := lssSingleLevel;
    repeat
      if not ALdapClient.Search(ExtendedRightsDN, False, '(objectClass=controlAccessRight)', ['cn', 'displayName', 'rightsGuid', 'validAccesses', 'appliesTo']) then
        Exit;
      UpdateFromExtendedRights(ALdapClient.SearchResult);
    until ALdapClient.SearchCookie = '';
  finally
    ALdapClient.SearchEnd;
  end;
end;

function TGUIDCache.AddGUID(AGUID, AType: RawUtf8): Integer;
begin
  if AGUID = '' then
    Exit;

  result := IndexOfGUID(AGUID, AType);
  if result >= 0 then
    Exit;

  result := Length(fGUIDS);
  SetLength(fGUIDS, result + 1);
  SetLength(fNames, result + 1);
  SetLength(fTypes, result + 1);

  fGUIDS[result] := UpperCase(AGUID);
  fNames[result] := '';
  fTypes[result] := AType;
end;

procedure TGUIDCache.SetGUIDName(AGUID, AName, AType: RawUtf8);
var
  c: Integer;
begin
  c := AddGUID(AGUID, AType);

  if c < 0 then
    Exit;

  fNames[c] := AName;
end;

function TGUIDCache.GUIDToName(AGUID, AType: RawUtf8): RawUtf8;
var
  c: Integer;
begin
  result := '';

  if (AGUID = '') then
    Exit;

  if (AGUID = ToUtf8(GUID_NULL)) then
    Exit;

  c := IndexOfGUID(AGUID, AType);
  if c < 0 then
  begin
    result := AGUID;
    Exit;
  end;
  result := fNames[c];
  if result = '' then
    result := AGUID;
end;

function TGUIDCache.NameToGUID(AName: RawUtf8): RawUtf8;
var
  c: Integer;
begin
  result := '';

  if AName = '' then
    Exit;

  c := IndexOfName(AName);
  if c < 0 then
    Exit;
  result := fGUIDS[c];
end;

procedure TGUIDCache.Clear;
begin
  fGUIDS := nil;
  fNames := nil;
  fTypes := nil;
end;

function TGUIDCache.Dump: RawUtf8;
var
  i: Integer;
begin
  result := '';

  for i := 0 to Pred(Count) do
  begin
    if i = 0 then
      result := FormatUtf8('{"GUID": "%", "Name": "%", "Type": "%"}', [fGUIDS[i], fNames[i], fTypes[i]])
    else
      result := FormatUtf8('%,{"GUID": "%", "Name": "%", "Type": "%"}', [result, fGUIDS[i], fNames[i], fTypes[i]]);
  end;
  result := FormatUtf8('[%]', [result]);
end;

end.

