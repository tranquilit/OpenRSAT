unit ursatstorage;

{$include mormot.defines.inc}

interface

uses
  // Lazarus / fpc
  Classes,
  SysUtils,
  // Submodules
  mormot.core.base,
  mormot.core.variants,
  mormot.net.ldap,
  // Rsat
  uoptions,
  usidcache;

type

  { TLdapStorage }

  TLdapStorage = class
  private
    fStorage: TDocVariantData;
    RetentionTimeSeconds: Integer;

    function InternalRetrieveObject(ObjectDN: RawUtf8): PDocVariantData;
    procedure InternalAddOrUpdate(pStorage: PDocVariantData; AttributeName: RawUtf8; Value: TRawUtf8DynArray); overload;
    procedure InternalAddOrUpdate(pStorage: PDocVariantData; Attribute: TLdapAttribute); overload;
    procedure InternalAddOrUpdate(pStorage: PDocVariantData; Attributes: TLdapAttributeList); overload;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure AddOrUpdate(ObjectDN, AttributeName: RawUtf8; Value: TRawUtf8DynArray); overload;
    procedure AddOrUpdate(ObjectDN: RawUtf8; Attribute: TLdapAttribute); overload;
    procedure AddOrUpdate(ObjectDN: RawUtf8; Attributes: TLdapAttributeList); overload;
    procedure AddOrUpdate(LdapResult: TLdapResult); overload;
    procedure AddOrUpdate(LdapResults: TLdapResultList); overload;

    // Check if the provided attribute is single valued.
    function IsSingleValue(ConfigDN, AttributeName: RawUtf8): Boolean;
    function GetSchemaObjectName(ConfigDN, AttributeName, AttributeValue: RawUtf8
      ): RawUtf8;

    function ListSchemaAttributeNames(ConfigDN, AttributeName: RawUtf8
      ): TRawUtf8DynArray;

    procedure ListSchemaAttributes(configDN: RawUtf8; data: PDocVariantData; AttributeNames: TRawUtf8DynArray);
    function GetSchemaObject(data: PDocVariantData; ObjectDN: RawUtf8; Attributes: TRawUtf8DynArray): boolean;

    function Count(ObjectDN, AttributeName: RawUtf8): Integer;
    function Get(ObjectDN, AttributeName: RawUtf8; Index: Integer = 0): RawUtf8;
    function GetFirst(ObjectDN, AttributeName: RawUtf8): RawUtf8;
    function GetLast(ObjectDN, AttributeName: RawUtf8): RawUtf8;
    function GetAll(ObjectDN, AttributeName: RawUtf8): TRawUtf8DynArray;

    procedure ToDocVariantData(data: PDocVariantData);
  end;

  { TRsatStorage }

  TRsatStorage = class(TSynPersistent)
  private
    fLdap: TLdapClient;
    fStorage: TLdapStorage;
    fSidCache: TSidCache;
    fOptions: TOptions;

    function GetStorage: TLdapStorage;
    procedure SetLdap(AValue: TLdapClient);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure SetupSchema;

    function SearchObject(ObjectDN, Filter, Attribute: RawUtf8): TLdapAttribute; overload;
    function SearchObject(ObjectDN, Filter: RawUtf8; Attributes: TRawUtf8DynArray): TLdapResult; overload;
    function Search(BaseDN: RawUtf8; TypesOnly: Boolean; Filter: RawUtf8;
      Attributes: TRawUtf8DynArray): Boolean;

    function ListSchemaAttributeNames(AttributeName: RawUtf8): TRawUtf8DynArray;
    procedure ListSchemaAttributes(data: PDocVariantData; AttributeNames: TRawUtf8DynArray);
    function IsSingleValued(AttributeName: RawUtf8): Boolean;
    function GetSchemaObjectName(AttributeName, AttributeValue: RawUtf8): RawUtf8;
    function GetSchemaObject(data: PDocVariantData; ObjectDN: RawUtf8; Attributes: TRawUtf8DynArray): boolean;

    property Storage: TLdapStorage read GetStorage;
    property SidCache: TSidCache read fSidCache;
    property Options: TOptions read fOptions;
    property Ldap: TLdapClient read fLdap write SetLdap;
  end;

implementation
uses
  // Submodules
  mormot.core.log,
  mormot.core.text;

{ TLdapStorage }

function TLdapStorage.InternalRetrieveObject(ObjectDN: RawUtf8): PDocVariantData;
var
  CN: RawUtf8;
  path: TStringArray;
  i, pathLen: Integer;
  pStorage: PDocVariantData;
begin
  result := nil;

  try
    CN := DNToCN(ObjectDN);
  except
    Exit;
  end;

  path := String(CN).Split('/');
  pathLen := Length(path);
  pStorage := @fStorage;

  if not Assigned(path) or (pathLen <= 0) then
    Exit;

  for i := 0 to pathLen - 2 do
  begin
    if not Assigned(pStorage) or (path[i] = '') then
      Exit;
    pStorage := pStorage^.O_[path[i]]^.O_['children'];
  end;
  if not Assigned(pStorage) or (path[pathLen - 1] = '') then
    Exit;
  pStorage := pStorage^.O_[path[pathLen - 1]];

  result := pStorage;
end;

procedure TLdapStorage.InternalAddOrUpdate(pStorage: PDocVariantData;
  AttributeName: RawUtf8; Value: TRawUtf8DynArray);
var
  data: TDocVariantData;
  v: RawUtf8;
begin
  if not Assigned(pStorage) then
    Exit;

  data.init();
  for v in Value do
    data.AddItem(v);
  pStorage^.O_['values']^.O_[AttributeName]^.AddOrUpdateValue('value', Variant(data));
  pStorage^.O_['values']^.O_[AttributeName]^.AddOrUpdateValue('timestamp', Now);
end;

procedure TLdapStorage.InternalAddOrUpdate(pStorage: PDocVariantData;
  Attribute: TLdapAttribute);
begin
  if not Assigned(pStorage) or not Assigned(Attribute) then
    Exit;

  InternalAddOrUpdate(pStorage, Attribute.AttributeName, Attribute.GetAllReadable);
end;

procedure TLdapStorage.InternalAddOrUpdate(pStorage: PDocVariantData;
  Attributes: TLdapAttributeList);
var
  Attr: TLdapAttribute;
begin
  if not Assigned(pStorage) or not Assigned(Attributes) then
    Exit;

  for Attr in Attributes.Items do
    InternalAddOrUpdate(pStorage, Attr);
end;

constructor TLdapStorage.Create;
begin
  fStorage.Init();

  RetentionTimeSeconds := 300;
end;

destructor TLdapStorage.Destroy;
begin
  inherited Destroy;
end;

procedure TLdapStorage.Clear;
begin
  fStorage.Clear;
end;

procedure TLdapStorage.AddOrUpdate(ObjectDN, AttributeName: RawUtf8;
  Value: TRawUtf8DynArray);
var
  pStorage: PDocVariantData;
begin
  pStorage := InternalRetrieveObject(ObjectDN);
  if not Assigned(pStorage) then
    Exit;

  InternalAddOrUpdate(pStorage, AttributeName, Value);
end;

procedure TLdapStorage.AddOrUpdate(ObjectDN: RawUtf8; Attribute: TLdapAttribute
  );
var
  pStorage: PDocVariantData;
begin
  if not Assigned(Attribute) then
    Exit;

  pStorage := InternalRetrieveObject(ObjectDN);
  if not Assigned(pStorage) then
    Exit;

  InternalAddOrUpdate(pStorage, Attribute);
end;

procedure TLdapStorage.AddOrUpdate(ObjectDN: RawUtf8;
  Attributes: TLdapAttributeList);
var
  pStorage: PDocVariantData;
begin
  if not Assigned(Attributes) then
    Exit;

  pStorage := InternalRetrieveObject(ObjectDN);
  if not Assigned(pStorage) then
    Exit;

  InternalAddOrUpdate(pStorage, Attributes);
end;

procedure TLdapStorage.AddOrUpdate(LdapResult: TLdapResult);
var
  pStorage: PDocVariantData;
begin
  if not Assigned(LdapResult) then
    Exit;

  pStorage := InternalRetrieveObject(LdapResult.ObjectName);
  if not Assigned(pStorage) then
    Exit;

  InternalAddOrUpdate(pStorage, LdapResult.Attributes);
end;

procedure TLdapStorage.AddOrUpdate(LdapResults: TLdapResultList);
var
  LdapResult: TLdapResult;
begin
  if not Assigned(LdapResults) then
    Exit;

  for LdapResult in LdapResults.Items do
    AddOrUpdate(LdapResult);
end;

function TLdapStorage.IsSingleValue(ConfigDN, AttributeName: RawUtf8): Boolean;
var
  pStorage, pItem: PDocVariantData;
begin
  result := False;

  pStorage := InternalRetrieveObject('CN=Schema,' + ConfigDN);
  if not Assigned(pStorage) or
     not pStorage^.Exists('children') then
    Exit;

  for pItem in pStorage^.O['children']^.Objects do
  begin
    if not Assigned(pItem) or
       not pItem^.Exists('values') or
       not pItem^.O['values']^.Exists('lDAPDisplayName') or
       not pItem^.O['values']^.Exists('isSingleValued') or
       not (pItem^.O['values']^.A['lDAPDisplayName']^.ToRawUtf8DynArray[0] = AttributeName) then
      continue;
    result := (pItem^.O['values']^.A['isSingleValued']^.ToRawUtf8DynArray[0] = 'TRUE');
    Exit;
  end;
end;

function TLdapStorage.ListSchemaAttributeNames(ConfigDN, AttributeName: RawUtf8
  ): TRawUtf8DynArray;
var
  pStorage, pData: PDocVariantData;
  i: Integer;
begin
  result := nil;

  pStorage := InternalRetrieveObject('CN=Schema,' + ConfigDN);
  if not Assigned(pStorage) or
     not pStorage^.Exists('children') then
    Exit;

  SetLength(result, pStorage^.O['children']^.Count);
  for i := 0 to pStorage^.O['children']^.Count - 1 do
  begin
    pData := pStorage^.O_['children']^._[i]^.O_['values']^.O_[AttributeName]^.A_['value'];
    if Assigned(pData) and (pData.Count > 0) then
      result[i] := pData^.ToRawUtf8DynArray[0]
    else
      result[i] := '';
  end;
end;

procedure TLdapStorage.ListSchemaAttributes(configDN: RawUtf8;
  data: PDocVariantData; AttributeNames: TRawUtf8DynArray);
var
  pItem, pStorage: PDocVariantData;
  rowData: TDocVariantData;
  AttributeName: RawUtf8;
begin
  pStorage := InternalRetrieveObject('CN=Schema,' + ConfigDN);
  if not Assigned(pStorage) then
    Exit;

  rowData.init();
  for pItem in pStorage^.O['children']^.Objects do
  begin
    for AttributeName in AttributeNames do
      rowData.AddOrUpdateValue(AttributeName, Variant(pItem^.O['values']^.O[AttributeName].A['value']^));
    data.AddItem(rowData);
    rowData.Clear;
  end;
end;

function TLdapStorage.GetSchemaObject(data: PDocVariantData; ObjectDN: RawUtf8;
  Attributes: TRawUtf8DynArray): boolean;
var
  pStorage: PDocVariantData;
  Attribute: RawUtf8;
begin
  result := False;

  pStorage := InternalRetrieveObject(ObjectDN);
  if not Assigned(pStorage) then
    Exit;
  pStorage := pStorage^.O['values'];

  for Attribute in Attributes do
    data.AddOrUpdateValue(Attribute, Variant(pStorage^.O[Attribute]^.A['value']^));
end;

function TLdapStorage.Count(ObjectDN, AttributeName: RawUtf8): Integer;
var
  Values: TRawUtf8DynArray;
begin
  result := -1;

  Values := GetAll(ObjectDN, AttributeName);
  if not Assigned(Values) then
    Exit;
  result := Length(Values);
end;

function TLdapStorage.Get(ObjectDN, AttributeName: RawUtf8; Index: Integer
  ): RawUtf8;
var
  Values: TRawUtf8DynArray;
  len: Integer;
begin
  result := '';

  Values := GetAll(ObjectDN, AttributeName);
  if not Assigned(Values) then
    Exit;
  len := Length(Values);
  if (index < 0) or (index >= len) then
    Exit;
  result := Values[Index];
end;

function TLdapStorage.GetFirst(ObjectDN, AttributeName: RawUtf8): RawUtf8;
begin
  result := Get(ObjectDN, AttributeName);
end;

function TLdapStorage.GetLast(ObjectDN, AttributeName: RawUtf8): RawUtf8;
var
  Values: TRawUtf8DynArray;
  len: SizeInt;
begin
  result := '';

  Values := GetAll(ObjectDN, AttributeName);
  if not Assigned(Values) then
    Exit;
  len := Length(Values);
  if (len <= 0) then
    Exit;
  result := Values[len - 1];
end;

function TLdapStorage.GetAll(ObjectDN, AttributeName: RawUtf8): TRawUtf8DynArray;
var
  pStorage: PDocVariantData;
begin
  result := nil;

  pStorage := InternalRetrieveObject(ObjectDN);
  if not Assigned(pStorage) then
    Exit;

  if not pStorage^.Exists('values') or
     not pStorage^.O['values']^.Exists(AttributeName) then
    Exit;

  result := pStorage^.O['values']^.O[AttributeName]^.A['value']^.ToRawUtf8DynArray;
end;

function TLdapStorage.GetSchemaObjectName(ConfigDN, AttributeName,
  AttributeValue: RawUtf8): RawUtf8;
var
  pStorage, pItem: PDocVariantData;
begin
  result := '';

  pStorage := InternalRetrieveObject('CN=Schema,' + ConfigDN);
  if not Assigned(pStorage) or
     not pStorage^.Exists('children') then
    Exit;

  for pItem in pStorage^.O['children']^.Objects do
  begin
    if not pItem^.Exists('values') or
       not pItem^.O['values']^.Exists(AttributeName) or
       not pItem^.O['values']^.O[AttributeName]^.Exists('value') or
       not (pItem^.O['values']^.O[AttributeName]^.A['value']^.ToRawUtf8DynArray[0] = AttributeValue) then
      continue;
    result := pItem^.O['values']^.O['distinguishedName']^.A['value']^.ToRawUtf8DynArray[0];
    break;
  end;
end;

procedure TLdapStorage.ToDocVariantData(data: PDocVariantData);
begin
  data^.InitCopy(Variant(fStorage), []);
end;

{ TRsatStorage }

function TRsatStorage.GetStorage: TLdapStorage;
begin
  result := nil;

  if not Assigned(fLdap) then
    Exit;
  result := fStorage;
end;

procedure TRsatStorage.SetLdap(AValue: TLdapClient);
var
  start: TDateTime;
begin
  if fLdap=AValue then Exit;
  fLdap:=AValue;

  if not Assigned(fLdap) then
  begin
    FreeAndNil(fStorage);
    FreeAndNil(fSidCache);
    Exit;
  end;

  fStorage := TLdapStorage.Create;
  fSidCache := TSidCache.Create(fLdap, fLdap.DefaultDN());

  start := Now;
  SetupSchema;
  TSynLog.Add.Log(sllDebug, FormatDateTime('ss:zzz', Now - start));
end;

constructor TRsatStorage.Create;
begin
  inherited Create;

  fStorage := nil;
  fSidCache := nil;
  fOptions := TOptions.Create;
end;

destructor TRsatStorage.Destroy;
begin
  FreeAndNil(fStorage);
  FreeAndNil(fSidCache);
  FreeAndNil(fOptions);

  inherited Destroy;
end;

procedure TRsatStorage.SetupSchema;
const
  BASE_SCHEMA_OBJECTS: TRawUtf8DynArray = ['CN=Top', 'CN=Attribute-Schema', 'CN=Class-Schema'];
var
  SearchResult: TLdapAttribute;
  baseSchemaObject, SchemaDN: RawUtf8;
  Attributes: TRawUtf8DynArray;
  i: Integer;
begin
  attributes := ['distinguishedName'];

  for baseSchemaObject in BASE_SCHEMA_OBJECTS do
  begin
    SchemaDN := FormatUtf8('%,CN=Schema,%', [baseSchemaObject, fLdap.ConfigDN]);
    SearchResult := fLdap.SearchObject(SchemaDN, '', 'systemMustContain');
    if not Assigned(SearchResult) then
      continue;
    for i := 0 to SearchResult.count - 1 do
      if SearchResult.GetReadable(i) <> 'nTSecurityDescriptor' then
        Insert(SearchResult.GetReadable(i), attributes, 0);
  end;

  fLdap.SearchBegin();
  try
    fLdap.SearchScope := lssWholeSubtree;
    repeat
      if not fLdap.Search('CN=Schema, ' + fLdap.ConfigDN, False, '', ['*']) then
        Exit;

      fStorage.AddOrUpdate(fLdap.SearchResult);
    until fLdap.SearchCookie = '';
  finally
    fLdap.SearchEnd;
  end;
end;

function TRsatStorage.SearchObject(ObjectDN, Filter, Attribute: RawUtf8): TLdapAttribute;
begin
  result := fLdap.SearchObject(ObjectDN, Filter, Attribute);
  if not Assigned(result) then
    Exit;
  fStorage.AddOrUpdate(ObjectDN, result);
end;

function TRsatStorage.SearchObject(ObjectDN, Filter: RawUtf8;
  Attributes: TRawUtf8DynArray): TLdapResult;
begin
  result := fLdap.SearchObject(ObjectDN, Filter,  Attributes);
  if not Assigned(result) then
    Exit;
  fStorage.AddOrUpdate(result);
end;

function TRsatStorage.Search(BaseDN: RawUtf8; TypesOnly: Boolean;
  Filter: RawUtf8; Attributes: TRawUtf8DynArray): Boolean;
begin
  result := fLdap.Search(BaseDN, TypesOnly, Filter, Attributes);
  if not result then
    Exit;
  fStorage.AddOrUpdate(fLdap.SearchResult);
end;

function TRsatStorage.ListSchemaAttributeNames(AttributeName: RawUtf8
  ): TRawUtf8DynArray;
begin
  result := fStorage.ListSchemaAttributeNames(fLdap.ConfigDN, AttributeName);
end;

procedure TRsatStorage.ListSchemaAttributes(data: PDocVariantData;
  AttributeNames: TRawUtf8DynArray);
begin
  fStorage.ListSchemaAttributes(fLdap.ConfigDN, data, AttributeNames);
end;

function TRsatStorage.IsSingleValued(AttributeName: RawUtf8): Boolean;
begin
  result := fStorage.IsSingleValue(fLdap.ConfigDN, AttributeName);
end;

function TRsatStorage.GetSchemaObjectName(AttributeName, AttributeValue: RawUtf8
  ): RawUtf8;
begin
  result := fStorage.GetSchemaObjectName(fLdap.ConfigDN, AttributeName, AttributeValue);
end;

function TRsatStorage.GetSchemaObject(data: PDocVariantData; ObjectDN: RawUtf8;
  Attributes: TRawUtf8DynArray): boolean;
begin
  result := fStorage.GetSchemaObject(data, ObjectDN, Attributes);
end;

end.

