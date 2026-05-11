unit uselectobjectguidpresenter;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.base,
  mormot.core.text,
  mormot.core.variants,
  mormot.net.ldap;

type

  { ISelectObjectGUIDView }

  ISelectObjectGUIDView = Interface
    procedure SetMultiSelect(AMultiSelect: Boolean);
    procedure SetLdapClient(ALdapClient: TLdapClient);
    procedure LoadData(AData: PDocVariantData);
    function GetFilter: RawUtf8;
    function GetSelectedGUID: RawUtf8;
    function GetSelectedName: RawUtf8;
    procedure ShowLdapError(ALdapErrorString: RawUtf8; ALdapError: TLdapError);
  end;

  { TSelectObjectGUIDPresenter }

  TSelectObjectGUIDPresenter = class
  private
    fAllowedGUIDType: TRawUtf8DynArray;
    fView: ISelectObjectGUIDView;
    fLdapClient: TLdapClient;
    fData: TDocVariantData;
    fNeedRefresh: Boolean;

    procedure SearchClassSchema();
    procedure SearchAttributeSchema();
    procedure SearchExtendedRights();

    function GetExtendedRightTypeName(AValidAccesses: RawUtf8): RawUtf8;
    procedure AddItem(ADistinguishedName, AName, AGUID, AType: RawUtf8);
    procedure AddSearchClassSchema(ALdapResultList: TLdapResultList);
    procedure AddSearchAttributeSchema(ALdapResultList: TLdapResultList);
    procedure AddSearchExtendedRights(ALdapResultList: TLdapResultList);

    procedure BuildGridData(AFilter: RawUtf8; AAllowedType: TRawUtf8DynArray; AGridData: PDocVariantData);
  public
    constructor Create(AView: ISelectObjectGUIDView);

    procedure SetLdapClient(ALdapClient: TLdapClient);
    procedure Refresh;
    procedure Search;

    property NeedRefresh: Boolean read fNeedRefresh;
    property AllowedGUIDType: TRawUtf8DynArray read fAllowedGUIDType write fAllowedGUIDType;
  end;

implementation

{ TSelectObjectGUIDPresenter }

procedure TSelectObjectGUIDPresenter.Search;
var
  GridData: TDocVariantData;
  Filter: RawUtf8;
begin
  if NeedRefresh then
    Refresh;

  Filter := fView.GetFilter;

  GridData.InitArray([], JSON_FAST);
  BuildGridData(Filter, AllowedGUIDType, @GridData);
  fView.LoadData(@GridData);
end;

procedure TSelectObjectGUIDPresenter.SearchClassSchema;
const
  cPAGE_SIZE = 1000;
begin
  fLdapClient.SearchBegin(cPAGE_SIZE);
  try
    fLdapClient.SearchScope := lssSingleLevel;
    repeat
      if not fLdapClient.Search(fLdapClient.SchemaDN, False, '(objectClass=classSchema)', ['distinguishedName', 'cn', 'schemaIDGUID']) then
      begin
        fView.ShowLdapError(fLdapClient.ResultString, fLdapClient.ResultError);
        Exit;
      end;
      AddSearchClassSchema(fLdapClient.SearchResult);
    until fLdapClient.SearchCookie = '';
  finally
    fLdapClient.SearchEnd;
  end;
end;

procedure TSelectObjectGUIDPresenter.SearchAttributeSchema;
const
  cPAGE_SIZE = 1000;
begin
  fLdapClient.SearchBegin(cPAGE_SIZE);
  try
    fLdapClient.SearchScope := lssSingleLevel;
    repeat
      if not fLdapClient.Search(fLdapClient.SchemaDN, False, '(objectClass=attributeSchema)', ['distinguishedName', 'cn', 'schemaIDGUID']) then
      begin
        fView.ShowLdapError(fLdapClient.ResultString, fLdapClient.ResultError);
        Exit;
      end;
      AddSearchAttributeSchema(fLdapClient.SearchResult);
    until fLdapClient.SearchCookie = '';
  finally
    fLdapClient.SearchEnd;
  end;
end;

procedure TSelectObjectGUIDPresenter.SearchExtendedRights;
const
  cPAGE_SIZE = 1000;
var
  ExtendedRightsDN: RawUtf8;
begin
  ExtendedRightsDN := FormatUtf8('CN=Extended-Rights,%', [fLdapClient.ConfigDN]);
  fLdapClient.SearchBegin(cPAGE_SIZE);
  try
    fLdapClient.SearchScope := lssSingleLevel;
    repeat
      if not fLdapClient.Search(ExtendedRightsDN, False, '(objectClass=controlAccessRight)', ['distinguishedName', 'cn', 'rightsGuid', 'validAccesses']) then
      begin
        fView.ShowLdapError(fLdapClient.ResultString, fLdapClient.ResultError);
        Exit;
      end;
      AddSearchExtendedRights(fLdapClient.SearchResult);
    until fLdapClient.SearchCookie = '';
  finally
    fLdapClient.SearchEnd;
  end;
end;

function TSelectObjectGUIDPresenter.GetExtendedRightTypeName(AValidAccesses: RawUtf8): RawUtf8;
var
  ValidAccesses: Integer;
begin
  result := 'controlAccessRight';
  ValidAccesses := 0;
  if not TryStrToInt(AValidAccesses, ValidAccesses) then
    Exit;

  if ((ValidAccesses and 256) <> 0) then
    result := 'extendedRight'
  else if ((ValidAccesses and 8) <> 0) then
    result := 'validateRight'
  else if ((ValidAccesses and 16) <> 0) then
    result := 'propertySet';
end;

procedure TSelectObjectGUIDPresenter.AddItem(ADistinguishedName, AName, AGUID, AType: RawUtf8);
var
  NewItem: TDocVariantData;
begin
  NewItem.Init(JSON_FAST);
  NewItem.AddOrUpdateValue('distinguishedName', ADistinguishedName);
  NewItem.AddOrUpdateValue('name', AName);
  NewItem.AddOrUpdateValue('guid', AGUID);
  NewItem.AddOrUpdateValue('type', AType);
  fData.AddItem(NewItem);
end;

procedure TSelectObjectGUIDPresenter.AddSearchClassSchema(ALdapResultList: TLdapResultList);
var
  i: Integer;
begin
  for i := 0 to Pred(ALdapResultList.Count) do
  begin
    AddItem(
      ALdapResultList.Items[i].Find('distinguishedName').GetReadable(),
      ALdapResultList.Items[i].Find('cn').GetReadable(),
      ToUtf8(PGuid(ALdapResultList.Items[i].Find('schemaIDGUID').GetRaw())^),
      'classSchema'
    );
  end;
end;

procedure TSelectObjectGUIDPresenter.AddSearchAttributeSchema(ALdapResultList: TLdapResultList);
var
  i: Integer;
begin
  for i := 0 to Pred(ALdapResultList.Count) do
  begin
    AddItem(
      ALdapResultList.Items[i].Find('distinguishedName').GetReadable(),
      ALdapResultList.Items[i].Find('cn').GetReadable(),
      ToUtf8(PGuid(ALdapResultList.Items[i].Find('schemaIDGUID').GetRaw())^),
      'attributeSchema'
    );
  end;
end;

procedure TSelectObjectGUIDPresenter.AddSearchExtendedRights(ALdapResultList: TLdapResultList);
var
  i: Integer;
begin
  for i := 0 to Pred(ALdapResultList.Count) do
  begin
    AddItem(
      ALdapResultList.Items[i].Find('distinguishedName').GetReadable(),
      ALdapResultList.Items[i].Find('cn').GetReadable(),
      ALdapResultList.Items[i].Find('rightsGuid').GetReadable(),
      GetExtendedRightTypeName(ALdapResultList.Items[i].Find('validAccesses').GetReadable())
    );
  end;
end;

procedure TSelectObjectGUIDPresenter.BuildGridData(AFilter: RawUtf8; AAllowedType: TRawUtf8DynArray;
  AGridData: PDocVariantData);
var
  i: Integer;
  PData: PDocVariantData;
  LoweredFilter: RawUtf8;

  function TypeAllowed(PData: PDocVariantData; AAllowedType: TRawUtf8DynArray): Boolean;
  var
    t: RawUtf8;
  begin
    result := True;
    if Assigned(AAllowedType) then
    begin
      for t in AAllowedType do
        if (t = PData^.S['type']) then
          Exit;
      result := False;
    end;
  end;

  function NameAllowed(PData: PDocVariantData; AFilter: RawUtf8): Boolean;
  begin
    result := (AFilter = '') or PData^.S['name'].ToLower.Contains(AFilter);
  end;

begin
  LoweredFilter := LowerCase(AFilter);
  for i := 0 to Pred(fData.Count) do
  begin
    PData := fData._[i];
    if TypeAllowed(PData, AAllowedType) and NameAllowed(PData, LoweredFilter) then
      AGridData^.AddItem(PData^);
  end;
end;

constructor TSelectObjectGUIDPresenter.Create(AView: ISelectObjectGUIDView);
begin
  fView := AView;

  fData.InitArray([], JSON_FAST);
  fNeedRefresh := True;
end;

procedure TSelectObjectGUIDPresenter.SetLdapClient(ALdapClient: TLdapClient);
begin
  fLdapClient := ALdapClient;
end;

procedure TSelectObjectGUIDPresenter.Refresh;
begin
  SearchClassSchema;
  SearchAttributeSchema;
  SearchExtendedRights;
end;

end.

