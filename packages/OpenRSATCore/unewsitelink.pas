unit unewsitelink;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.base,
  mormot.core.text,
  mormot.net.ldap,
  udoublelistlogic,
  ursatldapclient;

type

  { TNewSiteLinkPresenter }
  TNewSiteLinkPresenter = class(TDoubleListLogic)
  private
    fObjectOU: RawUtf8;
    fLdap: TRsatLdapClient;

    function SearchSitesInLdap: boolean;

  public
    constructor Create(ALdap: TRsatLdapClient; ObjectOU: RawUtf8);
    destructor Destroy; override;

    procedure GetAllResources; override;
    function GetResultName(Obj: TLdapResult): RawUtf8;
    function GetSiteAttrValue(List: TLdapResultArray; idx: Integer; attr: RawUtf8): RawUtf8;
    function CanCreateSiteLink(const Name: RawUtf8): Boolean;
    function CreateSiteLink(const Name: RawUtf8): Boolean;

    property Ldap: TRsatLdapClient read fLdap;
  end;

implementation

constructor TNewSiteLinkPresenter.Create(ALdap: TRsatLdapClient; ObjectOU: RawUtf8);
begin
  fLdap := ALdap;
  fObjectOU := ObjectOU;
end;

destructor TNewSiteLinkPresenter.Destroy;
begin
  inherited Destroy;
end;

function TNewSiteLinkPresenter.SearchSitesInLdap: boolean;
begin
  Result := fLdap.Search(FormatUtf8('CN=Sites,%', [fLdap.ConfigDN]), false, '(&(objectClass=site))', ['name', 'distinguishedName']);
end;

procedure TNewSiteLinkPresenter.GetAllResources;
var
  LdapResult: TLdapResult;
begin
  Ldap.SearchBegin();
  try
    Ldap.SearchScope := lssSingleLevel;
    repeat
      if not SearchSitesInLdap then
        Exit;

      for LdapResult in Ldap.SearchResult.Items do
        AddToList(LdapResult);
    until Ldap.SearchCookie = '';
  finally
    Ldap.SearchEnd;
  end;
end;

function TNewSiteLinkPresenter.GetResultName(Obj: TLdapResult): RawUtf8;
begin
  Result := Obj.Find('name').GetReadable();
end;

function TNewSiteLinkPresenter.GetSiteAttrValue(List: TLdapResultArray; idx: Integer; attr: RawUtf8): RawUtf8;
begin
  Result := List[idx].Find(attr).GetReadable();
end;

function TNewSiteLinkPresenter.CanCreateSiteLink(const Name: RawUtf8): Boolean;
begin
  Result := (Name <> '') and (Length(InResult) >= 2);
end;

function TNewSiteLinkPresenter.CreateSiteLink(const Name: RawUtf8): Boolean;
var
  DN: RawUtf8;
  AttrList: TLdapAttributeList;
  Attr: TLdapAttribute;
  i, n: Integer;
begin
  Result := False;
  DN := FormatUtf8('CN=%,%', [Name, fObjectOU]);
  AttrList := TLdapAttributeList.Create;
  try
    Attr := AttrList.Add('objectClass', 'top');
    Attr.Add('siteLink');

    Attr := AttrList.Add('siteList', GetSiteAttrValue(InResult, 0, 'distinguishedName'));
    for i := 1 to High(InResult) do
      Attr.Add(GetSiteAttrValue(InResult, i, 'distinguishedName'));

    AttrList.Add('cost', '100');
    AttrList.Add('replInterval', '180');

    Result := fLdap.Add(DN, AttrList);
  finally
    AttrList.Free;
    for n := 0 to High(InResult) do
      FreeAndNil(InResult[n]);
    for n := 0 to High(OutResult) do
      FreeAndNil(OutResult[n]);
  end;
end;

end.

