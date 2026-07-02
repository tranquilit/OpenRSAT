unit unewsitelinkbridge;

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

  { TNewSiteLinkBridgePresenter }
  TNewSiteLinkBridgePresenter = class(TDoubleListLogic)
  private
    fObjectOU: RawUtf8;
    fLdap: TRsatLdapClient;

    function SearchSiteLinksInLdap: boolean;

  public
    constructor Create(ALdap: TRsatLdapClient; ObjectOU: RawUtf8);
    destructor Destroy; override;

    procedure GetAllResources; override;
    function GetResultName(Obj: TLdapResult): RawUtf8;
    function GetSiteLinkAttrValue(List: TLdapResultArray; idx: Integer; attr: RawUtf8): RawUtf8;
    function CanCreateSiteLinkBridge(const Name: RawUtf8): Boolean;
    function CreateSiteLinkBridge(const Name: RawUtf8): Boolean;

    property Ldap: TRsatLdapClient read fLdap;
  end;

implementation

constructor TNewSiteLinkBridgePresenter.Create(ALdap: TRsatLdapClient; ObjectOU: RawUtf8);
begin
  fLdap := ALdap;
  fObjectOU := ObjectOU;
end;

destructor TNewSiteLinkBridgePresenter.Destroy;
begin
  inherited Destroy;
end;

function TNewSiteLinkBridgePresenter.SearchSiteLinksInLdap: boolean;
begin
  Result := Ldap.Search(fObjectOU, false, '(&(objectClass=siteLink))', ['name', 'distinguishedName']);
end;

procedure TNewSiteLinkBridgePresenter.GetAllResources;
var
  LdapResult: TLdapResult;
begin
  Ldap.SearchBegin();
  try
    Ldap.SearchScope := lssSingleLevel;
    repeat
      if not SearchSiteLinksInLdap then
        Exit;

      for LdapResult in Ldap.SearchResult.Items do
        AddToList(LdapResult);
    until Ldap.SearchCookie = '';
  finally
    Ldap.SearchEnd;
  end;
end;

function TNewSiteLinkBridgePresenter.GetResultName(Obj: TLdapResult): RawUtf8;
begin
  Result := Obj.Find('name').GetReadable();
end;

function TNewSiteLinkBridgePresenter.GetSiteLinkAttrValue(List: TLdapResultArray; idx: Integer; attr: RawUtf8): RawUtf8;
begin
  Result := List[idx].Find(attr).GetReadable()
end;

function TNewSiteLinkBridgePresenter.CanCreateSiteLinkBridge(const Name: RawUtf8): Boolean;
begin
  Result := (Name <> '') and (Length(InResult) >= 2);
end;

function TNewSiteLinkBridgePresenter.CreateSiteLinkBridge(const Name: RawUtf8): Boolean;
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
    Attr.Add('siteLinkBridge');

    Attr := AttrList.Add('siteLinkList', GetSiteLinkAttrValue(InResult, 0, 'distinguishedName'));
    for i := 1 to High(InResult) do
      Attr.Add(GetSiteLinkAttrValue(InResult, i, 'distinguishedName'));

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

