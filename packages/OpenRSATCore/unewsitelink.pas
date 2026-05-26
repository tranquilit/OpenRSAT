unit unewsitelink;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.base,
  mormot.core.text,
  mormot.net.ldap,
  ursatldapclient;

type

  { Ldap Result }
  TLdapResultArray = array of TLdapResult;

  { TNewSiteLinkPresenter }
  TNewSiteLinkPresenter = class
  private
    fLdap: TRsatLdapClient;
    fInSite, fNotInSite: TLdapResultArray;
    
    procedure RemoveItemFromArray(var List: TLdapResultArray; Index: Integer);
    procedure AddItemInResultArray(var List: TLdapResultArray; Item: TLdapResult);

    function SearchSitesInLdap: boolean;

  public
    constructor Create(ALdap: TRsatLdapClient);

    procedure GetAllSites;
    procedure MoveItemToInSite(Index: Integer);
    procedure MoveItemToNotInSite(Index: Integer);

    function GetSitesInSiteLink: TLdapResultArray;
    function GetSitesNotInSiteLink: TLdapResultArray;
    function GetResultName(Obj: TLdapResult): RawUtf8;
    function GetSiteAttrValue(List: TLdapResultArray; idx: Integer; attr: RawUtf8): RawUtf8;
    function GetNbSites: Integer;
    function CanCreateSiteLink(const Name: RawUtf8): Boolean;
    function CreateSiteLink(const Name: RawUtf8): Boolean;

    property Ldap: TRsatLdapClient read fLdap;
  end;

implementation

constructor TNewSiteLinkPresenter.Create(ALdap: TRsatLdapClient);
begin
  fLdap := ALdap;
end;

{ PRIVATE }
procedure TNewSiteLinkPresenter.RemoveItemFromArray(var List: TLdapResultArray; Index: Integer);
var
  i: Integer;
begin
  for i := Index to High(List) - 1 do
    List[i] := List[i + 1];

  SetLength(List, Length(List) - 1);
end;

procedure TNewSiteLinkPresenter.AddItemInResultArray(var List: TLdapResultArray; Item: TLdapResult);
var
  ListLen: Integer;
begin
  if not Assigned(Item) then
    exit;
  
  ListLen := Length(List);
  SetLength(List, ListLen + 1);
  List[ListLen] := TLdapResult(Item.Clone);
end;

function TNewSiteLinkPresenter.SearchSitesInLdap: boolean;
begin
  Result := fLdap.Search(FormatUtf8('CN=Sites,%', [fLdap.ConfigDN]), false, FormatUtf8('(&(objectClass=site))', []), ['name', 'distinguishedName'])
end;

{ PUBLIC }
procedure TNewSiteLinkPresenter.MoveItemToInSite(Index: Integer);
begin
  SetLength(fInSite, Length(fInSite) + 1);
  fInSite[High(fInSite)] := fNotInSite[Index];
  RemoveItemFromArray(fNotInSite, Index);
end;

procedure TNewSiteLinkPresenter.MoveItemToNotInSite(Index: Integer);
begin
  SetLength(fNotInSite, Length(fNotInSite) + 1);
  fNotInSite[High(fNotInSite)] := fInSite[Index];
  RemoveItemFromArray(fInSite, Index);
end;

procedure TNewSiteLinkPresenter.GetAllSites;
var
  LdapResult: TLdapResult;
begin
  fLdap.SearchBegin();
  fLdap.SearchScope := lssSingleLevel;
  repeat
    if not SearchSitesInLdap then
      Exit;
    
    for LdapResult in fLdap.SearchResult.Items do
      AddItemInResultArray(fNotInSite, LdapResult);
  until fLdap.SearchCookie = '';
  fLdap.SearchEnd;
end;

function TNewSiteLinkPresenter.GetSitesInSiteLink: TLdapResultArray;
begin
  Result := fInSite;
end;

function TNewSiteLinkPresenter.GetSitesNotInSiteLink: TLdapResultArray;
begin
  Result := fNotInSite;
end;

function TNewSiteLinkPresenter.GetResultName(Obj: TLdapResult): RawUtf8;
begin
  Result := Obj.Find('name').GetReadable();
end;

function TNewSiteLinkPresenter.GetSiteAttrValue(List: TLdapResultArray; idx: Integer; attr: RawUtf8): RawUtf8;
begin
  Result := List[idx].Find(attr).GetReadable()
end;

function TNewSiteLinkPresenter.GetNbSites: Integer;
begin
  Result := Length(fInSite) + Length(fNotInSite);
end;

function TNewSiteLinkPresenter.CanCreateSiteLink(const Name: RawUtf8): Boolean;
begin
  Result := (Name <> '') and (Length(fInSite) >= 2);
end;

function TNewSiteLinkPresenter.CreateSiteLink(const Name: RawUtf8): Boolean;
var
  DN: RawUtf8;
  AttrList: TLdapAttributeList;
  Attr: TLdapAttribute;
  i: Integer;
begin
  Result := False;
  DN := FormatUtf8('CN=%,CN=SMTP,CN=Inter-Site Transports,CN=Sites,%', [Name, fLdap.ConfigDN]);
  AttrList := TLdapAttributeList.Create;
  try
    Attr := AttrList.Add('objectClass', 'top');
    Attr.Add('siteLink');
    Attr := AttrList.Add('siteList', GetSiteAttrValue(fInSite, 0, 'distinguishedName'));

    for i := 1 to High(fInSite) do
      Attr.Add(GetSiteAttrValue(fInSite, i, 'distinguishedName'));

    AttrList.Add('cost', '100');
    AttrList.Add('replInterval', '180');

    Result := fLdap.Add(DN, AttrList);
  finally
    AttrList.Free;
  end;
end;

end.

