unit ugeneralpropertysitelink;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.base,
  mormot.core.text,
  mormot.net.ldap,
  uproperty,
  ursatldapclient;

type
  
  { Ldap Result }
  TLdapResultArray = array of TLdapResult;
  
  { TGeneralPropertySiteLink }
  TGeneralPropertySiteLink = class
  private
    fProperty: TProperty;
    fLdap: TRsatLdapClient;
    fInSite, fNotInSite: TLdapResultArray;
    
    procedure RemoveItemFromArray(var List: TLdapResultArray; Index: Integer);
    procedure AddItemInResultArray(var List: TLdapResultArray; Item: TLdapResult);

    function SearchSitesInLdap: boolean;

  public
    constructor Create(Props: TProperty);

    procedure GetAllSites;
    procedure MoveItemToInSite(Index: Integer);
    procedure MoveItemToNotInSite(Index: Integer);

    function GetSitesInSiteLink: TLdapResultArray;
    function GetSitesNotInSiteLink: TLdapResultArray;
    function GetResultName(Obj: TLdapResult): RawUtf8;
    function GetSiteAttrValue(List: TLdapResultArray; idx: Integer; attr: RawUtf8): RawUtf8;
    function GetNbSites: Integer;

    property Ldap: TRsatLdapClient read fLdap;
  end;

implementation

constructor TGeneralPropertySiteLink.Create(Props: TProperty);
begin
  fProperty := Props;
  fLdap := fProperty.LdapClient;
end;

{ PRIVATE }
procedure TGeneralPropertySiteLink.RemoveItemFromArray(var List: TLdapResultArray; Index: Integer);
var
  i: Integer;
begin
  for i := Index to High(List) - 1 do
    List[i] := List[i + 1];

  SetLength(List, Length(List) - 1);
end;

procedure TGeneralPropertySiteLink.AddItemInResultArray(var List: TLdapResultArray; Item: TLdapResult);
var
  ListLen: Integer;
begin
  if not Assigned(Item) then
    exit;
  
  ListLen := Length(List);
  SetLength(List, ListLen + 1);
  List[ListLen] := TLdapResult(Item.Clone);
end;

function TGeneralPropertySiteLink.SearchSitesInLdap: boolean;
begin
  Result := fLdap.Search(FormatUtf8('CN=Sites,%', [fLdap.ConfigDN]), false, FormatUtf8('(&(objectClass=site))', []), ['name', 'distinguishedName'])
end;


{ PUBLIC }
procedure TGeneralPropertySiteLink.MoveItemToInSite(Index: Integer);
begin
  SetLength(fInSite, Length(fInSite) + 1);
  fInSite[High(fInSite)] := fNotInSite[Index];
  RemoveItemFromArray(fNotInSite, Index);
end;

procedure TGeneralPropertySiteLink.MoveItemToNotInSite(Index: Integer);
begin
  SetLength(fNotInSite, Length(fNotInSite) + 1);
  fNotInSite[High(fNotInSite)] := fInSite[Index];
  RemoveItemFromArray(fInSite, Index);
end;

procedure TGeneralPropertySiteLink.GetAllSites;
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

function TGeneralPropertySiteLink.GetSitesInSiteLink: TLdapResultArray;
begin
  Result := fInSite;
end;

function TGeneralPropertySiteLink.GetSitesNotInSiteLink: TLdapResultArray;
begin
  Result := fNotInSite;
end;

function TGeneralPropertySiteLink.GetResultName(Obj: TLdapResult): RawUtf8;
begin
  Result := Obj.Find('name').GetReadable();
end;

function TGeneralPropertySiteLink.GetSiteAttrValue(List: TLdapResultArray; idx: Integer; attr: RawUtf8): RawUtf8;
begin
  Result := List[idx].Find(attr).GetReadable()
end;

function TGeneralPropertySiteLink.GetNbSites: Integer;
begin
  Result := Length(fInSite) + Length(fNotInSite);
end;

end.

