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
  udoublelistlogic,
  ursatldapclient;

type
  TLdapResultArray = array of TLdapResult;

  TGeneralPropertySiteLink = class(TDoubleListLogic)
  private
    function SearchSitesInLdap: boolean;
  public
    constructor Create(P: TProperty);
    procedure GetAllResources; override;
    procedure SyncAttributeProperty(Option: TLdapAddOption); override;
  end;

implementation

constructor TGeneralPropertySiteLink.Create(P: TProperty);
begin
  Props := P;
  Ldap := P.LdapClient;
end;

procedure TGeneralPropertySiteLink.GetAllResources;
var
  LdapResult: TLdapResult;
begin
  Ldap.SearchBegin();
  Ldap.SearchScope := lssSingleLevel;
  repeat
    if not SearchSitesInLdap then
      Exit;

    for LdapResult in Ldap.SearchResult.Items do
      AddToList(LdapResult);
  until Ldap.SearchCookie = '';
  Ldap.SearchEnd;
end;

function TGeneralPropertySiteLink.SearchSitesInLdap: boolean;
begin
  Result := Ldap.Search(FormatUtf8('CN=Sites,%', [Ldap.ConfigDN]), false, '(&(objectClass=site))', ['name', 'distinguishedName']);
end;

procedure TGeneralPropertySiteLink.SyncAttributeProperty(Option: TLdapAddOption);
var
  i: Integer;
  DN: RawUtf8;
begin
  if Length(InResult) = 0 then
  begin
    Props.Add('siteList', '', aoReplaceValue);
    Exit;
  end;

  DN := InResult[0].Find('distinguishedName').GetReadable();
  Props.Add('siteList', DN, aoReplaceValue);

  for i := 1 to High(InResult) do
  begin
    DN := InResult[i].Find('distinguishedName').GetReadable();
    Props.Add('siteList', DN, aoNoDuplicateValue);
  end;
end;

end.

