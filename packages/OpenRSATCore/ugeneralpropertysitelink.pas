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
  usitelinklogic,
  ursatldapclient;

type
  TLdapResultArray = array of TLdapResult;

  TGeneralPropertySiteLink = class(TSiteLinkLogic)
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
      AddToNotInSite(LdapResult);
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
  if Length(InSite) = 0 then
  begin
    Props.Add('siteList', '', aoReplaceValue);
    Exit;
  end;

  DN := InSite[0].Find('distinguishedName').GetReadable();
  Props.Add('siteList', DN, aoReplaceValue);

  for i := 1 to High(InSite) do
  begin
    DN := InSite[i].Find('distinguishedName').GetReadable();
    Props.Add('siteList', DN, aoNoDuplicateValue);
  end;
end;

end.

