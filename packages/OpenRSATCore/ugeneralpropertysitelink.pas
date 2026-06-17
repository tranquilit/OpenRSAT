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
  TGeneralPropertySiteLink = class(TDoubleListLogic)
  private
    fProperty: TProperty;
    fLdap: TLdapClient;

    function SearchSitesInLdap: boolean;
  public
    constructor Create(P: TProperty);

    procedure GetAllResources; override;
    procedure SyncAttributeProperty(Option: TLdapAddOption);
    procedure SetScalarProperty(const Attribute, Value: RawUtf8; Option: TLdapAddOption);
    function FindAttribute(Attribute: RawUtf8): TLdapAttribute; virtual;
    function FindAttribute(Attribute: RawUtf8; LdapResult: TLdapResult): TLdapAttribute; virtual;

    property Props: TProperty read fProperty write fProperty;
    property Ldap: TLdapClient read fLdap write fLdap;
  end;

implementation

constructor TGeneralPropertySiteLink.Create(P: TProperty);
begin
  fProperty := P;
  fLdap := P.LdapClient;
end;

procedure TGeneralPropertySiteLink.GetAllResources;
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

procedure TGeneralPropertySiteLink.SetScalarProperty(const Attribute, Value: RawUtf8; Option: TLdapAddOption);
begin
  Props.Add(Attribute, Value, Option);
end;

function TGeneralPropertySiteLink.FindAttribute(Attribute: RawUtf8): TLdapAttribute;
begin
  Result := fProperty.Attributes.Find(Attribute);
end;

function TGeneralPropertySiteLink.FindAttribute(Attribute: RawUtf8; LdapResult: TLdapResult): TLdapAttribute;
begin
  Result := LdapResult.Attributes.Find(Attribute);
end;

end.

