unit ugeneralpropertysitelinkbridge;

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
  TGeneralPropertySiteLinkBridge = class(TDoubleListLogic)
  private
    fProperty: TProperty;
    fLdap: TLdapClient;

    function SearchSitesInLdap: boolean;
    function ExtractGroup(const S: RawUtf8): RawUtf8;
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

constructor TGeneralPropertySiteLinkBridge.Create(P: TProperty);
begin
  inherited Create;
  fProperty := P;
  fLdap := P.LdapClient;
end;

procedure TGeneralPropertySiteLinkBridge.GetAllResources;
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

function TGeneralPropertySiteLinkBridge.SearchSitesInLdap: boolean;
var
  Group: RawUtf8;
begin
  Group := ExtractGroup(Props.distinguishedName);

  if Group = '' then
    Exit(False);

  Result := Ldap.Search(FormatUtf8('%,CN=Inter-Site Transports,CN=Sites,%', [Group, Ldap.ConfigDN]), false, '(&(objectClass=siteLink))', ['name', 'distinguishedName']);
end;

function TGeneralPropertySiteLinkBridge.ExtractGroup(const S: RawUtf8): RawUtf8;
var
  Pairs: TNameValueDNs;
begin
  Result := '';
  ParseDN(S, Pairs);

  if Length(Pairs) > 1 then
    Result := FormatUtf8('CN=%', [Pairs[1].Value]);
end;

procedure TGeneralPropertySiteLinkBridge.SyncAttributeProperty(Option: TLdapAddOption);
var
  i: Integer;
  DN: RawUtf8;
begin
  if Length(InResult) = 0 then
  begin
    Props.Add('siteLinkList', '', aoReplaceValue);
    Exit;
  end;

  DN := InResult[0].Find('distinguishedName').GetReadable();
  Props.Add('siteLinkList', DN, aoReplaceValue);

  for i := 1 to High(InResult) do
  begin
    DN := InResult[i].Find('distinguishedName').GetReadable();
    Props.Add('siteLinkList', DN, aoNoDuplicateValue);
  end;
end;

procedure TGeneralPropertySiteLinkBridge.SetScalarProperty(
  const Attribute, Value: RawUtf8; Option: TLdapAddOption);
begin
  Props.Add(Attribute, Value, Option);
end;

function TGeneralPropertySiteLinkBridge.FindAttribute(Attribute: RawUtf8): TLdapAttribute;
begin
  if Assigned(fProperty) then
    Result := fProperty.Attributes.Find(Attribute)
  else
    Result := nil;
end;

function TGeneralPropertySiteLinkBridge.FindAttribute(
  Attribute: RawUtf8; LdapResult: TLdapResult): TLdapAttribute;
begin
  Result := LdapResult.Attributes.Find(Attribute);
end;

end.

