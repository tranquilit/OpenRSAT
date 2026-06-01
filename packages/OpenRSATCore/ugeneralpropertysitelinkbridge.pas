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
  TLdapResultArray = array of TLdapResult;

  TGeneralPropertySiteLinkBridge = class(TDoubleListLogic)
  private
    function SearchSitesInLdap: boolean;
    function ExtractGroup(const S: RawUtf8): RawUtf8;
  public
    constructor Create(P: TProperty);
    procedure GetAllResources; override;
    procedure SyncAttributeProperty(Option: TLdapAddOption); override;
  end;

implementation

constructor TGeneralPropertySiteLinkBridge.Create(P: TProperty);
begin
  Props := P;
  Ldap := P.LdapClient;
end;

procedure TGeneralPropertySiteLinkBridge.GetAllResources;
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

function TGeneralPropertySiteLinkBridge.SearchSitesInLdap: boolean;
var
  Group: RawUtf8;
begin
  Group := ExtractGroup(Props.distinguishedName);
  Result := Ldap.Search(FormatUtf8('%,CN=Inter-Site Transports,CN=Sites,%', [Group, Ldap.ConfigDN]), false, '(&(objectClass=siteLink))', ['name', 'distinguishedName']);
end;

function TGeneralPropertySiteLinkBridge.ExtractGroup(const S: RawUtf8): RawUtf8;
var
  p1, p2: Integer;
begin
  Result := '';

  p1 := Pos('CN=', S);
  if p1 = 0 then
    Exit;

  p1 := PosEx('CN=', S, p1 + 3);
  if p1 = 0 then
    Exit;

  p2 := PosEx(',', S, p1);
  if p2 = 0 then
    Result := Copy(S, p1, Length(S) - p1 + 1)
  else
    Result := Copy(S, p1, p2 - p1);
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

end.

