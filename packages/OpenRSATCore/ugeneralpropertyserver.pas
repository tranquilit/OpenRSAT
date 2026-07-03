unit ugeneralpropertyserver;

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
  TGeneralPropertyServer = class(TDoubleListLogic)
  private
    fProperty: TProperty;
    fLdap: TLdapClient;

    function SearchTransportsInLdap: boolean;
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

constructor TGeneralPropertyServer.Create(P: TProperty);
begin
  fProperty := P;
  fLdap := P.LdapClient;
end;

procedure TGeneralPropertyServer.GetAllResources;
var
  LdapResult: TLdapResult;
begin
  Ldap.SearchBegin();
  try
    Ldap.SearchScope := lssSingleLevel;
    repeat
      if not SearchTransportsInLdap then
        Exit;

      for LdapResult in Ldap.SearchResult.Items do
        AddToList(LdapResult);
    until Ldap.SearchCookie = '';
  finally
    Ldap.SearchEnd;
  end;
end;

function TGeneralPropertyServer.SearchTransportsInLdap: boolean;
begin
  Result := Ldap.Search(FormatUtf8('CN=Inter-Site Transports,CN=Sites,%', [Ldap.ConfigDN]), false, '(&(objectClass=interSiteTransport))', ['name', 'distinguishedName']);
end;

procedure TGeneralPropertyServer.SyncAttributeProperty(Option: TLdapAddOption);
var
  i: Integer;
  DN: RawUtf8;
begin
  if Length(InResult) = 0 then
  begin
    Props.Add('bridgeheadTransportList', '', aoReplaceValue);
    Exit;
  end;

  DN := InResult[0].Find('distinguishedName').GetReadable();
  Props.Add('bridgeheadTransportList', DN, aoReplaceValue);

  for i := 1 to High(InResult) do
  begin
    DN := InResult[i].Find('distinguishedName').GetReadable();
    Props.Add('bridgeheadTransportList', DN, aoNoDuplicateValue);
  end;
end;

procedure TGeneralPropertyServer.SetScalarProperty(const Attribute, Value: RawUtf8; Option: TLdapAddOption);
begin
  Props.Add(Attribute, Value, Option);
end;

function TGeneralPropertyServer.FindAttribute(Attribute: RawUtf8): TLdapAttribute;
begin
  Result := fProperty.Attributes.Find(Attribute);
end;

function TGeneralPropertyServer.FindAttribute(Attribute: RawUtf8; LdapResult: TLdapResult): TLdapAttribute;
begin
  Result := LdapResult.Attributes.Find(Attribute);
end;

end.

