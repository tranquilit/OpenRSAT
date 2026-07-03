unit ugeneralpropertyntdsdsa;

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

  { TQueryPoliciesList }
  TQueryPoliciesList = array of TLdapResult;

  { TGeneralPropertyNTDSDSA }
  TGeneralPropertyNTDSDSA = class
  private
    fProperty: TProperty;
    fLdap: TLdapClient;
    fQueryPolicies: TQueryPoliciesList;

    procedure AddItemToQueryList(Item: TLdapResult);
  public
    constructor Create(P: TProperty);

    procedure GetAllQueryPolicies;
    procedure SyncAttributeProperty;
    procedure SetScalarProperty(const Attribute, Value: RawUtf8; Option: TLdapAddOption);
    function GetPolicyDistinguishedName: RawUtf8;
    function GetAttributeName(Attr: TLdapAttributeList): RawUtf8;
    function GetDNbyName(Name: RawUtf8): RawUtf8;
    function GetNamebyDN(DN: RawUtf8): RawUtf8;

    property Props: TProperty read fProperty write fProperty;
    property Ldap: TLdapClient read fLdap write fLdap;
    property QueryPolicies: TQueryPoliciesList read fQueryPolicies write fQueryPolicies;
  end;

implementation

procedure TGeneralPropertyNTDSDSA.AddItemToQueryList(Item: TLdapResult);
var
  ListLen: Integer;
begin
  if not Assigned(Item) then
    exit;

  ListLen := Length(fQueryPolicies);
  SetLength(fQueryPolicies, ListLen + 1);
  fQueryPolicies[ListLen] := TLdapResult(Item.Clone);
end;

constructor TGeneralPropertyNTDSDSA.Create(P: TProperty);
begin
  fProperty := P;
  fLdap := P.LdapClient;
end;

procedure TGeneralPropertyNTDSDSA.GetAllQueryPolicies;
var
  LdapResult: TLdapResult;
begin
  fLdap.SearchBegin();
  try
    fLdap.SearchScope := lssSingleLevel;
    repeat
      if not Ldap.Search(FormatUtf8('CN=Query-Policies,CN=Directory Service,CN=Windows NT,CN=Services,%',[Ldap.ConfigDN]),
        false, '(&(objectClass=queryPolicy))', ['name', 'distinguishedName']) then
          Exit;

      for LdapResult in Ldap.SearchResult.Items do
        AddItemToQueryList(LdapResult);
    until fLdap.SearchCookie = '';
  finally
    fLdap.SearchEnd;
  end;
end;

procedure TGeneralPropertyNTDSDSA.SyncAttributeProperty;
//var
  //i: Integer;
  //DN: RawUtf8;
begin
  //if Length(InResult) = 0 then
  //begin
  //  Props.Add('bridgeheadTransportList', '', aoReplaceValue);
  //  Exit;
  //end;
  //
  //DN := InResult[0].Find('distinguishedName').GetReadable();
  //Props.Add('bridgeheadTransportList', DN, aoReplaceValue);
  //
  //for i := 1 to High(InResult) do
  //begin
  //  DN := InResult[i].Find('distinguishedName').GetReadable();
  //  Props.Add('bridgeheadTransportList', DN, aoNoDuplicateValue);
  //end;
end;

procedure TGeneralPropertyNTDSDSA.SetScalarProperty(const Attribute, Value: RawUtf8; Option: TLdapAddOption);
begin
  Props.Add(Attribute, Value, Option);
end;

function TGeneralPropertyNTDSDSA.GetPolicyDistinguishedName: RawUtf8;
begin
  Result := fProperty.Attributes.Find('queryPolicyObject').GetReadable();
end;

function TGeneralPropertyNTDSDSA.GetAttributeName(Attr: TLdapAttributeList): RawUtf8;
begin
  Result := Attr.Find('name').GetReadable();
end;

function TGeneralPropertyNTDSDSA.GetDNbyName(Name: RawUtf8): RawUtf8;
var
  Value: TLdapResult;
begin
  for Value in fQueryPolicies do
  begin
    if Value.Attributes.Find('name').GetReadable() = Name then
    begin
        Result := Value.Attributes.Find('distinguishedName').GetReadable();
    end;
  end;
end;

function TGeneralPropertyNTDSDSA.GetNamebyDN(DN: RawUtf8): RawUtf8;
var
  Value: TLdapResult;
begin
  for Value in fQueryPolicies do
  begin
    if Value.Attributes.Find('distinguishedName').GetReadable() = DN then
    begin
        Result := Value.Attributes.Find('name').GetReadable();
    end;
  end;
end;

end.

