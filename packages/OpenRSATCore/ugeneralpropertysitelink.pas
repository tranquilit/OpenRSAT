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
  uschedulinglogic,
  ursatldapclient;

type

  { TGeneralPropertySiteLink }

  TGeneralPropertySiteLink = class(TDoubleListLogic)
  private
    fProperty: TProperty;
    fLdap: TLdapClient;
    fScheduling: TSchedulingLogic;

    function SearchSitesInLdap: boolean;
  public
    constructor Create(P: TProperty);
    destructor Destroy; override;

    procedure GetAllResources; override;
    procedure SyncAttributeProperty;
    procedure SetScalarProperty(const Attribute, Value: RawUtf8; Option: TLdapAddOption);
    procedure SaveSchedule();
    function FindAttribute(Attribute: RawUtf8): TLdapAttribute; virtual;
    function FindAttribute(Attribute: RawUtf8; LdapResult: TLdapResult): TLdapAttribute; virtual;

    property Props: TProperty read fProperty write fProperty;
    property Ldap: TLdapClient read fLdap write fLdap;
    property Scheduling: TSchedulingLogic read fScheduling write fScheduling;
  end;

implementation

constructor TGeneralPropertySiteLink.Create(P: TProperty);
begin
  fProperty := P;
  fLdap := P.LdapClient;

  fScheduling := TSchedulingLogic.Create(SiteLinkSchedulingPage);
  fScheduling.SetupHoursRawByteString;
end;

destructor TGeneralPropertySiteLink.Destroy;
begin
  Inherited Destroy;
  fScheduling.Free;
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

procedure TGeneralPropertySiteLink.SyncAttributeProperty;
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

procedure TGeneralPropertySiteLink.SaveSchedule;
var
  Header: RawByteString;
  Schedule: RawByteString;
  Value: UInt32;
begin
  Schedule := fScheduling.SaveSchedule();

  SetLength(Header, 20);
  Value := Length(Header) + Length(Schedule);
  Move(Value, Header[1], SizeOf(UInt32));
  Value := 0;
  Move(Value, Header[5], SizeOf(UInt32));
  Value := 1;
  Move(Value, Header[9], SizeOf(UInt32));
  Value := 0;
  Move(Value, Header[13], SizeOf(UInt32));
  Value := Length(Header);
  Move(Value, Header[17], SizeOf(UInt32));

  Props.Add('schedule', Header + Schedule);
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

