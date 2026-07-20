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

  { TGeneralPropertySiteLink }

  TGeneralPropertySiteLink = class(TDoubleListLogic)
  private
    fProperty: TProperty;
    fLdap: TLdapClient;

    function SearchSitesInLdap: boolean;
    function SaveHoursToSchedule(const Hours: RawByteString): RawByteString;
  public
    constructor Create(P: TProperty);

    procedure GetAllResources; override;
    procedure SyncAttributeProperty;
    procedure SetScalarProperty(const Attribute, Value: RawUtf8; Option: TLdapAddOption);
    procedure LoadScheduleToHours(const ScheduleData: RawByteString; var Hours: RawByteString);
    procedure SaveSchedule(const Hours: RawByteString);
    function FindAttribute(Attribute: RawUtf8): TLdapAttribute; virtual;
    function FindAttribute(Attribute: RawUtf8; LdapResult: TLdapResult): TLdapAttribute; virtual;
    function GetScheduleHeader: RawByteString;

    property Props: TProperty read fProperty write fProperty;
    property Ldap: TLdapClient read fLdap write fLdap;
  end;

const
  ScheduleHeader = #$BC#0#0#0#0#0#0#0#1#0#0#0#0#0#0#0#$14#0#0#0;

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

procedure TGeneralPropertySiteLink.LoadScheduleToHours(const ScheduleData: RawByteString; var Hours: RawByteString);
var
  Data: RawByteString;
  i, ADDay, ADHour, GridDay, GridIndex: Integer;
begin
  FillByte(Hours[1], Length(Hours), 0);
  if ScheduleData = '' then
  begin
    FillByte(Hours[1], Length(Hours), $0F);
    Exit;
  end;

  Data := ScheduleData;
  Delete(Data, 1, 20);
  for i := 0 to 167 do
  begin
    ADDay  := i div 24;
    ADHour := i mod 24;

    GridDay := (ADDay + 6) mod 7;
    GridIndex := GridDay * 24 + ADHour;
    if Byte(Data[i + 1]) = $FF then
      Hours[1 + GridIndex div 8] := Char(Byte(Hours[1 + GridIndex div 8]) or (1 shl (GridIndex mod 8)));
  end;
end;

procedure TGeneralPropertySiteLink.SaveSchedule(const Hours: RawByteString);
var
  Schedule: RawByteString;
begin
  Schedule := SaveHoursToSchedule(Hours);
  Props.Add('schedule', ScheduleHeader + Schedule);
end;

function TGeneralPropertySiteLink.FindAttribute(Attribute: RawUtf8): TLdapAttribute;
begin
  Result := fProperty.Attributes.Find(Attribute);
end;

function TGeneralPropertySiteLink.FindAttribute(Attribute: RawUtf8; LdapResult: TLdapResult): TLdapAttribute;
begin
  Result := LdapResult.Attributes.Find(Attribute);
end;

function TGeneralPropertySiteLink.GetScheduleHeader: RawByteString;
begin
  Result := ScheduleHeader;
end;

function TGeneralPropertySiteLink.SaveHoursToSchedule(const Hours: RawByteString): RawByteString;
var
  i, ADDay, ADHour, GridDay, GridIndex: Integer;
begin
  SetLength(Result, 168);

  for i := 0 to 167 do
  begin
    ADDay  := i div 24;
    ADHour := i mod 24;

    GridDay := (ADDay + 6) mod 7;
    GridIndex := GridDay * 24 + ADHour;
    if (Byte(Hours[1 + GridIndex div 8]) and (1 shl (GridIndex mod 8))) <> 0 then
      Result[i + 1] := Char($FF)
    else
      Result[i + 1] := Char($00);
  end;
end;

end.

