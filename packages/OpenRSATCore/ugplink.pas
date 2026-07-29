unit ugplink;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.base,
  mormot.core.text,
  mormot.core.variants,
  mormot.net.ldap;

type
  TGPLink = record
    Link: RawUtf8;
    DistinguishedName: RawUtf8;
    Flag: Integer;
  end;

  TGPLinkDynArr = Array of TGPLink;

  { TGPLinkList }

  TGPLinkList = class
  private
    fGPLinks: TGPLinkDynArr;
    fLdapClient: TLdapClient;

    function GetData: TDocVariantData;
    function GetGPLink: RawUtf8;
    procedure SetGPLink(AValue: RawUtf8);
    procedure ChangeFlag(const Elements: TRawUtf8DynArray; Flag: Integer);
  public
    function IndexOf(const DistinguishedName: RawUtf8): Integer;
    function Exists(const DistinguishedName: RawUtf8): Boolean;
    procedure Disable(const Elements: TRawUtf8DynArray);
    procedure Enable(const Elements: TRawUtf8DynArray);
    procedure Enforce(const Elements: TRawUtf8DynArray);

    procedure Add(const Elements: TRawUtf8DynArray);
    procedure Del(const Elements: TRawUtf8DynArray);

    property GPLink: RawUtf8 read GetGPLink write SetGPLink;
    property LdapClient: TLdapClient read fLdapClient write fLdapClient;
    property Data: TDocVariantData read GetData;
  end;

operator=(Destination, Source: TGPLink): Boolean;

function GPLinkToGPLinkArr(GPLink: RawUtf8): TGPLinkDynArr;
function GPLinkArrToGPLink(GPLinkArr: TGPLinkDynArr): RawUtf8;

operator in(Element: TGPLink; Elements: TGPLinkDynArr): Boolean;

implementation

operator=(Destination, Source: TGPLink): Boolean;
begin
  result := (Destination.Link = Source.Link);
end;

function GPLinkToGPLinkArr(GPLink: RawUtf8): TGPLinkDynArr;
var
  LinkStart, LinkEnd, Count: Integer;
  Link: TGPLink;
  LinkArr: TStringArray;
begin
  result := nil;
  LinkStart := 0;
  LinkEnd := 0;
  Count := 0;

  while GPLink <> '' do
  begin
    LinkStart := String(GPLink).IndexOf('[LDAP://', LinkEnd);
    if LinkStart < 0 then
      break;
    LinkEnd := String(GPLink).IndexOf(']', LinkStart);

    Link.Link := String(GPLink).Substring(LinkStart + 8, LinkEnd - 8 - LinkStart);
    LinkArr := String(Link.Link).Split(';');
    if Length(LinkArr) <> 2 then
      raise Exception.Create('Wrong GPLink format');
    Link.DistinguishedName := LinkArr[0];
    if not TryStrToInt(LinkArr[1], Link.Flag) then
      raise Exception.Create('GPLink flag is not a number');

    Insert(Link, result, Count);
    Inc(Count);
  end;
end;

function GPLinkArrToGPLink(GPLinkArr: TGPLinkDynArr): RawUtf8;
var
  Link: TGPLink;
begin
  result := '';
  for Link in GPLinkArr do
    result := FormatUtf8('%[LDAP://%;%]', [result, Link.DistinguishedName, Link.Flag]);
end;

operator in(Element: TGPLink; Elements: TGPLinkDynArr): Boolean;
var
  i: Integer;
begin
  result := True;
  for i := 0 to High(Elements) do
    if Elements[i] = Element then
      Exit;
  result := False;
end;

{ TGPLinkList }

function TGPLinkList.GetGPLink: RawUtf8;
begin
  result := GPLinkArrToGPLink(fGPLinks);
end;

function TGPLinkList.GetData: TDocVariantData;
var
  Filter: RawUtf8;
  Link: TGPLink;
  Row, GP: TDocVariantData;
  P: PDocVariantData;
begin
  result.InitArray([], JSON_FAST);

  if Assigned(LdapClient) then
  begin
    Filter := '';
    for Link in fGPLinks do
      Filter := FormatUtf8('%(distinguishedName=%)', [Filter, Link.DistinguishedName]);
    if Filter = '' then
      Exit;
    Filter := FormatUtf8('(|%)', [Filter]);

    LdapClient.SearchScope := lssWholeSubtree;
    if not LdapClient.SearchAllDocRaw(GP, LdapClient.DefaultDN, Filter, ['distinguishedName', 'displayName', 'flags', 'name'], [roAutoRange, roKnownValuesAsArray, roObjectNameAtRoot]) then
      Exit;
  end;

  for Link in fGPLinks do
  begin
    Row.Init(JSON_FAST);
    Row.AddValue('distinguishedName', Link.DistinguishedName);
    Row.AddValue('link_flags', Link.Flag);
    if GP.Exists(Link.DistinguishedName) then
    begin
      P := GP.O[Link.DistinguishedName];
      if not Assigned(P) then
        Continue;
      Row.AddValue('name', P^.U['displayName']);
      Row.AddValue('gpo_flags', P^.U['flags']);
      Row.AddValue('id', P^.U['name']);
    end;
    result.AddItem(Row);
    Row.Clear;
  end;
end;

procedure TGPLinkList.SetGPLink(AValue: RawUtf8);
begin
  fGPLinks := GPLinkToGPLinkArr(AValue);
end;

procedure TGPLinkList.ChangeFlag(const Elements: TRawUtf8DynArray; Flag: Integer
  );
var
  Index, i: Integer;
begin
  for i := 0 to High(Elements) do
  begin
    Index := IndexOf(Elements[i]);

    fGPLinks[Index].Flag := Flag;
  end;
end;

function TGPLinkList.IndexOf(const DistinguishedName: RawUtf8): Integer;
var
  i: Integer;
begin
  result := -1;
  for i := 0 to High(fGPLinks) do
    if fGPLinks[i].DistinguishedName = DistinguishedName then
    begin
      result := i;
      Exit;
    end;
end;

function TGPLinkList.Exists(const DistinguishedName: RawUtf8): Boolean;
begin
  result := IndexOf(DistinguishedName) >= 0;
end;

procedure TGPLinkList.Disable(const Elements: TRawUtf8DynArray);
begin
  ChangeFlag(Elements, 1);
end;

procedure TGPLinkList.Enable(const Elements: TRawUtf8DynArray);
begin
  ChangeFlag(Elements, 0);
end;

procedure TGPLinkList.Enforce(const Elements: TRawUtf8DynArray);
begin
  ChangeFlag(Elements, 2);
end;

procedure TGPLinkList.Add(const Elements: TRawUtf8DynArray);
var
  i: Integer;
  c: SizeInt;
begin
  for i := 0 to High(Elements) do
    if not Exists(Elements[i]) then
    begin
      c := Length(fGPLinks);
      SetLength(fGPLinks, c + 1);
      fGPLinks[c].DistinguishedName := Elements[i];
      fGPLinks[c].Flag := 1;
    end;
end;

procedure TGPLinkList.Del(const Elements: TRawUtf8DynArray);
var
  i, Index: Integer;
begin
  for i := High(Elements) downto 0 do
  begin
    Index := IndexOf(Elements[i]);
    if Index >= 0 then
      Delete(fGPLinks, Index, 1);
  end;
end;

end.

