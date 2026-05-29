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
  ursatldapclient;

type
  
  { Ldap Result }
  TLdapResultArray = array of TLdapResult;
  
  { TGeneralPropertySiteLink }
  TGeneralPropertySiteLink = class
  private
    fProperty: TProperty;
    fLdap: TRsatLdapClient;
    fInSite, fNotInSite: TLdapResultArray;
    
    procedure RemoveItemFromArray(var List: TLdapResultArray; Index: Integer);
    procedure AddItemInResultArray(var List: TLdapResultArray; Item: TLdapResult);

    function SearchSitesInLdap: boolean;

  public
    constructor Create(Props: TProperty);

    procedure GetAllSites;
    procedure MoveItemToInSite(Index: Integer);
    procedure MoveItemToNotInSite(Index: Integer);
    procedure SetScalarProperty(const Attribute, Value: RawUtf8; Option: TLdapAddOption);
    procedure SyncSiteListProperty(Option: TLdapAddOption);

    function GetSitesInSiteLink: TLdapResultArray;
    function GetSitesNotInSiteLink: TLdapResultArray;
    function GetResultName(Obj: TLdapResult): RawUtf8;
    function GetSiteAttrValue(List: TLdapResultArray; idx: Integer; attr: RawUtf8): RawUtf8;
    function GetNbSites: Integer;
    function GetValueFromAttribute(Attribute: TLdapAttribute): RawUtf8; virtual;
    function FindAttribute(Attribute: RawUtf8): TLdapAttribute; virtual;
    function FindAttribute(Attribute: RawUtf8; LdapResult: TLdapResult): TLdapAttribute; virtual;

    property Ldap: TRsatLdapClient read fLdap;
  end;

implementation

constructor TGeneralPropertySiteLink.Create(Props: TProperty);
begin
  fProperty := Props;
  fLdap := fProperty.LdapClient;
end;

{ PRIVATE }
procedure TGeneralPropertySiteLink.RemoveItemFromArray(var List: TLdapResultArray; Index: Integer);
var
  i: Integer;
begin
  for i := Index to High(List) - 1 do
    List[i] := List[i + 1];

  SetLength(List, Length(List) - 1);
end;

procedure TGeneralPropertySiteLink.AddItemInResultArray(var List: TLdapResultArray; Item: TLdapResult);
var
  ListLen: Integer;
begin
  if not Assigned(Item) then
    exit;
  
  ListLen := Length(List);
  SetLength(List, ListLen + 1);
  List[ListLen] := TLdapResult(Item.Clone);
end;

function TGeneralPropertySiteLink.SearchSitesInLdap: boolean;
begin
  Result := fLdap.Search(FormatUtf8('CN=Sites,%', [fLdap.ConfigDN]), false, '(&(objectClass=site))', ['name', 'distinguishedName'])
end;


{ PUBLIC }
procedure TGeneralPropertySiteLink.MoveItemToInSite(Index: Integer);
begin
  SetLength(fInSite, Length(fInSite) + 1);
  fInSite[High(fInSite)] := fNotInSite[Index];
  RemoveItemFromArray(fNotInSite, Index);
end;

procedure TGeneralPropertySiteLink.MoveItemToNotInSite(Index: Integer);
begin
  SetLength(fNotInSite, Length(fNotInSite) + 1);
  fNotInSite[High(fNotInSite)] := fInSite[Index];
  RemoveItemFromArray(fInSite, Index);
end;

procedure TGeneralPropertySiteLink.GetAllSites;
var
  LdapResult: TLdapResult;
begin
  fLdap.SearchBegin();
  fLdap.SearchScope := lssSingleLevel;
  try
     repeat
     if not SearchSitesInLdap then
       Exit;
    
     for LdapResult in fLdap.SearchResult.Items do
       AddItemInResultArray(fNotInSite, LdapResult);
     until fLdap.SearchCookie = '';
  finally
    fLdap.SearchEnd;
  end;
end;

procedure TGeneralPropertySiteLink.SetScalarProperty(const Attribute, Value: RawUtf8; Option: TLdapAddOption);
begin
  fProperty.Add(Attribute, Value, Option);
end;

procedure TGeneralPropertySiteLink.SyncSiteListProperty(Option: TLdapAddOption);
var
  i: Integer;
  DN: RawUtf8;
begin
  if Length(fInSite) = 0 then
  begin
    fProperty.Add('siteList', '', aoReplaceValue);
    Exit;
  end;

  DN := fInSite[0].Find('distinguishedName').GetReadable();
  fProperty.Add('siteList', DN, aoReplaceValue);

  for i := 1 to High(fInSite) do
  begin
    DN := fInSite[i].Find('distinguishedName').GetReadable();
    fProperty.Add('siteList', DN, aoNoDuplicateValue);
  end;
end;

function TGeneralPropertySiteLink.GetSitesInSiteLink: TLdapResultArray;
begin
  Result := fInSite;
end;

function TGeneralPropertySiteLink.GetSitesNotInSiteLink: TLdapResultArray;
begin
  Result := fNotInSite;
end;

function TGeneralPropertySiteLink.GetResultName(Obj: TLdapResult): RawUtf8;
begin
  Result := Obj.Find('name').GetReadable();
end;

function TGeneralPropertySiteLink.GetSiteAttrValue(List: TLdapResultArray; idx: Integer; attr: RawUtf8): RawUtf8;
begin
  Result := List[idx].Find(attr).GetReadable()
end;

function TGeneralPropertySiteLink.GetNbSites: Integer;
begin
  Result := Length(fInSite) + Length(fNotInSite);
end;

function TGeneralPropertySiteLink.GetValueFromAttribute(Attribute: TLdapAttribute): RawUtf8;
begin
  if Attribute <> nil then
    Result := Attribute.GetReadable()
  else
    Result := '';
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

