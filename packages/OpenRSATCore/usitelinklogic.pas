unit usitelinklogic;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.base,
  mormot.core.text,
  mormot.net.ldap,
  uproperty,
  uintfsitelinklogic,
  ursatldapclient;

type  
  TLdapResultArray = array of TLdapResult;

  TSiteLinkLogic = class abstract(TInterfacedObject, ISiteLinkLogic)
  private
    fProperty: TProperty;
    fLdap: TRsatLdapClient;
    fInSite, fNotInSite: TLdapResultArray;

    procedure RemoveFromArray(var List: TLdapResultArray; Index: Integer);
  protected
    procedure AddToNotInSite(Item: TLdapResult); virtual;
    procedure AddToInSite(Item: TLdapResult); virtual;
  public
    procedure GetAllResources; virtual; abstract;
    procedure MoveItemToInSite(Index: Integer); virtual;
    procedure MoveItemToNotInSite(Index: Integer); virtual;
    procedure SetScalarProperty(const Attribute, Value: RawUtf8; Option: TLdapAddOption); virtual;
    procedure SyncAttributeProperty(Option: TLdapAddOption); virtual; abstract;
    procedure AddItemInResultArray(var List: TLdapResultArray; Item: TLdapResult); virtual;
    procedure SetProperties(P: TProperty); virtual;
    procedure SetLdap(L: TRsatLdapClient); virtual;

    function GetProps: TProperty; virtual;
    function GetLdap: TRsatLdapClient; virtual;
    function GetInSite: TLdapResultArray; virtual;
    function GetNotInSite: TLdapResultArray; virtual;
    function GetItemAttrValue(List: TLdapResultArray; idx: Integer; attr: RawUtf8): RawUtf8; virtual;
    function GetResultName(Obj: TLdapResult): RawUtf8; virtual;
    function GetNbSites: Integer; virtual;
    function GetValueFromAttribute(Attribute: TLdapAttribute): RawUtf8; virtual;
    function FindAttribute(Attribute: RawUtf8): TLdapAttribute; virtual;
    function FindAttribute(Attribute: RawUtf8; LdapResult: TLdapResult): TLdapAttribute; virtual;

    property Props: TProperty read fProperty write fProperty;
    property Ldap: TRsatLdapClient read fLdap write fLdap; 
    property InSite: TLdapResultArray read fInSite write fInSite;
    property NotInSite: TLdapResultArray read fNotInSite write fNotInSite;
  end;

implementation

procedure TSiteLinkLogic.RemoveFromArray(var List: TLdapResultArray; Index: Integer);
var
  i: Integer;
begin
  for i := Index to High(List) - 1 do
    List[i] := List[i + 1];
  SetLength(List, Length(List) - 1);
end;

procedure TSiteLinkLogic.AddItemInResultArray(var List: TLdapResultArray; Item: TLdapResult);
var
  ListLen: Integer;
begin
  if not Assigned(Item) then
    exit;

  ListLen := Length(List);
  SetLength(List, ListLen + 1);
  List[ListLen] := TLdapResult(Item.Clone);
end;

procedure TSiteLinkLogic.AddToNotInSite(Item: TLdapResult);
begin
  AddItemInResultArray(fNotInSite, Item);
end;

procedure TSiteLinkLogic.AddToInSite(Item: TLdapResult);
begin
  AddItemInResultArray(fInSite, Item);
end;

procedure TSiteLinkLogic.MoveItemToInSite(Index: Integer);
begin
  AddToInSite(fNotInSite[Index]);
  RemoveFromArray(fNotInSite, Index);
end;

procedure TSiteLinkLogic.MoveItemToNotInSite(Index: Integer);
begin
  AddToNotInSite(fInSite[Index]);
  RemoveFromArray(fInSite, Index);
end;

procedure TSiteLinkLogic.SetScalarProperty(const Attribute, Value: RawUtf8; Option: TLdapAddOption);
begin
  fProperty.Add(Attribute, Value, Option);
end;

procedure TSiteLinkLogic.SetProperties(P: TProperty);
begin
  fProperty := P;
end;

procedure TSiteLinkLogic.SetLdap(L: TRsatLdapClient);
begin
  fLdap := L;
end;

function TSiteLinkLogic.GetProps: TProperty;
begin
  Result := fProperty;
end;

function TSiteLinkLogic.GetLdap: TRsatLdapClient;
begin
  Result := fLdap;
end;

function TSiteLinkLogic.GetInSite: TLdapResultArray;
begin
  Result := fInSite;
end;

function TSiteLinkLogic.GetNotInSite: TLdapResultArray;
begin
  Result := fNotInSite;
end;

function TSiteLinkLogic.GetItemAttrValue(List: TLdapResultArray; idx: Integer; attr: RawUtf8): RawUtf8;
begin
  Result := List[idx].Find(attr).GetReadable();
end;

function TSiteLinkLogic.GetResultName(Obj: TLdapResult): RawUtf8;
begin
  Result := Obj.Find('name').GetReadable();
end;

function TSiteLinkLogic.GetNbSites: Integer;
begin
  Result := Length(fInSite) + Length(fNotInSite);
end;

function TSiteLinkLogic.GetValueFromAttribute(Attribute: TLdapAttribute): RawUtf8;
begin
  if Attribute <> nil then
    Result := Attribute.GetReadable()
  else
    Result := '';
end;

function TSiteLinkLogic.FindAttribute(Attribute: RawUtf8): TLdapAttribute;
begin
  Result := fProperty.Attributes.Find(Attribute);
end;

function TSiteLinkLogic.FindAttribute(Attribute: RawUtf8; LdapResult: TLdapResult): TLdapAttribute;
begin
  Result := LdapResult.Attributes.Find(Attribute);
end;

end.

