unit udoublelistlogic;

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
  { TMovingState }
  TMovingState = (msOutOfResult, msInResult);

  { TLdapResultArray }
  TLdapResultArray = array of TLdapResult;

  { ISiteLinkLogic }
  IDoubleListLogic = interface
    procedure GetAllResources;
    procedure MoveItem(State: TMovingState; Index: Integer);
    procedure SetScalarProperty(const Attribute, Value: RawUtf8; Option: TLdapAddOption);
    procedure SyncAttributeProperty(Option: TLdapAddOption);
    procedure AddToList(Item: TLdapResult);
    procedure AddToList(var List: TLdapResultArray; Item: TLdapResult);
    function GetItemAttrValue(List: TLdapResultArray; idx: Integer; attr: RawUtf8): RawUtf8;
    function GetResultName(Obj: TLdapResult): RawUtf8;
    function GetNbElementsInLists: Integer;
    function GetValueFromAttribute(Attribute: TLdapAttribute): RawUtf8;
    function FindAttribute(Attribute: RawUtf8): TLdapAttribute;
    function FindAttribute(Attribute: RawUtf8; LdapResult: TLdapResult): TLdapAttribute;
  end;
  
  { TDoubleListLogic }
  TDoubleListLogic = class abstract(TInterfacedObject, IDoubleListLogic)
  private
    fProperty: TProperty;
    fLdap: TRsatLdapClient;
    fOutResult, fInResult: TLdapResultArray;

    procedure RemoveFromArray(var List: TLdapResultArray; Index: Integer);
  protected
    procedure AddToList(Item: TLdapResult); virtual;
    procedure AddToList(var List: TLdapResultArray; Item: TLdapResult); virtual;
  public
    procedure GetAllResources; virtual; abstract;
    procedure MoveItem(State: TMovingState; Index: Integer); virtual;
    procedure SetScalarProperty(const Attribute, Value: RawUtf8; Option: TLdapAddOption); virtual;
    procedure SyncAttributeProperty(Option: TLdapAddOption); virtual; abstract;

    function GetItemAttrValue(List: TLdapResultArray; idx: Integer; attr: RawUtf8): RawUtf8; virtual;
    function GetResultName(Obj: TLdapResult): RawUtf8; virtual;
    function GetNbElementsInLists: Integer; virtual;
    function GetValueFromAttribute(Attribute: TLdapAttribute): RawUtf8; virtual;
    function FindAttribute(Attribute: RawUtf8): TLdapAttribute; virtual;
    function FindAttribute(Attribute: RawUtf8; LdapResult: TLdapResult): TLdapAttribute; virtual;

    property Props: TProperty read fProperty write fProperty;
    property Ldap: TRsatLdapClient read fLdap write fLdap; 
    property InResult: TLdapResultArray read fInResult write fInResult;
    property OutResult: TLdapResultArray read fOutResult write fOutResult;
  end;

implementation

procedure TDoubleListLogic.RemoveFromArray(var List: TLdapResultArray; Index: Integer);
var
  i: Integer;
begin
  for i := Index to High(List) - 1 do
    List[i] := List[i + 1];
  SetLength(List, Length(List) - 1);
end;

procedure TDoubleListLogic.AddToList(Item: TLdapResult);
var
  ListLen: Integer;
begin
  if not Assigned(Item) then
    exit;

  ListLen := Length(fOutResult);
  SetLength(fOutResult, ListLen + 1);
  fOutResult[ListLen] := TLdapResult(Item.Clone);
end;

procedure TDoubleListLogic.AddToList(var List: TLdapResultArray; Item: TLdapResult);
var
  ListLen: Integer;
begin
  if not Assigned(Item) then
    exit;

  ListLen := Length(List);
  SetLength(List, ListLen + 1);
  List[ListLen] := TLdapResult(Item.Clone);
end;

procedure TDoubleListLogic.MoveItem(State: TMovingState; Index: Integer);
begin
  if State = msInResult then
  begin
    AddToList(fInResult, fOutResult[Index]);
    RemoveFromArray(fOutResult, Index);
  end
  else
  begin
    AddToList(fOutResult, fInResult[Index]);
    RemoveFromArray(fInResult, Index);
  end;
end;

procedure TDoubleListLogic.SetScalarProperty(const Attribute, Value: RawUtf8; Option: TLdapAddOption);
begin
  fProperty.Add(Attribute, Value, Option);
end;

function TDoubleListLogic.GetItemAttrValue(List: TLdapResultArray; idx: Integer; attr: RawUtf8): RawUtf8;
begin
  Result := List[idx].Find(attr).GetReadable();
end;

function TDoubleListLogic.GetResultName(Obj: TLdapResult): RawUtf8;
begin
  Result := Obj.Find('name').GetReadable();
end;

function TDoubleListLogic.GetNbElementsInLists: Integer;
begin
  Result := Length(fInResult) + Length(fOutResult);
end;

function TDoubleListLogic.GetValueFromAttribute(Attribute: TLdapAttribute): RawUtf8;
begin
  if Attribute <> nil then
    Result := Attribute.GetReadable()
  else
    Result := '';
end;

function TDoubleListLogic.FindAttribute(Attribute: RawUtf8): TLdapAttribute;
begin
  Result := fProperty.Attributes.Find(Attribute);
end;

function TDoubleListLogic.FindAttribute(Attribute: RawUtf8; LdapResult: TLdapResult): TLdapAttribute;
begin
  Result := LdapResult.Attributes.Find(Attribute);
end;

end.

