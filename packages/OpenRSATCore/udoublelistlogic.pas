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
    fOutResult, fInResult: TLdapResultArray;

    procedure RemoveFromArray(var List: TLdapResultArray; Index: Integer);
  protected
    procedure AddToList(Item: TLdapResult); virtual;
    procedure AddToList(var List: TLdapResultArray; Item: TLdapResult); virtual;
  public
    destructor Destroy; override;

    procedure GetAllResources; virtual; abstract;
    procedure MoveItem(State: TMovingState; Index: Integer); virtual;

    function GetItemAttrValue(List: TLdapResultArray; idx: Integer; attr: RawUtf8): RawUtf8; virtual;
    function GetResultName(Obj: TLdapResult): RawUtf8; virtual;
    function GetNbElementsInLists: Integer; virtual;
    function GetValueFromAttribute(Attribute: TLdapAttribute): RawUtf8; virtual;
    function FindAttribute(Attribute: RawUtf8): TLdapAttribute; virtual; abstract;
    function FindAttribute(Attribute: RawUtf8; LdapResult: TLdapResult): TLdapAttribute; virtual; abstract;

    property InResult: TLdapResultArray read fInResult write fInResult;
    property OutResult: TLdapResultArray read fOutResult write fOutResult;
  end;

implementation

destructor TDoubleListLogic.Destroy;
var
  i: Integer;
begin
  for i := 0 to High(fInResult) do
    FreeAndNil(fInResult[i]);
  SetLength(fInResult, 0);

  for i := 0 to High(fOutResult) do
    FreeAndNil(fOutResult[i]);
  SetLength(fOutResult, 0);

  inherited Destroy;
end;

procedure TDoubleListLogic.RemoveFromArray(var List: TLdapResultArray; Index: Integer);
var
  i, Last: Integer;
begin
  Last := High(List);
  if (Index < 0) or (Index > Last) then
    Exit;

  for i := Index to Last - 1 do
    List[i] := List[i + 1];

  FreeAndNil(List[Last]);
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

end.

