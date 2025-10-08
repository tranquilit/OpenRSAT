unit uaductreeview;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  ComCtrls,
  mormot.core.base,
  mormot.core.variants,
  mormot.net.ldap;

type

  { TGPLinks }

  TGPLinks = class
  private
    fDistinguishedName: Array of String;
    fFlag: Array of Integer;
    fCount: Integer;
    procedure AddLink(ADistinguishedName: String; AFlag: Integer);
    function GetDistinguishedName(aValue: Integer): String;
    function GetFlag(aValue: Integer): Integer;
    procedure SetLinks(GPLink: String);
  public
    constructor Create(GPLink: String);
    property Count: Integer read fCount;
    property DistinguishedName[aValue: Integer]: String read GetDistinguishedName;
    property Flag[aValue: Integer]: Integer read GetFlag;
  end;

  TADUCTreeNodeType = (
    atntNone,
    atntQuery,
    atntObject
  );

  // Node data for query type

  { TADUCTreeNodeQuery }

  TADUCTreeNodeQuery = class
    Name: RawUtf8;
    Description: RawUtf8;
    // If BaseDN = '', it means it's a folder. Otherwise, it's a query definition.
    BaseDN: RawUtf8;
    IncludeSubContainers: Boolean;
    QueryString: RawUtf8;

    // Create a folder type node.
    constructor CreateFolder(AName, ADescription: RawUtf8); overload;

    // Create a query type node.
    // No BaseDN means it is a folder type node.
    constructor CreateQuery(AName, ADescription, ABaseDN, AQueryString: RawUtf8; AIncludeSubContainers: Boolean); overload;
  end;

  { TADUCTreeNodeObject }

  TADUCTreeNodeObject = class
  private
    fAttributes: TLdapAttributeList;
    fGPLinks: TGPLinks;

    function GetFirstAttribute(AttributeName: RawUtf8): RawUtf8;
    function GetLastAttribute(AttributeName: RawUtf8): RawUtf8;
    function GetAllAttribute(AttributeName: RawUtf8): TRawUtf8DynArray;
    function GetName: String;
    procedure SetAttribute(AttributeName: RawUtf8; AttributeValues: TRawUtf8DynArray);

    function GetDistinguishedName: String;
    function GetGPLink: String;
    function GetGPLinks: TGPLinks;
    function GetLastObjectClass: String;
    function GetObjectClass: TRawUtf8DynArray;
    procedure SetDistinguishedName(AValue: String);
    procedure SetGPLink(AValue: String);
    procedure SetName(AValue: String);
    procedure SetObjectClass(AValue: TRawUtf8DynArray);
  public
    constructor Create;
    destructor Destroy; override;

    function Find(AttributeName: String): TLdapAttribute;
    property DistinguishedName: String read GetDistinguishedName write SetDistinguishedName;
    property Name: String read GetName write SetName;
    property ObjectClass: TRawUtf8DynArray read GetObjectClass write SetObjectClass;
    property LastObjectClass: String read GetLastObjectClass;
    property GPLink: String read GetGPLink write SetGPLink;
    property GPLinks: TGPLinks read GetGPLinks;
  end;


  { TADUCTreeNode }

  TADUCTreeNode = class(TTreeNode)
  private
    fNodeType: TADUCTreeNodeType;

    procedure SetNodeType(AValue: TADUCTreeNodeType);
    procedure ClearData;
  public
    property NodeType: TADUCTreeNodeType read fNodeType write SetNodeType;
    function IsType(ANodeType: TADUCTreeNodeType): Boolean;
    function IsNoneType: Boolean;
    function IsObjectType: Boolean;
    function IsQueryType: Boolean;
    function GetNodeDataObject: TADUCTreeNodeObject;
    function GetNodeDataQuery: TADUCTreeNodeQuery;
  published
    destructor Destroy; override;
  end;


implementation
uses
  mormot.core.text;

{ TGPLinks }

procedure TGPLinks.AddLink(ADistinguishedName: String; AFlag: Integer);
begin
  Insert(ADistinguishedName, fDistinguishedName, fCount);
  Insert(AFlag, fFlag, fCount);
  Inc(fCount);
end;

function TGPLinks.GetDistinguishedName(aValue: Integer): String;
begin
  result := '';
  if (aValue < Low(fDistinguishedName)) or (aValue > High(fDistinguishedName)) then
    Exit;
  result := fDistinguishedName[aValue];
end;

function TGPLinks.GetFlag(aValue: Integer): Integer;
begin
  result := -1;
  if (aValue < Low(fFlag)) or (aValue > High(fFlag)) then
    Exit;
  result := fFlag[aValue];
end;

procedure TGPLinks.SetLinks(GPLink: String);
var
  Link, NewLink: String;
  LinkStart, LinkEnd: SizeInt;
  LinkArr: TStringArray;
begin
  LinkStart := 0;
  LinkEnd := 0;

  while GPLink <> '' do
  begin
    LinkStart := GPLink.IndexOf('[LDAP://', LinkEnd);
    if LinkStart < 0 then
      break;
    LinkEnd := GPLink.IndexOf(']', LinkStart);

    Link := GPLink.Substring(LinkStart + 8, LinkEnd - 8 - LinkStart);
    LinkArr := Link.Split(';');
    AddLink(LinkArr[0], StrToInt(LinkArr[1]));
  end;
end;

constructor TGPLinks.Create(GPLink: String);
begin
  fCount := 0;
  fDistinguishedName := [];
  fFlag := [];

  SetLinks(GPLink);
end;

{ TADUCTreeNodeQuery }

constructor TADUCTreeNodeQuery.CreateFolder(AName, ADescription: RawUtf8);
begin
  Name := AName;
  Description := ADescription;
  BaseDN := '';
  QueryString := '';
  IncludeSubContainers := False;
end;

constructor TADUCTreeNodeQuery.CreateQuery(AName, ADescription, ABaseDN,
  AQueryString: RawUtf8; AIncludeSubContainers: Boolean);
begin
  Name := AName;
  Description := ADescription;
  BaseDN := ABaseDN;
  QueryString := AQueryString;
  IncludeSubContainers := AIncludeSubContainers;
end;

{ TADUCTreeNodeObject }

function TADUCTreeNodeObject.GetFirstAttribute(AttributeName: RawUtf8): RawUtf8;
var
  Attribute: TLdapAttribute;
begin
  result := '';

  // Fast Exit
  if not Assigned(Self) then
    Exit;

  // Find attribute
  Attribute := fAttributes.Find(AttributeName);
  if Assigned(Attribute) then
    result := Attribute.GetReadable();
end;

function TADUCTreeNodeObject.GetLastAttribute(AttributeName: RawUtf8): RawUtf8;
var
  Attribute: TLdapAttribute;
begin
  result := '';

  // Fast Exit
  if not Assigned(Self) then
    Exit;

  // Find attribute
  Attribute := fAttributes.Find(AttributeName);
  if Assigned(Attribute) and (Attribute.Count > 0) then
    result := Attribute.GetReadable(Attribute.Count - 1);
end;

function TADUCTreeNodeObject.GetAllAttribute(AttributeName: RawUtf8
  ): TRawUtf8DynArray;
var
  Attribute: TLdapAttribute;
begin
  result := [];

  // Fast Exit
  if not Assigned(Self) then
    Exit;

  // Find Attribut
  Attribute := fAttributes.Find(AttributeName);
  if Assigned(Attribute) then
    result := Attribute.GetAllReadable;
end;

function TADUCTreeNodeObject.GetName: String;
begin
  result := GetFirstAttribute('name');
end;

procedure TADUCTreeNodeObject.SetAttribute(AttributeName: RawUtf8;
  AttributeValues: TRawUtf8DynArray);
var
  Attribute: TLdapAttribute;
  AValue: RawUtf8;
begin
  // Fast Exit
  if not Assigned(Self) then
    Exit;

  Attribute := fAttributes.Find(AttributeName);
  if not Assigned(Attribute) then
    Attribute := fAttributes.Add(AttributeName);
  Attribute.Clear;
  for AValue in AttributeValues do
    Attribute.Add(AValue);
end;

function TADUCTreeNodeObject.GetDistinguishedName: String;
begin
  result := GetFirstAttribute('distinguishedName');
end;

function TADUCTreeNodeObject.GetGPLink: String;
begin
  result := GetFirstAttribute('gPLink');
end;

function TADUCTreeNodeObject.GetGPLinks: TGPLinks;
begin
end;

function TADUCTreeNodeObject.GetLastObjectClass: String;
begin
  result := GetLastAttribute('objectClass')
end;

function TADUCTreeNodeObject.GetObjectClass: TRawUtf8DynArray;
begin
  result := GetAllAttribute('objectClass');
end;

procedure TADUCTreeNodeObject.SetDistinguishedName(AValue: String);
begin
  SetAttribute('distinguishedName', [AValue]);
end;

procedure TADUCTreeNodeObject.SetGPLink(AValue: String);
begin
  SetAttribute('gPLink', [AValue]);
end;

procedure TADUCTreeNodeObject.SetName(AValue: String);
begin
  SetAttribute('name', [AValue]);
end;

procedure TADUCTreeNodeObject.SetObjectClass(AValue: TRawUtf8DynArray);
begin
  SetAttribute('objectClass', AValue);
end;

constructor TADUCTreeNodeObject.Create;
begin
  fAttributes := TLdapAttributeList.Create;
end;

destructor TADUCTreeNodeObject.Destroy;
begin
  FreeAndNil(fAttributes);

  inherited Destroy;
end;

function TADUCTreeNodeObject.Find(AttributeName: String): TLdapAttribute;
begin
  result := fAttributes.Find(AttributeName);
end;

{ TADUCTreeNode }

procedure TADUCTreeNode.SetNodeType(AValue: TADUCTreeNodeType);
begin
  if fNodeType = AValue then
    Exit;

  ClearData;
  case AValue of
    atntObject: data := TADUCTreeNodeObject.Create;
    atntQuery: data := TADUCTreeNodeQuery.Create;
  end;
  fNodeType := AValue;
end;

procedure TADUCTreeNode.ClearData;
var
  pdata: Pointer;
begin
  pdata := Data;
  if Assigned(pdata) then
    FreeAndNil(pdata);
end;

function TADUCTreeNode.IsType(ANodeType: TADUCTreeNodeType): Boolean;
begin
  result := (NodeType = ANodeType);
end;

function TADUCTreeNode.IsNoneType: Boolean;
begin
  result := IsType(atntNone);
end;

function TADUCTreeNode.IsObjectType: Boolean;
begin
  result := IsType(atntObject);
end;

function TADUCTreeNode.IsQueryType: Boolean;
begin
  result := IsType(atntQuery);
end;

function TADUCTreeNode.GetNodeDataObject: TADUCTreeNodeObject;
begin
  result := nil;
  if IsType(atntObject) then
    result := TADUCTreeNodeObject(data);
end;

function TADUCTreeNode.GetNodeDataQuery: TADUCTreeNodeQuery;
begin
  result := nil;
  if IsType(atntQuery) then
    result := TADUCTreeNodeQuery(data);
end;

destructor TADUCTreeNode.Destroy;
begin
  ClearData;
  inherited Destroy;
end;

end.

