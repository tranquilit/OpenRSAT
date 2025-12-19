unit uaductreeview;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  ComCtrls,
  mormot.core.base,
  mormot.core.variants,
  mormot.net.ldap,
  ugplink;

type

  TADUCTreeNodeType = (
    atntNone,
    atntQuery,
    atntObject,
    atntGPO
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

    function GetFirstAttribute(AttributeName: RawUtf8): RawUtf8;
    function GetGPOptions: RawUtf8;
    function GetLastAttribute(AttributeName: RawUtf8): RawUtf8;
    function GetAllAttribute(AttributeName: RawUtf8): TRawUtf8DynArray;
    function GetName: String;
    procedure SetAttribute(AttributeName: RawUtf8; AttributeValues: TRawUtf8DynArray);

    function GetDistinguishedName: String;
    function GetGPLink: String;
    function GetLastObjectClass: String;
    function GetObjectClass: TRawUtf8DynArray;
    procedure SetDistinguishedName(AValue: String);
    procedure SetGPLink(AValue: String);
    procedure SetGPOptions(AValue: RawUtf8);
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
    property GPOptions: RawUtf8 read GetGPOptions write SetGPOptions;
  end;

  { TADUCTreeNodeGPO }

  TADUCTreeNodeGPO = class
  private
    fLink: TGPLink;

    function GetDistinguishedName: RawUtf8;
    function GetFlag: Integer;
    function GetGPLink: TGPLink;
    procedure SetFlag(AValue: Integer);
    procedure SetGPLink(AValue: TGPLink);
  public

    property DistinguishedName: RawUtf8 read GetDistinguishedName;
    property Flag: Integer read GetFlag write SetFlag;
    property GPLink: TGPLink read GetGPLink write SetGPLink;
  end;


  { TADUCTreeNode }

  TADUCTreeNode = class(TTreeNode)
  private
    fNodeType: TADUCTreeNodeType;

    procedure SetNodeType(AValue: TADUCTreeNodeType);
    procedure ClearData;
  public
    destructor Destroy; override;
    property NodeType: TADUCTreeNodeType read fNodeType write SetNodeType;
    function IsType(ANodeType: TADUCTreeNodeType): Boolean;
    function IsNoneType: Boolean;
    function IsObjectType: Boolean;
    function IsQueryType: Boolean;
    function IsGPOType: Boolean;
    function GetNodeDataObject: TADUCTreeNodeObject;
    function GetNodeDataQuery: TADUCTreeNodeQuery;
    function GetNodeDataGPO: TADUCTreeNodeGPO;

    function GetFirstChildGPO(): TADUCTreeNode;
    function GetNextChildGPO(ANode: TADUCTreeNode): TADUCTreeNode;

    function ADUCNodeCompare(Node1, Node2: TTreeNode): integer;
  end;

implementation
uses
  mormot.core.text;

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

function TADUCTreeNodeObject.GetGPOptions: RawUtf8;
begin
  if not Assigned(Self) then
    Exit;

  result := fAttributes.Find('gPOptions').GetReadable();
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

//function TADUCTreeNodeObject.GetGPLinks: TGPLinks;
//begin
//  result := fGPLinks;
//end;

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

procedure TADUCTreeNodeObject.SetGPOptions(AValue: RawUtf8);
begin
  SetAttribute('gPOptions', [AValue]);
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

{ TADUCTreeNodeGPO }

function TADUCTreeNodeGPO.GetGPLink: TGPLink;
begin
  result := fLink;
end;

procedure TADUCTreeNodeGPO.SetFlag(AValue: Integer);
begin
  if AValue = fLink.Flag then
    Exit;
  fLink.Flag := AValue;
end;

function TADUCTreeNodeGPO.GetDistinguishedName: RawUtf8;
begin
  result := fLink.DistinguishedName;
end;

function TADUCTreeNodeGPO.GetFlag: Integer;
begin
  result := fLink.Flag;
end;

procedure TADUCTreeNodeGPO.SetGPLink(AValue: TGPLink);
begin
  if AValue = fLink then
    Exit;

  fLink := AValue;
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
    atntGPO: data := TADUCTreeNodeGPO.Create;
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

function TADUCTreeNode.IsGPOType: Boolean;
begin
  result := IsType(atntGPO);
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

function TADUCTreeNode.GetNodeDataGPO: TADUCTreeNodeGPO;
begin
  result := nil;
  if IsType(atntGPO) then
    result := TADUCTreeNodeGPO(data);
end;

function TADUCTreeNode.GetFirstChildGPO(): TADUCTreeNode;
begin
  result := GetNextChildGPO(nil);
end;

function TADUCTreeNode.GetNextChildGPO(ANode: TADUCTreeNode): TADUCTreeNode;
var
  i: Integer;
  Node: TADUCTreeNode;
begin
  result := nil;

  if not Assigned(ANode) then
    Node := (GetFirstChild as TADUCTreeNode)
  else
    Node := (ANode.GetNextSibling as TADUCTreeNode);

  if not Assigned(Node) then
    Exit;

  repeat
    if Node.IsGPOType then
    begin
      result := Node;
      Exit;
    end;
    Node := (Node.GetNextSibling as TADUCTreeNode);
  until Node = nil;
end;

function TADUCTreeNode.ADUCNodeCompare(Node1, Node2: TTreeNode): integer;
begin
  result := Ord((Node1 as TADUCTreeNode).fNodeType) - Ord((Node2 as TADUCTreeNode).fNodeType);
  if (result = 0) then
  begin
    {$IFDEF UNIX}
    Result := CompareStr(Node1.Text, Node2.Text);
    {$ELSE}
    Result := AnsiCompareStr(Node1.Text,Node2.Text);
    {$ENDIF}
  end;
end;

destructor TADUCTreeNode.Destroy;
begin
  ClearData;
  inherited Destroy;
end;

end.

