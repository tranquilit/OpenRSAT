unit uvisshowrelationship;

{$mode objfpc}{$H+}
{$modeswitch TypeHelpers}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Spin,
  StdCtrls, Buttons, LvlGraphCtrl, ursatldapclient, mormot.core.base,
  mormot.net.ldap, mormot.core.variants, tis.ui.searchedit;

type

  { TNodeRelationShip }

  TNodeRelationShip = class
  private
    fName: RawUtf8;
    fDistinguishedName: RawUtf8;
    fObjectClass: TRawUtf8DynArray;
    fMembers: TRawUtf8DynArray;
    fMembersOf: TRawUtf8DynArray;
    fGraphNode: TLvlGraphNode;
  public
    constructor Create(ADistinguishedName: RawUtf8; AName: RawUtf8 = ''; AObjectClass: TRawUtf8DynArray = nil);

    procedure AddMemberOf(ADistinguishedName: RawUtf8);
    procedure AddMember(ADistinguishedName: RawUtf8);
    procedure AddObjectClass(ObjectClass: RawUtf8);

    function HasMember(ADistinguishedName: RawUtf8): Boolean;
    function HasMemberOf(ADistinguishedName: RawUtf8): Boolean;

    property Name: RawUtf8 read fName write fName;
    property DistinguishedName: RawUtf8 read fDistinguishedName;
    property ObjectClass: TRawUtf8DynArray read fObjectClass write fObjectClass;
    property Members: TRawUtf8DynArray read fMembers;
    property MembersOf: TRawUtf8DynArray read fMembersOf;
    property GraphNode: TLvlGraphNode read fGraphNode write fGraphNode;
  end;

  TNodeRelationShipDynArray = Array of TNodeRelationShip;

  { TNodeRelationShipDynArrayHelper }

  TNodeRelationShipDynArrayHelper = type helper for TNodeRelationShipDynArray
    function Count: Integer;
    function HasNode(ADistinguishedName: RawUtf8): Boolean;
    function GetNode(ADistinguishedName: RawUtf8): TNodeRelationShip;
    function AddNode(ADistinguishedName: RawUtf8; AName: RawUtf8 = ''; AObjectClass: TRawUtf8DynArray = nil): TNodeRelationShip;
    procedure AddNode(ANode: TNodeRelationShip);
    function GetOrAddNode(ADistinguishedName: RawUtf8; AName: RawUtf8 = ''; AObjectClass: TRawUtf8DynArray = nil): TNodeRelationShip;
  end;

  PNodeRelationShip = ^TNodeRelationShip;

  { TVisShowRelationship }

  TVisShowRelationship = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    LvlGraphControl1: TLvlGraphControl;
    Panel1: TPanel;
    TisSearchEdit1: TTisSearchEdit;
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure LvlGraphControl1DblClick(Sender: TObject);
    procedure TisSearchEdit1Search(Sender: TObject; const aText: string);
  private
    fLdapClient: TRsatLdapClient;
    fDistinguishedName: RawUtf8;
    fData: TDocVariantData;

    fNodesFound: Array of TLvlGraphNode;
    fNodesFoundIdx: Integer;
    fNodesFoundCount: Integer;

    fNodes: TNodeRelationShipDynArray;

    procedure ResolveFullAD;
    procedure ResolveObject(ADistinguishedName: RawUtf8);

    procedure ScrollNodeIntoView(ANode: TLvlGraphNode);

    function ResolveMembers(Objects: TRawUtf8DynArray): TRawUtf8DynArray;
    function ResolveMemberOf(Objects: TRawUtf8DynArray): TRawUtf8DynArray;
  public
    constructor Create(TheOwner: TComponent; ALdapClient: TRsatLdapClient; ADistinguishedName: RawUtf8); reintroduce;

    procedure ClearView();
    procedure CreateView(ADistinguishedName: RawUtf8);
  end;

implementation
uses
  uhelpers,
  ufrmrsat,
  variants,
  ucoredatamodule,
  mormot.core.data,
  mormot.core.rtti,
  mormot.core.text;

{$R *.lfm}

{ TNodeRelationShip }

constructor TNodeRelationShip.Create(ADistinguishedName: RawUtf8;
  AName: RawUtf8; AObjectClass: TRawUtf8DynArray);
begin
  fDistinguishedName := ADistinguishedName;
  fName := AName;
  fObjectClass := AObjectClass;
  fMembersOf := nil;
  fMembers := nil;
  fGraphNode := nil;
end;

procedure TNodeRelationShip.AddMemberOf(ADistinguishedName: RawUtf8);
var
  a: RawUtf8;
begin
  a := LowerCase(ADistinguishedName);
  if not HasMemberOf(a) then
    Insert(a, fMembersOf, Length(fMembersOf))
end;

procedure TNodeRelationShip.AddMember(ADistinguishedName: RawUtf8);
var
  a: RawUtf8;
begin
  a := LowerCase(ADistinguishedName);
  if not HasMember(a) then
    Insert(a, fMembers, Length(fMembers));
end;

procedure TNodeRelationShip.AddObjectClass(ObjectClass: RawUtf8);
begin
  Insert(ObjectClass, fObjectClass, Length(fObjectClass));
end;

function TNodeRelationShip.HasMember(ADistinguishedName: RawUtf8): Boolean;
var
  a: RawUtf8;
  i: Integer;
begin
  result := True;
  a := LowerCase(ADistinguishedName);
  for i := 0 to High(fMembers) do
    if fMembers[i] = a then
      Exit;
  result := False;
end;

function TNodeRelationShip.HasMemberOf(ADistinguishedName: RawUtf8): Boolean;
var
  i: Integer;
  a: RawUtf8;
begin
  result := True;
  a := LowerCase(ADistinguishedName);
  for i := 0 to High(fMembersOf) do
    if LowerCase(fMembersOf[i]) = a then
      Exit;
  result := False;
end;

{ TNodeRelationShipDynArrayHelper }

function TNodeRelationShipDynArrayHelper.Count: Integer;
begin
  result := Length(Self);
end;

function TNodeRelationShipDynArrayHelper.HasNode(ADistinguishedName: RawUtf8
  ): Boolean;
begin
  result := Assigned(GetNode(ADistinguishedName));
end;

function TNodeRelationShipDynArrayHelper.GetNode(ADistinguishedName: RawUtf8
  ): TNodeRelationShip;
var
  a: RawUtf8;
  i: Integer;
begin
  result := nil;
  a := LowerCase(ADistinguishedName);
  for i := 0 to High(Self) do
    if LowerCase(Self[i].DistinguishedName) = a then
    begin
      result := Self[i];
      Exit;
    end;
end;

function TNodeRelationShipDynArrayHelper.AddNode(ADistinguishedName: RawUtf8;
  AName: RawUtf8; AObjectClass: TRawUtf8DynArray): TNodeRelationShip;
begin
  result := TNodeRelationShip.Create(ADistinguishedName, AName, AObjectClass);
  Insert(result, Self, Count);
end;

procedure TNodeRelationShipDynArrayHelper.AddNode(ANode: TNodeRelationShip);
begin
  Insert(ANode, Self, Count);
end;

function TNodeRelationShipDynArrayHelper.GetOrAddNode(
  ADistinguishedName: RawUtf8; AName: RawUtf8; AObjectClass: TRawUtf8DynArray
  ): TNodeRelationShip;
var
  cObjectClass: TRawUtf8DynArray;
begin
  DynArrayCopy(@cObjectClass, @AObjectClass, TypeInfo(TRawUtf8DynArray));
  result := GetNode(ADistinguishedName);
  if Assigned(result) then
  begin
    if (AName <> '') then
      result.Name := AName;
    if (Assigned(cObjectClass)) then
      result.ObjectClass := cObjectClass;
  end
  else
    result := AddNode(ADistinguishedName, AName, cObjectClass);
end;

{ TVisShowRelationship }

procedure TVisShowRelationship.LvlGraphControl1DblClick(Sender: TObject);
var
  SearchObject: TLdapResult;
begin
  if not Assigned(LvlGraphControl1.SelectedNode) then
    Exit;
  SearchObject := fLdapClient.SearchObject(fLdapClient.DefaultDN(), FormatUtf8('name=%', [LdapEscape(LvlGraphControl1.SelectedNode.Caption)]), ['distinguishedName'], lssWholeSubtree);
  FrmRSAT.OpenProperty(SearchObject.ObjectName);
end;

procedure TVisShowRelationship.BitBtn3Click(Sender: TObject);
begin
  if not Assigned(fNodesFound) then
    Exit;

  LvlGraphControl1.Graph.ClearSelection;
  Inc(fNodesFoundIdx);
  if fNodesFoundIdx >= fNodesFoundCount then
    fNodesFoundIdx := 0;
  fNodesFound[fNodesFoundIdx].Selected := True;
  ScrollNodeIntoView(fNodesFound[fNodesFoundIdx]);
end;

procedure TVisShowRelationship.BitBtn4Click(Sender: TObject);
begin
  if not Assigned(fNodesFound) then
    Exit;

  LvlGraphControl1.Graph.ClearSelection;
  Dec(fNodesFoundIdx);
  if fNodesFoundIdx < 0 then
    fNodesFoundIdx := fNodesFoundCount - 1;
  fNodesFound[fNodesFoundIdx].Selected := True;
  ScrollNodeIntoView(fNodesFound[fNodesFoundIdx]);
end;

procedure TVisShowRelationship.TisSearchEdit1Search(Sender: TObject;
  const aText: string);
var
  i: Integer;
begin
  LvlGraphControl1.Graph.ClearSelection;
  if aText = '' then
    Exit;
  fNodesFoundCount := 0;
  fNodesFound := nil;
  fNodesFoundIdx := 0;
  for i := 0 to Pred(LvlGraphControl1.Graph.NodeCount) do
  begin
    if LvlGraphControl1.Graph.Nodes[i].Caption.StartsWith(aText) then
    begin
      if fNodesFoundCount = 0 then
      begin
        LvlGraphControl1.Graph.Nodes[i].Selected := True;
        ScrollNodeIntoView(LvlGraphControl1.Graph.Nodes[i]);
      end;
      Insert(LvlGraphControl1.Graph.Nodes[i], fNodesFound, fNodesFoundCount);
      Inc(fNodesFoundCount);
    end;
  end;
end;

procedure TVisShowRelationship.ResolveFullAD;
var
  data: TDocVariantData;
  SearchResult: TLdapResult;
  member, dataName: RawUtf8;
  BaseNode, MemberNode: TLvlGraphNode;
  Count: Integer;
begin
  Count := 0;
  data.init();
  fLdapClient.SearchBegin();
  repeat
    fLdapClient.SearchRangeBegin;
    fLdapClient.Search(fLdapClient.DefaultDN(), False, '(|(objectClass=group)(objectClass=user))', ['name', 'member', 'objectClass']);
    fLdapClient.SearchRangeEnd;

    for SearchResult in fLdapClient.SearchResult.Items do
    begin
      if not Assigned(SearchResult) then
        continue;
      data.O_[SearchResult.ObjectName]^.AddValue('name', SearchResult.Find('name').GetReadable());
      for member in SearchResult.Find('member').GetAllReadable do
        data.O_[SearchResult.ObjectName]^.A_['member']^.AddItem(member);
      data.O_[SearchResult.ObjectName]^.AddValue('objectClass', SearchResult.Find('objectClass').GetReadable(SearchResult.Find('objectClass').Count - 1));
    end;
    Inc(Count);
  until (fLdapClient.SearchCookie = '') or (Count >= 5);
  fLdapClient.SearchEnd;

  for dataName in data.GetNames do
  begin
    BaseNode := LvlGraphControl1.Graph.GetNode(data.O[dataName]^.S['name'], True);
    BaseNode.ImageIndex := ObjectClassToImageIndex(data.O[dataName]^.S['objectClass']);
    for member in data.O[dataName]^.A_['member']^.ToRawUtf8DynArray do
    begin
      MemberNode := LvlGraphControl1.Graph.GetNode(data.O[member]^.S['Name'], True);
      MemberNode.ImageIndex := ObjectClassToImageIndex(data.O[member]^.S['objectClass']);
      LvlGraphControl1.Graph.GetEdge(BaseNode, MemberNode, True);
    end;
  end;
end;

procedure TVisShowRelationship.ResolveObject(ADistinguishedName: RawUtf8);
var
  Count: Integer;
  Members, MembersOf: TRawUtf8DynArray;
begin
  Count := 0;
  Members := [ADistinguishedName];
  MembersOf := nil; // [ADistinguishedName];
  repeat
    Members := ResolveMembers(Members);
    //MembersOf := ResolveMemberOf(MembersOf);
    Inc(Count);
    if Count > 100 then
    begin
      ShowMessage('Stop member resolutions (more than 100 iteration).');
      Break;
    end;
  until (Members = nil) and (MembersOf = nil);
end;

procedure TVisShowRelationship.ScrollNodeIntoView(ANode: TLvlGraphNode);
var
  R: TRect;
begin
  R := ANode.DrawnCaptionRect;
  LvlGraphControl1.ScrollLeft := R.Left - (LvlGraphControl1.Width div 2);
  LvlGraphControl1.ScrollTop := R.Top - (LvlGraphControl1.Height div 2);
end;

function UpdateGraphOptions(Options: TLvlGraphCtrlOptions; Option: TLvlGraphCtrlOption; Included: Boolean): TLvlGraphCtrlOptions;
begin
  result := Options;
  if Included then
    Include(Result, Option)
  else
    Exclude(Result, Option);
end;

function TVisShowRelationship.ResolveMembers(Objects: TRawUtf8DynArray
  ): TRawUtf8DynArray;
var
  member, Filter, m, r, s: RawUtf8;
  SearchResult: TLdapResult;
  idx: Integer;
  a: PDocVariantData;
  ANode, ANodeMember: TNodeRelationShip;
begin
  result := nil;
  idx := 0;

  /// Resolve missing Objects
  Filter := '';
  for member in Objects do
    Filter := FormatUtf8('%(distinguishedName=%)', [Filter, member]);

  if Filter <> '' then
  begin
    Filter := FormatUtf8('(|%)', [Filter]);

    repeat
      if not fLdapClient.Search(fLdapClient.DefaultDN(), False, Filter, ['name', 'member', 'objectClass']) then
        Exit;

      for SearchResult in fLdapClient.SearchResult.Items do
      begin
        if not Assigned(SearchResult) then
          continue;
        ANode := fNodes.GetOrAddNode(SearchResult.ObjectName, SearchResult.Find('name').GetReadable(), SearchResult.Find('objectClass').GetAllReadable);
        for member in SearchResult.Find('member').GetAllReadable do
        begin
          ANode.AddMember(member);
          fNodes.GetOrAddNode(member).AddMemberOf(SearchResult.ObjectName);
        end;
      end;
    until fLdapClient.SearchCookie = '';
  end;

  /// Resolve missing Objects' member
  Filter := '';
  for member in Objects do
    for m in fNodes.GetNode(member).Members do
      Filter := FormatUtf8('%(distinguishedName=%)', [Filter, m]);
    //for m in fData.O_[member]^.A_['member']^.ToRawUtf8DynArray do

  if Filter <> '' then
  begin
    Filter := FormatUtf8('(|%)', [Filter]);

    repeat
      if not fLdapClient.Search(fLdapClient.DefaultDN(), False, Filter, ['name', 'member', 'objectClass']) then
        Exit;

      for SearchResult in fLdapClient.SearchResult.Items do
      begin
        if not Assigned(SearchResult) then
          Continue;

        ANode := fNodes.GetOrAddNode(SearchResult.ObjectName, SearchResult.Find('name').GetReadable(), SearchResult.Find('objectClass').GetAllReadable);
        for member in SearchResult.Find('member').GetAllReadable do
        begin
          ANode.AddMember(member);
          fNodes.GetOrAddNode(member).AddMemberOf(SearchResult.ObjectName);
        end;
      end;
    until fLdapClient.SearchCookie = '';
  end;

  /// Create node and links
  LvlGraphControl1.BeginUpdate;
  for member in Objects do
  begin
    ANode := fNodes.GetNode(member);
    ANode.GraphNode := LvlGraphControl1.Graph.GetNode(ANode.Name, True);
    ANode.GraphNode.ImageIndex := ObjectClassToImageIndex(ANode.ObjectClass);

    SetLength(result, idx + Length(ANode.Members));
    for m in ANode.Members do
    begin
      result[Idx] := m;
      Inc(Idx);
      ANodeMember := fNodes.GetNode(m);
      ANodeMember.GraphNode := LvlGraphControl1.Graph.GetNode(ANodeMember.Name, True);
      ANodeMember.GraphNode.ImageIndex := ObjectClassToImageIndex(ANodeMember.ObjectClass);
      if ANodeMember.GraphNode <> ANode.GraphNode then
        LvlGraphControl1.Graph.GetEdge(ANode.GraphNode, ANodeMember.GraphNode, True);
    end;
  end;
  LvlGraphControl1.EndUpdate;
end;

function TVisShowRelationship.ResolveMemberOf(Objects: TRawUtf8DynArray
  ): TRawUtf8DynArray;
var
  o, Filter, m, n, s, r: RawUtf8;
  SearchResult: TLdapResult;
  idx: Integer;
  TargetNode, BaseNode: TLvlGraphNode;
  Obj: PDocVariantData;
begin
  result := nil;
  idx := 0;
  Filter := '';
  for o in Objects do
  begin
    Filter := FormatUtf8('%(member=%)', [Filter, LdapEscape(o)]);
    if not fData.Exists(LowerCase(o)) then
      Filter := FormatUtf8('%(distinguishedName=%)', [Filter, LdapEscape(o)]);
  end;

  if Filter <> '' then
  begin
    Filter := FormatUtf8('(|%)', [Filter]);

    repeat
      if not fLdapClient.Search(fLdapClient.DefaultDN(), False, Filter, ['name', 'member', 'objectClass']) then
        Exit;

      for SearchResult in fLdapClient.SearchResult.Items do
      begin
        if not Assigned(SearchResult) then
          continue;
        r := LowerCase(SearchResult.ObjectName);
        fData.O_[r]^.AddValue('name', SearchResult.Find('name').GetReadable());

        for m in SearchResult.Find('objectClass').GetAllReadable do
          fData.O_[r]^.A_['objectClass']^.AddItem(m);

        for m in SearchResult.Find('member').GetAllReadable do
        begin
          fData.O_[r]^.A_['member']^.AddItem(m);
          s := LowerCase(m);
          if not fData.O_[s]^.O_['memberOf']^.Exists(r) then
            fData.O[s]^.O_['memberOf']^.O_[r];
        end;

      end;
    until fLdapClient.SearchCookie = '';
  end;

  /// Create node and links
  LvlGraphControl1.BeginUpdate;
  for o in Objects do
  begin
    s := LowerCase(o);

    TargetNode := LvlGraphControl1.Graph.GetNode(fData.O_[s]^.S['name'], True);
    TargetNode.ImageIndex := ObjectClassToImageIndex(fData.O_[s]^.A['objectClass']^.ToRawUtf8DynArray);
    if VarIsNull(fData.O_[s]^.GetValueOrNull('memberOf')) then
      continue;
    SetLength(result, idx + fData.O_[s]^.O_['memberOf']^.Count);
    for m in fData.O_[s]^.O_['memberOf']^.Names do
    begin
      result[idx] := m;
      Inc(idx);
      r := LowerCase(m);

      BaseNode := LvlGraphControl1.Graph.GetNode(fData.O_[r]^.S['name'], True);
      BaseNode.ImageIndex := ObjectClassToImageIndex(fData.O_[r]^.A['objectClass']^.ToRawUtf8DynArray);

      if BaseNode <> TargetNode then
        LvlGraphControl1.Graph.GetEdge(BaseNode, TargetNode, True);
    end;
  end;
  LvlGraphControl1.EndUpdate;
end;

constructor TVisShowRelationship.Create(TheOwner: TComponent;
  ALdapClient: TRsatLdapClient; ADistinguishedName: RawUtf8);
begin
  inherited Create(TheOwner);

  fLdapClient := ALdapClient;
  fDistinguishedName := ADistinguishedName;

  fData.Init();

  LvlGraphControl1.Images := CoreDataModule.ImageList1;
  ClearView();
  CreateView(ADistinguishedName);
end;

procedure TVisShowRelationship.ClearView();
begin
  LvlGraphControl1.Clear;
end;

procedure TVisShowRelationship.CreateView(ADistinguishedName: RawUtf8);
var
  BaseNode, MemberNode: TLvlGraphNode;
  LdapResult, SearchResult: TLdapResult;
  member, Filter, NewFilter, NodeName, dataName: RawUtf8;
  NodeMember: TRawUtf8DynArray;
  data: TDocVariantData;
  Count: Integer;
  Members, MembersOf: TRawUtf8DynArray;
begin
  if ADistinguishedName = '' then
    ResolveFullAD
  else
    ResolveObject(ADistinguishedName);
  Exit;

  LdapResult := TLdapResult(fLdapClient.SearchObject(ADistinguishedName, '', ['name', 'member', 'memberOf']).Clone);
  //NodeName := LdapResult.Find('name').GetReadable();
  NodeMember := LdapResult.Find('member').GetAllReadable;

  //BaseNode := LvlGraphControl1.Graph.GetNode(NodeName, True);

  Filter := FormatUtf8('(distinguishedName=%)', [LdapEscape(ADistinguishedName)]);
  for member in NodeMember do
    Filter := FormatUtf8('%(distinguishedName=%)', [Filter, LdapEscape(member)]);
  Filter := FormatUtf8('(|%(member=%))', [Filter, LdapEscape(ADistinguishedName)]);

  NewFilter := '';
  Count := 0;
  LvlGraphControl1.BeginUpdate;
  fLdapClient.SearchBegin();
  fLdapClient.SearchScope := lssWholeSubtree;
  repeat
    repeat
      fLdapClient.SearchRangeBegin;
      if not fLdapClient.Search(fLdapClient.DefaultDN, False, Filter, ['name', 'member', 'memberOf']) then
        Break;
      fLdapClient.SearchRangeEnd;

      for SearchResult in fLdapClient.SearchResult.Items do
      begin
        if not Assigned(SearchResult) then
          Continue;

        NodeName := SearchResult.Find('name').GetReadable();
        NewFilter := FormatUtf8('%(distinguishedName=%)', [NewFilter, LdapEscape(SearchResult.ObjectName)]);
        NewFilter := FormatUtf8('%(member=%)', [NewFilter, LdapEscape(SearchResult.ObjectName)]);
        for member in SearchResult.Find('member').GetAllReadable do
          NewFilter := FormatUtf8('%(distinguishedName=%)', [NewFilter, LdapEscape(member)]);

        MemberNode := LvlGraphControl1.Graph.GetNode(NodeName, True);

        if NodeMember.Contains(SearchResult.ObjectName) then
          LvlGraphControl1.Graph.GetEdge(BaseNode, MemberNode, True)
        else
          LvlGraphControl1.Graph.GetEdge(MemberNode, BaseNode, True);
      end;
    until fLdapClient.SearchCookie = '';
    Filter := NewFilter;
    NewFilter := '';
    Inc(Count);
  until Count = 5;
  fLdapClient.SearchEnd;
  LvlGraphControl1.EndUpdate;
  FreeAndNil(LdapResult);
end;

end.

