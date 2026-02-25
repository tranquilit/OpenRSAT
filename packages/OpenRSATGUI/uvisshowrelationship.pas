unit uvisshowrelationship;

{$mode objfpc}{$H+}
{$modeswitch TypeHelpers}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Spin,
  StdCtrls, Buttons, Menus, ActnList, IntfGraphics, EditBtn, LvlGraphCtrl,
  ursatldapclient, mormot.core.base, mormot.net.ldap, mormot.core.variants,
  Math,
  mormot.core.log,
  tis.ui.searchedit, tis.ui.grid.core;

type

  StorageID = Int64;

  StorageIDDynArray = Array of StorageID;

  { TRelationStorage }

  TRelationStorage = class
  private
    fLog: TSynLog;
    fCount: Integer;
    fCapacity: Integer;

    /// Index == DN id
    fDistinguishedNames: TRawUtf8DynArray;
    fNames: TRawUtf8DynArray;
    fMembers: Array of StorageIDDynArray;
    fMembersOf: Array of StorageIDDynArray;
    fObjectClasses: TRawUtf8DynArrayDynArray;

    /// DocVariantData repr.
    fDocVariantData: TDocVariantData;

    function ValidIndex(AID: StorageID): Boolean;
    function EmptyArray(AArray: TRawUtf8DynArray): Boolean;
    function EmptyRawUtf8(ARawUtf8: RawUtf8): Boolean;

    procedure IncreaseCapacity(ACount: Integer = 1);
    procedure AddMembers(AID: StorageID; AMembers: TRawUtf8DynArray);
    procedure AddObjectClass(AID: StorageID; AObjectClass: TRawUtf8DynArray);
    function AddMember(AID: StorageID; AMember: RawUtf8): StorageID;
  public
    constructor Create;
    function GetOrAdd(ADistinguishedName: RawUtf8): StorageID;
    function GetOrAddByName(AName: RawUtf8): StorageID;
    function Get(ADistinguishedName: RawUtf8): StorageID;
    function GetByName(AName: RawUtf8): StorageID;
    function Add(ADistinguishedName: RawUtf8; AName: RawUtf8 = ''; AMembers: TRawUtf8DynArray = nil): StorageID;
    function GetDistinguishedName(AID: StorageID): RawUtf8;
    function GetName(AID: StorageID): RawUtf8;
    function GetObjectClass(AID: StorageID): TRawUtf8DynArray;
    function GetMembers(AID: StorageID): StorageIDDynArray;
    function GetMembersNamed(AID: StorageID): TRawUtf8DynArray;
    function GetMembersOf(AID: StorageID): StorageIDDynArray;
    function GetMembersOfNamed(AID: StorageID): TRawUtf8DynArray;
    function FillFromResultList(SearchResult: TLdapResultList): Boolean;
    function AsDocVariantData: PDocVariantData;
  end;

  TNodeKey = record
    Node: TLvlGraphNode;
    Key: Double;
    OldIdx: Integer;
  end;

  { TVisShowRelationship }

  TVisShowRelationship = class(TForm)
    Action_SaveToDOT: TAction;
    Action_UnfocusGroup: TAction;
    Action_SaveToPNG: TAction;
    Action_Synchronize: TAction;
    Action_Refresh: TAction;
    Action_Property: TAction;
    Action_FocusedSelectedGroup: TAction;
    ActionList1: TActionList;
    BitBtn_Sync: TBitBtn;
    BitBtn_Refresh: TBitBtn;
    BitBtn_Next: TBitBtn;
    BitBtn_Previous: TBitBtn;
    CheckBox_ShowUser: TCheckBox;
    CheckBox_HideSingleNode: TCheckBox;
    Label_SearchCount: TLabel;
    Label_Search: TLabel;
    LvlGraphControl: TLvlGraphControl;
    MenuItem_Property: TMenuItem;
    MenuItem_FocusSelectedNode: TMenuItem;
    MenuItem_SaveToPNG: TMenuItem;
    MenuItem_UnfocusGroup: TMenuItem;
    MenuItem_SaveToDOT: TMenuItem;
    Panel_Top: TPanel;
    PopupMenu_Graph: TPopupMenu;
    SaveDialog1: TSaveDialog;
    Splitter1: TSplitter;
    TisGrid_GroupData: TTisGrid;
    TisSearchEdit1: TTisSearchEdit;
    procedure Action_FocusedSelectedGroupExecute(Sender: TObject);
    procedure Action_FocusedSelectedGroupUpdate(Sender: TObject);
    procedure Action_PropertyExecute(Sender: TObject);
    procedure Action_PropertyUpdate(Sender: TObject);
    procedure Action_RefreshExecute(Sender: TObject);
    procedure Action_SaveToDOTExecute(Sender: TObject);
    procedure Action_SaveToPNGExecute(Sender: TObject);
    procedure Action_SynchronizeExecute(Sender: TObject);
    procedure Action_UnfocusGroupExecute(Sender: TObject);
    procedure BitBtn_NextClick(Sender: TObject);
    procedure BitBtn_PreviousClick(Sender: TObject);
    procedure CheckBox_ShowUserChange(Sender: TObject);
    procedure CheckBox_HideSingleNodeChange(Sender: TObject);
    procedure LvlGraphControlDblClick(Sender: TObject);
    procedure LvlGraphControlMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LvlGraphControlMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure LvlGraphControlMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TisSearchEdit1Search(Sender: TObject; const aText: string);
  private
    fLog: TSynLog;
    fMoving: Boolean;
    fOriginX, fOriginY: Integer;

    fLdapClient: TRsatLdapClient;
    fDistinguishedName: RawUtf8;

    fNodesFound: Array of TLvlGraphNode;
    fNodesFoundIdx: Integer;
    fNodesFoundCount: Integer;

    fStorage: TRelationStorage;

    function FindNodeIndex(Node: TLvlGraphNode): integer;
    procedure SynchronizeFullAD;
    procedure SynchronizeGroup(ADistinguishedName: RawUtf8);

    procedure RefreshFullAD;
    procedure RefreshGroup(ADistinguishedName: RawUtf8);

    procedure ScrollNodeIntoView(ANode: TLvlGraphNode);
    procedure SaveToPNG(FileName: RawUtf8);
    procedure SaveToDOT(FileName: RawUtf8);
  public
    constructor Create(TheOwner: TComponent; ALdapClient: TRsatLdapClient; ADistinguishedName: RawUtf8); reintroduce;
    destructor Destroy; override;
    procedure ClearView();
    procedure CreateView(ADistinguishedName: RawUtf8);
  end;

implementation
uses
  uhelpers,
  ufrmrsat,
  variants,
  FPWritePNG,
  ucoredatamodule,
  mormot.core.data,
  mormot.core.rtti,
  mormot.core.text;

{$R *.lfm}

{ TRelationStorage }

procedure TRelationStorage.AddMembers(AID: StorageID; AMembers: TRawUtf8DynArray
  );
var
  i: Integer;
begin
  if not ValidIndex(AID) or EmptyArray(AMembers) then
    Exit;

  for i := 0 to High(AMembers) do
    if AMembers[i] <> '' then
      AddMember(AID, AMembers[i]);
end;

procedure TRelationStorage.AddObjectClass(AID: StorageID;
  AObjectClass: TRawUtf8DynArray);
var
  i, Len: Integer;
begin
  if not ValidIndex(AID) or EmptyArray(AObjectClass) then
    Exit;

  Len := Length(AObjectClass);
  SetLength(fObjectClasses[AID], Len);
  for i := 0 to Pred(Len) do
    fObjectClasses[AID][i] := AObjectClass[i];
end;

function TRelationStorage.AddMember(AID: StorageID; AMember: RawUtf8
  ): StorageID;
var
  i: Integer;
  Members, MembersOf: TInt64DynArray;
begin
  result := -1;
  if not ValidIndex(AID) or EmptyRawUtf8(AMember) then
    Exit;

  result := GetOrAdd(AMember);
  if not ValidIndex(result) then
    Exit;

  Members := fMembers[AID];
  for i := 0 to High(Members) do
    if fDistinguishedNames[Members[i]] = AMember then
      Exit;

  Insert(result, fMembers[AID], Length(fMembers[AID]));
  Insert(AID, fMembersOf[result], Length(fMembersOf[result]));
end;

function TRelationStorage.GetMembers(AID: StorageID): StorageIDDynArray;
begin
  result := nil;
  if not ValidIndex(AID) then
    Exit;

  result := fMembers[AID];
end;

function TRelationStorage.GetMembersNamed(AID: StorageID): TRawUtf8DynArray;
var
  Members: StorageIDDynArray;
  i, Len: Integer;
begin
  result := nil;

  Members := GetMembers(AID);
  if not Assigned(Members) then
    Exit;
  Len := Length(Members);
  SetLength(Result, Len);
  for i := 0 to Pred(Len) do
    result[i] := GetDistinguishedName(Members[i]);
end;

function TRelationStorage.GetMembersOf(AID: StorageID): StorageIDDynArray;
begin
  result := nil;
  if not ValidIndex(AID) then
    Exit;

  result := fMembersOf[AID];
end;

function TRelationStorage.GetMembersOfNamed(AID: StorageID): TRawUtf8DynArray;
var
  MembersOf: StorageIDDynArray;
  i, Len: Integer;
begin
  result := nil;

  MembersOf := GetMembersOf(AID);
  if not Assigned(MembersOf) then
    Exit;
  Len := Length(MembersOf);
  SetLength(result, Len);
  for i := 0 to Pred(Len) do
    result[i] := GetDistinguishedName(MembersOf[i]);
end;

function TRelationStorage.FillFromResultList(SearchResult: TLdapResultList
  ): Boolean;
var
  Item: TLdapResult;
  Start: TDateTime;
  ID: StorageID;
begin
  result := False;

  Start := Now;
  for Item in SearchResult.Items do
  begin
    if not Assigned(Item) then
      continue;
    ID := GetOrAdd(Item.ObjectName);
    if not ValidIndex(ID) then
      Continue;
    fNames[ID] := Item.Find('name').GetReadable();
    AddMembers(ID, Item.Find('member').GetAllReadable);
    AddObjectClass(ID, Item.Find('objectClass').GetAllReadable);
  end;
  TSynLog.Add.Log(sllInfo, 'Time spent: %', [FormatDateTime('hh:nn:ss zzz', Now - Start)]);
end;

constructor TRelationStorage.Create;
begin
  fLog := TSynLog.Add;

  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);

  fCount := 0;
  fCapacity := 0;
  fDistinguishedNames := nil;
  fNames := nil;
  fMembers := nil;
  fMembersOf := nil;

  fDocVariantData.Init();
end;

function TRelationStorage.GetOrAdd(ADistinguishedName: RawUtf8): StorageID;
begin
  if EmptyRawUtf8(ADistinguishedName) then
    Exit;

  result := Get(ADistinguishedName);
  if not ValidIndex(result) then
    result := Add(ADistinguishedName);
end;

function TRelationStorage.GetOrAddByName(AName: RawUtf8): StorageID;
begin
  if EmptyRawUtf8(AName) then
    Exit;

  result := GetByName(AName);
  if not ValidIndex(result) then
    result := Add('', AName);
end;

function TRelationStorage.Get(ADistinguishedName: RawUtf8): StorageID;
var
  i: Integer;
begin
  if EmptyRawUtf8(ADistinguishedName) then
    Exit;

  for i := 0 to Pred(fCount) do
    if fDistinguishedNames[i] = ADistinguishedName then
    begin
      result := i;
      Exit;
    end;
  result := -1;
end;

function TRelationStorage.GetByName(AName: RawUtf8): StorageID;
var
  i: Integer;
begin
  if EmptyRawUtf8(AName) then
    Exit;

  for i := 0 to Pred(fCount) do
    if fNames[i] = AName then
    begin
      result := i;
      Exit;
    end;
  result := -1;
end;

function TRelationStorage.Add(ADistinguishedName: RawUtf8; AName: RawUtf8;
  AMembers: TRawUtf8DynArray): StorageID;
begin
  result := -1;
  if EmptyRawUtf8(ADistinguishedName) then
    Exit;

  if fCount = fCapacity then
    IncreaseCapacity();
  result := fCount;
  Inc(fCount);
  fDistinguishedNames[result] := ADistinguishedName;
  fNames[result] := AName;
  fMembers[result] := nil;
  fMembersOf[result] := nil;
  fObjectClasses[result] := nil;
  if Assigned(AMembers) then
    AddMembers(result, AMembers);
end;

function TRelationStorage.GetDistinguishedName(AID: StorageID): RawUtf8;
begin
  result := '';
  if not ValidIndex(AID) then
    Exit;
  result := fDistinguishedNames[AID];
end;

function TRelationStorage.GetName(AID: StorageID): RawUtf8;
begin
  result := '';
  if not ValidIndex(AID) then
    Exit;
  result := fNames[AID];
end;

function TRelationStorage.GetObjectClass(AID: StorageID): TRawUtf8DynArray;
begin
  result := nil;
  if not ValidIndex(AID) then
    Exit;
  result := fObjectClasses[AID];
end;

function TRelationStorage.AsDocVariantData: PDocVariantData;
var
  Row: TDocVariantData;
  i: Integer;
begin
  result := nil;
  if fCount <= 0 then
    Exit;

  Row.Init();
  fDocVariantData.Clear;
  for i := 0 to Pred(fCount) do
  begin
    Row.AddValue('index', i);
    Row.AddValue('distinguishedName', fDistinguishedNames[i]);
    Row.AddValue('name', fNames[i]);
    Row.AddValue('member', String.Join(',', TStringDynArray(GetMembersNamed(i))));
    Row.AddValue('memberOf', String.Join(',', TStringDynArray(GetMembersOfNamed(i))));
    fDocVariantData.AddItem(Row);
    Row.Clear;
  end;
  result := @fDocVariantData;
end;

function TRelationStorage.ValidIndex(AID: StorageID): Boolean;
begin
  result := (AID >= 0) and (AID < fCount);
  if Assigned(fLog) and not result then
    fLog.Log(sllTrace, 'Invalid index (%)', [AID], Self);
end;

function TRelationStorage.EmptyArray(AArray: TRawUtf8DynArray): Boolean;
begin
  result := not Assigned(AArray);
  if Assigned(fLog) and result then
    fLog.Log(sllTrace, 'No members', Self);
end;

function TRelationStorage.EmptyRawUtf8(ARawUtf8: RawUtf8): Boolean;
begin
  result := (ARawUtf8 = '');
  if Assigned(fLog) and result then
    fLog.Log(sllTrace, 'Empty rawUtf8');
end;

procedure TRelationStorage.IncreaseCapacity(ACount: Integer);
begin
  if ACount <= 0 then
    Exit;

  Inc(fCapacity, ACount);
  SetLength(fDistinguishedNames, fCapacity);
  SetLength(fNames, fCapacity);
  SetLength(fMembers, fCapacity);
  SetLength(fMembersOf, fCapacity);
  SetLength(fObjectClasses, fCapacity);
end;

{ TVisShowRelationship }

procedure TVisShowRelationship.LvlGraphControlDblClick(Sender: TObject);
begin
  Action_Property.Execute;
end;

procedure TVisShowRelationship.LvlGraphControlMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  fMoving := True;
  fOriginX := X;
  fOriginY := Y;
  LvlGraphControl.SelectedNode := LvlGraphControl.GetNodeAt(X, Y);
end;

procedure TVisShowRelationship.LvlGraphControlMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if fMoving then
  begin
    LvlGraphControl.ScrollLeft := LvlGraphControl.ScrollLeft - (X - fOriginX);
    LvlGraphControl.ScrollTop := LvlGraphControl.ScrollTop - (Y - fOriginY);
    fOriginX := X;
    fOriginY := Y;
  end;
end;

procedure TVisShowRelationship.LvlGraphControlMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  fMoving := False;
end;

procedure TVisShowRelationship.BitBtn_NextClick(Sender: TObject);
begin
  if not Assigned(fNodesFound) then
    Exit;

  LvlGraphControl.Graph.ClearSelection;
  Inc(fNodesFoundIdx);
  if fNodesFoundIdx >= fNodesFoundCount then
    fNodesFoundIdx := 0;
  fNodesFound[fNodesFoundIdx].Selected := True;
  ScrollNodeIntoView(fNodesFound[fNodesFoundIdx]);
  Label_SearchCount.Caption := FormatUtf8('% / %', [fNodesFoundIdx + 1, fNodesFoundCount]);
end;

procedure TVisShowRelationship.Action_SynchronizeExecute(Sender: TObject);
begin
  if fDistinguishedName = '' then
    SynchronizeFullAD
  else
    SynchronizeGroup(fDistinguishedName);

  Action_Refresh.Execute;
end;

procedure TVisShowRelationship.Action_UnfocusGroupExecute(Sender: TObject);
begin
  fDistinguishedName := '';
  Action_Refresh.Execute;
end;

procedure TVisShowRelationship.Action_RefreshExecute(Sender: TObject);
begin
  if fDistinguishedName = '' then
    RefreshFullAD
  else
    RefreshGroup(fDistinguishedName);
end;

procedure TVisShowRelationship.Action_SaveToDOTExecute(Sender: TObject);
begin
  SaveDialog1.Filter := 'dot|*.dot';
  SaveDialog1.FileName := '';
  if SaveDialog1.Execute then
    SaveToDOT(SaveDialog1.FileName);
end;

procedure TVisShowRelationship.Action_SaveToPNGExecute(Sender: TObject);
begin
  SaveDialog1.Filter := 'png|*.png';
  SaveDialog1.FileName := '';
  if SaveDialog1.Execute then
    SaveToPNG(SaveDialog1.FileName);
end;

procedure TVisShowRelationship.Action_FocusedSelectedGroupExecute(
  Sender: TObject);
var
  idx: Int64;
begin
  idx := fStorage.GetByName(LvlGraphControl.SelectedNode.Caption);
  if idx < 0 then
    Exit;

  fDistinguishedName := fStorage.fDistinguishedNames[idx];
  Action_Refresh.Execute;
end;

procedure TVisShowRelationship.Action_FocusedSelectedGroupUpdate(Sender: TObject
  );
begin
  Action_FocusedSelectedGroup.Enabled := Assigned(LvlGraphControl.SelectedNode);
end;

procedure TVisShowRelationship.Action_PropertyExecute(Sender: TObject);
var
  idx: Int64;
begin
  if not Assigned(LvlGraphControl.SelectedNode) then
    Exit;
  idx := fStorage.GetByName(LvlGraphControl.SelectedNode.Caption);
  if idx < 0 then
    Exit;
  FrmRSAT.OpenProperty(fStorage.fDistinguishedNames[idx], fStorage.fNames[idx]);
end;

procedure TVisShowRelationship.Action_PropertyUpdate(Sender: TObject);
begin
  Action_Property.Enabled := Assigned(LvlGraphControl.SelectedNode);
end;

procedure TVisShowRelationship.BitBtn_PreviousClick(Sender: TObject);
begin
  if not Assigned(fNodesFound) then
    Exit;

  LvlGraphControl.Graph.ClearSelection;
  Dec(fNodesFoundIdx);
  if fNodesFoundIdx < 0 then
    fNodesFoundIdx := fNodesFoundCount - 1;
  fNodesFound[fNodesFoundIdx].Selected := True;
  ScrollNodeIntoView(fNodesFound[fNodesFoundIdx]);
  Label_SearchCount.Caption := FormatUtf8('% / %', [fNodesFoundIdx + 1, fNodesFoundCount]);
end;

procedure TVisShowRelationship.CheckBox_ShowUserChange(Sender: TObject);
begin
  Action_Refresh.Execute;
end;

procedure TVisShowRelationship.CheckBox_HideSingleNodeChange(Sender: TObject);
begin
  Action_Refresh.Execute;
end;

procedure TVisShowRelationship.TisSearchEdit1Search(Sender: TObject;
  const aText: string);
var
  i: Integer;
begin
  LvlGraphControl.Graph.ClearSelection;
  if aText = '' then
    Exit;
  fNodesFoundCount := 0;
  fNodesFound := nil;
  fNodesFoundIdx := 0;
  for i := 0 to Pred(LvlGraphControl.Graph.NodeCount) do
  begin
    if LvlGraphControl.Graph.Nodes[i].Caption.StartsWith(aText) then
    begin
      if fNodesFoundCount = 0 then
      begin
        LvlGraphControl.Graph.Nodes[i].Selected := True;
        ScrollNodeIntoView(LvlGraphControl.Graph.Nodes[i]);
      end;
      Insert(LvlGraphControl.Graph.Nodes[i], fNodesFound, fNodesFoundCount);
      Inc(fNodesFoundCount);
    end;
  end;
  Label_SearchCount.Caption := FormatUtf8('% / %', [fNodesFoundIdx + 1, fNodesFoundCount]);
  BitBtn_Next.SetFocus;
end;

procedure TVisShowRelationship.SynchronizeFullAD;
begin
  fLdapClient.SearchBegin();
  try
    fLdapClient.SearchScope := lssWholeSubtree;
    repeat
      if not fLdapClient.Search(fLdapClient.DefaultDN(), False, '(|(objectClass=group)(objectClass=user))', ['name', 'member', 'objectClass']) then
        Exit;
      fStorage.FillFromResultList(fLdapClient.SearchResult);
    until fLdapClient.SearchCookie = '';
  finally
    fLdapClient.SearchEnd;
    TisGrid_GroupData.LoadData(fStorage.AsDocVariantData);
  end;
end;

procedure TVisShowRelationship.SynchronizeGroup(ADistinguishedName: RawUtf8);
var
  Filter: RawUtf8;
begin

  fLdapClient.SearchObject(ADistinguishedName, '', ['name', 'member', 'objectClass']);
  fStorage.FillFromResultList(fLdapClient.SearchResult);

  Filter := FormatUtf8('member:1.2.840.113556.1.4.1941:=%', [LdapEscape(ADistinguishedName)]);

  fLdapClient.SearchBegin();
  try
    fLdapClient.SearchScope := lssWholeSubtree;
    repeat
      fLdapClient.SearchRangeBegin;
      try
        if not fLdapClient.Search(fLdapClient.DefaultDN(), False, Filter, ['name', 'member', 'objectClass']) then
          Exit;
      finally
        fLdapClient.SearchRangeEnd;
      end;
      fStorage.FillFromResultList(fLdapClient.SearchResult);
    until fLdapClient.SearchCookie = '';
  finally
    fLdapClient.SearchEnd;
  end;

  Filter := FormatUtf8('memberOf:1.2.840.113556.1.4.1941:=%', [LdapEscape(ADistinguishedName)]);

  fLdapClient.SearchBegin();
  try
    fLdapClient.SearchScope := lssWholeSubtree;
    repeat
      fLdapClient.SearchRangeBegin;
      try
        if not fLdapClient.Search(fLdapClient.DefaultDN(), False, Filter, ['name', 'member', 'objectClass']) then
          Exit;
      finally
        fLdapClient.SearchRangeEnd;
      end;
      fStorage.FillFromResultList(fLdapClient.SearchResult);
    until fLdapClient.SearchCookie = '';
  finally
    fLdapClient.SearchEnd;
  end;
  TisGrid_GroupData.LoadData(fStorage.AsDocVariantData);
end;

procedure TVisShowRelationship.RefreshFullAD;
var
  i, j: Integer;
  SourceNode, TargetNode: TLvlGraphNode;
  TargetName, SourceName: RawUtf8;
  ID: StorageID;
  Members: StorageIDDynArray;
begin
  LvlGraphControl.Clear;
  LvlGraphControl.BeginUpdate;
  try
    for i := 0 to Pred(fStorage.fCount) do
    begin
      if not Assigned(fStorage.fObjectClasses[i]) then
        Continue;
      if not CheckBox_ShowUser.Checked and fStorage.fObjectClasses[i].Contains('user') then
        continue;
      if CheckBox_HideSingleNode.Checked and not Assigned(fStorage.fMembers[i]) and not Assigned(fStorage.fMembersOf[i]) then
        continue;
      SourceName := fStorage.GetName(i);
      if SourceName = '' then
        SourceName := DNToCN(fStorage.GetDistinguishedName(i));
      SourceNode := LvlGraphControl.Graph.GetNode(SourceName, True);
      SourceNode.ImageIndex := ObjectClassToImageIndex(fStorage.GetObjectClass(i));
      Members := fStorage.GetMembers(i);
      for j := 0 to High(Members) do
      begin
        ID := Members[j];
        if not Assigned(fStorage.GetObjectClass(ID)) then
          Continue;
        if not CheckBox_ShowUser.Checked and fStorage.GetObjectClass(ID).Contains('user') then
          continue;
        TargetName := fStorage.GetName(ID);
        if TargetName = '' then
          TargetName := DNToCN(fStorage.GetDistinguishedName(ID));
        TargetNode := LvlGraphControl.Graph.GetNode(TargetName, True);
        if (TargetNode = SourceNode) then
          Continue;
        TargetNode.ImageIndex := ObjectClassToImageIndex(fStorage.GetObjectClass(ID));
        LvlGraphControl.Graph.GetEdge(SourceNode, TargetNode, True);
      end;
    end;
  finally
    LvlGraphControl.EndUpdate;
  end;
end;

procedure TVisShowRelationship.RefreshGroup(ADistinguishedName: RawUtf8);
var
  BaseNode: TLvlGraphNode;
  aLog: ISynLog;
  ID: StorageID;

  procedure AddMembers(BaseNode: TLvlGraphNode; ID: StorageID);
  var
    i: Integer;
    MemberNode: TLvlGraphNode;
    Members: StorageIDDynArray;
    MemberID: StorageID;
  begin
    aLog.Log(sllInfo, 'Add (%) member to %', [Length(fStorage.GetMembers(ID)), BaseNode.Caption]);
    if BaseNode.Caption = '' then
      BaseNode.Caption := DNToCN(fStorage.GetDistinguishedName(ID));
    Members := fStorage.GetMembers(ID);
    for i := 0 to High(Members) do
    begin
      MemberID := Members[i];
      if not CheckBox_ShowUser.Checked and fStorage.fObjectClasses[MemberID].Contains('user') then
        continue;
      MemberNode := LvlGraphControl.Graph.GetNode(fStorage.GetName(MemberID), True);
      if MemberNode.Caption = '' then
        MemberNode.Caption := DNToCN(fStorage.GetDistinguishedName(MemberID));
      MemberNode.ImageIndex := ObjectClassToImageIndex(fStorage.GetObjectClass(MemberID));
      AddMembers(MemberNode, MemberID);
      if BaseNode <> MemberNode then
        LvlGraphControl.Graph.GetEdge(BaseNode, MemberNode, True);
    end;
  end;

  procedure AddMembersOf(BaseNode: TLvlGraphNode; ID: StorageID);
  var
    i: Integer;
    MemberOfNode: TLvlGraphNode;
    MemberOfID: StorageID;
    MembersOf: StorageIDDynArray;
  begin
    aLog.Log(sllInfo, 'Add (%) member of %', [Length(fStorage.fMembersOf[ID]), BaseNode.Caption]);
    if BaseNode.Caption = '' then
      BaseNode.Caption := DNToCN(fStorage.GetDistinguishedName(ID));
    MembersOf := fStorage.GetMembersOf(ID);
    for i := 0 to High(MembersOf) do
    begin
      MemberOfID := MembersOf[i];
      MemberOfNode := LvlGraphControl.Graph.GetNode(fStorage.GetName(MemberOfID), True);
      if MemberOfNode.Caption = '' then
        MemberOfNode.Caption := DNToCN(fStorage.GetDistinguishedName(MemberOfID));
      MemberOfNode.ImageIndex := ObjectClassToImageIndex(fStorage.GetObjectClass(MemberOfID));
      AddMembersOf(MemberOfNode, MemberOfID);
      if BaseNode <> MemberOfNode then
        LvlGraphControl.Graph.GetEdge(MemberOfNode, BaseNode, True);
    end;
  end;
begin
  aLog := TSynLog.Enter('Start refresh', []);
  LvlGraphControl.Clear;

  ID := fStorage.Get(ADistinguishedName);
  if not fStorage.ValidIndex(ID) then
  begin
    RefreshFullAD;
    Exit;
  end;

  LvlGraphControl.BeginUpdate;
  try
    BaseNode := LvlGraphControl.Graph.GetNode(fStorage.GetName(ID), True);
    if BaseNode.Caption = '' then
      BaseNode.Caption := DNToCN(fStorage.GetDistinguishedName(ID));
    BaseNode.ImageIndex := ObjectClassToImageIndex(fStorage.GetObjectClass(ID));
    LvlGraphControl.SelectedNode := BaseNode;
    AddMembers(BaseNode, ID);
    AddMembersOf(BaseNode, ID);
  finally
    LvlGraphControl.EndUpdate;
  end;
  ScrollNodeIntoView(BaseNode);
end;

procedure TVisShowRelationship.ScrollNodeIntoView(ANode: TLvlGraphNode);
var
  R: TRect;
begin
  R := ANode.DrawnCaptionRect;
  LvlGraphControl.ScrollLeft := R.Left - (LvlGraphControl.Width div 2);
  LvlGraphControl.ScrollTop := R.Top - (LvlGraphControl.Height div 2);
end;

procedure TVisShowRelationship.SaveToPNG(FileName: RawUtf8);
const
  OFFSET = 25;
var
  Bitmap: TBitmap;
  Image: TLazIntfImage;
  Writer: TFPWriterPNG;
  y, x, ScrollLeftBak, ScrollTopBak: Integer;
  DrawSize: TPoint;
  w, h: LongInt;
begin
  Bitmap := TBitmap.Create;
  try
    ScrollLeftBak := LvlGraphControl.ScrollLeft;
    ScrollTopBak := LvlGraphControl.ScrollTop;
    w := LvlGraphControl.BoundsRect.Width - OFFSET;
    h := LvlGraphControl.BoundsRect.Height - OFFSET;

    DrawSize := LvlGraphControl.GetDrawSize;
    if (DrawSize.X <= OFFSET) or (DrawSize.Y <= OFFSET) then
      Exit;

    Bitmap.SetSize(DrawSize.X, DrawSize.Y);
    Bitmap.Canvas.Brush.Color := clWhite;
    Bitmap.Canvas.FillRect(0, 0, Bitmap.Width, Bitmap.Height);

    LvlGraphControl.ScrollLeft := 0;
    LvlGraphControl.ScrollTop := 0;

    for y := 0 to Bitmap.Height div h do
    begin
      LvlGraphControl.ScrollTop := y * h;
      for x := 0 to Bitmap.Width div w do
      begin
        LvlGraphControl.ScrollLeft := x * w;
        LvlGraphControl.PaintTo(Bitmap.Canvas, LvlGraphControl.ScrollLeft, LvlGraphControl.ScrollTop);
      end;
    end;

    Image := Bitmap.CreateIntfImage;
    try
      Writer := TFPWriterPNG.create;
      try
        Image.SaveToFile(FileName, Writer);
      finally
        FreeAndNil(Writer);
      end;
    finally
      FreeAndNil(Image);
    end;
  finally
    LvlGraphControl.ScrollLeft := ScrollLeftBak;
    LvlGraphControl.ScrollTop := ScrollTopBak;
    FreeAndNil(Bitmap);
  end;
end;

function DotEscapeLabel(const S: RawUtf8): RawUtf8;
begin
  result := StringReplace(S, '\', '\\', [rfReplaceAll]);
  result := StringReplace(result, '"', '\"', [rfReplaceAll]);
  Result := StringReplace(result, LineEnding, '\n', [rfReplaceAll]);
end;

function NodeId(Index: Integer): RawUtf8;
begin
  result := 'n' + IntToStr(Index);
end;

function TVisShowRelationship.FindNodeIndex(Node: TLvlGraphNode): integer;
begin
  for result := 0 to Pred(LvlGraphControl.Graph.NodeCount) do
    if LvlGraphControl.Graph.Nodes[result] = Node then
      Exit;
  result := -1;
end;

procedure TVisShowRelationship.SaveToDOT(FileName: RawUtf8);
var
  Lines: TStringList;
  i, tgtIndex, j: Integer;
  N, T: TLvlGraphNode;
  E: TLvlGraphEdge;
  EdgeOp: String;
begin
  Lines := TStringList.Create;
  try
    Lines.Add('digraph G {');
    EdgeOp := '->';

    Lines.Add('  rankdir=LR;');

    for i := 0 to Pred(LvlGraphControl.Graph.NodeCount) do
    begin
      N := LvlGraphControl.Graph.Nodes[i];

      if N.Caption <> '' then
        Lines.Add(Format('  %s [label="%s"];', [NodeId(i), DotEscapeLabel(N.Caption)]))
      else
        Lines.Add(Format('  %s [label="%s"];', [NodeId(i), NodeId(i)]));
    end;

    for i := 0 to Pred(LvlGraphControl.Graph.NodeCount) do
    begin
      N := LvlGraphControl.Graph.Nodes[i];
      for j := 0 to Pred(N.OutEdgeCount) do
      begin
        E := N.OutEdges[j];
        T := E.Target;

        tgtIndex := FindNodeIndex(T);
        if tgtIndex < 0 then
          Continue;

        if E.BackEdge then
          Lines.Add(Format('  %s %s %s [style=dashed];', [NodeId(i), EdgeOp, NodeId(tgtIndex)]))
        else if E.Weight > 0 then
          Lines.Add(Format('  %s %s %s [penwidth=%.3f];', [NodeId(i), EdgeOp, NodeId(tgtIndex), E.Weight]))
        else
          Lines.Add(Format('  %s %s %s;', [NodeId(i), EdgeOp, NodeId(tgtIndex)]));
      end;
    end;

    Lines.Add('}');
    Lines.SaveToFile(FileName);
  finally
    FreeAndNil(Lines);
  end;
end;

constructor TVisShowRelationship.Create(TheOwner: TComponent;
  ALdapClient: TRsatLdapClient; ADistinguishedName: RawUtf8);
begin
  inherited Create(TheOwner);

  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);

  fLdapClient := ALdapClient;
  fDistinguishedName := ADistinguishedName;
  fStorage := TRelationStorage.Create;
  fMoving := False;

  LvlGraphControl.Images := CoreDataModule.ImageList1;
end;

destructor TVisShowRelationship.Destroy;
begin
  FreeAndNil(fStorage);
  inherited Destroy;
end;

procedure TVisShowRelationship.ClearView();
begin
  LvlGraphControl.Clear;
end;

procedure TVisShowRelationship.CreateView(ADistinguishedName: RawUtf8);
begin
  SynchronizeFullAD;

  RefreshGroup('');
end;

end.

