unit uvisshowrelationship;

{$mode objfpc}{$H+}
{$modeswitch TypeHelpers}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Spin,
  StdCtrls, Buttons, Menus, ActnList, IntfGraphics, EditBtn, LvlGraphCtrl,
  ursatldapclient, mormot.core.base, mormot.net.ldap, mormot.core.variants,
  Math,
  tis.ui.searchedit, tis.ui.grid.core;

type

  { TRelationStorage }

  TRelationStorage = class
  private
    fCount: Integer;
    fCapacity: Integer;

    /// Index == DN id
    fDistinguishedNames: TRawUtf8DynArray;
    fNames: TRawUtf8DynArray;
    fMembers: Array of TInt64DynArray;
    fMembersOf: Array of TInt64DynArray;
    fObjectClasses: TRawUtf8DynArrayDynArray;

    /// DocVariantData repr.
    fDocVariantData: TDocVariantData;

    procedure IncreaseCapacity(ACount: Integer = 1);
    procedure AddMembers(AIdx: Int64; AMembers: TRawUtf8DynArray);
    procedure AddObjectClass(AIdx: Int64; AObjectClass: TRawUtf8DynArray);
    function AddMember(AIdx: Int64; AMember: RawUtf8): Int64;
    function GetMembers(AIdx: Int64): TRawUtf8DynArray;
    function GetMembersOf(AIdx: Int64): TRawUtf8DynArray;
  public
    constructor Create;
    function GetOrAdd(ADistinguishedName: RawUtf8): Int64;
    function GetOrAddByName(AName: RawUtf8): Int64;
    function Get(ADistinguishedName: RawUtf8): Int64;
    function GetByName(AName: RawUtf8): Int64;
    function Add(ADistinguishedName: RawUtf8; AName: RawUtf8 = ''; AMembers: TRawUtf8DynArray = nil): Int64;
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
    BitBtn_Refresh: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    LvlGraphControl1: TLvlGraphControl;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    SaveDialog1: TSaveDialog;
    Splitter1: TSplitter;
    TisGrid1: TTisGrid;
    TisSearchEdit1: TTisSearchEdit;
    procedure Action_FocusedSelectedGroupExecute(Sender: TObject);
    procedure Action_FocusedSelectedGroupUpdate(Sender: TObject);
    procedure Action_RefreshExecute(Sender: TObject);
    procedure Action_SaveToDOTExecute(Sender: TObject);
    procedure Action_SaveToPNGExecute(Sender: TObject);
    procedure Action_SynchronizeExecute(Sender: TObject);
    procedure Action_UnfocusGroupExecute(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure LvlGraphControl1DblClick(Sender: TObject);
    procedure LvlGraphControl1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LvlGraphControl1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure LvlGraphControl1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TisSearchEdit1Search(Sender: TObject; const aText: string);
  private
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
  mormot.core.log,
  mormot.core.text;

{$R *.lfm}

{ TRelationStorage }

procedure TRelationStorage.AddMembers(AIdx: Int64; AMembers: TRawUtf8DynArray);
var
  i: Integer;
begin
  for i := 0 to High(AMembers) do
    AddMember(AIdx, AMembers[i]);
end;

procedure TRelationStorage.AddObjectClass(AIdx: Int64; AObjectClass: TRawUtf8DynArray);
var
  i: Integer;
begin
  SetLength(fObjectClasses[AIdx], Length(AObjectClass));
  for i := 0 to High(AObjectClass) do
    fObjectClasses[AIdx][i] := AObjectClass[i];
end;

function TRelationStorage.AddMember(AIdx: Int64; AMember: RawUtf8): Int64;
var
  i: Integer;
begin
  result := GetOrAdd(AMember);
  for i := 0 to High(fMembers[AIdx]) do
    if fDistinguishedNames[fMembers[AIdx][i]] = AMember then
      Exit;
  Insert(result, fMembers[AIdx], Length(fMembers[AIdx]));
  Insert(AIdx, fMembersOf[result], Length(fMembersOf[result]));
end;

function TRelationStorage.GetMembers(AIdx: Int64): TRawUtf8DynArray;
var
  i: Integer;
begin
  SetLength(Result, Length(fMembers[AIdx]));
  for i := 0 to High(fMembers[AIdx]) do
    result[i] := fDistinguishedNames[fMembers[AIdx][i]];
end;

function TRelationStorage.GetMembersOf(AIdx: Int64): TRawUtf8DynArray;
var
  i: Integer;
begin
  SetLength(Result, Length(fMembersOf[AIdx]));
  for i := 0 to High(fMembersOf[AIdx]) do
    result[i] := fDistinguishedNames[fMembersOf[AIdx][i]];
end;

constructor TRelationStorage.Create;
begin
  fCount := 0;
  fCapacity := 0;
  fDistinguishedNames := nil;
  fNames := nil;
  fMembers := nil;
  fMembersOf := nil;

  fDocVariantData.Init();
end;

function TRelationStorage.GetOrAdd(ADistinguishedName: RawUtf8): Int64;
begin
  result := Get(ADistinguishedName);
  if result < 0 then
    result := Add(ADistinguishedName);
end;

function TRelationStorage.GetOrAddByName(AName: RawUtf8): Int64;
begin
  result := GetByName(AName);
  if result < 0 then
    result := Add('', AName);
end;

function TRelationStorage.Get(ADistinguishedName: RawUtf8): Int64;
begin
  for result := 0 to Pred(fCount) do
    if fDistinguishedNames[result] = ADistinguishedName then
      Exit;
  result := -1;
end;

function TRelationStorage.GetByName(AName: RawUtf8): Int64;
begin
  for result := 0 to Pred(fCount) do
    if fNames[result] = AName then
      Exit;
  result := -1;
end;

function TRelationStorage.Add(ADistinguishedName: RawUtf8; AName: RawUtf8;
  AMembers: TRawUtf8DynArray): Int64;
begin
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

function TRelationStorage.AsDocVariantData: PDocVariantData;
var
  Row: TDocVariantData;
  i: Integer;
begin
  result := nil;

  Row.Init();
  fDocVariantData.Clear;
  for i := 0 to Pred(fCount) do
  begin
    Row.AddValue('index', i);
    Row.AddValue('distinguishedName', fDistinguishedNames[i]);
    Row.AddValue('name', fNames[i]);
    Row.AddValue('member', String.Join(',', TStringDynArray(GetMembers(i))));
    Row.AddValue('memberOf', String.Join(',', TStringDynArray(GetMembersOf(i))));
    fDocVariantData.AddItem(Row);
    Row.Clear;
  end;
  result := @fDocVariantData;
end;

procedure TRelationStorage.IncreaseCapacity(ACount: Integer);
begin
  Inc(fCapacity, ACount);
  SetLength(fDistinguishedNames, fCapacity);
  SetLength(fNames, fCapacity);
  SetLength(fMembers, fCapacity);
  SetLength(fMembersOf, fCapacity);
  SetLength(fObjectClasses, fCapacity);
end;

{ TVisShowRelationship }

procedure TVisShowRelationship.LvlGraphControl1DblClick(Sender: TObject);
var
  SearchObject: TLdapResult;
  idx: Int64;
begin
  if not Assigned(LvlGraphControl1.SelectedNode) then
    Exit;
  idx := fStorage.GetByName(LvlGraphControl1.SelectedNode.Caption);
  if idx < 0 then
    Exit;
  FrmRSAT.OpenProperty(fStorage.fDistinguishedNames[idx], fStorage.fNames[idx]);
end;

procedure TVisShowRelationship.LvlGraphControl1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  fMoving := True;
  fOriginX := X;
  fOriginY := Y;
  LvlGraphControl1.SelectedNode := LvlGraphControl1.GetNodeAt(X, Y);
end;

procedure TVisShowRelationship.LvlGraphControl1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if fMoving then
  begin
    LvlGraphControl1.ScrollLeft := LvlGraphControl1.ScrollLeft - (X - fOriginX);
    LvlGraphControl1.ScrollTop := LvlGraphControl1.ScrollTop - (Y - fOriginY);
    fOriginX := X;
    fOriginY := Y;
  end;
end;

procedure TVisShowRelationship.LvlGraphControl1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  fMoving := False;
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
  Label1.Caption := FormatUtf8('% / %', [fNodesFoundIdx, fNodesFoundCount]);
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
  if SaveDialog1.Execute then
    SaveToDOT(SaveDialog1.FileName);
end;

procedure TVisShowRelationship.Action_SaveToPNGExecute(Sender: TObject);
begin
  if SaveDialog1.Execute then
    SaveToPNG(SaveDialog1.FileName);
end;

procedure TVisShowRelationship.Action_FocusedSelectedGroupExecute(
  Sender: TObject);
var
  idx: Int64;
begin
  idx := fStorage.GetByName(LvlGraphControl1.SelectedNode.Caption);
  if idx < 0 then
    Exit;

  fDistinguishedName := fStorage.fDistinguishedNames[idx];
  Action_Refresh.Execute;
end;

procedure TVisShowRelationship.Action_FocusedSelectedGroupUpdate(Sender: TObject
  );
begin
  Action_FocusedSelectedGroup.Enabled := Assigned(LvlGraphControl1.SelectedNode);
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
  Label1.Caption := FormatUtf8('% / %', [fNodesFoundIdx, fNodesFoundCount]);
end;

procedure TVisShowRelationship.CheckBox1Change(Sender: TObject);
begin
  Action_Refresh.Execute;
end;

procedure TVisShowRelationship.CheckBox2Change(Sender: TObject);
begin
  Action_Refresh.Execute;
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
  Label1.Caption := FormatUtf8('% / %', [fNodesFoundIdx, fNodesFoundCount]);
  BitBtn3.SetFocus;
end;

procedure TVisShowRelationship.SynchronizeFullAD;
var
  SearchResult: TLdapResult;
  Start: TDateTime;
  idx: Int64;
begin
  fLdapClient.SearchBegin();
  try
    fLdapClient.SearchScope := lssWholeSubtree;
    repeat
      if not fLdapClient.Search(fLdapClient.DefaultDN(), False, '(|(objectClass=group)(objectClass=user))', ['name', 'member', 'objectClass']) then
        Exit;
      Start := Now;
      for SearchResult in fLdapClient.SearchResult.Items do
      begin
        if not Assigned(SearchResult) then
          continue;
        idx := fStorage.GetOrAdd(SearchResult.ObjectName);
        if idx < 0 then
          Continue;
        fStorage.fNames[idx] := SearchResult.Find('name').GetReadable();
        fStorage.AddMembers(idx, SearchResult.Find('member').GetAllReadable);
        fStorage.AddObjectClass(idx, SearchResult.Find('objectClass').GetAllReadable);
      end;
      TSynLog.Add.Log(sllInfo, 'Time spent: %', [FormatDateTime('hh:nn:ss zzz', Now - Start)]);
    until fLdapClient.SearchCookie = '';
  finally
    fLdapClient.SearchEnd;
    TisGrid1.LoadData(fStorage.AsDocVariantData);
  end;
end;

procedure TVisShowRelationship.SynchronizeGroup(ADistinguishedName: RawUtf8);
begin
end;

procedure TVisShowRelationship.RefreshFullAD;
var
  i, j: Integer;
  SourceNode, TargetNode: TLvlGraphNode;
  TargetName, SourceName: RawUtf8;
  idx: Int64;
begin
  LvlGraphControl1.Clear;
  LvlGraphControl1.BeginUpdate;
  try
    for i := 0 to Pred(fStorage.fCount) do
    begin
      if not Assigned(fStorage.fObjectClasses[i]) then
        Continue;
      if not CheckBox1.Checked and fStorage.fObjectClasses[i].Contains('user') then
        continue;
      if CheckBox2.Checked and not Assigned(fStorage.fMembers[i]) and not Assigned(fStorage.fMembersOf[i]) then
        continue;
      SourceName := fStorage.fNames[i];
      if SourceName = '' then
        SourceName := DNToCN(fStorage.fDistinguishedNames[i]);
      SourceNode := LvlGraphControl1.Graph.GetNode(SourceName, True);
      SourceNode.ImageIndex := ObjectClassToImageIndex(fStorage.fObjectClasses[i]);
      for j := 0 to High(fStorage.fMembers[i]) do
      begin
        idx := fStorage.fMembers[i][j];
        if not Assigned(fStorage.fObjectClasses[idx]) then
          Continue;
        if not CheckBox1.Checked and fStorage.fObjectClasses[idx].Contains('user') then
          continue;
        TargetName := fStorage.fNames[idx];
        if TargetName = '' then
          TargetName := DNToCN(fStorage.fDistinguishedNames[idx]);
        TargetNode := LvlGraphControl1.Graph.GetNode(TargetName, True);
        if (TargetNode = SourceNode) then
          Continue;
        TargetNode.ImageIndex := ObjectClassToImageIndex(fStorage.fObjectClasses[idx]);
        LvlGraphControl1.Graph.GetEdge(SourceNode, TargetNode, True);
      end;
    end;
  finally
    LvlGraphControl1.EndUpdate;
  end;
end;

procedure TVisShowRelationship.RefreshGroup(ADistinguishedName: RawUtf8);
var
  idx: Int64;
  BaseNode: TLvlGraphNode;
  aLog: ISynLog;

  procedure AddMembers(BaseNode: TLvlGraphNode; AIdx: Int64);
  var
    i: Integer;
    MemberNode: TLvlGraphNode;
    mIdx: Int64;
  begin
    aLog.Log(sllInfo, 'Add (%) member to %', [Length(fStorage.fMembers[AIdx]), BaseNode.Caption]);
    if BaseNode.Caption = '' then
      BaseNode.Caption := DNToCN(fStorage.fDistinguishedNames[AIdx]);
    for i := 0 to High(fStorage.fMembers[AIdx]) do
    begin
      mIdx := fStorage.fMembers[AIdx][i];
      MemberNode := LvlGraphControl1.Graph.GetNode(fStorage.fNames[mIdx], True);
      if MemberNode.Caption = '' then
        MemberNode.Caption := DNToCN(fStorage.fDistinguishedNames[mIdx]);
      MemberNode.ImageIndex := ObjectClassToImageIndex(fStorage.fObjectClasses[mIdx]);
      AddMembers(MemberNode, mIdx);
      if BaseNode <> MemberNode then
        LvlGraphControl1.Graph.GetEdge(BaseNode, MemberNode, True);
    end;
  end;

  procedure AddMembersOf(BaseNode: TLvlGraphNode; AIdx: Int64);
  var
    i: Integer;
    MemberOfNode: TLvlGraphNode;
    mIdx: Int64;
  begin
    aLog.Log(sllInfo, 'Add (%) member of %', [Length(fStorage.fMembersOf[AIdx]), BaseNode.Caption]);
    if BaseNode.Caption = '' then
      BaseNode.Caption := DNToCN(fStorage.fDistinguishedNames[AIdx]);
    for i := 0 to High(fStorage.fMembersOf[AIdx]) do
    begin
      mIdx := fStorage.fMembersOf[AIdx][i];
      MemberOfNode := LvlGraphControl1.Graph.GetNode(fStorage.fNames[mIdx], True);
      if MemberOfNode.Caption = '' then
        MemberOfNode.Caption := DNToCN(fStorage.fDistinguishedNames[mIdx]);
      MemberOfNode.ImageIndex := ObjectClassToImageIndex(fStorage.fObjectClasses[mIdx]);
      AddMembersOf(MemberOfNode, mIdx);
      if BaseNode <> MemberOfNode then
        LvlGraphControl1.Graph.GetEdge(MemberOfNode, BaseNode, True);
    end;
  end;
begin
  aLog := TSynLog.Enter('Start refresh', []);
  LvlGraphControl1.Clear;
  idx := fStorage.GetOrAdd(ADistinguishedName);
  if idx < 0 then
  begin
    RefreshFullAD;
    Exit;
  end;
  LvlGraphControl1.BeginUpdate;
  try
    BaseNode := LvlGraphControl1.Graph.GetNode(fStorage.fNames[idx], True);
    if BaseNode.Caption = '' then
      BaseNode.Caption := DNToCN(fStorage.fDistinguishedNames[idx]);
    BaseNode.ImageIndex := ObjectClassToImageIndex(fStorage.fObjectClasses[idx]);
    LvlGraphControl1.SelectedNode := BaseNode;
    AddMembers(BaseNode, idx);
    AddMembersOf(BaseNode, idx);
  finally
    LvlGraphControl1.EndUpdate;
  end;
  ScrollNodeIntoView(BaseNode);
end;

procedure TVisShowRelationship.ScrollNodeIntoView(ANode: TLvlGraphNode);
var
  R: TRect;
begin
  R := ANode.DrawnCaptionRect;
  LvlGraphControl1.ScrollLeft := R.Left - (LvlGraphControl1.Width div 2);
  LvlGraphControl1.ScrollTop := R.Top - (LvlGraphControl1.Height div 2);
end;

procedure TVisShowRelationship.SaveToPNG(FileName: RawUtf8);
const
  Scale = 1;
var
  bmp: TBitmap;
  img: TLazIntfImage;
  Writer: TFPWriterPNG;
  oldL, oldT: Integer;
  oldBounds: TRect;
  sz: TPoint;
begin
  bmp := TBitmap.Create;
  try
    oldL := LvlGraphControl1.ScrollLeft;
    oldT := LvlGraphControl1.ScrollTop;
    oldBounds := LvlGraphControl1.BoundsRect;

    sz := LvlGraphControl1.GetDrawSize;
    if (sz.X <= 0) or (sz.Y <= 0) then
      Exit;

    sz.X += 25;
    sz.Y += 25;
    bmp.SetSize(sz.X, sz.Y);
    bmp.Canvas.Brush.Color := clWhite;
    bmp.Canvas.FillRect(0, 0, bmp.Width, bmp.Height);

    LvlGraphControl1.ScrollLeft := 0;
    LvlGraphControl1.ScrollTop := 0;

    LvlGraphControl1.Align := alNone;
    LvlGraphControl1.SetBounds(oldBounds.Left, oldBounds.Top, sz.X, sz.Y);

    LvlGraphControl1.PaintTo(bmp.Canvas, 0, 0);

    img := bmp.CreateIntfImage;
    try
      Writer := TFPWriterPNG.create;
      try
        img.SaveToFile(FileName, Writer);
      finally
        FreeAndNil(Writer);
      end;
    finally
      FreeAndNil(img);
    end;
  finally
    LvlGraphControl1.Align := alClient;
    LvlGraphControl1.SetBounds(oldBounds.Left, oldBounds.Top, oldBounds.Width, oldBounds.Height);
    LvlGraphControl1.ScrollLeft := oldL;
    LvlGraphControl1.ScrollTop := oldT;
    FreeAndNil(bmp);
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
  for result := 0 to Pred(LvlGraphControl1.Graph.NodeCount) do
    if LvlGraphControl1.Graph.Nodes[result] = Node then
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

    for i := 0 to Pred(LvlGraphControl1.Graph.NodeCount) do
    begin
      N := LvlGraphControl1.Graph.Nodes[i];

      if N.Caption <> '' then
        Lines.Add(Format('  %s [label="%s"];', [NodeId(i), DotEscapeLabel(N.Caption)]))
      else
        Lines.Add(Format('  %s [label="%s"];', [NodeId(i), NodeId(i)]));
    end;

    for i := 0 to Pred(LvlGraphControl1.Graph.NodeCount) do
    begin
      N := LvlGraphControl1.Graph.Nodes[i];
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

  fLdapClient := ALdapClient;
  fDistinguishedName := ADistinguishedName;

  fStorage := TRelationStorage.Create;

  LvlGraphControl1.Images := CoreDataModule.ImageList1;
  ClearView();
  CreateView(ADistinguishedName);
  fMoving := False;
end;

destructor TVisShowRelationship.Destroy;
begin
  FreeAndNil(fStorage);
  inherited Destroy;
end;

procedure TVisShowRelationship.ClearView();
begin
  LvlGraphControl1.Clear;
end;

procedure TVisShowRelationship.CreateView(ADistinguishedName: RawUtf8);
begin
  SynchronizeFullAD;

  RefreshGroup(ADistinguishedName);
end;

end.

