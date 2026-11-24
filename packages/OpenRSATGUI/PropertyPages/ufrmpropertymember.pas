unit ufrmpropertymember;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  Buttons,
  Dialogs,
  SysUtils,
  Forms,
  Controls,
  StdCtrls,
  ExtCtrls,
  ActnList,
  VirtualTrees,
  tis.ui.grid.core,
  mormot.core.base,
  mormot.core.log,
  mormot.core.variants,
  mormot.net.ldap,
  uproperty,
  uopenrsatuicontextinterface,
  upropertyframe,
  uvisobjectsselector,
  ulog;

type

  { TFrmPropertyMember }

  TFrmPropertyMember = class(TPropertyFrame)
    Action_Add: TAction;
    Action_Delete: TAction;
    ActionList1: TActionList;
    BitBtn_Add: TBitBtn;
    BitBtn_Delete: TBitBtn;
    Label_Members: TLabel;
    Panel_Bottom: TPanel;
    Timer_SearchInGrid: TTimer;
    TisGrid_Members: TTisGrid;
    procedure Action_AddExecute(Sender: TObject);
    procedure Action_DeleteExecute(Sender: TObject);
    procedure Action_DeleteUpdate(Sender: TObject);
    procedure Timer_SearchInGridTimer(Sender: TObject);
    procedure TisGrid_MembersBeforeDeleteRows(aSender: TTisGrid;
      aRows: PDocVariantData; var aAskUser, aAbort: Boolean);
    procedure TisGrid_MembersDblClick(Sender: TObject);
    procedure TisGrid_MembersGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure TisGrid_MembersKeyPress(Sender: TObject; var Key: char);
  private
    function GetLdap: TLdapClient;
  private
    fLog: TSynLogClass;
    fProperty: TProperty;
    fSearchWord: RawUtf8;

    function DCFilter: RawUtf8;
    function GCFilter: RawUtf8;

    procedure ClearGrid;
    procedure FillGrid(const Members: TRawUtf8DynArray);
    procedure ExpandGridFromDC;
    procedure ExpandGridFromGC;

    property Ldap: TLdapClient read GetLdap;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Update(Props: TProperty); override;
  end;

implementation
uses
  mormot.core.text,
  ucoredatamodule,
  ucommon,
  ucommonui,
  uhelpersui,
  ursatldapclientui;

{$R *.lfm}

{ TFrmPropertyMember }

procedure TFrmPropertyMember.TisGrid_MembersDblClick(Sender: TObject);
begin
  if Assigned(TisGrid_Members.FocusedRow) then
    fIContext.OpenProperty(TisGrid_Members.FocusedRow^.S['distinguishedName']);
end;

procedure TFrmPropertyMember.TisGrid_MembersGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  GridColumn: TTisGridColumn;
  NodeData: PDocVariantData;
begin
  GridColumn := TisGrid_Members.FindColumnByIndex(Column);
  if GridColumn.PropertyName <> 'name' then
    Exit;

  NodeData := TisGrid_Members.GetNodeAsPDocVariantData(Node);
  if not Assigned(NodeData) then
    Exit;

  ImageIndex := ObjectClassToImageIndex(NodeData^.U['objectClass']);
end;

procedure TFrmPropertyMember.TisGrid_MembersKeyPress(Sender: TObject; var Key: char
  );
begin
  SearchInGrid(Timer_SearchInGrid, TisGrid_Members, fSearchWord, Key);
end;

function TFrmPropertyMember.GetLdap: TLdapClient;
begin
  result := fProperty.LdapClient;
end;

function TFrmPropertyMember.DCFilter: RawUtf8;
var
  Node: PVirtualNode;
  NodeData: PDocVariantData;
begin
  result := '';

  Node := TisGrid_Members.GetFirst();
  while Assigned(Node) do
  begin
    NodeData := TisGrid_Members.GetNodeAsPDocVariantData(Node);
    Node := TisGrid_Members.GetNext(Node);
    if not Assigned(NodeData) or not NodeData^.Exists('distinguishedName') then
      Continue;
    result := FormatUtf8('%(distinguishedName=%)', [result, LdapEscape(NodeData^.U['distinguishedName'])]);
  end;
  if result = '' then
    Exit;
  result := FormatUtf8('(|%)', [result]);
end;

function TFrmPropertyMember.GCFilter: RawUtf8;
begin
  result := '';
end;

procedure TFrmPropertyMember.ClearGrid;
begin
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, 'Clear grid', Self);
  TisGrid_Members.Clear;
end;

procedure TFrmPropertyMember.FillGrid(const Members: TRawUtf8DynArray);
var
  MembersData: TDocVariantData;
  i: Integer;
  ILog: ISynLog;
begin
  ILog := nil;
  if Assigned(fLog) then
    fLog.EnterLocal(ILog, 'Fill grid', [], Self);

  MembersData.InitArray([], JSON_FAST);

  if Assigned(ILog) then
    ILog.Log(sllTrace, 'Prepare % elements', [Length(Members)]);

  for i := 0 to length(Members) - 1 do
    MembersData.AddItem(_ObjFast(['name', GetDNName(Members[i]), 'distinguishedName', Members[i], 'objectClass', '', 'ADSF', DNToCN(GetParentDN(Members[i]))]));
  if Assigned(ILog) then
    ILog.Log(sllTrace, 'Load data in grid');
  TisGrid_Members.LoadData(@MembersData);
end;

procedure TFrmPropertyMember.ExpandGridFromDC;
var
  Node, PrevNode: PVirtualNode;
  NodeData, P: PDocVariantData;
  Data: TDocVariantData;
  ILog: ISynLog;
begin
  ILog := nil;
  if Assigned(fLog) then
    fLog.EnterLocal(ILog, 'Expand grid from DC(%)', [Ldap.Settings.TargetUri], Self);

  Ldap.SearchScope := lssWholeSubtree;
  if not Ldap.SearchAllDocRaw(Data, Ldap.DefaultDN, DCFilter, ['name', 'distinguishedName', 'objectClass'], [roAutoRange, roKnownValuesAsArray, roObjectNameAtRoot]) then
    Exit;

  if Assigned(ILog) then
    ILog.Log(sllTrace, '% found / % total members', [Data.Count, TisGrid_Members.Data.Count]);

  TisGrid_Members.BeginUpdate;
  try
    Node := TisGrid_Members.GetFirst();
    while Assigned(Node) do
    begin
      NodeData := TisGrid_Members.GetNodeAsPDocVariantData(Node);
      PrevNode := Node;
      Node := TisGrid_Members.GetNext(Node);
      if not Assigned(NodeData) then
        Continue;
      P := Data.O[NodeData^.U['distinguishedName']];
      if not Assigned(P) then
        Continue;
      NodeData^.AddOrUpdateValue('objectClass', P^.GetValueOrEmpty('objectClass'));
      NodeData^.AddOrUpdateValue('name', P^.GetValueOrDefault('name', NodeData^.GetValueOrEmpty('name')));
      TisGrid_Members.InvalidateNode(PrevNode);
    end;
  finally
    TisGrid_Members.EndUpdate;
  end;
end;

procedure TFrmPropertyMember.ExpandGridFromGC;
begin
  if Assigned(fLog) then
    fLog.Add.Log(sllWarning, 'Members from global catalog does not have expanded information such as objectClass.', Self);
end;

procedure TFrmPropertyMember.Action_DeleteExecute(Sender: TObject);
var
  Node: PVirtualNode;
  NodeData: PDocVariantData;
begin
  if mrYes <> MessageDlg(rsTitleDeleteMember, FormatUtf8(rsDeleteMemberFromGroups, [TisGrid_Members.SelectedCount]), mtConfirmation, mbYesNoCancel, 0) then
    Exit;

  TisGrid_Members.DeleteSelectedRows;

  Node := TisGrid_Members.GetFirst();
  if not Assigned(Node) then
  begin
    fProperty.Add('member', '');
    Exit;
  end;

  NodeData := TisGrid_Members.GetNodeAsPDocVariantData(Node);
  if not Assigned(NodeData) then
    Exit;

  fProperty.Add('member', NodeData^.U['distinguishedName']);
  Node := TisGrid_Members.GetNext(Node);
  while Assigned(Node) do
  begin
    NodeData := TisGrid_Members.GetNodeAsPDocVariantData(Node);
    Node := TisGrid_Members.GetNext(Node);
    if not Assigned(NodeData) then
      Continue;
    fProperty.Add('member', NodeData^.U['distinguishedName'], aoAlways);
  end;
  Update(fProperty);
end;

procedure TFrmPropertyMember.Action_AddExecute(Sender: TObject);
var
  Vis: TVisObjectsSelector;
  NewMember: RawUtf8;
  ExcludedObjects: TRawUtf8DynArray;
  i: Integer;
begin
  ExcludedObjects := nil;
  SetLength(ExcludedObjects, TisGrid_Members.TotalCount);
  for i := 0 to TisGrid_Members.TotalCount - 1 do
    ExcludedObjects[i] := TisGrid_Members.Data._[i]^.U['distinguishedName'];

  Vis := TVisObjectsSelector.Create(Self);
  try
    Vis.LdapClient := fProperty.LdapClient;
    Vis.AllowedObjectTypes := [otfUser, otfGroup, otfContact, otfComputer];
    Vis.SelectedObjectTypes := [otfUser, otfGroup, otfContact, otfComputer];
    Vis.AllowMultiSelect := True;
    Vis.ExcludedObjects := ExcludedObjects;
    if Vis.ShowModal <> mrOK then
      Exit;
    for NewMember in Vis.SelectedObjects do
      if NewMember <> '' then
        fProperty.Add('member', NewMember, aoAlways);
  finally
    FreeAndNil(Vis);
  end;
  Update(fProperty);
end;

procedure TFrmPropertyMember.Action_DeleteUpdate(Sender: TObject);
begin
  Action_Delete.Enabled := (TisGrid_Members.SelectedCount > 0);
end;

procedure TFrmPropertyMember.Timer_SearchInGridTimer(Sender: TObject);
begin
  Timer_SearchInGrid.Enabled := False;
end;

procedure TFrmPropertyMember.TisGrid_MembersBeforeDeleteRows(aSender: TTisGrid;
  aRows: PDocVariantData; var aAskUser, aAbort: Boolean);
begin
  aAskUser := False;
end;

constructor TFrmPropertyMember.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fLog := TOpenRSATLog;
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, 'Create', Self);

  Caption := 'Member';
end;

procedure TFrmPropertyMember.Update(Props: TProperty);
begin
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, 'Update', Self);

  fProperty := Props;

  ClearGrid;
  FillGrid(fProperty.GetAllReadable('member'));
  ExpandGridFromDC;
  ExpandGridFromGC;
end;

end.

