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
  mormot.core.base,
  mormot.core.log, mormot.core.variants,
  uproperty,
  tis.ui.grid.core, VirtualTrees,
  upropertyframe;

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
    fLog: TSynLog;
    fProperty: TProperty;
    fSearchWord: RawUtf8;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Update(Props: TProperty); override;
  end;

implementation
uses
  mormot.core.text,
  mormot.net.ldap,
  ucoredatamodule,
  ucommon,
  uhelpers,
  uOmniselect,
  ursatldapclient;

{$R *.lfm}

{ TFrmPropertyMember }

procedure TFrmPropertyMember.TisGrid_MembersDblClick(Sender: TObject);
begin
  fProperty.Core.OpenProperty(TisGrid_Members.FocusedRow^.S['distinguishedName'], TisGrid_Members.FocusedRow^.S['name']);
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

  ImageIndex := CoreDataModule.objectClassToImageIndex(NodeData^.U['objectClass']);
end;

procedure TFrmPropertyMember.TisGrid_MembersKeyPress(Sender: TObject; var Key: char
  );
begin
  SearchInGrid(Timer_SearchInGrid, TisGrid_Members, fSearchWord, Key);
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
  Filter, NewMember: RawUtf8;
  i: Integer;
  Omniselect: TVisOmniselect;
begin
  // Set Filter
  Filter := '';
  for i := 0 to TisGrid_Members.TotalCount - 1 do
    Filter := FormatUtf8('%(distinguishedName=%)', [Filter, LdapEscape(TisGrid_Members.Data._[i]^.U['distinguishedName'])]);
  if Filter <> '' then
    Filter := FormatUtf8('(!(|%))', [Filter]);

  // Omniselect
  Omniselect := TVisOmniselect.Create(self, fProperty.LdapClient, ['group', 'user', 'computer', 'contact', 'Managed Service Accounts'], fProperty.LdapClient.DefaultDN, True, Filter);
  try
    Omniselect.Caption := rsTitleSelectGroups;
    if Omniselect.ShowModal() <> mrOK then
      Exit;
    for NewMember in Omniselect.SelectedObjects do
      fProperty.Add('member', NewMember, aoAlways);
  finally
    FreeAndNil(Omniselect);
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

  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);

  Caption := 'Member';
end;

procedure TFrmPropertyMember.Update(Props: TProperty);
var
  Member, Filter: RawUtf8;
  SearchResult: TLdapResult;
  Row: TDocVariantData;
  ObjectClass: TLdapAttribute;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Update', Self);

  fProperty := Props;

  Filter := '';
  for Member in fProperty.GetAllReadable('member') do
    Filter := FormatUtf8('%(distinguishedName=%)', [Filter, LdapEscape(Member)]);
  if Filter <> '' then
    Filter := FormatUtf8('(|%)', [Filter]);

  TisGrid_Members.Clear;

  fProperty.LdapClient.SearchBegin(2);
  try
    Row.Init();
    fProperty.LdapClient.SearchScope := lssWholeSubtree;
    repeat
      if not fProperty.LdapClient.Search(fProperty.LdapClient.DefaultDN, False, Filter, ['distinguishedName', 'objectClass', 'name']) then
      begin
        ShowLdapSearchError(fProperty.LdapClient);
        Exit;
      end;

      TisGrid_Members.BeginUpdate;
      try
        for SearchResult in fProperty.LdapClient.SearchResult.Items do
        begin
          if not Assigned(SearchResult) then
            continue;

          Row.AddValue('name', SearchResult.Find('name').GetReadable());
          Row.AddValue('distinguishedName', SearchResult.Find('distinguishedName').GetReadable());
          ObjectClass := SearchResult.Find('objectClass');
          Row.AddValue('objectClass', ObjectClass.GetReadable(ObjectClass.Count - 1));
          Row.AddValue('ADSF', DNToCN(GetParentDN(Row.U['distinguishedName'])));
          TisGrid_Members.Data.AddItem(Row);
          Row.Clear;
        end;
      finally
        TisGrid_Members.EndUpdate;
        TisGrid_Members.LoadData;
      end;
    until fProperty.LdapClient.SearchCookie = '';
  finally
    fProperty.LdapClient.SearchEnd;
  end;
end;

end.

