unit ufrmpropertysecurity;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Buttons,
  Forms,
  Controls,
  PairSplitter,
  ExtCtrls,
  ActnList,
  tis.ui.grid.core,
  mormot.core.base,
  mormot.core.log,
  mormot.core.os.security,
  mormot.net.ldap,
  uproperty,
  upropertyframe,
  uvisadvancedsecurity,
  VirtualTrees,
  mormot.core.variants;

type

  { TFrmPropertySecurity }

  TFrmPropertySecurity = class(TPropertyFrame)
    Action_Add: TAction;
    Action_Delete: TAction;
    Action_Advanced: TAction;
    ActionList1: TActionList;
    BitBtn_AdvSecurity: TBitBtn;
    BitBtn_SecurityAddUser: TBitBtn;
    BitBtn_SecurityDeleteUser: TBitBtn;
    Panel1: TPanel;
    Panel_Top: TPanel;
    Panel_Bottom: TPanel;
    Panel_SecurityBottomButtons: TPanel;
    Splitter1: TSplitter;
    Timer_TisGridSearch: TTimer;
    TisGrid_SecurityListRight: TTisGrid;
    TisGrid_SecurityListUser: TTisGrid;
    procedure Action_AdvancedExecute(Sender: TObject);
    procedure Timer_TisGridSearchTimer(Sender: TObject);
    procedure TisGrid_SecurityListRightGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure TisGrid_SecurityListRightGetText(aSender: TBaseVirtualTree;
      aNode: PVirtualNode; const aCell: TDocVariantData; aColumn: TColumnIndex;
      aTextType: TVSTTextType; var aText: string);
    procedure TisGrid_SecurityListRightKeyPress(Sender: TObject; var Key: char);
    procedure TisGrid_SecurityListUserFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure TisGrid_SecurityListUserKeyPress(Sender: TObject; var Key: char);
  private
    fLog: TSynLog;
    fProperty: TProperty;
    fSearchWord: RawUtf8;

    procedure SecurityFillListUser;
    procedure SecurityFillRightGrid(Sid, SidName: RawUtf8);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Update(Props: TProperty); override;
  end;

implementation
uses
  mormot.core.data,
  mormot.core.text,
  ucommon,
  ucoredatamodule,
  uhelpersui,
  ursatldapclient;

{$R *.lfm}

{ TFrmPropertySecurity }

procedure TFrmPropertySecurity.Action_AdvancedExecute(Sender: TObject);
var
  Vis: TVisAdvancedSecurity;
begin
  Vis := TVisAdvancedSecurity.Create(self, fProperty.LdapClient, fProperty.SecurityDescriptor, fProperty.DistinguishedName, fProperty.LdapClient.DefaultDN());
  try
    Vis.Caption := FormatUtf8(rsTitleAdvancedSecurity, [fProperty.GetReadable('name')]);
    if Vis.ShowModal() <> mrOK then
      Exit;
  finally
    FreeAndNil(Vis);
  end;

  // Update security descriptor binary value
  fProperty.SecurityDescriptor := fProperty.SecurityDescriptor;
  SecurityFillListUser();
end;

procedure TFrmPropertySecurity.Timer_TisGridSearchTimer(Sender: TObject);
begin
  Timer_TisGridSearch.Enabled := False;
end;

procedure TFrmPropertySecurity.TisGrid_SecurityListRightGetImageIndex(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
begin
  case TisGrid_SecurityListRight.FindColumnByIndex(Column).PropertyName of
    'allow':
    begin
      if TisGrid_SecurityListRight.GetNodeAsPDocVariantData(Node)^.B['allow'] then
        ImageIndex := Ord(ileValid)
      else
        ImageIndex := -1;
    end;
    'deny':
    begin
      if TisGrid_SecurityListRight.GetNodeAsPDocVariantData(Node)^.B['deny'] then
        ImageIndex := Ord(ileValid)
      else
        ImageIndex := -1;
    end;
  end;
end;

procedure TFrmPropertySecurity.TisGrid_SecurityListRightGetText(
  aSender: TBaseVirtualTree; aNode: PVirtualNode; const aCell: TDocVariantData;
  aColumn: TColumnIndex; aTextType: TVSTTextType; var aText: string);
begin
  if TisGrid_SecurityListRight.FindColumnByIndex(aColumn).PropertyName <> 'name' then
    aText := '';
end;

procedure TFrmPropertySecurity.TisGrid_SecurityListRightKeyPress(
  Sender: TObject; var Key: char);
begin
  SearchInGrid(Timer_TisGridSearch, TisGrid_SecurityListRight, fSearchWord, Key);
end;

procedure TFrmPropertySecurity.TisGrid_SecurityListUserFocusChanged(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
var
  NodeData: PDocVariantData;
begin
  NodeData := TisGrid_SecurityListUser.GetNodeAsPDocVariantData(Node);
  if not Assigned(NodeData) then
    Exit;
  SecurityFillRightGrid(NodeData^.S['sid'], NodeData^.S['name']);
end;

procedure TFrmPropertySecurity.TisGrid_SecurityListUserKeyPress(
  Sender: TObject; var Key: char);
begin
  SearchInGrid(Timer_TisGridSearch, TisGrid_SecurityListUser, fSearchWord, Key);
end;

procedure TFrmPropertySecurity.SecurityFillListUser;
var
  newItem, sids: TDocVariantData;
  len: SizeInt;
  i: Integer;
  rawSidArr: Array of RawUtf8;
  Filter, namedSid: RawUtf8;
  acl: TSecAce;
  SearchResult: TLdapResult;
begin
  newItem.Init([dvoCheckForDuplicatedNames]);

  TisGrid_SecurityListUser.BeginUpdate();
  try
    TisGrid_SecurityListUser.Clear();
    // Resolve Sids
    rawSidArr := [];
    len := Length(fProperty.SecurityDescriptor^.Dacl);
    for i := 0 to len - 1 do
      Insert(fProperty.SecurityDescriptor^.Dacl[i].Sid, rawSidArr, i);

    Filter := '(|';
    for acl in fProperty.SecurityDescriptor^.Dacl do
      Filter := FormatUtf8('%(objectSid=%)', [Filter, RawSidToText(acl.Sid)]);
    Filter := FormatUtf8('%)', [Filter]);
    sids.init();
    fProperty.LdapClient.SearchBegin();
    try
      fProperty.LdapClient.SearchScope := lssWholeSubtree;
      repeat
        if not fProperty.LdapClient.Search(fProperty.LdapClient.DefaultDN(), False, Filter, ['name', 'objectSid']) then
        begin
          ShowLdapSearchError(fProperty.LdapClient);
          Exit;
        end;

        for SearchResult in fProperty.LdapClient.SearchResult.Items do
        begin
          if not Assigned(SearchResult) then
            continue;
          sids.AddValue(SearchResult.Find('objectSid').GetReadable(), SearchResult.Find('name').GetReadable());
        end;
      until fProperty.LdapClient.SearchCookie = '';
    finally
      fProperty.LdapClient.SearchEnd;
    end;
    for i := 0 to len - 1 do
    begin
      namedSid := fProperty.SecurityDescriptor^.Dacl[i].SidText();
      if TisGrid_SecurityListUser.Data.SearchItemByProp('sid', namedSid, True) <> -1 then
        continue;
      newItem.AddValue('sid', namedSid);
      namedSid := RawSidToText(fProperty.SecurityDescriptor^.Dacl[i].Sid);
      if SidToKnown(namedSid) <> wksNull then
        newItem.AddValue('name', WELL_KNOWN_SID_NAMES[SidToKnown(namedSid)])
      else if sids.Exists(namedSid) then
        newItem.AddValue('name', sids.S[namedSid])
      else
        newItem.AddValue('name', namedSid);
      TisGrid_SecurityListUser.Data.AddItem(newItem);
      newItem.clear();
    end;
  finally
    TisGrid_SecurityListUser.EndUpdate();
    TisGrid_SecurityListUser.LoadData();
  end;
end;

procedure TFrmPropertySecurity.SecurityFillRightGrid(Sid, SidName: RawUtf8);
var
  secAccess: TSecAccess;
  AccessRow: TDocVariantData;
  allow, deny: Boolean;
  ace: TSecAce;
begin
  // header
  TisGrid_SecurityListRight.FindColumnByPropertyName('name').Text := FormatUtf8(rsTitlePermissionsFor, [SidName]);

  // data
  TisGrid_SecurityListRight.Clear();
  TisGrid_SecurityListRight.BeginUpdate();
  try
    AccessRow.init([dvoCheckForDuplicatedNames]);
    // One Row for each SEC_ACCESS
    for secAccess in SEC_ACCESS do
    begin
      AccessRow.AddValue('id', secAccess); // Is this Used ?
      AccessRow.AddValue('name', SEC_ACCESS_NAMES[secAccess]);
      allow := False;
      deny := False;
      for ace in fProperty.SecurityDescriptor^.Dacl do
      begin
        if (ace.SidText() <> sid) then        // look only for selected Ace
          continue;
        if not (secAccess in ace.mask) then // search the correct SEC_ACCESS for the row
          continue;
        case ace.AceType of
          satObjectAccessAllowed,
          satAccessAllowed:
            allow := True;
          satObjectAccessDenied,
          satAccessDenied:
            deny := True;
        end;
      end;
      AccessRow.AddValue('allow', allow);
      AccessRow.AddValue('deny', deny);
      TisGrid_SecurityListRight.Data.AddItem(AccessRow);
      AccessRow.Clear();
    end;
  finally
    TisGrid_SecurityListRight.EndUpdate();
    TisGrid_SecurityListRight.LoadData();
  end;
end;

constructor TFrmPropertySecurity.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);

  Caption := 'Security';
end;

procedure TFrmPropertySecurity.Update(Props: TProperty);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Update', Self);

  fProperty := Props;

  SecurityFillListUser();
end;

end.

