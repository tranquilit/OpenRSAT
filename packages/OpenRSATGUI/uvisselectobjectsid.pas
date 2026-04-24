unit uvisselectobjectsid;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ExtCtrls,
  StdCtrls,
  Buttons, ActnList,
  mormot.core.base,
  mormot.core.log,
  mormot.core.text,
  mormot.core.variants,
  mormot.net.ldap,
  tis.ui.grid.core;

type

  { TVisSelectObjectSID }

  TVisSelectObjectSID = class(TForm)
    Action1: TAction;
    ActionList1: TActionList;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Timer1: TTimer;
    TisGrid1: TTisGrid;
    procedure Action1Execute(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    fLog: TSynLog;
    fMultiSelect: Boolean;
    fLdapClient: TLdapClient;
    function GetSelectedDistinguishedName: RawUtf8;
    function GetSelectedDistinguishedNames: TRawUtf8DynArray;
    function GetSelectedName: RawUtf8;
    function GetSelectedNames: TRawUtf8DynArray;
    function GetSelectedSid: RawUtf8;
    function GetSelectedSids: TRawUtf8DynArray;
    procedure SetLdapClient(AValue: TLdapClient);
    procedure SetMultiSelect(AValue: Boolean);

    procedure UpdateGrid(AFilter: RawUtf8);
    procedure UpdateGridFromKnownSID(AFilter: RawUtf8);
    procedure UpdateGridFromLdap(AFilter: RawUtf8);
    procedure AddResultToGrid(AResult: TLdapResultList);
  public
    constructor Create(TheOwner: TComponent); override;

    property MultiSelect: Boolean read fMultiSelect write SetMultiSelect;
    property LdapClient: TLdapClient read fLdapClient write SetLdapClient;

    property SelectedSids: TRawUtf8DynArray read GetSelectedSids;
    property SelectedSid: RawUtf8 read GetSelectedSid;
    property SelectedNames: TRawUtf8DynArray read GetSelectedNames;
    property SelectedName: RawUtf8 read GetSelectedName;
    property SelectedDistinguishedNames: TRawUtf8DynArray read GetSelectedDistinguishedNames;
    property SelectedDistinguishedName: RawUtf8 read GetSelectedDistinguishedName;
  end;

implementation
uses
  VirtualTrees,
  mormot.core.os.security,
  ucommon;

{$R *.lfm}

{ TVisSelectObjectSID }

procedure TVisSelectObjectSID.FormShow(Sender: TObject);
var
  SelectionOptions: TVTSelectionOptions;
begin
  if not Assigned(LdapClient) then
    raise Exception.Create('Missing LDAPClient.');

  SelectionOptions := TisGrid1.TreeOptions.SelectionOptions;
  if MultiSelect then
    Include(SelectionOptions, toMultiSelect)
  else
    Exclude(SelectionOptions, toMultiSelect);
  TisGrid1.TreeOptions.SelectionOptions := SelectionOptions;
  Action1.Execute;
end;

procedure TVisSelectObjectSID.Action1Execute(Sender: TObject);
begin
  UpdateGrid(ComboBox1.Text);
end;

procedure TVisSelectObjectSID.ComboBox1Change(Sender: TObject);
begin
  Timer1.Enabled := False;
  Timer1.Enabled := True;
end;

procedure TVisSelectObjectSID.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;

  Action1.Execute;
end;

procedure TVisSelectObjectSID.SetLdapClient(AValue: TLdapClient);
begin
  if fLdapClient = AValue then
    Exit;
  fLdapClient := AValue;
end;

function TVisSelectObjectSID.GetSelectedDistinguishedName: RawUtf8;
begin
  result := '';
  if MultiSelect then
    Exit;
  if not Assigned(TisGrid1.FocusedRow) then
    Exit;
  if not TisGrid1.FocusedRow^.Exists('distinguishedName') then
    Exit;
  result := TisGrid1.FocusedRow^.U['distinguishedName'];
end;

function TVisSelectObjectSID.GetSelectedDistinguishedNames: TRawUtf8DynArray;
begin
  result := nil;

  if not MultiSelect then
    Exit;
  raise Exception.Create('Not implemented.');
end;

function TVisSelectObjectSID.GetSelectedName: RawUtf8;
begin
  result := '';
  if MultiSelect then
    Exit;

  if not Assigned(TisGrid1.FocusedRow) then
    Exit;

  if not TisGrid1.FocusedRow^.Exists('cn') then
    Exit;

  result := TisGrid1.FocusedRow^.U['cn'];
end;

function TVisSelectObjectSID.GetSelectedNames: TRawUtf8DynArray;
begin
  result := nil;
  raise Exception.Create('Not implemented.');
end;

function TVisSelectObjectSID.GetSelectedSid: RawUtf8;
begin
  result := '';

  if MultiSelect then
    Exit;

  if not Assigned(TisGrid1.FocusedRow) then
    Exit;

  if not TisGrid1.FocusedRow^.Exists('objectSid') then
    Exit;

  result := TisGrid1.FocusedRow^.U['objectSid'];
end;

function TVisSelectObjectSID.GetSelectedSids: TRawUtf8DynArray;
var
  i: Integer;
begin
  result := nil;

  if TisGrid1.SelectedCount <= 0 then
    Exit;

  SetLength(result, TisGrid1.SelectedCount);
  for i := 0 to Pred(TisGrid1.SelectedCount) do
  begin
    if not TisGrid1.SelectedRows._[i]^.Exists('objectSid') then
      continue;
    result[i] := TisGrid1.SelectedRows._[i]^.U['objectSid'];
  end;
end;

procedure TVisSelectObjectSID.SetMultiSelect(AValue: Boolean);
begin
  if fMultiSelect = AValue then
    Exit;
  fMultiSelect := AValue;
end;

procedure TVisSelectObjectSID.UpdateGrid(AFilter: RawUtf8);
begin
  TisGrid1.Clear;
  UpdateGridFromKnownSID(AFilter);
  UpdateGridFromLdap(AFilter);
end;

procedure TVisSelectObjectSID.UpdateGridFromKnownSID(AFilter: RawUtf8);
var
  NewRow: TDocVariantData;
  ksid: TWellKnownSid;
begin
  TisGrid1.BeginUpdate;
  try
    NewRow.Init(JSON_FAST);
    for ksid := wksWorld to High(TWellKnownSid) do
    begin
      if not WELL_KNOWN_SID_NAMES[ksid].ToLower.Contains(LowerCase(AFilter)) and not String(RawSidToText(KnownRawSid(ksid))).ToLower.Contains(LowerCase(AFilter)) then
        continue;
      NewRow.AddOrUpdateValue('cn', WELL_KNOWN_SID_NAMES[ksid]);
      NewRow.AddOrUpdateValue('objectSid', RawSidToText(KnownRawSid(ksid)));
      TisGrid1.Data.AddItem(NewRow);
      NewRow.Clear;
    end;
  finally
    TisGrid1.EndUpdate;
    TisGrid1.LoadData();
  end;
end;

procedure TVisSelectObjectSID.UpdateGridFromLdap(AFilter: RawUtf8);
var
  Filter: RawUtf8;
begin
  Filter := '';
  if AFilter <> '' then
    Filter := FormatUtf8('(|(cn=*%*)(objectSid=*%*))', [LdapEscape(AFilter), LdapEscape(AFilter)]);
  Filter := FormatUtf8('(&(objectSid=*)%)', [Filter]);

  LdapClient.SearchBegin();
  try
    LdapClient.SearchScope := lssWholeSubtree;
    if not LdapClient.Search(LdapClient.DefaultDN, False, Filter, ['distinguishedName', 'objectSid', 'cn']) then
      Exit;
    AddResultToGrid(LdapClient.SearchResult);
  finally
    LdapClient.SearchEnd;
  end;
end;

procedure TVisSelectObjectSID.AddResultToGrid(AResult: TLdapResultList);
var
  NewRow: TDocVariantData;
  SearchResult: TLdapResult;
  Attr: TLdapAttribute;
begin
  TisGrid1.BeginUpdate;
  try
    NewRow.Init(JSON_FAST);
    for SearchResult in AResult.Items do
    begin
      if not Assigned(SearchResult) then
        continue;
      for Attr in SearchResult.Attributes.Items do
      begin
        if not Assigned(Attr) then
          continue;
        NewRow.AddOrUpdateValue(Attr.AttributeName, Attr.GetReadable());
      end;
      TisGrid1.Data.AddItem(NewRow);
      NewRow.Clear;
    end;
  finally
    TisGrid1.EndUpdate;
    TisGrid1.LoadData();
  end;
end;

constructor TVisSelectObjectSID.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fLog := TSynLog.Add;

  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);

  fLdapClient := nil;
  fMultiSelect := False;
  Timer1.Enabled := False;
end;

end.

