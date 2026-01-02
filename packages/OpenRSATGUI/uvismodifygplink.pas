unit uvismodifygplink;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  StdCtrls,
  VirtualTrees,
  {$IFDEF WINDOWS}
  ActiveX,
  {$ELSE}
  FakeActiveX,
  {$ENDIF WINDOWS}
  ExtCtrls,
  Menus,
  ActnList,
  Buttons,
  tis.ui.grid.core,
  tis.ui.searchedit,
  mormot.core.base,
  mormot.core.variants,
  mormot.net.ldap,
  ugplink,
  ufrmrsat;

type

  { TVisModifyGPLink }

  TVisModifyGPLink = class(TForm)
    Action_Apply: TAction;
    Action_OK: TAction;
    Action_EnableGPO: TAction;
    Action_DisableGPO: TAction;
    Action_EnforceGPO: TAction;
    ActionList1: TActionList;
    BitBtn_OK: TBitBtn;
    BitBtn_Apply: TBitBtn;
    BitBtn_Cancel: TBitBtn;
    Edit1: TEdit;
    Label1: TLabel;
    Label_LinkedGPO: TLabel;
    Label_AvailableGPO: TLabel;
    MenuItem_EnableGPO: TMenuItem;
    MenuItem_DisableGPO: TMenuItem;
    MenuItem_EnforceGPO: TMenuItem;
    Panel_Top: TPanel;
    Panel_Client: TPanel;
    Panel_Bottom: TPanel;
    Panel_Right: TPanel;
    Panel_Left: TPanel;
    PopupMenu1: TPopupMenu;
    Splitter1: TSplitter;
    TisGrid_LinkedGPO: TTisGrid;
    TisGrid_AvailableGPO: TTisGrid;
    TisSearchEdit1: TTisSearchEdit;
    TisSearchEdit2: TTisSearchEdit;
    procedure Action_ApplyExecute(Sender: TObject);
    procedure Action_DisableGPOExecute(Sender: TObject);
    procedure Action_EnableGPOExecute(Sender: TObject);
    procedure Action_EnforceGPOExecute(Sender: TObject);
    procedure Action_OKExecute(Sender: TObject);
    procedure TisGrid_AvailableGPOBeforeDeleteRows(aSender: TTisGrid;
      aRows: PDocVariantData; var aAskUser, aAbort: Boolean);
    procedure TisGrid_AvailableGPODragDrop(Sender: TBaseVirtualTree;
      Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
      Shift: TShiftState; const Pt: TPoint; var Effect: LongWord;
      Mode: TDropMode);
    procedure TisGrid_AvailableGPODragOver(Sender: TBaseVirtualTree;
      Source: TObject; Shift: TShiftState; State: TDragState; const Pt: TPoint;
      Mode: TDropMode; var Effect: LongWord; var Accept: Boolean);
    procedure TisGrid_LinkedGPOBeforeDeleteRows(aSender: TTisGrid;
      aRows: PDocVariantData; var aAskUser, aAbort: Boolean);
    procedure TisGrid_LinkedGPODragDrop(Sender: TBaseVirtualTree;
      Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
      Shift: TShiftState; const Pt: TPoint; var Effect: LongWord;
      Mode: TDropMode);
    procedure TisGrid_LinkedGPODragOver(Sender: TBaseVirtualTree;
      Source: TObject; Shift: TShiftState; State: TDragState; const Pt: TPoint;
      Mode: TDropMode; var Effect: LongWord; var Accept: Boolean);
    procedure TisGrid_LinkedGPOGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure TisSearchEdit1Search(Sender: TObject; const aText: string);
    procedure TisSearchEdit2Search(Sender: TObject; const aText: string);
  private
    fGroupPolicyContainerArr: TLdapResultObjArray;
    fGPLinkArr: TGPLinkDynArr;

    function GetGPLinkArr: TGPLinkDynArr;
    procedure SetGPLinkArr(AValue: TGPLinkDynArr);
    function GetDistinguishedName: RawUtf8;
    procedure SetDistinguishedName(AValue: RawUtf8);
    procedure RefreshGroupPolicyContainerArr;
    procedure RefreshDistinguishedNameGPLinkArr;
    procedure UpdateGrids;

    procedure ChangeGPOFlag(Flag: Integer);

    property GPLinkArr: TGPLinkDynArr read GetGPLinkArr write SetGPLinkArr;
  public
    constructor Create(TheOwner: TComponent; ADistinguishedName: RawUtf8); reintroduce;
    destructor Destroy; override;

    property DistinguishedName: RawUtf8 read GetDistinguishedName write SetDistinguishedName;
  end;

implementation
uses
  mormot.core.text,
  ucoredatamodule,
  ursatldapclientui;

{$R *.lfm}

{ TVisModifyGPLink }

procedure TVisModifyGPLink.TisGrid_AvailableGPODragOver(
  Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState;
  State: TDragState; const Pt: TPoint; Mode: TDropMode; var Effect: LongWord;
  var Accept: Boolean);
begin
  Accept := Source = TisGrid_LinkedGPO;
end;

procedure TVisModifyGPLink.TisGrid_LinkedGPOBeforeDeleteRows(aSender: TTisGrid;
  aRows: PDocVariantData; var aAskUser, aAbort: Boolean);
begin
  aAskUser := False;
end;

procedure TVisModifyGPLink.TisGrid_AvailableGPODragDrop(
  Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject;
  Formats: TFormatArray; Shift: TShiftState; const Pt: TPoint;
  var Effect: LongWord; Mode: TDropMode);
var
  Row: PDocVariantData;
begin
  for Row in TisGrid_LinkedGPO.SelectedRows.Objects do
    TisGrid_AvailableGPO.Data.AddItem(Row^);
  TisGrid_LinkedGPO.DeleteSelectedRows;
  TisGrid_AvailableGPO.LoadData;
end;

procedure TVisModifyGPLink.TisGrid_AvailableGPOBeforeDeleteRows(
  aSender: TTisGrid; aRows: PDocVariantData; var aAskUser, aAbort: Boolean);
begin
  aAskUser := False;
end;

procedure TVisModifyGPLink.Action_EnableGPOExecute(Sender: TObject);
begin
  ChangeGPOFlag(0);
end;

procedure TVisModifyGPLink.Action_EnforceGPOExecute(Sender: TObject);
begin
  ChangeGPOFlag(2);
end;

procedure TVisModifyGPLink.Action_OKExecute(Sender: TObject);
begin
  Action_Apply.Execute;
end;

procedure TVisModifyGPLink.Action_DisableGPOExecute(Sender: TObject);
begin
  ChangeGPOFlag(1);
end;

procedure TVisModifyGPLink.Action_ApplyExecute(Sender: TObject);
var
  NewGPLinkArr: TGPLinkDynArr;
  NewGPLink: TGPLink;
  Count: Integer;
  Row: PDocVariantData;
  GPLink: RawUtf8;
begin
  NewGPLinkArr := [];
  Count := 0;
  for Row in TisGrid_LinkedGPO.Data.Objects do
  begin
    if not Assigned(Row) then
      continue;
    NewGPLink.DistinguishedName := Row^.S['distinguishedName'];
    NewGPLink.Flag := Row^.I['flag'];
    Insert(NewGPLink, NewGPLinkArr, Count);
    Inc(Count);
  end;
  GPLink := GPLinkArrToGPLink(NewGPLinkArr);
  if not FrmRSAT.LdapClient.Modify(Edit1.Text, lmoReplace, 'gPLink', GPLink) then
  begin
    ShowLdapModifyError(FrmRSAT.LdapClient);
    Exit;
  end;
end;

procedure TVisModifyGPLink.TisGrid_LinkedGPODragDrop(Sender: TBaseVirtualTree;
  Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
  Shift: TShiftState; const Pt: TPoint; var Effect: LongWord; Mode: TDropMode);
var
  Row: PDocVariantData;
begin
  for Row in TisGrid_AvailableGPO.SelectedRows.Objects do
    TisGrid_LinkedGPO.Data.AddItem(Row^);
  TisGrid_AvailableGPO.DeleteSelectedRows;
  TisGrid_LinkedGPO.LoadData();
end;

procedure TVisModifyGPLink.TisGrid_LinkedGPODragOver(Sender: TBaseVirtualTree;
  Source: TObject; Shift: TShiftState; State: TDragState; const Pt: TPoint;
  Mode: TDropMode; var Effect: LongWord; var Accept: Boolean);
begin
  Accept := Source = TisGrid_AvailableGPO;
end;

procedure TVisModifyGPLink.TisGrid_LinkedGPOGetImageIndex(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
var
  NodeData: PDocVariantData;
begin
  if ImageIndex >= 0 then
    Exit;
  NodeData := TisGrid_LinkedGPO.GetNodeAsPDocVariantData(Node);
  if not Assigned(NodeData) or not NodeData^.Exists('flag') then
    Exit;
  case NodeData^.I['flag'] of
    0: ImageIndex := Ord(ileADGPOEnabled);
    1: ImageIndex := Ord(ileADGPODisabled);
    2: ImageIndex := Ord(ileADGPOEnforced);
  end;
end;

procedure TVisModifyGPLink.TisSearchEdit1Search(Sender: TObject;
  const aText: string);
begin
  UpdateGrids;
end;

procedure TVisModifyGPLink.TisSearchEdit2Search(Sender: TObject;
  const aText: string);
begin
  UpdateGrids;
end;

function TVisModifyGPLink.GetGPLinkArr: TGPLinkDynArr;
begin
  result := fGPLinkArr;
end;

procedure TVisModifyGPLink.SetGPLinkArr(AValue: TGPLinkDynArr);
begin
  if GPLinkArr = AValue then
    Exit;
  fGPLinkArr := AValue;

  UpdateGrids;
end;

function TVisModifyGPLink.GetDistinguishedName: RawUtf8;
begin
  result := Edit1.Text;
end;

procedure TVisModifyGPLink.SetDistinguishedName(AValue: RawUtf8);
begin
  if AValue = Edit1.Text then
    Exit;
  Edit1.Text := AValue;

  RefreshDistinguishedNameGPLinkArr;
end;

procedure TVisModifyGPLink.RefreshGroupPolicyContainerArr;
var
  SearchResult: TLdapResult;
  Count: Integer;
begin
  Count := 0;
  FrmRSAT.LdapClient.SearchBegin();
  try
    FrmRSAT.LdapClient.SearchScope := lssSingleLevel;
    repeat
      if not FrmRSAT.LdapClient.Search(FormatUtf8('CN=Policies,CN=System,%', [FrmRSAT.LdapClient.DefaultDN]), False, 'objectClass=groupPolicyContainer', ['displayName']) then
      begin
        ShowLdapSearchError(FrmRSAT.LdapClient);
        Exit;
      end;

      SetLength(fGroupPolicyContainerArr, Count + FrmRSAT.LdapClient.SearchResult.Count);
      for SearchResult in FrmRSAT.LdapClient.SearchResult.Items do
      begin
        if not Assigned(SearchResult) then
          continue;
        fGroupPolicyContainerArr[Count] := TLdapResult(SearchResult.Clone);
        Inc(Count);
      end;
    until FrmRSAT.LdapClient.SearchCookie = '';
  finally
    FrmRSAT.LdapClient.SearchEnd;
  end;
end;

procedure TVisModifyGPLink.RefreshDistinguishedNameGPLinkArr;
var
  GPLinkAttribute: TLdapAttribute;
begin
  GPLinkAttribute := FrmRSAT.LdapClient.SearchObject(Edit1.Text, '', 'gPLink');
  GPLinkArr := GPLinkToGPLinkArr(GPLinkAttribute.GetReadable());
end;

procedure TVisModifyGPLink.UpdateGrids;
var
  GroupPolicyContainer: TLdapResult;
  GPLink: TGPLink;
  Found: Boolean;
  Row: TDocVariantData;
begin
  TisGrid_LinkedGPO.Clear;
  TisGrid_AvailableGPO.Clear;
  TisGrid_LinkedGPO.BeginUpdate;
  TisGrid_AvailableGPO.BeginUpdate;
  try
    Row.Init();
    for GroupPolicyContainer in fGroupPolicyContainerArr do
    begin
      Found := False;
      Row.AddValue('name', GroupPolicyContainer.Find('displayName').GetReadable());
      Row.AddValue('distinguishedName', GroupPolicyContainer.ObjectName);
      Row.AddValue('flag', 0);
      for GPLink in GPLinkArr do
      begin
        if String(GPLink.DistinguishedName).ToLower = String(GroupPolicyContainer.ObjectName).ToLower then
        begin
          Row.AddOrUpdateValue('flag', GPLink.Flag);
          Found := True;
          Break;
        end;
      end;
      if Found then
      begin
        if (TisSearchEdit1.Text = '') or Row.S['name'].ToLower.Contains(String(TisSearchEdit1.Text).ToLower) then
          TisGrid_LinkedGPO.Data.AddItem(Row);
      end
      else
      begin
        if (TisSearchEdit2.Text = '') or Row.S['name'].ToLower.Contains(String(TisSearchEdit2.Text).ToLower) then
          TisGrid_AvailableGPO.Data.AddItem(Row);
      end;
      Row.Clear;
    end;
  finally
    TisGrid_AvailableGPO.EndUpdate;
    TisGrid_LinkedGPO.EndUpdate;
    TisGrid_AvailableGPO.LoadData();
    TisGrid_LinkedGPO.LoadData();
  end;
end;

procedure TVisModifyGPLink.ChangeGPOFlag(Flag: Integer);
var
  Row: PDocVariantData;
begin
  for Row in TisGrid_LinkedGPO.SelectedObjects do
    if Assigned(Row) then
      Row^.AddOrUpdateValue('flag', Flag);
  TisGrid_LinkedGPO.LoadData();
end;

constructor TVisModifyGPLink.Create(TheOwner: TComponent;
  ADistinguishedName: RawUtf8);
begin
  Inherited Create(TheOwner);

  RefreshGroupPolicyContainerArr;
  DistinguishedName := ADistinguishedName;
end;

destructor TVisModifyGPLink.Destroy;
var
  i: Integer;
begin
  for i := 0 to Pred(Length(fGroupPolicyContainerArr)) do
    FreeAndNil(fGroupPolicyContainerArr[i]);
  inherited Destroy;
end;

end.

