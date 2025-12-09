unit ufrmpropertymemberof;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  Dialogs,
  SysUtils,
  Forms,
  Controls,
  StdCtrls,
  ExtCtrls,
  ActnList,
  buttons,
  mormot.core.base,
  mormot.core.log,
  mormot.core.variants,
  mormot.net.ldap,
  tis.ui.grid.core,
  ursatldapclient,
  uproperty, VirtualTrees,
  upropertyframe;

type

  { TPropertyMemberOf }

  TPropertyMemberOf = class
  private
    fLog: TSynLog;
  public
    constructor Create;
    destructor Destroy; override;

    function GetMemberOf(Props: TProperty): TRawUtf8DynArray;
    procedure GetMemberOfInfo(MemberOf: RawUtf8; out MemberOfName: RawUtf8; out MemberOfCanonicalName: RawUtf8);

    function GetPrimaryGroupDN(Props: TProperty): RawUtf8;
    function GetPrimaryGroupName(PrimaryGroupDN: RawUtf8): RawUtf8;

    function HasPrimaryGroup(PrimaryGroupDN: RawUtf8): Boolean;
  end;

  { TFrmPropertyMemberOf }

  TFrmPropertyMemberOf = class(TPropertyFrame)
    Action_Add: TAction;
    Action_Delete: TAction;
    Action_PrimaryGroup: TAction;
    ActionList1: TActionList;
    Btn_Add: TBitBtn;
    Btn_DefinePrimaryGroup: TBitBtn;
    Btn_Delete: TBitBtn;
    Edit_PrimaryGroup: TEdit;
    Label_mof_noPrimaryGroup: TLabel;
    Label_PrimaryGroupKey: TLabel;
    Label_mof_title: TLabel;
    Line_mof: TShape;
    Grid_MemberOf: TTisGrid;
    Panel_Buttons: TPanel;
    Panel_NoPrimaryGroup: TPanel;
    Panel_PrimaryGroup: TPanel;
    Label_DefinePrimaryGroup: TLabel;
    procedure Action_AddExecute(Sender: TObject);
    procedure Action_AddUpdate(Sender: TObject);
    procedure Action_DeleteExecute(Sender: TObject);
    procedure Action_DeleteUpdate(Sender: TObject);
    procedure Action_PrimaryGroupExecute(Sender: TObject);
    procedure Action_PrimaryGroupUpdate(Sender: TObject);
    procedure Grid_MemberOfBeforeDeleteRows(aSender: TTisGrid;
      aRows: PDocVariantData; var aAskUser, aAbort: Boolean);
    procedure Grid_MemberOfDblClick(Sender: TObject);
  private
    fLog: TSynLog;
    fPropertyMemberOf: TPropertyMemberOf;
    fProperty: TProperty;

    procedure FillMemberOf();
    procedure FillPrimaryGroup();

    function GetGroupToAdd(): TRawUtf8DynArray;
    function GetGroupToDelete(): TRawUtf8DynArray;

    procedure AddRowFromDN(MemberOf, MemberOfName, MemberOfCanonicalName: RawUtf8);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(Props: TProperty); override;

  end;

implementation
uses
  mormot.core.os.security,
  mormot.core.text,
  ucommon,
  ursatldapclientui,
  uOmniselect,
  ufrmrsat;

{$R *.lfm}

{ TPropertyMemberOf }

constructor TPropertyMemberOf.Create;
begin
  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);
end;

destructor TPropertyMemberOf.Destroy;
begin
  inherited Destroy;
end;

function TPropertyMemberOf.GetMemberOf(Props: TProperty): TRawUtf8DynArray;
begin
  result := nil;

  if not Assigned(Props) or not Assigned(Props.Attributes) then
    Exit;

  result := Props.Attributes.Find('memberOf').GetAllReadable;
end;

procedure TPropertyMemberOf.GetMemberOfInfo(MemberOf: RawUtf8; out
  MemberOfName: RawUtf8; out MemberOfCanonicalName: RawUtf8);
var
  CanonicalName: RawUtf8;
  CanonicalNameSplitted: TStringArray;
  Count: Integer;
begin
  CanonicalName := DNToCN(MemberOf);
  CanonicalNameSplitted := String(CanonicalName).Split('/');

  Count := Length(CanonicalNameSplitted);
  MemberOfName := CanonicalNameSplitted[Count - 1];
  Delete(CanonicalNameSplitted, Count - 1, 1);
  MemberOfCanonicalName := String.Join('/', CanonicalNameSplitted);
end;

function TPropertyMemberOf.GetPrimaryGroupDN(Props: TProperty): RawUtf8;
var
  PrimaryGroupID: Integer;
  DomainSID: RawSid;
  P: Pointer;
  SID: TSid;
  Filter: RawUtf8;
  PrimaryGroupAttribute: TLdapAttribute;
begin
  result := '';

  // Fast exit
  if not Assigned(Props) or not Assigned(Props.Attributes) or not Assigned(Props.LdapClient) or not Props.LdapClient.Connected then
    Exit;

  PrimaryGroupID := Utf8ToInteger(Props.Attributes.GetByName('primaryGroupID'), -1);

  // Fast exit
  if (primaryGroupID = -1) then
    Exit;

  // Create SID
  DomainSID := RawSidToText(Props.LdapClient.DomainSid);
  P := @DomainSID[1];
  TextToSid(P, SID);
  Inc(SID.SubAuthorityCount);
  SID.SubAuthority[SID.SubAuthorityCount - 1] := primaryGroupID;

  // get primary group distinguished name
  Filter := FormatUtf8('(objectSid=%)', [LdapEscape(SidToText(@SID))]);
  PrimaryGroupAttribute := Props.LdapClient.SearchObject(Props.LdapClient.DefaultDN(), Filter, 'distinguishedName', lssWholeSubtree);
  if not Assigned(PrimaryGroupAttribute) then
  begin
    ShowLdapSearchError(Props.LdapClient);
    Exit;
  end;
  result := PrimaryGroupAttribute.GetReadable();
end;

function TPropertyMemberOf.GetPrimaryGroupName(PrimaryGroupDN: RawUtf8
  ): RawUtf8;
var
  Splitted, SplittedName: TStringArray;
begin
  Result := '';

  Splitted := String(PrimaryGroupDN).Split(',');
  if not Assigned(Splitted) or (Length(Splitted) <= 0) then
    Exit;

  SplittedName := Splitted[0].Split('=');
  if not Assigned(SplittedName) or (Length(SplittedName) <> 2) then
    Exit;
  Result := SplittedName[1];
end;

function TPropertyMemberOf.HasPrimaryGroup(PrimaryGroupDN: RawUtf8): Boolean;
begin
  result := (PrimaryGroupDN <> '');
end;

{ TFrmPropertyMemberOf }

constructor TFrmPropertyMemberOf.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fPropertyMemberOf := TPropertyMemberOf.Create;

  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);

  Caption := 'Member Of';
end;

destructor TFrmPropertyMemberOf.Destroy;
begin
  FreeAndNil(fPropertyMemberOf);

  inherited Destroy;
end;

procedure TFrmPropertyMemberOf.Action_AddExecute(Sender: TObject);
var
  GroupToAdd: TRawUtf8DynArray;
  MemberOf, MemberOfCanonicalName, MemberOfName: RawUtf8;
begin
  GroupToAdd := GetGroupToAdd();

  fProperty.AddMemberOf(GroupToAdd);

  Grid_MemberOf.BeginUpdate;
  try
    for MemberOf in GroupToAdd do
    begin
      if MemberOf = '' then
        continue;
      fPropertyMemberOf.GetMemberOfInfo(MemberOf, MemberOfName, MemberOfCanonicalName);
      AddRowFromDN(MemberOf, MemberOfName, MemberOfCanonicalName);
    end;
  finally
    Grid_MemberOf.LoadData();
    Grid_MemberOf.EndUpdate;
  end;
end;

procedure TFrmPropertyMemberOf.Action_AddUpdate(Sender: TObject);
begin
  Action_Add.Enabled := True;
end;

procedure TFrmPropertyMemberOf.Action_DeleteExecute(Sender: TObject);
var
  GroupToDelete: TRawUtf8DynArray;
  ModalResult: TModalResult;
begin
  ModalResult := MessageDlg('Delete selected memberOf', FormatUtf8('Do you want to remove % from the selected group(s)?', [fProperty.name]), mtWarning, mbYesNo, 0);

  if (ModalResult <> mrYes) then
    Exit;

  GroupToDelete := GetGroupToDelete();

  fProperty.DeleteMemberOf(GroupToDelete);

  Grid_MemberOf.DeleteSelectedRows;
end;

procedure TFrmPropertyMemberOf.Action_DeleteUpdate(Sender: TObject);
begin
  Action_Delete.Enabled := (Grid_MemberOf.SelectedCount > 0);
end;

procedure TFrmPropertyMemberOf.Action_PrimaryGroupExecute(Sender: TObject);
begin

end;

procedure TFrmPropertyMemberOf.Action_PrimaryGroupUpdate(Sender: TObject);
begin
  Action_PrimaryGroup.Enabled := False;
end;

procedure TFrmPropertyMemberOf.Grid_MemberOfBeforeDeleteRows(aSender: TTisGrid;
  aRows: PDocVariantData; var aAskUser, aAbort: Boolean);
begin
  aAskUser := False;
  aAskUser := False;
end;

procedure TFrmPropertyMemberOf.Grid_MemberOfDblClick(Sender: TObject);
begin
  if Assigned(FrmRSAT) and Assigned(Grid_MemberOf.FocusedRow) then
    FrmRSAT.OpenProperty(Grid_MemberOf.FocusedRow^.S['distinguishedName']);
end;

procedure TFrmPropertyMemberOf.FillMemberOf;
var
  MemberOfList: TRawUtf8DynArray;
  MemberOf, MemberOfName, MemberOfCanonicalName: RawUtf8;
begin
  if not Assigned(fProperty) then
    Exit;

  MemberOfList := fPropertyMemberOf.GetMemberOf(fProperty);
  Grid_MemberOf.BeginUpdate();
  try
    for MemberOf in MemberOfList do
    begin
      fPropertyMemberOf.GetMemberOfInfo(MemberOf, MemberOfName, MemberOfCanonicalName);
      AddRowFromDN(MemberOf, MemberOfName, MemberOfCanonicalName);
    end;
  finally
    Grid_MemberOf.LoadData();
    Grid_MemberOf.EndUpdate();
  end;
end;

procedure TFrmPropertyMemberOf.FillPrimaryGroup;
var
  HasPrimaryGroup: Boolean;
  PrimaryGroupDN, PrimaryGroupDNName, PrimaryGroupDNCanonicalName: RawUtf8;
begin
  PrimaryGroupDN := fPropertyMemberOf.GetPrimaryGroupDN(fProperty);

  HasPrimaryGroup := fPropertyMemberOf.HasPrimaryGroup(PrimaryGroupDN);
  Panel_PrimaryGroup.Enabled := HasPrimaryGroup;
  Panel_NoPrimaryGroup.Visible := not HasPrimaryGroup;
  Panel_NoPrimaryGroup.Enabled := not HasPrimaryGroup;

  Edit_PrimaryGroup.Text := fPropertyMemberOf.GetPrimaryGroupName(PrimaryGroupDN);

  if HasPrimaryGroup then
  begin
    fPropertyMemberOf.GetMemberOfInfo(PrimaryGroupDN, PrimaryGroupDNName, PrimaryGroupDNCanonicalName);
    AddRowFromDN(PrimaryGroupDN, PrimaryGroupDNName, PrimaryGroupDNCanonicalName);
    Grid_MemberOf.LoadData();
  end;
end;

function TFrmPropertyMemberOf.GetGroupToAdd(): TRawUtf8DynArray;
var
  Filter: RawUtf8;
  VisSelect: TVisOmniselect;
  Row: PDocVariantData;
begin
  result := nil;

  // Exclude self being a member of a group
  // Do not use member=[self] cause it doesn't take care of the current modification.
  // Filter := FormatUtf8('(!(member=%))', [LdapEscape(fProperty.distinguishedName)]);
  Filter := '';
  for Row in Grid_MemberOf.Data.Objects do
  begin
    if not Assigned(Row) then
      continue;
    Filter := FormatUtf8('%(distinguishedName=%)', [Filter, LdapEscape(Row^.S['distinguishedName'])]);
  end;
  if Filter <> '' then
    Filter := FormatUtf8('(!(|%))', [Filter]);

  // Select groups to add
  VisSelect := TVisOmniselect.Create(self, fProperty.LdapClient, ['group'], fProperty.LdapClient.DefaultDN(), True, Filter);
  try
    VisSelect.Caption := rsTitleSelectGroups;
    if VisSelect.ShowModal() <> mrOK then
      Exit;
    result := VisSelect.SelectedObjects;
  finally
    FreeAndNil(VisSelect);
  end;
end;

function TFrmPropertyMemberOf.GetGroupToDelete(): TRawUtf8DynArray;
var
  SelectedRow: PDocVariantData;
  GroupToDelete: TRawUtf8DynArray;
  i: Integer;
  NextNode, Node: PVirtualNode;
begin
  result := [];

  SetLength(GroupToDelete, Grid_MemberOf.SelectedCount);
  i := 0;
  NextNode := Grid_MemberOf.GetFirstSelected();
  while Assigned(NextNode) do
  begin
    Node := NextNode;
    SelectedRow := Grid_MemberOf.GetNodeAsPDocVariantData(Node);
    NextNode := Grid_MemberOf.GetNextSelected(Node);

    if not Assigned(SelectedRow) then
      continue;
    if SelectedRow^.S['distinguishedName'] = fPropertyMemberOf.GetPrimaryGroupDN(fProperty) then
    begin
      Grid_MemberOf.Selected[Node] := False;
      MessageDlg('Active Directory Domain Services', 'The primary group cannot bo removed. Set another group as primary if you want to remove this one.', mtWarning, [mbOK], 0);
      continue;
    end;
    GroupToDelete[i] := SelectedRow^.S['distinguishedName'];
    Inc(i);
  end;
  result := GroupToDelete;
end;

procedure TFrmPropertyMemberOf.AddRowFromDN(MemberOf, MemberOfName,
  MemberOfCanonicalName: RawUtf8);
var
  Row: TDocVariantData;
begin
  Row.Init();
  Row.S['name'] := MemberOfName;
  Row.S['ADSF'] := MemberOfCanonicalName;
  Row.S['distinguishedName'] := MemberOf;
  Grid_MemberOf.Data.AddItem(Row);
end;

procedure TFrmPropertyMemberOf.Update(Props: TProperty);
begin
  fProperty := Props;
  FillMemberOf;
  FillPrimaryGroup;
end;

end.

