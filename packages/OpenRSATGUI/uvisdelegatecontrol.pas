unit uvisdelegatecontrol;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ComCtrls,
  ExtCtrls,
  StdCtrls,
  Buttons,
  ActnList,
  CheckLst,
  mormot.core.base,
  mormot.net.ldap,
  uinterfacecore;

type

  { TVisDelegateControl }

  TVisDelegateControl = class(TForm)
    Action_Add: TAction;
    Action_Remove: TAction;
    Action_Back: TAction;
    Action_Next: TAction;
    Action_Cancel: TAction;
    ActionList1: TActionList;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Btn_Finish: TBitBtn;
    Btn_Back: TBitBtn;
    Btn_Next: TBitBtn;
    Btn_Cancel: TBitBtn;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckListBox1: TCheckListBox;
    CheckListBox2: TCheckListBox;
    CheckListBox3: TCheckListBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    ListBox1: TListBox;
    Memo1: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    Panel_Bottom: TPanel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    TS_Permissions: TTabSheet;
    TS_ADObjectType: TTabSheet;
    TS_Welcome: TTabSheet;
    TS_UsersAndGroups: TTabSheet;
    TS_TasksToDelegate: TTabSheet;
    TS_Resume: TTabSheet;
    procedure Action_AddExecute(Sender: TObject);
    procedure Action_BackExecute(Sender: TObject);
    procedure Action_BackUpdate(Sender: TObject);
    procedure Action_NextExecute(Sender: TObject);
    procedure Action_NextUpdate(Sender: TObject);
    procedure Action_RemoveExecute(Sender: TObject);
    procedure Action_RemoveUpdate(Sender: TObject);
    procedure CheckBox3Change(Sender: TObject);
    procedure CheckBox4Change(Sender: TObject);
    procedure CheckBox5Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure RadioButton1Change(Sender: TObject);
    procedure RadioButton4Change(Sender: TObject);
    procedure TS_ADObjectTypeShow(Sender: TObject);
    procedure TS_PermissionsShow(Sender: TObject);
    procedure TS_ResumeShow(Sender: TObject);
  private
    fSelectedObject: RawUtf8;
    fBaseDN: RawUtf8;
    fCore: ICore;

    fUsersAndGroups: array of record
      sAMAccountName: RawUtf8;
      distinguishedName: RawUtf8;
      dc: RawUtf8;
      objectSid: RawUtf8;
    end;

    function CheckedCount(CheckListBox: TCheckListBox): Integer;
    procedure RefreshPermissionList;
  public
    constructor Create(TheOwner: TComponent; ACore: ICore); overload;
    procedure Execute;

    property SelectedObject: RawUtf8 read fSelectedObject write fSelectedObject;
    property BaseDN: RawUtf8 read fBaseDN write fBaseDN;

  end;

const
  CN_EXTENDED_RIGHTS: RawUtf8 = 'CN=Extended-Rights';

implementation

uses
  mormot.core.os.security,
  mormot.core.text,
  mormot.core.variants,
  ucommon,
  uOmniselect,
  ursatldapclientui;

{$R *.lfm}

{ TVisDelegateControl }

procedure TVisDelegateControl.Action_BackExecute(Sender: TObject);
begin
  if PageControl1.ActivePage = TS_UsersAndGroups then
    PageControl1.ActivePage := TS_Welcome
  else if PageControl1.ActivePage = TS_TasksToDelegate then
    PageControl1.ActivePage := TS_UsersAndGroups
  else if PageControl1.ActivePage = TS_ADObjectType then
    PageControl1.ActivePage := TS_TasksToDelegate
  else if PageControl1.ActivePage = TS_Permissions then
    PageControl1.ActivePage := TS_ADObjectType
  else if PageControl1.ActivePage = TS_Resume then
    if RadioButton1.Checked then
      PageControl1.ActivePage := TS_TasksToDelegate
    else
      PageControl1.ActivePage := TS_Permissions;

  Btn_Back.Visible := False;
  Btn_Finish.Visible := (PageControl1.ActivePage = TS_Resume);
  Btn_Next.Visible := not Btn_Finish.Visible;
  Btn_Back.Visible := True;
end;

procedure TVisDelegateControl.Action_AddExecute(Sender: TObject);
var
  vis: TVisOmniselect;
  Filter: RawUtf8;
  Item: String;
  ResItem: TLdapResult;
  Index: SizeInt;
begin
  vis := TVisOmniselect.Create(Self, fCore.LdapClient, ['user', 'group', 'computer']);
  try
    if Vis.ShowModal <> mrOK then
      Exit;
    Filter := '(|';
    for Item in vis.SelectedObjects do
      Filter := FormatUtf8('%(distinguishedName=%)', [Filter, LdapEscape(Item)]);
    Filter := FormatUtf8('%)', [Filter]);

    try
      fCore.LdapClient.SearchBegin();
      fCore.LdapClient.SearchScope := lssWholeSubtree;
      repeat
        if not fCore.LdapClient.Search(BaseDN, False, Filter, ['distinguishedName', 'objectSID', 'sAMAccountName']) then
        begin
          ShowLdapSearchError(fCore.LdapClient);
          Exit;
        end;

        Index := Length(fUsersAndGroups);
        SetLength(fUsersAndGroups, Index + fCore.LdapClient.SearchResult.Count);
        for ResItem in fCore.LdapClient.SearchResult.Items do
        begin
          fUsersAndGroups[Index].sAMAccountName := ResItem.Find('sAMAccountName').GetReadable();
          fUsersAndGroups[Index].distinguishedName := ResItem.Find('distinguishedName').GetReadable();
          fUsersAndGroups[Index].objectSid := ResItem.Find('objectSID').GetReadable();
          fUsersAndGroups[Index].dc := String(DNToCN(fUsersAndGroups[Index].distinguishedName)).Split('/')[0];
          ListBox1.Items.Add(fUsersAndGroups[Index].sAMAccountName);
          Inc(Index);
        end;
      until fCore.LdapClient.SearchCookie = '';
    finally
      fCore.LdapClient.SearchEnd;
    end;
    ListBox1.Refresh;
  finally
    FreeAndNil(vis);
  end;
  Action_Next.Update;
  if Btn_Next.Enabled then
    Btn_Next.SetFocus
  else
    Btn_Back.SetFocus;
end;

procedure TVisDelegateControl.Action_BackUpdate(Sender: TObject);
begin
  Action_Back.Enabled := Assigned(PageControl1.ActivePage) and (PageControl1.ActivePage <> TS_Welcome);
end;

procedure TVisDelegateControl.Action_NextExecute(Sender: TObject);
begin

  if PageControl1.ActivePage = TS_Welcome then
    PageControl1.ActivePage := TS_UsersAndGroups
  else if PageControl1.ActivePage = TS_UsersAndGroups then
    PageControl1.ActivePage := TS_TasksToDelegate
  else if PageControl1.ActivePage = TS_TasksToDelegate then
  begin
    if RadioButton2.Checked then
      PageControl1.ActivePage := TS_ADObjectType
    else
      PageControl1.ActivePage := TS_Resume;
  end
  else if PageControl1.ActivePage = TS_ADObjectType then
    PageControl1.ActivePage := TS_Permissions
  else if PageControl1.ActivePage = TS_Permissions then
    PageControl1.ActivePage := TS_Resume;

  Btn_Finish.Visible := (PageControl1.ActivePage = TS_Resume);
  Btn_Finish.Default := Btn_Finish.Visible;
  Btn_Next.Visible := not Btn_Finish.Visible;
end;

procedure TVisDelegateControl.Action_NextUpdate(Sender: TObject);
begin
  if (PageControl1.ActivePage = TS_Welcome) or (PageControl1.ActivePage = TS_Resume) then
  begin
    Action_Next.Enabled := True;
  end
  else if PageControl1.ActivePage = TS_UsersAndGroups then
  begin
    Action_Next.Enabled := (ListBox1.Count > 0);
  end
  else if PageControl1.ActivePage = TS_TasksToDelegate then
  begin
    Action_Next.Enabled := RadioButton2.Checked or (RadioButton1.Checked and (CheckedCount(CheckListBox1) > 0));
  end
  else if PageControl1.ActivePage = TS_ADObjectType then
  begin
    Action_Next.Enabled := RadioButton3.Checked or (RadioButton4.Checked and (CheckedCount(CheckListBox2) > 0));
  end
  else if PageControl1.ActivePage = TS_Permissions then
  begin
    Action_Next.Enabled := (CheckedCount(CheckListBox3) > 0);
  end;
  Btn_Next.Default := Action_Next.Enabled;
  Btn_Back.Default := not Action_Next.Enabled;
end;

procedure TVisDelegateControl.Action_RemoveExecute(Sender: TObject);
var
  i: Integer;
begin
  i := ListBox1.Count - 1;

  while i >= 0 do
  begin
    if ListBox1.Selected[i] then
    begin
      assert(fUsersAndGroups[i].sAMAccountName = ListBox1.Items[i], 'Name are different.');
      ListBox1.Items.Delete(i);
      Delete(fUsersAndGroups, i, 1);
    end;
    Dec(i);
  end;
end;

procedure TVisDelegateControl.Action_RemoveUpdate(Sender: TObject);
begin
  Action_Remove.Enabled := (ListBox1.SelCount > 0);
end;

procedure TVisDelegateControl.CheckBox3Change(Sender: TObject);
begin
  RefreshPermissionList;
end;

procedure TVisDelegateControl.CheckBox4Change(Sender: TObject);
begin
  RefreshPermissionList;
end;

procedure TVisDelegateControl.CheckBox5Change(Sender: TObject);
begin
  RefreshPermissionList;
end;

procedure TVisDelegateControl.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePage := TS_Welcome;
  CheckListBox1.ItemEnabled[5] := False;
  CheckListBox1.ItemEnabled[6] := False;
  CheckListBox1.ItemEnabled[8] := False;
end;

procedure TVisDelegateControl.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    27: Close;
  end;
end;

procedure TVisDelegateControl.RadioButton1Change(Sender: TObject);
begin
  CheckListBox1.Enabled := RadioButton1.Checked;
end;

procedure TVisDelegateControl.RadioButton4Change(Sender: TObject);
begin
  CheckListBox2.Enabled := RadioButton4.Checked;
  CheckBox1.Enabled := RadioButton4.Checked;
  CheckBox2.Enabled := RadioButton4.Checked;
end;

procedure TVisDelegateControl.TS_ADObjectTypeShow(Sender: TObject);
var
  SearchResult: TLdapResult;
begin
  if CheckListBox2.Count > 0 then
    Exit;

  CheckListBox2.Items.BeginUpdate;
  fCore.LdapClient.SearchBegin();
  try
    fCore.LdapClient.SearchScope := lssSingleLevel;

    repeat
      if not fCore.LdapClient.Search(Join(['CN=Schema,', fCore.LdapClient.ConfigDN]), False, '(|(objectClass=classSchema))', ['lDAPDisplayName', 'objectClass', 'schemaIDGUID']) then
      begin
        ShowLdapSearchError(fCore.LdapClient);
        Exit;
      end;

      for SearchResult in fCore.LdapClient.SearchResult.Items do
      begin
        if not Assigned(SearchResult) then
          continue;
        CheckListBox2.Items.Add(SearchResult.Find('lDAPDisplayName').GetReadable());
      end;
    until fCore.LdapClient.SearchCookie = '';
  finally
    fCore.LdapClient.SearchEnd;
    CheckListBox2.Items.EndUpdate;
  end;
end;

procedure TVisDelegateControl.TS_PermissionsShow(Sender: TObject);
var
  DisplayName: RawUtf8;
  item, SearchResult: TLdapResult;
begin
  if CheckListBox3.Count > 0 then
    Exit;

  CheckListBox3.Items.BeginUpdate;
  try
    fCore.LdapClient.SearchBegin();
    try
      fCore.LdapClient.SearchScope := lssSingleLevel;

      repeat
        if not fCore.LdapClient.Search(Join(['CN=Schema,', fCore.LdapClient.ConfigDN]), False, '', ['lDAPDisplayName', 'schemaIDGUID']) then
        begin
          ShowLdapSearchError(fCore.LdapClient);
          Exit;
        end;

        for SearchResult in fCore.LdapClient.SearchResult.Items do
        begin
          if not Assigned(SearchResult) then
            continue;
          CheckListBox3.Items.Add(SearchResult.Find('lDAPDisplayName').GetReadable());
        end;
      until fCore.LdapClient.SearchCookie = '';
    finally
      fCore.LdapClient.SearchEnd;
    end;

    fCore.LdapClient.SearchBegin();
    try
      fCore.LdapClient.SearchScope := lssSingleLevel;
      repeat
        if not fCore.LdapClient.Search(FormatUtf8('%,%', [CN_EXTENDED_RIGHTS, fCore.LdapClient.ConfigDN]), False, '', ['displayName', 'objectGUID']) then
        begin
          ShowLdapSearchError(fCore.LdapClient);
          Exit;
        end;
        for item in fCore.LdapClient.SearchResult.Items do
        begin
          DisplayName := item.Find('displayName').GetReadable();
          CheckListBox3.Items.Add(DisplayName);
        end;
      until fCore.LdapClient.SearchCookie = '';
    finally
      fCore.LdapClient.SearchEnd;
    end;
  finally
    CheckListBox3.Items.EndUpdate;
  end;
end;

procedure TVisDelegateControl.TS_ResumeShow(Sender: TObject);
  var
  i: Integer;
begin
  Memo1.Clear;
  Memo1.Lines.BeginUpdate;
  try
    Memo1.Lines.Add(rsDelegateControlFolder);
    Memo1.Lines.Add('');
    Memo1.Lines.Add('    %s', [DNToCN(SelectedObject)]);
    Memo1.Lines.Add('');
    Memo1.Lines.Add(rsDelegateControlObjects);
    Memo1.Lines.Add('');
    for i := 0 to high(fUsersAndGroups) do
      Memo1.Lines.Add('    %s', [fUsersAndGroups[i].sAMAccountName]);
    Memo1.Lines.Add('');
    Memo1.Lines.Add(rsDelegateControlTasks);
    Memo1.Lines.Add('');
    for i := 0 to CheckListBox1.Count - 1 do
      if CheckListBox1.Checked[i] then
        Memo1.Lines.Add('    %s', [CheckListBox1.Items[i]]);
    Memo1.Lines.Add('');
  finally
    Memo1.Lines.EndUpdate;
    Memo1.Update;
  end;
end;

function TVisDelegateControl.CheckedCount(CheckListBox: TCheckListBox): Integer;
var
  i: Integer;
begin
  result := 0;
  for i := 0 to CheckListBox.Count - 1 do
    if CheckListBox.Checked[i] then
      Inc(result);
end;

procedure TVisDelegateControl.RefreshPermissionList;
begin

end;

constructor TVisDelegateControl.Create(TheOwner: TComponent; ACore: ICore);
begin
  inherited Create(TheOwner);

  fCore := ACore;
end;

procedure TVisDelegateControl.Execute;
var
  res, NewAttr: TLdapAttribute;
  SecurityDescriptor: TSecurityDescriptor;
  NewAcl: TSecAcl;
  i: Integer;
  user, Filter: String;
  Item: TLdapResult;

  procedure CreateDeleteManage(NewAcl: PSecAcl; ObjectType: RawUtf8);
  var
    Index: Integer;
    ObjectDN: RawUtf8;
    guid: TGuid;
    LdapObject: TLdapAttribute;
  begin
    //ObjectDN := VisMain.Storage.GetSchemaObjectName('lDAPDisplayName', ObjectType);

    LdapObject := fCore.LdapClient.SearchObject(Join(['CN=Schema,', fCore.LdapClient.ConfigDN]), FormatUtf8('(lDAPDisplayName=%)', [LdapEscape(ObjectType)]), 'distinguishedName', lssWholeSubtree);
    if not Assigned(LdapObject) then
    begin
      ShowLdapSearchError(fCore.LdapClient);
      Exit;
    end;
    ObjectDN := LdapObject.GetReadable();

    LdapObject := fCore.LdapClient.SearchObject(ObjectDN, '', 'schemaIDGUID');
    if not Assigned(LdapObject) then
    begin
      ShowLdapSearchError(fCore.LdapClient);
      Exit;
    end;
    guid := PGuid(LdapObject.GetRaw())^;

    Index := Length(NewAcl^);
    SetLength(NewAcl^, Index + 2);
    NewAcl^[Index].AceType := satObjectAccessAllowed;
    NewAcl^[Index].Flags := [safContainerInherit];
    NewAcl^[Index].Mask := [samCreateChild, samDeleteChild];
    NewAcl^[Index].ObjectType := guid;
    NewAcl^[Index].InheritedObjectType := GUID_NULL;

    Inc(Index);
    NewAcl^[Index].AceType := satObjectAccessAllowed;
    NewAcl^[Index].Flags := [safContainerInherit, safInheritOnly];
    NewAcl^[Index].Mask := [samCreateChild, samDeleteChild, samListChildren, samSelfWrite, samReadProp, samWriteProp, samDeleteTree, samListObject, samControlAccess, samDelete, samReadControl, samWriteDac, samWriteOwner];
    NewAcl^[Index].ObjectType := GUID_NULL;
    NewAcl^[Index].InheritedObjectType := guid;
  end;

  procedure FullControl(NewAcl: PSecAcl);
  var
    Index: Integer;
  begin
    Index := Length(NewAcl^);
    SetLength(NewAcl^, Index + 1);
    NewAcl^[Index].AceType := satObjectAccessAllowed;
    NewAcl^[Index].Flags := [safContainerInherit];
    NewAcl^[Index].Mask := [samCreateChild, samDeleteChild, samListChildren, samSelfWrite, samReadProp, samWriteProp, samDeleteTree, samListObject, samControlAccess, samDelete, samReadControl, samWriteDac, samWriteOwner];
    NewAcl^[Index].ObjectType := GUID_NULL;
    NewAcl^[Index].InheritedObjectType := GUID_NULL;
  end;

  procedure GenerateResultantSetOfPolicy(NewAcl: PSecAcl; Id: RawUtf8);
  begin

  end;

  procedure JoinDomain(NewAcl: PSecAcl);
  var
    Index: Integer;
    ObjectDN: RawUtf8;
    LdapObject: TLdapAttribute;
    guid: TGuid;
  begin
    //ObjectDN := VisMain.Storage.GetSchemaObjectName('lDAPDisplayName', 'computer');

    LdapObject := fCore.LdapClient.SearchObject(Join(['CN=Schema,', fCore.LdapClient.ConfigDN]), FormatUtf8('(lDAPDisplayName=%)', [LdapEscape('computer')]), 'distinguishedName', lssWholeSubtree);
    if not Assigned(LdapObject) then
    begin
      ShowLdapSearchError(fCore.LdapClient);
      Exit;
    end;
    ObjectDN := LdapObject.GetReadable();

    LdapObject := fCore.LdapClient.SearchObject(ObjectDN, '', 'schemaIDGUID');
    if not Assigned(LdapObject) then
    begin
      ShowLdapSearchError(fCore.LdapClient);
      Exit;
    end;
    guid := PGuid(LdapObject.GetRaw())^;

    Index := Length(NewAcl^);
    SetLength(NewAcl^, Index + 1);
    NewAcl^[Index].AceType := satObjectAccessAllowed;
    NewAcl^[Index].Flags := [safContainerInherit];
    NewAcl^[Index].Mask := [samCreateChild];
    NewAcl^[Index].ObjectType := guid;
    NewAcl^[Index].InheritedObjectType := GUID_NULL;
  end;

  procedure ManageGroupPolicyLink(NewAcl: PSecAcl);
  begin

  end;

  procedure ModifyGroupMembership(NewAcl: PSecAcl);
  var
    ObjectDN: RawUtf8;
    Index: Integer;
    LdapObject: TLdapAttribute;
    guid: TGuid;
  begin
    //ObjectDN := VisMain.Storage.GetSchemaObjectName('lDAPDisplayName', 'member');

    LdapObject := fCore.LdapClient.SearchObject(Join(['CN=Schema,', fCore.LdapClient.ConfigDN]), FormatUtf8('(lDAPDisplayName=%)', [LdapEscape('member')]), 'distinguishedName', lssWholeSubtree);
    if not Assigned(LdapObject) then
    begin
      ShowLdapSearchError(fCore.LdapClient);
      Exit;
    end;
    ObjectDN := LdapObject.GetReadable();

    LdapObject := fCore.LdapClient.SearchObject(ObjectDN, '', 'schemaIDGUID');
    if not Assigned(LdapObject) then
    begin
      ShowLdapSearchError(fCore.LdapClient);
      Exit;
    end;
    guid := PGuid(LdapObject.GetRaw())^;

    Index := Length(NewAcl^);
    SetLength(NewAcl^, Index + 1);
    NewAcl^[Index].AceType := satObjectAccessAllowed;
    NewAcl^[Index].Flags := [safContainerInherit, safInheritOnly];
    NewAcl^[Index].Mask := [samReadProp, samWriteProp];
    NewAcl^[Index].ObjectType := guid;

    //ObjectDN := VisMain.Storage.GetSchemaObjectName('lDAPDisplayName', 'group');

    LdapObject := fCore.LdapClient.SearchObject(Join(['CN=Schema,', fCore.LdapClient.ConfigDN]), FormatUtf8('(lDAPDisplayName=%)', [LdapEscape('group')]), 'distinguishedName', lssWholeSubtree);
    if not Assigned(LdapObject) then
    begin
      ShowLdapSearchError(fCore.LdapClient);
      Exit;
    end;
    ObjectDN := LdapObject.GetReadable();

    LdapObject := fCore.LdapClient.SearchObject(ObjectDN, '', 'schemaIDGUID');
    if not Assigned(LdapObject) then
    begin
      ShowLdapSearchError(fCore.LdapClient);
      Exit;
    end;
    guid := PGuid(LdapObject.GetRaw())^;

    NewAcl^[Index].InheritedObjectType := guid;
  end;

  procedure ReadAllInformation(NewAcl: PSecAcl; ObjectType: RawUtf8);
  var
    ObjectDN: RawUtf8;
    Index: Integer;
    LdapObject: TLdapAttribute;
    guid: TGuid;
  begin
    //ObjectDN := VisMain.Storage.GetSchemaObjectName('lDAPDisplayName', ObjectType);

    LdapObject := fCore.LdapClient.SearchObject(Join(['CN=Schema,', fCore.LdapClient.ConfigDN]), FormatUtf8('(lDAPDisplayName=%)', [LdapEscape(ObjectType)]), 'distinguishedName', lssWholeSubtree);
    if not Assigned(LdapObject) then
    begin
      ShowLdapSearchError(fCore.LdapClient);
      Exit;
    end;
    ObjectDN := LdapObject.GetReadable();

    LdapObject := fCore.LdapClient.SearchObject(ObjectDN, '', 'schemaIDGUID');
    if not Assigned(LdapObject) then
    begin
      ShowLdapSearchError(fCore.LdapClient);
      Exit;
    end;
    guid := PGuid(LdapObject.GetRaw())^;

    Index := Length(NewAcl^);
    SetLength(NewAcl^, Index + 1);
    NewAcl^[Index].AceType := satObjectAccessAllowed;
    NewAcl^[Index].Flags := [safContainerInherit, safInheritOnly];
    NewAcl^[Index].Mask := [samReadProp];
    NewAcl^[Index].ObjectType := GUID_NULL;
    NewAcl^[Index].InheritedObjectType := guid;
  end;

  procedure ResetPasswords(NewAcl: PSecAcl; ObjectType: RawUtf8);
  var
    ObjectDN: RawUtf8;
    Index: Integer;
    attr, LdapObject: TLdapAttribute;
    guid: TGuid;
  begin
    //ObjectDN := VisMain.Storage.GetSchemaObjectName('lDAPDisplayName', 'pwdLastSet');

    LdapObject := fCore.LdapClient.SearchObject(Join(['CN=Schema,', fCore.LdapClient.ConfigDN]), FormatUtf8('(lDAPDisplayName=%)', [LdapEscape('pwdLastSet')]), 'distinguishedName', lssWholeSubtree);
    if not Assigned(LdapObject) then
    begin
      ShowLdapSearchError(fCore.LdapClient);
      Exit;
    end;

    ObjectDN := LdapObject.GetReadable();

    LdapObject := fCore.LdapClient.SearchObject(ObjectDN, '', 'schemaIDGUID');
    if not Assigned(LdapObject) then
    begin
      ShowLdapSearchError(fCore.LdapClient);
      Exit;
    end;
    guid := PGuid(LdapObject.GetRaw())^;

    Index := Length(NewAcl^);
    SetLength(NewAcl^, Index + 1);
    NewAcl^[Index].AceType := satObjectAccessAllowed;
    NewAcl^[Index].Flags := [safContainerInherit, safInheritOnly];
    NewAcl^[Index].Mask := [samReadProp, samWriteProp];
    NewAcl^[Index].ObjectType := guid;

    //ObjectDN := VisMain.Storage.GetSchemaObjectName('lDAPDisplayName', ObjectType);

    LdapObject := fCore.LdapClient.SearchObject(Join(['CN=Schema,', fCore.LdapClient.ConfigDN]), FormatUtf8('(lDAPDisplayName=%)', [LdapEscape(ObjectType)]), 'distinguishedName', lssWholeSubtree);
    if not Assigned(LdapObject) then
      Exit;
    ObjectDN := LdapObject.GetReadable();

    attr := fCore.LdapClient.SearchObject(ObjectDN, '', 'schemaIDGUID');
    guid := PGuid(attr.GetRaw())^;

    NewAcl^[Index].InheritedObjectType := guid;
  end;

begin

  if not Assigned(fCore.LdapClient) then
    Exit;

  // Get selected object acl
  res := fCore.LdapClient.SearchObject(SelectedObject, '', 'nTSecurityDescriptor');
  if not Assigned(res) then
  begin
    ShowLdapSearchError(fCore.LdapClient);
    Exit;
  end;

  if not SecurityDescriptor.FromBinary(res.GetRaw()) then
    Exit;

  //data.Init();
  //VisMain.Storage.ListSchemaAttributes(@data, ['lDAPDisplayName', 'schemaIDGUID']);

  NewAcl := [];
  // Create acl
  for i := 0 to CheckListBox1.Count - 1 do
  begin
    if not CheckListBox1.Checked[i] then
      continue;
    case i of
      0: CreateDeleteManage(@NewAcl, 'computer');              // Create, delete, and manage computers
      1: CreateDeleteManage(@NewAcl, 'inetOrgPerson');         // Create, delete, and manage inetOrgPerson accounts
      2: CreateDeleteManage(@NewAcl, 'group');                 // Create, delete, and manage groups accounts
      3: CreateDeleteManage(@NewAcl, 'user');                  // Create, delete, and manage users accounts
      4: FullControl(@NewAcl);                                 // Full control
      5: GenerateResultantSetOfPolicy(@NewAcl, 'logging');     // Generate Resultant Set of Policy (Logging)
      6: GenerateResultantSetOfPolicy(@NewAcl, 'planning');    // Generate Resultant Set of Policy (Planning)
      7: JoinDomain(@NewAcl);                                  // Join a computer to the domain
      8: ManageGroupPolicyLink(@NewAcl);                       // Manage Group Policy links
      9: ModifyGroupMembership(@NewAcl);                       // Modify the membership of a group
      10: ReadAllInformation(@NewAcl, 'computer');             // Read all computer information
      11: ReadAllInformation(@NewAcl, 'inetOrgPerson');        // Read all inetOrgPerson information
      12: ReadAllInformation(@NewAcl, 'group');                // Read all group information
      13: ReadAllInformation(@NewAcl, 'user');                 // Read all user information
      14: ResetPasswords(@NewAcl, 'computer');                 // Reset computer passwords and force password change at next logon
      15: ResetPasswords(@NewAcl, 'inetOrgPerson');            // Reset inetOrgPerson passwords and force password change at next logon
      16: ResetPasswords(@NewAcl, 'user');                     // Reset user passwords and force password change at next logon
      else
        raise Exception.Create('Invalid task to delegate id');
    end;
  end;

  // Retrieve user sids
  Filter := '(|';
  for user in ListBox1.Items do
    Filter := FormatUtf8('%(sAMAccountName=%)', [Filter, LdapEscape(user)]);
  Filter := FormatUtf8('%)', [Filter]);

  fCore.LdapClient.SearchBegin();
  try
    fCore.LdapClient.SearchScope := lssWholeSubtree;
    repeat
      if not fCore.LdapClient.Search(fBaseDN, False, Filter, ['sAMAccountName', 'objectSID']) then
      begin
        ShowLdapSearchError(fCore.LdapClient);
        Exit;
      end;

      for Item in fCore.LdapClient.SearchResult.Items do
      begin
        // For each user, change acl SID and add it to the dacl
        for i := 0 to High(NewAcl) do
          NewAcl[i].Sid := Item.Find('objectSID').GetRaw();
        SecurityDescriptor.Dacl := Concat(SecurityDescriptor.Dacl, NewAcl);
      end;
    until fCore.LdapClient.SearchCookie = '';
  finally
    fCore.LdapClient.SearchEnd;
  end;
  fCore.LdapClient.OrderAcl(SelectedObject, fBaseDN, @SecurityDescriptor.Dacl);
  NewAttr := TLdapAttribute.Create('nTSecurityDescriptor', atNTSecurityDescriptor);
  try
    NewAttr.Add(SecurityDescriptor.ToBinary);

    if not fCore.LdapClient.Modify(SelectedObject, lmoReplace, NewAttr) then
    begin
      ShowLdapModifyError(fCore.LdapClient);
      Exit;
    end;
  finally
    FreeAndNil(NewAttr);
  end;
end;

end.

