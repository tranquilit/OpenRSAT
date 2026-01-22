unit uvisaddaces;

{$mode ObjFPC}{$H+}

interface

uses
  ActnList,
  Buttons,
  Classes,
  Controls,
  Dialogs,
  ExtCtrls,
  Forms,
  Graphics,
  StdCtrls,
  VirtualTrees,
  SysUtils,
  tis.ui.grid.core,
  tis.ui.searchedit,
  mormot.core.os.security,
  mormot.core.text,
  mormot.core.base,
  mormot.net.ldap,
  mormot.core.log,
  mormot.core.variants,
  ursatldapclient;

type

  { TVisAddACEs }

  TVisAddACEs = class(TForm)
    Action_Replace: TAction;
    Action_Clear: TAction;
    BitBtn_Replace: TBitBtn;
    BitBtn_Clear: TBitBtn;
    CheckBox_CC: TCheckBox;
    CheckBox_SD: TCheckBox;
    CheckBox_RC: TCheckBox;
    CheckBox_WD: TCheckBox;
    CheckBox_WO: TCheckBox;
    CheckBox_OI: TCheckBox;
    CheckBox_CI: TCheckBox;
    CheckBox_IO: TCheckBox;
    CheckBox_NP: TCheckBox;
    CheckBox_DC: TCheckBox;
    CheckBox_LC: TCheckBox;
    CheckBox_SW: TCheckBox;
    CheckBox_RP: TCheckBox;
    CheckBox_WP: TCheckBox;
    CheckBox_DT: TCheckBox;
    CheckBox_LO: TCheckBox;
    CheckBox_CR: TCheckBox;
    CheckGroup_Access: TCheckGroup;
    CheckGroup_Inheritance: TCheckGroup;
    ComboBox_Property: TComboBox;
    ComboBox_Inheritance: TComboBox;
    Label_Property: TLabel;
    Label_Inheritance: TLabel;
    Label_Title: TLabel;
    Label_Trustee: TLabel;
    Panel1: TPanel;
    Panel_type: TPanel;
    Panel_Center: TPanel;
      Panel_Grid: TPanel;
        BitBtn_Add: TBitBtn;
        BitBtn_Delete: TBitBtn;
    Panel_Bottom: TPanel;
      BitBtn_OK: TBitBtn;
      BitBtn_Cancel: TBitBtn;

    ActionList: TActionList;
      Action_Search: TAction;
      Action_Add: TAction;
      Action_Delete: TAction;
      Action_OK: TAction;
      RadioButton_Allow: TRadioButton;
      RadioButton_Deny: TRadioButton;
      Shape1: TShape;
      TisGrid1: TTisGrid;
      TisSearchEdit_Trustee: TTisSearchEdit;
      procedure Action_AddExecute(Sender: TObject);
      procedure Action_AddUpdate(Sender: TObject);
      procedure Action_ClearExecute(Sender: TObject);
      procedure Action_DeleteExecute(Sender: TObject);
      procedure Action_DeleteUpdate(Sender: TObject);
      procedure Action_OKExecute(Sender: TObject);
      procedure Action_ReplaceExecute(Sender: TObject);
      procedure Action_ReplaceUpdate(Sender: TObject);
      procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
      procedure TisGrid1FocusChanged(Sender: TBaseVirtualTree;
        Node: PVirtualNode; Column: TColumnIndex);
      procedure TisGrid1GetText(aSender: TBaseVirtualTree; aNode: PVirtualNode;
        const aCell: TDocVariantData; aColumn: TColumnIndex;
        aTextType: TVSTTextType; var aText: string);
      procedure TisSearchEdit_TrusteeBeforeSearch(Sender: TObject;
        const aText: string; var aAbort: Boolean);
  private
    Ldap: TRsatLdapClient;
    fBaseDN: RawUtf8;
    Trustees, appliesTo, Properties: TDocVariantData;
    facl: TSecAcl;
    function GetAccessMask(): Integer;
    function GetAceFlags(): Integer;
    procedure FillProperties();
    procedure FillInheritance();
  public
    constructor Create(TheOwner: TComponent; ALdap: TRsatLdapClient; ABaseDN: RawUtf8); reintroduce;
    property Acl: TSecAcl read facl;
  end;

const
  CN_EXTENDED_RIGHTS: RawUtf8 = 'CN=Extended-Rights';
  CN_SCHEMA: RawUtf8 = 'CN=Schema';

implementation

uses
  mormot.core.data,
  ucommon,
  ursatldapclientui;
{$R *.lfm}

{ TVisAddACEs }

function TVisAddACEs.GetAccessMask(): Integer;
begin
  result := 0;

  if CheckBox_CC.Checked then
    result += (1 << Ord(samCreateChild));
  if CheckBox_DC.Checked then
    result += (1 << Ord(samDeleteChild));
  if CheckBox_LC.Checked then
    result += (1 << Ord(samListChildren));
  if CheckBox_SW.Checked then
    result += (1 << Ord(samSelfWrite));
  if CheckBox_RP.Checked then
    result += (1 << Ord(samReadProp));
  if CheckBox_WP.Checked then
    result += (1 << Ord(samWriteProp));
  if CheckBox_DT.Checked then
    result += (1 << Ord(samDeleteTree));
  if CheckBox_LO.Checked then
    result += (1 << Ord(samListObject));
  if CheckBox_CR.Checked then
    result += (1 << Ord(samControlAccess));
  if CheckBox_SD.Checked then
    result += (1 << Ord(samDelete));
  if CheckBox_RC.Checked then
    result += (1 << Ord(samReadControl));
  if CheckBox_WD.Checked then
    result += (1 << Ord(samWriteDac));
  if CheckBox_WO.Checked then
    result += (1 << Ord(samWriteOwner));
end;

function TVisAddACEs.GetAceFlags(): Integer;
begin
  result := 0;

  if CheckBox_OI.Checked then
    result += (1 << Ord(safObjectInherit));
  if CheckBox_CI.Checked then
    result += (1 << Ord(safContainerInherit));
  if CheckBox_IO.Checked then
    result += (1 << Ord(safInheritOnly));
  if CheckBox_NP.Checked then
    result += (1 << Ord(safNoPropagateInherit));
end;

procedure TVisAddACEs.FillProperties();
var
  item: TLdapResult;
  aLog: ISynLog;
  data: TDocVariantData;
  rowData: PDocVariantData;
begin
  aLog := TSynLog.Enter('Fill Properties ComboBox', []);
  ComboBox_Property.Items.BeginUpdate;
  ComboBox_Property.Items.Clear;
  if Assigned(aLog) then
    aLog.Log(sllDebug, 'Search in Schema Cache.');

  data.init();
  //VisMain.Storage.ListSchemaAttributes(@data, ['lDAPDisplayName', 'schemaIDGUID']);
  for rowData in data.Objects do
  begin
    if not Assigned(rowData) or
       not rowData^.Exists('lDAPDisplayName') or
       rowData^.O['lDAPDisplayName']^.IsVoid then
      continue;
    ComboBox_Property.Items.Add(rowData^.A['lDAPDisplayName']^.ToRawUtf8DynArray[0]);
    Properties.AddOrUpdateValue(rowData^.A['lDAPDisplayName']^.ToRawUtf8DynArray[0], rowData^.A['schemaIDGUID']^.ToRawUtf8DynArray[0]);
  end;
  if Assigned(aLog) then
    aLog.Log(sllDebug, 'Search in Extended-Rights.');
  Ldap.SearchBegin();
  try
    Ldap.SearchScope := lssSingleLevel;
    repeat
      if not Ldap.Search(FormatUtf8('%,%', [CN_EXTENDED_RIGHTS, Ldap.ConfigDN]), False, '', ['displayName', 'objectGUID']) then
        Exit;
      for item in Ldap.SearchResult.Items do
      begin
        ComboBox_Property.Items.Add(item.Find('displayName').GetReadable());
        Properties.AddOrUpdateValue(item.Find('displayName').GetReadable(), item.Find('objectGUID').GetRaw());
      end;
    until Ldap.SearchCookie = '';
  finally
    Ldap.SearchEnd;
  end;
  ComboBox_Property.Items.EndUpdate;
end;

procedure TVisAddACEs.FillInheritance();
var
  aLog: ISynLog;
  data: TDocVariantData;
  rowData: PDocVariantData;
  ObjectClassArray: TRawUtf8DynArray;
begin
  aLog := TSynLog.Enter('Fill Inheritance ComboBox.', []);
  ComboBox_Inheritance.Items.BeginUpdate;
  ComboBox_Inheritance.Items.Clear;
  if Assigned(aLog) then
    aLog.log(sllDebug, 'Search in Schema Cache');
  data.init();
  {vismain.Storage.ListSchemaAttributes(@data, ['lDAPDisplayName', 'objectClass', 'schemaIDGUID']);}

  for rowData in data.Objects do
  begin
    if not Assigned(rowData) or
       not rowData^.Exists('objectClass') or
       rowData^.O['objectClass']^.IsVoid then
      continue;
    ObjectClassArray := rowData^.A['objectClass']^.ToRawUtf8DynArray;
    if ObjectClassArray[High(ObjectClassArray)] <> 'classSchema' then
      continue;
    ComboBox_Inheritance.Items.Add(rowData^.A['lDAPDisplayName']^.ToRawUtf8DynArray[0]);
    appliesTo.AddOrUpdateValue(rowData^.A['lDAPDisplayName']^.ToRawUtf8DynArray[0], rowData^.A['schemaIDGUID']^.ToRawUtf8DynArray[0]);
  end;
  ComboBox_Inheritance.Items.EndUpdate;
end;

// Built-in security principals
// Service Accounts
// Computers
// Groups
// Users
procedure TVisAddACEs.TisSearchEdit_TrusteeBeforeSearch(Sender: TObject;
  const aText: string; var aAbort: Boolean);
var
  Filter, s: RawUtf8;
  item: TLdapResult;
begin
  s := LdapEscape(aText);
  Filter := FormatUtf8('&(|(objectClass=user)(objectClass=computer)(objectClass=group))(|(name=%*)(sAMAccountName=%*)(cn=%*)(displayName=%*)(distinguishedName=%*))', [s, s, s, s, s]);

  // Search Trustees in Domain.
  Ldap.SearchBegin(TisSearchEdit_Trustee.DropDownCount);
  try
    Ldap.SearchScope := lssWholeSubtree;
    if not Ldap.Search(Ldap.DefaultDN(fBaseDN), False, Filter, ['distinguishedName', 'objectSid']) then
      Exit;
  finally
    Ldap.SearchEnd;
  end;

  // Add trustees in ComboBox
  TisSearchEdit_Trustee.Items.BeginUpdate;
  try
    TisSearchEdit_Trustee.Items.Clear;
    for item in Ldap.SearchResult.Items do
    begin
      TisSearchEdit_Trustee.Items.Add(item.Attr[atDistinguishedName]);
      Trustees.AddOrUpdateValue(item.Attr[atDistinguishedName], item.Attributes.GetByName('objectSid'));
    end;
  finally
    TisSearchEdit_Trustee.Items.EndUpdate;
  end;

  if Ldap.SearchResult.Count >= TisSearchEdit_Trustee.DropDownCount then
    Exit;
  // Search Trustees in Configuration
  Filter := FormatUtf8('&(objectClass=foreignSecurityPrincipal)(|(name=%*)(cn=%*))', [s, s]);
  Ldap.SearchBegin(TisSearchEdit_Trustee.DropDownCount - Ldap.SearchResult.Count);
  try
    Ldap.SearchScope := lssWholeSubtree;
    if not Ldap.Search(FormatUtf8('CN=WellKnown Security Principals,%', [Ldap.ConfigDN()]), False, Filter, ['name', 'objectSid']) then
      Exit;
  finally
    Ldap.SearchEnd;
  end;

  // Add trustees in ComboBox
  TisSearchEdit_Trustee.Items.BeginUpdate;
  try
    for item in Ldap.SearchResult.Items do
    begin
      TisSearchEdit_Trustee.Items.Add(item.Attr[atName]);
      Trustees.AddOrUpdateValue(item.Attr[atName], item.Attributes.GetByName('objectSid'));
    end;
  finally
    TisSearchEdit_Trustee.Items.EndUpdate;
  end;
  if TisSearchEdit_Trustee.Items.Count >= 1 then
    TisSearchEdit_Trustee.DroppedDown := True;
end;

procedure TVisAddACEs.Action_AddExecute(Sender: TObject);
var
  data: TDocVariantData;
  node: PVirtualNode;
begin
  data.Init(JSON_FAST);
  if (RadioButton_Allow.Checked) then
    data.AddValue('type', 0)
  else
    data.AddValue('type', 1);
  data.AddValue('sid', TisSearchEdit_Trustee.Text);
  data.AddValue('mask', GetAccessMask());
  data.AddValue('objectType', ComboBox_Property.Text);
  data.AddValue('flags', GetAceFlags());
  data.AddValue('iObjectType', ComboBox_Inheritance.Text);
  node := TisGrid1.GetNodeBy(@data);
  if Assigned(node) then
  begin
    TisGrid1.FocusedNode := node;
    MessageDlg(rsWarning, 'ACE already exists.', mtWarning, [mbOK], 0);
    Exit;
  end;
  TisGrid1.Data.AddItem(data);
  TisGrid1.LoadData;
end;

procedure TVisAddACEs.Action_AddUpdate(Sender: TObject);
begin
  action_Add.Enabled := (TisSearchEdit_Trustee.Text <> '') and (GetAccessMask() <> 0) and (Trustees.S[TisSearchEdit_Trustee.Text] <> '');
end;

procedure TVisAddACEs.Action_ClearExecute(Sender: TObject);
begin
  RadioButton_Allow.Checked := True;
  RadioButton_Deny.Checked := False;
  TisSearchEdit_Trustee.Text := '';
  ComboBox_Property.Text := '';
  ComboBox_Inheritance.Text := '';
  CheckBox_CC.Checked := False;
  CheckBox_DC.Checked := False;
  CheckBox_LC.Checked := False;
  CheckBox_SW.Checked := False;
  CheckBox_RP.Checked := False;
  CheckBox_WP.Checked := False;
  CheckBox_DT.Checked := False;
  CheckBox_LO.Checked := False;
  CheckBox_CR.Checked := False;
  CheckBox_SD.Checked := False;
  CheckBox_RC.Checked := False;
  CheckBox_WD.Checked := False;
  CheckBox_WO.Checked := False;

  CheckBox_OI.Checked := False;
  CheckBox_CI.Checked := False;
  CheckBox_IO.Checked := False;
  CheckBox_NP.Checked := False;
  TisGrid1.ClearSelection;
  TisGrid1.FocusedNode := Nil;
end;

procedure TVisAddACEs.Action_DeleteExecute(Sender: TObject);
var
  count: Integer;
begin
  count := TisGrid1.Data.Count;
  TisGrid1.DeleteSelectedRows;
  assert(TisGrid1.Data.Count = count - 1, 'Invalid number of deletion.');
end;

procedure TVisAddACEs.Action_DeleteUpdate(Sender: TObject);
begin
  Action_Delete.Enabled := (TisGrid1.SelectedCount = 1);
end;

procedure TVisAddACEs.Action_OKExecute(Sender: TObject);
var
  obj: PDocVariantData;
  ace: TSecAce;
  sid: RawUtf8;
  guid, iguid: TGuid;
  found, objectType: Boolean;
  i: Integer;
  AceType: TSecAceType;
  flag: TSecAceFlag;
  _mask: TSecAccess;
begin
  if (TisGrid1.Data.Count <= 0) or (mrOK <> MessageDlg(rsConfirmation, 'Do you want to add the following ACE list to this object' + #39 + 's ACL ? This action is going to modify security rules.', mtWarning, [mbOK, mbCancel], 0)) then
  begin
    ModalResult := mrCancel;
    Exit;
  end;
  facl := [];

  // Convertion tisgrid -> ACEs (resolution & verification)
  for obj in TisGrid1.Data.Objects do
  begin
    if not Assigned(obj) then
      continue;
    // Resolve information
    sid := TextToRawSid(Trustees.S[obj^.S['sid']]);
    guid := StringToGuid(Properties.S[obj^.S['objectType']]);
    iguid := StringToGuid(appliesTo.S[obj^.S['iObjectType']]);
    objectType := not (IsNullGuid(guid) and IsNullGuid(iguid));
    if objectType then
    begin
      if (obj^.I['type'] = 0) then
        AceType := satObjectAccessAllowed
      else
        AceType := satObjectAccessDenied;
    end
    else
    begin
      if (obj^.I['type'] = 0) then
        AceType := satAccessAllowed
      else
        AceType := satAccessDenied;
    end;
    Found := False;
    // Search if two ace can be combined.
    for i := 0 to Length(facl) - 1 do
    begin
      if (aceType = facl[i].AceType) and (facl[i].Sid = sid) and IsEqualGuid(@facl[i].ObjectType, @guid) and IsEqualGuid(@facl[i].InheritedObjectType, @iguid) then
      begin
        found := True;
        // ACE with raw can be combined.
        for _mask := Low(TSecAccess) to High(TSecAccess) do
        begin
          if (not (_mask in facl[i].Mask) and ((obj^.I['mask'] and (1 << Ord(_mask))) <> 0)) then
            facl[i].mask += [_mask];
        end;
        for flag := Low(TSecAceFlag) to High(TSecAceFlag) do
        begin
          if (not (flag in facl[i].Flags) and ((obj^.I['flags'] and (1 << Ord(flag))) <> 0)) then
            facl[i].Flags += [flag];
        end;
        break;
      end;
    end;

    if not Found then
    begin
      ace.Sid := sid;
      ace.Flags := [];
      for flag := Low(TSecAceFlag) to High(TSecAceFlag) do
      begin
        if ((obj^.I['flags'] and (1 << Ord(flag))) <> 0) then
          ace.Flags += [flag];
      end;
      ace.Mask := [];
      for _mask := Low(TSecAccess) to High(TSecAccess) do
      begin
        if ((obj^.I['mask'] and (1 << Ord(_mask))) <> 0) then
          ace.Mask += [_mask];
      end;
      ace.ObjectType := guid;
      ace.InheritedObjectType := iguid;
      ace.AceType := aceType;
      Insert(ace, facl, 0);
    end;
  end;
end;

procedure TVisAddACEs.Action_ReplaceExecute(Sender: TObject);
var
  PData: PDocVariantData;
  Data: TDocVariantData;
  node: PVirtualNode;
begin
  PData := TisGrid1.GetNodeAsPDocVariantData(Nil);
  if not Assigned(PData) then
    Exit;

  data.Init(JSON_FAST);
  if (RadioButton_Allow.Checked) then
    data.AddValue('type', 0)
  else
    data.AddValue('type', 1);
  data.AddValue('sid', TisSearchEdit_Trustee.Text);
  data.AddValue('mask', GetAccessMask());
  data.AddValue('objectType', ComboBox_Property.Text);
  data.AddValue('flags', GetAceFlags());
  data.AddValue('iObjectType', ComboBox_Inheritance.Text);
  node := TisGrid1.GetNodeBy(@data);
  if Assigned(node) then
  begin
    TisGrid1.FocusedNode := node;
    MessageDlg(rsWarning, 'ACE already exists.', mtWarning, [mbOK], 0);
    Exit;
  end;
  PData^ := data;
  TisGrid1.LoadData;
  TisGrid1.FocusedNode := TisGrid1.GetNodeBy(PData);
end;

procedure TVisAddACEs.Action_ReplaceUpdate(Sender: TObject);
begin
  Action_Replace.Enabled := Assigned(TisGrid1.FocusedNode);
end;

procedure TVisAddACEs.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    27: Close;
  end;
end;

procedure TVisAddACEs.TisGrid1FocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  Data: PDocVariantData;
begin
  Data := TisGrid1.GetNodeAsPDocVariantData(Node);
  if not Assigned(Data) then
    Exit;
  // Type
  RadioButton_Allow.Checked := (Data^.S['type'] = '0');
  RadioButton_Deny.Checked := not RadioButton_Allow.Checked;

  // Trustee
  TisSearchEdit_Trustee.Text := Data^.S['sid'];

  // Access
  CheckBox_CC.Checked := (Data^.I['mask'] and (1 << Ord(samCreateChild)) <> 0);
  CheckBox_DC.Checked := (Data^.I['mask'] and (1 << Ord(samDeleteChild)) <> 0);
  CheckBox_LC.Checked := (Data^.I['mask'] and (1 << Ord(samListChildren)) <> 0);
  CheckBox_SW.Checked := (Data^.I['mask'] and (1 << Ord(samSelfWrite)) <> 0);
  CheckBox_RP.Checked := (Data^.I['mask'] and (1 << Ord(samReadProp)) <> 0);
  CheckBox_WP.Checked := (Data^.I['mask'] and (1 << Ord(samWriteProp)) <> 0);
  CheckBox_DT.Checked := (Data^.I['mask'] and (1 << Ord(samDeleteTree)) <> 0);
  CheckBox_LO.Checked := (Data^.I['mask'] and (1 << Ord(samListObject)) <> 0);
  CheckBox_CR.Checked := (Data^.I['mask'] and (1 << Ord(samControlAccess)) <> 0);
  CheckBox_SD.Checked := (Data^.I['mask'] and (1 << Ord(samDelete)) <> 0);
  CheckBox_RC.Checked := (Data^.I['mask'] and (1 << Ord(samReadControl)) <> 0);
  CheckBox_WD.Checked := (Data^.I['mask'] and (1 << Ord(samWriteDac)) <> 0);
  CheckBox_WO.Checked := (Data^.I['mask'] and (1 << Ord(samWriteOwner)) <> 0);

  // Flags
  CheckBox_OI.Checked := (Data^.I['flags'] and (1 << Ord(safObjectInherit)) <> 0);
  CheckBox_CI.Checked := (Data^.I['flags'] and (1 << Ord(safContainerInherit)) <> 0);
  CheckBox_IO.Checked := (Data^.I['flags'] and (1 << Ord(safInheritOnly)) <> 0);
  CheckBox_NP.Checked := (Data^.I['flags'] and (1 << Ord(safNOPropagateInherit)) <> 0);

  // Property
  if Data^.Exists('objectType') then
    ComboBox_Property.Text := Data^.S['objectType'];

  // Inheritance
  if Data^.Exists('iObjectType') then
    ComboBox_Inheritance.Text := Data^.S['iObjectType'];
end;

procedure TVisAddACEs.TisGrid1GetText(aSender: TBaseVirtualTree;
  aNode: PVirtualNode; const aCell: TDocVariantData; aColumn: TColumnIndex;
  aTextType: TVSTTextType; var aText: string);
var
  value: LongInt;
  access: TSecAccess;
  flag: TSecAceFlag;
begin
  case TisGrid1.FindColumnByIndex(aColumn).PropertyName of
  'type':
    begin
      if aText = '0' then
        aText := 'Allow'
      else
        aText := 'Deny';
    end;
  'mask':
    begin
      value := StrToInt(aText);
      aText := '';
      for access := Low(TSecAccess) to High(TSecAccess) do
      begin
        if ((value and (1 << Ord(access))) <> 0) then
        begin
          if aText = '' then
            aText := SAM_SDDL[access]
          else
            aText += ', ' + SAM_SDDL[access];
        end;
      end;
    end;
  'flags':
    begin
      value := StrToInt(aText);
      aText := '';
      for flag := Low(TSecAceFlag) to High(TSecAceFlag) do
      begin
        if ((value and (1 << Ord(flag))) <> 0) then
        begin
          if aText = '' then
            aText := SAF_SDDL[flag]
          else
            aText += ', ' + SAF_SDDL[flag];
        end;
      end;
    end;
  end;
end;

constructor TVisAddACEs.Create(TheOwner: TComponent; ALdap: TRsatLdapClient;
  ABaseDN: RawUtf8);
begin
  Inherited Create(TheOwner);
  Ldap := ALdap;
  fBaseDN := ABaseDN;
  Trustees.Init([dvoCheckForDuplicatedNames, dvoReturnNullForUnknownProperty, dvoValueCopiedByReference]);
  appliesTo.Init([dvoCheckForDuplicatedNames, dvoReturnNullForUnknownProperty, dvoValueCopiedByReference]);
  Properties.Init([dvoCheckForDuplicatedNames, dvoReturnNullForUnknownProperty, dvoValueCopiedByReference]);

  FillProperties();
  FillInheritance();
end;

end.

