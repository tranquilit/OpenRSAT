unit uvisadvancedsecuritytest;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils, Controls,
  mormot.core.test,
  mormot.core.os.security,
  mormot.core.variants,
  mormot.core.base,
  mormot.net.ldap,
  ursatldapclient,
  uvisadvancedsecurity,
  VirtualTrees;

type

  TVisAdvancedSecurityTestException = class(Exception);

  { TSidCacheTest }

  TSidCacheTest = class(TSynTestCase)
  private
    BaseDN: RawUtf8;
    fSidCache: TSidCache;
    fLdapClient: TLdapClient;
  published
    procedure RunWithoutLdapClient;
    procedure RunWithLdapClient;
  public
    procedure Setup; override;
    procedure MethodSetup; override;
    procedure CleanUp; override;
    procedure MethodCleanUp; override;
  end;

  { TGuidCacheTest }

  TGuidCacheTest = class(TSynTestCase)
  private
    BaseDN: RawUtf8;
    fGuidCache: TGUIDCache;
    fLdapClient: TLdapClient;
  published
    procedure RunWithoutLdapClient;
    procedure RunWithLdapClient;
  public
    procedure Setup; override;
    procedure MethodSetup; override;
    procedure CleanUp; override;
    procedure MethodCleanUp; override;
  end;


  { TVisAdvancedSecurityTest }

  TVisAdvancedSecurityTest = class(TSynTestCase)
  private
    fSecurityDescriptor: TSecurityDescriptor;
    fVis: TVisAdvancedSecurity;
    fLdapClient: TRsatLdapClient;
    BaseDN: RawUtf8;

    procedure TestRightPanel(NodeData: PDocVariantData);
  published
    procedure Creation;
    procedure Action_SelectOwner;
    procedure Action_SelectGroup;
    procedure Action_AddACE;
    procedure Action_DuplicateACE;
    procedure Action_DeleteACE;
    procedure Action_SelectPrincipal;
    procedure Action_SelectObject;
    procedure Action_SelectInheritedObject;
    procedure Action_Apply;
    procedure Action_Cancel;
    procedure Action_OK;
    procedure ChangeObjectName;
    procedure ChangeObjectDN;
    procedure ChangeLdapClient;
    procedure SetSecurityDescriptor;
    procedure ChangeOwner;
    procedure ChangePrincipal;
  public
    procedure Setup; override;
    procedure CleanUp; override;
    procedure MethodSetup; override;
    procedure MethodCleanUp; override;
  end;

function GetBaseDN(DefaultDN: RawUtf8): RawUtf8;
function SetupLdapClient: TLdapClient;

implementation
uses
  mormot.core.buffers,
  mormot.core.text,
  ucommon;

function GetBaseDN(DefaultDN: RawUtf8): RawUtf8;
begin
  result := GetEnvironmentVariable('DELEGATION_OU');
  if result = '' then
    result := FormatUtf8('OU=test-openrsat,%', [DefaultDN]);
end;

function SetupLdapClient: TLdapClient;
var
  ObjectClassAttr: TLdapAttribute;
begin
  result := TRsatLdapClient.Create;
  result.Settings.UserName := GetEnvironmentVariable('LDAP_USERNAME');
  result.Settings.Password := GetEnvironmentVariable('LDAP_PASSWORD');
  result.Settings.KerberosDN := GetEnvironmentVariable('LDAP_DOMAIN');
  result.Settings.TargetHost := GetEnvironmentVariable('LDAP_DOMAINCONTROLLER');
  result.Settings.AutoBind := lcbKerberos;
  result.Settings.AutoReconnect := True;
  result.Connect();
  if not result.Connected then
    raise TVisAdvancedSecurityTestException.Create(FormatUtf8('Connection failure (%)', [result.ResultString]));

  // Ensure OU exists on the domain.
  ObjectClassAttr := result.SearchObject(GetBaseDN(Result.DefaultDN()), '', 'objectClass');
  if not Assigned(ObjectClassAttr) then
    raise TVisAdvancedSecurityTestException.Create('Cannot retrieve objectClass on OU.');
  if ObjectClassAttr.GetReadable(Pred(ObjectClassAttr.Count)) <> 'organizationalUnit' then
    raise TVisAdvancedSecurityTestException.Create('BaseDN is not an OU.');
end;

{ TSidCacheTest }

procedure TSidCacheTest.RunWithoutLdapClient;
begin
  Check(fSidCache.Count = 0);
  fSidCache.AddSID('S-1-5');
  Check(fSidCache.Count = 1);
  fSidCache.AddSID('S-1-5');
  Check(fSidCache.Count = 1);
  fSidCache.AddSID('S-1-5-3');
  Check(fSidCache.Count = 2);
  fSidCache.AddSID('');
  Check(fSidCache.Count = 2);
  Check(fSidCache.SIDToName('S-1-5') = 'S-1-5');
  Check(fSidCache.SIDToName('') = '');
  Check(fSidCache.SIDToName('S-1') = 'S-1');
  Check(fSidCache.SIDToName('S-1-5-3') = 'S-1-5-3');
  Check(fSidCache.NameToSID('S-1-5') = 'S-1-5');
  Check(fSidCache.NameToSID('S-1') = '');
  Check(fSidCache.NameToSID('') = ''); // False

  fSidCache.SetSIDName('S-1-5', 'Domain');
  Check(fSidCache.Count = 2);
  Check(fSidCache.SIDToName('S-1-5') = 'Domain');
  Check(fSidCache.SIDToName('S-1-5-3') = 'S-1-5-3');
  fSidCache.SetSIDName('S-1-5', 'Domains');
  Check(fSidCache.Count = 2);
  Check(fSidCache.SIDToName('S-1-5') = 'Domains');
  Check(fSidCache.NameToSID('Domain') = '');
  Check(fSidCache.NameToSID('Domains') = 'S-1-5');
  fSidCache.Clear;
  Check(fSidCache.Count = 0);
  Check(fSidCache.SIDToName('S-1-5') = 'S-1-5');
  Check(fSidCache.NameToSID('Domains') = '');
end;

procedure TSidCacheTest.RunWithLdapClient;
const
  ENTRY_COUNT = 100;
var
  TestOU, ObjectSid: RawUtf8;
  Attributes: TLdapAttributeList;
  Attr: TLdapAttribute;
  i: Integer;
  LdapResult: TLdapResultList;
begin
  TestOU := FormatUtf8('OU=test-sid,%', [BaseDN]);
  Attributes := TLdapAttributeList.Create;
  try
    Attr := Attributes.Add('objectClass', 'top');
    Attr.Add('organizationalUnit');
    fLdapClient.Add(TestOU, Attributes);
    Attributes.Clear;

    Attr := Attributes.Add('objectClass', 'top');
    Attr.Add('person');
    Attr.Add('organizationalPerson');
    Attr.Add('user');
    for i := 0 to ENTRY_COUNT do
      fLdapClient.Add(FormatUtf8('cn=user_%,%', [i, TestOU]), Attributes);
  finally
    FreeAndNil(Attributes);
  end;

  fLdapClient.SearchBegin(ENTRY_COUNT);
  try
    fLdapClient.SearchScope := lssSingleLevel;
    fLdapClient.Search(TestOU, False, '', ['objectSid', 'name']);
    LdapResult := TLdapResultList(fLdapClient.SearchResult.Clone);
  finally
    fLdapClient.SearchEnd;
  end;
  try
    for i := 0 to Pred(LdapResult.Count) do
    begin
      if not Assigned(LdapResult.Items[i]) then
        continue;
      ObjectSid := LdapResult.Items[i].Find('objectSid').GetReadable();
      fSidCache.AddSID(ObjectSid);
    end;
    Check(fSidCache.Count = ENTRY_COUNT);
    for i := 0 to Pred(LdapResult.Count) do
    begin
      if not Assigned(LdapResult.Items[i]) then
        continue;
      Check(fSidCache.SIDToName(LdapResult.Items[i].Find('objectSid').GetReadable()) = LdapResult.Items[i].Find('objectSid').GetReadable());
      Check(fSidCache.NameToSID(LdapResult.Items[i].Find('name').GetReadable()) = '');
    end;

    fSidCache.ResolveSIDS(fLdapClient);
    Check(fSidCache.Count = ENTRY_COUNT);
    for i := 0 to Pred(LdapResult.Count) do
    begin
      if not Assigned(LdapResult.Items[i]) then
        continue;
      Check(fSidCache.SIDToName(LdapResult.Items[i].Find('objectSid').GetReadable()) = LdapResult.Items[i].Find('name').GetReadable());
      Check(fSidCache.NameToSID(LdapResult.Items[i].Find('name').GetReadable()) = LdapResult.Items[i].Find('objectSid').GetReadable());
    end;
    fSidCache.Clear;
    Check(fSidCache.Count = 0);
  finally
    FreeAndNil(LdapResult);
  end;
  fLdapClient.Delete(TestOU, True);
end;

procedure TSidCacheTest.Setup;
begin
  fLdapClient := SetupLdapClient;
  BaseDN := fLdapClient.DefaultDN();
end;

procedure TSidCacheTest.MethodSetup;
begin
  fSidCache := TSidCache.Create;
end;

procedure TSidCacheTest.CleanUp;
begin
  FreeAndNil(fLdapClient);
end;

procedure TSidCacheTest.MethodCleanUp;
begin
  FreeAndNil(fSidCache);
end;

{ TGuidCacheTest }

procedure TGuidCacheTest.RunWithoutLdapClient;
begin
  Check(fGuidCache.Count = 0);
  fGuidCache.AddGUID('{00-11}', 'class');
  fGuidCache.AddGUID('{01-10}', 'class');
  Check(fGuidCache.Count = 2);
  fGuidCache.AddGUID('', 'class');
  Check(fGuidCache.Count = 2);
  Check(fGuidCache.GUIDToName('{00-11}', 'class') = '{00-11}');
  Check(fGuidCache.GUIDToName('', 'class') = '');
  Check(fGuidCache.GUIDToName('{01-10}', 'class') = '{01-10}');
  fGuidCache.SetGUIDName('{00-11}', 'Mail', 'class');
  Check(fGuidCache.GUIDToName('{00-11}', 'class') = 'Mail');
  Check(fGuidCache.GUIDToName('', 'class') = '');
  Check(fGuidCache.GUIDToName('{01-10}', 'class') = '{01-10}');
  Check(fGuidCache.NameToGUID('Mail') = '{00-11}');
  Check(fGuidCache.NameToGUID('Address') = '');
  Check(fGuidCache.NameToGUID('') = '');
  fGuidCache.Clear;
  Check(fGuidCache.Count = 0);
end;

procedure TGuidCacheTest.RunWithLdapClient;
var
  GUID, ExtendedRightsDN, v: RawUtf8;
  i, c: Integer;
  vFile: TextFile;
  ValidAccess: Longint;
begin
  fGuidCache.ResolveGUIDS(fLdapClient);

  v := fGuidCache.Dump;
  AssignFile(vFile, 'guid.json');
  Rewrite(vFile);
  WriteLn(vFile, v);
  CloseFile(vFile);
  c := 0;

  fLdapClient.SearchBegin();
  try
    fLdapClient.SearchScope := lssSingleLevel;
    repeat
      fLdapClient.Search(fLdapClient.SchemaDN, False, '(objectClass=classSchema)', ['cn', 'lDAPDisplayName', 'governsID', 'schemaIDGUID']);

      for i := 0 to Pred(fLdapClient.SearchResult.Count) do
      begin
        if not Assigned(fLdapClient.SearchResult.Items[i]) then
          continue;
        GUID := ToUtf8(PGuid(fLdapClient.SearchResult.Items[i].Find('schemaIDGUID').GetRaw())^);
        Check(fGuidCache.GUIDToName(GUID, 'class') = fLdapClient.SearchResult.Items[i].Find('cn').GetReadable(), 'Class: Name: ' + fLdapClient.SearchResult.Items[i].Find('cn').GetReadable());
        Check(fGuidCache.NameToGUID(fLdapClient.SearchResult.Items[i].Find('cn').GetReadable()) = UpperCase(GUID), 'Class: GUID: ' + GUID);
        Inc(c);
      end;
    until fLdapClient.SearchCookie = '';
  finally
    fLdapClient.SearchEnd;
  end;

  fLdapClient.SearchBegin();
  try
    fLdapClient.SearchScope := lssSingleLevel;
    repeat
      fLdapClient.Search(fLdapClient.SchemaDN, False, '(objectClass=attributeSchema)', ['cn', 'lDAPDisplayName', 'attributeID', 'schemaIDGUID']);

      for i := 0 to Pred(fLdapClient.SearchResult.Count) do
      begin
        if not Assigned(fLdapClient.SearchResult.Items[i]) then
          continue;
        GUID := ToUtf8(PGuid(fLdapClient.SearchResult.Items[i].Find('schemaIDGUID').GetRaw())^);
        Check(fGuidCache.GUIDToName(GUID, 'attribute') = fLdapClient.SearchResult.Items[i].Find('cn').GetReadable(), 'Attribute: Name: ' + fLdapClient.SearchResult.Items[i].Find('cn').GetReadable());
        Check(fGuidCache.NameToGUID(fLdapClient.SearchResult.Items[i].Find('cn').GetReadable()) = UpperCase(GUID), 'Attribute: GUID: ' + GUID);
        Inc(c);
      end;
    until fLdapClient.SearchCookie = '';
  finally
    fLdapClient.SearchEnd;
  end;

  ExtendedRightsDN := FormatUtf8('CN=Extended-Rights,%', [fLdapClient.ConfigDN]);

  fLdapClient.SearchBegin();
  try
    fLdapClient.SearchScope := lssSingleLevel;
    fLdapClient.Search(ExtendedRightsDN, False, '(objectClass=controlAccessRight)', ['cn', 'displayName', 'rightsGuid', 'validAccesses', 'appliesTo']);

    for i := 0 to Pred(fLdapClient.SearchResult.Count) do
    begin
      if not Assigned(fLdapClient.SearchResult.Items[i]) then
        continue;
      GUID := fLdapClient.SearchResult.Items[i].Find('rightsGuid').GetReadable();
      TryStrToInt(fLdapClient.SearchResult.Items[i].Find('validAccesses').GetReadable(), ValidAccess);

      if ((ValidAccess and 256) <> 0) then
        Check(fGuidCache.GUIDToName(GUID, 'extendedRight') = fLdapClient.SearchResult.Items[i].Find('cn').GetReadable(), 'Extended: Name: ExtendedRight')
      else if ((ValidAccess and 16) <> 0) then
        Check(fGuidCache.GUIDToName(GUID, 'propertySet') = fLdapClient.SearchResult.Items[i].Find('cn').GetReadable(), 'Extended: Name: PropertySet')
      else if ((ValidAccess and 8) <> 0) then
        Check(fGuidCache.GUIDToName(GUID, 'validateRight') = fLdapClient.SearchResult.Items[i].Find('cn').GetReadable(), 'Extended: Name: ValidateRight')
      else
        Check(fGuidCache.GUIDToName(GUID, 'controlAccessRight') = fLdapClient.SearchResult.Items[i].Find('cn').GetReadable(), 'Extended: Name: Default type');
      Check(fGuidCache.NameToGUID(fLdapClient.SearchResult.Items[i].Find('cn').GetReadable()) = UpperCase(GUID), 'Extended: GUID: ' + GUID);
      Inc(c);
    end;
  finally
    fLdapClient.SearchEnd;
  end;
  Check(fGuidCache.Count = c);
end;

procedure TGuidCacheTest.Setup;
begin
  fLdapClient := SetupLdapClient;
end;

procedure TGuidCacheTest.MethodSetup;
begin
  fGuidCache := TGUIDCache.Create;
end;

procedure TGuidCacheTest.CleanUp;
begin
  FreeAndNil(fLdapClient);
end;

procedure TGuidCacheTest.MethodCleanUp;
begin
  FreeAndNil(fGuidCache);
end;

{ TVisAdvancedSecurityTest }

procedure TVisAdvancedSecurityTest.TestRightPanel(NodeData: PDocVariantData);
var
  AceType: TSecAceType;
  AceMask: TSecAccessMask;
  AceFlags: TSecAceFlags;
begin
  AceType := TSecAceType(Integer(NodeData^.I['ace_type']));
  if (satObjectAccessAllowed = AceType) or (satAccessAllowed = AceType) then
    Check(fVis.ComboBox_Type.Text = rsAllow, 'ACE Type')
  else
    Check(fVis.ComboBox_Type.Text = rsDeny, 'ACE Type');

  Check(fVis.Edit_Principal.Text = NodeData^.S['account'], 'ACE Principal');

  AceMask := TSecAccessMask(Integer(NodeData^.I['rights']));
  Check(fVis.CheckBoxCC.Checked = (samCreateChild in AceMask), 'Create Child');
  Check(fVis.CheckBoxDC.Checked = (samDeleteChild in AceMask), 'Delete Child');
  Check(fVis.CheckBoxLC.Checked = (samListChildren in AceMask), 'List Children');
  Check(fVis.CheckBoxSW.Checked = (samSelfWrite in AceMask), 'Self Write');
  Check(fVis.CheckBoxRP.Checked = (samReadProp in AceMask), 'Read Prop');
  Check(fVis.CheckBoxWP.Checked = (samWriteProp in AceMask), 'Write Prop');
  Check(fVis.CheckBoxDT.Checked = (samDeleteTree in AceMask), 'Delete Tree');
  Check(fVis.CheckBoxLO.Checked = (samListObject in AceMask), 'List Object');
  Check(fVis.CheckBoxCA.Checked = (samControlAccess in AceMask), 'Control Access');
  Check(fVis.CheckBoxD.Checked = (samDelete in AceMask), 'Delete');
  Check(fVis.CheckBoxRC.Checked = (samReadControl in AceMask), 'Read Control');
  Check(fVis.CheckBoxWDAC.Checked = (samWriteDac in AceMask), 'Write DAC');
  Check(fVis.CheckBoxWO.Checked = (samWriteOwner in AceMask), 'Write Owner');

  Check(fVis.ComboBox_Object.Text = NodeData^.S['object'], 'ACE Object');

  AceFlags := TSecAceFlags(Int8(NodeData^.I['ace_flags']));
  Check(fVis.CheckBoxOI.Checked = (safObjectInherit in AceFlags), 'Object Inherit');
  Check(fVis.CheckBoxCI.Checked = (safContainerInherit in AceFlags), 'Container Inherit');
  Check(fVis.CheckBoxIO.Checked = (safInheritOnly in AceFlags), 'Inherit Only');
  Check(fVis.CheckBoxNP.Checked = (safNoPropagateInherit in AceFlags), 'No Propagate Inherit');

  Check(fVis.ComboBox_ObjectInheritance.Text = NodeData^.S['inheritedObject'], 'ACE Object Inheritance');
end;

procedure TVisAdvancedSecurityTest.Creation;
begin
  Check(fVis.Caption = FormatUtf8(rsVisAdvancedSecurityTitle, ['']));
  Check(fVis.ComboBox_Type.Items.Count = 2);
  Check(fVis.ComboBox_Type.Items[0] = rsAllow);
  Check(fVis.ComboBox_Type.Items[1] = rsDeny);
  Check(fVis.TisGrid1.Data.Count = 0);
  Check(fVis.TisGrid1.Header.Columns.Count = 6);
  Check(fVis.ObjectName = '');
  Check(fVis.ObjectDN = '');
  Check(not fVis.SDChanged);
  Check(not Assigned(fVis.LdapClient));

  Check(fVis.BitBtn_Owner.Action = fVis.Action_SelectOwner);
  Check(fVis.BitBtn_Owner.Caption = 'Select');
  Check(fVis.BitBtn_Group.Action = fVis.Action_SelectGroup);
  Check(fVis.BitBtn_Group.Caption = 'Select');
  Check(fVis.BitBtn_Add.Action = fVis.Action_AddACE);
  Check(fVis.BitBtn_Add.Caption = 'Add');
  Check(fVis.BitBtn_Duplicate.Action = fVis.Action_DuplicateACE);
  Check(fVis.BitBtn_Duplicate.Caption = 'Duplicate');
  Check(fVis.BitBtn_Delete.Action = fVis.Action_DeleteACE);
  Check(fVis.BitBtn_Delete.Caption = 'Delete');
  Check(fVis.BitBtn_Principal.Action = fVis.Action_SelectPrincipal);
  Check(fVis.BitBtn_Principal.Caption = 'Select');
  Check(fVis.BitBtn_Object.Action = fVis.Action_SelectObject);
  Check(fVis.BitBtn_Object.Caption = 'Select');
  Check(fVis.BitBtn_ObjectInheritance.Action = fVis.Action_SelectInheritedObject);
  Check(fVis.BitBtn_ObjectInheritance.Caption = 'Select');
  Check(fVis.BitBtn_OK.Action = fVis.Action_OK);
  check(fVis.BitBtn_OK.Caption = 'OK');
  Check(fVis.BitBtn_Cancel.Action = fVis.Action_Cancel);
  Check(fVis.BitBtn_Cancel.Caption = 'Cancel');
  Check(fVis.BitBtn_Apply.Action = fVis.Action_Apply);
  Check(fVis.BitBtn_Apply.Caption = 'Apply');
end;

procedure TVisAdvancedSecurityTest.Action_SelectOwner;
begin
  Check(Assigned(fVis.Action_SelectOwner.OnExecute), 'OnExecute assigned');
  Check(Assigned(fVis.Action_SelectOwner.OnUpdate), 'OnUpdate assigned');

  Check(fVis.Action_SelectOwner.Caption = 'Select');
  Check(fVis.Action_SelectOwner.Update, 'Update');
  Check(fVis.Action_SelectOwner.Enabled, 'Enabled');
end;

procedure TVisAdvancedSecurityTest.Action_SelectGroup;
begin
  Check(not Assigned(fVis.Action_SelectGroup.OnExecute), 'OnExecute not assigned');
  Check(Assigned(fVis.Action_SelectGroup.OnUpdate), 'OnUpdate assigned');

  Check(fVis.Action_SelectGroup.Caption = 'Select');
  Check(fVis.Action_SelectGroup.Update, 'Update');
  Check(not fVis.Action_SelectGroup.Enabled, 'Not enabled');
end;

procedure TVisAdvancedSecurityTest.Action_AddACE;
const
  ADD_COUNT = 100;
var
  NodeData: PDocVariantData;
  i: Integer;
begin
  Check(Assigned(fVis.Action_AddACE.OnExecute), 'OnExecute assigned');
  Check(Assigned(fVis.Action_AddACE.OnUpdate), 'OnUpdate assigned');

  Check(fVis.Action_AddACE.Caption = 'Add');
  Check(fVis.Action_AddACE.Update, 'Update');
  Check(fVis.Action_AddACE.Enabled, 'Enabled');

  Check(Length(fVis.SecurityDescriptor.Dacl) = 0, 'Empty DACL before add');
  Check(fVis.TisGrid1.TotalCount = 0, 'No data in grid before add');
  Check(not Assigned(fVis.TisGrid1.FocusedNode), 'No row selected in grid before add');
  Check(fVis.Action_AddACE.Execute, 'Execute');
  Check(Length(fVis.SecurityDescriptor.Dacl) = 0, 'Empty DACL after add');
  Check(fVis.TisGrid1.TotalCount = 1, 'New row added to grid');
  Check(Assigned(fVis.TisGrid1.FocusedNode), 'New row selected');
  NodeData := fVis.TisGrid1.GetNodeAsPDocVariantData(fVis.TisGrid1.FocusedNode);
  Check(Assigned(NodeData), 'Data assigned to node');
  Check(NodeData^.Exists('index'), 'Index exists in added data');
  Check(NodeData^.I['index'] = 0, 'Index is 0');
  Check(NodeData^.Exists('state'), 'State exists in added data');
  Check(NodeData^.I['state'] = 1, 'State is 1 (added)');

  for i := 1 to ADD_COUNT do
  begin
    Check(fVis.Action_AddACE.Execute, 'Execute');
    Check(Length(fVis.SecurityDescriptor.Dacl) = 0, 'Empty DACL after add');
    Check(fVis.TisGrid1.TotalCount = i + 1, 'New row added to grid');
    Check(Assigned(fVis.TisGrid1.FocusedNode), 'New row selected');
    NodeData := fVis.TisGrid1.GetNodeAsPDocVariantData(fVis.TisGrid1.FocusedNode);
    Check(Assigned(NodeData), 'Data assigned to node');
    Check(NodeData^.Exists('index'), 'Index exists in added data');
    Check(NodeData^.I['index'] = i, 'Index is 0');
    Check(NodeData^.Exists('state'), 'State exists in added data');
    Check(NodeData^.I['state'] = 1, 'State is 1 (added)');
  end;
end;

procedure TVisAdvancedSecurityTest.Action_DuplicateACE;
const
  DUPLICATE_COUNT = 100;
var
  NodeData, BaseData: PDocVariantData;
  i: Integer;
  v: RawUtf8;
  BaseNode: PVirtualNode;
begin
  Check(Assigned(fVis.Action_DuplicateACE.OnExecute), 'OnExecute assigned');
  Check(Assigned(fVis.Action_DuplicateACE.OnUpdate), 'OnUpdate assigned');

  Check(fVis.Action_DuplicateACE.Caption = 'Duplicate');
  Check(fVis.Action_DuplicateACE.Update, 'Update');
  Check(not fVis.Action_DuplicateACE.Enabled, 'Not enabled');

  fVis.Action_AddACE.Execute;
  Check(fVis.Action_DuplicateACE.Update, 'Update after add');
  Check(fVis.Action_DuplicateACE.Enabled, 'Enabled');
  BaseNode := fVis.TisGrid1.FocusedNode;
  NodeData := fVis.TisGrid1.GetNodeAsPDocVariantData(fVis.TisGrid1.FocusedNode);
  Check(Assigned(NodeData));
  if not Assigned(NodeData) then
    Exit;

  NodeData^.I['ace_type'] := 1;
  NodeData^.S['account_sid'] := 'S-1-1-0';
  NodeData^.I['rights'] :=  Integer([samReadProp, samWriteProp]);
  NodeData^.S['object_guid'] := 'ab721a54-1e2f-11d0-9819-00aa0040529b';
  NodeData^.I['ace_flags'] := Integer([safInheritOnly, safContainerInherit]);
  NodeData^.S['inherited_object_guid'] := 'bf967aba-0de6-11d0-a285-00aa003049e2';
  BaseData := fVis.TisGrid1.GetNodeAsPDocVariantData(BaseNode);

  Check(Length(fVis.SecurityDescriptor.Dacl) = 0, 'Empty DACL before add');
  Check(fVis.TisGrid1.TotalCount = 1, 'Data in grid before add');
  Check(Assigned(fVis.TisGrid1.FocusedNode), 'Row selected in grid before add');

  Check(fVis.Action_DuplicateACE.Execute, 'Execute');
  Check(Length(fVis.SecurityDescriptor.Dacl) = 0, 'Empty DACL after duplicate');
  Check(fVis.TisGrid1.TotalCount = 2, 'New row duplicated to grid');
  Check(Assigned(fVis.TisGrid1.FocusedNode), 'New row selected');
  NodeData := fVis.TisGrid1.GetNodeAsPDocVariantData(fVis.TisGrid1.FocusedNode);
  Check(Assigned(NodeData), 'Data assigned to node');
  Check(NodeData^.Exists('index'), 'Index exists in duplicated data');
  Check(NodeData^.I['index'] = 1, 'Index is 0');
  Check(NodeData^.Exists('state'), 'State exists in duplicated data');
  Check(NodeData^.I['state'] = 1, 'State is 1 (added)');
  Check(NodeData^.I['ace_type'] = BaseData^.I['ace_type'], 'Compare ace_type');
  Check(NodeData^.S['account_sid'] = BaseData^.S['account_sid'], 'Compare account_sid');
  Check(NodeData^.I['rights'] = BaseData^.I['rights'], 'Compare rights');
  Check(NodeData^.S['object_guid'] = BaseData^.S['object_guid'], 'Compare object_guid');
  Check(NodeData^.I['ace_flags'] = BaseData^.I['ace_flags'], 'Compare ace_flags');
  Check(NodeData^.S['inherited_object_guid'] = BaseData^.S['inherited_object_guid'], 'Compare inherited_object_guid');

  for i := 2 to DUPLICATE_COUNT do
  begin
    Check(fVis.Action_DuplicateACE.Execute, 'Execute');
    Check(Length(fVis.SecurityDescriptor.Dacl) = 0, 'Empty DACL after duplicate');
    Check(fVis.TisGrid1.TotalCount = i + 1, 'New row duplicated to grid');
    Check(Assigned(fVis.TisGrid1.FocusedNode), 'New row selected');
    BaseData := fVis.TisGrid1.GetNodeAsPDocVariantData(BaseNode);
    NodeData := fVis.TisGrid1.GetNodeAsPDocVariantData(fVis.TisGrid1.FocusedNode);
    Check(Assigned(NodeData), 'Data assigned to node');
    Check(NodeData^.Exists('index'), 'Index exists in duplicated data');
    Check(NodeData^.I['index'] = i, 'Index is 0');
    Check(NodeData^.Exists('state'), 'State exists in duplicated data');
    Check(NodeData^.I['state'] = 1, 'State is 1 (added)');
    Check(NodeData^.I['ace_type'] = BaseData^.I['ace_type'], 'Compare ace_type');
    Check(NodeData^.S['account_sid'] = BaseData^.S['account_sid'], 'Compare account_sid');
    Check(NodeData^.I['rights'] = BaseData^.I['rights'], 'Compare rights');
    Check(NodeData^.S['object_guid'] = BaseData^.S['object_guid'], 'Compare object_guid');
    Check(NodeData^.I['ace_flags'] = BaseData^.I['ace_flags'], 'Compare ace_flags');
    Check(NodeData^.S['inherited_object_guid'] = BaseData^.S['inherited_object_guid'], 'Compare inherited_object_guid');
  end;
end;

procedure TVisAdvancedSecurityTest.Action_DeleteACE;
const
  DELETE_COUNT = 100;
var
  i: Integer;
  NodeData: PDocVariantData;
  Node: PVirtualNode;
begin
  Check(Assigned(fVis.Action_DeleteACE.OnExecute), 'OnExecute assigned');
  Check(Assigned(fVis.Action_DeleteACE.OnUpdate), 'OnUpdate assigned');

  Check(fVis.Action_DeleteACE.Caption = 'Delete');
  Check(fVis.Action_DeleteACE.Update, 'Update');
  Check(not fVis.Action_DeleteACE.Enabled, 'Not enabled');

  fVis.Action_AddACE.Execute;
  Check(fVis.Action_DeleteACE.Update, 'Update after add');
  Check(fVis.Action_DeleteACE.Enabled, 'Enabled');
  Check(fVis.TisGrid1.TotalCount = 1, '1 row in data');
  Check(fVis.Action_DeleteACE.Execute, 'Delete data');
  Check(fVis.TisGrid1.TotalCount = 1, 'Data not deleted');
  NodeData := fVis.TisGrid1.GetNodeAsPDocVariantData(fVis.TisGrid1.FocusedNode);
  Check(Assigned(NodeData), 'NodeData retrieved');
  if not Assigned(NodeData) then
    Exit;
  Check(NodeData^.Exists('state') and (nodeData^.I['state'] = 3), 'NodeData state changed');

  fVis.Action_AddACE.Execute;
  NodeData := fVis.TisGrid1.GetNodeAsPDocVariantData(fVis.TisGrid1.FocusedNode);
  Check(Assigned(NodeData));
  if not Assigned(NodeData) then
    Exit;

  NodeData^.I['ace_type'] := 0;
  NodeData^.S['account_sid'] := 'S-1-1-0';
  NodeData^.I['rights'] :=  Integer([samReadProp, samWriteProp]);
  NodeData^.S['object_guid'] := 'ab721a54-1e2f-11d0-9819-00aa0040529b';
  NodeData^.I['ace_flags'] := Integer([safInheritOnly, safContainerInherit]);
  NodeData^.S['inherited_object_guid'] := 'bf967aba-0de6-11d0-a285-00aa003049e2';
  for i := 1 to DELETE_COUNT do
    fVis.Action_DuplicateACE.Execute;

  Node := fVis.TisGrid1.GetFirst();
  Node := fVis.TisGrid1.GetNext(Node); // Skip first, already deleted
  while Assigned(Node) do
  begin
    fVis.TisGrid1.FocusedNode := Node;
    NodeData := fVis.TisGrid1.GetNodeAsPDocVariantData(fVis.TisGrid1.FocusedNode);
    Check(Assigned(NodeData) and NodeData^.Exists('state'), 'state exists in nodeData');
    Check(NodeData^.I['state'] = 1, 'NodeData is in added state');
    Check(fVis.Action_DeleteACE.Execute, 'Delete');
    Check(NodeData^.I['state'] = 3, 'NodeData is in delete state');
    Node := fVis.TisGrid1.GetNext(Node);
  end;
end;

procedure TVisAdvancedSecurityTest.Action_SelectPrincipal;
begin
  Check(Assigned(fVis.Action_SelectPrincipal.OnExecute), 'OnExecute assigned');
  Check(Assigned(fVis.Action_SelectPrincipal.OnUpdate), 'OnUpdate assigned');

  Check(fVis.Action_SelectPrincipal.Caption = 'Select');
  Check(fVis.Action_SelectPrincipal.Update, 'Update');
  Check(fVis.Action_SelectPrincipal.Enabled, 'Enabled');
end;

procedure TVisAdvancedSecurityTest.Action_SelectObject;
begin
  Check(Assigned(fVis.Action_SelectObject.OnExecute), 'OnExecute assigned');
  Check(Assigned(fVis.Action_SelectObject.OnUpdate), 'OnUpdate assigned');

  Check(fVis.Action_SelectObject.Caption = 'Select');
  Check(fVis.Action_SelectObject.Update, 'Update');
  Check(fVis.Action_SelectObject.Enabled, 'Enabled');
end;

procedure TVisAdvancedSecurityTest.Action_SelectInheritedObject;
begin
  Check(Assigned(fVis.Action_SelectInheritedObject.OnExecute), 'OnExecute assigned');
  Check(Assigned(fVis.Action_SelectInheritedObject.OnUpdate), 'OnUpdate assigned');

  Check(fVis.Action_SelectInheritedObject.Caption = 'Select');
  Check(fVis.Action_SelectInheritedObject.Update, 'Update');
  Check(fVis.Action_SelectInheritedObject.Enabled, 'Enabled');
end;

procedure TVisAdvancedSecurityTest.Action_Apply;
begin
  Check(Assigned(fVis.Action_Apply.OnExecute), 'OnExecute assigned');
  Check(Assigned(fVis.Action_Apply.OnUpdate), 'OnUpdate assigned');

  Check(fVis.Action_Apply.Caption = 'Apply');
  Check(fVis.Action_Apply.Update, 'Update');
  Check(not fVis.Action_Apply.Enabled, 'Not enabled');
end;

procedure TVisAdvancedSecurityTest.Action_Cancel;
begin
  Check(Assigned(fVis.Action_Cancel.OnExecute), 'OnExecute assigned');
  Check(Assigned(fVis.Action_Cancel.OnUpdate), 'OnUpdate assigned');

  Check(fVis.Action_Cancel.Caption = 'Cancel');
  Check(fVis.Action_Cancel.Update, 'Update');
  Check(fVis.Action_Cancel.Enabled, 'Enabled');
end;

procedure TVisAdvancedSecurityTest.Action_OK;
begin
  Check(Assigned(fVis.Action_OK.OnExecute), 'OnExecute assigned');
  Check(Assigned(fVis.Action_OK.OnUpdate), 'OnUpdate assigned');

  Check(fVis.Action_OK.Caption = 'OK');
  Check(fVis.Action_OK.Update, 'Update');
  Check(fVis.Action_OK.Enabled, 'Enabled');
end;

procedure TVisAdvancedSecurityTest.ChangeObjectName;
begin
  fVis.ObjectName := 'testa';
  Check(fVis.ObjectName = 'testa');
  Check(fVis.Caption = FormatUtf8(rsVisAdvancedSecurityTitle, ['testa']));
  fVis.ObjectName := 'userb';
  Check(fVis.ObjectName = 'userb');
  Check(fVis.Caption = FormatUtf8(rsVisAdvancedSecurityTitle, ['userb']));
end;

procedure TVisAdvancedSecurityTest.ChangeObjectDN;
begin
  fVis.ObjectDN := 'OU=test,DC=openrsat,DC=lan';
  Check(fVis.ObjectDN = 'OU=test,DC=openrsat,DC=lan');
  fVis.ObjectDN := 'OU=test2,DC=test,DC=openrsat,DC=lan';
  Check(fVis.ObjectDN = 'OU=test2,DC=test,DC=openrsat,DC=lan');
end;

procedure TVisAdvancedSecurityTest.ChangeLdapClient;
begin
  Check(not Assigned(fVis.LdapClient));
  fVis.LdapClient := fLdapClient;
  Check(Assigned(fVis.LdapClient));
  fVis.LdapClient := nil;
  Check(not Assigned(fVis.LdapClient));
end;

procedure TVisAdvancedSecurityTest.SetSecurityDescriptor;
var
  NodeData: PDocVariantData;
  Node: PVirtualNode;
  idx: Int64;
  sid: RawUtf8;
begin
  Check(fVis.Edit_Owner.Text = '', 'Empty owner');
  Check(fVis.Edit_Group.Text = '', 'Empty group');
  Check(fVis.TisGrid1.TotalCount = 0, 'Empty data');

  fVis.SecurityDescriptor := fSecurityDescriptor;
  Check(not fVis.SDChanged);

  Check(fVis.Edit_Owner.Text = RawSidToText(fSecurityDescriptor.Owner), 'Owner text');
  Check(fVis.Edit_Group.Text = RawSidToText(fSecurityDescriptor.Group), 'Group text');
  Check(fVis.TisGrid1.Data.Count = Length(fSecurityDescriptor.Dacl), 'Grid data count');

  Check(Assigned(fVis.TisGrid1.FocusedNode));
  NodeData := fVis.TisGrid1.GetNodeAsPDocVariantData(fVis.TisGrid1.FocusedNode);
  Check(Assigned(NodeData));
  Check(NodeData^.Exists('index') and (NodeData^.I['index'] = 0), 'ACE index');
  Check(not NodeData^.Exists('state'), 'No state');

  TestRightPanel(NodeData);

  // No LDAPClient, no resolution
  Node := fVis.TisGrid1.GetFirst();
  while Assigned(Node) do
  begin
    NodeData := fVis.TisGrid1.GetNodeAsPDocVariantData(Node);

    idx := NodeData^.I['index'];
    Check(NodeData^.I['ace_type'] = Ord(fSecurityDescriptor.Dacl[idx].AceType), 'ACE type');
    Check(NodeData^.S['account_sid'] = RawSidToText(fSecurityDescriptor.Dacl[idx].Sid), 'ACE sid');
    Check(NodeData^.I['rights'] = Integer(fSecurityDescriptor.Dacl[idx].Mask), 'ACE rights');
    Check(NodeData^.S['object_guid'] = fSecurityDescriptor.Dacl[idx].ObjectText(), FormatUtf8('ACE object: (% = %)', [NodeData^.S['object_guid'], ToUtf8(fSecurityDescriptor.Dacl[idx].ObjectType)]));
    Check(NodeData^.I['ace_flags'] = Int8(fSecurityDescriptor.Dacl[idx].Flags), 'ACE flags');
    Check(NodeData^.S['inherited_object_guid'] = fSecurityDescriptor.Dacl[idx].InheritedText(), 'ACE object inherited');

    Check(NodeData^.B['type'] = True, 'ACE bool type');
    if SidToKnown(RawSidToText(fSecurityDescriptor.Dacl[idx].Sid)) = wksNull then
      Check(NodeData^.S['account'] = RawSidToText(fSecurityDescriptor.Dacl[idx].Sid), FormatUtf8('ACE text sid (nil): (% = %)', [NodeData^.S['account'], RawSidToText(fSecurityDescriptor.Dacl[idx].Sid)]))
    else
      Check(NodeData^.S['account'] = WELL_KNOWN_SID_NAMES[SidToKnown(RawSidToText(fSecurityDescriptor.Dacl[idx].Sid))], 'ACE text sid');
    Check(NodeData^.S['permissions'] = AccessMaskToString(fSecurityDescriptor.Dacl[idx].Mask), 'ACE permissions');
    if IsNullGuid(fSecurityDescriptor.Dacl[idx].ObjectType) then
      Check(NodeData^.S['object'] = '', FormatUtf8('ACE text object (nil): (% = %)', [NodeData^.S['object'], '']))
    else
      Check(NodeData^.S['object'] = fSecurityDescriptor.Dacl[idx].ObjectText(), FormatUtf8('ACE text object: (% = %)', [NodeData^.S['object'], ToUtf8(fSecurityDescriptor.Dacl[idx].ObjectType)]));
    Check(NodeData^.S['flags'] = InheritanceFlagsToString(fSecurityDescriptor.Dacl[idx].Flags), 'ACE text flags');
    if IsNullGuid(fSecurityDescriptor.Dacl[idx].InheritedObjectType) then
      Check(NodeData^.S['inheritedObject'] = '', 'ACE text object inherited (nil)')
    else
      Check(NodeData^.S['inheritedObject'] = fSecurityDescriptor.Dacl[idx].InheritedText(), 'ACE text object inherited');
    Node := fVis.TisGrid1.GetNext(Node);
  end;

  // Change LDAPClient
  fVis.SecurityDescriptor.Clear;
  fVis.LdapClient := fLdapClient;
  fVis.SecurityDescriptor := fSecurityDescriptor;
  Check(not fVis.SDChanged);
  Check(fVis.Edit_Owner.Text = fLdapClient.SearchObject(fLdapClient.DefaultDN, FormatUtf8('(objectSid=%)', [LdapEscape(RawSidToText(fSecurityDescriptor.Owner))]), 'name', lssWholeSubtree).GetReadable(), 'Owner name');
  Check(fVis.Edit_Group.Text = fLdapClient.SearchObject(fLdapClient.DefaultDN, FormatUtf8('(objectSid=%)', [LdapEscape(RawSidToText(fSecurityDescriptor.Group))]), 'name', lssWholeSubtree).GetReadable(), 'Group name');
  Check(fVis.TisGrid1.Data.Count = Length(fSecurityDescriptor.Dacl));

  Node := fVis.TisGrid1.GetFirst();
  while Assigned(Node) do
  begin
    NodeData := fVis.TisGrid1.GetNodeAsPDocVariantData(Node);

    idx := NodeData^.I['index'];
    Check(NodeData^.I['ace_type'] = Ord(fSecurityDescriptor.Dacl[idx].AceType), 'ACE type');
    Check(NodeData^.S['account_sid'] = RawSidToText(fSecurityDescriptor.Dacl[idx].Sid), 'ACE sid');
    Check(NodeData^.I['rights'] = Integer(fSecurityDescriptor.Dacl[idx].Mask), 'ACE rights');
    Check(NodeData^.S['object_guid'] = fSecurityDescriptor.Dacl[idx].ObjectText(), 'ACE object');
    Check(NodeData^.I['ace_flags'] = Int8(fSecurityDescriptor.Dacl[idx].Flags), 'ACE flags');
    Check(NodeData^.S['inherited_object_guid'] = fSecurityDescriptor.Dacl[idx].InheritedText(), 'ACE object inherited');

    Check(NodeData^.B['type'] = True, 'ACE bool type');
    if SidToKnown(RawSidToText(fSecurityDescriptor.Dacl[idx].Sid)) = wksNull then
    begin
      sid := fLdapClient.SearchObject(fLdapClient.DefaultDN(), FormatUtf8('(objectSid=%)', [LdapEscape(RawSidToText(fSecurityDescriptor.Dacl[idx].Sid))]), 'name', lssWholeSubtree).GetReadable();
      if sid = '' then
        Check(NodeData^.S['account'] = RawSidToText(fSecurityDescriptor.Dacl[idx].Sid), 'ACE text sid (nil)')
      else
        Check(NodeData^.S['account'] = sid, 'ACE text sid (nil)');
    end
    else
      Check(NodeData^.S['account'] = WELL_KNOWN_SID_NAMES[SidToKnown(RawSidToText(fSecurityDescriptor.Dacl[idx].Sid))], 'ACE text sid');
    Check(NodeData^.S['permissions'] = AccessMaskToString(fSecurityDescriptor.Dacl[idx].Mask), 'ACE permissions');
    if IsNullGuid(fSecurityDescriptor.Dacl[idx].ObjectType) then
      Check(NodeData^.S['object'] = '', 'ACE text object (nil)');
    //else Missing name resolution for guid
    //  Check(NodeData^.S['object'] = ToUtf8(fSecurityDescriptor.Dacl[idx].ObjectType), 'ACE text object');
    Check(NodeData^.S['flags'] = InheritanceFlagsToString(fSecurityDescriptor.Dacl[idx].Flags), 'ACE text flags');
    if IsNullGuid(fSecurityDescriptor.Dacl[idx].InheritedObjectType) then
      Check(NodeData^.S['inheritedObject'] = '', 'ACE text object inherited (nil)');
    //else Missing name resolution for guid
    //  Check(NodeData^.S['inheritedObject'] = ToUtf8(fSecurityDescriptor.Dacl[idx].InheritedObjectType), 'ACE text object inherited');
    Node := fVis.TisGrid1.GetNext(Node);
  end;
end;

procedure TVisAdvancedSecurityTest.ChangeOwner;
var
  Obj: TLdapResult;
begin
  fVis.SecurityDescriptor := fSecurityDescriptor;

  // Missing LDAPCLIENT
  try
    fVis.ChangeOwner('');
    Check(False, 'LDAPClient should be missing.');
  except
    On E: TVisAdvancedSecurityException do Check(E.Message = 'Missing LdapClient.');
  end;
  fVis.LdapClient := fLdapClient;
  // Cannot be empty
  try
    fVis.ChangeOwner('');
    Check(False, 'Owner should be empty.');
  except
    On E: TVisAdvancedSecurityException do Check(E.Message = 'Empty objectName to change SD owner.');
  end;
  // Cannot find entry
  try
    fVis.ChangeOwner('CN=InvalidEntry,DC=fake,DC=lan');
    Check(False, 'Owner should be invalid entry.');
  except
    on E: TVisAdvancedSecurityException do Check(E.Message = 'Cannot retrieve objectName to change SD owner.');
  end;
  Obj := TLdapResult(fLdapClient.SearchObject(fLdapClient.DefaultDN, FormatUtf8('(sAMAccountName=%)', [LdapEscape('Domain Users')]), ['objectSid'], lssWholeSubtree).Clone);
  try
    fVis.ObjectDN := FormatUtf8('CN=%,%', [ClassName, BaseDN]);
    fVis.ChangeOwner(Obj.ObjectName);
    Check(fVis.SDChanged);
  finally
    FreeAndNil(Obj);
  end;
end;

procedure TVisAdvancedSecurityTest.ChangePrincipal;
var
  Obj: TLdapResult;
begin
  // Missing LDAPCLIENT
  try
    fVis.ChangePrincipal('');
    Check(False, 'LDAPClient should be missing.');
  except
    On E: TVisAdvancedSecurityException do Check(E.Message = 'Missing LdapClient.');
  end;
  fVis.LdapClient := fLdapClient;
  // Cannot be empty
  try
    fVis.ChangePrincipal('');
    Check(False, 'Principal should be empty.');
  except
    On E: TVisAdvancedSecurityException do Check(E.Message = 'Empty objectName to change SD owner.');
  end;
  // Cannot find entry
  try
    fVis.ChangePrincipal('CN=InvalidEntry,DC=fake,DC=lan');
    Check(False, 'Principal should be invalid entry.');
  except
    on E: TVisAdvancedSecurityException do Check(E.Message = 'Cannot retrieve objectName to change SD owner.');
  end;
  Obj := TLdapResult(fLdapClient.SearchObject(fLdapClient.DefaultDN, FormatUtf8('(sAMAccountName=%)', [LdapEscape('Domain Users')]), ['objectSid'], lssWholeSubtree).Clone);
  try
    fVis.ObjectDN := FormatUtf8('CN=%,%', [ClassName, BaseDN]);
    fVis.ChangePrincipal(Obj.ObjectName);
  finally
    FreeAndNil(Obj);
  end;
end;

procedure TVisAdvancedSecurityTest.Setup;
var
  ObjectClassAttr, Attr: TLdapAttribute;
  Attributes: TLdapAttributeList;
  UserDN: RawUtf8;
begin
  fLdapClient := (SetupLdapClient as TRsatLdapClient);
  BaseDN := GetBaseDN(fLdapClient.DefaultDN());

  if Assigned(fLdapClient) then
    fLdapClient.Delete(FormatUtf8('CN=%,%', [ClassName, BaseDN]));

  // Create User and Get Security Descriptor
  Attributes := TLdapAttributeList.Create;
  try
    Attr := Attributes.Add('objectClass', 'top');
    Attr.Add('person');
    Attr.Add('organizationalPerson');
    Attr.Add('user');
    Attributes.Add('sAMAccountName', ClassName);
    UserDN := FormatUtf8('CN=%,%', [ClassName, BaseDN]);
    if not fLdapClient.Add(UserDN, Attributes) then
      raise TVisAdvancedSecurityTestException.Create(FormatUtf8('Failed to create user. (%)', [fLdapClient.ResultString]));
    Attr := fLdapClient.SearchObject(UserDN, '', 'nTSecurityDescriptor');
    if not Assigned(Attr) then
      raise TVisAdvancedSecurityTestException.Create(FormatUtf8('Cannot retrieve SD. (%)', [fLdapClient.ResultString]));
    if not fSecurityDescriptor.FromBinary(Attr.GetRaw()) then
      raise TVisAdvancedSecurityTestException.Create('Cannot convert attribute to SD.');
  finally
    FreeAndNil(Attributes);
  end;
end;

procedure TVisAdvancedSecurityTest.CleanUp;
begin
  if Assigned(fLdapClient) then
    fLdapClient.Delete(FormatUtf8('CN=%,%', [ClassName, BaseDN]));

  FreeAndNil(fLdapClient);
end;

procedure TVisAdvancedSecurityTest.MethodSetup;
begin
  fVis := TVisAdvancedSecurity.Create(Nil);
end;

procedure TVisAdvancedSecurityTest.MethodCleanUp;
begin
  if Assigned(fVis) then
    FreeAndNil(fVis);
end;

end.

