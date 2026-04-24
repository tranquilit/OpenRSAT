unit uvisadvancedsecurity;

{$mode ObjFPC}{$H+}

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
  Buttons,
  ActnList,
  tis.ui.grid.core,
  mormot.core.os.security,
  mormot.core.variants,
  VirtualTrees,
  mormot.core.base,
  mormot.core.text,
  mormot.net.ldap,
  mormot.core.collections,
  ucoredatamodule,
  ursatldapclient,
  ucommon;

type

  TVisAdvancedSecurityException = class(Exception);

  { TVisAdvancedSecurityMissingNodeDataException }

  TVisAdvancedSecurityMissingNodeDataException = class(TVisAdvancedSecurityException)
    constructor Create; reintroduce;
  end;

  { TVisAdvancedSecurityMissingLdapClientException }

  TVisAdvancedSecurityMissingLdapClientException = class(TVisAdvancedSecurityException)
    constructor Create; reintroduce;
  end;

  { TSidCache }

  TSidCache = Class
  private
    fSIDS: TRawUtf8DynArray;
    fNames: TRawUtf8DynArray;

    function BuildFilter: RawUtf8;
    function GetCount: Integer;
    procedure ResolveKnownSIDS;
    procedure UpdateSIDS(ALdapResult: TLdapResultList);
    function IndexOfSID(ASID: RawUtf8): Integer;
    function IndexOfName(AName: RawUtf8): Integer;
  public
    procedure ResolveSIDS(ALdapClient: TLdapClient);
    procedure AddSID(ASID: RawUtf8);
    procedure SetSIDName(ASID, AName: RawUtf8);
    function SIDToName(ASID: RawUtf8): RawUtf8;
    function NameToSID(AName: RawUtf8): RawUtf8;

    procedure Clear;
    property Count: Integer read GetCount;
  end;

  { TGUIDCache }

  TGUIDCache = class
  private
    fGUIDS: TRawUtf8DynArray;
    fNames: TRawUtf8DynArray;
    fTypes: TRawUtf8DynArray;

    function GetCount: Integer;
    procedure UpdateFromClassSchema(ALdapResult: TLdapResultList);
    procedure UpdateFromAttributeSchema(ALdapResult: TLdapResultList);
    procedure UpdateFromExtendedRights(ALdapResult: TLdapResultList);
    function IndexOfGUID(AGUID, AType: RawUtf8): Integer;
    function IndexOfName(AName: RawUtf8): Integer;
  public
    procedure ResolveGUIDS(ALdapClient: TLdapClient);
    function AddGUID(AGUID, AType: RawUtf8): Integer;
    procedure SetGUIDName(AGUID, AName, AType: RawUtf8);
    function GUIDToName(AGUID, AType: RawUtf8): RawUtf8;
    function NameToGUID(AName: RawUtf8): RawUtf8;

    procedure Clear;
    function Dump: RawUtf8;
    property Count: Integer read GetCount;
  end;

  { TVisAdvancedSecurity }

  TVisAdvancedSecurity = class(TForm)
    Action_Apply: TAction;
    Action_Cancel: TAction;
    Action_OK: TAction;
    Action_SelectOwner: TAction;
    Action_SelectGroup: TAction;
    Action_AddACE: TAction;
    Action_DuplicateACE: TAction;
    Action_DeleteACE: TAction;
    Action_SelectPrincipal: TAction;
    Action_SelectObject: TAction;
    Action_SelectInheritedObject: TAction;
    ActionList1: TActionList;
    BitBtn_Apply: TBitBtn;
    BitBtn_Cancel: TBitBtn;
    BitBtn_OK: TBitBtn;
    BitBtn_Group: TBitBtn;
    BitBtn_Owner: TBitBtn;
    BitBtn_Principal: TBitBtn;
    BitBtn_Add: TBitBtn;
    BitBtn_Duplicate: TBitBtn;
    BitBtn_Delete: TBitBtn;
    BitBtn_Object: TBitBtn;
    BitBtn_ObjectInheritance: TBitBtn;
    CheckBoxCC: TCheckBox;
    CheckBoxD: TCheckBox;
    CheckBoxRC: TCheckBox;
    CheckBoxWO: TCheckBox;
    CheckBoxWDAC: TCheckBox;
    CheckBoxOI: TCheckBox;
    CheckBoxCI: TCheckBox;
    CheckBoxIO: TCheckBox;
    CheckBoxNP: TCheckBox;
    CheckBoxDC: TCheckBox;
    CheckBoxLC: TCheckBox;
    CheckBoxSW: TCheckBox;
    CheckBoxRP: TCheckBox;
    CheckBoxWP: TCheckBox;
    CheckBoxDT: TCheckBox;
    CheckBoxLO: TCheckBox;
    CheckBoxCA: TCheckBox;
    ComboBox_Type: TComboBox;
    ComboBox_ObjectInheritance: TComboBox;
    ComboBox_Object: TComboBox;
    Edit_Group: TEdit;
    Edit_Owner: TEdit;
    Edit_Principal: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label_Group: TLabel;
    Label_Owner: TLabel;
    Label_Type: TLabel;
    Label_Principal: TLabel;
    Label_Object: TLabel;
    Label_ObjectInheritance: TLabel;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel14: TPanel;
    Panel15: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    ScrollBox1: TScrollBox;
    Splitter1: TSplitter;
    TisGrid1: TTisGrid;
    procedure Action_AddACEExecute(Sender: TObject);
    procedure Action_AddACEUpdate(Sender: TObject);
    procedure Action_ApplyExecute(Sender: TObject);
    procedure Action_ApplyUpdate(Sender: TObject);
    procedure Action_CancelExecute(Sender: TObject);
    procedure Action_CancelUpdate(Sender: TObject);
    procedure Action_DeleteACEExecute(Sender: TObject);
    procedure Action_DeleteACEUpdate(Sender: TObject);
    procedure Action_DuplicateACEExecute(Sender: TObject);
    procedure Action_DuplicateACEUpdate(Sender: TObject);
    procedure Action_OKExecute(Sender: TObject);
    procedure Action_OKUpdate(Sender: TObject);
    procedure Action_SelectGroupUpdate(Sender: TObject);
    procedure Action_SelectInheritedObjectExecute(Sender: TObject);
    procedure Action_SelectInheritedObjectUpdate(Sender: TObject);
    procedure Action_SelectObjectExecute(Sender: TObject);
    procedure Action_SelectObjectUpdate(Sender: TObject);
    procedure Action_SelectOwnerExecute(Sender: TObject);
    procedure Action_SelectOwnerUpdate(Sender: TObject);
    procedure Action_SelectPrincipalExecute(Sender: TObject);
    procedure Action_SelectPrincipalUpdate(Sender: TObject);
    procedure CheckBoxCAChange(Sender: TObject);
    procedure CheckBoxCCChange(Sender: TObject);
    procedure CheckBoxCIChange(Sender: TObject);
    procedure CheckBoxDCChange(Sender: TObject);
    procedure CheckBoxDChange(Sender: TObject);
    procedure CheckBoxDTChange(Sender: TObject);
    procedure CheckBoxIOChange(Sender: TObject);
    procedure CheckBoxLCChange(Sender: TObject);
    procedure CheckBoxLOChange(Sender: TObject);
    procedure CheckBoxNPChange(Sender: TObject);
    procedure CheckBoxOIChange(Sender: TObject);
    procedure CheckBoxRCChange(Sender: TObject);
    procedure CheckBoxRPChange(Sender: TObject);
    procedure CheckBoxSWChange(Sender: TObject);
    procedure CheckBoxWDACChange(Sender: TObject);
    procedure CheckBoxWOChange(Sender: TObject);
    procedure CheckBoxWPChange(Sender: TObject);
    procedure ComboBox_ObjectChange(Sender: TObject);
    procedure ComboBox_ObjectInheritanceChange(Sender: TObject);
    procedure ComboBox_TypeChange(Sender: TObject);
    procedure Edit_PrincipalChange(Sender: TObject);
    procedure TisGrid1DrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; const CellText: String;
      const CellRect: TRect; var DefaultDraw: Boolean);
    procedure TisGrid1FocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure TisGrid1GetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure TisGrid1GetText(aSender: TBaseVirtualTree; aNode: PVirtualNode;
      const aCell: TDocVariantData; aColumn: TColumnIndex;
      aTextType: TVSTTextType; var aText: string);
  private
    fLdapClient: TRsatLdapClient;
    fObjectDN: RawUtf8;
    fObjectName: RawUtf8;
    fIsUpdating: Integer;

    fSecurityDescriptor: TSecurityDescriptor;

    fSidCache: TSidCache;
    fGuidCache: TGUIDCache;

    fOwnerChange: Boolean;
    fDACLChange: Boolean;

    function GetSDChangeds: Boolean;

    procedure FlagsToGUI(AFlags: TSecAceFlags);
    procedure MaskToGUI(AMask: TSecAccessMask);

    function GUIToFlags: TSecAceFlags;
    function GUIToMask: TSecAccessMask;
    procedure SetLdapClient(AValue: TRsatLdapClient);
    procedure SetObjectDN(AValue: RawUtf8);
    procedure SetObjectName(AValue: RawUtf8);

    procedure UpdateRightPanel(NodeData: PDocVariantData);
    procedure UpdateRightPanelType(NodeData: PDocVariantData);
    procedure UpdateRightPanelAccount(NodeData: PDocVariantData);
    procedure UpdateRightPanelObject(NodeData: PDocVariantData);
    procedure UpdateRightPanelInheritedObject(NodeData: PDocVariantData);
    procedure UpdateRightPanelRights(NodeData: PDocVariantData);
    procedure UpdateRightPanelFlags(NodeData: PDocVariantData);

    function IsUpdating: Boolean;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure SaveRightPanel;
    procedure SaveRightPanelType(NodeData: PDocVariantData);
    procedure SaveRightPanelAccount(NodeData: PDocVariantData);
    procedure SaveRightPanelObject(NodeData: PDocVariantData);
    procedure SaveRightPanelInheritedObject(NodeData: PDocVariantData);
    procedure SaveRightPanelRights(NodeData: PDocVariantData);
    procedure SaveRightPanelFlags(NodeData: PDocVariantData);

    procedure UpdateGridData;
    procedure UpdateGridNodeData(NodeData: PDocVariantData);

    procedure SDToGUI;
    procedure GUIToSD;

    procedure DACLToGUI;
    procedure GUIToDACL;

    procedure AddACEToGUI(const ACE: TSecAce; Index: Integer);
    procedure AddNodeToACL(var DACL: TSecAcl; const Node: PVirtualNode);

    procedure SetSecurityDescriptor(AValue: TSecurityDescriptor);
    function ResolveObjectNameToSidCache(AObjectName: RawUtf8): RawUtf8;
    function NewACENextIndex: Integer;
  public

    constructor Create(TheOwner: TComponent; AObjectName: RawUtf8 = ''); reintroduce;
    destructor Destroy; override;

    procedure ChangeOwner(ATextSid, AName: RawUtf8);
    procedure ChangePrincipal(ATextSid, AName: RawUtf8);

    property SecurityDescriptor: TSecurityDescriptor read fSecurityDescriptor write SetSecurityDescriptor;
    property ObjectName: RawUtf8 read fObjectName write SetObjectName;
    property ObjectDN: RawUtf8 read fObjectDN write SetObjectDN;
    property LdapClient: TRsatLdapClient read fLdapClient write SetLdapClient;
    property SDChanged: Boolean read GetSDChangeds;
  end;

function AccessMaskToString(AMask: TSecAccessMask): RawUtf8;
function InheritanceFlagsToString(AFlags: TSecAceFlags): RawUtf8;

  /// NodeData:
  ///   state
  ///   type
  ///   ace_type
  ///   account
  ///   account_sid
  ///   permissions
  ///   rights
  ///   object
  ///   object_guid
  ///   flags
  ///   ace_flags
  ///   inheritedObject
  ///   inherited_object_guid

implementation
uses
  ufrmrsat,
  uvisselectobjectsid;

{$R *.lfm}

function AccessMaskToString(AMask: TSecAccessMask): RawUtf8;
var
  m: TSecAccess;
begin
  result := '';

  // Full control
  if AMask >= [samCreateChild, samDeleteChild, samListChildren, samSelfWrite, samReadProp, samWriteProp,
    samDeleteTree, samListObject,samControlAccess, samDelete, samReadControl, samWriteDac, samWriteOwner] then
  begin
    result := rsSecAccessFullControl;
    AMask -= [samCreateChild, samDeleteChild, samListChildren, samSelfWrite, samReadProp, samWriteProp,
      samDeleteTree, samListObject, samControlAccess, samDelete, samReadControl, samWriteDac, samWriteOwner];
  end;

  // List content
  if AMask >= [samListChildren, samListObject] then
  begin
    result := rsSecAccessListContent;
    AMask -= [samListChildren, samListObject];
  end;

  for m in AMask do
    if result = '' then
      result := SEC_ACCESS_NAMES[m]
    else
      result := FormatUtf8('%, %', [result, SEC_ACCESS_NAMES[m]]);
end;

function InheritanceFlagsToString(AFlags: TSecAceFlags): RawUtf8;
var
  f: TSecAceFlag;
begin
  result := '';

  for f in AFlags do
    if result = '' then
      result := SEC_FLAGS_NAMES[f]
    else
      result := FormatUtf8('%, %', [result, SEC_FLAGS_NAMES[f]]);
end;

{ TVisAdvancedSecurityMissingNodeDataException }

constructor TVisAdvancedSecurityMissingNodeDataException.Create;
begin
  Message := rsMissingNodeData;
end;

{ TVisAdvancedSecurityMissingLdapClientException }

constructor TVisAdvancedSecurityMissingLdapClientException.Create;
begin
  Message := rsMissingLdapClient;
end;

{ TSidCache }

function TSidCache.BuildFilter: RawUtf8;
var
  i: Integer;
begin
  result := '';

  for i := 0 to High(fSIDS) do
    if (SidToKnown(fSIDS[i]) = wksNull) then
      result := FormatUtf8('%(objectSid=%)', [result, LdapEscape(fSIDS[i])]);
  if result <> '' then
    result := FormatUtf8('(|%)', [result]);
end;

function TSidCache.GetCount: Integer;
begin
  result := Length(fSIDS);
end;

procedure TSidCache.ResolveKnownSIDS;
var
  i: Integer;
begin
  for i := 0 to High(fSIDS) do
    if SidToKnown(fSIDS[i]) <> wksNull then
      fNames[i] := WELL_KNOWN_SID_NAMES[SidToKnown(fSIDS[i])];
end;

function TSidCache.IndexOfSID(ASID: RawUtf8): Integer;
var
  i: Integer;
  lSID: RawUtf8;
begin
  result := -1;
  if ASid = '' then
    Exit;
  lSID := UpperCase(ASID);
  for i := 0 to High(fSIDS) do
    if fSIDS[i] = lSID then
    begin
      result := i;
      Exit;
    end;
end;

function TSidCache.IndexOfName(AName: RawUtf8): Integer;
var
  i: Integer;
begin
  result := -1;
  if AName = '' then
    Exit;

  for i := 0 to High(fNames) do
    if fNames[i] = AName then
    begin
      result := i;
      Exit;
    end;
end;

procedure TSidCache.ResolveSIDS(ALdapClient: TLdapClient);
var
  Filter: RawUtf8;
begin
  // Fill Well known sid
  ResolveKnownSIDS;

  // Ignore Well known sid for ldap query

  if not Assigned(ALdapClient) then
    Exit;

  Filter := BuildFilter;
  if Filter = '' then
    Exit;

  ALdapClient.SearchBegin();
  try
    ALdapClient.SearchScope := lssWholeSubtree;
    repeat
      if not ALdapClient.Search(ALdapClient.DefaultDN, False, Filter, ['name', 'objectSid']) then
        Exit;
      UpdateSIDS(ALdapClient.SearchResult);
    until ALdapClient.SearchCookie = '';
  finally
    ALdapClient.SearchEnd;
  end;
end;

procedure TSidCache.UpdateSIDS(ALdapResult: TLdapResultList);
var
  SearchResult: TLdapResult;
  SID, Name: RawUtf8;
begin
  for SearchResult in ALdapResult.Items do
  begin
    if not Assigned(SearchResult) then
      continue;
    SID := SearchResult.Find('objectSid').GetReadable();
    Name := SearchResult.Find('name').GetReadable();
    SetSIDName(SID, Name);
  end;
end;

procedure TSidCache.AddSID(ASID: RawUtf8);
var
  c: SizeInt;
begin
  if (ASID = '') then
    Exit;
  c := IndexOfSID(ASID);
  if c >= 0 then
    Exit;

  c := Length(fSIDS);

  SetLength(fSIDS, c + 1);
  SetLength(fNames, c + 1);

  fSIDS[c] := UpperCase(ASID);
  fNames[c] := '';
end;

procedure TSidCache.SetSIDName(ASID, AName: RawUtf8);
var
  c: Integer;
begin
  c := IndexOfSID(ASID);
  if c < 0 then
    Exit;

  fNames[c] := AName;
end;

function TSidCache.SIDToName(ASID: RawUtf8): RawUtf8;
var
  c: Integer;
begin
  result := '';

  try
    c := IndexOfSID(ASID);
    if c < 0 then
      Exit;

    result := fNames[c];
  finally
    if result = '' then
      result := ASID;
  end;
end;

function TSidCache.NameToSID(AName: RawUtf8): RawUtf8;
var
  c: Integer;
begin
  result := '';
  if AName = '' then
    Exit;

  c := IndexOfName(AName);
  if c < 0 then
  begin
    c := IndexOfSID(AName);
    if c < 0 then
      Exit;
  end;

  result := fSIDS[c];
end;

procedure TSidCache.Clear;
begin
  fSIDS := nil;
  fNames := nil;
end;

{ TGUIDCache }

procedure TGUIDCache.UpdateFromClassSchema(ALdapResult: TLdapResultList);
var
  SearchResult: TLdapResult;
  Name, GUID: RawUtf8;
begin
  for SearchResult in ALdapResult.Items do
  begin
    if not Assigned(SearchResult) then
      continue;

    Name := SearchResult.Find('cn').GetReadable();
    GUID := ToUtf8(PGuid(SearchResult.Find('schemaIDGUID').GetRaw())^);
    SetGUIDName(GUID, Name, 'class');
  end;
end;

function TGUIDCache.GetCount: Integer;
begin
  result := Length(fGUIDS);
end;

procedure TGUIDCache.UpdateFromAttributeSchema(ALdapResult: TLdapResultList);
var
  SearchResult: TLdapResult;
  Name, GUID: RawUtf8;
begin
  for SearchResult in ALdapResult.Items do
  begin
    if not Assigned(SearchResult) then
      continue;

    Name := SearchResult.Find('cn').GetReadable();
    GUID := ToUtf8(PGuid(SearchResult.Find('schemaIDGUID').GetRaw())^);
    SetGUIDName(GUID, Name, 'attribute');
  end;
end;

procedure TGUIDCache.UpdateFromExtendedRights(ALdapResult: TLdapResultList);
var
  SearchResult: TLdapResult;
  Name, GUID, T: RawUtf8;
  ValidAccesses: Longint;
begin
  for SearchResult in ALdapResult.Items do
  begin
    if not Assigned(SearchResult) then
      continue;

    Name := SearchResult.Find('cn').GetReadable();
    GUID := SearchResult.Find('rightsGuid').GetReadable();
    ValidAccesses := 0;
    TryStrToInt(SearchResult.Find('validAccesses').GetReadable(), ValidAccesses);

    T := 'controlAccessRight';
    if ((ValidAccesses and 256) <> 0) then
      T := 'extendedRight'
    else if ((ValidAccesses and 8) <> 0) then
      T := 'validateRight'
    else if ((ValidAccesses and 16) <> 0) then
      T := 'propertySet';

    SetGUIDName(GUID, Name, T);
  end;
end;

function TGUIDCache.IndexOfGUID(AGUID, AType: RawUtf8): Integer;
var
  uGUID: RawUtf8;
  i: Integer;
begin
  result := -1;

  uGUID := UpperCase(AGUID);
  for i := 0 to High(fGUIDS) do
    if (fGUIDS[i] = uGUID) and ((AType = '') or (fTypes[i] = AType)) then
    begin
      result := i;
      Exit;
    end;
end;

function TGUIDCache.IndexOfName(AName: RawUtf8): Integer;
var
  i: Integer;
begin
  result := -1;

  for i := 0 to High(fNames) do
    if (fNames[i] = AName) then
    begin
      result := i;
      Exit;
    end;
end;

procedure TGUIDCache.ResolveGUIDS(ALdapClient: TLdapClient);
var
  ExtendedRightsDN: RawUtf8;
begin
  if not Assigned(ALdapClient) then
    Exit;

  ALdapClient.SearchBegin();
  try
    ALdapClient.SearchScope := lssSingleLevel;
    repeat
      if not ALdapClient.Search(ALdapClient.SchemaDN, False, '(objectClass=classSchema)', ['cn', 'lDAPDisplayName', 'governsID', 'schemaIDGUID']) then
        Exit;
      UpdateFromClassSchema(ALdapClient.SearchResult);
    until ALdapClient.SearchCookie = '';
  finally
    ALdapClient.SearchEnd;
  end;

  ALdapClient.SearchBegin();
  try
    ALdapClient.SearchScope := lssSingleLevel;
    repeat
      if not ALdapClient.Search(ALdapClient.SchemaDN, False, '(objectClass=attributeSchema)', ['cn', 'lDAPDisplayName', 'attributeID', 'schemaIDGUID']) then
        Exit;
      UpdateFromAttributeSchema(ALdapClient.SearchResult);
    until ALdapClient.SearchCookie = '';
  finally
    ALdapClient.SearchEnd;
  end;

  ExtendedRightsDN := FormatUtf8('CN=Extended-Rights,%', [ALdapClient.ConfigDN]);
  ALdapClient.SearchBegin();
  try
    ALdapClient.SearchScope := lssSingleLevel;
    repeat
      if not ALdapClient.Search(ExtendedRightsDN, False, '(objectClass=controlAccessRight)', ['cn', 'displayName', 'rightsGuid', 'validAccesses', 'appliesTo']) then
        Exit;
      UpdateFromExtendedRights(ALdapClient.SearchResult);
    until ALdapClient.SearchCookie = '';
  finally
    ALdapClient.SearchEnd;
  end;
end;

function TGUIDCache.AddGUID(AGUID, AType: RawUtf8): Integer;
begin
  if AGUID = '' then
    Exit;

  result := IndexOfGUID(AGUID, AType);
  if result >= 0 then
    Exit;

  result := Length(fGUIDS);
  SetLength(fGUIDS, result + 1);
  SetLength(fNames, result + 1);
  SetLength(fTypes, result + 1);

  fGUIDS[result] := UpperCase(AGUID);
  fNames[result] := '';
  fTypes[result] := AType;
end;

procedure TGUIDCache.SetGUIDName(AGUID, AName, AType: RawUtf8);
var
  c: Integer;
begin
  c := AddGUID(AGUID, AType);

  if c < 0 then
    Exit;

  fNames[c] := AName;
end;

function TGUIDCache.GUIDToName(AGUID, AType: RawUtf8): RawUtf8;
var
  c: Integer;
begin
  result := '';

  if (AGUID = '') then
    Exit;

  if (AGUID = ToUtf8(GUID_NULL)) then
    Exit;

  c := IndexOfGUID(AGUID, AType);
  if c < 0 then
  begin
    result := AGUID;
    Exit;
  end;
  result := fNames[c];
  if result = '' then
    result := AGUID;
end;

function TGUIDCache.NameToGUID(AName: RawUtf8): RawUtf8;
var
  c: Integer;
begin
  result := '';

  if AName = '' then
    Exit;

  c := IndexOfName(AName);
  if c < 0 then
    Exit;
  result := fGUIDS[c];
end;

procedure TGUIDCache.Clear;
begin
  fGUIDS := nil;
  fNames := nil;
  fTypes := nil;
end;

function TGUIDCache.Dump: RawUtf8;
var
  i: Integer;
begin
  result := '';

  for i := 0 to Pred(Count) do
  begin
    if i = 0 then
      result := FormatUtf8('{"GUID": "%", "Name": "%", "Type": "%"}', [fGUIDS[i], fNames[i], fTypes[i]])
    else
      result := FormatUtf8('%,{"GUID": "%", "Name": "%", "Type": "%"}', [result, fGUIDS[i], fNames[i], fTypes[i]]);
  end;
  result := FormatUtf8('[%]', [result]);
end;

{ TVisAdvancedSecurity }

procedure TVisAdvancedSecurity.TisGrid1FocusChanged(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
var
  NodeData: PDocVariantData;
begin
  NodeData := TisGrid1.GetNodeAsPDocVariantData(Node);
  if not Assigned(NodeData) then
    Exit;

  BeginUpdate;
  try
    UpdateRightPanel(NodeData);
  finally
    EndUpdate;
  end;
end;

procedure TVisAdvancedSecurity.Action_AddACEExecute(Sender: TObject);
var
  NewACE: TDocVariantData;
begin
  NewACE.Init(JSON_FAST);
  NewACE.I['index'] := NewACENextIndex;
  NewACE.I['state'] := 1; // 0: Nothing; 1: New; 2: Update; 3: Deleted
  TisGrid1.Data.AddItem(NewACE);
  TisGrid1.LoadData();
  TisGrid1.ClearSelection;
  TisGrid1.FocusedNode := TisGrid1.GetLast();
  fDACLChange := True;
end;

procedure TVisAdvancedSecurity.Action_AddACEUpdate(Sender: TObject);
begin
  Action_AddACE.Enabled := True;
end;

procedure TVisAdvancedSecurity.Action_ApplyExecute(Sender: TObject);
begin
  if not Assigned(LdapClient) then
    raise TVisAdvancedSecurityMissingLdapClientException.Create;

  if ObjectDN = '' then
    raise TVisAdvancedSecurityException.Create('Missing ObjectDN to modify SD.');

  // Ensure SD is updated from GUI.
  GUIToSD;

  // Modify the SD
  if not LdapClient.Modify(ObjectDN, lmoReplace, 'nTSecurityDescriptor', SecurityDescriptor.ToBinary) then
    raise TVisAdvancedSecurityException.Create('Failed to modify SD.');

  // Reload SD from the domain
  fSecurityDescriptor.FromBinary(fLdapClient.SearchObject(ObjectDN, '', 'nTSecurityDescriptor').GetRaw());

  // Update GUI from reloaded SD
  SDToGUI;

  fDACLChange := False;
  fOwnerChange := False;
end;

procedure TVisAdvancedSecurity.Action_ApplyUpdate(Sender: TObject);
begin
  Action_Apply.Enabled := SDChanged;
end;

procedure TVisAdvancedSecurity.Action_CancelExecute(Sender: TObject);
begin
  Close;
end;

procedure TVisAdvancedSecurity.Action_CancelUpdate(Sender: TObject);
begin
  Action_Cancel.Enabled := True;
end;

procedure TVisAdvancedSecurity.Action_DeleteACEExecute(Sender: TObject);
var
  NodeData: PDocVariantData;
begin
  NodeData := TisGrid1.GetNodeAsPDocVariantData(TisGrid1.FocusedNode);
  if not Assigned(NodeData) then
    Exit;

  NodeData^.AddOrUpdateValue('state', 3);
  TisGrid1.InvalidateNode(TisGrid1.FocusedNode);
  fDACLChange := True;
end;

procedure TVisAdvancedSecurity.Action_DeleteACEUpdate(Sender: TObject);
begin
  Action_DeleteACE.Enabled := Assigned(TisGrid1.FocusedNode);
end;

procedure TVisAdvancedSecurity.Action_DuplicateACEExecute(Sender: TObject);
var
  NewACE: TDocVariantData;
  NodeData: PDocVariantData;
  n: RawUtf8;
begin
  NodeData := TisGrid1.GetNodeAsPDocVariantData(TisGrid1.FocusedNode);
  if not Assigned(NodeData) then
    Exit;

  NewACE.Init(JSON_FAST);
  for n in NodeData^.Names do
  begin
    if n = '' then
      continue;
    NewACE.AddOrUpdateValue(n, NodeData^.Value[n]);
  end;
  NewACE.I['index'] := NewACENextIndex;
  NewACE.I['state'] := 1;
  TisGrid1.Data.AddItem(NewACE);
  TisGrid1.LoadData();
  TisGrid1.ClearSelection;
  TisGrid1.FocusedNode := TisGrid1.GetLast();
  fDACLChange := True;
end;

procedure TVisAdvancedSecurity.Action_DuplicateACEUpdate(Sender: TObject);
begin
  Action_DuplicateACE.Enabled := Assigned(TisGrid1.FocusedNode);
end;

procedure TVisAdvancedSecurity.Action_OKExecute(Sender: TObject);
begin
  Action_Apply.Execute;
  Close;
end;

procedure TVisAdvancedSecurity.Action_OKUpdate(Sender: TObject);
begin
  Action_OK.Enabled := True;
end;

procedure TVisAdvancedSecurity.Action_SelectGroupUpdate(Sender: TObject);
begin
  Action_SelectGroup.Enabled := False;
end;

procedure TVisAdvancedSecurity.Action_SelectInheritedObjectExecute(
  Sender: TObject);
begin
  ShowMessage('Not yet implemented.');
end;

procedure TVisAdvancedSecurity.Action_SelectInheritedObjectUpdate(
  Sender: TObject);
begin
  Action_SelectInheritedObject.Enabled := True;
end;

procedure TVisAdvancedSecurity.Action_SelectObjectExecute(Sender: TObject);
begin
  ShowMessage('Not yet implemented.');
end;

procedure TVisAdvancedSecurity.Action_SelectObjectUpdate(Sender: TObject);
begin
  Action_SelectObject.Enabled := True;
end;

procedure TVisAdvancedSecurity.Action_SelectOwnerExecute(Sender: TObject);
var
  Vis: TVisSelectObjectSID;
begin
  Vis := TVisSelectObjectSID.Create(Self);
  try
    Vis.LdapClient := fLdapClient;
    Vis.MultiSelect := False;
    if Vis.ShowModal <> mrOK then
      Exit;

    ChangeOwner(Vis.SelectedSid, Vis.SelectedName);
  finally
    FreeAndNil(Vis);
  end;
end;

procedure TVisAdvancedSecurity.Action_SelectOwnerUpdate(Sender: TObject);
begin
  Action_SelectOwner.Enabled := True;
end;

procedure TVisAdvancedSecurity.Action_SelectPrincipalExecute(Sender: TObject
  );
var
  Vis: TVisSelectObjectSID;
begin
  Vis := TVisSelectObjectSID.Create(Self);
  try
    Vis.LdapClient := fLdapClient;
    Vis.MultiSelect := False;
    if Vis.ShowModal <> mrOK then
      Exit;

    ChangePrincipal(Vis.SelectedSid, Vis.SelectedName);
  finally
    FreeAndNil(Vis);
  end;
end;

procedure TVisAdvancedSecurity.Action_SelectPrincipalUpdate(Sender: TObject);
begin
  Action_SelectPrincipal.Enabled := True;
end;

procedure TVisAdvancedSecurity.CheckBoxCAChange(Sender: TObject);
begin
  if not IsUpdating then
    SaveRightPanel;
end;

procedure TVisAdvancedSecurity.CheckBoxCCChange(Sender: TObject);
begin
  if not IsUpdating then
    SaveRightPanel;
end;

procedure TVisAdvancedSecurity.CheckBoxCIChange(Sender: TObject);
begin
  if not IsUpdating then
    SaveRightPanel;
end;

procedure TVisAdvancedSecurity.CheckBoxDCChange(Sender: TObject);
begin
  if not IsUpdating then
    SaveRightPanel;
end;

procedure TVisAdvancedSecurity.CheckBoxDChange(Sender: TObject);
begin
  if not IsUpdating then
    SaveRightPanel;
end;

procedure TVisAdvancedSecurity.CheckBoxDTChange(Sender: TObject);
begin
  if not IsUpdating then
    SaveRightPanel;
end;

procedure TVisAdvancedSecurity.CheckBoxIOChange(Sender: TObject);
begin
  if not IsUpdating then
    SaveRightPanel;
end;

procedure TVisAdvancedSecurity.CheckBoxLCChange(Sender: TObject);
begin
  if not IsUpdating then
    SaveRightPanel;
end;

procedure TVisAdvancedSecurity.CheckBoxLOChange(Sender: TObject);
begin
  if not IsUpdating then
    SaveRightPanel;
end;

procedure TVisAdvancedSecurity.CheckBoxNPChange(Sender: TObject);
begin
  if not IsUpdating then
    SaveRightPanel;
end;

procedure TVisAdvancedSecurity.CheckBoxOIChange(Sender: TObject);
begin
  if not IsUpdating then
    SaveRightPanel;
end;

procedure TVisAdvancedSecurity.CheckBoxRCChange(Sender: TObject);
begin
  if not IsUpdating then
    SaveRightPanel;
end;

procedure TVisAdvancedSecurity.CheckBoxRPChange(Sender: TObject);
begin
  if not IsUpdating then
    SaveRightPanel;
end;

procedure TVisAdvancedSecurity.CheckBoxSWChange(Sender: TObject);
begin
  if not IsUpdating then
    SaveRightPanel;
end;

procedure TVisAdvancedSecurity.CheckBoxWDACChange(Sender: TObject);
begin
  if not IsUpdating then
    SaveRightPanel;
end;

procedure TVisAdvancedSecurity.CheckBoxWOChange(Sender: TObject);
begin
  if not IsUpdating then
    SaveRightPanel;
end;

procedure TVisAdvancedSecurity.CheckBoxWPChange(Sender: TObject);
begin
  if not IsUpdating then
    SaveRightPanel;
end;

procedure TVisAdvancedSecurity.ComboBox_ObjectChange(Sender: TObject);
begin
  if not IsUpdating then
    SaveRightPanel;
end;

procedure TVisAdvancedSecurity.ComboBox_ObjectInheritanceChange(Sender: TObject
  );
begin
  if not IsUpdating then
    SaveRightPanel;
end;

procedure TVisAdvancedSecurity.ComboBox_TypeChange(Sender: TObject);
begin
  if not IsUpdating then
    SaveRightPanel;
end;

procedure TVisAdvancedSecurity.Edit_PrincipalChange(Sender: TObject);
begin
  if not IsUpdating then
    SaveRightPanel;
end;

procedure TVisAdvancedSecurity.TisGrid1DrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const CellText: String; const CellRect: TRect; var DefaultDraw: Boolean);
var
  NodeData: PDocVariantData;
begin
  NodeData := TisGrid1.GetNodeAsPDocVariantData(Node);
  if not Assigned(NodeData) or not NodeData^.Exists('state') then
    Exit;

  case NodeData^.I['state'] of
    1: TargetCanvas.Font.Bold := True;
    2: TargetCanvas.Font.Italic := True;
    3: TargetCanvas.Font.StrikeThrough := True;
  end;
end;

procedure TVisAdvancedSecurity.TisGrid1GetImageIndex(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
var
  Col: TTisGridColumn;
  NodeData: PDocVariantData;
begin
  Col := TisGrid1.FindColumnByIndex(Column);
  if not Assigned(Col) then
    Exit;

  NodeData := TisGrid1.GetNodeAsPDocVariantData(Node);
  if not Assigned(NodeData) or not NodeData^.Exists(Col.PropertyName) then
    Exit;

  case Col.PropertyName of
    'type':
    begin
      if NodeData^.B[Col.PropertyName] then
        ImageIndex := 28
      else
        ImageIndex := 49;
    end;
  end;
end;

procedure TVisAdvancedSecurity.TisGrid1GetText(aSender: TBaseVirtualTree;
  aNode: PVirtualNode; const aCell: TDocVariantData; aColumn: TColumnIndex;
  aTextType: TVSTTextType; var aText: string);
var
  Col: TTisGridColumn;
  NodeData: PDocVariantData;
begin
  Col := TisGrid1.FindColumnByIndex(aColumn);
  if not Assigned(Col) then
    Exit;

  NodeData := TisGrid1.GetNodeAsPDocVariantData(aNode);
  if not Assigned(NodeData) or not NodeData^.Exists(Col.PropertyName) then
    Exit;

  case Col.PropertyName of
  'type': aText := '';
  end;
end;

function TVisAdvancedSecurity.GetSDChangeds: Boolean;
begin
  result := fOwnerChange or fDACLChange;
end;

procedure TVisAdvancedSecurity.FlagsToGUI(AFlags: TSecAceFlags);
begin
  CheckBoxOI.Checked := safObjectInherit in AFlags;
  CheckBoxCI.Checked := safContainerInherit in AFlags;
  CheckBoxIO.Checked := safInheritOnly in AFlags;
  CheckBoxNP.Checked := safNoPropagateInherit in AFlags;
end;

procedure TVisAdvancedSecurity.MaskToGUI(AMask: TSecAccessMask);
begin
  CheckBoxCC.Checked := samCreateChild in AMask;
  CheckBoxDC.Checked := samDeleteChild in AMask;
  CheckBoxLC.Checked := samListChildren in AMask;
  CheckBoxSW.Checked := samSelfWrite in AMask;
  CheckBoxRP.Checked := samReadProp in AMask;
  CheckBoxWP.Checked := samWriteProp in AMask;
  CheckBoxDT.Checked := samDeleteTree in AMask;
  CheckBoxLO.Checked := samListObject in AMask;
  CheckBoxCA.Checked := samControlAccess in AMask;
  CheckBoxD.Checked := samDelete in AMask;
  CheckBoxRC.Checked := samReadControl in AMask;
  CheckBoxWDAC.Checked := samWriteDac in AMask;
  CheckBoxWO.Checked := samWriteOwner in AMask;
end;

function TVisAdvancedSecurity.GUIToFlags: TSecAceFlags;
begin
  result := [];

  if CheckBoxOI.Checked then
    Include(Result, safObjectInherit);
  if CheckBoxCI.Checked then
    Include(Result, safContainerInherit);
  if CheckBoxIO.Checked then
    Include(Result, safInheritOnly);
  if CheckBoxNP.Checked then
    Include(Result, safNoPropagateInherit);
end;

function TVisAdvancedSecurity.GUIToMask: TSecAccessMask;
begin
  result := [];

  if CheckBoxCC.Checked then
    Include(result, samCreateChild);
  if CheckBoxDC.Checked then
    Include(result, samDeleteChild);
  if CheckBoxLC.Checked then
    Include(result, samListChildren);
  if CheckBoxSW.Checked then
    Include(result, samSelfWrite);
  if CheckBoxRP.Checked then
    Include(result, samReadProp);
  if CheckBoxWP.Checked then
    Include(result, samWriteProp);
  if CheckBoxDT.Checked then
    Include(result, samDeleteTree);
  if CheckBoxLO.Checked then
    Include(result, samListObject);
  if CheckBoxCA.Checked then
    Include(result, samControlAccess);
  if CheckBoxD.Checked then
    Include(result, samDelete);
  if CheckBoxRC.Checked then
    Include(result, samReadControl);
  if CheckBoxWDAC.Checked then
    Include(result, samWriteDac);
  if CheckBoxWO.Checked then
    Include(result, samWriteOwner);
end;

procedure TVisAdvancedSecurity.SetLdapClient(AValue: TRsatLdapClient);
begin
  if fLdapClient = AValue then
    Exit;
  fLdapClient := AValue;

  fGuidCache.ResolveGUIDS(fLdapClient);
end;

procedure TVisAdvancedSecurity.SetObjectDN(AValue: RawUtf8);
begin
  if fObjectDN = AValue then
    Exit;
  fObjectDN := AValue;
end;

procedure TVisAdvancedSecurity.SetObjectName(AValue: RawUtf8);
begin
  fObjectName := AValue;

  Caption := FormatUtf8(rsVisAdvancedSecurityTitle, [fObjectName]);
end;

procedure TVisAdvancedSecurity.UpdateRightPanel(NodeData: PDocVariantData);
begin
  if not Assigned(NodeData) then
    Exit;

  UpdateRightPanelType(NodeData);
  UpdateRightPanelAccount(NodeData);
  UpdateRightPanelObject(NodeData);
  UpdateRightPanelInheritedObject(NodeData);
  UpdateRightPanelRights(NodeData);
  UpdateRightPanelFlags(NodeData);
end;

procedure TVisAdvancedSecurity.UpdateRightPanelType(NodeData: PDocVariantData
  );
begin
  if Assigned(NodeData) and NodeData^.Exists('type') then
  begin
    if NodeData^.B['type'] then
      ComboBox_Type.Text := rsAllow
    else
      ComboBox_Type.Text := rsDeny;
  end
  else
    ComboBox_Type.Text := '';
end;

procedure TVisAdvancedSecurity.UpdateRightPanelAccount(
  NodeData: PDocVariantData);
begin
  if Assigned(NodeData) and NodeData^.Exists('account_sid') then
    Edit_Principal.Text := fSidCache.SIDToName(NodeData^.S['account_sid'])
  else
    Edit_Principal.Text := '';
end;

procedure TVisAdvancedSecurity.UpdateRightPanelObject(
  NodeData: PDocVariantData);
var
  m: TSecAccessMask;
begin
  if Assigned(NodeData) and NodeData^.Exists('object_guid') then
  begin
    m := TSecAccessMask(Integer(NodeData^.I['rights']));
    if (samCreateChild in m) or (samDeleteChild in m) then
      ComboBox_Object.Text := fGuidCache.GUIDToName(NodeData^.S['object_guid'], 'class')
    else if (samReadProp in m) or (samWriteProp in m) then
    begin
      ComboBox_Object.Text := fGuidCache.GUIDToName(NodeData^.S['object_guid'], 'attribute');
      if ComboBox_Object.Text = '' then
        ComboBox_Object.Text := fGuidCache.GUIDToName(NodeData^.S['object_guid'], 'propertySet');
    end
    else if samSelfWrite in m then
      ComboBox_Object.Text := fGuidCache.GUIDToName(NodeData^.S['object_guid'], 'validateRight')
    else if samControlAccess in m then
    begin
      ComboBox_Object.Text := fGuidCache.GUIDToName(NodeData^.S['object_guid'], 'extendedRight');
      if ComboBox_Object.Text = '' then
        ComboBox_Object.Text := fGuidCache.GUIDToName(NodeData^.S['object_guid'], 'controlAccessRight');
    end;
  end
  else
    ComboBox_Object.Text := '';
end;

procedure TVisAdvancedSecurity.UpdateRightPanelInheritedObject(
  NodeData: PDocVariantData);
begin
  if Assigned(NodeData) and NodeData^.Exists('inherited_object_guid') then
    ComboBox_ObjectInheritance.Text := fGuidCache.GUIDToName(NodeData^.S['inherited_object_guid'], 'class')
  else
    ComboBox_ObjectInheritance.Text := '';
end;

procedure TVisAdvancedSecurity.UpdateRightPanelRights(
  NodeData: PDocVariantData);
begin
  if Assigned(NodeData) and NodeData^.Exists('rights') then
    MaskToGUI(TSecAccessMask(Integer(NodeData^.I['rights'])))
  else
    MaskToGUI([]);
end;

procedure TVisAdvancedSecurity.UpdateRightPanelFlags(
  NodeData: PDocVariantData);
begin
  if Assigned(NodeData) and NodeData^.Exists('ace_flags') then
    FlagsToGUI(TSecAceFlags(Int8(NodeData^.I['ace_flags'])))
  else
    FlagsToGUI([]);
end;

function TVisAdvancedSecurity.IsUpdating: Boolean;
begin
  result := (fIsUpdating > 0);
end;

procedure TVisAdvancedSecurity.BeginUpdate;
begin
  Inc(fIsUpdating);
end;

procedure TVisAdvancedSecurity.EndUpdate;
begin
  Dec(fIsUpdating);
end;

procedure TVisAdvancedSecurity.SaveRightPanel;
var
  NodeData: PDocVariantData;
begin
  NodeData := TisGrid1.GetNodeAsPDocVariantData(TisGrid1.FocusedNode);
  if not Assigned(NodeData) then
    Exit;

  if not NodeData^.Exists('state') or ((NodeData^.I['state'] <> 1) and (NodeData^.I['state'] <> 3)) then
    NodeData^.AddOrUpdateValue('state', 2);

  SaveRightPanelType(NodeData);
  SaveRightPanelAccount(NodeData);
  SaveRightPanelObject(NodeData);
  SaveRightPanelInheritedObject(NodeData);
  SaveRightPanelRights(NodeData);
  SaveRightPanelFlags(NodeData);

  UpdateGridNodeData(NodeData);

  TisGrid1.InvalidateNode(TisGrid1.FocusedNode);
  fDACLChange := True;
end;

procedure TVisAdvancedSecurity.SaveRightPanelType(NodeData: PDocVariantData);
begin
  if ComboBox_Type.Text = rsAllow then
  begin
    if ComboBox_Object.Text = '' then
      NodeData^.AddOrUpdateValue('ace_type', Integer(satAccessAllowed))
    else
      NodeData^.AddOrUpdateValue('ace_type', Integer(satObjectAccessAllowed));
  end
  else if ComboBox_Type.Text = rsDeny then
  begin
    if ComboBox_Object.Text = '' then
      NodeData^.AddOrUpdateValue('ace_type', Integer(satAccessDenied))
    else
      NodeData^.AddOrUpdateValue('ace_type', Integer(satObjectAccessDenied));
  end
  else
    NodeData^.Delete('ace_type');
end;

procedure TVisAdvancedSecurity.SaveRightPanelAccount(
  NodeData: PDocVariantData);
begin
  if (fSidCache.IndexOfSID(Edit_Principal.Text) >= 0) then
    NodeData^.AddOrUpdateValue('account_sid', Edit_Principal.Text)
  else
    NodeData^.AddOrUpdateValue('account_sid', fSidCache.NameToSID(Edit_Principal.Text));
end;

procedure TVisAdvancedSecurity.SaveRightPanelObject(NodeData: PDocVariantData
  );
begin
  NodeData^.AddOrUpdateValue('object_guid', fGuidCache.NameToGUID(ComboBox_Object.Text));
end;

procedure TVisAdvancedSecurity.SaveRightPanelInheritedObject(
  NodeData: PDocVariantData);
begin
  NodeData^.AddOrUpdateValue('inherited_object_guid', fGuidCache.NameToGUID(ComboBox_ObjectInheritance.Text));
end;

procedure TVisAdvancedSecurity.SaveRightPanelRights(NodeData: PDocVariantData
  );
begin
  NodeData^.AddOrUpdateValue('rights', Integer(GUIToMask));
end;

procedure TVisAdvancedSecurity.SaveRightPanelFlags(NodeData: PDocVariantData
  );
begin
  NodeData^.AddOrUpdateValue('ace_flags', Int8(GUIToFlags));
end;

procedure TVisAdvancedSecurity.UpdateGridData;
var
  Node: PVirtualNode;
  NodeData: PDocVariantData;
begin
  Node := TisGrid1.GetFirst();
  while Assigned(Node) do
  begin
    NodeData := TisGrid1.GetNodeAsPDocVariantData(Node);
    Node := TisGrid1.GetNext(Node);
    if not Assigned(NodeData) then
      continue;
    UpdateGridNodeData(NodeData);
  end;
end;

procedure TVisAdvancedSecurity.UpdateGridNodeData(NodeData: PDocVariantData);
begin
  if not Assigned(NodeData) then
    Exit;
  NodeData^.AddOrUpdateValue('type', (TSecAceType(NodeData^.I['ace_type']) = satObjectAccessAllowed) or (TSecAceType(NodeData^.I['ace_type']) = satAccessAllowed));
  NodeData^.AddOrUpdateValue('account', fSidCache.SIDToName(NodeData^.S['account_sid']));
  NodeData^.AddOrUpdateValue('permissions', AccessMaskToString(TSecAccessMask(Integer(NodeData^.I['rights']))));
  NodeData^.AddOrUpdateValue('object', fGuidCache.GUIDToName(NodeData^.S['object_guid'], '')); // To be determined
  NodeData^.AddOrUpdateValue('flags', InheritanceFlagsToString(TSecAceFlags(Int8(NodeData^.I['ace_flags']))));
  NodeData^.AddOrUpdateValue('inheritedObject', fGuidCache.GUIDToName(NodeData^.S['inherited_object_guid'], 'class'));
end;

procedure TVisAdvancedSecurity.SDToGUI;
begin
  BeginUpdate;
  try
    fSidCache.AddSID(RawSidToText(fSecurityDescriptor.Owner));
    fSidCache.AddSID(RawSidToText(fSecurityDescriptor.Group));

    DACLToGUI;

    fSidCache.ResolveSIDS(LdapClient);

    Edit_Owner.Text := fSidCache.SIDToName(RawSidToText(fSecurityDescriptor.Owner));
    Edit_Group.Text := fSidCache.SIDToName(RawSidToText(fSecurityDescriptor.Group));

    UpdateGridData();
    UpdateRightPanel(TisGrid1.GetNodeAsPDocVariantData(TisGrid1.FocusedNode));
  finally
    EndUpdate;
  end;
end;

procedure TVisAdvancedSecurity.GUIToSD;
begin
  if fOwnerChange then
  begin
    if (Edit_Group.Text = '') then
      raise TVisAdvancedSecurityException.Create('Empty owner for security descriptor.');

    fSecurityDescriptor.Owner := TextToRawSid(fSidCache.NameToSID(Edit_Owner.Text));
    if (fSecurityDescriptor.Owner = '') then
      raise TVisAdvancedSecurityException.Create('Cannot retrieve owner SID for security descriptor.');
  end;

  if fDACLChange then
    GUIToDACL;
end;

procedure TVisAdvancedSecurity.DACLToGUI;
var
  i: Integer;
begin
  TisGrid1.Clear;
  TisGrid1.BeginUpdate;
  try
    for i := 0 to High(fSecurityDescriptor.Dacl) do
      AddAceToGUI(fSecurityDescriptor.Dacl[i], i);
  finally
    TisGrid1.EndUpdate;
    TisGrid1.LoadData();
  end;
end;

procedure TVisAdvancedSecurity.GUIToDACL;
var
  Node: PVirtualNode;
  DACL: TSecAcl;
begin
  DACL := [];

  Node := TisGrid1.GetFirst();
  while Assigned(Node) do
  begin
    AddNodeToACL(DACL, Node);
    Node := TisGrid1.GetNext(Node);
  end;
  LdapClient.OrderAcl(ObjectDN, '', @Dacl);
  fSecurityDescriptor.Dacl := DACL;
end;

procedure TVisAdvancedSecurity.AddACEToGUI(const ACE: TSecAce; Index: Integer);
var
  Data: TDocVariantData;
begin
  // Search for a compatible row to merge ACE in gui
  // Condition: Similar type, similar principal, similar mask, similar flags
  // Result: Merge objectType and inheritedObjectType

  fSidCache.AddSID(RawSidToText(ACE.Sid));

  Data.Init();
  Data.I['index'] := Index;
  Data.I['ace_type'] := Integer(ACE.AceType);
  Data.S['account_sid'] := RawSidToText(ACE.Sid);
  Data.I['rights'] :=  Integer(ACE.Mask);
  Data.S['object_guid'] := ACE.ObjectText();
  Data.I['ace_flags'] := Int8(ACE.Flags);
  Data.S['inherited_object_guid'] := ACE.InheritedText();
  TisGrid1.Data.AddItem(Data);
  Data.Clear;
end;

procedure TVisAdvancedSecurity.AddNodeToACL(var DACL: TSecAcl; const Node: PVirtualNode);
var
  NodeData: PDocVariantData;
  ACE: TSecAce;
begin
  NodeData := TisGrid1.GetNodeAsPDocVariantData(Node);
  if not Assigned(NodeData) then
    Exit;

  if NodeData^.Exists('state') and (NodeData^.I['state'] = 3) then
    Exit;

  if not NodeData^.Exists('ace_type') then
    raise TVisAdvancedSecurityException.Create('Missing ace_type.');
  ACE.AceType := TSecAceType(NodeData^.I['ace_type']);
  if not NodeData^.Exists('account_sid') then
    raise TVisAdvancedSecurityException.Create('Missing account_sid.');
  ACE.Sid := TextToRawSid(NodeData^.S['account_sid']);
  if not NodeData^.Exists('rights') then
    raise TVisAdvancedSecurityException.Create('Missing rights.');
  ACE.Mask := TSecAccessMask(Integer(NodeData^.I['rights']));
  if not NodeData^.Exists('object_guid') then
    raise TVisAdvancedSecurityException.Create('Missing object_guid.');
  ACE.ObjectType := RawUtf8ToGuid(NodeData^.S['object_guid']);
  if not NodeData^.Exists('ace_flags') then
    raise TVisAdvancedSecurityException.Create('Missing ace_flags.');
  ACE.Flags := TSecAceFlags(Int8(NodeData^.I['ace_flags']));
  if not NodeData^.Exists('inherited_object_guid') then
    raise TVisAdvancedSecurityException.Create('Missing inherited_object_guid.');
  ACE.InheritedObjectType := RawUtf8ToGuid(NodeData^.S['inherited_object_guid']);
  Insert(ACE, DACL, Length(DACL));
end;

procedure TVisAdvancedSecurity.SetSecurityDescriptor(AValue: TSecurityDescriptor);
begin
  if fSecurityDescriptor.IsEqual(AValue) then
    Exit;
  fSecurityDescriptor := AValue;
  SDToGUI;
end;

function TVisAdvancedSecurity.ResolveObjectNameToSidCache(
  AObjectName: RawUtf8): RawUtf8;
var
  Res: TLdapResult;
  SID, N: RawUtf8;
begin
  if not Assigned(LdapClient) then
    raise TVisAdvancedSecurityMissingLdapClientException.Create;
  Res := LdapClient.SearchObject(AObjectName, '', ['name', 'objectSid']);
  if not Assigned(Res) then
    Exit;

  SID := Res.Find('objectSid').GetReadable();
  N := Res.Find('name').GetReadable();
  fSidCache.AddSID(SID);
  fSidCache.SetSIDName(SID, N);
  result := SID;
end;

function TVisAdvancedSecurity.NewACENextIndex: Integer;
begin
  result := TisGrid1.Data.Count;
end;

procedure TVisAdvancedSecurity.ChangeOwner(ATextSid, AName: RawUtf8);
begin
  fSidCache.AddSID(ATextSid);
  fSidCache.SetSIDName(ATextSid, AName);

  fOwnerChange := True;
  Edit_Owner.Caption := fSidCache.SIDToName(ATextSid);
end;

procedure TVisAdvancedSecurity.ChangePrincipal(ATextSid, AName: RawUtf8);
begin
  fSidCache.AddSID(ATextSid);
  fSidCache.SetSIDName(ATextSid, AName);

  Edit_Principal.Caption := fSidCache.SIDToName(ATextSid);
end;

constructor TVisAdvancedSecurity.Create(TheOwner: TComponent;
  AObjectName: RawUtf8);
begin
  inherited Create(TheOwner);

  fOwnerChange := False;
  fDACLChange := False;

  fSidCache := TSidCache.Create;
  fGuidCache := TGUIDCache.Create;

  if Assigned(FrmRSAT) then
    LdapClient := FrmRSAT.LdapClient;

  ComboBox_Type.Items.AddStrings([rsAllow, rsDeny]);
  ObjectName := AObjectName;
end;

destructor TVisAdvancedSecurity.Destroy;
begin
  FreeAndNil(fSidCache);
  FreeAndNil(fGuidCache);

  inherited Destroy;
end;

end.

