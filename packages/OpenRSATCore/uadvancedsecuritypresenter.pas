unit uadvancedsecuritypresenter;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.base,
  mormot.core.text,
  mormot.core.variants,
  mormot.core.os.security,
  mormot.net.ldap,
  usidcache,
  uguidcache;

type

  { IAdvancedSecurityView }

  IAdvancedSecurityView = interface
    procedure SetTitle(const aName: RawUtf8);
    procedure SetOwnerText(const aName: RawUtf8);
    procedure SetGroupText(const aName: RawUtf8);
    procedure RefreshACEGrid(aACEs: PDocVariantData);
    procedure RefreshACEGridIndex(aACE: PDocVariantData; aIndex: Integer);
    procedure SelectACE(aIndex: Integer);

    procedure SetRightPanelType(aIsAllow: Boolean);
    procedure SetRightPanelAccount(const aName: RawUtf8);
    procedure SetRightPanelMask(aMask: TSecAccessMask);
    procedure SetRightPanelFlags(aFlags: TSecAceFlags);
    procedure SetRightPanelObject(const aGUIDName: RawUtf8);
    procedure SetRightPanelInheritedObject(const aGUIDName: RawUtf8);

    procedure SetApplyEnabled(aEnabled: Boolean);
    procedure SetDeleteEnabled(aEnabled: Boolean);
    procedure SetDuplicateEnabled(aEnabled: Boolean);

    function  GetCurrentMask: TSecAccessMask;
    function  GetCurrentFlags: TSecAceFlags;
    function  GetCurrentType: Boolean;
    function  GetCurrentPrincipalText: RawUtf8;
    function  GetCurrentObjectText: RawUtf8;
    function  GetCurrentInheritedObjectText: RawUtf8;

    function  PickPrincipal(out aSid, aName: RawUtf8): Boolean;
    function  PickOwner(out aSid, aName: RawUtf8): Boolean;
    function PickObject(out aGUID, aName: RawUtf8; aAllowedtype: TRawUtf8DynArray): Boolean;

    procedure ShowError(const aMsg: RawUtf8);
    procedure CloseRequest;
  end;

  { TAdvancedSecurityPresenter }

  TAdvancedSecurityPresenter = class
  private
    fView: IAdvancedSecurityView;
    fLdapClient: TLdapClient;
    fName: RawUtf8;
    fDistinguishedName: RawUtf8;
    fSD: TSecurityDescriptor;
    fSidCache: TSidCache;
    fGuidCache: TGuidCache;
    fACEs: TDocVariantData;
    fOwnerChanged: Boolean;
    fDACLChanged: Boolean;
    fFocusedACEIndex: Integer;
    fIsUpdatingCount: Integer;

    function GetSDChanged: Boolean;
    procedure NotifyButtonStates;

    procedure ResolveViewData;
    procedure LoadViewFromSD(const aSD: TSecurityDescriptor);
    procedure LoadViewFromDACL(const aDACL: TSecACL);
    procedure SaveViewToDACL(out aDACL: TSecAcl);

    procedure ACEToDoc(const aACE: TSecAce; aIndex: Integer;
                        out aDoc: TDocVariantData);
    function DocToACE(const aDoc: TDocVariantData): TSecAce;
    function NewACENextIndex: Integer;
    procedure UpdateACEDocState(aACE: PDocVariantData; aState: Integer);

    procedure BeginUpdate;
    procedure EndUpdate;
    function IsUpdating: Boolean;
  public
    constructor Create(aView: IAdvancedSecurityView);
    destructor Destroy; override;

    // Setup the LdapClient used to contact the server.
    // Providing a valid LdapClient fill the guid cache.
    procedure SetLdapClient(aClient: TLdapClient);
    // Setup the DistinguishedName used to modify the SD.
    procedure SetDistinguishedName(const aDN: RawUtf8);
    // Setup the Name of the object used for the window title.
    procedure SetName(const aName: RawUtf8);
    // Setup the default SD to use.
    // Providing a valid SD fill the view with the data.
    procedure SetSecurityDescriptor(const aSD: TSecurityDescriptor);
    // Retrieve the current SD.
    function GetSecurityDescriptor: TSecurityDescriptor;
    // Change the focused ACE.
    // Used to update the RightPanel in the view.
    procedure DoACESelectionChanged(aACEIndex: Integer);
    procedure DoRightPanelChanged;
    // Update the focused ACE mask with the data from the view.
    procedure DoRightPanelMaskChanged;
    // Update the focused ACE type with the data from the view.
    procedure DoRightPanelTypeChanged;
    // Update the focused ACE principal with the data from the view.
    procedure DoRightPanelPrincipalChanged;
    // Update the focused ACE object with the data from the view.
    procedure DoRightPanelObjectChanged;
    // Update the focused ACE flags with the data from the view.
    procedure DoRightPanelFlagsChanged;
    // Update the focused ACE inherited object with the data from the view.
    procedure DoRightPanelInheritedObjectChanged;

    function ActionAddACE: Integer;
    function ActionDuplicateACE: Integer;
    function ActionDeleteACE: Integer;
    procedure ActionSelectOwner;
    procedure ActionSelectPrincipal;
    procedure ActionSelectObject;
    procedure ActionSelectInheritedObject;
    procedure ActionApply;
    procedure ActionCancel;
    procedure ActionOK;

    property SidCache: TSidCache read FSidCache;
    property GuidCache: TGUIDCache read FGuidCache;
    property SDChanged: Boolean read GetSDChanged;
    property LdapClient: TLdapClient read fLdapClient;
  end;

function AccessMaskToString(AMask: TSecAccessMask): RawUtf8;
function InheritanceFlagsToString(AFlags: TSecAceFlags): RawUtf8;

implementation
uses
  ursatldapclient,
  ucommon;

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

{ TAdvancedSecurityPresenter }

function TAdvancedSecurityPresenter.GetSDChanged: Boolean;
begin
  result := fOwnerChanged or fDACLChanged;
end;

procedure TAdvancedSecurityPresenter.NotifyButtonStates;
begin
  fView.SetApplyEnabled(SDChanged);
  fView.SetDeleteEnabled(fFocusedACEIndex >= 0);
  fView.SetDuplicateEnabled(fFocusedACEIndex >= 0);
end;

procedure TAdvancedSecurityPresenter.ResolveViewData;
var
  i: Integer;

  procedure ResolveDoc(aDoc: PDocVariantData);
  begin
    aDoc^.AddOrUpdateValue('type',
      (satAccessAllowed = TSecAceType(Integer(aDoc^.I['ace_type']))) or
      (satObjectAccessAllowed = TSecAceType(Integer(aDoc^.I['ace_type'])))
    );
    aDoc^.AddOrUpdateValue('account', fSidCache.SIDToName(aDoc^.S['account_sid']));
    aDoc^.AddOrUpdateValue('permissions', AccessMaskToString(TSecAccessMask(Integer(aDoc^.I['rights']))));
    aDoc^.AddOrUpdateValue('object', fGuidCache.GUIDToName(aDoc^.S['object_guid'], ''));
    aDoc^.AddOrUpdateValue('flags', InheritanceFlagsToString(TSecAceFlags(Int8(aDoc^.I['ace_flags']))));
    aDoc^.AddOrUpdateValue('inheritedObject', fGuidCache.GUIDToName(aDoc^.S['inherited_object_guid'], ''));
  end;

begin
  fSidCache.ResolveSIDS(fLdapClient);

  for i := 0 to Pred(fACEs.Count) do
    ResolveDoc(fACEs._[i]);
end;

procedure TAdvancedSecurityPresenter.LoadViewFromSD(
  const aSD: TSecurityDescriptor);
begin
  fSidCache.AddSID(RawSidToText(fSD.Owner));
  fSidCache.AddSID(RawSidToText(fSD.Group));

  LoadViewFromDACL(aSD.Dacl);

  ResolveViewData;

  fView.SetOwnerText(fSidCache.SIDToName(RawSidToText(fSD.Owner)));
  fView.SetGroupText(fSidCache.SIDToName(RawSidToText(fSD.Group)));

  fView.RefreshACEGrid(@fACEs);
end;

procedure TAdvancedSecurityPresenter.LoadViewFromDACL(const aDACL: TSecACL);
var
  i: Integer;
  Doc: TDocVariantData;
begin
  fACEs.Clear;
  Doc.Init(JSON_FAST);
  for i := 0 to Pred(Length(aDACL)) do
  begin
    fSidCache.AddSID(RawSidToText(aDACL[i].Sid));
    AceToDoc(aDACL[i], i, Doc);
    fACEs.AddItem(Doc);
    Doc.Clear;
  end;
end;

procedure TAdvancedSecurityPresenter.SaveViewToDACL(out aDACL: TSecAcl);
var
  i, c: Integer;
begin
  aDACL := nil;
  c := 0;
  for i := 0 to Pred(fACEs.Count) do
    if not fACEs._[i]^.Exists('state') or (fACEs._[i]^.I['state'] <> 3) then
    begin
      Insert(DocToACE(fACEs._[i]^), aDACL, c);
      Inc(c);
    end;
end;

procedure TAdvancedSecurityPresenter.ACEToDoc(const aACE: TSecAce;
  aIndex: Integer; out aDoc: TDocVariantData);
begin
  aDoc.AddOrUpdateValue('index', aIndex);
  aDoc.AddOrUpdateValue('ace_type', Integer(aACE.AceType));
  aDoc.AddOrUpdateValue('account_sid', RawSidToText(aACE.Sid));
  aDoc.AddOrUpdateValue('rights', Integer(aACE.Mask));
  aDoc.AddOrUpdateValue('object_guid', aACE.ObjectText());
  aDoc.AddOrUpdateValue('ace_flags', Int8(aACE.Flags));
  aDoc.AddOrUpdateValue('inherited_object_guid', aACE.InheritedText());
end;

function TAdvancedSecurityPresenter.DocToACE(const aDoc: TDocVariantData): TSecAce;
begin
  result.AceType := TSecAceType(aDoc.I['ace_type']);
  result.Sid := TextToRawSid(aDoc.S['account_sid']);
  result.Mask := TSecAccessMask(Integer(aDoc.I['rights']));
  result.ObjectType := RawUtf8ToGuid(aDoc.S['object_guid']);
  result.Flags := TSecAceFlags(Int8(aDoc.I['ace_flags']));
  result.InheritedObjectType := RawUtf8ToGuid(aDoc.S['inherited_object_guid']);
end;

function TAdvancedSecurityPresenter.NewACENextIndex: Integer;
begin
  result := fACEs.Count;
end;

procedure TAdvancedSecurityPresenter.UpdateACEDocState(aACE: PDocVariantData; aState: Integer);
begin
  case aState of
  1, 3:
    aACE^.AddOrUpdateValue('state', aState);
  2:
  begin
    if not aACE^.Exists('state') or ((aACE^.I['state'] <> 1) and (aACE^.I['state'] <> 3)) then
      aACE^.AddOrUpdateValue('state', aState);
  end
  end;
end;

procedure TAdvancedSecurityPresenter.BeginUpdate;
begin
  Inc(fIsUpdatingCount);
end;

procedure TAdvancedSecurityPresenter.EndUpdate;
begin
  Dec(fIsUpdatingCount);
end;

function TAdvancedSecurityPresenter.IsUpdating: Boolean;
begin
  result := fIsUpdatingCount > 0;
end;

constructor TAdvancedSecurityPresenter.Create(aView: IAdvancedSecurityView);
begin
  if not Assigned(aView) then
    raise Exception.Create('Missing aView.');

  fView := aView;
  fLdapClient := nil;
  fName := '';
  fDistinguishedName := '';
  fSidCache := TSidCache.Create;
  fGuidCache := TGUIDCache.Create;
  fACEs.InitArray([], JSON_FAST);
  fOwnerChanged := False;
  fDACLChanged := False;
  fFocusedACEIndex := -1;

  fIsUpdatingCount := 0;
  NotifyButtonStates;
end;

destructor TAdvancedSecurityPresenter.Destroy;
begin
  fView := nil;

  FreeAndNil(fSidCache);
  FreeAndNil(fGuidCache);
  inherited Destroy;
end;

procedure TAdvancedSecurityPresenter.SetLdapClient(aClient: TLdapClient);
begin
  if fLdapClient = AClient then
    Exit;
  fLdapClient := aClient;

  fGuidCache.ResolveGUIDS(fLdapClient);
end;

procedure TAdvancedSecurityPresenter.SetDistinguishedName(const aDN: RawUtf8);
begin
  if fDistinguishedName = aDN then
    Exit;
  fDistinguishedName := aDN;
end;

procedure TAdvancedSecurityPresenter.SetName(const aName: RawUtf8);
begin
  if fName = aName then
    Exit;
  fName := aName;

  fView.SetTitle(fName);
end;

procedure TAdvancedSecurityPresenter.SetSecurityDescriptor(
  const aSD: TSecurityDescriptor);
begin
  fSD := aSD;

  LoadViewFromSD(fSD);
end;

function TAdvancedSecurityPresenter.GetSecurityDescriptor: TSecurityDescriptor;
begin
  result := fSD;
end;

procedure TAdvancedSecurityPresenter.DoACESelectionChanged(aACEIndex: Integer);
begin
  if (aACEIndex < 0) or (aACEIndex >= fACEs.Count) then
    Exit;

  fFocusedACEIndex := aACEIndex;
  BeginUpdate;
  try
    fView.SetRightPanelType(
      (satAccessAllowed = TSecAceType(Integer(fACEs._[fFocusedACEIndex]^.I['ace_type']))) or
      (satObjectAccessAllowed = TSecAceType(Integer(fACEs._[fFocusedACEIndex]^.I['ace_type'])))
    );
    fView.SetRightPanelAccount(fSidCache.SIDToName(fACEs._[fFocusedACEIndex]^.S['account_sid']));
    fView.SetRightPanelMask(TSecAccessMask(Integer(fACEs._[fFocusedACEIndex]^.I['rights'])));
    fView.SetRightPanelObject(fGuidCache.GUIDToName(fACEs._[fFocusedACEIndex]^.S['object_guid'], ''));
    fView.SetRightPanelFlags(TSecAceFlags(Int8(fACEs._[fFocusedACEIndex]^.I['ace_flags'])));
    fView.SetRightPanelInheritedObject(fGuidCache.GUIDToName(fACEs._[fFocusedACEIndex]^.S['inherited_object_guid'], ''));
  finally
    EndUpdate;
  end;
  NotifyButtonStates;
end;

procedure TAdvancedSecurityPresenter.DoRightPanelChanged;
begin
  if IsUpdating then
    Exit;

  DoRightPanelTypeChanged;
  DoRightPanelPrincipalChanged;
  DoRightPanelMaskChanged;
  DoRightPanelObjectChanged;
  DoRightPanelFlagsChanged;
  DoRightPanelInheritedObjectChanged;
end;

procedure TAdvancedSecurityPresenter.DoRightPanelMaskChanged;
var
  CurrentMask: TSecAccessMask;
begin
  if IsUpdating then
    Exit;

  CurrentMask := fView.GetCurrentMask;
  fACEs._[fFocusedACEIndex]^.AddOrUpdateValue('rights', Integer(CurrentMask));
  fACEs._[fFocusedACEIndex]^.AddOrUpdateValue('permissions', AccessMaskToString(CurrentMask));
  UpdateACEDocState(fACEs._[fFocusedACEIndex], 2);
  fView.RefreshACEGridIndex(fACEs._[fFocusedACEIndex], fFocusedACEIndex);
  fDACLChanged := True;
  NotifyButtonStates;
end;

procedure TAdvancedSecurityPresenter.DoRightPanelTypeChanged;
var
  HasObjectLinked, CurrentTypeAllow: Boolean;
  ACEType: TSecAceType;
begin
  if IsUpdating then
    Exit;

  CurrentTypeAllow := fView.GetCurrentType;
  HasObjectLinked := (fView.GetCurrentObjectText <> '') or (fView.GetCurrentInheritedObjectText <> '');

  if CurrentTypeAllow then
  begin
    if HasObjectLinked then
      ACEType := satObjectAccessAllowed
    else
      ACEType := satAccessAllowed;
  end
  else
  begin
    if HasObjectLinked then
      ACEType := satObjectAccessDenied
    else
      ACEType := satAccessDenied;
  end;
  fACEs._[fFocusedACEIndex]^.AddOrUpdateValue('ace_type', Ord(ACEType));
  fACEs._[fFocusedACEIndex]^.AddOrUpdateValue('type', (ACEType = satObjectAccessAllowed) or (ACEType = satAccessAllowed));
  UpdateACEDocState(fACEs._[fFocusedACEIndex], 2);
  fView.RefreshACEGridIndex(fACEs._[fFocusedACEIndex], fFocusedACEIndex);
  fDACLChanged := True;
  NotifyButtonStates;
end;

procedure TAdvancedSecurityPresenter.DoRightPanelPrincipalChanged;
var
  CurrentPrincipal, SID: RawUtf8;
begin
  if IsUpdating then
    Exit;

  CurrentPrincipal := fView.GetCurrentPrincipalText;
  SID := fSidCache.NameToSID(CurrentPrincipal);
  fACEs._[fFocusedACEIndex]^.AddOrUpdateValue('account_sid', SID);
  fACEs._[fFocusedACEIndex]^.AddOrUpdateValue('account', CurrentPrincipal);
  UpdateACEDocState(fACEs._[fFocusedACEIndex], 2);
  fView.RefreshACEGridIndex(fACEs._[fFocusedACEIndex], fFocusedACEIndex);
  fDACLChanged := True;
  NotifyButtonStates;
end;

procedure TAdvancedSecurityPresenter.DoRightPanelObjectChanged;
var
  CurrentObject, GUID: RawUtf8;
begin
  if IsUpdating then
    Exit;

  CurrentObject := fView.GetCurrentObjectText;
  GUID := fGuidCache.NameToGUID(CurrentObject);
  fACEs._[fFocusedACEIndex]^.AddOrUpdateValue('object_guid', GUID);
  fACEs._[fFocusedACEIndex]^.AddOrUpdateValue('object', CurrentObject);
  UpdateACEDocState(fACEs._[fFocusedACEIndex], 2);
  fView.RefreshACEGridIndex(fACEs._[fFocusedACEIndex], fFocusedACEIndex);
  fDACLChanged := True;
  NotifyButtonStates;
end;

procedure TAdvancedSecurityPresenter.DoRightPanelFlagsChanged;
var
  CurrentFlags: TSecAceFlags;
begin
  if IsUpdating then
    Exit;

  CurrentFlags := fView.GetCurrentFlags;
  fACEs._[fFocusedACEIndex]^.AddOrUpdateValue('ace_flags', Int8(CurrentFlags));
  fACEs._[fFocusedACEIndex]^.AddOrUpdateValue('flags', InheritanceFlagsToString(CurrentFlags));
  UpdateACEDocState(fACEs._[fFocusedACEIndex], 2);
  fView.RefreshACEGridIndex(fACEs._[fFocusedACEIndex], fFocusedACEIndex);
  fDACLChanged := True;
  NotifyButtonStates;
end;

procedure TAdvancedSecurityPresenter.DoRightPanelInheritedObjectChanged;
var
  CurrentInheritedObject, GUID: RawUtf8;
begin
  if IsUpdating then
    Exit;

  CurrentInheritedObject := fView.GetCurrentInheritedObjectText;
  GUID := fGuidCache.NameToGUID(CurrentInheritedObject);
  fACEs._[fFocusedACEIndex]^.AddOrUpdateValue('inherited_object_guid', GUID);
  fACEs._[fFocusedACEIndex]^.AddOrUpdateValue('inheritedObject', CurrentInheritedObject);
  UpdateACEDocState(fACEs._[fFocusedACEIndex], 2);
  fView.RefreshACEGridIndex(fACEs._[fFocusedACEIndex], fFocusedACEIndex);
  fDACLChanged := True;
  NotifyButtonStates;
end;

function TAdvancedSecurityPresenter.ActionAddACE: Integer;
var
  NewACE: TDocVariantData;
begin
  NewACE.Init(JSON_FAST);
  NewACE.I['index'] := NewACENextIndex;
  NewACE.I['state'] := 1; // 0: Nothing; 1: New; 2: Update; 3: Deleted
  NewACE.I['ace_type'] := Ord(satAccessDenied);
  NewACE.B['type'] := False;
  NewACE.I['rights'] := Integer([samWriteOwner]);
  NewACE.S['permissions'] := AccessMaskToString([samWriteOwner]);
  NewACE.S['account_sid'] := RawSidToText(KnownRawSid(wksAnonymous));
  NewACE.S['account'] := WELL_KNOWN_SID_NAMES[wksAnonymous];
  fSidCache.AddSID(NewACE.S['account_sid']);
  fSidCache.SetSIDName(NewACE.S['account_sid'], NewACE.S['account']);
  fACEs.AddItem(NewACE);
  fView.RefreshACEGrid(@fACEs);
  fView.SelectACE(NewACE.I['index']);
  fDACLChanged := True;
  NotifyButtonStates;
  result := NewACE.I['index'];
end;

function TAdvancedSecurityPresenter.ActionDuplicateACE: Integer;
var
  FocusedData: PDocVariantData;
  NewACE: TDocVariantData;
  n: RawUtf8;
begin
  FocusedData := fACEs._[fFocusedACEIndex];

  NewACE.Init(JSON_FAST);
  for n in FocusedData^.Names do
  begin
    if n = '' then
      continue;
    NewACE.AddOrUpdateValue(n, FocusedData^.Value[n]);
  end;
  NewACE.AddOrUpdateValue('index', NewACENextIndex);
  NewACE.AddOrUpdateValue('state', 1);
  fACEs.AddItem(NewACE);
  fView.RefreshACEGrid(@fACEs);
  fView.SelectACE(NewACE.I['index']);
  fDACLChanged := True;
  NotifyButtonStates;
  result := NewACE.I['index'];
end;

function TAdvancedSecurityPresenter.ActionDeleteACE: Integer;
var
  FocusedData: PDocVariantData;
begin
  FocusedData := fACEs._[fFocusedACEIndex];

  FocusedData^.AddOrUpdateValue('state', 3);
  fView.RefreshACEGridIndex(FocusedData, FocusedData^.I['index']);
  fDACLChanged := True;
  NotifyButtonStates;
  result := FocusedData^.I['index'];
end;

procedure TAdvancedSecurityPresenter.ActionSelectOwner;
var
  Sid, Name: RawUtf8;
begin
  if not fView.PickOwner(Sid, Name) then
    Exit;

  fSidCache.AddSID(Sid);
  fSidCache.SetSIDName(Sid, Name);
  fSD.Owner := TextToRawSid(Sid);
  fOwnerChanged := True;

  fView.SetOwnerText(Name);
  NotifyButtonStates;
end;

procedure TAdvancedSecurityPresenter.ActionSelectPrincipal;
var
  Sid, Name: RawUtf8;
begin
  if not fView.PickPrincipal(Sid, Name) then
    Exit;

  fSidCache.AddSID(Sid);
  fSidCache.SetSIDName(Sid, Name);

  fView.SetRightPanelAccount(Name);
end;

procedure TAdvancedSecurityPresenter.ActionSelectObject;
var
  GUID, Name: RawUtf8;
begin
  if not fView.PickObject(GUID, Name, nil) then
    Exit;

  fView.SetRightPanelObject(Name);
  DoRightPanelTypeChanged;
end;

procedure TAdvancedSecurityPresenter.ActionSelectInheritedObject;
var
  GUID, Name: RawUtf8;
begin
  if not fView.PickObject(GUID, Name, ['classSchema']) then
    Exit;

  fView.SetRightPanelInheritedObject(Name);
  DoRightPanelTypeChanged;
end;

procedure TAdvancedSecurityPresenter.ActionApply;
var
  DACL: TSecAcl;
begin
  if not SDChanged then
    Exit;

  SaveViewToDACL(DACL);
  OrderACL(fLdapClient, fDistinguishedName, @DACL);
  fSD.Dacl := DACL;

  if not fLdapClient.Modify(fDistinguishedName, lmoReplace, 'nTSecurityDescriptor', fSD.ToBinary) then
  begin
    fView.ShowError('Failed to modify nTSecurityDescriptor.');
    Exit;
  end;

  if not fSD.FromBinary(fLdapClient.SearchObject(fDistinguishedName, '', 'nTSecurityDescriptor').GetRaw()) then
  begin
    fView.ShowError('Failed to retrieve nTSecurityDescriptor.');
    Exit;
  end;

  LoadViewFromSD(fSD);

  fDACLChanged := False;
  fOwnerChanged := False;
  NotifyButtonStates;
end;

procedure TAdvancedSecurityPresenter.ActionCancel;
begin
  fView.CloseRequest;
end;

procedure TAdvancedSecurityPresenter.ActionOK;
begin
  ActionApply;

  fView.CloseRequest;
end;

end.

