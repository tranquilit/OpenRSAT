unit unittest.uldapedit;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.test,
  mormot.core.text,
  mormot.net.ldap,
  uldapedit; // Adjust if TLdapEditSession lives in another unit.

type
  // Test-only access to protected internals for branch/invariant tests.
  TLdapEditSessionAccess = class(TLdapEditSession)
  public
    function AccessAttr(const Name: RawUtf8): TLdapAttribute;
    function StateIndex(const DN, Name: RawUtf8; const Value: RawByteString): Integer;
    function StateAt(Index: Integer): TLdapValueState;
    function StateCount: Integer;
    function EqualAttr(A, B: TLdapAttribute): Boolean;
    property DN: RawUtf8 read fDN;
    property Initial: TLdapAttributeList read fInitial;
    property Working: TLdapAttributeList read fWorking;
  end;

  TUnitTestLdapEditSession = class(TSynTestCase)
  private
    fSession: TLdapEditSessionAccess;
    procedure ResetSession;
    procedure LoadEmpty;
    procedure LoadStandard;
    procedure CheckRawArray(const Actual: TRawByteStringDynArray;
      const Expected: array of RawByteString; const Context: RawUtf8);
    procedure CheckUtf8Array(const Actual: TRawUtf8DynArray;
      const Expected: array of RawUtf8; const Context: RawUtf8);
    procedure CheckRawSet(const Actual: TRawByteStringDynArray;
      const Expected: array of RawByteString; const Context: RawUtf8);
    procedure CheckDNSet(const Actual: TRawUtf8DynArray;
      const Expected: array of RawUtf8; const Context: RawUtf8);
    procedure CheckState(const DN, Name: RawUtf8; const Value: RawByteString;
      Initial, Working: TLdapPresence; const Context: RawUtf8 = '');
  public
    procedure MethodSetup; override;
    procedure MethodCleanUp; override;
  published
    procedure Create_Unloaded;
    procedure Load_Empty;
    procedure Load_ClonesAndOwnsSnapshots;
    procedure Load_AlreadyLoaded;
    procedure GetAttr_ExistingAndNew;
    procedure Read_RawAndReadableIndexes;
    procedure Read_MissingAndEmpty;
    procedure Read_RawVersusReadableGuid;
    procedure Read_ArrayIsolation;
    procedure SetValue_CreateReplaceAndRevert;
    procedure SetValues_ReplaceDeduplicateAndReorder;
    procedure SetValues_EmptyCollections;
    procedure ZeroLengthValue_IsNotMissingAttribute;
    procedure AddValue_CreateAppendAndDuplicate;
    procedure RemoveValue_MissingAndNotFound;
    procedure RemoveValue_FirstMiddleLastAndOnly;
    procedure RemoveValue_SpareCapacity;
    procedure Binary_LocalRoundTrip;
    procedure Text_Utf8RoundTrip;
    procedure ClearAttribute_ExistingMissingAndRepeated;
    procedure Restore_ExistingDeletedNewAndMissing;
    procedure AttributeEqual_AllBranches;
    procedure IsModified_LocalBranches;
    procedure IsModified_EmptyInitialAttribute;
    procedure External_IndexKeyAndBinary;
    procedure External_LoadDefaultAbsentUnknownAndRepeated;
    procedure External_AllPresenceTransitions;
    procedure External_ResolveUnknownMatrix;
    procedure External_SetNewAllPresences;
    procedure External_ForwardProjection;
    procedure External_BackwardProjection;
    procedure External_BinaryProjectionAndRestore;
    procedure External_RestoreMissingAndCancel;
    procedure IsModified_ExternalOnlyAndLocalScope;
    procedure RestoreAll_NoExternalAndMixed;
    procedure InvalidLocalName_MutatorsReject;
    procedure External_KeyRepresentation_CurrentPolicy;
    procedure RestoreLocal_PreservesExternal;
    procedure Revision_CharacterizesCurrentPolicy;
    procedure Read_NegativeReadableIndex;

  end;

implementation

const
  USER_DN = 'CN=Alice,OU=Users,DC=test,DC=lan';
  OTHER_USER = 'CN=Bob,OU=Users,DC=test,DC=lan';
  GROUP_A = 'CN=Support,OU=Groups,DC=test,DC=lan';
  GROUP_B = 'CN=VPN,OU=Groups,DC=test,DC=lan';
  GROUP_C = 'CN=IT,OU=Groups,DC=test,DC=lan';
  GROUP_D = 'CN=Finance,OU=Groups,DC=test,DC=lan';
  GROUP_E = 'CN=Sales,OU=Groups,DC=test,DC=lan';
  GROUP_F = 'CN=Audit,OU=Groups,DC=test,DC=lan';

function Bytes(const Data: array of Byte): RawByteString;
var
  i: Integer;
begin
  SetLength(Result, Length(Data));
  for i := 0 to High(Data) do
    Result[i + 1] := AnsiChar(Data[i]);
end;

function RawValues(const Values: array of RawByteString): TRawByteStringDynArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, Length(Values));
  for i := 0 to High(Values) do
    Result[i] := Values[i];
end;

function SameBytes(const A, B: RawByteString): Boolean;
begin
  if Length(A) <> Length(B) then
    Exit(False);
  if Length(A) = 0 then
    Exit(True);
  Result := CompareMem(Pointer(A), Pointer(B), Length(A));
end;

{ TLdapEditSessionAccess }

function TLdapEditSessionAccess.AccessAttr(const Name: RawUtf8): TLdapAttribute;
begin
  Result := GetAttr(Name);
end;

function TLdapEditSessionAccess.StateIndex(const DN, Name: RawUtf8;
  const Value: RawByteString): Integer;
begin
  Result := GetValueStateIndex(DN, Name, Value);
end;

function TLdapEditSessionAccess.StateAt(Index: Integer): TLdapValueState;
begin
  Result := fExternalValueStates[Index];
end;

function TLdapEditSessionAccess.StateCount: Integer;
begin
  Result := Length(fExternalValueStates);
end;

function TLdapEditSessionAccess.EqualAttr(A, B: TLdapAttribute): Boolean;
begin
  Result := AttributeEqual(A, B);
end;

{ TUnitTestLdapEditSession }

procedure TUnitTestLdapEditSession.MethodSetup;
begin
  inherited MethodSetup;
  fSession := TLdapEditSessionAccess.Create;
end;

procedure TUnitTestLdapEditSession.MethodCleanUp;
begin
  FreeAndNil(fSession);
  inherited MethodCleanUp;
end;

procedure TUnitTestLdapEditSession.ResetSession;
begin
  FreeAndNil(fSession);
  fSession := TLdapEditSessionAccess.Create;
end;

procedure TUnitTestLdapEditSession.LoadEmpty;
var
  Source: TLdapAttributeList;
begin
  Source := TLdapAttributeList.Create;
  try
    fSession.Load(USER_DN, Source);
  finally
    Source.Free;
  end;
end;

procedure TUnitTestLdapEditSession.LoadStandard;
var
  Source: TLdapAttributeList;
begin
  Source := TLdapAttributeList.Create;
  try
    Source.Add('description', 'Original');
    Source.Add('otherTelephone', 'A');
    Source.Add('otherTelephone', 'B');
    Source.Add('otherTelephone', 'C');
    Source.Add('telephoneNumber', '0123456789');
    fSession.Load(USER_DN, Source);
  finally
    Source.Free;
  end;
end;

procedure TUnitTestLdapEditSession.CheckRawArray(const Actual: TRawByteStringDynArray;
  const Expected: array of RawByteString; const Context: RawUtf8);
var
  i: Integer;
begin
  if not CheckEqual(Length(Actual), Length(Expected), Context + ' count') then
    Exit;
  for i := 0 to High(Expected) do
    CheckEqual(Actual[i], Expected[i], FormatUtf8('% index %', [Context, i]));
end;

procedure TUnitTestLdapEditSession.CheckUtf8Array(const Actual: TRawUtf8DynArray;
  const Expected: array of RawUtf8; const Context: RawUtf8);
var
  i: Integer;
begin
  if not CheckEqual(Length(Actual), Length(Expected), Context + ' count') then
    Exit;
  for i := 0 to High(Expected) do
    CheckEqual(Actual[i], Expected[i], FormatUtf8('% index %', [Context, i]));
end;

procedure TUnitTestLdapEditSession.CheckRawSet(const Actual: TRawByteStringDynArray;
  const Expected: array of RawByteString; const Context: RawUtf8);
var
  Matched: array of Boolean;
  i, j: Integer;
  Found: Boolean;
begin
  if not CheckEqual(Length(Actual), Length(Expected), Context + ' count') then
    Exit;
  SetLength(Matched, Length(Actual));
  for i := 0 to High(Expected) do
  begin
    Found := False;
    for j := 0 to High(Actual) do
      if not Matched[j] and SameBytes(Actual[j], Expected[i]) then
      begin
        Matched[j] := True;
        Found := True;
        Break;
      end;
    CheckUtf8(Found, '% expected item % not found', [Context, i]);
  end;
end;

procedure TUnitTestLdapEditSession.CheckDNSet(const Actual: TRawUtf8DynArray;
  const Expected: array of RawUtf8; const Context: RawUtf8);
var
  A, E: TRawByteStringDynArray;
  i: Integer;
begin
  A := nil;
  E := nil;
  SetLength(A, Length(Actual));
  for i := 0 to High(Actual) do
    A[i] := Actual[i];
  SetLength(E, Length(Expected));
  for i := 0 to High(Expected) do
    E[i] := Expected[i];
  CheckRawSet(A, E, Context);
end;

procedure TUnitTestLdapEditSession.CheckState(const DN, Name: RawUtf8;
  const Value: RawByteString; Initial, Working: TLdapPresence;
  const Context: RawUtf8);
var
  i: Integer;
  S: TLdapValueState;
begin
  i := fSession.StateIndex(DN, Name, Value);
  if not CheckUtf8((i >= 0) and (i < fSession.StateCount),
    '% state must exist: % / %', [Context, DN, Name]) then
    Exit;
  S := fSession.StateAt(i);
  CheckEqual(S.DN, DN, Context + ' DN');
  CheckEqual(S.AttributeName, Name, Context + ' attribute');
  CheckEqual(S.Value, Value, Context + ' raw value');
  CheckEqual(Ord(S.InitialPresence), Ord(Initial), Context + ' initial');
  CheckEqual(Ord(S.WorkingPresence), Ord(Working), Context + ' working');
end;

procedure TUnitTestLdapEditSession.Create_Unloaded;
begin
  CheckEqual(fSession.Revision, 0);
  CheckEqual(fSession.DN, '');
  Check(fSession.Initial = nil, 'Initial must be nil before Load');
  Check(fSession.Working = nil, 'Working must be nil before Load');
  CheckEqual(fSession.StateCount, 0);
end;

procedure TUnitTestLdapEditSession.Load_Empty;
begin
  LoadEmpty;
  CheckEqual(fSession.DN, USER_DN);
  Check(Assigned(fSession.Initial));
  Check(Assigned(fSession.Working));
  Check(fSession.Initial <> fSession.Working);
  CheckEqual(fSession.Initial.Count, 0);
  CheckEqual(fSession.Working.Count, 0);
  CheckEqual(fSession.Revision, 0);
  Check(not fSession.IsModified, 'Empty session is clean');
end;

procedure TUnitTestLdapEditSession.Load_ClonesAndOwnsSnapshots;
var
  Source: TLdapAttributeList;
begin
  Source := TLdapAttributeList.Create;
  try
    Source.Add('description', 'Original');
    Source.Add('otherTelephone', 'A');
    Source.Add('otherTelephone', 'B');
    fSession.Load(USER_DN, Source);
    Check(fSession.Initial <> Source);
    Check(fSession.Working <> Source);
    Check(fSession.Initial <> fSession.Working);
    Check(fSession.Initial.Find('description') <> Source.Find('description'));
    Check(fSession.Working.Find('description') <> Source.Find('description'));
    Check(fSession.Working.Find('description') <>
      fSession.Initial.Find('description'));
    Source.Add('description', 'Source changed', aoReplaceValue);
    Source.Add('otherTelephone', 'C');
    Source.Delete('description');
    CheckEqual(fSession.GetRaw('description'), 'Original');
    CheckRawArray(fSession.GetAllRaw('otherTelephone'), ['A', 'B'], 'Source isolated');
  finally
    Source.Free;
  end;
  CheckEqual(fSession.GetRaw('description'), 'Original', 'Source already freed');
  fSession.SetValue('description', 'Working changed');
  CheckEqual(fSession.Initial.Find('description').GetRaw, 'Original');
  CheckEqual(fSession.GetRaw('description'), 'Working changed');
  Check(fSession.IsModified);
  fSession.Restore('description');
  CheckEqual(fSession.GetRaw('description'), 'Original');
  Check(not fSession.IsModified);
end;

procedure TUnitTestLdapEditSession.Load_AlreadyLoaded;
var
  Source: TLdapAttributeList;
  Raised: Boolean;
  Rev: Int64;
begin
  LoadStandard;
  fSession.SetValue('description', 'Edited');
  fSession.LoadValueState(GROUP_A, 'member', USER_DN);
  Rev := fSession.Revision;
  Source := TLdapAttributeList.Create;
  try
    Source.Add('description', 'Other');
    Raised := False;
    try
      fSession.Load(GROUP_B, Source);
    except
      on E: Exception do
      begin
        Raised := True;
        CheckEqual(E.Message, 'Already loaded');
      end;
    end;
    Check(Raised, 'Second Load must raise');
  finally
    Source.Free;
  end;
  CheckEqual(fSession.DN, USER_DN);
  CheckEqual(fSession.GetRaw('description'), 'Edited');
  CheckEqual(fSession.Initial.Find('description').GetRaw, 'Original');
  CheckEqual(fSession.Revision, Rev);
  CheckState(GROUP_A, 'member', USER_DN, lpPresent, lpPresent);
end;

procedure TUnitTestLdapEditSession.GetAttr_ExistingAndNew;
var
  A, B: TLdapAttribute;
  N: Integer;
begin
  LoadStandard;
  N := fSession.Working.Count;
  A := fSession.AccessAttr('description');
  Check(A = fSession.Working.Find('description'));
  CheckEqual(fSession.Working.Count, N);
  Check(fSession.AccessAttr('DESCRIPTION') = A, 'Case-insensitive local name');
  B := fSession.AccessAttr('newAttribute');
  Check(Assigned(B));
  CheckEqual(B.Count, 0);
  CheckEqual(fSession.Working.Count, N + 1);
  Check(fSession.AccessAttr('newAttribute') = B);
  CheckEqual(fSession.Working.Count, N + 1);
  Check(fSession.Initial.Find('newAttribute') = nil);
end;

procedure TUnitTestLdapEditSession.Read_RawAndReadableIndexes;
begin
  LoadStandard;
  CheckEqual(fSession.GetRaw('description'), 'Original');
  CheckEqual(fSession.GetRaw('DESCRIPTION', 0), 'Original');
  CheckEqual(fSession.GetRaw('otherTelephone', 1), 'B');
  CheckEqual(fSession.GetRaw('otherTelephone', 2), 'C');
  CheckEqual(fSession.GetRaw('otherTelephone', 3), '');
  CheckEqual(fSession.GetRaw('otherTelephone', -1), '');
  CheckEqual(fSession.GetRaw('otherTelephone', Low(Integer)), '');
  CheckEqual(fSession.GetRaw('otherTelephone', High(Integer)), '');
  CheckEqual(fSession.GetReadable('description'), 'Original');
  CheckEqual(fSession.GetReadable('DESCRIPTION', 0), 'Original');
  CheckEqual(fSession.GetReadable('otherTelephone', 1), 'B');
  CheckEqual(fSession.GetReadable('otherTelephone', 2), 'C');
  CheckEqual(fSession.GetReadable('otherTelephone', 3), '');
  CheckEqual(fSession.GetReadable('otherTelephone', High(Integer)), '');
  CheckRawArray(fSession.GetAllRaw('otherTelephone'), ['A', 'B', 'C'], 'Raw all');
  CheckUtf8Array(fSession.GetAllReadable('otherTelephone'), ['A', 'B', 'C'],
    'Readable all');
  CheckRawArray(fSession.GetAllRaw('DESCRIPTION'), ['Original'], 'Single value');
  CheckUtf8Array(fSession.GetAllReadable('description'), ['Original'], 'Single value');
  CheckEqual(fSession.Revision, 0);
  Check(not fSession.IsModified);
end;

procedure TUnitTestLdapEditSession.Read_MissingAndEmpty;
var
  N: Integer;
  Rev: Int64;
begin
  LoadStandard;
  N := fSession.Working.Count;
  CheckEqual(fSession.GetRaw('missing'), '');
  CheckEqual(fSession.GetRaw('missing', 1), '');
  CheckEqual(fSession.GetReadable('missing'), '');
  CheckEqual(fSession.GetReadable('missing', -1), '');
  CheckEqual(fSession.GetReadable('missing', 1), '');
  CheckEqual(fSession.GetRaw(''), '');
  CheckEqual(fSession.GetReadable(''), '');
  CheckRawArray(fSession.GetAllRaw('missing'), [], 'Missing raw');
  CheckUtf8Array(fSession.GetAllReadable('missing'), [], 'Missing readable');
  CheckRawArray(fSession.GetAllRaw(''), [], 'Empty name raw');
  CheckUtf8Array(fSession.GetAllReadable(''), [], 'Empty name readable');
  CheckEqual(fSession.Working.Count, N, 'Reads must not allocate attributes');
  CheckEqual(fSession.Revision, 0);

  // Empty attribute object (not a single zero-length value).
  fSession.SetValues('emptyAttribute', nil);
  Rev := fSession.Revision;
  CheckEqual(fSession.GetRaw('emptyAttribute'), '');
  CheckEqual(fSession.GetReadable('emptyAttribute'), '');
  CheckRawArray(fSession.GetAllRaw('emptyAttribute'), [], 'Empty raw');
  CheckUtf8Array(fSession.GetAllReadable('emptyAttribute'), [], 'Empty readable');
  CheckEqual(fSession.Revision, Rev);
end;

procedure TUnitTestLdapEditSession.Read_RawVersusReadableGuid;
var
  Raw: RawByteString;
  All: TRawUtf8DynArray;
  Rev: Int64;
begin
  LoadEmpty;
  // GUID in LDAP binary layout: 3f2504e0-4f89-11d3-9a0c-0305e82c3301.
  Raw := Bytes([$E0, $04, $25, $3F, $89, $4F, $D3, $11,
    $9A, $0C, $03, $05, $E8, $2C, $33, $01]);
  fSession.SetValue('objectGUID', Raw);
  Rev := fSession.Revision;
  CheckEqual(fSession.GetRaw('objectGUID'), Raw);
  CheckEqual(LowerCase(fSession.GetReadable('objectGUID')),
    '3f2504e0-4f89-11d3-9a0c-0305e82c3301');
  All := fSession.GetAllReadable('objectGUID');
  if Check(Length(All) = 1, 'One readable GUID') then
  begin
    CheckEqual(LowerCase(All[0]), '3f2504e0-4f89-11d3-9a0c-0305e82c3301');
    All[0] := 'not the GUID';
  end;
  CheckRawArray(fSession.GetAllRaw('objectGUID'), [Raw], 'Raw GUID unchanged');
  CheckEqual(LowerCase(fSession.GetReadable('objectGUID')),
    '3f2504e0-4f89-11d3-9a0c-0305e82c3301');
  CheckEqual(fSession.Revision, Rev);
end;

procedure TUnitTestLdapEditSession.Read_ArrayIsolation;
var
  Raw: TRawByteStringDynArray;
  Readable: TRawUtf8DynArray;
begin
  LoadStandard;
  Raw := fSession.GetAllRaw('otherTelephone');
  Readable := fSession.GetAllReadable('otherTelephone');
  if Check((Length(Raw) = 3) and (Length(Readable) = 3)) then
  begin
    Raw[0] := 'Tampered raw';
    Readable[1] := 'Tampered readable';
  end;
  SetLength(Raw, 0);
  SetLength(Readable, 0);
  CheckRawArray(fSession.GetAllRaw('otherTelephone'), ['A', 'B', 'C'], 'Raw isolated');
  CheckUtf8Array(fSession.GetAllReadable('otherTelephone'), ['A', 'B', 'C'],
    'Readable isolated');
  CheckEqual(fSession.Revision, 0);
  Check(not fSession.IsModified);
end;

procedure TUnitTestLdapEditSession.SetValue_CreateReplaceAndRevert;
begin
  LoadStandard;
  fSession.SetValue('description', 'Edited');
  CheckRawArray(fSession.GetAllRaw('description'), ['Edited'], 'Replace single');
  Check(fSession.IsModified('description'));
  fSession.SetValue('DESCRIPTION', 'Original');
  Check(not fSession.IsModified('description'));
  Check(not fSession.IsModified);
  fSession.SetValue('otherTelephone', 'D');
  CheckRawArray(fSession.GetAllRaw('otherTelephone'), ['D'], 'Replace all values');
  Check(fSession.IsModified('otherTelephone'));
  fSession.SetValue('newAttribute', 'new');
  CheckRawArray(fSession.GetAllRaw('newAttribute'), ['new'], 'Create attribute');
  Check(fSession.IsModified('newAttribute'));
  Check(fSession.Initial.Find('newAttribute') = nil);
  fSession.SetValue('newAttribute', 'new');
  CheckRawArray(fSession.GetAllRaw('newAttribute'), ['new'], 'Same value');
  CheckEqual(fSession.Revision, 5);
end;

procedure TUnitTestLdapEditSession.SetValues_ReplaceDeduplicateAndReorder;
var
  V: TRawByteStringDynArray;
begin
  LoadStandard;
  V := RawValues(['C', 'A', 'B', 'A', 'C']);
  fSession.SetValues('otherTelephone', V);
  CheckRawArray(fSession.GetAllRaw('otherTelephone'), ['C', 'A', 'B'], 'Deduplicate');
  Check(not fSession.IsModified('otherTelephone'), 'Order does not matter');
  Check(not fSession.IsModified);
  V[0] := 'Caller changed';
  CheckRawArray(fSession.GetAllRaw('otherTelephone'), ['C', 'A', 'B'], 'Input isolated');
  fSession.SetValues('otherTelephone', RawValues(['A', 'D']));
  CheckRawArray(fSession.GetAllRaw('otherTelephone'), ['A', 'D'], 'Replace collection');
  Check(fSession.IsModified('otherTelephone'));
  fSession.SetValues('newAttribute', RawValues(['X', 'X', 'Y']));
  CheckRawArray(fSession.GetAllRaw('newAttribute'), ['X', 'Y'], 'Create collection');
  Check(fSession.IsModified('newAttribute'));
  fSession.SetValues('description', RawValues(['Original']));
  Check(not fSession.IsModified('description'));
end;

procedure TUnitTestLdapEditSession.SetValues_EmptyCollections;
var
  V: TRawByteStringDynArray;
begin
  LoadStandard;
  fSession.SetValues('otherTelephone', nil);
  CheckRawArray(fSession.GetAllRaw('otherTelephone'), [], 'Clear via SetValues');
  Check(fSession.IsModified('otherTelephone'));
  fSession.SetValues('missing', nil);
  CheckRawArray(fSession.GetAllRaw('missing'), [], 'Absent -> empty');
  Check(not fSession.IsModified('missing'), 'Absent and zero values are equivalent');
  SetLength(V, 0);
  fSession.SetValues('missing', V);
  Check(not fSession.IsModified('missing'));
  fSession.Restore('otherTelephone');
  CheckRawArray(fSession.GetAllRaw('otherTelephone'), ['A', 'B', 'C'], 'Restored');
  Check(not fSession.IsModified, 'Empty added attribute must not leave dirty state');
end;

procedure TUnitTestLdapEditSession.ZeroLengthValue_IsNotMissingAttribute;
begin
  LoadEmpty;
  fSession.SetValue('newAttribute', '');
  CheckRawArray(fSession.GetAllRaw('newAttribute'), [''], 'One empty value');
  CheckUtf8Array(fSession.GetAllReadable('newAttribute'), [''], 'One empty readable value');
  Check(fSession.IsModified('newAttribute'));
  fSession.AddValue('newAttribute', '');
  CheckRawArray(fSession.GetAllRaw('newAttribute'), [''], 'No duplicate empty value');
  fSession.RemoveValue('newAttribute', '');
  CheckRawArray(fSession.GetAllRaw('newAttribute'), [], 'Removed empty value');
  Check(not fSession.IsModified('newAttribute'), 'Absent on both sides');
  Check(not fSession.IsModified);
  fSession.SetValues('newAttribute', RawValues(['', '', 'X']));
  CheckRawArray(fSession.GetAllRaw('newAttribute'), ['', 'X'], 'Empty value dedup');
  fSession.ClearAttribute('newAttribute');
  Check(not fSession.IsModified);
end;

procedure TUnitTestLdapEditSession.AddValue_CreateAppendAndDuplicate;
begin
  LoadStandard;
  fSession.AddValue('otherTelephone', 'D');
  CheckRawArray(fSession.GetAllRaw('otherTelephone'), ['A', 'B', 'C', 'D'], 'Append');
  fSession.AddValue('OTHERTELEPHONE', 'D');
  CheckRawArray(fSession.GetAllRaw('otherTelephone'), ['A', 'B', 'C', 'D'], 'Duplicate');
  fSession.AddValue('newAttribute', 'X');
  CheckRawArray(fSession.GetAllRaw('newAttribute'), ['X'], 'Create');
  fSession.AddValue('newAttribute', 'x');
  CheckRawArray(fSession.GetAllRaw('newAttribute'), ['X', 'x'], 'Representation case');
  Check(fSession.IsModified);
  CheckEqual(fSession.Revision, 4);
end;

procedure TUnitTestLdapEditSession.RemoveValue_MissingAndNotFound;
var
  N: Integer;
begin
  LoadStandard;
  N := fSession.Working.Count;
  fSession.RemoveValue('missing', 'A');
  fSession.RemoveValue('otherTelephone', 'not found');
  fSession.RemoveValue('otherTelephone', 'a');
  CheckEqual(fSession.Revision, 0);
  CheckEqual(fSession.Working.Count, N);
  CheckRawArray(fSession.GetAllRaw('otherTelephone'), ['A', 'B', 'C'], 'No mutation');
  Check(not fSession.IsModified);
end;

procedure TUnitTestLdapEditSession.RemoveValue_FirstMiddleLastAndOnly;
begin
  LoadStandard;
  fSession.RemoveValue('otherTelephone', 'B');
  CheckRawArray(fSession.GetAllRaw('otherTelephone'), ['A', 'C'], 'Remove middle');
  fSession.RemoveValue('otherTelephone', 'A');
  CheckRawArray(fSession.GetAllRaw('otherTelephone'), ['C'], 'Remove first');
  fSession.AddValue('otherTelephone', 'D');
  fSession.RemoveValue('otherTelephone', 'D');
  CheckRawArray(fSession.GetAllRaw('otherTelephone'), ['C'], 'Remove last');
  fSession.RemoveValue('otherTelephone', 'C');
  Check(fSession.Working.Find('otherTelephone') = nil, 'Remove last remaining attribute');
  CheckRawArray(fSession.GetAllRaw('otherTelephone'), [], 'All removed');
  Check(fSession.IsModified('otherTelephone'));
  CheckEqual(fSession.Revision, 5);
  CheckRawArray(Copy(fSession.Initial.Find('otherTelephone').List, 0,
    fSession.Initial.Find('otherTelephone').Count), ['A', 'B', 'C'],
    'Initial snapshot preserved');
  fSession.Restore('otherTelephone');
  CheckRawArray(fSession.GetAllRaw('otherTelephone'), ['A', 'B', 'C'], 'Restore removed');
  Check(not fSession.IsModified);
end;

procedure TUnitTestLdapEditSession.RemoveValue_SpareCapacity;
var
  A: TLdapAttribute;
  V: TRawByteStringDynArray;
  i: Integer;
begin
  LoadEmpty;
  // Force the backing list to have a capacity greater than Count.
  A := fSession.AccessAttr('newAttribute');
  for i := 1 to 32 do
    A.Add(IntToStr(i), aoAlwaysFast);
  CheckEqual(A.Count, 32);
  Check(Length(A.List) > A.Count, 'Backing array must have spare capacity');
  fSession.RemoveValue('newAttribute', '16');
  V := fSession.GetAllRaw('newAttribute');
  if Check(Length(V) = 31, 'Use Count, not backing capacity') then
    for i := 0 to High(V) do
      if i < 15 then
        CheckEqual(V[i], IntToStr(i + 1))
      else
        CheckEqual(V[i], IntToStr(i + 2));
end;

procedure TUnitTestLdapEditSession.Binary_LocalRoundTrip;
var
  A, B, C: RawByteString;
begin
  LoadEmpty;
  A := Bytes([0, $80, $FF, 0, $41]);
  B := Bytes([0, $80, $FE, 0, $41]);
  C := Bytes([0, $80, $FF, 0, $41, 0]);
  fSession.SetValues('testBinary', RawValues([A, B, C, A]));
  CheckRawArray(fSession.GetAllRaw('testBinary'), [A, B, C], 'Binary dedup');
  CheckEqual(fSession.GetRaw('testBinary', 0), A);
  CheckEqual(fSession.GetRaw('testBinary', 1), B);
  CheckEqual(fSession.GetRaw('testBinary', 2), C);
  fSession.RemoveValue('testBinary', B);
  CheckRawArray(fSession.GetAllRaw('testBinary'), [A, C], 'Binary remove exact');
  fSession.AddValue('testBinary', B);
  CheckRawArray(fSession.GetAllRaw('testBinary'), [A, C, B], 'Binary add');
  fSession.SetValue('testBinary', C);
  CheckRawArray(fSession.GetAllRaw('testBinary'), [C], 'Binary replace');
  fSession.Restore('testBinary');
  CheckRawArray(fSession.GetAllRaw('testBinary'), [], 'Restore binary absent');
  Check(not fSession.IsModified);
end;

procedure TUnitTestLdapEditSession.Text_Utf8RoundTrip;
var
  U: RawByteString;
  R: TRawUtf8DynArray;
begin
  LoadEmpty;
  // UTF-8 bytes for cafe with acute accent, space and U+1F600.
  U := Bytes([$63, $61, $66, $C3, $A9, $20, $F0, $9F, $98, $80]);
  fSession.SetValue('description', U);
  CheckEqual(fSession.GetRaw('description'), U);
  CheckEqual(fSession.GetReadable('description'), U);
  CheckRawArray(fSession.GetAllRaw('description'), [U], 'UTF-8 raw');
  R := fSession.GetAllReadable('description');
  if Check(Length(R) = 1) then
    CheckEqual(R[0], U);
  fSession.RemoveValue('description', U);
  Check(not fSession.IsModified);
end;

procedure TUnitTestLdapEditSession.ClearAttribute_ExistingMissingAndRepeated;
var
  Rev: Int64;
begin
  LoadStandard;
  fSession.ClearAttribute('missing');
  CheckEqual(fSession.Revision, 0);
  fSession.ClearAttribute('OTHERTELEPHONE');
  CheckEqual(fSession.Revision, 1);
  Check(fSession.Working.Find('otherTelephone') = nil);
  CheckRawArray(fSession.GetAllRaw('otherTelephone'), [], 'Cleared');
  Check(fSession.IsModified('otherTelephone'));
  fSession.ClearAttribute('otherTelephone');
  CheckEqual(fSession.Revision, 1, 'Repeated clear is ignored');
  fSession.SetValues('newEmpty', nil);
  Rev := fSession.Revision;
  fSession.ClearAttribute('newEmpty');
  Check(fSession.Working.Find('newEmpty') = nil);
  // The implementation may canonicalize empty attributes at SetValues.
  Check((fSession.Revision = Rev) or (fSession.Revision = Rev + 1));
  fSession.Restore('otherTelephone');
  Check(not fSession.IsModified);
end;

procedure TUnitTestLdapEditSession.Restore_ExistingDeletedNewAndMissing;
begin
  LoadStandard;
  fSession.SetValue('description', 'Edited');
  fSession.ClearAttribute('otherTelephone');
  fSession.SetValue('newAttribute', 'new');
  fSession.Restore('description');
  CheckEqual(fSession.GetRaw('description'), 'Original');
  Check(fSession.IsModified('otherTelephone'));
  Check(fSession.IsModified('newAttribute'));
  fSession.Restore('otherTelephone');
  CheckRawArray(fSession.GetAllRaw('otherTelephone'), ['A', 'B', 'C'], 'Restore deleted');
  fSession.Restore('newAttribute');
  Check(fSession.Working.Find('newAttribute') = nil);
  fSession.Restore('neverLoaded');
  Check(fSession.Working.Find('neverLoaded') = nil);
  Check(not fSession.IsModified('neverLoaded'));
  Check(not fSession.IsModified);
  // A second edit and restore verifies that Assign did not alias the initial array.
  fSession.RemoveValue('otherTelephone', 'B');
  fSession.Restore('otherTelephone');
  CheckRawArray(fSession.GetAllRaw('otherTelephone'), ['A', 'B', 'C'], 'Restore twice');
  Check(not fSession.IsModified);
end;

procedure TUnitTestLdapEditSession.AttributeEqual_AllBranches;
var
  Lists: TLdapAttributeList;
  A, B: TLdapAttribute;
begin
  // A and B are standalone attributes owned by Lists; nil is also a valid input.
  Lists := TLdapAttributeList.Create;
  try
    A := Lists.Add('a');
    B := Lists.Add('b');
    Check(fSession.EqualAttr(nil, nil), 'nil / nil');
    Check(fSession.EqualAttr(nil, B), 'nil / empty');
    Check(fSession.EqualAttr(A, nil), 'empty / nil');
    Check(fSession.EqualAttr(A, B), 'empty / empty');
    A.Add('X');
    Check(not fSession.EqualAttr(A, nil), 'nonempty / nil');
    Check(not fSession.EqualAttr(nil, A), 'nil / nonempty');
    Check(not fSession.EqualAttr(A, B), 'nonempty / empty');
    Check(not fSession.EqualAttr(B, A), 'empty / nonempty');
    B.Add('X');
    Check(fSession.EqualAttr(A, B), 'same single value');
    B.Add('Y');
    Check(not fSession.EqualAttr(A, B), 'different counts');
    Check(not fSession.EqualAttr(B, A), 'different counts reversed');
    A.Add('Z');
    Check(not fSession.EqualAttr(A, B), 'same count, different value');
    A.Clear;
    A.Add('Y');
    A.Add('X');
    Check(fSession.EqualAttr(A, B), 'same values, reversed order');
    Check(fSession.EqualAttr(B, A), 'symmetric for unique values');
    Check(fSession.EqualAttr(A, A), 'reflexive');
    A.Clear;
    B.Clear;
    A.Add('');
    Check(not fSession.EqualAttr(A, B), 'one empty value is not zero values');
    B.Add('');
    Check(fSession.EqualAttr(A, B), 'same zero-length value');
    A.Clear;
    B.Clear;
    A.Add(Bytes([0, $80, $FF]));
    B.Add(Bytes([0, $80, $FE]));
    Check(not fSession.EqualAttr(A, B), 'different binary bytes after NUL');
    B.Clear;
    B.Add(Bytes([0, $80, $FF]));
    Check(fSession.EqualAttr(A, B), 'identical binary values');
  finally
    Lists.Free;
  end;
end;

procedure TUnitTestLdapEditSession.IsModified_LocalBranches;
begin
  LoadStandard;
  Check(not fSession.IsModified('description'));
  Check(not fSession.IsModified('otherTelephone'));
  Check(not fSession.IsModified('neverLoaded'), 'Both absent');
  Check(not fSession.IsModified(''), 'No attribute named empty string');
  Check(not fSession.IsModified);
  fSession.SetValue('description', 'Edited');
  Check(fSession.IsModified('DESCRIPTION'));
  Check(fSession.IsModified, 'Initial loop true');
  Check(not fSession.IsModified('otherTelephone'));
  fSession.Restore('description');
  Check(not fSession.IsModified);
  fSession.AddValue('newAttribute', 'new');
  Check(fSession.IsModified('newAttribute'));
  Check(fSession.IsModified, 'Working loop true (new attribute only)');
  fSession.ClearAttribute('newAttribute');
  Check(not fSession.IsModified);
  fSession.ClearAttribute('description');
  Check(fSession.IsModified('description'));
  Check(fSession.IsModified, 'Initial loop true (deleted attribute)');
  fSession.RestoreAll;
  Check(not fSession.IsModified);
end;

procedure TUnitTestLdapEditSession.IsModified_EmptyInitialAttribute;
var
  Source: TLdapAttributeList;
begin
  Source := TLdapAttributeList.Create;
  try
    Source.Add('emptyAttribute');
    fSession.Load(USER_DN, Source);
  finally
    Source.Free;
  end;
  Check(not fSession.IsModified('emptyAttribute'), 'Empty cloned attribute is clean');
  Check(not fSession.IsModified, 'Empty attribute must not mark loaded session dirty');
  fSession.ClearAttribute('emptyAttribute');
  Check(not fSession.IsModified('emptyAttribute'), 'Zero values and absent equivalent');
  Check(not fSession.IsModified);
  fSession.SetValue('emptyAttribute', '');
  Check(fSession.IsModified('emptyAttribute'), 'One zero-length value differs');
  Check(fSession.IsModified);
  fSession.Restore('emptyAttribute');
  CheckRawArray(fSession.GetAllRaw('emptyAttribute'), [], 'Restore initially empty');
  Check(not fSession.IsModified('emptyAttribute'));
  Check(not fSession.IsModified);
end;

procedure TUnitTestLdapEditSession.External_IndexKeyAndBinary;
var
  A, B: RawByteString;
begin
  LoadEmpty;
  A := Bytes([0, $80, $FF]);
  B := Bytes([0, $80, $FE]);
  CheckEqual(fSession.StateIndex(GROUP_A, 'member', A), -1);
  fSession.LoadValueState(GROUP_A, 'member', A);
  fSession.LoadValueState(GROUP_B, 'member', A);
  fSession.LoadValueState(GROUP_A, 'otherAttribute', A);
  fSession.LoadValueState(GROUP_A, 'member', B);
  CheckEqual(fSession.StateCount, 4);
  CheckEqual(fSession.StateIndex(GROUP_A, 'member', A), 0);
  CheckEqual(fSession.StateIndex(GROUP_B, 'member', A), 1);
  CheckEqual(fSession.StateIndex(GROUP_A, 'otherAttribute', A), 2);
  CheckEqual(fSession.StateIndex(GROUP_A, 'member', B), 3);
  CheckEqual(fSession.StateIndex(GROUP_C, 'member', A), -1, 'Wrong DN');
  CheckEqual(fSession.StateIndex(GROUP_A, 'wrong', A), -1, 'Wrong attribute');
  CheckEqual(fSession.StateIndex(GROUP_A, 'member', A + #0), -1, 'Wrong length');
  CheckEqual(fSession.StateIndex(GROUP_A, 'member', ''), -1, 'Wrong value');
  fSession.LoadValueState(GROUP_A, 'member', '');
  CheckEqual(fSession.StateIndex(GROUP_A, 'member', ''), 4);
  CheckState(GROUP_A, 'member', A, lpPresent, lpPresent);
  CheckState(GROUP_A, 'member', B, lpPresent, lpPresent);
  Check(not fSession.IsModified);
end;

procedure TUnitTestLdapEditSession.External_LoadDefaultAbsentUnknownAndRepeated;
var
  Rev: Int64;
begin
  LoadEmpty;
  fSession.LoadValueState(GROUP_A, 'member', USER_DN, lpUnknown);
  CheckEqual(fSession.StateCount, 0, 'Unknown initial load ignored');
  CheckEqual(fSession.Revision, 0);
  fSession.LoadValueState(GROUP_A, 'member', USER_DN);
  CheckState(GROUP_A, 'member', USER_DN, lpPresent, lpPresent);
  fSession.LoadValueState(GROUP_B, 'member', USER_DN, lpAbsent);
  CheckState(GROUP_B, 'member', USER_DN, lpAbsent, lpAbsent);
  CheckEqual(fSession.StateCount, 2);
  CheckEqual(fSession.Revision, 2);
  Check(not fSession.IsModified);
  fSession.LoadValueState(GROUP_A, 'member', USER_DN, lpAbsent);
  fSession.LoadValueState(GROUP_B, 'member', USER_DN, lpPresent);
  fSession.LoadValueState(GROUP_A, 'member', USER_DN, lpUnknown);
  CheckEqual(fSession.StateCount, 2);
  CheckEqual(fSession.Revision, 2, 'Known initial must not be overwritten');
  CheckState(GROUP_A, 'member', USER_DN, lpPresent, lpPresent);
  CheckState(GROUP_B, 'member', USER_DN, lpAbsent, lpAbsent);
  fSession.SetValueState(GROUP_A, 'member', USER_DN, lpAbsent);
  fSession.SetValueState(GROUP_B, 'member', USER_DN, lpPresent);
  Rev := fSession.Revision;
  fSession.LoadValueState(GROUP_A, 'member', USER_DN, lpPresent);
  fSession.LoadValueState(GROUP_B, 'member', USER_DN, lpAbsent);
  CheckState(GROUP_A, 'member', USER_DN, lpPresent, lpAbsent);
  CheckState(GROUP_B, 'member', USER_DN, lpAbsent, lpPresent);
  CheckEqual(fSession.Revision, Rev);
  Check(fSession.IsModified);
end;

procedure TUnitTestLdapEditSession.External_AllPresenceTransitions;
var
  Initial, Previous, Next: TLdapPresence;
  Rev: Int64;
  Context: RawUtf8;
begin
  // Exhaustive Cartesian product of enum values; also tests all transitions
  // from each previous working state to each next working state (27 cases).
  // Known -> unknown is currently allowed by SetValueState: characterize it,
  // not a recommendation to emit LDAP operations for that state.
  for Initial := Low(TLdapPresence) to High(TLdapPresence) do
    for Previous := Low(TLdapPresence) to High(TLdapPresence) do
      for Next := Low(TLdapPresence) to High(TLdapPresence) do
      begin
        ResetSession;
        LoadEmpty;
        if Initial = lpUnknown then
          fSession.SetValueState(GROUP_A, 'member', USER_DN, Previous)
        else
        begin
          fSession.LoadValueState(GROUP_A, 'member', USER_DN, Initial);
          fSession.SetValueState(GROUP_A, 'member', USER_DN, Previous);
        end;
        Context := FormatUtf8('initial=% previous=% next=%',
          [Ord(Initial), Ord(Previous), Ord(Next)]);
        CheckState(GROUP_A, 'member', USER_DN, Initial, Previous, Context);
        Check(fSession.IsModified = (Initial <> Previous), String(Context));
        Rev := fSession.Revision;
        fSession.SetValueState(GROUP_A, 'member', USER_DN, Next);
        CheckEqual(fSession.StateCount, 1, Context);
        CheckEqual(fSession.Revision, Rev + 1, Context);
        CheckState(GROUP_A, 'member', USER_DN, Initial, Next, Context);
        Check(fSession.IsModified = (Initial <> Next), String(Context));
        CheckEqual(Length(fSession.GetValuesState(GROUP_A, 'member', lsInitial)),
          Ord(Initial = lpPresent), Context + ' forward initial');
        CheckEqual(Length(fSession.GetValuesState(GROUP_A, 'member', lsWorking)),
          Ord(Next = lpPresent), Context + ' forward working');
        CheckEqual(Length(fSession.GetValuesStateBackward('member', USER_DN, lsInitial)),
          Ord(Initial = lpPresent), Context + ' backward initial');
        CheckEqual(Length(fSession.GetValuesStateBackward('member', USER_DN, lsWorking)),
          Ord(Next = lpPresent), Context + ' backward working');
        fSession.RestoreValueState(GROUP_A, 'member', USER_DN);
        CheckState(GROUP_A, 'member', USER_DN, Initial, Initial, Context);
        Check(not fSession.IsModified, String(Context + ' restored'));
      end;
end;

procedure TUnitTestLdapEditSession.External_ResolveUnknownMatrix;
var
  Previous, Resolved, Expected: TLdapPresence;
  Rev: Int64;
begin
  // All 3 working states x both known initial states. Unknown Load is a no-op.
  for Previous := Low(TLdapPresence) to High(TLdapPresence) do
    for Resolved := lpAbsent to lpPresent do
    begin
      ResetSession;
      LoadEmpty;
      fSession.SetValueState(GROUP_A, 'member', USER_DN, Previous);
      CheckState(GROUP_A, 'member', USER_DN, lpUnknown, Previous);
      Rev := fSession.Revision;
      fSession.LoadValueState(GROUP_A, 'member', USER_DN, lpUnknown);
      CheckEqual(fSession.Revision, Rev);
      CheckState(GROUP_A, 'member', USER_DN, lpUnknown, Previous);
      fSession.LoadValueState(GROUP_A, 'member', USER_DN, Resolved);
      if Previous = lpUnknown then
        Expected := Resolved
      else
        Expected := Previous;
      CheckState(GROUP_A, 'member', USER_DN, Resolved, Expected);
      CheckEqual(fSession.StateCount, 1);
      CheckEqual(fSession.Revision, Rev + 1);
      Check(fSession.IsModified = (Resolved <> Expected));
      fSession.LoadValueState(GROUP_A, 'member', USER_DN, Resolved);
      CheckEqual(fSession.Revision, Rev + 1, 'Already resolved, no-op');
      CheckState(GROUP_A, 'member', USER_DN, Resolved, Expected);
    end;
end;

procedure TUnitTestLdapEditSession.External_SetNewAllPresences;
var
  P: TLdapPresence;
  Value: RawByteString;
begin
  LoadEmpty;
  for P := Low(TLdapPresence) to High(TLdapPresence) do
  begin
    Value := IntToStr(Ord(P));
    fSession.SetValueState(GROUP_A, 'member', Value, P);
    CheckState(GROUP_A, 'member', Value, lpUnknown, P);
    CheckEqual(fSession.StateCount, Ord(P) + 1);
    CheckEqual(fSession.Revision, Ord(P) + 1);
  end;
  CheckRawSet(fSession.GetValuesState(GROUP_A, 'member'), ['2'], 'Only present');
  CheckRawSet(fSession.GetValuesState(GROUP_A, 'member', lsInitial), [], 'All unknown initially');
  Check(fSession.IsModified);
end;

procedure TUnitTestLdapEditSession.External_ForwardProjection;
var
  V: TRawByteStringDynArray;
  Rev: Int64;
begin
  LoadEmpty;
  CheckRawSet(fSession.GetValuesState(GROUP_A, 'member'), [], 'Empty collection');
  fSession.LoadValueState(GROUP_A, 'member', 'Keep');
  fSession.LoadValueState(GROUP_A, 'member', 'Remove');
  fSession.SetValueState(GROUP_A, 'member', 'Remove', lpAbsent);
  fSession.LoadValueState(GROUP_A, 'member', 'Add', lpAbsent);
  fSession.SetValueState(GROUP_A, 'member', 'Add', lpPresent);
  fSession.LoadValueState(GROUP_A, 'member', 'Absent', lpAbsent);
  fSession.SetValueState(GROUP_A, 'member', 'Unknown', lpUnknown);
  fSession.SetValueState(GROUP_A, 'member', 'UnresolvedAdd', lpPresent);
  fSession.LoadValueState(GROUP_B, 'member', 'Other DN');
  fSession.LoadValueState(GROUP_A, 'otherAttribute', 'Other attribute');
  Rev := fSession.Revision;
  CheckRawSet(fSession.GetValuesState(GROUP_A, 'member', lsInitial),
    ['Keep', 'Remove'], 'Initial snapshot');
  CheckRawSet(fSession.GetValuesState(GROUP_A, 'member', lsWorking),
    ['Keep', 'Add', 'UnresolvedAdd'], 'Working snapshot');
  CheckRawSet(fSession.GetValuesState(GROUP_A, 'member'),
    ['Keep', 'Add', 'UnresolvedAdd'], 'Default working');
  CheckRawSet(fSession.GetValuesState(GROUP_B, 'member'), ['Other DN'], 'DN filter');
  CheckRawSet(fSession.GetValuesState(GROUP_A, 'otherAttribute'),
    ['Other attribute'], 'Attribute filter');
  CheckRawSet(fSession.GetValuesState(GROUP_C, 'member'), [], 'Unknown DN');
  CheckRawSet(fSession.GetValuesState(GROUP_A, 'missing'), [], 'Unknown attribute');
  V := fSession.GetValuesState(GROUP_A, 'member');
  if Check(Length(V) = 3) then
    V[0] := 'Tampered';
  CheckRawSet(fSession.GetValuesState(GROUP_A, 'member'),
    ['Keep', 'Add', 'UnresolvedAdd'], 'Result isolated');
  CheckEqual(fSession.Revision, Rev, 'Projection is read-only');
end;

procedure TUnitTestLdapEditSession.External_BackwardProjection;
var
  V: TRawUtf8DynArray;
  Rev: Int64;
begin
  LoadEmpty;
  CheckDNSet(fSession.GetValuesStateBackward('member', USER_DN), [], 'Empty');
  fSession.LoadValueState(GROUP_A, 'member', USER_DN);
  fSession.LoadValueState(GROUP_B, 'member', USER_DN);
  fSession.SetValueState(GROUP_B, 'member', USER_DN, lpAbsent);
  fSession.LoadValueState(GROUP_C, 'member', USER_DN, lpAbsent);
  fSession.SetValueState(GROUP_C, 'member', USER_DN, lpPresent);
  fSession.LoadValueState(GROUP_D, 'member', USER_DN, lpAbsent);
  fSession.SetValueState(GROUP_E, 'member', USER_DN, lpUnknown);
  fSession.SetValueState(GROUP_F, 'member', USER_DN, lpPresent);
  fSession.LoadValueState(GROUP_D, 'otherAttribute', USER_DN);
  fSession.LoadValueState(GROUP_E, 'member', OTHER_USER);
  Rev := fSession.Revision;
  CheckDNSet(fSession.GetValuesStateBackward('member', USER_DN, lsInitial),
    [GROUP_A, GROUP_B], 'Initial membership');
  CheckDNSet(fSession.GetValuesStateBackward('member', USER_DN, lsWorking),
    [GROUP_A, GROUP_C, GROUP_F], 'Working membership');
  CheckDNSet(fSession.GetValuesStateBackward('member', USER_DN),
    [GROUP_A, GROUP_C, GROUP_F], 'Default working');
  CheckDNSet(fSession.GetValuesStateBackward('otherAttribute', USER_DN),
    [GROUP_D], 'Attribute filter');
  CheckDNSet(fSession.GetValuesStateBackward('member', OTHER_USER),
    [GROUP_E], 'Value filter');
  CheckDNSet(fSession.GetValuesStateBackward('missing', USER_DN), [], 'Unknown attribute');
  CheckDNSet(fSession.GetValuesStateBackward('member', 'missing'), [], 'Unknown value');
  V := fSession.GetValuesStateBackward('member', USER_DN);
  if Check(Length(V) = 3) then
    V[0] := 'Tampered';
  CheckDNSet(fSession.GetValuesStateBackward('member', USER_DN),
    [GROUP_A, GROUP_C, GROUP_F], 'Result isolated');
  CheckEqual(fSession.Revision, Rev);
end;

procedure TUnitTestLdapEditSession.External_BinaryProjectionAndRestore;
var
  A, B: RawByteString;
begin
  LoadEmpty;
  A := Bytes([0, $80, $FF, $00]);
  B := Bytes([0, $80, $FE, $00]);
  fSession.LoadValueState(GROUP_A, 'cACertificate', A);
  fSession.LoadValueState(GROUP_A, 'cACertificate', B, lpAbsent);
  fSession.LoadValueState(GROUP_A, 'cACertificate', '');
  fSession.SetValueState(GROUP_A, 'cACertificate', A, lpAbsent);
  fSession.SetValueState(GROUP_A, 'cACertificate', B, lpPresent);
  CheckRawSet(fSession.GetValuesState(GROUP_A, 'cACertificate', lsInitial), [A, ''],
    'Initial binary values');
  CheckRawSet(fSession.GetValuesState(GROUP_A, 'cACertificate'), [B, ''], 'Working binary');
  CheckDNSet(fSession.GetValuesStateBackward('cACertificate', A, lsInitial), [GROUP_A],
    'Binary backward initial');
  CheckDNSet(fSession.GetValuesStateBackward('cACertificate', A), [], 'Binary removed');
  CheckDNSet(fSession.GetValuesStateBackward('cACertificate', B), [GROUP_A], 'Binary added');
  fSession.RestoreValueState(GROUP_A, 'cACertificate', A);
  CheckState(GROUP_A, 'cACertificate', A, lpPresent, lpPresent);
  CheckState(GROUP_A, 'cACertificate', B, lpAbsent, lpPresent);
  fSession.RestoreValueState(GROUP_A, 'cACertificate', B);
  CheckRawSet(fSession.GetValuesState(GROUP_A, 'cACertificate'), [A, ''], 'Binary restored');
  Check(not fSession.IsModified);
end;

procedure TUnitTestLdapEditSession.External_RestoreMissingAndCancel;
var
  Rev: Int64;
begin
  LoadStandard;
  fSession.RestoreValueState(GROUP_A, 'member', USER_DN);
  CheckEqual(fSession.StateCount, 0);
  CheckEqual(fSession.Revision, 0);
  fSession.LoadValueState(GROUP_A, 'member', USER_DN);
  fSession.LoadValueState(GROUP_B, 'member', USER_DN, lpAbsent);
  fSession.SetValueState(GROUP_A, 'member', USER_DN, lpAbsent);
  fSession.SetValueState(GROUP_B, 'member', USER_DN, lpPresent);
  Check(fSession.IsModified);
  Rev := fSession.Revision;
  fSession.RestoreValueState(GROUP_C, 'member', USER_DN);
  fSession.RestoreValueState(GROUP_A, 'wrong', USER_DN);
  fSession.RestoreValueState(GROUP_A, 'member', OTHER_USER);
  CheckEqual(fSession.Revision, Rev);
  CheckEqual(fSession.StateCount, 2);
  fSession.RestoreValueState(GROUP_A, 'member', USER_DN);
  CheckState(GROUP_A, 'member', USER_DN, lpPresent, lpPresent);
  CheckState(GROUP_B, 'member', USER_DN, lpAbsent, lpPresent);
  Check(fSession.IsModified);
  fSession.RestoreValueState(GROUP_B, 'member', USER_DN);
  Check(not fSession.IsModified);
  fSession.SetValueState(GROUP_B, 'member', USER_DN, lpPresent);
  fSession.SetValueState(GROUP_B, 'member', USER_DN, lpAbsent);
  Check(not fSession.IsModified, 'Add then delete');
  fSession.SetValueState(GROUP_A, 'member', USER_DN, lpAbsent);
  fSession.SetValueState(GROUP_A, 'member', USER_DN, lpPresent);
  Check(not fSession.IsModified, 'Delete then add');
  CheckEqual(fSession.StateCount, 2, 'No duplicate state records');
end;

procedure TUnitTestLdapEditSession.IsModified_ExternalOnlyAndLocalScope;
begin
  LoadStandard;
  fSession.LoadValueState(GROUP_A, 'member', USER_DN);
  fSession.LoadValueState(GROUP_B, 'member', USER_DN, lpAbsent);
  Check(not fSession.IsModified, 'Unchanged external records');
  fSession.SetValueState(GROUP_B, 'member', USER_DN, lpPresent);
  Check(fSession.IsModified, 'External loop true after first unchanged record');
  Check(not fSession.IsModified('description'), 'Named query only checks local attributes');
  Check(not fSession.IsModified('member'), 'External attr name not a local modification');
  fSession.RestoreValueState(GROUP_B, 'member', USER_DN);
  Check(not fSession.IsModified);
  fSession.SetValueState(GROUP_C, 'member', USER_DN, lpPresent);
  Check(fSession.IsModified, 'Unknown initial with explicit intention');
  fSession.RestoreValueState(GROUP_C, 'member', USER_DN);
  Check(not fSession.IsModified, 'Unknown/unknown is clean');
end;

procedure TUnitTestLdapEditSession.RestoreAll_NoExternalAndMixed;
var
  Rev: Int64;
begin
  LoadStandard;
  fSession.SetValue('description', 'Edited');
  Rev := fSession.Revision;
  fSession.RestoreAll;
  CheckEqual(fSession.Revision, Rev + 1);
  CheckEqual(fSession.GetRaw('description'), 'Original');
  Check(not fSession.IsModified);
  fSession.SetValue('description', 'Edited again');
  fSession.ClearAttribute('otherTelephone');
  fSession.AddValue('newAttribute', 'new');
  fSession.LoadValueState(GROUP_A, 'member', USER_DN);
  fSession.LoadValueState(GROUP_B, 'member', USER_DN, lpAbsent);
  fSession.SetValueState(GROUP_A, 'member', USER_DN, lpAbsent);
  fSession.SetValueState(GROUP_B, 'member', USER_DN, lpPresent);
  fSession.SetValueState(GROUP_C, 'member', USER_DN, lpPresent);
  Rev := fSession.Revision;
  fSession.RestoreAll;
  CheckEqual(fSession.Revision, Rev + 1);
  CheckEqual(fSession.GetRaw('description'), 'Original');
  CheckRawArray(fSession.GetAllRaw('otherTelephone'), ['A', 'B', 'C'], 'Restore all local');
  Check(fSession.Working.Find('newAttribute') = nil);
  CheckEqual(fSession.StateCount, 3);
  CheckState(GROUP_A, 'member', USER_DN, lpPresent, lpPresent);
  CheckState(GROUP_B, 'member', USER_DN, lpAbsent, lpAbsent);
  CheckState(GROUP_C, 'member', USER_DN, lpUnknown, lpUnknown);
  CheckDNSet(fSession.GetValuesStateBackward('member', USER_DN), [GROUP_A], 'Restore external');
  Check(not fSession.IsModified);
  fSession.RemoveValue('otherTelephone', 'B');
  fSession.RestoreAll;
  CheckRawArray(fSession.GetAllRaw('otherTelephone'), ['A', 'B', 'C'], 'Restore after re-edit');
  Check(not fSession.IsModified);
end;

procedure TUnitTestLdapEditSession.InvalidLocalName_MutatorsReject;
var
  i: Integer;
  Raised: Boolean;
  Rev: Int64;
  N: Integer;
begin
  LoadStandard;
  Rev := fSession.Revision;
  N := fSession.Working.Count;
  for i := 0 to 2 do
  begin
    Raised := False;
    try
      case i of
        0: fSession.SetValue('', 'X');
        1: fSession.SetValues('', RawValues(['X']));
        2: fSession.AddValue('', 'X');
      end;
    except
      on E: ELdap do
        Raised := True;
    end;
    CheckUtf8(Raised, 'Empty name rejected by mutator %', [i]);
    CheckEqual(fSession.Working.Count, N);
    CheckEqual(fSession.Revision, Rev);
  end;
  fSession.RemoveValue('', 'X');
  fSession.ClearAttribute('');
  CheckEqual(fSession.Revision, Rev, 'Missing empty-name removal/clear');
  Check(not fSession.IsModified);
end;

procedure TUnitTestLdapEditSession.External_KeyRepresentation_CurrentPolicy;
begin
  // Characterization only: the current external keys use exact strings.
  // LDAP DN and attribute-description equivalence require a separate policy.
  LoadEmpty;
  fSession.LoadValueState(GROUP_A, 'member', 'Value');
  CheckEqual(fSession.StateIndex(GROUP_A, 'MEMBER', 'Value'), -1);
  CheckEqual(fSession.StateIndex(LowerCase(GROUP_A), 'member', 'Value'), -1);
  CheckEqual(fSession.StateIndex(GROUP_A, 'member', 'value'), -1);
  fSession.LoadValueState(GROUP_A, 'MEMBER', 'Value');
  fSession.LoadValueState(GROUP_A, 'member', 'value');
  CheckEqual(fSession.StateCount, 3);
  CheckRawSet(fSession.GetValuesState(GROUP_A, 'member'), ['Value', 'value'],
    'Exact attribute and value representation');
  CheckRawSet(fSession.GetValuesState(GROUP_A, 'MEMBER'), ['Value'],
    'Separate attribute spelling');
  CheckDNSet(fSession.GetValuesStateBackward('member', 'Value'), [GROUP_A],
    'Exact value representation');
  Check(not fSession.IsModified);
end;

procedure TUnitTestLdapEditSession.RestoreLocal_PreservesExternal;
begin
  LoadStandard;
  fSession.LoadValueState(GROUP_A, 'member', USER_DN);
  fSession.SetValueState(GROUP_A, 'member', USER_DN, lpAbsent);
  fSession.SetValue('description', 'Edited');
  fSession.Restore('description');
  Check(not fSession.IsModified('description'));
  CheckState(GROUP_A, 'member', USER_DN, lpPresent, lpAbsent);
  Check(fSession.IsModified, 'Local restore does not clear external intent');
  fSession.SetValue('description', 'Edited again');
  fSession.RestoreValueState(GROUP_A, 'member', USER_DN);
  CheckState(GROUP_A, 'member', USER_DN, lpPresent, lpPresent);
  CheckEqual(fSession.GetRaw('description'), 'Edited again');
  Check(fSession.IsModified, 'External restore does not clear local intent');
  fSession.Restore('description');
  Check(not fSession.IsModified);
end;

procedure TUnitTestLdapEditSession.Revision_CharacterizesCurrentPolicy;
var
  R: Int64;
begin
  // The supplied code increments on successful calls, even if they are no-ops.
  // This test intentionally characterizes that policy, not semantic revisions.
  LoadStandard;
  R := 0;
  CheckEqual(fSession.Revision, R);
  fSession.SetValue('description', 'Original'); Inc(R);
  CheckEqual(fSession.Revision, R, 'SetValue unchanged');
  fSession.SetValues('otherTelephone', RawValues(['A', 'B', 'C'])); Inc(R);
  CheckEqual(fSession.Revision, R, 'SetValues unchanged');
  fSession.AddValue('otherTelephone', 'A'); Inc(R);
  CheckEqual(fSession.Revision, R, 'Duplicate AddValue');
  fSession.RemoveValue('otherTelephone', 'missing');
  fSession.RemoveValue('missing', 'A');
  CheckEqual(fSession.Revision, R, 'Ignored removals');
  fSession.ClearAttribute('missing');
  CheckEqual(fSession.Revision, R, 'Ignored clear');
  fSession.Restore('description'); Inc(R);
  CheckEqual(fSession.Revision, R, 'Restore unchanged');
  fSession.Restore('missing'); Inc(R);
  CheckEqual(fSession.Revision, R, 'Restore absent');
  fSession.RestoreAll; Inc(R);
  CheckEqual(fSession.Revision, R, 'RestoreAll unchanged');
  fSession.LoadValueState(GROUP_A, 'member', USER_DN, lpUnknown);
  CheckEqual(fSession.Revision, R, 'Unknown load ignored');
  fSession.LoadValueState(GROUP_A, 'member', USER_DN); Inc(R);
  CheckEqual(fSession.Revision, R, 'First load');
  fSession.LoadValueState(GROUP_A, 'member', USER_DN);
  CheckEqual(fSession.Revision, R, 'Repeated load ignored');
  fSession.SetValueState(GROUP_A, 'member', USER_DN, lpPresent); Inc(R);
  CheckEqual(fSession.Revision, R, 'SetValueState unchanged');
  fSession.RestoreValueState(GROUP_A, 'member', USER_DN); Inc(R);
  CheckEqual(fSession.Revision, R, 'RestoreValueState unchanged');
  fSession.RestoreValueState(GROUP_B, 'member', USER_DN);
  CheckEqual(fSession.Revision, R, 'Unknown restore ignored');
  Check(not fSession.IsModified, 'Revision not the dirty flag');
  fSession.SetValue('description', 'Edited'); Inc(R);
  CheckEqual(fSession.Revision, R);
  Check(fSession.IsModified);
  fSession.SetValue('description', 'Original'); Inc(R);
  CheckEqual(fSession.Revision, R);
  Check(not fSession.IsModified);
end;

procedure TUnitTestLdapEditSession.Read_NegativeReadableIndex;
begin
  // Enable only after adding a negative-index guard, or in an isolated
  // process with range checking. The supplied mORMot GetReadable may read
  // outside the array for negative indexes on a nonempty attribute.
  LoadStandard;
  CheckEqual(fSession.GetReadable('description', -1), '');
  CheckEqual(fSession.GetReadable('description', Low(Integer)), '');
end;

end.

