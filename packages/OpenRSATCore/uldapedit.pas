unit uldapedit;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.base,
  mormot.core.text,
  mormot.net.ldap;

type

  TLdapPresence = (
    lpUnknown,
    lpAbsent,
    lpPresent
  );

  TLdapSnapshot = (
    lsInitial,
    lsWorking
  );

  TLdapValueState = record
    DN: RawUtf8;
    AttributeName: RawUtf8;
    Value: RawByteString;

    InitialPresence: TLdapPresence;
    WorkingPresence: TLdapPresence;
  end;
  TLdapValueStateDynArray = Array of TLdapValueState;

  // Représente un changement LDAP
  TLdapChange = record
    DN: RawUtf8;
    AttributeName: RawUtf8;
    Operation: TLdapModifyOp;
    Values: TRawByteStringDynArray;
  end;
  // Représente un ensemble de changement LDAP
  TLdapChangeDynArray = Array of TLdapChange;

  // Représente l'état d'application des changements. Permet de mettre à jour l'état initial à partir des modifications effectué
  // Gère les erreurs pour ne pas les comptabilisé comme modifié
  TLdapChangeApplier = class

  end;

//function ComputeAttributeDiff(const DN: RawUtf8; const Initial, Working: TLdapAttributeList): TLdapChangeDynArray;

  // Prends en charge les modifications entre un état initial et courant.

  { TLdapEditSession }

  TLdapEditSession = class
  protected
    fDN: RawUtf8;
    fInitial: TLdapAttributeList;
    fWorking: TLdapAttributeList;

    fExternalValueStates: TLdapValueStateDynArray;

    fRevision: Int64;

    function GetAttr(const Name: RawUtf8): TLdapAttribute;
    function GetValueStateIndex(const DN, Name: RawUtf8; const Value: RawByteString): Integer;
    function AttributeEqual(A, B: TLdapAttribute): Boolean;
    function GetStatePresence(const State: TLdapValueState; Snapshot: TLdapSnapshot): TLdapPresence;
    procedure BuildLocalChanges(var Changes: TLdapChangeDynArray);
    procedure BuildExternalChanges(var Changes: TLdapChangeDynArray);
    procedure AppendChange(var Changes: TLdapChangeDynArray; const DN, AttributeName: RawUtf8; Op: TLdapModifyOp; const Value: RawByteString);
    function GetChangeIndex(var Changes: TLdapChangeDynArray; const DN, AttributeName: RawUtf8; Op: TLdapModifyOp): Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Load(const DN: RawUtf8; Attributes: TLdapAttributeList);

    function GetRaw(const Name: RawUtf8; Index: Integer = 0): RawByteString;
    function GetReadable(const Name: RawUtf8; index: Integer = 0): RawUtf8;
    function GetAllRaw(const Name: RawUtf8): TRawByteStringDynArray;
    function GetAllReadable(const Name: RawUtf8): TRawUtf8DynArray;

    procedure SetValue(const Name: RawUtf8; const Value: RawByteString);
    procedure SetValues(const Name: RawUtf8; const Values: TRawByteStringDynArray);
    procedure AddValue(const Name: RawUtf8; const Value: RawByteString);
    procedure RemoveValue(const Name: RawUtf8; const Value: RawByteString);
    procedure ClearAttribute(const Name: RawUtf8);
    procedure Restore(const Name: RawUtf8);
    procedure RestoreAll;

    // Set initial value presence to lpPresent
    procedure LoadValueState(const DN, Name: RawUtf8; const Value: RawByteString; Presence: TLdapPresence = lpPresent);
    // Set working value precence
    procedure SetValueState(const DN, Name: RawUtf8; const Value: RawByteString; Presence: TLdapPresence);
    function GetValuesState(const DN, Name: RawUtf8; Snapshot: TLdapSnapshot = lsWorking
      ): TRawByteStringDynArray;
    function GetValuesStateBackward(const Name: RawUtf8;
      const Value: RawByteString; Snapshot: TLdapSnapshot = lsWorking): TRawUtf8DynArray;
    procedure RestoreValueState(const DN, Name: RawUtf8; const Value: rawbytestring);

    function IsModified: Boolean; overload;
    function IsModified(const Name: RawUtf8): Boolean; overload;

    function BuildChangeSet: TLdapChangeDynArray;

    property Revision: Int64 read fRevision;
  end;

implementation

{ TLdapEditSession }

function TLdapEditSession.GetAttr(const Name: RawUtf8): TLdapAttribute;
begin
  result := fWorking.Find(Name);
  if not Assigned(result) then
    result := fWorking.Add(Name);
end;

function TLdapEditSession.GetValueStateIndex(const DN, Name: RawUtf8;
  const Value: RawByteString): Integer;
var
  i: Integer;
begin
  result := -1;

  for i := 0 to Length(fExternalValueStates) - 1 do
    if (fExternalValueStates[i].DN = DN) and (fExternalValueStates[i].AttributeName = Name) and (fExternalValueStates[i].Value = Value) then
    begin
      result := i;
      Exit;
    end;
end;

function TLdapEditSession.AttributeEqual(A, B: TLdapAttribute): Boolean;
var
  CountA, CountB, i: Integer;
begin
  result := False;

  CountA := 0;
  CountB := 0;

  if Assigned(A) then
    CountA := A.Count;
  if Assigned(B) then
    CountB := B.Count;

  if CountA <> CountB then
    Exit;

  for i := 0 to CountA - 1 do
    if B.FindIndex(A.GetRaw(i)) < 0 then
      Exit;

  result := True;
end;

function TLdapEditSession.GetStatePresence(const State: TLdapValueState;
  Snapshot: TLdapSnapshot): TLdapPresence;
begin
  case Snapshot of
    lsInitial: result := State.InitialPresence;
    lsWorking: result := State.WorkingPresence;
  end;
end;

procedure TLdapEditSession.BuildLocalChanges(var Changes: TLdapChangeDynArray);
var
  i: Integer;
  WorkAttr: TLdapAttribute;
  AttributeName: RawUtf8;
begin
  for i := 0 to fInitial.Count - 1 do
  begin

  end;
end;

procedure TLdapEditSession.BuildExternalChanges(var Changes: TLdapChangeDynArray
  );
var
  i: Integer;
begin
  for i := 0 to High(fExternalValueStates) do
  begin
    if fExternalValueStates[i].InitialPresence = fExternalValueStates[i].WorkingPresence then
      Continue;

    if (fExternalValueStates[i].InitialPresence = lpUnknown) or
       (fExternalValueStates[i].WorkingPresence = lpUnknown) then
      raise Exception.Create('Unresolved value presence');

    case fExternalValueStates[i].WorkingPresence of
      lpPresent: AppendChange(Changes,
        fExternalValueStates[i].DN,
        fExternalValueStates[i].AttributeName,
        lmoAdd,
        fExternalValueStates[i].Value);
      lpAbsent: AppendChange(Changes,
        fExternalValueStates[i].DN,
        fExternalValueStates[i].AttributeName,
        lmoDelete,
        fExternalValueStates[i].Value);
    end;
  end;
end;

procedure TLdapEditSession.AppendChange(var Changes: TLdapChangeDynArray;
  const DN, AttributeName: RawUtf8; Op: TLdapModifyOp;
  const Value: RawByteString);
var
  i: Integer;
  c: SizeInt;
begin
  i := GetChangeIndex(Changes, DN, AttributeName, Op);
  if (i < 0) then
  begin
    i := Length(Changes);
    SetLength(Changes, i + 1);
    Changes[i].DN := DN;
    Changes[i].AttributeName := AttributeName;
    Changes[i].Operation := Op;
  end;
  c := Length(Changes[i].Values);
  SetLength(Changes[i].Values, c + 1);
  Changes[i].Values[c] := Value;
end;

function TLdapEditSession.GetChangeIndex(var Changes: TLdapChangeDynArray;
  const DN, AttributeName: RawUtf8; Op: TLdapModifyOp): Integer;
var
  i: Integer;
begin
  result := -1;

  for i := 0 to High(Changes) do
    if (Changes[i].DN = DN) and (Changes[i].AttributeName = AttributeName) and (Changes[i].Operation = Op) then
    begin
      result := i;
      Exit;
    end;
end;

constructor TLdapEditSession.Create;
begin
  fRevision := 0;
  fInitial := nil;
  fWorking := nil;
  fExternalValueStates := nil;
end;

destructor TLdapEditSession.Destroy;
begin
  FreeAndNil(fInitial);
  FreeAndNil(fWorking);

  inherited Destroy;
end;

procedure TLdapEditSession.Load(const DN: RawUtf8;
  Attributes: TLdapAttributeList);
begin
  if Assigned(fInitial) or Assigned(fWorking) then
    raise Exception.Create('Already loaded');

  fDN := DN;
  fInitial := TLdapAttributeList(Attributes.Clone);
  fWorking := TLdapAttributeList(Attributes.Clone);
end;

function TLdapEditSession.GetRaw(const Name: RawUtf8; Index: Integer
  ): RawByteString;
begin
  result := fWorking.Find(Name).GetRaw(Index);
end;

function TLdapEditSession.GetReadable(const Name: RawUtf8; index: Integer
  ): RawUtf8;
begin
  result := fWorking.Find(Name).GetReadable(index);
end;

function TLdapEditSession.GetAllRaw(const Name: RawUtf8
  ): TRawByteStringDynArray;
var
  Attr: TLdapAttribute;
  i: Integer;
begin
  result := nil;
  Attr := fWorking.Find(Name);
  if not Assigned(Attr) then
    Exit;

  SetLength(result, Attr.Count);
  for i := 0 to Attr.Count - 1 do
    result[i] := Attr.GetRaw(i);
end;

function TLdapEditSession.GetAllReadable(const Name: RawUtf8
  ): TRawUtf8DynArray;
var
  Values: TRawUtf8DynArray;
begin
  Values := fWorking.Find(Name).GetAllReadable;
  result := Copy(Values, 0, Length(Values));
end;

procedure TLdapEditSession.SetValue(const Name: RawUtf8;
  const Value: RawByteString);
var
  Attr: TLdapAttribute;
begin
  Attr := GetAttr(Name);
  Attr.Add(Value, aoReplaceValue);
  Inc(fRevision);
end;

procedure TLdapEditSession.SetValues(const Name: RawUtf8;
  const Values: TRawByteStringDynArray);
var
  Attr: TLdapAttribute;
  i: Integer;
begin
  Attr := GetAttr(Name);
  Attr.Clear;
  for i := 0 to Length(Values) - 1 do
    Attr.Add(Values[i], aoNoDuplicateValue);
  Inc(fRevision);
end;

procedure TLdapEditSession.AddValue(const Name: RawUtf8;
  const Value: RawByteString);
var
  Attr: TLdapAttribute;
begin
  Attr := GetAttr(Name);
  Attr.Add(Value, aoNoDuplicateValue);
  Inc(fRevision);
end;

procedure TLdapEditSession.RemoveValue(const Name: RawUtf8;
  const Value: RawByteString);
var
  Attr: TLdapAttribute;
  Values: TRawByteStringDynArray;
  Idx: PtrInt;
  i: Integer;
begin
  Attr := fWorking.Find(Name);
  if not Assigned(Attr) then
    Exit;

  Idx := Attr.FindIndex(Value);
  if Idx < 0 then
    Exit;

  Values := Copy(Attr.List, 0, Attr.Count);
  Attr.Clear;

  for i := 0 to High(Values) do
    if (i <> Idx) then
      Attr.Add(Values[i], aoAlways);

  if Attr.Count = 0 then
    fWorking.Delete(name);

  Inc(fRevision);
end;

procedure TLdapEditSession.ClearAttribute(const Name: RawUtf8);
begin
  if not Assigned(fWorking.Find(Name)) then
    Exit;

  fWorking.Delete(Name);
  Inc(fRevision);
end;

procedure TLdapEditSession.Restore(const Name: RawUtf8);
var
  Attr, InitialAttr: TLdapAttribute;
begin
  InitialAttr := fInitial.Find(Name);
  if not Assigned(InitialAttr) then
    fWorking.Delete(Name)
  else
  begin
    Attr := GetAttr(Name);
    Attr.Assign(InitialAttr);
  end;
  Inc(fRevision);
end;

procedure TLdapEditSession.RestoreAll;
var
  i: Integer;
begin
  FreeAndNil(fWorking);
  fWorking := TLdapAttributeList(fInitial.Clone);

  for i := 0 to High(fExternalValueStates) do
    fExternalValueStates[i].WorkingPresence := fExternalValueStates[i].InitialPresence;
  Inc(fRevision);
end;

procedure TLdapEditSession.LoadValueState(const DN, Name: RawUtf8;
  const Value: RawByteString; Presence: TLdapPresence);
var
  i: Integer;
begin
  if Presence = lpUnknown then
    Exit;

  i := GetValueStateIndex(DN, Name, Value);
  if (i < 0) then
  begin
    i := Length(fExternalValueStates);
    SetLength(fExternalValueStates, i + 1);
    fExternalValueStates[i].DN := DN;
    fExternalValueStates[i].AttributeName := Name;
    fExternalValueStates[i].Value := Value;
    fExternalValueStates[i].InitialPresence := Presence;
    fExternalValueStates[i].WorkingPresence := Presence;
    Inc(fRevision);
    Exit;
  end;

  if fExternalValueStates[i].InitialPresence <> lpUnknown then
    Exit;
  fExternalValueStates[i].InitialPresence := Presence;

  if fExternalValueStates[i].WorkingPresence = lpUnknown then
    fExternalValueStates[i].WorkingPresence := Presence;

  Inc(fRevision);
end;

procedure TLdapEditSession.SetValueState(const DN, Name: RawUtf8;
  const Value: RawByteString; Presence: TLdapPresence);
var
  i: Integer;
begin
  i := GetValueStateIndex(DN, Name, Value);
  if (i < 0) then
  begin
    i := Length(fExternalValueStates);
    SetLength(fExternalValueStates, i + 1);
    fExternalValueStates[i].DN := DN;
    fExternalValueStates[i].AttributeName := Name;
    fExternalValueStates[i].Value := Value;
    fExternalValueStates[i].InitialPresence := lpUnknown;
  end;
  fExternalValueStates[i].WorkingPresence := Presence;
  Inc(fRevision);
end;

function TLdapEditSession.GetValuesState(const DN, Name: RawUtf8;
  Snapshot: TLdapSnapshot): TRawByteStringDynArray;
var
  i, c: Integer;
begin
  result := nil;
  c := 0;
  for i := 0 to Length(fExternalValueStates) - 1 do
    if (fExternalValueStates[i].DN = DN) and (fExternalValueStates[i].AttributeName = Name) and (GetStatePresence(fExternalValueStates[i], Snapshot) = lpPresent) then
    begin
      SetLength(result, c + 1);
      result[c] := fExternalValueStates[i].Value;
      Inc(c);
    end;
end;

function TLdapEditSession.GetValuesStateBackward(const Name: RawUtf8;
  const Value: RawByteString; Snapshot: TLdapSnapshot): TRawUtf8DynArray;
var
  c, i: Integer;
begin
  result := nil;
  c := 0;
  for i := 0 to Length(fExternalValueStates) - 1 do
    if (fExternalValueStates[i].AttributeName = Name) and (fExternalValueStates[i].Value = Value) and (GetStatePresence(fExternalValueStates[i], Snapshot) = lpPresent) then
    begin
      SetLength(result, c + 1);
      result[c] := fExternalValueStates[i].DN;
      Inc(c);
    end;
end;

procedure TLdapEditSession.RestoreValueState(const DN, Name: RawUtf8;
  const Value: rawbytestring);
var
  i: Integer;
begin
  i := GetValueStateIndex(DN, Name, Value);
  if i < 0 then
    Exit;

  fExternalValueStates[i].WorkingPresence := fExternalValueStates[i].InitialPresence;
  Inc(fRevision);
end;

function TLdapEditSession.IsModified: Boolean;
var
  i: Integer;
begin
  result := True;

  for i := 0 to fInitial.Count - 1 do
    if IsModified(fInitial.Items[i].AttributeName) then
      Exit;

  for i := 0 to fWorking.Count - 1 do
    if IsModified(fWorking.Items[i].AttributeName) then
      Exit;

  for i := 0 to High(fExternalValueStates) do
    if fExternalValueStates[i].InitialPresence <> fExternalValueStates[i].WorkingPresence then
      Exit;

  result := False;

end;

function TLdapEditSession.IsModified(const Name: RawUtf8): Boolean;
begin
  result := not AttributeEqual(fInitial.Find(Name), fWorking.Find(Name));
end;

function TLdapEditSession.BuildChangeSet: TLdapChangeDynArray;
begin
  result := nil;

  BuildLocalChanges(result);
  BuildExternalChanges(result);
end;

end.

