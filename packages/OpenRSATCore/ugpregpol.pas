unit ugpregpol;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  StrUtils,
  mormot.core.base,
  mormot.core.text,
  ugpocore;

const
  // Registry value types (winreg.h)
  REG_NONE = 0;
  REG_SZ = 1;
  REG_EXPAND_SZ = 2;
  REG_BINARY = 3;
  REG_DWORD = 4;
  REG_DWORD_BIG_ENDIAN = 5;
  REG_LINK = 6;
  REG_MULTI_SZ = 7;
  REG_QWORD = 11;

  // REGF record types
  REGDF_END = $00;
  REGDF_KEY = $6C;   // 'l'
  REGDF_VALUE = $76; // 'v'

  // Header layout of a Registry.pol file
  REGDF_HEADER_SIZE = 24;
  REGDF_HEADER_SIGNATURE = 'PRegf';
  REGDF_HEADER_MACHINE_OFFSET = 12;

type

  { TGPRegPolValue }

  /// A single value of a Registry.pol key.
  TGPRegPolValue = class
  private
    fName: RawUtf8;
    fValueType: Cardinal;
    fData: RawByteString;
  public
    constructor Create(const AName: RawUtf8; AValueType: Cardinal;
      const AData: RawByteString); overload;
    constructor Create(const AName: RawUtf8); overload;

    /// Set the value as a REG_SZ string.
    procedure SetString(const AValue: RawUtf8);
    /// Set the value as a REG_DWORD (little endian).
    procedure SetDWord(AValue: Cardinal);
    /// Set the value as a REG_BINARY blob.
    procedure SetBinary(const AData: RawByteString);
    /// Set the raw value type and data (used by the visual editor).
    procedure SetData(AValueType: Cardinal; const AData: RawByteString);

    /// Read the value as a string (REG_SZ / REG_EXPAND_SZ).
    function AsString: RawUtf8;
    /// Read the value as a DWORD (REG_DWORD).
    function AsDWord: Cardinal;

    property Name: RawUtf8 read fName;
    property ValueType: Cardinal read fValueType;
    property Data: RawByteString read fData;
  end;
  TGPRegPolValueArray = array of TGPRegPolValue;

  TGPRegPolKey = class;
  TGPRegPolKeyArray = array of TGPRegPolKey;

  { TGPRegPolKey }

  /// A key of a Registry.pol file, holding values and nested sub keys.
TGPRegPolKey = class
  private
    fPath: RawUtf8;
    fValues: TGPRegPolValueArray;
    fSubKeys: TGPRegPolKeyArray;

    function GetName: RawUtf8;
    function FindValue(const AName: RawUtf8): TGPRegPolValue;
    function FindSubKey(const AName: RawUtf8): TGPRegPolKey;
  public
    constructor Create(const APath: RawUtf8);
    destructor Destroy; override;

    /// Set a REG_SZ value (creates it when missing).
    procedure SetStringValue(const AName, AValue: RawUtf8);
    /// Set a REG_DWORD value (creates it when missing).
    procedure SetDWordValue(const AName: RawUtf8; AValue: Cardinal);
    /// Set the raw type and data of a value (creates it when missing).
    procedure SetValueData(const AName: RawUtf8; AType: Cardinal;
      const AData: RawByteString);
    /// Remove a value of the key.
    procedure RemoveValue(const AName: RawUtf8);

    /// Retrieve a value of the key, or nil.
    function GetValue(const AName: RawUtf8): TGPRegPolValue;
    /// Retrieve a sub key by its name (not the full path), or nil.
    function GetSubKey(const AName: RawUtf8): TGPRegPolKey;
    /// Create (or retrieve) a sub key by its name.
    function AddSubKey(const AName: RawUtf8): TGPRegPolKey;

    /// Full key path (e.g. Software\Policies\Test)
    property Path: RawUtf8 read fPath;
    /// Simple key name (the last segment of the path)
    property Name: RawUtf8 read GetName;
    property Values: TGPRegPolValueArray read fValues;
    property SubKeys: TGPRegPolKeyArray read fSubKeys;
  end;

  { TGPRegPol }

  /// A complete Registry.pol file (REGF format), as stored in the GPT.
  TGPRegPol = class
  private
    fMachine: Boolean;
    fRootKeys: array of TGPRegPolKey;
  public
    destructor Destroy; override;

    /// Parse a Registry.pol binary content. Raises EGPOException on an
    /// invalid file. The caller owns the returned instance.
    class function LoadFromBytes(const ABytes: RawByteString): TGPRegPol;

    /// Serialize the policy back to the REGF binary format.
    function SaveToBytes: RawByteString;

    /// Retrieve a key by its full path (e.g. Software\Policies\Test), or nil.
    function FindKey(const APath: RawUtf8): TGPRegPolKey;

    /// Create (or retrieve) a key by its full path.
    function AddKey(const APath: RawUtf8): TGPRegPolKey;

    /// True when the file targets the machine side, False for the user side.
    property Machine: Boolean read fMachine write fMachine;
    property RootKeys: TGPRegPolKeyArray read fRootKeys;
  end;

implementation

{ TGPRegPolValue }

constructor TGPRegPolValue.Create(const AName: RawUtf8; AValueType: Cardinal;
  const AData: RawByteString);
begin
  fName := AName;
  fValueType := AValueType;
  fData := AData;
end;

constructor TGPRegPolValue.Create(const AName: RawUtf8);
begin
  Create(AName, REG_SZ, '');
end;

procedure TGPRegPolValue.SetString(const AValue: RawUtf8);
begin
  fValueType := REG_SZ;
  fData := AValue;
end;

procedure TGPRegPolValue.SetDWord(AValue: Cardinal);
begin
  fValueType := REG_DWORD;
  SetLength(fData, SizeOf(Cardinal));
  PCardinal(@fData[1])^ := AValue;
end;

procedure TGPRegPolValue.SetBinary(const AData: RawByteString);
begin
  fValueType := REG_BINARY;
  fData := AData;
end;

procedure TGPRegPolValue.SetData(AValueType: Cardinal; const AData: RawByteString);
begin
  fValueType := AValueType;
  fData := AData;
end;

function TGPRegPolValue.AsString: RawUtf8;
begin
  if (fValueType in [REG_SZ, REG_EXPAND_SZ]) then
    result := fData
  else
    result := '';
end;

function TGPRegPolValue.AsDWord: Cardinal;
begin
  result := 0;
  if (fValueType = REG_DWORD) and (Length(fData) >= SizeOf(Cardinal)) then
    result := PCardinal(@fData[1])^;
end;

{ TGPRegPolKey }

constructor TGPRegPolKey.Create(const APath: RawUtf8);
begin
  fPath := APath;
end;

destructor TGPRegPolKey.Destroy;
var
  i: Integer;
begin
  for i := 0 to High(fValues) do
    fValues[i].Free;
  fValues := nil;

  for i := 0 to High(fSubKeys) do
    fSubKeys[i].Free;
  fSubKeys := nil;

  inherited Destroy;
end;

function TGPRegPolKey.FindValue(const AName: RawUtf8): TGPRegPolValue;
var
  i: Integer;
begin
  result := nil;
  for i := 0 to High(fValues) do
    if (fValues[i].fName = AName) then
      Exit(fValues[i]);
end;

function TGPRegPolKey.GetName: RawUtf8;
var
  Sep: Integer;
begin
  Sep := RPos('\', fPath);
  if (Sep = 0) then
    result := fPath
  else
    result := Copy(fPath, Sep + 1, MaxInt);
end;

function TGPRegPolKey.FindSubKey(const AName: RawUtf8): TGPRegPolKey;
var
  i: Integer;
begin
  result := nil;
  for i := 0 to High(fSubKeys) do
    if SameText(fSubKeys[i].GetName, AName) then
      Exit(fSubKeys[i]);
end;

procedure TGPRegPolKey.SetStringValue(const AName, AValue: RawUtf8);
var
  Value: TGPRegPolValue;
begin
  Value := FindValue(AName);
  if not Assigned(Value) then
  begin
    Value := TGPRegPolValue.Create(AName);
    SetLength(fValues, Length(fValues) + 1);
    fValues[High(fValues)] := Value;
  end;
  Value.SetString(AValue);
end;

procedure TGPRegPolKey.SetDWordValue(const AName: RawUtf8; AValue: Cardinal);
var
  Value: TGPRegPolValue;
begin
  Value := FindValue(AName);
  if not Assigned(Value) then
  begin
    Value := TGPRegPolValue.Create(AName);
    SetLength(fValues, Length(fValues) + 1);
    fValues[High(fValues)] := Value;
  end;
  Value.SetDWord(AValue);
end;

procedure TGPRegPolKey.SetValueData(const AName: RawUtf8; AType: Cardinal;
  const AData: RawByteString);
var
  Value: TGPRegPolValue;
begin
  Value := FindValue(AName);
  if not Assigned(Value) then
  begin
    Value := TGPRegPolValue.Create(AName);
    SetLength(fValues, Length(fValues) + 1);
    fValues[High(fValues)] := Value;
  end;
  Value.SetData(AType, AData);
end;

procedure TGPRegPolKey.RemoveValue(const AName: RawUtf8);
var
  i: Integer;
begin
  for i := High(fValues) downto 0 do
    if (fValues[i].fName = AName) then
    begin
      fValues[i].Free;
      fValues[i] := fValues[High(fValues)];
      SetLength(fValues, Length(fValues) - 1);
    end;
end;

function TGPRegPolKey.GetValue(const AName: RawUtf8): TGPRegPolValue;
begin
  result := FindValue(AName);
end;

function TGPRegPolKey.GetSubKey(const AName: RawUtf8): TGPRegPolKey;
begin
  result := FindSubKey(AName);
end;

function TGPRegPolKey.AddSubKey(const AName: RawUtf8): TGPRegPolKey;
begin
  result := FindSubKey(AName);
  if Assigned(result) then
    Exit;

  result := TGPRegPolKey.Create(fPath + '\' + AName);
  SetLength(fSubKeys, Length(fSubKeys) + 1);
  fSubKeys[High(fSubKeys)] := result;
end;

{ TGPRegPol }

destructor TGPRegPol.Destroy;
var
  i: Integer;
begin
  for i := 0 to High(fRootKeys) do
    fRootKeys[i].Free;
  fRootKeys := nil;

  inherited Destroy;
end;

function TGPRegPol.FindKey(const APath: RawUtf8): TGPRegPolKey;
var
  i, Start, Sep: Integer;
  Part, ChildName: RawUtf8;
  Current: TGPRegPolKey;
  Parts: array of RawUtf8;
begin
  result := nil;

  if (APath = '') then
    Exit;

  // Split the path on '\' and walk the sub keys.
  Start := 1;
  repeat
    Sep := PosEx('\', APath, Start);
    if (Sep = 0) then
      Part := Copy(APath, Start, MaxInt)
    else
    begin
      Part := Copy(APath, Start, Sep - Start);
      Start := Sep + 1;
    end;

    SetLength(Parts, Length(Parts) + 1);
    Parts[High(Parts)] := Part;
  until (Sep = 0);

  if (Length(Parts) = 0) then
    Exit;

  // First part is a root key.
  Current := nil;
  for i := 0 to High(fRootKeys) do
    if SameText(fRootKeys[i].fPath, Parts[0]) then
      Current := fRootKeys[i];
  if not Assigned(Current) then
    Exit;

  // Remaining parts: sub keys.
  for i := 1 to High(Parts) do
  begin
    ChildName := Parts[i];
    if SameText(Current.fPath, ChildName) then
      Continue;
    Current := Current.FindSubKey(ChildName);
    if not Assigned(Current) then
      Exit;
  end;

  result := Current;
end;

function TGPRegPol.AddKey(const APath: RawUtf8): TGPRegPolKey;
var
  i, Start, Sep: Integer;
  Part, ChildName: RawUtf8;
  Current: TGPRegPolKey;
  Parts: array of RawUtf8;
begin
  result := nil;

  if (APath = '') then
    Exit;

  // Split the path on '\'.
  Start := 1;
  repeat
    Sep := PosEx('\', APath, Start);
    if (Sep = 0) then
      Part := Copy(APath, Start, MaxInt)
    else
    begin
      Part := Copy(APath, Start, Sep - Start);
      Start := Sep + 1;
    end;
    SetLength(Parts, Length(Parts) + 1);
    Parts[High(Parts)] := Part;
  until (Sep = 0);

  if (Length(Parts) = 0) then
    Exit;

  // First part: root key.
  Current := nil;
  for i := 0 to High(fRootKeys) do
    if SameText(fRootKeys[i].fPath, Parts[0]) then
      Current := fRootKeys[i];

  if not Assigned(Current) then
  begin
    Current := TGPRegPolKey.Create(Parts[0]);
    SetLength(fRootKeys, Length(fRootKeys) + 1);
    fRootKeys[High(fRootKeys)] := Current;
  end;

  // Remaining parts: sub keys.
  for i := 1 to High(Parts) do
  begin
    ChildName := Parts[i];
    if SameText(Current.fPath, ChildName) then
      Continue;
    Current := Current.AddSubKey(ChildName);
  end;

  result := Current;
end;

{ REGF binary reading/writing }

// All offsets below are 0-based byte positions. RawByteString indexing is
// 1-based, hence the systematic AOffset + 1 in string accesses.

function Utf8ToUtf16Le(const AValue: RawUtf8): RawByteString;
var
  W: WideString;
begin
  W := UTF8Decode(AValue);
  SetLength(result, Length(W) * 2);
  if (Length(W) > 0) then
    Move(W[1], result[1], Length(W) * 2);
end;

function Utf16LeToUtf8(const ABytes: RawByteString): RawUtf8;
var
  W: WideString;
begin
  SetLength(W, Length(ABytes) div 2);
  if (Length(ABytes) >= 2) then
    Move(ABytes[1], W[1], Length(ABytes));
  result := UTF8Encode(W);
end;

function ReadWordLE(const ABytes: RawByteString; AOffset: Integer): Word; inline;
begin
  result := Ord(ABytes[AOffset + 1]) or
    (Ord(ABytes[AOffset + 2]) shl 8);
end;

function ReadDWordLE(const ABytes: RawByteString; AOffset: Integer): Cardinal; inline;
begin
  result := Ord(ABytes[AOffset + 1]) or
    (Cardinal(Ord(ABytes[AOffset + 2])) shl 8) or
    (Cardinal(Ord(ABytes[AOffset + 3])) shl 16) or
    (Cardinal(Ord(ABytes[AOffset + 4])) shl 24);
end;

/// Read a UTF-16LE null-terminated string at AOffset. Returns the text and
/// the number of bytes consumed (including the null terminator).
function ReadUtf16Z(const ABytes: RawByteString; AOffset, AMaxBytes: Integer;
  out AText: RawUtf8; out AConsumed: Integer): Boolean;
var
  i: Integer;
begin
  result := False;
  AText := '';
  AConsumed := 0;

  // Scan UTF-16LE characters (2 bytes each). Stepping by 2 is mandatory,
  // otherwise the zero high byte of a character would match a null pair.
  i := AOffset;
  while (i + 1 < AOffset + AMaxBytes) do
  begin
    if (Ord(ABytes[i + 1]) = 0) and (Ord(ABytes[i + 2]) = 0) then
    begin
      AText := Utf16LeToUtf8(Copy(ABytes, AOffset + 1, i - AOffset));
      AConsumed := i - AOffset + 2;
      Exit(True);
    end;
    Inc(i, 2);
  end;
end;

class function TGPRegPol.LoadFromBytes(const ABytes: RawByteString): TGPRegPol;

  function ParseKey(AOffset, AMaxBytes: Integer;
    const AParentPath: RawUtf8): TGPRegPolKey;
  var
    RecordSize: Word;
    KeyName, FullPath, Text: RawUtf8;
    Consumed, Pos, KeyEnd: Integer;
    ValueName: RawUtf8;
    ValueType, ValueSize: Cardinal;
    BType: Byte;
  begin
    result := nil;

    if (AMaxBytes < 3) then
      raise EGPOException.Create('Invalid Registry.pol: truncated key record', []);

    BType := Ord(ABytes[AOffset + 1]);
    if (BType <> REGDF_KEY) then
      raise EGPOException.Create('Invalid Registry.pol: expected a key record', []);

    RecordSize := ReadWordLE(ABytes, AOffset + 1);
    if (RecordSize < 3) or (RecordSize > AMaxBytes) then
      raise EGPOException.Create('Invalid Registry.pol: invalid key record size', []);

    if not ReadUtf16Z(ABytes, AOffset + 3, RecordSize - 3, KeyName, Consumed) then
      raise EGPOException.Create('Invalid Registry.pol: unterminated key name', []);

    if (AParentPath <> '') then
      FullPath := AParentPath + '\' + KeyName
    else
      FullPath := KeyName;

    result := TGPRegPolKey.Create(FullPath);
    KeyEnd := AOffset + RecordSize;
    Pos := AOffset + 3 + Consumed;

    while (Pos < KeyEnd) do
    begin
      BType := Ord(ABytes[Pos + 1]);
      case BType of
        REGDF_END:
        begin
          Inc(Pos, 3);
        end;
        REGDF_VALUE:
        begin
          if (KeyEnd - Pos < 3) then
            raise EGPOException.Create('Invalid Registry.pol: truncated value record', []);
          ValueSize := ReadWordLE(ABytes, Pos + 1);
          if (ValueSize < 3) or (Pos + ValueSize > KeyEnd) then
            raise EGPOException.Create('Invalid Registry.pol: invalid value record size', []);

          if not ReadUtf16Z(ABytes, Pos + 3, ValueSize - 3, ValueName, Consumed) then
            raise EGPOException.Create('Invalid Registry.pol: unterminated value name', []);

          if (Pos + 3 + Consumed + 8 > Pos + ValueSize) then
            raise EGPOException.Create('Invalid Registry.pol: truncated value data', []);

          ValueType := ReadDWordLE(ABytes, Pos + 3 + Consumed);
          ValueSize := ReadDWordLE(ABytes, Pos + 3 + Consumed + 4);

          if (Pos + 3 + Consumed + 8 + Integer(ValueSize) > Pos + Integer(RecordSize)) then
            raise EGPOException.Create('Invalid Registry.pol: value data out of bounds', []);

          SetLength(result.fValues, Length(result.fValues) + 1);
          result.fValues[High(result.fValues)] := TGPRegPolValue.Create(
            ValueName, ValueType,
            Copy(ABytes, Pos + 4 + Consumed + 8, ValueSize));

          Inc(Pos, 3 + Consumed + 8 + Integer(ValueSize));
        end;
        REGDF_KEY:
        begin
          SetLength(result.fSubKeys, Length(result.fSubKeys) + 1);
          result.fSubKeys[High(result.fSubKeys)] :=
            ParseKey(Pos, KeyEnd - Pos, FullPath);
          Inc(Pos, ReadWordLE(ABytes, Pos + 1));
        end;
        else
          raise EGPOException.Create('Invalid Registry.pol: unknown record type %',
            [BType]);
      end;
    end;
  end;

var
  Pos: Integer;
  BType: Byte;
begin
  result := TGPRegPol.Create;

  if (Length(ABytes) < REGDF_HEADER_SIZE) then
    raise EGPOException.Create('Invalid Registry.pol: file too short', []);

  if (Copy(ABytes, 1, 5) <> REGDF_HEADER_SIGNATURE) then
    raise EGPOException.Create('Invalid Registry.pol: bad signature', []);

  result.fMachine := (ReadDWordLE(ABytes, REGDF_HEADER_MACHINE_OFFSET) <> 0);

  Pos := REGDF_HEADER_SIZE;
  while (Pos < Length(ABytes)) do
  begin
    BType := Ord(ABytes[Pos + 1]);
    case BType of
      REGDF_END:
        Inc(Pos, 3);
      REGDF_KEY:
      begin
        SetLength(result.fRootKeys, Length(result.fRootKeys) + 1);
        result.fRootKeys[High(result.fRootKeys)] :=
          ParseKey(Pos, Length(ABytes) - Pos, '');
        Inc(Pos, ReadWordLE(ABytes, Pos + 1));
      end;
      else
        raise EGPOException.Create('Invalid Registry.pol: unknown top-level record type %',
          [BType]);
    end;
  end;
end;

function TGPRegPol.SaveToBytes: RawByteString;
  procedure WriteWord(var ABytes: RawByteString; AOffset: Integer; AValue: Word);
  begin
    ABytes[AOffset + 1] := AnsiChar(AValue and $FF);
    ABytes[AOffset + 2] := AnsiChar((AValue shr 8) and $FF);
  end;

  function SerializeKey(const AKey: TGPRegPolKey): RawByteString;
  var
    NameBytes, Children, Child: RawByteString;
    i: Integer;
    Value: TGPRegPolValue;
    ValueNameBytes, ValueData: RawByteString;
    ValueRecord: RawByteString;
  begin
    NameBytes := Utf8ToUtf16Le(AKey.GetName + #0);
    Children := '';

    for i := 0 to High(AKey.fValues) do
    begin
      Value := AKey.fValues[i];
      ValueNameBytes := Utf8ToUtf16Le(Value.fName + #0);
      ValueData := Value.fData;
      ValueRecord := #$76 + #0#0 + ValueNameBytes +
        AnsiChar(Value.fValueType and $FF) +
        AnsiChar((Value.fValueType shr 8) and $FF) +
        AnsiChar((Value.fValueType shr 16) and $FF) +
        AnsiChar((Value.fValueType shr 24) and $FF) +
        AnsiChar(Length(ValueData) and $FF) +
        AnsiChar((Length(ValueData) shr 8) and $FF) +
        AnsiChar((Length(ValueData) shr 16) and $FF) +
        AnsiChar((Length(ValueData) shr 24) and $FF) +
        ValueData;
      WriteWord(ValueRecord, 1, 3 + Length(ValueNameBytes) + 8 + Length(ValueData));
      Children := Children + ValueRecord;
    end;

    for i := 0 to High(AKey.fSubKeys) do
      Children := Children + SerializeKey(AKey.fSubKeys[i]);

    // REGDF_END record
    Children := Children + #0 + #3 + #0;

    result := #$6C + #0#0 + NameBytes + Children;
    WriteWord(result, 1, 3 + Length(NameBytes) + Length(Children));
  end;

  procedure WriteDWord(var ABytes: RawByteString; AOffset: Integer; AValue: Cardinal);
  begin
    ABytes[AOffset + 1] := AnsiChar(AValue and $FF);
    ABytes[AOffset + 2] := AnsiChar((AValue shr 8) and $FF);
    ABytes[AOffset + 3] := AnsiChar((AValue shr 16) and $FF);
    ABytes[AOffset + 4] := AnsiChar((AValue shr 24) and $FF);
  end;

var
  i: Integer;
  KeyBytes: RawByteString;
begin
  // 24-byte REGF header.
  result := REGDF_HEADER_SIGNATURE + #0 +
    #1 + #1 +           // major, minor version
    #0 + #0 +           // flags
    #0 + #0 +           // reserved
    #0 + #0 + #0 + #0 + // machine (patched below)
    #0 + #0 + #0 + #0 + // reserved
    #0 + #0 + #0 + #0;  // reserved

  WriteDWord(result, REGDF_HEADER_MACHINE_OFFSET, Ord(fMachine));

  for i := 0 to High(fRootKeys) do
  begin
    KeyBytes := SerializeKey(fRootKeys[i]);
    result := result + KeyBytes;
  end;
end;

end.