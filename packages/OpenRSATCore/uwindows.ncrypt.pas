unit uwindows.ncrypt;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.os
  {$IFDEF WINDOWS}
  ,windows
  {$ENDIF WINDOWS};

{$IFDEF WINDOWS}
type
  SECURITY_STATUS = LongInt;

  NCRYPT_DESCRIPTOR_HANDLE = Pointer;
  PNCRYPT_DESCRIPTOR_HANDLE = ^NCRYPT_DESCRIPTOR_HANDLE;

  PFN_NCRYPT_ALLOC = function(cbSize: NativeUInt): Pointer; stdcall;
  PFN_NCRYPT_FREE = procedure(pv: Pointer); stdcall;

  NCRYPT_ALLOC_PARA = record
    cbSize: DWORD;
    pfnAlloc: PFN_NCRYPT_ALLOC;
    pfnFree: PFN_NCRYPT_FREE;
  end;
  PNCRYPT_ALLOC_PARA = ^NCRYPT_ALLOC_PARA;

  TNcryptUnprotectSecret = function(
    phDescriptor: PNCRYPT_DESCRIPTOR_HANDLE;
    dwFlags: Cardinal;
    pbProtectedBlob: PByte;
    cbProtectedBlob: Cardinal;
    pMemPara: Pointer;
    hWnd: HWND;
    out ppbData: PByte;
    out pcbData: Cardinal
  ): LongInt; stdcall;

  TNCryptCloseProtectionDescriptor = function(
    hDescriptor: NCRYPT_DESCRIPTOR_HANDLE
  ): LongInt; stdcall;

  TNCryptGetProtectionDescriptorInfo = function(
    hDescriptor: NCRYPT_DESCRIPTOR_HANDLE;
    pMemPara: PNCRYPT_ALLOC_PARA;
    dwInfoType: DWord;
    ppvInfo: PPointer
  ): LongInt; stdcall;

const
  /// decodes only the header of the protected data blob.
  /// No actual decryption takes place.
  NCRYPT_UNPROTECT_NO_DECRYPT = $00000001;
  /// requests that the key service provider not display a user interface
  NCRYPT_SILENT_FLAG          = $00000040;

  NCRYPT_PROTECTION_INFO_TYPE_DESCRIPTOR_STRING = $00000001;

var
  NCryptDll: THandle;
  NcryptUnprotectSecret: TNcryptUnprotectSecret;
  NCryptCloseProtectionDescriptor: TNCryptCloseProtectionDescriptor;
  NCryptGetProtectionDescriptorInfo: TNCryptGetProtectionDescriptorInfo;

/// Decrypts an encrypted LAPS value in Windows using DPAPI-NG.
///
/// The value provided must match the contents of the
/// msLAPS-EncryptedPassword attribute encoded in hexadecimal format.
///
/// The function:
/// - converts the hexadecimal representation to binary data;
/// - validates and extracts the 16-byte LAPS header;
/// - verifies the size of the encrypted content and the reserved field;
/// - extracts the DPAPI-NG blob;
/// - decrypts it using NCryptUnprotectSecret with the
///   current Windows security context;
/// - converts the UTF-16LE result to a RawByteString.
///
/// @param ProtectedData Hexadecimal value of msLAPS-EncryptedPassword.
/// @returns The decrypted LAPS content, typically a JSON object containing
///          the account name, password, and timestamp.
///
/// @raises Exception If the value is empty, if the LAPS header is invalid,
///         if the sizes are inconsistent, or if DPAPI-NG cannot
///         decrypt the secret. Decryption may fail, in particular, if
///         the current Windows security context is not authorized to
///         decrypt the password.
function DecryptDpapiNgWindows(
  const ProtectedData: RawByteString
): RawByteString;
{$ENDIF WINDOWS}

implementation

{$IFDEF WINDOWS}

/// convert a RawByteString into TBytes
// - Used to convert the msLAPS-EncryptedPassword LDAP attribute
function HexToBytes(const S: RawByteString): TBytes;
var
  i, L: Integer;

  function HexNibble(C: AnsiChar): Byte;
  begin
    case C of
      '0'..'9': Result := Ord(C) - Ord('0');
      'a'..'f': Result := Ord(C) - Ord('a') + 10;
      'A'..'F': Result := Ord(C) - Ord('A') + 10;
    else
      raise Exception.CreateFmt('Invalid hexadecimal character: %s', [C]);
    end;
  end;

begin
  L := Length(S);

  if (L and 1) <> 0 then
    raise Exception.CreateFmt(
      'Invalid hexadecimal string length: %d',
      [L]
    );

  SetLength(Result, L div 2);

  for i := 0 to High(Result) do
    Result[i] :=
      (HexNibble(S[i * 2 + 1]) shl 4) or
       HexNibble(S[i * 2 + 2]);
end;

/// convert a TBytes into a RawByteString
// - Used to convert the result of NCryptUnprotectSecret
function Utf16LeBytesToString(const AData: TBytes): RawByteString;
begin
  Result := '';

  if Length(AData) = 0 then
    Exit;

  SetString(
    Result,
    PWideChar(@AData[0]),
    Length(AData) div 2
  );
end;

/// convert a SECURITY_STATUS into a hex code
function StatusToHex(const AStatus: SECURITY_STATUS): string;
var
  U: DWord absolute AStatus;
begin
  Result := '0x' + IntToHex(U, 8);
end;

/// read a UInt32 from a TBytes at offset
function ReadUInt32LE(const Data: TBytes; Offset: Integer): Cardinal;
begin
  result := Cardinal(Data[Offset]) or
    (Cardinal(Data[Offset + 1]) shl 8) or
    (Cardinal(Data[Offset + 2]) shl 16) or
    (Cardinal(Data[Offset + 3]) shl 24);
end;

/// convert a PWideChar to a UnicodeString
function PWideCharToUnicodeString(P: PWideChar): UnicodeString;
var
  L: SizeInt;
begin
  Result := '';

  if P = nil then
    Exit;

  L := 0;
  while P[L] <> #0 do
    Inc(L);

  SetString(Result, P, L);
end;

function NcryptUnprotectSecretAvailable: Boolean;
begin
  result := Assigned(NcryptUnprotectSecret) or
    DelayedProc(NcryptUnprotectSecret, NCryptDll, 'ncrypt.dll', 'NcryptUnprotectSecret');
end;

function NCryptCloseProtectionDescriptorAvailable: Boolean;
begin
  result := Assigned(NCryptCloseProtectionDescriptor) or
    DelayedProc(NCryptCloseProtectionDescriptor, NCryptDll, 'ncrypt.dll', 'NCryptCloseProtectionDescriptor');
end;

function DecryptDpapiNgWindows(const ProtectedData: RawByteString
  ): RawByteString;
var
  Descriptor: NCRYPT_DESCRIPTOR_HANDLE;
  Plain: PByte;
  PlainLen, EncryptedPasswordSize, ReservedValue: Cardinal;
  Status: SECURITY_STATUS;
  Value, ProtectedDataBytes, UnprotectedDataBytes: TBytes;
begin
  result := '';

  Descriptor := nil;
  Plain := nil;
  PlainLen := 0;

  if not NcryptUnprotectSecretAvailable then
    raise Exception.Create('NcryptUnprotectSecret is not available on this version of Windows');
  if not NCryptCloseProtectionDescriptorAvailable then
    raise Exception.Create('NCryptCloseProtectionDescriptor is not available on this version of Windowss');

  ProtectedDataBytes := HexToBytes(ProtectedData);

  if Length(ProtectedDataBytes) = 0 then
    raise Exception.Create('Empty data.');

  if Length(ProtectedData) < 16 then
    raise Exception.Create('Invalid LAPS blob');

  EncryptedPasswordSize := ReadUInt32LE(ProtectedDataBytes, 8);
  ReservedValue := ReadUInt32LE(ProtectedDataBytes, 12);

  if ReservedValue <> 0 then
    raise Exception.CreateFmt(
    'Invalid LAPS blob: Reserved=0x%.8x',
    [ReservedValue]
  );

  if EncryptedPasswordSize <> Cardinal(Length(ProtectedDataBytes) - 16) then
    raise Exception.CreateFmt(
      'Invalid LAPS encrypted size: header=%d actual=%d',
      [EncryptedPasswordSize, Length(ProtectedDataBytes) - 16]
    );

  Value := Copy(ProtectedDataBytes, 16, EncryptedPasswordSize);

  Status := NCryptUnprotectSecret(
    @Descriptor,
    NCRYPT_SILENT_FLAG,
    @Value[0],
    Length(Value),
    nil,
    0,
    Plain,
    PlainLen
  );

  if Status <> 0 then
    raise Exception.CreateFmt(
      'NCryptUnprotectSecret failed: %s (%d)',
      [StatusToHex(Status), Status]
    );

  try
    SetLength(UnprotectedDataBytes, PlainLen);

    if PlainLen <> 0 then
      Move(Plain^, UnprotectedDataBytes[0], PlainLen);

    result := Utf16LeBytesToString(UnprotectedDataBytes);
  finally
    if Plain <> nil then
    begin
      FillChar(Plain^, PlainLen, 0);
      LocalFree(HLOCAL(Plain));
    end;

    if Descriptor <> nil then
      NCryptCloseProtectionDescriptor(Descriptor);
  end;
end;
{$ENDIF WINDOWS}

end.

