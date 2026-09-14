unit uwindows.ncrypt;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils
  {$IFDEF WINDOWS}
  ,windows
  {$ENDIF WINDOWS};

type
  NCRYPT_DESCRIPTOR_HANDLE = Pointer;
  PNCRYPT_DESCRIPTOR_HANDLE = ^NCRYPT_DESCRIPTOR_HANDLE;

  NCRYPT_ALLOC_PARA = Pointer;
  PNCRYPT_ALLOC_PARA = ^NCRYPT_ALLOC_PARA;

const
  NCRYPT_SILENT_FLAG = $00000040;

{$IFDEF WINDOWS}
function NCryptUnprotectSecret(
  phDescriptor: PNCRYPT_DESCRIPTOR_HANDLE;
  dwFlags: Cardinal;
  pbProtectedBlob: PByte;
  cbProtectedBlob: Cardinal;
  pMemPara: Pointer;
  hWnd: HWND;
  out ppbData: PByte;
  out pcbData: Cardinal
): LongInt; stdcall; external 'ncrypt.dll';

function NCryptCloseProtectionDescriptor(
  hDescriptor: NCRYPT_DESCRIPTOR_HANDLE
): LongInt; stdcall; external 'ncrypt.dll';

function NCryptGetProtectionDescriptorInfo(
  hDescriptor: NCRYPT_DESCRIPTOR_HANDLE;
  const pMemPara: PNCRYPT_ALLOC_PARA;
  dwInfoType: DWord;
  ppvInfo: Pointer
): LongInt; stdcall; external 'ncrypt.dll';

function DecryptDpapiNgWindows(
  const ProtectedData: TBytes
): TBytes;
{$ENDIF WINDOWS}

implementation

{$IFDEF WINDOWS}
function DecryptDpapiNgWindows(
  const ProtectedData: TBytes
): TBytes;
var
  Descriptor: NCRYPT_DESCRIPTOR_HANDLE;
  Plain: PByte;
  PlainLen: Cardinal;
  Status: LongInt;
begin
  Descriptor := nil;
  Plain := nil;
  PlainLen := 0;

  if Length(ProtectedData) = 0 then
    raise Exception.Create('Empty data.');

  Status := NCryptUnprotectSecret(
    @Descriptor,
    NCRYPT_SILENT_FLAG,
    @ProtectedData[0],
    Length(ProtectedData),
    nil,
    0,
    Plain,
    PlainLen
  );

  if Status <> 0 then
    raise Exception.CreateFmt(
      'NCryptUnprotectSecret failed (%)',
      [Cardinal(Status)]
    );

  try
    SetLength(Result, PlainLen);

    if PlainLen <> 0 then
      Move(Plain^, Result[0], PlainLen);
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

