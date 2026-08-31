unit ugptcore;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  {$IFDEF WINDOWS}
  Windows,
  jwawinnetwk,
  {$ENDIF}
  Process,
  mormot.core.base,
  mormot.core.log,
  mormot.net.ldap,
  ugpocore,
  ugpregpol,
  ulog;

type

  { TGPTFileInfo }

  /// A file of the Group Policy Template (GPT), in the SYSVOL.
  TGPTFileInfo = record
    /// Relative path inside the GPT (e.g. User\Registry.pol)
    Path: RawUtf8;
    /// Size of the file, in bytes
    Size: Int64;
    /// Last modification date/time of the file
    Modified: RawUtf8;
  end;
  TGPTFileInfoDynArray = array of TGPTFileInfo;

  { TGptIni }

  /// The [General] section of a GPT.INI file, as stored in the SYSVOL.
  /// The Has* flags tell whether the field was present in the file.
  TGptIni = record
    /// GPO version (same value as the versionNumber GPC attribute)
    Version: Cardinal;
    /// Display name of the GPO
    DisplayName: RawUtf8;
    /// GPO options (same value as the flags GPC attribute)
    Options: Integer;
    HasVersion: Boolean;
    HasDisplayName: Boolean;
    HasOptions: Boolean;
  end;

  { TGPTCore }

  /// Access to the Group Policy Template (GPT) of a GPO, hosted on the
  /// SYSVOL file share. Uses the native Windows file API (UNC) on Windows,
  /// and the smbclient command line tool on Linux and macOS.
  /// The LastError property reports the real reason of a failure.
  TGPTCore = class
  private
    fLog: TSynLogClass;
    fLdapClient: TLdapClient;
    fLastError: RawUtf8;

    function GetDomainName: RawUtf8;
    function GetTargetHost: RawUtf8;
    function GetShareUrl(const AUseKerberos: Boolean): RawUtf8;
    function GetRelativePath(const AGPO: TGPO): RawUtf8;
    function GetCredentials: RawUtf8;
    function GetUserName: RawUtf8;
    function GetPassword: RawUtf8;
    function FindSmbClient: RawUtf8;
    function RunSmbClient(const AShareUrl, ACommand: RawUtf8;
      out AOutput: RawUtf8): Boolean;
    function WriteTempFile(const AContent: RawByteString;
      out ATempFile: RawUtf8): Boolean;
    {$IFDEF WINDOWS}
    function GetUncShareRoot: RawUtf8;
    function GetUncRoot(const AGPO: TGPO): RawUtf8;
    function EnsureShareConnected: Boolean;
    function ListFilesUnc(AGPO: TGPO; out AFiles: TGPTFileInfoDynArray): Boolean;
    function ReadFileUnc(AGPO: TGPO; const ARelativePath: RawUtf8;
      out AContent: RawUtf8): Boolean;
    function WriteFileUnc(AGPO: TGPO; const ARelativePath: RawUtf8;
      const AContent: RawByteString): Boolean;
    function DeleteFileUnc(AGPO: TGPO; const ARelativePath: RawUtf8): Boolean;
    function CreateDirectoryUnc(AGPO: TGPO; const ARelativePath: RawUtf8): Boolean;
    {$ENDIF}
  public
    constructor Create(ALdapClient: TLdapClient);
    destructor Destroy; override;

    /// Last error message of the last failed operation (empty on success).
    property LastError: RawUtf8 read fLastError;

    /// List the files of the GPT of a GPO. Returns False when the GPT is not
    /// reachable (no smbclient, no credentials, authentication rejected...).
    function ListFiles(AGPO: TGPO; out AFiles: TGPTFileInfoDynArray): Boolean;

    /// Read the raw content of a file of the GPT (e.g. GPT.INI).
    /// Returns False when the file cannot be read.
    function ReadFile(AGPO: TGPO; const ARelativePath: RawUtf8;
      out AContent: RawUtf8): Boolean;

    /// Write the raw content of a file inside the GPT (e.g. GPT.INI).
    /// Creates the file or overwrites it. Returns False on failure.
    function WriteFile(AGPO: TGPO; const ARelativePath: RawUtf8;
      const AContent: RawByteString): Boolean;

    /// Delete a file inside the GPT. Returns False on failure.
    function DeleteFile(AGPO: TGPO; const ARelativePath: RawUtf8): Boolean;

    /// Create a directory inside the GPT. Returns False on failure.
    function CreateDirectory(AGPO: TGPO; const ARelativePath: RawUtf8): Boolean;

    /// Read and parse the GPT.INI file of a GPO.
    function ReadGptIni(AGPO: TGPO; out AGptIni: TGptIni): Boolean;

    /// Write a GPT.INI file (from the [General] fields) inside the GPT.
    function UpdateGptIni(AGPO: TGPO; const AGptIni: TGptIni): Boolean;

    /// Read and parse the Registry.pol of a side (User or Machine) of a GPO.
    /// Returns False when the file does not exist or cannot be read.
    /// The caller owns the returned TGPRegPol instance.
    function ReadRegistryPol(AGPO: TGPO; AUserSide: Boolean;
      out APol: TGPRegPol): Boolean;

    /// Write a Registry.pol of a side (User or Machine) of a GPO.
    function WriteRegistryPol(AGPO: TGPO; AUserSide: Boolean;
      const APol: TGPRegPol): Boolean;
  end;

/// Relative path of the Registry.pol of a side (User or Machine).
function GetRegistryPolPath(AUserSide: Boolean): RawUtf8;

/// Parse the output of the smbclient "recurse; ls" command into file entries.
function ParseSmbListOutput(const AOutput: RawUtf8): TGPTFileInfoDynArray;

/// Parse the [General] section of a GPT.INI content into fields.
function ParseGptIni(const AContent: RawUtf8): TGptIni;

/// Serialize a TGptIni record into a GPT.INI [General] section text.
function GptIniToText(const AGptIni: TGptIni): RawUtf8;

implementation

uses
  mormot.core.text,
  mormot.core.unicode;

{$IFDEF WINDOWS}
{ Windows native transport (UNC) }

function TGPTCore.GetUncShareRoot: RawUtf8;
begin
  result := '';
  if (GetTargetHost = '') then
    Exit;
  // \\host\SysVol
  result := FormatUtf8('\\%\%', [GetTargetHost, 'SysVol']);
end;

function TGPTCore.GetUncRoot(const AGPO: TGPO): RawUtf8;
begin
  result := '';
  if not Assigned(AGPO) then
    Exit;
  // \\host\SysVol\<domain>\Policies\{GUID}
  result := FormatUtf8('%\%\Policies\%',
    [GetUncShareRoot, GetDomainName, AGPO.Name]);
end;

function TGPTCore.EnsureShareConnected: Boolean;
var
  NetResource: TNetResource;
  UncShareRoot: RawUtf8;
  RemoteName, UserName, Password: WideString;
  Err: DWORD;
begin
  result := True;
  UncShareRoot := GetUncShareRoot;
  if (UncShareRoot = '') then
  begin
    fLastError := 'No SYSVOL share URL available.';
    Exit(False);
  end;

  // Already connected (domain-joined machine, session credentials)?
  if DirectoryExists(UncShareRoot) then
    Exit;

  // Connect with the LDAP credentials of the application. Keep the
  // WideStrings alive while the NetResource record is used.
  RemoteName := UTF8Decode(UncShareRoot);
  UserName := UTF8Decode(GetUserName);
  Password := UTF8Decode(GetPassword);

  FillChar(NetResource, SizeOf(NetResource), 0);
  NetResource.dwType := RESOURCETYPE_DISK;
  NetResource.lpLocalName := nil;
  NetResource.lpRemoteName := PWideChar(RemoteName);
  NetResource.lpProvider := nil;

  Err := WNetAddConnection2W(NetResource, PWideChar(Password),
    PWideChar(UserName), CONNECT_TEMPORARY);

  if (Err = NO_ERROR) then
    Exit;

  fLastError := FormatUtf8('Unable to connect to "%" (error %).',
    [UncShareRoot, Err]);
  result := False;
end;

function TGPTCore.ListFilesUnc(AGPO: TGPO; out AFiles: TGPTFileInfoDynArray): Boolean;
var
  Root: RawUtf8;

  procedure Walk(const ADirectory: RawUtf8);
  var
    SearchRec: TSearchRec;
    Relative: RawUtf8;
  begin
    if FindFirst(ADirectory + '\*', faAnyFile, SearchRec) <> 0 then
    begin
      FindClose(SearchRec);
      Exit;
    end;
    try
      repeat
        if (SearchRec.Name = '.') or (SearchRec.Name = '..') then
          Continue;

        Relative := Copy(ADirectory, Length(Root) + 2, MaxInt);
        if (Relative <> '') then
          Relative := Relative + '\';
        Relative := Relative + SearchRec.Name;

        if (SearchRec.Attr and faDirectory) <> 0 then
          Walk(ADirectory + '\' + SearchRec.Name)
        else
        begin
          SetLength(AFiles, Length(AFiles) + 1);
          AFiles[High(AFiles)].Path := StringToUtf8(Relative);
          AFiles[High(AFiles)].Size := SearchRec.Size;
          AFiles[High(AFiles)].Modified := StringToUtf8(DateTimeToStr(
            FileDateToDateTime(SearchRec.Time)));
        end;
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  end;
begin
  result := False;
  AFiles := nil;

  if not EnsureShareConnected then
    Exit;

  Root := GetUncRoot(AGPO);
  if (Root = '') then
    Exit;

  Walk(Root);
  result := True;
end;

function TGPTCore.ReadFileUnc(AGPO: TGPO; const ARelativePath: RawUtf8;
  out AContent: RawUtf8): Boolean;
var
  FullPath: RawUtf8;
begin
  result := False;
  AContent := '';

  if not EnsureShareConnected then
    Exit;

  FullPath := GetUncRoot(AGPO) + '\' + ARelativePath;
  if not FileExists(FullPath) then
  begin
    fLastError := FormatUtf8('The file "%" does not exist.', [FullPath]);
    Exit;
  end;

  try
    with TFileStream.Create(FullPath, fmOpenRead or fmShareDenyNone) do
    try
      SetLength(AContent, Size);
      Read(Pointer(AContent)^, Size);
    finally
      Free;
    end;
  except
    on E: Exception do
    begin
      fLastError := E.Message;
      Exit;
    end;
  end;

  result := True;
end;

function TGPTCore.WriteFileUnc(AGPO: TGPO; const ARelativePath: RawUtf8;
  const AContent: RawByteString): Boolean;
var
  FullPath, DirectoryPath: RawUtf8;
begin
  result := False;

  if not EnsureShareConnected then
    Exit;

  FullPath := GetUncRoot(AGPO) + '\' + ARelativePath;
  DirectoryPath := ExtractFilePath(FullPath);
  if (DirectoryPath <> '') and not DirectoryExists(DirectoryPath) then
    ForceDirectories(DirectoryPath);

  try
    with TFileStream.Create(FullPath, fmCreate or fmShareDenyNone) do
    try
      WriteBuffer(Pointer(AContent)^, Length(AContent));
    finally
      Free;
    end;
  except
    on E: Exception do
    begin
      fLastError := E.Message;
      Exit;
    end;
  end;

  result := True;
end;

function TGPTCore.DeleteFileUnc(AGPO: TGPO; const ARelativePath: RawUtf8): Boolean;
var
  FullPath: RawUtf8;
begin
  result := False;

  if not EnsureShareConnected then
    Exit;

  FullPath := GetUncRoot(AGPO) + '\' + ARelativePath;
  if not SysUtils.DeleteFile(FullPath) then
  begin
    fLastError := FormatUtf8('Unable to delete "%".', [FullPath]);
    Exit;
  end;

  result := True;
end;

function TGPTCore.CreateDirectoryUnc(AGPO: TGPO; const ARelativePath: RawUtf8): Boolean;
var
  FullPath: RawUtf8;
begin
  result := False;

  if not EnsureShareConnected then
    Exit;

  FullPath := GetUncRoot(AGPO) + '\' + ARelativePath;
  if not ForceDirectories(FullPath) then
  begin
    fLastError := FormatUtf8('Unable to create the directory "%".', [FullPath]);
    Exit;
  end;

  result := True;
end;
{$ENDIF}

{ Parsing helpers }

function ParseSmbListOutput(const AOutput: RawUtf8): TGPTFileInfoDynArray;
var
  Lines: TStringList;
  i, SpacePos, TypePos: Integer;
  Line, Rest, Name, Size, Modified: RawUtf8;
  FileSize: Int64;
begin
  result := nil;

  Lines := TStringList.Create;
  try
    Lines.Text := UTF8ToString(AOutput);
    for i := 0 to Lines.Count - 1 do
    begin
      Line := Trim(Lines[i]);
      if (Line = '') then
        Continue;

      // Each entry looks like:
      //   "name <spaces> D|A <spaces> size <date>"
      TypePos := PosEx('  D ', Line);
      if (TypePos = 0) then
        TypePos := PosEx('  A ', Line);
      if (TypePos = 0) then
        Continue;

      Name := Trim(Copy(Line, 1, TypePos));
      if (Name = '.') or (Name = '..') then
        Continue;

      // Rest starts with the right-aligned size column, then the date.
      Rest := Trim(Copy(Line, TypePos + 3, MaxInt));
      SpacePos := PosEx('  ', Rest);
      if (SpacePos = 0) then
        Continue;

      Size := Trim(Copy(Rest, 1, SpacePos));
      Modified := Trim(Copy(Rest, SpacePos + 2, MaxInt));

      FileSize := 0;
      TryStrToInt64(Size, FileSize);

      SetLength(result, Length(result) + 1);
      result[High(result)].Path := StringToUtf8(Name);
      result[High(result)].Size := FileSize;
      result[High(result)].Modified := StringToUtf8(Modified);
    end;
  finally
    Lines.Free;
  end;
end;

{ TGPTCore }

function TGPTCore.GetDomainName: RawUtf8;
begin
  result := GetDomainNameFromDN(fLdapClient.DefaultDN);
end;

function TGPTCore.GetTargetHost: RawUtf8;
begin
  // The Domain Controller the application already talks to via LDAP.
  result := fLdapClient.Settings.TargetHost;
  if (result = '') then
    result := GetDomainName;
end;

function TGPTCore.GetShareUrl(const AUseKerberos: Boolean): RawUtf8;
var
  Host: RawUtf8;
begin
  result := '';

  // Kerberos requires the DNS name of the DC (SPN matching), while
  // NTLM (user%password) works with the IP address as well.
  if AUseKerberos then
    Host := GetDomainName
  else
    Host := GetTargetHost;

  if (Host = '') then
    Exit;

  // smbclient //host/SysVol
  result := FormatUtf8('//%\%', [Host, 'SysVol']);
end;

function TGPTCore.GetRelativePath(const AGPO: TGPO): RawUtf8;
begin
  result := '';
  if not Assigned(AGPO) then
    Exit;
  // Inside the SysVol share, the GPT lives in <domain>\Policies\{GUID}
  result := FormatUtf8('%\Policies\%', [GetDomainName, AGPO.Name]);
end;

function TGPTCore.GetUserName: RawUtf8;
begin
  result := fLdapClient.Settings.UserName;
end;

function TGPTCore.GetPassword: RawUtf8;
begin
  result := fLdapClient.Settings.Password;
end;

function TGPTCore.GetCredentials: RawUtf8;
begin
  result := '';
  if (GetUserName <> '') then
    result := GetUserName + '%' + GetPassword;
end;

function TGPTCore.FindSmbClient: RawUtf8;
var
  PathEnv, Dir: RawUtf8;
  Start, Sep: Integer;
begin
  result := '';

  PathEnv := GetEnvironmentVariable('PATH');
  Start := 1;
  repeat
    Sep := PosEx(':', PathEnv, Start);
    if (Sep = 0) then
      Dir := Copy(PathEnv, Start, MaxInt)
    else
    begin
      Dir := Copy(PathEnv, Start, Sep - Start);
      Start := Sep + 1;
    end;

    if (Dir <> '') then
    begin
      result := IncludeTrailingPathDelimiter(Dir) + 'smbclient';
      if FileExists(result) then
        Exit;
    end;
  until (Sep = 0);

  result := '';
end;

function TGPTCore.RunSmbClient(const AShareUrl, ACommand: RawUtf8;
  out AOutput: RawUtf8): Boolean;
var
  Process: TProcess;
  OutputStream: TStringStream;
  ExePath: RawUtf8;
  Credentials: RawUtf8;
  UseKerberos: Boolean;
begin
  result := False;
  AOutput := '';
  fLastError := '';

  if (AShareUrl = '') then
  begin
    fLastError := 'No SYSVOL share URL available.';
    if Assigned(fLog) then
      fLog.Add.Log(sllError, '%', [fLastError], Self);
    Exit;
  end;

  // smbclient must be installed and reachable through the PATH.
  ExePath := FindSmbClient;
  if (ExePath = '') then
  begin
    fLastError := 'smbclient not found in the PATH. Install the samba-client package.';
    if Assigned(fLog) then
      fLog.Add.Log(sllError, '%', [fLastError], Self);
    Exit;
  end;

  Credentials := GetCredentials;
  UseKerberos := (Credentials = '');

  Process := TProcess.Create(nil);
  OutputStream := TStringStream.Create('');
  try
    Process.Executable := ExePath;
    Process.Parameters.Add(AShareUrl);
    if UseKerberos then
    begin
      // Kerberos SSO: use the ticket of the current user (kinit).
      Process.Parameters.Add('-k');
    end
    else
    begin
      Process.Parameters.Add('-U');
      Process.Parameters.Add(Credentials);
    end;
    Process.Parameters.Add('-c');
    Process.Parameters.Add(ACommand);
    Process.Options := [poUsePipes, poStderrToOutPut];
    Process.ShowWindow := swoHIDE;

    try
      Process.Execute;
      OutputStream.CopyFrom(Process.Output, 0);
      Process.WaitOnExit;
    except
      on E: Exception do
      begin
        fLastError := E.Message;
        if Assigned(fLog) then
          fLog.Add.Log(sllError, 'smbclient execution failed: %', [E.Message], Self);
        Exit;
      end;
    end;

    AOutput := OutputStream.DataString;
    if (Process.ExitStatus <> 0) then
    begin
      // Report the real smbclient error (stderr is captured in AOutput).
      fLastError := Trim(AOutput);
      if (fLastError = '') then
        fLastError := FormatUtf8('smbclient failed with exit code %',
          [Process.ExitStatus]);
      if Assigned(fLog) then
        fLog.Add.Log(sllError, 'smbclient: %', [fLastError], Self);
      Exit;
    end;

    result := True;
  finally
    OutputStream.Free;
    Process.Free;
  end;
end;

function TGPTCore.ListFiles(AGPO: TGPO; out AFiles: TGPTFileInfoDynArray): Boolean;
var
  Output, ShareUrl, RelativePath, Command: RawUtf8;
begin
  result := False;
  AFiles := nil;

  if not Assigned(AGPO) then
    Exit;

  {$IFDEF WINDOWS}
  result := ListFilesUnc(AGPO, AFiles);
  {$ELSE}
  ShareUrl := GetShareUrl(GetCredentials = '');
  RelativePath := GetRelativePath(AGPO);
  if (ShareUrl = '') or (RelativePath = '') then
    Exit;

  Command := FormatUtf8('cd "%"; recurse; ls', [RelativePath]);
  if not RunSmbClient(ShareUrl, Command, Output) then
  begin
    if Assigned(fLog) then
      fLog.Add.Log(sllError, 'Unable to list the GPT of "%"', [AGPO.DisplayName], Self);
    Exit;
  end;

  AFiles := ParseSmbListOutput(Output);
  result := True;
  {$ENDIF}
end;

function TGPTCore.ReadFile(AGPO: TGPO; const ARelativePath: RawUtf8;
  out AContent: RawUtf8): Boolean;
var
  Output, ShareUrl, RelativePath, Command: RawUtf8;
  TempFile: RawUtf8;
begin
  result := False;
  AContent := '';

  if not Assigned(AGPO) or (ARelativePath = '') then
    Exit;

  {$IFDEF WINDOWS}
  result := ReadFileUnc(AGPO, ARelativePath, AContent);
  {$ELSE}
  ShareUrl := GetShareUrl(GetCredentials = '');
  RelativePath := GetRelativePath(AGPO);
  if (ShareUrl = '') or (RelativePath = '') then
    Exit;

  TempFile := GetTempDir + 'openrsat-gpt-' + AGPO.Name + '-' +
    IntToStr(GetProcessID) + '.tmp';
  SysUtils.DeleteFile(TempFile);

  Command := FormatUtf8('cd "%"; get "%" "%"', [RelativePath, ARelativePath, TempFile]);
  if not RunSmbClient(ShareUrl, Command, Output) then
    Exit;

  if not FileExists(TempFile) then
  begin
    fLastError := FormatUtf8('The file "%" was not downloaded by smbclient.',
      [ARelativePath]);
    Exit;
  end;

  try
    with TFileStream.Create(TempFile, fmOpenRead or fmShareDenyNone) do
    try
      SetLength(AContent, Size);
      Read(Pointer(AContent)^, Size);
    finally
      Free;
    end;
  finally
    SysUtils.DeleteFile(TempFile);
  end;

  result := True;
  {$ENDIF}
end;

{ TGPTCore }

function TGPTCore.WriteTempFile(const AContent: RawByteString;
  out ATempFile: RawUtf8): Boolean;
begin
  result := False;
  ATempFile := '';

  ATempFile := GetTempDir + 'openrsat-put-' + IntToStr(GetProcessID) + '-' +
    IntToStr(GetTickCount64) + '.tmp';
  SysUtils.DeleteFile(ATempFile);

  try
    with TFileStream.Create(ATempFile, fmCreate or fmShareDenyNone) do
    try
      WriteBuffer(Pointer(AContent)^, Length(AContent));
    finally
      Free;
    end;
  except
    on E: Exception do
    begin
      fLastError := E.Message;
      if Assigned(fLog) then
        fLog.Add.Log(sllError, 'Unable to write temporary file: %', [E.Message], Self);
      Exit;
    end;
  end;

  result := True;
end;

function TGPTCore.WriteFile(AGPO: TGPO; const ARelativePath: RawUtf8;
  const AContent: RawByteString): Boolean;
var
  Output, ShareUrl, RelativePath, Command, TempFile: RawUtf8;
begin
  result := False;

  if not Assigned(AGPO) or (ARelativePath = '') then
    Exit;

  {$IFDEF WINDOWS}
  result := WriteFileUnc(AGPO, ARelativePath, AContent);
  {$ELSE}
  ShareUrl := GetShareUrl(GetCredentials = '');
  RelativePath := GetRelativePath(AGPO);
  if (ShareUrl = '') or (RelativePath = '') then
    Exit;

  if not WriteTempFile(AContent, TempFile) then
    Exit;

  try
    Command := FormatUtf8('cd "%"; put "%" "%"',
      [RelativePath, TempFile, ARelativePath]);
    if not RunSmbClient(ShareUrl, Command, Output) then
    begin
      if Assigned(fLog) then
        fLog.Add.Log(sllError, 'Unable to write "%" of GPO "%"',
          [ARelativePath, AGPO.DisplayName], Self);
      Exit;
    end;
    result := True;
  finally
    SysUtils.DeleteFile(TempFile);
  end;
  {$ENDIF}
end;

function TGPTCore.DeleteFile(AGPO: TGPO; const ARelativePath: RawUtf8): Boolean;
var
  Output, ShareUrl, RelativePath, Command: RawUtf8;
begin
  result := False;

  if not Assigned(AGPO) or (ARelativePath = '') then
    Exit;

  {$IFDEF WINDOWS}
  result := DeleteFileUnc(AGPO, ARelativePath);
  {$ELSE}
  ShareUrl := GetShareUrl(GetCredentials = '');
  RelativePath := GetRelativePath(AGPO);
  if (ShareUrl = '') or (RelativePath = '') then
    Exit;

  Command := FormatUtf8('cd "%"; del "%"', [RelativePath, ARelativePath]);
  if not RunSmbClient(ShareUrl, Command, Output) then
    Exit;

  result := True;
  {$ENDIF}
end;

function TGPTCore.CreateDirectory(AGPO: TGPO; const ARelativePath: RawUtf8): Boolean;
var
  Output, ShareUrl, RelativePath, Command: RawUtf8;
begin
  result := False;

  if not Assigned(AGPO) or (ARelativePath = '') then
    Exit;

  {$IFDEF WINDOWS}
  result := CreateDirectoryUnc(AGPO, ARelativePath);
  {$ELSE}
  ShareUrl := GetShareUrl(GetCredentials = '');
  RelativePath := GetRelativePath(AGPO);
  if (ShareUrl = '') or (RelativePath = '') then
    Exit;

  Command := FormatUtf8('cd "%"; mkdir "%"', [RelativePath, ARelativePath]);
  if not RunSmbClient(ShareUrl, Command, Output) then
    Exit;

  result := True;
  {$ENDIF}
end;

function TGPTCore.ReadGptIni(AGPO: TGPO; out AGptIni: TGptIni): Boolean;
var
  Content: RawUtf8;
begin
  result := False;

  if not Assigned(AGPO) then
    Exit;

  if not ReadFile(AGPO, 'GPT.INI', Content) then
    Exit;

  AGptIni := ParseGptIni(Content);
  result := True;
end;

function TGPTCore.UpdateGptIni(AGPO: TGPO; const AGptIni: TGptIni): Boolean;
begin
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, 'Update GPT.INI of GPO "%"', [AGPO.DisplayName], Self);

  result := WriteFile(AGPO, 'GPT.INI', GptIniToText(AGptIni));
end;

function GetRegistryPolPath(AUserSide: Boolean): RawUtf8;
begin
  if AUserSide then
    result := 'User\Registry.pol'
  else
    result := 'Machine\Registry.pol';
end;

function TGPTCore.ReadRegistryPol(AGPO: TGPO; AUserSide: Boolean;
  out APol: TGPRegPol): Boolean;
var
  Content: RawUtf8;
begin
  result := False;
  APol := nil;

  if not Assigned(AGPO) then
    Exit;

  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, 'Read % Registry.pol of GPO "%"',
      [GetRegistryPolPath(AUserSide), AGPO.DisplayName], Self);

  if not ReadFile(AGPO, GetRegistryPolPath(AUserSide), Content) then
    Exit;

  try
    APol := TGPRegPol.LoadFromBytes(Content);
  except
    on E: EGPOException do
    begin
      fLastError := E.Message;
      if Assigned(fLog) then
        fLog.Add.Log(sllError, 'Registry.pol of GPO "%": %',
          [AGPO.DisplayName, E.Message], Self);
      Exit;
    end;
  end;

  result := True;
end;

function TGPTCore.WriteRegistryPol(AGPO: TGPO; AUserSide: Boolean;
  const APol: TGPRegPol): Boolean;
begin
  result := False;

  if not Assigned(AGPO) or not Assigned(APol) then
    Exit;

  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, 'Write % Registry.pol of GPO "%"',
      [GetRegistryPolPath(AUserSide), AGPO.DisplayName], Self);

  result := WriteFile(AGPO, GetRegistryPolPath(AUserSide), APol.SaveToBytes);
end;

function ParseGptIni(const AContent: RawUtf8): TGptIni;
var
  Lines: TStringList;
  i, EqPos: Integer;
  Line, Key, Value: RawUtf8;
  InGeneral: Boolean;
begin
  result.Version := 0;
  result.DisplayName := '';
  result.Options := 0;
  result.HasVersion := False;
  result.HasDisplayName := False;
  result.HasOptions := False;

  InGeneral := False;
  Lines := TStringList.Create;
  try
    Lines.Text := UTF8ToString(AContent);
    for i := 0 to Lines.Count - 1 do
    begin
      Line := Trim(Lines[i]);
      if (Line = '') then
        Continue;

      if (Line[1] = '[') then
      begin
        InGeneral := SameText(Line, '[General]');
        Continue;
      end;

      if not InGeneral then
        Continue;

      EqPos := PosEx('=', Line);
      if (EqPos = 0) then
        Continue;

      Key := Trim(Copy(Line, 1, EqPos - 1));
      Value := Trim(Copy(Line, EqPos + 1, MaxInt));

      if SameText(Key, 'Version') then
        result.HasVersion := TryStrToDWord(Value, result.Version)
      else if SameText(Key, 'DisplayName') then
      begin
        result.HasDisplayName := True;
        result.DisplayName := StringToUtf8(Value);
      end
      else if SameText(Key, 'Options') then
        result.HasOptions := TryStrToInt(Value, result.Options);
    end;
  finally
    Lines.Free;
  end;
end;

function GptIniToText(const AGptIni: TGptIni): RawUtf8;
begin
  result := '[General]' + #13#10;
  if AGptIni.HasVersion then
    result := result + FormatUtf8('Version=%' + #13#10, [AGptIni.Version]);
  if AGptIni.HasDisplayName then
    result := result + FormatUtf8('DisplayName=%' + #13#10, [AGptIni.DisplayName]);
  if AGptIni.HasOptions then
    result := result + FormatUtf8('Options=%' + #13#10, [AGptIni.Options]);
end;

constructor TGPTCore.Create(ALdapClient: TLdapClient);
begin
  fLog := TOpenRSATLog;
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, 'Create', Self);

  fLdapClient := ALdapClient;
end;

destructor TGPTCore.Destroy;
begin
  inherited Destroy;
end;

end.