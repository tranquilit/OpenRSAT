unit ulog;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.base,
  mormot.core.log,
  mormot.core.os;

type
  TOpenRSATLog = class(TSynLog);
  TADUCLog = class(TSynLog);
  TADDNSLog = class(TSynLog);
  TADSSLog = class(TSynLog);
  TADSILog = class(TSynLog);
  TLdapLog = class(TSynLog);

procedure InitLoggingFromCommandLine;

implementation

uses
  mormot.core.text,
  mormot.core.unicode;

procedure InitDefaultLogging(LogClass: TSynLogClass; DestPath: TFileName);
begin
  With LogClass.Family do
  begin
    DestinationPath := DestPath;
    CustomFileName := LogClass.ClassName;
    PerThreadLog := ptIdentifiedInOneFile;
    Level := LOG_WNG;
  end;
end;

procedure InitDefaultLogging;
var
  DestPath: TFileName;
  LogClass: TSynLogClass;
begin
  DestPath := MakePath([ExtractFileDir(ParamStr(0)), 'log']);
  if not IsDirectoryWritable(DestPath) then
    DestPath := GetSystemPath(spLog);

  for LogClass in [TSynLog, TOpenRSATLog, TADUCLog, TADDNSLog, TADSSLog, TADSILog, TLdapLog] do
    InitDefaultLogging(LogClass, DestPath);
end;

function LogLevelsFromText(const Text: RawUtf8): TSynLogLevels;
var
  S: string;
begin
  S := LowerCase(Trim(Text));

  if FindRawUtf8(['', 'none', 'off', 'silent', '0'], S) <> -1 then
    Result := []
  else if FindRawUtf8(['critical', 'crit', 'fatal', '1'], S) <> -1 then
    Result := LOG_CRI
  else if FindRawUtf8(['error', 'err', '2'], S) <> -1 then
    Result := LOG_ERR
  else if FindRawUtf8(['warning', 'warn', 'w', '3'], S) <> -1 then
    Result := LOG_WNG
  else if FindRawUtf8(['info', 'information', '4'], S) <> -1 then
    Result := LOG_NFO
  else if FindRawUtf8(['debug', 'dbg', '5'], S) <> -1 then
    Result := LOG_NFO + [sllDebug]
  else if FindRawUtf8(['trace', 'trc', '6'], S) <> -1 then
    Result := LOG_NFO + [sllDebug, sllTrace]
  else if FindRawUtf8(['verbose', 'all', '*'], S) <> -1 then
    Result := LOG_VERBOSE
  else
    raise Exception.CreateFmt('Invalid log level "%s"', [Text]);

  if FindRawUtf8(['trace', 'trc', 'verbose', 'all', '*'], S) <> -1 then
    Result := Result + [sllEnter, sllLeave];
end;

procedure ConfigureLogFamily(
  LogClass: TSynLogClass;
  const FileBaseName: TFileName;
  Levels: TSynLogLevels);
begin
  with LogClass.Family do
  begin
    DestinationPath := MakePath([ExtractFileDir(ParamStr(0)), 'log']);
    if not IsDirectoryWritable(DestinationPath) then
      DestinationPath := GetSystemPath(spLog);
    CustomFileName := FileBaseName;
    PerThreadLog := ptIdentifiedInOneFile;

    // mORMot recommends initializing the family before using it;
    // set Level last.
    Level := Levels;
    EchoToConsole := Levels;
    FileExistsAction := acAppend;

  if sllDebug in Levels then
    AutoFlushTimeOut := 1
  else
    AutoFlushTimeOut := 60;
  end;

  TSynLog.Add.Log(sllInfo, 'Logging % to %%.log', [LogClass.ClassName, LogClass.Family.DestinationPath, LogClass.Family.CustomFileName]);
end;

procedure InitLoggingFromCommandLine;
var
  V: RawUtf8;
  OpenRSATLevels, ADUCLevels, ADDNSLevels, ADSSLevels, ADSILevels, LDAPLevels, GlobalLevels: TSynLogLevels;
begin
  GlobalLevels := LogLevelsFromText('warning');
  OpenRSATLevels := LogLevelsFromText('warning');
  ADUCLevels := LogLevelsFromText('warning');
  ADDNSLevels := LogLevelsFromText('warning');
  ADSSLevels := LogLevelsFromText('warning');
  ADSILevels := LogLevelsFromText('warning');
  LDAPLevels := LogLevelsFromText('warning');

  if Executable.Command.Get(['l', 'loglevel', 'log-level'], V,
     'global log #level: off, critical, error, warning, info, debug, trace, all') then
  begin
    GlobalLevels := LogLevelsFromText(V);
    OpenRSATLevels := LogLevelsFromText(V);
    ADUCLevels := LogLevelsFromText(V);
    ADDNSLevels := LogLevelsFromText(V);
    ADSSLevels := LogLevelsFromText(V);
    ADSILevels := LogLevelsFromText(V);
    LDAPLevels := LogLevelsFromText(V);
  end;

  // Family-specific overrides:
  if Executable.Command.Get(['loglevel-openrsat', 'log-level-openrsat'], V, 'openrsat log #level') then
    OpenRSATLevels := LogLevelsFromText(V);
  if Executable.Command.Get(['loglevel-aduc', 'log-level-aduc'], V, 'aduc log #level') then
    ADUCLevels := LogLevelsFromText(V);
  if Executable.Command.Get(['loglevel-addns', 'log-level-addns'], V, 'addns log #level') then
    ADDNSLevels := LogLevelsFromText(V);
  if Executable.Command.Get(['loglevel-adss', 'log-level-adss'], V, 'adss log #level') then
    ADSSLevels := LogLevelsFromText(V);
  if Executable.Command.Get(['loglevel-adsi', 'log-level-adsi'], V, 'adsi log #level') then
    ADSILevels := LogLevelsFromText(V);
  if Executable.Command.Get(['loglevel-ldap', 'log-level-ldap'], V, 'ldap log #level') then
    LDAPLevels := LogLevelsFromText(V);

  if (sllDebug in (GlobalLevels + OpenRSATLevels + ADUCLevels + ADDNSLevels + ADSSLevels + ADSILevels + LDAPLevels))
    {$ifdef DEVMODE} or True{$endif} then
  begin
    AllocConsole;
    System.IsConsole := True;
    System.SysInitStdIO;
  end;

  ConfigureLogFamily(TSynLog, 'OpenRSAT', GlobalLevels);
  ConfigureLogFamily(TOpenRSATLog, 'OpenRSAT', OpenRSATLevels);
  ConfigureLogFamily(TADUCLog, 'aduc', ADUCLevels);
  ConfigureLogFamily(TADDNSLog, 'addns', ADDNSLevels);
  ConfigureLogFamily(TADSSLog, 'adss', ADSSLevels);
  ConfigureLogFamily(TADSILog, 'adsi', ADSILevels);
  ConfigureLogFamily(TLdapLog, 'ldap', LDAPLevels);
end;

initialization
  InitDefaultLogging;

end.


