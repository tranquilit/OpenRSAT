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
    CustomFileName := 'OpenRSAT';
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

function ConfigureLogFamily(
  LogClass: TSynLogClass;
  CustomLog: RawUtf8;
  GlobalLog: RawUtf8;
  DefaultLog: RawUtf8): TSynLogLevels;
begin
  with LogClass.Family do
  begin
    DestinationPath := MakePath([ExtractFileDir(ParamStr(0)), 'log']);
    if not IsDirectoryWritable(DestinationPath) then
      DestinationPath := GetSystemPath(spLog);
    PerThreadLog := ptIdentifiedInOneFile;

    // mORMot recommends initializing the family before using it;
    // set Level last.
    if CustomLog <> '' then
      Level := LogLevelsFromText(CustomLog)
    else if GlobalLog <> '' then
      Level := LogLevelsFromText(GlobalLog)
    else
      Level := LogLevelsFromText(DefaultLog);

    EchoToConsole := Level;
    FileExistsAction := acAppend;

    RotateFileCount := 5;
    RotateFileSizeKB := 4096;
    ArchivePath := MakePath([DestinationPath, 'archive']);

    if sllDebug in Level then
      AutoFlushTimeOut := 1
    else
      AutoFlushTimeOut := 60;
    result := Level;
  end;

  TSynLog.Add.Log(sllInfo, 'Logging % to %%.log', [LogClass.ClassName, LogClass.Family.DestinationPath, LogClass.Family.CustomFileName]);
end;

procedure InitLoggingFromCommandLine;
var
  V, G: RawUtf8;
  Levels: TSynLogLevels;
begin
  Levels := [];
  with Executable.Command do
  begin
    G := Param(['l', 'loglevel', 'log-level'], 'global log #level: off, critical, error, warning, info, debug, trace, all');
    ConfigureLogFamily(TSynLog, '', G, 'warning');
    V := Param(['loglevel-openrsat', 'log-level-openrsat'], 'openrsat log #level');
    Levels += ConfigureLogFamily(TOpenRSATLog, V, G, 'warning');
    V := Param(['loglevel-aduc', 'log-level-aduc'], 'aduc log #level');
    Levels += ConfigureLogFamily(TADUCLog, V, G, 'warning');
    V := Param(['loglevel-addns', 'log-level-addns'], 'addns log #level');
    Levels += ConfigureLogFamily(TADDNSLog, V, G, 'warning');
    V := Param(['loglevel-adss', 'log-level-adss'], 'adss log #level');
    Levels += ConfigureLogFamily(TADSSLog, V, G, 'warning');
    V := Param(['loglevel-adsi', 'log-level-adsi'], 'adsi log #level');
    Levels += ConfigureLogFamily(TADSILog, V, G, 'warning');
    V := Param(['loglevel-ldap', 'log-level-ldap'], 'ldap log #level');
    Levels += ConfigureLogFamily(TLdapLog, V, G, 'warning');
  end;

  if (sllDebug in Levels){$ifdef DEVMODE} or True{$endif} then
  begin
    AllocConsole;
    System.IsConsole := True;
    System.SysInitStdIO;
  end;
end;

initialization
  InitDefaultLogging;

end.


