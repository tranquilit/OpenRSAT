program openrsat;

{$mode objfpc}{$H+}

uses
  // Lazarus / fpc
  {$IFDEF UNIX}
    cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
    athreads,
  {$ENDIF}
  datetimectrls,
  Forms,
  Interfaces, // this includes the LCL widgetset
  lazcontrols,
  SysUtils,
  LazUTF8,
  //Submodules
  {$IFDEF WINDOWS} // Adds dark mode support, must be included after the LCL widgetset
    uDarkStyleSchemes,
  {$ENDIF}
  mormot.core.log,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.lib.gssapi,
  mormot.lib.openssl11,
  mormot.crypt.openssl,
  // Rsat
  uvisopenrsat,
  utranslation;

const
  LOG_TRC = LOG_NFO + [sllTrace];
var
  cmd: TExecutableCommandLine;
  {$IFDEF DARWIN}
  AppPath: String;
  {$ENDIF}

{$R *.res}
{$I mormot.defines.inc}

procedure InitLogging(SynlogClass: TSynLogClass=Nil; LogLevel: String=''; AFileName:String='');
begin
  if not Assigned(SynlogClass) then
    SynlogClass := TSynLog;

  if loglevel <> '' then
  begin
    case loglevel of
      'trace': SynlogClass.Family.Level := LOG_TRC;
      'debug': SynlogClass.Family.Level := LOG_ALL;
      'warning': SynlogClass.Family.Level := LOG_WNG;
      'info': SynlogClass.Family.Level := LOG_NFO;
      else SynlogClass.Family.Level := LOG_ERR;
    end;
    SynlogClass.Family.EchoToConsole := TSynLog.Family.Level;
    SynlogClass.Family.PerThreadLog := ptIdentifiedInOneFile;
  end
  else
    SynlogClass.Family.Level := LOG_ERR;

  SynlogClass.Family.RotateFileCount := 5;
  SynlogClass.Family.RotateFileSizeKB := 4096;
  SynlogClass.Family.FileExistsAction := acAppend;

  SynlogClass.Family.DestinationPath := MakePath([ExtractFileDir(ParamStr(0)), 'log']);
  if not IsDirectoryWritable(SynlogClass.Family.DestinationPath) then
    SynlogClass.Family.DestinationPath := GetSystemPath(spLog);
  SynlogClass.Family.ArchivePath := SynlogClass.Family.DestinationPath;

  if AFileName<>'' then
    SynlogClass.Family.CustomFileName := AFileName;

  if loglevel='debug' then
    SynlogClass.Family.AutoFlushTimeOut := 1
  else
    SynlogClass.Family.AutoFlushTimeOut := 60;
  TSynLog.Add.Log(sllInfo, 'Logging % with level=% to %%.log', [SynlogClass.ClassName, loglevel, SynlogClass.Family.DestinationPath, SynlogClass.Family.CustomFileName]);
end;

procedure InitLoggingFromCommandLine;
var
  loglevel: String;
begin
  Executable.Version.RetrieveInformationFromFileName;
  loglevel := Executable.Command.ParamS('loglevel', 'Define the level of log (trace, debug, warning, info)'{$ifdef DEBUG}, 'debug'{$endif});
  {$ifdef DEVMODE}
  AllocConsole;
  System.IsConsole := True; // in System unit
  System.SysInitStdIO;      // in System unit
  {$endif}
  InitLogging(TSynLog, LogLevel);
end;

procedure InitLangFromCommandLine;
var
  lang: String;
begin
  lang := Executable.Command.ParamS('lang');

  ChangeLang(lang);
end;

function GetApplicationName: String;
begin
  result := 'OpenRSAT';
end;

begin
  InitLoggingFromCommandLine;
  TSynLog.Add.Log(sllDebug, 'Logger Initialization');
  RequireDerivedFormResource := True;
  Application.Initialize;

  {$IFDEF DEBUG}
  if FileExists('heap.trc') then
    DeleteFile('heap.trc');
  SetHeapTraceOutput('heap.trc');
  {$ENDIF DEBUG}

  {$IFDEF DARWIN}
  AppPath := ExtractFilePath(ParamStr(0));
  if not OpenSslInitialize(MakePath([AppPath, 'lib', 'libcrypto.dylib']), MakePath([AppPath, 'lib', 'libssl.dylib']), '') then
    raise Exception.CreateFmt('Unable to load OpenSSL from %s', [MakePath([AppPath, 'lib', 'libssl.dylib'])]);
  {$ENDIF}

  cmd := Executable.Command;

  {$IFDEF USE_OPENSSL}
  // refine the OpenSSL library path - RegisterOpenSsl is done in Run method
  OpenSslDefaultCrypto := cmd.ParamS('lib&crypto',   'the OpenSSL libcrypto #filename');
  OpenSslDefaultSsl    := cmd.ParamS('lib&ssl',      'the OpenSSL libssl #filename');
  OpenSslDefaultPath   := cmd.ParamS('openssl&path', 'the OpenSSL library #path');
  {$ENDIF USE_OPENSSL}
  {$IFDEF OSPOSIX}
  GssLib_Custom := cmd.ParamS('lib&krb5', 'the Kerberos libgssapi #filename');
  {$ENDIF OSPOSIX}
  RegisterOpenSsl;

  OnGetApplicationName := @GetApplicationName;
  Application.CreateForm(TVisOpenRSAT, VisOpenRSAT);
  Application.Run();
end.

