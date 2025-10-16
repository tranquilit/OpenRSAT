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
  mormot.lib.openssl11,
  mormot.crypt.openssl,
  // Rsat
  uvisopenrsat,
  utranslation;

const
  LOG_TRC = LOG_NFO + [sllTrace];

{$R *.res}

function GetCmdParamsEx(LongName:String;ShortName:String='';DefaultValue:String=''):String;
var
  i: integer;
  S: String;
  found, NextIsValue: Boolean;
begin
  Result := DefaultValue;
  Found := False;
  NextIsValue := False;
  i := 1;

  while (i <= ParamCount) and not Found do
  begin
    S:=ParamStrUTF8(i);
    if NextIsValue then
    begin
      Found := True;
      Result := S;
      Break;
    end;

    if longname<>'' then
      if
          (UTF8CompareStr(Copy(S, 1, Length(LongName)+2), '/'+LongName+'=') = 0) or
          (UTF8CompareStr(Copy(S, 1, Length(LongName)+3), '--'+LongName+'=') = 0) then
      begin
        found := True;
        NextIsValue := False;
        Result:=Copy(S,pos('=',S)+1,MaxInt);
        found := True;
        Break;
      end;

    if shortname<>'' then
      if
          (UTF8CompareStr(Copy(S, 1, 2), '/'+ShortName) = 0) or
          (UTF8CompareStr(Copy(S, 1, 2), '-'+ShortName) = 0) then
      begin
        if length(S)>2 then
        // short form like -ldebug
        begin
          Result:=Copy(S,3,MaxInt);
          found := True;
          Break;
        end
        else
          NextIsValue := True;
      end;

    inc(i);
  end;
end;

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
  loglevel := GetCmdParamsEx('loglevel','l'{$ifdef DEBUG}, 'debug'{$endif});
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
  lang := GetCmdParamsEx('lang');

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
  Application.Scaled:=True;
  Application.Initialize;

  {$IFDEF DEBUG}
  if FileExists('heap.trc') then
    DeleteFile('heap.trc');
  SetHeapTraceOutput('heap.trc');
  {$ENDIF DEBUG}

  {$IFDEF DARWIN}
  if not OpenSslInitialize(MakePath(['lib', 'libcrypto.dylib']), MakePath(['lib', 'libssl.dylib']), '') then
    raise Exception.CreateFmt('Unable to load OpenSSL from %s', [MakePath(['lib', 'libssl.dylib'])]);
  RegisterOpenSsl;
  {$ENDIF}

  OnGetApplicationName := @GetApplicationName;
  Application.CreateForm(TVisOpenRSAT, VisOpenRSAT);
  Application.Run();
end.

