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
  controls,
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
  ulog;

var
  cmd: TExecutableCommandLine;
  {$IFDEF DARWIN}
  AppPath: String;
  {$ENDIF}

{$R *.res}
{$I mormot.defines.inc}

function GetApplicationName: String;
begin
  result := 'OpenRSAT';
end;

function ParseArgs(out AOpenRSATArgs: TOpenRSATArgs): Boolean;
begin
  result := False;

  FillChar(AOpenRSATArgs, SizeOf(AOpenRSATArgs), 0);

  With Executable.Command do
  begin
    ExeDescription := 'OpenRSAT';
    {$IFDEF USE_OPENSSL}
    // refine the OpenSSL library path - RegisterOpenSsl is done in Run method
    OpenSslDefaultCrypto := ParamS('lib&crypto',   'the OpenSSL libcrypto #filename');
    OpenSslDefaultSsl    := ParamS('lib&ssl',      'the OpenSSL libssl #filename');
    OpenSslDefaultPath   := ParamS('openssl&path', 'the OpenSSL library #path');
    {$ENDIF USE_OPENSSL}
    {$IFDEF OSPOSIX}
    GssLib_Custom := ParamS('lib&krb5', 'the Kerberos libgssapi #filename');
    {$ENDIF OSPOSIX}

    result := not (ConsoleHelpFailed());
  end;
end;

var
  OpenRSATArgs: TOpenRSATArgs;

begin
  {$IFDEF DEBUG}
  if FileExists('heap.trc') then
    DeleteFile('heap.trc');
  SetHeapTraceOutput('heap.trc');
  {$ENDIF DEBUG}

  InitLoggingFromCommandLine;
  if not ParseArgs(OpenRSATArgs) then
    Halt(1);

  //TSynLog.Add.Log(sllDebug, 'Logger Initialization');
  RequireDerivedFormResource := True;
  Application.Scaled:=True;
  Application.LayoutAdjustmentPolicy := lapAutoAdjustForDPI;
  Application.Initialize;

  {$IFDEF DARWIN}
  AppPath := ExtractFilePath(ParamStr(0));
  if not OpenSslInitialize(MakePath([AppPath, 'lib', 'libcrypto.dylib']), MakePath([AppPath, 'lib', 'libssl.dylib']), '') then
    raise Exception.CreateFmt('Unable to load OpenSSL from %s', [MakePath([AppPath, 'lib', 'libssl.dylib'])]);
  {$ENDIF}

  RegisterOpenSsl;
  TSynLog.Add.Log(sllDebug, 'OpenSSL version: %', [OpenSslVersionText]);

  OnGetApplicationName := @GetApplicationName;
  Application.CreateForm(TVisOpenRSAT, VisOpenRSAT);
  VisOpenRSAT.Args := OpenRSATArgs;
  Application.Run();
end.

