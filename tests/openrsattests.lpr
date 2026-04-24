program openrsattests;

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  interfaces,
  Forms,
  mormot.core.test,
  ucommon,
  uproperty,
  uvisadvancedsecuritytest;

type

  { TOpenRSATTests }

  TOpenRSATTests = class(TSynTestsLogged)
  published
    procedure Units;
  end;

{ TOpenRSATTests }

procedure TOpenRSATTests.Units;
begin
  AddCase([
    TCommonTests,
    TPropertyTests,
    TVisAdvancedSecurityTest,
    TSidCacheTest,
    TGuidCacheTest
  ]);
end;

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.ShowMainForm := False;
  TOpenRSATTests.RunAsConsole('OpenRSAT Regression Tests');
end.

