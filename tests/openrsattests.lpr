program openrsattests;

uses
  {$IFDEF UNIX}
    cthreads,
  {$ENDIF}
  mormot.core.test,
  {$IFDEF INTEGRATION}
  integrationtest.uadvancedsecuritypresenter,
  integrationtest.ugeneralpropertysitelink,
  {$ENDIF INTEGRATION}
  unittest.uadvancedsecuritypresenter,
  unittest.ugeneratekeytab,
  unittest.uviewkeytabpresenter,
  unittest.udoublelistlogic,
  unittest.uscheduling;

type

  { TOpenRSATTests }

  TOpenRSATTests = class(TSynTestsLogged)
  published
    procedure Units;
    {$IFDEF INTEGRATION}
    procedure Integrations;
    {$ENDIF INTEGRATION}
    procedure Performances;
    procedure Security;
  end;

{ TOpenRSATTests }

procedure TOpenRSATTests.Units;
begin
  AddCase([
    TTestAdvancedSecurityPresenter,
    TUnitTestGenerateKeyTab,
    TUnitTestViewKeyTabPresenter,
    TUnitTestDoubleListLogic,
    TUnitTestScheduling
  ]);
end;

{$IFDEF INTEGRATION}
procedure TOpenRSATTests.Integrations;
begin
  AddCase([
    TIntegrationTestAdvancedSecurityPresenter,
    TIntegrationTestGeneralPropertySiteLink
  ]);
end;
{$ENDIF INTEGRATION}

procedure TOpenRSATTests.Performances;
begin
  AddCase([
  ]);
end;

procedure TOpenRSATTests.Security;
begin
  AddCase([
  ]);
end;

begin
  TOpenRSATTests.RunAsConsole('OpenRSAT Regression Tests');
end.

