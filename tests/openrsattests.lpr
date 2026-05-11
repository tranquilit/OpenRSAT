program openrsattests;

uses
  mormot.core.test,
  integrationtest.uadvancedsecuritypresenter,
  unittest.uadvancedsecuritypresenter;

type

  { TOpenRSATTests }

  TOpenRSATTests = class(TSynTestsLogged)
  published
    procedure Units;
    procedure Integrations;
    procedure Performances;
    procedure Security;
  end;

{ TOpenRSATTests }

procedure TOpenRSATTests.Units;
begin
  AddCase([
    TTestAdvancedSecurityPresenter
  ]);
end;

procedure TOpenRSATTests.Integrations;
begin
  AddCase([
    TIntegrationTestAdvancedSecurityPresenter
  ]);
end;

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

