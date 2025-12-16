program openrsattests;

uses
  mormot.core.test,
  ucommon;

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
    TCommonTests
  ]);
end;

begin
  TOpenRSATTests.RunAsConsole('OpenRSAT Regression Tests');
end.

