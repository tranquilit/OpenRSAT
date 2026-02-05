program openrsattests;

uses
  mormot.core.test,
  ucommon,
  uproperty;

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
    TPropertyTests
  ]);
end;

begin
  TOpenRSATTests.RunAsConsole('OpenRSAT Regression Tests');
end.

