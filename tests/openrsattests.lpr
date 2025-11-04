program openrsattests;

uses
  interfaces,
  mormot.core.test,
  ufrmcore,
  uvispropertieslist;

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
    TTestVisPropertiesList,
    TTestFrmCore
  ]);
end;

begin
  TOpenRSATTests.RunAsConsole('OpenRSAT Regression Tests');
end.

