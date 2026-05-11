program openrsattestsgui;

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  interfaces,
  Forms,
  mormot.core.test;

type

  { TOpenRSATTests }

  TOpenRSATTestsGUI = class(TSynTestsLogged)
  published
    procedure GUI;
  end;

{ TOpenRSATTests }

procedure TOpenRSATTestsGUI.GUI;
begin
  AddCase([]);
end;

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.ShowMainForm := False;
  TOpenRSATTestsGUI.RunAsConsole('OpenRSAT GUI Regression Tests');
end.

