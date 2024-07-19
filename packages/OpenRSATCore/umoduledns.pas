unit umoduledns;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  uabstractmodule,
  ucore;

type

  { TModuleDNS }

  TModuleDNS = Class(TAbstractModule)
    function GetModuleEnabled: Boolean; override;
    procedure SetModuleEnabled(AValue: Boolean); override;
    function GetModuleName: String; override;
    function GetModuleOptions: TAbstractModuleOptions; override;
    procedure Refresh; override;

    constructor Create(ACore: TRsatCore);
  end;

implementation

{ TModuleDNS }

function TModuleDNS.GetModuleEnabled: Boolean;
begin
  result := True;
end;

procedure TModuleDNS.SetModuleEnabled(AValue: Boolean);
begin

end;

function TModuleDNS.GetModuleName: String;
begin
  result := 'DNS';
end;

function TModuleDNS.GetModuleOptions: TAbstractModuleOptions;
begin

end;

procedure TModuleDNS.Refresh;
begin

end;

constructor TModuleDNS.Create(ACore: TRsatCore);
begin

end;

end.

