unit umoduleadsi;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.base,
  umodule,
  umoduleadsioption,
  uoption,
  ursat;

type

  { TModuleADSI }

  TModuleADSI = class(TModule)
  private
    function GetADSIOption: TModuleADSIOption;
  public
    constructor Create(ARSAT: TRSAT);
    destructor Destroy; override;

    property ADSIOption: TModuleADSIOption read GetADSIOption;
  end;

implementation
uses
  ucommon;

{ TModuleADSI }

function TModuleADSI.GetADSIOption: TModuleADSIOption;
begin
  result := (fOption as TModuleADSIOption);
end;

constructor TModuleADSI.Create(ARSAT: TRSAT);
begin
  inherited Create('ServicesAndInterfaces', rsModuleADSIDisplayName);

  fRSAT := ARSAT;
  fOption := TModuleADSIOption.Create;
end;

destructor TModuleADSI.Destroy;
begin
  FreeAndNil(fOption);

  inherited Destroy;
end;

end.

