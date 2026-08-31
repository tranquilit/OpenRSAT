unit umodulegpo;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.base,
  uoption,
  umodule,
  umodulegpooption,
  ursat;

type

  { TModuleGPO }

  TModuleGPO = class(TModule)
  private
    function GetGPOOption: TModuleGPOOption;
  public
    constructor Create(ARSAT: TRSAT);
    destructor Destroy; override;

    property GPOOption: TModuleGPOOption read GetGPOOption;
  end;

implementation
uses
  ucommon;

{ TModuleGPO }

function TModuleGPO.GetGPOOption: TModuleGPOOption;
begin
  result := (fOption as TModuleGPOOption);
end;

constructor TModuleGPO.Create(ARSAT: TRSAT);
begin
  inherited Create('GroupPolicy', rsModuleGPODisplayName);

  fOption := TModuleGPOOption.Create;
  fRSAT := ARSAT;
  fOption.Load;
end;

destructor TModuleGPO.Destroy;
begin
  FreeAndNil(fOption);

  inherited Destroy;
end;

end.
