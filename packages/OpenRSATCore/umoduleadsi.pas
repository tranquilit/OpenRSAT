unit umoduleadsi;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.base,
  umodule,
  umoduleadsioption,
  uoption;

type

  { TModuleADSI }

  TModuleADSI = class(TModule)
  private
    fEnabled: Boolean;
    fOption: TModuleADSIOption;
  public
    constructor Create;
    destructor Destroy; override;

    /// TModule
    function GetModuleEnabled: Boolean; override;
    procedure SetModuleEnabled(AValue: Boolean); override;
    function GetModuleName: RawUtf8; override;
    function GetModuleDisplayName: RawUtf8; override;
    function GetOption: TOption; override;
  end;

implementation
uses
  ucommon;

{ TModuleADSI }

constructor TModuleADSI.Create;
begin
  fEnabled := True;
  fOption := TModuleADSIOption.Create;
end;

destructor TModuleADSI.Destroy;
begin
  FreeAndNil(fOption);

  inherited Destroy;
end;

function TModuleADSI.GetModuleEnabled: Boolean;
begin
  result := fEnabled;
end;

procedure TModuleADSI.SetModuleEnabled(AValue: Boolean);
begin
  if AValue = fEnabled then
    Exit;
  fEnabled := AValue;
end;

function TModuleADSI.GetModuleName: RawUtf8;
begin
  result := rsModuleADSIName;
end;

function TModuleADSI.GetModuleDisplayName: RawUtf8;
begin
  result := rsModuleADSIDisplayName;
end;

function TModuleADSI.GetOption: TOption;
begin
  result := fOption;
end;

end.

