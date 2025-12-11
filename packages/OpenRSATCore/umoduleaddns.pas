unit umoduleaddns;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.base,
  umodule,
  umoduleaddnsoption,
  uoption;

type

  { TModuleADDNS }

  TModuleADDNS = class(TModule)
    fEnabled: Boolean;
    fOption: TModuleADDNSOption;
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

{ TModuleADDNS }

constructor TModuleADDNS.Create;
begin
  fEnabled := True;
  fOption := nil;
end;

destructor TModuleADDNS.Destroy;
begin
  inherited Destroy;
end;

function TModuleADDNS.GetModuleEnabled: Boolean;
begin
  result := fEnabled;
end;

procedure TModuleADDNS.SetModuleEnabled(AValue: Boolean);
begin
  if AValue = fEnabled then
    Exit;
end;

function TModuleADDNS.GetModuleName: RawUtf8;
begin
  result := rsModuleDNSName;
end;

function TModuleADDNS.GetModuleDisplayName: RawUtf8;
begin
  result := rsModuleDNSDisplayName;
end;

function TModuleADDNS.GetOption: TOption;
begin
  result := fOption;
end;

end.

