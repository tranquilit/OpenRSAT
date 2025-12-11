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
  protected
    function GetEnabled: Boolean; override;
    procedure SetEnabled(AValue: Boolean); override;
    function GetName: RawUtf8; override;
    function GetDisplayName: RawUtf8; override;
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

function TModuleADDNS.GetEnabled: Boolean;
begin
  result := fEnabled;
end;

procedure TModuleADDNS.SetEnabled(AValue: Boolean);
begin
  if AValue = fEnabled then
    Exit;
end;

function TModuleADDNS.GetName: RawUtf8;
begin
  result := rsModuleDNSName;
end;

function TModuleADDNS.GetDisplayName: RawUtf8;
begin
  result := rsModuleDNSDisplayName;
end;

function TModuleADDNS.GetOption: TOption;
begin
  result := fOption;
end;

end.

