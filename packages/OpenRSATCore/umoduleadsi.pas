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

function TModuleADSI.GetEnabled: Boolean;
begin
  result := fEnabled;
end;

procedure TModuleADSI.SetEnabled(AValue: Boolean);
begin
  if AValue = fEnabled then
    Exit;
  fEnabled := AValue;
end;

function TModuleADSI.GetName: RawUtf8;
begin
  result := rsModuleADSIName;
end;

function TModuleADSI.GetDisplayName: RawUtf8;
begin
  result := rsModuleADSIDisplayName;
end;

function TModuleADSI.GetOption: TOption;
begin
  result := fOption;
end;

end.

