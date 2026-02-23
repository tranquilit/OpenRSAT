unit umoduleadss;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.base,
  uoption,
  umodule,
  umoduleadssoption,
  ursat;

type

  { TModuleADSS }

  TModuleADSS = class(TModule)
  private
    function GetADSSOption: TModuleADSSOption;
    function GetShowService: Boolean;
  public
    constructor Create(ARSAT: TRSAT);
    destructor Destroy; override;


    property ADSSOption: TModuleADSSOption read GetADSSOption;
    // Expose Option
    property ShowService: Boolean read GetShowService;

    /// TModule
  protected
    procedure SetEnabled(AValue: Boolean); override;
  end;

implementation
uses
  ucommon;

{ TModuleADSS }

function TModuleADSS.GetShowService: Boolean;
begin
  result := ADSSOption.ShowService;
end;

function TModuleADSS.GetADSSOption: TModuleADSSOption;
begin
  result := (fOption as TModuleADSSOption);
end;

constructor TModuleADSS.Create(ARSAT: TRSAT);
begin
  inherited Create(rsModuleADSSName, rsModuleADSSDisplayName);

  fEnabled := True;
  fOption := TModuleADSSOption.Create;
  fRSAT := ARSAT;
  fOption.Load;
end;

destructor TModuleADSS.Destroy;
begin
  FreeAndNil(fOption);

  inherited Destroy;
end;

procedure TModuleADSS.SetEnabled(AValue: Boolean);
begin
  if AValue = fEnabled then
    Exit;
  fEnabled := AValue;
end;

end.

