unit umodule;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.base,
  uoption,
  ursat;

type
  /// TModule provide an Abstract interface to a RSAT module.

  TModuleParameter = (
    mpEnabled,
    mpNeedRefresh
  );

  TModuleParameters = Set of TModuleParameter;
  { TModule }

  TModule = class
  protected
    fParameters: TModuleParameters;
    fName: RawUtf8;
    fDisplayName: RawUtf8;
    fOption: TOption;
    fRSAT: TRSAT;

    procedure SetNeedRefresh(AValue: Boolean); virtual;
    function GetNeedRefresh: Boolean; virtual;
    procedure SetEnabled(AValue: Boolean); virtual;
    function GetEnabled: Boolean; virtual;
  public
    constructor Create(AName, ADisplayName: RawUtf8);
    // Module status
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property NeedRefresh: Boolean read GetNeedRefresh write SetNeedRefresh;
    // Retrieve module name
    property Name: RawUtf8 read fName;
    // Retrieve module display name
    property DisplayName: RawUtf8 read fDisplayName;
    // Retrieve module option
    property Option: TOption read fOption;

    property RSAT: TRSAT read fRSAT;
  end;

implementation

{ TModule }

procedure TModule.SetNeedRefresh(AValue: Boolean);
begin
  if AValue then
    Include(fParameters, mpNeedRefresh)
  else
    Exclude(fParameters, mpNeedRefresh);
end;

function TModule.GetNeedRefresh: Boolean;
begin
  result := (mpNeedRefresh in fParameters);
end;

procedure TModule.SetEnabled(AValue: Boolean);
begin
  if AValue then
    Include(fParameters, mpEnabled)
  else
    Exclude(fParameters, mpEnabled);
end;

function TModule.GetEnabled: Boolean;
begin
  result := mpEnabled in fParameters;
end;

constructor TModule.Create(AName, ADisplayName: RawUtf8);
begin
  fName := AName;
  fDisplayName := ADisplayName;
  fParameters := [mpEnabled, mpNeedRefresh];
end;

end.

