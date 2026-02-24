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

  { TModule }

  TModule = class
  protected
    fEnabled: Boolean;
    fName: RawUtf8;
    fDisplayName: RawUtf8;
    fOption: TOption;
    fRSAT: TRSAT;

    procedure SetEnabled(AValue: Boolean); virtual; abstract;
  public
    constructor Create(AName, ADisplayName: RawUtf8);
    // Module status
    property Enabled: Boolean read fEnabled write SetEnabled;
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

constructor TModule.Create(AName, ADisplayName: RawUtf8);
begin
  fName := AName;
  fDisplayName := ADisplayName;
end;

end.

