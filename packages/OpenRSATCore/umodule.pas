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
  TModule = class
  protected
    function GetEnabled: Boolean; virtual; abstract;
    procedure SetEnabled(AValue: Boolean); virtual; abstract;
    function GetName: RawUtf8; virtual; abstract;
    function GetDisplayName: RawUtf8; virtual; abstract;
    function GetOption: TOption; virtual; abstract;
    function GetRSAT: TRSAT; virtual; abstract;

  public
    // Module status
    property Enabled: Boolean read GetEnabled write SetEnabled;
    // Retrieve module name
    property Name: RawUtf8 read GetName;
    // Retrieve module display name
    property DisplayName: RawUtf8 read GetDisplayName;
    // Retrieve module option
    property Option: TOption read GetOption;

    property RSAT: TRSAT read GetRSAT;
  end;

implementation

end.

