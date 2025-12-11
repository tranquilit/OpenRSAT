unit umodule;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.base,
  uoption;

type
  TModule = class
  public
    function GetModuleEnabled: Boolean; virtual; abstract;
    procedure SetModuleEnabled(AValue: Boolean); virtual; abstract;
    function GetModuleName: RawUtf8; virtual; abstract;
    function GetModuleDisplayName: RawUtf8; virtual; abstract;
    function GetOption: TOption; virtual; abstract;
  end;

implementation

end.

