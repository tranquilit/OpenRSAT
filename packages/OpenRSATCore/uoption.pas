unit uoption;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  IniFiles,
  mormot.core.base;

type

  TOption = class;

  TProcRsatOptionOfObject = procedure(Option: TOption) of Object;

  { TOption }
  TOption = class
  public
    procedure Load(IniFile: TIniFile); virtual; abstract;
    procedure Save(IniFile: TIniFile); virtual; abstract;
    function Changed: Boolean; virtual; abstract;
    function OptionName: RawUtf8; virtual; abstract;

    procedure RegisterObserver(Observer: TProcRsatOptionOfObject); virtual; abstract;
    procedure RemoveObserver(Observer: TProcRsatOptionOfObject); virtual; abstract;
  end;

implementation

end.

