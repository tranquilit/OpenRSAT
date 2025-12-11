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
    procedure Load(Path: RawUtf8 = ''); overload;
    procedure Load(IniFile: TIniFile); virtual; abstract; overload;
    procedure Save(Path: RawUtf8 = ''); overload;
    procedure Save(IniFile: TIniFile); virtual; abstract; overload;
    function Changed: Boolean; virtual; abstract;

    procedure RegisterObserver(Observer: TProcRsatOptionOfObject); virtual; abstract;
    procedure RemoveObserver(Observer: TProcRsatOptionOfObject); virtual; abstract;
  end;

implementation
uses
  uconfig;

{ TOption }

procedure TOption.Load(Path: RawUtf8);
var
  IniFile: TIniFile;
begin
  if Path = '' then
    Path := OptionFilePath;

  IniFile := TIniFile.Create(Path);
  try
    Load(IniFile);
  finally
    FreeAndNil(IniFile);
  end;
end;

procedure TOption.Save(Path: RawUtf8);
var
  IniFile: TIniFile;
begin
  if Path = '' then
    Path := OptionFilePath;

  IniFile := TIniFile.Create(Path);
  try
    Save(IniFile);
  finally
    FreeAndNil(IniFile);
  end;
end;

end.

