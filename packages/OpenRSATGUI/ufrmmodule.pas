unit ufrmmodule;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  mormot.core.base,
  ufrmoption,
  umodule,
  uoption;

type

  { TFrameModule }

  TFrameModule = class(TFrame)
  public
    // Expose TModule
    function GetModuleEnabled: Boolean;
    procedure SetModuleEnabled(AValue: Boolean);
    function GetModuleName: RawUtf8;
    function GetModuleDisplayName: RawUtf8;
    function GetOption: TOption;
  published
    procedure Load; virtual; abstract;
    procedure Refresh; virtual; abstract;
    function Module: TModule; virtual; abstract;
    function FrmOption: TFrameOption; virtual; abstract;
  end;

implementation

{ TFrameModule }

function TFrameModule.GetModuleEnabled: Boolean;
begin
  result := Module.GetModuleEnabled;
end;

procedure TFrameModule.SetModuleEnabled(AValue: Boolean);
begin
  Module.SetModuleEnabled(AValue);
end;

function TFrameModule.GetModuleName: RawUtf8;
begin
  result := Module.GetModuleName;
end;

function TFrameModule.GetModuleDisplayName: RawUtf8;
begin
  result := Module.GetModuleDisplayName;
end;

function TFrameModule.GetOption: TOption;
begin
  result := Module.GetOption;
end;

end.

