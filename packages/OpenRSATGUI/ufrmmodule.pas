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
  protected
    function GetFrmOption: TFrameOption; virtual; abstract;
    function GetFrmOptionClass: TFrameOptionClass; virtual; abstract;
    function GetModule: TModule; virtual; abstract;
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
    property Module: TModule read GetModule;
    property FrmOption: TFrameOption read GetFrmOption;
    property FrmOptionClass: TFrameOptionClass read GetFrmOptionClass;
  end;

implementation

{ TFrameModule }

function TFrameModule.GetModuleEnabled: Boolean;
begin
  result := Assigned(Module) and Module.GetModuleEnabled;
end;

procedure TFrameModule.SetModuleEnabled(AValue: Boolean);
begin
  if Assigned(Module) then
    Module.SetModuleEnabled(AValue);
end;

function TFrameModule.GetModuleName: RawUtf8;
begin
  result := '';
  if Assigned(Module) then
    result := Module.GetModuleName;
end;

function TFrameModule.GetModuleDisplayName: RawUtf8;
begin
  result := '';
  if Assigned(Module) then
    result := Module.GetModuleDisplayName;
end;

function TFrameModule.GetOption: TOption;
begin
  result := nil;
  if Assigned(Module) then
    result := Module.GetOption;
end;

end.

