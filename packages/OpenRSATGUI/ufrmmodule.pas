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
    function GetFrmOptionClass: TFrameOptionClass; virtual; abstract;
    function GetModule: TModule; virtual; abstract;

    function GetOnLdapConnect: TNotifyEvent; virtual; abstract;
    function GetOnLdapClose: TNotifyEvent; virtual; abstract;

  published
    procedure Load; virtual; abstract;
    procedure Refresh; virtual; abstract;

    property Module: TModule read GetModule;
    property FrmOptionClass: TFrameOptionClass read GetFrmOptionClass;

    property OnLdapConnect: TNotifyEvent read GetOnLdapConnect;
    property OnLdapClose: TNotifyEvent read GetOnLdapClose;

  // Expose TModule
  private
    function GetModuleDisplayName: RawUtf8;
    function GetModuleEnabled: Boolean;
    function GetModuleName: RawUtf8;
    function GetModuleOption: TOption;
    procedure SetModuleEnabled(AValue: Boolean);
  public
    property ModuleEnabled: Boolean read GetModuleEnabled write SetModuleEnabled;
    property ModuleName: RawUtf8 read GetModuleName;
    property ModuleDisplayName: RawUtf8 read GetModuleDisplayName;
    property ModuleOption: TOption read GetModuleOption;
  end;

implementation

{ TFrameModule }

function TFrameModule.GetModuleEnabled: Boolean;
begin
  result := Assigned(Module) and Module.Enabled;
end;

procedure TFrameModule.SetModuleEnabled(AValue: Boolean);
begin
  if Assigned(Module) then
    Module.Enabled := AValue;
end;

function TFrameModule.GetModuleOption: TOption;
begin
  result := nil;
  if Assigned(Module) then
    result := Module.Option;
end;

function TFrameModule.GetModuleName: RawUtf8;
begin
  result := '';
  if Assigned(Module) then
    result := Module.Name;
end;

function TFrameModule.GetModuleDisplayName: RawUtf8;
begin
  result := '';
  if Assigned(Module) then
    result := Module.DisplayName;
end;

end.

