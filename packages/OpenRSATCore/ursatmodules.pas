unit ursatmodules;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.log,
  uinterfacemodule;

type

  { TRsatModules }

  TRsatModules = class
  private
    fLog: TSynLog;
    fModules: Array of IModule;
    fActiveModule: IModule;

    function InternalRefresh(AModule: IModule): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function RegisterModule(AModule: IModule): Boolean;
    function Change(AName: String): Boolean;
    function Refresh: Boolean;
    function RefreshAll: Boolean;
  end;

implementation
uses
  mormot.core.base;

{ TRsatModules }

function TRsatModules.InternalRefresh(AModule: IModule): Boolean;
begin
  result := False;
  if Assigned(AModule) then
    AModule.Refresh;
  result := True;
end;

constructor TRsatModules.Create;
begin
  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Create', [Self.ClassName]);
end;

destructor TRsatModules.Destroy;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Destroy', [Self.ClassName]);

  inherited Destroy;
end;

function TRsatModules.RegisterModule(AModule: IModule): Boolean;
var
  Module: IModule;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - RegisterModule', [Self.ClassName]);

  result := False;

  if not Assigned(AModule) then
  begin
    if Assigned(fLog) then
      fLog.Log(sllWarning, '% - Module not assigned.', [Self.ClassName]);
    Exit;
  end;

  for Module in fModules do
    if (Module = AModule) or (Module.GetModuleName = AModule.GetModuleName) then
    begin
      if Assigned(fLog) then
        fLog.Log(sllWarning, '% - Module "%" already exists.', [Self.ClassName, AModule.GetModuleName]);
      Exit;
    end;

  Insert(AModule, fModules, 0);
  if Assigned(fLog) then
    fLog.Log(sllInfo, '% - Module "%" added.', [Self.ClassName, AModule.GetModuleName]);
  result := True;
end;

function TRsatModules.Change(AName: String): Boolean;
var
  Module: IModule;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Change', [Self.ClassName]);

  result := True;

  for Module in fModules do
    if (Module.GetModuleName = AName) then
    begin
      if Assigned(fLog) then
        fLog.Log(sllInfo, '% - Found module "%".', [Self.ClassName, AName]);
      fActiveModule := Module;
      Exit;
    end;

  if Assigned(fLog) then
    fLog.Log(sllWarning, '% - Module not found "%".', [Self.ClassName, AName]);
  result := False;
  fActiveModule := nil;
end;

function TRsatModules.Refresh: Boolean;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Refresh', [Self.ClassName]);

  result := InternalRefresh(fActiveModule);
end;

function TRsatModules.RefreshAll: Boolean;
var
  Module: IModule;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - RefreshAll', [Self.ClassName]);

  result := True;
  for Module in fModules do
    result := InternalRefresh(Module) and result;
end;

end.

