unit ufrmmodules;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.base,
  mormot.core.log,
  ufrmmodule;

type

  TFrameModuleDynArray = Array of TFrameModule;

  { TFrmModules }

  TFrmModules = class
  private
    fLog: TSynLog;
    fFrmModules: TFrameModuleDynArray;
    fActiveModule: TFrameModule;

    function InternalRefresh(AModule: TFrameModule): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function RegisterModule(AFrmModule: TFrameModule): Boolean;
    function Change(AName: RawUtf8): Boolean;
    function Refresh: Boolean;
    function RefreshAll: Boolean;
    property Items: TFrameModuleDynArray read fFrmModules;
  end;

implementation

{ TFrmModules }

function TFrmModules.InternalRefresh(AModule: TFrameModule): Boolean;
begin
  result := False;
  if Assigned(AModule) then
    AModule.Refresh;
  result := True;
end;

constructor TFrmModules.Create;
begin
  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Create', [Self.ClassName]);
end;

destructor TFrmModules.Destroy;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Destroy', [Self.ClassName]);

  inherited Destroy;
end;

function TFrmModules.RegisterModule(AFrmModule: TFrameModule): Boolean;
var
  fm: TFrameModule;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'RegisterModule', Self);

  result := False;

  if not Assigned(AFrmModule) then
  begin
    if Assigned(fLog) then
      fLog.Log(sllWarning, 'Module not assigned.', Self);
    Exit;
  end;

  for fm in fFrmModules do
    if (fm = AFrmModule) or (fm.ModuleName = AFrmModule.ModuleName) then
    begin
      if Assigned(fLog) then
        fLog.Log(sllWarning, 'Module "%" already exists.', [AFrmModule.ModuleName], self);
      Exit;
    end;

  Insert(AFrmModule, fFrmModules, 0);
  if Assigned(fLog) then
    fLog.Log(sllInfo, 'Module "%" added.', [AFrmModule.ModuleName], self);
  result := True;
end;

function TFrmModules.Change(AName: RawUtf8): Boolean;
var
  fm: TFrameModule;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Change', [Self.ClassName]);

  result := True;

  for fm in fFrmModules do
    if (fm.ModuleName = AName) then
    begin
      if Assigned(fLog) then
        fLog.Log(sllInfo, '% - Found module "%".', [Self.ClassName, AName]);
      fActiveModule := fm;
      Exit;
    end;

  if Assigned(fLog) then
    fLog.Log(sllWarning, '% - Module not found "%".', [Self.ClassName, AName]);
  result := False;
  fActiveModule := nil;
end;

function TFrmModules.Refresh: Boolean;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Refresh', [Self.ClassName]);

  result := InternalRefresh(fActiveModule);
end;

function TFrmModules.RefreshAll: Boolean;
var
  fm: TFrameModule;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - RefreshAll', [Self.ClassName]);

  result := True;
  for fm in fFrmModules do
    result := InternalRefresh(fm) and result;
end;

end.

