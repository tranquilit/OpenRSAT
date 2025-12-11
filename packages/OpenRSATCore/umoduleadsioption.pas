unit umoduleadsioption;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  IniFiles,
  mormot.core.base,
  mormot.core.log,
  uoption;

type

  { TModuleADSIOption }

  TModuleADSIOption = class(TOption)
  private
    fLog: TSynLog;
    fChanged: Boolean;

    fObservers: Array of TProcRsatOptionOfObject;

    procedure Notify;
  public
    constructor Create;
    destructor Destroy; override;

  /// TOption
  public
    procedure Load(IniFile: TIniFile); override;
    procedure Save(IniFile: TIniFile); override;
    function Changed: Boolean; override;

    procedure RegisterObserver(Observer: TProcRsatOptionOfObject); override;
    procedure RemoveObserver(Observer: TProcRsatOptionOfObject); override;
  end;

implementation

{ TModuleADSIOption }

procedure TModuleADSIOption.Notify;
var
  Observer: TProcRsatOptionOfObject;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Notify', Self);

  for Observer in fObservers do
    Observer(Self);
end;

constructor TModuleADSIOption.Create;
begin
  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);

  fObservers := [];
  fChanged := False;
end;

destructor TModuleADSIOption.Destroy;
begin
  inherited Destroy;
end;

procedure TModuleADSIOption.Load(IniFile: TIniFile);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Load', Self);

  fChanged := False;
  Notify;
end;

procedure TModuleADSIOption.Save(IniFile: TIniFile);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Load', Self);

  fChanged := False;
  Notify;
end;

function TModuleADSIOption.Changed: Boolean;
begin
  result := fChanged;
end;

procedure TModuleADSIOption.RegisterObserver(Observer: TProcRsatOptionOfObject);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'RegisterObserver', Self);

  MultiEventAdd(fObservers, TMethod(Observer));
end;

procedure TModuleADSIOption.RemoveObserver(Observer: TProcRsatOptionOfObject);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'RemoveObserver', Self);

  MultiEventRemove(fObservers, TMethod(Observer));
end;

end.

