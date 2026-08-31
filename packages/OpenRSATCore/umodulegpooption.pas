unit umodulegpooption;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  IniFiles,
  mormot.core.base,
  mormot.core.log,
  uoption,
  ulog;

type

  { TModuleGPOOption }

  TModuleGPOOption = class(TOption)
  private
    fLog: TSynLogClass;
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

{ TModuleGPOOption }

procedure TModuleGPOOption.Notify;
var
  Observer: TProcRsatOptionOfObject;
begin
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, 'Notify', Self);

  for Observer in fObservers do
    Observer(Self);
end;

constructor TModuleGPOOption.Create;
begin
  fLog := TOpenRSATLog;
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, 'Create', Self);

  fObservers := [];
  fChanged := False;
end;

destructor TModuleGPOOption.Destroy;
begin
  inherited Destroy;
end;

procedure TModuleGPOOption.Load(IniFile: TIniFile);
begin
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, 'Load', Self);

  fChanged := False;
  Notify;
end;

procedure TModuleGPOOption.Save(IniFile: TIniFile);
begin
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, 'Save', Self);

  fChanged := False;
  Notify;
end;

function TModuleGPOOption.Changed: Boolean;
begin
  result := fChanged;
end;

procedure TModuleGPOOption.RegisterObserver(Observer: TProcRsatOptionOfObject);
begin
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, 'RegisterObserver', Self);

  MultiEventAdd(fObservers, TMethod(Observer));
end;

procedure TModuleGPOOption.RemoveObserver(Observer: TProcRsatOptionOfObject);
begin
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, 'RemoveObserver', Self);

  MultiEventRemove(fObservers, TMethod(Observer));
end;

end.
