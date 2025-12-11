unit umoduleaddnsoption;

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

  { TModuleADDNSOption }

  TModuleADDNSOption = class(TOption)
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

{ TModuleADDNSOption }

procedure TModuleADDNSOption.Notify;
var
  Observer: TProcRsatOptionOfObject;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Notify', Self);

  for Observer in fObservers do
    Observer(Self);
end;

constructor TModuleADDNSOption.Create;
begin
  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace);

  fObservers := [];
  fChanged := False;
end;

destructor TModuleADDNSOption.Destroy;
begin
  inherited Destroy;
end;

procedure TModuleADDNSOption.Load(IniFile: TIniFile);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Load', Self);

  fChanged := False;
  Notify;
end;

procedure TModuleADDNSOption.Save(IniFile: TIniFile);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Save', Self);

  fChanged := False;
  Notify;
end;

function TModuleADDNSOption.Changed: Boolean;
begin
  result := fChanged;
end;

procedure TModuleADDNSOption.RegisterObserver(Observer: TProcRsatOptionOfObject
  );
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'RegisterObserver', Self);

  MultiEventAdd(fObservers, TMethod(Observer));
end;

procedure TModuleADDNSOption.RemoveObserver(Observer: TProcRsatOptionOfObject);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'RemoveObserver', Self);

  MultiEventRemove(fObservers, TMethod(Observer));
end;

end.

