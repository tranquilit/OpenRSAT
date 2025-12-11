unit umoduleadssoption;

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

  { TModuleADSSOption }

  TModuleADSSOption = class(TOption)
  private
    fLog: TSynLog;
    fChanged: Boolean;

    fObservers: Array of TProcRsatOptionOfObject;

    fShowService: Boolean;
    function GetShowService: Boolean;
    procedure SetShowService(AValue: Boolean);

    procedure Notify;
  public
    // TOptions
    procedure Load(IniFile: TIniFile); override;
    procedure Save(IniFile: TIniFile); override;
    function Changed: Boolean; override;
    procedure RegisterObserver(Observer: TProcRsatOptionOfObject); override;
    procedure RemoveObserver(Observer: TProcRsatOptionOfObject); override;

    property ShowService: Boolean read GetShowService write SetShowService;
  end;

implementation

{ TModuleADSSOption }

function TModuleADSSOption.GetShowService: Boolean;
begin
  result := fShowService;
end;

procedure TModuleADSSOption.SetShowService(AValue: Boolean);
begin
  if AValue = fShowService then
    Exit;

  fShowService := Avalue;
  fChanged := True;
end;

procedure TModuleADSSOption.Notify;
var
  Observer: TProcRsatOptionOfObject;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Notify', Self);

  for Observer in fObservers do
    Observer(Self);
end;

procedure TModuleADSSOption.Load(IniFile: TIniFile);
const
  Section = 'ADSS';
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Load', Self);

  fShowService := IniFile.ReadBool(Section, 'ShowService', False);

  fChanged := False;
  Notify;
end;

procedure TModuleADSSOption.Save(IniFile: TIniFile);
const
  Section = 'ADSS';
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Save', Self);

  IniFile.WriteBool(Section, 'ShowService', fShowService);

  fChanged := False;
  Notify;
end;

function TModuleADSSOption.Changed: Boolean;
begin
  result := fChanged;
end;

procedure TModuleADSSOption.RegisterObserver(Observer: TProcRsatOptionOfObject
  );
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'RegisterObserver', Self);

  MultiEventAdd(fObservers, TMethod(Observer));
end;

procedure TModuleADSSOption.RemoveObserver(Observer: TProcRsatOptionOfObject);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'RemoveObserver', Self);

  MultiEventRemove(fObservers, TMethod(Observer));
end;

end.

