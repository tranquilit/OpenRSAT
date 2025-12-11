unit ursatoption;

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

  TThemeMode = (tmLight, tmDark, tmSystem);

  { TRsatOption }

  TRsatOption = class(TOption)
  private
    // If assigned, enable logging activities
    fLog: TSynLog;
    fUpdating: Integer;
    fOptionsPath: String;
    fChanged: Boolean;

    fLang: String;
    fTheme: TThemeMode;
    fAdvancedView: Boolean;

    fObservers: Array of TProcRsatOptionOfObject;

    procedure Notify;
    procedure SetAdvancedView(AValue: Boolean);
    procedure SetLang(AValue: String);
    procedure SetTheme(AValue: TThemeMode);
  public
    // Inherited
    procedure Load(IniFile: TIniFile); override;
    procedure Save(IniFile: TIniFile); override;
    // Options or FrameOptions has changed
    function Changed: Boolean; override;
    procedure RegisterObserver(Observer: TProcRsatOptionOfObject); override;
    procedure RemoveObserver(Observer: TProcRsatOptionOfObject); override;

    constructor Create;
    procedure Load; overload;
    procedure Save; overload;
    procedure BeginUpdate;
    procedure EndUpdate;

    property Lang: String read fLang write SetLang;
    property Theme: TThemeMode read fTheme write SetTheme;
    property AdvancedView: Boolean read fAdvancedView write SetAdvancedView;
  end;

implementation
uses
  mormot.core.text;

{ TRsatOption }

procedure TRsatOption.Notify;
var
  Observer: TProcRsatOptionOfObject;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Notify', Self);

  for Observer in fObservers do
    Observer(Self);
end;

procedure TRsatOption.SetAdvancedView(AValue: Boolean);
begin
  if fAdvancedView = AValue then
    Exit;
  fAdvancedView := AValue;

  if (fUpdating = 0) then
    Save;
end;

procedure TRsatOption.SetLang(AValue: String);
begin
  if fLang = AValue then
    Exit;
  fLang := AValue;

  if (fUpdating = 0) then
  begin
    Save;
    Notify;
  end;
end;

procedure TRsatOption.SetTheme(AValue: TThemeMode);
begin
  if fTheme = AValue then
    Exit;
  fTheme := AValue;

  if (fUpdating = 0) then
  begin
    Save;
    Notify;
  end;
end;

constructor TRsatOption.Create;
begin
  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);

  fOptionsPath := MakePath([GetAppConfigDir(False), 'options.ini']);
  if Assigned(fLog) then
    fLog.Log(sllInfo, 'Options path: %', [fOptionsPath], Self);
end;

procedure TRsatOption.Load(IniFile: TIniFile);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Load', Self);

  // Load own options.
  fTheme := TThemeMode(IniFile.ReadInt64('General', 'theme', Ord(tmSystem)));
  fLang := IniFile.ReadString('General', 'lang', '');
  AdvancedView := IniFile.ReadBool('General', 'advancedView', False);

  fChanged := False;

  Notify;
end;

procedure TRsatOption.Load;
var
  IniFile: TIniFile;
begin
  if Assigned(fLog) then
    fLog.Log(sllInfo, 'Load options from: %', [fOptionsPath], Self);

  // Create ini file
  IniFile := TIniFile.Create(fOptionsPath);
  try
    Load(IniFile);
  finally
    FreeAndNil(IniFile);
  end;
end;

procedure TRsatOption.Save(IniFile: TIniFile);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Save', Self);

  // Save own options.
  IniFile.WriteInt64('General', 'theme', Ord(fTheme));
  IniFile.WriteString('General', 'lang', fLang);
  IniFile.WriteBool('General', 'advancedView', fAdvancedView);

  fChanged := False;

  Notify;
end;

procedure TRsatOption.Save;
var
  IniFile: TIniFile;
begin
  if Assigned(fLog) then
    fLog.Log(sllInfo, 'Save options to: %', [fOptionsPath], Self);

  // Create ini file
  IniFile := TIniFile.Create(fOptionsPath);
  try
    Save(IniFile);
    fChanged := False;
  finally
    FreeAndNil(IniFile);
  end;
end;

function TRsatOption.Changed: Boolean;
begin
  result := fChanged;
end;

procedure TRsatOption.BeginUpdate;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'BeginUpdate', Self);

  Inc(fUpdating);
end;

procedure TRsatOption.EndUpdate;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'EndUpdate', Self);

  Dec(fUpdating);
end;

procedure TRsatOption.RegisterObserver(Observer: TProcRsatOptionOfObject);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'RegisterObserver', Self);

  MultiEventAdd(fObservers, TMethod(Observer));
end;

procedure TRsatOption.RemoveObserver(Observer: TProcRsatOptionOfObject);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'RemoveObserver', Self);

  MultiEventRemove(fObservers, TMethod(Observer));
end;

end.

