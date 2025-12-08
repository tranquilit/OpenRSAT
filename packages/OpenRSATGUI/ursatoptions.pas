unit ursatoptions;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.base,
  uinterfacemodule;

type

  TThemeMode = (tmLight, tmDark, tmSystem);

  { TRsatOptions }

  TRsatOptions = class(TFrameOptions)
  private
    fUpdating: Integer;
    fOptionsPath: String;
    fSection: String;

    fLang: String;
    fTheme: TThemeMode;

    fModulesOptions: Array of IOptions;
    procedure SetTheme(AValue: TThemeMode);

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  published
    procedure AddOptions(AModuleOptions: IOptions);

    procedure Save; override;
    procedure Load; override;

    procedure BeginUpdate;
    procedure EndUpdate;

    property Theme: TThemeMode read fTheme write SetTheme;
    property Lang: String read fLang;
  end;

implementation

uses
  IniFiles,
  mormot.core.text;

{ TRsatOptions }

procedure TRsatOptions.SetTheme(AValue: TThemeMode);
begin
  if fTheme = AValue then
    Exit;
  fTheme := AValue;
  Save;
end;

constructor TRsatOptions.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fUpdating := 0;
  fOptionsPath := MakePath([GetAppConfigDir(False), 'options.ini']);
  fSection := 'global';

  fLang := '';
  fTheme := tmSystem;

  fModulesOptions := [];
  Load;
end;

destructor TRsatOptions.Destroy;
begin
  inherited Destroy;
end;

procedure TRsatOptions.AddOptions(AModuleOptions: IOptions);
begin
  Insert(AModuleOptions, fModulesOptions, 0);
end;

procedure TRsatOptions.Save;
var
  ModuleOptions: IOptions;
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(fOptionsPath);
  try
    // Save own options.
    IniFile.WriteInt64('General', 'theme', Ord(fTheme));
    IniFile.WriteString('General', 'lang', fLang);

    // Save modules options.
    for ModuleOptions in fModulesOptions do
      ModuleOptions.Save(IniFile);
  finally
    FreeAndNil(IniFile);
  end;
end;

procedure TRsatOptions.Load;
var
  ModuleOptions: IOptions;
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(fOptionsPath);
  try
    // Load own options.
    fTheme := TThemeMode(IniFile.ReadInt64('General', 'theme', Ord(tmSystem)));
    fLang := IniFile.ReadString('General', 'lang', '');

    // Save modules options.
    for ModuleOptions in fModulesOptions do
      ModuleOptions.Load(IniFile);
  finally
    FreeAndNil(IniFile);
  end;
end;

procedure TRsatOptions.BeginUpdate;
begin
  Inc(fUpdating);
end;

procedure TRsatOptions.EndUpdate;
begin
  Dec(fUpdating);

  Save;
end;

end.

