unit uthemeinit;

{$mode objfpc}{$H+}

interface

implementation

uses
  SysUtils,
  IniFiles;

{$IFDEF LINUX}
function setenv(name, value: PChar; overwrite: LongInt): LongInt;
  cdecl; external 'c' name 'setenv';

function FindGtk2Theme(const ANames: array of String): String;
var
  Name: String;
begin
  result := '';
  for Name in ANames do
    if DirectoryExists('/usr/share/themes/' + Name + '/gtk-2.0') then
      Exit(Name);
end;

function OpenRSATOptionsPath: String;
var
  ConfigHome: String;
begin
  ConfigHome := GetEnvironmentVariable('XDG_CONFIG_HOME');
  if ConfigHome = '' then
    ConfigHome := IncludeTrailingPathDelimiter(
      GetEnvironmentVariable('HOME')) + '.config';
  result := IncludeTrailingPathDelimiter(ConfigHome) +
    'OpenRSAT' + PathDelim + 'options.ini';
end;

procedure ApplyLinuxThemeFromOptions;
var
  Ini: TIniFile;
  Theme: Integer;
  ThemeName, ThemePath, RcPath: String;
  RcFile: TextFile;
  DarkMode: Boolean;
begin
  // Respect an explicit GTK2_RC_FILES from the environment.
  if GetEnvironmentVariable('GTK2_RC_FILES') <> '' then
    Exit;

  Ini := TIniFile.Create(OpenRSATOptionsPath);
  try
    Theme := Ini.ReadInteger('General', 'theme', 2); // 2 = tmSystem
  finally
    Ini.Free;
  end;

  case Theme of
    0: // tmLight
      begin
        DarkMode := False;
        ThemeName := FindGtk2Theme(['Arc', 'Adwaita']);
      end;
    1: // tmDark
      begin
        DarkMode := True;
        ThemeName := FindGtk2Theme(
          ['Arc-Dark', 'Adwaita-dark', 'Materia-dark', 'Numix']);
      end;
  else
    // tmSystem: keep the GTK2 theme configured by the system.
    Exit;
  end;

  if ThemeName = '' then
    Exit;

  ThemePath := '/usr/share/themes/' + ThemeName + '/gtk-2.0/gtkrc';
  if not FileExists(ThemePath) then
    Exit;

  // Create a small RC file that selects the GTK2 theme name and includes
  // the theme styles. It is read by GTK2 during its initialization.
  RcPath := ExtractFilePath(OpenRSATOptionsPath) + 'gtkrc-2.0';
  try
    ForceDirectories(ExtractFilePath(RcPath));
    AssignFile(RcFile, RcPath);
    Rewrite(RcFile);
    try
      WriteLn(RcFile, 'gtk-theme-name = "' + ThemeName + '"');
      WriteLn(RcFile, 'include "' + ThemePath + '"');
    finally
      CloseFile(RcFile);
    end;
  except
    RcPath := ThemePath;
  end;

  setenv('GTK2_RC_FILES', PChar(RcPath), 1);
  if DarkMode then
    setenv('OPENRSAT_DARK_MODE', '1', 1)
  else
    setenv('OPENRSAT_DARK_MODE', '0', 1);
end;
{$ENDIF LINUX}

initialization
  {$IFDEF LINUX}
  ApplyLinuxThemeFromOptions;
  {$ENDIF LINUX}
end.
