unit utheme;

{$mode objfpc}{$H+}
{$IFDEF DARWIN}
{$modeswitch objectivec1}
{$ENDIF DARWIN}

interface

uses
  Classes,
  {$IFDEF DARWIN}
  CocoaAll,
  {$ENDIF DARWIN}
  {$IFDEF LINUX}
  gtk2,
  glib2,
  {$ENDIF LINUX}
  SysUtils;

function IsDarkMode: Boolean;

{$IFDEF DARWIN}
function IsMacDarkMode: Boolean;
function GetMacThemeName: String;
{$ENDIF DARWIN}
{$IFDEF LINUX}
function IsLinuxDarkMode: Boolean;
function GetGtkThemeName: String;
{$ENDIF LINUX}
{$IFDEF WINDOWS}
function IsWinDarkMode: Boolean;
function GetWinThemeName: String;
{$ENDIF WINDOWS}

implementation
uses
  uDarkStyleParams,
  mormot.core.base,
  mormot.core.log;

function IsDarkMode: Boolean;
begin
  result := False;

  {$IFDEF DARWIN}
  result := IsMacDarkMode;
  {$ENDIF DARWIN}

  {$IFDEF LINUX}
  result := IsLinuxDarkMode;
  {$ENDIF LINUX}

  {$IFDEF WINDOWS}
  result := IsWinDarkMode;
  {$ENDIF WINDOWS}
end;

{$IFDEF DARWIN}
function IsMacDarkMode: Boolean;
begin
  result := GetMacThemeName.ToLower.Contains('dark');
end;

function GetMacThemeName: String;
var
  Appearance: NSAppearance;
begin
  result := '';
  Appearance := NSAppearance.currentAppearance;
  if not Assigned(Appearance) then
    Exit;
  result := Appearance.name.UTF8String;
end;

{$ENDIF DARWIN}

{$IFDEF LINUX}
function IsLinuxDarkMode: Boolean;
begin
  result := LowerCase(GetGtkThemeName).Contains('dark');
end;

function GetGtkThemeName: String;
var
  Settings: PGtkSettings;
  Theme: Pgchar;
begin
  result := '';
  Theme := nil;
  TSynLog.Add.Log(sllTrace, 'get theme name');
  Settings := gtk_settings_get_default;
  TSynLog.Add.Log(sllTrace, 'settings retrieved');

  if not Assigned(Settings) then
    Exit;

  g_object_get(Settings, PChar('gtk-theme-name'), @Theme, nil);
  if Theme <> nil then
  begin
    TSynLog.Add.Log(sllTrace, 'Theme name: ', [theme]);
    result := StrPas(@Theme[1]);
    g_free(Theme);
  end;
end;

{$ENDIF LINUX}

{$IFDEF WINDOWS}
function IsWinDarkMode: Boolean;
begin
  result := IsDarkModeEnabled;
end;

function GetWinThemeName: String;
begin
  result := '';
end;
{$ENDIF WINDOWS}

end.

