unit uconfig;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.base;

/// Get file path for OpenRSAT configurations.
/// Path based on ConfigFolderPath.
/// filename: config.ini
function ConfigFilePath: RawUtf8;
/// Get file path for OpenRSAT options.
/// Path based on ConfigFolderPath.
/// filename: options.ini
function OptionFilePath: RawUtf8;
/// Get file path for OpenRSAT visual components.
/// Path based on ConfigFolderPath.
/// filename: OpenRSAT.ini
function VisBakFilePath: RawUtf8;
/// Get folder path for OpenRSAT local storage
/// Path based on GetAppConfigDir.
function ConfigFolderPath: RawUtf8;

implementation
uses
  mormot.core.text;

function ConfigFilePath: RawUtf8;
begin
  result := MakePath([ConfigFolderPath, 'config.ini']);
end;

function OptionFilePath: RawUtf8;
begin
  result := MakePath([ConfigFolderPath, 'options.ini']);
end;

function VisBakFilePath: RawUtf8;
begin
  result := MakePath([ConfigFolderPath, 'OpenRSAT.ini']);
end;

function ConfigFolderPath: RawUtf8;
begin
  result := GetAppConfigDir(False);
end;

end.

