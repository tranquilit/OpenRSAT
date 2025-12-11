unit uconfig;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.base;

function ConfigFilePath: RawUtf8;
function OptionFilePath: RawUtf8;
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

function ConfigFolderPath: RawUtf8;
begin
  result := GetAppConfigDir(False);
end;

end.

