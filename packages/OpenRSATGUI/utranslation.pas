unit utranslation;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  LCLType,
  mormot.core.base;

procedure SetLanguage(const Lang: RawUtf8);

function TranslateFromResource(const ALang: RawUtf8; ForceUpdate: Boolean = True;
  const ResBaseName: RawUtf8 = 'TRANSLATION'): Boolean;

implementation
uses
  LCLTranslator,
  LResources,
  Translations,
  Forms,
  mormot.core.text;

function LocaleFolderPath: RawUtf8;
begin
  AppFolder := ExtractFilePath(Application.ExeName);
  {$IFDEF WINDOWS}
  result := MakePath([AppFolder, 'locale']);
  {$ELSEIF DARWIN}
  result := MakePath([AppFolder, '..', 'Resources', 'locale']);
  {$ELSEIF LINUX}
  result := '/usr/share/openrsat/locale/';
  {$ELSE}
  result := MakePath([AppFolder, 'locale']);
  {$ENDIF}
end;

procedure SetLanguage(const Lang: RawUtf8);
begin
  SetDefaultLang(Lang, LocaleFolderPath);
end;

function TranslateFromResource(const ALang: RawUtf8; ForceUpdate: Boolean;
  const ResBaseName: RawUtf8): Boolean;
var
  Res: TResourceStream;
  PoFile: TPOFile;
  LocalTr: TUpdateTranslator;
  I: Integer;
  ResName: RawUtf8;
begin
  Result := False;

  ResName := FormatUtf8('%.%', [ResBaseName, ALang]);

  Res := TResourceStream.Create(HInstance, ResName, RT_RCDATA);
  try
    PoFile := TPOFile.Create(Res);
    try
      Result := TranslateResourceStrings(PoFile);
      LocalTr := TPOTranslator.Create(PoFile);

      if Assigned(LRSTranslator) then
        LRSTranslator.Free;
      LRSTranslator := LocalTr;

      if ForceUpdate then
      begin
        for I := 0 to Pred(Screen.CustomFormCount) do
          LocalTr.UpdateTranslation(Screen.CustomForms[I]);
        for I := 0 to Pred(Screen.DataModuleCount) do
          LocalTr.UpdateTranslation(Screen.DataModules[I]);
      end;
    finally
    end;
  finally
    Res.Free;
  end;
end;

end.

