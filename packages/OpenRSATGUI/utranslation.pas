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

function LanguageFolderPath(const Lang: RawUtf8): RawUtf8;
begin
  result := MakePath([GetAppConfigDir(False), 'languages', Lang]);
end;

procedure SetLanguageResourceStrings(const Lang: RawUtf8);
var
  PO: TPOFile;
  FilePath: TFileName;
begin
  FilePath := MakePath([LanguageFolderPath(Lang), 'ucommon.po']);
  if not FileExists(FilePath) then
    Exit;

  PO := TPOFile.Create(FilePath);
  try
    if not Translations.TranslateUnitResourceStrings('ucommon', PO) then
      Exit;
  finally
    FreeAndNil(PO);
  end;
end;

procedure SetLanguageForms(const Lang: RawUtf8);
const
  FormsFileNames: TRawUtf8DynArray = ('OpenRSATGUI.po', 'OpenRSAT.po');
var
  Translator: TPOTranslator;
  POFile, TempPOFile: TPOFile;
  item, NewItem: TPOFileItem;
  i, j: Integer;
  FilePath: TFileName;
begin
  TempPOFile := TPOFile.Create;
  try
    for j := 0 to High(FormsFileNames) do
    begin
      FilePath := MakePath([LanguageFolderPath(Lang), FormsFileNames[j]]);
      if not FileExists(FilePath) then
        Continue;
      POFile := TPOFile.Create(FilePath);
      try
        for i := 0 to POFile.Count - 1 do
        begin
          NewItem := nil;
          item := POFile.PoItems[i];
          TempPOFile.FillItem(
            NewItem,
            Item.IdentifierLow,
            item.Original,
            item.Translation,
            item.Comments,
            item.Context,
            item.Flags,
            item.PreviousID,
            item.LineNr
          );
        end;
      finally
        FreeAndNil(POFile);
      end;
    end;
  finally
    Translator := TPOTranslator.Create(TempPOFile);
    TempPOFile := nil;
    FreeAndNil(LRSTranslator);
    LRSTranslator := Translator;

    for I := 0 to Pred(Screen.CustomFormCount) do
      Translator.UpdateTranslation(Screen.CustomForms[I]);
    for I := 0 to Pred(Screen.DataModuleCount) do
      Translator.UpdateTranslation(Screen.DataModules[I]);
  end;
end;

procedure SetLanguage(const Lang: RawUtf8);
begin
  SetLanguageResourceStrings(Lang);
  SetLanguageForms(Lang);
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

