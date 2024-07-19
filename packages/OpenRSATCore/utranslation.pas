unit utranslation;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  LCLTranslator,
  LResources,
  Translations;

function TranslateTo(Lang, unitname: string): boolean;
procedure ChangeLang(Lang: String);

implementation

function TranslateTo(Lang, unitname: string): boolean;
var
  res: TLResource;
  ii: Integer;
  POFile: TPOFile;
  LocalTranslator: TUpdateTranslator;
  tmp: TAbstractTranslator;
begin
  Result := false;

  //res := LazarusResources.Find(POFileName);
  //if not Assigned(res) then
  //  Exit;


  POFile := TPOFile.Create(Format('%slanguages\%s.%s.po', [Application.Location, unitname, Lang]));
  try
    //POFile.ReadPOText(res.Value);
    Result := Translations.TranslateResourceStrings(POFile);
    if not Result then
      Exit;

    LocalTranslator := TPOTranslator.Create(POFile);
    try
      tmp := LRSTranslator;
      LRSTranslator := LocalTranslator;
      for ii := 0 to Screen.CustomFormCount-1
        do LocalTranslator.UpdateTranslation(Screen.CustomForms[ii]);
      for ii := 0 to Screen.DataModuleCount-1
        do LocalTranslator.UpdateTranslation(Screen.DataModules[ii]);
      LRSTranslator := tmp;
    finally
      LocalTranslator.free;
    end;
    //POFile.Destroy; //DONT! Already done in LocalTranslator.Destroy
  finally
    //POFile.Free;
  end;
end;

procedure ChangeLang(Lang: String);
begin
  SetDefaultLang(lang);
  TranslateTo(Lang, 'uresourcestring');
end;

end.

