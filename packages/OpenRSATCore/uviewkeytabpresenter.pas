unit uviewkeytabpresenter;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  base64,
  DateUtils,
  mormot.core.base,
  mormot.core.os.security,
  mormot.core.text,
  mormot.core.variants,
  mormot.crypt.secure;

type

  { IViewKeyTabView }

  IViewKeyTabView = Interface
    function GetFileName: RawUtf8;
    procedure SetFileName(AFileName: RawUtf8);
    function GetFolder: RawUtf8;
    procedure SetFolder(AFolder: RawUtf8);
    procedure LoadEntries(AEntries: PDocVariantData);
    procedure ClearEntries;
    procedure CopyToClipboard(AContent: RawByteString);
    function PickFolder(AInFolder: RawUtf8; out AOutFolder: RawUtf8): Boolean;
    function PickFile(AFolder: RawUtf8; out AFileName: RawUtf8): Boolean;
    procedure OpenFolder(AFolder: RawUtf8);
  end;

  { TViewKeyTabPresenter }

  TViewKeyTabPresenter = class
  private
    fView: IViewKeyTabView;
    fKeyTab: TKerberosKeyTab;
    fFolder: RawUtf8;

    procedure KeyTabEntryToDocEntry(AKeyTabEntry: TKerberosKeyEntry; ADocEntry: PDocVariantData);
    procedure KeyTabToDoc(ADoc: PDocVariantData);
  public
    constructor Create(AView: IViewKeyTabView);
    destructor Destroy; override;

    procedure LoadFromFile(AFileName: RawUtf8);
    procedure LoadFromKeyTab(AKeyTab: TKerberosKeyTab);
    procedure SaveToFile;
    procedure CopyToClipboard;
    procedure PickFile;
    procedure PickFolder;
  end;

implementation

{ TViewKeyTabPresenter }

procedure TViewKeyTabPresenter.KeyTabEntryToDocEntry(AKeyTabEntry: TKerberosKeyEntry; ADocEntry: PDocVariantData);
begin
  if not Assigned(ADocEntry) then
    Exit;

  ADocEntry^.AddOrUpdateValue('kvno', AKeyTabEntry.KeyVersion);
  ADocEntry^.AddOrUpdateValue('timestamp', FormatDateTime('dd/mm/yyyy hh:nn:ss', UnixToDateTime(AKeyTabEntry.Timestamp)));
  ADocEntry^.AddOrUpdateValue('principal', AKeyTabEntry.Principal);
  ADocEntry^.AddOrUpdateValue('encryptionType', ENCTYPE_NAME[AKeyTabEntry.EncType]);
  ADocEntry^.AddOrUpdateValue('key', BinToHex(AKeyTabEntry.Key));
end;

procedure TViewKeyTabPresenter.KeyTabToDoc(ADoc: PDocVariantData);
var
  i: Integer;
  DocEntry: TDocVariantData;
begin
  if not Assigned(ADoc) then
    Exit;

  ADoc^.Clear;
  DocEntry.Init(JSON_FAST);
  for i := 0 to High(fKeyTab.Entry) do
  begin
    KeyTabEntryToDocEntry(fKeyTab.Entry[i], @DocEntry);
    ADoc^.AddItem(DocEntry);
    DocEntry.Clear;
  end;
end;

constructor TViewKeyTabPresenter.Create(AView: IViewKeyTabView);
begin
  fView := AView;
  fKeyTab := TKerberosKeyTab.Create;
end;

destructor TViewKeyTabPresenter.Destroy;
begin
  FreeAndNil(fKeyTab);
  inherited Destroy;
end;

procedure TViewKeyTabPresenter.LoadFromFile(AFileName: RawUtf8);
var
  Doc: TDocVariantData;
  p: TStringArray;
begin
  fKeyTab.Clear;
  fKeyTab.LoadFromFile(AFileName);
  p := String(AFileName).Split(PathDelim);
  fView.SetFileName(p[High(p)]);
  Delete(p, High(p), 1);
  fFolder := String.Join(PathDelim, p);
  if not String(fFolder).EndsWith(PathDelim) then
    Append(fFolder, PathDelim);
  fView.SetFolder(fFolder);
  Doc.Init(JSON_FAST);
  KeyTabToDoc(@Doc);
  fView.LoadEntries(@Doc);
end;

procedure TViewKeyTabPresenter.LoadFromKeyTab(AKeyTab: TKerberosKeyTab);
var
  bin: RawByteString;
  Doc: TDocVariantData;
begin
  fKeyTab.Clear;
  bin := AKeyTab.SaveToBinary;
  fKeyTab.LoadFromBinary(bin);
  FillZero(bin);
  fView.SetFileName(FormatUtf8('%.keytab', [fKeyTab.MachineAccountPrincipal(True)]));
  Doc.Init(JSON_FAST);
  KeyTabToDoc(@Doc);
  fView.LoadEntries(@Doc);
end;

procedure TViewKeyTabPresenter.SaveToFile;
begin
  fKeyTab.SaveToFile(FormatUtf8('%%', [fView.GetFolder, fView.GetFileName]));
  fView.OpenFolder(fView.GetFolder);
end;

procedure TViewKeyTabPresenter.CopyToClipboard;
var
  b64Encoded: RawByteString;
begin
  b64Encoded := EncodeStringBase64(fKeyTab.SaveToBinary);
  fView.CopyToClipboard(b64Encoded);
  FillZero(b64Encoded); // anti-forensic
end;

procedure TViewKeyTabPresenter.PickFile;
var
  FileName: RawUtf8;
begin
  if not fView.PickFile(fFolder, FileName) then
    Exit;
  LoadFromFile(FileName);
end;

procedure TViewKeyTabPresenter.PickFolder;
begin
  if not fView.PickFolder(fFolder, fFolder) then
    Exit;
  fView.SetFolder(fFolder);
end;

end.

