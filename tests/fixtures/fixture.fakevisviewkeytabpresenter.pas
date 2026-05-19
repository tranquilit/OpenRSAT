unit fixture.fakevisviewkeytabpresenter;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.base,
  mormot.core.variants,
  uviewkeytabpresenter,
  fixture.basefakevis;

type

  { TFakeVisViewKeyTabPresenter }

  TFakeVisViewKeyTabPresenter = class(TFakeForm, IViewKeyTabView)
  public
    FileName: RawUtf8;
    Folder: RawUtf8;
    Entries: TDocVariantData;
    ClipboardContent: RawByteString;
  public // IViewKeyTabView
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

implementation

{ TFakeVisViewKeyTabPresenter }

function TFakeVisViewKeyTabPresenter.GetFileName: RawUtf8;
begin
  result := FileName;
end;

procedure TFakeVisViewKeyTabPresenter.SetFileName(AFileName: RawUtf8);
begin
  FileName := AFileName;
end;

function TFakeVisViewKeyTabPresenter.GetFolder: RawUtf8;
begin
  result := Folder;
end;

procedure TFakeVisViewKeyTabPresenter.SetFolder(AFolder: RawUtf8);
begin
  Folder := AFolder;
end;

procedure TFakeVisViewKeyTabPresenter.LoadEntries(AEntries: PDocVariantData);
begin
  Entries.InitFrom(AEntries^, True);
end;

procedure TFakeVisViewKeyTabPresenter.ClearEntries;
begin
end;

procedure TFakeVisViewKeyTabPresenter.CopyToClipboard(AContent: RawByteString);
begin
  ClipboardContent := AContent;
end;

function TFakeVisViewKeyTabPresenter.PickFolder(AInFolder: RawUtf8; out AOutFolder: RawUtf8): Boolean;
begin

end;

function TFakeVisViewKeyTabPresenter.PickFile(AFolder: RawUtf8; out AFileName: RawUtf8): Boolean;
begin

end;

procedure TFakeVisViewKeyTabPresenter.OpenFolder(AFolder: RawUtf8);
begin

end;

end.

