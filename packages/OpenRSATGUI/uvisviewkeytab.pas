unit uvisviewkeytab;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  Clipbrd,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ExtCtrls,
  Buttons,
  LCLIntf,
  StdCtrls,
  ActnList,
  uviewkeytabpresenter,
  mormot.core.base,
  mormot.core.os.security,
  mormot.core.variants,
  tis.ui.grid.core;

type

  { TVisViewKeyTab }

  TVisViewKeyTab = class(TForm, IViewKeyTabView)
    Action_LoadFromFile: TAction;
    Action_SelectFolder: TAction;
    Action_SaveToFile: TAction;
    Action_CopyToClipboard: TAction;
    ActionList1: TActionList;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    TisGrid1: TTisGrid;
    procedure Action_CopyToClipboardExecute(Sender: TObject);
    procedure Action_LoadFromFileExecute(Sender: TObject);
    procedure Action_SaveToFileExecute(Sender: TObject);
    procedure Action_SaveToFileUpdate(Sender: TObject);
    procedure Action_SelectFolderExecute(Sender: TObject);
  private
    fPresenter: TViewKeyTabPresenter;

    function SelectPath(AFolder: RawUtf8; out APath: RawUtf8): Boolean;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetKeyTab(AKeyTab: TKerberosKeyTab);

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
uses
  ucommon;

{$R *.lfm}

{ TVisViewKeyTab }

procedure TVisViewKeyTab.Action_LoadFromFileExecute(Sender: TObject);
begin
  fPresenter.PickFile;
end;

procedure TVisViewKeyTab.Action_CopyToClipboardExecute(Sender: TObject);
begin
  fPresenter.CopyToClipboard;
end;

procedure TVisViewKeyTab.Action_SaveToFileExecute(Sender: TObject);
begin
  fPresenter.SaveToFile;
end;

procedure TVisViewKeyTab.Action_SaveToFileUpdate(Sender: TObject);
begin
  Action_SaveToFile.Enabled := (GetFolder <> '') and (GetFileName <> '');
end;

procedure TVisViewKeyTab.Action_SelectFolderExecute(Sender: TObject);
begin
  fPresenter.PickFolder;
end;

function TVisViewKeyTab.SelectPath(AFolder: RawUtf8; out APath: RawUtf8): Boolean;
begin
  SelectDirectoryDialog1.InitialDir := AFolder;
  result := SelectDirectoryDialog1.Execute;
  if result then
    APath := SelectDirectoryDialog1.FileName;
end;

constructor TVisViewKeyTab.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fPresenter := TViewKeyTabPresenter.Create(Self);
end;

destructor TVisViewKeyTab.Destroy;
begin
  FreeAndNil(fPresenter);

  inherited Destroy;
end;

procedure TVisViewKeyTab.SetKeyTab(AKeyTab: TKerberosKeyTab);
begin
  fPresenter.LoadFromKeyTab(AKeyTab);
end;

function TVisViewKeyTab.GetFileName: RawUtf8;
begin
  result := Edit1.Text;
end;

procedure TVisViewKeyTab.SetFileName(AFileName: RawUtf8);
begin
  Edit1.Text := AFileName;
end;

function TVisViewKeyTab.GetFolder: RawUtf8;
begin
  result := Edit2.Text;
  if not String(result).EndsWith('/') then
    result := result + '/';
end;

procedure TVisViewKeyTab.SetFolder(AFolder: RawUtf8);
begin
  Edit2.Text := AFolder;
end;

procedure TVisViewKeyTab.LoadEntries(AEntries: PDocVariantData);
begin
  TisGrid1.LoadData(AEntries);
end;

procedure TVisViewKeyTab.ClearEntries;
begin
  TisGrid1.Clear;
end;

procedure TVisViewKeyTab.CopyToClipboard(AContent: RawByteString);
begin
  Clipboard.AsText := AContent;
end;

function TVisViewKeyTab.PickFolder(AInFolder: RawUtf8; out AOutFolder: RawUtf8): Boolean;
begin
  result := SelectPath(AInFolder, AOutFolder);
end;

function TVisViewKeyTab.PickFile(AFolder: RawUtf8; out AFileName: RawUtf8): Boolean;
begin
  result := SelectPath(AFolder, AFileName);
end;

procedure TVisViewKeyTab.OpenFolder(AFolder: RawUtf8);
begin
  if (mrYes <> MessageDlg(rsGenerateKeyTab, rsGenerateKeyTabFinished, mtConfirmation, mbYesNoCancel, 0)) then
    Exit;
  OpenDocument(AFolder);
end;

end.

