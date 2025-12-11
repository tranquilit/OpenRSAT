unit ufrmmoduleaducoption;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  ExtCtrls,
  StdCtrls,
  CheckLst,
  tis.ui.searchedit,
  mormot.core.base,
  mormot.core.log,
  umoduleaducoption,
  uoption,
  ufrmoption;

type

  { TFrmModuleADUCOption }

  TFrmModuleADUCOption = class(TFrameOption)
    CheckListBox2: TCheckListBox;
    Edit_SearchPageNumber: TEdit;
    Edit_SearchPageSize: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label_Filter: TLabel;
    Label_SearchPageNumber: TLabel;
    Label_SearchPageSize: TLabel;
    Memo2: TMemo;
    Memo_Filter: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    TisSearchEdit3: TTisSearchEdit;
    procedure CheckListBox2ClickCheck(Sender: TObject);
    procedure Edit_SearchPageNumberChange(Sender: TObject);
    procedure Edit_SearchPageSizeChange(Sender: TObject);
    procedure Memo2Change(Sender: TObject);
    procedure Memo_FilterChange(Sender: TObject);
  private
    fSection: String;
    fLog: TSynLog;
    fChanged: Boolean;

    fADUCOption: TModuleADUCOption;

    function GetGridFilter: RawUtf8;
    function GetSearchPageNumber: Integer;
    function GetSearchPageSize: Integer;
    function GetTreeFilter: RawUtf8;
    function GetTreeObjectClasses: TRawUtf8DynArray;
    procedure SetGridFilter(AValue: RawUtf8);
    procedure SetSearchPageNumber(AValue: Integer);
    procedure SetSearchPageSize(AValue: Integer);
    procedure SetTreeFilter(AValue: RawUtf8);
    procedure SetTreeObjectClasses(AValue: TRawUtf8DynArray);
  public
    constructor Create(TheOwner: TComponent; Option: TOption); override;

    property ADUCOption: TModuleADUCOption read fADUCOption;
  published
    // Inherited TFrameOptions
    function OptionChanged: Boolean; override;
    procedure Load; override;
    procedure Save; override;

    property SearchPageSize: Integer read GetSearchPageSize write SetSearchPageSize;
    property SearchPageNumber: Integer read GetSearchPageNumber write SetSearchPageNumber;
    property GridFilter: RawUtf8 read GetGridFilter write SetGridFilter;
    property TreeFilter: RawUtf8 read GetTreeFilter write SetTreeFilter;
    property TreeObjectClasses: TRawUtf8DynArray read GetTreeObjectClasses write SetTreeObjectClasses;
  end;

implementation
uses
  IniFiles,
  uconfig,
  mormot.core.text;

{$R *.lfm}

{ TFrmModuleADUCOption }

procedure TFrmModuleADUCOption.Memo2Change(Sender: TObject);
begin
  fChanged := True;
end;

procedure TFrmModuleADUCOption.Memo_FilterChange(Sender: TObject);
begin
  fChanged := True;
end;

procedure TFrmModuleADUCOption.Edit_SearchPageSizeChange(Sender: TObject);
begin
  fChanged := True;
end;

procedure TFrmModuleADUCOption.Edit_SearchPageNumberChange(Sender: TObject);
begin
  fChanged := True;
end;

procedure TFrmModuleADUCOption.CheckListBox2ClickCheck(Sender: TObject);
begin
  fChanged := True;
end;

procedure TFrmModuleADUCOption.SetGridFilter(AValue: RawUtf8);
begin
  if Memo_Filter.Text = AValue then
    Exit;
  Memo_Filter.Text := AValue;
end;

function TFrmModuleADUCOption.GetGridFilter: RawUtf8;
begin
  result := Memo_Filter.Text;
end;

function TFrmModuleADUCOption.GetSearchPageNumber: Integer;
begin
  TryStrToInt(Edit_SearchPageNumber.Text, result);
end;

function TFrmModuleADUCOption.GetSearchPageSize: Integer;
begin
  TryStrToInt(Edit_SearchPageSize.Text, result);
end;

function TFrmModuleADUCOption.GetTreeFilter: RawUtf8;
begin
  result := Memo2.Text;
end;

function TFrmModuleADUCOption.GetTreeObjectClasses: TRawUtf8DynArray;
var
  i: Integer;
begin
  result := [];
  for i := 0 to CheckListBox2.Count - 1 do
    if CheckListBox2.Checked[i] then
      Insert(CheckListBox2.Items[i], result, 0);
end;

procedure TFrmModuleADUCOption.SetSearchPageNumber(AValue: Integer);
begin
  Edit_SearchPageNumber.Text := IntToStr(AValue);
end;

procedure TFrmModuleADUCOption.SetSearchPageSize(AValue: Integer);
begin
  Edit_SearchPageSize.Text := IntToStr(AValue);
end;

procedure TFrmModuleADUCOption.SetTreeFilter(AValue: RawUtf8);
begin
  if Memo2.Text = AValue then
    Exit;
  Memo2.Text := AValue;
end;

procedure TFrmModuleADUCOption.SetTreeObjectClasses(AValue: TRawUtf8DynArray);
var
  i: Integer;

  function Contains(Arr: TRawUtf8DynArray; v: RawUtf8): Boolean;
  var
    ArrItem: RawUtf8;
  begin
    result := True;
    for ArrItem in Arr do
      if ArrItem = v then
        Exit;
    result := False;
  end;
begin
  for i := 0 to CheckListBox2.Count - 1 do
    CheckListBox2.Checked[i] := Contains(AValue, CheckListBox2.Items[i]);
end;

constructor TFrmModuleADUCOption.Create(TheOwner: TComponent; Option: TOption);
begin
  inherited Create(TheOwner);

  fLog := TSynLog.Add;

  fADUCOption := (Option as TModuleADUCOption);

  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);

  fSection := 'ADUC';
  fChanged := False;
end;

function TFrmModuleADUCOption.OptionChanged: Boolean;
begin
  result := fChanged;
end;

procedure TFrmModuleADUCOption.Load;
var
  IniFile: TIniFile;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Load', Self);

  if not Assigned(Self) then
  begin
    if Assigned(fLog) then
      fLog.Log(sllWarning, 'Could not be loaded: Self or Options not assigned.', Self);
    Exit;
  end;

  IniFile := TIniFile.Create(OptionFilePath);
  try
    ADUCOption.Load(IniFile);
  finally
    FreeAndNil(IniFile);
  end;

  SearchPageSize := ADUCOption.SearchPageSize;
  SearchPageNumber := ADUCOption.SearchPageNumber;
  GridFilter := ADUCOption.GridFilter;
  TreeFilter := ADUCOption.TreeFilter;
  TreeObjectClasses := ADUCOption.TreeObjectClasses;

  fChanged := False;
end;

procedure TFrmModuleADUCOption.Save;
var
  IniFile: TIniFile;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Save', Self);

  if not Assigned(Self) then
  begin
    if Assigned(fLog) then
      fLog.Log(sllWarning, 'Could not be saved: Self or Options not assigned.', Self);
    Exit;
  end;

  ADUCOption.SearchPageSize := SearchPageSize;
  ADUCOption.SearchPageNumber := SearchPageNumber;
  ADUCOption.GridFilter := GridFilter;
  ADUCOption.TreeFilter := TreeFilter;
  ADUCOption.TreeObjectClasses := TreeObjectClasses;

  IniFile := TIniFile.Create(OptionFilePath);
  try
    ADUCOption.Save(IniFile);
  finally
    FreeAndNil(IniFile);
  end;

  fChanged := False;
end;

end.

