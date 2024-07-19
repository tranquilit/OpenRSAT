unit ufrmmoduleaducoptions;

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
  IniFiles,
  uinterfacemodule,
  tis.ui.searchedit,
  mormot.core.base,
  mormot.core.log;

type

  { TFrmModuleADUCOptions }

  TFrmModuleADUCOptions = class(TFrameOptions)
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

    function GetGridFilter: String;
    function GetSearchPageNumber: Integer;
    function GetSearchPageSize: Integer;
    function GetTreeFilter: String;
    function GetTreeObjectClasses: TStringArray;
    procedure SetGridFilter(AValue: String);
    procedure SetSearchPageNumber(AValue: Integer);
    procedure SetSearchPageSize(AValue: Integer);
    procedure SetTreeFilter(AValue: String);
    procedure SetTreeObjectClasses(AValue: TStringArray);
  public
    constructor Create(TheOwner: TComponent); override;
  published
    // Inherited TFrameOptions
    function OptionChanged: Boolean; override;
    procedure Load(Options: TOptions); override;
    procedure Save(Options: TOptions); override;

    property SearchPageSize: Integer read GetSearchPageSize write SetSearchPageSize;
    property SearchPageNumber: Integer read GetSearchPageNumber write SetSearchPageNumber;
    property GridFilter: String read GetGridFilter write SetGridFilter;
    property TreeFilter: String read GetTreeFilter write SetTreeFilter;
    property TreeObjectClasses: TStringArray read GetTreeObjectClasses write SetTreeObjectClasses;
  end;

  { TModuleADUCOptions }

  TModuleADUCOptions = class(TOptions)

  private
    fSection: String;
    fLog: TSynLog;
    fChanged: Boolean;
    fFrame: TFrmModuleADUCOptions;

    fObservers: Array of TProcRsatOptionsOfObject;

    fSearchPageSize: Integer;
    fSearchPageNumber: Integer;
    fGridFilter: String;
    fTreeFilter: String;
    fTreeObjectClasses: TStringArray;
    fShowGPO: Boolean;

    procedure SetGridFilter(AValue: String);
    procedure SetSearchPageNumber(AValue: Integer);
    procedure SetSearchPageSize(AValue: Integer);
    procedure SetShowGPO(AValue: Boolean);
    procedure SetTreeFilter(AValue: String);
    procedure SetTreeObjectClasses(AValue: TStringArray);
  public
    // Inherited TOptions
    procedure Load(IniFile: TIniFile); override;
    procedure Save(IniFile: TIniFile); override;
    function Changed: Boolean; override;
    function OptionName: String; override;
    procedure CreateFrame(TheOwner: TComponent); override;
    function GetFrame: TFrameOptions; override;
    procedure DeleteFrame; override;
    procedure RegisterObserver(Observer: TProcRsatOptionsOfObject); override;
    procedure RemoveObserver(Observer: TProcRsatOptionsOfObject); override;

    property SearchPageSize: Integer read fSearchPageSize write SetSearchPageSize;
    property SearchPageNumber: Integer read fSearchPageNumber write SetSearchPageNumber;
    property GridFilter: String read fGridFilter write SetGridFilter;
    property TreeFilter: String read fTreeFilter write SetTreeFilter;
    property TreeObjectClasses: TStringArray read fTreeObjectClasses write SetTreeObjectClasses;
    property ShowGPO: Boolean read fShowGPO write SetShowGPO;
  end;

implementation
uses
  mormot.core.text;

{$R *.lfm}

{ TFrmModuleADUCOptions }

procedure TFrmModuleADUCOptions.Memo2Change(Sender: TObject);
begin
  fChanged := True;
end;

procedure TFrmModuleADUCOptions.Memo_FilterChange(Sender: TObject);
begin
  fChanged := True;
end;

procedure TFrmModuleADUCOptions.Edit_SearchPageSizeChange(Sender: TObject);
begin
  fChanged := True;
end;

procedure TFrmModuleADUCOptions.Edit_SearchPageNumberChange(Sender: TObject);
begin
  fChanged := True;
end;

procedure TFrmModuleADUCOptions.CheckListBox2ClickCheck(Sender: TObject);
begin
  fChanged := True;
end;

procedure TFrmModuleADUCOptions.SetGridFilter(AValue: String);
begin
  if Memo_Filter.Text = AValue then
    Exit;
  Memo_Filter.Text := AValue;
end;

function TFrmModuleADUCOptions.GetGridFilter: String;
begin
  result := Memo_Filter.Text;
end;

function TFrmModuleADUCOptions.GetSearchPageNumber: Integer;
begin
  TryStrToInt(Edit_SearchPageNumber.Text, result);
end;

function TFrmModuleADUCOptions.GetSearchPageSize: Integer;
begin
  TryStrToInt(Edit_SearchPageSize.Text, result);
end;

function TFrmModuleADUCOptions.GetTreeFilter: String;
begin
  result := Memo2.Text;
end;

function TFrmModuleADUCOptions.GetTreeObjectClasses: TStringArray;
var
  i: Integer;
begin
  result := [];
  for i := 0 to CheckListBox2.Count - 1 do
    if CheckListBox2.Checked[i] then
      Insert(CheckListBox2.Items[i], result, 0);
end;

procedure TFrmModuleADUCOptions.SetSearchPageNumber(AValue: Integer);
begin
  Edit_SearchPageNumber.Text := IntToStr(AValue);
end;

procedure TFrmModuleADUCOptions.SetSearchPageSize(AValue: Integer);
begin
  Edit_SearchPageSize.Text := IntToStr(AValue);
end;

procedure TFrmModuleADUCOptions.SetTreeFilter(AValue: String);
begin
  if Memo2.Text = AValue then
    Exit;
  Memo2.Text := AValue;
end;

procedure TFrmModuleADUCOptions.SetTreeObjectClasses(AValue: TStringArray);
var
  i: Integer;

  function Contains(Arr: TStringArray; v: String): Boolean;
  var
    ArrItem: String;
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

constructor TFrmModuleADUCOptions.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fLog := TSynLog.Add;

  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);

  fSection := 'ADUC';
  fChanged := False;
end;

function TFrmModuleADUCOptions.OptionChanged: Boolean;
begin
  result := fChanged;
end;

procedure TFrmModuleADUCOptions.Load(Options: TOptions);
var
  ADUCOptions: TModuleADUCOptions absolute Options;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Load', Self);

  if not Assigned(Self) or not Assigned(ADUCOptions) then
  begin
    if Assigned(fLog) then
      fLog.Log(sllWarning, 'Could not be loaded: Self or Options not assigned.', Self);
    Exit;
  end;

  SearchPageSize := ADUCOptions.SearchPageSize;
  SearchPageNumber := ADUCOptions.SearchPageNumber;
  GridFilter := ADUCOptions.GridFilter;
  TreeFilter := ADUCOptions.TreeFilter;
  TreeObjectClasses := ADUCOptions.TreeObjectClasses;

  fChanged := False;
end;

procedure TFrmModuleADUCOptions.Save(Options: TOptions);
var
  ADUCOptions: TModuleADUCOptions absolute Options;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Save', Self);

  if not Assigned(Self) or not Assigned(Options) then
  begin
    if Assigned(fLog) then
      fLog.Log(sllWarning, 'Could not be saved: Self or Options not assigned.', Self);
    Exit;
  end;

  ADUCOptions.SearchPageSize := SearchPageSize;
  ADUCOptions.SearchPageNumber := SearchPageNumber;
  ADUCOptions.GridFilter := GridFilter;
  ADUCOptions.TreeFilter := TreeFilter;
  ADUCOptions.TreeObjectClasses := TreeObjectClasses;

  fChanged := False;
end;

{ TModuleADUCOptions }

procedure TModuleADUCOptions.SetGridFilter(AValue: String);
begin
  if fGridFilter = AValue then
    Exit;
  fGridFilter := AValue;

  fChanged := True;
end;

procedure TModuleADUCOptions.SetSearchPageNumber(AValue: Integer);
begin
  if fSearchPageNumber = AValue then
    Exit;
  fSearchPageNumber := AValue;

  fChanged := True;
end;

procedure TModuleADUCOptions.SetSearchPageSize(AValue: Integer);
begin
  if fSearchPageSize = AValue then
    Exit;
  fSearchPageSize := AValue;

  fChanged := True;
end;

procedure TModuleADUCOptions.SetShowGPO(AValue: Boolean);
begin
  if fShowGPO = AValue then
    Exit;
  fShowGPO := AValue;

  fChanged := True;
end;

procedure TModuleADUCOptions.SetTreeFilter(AValue: String);
begin
  if fTreeFilter = AValue then
    Exit;
  fTreeFilter := AValue;

  fChanged := True;
end;

procedure TModuleADUCOptions.SetTreeObjectClasses(AValue: TStringArray);
begin
  if fTreeObjectClasses = AValue then
    Exit;
  fTreeObjectClasses := AValue;

  fChanged := True;
end;

procedure TModuleADUCOptions.Load(IniFile: TIniFile);
const
  Section = 'ADUC';
  DEFAULT_TREE_OBJECT_CLASSES = 'container;organizationalUnit;lostAndFound;builtinDomain;msDS-QuotaContainer;msTPM-InformationObjectsContainer';
begin
  if Assigned(fLog) then
    fLog.Log(sllDebug, 'Load', Self);

  fSearchPageSize := IniFile.ReadInt64(Section, 'SearchPageSize', 1000);
  fSearchPageNumber := IniFile.ReadInt64(Section, 'SearchPageNumber', 2);
  fGridFilter := IniFile.ReadString(Section, 'GridFilter', '');
  fTreeFilter := IniFile.ReadString(Section, 'TreeFilter', '');
  fTreeObjectClasses := IniFile.ReadString(Section, 'TreeObjectClasses', DEFAULT_TREE_OBJECT_CLASSES).Split(';');

  fChanged := False;
end;

procedure TModuleADUCOptions.Save(IniFile: TIniFile);
const
  SECTION = 'ADUC';
begin
  if Assigned(fLog) then
    fLog.Log(sllDebug, 'Save', Self);

  if Assigned(fFrame) and fFrame.OptionChanged then
    fFrame.Save(Self);

  IniFile.WriteInt64(SECTION, 'SearchPageSize', fSearchPageSize);
  IniFile.WriteInt64(SECTION, 'SearchPageNumber', fSearchPageNumber);
  IniFile.WriteString(SECTION, 'GridFilter', fGridFilter);
  IniFile.WriteString(SECTION, 'TreeFilter', fTreeFilter);
  IniFile.WriteString(SECTION, 'TreeObjectClasses', String.Join(';', fTreeObjectClasses));

  fChanged := False;
end;

function TModuleADUCOptions.Changed: Boolean;
begin
  result := fChanged or fFrame.OptionChanged;
end;

function TModuleADUCOptions.OptionName: String;
begin
  result := 'ADUC';
end;

procedure TModuleADUCOptions.CreateFrame(TheOwner: TComponent);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'CreateFrame', Self);

  fFrame := TFrmModuleADUCOptions.Create(TheOwner);
  fFrame.Load(Self);
  RegisterObserver(@fFrame.Load);
end;

function TModuleADUCOptions.GetFrame: TFrameOptions;
begin
  result := fFrame;
end;

procedure TModuleADUCOptions.DeleteFrame;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'DeleteFrame', Self);

  RemoveObserver(@fFrame.Load);
  FreeAndNil(fFrame);
end;

procedure TModuleADUCOptions.RegisterObserver(Observer: TProcRsatOptionsOfObject
  );
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'RegisterObserver', Self);

  MultiEventAdd(fObservers, TMethod(Observer));
end;

procedure TModuleADUCOptions.RemoveObserver(Observer: TProcRsatOptionsOfObject);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'RemoveObserver', Self);

  MultiEventRemove(fObservers, TMethod(Observer));
end;

end.

