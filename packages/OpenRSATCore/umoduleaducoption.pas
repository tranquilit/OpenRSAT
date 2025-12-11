unit umoduleaducoption;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  IniFiles,
  mormot.core.base,
  mormot.core.log,
  uoption;

type

  { TModuleADUCOption }

  TModuleADUCOption = class(TOption)
  private
    fLog: TSynLog;
    fChanged: Boolean;

    fObservers: Array of TProcRsatOptionOfObject;

    fSearchPageSize: Integer;
    fSearchPageNumber: Integer;
    fGridFilter: RawUtf8;
    fTreeFilter: RawUtf8;
    fTreeObjectClasses: TRawUtf8DynArray;
    fShowGPO: Boolean;

    procedure SetGridFilter(AValue: RawUtf8);
    procedure SetSearchPageNumber(AValue: Integer);
    procedure SetSearchPageSize(AValue: Integer);
    procedure SetShowGPO(AValue: Boolean);
    procedure SetTreeFilter(AValue: RawUtf8);
    procedure SetTreeObjectClasses(AValue: TRawUtf8DynArray);

  public
    constructor Create;
    destructor Destroy; override;

    property SearchPageSize: Integer read fSearchPageSize write SetSearchPageSize;
    property SearchPageNumber: Integer read fSearchPageNumber write SetSearchPageNumber;
    property GridFilter: RawUtf8 read fGridFilter write SetGridFilter;
    property TreeFilter: RawUtf8 read fTreeFilter write SetTreeFilter;
    property TreeObjectClasses: TRawUtf8DynArray read fTreeObjectClasses write SetTreeObjectClasses;
    property ShowGPO: Boolean read fShowGPO write SetShowGPO;

  /// TOption
  public
    procedure Load(IniFile: TIniFile); override;
    procedure Save(IniFile: TIniFile); override;
    function Changed: Boolean; override;
    procedure RegisterObserver(Observer: TProcRsatOptionOfObject); override;
    procedure RemoveObserver(Observer: TProcRsatOptionOfObject); override;
  end;

implementation

{ TModuleADUCOption }

procedure TModuleADUCOption.SetGridFilter(AValue: RawUtf8);
begin
  if fGridFilter = AValue then
    Exit;
  fGridFilter := AValue;

  fChanged := True;
end;

procedure TModuleADUCOption.SetSearchPageNumber(AValue: Integer);
begin
  if fSearchPageNumber = AValue then
    Exit;
  fSearchPageNumber := AValue;

  fChanged := True;
end;

procedure TModuleADUCOption.SetSearchPageSize(AValue: Integer);
begin
  if fSearchPageSize = AValue then
    Exit;
  fSearchPageSize := AValue;

  fChanged := True;
end;

procedure TModuleADUCOption.SetShowGPO(AValue: Boolean);
begin
  if fShowGPO = AValue then
    Exit;
  fShowGPO := AValue;

  fChanged := True;
end;

procedure TModuleADUCOption.SetTreeFilter(AValue: RawUtf8);
begin
  if fTreeFilter = AValue then
    Exit;
  fTreeFilter := AValue;

  fChanged := True;
end;

procedure TModuleADUCOption.SetTreeObjectClasses(AValue: TRawUtf8DynArray);
begin
  if fTreeObjectClasses = AValue then
    Exit;
  fTreeObjectClasses := AValue;

  fChanged := True;
end;

constructor TModuleADUCOption.Create;
begin
  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);

  fObservers := [];
  fChanged := False;
end;

destructor TModuleADUCOption.Destroy;
begin
  inherited Destroy;
end;

procedure TModuleADUCOption.Load(IniFile: TIniFile);
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
  fTreeObjectClasses := TRawUtf8DynArray(IniFile.ReadString(Section, 'TreeObjectClasses', DEFAULT_TREE_OBJECT_CLASSES).Split(';'));

  fChanged := False;
end;

procedure TModuleADUCOption.Save(IniFile: TIniFile);
const
  SECTION = 'ADUC';
begin
  if Assigned(fLog) then
    fLog.Log(sllDebug, 'Save', Self);

  IniFile.WriteInt64(SECTION, 'SearchPageSize', fSearchPageSize);
  IniFile.WriteInt64(SECTION, 'SearchPageNumber', fSearchPageNumber);
  IniFile.WriteString(SECTION, 'GridFilter', fGridFilter);
  IniFile.WriteString(SECTION, 'TreeFilter', fTreeFilter);
  IniFile.WriteString(SECTION, 'TreeObjectClasses', String.Join(';', TStringArray(fTreeObjectClasses)));

  fChanged := False;
end;

function TModuleADUCOption.Changed: Boolean;
begin
  result := fChanged;
end;

procedure TModuleADUCOption.RegisterObserver(Observer: TProcRsatOptionOfObject);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'RegisterObserver', Self);

  MultiEventAdd(fObservers, TMethod(Observer));
end;

procedure TModuleADUCOption.RemoveObserver(Observer: TProcRsatOptionOfObject);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'RemoveObserver', Self);

  MultiEventRemove(fObservers, TMethod(Observer));
end;

end.

