unit ufrmmoduleadssoptions;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  ExtCtrls,
  StdCtrls,
  IniFiles,
  mormot.core.log,
  uinterfacemodule;

type

  { TFrmModuleADSSOptions }

  TFrmModuleADSSOptions = class(TFrameOptions)
    CheckBox1: TCheckBox;
    Label1: TLabel;
    Panel1: TPanel;
    procedure CheckBox1Change(Sender: TObject);
  private
    fLog: TSynLog;
    fChanged: Boolean;
    function GetShowService: Boolean;
    procedure SetShowService(AValue: Boolean);

  published
    // TFrameOptions
    function OptionChanged: Boolean; override;
    procedure Load(Options: TOptions); override;
    procedure Save(Options: TOptions); override;

  public
    property ShowService: Boolean read GetShowService write SetShowService;
  end;

  { TModuleADSSOptions }

  TModuleADSSOptions = class(TOptions)
  private
    fLog: TSynLog;
    fFrame: TFrmModuleADSSOptions;
    fChanged: Boolean;

    fObservers: Array of TProcRsatOptionsOfObject;

    fShowService: Boolean;
    function GetShowService: Boolean;
    procedure SetShowService(AValue: Boolean);

    procedure Notify;
  public
    // TOptions
    procedure Load(IniFile: TIniFile); override;
    procedure Save(IniFile: TIniFile); override;
    function Changed: Boolean; override;
    function OptionName: String; override;
    procedure CreateFrame(TheOwner: TComponent); override;
    function GetFrame: TFrameOptions; override;
    procedure DeleteFrame; override;
    procedure RegisterObserver(Observer: TProcRsatOptionsOfObject); override;
    procedure RemoveObserver(Observer: TProcRsatOptionsOfObject); override;

    property ShowService: Boolean read GetShowService write SetShowService;
  end;

implementation
uses
  mormot.core.base;

{$R *.lfm}

{ TFrmModuleADSSOptions }

procedure TFrmModuleADSSOptions.CheckBox1Change(Sender: TObject);
begin
  fChanged := True;
end;

function TFrmModuleADSSOptions.GetShowService: Boolean;
begin
  result := CheckBox1.Checked;
end;

procedure TFrmModuleADSSOptions.SetShowService(AValue: Boolean);
begin
  if (AValue = CheckBox1.Checked) then
    Exit;

  CheckBox1.Checked := AValue;
  fChanged := True;
end;

function TFrmModuleADSSOptions.OptionChanged: Boolean;
begin
  result := fChanged;
end;

procedure TFrmModuleADSSOptions.Load(Options: TOptions);
var
  ADSSOptions: TModuleADSSOptions absolute Options;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Load', Self);

  if not Assigned(Self) or not Assigned(ADSSOptions) then
  begin
    if Assigned(fLog) then
      fLog.Log(sllWarning, 'Could not be loaded: Self of Options not assigned', Self);
    Exit;
  end;

  ShowService := ADSSOptions.ShowService;

  fChanged := False;
end;

procedure TFrmModuleADSSOptions.Save(Options: TOptions);
var
  ADSSOptions: TModuleADSSOptions absolute Options;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Save', Self);

  ADSSOptions.ShowService := ShowService;

  fChanged := False;
end;

{ TModuleADSSOptions }

function TModuleADSSOptions.GetShowService: Boolean;
begin
  result := fShowService;
end;

procedure TModuleADSSOptions.SetShowService(AValue: Boolean);
begin
  if AValue = fShowService then
    Exit;

  fShowService := Avalue;
  fChanged := True;
end;

procedure TModuleADSSOptions.Notify;
var
  Observer: TProcRsatOptionsOfObject;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Notify', Self);

  for Observer in fObservers do
    Observer(Self);
end;

procedure TModuleADSSOptions.Load(IniFile: TIniFile);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Load', Self);

  fShowService := IniFile.ReadBool(OptionName, 'ShowService', False);

  fChanged := False;
  Notify;
end;

procedure TModuleADSSOptions.Save(IniFile: TIniFile);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Save', Self);

  if Assigned(fFrame) and fFrame.OptionChanged then
    fFrame.Save(Self);

  IniFile.WriteBool(OptionName, 'ShowService', fShowService);

  fChanged := False;
  Notify;
end;

function TModuleADSSOptions.Changed: Boolean;
begin
  result := fChanged or fFrame.OptionChanged;
end;

function TModuleADSSOptions.OptionName: String;
begin
  result := 'ADSS';
end;

procedure TModuleADSSOptions.CreateFrame(TheOwner: TComponent);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'CreateFrame', Self);

  fFrame := TFrmModuleADSSOptions.Create(TheOwner);
  fFrame.Load(Self);
  RegisterObserver(@fFrame.Load);
end;

function TModuleADSSOptions.GetFrame: TFrameOptions;
begin
  result := fFrame;
end;

procedure TModuleADSSOptions.DeleteFrame;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'DeleteFrame', Self);

  RemoveObserver(@fFrame.Load);
  FreeAndNil(fFrame);
end;

procedure TModuleADSSOptions.RegisterObserver(Observer: TProcRsatOptionsOfObject
  );
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'RegisterObserver', Self);

  MultiEventAdd(fObservers, TMethod(Observer));
end;

procedure TModuleADSSOptions.RemoveObserver(Observer: TProcRsatOptionsOfObject);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'RemoveObserver', Self);

  MultiEventRemove(fObservers, TMethod(Observer));
end;

end.

