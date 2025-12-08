unit ufrmrsatoptions;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  StdCtrls,
  uinterfacemodule,
  mormot.core.log,
  IniFiles;

type

  TThemeMode = (tmLight, tmDark, tmSystem);

  TModuleOptionsArray = Array of TOptions;

  { TFrmRsatOptions }

  TFrmRsatOptions = class(TFrameOptions)
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure ComboBox3Change(Sender: TObject);
  private
    fLog: TSynLog;
    fChanged: Boolean;
  public
    // Inherited TFrame
    constructor Create(TheOwner: TComponent); reintroduce;
    destructor Destroy; override;

  published
    // Inherited TFrameOptions
    function OptionChanged: Boolean; override;
    procedure Load(Options: TOptions); override;
    procedure Save(Options: TOptions); override;
  end;

  { TRsatOptions }

  TRsatOptions = class(TOptions)
  private
    // If assigned, enable logging activities
    fLog: TSynLog;
    fUpdating: Integer;
    fOptionsPath: String;
    fChanged: Boolean;

    fLang: String;
    fTheme: TThemeMode;
    fAdvancedView: Boolean;

    fFrame: TFrmRsatOptions;
    fModulesOptions: TModuleOptionsArray;
    fObservers: Array of TProcRsatOptionsOfObject;

    procedure Notify;
    procedure SetAdvancedView(AValue: Boolean);
    procedure SetLang(AValue: String);
    procedure SetTheme(AValue: TThemeMode);
  public
    // Inherited
    procedure Load(IniFile: TIniFile); override;
    procedure Save(IniFile: TIniFile); override;
    // Options or FrameOptions has changed
    function Changed: Boolean; override;
    function OptionName: String; override;
    procedure CreateFrame(TheOwner: TComponent); override;
    function GetFrame: TFrameOptions; override;
    procedure DeleteFrame; override;
    procedure RegisterObserver(Observer: TProcRsatOptionsOfObject); override;
    procedure RemoveObserver(Observer: TProcRsatOptionsOfObject); override;

    constructor Create;
    procedure Load; overload;
    procedure Save; overload;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure AddOptions(Options: TOptions);

    property Lang: String read fLang write SetLang;
    property Theme: TThemeMode read fTheme write SetTheme;
    property AdvancedView: Boolean read fAdvancedView write SetAdvancedView;

    property ModulesOptions: TModuleOptionsArray read fModulesOptions;
  end;

implementation

uses
  mormot.core.text,
  mormot.core.base;

{$R *.lfm}

{ TFrmRsatOptions }

procedure TFrmRsatOptions.ComboBox3Change(Sender: TObject);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Change AdvancedView (%)', [ComboBox3.Text], Self);

  fChanged := True;
end;

constructor TFrmRsatOptions.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fChanged := False;
  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllInfo, 'Create', Self);
end;

function TFrmRsatOptions.OptionChanged: Boolean;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'OptionChanged (%)', [fChanged], Self);

  result := fChanged;
end;

procedure TFrmRsatOptions.Load(Options: TOptions);
var
  RSATOptions: TRsatOptions absolute Options;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Load', Self);

  if not Assigned(Self) or not Assigned(Options) then
  begin
    if Assigned(fLog) then
      fLog.Log(sllWarning, 'Could not be loaded: Self or Options not assigned.', Self);
    Exit;
  end;

  ComboBox1.ItemIndex := Ord(RSATOptions.Theme);
  ComboBox3.ItemIndex := Ord(RSATOptions.AdvancedView);

  fChanged := False;
end;

procedure TFrmRsatOptions.Save(Options: TOptions);
var
  RSATOptions: TRsatOptions absolute Options;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Save', Self);

  if not Assigned(Self) or not Assigned(Options) then
  begin
    if Assigned(fLog) then
      fLog.Log(sllWarning, 'Could not be saved: Self or Options not assigned.', Self);
    Exit;
  end;

  RSATOptions.Theme := TThemeMode(ComboBox1.ItemIndex);
  RSATOptions.AdvancedView := Boolean(ComboBox3.ItemIndex);

  fChanged := False;
end;

destructor TFrmRsatOptions.Destroy;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Destroy', Self);

  inherited Destroy;
end;

{ TRsatOptions }

procedure TRsatOptions.Notify;
var
  Observer: TProcRsatOptionsOfObject;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Notify', Self);

  for Observer in fObservers do
    Observer(Self);
end;

procedure TRsatOptions.SetAdvancedView(AValue: Boolean);
begin
  if fAdvancedView = AValue then
    Exit;
  fAdvancedView := AValue;

  if (fUpdating = 0) then
    Save;
end;

procedure TRsatOptions.SetLang(AValue: String);
begin
  if fLang = AValue then
    Exit;
  fLang := AValue;

  if (fUpdating = 0) then
  begin
    Save;
    Notify;
  end;
end;

procedure TRsatOptions.SetTheme(AValue: TThemeMode);
begin
  if fTheme = AValue then
    Exit;
  fTheme := AValue;

  if (fUpdating = 0) then
  begin
    Save;
    Notify;
  end;
end;

constructor TRsatOptions.Create;
begin
  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);

  fOptionsPath := MakePath([GetAppConfigDir(False), 'options.ini']);
  if Assigned(fLog) then
    fLog.Log(sllInfo, 'Options path: %', [fOptionsPath], Self);
end;

procedure TRsatOptions.Load(IniFile: TIniFile);
var
  ModuleOptions: TOptions;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Load', Self);

  // Load own options.
  fTheme := TThemeMode(IniFile.ReadInt64('General', 'theme', Ord(tmSystem)));
  fLang := IniFile.ReadString('General', 'lang', '');
  AdvancedView := IniFile.ReadBool('General', 'advancedView', False);

  fChanged := False;

  // Save modules options.
  for ModuleOptions in fModulesOptions do
    if Assigned(ModuleOptions) then
      ModuleOptions.Load(IniFile);

  Notify;
end;

procedure TRsatOptions.Load;
var
  IniFile: TIniFile;
begin
  if Assigned(fLog) then
    fLog.Log(sllInfo, 'Load options from: %', [fOptionsPath], Self);

  // Create ini file
  IniFile := TIniFile.Create(fOptionsPath);
  try
    Load(IniFile);
  finally
    FreeAndNil(IniFile);
  end;
end;

procedure TRsatOptions.Save(IniFile: TIniFile);
var
  ModuleOptions: TOptions;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Save', Self);

  if Assigned(fFrame) and fFrame.OptionChanged then
    fFrame.Save(Self);

  // Save own options.
  IniFile.WriteInt64('General', 'theme', Ord(fTheme));
  IniFile.WriteString('General', 'lang', fLang);
  IniFile.WriteBool('General', 'advancedView', fAdvancedView);

  fChanged := False;

  // Save modules options.
  for ModuleOptions in fModulesOptions do
    if Assigned(ModuleOptions) then
      ModuleOptions.Save(IniFile);

  Notify;
end;

procedure TRsatOptions.Save;
var
  IniFile: TIniFile;
begin
  if Assigned(fLog) then
    fLog.Log(sllInfo, 'Save options to: %', [fOptionsPath], Self);

  // Create ini file
  IniFile := TIniFile.Create(fOptionsPath);
  try
    Save(IniFile);
    fChanged := False;
  finally
    FreeAndNil(IniFile);
  end;
end;

function TRsatOptions.Changed: Boolean;
begin
  result := fChanged or fFrame.fChanged;
end;

function TRsatOptions.OptionName: String;
begin
  result := 'General';
end;

procedure TRsatOptions.CreateFrame(TheOwner: TComponent);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'CreateFrame', Self);

  fFrame := TFrmRsatOptions.Create(TheOwner);
  fFrame.Load(Self);
  RegisterObserver(@fFrame.Load);
end;

function TRsatOptions.GetFrame: TFrameOptions;
begin
  result := fFrame;
end;

procedure TRsatOptions.DeleteFrame;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'DeleteFrame', Self);

  RemoveObserver(@fFrame.Load);
  FreeAndNil(fFrame);
end;

procedure TRsatOptions.BeginUpdate;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'BeginUpdate', Self);

  Inc(fUpdating);
end;

procedure TRsatOptions.EndUpdate;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'EndUpdate', Self);

  Dec(fUpdating);
end;

procedure TRsatOptions.AddOptions(Options: TOptions);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'AddOptions', Self);

  Insert(Options, fModulesOptions, High(fModulesOptions));
end;

procedure TRsatOptions.RegisterObserver(Observer: TProcRsatOptionsOfObject);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'RegisterObserver', Self);

  MultiEventAdd(fObservers, TMethod(Observer));
end;

procedure TRsatOptions.RemoveObserver(Observer: TProcRsatOptionsOfObject);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'RemoveObserver', Self);

  MultiEventRemove(fObservers, TMethod(Observer));
end;

end.

