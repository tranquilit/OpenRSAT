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
  mormot.core.base,
  mormot.core.log,
  umoduleadssoption,
  uoption,
  ufrmoption;

type

  { TFrmModuleADSSOptions }

  TFrmModuleADSSOptions = class(TFrameOption)
    CheckBox1: TCheckBox;
    Label1: TLabel;
    Panel1: TPanel;
    procedure CheckBox1Change(Sender: TObject);
  private
    fLog: TSynLog;
    fChanged: Boolean;

    fOption: TModuleADSSOption;

    function GetShowService: Boolean;
    procedure SetShowService(AValue: Boolean);

  published
    // TFrameOptions
    function OptionChanged: Boolean; override;
    procedure Load; override;
    procedure Save; override;

  public
    constructor Create(TheOwner: TComponent; Option: TOption); override;

    property Option: TModuleADSSOption read fOption;
    property ShowService: Boolean read GetShowService write SetShowService;
  end;

implementation
uses
  uconfig,
  mormot.core.text;

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

procedure TFrmModuleADSSOptions.Load;
var
  IniFile: TIniFile;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Load', Self);

  if not Assigned(Self) or not Assigned(Option) then
  begin
    if Assigned(fLog) then
      fLog.Log(sllWarning, 'Could not be loaded: Self of Options not assigned', Self);
    Exit;
  end;

  IniFile := TIniFile.Create(OptionFilePath);
  try
    Option.Load(IniFile);
  finally
    FreeAndNil(IniFile);
  end;

  ShowService := Option.ShowService;

  fChanged := False;
end;

procedure TFrmModuleADSSOptions.Save;
var
  IniFile: TIniFile;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Save', Self);

  Option.ShowService := ShowService;

  IniFile := TIniFile.Create(OptionFilePath);
  try
    Option.Save(IniFile);
  finally
    FreeAndNil(IniFile);
  end;

  fChanged := False;
end;

constructor TFrmModuleADSSOptions.Create(TheOwner: TComponent; Option: TOption);
begin
  inherited Create(TheOwner);

  fOption := TModuleADSSOption(Option);
end;

end.

