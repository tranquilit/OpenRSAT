unit ufrmrsatoptions;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  StdCtrls,
  IniFiles,
  mormot.core.log,
  ufrmoption,
  ursatoption,
  uoption;

type

  { TFrmRSATOption }

  TFrmRSATOption = class(TFrameOption)
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure ComboBox2Change(Sender: TObject);
    procedure ComboBox3Change(Sender: TObject);
  private
    fLog: TSynLog;
    fChanged: Boolean;

    fRSATOption: TRSATOption;
  public
    // Inherited TFrame
    constructor Create(TheOwner: TComponent; Option: TOption); override;
    destructor Destroy; override;

    property RSATOption: TRSATOption read fRSATOption;
  published
    // Inherited TFrameOptions
    function OptionChanged: Boolean; override;
    procedure Load; override;
    procedure Save; override;
  end;

implementation

uses
  Dialogs,
  mormot.core.text,
  mormot.core.base,
  ucommon,
  ufrmrsat;

{$R *.lfm}

{ TFrmRSATOption }

procedure TFrmRSATOption.ComboBox3Change(Sender: TObject);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Change AdvancedView (%)', [ComboBox3.Text], Self);

  fChanged := True;
end;

procedure TFrmRSATOption.ComboBox2Change(Sender: TObject);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Change Lang (%)', [ComboBox2.Text], Self);

  fChanged := True;
end;

constructor TFrmRSATOption.Create(TheOwner: TComponent; Option: TOption);
begin
  inherited Create(TheOwner);

  fChanged := False;
  fRSATOption := TRSATOption(Option);

  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllInfo, 'Create', Self);
end;

function TFrmRSATOption.OptionChanged: Boolean;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'OptionChanged (%)', [fChanged], Self);

  result := fChanged;
end;

procedure TFrmRSATOption.Load;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Load', Self);

  if not Assigned(Self) then
  begin
    if Assigned(fLog) then
      fLog.Log(sllWarning, 'Could not be loaded: Self or Options not assigned.', Self);
    Exit;
  end;

  ComboBox1.ItemIndex := Ord(RSATOption.Theme);
  ComboBox2.Text := RSATOption.Lang;
  ComboBox3.ItemIndex := Ord(RSATOption.AdvancedView);

  fChanged := False;
end;

procedure TFrmRSATOption.Save;
var
  ShouldRestart: Boolean;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Save', Self);

  if not Assigned(Self) then
  begin
    if Assigned(fLog) then
      fLog.Log(sllWarning, 'Could not be saved: Self or Options not assigned.', Self);
    Exit;
  end;

  ShouldRestart := (ComboBox2.Text <> RSATOption.Lang);

  RSATOption.BeginUpdate;
  try
    RSATOption.Theme := TThemeMode(ComboBox1.ItemIndex);
    RSATOption.Lang := ComboBox2.Text;
    RSATOption.AdvancedView := Boolean(ComboBox3.ItemIndex);
  finally
    RSATOption.Save;
  end;

  if ShouldRestart and (MessageDlg(rsRestartTitle, rsChangeOptionRequiredRestart, mtConfirmation, mbYesNoCancel, 0) = mrYes) then
    FrmRSAT.Restart;

  fChanged := False;
end;

destructor TFrmRSATOption.Destroy;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Destroy', Self);

  inherited Destroy;
end;

end.

