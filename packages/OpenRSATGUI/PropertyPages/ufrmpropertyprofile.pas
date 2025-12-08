unit ufrmpropertyprofile;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  StdCtrls,
  Graphics,
  mormot.core.base,
  mormot.core.log,
  uproperty,
  upropertyframe;

type

  { TFrmPropertyProfile }

  TFrmPropertyProfile = class(TPropertyFrame)
    ComboBox_ConnectTo: TComboBox;
    Edit_ConnectTo: TEdit;
    Edit_HomeDirectory: TEdit;
    Edit_ProfilePath: TEdit;
    Edit_ScriptPath: TEdit;
    GroupBox_HomeFolder: TGroupBox;
    GroupBox_UserProfile: TGroupBox;
    Label_ConnectTo: TLabel;
    Label_ProfilePath: TLabel;
    Label_ScriptPath: TLabel;
    RadioButton_ConnectTo: TRadioButton;
    RadioButton_HomeDirectory: TRadioButton;
    procedure ComboBox_ConnectToChange(Sender: TObject);
    procedure Edit_ConnectToChange(Sender: TObject);
    procedure Edit_HomeDirectoryChange(Sender: TObject);
    procedure Edit_ProfilePathChange(Sender: TObject);
    procedure Edit_ScriptPathChange(Sender: TObject);
    procedure RadioButton_ConnectToChange(Sender: TObject);
    procedure RadioButton_HomeDirectoryChange(Sender: TObject);
  private
    fLog: TSynLog;
    fProperty: TProperty;

    procedure ChangeHomeFolderType;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Update(Props: TProperty); override;
  end;

implementation
uses
  ucommon,
  uhelpersui;

{$R *.lfm}

{ TFrmPropertyProfile }

procedure TFrmPropertyProfile.Edit_ProfilePathChange(Sender: TObject);
begin
  fProperty.Add('profilePath', Edit_ProfilePath.Caption);
end;

procedure TFrmPropertyProfile.Edit_HomeDirectoryChange(Sender: TObject);
begin
  if IsLocalPath(Edit_HomeDirectory.Caption) then
  begin
    Edit_HomeDirectory.Font.Color := clDefault;
    fProperty.Add('homeDirectory', Edit_HomeDirectory.Caption);
  end
  else
  begin
    Edit_HomeDirectory.Font.Color := clRed;
    fProperty.Restore('homeDirectory');
  end;
end;

procedure TFrmPropertyProfile.ComboBox_ConnectToChange(Sender: TObject);
begin
  fProperty.Add('homeDrive', ComboBox_ConnectTo.Caption);
end;

procedure TFrmPropertyProfile.Edit_ConnectToChange(Sender: TObject);
begin
  if IsServerPath(Edit_ConnectTo.Caption) then
  begin
    Edit_ConnectTo.Font.Color := clDefault;
    fProperty.Add('homeDirectory', Edit_ConnectTo.Caption);
  end
  else
  begin
    Edit_ConnectTo.Font.Color := clRed;
    fProperty.Restore('homeDirectory');
  end;
end;

procedure TFrmPropertyProfile.Edit_ScriptPathChange(Sender: TObject);
begin
  fProperty.Add('scriptPath', Edit_ScriptPath.Caption);
end;

procedure TFrmPropertyProfile.RadioButton_ConnectToChange(Sender: TObject);
begin
  ChangeHomeFolderType;
end;

procedure TFrmPropertyProfile.RadioButton_HomeDirectoryChange(Sender: TObject);
begin
  ChangeHomeFolderType;
end;

procedure TFrmPropertyProfile.ChangeHomeFolderType;
var
  LocalPath: Boolean;
begin
  LocalPath := RadioButton_HomeDirectory.Checked;

  Edit_HomeDirectory.Enabled := LocalPath;

  ComboBox_ConnectTo.Enabled := not LocalPath;
  Label_ConnectTo.Enabled := not LocalPath;
  Edit_ConnectTo.Enabled := not LocalPath;

  Edit_HomeDirectory.CaptionNoChange := '';
  ComboBox_ConnectTo.CaptionNoChange := '';
  Edit_ConnectTo.CaptionNoChange := '';

  if LocalPath then
  begin
    Edit_HomeDirectory.CaptionNoChange := fProperty.GetReadable('homeDirectory');
  end
  else
  begin
    Edit_ConnectTo.CaptionNoChange := fProperty.GetReadable('homeDirectory');
    ComboBox_ConnectTo.CaptionNoChange := fProperty.GetReadable('homeDrive');
  end;
end;

constructor TFrmPropertyProfile.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);

  Caption := 'Profile';
end;

procedure TFrmPropertyProfile.Update(Props: TProperty);
var
  HomeDrive: RawUtf8;
  HasHomeDrive: Boolean;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Update', Self);

  fProperty := Props;

  Edit_ProfilePath.CaptionNoChange := fProperty.GetReadable('profilePath');
  Edit_ScriptPath.CaptionNoChange := fProperty.GetReadable('scriptPath');

  HomeDrive := fProperty.GetReadable('homeDrive');
  HasHomeDrive := ComboBox_ConnectTo.Items.IndexOf(HomeDrive) >= 0;

  RadioButton_HomeDirectory.CheckedNoChange := not HasHomeDrive;
  Edit_HomeDirectory.Enabled := not HasHomeDrive;

  RadioButton_ConnectTo.CheckedNoChange := HasHomeDrive;
  ComboBox_ConnectTo.Enabled := HasHomeDrive;
  if HasHomeDrive then
    ComboBox_ConnectTo.CaptionNoChange := HomeDrive;
  Label_ConnectTo.Enabled := HasHomeDrive;
  Edit_ConnectTo.Enabled := HasHomeDrive;
  if HasHomeDrive then
    Edit_ConnectTo.CaptionNoChange := fProperty.GetReadable('homeDirectory')
  else
    Edit_HomeDirectory.CaptionNoChange := fProperty.GetReadable('homeDirectory');
end;

end.

