unit ufrmpropertyoperatingsystem;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  StdCtrls, ExtCtrls,
  mormot.core.base,
  mormot.core.log,
  uproperty,
  upropertyframe;

type

  { TFrmPropertyOperatingSystem }

  TFrmPropertyOperatingSystem = class(TPropertyFrame)
    Edit_OperatingSystem: TEdit;
    Edit_OperatingSystemServicePack: TEdit;
    Edit_OperatingSystemVersion: TEdit;
    Label_OperatingSystem: TLabel;
    Label_OperatingSystemServicePack: TLabel;
    Label_OperatingSystemVersion: TLabel;
    Panel1: TPanel;
  private
    fLog: TSynLog;
    fProperty: TProperty;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Update(Props: TProperty); override;
  end;

implementation
uses
  uhelpers;

{$R *.lfm}

{ TFrmPropertyOperatingSystem }

constructor TFrmPropertyOperatingSystem.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);

  Caption := 'Operating System';
end;

procedure TFrmPropertyOperatingSystem.Update(Props: TProperty);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Update', Self);

  fProperty := Props;

  Edit_OperatingSystem.CaptionNoChange := fProperty.GetReadable('operatingSystem');
  Edit_OperatingSystemVersion.CaptionNoChange := fProperty.GetReadable('operatingSystemVersion');
  Edit_OperatingSystemServicePack.CaptionNoChange := fProperty.GetReadable('operatingSystemServicePack');
end;

end.

