unit ufrmpropertylocation;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls, ExtCtrls, StdCtrls,
  mormot.core.base,
  mormot.core.log,
  uproperty,
  upropertyframe;

type

  { TFrmPropertyLocation }

  TFrmPropertyLocation = class(TPropertyFrame)
    Edit_Location: TEdit;
    Image: TImage;
    Label_Location: TLabel;
    Line: TShape;
    Panel_Content: TPanel;
    Panel_Header: TPanel;
    procedure Edit_LocationChange(Sender: TObject);
  private
    fLog: TSynLog;
    fProperty: TProperty;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Update(Props: TProperty); override;
  end;

implementation
uses
  ucommon,
  uhelpersui;

{$R *.lfm}

{ TFrmPropertyLocation }

procedure TFrmPropertyLocation.Edit_LocationChange(Sender: TObject);
begin
  fProperty.Add('location', Edit_Location.Caption);
end;

constructor TFrmPropertyLocation.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);

  Caption := 'Location';
end;

procedure TFrmPropertyLocation.Update(Props: TProperty);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Update', Self);

  fProperty := Props;

  Edit_Location.CaptionNoChange := fProperty.GetReadable('location');
end;

end.

