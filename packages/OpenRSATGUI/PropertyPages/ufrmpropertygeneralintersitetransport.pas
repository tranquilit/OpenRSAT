unit ufrmpropertygeneralintersitetransport;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  ExtCtrls,
  StdCtrls,
  mormot.core.base,
  mormot.core.log,
  uhelpersui,
  uproperty,
  upropertyframe;

type

  { TFrmPropertyGeneralInterSiteTransport }

  TFrmPropertyGeneralInterSiteTransport = class(TPropertyFrame)
    CheckBox_Bridge: TCheckBox;
    CheckBox_Schedules: TCheckBox;
    Edit_Description: TEdit;
    Edit_Name: TEdit;
    Image_Logo: TImage;
    Label_Description: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Shape1: TShape;
  private
    fLog: TSynLog;
    fProperty: TProperty;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Update(Props: TProperty); override;
  end;

implementation

{$R *.lfm}

constructor TFrmPropertyGeneralInterSiteTransport.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);

  Caption := 'General';
end;

procedure TFrmPropertyGeneralInterSiteTransport.Update(Props: TProperty);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Update', Self);

  fProperty := Props;

  Edit_Name.Text := fProperty.name;
  Edit_Description.Text := fProperty.description;
end;

end.

