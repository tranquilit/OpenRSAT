unit ufrmpropertygeneraldefault;

{$mode objfpc}{$H+}

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
  uproperty,
  upropertyframe;

type

  { TFrmPropertyGeneralDefault }

  TFrmPropertyGeneralDefault = class(TPropertyFrame)
    Edit_Name: TEdit;
    Edit_Description: TEdit;
    Image: TImage;
    Label_Description: TLabel;
    Line: TShape;
    Panel_Content: TPanel;
    Panel_Header: TPanel;
    procedure Edit_DescriptionChange(Sender: TObject);
  private
    fLog: TSynLog;
    fProperty: TProperty;
  public
    constructor Create(TheOwner: TComponent); override;

    procedure Update(Props: TProperty); override;
  end;

implementation

{$R *.lfm}

{ TFrmPropertyGeneralDefault }

procedure TFrmPropertyGeneralDefault.Edit_DescriptionChange(Sender: TObject);
begin
  fProperty.Add('description', Edit_Description.Text);
end;

constructor TFrmPropertyGeneralDefault.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);

  Caption := 'General';
end;

procedure TFrmPropertyGeneralDefault.Update(Props: TProperty);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Update', Self);

  fProperty := Props;

  Edit_Name.Text := Props.name;
  Edit_Description.Text := Props.description;
end;

end.

