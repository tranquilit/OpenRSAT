unit ufrmpropertygeneralcomputer;

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
  uhelpers,
  uproperty,
  upropertyframe;

type

  { TFrmPropertyGeneralComputer }

  TFrmPropertyGeneralComputer = class(TPropertyFrame)
    Edit_Name: TEdit;
    Edit_dcType: TEdit;
    Edit_Description: TEdit;
    Edit_DNSHostName: TEdit;
    Edit_SamaccountName: TEdit;
    Edit_Site: TEdit;
    Image: TImage;
    Label_dcType: TLabel;
    Label_Description: TLabel;
    Label_DNSHostName: TLabel;
    Label_SamaccountName: TLabel;
    Label_Site: TLabel;
    Line_Top: TShape;
    Panel_Header: TPanel;
    Panel_Top: TPanel;
    procedure Edit_DescriptionChange(Sender: TObject);
    procedure Edit_SamaccountNameChange(Sender: TObject);
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
  mormot.core.text,
  mormot.net.ldap;

{$R *.lfm}

{ TFrmPropertyGeneralComputer }

procedure TFrmPropertyGeneralComputer.Edit_SamaccountNameChange(Sender: TObject
  );
begin
  fProperty.Add('sAMAccountName', Edit_SamaccountName.Text);
end;

procedure TFrmPropertyGeneralComputer.Edit_DescriptionChange(Sender: TObject);
begin
  fProperty.Add('description', Edit_Description.Text);
end;

constructor TFrmPropertyGeneralComputer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);

  Caption := 'General';
end;

procedure TFrmPropertyGeneralComputer.Update(Props: TProperty);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Update', Self);

  fProperty := Props;

  Edit_Name.CaptionNoChange := fProperty.name;
  Edit_SamaccountName.CaptionNoChange := fProperty.sAMAccountName;
  Edit_DNSHostName.CaptionNoChange := fProperty.GetReadable('dNSHostName');
  Edit_dcType.CaptionNoChange := fProperty.dcTypeFromUAC;
  Edit_Site.CaptionNoChange := fProperty.SiteFromServerReference;
  Edit_Description.CaptionNoChange := fProperty.description;
end;

end.

