unit ufrmpropertygeneralsite;

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
  uproperty, tis.ui.grid.core,
  upropertyframe;

type

  { TFrmPropertyGeneralSite }

  TFrmPropertyGeneralSite = class(TPropertyFrame)
    Edit_Name: TEdit;
    Edit_Description: TEdit;
    Image: TImage;
    Label_Description: TLabel;
    Label_Subnet: TLabel;
    Line_top: TShape;
    ListBox_Subnet: TListBox;
    Panel_Content: TPanel;
    Panel_Header: TPanel;
  private
    fLog: TSynLog;
    fProperty: TProperty;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Update(Props: TProperty); override;
  end;

implementation

{$R *.lfm}

{ TFrmPropertyGeneralSite }

constructor TFrmPropertyGeneralSite.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);

  Caption := 'General';
end;

procedure TFrmPropertyGeneralSite.Update(Props: TProperty);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Update', Self);

  fProperty := Props;

  Edit_Name.CaptionNoChange := fProperty.name;
  Edit_Description.CaptionNoChange := fProperty.description;
  ListBox_Subnet.Items.AddStrings(TStringDynArray(fProperty.SubnetsFromSiteObject));
end;

end.

