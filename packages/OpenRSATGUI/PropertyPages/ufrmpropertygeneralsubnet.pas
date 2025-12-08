unit ufrmpropertygeneralsubnet;

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
  mormot.core.variants,
  uhelpers,
  uproperty,
  upropertyframe;

type

  { TFrmPropertyGeneralSubnet }

  TFrmPropertyGeneralSubnet = class(TPropertyFrame)
    ComboBox_Site: TComboBox;
    Edit_Name: TEdit;
    Edit_Description: TEdit;
    Edit_Prefix: TEdit;
    Image: TImage;
    Label_Description: TLabel;
    Label_Prefix: TLabel;
    Label_Site: TLabel;
    Line_Top: TShape;
    Panel_Content: TPanel;
    Panel_Header: TPanel;
    procedure ComboBox_SiteChange(Sender: TObject);
    procedure Edit_DescriptionChange(Sender: TObject);
  private
    fLog: TSynLog;
    fProperty: TProperty;
    fSubnets: TDocVariantData;

    procedure UpdateSubnetList;
    function SiteName: RawUtf8;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Update(Props: TProperty); override;
  end;

implementation

{$R *.lfm}

{ TFrmPropertyGeneralSubnet }

procedure TFrmPropertyGeneralSubnet.ComboBox_SiteChange(Sender: TObject);
begin

end;

procedure TFrmPropertyGeneralSubnet.Edit_DescriptionChange(Sender: TObject);
begin
  fProperty.Add('description', Edit_Description.Text);
end;

procedure TFrmPropertyGeneralSubnet.UpdateSubnetList;
begin
  fSubnets.Clear;
  fProperty.Subnets(@fSubnets);
  ComboBox_Site.Items.BeginUpdate;
  try
    ComboBox_Site.Items.Clear;
    ComboBox_Site.Items.Add('');
    ComboBox_Site.Items.AddStrings(TStringDynArray(fSubnets.O_['name']^.GetNames));
  finally
    ComboBox_Site.Items.EndUpdate;
  end;
end;

function TFrmPropertyGeneralSubnet.SiteName: RawUtf8;
var
  siteObject: RawUtf8;
begin
  result := '';
  siteObject := fProperty.GetReadable('siteObject');
  if siteObject <> '' then
    result := fSubnets.O_['dn']^.S[siteObject];
end;

constructor TFrmPropertyGeneralSubnet.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fSubnets.Init();

  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);

  Caption := 'General';
end;

procedure TFrmPropertyGeneralSubnet.Update(Props: TProperty);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Update', Self);

  fProperty := Props;

  UpdateSubnetList;

  Edit_Name.CaptionNoChange := ''; // Do not display name for subnet. Name is used on prefix entry.
  Edit_Description.CaptionNoChange := fProperty.description;
  ComboBox_Site.CaptionNoChange := SiteName;
  Edit_Prefix.CaptionNoChange := fProperty.GetReadable('name');
end;

end.

