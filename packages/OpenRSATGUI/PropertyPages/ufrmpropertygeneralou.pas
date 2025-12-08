unit ufrmpropertygeneralou;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  ExtCtrls,
  StdCtrls,
  LCLType,
  mormot.core.base,
  mormot.core.log,
  mormot.core.variants,
  uhelpersui,
  uproperty,
  upropertyframe;

type

  { TFrmPropertyGeneralOU }

  TFrmPropertyGeneralOU = class(TPropertyFrame)
    ComboBox_co: TComboBox;
    Edit_Description: TEdit;
    Edit_L: TEdit;
    Edit_Name: TEdit;
    Edit_PostalCode: TEdit;
    Edit_ST: TEdit;
    Image: TImage;
    Label_City: TLabel;
    Label_Country: TLabel;
    Label_Description: TLabel;
    Label_State: TLabel;
    Label_Street: TLabel;
    Label_ZIP: TLabel;
    Line_Top: TShape;
    Memo_Street: TMemo;
    Panel_Content: TPanel;
    Panel_Header: TPanel;
    procedure ComboBox_coChange(Sender: TObject);
    procedure Edit_DescriptionChange(Sender: TObject);
    procedure Edit_LChange(Sender: TObject);
    procedure Edit_PostalCodeChange(Sender: TObject);
    procedure Edit_STChange(Sender: TObject);
    procedure Memo_StreetChange(Sender: TObject);
  private
    fLog: TSynLog;
    fProperty: TProperty;

    fCountryCodes: TDocVariantData;

    procedure LoadCountryCodes;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Update(Props: TProperty); override;
  end;

implementation

{$R *.lfm}

{ TFrmPropertyGeneralOU }

procedure TFrmPropertyGeneralOU.Edit_DescriptionChange(Sender: TObject);
begin
  fProperty.Add('description', Edit_Description.Text);
end;

procedure TFrmPropertyGeneralOU.ComboBox_coChange(Sender: TObject);
var
  idx: Integer;
  PCountryCode: PDocVariantData;
  c, co, countryCode: RawUtf8;
begin
  idx := ComboBox_co.ItemIndex;
  PCountryCode := fCountryCodes._[idx];

  if not Assigned(PCountryCode) then
  begin
    if Assigned(fLog) then
      fLog.Log(sllWarning, 'Cannot retrieve CountryCode', Self);
    Exit;
  end;
  c := PCountryCode^.U['alpha2'];
  co := PCountryCode^.U['name'];
  countryCode := PCountryCode^.U['id'];

  fProperty.Add('c', c);
  fProperty.Add('co', co);
  fProperty.Add('countryCode', countryCode);
end;

procedure TFrmPropertyGeneralOU.Edit_LChange(Sender: TObject);
begin
  fProperty.Add('l', Edit_L.Text);
end;

procedure TFrmPropertyGeneralOU.Edit_PostalCodeChange(Sender: TObject);
begin
  fProperty.Add('postalCode', Edit_PostalCode.Text);
end;

procedure TFrmPropertyGeneralOU.Edit_STChange(Sender: TObject);
begin
  fProperty.Add('st', Edit_ST.Text);
end;

procedure TFrmPropertyGeneralOU.Memo_StreetChange(Sender: TObject);
begin
  fProperty.Add('street', Memo_Street.Text);
end;

procedure TFrmPropertyGeneralOU.LoadCountryCodes;
var
  Resource: TResourceStream;
  s: RawUtf8;
  CountryCode: PDocVariantData;
begin
  Resource := TResourceStream.Create(HInstance, 'COUNTRY_CODES_EN', RT_RCDATA);
  try
    SetLength(s, Resource.Size);
    Resource.Read(s[1], Resource.Size);
    fCountryCodes.InitJson(s);
  finally
    FreeAndNil(Resource);
  end;

  ComboBox_co.Clear();
  ComboBox_co.Items.BeginUpdate;
  try
    for CountryCode in fCountryCodes.Objects do
      ComboBox_co.Items.Add(CountryCode^.S['name']);
  finally
    ComboBox_co.Items.EndUpdate;
  end;
end;

constructor TFrmPropertyGeneralOU.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);

  LoadCountryCodes;
  Caption := 'General';
end;

procedure TFrmPropertyGeneralOU.Update(Props: TProperty);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Update', Self);

  fProperty := Props;

  Edit_Description.CaptionNoChange := fProperty.description;
  Memo_Street.CaptionNoChange := fProperty.GetReadable('street');
  Edit_L.CaptionNoChange := fProperty.GetReadable('l');
  Edit_ST.CaptionNoChange := fProperty.GetReadable('st');
  Edit_PostalCode.CaptionNoChange := fproperty.GetReadable('postalCode');
  ComboBox_co.CaptionNoChange := fProperty.GetReadable('co');
end;

end.

