unit ufrmpropertyaddress;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  StdCtrls,
  ExtCtrls,
  ActnList,
  mormot.core.base,
  mormot.core.log,
  mormot.core.variants,
  mormot.net.ldap,
  uproperty,
  upropertyframe;

type

  { TFrmPropertyAddress }

  TFrmPropertyAddress = class(TPropertyFrame)
    Action1: TAction;
    ComboBox_CountryCode: TComboBox;
    Edit_City: TEdit;
    Edit_PostalCode: TEdit;
    Edit_PostOfficeBox: TEdit;
    Edit_State: TEdit;
    Label_CountryCode: TLabel;
    Label_City: TLabel;
    Label_PostalCode: TLabel;
    Label_PostOfficeBox: TLabel;
    Label_State: TLabel;
    Label_Street: TLabel;
    Memo_Street: TMemo;
    Panel1: TPanel;
    procedure ComboBox_CountryCodeChange(Sender: TObject);
    procedure Edit_CityChange(Sender: TObject);
    procedure Edit_PostalCodeChange(Sender: TObject);
    procedure Edit_PostOfficeBoxChange(Sender: TObject);
    procedure Edit_StateChange(Sender: TObject);
    procedure Memo_StreetChange(Sender: TObject);
  private
    fProperty: TProperty;
    fCountryCodes: TDocVariantData;
    fLog: TSynLog;
    IsUpdating: Boolean;

    // Expose TLdapAttributeList.Add with default replace value option.
    procedure Add(AttributeName, AttributeValue: RawUtf8; Option: TLdapAddOption = aoReplaceValue);

    // Retrieve countryCodes from resources.
    // Fill fCountryCodes.
    // Fill ComboBox.
    procedure LoadCountryCodes;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Update(Props: TProperty); override;
  end;

implementation

uses
  LCLType;

{$R *.lfm}

{ TFrmPropertyAddress }

procedure TFrmPropertyAddress.ComboBox_CountryCodeChange(Sender: TObject);
var
  idx: Integer;
  PCountryCode: PDocVariantData;
  c, co, countryCode: RawUtf8;
begin
  if IsUpdating then
    Exit;
  idx := ComboBox_CountryCode.ItemIndex;
  PCountryCode := fCountryCodes._[idx];

  if not Assigned(PCountryCode) then
  begin
    if Assigned(fLog) then
      fLog.Log(sllWarning, 'Cannot retrieve CountryCode', Self);
    Exit;
  end;
  c := PCountryCode^.S['alpha2'];
  co := PCountryCode^.S['name'];
  countryCode := PCountryCode^.S['id'];

  Add('c', c);
  Add('co', co);
  Add('countryCode', countryCode);
end;

procedure TFrmPropertyAddress.Edit_CityChange(Sender: TObject);
begin
  Add('l', Edit_City.Text);
end;

procedure TFrmPropertyAddress.Edit_PostalCodeChange(Sender: TObject);
begin
  Add('postalCode', Edit_PostalCode.Text);
end;

procedure TFrmPropertyAddress.Edit_PostOfficeBoxChange(Sender: TObject);
begin
  Add('postOfficeBox', Edit_PostOfficeBox.Text);
end;

procedure TFrmPropertyAddress.Edit_StateChange(Sender: TObject);
begin
  Add('st', Edit_State.Text);
end;

procedure TFrmPropertyAddress.Memo_StreetChange(Sender: TObject);
begin
  Add('streetAddress', Memo_Street.Text);
end;

procedure TFrmPropertyAddress.Add(AttributeName, AttributeValue: RawUtf8;
  Option: TLdapAddOption);
begin
  if not IsUpdating and Assigned(fProperty) then
    fProperty.Add(AttributeName, AttributeValue, Option);
end;

procedure TFrmPropertyAddress.LoadCountryCodes;
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

  ComboBox_CountryCode.Clear();
  ComboBox_CountryCode.Items.BeginUpdate;
  try
    for CountryCode in fCountryCodes.Objects do
      ComboBox_CountryCode.Items.Add(CountryCode^.S['name']);
  finally
    ComboBox_CountryCode.Items.EndUpdate;
  end;
end;

constructor TFrmPropertyAddress.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);

  LoadCountryCodes;
  Caption := 'Address';
  IsUpdating := False;
end;

procedure TFrmPropertyAddress.Update(Props: TProperty);
begin
  fProperty := Props;

  IsUpdating := True;
  Memo_Street.Text := Props.Attributes.GetByName('streetAddress');
  Edit_PostOfficeBox.Text := Props.Attributes.GetByName('postOfficeBox');
  Edit_City.Text := Props.Attributes.GetByName('l');
  Edit_State.Text := Props.Attributes.GetByName('st');
  Edit_PostalCode.Text := Props.Attributes.GetByName('postalCode');

  try
    ComboBox_CountryCode.ItemIndex := ComboBox_CountryCode.Items.IndexOf(Props.Attributes.GetByName('co'));
  finally
    IsUpdating := False;
  end;
end;

end.

