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
    ComboBox_adr_CountryCode: TComboBox;
    Edit_adr_l: TEdit;
    Edit_adr_PostalCode: TEdit;
    Edit_adr_postOfficeBox: TEdit;
    Edit_adr_st: TEdit;
    Label_adr_CountryCode: TLabel;
    Label_adr_l: TLabel;
    Label_adr_PostalCode: TLabel;
    Label_adr_postOfficeBox: TLabel;
    Label_adr_st: TLabel;
    Label_adr_streetAddress: TLabel;
    Memo_adr_streetAddress: TMemo;
    Panel1: TPanel;
    procedure ComboBox_adr_CountryCodeChange(Sender: TObject);
    procedure Edit_adr_lChange(Sender: TObject);
    procedure Edit_adr_PostalCodeChange(Sender: TObject);
    procedure Edit_adr_postOfficeBoxChange(Sender: TObject);
    procedure Edit_adr_stChange(Sender: TObject);
    procedure Memo_adr_streetAddressChange(Sender: TObject);
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

procedure TFrmPropertyAddress.ComboBox_adr_CountryCodeChange(Sender: TObject);
var
  idx: Integer;
  PCountryCode: PDocVariantData;
  c, co, countryCode: RawUtf8;
begin
  if IsUpdating then
    Exit;
  idx := ComboBox_adr_CountryCode.ItemIndex;
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

procedure TFrmPropertyAddress.Edit_adr_lChange(Sender: TObject);
begin
  Add('l', Edit_adr_l.Text);
end;

procedure TFrmPropertyAddress.Edit_adr_PostalCodeChange(Sender: TObject);
begin
  Add('postalCode', Edit_adr_PostalCode.Text);
end;

procedure TFrmPropertyAddress.Edit_adr_postOfficeBoxChange(Sender: TObject);
begin
  Add('postOfficeBox', Edit_adr_postOfficeBox.Text);
end;

procedure TFrmPropertyAddress.Edit_adr_stChange(Sender: TObject);
begin
  Add('st', Edit_adr_st.Text);
end;

procedure TFrmPropertyAddress.Memo_adr_streetAddressChange(Sender: TObject);
begin
  Add('streetAddress', Memo_adr_streetAddress.Text);
end;

procedure TFrmPropertyAddress.Add(AttributeName, AttributeValue: RawUtf8;
  Option: TLdapAddOption);
begin
  if Assigned(fProperty) then
    fProperty.Attributes.Add(AttributeName, AttributeValue, Option);
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

  ComboBox_adr_CountryCode.Clear();
  ComboBox_adr_CountryCode.Items.BeginUpdate;
  try
    for CountryCode in fCountryCodes.Objects do
      ComboBox_adr_CountryCode.Items.Add(CountryCode^.S['name']);
  finally
    ComboBox_adr_CountryCode.Items.EndUpdate;
  end;
end;

constructor TFrmPropertyAddress.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);

  LoadCountryCodes;
  Caption := '_Address';
  IsUpdating := False;
end;

procedure TFrmPropertyAddress.Update(Props: TProperty);
begin
  fProperty := Props;

  Memo_adr_streetAddress.Text := Props.Attributes.GetByName('streetAddress');
  Edit_adr_postOfficeBox.Text := Props.Attributes.GetByName('postOfficeBox');
  Edit_adr_l.Text := Props.Attributes.GetByName('l');
  Edit_adr_st.Text := Props.Attributes.GetByName('st');
  Edit_adr_PostalCode.Text := Props.Attributes.GetByName('postalCode');

  IsUpdating := True;
  try
    ComboBox_adr_CountryCode.ItemIndex := ComboBox_adr_CountryCode.Items.IndexOf(Props.Attributes.GetByName('co'));
  finally
    IsUpdating := False;
  end;
end;

end.

