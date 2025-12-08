unit ufrmpropertygeneraluser;

{$mode objfpc}{$H+}

interface

uses
  buttons,
  Classes,
  SysUtils,
  Forms,
  Controls,
  ExtCtrls,
  StdCtrls, ActnList,
  mormot.core.base,
  mormot.core.log,
  uproperty,
  upropertyframe;

type

  { TFrmPropertyGeneralUser }

  TFrmPropertyGeneralUser = class(TPropertyFrame)
    Action_HomePage: TAction;
    Action_OtherPhoneNumber: TAction;
    ActionList1: TActionList;
    Btn_OtherTelephone: TBitBtn;
    Btn_HomePage: TBitBtn;
    Edit_Name: TEdit;
    Edit_Description: TEdit;
    Edit_DisplayName: TEdit;
    Edit_GivenName: TEdit;
    Edit_Initials: TEdit;
    Edit_Mail: TEdit;
    Edit_PhysicalDeliveryOfficeName: TEdit;
    Edit_Surname: TEdit;
    Edit_TelephoneNumber: TEdit;
    Edit_HomePage: TEdit;
    Image: TImage;
    Label_Description: TLabel;
    Label_DesktopProfile: TLabel;
    Label_DisplayName: TLabel;
    Label_GivenName: TLabel;
    Label_Initials: TLabel;
    Label_Mail: TLabel;
    Label_Surname: TLabel;
    Label_TelephoneNumber: TLabel;
    Label_HomePage: TLabel;
    Line_Bottom: TShape;
    Line_Top: TShape;
    Panel_Bottom: TPanel;
    Panel_Header: TPanel;
    Panel_Top: TPanel;
    procedure Action_OtherPhoneNumberExecute(Sender: TObject);
    procedure Edit_DescriptionChange(Sender: TObject);
    procedure Edit_DisplayNameChange(Sender: TObject);
    procedure Edit_GivenNameChange(Sender: TObject);
    procedure Edit_HomePageChange(Sender: TObject);
    procedure Edit_InitialsChange(Sender: TObject);
    procedure Edit_MailChange(Sender: TObject);
    procedure Edit_PhysicalDeliveryOfficeNameChange(Sender: TObject);
    procedure Edit_SurnameChange(Sender: TObject);
    procedure Edit_TelephoneNumberChange(Sender: TObject);
  private
    fLog: TSynLog;
    fProperty: TProperty;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Update(Props: TProperty); override;
  end;

implementation
uses
  mormot.core.variants,
  mormot.net.ldap,
  uhelpers,
  uvislistother;

{$R *.lfm}

{ TFrmPropertyGeneralUser }

procedure TFrmPropertyGeneralUser.Edit_HomePageChange(Sender: TObject);
begin
  fProperty.Add('wWWHomePage', Edit_HomePage.Text);
end;

procedure TFrmPropertyGeneralUser.Edit_InitialsChange(Sender: TObject);
begin
  fProperty.Add('initials', Edit_Initials.Text);
end;

procedure TFrmPropertyGeneralUser.Edit_DescriptionChange(Sender: TObject);
begin
  fProperty.Add('description', Edit_Description.Text)
end;

procedure TFrmPropertyGeneralUser.Action_OtherPhoneNumberExecute(Sender: TObject);
var
  VisTelOther: TVisListOther;
  TelephoneNumbers: TDocVariantData;
  Items: TRawUtf8DynArray;
  i: Integer;
begin
  TelephoneNumbers.InitArrayFrom(fProperty.getAllReadable('otherTelephone'), []);

  VisTelOther := TVisListOther.Create(self, 'Telephone Numbers', @TelephoneNumbers, 64);
  try
    if VisTelOther.ShowModal() <> mrOK then
      Exit;
  finally
    FreeAndNil(VisTelOther);
  end;

  Items := TelephoneNumbers.ToRawUtf8DynArray;
  if Length(Items) >= 1 then
    fProperty.Add('otherTelephone', Items[0]);
  for i := 1 to Length(Items) - 1 do
    fProperty.Add('otherTelephone', Items[i], aoAlways);
end;

procedure TFrmPropertyGeneralUser.Edit_DisplayNameChange(Sender: TObject);
begin
  fProperty.Add('displayName', Edit_DisplayName.Text);
end;

procedure TFrmPropertyGeneralUser.Edit_GivenNameChange(Sender: TObject);
begin
  fProperty.Add('givenName', Edit_GivenName.Text);
end;

procedure TFrmPropertyGeneralUser.Edit_MailChange(Sender: TObject);
begin
  fProperty.Add('mail', Edit_Mail.Text);
end;

procedure TFrmPropertyGeneralUser.Edit_PhysicalDeliveryOfficeNameChange(
  Sender: TObject);
begin
  fProperty.Add('physicalDeliveryOfficeName', Edit_PhysicalDeliveryOfficeName.Text);
end;

procedure TFrmPropertyGeneralUser.Edit_SurnameChange(Sender: TObject);
begin
  fProperty.Add('sn', Edit_Surname.Text)
end;

procedure TFrmPropertyGeneralUser.Edit_TelephoneNumberChange(Sender: TObject);
begin
  fProperty.Add('telephoneNumber', Edit_TelephoneNumber.Text);
end;

constructor TFrmPropertyGeneralUser.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);

  Caption := 'General';
end;

procedure TFrmPropertyGeneralUser.Update(Props: TProperty);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Update', Self);

  fProperty := Props;

  Edit_Name.CaptionNoChange := Props.name;
  Edit_GivenName.CaptionNoChange := Props.GetReadable('givenName');
  Edit_Initials.CaptionNoChange := Props.GetReadable('initials');
  Edit_Surname.CaptionNoChange := Props.GetReadable('sn');
  Edit_DisplayName.CaptionNoChange := Props.GetReadable('displayName');
  Edit_Description.CaptionNoChange := Props.description;
  Edit_PhysicalDeliveryOfficeName.CaptionNoChange := Props.GetReadable('physicalDeliveryOfficeName');
  Edit_TelephoneNumber.CaptionNoChange := Props.GetReadable('telephoneNumber');
  Edit_Mail.CaptionNoChange := Props.GetReadable('mail');
  Edit_HomePage.CaptionNoChange := Props.GetReadable('wWWHomePage');
end;

end.

