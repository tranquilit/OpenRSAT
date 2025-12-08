unit ufrmpropertytelephone;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Buttons,
  Forms,
  Controls,
  StdCtrls,
  mormot.core.base,
  mormot.core.log,
  uproperty,
  upropertyframe;

type

  { TFrmPropertyTelephone }

  TFrmPropertyTelephone = class(TPropertyFrame)
    BitBtn_OtherFacsimileTelephoneNumber: TBitBtn;
    BitBtn_OtherHomePhone: TBitBtn;
    BitBtn_OtherIpPhone: TBitBtn;
    BitBtn_OtherMobile: TBitBtn;
    BitBtn_OtherPager: TBitBtn;
    Edit_FacsimileTelephoneNumber: TEdit;
    Edit_HomePhone: TEdit;
    Edit_IpPhone: TEdit;
    Edit_Mobile: TEdit;
    Edit_Pager: TEdit;
    GroupBox_Numbers: TGroupBox;
    Label_FacsimileTelephoneNumber: TLabel;
    Label_HomePhone: TLabel;
    Label_Info: TLabel;
    Label_IpPhone: TLabel;
    Label_Mobile: TLabel;
    Label_Pager: TLabel;
    Memo_Info: TMemo;
    procedure BitBtn_OtherFacsimileTelephoneNumberClick(Sender: TObject);
    procedure BitBtn_OtherHomePhoneClick(Sender: TObject);
    procedure BitBtn_OtherIpPhoneClick(Sender: TObject);
    procedure BitBtn_OtherMobileClick(Sender: TObject);
    procedure BitBtn_OtherPagerClick(Sender: TObject);
    procedure Edit_FacsimileTelephoneNumberChange(Sender: TObject);
    procedure Edit_HomePhoneChange(Sender: TObject);
    procedure Edit_IpPhoneChange(Sender: TObject);
    procedure Edit_MobileChange(Sender: TObject);
    procedure Edit_PagerChange(Sender: TObject);
    procedure Memo_InfoChange(Sender: TObject);
  private
    fLog: TSynLog;
    fProperty: TProperty;

    procedure ChangeOther(OtherName, OtherAttributeName: RawUtf8);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Update(Props: TProperty); override;
  end;

implementation
uses
  mormot.core.variants,
  mormot.net.ldap,
  ucommon,
  uhelpers,
  uvislistother;

{$R *.lfm}

{ TFrmPropertyTelephone }

procedure TFrmPropertyTelephone.Edit_HomePhoneChange(Sender: TObject);
begin
  fProperty.Add('homePhone', Edit_HomePhone.Caption);
end;

procedure TFrmPropertyTelephone.Edit_IpPhoneChange(Sender: TObject);
begin
  fProperty.Add('ipPhone', Edit_IpPhone.Caption);
end;

procedure TFrmPropertyTelephone.Edit_FacsimileTelephoneNumberChange(
  Sender: TObject);
begin
  fProperty.Add('facsimileTelephoneNumber', Edit_FacsimileTelephoneNumber.Caption);
end;

procedure TFrmPropertyTelephone.BitBtn_OtherHomePhoneClick(Sender: TObject);
begin
  ChangeOther('Home Phone', 'otherHomePhone');
end;

procedure TFrmPropertyTelephone.BitBtn_OtherIpPhoneClick(Sender: TObject);
begin
  ChangeOther('Ip Phone', 'ipPhone');
end;

procedure TFrmPropertyTelephone.BitBtn_OtherFacsimileTelephoneNumberClick(
  Sender: TObject);
begin
  ChangeOther('Facsimile Telephone Number', 'facsimileTelephoneNumber');
end;

procedure TFrmPropertyTelephone.BitBtn_OtherMobileClick(Sender: TObject);
begin
  ChangeOther('Mobile', 'otherMobile');
end;

procedure TFrmPropertyTelephone.BitBtn_OtherPagerClick(Sender: TObject);
begin
  ChangeOther('Pager', 'otherPager');
end;

procedure TFrmPropertyTelephone.Edit_MobileChange(Sender: TObject);
begin
  fProperty.Add('mobile', Edit_Mobile.Caption);
end;

procedure TFrmPropertyTelephone.Edit_PagerChange(Sender: TObject);
begin
  fProperty.Add('pager', Edit_Pager.Caption);
end;

procedure TFrmPropertyTelephone.Memo_InfoChange(Sender: TObject);
begin
  fProperty.Add('info', Memo_Info.Caption);
end;

procedure TFrmPropertyTelephone.ChangeOther(OtherName, OtherAttributeName: RawUtf8);
var
  OtherData: TDocVariantData;
  VisTelOther: TVisListOther;
  OtherArray: TRawUtf8DynArray;
  Count: SizeInt;
  i: Integer;
begin
  OtherData.InitArrayFrom(fProperty.GetAllReadable(OtherAttributeName), []);

  VisTelOther := TVisListOther.Create(self, OtherName, @OtherData, 64);
  try
    if VisTelOther.ShowModal() <> mrOK then
      Exit;
    OtherArray := OtherData.ToRawUtf8DynArray;
    Count := Length(OtherArray);
    if not Assigned(OtherArray) or (Count <= 0) then
    begin
      fProperty.Add(OtherAttributeName, '');
      Exit;
    end;
    fProperty.Add(OtherAttributeName, OtherArray[0]);
    for i := 1 to Count - 1 do
      fProperty.Add(OtherAttributeName, OtherArray[i]);
  finally
    FreeAndNil(VisTelOther);
  end;
end;

constructor TFrmPropertyTelephone.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);

  Caption := 'Telephone';
end;

procedure TFrmPropertyTelephone.Update(Props: TProperty);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Update', Self);

  fProperty := Props;

  Edit_HomePhone.CaptionNoChange := fProperty.GetReadable('homePhone');
  Edit_Pager.CaptionNoChange := fProperty.GetReadable('pager');
  Edit_Mobile.CaptionNoChange := fProperty.GetReadable('mobile');
  Edit_FacsimileTelephoneNumber.CaptionNoChange := fProperty.GetReadable('facsimileTelephoneNumber');
  Edit_IpPhone.CaptionNoChange := fProperty.GetReadable('ipPhone');
  Memo_Info.CaptionNoChange := fProperty.GetReadable('info');
end;

end.

