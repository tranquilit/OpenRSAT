unit ufrmpropertygeneralvolume;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  Buttons,
  SysUtils,
  Forms,
  Controls,
  ExtCtrls,
  Graphics,
  StdCtrls, ActnList,
  mormot.core.base,
  mormot.core.log,
  uproperty,
  upropertyframe;

type

  { TFrmPropertyGeneralVolume }

  TFrmPropertyGeneralVolume = class(TPropertyFrame)
    Action1: TAction;
    ActionList1: TActionList;
    BitBtn_Keywords: TBitBtn;
    Edit_Name: TEdit;
    Edit_Description: TEdit;
    Edit_UNCName: TEdit;
    Image: TImage;
    Label_Description: TLabel;
    Label_UNCName: TLabel;
    Line_Top: TShape;
    Panel_Content: TPanel;
    Panel_Header: TPanel;
    procedure Action1Execute(Sender: TObject);
    procedure Edit_DescriptionChange(Sender: TObject);
    procedure Edit_UNCNameChange(Sender: TObject);
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
  ucommon,
  uhelpersui,
  uvislistother;

{$R *.lfm}

{ TFrmPropertyGeneralVolume }

procedure TFrmPropertyGeneralVolume.Action1Execute(Sender: TObject);
var
  Keywords: TDocVariantData;
  Vis: TVisListOther;
  Items: TRawUtf8DynArray;
  i: Integer;
begin
  Keywords.InitArray([]);

  Vis := TVisListOther.Create(self, 'keywords', @Keywords, 256);
  try
    if Vis.ShowModal() <> mrOK then
      Exit;
  finally
    FreeAndNil(Vis);
  end;

  Items := Keywords.ToRawUtf8DynArray;
  if Length(Items) >= 1 then
    fProperty.Add('keywords', Items[0]);
  for i := 1 to Length(Items) - 1 do
    fProperty.Add('keywords', Items[i], aoAlways);
end;

procedure TFrmPropertyGeneralVolume.Edit_DescriptionChange(Sender: TObject);
begin
  fProperty.Add('description', Edit_Description.Text);
end;

procedure TFrmPropertyGeneralVolume.Edit_UNCNameChange(Sender: TObject);
begin
  if IsServerPath(Edit_UNCName.Text) then
  begin
    Edit_UNCName.Font.Color := clDefault;
    fProperty.Add('uNCName', Edit_UNCName.Text);
  end
  else
  begin
    Edit_UNCName.Font.Color := clRed;
    fProperty.Restore('uNCName');
  end;
end;

constructor TFrmPropertyGeneralVolume.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);

  Caption := 'General';
end;

procedure TFrmPropertyGeneralVolume.Update(Props: TProperty);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Update', Self);

  fProperty := Props;

  Edit_Name.CaptionNoChange := fProperty.name;
  Edit_Description.CaptionNoChange := fProperty.description;
  Edit_UNCName.CaptionNoChange := fProperty.GetReadable('uNCName');
end;

end.

