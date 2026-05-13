unit ufrmpropertygeneralntdsdsa;

{$mode ObjFPC}{$H+}

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
  uhelpersui,
  uproperty,
  upropertyframe;

type

  { TFrmPropertyGeneralNTDSDSA }

  TFrmPropertyGeneralNTDSDSA = class(TPropertyFrame)
    CheckBox_GlobalCatalog: TCheckBox;
    ComboBox_QueryPolicy: TComboBox;
    Edit_DNSAlias: TEdit;
    Edit_Description: TEdit;
    Edit_Name: TEdit;
    Image_Logo: TImage;
    Label_Info: TLabel;
    Label_DNSAlias: TLabel;
    Label_QueryPolicy: TLabel;
    Label_Description: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Shape1: TShape;
  private
    fLog: TSynLog;
    fProperty: TProperty;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Update(Props: TProperty); override;
  end;

implementation

{$R *.lfm}

constructor TFrmPropertyGeneralNTDSDSA.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);

  Caption := 'General';
end;

procedure TFrmPropertyGeneralNTDSDSA.Update(Props: TProperty);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Update', Self);

  fProperty := Props;

  Edit_Name.Text := fProperty.name;
  Edit_Description.Text := fProperty.description;
end;

end.

