unit ufrmpropertygeneralntdssitesettings;

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
  
  { TFrmPropertyGeneralNTDSSiteSettings }

  TFrmPropertyGeneralNTDSSiteSettings = class(TPropertyFrame)
    Button_Schedule: TButton;
    CheckBox_Enable: TCheckBox;
    ComboBox_Refresh: TComboBox;
    Edit_Site: TEdit;
    Edit_Server: TEdit;
    Edit_Description: TEdit;
    Edit_Name: TEdit;
    GroupBox_UniversalGroup: TGroupBox;
    GroupBox_InterSiteTopologyGenerator: TGroupBox;
    Image_Logo: TImage;
    Label_Refresh: TLabel;
    Label_Site: TLabel;
    Label_Server: TLabel;
    Label_Description: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Shape1: TShape;
  private
    fLog: TSynLog;
    fProperty: TProperty;
    function GetArgByPosition(Attribute: RawUtf8; n: Integer): RawUtf8;
    function RemoveCN(Value: RawUtf8): RawUtf8;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Update(Props: TProperty); override; 
  end;

implementation

uses
  mormot.net.ldap;

{$R *.lfm}

constructor TFrmPropertyGeneralNTDSSiteSettings.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);

  Caption := 'General';
end;

procedure TFrmPropertyGeneralNTDSSiteSettings.Update(Props: TProperty);
var
  att: TLdapAttribute;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Update', Self);

  fProperty := Props;
  
  Edit_Name.Text := fProperty.name;
  Edit_Description.Text := fProperty.description;
  att := fProperty.Attributes.Find('interSiteTopologyGenerator');
  if Assigned(att) then
  begin
    Edit_Server.Text := RemoveCN(GetArgByPosition(att.GetReadable, 2));
    Edit_Site.Text := RemoveCN(GetArgByPosition(att.GetReadable, 4));
  end;
end; 

function TFrmPropertyGeneralNTDSSiteSettings.GetArgByPosition(Attribute: RawUtf8; n: Integer): RawUtf8;
var
  i, startPos, endPos, len: Integer;
begin
  len := Length(Attribute);
  startPos := 1;
  for i := 1 to N - 1 do
  begin
    startPos := PosEx(',', Attribute, startPos);
    if startPos = 0 then
      exit;
    
    Inc(startPos);
  end;
  
  endPos := PosEx(',', Attribute, startPos);
  if endPos = 0 then
    endPos := len + 1;
  
  Result := Copy(Attribute, startPos, endPos - startPos);
end;

function TFrmPropertyGeneralNTDSSiteSettings.RemoveCN(Value: RawUtf8): RawUtf8;
var
  p: Integer;
begin
  p := Pos('=', Value);
  if p = 0 then
    exit('');
  
  Result := Copy(Value, p + 1, Length(Value) - p);
end;

end.

