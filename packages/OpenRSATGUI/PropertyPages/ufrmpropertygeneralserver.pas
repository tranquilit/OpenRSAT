unit ufrmpropertygeneralserver;

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

  { TFrmPropertyGeneralServer }

  TFrmPropertyGeneralServer = class(TPropertyFrame)
    Button_Add: TButton;
    Button_Remove: TButton;
    Edit_Description: TEdit;
    Edit_DCType: TEdit;
    Edit_Domain: TEdit;
    Edit_Computer: TEdit;
    Edit_Name: TEdit;
    Image_Logo: TImage;
    Label_TransportsAvailable: TLabel;
    Label_Transports: TLabel;
    Label_Description: TLabel;
    Label_DCType: TLabel;
    Label_Domain: TLabel;
    Label_Computer: TLabel;
    ListBox_TransportsAvailable: TListBox;
    ListBox_Transports: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Shape1: TShape;
  private
    fLog: TSynLog;
    fProperty: TProperty;
    function GetDomain(Value: RawUtf8): RawUtf8;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Update(Props: TProperty); override;
  end;

implementation

uses
  mormot.net.ldap;

{$R *.lfm}

constructor TFrmPropertyGeneralServer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);

  Caption := 'General';
end;

procedure TFrmPropertyGeneralServer.Update(Props: TProperty);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Update', Self);

  fProperty := Props;

  Edit_Name.Text := fProperty.name;
  Edit_Computer.Text := fProperty.CN;
  Edit_Description.Text := fProperty.description;
  Edit_Domain.Text := DNToCN(fProperty.LdapClient.DefaultDN);
end;

function TFrmPropertyGeneralServer.GetDomain(Value: RawUtf8): RawUtf8;
var
  p: Integer;
begin
  p := Pos('.', Value);
  if p = 0 then
    exit('');

  Result := Copy(Value, p + 1, Length(Value) - p);
end;

end.

