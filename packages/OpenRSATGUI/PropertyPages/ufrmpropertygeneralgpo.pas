unit ufrmpropertygeneralgpo;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  ExtCtrls,
  StdCtrls,
  LCLIntf,
  mormot.core.base,
  mormot.core.log,
  uproperty,
  upropertyframe,
  ulog;

type

  { TFrmPropertyGeneralGPO }

  TFrmPropertyGeneralGPO = class(TPropertyFrame)
    ComboBox1: TComboBox;
    Edit1: TEdit;
    Edit_Description: TEdit;
    Edit_Name: TEdit;
    Image: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label_Description: TLabel;
    Line: TShape;
    Panel_Content: TPanel;
    Panel_Header: TPanel;

    procedure ComboBox1Change(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit_DescriptionChange(Sender: TObject);
    procedure Label4Click(Sender: TObject);
    procedure Label4MouseEnter(Sender: TObject);
    procedure Label4MouseLeave(Sender: TObject);
  private
    fLog: TSynLogClass;
    fProperty: TProperty;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Update(Props: TProperty); override;
  end;

implementation

{$R *.lfm}

{ TFrmPropertyGeneralGPO }

procedure TFrmPropertyGeneralGPO.ComboBox1Change(Sender: TObject);
begin
  fProperty.Add('flags', IntToStr(ComboBox1.ItemIndex));
end;

procedure TFrmPropertyGeneralGPO.Edit1Change(Sender: TObject);
begin
  fProperty.Add('displayName', Edit1.Text);
end;

procedure TFrmPropertyGeneralGPO.Edit_DescriptionChange(Sender: TObject);
begin
  fProperty.Add('description', Edit_Description.Text);
end;

procedure TFrmPropertyGeneralGPO.Label4Click(Sender: TObject);
begin
  OpenDocument(Label4.Caption);
end;

procedure TFrmPropertyGeneralGPO.Label4MouseEnter(Sender: TObject);
begin
  Label4.Cursor := crHandPoint;
end;

procedure TFrmPropertyGeneralGPO.Label4MouseLeave(Sender: TObject);
begin
  Label4.Cursor := crDefault;
end;

constructor TFrmPropertyGeneralGPO.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fLog := TOpenRSATLog;
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, 'Create', Self);

  Caption := 'General';
end;

procedure TFrmPropertyGeneralGPO.Update(Props: TProperty);
var
  flags: Longint;
begin
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, 'Update', Self);

  fProperty := Props;

  Edit_Name.Text := Props.name;
  Edit_Description.Text := Props.description;
  Edit1.Text := Props.GetReadable('displayName');
  Label4.Caption := Props.GetReadable('gPCFileSysPath');
  if TryStrToInt(Props.GetReadable('flags'), flags) then
    ComboBox1.ItemIndex := flags;
end;

end.

