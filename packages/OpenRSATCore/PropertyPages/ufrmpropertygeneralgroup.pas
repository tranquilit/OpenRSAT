unit ufrmpropertygeneralgroup;

{$mode objfpc}{$H+}

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
  uproperty,
  upropertyframe;

type

  { TFrmPropertyGeneralGroup }

  TFrmPropertyGeneralGroup = class(TPropertyFrame)
    Edit_Name: TEdit;
    Edit_Description: TEdit;
    Edit_Mail: TEdit;
    Edit_SamaccountName: TEdit;
    Image: TImage;
    Label_Description: TLabel;
    Label_Info: TLabel;
    Label_Mail: TLabel;
    Label_SamaccountName: TLabel;
    Line_Top: TShape;
    Memo_Info: TMemo;
    Panel_GroupType: TPanel;
    Panel_Header: TPanel;
    Panel_Top: TPanel;
    RB_Distribution: TRadioButton;
    RB_Global: TRadioButton;
    RB_Local: TRadioButton;
    RB_Security: TRadioButton;
    RB_Universal: TRadioButton;
    RadioGroup_Scope: TRadioGroup;
    RadioGroup_Type: TRadioGroup;
    procedure Edit_DescriptionChange(Sender: TObject);
    procedure Edit_MailChange(Sender: TObject);
    procedure Edit_SamaccountNameChange(Sender: TObject);
    procedure Memo_InfoChange(Sender: TObject);
    procedure OnChangeGroupType(Sender: TObject);
  private
    fLog: TSynLog;
    fProperty: TProperty;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Update(Props: TProperty); override;
  end;

implementation

uses
  mormot.net.ldap,
  uhelpers;

{$R *.lfm}

{ TFrmPropertyGeneralGroup }

procedure TFrmPropertyGeneralGroup.OnChangeGroupType(Sender: TObject);
var
  GroupTypes: TGroupTypes;
begin
  GroupTypes := [];

  if RB_Local.Checked then
    Include(GroupTypes, gtDomainLocal);
  if RB_Global.Checked then
    Include(GroupTypes, gtGlobal);
  if RB_Universal.Checked then
    Include(GroupTypes, gtUniversal);

  if RB_Security.Checked then
    Include(GroupTypes, gtSecurity);

  fProperty.Add('groupType', IntToStr(GroupTypesValue(GroupTypes)));
end;

procedure TFrmPropertyGeneralGroup.Edit_SamaccountNameChange(Sender: TObject);
begin
  fProperty.Add('sAMAccountName', Edit_SamaccountName.Text);
end;

procedure TFrmPropertyGeneralGroup.Memo_InfoChange(Sender: TObject);
begin
  fProperty.Add('info', Memo_Info.Text);
end;

procedure TFrmPropertyGeneralGroup.Edit_DescriptionChange(Sender: TObject);
begin
  fProperty.Add('description', Edit_Description.Text);
end;

procedure TFrmPropertyGeneralGroup.Edit_MailChange(Sender: TObject);
begin
  fProperty.Add('mail', Edit_Mail.Text);
end;

constructor TFrmPropertyGeneralGroup.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);

  Caption := 'General';
end;

procedure TFrmPropertyGeneralGroup.Update(Props: TProperty);
var
  GroupType: Longint;
  GroupTypes: TGroupTypes;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Update');

  fProperty := Props;

  Edit_Name.CaptionNoChange := Props.name;
  Edit_SamaccountName.CaptionNoChange := Props.sAMAccountName;
  Edit_Description.CaptionNoChange := Props.description;
  Edit_Mail.CaptionNoChange := Props.GetReadable('mail');
  if not TryStrToInt(Props.GetReadable('groupType'), GroupType) then
    Exit;
  GroupTypes := GroupTypesFromInteger(GroupType);
  RB_Local.CheckedNoChange := (gtDomainLocal in GroupTypes);
  RB_Global.CheckedNoChange := (gtGlobal in GroupTypes);
  RB_Universal.CheckedNoChange := (gtUniversal in GroupTypes);
  RB_Security.CheckedNoChange := (gtSecurity in GroupTypes);
  RB_Distribution.CheckedNoChange := not RB_Security.Checked;
  Memo_Info.CaptionNoChange := Props.GetReadable('info');
end;

end.

