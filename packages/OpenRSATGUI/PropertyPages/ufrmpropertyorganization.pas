unit ufrmpropertyorganization;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  StdCtrls,
  Buttons, ExtCtrls, ActnList,
  mormot.core.base,
  mormot.core.log,
  uproperty,
  tis.ui.grid.core,
  upropertyframe;

type

  { TFrmPropertyOrganization }

  TFrmPropertyOrganization = class(TPropertyFrame)
    Action_Change: TAction;
    Action_Properties: TAction;
    Action_Clear: TAction;
    ActionList1: TActionList;
    BitBtn_Change: TBitBtn;
    BitBtn_Clear: TBitBtn;
    BitBtn_Property: TBitBtn;
    Edit_Company: TEdit;
    Edit_Department: TEdit;
    Edit_Manager: TEdit;
    Edit_Title: TEdit;
    GroupBox_Manager: TGroupBox;
    Label_Company: TLabel;
    Label_Department: TLabel;
    Label_DirectReports: TLabel;
    Label_Manager: TLabel;
    Label_Title: TLabel;
    Panel_Organization: TPanel;
    Panel_DirectReports: TPanel;
    TisGrid_DirectReports: TTisGrid;
    procedure Action_ChangeExecute(Sender: TObject);
    procedure Action_ClearExecute(Sender: TObject);
    procedure Action_ClearUpdate(Sender: TObject);
    procedure Action_PropertiesExecute(Sender: TObject);
    procedure Action_PropertiesUpdate(Sender: TObject);
    procedure Edit_CompanyChange(Sender: TObject);
    procedure Edit_DepartmentChange(Sender: TObject);
    procedure Edit_TitleChange(Sender: TObject);
  private
    fLog: TSynLog;
    fProperty: TProperty;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Update(Props: TProperty); override;

  end;

implementation
uses
  mormot.core.text,
  mormot.core.variants,
  mormot.net.ldap,
  ucommon,
  uhelpersui,
  uOmniselect;

{$R *.lfm}

{ TFrmPropertyOrganization }

procedure TFrmPropertyOrganization.Edit_TitleChange(Sender: TObject);
begin
  fProperty.Add('title', Edit_Title.Caption);
end;

procedure TFrmPropertyOrganization.Edit_DepartmentChange(Sender: TObject);
begin
  fProperty.Add('department', Edit_Department.Caption);
end;

procedure TFrmPropertyOrganization.Edit_CompanyChange(Sender: TObject);
begin
  fProperty.Add('company', Edit_Company.Caption);
end;

procedure TFrmPropertyOrganization.Action_ChangeExecute(Sender: TObject);
var
  Filter: RawUtf8;
  Omniselect: TVisOmniselect;
  DNarr: TRawUtf8DynArray;
begin
  // Set Filter
  Filter := FormatUtf8('(!(distinguishedName=%))', [LdapEscape(fProperty.DistinguishedName)]); // Dont allow self

  // Omniselect
  Omniselect := TVisOmniselect.Create(self, fProperty.LdapClient, ['user'], fProperty.LdapClient.DefaultDN, False, Filter);
  try
    Omniselect.Caption := rsTitleSelectNewManager;
    if Omniselect.ShowModal() <> mrOK then
      Exit;
    DNarr := Omniselect.SelectedObjects;
    if Length(DNarr) <> 1 then
      Exit;
    fProperty.Add('manager', DNarr[0]);
  finally
    FreeAndNil(Omniselect);
  end;
  Update(fProperty);
end;

procedure TFrmPropertyOrganization.Action_ClearExecute(Sender: TObject);
begin
  fProperty.Add('manager', '');
  Update(fProperty);
end;

procedure TFrmPropertyOrganization.Action_ClearUpdate(Sender: TObject);
begin
  Action_Clear.Enabled := Edit_Manager.Caption <> '';
end;

procedure TFrmPropertyOrganization.Action_PropertiesExecute(Sender: TObject);
begin
  fProperty.Core.OpenProperty(fProperty.GetReadable('manager'));
end;

procedure TFrmPropertyOrganization.Action_PropertiesUpdate(Sender: TObject);
begin
  Action_Properties.Enabled := Edit_Manager.Caption <> '';
end;

constructor TFrmPropertyOrganization.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);

  Caption := 'Organization';

  UnifyButtonsWidth([BitBtn_Change, BitBtn_Property, BitBtn_Clear]);
end;

procedure TFrmPropertyOrganization.Update(Props: TProperty);
var
  Row: TDocVariantData;
  DirectReport: RawUtf8;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Update', Self);

  fProperty := Props;

  Edit_Title.CaptionNoChange := fProperty.GetReadable('title');
  Edit_Department.CaptionNoChange := fProperty.GetReadable('department');
  Edit_Company.CaptionNoChange := fProperty.GetReadable('company');

  Edit_Manager.CaptionNoChange := fProperty.ManagerName;

  TisGrid_DirectReports.BeginUpdate;
  try
    Row.Init();
    for DirectReport in fProperty.DirectReportsNames do
    begin
      if DirectReport = '' then
        Continue;
      Row.S['name'] := DirectReport;
      TisGrid_DirectReports.Data.AddItem(Row);
      Row.Clear;
    end;
  finally
    TisGrid_DirectReports.EndUpdate;
    TisGrid_DirectReports.LoadData();
  end;
end;

end.

