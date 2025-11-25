unit ufrmpropertymanagedby;

{$mode objfpc}{$H+}

interface

uses
  buttons,
  Classes,
  Controls,
  Dialogs,
  ExtCtrls,
  Forms,
  SysUtils,
  StdCtrls,
  ActnList,
  uproperty,
  mormot.core.base,
  mormot.core.log,
  mormot.net.ldap,
  upropertyframe;

type

  { TPropertyManagedBy }

  TPropertyManagedBy = class
  private
    fLog: TSynLog;
    fManagerAttributes: TLdapAttributeList;
  public
    constructor Create;
    destructor Destroy; override;

    function AllowMembershipListUpdate(Allow: Boolean; Props: TProperty): Boolean;
    function AllowMembershipList(Props: TProperty): Boolean;
    function ChangeManagedBy(ManagedBy: RawUtf8; Props: TProperty): Boolean;
    function UpdateManagedByAttributes(ManagedBy: RawUtf8; Props: TProperty): Boolean;

    property ManagerAttributes: TLdapAttributeList read fManagerAttributes write fManagerAttributes;
  end;

  { TFrmPropertyManagedBy }

  TFrmPropertyManagedBy = class(TPropertyFrame)
    Action_Change: TAction;
    Action_Property: TAction;
    Action_Clear: TAction;
    Action_Manage: TAction;
    ActionList1: TActionList;
    Btn_Clear: TBitBtn;
    Btn_Change: TBitBtn;
    Btn_Property: TBitBtn;
    CheckBox_AllowManage: TCheckBox;
    Edit_CountryRegion: TEdit;
    Edit_FaxNumber: TEdit;
    Edit_City: TEdit;
    Edit_ManagedBy: TEdit;
    Edit_Office: TEdit;
    Edit_StateProvince: TEdit;
    Edit_TelephoneNumber: TEdit;
    Label_CountryRegion: TLabel;
    Label_ManagedBy: TLabel;
    Label_FaxNumber: TLabel;
    Label_City: TLabel;
    Labe_Office: TLabel;
    Label_StateProvince: TLabel;
    Label_StreetAddress: TLabel;
    Label_TelephoneNumber: TLabel;
    Line_Middle: TShape;
    Edit_StreetAddress: TMemo;
    Panel_Bottom: TPanel;
    Panel_Top: TPanel;
    procedure Action_ChangeExecute(Sender: TObject);
    procedure Action_ChangeUpdate(Sender: TObject);
    procedure Action_ClearExecute(Sender: TObject);
    procedure Action_ClearUpdate(Sender: TObject);
    procedure Action_ManageUpdate(Sender: TObject);
    procedure Action_PropertyExecute(Sender: TObject);
    procedure Action_PropertyUpdate(Sender: TObject);
  private
    fLog: TSynLog;
    fPropertyManagedBy: TPropertyManagedBy;
    fProperty: TProperty;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure Update(Props: TProperty); override;
  end;

const
  SELF_MEMBERSHIP_TEXT_GUID = '{bf9679c0-0de6-11d0-a285-00aa003049e2}';

implementation
uses
  mormot.core.os.security,
  mormot.core.text,
  ucommon,
  ursatldapclient,
  uOmniselect;

{$R *.lfm}

{ TPropertyManagedBy }

constructor TPropertyManagedBy.Create;
begin
  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);
end;

destructor TPropertyManagedBy.Destroy;
begin
  FreeAndNil(fManagerAttributes);

  inherited Destroy;
end;

function TPropertyManagedBy.AllowMembershipListUpdate(Allow: Boolean;
  Props: TProperty): Boolean;
var
  PSecDesc: PSecurityDescriptor;
  guid: TGuid;
  ManagedBySid: RawSid;
begin
  result := False;

  PSecDesc := Props.SecurityDescriptor;
  if not Assigned(PSecDesc) then
  begin
    if Assigned(fLog) then
      fLog.Log(sllTrace, 'Cannot retrieve nTSecurityDescriptor', Self);
    Exit;
  end;

  guid := RawUtf8ToGuid(SELF_MEMBERSHIP_TEXT_GUID);
  ManagedBySid := fManagerAttributes.Find('objectSid').GetRaw();

  if Allow then
  begin
    if not Assigned(SecDescAddACE(PSecDesc, guid, ManagedBySid, satObjectAccessAllowed, [samWriteProp])) then
    begin
      if Assigned(fLog) then
      begin
        fLog.Log(sllWarning, 'Cannot add ace to nTSecurityDescriptor', Self);
        fLog.Log(sllDebug, 'Guid: %', [SELF_MEMBERSHIP_TEXT_GUID], Self);
        fLog.Log(sllDebug, 'Sid: %', [ManagedBySid], Self);
      end;
      Exit;
    end;
  end
  else
  begin
    if not SecDescDeleteACE(PSecDesc, guid, ManagedBySid, satObjectAccessAllowed, [samWriteProp]) then
    begin
      if Assigned(fLog) then
      begin
        fLog.Log(sllWarning, 'Cannot delete ace to nTSecurityDescriptor', Self);
        fLog.Log(sllDebug, 'Guid: %', [SELF_MEMBERSHIP_TEXT_GUID], Self);
        fLog.Log(sllDebug, 'Sid: %', [ManagedBySid], Self);
      end;
      Exit;
    end;
  end;
  OrderAcl(Props.Core.LdapClient, Props.distinguishedName, Props.Core.LdapClient.DefaultDN, @PSecDesc^.Dacl);

  Props.SecurityDescriptor := PSecDesc;
  result := True;
end;

function TPropertyManagedBy.AllowMembershipList(Props: TProperty): Boolean;
var
  PSecDesc: PSecurityDescriptor;
  ManagedBySid: RawSid;
  guid: TGuid;
begin
  result := False;

  PSecDesc := Props.SecurityDescriptor;
  if not Assigned(PSecDesc) then
  begin
    if Assigned(fLog) then
      fLog.Log(sllWarning, 'Cannot retrieve nTSecurityDescriptor');
    Exit;
  end;

  guid := RawUtf8ToGuid(SELF_MEMBERSHIP_TEXT_GUID);
  ManagedBySid := fManagerAttributes.Find('objectSid').GetRaw();

  result := (SecDescFindACE(PSecDesc, satObjectAccessAllowed, ManagedBySid, [samWriteProp], @guid) >= 0);
end;

function TPropertyManagedBy.ChangeManagedBy(ManagedBy: RawUtf8; Props: TProperty
  ): Boolean;
begin
  result := False;

  // Remove old ACE
  AllowMembershipListUpdate(False, Props);

  // Change manager
  Props.managedBy := ManagedBy;
  result := True;
end;

function TPropertyManagedBy.UpdateManagedByAttributes(ManagedBy: RawUtf8;
  Props: TProperty): Boolean;
var
  LdapResult: TLdapResult;
begin
  result := False;

  if (ManagedBy <> '') then
  begin
    LdapResult := Props.Core.LdapClient.SearchObject(ManagedBy, '', [
      'name',
      'distinguishedName',
      'physicalDeliveryOfficeName',
      'streetAddress',
      'l',
      'st',
      'co',
      'telephoneNumber',
      'facsimileTelephoneNumber',
      'objectClass',
      'objectSid'
    ]);
    if not Assigned(LdapResult) then
    begin
      if Assigned(fLog) then
        fLog.Log(sllError, 'Ldap Search Error: "%"', [Props.Core.LdapClient.ResultString], Self);
      ShowLdapSearchError(Props.Core.LdapClient);
      Exit;
    end;
    fManagerAttributes := TLdapAttributeList(LdapResult.Attributes.Clone);
  end
  else
    FreeAndNil(fManagerAttributes);

  result := True;
end;

{ TFrmPropertyManagedBy }

procedure TFrmPropertyManagedBy.Action_ChangeUpdate(Sender: TObject);
begin
  Action_Change.Enabled := True;
end;

procedure TFrmPropertyManagedBy.Action_ClearExecute(Sender: TObject);
begin
  fPropertyManagedBy.ChangeManagedBy('', fProperty);
  Update(fProperty);
end;

procedure TFrmPropertyManagedBy.Action_ClearUpdate(Sender: TObject);
begin
  Action_Property.Enabled := (fProperty.managedBy <> '');
end;

procedure TFrmPropertyManagedBy.Action_ManageUpdate(Sender: TObject);
begin
  Action_Manage.Enabled := (fProperty.managedBy <> '');
end;

procedure TFrmPropertyManagedBy.Action_PropertyExecute(Sender: TObject);
begin
  fProperty.Core.OpenProperty(fProperty.managedBy);
end;

procedure TFrmPropertyManagedBy.Action_PropertyUpdate(Sender: TObject);
begin
  Action_Property.Enabled := (fProperty.managedBy <> '');
end;

procedure TFrmPropertyManagedBy.Action_ChangeExecute(Sender: TObject);
var
  Filter, ManagerDN: RawUtf8;
  Omniselect: TVisOmniselect;
  DNarr: TRawUtf8DynArray;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Change managedBy', Self);

  // Set Filter
  Filter := FormatUtf8('(!(distinguishedName=%))', [LdapEscape(fProperty.distinguishedName)]); // Dont allow self

  // Omniselect
  DNarr := [''];
  Omniselect := TVisOmniselect.Create(self, fProperty.Core.LdapClient, ['user', 'group', 'contacts'], fProperty.Core.LdapClient.DefaultDN(), False, Filter);
  try
    Omniselect.Caption := rsTitleSelectNewManager;
    if Omniselect.ShowModal() <> mrOK then
    begin
      if Assigned(fLog) then
        fLog.Log(sllInfo, 'Action cancel by user', Self);
      Exit;
    end;
    DNarr := Omniselect.SelectedObjects;
    if (Length(DNarr) <> 1) then
    begin
      if Assigned(fLog) then
        fLog.Log(sllInfo, 'Invalid number of selected DN');
      Exit;
    end;
    ManagerDN := DNarr[0];
  finally
    FreeAndNil(Omniselect);
  end;

  fPropertyManagedBy.ChangeManagedBy(ManagerDN, fProperty);
  Update(fProperty);
end;

constructor TFrmPropertyManagedBy.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fPropertyManagedBy := TPropertyManagedBy.Create;

  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);

  Caption := 'Managed By';

  UnifyButtonsWidth([Btn_Clear, Btn_Change, Btn_Property]);
end;

destructor TFrmPropertyManagedBy.Destroy;
begin
  FreeAndNil(fPropertyManagedBy);

  inherited Destroy;
end;

procedure TFrmPropertyManagedBy.Update(Props: TProperty);
var
  ManagedBy: RawUtf8;
  ManagedByAttributes: TLdapAttributeList;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Update', Self);

  fProperty := Props;

  ManagedBy := fProperty.managedBy;

  fPropertyManagedBy.UpdateManagedByAttributes(ManagedBy, fProperty);

  ManagedByAttributes := fPropertyManagedBy.fManagerAttributes;

  Edit_ManagedBy.Text := DNToCN(ManagedByAttributes.GetByName('distinguishedName'));
  Edit_Office.Text := ManagedByAttributes.GetByName('physicalDeliveryOfficeName');
  Edit_StreetAddress.Text := ManagedByAttributes.GetByName('streetAddress');
  Edit_City.Text := ManagedByAttributes.GetByName('l');
  Edit_StateProvince.Text := ManagedByAttributes.GetByName('st');
  Edit_CountryRegion.Text := ManagedByAttributes.GetByName('co');
  Edit_TelephoneNumber.Text := ManagedByAttributes.GetByName('telephoneNumber');
  Edit_FaxNumber.Text := ManagedByAttributes.GetByName('facsimileTelephoneNumber');

  // Manager can update membership list
  CheckBox_AllowManage.Checked := fPropertyManagedBy.AllowMembershipList(fProperty);
end;

end.

