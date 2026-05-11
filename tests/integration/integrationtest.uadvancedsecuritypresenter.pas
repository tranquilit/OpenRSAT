unit integrationtest.uadvancedsecuritypresenter;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.os.security,
  mormot.core.base,
  mormot.core.test,
  mormot.core.text,
  mormot.core.variants,
  mormot.net.ldap,
  fixture.ldapclient,
  fixture.fakevisadvancedsecurity,
  uadvancedsecuritypresenter;

type

  { TIntegrationTestAdvancedSecurityPresenter }

  TIntegrationTestAdvancedSecurityPresenter = class(TSynTestCase)
  private
    View: TFakeVisAdvancedSecurity;
    Presenter: TAdvancedSecurityPresenter;
    LdapClient: TLdapClient;
    BaseDN: RawUtf8;
    DN: RawUtf8;
    SD: TSecurityDescriptor;
  public
    procedure Setup; override;
    procedure CleanUp; override;
    procedure MethodSetup; override;
    procedure MethodCleanUp; override;
  published
    procedure ActionApply_NoChanges;
    procedure ActionApply_OwnerChanged;
    procedure ActionApply_NewACE;
    procedure ActionApply_DeleteACE;
    procedure ActionApply_NewDeleteModifyACL;
  end;

implementation

{ TIntegrationTestAdvancedSecurityPresenter }

procedure TIntegrationTestAdvancedSecurityPresenter.Setup;
begin
  LdapClient := SetupLdapClient;
  BaseDN := GetBaseDN(LdapClient.DefaultDN());
end;

procedure TIntegrationTestAdvancedSecurityPresenter.CleanUp;
begin
  if Assigned(LdapClient) then
    FreeAndNil(LdapClient);
end;

procedure TIntegrationTestAdvancedSecurityPresenter.MethodSetup;
begin
  if not SetupTestUser(LdapClient, ClassName, DN) then
    raise Exception.Create('Failed to setup test user.');

  if not SD.FromBinary(LdapClient.SearchObject(DN, '', 'nTSecurityDescriptor').GetRaw()) then
    raise Exception.Create('Failed to load test user SD.');

  View := TFakeVisAdvancedSecurity.Create;
  Presenter := TAdvancedSecurityPresenter.Create(View);
  Presenter.SetDistinguishedName(DN);
  Presenter.SetLdapClient(LdapClient);
  Presenter.SetSecurityDescriptor(SD);
end;

procedure TIntegrationTestAdvancedSecurityPresenter.MethodCleanUp;
begin
  if Assigned(LdapClient) then
    LdapClient.Delete(DN);
  if Assigned(View) then
    FreeAndNil(View);
  if Assigned(Presenter) then
    FreeAndNil(Presenter);
end;

procedure TIntegrationTestAdvancedSecurityPresenter.ActionApply_NoChanges;
begin
  Presenter.ActionApply;

  Check(not Presenter.SDChanged, 'No changes left.');
  Check(SD.IsEqual(Presenter.GetSecurityDescriptor), 'No changes so SD are equals.');
end;

procedure TIntegrationTestAdvancedSecurityPresenter.ActionApply_OwnerChanged;
var
  GroupDN: RawUtf8;
  GroupAttributes: TLdapResult;
  NewSD: TSecurityDescriptor;
begin
  // Prepare group to be used as new owner.
  SetupTestGroup(LdapClient, ClassName, GroupDN);
  GroupAttributes := TLdapResult(LdapClient.SearchObject(GroupDN, '', ['name', 'objectSid']).Clone);

  View.PickOwnerName := GroupAttributes.Find('name').GetReadable();
  View.PickOwnerSID := GroupAttributes.Find('objectSid').GetReadable();

  Presenter.ActionSelectOwner;

  Check(Presenter.SDChanged, 'Owner has been changed.');
  Presenter.ActionApply;
  Check(not Presenter.SDChanged, 'No changes left after apply.');
  Check(NewSD.FromBinary(LdapClient.SearchObject(DN, '', 'nTSecurityDescriptor').GetRaw()), 'NewSD can be retrieved and loaded.');
  Check(RawSidToText(NewSD.Owner) = GroupAttributes.Find('objectSid').GetReadable(), 'New SD owner is the test group.');
  FreeAndNil(GroupAttributes);
end;

procedure TIntegrationTestAdvancedSecurityPresenter.ActionApply_NewACE;
var
  GroupDN, sid: RawUtf8;
  GroupAttributes: TLdapResult;
  NewSD: TSecurityDescriptor;
  Found: Boolean;
  i: Integer;
  pCount: SizeInt;
begin
  // Setup group for test
  SetupTestGroup(LdapClient, ClassName, GroupDN);
  GroupAttributes := TLdapResult(LdapClient.SearchObject(GroupDN, '', ['name', 'objectSid']).Clone);

  pCount := Length(Presenter.GetSecurityDescriptor.Dacl);
  // Add Group SID to Sid cache
  Presenter.SidCache.AddSID(GroupAttributes.Find('objectSid').GetReadable());
  Presenter.SidCache.SetSIDName(GroupAttributes.Find('objectSid').GetReadable(), GroupAttributes.Find('name').GetReadable());
  // Add New ACE
  Presenter.DoACESelectionChanged(Presenter.ActionAddACE);
  // Setup vis to fill the new ACE
  View.RightPanelType := True;
  View.RightPanelAccount := GroupAttributes.Find('name').GetReadable();
  View.RightPanelMask := [samReadProp, samReadControl];
  // Update the new ACE
  Presenter.DoRightPanelChanged;

  Check(Presenter.SDChanged, 'ACE has been added.');
  Presenter.ActionApply;
  Check(not Presenter.SDChanged, 'No changes left after apply.');
  Check(NewSD.FromBinary(LdapClient.SearchObject(DN, '', 'nTSecurityDescriptor').GetRaw()), 'NewSD can be retrieved and loaded.');
  Check(Length(NewSD.Dacl) = pCount + 1, 'One ACE has been added.');
  // Search new SD
  sid := GroupAttributes.Find('objectSid').GetReadable();
  Found := False;
  for i := 0 to High(NewSD.Dacl) do
    if RawSidToText(NewSD.Dacl[i].Sid) = sid then
    begin
      Found := True;
      break;
    end;
  Check(Found, 'New ACE exists in SD.');
  Check(RawSidToText(NewSD.Dacl[i].Sid) = sid, 'New ACE has coorect sid.');
  Check(NewSD.Dacl[i].AceType = satAccessAllowed, 'New ACE has correct type.');
  Check(NewSD.Dacl[i].Mask = [samReadControl, samReadProp], 'New ACE has correct mask.');
  FreeAndNil(GroupAttributes);
end;

procedure TIntegrationTestAdvancedSecurityPresenter.ActionApply_DeleteACE;
var
  GroupDN, sid: RawUtf8;
  GroupAttributes: TLdapResult;
  pCount: SizeInt;
  NewSD: TSecurityDescriptor;
  Found: Boolean;
  i: Integer;
begin
  // Setup group for test
  SetupTestGroup(LdapClient, ClassName, GroupDN);
  GroupAttributes := TLdapResult(LdapClient.SearchObject(GroupDN, '', ['name', 'objectSid']).Clone);

  SetLength(SD.Dacl, Length(SD.Dacl) + 1);
  SD.Dacl[Length(SD.Dacl) - 1].Sid := GroupAttributes.Find('objectSid').GetRaw();
  SD.Dacl[Length(SD.Dacl) - 1].AceType := satAccessAllowed;
  SD.Dacl[Length(SD.Dacl) - 1].Mask := [samWriteOwner, samWriteProp];
  if not LdapClient.Modify(DN, lmoReplace, 'nTSecurityDescriptor', SD.ToBinary) then
    raise Exception.Create(LdapClient.ResultString);

  pCount := Length(SD.Dacl);
  SD.FromBinary(LdapClient.SearchObject(DN, '', 'nTSecurityDescriptor').GetRaw());
  Presenter.SetSecurityDescriptor(SD);

  // Search new SD
  sid := GroupAttributes.Find('objectSid').GetReadable();
  Found := False;
  for i := 0 to High(SD.Dacl) do
    if RawSidToText(SD.Dacl[i].Sid) = sid then
    begin
      Found := True;
      break;
    end;

  Presenter.DoACESelectionChanged(i);
  Presenter.ActionDeleteACE;

  Check(Presenter.SDChanged, 'ACE has been removed.');
  Presenter.ActionApply;
  Check(not Presenter.SDChanged, 'No changes left after apply.');
  Check(NewSD.FromBinary(LdapClient.SearchObject(DN, '', 'nTSecurityDescriptor').GetRaw()), 'NewSD can be retrieved and loaded.');
  Check(Length(NewSD.Dacl) = pCount - 1, 'One ACE has been removed.');
  Found := False;
  for i := 0 to High(NewSD.Dacl) do
    if RawSidToText(NewSD.Dacl[i].Sid) = sid then
    begin
      Found := True;
      break;
    end;
  Check(not Found, 'Delete ACE does not exists in SD.');
  FreeAndNil(GroupAttributes);
end;

procedure TIntegrationTestAdvancedSecurityPresenter.ActionApply_NewDeleteModifyACL;
begin

end;

end.

