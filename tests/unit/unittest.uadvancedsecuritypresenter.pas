unit unittest.uadvancedsecuritypresenter;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.base,
  mormot.core.text,
  mormot.core.variants,
  mormot.core.os.security,
  mormot.core.test,
  mormot.net.ldap,
  fixture.fakevisadvancedsecurity,
  uadvancedsecuritypresenter;

const
  cTEST_SD_TEXT = 'O:S-1-5-21-0000000000-000000000-0000000000-512' +
    'G:S-1-5-21-0000000000-000000000-0000000000-512' +
    'D:AI(A;;LCRPLORC;;;PS)' +
    '(A;;RC;;;AU)' +
    '(A;;CCDCLCSWRPWPDTLOCRSDRCWDWO;;;SY)' +
    '(A;;CCDCLCSWRPWPDTLOCRSDRCWDWO;;;AO)';
type

  { TTestAdvancedSecurityPresenter }

  TTestAdvancedSecurityPresenter = class(TSynTestCase)
  public
    View: TFakeVisAdvancedSecurity;
    Presenter: TAdvancedSecurityPresenter;
    SD: TSecurityDescriptor;

    procedure MethodSetup; override;
    procedure MethodCleanUp; override;
    procedure Setup; override;
  published
    procedure CreatePresenter_WithoutFakeView;
    procedure CreatePresenter_WithFakeView;
    procedure SetLdapClient;
    procedure SetName;
    procedure SetSecurityDescriptor_Empty;
    procedure SetSecurityDescriptor_ValidValues;
    procedure GetSecurityDescriptor;
    procedure DoACESelectionChanged_Negative;
    procedure DoACESelectionChanged_OutOfRange;
    procedure DoACESelectionChanged_Valid;
    procedure DoRightPanelMaskChanged;
    procedure DoRightPanelTypeChanged;
    procedure DoRightPanelPrincipalChanged;
    procedure DoRightPanelObjectChanged;
    procedure DoRightPanelFlagsChanged;
    procedure DoRightPanelInheritedObjectChanged;
  end;

implementation

{ TTestAdvancedSecurityPresenter }

procedure TTestAdvancedSecurityPresenter.MethodSetup;
begin
  View := TFakeVisAdvancedSecurity.Create;
  Presenter := TAdvancedSecurityPresenter.Create(View);
end;

procedure TTestAdvancedSecurityPresenter.MethodCleanUp;
begin
  FreeAndNil(Presenter);
  FreeAndNil(View);
end;

procedure TTestAdvancedSecurityPresenter.Setup;
begin
  SD.FromText(cTEST_SD_TEXT);
end;

procedure TTestAdvancedSecurityPresenter.CreatePresenter_WithoutFakeView;
begin
  try
    TAdvancedSecurityPresenter.Create(nil);
    Check(False, 'Presenter should raise an exception with empty view.');
  except
    on E: Exception do
    begin
      Check(True, 'Presenter raised an exception with empty view.');
    end;
  end;
end;

procedure TTestAdvancedSecurityPresenter.CreatePresenter_WithFakeView;
begin
  Check(Presenter.SDChanged = False, 'SDChanged default is False.');
  Check(Assigned(Presenter.SidCache), 'SIDCache default is assigned.');
  Check(Presenter.SidCache.Count = 0, 'SIDCache default count is 0.');
  Check(Assigned(Presenter.GuidCache), 'GUIDCache default is assigned.');
  Check(Presenter.GuidCache.Count = 0, 'GUIDCache default count is 0.');
  Check(not Assigned(Presenter.LdapClient), 'LdapClient default is not Assigned.');
end;

procedure TTestAdvancedSecurityPresenter.SetLdapClient;
var
  LdapClient: TLdapClient;
begin
  Presenter.SetLdapClient(nil);
  Check(not Assigned(Presenter.LdapClient), 'LdapClient is assigned to nil.');
  LdapClient := TLdapClient.Create;
  try
    Presenter.SetLdapClient(LdapClient);
    Check(Assigned(Presenter.LdapClient), 'LdapClient is assigned.');
  finally
    FreeAndNil(LdapClient);
  end;
end;

procedure TTestAdvancedSecurityPresenter.SetName;
begin
  Presenter.SetName('Name');
  Check(View.Title = 'Name', 'SetName send the name to the view SetTitle.');
  Check(View.SetTitleCount = 1, 'SetName has been called once.');
  Presenter.SetName('Name');
  Check(View.SetTitleCount = 1, 'SetName with same name does not call SetTitle.');
  Presenter.SetName('NewName');
  Check(View.Title = 'NewName', 'SetName send the new name to the view SetTitle.');
  Check(View.SetTitleCount = 2, 'SetName has been called twice.');
end;

procedure TTestAdvancedSecurityPresenter.SetSecurityDescriptor_Empty;
var
  EmptySD: TSecurityDescriptor;
begin
  Presenter.SetSecurityDescriptor(EmptySD);
  Check(View.OwnerText = '', 'Empty SD does not have owner.');
  Check(View.SetOwnerCount = 1, 'SetOwner has been called once.');
  Check(View.GroupText = '', 'Empty SD does not have group.');
  Check(View.SetGroupCount = 1, 'SetGroup has been called once.');
  Check(View.ACEGrid.Count = 0, 'Empty DACL provide empty ACE Grid.');
  Check(View.RefreshACEGridCount = 1, 'RefreshACEGrid has been called once.');
end;

procedure TTestAdvancedSecurityPresenter.SetSecurityDescriptor_ValidValues;
begin
  Presenter.SidCache.AddSID(RawSidToText(SD.Owner));
  Presenter.SidCache.SetSIDName(RawSidToText(SD.Owner), 'Domain Admins');

  Presenter.SetSecurityDescriptor(SD);

  Check(Presenter.SidCache.Count = 5, 'Test SD has 5 different SID.');
  Check(Presenter.GuidCache.Count = 0, 'Test SD cannot resolve GUID without LdapClient connected.');

  Check(View.OwnerText = 'Domain Admins', 'Test SD has "Domain Admins" owner.');
  Check(View.SetOwnerCount = 1, 'SetOwner has been called once.');
  Check(View.GroupText = 'Domain Admins', 'Test SD has "Domain Admins" group.');
  Check(View.SetGroupCount = 1, 'SetGroup has been called once.');
  Check(View.ACEGrid.Count = 4, 'Test SD has 4 ACE.');
  Check(View.RefreshACEGridCount = 1, 'RefreshACEGrid has been called once.');
end;

procedure TTestAdvancedSecurityPresenter.GetSecurityDescriptor;
begin
  Presenter.SetSecurityDescriptor(SD);
  Check(Presenter.GetSecurityDescriptor.IsEqual(SD), 'SD are equal if no modifications are performed.');
end;

procedure TTestAdvancedSecurityPresenter.DoACESelectionChanged_Negative;
begin
  Presenter.SetSecurityDescriptor(SD);
  Presenter.DoACESelectionChanged(-1);

  Check(View.SetRightPanelTypeCount = 0, 'Negative Selection index does not update RightPanelType.');
  Check(View.SetRightPanelAccountCount = 0, 'Negative Selection index does not update RightPanelAccount.');
  Check(View.SetRightPanelMaskCount = 0, 'Negative Selection index does not update RightPanelMask.');
  Check(View.SetRightPanelObjectCount = 0, 'Negative Selection index does not update RightPanelObject.');
  Check(View.SetRightPanelFlagsCount = 0, 'Negative Selection index does not update RightPanelFlags.');
  Check(View.SetRightPanelInheritedObjectCount = 0, 'Negative Selection index does not update RightPanelInheritedObject.');
end;

procedure TTestAdvancedSecurityPresenter.DoACESelectionChanged_OutOfRange;
begin
  Presenter.SetSecurityDescriptor(SD);
  Presenter.DoACESelectionChanged(8);

  Check(View.SetRightPanelTypeCount = 0, 'OutOfRange Selection index does not update RightPanelType.');
  Check(View.SetRightPanelAccountCount = 0, 'OutOfRange Selection index does not update RightPanelAccount.');
  Check(View.SetRightPanelMaskCount = 0, 'OutOfRange Selection index does not update RightPanelMask.');
  Check(View.SetRightPanelObjectCount = 0, 'OutOfRange Selection index does not update RightPanelObject.');
  Check(View.SetRightPanelFlagsCount = 0, 'OutOfRange Selection index does not update RightPanelFlags.');
  Check(View.SetRightPanelInheritedObjectCount = 0, 'OutOfRange Selection index does not update RightPanelInheritedObject.');
end;

procedure TTestAdvancedSecurityPresenter.DoACESelectionChanged_Valid;
begin
  Presenter.SetSecurityDescriptor(SD);
  Presenter.DoACESelectionChanged(1);

  Check(View.SetRightPanelTypeCount = 1, 'Valid Selection index update RightPanelType.');
  Check(View.RightPanelType = True, 'Valid Selection index provide valid type info.');
  Check(View.SetRightPanelAccountCount = 1, 'Valid Selection index update RightPanelAccount.');
  Check(View.RightPanelAccount = 'Authenticated User', 'Valid Selection index provide valid account info.');
  Check(View.SetRightPanelMaskCount = 1, 'Valid Selection index update RightPanelMask.');
  Check(View.RightPanelMask = [samReadControl], 'Valid Selection index provide valid mask info.');
  Check(View.SetRightPanelObjectCount = 1, 'Valid Selection index update RightPanelObject.');
  Check(View.RightPanelObject = '', 'Valid Selection index provide valid object info.');
  Check(View.SetRightPanelFlagsCount = 1, 'Valid Selection index update RightPanelFlags.');
  Check(View.RightPanelFlags = [], 'Valid Selection index provide valid flags info.');
  Check(View.SetRightPanelInheritedObjectCount = 1, 'Valid Selection index update RightPanelInheritedObject.');
  Check(View.RightPanelInheritedObject = '', 'Valid Selection index provide valid inheritedObject info.');
end;

procedure TTestAdvancedSecurityPresenter.DoRightPanelMaskChanged;
begin
  Presenter.SetSecurityDescriptor(SD);
  Presenter.DoACESelectionChanged(0);

  View.RightPanelMask := [samReadControl, samReadProp, samListObject];

  Presenter.DoRightPanelMaskChanged;

  Check(Presenter.SDChanged, 'SD has been changed.');
  Check(View.ACEGrid._[0]^.I['rights'] = 131216, 'Focused ACE rights has been updated.');
  Check(View.ACEGrid._[0]^.S['permissions'] = 'Read, List Object, Read Control', 'Focused ACE permission has been updated.');
  Check(View.ACEGrid._[0]^.I['state'] = 2, 'Focused ACE state has been updated to 2 (modified).');

  Check(View.RefreshACEGridIndexCount = 1, 'RefreshACEGridIndex has been called once.');
  Check(View.RefreshACEGridIndexIndex = 0, 'RefreshACEGridIndex index is 0.');
end;

procedure TTestAdvancedSecurityPresenter.DoRightPanelTypeChanged;
begin
  Presenter.SetSecurityDescriptor(SD);
  Presenter.DoACESelectionChanged(1);

  View.RightPanelType := False;

  Presenter.DoRightPanelTypeChanged;

  Check(Presenter.SDChanged, 'SD has been changed.');
  Check(View.ACEGrid._[1]^.I['ace_type'] = 2, 'Focused ACE ace_type has been updated.');
  Check(View.ACEGrid._[1]^.B['type'] = False, 'Focused ACE type has been updated.');
  Check(View.ACEGrid._[1]^.I['state'] = 2, 'Focused ACE state has been updated to 2 (modified).');

  Check(View.RefreshACEGridIndexCount = 1, 'RefreshACEGridIndex has been called once.');
  Check(View.RefreshACEGridIndexIndex = 1, 'RefreshACEGridIndex index is 1.');
end;

procedure TTestAdvancedSecurityPresenter.DoRightPanelPrincipalChanged;
begin
  Presenter.SetSecurityDescriptor(SD);
  Presenter.DoACESelectionChanged(1);

  View.RightPanelAccount := 'test_sid';
  Presenter.SidCache.AddSID('S-1-5-333');
  Presenter.SidCache.SetSIDName('S-1-5-333', 'test_sid');

  Presenter.DoRightPanelPrincipalChanged;

  Check(Presenter.SDChanged, 'SD has been changed.');
  Check(View.ACEGrid._[1]^.S['account_sid'] = 'S-1-5-333', 'Focused ACE account_sid has been updated.');
  Check(View.ACEGrid._[1]^.S['account'] = 'test_sid', 'Focused ACE account has been updated.');
  Check(View.ACEGrid._[1]^.I['state'] = 2, 'Focused ACE state has been updated to 2 (modified).');

  Check(View.RefreshACEGridIndexCount = 1, 'RefreshACEGridIndex has been called once.');
  Check(View.RefreshACEGridIndexIndex = 1, 'RefreshACEGridIndex index is 1.');
end;

procedure TTestAdvancedSecurityPresenter.DoRightPanelObjectChanged;
begin
  Presenter.SetSecurityDescriptor(SD);
  Presenter.DoACESelectionChanged(1);

  View.RightPanelObject := 'test_guid';
  Presenter.GuidCache.AddGUID('9b026da6-0d3c-465c-8bee-5199d7165cba', '');
  Presenter.GuidCache.SetGUIDName('9b026da6-0d3c-465c-8bee-5199d7165cba', 'test_guid', '');

  Presenter.DoRightPanelObjectChanged;

  Check(Presenter.SDChanged, 'SD has been changed.');
  Check(View.ACEGrid._[1]^.S['object_guid'] = '9B026DA6-0D3C-465C-8BEE-5199D7165CBA', 'Focused ACE object_guid has been updated.');
  Check(View.ACEGrid._[1]^.S['object'] = 'test_guid', 'Focused ACE object has been updated.');
  Check(View.ACEGrid._[1]^.I['state'] = 2, 'Focused ACE state has been updated to 2 (modified).');

  Check(View.RefreshACEGridIndexCount = 1, 'RefreshACEGridIndex has been called once.');
  Check(View.RefreshACEGridIndexIndex = 1, 'RefreshACEGridIndex index is 1.');
end;

procedure TTestAdvancedSecurityPresenter.DoRightPanelFlagsChanged;
begin
  Presenter.SetSecurityDescriptor(SD);
  Presenter.DoACESelectionChanged(1);

  View.RightPanelFlags := [safContainerInherit];

  Presenter.DoRightPanelFlagsChanged;

  Check(Presenter.SDChanged, 'SD has been changed.');
  Check(View.ACEGrid._[1]^.I['ace_flags'] = 2, 'Focused ACE ace_flags has been updated.');
  Check(View.ACEGrid._[1]^.S['flags'] = 'This object and all descendants', 'Focused ACE flags has been updated.');
  Check(View.ACEGrid._[1]^.I['state'] = 2, 'Focused ACE state has been updated to 2 (modified).');

  Check(View.RefreshACEGridIndexCount = 1, 'RefreshACEGridIndex has been called once.');
  Check(View.RefreshACEGridIndexIndex = 1, 'RefreshACEGridIndex index is 1.');
end;

procedure TTestAdvancedSecurityPresenter.DoRightPanelInheritedObjectChanged;
begin
  Presenter.SetSecurityDescriptor(SD);
  Presenter.DoACESelectionChanged(1);

  View.RightPanelInheritedObject := 'test_guid';
  Presenter.GuidCache.AddGUID('9b026da6-0d3c-465c-8bee-5199d7165cba', '');
  Presenter.GuidCache.SetGUIDName('9b026da6-0d3c-465c-8bee-5199d7165cba', 'test_guid', '');

  Presenter.DoRightPanelInheritedObjectChanged;

  Check(Presenter.SDChanged, 'SD has been changed.');
  Check(View.ACEGrid._[1]^.S['inherited_object_guid'] = '9B026DA6-0D3C-465C-8BEE-5199D7165CBA', 'Focused ACE inherited object guid has been updated.');
  Check(View.ACEGrid._[1]^.S['inheritedObject'] = 'test_guid', 'Focused ACE inhertied object has been updated.');
  Check(View.ACEGrid._[1]^.I['state'] = 2, 'Focused ACE state has been updated to 2 (modified).');

  Check(View.RefreshACEGridIndexCount = 1, 'RefreshACEGridIndex has been called once.');
  Check(View.RefreshACEGridIndexIndex = 1, 'RefreshACEGridIndex index is 1.');
end;

end.

