unit integrationtest.ugpocore;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.base,
  mormot.core.test,
  mormot.core.text,
  mormot.net.ldap,
  fixture.ldapclient,
  ugpocore;

type

  { TIntegrationTestGPOCore }

  TIntegrationTestGPOCore = class(TSynTestCase)
  private
    Logic: TGPOLogic;
    LdapClient: TLdapClient;
    DomainDN: RawUtf8;
    GPOList: TGPOList;
  public
    procedure Setup; override;
    procedure CleanUp; override;
    procedure MethodSetup; override;
    procedure MethodCleanUp; override;
  published
    procedure List_ReturnsExistingGPOs;
    procedure Create_ValidGPO;
    procedure Create_EmptyName;
    procedure Create_DuplicatedName;
    procedure FindByName_Existing;
    procedure FindByName_Missing;
    procedure Rename_ValidName;
    procedure Rename_EmptyName;
    procedure Duplicate_Valid;
    procedure Duplicate_EmptyName;
    procedure UpdateConfiguration_Valid;
    procedure UpdateConfiguration_NoChange;
    procedure Delete_Existing;
  end;

implementation

procedure TIntegrationTestGPOCore.Setup;
begin
  LdapClient := SetupLdapClient;
  DomainDN := LdapClient.DefaultDN;
end;

procedure TIntegrationTestGPOCore.CleanUp;
begin
  if Assigned(LdapClient) then
    FreeAndNil(LdapClient);
end;

procedure TIntegrationTestGPOCore.MethodSetup;
begin
  Logic := TGPOLogic.Create(LdapClient);
end;

procedure TIntegrationTestGPOCore.MethodCleanUp;
var
  GPO: TGPO;
begin
  for GPO in GPOList do
    GPO.Free;
  GPOList := nil;

  FreeAndNil(Logic);
end;

procedure TIntegrationTestGPOCore.List_ReturnsExistingGPOs;
begin
  GPOList := Logic.List(DomainDN);

  Check(Length(GPOList) > 0, 'At least one GPO should exist in the domain');
end;

procedure TIntegrationTestGPOCore.Create_ValidGPO;
var
  DN: RawUtf8;
begin
  DN := Logic.Add(DomainDN, 'OpenRSAT Test GPO');

  Check(DN <> '', 'Created GPO should return its distinguished name');

  if (DN <> '') then
    Logic.Delete(DN);
end;

procedure TIntegrationTestGPOCore.Create_EmptyName;
var
  RaisedException: Boolean;
begin
  RaisedException := False;
  try
    Logic.Add(DomainDN, '');
  except
    on E: EGPOException do
      RaisedException := True;
  end;

  Check(RaisedException, 'Create with an empty name should raise EGPOException');
end;

procedure TIntegrationTestGPOCore.Create_DuplicatedName;
var
  DN: RawUtf8;
  RaisedException: Boolean;
begin
  DN := Logic.Add(DomainDN, 'OpenRSAT Test GPO');
  Check(DN <> '', 'Created GPO should return its distinguished name');

  RaisedException := False;
  try
    Logic.Add(DomainDN, 'OpenRSAT Test GPO');
  except
    on E: EGPOException do
      RaisedException := True;
  end;

  Check(RaisedException, 'Create with an existing name should raise EGPOException');

  if (DN <> '') then
    Logic.Delete(DN);
end;

procedure TIntegrationTestGPOCore.FindByName_Existing;
var
  DN: RawUtf8;
  GPO: TGPO;
begin
  DN := Logic.Add(DomainDN, 'OpenRSAT Test GPO');
  if (DN = '') then
    Exit;

  GPO := Logic.FindByName(DomainDN, 'OpenRSAT Test GPO');
  try
    Check(Assigned(GPO), 'FindByName should return the created GPO');
    if Assigned(GPO) then
    begin
      CheckEqual(GPO.DisplayName, 'OpenRSAT Test GPO');
      CheckEqual(GPO.DistinguishedName, DN);
    end;
  finally
    GPO.Free;
  end;

  Logic.Delete(DN);
end;

procedure TIntegrationTestGPOCore.FindByName_Missing;
var
  GPO: TGPO;
begin
  GPO := Logic.FindByName(DomainDN, 'OpenRSAT Missing GPO');

  Check(not Assigned(GPO), 'FindByName should not return a missing GPO');
end;

procedure TIntegrationTestGPOCore.Rename_ValidName;
var
  DN: RawUtf8;
  GPO: TGPO;
begin
  DN := Logic.Add(DomainDN, 'OpenRSAT Test GPO');
  if (DN = '') then
    Exit;

  GPO := Logic.FindByName(DomainDN, 'OpenRSAT Test GPO');
  try
    Check(Logic.Rename(GPO, 'OpenRSAT Test GPO Renamed'), 'Rename should succeed');

    GPO.Free;
    GPO := Logic.FindByName(DomainDN, 'OpenRSAT Test GPO Renamed');
    Check(Assigned(GPO), 'FindByName should return the renamed GPO');
  finally
    GPO.Free;
  end;

  Logic.Delete(DN);
end;

procedure TIntegrationTestGPOCore.Rename_EmptyName;
var
  DN: RawUtf8;
  GPO: TGPO;
  RaisedException: Boolean;
begin
  DN := Logic.Add(DomainDN, 'OpenRSAT Test GPO');
  if (DN = '') then
    Exit;

  GPO := Logic.FindByName(DomainDN, 'OpenRSAT Test GPO');
  try
    RaisedException := False;
    try
      Logic.Rename(GPO, '');
    except
      on E: EGPOException do
        RaisedException := True;
    end;

    Check(RaisedException, 'Rename with an empty name should raise EGPOException');
  finally
    GPO.Free;
  end;

  Logic.Delete(DN);
end;

procedure TIntegrationTestGPOCore.Duplicate_Valid;
var
  DN, DuplicatedDN: RawUtf8;
  GPO, DuplicatedGPO: TGPO;
begin
  DN := Logic.Add(DomainDN, 'OpenRSAT Test GPO');
  if (DN = '') then
    Exit;

  GPO := Logic.FindByName(DomainDN, 'OpenRSAT Test GPO');
  try
    Check(Assigned(GPO), 'FindByName should return the created GPO');
    if not Assigned(GPO) then
      Exit;

    DuplicatedDN := Logic.Duplicate(GPO, 'OpenRSAT Test GPO Duplicate');
    Check(DuplicatedDN <> '', 'Duplicate should return the new distinguished name');

    if (DuplicatedDN <> '') then
    begin
      DuplicatedGPO := Logic.FindByName(DomainDN, 'OpenRSAT Test GPO Duplicate');
      try
        Check(Assigned(DuplicatedGPO), 'FindByName should return the duplicated GPO');
        if Assigned(DuplicatedGPO) then
        begin
          CheckEqual(DuplicatedGPO.Flags, GPO.Flags);
          CheckEqual(DuplicatedGPO.FunctionalityVersion, GPO.FunctionalityVersion);
          CheckNot(DuplicatedGPO.DistinguishedName = GPO.DistinguishedName,
            'Duplicated GPO should have its own distinguished name');
        end;
      finally
        DuplicatedGPO.Free;
      end;
      Logic.Delete(DuplicatedDN);
    end;
  finally
    GPO.Free;
  end;

  Logic.Delete(DN);
end;

procedure TIntegrationTestGPOCore.Duplicate_EmptyName;
var
  DN: RawUtf8;
  GPO: TGPO;
  RaisedException: Boolean;
begin
  DN := Logic.Add(DomainDN, 'OpenRSAT Test GPO');
  if (DN = '') then
    Exit;

  GPO := Logic.FindByName(DomainDN, 'OpenRSAT Test GPO');
  try
    RaisedException := False;
    try
      Logic.Duplicate(GPO, '');
    except
      on E: EGPOException do
        RaisedException := True;
    end;

    Check(RaisedException, 'Duplicate with an empty name should raise EGPOException');
  finally
    GPO.Free;
  end;

  Logic.Delete(DN);
end;

procedure TIntegrationTestGPOCore.UpdateConfiguration_Valid;
var
  DN: RawUtf8;
  GPO: TGPO;
begin
  DN := Logic.Add(DomainDN, 'OpenRSAT Test GPO');
  if (DN = '') then
    Exit;

  GPO := Logic.FindByName(DomainDN, 'OpenRSAT Test GPO');
  try
    Check(Assigned(GPO), 'FindByName should return the created GPO');
    if not Assigned(GPO) then
      Exit;

    Check(Logic.UpdateConfiguration(GPO, GPO_FLAG_MACHINEDISABLED, 2, 3,
      'OpenRSAT Test GPO Description', ''), 'UpdateConfiguration should succeed');

    GPO.Free;
    GPO := Logic.FindByName(DomainDN, 'OpenRSAT Test GPO');
    Check(Assigned(GPO), 'FindByName should return the updated GPO');
    if Assigned(GPO) then
    begin
      CheckEqual(GPO.Flags, GPO_FLAG_MACHINEDISABLED);
      CheckEqual(GPO.UserVersion, 2);
      CheckEqual(GPO.MachineVersion, 3);
      CheckEqual(GPO.Description, 'OpenRSAT Test GPO Description');
    end;
  finally
    GPO.Free;
  end;

  Logic.Delete(DN);
end;

procedure TIntegrationTestGPOCore.UpdateConfiguration_NoChange;
var
  DN: RawUtf8;
  GPO: TGPO;
begin
  DN := Logic.Add(DomainDN, 'OpenRSAT Test GPO');
  if (DN = '') then
    Exit;

  GPO := Logic.FindByName(DomainDN, 'OpenRSAT Test GPO');
  try
    Check(Assigned(GPO), 'FindByName should return the created GPO');
    if Assigned(GPO) then
      Check(Logic.UpdateConfiguration(GPO, GPO.Flags, GPO.UserVersion,
        GPO.MachineVersion, GPO.Description, GPO.WQLFilter),
        'UpdateConfiguration with no change should succeed');
  finally
    GPO.Free;
  end;

  Logic.Delete(DN);
end;

procedure TIntegrationTestGPOCore.Delete_Existing;
var
  DN: RawUtf8;
  GPO: TGPO;
begin
  DN := Logic.Add(DomainDN, 'OpenRSAT Test GPO');
  if (DN = '') then
    Exit;

  Check(Logic.Delete(DN), 'Delete should succeed');

  GPO := Logic.FindByName(DomainDN, 'OpenRSAT Test GPO');
  Check(not Assigned(GPO), 'Deleted GPO should not be found');
end;

end.
