unit integrationtest.ugeneralpropertysitelink;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.base,
  mormot.core.test,
  mormot.core.text,
  mormot.core.variants,
  mormot.net.ldap,
  fixture.ldapclient,
  ursatldapclient,
  uproperty,
  udoublelistlogic,
  ugeneralpropertysitelink;

type

  { TIntegrationTestDoubleListLogic }
  TIntegrationTestGeneralPropertySiteLink = class(TSynTestCase)
  private
    PropertyObj: TProperty;
    Logic: TGeneralPropertySiteLink;
    LdapClient: TLdapClient;
    BaseDN: RawUtf8;
    DN: RawUtf8;
  public
    procedure Setup; override;
    procedure CleanUp; override;
    procedure MethodSetup; override;
    procedure MethodCleanUp; override;
  published
    procedure GetAllResources_Valid;
    procedure MoveItem_OutToIn;
    procedure MoveItem_InToOut;
    procedure SyncAttributeProperty_Empty;
    procedure SyncAttributeProperty_SingleSite;
    procedure SyncAttributeProperty_MultipleSites;
  end;

implementation

procedure TIntegrationTestGeneralPropertySiteLink.Setup;
begin
  LdapClient := SetupLdapClient;
  BaseDN := GetBaseDN(LdapClient.DefaultDN());
end;

procedure TIntegrationTestGeneralPropertySiteLink.CleanUp;
begin
  if Assigned(LdapClient) then
    FreeAndNil(LdapClient);
end;

procedure TIntegrationTestGeneralPropertySiteLink.MethodSetup;
begin
  if not SetupTestUser(LdapClient, ClassName, DN) then
    raise Exception.Create('Failed to setup test user.');

  PropertyObj := TProperty.Create;
  PropertyObj.Attributes := TLdapAttributeList.Create;
  PropertyObj.distinguishedName := DN;

  Logic := TGeneralPropertySiteLink.Create(PropertyObj);
  Logic.Ldap := LdapClient;
end;

procedure TIntegrationTestGeneralPropertySiteLink.MethodCleanUp;
begin
  if Assigned(LdapClient) then
    LdapClient.Delete(DN);
end;

procedure TIntegrationTestGeneralPropertySiteLink.GetAllResources_Valid;
begin
  Logic.GetAllResources;

  Check(Length(Logic.OutResult) > 0, 'At least one AD site should be returned');
  Check(Logic.GetNbElementsInLists = Length(Logic.OutResult), 'All entries are initially in OutResult');
end;

procedure TIntegrationTestGeneralPropertySiteLink.MoveItem_OutToIn;
var
  InitialCount: Integer;
begin
  Logic.GetAllResources;

  InitialCount := Length(Logic.OutResult);
  Logic.MoveItem(msInResult, 0);

  Check(Length(Logic.OutResult) = InitialCount - 1, 'One item removed');
  Check(Length(Logic.InResult) = 1, 'One item added');
end;

procedure TIntegrationTestGeneralPropertySiteLink.MoveItem_InToOut;
var
  InitialCount: Integer;
begin
  Logic.GetAllResources;

  InitialCount := Length(Logic.OutResult);
  Logic.MoveItem(msInResult, 0);
  Logic.MoveItem(msOutOfResult, 0);

  Check(Length(Logic.OutResult) = InitialCount, 'Item restored');
  Check(Length(Logic.InResult) = 0, 'InResult empty');
end;

procedure TIntegrationTestGeneralPropertySiteLink.SyncAttributeProperty_Empty;
begin
  Logic.SyncAttributeProperty;

  Check(PropertyObj.GetReadable('siteList') = '', 'Empty siteList expected');
end;

procedure TIntegrationTestGeneralPropertySiteLink.SyncAttributeProperty_SingleSite;
var
  DNValue: RawUtf8;
begin
  Logic.GetAllResources;

  Logic.MoveItem(msInResult, 0);

  DNValue := Logic.InResult[0].Find('distinguishedName').GetReadable();
  Logic.SyncAttributeProperty;

  Check(PropertyObj.GetReadable('siteList') = DNValue, 'Selected site stored');
end;

procedure TIntegrationTestGeneralPropertySiteLink.SyncAttributeProperty_MultipleSites;
var
  Values: TRawUtf8DynArray;
begin
  Logic.GetAllResources;

  if Length(Logic.OutResult) < 2 then
    Exit;

  Logic.MoveItem(msInResult, 0);
  Logic.MoveItem(msInResult, 0);

  Logic.SyncAttributeProperty;

  Values := PropertyObj.GetAllReadable('siteList');
  Check(Length(Values) = 2, 'Two sites stored');
end;

end.

