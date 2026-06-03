unit integrationtest.udoublelistlogic;

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
  fixture.fakevisadvancedsecurity,
  ursatldapclient,
  udoublelistlogic;

type

  { TIntegrationTestDoubleListLogic }

  TIntegrationTestDoubleListLogic = class(TSynTestCase)
  private
    Logic: TDoubleListLogic;
    LdapClient: TLdapClient;
    BaseDN: RawUtf8;
    DN: RawUtf8;
  public
    procedure Setup; override;
    procedure CleanUp; override;
    procedure MethodSetup; override;
    procedure MethodCleanUp; override;
  end;
  
implementation

procedure TIntegrationTestDoubleListLogic.Setup;
begin
  LdapClient := SetupLdapClient;
  BaseDN := GetBaseDN(LdapClient.DefaultDN());
end;

procedure TIntegrationTestDoubleListLogic.CleanUp;
begin
  if Assigned(LdapClient) then
    FreeAndNil(LdapClient);
end;

procedure TIntegrationTestDoubleListLogic.MethodSetup;
begin
  if not SetupTestUser(LdapClient, ClassName, DN) then
    raise Exception.Create('Failed to setup test user.');

end;

procedure TIntegrationTestDoubleListLogic.MethodCleanUp;
begin
  if Assigned(LdapClient) then
    LdapClient.Delete(DN);
end;

end.

