unit ursat;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  ursatldapclient,
  uldapconfigs,
  ursatoption;

type

  { TRSAT }

  TRSAT = class
  private
    fLdapClient: TRsatLdapClient;
    fLdapConfig: TLdapConfigs;
    fRSATOption: TRSATOption;

    //procedure CloseProperty(VisProperty: TForm); virtual;
    //function OpenProperty(DistinguishedName: RawUtf8; Name: RawUtf8 = ''): TForm; virtual;
    //procedure Load; virtual;
    //procedure ChangeDomainController(DomainController: RawUtf8); virtual;

  public
    constructor Create;
    destructor Destroy; override;
    procedure Load;
  published
    property LdapClient: TRsatLdapClient read fLdapClient;
    property LdapConfigs: TLdapConfigs read fLdapConfig;
    property RsatOption: TRsatOption read fRsatOption;
  end;

implementation

{ TRSAT }

constructor TRSAT.Create;
begin
  fLdapClient := TRsatLdapClient.Create;
  fLdapConfig := TLdapConfigs.Create;
  fRSATOption := TRsatOption.Create;
end;

destructor TRSAT.Destroy;
begin
  FreeAndNil(fLdapClient);
  FreeAndNil(fLdapConfig);
  FreeAndNil(fRSATOption);

  inherited Destroy;
end;

procedure TRSAT.Load;
begin
  LdapConfigs.LoadConfig();
  RsatOption.Load;
end;

end.

