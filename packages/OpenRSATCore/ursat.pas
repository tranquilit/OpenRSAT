unit ursat;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  ursatldapclient,
  uldapconfigs,
  ursatoption,
  uoption;

type

  { TRSAT }

  TRSAT = class
  private
    fLdapConnection: ILdapConnection;
    fLdapClient: TRsatLdapClient;
    fLdapConfig: TLdapConfigs;
    fRSATOption: TRSATOption;

  public
    constructor Create(AOnOptionChange: TProcRsatOptionOfObject);
    destructor Destroy; override;
    procedure Load;
  published
    property LdapClient: TRsatLdapClient read fLdapClient;
    property LdapConfigs: TLdapConfigs read fLdapConfig;
    property RsatOption: TRsatOption read fRsatOption;
    property LdapConnection: ILdapConnection read fLdapConnection;
  end;

implementation

{ TRSAT }

constructor TRSAT.Create(AOnOptionChange: TProcRsatOptionOfObject);
begin
  fLdapConnection := TMormotLdapConnection.Create;
  fLdapClient := TRsatLdapClient.Create;
  fLdapConfig := TLdapConfigs.Create;
  fRSATOption := TRsatOption.Create;
  fRSATOption.RegisterObserver(AOnOptionChange);
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

