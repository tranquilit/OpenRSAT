unit umoduleadsi;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.base,
  mormot.core.variants,
  mormot.net.ldap,
  umodule,
  umoduleadsioption,
  ursatldapclient,
  uoption,
  ursat;

type

  { TModuleADSI }

  TModuleADSI = class(TModule)
  private
    function GetADSIOption: TModuleADSIOption;
    function GetLDAP: ILdapConnection;
  public
    constructor Create(ARSAT: TRSAT);
    destructor Destroy; override;

    function GetNamingContexts(PNamingContexts: PDocVariantData): Boolean;

    property ADSIOption: TModuleADSIOption read GetADSIOption;
    property LDAP: ILdapConnection read GetLDAP;
    /// TModule
  protected
    procedure SetEnabled(AValue: Boolean); override;
  end;

implementation
uses
  ucommon;

{ TModuleADSI }

function TModuleADSI.GetADSIOption: TModuleADSIOption;
begin
  result := (fOption as TModuleADSIOption);
end;

function TModuleADSI.GetLDAP: ILdapConnection;
begin
  result := RSAT.LdapConnection;
end;

constructor TModuleADSI.Create(ARSAT: TRSAT);
begin
  inherited Create('ServicesAndInterfaces', rsModuleADSIDisplayName);

  fRSAT := ARSAT;
  fEnabled := True;
  fOption := TModuleADSIOption.Create;
end;

destructor TModuleADSI.Destroy;
begin
  FreeAndNil(fOption);

  inherited Destroy;
end;

function TModuleADSI.GetNamingContexts(PNamingContexts: PDocVariantData
  ): Boolean;
var
  SR: TLdapSearchRequest;
  res: TLdapSearchResult;
  NamingContext: RawUtf8;
  i: Integer;
begin
  result := False;

  SR.Options := DefaultSearchRequestOptions;
  for NamingContext in LDAP.Context.NamingContexts do
  begin
    SearchRequest(SR, NamingContext, '', ['*'], lssBaseObject);
    res := LDAP.Search(SR);
    if not res.OperationResult.Success then
      Exit;
    if res.ReturnedCount <> 1 then
      Exit;
    for i := 0 to res.Entries[0].AttributeCount - 1 do
      PNamingContexts^.O_[NamingContext]^.A_[res.Entries[0].Attributes[i].Name]^.InitArrayFrom(res.Entries[0].Attributes[i].Values, JSON_FAST);
  end;
end;

procedure TModuleADSI.SetEnabled(AValue: Boolean);
begin
  if AValue = fEnabled then
    Exit;
  fEnabled := AValue;
end;

end.

