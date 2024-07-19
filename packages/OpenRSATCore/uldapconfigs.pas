unit uldapconfigs;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.net.ldap,
  mormot.core.base;

type

  TMLdapClientSettings = class(TLdapClientSettings)
  private
    fUseCredentials: Boolean;
  public
    property Password: SpiUtf8 read fPassword write fPassword;
  published
    property UseCredentials: Boolean read fUseCredentials write fUseCredentials default True;
  end;

  { TConnectionSettings }

  TConnectionSettings = class(TMLdapClientSettings)
  protected
    // Search settings
  published
    property Timeout: Integer read fTimeout write fTimeout default 5000;
  end;

  { TLdapConfigs }

  TLdapConfigs = class
  private
    fAutoConnect: Boolean;
    fLastConfig: String;
    fLdapConnectionSettings: TConnectionSettings;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadConfig(AConfigName: String = '');
    procedure LoadConfig(const AConfigName: String; out aConfig: TMLdapClientSettings);
    procedure SaveConfig(AConfigName: String = '');
    procedure SaveConfig(const AConfigName: String; const aConfig: TMLdapClientSettings);
    function IsConfigValid: Boolean;
    property LdapConnectionSettings: TConnectionSettings read fLdapConnectionSettings;
  published
    property LastConfig: String read fLastConfig write fLastConfig;
    property AutoConnect: Boolean read fAutoConnect write fAutoConnect;
  end;

implementation

uses
  Controls,
  tisinifiles,
  mormot.core.data,
  mormot.core.os,
  mormot.core.variants,
  uvisconnectconfigs,
  ucoredatamodule;

{ TLdapConfigs }

constructor TLdapConfigs.Create;
begin
  fLdapConnectionSettings := TConnectionSettings.Create;
  fAutoConnect := True;
  fLastConfig := '';
end;

destructor TLdapConfigs.Destroy;
begin
  FreeAndNil(fLdapConnectionSettings);

  inherited Destroy;
end;

procedure TLdapConfigs.LoadConfig(AConfigName: String);
begin
  IniToObject(StringFromFile(CoreDataModule.ConfigFilePath), Self, 'global');

  if AConfigName <> '' then
    LastConfig := AConfigName;
  fLdapConnectionSettings.Tls := False;
  fLdapConnectionSettings.AutoBind := lcbKerberos;
  IniToObject(StringFromFile(CoreDataModule.ConfigFilePath), fLdapConnectionSettings, LastConfig);
end;

procedure TLdapConfigs.LoadConfig(const AConfigName: String; out
  aConfig: TMLdapClientSettings);
begin
  if (AConfigName = '') then
    LastConfig := AConfigName;

  aConfig := TMLdapClientSettings.Create;
  aConfig.Tls := False;
  aConfig.AutoBind := lcbKerberos;
  IniToObject(StringFromFile(CoreDataModule.ConfigFilePath), aConfig, LastConfig);
end;

procedure TLdapConfigs.SaveConfig(AConfigName: String);
var
  ini: TTisInifiles;
  aSettings: TDocVariantData;
  aField: TDocVariantFields;
begin
  if (AConfigName <> '') then
    LastConfig := AConfigName;

  ObjectToVariant(Self, Variant(aSettings), []);
  aSettings.Delete('Password');
  ini := TTisInifiles.Create(CoreDataModule.ConfigFilePath);
  try
    for aField in aSettings.Fields do
      ini.writeString('global', aField.Name^, VariantToString(aField.Value^));
    SaveConfig(LastConfig, fLdapConnectionSettings);
  finally
    FreeAndNil(ini);
  end;
end;

procedure TLdapConfigs.SaveConfig(const AConfigName: String;
  const aConfig: TMLdapClientSettings);
var
  ini: TTisInifiles;
  aSettings: TDocVariantData;
  aField: TDocVariantFields;
begin
  ini := TTisInifiles.Create(CoreDataModule.ConfigFilePath);
  try
    ObjectToVariant(aConfig, Variant(aSettings), []);
    aSettings.Delete('Password');
    for aField in aSettings.Fields do
      ini.WriteString(AConfigName, aField.Name^, VariantToString(aField.Value^));
  finally
    FreeAndNil(ini);
  end;
end;

function TLdapConfigs.IsConfigValid: Boolean;
var
  pwd: TFormConnectConfigs;
begin
  Result := False;
  if (fLdapConnectionSettings.TargetHost = '') and fLdapConnectionSettings.Tls then
    Exit;
  if (fLdapConnectionSettings.TargetPort = '') then
    Exit;
  if fLdapConnectionSettings.UseCredentials then
  begin
    if (fLdapConnectionSettings.UserName = '') then
      Exit;
    if (fLdapConnectionSettings.Password = '') then
    begin
      pwd := TFormConnectConfigs.Create(CoreDataModule, Self);
      try
        result := pwd.ShowModal = mrOK;
      finally
        FreeAndNil(pwd);
      end;
      Exit;
    end;
  end;
  result := True;
end;

end.

