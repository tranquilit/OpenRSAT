unit uinterfacecore;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  uldapconfigs,
  ursatldapclient,
  ursatmodules,
  ufrmrsatoptions;

type

  { ICore }

  ICore = Interface
    function GetActive: Boolean; virtual;
    function GetLdapClient: TRsatLdapClient; virtual;
    function GetLdapConfigs: TLdapConfigs; virtual;
    function GetModules: TRsatModules; virtual;
    function GetRsatOptions: TRsatOptions; virtual;

    procedure Activate; virtual;
    procedure CloseProperty(VisProperty: TForm); virtual;
    procedure Deactivate; virtual;
    procedure OpenProperty(Name, DistinguishedName: String); virtual;
    procedure Load; virtual;

    property Active: Boolean read GetActive;
    property LdapClient: TRsatLdapClient read GetLdapClient;
    property LdapConfigs: TLdapConfigs read GetLdapConfigs;
    property Modules: TRsatModules read GetModules;
    property RsatOptions: TRsatOptions read GetRsatOptions;
  end;

implementation

end.

