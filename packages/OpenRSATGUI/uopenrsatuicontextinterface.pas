unit uopenrsatuicontextinterface;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  IniPropStorage,
  mormot.core.base,
  mormot.core.text,
  mormot.net.ldap,
  ursat;

type
  { IOpenRSATUIContext }

  IOpenRSATUIContext = Interface
    function GetIniPropStorage: TIniPropStorage;
    function GetComponentOwner: TComponent;
    function GetRSAT: TRSAT;
    procedure OpenProperty(DistinguishedName: RawUtf8);
    procedure CloseProperty(DistinguishedName: RawUtf8);
    procedure ChangeDomainController(DomainController: RawUtf8);

    property IniPropStorage: TIniPropStorage read GetIniPropStorage;
    property RSAT: TRSAT read GetRSAT;
    property ComponentOwner: TComponent read GetComponentOwner;
  end;

implementation

end.

