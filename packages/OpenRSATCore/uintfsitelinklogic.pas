unit uintfsitelinklogic;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.base,
  mormot.core.text,
  mormot.net.ldap,
  uproperty,
  ursatldapclient;

type
  { Ldap Result }
  TLdapResultArray = array of TLdapResult;
  
  { ISiteLinkLogic }
  ISiteLinkLogic = interface
    procedure GetAllResources;
    procedure MoveItemToInSite(Index: Integer);
    procedure MoveItemToNotInSite(Index: Integer);
    procedure SetScalarProperty(const Attribute, Value: RawUtf8; Option: TLdapAddOption);
    procedure SyncAttributeProperty(Option: TLdapAddOption);
    procedure SetProperties(P: TProperty);
    procedure SetLdap(L: TRsatLdapClient);
    procedure AddToNotInSite(Item: TLdapResult);
    procedure AddToInSite(Item: TLdapResult);
    function GetProps: TProperty;
    function GetLdap: TRsatLdapClient;
    function GetInSite: TLdapResultArray;
    function GetNotInSite: TLdapResultArray;
    function GetItemAttrValue(List: TLdapResultArray; idx: Integer; attr: RawUtf8): RawUtf8;
    function GetResultName(Obj: TLdapResult): RawUtf8;
    function GetNbSites: Integer;
    function GetValueFromAttribute(Attribute: TLdapAttribute): RawUtf8;
    function FindAttribute(Attribute: RawUtf8): TLdapAttribute;
    function FindAttribute(Attribute: RawUtf8; LdapResult: TLdapResult): TLdapAttribute;
  end;

implementation

end.

