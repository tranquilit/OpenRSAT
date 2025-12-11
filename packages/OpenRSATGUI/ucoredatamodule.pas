unit ucoredatamodule;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Controls,
  mormot.core.base,
  mormot.net.ldap;

type

  TImageListEnum = (
    ileAppIcon,
    ileToolPrevious,
    ileToolNext,
    ileToolParent,
    ileToolProperty,
    ileToolSearch,
    ileToolFilter,
    ileToolNewUser,
    ileToolNewGroup,
    ileToolNewOU,
    ileToolCopy,
    ileToolCut,
    ileToolPaste,
    ileToolDelete,
    ileToolAddToGroup,
    ileADUser,
    ileADGroup,
    ileADComputer,
    ileADUnknown,
    ileADDomain,
    ileADOU,
    ileADContainer,
    ileAD_,
    ileADSharedFolder,
    ileADPrinter,
    ileAD__,
    ileAD___n,
    ileADGroupPolicyContainer,
    ileValid,
    ileAdd,
    ileSettings,
    ileRefresh,
    ileConnected,
    ileNotConnected,
    ileSearch,
    ileADSiteLink,
    ileADSite,
    ileADSiteTool,
    ileADSiteSettings,
    ileADServeur,
    ileADSubnet
  );

  { TCoreDataModule }

  TCoreDataModule = class(TDataModule)
    ImageList1: TImageList;
    ImageList2: TImageList;
  public
    constructor Create(AOwner: TComponent); override;
  end;

function ObjectClassToImageIndex(ObjectClass: RawUtf8): Integer;

var
  CoreDataModule: TCoreDataModule;

implementation

uses
  mormot.core.text;

function ObjectClassToImageIndex(ObjectClass: RawUtf8): Integer;
begin
  case objectClass of
   'organizationalUnit':    result := Ord(ileADOU);
   'domain', 'domainDNS':   result := Ord(ileADDomain);
   'computer' :             result := Ord(ileADComputer);
   'user', 'inetOrgPerson', 'contact': result := Ord(ileADUser);
   'group':                 result := Ord(ileADGroup);
   'container', 'builtinDomain', 'lostAndFound', 'interSiteTransport':             result := Ord(ileADContainer);
   'groupPolicyContainer': result := Ord(ileADGroupPolicyContainer);
   'site': result := Ord(ileADSite);
   'siteLink', 'siteLinkBridge': result := Ord(ileADSiteLink);
   'server': result := Ord(ileADServeur);
   'nTDSSiteSettings', 'nTDSDSA': result := Ord(ileADSiteSettings);
   'subnet': result := Ord(ileADSubnet);
   else
     result := Ord(ileADUnknown);
   end;
   if (result = Ord(ileADUnknown)) and String(objectClass).Contains('Container') then
     result := Ord(ileADContainer);
end;

{$R *.lfm}

{ TCoreDataModule }

constructor TCoreDataModule.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

end.

