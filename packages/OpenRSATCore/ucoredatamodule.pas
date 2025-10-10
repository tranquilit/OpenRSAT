unit ucoredatamodule;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils, Controls,
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
  private
    function GetConfigFilePath: String;
    function GetConfigFolderPath: String;
    function GetQueryFolderPath: String;
  public
    constructor Create(AOwner: TComponent); override;

    function objectClassToImageIndex(ObjectClass: String): Integer;
  published
    property ConfigFolderPath: String read GetConfigFolderPath;
    property ConfigFilePath: String read GetConfigFilePath;
    property QueryFolderPath: String read GetQueryFolderPath;
  end;

var
  CoreDataModule: TCoreDataModule;

implementation

uses
  mormot.core.text;

{$R *.lfm}

{ TCoreDataModule }

function TCoreDataModule.GetConfigFilePath: String;
begin
  result := MakePath([ConfigFolderPath, 'config.ini']);
end;

function TCoreDataModule.GetConfigFolderPath: String;
begin
  result := GetAppConfigDir(False);
end;

function TCoreDataModule.GetQueryFolderPath: String;
begin
  result := EnsureDirectoryExists([ConfigFolderPath, 'QueryFolder']);
end;

constructor TCoreDataModule.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

function TCoreDataModule.objectClassToImageIndex(ObjectClass: String): Integer;
begin
  case objectClass of
   'organizationalUnit':    result := Ord(ileADOU);
   'domain', 'domainDNS':   result := Ord(ileADDomain);
   'computer' :             result := Ord(ileADComputer);
   'user', 'inetOrgPerson': result := Ord(ileADUser);
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
   if (result = Ord(ileADUnknown)) and objectClass.Contains('Container') then
     result := Ord(ileADContainer);
end;


end.

