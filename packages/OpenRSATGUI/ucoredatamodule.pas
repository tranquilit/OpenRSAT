unit ucoredatamodule;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Controls,
  Forms,
  mormot.core.base,
  mormot.core.variants,
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
    ileADSubnet,
    ileADGPODisabled,
    ileADGPOEnabled,
    ileADGPOEnforced,
    ileADOUGPOInheritanceDisable,
    ileADContainerGPOInheritanceDisabled
  );

  { TCoreDataModule }

  TCoreDataModule = class(TDataModule)
    ImageList1: TImageList;
    ImageList2: TImageList;
  public
    constructor Create(AOwner: TComponent); override;
  end;

function ObjectClassToImageIndex(ObjectClass: RawUtf8): Integer;
function ObjectClassToImageIndex(ObjectClass: TRawUtf8DynArray): Integer;
function LdapObjectToImageIndex(NodeData: PDocVariantData): Integer;

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

function ObjectClassToImageIndex(ObjectClass: TRawUtf8DynArray): Integer;
var
  idx: Integer;
begin
  result := Ord(ileADUnknown);
  idx := High(ObjectClass);
  if (idx >= 0) then
    result := ObjectClassToImageIndex(ObjectClass[idx]);
end;

function LdapObjectToImageIndex(NodeData: PDocVariantData): Integer;
var
  UAC: TUserAccountControls;
begin
  result := Ord(ileADUnknown);

  if not Assigned(NodeData) or not NodeData^.Exists('objectClass') then
    Exit;
  result := ObjectClassToImageIndex(NodeData^.A_['objectClass']^.ToRawUtf8DynArray);
  if not NodeData^.Exists('userAccountControl') then
    Exit;
  UAC := UserAccountControlsFromText(NodeData^.U['userAccountControl']);

  if (result = Ord(ileADUser)) and (uacAccountDisable in UAC) then
    result := 51;
end;

{$R *.lfm}

{ TCoreDataModule }

constructor TCoreDataModule.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

initialization
  if not Assigned(CoreDataModule) then
    Application.CreateForm(TCoreDataModule, CoreDataModule);

end.

