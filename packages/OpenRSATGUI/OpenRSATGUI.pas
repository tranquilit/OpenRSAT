{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit OpenRSATGUI;

{$warn 5023 off : no warning about unused units}
interface

uses
  ufrmmoduleaduc, ucoredatamodule, uvisconnectconfigs, ufrmmoduledns,
  utreeselectionhistory, uvisproperties, uOmniselect, uvisadvancedsecurity,
  uvisattributeeditor, uvislistother, uvislogonhours, uvislogonworkstation,
  uvisaddaces, uvissearch, ufrmnewcomputer, ufrmnewcontact, ufrmnewgroup,
  ufrmnewou, ufrmnewsharedfolder, ufrmnewuser, uvisnewobject, uvischangedn,
  uvisdelegatecontrol, uVisOptions, uvistaskmove, uvistaskresetpassword,
  utranslation, ufrmrsatoptions, uvisselectnewrecordtype,
  uvisnewresourcerecord, uvisnewzonewizard, ufrmmodulesitesandservices,
  ufrmmoduleserviceinterfaces, ufrmnewobject, ufrmnewsite, ufrmnewsubnet,
  uaductreeview, ufrmmoduleadssoptions, uvispropertieslist,
  uvischangedomaincontroller, uvisoperationmasters, upropertyframe,
  ufrmpropertyobject, ufrmpropertymanagedby, ufrmpropertyaddress,
  ufrmpropertymemberof, ufrmpropertygeneraldefault, ufrmpropertygeneraluser,
  ufrmpropertygeneralgroup, ufrmpropertygeneralcomputer,
  ufrmpropertygeneralou, ufrmpropertygeneralsubnet, ufrmpropertygeneralsite,
  ufrmpropertygeneralvolume, ufrmpropertyoperatingsystem, ufrmpropertyaccount,
  ufrmpropertyprofile, ufrmpropertybitlocker, ufrmpropertylaps,
  ufrmpropertytelephone, ufrmpropertylocation, ufrmpropertyorganization,
  ufrmpropertypublishedcertificates, ufrmpropertymember, ufrmpropertysecurity,
  ufrmpropertyattributes, uhelpersui, ucommonui, ursatldapclientui,
  ufrmoption, ufrmmodule, ufrmmoduleaducoption, ufrmrsat, ufrmmodules,
  uvismodifygplink, uvisprofilemanager, uviseditaduccolumns,
  uvisprofileconfiguration, uvisrootdseinfos, uvisshowrelationship, utheme,
  ufrmpropertyntauthcertificates, ufrmpropertyunixattributes,
  uvisselectobjectsid, ufrmnewinetorgperson,
  ufrmnewmsdsshadowprincipalcontainer, ufrmnewmsimagingpsps,
  ufrmnewmsdskeycredential, ufrmnewmsdsresourcepropertylist, ufrmnewprinter,
  uvisselectobjectguid, ufrmnewsitelink, ufrmnewserver, ufrmnewsitelinkbridge,
  ufrmpropertygeneralsitelink, ufrmpropertygeneralsitelinkbridge,
  ufrmpropertygeneralntdssitesettings, ufrmpropertygeneralserver,
  ufrmpropertygeneralntdsdsa, ufrmpropertyconnections,
  ufrmnewmsdnsserversettings, ufrmpropertygeneralintersitetransport,
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('OpenRSATGUI', @Register);
end.
