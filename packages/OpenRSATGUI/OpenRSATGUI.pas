{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit OpenRSATGUI;

{$warn 5023 off : no warning about unused units}
interface

uses
  ursatldapclient, ursatmodules, ufrmcore, ufrmmoduleaduc, ucoredatamodule, 
  uldapconfigs, uvisconnectconfigs, uvisconnectoptions, ufrmmoduledns, 
  uinterfacemodule, uinterfacecore, utreeselectionhistory, uvisproperties, 
  uOmniselect, uvisadvancedsecurity, uvisattributeeditor, uvislistother, 
  uvislogonhours, uvislogonworkstation, uvisaddaces, ucommon, uvissearch, 
  ufrmnewcomputer, ufrmnewcontact, ufrmnewgroup, ufrmnewou, 
  ufrmnewsharedfolder, ufrmnewuser, uvisnewobject, uvischangedn, 
  uvischangepartition, uvisdelegatecontrol, uVisOptions, uvistaskmove, 
  uvistaskresetpassword, utranslation, usidcache, ufrmmoduleaducoptions, 
  ufrmrsatoptions, udns, uvisselectnewrecordtype, uvisnewresourcerecord, 
  uvisnewzonewizard, ufrmmodulesitesandservices, ufrmmoduleserviceinterfaces, 
  ufrmnewobject, ufrmnewsite, ufrmnewsubnet, uresourcestring, uaductreeview, 
  ufrmmoduleadssoptions, uvisconnectiondetails, uvispropertieslist, uproperty, 
  uvischangedomaincontroller, uvisoperationmasters, upropertyframe, 
  ufrmpropertyobject, ufrmpropertymanagedby, ufrmpropertyaddress, 
  ufrmpropertymemberof, ufrmpropertygeneraldefault, ufrmpropertygeneraluser, 
  ufrmpropertygeneralgroup, ufrmpropertygeneralcomputer, 
  ufrmpropertygeneralou, ufrmpropertygeneralsubnet, ufrmpropertygeneralsite, 
  ufrmpropertygeneralvolume, ufrmpropertyoperatingsystem, ufrmpropertyaccount, 
  ufrmpropertyprofile, ufrmpropertybitlocker, ufrmpropertylaps, 
  ufrmpropertytelephone, ufrmpropertylocation, ufrmpropertyorganization, 
  ufrmpropertypublishedcertificates, ufrmpropertymember, ufrmpropertysecurity, 
  ufrmpropertyattributes, uhelpersui, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('OpenRSATGUI', @Register);
end.
