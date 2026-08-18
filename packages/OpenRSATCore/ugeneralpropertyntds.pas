unit ugeneralpropertyntds;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.base,
  mormot.core.text,
  mormot.core.variants,
  mormot.net.ldap,
  uproperty,
  uschedulinglogic,
  ursatldapclient;

type
  { TGeneralPropertyNTDSLogic }
  TGeneralPropertyNTDSLogic = class
  private
    fProperty: TProperty;
    fLdap: TLdapClient;
  public

    function FindAttribute(Attribute: RawUtf8): TLdapAttribute;
    function GetByteFromAttribute(Attribute: TLdapAttribute): RawByteString;

    property Props: TProperty read fProperty write fProperty;
    property Ldap: TLdapClient read fLdap write fLdap;
  end;

implementation

function TGeneralPropertyNTDSLogic.FindAttribute(Attribute: RawUtf8): TLdapAttribute;
begin
  Result := fProperty.Get(Attribute);
end;

function TGeneralPropertyNTDSLogic.GetByteFromAttribute(Attribute: TLdapAttribute): RawByteString;
begin
  if Attribute <> nil then
    Result := Attribute.GetRaw()
  else
    Result := '';
end;

end.

