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
    fScheduling: TSchedulingLogic;
  public
    constructor Create(P: TProperty);
    destructor Destroy; override;

    procedure SaveSchedule;
    function FindAttribute(Attribute: RawUtf8): TLdapAttribute;
    function GetByteFromAttribute(Attribute: TLdapAttribute): RawByteString;

    property Props: TProperty read fProperty write fProperty;
    property Ldap: TLdapClient read fLdap write fLdap;
    property Scheduling: TSchedulingLogic read fScheduling write fScheduling;
  end;

implementation

constructor TGeneralPropertyNTDSLogic.Create(P: TProperty);
begin
  fProperty := P;
  fLdap := P.LdapClient;

  fScheduling := TSchedulingLogic.Create(NTDSSchedulingPage);
  fScheduling.SetupHoursRawByteString;
end;

destructor TGeneralPropertyNTDSLogic.Destroy;
begin
  Inherited Destroy;
  fScheduling.Free;
end;

procedure TGeneralPropertyNTDSLogic.SaveSchedule;
var
  Schedule: RawByteString;
begin
  Schedule := fScheduling.SaveSchedule();
  Props.Add('schedule', fScheduling.GetScheduleHeader + Schedule);
end;

function TGeneralPropertyNTDSLogic.FindAttribute(Attribute: RawUtf8): TLdapAttribute;
begin
  Result := fProperty.Attributes.Find(Attribute);
end;

function TGeneralPropertyNTDSLogic.GetByteFromAttribute(Attribute: TLdapAttribute): RawByteString;
begin
  if Attribute <> nil then
    Result := Attribute.GetRaw()
  else
    Result := '';
end;

end.

