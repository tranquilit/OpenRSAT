unit uschedulinglogic;

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

  { KindOfPage }
  KindOfPage = (LogonHoursPage, SiteLinkSchedulingPage, NTDSSchedulingPage);

  { TSchedulingLogic }
  TSchedulingLogic = class
  private
    fHours: RawByteString;
    fKindOfPage: KindOfPage;
  public
    constructor Create(Page: KindOfPage);
    destructor Destroy; override;

    procedure SetupHoursRawByteString;
    procedure LoadScheduleToHours(const ScheduleData: RawByteString);
    function SaveSchedule: RawByteString;

    property Hours: RawByteString read fHours write fHours;
  end;

implementation

constructor TSchedulingLogic.Create(Page: KindOfPage);
begin
  fKindOfPage := Page;
end;

destructor TSchedulingLogic.Destroy;
begin
  inherited Destroy;
end;

procedure TSchedulingLogic.SetupHoursRawByteString;
begin
  fHours := '';
  case fKindOfPage of
    NTDSSchedulingPage:
    begin
      SetLength(fHours, 168);
      FillByte(fHours[1], 168, 0);
    end
  else
    begin
      SetLength(fHours, 21);
      FillByte(fHours[1], 21, 0);
    end;
  end;
end;

procedure TSchedulingLogic.LoadScheduleToHours(const ScheduleData: RawByteString);
var
  Data: RawByteString;
  i, ADDay, ADHour, GridDay, GridIndex: Integer;
begin
  FillByte(fHours[1], Length(fHours), 0);
  if ScheduleData = '' then
  begin
    FillByte(fHours[1], Length(fHours), $00);
    Exit;
  end;

  Data := ScheduleData;
  Delete(Data, 1, 20);
  fHours := Data;
  Exit;
end;

function TSchedulingLogic.SaveSchedule: RawByteString;
begin
  Result := fHours;
end;

end.

