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
  { ISchedulingLogic }
  ISchedulingLogic = interface
    procedure SetupHoursRawByteString;
    procedure LoadScheduleToHours(const ScheduleData: RawByteString);
    function SaveSchedule: RawByteString;
    function GetScheduleHeader: RawByteString;
  end;

  { TSchedulingLogic }
  TSchedulingLogic = class abstract(TInterfacedObject, ISchedulingLogic)
  private
    fScheduleHeader, fHours: RawByteString;
  public
    destructor Destroy; override;

    procedure SetupHoursRawByteString; virtual;
    procedure LoadScheduleToHours(const ScheduleData: RawByteString); virtual;
    function SaveSchedule: RawByteString; virtual;
    function GetScheduleHeader: RawByteString; virtual;

    property Hours: RawByteString read fHours write fHours;
  end;

const
  ScheduleHeader = #$BC#0#0#0#0#0#0#0#1#0#0#0#0#0#0#0#$14#0#0#0;

implementation

destructor TSchedulingLogic.Destroy;
begin
  inherited Destroy;
end;

procedure TSchedulingLogic.SetupHoursRawByteString;
begin
  fHours := '';
  SetLength(fHours, 21);
  FillByte(fHours[1], 21, 0);
end;

procedure TSchedulingLogic.LoadScheduleToHours(const ScheduleData: RawByteString);
var
  Data: RawByteString;
  i, ADDay, ADHour, GridDay, GridIndex: Integer;
begin
  FillByte(fHours[1], Length(fHours), 0);
  if ScheduleData = '' then
  begin
    FillByte(fHours[1], Length(fHours), $0F);
    Exit;
  end;

  Data := ScheduleData;
  Delete(Data, 1, 20);
  for i := 0 to 167 do
  begin
    ADDay  := i div 24;
    ADHour := i mod 24;

    GridDay := (ADDay + 6) mod 7;
    GridIndex := GridDay * 24 + ADHour;
    if Byte(Data[i + 1]) = $FF then
      fHours[1 + GridIndex div 8] := Char(Byte(fHours[1 + GridIndex div 8]) or (1 shl (GridIndex mod 8)));
  end;
end;

function TSchedulingLogic.SaveSchedule: RawByteString;
var
  i, ADDay, ADHour, GridDay, GridIndex: Integer;
begin
  SetLength(Result, 168);

  for i := 0 to 167 do
  begin
    ADDay  := i div 24;
    ADHour := i mod 24;

    GridDay := (ADDay + 6) mod 7;
    GridIndex := GridDay * 24 + ADHour;
    if (Byte(fHours[1 + GridIndex div 8]) and (1 shl (GridIndex mod 8))) <> 0 then
      Result[i + 1] := Char($FF)
    else
      Result[i + 1] := Char($00);
  end;
end;

function TSchedulingLogic.GetScheduleHeader: RawByteString;
begin
  Result := ScheduleHeader;
end;

end.

