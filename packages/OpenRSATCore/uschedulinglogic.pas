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
    fOpt1, fOpt2, fOpt3, fOpt4: Byte;
  public
    constructor Create(Page: KindOfPage);
    destructor Destroy; override;

    procedure SetupHoursRawByteString;
    procedure LoadScheduleToHours(const ScheduleData: RawByteString);
    function SaveSchedule: RawByteString;
    function GetScheduleHeader: RawByteString;

    property Hours: RawByteString read fHours write fHours;
  end;

const
  ScheduleHeader = #$BC#0#0#0#0#0#0#0#1#0#0#0#0#0#0#0#$14#0#0#0;

implementation

constructor TSchedulingLogic.Create(Page: KindOfPage);
begin
  fOpt1 := $00;
  fOpt2 := $00;
  fOpt3 := $00;
  fOpt4 := $00;

  fKindOfPage := Page;
  case fKindOfPage of
    SiteLinkSchedulingPage:
    begin
      fOpt1 := $F0;
      fOpt2 := $FF;
    end;
    NTDSSchedulingPage:
    begin
      fOpt1 := $00;
      fOpt2 := $01;
      fOpt3 := $05;
      fOpt4 := $0F;
    end;
  end;
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
    FillByte(fHours[1], Length(fHours), fOpt1);
    Exit;
  end;

  Data := ScheduleData;
  Delete(Data, 1, 20);
  if fKindOfPage = NTDSSchedulingPage then
  begin
    fHours := Data;
    Exit;
  end;

  for i := 0 to 167 do
  begin
    ADDay  := i div 24;
    ADHour := i mod 24;

    GridDay := (ADDay + 6) mod 7;
    GridIndex := GridDay * 24 + ADHour;
    if Byte(Data[i + 1]) = fOpt2 then
      fHours[1 + GridIndex div 8] := Char(Byte(fHours[1 + GridIndex div 8]) or (1 shl (GridIndex mod 8)));
  end;
end;

function TSchedulingLogic.SaveSchedule: RawByteString;
var
  i, ADDay, ADHour, GridDay, GridIndex: Integer;
begin
  if fKindOfPage = NTDSSchedulingPage then
  begin
    Result := fHours;
    Exit;
  end;

  SetLength(Result, 168);
  for i := 0 to 167 do
  begin
    ADDay  := i div 24;
    ADHour := i mod 24;

    GridDay := (ADDay + 6) mod 7;
    GridIndex := GridDay * 24 + ADHour;
    if (Byte(fHours[1 + GridIndex div 8]) and (1 shl (GridIndex mod 8))) <> 0 then
      Result[i + 1] := Char(fOpt2)
    else
      Result[i + 1] := Char(fOpt1);
  end;
end;

function TSchedulingLogic.GetScheduleHeader: RawByteString;
begin
  Result := ScheduleHeader;
end;

end.

