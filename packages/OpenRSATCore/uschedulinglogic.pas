unit uschedulinglogic;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  DateUtils,
  mormot.core.base,
  mormot.core.text,
  mormot.net.ldap,
  uproperty,
  ursatldapclient;

type

  TSchedule = record
    Slots: TByteDynArray;
    HeaderSize: Integer;
    BodySize: Integer;
  end;

  TScheduleAttributKind = (
    sakLogonHour,
    sakSchedule
  );

  { TSchedulePresenter }
  TSchedulePresenter = class
  private
    fDayOffset: Integer;
    fSchedule: TSchedule;
    fScheduleAttributKind: TScheduleAttributKind;
    fUTCOffset: Integer;

    function GetRawValue: RawByteString;
    procedure SetRawValue(AValue: RawByteString);
  public
    constructor Create(AScheduleAttributKind: TScheduleAttributKind);
    destructor Destroy; override;

    procedure SetLocalValue(ADay, AHour, AValue: Integer);
    function GetLocalValue(ADay, AHour: Integer): Integer;

    procedure SetLocalValues(ADayFrom, ADayTo, AHourFrom, AHourTo, AValue: Integer);

    property RawValue: RawByteString read GetRawValue write SetRawValue;
    property UTCOffset: Integer read fUTCOffset write fUTCOffset;
    property DayOffset: Integer read fDayOffset write fDayOffset;
  end;

function LogonHoursSlotIndex(ADay, AHour: Integer): Integer;
function ScheduleSlotIndex(ADay, AHour: Integer): Integer;

procedure LogonHoursDecode(const AValue: RawByteString; out ASchedule: TSchedule);
function LogonHoursEncode(const ASchedule: TSchedule): RawByteString;
procedure ADScheduleDecode(const AValue: RawByteString; out ASchedule: TSchedule);
function ADScheduleEncode(const ASchedule: TSchedule): RawByteString;

implementation

function LogonHoursSlotIndex(ADay, AHour: Integer): Integer;
begin
  /// calculate index
  result := ((ADay * 24) + AHour);
  /// make it circular
  result := ((result mod 168) + 168) mod 168;
end;

function ScheduleSlotIndex(ADay, AHour: Integer): Integer;
begin
  /// calculate index
  result := (ADay * 24 * 4) + (AHour * 4);
  /// make it circular
  result := ((result mod 672) + 672) mod 672;
end;

procedure ScheduleSetIndex(var ASchedule: TSchedule; AIdx: Integer; AState: Boolean);
var
  i, s, m: Integer;
begin
  if (AIdx < 0) or (AIdx >= ASchedule.BodySize * 8) then
    raise Exception.Create('Invalid range.');

  i := ASchedule.HeaderSize + (AIdx div 8);
  s := AIdx mod 8;
  m := (1 shl s);

  if AState then
    ASchedule.Slots[i] := ASchedule.Slots[i] or m
  else
    ASchedule.Slots[i] := ASchedule.Slots[i] and not m;
end;

function ScheduleGetIndex(var ASchedule: TSchedule; AIdx: Integer): Boolean;
var
  i, s, m: Integer;
begin
  if (AIdx < 0) or (AIdx >= ASchedule.BodySize * 8) then
    raise Exception.Create('Invalid range.');

  i := ASchedule.HeaderSize + (AIdx div 8);
  s := AIdx mod 8;
  m := (1 shl s);

  result := (ASchedule.Slots[i] and m) <> 0;
end;

procedure LogonHoursDecode(const AValue: RawByteString; out ASchedule: TSchedule);
var
  i: Integer;
begin
  // Define structure of a logonHours attribut.
  // 7 days * 24 hours / 8 bits in a byte
  // 21 bytes length
  ASchedule.BodySize := 21;
  ASchedule.HeaderSize := 0;

  if Length(AValue) <> ASchedule.BodySize then
    raise Exception.Create('Invalid logonHours length.');

  // Copy bytes from logonHours to slots.
  SetLength(ASchedule.Slots, ASchedule.BodySize);
  for i := 0 to ASchedule.BodySize - 1 do
    ASchedule.Slots[i] := Byte(AValue[i + 1]);
end;

function LogonHoursEncode(const ASchedule: TSchedule): RawByteString;
var
  i: Integer;
begin
  SetLength(result, ASchedule.BodySize);
  for i := 0 to ASchedule.BodySize - 1 do
    result[i + 1] := Char(ASchedule.Slots[i]);
end;

procedure ADScheduleDecode(const AValue: RawByteString; out ASchedule: TSchedule);
var
  i: Integer;
  vl, vr: Byte;
begin
  // Define structure of a schedule attribut.
  // 7 days * 24 hours * 4 quarters / 4 bits in a byte
  // 168 bytes length
  // I prefer to fill bytes.
  // 168 bytes / 2 (8 bits in a bit)
  // 84 bytes length
  ASchedule.BodySize := 84;
  // Header size is 20.
  ASchedule.HeaderSize := 20;

  if Length(AValue) <> (ASchedule.BodySize * 2) + ASchedule.HeaderSize then
    raise Exception.Create('Invalid schedule length.');

  // Set Length of slots (84 + 20 = 104)
  SetLength(ASchedule.Slots, ASchedule.BodySize + ASchedule.HeaderSize);

  // Copy header (20)
  for i := 0 to ASchedule.HeaderSize - 1 do
    ASchedule.Slots[i] := Byte(AValue[i + 1]);

  // Copy body (84)
  for i := 0 to ASchedule.BodySize - 1 do
  begin
    // Get left part:
    //  - Retrieve value at header + (index * 2)
    //  - Shift left value
    // Get right part:
    //  - Retrieve value at header + (index * 2) + 1
    //  - Shift left value
    //  - Shift right value
    vl := Byte(AValue[ASchedule.HeaderSize + (i * 2) + 1]);
    vr := Byte(AValue[ASchedule.HeaderSize + (i * 2) + 2]);
    ASchedule.Slots[ASchedule.HeaderSize + i] := ((vl and $0f)) or ((vr and $0f) shl 4);
  end;
end;

function ADScheduleEncode(const ASchedule: TSchedule): RawByteString;
var
  i: Integer;
  v: Byte;
begin
  SetLength(result, ASchedule.HeaderSize + (ASchedule.BodySize * 2));

  for i := 0 to ASchedule.HeaderSize - 1 do
    result[i + 1] := Char(ASchedule.Slots[i]);

  for i := 0 to ASchedule.BodySize - 1 do
  begin
    v := (ASchedule.Slots[ASchedule.HeaderSize + i]);
    result[(i * 2) + ASchedule.HeaderSize + 1] := Char(($f shl 4) or ((v and $0f)));
    result[(i * 2) + ASchedule.HeaderSize + 2] := Char(($f shl 4) or ((v and $f0) shr 4));
  end;
end;

function TSchedulePresenter.GetRawValue: RawByteString;
begin
  case fScheduleAttributKind of
    sakLogonHour: result := LogonHoursEncode(fSchedule);
    sakSchedule: result := ADScheduleEncode(fSchedule);
    else
      raise Exception.Create('Invalid TScheduleAttributKind.');
  end;
end;

procedure TSchedulePresenter.SetRawValue(AValue: RawByteString);
begin
  case fScheduleAttributKind of
    sakLogonHour: LogonHoursDecode(AValue, fSchedule);
    sakSchedule: ADScheduleDecode(AValue, fSchedule);
    else
      raise Exception.Create('Invalid TScheduleAttributKind.');
  end;
end;

constructor TSchedulePresenter.Create(
  AScheduleAttributKind: TScheduleAttributKind);
begin
  fScheduleAttributKind := AScheduleAttributKind;
  fUTCOffset := -(GetLocalTimeOffset() div 60);
  fDayOffset := 1;
end;

destructor TSchedulePresenter.Destroy;
begin
  inherited Destroy;
end;

procedure TSchedulePresenter.SetLocalValue(ADay, AHour, AValue: Integer);
var
  Idx, i: Integer;
begin
  case fScheduleAttributKind of
    sakLogonHour: ScheduleSetIndex(fSchedule, LogonHoursSlotIndex(ADay + DayOffset, AHour - UTCOffset), (AValue <> 0));
    sakSchedule:
    begin
      Idx := ScheduleSlotIndex(ADay + DayOffset, AHour - UTCOffset);
      for i := 0 to 3 do
        ScheduleSetIndex(fSchedule, Idx + i, (AValue and ($01 shl (3 - i))) <> 0);
    end;
    else
      raise Exception.Create('Invalid TScheduleAttributKind.');
  end;
end;

function TSchedulePresenter.GetLocalValue(ADay, AHour: Integer): Integer;
var
  Idx, i: Integer;
begin
  result := 0;

  case fScheduleAttributKind of
    sakLogonHour:
    begin
      if ScheduleGetIndex(fSchedule, LogonHoursSlotIndex(ADay + DayOffset, AHour - UTCOffset)) then
        result := 1;
    end;
    sakSchedule:
    begin
      Idx := ScheduleSlotIndex(ADay + DayOffset, AHour - UTCOffset);
      for i := 0 to 3 do
        if ScheduleGetIndex(fSchedule, Idx + i) then
          result := result + (1 shl (3 - i));
    end;
    else
      raise Exception.Create('Invalid TScheduleAttributKind.');
  end;
end;

procedure TSchedulePresenter.SetLocalValues(ADayFrom, ADayTo, AHourFrom,
  AHourTo, AValue: Integer);
var
  Day, Hour: Integer;
begin
  for Day := ADayFrom to ADayTo do
    for Hour := AHourFrom to AHourTo do
      SetLocalValue(Day, Hour, AValue);
end;

end.

