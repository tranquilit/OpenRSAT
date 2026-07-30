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
    fHeader, fHours: RawByteString;
    fKindOfPage: KindOfPage;
    fDefaultType: Byte;

    procedure SetupHoursRawByteString;
  public
    constructor Create(Page: KindOfPage);
    destructor Destroy; override;

    procedure LoadScheduleToHours(const ScheduleData: RawByteString);
    function GetHeader: RawByteString;

    property Hours: RawByteString read fHours write fHours;
  end;

implementation

constructor TSchedulingLogic.Create(Page: KindOfPage);
begin
  fKindOfPage := Page;
  case Page of
    SiteLinkSchedulingPage: fDefaultType := $FF;
    NTDSSchedulingPage: fDefaultType := $0F;
  end;

  SetupHoursRawByteString
end;

destructor TSchedulingLogic.Destroy;
begin
  inherited Destroy;
end;

procedure TSchedulingLogic.SetupHoursRawByteString;
begin
  fHours := '';
  SetLength(fHours, 168);
  FillByte(fHours[1], 168, fDefaultType);
end;

procedure TSchedulingLogic.LoadScheduleToHours(const ScheduleData: RawByteString);
var
  Data: RawByteString;
begin
  if ScheduleData = '' then
  begin
    FillByte(fHours[1], Length(fHours), fDefaultType);
    Exit;
  end;

  Data := ScheduleData;
  Delete(Data, 21, Length(Data));
  fHeader := Data;

  Data := ScheduleData;
  Delete(Data, 1, 20);
  fHours := Data;
end;

function TSchedulingLogic.GetHeader: RawByteString;
var
  NewHeader: RawByteString;
  Value: UInt32;
begin
  if fHeader <> '' then
  begin
    Result := fHeader;
    Exit;
  end;

  SetLength(NewHeader, 20);

  // Header + Schedule size
  Value := Length(NewHeader) + Length(fHours);
  Move(Value, NewHeader[1], SizeOf(UInt32));

  // Bandwidth (not used)
  Value := 0;
  Move(Value, NewHeader[5], SizeOf(UInt32));

  // Number of schedule (default 1)
  Value := 1;
  Move(Value, NewHeader[9], SizeOf(UInt32));

  // Offset
  Value := 0;
  Move(Value, NewHeader[13], SizeOf(UInt32));

  // Header size
  Value := Length(NewHeader);
  Move(Value, NewHeader[17], SizeOf(UInt32));

  Result := NewHeader;
end;

end.

