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
    function GetHeader: RawByteString;

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
  SetLength(fHours, 168);
  FillByte(fHours[1], 168, $00);
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

function TSchedulingLogic.GetHeader: RawByteString;
var
  Header: RawByteString;
  Value: UInt32;
begin
  SetLength(Header, 20);

  // Header + Schedule size
  Value := Length(Header) + Length(fHours);
  Move(Value, Header[1], SizeOf(UInt32));

  // Bandwidth (not used)
  Value := 0;
  Move(Value, Header[5], SizeOf(UInt32));

  // Number of schedule (default 1)
  Value := 1;
  Move(Value, Header[9], SizeOf(UInt32));

  // Offset
  Value := 0;
  Move(Value, Header[13], SizeOf(UInt32));

  // Header size
  Value := Length(Header);
  Move(Value, Header[17], SizeOf(UInt32));

  Result := Header;
end;

end.

