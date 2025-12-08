unit uhelpers;

{$mode objfpc}{$H+}
{$modeswitch TypeHelpers}

interface

uses
  Classes,
  SysUtils,
  mormot.core.base;

type

  { TRawUtf8DynArrayHelper }

  TRawUtf8DynArrayHelper = type helper for TRawUtf8DynArray
    function Count: Integer;
    function Contains(AValue: RawUtf8): Boolean;
    procedure Append(AValue: RawUtf8);
    procedure Remove(Index: Integer);
  end;

implementation

{ TRawUtf8DynArrayHelper }

function TRawUtf8DynArrayHelper.Count: Integer;
begin
  result := Length(Self);
end;

function TRawUtf8DynArrayHelper.Contains(AValue: RawUtf8): Boolean;
var
  v: RawUtf8;
begin
  result := False;
  for v in Self do
  begin
    result := v = AValue;
    if result then
      Exit;
  end;
end;

procedure TRawUtf8DynArrayHelper.Append(AValue: RawUtf8);
begin
  Insert(AValue, Self, Count);
end;

procedure TRawUtf8DynArrayHelper.Remove(Index: Integer);
begin
  Delete(Self, Index, 1);
end;

end.

