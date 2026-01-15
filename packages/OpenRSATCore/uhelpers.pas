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
    function IndexOf(AValue: RawUtf8): Integer;
  end;

implementation

{ TRawUtf8DynArrayHelper }

function TRawUtf8DynArrayHelper.Count: Integer;
begin
  result := Length(Self);
end;

function TRawUtf8DynArrayHelper.Contains(AValue: RawUtf8): Boolean;
begin
  result := (IndexOf(AValue) >= 0);
end;

procedure TRawUtf8DynArrayHelper.Append(AValue: RawUtf8);
begin
  Insert(AValue, Self, Count);
end;

procedure TRawUtf8DynArrayHelper.Remove(Index: Integer);
begin
  Delete(Self, Index, 1);
end;

function TRawUtf8DynArrayHelper.IndexOf(AValue: RawUtf8): Integer;
begin
  for result := 0 to Pred(Count) do
    if Self[result] = AValue then
      Exit;
  result := -1;
end;

end.

