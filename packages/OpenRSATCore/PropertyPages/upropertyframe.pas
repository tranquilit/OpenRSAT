unit upropertyframe;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  Forms,
  SysUtils,
  uproperty;

type
  TPropertyFrame = class(TFrame)
  public
    procedure Update(Props: TProperty); virtual; abstract;
  end;

implementation

end.

