unit upropertyframe;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  Forms,
  SysUtils,
  uproperty;

type

  { TPropertyFrame }

  TPropertyFrame = class(TFrame)
  public
    procedure Update(Props: TProperty); virtual; abstract;
    procedure DropFiles(const FileNames: array of string); virtual;
  end;

  TPropertyFrameClass = class of TPropertyFrame;

implementation

{ TPropertyFrame }

procedure TPropertyFrame.DropFiles(const FileNames: array of string);
begin

end;

end.

