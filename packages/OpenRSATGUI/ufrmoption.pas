unit ufrmoption;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  uoption;

type

  { TFrameOption }

  TFrameOption = class(TFrame)
    function OptionChanged: Boolean; virtual; abstract;
    procedure Load; virtual; abstract;
    procedure Save; virtual; abstract;
  end;

implementation

end.

