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
    constructor Create(TheOwner: TComponent; Option: TOption); virtual; abstract; overload;
    function OptionChanged: Boolean; virtual; abstract;
    procedure Load; virtual; abstract;
    procedure Save; virtual; abstract;
  end;

  TFrameOptionClass = class of TFrameOption;

implementation

end.

