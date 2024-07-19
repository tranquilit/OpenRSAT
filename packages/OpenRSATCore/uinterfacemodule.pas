unit uinterfacemodule;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  IniFiles,
  Forms;

type
  TFrameOptions = class;

  TOptions = class;

  TProcRsatOptionsOfObject = procedure(Options: TOptions) of Object;

  { TOptions }

  TOptions = class
  public
    procedure Load(IniFile: TIniFile); virtual; abstract;
    procedure Save(IniFile: TIniFile); virtual; abstract;
    function Changed: Boolean; virtual; abstract;
    function OptionName: String; virtual; abstract;
    procedure CreateFrame(TheOwner: TComponent); virtual; abstract;
    function GetFrame: TFrameOptions; virtual; abstract;
    procedure DeleteFrame; virtual; abstract;
    procedure RegisterObserver(Observer: TProcRsatOptionsOfObject); virtual; abstract;
    procedure RemoveObserver(Observer: TProcRsatOptionsOfObject); virtual; abstract;
  end;

  TFrameOptions = class(TFrame)
    function OptionChanged: Boolean; virtual; abstract;
    procedure Load(Options: TOptions); virtual; abstract;
    procedure Save(Options: TOptions); virtual; abstract;
  end;

  { TAbstractModule }

  IModule = Interface
    function GetModuleEnabled: Boolean;
    procedure SetModuleEnabled(AValue: Boolean);
    function GetModuleName: String;
    function GetModuleDisplayName: String;
    function GetOptions: TOptions;
    procedure Refresh;
    procedure Load;
  end;

  TFrameModule = class(TFrame, IModule)
    // IModule
    function GetModuleEnabled: Boolean; virtual; abstract;
    procedure SetModuleEnabled(AValue: Boolean); virtual; abstract;
    function GetModuleName: String; virtual; abstract;
    function GetModuleDisplayName: String; virtual; abstract;
    function GetOptions: TOptions; virtual; abstract;
    procedure Refresh; virtual; abstract;
    procedure Load; virtual; abstract;
  end;

implementation

end.

