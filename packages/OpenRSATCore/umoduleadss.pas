unit umoduleadss;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.base,
  uoption,
  umodule,
  umoduleadssoption;

type

  { TModuleADSS }

  TModuleADSS = class(TModule)
  private
    fEnabled: Boolean;
    fOption: TModuleADSSOption;

    function GetShowService: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function ADSSOption: TModuleADSSOption;
    // Expose Option
    property ShowService: Boolean read GetShowService;

    /// TModule
  protected
    function GetEnabled: Boolean; override;
    procedure SetEnabled(AValue: Boolean); override;
    function GetName: RawUtf8; override;
    function GetDisplayName: RawUtf8; override;
    function GetOption: TOption; override;
  end;

implementation
uses
  ucommon;

{ TModuleADSS }

function TModuleADSS.GetShowService: Boolean;
begin
  result := fOption.ShowService;
end;

constructor TModuleADSS.Create;
begin
  fEnabled := True;
  fOption := TModuleADSSOption.Create;
  fOption.Load;
end;

destructor TModuleADSS.Destroy;
begin
  FreeAndNil(fOption);

  inherited Destroy;
end;

function TModuleADSS.ADSSOption: TModuleADSSOption;
begin
  result := fOption;
end;

function TModuleADSS.GetEnabled: Boolean;
begin
  result := fEnabled;
end;

procedure TModuleADSS.SetEnabled(AValue: Boolean);
begin
  if AValue = fEnabled then
    Exit;
  fEnabled := AValue;
end;

function TModuleADSS.GetName: RawUtf8;
begin
  result := rsModuleADSSName;
end;

function TModuleADSS.GetDisplayName: RawUtf8;
begin
  result := rsModuleADSIDisplayName;
end;

function TModuleADSS.GetOption: TOption;
begin
  result := fOption;
end;

end.

