unit umoduleaduc;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.base,
  umoduleaducoption,
  uoption,
  umodule;

type

  { TModuleADUC }

  TModuleADUC = class(TModule)
  private
    fModuleADUCOption: TModuleADUCOption;

    fEnabled: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    property ADUCOption: TModuleADUCOption read fModuleADUCOption;

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

{ TModuleADUC }

constructor TModuleADUC.Create;
begin
  fEnabled := True;
  fModuleADUCOption := TModuleADUCOption.Create;
  fModuleADUCOption.Load;
end;

destructor TModuleADUC.Destroy;
begin
  FreeAndNil(fModuleADUCOption);

  inherited Destroy;
end;

function TModuleADUC.GetEnabled: Boolean;
begin
  result := fEnabled;
end;

procedure TModuleADUC.SetEnabled(AValue: Boolean);
begin
  if AValue = fEnabled then
    Exit;
  fEnabled := AValue;
end;

function TModuleADUC.GetName: RawUtf8;
begin
  result := rsModuleADUCName;
end;

function TModuleADUC.GetDisplayName: RawUtf8;
begin
  result := rsModuleADUCDisplayName;
end;

function TModuleADUC.GetOption: TOption;
begin
  result := fModuleADUCOption;
end;

end.

