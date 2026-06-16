unit upropertyframe;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  Forms,
  SysUtils,
  uproperty,
  uopenrsatuicontextinterface;

type

  { TPropertyFrame }

  TPropertyFrame = class(TFrame)
  protected
    fIContext: IOpenRSATUIContext;
  public
    constructor Create(TheOwner: TComponent; Context: IOpenRSATUIContext); overload;
    procedure Update(Props: TProperty); virtual; abstract;
    procedure DropFiles(const FileNames: array of string); virtual;

    property IContext: IOpenRSATUIContext read fIContext write fIContext;
  end;

  TPropertyFrameClass = class of TPropertyFrame;

implementation

{ TPropertyFrame }

constructor TPropertyFrame.Create(TheOwner: TComponent;
  Context: IOpenRSATUIContext);
begin
  Create(TheOwner);
  IContext := Context;
end;

procedure TPropertyFrame.DropFiles(const FileNames: array of string);
begin

end;

end.

