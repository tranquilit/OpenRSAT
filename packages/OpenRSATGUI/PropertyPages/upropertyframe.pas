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

  TNotifyCancelEvent = procedure(Sender: TObject; var Cancel: Boolean) of object;

  { TPropertyFrame }

  TPropertyFrame = class(TFrame)
  protected
    fIContext: IOpenRSATUIContext;
    fOnBeforeApply: TNotifyCancelEvent;
    fOnAfterApply: TNotifyEvent;
  public
    constructor Create(TheOwner: TComponent; Context: IOpenRSATUIContext); overload;
    procedure Update(Props: TProperty); virtual; abstract;
    procedure DoBeforeApply(var Cancel: Boolean); virtual;
    procedure DoAfterApply; virtual;
    procedure DropFiles(const FileNames: array of string); virtual;

    property IContext: IOpenRSATUIContext read fIContext write fIContext;
  end;

  TPropertyFrameClass = class of TPropertyFrame;

implementation

{ TPropertyFrame }

constructor TPropertyFrame.Create(TheOwner: TComponent;
  Context: IOpenRSATUIContext);
begin
  IContext := Context;
  fOnBeforeApply := nil;
  fOnAfterApply := nil;
  Create(TheOwner);
end;

procedure TPropertyFrame.DoBeforeApply(var Cancel: Boolean);
begin
  if Assigned(fOnBeforeApply) then
    fOnBeforeApply(Self, Cancel);
end;

procedure TPropertyFrame.DoAfterApply;
begin
  if Assigned(fOnAfterApply) then
    fOnAfterApply(Self);
end;

procedure TPropertyFrame.DropFiles(const FileNames: array of string);
begin

end;

end.

