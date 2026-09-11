unit uhintwindow;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  Controls,
  Forms,
  SysUtils,
  mormot.core.base;

procedure ShowHintWindow(const TheParent: TControl; const Msg: RawUtf8; HideDelay: Integer = 0);
procedure HideHintWindow;

implementation

var
  HintWnd: THintWindow = nil;

procedure ShowHintWindow(const TheParent: TControl; const Msg: RawUtf8;
  HideDelay: Integer);
var
  R: TRect;
  P: TPoint;
begin
  if not Assigned(HintWnd) then
    HintWnd := THintWindow.Create(TheParent);

  R := HintWnd.CalcHintRect(300, Msg, nil);
  P := TheParent.ClientToScreen(Point(0, TheParent.Height + 2));
  R.Offset(P);

  HintWnd.AutoHide := HideDelay > 0;
  HintWnd.HideInterval := HideDelay;

  HintWnd.ActivateHint(R, Msg);
end;

procedure HideHintWindow;
begin
  if Assigned(HintWnd) then
    HintWnd.Hide;
end;

end.

