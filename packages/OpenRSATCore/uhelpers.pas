unit uhelpers;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  controls,
  SysUtils,
  StdCtrls;

type

  { TCustomEditHelper }

  TCustomEditHelper = class helper for TCustomEdit
  private
    function GetCaptionNoChange: TCaption;
    procedure SetCaptionNoChange(AValue: TCaption);
  public
    property CaptionNoChange: TCaption read GetCaptionNoChange write SetCaptionNoChange;
  end;

  { TButtonControlHelper }

  TButtonControlHelper = class helper for TButtonControl
  private
    function GetCheckedNoChange: Boolean;
    procedure SetCheckedNoChange(AValue: Boolean);
  public
    property CheckedNoChange: Boolean read GetCheckedNoChange write SetCheckedNoChange;
  end;

  { TCustomComboBoxHelper }

  TCustomComboBoxHelper = class helper for TCustomComboBox
  private
    function GetCaptionNoChange: TCaption;
    procedure SetCaptionNoChange(AValue: TCaption);

  public
    property CaptionNoChange: TCaption read GetCaptionNoChange write SetCaptionNoChange;
  end;

implementation

{ TCustomEditHelper }

function TCustomEditHelper.GetCaptionNoChange: TCaption;
begin
  result := Caption;
end;

procedure TCustomEditHelper.SetCaptionNoChange(AValue: TCaption);
var
  OnChangeBak: TNotifyEvent;
begin
  OnChangeBak := OnChange;
  try
    OnChange := nil;
    Caption := AValue;
  finally
    OnChange := OnChangeBak;
  end;
end;

{ TButtonControlHelper }

function TButtonControlHelper.GetCheckedNoChange: Boolean;
begin
  result := Checked;
end;

procedure TButtonControlHelper.SetCheckedNoChange(AValue: Boolean);
var
  OnChangeBak: TNotifyEvent;
begin
  OnChangeBak := OnChange;
  try
    OnChange := nil;
    Checked := AValue;
  finally
    OnChange := OnChangeBak;
  end;
end;

{ TCustomComboBoxHelper }

function TCustomComboBoxHelper.GetCaptionNoChange: TCaption;
begin
  result := Caption;
end;

procedure TCustomComboBoxHelper.SetCaptionNoChange(AValue: TCaption);
var
  OnChangeBak: TNotifyEvent;
begin
  OnChangeBak := OnChange;
  try
    OnChange := nil;
    Caption := AValue;
  finally
    OnChange := OnChangeBak;
  end;
end;

end.

