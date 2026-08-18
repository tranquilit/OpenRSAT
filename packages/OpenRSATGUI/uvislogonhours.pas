unit uvislogonhours;

{$mode ObjFPC}{$H+}

interface

uses
  ActnList,
  Buttons,
  Classes,
  ComCtrls,
  Controls,
  ExtCtrls,
  Dialogs,
  Forms,
  Grids,
  SpinEx,
  StdCtrls,
  Types,
  uschedulinglogic,
  ucommon;

type

  TSchedulePageKind = (
    spkLogonHours,
    spkSiteLinkSchedule,
    spkNTDSSchedule
  );

  { TScheduleValue }
  TScheduleValue = (svDenied, svAvailable, svOnce, svTwice);
  TScheduleValueGetter = function(i: Integer): TScheduleValue of object;
  TScheduleValueSetter = procedure(i: Integer; Value: TScheduleValue) of object;

  TCheckButtonInConstructor = procedure of object;
  TApplyColorOnTile = function(aCol, aRow: Integer): Integer of object;
  TSetRadioButtonsAfterSelection = procedure(defRect: TRect) of object;
  TUpdateColor = procedure(Sender: TObject; defRect: TRect) of object;

  { TVisLogonHours }
  TVisLogonHours = class(TForm)
    Label_Title: TLabel;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    Panel14: TPanel;
    Panel15: TPanel;
    Panel16: TPanel;
    Panel17: TPanel;
    Panel18: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    Panel_LogonHours: TPanel;
    Panel_NTDSSchedule: TPanel;
    Panel_SiteLinkSchedule: TPanel;
    RadioButton_LogonPermitted: TRadioButton;
    RadioButton_LogonDenied: TRadioButton;
    RadioButton_FourTimesPerHour: TRadioButton;
    RadioButton_TwicePerHour: TRadioButton;
    RadioButton_OncePerHour: TRadioButton;
    RadioButton_None: TRadioButton;
    RadioButton_ReplicationAvailable: TRadioButton;
    RadioButton_ReplicationNotAvailable: TRadioButton;
    RadioButton_Secret: TRadioButton;
    SpinEdit_UTC: TSpinEditEx;
    DrawGrid: TDrawGrid;
    Btn_OK: TBitBtn;
    Btn_Cancel: TBitBtn;
    Label_Recap: TLabel;
    procedure DrawGridAfterSelection(Sender: TObject; aCol, aRow: Integer);
    procedure DrawGridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure RadioButtonChange(Sender: TObject);
    procedure SpinEdit_UTCChange(Sender: TObject);

  private
    function GetUpdating: Boolean;
  private
    fUpdating: Integer;

    fSchedulePageKind: TSchedulePageKind;
    fPresenter: TSchedulePresenter;

    procedure SetupLogonHoursPage;
    procedure SetupSiteLinkSchedulePage;
    procedure SetupNTDSSchedulePage;

    function GetTileFilledPercentage(ACol, ARow: Integer): Integer;
    function GetSelection: TGridRect;

    procedure SetFirstColWidth;
    procedure SetRadioButtonFromSelection(Rect: TGridRect);
    procedure RefreshGrid;
    procedure DrawSelection;
    procedure UpdateRecapCaptionFromSelection(Rect: TGridRect);

    procedure BeginUpdate;
    procedure EndUpdate;

    property Updating: Boolean read GetUpdating;

    function GetRawValue: RawByteString;
    procedure SetRawValue(AValue: RawByteString);

  public
    constructor Create(TheOwner: TComponent; ASchedulePageKind: TSchedulePageKind); reintroduce;
    destructor Destroy; override;

    property RawValue: RawByteString read GetRawValue write SetRawValue;
  end;

const
  days: array[0..6] of String = (rsMonday, rsTuesday, rsWednesday, rsThursday, rsFriday, rsSaturday,rsSunday);

implementation
uses
  Graphics,
  SysUtils,
  Math,
  ucommonui,
  mormot.core.text;
{$R *.lfm}

{ TVisLogonHours }

// Form
constructor TVisLogonHours.Create(TheOwner: TComponent;
  ASchedulePageKind: TSchedulePageKind);
begin
  Inherited Create(TheOwner);

  fUpdating := 0;

  fSchedulePageKind := ASchedulePageKind;

  case fSchedulePageKind of
    spkLogonHours: SetupLogonHoursPage;
    spkSiteLinkSchedule: SetupSiteLinkSchedulePage;
    spkNTDSSchedule: SetupNTDSSchedulePage;
  end;

  BeginUpdate;
  try
    SpinEdit_UTC.Value := fPresenter.UTCOffset;
    SetFirstColWidth;
  finally
    EndUpdate;
  end;

  UnifyButtonsWidth([Btn_OK, Btn_Cancel]);
end;

destructor TVisLogonHours.Destroy;
begin
  FreeAndNil(fPresenter);

  inherited Destroy;
end;

procedure TVisLogonHours.SetFirstColWidth;
var
  m, i: Integer;
begin
  m := 0;

  for i := 0 to High(days) do
    m := Max(m, DrawGrid.Canvas.TextWidth(days[i]));

  DrawGrid.Columns.Items[0].MinSize := m + 8;
  DrawGrid.Selection := Rect(1, 1, 0, 0);
end;

procedure TVisLogonHours.SetRadioButtonFromSelection(Rect: TGridRect);
var
  v, prev: Integer;
  row, col: LongInt;
begin
  prev := -1;
  BeginUpdate;
  try
    case fSchedulePageKind of
      spkLogonHours:
      begin
        RadioButton_LogonPermitted.Checked := False;
        RadioButton_LogonDenied.Checked := False;
      end;
      spkNTDSSchedule:
      begin
        RadioButton_None.Checked := False;
        RadioButton_OncePerHour.Checked := False;
        RadioButton_TwicePerHour.Checked := False;
        RadioButton_FourTimesPerHour.Checked := False;
      end;
      spkSiteLinkSchedule:
      begin
        RadioButton_ReplicationAvailable.Checked := False;
        RadioButton_ReplicationNotAvailable.Checked := False;
      end;
    end;

    for row := Rect.Top to Rect.Bottom do
      for col := Rect.Left to Rect.Right do
      begin
        v := fPresenter.GetLocalValue(row - 1, col - 1);
        if (prev >= 0) and (v <> prev) then
          Exit;
        prev := v;
      end;
    case fSchedulePageKind of
      spkLogonHours:
      begin
        RadioButton_LogonDenied.Checked := v = 0;
        RadioButton_LogonPermitted.Checked := v = 1;
      end;
      spkNTDSSchedule:
      begin
        RadioButton_None.Checked := v = 0;
        RadioButton_OncePerHour.Checked := v = $08;
        RadioButton_TwicePerHour.Checked := v = $06;
        RadioButton_FourTimesPerHour.Checked := v = $0f;
      end;
      spkSiteLinkSchedule:
      begin
        RadioButton_ReplicationAvailable.Checked := v <> 0;
        RadioButton_ReplicationNotAvailable.Checked := v = 0;
      end;
    end;
  finally
    EndUpdate;
  end;
end;

// Draw
procedure TVisLogonHours.DrawGridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  style: TTextStyle;
  newRect, innerRect: TRect;
  Percent: Integer;
begin
  // Header rows
  if aCol = 0 then
  begin
    DrawGrid.Canvas.Brush.Color := clDefault;
    DrawGrid.Canvas.Rectangle(aRect);
    if aRow > 0 then
    begin
      style.Alignment := taLeftJustify;
      style.Layout := tlCenter;
      DrawGrid.Canvas.TextRect(aRect, 2, 0, days[aRow-1], style);
    end;
    Exit;
  end;

  // Header columns
  if aRow = 0 then
  begin
    // newRect to fuse two columns together
    newRect.Top    := aRect.Top;
    newRect.Left   := aRect.Left - 1;
    newRect.Height := aRect.Height;
    if (aCol mod 2 = 0) and (acol <> HoursPerDay) then // if left and not last column
      newRect.Width := aRect.Width + 2
    else
      newRect.Width := aRect.Width + 1;

    // Rect
    DrawGrid.Canvas.Brush.Color := clDefault;
    DrawGrid.Canvas.Rectangle(newRect);

    // Text
    style.Layout := tlCenter;
    if acol mod 2 = 0 then
      // if left
      if acol = HoursPerDay then begin // last column is midnight
        style.Alignment := taCenter;
        DrawGrid.Canvas.TextRect(aRect, 0, 0, '0', style);
      end else begin
        style.Alignment := taRightJustify;
        DrawGrid.Canvas.TextRect(aRect, 0, 0, (aCol div 10).ToString(), style);
      end
    else
    // if right
      if acol = 1 then begin // first column is midnight
        style.Alignment := taCenter;
        DrawGrid.Canvas.TextRect(aRect, 0, 0, '0', style);
      end else begin
        style.Alignment := taRightJustify; // I wish I could use taLeftJustify
        newRect.Left    := aRect.Left;
        newRect.Width   := DrawGrid.Canvas.TextWidth(((aCol-1) mod 10).ToString());
        DrawGrid.Canvas.TextRect(newRect, 0, 0, ((aCol-1) mod 10).ToString(), style);
      end;
    Exit;
  end;

  // Cells
  if (aRow > 0) and (aCol > 0) then
  begin
    DrawGrid.Canvas.Brush.Color := clBtnShadow;
    DrawGrid.Canvas.Rectangle(aRect);

    Percent := GetTileFilledPercentage(ACol - 1, ARow - 1);
    if Percent > 0 then
    begin
      innerRect := aRect;
      innerRect.Top := aRect.Bottom - ((aRect.Bottom - aRect.Top) * Percent div 100);

      DrawGrid.Canvas.Brush.Color := clBlue;
      DrawGrid.Canvas.Rectangle(innerRect);
    end;
  end;
end;

procedure TVisLogonHours.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    27: Close;
  end;
end;

procedure TVisLogonHours.RadioButtonChange(Sender: TObject);
var
  v: Integer;
  Sel: TGridRect;
begin
  if Updating then
    Exit;

  Sel := GetSelection;

  v := $00;
  if Sender = RadioButton_None then
    v := $00
  else if Sender = RadioButton_OncePerHour then
    v := $08
  else if Sender = RadioButton_TwicePerHour then
    v := $06
  else if Sender = RadioButton_FourTimesPerHour then
    v := $0f
  else if Sender = RadioButton_LogonDenied then
    v := $00
  else if Sender = RadioButton_LogonPermitted then
    v := $01
  else if Sender = RadioButton_ReplicationAvailable then
    v := $0f
  else if Sender = RadioButton_ReplicationNotAvailable then
    v := $00
  else
    raise Exception.Create('Sender not supported.');

  fPresenter.SetLocalValues(Sel.Top - 1, Sel.Bottom - 1, Sel.Left - 1, Sel.Right - 1, v);
  RefreshGrid;
end;

function TVisLogonHours.GetUpdating: Boolean;
begin
  result := fUpdating > 0;
end;

procedure TVisLogonHours.SetupLogonHoursPage;
begin
  fPresenter := TSchedulePresenter.Create(sakLogonHour);

  Panel_LogonHours.Visible := True;
end;

procedure TVisLogonHours.SetupSiteLinkSchedulePage;
begin
  fPresenter := TSchedulePresenter.Create(sakSchedule);

  Panel_SiteLinkSchedule.Visible := True;
end;

procedure TVisLogonHours.SetupNTDSSchedulePage;
begin
  fPresenter := TSchedulePresenter.Create(sakSchedule);

  Panel_NTDSSchedule.Visible := True;
end;

function TVisLogonHours.GetTileFilledPercentage(ACol, ARow: Integer): Integer;
var
  v: Integer;
begin
  result := 0;

  v := fPresenter.GetLocalValue(ARow, ACol);
  case fSchedulePageKind of
    spkLogonHours:
      if v <> 0 then
        result := 100
      else
        result := 0;
    spkSiteLinkSchedule, spkNTDSSchedule:
    begin
      if v = $00 then
        result := 0
      else if v = $08 then
        result := 33
      else if (v = $06) or (v = $0a) then
        result := 67
      else
        result := 100
    end;
  end;
end;

function TVisLogonHours.GetSelection: TGridRect;
begin
  result := DrawGrid.Selection;

  if result.Left = 0 then
  begin
    result.Left := 1;
    if result.Width <= 0 then
      result.Width := DrawGrid.ColCount - 2;
  end;

  if result.Top = 0 then
  begin
    result.Top := 1;
    if result.Height <= 0 then
      result.Height := DrawGrid.RowCount - 2;
  end;
end;

procedure TVisLogonHours.RefreshGrid;
begin
  DrawGrid.Repaint;
  DrawSelection;
end;

procedure TVisLogonHours.DrawSelection;
var
  Sel: TGridRect;
  l, w, col: Integer;
  rect: TRect;
begin
  Sel := GetSelection;

  l := DrawGrid.ColWidths[0];
  for col := 1 to Sel.Left - 1 do
    l += DrawGrid.ColWidths[col];
  rect.Left := l;

  w := DrawGrid.ColWidths[Sel.Left];
  for col := 1 + Sel.Left to Sel.Right do
    w += DrawGrid.ColWidths[col];
  rect.Width := w;

  rect.Top    := (Sel.Top    - 1) * DrawGrid.DefaultRowHeight + DrawGrid.RowHeights[0];
  rect.Height := (Sel.Height + 1) * DrawGrid.DefaultRowHeight;
  DrawGrid.Canvas.Brush.Color := clRed;
  DrawGrid.Canvas.FrameRect(rect);

  SetRadioButtonFromSelection(Sel);

  UpdateRecapCaptionFromSelection(Sel);
end;

procedure TVisLogonHours.UpdateRecapCaptionFromSelection(Rect: TGridRect);
begin
  // Set Label_Recap
  if Rect.Top = Rect.Bottom then
    Label_Recap.Caption := FormatUtf8('On % from %:00 to %:59', [days[Rect.Top - 1], Rect.Left - 1, Rect.Right - 1])
  else
    Label_Recap.Caption := FormatUtf8('On % through % from %:00 to %:59', [days[Rect.Top - 1], days[Rect.Bottom - 1], Rect.Left - 1, Rect.Right - 1]);
end;

procedure TVisLogonHours.BeginUpdate;
begin
  Inc(fUpdating);
end;

procedure TVisLogonHours.EndUpdate;
begin
  Dec(fUpdating);
end;

procedure TVisLogonHours.DrawGridAfterSelection(Sender: TObject; aCol, aRow: Integer);
begin
  RefreshGrid;
end;

procedure TVisLogonHours.SpinEdit_UTCChange(Sender: TObject);
var
  s: String;
begin
  if Integer(SpinEdit_UTC.Value) < 0 then
    s := '-'
  else
    s := '+';
  s += Abs(Integer(SpinEdit_UTC.Value)).ToString();
  SpinEdit_UTC.Text := s;

  fPresenter.UTCOffset := SpinEdit_UTC.Value;

  if not Updating then
    RefreshGrid;
end;

function TVisLogonHours.GetRawValue: RawByteString;
begin
  result := fPresenter.RawValue;
end;


procedure TVisLogonHours.SetRawValue(AValue: RawByteString);
begin
  fPresenter.RawValue := AValue;
end;

end.

