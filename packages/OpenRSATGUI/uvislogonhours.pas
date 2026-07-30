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

  { TScheduleValue }
  TScheduleValue = (svDenied, svAvailable, svOnce, svTwice);
  TScheduleValueGetter = function(i: Integer): TScheduleValue of object;
  TScheduleValueSetter = procedure(i: Integer; Value: TScheduleValue) of object;

  TCheckButtonInConstructor = procedure of object;
  TApplyColorOnTile = procedure(aCol, aRow: Integer) of object;
  TSetRadioButtonsAfterSelection = procedure(defRect: TRect) of object;
  TUpdateColor = procedure(Sender: TObject; defRect: TRect) of object;

  { TVisLogonHours }
  TVisLogonHours = class(TForm)
    Label_Title: TLabel;
    RadioButton_Secret: TRadioButton;
    SpinEdit_UTC: TSpinEditEx;
    DrawGrid: TDrawGrid;
    Btn_OK: TBitBtn;
    Btn_Cancel: TBitBtn;
    Panel_Allowed: TPanel;
    RadioButton_Allowed: TRadioButton;
    Panel_Denied: TPanel;
    RadioButton_Denied: TRadioButton;
    Panel_Once: TPanel;
    RadioButton_Once: TRadioButton;
    Panel_Twice: TPanel;
    RadioButton_Twice: TRadioButton;
    Label_Recap: TLabel;
    Timer_Fix: TTimer;
    ActionList: TActionList;
    Action_OK: TAction;
    procedure Action_OKExecute(Sender: TObject);
    procedure Action_OKUpdate(Sender: TObject);
    procedure DrawGridAfterSelection(Sender: TObject; aCol, aRow: Integer);
    procedure DrawGridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure DrawGridExit(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure RadioButton_Click(Sender: TObject);
    procedure SpinEdit_UTCChange(Sender: TObject);
    procedure Timer_FixTimer(Sender: TObject);

  private
    PHours: PRawByteString;
    HoursUTC: RawByteString;
    ReadingData: Boolean;
    GetScheduleValue: TScheduleValueGetter;
    SetScheduleValue: TScheduleValueSetter;
    CheckRadioButton: TCheckButtonInConstructor;
    ApplyColorOnTile: TApplyColorOnTile;
    SetRadioButtonsAfterSelection: TSetRadioButtonsAfterSelection;
    UpdateColor: TUpdateColor;
    fPageOption: KindOfPage;
    procedure CreateLogonHours;
    procedure CreateBasicSchedule(pageName, allowedCaption, deniedCaption: String);
    procedure CreateFullSchedule(pageName, opt1, opt2, opt3, opt4: String);
    procedure DrawFirstGrid;

    // logonHours functions
    procedure CheckButtonForLogonHours;
    procedure ApplyColorOnLogonHours(aCol, aRow: Integer);
    procedure SetRadioButtonsForLogonHours(defRect: TRect);
    procedure UpdateColorForLogonHours(Sender: TObject; defRect: TRect);
    function GetBit(i: Integer): Boolean;
    procedure SetBit(i: Integer; b: Boolean);

    // schedule functions
    procedure CheckButtonForSchedules;
    procedure ApplyColorOnSchedules(aCol, aRow: Integer);
    procedure SetRadioButtonsForSchedules(defRect: TRect);
    procedure UpdateColorForSchedules(Sender: TObject; defRect: TRect);
    function GetScheduleValueSiteLinkGUI(i: Integer): TScheduleValue;
    function GetScheduleValueNTDSGUI(i: Integer): TScheduleValue;
    procedure SetScheduleValueSiteLinkGUI(i: Integer; Value: TScheduleValue);
    procedure SetScheduleValueNTDSGUI(i: Integer; Value: TScheduleValue);

  public
    constructor Create(TheOwner: TComponent; _PHours: PRawByteString; PageOption: KindOfPage); reintroduce;
  end;

const
  days: array[0..6] of String = (rsMonday, rsTuesday, rsWednesday, rsThursday, rsFriday, rsSaturday,rsSunday);

implementation
uses
  Graphics,
  SysUtils,
  ucommonui,
  mormot.core.text;
{$R *.lfm}

{ TVisLogonHours }

// Form
constructor TVisLogonHours.Create(TheOwner: TComponent; _PHours: PRawByteString; PageOption: KindOfPage);
begin
  ReadingData := True;
  Inherited Create(TheOwner);

  fPageOption := PageOption;
  case fPageOption of
    LogonHoursPage: CreateLogonHours;
    SiteLinkSchedulingPage: CreateBasicSchedule('Site Link Scheduling', rsReplicationAvailable, rsReplicationDenied);
    NTDSSchedulingPage: CreateFullSchedule('Schedule for Site Settings', rsNone, rsOncePerHour, rsTwicePerHour, rsFourPerHour);
  end;

  SpinEdit_UTC.Value := -(GetLocalTimeOffset() div 60);

  PHours := _PHours;
  HoursUTC := _PHours^;

  DrawFirstGrid;

  CheckRadioButton;
  UnifyButtonsWidth([Btn_OK, Btn_Cancel]);
end;

procedure TVisLogonHours.CreateLogonHours;
begin
  CheckRadioButton := @CheckButtonForLogonHours;
  ApplyColorOnTile:= @ApplyColorOnLogonHours;
  SetRadioButtonsAfterSelection := @SetRadioButtonsForLogonHours;
  UpdateColor := @UpdateColorForLogonHours;
end;

procedure TVisLogonHours.CreateBasicSchedule(pageName, allowedCaption, deniedCaption: String);
begin
  Caption := pageName;
  RadioButton_Allowed.Caption := allowedCaption;
  RadioButton_Denied.Caption := deniedCaption;
  CheckRadioButton := @CheckButtonForSchedules;
  ApplyColorOnTile:= @ApplyColorOnSchedules;
  SetRadioButtonsAfterSelection := @SetRadioButtonsForSchedules;
  UpdateColor := @UpdateColorForSchedules;
  GetScheduleValue := @GetScheduleValueSiteLinkGUI;
  SetScheduleValue := @SetScheduleValueSiteLinkGUI;
end;

procedure TVisLogonHours.CreateFullSchedule(pageName, opt1, opt2, opt3, opt4: String);
begin
  Caption := pageName;
  RadioButton_Denied.Caption := opt1;
  RadioButton_Once.Caption := opt2;
  RadioButton_Twice.Caption := opt3;
  RadioButton_Allowed.Caption := opt4;
  Panel_Once.Visible := True;
  Panel_Twice.Visible := True;
  RadioButton_Once.Visible := True;
  RadioButton_Twice.Visible := True;
  CheckRadioButton := @CheckButtonForSchedules;
  ApplyColorOnTile:= @ApplyColorOnSchedules;
  SetRadioButtonsAfterSelection := @SetRadioButtonsForSchedules;
  UpdateColor := @UpdateColorForSchedules;
  GetScheduleValue := @GetScheduleValueNTDSGUI;
  SetScheduleValue := @SetScheduleValueNTDSGUI;
end;

procedure TVisLogonHours.DrawFirstGrid;
var
  s: String;
  colWidth, max: Integer;
begin
  max := 0;
  for s in days do
  begin
    colWidth := DrawGrid.Canvas.TextWidth(s);
    if max < colWidth then
      max := colWidth;
  end;
  DrawGrid.Columns.Items[0].MinSize := max + 8;
  DrawGrid.Selection := Rect(1, 1, 0, 0);
end;

// Draw
procedure TVisLogonHours.DrawGridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  style: TTextStyle;
  newRect: TRect;
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
    ApplyColorOnTile(aCol, aRow);
    DrawGrid.Canvas.Rectangle(aRect);
  end;

  // Fix draw selection
  if (aRow = 7) and (aCol = HoursPerDay) then
    Timer_Fix.Enabled := True;
end;

procedure TVisLogonHours.DrawGridExit(Sender: TObject);
begin
  DrawGridAfterSelection(DrawGrid, 0, 0);
end;

procedure TVisLogonHours.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    27: Close;
  end;
end;

procedure TVisLogonHours.Timer_FixTimer(Sender: TObject);
begin
  DrawGridAfterSelection(DrawGrid, 0, 0);
  Timer_Fix.Enabled := False;
end;

procedure TVisLogonHours.DrawGridAfterSelection(Sender: TObject; aCol, aRow: Integer);
var
  col, l, w: Integer;
  defRect, rect: TRect;
begin
  defRect := DrawGrid.Selection;
  // Row Select
  if DrawGrid.Selection.Left = 0 then
  begin
    defRect.Left := 1;
    if DrawGrid.Selection.Width = 0 then
      defRect.Width := DrawGrid.ColCount - 2;
  end;
  if DrawGrid.Selection.Top = 0 then
  begin
    defRect.Top := 1;
    if DrawGrid.Selection.Height = 0 then
      defRect.Height := DrawGrid.RowCount - 2;
  end;

  {$IfDEF WINDOWS}
  // Draw Selection rect
  DrawGrid.Repaint(); // Fix flickering
  {$ENDIF}

  l:= DrawGrid.ColWidths[0];
  for col := 1 to defRect.Left -1 do
    l += DrawGrid.ColWidths[col];
  rect.Left   := l;

  w := DrawGrid.ColWidths[defRect.Left];
  for col := 1 + defRect.Left to defRect.Right do
    w += DrawGrid.ColWidths[col];
  rect.Width  := w;

  rect.Top    := (defRect.Top    - 1) * DrawGrid.DefaultRowHeight + DrawGrid.RowHeights[0];
  rect.Height := (defRect.Height + 1) * DrawGrid.DefaultRowHeight;
  DrawGrid.Canvas.Brush.Color := clRed;
  DrawGrid.Canvas.FrameRect(rect);

  // Set RadioButtons
  ReadingData := True;
  SetRadioButtonsAfterSelection(defRect);

  ReadingData := False;

  // Set Label_Recap
  if defRect.Top = defRect.Bottom then
    Label_Recap.Caption := FormatUtf8('On % from %:00 to %:59', [days[defRect.Top - 1], defRect.Left - 1, defRect.Right - 1])
  else
    Label_Recap.Caption := FormatUtf8('On % through % from %:00 to %:59', [days[defRect.Top - 1], days[defRect.Bottom - 1], defRect.Left - 1, defRect.Right - 1]);
end;

// Buttons
procedure TVisLogonHours.Action_OKExecute(Sender: TObject); // OK
begin
  PHours^ := HoursUTC;
end;

procedure TVisLogonHours.Action_OKUpdate(Sender: TObject);
begin
  Action_OK.Enabled  := HoursUTC <> PHours^;
  Btn_OK.Default     := Action_OK.Enabled;
  Btn_Cancel.Default := not Action_OK.Enabled;
end;

procedure TVisLogonHours.RadioButton_Click(Sender: TObject);
var
  defRect: TGridRect;
begin
  if ReadingData then
    Exit;

  defRect := DrawGrid.Selection;

  if DrawGrid.Selection.Left = 0 then
  begin
    defRect.Left := 1;
    if DrawGrid.Selection.Width = 0 then
      defRect.Width := DrawGrid.ColCount - 2;
  end;

  if DrawGrid.Selection.Top = 0 then
  begin
    defRect.Top := 1;
    if DrawGrid.Selection.Height = 0 then
      defRect.Height := DrawGrid.RowCount - 2;
  end;

  UpdateColor(Sender, defRect);

  DrawGrid.Repaint();
  DrawGridAfterSelection(DrawGrid, 0, 0);
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

  // Shift data
  Timer_Fix.Enabled := True;
end;

// Functions for Schedules and LogonHours
procedure TVisLogonHours.CheckButtonForSchedules;
begin
  case GetScheduleValue(-Integer(SpinEdit_UTC.Value)) of
    svAvailable: RadioButton_Allowed.Checked := True;
    svTwice: RadioButton_Twice.Checked := True;
    svOnce: RadioButton_Once.Checked := True;
    svDenied: RadioButton_Denied.Checked := True;
  end;
end;

procedure TVisLogonHours.CheckButtonForLogonHours;
begin
  RadioButton_Allowed.Checked := GetBit(-Integer(SpinEdit_UTC.Value));
  RadioButton_Denied.Checked  := not RadioButton_Allowed.Checked;
end;

procedure TVisLogonHours.ApplyColorOnSchedules(aCol, aRow: Integer);
begin
  case GetScheduleValue((aRow - 1) * HoursPerDay + aCol - 1 - Integer(SpinEdit_UTC.Value)) of
    svDenied: DrawGrid.Canvas.Brush.Color := clBtnShadow;
    svAvailable: DrawGrid.Canvas.Brush.Color := clBlue;
    svOnce: DrawGrid.Canvas.Brush.Color := clGradientInactiveCaption;
    svTwice: DrawGrid.Canvas.Brush.Color := clGradientActiveCaption;
  end;
end;

procedure TVisLogonHours.ApplyColorOnLogonHours(aCol, aRow: Integer);
begin
  if GetBit((aRow - 1) * HoursPerDay + aCol - 1 - Integer(SpinEdit_UTC.Value)) then
    DrawGrid.Canvas.Brush.Color := clBlue
  else
    DrawGrid.Canvas.Brush.Color := clBtnShadow;
end;

procedure TVisLogonHours.SetRadioButtonsForSchedules(defRect: TRect);
var
  col, row: Integer;
  FirstValue: TScheduleValue;
  SameValue: Boolean;
begin
  FirstValue := GetScheduleValue((defRect.Top - 1) * HoursPerDay + defRect.Left - 1 - Integer(SpinEdit_UTC.Value));
  SameValue := True;
  for row := defRect.Top to defRect.Bottom do
  begin
    for col := defRect.Left to defRect.Right do
    begin
      if GetScheduleValue((row - 1) * HoursPerDay + col - 1 - Integer(SpinEdit_UTC.Value)) <> FirstValue then
      begin
        SameValue := False;
        Break;
      end;
    end;

    if not SameValue then
      Break;
  end;

  RadioButton_Denied.Checked := False;
  RadioButton_Allowed.Checked := False;
  RadioButton_Once.Checked := False;
  RadioButton_Twice.Checked := False;

  if SameValue then
  begin
    case FirstValue of
      svDenied: RadioButton_Denied.Checked := True;
      svAvailable: RadioButton_Allowed.Checked := True;
      svOnce: RadioButton_Once.Checked := True;
      svTwice: RadioButton_Twice.Checked := True;
    end;
  end;
end;

procedure TVisLogonHours.SetRadioButtonsForLogonHours(defRect: TRect);
var
  col, row: Integer;
  Blue, Grey, bit: Boolean;
begin
  Blue := True;
  Grey := True;
  for row := defRect.Top to defRect.Bottom do
    if Blue or Grey then
      for col := defRect.Left to defRect.Right do
      begin
        bit := GetBit((row - 1) * HoursPerDay + col - 1 - Integer(SpinEdit_UTC.Value));
        Blue := Blue and bit;
        Grey := Grey and not bit;
        if not (Blue or Grey) then
          break;
      end
    else
      break;

  RadioButton_Allowed.Checked := Blue;
  RadioButton_Denied.Checked  := Grey;
  RadioButton_Secret.Checked  := not (Blue or Grey);
end;

procedure TVisLogonHours.UpdateColorForSchedules(Sender: TObject; defRect: TRect);
var
  NewValue: TScheduleValue;
  aRow, aCol: LongInt;
begin
  if Sender = RadioButton_Denied then
    NewValue := svDenied
  else if Sender = RadioButton_Allowed then
    NewValue := svAvailable
  else if Sender = RadioButton_Once then
    NewValue := svOnce
  else
    NewValue := svTwice;

  for aRow := defRect.Top to defRect.Bottom do
    for aCol := defRect.Left to defRect.Right do
      SetScheduleValue((aRow - 1) * HoursPerDay + aCol - 1 - Integer(SpinEdit_UTC.Value), NewValue);
end;

procedure TVisLogonHours.UpdateColorForLogonHours(Sender: TObject; defRect: TRect);
var
  b: Boolean;
  aRow, aCol: LongInt;
begin
  b := (Sender as TRadioButton).Name = RadioButton_Allowed.Name;
  for aRow := defRect.Top to defRect.Bottom do
    for aCol := defRect.Left to defRect.Right do
      SetBit((aRow-1) * HoursPerDay + aCol-1 - Integer(SpinEdit_UTC.Value), b);
end;

function TVisLogonHours.GetScheduleValueSiteLinkGUI(i: Integer): TScheduleValue;
var
  Value: Byte;
  GridDay, GridHour: Integer;
  ADDay, ADIndex: Integer;
begin
  if i < 0 then
    i += 168;
  i := i mod 168;

  GridDay := i div 24;
  GridHour := i mod 24;

  ADDay := (GridDay + 1) mod 7;
  ADIndex := ADDay * 24 + GridHour;

  Value := Byte(HoursUTC[ADIndex + 1]);
  case Value of
    $F0: Result := svDenied;
    $FF: Result := svAvailable;
  end;
end;

function TVisLogonHours.GetScheduleValueNTDSGUI(i: Integer): TScheduleValue;
var
  Value: Byte;
  GridDay, GridHour: Integer;
  ADDay, ADIndex: Integer;
begin
  if i < 0 then
    i += 168;
  i := i mod 168;

  GridDay := i div 24;
  GridHour := i mod 24;

  ADDay := (GridDay + 1) mod 7;
  ADIndex := ADDay * 24 + GridHour;

  Value := Byte(HoursUTC[ADIndex + 1]);
  case Value of
    $00: Result := svDenied;
    $01: Result := svOnce;
    $05: Result := svTwice;
    $0F: Result := svAvailable;
  end;
end;

function TVisLogonHours.GetBit(i: Integer): Boolean;
begin
  if i < 0 then
    i += 168;
  i := i mod 168;

  result := Boolean(Byte(HoursUTC[1 + i div 8]) and Byte(1 Shl (i mod 8)));
end;

procedure TVisLogonHours.SetBit(i: Integer; b: Boolean);
begin
  if i < 0 then
    i += 168;
  i := i mod 168;

  HoursUTC[1 + i div 8] := Char(Byte(HoursUTC[1 + i div 8]) and not (Byte(1) Shl (i mod 8)) or (Byte(b) Shl (i mod 8)))
end;

procedure TVisLogonHours.SetScheduleValueSiteLinkGUI(i: Integer; Value: TScheduleValue);
var
  GridDay, GridHour: Integer;
  ADDay, ADIndex: Integer;
begin
  if i < 0 then
    i += 168;
  i := i mod 168;

  GridDay := i div 24;
  GridHour := i mod 24;

  ADDay := (GridDay + 1) mod 7;
  ADIndex := ADDay * 24 + GridHour;

  case Value of
    svDenied: HoursUTC[ADIndex + 1] := Char($F0);
    svAvailable: HoursUTC[ADIndex + 1] := Char($FF);
  end;
end;

procedure TVisLogonHours.SetScheduleValueNTDSGUI(i: Integer; Value: TScheduleValue);
var
  GridDay, GridHour: Integer;
  ADDay, ADIndex: Integer;
begin
  if i < 0 then
    i += 168;
  i := i mod 168;

  GridDay := i div 24;
  GridHour := i mod 24;

  ADDay := (GridDay + 1) mod 7;
  ADIndex := ADDay * 24 + GridHour;

  case Value of
    svDenied: HoursUTC[ADIndex + 1] := Char($00);
    svAvailable: HoursUTC[ADIndex + 1] := Char($0F);
    svOnce: HoursUTC[ADIndex + 1] := Char($01);
    svTwice: HoursUTC[ADIndex + 1] := Char($05);
  end;
end;

end.

