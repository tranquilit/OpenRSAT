unit ufrmchangeschedule;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  ExtCtrls,
  StdCtrls,
  Spin,
  Math,
  Dialogs,
  Graphics,
  Grids,
  mormot.core.base,
  mormot.core.log,
  mormot.core.text,
  mormot.net.ldap,
  uhelpersui,
  uproperty,
  ursatldapclient,
  ulog, Types;

type

  { TScheduleState }
  TScheduleState = (ssAvailable, ssNotAvailable);

  { TFrmChangeSchedule }
  TFrmChangeSchedule = class(TForm)
    Button_OK: TButton;
    Button_Cancel: TButton;
    DrawGrid1: TDrawGrid;
    Label1: TLabel;
    Label_Selection: TLabel;
    Panel_Replication: TPanel;
    Panel_ClosingBtns: TPanel;
    RadioButton_NotAvailable: TRadioButton;
    RadioButton_Available: TRadioButton;
    Shape_NotAvailable: TShape;
    Shape_Available: TShape;
    procedure DrawGrid1DrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure DrawGrid1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DrawGrid1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure DrawGrid1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Label1Click(Sender: TObject);
    procedure RadioButton_AvailableClick(Sender: TObject);
    procedure RadioButton_NotAvailableClick(Sender: TObject);
  private
    fSchedule: array of array of TScheduleState;

    fSelectedStartCol, fSelectedStartRow: Integer;
    fSelectedEndCol, fSelectedEndRow: Integer;
    FDragging: Boolean;

    procedure SelectColumn(ACol: Integer);
    procedure SelectRow(ARow: Integer);
    procedure UpdateRadioButtons;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  FrmChangeSchedule: TFrmChangeSchedule;

implementation

{$R *.lfm}

constructor TFrmChangeSchedule.Create(TheOwner: TComponent);
var
  Col, Row: Integer;
begin
  inherited Create(TheOwner);

  Caption := 'Schedule for Site Link';

  DrawGrid1.Width := DrawGrid1.DefaultColWidth + 244;
  DrawGrid1.Height := DrawGrid1.DefaultRowHeight * 8 + 4;

  fSelectedStartCol := 1;
  fSelectedEndCol := DrawGrid1.ColCount - 1;
  fSelectedStartRow := 1;
  fSelectedEndRow := DrawGrid1.RowCount - 1;

  SetLength(FSchedule, DrawGrid1.ColCount, DrawGrid1.RowCount);
  for Col := 0 to DrawGrid1.ColCount - 1 do
    for Row := 0 to DrawGrid1.RowCount - 1 do
      fSchedule[Col][Row] := ssNotAvailable;
end;

destructor TFrmChangeSchedule.Destroy;
begin
  inherited Destroy;
end;

procedure TFrmChangeSchedule.DrawGrid1DrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  MinCol, MaxCol, MinRow, MaxRow: Integer;
begin
  if (aCol > 0) and (aRow > 0) then
  begin
    case fSchedule[aCol][aRow] of
      ssAvailable:
        DrawGrid1.Canvas.Brush.Color := clBlue;

      ssNotAvailable:
        DrawGrid1.Canvas.Brush.Color := clWhite;
    end;
  end
  else
    DrawGrid1.Canvas.Brush.Color := clBtnFace;

  DrawGrid1.Canvas.FillRect(aRect);

  if (fSelectedStartCol <> -1) and (fSelectedStartRow <> -1) then
  begin
    MinCol := Math.Min(fSelectedStartCol, fSelectedEndCol);
    MaxCol := Math.Max(fSelectedStartCol, fSelectedEndCol);
    MinRow := Math.Min(fSelectedStartRow, fSelectedEndRow);
    MaxRow := Math.Max(fSelectedStartRow, fSelectedEndRow);

    if (aCol >= MinCol) and (aCol <= MaxCol)
    and (aRow >= MinRow) and (aRow <= MaxRow) then
    begin
      DrawGrid1.Canvas.Pen.Color := clRed;
      DrawGrid1.Canvas.Pen.Width := 2;
      DrawGrid1.Canvas.Rectangle(aRect);
    end;
  end;

  if aCol = 0 then
    case aRow of
      0: DrawGrid1.Canvas.TextRect(aRect, aRect.Left + 2, aRect.Top + 2, 'All');
      1: DrawGrid1.Canvas.TextRect(aRect, aRect.Left + 2, aRect.Top + 2, 'Monday');
      2: DrawGrid1.Canvas.TextRect(aRect, aRect.Left + 2, aRect.Top + 2, 'Tuesday');
      3: DrawGrid1.Canvas.TextRect(aRect, aRect.Left + 2, aRect.Top + 2, 'Wednesday');
      4: DrawGrid1.Canvas.TextRect(aRect, aRect.Left + 2, aRect.Top + 2, 'Thursday');
      5: DrawGrid1.Canvas.TextRect(aRect, aRect.Left + 2, aRect.Top + 2, 'Friday');
      6: DrawGrid1.Canvas.TextRect(aRect, aRect.Left + 2, aRect.Top + 2, 'Saturday');
      7: DrawGrid1.Canvas.TextRect(aRect, aRect.Left + 2, aRect.Top + 2, 'Sunday');
    end;
end;



procedure TFrmChangeSchedule.DrawGrid1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Col, Row: Integer;
begin
  DrawGrid1.MouseToCell(X, Y, Col, Row);


  if (Col = 0) and (Row = 0) then
  begin
    fSelectedStartCol := 1;
    fSelectedEndCol := DrawGrid1.ColCount - 1;

    fSelectedStartRow := 1;
    fSelectedEndRow := DrawGrid1.RowCount - 1;

    UpdateRadioButtons;
    DrawGrid1.Invalidate;
    Exit;
  end;

  if (Row = 0) and (Col > 0) then
  begin
    SelectColumn(Col);
    Exit;
  end;

  if (Col = 0) and (Row > 0) then
  begin
    SelectRow(Row);
    Exit;
  end;

  if (Col > 0) and (Row > 0) then
  begin
    FDragging := True;

    fSelectedStartCol := Col;
    fSelectedStartRow := Row;

    fSelectedEndCol := Col;
    FSelectedEndRow := Row;

    DrawGrid1.Invalidate;
  end;
end;


procedure TFrmChangeSchedule.DrawGrid1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Col, Row: Integer;
begin
  if not FDragging then
    Exit;

  DrawGrid1.MouseToCell(X, Y, Col, Row);

  if Col < 1 then
    Col := 1;

  if Row < 1 then
    Row := 1;

  fSelectedEndCol := Col;
  fSelectedEndRow := Row;

  DrawGrid1.Invalidate;
end;


procedure TFrmChangeSchedule.DrawGrid1MouseUp(
  Sender: TObject;
  Button: TMouseButton;
  Shift: TShiftState;
  X, Y: Integer);
begin
  FDragging := False;

  UpdateRadioButtons;
end;

procedure TFrmChangeSchedule.Label1Click(Sender: TObject);
begin

end;


procedure TFrmChangeSchedule.RadioButton_AvailableClick(Sender: TObject);
var
  Col, Row: Integer;
begin
  for Col := Min(fSelectedStartCol, fSelectedEndCol) to Max(fSelectedStartCol, fSelectedEndCol) do
    for Row := Min(fSelectedStartRow, fSelectedEndRow) to Max(fSelectedStartRow, fSelectedEndRow) do
      fSchedule[Col][Row] := ssAvailable;

  DrawGrid1.Invalidate;
end;



procedure TFrmChangeSchedule.RadioButton_NotAvailableClick(Sender: TObject);
var
  Col, Row: Integer;
begin
  for Col := Min(fSelectedStartCol, fSelectedEndCol) to Max(fSelectedStartCol, fSelectedEndCol) do
    for Row := Min(fSelectedStartRow, fSelectedEndRow) to Max(fSelectedStartRow, fSelectedEndRow) do
      fSchedule[Col][Row] := ssNotAvailable;

  DrawGrid1.Invalidate;
end;



procedure TFrmChangeSchedule.SelectColumn(ACol: Integer);
begin
  fSelectedStartCol := ACol;
  fSelectedEndCol := ACol;

  fSelectedStartRow := DrawGrid1.FixedRows;
  fSelectedEndRow := DrawGrid1.RowCount - 1;

  UpdateRadioButtons;
  DrawGrid1.Invalidate;
end;


procedure TFrmChangeSchedule.SelectRow(ARow: Integer);
begin

fSelectedStartCol := DrawGrid1.FixedCols;
  fSelectedEndCol := DrawGrid1.ColCount - 1;

  fSelectedStartRow := ARow;
  fSelectedEndRow := ARow;

  UpdateRadioButtons;
  DrawGrid1.Invalidate;
end;


procedure TFrmChangeSchedule.UpdateRadioButtons;
var
  Col, Row: Integer;
  HasAvailable, HasNotAvailable: Boolean;
  MinCol, MaxCol, MinRow, MaxRow: Integer;
begin
  if fSelectedStartCol = -1 then
    Exit;

  if fSelectedStartCol < fSelectedEndCol then
  begin
    MinCol := fSelectedStartCol;
    MaxCol := fSelectedEndCol;
  end
  else
  begin
    MinCol := fSelectedEndCol;
    MaxCol := fSelectedStartCol;
  end;

  if fSelectedStartRow < fSelectedEndRow then
  begin
    MinRow := fSelectedStartRow;
    MaxRow := fSelectedEndRow;
  end
  else
  begin
    MinRow := fSelectedEndRow;
    MaxRow := fSelectedStartRow;
  end;

  HasAvailable := False;
  HasNotAvailable := False;

  for Col := MinCol to MaxCol do
    for Row := MinRow to MaxRow do
      case fSchedule[Col][Row] of
        ssAvailable:
          HasAvailable := True;

        ssNotAvailable:
          HasNotAvailable := True;
      end;

  if HasAvailable and not HasNotAvailable then
  begin
    RadioButton_Available.Checked := True;
    RadioButton_NotAvailable.Checked := False;
  end
  else if HasNotAvailable and not HasAvailable then
  begin
    RadioButton_Available.Checked := False;
    RadioButton_NotAvailable.Checked := True;
  end
  else
  begin
    // Zone mixte
    RadioButton_Available.Checked := False;
    RadioButton_NotAvailable.Checked := False;
  end;
end;


end.

