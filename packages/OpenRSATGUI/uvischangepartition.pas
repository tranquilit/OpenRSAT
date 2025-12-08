unit uvischangepartition;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  Buttons;

type

  { TVisChangePartition }

  TVisChangePartition = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    ComboBox1: TComboBox;
    Panel1: TPanel;
    {$push}{$warn 5024 off}
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    {$pop}
  private

  public
    procedure AddPartition(aValue: String);
    procedure AddPartitions(aValue: TStringArray);
    procedure SetCurrentPartition(aValue: String);
    function GetPartition: String;
  end;

implementation

{$R *.lfm}

{ TVisChangePartition }

procedure TVisChangePartition.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    27: Close;
  end;
end;

procedure TVisChangePartition.AddPartition(aValue: String);
begin
  ComboBox1.Items.Add(aValue);
end;

procedure TVisChangePartition.AddPartitions(aValue: TStringArray);
begin
  ComboBox1.Items.AddStrings(aValue);
end;

procedure TVisChangePartition.SetCurrentPartition(aValue: String);
begin
  ComboBox1.ItemIndex := ComboBox1.Items.IndexOf(aValue);
end;

function TVisChangePartition.GetPartition: String;
begin
  result := ComboBox1.Text;
end;

end.

