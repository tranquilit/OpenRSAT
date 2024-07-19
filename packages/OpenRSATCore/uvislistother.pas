unit uvislistother;

{$mode ObjFPC}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
  ActnList,
  Buttons,
  Classes,
  Controls,
  ExtCtrls,
  Forms,
  StdCtrls,
  mormot.core.variants;

type

  { TVisListOther }

  TVisListOther = class(TForm)
    Label_NewValue: TLabel;
    Edit_NewValue: TEdit;
    BitBtn_Add: TBitBtn;
    Label_Current: TLabel;
    ListBox_Current: TListBox;
    BitBtn_Edit: TBitBtn;
    BitBtn_Remove: TBitBtn;
    Panel_Bottom: TPanel;
      Btn_BottomOK: TBitBtn;
      Btn_BottomCancel: TBitBtn;
    ActionList: TActionList;
      Action_Add: TAction;
      Action_Edit: TAction;
      Action_Remove: TAction;
      Action_OK: TAction;
    procedure Action_AddExecute(Sender: TObject);
    procedure Action_AddUpdate(Sender: TObject);
    procedure Action_EditExecute(Sender: TObject);
    procedure Action_EditUpdate(Sender: TObject);
    procedure Action_OKExecute(Sender: TObject);
    procedure Action_RemoveExecute(Sender: TObject);
    procedure Action_RemoveUpdate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    PdocArr: PDocVariantData;
  public
    constructor Create(TheOwner: TComponent; _caption: String ; _PdocArr: PDocVariantData; _MaxLength: Integer); overload;
  end;

implementation
uses
  Dialogs,
  SysUtils,
  mormot.core.base,
  ucommon;
{$R *.lfm}

{ TVisListOther }

constructor TVisListOther.Create(TheOwner: TComponent; _caption: String; _PdocArr: PDocVariantData; _MaxLength: Integer);
var
  pv: PVariant;
begin
  Inherited Create(TheOwner);

  self.Caption := _caption + ' ' + self.Caption;

  self.Edit_NewValue.MaxLength := _MaxLength;

  PdocArr := _PdocArr;
  for pv in PdocArr^.Items do
    ListBox_Current.Items.Add(pv^);
end;

procedure TVisListOther.FormShow(Sender: TObject);
begin
  UnifyButtonsWidth([BitBtn_Add, BitBtn_Edit, BitBtn_Remove]);
  UnifyButtonsWidth([Btn_BottomOK, Btn_BottomCancel]);
end;

procedure TVisListOther.Action_AddExecute(Sender: TObject); // Add
begin
  ListBox_Current.ItemIndex := ListBox_Current.Items.Add(Edit_NewValue.Text);
  Edit_NewValue.Text := '';
end;

procedure TVisListOther.Action_AddUpdate(Sender: TObject);
begin
  Action_Add.Enabled := Edit_NewValue.Text <> '';
end;

procedure TVisListOther.Action_EditExecute(Sender: TObject); // Edit
var
  s: String;
begin
  s := Dialogs.InputBox(rsRename, '', ListBox_Current.Items.Strings[ListBox_Current.ItemIndex]);
  ListBox_Current.Items.Delete(ListBox_Current.ItemIndex);
  ListBox_Current.ItemIndex := ListBox_Current.Items.Add(s);
end;

procedure TVisListOther.Action_EditUpdate(Sender: TObject);
begin
  Action_Edit.Enabled := ListBox_Current.ItemIndex <> -1;
end;

procedure TVisListOther.Action_RemoveExecute(Sender: TObject); // Remove
begin
  ListBox_Current.Items.Delete(ListBox_Current.ItemIndex);
  if ListBox_Current.Count <> 0 then
    ListBox_Current.ItemIndex := 0;
end;

procedure TVisListOther.Action_RemoveUpdate(Sender: TObject);
begin
  Action_Remove.Enabled := ListBox_Current.ItemIndex <> -1
end;

procedure TVisListOther.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    27: Close;
  end;
end;

procedure TVisListOther.Action_OKExecute(Sender: TObject); // OK
begin
  PdocArr^.Clear();
  PdocArr^.InitArrayFrom(TRawUtf8DynArray(ListBox_Current.Items.ToStringArray()), []);
end;

end.

