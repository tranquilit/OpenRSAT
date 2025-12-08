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
  mormot.core.base,
  mormot.core.text,
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
    fItems: PDocVariantData;
  public
    constructor Create(TheOwner: TComponent; AttributeName: RawUtf8;
      Items: PDocVariantData; MaxItemLength: Integer); overload;
  end;

implementation
uses
  Dialogs,
  SysUtils,
  ucommon;
{$R *.lfm}

{ TVisListOther }

constructor TVisListOther.Create(TheOwner: TComponent; AttributeName: RawUtf8; Items: PDocVariantData; MaxItemLength: Integer);
var
  Item: RawUtf8;
begin
  Inherited Create(TheOwner);

  Caption := FormatUtf8('% %', [AttributeName, Self.Caption]);

  Edit_NewValue.MaxLength := MaxItemLength;

  fItems := Items;

  ListBox_Current.Items.BeginUpdate;
  try
    for Item in fItems^.ToRawUtf8DynArray do
      ListBox_Current.Items.Add(Item);
  finally
    ListBox_Current.Items.EndUpdate;
  end;
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
begin
  ListBox_Current.Sorted := False;
  ListBox_Current.Items.Strings[ListBox_Current.ItemIndex] := Dialogs.InputBox(rsRename, '', ListBox_Current.Items.Strings[ListBox_Current.ItemIndex]);
  ListBox_Current.Sorted := True;
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
  fItems^.Clear();
  fItems^.InitArrayFrom(TRawUtf8DynArray(ListBox_Current.Items.ToStringArray()), []);
end;

end.

