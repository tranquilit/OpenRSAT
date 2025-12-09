unit uvislogonworkstation;

{$mode ObjFPC}{$H+}

interface

uses
  ActnList,
  Buttons,
  Classes,
  ComCtrls,
  Controls,
  ExtCtrls,
  Forms,
  StdCtrls;

type

  { TVisLogonWorkstation }

  TVisLogonWorkstation = class(TForm)
    Label_Info: TLabel;
    Label_Title: TLabel;
    RadioButton_All: TRadioButton;
    RadioButton_Following: TRadioButton;
    GroupBox_Following: TGroupBox;
      Label_Name: TLabel;
      Edit_Name: TEdit;
      ListBox_Names: TListBox;
      Btn_Add: TBitBtn;
      Btn_Edit: TBitBtn;
      Btn_Remove: TBitBtn;
    Panel_Bottom: TPanel;
      Btn_OK: TBitBtn;
      Btn_Cancel: TBitBtn;
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
      procedure RadioButton_FollowingChange(Sender: TObject);
  private
    PUserWorkstations: PString;
  public
    constructor Create(TheOwner: TComponent; _PUserWorkstations: PString); reintroduce;
  end;

implementation
uses
  Dialogs,
  SysUtils,
  ucommon,
  ucommonui;
{$R *.lfm}

{ TVisLogonWorkstation }

// Form
constructor TVisLogonWorkstation.Create(TheOwner: TComponent; _PUserWorkstations: PString);
begin
  if not Assigned(_PUserWorkstations) then
    Exit;
  Inherited Create(TheOwner);
  PUserWorkstations := _PUserWorkstations;
  if _PUserWorkstations^ <> '' then
  begin
    RadioButton_All.Checked := False;
    RadioButton_Following.Checked := True;
    GroupBox_Following.Enabled := True;
    ListBox_Names.Items.AddStrings(_PUserWorkstations^.Split([',']));
  end
  else
  begin
    RadioButton_All.Checked := True;
    RadioButton_Following.Checked := False;
    GroupBox_Following.Enabled := False;
  end;

  UnifyButtonsWidth([Btn_Add, Btn_Edit, Btn_Remove]);
  UnifyButtonsWidth([Btn_OK, Btn_Cancel]);
end;

// Radio
procedure TVisLogonWorkstation.RadioButton_FollowingChange(Sender: TObject);
begin
  GroupBox_Following.Enabled := RadioButton_Following.Checked;
end;

// Actions
procedure TVisLogonWorkstation.Action_AddExecute(Sender: TObject); // Add
begin
  ListBox_Names.ItemIndex := ListBox_Names.Items.Add(Edit_Name.Text);
  Edit_Name.Text := '';
end;

procedure TVisLogonWorkstation.Action_AddUpdate(Sender: TObject);
begin
  Action_Add.Enabled := Edit_Name.Text <> '';
  Btn_Add.Default := Action_Add.Enabled;
  Btn_OK.Default  := not Action_Add.Enabled;
end;

procedure TVisLogonWorkstation.Action_EditExecute(Sender: TObject); // Edit
var
  s: String;
begin
  s := Dialogs.InputBox(rsRename, '', ListBox_Names.Items.Strings[ListBox_Names.ItemIndex]);
  ListBox_Names.Items.Delete(ListBox_Names.ItemIndex);
  ListBox_Names.ItemIndex := ListBox_Names.Items.Add(s);
end;

procedure TVisLogonWorkstation.Action_EditUpdate(Sender: TObject);
begin
  Action_Edit.Enabled := ListBox_Names.ItemIndex <> -1;
end;

procedure TVisLogonWorkstation.Action_RemoveExecute(Sender: TObject); // Remove
begin
  ListBox_Names.Items.Delete(ListBox_Names.ItemIndex);
end;

procedure TVisLogonWorkstation.Action_RemoveUpdate(Sender: TObject);
begin
  Action_Remove.Enabled := ListBox_Names.ItemIndex <> -1;
end;

procedure TVisLogonWorkstation.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    27: Close;
  end;
end;

procedure TVisLogonWorkstation.Action_OKExecute(Sender: TObject); // OK
begin
  if RadioButton_All.Checked then
    PUserWorkstations^ := ''
  else
    PUserWorkstations^ := String.Join(',', ListBox_Names.Items.ToStringArray);
end;

end.

