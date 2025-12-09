unit uvistaskresetpassword;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  Forms,
  ExtCtrls,
  Buttons,
  StdCtrls, Controls;

type

  { TVisTaskResetPassword }

  TVisTaskResetPassword = class(TForm)
    Panel_Top: TPanel;
      Label_NewPassword: TLabel;
      Label_ConfirmPassword: TLabel;
      Edit_NewPassword: TEdit;
      Edit_ConfirmPassword: TEdit;
    Panel_Middle: TPanel;
      CheckBox_Change: TCheckBox;
      Label_Effect: TLabel;
      CheckBox_Unlock: TCheckBox;
      Label_Status: TLabel;
    Panel_Bottom: TPanel;
      Btn_Ok: TBitBtn;
      Btn_Cancel: TBitBtn;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  VisTaskResetPassword: TVisTaskResetPassword;

implementation
uses
  ucommonui;
{$R *.lfm}

{ TVisTaskResetPassword }

procedure TVisTaskResetPassword.FormShow(Sender: TObject);
begin
  UnifyButtonsWidth([Btn_Ok, Btn_Cancel]);
end;

procedure TVisTaskResetPassword.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    27: Close;
  end;
end;

end.

