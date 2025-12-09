unit ufrmnewgroup;

{$mode ObjFPC}{$H+}

interface

uses
  // Lazarus / fpc
  ActnList,
  Classes,
  ExtCtrls,
  Forms,
  StdCtrls;

type

  { TFrmNewGroup }

  TFrmNewGroup = class(TFrame)
    Edit_Name: TEdit;
    Label_Name: TLabel;
    Label_nETBIOSName: TLabel;
    Edit_nETBIOSName: TEdit;
    RadioGroup_Scope: TRadioGroup;
      RadioBtn_Local: TRadioButton;
      RadioBtn_Global: TRadioButton;
      RadioBtn_Universal: TRadioButton;
    RadioGroup_Type: TRadioGroup;
      RadioBtn_Security: TRadioButton;
      RadioBtn_Distribution: TRadioButton;
      Panel_Empty: TPanel;
    ActionList: TActionList;
      Action_Next: TAction;
    procedure Action_NextExecute(Sender: TObject);
    procedure Action_NextUpdate(Sender: TObject);
    procedure Edit_NameChange(Sender: TObject);
  private
    procedure Load;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

const
  // https://learn.microsoft.com/en-us/windows/win32/adschema/a-grouptype#remarks
  GT_BUILTIN          = $00000001;
  GT_GLOBAL_SCOPE     = $00000002;
  GT_LOCAL_SCOPE      = $00000004;
  GT_UNIVERSAL_SCOPE  = $00000008;
  GT_APP_BASIC        = $00000010;
  GT_APP_QUERY        = $00000020;
  GT_SECURITY         = $80000000;

implementation
uses
  // Lazarus / fpc
  Controls,
  Dialogs,
  SysUtils,
  // Submodules
  mormot.core.text,
  mormot.net.ldap,
  // Rsat
  ucommon,
  ucoredatamodule,
  ursatldapclientui,
  uvisnewobject;
{$R *.lfm}

{ TFrmNewGroup }

// Edit
procedure TFrmNewGroup.Edit_NameChange(Sender: TObject);
begin
  Edit_nETBIOSName.Text := Edit_Name.Text;
end;

procedure TFrmNewGroup.Load;
begin
  Edit_Name.SetFocus;
end;

constructor TFrmNewGroup.Create(TheOwner: TComponent);
var
  OwnerNewObject: TVisNewObject absolute TheOwner;
begin
  inherited Create(TheOwner);

  OwnerNewObject.Caption := rsNewObjectGroup;
  OwnerNewObject.Btn_Next.Action := ActionList.ActionByName('Action_Next');
  OwnerNewObject.Btn_Next.Caption := rsNewObjectBtnOK;
  OwnerNewObject.Btn_Next.Default := True;
  OwnerNewObject.Btn_Back.Visible := False;
  OwnerNewObject.Image_Object.ImageIndex := Ord(ileADGroup);
  OwnerNewObject.CallBack := @Load;
end;

// Actions
procedure TFrmNewGroup.Action_NextExecute(Sender: TObject);
var
  NewObject: TVisNewObject;
  Att: TLdapAttributeList;
  BinMask: Int32;
begin
  NewObject := (owner as TVisNewObject);
  Att := TLdapAttributeList.Create();
  try
    Att.Add('objectClass', 'top').Add('group');

    if (Edit_nETBIOSName.Text <> '') then
      Att.Add('sAMAccountName', Edit_nETBIOSName.Text);
    if RadioBtn_Local.Checked then // https://learn.microsoft.com/en-us/windows/win32/adschema/a-grouptype#remarks
      BinMask := Int32(GT_LOCAL_SCOPE)
    else if RadioBtn_Global.Checked then
      BinMask := Int32(GT_GLOBAL_SCOPE)
    else // RadioBtn_universal.Checked
      BinMask := Int32(GT_UNIVERSAL_SCOPE);
    if RadioBtn_Security.Checked then // security group or not
      BinMask := BinMask or Int32(GT_SECURITY);
    Att.Add('groupType', BinMask.ToString);

    if not NewObject.Ldap.Add('CN=' + Edit_Name.Text + ',' + NewObject.ObjectOU, Att) then
    begin
      ShowLdapAddError(NewObject.Ldap);
      Exit;
    end;
    NewObject.ModalResult := mrOK;
  finally
    FreeAndNil(Att);
  end;
end;

procedure TFrmNewGroup.Action_NextUpdate(Sender: TObject);
begin
  Action_Next.Enabled := (Edit_Name.Text <> '') and (Edit_nETBIOSName.Text <> '');
end;

end.

