unit ufrmnewsharedfolder;

{$mode ObjFPC}{$H+}

interface

uses
  // Lazarus / fpc
  ActnList,
  Classes,
  Controls,
  Forms,
  StdCtrls;

type

  { TFrmNewSharedFolder }

  TFrmNewSharedFolder = class(TFrame)
    ActionList: TActionList;
    Action_Next: TAction;
    Edit_Name: TEdit;
    Edit_Path: TEdit;
    Label_Name: TLabel;
    Label_Path: TLabel;
    procedure Action_NextExecute(Sender: TObject);
    procedure Action_NextUpdate(Sender: TObject);
    procedure Edit_PathChange(Sender: TObject);
  private
    procedure Load;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

implementation
uses
  // Lazarus / fpc
  Dialogs,
  Graphics,
  SysUtils,
  // Submodules
  mormot.core.base,
  mormot.core.text,
  mormot.net.ldap,
  // Rsat
  ucommon,
  ursatldapclient,
  uvisnewobject;
{$R *.lfm}

{ TFrmNewSharedFolder }

procedure TFrmNewSharedFolder.Action_NextExecute(Sender: TObject);
var
  VisNewObject: TVisNewObject;
  Attr: TLdapAttributeList;
  Att: TLdapAttribute;
  DN: RawUtf8;
begin
  VisNewObject := (owner as TVisNewObject);

  Attr := TLdapAttributeList.Create();
  try
    Att := Attr.Add('objectClass', 'top');
    Att.Add('leaf');
    Att.Add('connectionPoint');
    Att.Add('volume');

    if (Edit_Name.Text <> '') then
      Attr.Add('name', Edit_Name.Text);

    if (Edit_Path.Text <> '') then
      Attr.Add('uNCName', Edit_Path.Text);

    DN := FormatUtf8('CN=%,%', [Edit_Name.Text, VisNewObject.ObjectOU]);
    if not VisNewObject.Ldap.Add(DN, Attr) then
    begin
      ShowLdapAddError(VisNewObject.Ldap);
      Exit;
    end;
  finally
    FreeAndNil(Attr);
  end;

  VisNewObject.ModalResult := mrOK;
end;

procedure TFrmNewSharedFolder.Action_NextUpdate(Sender: TObject);
begin
  Action_Next.Enabled := (Edit_Name.Text <> '') and IsServerPath(Edit_Path.Text);
end;

procedure TFrmNewSharedFolder.Edit_PathChange(Sender: TObject);
begin
  if IsServerPath(Edit_Path.Text) then
    Edit_Path.Font.Color := clDefault
  else
  begin
    Edit_Path.Font.Color := clRed;
    Action_Next.Enabled := False;
  end;
end;

procedure TFrmNewSharedFolder.Load;
begin
  Edit_Name.SetFocus;
end;

constructor TFrmNewSharedFolder.Create(TheOwner: TComponent);
var
  OwnerNewObject: TVisNewObject absolute TheOwner;
begin
  inherited Create(TheOwner);

  OwnerNewObject.Caption := rsNewObjectSharedFolder;
  OwnerNewObject.Btn_Next.Action := ActionList.ActionByName('Action_Next');
  OwnerNewObject.Btn_Next.Caption := rsNewObjectBtnOK;
  OwnerNewObject.Btn_Next.Default := True;
  OwnerNewObject.Btn_Back.Visible := False;
  OwnerNewObject.Image_Object.ImageIndex := -1;
  OwnerNewObject.CallBack := @Load;
end;

end.

