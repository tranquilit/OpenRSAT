unit ufrmnewmsdskeycredential;

{$mode ObjFPC}{$H+}

interface

uses
  ActnList,
  Classes,
  ExtCtrls,
  Forms,
  StdCtrls;

type

  { TFrmNewMsDSKeyCredential }

  TFrmNewMsDSKeyCredential = class(TFrame)
    Action_Next: TAction;
    Action_Back: TAction;
    ActionList: TActionList;
    Edit_KeyID: TEdit;
    Edit_cn: TEdit;
    Label_KeyID: TLabel;
    Label_cn: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure Action_BackExecute(Sender: TObject);
    procedure Action_BackUpdate(Sender: TObject);
    procedure Action_NextExecute(Sender: TObject);
    procedure Action_NextUpdate(Sender: TObject);
  private
    PageID: Integer;
    procedure Load;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

implementation

uses
  Controls,
  Dialogs,
  SysUtils,
  mormot.core.text,
  mormot.net.ldap,
  mormot.core.base,
  ucommon,
  ucoredatamodule,
  ursatldapclientui,
  uvisnewobject;

{$R *.lfm}

procedure TFrmNewMsDSKeyCredential.Action_NextExecute(Sender: TObject);
var
  NewObject: TVisNewObject;
  AttList: TLdapAttributeList;
  Att: TLdapAttribute;
  DN: RawUtf8;
begin
  case (PageID) of
    0:
    begin
      (owner as TVisNewObject).Btn_Next.Caption := rsNewObjectBtnOK;
      PageID += 1;
      Panel1.Visible := False;
      Panel2.Visible := True;
      Edit_KeyID.SetFocus;
    end;
    1:
    begin
      NewObject := (owner as TVisNewObject);
      AttList := TLdapAttributeList.Create();
      try
        Att := AttList.Add(atObjectClass, 'top');
        Att.Add('msDS-KeyCredential');

        AttList.Add('msDS-KeyId', Edit_KeyId.Text);

        DN := FormatUtf8('CN=%,%', [Edit_cn.Text, NewObject.ObjectOU]);
        if not NewObject.Ldap.Add(DN, AttList) then
          exit;
      finally
        FreeAndNil(Att);
      end;
      NewObject.ModalResult := mrOK;
    end;
  end;
end;

procedure TFrmNewMsDSKeyCredential.Action_NextUpdate(Sender: TObject);
begin
  if PageID = 0 then
    Action_Next.Enabled := (Edit_cn.Text <> '')
  else if PageID = 1 then
    Action_Next.Enabled := (Edit_KeyID.Text <> '')
end;

procedure TFrmNewMsDSKeyCredential.Action_BackExecute(Sender: TObject);
begin
  PageID -= 1;
  Panel1.Visible := True;
  Panel2.Visible := False;
  Edit_cn.SetFocus;
  (owner as TVisNewObject).Btn_Next.Caption := rsNewObjectBtnNext;
end;

procedure TFrmNewMsDSKeyCredential.Action_BackUpdate(Sender: TObject);
begin
  if PageID = 0 then
    Action_Back.Enabled := False
  else
    Action_Back.Enabled := True;
end;

procedure TFrmNewMsDSKeyCredential.Load;
begin
  Edit_cn.SetFocus;
end;

constructor TFrmNewMsDSKeyCredential.Create(TheOwner: TComponent);
var
  OwnerNewObject: TVisNewObject absolute TheOwner;
begin
  inherited Create(TheOwner);

  OwnerNewObject.Caption := rsNewObjectMsDSKeyCredential;
  OwnerNewObject.Btn_Next.Action := ActionList.ActionByName('Action_Next');
  OwnerNewObject.Btn_Next.Caption := rsNewObjectBtnNext;
  OwnerNewObject.Btn_Next.Default := True;
  OwnerNewObject.Btn_Back.Action := ActionList.ActionByName('Action_Back');
  OwnerNewObject.Btn_Back.Caption := rsNewObjectBtnBack;
  PageID := 0;
  OwnerNewObject.CallBack := @Load;
end;

end.

