unit ufrmnewmsdnsserversettings;

{$mode ObjFPC}{$H+}

interface

uses
  ActnList,
  Classes,
  ExtCtrls,
  Forms,
  StdCtrls;

type

  { TFrmNewMsDNSServerSettings }

  TFrmNewMsDNSServerSettings = class(TFrame)
    Action_Next: TAction;
    ActionList: TActionList;
    Edit_cn: TEdit;
    Label_cn: TLabel;
    Panel1: TPanel;
    procedure Action_NextExecute(Sender: TObject);
    procedure Action_NextUpdate(Sender: TObject);
  private
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
  mormot.core.base,
  mormot.net.ldap,
  ucommon,
  ucoredatamodule,
  ursatldapclientui,
  uvisnewobject;

{$R *.lfm}

procedure TFrmNewMsDNSServerSettings.Load;
begin
  Edit_cn.SetFocus;
end;

procedure TFrmNewMsDNSServerSettings.Action_NextExecute(Sender: TObject);
var
  NewObject: TVisNewObject;
  AttList: TLdapAttributeList;
  Att: TLdapAttribute;
  DN: RawUtf8;
begin
  NewObject := (owner as TVisNewObject);
  AttList := TLdapAttributeList.Create();
  try
    Att := AttList.Add(atObjectClass, 'top');
    Att.Add('msDNS-ServerSettings');

    DN := FormatUtf8('CN=%,%', [Edit_cn.Text, NewObject.ObjectOU]);
    if not NewObject.Ldap.Add(DN, AttList) then
      Exit;
  finally
    FreeAndNil(Att);
  end;

  NewObject.ModalResult := mrOK;
end;

procedure TFrmNewMsDNSServerSettings.Action_NextUpdate(Sender: TObject);
begin
  Action_Next.Enabled := (Edit_cn.Text <> '');
end;

constructor TFrmNewMsDNSServerSettings.Create(TheOwner: TComponent);
var
  OwnerNewObject: TVisNewObject absolute TheOwner;
begin
  inherited Create(TheOwner);

  OwnerNewObject.Caption := rsNewObjectMsDNSServerSettings;
  OwnerNewObject.Btn_Next.Action := ActionList.ActionByName('Action_Next');
  OwnerNewObject.Btn_Next.Caption := rsNewObjectBtnOK;
  OwnerNewObject.Btn_Next.Default := True;
  OwnerNewObject.Btn_Back.Visible := False;
  OwnerNewObject.CallBack := @Load;
end;

end.

