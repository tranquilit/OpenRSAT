unit ufrmnewserver;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  StdCtrls,
  ActnList, ExtCtrls,
  tis.ui.grid.core,
  mormot.core.base,
  mormot.net.ldap,
  ursatldapclient;

type

  { TFrmNewServer }

  TFrmNewServer = class(TFrame)
    Action_Next: TAction;
    ActionList: TActionList;
    Edit_Name: TEdit;
    Label_Name: TLabel;
    Panel1: TPanel;
    procedure Action_NextExecute(Sender: TObject);
    procedure Action_NextUpdate(Sender: TObject);
  private
    fLdap: TRsatLdapClient;
  public
    constructor Create(TheOwner: TComponent; ALdap: TRsatLdapClient); reintroduce;
  end;

implementation

uses
  lclintf,
  StrUtils,
  mormot.net.sock,
  mormot.core.variants,
  ucommon,
  ucommonui,
  ucoredatamodule,
  ursatldapclientui,
  uvisnewobject;

{$R *.lfm}

procedure TFrmNewServer.Action_NextExecute(Sender: TObject);
var
  DistinguishedName: String;
  AttributeList: TLdapAttributeList;
  Attribute: TLdapAttribute;
begin
  DistinguishedName := Format('CN=%s,CN=Servers,CN=Default-First-Site-Name,CN=Sites,%s', [LdapEscape(Edit_Name.Text), fLdap.ConfigDN]);  // Need some change about path
  AttributeList := TLdapAttributeList.Create;

  try
    Attribute := AttributeList.Add('objectClass', 'top');
    Attribute.Add('server');

    if not fLdap.Add(DistinguishedName, AttributeList) then
      Exit;
  finally
    FreeAndNil(AttributeList);
  end;
end;

procedure TFrmNewServer.Action_NextUpdate(Sender: TObject);
begin
  Action_Next.Enabled := (Edit_Name.Text <> '');
end;

constructor TFrmNewServer.Create(TheOwner: TComponent; ALdap: TRsatLdapClient);
var
  OwnerNewObject: TVisNewObject absolute TheOwner;
begin
  inherited Create(TheOwner);

  fLdap := ALdap;

  OwnerNewObject.Caption := rsNewObjectServer;
  OwnerNewObject.Btn_Next.Action := ActionList.ActionByName('Action_Next');
  OwnerNewObject.Btn_Next.Caption := rsNewObjectBtnOK;
  OwnerNewObject.Btn_Next.ModalResult := mrOK;
  OwnerNewObject.Btn_Next.Default := True;
  OwnerNewObject.Btn_Back.Visible := False;
  OwnerNewObject.Image_Object.ImageIndex := Ord(ileADUnknown);
end;

end.

