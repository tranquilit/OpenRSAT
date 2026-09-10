unit ufrmnewpasswordsettings;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  ActnList,
  mormot.net.ldap;

type

  { TFrmNewPasswordSettings }

  TFrmNewPasswordSettings = class(TFrame)
    Action_OK: TAction;
    ActionList1: TActionList;
  private
    fLdap: TLdapClient;
  public
    constructor Create(TheOwner: TComponent; ALdap: TLdapClient); reintroduce;
  end;

implementation
uses
  ucommon,
  uvisnewobject;

{$R *.lfm}

{ TFrmNewPasswordSettings }

constructor TFrmNewPasswordSettings.Create(TheOwner: TComponent;
  ALdap: TLdapClient);
var
  OwnerNewObject: TVisNewObject absolute TheOwner;
begin
  inherited Create(TheOwner);

  fLdap := ALdap;

  OwnerNewObject.Caption := rsNewObjectPasswordSettings;
  OwnerNewObject.Btn_Next.Action := Action_OK;
  OwnerNewObject.Btn_Next.Caption := rsNewObjectBtnOK;
  OwnerNewObject.Btn_Next.Default := True;
  OwnerNewObject.Btn_Back.Visible := False;
  OwnerNewObject.Image_Object.ImageIndex := -1;
end;

end.

