unit ufrmnewsitelink;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  StdCtrls,
  Dialogs,
  ActnList, ExtCtrls,
  tis.ui.grid.core,
  mormot.core.base,
  mormot.net.ldap,
  ursatldapclient; 

type
  
  { TFrmNewSiteLink }

  TFrmNewSiteLink = class(TFrame)
    Action_Next: TAction;
    ActionList: TActionList;
    Button_Add: TButton;
    Button_Remove: TButton;
    Edit_Name: TEdit;
    Label_Name: TLabel;
    Label_LeftList: TLabel;
    Label_RightList: TLabel;
    Label_Warning: TLabel;
    ListBox_NotInSiteLink: TListBox;
    ListBox_InSiteLink: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    procedure Action_NextExecute(Sender: TObject);
    procedure Action_NextUpdate(Sender: TObject);
    procedure Button_AddClick(Sender: TObject);
    procedure Button_RemoveClick(Sender: TObject);
    procedure ListBox_InSiteLinkSelectionChange(Sender: TObject; User: boolean);
    procedure ListBox_NotInSiteLinkSelectionChange(Sender: TObject; 
      User: boolean);
  private
    fLdap: TRsatLdapClient;
    procedure Load;
  public
    constructor Create(TheOwner: TComponent; ALdap: TRsatLdapClient); reintroduce;
  end;

implementation

uses
  lclintf,
  mormot.net.sock,
  mormot.core.variants,
  ucommon,
  ucoredatamodule,
  uvisnewobject;

{$R *.lfm}

procedure TFrmNewSiteLink.Load;
begin
  Edit_Name.SetFocus;
end;

procedure TFrmNewSiteLink.Action_NextExecute(Sender: TObject);
var
  DistinguishedName: String;
  AttributeList: TLdapAttributeList;
  Attribute: TLdapAttribute;
begin
  DistinguishedName := Format('CN=%s,CN=SMTP,CN=Inter-Site Transports,CN=Sites,%s', [LdapEscape(Edit_Name.Text), fLdap.ConfigDN]);
  AttributeList := TLdapAttributeList.Create;

  try
    Attribute := AttributeList.Add('objectClass', 'top');
    Attribute.Add('siteLink');

    if not fLdap.Add(DistinguishedName, AttributeList) then
      Exit;
  finally
    FreeAndNil(AttributeList);
  end;
end;

procedure TFrmNewSiteLink.Action_NextUpdate(Sender: TObject);
begin
  Action_Next.Enabled := (Edit_Name.Text <> '');
end;

procedure TFrmNewSiteLink.Button_AddClick(Sender: TObject);
var
  idx: Integer;
begin
  idx := ListBox_NotInSiteLink.ItemIndex;
  if idx <> -1 then
  begin
    ListBox_InSiteLink.Items.Add(ListBox_NotInSiteLink.Items[idx]);
    ListBox_InSiteLink.ItemIndex := ListBox_InSiteLink.Items.Count - 1;
    ListBox_InSiteLink.SetFocus;
    ListBox_NotInSiteLink.Items.Delete(idx);
  end;
end;

procedure TFrmNewSiteLink.Button_RemoveClick(Sender: TObject);
var
  idx: Integer;
begin
  idx := ListBox_InSiteLink.ItemIndex;
  if idx <> -1 then
  begin
    ListBox_NotInSiteLink.Items.Add(ListBox_InSiteLink.Items[idx]);
    ListBox_NotInSiteLink.ItemIndex := ListBox_NotInSiteLink.Items.Count - 1;
    ListBox_NotInSiteLink.SetFocus;
    ListBox_InSiteLink.Items.Delete(idx);
  end;
end;

procedure TFrmNewSiteLink.ListBox_InSiteLinkSelectionChange(Sender: TObject; 
  User: boolean);
begin
  Button_Remove.Enabled := True; 
  Button_Add.Enabled := False;
end;

procedure TFrmNewSiteLink.ListBox_NotInSiteLinkSelectionChange(Sender: TObject; 
  User: boolean);
begin
  Button_Add.Enabled := True; 
  Button_Remove.Enabled := False;
end;

constructor TFrmNewSiteLink.Create(TheOwner: TComponent; ALdap: TRsatLdapClient);
var
  OwnerNewObject: TVisNewObject absolute TheOwner;
begin
  inherited Create(TheOwner);

  fLdap := ALdap;

  OwnerNewObject.Caption := rsNewObjectSiteLink;
  OwnerNewObject.Btn_Next.Action := ActionList.ActionByName('Action_Next');
  OwnerNewObject.Btn_Next.Caption := rsNewObjectBtnOK;
  OwnerNewObject.Btn_Next.ModalResult := mrOK;
  OwnerNewObject.Btn_Next.Default := True;
  OwnerNewObject.Btn_Back.Visible := False;
  OwnerNewObject.Image_Object.ImageIndex := Ord(ileADUnknown);
  OwnerNewObject.CallBack := @Load;
end; 

end.

