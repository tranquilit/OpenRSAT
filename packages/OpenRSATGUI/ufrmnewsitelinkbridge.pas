unit ufrmnewsitelinkbridge;

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
  
  { TFrmNewSiteLinkBridge }

  TFrmNewSiteLinkBridge = class(TFrame)
    Action_Next: TAction;
    ActionList: TActionList;
    Button_Add: TButton;
    Button_Remove: TButton;
    ListBox_InSiteLinkBridge: TListBox;
    ListBox_NotInSiteLinkBridge: TListBox;
    Edit_Name: TEdit;
    Label_LeftListBox: TLabel;
    Label_RightListBox: TLabel;
    Label_Warning: TLabel;
    Label_Name: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    procedure Action_NextExecute(Sender: TObject);
    procedure Action_NextUpdate(Sender: TObject);
    procedure Button_AddClick(Sender: TObject);
    procedure Button_RemoveClick(Sender: TObject);
    procedure ListBox_InSiteLinkBridgeSelectionChange(Sender: TObject; 
      User: boolean);
    procedure ListBox_NotInSiteLinkBridgeSelectionChange(Sender: TObject; 
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

procedure TFrmNewSiteLinkBridge.Action_NextExecute(Sender: TObject);
var
  DistinguishedName: RawUtf8;
  AttributeList: TLdapAttributeList;
  Attribute: TLdapAttribute;
begin
  DistinguishedName := Format('CN=%s,CN=SMTP,CN=Inter-Site Transports,CN=Sites,%s', [Edit_Name.Text, fLdap.ConfigDN]);
  AttributeList := TLdapAttributeList.Create;

  try
    Attribute := AttributeList.Add('objectClass', 'top');
    Attribute.Add('siteLinkBridge');

    if not fLdap.Add(DistinguishedName, AttributeList) then
      Exit;
  finally
    FreeAndNil(AttributeList);
  end;
end;

procedure TFrmNewSiteLinkBridge.Action_NextUpdate(Sender: TObject);
begin
  Action_Next.Enabled := (Edit_Name.Text <> '');
end;

procedure TFrmNewSiteLinkBridge.Button_AddClick(Sender: TObject);
var
  idx: Integer;
begin
  idx := ListBox_NotInSiteLinkBridge.ItemIndex;
  if idx <> -1 then
  begin
    ListBox_InSiteLinkBridge.Items.Add(ListBox_NotInSiteLinkBridge.Items[idx]);
    ListBox_InSiteLinkBridge.ItemIndex := ListBox_InSiteLinkBridge.Items.Count - 1;
    ListBox_InSiteLinkBridge.SetFocus;
    ListBox_NotInSiteLinkBridge.Items.Delete(idx);
  end;
end;

procedure TFrmNewSiteLinkBridge.Button_RemoveClick(Sender: TObject);
var
  idx: Integer;
begin
  idx := ListBox_InSiteLinkBridge.ItemIndex;
  if idx <> -1 then
  begin
    ListBox_NotInSiteLinkBridge.Items.Add(ListBox_InSiteLinkBridge.Items[idx]);
    ListBox_NotInSiteLinkBridge.ItemIndex := ListBox_NotInSiteLinkBridge.Items.Count - 1;
    ListBox_NotInSiteLinkBridge.SetFocus;
    ListBox_InSiteLinkBridge.Items.Delete(idx);
  end;
end;

procedure TFrmNewSiteLinkBridge.ListBox_InSiteLinkBridgeSelectionChange(
  Sender: TObject; User: boolean);
begin
  Button_Remove.Enabled := True; 
  Button_Add.Enabled := False;
end;

procedure TFrmNewSiteLinkBridge.ListBox_NotInSiteLinkBridgeSelectionChange(
  Sender: TObject; User: boolean);
begin
  Button_Add.Enabled := True; 
  Button_Remove.Enabled := False;
end; 

procedure TFrmNewSiteLinkBridge.Load;
begin
  Edit_Name.SetFocus;
end; 

constructor TFrmNewSiteLinkBridge.Create(TheOwner: TComponent; ALdap: TRsatLdapClient);
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

