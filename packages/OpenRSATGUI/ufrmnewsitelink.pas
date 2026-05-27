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
  ursatldapclient,
  unewsitelink;

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
    procedure ListBox_NotInSiteLinkSelectionChange(Sender: TObject; User: boolean);
  
type
  { Ldap Result }
  TLdapResultArray = array of TLdapResult;
    
  private
    fNewSiteLinkLogic: TNewSiteLinkPresenter;

    procedure Load;
    procedure LoadListBox;
    function GetName: RawUtf8;
  public
    constructor Create(TheOwner: TComponent; ALdap: TRsatLdapClient); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  lclintf,
  mormot.net.sock,
  mormot.core.variants,
  mormot.core.text,
  ucommon,
  ucoredatamodule,
  uvisnewobject;

{$R *.lfm}

procedure TFrmNewSiteLink.Load;
begin
  Edit_Name.SetFocus;
end;

procedure TFrmNewSiteLink.Action_NextExecute(Sender: TObject);
begin
  fNewSiteLinkLogic.CreateSiteLink(GetName);
end;

procedure TFrmNewSiteLink.Action_NextUpdate(Sender: TObject);
begin
  Action_Next.Enabled := fNewSiteLinkLogic.CanCreateSiteLink(GetName);
end;

procedure TFrmNewSiteLink.Button_AddClick(Sender: TObject);
begin
  if ListBox_NotInSiteLink.ItemIndex <> -1 then
  begin
    fNewSiteLinkLogic.MoveItemToInSite(ListBox_NotInSiteLink.ItemIndex);
    LoadListBox;
  end;
end;

procedure TFrmNewSiteLink.Button_RemoveClick(Sender: TObject);
begin
  if ListBox_InSiteLink.ItemIndex <> -1 then
  begin
    fNewSiteLinkLogic.MoveItemToNotInSite(ListBox_InSiteLink.ItemIndex);
    LoadListBox;
  end;
end;

procedure TFrmNewSiteLink.ListBox_InSiteLinkSelectionChange(Sender: TObject; User: boolean);
begin
  if fNewSiteLinkLogic.GetNbSites < 3 then
    Exit;

  Button_Remove.Enabled := True;
  Button_Add.Enabled := False;
end;

procedure TFrmNewSiteLink.ListBox_NotInSiteLinkSelectionChange(Sender: TObject; User: boolean);
begin
  if fNewSiteLinkLogic.GetNbSites < 3 then
    Exit;

  Button_Add.Enabled := True;
  Button_Remove.Enabled := False;
end;

constructor TFrmNewSiteLink.Create(TheOwner: TComponent; ALdap: TRsatLdapClient);
var
  OwnerNewObject: TVisNewObject absolute TheOwner;
begin
  inherited Create(TheOwner);

  fNewSiteLinkLogic := TNewSiteLinkPresenter.Create(ALdap);

  OwnerNewObject.Caption := rsNewObjectSiteLink;
  OwnerNewObject.Btn_Next.Action := ActionList.ActionByName('Action_Next');
  OwnerNewObject.Btn_Next.Caption := rsNewObjectBtnOK;
  OwnerNewObject.Btn_Next.ModalResult := mrOK;
  OwnerNewObject.Btn_Next.Default := True;
  OwnerNewObject.Btn_Back.Visible := False;
  OwnerNewObject.Image_Object.ImageIndex := Ord(ileADUnknown);
  OwnerNewObject.CallBack := @Load;
  
  fNewSiteLinkLogic.GetAllSites;
  
  if Length(fNewSiteLinkLogic.GetSitesNotInSiteLink) < 2 then
  begin
    MessageDlg(rsTooFewSitesAvailableForSiteLink, mtError,[mbOK], 0);
    if Length(fNewSiteLinkLogic.GetSitesNotInSiteLink) > 0 then
      fNewSiteLinkLogic.MoveItemToInSite(0);
  end
  else if Length(fNewSiteLinkLogic.GetSitesNotInSiteLink) = 2 then
  begin
    fNewSiteLinkLogic.MoveItemToInSite(0);
    fNewSiteLinkLogic.MoveItemToInSite(0);
  end;
  
  LoadListBox;
end;

destructor TFrmNewSiteLink.Destroy;
begin
  FreeAndNil(fNewSiteLinkLogic);
  inherited Destroy;
end;

procedure TFrmNewSiteLink.LoadListBox;
var
  r: TLdapResult;
begin
  ListBox_NotInSiteLink.Clear;
  for r in fNewSiteLinkLogic.GetSitesNotInSiteLink do
    ListBox_NotInSiteLink.Items.Add(fNewSiteLinkLogic.GetResultName(r));

  ListBox_InSiteLink.Clear;
  for r in fNewSiteLinkLogic.GetSitesInSiteLink do
    ListBox_InSiteLink.Items.Add(fNewSiteLinkLogic.GetResultName(r));
end;

function TFrmNewSiteLink.GetName: RawUtf8;
begin
  Result := Edit_Name.Text;
end;

end.

