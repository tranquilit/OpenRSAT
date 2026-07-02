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
  udoublelistlogic,
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
    fLogic: TNewSiteLinkPresenter;
    fLdap: TRsatLdapClient;
    fObjectOU: RawUtf8;

    procedure Load;
    procedure LoadListBox;
  public
    constructor Create(TheOwner: TComponent; ALdap: TRsatLdapClient; ObjectOU: RawUtf8); reintroduce;
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
  fLogic.CreateSiteLink(Edit_Name.Text);
end;

procedure TFrmNewSiteLink.Action_NextUpdate(Sender: TObject);
begin
  Action_Next.Enabled := fLogic.CanCreateSiteLink(Edit_Name.Text);
end;

procedure TFrmNewSiteLink.Button_AddClick(Sender: TObject);
var
  idx: Integer;
begin
  idx := ListBox_NotInSiteLink.ItemIndex;
  if ListBox_NotInSiteLink.ItemIndex <> -1 then
  begin
    fLogic.MoveItem(msInResult, idx);
    LoadListBox;
  end;
end;

procedure TFrmNewSiteLink.Button_RemoveClick(Sender: TObject);
var
  idx: Integer;
begin
  idx := ListBox_InSiteLink.ItemIndex;
  if ListBox_InSiteLink.ItemIndex <> -1 then
  begin
    fLogic.MoveItem(msOutOfResult, idx);
    LoadListBox;
  end;
end;

procedure TFrmNewSiteLink.ListBox_InSiteLinkSelectionChange(Sender: TObject; User: boolean);
begin
  Button_Remove.Enabled := True;
  Button_Add.Enabled := False;
end;

procedure TFrmNewSiteLink.ListBox_NotInSiteLinkSelectionChange(Sender: TObject; User: boolean);
begin
  Button_Add.Enabled := True;
  Button_Remove.Enabled := False;
end;

constructor TFrmNewSiteLink.Create(TheOwner: TComponent; ALdap: TRsatLdapClient; ObjectOU: RawUtf8);
var
  OwnerNewObject: TVisNewObject absolute TheOwner;
begin
  inherited Create(TheOwner);

  fLdap := ALdap;
  fObjectOU := ObjectOU;

  fLogic := TNewSiteLinkPresenter.Create(fLdap, fObjectOU);

  OwnerNewObject.Caption := rsNewObjectSiteLink;
  OwnerNewObject.Btn_Next.Action := ActionList.ActionByName('Action_Next');
  OwnerNewObject.Btn_Next.Caption := rsNewObjectBtnOK;
  OwnerNewObject.Btn_Next.ModalResult := mrOK;
  OwnerNewObject.Btn_Next.Default := True;
  OwnerNewObject.Btn_Back.Visible := False;
  OwnerNewObject.Image_Object.ImageIndex := Ord(ileADUnknown);
  OwnerNewObject.CallBack := @Load;
  
  fLogic.GetAllResources;
  if Length(fLogic.OutResult) < 2 then
  begin
    MessageDlg(rsTooFewSitesAvailableForSiteLink, mtError,[mbOK], 0);
    if Length(fLogic.OutResult) > 0 then
      fLogic.MoveItem(msInResult, 0);
  end
  else if Length(fLogic.InResult) = 2 then
  begin
    fLogic.MoveItem(msInResult, 0);
    fLogic.MoveItem(msInResult, 0);
  end;
  
  LoadListBox;
end;

destructor TFrmNewSiteLink.Destroy;
begin
  FreeAndNil(fLogic);
  inherited Destroy;
end;

procedure TFrmNewSiteLink.LoadListBox;
var
  r: TLdapResult;
begin
  ListBox_NotInSiteLink.Clear;
  for r in fLogic.OutResult do
    ListBox_NotInSiteLink.Items.Add(fLogic.GetResultName(r));

  ListBox_InSiteLink.Clear;
  for r in fLogic.InResult do
    ListBox_InSiteLink.Items.Add(fLogic.GetResultName(r));
end;

end.

