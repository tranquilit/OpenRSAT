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
  udoublelistlogic,
  unewsitelinkbridge,
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
    procedure ListBox_InSiteLinkBridgeSelectionChange(Sender: TObject;User: boolean);
    procedure ListBox_NotInSiteLinkBridgeSelectionChange(Sender: TObject;User: boolean);

  private
    fLogic: TNewSiteLinkBridgePresenter;
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
  ucommon,
  ucoredatamodule,
  uvisnewobject;

{$R *.lfm}

procedure TFrmNewSiteLinkBridge.Action_NextExecute(Sender: TObject);
begin
  fLogic.CreateSiteLinkBridge(Edit_Name.Text);
end;

procedure TFrmNewSiteLinkBridge.Action_NextUpdate(Sender: TObject);
begin
  Action_Next.Enabled := fLogic.CanCreateSiteLinkBridge(Edit_Name.Text);
end;

procedure TFrmNewSiteLinkBridge.Button_AddClick(Sender: TObject);
var
  idx: Integer;
begin
  idx := ListBox_NotInSiteLinkBridge.ItemIndex;
  if ListBox_NotInSiteLinkBridge.ItemIndex <> -1 then
  begin
    fLogic.MoveItem(msInResult, idx);
    LoadListBox;
  end;
end;

procedure TFrmNewSiteLinkBridge.Button_RemoveClick(Sender: TObject);
var
  idx: Integer;
begin
  idx := ListBox_InSiteLinkBridge.ItemIndex;
  if ListBox_InSiteLinkBridge.ItemIndex <> -1 then
  begin
    fLogic.MoveItem(msOutOfResult, idx);
    LoadListBox;
  end;
end;

procedure TFrmNewSiteLinkBridge.ListBox_InSiteLinkBridgeSelectionChange(Sender: TObject; User: boolean);
begin
  Button_Remove.Enabled := True;
  Button_Add.Enabled := False;
end;

procedure TFrmNewSiteLinkBridge.ListBox_NotInSiteLinkBridgeSelectionChange(Sender: TObject; User: boolean);
begin
  Button_Add.Enabled := True;
  Button_Remove.Enabled := False;
end;

procedure TFrmNewSiteLinkBridge.Load;
begin
  Edit_Name.SetFocus;
end;

constructor TFrmNewSiteLinkBridge.Create(TheOwner: TComponent; ALdap: TRsatLdapClient; ObjectOU: RawUtf8);
var
  OwnerNewObject: TVisNewObject absolute TheOwner;
begin
  inherited Create(TheOwner);

  fLdap := ALdap;
  fObjectOU := ObjectOU;

  fLogic := TNewSiteLinkBridgePresenter.Create(fLdap, fObjectOU);

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
    fLogic.MoveItem(msInResult, 0);;
    fLogic.MoveItem(msInResult, 0);
  end;

  LoadListBox;
end;

destructor TFrmNewSiteLinkBridge.Destroy;
begin
  FreeAndNil(fLogic);
  inherited Destroy;
end;

procedure TFrmNewSiteLinkBridge.LoadListBox;
var
  r: TLdapResult;
begin
  ListBox_NotInSiteLinkBridge.Clear;
  for r in fLogic.OutResult do
    ListBox_NotInSiteLinkBridge.Items.Add(fLogic.GetResultName(r));

  ListBox_InSiteLinkBridge.Clear;
  for r in fLogic.InResult do
    ListBox_InSiteLinkBridge.Items.Add(fLogic.GetResultName(r));
end;

end.

