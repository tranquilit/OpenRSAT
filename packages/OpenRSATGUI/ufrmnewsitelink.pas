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
    procedure ListBox_NotInSiteLinkSelectionChange(Sender: TObject; User: boolean);
  
type
  { Ldap Result }
  TLdapResultArray = array of TLdapResult;
    
  private
    fLdap: TRsatLdapClient;

    fInSite: TLdapResultArray;
    fNotInSite: TLdapResultArray;

    procedure Load;
    procedure LoadListBox;
    procedure MoveItem(var Src, Dest: TLdapResultArray; Index: Integer);
  public
    constructor Create(TheOwner: TComponent; ALdap: TRsatLdapClient); reintroduce;
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
var
  DistinguishedName: String;
  AttributeList: TLdapAttributeList;
  Attribute: TLdapAttribute;
  i: Integer;
begin
  DistinguishedName := Format('CN=%s,CN=SMTP,CN=Inter-Site Transports,CN=Sites,%s', [Edit_Name.Text, fLdap.ConfigDN]);
  AttributeList := TLdapAttributeList.Create;

  try
    Attribute := AttributeList.Add('objectClass', 'top');
    Attribute.Add('siteLink');
    
    Attribute := AttributeList.Add('siteList', fInSite[0].Find('distinguishedName').GetReadable());
    for i := 1 to Length(fInSite) - 1 do
      Attribute.Add(fInSite[i].Find('distinguishedName').GetReadable());
    
    AttributeList.Add('cost', '100');
    AttributeList.Add('replInterval', '180');
    if not fLdap.Add(DistinguishedName, AttributeList) then
      Exit;
  finally
    FreeAndNil(AttributeList);
  end;
end;

procedure TFrmNewSiteLink.Action_NextUpdate(Sender: TObject);
begin
  if Length(fInSite) + Length(fNotInSite) > 2 then
    Action_Next.Enabled := (Edit_Name.Text <> '') and (Length(fInSite) >= 2)
  else
    Action_Next.Enabled := (Edit_Name.Text <> '');
end;

procedure TFrmNewSiteLink.Button_AddClick(Sender: TObject);
var
  idx: Integer;
begin
  idx := ListBox_NotInSiteLink.ItemIndex;
  if idx <> -1 then
    MoveItem(fNotInSite, fInSite, idx);
end;

procedure TFrmNewSiteLink.Button_RemoveClick(Sender: TObject);
var
  idx: Integer;
begin
  idx := ListBox_InSiteLink.ItemIndex;
  if idx <> -1 then
    MoveItem(fInSite, fNotInSite, idx);
end;

procedure TFrmNewSiteLink.ListBox_InSiteLinkSelectionChange(Sender: TObject;
  User: boolean);
begin
  if Length(fInSite) + Length(fNotInSite) < 3 then
    Exit;

  Button_Remove.Enabled := True;
  Button_Add.Enabled := False;
end;

procedure TFrmNewSiteLink.ListBox_NotInSiteLinkSelectionChange(Sender: TObject;
  User: boolean);
begin
  if Length(fInSite) + Length(fNotInSite) < 3 then
    Exit;

  Button_Add.Enabled := True;
  Button_Remove.Enabled := False;
end;

procedure TFrmNewSiteLink.MoveItem(var Src, Dest: TLdapResultArray; Index: Integer);
var
  i: Integer;
begin
  SetLength(Dest, Length(Dest) + 1);
  Dest[High(Dest)] := Src[Index];

  for i := Index to High(Src) - 1 do
    Src[i] := Src[i + 1];

  SetLength(Src, Length(Src) - 1);
  LoadListBox;
end;

procedure TFrmNewSiteLink.LoadListBox();
var
  ResultNotInSite, ResultInSite: TLdapResult;
begin
  ListBox_NotInSiteLink.Clear;
  ListBox_InSiteLink.Clear;
  
  for ResultNotInSite in fNotInSite do
  begin
    ListBox_NotInSiteLink.Items.Add(ResultNotInSite.Find('name').GetReadable());
  end;
  
  for ResultInSite in fInSite do
  begin
    ListBox_InSiteLink.Items.Add(ResultInSite.Find('name').GetReadable());
  end;
end;

constructor TFrmNewSiteLink.Create(TheOwner: TComponent; ALdap: TRsatLdapClient);
var
  OwnerNewObject: TVisNewObject absolute TheOwner;
  Result: TLdapResult;
  n: Integer;
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

  n := 0;
  fLdap.SearchBegin();
  fLdap.SearchScope := lssSingleLevel;
  
  repeat
    if not fLdap.Search(FormatUtf8('CN=Sites,%', [fLdap.ConfigDN]), false, FormatUtf8('(&(objectClass=site))', []), ['name', 'distinguishedName']) then
      Exit;
    
    for Result in fLdap.SearchResult.Items do
    begin
      if not Assigned(Result) then
        continue;
  
      SetLength(fNotInSite, n + 1);
      fNotInSite[n] := Result;
      n += 1;
    end;
  until fLdap.SearchCookie = '';
  fLdap.SearchEnd;
  
  if n < 2 then
  begin
    MessageDlg(rsTooFewSitesAvailableForSiteLink, mtError,[mbOK], 0);
    SetLength(fInSite, 1);
    fInSite[0] := fNotInSite[0];
    SetLength(fNotInSite, 0);
  end;

  if n = 2 then
  begin
    SetLength(fInSite, 2);
    fInSite[0] := fNotInSite[0];
    fInSite[1] := fNotInSite[1];
    SetLength(fNotInSite, 0);
  end;
  
  LoadListBox;
end;

end.

