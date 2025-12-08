unit ufrmnewsite;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  StdCtrls,
  ActnList,
  ExtCtrls,
  tis.ui.grid.core,
  mormot.net.ldap,
  mormot.core.base;

type

  { TFrmNewSite }

  TFrmNewSite = class(TFrame)
    Action_ok: TAction;
    ActionList1: TActionList;
    Edit1: TEdit;
    Label_Name: TLabel;
    Label_Info: TLabel;
    Timer_SearchInGrid: TTimer;
    TisGrid1: TTisGrid;
    procedure Action_okExecute(Sender: TObject);
    procedure Action_okUpdate(Sender: TObject);
    procedure Timer_SearchInGridTimer(Sender: TObject);
    procedure TisGrid1KeyPress(Sender: TObject; var Key: char);
  private
    fLdap: TLdapClient;

    fSearchWord: RawUtf8;

    procedure Load;
    procedure LoadSiteLinks;
  public
    constructor Create(TheOwner: TComponent; ALdap: TLdapClient); reintroduce;
  end;

implementation
uses
  mormot.core.variants,
  ucommon,
  ucoredatamodule,
  ursatldapclient,
  uvisnewobject;

{$R *.lfm}

{ TFrmNewSite }

procedure TFrmNewSite.Action_okExecute(Sender: TObject);
var
  AttributeList: TLdapAttributeList;
  Attribute: TLdapAttribute;
  NodeData: PDocVariantData;
  DistinguishedName: String;
begin
  // Create site object
  AttributeList := TLdapAttributeList.Create;
  try
    Attribute := AttributeList.Add('objectClass', 'top');
    Attribute.Add('site');
    DistinguishedName := Format('CN=%s,CN=Sites,%s', [LdapEscape(Edit1.Text), fLdap.ConfigDN]);
    if not fLdap.Add(DistinguishedName, AttributeList) then
    begin
      ShowLdapAddError(fLdap);
      Exit;
    end;
  finally
    FreeAndNil(AttributeList);
  end;

  // Create server container
  AttributeList := TLdapAttributeList.Create;
  try
    Attribute := AttributeList.Add('objectClass', 'top');
    Attribute.Add('serversContainer');
    if not fLdap.Add(Format('CN=Servers,%s', [DistinguishedName]), AttributeList) then
    begin
      ShowLdapAddError(fLdap);
      Exit;
    end;
  finally
    FreeAndNil(AttributeList);
  end;

  // Create site settings
  AttributeList := TLdapAttributeList.Create;
  try
    Attribute := AttributeList.Add('objectClass', 'top');
    Attribute.Add('applicationSiteSettings');
    Attribute.Add('nTDSSiteSettings');
    if not fLdap.Add(Format('CN=NTDS Site Settings,%s', [DistinguishedName]), AttributeList) then
    begin
      ShowLdapAddError(fLdap);
      Exit;
    end;
  finally
    FreeAndNil(AttributeList);
  end;

  // Add site object to site link list
  Attribute := TLdapAttribute.Create('siteList', atUndefined);
  try
    NodeData := TisGrid1.GetNodeAsPDocVariantData(TisGrid1.GetFirstSelected);
    Attribute.Add(DistinguishedName);
    if not fLdap.Modify(NodeData^.S['distinguishedName'], lmoAdd, Attribute) then
    begin
      ShowLdapModifyError(fLdap);
      Exit;
    end;
  finally
    FreeAndNil(Attribute);
  end;
end;

procedure TFrmNewSite.Action_okUpdate(Sender: TObject);
begin
  Action_ok.Enabled := (Edit1.Text <> '') and (TisGrid1.SelectedCount = 1);
end;

procedure TFrmNewSite.Timer_SearchInGridTimer(Sender: TObject);
begin
  Timer_SearchInGrid.Enabled := False;
end;

procedure TFrmNewSite.TisGrid1KeyPress(Sender: TObject; var Key: char);
begin
  SearchInGrid(Timer_SearchInGrid, TisGrid1, fSearchWord, Key);
end;

procedure TFrmNewSite.Load;
begin
  Edit1.SetFocus;
end;

procedure TFrmNewSite.LoadSiteLinks;
var
  SearchResult: TLdapResult;
  LinkName, DistinguishedName: RawUtf8;
  Link: TDocVariantData;
begin
  Link.Init();
  fLdap.SearchBegin();
  TisGrid1.BeginUpdate;
  try
    fLdap.SearchScope := lssWholeSubtree;

    repeat
      if not fLdap.Search(fLdap.ConfigDN, False, '(objectClass=siteLink)', ['name', 'distinguishedName']) then
      begin
        ShowLdapSearchError(fLdap);
        Exit;
      end;

      for SearchResult in fLdap.SearchResult.Items do
      begin
        if not Assigned(SearchResult) then
          continue;

        LinkName := SearchResult.Find('name').GetReadable();
        DistinguishedName := SearchResult.Find('distinguishedName').GetReadable();
        Link.AddValue('distinguishedName', DistinguishedName);
        Link.AddValue('linkName', LinkName);
        Link.AddValue('transport', String(DistinguishedName).Split(',')[1].Split('=')[1]);
        TisGrid1.Data.AddItem(Link);
        Link.Clear;
      end;
    until fLdap.SearchCookie = '';
  finally
    TisGrid1.EndUpdate;
    fLdap.SearchEnd;
    TisGrid1.LoadData;
  end;
end;

constructor TFrmNewSite.Create(TheOwner: TComponent; ALdap: TLdapClient);
var
  OwnerNewObject: TVisNewObject absolute TheOwner;
begin
  inherited Create(TheOwner);

  Self.Parent := (TheOwner as TWinControl);
  fLdap := ALdap;
  LoadSiteLinks;

  OwnerNewObject.Caption := rsNewObjectSite;
  OwnerNewObject.Btn_Next.Action := ActionList1.ActionByName('Action_ok');
  OwnerNewObject.Btn_Next.Caption := rsNewObjectBtnOK;
  OwnerNewObject.Btn_Next.ModalResult := mrOK;
  OwnerNewObject.Btn_Next.Default := True;
  OwnerNewObject.Btn_Back.Visible := False;
  OwnerNewObject.Image_Object.ImageIndex := Ord(ileADUnknown);
  OwnerNewObject.CallBack := @Load;
end;

end.

