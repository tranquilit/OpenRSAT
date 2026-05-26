unit ufrmpropertygeneralsitelink;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  ExtCtrls,
  StdCtrls,
  Spin,
  Dialogs,
  mormot.core.base,
  mormot.core.log,
  mormot.core.text,
  mormot.net.ldap,
  uhelpersui,
  uproperty,
  ursatldapclient,
  upropertyframe;

type
  { TFrmPropertyGeneralSiteLink }
  TFrmPropertyGeneralSiteLink = class(TPropertyFrame)
    Button_Schedule: TButton;
    Button_Add: TButton;
    Button_Remove: TButton;
    Edit_Name: TEdit;
    Edit_Description: TEdit;
    Image_Logo: TImage;
    Label_Cost: TLabel;
    Label_Minutes: TLabel;
    Label_Replicate: TLabel;
    Label_NotInSiteLink: TLabel;
    Label_InSiteLink: TLabel;
    Label_Description: TLabel;
    ListBox_NotInSiteLink: TListBox;
    ListBox_InSiteLink: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Line_Header: TShape;
    SpinEdit_Replicate: TSpinEdit;
    SpinEdit_Cost: TSpinEdit;
    procedure Button_AddClick(Sender: TObject);
    procedure Button_RemoveClick(Sender: TObject);
    procedure Edit_DescriptionChange(Sender: TObject);
    procedure ListBox_InSiteLinkSelectionChange(Sender: TObject; User: boolean);
    procedure ListBox_NotInSiteLinkSelectionChange(Sender: TObject; User: boolean);
    procedure SpinEdit_CostChange(Sender: TObject);
    procedure SpinEdit_ReplicateChange(Sender: TObject);
    
type
  { LDAP Result }
  TLdapResultArray = array of TLdapResult;
  
  private
    fLog: TSynLog;
    fLdap: TRsatLdapClient;
    fProperty: TProperty;

    fNotInSite, fInSite: TLdapResultArray;
    procedure GetAllSites;
    procedure AddItemInResultArray(var List: TLdapResultArray; Item: TLdapResult);
    procedure LoadListBox;
    procedure PrepareListBox;
    procedure MoveItemToInSite(Index: Integer);
    procedure MoveItemToNotInSite(Index: Integer);
    procedure RemoveItemFromArray(var List: TLdapResultArray; Index: Integer);
    function SearchSitesInLdap: boolean;
    function GetResultName(Obj: TLdapResult): RawUtf8;
    function GetSitesInSiteLink: TLdapResultArray;
    function GetSitesNotInSiteLink: TLdapResultArray;
    
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Update(Props: TProperty); override;
  end;

implementation

{$R *.lfm}

procedure TFrmPropertyGeneralSiteLink.Button_AddClick(Sender: TObject);
var
  idx: Integer;
begin
  idx := ListBox_NotInSiteLink.ItemIndex;
  if idx <> -1 then
  begin
    MoveItemToInSite(idx);
    fProperty.Add('siteList', fInSite[Length(fInSite) - 1].Find('distinguishedName').GetReadable(), aoNoDuplicateValue);
    LoadListBox;
  end;
end;

procedure TFrmPropertyGeneralSiteLink.Button_RemoveClick(Sender: TObject);
var
  idx, n: Integer;
begin
  idx := ListBox_InSiteLink.ItemIndex;
  if idx <> -1 then
  begin
    MoveItemToNotInSite(idx);
    if Length(fInSite) > 0 then
    begin
      fProperty.Add('siteList', fInSite[0].Find('distinguishedName').GetReadable());
      for n := 1 to High(fInSite) do
        fProperty.Add('siteList', fInSite[n].Find('distinguishedName').GetReadable(), aoNoDuplicateValue);
    end
    else
      fProperty.Add('siteList', '');

    LoadListBox;
  end;
end;

procedure TFrmPropertyGeneralSiteLink.Edit_DescriptionChange(Sender: TObject);
begin
  fProperty.Add('description', Edit_Description.Text);
end;

procedure TFrmPropertyGeneralSiteLink.ListBox_InSiteLinkSelectionChange(
  Sender: TObject; User: boolean);
begin
  Button_Remove.Enabled := True;
  Button_Add.Enabled := False;
end;

procedure TFrmPropertyGeneralSiteLink.ListBox_NotInSiteLinkSelectionChange(
  Sender: TObject; User: boolean);
begin
  Button_Add.Enabled := True;
  Button_Remove.Enabled := False;
end;

procedure TFrmPropertyGeneralSiteLink.SpinEdit_CostChange(Sender: TObject);
begin
  fProperty.Add('cost', FloatToStr(SpinEdit_Cost.Value));
end;

procedure TFrmPropertyGeneralSiteLink.SpinEdit_ReplicateChange(Sender: TObject);
begin
  fProperty.Add('replInterval', FloatToStr(SpinEdit_Replicate.Value));
end;

constructor TFrmPropertyGeneralSiteLink.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);

  Caption := 'General';
end;

procedure TFrmPropertyGeneralSiteLink.Update(Props: TProperty);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Update', Self);

  fProperty := Props;
  fLdap := fProperty.LdapClient;
  
  GetAllSites;
  PrepareListBox;

  Edit_Name.CaptionNoChange := fProperty.name;
  Edit_Description.CaptionNoChange := fProperty.description;
  SpinEdit_Cost.Value := StrToFloat(fProperty.Attributes.Find('cost').GetReadable());
  SpinEdit_Replicate.Value := StrToFloat(fProperty.Attributes.Find('replInterval').GetReadable());
  LoadListBox;
end;

procedure TFrmPropertyGeneralSiteLink.GetAllSites;
var
  LdapResult: TLdapResult;
begin
  fLdap.SearchBegin();
  fLdap.SearchScope := lssSingleLevel;
  repeat
    if not SearchSitesInLdap then
      Exit;
    
    for LdapResult in fLdap.SearchResult.Items do
      AddItemInResultArray(fNotInSite, LdapResult);
  until fLdap.SearchCookie = '';
  fLdap.SearchEnd;
end;

function TFrmPropertyGeneralSiteLink.SearchSitesInLdap: boolean;
begin
  Result := fLdap.Search(FormatUtf8('CN=Sites,%', [fLdap.ConfigDN]), false, FormatUtf8('(&(objectClass=site))', []), ['name', 'distinguishedName'])
end;

procedure TFrmPropertyGeneralSiteLink.AddItemInResultArray(var List: TLdapResultArray; Item: TLdapResult);
var
  ListLen: Integer;
begin
  if not Assigned(Item) then
    exit;
  
  ListLen := Length(List);
  SetLength(List, ListLen + 1);
  List[ListLen] := TLdapResult(Item.Clone);
end;

function TFrmPropertyGeneralSiteLink.GetResultName(Obj: TLdapResult): RawUtf8;
begin
  Result := Obj.Find('name').GetReadable();
end;

function TFrmPropertyGeneralSiteLink.GetSitesInSiteLink: TLdapResultArray;
begin
  Result := fInSite;
end;

function TFrmPropertyGeneralSiteLink.GetSitesNotInSiteLink: TLdapResultArray;
begin
  Result := fNotInSite;
end;

procedure TFrmPropertyGeneralSiteLink.LoadListBox;
var
  r: TLdapResult;
begin
  ListBox_NotInSiteLink.Clear;
  for r in GetSitesNotInSiteLink do
    ListBox_NotInSiteLink.Items.Add(GetResultName(r));

  ListBox_InSiteLink.Clear;
  for r in GetSitesInSiteLink do
    ListBox_InSiteLink.Items.Add(GetResultName(r));
end;

procedure TFrmPropertyGeneralSiteLink.PrepareListBox;
var
  SiteList: TLdapAttribute;
  Site: RawUtf8;
  n: Integer;
begin
  SiteList := fProperty.Attributes.Find('siteList');
  if not Assigned(SiteList) then
    exit;

  n := Length(fNotInSite) - 1;
  while n >= 0 do
  begin
    for Site in SiteList.GetAllReadable do
    begin
      if fNotInSite[n].Attributes.Find('distinguishedName').GetReadable() = Site then
      begin
        MoveItemToInSite(n);
        break;
      end;
    end;

    Dec(n);
  end
end;


procedure TFrmPropertyGeneralSiteLink.MoveItemToInSite(Index: Integer);
begin
  SetLength(fInSite, Length(fInSite) + 1);
  fInSite[High(fInSite)] := fNotInSite[Index];
  RemoveItemFromArray(fNotInSite, Index);
end;


procedure TFrmPropertyGeneralSiteLink.MoveItemToNotInSite(Index: Integer);
begin
  SetLength(fNotInSite, Length(fNotInSite) + 1);
  fNotInSite[High(fNotInSite)] := fInSite[Index];
  RemoveItemFromArray(fInSite, Index);
end;


procedure TFrmPropertyGeneralSiteLink.RemoveItemFromArray(var List: TLdapResultArray; Index: Integer);
var
  i: Integer;
begin
  for i := Index to High(List) - 1 do
    List[i] := List[i + 1];

  SetLength(List, Length(List) - 1);
end;

end.

