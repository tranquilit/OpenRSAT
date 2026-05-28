unit ufrmpropertygeneralsitelinkbridge;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  ExtCtrls,
  StdCtrls,
  Dialogs,
  mormot.core.base,
  mormot.core.log,
  mormot.core.text,
  mormot.net.ldap,
  uhelpersui,
  uproperty,
  ursatldapclient,
  ulog;

type

  { TLdapResultArray }
  TLdapResultArray = array of TLdapResult;
  
  { TFrmPropertyGeneralSiteLinkBridge }  
  TFrmPropertyGeneralSiteLinkBridge = class(TPropertyFrame)
    Button_Add: TButton;
    Button_Remove: TButton;
    Edit_Name: TEdit;
    Edit_Description: TEdit;
    Image_Logo: TImage;
    Label_NotInSiteLinkBridge: TLabel;
    Label_InSiteLinkBridge: TLabel;
    Label_Description: TLabel;
    ListBox_NotInSiteLinkBridge: TListBox;
    ListBox_InSiteLinkBridge: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Line_Header: TShape;
    procedure Button_AddClick(Sender: TObject);
    procedure Button_RemoveClick(Sender: TObject);
  private
    fLog: TSynLogClass;
    fProperty: TProperty;
    fLdap: TRsatLdapClient;
    
    fInSite, fNotInSite: TLdapResultArray;
    
    procedure AddItemInResultArray(var List: TLdapResultArray; Item: TLdapResult);
    function ExtractGroup(const S: RawUtf8): RawUtf8;
    function SearchSitesInLdap: boolean;
    function GetSitesInSiteLink: TLdapResultArray;
    function GetSitesNotInSiteLink: TLdapResultArray;
    procedure RetrieveSiteLinks;
    function GetResultName(Obj: TLdapResult): RawUtf8;
    procedure LoadListBox;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Update(Props: TProperty); override;
  end;

implementation

{$R *.lfm}

procedure TFrmPropertyGeneralSiteLinkBridge.Button_AddClick(Sender: TObject);
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

procedure TFrmPropertyGeneralSiteLinkBridge.Button_RemoveClick(Sender: TObject);
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

procedure TFrmPropertyGeneralSiteLinkBridge.AddItemInResultArray(var List: TLdapResultArray; Item: TLdapResult);
var
  ListLen: Integer;
begin
  if not Assigned(Item) then
    exit;

  ListLen := Length(List);
  SetLength(List, ListLen + 1);
  List[ListLen] := TLdapResult(Item.Clone);
end;

function TFrmPropertyGeneralSiteLinkBridge.ExtractGroup(const S: RawUtf8): RawUtf8;
var
  p1, p2: Integer;
begin
  Result := '';

  p1 := Pos('CN=', S);
  if p1 = 0 then Exit;

  p1 := PosEx('CN=', S, p1 + 3);
  if p1 = 0 then Exit;

  p2 := PosEx(',', S, p1);
  if p2 = 0 then
    Result := Copy(S, p1, Length(S) - p1 + 1)
  else
    Result := Copy(S, p1, p2 - p1);
end;

function TFrmPropertyGeneralSiteLinkBridge.SearchSitesInLdap: boolean;
var
  Group: RawUtf8;
begin
  Group := ExtractGroup(fProperty.distinguishedName);
  Result := fLdap.Search(FormatUtf8('%,CN=Inter-Site Transports,CN=Sites,%', [Group, fLdap.ConfigDN]), false, '(&(objectClass=siteLink))', ['name', 'distinguishedName'])
end;

procedure TFrmPropertyGeneralSiteLinkBridge.RetrieveSiteLinks;
var
  LdapResult: TLdapResult;
begin
  fLdap.SearchBegin();
  fLdap.SearchScope := lssWholeSubtree;
  repeat
    if not SearchSitesInLdap then
      Exit;

    for LdapResult in fLdap.SearchResult.Items do
      AddItemInResultArray(fNotInSite, LdapResult);
  until fLdap.SearchCookie = '';
  fLdap.SearchEnd;
end;


function TFrmPropertyGeneralSiteLinkBridge.GetSitesInSiteLink: TLdapResultArray;
begin
  Result := fInSite;
end;

function TFrmPropertyGeneralSiteLinkBridge.GetSitesNotInSiteLink: TLdapResultArray;
begin
  Result := fNotInSite;
end;

function TFrmPropertyGeneralSiteLinkBridge.GetResultName(Obj: TLdapResult): RawUtf8;
begin
  Result := Obj.Find('name').GetReadable();
end;

procedure TFrmPropertyGeneralSiteLinkBridge.LoadListBox;
var
  r: TLdapResult;
begin
  ListBox_NotInSiteLinkBridge.Clear;
  for r in GetSitesNotInSiteLink do
    ListBox_NotInSiteLinkBridge.Items.Add(GetResultName(r));

  ListBox_InSiteLinkBridge.Clear;
  for r in GetSitesInSiteLink do
    ListBox_InSiteLinkBridge.Items.Add(GetResultName(r));
end;

constructor TFrmPropertyGeneralSiteLinkBridge.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fLog := TOpenRSATLog;
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, 'Create', Self);

  Caption := 'General';
end;

procedure TFrmPropertyGeneralSiteLinkBridge.Update(Props: TProperty);
begin
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, 'Update', Self);

  fProperty := Props;
  fLdap := fProperty.LdapClient;

  Edit_Name.CaptionNoChange := fProperty.name;
  Edit_Description.CaptionNoChange := fProperty.description;

  RetrieveSiteLinks;
  LoadListBox;
end;

end.

