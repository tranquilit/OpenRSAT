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
  private
    fLog: TSynLogClass;
    fProperty: TProperty;
    fLdap: TRsatLdapClient;
    
    fInSite, fNotInSite: TLdapResultArray;
    
    procedure AddItemInResultArray(var List: TLdapResultArray; Item: TLdapResult);
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


function TFrmPropertyGeneralSiteLinkBridge.SearchSitesInLdap: boolean;
begin
  Result := fLdap.Search(FormatUtf8('CN=Sites,%', [fLdap.ConfigDN]), false, '(&(objectClass=siteLink))', ['name', 'distinguishedName'])
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

