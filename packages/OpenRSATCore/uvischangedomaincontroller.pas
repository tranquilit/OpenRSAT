unit uvischangedomaincontroller;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  Buttons,
  tis.ui.grid.core,
  mormot.core.log,
  mormot.core.variants,
  uinterfacecore;

type

  { TVisChangeDomainController }

  TVisChangeDomainController = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    CheckBox1: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    TisGrid1: TTisGrid;
    procedure FormShow(Sender: TObject);
  private
    fCore: ICore;
    fLog: TSynLog;
    fSitesData: TDocVariantData;

    procedure UpdateCurrentServer;
    procedure FillGrid;
    procedure RetrieveServers;
    procedure RetrieveServersExtraInfos;
    procedure NotifyUpdateSitesData;
  public
  end;

implementation
uses
  mormot.core.base,
  mormot.core.text,
  mormot.net.ldap,
  ucommon,
  ursatldapclient;

{$R *.lfm}

{ TVisChangeDomainController }

procedure TVisChangeDomainController.FormShow(Sender: TObject);
var
  SearchResult: TLdapResult;
  NewRow: TDocVariantData;
  DistinguishedName: RawUtf8;
begin
  UpdateCurrentServer;
  fSitesData.Init();
  FillGrid;
end;

procedure TVisChangeDomainController.UpdateCurrentServer;
begin
  Label2.Caption := fCore.LdapClient.Settings.TargetHost;
end;

procedure TVisChangeDomainController.FillGrid;
begin
  RetrieveServers;
  RetrieveServersExtraInfos;
end;

procedure TVisChangeDomainController.RetrieveServers;
var
  SearchResult: TLdapResult;
  DistinguishedName: RawUtf8;
  NewObject: PDocVariantData;

  function GetSite(DistinguishedName: RawUtf8): RawUtf8;
  var
    DistinguishedNameSplitted, NameSplitted: TStringArray;
  begin
    result := '';

    DistinguishedNameSplitted := String(DistinguishedName).Split(',');

    if (Length(DistinguishedNameSplitted) < 3) then
      Exit;

    NameSplitted := DistinguishedNameSplitted[2].Split('=');
    if (Length(NameSplitted) <> 2) then
      Exit;
    result := NameSplitted[1];
  end;

begin
  fCore.LdapClient.SearchBegin();
  try
    fCore.LdapClient.SearchScope := lssWholeSubtree;

    repeat
      if not fCore.LdapClient.Search(fCore.LdapClient.ConfigDN, False, '(objectClass=server)', ['distinguishedName', 'dNSHostName']) then
      begin
        if Assigned(fLog) then
          fLog.Log(sllError, fCore.LdapClient.ResultString);
        ShowLdapSearchError(fCore.LdapClient);
        Exit;
      end;

      for SearchResult in fCore.LdapClient.SearchResult.Items do
      begin
        if not Assigned(SearchResult) then
          continue;
        DistinguishedName := SearchResult.Find('distinguishedName').GetReadable();
        NewObject := fSitesData.O_[DistinguishedName];
        NewObject^.AddValue('distinguishedName', DistinguishedName);
        NewObject^.AddValue('name', SearchResult.Find('dNSHostName').GetReadable());
        NewObject^.AddValue('site', GetSite(DistinguishedName));

        //NewRow.AddValue('dctype'); // options on child object (nTDSDSA and applicationSettings)
        //NewRow.AddValue('dcversion'); // msDS-Behavior-Version on child object (nTDSDSA and applicationSettings)
        //NewRow.AddValue('status'); // Search on RootDSE, short timeout, perform in threads?
      end;
    until fCore.LdapClient.SearchCookie = '';
  finally
    fCore.LdapClient.SearchEnd();
  end;
  NotifyUpdateSitesData;
end;

procedure TVisChangeDomainController.RetrieveServersExtraInfos;
var
  SearchResult: TLdapResult;
  dcversion, dctype, DistinguishedName: RawUtf8;
begin
  fCore.LdapClient.SearchBegin();
  try
    repeat
      if not fCore.LdapClient.Search(fCore.LdapClient.ConfigDN, False, '(&(objectClass=nTDSDSA)(objectClass=applicationSettings))', ['options', 'msDS-Behavior-Version']) then
      begin
        if Assigned(fLog) then
          fLog.Log(sllError, 'Ldap Search Error: "%"', [fCore.LdapClient.ResultString]);
        ShowLdapSearchError(fCore.LdapClient);
        Exit;
      end;

      for SearchResult in fCore.LdapClient.SearchResult.Items do
      begin
        if not Assigned(SearchResult) then
          continue;
        DistinguishedName := SearchResult.Find('distinguishedName').GetReadable();
        DistinguishedName := GetParentDN(DistinguishedName);
        fSitesData.O_[DistinguishedName]^.S['dcversion'] := SearchResult.Find('msDS-Behavior-Version').GetReadable();
        fSitesData.O_[DistinguishedName]^.S['dctype'] := SearchResult.Find('options').GetReadable();
      end;
    until fCore.LdapClient.SearchCookie = '';
  finally
    fCore.LdapClient.SearchEnd();
  end;
  // Request information
  // Complete missing information to SitesData
end;

procedure TVisChangeDomainController.NotifyUpdateSitesData;
var
  SiteName: RawUtf8;
begin
  TisGrid1.BeginUpdate;
  try
    TisGrid1.Clear;
    for SiteName in fSitesData.GetNames do
      TisGrid1.Data.AddItem(fSitesData.O_[SiteName]^);
  finally
    TisGrid1.EndUpdate;
  end;
end;

end.

