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
  ActnList,
  tis.ui.grid.core,
  mormot.core.base,
  mormot.core.log,
  mormot.core.variants,
  mormot.net.ldap;

type

  TCallBack = procedure(Data: Pointer) of Object;

  /// the decoded fields of msDS-Behavior-Version attribute
  // https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-adts/8f0d9838-d9f2-44b8-b018-b41b62c0580c
  TMsdsBehaviorVersion = (
    mbvWin2000,        // 0
    mbv_,
    mbvWin2003,        // 2
    mbvWin2008,        // 3
    mbvWin2008R2,      // 4
    mbvWin2012,        // 5
    mbvWin2012R2,      // 6
    mbvWin2016);       // 7

  /// the decoded fields of nTDSDSA option
  // https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-adts/8ebf2419-1169-4413-88e2-12a5ad499cf5
  TNtdsDSAOption = (
    ndoGC,       // 1
    ndoDI,       // 2
    ndoDO,       // 4
    ndoDNX,      // 8
    ndoDS);      // 16

  { TThreadIsDCOnline }

  TThreadIsDCOnlineCallBack = procedure(DomainController: RawUtf8; Success: Boolean) of object;

  TThreadIsDCOnline = class(TThread)
  private
    fSettings: TLdapClientSettings;
    fDomainController: RawUtf8;
    fCallBack: TThreadIsDCOnlineCallBack;
    fSuccess: Boolean;

    procedure SendResult;
  public
    /// Send Ldap Settings, it will create a copy.
    procedure SetSettings(Settings: TLdapClientSettings);
    /// Define the domaine controller to test.
    procedure SetDomainController(DomainController: RawUtf8);
    /// Set callback to know the result.
    procedure SetCallBack(CallBack: TThreadIsDCOnlineCallBack);

    procedure Execute; override;
  end;

  { TChangeDomainController }

  TChangeDomainController = class
  private
    fLog: TSynLog;
    fSitesData: TDocVariantData;

    function GetSite(DistinguishedName: RawUtf8): RawUtf8;
  public
    function GetCurrentServer: RawUtf8;
    function RetrieveServers(CallBack: TCallBack; RetrieveExtra: Boolean = True): Boolean;
    function RetrieveServersExtra(CallBack: TCallBack): Boolean;
    function ChangeConnection(DomainController: RawUtf8): Boolean;
    function TestConnection(DomainController: RawUtf8): Boolean;
  end;

  { TVisChangeDomainController }

  TVisChangeDomainController = class(TForm)
    Action_OK: TAction;
    ActionList1: TActionList;
    BitBtn_Cancel: TBitBtn;
    BitBtn_OK: TBitBtn;
    CheckBox1: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    Timer_SearchInGrid: TTimer;
    TisGrid1: TTisGrid;
    procedure Action_OKExecute(Sender: TObject);
    procedure Action_OKUpdate(Sender: TObject);
    procedure BitBtn_CancelKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
    procedure Timer_SearchInGridTimer(Sender: TObject);
    procedure TisGrid1DblClick(Sender: TObject);
    procedure TisGrid1KeyPress(Sender: TObject; var Key: char);
  private
    fLog: TSynLog;
    fChangeDomainController: TChangeDomainController;
    fSearchWord: RawUtf8;
    fAllowClose: Boolean;

    procedure UpdateCurrentServer;
    procedure FillGrid;
    procedure OnRetrievedServers(data: Pointer);
    procedure UpdateRow(SiteName: RawUtf8; Success: Boolean);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    function DomainController: RawUtf8;
  end;

const
  OPTION_VALUE: Array[TNtdsDSAOption] of integer = (
    1,
    2,
    4,
    8,
    16);
  SITE_STATUS: Array[Boolean] of String = (
    'Unavailable',
    'Online');

function DcVersionFromText(Value: RawUtf8): RawUtf8;
function DCOptionFromText(Value: RawUtf8): RawUtf8;

implementation
uses
  mormot.core.rtti,
  mormot.core.text,
  mormot.net.dns,
  ucommon,
  ucommonui,
  ursatldapclient,
  ursatldapclientui,
  ufrmrsat;

var
  MsdsBehaviorVersionValue: Array[TMsdsBehaviorVersion] of String;
  NtdsDSAOptionValue: Array[TNtdsDSAOption] of String;

function DcVersionFromText(Value: RawUtf8): RawUtf8;
var
  dcVersionIndex: Longint;
begin
  result := '';
  if TryStrToInt(Value, dcVersionIndex) and (dcVersionIndex >= Ord(Low(TMsdsBehaviorVersion))) and (dcVersionIndex <= Ord(High(TMsdsBehaviorVersion))) then
    result := MsdsBehaviorVersionValue[TMsdsBehaviorVersion(dcVersionIndex)];
end;

function DCOptionFromText(Value: RawUtf8): RawUtf8;
var
  dcOption: Longint;
  i: TNtdsDSAOption;
  f: Integer;
begin
  result := '';
  if TryStrToInt(Value, dcOption) then
    for i := Low(i) to High(i) do
    begin
      f := OPTION_VALUE[i];
      if dcOption and f = 0 then
        continue;
      result := NtdsDSAOptionValue[i];
      break;
    end;
end;

{$R *.lfm}

{ TThreadIsDCOnline }

procedure TThreadIsDCOnline.SendResult;
begin
  fCallBack(fDomainController, fSuccess);
end;

procedure TThreadIsDCOnline.SetSettings(Settings: TLdapClientSettings);
begin
  if not Assigned(fSettings) then
    fSettings := TLdapClientSettings.Create;
  CopyObject(Settings, fSettings);
end;

procedure TThreadIsDCOnline.SetDomainController(DomainController: RawUtf8);
begin
  fDomainController := DomainController;
end;

procedure TThreadIsDCOnline.SetCallBack(CallBack: TThreadIsDCOnlineCallBack);
begin
  fCallBack := CallBack;
end;

procedure TThreadIsDCOnline.Execute;
var
  LdapClient: TRsatLdapClient;
begin
  TSynLog.Add.Log(sllTrace, 'Is DC online?', self);
  fSettings.TargetHost := fDomainController;
  fSettings.KerberosSpn := '';
  fSettings.UserName := '';
  fSettings.Password := '';
  LdapClient := TRsatLdapClient.Create(fSettings);
  try
    fSuccess := False;
    LdapClient.Connect(); // Don't care about the success for an anonymous connection.
    if not Assigned(LdapClient.SearchObject('', '', 'dnsHostName')) then
    begin
      TSynLog.Add.Log(sllTrace, 'Cannot retrieve root object "dnsHostName" attribute.', self);
      Exit;
    end;
    TSynLog.Add.Log(sllTrace, 'DC is online!');
    fSuccess := True;
  finally
    Synchronize(@SendResult);
    FreeAndNil(LdapClient);
    FreeAndNil(fSettings);
  end;
end;

{ TChangeDomainController }

function TChangeDomainController.GetSite(DistinguishedName: RawUtf8): RawUtf8;
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

function TChangeDomainController.GetCurrentServer: RawUtf8;
begin
  result := '';
  if Assigned(FrmRSAT) and Assigned(FrmRSAT.LdapClient) and Assigned(FrmRSAT.LdapClient.Settings) then
    result := FrmRSAT.LdapClient.Settings.TargetHost;
end;

function TChangeDomainController.RetrieveServers(CallBack: TCallBack;
  RetrieveExtra: Boolean): Boolean;
var
  Ldap: TRsatLdapClient;
  SearchResult: TLdapResult;
  DistinguishedName: RawUtf8;
  NewObject: PDocVariantData;
begin
  result := False;
  if not Assigned(FrmRSAT) or not Assigned(FrmRSAT.LdapClient) then
    Exit;

  Ldap := FrmRSAT.LdapClient;

  Ldap.SearchBegin();
  try
    Ldap.SearchScope := lssWholeSubtree;

    repeat
      if not Ldap.Search(Ldap.ConfigDN, False, '(objectClass=server)', ['distinguishedName', 'dNSHostName']) then
      begin
        if Assigned(fLog) then
          fLog.Log(sllError, Ldap.ResultString);
        Exit;
      end;

      for SearchResult in Ldap.SearchResult.Items do
      begin
        if not Assigned(SearchResult) then
          continue;
        DistinguishedName := SearchResult.Find('distinguishedName').GetReadable();
        NewObject := fSitesData.O_[DistinguishedName];
        NewObject^.S['distinguishedName'] := DistinguishedName;
        NewObject^.S['name'] := SearchResult.Find('dNSHostName').GetReadable();
        NewObject^.S['site'] := GetSite(DistinguishedName);
      end;
    until Ldap.SearchCookie = '';
  finally
    Ldap.SearchEnd();
  end;

  if RetrieveExtra then
  begin
    result := RetrieveServersExtra(CallBack);
    Exit;
  end;
  if Assigned(CallBack) then
    CallBack(Pointer(@fSitesData));
  result := True;
end;

function TChangeDomainController.RetrieveServersExtra(CallBack: TCallBack
  ): Boolean;
var
  Ldap: TRsatLdapClient;
  SearchResult: TLdapResult;
  DistinguishedName: RawUtf8;
begin
  result := False;
  Ldap := FrmRSAT.LdapClient;

  Ldap.SearchBegin();
  try
    repeat
      if not Ldap.Search(Ldap.ConfigDN, False, '(&(objectClass=nTDSDSA)(objectClass=applicationSettings))', ['options', 'msDS-Behavior-Version', 'distinguishedName']) then
      begin
        if Assigned(fLog) then
          fLog.Log(sllError, 'Ldap Search Error: "%"', [Ldap.ResultString]);
        Exit;
      end;

      for SearchResult in Ldap.SearchResult.Items do
      begin
        if not Assigned(SearchResult) then
          continue;
        DistinguishedName := SearchResult.Find('distinguishedName').GetReadable();
        DistinguishedName := GetParentDN(DistinguishedName);
        fSitesData.O_[DistinguishedName]^.S['dcversion'] := DcVersionFromText(SearchResult.Find('msDS-Behavior-Version').GetReadable());
        fSitesData.O_[DistinguishedName]^.S['dctype'] := DCOptionFromText(SearchResult.Find('options').GetReadable());
        fSitesData.O_[DistinguishedName]^.S['status'] := 'Pending...';
      end;
    until Ldap.SearchCookie = '';
  finally
    Ldap.SearchEnd();
  end;

  if Assigned(CallBack) then
    CallBack(Pointer(@fSitesData));
  result := True;
end;

function TChangeDomainController.ChangeConnection(DomainController: RawUtf8
  ): Boolean;
var
  test: TRsatLdapClient;
begin
  result := False;
  test := TRsatLdapClient.Create(FrmRSAT.LdapClient.Settings);
  test.OnError := FrmRSAT.LdapClient.OnError;
  try
    test.Settings.TargetHost := DomainController;
    test.Settings.KerberosSpn := '';
    if not test.Connect() then
      Exit;
    FrmRSAT.ChangeDomainController(DomainController);
    result := True;
  finally
    FreeAndNil(test);
  end;
end;

function TChangeDomainController.TestConnection(DomainController: RawUtf8): Boolean;
var
  test: TRsatLdapClient;
begin
  result := False;
  test := TRsatLdapClient.Create(FrmRSAT.LdapClient.Settings);
  test.OnError := FrmRSAT.LdapClient.OnError;
  try
    test.Settings.TargetHost := DomainController;
    test.Settings.KerberosSpn := '';
    if not test.Connect() then
      Exit;
    result := True;
  finally
    FreeAndNil(test);
  end;
end;

{ TVisChangeDomainController }

procedure TVisChangeDomainController.FormShow(Sender: TObject);
begin
  UnifyButtonsWidth([BitBtn_Cancel, BitBtn_OK]);
  UpdateCurrentServer;
  FillGrid;
end;

procedure TVisChangeDomainController.Timer_SearchInGridTimer(Sender: TObject);
begin
  Timer_SearchInGrid.Enabled := False;
end;

procedure TVisChangeDomainController.TisGrid1DblClick(Sender: TObject);
begin
  BitBtn_OK.Click;
end;

procedure TVisChangeDomainController.TisGrid1KeyPress(Sender: TObject;
  var Key: char);
begin
  SearchInGrid(Timer_SearchInGrid, TisGrid1, fSearchWord, Key);
end;

procedure TVisChangeDomainController.Action_OKUpdate(Sender: TObject);
begin
  Action_OK.Enabled := (RadioButton1.Checked) or (RadioButton2.Checked and (TisGrid1.SelectedCount = 1));
end;

procedure TVisChangeDomainController.BitBtn_CancelKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  case Key of
    27: Close;
  end;
end;

procedure TVisChangeDomainController.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := fAllowClose;
end;

procedure TVisChangeDomainController.Action_OKExecute(Sender: TObject);
begin
  fAllowClose := fChangeDomainController.TestConnection(TisGrid1.SelectedRows._[0]^.S['name']);
end;

procedure TVisChangeDomainController.UpdateCurrentServer;
begin
  Label2.Caption := fChangeDomainController.GetCurrentServer;
end;

procedure TVisChangeDomainController.FillGrid;
begin
  fChangeDomainController.RetrieveServers(@OnRetrievedServers);
end;

procedure TVisChangeDomainController.OnRetrievedServers(data: Pointer);
var
  SitesData, Row: PDocVariantData;
  DistinguishedName: RawUtf8;
  thread: TThreadIsDCOnline;
begin
  TisGrid1.Clear;

  SitesData := PDocVariantData(data);
  if not Assigned(SitesData) then
    Exit;

  TisGrid1.BeginUpdate;
  try
    for DistinguishedName in SitesData^.GetNames do
      TisGrid1.Data.AddItem(SitesData^.O_[DistinguishedName]^);
  finally
    TisGrid1.EndUpdate;
    TisGrid1.LoadData;
  end;

  for Row in TisGrid1.Data.Objects do
  begin
    if not Assigned(Row) then
      continue;
    thread := TThreadIsDCOnline.Create(True);
    thread.SetDomainController(Row^.S['name']);
    thread.SetSettings(FrmRSAT.LdapClient.Settings);
    thread.SetCallBack(@UpdateRow);
    thread.FreeOnTerminate := True;
    thread.Start;
  end;
end;

procedure TVisChangeDomainController.UpdateRow(SiteName: RawUtf8;
  Success: Boolean);
var
  Row: PDocVariantData;
begin
  for Row in TisGrid1.Data.Objects do
  begin
    if not Assigned(Row) then
      continue;
    if (Row^.S['name'] <> SiteName) then
      continue;
    Row^.S['status'] := SITE_STATUS[Success];
    break;
  end;
  TisGrid1.LoadData;
end;

constructor TVisChangeDomainController.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fAllowClose := True;

  fChangeDomainController := TChangeDomainController.Create;
  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'ChangeDomainController visual created.', Self);
end;

destructor TVisChangeDomainController.Destroy;
begin
  FreeAndNil(fChangeDomainController);

  inherited Destroy;
end;

function TVisChangeDomainController.DomainController: RawUtf8;
begin
  result := TisGrid1.SelectedRows._[0]^.S['name'];
end;

initialization
  MsdsBehaviorVersionValue[mbvWin2000] := 'W2K';
  MsdsBehaviorVersionValue[mbv_] := '';
  MsdsBehaviorVersionValue[mbvWin2003] := 'W2K3';
  MsdsBehaviorVersionValue[mbvWin2008] := 'W2K8';
  MsdsBehaviorVersionValue[mbvWin2008R2] := 'W2K8 R2';
  MsdsBehaviorVersionValue[mbvWin2012] := 'W2K12';
  MsdsBehaviorVersionValue[mbvWin2012R2] := 'W2K12 R2';
  MsdsBehaviorVersionValue[mbvWin2016] := 'W2K16';

  NtdsDSAOptionValue[ndoGC] := 'GC';
  NtdsDSAOptionValue[ndoDI] := 'DI';
  NtdsDSAOptionValue[ndoDO] := 'DO';
  NtdsDSAOptionValue[ndoDNX] := 'DNX';
  NtdsDSAOptionValue[ndoDS] := 'DS';
end.

