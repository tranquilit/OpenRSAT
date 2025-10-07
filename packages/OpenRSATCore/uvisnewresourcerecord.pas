unit uvisnewresourcerecord;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ComCtrls,
  ExtCtrls,
  Buttons,
  StdCtrls,
  EditBtn,
  ActnList,
  mormot.net.dns,
  mormot.net.ldap,
  mormot.core.log,
  mormot.core.base,
  ursatldapclient,
  udns;

type

  { TVisNewResourceRecord }

  TVisNewResourceRecord = class(TForm)
    Action_Cancel: TAction;
    Action_OK: TAction;
    ActionList1: TActionList;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn_MXBrowse: TBitBtn;
    BitBtn_PTRBrowse: TBitBtn;
    BitBtn_CNAMEFQDNTARGET: TBitBtn;
    CheckBox1: TCheckBox;
    CheckBox_PTRAllowAny: TCheckBox;
    CheckBox_CNAMEDelete: TCheckBox;
    CheckBox_CNAMEAllow: TCheckBox;
    CheckBox_APtr: TCheckBox;
    CheckBox_ADelete: TCheckBox;
    CheckBox_AAllow: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit_MXHost: TEdit;
    Edit_MXFQDN: TEdit;
    Edit_MXMailServer: TEdit;
    Edit_MXPriority: TEdit;
    Edit_PTRHost: TEdit;
    Edit_PTRFQDN: TEdit;
    Edit_PTRHostname: TEdit;
    Edit_CNAMEAlias: TEdit;
    Edit_CNAMEFQDN: TEdit;
    Edit_CNAMEFQDNTARGET: TEdit;
    Edit_CNAMETimestamp: TEdit;
    Edit_AHost: TEdit;
    Edit_AFQDN: TEdit;
    Edit_AIPAddress: TEdit;
    Edit_ATimestamp: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label_MXHost: TLabel;
    Label_MXDefault: TLabel;
    Label_MXFQDN: TLabel;
    Label_MXMailServer: TLabel;
    Label_MXPriority: TLabel;
    Label_PTRHost: TLabel;
    Label_PTRFQDN: TLabel;
    Label_PTRHostname: TLabel;
    Label_PTRAllowAny: TLabel;
    Label_CNAMEAlias: TLabel;
    Label_APtr: TLabel;
    Label_ADelete: TLabel;
    Label_ATimestamp: TLabel;
    Label_AAllow: TLabel;
    Label_CNAMEFQDN: TLabel;
    Label_CNAMEFQDNTARGET: TLabel;
    Label_CNAMETimestamp: TLabel;
    Label_CNAMEDelete: TLabel;
    Label_CNAMEAllow: TLabel;
    Label_AHost: TLabel;
    Label_AFQDN: TLabel;
    Label_AIPAddress: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    TS_SRV: TTabSheet;
    TS_MX: TTabSheet;
    TS_PTR: TTabSheet;
    TS_CNAME: TTabSheet;
    TS_A: TTabSheet;
    procedure Action_OKExecute(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Edit_AHostChange(Sender: TObject);
    procedure Label_CNAMEDeleteClick(Sender: TObject);
    procedure Label_CNAMEAllowClick(Sender: TObject);
    procedure TS_AShow(Sender: TObject);
  private
    fRecordType: TDnsResourceRecord;
    fSerial: Cardinal;
    fLdapClient: TRsatLdapClient;
    fDcPrefix: String;
    fDistinguishedName: String;

    function CreateOrUpdateRecord(const dnsRecord: TDNSRecord; const DistinguishedName: String): Boolean;

    procedure OKExecuteA(var dnsRecord: TDNSRecord);
    procedure OKExecuteCNAME(var dnsRecord: TDNSRecord);
    procedure OKExecutePTR(var dnsRecord: TDNSRecord);
    procedure OKExecuteMX(var dnsRecord: TDNSRecord);
    procedure OKExecuteSRV(var dnsRecord: TDNSRecord);
  public
    constructor Create(TheOwner: TComponent; RecordType: TDnsResourceRecord; Serial: Cardinal; LdapClient: TRsatLdapClient; distinguishedName, dcPrefix: String); reintroduce;

  end;

implementation

{$R *.lfm}

{ TVisNewResourceRecord }

procedure TVisNewResourceRecord.Label_CNAMEDeleteClick(Sender: TObject);
begin
  CheckBox_CNAMEDelete.Checked := not CheckBox_CNAMEDelete.Checked;
end;

procedure TVisNewResourceRecord.Action_OKExecute(Sender: TObject);
var
  DnsRecord: TDNSRecord;
begin
  DnsRecord.RecType := Ord(fRecordType);
  DnsRecord.Version := $05;
  DnsRecord.Rank := $f0;
  DnsRecord.Flags := $00;
  DnsRecord.Serial := fSerial;
  DnsRecord.TtlSeconds := $e10; // To be defined
  DnsRecord.Reserved := $00;
  DnsRecord.Timestamp := $00;   // To be defined

  case fRecordType of
    drrA: OKExecuteA(DnsRecord);
    drrCNAME: OKExecuteCNAME(DnsRecord);
    drrPTR: OKExecutePTR(DnsRecord);
    drrMX: OKExecuteMX(DnsRecord);
    drrSRV: OKExecuteSRV(DnsRecord);
  end;
end;

procedure TVisNewResourceRecord.ComboBox1Change(Sender: TObject);
begin
  case ComboBox1.Text of
    '_finger': Edit4.Text := '79';
    '_ftp': Edit4.Text := '21';
    '_http': Edit4.Text := '80';
    '_kerberos': Edit4.Text := '88';
    '_ldap': Edit4.Text := '389';
    '_msdcs': Edit4.Text := '389';
    '_nntp': Edit4.Text := '119';
    '_telnet': Edit4.Text := '23';
    '_whois': Edit4.Text := '43';
  end;
end;

procedure TVisNewResourceRecord.Edit_AHostChange(Sender: TObject);
var
  s: String;
begin
  s := Format('%s.', [DNToCN(fLdapClient.DefaultDN())]);
  if fDcPrefix <> '' then
    s := Format('%s.%s', [fDcPrefix, s]);
  if Edit_AHost.Text <> '' then
    s := Format('%s.%s', [Edit_AHost.Text, s]);
  Edit_AFQDN.Text := s;
end;

procedure TVisNewResourceRecord.Label_CNAMEAllowClick(Sender: TObject);
begin
  CheckBox_CNAMEAllow.Checked := not CheckBox_CNAMEAllow.Checked;
end;

procedure TVisNewResourceRecord.TS_AShow(Sender: TObject);
begin
  Edit_AHostChange(Sender);
end;

function TVisNewResourceRecord.CreateOrUpdateRecord(
  const dnsRecord: TDNSRecord; const DistinguishedName: String): Boolean;
var
  LdapObj: TLdapResult;
  NewAttribute: TLdapAttribute;
  ARawByteString: RawByteString;
  NewAttributeList: TLdapAttributeList;
  BytesArray: Array[0..$ffff] of Byte;
  Len: Integer;
begin
  // Check if object exists
  result := False;

  Len := DNSRecordRecordToBytes(PByteArray(@BytesArray[0])^, dnsRecord);

  ARawByteString := Copy(String(@BytesArray[0]), 0, Len + 1);

  LdapObj := fLdapClient.SearchObject(DistinguishedName, '', ['dnsRecord']);
  if Assigned(LdapObj) then
  begin
    NewAttribute := TLdapAttribute.Create('dnsRecord', atUndefined);
    try
      NewAttribute.Add(ARawByteString);
      if not fLdapClient.Modify(DistinguishedName, lmoAdd, NewAttribute) then
      begin
        TSynLog.Add.Log(sllError, '% - Ldap Modify Error: %', [Self.Name, fLdapClient.ResultString]);
        Exit;
      end;
    finally
      FreeAndNil(NewAttribute);
    end;
  end
  else
  begin
    NewAttributeList := TLdapAttributeList.Create;
    try
      NewAttribute := NewAttributeList.Add('objectClass', 'top');
      NewAttribute.Add('dnsNode');

      NewAttributeList.Add('dnsRecord', ARawByteString);

      if not fLdapClient.Add(DistinguishedName, NewAttributeList) then
      begin
        TSynLog.Add.Log(sllError, '% - Ldap Add Error: %', [Self.Name, fLdapClient.ResultString]);
        Exit;
      end;
    finally
      FreeAndNil(NewAttributeList);
    end;
  end;
  result := True;
end;

procedure TVisNewResourceRecord.OKExecuteA(var dnsRecord: TDNSRecord);
var
  A: TRRA;
  DistinguishedName, s: String;
begin
  DNSRRABuild(A, Edit_AIPAddress.Text);
  dnsRecord.DataLength := Word(DNSRRARecordToBytes(PByteArray(@dnsRecord.RData[0])^, A));

  s := Edit_AHost.Text;
  if (s = '') and (fDcPrefix = '') then
    s := '@';
  DistinguishedName := Format('DC=%s%s,%s', [s, fDcPrefix, fDistinguishedName]);

  CreateOrUpdateRecord(dnsRecord, DistinguishedName);
end;

procedure TVisNewResourceRecord.OKExecuteCNAME(var dnsRecord: TDNSRecord);
var
  CNAME: TRRCname;
  DistinguishedName, s: String;
begin
  DNSRRCNAMEBuild(CNAME, Edit_CNAMEFQDNTARGET.Text);
  dnsRecord.DataLength := Word(DNSRRCNAMERecordToBytes(PByteArray(@dnsRecord.RData[0])^, CNAME));

  s := Edit_CNAMEAlias.Text;
  if (s = '') and (fDcPrefix = '') then
    s := '@';
  DistinguishedName := Format('DC=%s%s,%s', [s, fDcPrefix, fDistinguishedName]);

  CreateOrUpdateRecord(dnsRecord, DistinguishedName);
end;

procedure TVisNewResourceRecord.OKExecutePTR(var dnsRecord: TDNSRecord);
var
  DistinguishedName, s: String;
  APTR: TRRPTR;
begin
  DNSRRPTRBuild(APTR, Edit_PTRHostname.Text);
  dnsRecord.DataLength := Word(DNSRRPTRRecordToBytes(PByteArray(@dnsRecord.RData[0])^, APTR));

  s := Edit_PTRHost.Text;
  if (s = '') and (fDcPrefix = '') then
    s := '@';
  DistinguishedName := Format('DC=%s%s,%s', [s, fDcPrefix, fDistinguishedName]);

  CreateOrUpdateRecord(dnsRecord, DistinguishedName);
end;

procedure TVisNewResourceRecord.OKExecuteMX(var dnsRecord: TDNSRecord);
var
  MX: TRRMX;
  DistinguishedName, s: String;
begin
  DNSRRMXBuild(MX, Edit_MXMailServer.Text, StrToInt(Edit_MXPriority.Text));
  dnsRecord.DataLength := Word(DNSRRMXRecordToBytes(PByteArray(@dnsRecord.RData[0])^, MX));

  s := Edit_MXHost.Text;
  if (s = '') and (fDcPrefix = '') then
    s := '@';
  DistinguishedName := Format('DC=%s%s,%s', [s, fDcPrefix, fDistinguishedName]);

  CreateOrUpdateRecord(dnsRecord, DistinguishedName);
end;

procedure TVisNewResourceRecord.OKExecuteSRV(var dnsRecord: TDNSRecord);
var
  SRV: TRRSRV;
  DistinguishedName, s: String;
begin
  DNSRRSRVBuild(SRV, StrToInt(Edit2.Text), StrToint(Edit3.Text), StrToInt(Edit4.Text), Edit5.Text);
  dnsRecord.DataLength := Word(DNSRRSRVRecordToBytes(PByteArray(@dnsRecord.RData[0])^, SRV));

  s := '';
  if ComboBox1.Text <> '' then
    s := ComboBox1.Text;
  if ComboBox2.Text <> '' then
  begin
    if s = '' then
      s := ComboBox2.Text
    else
      s := Format('%s.%s', [s, ComboBox2.Text]);
  end;
  DistinguishedName := Format('DC=%s%s,%s', [s, fDcPrefix, fDistinguishedName]);

  CreateOrUpdateRecord(dnsRecord, DistinguishedName);
end;

constructor TVisNewResourceRecord.Create(TheOwner: TComponent;
  RecordType: TDnsResourceRecord; Serial: Cardinal;
  LdapClient: TRsatLdapClient; distinguishedName, dcPrefix: String);
begin
  Inherited Create(TheOwner);

  fSerial := Serial;
  fLdapClient := LdapClient;
  fDcPrefix := dcPrefix;
  fRecordType := RecordType;
  fDistinguishedName := distinguishedName;

  // Disable active page
  PageControl1.ActivePage := nil;

  // Active a page
  case fRecordType of
    drrCNAME: PageControl1.ActivePage := TS_CNAME;
    drrA: PageControl1.ActivePage := TS_A;
    drrMX: PageControl1.ActivePage := TS_MX;
    drrPTR: PageControl1.ActivePage := TS_PTR;
    drrSRV: PageControl1.ActivePage := TS_SRV;
  end;
  // Show active page
  if Assigned(PageControl1.ActivePage) then
    PageControl1.ActivePage.TabVisible := True
  else
    Close;
end;

end.

