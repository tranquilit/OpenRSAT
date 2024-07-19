unit uvisselectnewrecordtype;

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
  Buttons,
  ExtCtrls, ActnList,
  mormot.net.dns,
  ursatldapclient,
  udns;

type

  { TVisSelectNewRecordType }

  TVisSelectNewRecordType = class(TForm)
    Action_CreateRecord: TAction;
    Action_Cancel: TAction;
    ActionList1: TActionList;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    ListBox1: TListBox;
    Memo1: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure Action_CancelExecute(Sender: TObject);
    procedure Action_CreateRecordExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListBox1SelectionChange(Sender: TObject; User: boolean);
  private
    fSerial: Cardinal;
    fLdapClient: TRsatLdapClient;
    fDcPrefix: String;
    fDistinguishedName: String;
  public
    constructor Create(TheOwner: TComponent; Serial: Cardinal; LdapClient:
      TRsatLdapClient; distinguishedName, dcPrefix: String); reintroduce;
  end;

const
  NewDNSRecords = [
    drrAFSDB,
    drrA,
    drrCNAME,
    drrATMA,
    drrDS,
    drrDHCID,
    drrDNSKEY,
    drrDNAME,
    drrHINFO,
    drrISDN,
    drrMX,
    drrMG,
    drrMB,
    drrMINFO,
    drrNAPTR,
    drrNXT,
    drrPTR,
    drrKEY,
    drrMR,
    drrRP,
    drrRT,
    drrSRV,
    drrSIG,
    drrTXT,
    drrWKS,
    drrX25
  ];

implementation

uses
  ucommon,
  uvisnewresourcerecord;

{$R *.lfm}

function GetDnsRecordName(RecordType: TDnsResourceRecord): String;
begin
  result := '';

  case RecordType of
    drrA,drrAAAA: result := rsDNSRecordA;
    drrCNAME: result := rsDNSRecordCNAME;
    drrMX: result := rsDNSRecordMX;
    drrPTR: result := rsDNSRecordPTR;
    drrSRV: result := rsDNSRecordSRV;
  end;
end;

function GetDnsRecordType(RecordName: String): TDnsResourceRecord;
var
  RecordType: TDnsResourceRecord;
begin
  result := drrEmpty;

  for RecordType in NEWDNSRECORDS do
    if (GetDnsRecordName(RecordType) = RecordName) then
      Exit(RecordType);
end;

function GetDnsRecordDescription(RecordType: TDnsResourceRecord): String;
begin
  result := '';

  case RecordType of
    drrA, drrAAAA: result := rsDNSRecordADescription;
    drrCNAME: result := rsDNSRecordCNAMEDescription;
    drrMX: result := rsDNSRecordMXDescription;
    drrPTR: result := rsDNSRecordPTRDescription;
    drrSRV: result := rsDNSRecordSRVDescription;
  end;
end;

{ TVisSelectNewRecordType }

procedure TVisSelectNewRecordType.FormCreate(Sender: TObject);
var
  RecordType: TDnsResourceRecord;
  s: String;
begin
  ListBox1.Items.BeginUpdate;
  try
    for RecordType in NEWDNSRECORDS do
    begin
      s := GetDnsRecordName(RecordType);
      if s <> '' then
        ListBox1.Items.Add(s);
    end;
  finally
    ListBox1.Items.EndUpdate;
  end;
end;

procedure TVisSelectNewRecordType.Action_CancelExecute(Sender: TObject);
begin
  Close;
end;

procedure TVisSelectNewRecordType.Action_CreateRecordExecute(Sender: TObject);
begin
  With TVisNewResourceRecord.Create(Self, GetDnsRecordType(ListBox1.GetSelectedText), fSerial, fLdapClient, fDistinguishedName, fDcPrefix) do
  begin
    ShowModal;
  end;
end;

procedure TVisSelectNewRecordType.ListBox1SelectionChange(Sender: TObject;
  User: boolean);
begin
  Memo1.Clear;
  Memo1.Text := GetDnsRecordDescription(GetDnsRecordType(ListBox1.GetSelectedText));
end;

constructor TVisSelectNewRecordType.Create(TheOwner: TComponent;
  Serial: Cardinal; LdapClient: TRsatLdapClient; distinguishedName,
  dcPrefix: String);
begin
  Inherited Create(TheOwner);

  fSerial := Serial;
  fLdapClient := LdapClient;
  fDcPrefix := dcPrefix;
  fDistinguishedName := distinguishedName;
end;

end.

