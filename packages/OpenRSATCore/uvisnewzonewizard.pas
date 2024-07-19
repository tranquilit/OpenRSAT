unit uvisnewzonewizard;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ExtCtrls,
  StdCtrls,
  ComCtrls,
  Buttons,
  ActnList,
  ursatldapclient,
  mormot.net.ldap;

type

  TRawDNSProperty = type RawByteString;

  { TVisNewZoneWizard }

  TVisNewZoneWizard = class(TForm)
    Action_BackWizard: TAction;
    Action_BackZoneType: TAction;
    Action_BackZoneReplicationScope: TAction;
    Action_BackZoneName: TAction;
    Action_BackZoneFile: TAction;
    Action_BackDynamicZone: TAction;
    Action_BackFinish: TAction;
    Action_NextDynamicZone: TAction;
    Action_NextFinish: TAction;
    Action_NextZoneReplicationScope: TAction;
    Action_NextZoneName: TAction;
    Action_NextZoneFile: TAction;
    Action_NextWizard: TAction;
    Action_NextZoneType: TAction;
    Action_Cancel: TAction;
    ActionList1: TActionList;
    BitBtn_Back: TBitBtn;
    BitBtn_Next: TBitBtn;
    BitBtn_Cancel: TBitBtn;
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label3: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label4: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Memo1: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    RadioButton1: TRadioButton;
    RadioButton10: TRadioButton;
    RadioButton11: TRadioButton;
    RadioButton12: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    RadioButton6: TRadioButton;
    RadioButton7: TRadioButton;
    RadioButton8: TRadioButton;
    RadioButton9: TRadioButton;
    TS_DynamicZone: TTabSheet;
    TS_Finish: TTabSheet;
    TS_ZoneFile: TTabSheet;
    TS_ZoneName: TTabSheet;
    TS_ZoneReplicationScope: TTabSheet;
    TS_ZoneType: TTabSheet;
    TS_Wizard: TTabSheet;
    procedure Action_BackDynamicZoneExecute(Sender: TObject);
    procedure Action_BackFinishExecute(Sender: TObject);
    procedure Action_BackZoneFileExecute(Sender: TObject);
    procedure Action_BackZoneNameExecute(Sender: TObject);
    procedure Action_BackZoneReplicationScopeExecute(Sender: TObject);
    procedure Action_BackZoneTypeExecute(Sender: TObject);
    procedure Action_NextDynamicZoneExecute(Sender: TObject);
    procedure Action_NextWizardExecute(Sender: TObject);
    procedure Action_NextZoneFileExecute(Sender: TObject);
    procedure Action_NextZoneNameExecute(Sender: TObject);
    procedure Action_NextZoneNameUpdate(Sender: TObject);
    procedure Action_NextZoneReplicationScopeExecute(Sender: TObject);
    procedure Action_NextZoneTypeExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TS_DynamicZoneShow(Sender: TObject);
    procedure TS_FinishShow(Sender: TObject);
    procedure TS_WizardShow(Sender: TObject);
    procedure TS_ZoneFileShow(Sender: TObject);
    procedure TS_ZoneNameShow(Sender: TObject);
    procedure TS_ZoneReplicationScopeShow(Sender: TObject);
    procedure TS_ZoneTypeShow(Sender: TObject);
  private

  public
    procedure Apply(LdapClient: TRsatLdapClient);
  end;

implementation
uses
  mormot.net.dns,
  mormot.core.base,
  udns;

{$R *.lfm}

{ TVisNewZoneWizard }

procedure TVisNewZoneWizard.TS_WizardShow(Sender: TObject);
begin
  BitBtn_Back.Action := Action_BackWizard;
  BitBtn_Next.Action := Action_NextWizard;
end;

procedure TVisNewZoneWizard.TS_DynamicZoneShow(Sender: TObject);
begin
  BitBtn_Back.Action := Action_BackDynamicZone;
  BitBtn_Next.Action := Action_NextDynamicZone;
end;

procedure TVisNewZoneWizard.Action_NextWizardExecute(Sender: TObject);
begin
  PageControl1.ActivePage := TS_ZoneType;
end;

procedure TVisNewZoneWizard.Action_NextZoneFileExecute(Sender: TObject);
begin
  PageControl1.ActivePage := TS_DynamicZone;
end;

procedure TVisNewZoneWizard.Action_NextDynamicZoneExecute(Sender: TObject);
begin
  PageControl1.ActivePage := TS_Finish;
end;

procedure TVisNewZoneWizard.Action_BackZoneTypeExecute(Sender: TObject);
begin
  PageControl1.ActivePage := TS_Wizard;
end;

procedure TVisNewZoneWizard.Action_BackZoneReplicationScopeExecute(
  Sender: TObject);
begin
  PageControl1.ActivePage := TS_ZoneType;
end;

procedure TVisNewZoneWizard.Action_BackZoneNameExecute(Sender: TObject);
begin
  if CheckBox1.Checked then
    PageControl1.ActivePage := TS_ZoneReplicationScope
  else
    PageControl1.ActivePage := TS_ZoneType;
end;

procedure TVisNewZoneWizard.Action_BackZoneFileExecute(Sender: TObject);
begin
  PageControl1.ActivePage := TS_ZoneName;
end;

procedure TVisNewZoneWizard.Action_BackDynamicZoneExecute(Sender: TObject);
begin
  if CheckBox1.Checked then
    PageControl1.ActivePage := TS_ZoneName
  else
    PageControl1.ActivePage := TS_ZoneFile;
end;

procedure TVisNewZoneWizard.Action_BackFinishExecute(Sender: TObject);
begin
  PageControl1.ActivePage := TS_DynamicZone;
  BitBtn_Next.ModalResult := mrNone;
end;

procedure TVisNewZoneWizard.Action_NextZoneNameExecute(Sender: TObject);
begin
  if CheckBox1.Checked then
    PageControl1.ActivePage := TS_DynamicZone
  else
    PageControl1.ActivePage := TS_ZoneFile;
end;

procedure TVisNewZoneWizard.Action_NextZoneNameUpdate(Sender: TObject);
begin
  Action_NextZoneName.Enabled := (Edit1.Text <> '');
end;

procedure TVisNewZoneWizard.Action_NextZoneReplicationScopeExecute(
  Sender: TObject);
begin
  PageControl1.ActivePage := TS_ZoneName;
end;

procedure TVisNewZoneWizard.Action_NextZoneTypeExecute(Sender: TObject);
begin
  if CheckBox1.Checked then
    PageControl1.ActivePage := TS_ZoneReplicationScope
  else
    PageControl1.ActivePage := TS_ZoneName;
end;

procedure TVisNewZoneWizard.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePage := TS_Wizard;
end;

procedure TVisNewZoneWizard.TS_FinishShow(Sender: TObject);
begin
  BitBtn_Back.Action := Action_BackFinish;
  BitBtn_Next.Action := Action_NextFinish;
  BitBtn_Next.ModalResult := mrOK;
end;

procedure TVisNewZoneWizard.TS_ZoneFileShow(Sender: TObject);
begin
  BitBtn_Back.Action := Action_BackZoneFile;
  BitBtn_Next.Action := Action_NextZoneFile;
end;

procedure TVisNewZoneWizard.TS_ZoneNameShow(Sender: TObject);
begin
  BitBtn_Back.Action := Action_BackZoneName;
  BitBtn_Next.Action := Action_NextZoneName;
end;

procedure TVisNewZoneWizard.TS_ZoneReplicationScopeShow(Sender: TObject);
begin
  BitBtn_back.Action := Action_BackZoneReplicationScope;
  BitBtn_Next.Action := Action_NextZoneReplicationScope;
end;

procedure TVisNewZoneWizard.TS_ZoneTypeShow(Sender: TObject);
begin
  BitBtn_Back.Action := Action_BackZoneType;
  BitBtn_Next.Action := Action_NextZoneType;
end;

procedure TVisNewZoneWizard.Apply(LdapClient: TRsatLdapClient);
var
  DistinguishedName: String;
  AttributeList: TLdapAttributeList;
  Attribute: TLdapAttribute;
  DNSProperty: TDNSProperty;
  DNSRecord: TDNSRecord;
  RawDNSProperty: TRawDNSProperty;
  len: Integer;
  Buffer: Array[0..$ffff] of Byte;
  SOA: TRRSOA;
  NS: TRRNS;
begin
  if RadioButton4.Checked then
    DistinguishedName := Format('DC=%s,CN=MicrosoftDNS,DC=DomainDnsZones,%s', [Edit1.Text, LdapClient.RootDN])
  else if RadioButton5.Checked then
    DistinguishedName := Format('DC=%s,CN=MicrosoftDNS,DC=ForestDnsZones,%s', [Edit1.Text, LdapClient.DefaultDN()])
  else
    Exit;

  AttributeList := TLdapAttributeList.Create;
  try
    Attribute := AttributeList.Add('objectClass', 'top');
    Attribute.Add('dnsZone');

    DNSProperty.DataLength := 0;
    DNSProperty.NameLength := 0;
    DNSProperty.Flag := 0;
    DNSProperty.Version := 1;
    DNSProperty.Id := 0;
    DNSProperty.Name := 0;

    // Add zone type
    DNSProperty.Id := Ord(dpidPropertyZoneType);
    DNSProperty.DataLength := DNSPropertyZoneTypeRecordToBytes(PByteArray(@DNSProperty.Data)^, dpztDnsZoneTypePrimary);
    len := DNSPropertyRecordToBytes(PByteArray(@Buffer)^, DNSProperty);
    SetLength(RawDNSProperty, len);
    Move(Buffer, RawDNSProperty[1], len);
    Attribute := AttributeList.Add('dNSProperty', RawDNSProperty);
    // Add zone update
    DNSProperty.Id := Ord(dpidPropertyZoneAllowUpdate);
    DNSProperty.DataLength := DNSPropertyZoneAllowUpdateRecordToBytes(PByteArray(@DNSProperty.Data)^, dpzauDnsZoneUpdateSecure);
    len := DNSPropertyRecordToBytes(PByteArray(@Buffer)^, DNSProperty);
    SetLength(RawDNSProperty, len);
    Move(Buffer, RawDNSProperty[1], len);
    Attribute.Add(RawDNSProperty);
    // Add zone secure time
    DNSProperty.Id := Ord(dpidPropertyZoneSecureTime);
    DNSProperty.DataLength := DNSPropertyZoneSecureTimeRecordToBytes(PByteArray(@DNSProperty.Data)^, 0);
    len := DNSPropertyRecordToBytes(PByteArray(@Buffer)^, DNSProperty);
    SetLength(RawDNSProperty, len);
    Move(Buffer, RawDNSProperty[1], len);
    Attribute.Add(RawDNSProperty);
    // Add zone no refresh interval
    DNSProperty.Id := Ord(dpidPropertyZoneNoRefreshInterval);
    DNSProperty.DataLength := DNSPropertyZoneNoRefreshIntervalRecordToBytes(PByteArray(@DNSProperty.Data)^, 168);
    len := DNSPropertyRecordToBytes(PByteArray(@Buffer)^, DNSProperty);
    SetLength(RawDNSProperty, len);
    Move(Buffer, RawDNSProperty[1], len);
    Attribute.Add(RawDNSProperty);
    // Add zone refresh interval
    DNSProperty.Id := Ord(dpidPropertyZoneRefreshInterval);
    DNSProperty.DataLength := DNSPropertyZoneRefreshIntervalRecordToBytes(PByteArray(@DNSProperty.Data)^, 168);
    len := DNSPropertyRecordToBytes(PByteArray(@Buffer)^, DNSProperty);
    SetLength(RawDNSProperty, len);
    Move(Buffer, RawDNSProperty[1], len);
    Attribute.Add(RawDNSProperty);
    // Add zone aging state
    DNSProperty.Id := Ord(dpidPropertyZoneAgingState);
    DNSproperty.DataLength := DNSPropertyZoneAgingStateRecordToBytes(PByteArray(@DNSProperty.Data)^, False);
    len := DNSPropertyRecordToBytes(PByteArray(@Buffer)^, DNSProperty);
    SetLength(RawDNSProperty, len);
    Move(Buffer, RawDNSProperty[1], len);
    Attribute.Add(RawDNSProperty);
    // Add zone aging enable time
    DNSProperty.Id := Ord(dpidPropertyZoneAgingEnabledTime);
    DNSProperty.DataLength := DNSPropertyZoneAgingEnabledTimeRecordToBytes(PByteArray(@DNSProperty.Data)^, 0);
    len := DNSPropertyRecordToBytes(PByteArray(@Buffer)^, DNSProperty);
    SetLength(RawDNSProperty, len);
    Move(Buffer, RawDNSProperty[1], len);
    Attribute.Add(RawDNSProperty);

    if not LdapClient.Add(DistinguishedName, AttributeList) then
    begin
      ShowMessage(LdapClient.ResultString);
      Exit;
    end;
  finally
    FreeAndNil(AttributeList);
  end;

  AttributeList := TLdapAttributeList.Create;
  try
    //AttributeList.Add('dc', '@');
    Attribute := AttributeList.Add('objectClass', 'top');
    Attribute.Add('dnsNode');
    DNSRecord.Serial := 1;
    DNSRecord.Version := 5;
    DNSRecord.Flags := 0;
    DNSRecord.Reserved := 0;
    DNSRecord.Rank := 240;
    DNSRecord.Timestamp := 0;
    DNSRecord.TtlSeconds := 3600;

    DNSRRSOABuild(SOA, 1, 60 * 15, 60 * 10, 60 * 60 * 24, 60 * 60, 'srvads.tprud.lan', 'hostmaster.tprud.lan');
    DNSRecord.RecType := Ord(drrSOA);
    DNSRecord.DataLength := DNSRRSOARecordToBytes(PByteArray(@DNSRecord.RData)^, SOA);
    len := DNSRecordRecordToBytes(PByteArray(@Buffer)^, DNSRecord);
    SetLength(RawDNSProperty, len);
    Move(Buffer, RawDNSProperty[1], len);
    Attribute := AttributeList.Add('dnsRecord', RawDNSProperty);

    DNSRRNSBuild(NS, 'srvads.tprud.lan');
    DNSRecord.RecType := Ord(drrNS);
    DNSRecord.DataLength := DNSRRNSRecordToBytes(PByteArray(@DNSRecord.RData)^, NS);
    len := DNSRecordRecordToBytes(PByteArray(@Buffer)^, DNSRecord);
    SetLength(RawDNSProperty, len);
    Move(Buffer, RawDNSProperty[1], len);
    Attribute.Add(RawDNSProperty);

    if not LdapClient.Add(Format('DC=@,%s', [DistinguishedName]), AttributeList) then
    begin
      ShowMessage(LdapClient.ResultString);
      Exit;
    end;
  finally
    FreeAndNil(AttributeList);
  end;
end;

end.

