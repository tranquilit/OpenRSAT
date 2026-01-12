unit ufrmpropertypublishedcertificates;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  StdCtrls,
  Buttons,
  ActnList,
  LCLIntf,
  Dialogs,
  mormot.core.base,
  mormot.core.log,
  mormot.core.variants,
  uproperty,
  tis.ui.grid.core,
  VirtualTrees,
  upropertyframe;

type

  TPublishedCertificate = record
    Certificate: RawUtf8;
    issuedTo: RawUtf8;
    issuedBy: RawUtf8;
    intendedPurpose: TRawUtf8DynArray;
    expirationData: TDateTime;
  end;

  { TFrmPropertyPublishedCertificates }

  TFrmPropertyPublishedCertificates = class(TPropertyFrame)
    Action_View: TAction;
    Action_Add: TAction;
    Action_Remove: TAction;
    Action_Copy: TAction;
    ActionList1: TActionList;
    BitBtn_Add: TBitBtn;
    BitBtn_Copy: TBitBtn;
    BitBtn_Remove: TBitBtn;
    BitBtn_View: TBitBtn;
    Label_ListX509: TLabel;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    TisGrid_ListX509: TTisGrid;
    procedure Action_AddExecute(Sender: TObject);
    procedure Action_CopyExecute(Sender: TObject);
    procedure Action_CopyUpdate(Sender: TObject);
    procedure Action_RemoveExecute(Sender: TObject);
    procedure Action_RemoveUpdate(Sender: TObject);
    procedure Action_ViewExecute(Sender: TObject);
    procedure Action_ViewUpdate(Sender: TObject);
  private
    fLog: TSynLog;
    fProperty: TProperty;

    function CertToDoc(s: RawByteString; out PublishedCertificate: TPublishedCertificate): Boolean;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Update(Props: TProperty); override;
  end;

implementation
uses
  mormot.core.os,
  mormot.core.os.security,
  mormot.core.text,
  mormot.crypt.secure,
  mormot.crypt.x509,
  mormot.net.ldap,
  ucommon,
  ucommonui,
  uhelpersui;

const
  /// standard long identifier of Certificate usage
  // - i.e. match OpenSSL PX509.ExtendedKeyUsage/KeyUsage text
  CEKU_FULLTEXT: array[TXExtendedKeyUsage] of RawUtf8 = (
    'None',                                       //xkuNone
    'Server Authentification',                    //xkuServerAuth
    'Client Authentification',                    //xkuClientAuth
    'Code Signing',                               //xkuCodeSigning
    'Email Protection',                           //xkuEmailProtection
    'Time Stamping',                              //xkuTimeStamping
    'Online Certificate Status Protocol Signing', //xkuOcspSigning
    'Ms Publisher');                              //xkuMsPublisher

{$R *.lfm}

{ TFrmPropertyPublishedCertificates }

procedure TFrmPropertyPublishedCertificates.Action_ViewExecute(Sender: TObject);
var
  TempFile, Cert: RawUtf8;
begin
  TempFile := GetTempDir();
  if TempFile = '' then
    Exit;
  TempFile := FormatUtf8('%%.tmp.crt', [TempFile, TisGrid_ListX509.FocusedRow^.U['issuedTo']]);

  Cert := TisGrid_ListX509.FocusedRow^.U['certificate'];
  if not FileFromString(Cert, TempFile) then
    Exit;
  OpenDocument(TempFile);
end;

procedure TFrmPropertyPublishedCertificates.Action_AddExecute(Sender: TObject);
var
  FilePath, FileContent: String;
  PublishedCertificate: TPublishedCertificate;
begin
  if not OpenDialog.Execute() then
    Exit;

  for FilePath in OpenDialog.Files do
  begin
    FileContent := StringFromFile(FilePath);
    if not CertToDoc(FileContent, PublishedCertificate) then
    begin
      if Assigned(fLog) then
        fLog.Log(sllWarning, 'Invalid certificate. (%)', [FilePath], Self);
      MessageDlg('Invalid Certificate', FormatUtf8('Certificate cannot be added. (%)', [FilePath]), mtWarning, mbOKCancel, 0);
      continue;
    end;
    fProperty.Add('userCertificate', FileContent, aoAlways);
  end;
  Update(fProperty);
end;

procedure TFrmPropertyPublishedCertificates.Action_CopyExecute(Sender: TObject);
var
  Cert: RawUtf8;
begin
  SaveDialog.FileName := TisGrid_ListX509.FocusedRow^.U['issuedTo'] + SaveDialog.DefaultExt;
  if not SaveDialog.Execute() then
    Exit;

  Cert := TisGrid_ListX509.FocusedRow^.U['certificate'];
  FileFromString(Cert, SaveDialog.FileName);
end;

procedure TFrmPropertyPublishedCertificates.Action_CopyUpdate(Sender: TObject);
begin
  Action_Copy.Enabled := Assigned(TisGrid_ListX509.FocusedRow);
end;

procedure TFrmPropertyPublishedCertificates.Action_RemoveExecute(Sender: TObject
  );
var
  Node: PVirtualNode;
  NodeData: PDocVariantData;
begin
  TisGrid_ListX509.DeleteSelectedRows();

  Node := TisGrid_ListX509.GetFirst();
  // No node, clear userCertificate
  if not Assigned(Node) then
  begin
    fProperty.Add('userCertificate', '');
    Exit;
  end;
  NodeData := TisGrid_ListX509.GetNodeAsPDocVariantData(Node);
  if not Assigned(Node) then
    Exit;
  // Clear and set first certificate
  fProperty.Add('userCertificate', NodeData^.U['certificate']);
  Node := TisGrid_ListX509.GetNext(Node);
  // Add all certificates
  while Assigned(Node) do
  begin
    NodeData := TisGrid_ListX509.GetNodeAsPDocVariantData(Node);
    Node := TisGrid_ListX509.GetNext(Node);
    if not Assigned(NodeData) then
      continue;
    fProperty.Add('userCertificate', NodeData^.U['certificate'], aoAlways);
  end;
end;

procedure TFrmPropertyPublishedCertificates.Action_RemoveUpdate(Sender: TObject
  );
begin
  Action_Remove.Enabled := Assigned(TisGrid_ListX509.FocusedRow);
end;

procedure TFrmPropertyPublishedCertificates.Action_ViewUpdate(Sender: TObject);
begin
  Action_View.Enabled := Assigned(TisGrid_ListX509.FocusedRow);
end;

function TFrmPropertyPublishedCertificates.CertToDoc(s: RawByteString; out
  PublishedCertificate: TPublishedCertificate): Boolean;
var
  der: TCertDer;
  Certx509: TX509;
  ceku: TXExtendedKeyUsage;
begin
  result := False;
  // cert validation
  der := PemToDer(s);
  if not AsnDecChunk(der) then
    Exit;

  Certx509 := TX509.Create();
  try
    // support PEM or DER input
    if not Certx509.LoadFromDer(der) then
    begin
      if Assigned(fLog) then
        fLog.Log(sllWarning, 'Cannot load from DER certificate.');
      Exit;
    end;
    if (Certx509.SignatureAlgorithm = xsaNone) then
    begin
      if Assigned(fLog) then
        fLog.Log(sllWarning, 'Cannot find certificate signature algorithm.');
      Exit;
    end;

    // Get purpose
    PublishedCertificate.intendedPurpose := [];
    for ceku in Certx509.Signed.ExtendedKeyUsages do
      Insert(CEKU_FULLTEXT[ceku], PublishedCertificate.intendedPurpose, Length(PublishedCertificate.intendedPurpose));

    // Set Data
    PublishedCertificate.Certificate := s;
    PublishedCertificate.issuedTo := Certx509.Subject[xaCN];
    PublishedCertificate.issuedBy := Certx509.Issuer[xaCN];
    PublishedCertificate.expirationData := Certx509.NotAfter;
    result := True;
  finally
    FreeAndNil(Certx509);
  end;
end;

constructor TFrmPropertyPublishedCertificates.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);

  Caption := 'Published Certificates';

  UnifyButtonsWidth([BitBtn_Add, BitBtn_Copy, BitBtn_Remove]);
end;

procedure TFrmPropertyPublishedCertificates.Update(Props: TProperty);
var
  UserCert: RawUtf8;
  PublishedCertificate: TPublishedCertificate;
  Row: TDocVariantData;
  UserCertificate: TLdapAttribute;
  i: Integer;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Update', Self);

  fProperty := Props;

  TisGrid_ListX509.Clear;
  TisGrid_ListX509.BeginUpdate;
  try
    UserCertificate := fProperty.Get('userCertificate');
    if not Assigned(UserCertificate) then
      Exit;
    Row.Init();
    for i := 0 to Pred(UserCertificate.Count) do
    begin
      UserCert := UserCertificate.GetRaw(i);
      if UserCert = '' then
      begin
        if Assigned(fLog) then
          fLog.Log(sllInfo, 'Empty cert in list. Continue.');
        continue;
      end;
      if not CertToDoc(UserCert, PublishedCertificate) then
      begin
        if Assigned(fLog) then
          fLog.Log(sllWarning, 'Cannot convert certificate to TPublishedCertificate.');
        continue;
      end;
      Row.AddValue('certificate', PublishedCertificate.Certificate);
      Row.AddValue('issuedTo', PublishedCertificate.issuedTo);
      Row.AddValue('issuedBy', PublishedCertificate.issuedBy);
      Row.AddValue('intendedPurposes', String.join(', ', TStringArray(PublishedCertificate.intendedPurpose)));
      Row.AddValue('expirationDate', DateToStr(PublishedCertificate.expirationData));
      TisGrid_ListX509.Data.AddItem(Row);
      Row.Clear;
    end;
  finally
    TisGrid_ListX509.EndUpdate;
    TisGrid_ListX509.LoadData;
  end;
end;

end.

