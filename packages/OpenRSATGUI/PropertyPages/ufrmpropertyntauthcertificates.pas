unit ufrmpropertyntauthcertificates;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls, ExtCtrls, Buttons, ActnList, Dialogs,
  mormot.core.base,
  mormot.core.log,
  mormot.core.text,
  upropertyframe,
  uproperty, tis.ui.grid.core;

type

  { TFrmPropertyNTAuthCertificates }

  TFrmPropertyNTAuthCertificates = class(TPropertyFrame)
    Action_Export: TAction;
    Action_Add: TAction;
    Action_Delete: TAction;
    Action_View: TAction;
    ActionList1: TActionList;
    BitBtn_Export: TBitBtn;
    BitBtn_Add: TBitBtn;
    BitBtn_Del: TBitBtn;
    BitBtn_View: TBitBtn;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    SaveDialog1: TSaveDialog;
    TisGrid1: TTisGrid;
    procedure Action_ExportExecute(Sender: TObject);
    procedure Action_AddExecute(Sender: TObject);
    procedure Action_DeleteExecute(Sender: TObject);
    procedure Action_DeleteUpdate(Sender: TObject);
    procedure Action_ExportUpdate(Sender: TObject);
    procedure Action_ViewExecute(Sender: TObject);
    procedure Action_ViewUpdate(Sender: TObject);
    procedure TisGrid1DblClick(Sender: TObject);
  private
    fLog: TSynLog;
    fProperty: TProperty;

    procedure AddFromFiles(const FileNames: array of string);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Update(Props: TProperty); override;
    procedure DropFiles(const FileNames: array of string); override;

  end;

implementation
uses
  LCLIntf,
  mormot.core.os,
  mormot.core.variants,
  mormot.core.os.security,
  mormot.crypt.x509,
  mormot.crypt.secure,
  ucommonui;

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
    'Ms Publisher');

{$R *.lfm}

{ TFrmPropertyNTAuthCertificates }

procedure TFrmPropertyNTAuthCertificates.Action_AddExecute(Sender: TObject);
begin
  if not OpenDialog1.Execute() then
    Exit;

  AddFromFiles(OpenDialog1.Files.ToStringArray);
end;

procedure TFrmPropertyNTAuthCertificates.Action_ExportExecute(Sender: TObject);
var
  Row: PDocVariantData;
  FileName: String;
begin
  Row := TisGrid1.FocusedRow;
  if not Assigned(Row) then
    Exit;

  SaveDialog1.Filter := 'crt|.crt';
  SaveDialog1.FileName := FormatUtf8('%.crt', [Row^.U['name']]);
  if not SaveDialog1.Execute then
    Exit;

  FileName := SaveDialog1.FileName;
  if not FileName.EndsWith('.crt') then
    FileName := FormatUtf8('%.crt', [FileName]);
  if not FileFromString(Row^.U['certificate'], FileName) then
    Exit;
end;

procedure TFrmPropertyNTAuthCertificates.Action_DeleteExecute(Sender: TObject);
begin
  fProperty.DelNTAuthCertificate(TisGrid1.FocusedRow^.U['certificate']);
  Update(fProperty);
end;

procedure TFrmPropertyNTAuthCertificates.Action_DeleteUpdate(Sender: TObject);
begin
  Action_Delete.Enabled := Assigned(TisGrid1.FocusedRow);
end;

procedure TFrmPropertyNTAuthCertificates.Action_ExportUpdate(Sender: TObject);
begin
  Action_Export.Enabled := Assigned(TisGrid1.FocusedRow);
end;

procedure TFrmPropertyNTAuthCertificates.Action_ViewExecute(Sender: TObject);
var
  TempDir, TempFile, C: RawUtf8;
  Row: PDocVariantData;
begin
  TempDir := GetTempDir();
  if TempDir = '' then
    Exit;
  Row := TisGrid1.FocusedRow;
  if not Assigned(Row) then
    Exit;
  TempFile := FormatUtf8('%%.tmp.crt', [TempDir, Row^.U['name']]);

  C := Row^.U['certificate'];
  if not FileFromString(C, TempFile) then
    Exit;
  OpenDocument(TempFile);
end;

procedure TFrmPropertyNTAuthCertificates.Action_ViewUpdate(Sender: TObject);
begin
  Action_View.Enabled := Assigned(TisGrid1.FocusedRow);
end;

procedure TFrmPropertyNTAuthCertificates.TisGrid1DblClick(Sender: TObject);
begin
  Action_View.Execute;
end;

procedure TFrmPropertyNTAuthCertificates.AddFromFiles(
  const FileNames: array of string);
var
  CertX509: TX509;
  FilePath, FileContent: String;
  der: TCertDer;
begin
  CertX509 := TX509.Create;
  try
    for FilePath in FileNames do
    begin
      FileContent := StringFromFile(FilePath);
      der := PemToDer(FileContent);
      if not AsnDecChunk(der) then
        Continue;
      if not CertX509.LoadFromDer(der) then
      begin
        if assigned(fLog) then
          fLog.Log(sllWarning, 'Invalid certificate. (%)', [FilePath], Self);
        MessageDlg('Invalid Certificate', FormatUtf8('Certificate cannot be added. (%)', [FilePath]), mtWarning, mbOKCancel, 0);
        Continue;
      end;
      fProperty.AddNTAuthCertificate(FileContent);
    end;
  finally
    FreeAndNil(CertX509);
    Update(fProperty);
  end;
end;

constructor TFrmPropertyNTAuthCertificates.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);

  Caption := 'NTAuthCertificates';

  UnifyButtonsWidth([BitBtn_Add, BitBtn_Del, BitBtn_View, BitBtn_Export]);
end;

procedure TFrmPropertyNTAuthCertificates.Update(Props: TProperty);
var
  CACertificates: TRawByteStringDynArray;
  i, j: Integer;
  der: TCertDer;
  CertX509: TX509;
  NewRow: TDocVariantData;
  CurrentDateTime: TDateTime;
  IntendedPurposes: TStringArray;
  ceku: TXExtendedKeyUsage;
begin
  fProperty := Props;

  TisGrid1.Clear;
  CurrentDateTime := Now;
  NewRow.Init();
  CACertificates := fProperty.NTAuthCertificates;
  CertX509 := TX509.Create;
  try
    for i := 0 to Pred(Length(CACertificates)) do
    begin
      CertX509.Clear;
      der := PemToDer(CACertificates[i]);
      if not AsnDecChunk(der) then
        Continue;
      if not CertX509.LoadFromDer(der) then
        continue;
      if (Certx509.SignatureAlgorithm = xsaNone) then
        Continue;
      newRow.AddValue('name', CertX509.Subject[xaCN]);
      newRow.AddValue('status', (CertX509.NotBefore <= CurrentDateTime) and (CertX509.NotAfter >= CurrentDateTime));
      newRow.AddValue('issuedBy', CertX509.Issuer[xaCN]);
      j := 0;
      for ceku in CertX509.Signed.ExtendedKeyUsages do
      begin
        Inc(j);
        Insert(CEKU_FULLTEXT[ceku], IntendedPurposes, j);
      end;
      NewRow.AddValue('intendedPurposes', String.Join(', ', IntendedPurposes));
      newRow.AddValue('expirationDate', DateToStr(CertX509.NotAfter));
      newRow.AddValue('certificate', CACertificates[i]);
      TisGrid1.Data.AddItem(NewRow);
      NewRow.Clear;
    end;
  finally
    FreeAndNil(CertX509);
    TisGrid1.LoadData();
  end;
end;

procedure TFrmPropertyNTAuthCertificates.DropFiles(
  const FileNames: array of string);
begin
  AddFromFiles(FileNames);
end;

end.

