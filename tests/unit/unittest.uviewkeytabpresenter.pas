unit unittest.uviewkeytabpresenter;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  base64,
  mormot.core.test,
  mormot.core.os.security,
  uviewkeytabpresenter,
  fixture.fakevisviewkeytabpresenter;

const
  cKeyTabBinBase64: RawByteString = 'BQIAAAA/AAEADE9QRU5SU0FULkxBTgAMS0VZVEFCLVRFU1QkAAAAAWoMF0wCABEAEOX/3ocPjGhY9SnDwCaoNpsAAAACAAAATwABAAxPUEVOUlNBVC5MQU4ADEtFWVRBQi1URVNUJAAAAAFqDBdMAgASACDlp75piKYNrT3kx8pQ/0MTvEO6y15e04yoWA0L4tsfjQAAAAIAAABRAAIADE9QRU5SU0FULkxBTgAESFRUUAAYa2V5dGFiLXRlc3Qub3BlbnJzYXQubGFuAAAAAWoMF0wCABEAEJttVEELVNnNjKSqNWaTm5wAAAACAAAAYQACAAxPUEVOUlNBVC5MQU4ABEhUVFAAGGtleXRhYi10ZXN0Lm9wZW5yc2F0LmxhbgAAAAFqDBdMAgASACA+Wpdw/sl1jN6TcboL3n5x6GFFLRvKcPG8kEB5XqyrdgAAAAI=';

type

  { TUnitTestViewKeyTabPresenter }

  TUnitTestViewKeyTabPresenter = class(TSynTestCase)
  private
    fView: TFakeVisViewKeyTabPresenter;
    fPresenter: TViewKeyTabPresenter;

  public
    procedure MethodSetup; override;
    procedure MethodCleanUp; override;

  published
    procedure LoadFromKeyTab;
    procedure CopyToClipboard;
  end;

implementation

{ TUnitTestViewKeyTabPresenter }

procedure TUnitTestViewKeyTabPresenter.MethodSetup;
begin
  fView := TFakeVisViewKeyTabPresenter.Create;
  fPresenter := TViewKeyTabPresenter.Create(fView);
end;

procedure TUnitTestViewKeyTabPresenter.MethodCleanUp;
begin
  FreeAndNil(fView);
  FreeAndNil(fPresenter);
end;

procedure TUnitTestViewKeyTabPresenter.LoadFromKeyTab;
var
  KeyTab: TKerberosKeyTab;
begin
  // Prepare KeyTab
  KeyTab := TKerberosKeyTab.Create;
  KeyTab.LoadFromBinary(DecodeStringBase64(cKeyTabBinBase64));

  // Load KeyTab
  fPresenter.LoadFromKeyTab(KeyTab);

  // Checks
  Check(fView.Entries.Count = 4, 'KeyTab contains 4 entries.');
  Check(fView.Entries._[0]^.I['kvno'] = 2, 'KVNO is set to 2.');
  Check(fView.Entries._[0]^.S['timestamp'] = '19/05/2026 07:54:52', 'Timestamp is 19/05/2026 07:54:52.');
  Check(fView.Entries._[0]^.S['principal'] = 'KEYTAB-TEST$@OPENRSAT.LAN', 'Principal is KEYTAB-TEST$@OPENRSAT.LAN.');
  Check(fView.Entries._[0]^.S['encryptionType'] = 'aes128-cts-hmac-sha1-96', 'EncryptionType is aes128-cts-hmac-sha1-96.');
end;

procedure TUnitTestViewKeyTabPresenter.CopyToClipboard;
var
  KeyTab: TKerberosKeyTab;
begin
  KeyTab := TKerberosKeyTab.Create;
  KeyTab.LoadFromBinary(DecodeStringBase64(cKeyTabBinBase64));

  fPresenter.LoadFromKeyTab(KeyTab);
  fPresenter.CopyToClipboard;

  Check(fView.ClipboardContent = cKeyTabBinBase64, 'Clipboard content is based64.');
end;

end.

