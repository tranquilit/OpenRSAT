unit ufrmpropertybitlocker;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  StdCtrls,
  ExtCtrls,
  tis.ui.grid.core,
  mormot.core.base,
  mormot.core.log,
  uproperty,
  upropertyframe;

type

  { TFrmPropertyBitLocker }

  TFrmPropertyBitLocker = class(TPropertyFrame)
    Label_BitclockerDetails: TLabel;
    Label_BitlockerPassword: TLabel;
    Memo1: TMemo;
    Panel1: TPanel;
    TisGrid1: TTisGrid;
  private
    fLog: TSynLog;
    fProperty: TProperty;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Update(Props: TProperty); override;
  end;

implementation
uses
  mormot.core.variants,
  mormot.net.ldap;

{$R *.lfm}

{ TFrmPropertyBitLocker }

constructor TFrmPropertyBitLocker.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);

  Caption := 'BitLocker recovery';
end;

procedure TFrmPropertyBitLocker.Update(Props: TProperty);
var
  MsFVERecoveryInformation: TLdapResultList;
  LdapResult: TLdapResult;
  cn, recoveryPassword, dNSHostName: RawUtf8;
  Row: TDocVariantData;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Update', Self);

  fProperty := Props;

  Row.Init();
  MsFVERecoveryInformation := fProperty.msFVERecoveryInformation;
  TisGrid1.BeginUpdate;
  try
    for LdapResult in MsFVERecoveryInformation.Items do
    begin
      cn := LdapResult.Find('cn').GetReadable();
      recoveryPassword := LdapResult.Find('msFVE-RecoveryPassword').GetReadable();
      dNSHostName := fProperty.GetReadable('dNSHostName');

      Row.AddValue('date', String(cn).Substring(0, 25));
      Row.AddValue('passwordID', String(cn).Substring(26, 36));
      Row.AddValue('recoveryPassword', recoveryPassword);
      Row.AddValue('computer', dNSHostName);
      TisGrid1.Data.AddItem(Row);
      Row.Clear;
    end;
  finally
    TisGrid1.EndUpdate;
    TisGrid1.LoadData;
  end;
end;

end.

