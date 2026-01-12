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
  Row: TDocVariantData;
  MsFVERecoveryInformations: TFVERecoveryInformationDynArray;
  MsFVERecoveryInformation: TFVERecoveryInformation;
  dNSHostName: RawUtf8;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Update', Self);

  fProperty := Props;

  Row.Init();
  MsFVERecoveryInformations := fProperty.msFVERecoveryInformation;
  dNSHostName := fProperty.GetReadable('dNSHostName');
  TisGrid1.BeginUpdate;
  try
    for MsFVERecoveryInformation in MsFVERecoveryInformations do
    begin
      Row.AddValue('date', String(MsFVERecoveryInformation.cn).Substring(0, 25));
      Row.AddValue('passwordID', String(MsFVERecoveryInformation.cn).Substring(26, 36));
      Row.AddValue('recoveryPassword', MsFVERecoveryInformation.msFVERecoveryPassword);
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

