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
  Menus,
  ActnList,
  tis.ui.grid.core,
  mormot.core.base,
  mormot.core.log,
  mormot.core.variants,
  uproperty,
  upropertyframe,
  VirtualTrees;

type

  { TFrmPropertyBitLocker }

  TFrmPropertyBitLocker = class(TPropertyFrame)
    Action_CopyPassword: TAction;
    ActionList1: TActionList;
    Label_BitclockerDetails: TLabel;
    Label_BitlockerPassword: TLabel;
    Memo1: TMemo;
    MenuItem_CopyPassword: TMenuItem;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    TisGrid1: TTisGrid;
    procedure Action_CopyPasswordExecute(Sender: TObject);
    procedure Action_CopyPasswordUpdate(Sender: TObject);
    procedure TisGrid1Change(Sender: TBaseVirtualTree; Node: PVirtualNode);
  private
    fLog: TSynLog;
    fProperty: TProperty;

    procedure LoadDetails(PData: PDocVariantData);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Update(Props: TProperty); override;
  end;

implementation
uses
  Clipbrd,
  ucommon,
  mormot.core.text,
  mormot.net.ldap;

{$R *.lfm}

{ TFrmPropertyBitLocker }

procedure TFrmPropertyBitLocker.TisGrid1Change(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  LoadDetails(TisGrid1.GetNodeAsPDocVariantData(Node, False));
end;

procedure TFrmPropertyBitLocker.Action_CopyPasswordExecute(Sender: TObject);
begin
  Clipboard.AsText := TisGrid1.SelectedRows._[0]^.S['recoveryPassword'];
end;

procedure TFrmPropertyBitLocker.Action_CopyPasswordUpdate(Sender: TObject);
begin
  Action_CopyPassword.Enabled := (TisGrid1.SelectedCount = 1) and (TisGrid1.SelectedRows._[0]^.S['recoveryPassword'] <> '');
end;

procedure TFrmPropertyBitLocker.LoadDetails(PData: PDocVariantData);
begin
  if not Assigned(PData) then
    Exit;
  Memo1.Clear;
  Memo1.Append(rsBitlockerRecoveryPassword);
  Memo1.Append(FormatUtf8('        %', [PData^.S['recoveryPassword']]));
  Memo1.Append(FormatUtf8('%%', [rsBitlockerComputer, PData^.S['computer']]));
  Memo1.Append(FormatUtf8('%%', [rsBitlockerDate, PData^.S['date']]));
  Memo1.Append(FormatUtf8('%%', [rsBitlockerPasswordID, PData^.S['passwordID']]));
end;

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

