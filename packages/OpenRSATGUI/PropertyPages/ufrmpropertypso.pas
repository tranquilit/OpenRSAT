unit ufrmpropertypso;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  StdCtrls,
  ExtCtrls,
  Buttons,
  Graphics,
  ActnList,
  tis.ui.grid.core,
  mormot.core.base,
  mormot.core.text,
  mormot.core.variants,
  mormot.net.ldap,
  uproperty,
  uhintwindow,
  ucommon,
  upropertyframe;

type

  { TFrmPropertyPSO }

  /// Documentation about PSO parameters:
  /// - msDS-MinimumPasswordLength (integer) (default=7) (value=0-255)
  /// - msDS-PasswordHistoryLength (integer) (default=24) (value=0-24)
  /// - msDS-MaximumPasswordAge (TimeSpan?) (default=42 days) (value=1-999 days)
  /// - msDS-MinimumPasswordAge (TimeSpan?) (default=1 day) (value=1-998 days)
  /// - msDS-PasswordComplexityEnabled (Boolean) (default=true) (value=false/true)
  /// - msDS-PasswordReversibleEncryptionEnabled (Boolean) (default=false) (value=false/true)
  /// -
  /// - msDS-LockoutThreshold (integer) (default=0) (value=0-999)
  /// - msDS-LockoutDuration (TimeSpan) (default=Undefined) (value=1-99999 minutes)
  /// - msDS-LockoutObservationWindow (TimeSpan) (default=Undefined) (value=1-99999 minutes)
  /// TimeSpanToSeconds: Abs(Value) div 10000000
  /// Source:
  /// - https://byebyeprof.com/fr/Intranet/PSO
  TFrmPropertyPSO = class(TPropertyFrame)
    Action_Add: TAction;
    Action_Remove: TAction;
    ActionList1: TActionList;
    BitBtn_Add: TBitBtn;
    BitBtn_Remove: TBitBtn;
    CheckBox_PwdComplexity: TCheckBox;
    CheckBox_PwdReversibleEncryption: TCheckBox;
    Edit_Name: TEdit;
    Edit_Precedence: TEdit;
    Edit_MinPwdLength: TEdit;
    Edit_PwdHistoryLength: TEdit;
    Edit_MinPwdAge: TEdit;
    Edit_MaxPwdAge: TEdit;
    Edit_LockoutPwdObservationWindow: TEdit;
    Edit_LockoutPwdThreshold: TEdit;
    GroupBox_PasswordSettings: TGroupBox;
    GroupBox_AppliesTo: TGroupBox;
    Label_Name: TLabel;
    Label_LockoutPwdThreshold: TLabel;
    Label_LockoutPwdObservationWindow: TLabel;
    Label_Precedence: TLabel;
    Label_MinPwdLength: TLabel;
    Label_PwdHistoryLength: TLabel;
    Label_PwdComplexity: TLabel;
    Label_PwdReversibleEncryption: TLabel;
    Label_MinPwdAge: TLabel;
    Label_MaxPwdAge: TLabel;
    Panel_Name: TPanel;
    Panel_LockoutPwdThreshold: TPanel;
    Panel_LockoutPwdObservationWindow: TPanel;
    Panel_Precedence: TPanel;
    Panel_MinPwdLength: TPanel;
    Panel_PwdHistoryLength: TPanel;
    Panel_PwdComplexity: TPanel;
    Panel_PwdReversibleEncryption: TPanel;
    Panel_MinPwdAge: TPanel;
    Panel_MaxPwdAge: TPanel;
    Panel9: TPanel;
    ScrollBox1: TScrollBox;
    TisGrid_AppliesTo: TTisGrid;
    procedure CheckBox_PwdComplexityChange(Sender: TObject);
    procedure CheckBox_PwdReversibleEncryptionChange(Sender: TObject);
    procedure Edit_LockoutPwdObservationWindowChange(Sender: TObject);
    procedure Edit_LockoutPwdThresholdChange(Sender: TObject);
    procedure Edit_MaxPwdAgeChange(Sender: TObject);
    procedure Edit_MinPwdAgeChange(Sender: TObject);
    procedure Edit_MinPwdLengthChange(Sender: TObject);
    procedure Edit_PrecedenceChange(Sender: TObject);
    procedure Edit_PwdHistoryLengthChange(Sender: TObject);
  private
    fProperty: TProperty;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Update(Props: TProperty); override;

  end;

implementation

{$R *.lfm}

{ TFrmPropertyPSO }

procedure TFrmPropertyPSO.Edit_PrecedenceChange(Sender: TObject);
var
  v: int64;
begin
  if TryStrToInt64(Edit_Precedence.Text, v) and (v > 0) then
  begin
    Edit_Precedence.Font.Color := clDefault;
    HideHintWindow;
    fProperty.Add('msDS-PasswordSettingsPrecedence', Edit_Precedence.Text);
  end
  else
  begin
    Edit_Precedence.Font.Color := clRed;
    ShowHintWindow(Edit_Precedence, rsPSOInvalidPrecedence, 5000);
    fProperty.Restore('msDS-PasswordSettingsPrecedence');
  end;
end;

procedure TFrmPropertyPSO.Edit_PwdHistoryLengthChange(Sender: TObject);
var
  v: int64;
begin
  if TryStrToInt64(Edit_PwdHistoryLength.Text, v) and (v >= 0) and (v <= 24) then
  begin
    Edit_PwdHistoryLength.Font.Color := clDefault;
    HideHintWindow;
    fProperty.Add('msDS-PasswordHistoryLength', Edit_PwdHistoryLength.Text);
  end
  else
  begin
    Edit_PwdHistoryLength.Font.Color := clRed;
    ShowHintWindow(Edit_PwdHistoryLength, rsPSOInvalidPwdHistory, 5000);
    fProperty.Restore('msDS-PasswordHistoryLength');
  end;
end;

procedure TFrmPropertyPSO.Edit_MinPwdLengthChange(Sender: TObject);
var
  v: int64;
begin
  if TryStrToInt64(Edit_MinPwdLength.Text, v) and (v >= 0) and (v <= 255) then
  begin
    Edit_MinPwdLength.Font.Color := clDefault;
    HideHintWindow;
    fProperty.Add('msDS-MinimumPasswordLength', Edit_MinPwdLength.Text);
  end
  else
  begin
    Edit_MinPwdLength.Font.Color := clRed;
    ShowHintWindow(Edit_MinPwdLength, rsPSOInvalidMinPwdLength, 5000);
    fProperty.Restore('msDS-MinimumPasswordLength');
  end;
end;

procedure TFrmPropertyPSO.CheckBox_PwdComplexityChange(Sender: TObject);
begin
  if CheckBox_PwdComplexity.Checked then
    fProperty.Add('msDS-PasswordComplexityEnabled', 'TRUE')
  else
    fProperty.Add('msDS-PasswordComplexityEnabled', 'FALSE');
end;

procedure TFrmPropertyPSO.CheckBox_PwdReversibleEncryptionChange(Sender: TObject
  );
begin
  if CheckBox_PwdReversibleEncryption.Checked then
    fProperty.Add('msDS-PasswordReversibleEncryptionEnabled', 'TRUE')
  else
    fProperty.Add('msDS-PasswordReversibleEncryptionEnabled', 'FALSE');
end;

procedure TFrmPropertyPSO.Edit_LockoutPwdObservationWindowChange(Sender: TObject
  );
var
  v: int64;
begin
  if TryStrToInt64(Edit_LockoutPwdObservationWindow.Text, v) and (v >= 1) and (v <= 99999) then
  begin
    v := v * 60 * 10000000;
    Edit_LockoutPwdObservationWindow.Font.Color := clDefault;
    HideHintWindow;
    fProperty.Add('msDS-LockoutObservationWindow', IntToStr(-v));
  end
  else
  begin
    Edit_LockoutPwdObservationWindow.Font.Color := clRed;
    ShowHintWindow(Edit_LockoutPwdObservationWindow, rsPSOInvalidLockoutPwdObservationWindow, 5000);
    fProperty.Restore('msDS-LockoutObservationWindow');
  end;
end;

procedure TFrmPropertyPSO.Edit_LockoutPwdThresholdChange(Sender: TObject);
var
  v: int64;
begin
  if TryStrToInt64(Edit_LockoutPwdThreshold.Text, v) and (v >= 0) and (v <= 999) then
  begin
    Edit_LockoutPwdThreshold.Font.Color := clDefault;
    HideHintWindow;
    fProperty.Add('msDS-LockoutThreshold', Edit_LockoutPwdThreshold.Text);
  end
  else
  begin
    Edit_LockoutPwdThreshold.Font.Color := clRed;
    ShowHintWindow(Edit_LockoutPwdThreshold, rsPSOInvalidLockoutPwdThreshold);
    fProperty.Restore('msDS-LockoutThreshold');
  end;
end;

procedure TFrmPropertyPSO.Edit_MaxPwdAgeChange(Sender: TObject);
var
  v: int64;
begin
  if TryStrToInt64(Edit_MaxPwdAge.Text, v) and (v >= 1) and (v <= 999) then
  begin
    v := v * (24 * 3600) * 10000000;
    Edit_MaxPwdAge.Font.Color := clDefault;
    HideHintWindow;
    fProperty.Add('msDS-MaximumPasswordAge', IntToStr(-v));
  end
  else
  begin
    Edit_MaxPwdAge.Font.Color := clRed;
    ShowHintWindow(Edit_MaxPwdAge, rsPSOInvalidMaxPwdAge, 5000);
    fProperty.Restore('msDS-MaximumPasswordAge');
  end;
end;

procedure TFrmPropertyPSO.Edit_MinPwdAgeChange(Sender: TObject);
var
  v: int64;
begin
  if TryStrToInt64(Edit_MinPwdAge.Text, v) and (v >= 0) and (v <= 998) then
  begin
    v := v * (24 * 3600) * 10000000;
    Edit_MinPwdAge.Font.Color := clDefault;
    HideHintWindow;
    fProperty.Add('msDS-MinimumPasswordAge', IntToStr(-v));
  end
  else
  begin
    Edit_MinPwdAge.Font.Color := clRed;
    ShowHintWindow(Edit_MinPwdAge, rsPSOInvalidMinPwdAge, 5000);
    fProperty.Restore('msDS-MinimumPasswordAge');
  end;
end;

constructor TFrmPropertyPSO.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Caption := 'PSO';
end;

procedure TFrmPropertyPSO.Update(Props: TProperty);
var
  AppliesTo: TLdapAttribute;
  i: Integer;
  AppliesToData, SearchResultData, Row: TDocVariantData;
  Filter: RawUtf8;
  DN: RawByteString;
  P: PDocVariantData;
  v: int64;
begin
  fProperty := Props;

  Edit_Name.Text := fProperty.name;
  Edit_Precedence.Text := fProperty.GetRaw('msDS-PasswordSettingsPrecedence');
  Edit_MinPwdLength.Text := fProperty.GetRaw('msDS-MinimumPasswordLength');
  Edit_PwdHistoryLength.Text := fProperty.GetRaw('msDS-PasswordHistoryLength');
  CheckBox_PwdComplexity.Checked := fProperty.GetRaw('msDS-PasswordComplexityEnabled') <> 'FALSE';
  CheckBox_PwdReversibleEncryption.Checked := fProperty.GetRaw('msDS-PasswordReversibleEncryptionEnabled') <> 'FALSE';
  if TryStrToInt64(fProperty.GetRaw('msDS-MinimumPasswordAge'), v) then
    Edit_MinPwdAge.Text := IntToStr((Abs(v) div 10000000) div (3600 * 24));
  if TryStrToInt64(fProperty.GetRaw('msDS-MaximumPasswordAge'), v) then
    Edit_MaxPwdAge.Text := IntToStr((Abs(v) div 10000000) div (3600 * 24));
  Edit_LockoutPwdThreshold.Text := fProperty.GetRaw('msDS-LockoutThreshold');
  if TryStrToInt64(fProperty.GetRaw('msDS-LockoutObservationWindow'), v) then
    Edit_LockoutPwdObservationWindow.Text := IntToStr((Abs(v) div 10000000) div 60);

  TisGrid_AppliesTo.Clear;
  AppliesTo := fProperty.Get('msDS-PSOAppliesTo');
  if Assigned(AppliesTo) then
  begin
    Filter := '';
    for i := 0 to AppliesTo.Count - 1 do
       Filter := FormatUtf8('%(distinguishedName=%)', [Filter, LdapEscape(AppliesTo.GetRaw(i))]);
    if Filter = '' then
      Exit;
    Filter := FormatUtf8('(|%)', [Filter]);

    fProperty.LdapClient.SearchScope := lssWholeSubtree;
    if not fProperty.LdapClient.SearchAllDocRaw(SearchResultData, fProperty.LdapClient.DefaultDN, Filter, ['name', 'mail'], [roRawValues, roObjectNameAtRoot, roKnownValuesAsArray]) then
      Exit;

    AppliesToData.Init(JSON_FAST);
    TisGrid_AppliesTo.BeginUpdate;
    try
      for i := 0 to AppliesTo.Count - 1 do
      begin
        DN := AppliesTo.GetRaw(i);
        if not SearchResultData.Exists(DN) then
          Continue;
        P := SearchResultData.O[DN];
        if not Assigned(P) then
          Continue;

        Row.Init(JSON_FAST);
        Row.U['distinguishedName'] := AppliesTo.GetRaw(i);
        Row.U['name'] := P^.U['name'];
        Row.U['mail'] := P^.U['mail'];
        AppliesToData.AddItem(Row);
        Row.Clear;
      end;
    finally
      TisGrid_AppliesTo.EndUpdate;
      TisGrid_AppliesTo.LoadData(@AppliesToData);
    end;
  end;
end;

end.

