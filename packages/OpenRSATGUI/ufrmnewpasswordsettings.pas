unit ufrmnewpasswordsettings;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  ActnList,
  ComCtrls,
  ExtCtrls,
  Buttons, 
  StdCtrls,
  mormot.core.base,
  mormot.core.text,
  mormot.core.variants,
  mormot.net.ldap,
  tis.ui.grid.core,
  uvisobjectsselector;

type

  { TFrmNewPasswordSettings }

  TFrmNewPasswordSettings = class(TFrame)
    Action_Back: TAction;
    Action_Add: TAction;
    Action_Remove: TAction;
    Action_Next: TAction;
    ActionList1: TActionList;
    BitBtn_Add: TBitBtn;
    BitBtn_Remove: TBitBtn;
    CheckBox_PwdComplexity: TCheckBox;
    CheckBox_PwdReversibleEncryption: TCheckBox;
    Edit_LockoutDuration: TEdit;
    Edit_Name: TEdit;
    Edit_Precedence: TEdit;
    Edit_MinPwdLength: TEdit;
    Edit_PwdHistoryLength: TEdit;
    Edit_MinPwdAge: TEdit;
    Edit_MaxPwdAge: TEdit;
    Edit_LockoutThreshold: TEdit;
    Edit_LockoutObservationWindow: TEdit;
    Label_LockoutDuration: TLabel;
    Label_Name: TLabel;
    Label_LockoutObservationWindow: TLabel;
    Label_Precedence: TLabel;
    Label_MinPwdLength: TLabel;
    Label_PwdHistoryLength: TLabel;
    Label_PwdComplexity: TLabel;
    Label_PwdReversibleEncryption: TLabel;
    Label_MinPwdAge: TLabel;
    Label_MaxPwdAge: TLabel;
    Label_LockoutThreshold: TLabel;
    PageControl1: TPageControl;
    Panel_LockoutDuration: TPanel;
    Panel_Actions: TPanel;
    Panel_LockoutThreshold: TPanel;
    Panel_LockoutObservationWindow: TPanel;
    Panel_Name: TPanel;
    Panel_Precedence: TPanel;
    Panel_MinPwdLength: TPanel;
    Panel_PwdHistoryLength: TPanel;
    Panel_PwdComplexity: TPanel;
    Panel_PwdReversibleEncryption: TPanel;
    Panel_MinPwdAge: TPanel;
    Panel_MaxPwdAge: TPanel;
    ScrollBox1: TScrollBox;
    TabSheet_Settings: TTabSheet;
    TabSheet_AppliesTo: TTabSheet;
    TisGrid_AppliesTo: TTisGrid;
    procedure Action_AddExecute(Sender: TObject);
    procedure Action_BackExecute(Sender: TObject);
    procedure Action_NextExecute(Sender: TObject);
    procedure Action_RemoveExecute(Sender: TObject);
  private
    function GetDistinguishedName: RawUtf8;
  private
    fLdap: TLdapClient;

    function CheckSettings: Boolean;
    procedure Finish;
    procedure AppliesToAdd; overload;
    procedure AppliesToAdd(const SelectedObjects: TRawUtf8DynArray); overload;
    procedure AppliesToRemove;

    procedure UpdateGrid;

    function ShowObjectsSelector(out SelectedObjects: TRawUtf8DynArray): Boolean;

    property DistinguishedName: RawUtf8 read GetDistinguishedName;
  public
    constructor Create(TheOwner: TComponent; ALdap: TLdapClient);
  end;

implementation
uses
  uhintwindow,
  ucommon,
  uvisnewobject;

{$R *.lfm}

{ TFrmNewPasswordSettings }

procedure TFrmNewPasswordSettings.Action_NextExecute(Sender: TObject);
begin
  case PageControl1.ActivePageIndex of
    0: if CheckSettings then PageControl1.ActivePageIndex := 1;
    1: Finish;
  end;
end;

procedure TFrmNewPasswordSettings.Action_RemoveExecute(Sender: TObject);
begin
  AppliesToRemove;
end;

function TFrmNewPasswordSettings.GetDistinguishedName: RawUtf8;
begin
  result := FormatUtf8('CN=%,%', [LdapEscape(Edit_Name.Text), (Owner as TVisNewObject).ObjectOU]);
end;

function TFrmNewPasswordSettings.CheckSettings: Boolean;
var
  Attribute: TLdapAttribute;
  v: Int64;
begin
  result := False;

  Attribute := fLdap.SearchObject(DistinguishedName, '', 'cn');
  if Assigned(Attribute) then
  begin
    ShowHintWindow(Edit_Name, 'Already exists.', 5000);
    Exit;
  end;

  if not (ToInt64(Edit_Precedence.Text, v) and (v > 0)) then
  begin
    ShowHintWindow(Edit_Precedence, 'Invalid precedence (X > 0)', 5000);
    Exit;
  end;

  if not (ToInt64(Edit_MinPwdLength.Text, v) and (v >= 0) and (v <= 255)) then
  begin
    ShowHintWindow(Edit_MinPwdLength, 'Invalid password length (0 <= X <= 255)', 5000);
    Exit;
  end;

  if not (ToInt64(Edit_PwdHistoryLength.Text, v) and (v >= 0) and (v <= 24)) then
  begin
    ShowHintWindow(Edit_PwdHistoryLength, 'Invalid password history length (0 <= X <= 24)', 5000);
    Exit;
  end;

  if not (ToInt64(Edit_MinPwdAge.Text, v) and (v >= 0) and (v <= 998)) then
  begin
    ShowHintWindow(Edit_MinPwdAge, 'Invalid minimum password age (0 <= X <= 998)', 5000);
    Exit;
  end;

  if not (ToInt64(Edit_MaxPwdAge.Text, v) and (v >= 1) and (v <= 999)) then
  begin
    ShowHintWindow(Edit_MaxPwdAge, 'Invalid maximum password age (1 <= X <= 999)', 5000);
    Exit;
  end;

  if not (ToInt64(Edit_LockoutThreshold.Text, v) and (v >= 0) and (v <= 999)) then
  begin
    ShowHintWindow(Edit_LockoutThreshold, 'Invalid lockout threshold (0 <= X <= 999)', 5000);
    Exit;
  end;

  if not (ToInt64(Edit_LockoutObservationWindow.Text, v) and (v >= 0) and (v <= 99999)) then
  begin
    ShowHintWindow(Edit_LockoutObservationWindow, 'Invalid lockout observation window (0 <= X <= 99999)', 5000);
    Exit;
  end;

  if not (ToInt64(Edit_LockoutDuration.Text, v) and (v >= 0) and (v <= 99999)) then
  begin
    ShowHintWindow(Edit_LockoutDuration, 'Invalid lockout duration (0 <= X <= 99999)', 5000);
    Exit;
  end;
  result := True;
end;

procedure TFrmNewPasswordSettings.Action_BackExecute(Sender: TObject);
begin
  case PageControl1.ActivePageIndex of
    1: PageControl1.ActivePageIndex := 0;
  end;
end;

procedure TFrmNewPasswordSettings.Action_AddExecute(Sender: TObject);
begin
  AppliesToAdd;
end;

procedure TFrmNewPasswordSettings.Finish;
var
  Attributes: TLdapAttributeList;
  i: Integer;
  P: PDocVariantData;
  Attribute: TLdapAttribute;
begin
  Attributes := TLdapAttributeList.Create;
  try
    Attribute := Attributes.Add('objectClass', 'top');
    Attribute.Add('msDS-PasswordSettings');

    Attributes.Add('msDS-PasswordSettingsPrecedence', Edit_Precedence.Text);
    Attributes.Add('msDS-MinimumPasswordLength', Edit_MinPwdLength.Text);
    Attributes.Add('msDS-PasswordHistoryLength', Edit_PwdHistoryLength.Text);
    if CheckBox_PwdComplexity.Checked then
      Attributes.Add('msDS-PasswordComplexityEnabled', 'TRUE')
    else
      Attributes.Add('msDS-PasswordComplexityEnabled', 'FALSE');
    if CheckBox_PwdReversibleEncryption.Checked then
      Attributes.Add('msDS-PasswordReversibleEncryptionEnabled', 'TRUE')
    else
      Attributes.Add('msDS-PasswordReversibleEncryptionEnabled', 'FALSE');
    Attributes.Add('msDS-MinimumPasswordAge', IntToStr(-(Utf8ToInt64(Edit_MinPwdAge.Text) * 24 * 3600 * 10000000)));
    Attributes.Add('msDS-MaximumPasswordAge', IntToStr(-(Utf8ToInt64(Edit_MaxPwdAge.Text) * 24 * 3600 * 10000000)));
    Attributes.Add('msDS-LockoutThreshold', Edit_LockoutThreshold.Text);
    Attributes.Add('msDS-LockoutObservationWindow', IntToStr(-(Utf8ToInt64(Edit_LockoutObservationWindow.Text) * 60 * 10000000)));
    Attributes.Add('msDS-LockoutDuration', IntToStr(-(Utf8ToInt64(Edit_LockoutDuration.Text) * 60 * 10000000)));

    Attribute := Attributes.Add('msDS-PSOAppliesTo');
    for i := 0 to TisGrid_AppliesTo.Data.Count - 1 do
    begin
      P := TisGrid_AppliesTo.Data._[i];
      if not Assigned(P) or not P^.Exists('distinguishedName') then
        Continue;
      Attribute.Add(P^.U['distinguishedName']);
    end;

    if not fLdap.Add(DistinguishedName, Attributes) then
      Exit;
  finally
    FreeAndNil(Attributes);
  end;

  (Owner as TVisNewObject).ModalResult := mrOK;
end;

procedure TFrmNewPasswordSettings.AppliesToAdd;
var
  SelectedObjects: TRawUtf8DynArray;
begin
  if not ShowObjectsSelector(SelectedObjects) then
    Exit;

  AppliesToAdd(SelectedObjects);
end;

procedure TFrmNewPasswordSettings.AppliesToAdd(
  const SelectedObjects: TRawUtf8DynArray);
var
  i: Integer;
  Row: TDocVariantData;
begin
  TisGrid_AppliesTo.BeginUpdate;
  try
    for i := 0 to Length(SelectedObjects) - 1 do
    begin
      Row.Init(JSON_FAST);
      Row.U['distinguishedName'] := SelectedObjects[i];
      TisGrid_AppliesTo.Data.AddItem(Row);
      Row.Clear;
    end;
  finally
    TisGrid_AppliesTo.EndUpdate;
    UpdateGrid;
  end;
end;

procedure TFrmNewPasswordSettings.AppliesToRemove;
var
  Rows: TDocVariantData;
begin
  Rows := TisGrid_AppliesTo.SelectedRows;
  TisGrid_AppliesTo.DeleteRows(@Rows);
  UpdateGrid;
end;

procedure TFrmNewPasswordSettings.UpdateGrid;
var
  P, PSR: PDocVariantData;
  Filter: RawUtf8;
  SearchResultData: TDocVariantData;
  DN: String;
  i: Integer;
begin
  Filter := '';
  for i := 0 to TisGrid_AppliesTo.Data.Count - 1 do
  begin
    P := TisGrid_AppliesTo.Data._[i];
    if not Assigned(P) or not P^.Exists('distinguishedName') then
      Continue;
    Filter := FormatUtf8('%(distinguishedName=%)', [Filter, P^.U['distinguishedName']]);
  end;
  if Filter = '' then
    Exit;
  Filter := FormatUtf8('(|%)', [Filter]);
  fLdap.SearchScope := lssWholeSubtree;

  if not fLdap.SearchAllDocRaw(SearchResultData, fLdap.DefaultDN(), Filter, ['distinguishedName', 'cn', 'mail'], [roAutoRange, roRawValues, roObjectNameAtRoot, roKnownValuesAsArray]) then
    Exit;

  for i := 0 to TisGrid_AppliesTo.Data.Count - 1 do
  begin
    P := TisGrid_AppliesTo.Data._[i];
    if not Assigned(P) or not P^.Exists('distinguishedName') then
      Continue;
    DN := P^.U['distinguishedName'];
    if not SearchResultData.Exists(DN) then
      Continue;
    PSR := SearchResultData.O[DN];
    if not Assigned(PSR) then
      Continue;
    P^.U['name'] := PSR^.U['cn'];
    P^.U['mail'] := PSR^.U['mail'];
  end;
  TisGrid_AppliesTo.LoadData();
end;

function TFrmNewPasswordSettings.ShowObjectsSelector(out
  SelectedObjects: TRawUtf8DynArray): Boolean;
var
  Vis: TVisObjectsSelector;
begin
  result := False;
  Vis := TVisObjectsSelector.Create(Self);
  try
    Vis.AllowMultiSelect := True;
    Vis.AllowedObjectTypes := [otfGroup, otfUser, otfComputer, otfContact];
    Vis.SelectedObjectTypes := [otfGroup, otfUser, otfComputer, otfContact];
    Vis.LdapClient := fLdap;
    if Vis.ShowModal <> mrOK then
      Exit;
    SelectedObjects := Vis.SelectedObjects;
  finally
    FreeAndNil(Vis);
  end;

  result := True;
end;

constructor TFrmNewPasswordSettings.Create(TheOwner: TComponent;
  ALdap: TLdapClient);
var
  OwnerNewObject: TVisNewObject absolute TheOwner;
begin
  inherited Create(TheOwner);

  fLdap := ALdap;

  PageControl1.ActivePageIndex := 0;
  OwnerNewObject.Caption := rsNewObjectPasswordSettings;
  OwnerNewObject.Btn_Next.Action := Action_Next;
  OwnerNewObject.Btn_Next.Caption := rsNewObjectBtnNext;
  OwnerNewObject.Btn_Next.Default := True;
  OwnerNewObject.Btn_Back.Action := Action_Back;
  OwnerNewObject.Btn_Back.Caption := rsNewObjectBtnBack;
  OwnerNewObject.Image_Object.ImageIndex := -1;
  Edit_Name.SetFocus;
end;

end.

