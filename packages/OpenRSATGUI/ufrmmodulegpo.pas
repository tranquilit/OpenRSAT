unit ufrmmodulegpo;

{$mode objfpc}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  ComCtrls,
  ExtCtrls,
  StdCtrls,
  ActnList,
  Dialogs,
  VirtualTrees,
  tis.ui.grid.core,
  mormot.core.base,
  mormot.core.log,
  mormot.core.variants,
  mormot.net.ldap,
  ucoredatamodule,
  ucommon,
  ufrmmodule,
  ufrmoption,
  ugpocore,
  ugpregpol,
  ugptcore,
  umodule,
  umodulegpo,
  uopenrsatuicontextinterface,
  ursatldapclient,
  uoption,
  ursat,
  ulog;

type

  { TFrmModuleGPO }

  TFrmModuleGPO = class(TFrameModule)
    Action_Create: TAction;
    Action_Delete: TAction;
    Action_Duplicate: TAction;
    Action_Refresh: TAction;
    Action_Rename: TAction;
    ActionList1: TActionList;
    Button_CatalogAdd: TButton;
    Button_ConfigurationDiscardAll: TButton;
    Button_ConfigurationSave: TButton;
    Button_GPTFilesRefresh: TButton;
    Button_GPTIniRead: TButton;
    Button_ParameterDiscard: TButton;
    Button_ParameterStage: TButton;
    Button_RegKeyAdd: TButton;
    Button_RegValueApply: TButton;
    Button_RegValueDelete: TButton;
    CheckBox_IncrementMachine: TCheckBox;
    CheckBox_IncrementUser: TCheckBox;
    CheckBox_UpdateGptIni: TCheckBox;
    ComboBox_ConfigurationSource: TComboBox;
    ComboBox_RegValueType: TComboBox;
    Edit_RegValueName: TEdit;
    Label_ConfigurationSource: TLabel;
    Label_FilePath: TLabel;
    Label_GPOList: TLabel;
    Label_GPTFiles: TLabel;
    Label_ParameterDescription: TLabel;
    Label_ParameterName: TLabel;
    Label_RegHint: TLabel;
    Label_RegValueData: TLabel;
    Label_RegValueName: TLabel;
    Label_RegValueType: TLabel;
    Label_StagedCount: TLabel;
    Label_Total: TLabel;
    Memo_GPTIni: TMemo;
    Memo_ParameterValue: TMemo;
    Memo_RegValueData: TMemo;
    PageControl1: TPageControl;
    Panel_CatalogBottom: TPanel;
    Panel_ConfigurationEditor: TPanel;
    Panel_ConfigurationList: TPanel;
    Panel_ConfigurationSource: TPanel;
    Panel_FilePath: TPanel;
    Panel_GPCEditor: TPanel;
    Panel_GPTFiles: TPanel;
    Panel_Left: TPanel;
    Panel_ParameterActions: TPanel;
    Panel_RegistryEditor: TPanel;
    Panel_RegValueButtons: TPanel;
    Panel_RegValueForm: TPanel;
    Panel_Staging: TPanel;
    Panel_Top: TPanel;
    Splitter1: TSplitter;
    Splitter_Configuration: TSplitter;
    TabSheet_Catalog: TTabSheet;
    TabSheet_Configuration: TTabSheet;
    TabSheet_Summary: TTabSheet;
    TabSheet_Technical: TTabSheet;
    TisGrid_Attributes: TTisGrid;
    TisGrid_Catalog: TTisGrid;
    TisGrid_GPOList: TTisGrid;
    TisGrid_GPTFiles: TTisGrid;
    TisGrid_Parameters: TTisGrid;
    TisGrid_RegistryValues: TTisGrid;
    TisGrid_Summary: TTisGrid;
    ToolBar1: TToolBar;
    ToolButton_Create: TToolButton;
    ToolButton_Delete: TToolButton;
    ToolButton_Duplicate: TToolButton;
    ToolButton_Refresh: TToolButton;
    ToolButton_Rename: TToolButton;
    ToolButton_Separator1: TToolButton;
    ToolButton_Separator2: TToolButton;
    procedure Action_CreateExecute(Sender: TObject);
    procedure Action_CreateUpdate(Sender: TObject);
    procedure Action_DeleteExecute(Sender: TObject);
    procedure Action_DeleteUpdate(Sender: TObject);
    procedure Action_DuplicateExecute(Sender: TObject);
    procedure Action_DuplicateUpdate(Sender: TObject);
    procedure Action_RefreshExecute(Sender: TObject);
    procedure Action_RefreshUpdate(Sender: TObject);
    procedure Action_RenameExecute(Sender: TObject);
    procedure Action_RenameUpdate(Sender: TObject);
    procedure Button_CatalogAddClick(Sender: TObject);
    procedure Button_ConfigurationDiscardAllClick(Sender: TObject);
    procedure Button_ConfigurationSaveClick(Sender: TObject);
    procedure Button_GPTFilesRefreshClick(Sender: TObject);
    procedure Button_GPTIniReadClick(Sender: TObject);
    procedure Button_ParameterDiscardClick(Sender: TObject);
    procedure Button_ParameterStageClick(Sender: TObject);
    procedure Button_RegKeyAddClick(Sender: TObject);
    procedure Button_RegValueApplyClick(Sender: TObject);
    procedure Button_RegValueDeleteClick(Sender: TObject);
    procedure ComboBox_ConfigurationSourceChange(Sender: TObject);
    procedure TisGrid_CatalogFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure TisGrid_GPOListFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure TisGrid_GPOListGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure TisGrid_ParametersFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure TisGrid_RegistryValuesFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
  private
    fLog: TSynLogClass;

    fIContext: IOpenRSATUIContext;
    fModule: TModuleGPO;
    fLogic: TGPOLogic;
    fGPT: TGPTCore;
    fGPOs: TGPOList;
    fStagedChanges: TGPOModificationDynArray;
    fCatalog: TGPOCatalogItemDynArray;
    fRegPolUser: TGPRegPol;
    fRegPolMachine: TGPRegPol;
    fRegPolUserDirty: Boolean;
    fRegPolMachineDirty: Boolean;

    function GetLdapClient: TRsatLdapClient;

    function GetFocusedGPO: TGPO;
    function GetRegistrySourceIndex: Integer;
    function GetCurrentRegPol: TGPRegPol;
    function GetCurrentRegPolDirty: Boolean;
    procedure SetCurrentRegPolDirty(AValue: Boolean);
    function GetSelectedRegKey: TGPRegPolKey;
    function GetSelectedRegValue: TGPRegPolValue;

    function GetStagedValue(const AAttributeName: RawUtf8;
      out AValue: RawUtf8): Boolean;
    function GetStagedCount: Integer;

    procedure ClearList;
    procedure ClearStaging;
    procedure UpdateGPOList;
    procedure UpdateSummary(AGPO: TGPO);
    procedure UpdateAttributes(AGPO: TGPO);
    procedure UpdateParameters(AGPO: TGPO);
    procedure UpdateEditor(AGPO: TGPO);
    procedure UpdateCatalog;
    procedure UpdateStagedLabel;
    procedure RefreshGPTFiles;
    procedure ReadGPTIni;
    procedure SwitchConfigurationSource;
    procedure UpdateRegistryKeys;
    procedure UpdateRegistryValues;
    procedure UpdateRegistryEditor;
    procedure SaveChanges;

    procedure LdapConnectEvent(Sender: TObject);
    procedure LdapCloseEvent(Sender: TObject);

    procedure OnGPOOptionsChanged(Option: TOption);
  public
    constructor Create(Context: IOpenRSATUIContext); reintroduce;
    destructor Destroy; override;

    property LdapClient: TRsatLdapClient read GetLdapClient;
  protected
    function GetModule: TModule; override;
    function GetFrmOptionClass: TFrameOptionClass; override;
    function GetOnLdapConnect: TNotifyEvent; override;
    function GetOnLdapClose: TNotifyEvent; override;
  published
    ////////////////
    /// TFrameModule
    procedure Refresh; override;
    procedure Load; override;
    ///
    ////////////////
  end;

implementation
uses
  mormot.core.text;

{$R *.lfm}

{ Registry value helpers }

function RegValueTypeToText(AType: Cardinal): RawUtf8;
begin
  case AType of
    REG_SZ: result := 'REG_SZ';
    REG_EXPAND_SZ: result := 'REG_EXPAND_SZ';
    REG_DWORD: result := 'REG_DWORD';
    REG_QWORD: result := 'REG_QWORD';
    REG_BINARY: result := 'REG_BINARY';
    REG_MULTI_SZ: result := 'REG_MULTI_SZ';
    else
      result := FormatUtf8('0x%', [AType]);
  end;
end;

function RegValueTypeToIndex(AType: Cardinal): Integer;
begin
  case AType of
    REG_SZ: result := 0;
    REG_EXPAND_SZ: result := 1;
    REG_DWORD: result := 2;
    REG_QWORD: result := 3;
    REG_BINARY: result := 4;
    REG_MULTI_SZ: result := 5;
    else
      result := 0;
  end;
end;

function RegValueTypeFromIndex(AIndex: Integer): Cardinal;
begin
  case AIndex of
    0: result := REG_SZ;
    1: result := REG_EXPAND_SZ;
    2: result := REG_DWORD;
    3: result := REG_QWORD;
    4: result := REG_BINARY;
    5: result := REG_MULTI_SZ;
    else
      result := REG_SZ;
  end;
end;

function BinaryToHexText(const AData: RawByteString): RawUtf8;
var
  i: Integer;
begin
  result := '';
  for i := 1 to Length(AData) do
  begin
    if (result <> '') then
      result := result + ' ';
    result := result + IntToHex(Ord(AData[i]), 2);
  end;
end;

function HexTextToBinary(const AHexText: RawUtf8): RawByteString;
var
  i, Nibble, Value: Integer;
  ByteValue: Byte;
begin
  result := '';
  Value := -1;
  for i := 1 to Length(AHexText) do
  begin
    case AHexText[i] of
      '0'..'9': Nibble := Ord(AHexText[i]) - Ord('0');
      'a'..'f': Nibble := Ord(AHexText[i]) - Ord('a') + 10;
      'A'..'F': Nibble := Ord(AHexText[i]) - Ord('A') + 10;
      ' ', #9, ',', '-': Continue;
      else
        Exit;
    end;

    if (Value < 0) then
      Value := Nibble
    else
    begin
      ByteValue := (Value shl 4) or Nibble;
      result := result + AnsiChar(ByteValue);
      Value := -1;
    end;
  end;
end;

function RegValueToText(const AValue: TGPRegPolValue): RawUtf8;
begin
  case AValue.ValueType of
    REG_DWORD: result := FormatUtf8('%', [AValue.AsDWord]);
    REG_QWORD:
      if (Length(AValue.Data) >= 8) then
        result := FormatUtf8('%', [PInt64(@AValue.Data[1])^])
      else
        result := '';
    REG_BINARY: result := BinaryToHexText(AValue.Data);
    else
      result := AValue.AsString;
  end;
end;

function RegValueToEditText(const AValue: TGPRegPolValue): RawUtf8;
begin
  if (AValue.ValueType = REG_BINARY) then
    result := BinaryToHexText(AValue.Data)
  else
    result := RegValueToText(AValue);
end;

{ TFrmModuleGPO }

procedure TFrmModuleGPO.Action_RefreshExecute(Sender: TObject);
begin
  Refresh;
end;

procedure TFrmModuleGPO.Action_RefreshUpdate(Sender: TObject);
begin
  Action_Refresh.Enabled := Assigned(LdapClient) and LdapClient.Connected;
end;

procedure TFrmModuleGPO.Action_CreateExecute(Sender: TObject);
var
  DisplayName: RawUtf8;
  BackupOnError: TNotifyEvent;
begin
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, '% - Execute', [Action_Create.Caption]);

  DisplayName := InputBox(rsGPONewTitle, rsGPONewPrompt, '');
  if (DisplayName = '') then
    Exit;

  BackupOnError := LdapClient.OnError;
  LdapClient.OnError := nil;
  try
    try
      fLogic.Create(LdapClient.DefaultDN, DisplayName);
    except
      on E: EGPOException do
      begin
        MessageDlg(rsGPONewTitle, E.Message, mtError, [mbOK], 0);
        Exit;
      end;
    end;
  finally
    LdapClient.OnError := BackupOnError;
  end;

  Action_RefreshExecute(nil);
end;

procedure TFrmModuleGPO.Action_CreateUpdate(Sender: TObject);
begin
  Action_Create.Enabled := Assigned(LdapClient) and LdapClient.Connected;
end;

procedure TFrmModuleGPO.Action_DeleteExecute(Sender: TObject);
var
  GPO: TGPO;
  BackupOnError: TNotifyEvent;
begin
  GPO := GetFocusedGPO;
  if not Assigned(GPO) then
    Exit;

  if MessageDlg(rsGPODeleteTitle, FormatUtf8(rsGPODeleteConfirmation,
    [GPO.DisplayName]), mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Exit;

  BackupOnError := LdapClient.OnError;
  LdapClient.OnError := nil;
  try
    if not fLogic.Delete(GPO) then
    begin
      MessageDlg(rsGPODeleteTitle, rsLdapDeleteFailed, mtError, [mbOK], 0);
      Exit;
    end;
  finally
    LdapClient.OnError := BackupOnError;
  end;

  Action_RefreshExecute(nil);
end;

procedure TFrmModuleGPO.Action_DeleteUpdate(Sender: TObject);
begin
  Action_Delete.Enabled := Assigned(LdapClient) and LdapClient.Connected and
    Assigned(GetFocusedGPO);
end;

procedure TFrmModuleGPO.Action_DuplicateExecute(Sender: TObject);
var
  GPO: TGPO;
  NewDisplayName, NewDN: RawUtf8;
  BackupOnError: TNotifyEvent;
begin
  GPO := GetFocusedGPO;
  if not Assigned(GPO) then
    Exit;

  NewDisplayName := InputBox(rsGPOActionDuplicate, rsGPODuplicatePrompt,
    FormatUtf8('% - Copy', [GPO.DisplayName]));
  if (NewDisplayName = '') then
    Exit;

  BackupOnError := LdapClient.OnError;
  LdapClient.OnError := nil;
  try
    try
      NewDN := fLogic.Duplicate(GPO, NewDisplayName);
      if (NewDN = '') then
      begin
        MessageDlg(rsGPOActionDuplicate, rsLdapAddFailed, mtError, [mbOK], 0);
        Exit;
      end;
    except
      on E: EGPOException do
      begin
        MessageDlg(rsGPOActionDuplicate, E.Message, mtError, [mbOK], 0);
        Exit;
      end;
    end;
  finally
    LdapClient.OnError := BackupOnError;
  end;

  Action_RefreshExecute(nil);
end;

procedure TFrmModuleGPO.Action_DuplicateUpdate(Sender: TObject);
begin
  Action_Duplicate.Enabled := Assigned(LdapClient) and LdapClient.Connected and
    Assigned(GetFocusedGPO);
end;

procedure TFrmModuleGPO.Action_RenameExecute(Sender: TObject);
var
  GPO: TGPO;
  NewDisplayName: RawUtf8;
  BackupOnError: TNotifyEvent;
begin
  GPO := GetFocusedGPO;
  if not Assigned(GPO) then
    Exit;

  NewDisplayName := InputBox(rsGPORenameTitle, rsGPORenamePrompt, GPO.DisplayName);
  if (NewDisplayName = '') or SameText(NewDisplayName, GPO.DisplayName) then
    Exit;

  BackupOnError := LdapClient.OnError;
  LdapClient.OnError := nil;
  try
    try
      if not fLogic.Rename(GPO, NewDisplayName) then
      begin
        MessageDlg(rsGPORenameTitle, rsLdapModifyFailed, mtError, [mbOK], 0);
        Exit;
      end;
    except
      on E: EGPOException do
      begin
        MessageDlg(rsGPORenameTitle, E.Message, mtError, [mbOK], 0);
        Exit;
      end;
    end;
  finally
    LdapClient.OnError := BackupOnError;
  end;

  Action_RefreshExecute(nil);
end;

procedure TFrmModuleGPO.Action_RenameUpdate(Sender: TObject);
begin
  Action_Rename.Enabled := Assigned(LdapClient) and LdapClient.Connected and
    Assigned(GetFocusedGPO);
end;

procedure TFrmModuleGPO.Button_ParameterStageClick(Sender: TObject);
var
  AttributeName: RawUtf8;
  Row: PDocVariantData;
  i: Integer;
begin
  if (GetRegistrySourceIndex <> 0) then
    Exit;

  Row := TisGrid_Parameters.FocusedRow;
  if not Assigned(Row) or not Row^.Exists('name') then
    Exit;
  AttributeName := Row^.U['name'];

  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, 'Stage parameter "%"', [AttributeName], Self);

  // Update the staged value (add or replace the entry).
  for i := 0 to High(fStagedChanges) do
    if SameText(fStagedChanges[i].AttributeName, AttributeName) then
    begin
      fStagedChanges[i].Value := Memo_ParameterValue.Text;
      UpdateParameters(GetFocusedGPO);
      UpdateEditor(GetFocusedGPO);
      UpdateStagedLabel;
      Exit;
    end;

  SetLength(fStagedChanges, Length(fStagedChanges) + 1);
  fStagedChanges[High(fStagedChanges)].AttributeName := AttributeName;
  fStagedChanges[High(fStagedChanges)].Value := Memo_ParameterValue.Text;

  UpdateParameters(GetFocusedGPO);
  UpdateEditor(GetFocusedGPO);
  UpdateStagedLabel;
end;

procedure TFrmModuleGPO.Button_ParameterDiscardClick(Sender: TObject);
var
  AttributeName: RawUtf8;
  Row: PDocVariantData;
  i: Integer;
begin
  if (GetRegistrySourceIndex <> 0) then
    Exit;

  Row := TisGrid_Parameters.FocusedRow;
  if not Assigned(Row) or not Row^.Exists('name') then
    Exit;
  AttributeName := Row^.U['name'];

  for i := High(fStagedChanges) downto 0 do
    if SameText(fStagedChanges[i].AttributeName, AttributeName) then
    begin
      fStagedChanges[i] := fStagedChanges[High(fStagedChanges)];
      SetLength(fStagedChanges, Length(fStagedChanges) - 1);
    end;

  UpdateParameters(GetFocusedGPO);
  UpdateEditor(GetFocusedGPO);
  UpdateStagedLabel;
end;

procedure TFrmModuleGPO.Button_ConfigurationSaveClick(Sender: TObject);
begin
  SaveChanges;
end;

procedure TFrmModuleGPO.Button_ConfigurationDiscardAllClick(Sender: TObject);
begin
  if (GetStagedCount = 0) then
    Exit;

  if MessageDlg(rsGPOTabConfiguration, rsGPOConfigurationDiscardConfirmation,
    mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Exit;

  ClearStaging;
  UpdateParameters(GetFocusedGPO);
  UpdateEditor(GetFocusedGPO);
  UpdateStagedLabel;
end;

procedure TFrmModuleGPO.Button_CatalogAddClick(Sender: TObject);
var
  CatalogItem: TGPOCatalogItem;
  AttributeName, Value: RawUtf8;
  GPO: TGPO;
  Row: PDocVariantData;
  i: Integer;
begin
  GPO := GetFocusedGPO;
  if not Assigned(GPO) then
    Exit;

  Row := TisGrid_Catalog.FocusedRow;
  if not Assigned(Row) or not Row^.Exists('name') then
    Exit;

  CatalogItem.AttributeName := Row^.U['name'];
  CatalogItem.Description := Row^.U['description'];
  AttributeName := CatalogItem.AttributeName;
  if Assigned(GPO.Attributes.Find(AttributeName)) then
  begin
    MessageDlg(rsGPOTabCatalog, FormatUtf8(rsGPOCatalogAlreadyExists,
      [AttributeName]), mtInformation, [mbOK], 0);
    Exit;
  end;

  Value := InputBox(rsGPOTabCatalog, FormatUtf8(rsGPOCatalogAddPrompt,
    [AttributeName]), '');
  if (Value = '') then
    Exit;

  // Stage the new attribute.
  for i := 0 to High(fStagedChanges) do
    if SameText(fStagedChanges[i].AttributeName, AttributeName) then
      fStagedChanges[i].Value := Value;

  SetLength(fStagedChanges, Length(fStagedChanges) + 1);
  fStagedChanges[High(fStagedChanges)].AttributeName := AttributeName;
  fStagedChanges[High(fStagedChanges)].Value := Value;

  UpdateParameters(GPO);
  UpdateEditor(GPO);
  UpdateStagedLabel;
end;

procedure TFrmModuleGPO.Button_GPTFilesRefreshClick(Sender: TObject);
begin
  RefreshGPTFiles;
end;

procedure TFrmModuleGPO.Button_GPTIniReadClick(Sender: TObject);
begin
  ReadGPTIni;
end;

procedure TFrmModuleGPO.TisGrid_ParametersFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  if (GetRegistrySourceIndex = 0) then
    UpdateEditor(GetFocusedGPO)
  else
  begin
    UpdateRegistryValues;
    UpdateRegistryEditor;
  end;
end;

procedure TFrmModuleGPO.TisGrid_CatalogFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  // The description is shown in the grid itself.
end;

procedure TFrmModuleGPO.TisGrid_GPOListFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  GPO: TGPO;
begin
  GPO := GetFocusedGPO;
  UpdateSummary(GPO);
  UpdateAttributes(GPO);
  UpdateParameters(GPO);
  UpdateEditor(GPO);
end;

procedure TFrmModuleGPO.TisGrid_GPOListGetImageIndex(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
begin
  case TisGrid_GPOList.FindColumnByIndex(Column).PropertyName of
    'displayName':
    begin
      if (ImageIndex < 0) then
        ImageIndex := Ord(ileADGroupPolicyContainer);
    end;
  end;
end;

function TFrmModuleGPO.GetLdapClient: TRsatLdapClient;
begin
  result := fModule.RSAT.LdapClient;
end;

function TFrmModuleGPO.GetFocusedGPO: TGPO;
var
  Row: PDocVariantData;
  GPO: TGPO;
begin
  result := nil;

  Row := TisGrid_GPOList.FocusedRow;
  if not Assigned(Row) or not Row^.Exists('distinguishedName') then
    Exit;

  for GPO in fGPOs do
  begin
    if (GPO.DistinguishedName = Row^.U['distinguishedName']) then
      Exit(GPO);
  end;
end;

function TFrmModuleGPO.GetRegistrySourceIndex: Integer;
begin
  // 0 = GPC parameters, 1 = User Registry.pol, 2 = Machine Registry.pol
  result := ComboBox_ConfigurationSource.ItemIndex;
  if (result < 0) then
    result := 0;
end;

function TFrmModuleGPO.GetCurrentRegPol: TGPRegPol;
begin
  if (GetRegistrySourceIndex = 1) then
    result := fRegPolUser
  else
    result := fRegPolMachine;
end;

function TFrmModuleGPO.GetCurrentRegPolDirty: Boolean;
begin
  if (GetRegistrySourceIndex = 1) then
    result := fRegPolUserDirty
  else
    result := fRegPolMachineDirty;
end;

procedure TFrmModuleGPO.SetCurrentRegPolDirty(AValue: Boolean);
begin
  if (GetRegistrySourceIndex = 1) then
    fRegPolUserDirty := AValue
  else
    fRegPolMachineDirty := AValue;
  UpdateStagedLabel;
end;

function TFrmModuleGPO.GetSelectedRegKey: TGPRegPolKey;
var
  Row: PDocVariantData;
begin
  result := nil;

  Row := TisGrid_Parameters.FocusedRow;
  if not Assigned(Row) or not Row^.Exists('key') then
    Exit;

  result := GetCurrentRegPol.FindKey(Row^.U['key']);
end;

function TFrmModuleGPO.GetSelectedRegValue: TGPRegPolValue;
var
  Row: PDocVariantData;
  Key: TGPRegPolKey;
begin
  result := nil;

  Row := TisGrid_RegistryValues.FocusedRow;
  if not Assigned(Row) or not Row^.Exists('name') then
    Exit;

  Key := GetSelectedRegKey;
  if Assigned(Key) then
    result := Key.GetValue(Row^.U['name']);
end;

function TFrmModuleGPO.GetStagedValue(const AAttributeName: RawUtf8;
  out AValue: RawUtf8): Boolean;
var
  i: Integer;
begin
  result := False;
  for i := 0 to High(fStagedChanges) do
    if SameText(fStagedChanges[i].AttributeName, AAttributeName) then
    begin
      AValue := fStagedChanges[i].Value;
      Exit(True);
    end;
end;

function TFrmModuleGPO.GetStagedCount: Integer;
begin
  result := Length(fStagedChanges);
end;

procedure TFrmModuleGPO.ClearList;
var
  i: Integer;
begin
  for i := 0 to High(fGPOs) do
    fGPOs[i].Free;
  fGPOs := nil;
end;

procedure TFrmModuleGPO.ClearStaging;
begin
  fStagedChanges := nil;
  fRegPolUserDirty := False;
  fRegPolMachineDirty := False;
  CheckBox_IncrementUser.Checked := False;
  CheckBox_IncrementMachine.Checked := False;
  CheckBox_UpdateGptIni.Checked := False;
  UpdateStagedLabel;
end;

procedure TFrmModuleGPO.UpdateGPOList;
var
  GPO: TGPO;
  RowData: TDocVariantData;
begin
  TisGrid_GPOList.Clear;
  RowData.Init;

  TisGrid_GPOList.BeginUpdate;
  try
    for GPO in fGPOs do
    begin
      RowData.AddOrUpdateValue('displayName', GPO.DisplayName);
      RowData.AddOrUpdateValue('status', GPOFlagsToText(GPO.Flags));
      RowData.AddOrUpdateValue('whenChanged', GPO.WhenChanged);
      RowData.AddOrUpdateValue('distinguishedName', GPO.DistinguishedName);
      TisGrid_GPOList.Data.AddItem(RowData);
      RowData.Clear;
    end;
  finally
    TisGrid_GPOList.EndUpdate;
    TisGrid_GPOList.LoadData;
    TisGrid_GPOList.ClearSelection;
    TisGrid_GPOList.FocusedNode := nil;
  end;

  Label_Total.Caption := FormatUtf8(rsGPOElementsCount, [Length(fGPOs)]);
end;

procedure TFrmModuleGPO.UpdateSummary(AGPO: TGPO);
var
  RowData: TDocVariantData;
  TotalAttributes, KnownAttributes, UnknownAttributes: Integer;
  Attribute: TLdapAttribute;
begin
  TisGrid_Summary.Clear;
  RowData.Init;

  if not Assigned(AGPO) then
  begin
    TisGrid_Summary.LoadData;
    Exit;
  end;

  TotalAttributes := 0;
  KnownAttributes := 0;
  for Attribute in AGPO.Attributes.Items do
  begin
    Inc(TotalAttributes);
    if IsKnownGPOAttribute(Attribute.AttributeName) then
      Inc(KnownAttributes);
  end;
  UnknownAttributes := TotalAttributes - KnownAttributes;

  TisGrid_Summary.BeginUpdate;
  try
    RowData.AddOrUpdateValue('name', GPO_ATTR_DISPLAYNAME);
    RowData.AddOrUpdateValue('value', AGPO.DisplayName);
    TisGrid_Summary.Data.AddItem(RowData);
    RowData.Clear;

    RowData.AddOrUpdateValue('name', rsGPOStatus);
    RowData.AddOrUpdateValue('value', GPOFlagsToText(AGPO.Flags));
    TisGrid_Summary.Data.AddItem(RowData);
    RowData.Clear;

    RowData.AddOrUpdateValue('name', rsGPOUserVersion);
    RowData.AddOrUpdateValue('value', FormatUtf8('%', [AGPO.UserVersion]));
    TisGrid_Summary.Data.AddItem(RowData);
    RowData.Clear;

    RowData.AddOrUpdateValue('name', rsGPOMachineVersion);
    RowData.AddOrUpdateValue('value', FormatUtf8('%', [AGPO.MachineVersion]));
    TisGrid_Summary.Data.AddItem(RowData);
    RowData.Clear;

    RowData.AddOrUpdateValue('name', rsGPOFunctionalityVersion);
    RowData.AddOrUpdateValue('value',
      FormatUtf8('%', [AGPO.FunctionalityVersion]));
    TisGrid_Summary.Data.AddItem(RowData);
    RowData.Clear;

    RowData.AddOrUpdateValue('name', rsGPOSummaryTotalAttributes);
    RowData.AddOrUpdateValue('value', FormatUtf8('%', [TotalAttributes]));
    TisGrid_Summary.Data.AddItem(RowData);
    RowData.Clear;

    RowData.AddOrUpdateValue('name', rsGPOSummaryKnownAttributes);
    RowData.AddOrUpdateValue('value', FormatUtf8('%', [KnownAttributes]));
    TisGrid_Summary.Data.AddItem(RowData);
    RowData.Clear;

    RowData.AddOrUpdateValue('name', rsGPOSummaryUnknownAttributes);
    RowData.AddOrUpdateValue('value', FormatUtf8('%', [UnknownAttributes]));
    TisGrid_Summary.Data.AddItem(RowData);
    RowData.Clear;

    RowData.AddOrUpdateValue('name', rsGPOSummaryUserTechnologies);
    RowData.AddOrUpdateValue('value', FormatUtf8('%',
      [GPOExtensionsCount(AGPO.UserExtensionNames)]));
    TisGrid_Summary.Data.AddItem(RowData);
    RowData.Clear;

    RowData.AddOrUpdateValue('name', rsGPOSummaryMachineTechnologies);
    RowData.AddOrUpdateValue('value', FormatUtf8('%',
      [GPOExtensionsCount(AGPO.MachineExtensionNames)]));
    TisGrid_Summary.Data.AddItem(RowData);
    RowData.Clear;

    RowData.AddOrUpdateValue('name', GPO_ATTR_WHENCREATED);
    RowData.AddOrUpdateValue('value', AGPO.WhenCreated);
    TisGrid_Summary.Data.AddItem(RowData);
    RowData.Clear;

    RowData.AddOrUpdateValue('name', GPO_ATTR_WHENCHANGED);
    RowData.AddOrUpdateValue('value', AGPO.WhenChanged);
    TisGrid_Summary.Data.AddItem(RowData);
    RowData.Clear;
  finally
    TisGrid_Summary.EndUpdate;
    TisGrid_Summary.LoadData;
  end;
end;

procedure TFrmModuleGPO.UpdateAttributes(AGPO: TGPO);
var
  RowData: TDocVariantData;
  Attribute: TLdapAttribute;
begin
  TisGrid_Attributes.Clear;
  RowData.Init;

  Label_FilePath.Caption := '';
  if not Assigned(AGPO) then
  begin
    TisGrid_Attributes.LoadData;
    Exit;
  end;

  Label_FilePath.Caption := FormatUtf8('%: %', [rsGPOFilePath, AGPO.FileSysPath]);

  TisGrid_Attributes.BeginUpdate;
  try
    for Attribute in AGPO.Attributes.Items do
    begin
      RowData.AddOrUpdateValue('name', Attribute.AttributeName);
      RowData.AddOrUpdateValue('value', Attribute.GetReadable());
      TisGrid_Attributes.Data.AddItem(RowData);
      RowData.Clear;
    end;
  finally
    TisGrid_Attributes.EndUpdate;
    TisGrid_Attributes.LoadData;
  end;
end;

procedure TFrmModuleGPO.UpdateParameters(AGPO: TGPO);
var
  RowData: TDocVariantData;
  Attribute: TLdapAttribute;
  StagedValue: RawUtf8;
  HasStagedValue: Boolean;
  i: Integer;
begin
  (TisGrid_Parameters.Header.Columns[0] as TTisGridColumn).Text :=
    rsGPOConfigurationParameterName;
  (TisGrid_Parameters.Header.Columns[1] as TTisGridColumn).Text :=
    rsGPOConfigurationParameterValue;
  (TisGrid_Parameters.Header.Columns[2] as TTisGridColumn).Text :=
    rsGPOConfigurationStaged;

  TisGrid_Parameters.Clear;
  RowData.Init;

  if not Assigned(AGPO) then
  begin
    TisGrid_Parameters.LoadData;
    Exit;
  end;

  TisGrid_Parameters.BeginUpdate;
  try
    // Existing attributes of the GPO.
    for Attribute in AGPO.Attributes.Items do
    begin
      RowData.AddOrUpdateValue('name', Attribute.AttributeName);
      HasStagedValue := GetStagedValue(Attribute.AttributeName, StagedValue);
      if HasStagedValue then
        RowData.AddOrUpdateValue('value', StagedValue)
      else
        RowData.AddOrUpdateValue('value', Attribute.GetReadable());
      RowData.AddOrUpdateValue('staged', BoolToStr(HasStagedValue, True));
      TisGrid_Parameters.Data.AddItem(RowData);
      RowData.Clear;
    end;

    // Staged attributes that do not exist yet on the GPO (from the catalog).
    for i := 0 to High(fStagedChanges) do
    begin
      if Assigned(AGPO.Attributes.Find(fStagedChanges[i].AttributeName)) then
        Continue;
      RowData.AddOrUpdateValue('name', fStagedChanges[i].AttributeName);
      RowData.AddOrUpdateValue('value', fStagedChanges[i].Value);
      RowData.AddOrUpdateValue('staged', 'True');
      TisGrid_Parameters.Data.AddItem(RowData);
      RowData.Clear;
    end;
  finally
    TisGrid_Parameters.EndUpdate;
    TisGrid_Parameters.LoadData;
  end;

  UpdateStagedLabel;
end;

procedure TFrmModuleGPO.UpdateEditor(AGPO: TGPO);
var
  Row: PDocVariantData;
  AttributeName, StagedValue, CurrentValue: RawUtf8;
  i: Integer;
begin
  Label_ParameterName.Caption := '';
  Label_ParameterDescription.Caption := '';
  Memo_ParameterValue.Text := '';

  if not Assigned(AGPO) then
    Exit;

  Row := TisGrid_Parameters.FocusedRow;
  if not Assigned(Row) or not Row^.Exists('name') then
    Exit;

  AttributeName := Row^.U['name'];
  Label_ParameterName.Caption := AttributeName;

  // Description from the catalog.
  for i := 0 to High(fCatalog) do
    if SameText(fCatalog[i].AttributeName, AttributeName) then
    begin
      Label_ParameterDescription.Caption := fCatalog[i].Description;
      Break;
    end;

  // Value: staged one first, then the current one.
  if GetStagedValue(AttributeName, StagedValue) then
    Memo_ParameterValue.Text := StagedValue
  else
  begin
    CurrentValue := '';
    if Assigned(AGPO.Attributes.Find(AttributeName)) then
      CurrentValue := AGPO.Attributes.Find(AttributeName).GetReadable();
    Memo_ParameterValue.Text := CurrentValue;
  end;
end;

procedure TFrmModuleGPO.SwitchConfigurationSource;
var
  GPO: TGPO;
  Pol: TGPRegPol;
  UserSide: Boolean;
begin
  // 0 = GPC parameters, 1 = User Registry.pol, 2 = Machine Registry.pol
  Panel_GPCEditor.Visible := (GetRegistrySourceIndex = 0);
  Panel_RegistryEditor.Visible := (GetRegistrySourceIndex <> 0);
  TisGrid_Parameters.Visible := True;

  if (GetRegistrySourceIndex = 0) then
  begin
    UpdateParameters(GetFocusedGPO);
    UpdateEditor(GetFocusedGPO);
    Exit;
  end;

  // Registry mode: load the Registry.pol of the selected side when needed.
  UserSide := (GetRegistrySourceIndex = 1);
  Pol := GetCurrentRegPol;
  if not Assigned(Pol) then
  begin
    GPO := GetFocusedGPO;
    if Assigned(GPO) and Assigned(LdapClient) and LdapClient.Connected then
    begin
      Pol := nil;
      if fGPT.ReadRegistryPol(GPO, UserSide, Pol) then
      begin
        if UserSide then
          fRegPolUser := Pol
        else
          fRegPolMachine := Pol;
      end;
    end;

    // A GPO may have no Registry.pol yet: start with an empty one.
    if (GetRegistrySourceIndex = 1) and not Assigned(fRegPolUser) then
      fRegPolUser := TGPRegPol.Create;
    if (GetRegistrySourceIndex = 2) and not Assigned(fRegPolMachine) then
      fRegPolMachine := TGPRegPol.Create;
  end;

  UpdateRegistryKeys;
  UpdateRegistryValues;
  UpdateRegistryEditor;
end;

procedure TFrmModuleGPO.UpdateRegistryKeys;
var
  RowData: TDocVariantData;
  Pol: TGPRegPol;
  i, j: Integer;

  procedure AddKey(const AKey: TGPRegPolKey);
  var
    k: Integer;
  begin
    RowData.AddOrUpdateValue('key', AKey.Path);
    RowData.AddOrUpdateValue('name', AKey.Path);
    RowData.AddOrUpdateValue('value', '');
    RowData.AddOrUpdateValue('staged', '');
    TisGrid_Parameters.Data.AddItem(RowData);
    RowData.Clear;

    for k := 0 to High(AKey.SubKeys) do
      AddKey(AKey.SubKeys[k]);
  end;
begin
  (TisGrid_Parameters.Header.Columns[0] as TTisGridColumn).Text := rsGPOConfigurationRegKey;
  (TisGrid_Parameters.Header.Columns[1] as TTisGridColumn).Text := '';
  (TisGrid_Parameters.Header.Columns[2] as TTisGridColumn).Text := '';

  TisGrid_Parameters.Clear;
  RowData.Init;

  Pol := GetCurrentRegPol;
  if not Assigned(Pol) then
  begin
    TisGrid_Parameters.LoadData;
    Exit;
  end;

  TisGrid_Parameters.BeginUpdate;
  try
    for i := 0 to High(Pol.RootKeys) do
      AddKey(Pol.RootKeys[i]);
  finally
    TisGrid_Parameters.EndUpdate;
    TisGrid_Parameters.LoadData;
  end;
end;

procedure TFrmModuleGPO.UpdateRegistryValues;
var
  RowData: TDocVariantData;
  Key: TGPRegPolKey;
  Value: TGPRegPolValue;
  i: Integer;
begin
  TisGrid_RegistryValues.Clear;
  RowData.Init;

  Key := GetSelectedRegKey;
  if not Assigned(Key) then
  begin
    TisGrid_RegistryValues.LoadData;
    Label_RegHint.Caption := '';
    Exit;
  end;

  TisGrid_RegistryValues.BeginUpdate;
  try
    for i := 0 to High(Key.Values) do
    begin
      Value := Key.Values[i];
      RowData.AddOrUpdateValue('name', Value.Name);
      RowData.AddOrUpdateValue('vtype', RegValueTypeToText(Value.ValueType));
      RowData.AddOrUpdateValue('value', RegValueToText(Value));
      TisGrid_RegistryValues.Data.AddItem(RowData);
      RowData.Clear;
    end;
  finally
    TisGrid_RegistryValues.EndUpdate;
    TisGrid_RegistryValues.LoadData;
  end;

  Label_RegHint.Caption := rsGPOConfigurationRegHint;
end;

procedure TFrmModuleGPO.UpdateRegistryEditor;
var
  Value: TGPRegPolValue;
  Row: PDocVariantData;
begin
  Edit_RegValueName.Text := '';
  ComboBox_RegValueType.ItemIndex := 0;
  Memo_RegValueData.Text := '';
  Label_RegHint.Caption := rsGPOConfigurationRegHint;

  Row := TisGrid_RegistryValues.FocusedRow;
  if Assigned(Row) and Row^.Exists('name') then
  begin
    Value := GetSelectedRegValue;
    if Assigned(Value) then
    begin
      Edit_RegValueName.Text := Value.Name;
      ComboBox_RegValueType.ItemIndex := RegValueTypeToIndex(Value.ValueType);
      Memo_RegValueData.Text := RegValueToEditText(Value);
    end;
  end;
end;

procedure TFrmModuleGPO.UpdateCatalog;
var
  RowData: TDocVariantData;
  i: Integer;
begin
  (TisGrid_Catalog.Header.Columns[0] as TTisGridColumn).Text := rsGPOCatalogColumnName;
  (TisGrid_Catalog.Header.Columns[1] as TTisGridColumn).Text := rsGPOCatalogColumnDescription;

  fCatalog := GPOAttributeCatalog;

  TisGrid_Catalog.Clear;
  RowData.Init;

  TisGrid_Catalog.BeginUpdate;
  try
    for i := 0 to High(fCatalog) do
    begin
      RowData.AddOrUpdateValue('name', fCatalog[i].AttributeName);
      RowData.AddOrUpdateValue('description', fCatalog[i].Description);
      TisGrid_Catalog.Data.AddItem(RowData);
      RowData.Clear;
    end;
  finally
    TisGrid_Catalog.EndUpdate;
    TisGrid_Catalog.LoadData;
  end;
end;

procedure TFrmModuleGPO.ComboBox_ConfigurationSourceChange(Sender: TObject);
begin
  SwitchConfigurationSource;
end;

procedure TFrmModuleGPO.TisGrid_RegistryValuesFocusChanged(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
begin
  UpdateRegistryEditor;
end;

procedure TFrmModuleGPO.Button_RegValueApplyClick(Sender: TObject);
var
  Key: TGPRegPolKey;
  ValueName, DataText: RawUtf8;
  Data: RawByteString;
  TypeIndex: Integer;
  DWordValue: Cardinal;
  QWordValue: Int64;
begin
  Key := GetSelectedRegKey;
  if not Assigned(Key) then
    Exit;

  ValueName := Edit_RegValueName.Text;
  TypeIndex := ComboBox_RegValueType.ItemIndex;
  if (TypeIndex < 0) then
    TypeIndex := 0;
  DataText := Memo_RegValueData.Text;

  // Build the value data according to the selected type.
  Data := '';
  case TypeIndex of
    0: Data := DataText;                                  // REG_SZ
    1: Data := DataText;                                  // REG_EXPAND_SZ
    2: begin                                              // REG_DWORD
      if not TryStrToDWord(Trim(DataText), DWordValue) then
      begin
        MessageDlg(rsGPOTabConfiguration, rsGPOConfigurationRegInvalidDWord,
          mtError, [mbOK], 0);
        Exit;
      end;
      SetLength(Data, SizeOf(Cardinal));
      PCardinal(@Data[1])^ := DWordValue;
    end;
    3: begin                                              // REG_QWORD
      if not TryStrToInt64(Trim(DataText), QWordValue) then
      begin
        MessageDlg(rsGPOTabConfiguration, rsGPOConfigurationRegInvalidQWord,
          mtError, [mbOK], 0);
        Exit;
      end;
      SetLength(Data, SizeOf(Int64));
      PInt64(@Data[1])^ := QWordValue;
    end;
    4: Data := HexTextToBinary(DataText);                 // REG_BINARY
    5: Data := DataText;                                  // REG_MULTI_SZ
  end;

  // Update an existing value, or create it.
  Key.SetValueData(ValueName, RegValueTypeFromIndex(TypeIndex), Data);

  SetCurrentRegPolDirty(True);
  UpdateRegistryValues;
  UpdateRegistryEditor;
end;

procedure TFrmModuleGPO.Button_RegValueDeleteClick(Sender: TObject);
var
  Key: TGPRegPolKey;
  Value: TGPRegPolValue;
begin
  Key := GetSelectedRegKey;
  Value := GetSelectedRegValue;
  if not Assigned(Key) or not Assigned(Value) then
    Exit;

  Key.RemoveValue(Value.Name);
  SetCurrentRegPolDirty(True);
  UpdateRegistryValues;
  UpdateRegistryEditor;
end;

procedure TFrmModuleGPO.Button_RegKeyAddClick(Sender: TObject);
var
  Pol: TGPRegPol;
  KeyPath: RawUtf8;
begin
  Pol := GetCurrentRegPol;
  if not Assigned(Pol) then
    Exit;

  KeyPath := InputBox(rsGPOTabConfiguration, rsGPOConfigurationRegAddKeyPrompt, '');
  if (KeyPath = '') then
    Exit;

  Pol.AddKey(KeyPath);
  SetCurrentRegPolDirty(True);
  UpdateRegistryKeys;
end;

procedure TFrmModuleGPO.UpdateStagedLabel;
var
  Count: Integer;
begin
  Count := GetStagedCount;
  if fRegPolUserDirty then
    Inc(Count);
  if fRegPolMachineDirty then
    Inc(Count);

  if (Count = 0) then
    Label_StagedCount.Caption := rsGPOConfigurationStagedEmpty
  else
    Label_StagedCount.Caption := FormatUtf8(rsGPOConfigurationStagedChanges, [Count]);
end;

procedure TFrmModuleGPO.RefreshGPTFiles;
var
  GPO: TGPO;
  Files: TGPTFileInfoDynArray;
  RowData: TDocVariantData;
  i: Integer;
begin
  (TisGrid_GPTFiles.Header.Columns[0] as TTisGridColumn).Text :=
    rsGPOTechnicalGPTFileColumn;
  (TisGrid_GPTFiles.Header.Columns[1] as TTisGridColumn).Text :=
    rsGPOTechnicalGPTSizeColumn;

  GPO := GetFocusedGPO;
  TisGrid_GPTFiles.Clear;
  RowData.Init;

  if not Assigned(GPO) or not Assigned(LdapClient) or not LdapClient.Connected then
  begin
    TisGrid_GPTFiles.LoadData;
    Exit;
  end;

  if not fGPT.ListFiles(GPO, Files) then
  begin
    if (fGPT.LastError <> '') then
      Label_GPTFiles.Caption := FormatUtf8('%: %',
        [rsGPOTechnicalGPTUnavailable, fGPT.LastError])
    else
      Label_GPTFiles.Caption := rsGPOTechnicalGPTUnavailable;
    TisGrid_GPTFiles.LoadData;
    Exit;
  end;

  Label_GPTFiles.Caption := rsGPOTechnicalGPTFiles;

  TisGrid_GPTFiles.BeginUpdate;
  try
    for i := 0 to High(Files) do
    begin
      RowData.AddOrUpdateValue('path', Files[i].Path);
      RowData.AddOrUpdateValue('size', FormatUtf8('%', [Files[i].Size]));
      RowData.AddOrUpdateValue('modified', Files[i].Modified);
      TisGrid_GPTFiles.Data.AddItem(RowData);
      RowData.Clear;
    end;
  finally
    TisGrid_GPTFiles.EndUpdate;
    TisGrid_GPTFiles.LoadData;
  end;
end;

procedure TFrmModuleGPO.ReadGPTIni;
var
  GPO: TGPO;
  GptIni: TGptIni;
begin
  GPO := GetFocusedGPO;
  Memo_GPTIni.Lines.Clear;

  if not Assigned(GPO) or not Assigned(LdapClient) or not LdapClient.Connected then
    Exit;

  if not fGPT.ReadGptIni(GPO, GptIni) then
  begin
    if (fGPT.LastError <> '') then
      Memo_GPTIni.Lines.Text := FormatUtf8('%: %',
        [rsGPOTechnicalGPTIniUnavailable, fGPT.LastError])
    else
      Memo_GPTIni.Lines.Text := rsGPOTechnicalGPTIniUnavailable;
    Exit;
  end;

  if GptIni.HasVersion then
    Memo_GPTIni.Lines.Add(FormatUtf8('%: %',
      [rsGPOTechnicalGPTIniVersion, GptIni.Version]));
  if GptIni.HasDisplayName then
    Memo_GPTIni.Lines.Add(FormatUtf8('%: %',
      [rsGPOTechnicalGPTIniDisplayName, GptIni.DisplayName]));
  if GptIni.HasOptions then
    Memo_GPTIni.Lines.Add(FormatUtf8('%: %',
      [rsGPOTechnicalGPTIniOptions, GptIni.Options]));

  if (Memo_GPTIni.Lines.Count = 0) then
    Memo_GPTIni.Lines.Text := rsGPOTechnicalGPTIniEmpty;
end;

procedure TFrmModuleGPO.SaveChanges;
var
  GPO: TGPO;
  BackupOnError: TNotifyEvent;
  Modifications: TGPOModificationDynArray;
  i, Count: Integer;
  NewUserVersion, NewMachineVersion: Word;
  NewVersionNumber: Cardinal;
  GptIni: TGptIni;
  StagedFlags: RawUtf8;
begin
  GPO := GetFocusedGPO;
  if not Assigned(GPO) then
    Exit;

  // Nothing staged: nothing to save.
  if (GetStagedCount = 0) and (not fRegPolUserDirty) and (not fRegPolMachineDirty) and
    (not CheckBox_IncrementUser.Checked) and (not CheckBox_IncrementMachine.Checked) and
    (not CheckBox_UpdateGptIni.Checked) then
    Exit;

  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, 'Save changes on GPO "%"', [GPO.DisplayName], Self);

  Count := GetStagedCount;

  // Calculate the new version of User / Machine: a dirty Registry.pol side
  // bumps its version automatically, the checkboxes add manual increments.
  NewUserVersion := GPO.UserVersion;
  NewMachineVersion := GPO.MachineVersion;
  if fRegPolUserDirty or CheckBox_IncrementUser.Checked then
    Inc(NewUserVersion);
  if fRegPolMachineDirty or CheckBox_IncrementMachine.Checked then
    Inc(NewMachineVersion);

  NewVersionNumber := GPOPartsToVersionNumber(NewUserVersion, NewMachineVersion);
  if (NewVersionNumber <> GPO.VersionNumber) then
  begin
    Inc(Count);
    SetLength(Modifications, Count);
    Modifications[Count - 1].AttributeName := GPO_ATTR_VERSIONNUMBER;
    Modifications[Count - 1].Value := FormatUtf8('%', [NewVersionNumber]);
  end;

  // Copy the staged changes into the modification list.
  for i := 0 to High(fStagedChanges) do
  begin
    if (fStagedChanges[i].AttributeName = '') then
      Continue;
    SetLength(Modifications, Length(Modifications) + 1);
    Modifications[High(Modifications)].AttributeName :=
      fStagedChanges[i].AttributeName;
    Modifications[High(Modifications)].Value := fStagedChanges[i].Value;
  end;

  // Step 2 of the GPO modification flow: update the Registry.pol files of the
  // dirty sides on the SYSVOL (registry provider).
  if fRegPolUserDirty and Assigned(fRegPolUser) then
  begin
    if not fGPT.WriteRegistryPol(GPO, True, fRegPolUser) then
    begin
      if (fGPT.LastError <> '') then
        MessageDlg(rsGPOTabConfiguration,
          FormatUtf8('%: %', [rsGPOConfigurationRegWriteFailed, fGPT.LastError]),
          mtError, [mbOK], 0)
      else
        MessageDlg(rsGPOTabConfiguration, rsGPOConfigurationRegWriteFailed,
          mtError, [mbOK], 0);
      Exit;
    end;
  end;

  if fRegPolMachineDirty and Assigned(fRegPolMachine) then
  begin
    if not fGPT.WriteRegistryPol(GPO, False, fRegPolMachine) then
    begin
      if (fGPT.LastError <> '') then
        MessageDlg(rsGPOTabConfiguration,
          FormatUtf8('%: %', [rsGPOConfigurationRegWriteFailed, fGPT.LastError]),
          mtError, [mbOK], 0)
      else
        MessageDlg(rsGPOTabConfiguration, rsGPOConfigurationRegWriteFailed,
          mtError, [mbOK], 0);
      Exit;
    end;
  end;

  // Step 5 of the GPO modification flow: update the GPT.INI on the SYSVOL
  // (version, display name and options) before bumping the versionNumber.
  if CheckBox_UpdateGptIni.Checked then
  begin
    GptIni.Version := 0;
    GptIni.DisplayName := '';
    GptIni.Options := 0;
    GptIni.HasVersion := False;
    GptIni.HasDisplayName := False;
    GptIni.HasOptions := False;

    // Keep the fields of the current GPT.INI when it can be read.
    fGPT.ReadGptIni(GPO, GptIni);

    GptIni.Version := NewVersionNumber;
    GptIni.HasVersion := True;
    GptIni.DisplayName := GPO.DisplayName;
    GptIni.HasDisplayName := True;

    // The options follow the staged flags value when there is one.
    StagedFlags := '';
    if GetStagedValue(GPO_ATTR_FLAGS, StagedFlags) then
      GptIni.Options := StrToIntDef(StagedFlags, GPO.Flags)
    else
      GptIni.Options := GPO.Flags;
    GptIni.HasOptions := True;

    if not fGPT.UpdateGptIni(GPO, GptIni) then
    begin
      if (fGPT.LastError <> '') then
        MessageDlg(rsGPOTabConfiguration,
          FormatUtf8('%: %', [rsGPOTechnicalGPTIniUpdateFailed, fGPT.LastError]),
          mtError, [mbOK], 0)
      else
        MessageDlg(rsGPOTabConfiguration, rsGPOTechnicalGPTIniUpdateFailed,
          mtError, [mbOK], 0);
      Exit;
    end;
  end;

  BackupOnError := LdapClient.OnError;
  LdapClient.OnError := nil;
  try
    if not fLogic.ApplyModifications(GPO, Modifications) then
    begin
      MessageDlg(rsGPOTabConfiguration, rsGPOConfigurationFailed, mtError, [mbOK], 0);
      Exit;
    end;
  finally
    LdapClient.OnError := BackupOnError;
  end;

  MessageDlg(rsGPOTabConfiguration, rsGPOConfigurationSaved, mtInformation, [mbOK], 0);
  ClearStaging;
  Action_RefreshExecute(nil);
end;

procedure TFrmModuleGPO.LdapConnectEvent(Sender: TObject);
begin
  fModule.NeedRefresh := True;
end;

procedure TFrmModuleGPO.LdapCloseEvent(Sender: TObject);
begin
  ClearList;
  ClearStaging;
  FreeAndNil(fRegPolUser);
  FreeAndNil(fRegPolMachine);
  TisGrid_GPOList.Clear;
  UpdateSummary(nil);
  UpdateAttributes(nil);
  UpdateParameters(nil);
  UpdateEditor(nil);
  TisGrid_GPTFiles.Clear;
  TisGrid_Parameters.Clear;
  TisGrid_RegistryValues.Clear;
end;

procedure TFrmModuleGPO.OnGPOOptionsChanged(Option: TOption);
begin
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, 'GPO option changed', Self);
end;

constructor TFrmModuleGPO.Create(Context: IOpenRSATUIContext);
begin
  inherited Create(Context.ComponentOwner);

  fLog := TOpenRSATLog;
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, 'Create', Self);

  fIContext := Context;
  fModule := TModuleGPO.Create(Context.RSAT);
  fLogic := TGPOLogic.Create(Context.RSAT.LdapClient);
  fGPT := TGPTCore.Create(Context.RSAT.LdapClient);

  fModule.Option.RegisterObserver(@OnGPOOptionsChanged);

  Action_Refresh.Caption := rsGPOActionRefresh;
  Action_Create.Caption := rsGPOActionCreate;
  Action_Rename.Caption := rsRename;
  Action_Duplicate.Caption := rsGPOActionDuplicate;
  Action_Delete.Caption := rsDelete;

  Label_GPOList.Caption := rsGPOList;
  TabSheet_Summary.Caption := rsGPOTabSummary;
  TabSheet_Configuration.Caption := rsGPOTabConfiguration;
  TabSheet_Catalog.Caption := rsGPOTabCatalog;
  TabSheet_Technical.Caption := rsGPOTabTechnical;

  Button_ParameterStage.Caption := rsGPOConfigurationStage;
  Button_ParameterDiscard.Caption := rsGPOConfigurationDiscard;
  Button_ConfigurationSave.Caption := rsGPOConfigurationSave;
  Button_ConfigurationDiscardAll.Caption := rsGPOConfigurationDiscard;
  CheckBox_IncrementUser.Caption := rsGPOConfigurationIncrementUser;
  CheckBox_IncrementMachine.Caption := rsGPOConfigurationIncrementMachine;
  CheckBox_UpdateGptIni.Caption := rsGPOConfigurationUpdateGptIni;
  Button_CatalogAdd.Caption := rsGPOCatalogAdd;
  Button_GPTFilesRefresh.Caption := rsGPOTechnicalGPTFilesRefresh;
  Button_GPTIniRead.Caption := rsGPOTechnicalGPTIniRead;
  Label_GPTFiles.Caption := rsGPOTechnicalGPTFiles;

  Label_ConfigurationSource.Caption := rsGPOConfigurationSource;
  ComboBox_ConfigurationSource.Items.Clear;
  ComboBox_ConfigurationSource.Items.Add(rsGPOConfigurationSourceGPC);
  ComboBox_ConfigurationSource.Items.Add(rsGPOConfigurationSourceUser);
  ComboBox_ConfigurationSource.Items.Add(rsGPOConfigurationSourceMachine);
  ComboBox_ConfigurationSource.ItemIndex := 0;

  Button_RegValueApply.Caption := rsGPOConfigurationRegApply;
  Button_RegValueDelete.Caption := rsGPOConfigurationRegDelete;
  Button_RegKeyAdd.Caption := rsGPOConfigurationRegAddKey;
  Label_RegValueName.Caption := rsGPOConfigurationRegValueName;
  Label_RegValueType.Caption := rsGPOConfigurationRegValueType;
  Label_RegValueData.Caption := rsGPOConfigurationRegValueData;
  ComboBox_RegValueType.ItemIndex := 0;
end;

destructor TFrmModuleGPO.Destroy;
begin
  ClearList;
  FreeAndNil(fRegPolUser);
  FreeAndNil(fRegPolMachine);
  FreeAndNil(fGPT);
  FreeAndNil(fLogic);
  FreeAndNil(fModule);

  inherited Destroy;
end;

procedure TFrmModuleGPO.Refresh;
var
  BackupCursor: TCursor;
begin
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, 'Refresh', Self);

  // The catalog does not depend on the LDAP connection.
  UpdateCatalog;

  ClearList;
  ClearStaging;
  FreeAndNil(fRegPolUser);
  FreeAndNil(fRegPolMachine);
  if not Assigned(LdapClient) or not LdapClient.Connected then
  begin
    UpdateGPOList;
    UpdateSummary(nil);
    UpdateAttributes(nil);
    UpdateParameters(nil);
    UpdateEditor(nil);
    Exit;
  end;

  BackupCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    fGPOs := fLogic.List(LdapClient.DefaultDN);
  finally
    Screen.Cursor := BackupCursor;
  end;

  UpdateGPOList;
  UpdateSummary(GetFocusedGPO);
  UpdateAttributes(GetFocusedGPO);
  UpdateParameters(GetFocusedGPO);
  UpdateEditor(GetFocusedGPO);
end;

procedure TFrmModuleGPO.Load;
begin
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, 'Load', Self);
end;

function TFrmModuleGPO.GetModule: TModule;
begin
  result := fModule;
end;

function TFrmModuleGPO.GetFrmOptionClass: TFrameOptionClass;
begin
  result := nil;
end;

function TFrmModuleGPO.GetOnLdapConnect: TNotifyEvent;
begin
  result := @LdapConnectEvent;
end;

function TFrmModuleGPO.GetOnLdapClose: TNotifyEvent;
begin
  result := @LdapCloseEvent;
end;

end.