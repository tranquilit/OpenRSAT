unit uvissearch;

{$mode ObjFPC}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
  ActnList,
  Buttons,
  Classes,
  ComCtrls,
  ExtCtrls,
  Forms,
  StdCtrls,
  Menus,
  tis.ui.grid.core, tis.ui.searchedit,
  mormot.core.variants,
  mormot.core.base,
  mormot.net.ldap,
  VirtualTrees,
  Controls,
  ufrmmoduleaduc;

type

  { TVisSearch }

  TVisSearch = class(TForm)
    Action_AdvAdd: TAction;
    Action_AdvDel: TAction;
    Action_Properties: TAction;
    Action_SearchFrom: TAction;
    Action_SearchNew: TAction;
    Action_ShowInView: TAction;
    BitBtn_Add: TBitBtn;
    BitBtn_Del: TBitBtn;
    ComboBox_AdvCondition: TComboBox;
    Edit_AdvValue: TEdit;
    Edit_BasicValue: TEdit;
    Edit_PageSize: TEdit;
    Edit_PageCount: TEdit;
    Label_ResultCount: TLabel;
    Label_BasicValue: TLabel;
    Label_ExpFilter: TLabel;
    Label_PageSize: TLabel;
    Label_PageCount: TLabel;
    Memo_ExpFilter: TMemo;
    MenuItem_Properties: TMenuItem;
    MenuItem_SearchFrom: TMenuItem;
    MenuItem_SearchNew: TMenuItem;
    MenuItem_ShowInView: TMenuItem;
    Panel_Path: TPanel;
      Label_Path: TLabel;
      Edit_Path: TEdit;
      BitBtn_PathChange: TBitBtn;
      PopupMenu1: TPopupMenu;
      RadioButton_AllCondition: TRadioButton;
      RadioButton_OneCondition: TRadioButton;
    Shape_LineTop: TShape;
    Panel_Search: TPanel;
      PageControl_Search: TPageControl;
      Splitter1: TSplitter;
      TabSheet_Basic: TTabSheet;
      TabSheet_Advanced: TTabSheet;
      TabSheet_Expert: TTabSheet;
        TabSheet_Options: TTabSheet;
          Label_SearchScope: TLabel;
          ComboBox_SearchScope: TComboBox;
      Panel_Buttons: TPanel;
        BitBtn_Search: TBitBtn;
        Timer_SearchInGrid: TTimer;
        TisGrid_AdvDetails: TTisGrid;
        TisGrid_Result: TTisGrid;
    TisSearchEdit_AdvKey: TTisSearchEdit;
    Panel_Result: TPanel;
    ActionList: TActionList;
      Action_ChangeDN: TAction;
      Action_Search: TAction;
    procedure Action_AdvAddExecute(Sender: TObject);
    procedure Action_AdvAddUpdate(Sender: TObject);
    procedure Action_AdvDelExecute(Sender: TObject);
    procedure Action_AdvDelUpdate(Sender: TObject);
    procedure Action_ChangeDNExecute(Sender: TObject);
    procedure Action_PropertiesExecute(Sender: TObject);
    procedure Action_PropertiesUpdate(Sender: TObject);
    procedure Action_SearchExecute(Sender: TObject);
    procedure Action_SearchFromExecute(Sender: TObject);
    procedure Action_SearchFromUpdate(Sender: TObject);
    procedure Action_SearchNewExecute(Sender: TObject);
    procedure Action_SearchNewUpdate(Sender: TObject);
    procedure Action_SearchUpdate(Sender: TObject);
    procedure Action_ShowInViewExecute(Sender: TObject);
    procedure Action_ShowInViewUpdate(Sender: TObject);
    procedure ComboBox_AdvConditionChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure Timer_SearchInGridTimer(Sender: TObject);
    procedure TisGrid_ResultGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure TisGrid_ResultKeyPress(Sender: TObject; var Key: char);
  private
    SearchWord: RawUtf8;
    fModule: TFrmModuleADUC;
  public
    constructor Create(TheOwner: TComponent; AModule: TFrmModuleADUC); reintroduce;
  end;

implementation

uses
  Dialogs,
  SysUtils,
  mormot.core.data,
  mormot.core.log,
  mormot.core.text,
  ucommon,
  ucommonui,
  ucoredatamodule,
  ursatldapclientui,
  uvischangedn,
  ufrmrsat;

{$R *.lfm}

{ TVisSearch }

// Form
procedure TVisSearch.FormShow(Sender: TObject);
var
  LdapDisplayName: RawUtf8;
  SearchResult: TLdapResult;
begin
  ComboBox_SearchScope.Items.AddStrings([
    rsSearchScopeBaseObject,
    rsSearchScopeSingleLevel,
    rsSearchScopeWholeSubtree
  ]);
  ComboBox_SearchScope.ItemIndex := 2;
  PageControl_Search.ActivePageIndex := 0;
  Edit_BasicValue.SetFocus;
  UnifyButtonsWidth([BitBtn_Add, BitBtn_Del]);

  TisSearchEdit_AdvKey.Clear;
  TisSearchEdit_AdvKey.Items.BeginUpdate;
  FrmRSAT.RSAT.LdapClient.SearchBegin();
  try
    FrmRSAT.RSAT.LdapClient.SearchScope := lssWholeSubtree;

    repeat
      if not FrmRSAT.RSAT.LdapClient.Search(FrmRSAT.RSAT.LdapClient.SchemaDN, False, '', ['lDAPDisplayName']) then
        Exit;

      for SearchResult in FrmRSAT.RSAT.LdapClient.SearchResult.Items do
      begin
        if not Assigned(SearchResult) then
          continue;
        LdapDisplayName := SearchResult.Find('lDAPDisplayName').GetReadable();
        if LdapDisplayName = '' then
          continue;
        TisSearchEdit_AdvKey.Items.Add(LdapDisplayName);
      end;
    until FrmRSAT.RSAT.LdapClient.SearchCookie = '';
  finally
    FrmRSAT.RSAT.LdapClient.SearchEnd;
    TisSearchEdit_AdvKey.Items.EndUpdate;
  end;
end;

procedure TVisSearch.Timer_SearchInGridTimer(Sender: TObject);
begin
  Timer_SearchInGrid.Enabled := False;
end;

procedure TVisSearch.TisGrid_ResultGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  data: PDocVariantData;
begin
  if TisGrid_Result.FindColumnByIndex(Column).PropertyName = 'name' then
  begin
    data := TisGrid_Result.GetNodeAsPDocVariantData(Node);
    if Assigned(data) and data^.Exists('objectClass') then
      ImageIndex := ObjectClassToImageIndex(data^.S['objectClass']);
  end;
end;

procedure TVisSearch.TisGrid_ResultKeyPress(Sender: TObject; var Key: char);
begin
  if (Timer_SearchInGrid.Enabled = False) then
    SearchWord := ''
  else
    Timer_SearchInGrid.Enabled := False;
  Insert(key, SearchWord, Length(SearchWord) + 1);
  TisGrid_Result.Search(SearchWord);
  Timer_SearchInGrid.Enabled := True;
end;

procedure TVisSearch.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TVisSearch.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    27: Close;
  end;
end;

constructor TVisSearch.Create(TheOwner: TComponent; AModule: TFrmModuleADUC);
begin
  inherited Create(TheOwner);

  fModule := AModule;
end;

procedure TVisSearch.Action_SearchFromExecute(Sender: TObject);
begin
  Action_SearchNewExecute(Sender);
  Action_SearchExecute(Sender);
end;

procedure TVisSearch.Action_SearchFromUpdate(Sender: TObject);
begin
  Action_SearchFrom.Enabled := Assigned(TisGrid_Result.FocusedNode);
end;

procedure TVisSearch.Action_SearchNewExecute(Sender: TObject);
begin
  Edit_Path.Text := TisGrid_Result.FocusedRow^.S['distinguishedName'];
end;

procedure TVisSearch.Action_SearchNewUpdate(Sender: TObject);
begin
  Action_SearchNew.Enabled := Assigned(TisGrid_Result.FocusedNode);
end;

procedure TVisSearch.Action_SearchUpdate(Sender: TObject);
begin
  Action_Search.Enabled := ((PageControl_Search.ActivePageIndex = 1) and (TisGrid_AdvDetails.TotalCount > 0)) or
                           (PageControl_Search.ActivePageIndex = 0) or (PageControl_Search.ActivePageIndex = 2);
end;

procedure TVisSearch.Action_ShowInViewExecute(Sender: TObject);
var
  aLog: ISynLog;
  DN: RawUtf8;
begin
  aLog := TSynLog.Enter('Show in view', []);

  if not Assigned(TisGrid_Result.FocusedRow) or
     not TisGrid_Result.FocusedRow^.Exists('distinguishedName') then
  begin
    aLog.Log(sllWarning, 'Cannot show in view.');
    aLog.Log(sllDebug, 'No focused row or distinguishedName does not exists.');
    Exit;
  end;

  DN := TisGrid_Result.FocusedRow^.S['distinguishedName'];

  fModule.focus(DN);
end;

procedure TVisSearch.Action_ShowInViewUpdate(Sender: TObject);
begin
  Action_ShowInView.Enabled := TisGrid_Result.Enabled and TisGrid_Result.Focused and Assigned(TisGrid_Result.FocusedNode);
end;

procedure TVisSearch.ComboBox_AdvConditionChange(Sender: TObject);
begin
  Edit_AdvValue.Enabled := (ComboBox_AdvCondition.ItemIndex < 4);
end;

procedure TVisSearch.Action_ChangeDNExecute(Sender: TObject); // ChangeDN
begin
  with TVisChangeDN.create(self, FrmRSAT.RSAT.LdapClient, Edit_Path.Text, FrmRSAT.RSAT.LdapClient.DefaultDN) do
  try
    if ShowModal = mrOk then
      Edit_Path.Text := SelectedDN;
  finally
    Free();
  end;
end;

procedure TVisSearch.Action_AdvAddExecute(Sender: TObject);
var
  data: TDocVariantData;
begin
  data.init();

  data.AddValue('key', Trim(TisSearchEdit_AdvKey.Text));
  data.AddValue('condition', Trim(ComboBox_AdvCondition.Text));
  data.AddValue('value', Trim(Edit_AdvValue.Text));
  TisGrid_AdvDetails.Data.AddItem(data);
  TisGrid_AdvDetails.LoadData();
end;

procedure TVisSearch.Action_AdvAddUpdate(Sender: TObject);
begin
  Action_AdvAdd.Enabled := (TisSearchEdit_AdvKey.Text <> '') and
  (((ComboBox_AdvCondition.ItemIndex < 4) and (Edit_AdvValue.Text <> '')) or (ComboBox_AdvCondition.ItemIndex >= 4));
end;

procedure TVisSearch.Action_AdvDelExecute(Sender: TObject);
begin
  TisGrid_AdvDetails.DeleteSelectedRows;
end;

procedure TVisSearch.Action_AdvDelUpdate(Sender: TObject);
begin
  Action_AdvDel.Enabled := (TisGrid_AdvDetails.SelectedCount > 0);
end;

procedure TVisSearch.Action_PropertiesExecute(Sender: TObject);
begin
  Action_ShowInView.Execute;
  FrmRSAT.OpenProperty(TisGrid_Result.FocusedRow^.S['distinguishedName'], TisGrid_Result.FocusedRow^.S['name']);
end;

procedure TVisSearch.Action_PropertiesUpdate(Sender: TObject);
begin
  Action_Properties.Enabled := TisGrid_Result.Enabled and TisGrid_Result.Focused and Assigned(TisGrid_Result.FocusedNode);
end;

procedure TVisSearch.Action_SearchExecute(Sender: TObject);
var
  Filter, attribute: RawUtf8;
  Attributes: TRawUtf8DynArray;
  aLog: ISynLog;
  pageSize, pageCount: Longint;
  count: Integer;
  item: TLdapResult;
  data: TDocVariantData;
  attr: TLdapAttribute;
  row: PDocVariantData;
begin
  Filter := '';
  Attributes := ['objectClass', 'distinguishedName', 'name', 'description'];
  count := 0;
  data.Init();

  TisGrid_Result.Data.Clear;
  case PageControl_Search.ActivePageIndex of
    0:
    begin
      Filter := Trim(Edit_BasicValue.Text);
      if not (Filter = '') then
        Filter := FormatUtf8('|(cn=*%*)(dn=*%*)(name=*%*)', [Filter, Filter, Filter, Filter])
      else
        Filter := 'name=*';
    end;
    1:
    begin
      if (TisGrid_AdvDetails.TotalCount > 0) then
      begin
        if RadioButton_AllCondition.Checked then
          Filter := '&'
        else
          Filter := '|';
        for row in TisGrid_AdvDetails.Data.Objects do
        begin
          if not Assigned(row) then
            continue;
          case ComboBox_AdvCondition.ItemIndex of
            // Start with
            0: Filter := FormatUtf8('%(%=%*)', [Filter, row^.S['key'], row^.S['value']]);
            // End with
            1: Filter := FormatUtf8('%(%=*%)', [Filter, row^.S['key'], row^.S['value']]);
            // Equal
            2: Filter := FormatUtf8('%(%=%)', [Filter, row^.S['key'], row^.S['value']]);
            // Is different
            3: Filter := FormatUtf8('%(!(%=%))', [Filter, row^.S['key'], row^.S['value']]);
            // Present
            4: Filter := FormatUtf8('%(%=*)', [Filter, row^.S['key']]);
            // Absent
            5: Filter := FormatUtf8('%(!(%=*))', [Filter, row^.S['key']]);
          end;
        end;
      end;
    end;
    2:
    begin
      Filter := Trim(Memo_ExpFilter.Text);
    end;
    else
      Exit;
  end;

  aLog := TSynLog.Enter('Search', []);
  if not TryStrToInt(Edit_PageSize.Text, pageSize) or
     not TryStrToInt(Edit_PageCount.Text, pageCount) then
    aLog.Log(sllWarning, 'Cannot search.');

  FrmRSAT.RSAT.LdapClient.SearchBegin(pageSize);
  case ComboBox_SearchScope.ItemIndex of
    0: FrmRSAT.RSAT.LdapClient.SearchScope := lssBaseObject;
    1: FrmRSAT.RSAT.LdapClient.SearchScope := lssSingleLevel;
    2: FrmRSAT.RSAT.LdapClient.SearchScope := lssWholeSubtree;
  end;

  TisGrid_Result.BeginUpdate;
  try
    repeat
      if not FrmRSAT.RSAT.LdapClient.Search(Trim(Edit_Path.Text), False, Filter, Attributes) then
        Exit;
      for item in FrmRSAT.RSAT.LdapClient.SearchResult.Items do
      begin
        if not Assigned(item) or (item.Attributes.Count <= 0) then
          continue;
        for attribute in attributes do
        begin
          attr := item.Find(attribute);
          if not Assigned(attr) then
            continue;
          case attribute of
            'objectClass': data.AddValue(attribute, attr.GetReadable(attr.Count - 1));
          else
            data.AddValue(attribute, attr.GetReadable());
          end;
        end;
        TisGrid_Result.Data.AddItem(data);
        data.Clear;
      end;
      Inc(count);
    until (FrmRSAT.RSAT.LdapClient.SearchCookie = '') or (count = pageCount);
  finally
    FrmRSAT.RSAT.LdapClient.SearchEnd;
    TisGrid_Result.EndUpdate;
    TisGrid_Result.LoadData;
  end;
end;

end.

