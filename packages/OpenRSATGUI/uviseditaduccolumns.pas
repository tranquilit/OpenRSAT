unit uviseditaduccolumns;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Types,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  Buttons,
  ButtonPanel,
  ActnList,
  umoduleaduc,
  ursatldapclient,
  mormot.core.variants,
  mormot.net.ldap,
  mormot.core.base,
  tis.ui.searchedit;

type

  { TVisEditADUCColumns }

  TVisEditADUCColumns = class(TForm)
    Action1: TAction;
    Action2: TAction;
    Action3: TAction;
    Action4: TAction;
    Action5: TAction;
    ActionList1: TActionList;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    ButtonPanel1: TButtonPanel;
    Label1: TLabel;
    Label2: TLabel;
    ListBox1: TListBox;
    ListBox2: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    TisSearchEdit1: TTisSearchEdit;
    TisSearchEdit2: TTisSearchEdit;
    procedure Action1Execute(Sender: TObject);
    procedure Action2Execute(Sender: TObject);
    procedure Action5Execute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure ListBox2DblClick(Sender: TObject);
    procedure TisSearchEdit1Search(Sender: TObject; const aText: string);
    procedure TisSearchEdit2Search(Sender: TObject; const aText: string);
  private
    fADUC: TModuleADUC;
    fSortString: RawUtf8;
    fMappingLdapNameDisplayName: TDocVariantData;

    function Compare(const A, B): Integer;
    procedure Sort(ListBox: TListBox; AText: RawUtf8);
    function GetDisplayedColumns: TRawUtf8DynArray;
    procedure OnSearchEventFillColumnAttributes(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent; ADUC: TModuleADUC); reintroduce;

    property DisplayedColumns: TRawUtf8DynArray read GetDisplayedColumns;

  end;

implementation
uses
  uhelpers,
  mormot.core.data,
  uconfig,
  umoduleaducoption,
  ufrmrsat;

{$R *.lfm}

{ TVisEditADUCColumns }

procedure TVisEditADUCColumns.FormCreate(Sender: TObject);
var
  Filter: RawUtf8;
  Attributes: TRawUtf8DynArray;
begin
  Filter := '(objectClass=attributeSchema)';
  Attributes := ['name', 'lDAPDisplayName'];

  ListBox1.Clear;
  ListBox2.Clear;
  With FrmRSAT.LdapClient do
  begin
    SearchBegin(fADUC.ADUCOption.SearchPageSize);
    try
      SearchScope := lssSingleLevel;
      OnSearch := @OnSearchEventFillColumnAttributes;

      repeat
        if not Search(SchemaDN, False, Filter, Attributes) then
          Exit;
      until SearchCookie = '';
    finally
      OnSearch := nil;
      SearchEnd;
    end;
  end;

  TisSearchEdit1.Search;
  TisSearchEdit2.Search;
end;

procedure TVisEditADUCColumns.ListBox1DblClick(Sender: TObject);
begin
  Action1.Execute;
end;

procedure TVisEditADUCColumns.ListBox2DblClick(Sender: TObject);
begin
  Action2.Execute;
end;

procedure TVisEditADUCColumns.TisSearchEdit1Search(Sender: TObject;
  const aText: string);
begin
  Sort(ListBox1, aText);
end;

procedure TVisEditADUCColumns.TisSearchEdit2Search(Sender: TObject;
  const aText: string);
begin
  Sort(ListBox2, aText);
end;

function TVisEditADUCColumns.Compare(const A, B): Integer;
var
  AStart, AContains, BStart, BContains: Boolean;
  alower, blower, clower: String;
begin
  result := 0;
  alower := String(a).ToLower;
  blower := String(b).ToLower;
  clower := String(fSortString).ToLower;

  AStart := alower.StartsWith(clower);
  AContains := alower.Contains(clower);
  BStart := blower.StartsWith(clower);
  BContains := blower.Contains(clower);

  if AContains and not BContains then
    result := -1
  else if BContains and not AContains then
    result := 1
  else if AContains and BContains then
  begin
    if AStart and not BStart then
      result := -1
    else if BStart and not AStart then
      result := 1
    else
      result := String.Compare(alower, blower);
  end
  else
    result := String.Compare(alower, blower);
end;

procedure TVisEditADUCColumns.Sort(ListBox: TListBox; AText: RawUtf8);
var
  Obj: TDynArray;
  StringArray: Types.TStringDynArray;
begin
  fSortString := AText;

  StringArray := ListBox.Items.ToStringArray;
  Obj.Init(TypeInfo(TStringArray), StringArray);
  Obj.Sort(@Compare);
  ListBox.Items.Clear;
  ListBox.Items.AddStrings(StringArray);
end;

procedure TVisEditADUCColumns.Action1Execute(Sender: TObject);
var
  i: Integer;
begin
  if ListBox1.Count <= 0 then
    Exit;

  ListBox1.Items.BeginUpdate;
  ListBox2.Items.BeginUpdate;
  try
    for i := Pred(ListBox1.Count) downto 0 do
      if ListBox1.Selected[i] then
      begin
        ListBox2.Items.Add(ListBox1.Items[i]);
        ListBox1.Items.Delete(i);
      end;
  finally
    ListBox2.Items.EndUpdate;
    ListBox1.Items.EndUpdate;
  end;
end;

procedure TVisEditADUCColumns.Action2Execute(Sender: TObject);
var
  i: Integer;
begin
  if ListBox2.Count <= 0 then
    Exit;

  ListBox2.Items.BeginUpdate;
  ListBox1.Items.BeginUpdate;
  try
    for i := Pred(ListBox2.Count) downto 0 do
      if ListBox2.Selected[i] then
      begin
        ListBox1.Items.Add(ListBox2.Items[i]);
        ListBox2.Items.Delete(i);
      end;
  finally
    ListBox1.Items.EndUpdate;
    ListBox2.Items.EndUpdate;
  end;
end;

procedure TVisEditADUCColumns.Action5Execute(Sender: TObject);
begin
  fADUC.ADUCOption.GridAttributesFilter := TRawUtf8DynArray(String(DEFAULT_GRID_ATTRIBUTES_FILTER).Split(';'));
  fADUC.ADUCOption.Save(OptionFilePath);
  FormCreate(Sender);
end;

procedure TVisEditADUCColumns.OnSearchEventFillColumnAttributes(Sender: TObject);
var
  SearchResultItem: TLdapResult;
  AttributeDisplayName, AttributeLdapName: RawUtf8;
begin

  ListBox1.Items.BeginUpdate;
  ListBox2.Items.BeginUpdate;
  try
    With FrmRSAT.LdapClient do
    begin
      for SearchResultItem in SearchResult.Items do
      begin
        if not Assigned(SearchResultItem) then
          continue;
        AttributeDisplayName := SearchResultItem.Find('name').GetReadable();
        AttributeLdapName := SearchResultItem.Find('lDAPDisplayName').GetReadable();
        case AttributeLdapName of
          'name': AttributeDisplayName := 'Name';
        end;

        fMappingLdapNameDisplayName.U[AttributeLdapName] := AttributeDisplayName;
        fMappingLdapNameDisplayName.U[AttributeDisplayName] := AttributeLdapName;

        if fADUC.ADUCOption.GridAttributesFilter.Contains(AttributeLdapName) then
          ListBox2.Items.Add(AttributeDisplayName)
        else
          ListBox1.Items.Add(AttributeDisplayName);
      end;
    end;
  finally
    ListBox2.Items.EndUpdate;
    ListBox1.Items.EndUpdate;
  end;
end;

function TVisEditADUCColumns.GetDisplayedColumns: TRawUtf8DynArray;
var
  Count, i: Integer;
begin
  result := nil;
  Count := ListBox2.Items.Count;
  if Count <= 0 then
    Exit;

  SetLength(result, Count);
  for i := Pred(Count) downto 0 do
    result[i] := fMappingLdapNameDisplayName.U[ListBox2.Items[i]];
end;

constructor TVisEditADUCColumns.Create(TheOwner: TComponent; ADUC: TModuleADUC);
begin
  Inherited Create(TheOwner);

  fADUC := ADUC;
  fMappingLdapNameDisplayName.Init();
end;

end.

