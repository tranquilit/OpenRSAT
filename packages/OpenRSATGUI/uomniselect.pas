unit uOmniselect;

{$mode ObjFPC}{$H+}

interface

uses
  // Lazarus / fpc
  ActnList,
  Buttons,
  ComCtrls,
  Classes,
  ExtCtrls,
  Forms,
  Menus,
  StdCtrls,
  SysUtils,
  // Submodules
  mormot.core.base,
  mormot.core.variants,
  mormot.net.ldap,
  tis.ui.grid.core,
  tis.ui.searchedit,
  Virtualtrees,
  // Rsat
  Controls;

type

  { TVisOmniselect }

  TVisOmniselect = class(TForm)
    Action_MenuItemClick: TAction;
    Panel_Select: TPanel;
    PopupMenu_objList: TPopupMenu;
      TisGrid_Items: TTisGrid;
      ToolBar_Attributes: TToolBar;
        ToolButton_AttributesList: TToolButton;
    Panel_Bottom: TPanel;
      Label_Search: TLabel;
      SearchEdit_List: TTisSearchEdit;
	  Button_Cancel: TBitBtn;
      Button_OK: TBitBtn;
    ActionList: TActionList;
      Action_OK: TAction;
    procedure Action_MenuItemClickExecute(Sender: TObject);
    procedure Action_OKUpdate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure SearchEdit_ListSearch(Sender: TObject; const aText: string);
    procedure TisGrid_ItemsGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure ToolButton_AttributesListClick(Sender: TObject);
  private
    fLdap: TLdapClient;
    fBaseDN: RawUtf8;

    fAllowedObjectClass: TStringArray;
    fAllowMultiselect: Boolean;
    fFilter: RawUtf8;

    function GetSelectedObjects: TRawUtf8DynArray;
    procedure ListRefresh();
  public
    constructor Create(TheOwner: TComponent; Ldap: TLdapClient; AllowedObjectClass: TStringArray; baseDN: RawUtf8 = ''; AllowMultiselect: Boolean = True; Filter: RawUtf8 = ''); reintroduce;
    property SelectedObjects: TRawUtf8DynArray read GetSelectedObjects;
  end;

implementation
uses
  Dialogs,
  Graphics,
  mormot.core.data,
  mormot.core.text,
  mormot.core.unicode,
  ucoredatamodule,
  ursatldapclientui,
  ucommonui;
{$R *.lfm}

{ TVisOmniselect }

// Form
constructor TVisOmniselect.Create(TheOwner: TComponent; Ldap: TLdapClient;
  AllowedObjectClass: TStringArray; baseDN: RawUtf8; AllowMultiselect: Boolean;
  Filter: RawUtf8);
var
  objectClass: String;
  Item: TMenuItem;
begin
  Inherited Create(TheOwner);

  fLdap := Ldap;
  fBaseDN := baseDN;
  fAllowedObjectClass := AllowedObjectClass;
  fAllowMultiselect := AllowMultiselect;
  fFilter := Filter;

  if AllowMultiselect then
    TisGrid_Items.TreeOptions.SelectionOptions := TisGrid_Items.TreeOptions.SelectionOptions + [toMultiSelect];

  for objectClass in fAllowedObjectClass do
  begin
    Item := TMenuItem.Create(self);
    Item.Checked := True;
    Item.AutoCheck := True;
    Item.ShowAlwaysCheckable := True;
    Item.RightJustify := True;
    Item.Caption := objectClass;
    Item.ImageIndex := CoreDataModule.objectClassToImageIndex(objectClass);
    Item.OnClick := @Action_MenuItemClickExecute;
    PopupMenu_objList.Items.Add(Item); // Item is stored inside so no memleak
  end;
end;

procedure TVisOmniselect.FormShow(Sender: TObject);
begin
  UnifyButtonsWidth([Button_Cancel, Button_OK]);
  ListRefresh();
  SearchEdit_List.SetFocus;
end;

// List
procedure TVisOmniselect.ListRefresh();
var
  tmp, SearchName, SelectedDNs, SearchObjectClasses, SearchFilter: String;
  i, pageCount: Integer;
  Doc: PDocVariantData;
  Nodes: TNodeArray;
  Selected: Array of String;
  item: TLdapResult;
  newItem: TDocVariantData;
  escapedName, attr, pattern: RawUtf8;
  node: PVirtualNode;
begin
  pageCount := {VisMain.Storage.Options.SearchPageNumber}1;

  // Search Name
  SearchName := '';
  if (SearchEdit_List.Text <> '') then
  begin
    escapedName := LdapEscape(SearchEdit_List.Text);
    for attr in ['sAMAccountName', 'name'] do
    begin
      for pattern in ['(%=*%*)', '(%=%)', '(%=*%)', '(%=%*)'] do
        SearchName := FormatUtf8('%%', [SearchName, FormatUtf8(pattern, [attr, escapedName])]);
    end;
  end;

  // Add Previous selected
  Selected := [];
  SetLength(Selected, TisGrid_Items.SelectedCount);
  SelectedDNs := '';
  i := 0;
  for Doc in TisGrid_Items.SelectedRows.Objects do
  begin
    Selected[i] := Doc^.S['distinguishedName'];
    SelectedDNs := FormatUtf8('%(distinguishedName=%)', [SelectedDNs, LdapEscape(Selected[i])]);
    Inc(i);
  end;

  // Add constraint to objectClass
  SearchObjectClasses := '';
  for i := 0 to PopupMenu_objList.Items.Count - 1 do
  begin
    if not PopupMenu_objList.Items.Items[i].Checked then
      continue;
    tmp := PopupMenu_objList.Items.Items[i].Caption;
    if tmp = 'user' then // https://ldapwiki.com/wiki/Wiki.jsp?page=ObjectClass%20vs%20ObjectCategory
      SearchObjectClasses += '(&(objectCategory=user)(objectClass=user))'
    else
      SearchObjectClasses += FormatUtf8('(objectClass=%)', [LdapEscape(tmp)]);
  end;

  // Fill SearchFilter
  SearchFilter := '';
  if SearchObjectClasses <> '' then
    SearchFilter := FormatUtf8('(|%)', [SearchObjectClasses]);
  if SearchName <> '' then
    SearchFilter := FormatUtf8('(&(|%)%)', [SearchName, SearchFilter]);
  if SelectedDNs <> '' then
    SearchFilter := FormatUtf8('(|%%)', [SelectedDNs, SearchFilter]);
  if fFilter <> '' then
    SearchFilter := FormatUtf8('(&%%)', [fFilter, SearchFilter]);

  newItem.init();

  // Fill TisGrid_Items
  TisGrid_Items.Cursor := crHourGlass;
  TisGrid_Items.BeginUpdate();
  fLdap.SearchBegin({VisMain.Storage.Options.SearchPageSize});
  try
    TisGrid_Items.Clear();
    fLdap.SearchScope := lssWholeSubtree;
    repeat
      if not fLdap.Search(fLdap.DefaultDN(fBaseDN), False, SearchFilter, ['name', 'objectClass', 'description', 'distinguishedName']) then
      begin
        ShowLdapSearchError(fLdap);
        Exit;
      end;
      for item in fLdap.SearchResult.Items do
      begin
        if not Assigned(item) then
          continue;
        newItem.AddOrUpdateValue('name', item.Find('name').GetReadable());
        newItem.AddOrUpdateValue('description', item.Find('description').GetReadable());
        newItem.AddOrUpdateValue('objectClass', item.Find('objectClass').GetVariant());
        newItem.AddOrUpdateValue('distinguishedName', item.find('distinguishedName').GetReadable());
        newItem.AddOrUpdateValue('folder', DNToCN(newItem.S['distinguishedName']));
        TisGrid_Items.Data.AddItem(newItem);
        newItem.Clear;
      end;
      Dec(pageCount);
    until (fLdap.SearchCookie = '') or (pageCount = 0);
  finally
    fLdap.SearchEnd;
    TisGrid_Items.Cursor := crDefault;
    TisGrid_Items.EndUpdate();
    TisGrid_Items.LoadData();
  end;
  // Set Selection

  TisGrid_Items.FocusedNode := nil;
  TisGrid_Items.ClearSelection;

  for tmp in Selected do
  begin
    Nodes := TisGrid_Items.GetNodesBy('distinguishedName', tmp);
    for node in Nodes do
      TisGrid_Items.AddToSelection(node);
  end;
  TisGrid_Items.FocusedNode := TisGrid_Items.GetFirstSelected();
end;

function TVisOmniselect.GetSelectedObjects: TRawUtf8DynArray;
var
  row: PDocVariantData;
  count: Integer;
begin
  result := [];
  SetLength(result, TisGrid_Items.SelectedCount - 1);

  count := 0;
  for row in TisGrid_Items.SelectedRows.Objects do
  begin
    Insert(row^.S['distinguishedName'], result, count);
    Inc(count);
  end;
end;

procedure TVisOmniselect.TisGrid_ItemsGetImageIndex(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
var
  objectClass: TRawUtf8DynArray;
begin
  if TisGrid_Items.FindColumnByIndex(Column).PropertyName <> 'name' then
    Exit;
  objectClass := TisGrid_Items.GetNodeAsPDocVariantData(Node)^.A['objectClass']^.ToRawUtf8DynArray;
  ImageIndex := CoreDataModule.objectClassToImageIndex(objectClass[Length(objectClass) - 1]);
end;

procedure TVisOmniselect.ToolButton_AttributesListClick(Sender: TObject);
var
  aPoint: TPoint;
begin
  aPoint := ToolButton_AttributesList.ClientToScreen(ToolButton_AttributesList.BoundsRect.TopLeft);
  PopupMenu_objList.PopUp(aPoint.X, aPoint.Y + ToolButton_AttributesList.Height);
end;

procedure TVisOmniselect.SearchEdit_ListSearch(Sender: TObject;
  const aText: string);
begin
  ListRefresh();
end;

procedure TVisOmniselect.Action_MenuItemClickExecute(Sender: TObject);
begin
  ListRefresh();
end;

procedure TVisOmniselect.Action_OKUpdate(Sender: TObject);
begin
  Action_OK.Enabled := TisGrid_Items.SelectedCount > 0;
end;

procedure TVisOmniselect.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    27: Close;
  end;
end;

end.
