unit ucommonui;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  controls,
  ExtCtrls,
  ActnList,
  Menus,
  tis.ui.grid.core,
  VirtualTrees,
  mormot.core.base;

procedure SearchInGrid(const Timer: TTimer; const TisGrid: TTisGrid; var SearchWord: RawUtf8; const Key: Char);
procedure TTisGridClearSelection(grid: TTisGrid);
function TTisGridGetNextUnselected(grid: TTisGrid; Node: PVirtualNode): PVirtualNode;
procedure UnifyButtonsWidth(Butons: array of TControl; default: Integer = -1);

procedure RegisterNewMenuAction(ObjectClass: RawUtf8; Action: TAction);
procedure RegisterNewMenuActions(ObjectClass: RawUtf8; Actions: Array of TAction);
procedure BuildNewMenuItem(var NewMenuItem: TMenuItem; ObjectClass: RawUtf8);

implementation
uses
  mormot.core.variants;

procedure SearchInGrid(const Timer: TTimer;
  const TisGrid: TTisGrid; var SearchWord: RawUtf8; const Key: Char);
var
  RowData: PDocVariantData;
  ColumnName: RawUtf8;
  Node, NextNode: PVirtualNode;
begin
  case Timer.Enabled of
    True: Timer.Enabled := False;
    False: SearchWord := '';
  end;

  Insert(Key, SearchWord, Length(SearchWord) + 1);
  ColumnName := TisGrid.FocusedColumnObject.PropertyName;
  NextNode := TisGrid.GetFirst();
  TisGrid.FocusedNode := nil;
  TisGrid.ClearSelection;
  while Assigned(NextNode) do
  begin
    RowData := TisGrid.GetNodeAsPDocVariantData(NextNode);
    Node := NextNode;
    NextNode := TisGrid.GetNext(NextNode);
    if not Assigned(RowData) or not RowData^.Exists(ColumnName) then
      continue;

    if RowData^.S[ColumnName].StartsWith(SearchWord, True) then
    begin
      TisGrid.FocusedNode := Node;
      TisGrid.AddToSelection(Node);
      Break;
    end;
  end;
  Timer.Enabled := True;
  TisGrid.ScrollIntoView(TisGrid.FocusedNode, True);
end;

procedure TTisGridClearSelection(grid: TTisGrid);
var
  i: Integer;
  Selection: TNodeArray;
begin
  Selection := grid.GetSortedSelection(True);
  for i := Length(Selection) - 1 downto 0 do
    grid.RemoveFromSelection(Selection[i]);
  grid.FocusedNode := grid.RootNode;
end;

function TTisGridGetNextUnselected(grid: TTisGrid; Node: PVirtualNode
  ): PVirtualNode;
begin
  repeat
    Node := grid.GetNext(Node);
  until not grid.Selected[Node];
  result := Node;
end;

// Laz
procedure UnifyButtonsWidth(Butons: array of TControl; default: Integer);
var
  BtnWidth, i: Integer;
begin
  BtnWidth := 0;
  if default = -1 then
  begin
    for i := 0 to Length(Butons) - 1 do
      if BtnWidth < Butons[i].Width then
        BtnWidth := Butons[i].Width;
  end else
    BtnWidth := default;
  for i := 0 to Length(Butons) - 1 do
  begin
    Butons[i].Constraints.MinWidth := BtnWidth;
    Butons[i].Width := BtnWidth;
  end;
end;

type
  TActionObjectClassLink = record
    ObjectClass: RawUtf8;
    Actions: Array of TAction;
  end;

  TActionObjectClassLinks = Array of TActionObjectClassLink;

var
  ActionObjectClassLinks: TActionObjectClassLinks;

function GetObjectClassIndex(ObjectClass: RawUtf8): Integer;
var
  i: Integer;
begin
  result := -1;
  for i := 0 to High(ActionObjectClassLinks) do
    if ActionObjectClassLinks[i].ObjectClass = ObjectClass then
    begin
      result := i;
      Exit;
    end;
end;

procedure RegisterNewMenuAction(ObjectClass: RawUtf8; Action: TAction);
var
  Idx, c: SizeInt;
begin
  Idx := GetObjectClassIndex(ObjectClass);
  if Idx < 0 then
  begin
    Idx := Length(ActionObjectClassLinks);
    SetLength(ActionObjectClassLinks, Idx + 1);
    ActionObjectClassLinks[Idx].ObjectClass := ObjectClass;
  end;
  c := Length(ActionObjectClassLinks[Idx].Actions);
  SetLength(ActionObjectClassLinks[Idx].Actions, c + 1);
  ActionObjectClassLinks[Idx].Actions[c] := Action;
end;

procedure RegisterNewMenuActions(ObjectClass: RawUtf8; Actions: array of TAction
  );
var
  Action: TAction;
begin
  for Action in Actions do
    RegisterNewMenuAction(ObjectClass, Action);
end;

function AnyContainsArray(const Elements1, Elements2: TRawUtf8DynArray): Boolean;
var
  i, j: Integer;
begin
  result := true;
  for i := 0 to High(Elements1) do
    for j := 0 to High(Elements2) do
      if Elements1[i] = Elements2[j] then
        Exit;
  result := False;
end;

procedure BuildNewMenuItem(var NewMenuItem: TMenuItem;
  ObjectClass: RawUtf8);
var
  ActionLink: TActionObjectClassLink;
  MenuItem: TMenuItem;
  MenuItemList: array of TMenuItem;
  Idx, i: Integer;
  Count: SizeInt;
begin
  NewMenuItem.Clear;
  try
    Idx := GetObjectClassIndex(ObjectClass);
    if Idx < 0 then
      Exit;
    Count := Length(ActionObjectClassLinks[Idx].Actions);

    SetLength(MenuItemList, Count);
    for i := 0 to Count - 1 do
    begin
      MenuItemList[i] := TMenuItem.Create(NewMenuItem);
      MenuItemList[i].Action := ActionObjectClassLinks[Idx].Actions[i];
    end;

    NewMenuItem.Add(MenuItemList);
  finally
    NewMenuItem.Visible := NewMenuItem.Count > 0;
  end;
end;

end.

