unit ufrmpropertygplinks;

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
  ActnList,
  Menus,
  Dialogs,
  tis.ui.grid.core,
  mormot.core.base,
  mormot.core.log,
  mormot.core.text,
  mormot.core.variants,
  mormot.net.ldap,
  upropertyframe,
  uproperty,
  ugplink,
  ulog, VirtualTrees;

type

  { TFrmPropertyGPLinks }

  TFrmPropertyGPLinks = class(TPropertyFrame)
    Action_EnforceLink: TAction;
    Action_DisableLink: TAction;
    Action_EnableLink: TAction;
    Action_Add: TAction;
    Action_Delete: TAction;
    ActionList_GPLinks: TActionList;
    BitBtn_GPLinksAdd: TBitBtn;
    BitBtn_GPLinksDelete: TBitBtn;
    Label_GPLinks: TLabel;
    MenuItem_Delete: TMenuItem;
    MenuItem_Enable: TMenuItem;
    MenuItem_Disable: TMenuItem;
    MenuItem_Enforce: TMenuItem;
    Panel_Bottom: TPanel;
    PopupMenu_GPLinks: TPopupMenu;
    Separator1: TMenuItem;
    Timer_GPLinks: TTimer;
    TisGrid_GPLinks: TTisGrid;
    procedure Action_AddExecute(Sender: TObject);
    procedure Action_DeleteExecute(Sender: TObject);
    procedure Action_DeleteUpdate(Sender: TObject);
    procedure Action_DisableLinkExecute(Sender: TObject);
    procedure Action_EnableLinkExecute(Sender: TObject);
    procedure Action_EnforceLinkExecute(Sender: TObject);
    procedure Timer_GPLinksTimer(Sender: TObject);
    procedure TisGrid_GPLinksKeyPress(Sender: TObject; var Key: char);
  private
    fLog: TSynLogClass;
    fProperty: TProperty;
    fSearchWord: RawUtf8;

    fGPLinks: TGPLinkList;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure Update(Props: TProperty); override;
  end;

implementation
uses
  uvislistgpo,
  ucommonui;

{$R *.lfm}

{ TFrmPropertyGPLinks }

procedure TFrmPropertyGPLinks.TisGrid_GPLinksKeyPress(Sender: TObject;
  var Key: char);
begin
  SearchInGrid(Timer_GPLinks, TisGrid_GPLinks, fSearchWord, Key);
end;

constructor TFrmPropertyGPLinks.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fLog := TOpenRSATLog;
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, 'Create', Self);

  Caption := 'GPLinks';

  fGPLinks := TGPLinkList.Create;
  fGPLinks.LdapClient := IContext.GetRSAT.LdapClient;
end;

destructor TFrmPropertyGPLinks.Destroy;
begin
  FreeAndNil(fGPLinks);

  inherited Destroy;
end;

procedure TFrmPropertyGPLinks.Timer_GPLinksTimer(Sender: TObject);
begin
  Timer_GPLinks.Enabled := False;
end;

procedure TFrmPropertyGPLinks.Action_AddExecute(Sender: TObject);
var
  Vis: TVisListGPO;
begin
  Vis := TVisListGPO.Create(Self, IContext);
  try
    if Vis.ShowModal <> mrOK then
      Exit;

    fGPLinks.Add(Vis.Selected);
    fProperty.Add('gPLink', fGPLinks.GPLink);
    Update(fProperty);
  finally
    FreeAndNil(Vis);
  end;
end;

procedure TFrmPropertyGPLinks.Action_DeleteExecute(Sender: TObject);
var
  mr: TModalResult;
  i: Integer;
  DV: TDocVariantData;
  Elements: TRawUtf8DynArray;
begin
  mr := MessageDlg('Delete', 'Do you want to delete selected rows?', mtConfirmation, mbYesNoCancel, 0);
  if mr <> mrYes then
    Exit;

  DV := TisGrid_GPLinks.SelectedRows;
  SetLength(Elements, DV.Count);
  for i := 0 to High(Elements) do
    Elements[i] := DV._[i]^.U['distinguishedName'];
  fGPLinks.Del(Elements);
  fProperty.Add('gPLink', fGPLinks.GPLink);
  Update(fProperty);
end;

procedure TFrmPropertyGPLinks.Action_DeleteUpdate(Sender: TObject);
begin
  Action_Delete.Enabled := TisGrid_GPLinks.SelectedCount > 0;
end;

procedure TFrmPropertyGPLinks.Action_DisableLinkExecute(Sender: TObject);
var
  DV: TDocVariantData;
  i: Integer;
  Elements: TRawUtf8DynArray;
begin
  DV := TisGrid_GPLinks.SelectedRows;

  SetLength(Elements, DV.Count);
  for i := 0 to High(Elements) do
    Elements[i] := DV._[i]^.U['distinguishedName'];
  fGPLinks.Disable(Elements);
  fProperty.Add('gPLink', fGPLinks.GPLink);
  Update(fProperty);
end;

procedure TFrmPropertyGPLinks.Action_EnableLinkExecute(Sender: TObject);
var
  DV: TDocVariantData;
  Elements: TRawUtf8DynArray;
  i: Integer;
begin
  DV := TisGrid_GPLinks.SelectedRows;

  SetLength(Elements, DV.Count);
  for i := 0 to High(Elements) do
    Elements[i] := DV._[i]^.U['distinguishedName'];
  fGPLinks.Enable(Elements);
  fProperty.Add('gPLink', fGPLinks.GPLink);
  Update(fProperty);
end;

procedure TFrmPropertyGPLinks.Action_EnforceLinkExecute(Sender: TObject);
var
  Elements: TRawUtf8DynArray;
  DV: TDocVariantData;
  i: Integer;
begin
  DV := TisGrid_GPLinks.SelectedRows;

  SetLength(Elements, DV.Count);
  for i := 0 to High(Elements) do
    Elements[i] := DV._[i]^.U['distinguishedName'];
  fGPLinks.Enforce(Elements);
  fProperty.Add('gPLink', fGPLinks.GPLink);
  Update(fProperty);
end;

procedure TFrmPropertyGPLinks.Update(Props: TProperty);
var
  DV: TDocVariantData;
  Elements: TRawUtf8DynArray;
  Node: PVirtualNode;
  NodeData: PDocVariantData;
  i: Integer;

  function Contains(const Element: RawUtf8; const Elements: TRawUtf8DynArray): Boolean;
  var
    i: Integer;
  begin
    result := True;
    for i := 0 to High(Elements) do
      if Element = Elements[i] then
        Exit;
    result := False;
  end;

begin
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, 'Update', Self);

  fProperty := Props;

  DV := TisGrid_GPLinks.SelectedRows;
  SetLength(Elements, DV.Count);
  for i := 0 to DV.Count - 1 do
    Elements[i] := DV._[i]^.U['distinguishedName'];

  TisGrid_GPLinks.Clear;
  fGPLinks.GPLink := fProperty.GetReadable('gPLink');
  TisGrid_GPLinks.Data := fGPLinks.Data;
  TisGrid_GPLinks.LoadData();
  Node := TisGrid_GPLinks.GetFirst();
  while Assigned(Node) do
  begin
    NodeData := TisGrid_GPLinks.GetNodeAsPDocVariantData(Node);
    TisGrid_GPLinks.Selected[Node] := Contains(NodeData^.U['distinguishedName'], Elements);
    Node := TisGrid_GPLinks.GetNext(Node);
  end;
end;

end.

