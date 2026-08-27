unit uvisobjectsselector;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ExtCtrls,
  StdCtrls,
  Buttons,
  tis.ui.grid.core,
  uvischangedn,
  mormot.core.base,
  mormot.core.text,
  mormot.net.ldap,
  mormot.core.variants,
  VirtualTrees,
  {$IFDEF WINDOWS}
  ActiveX
  {$ELSE}
  FakeActiveX
  {$ENDIF};

type

  TObjectTypeFilter = (
    otfUser,
    otfGroup,
    otfComputer,
    otfContact
  );

  TObjectTypesFilter = Set of TObjectTypeFilter;

  { TVisObjectsSelector }

  TVisObjectsSelector = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckGroup1: TCheckGroup;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    TisGrid1: TTisGrid;
    TisGrid2: TTisGrid;
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure TisGrid1DblClick(Sender: TObject);
    procedure TisGrid1DragDrop(Sender: TBaseVirtualTree; Source: TObject;
      DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
      const Pt: TPoint; var Effect: LongWord; Mode: TDropMode);
    procedure TisGrid1DragOver(Sender: TBaseVirtualTree; Source: TObject;
      Shift: TShiftState; State: TDragState; const Pt: TPoint; Mode: TDropMode;
      var Effect: LongWord; var Accept: Boolean);
    procedure TisGrid1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure TisGrid1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TisGrid1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure TisGrid1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TisGrid2DblClick(Sender: TObject);
    procedure TisGrid2DragDrop(Sender: TBaseVirtualTree; Source: TObject;
      DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
      const Pt: TPoint; var Effect: LongWord; Mode: TDropMode);
    procedure TisGrid2DragOver(Sender: TBaseVirtualTree; Source: TObject;
      Shift: TShiftState; State: TDragState; const Pt: TPoint; Mode: TDropMode;
      var Effect: LongWord; var Accept: Boolean);
    procedure TisGrid2KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure TisGrid2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TisGrid2MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure TisGrid2MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    fAllowedObjectTypes: TObjectTypesFilter;
    fAllowMultiSelect: Boolean;
    fExcludedObjects: TRawUtf8DynArray;
    fLdapClient: TLdapCLient;
    fSelectedObjectTypes: TObjectTypesFilter;

    // Edit by mouseUp / mouseDown
    // Read by mouseMove
    ReadyToDrag: Boolean;

    function GetAllowMultiSelect: Boolean;
    function GetObjectClassFilter: RawUtf8;
    function GetExclusionFilter: RawUtf8;
    function GetAttributes: TRawUtf8DynArray;
    function GetFilter: RawUtf8;
    function GetLocation: RawUtf8;
    function GetSelectedObjects: TRawUtf8DynArray;
    function GetSelectedObjectTypes: TObjectTypesFilter;
    procedure SetAllowedObjectTypes(AValue: TObjectTypesFilter);
    procedure SetAllowMultiSelect(AValue: Boolean);
    procedure SetLdapClient(AValue: TLdapClient);
    procedure SetSelectedObjectTypes(AValue: TObjectTypesFilter);

  public
    constructor Create(TheOwner: TComponent); override;

    procedure Search;
    procedure SelectLocation;

    procedure AddSelection;
    procedure RemoveSelection;

    function RowExistsInGrid(const AGrid: TTisGrid; Key, Value: RawUtf8): Boolean;

    property LdapClient: TLdapClient read fLdapClient write SetLdapClient;
    property Location: RawUtf8 read GetLocation;
    property Filter: RawUtf8 read GetFilter;
    property Attributes: TRawUtf8DynArray read GetAttributes;

    property SelectedObjects: TRawUtf8DynArray read GetSelectedObjects;
    property AllowedObjectTypes: TObjectTypesFilter read fAllowedObjectTypes write SetAllowedObjectTypes;
    property SelectedObjectTypes: TObjectTypesFilter read GetSelectedObjectTypes write SetSelectedObjectTypes;
    property ExcludedObjects: TRawUtf8DynArray read fExcludedObjects write fExcludedObjects;
    property AllowMultiSelect: Boolean read GetAllowMultiSelect write SetAllowMultiSelect;
  end;

const
  C_OBJECT_TYPE_FILTER_ATTRIBUTE: Array[TObjectTypeFilter] of RawUtf8 = (
    'user',
    'group',
    'computer',
    'contact'
  );

implementation
uses
  ursatldapclientui;

{$R *.lfm}

{ TVisObjectsSelector }

procedure TVisObjectsSelector.BitBtn3Click(Sender: TObject);
var
  C: TCursor;
begin
  TisGrid1.BeginUpdate;
  C := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    Search;
  finally
    TisGrid1.EndUpdate;
    Screen.Cursor := C;
  end;
end;

procedure TVisObjectsSelector.BitBtn4Click(Sender: TObject);
begin
  SelectLocation;
end;

procedure TVisObjectsSelector.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    27: Close;
  end;
end;

procedure TVisObjectsSelector.FormShow(Sender: TObject);
begin
  Edit1.SetFocus;
end;

procedure TVisObjectsSelector.TisGrid1DblClick(Sender: TObject);
begin
  AddSelection;
end;

procedure TVisObjectsSelector.TisGrid1DragDrop(Sender: TBaseVirtualTree;
  Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
  Shift: TShiftState; const Pt: TPoint; var Effect: LongWord; Mode: TDropMode);
begin
  RemoveSelection;
end;

procedure TVisObjectsSelector.TisGrid1DragOver(Sender: TBaseVirtualTree;
  Source: TObject; Shift: TShiftState; State: TDragState; const Pt: TPoint;
  Mode: TDropMode; var Effect: LongWord; var Accept: Boolean);
begin
  Accept := (Source = TisGrid2);
end;

procedure TVisObjectsSelector.TisGrid1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    32: AddSelection;
  end;
end;

procedure TVisObjectsSelector.TisGrid1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ReadyToDrag := (Button = mbLeft);
end;

procedure TVisObjectsSelector.TisGrid1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if ReadyToDrag then
  begin
    TisGrid1.BeginDrag(True);
    ReadyToDrag := False;
  end;
end;

procedure TVisObjectsSelector.TisGrid1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ReadyToDrag := False;
end;

procedure TVisObjectsSelector.TisGrid2DblClick(Sender: TObject);
begin
  RemoveSelection;
end;

procedure TVisObjectsSelector.TisGrid2DragDrop(Sender: TBaseVirtualTree;
  Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
  Shift: TShiftState; const Pt: TPoint; var Effect: LongWord; Mode: TDropMode);
begin
  AddSelection;
end;

procedure TVisObjectsSelector.TisGrid2DragOver(Sender: TBaseVirtualTree;
  Source: TObject; Shift: TShiftState; State: TDragState; const Pt: TPoint;
  Mode: TDropMode; var Effect: LongWord; var Accept: Boolean);
begin
  Accept := (Source = TisGrid1);
end;

procedure TVisObjectsSelector.TisGrid2KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    46: RemoveSelection;
  end;
end;

procedure TVisObjectsSelector.TisGrid2MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ReadyToDrag := (Button = mbLeft);
end;

procedure TVisObjectsSelector.TisGrid2MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if ReadyToDrag then
  begin
    TisGrid2.BeginDrag(True);
    ReadyToDrag := False;
  end;
end;

procedure TVisObjectsSelector.TisGrid2MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ReadyToDrag := False;
end;

function TVisObjectsSelector.GetObjectClassFilter: RawUtf8;
var
  ObjectTypes: TObjectTypesFilter;
  ot: TObjectTypeFilter;
begin
  result := '';
  ObjectTypes := SelectedObjectTypes;
  for ot := Low(TObjectTypeFilter) to High(TObjectTypeFilter) do
    if ot in ObjectTypes then
    begin
      if ot = otfUser then
        result := FormatUtf8('%(&(objectClass=user)(!(objectClass=computer)))', [result])
      else
        result := FormatUtf8('%(objectClass=%)', [result, C_OBJECT_TYPE_FILTER_ATTRIBUTE[ot]]);
    end;
  if result <> '' then
    result := FormatUtf8('(|%)', [result]);
end;

function TVisObjectsSelector.GetAllowMultiSelect: Boolean;
begin
  result := (toMultiSelect in TisGrid1.TreeOptions.SelectionOptions);
end;

function TVisObjectsSelector.GetExclusionFilter: RawUtf8;
var
  ExcludedObject: RawUtf8;
begin
  result := '';
  for ExcludedObject in ExcludedObjects do
    if ExcludedObject <> '' then
      result := FormatUtf8('%(distinguishedName=%)', [result, LdapEscape(ExcludedObject)]);

  if result <> '' then
    result := FormatUtf8('(!(&%))', [result]);
end;

procedure TVisObjectsSelector.SetLdapClient(AValue: TLdapClient);
begin
  if fLdapClient = AValue then
    Exit;

  fLdapClient := AValue;

  Edit3.Text := fLdapClient.DefaultDN();
end;

procedure TVisObjectsSelector.SetSelectedObjectTypes(AValue: TObjectTypesFilter
  );
begin
  if fSelectedObjectTypes = AValue then
    Exit;
  fSelectedObjectTypes := AValue;

  CheckBox1.Checked := (otfUser in fSelectedObjectTypes);
  CheckBox2.Checked := (otfGroup in fSelectedObjectTypes);
  CheckBox3.Checked := (otfComputer in fSelectedObjectTypes);
  CheckBox4.Checked := (otfContact in fSelectedObjectTypes);
end;

constructor TVisObjectsSelector.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fAllowedObjectTypes := [];
  fSelectedObjectTypes := [];
  fExcludedObjects := nil;
  fLdapClient := nil;
  fAllowMultiSelect := False;
  ReadyToDrag := False;
end;

function TVisObjectsSelector.GetAttributes: TRawUtf8DynArray;
begin
  result := ['name', 'description'];
end;

function TVisObjectsSelector.GetFilter: RawUtf8;
var
  ExclusionFilter: RawUtf8;
begin
  // ObjectClass && Name && Filter && ExclusionFilter
  result := GetObjectClassFilter;
  if Edit1.Text <> '' then
    result := FormatUtf8('%(anr=%*)', [result, LdapEscape(Edit1.Text)]);
  if Edit2.Text <> '' then
    result := FormatUtf8('%(%)', [result, Edit2.Text]);

  ExclusionFilter := GetExclusionFilter;
  if ExclusionFilter <> '' then
    result := FormatUtf8('%(%)', [result, ExclusionFilter]);

  if result <> '' then
    result := FormatUtf8('(&%)', [result]);
end;

function TVisObjectsSelector.GetLocation: RawUtf8;
begin
  result := Edit3.Text;
end;

function TVisObjectsSelector.GetSelectedObjects: TRawUtf8DynArray;
var
  i: Integer;
  P: PDocVariantData;
begin
  result := nil;
  if AllowMultiSelect then
  begin
    SetLength(result, TisGrid2.Data.Count);
    for i := 0 to TisGrid2.Data.Count - 1 do
    begin
      P := TisGrid2.Data._[i];
      if not Assigned(P) or not P^.Exists('objectName') then
        Continue;
      result[i] := P^.U['objectName'];
    end;
  end
  else
  begin
    if (TisGrid1.SelectedCount <> 1) then
      Exit;
    P := TisGrid1.GetNodeAsPDocVariantData(TisGrid1.GetFirstSelected());
    if not Assigned(P) or not P^.Exists('objectName') then
      Exit;
    result := [P^.U['objectName']];
  end;
end;

function TVisObjectsSelector.GetSelectedObjectTypes: TObjectTypesFilter;
begin
  result := [];

  if CheckBox1.Checked then
    Include(result, otfUser);
  if CheckBox2.Checked then
    Include(result, otfGroup);
  if CheckBox3.Checked then
    Include(result, otfComputer);
  if CheckBox4.Checked then
    Include(result, otfContact);
end;

procedure TVisObjectsSelector.SetAllowedObjectTypes(AValue: TObjectTypesFilter);
begin
  if fAllowedObjectTypes = AValue then
    Exit;

  fAllowedObjectTypes := AValue;

  CheckBox1.Visible := (otfUser in fAllowedObjectTypes);
  CheckBox2.Visible := (otfGroup in fAllowedObjectTypes);
  CheckBox3.Visible := (otfComputer in fAllowedObjectTypes);
  CheckBox4.Visible := (otfContact in fAllowedObjectTypes);
end;

procedure TVisObjectsSelector.SetAllowMultiSelect(AValue: Boolean);
var
  SelectionOptions: TVTSelectionOptions;
begin
  SelectionOptions := TisGrid1.TreeOptions.SelectionOptions;
  if AValue then
    Include(SelectionOptions, toMultiSelect)
  else
    Exclude(SelectionOptions, toMultiSelect);
  TisGrid1.TreeOptions.SelectionOptions := SelectionOptions;

  Panel2.Visible := AllowMultiSelect;
end;

procedure TVisObjectsSelector.Search;
var
  LdapResult: TDocVariantData;
begin
  if not Assigned(LdapClient) then
    raise Exception.Create('Missing LdapClient');

  LdapClient.SearchScope := lssWholeSubtree;

  if not LdapClient.SearchAllDocRaw(LdapResult, Location, Filter, Attributes, [roKnownValuesAsArray, roObjectNameAtRoot, roAutoRange, roSortByName]) then
  begin
    ShowLdapSearchError(LdapClient);
    Exit;
  end;

  LdapResult.TrimAsArray;
  TisGrid1.Data := LdapResult;
  TisGrid1.LoadData();
end;

procedure TVisObjectsSelector.SelectLocation;
var
  Vis: TVisChangeDN;
begin
  Vis := TVisChangeDN.Create(Self, LdapClient, Location, '');
  try
    if Vis.ShowModal <> mrOK then
      Exit;
    Edit3.Text := Vis.SelectedDN;
  finally
    FreeAndNil(Vis);
  end;
end;

procedure TVisObjectsSelector.AddSelection;
var
  Rows: TDocVariantData;
  i: Integer;
  P: PDocVariantData;
begin
  Rows := TisGrid1.SelectedRows;
  TisGrid2.BeginUpdate;
  try
    for i := 0 to Rows.Count - 1 do
    begin
      P := Rows._[i];
      if not Assigned(P) or not P^.Exists('objectName') or RowExistsInGrid(TisGrid2, 'objectName', P^.U['objectName']) then
        Continue;
      TisGrid2.Data.AddItem(P^);
    end;
  finally
    TisGrid2.EndUpdate;
    TisGrid2.LoadData();
  end;
end;

procedure TVisObjectsSelector.RemoveSelection;
var
  Rows: TDocVariantData;
begin
  Rows := TisGrid2.SelectedRows;
  if mrYes = MessageDlg('Delete rows', FormatUtf8('Remove % rows?', [Rows.Count]), mtConfirmation, mbYesNoCancel, 0) then
    TisGrid2.DeleteRows(@Rows);
end;

function TVisObjectsSelector.RowExistsInGrid(const AGrid: TTisGrid; Key,
  Value: RawUtf8): Boolean;
var
  i: Integer;
  P: PDocVariantData;
begin
  result := True;
  for i := 0 to AGrid.Data.Count - 1 do
  begin
    P := AGrid.Data._[i];
    if not Assigned(P) or not P^.Exists(Key) then
      Continue;
    if P^.U[Key] = Value then
      Exit;
  end;
  result := False;
end;

end.

