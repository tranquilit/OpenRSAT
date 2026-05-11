unit uvisselectobjectguid;

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
  ActnList,
  tis.ui.grid.core,
  mormot.core.base,
  mormot.core.variants,
  mormot.net.ldap,
  uselectobjectguidpresenter,
  VirtualTrees;

type

  { TVisSelectObjectGUID }

  TVisSelectObjectGUID = class(TForm, ISelectObjectGUIDView)
    Action1: TAction;
    ActionList1: TActionList;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Timer1: TTimer;
    TisGrid1: TTisGrid;
    procedure Action1Execute(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TisGrid1DblClick(Sender: TObject);
  private
    fPresenter: TSelectObjectGUIDPresenter;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure SetMultiSelect(AMultiSelect: Boolean);
    procedure SetAllowedType(AAllowedType: TRawUtf8DynArray);
    procedure SetLdapClient(ALdapClient: TLdapClient);
    function GetFilter: RawUtf8;
    function GetSelectedGUID: RawUtf8;
    function GetSelectedName: RawUtf8;
    procedure LoadData(AData: PDocVariantData);
    procedure ShowLdapError(ALdapErrorString: RawUtf8; ALdapError: TLdapError);
  end;

implementation

{$R *.lfm}

{ TVisSelectObjectGUID }

procedure TVisSelectObjectGUID.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;

  Action1.Execute;
end;

procedure TVisSelectObjectGUID.TisGrid1DblClick(Sender: TObject);
begin
  BitBtn2.Click;
end;

procedure TVisSelectObjectGUID.Action1Execute(Sender: TObject);
var
  c: TCursor;
begin
  c := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
    fPresenter.Search;
  finally
    Screen.Cursor := c;
    BitBtn2.Default := True;
  end;
end;

procedure TVisSelectObjectGUID.ComboBox1Change(Sender: TObject);
begin
  BitBtn1.Default := True;
end;

procedure TVisSelectObjectGUID.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    27: Close;
    117: ComboBox1.SetFocus;
  end;
end;

procedure TVisSelectObjectGUID.FormShow(Sender: TObject);
begin
  Timer1.Enabled := True;
end;

constructor TVisSelectObjectGUID.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fPresenter := TSelectObjectGUIDPresenter.Create(Self);
  Timer1.Enabled := False;
end;

destructor TVisSelectObjectGUID.Destroy;
begin
  FreeAndNil(fPresenter);

  inherited Destroy;
end;

procedure TVisSelectObjectGUID.SetMultiSelect(AMultiSelect: Boolean);
var
  SelectionOptions: TVTSelectionOptions;
begin
  SelectionOptions := TisGrid1.TreeOptions.SelectionOptions;
  if AMultiSelect then
    Include(SelectionOptions, toMultiSelect)
  else
    Exclude(SelectionOptions, toMultiSelect);
  TisGrid1.TreeOptions.SelectionOptions := SelectionOptions;
end;

procedure TVisSelectObjectGUID.SetAllowedType(AAllowedType: TRawUtf8DynArray);
begin
  fPresenter.AllowedGUIDType := AAllowedType;
end;

procedure TVisSelectObjectGUID.SetLdapClient(ALdapClient: TLdapClient);
begin
  fPresenter.SetLdapClient(ALdapClient);
end;

function TVisSelectObjectGUID.GetFilter: RawUtf8;
begin
  result := ComboBox1.Caption;
end;

function TVisSelectObjectGUID.GetSelectedGUID: RawUtf8;
begin
  result := TisGrid1.FocusedRow^.S['guid'];
end;

function TVisSelectObjectGUID.GetSelectedName: RawUtf8;
begin
  result := TisGrid1.FocusedRow^.S['name'];
end;

procedure TVisSelectObjectGUID.LoadData(AData: PDocVariantData);
begin
  TisGrid1.LoadData(AData);
end;

procedure TVisSelectObjectGUID.ShowLdapError(ALdapErrorString: RawUtf8; ALdapError: TLdapError);
begin
  ShowMessage(ALdapErrorString);
end;

end.

