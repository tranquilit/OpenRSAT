unit uvislistgpo;

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
  uopenrsatuicontextinterface,
  ursatldapclient,
  tis.ui.grid.core,
  mormot.core.base,
  mormot.core.text,
  mormot.core.variants,
  mormot.net.ldap, VirtualTrees;

type

  { TVisListGPO }

  TVisListGPO = class(TForm)
    Action_Search: TAction;
    ActionList1: TActionList;
    BitBtn_OK: TBitBtn;
    BitBtn_Cancel: TBitBtn;
    BitBtn_Search: TBitBtn;
    Edit_Search: TEdit;
    Panel_Top: TPanel;
    Panel_Bottom: TPanel;
    Timer_Search: TTimer;
    Timer_SearchInGrid: TTimer;
    TisGrid1: TTisGrid;
    procedure Action_SearchExecute(Sender: TObject);
    procedure Edit_SearchChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer_SearchTimer(Sender: TObject);
  private
    fContext: IOpenRSATUIContext;

    function GetSelected: TRawUtf8DynArray;
    procedure RefreshGPOList;
  public
    constructor Create(TheOwner: TComponent; IContext: IOpenRSATUIContext); reintroduce;
    destructor Destroy; override;

    property Selected: TRawUtf8DynArray read GetSelected;
  end;

implementation

{$R *.lfm}

{ TVisListGPO }

procedure TVisListGPO.FormShow(Sender: TObject);
begin
  Edit_Search.SetFocus;
  RefreshGPOList;
end;

procedure TVisListGPO.Timer_SearchTimer(Sender: TObject);
begin
  Action_Search.Execute;
  Timer_Search.Enabled := False;
end;

procedure TVisListGPO.Action_SearchExecute(Sender: TObject);
begin
  RefreshGPOList;
end;

procedure TVisListGPO.Edit_SearchChange(Sender: TObject);
begin
  Timer_Search.Enabled := False;
  Timer_Search.Enabled := True;
end;

procedure TVisListGPO.RefreshGPOList;
var
  LdapClient: TRsatLdapClient;
  DV: TDocVariantData;
  N, Filter: RawUtf8;
  C: TCursor;
begin
  TisGrid1.Clear;
  try
    C := Cursor;
    Cursor := crHourGlass;
    LdapClient := fContext.GetRSAT.LdapClient;
    LdapClient.SearchScope := lssWholeSubtree;
    if Edit_Search.Text <> '' then
      Filter := FormatUtf8('(&(objectClass=groupPolicyContainer)(|(displayName=%*)(displayName=*%*)(name=*%*)))', [LdapEscape(Edit_Search.Text), LdapEscape(Edit_Search.Text), LdapEscape(Edit_Search.Text)])
    else
      Filter := '(objectClass=groupPolicyContainer)';

    if not LdapClient.SearchAllDocRaw(DV, LdapClient.DefaultDN, Filter, ['name', 'distinguishedName', 'displayName', 'flags'], [roAutoRange, roKnownValuesAsArray, roObjectNameAtRoot]) then
      Exit;

    for N in DV.Names do
    begin
      if N = '' then
        Continue;
      TisGrid1.Data.AddItem(DV.O[N]^);
    end;
    TisGrid1.LoadData();
  finally
    Cursor := C;
  end;
end;

function TVisListGPO.GetSelected: TRawUtf8DynArray;
var
  i: Integer;
  P: PDocVariantData;
  DV: TDocVariantData;
begin
  SetLength(result, TisGrid1.SelectedCount);
  DV := TisGrid1.SelectedRows;
  for i := 0 to TisGrid1.SelectedCount - 1 do
  begin
    P := DV._[i];
    if not Assigned(P) then
      Continue;
    result[i] := P^.U['distinguishedName'];
  end;
end;

constructor TVisListGPO.Create(TheOwner: TComponent;
  IContext: IOpenRSATUIContext);
begin
  inherited Create(TheOwner);

  fContext := IContext;
end;

destructor TVisListGPO.Destroy;
begin
  fContext := nil;

  inherited Destroy;
end;

end.

