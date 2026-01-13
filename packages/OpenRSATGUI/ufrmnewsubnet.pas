unit ufrmnewsubnet;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  StdCtrls,
  ActnList, ExtCtrls,
  tis.ui.grid.core,
  mormot.core.base,
  mormot.net.ldap,
  ursatldapclient;

type

  { TFrmNewSubnet }

  TFrmNewSubnet = class(TFrame)
    Action_ok: TAction;
    ActionList1: TActionList;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Timer_SearchInGrid: TTimer;
    TisGrid1: TTisGrid;
    procedure Action_okExecute(Sender: TObject);
    procedure Action_okUpdate(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Label2Click(Sender: TObject);
    procedure Label2MouseEnter(Sender: TObject);
    procedure Label2MouseLeave(Sender: TObject);
    procedure Timer_SearchInGridTimer(Sender: TObject);
    procedure TisGrid1KeyPress(Sender: TObject; var Key: char);
    procedure TisGrid1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    fLdap: TRsatLdapClient;

    ValidPrefix: Boolean;

    fSearchWord: RawUtf8;

    procedure Load;
  public
    constructor Create(TheOwner: TComponent; ALdap: TRsatLdapClient); reintroduce;
  end;

implementation
uses
  lclintf,
  StrUtils,
  mormot.net.sock,
  mormot.core.variants,
  ucommon,
  ucommonui,
  ucoredatamodule,
  ursatldapclientui,
  uvisnewobject;

{$R *.lfm}


function ExpandIPv6(ShortIPv6: String): String;
var
  IPv6Parts: TStringArray;
  SubPos, MissingParts, j, i: Integer;
begin
  IPv6Parts := ShortIPv6.Split(':');

  // Find substitution
  SubPos := -1;
  for i := 0 to High(IPv6Parts) do
  begin
    if IPv6Parts[i] = '' then
    begin
      SubPos := i;
      break;
    end;
  end;

  // Add missing parts
  MissingParts := 7 - Length(IPv6Parts);
  for i := 0 to MissingParts do
    Insert('', IPv6Parts, SubPos);

  // Fill missing zero
  for i := 0 to High(IPv6Parts) do
  begin
    for j := 0 to 3 - Length(IPv6Parts[i]) do
      Insert('0', IPv6Parts[i], 0);
  end;

  result := String.Join(':', IPv6Parts);
end;

{ TFrmNewSubnet }

procedure TFrmNewSubnet.Action_okUpdate(Sender: TObject);
begin
  Action_ok.Enabled := ValidPrefix;
end;

procedure TFrmNewSubnet.Edit1Change(Sender: TObject);
var
  ipaddr: TNetAddr;
  ipres: TNetResult;
  Values, IPBlocs: TStringArray;
  IPaddress, IPLength: String;
  Bytes: Array[0..15] of Byte;
  i, bitPos, BitMask: Integer;
  Value, PrefixMask: Longint;
  IPv4Address: Integer;
begin
  ValidPrefix := False;

  Values := String(Edit1.Text).Split('/');

  if Length(Values) <> 2 then
    Exit;
  IPaddress := Values[0];
  IPLength := Values[1];

  if not TryStrToInt(IPLength, PrefixMask) or (PrefixMask < 0) or (PrefixMask > 128) then
    Exit;

  ipres := ipaddr.SetFrom(IPaddress, '0', nlTcp);
  if not (ipres = nrOK) then
    Exit;

  // Check length validity
  if ipaddr.Family = nfIP4 then
  begin
    Values := IPaddress.Split('.');
    for i := 0 to 3 do
    begin
      if not (TryStrToInt(Values[i], Value) and (Value >= 0) and (Value <= 255)) then
        Exit;
      Bytes[i] := Value;
    end;
    IPv4Address := (Integer(Bytes[0]) shl 24) or (Integer(Bytes[1]) shl 16) or (Integer(Bytes[2]) shl 8) or Integer(Bytes[3]);
    ValidPrefix := (PrefixMask = 32) or (IPv4Address and ((1 shl (32 - PrefixMask)) - 1) = 0);
  end
  else if ipaddr.Family = nfIP6 then
  begin
    IPaddress := ExpandIPv6(IPaddress);
    IPBlocs := IPaddress.Split(':');
    for i := 0 to High(IPBlocs) do
    begin
      Value := Hex2Dec(IPBlocs[i]);
      Bytes[i * 2] := (Value shr 8) and $ff;
      Bytes[i * 2 + 1] := Value and $ff;
    end;
    ValidPrefix := True;
    bitPos := 0;
    for i := 0 to 15 do
    begin
      if bitPos + 8 <= PrefixMask then
      begin
        bitPos := bitPos + 8;
        continue;
      end;

      if bitPos >= PrefixMask then
      begin
        if Bytes[i] <> 0 then
        begin
          ValidPrefix := False;
          Break;
        end;
      end
      else
      begin
        BitMask := PrefixMask - bitPos;
        if (Bytes[i] and ((1 shl (8 - BitMask)) - 1)) <> 0 then
        begin
          ValidPrefix := False;
          Break;
        end;
      end;
    bitPos := bitPos + 8;
    end;
  end;
  if ValidPrefix then
    Edit2.Text := String(Edit1.Text).ToLower
  else
    Edit2.Text := '';
end;

procedure TFrmNewSubnet.Label2Click(Sender: TObject);
begin
  OpenURL('https://learn.microsoft.com/previous-versions/windows/it-pro/windows-server-2008-R2-and-2008/cc730868(v=ws.11)');
end;

procedure TFrmNewSubnet.Label2MouseEnter(Sender: TObject);
begin
  Screen.Cursor := crHandPoint;
end;

procedure TFrmNewSubnet.Label2MouseLeave(Sender: TObject);
begin
  Screen.Cursor := crDefault;
end;

procedure TFrmNewSubnet.Timer_SearchInGridTimer(Sender: TObject);
begin
  Timer_SearchInGrid.Enabled := False;
end;

procedure TFrmNewSubnet.TisGrid1KeyPress(Sender: TObject; var Key: char);
begin
  SearchInGrid(Timer_SearchInGrid, TisGrid1, fSearchWord, Key);
end;

procedure TFrmNewSubnet.TisGrid1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not Assigned(TisGrid1.GetNodeAt(X, Y)) then
  begin
    TisGrid1.ClearSelection;
    TisGrid1.FocusedNode := nil;
  end;
end;

procedure TFrmNewSubnet.Load;
var
  SearchResult: TLdapResult;
  NewSiteName: TDocVariantData;
begin
  NewSiteName.Init();
  fLdap.SearchBegin();
  TisGrid1.Clear;
  TisGrid1.BeginUpdate;
  try
    fLdap.SearchScope := lssWholeSubtree;

    repeat
      if not fLdap.Search(Format('CN=Sites,%s', [fLdap.ConfigDN]), False, '(objectClass=site)', ['name', 'distinguishedName']) then
        Exit;

      for SearchResult in fLdap.SearchResult.Items do
      begin
        if not Assigned(SearchResult) then
          continue;

        NewSiteName.AddValue('siteName', SearchResult.Find('name').GetReadable());
        NewSiteName.AddValue('distinguishedName', SearchResult.Find('distinguishedName').GetReadable());
        TisGrid1.Data.AddItem(NewSiteName);
        NewSiteName.Clear;
      end;
    until fLdap.SearchCookie = '';
  finally
    TisGrid1.EndUpdate;
    fLdap.SearchEnd;
    TisGrid1.LoadData;
    TisGrid1.ClearSelection;
    TisGrid1.FocusedNode := nil;
  end;
end;

procedure TFrmNewSubnet.Action_okExecute(Sender: TObject);
var
  DistinguishedName: String;
  AttributeList: TLdapAttributeList;
  Attribute: TLdapAttribute;
  SelectedData: PDocVariantData;
begin
  DistinguishedName := Format('CN=%s,CN=Subnets,CN=Sites,%s', [LdapEscape(Edit2.Text), fLdap.ConfigDN]);
  AttributeList := TLdapAttributeList.Create;

  try
    Attribute := AttributeList.Add('objectClass', 'top');
    Attribute.Add('subnet');

    SelectedData := TisGrid1.GetNodeAsPDocVariantData(TisGrid1.GetFirstSelected);
    if Assigned(SelectedData) then
    begin
      AttributeList.Add('siteObject', SelectedData^.S['distinguishedName']);
    end;
    if not fLdap.Add(DistinguishedName, AttributeList) then
      Exit;
  finally
    FreeAndNil(AttributeList);
  end;
end;

constructor TFrmNewSubnet.Create(TheOwner: TComponent; ALdap: TRsatLdapClient);
var
  OwnerNewObject: TVisNewObject absolute TheOwner;
begin
  inherited Create(TheOwner);

  fLdap := ALdap;

  OwnerNewObject.Caption := rsNewObjectSubnet;
  OwnerNewObject.Btn_Next.Action := ActionList1.ActionByName('Action_ok');
  OwnerNewObject.Btn_Next.Caption := rsNewObjectBtnOK;
  OwnerNewObject.Btn_Next.ModalResult := mrOK;
  OwnerNewObject.Btn_Next.Default := True;
  OwnerNewObject.Btn_Back.Visible := False;
  OwnerNewObject.Image_Object.ImageIndex := Ord(ileADUnknown);
  OwnerNewObject.CallBack := @Load;
end;

end.

