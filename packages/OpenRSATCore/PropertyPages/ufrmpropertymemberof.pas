unit ufrmpropertymemberof;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  StdCtrls,
  ExtCtrls,
  ActnList,
  buttons,
  mormot.core.base,
  mormot.core.log,
  mormot.core.variants,
  mormot.net.ldap,
  tis.ui.grid.core,
  ursatldapclient,
  uproperty,
  upropertyframe;

type

  { TFrmPropertyMemberOf }

  TFrmPropertyMemberOf = class(TPropertyFrame)
    Action_Add: TAction;
    Action_Delete: TAction;
    Action_PrimaryGroup: TAction;
    ActionList1: TActionList;
    Btn_Add: TBitBtn;
    Btn_DefinePrimaryGroup: TBitBtn;
    Btn_Delete: TBitBtn;
    Edit_PrimaryGroup: TEdit;
    Label_mof_noPrimaryGroup: TLabel;
    Label_PrimaryGroupKey: TLabel;
    Label_mof_title: TLabel;
    Line_mof: TShape;
    Grid_MemberOf: TTisGrid;
    Panel_Buttons: TPanel;
    Panel_NoPrimaryGroup: TPanel;
    Panel_PrimaryGroup: TPanel;
    Label_DefinePrimaryGroup: TLabel;
  private
    fLog: TSynLog;

    procedure FillMemberOf(Attribute: TLdapAttribute);
    procedure FillPrimaryGroup(PrimaryGroupDN: RawUtf8);

    procedure AddRowFromDN(DistinguishedName: RawUtf8);

    function GetPrimaryGroupDN(primaryGroupID: Integer; LdapClient: TRsatLdapClient
      ): RawUtf8;
    function GetPrimaryGroupName(PrimaryGroupDN: RawUtf8): RawUtf8;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Update(Props: TProperty); override;

  end;

implementation
uses
  mormot.core.os.security,
  mormot.core.text;

{$R *.lfm}

{ TFrmPropertyMemberOf }

constructor TFrmPropertyMemberOf.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);
end;

procedure TFrmPropertyMemberOf.FillMemberOf(Attribute: TLdapAttribute);
var
  i: Integer;
begin
  if not Assigned(Attribute) then
    Exit;

  Grid_MemberOf.BeginUpdate();
  try
    for i := 0 to Attribute.Count - 1 do
      AddRowFromDN(Attribute.GetReadable(i));
  finally
    Grid_MemberOf.LoadData();
    Grid_MemberOf.EndUpdate();
  end;
end;

procedure TFrmPropertyMemberOf.FillPrimaryGroup(PrimaryGroupDN: RawUtf8);
var
  HasPrimaryGroup: Boolean;
begin
  HasPrimaryGroup := (PrimaryGroupDN <> '');
  Panel_PrimaryGroup.Enabled := HasPrimaryGroup;
  Panel_NoPrimaryGroup.Visible := not HasPrimaryGroup;
  Panel_NoPrimaryGroup.Enabled := not HasPrimaryGroup;

  Edit_PrimaryGroup.Text := GetPrimaryGroupName(PrimaryGroupDN);

  if HasPrimaryGroup then
    AddRowFromDN(PrimaryGroupDN);
end;

procedure TFrmPropertyMemberOf.AddRowFromDN(DistinguishedName: RawUtf8);
var
  Row: TDocVariantData;
  CanonicalName: RawUtf8;
  CanonicalNameSplitted: TStringArray;
  Count: Integer;
begin
  Row.Init();

  CanonicalName := DNToCN(DistinguishedName);
  CanonicalNameSplitted := String(CanonicalName).Split('/');

  Count := Length(CanonicalNameSplitted);
  Row.S['name'] := CanonicalNameSplitted[Count - 1];
  Delete(CanonicalNameSplitted, Count - 1, 1);
  Row.S['ADSF'] := String.Join('/', CanonicalNameSplitted);
  Row.S['distinguishedName'] := DistinguishedName;
  Grid_MemberOf.Data.AddItem(Row);
end;

function TFrmPropertyMemberOf.GetPrimaryGroupDN(primaryGroupID: Integer; LdapClient: TRsatLdapClient): RawUtf8;
var
  DomainSID, Filter: RawUtf8;
  P: PUtf8Char;
  SID: TSid;
  PrimaryGroupAttribute: TLdapAttribute;
begin
  result := '';

  // Fast exit
  if (primaryGroupID = -1) then
    Exit;

  // Create SID
  DomainSID := LdapClient.DomainSid;
  P := @DomainSID[1];
  TextToSid(P, SID);
  Inc(SID.SubAuthorityCount);
  SID.SubAuthority[SID.SubAuthorityCount - 1] := primaryGroupID;

  // get primary group distinguished name
  Filter := FormatUtf8('(objectSid=%)', [LdapEscape(SidToText(@SID))]);
  PrimaryGroupAttribute := LdapClient.SearchObject(LdapClient.DefaultDN(), Filter, 'distinguishedName', lssWholeSubtree);
  if not Assigned(PrimaryGroupAttribute) then
  begin
    ShowLdapSearchError(LdapClient);
    Exit;
  end;
  result := PrimaryGroupAttribute.GetReadable();
end;

function TFrmPropertyMemberOf.GetPrimaryGroupName(PrimaryGroupDN: RawUtf8): RawUtf8;
var
  Splitted, SplittedName: TStringArray;
begin
  Result := '';

  Splitted := String(PrimaryGroupDN).Split(',');
  if not Assigned(Splitted) or (Length(Splitted) <= 0) then
    Exit;

  SplittedName := Splitted[0].Split('=');
  if not Assigned(SplittedName) or (Length(SplittedName) <> 2) then
    Exit;
  Result := SplittedName[1];
end;

procedure TFrmPropertyMemberOf.Update(Props: TProperty);
begin
  FillMemberOf(Props.Attributes.Find('memberOf'));
  FillPrimaryGroup(GetPrimaryGroupDN(Utf8ToInteger(Props.Attributes.GetByName('primaryGroupID'), -1), Props.LdapClient));
end;

end.

