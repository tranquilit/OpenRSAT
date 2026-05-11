unit fixture.fakevisadvancedsecurity;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.base,
  mormot.core.os.security,
  mormot.core.variants,
  fixture.basefakevis,
  uadvancedsecuritypresenter;

type

  { TFakeVisAdvancedSecurity }

  TFakeVisAdvancedSecurity = class(TFakeForm, IAdvancedSecurityView)
  public
    Title: RawUtf8;
    SetTitleCount: Integer;

    OwnerText: RawUtf8;
    SetOwnerCount: Integer;
    GroupText: RawUtf8;
    SetGroupCount: Integer;
    ACEGrid: TDocVariantData;
    RefreshACEGridCount: Integer;

    RightPanelType: Boolean;
    SetRightPanelTypeCount: Integer;
    RightPanelAccount: RawUtf8;
    SetRightPanelAccountCount: Integer;
    RightPanelMask: TSecAccessMask;
    SetRightPanelMaskCount: Integer;
    RightPanelFlags: TSecAceFlags;
    SetRightPanelFlagsCount: Integer;
    RightPanelObject: RawUtf8;
    SetRightPanelObjectCount: Integer;
    RightPanelInheritedObject: RawUtf8;
    SetRightPanelInheritedObjectCount: Integer;

    RefreshACEGridIndexIndex: Integer;
    RefreshACEGridIndexCount: Integer;

    PickOwnerSID: RawUtf8;
    PickOwnerName: RawUtf8;

    constructor Create;
    procedure CleanUp;
  public
    procedure SetTitle(const aName: RawUtf8);
    procedure SetOwnerText(const aName: RawUtf8);
    procedure SetGroupText(const aName: RawUtf8);
    procedure RefreshACEGrid(aACEs: PDocVariantData);
    procedure RefreshACEGridIndex(aACE: PDocVariantData; aIndex: Integer);
    procedure SelectACE(aIndex: Integer);

    procedure SetRightPanelType(aIsAllow: Boolean);
    procedure SetRightPanelAccount(const aName: RawUtf8);
    procedure SetRightPanelMask(aMask: TSecAccessMask);
    procedure SetRightPanelFlags(aFlags: TSecAceFlags);
    procedure SetRightPanelObject(const aGUIDName: RawUtf8);
    procedure SetRightPanelInheritedObject(const aGUIDName: RawUtf8);

    procedure SetApplyEnabled(aEnabled: Boolean);
    procedure SetDeleteEnabled(aEnabled: Boolean);
    procedure SetDuplicateEnabled(aEnabled: Boolean);

    function  GetCurrentMask: TSecAccessMask;
    function  GetCurrentFlags: TSecAceFlags;
    function  GetCurrentType: Boolean;
    function  GetCurrentPrincipalText: RawUtf8;
    function  GetCurrentObjectText: RawUtf8;
    function  GetCurrentInheritedObjectText: RawUtf8;

    function  PickPrincipal(out aSid, aName: RawUtf8): Boolean;
    function  PickOwner(out aSid, aName: RawUtf8): Boolean;

    procedure ShowError(const aMsg: RawUtf8);
    procedure CloseRequest;
  end;

implementation

{ TFakeVisAdvancedSecurity }

constructor TFakeVisAdvancedSecurity.Create;
begin
  CleanUp;
end;

procedure TFakeVisAdvancedSecurity.CleanUp;
begin
  Title := '';
  SetTitleCount := 0;

  OwnerText := '';
  SetOwnerCount := 0;
  GroupText := '';
  SetGroupCount := 0;
  ACEGrid.Init();
  RefreshACEGridCount := 0;

  RightPanelType := False;
  SetRightPanelTypeCount := 0;
  RightPanelAccount := '';
  SetRightPanelAccountCount := 0;
  RightPanelMask := [];
  SetRightPanelMaskCount := 0;
  RightPanelFlags := [];
  SetRightPanelFlagsCount := 0;
  RightPanelObject := '';
  SetRightPanelObjectCount := 0;
  RightPanelInheritedObject := '';
  SetRightPanelInheritedObjectCount := 0;

  RefreshACEGridIndexIndex := 0;
  RefreshACEGridIndexCount := 0;

  PickOwnerSID := '';
  PickOwnerName := '';
end;

procedure TFakeVisAdvancedSecurity.SetTitle(const aName: RawUtf8);
begin
  Title := aName;
  Inc(SetTitleCount);
end;

procedure TFakeVisAdvancedSecurity.SetOwnerText(const aName: RawUtf8);
begin
  OwnerText := aName;
  Inc(SetOwnerCount);
end;

procedure TFakeVisAdvancedSecurity.SetGroupText(const aName: RawUtf8);
begin
  GroupText := aName;
  Inc(SetGroupCount);
end;

procedure TFakeVisAdvancedSecurity.RefreshACEGrid(aACEs: PDocVariantData);
begin
  ACEGrid.InitFrom(aACEs^, True);
  Inc(RefreshACEGridCount);
end;

procedure TFakeVisAdvancedSecurity.RefreshACEGridIndex(aACE: PDocVariantData; aIndex: Integer);
begin
  RefreshACEGridIndexIndex := aIndex;
  Inc(RefreshACEGridIndexCount);
end;

procedure TFakeVisAdvancedSecurity.SelectACE(aIndex: Integer);
begin

end;

procedure TFakeVisAdvancedSecurity.SetRightPanelType(aIsAllow: Boolean);
begin
  RightPanelType := aIsAllow;
  Inc(SetRightPanelTypeCount);
end;

procedure TFakeVisAdvancedSecurity.SetRightPanelAccount(const aName: RawUtf8);
begin
  RightPanelAccount := aName;
  Inc(SetRightPanelAccountCount);
end;

procedure TFakeVisAdvancedSecurity.SetRightPanelMask(aMask: TSecAccessMask);
begin
  RightPanelMask := aMask;
  Inc(SetRightPanelMaskCount);
end;

procedure TFakeVisAdvancedSecurity.SetRightPanelFlags(aFlags: TSecAceFlags);
begin
  RightPanelFlags := aFlags;
  Inc(SetRightPanelFlagsCount);
end;

procedure TFakeVisAdvancedSecurity.SetRightPanelObject(const aGUIDName: RawUtf8);
begin
  RightPanelObject := aGUIDName;
  Inc(SetRightPanelObjectCount);
end;

procedure TFakeVisAdvancedSecurity.SetRightPanelInheritedObject(const aGUIDName: RawUtf8);
begin
  RightPanelInheritedObject := aGUIDName;
  Inc(SetRightPanelInheritedObjectCount);
end;

procedure TFakeVisAdvancedSecurity.SetApplyEnabled(aEnabled: Boolean);
begin

end;

procedure TFakeVisAdvancedSecurity.SetDeleteEnabled(aEnabled: Boolean);
begin

end;

procedure TFakeVisAdvancedSecurity.SetDuplicateEnabled(aEnabled: Boolean);
begin

end;

function TFakeVisAdvancedSecurity.GetCurrentMask: TSecAccessMask;
begin
  result := RightPanelMask;
end;

function TFakeVisAdvancedSecurity.GetCurrentFlags: TSecAceFlags;
begin
  result := RightPanelFlags;
end;

function TFakeVisAdvancedSecurity.GetCurrentType: Boolean;
begin
  result := RightPanelType;
end;

function TFakeVisAdvancedSecurity.GetCurrentPrincipalText: RawUtf8;
begin
  result := RightPanelAccount;
end;

function TFakeVisAdvancedSecurity.GetCurrentObjectText: RawUtf8;
begin
  result := RightPanelObject;
end;

function TFakeVisAdvancedSecurity.GetCurrentInheritedObjectText: RawUtf8;
begin
  result := RightPanelInheritedObject;
end;

function TFakeVisAdvancedSecurity.PickPrincipal(out aSid, aName: RawUtf8): Boolean;
begin

end;

function TFakeVisAdvancedSecurity.PickOwner(out aSid, aName: RawUtf8): Boolean;
begin
  aSid := PickOwnerSID;
  aName := PickOwnerName;
end;

procedure TFakeVisAdvancedSecurity.ShowError(const aMsg: RawUtf8);
begin

end;

procedure TFakeVisAdvancedSecurity.CloseRequest;
begin

end;

end.

