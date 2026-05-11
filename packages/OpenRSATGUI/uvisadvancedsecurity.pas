unit uvisadvancedsecurity;

{$mode ObjFPC}{$H+}

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
  mormot.core.os.security,
  mormot.core.variants,
  VirtualTrees,
  mormot.core.base,
  mormot.core.text,
  mormot.net.ldap,
  ucoredatamodule,
  uadvancedsecuritypresenter,
  ucommon;

{ TODO:
  Add column "Inherited from" with the base object of the ACE.
  Add "Restore default" button to reset the default ACL.
  View ACL flags.
  Show warning if owner / group is not "Domain Admins".
  Add button to easely set "Domain Admins" to owner / group.
  Update column Inheritance flags text with "This object", "This object and all descendants", ...
}

type

  TVisAdvancedSecurityException = class(Exception);

  { TVisAdvancedSecurity }

  TVisAdvancedSecurity = class(TForm, IAdvancedSecurityView)
    Action_SelectPrincipal: TAction;
    Action_AddACE: TAction;
    Action_SelectOwner: TAction;
    Action_Apply: TAction;
    Action_Cancel: TAction;
    Action_DeleteACE: TAction;
    Action_DuplicateACE: TAction;
    Action_OK: TAction;
    Action_SelectGroup: TAction;
    Action_SelectInheritedObject: TAction;
    Action_SelectObject: TAction;
    ActionList1: TActionList;
    BitBtn_Apply: TBitBtn;
    BitBtn_Cancel: TBitBtn;
    BitBtn_OK: TBitBtn;
    BitBtn_Group: TBitBtn;
    BitBtn_Owner: TBitBtn;
    BitBtn_Principal: TBitBtn;
    BitBtn_Add: TBitBtn;
    BitBtn_Duplicate: TBitBtn;
    BitBtn_Delete: TBitBtn;
    BitBtn_Object: TBitBtn;
    BitBtn_ObjectInheritance: TBitBtn;
    CheckBoxCC: TCheckBox;
    CheckBoxD: TCheckBox;
    CheckBoxRC: TCheckBox;
    CheckBoxWO: TCheckBox;
    CheckBoxWDAC: TCheckBox;
    CheckBoxOI: TCheckBox;
    CheckBoxCI: TCheckBox;
    CheckBoxIO: TCheckBox;
    CheckBoxNP: TCheckBox;
    CheckBoxDC: TCheckBox;
    CheckBoxLC: TCheckBox;
    CheckBoxSW: TCheckBox;
    CheckBoxRP: TCheckBox;
    CheckBoxWP: TCheckBox;
    CheckBoxDT: TCheckBox;
    CheckBoxLO: TCheckBox;
    CheckBoxCA: TCheckBox;
    ComboBox_Type: TComboBox;
    Edit_InheritedObject: TEdit;
    Edit_Object: TEdit;
    Edit_Group: TEdit;
    Edit_Owner: TEdit;
    Edit_Principal: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label_Group: TLabel;
    Label_Owner: TLabel;
    Label_Type: TLabel;
    Label_Principal: TLabel;
    Label_Object: TLabel;
    Label_ObjectInheritance: TLabel;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel14: TPanel;
    Panel15: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    ScrollBox1: TScrollBox;
    Splitter1: TSplitter;
    TisGrid1: TTisGrid;
    procedure Action_AddACEExecute(Sender: TObject);
    procedure Action_ApplyExecute(Sender: TObject);
    procedure Action_CancelExecute(Sender: TObject);
    procedure Action_DeleteACEExecute(Sender: TObject);
    procedure Action_DuplicateACEExecute(Sender: TObject);
    procedure Action_OKExecute(Sender: TObject);
    procedure Action_SelectInheritedObjectExecute(Sender: TObject);
    procedure Action_SelectObjectExecute(Sender: TObject);
    procedure Action_SelectOwnerExecute(Sender: TObject);
    procedure Action_SelectPrincipalExecute(Sender: TObject);
    procedure CheckBoxCAChange(Sender: TObject);
    procedure CheckBoxCCChange(Sender: TObject);
    procedure CheckBoxCIChange(Sender: TObject);
    procedure CheckBoxDCChange(Sender: TObject);
    procedure CheckBoxDChange(Sender: TObject);
    procedure CheckBoxDTChange(Sender: TObject);
    procedure CheckBoxIOChange(Sender: TObject);
    procedure CheckBoxLCChange(Sender: TObject);
    procedure CheckBoxLOChange(Sender: TObject);
    procedure CheckBoxNPChange(Sender: TObject);
    procedure CheckBoxOIChange(Sender: TObject);
    procedure CheckBoxRCChange(Sender: TObject);
    procedure CheckBoxRPChange(Sender: TObject);
    procedure CheckBoxSWChange(Sender: TObject);
    procedure CheckBoxWDACChange(Sender: TObject);
    procedure CheckBoxWOChange(Sender: TObject);
    procedure CheckBoxWPChange(Sender: TObject);
    procedure ComboBox_ObjectChange(Sender: TObject);
    procedure ComboBox_ObjectInheritanceChange(Sender: TObject);
    procedure ComboBox_TypeChange(Sender: TObject);
    procedure Edit_InheritedObjectChange(Sender: TObject);
    procedure Edit_ObjectChange(Sender: TObject);
    procedure Edit_PrincipalChange(Sender: TObject);
    procedure TisGrid1DrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; const CellText: String; const CellRect: TRect; var DefaultDraw: Boolean);
    procedure TisGrid1FocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure TisGrid1GetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure TisGrid1GetText(aSender: TBaseVirtualTree; aNode: PVirtualNode;
      const aCell: TDocVariantData; aColumn: TColumnIndex;
      aTextType: TVSTTextType; var aText: string);
  private
    fPresenter: TAdvancedSecurityPresenter;

    function SelectObjectSid(out aSid, aName: RawUtf8): Boolean;
    function SelectObjectGUID(out aGUID, aName: RawUtf8; aAllowedType: TRawUtf8DynArray): Boolean;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetLdapClient(aClient: TLdapClient);
    procedure SetDistinguishedName(const aDN: RawUtf8);
    procedure SetObjectName(const aName: RawUtf8);
    procedure SetSecurityDescriptor(const aSD: TSecurityDescriptor);
    function GetSecurityDescriptor: TSecurityDescriptor;
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
    function PickObject(out aGUID, aName: RawUtf8; aAllowedtype: TRawUtf8DynArray): Boolean;

    procedure ShowError(const aMsg: RawUtf8);
    procedure CloseRequest;
  end;

implementation
uses
  uvisselectobjectguid,
  uvisselectobjectsid;

{$R *.lfm}

{ TVisAdvancedSecurity }

procedure TVisAdvancedSecurity.Action_AddACEExecute(Sender: TObject);
begin
  fPresenter.ActionAddACE;
end;

procedure TVisAdvancedSecurity.Action_ApplyExecute(Sender: TObject);
begin
  fPresenter.ActionApply;
end;

procedure TVisAdvancedSecurity.Action_CancelExecute(Sender: TObject);
begin
  fPresenter.ActionCancel;
end;

procedure TVisAdvancedSecurity.Action_DeleteACEExecute(Sender: TObject);
begin
  fPresenter.ActionDeleteACE;
end;

procedure TVisAdvancedSecurity.Action_DuplicateACEExecute(Sender: TObject);
begin
  fPresenter.ActionDuplicateACE;
end;

procedure TVisAdvancedSecurity.Action_OKExecute(Sender: TObject);
begin
  fPresenter.ActionOK;
end;

procedure TVisAdvancedSecurity.Action_SelectInheritedObjectExecute(Sender: TObject);
begin
  fPresenter.ActionSelectInheritedObject;
end;

procedure TVisAdvancedSecurity.Action_SelectObjectExecute(Sender: TObject);
begin
  fPresenter.ActionSelectObject;
end;

procedure TVisAdvancedSecurity.Action_SelectOwnerExecute(Sender: TObject);
begin
  fPresenter.ActionSelectOwner;
end;

procedure TVisAdvancedSecurity.Action_SelectPrincipalExecute(Sender: TObject);
begin
  fPresenter.ActionSelectPrincipal;
end;

procedure TVisAdvancedSecurity.CheckBoxCAChange(Sender: TObject);
begin
  fPresenter.DoRightPanelMaskChanged;
end;

procedure TVisAdvancedSecurity.CheckBoxCCChange(Sender: TObject);
begin
  fPresenter.DoRightPanelMaskChanged;
end;

procedure TVisAdvancedSecurity.CheckBoxCIChange(Sender: TObject);
begin
  fPresenter.DoRightPanelFlagsChanged;
end;

procedure TVisAdvancedSecurity.CheckBoxDCChange(Sender: TObject);
begin
  fPresenter.DoRightPanelMaskChanged;
end;

procedure TVisAdvancedSecurity.CheckBoxDChange(Sender: TObject);
begin
  fPresenter.DoRightPanelMaskChanged;
end;

procedure TVisAdvancedSecurity.CheckBoxDTChange(Sender: TObject);
begin
  fPresenter.DoRightPanelMaskChanged;
end;

procedure TVisAdvancedSecurity.CheckBoxIOChange(Sender: TObject);
begin
  fPresenter.DoRightPanelFlagsChanged;
end;

procedure TVisAdvancedSecurity.CheckBoxLCChange(Sender: TObject);
begin
  fPresenter.DoRightPanelMaskChanged;
end;

procedure TVisAdvancedSecurity.CheckBoxLOChange(Sender: TObject);
begin
  fPresenter.DoRightPanelMaskChanged;
end;

procedure TVisAdvancedSecurity.CheckBoxNPChange(Sender: TObject);
begin
  fPresenter.DoRightPanelFlagsChanged;
end;

procedure TVisAdvancedSecurity.CheckBoxOIChange(Sender: TObject);
begin
  fPresenter.DoRightPanelFlagsChanged;
end;

procedure TVisAdvancedSecurity.CheckBoxRCChange(Sender: TObject);
begin
  fPresenter.DoRightPanelMaskChanged;
end;

procedure TVisAdvancedSecurity.CheckBoxRPChange(Sender: TObject);
begin
  fPresenter.DoRightPanelMaskChanged;
end;

procedure TVisAdvancedSecurity.CheckBoxSWChange(Sender: TObject);
begin
  fPresenter.DoRightPanelMaskChanged;
end;

procedure TVisAdvancedSecurity.CheckBoxWDACChange(Sender: TObject);
begin
  fPresenter.DoRightPanelMaskChanged;
end;

procedure TVisAdvancedSecurity.CheckBoxWOChange(Sender: TObject);
begin
  fPresenter.DoRightPanelMaskChanged;
end;

procedure TVisAdvancedSecurity.CheckBoxWPChange(Sender: TObject);
begin
  fPresenter.DoRightPanelMaskChanged;
end;

procedure TVisAdvancedSecurity.ComboBox_ObjectChange(Sender: TObject);
begin
  fPresenter.DoRightPanelObjectChanged;
end;

procedure TVisAdvancedSecurity.ComboBox_ObjectInheritanceChange(Sender: TObject);
begin
  fPresenter.DoRightPanelInheritedObjectChanged;
end;

procedure TVisAdvancedSecurity.ComboBox_TypeChange(Sender: TObject);
begin
  fPresenter.DoRightPanelTypeChanged;
end;

procedure TVisAdvancedSecurity.Edit_InheritedObjectChange(Sender: TObject);
begin
  fPresenter.DoRightPanelInheritedObjectChanged;
end;

procedure TVisAdvancedSecurity.Edit_ObjectChange(Sender: TObject);
begin
  fPresenter.DoRightPanelObjectChanged;
end;

procedure TVisAdvancedSecurity.Edit_PrincipalChange(Sender: TObject);
begin
  fPresenter.DoRightPanelPrincipalChanged;
end;

procedure TVisAdvancedSecurity.TisGrid1DrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; const CellText: String; const CellRect: TRect; var DefaultDraw: Boolean);
var
  NodeData: PDocVariantData;
begin
  NodeData := TisGrid1.GetNodeAsPDocVariantData(Node);
  if not Assigned(NodeData) or not NodeData^.Exists('state') then
    Exit;

  case NodeData^.I['state'] of
    1: TargetCanvas.Font.Bold := True;
    2: TargetCanvas.Font.Italic := True;
    3: TargetCanvas.Font.StrikeThrough := True;
  end;
end;

procedure TVisAdvancedSecurity.TisGrid1FocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
var
  NodeData: PDocVariantData;
begin
  NodeData := TisGrid1.GetNodeAsPDocVariantData(Node);
  if not Assigned(NodeData) then
    Exit;

  fPresenter.DoACESelectionChanged(NodeData^.I['index']);
end;

procedure TVisAdvancedSecurity.TisGrid1GetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
var
  NodeData: PDocVariantData;
begin
  if TisGrid1.FindColumnByIndex(Column).PropertyName = 'type' then
  begin
    NodeData := TisGrid1.GetNodeAsPDocVariantData(Node);
    if not Assigned(NodeData) or not NodeData^.Exists('type') then
      Exit;
    if NodeData^.B['type'] then
      ImageIndex := 28
    else
      ImageIndex := 49;
  end;
end;

procedure TVisAdvancedSecurity.TisGrid1GetText(aSender: TBaseVirtualTree; aNode: PVirtualNode;
  const aCell: TDocVariantData; aColumn: TColumnIndex; aTextType: TVSTTextType; var aText: string);
begin
  if TisGrid1.FindColumnByIndex(aColumn).PropertyName = 'type' then
    aText := '';
end;

function TVisAdvancedSecurity.SelectObjectSid(out aSid, aName: RawUtf8): Boolean;
var
  Vis: TVisSelectObjectSID;
begin
  result := False;

  Vis := TVisSelectObjectSID.Create(Self);
  try
    Vis.MultiSelect := False;
    Vis.LdapClient := fPresenter.LdapClient;
    if Vis.ShowModal <> mrOK then
      Exit;
    result := True;
    aName := Vis.SelectedName;
    aSid := Vis.SelectedSid;
  finally
    FreeAndNil(Vis);
  end;
end;

function TVisAdvancedSecurity.SelectObjectGUID(out aGUID, aName: RawUtf8; aAllowedType: TRawUtf8DynArray): Boolean;
var
  Vis: TVisSelectObjectGUID;
begin
  result := False;

  Vis := TVisSelectObjectGUID.Create(Self);
  try
    Vis.SetMultiSelect(False);
    Vis.SetLdapClient(fPresenter.LdapClient);
    Vis.SetAllowedType(aAllowedType);
    if Vis.ShowModal <> mrOK then
      Exit;
    result := True;
    aName := Vis.GetSelectedName;
    aGUID := Vis.GetSelectedGUID;
  finally
    FreeAndNil(Vis);
  end;
end;

constructor TVisAdvancedSecurity.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fPresenter := TAdvancedSecurityPresenter.Create(Self);
  ComboBox_Type.Items.AddStrings([rsAllow, rsDeny]);
end;

destructor TVisAdvancedSecurity.Destroy;
begin
  FreeAndNil(fPresenter);

  inherited Destroy;
end;

procedure TVisAdvancedSecurity.SetLdapClient(aClient: TLdapClient);
begin
  fPresenter.SetLdapClient(aClient);
end;

procedure TVisAdvancedSecurity.SetDistinguishedName(const aDN: RawUtf8);
begin
  fPresenter.SetDistinguishedName(aDN);
end;

procedure TVisAdvancedSecurity.SetObjectName(const aName: RawUtf8);
begin
  fPresenter.SetName(aName);
end;

procedure TVisAdvancedSecurity.SetSecurityDescriptor(const aSD: TSecurityDescriptor);
begin
  fPresenter.SetSecurityDescriptor(aSD);
end;

function TVisAdvancedSecurity.GetSecurityDescriptor: TSecurityDescriptor;
begin
  result := fPresenter.GetSecurityDescriptor;
end;

procedure TVisAdvancedSecurity.SetTitle(const aName: RawUtf8);
begin
  Caption := FormatUtf8(rsVisAdvancedSecurityTitle, [aName]);
end;

procedure TVisAdvancedSecurity.SetOwnerText(const aName: RawUtf8);
begin
  Edit_Owner.Caption := aName;
end;

procedure TVisAdvancedSecurity.SetGroupText(const aName: RawUtf8);
begin
  Edit_Group.Caption := aName;
end;

procedure TVisAdvancedSecurity.RefreshACEGrid(aACEs: PDocVariantData);
begin
  TisGrid1.Clear;
  TisGrid1.LoadData(aACEs);
end;

procedure TVisAdvancedSecurity.RefreshACEGridIndex(aACE: PDocVariantData; aIndex: Integer);
var
  Node, CurrNode: PVirtualNode;
  NodeData: PDocVariantData;
begin
  Node := TisGrid1.GetFirst();
  while Assigned(Node) do
  begin
    CurrNode := Node;
    NodeData := TisGrid1.GetNodeAsPDocVariantData(Node);
    Node := TisGrid1.GetNext(CurrNode);
    if not Assigned(NodeData) then
      continue;

    if NodeData^.I['index'] = aIndex then
    begin
      NodeData^ := aACE^;
      TisGrid1.InvalidateNode(CurrNode);
      Break;
    end;
  end;
end;

procedure TVisAdvancedSecurity.SelectACE(aIndex: Integer);
var
  Node, CurrNode: PVirtualNode;
  NodeData: PDocVariantData;
begin
  Node := TisGrid1.GetFirst();
  while Assigned(Node) do
  begin
    CurrNode := Node;
    NodeData := TisGrid1.GetNodeAsPDocVariantData(Node);
    Node := TisGrid1.GetNext(Node);
    if not Assigned(NodeData) then
      continue;

    if NodeData^.I['index'] = aIndex then
    begin
      TisGrid1.FocusedNode := CurrNode;
      Break;
    end;
  end;
end;

procedure TVisAdvancedSecurity.SetRightPanelType(aIsAllow: Boolean);
begin
  if aIsAllow then
    ComboBox_Type.Caption := rsAllow
  else
    ComboBox_Type.Caption := rsDeny;
end;

procedure TVisAdvancedSecurity.SetRightPanelAccount(const aName: RawUtf8);
begin
  Edit_Principal.Caption := aName;
end;

procedure TVisAdvancedSecurity.SetRightPanelMask(aMask: TSecAccessMask);
begin
  CheckBoxCC.Checked := samCreateChild in aMask;
  CheckBoxDC.Checked := samDeleteChild in aMask;
  CheckBoxLC.Checked := samListChildren in aMask;
  CheckBoxSW.Checked := samSelfWrite in aMask;
  CheckBoxRP.Checked := samReadProp in aMask;
  CheckBoxWP.Checked := samWriteProp in aMask;
  CheckBoxDT.Checked := samDeleteTree in aMask;
  CheckBoxLO.Checked := samListObject in aMask;
  CheckBoxCA.Checked := samControlAccess in aMask;
  CheckBoxD.Checked := samDelete in aMask;
  CheckBoxRC.Checked := samReadControl in aMask;
  CheckBoxWDAC.Checked := samWriteDac in aMask;
  CheckBoxWO.Checked := samWriteOwner in aMask;
end;

procedure TVisAdvancedSecurity.SetRightPanelFlags(aFlags: TSecAceFlags);
begin
  CheckBoxOI.Checked := safObjectInherit in aFlags;
  CheckBoxCI.Checked := safContainerInherit in aFlags;
  CheckBoxIO.Checked := safInheritOnly in aFlags;
  CheckBoxNP.Checked := safNoPropagateInherit in aFlags;
end;

procedure TVisAdvancedSecurity.SetRightPanelObject(const aGUIDName: RawUtf8);
begin
  Edit_Object.Caption := aGUIDName;
end;

procedure TVisAdvancedSecurity.SetRightPanelInheritedObject(const aGUIDName: RawUtf8);
begin
  Edit_InheritedObject.Caption := aGUIDName;
end;

procedure TVisAdvancedSecurity.SetApplyEnabled(aEnabled: Boolean);
begin
  BitBtn_Apply.Enabled := aEnabled;
end;

procedure TVisAdvancedSecurity.SetDeleteEnabled(aEnabled: Boolean);
begin
  BitBtn_Delete.Enabled := aEnabled;
end;

procedure TVisAdvancedSecurity.SetDuplicateEnabled(aEnabled: Boolean);
begin
  BitBtn_Duplicate.Enabled := aEnabled;
end;

function TVisAdvancedSecurity.GetCurrentMask: TSecAccessMask;
begin
  result := [];

  if CheckBoxCC.Checked then
    Include(result, samCreateChild);
  if CheckBoxDC.Checked then
    Include(result, samDeleteChild);
  if CheckBoxLC.Checked then
    Include(result, samListChildren);
  if CheckBoxSW.Checked then
    Include(result, samSelfWrite);
  if CheckBoxRP.Checked then
    Include(result, samReadProp);
  if CheckBoxWP.Checked then
    Include(result, samWriteProp);
  if CheckBoxDT.Checked then
    Include(result, samDeleteTree);
  if CheckBoxLO.Checked then
    Include(result, samListObject);
  if CheckBoxCA.Checked then
    Include(result, samControlAccess);
  if CheckBoxD.Checked then
    Include(result, samDelete);
  if CheckBoxRC.Checked then
    Include(result, samReadControl);
  if CheckBoxWDAC.Checked then
    Include(result, samWriteDac);
  if CheckBoxWO.Checked then
    Include(result, samWriteOwner);
end;

function TVisAdvancedSecurity.GetCurrentFlags: TSecAceFlags;
begin
  result := [];

  if CheckBoxOI.Checked then
    Include(Result, safObjectInherit);
  if CheckBoxCI.Checked then
    Include(Result, safContainerInherit);
  if CheckBoxIO.Checked then
    Include(Result, safInheritOnly);
  if CheckBoxNP.Checked then
    Include(Result, safNoPropagateInherit);
end;

function TVisAdvancedSecurity.GetCurrentType: Boolean;
begin
  result := (ComboBox_Type.Caption = rsAllow);
end;

function TVisAdvancedSecurity.GetCurrentPrincipalText: RawUtf8;
begin
  result := Edit_Principal.Caption;
end;

function TVisAdvancedSecurity.GetCurrentObjectText: RawUtf8;
begin
  result := Edit_Object.Caption;
end;

function TVisAdvancedSecurity.GetCurrentInheritedObjectText: RawUtf8;
begin
  result := Edit_InheritedObject.Caption;
end;

function TVisAdvancedSecurity.PickPrincipal(out aSid, aName: RawUtf8): Boolean;
begin
  result := SelectObjectSid(aSid, aName);
end;

function TVisAdvancedSecurity.PickOwner(out aSid, aName: RawUtf8): Boolean;
begin
  result := SelectObjectSid(aSid, aName);
end;

function TVisAdvancedSecurity.PickObject(out aGUID, aName: RawUtf8; aAllowedtype: TRawUtf8DynArray): Boolean;
begin
  result := SelectObjectGUID(aGUID, aName, aAllowedtype);
end;

procedure TVisAdvancedSecurity.ShowError(const aMsg: RawUtf8);
begin
  MessageDlg('Advanced Security Error', aMsg, mtError, [mbOK], 0);
end;

procedure TVisAdvancedSecurity.CloseRequest;
begin
  Close;
end;

end.

