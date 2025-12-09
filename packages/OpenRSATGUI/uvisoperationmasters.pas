unit uvisoperationmasters;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ComCtrls,
  StdCtrls,
  Buttons,
  ExtCtrls,
  ActnList,
  uproperty,
  mormot.net.ldap,
  mormot.core.base;

type

  TChangeOperationMasterCallBack = Procedure(DomainController: RawUtf8) of Object;

  { TVisOperationMasters }

  TVisOperationMasters = class(TForm)
    Action_SchemaMaster: TAction;
    Action_DomainNamingMaster: TAction;
    Action_PDC: TAction;
    Action_RID: TAction;
    Action_InfrastructureMaster: TAction;
    Actions: TActionList;
    BitBtn_SchemaMaster: TBitBtn;
    BitBtn_DomainNamingMaster: TBitBtn;
    BitBtn_PDC: TBitBtn;
    BitBtn_RID: TBitBtn;
    BitBtn_InfrastructureMaster: TBitBtn;
    Edit_SchemaMaster: TEdit;
    Edit_DomainNamingMaster: TEdit;
    Edit_PDC: TEdit;
    Edit_RID: TEdit;
    Edit_InfrastructureMaster: TEdit;
    Label_SchemaMaster: TLabel;
    Label_DomainNamingMaster: TLabel;
    Label_PDC: TLabel;
    Label_RID: TLabel;
    Label_InfrastructureMaster: TLabel;
    Panel_SchemaMaster: TPanel;
    Panel_InfrastructureMaster: TPanel;
    Panel_RID: TPanel;
    Panel_PDC: TPanel;
    Panel_DomainNamingMaster: TPanel;
    procedure Action_DomainNamingMasterExecute(Sender: TObject);
    procedure Action_InfrastructureMasterExecute(Sender: TObject);
    procedure Action_PDCExecute(Sender: TObject);
    procedure Action_RIDExecute(Sender: TObject);
    procedure Action_SchemaMasterExecute(Sender: TObject);
  private
    function SchemaMaster(LdapClient: TLdapClient): RawUtf8;
    function DomainNamingMaster(LdapClient: TLdapClient): RawUtf8;
    function PDC(LdapClient: TLdapClient): RawUtf8;
    function RID(LdapClient: TLdapClient): RawUtf8;
    function InfrastructureMaster(LdapClient: TLdapClient): RawUtf8;

    function DCHostName(DistinguishedName: RawUtf8): RawUtf8;

    function DCSettings(DomainController: RawUtf8): RawUtf8;

    function ChangeMaster(Master: RawUtf8): RawUtf8;

    procedure UpdateEdit(Edit: TEdit; DomainController: RawUtf8);
  public
    constructor Create(TheOwner: TComponent); reintroduce;

  end;

implementation
uses
  mormot.core.variants,
  mormot.core.text,
  ursatldapclient,
  ursatldapclientui,
  uvischangedomaincontroller,
  ucommon,
  ufrmrsat;

{$R *.lfm}

{ TVisOperationMasters }

procedure TVisOperationMasters.Action_SchemaMasterExecute(Sender: TObject);
begin
  UpdateEdit(Edit_SchemaMaster, ChangeMaster(SchemaMaster(FrmRSAT.LdapClient)));
end;

procedure TVisOperationMasters.Action_DomainNamingMasterExecute(Sender: TObject
  );
begin
  UpdateEdit(Edit_DomainNamingMaster, ChangeMaster(DomainNamingMaster(FrmRSAT.LdapClient)));
end;

procedure TVisOperationMasters.Action_InfrastructureMasterExecute(
  Sender: TObject);
begin
  UpdateEdit(Edit_InfrastructureMaster, ChangeMaster(InfrastructureMaster(FrmRSAT.LdapClient)));
end;

procedure TVisOperationMasters.Action_PDCExecute(Sender: TObject);
begin
  UpdateEdit(Edit_PDC, ChangeMaster(PDC(FrmRSAT.LdapClient)));
end;

procedure TVisOperationMasters.Action_RIDExecute(Sender: TObject);
begin
  UpdateEdit(Edit_RID, ChangeMaster(RID(FrmRSAT.LdapClient)));
end;

function TVisOperationMasters.SchemaMaster(LdapClient: TLdapClient): RawUtf8;
begin
  result := FormatUtf8('CN=Schema,CN=Configuration,%', [LdapClient.DefaultDN]);
end;

function TVisOperationMasters.DomainNamingMaster(LdapClient: TLdapClient
  ): RawUtf8;
begin
  result := FormatUtf8('CN=Partitions,CN=Configuration,%', [LdapClient.DefaultDN]);
end;

function TVisOperationMasters.PDC(LdapClient: TLdapClient): RawUtf8;
begin
  result := LdapClient.RootDN;
end;

function TVisOperationMasters.RID(LdapClient: TLdapClient): RawUtf8;
begin
  result := FormatUtf8('CN=RID Manager$,CN=System,%', [LdapClient.DefaultDN]);
end;

function TVisOperationMasters.InfrastructureMaster(LdapClient: TLdapClient
  ): RawUtf8;
begin
  result := FormatUtf8('CN=Infrastructure,%', [LdapClient.DefaultDN]);
end;

function TVisOperationMasters.DCHostName(DistinguishedName: RawUtf8): RawUtf8;
var
  RoleOwner: TLdapAttribute;
  DCSettingsDN: RawUtf8;
  LdapClient: TRsatLdapClient;
begin
  result := '';
  LdapClient := FrmRSAT.LdapClient;
  RoleOwner := LdapClient.SearchObject(DistinguishedName, '', 'fSMORoleOwner');

  if not Assigned(RoleOwner) then
    Exit;

  DCSettingsDN := GetParentDN(RoleOwner.GetReadable());

  result := LdapClient.SearchObject(DCSettingsDN, '', 'dNSHostName').GetReadable();
end;

function TVisOperationMasters.DCSettings(DomainController: RawUtf8): RawUtf8;
var
  LdapClient: TRsatLdapClient;
  DCObject: TLdapAttribute;
  DCObjectDN: RawUtf8;
begin
  result := '';
  LdapClient := FrmRSAT.LdapClient;
  DCObject := LdapClient.SearchObject(LdapClient.ConfigDN, FormatUtf8('(dNSHostName=%)', [LdapEscape(DomainController)]), 'distinguishedName', lssWholeSubtree);

  DCObjectDN := DCObject.GetReadable();
  if DCObjectDN <> '' then
    result := FormatUtf8('CN=NTDS Settings,%', [DCObjectDN]);
end;

function TVisOperationMasters.ChangeMaster(Master: RawUtf8): RawUtf8;
var
  Vis: TVisChangeDomainController;
  NewDCSettings: RawUtf8;
  mr: Integer;
begin
  result := '';
  Vis := TVisChangeDomainController.Create(Self);
  try
    mr := Vis.ShowModal;
    if mr <> mrOK then
      Exit;
    NewDCSettings := DCSettings(Vis.DomainController);
    if NewDCSettings = '' then
      Exit;
    if not FrmRSAT.LdapClient.Modify(Master, lmoReplace, 'fSMORoleOwner', NewDCSettings) then
    begin
      ShowLdapModifyError(FrmRSAT.LdapClient);
      Exit;
    end;
    result := Vis.DomainController;
  finally
    FreeAndNil(Vis);
  end;
end;

procedure TVisOperationMasters.UpdateEdit(Edit: TEdit; DomainController: RawUtf8);
begin
  if DomainController <> '' then
    Edit.Caption := DomainController;
end;

constructor TVisOperationMasters.Create(TheOwner: TComponent);
begin
  Inherited Create(TheOwner);

  Edit_SchemaMaster.Caption := DCHostName(SchemaMaster(FrmRSAT.LdapClient));
  Edit_DomainNamingMaster.Caption := DCHostName(DomainNamingMaster(FrmRSAT.LdapClient));
  Edit_PDC.Caption := DCHostName(PDC(FrmRSAT.LdapClient));
  Edit_RID.Caption := DCHostName(RID(FrmRSAT.LdapClient));
  Edit_InfrastructureMaster.Caption := DCHostName(InfrastructureMaster(FrmRSAT.LdapClient));
end;

end.

