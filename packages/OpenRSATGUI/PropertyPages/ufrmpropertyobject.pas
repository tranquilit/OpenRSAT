unit ufrmpropertyobject;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  Dialogs,
  SysUtils,
  Forms,
  Controls,
  ExtCtrls,
  StdCtrls,
  ActnList,
  mormot.core.log,
  mormot.core.os.security,
  uproperty,
  ucommon,
  upropertyframe;

type

  { TPropertyObject }

  TPropertyObject = class
  private
    fLog: TSynLog;

    function IsSelfProtected(PSecDesc: PSecurityDescriptor; Sid: RawSid): Boolean;
    function IsParentProtected(Props: TProperty; Sid: RawSid): Boolean;

    function AddProtection(PSecDesc: PSecurityDescriptor; Sid: RawSid): Boolean;
    function DelProtection(PSecDesc: PSecurityDescriptor; Sid: RawSid): Boolean;
  public
    constructor Create;
    // Protection works only if Self and Parent does not allow deletion.
    // If one of them allow, deletion is allowed.
    function IsProtected(Props: TProperty): Boolean;

    function SetProtected(Props: TProperty; Protected: Boolean): Boolean;
  end;

  { TFrmPropertyObject }

  TFrmPropertyObject = class(TPropertyFrame)
    Action_Protection: TAction;
    ActionList1: TActionList;
    CheckBox_Protection: TCheckBox;
    Edit_CanonicalName: TEdit;
    Edit_ObjectClass: TEdit;
    Edit_USNActual: TEdit;
    Edit_USNOriginal: TEdit;
    Edit_WhenChanged: TEdit;
    Edit_WhenCreated: TEdit;
    Label_CanonicalName: TLabel;
    Label_ObjectClass: TLabel;
    Label_USNUpdate: TLabel;
    Label_USNActual: TLabel;
    Label_USNOriginal: TLabel;
    Label_WhenChanged: TLabel;
    Label_WhenCreated: TLabel;
    Panel_ObjectExtra: TPanel;
    Panel_Object: TPanel;
    Panel_ObjectInfo: TPanel;
    procedure Action_ProtectionExecute(Sender: TObject);
  private
    fPropertyObject: TPropertyObject;
    fLog: TSynLog;
    fProperty: TProperty;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(Props: TProperty); override;
  end;

implementation
uses
  mormot.core.base,
  mormot.core.datetime,
  mormot.net.ldap,
  ursatldapclientui;

{$R *.lfm}

{ TPropertyObject }

function TPropertyObject.IsSelfProtected(PSecDesc: PSecurityDescriptor;
  Sid: RawSid): Boolean;
begin
  result := False;

  if not Assigned(PSecDesc) then
    Exit;

  result := (SecDescFindACE(PSecDesc, satAccessDenied, Sid, [samDelete, samDeleteTree], @ATTR_UUID[kaNull]) >= 0);
end;

function TPropertyObject.IsParentProtected(Props: TProperty; Sid: RawSid
  ): Boolean;
var
  Attribute: TLdapAttribute;
  SecDesc: TSecurityDescriptor;
begin
  result := False;

  if not Assigned(Props) or not Assigned(Props.Core) or not Assigned(Props.Core.LdapClient) then
    Exit;

  result := (Props.distinguishedName = Props.Core.LdapClient.DefaultDN);

  if result then
    Exit;

  Attribute := Props.Core.LdapClient.SearchObject(GetParentDN(Props.distinguishedName), '', 'nTSecurityDescriptor');
  if not Assigned(Attribute) then
  begin
    ShowLdapSearchError(Props.Core.LdapClient);
    Exit;
  end;

  if not SecDesc.FromBinary(Attribute.GetRaw()) then
  begin
    MessageDlg(rsLdapError, rsSecurityDescriptorInvalid, mtError, mbOKCancel, 0);
    Exit;
  end;
  result := (SecDescFindACE(@SecDesc, satAccessDenied, Sid, [samDeleteChild], @ATTR_UUID[kaNull]) >= 0);
end;

function TPropertyObject.AddProtection(PSecDesc: PSecurityDescriptor;
  Sid: RawSid): Boolean;
begin
  result := False;

  if not Assigned(PSecDesc) then
    Exit;

  if not Assigned(SecDescAddOrUpdateACE(PSecDesc, ATTR_UUID[kaNull], Sid, satAccessDenied, [samDelete, samDeleteTree])) then
  begin
    if Assigned(fLog) then
      fLog.Log(sllWarning, 'Cannot add ACE', Self);
    Exit;
  end;
  result := True;
end;

function TPropertyObject.DelProtection(PSecDesc: PSecurityDescriptor; Sid: RawSid): Boolean;
var
  i: Integer;
begin
  result := False;

  if not Assigned(PSecDesc) then
    Exit;

  i := SecDescFindACE(PSecDesc, satAccessDenied, Sid, [samDelete, samDeleteTree], @ATTR_UUID[kaNull]);
  if i < 0 then
  begin
    if Assigned(fLog) then
      fLog.Log(sllWarning, 'Cannot find ACE', Self);
    Dialogs.MessageDlg(rsTitleNotFound, 'Cannot find ACE', mtError, [mbOK], 0);
    Exit;
  end;
  PSecDesc^.Dacl[i].Mask -= [samDelete, samDeleteTree];
  result := True;
end;

constructor TPropertyObject.Create;
begin
  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);
end;

function TPropertyObject.IsProtected(Props: TProperty): Boolean;
var
  Sid: RawSid;
begin
  Sid := KnownRawSid(wksWorld);

  result := IsSelfProtected(Props.SecurityDescriptor, Sid) and IsParentProtected(Props, Sid);
end;

function TPropertyObject.SetProtected(Props: TProperty; Protected: Boolean): Boolean;
var
  PSecDesc: PSecurityDescriptor;
  Sid: RawSid;
begin
  result := False;

  PSecDesc := Props.SecurityDescriptor;
  if not Assigned(PSecDesc) then
    Exit;

  Sid := KnownRawSid(wksWorld);

  if Protected then
    result := AddProtection(PSecDesc, Sid)
  else
    result := DelProtection(PSecDesc, Sid);
  if not result then
    Exit;

  Props.LdapClient.OrderAcl(Props.DistinguishedName, Props.LdapClient.DefaultDN(), @PSecDesc^.Dacl);

  Props.SecurityDescriptor := PSecDesc;
end;

{ TFrmPropertyObject }

procedure TFrmPropertyObject.Action_ProtectionExecute(Sender: TObject);
begin
  fPropertyObject.SetProtected(fProperty, CheckBox_Protection.Checked);
end;

constructor TFrmPropertyObject.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fPropertyObject := TPropertyObject.Create;

  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);

  Caption := 'Object';
end;

destructor TFrmPropertyObject.Destroy;
begin
  FreeAndNil(fPropertyObject);

  inherited Destroy;
end;

procedure TFrmPropertyObject.Update(Props: TProperty);
var
  ObjectClassAttribute, Attribute: TLdapAttribute;
  Sid: RawSid;
  SecurityDescriptor: PSecurityDescriptor;
  SecDesc: TSecurityDescriptor;
  FoundSelf, FoundParent: Boolean;
begin
  fProperty := Props;

  Edit_CanonicalName.Text := fProperty.canonicalName;
  ObjectClassAttribute := fProperty.Attributes.Find('objectClass');
  Edit_ObjectClass.Text := ObjectClassAttribute.GetReadable(ObjectClassAttribute.Count - 1);
  Edit_WhenCreated.Text := DateTimetoStr(fProperty.whenCreated);
  Edit_WhenChanged.Text := DateTimetoStr(fProperty.whenChanged);
  Edit_USNActual.Text := fProperty.Attributes.Find('uSNChanged').GetReadable();
  Edit_USNOriginal.Text := fProperty.Attributes.Find('uSNCreated').GetReadable();

  CheckBox_Protection.Checked := fPropertyObject.IsProtected(fProperty);

  Sid := KnownRawSid(wksWorld);
  // Self

  SecurityDescriptor := Props.SecurityDescriptor;
  if not Assigned(SecurityDescriptor) then
    Exit;

  FoundSelf := SecDescFindACE(SecurityDescriptor,
    satAccessDenied, Sid,
    [samDelete, samDeleteTree], @ATTR_UUID[kaNull]) <> -1;

  // Parent
  FoundParent := False;
  if (Props.distinguishedName = Props.Core.LdapClient.DefaultDN) then
    FoundParent := True
  else
  begin
    Attribute := Props.Core.LdapClient.SearchObject(GetParentDN(Props.distinguishedName), '', 'nTSecurityDescriptor');
    if not Assigned(Attribute) then
    begin
      ShowLdapSearchError(Props.Core.LdapClient);
      Exit;
    end;

    if not SecDesc.FromBinary(Attribute.GetRaw()) then
    begin
      MessageDlg(rsLdapError, rsSecurityDescriptorInvalid, mtError, mbOKCancel, 0);
      Exit;
    end;
    FoundParent := SecDescFindACE(@SecDesc,
      satAccessDenied, Sid,
      [samDeleteChild], @ATTR_UUID[kaNull]) <> -1;
  end;

  // Protect against accidental deletion
  CheckBox_Protection.Checked := FoundSelf and FoundParent;
end;

end.

