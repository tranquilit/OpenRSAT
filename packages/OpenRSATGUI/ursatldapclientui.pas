unit ursatldapclientui;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  Dialogs,
  SysUtils,
  System.UITypes,
  mormot.core.base,
  mormot.core.os.security,
  mormot.net.ldap,
  ursatldapclient;

type

  { TRSATLdapClientHelper }

  TRSATLdapClientHelper = class helper for TRsatLdapClient
    function MoveLdapEntries(oldDN, newDN: array of String): Boolean;
    function GetAceRoot(DN: RawUtf8; Ace: TSecAce): RawUtf8;
    procedure OrderAcl(DN, baseDN: RawUtf8; Acl: PSecAcl);
    function ProtectAgainstAccidentalDeletion(DN, BaseDN: RawUtf8;
      out NewSecDesc: TSecurityDescriptor; Enabled: Boolean): Boolean;
  end;

function ShowLdapError(LdapClient: TLdapClient): TModalResult;
function ShowLdapSearchError(LdapClient: TLdapClient): TModalResult;
function ShowLdapModifyError(LdapClient: TLdapClient): TModalResult;
function ShowLdapDeleteError(LdapClient: TLdapClient): TModalResult;
function ShowLdapConnectError(LdapClient: TLdapClient): TModalResult;
function ShowLdapAddError(LdapClient: TLdapClient): TModalResult;


implementation
uses
  mormot.core.log,
  mormot.core.text,
  ucommon;

function AceIsUseless(Ace: PSecAce): Boolean;
begin
  result := Ace^.Mask = [];
end;

function CompareAce(p1, p2: PSecAce; sdArr: array of TSecurityDescriptor
  ): Integer;
const
  DENY: set of TSecAceType = [satAccessDenied, satObjectAccessDenied, satCallbackAccessDenied, satCallbackObjectAccessDenied];
begin
  result := 0;

  if not Assigned(p1) or not Assigned(p2) then
    Exit;

  // Compare inheritance
  result := GetAceParentCount(p1^, sdArr) - GetAceParentCount(p2^, sdArr);
  if result <> 0 then
    Exit;

  // Compare deny / allow
  if (p1^.AceType in DENY) = (p2^.AceType in DENY) then
    result := Ord(p1^.AceType) - Ord(p2^.AceType)
  else if p1^.AceType in DENY then
    result := -1
  else
    result := 1;
  if result <> 0 then
    Exit;

  // Compare global or object access
  if IsNullGuid(p1^.ObjectType) then
    result := -1;
  if IsNullGuid(p2^.ObjectType) then
    result := result + 1;
  if result <> 0 then
    Exit;

  // Compare sid
  result := CompareStr(RawSidToText(p1^.sid), RawSidToText(p2^.Sid));
end;

procedure InnerOrderAcl(Acl: PSecAcl; sdArr: Array of TSecurityDescriptor);
var
  ace: TSecAce;
  idx, j, lowest: Integer;
begin
  idx := 0;
  while idx < Length(acl^) do // select sort
  begin
    if AceIsUseless(@acl^[idx]) then
    begin
      Delete(acl^, idx, 1);
      continue;
    end;

    lowest := idx;
    for j := idx to High(acl^) do
      if CompareAce(@acl^[j], @acl^[lowest], sdArr) < 0 then
        lowest := j;

    if lowest > idx then
    begin
      ace := acl^[lowest];
      Delete(acl^, lowest, 1);
      Insert(ace, acl^, idx);
    end;
    Inc(idx);
  end;
end;

function GetLdapErrorCustomMessage(LdapClient: TLdapClient): RawUtf8;
begin
  result := '';

  case LdapClient.ResultError of
    leOperationsError: result := rsOperationsError;
    leProtocolError: result := rsProtocolError;
    leTimeLimitExceeded: result := rsTimeLimitExceeded;
    leSizeLimitExceeded: result := rsSizeLimitExceeded;
    leAuthMethodNotSupported: result := rsAuthMethodNotSupported;
    leStrongerAuthRequired: result := rsStrongerAuthRequired;
    leReferral: result := rsReferral;
    //leAdminLimitExceeded: Result := rsAdminLimitExceeded;
    //leUnavailableCriticalExtension: result := rsUnavailableCriticalExtension;
    leConfidentalityRequired: result := rsConfidentialityRequired;
    leSaslBindInProgress: result := rsSaslBindInProgress;
    leNoSuchAttribute: result := rsNoSuchAttribute;
    leUndefinedAttributeType: result := rsUndefinedAttributeType;
    leInappropriateMatching: result := rsInappropriateMatching;
    leConstraintViolation: result := rsConstraintViolation;
    leInsufficientAccessRights: result := rsInsufficientAccessRights;
  end;

  if (result = '') then
    result := LdapClient.ResultString;
end;

function ShowLdapError(Message: RawUtf8): TModalResult;
begin
  result := MessageDlg(rsLdapError, Message, mtError, mbOKCancel, 0);
end;

function ShowLdapError(LdapClient: TLdapClient): TModalResult;
var
  Message: RawUtf8;
begin
  Message := GetLdapErrorCustomMessage(LdapClient);

  result := ShowLdapError(FormatUtf8(rsLdapFailed, [Message]));
end;

function ShowLdapSearchError(LdapClient: TLdapClient): TModalResult;
var
  Message: RawUtf8;
begin
  Message := GetLdapErrorCustomMessage(LdapClient);

  result := ShowLdapError(FormatUtf8(rsLdapSearchFailed, [Message]));
end;

function ShowLdapModifyError(LdapClient: TLdapClient): TModalResult;
var
  Message: RawUtf8;
begin
  Message := GetLdapErrorCustomMessage(LdapClient);

  result := ShowLdapError(FormatUtf8(rsLdapModifyFailed, [Message]));
end;

function ShowLdapDeleteError(LdapClient: TLdapClient): TModalResult;
var
  Message: RawUtf8;
begin
  Message := GetLdapErrorCustomMessage(LdapClient);

  result := ShowLdapError(FormatUtf8(rsLdapDeleteFailed, [Message]));
end;

function ShowLdapConnectError(LdapClient: TLdapClient): TModalResult;
var
  Message: RawUtf8;
begin
  Message := GetLdapErrorCustomMessage(LdapClient);

  if String(Message).ToLower.Contains('52e') then
    Message := 'Invalid credentials.';

  result := ShowLdapError(FormatUtf8(rsLdapConnectFailed, [Message]));
end;

function ShowLdapAddError(LdapClient: TLdapClient): TModalResult;
var
  Message: RawUtf8;
begin
  Message := GetLdapErrorCustomMessage(LdapClient);

  result := ShowLdapError(FormatUtf8(rsLdapAddFailed, [Message]));
end;

{ TRSATLdapClientHelper }

function TRSATLdapClientHelper.MoveLdapEntries(oldDN, newDN: array of String
  ): Boolean;
var
  i: Integer;
begin
  result := False;

  if Length(oldDN) <> Length(newDN) then
    Exit;

  result := True;
  //if MessageDlg(rsWarning, rsLdapMoveWarningMessage, mtWarning, [mbYes, mbNo, mbCancel], 0) <> mrYes then
  //  Exit;

  for i := 0 to Length(oldDN) - 1 do
  begin
    result := MoveLdapEntry(oldDN[i], newDN[i]);
    if not result then
      Exit;
  end;
end;

function TRSATLdapClientHelper.ProtectAgainstAccidentalDeletion(DN,
  BaseDN: RawUtf8; out NewSecDesc: TSecurityDescriptor; Enabled: Boolean
  ): Boolean;
var
  Sid: RawSid;
  SecDescParent: TSecurityDescriptor;
  data: TLdapAttribute;
  i: Integer;
begin
  result := False;
  Sid := KnownRawSid(wksWorld);

  // Parent
  if Enabled then
  begin
    DN := GetParentDN(DN); // Get Parent SecDesc
    data := Self.SearchObject(atNTSecurityDescriptor, DN, '');
    if not Assigned(data) then
      Exit; // Failure
    if not SecDescParent.FromBinary(data.GetRaw()) then
    begin
      //Dialogs.MessageDlg(rsTitleParsing, rsACEParsing, mtError, [mbOK], 0);
      Exit; // Failure
    end;

    if SecDescFindACE(@SecDescParent, satAccessDenied, Sid, [samDeleteChild], @ATTR_UUID[kaNull]) = -1 then // Search if ACE already exists
    begin
      if MessageDlg(rsConfirmation, rsACEpaadParent, mtConfirmation, mbYesNo, 0) <> mrYes then // User Confirmation
        Exit; // Failure

      if not Assigned(SecDescAddOrUpdateACE(@SecDescParent, ATTR_UUID[kaNull], Sid, satAccessDenied, [samDeleteChild])) then // Add ACE
      begin
        //Dialogs.MessageDlg(rsTitleParsing, FormatUtf8(rsACECreateFailure, [DN]), mtError, [mbOK], 0);
        Exit; // Failure
      end;

      OrderAcl(DN, BaseDN, @SecDescParent.Dacl); // Order

      if not Self.Modify(DN, lmoReplace, atNTSecurityDescriptor, SecDescParent.ToBinary()) then // Modify
        Exit; // Failure
    end;
  end;

  // Self
  if Enabled then
  begin // Add ACE
    if not Assigned(SecDescAddOrUpdateACE(@NewSecDesc, ATTR_UUID[kaNull], Sid, satAccessDenied, [samDelete, samDeleteTree])) then
    begin
      //Dialogs.MessageDlg(rsTitleParsing, FormatUtf8(rsACECreateFailure, [DN]), mtError, [mbOK], 0);
      Exit; // Failure
    end
  end
  else
  begin // Remove ACE
    i := SecDescFindACE(@NewSecDesc, satAccessDenied, Sid, [samDelete, samDeleteTree], @ATTR_UUID[kaNull]);
    if i = -1 then
    begin
      //Dialogs.MessageDlg(rsTitleNotFound, FormatUtf8(rsACENotFound, [DN]), mtError, [mbOK], 0);
      Exit; // Failure
    end;
    NewSecDesc.Dacl[i].Mask -= [samDelete, samDeleteTree];
  end;

  OrderAcl(DN, BaseDN, @NewSecDesc.Dacl); // Order

  result := True; // Success
end;

function TRSATLdapClientHelper.GetAceRoot(DN: RawUtf8; Ace: TSecAce): RawUtf8;
var
  sd: TSecurityDescriptor;
  pace: TSecAce;
  pDN: RawUtf8;
  LdapObject: TLdapAttribute;
begin
  pDN := GetParentDN(DN);
  LdapObject := Self.SearchObject(atNTSecurityDescriptor, pDN, '');
  if not Assigned(LdapObject) then
    Exit;
  sd.FromBinary(LdapObject.GetRaw());
  for pace in sd.Dacl do
  begin
    if (TSecAceFlag.safInherited in Ace.Flags) and (AceIsEqual(ace, pace)) then
    begin
      result := GetAceRoot(pDN, ace);
      Exit;
    end;
  end;
  result := DN;
end;

// https://learn.microsoft.com/en-us/windows/win32/secauthz/order-of-aces-in-a-dacl
procedure TRSATLdapClientHelper.OrderAcl(DN, baseDN: RawUtf8; Acl: PSecAcl);
var
  sdArr: Array of TSecurityDescriptor;
  sd: TSecurityDescriptor;
  parent, filter: RawUtf8;
  res: TLdapResult;
begin
  TSynLog.Add.Log(sllDebug, FormatUtf8('Start ordering ACL for %...', [DN]));
  sdArr := [];

  parent := DN;
  filter := '';
  while not (parent = Self.DefaultDN(baseDN)) and not (parent = '') do
  begin
    if (filter = '') then
      filter := '|';
    parent := GetParentDN(parent);
    filter := FormatUtf8('%(distinguishedName=%)', [filter, LdapEscape(parent)]);
  end;

  // Get SDs
  Self.SearchBegin();
  try
    repeat
      Self.SearchScope := lssWholeSubtree;
      if not Self.Search([atNTSecurityDescriptor], filter) then
        Exit;
      for res in Self.SearchResult.Items do
      begin
        if not sd.FromBinary(res.Attributes.Find(atNTSecurityDescriptor).GetRaw()) then
          continue;
        Insert(sd, sdArr, Length(sdArr));
      end;
    until (Self.SearchCookie = '');
  finally
    Self.SearchEnd();
  end;

  // Order acl
  InnerOrderAcl(acl, sdArr);
  TSynLog.Add.Log(sllDebug, 'End ordering ACL for.');
end;

end.

