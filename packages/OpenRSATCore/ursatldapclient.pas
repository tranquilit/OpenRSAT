unit ursatldapclient;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Dialogs,
  System.UITypes,
  mormot.core.base,
  mormot.core.variants,
  mormot.net.ldap;

type

  TProcLdapClientObject = procedure(LdapClient: TLdapClient) of Object;

  { TRsatLdapClient }

  TRsatLdapClient = class(TLdapClient)
  private
    fDomainControllerName: RawUtf8;
    fDomainName: RawUtf8;
    procedure SetDomainControllerName(AValue: RawUtf8);
    procedure SetDomainName(AValue: RawUtf8);
  protected
    fPageNumber: Integer;
    fSearchAllResult: TLdapResultObjArray;

    fObserversConnect: Array of TProcLdapClientObject;
    fObserversClose: Array of TProcLdapClientObject;

    procedure NotifyConnect;
    procedure NotifyClose;
  public
    procedure SearchPagingBegin(PageNumber: Integer);
    function SearchAllDocPaged(DocResult: PDocVariantData; const BaseDN: RawUtf8;
      TypesOnly: boolean; const Filter: RawUtf8;
      const Attributes: array of RawUtf8): Boolean;
    procedure SearchPagingEnd;
    function MoveLdapEntries(oldDN, newDN: Array of String): Boolean;
    function MoveLdapEntry(oldDN, newDN: string): Boolean;
    function RenameLdapEntry(DN, newName: string): Boolean;

    property DomainName: RawUtf8 read fDomainName write SetDomainName;
    property DomainControllerName: RawUtf8 read fDomainControllerName write SetDomainControllerName;

    function RegisterObserverConnect(Func: TProcLdapClientObject): Boolean;
    function RemoveObserverConnect(Func: TProcLdapClientObject): Boolean;
    function RegisterObserverClose(Func: TProcLdapClientObject): Boolean;
    function RemoveObserverClose(Func: TProcLdapClientObject): Boolean;
    procedure ChangeSettings(ASettings: TLdapClientSettings);
  public
    function Connect(DiscoverMode: TLdapClientConnect=[lccCldap, lccTlsFirst];
      DelayMS: integer=500): boolean;
    function Close: boolean;
  end;

  function ShowLdapSearchError(LdapClient: TLdapClient): TModalResult;
  function ShowLdapModifyError(LdapClient: TLdapClient): TModalResult;
  function ShowLdapDeleteError(LdapClient: TLdapClient): TModalResult;
  function ShowLdapConnectError(LdapClient: TLdapClient): TModalResult;
  function ShowLdapAddError(LdapClient: TLdapClient): TModalResult;

implementation

uses
  mormot.core.log,
  mormot.core.text,
  mormot.core.rtti,
  ucommon;

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

{ TRsatLdapClient }

procedure TRsatLdapClient.SetDomainControllerName(AValue: RawUtf8);
begin
  if fDomainControllerName = AValue then
    Exit;
  fDomainControllerName := AValue;
  Settings.TargetHost := fDomainControllerName;
  Settings.KerberosSpn := '';
  Reconnect('Change Domain Controller');
  NotifyConnect;
end;

procedure TRsatLdapClient.SetDomainName(AValue: RawUtf8);
begin
  if fDomainName = AValue then
    Exit;
  fDomainName := AValue;
  Settings.KerberosDN := fDomainName;
  Reconnect('Change Domain');
  NotifyConnect;
end;

procedure TRsatLdapClient.NotifyConnect;
var
  Func: TProcLdapClientObject;
begin
  for Func in fObserversConnect do
    Func(Self);
end;

procedure TRsatLdapClient.NotifyClose;
var
  Func: TProcLdapClientObject;
begin
  for Func in fObserversClose do
    Func(Self);
end;

procedure TRsatLdapClient.SearchPagingBegin(PageNumber: Integer);
begin
  fPageNumber := PageNumber;
end;

function TRsatLdapClient.SearchAllDocPaged(DocResult: PDocVariantData; const BaseDN: RawUtf8;
  TypesOnly: boolean; const Filter: RawUtf8; const Attributes: array of RawUtf8
  ): Boolean;
var
  PageCount: Integer;
  item: TLdapResult;
  Attribute: TLdapAttribute;
begin
  PageCount := 0;
  repeat
    result := Search(BaseDN, TypesOnly, Filter, Attributes);

    for item in fSearchResult.Items do
    begin
      if not Assigned(item) then
        continue;
      for Attribute in item.Attributes.Items do
      begin
        if not Assigned(Attribute) then
          continue;
        DocResult^.O_[item.ObjectName]^.AddOrUpdateValue(Attribute.AttributeName, Attribute.GetVariant());
      end;
    end;
    Inc(PageCount);
  until (SearchCookie = '') or (fPageNumber = PageCount) or not result;
end;

procedure TRsatLdapClient.SearchPagingEnd;
begin
  fPageNumber := 0;
end;

function TRsatLdapClient.MoveLdapEntries(oldDN, newDN: array of String
  ): Boolean;
var
  i: Integer;
begin
  result := False;

  if Length(oldDN) <> Length(newDN) then
    Exit;

  result := True;
  if MessageDlg(rsWarning, rsLdapMoveWarningMessage, mtWarning, [mbYes, mbNo, mbCancel], 0) <> mrYes then
    Exit;

  for i := 0 to Length(oldDN) - 1 do
  begin
    result := MoveLdapEntry(oldDN[i], newDN[i]);
    if not result then
      Exit;
  end;
end;

function TRsatLdapClient.MoveLdapEntry(oldDN, newDN: string): Boolean;
var
  DNs: TNameValueDNs;
  newRdn, newParentDN: String;
  i: Integer;
  aLog: ISynLog;
begin
  result := False;
  aLog := TSynLog.Enter('Move Ldap Entry', []);

  assert(oldDN <> '', 'OldDN is empty.');
  assert(newDN <> '', 'NewDN is empty.');
  assert(Assigned(Self), 'Ldap instance is null.');

  if (oldDN = '') or (newDN = '') then
  begin
    if Assigned(aLog) then
      aLog.Log(sllDebug, 'oldDN or newDN is empty');
    Exit;
  end;
  ParseDN(newDN, DNs);
  newRdn := DNs[0].Name + '=' + DNs[0].Value;
  newParentDN := DNs[1].Name + '=' + DNs[1].Value;
  for i := 2 to High(DNs) do
    newParentDN += ',' + DNs[i].Name + '=' + DNs[i].Value;
  if Assigned(aLog) then
    aLog.Log(sllDebug, FormatUtf8('Moving Ldap entry "%" as "%" to "%".', [oldDN, newRDN, newParentDN]));

  result := ModifyDN(oldDN, newRdn, newParentDN, True);
end;

function TRsatLdapClient.RenameLdapEntry(DN, newName: string): Boolean;
var
  aLog: ISynLog;
begin
  result := False;
  aLog := TSynLog.Enter('Rename Ldap Entry', []);

  assert(DN <> '', 'DN is empty');
  assert(newName <> '', 'newName is empty');
  assert(Assigned(Self), 'Ldap instance is null.');

  if (DN = '') or (newName = '') then
  begin
    if Assigned(aLog) then
      aLog.Log(sllDebug, 'DN or NewName is empty');
    Exit;
  end;
  if Assigned(aLog) then
    aLog.Log(sllDebug, FormatUtf8('Renaming Ldap entry "%" as "%".', [DN, newName]));

  result := ModifyDN(DN, newName, '', True);
end;

function TRsatLdapClient.RegisterObserverConnect(Func: TProcLdapClientObject
  ): Boolean;
begin
  MultiEventAdd(fObserversConnect, TMethod(Func));
end;

function TRsatLdapClient.RemoveObserverConnect(Func: TProcLdapClientObject
  ): Boolean;
begin
  MultiEventRemove(fObserversConnect, TMethod(Func));
end;

function TRsatLdapClient.RegisterObserverClose(Func: TProcLdapClientObject
  ): Boolean;
begin
  MultiEventAdd(fObserversClose, TMethod(Func));
end;

function TRsatLdapClient.RemoveObserverClose(Func: TProcLdapClientObject
  ): Boolean;
begin
  MultiEventRemove(fObserversClose, TMethod(Func));
end;

procedure TRsatLdapClient.ChangeSettings(ASettings: TLdapClientSettings);
begin
  if Assigned(fSettings) then
    FreeAndNil(fSettings);
  fSettings := TLdapClientSettings.Create;

  CopyObject(ASettings, fSettings);
  Reconnect('Change settings');
end;

function TRsatLdapClient.Connect(DiscoverMode: TLdapClientConnect;
  DelayMS: integer): boolean;
begin
  Result := inherited Connect(DiscoverMode, DelayMS);

  if Result then
    NotifyConnect;
end;

function TRsatLdapClient.Close: boolean;
begin
  Result := inherited Close;

  NotifyClose;
end;

end.

