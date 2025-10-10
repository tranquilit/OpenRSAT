unit ursatldapclient;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Dialogs,
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
    procedure MoveLdapEntries(oldDN, newDN: Array of String);
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

implementation

uses
  System.UITypes,
  mormot.core.log,
  mormot.core.text,
  mormot.core.rtti,
  ucommon;

{ TRsatLdapClient }

procedure TRsatLdapClient.SetDomainControllerName(AValue: RawUtf8);
begin
  if fDomainControllerName = AValue then
    Exit;
  fDomainControllerName := AValue;
  Settings.TargetHost := fDomainControllerName;
  Reconnect('Change Domain Controller');
end;

procedure TRsatLdapClient.SetDomainName(AValue: RawUtf8);
begin
  if fDomainName = AValue then
    Exit;
  fDomainName := AValue;
  Settings.KerberosDN := fDomainName;
  Reconnect('Change Domain');
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

procedure TRsatLdapClient.MoveLdapEntries(oldDN, newDN: array of String);
var
  i: Integer;
begin
  if Length(oldDN) <> Length(newDN) then
    Exit;
  if MessageDlg(rsWarning, rsLdapMoveWarningMessage, mtWarning, [mbYes, mbNo, mbCancel], 0) <> mrYes then
    Exit;
  for i := 0 to Length(oldDN) - 1 do
  begin
    MoveLdapEntry(oldDN[i], newDN[i]);
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
    aLog.Log(sllDebug, 'oldDN or newDN is empty');
    Exit;
  end;
  ParseDN(newDN, DNs);
  newRdn := DNs[0].Name + '=' + DNs[0].Value;
  newParentDN := DNs[1].Name + '=' + DNs[1].Value;
  for i := 2 to High(DNs) do
    newParentDN += ',' + DNs[i].Name + '=' + DNs[i].Value;
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
    aLog.Log(sllDebug, 'DN or NewName is empty');
    Exit;
  end;
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

  NotifyConnect;
end;

function TRsatLdapClient.Close: boolean;
begin
  NotifyClose;

  Result := inherited Close;
end;

end.

