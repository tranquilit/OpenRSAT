unit ursatldapclient;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
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
  public
    procedure SearchPagingBegin(PageNumber: Integer);
    function SearchAllDocPaged(DocResult: PDocVariantData; const BaseDN: RawUtf8;
      TypesOnly: boolean; const Filter: RawUtf8;
      const Attributes: array of RawUtf8): Boolean;
    procedure SearchPagingEnd;
    function MoveLdapEntry(oldDN, newDN: string): Boolean;
    function RenameLdapEntry(DN, newName: string): Boolean;

    property DomainName: RawUtf8 read fDomainName write SetDomainName;
    property DomainControllerName: RawUtf8 read fDomainControllerName write SetDomainControllerName;

    procedure ChangeSettings(ASettings: TLdapClientSettings; AutoConnect: Boolean = True);
  public
    function Connect(DiscoverMode: TLdapClientConnect=[lccCldap, lccTlsFirst];
      DelayMS: integer=500): boolean;
    function Close: boolean;
  private
    fOnConnect: TNotifyEvent;
    fOnClose: TNotifyEvent;
    fOnSearch: TNotifyEvent;
    fOnDelete: TNotifyEvent;
    fOnAdd: TNotifyEvent;
    fOnModify: TNotifyEvent;
    fOnError: TNotifyEvent;

    procedure SetOnAdd(AValue: TNotifyEvent);
    procedure SetOnClose(AValue: TNotifyEvent);
    procedure SetOnConnect(AValue: TNotifyEvent);
    procedure SetOnDelete(AValue: TNotifyEvent);
    procedure SetOnError(AValue: TNotifyEvent);
    procedure SetOnModify(AValue: TNotifyEvent);
    procedure SetOnSearch(AValue: TNotifyEvent);
  published
    property OnConnect: TNotifyEvent read fOnConnect write SetOnConnect;
    property OnClose: TNotifyEvent read fOnClose write SetOnClose;
    property OnSearch: TNotifyEvent read fOnSearch write SetOnSearch;
    property OnDelete: TNotifyEvent read fOnDelete write SetOnDelete;
    property OnAdd: TNotifyEvent read fOnAdd write SetOnAdd;
    property OnModify: TNotifyEvent read fOnModify write SetOnModify;
    property OnError: TNotifyEvent read fOnError write SetOnError;
  end;

implementation

uses
  mormot.core.log,
  mormot.core.text,
  mormot.core.rtti;

{ TRsatLdapClient }

procedure TRsatLdapClient.SetDomainControllerName(AValue: RawUtf8);
begin
  if fDomainControllerName = AValue then
    Exit;
  Close;
  fDomainControllerName := AValue;
  Settings.TargetHost := fDomainControllerName;
  Settings.KerberosSpn := '';
  Connect();
end;

procedure TRsatLdapClient.SetDomainName(AValue: RawUtf8);
begin
  if fDomainName = AValue then
    Exit;
  Close;
  fDomainName := AValue;
  Settings.KerberosDN := fDomainName;
  Connect;
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

procedure TRsatLdapClient.ChangeSettings(ASettings: TLdapClientSettings;
  AutoConnect: Boolean);
begin
  if Assigned(fSettings) then
    FreeAndNil(fSettings);
  Close;
  fSettings := TLdapClientSettings.Create;

  CopyObject(ASettings, fSettings);
  if AutoConnect then
  Connect;
end;

function TRsatLdapClient.Connect(DiscoverMode: TLdapClientConnect;
  DelayMS: integer): boolean;
begin
  Result := inherited Connect(DiscoverMode, DelayMS);

  if Result and Assigned(fOnConnect) then
    fOnConnect(Self)
  else if Assigned(fOnError) then
    fOnError(Self);
end;

function TRsatLdapClient.Close: boolean;
begin
  Result := inherited Close;

  if Assigned(fOnClose) then
    fOnClose(Self)
  else if Assigned(fOnError) then
    fOnError(Self);
end;

procedure TRsatLdapClient.SetOnAdd(AValue: TNotifyEvent);
begin
  if fOnAdd=AValue then Exit;
  fOnAdd:=AValue;
end;

procedure TRsatLdapClient.SetOnClose(AValue: TNotifyEvent);
begin
  if fOnClose=AValue then Exit;
  fOnClose:=AValue;
end;

procedure TRsatLdapClient.SetOnConnect(AValue: TNotifyEvent);
begin
  if fOnConnect=AValue then Exit;
  fOnConnect:=AValue;
end;

procedure TRsatLdapClient.SetOnDelete(AValue: TNotifyEvent);
begin
  if fOnDelete=AValue then Exit;
  fOnDelete:=AValue;
end;

procedure TRsatLdapClient.SetOnError(AValue: TNotifyEvent);
begin
  if fOnError=AValue then Exit;
  fOnError:=AValue;
end;

procedure TRsatLdapClient.SetOnModify(AValue: TNotifyEvent);
begin
  if fOnModify=AValue then Exit;
  fOnModify:=AValue;
end;

procedure TRsatLdapClient.SetOnSearch(AValue: TNotifyEvent);
begin
  if fOnSearch=AValue then Exit;
  fOnSearch:=AValue;
end;

end.

