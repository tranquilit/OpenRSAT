unit umoduleaddns;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.base,
  mormot.net.ldap,
  umodule,
  umoduleaddnsoption,
  uoption;

type

  { TModuleADDNS }

  TModuleADDNS = class(TModule)
  private
    fEnabled: Boolean;
    fOption: TModuleADDNSOption;
    fLdapClient: TLdapClient;

    fSearchResult: TLdapResultList;
  public
    constructor Create(LdapClient: TLdapClient); reintroduce;
    destructor Destroy; override;

    function GetZoneInfo(DistinguishedName: RawUtf8): TLdapResult;
    function GetZoneNodes(DistinguishedName: RawUtf8): TLdapResultList;
    function GetNodeInfo(DistinguishedName: RawUtf8): TLdapResult;

    /// TModule
  protected
    function GetEnabled: Boolean; override;
    procedure SetEnabled(AValue: Boolean); override;
    function GetName: RawUtf8; override;
    function GetDisplayName: RawUtf8; override;
    function GetOption: TOption; override;
  end;

implementation
uses
  mormot.core.text,
  ucommon;

{ TModuleADDNS }

constructor TModuleADDNS.Create(LdapClient: TLdapClient);
begin
  fEnabled := True;
  fOption := TModuleADDNSOption.Create;
  fLdapClient := LdapClient;

  fSearchResult := TLdapResultList.Create;
end;

destructor TModuleADDNS.Destroy;
begin
  FreeAndNil(fOption);
  FreeAndNil(fSearchResult);

  inherited Destroy;
end;

function TModuleADDNS.GetZoneInfo(DistinguishedName: RawUtf8): TLdapResult;
begin
  result := nil;
  if not Assigned(fLdapClient) then
    Exit;
  result := fLdapClient.SearchObject(DistinguishedName, '', ['*']);
end;

function TModuleADDNS.GetZoneNodes(DistinguishedName: RawUtf8
  ): TLdapResultList;
var
  Item, NewItem: TLdapResult;
  Attr, NewAttr: TLdapAttribute;
  i: Integer;
begin
  result := nil;
  if not Assigned(fLdapClient) then
    Exit;

  fSearchResult.Clear;
  fLdapClient.SearchBegin();
  try
    fLdapClient.SearchScope := lssSingleLevel;
    repeat
      if not fLdapClient.Search(DistinguishedName, False, '', ['*']) then
        raise Exception.Create(FormatUtf8('Ldap Search Failed: %', [fLdapClient.ResultString]));

      for Item in fLdapClient.SearchResult.Items do
      begin
        if not Assigned(Item) then
          continue;
        NewItem := fSearchResult.Add;
        NewItem.ObjectName := Item.ObjectName;
        for Attr in Item.Attributes.Items do
        begin
          if not Assigned(Attr) then
            continue;
          NewAttr := NewItem.Attributes.Add(Attr.AttributeName);
          for i := 0 to Pred(Attr.Count) do
            NewAttr.Add(Attr.GetRaw(i));
          NewAttr.AfterAdd;
        end;
      end;
      fSearchResult.AfterAdd;
    until fLdapClient.SearchCookie = '';
  finally
    fLdapClient.SearchEnd;
  end;
  result := fSearchResult;
end;

function TModuleADDNS.GetNodeInfo(DistinguishedName: RawUtf8): TLdapResult;
begin
  result := nil;
  if not Assigned(fLdapClient) then
    Exit;
end;

function TModuleADDNS.GetEnabled: Boolean;
begin
  result := fEnabled;
end;

procedure TModuleADDNS.SetEnabled(AValue: Boolean);
begin
  if AValue = fEnabled then
    Exit;
end;

function TModuleADDNS.GetName: RawUtf8;
begin
  result := rsModuleDNSName;
end;

function TModuleADDNS.GetDisplayName: RawUtf8;
begin
  result := rsModuleDNSDisplayName;
end;

function TModuleADDNS.GetOption: TOption;
begin
  result := fOption;
end;

end.

