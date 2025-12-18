unit ugplink;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.base;

type
  TGPLink = record
    Link: RawUtf8;
    DistinguishedName: RawUtf8;
    Flag: Integer;
  end;

  TGPLinkDynArr = Array of TGPLink;

operator=(Destination, Source: TGPLink): Boolean;
function GPLinkToGPLinkArr(GPLink: RawUtf8): TGPLinkDynArr;
function GPLinkArrToGPLink(GPLinkArr: TGPLinkDynArr): RawUtf8;

implementation
uses
  mormot.core.text;

operator=(Destination, Source: TGPLink): Boolean;
begin
  result := (Destination.Link = Source.Link);
end;

function GPLinkToGPLinkArr(GPLink: RawUtf8): TGPLinkDynArr;
var
  LinkStart, LinkEnd, Count: Integer;
  Link: TGPLink;
  LinkArr: TStringArray;
begin
  result := nil;
  LinkStart := 0;
  LinkEnd := 0;
  Count := 0;

  while GPLink <> '' do
  begin
    LinkStart := String(GPLink).IndexOf('[LDAP://', LinkEnd);
    if LinkStart < 0 then
      break;
    LinkEnd := String(GPLink).IndexOf(']', LinkStart);

    Link.Link := String(GPLink).Substring(LinkStart + 8, LinkEnd - 8 - LinkStart);
    LinkArr := String(Link.Link).Split(';');
    if Length(LinkArr) <> 2 then
      raise Exception.Create('Wrong GPLink format');
    Link.DistinguishedName := LinkArr[0];
    if not TryStrToInt(LinkArr[1], Link.Flag) then
      raise Exception.Create('GPLink flag is not a number');

    Insert(Link, result, Count);
    Inc(Count);
  end;
end;

function GPLinkArrToGPLink(GPLinkArr: TGPLinkDynArr): RawUtf8;
var
  GPLink: TGPLink;
begin
  result := '';
  for GPLink in GPLinkArr do
    result := FormatUtf8('%[LDAP://%;%]', [result, GPLink.DistinguishedName, GPLink.Flag]);
end;

end.

