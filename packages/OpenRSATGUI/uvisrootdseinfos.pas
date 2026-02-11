unit uvisrootdseinfos;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs, StdCtrls, ExtCtrls, Grids, ComCtrls, Buttons,
  mormot.net.ldap, tis.ui.grid.core, ursatldapclient, uldapconfigs;

type

  { TVisRootDSEInfos }

  TVisRootDSEInfos = class(TForm)
    Edit_Domain: TEdit;
    Edit_DomainController: TEdit;
    Label1: TLabel;
    Label_Domain: TLabel;
    Label_DomainController: TLabel;
    Panel1: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    TisGrid1: TTisGrid;
    procedure FormShow(Sender: TObject);
  private
    fLdapSettings: TMLdapClientSettings;

    procedure ClearLdapInformation;
    procedure UpdateLdapInformation;
    procedure FillLdapInformation(LdapClient: TRsatLdapClient);
  public
    constructor Create(TheOwner: TComponent; ALdapSettings: TMLdapClientSettings);
      reintroduce;
  end;

implementation
uses
  mormot.core.base,
  mormot.core.text,
  mormot.core.variants,
  ucommon,
  ursatldapclientui;

{$R *.lfm}

{ TVisRootDSEInfos }

procedure TVisRootDSEInfos.FillLdapInformation(LdapClient: TRsatLdapClient);
var
  Item: TLdapAttribute;
  Value: RawUtf8;
  NewRow: TDocVariantData;
begin
  if not Assigned(LdapClient) or not Assigned(LdapClient.SearchResult) or (LdapClient.SearchResult.Count <> 1) then
    Exit;

  NewRow.Init();
  for Item in LdapClient.SearchResult.Items[0].Attributes.Items do
  begin
    if not Assigned(Item) then
      continue;

    for Value in Item.GetAllReadable do
    begin
      NewRow.AddValue('name', Item.AttributeName);
      NewRow.AddValue('value', Value);
      TisGrid1.Data.AddItem(NewRow);
      NewRow.Clear;
    end;
  end;
  TisGrid1.LoadData();
end;

procedure TVisRootDSEInfos.FormShow(Sender: TObject);
begin
  UpdateLdapInformation;
end;

procedure TVisRootDSEInfos.ClearLdapInformation;
begin
  Edit_Domain.Clear;
  Edit_DomainController.Clear;

  TisGrid1.Clear;
end;

procedure TVisRootDSEInfos.UpdateLdapInformation;
var
  LdapClient: TRsatLdapClient;
begin
  ClearLdapInformation;

  // Retrieve RootDSE information
  LdapClient := TRsatLdapClient.Create(fLdapSettings);
  try
    With LdapClient do
    begin
      // Server info
      Edit_Domain.Text := Settings.KerberosDN;
      Edit_DomainController.Text := Settings.TargetHost;
      Settings.AutoBind := lcbPlain;
      Settings.UserName := '';
      Settings.Password := '';
      Connect();
      SearchObject('', '', ['*']);
      FillLdapInformation(LdapClient);
    end;
  finally
    FreeAndNil(LdapClient);
  end;
end;

constructor TVisRootDSEInfos.Create(TheOwner: TComponent;
  ALdapSettings: TMLdapClientSettings);
begin
  inherited Create(TheOwner);

  fLdapSettings := ALdapSettings;
end;

end.

