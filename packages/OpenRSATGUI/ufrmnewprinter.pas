unit ufrmnewprinter;

{$mode ObjFPC}{$H+}

interface

uses
  ActnList,
  Classes,
  Controls,
  Forms,
  StdCtrls,
  ExtCtrls,
  SysUtils,
  mormot.core.base;

type 
  { TFrmNewPrinter }
  TFrmNewPrinter = class(TFrame)
    Action_Next: TAction;
    ActionList: TActionList;
    Edit_NetworkPath: TEdit;
    Label_NetworkPath: TLabel;
    Panel1: TPanel;
    procedure Action_NextExecute(Sender: TObject);
    procedure Action_NextUpdate(Sender: TObject);
  private
    function UNCToDashName(const UNC: RawUtf8): RawUtf8;
    function GetServerName(const UNC: string): string;
    function GetShareName(const UNC: string): string;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

implementation

uses
  Dialogs,
  Graphics,
  mormot.core.text,
  mormot.net.ldap,
  ucommon,
  ursatldapclientui,
  uvisnewobject;

{$R *.lfm}

function TFrmNewPrinter.UNCToDashName(const UNC: RawUtf8): RawUtf8;
var
  s: RawUtf8;
  p: Integer;
begin
  Result := '';
  if (Length(UNC) < 3) or (Copy(UNC, 1, 2) <> '\\') then
    Exit('');

  s := Copy(UNC, 3, MaxInt);
  p := Pos('\', s);
  if p = 0 then
    exit('');

  Result := Copy(s, 1, p - 1) + '-' + Copy(s, p + 1, MaxInt);
end;

function TFrmNewPrinter.GetServerName(const UNC: string): string;
var
  p: Integer;
begin
  if (Length(UNC) < 3) or (Copy(UNC, 1, 2) <> '\\') then
    Exit('');

  p := PosEx('\', UNC, 3);
  if p = 0 then
    Result := Copy(UNC, 3, MaxInt)
  else
    Result := Copy(UNC, 3, p - 3);
end;

function TFrmNewPrinter.GetShareName(const UNC: string): string;
var
  p: Integer;
begin
  if (Length(UNC) < 3) or (Copy(UNC, 1, 2) <> '\\') then
    Exit('');

  p := PosEx('\', UNC, 3);
  if p = 0 then
    Exit('')
  else
    Result := Copy(UNC, p + 1, MaxInt);
end;

procedure TFrmNewPrinter.Action_NextExecute(Sender: TObject);
var
  VisNewObject: TVisNewObject;
  AttrList: TLdapAttributeList;
  Att: TLdapAttribute;
  DN: RawUtf8;
begin
  VisNewObject := (owner as TVisNewObject);

  AttrList := TLdapAttributeList.Create();
  try
    Att := AttrList.Add('objectClass', 'top');
    Att.Add('leaf');
    Att.Add('connectionPoint');
    Att.Add('printQueue');

    if Edit_NetworkPath.Text = '' then
      exit;

    AttrList.Add('uNCName', Edit_NetworkPath.Text);
    AttrList.Add('serverName', GetServerName(Edit_NetworkPath.Text));
    AttrList.Add('shortServerName', GetServerName(Edit_NetworkPath.Text));
    AttrList.Add('versionNumber', '0');
    AttrList.Add('printShareName', GetShareName(Edit_NetworkPath.Text));
    AttrList.Add('printerName', GetShareName(Edit_NetworkPath.Text));

    DN := FormatUtf8('CN=%,%', [UNCToDashName(Edit_NetworkPath.Text), VisNewObject.ObjectOU]);
    if not VisNewObject.Ldap.Add(DN, AttrList) then
      Exit;
  finally
    FreeAndNil(AttrList);
  end;

  VisNewObject.ModalResult := mrOK; 
end;

procedure TFrmNewPrinter.Action_NextUpdate(Sender: TObject);
begin
  Action_Next.Enabled := (Edit_NetworkPath.Text <> '')
end;

constructor TFrmNewPrinter.Create(TheOwner: TComponent);
var
  OwnerNewObject: TVisNewObject absolute TheOwner;
begin
  inherited Create(TheOwner);

  OwnerNewObject.Caption := rsNewObjectPrinter;
  OwnerNewObject.Btn_Next.Action := ActionList.ActionByName('Action_Next');
  OwnerNewObject.Btn_Next.Caption := rsNewObjectBtnOK;
  OwnerNewObject.Btn_Next.Default := True;
  OwnerNewObject.Btn_Back.Visible := False;
  OwnerNewObject.Image_Object.ImageIndex := -1;
end;

end.

