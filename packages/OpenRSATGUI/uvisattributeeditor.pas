unit uvisattributeeditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ExtCtrls,
  StdCtrls,
  Buttons,
  ActnList,
  ComCtrls,
  DateTimePicker,
  mormot.core.variants,
  mormot.net.ldap,
  mormot.core.base,
  uinterfacecore;

type

  { TVisAttributeEditor }
  TLdapSyntax = (
    lsInvalid,
    lsBoolean,                    // 2.5.5.8  - 1
    lsEnumeration,                // 2.5.5.9  - 10
    lsInteger,                    // 2.5.5.9  - 2
    lsLargeInteger,               // 2.5.5.16 - 65
    lsObjectAccessPoint,          // 2.5.5.14 - 127
    lsObjectDNString,             // 2.5.5.14 - 127
    lsObjectORName,               // 2.5.5.7  - 127
    lsObjectDNBinary,             // 2.5.5.7  - 127
    lsObjectDSDN,                 // 2.5.5.1  - 127
    lsObjectPresentationAddress,  // 2.5.5.13 - 127
    lsObjectReplicaLink,          // 2.5.5.10 - 127
    lsStringCase,                 // 2.5.5.3  - 27
    lsStringIA5,                  // 2.5.5.5  - 22
    lsStringNTSecDesc,            // 2.5.5.15 - 66
    lsStringNumeric,              // 2.5.5.6  - 18
    lsStringObjectIdentifier,     // 2.5.5.2  - 6
    lsStringOctet,                // 2.5.5.10 - 4
    lsStringPrintable,            // 2.5.5.5  - 19
    lsStringSid,                  // 2.5.5.17 - 4
    lsStringTeletex,              // 2.5.5.4  - 20
    lsStringUnicode,              // 2.5.5.12 - 64
    lsStringUTCTime,              // 2.5.5.11 - 23
    lsStringGenerelizedTime       // 2.5.5.11 - 24
  );

  TVisAttributeEditor = class(TForm)
    Action_Up: TAction;
    Action_Down: TAction;
    Action_OK: TAction;
    Action_Cancel: TAction;
    Action_Add: TAction;
    Action_Delete: TAction;
    ActionList1: TActionList;
    BitBtn_Up: TBitBtn;
    BitBtn_Down: TBitBtn;
    BitBtn_Add: TBitBtn;
    BitBtn_Delete: TBitBtn;
    BitBtn_OK: TBitBtn;
    BitBtn_Cancel: TBitBtn;
    DateTimePicker1: TDateTimePicker;
    Edit1: TEdit;
    Edit_String: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label_StringList: TLabel;
    Label_String: TLabel;
    ListBox_StringList: TListBox;
    PageControl1: TPageControl;
    Panel_Bottom: TPanel;
    TS_TimeEditor: TTabSheet;
    TS_MultiTimeEditor: TTabSheet;
    TS_StringEditor: TTabSheet;
    TS_MultiStringEditor: TTabSheet;
    procedure Action_AddExecute(Sender: TObject);
    procedure Action_AddUpdate(Sender: TObject);
    procedure Action_CancelUpdate(Sender: TObject);
    procedure Action_DeleteExecute(Sender: TObject);
    procedure Action_DeleteUpdate(Sender: TObject);
    procedure Action_DownExecute(Sender: TObject);
    procedure Action_DownUpdate(Sender: TObject);
    procedure Action_OKUpdate(Sender: TObject);
    procedure Action_UpExecute(Sender: TObject);
    procedure Action_UpUpdate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    fData: TLdapAttribute;
    fAttributeName: RawUtf8;
    fAttr: TLdapAttribute;
    fCore: ICore;

    procedure EditString(isSingleValue: Boolean);
    procedure EditTime(isSingleValue: Boolean);
    function GetAttr: TLdapAttribute;

    procedure GetAttrSingleString;
    procedure GetAttrMultiString;
    procedure GetAttrSingleTime;
    procedure GetAttrMultiTime;
  public
    constructor Create(TheOwner: TComponent; ACore: ICore; Data: TLdapAttribute; AttributeName: RawUtf8); reintroduce;
    destructor Destroy; override;

    property Attr: TLdapAttribute read GetAttr;
  end;

  function TLdapSyntaxFromString(AttributeSyntax, oMSyntax, oMObjectClass: RawUtf8): TLdapSyntax;

implementation

uses
  mormot.core.text,
  mormot.core.datetime,
  mormot.core.os,
  ucommon,
  ucommonui;

function TLdapSyntaxFromString(AttributeSyntax, oMSyntax, oMObjectClass: RawUtf8): TLdapSyntax;
begin
  case AttributeSyntax of
  '2.5.5.8': result := lsBoolean;
  '2.5.5.9':
    case oMSyntax of
      '10': result := lsEnumeration;
      '2': result := lsInteger;
      else
        result := lsInvalid;
    end;
  '2.5.5.16': result := lsLargeInteger;
  '2.5.5.14':
    case oMObjectClass of
      '1.3.12.2.1011.28.0.702': result := lsObjectAccessPoint;
      '1.2.840.113556.1.1.1.12': result := lsObjectDNString;
      else
        result := lsInvalid;
    end;
  '2.5.5.7':
    case oMObjectClass of
      '2.6.6.1.2.5.11.29': result := lsObjectORName;
      '1.2.840.113556.1.1.1.11': result := lsObjectDNBinary;
      else
        result := lsInvalid;
    end;
  '2.5.5.1': result := lsObjectDSDN;
  '2.5.5.13': result := lsObjectPresentationAddress;
  '2.5.5.3': result := lsStringCase;
  '2.5.5.5':
    case oMSyntax of
      '22': result := lsStringIA5;
      '19': result := lsStringPrintable;
      else
        result := lsInvalid;
    end;
  '2.5.5.15': result := lsStringNTSecDesc;
  '2.5.5.6': result := lsStringNumeric;
  '2.5.5.2': result := lsStringObjectIdentifier;
  '2.5.5.10':
    case oMSyntax of
      '127': result := lsObjectReplicaLink;
      '4': result := lsStringOctet;
      else
        result := lsInvalid;
    end;
  '2.5.5.17': result := lsStringSid;
  '2.5.5.4': result := lsStringTeletex;
  '2.5.5.12': result := lsStringUnicode;
  '2.5.5.11':
    case oMSyntax of
      '23': result := lsStringUTCTime;
      '24': result := lsStringGenerelizedTime;
      else
        result := lsInvalid;
    end;
  else
    result := lsInvalid;
  end;
end;

{$R *.lfm}

{ TVisAttributeEditor }

procedure TVisAttributeEditor.Action_AddExecute(Sender: TObject);
begin
  ListBox_StringList.Items.Add(Edit1.Text);
  ListBox_StringList.Update;
end;

procedure TVisAttributeEditor.Action_AddUpdate(Sender: TObject);
begin
  Action_Add.Enabled := Edit1.Text <> '';
end;

procedure TVisAttributeEditor.Action_CancelUpdate(Sender: TObject);
begin
  Action_Cancel.Enabled := True;
end;

procedure TVisAttributeEditor.Action_DeleteExecute(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to ListBox_StringList.Count - 1 do
  begin
    if ListBox_StringList.Selected[i] then
    begin
      ListBox_StringList.Items.Delete(i);
      break;
    end;
  end;
end;

procedure TVisAttributeEditor.Action_DeleteUpdate(Sender: TObject);
begin
  Action_Delete.Enabled := ListBox_StringList.SelCount > 0;
end;

procedure TVisAttributeEditor.Action_DownExecute(Sender: TObject);
var
  i: Integer;
  item: String;
begin
  for i := 0 to ListBox_StringList.Count - 1 do
  begin
    if (ListBox_StringList.Selected[i] and (i < ListBox_StringList.Count - 1)) then
    begin
      item := ListBox_StringList.Items[i];
      ListBox_StringList.Items.Delete(i);
      ListBox_StringList.Items.Insert(i + 1, item);
      ListBox_StringList.ClearSelection;
      ListBox_StringList.Selected[i + 1] := True;
      break;
    end;
  end;
end;

procedure TVisAttributeEditor.Action_DownUpdate(Sender: TObject);
begin
  Action_Down.Enabled := ListBox_StringList.SelCount = 1;
end;

procedure TVisAttributeEditor.Action_OKUpdate(Sender: TObject);
begin
  Action_OK.Enabled := True;
end;

procedure TVisAttributeEditor.Action_UpExecute(Sender: TObject);
var
  item: String;
  i: Integer;
begin
  for i := 0 to ListBox_StringList.Count - 1 do
  begin
    if (ListBox_StringList.Selected[i] and (i > 0)) then
    begin
      item := ListBox_StringList.Items[i];
      ListBox_StringList.Items.Delete(i);
      ListBox_StringList.Items.Insert(i - 1, item);
      ListBox_StringList.ClearSelection;
      ListBox_StringList.Selected[i - 1] := True;
      break;
    end;
  end;
end;

procedure TVisAttributeEditor.Action_UpUpdate(Sender: TObject);
begin
  Action_Up.Enabled := ListBox_StringList.SelCount = 1;
end;

procedure TVisAttributeEditor.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    27: Close;
  end;
end;

procedure TVisAttributeEditor.EditString(isSingleValue: Boolean);
var
  value: RawUtf8;
begin
  if isSingleValue then
  begin
    PageControl1.ActivePage := TS_StringEditor;
    Edit_String.Text := fData.GetReadable();
  end
  else
  begin
    PageControl1.ActivePage := TS_MultiStringEditor;
    ListBox_StringList.Items.BeginUpdate;
    try
      for Value in fData.GetAllReadable do
        ListBox_StringList.Items.Add(Value);
    finally
      ListBox_StringList.Items.EndUpdate;
      ListBox_StringList.Update;
    end;
  end;
end;

procedure TVisAttributeEditor.EditTime(isSingleValue: Boolean);
begin
  if isSingleValue then
  begin
    PageControl1.ActivePage := TS_TimeEditor;
    DateTimePicker1.DateTime := Iso8601ToDateTime(fData.GetReadable());
  end
  else
  begin
    PageControl1.ActivePage := TS_MultiTimeEditor;

  end;
end;

function TVisAttributeEditor.GetAttr: TLdapAttribute;
var
  CustomGetAttr: Array of Procedure of Object;
begin
  if not Assigned(fAttr) then
    fAttr := TLdapAttribute.Create(fAttributeName, AttributeNameType(fAttributeName));
  fAttr.Clear;

  SetLength(CustomGetAttr, PageControl1.PageCount);

  CustomGetAttr[TS_StringEditor.PageIndex] := @GetAttrSingleString;
  CustomGetAttr[TS_MultiStringEditor.PageIndex] := @GetAttrMultiString;
  CustomGetAttr[TS_TimeEditor.PageIndex] := @GetAttrSingleTime;
  CustomGetAttr[TS_MultiTimeEditor.PageIndex] := @GetAttrMultiTime;

  CustomGetAttr[PageControl1.PageIndex];
  result := fAttr;
end;

procedure TVisAttributeEditor.GetAttrSingleString;
begin
  Caption := rsSingleStringEditor;
  fAttr.Add(Edit_String.Text);
end;

procedure TVisAttributeEditor.GetAttrMultiString;
var
  Value: String;
begin
  Caption := rsMultiStringEditor;
  for Value in ListBox_StringList.Items do
    fAttr.Add(Value);
end;

procedure TVisAttributeEditor.GetAttrSingleTime;
var
  d: RawUtf8;
begin
  Caption := rsSingleTimeEditor;
  d := DateTimeToIso8601(DateTimePicker1.DateTime, False);
  fAttr.add(d);
end;

procedure TVisAttributeEditor.GetAttrMultiTime;
begin
  Caption := rsMultiTimeEditor;
end;

constructor TVisAttributeEditor.Create(TheOwner: TComponent; ACore: ICore;
  Data: TLdapAttribute; AttributeName: RawUtf8);
var
  attributeSyntax, oMSyntax, oMObjectClass: RawUtf8;
  syntax: TLdapSyntax;
  isSingleValue: Boolean;
  Attributes: TLdapResult;
begin
  inherited Create(TheOwner);
  fAttributeName := AttributeName;
  if Assigned(Data) then
    fData := TLdapAttribute(Data.Clone)
  else
    fData := TLdapAttribute.Create(fAttributeName, atUndefined);

  fCore := ACore;

  PageControl1.ShowTabs := False;

  Attributes := fCore.LdapClient.SearchObject(
    FormatUtf8('CN=Schema,%', [fcore.LdapClient.ConfigDN]),
    FormatUtf8('(lDAPDisplayName=%)', [LdapEscape(fData.AttributeName)]),
    ['attributeSyntax', 'oMSyntax', 'oMObjectClass', 'isSingleValued'],
    lssSingleLevel
  );

  attributeSyntax := Attributes.Find('attributeSyntax').GetReadable();

  oMSyntax := Attributes.Find('oMSyntax').GetReadable();

  oMObjectClass := Attributes.Find('oMObjectClass').GetReadable();

  case Attributes.Find('isSingleValued').GetReadable() of
    'TRUE': isSingleValue := True;
    'FALSE': isSingleValue := False;
    else
      isSingleValue := True;
  end;


  syntax := TLdapSyntaxFromString(attributeSyntax, oMSyntax, oMObjectClass);

  case syntax of
    lsStringCase,
    lsStringIA5,
    lsStringNTSecDesc,
    lsStringNumeric,
    lsStringObjectIdentifier,
    lsStringOctet,
    lsStringPrintable,
    lsStringSid,
    lsStringTeletex,
    lsStringUnicode: EditString(isSingleValue);
    lsStringUTCTime,
    lsStringGenerelizedTime: EditTime(isSingleValue);
    else
      EditString(isSingleValue);
  end;

  UnifyButtonsWidth([BitBtn_OK, BitBtn_Cancel]);
  UnifyButtonsWidth([BitBtn_Add, BitBtn_Delete, BitBtn_Down, BitBtn_Up]);
end;

destructor TVisAttributeEditor.Destroy;
begin
  FreeAndNil(fData);
  FreeAndNil(fAttr);
  inherited Destroy;
end;

end.

