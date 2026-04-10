unit ufrmpropertyunixattributes;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  StdCtrls,
  ExtCtrls,
  Dialogs,
  mormot.core.base,
  mormot.core.log,
  mormot.net.ldap,
  mormot.core.text,
  uproperty,
  upropertyframe;

type

  { TFrmPropertyUnixAttributes }

  TFrmPropertyUnixAttributes = class(TPropertyFrame)
    ComboBox_NisDomain: TComboBox;
    ComboBox_PrimaryGroup: TComboBox;
    Edit_Gid: TEdit;
    Edit_HomeDirectory: TEdit;
    Edit_LoginShell: TEdit;
    Edit_Uid: TEdit;
    Label1: TLabel;
    Label_Gid: TLabel;
    Label_InfoGroup: TLabel;
    Label_PrimaryGroup: TLabel;
    Label_HomeDirectory: TLabel;
    Label_LoginShell: TLabel;
    Label_Uid: TLabel;
    Label_InfoUser: TLabel;
    Panel_Info: TPanel;
    Panel_Gid: TPanel;
    Panel_OptionsGroup: TPanel;
    Panel_OptionsUser: TPanel;
    Panel_InfoGroup: TPanel;
    Panel_InfoUser: TPanel;
    Panel_NisDomain: TPanel;
    Panel_PrimaryGroup: TPanel;
    Panel_HomeDirectory: TPanel;
    Panel_LoginShell: TPanel;
    Panel_Uid: TPanel;
    procedure ComboBox_NisDomainChange(Sender: TObject);
    procedure ComboBox_PrimaryGroupChange(Sender: TObject);
    procedure Edit_GidChange(Sender: TObject);
    procedure Edit_HomeDirectoryChange(Sender: TObject);
    procedure Edit_LoginShellChange(Sender: TObject);
    procedure Edit_UidChange(Sender: TObject);

  private
    fLog: TSynLog;
    fProperty: TProperty;

    procedure SetupFields();
    procedure UpdateEditField(Attribute: RawUtf8; Field: TEdit);
    procedure UpdateComboBoxField(Attribute: RawUtf8; Field: TComboBox);

  public
    constructor Create(TheOwner: TComponent); override;
    procedure Update(Props: TProperty); override;

  end;

implementation

uses
  mormot.core.os.security,
  ucommon,
  uhelpersui,
  ursatldapclientui,
  uvislogonhours,
  uvislogonworkstation;

{$R *.lfm}

procedure TFrmPropertyUnixAttributes.ComboBox_NisDomainChange(Sender: TObject);
begin
  fProperty.Add('msSFU30NisDomain', FormatUtf8('%', [ComboBox_NisDomain.Text]));
end;

procedure TFrmPropertyUnixAttributes.ComboBox_PrimaryGroupChange(Sender: TObject);
begin
  fProperty.Add('primaryGroupID', FormatUtf8('%', [ComboBox_PrimaryGroup.Text]));
end;

procedure TFrmPropertyUnixAttributes.Edit_GidChange(Sender: TObject);
begin
  fProperty.Add('gidNumber', FormatUtf8('%', [Edit_Gid.Text]));
end;

procedure TFrmPropertyUnixAttributes.Edit_HomeDirectoryChange(Sender: TObject);
begin
  fProperty.Add('unixHomeDirectory', FormatUtf8('%', [Edit_HomeDirectory.Text]));
end;

procedure TFrmPropertyUnixAttributes.Edit_LoginShellChange(Sender: TObject);
begin
  fProperty.Add('loginShell', FormatUtf8('%', [Edit_LoginShell.Text]));
end;

procedure TFrmPropertyUnixAttributes.Edit_UidChange(Sender: TObject);
begin
  fProperty.Add('uid', FormatUtf8('%', [Edit_Uid.Text]));
end;

procedure TFrmPropertyUnixAttributes.SetupFields();
var
  ObjectClass: TRawUtf8DynArray;
  obj: RawUtf8;
begin
  UpdateComboBoxField('msSFU30NisDomain', ComboBox_NisDomain);
  ObjectClass := fProperty.GetAllReadable('objectClass');
  for obj in ObjectClass do;
  begin
    if obj = 'user' then
    begin
      Panel_InfoGroup.Visible := false;
      Panel_OptionsGroup.Visible := false;
      UpdateEditField('uid', Edit_Uid);
      UpdateEditField('loginShell', Edit_LoginShell);
      UpdateEditField('unixHomeDirectory', Edit_HomeDirectory);
      UpdateComboBoxField('primaryGroupID', ComboBox_PrimaryGroup);
    end
    else if obj = 'group' then
    begin
      Panel_InfoUser.Visible := false;
      Panel_OptionsUser.Visible := false;
      UpdateEditField('gidNumber', Edit_Gid);
    end;
  end;
end;

procedure TFrmPropertyUnixAttributes.UpdateEditField(Attribute: RawUtf8; Field: TEdit);
var
  AttributeValue: String;
begin
  AttributeValue := fProperty.GetReadable(Attribute, 0);
  if AttributeValue <> '' then
    Field.Text := AttributeValue;
end;

procedure TFrmPropertyUnixAttributes.UpdateComboBoxField(Attribute: RawUtf8; Field: TComboBox);
var
  AttributeValue: String;
begin
  AttributeValue := fProperty.GetReadable(Attribute, 0);
  if AttributeValue <> '' then
    Field.Text := AttributeValue;
end;

constructor TFrmPropertyUnixAttributes.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);

  Caption := 'UNIX Attributes';
end;

procedure TFrmPropertyUnixAttributes.Update(Props: TProperty);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Update', Self);

  fProperty := Props;

  SetupFields();
end;

end.

