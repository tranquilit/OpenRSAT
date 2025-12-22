unit uvisprofilemanager;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils, Types,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ComCtrls,
  Menus,
  ActnList,
  ExtCtrls,
  Buttons, IniPropStorage,
  IniFiles,
  mormot.core.base,
  mormot.core.text,
  uldapconfigs;

type

  { TVisProfileManager }

  TVisProfileManager = class(TForm)
    Action_Add: TAction;
    Action_Delete: TAction;
    Action_Edit: TAction;
    ActionList1: TActionList;
    BitBtn_OK: TBitBtn;
    IniPropStorage1: TIniPropStorage;
    ListView1: TListView;
    MenuItem_SmallIcons: TMenuItem;
    MenuItem_LargeIcons: TMenuItem;
    MenuItem_List: TMenuItem;
    MenuItem_Table: TMenuItem;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    ToolBar1: TToolBar;
    ToolButton_Add: TToolButton;
    ToolButton_Delete: TToolButton;
    ToolButton_Edit: TToolButton;
    ToolButton_SwitchListView: TToolButton;
    procedure Action_AddExecute(Sender: TObject);
    procedure Action_DeleteExecute(Sender: TObject);
    procedure Action_DeleteUpdate(Sender: TObject);
    procedure Action_EditExecute(Sender: TObject);
    procedure Action_EditUpdate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListView1Edited(Sender: TObject; Item: TListItem;
      var AValue: string);
    procedure ListView1Resize(Sender: TObject);
    procedure MenuItem_LargeIconsClick(Sender: TObject);
    procedure MenuItem_ListClick(Sender: TObject);
    procedure MenuItem_SmallIconsClick(Sender: TObject);
    procedure MenuItem_TableClick(Sender: TObject);
  private
    fIniFile: TIniFile;
    fLdapConfigs: TLdapConfigs;
  public
    constructor Create(TheOwner: TComponent; ALdapConfigs: TLdapConfigs);
      reintroduce;
    destructor Destroy; override;

    procedure SwitchListView(ViewStyle: TViewStyle);
    procedure LoadProfiles(ForceUpdate: Boolean = True);
    procedure LoadProfile(Section: RawUtf8);
  end;

implementation
uses
  uvisconnectoptions,
  ucommon,
  uconfig;

{$R *.lfm}

{ TVisProfileManager }

procedure TVisProfileManager.MenuItem_TableClick(Sender: TObject);
begin
  SwitchListView(vsReport);
end;

procedure TVisProfileManager.Action_AddExecute(Sender: TObject);
var
  ProfileEditor: TVisConnectOptions;
  SectionName: RawUtf8;
  Count: Integer;
begin
  ProfileEditor := TVisConnectOptions.Create(Self);
  try
    ProfileEditor.Settings := fLdapConfigs.LdapConnectionSettings;
    if ProfileEditor.ShowModal <> mrOK then
      Exit;
    Count := 0;
    SectionName := ProfileEditor.Edit_DomainController.Caption;
    while fIniFile.SectionExists(SectionName) do
    begin
      Inc(Count);
      SectionName := FormatUtf8('%_%', [ProfileEditor.Edit_DomainController.Caption, Count]);
    end;
    fLdapConfigs.SaveConfig(SectionName, fLdapConfigs.LdapConnectionSettings);
    LoadProfiles();
  finally
    FreeAndNil(ProfileEditor);
  end;
end;

procedure TVisProfileManager.Action_DeleteExecute(Sender: TObject);
begin
  if not Assigned(ListView1.Selected) then
    Exit;

  if mrYes <> MessageDlg(rsConfirmation, FormatUtf8(rsDeleteProfile, [ListView1.Selected.Caption]), mtWarning, mbYesNoCancel, 0) then
    Exit;

  fIniFile.EraseSection(ListView1.Selected.Caption);
  LoadProfiles;
end;

procedure TVisProfileManager.Action_DeleteUpdate(Sender: TObject);
begin
  Action_Delete.Enabled := Assigned(ListView1.Selected);
end;

procedure TVisProfileManager.Action_EditExecute(Sender: TObject);
var
  ProfileEditor: TVisConnectOptions;
begin
  ProfileEditor := TVisConnectOptions.Create(Self);
  try
    fLdapConfigs.LoadConfig(ListView1.Selected.Caption);
    ProfileEditor.Settings := fLdapConfigs.LdapConnectionSettings;
    if ProfileEditor.ShowModal <> mrOK then
      Exit;
    fLdapConfigs.SaveConfig(ListView1.Selected.Caption, fLdapConfigs.LdapConnectionSettings);
    LoadProfiles;
  finally
    FreeAndNil(ProfileEditor);
  end;
end;

procedure TVisProfileManager.Action_EditUpdate(Sender: TObject);
begin
  Action_Edit.Enabled := Assigned(ListView1.Selected);
end;

procedure TVisProfileManager.FormShow(Sender: TObject);
begin
  SwitchListView(ListView1.ViewStyle);
end;

procedure TVisProfileManager.ListView1Edited(Sender: TObject; Item: TListItem;
  var AValue: string);
  procedure CopyStrings(Ini: TIniFile; Src, Dst: RawUtf8; Keys: TRawUtf8DynArray; Default: RawUtf8 = '');
  var
    Key: RawUtf8;
  begin
    for Key in Keys do
      Ini.WriteString(Dst, Key, Ini.ReadString(Src, Key, Default));
  end;
  procedure CopyInts(Ini: TIniFile; Src, Dst: RawUtf8; Keys: TRawUtf8DynArray; Default: Integer = 0);
  var
    Key: RawUtf8;
  begin
    for Key in Keys do
      Ini.WriteInteger(Dst, Key, Ini.ReadInteger(Src, Key, Default));
  end;

begin
  if fIniFile.SectionExists(AValue) then
  begin
    AValue := Item.Caption;
    Exit;
  end;
  CopyStrings(fIniFile, Item.Caption, AValue, ['TargetHost', 'Username', 'KerberosDN', 'KerberosSpn']);
  CopyInts(fIniFile, Item.Caption, AValue, ['TargetPort', 'Tls', 'AllowUnsafePasswordBind', 'AutoReconnect', 'AutoBind', 'Timeout', 'KerberosDisableChannelBinding', 'UseCredentials', 'SearchPageSize', 'SearchPageNumber']);
  fIniFile.EraseSection(Item.Caption);
end;

procedure TVisProfileManager.ListView1Resize(Sender: TObject);
begin
  LoadProfiles(False);
end;

procedure TVisProfileManager.MenuItem_LargeIconsClick(Sender: TObject);
begin
  SwitchListView(vsIcon);
end;

procedure TVisProfileManager.MenuItem_ListClick(Sender: TObject);
begin
  SwitchListView(vsList);
end;

procedure TVisProfileManager.MenuItem_SmallIconsClick(Sender: TObject);
begin
  SwitchListView(vsSmallIcon);
end;

constructor TVisProfileManager.Create(TheOwner: TComponent;
  ALdapConfigs: TLdapConfigs);
begin
  inherited Create(TheOwner);

  fLdapConfigs := ALdapConfigs;
  LoadProfiles;
  IniPropStorage1.IniFileName := VisBakFilePath;
end;

destructor TVisProfileManager.Destroy;
begin
  FreeAndNil(fIniFile);

  inherited Destroy;
end;

procedure TVisProfileManager.SwitchListView(ViewStyle: TViewStyle);
begin
  ListView1.ViewStyle := ViewStyle;
  LoadProfiles;
  MenuItem_Table.Checked := ViewStyle = vsReport;
  MenuItem_SmallIcons.Checked := ViewStyle = vsSmallIcon;
  MenuItem_List.Checked := ViewStyle = vsList;
  MenuItem_LargeIcons.Checked := ViewStyle = vsIcon;
end;

procedure TVisProfileManager.LoadProfiles(ForceUpdate: Boolean);
var
  Sections: TStringList;
  Section: RawUtf8;
begin
  if ForceUpdate and Assigned(fIniFile) then
    FreeAndNil(fIniFile);
  if not Assigned(fIniFile) then
    fIniFile := TIniFile.Create(ConfigFilePath);
  Sections := TStringList.Create;
  ListView1.Items.BeginUpdate;
  try
    ListView1.Clear;
    fIniFile.ReadSections(Sections);
    for Section in Sections do
      if Section <> 'global' then
        LoadProfile(Section);
  finally
    ListView1.Items.EndUpdate;
    FreeAndNil(Sections);
  end;
end;

procedure TVisProfileManager.LoadProfile(Section: RawUtf8);
var
  NewViewItem: TListItem;
begin
  if not fIniFile.SectionExists(Section) then
    Exit;

  NewViewItem := ListView1.Items.Add;
  NewViewItem.Caption := Section;
  NewViewItem.ImageIndex := 18;
  NewViewItem.SubItems.Add(fIniFile.ReadString(Section, 'KerberosDN', ''));
  NewViewItem.SubItems.Add(fIniFile.ReadString(Section, 'TargetHost', ''));
  NewViewItem.SubItems.Add(fIniFile.ReadString(Section, 'Username', ''));
end;

end.

