unit uvisopenrsat;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  Menus,
  ComCtrls,
  ActnList,
  LResources,
  IniPropStorage,
  ufrmcore,
  {$IFDEF WINDOWS} // Adds dark mode support, must be included after the LCL widgetset
    uDarkStyleParams,
    uMetaDarkStyle,
    uDarkStyleSchemes,
  {$ENDIF}
  mormot.core.log;

type

  { TVisRsatConsole }

  { TVisOpenRSAT }

  TVisOpenRSAT = class(TForm)
    Action_NewWindow: TAction;
    Action_OpenedProperties: TAction;
    ActionList: TActionList;
    Action_Quit: TAction;
    IniPropStorage1: TIniPropStorage;
    MainMenu: TMainMenu;
    MenuItem_RetrieveOldConfiguration: TMenuItem;
    MenuItem_AdvancedFeatures: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem_DEBUG: TMenuItem;
    MenuItem_File: TMenuItem;
    MenuItem_FileOptions: TMenuItem;
    MenuItem_FileQuit: TMenuItem;
    MenuItem_Help: TMenuItem;
    MenuItem_LdapConnect: TMenuItem;
    MenuItem_LdapConnectOption: TMenuItem;
    MenuItem_LdapDisconnect: TMenuItem;
    MenuItem_ShowGPO: TMenuItem;
    MenuItem_View: TMenuItem;
    MenuItem_ViewTheme: TMenuItem;
    MenuItem_ViewThemeDark: TMenuItem;
    MenuItem_ViewThemeLight: TMenuItem;
    MenuItem_ViewThemeSystem: TMenuItem;
    MenuItem_ViewWindows: TMenuItem;
    Separator4: TMenuItem;
    Separator_File: TMenuItem;
    procedure Action_NewWindowExecute(Sender: TObject);
    procedure Action_OpenedPropertiesUpdate(Sender: TObject);
    procedure Action_QuitExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItem_ViewThemeDarkClick(Sender: TObject);
    procedure MenuItem_ViewThemeLightClick(Sender: TObject);
    procedure MenuItem_ViewThemeSystemClick(Sender: TObject);
  private
    fFrmCore: TFrmCore;
    fLog: TSynLog;

    procedure RestoreOldConfig;
    procedure UpdateViewThemeButtons;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure ViewMenuItemClick(Sender: TObject);

  end;

var
  VisOpenRSAT: TVisOpenRSAT;

implementation
uses
  process,
  mormot.core.base,
  mormot.core.text,
  ufrmrsatoptions;

{$R *.lfm}

{ TVisRsatConsole }

procedure TVisOpenRSAT.Action_QuitExecute(Sender: TObject);
begin
  Close;
end;

procedure TVisOpenRSAT.Action_OpenedPropertiesUpdate(Sender: TObject);
var
  AName: String;
  i: Integer;
  NewMenuItem: TMenuItem;
  found: Boolean;
  ItemsFound: Array of TMenuItem;

  function Contains(Items: Array of TMenuItem; Item: TMenuItem): Boolean;
  var
    v: TMenuItem;
  begin
    result := True;
    for v in Items do
      if v = Item then
        Exit;
    result := False;
  end;

begin
  if fFrmCore.VisPropertiesList.Count = MenuItem_ViewWindows.Count then
    Exit;
  ItemsFound := [];

  for AName in fFrmCore.VisPropertiesList.GetNames do
  begin
    found := False;
    for i := 0 to MenuItem_ViewWindows.Count - 1 do
    begin
      Found := (MenuItem_ViewWindows.Items[i].Caption = AName);
      if Found then
      begin
        Insert(MenuItem_ViewWindows.Items[i], ItemsFound, 0);
        Break;
      end;
    end;
    if Found then
      continue;
    NewMenuItem := TMenuItem.Create(Self);
    NewMenuItem.Caption := AName;
    NewMenuItem.OnClick := @ViewMenuItemClick;
    NewMenuitem.ImageIndex := -1;
    MenuItem_ViewWindows.Add(NewMenuItem);
    Insert(NewMenuItem, ItemsFound, 0);
  end;
  i := MenuItem_ViewWindows.Count - 1;
  while i >= 0 do
  begin
    if not Contains(ItemsFound, MenuItem_ViewWindows.Items[i]) then
      MenuItem_ViewWindows.Remove(MenuItem_ViewWindows.Items[i]);
    Dec(i);
  end;

end;

procedure TVisOpenRSAT.Action_NewWindowExecute(Sender: TObject);
var
  CheminExecutable: String;
begin
  CheminExecutable := ParamStr(0);

  with TProcess.Create(nil) do
  try
    Executable := CheminExecutable;
    Execute;
  finally
    Free;
  end;
end;

procedure TVisOpenRSAT.FormActivate(Sender: TObject);
begin
  fFrmCore.Activate;
end;

procedure TVisOpenRSAT.FormDeactivate(Sender: TObject);
begin
  fFrmCore.Deactivate;
end;

procedure TVisOpenRSAT.FormPaint(Sender: TObject);
begin
  fFrmCore.Refresh;
end;

procedure TVisOpenRSAT.FormShow(Sender: TObject);
begin
  fFrmCore.Load;
  MenuItem_LdapConnect.Action := fFrmCore.Action_LdapConnect;
  MenuItem_LdapConnectOption.Action := fFrmCore.Action_LdapOptions;
  MenuItem_LdapDisconnect.Action := fFrmCore.Action_LdapDisconnect;
  MenuItem_FileOptions.Action := fFrmCore.Action_Options;
  MenuItem_AdvancedFeatures.Action := fFrmCore.Action_AdvancedFeatures;
  MakeFullyVisible();
  Activate;

  fFrmCore.Timer_AutoConnect.Enabled := fFrmCore.LdapConfigs.AutoConnect;
  UpdateViewThemeButtons;
  IniPropStorage1.Restore;
end;

procedure TVisOpenRSAT.MenuItem_ViewThemeDarkClick(Sender: TObject);
begin
  fFrmCore.RsatOptions.Theme := tmDark;
  UpdateViewThemeButtons;
end;

procedure TVisOpenRSAT.MenuItem_ViewThemeLightClick(Sender: TObject);
begin
  fFrmCore.RsatOptions.Theme := tmLight;
  UpdateViewThemeButtons;
end;

procedure TVisOpenRSAT.MenuItem_ViewThemeSystemClick(Sender: TObject);
begin
  fFrmCore.RsatOptions.Theme := tmSystem;
  UpdateViewThemeButtons;
end;

procedure TVisOpenRSAT.RestoreOldConfig;
var
  Paths: TStringArray;
  oldPath: TFileName;
  newPath: String;
begin
  newPath := GetAppConfigDir(False);
  Paths := newPath.Split(PathDelim);
  if Paths[high(Paths)] = '' then
    Delete(Paths, high(Paths), 1);
  Paths[high(Paths)] := 'RsatConsole';
  oldPath := String.Join(PathDelim, Paths);
  oldPath := MakePath([oldPath]);
  if DirectoryExists(oldPath) then
  begin
    oldPath := IncludeTrailingPathDelimiter(oldPath);
    newPath := IncludeTrailingPathDelimiter(newPath);
    RenameFile(oldPath, newPath);
  end;
end;

procedure TVisOpenRSAT.UpdateViewThemeButtons;
begin
  MenuItem_ViewThemeDark.Checked := fFrmCore.RsatOptions.Theme = tmDark;
  MenuItem_ViewThemeLight.Checked := fFrmCore.RsatOptions.Theme = tmLight;
  MenuItem_ViewThemeSystem.Checked := fFrmCore.RsatOptions.Theme = tmSystem;
end;

constructor TVisOpenRSAT.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fLog := TSynLog.Add;

  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Create', [Self.Name]);

  // Setup OpenRSATCore
  fFrmCore := TFrmCore.Create(Self);
  fFrmCore.Parent := Self;
  fFrmCore.Align := alClient;

  // Setup theme for windows
  {$IFDEF WINDOWS}
  if fFrmCore.RsatOptions.Theme = tmDark then
    PreferredAppMode := pamForceDark
  else if fFrmCore.RsatOptions.Theme = tmLight then
    PreferredAppMode := pamForceLight
  else
    PreferredAppMode := pamAllowDark;
  uMetaDarkStyle.ApplyMetaDarkStyle(DefaultDark);
  {$ENDIF}

  // Check for old config
  if not DirectoryExists(GetAppConfigDir(False)) then
    RestoreOldConfig;

  IniPropStorage1.IniFileName := MakePath([GetAppConfigDir(False), 'OpenRSAT.ini']);
end;

destructor TVisOpenRSAT.Destroy;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Destroy', [Self.Name]);

  inherited Destroy;
end;

procedure TVisOpenRSAT.ViewMenuItemClick(Sender: TObject);
var
  i: Integer;
begin
  i := MenuItem_ViewWindows.IndexOf(sender as TMenuItem);
  if i = -1 then
    Exit;
  fFrmCore.VisPropertiesList.Focus(i);
end;

//initialization
//  {$I ../language.lrs}

end.

