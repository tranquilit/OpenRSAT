#define MyAppName "OpenRSAT"

#ifndef MyAppVersion
  #define MyAppVersion "0"
#endif

#define MyAppPublisher "Tranquil IT"
#define MyAppURL "https://www.tranquil.it/openrsat/"
#define MyAppExeName "OpenRSAT.exe"
#ifndef MyAppId
  #define MyAppId "{{00598442-B544-4E7E-9594-C3C66ED2F9CA}"
#endif

[Setup]
AppId={#MyAppId}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}

DefaultDirName={autopf}\{#MyAppName}
DefaultGroupName={#MyAppName}
DisableProgramGroupPage=yes

PrivilegesRequired=admin
ArchitecturesInstallIn64BitMode=x64compatible

OutputDir=.
OutputBaseFilename=OpenRSAT-Setup-{#MyAppVersion}
Compression=lzma2
SolidCompression=yes
WizardStyle=classic dynamic

UninstallDisplayIcon={app}\{#MyAppExeName}
CloseApplications=yes

LicenseFile=LICENSE.txt

PrivilegesRequiredOverridesAllowed=dialog

SetupIconFile=OpenRSAT.ico

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"
Name: "french"; MessagesFile: "compiler:Languages\French.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[Files]
Source: "OpenRSAT-windows-x64.exe"; DestDir: "{app}"; DestName: "{#MyAppExeName}"; Check: IsWin64; Flags: ignoreversion
Source: "OpenRSAT-windows-x86.exe"; DestDir: "{app}"; DestName: "{#MyAppExeName}"; Check: not IsWin64; Flags: ignoreversion

Source: "LICENSE.txt"; DestDir: "{app}"; Flags: ignoreversion skipifsourcedoesntexist

Source: "languages\*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs

[Icons]
Name: "{group}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"
Name: "{autoprograms}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"
Name: "{autodesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: desktopicon

[Run]
Filename: "{app}\{#MyAppExeName}"; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, '&', '&&')}}"; Flags: nowait postinstall skipifsilent

[UninstallDelete]
Type: filesandordirs; Name: "{app}\log"
Type: dirifempty; Name: "{app}"
