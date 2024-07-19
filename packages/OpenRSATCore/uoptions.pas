unit uoptions;

{$mode ObjFPC}{$H+}

interface

uses
  // Lazarus / fpc
  Classes,
  SysUtils,
  // Submodules
  mormot.core.base,
  mormot.core.text,
  tisinifiles;

type

  TConsoleMode = (
    cmUsersAndComputers,
    cmSitesAndServices
  );

  IOptionSection = interface
    procedure LoadOptions(f: TTisInifiles);
    procedure LoadOptions;

    procedure SaveOptions(f: TTisInifiles);
    procedure SaveOptions;

    property OptionsPath: RawUtf8;
    property Section: RawUtf8;
    property Updating: Integer;
  end;

  { TUsersAndComputersOptions }

  TUsersAndComputersOptions = class(TInterfacedObject, IOptionSection)
  private
    fOptionsPath: RawUtf8;
    fSection: RawUtf8;
    fUpdating: Integer;

    fSearchPageSize: Integer;
    fSearchPageNumber: Integer;

    fViewFilter: RawUtf8;

    fCustomTreeFilter: RawUtf8;
    fTreeObjectClass: TRawUtf8DynArray;

    procedure SetCustomTreeFilter(AValue: RawUtf8);
    procedure SetSearchPageNumber(AValue: Integer);
    procedure SetSearchPageSize(AValue: Integer);
    procedure SetTreeObjectClass(AValue: TRawUtf8DynArray);
    procedure SetViewFilter(AValue: RawUtf8);
  public
    constructor Create(OptionsPath: RawUtf8); overload;

    procedure LoadOptions(f: TTisInifiles);
    procedure LoadOptions;
    procedure SaveOptions(f: TTisInifiles);
    procedure SaveOptions;

    property OptionsPath: RawUtf8 read fOptionsPath write fOptionsPath;
    property Section: RawUtf8 read fSection write fSection;
    property Updating: Integer read fUpdating write fUpdating;
  published
    property SearchPageSize: Integer read fSearchPageSize write SetSearchPageSize;
    property SearchPageNumber: Integer read fSearchPageNumber write SetSearchPageNumber;

    property ViewFilter: RawUtf8 read fViewFilter write SetViewFilter;

    property CustomTreeFilter: RawUtf8 read fCustomTreeFilter write SetCustomTreeFilter;
    property TreeObjectClass: TRawUtf8DynArray read fTreeObjectClass write SetTreeObjectClass;
  end;

  { TSitesAndServicesOptions }

  TSitesAndServicesOptions = class(TInterfacedObject, IOptionSection)
  private
    fOptionsPath: RawUtf8;
    fSection: RawUtf8;
    fUpdating: Integer;

    fSearchPageSize: Integer;
    fSearchPageNumber: Integer;

    fViewFilter: RawUtf8;

    fCustomTreeFilter: RawUtf8;
    fTreeObjectClass: TRawUtf8DynArray;
    procedure SetCustomTreeFilter(AValue: RawUtf8);
    procedure SetSearchPageNumber(AValue: Integer);
    procedure SetSearchPageSize(AValue: Integer);
    procedure SetTreeObjectClass(AValue: TRawUtf8DynArray);
    procedure SetViewFilter(AValue: RawUtf8);

  public
    constructor Create(OptionsPath: RawUtf8); overload;

    procedure LoadOptions(f: TTisInifiles);
    procedure LoadOptions;
    procedure SaveOptions(f: TTisInifiles);
    procedure SaveOptions;
  published
    property SearchPageSize: Integer read fSearchPageSize write SetSearchPageSize;
    property SearchPageNumber: Integer read fSearchPageNumber write SetSearchPageNumber;

    property ViewFilter: RawUtf8 read fViewFilter write SetViewFilter;

    property CustomTreeFilter: RawUtf8 read fCustomTreeFilter write SetCustomTreeFilter;
    property TreeObjectClass: TRawUtf8DynArray read fTreeObjectClass write SetTreeObjectClass;
  end;

  { TOptions }

  TOptions = class(TSynPersistent)
  private
    fUpdating: Integer;
    fOptionsPath: RawUtf8;
    fSection: RawUtf8;

    // Lang
    fLang: RawUtf8;
    // Console mode
    fConsoleMode: TConsoleMode;
    // Users and Computers
    fUsersAndComputers: TUsersAndComputersOptions;
    // Sites and Services
    fSitesAndServices: TSitesAndServicesOptions;

    function GetConsoleMode: TConsoleMode;
    function GetCustomTreeFilter: RawUtf8;
    function GetLang: RawUtf8;
    function GetSearchPageNumber: Integer;
    function GetSearchPageSize: Integer;
    function GetTreeObjectClass: TRawUtf8DynArray;
    function GetTreeObjectClassRaw: RawUtf8;
    function GetViewFilter: RawUtf8;
    procedure SetConsoleMode(AValue: TConsoleMode);
    procedure SetCustomTreeFilter(AValue: RawUtf8);
    procedure SetLang(AValue: RawUtf8);
    procedure SetSearchPageNumber(AValue: Integer);
    procedure SetSearchPageSize(AValue: Integer);
    procedure SetTreeObjectClass(AValue: TRawUtf8DynArray);
    procedure SetTreeObjectClassRaw(AValue: RawUtf8);
    procedure SetViewFilter(AValue: RawUtf8);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure SaveOptions;
    procedure LoadOptions;

    property UsersAndComputers: TUsersAndComputersOptions read fUsersAndComputers;
    property SitesAndServices: TSitesAndServicesOptions read fSitesAndServices;

    property SearchPageSize: Integer read GetSearchPageSize write SetSearchPageSize;
    property SearchPageNumber: Integer read GetSearchPageNumber write SetSearchPageNumber;
    property ViewFilter: RawUtf8 read GetViewFilter write SetViewFilter;
    property CustomTreeFilter: RawUtf8 read GetCustomTreeFilter write SetCustomTreeFilter;
    property TreeObjectClass: TRawUtf8DynArray read GetTreeObjectClass write SetTreeObjectClass;
    property TreeObjectClassRaw: RawUtf8 read GetTreeObjectClassRaw write SetTreeObjectClassRaw;
  published
    property Lang: RawUtf8 read GetLang write SetLang;
    property ConsoleMode: TConsoleMode read GetConsoleMode write SetConsoleMode;
  end;

implementation

uses
  // Submodules
  mormot.core.data,
  mormot.core.os,
  mormot.core.variants;

{ TUsersAndComputersOptions }

procedure TUsersAndComputersOptions.SetCustomTreeFilter(AValue: RawUtf8);
begin
  if fCustomTreeFilter = AValue then
    Exit;
  fCustomTreeFilter := AValue;
end;

procedure TUsersAndComputersOptions.SetSearchPageNumber(AValue: Integer);
begin
  if fSearchPageNumber = AValue then
    Exit;
  fSearchPageNumber := AValue;
end;

procedure TUsersAndComputersOptions.SetSearchPageSize(AValue: Integer);
begin
  if fSearchPageSize = AValue then
    Exit;
  fSearchPageSize := AValue;
end;

procedure TUsersAndComputersOptions.SetTreeObjectClass(AValue: TRawUtf8DynArray
  );
begin
  if fTreeObjectClass = AValue then
    Exit;
  fTreeObjectClass := AValue;
end;

procedure TUsersAndComputersOptions.SetViewFilter(AValue: RawUtf8);
begin
  if fViewFilter = AValue then
    Exit;
  fViewFilter := AValue;
end;

constructor TUsersAndComputersOptions.Create(OptionsPath: RawUtf8);
begin
  inherited Create;

  fOptionsPath := OptionsPath;
  fSection := 'UsersAndComputers';
  fUpdating := 0;

  ViewFilter := '';
  CustomTreeFilter := '';
  TreeObjectClass := ['container', 'organizationalUnit', 'lostAndFound', 'builtinDomain', 'msDS-QuotaContainer', 'msTPM-InformationObjectsContainer'];
end;

procedure TUsersAndComputersOptions.LoadOptions(f: TTisInifiles);
var
  aTreeObjectClass: TRawUtf8DynArray;
  i: Integer;
  StringTreeObjectClass: TStringArray;
  str: String;
begin
  SearchPageSize := f.ReadInteger(Section, 'SearchPageSize', 1000);
  SearchPageNumber := f.ReadInteger(Section, 'SearchPageNumber', 2);
  ViewFilter := f.ReadString(Section, 'ViewFilter', ViewFilter);
  CustomTreeFilter := f.ReadString(Section, 'CustomTreeFilter', CustomTreeFilter);
  str := f.ReadString(Section, 'TreeObjectClass', '');
  if str <> '' then
  begin
    StringTreeObjectClass := str.Split(';');
    aTreeObjectClass := [];
    for i := 0 to Length(StringTreeObjectClass) - 1 do
      Insert(StringTreeObjectClass[i], aTreeObjectClass, i);
    TreeObjectClass := aTreeObjectClass;
  end;
end;

procedure TUsersAndComputersOptions.LoadOptions;
var
  f: TTisInifiles;
begin
  f := TTisInifiles.Create(OptionsPath);
  try
    Inc(fUpdating);
    LoadOptions(f);
  finally
    Dec(fUpdating);
    FreeAndNil(f);
  end;
end;

procedure TUsersAndComputersOptions.SaveOptions(f: TTisInifiles);
var
  RawTreeObjectClass: RawUtf8;
  i: Integer;
begin
  f.WriteIntegerIfChanged(Section, 'SearchPageSize', SearchPageSize);
  f.WriteIntegerIfChanged(Section, 'SearchPageNumber', SearchPageNumber);
  f.WriteStringIfChanged(Section, 'ViewFilter', ViewFilter);
  f.WriteStringIfChanged(Section, 'CustomTreeFilter', CustomTreeFilter);
  for i := 0 to High(TreeObjectClass) do
    if i = 0 then
      RawTreeObjectClass := TreeObjectClass[i]
    else
      RawTreeObjectClass := FormatUtf8('%;%', [RawTreeObjectClass, TreeObjectClass[i]]);
  f.WriteStringIfChanged(Section, 'TreeObjectClass', RawTreeObjectClass);
end;

procedure TUsersAndComputersOptions.SaveOptions;
var
  f: TTisInifiles;
begin
  f := TTisInifiles.Create(OptionsPath);
  try
    Inc(fUpdating);
    SaveOptions(f);
  finally
    Dec(fUpdating);
    FreeAndNil(f);
  end;
end;

{ TSitesAndServicesOptions }

procedure TSitesAndServicesOptions.SetCustomTreeFilter(AValue: RawUtf8);
begin
  if fCustomTreeFilter = AValue then
    Exit;
  fCustomTreeFilter := AValue;
end;

procedure TSitesAndServicesOptions.SetSearchPageNumber(AValue: Integer);
begin
  if fSearchPageNumber = AValue then
    Exit;
  fSearchPageNumber := AValue;
end;

procedure TSitesAndServicesOptions.SetSearchPageSize(AValue: Integer);
begin
  if fSearchPageSize = AValue then
    Exit;
  fSearchPageSize := AValue;
end;

procedure TSitesAndServicesOptions.SetTreeObjectClass(AValue: TRawUtf8DynArray);
begin
  if fTreeObjectClass = AValue then
    Exit;
  fTreeObjectClass := AValue;
end;

procedure TSitesAndServicesOptions.SetViewFilter(AValue: RawUtf8);
begin
  if fViewFilter = AValue then
    Exit;
  fViewFilter := AValue;
end;

constructor TSitesAndServicesOptions.Create(OptionsPath: RawUtf8);
begin
  inherited Create;

  fOptionsPath := OptionsPath;
  fSection := 'SitesAndServices';
  fUpdating := 0;

  ViewFilter := '';
  CustomTreeFilter := '';
  TreeObjectClass := ['sitesContainer', 'interSiteTransportContainer', 'subnetContainer', 'site', 'interSiteTransport', 'serversContainer', 'server', 'nTDSDSA'];
end;

procedure TSitesAndServicesOptions.LoadOptions(f: TTisInifiles);
var
  TempTreeObjectClass: TStringArray;
  aTreeObjectClass: TRawUtf8DynArray;
  i: Integer;
  str: String;
begin
  SearchPageSize := f.ReadInteger(fSection, 'SearchPageSize', 1000);
  SearchPageNumber := f.ReadInteger(fSection, 'SearchPageNumber', 2);
  ViewFilter := f.ReadString(fSection, 'ViewFilter', ViewFilter);
  CustomTreeFilter := f.ReadString(fSection, 'CustomTreeFilter', CustomTreeFilter);
  str := f.ReadString(fSection, 'TreeObjectClass', '');
  if str <> '' then
  begin
    TempTreeObjectClass := str.Split(';');
    aTreeObjectClass := [];
    for i := 0 to High(TempTreeObjectClass) do
      Insert(TempTreeObjectClass[i], aTreeObjectClass, i);
    TreeObjectClass := aTreeObjectClass;
  end;
end;

procedure TSitesAndServicesOptions.LoadOptions;
var
  f: TTisInifiles;
begin
  f := TTisInifiles.Create(fOptionsPath);
  try
    LoadOptions(f);
  finally
    FreeAndNil(f);
  end;
end;

procedure TSitesAndServicesOptions.SaveOptions;
var
  f: TTisInifiles;
begin
  f := TTisInifiles.Create(fOptionsPath);
  try
    SaveOptions(f);
  finally
    FreeAndNil(f);
  end;
end;

procedure TSitesAndServicesOptions.SaveOptions(f: TTisInifiles);
var
  i: Integer;
  RawTreeObjectClass: RawUtf8;
begin
  f.WriteIntegerIfChanged(fSection, 'SearchPageSize', SearchPageSize);
  f.WriteIntegerIfChanged(fSection, 'SearchPageNumber', SearchPageNumber);
  f.WriteStringIfChanged(fSection, 'ViewFilter', ViewFilter);
  f.WriteStringIfChanged(fSection, 'CustomTreeFilter', CustomTreeFilter);
  for i := 0 to High(TreeObjectClass) do
    if i = 0 then
      RawTreeObjectClass := TreeObjectClass[i]
    else
      RawTreeObjectClass := FormatUtf8('%;%', [RawTreeObjectClass, TreeObjectClass[i]]);
  f.WriteStringIfChanged(fSection, 'TreeObjectClass', RawTreeObjectClass);
end;

{ TOptions }

procedure TOptions.BeginUpdate;
begin
  Inc(fUpdating);
end;

procedure TOptions.EndUpdate;
begin
  Dec(fUpdating);

  SaveOptions;
end;

procedure TOptions.SaveOptions;
var
  f: TTisInifiles;
begin
  if fUpdating <> 0 then
    Exit;
  f := TTisInifiles.Create(fOptionsPath);
  try
    f.WriteStringIfChanged(fSection, 'Lang', Lang);
    UsersAndComputers.SaveOptions(f);
    SitesAndServices.SaveOptions(f);
  finally
    FreeAndNil(f);
  end;
end;

procedure TOptions.LoadOptions;
var
  f: TTisInifiles;
begin
  f := TTisInifiles.Create(fOptionsPath);
  try
    Inc(fUpdating);
    Lang := f.ReadString(fSection, 'Lang', 'en');
    UsersAndComputers.LoadOptions(f);
    SitesAndServices.LoadOptions(f);
  finally
    Dec(fUpdating);
    FreeAndNil(f);
  end;
end;

function TOptions.GetLang: RawUtf8;
begin
  result := fLang;
end;

function TOptions.GetSearchPageNumber: Integer;
begin
  case fConsoleMode of
    cmUsersAndComputers: result := fUsersAndComputers.SearchPageNumber;
    cmSitesAndServices: result := fSitesAndServices.SearchPageNumber;
    else
      result := -1;
  end;
end;

function TOptions.GetSearchPageSize: Integer;
begin
  case fConsoleMode of
    cmUsersAndComputers: result := fUsersAndComputers.SearchPageSize;
    cmSitesAndServices: result := fSitesAndServices.SearchPageSize;
    else
      result := -1;
  end;
end;

function TOptions.GetTreeObjectClass: TRawUtf8DynArray;
begin
  case fConsoleMode of
    cmUsersAndComputers: result := fUsersAndComputers.TreeObjectClass;
    cmSitesAndServices: result := fSitesAndServices.TreeObjectClass;
    else
      result := [];
  end;
end;

function TOptions.GetTreeObjectClassRaw: RawUtf8;
var
  Item: RawUtf8;
begin
  result := '';

  for Item in TreeObjectClass do
    result := FormatUtf8('%;%', [Item, result]);
end;

function TOptions.GetViewFilter: RawUtf8;
begin
  case fConsoleMode of
    cmUsersAndComputers: result := fUsersAndComputers.ViewFilter;
    cmSitesAndServices: result := fSitesAndServices.ViewFilter;
    else
      result := '';
  end;
end;

function TOptions.GetConsoleMode: TConsoleMode;
begin
  result := fConsoleMode;
end;

function TOptions.GetCustomTreeFilter: RawUtf8;
begin
  case fConsoleMode of
    cmUsersAndComputers: result := fUsersAndComputers.CustomTreeFilter;
    cmSitesAndServices: result := fSitesAndServices.CustomTreeFilter;
    else
      result := '';
  end;
end;

procedure TOptions.SetConsoleMode(AValue: TConsoleMode);
begin
  if (AValue = fConsoleMode) then
    Exit;
  fConsoleMode := AValue;
  SaveOptions;
end;

procedure TOptions.SetCustomTreeFilter(AValue: RawUtf8);
begin
  case fConsoleMode of
    cmUsersAndComputers: fUsersAndComputers.CustomTreeFilter := AValue;
    cmSitesAndServices: fSitesAndServices.CustomTreeFilter := AValue;
  end;
end;

procedure TOptions.SetLang(AValue: RawUtf8);
begin
  if (fLang <> AValue) then
    fLang := AValue;
  SaveOptions;
end;

procedure TOptions.SetSearchPageNumber(AValue: Integer);
begin
  case fConsoleMode of
    cmUsersAndComputers: fUsersAndComputers.SearchPageNumber := AValue;
    cmSitesAndServices: fSitesAndServices.SearchPageNumber := AValue;
  end;
end;

procedure TOptions.SetSearchPageSize(AValue: Integer);
begin
  case fConsoleMode of
    cmUsersAndComputers: fUsersAndComputers.SearchPageSize := AValue;
    cmSitesAndServices: fSitesAndServices.SearchPageSize := AValue;
  end;
end;

procedure TOptions.SetTreeObjectClass(AValue: TRawUtf8DynArray);
begin
  case fConsoleMode of
    cmUsersAndComputers: fUsersAndComputers.TreeObjectClass := AValue;
    cmSitesAndServices: fSitesAndServices.TreeObjectClass := AValue;
  end;
end;

procedure TOptions.SetTreeObjectClassRaw(AValue: RawUtf8);
var
  RawItems: TStringArray;
  aTreeObjectClass: TRawUtf8DynArray;
  Item: String;
begin
  aTreeObjectClass := [];
  RawItems := String(AValue).Split(';');

  for Item in RawItems do
    if Item <> '' then
      Insert(Item, aTreeObjectClass, 0);
  SetTreeObjectClass(aTreeObjectClass);
end;

procedure TOptions.SetViewFilter(AValue: RawUtf8);
begin
  case fConsoleMode of
    cmUsersAndComputers: fUsersAndComputers.ViewFilter := AValue;
    cmSitesAndServices: fSitesAndServices.ViewFilter := AValue;
  end;
end;

constructor TOptions.Create;
begin
  inherited Create;

  fUpdating := 0;
  fOptionsPath := MakePath([GetAppConfigDir(False), 'options.ini']);
  fSection := 'global';

  fLang := '';
  fConsoleMode := cmUsersAndComputers;

  fUsersAndComputers := TUsersAndComputersOptions.Create(fOptionsPath);
  fSitesAndServices := TSitesAndServicesOptions.Create(foptionsPath);
end;

destructor TOptions.Destroy;
begin
  FreeAndNil(fUsersAndComputers);
  FreeAndNil(fSitesAndServices);
  inherited Destroy;
end;

end.

