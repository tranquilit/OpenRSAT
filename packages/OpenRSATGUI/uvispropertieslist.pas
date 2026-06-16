unit uvispropertieslist;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.base,
  mormot.core.log, mormot.net.ldap,
  {$IFDEF OPENRSATTESTSOLD}
  mormot.core.test,
  {$ENDIF}
  uopenrsatuicontextinterface,
  uvisproperties,
  ursatldapclient,
  ulog,
  ursat;

type

  TVisPropertiesDynArray = Array of TVisProperties;

  { TVisPropertiesList }

  TVisPropertiesList = class
  private
    fLog: TSynLogClass;
    fIContext: IOpenRSATUIContext;
    fItems: TVisPropertiesDynArray;
  public
    constructor Create(Context: IOpenRSATUIContext);
    function Open(AName: String; ADistinguishedName: String): TVisProperties;
    function Open(const ADistinguishedName: RawUtf8): TVisProperties;
    function New(AName: String; ADistinguishedName: String): TVisProperties;
    function New(const ADistinguishedName: RawUtf8): TVisProperties;
    function Close(aForm: TVisProperties): boolean;
    function Close(const ADistinguishedName: RawUtf8): Boolean;
    function CloseAll: boolean;
    function Exists(AName: String): boolean;
    function Exists(const ADistinguishedName: RawUtf8): Boolean;
    function Focus(DN: String): TVisProperties;
    function Focus(VisProperties: TVisProperties): TVisProperties;
    function Focus(index: Integer): TVisProperties;
    function GetIndex(const ADistinguishedName: RawUtf8): Integer;
    function Get(const ADistinguishedName: RawUtf8): TVisProperties;
    function Count: Integer;
    function GetNames: TStringArray;
  end;

  {$IFDEF OPENRSATTESTSOLD}

  { TTestVisPropertiesList }

  TTestVisPropertiesList = class(TSynTestCase)
  published
    procedure MethodCreate;
    procedure MethodNew;
    procedure MethodOpen;
    procedure MethodClose;
    procedure MethodCloseAll;
    procedure MethodExists;
    procedure MethodFocus;
    procedure MethodCount;
    procedure MethodGetNames;
  end;
  {$ENDIF}

implementation
uses
  dialogs,
  mormot.core.text;

{ TVisPropertiesList }

constructor TVisPropertiesList.Create(Context: IOpenRSATUIContext);
begin
  fLog := TOpenRSATLog;
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, '% - Create', [Self.ClassName]);

  fIContext := Context;
  fItems := [];
end;

function TVisPropertiesList.Open(AName: String; ADistinguishedName: String
  ): TVisProperties;
begin
  result := nil;

  TOpenRSATLog.Add.Log(sllDebug, FormatUtf8('Open new view: %.', [AName]));

  if (AName = '') then
  begin
    ShowMessage('Cannot open property with empty name.');
    Exit;
  end;

  if Exists(AName) then // Already exists
  begin
    result := Focus(ADistinguishedName);
    Exit;
  end;

  // Create new
  result := New(AName, ADistinguishedName);
end;

function TVisPropertiesList.Open(const ADistinguishedName: RawUtf8): TVisProperties;
begin
  result := nil;

  TOpenRSATLog.Add.Log(sllInfo, 'Open properties for "%"', [ADistinguishedName], Self);

  if Exists(ADistinguishedName) then
    result := Focus(ADistinguishedName)
  else
    result := New(ADistinguishedName);
end;

function TVisPropertiesList.New(AName: String; ADistinguishedName: String
  ): TVisProperties;
var
  c: SizeInt;
begin
  result := nil;

  c := Count;
  SetLength(fItems, c + 1);

  fItems[c] := TVisProperties.Create(fIContext, ADistinguishedName);
  result := fItems[c];
  fItems[c].Caption := AName;
  if Assigned(fItems[c].Owner) then
    fItems[c].Show();
end;

function TVisPropertiesList.New(const ADistinguishedName: RawUtf8): TVisProperties;
var
  c: Integer;
  Attribute: TLdapAttribute;
begin
  result := nil;

  c := Count;
  SetLength(fItems, c + 1);

  fItems[c] := TVisProperties.Create(fIContext, ADistinguishedName);
  result := fItems[c];
  Attribute := fIContext.RSAT.LdapClient.SearchObject(ADistinguishedName, '', 'name');
  result.Caption := Attribute.GetReadable();
  result.Show;
end;

function TVisPropertiesList.Close(aForm: TVisProperties): boolean;
var
  i: Integer;
  c: SizeInt;
begin
  result := True;
  TOpenRSATLog.Add.Log(sllDebug, FormatUtf8('Close view: %', [aForm.Name]));
  c := Count;

  for i := 0 to c - 1 do
    if fItems[i] = aForm then
    begin
      Delete(fItems, i, 1);
      //VisMain.MenuItem_ViewWindows.Delete(i);
      Exit;
    end;
  result := False;
end;

function TVisPropertiesList.Close(const ADistinguishedName: RawUtf8): Boolean;
var
  c, Idx: Integer;
begin
  result := False;
  TOpenRSATLog.Add.Log(sllInfo, 'Close property for "%"', [ADistinguishedName], Self);

  Idx := GetIndex(ADistinguishedName);
  if Idx >= 0 then
  begin
    Delete(fItems, Idx, 1);
    result := True;
  end;
end;

function TVisPropertiesList.CloseAll: boolean;
var
  item: TVisProperties;
begin
  result := True;

  for item in fItems do
    result := (result and Close(item));
end;

function TVisPropertiesList.Exists(AName: String): boolean;
var
  item: TVisProperties;
begin
  result := True;

  for item in fItems do
  begin
    if not Assigned(Item) then
      continue;
    if item.Caption = AName then
      Exit;
  end;
  result := False;
end;

function TVisPropertiesList.Exists(const ADistinguishedName: RawUtf8): Boolean;
begin
  result := GetIndex(ADistinguishedName) >= 0;
end;

function TVisPropertiesList.Focus(DN: String): TVisProperties;
var
  item: TVisProperties;
begin
  result := nil;
  for item in fItems do
    if item.DistinguishedName = DN then
    begin
      if Assigned(item.Owner) then
      begin
        item.Show();
        item.SetFocus();
      end;
      result := item;
      Exit;
    end;
end;

function TVisPropertiesList.Focus(VisProperties: TVisProperties
  ): TVisProperties;
var
  item: TVisProperties;
begin
  result := nil;
  for item in fItems do
    if item = VisProperties then
    begin
      if Assigned(item.Owner) then
      begin
        item.Show;
        item.SetFocus;
      end;
      result := item;
      Exit;
    end;
end;

function TVisPropertiesList.Focus(index: Integer): TVisProperties;
begin
  result := nil;

  if (index < 0) or (index >= Count) then
    Exit;

  if Assigned(fItems[index].Owner) then
  begin
    fItems[index].Show;
    fItems[index].SetFocus;
  end;
  result := fItems[index];
end;

function TVisPropertiesList.GetIndex(const ADistinguishedName: RawUtf8
  ): Integer;
var
  i: Integer;
begin
  result := -1;
  if not Assigned(fItems) then
    Exit;

  for i := 0 to Count - 1 do
  begin
    if Assigned(fItems[i]) and (fItems[i].DistinguishedName = ADistinguishedName) then
    begin
      result := i;
      Exit;
    end;
  end;
end;

function TVisPropertiesList.Get(const ADistinguishedName: RawUtf8
  ): TVisProperties;
var
  Idx: Integer;
begin
  result := nil;
  Idx := GetIndex(ADistinguishedName);
  if Idx >= 0 then
    result := fItems[Idx];
end;

function TVisPropertiesList.Count: Integer;
begin
  result := Length(fItems);
end;

function TVisPropertiesList.GetNames: TStringArray;
var
  i: Integer;
begin
  result := [];
  for i := 0 to High(fItems) do
    Insert(fItems[i].Caption, result, i);
end;

{$IFDEF OPENRSATTESTSOLD}

{ TTestVisPropertiesList }

procedure TTestVisPropertiesList.MethodCreate;
var
  Vis: TVisPropertiesList;
  Core: TFrmCore;
begin
  Vis := TVisPropertiesList.Create(nil);
  try
    Check(Assigned(Vis));
    Check(not Assigned(Vis.fCore));
    Check(Assigned(Vis.fLog));
    Check(not Assigned(Vis.fItems));
  finally
    FreeAndNil(Vis);
  end;

  Core := TFrmCore.Create(nil);
  try
    Vis := TVisPropertiesList.Create(Core);
    try
      Check(Assigned(Vis));
      Check(Assigned(Vis.fCore));
      Check(Assigned(Vis.fLog));
      Check(not Assigned(Vis.fItems));
    finally
      FreeAndNil(Vis);
    end;
  finally
    FreeAndNil(Vis);
  end;
end;

procedure TTestVisPropertiesList.MethodNew;
var
  Vis: TVisPropertiesList;
  Core: TFrmCore;
begin
  Vis := TVisPropertiesList.Create(nil);
  try
    Check(Assigned(Vis.New('Test', 'Test')));
    Check(Length(Vis.fItems) = 1);
    Check(Assigned(Vis.fItems[0]));
    Check(Assigned(Vis.New('Test', 'Test')));
    Check(Length(Vis.fItems) = 2);
    Check(Assigned(Vis.fItems[1]));
  finally
    FreeAndNil(Vis);
  end;

  Core := TFrmCore.Create(nil);
  try
    Vis := TVisPropertiesList.Create(Core);
    try
      Check(Assigned(Vis.New('Test', 'Test')));
      Check(Length(Vis.fItems) = 1);
      Check(Assigned(Vis.fItems[0]));
      Check(Assigned(Vis.New('Test', 'Test')));
      Check(Length(Vis.fItems) = 2);
      Check(Assigned(Vis.fItems[1]));
    finally
      FreeAndNil(Vis);
    end;
  finally
    FreeAndNil(Core);
  end;
end;

procedure TTestVisPropertiesList.MethodOpen;
var
  Vis: TVisPropertiesList;
  PItem: TVisProperties;
  Core: TFrmCore;
begin
  Vis := TVisPropertiesList.Create(nil);
  try
    Check(Assigned(Vis.Open('Test', 'Test')));
    Check(Length(Vis.fItems) = 1);
    Check(Assigned(Vis.fItems[0]));
    PItem := Vis.fItems[0];
    Check(Assigned(Vis.Open('Test', 'Test')));
    Check(Length(Vis.fItems) = 1);
    Check(vis.fItems[0] = PItem);
  finally
    FreeAndNil(Vis);
    PItem := nil;
  end;

  Core := TFrmCore.Create(nil);
  try
    Vis := TVisPropertiesList.Create(Core);
    try
      Check(Assigned(Vis.Open('Test', 'Test')));
      Check(Length(Vis.fItems) = 1);
      Check(Assigned(Vis.fItems[0]));
      PItem := Vis.fItems[0];
      Check(Assigned(Vis.Open('Test', 'Test')));
      Check(Length(Vis.fItems) = 1);
      Check(Vis.fItems[0] = PItem);
    finally
      FreeAndNil(Vis);
      PItem := nil;
    end;
  finally
    FreeAndNil(Core);
  end;
end;

procedure TTestVisPropertiesList.MethodClose;
var
  Vis: TVisPropertiesList;
  PItem: TVisProperties;
  Core: TFrmCore;
begin
  Vis := TVisPropertiesList.Create(nil);
  try
    Vis.New('Test', 'Test');
    Check(Length(Vis.fItems) = 1);
    PItem := Vis.fItems[0];
    Check(Vis.Close(PItem));
    Check(Length(Vis.fItems) = 0);
  finally
    FreeAndNil(Vis);
    PItem := nil;
  end;

  Core := TFrmCore.Create(nil);
  try
    Vis := TVisPropertiesList.Create(Core);
    try
      Check(Assigned(Vis.New('Test', 'Test')));
      Check(Length(Vis.fItems) = 1);
      PItem := Vis.fItems[0];
      Check(Vis.Close(PItem));
      Check(Length(Vis.fItems) = 0);
    finally
      FreeAndNil(Vis);
      PItem := nil;
    end;
  finally
    FreeAndNil(Core);
  end;
end;

procedure TTestVisPropertiesList.MethodCloseAll;
var
  Vis: TVisPropertiesList;
  i: Integer;
  Core: TFrmCore;
begin
  Vis := TVisPropertiesList.Create(nil);
  try
    for i := 0 to 10 do
      Check(Assigned(Vis.New(FormatUtf8('Test%', [i]), FormatUtf8('Test%', [i]))));
    Check(Length(Vis.fItems) = 11);
    Check(Vis.CloseAll);
    Check(Length(Vis.fItems) = 0);
  finally
    FreeAndNil(Vis);
  end;

  Core := TFrmCore.Create(nil);
  try
    Vis := TVisPropertiesList.Create(Core);
    try
      for i := 0 to 10 do
        Check(Assigned(Vis.New(FormatUtf8('Test%', [i]), FormatUtf8('Test%', [i]))));
      Check(Length(Vis.fItems) = 11);
      Check(Vis.CloseAll);
      Check(Length(Vis.fItems) = 0);
    finally
      FreeAndNil(Vis);
    end;
  finally
    FreeAndNil(Core);
  end;
end;

procedure TTestVisPropertiesList.MethodExists;
var
  Vis: TVisPropertiesList;
  Core: TFrmCore;
begin
  Vis := TVisPropertiesList.Create(nil);
  try
    Check(Assigned(Vis.New('Test', 'TestTest')));
    Check(Vis.Exists('Test'));
    Check(Not Vis.Exists('TestTest'));
  finally
    FreeAndNil(Vis);
  end;

  Core := TFrmCore.Create(nil);
  try
    Vis := TVisPropertiesList.Create(Core);
    try
      Check(Assigned(Vis.New('Test', 'TestTest')));
      Check(Vis.Exists('Test'));
      Check(not Vis.Exists('TestTest'));
    finally
      FreeAndNil(Vis);
    end;
  finally
    FreeAndNil(Core);
  end;
end;

procedure TTestVisPropertiesList.MethodFocus;
var
  Vis: TVisPropertiesList;
  Core: TFrmCore;
  VisProperty: TVisProperties;
begin
  Vis := TVisPropertiesList.Create(nil);
  try
    VisProperty := Vis.New('Test', 'TestTest');
    Check(Assigned(VisProperty));
    Check(Assigned(Vis.Focus(VisProperty)));
    Check(not Assigned(Vis.Focus(nil)));
    Check(not Assigned(Vis.Focus('Test')));
    Check(Assigned(Vis.Focus('TestTest')));
    Check(Assigned(Vis.Focus(0)));
    Check(not Assigned(Vis.Focus(1)));
    Check(not Assigned(Vis.Focus(-1)));
  finally
    FreeAndNil(Vis);
    VisProperty := nil;
  end;

  Core := TFrmCore.Create(nil);
  try
    Vis := TVisPropertiesList.Create(Core);
    try
      VisProperty := Vis.New('Test', 'TestTest');
      Check(Assigned(VisProperty));
      Check(not Assigned(Vis.Focus('Test')));
      Check(Assigned(Vis.Focus('TestTest')));
      Check(Assigned(Vis.Focus(VisProperty)));
      Check(not Assigned(Vis.Focus(nil)));
      Check(Assigned(Vis.Focus(0)));
      Check(not Assigned(Vis.Focus(1)));
      Check(not Assigned(Vis.Focus(-1)));
    finally
      FreeAndNil(Vis);
    end;
  finally
    FreeAndNil(Core);
    VisProperty := nil;
  end;
end;

procedure TTestVisPropertiesList.MethodCount;
var
  Vis: TVisPropertiesList;
  i: Integer;
  Core: TFrmCore;
begin
  Vis := TVisPropertiesList.Create(nil);
  try
    for i := 0 to 10 do
      Check(Assigned(Vis.New(FormatUtf8('Test%', [i]), FormatUtf8('Test%', [i]))));
    Check(Vis.Count = 11);
  finally
    FreeAndNil(Vis);
  end;

  Core := TFrmCore.Create(nil);
  try
    Vis := TVisPropertiesList.Create(Core);
    try
      for i := 0 to 10 do
        Check(Assigned(Vis.New(FormatUtf8('Test%', [i]), FormatUtf8('Test%', [i]))));
      Check(Vis.Count = 11);
    finally
      FreeAndNil(Vis);
    end;
  finally
    FreeAndNil(Core);
  end;
end;

procedure TTestVisPropertiesList.MethodGetNames;
var
  Vis: TVisPropertiesList;
  i: Integer;
  Names: TStringArray;
  Core: TFrmCore;
begin
  Vis := TVisPropertiesList.Create(nil);
  try
    for i := 0 to 10 do
      Check(Assigned(Vis.New(FormatUtf8('Test%', [i]), FormatUtf8('Test%', [i]))));
    Names := Vis.GetNames;
    Check(Length(Names) = Length(Vis.fItems));
    for i := 0 to 10 do
      Check(Names[i] = Vis.fItems[i].Caption);
  finally
    FreeAndNil(Vis);
    Names := nil;
  end;

  Core := TFrmCore.Create(nil);
  try
    Vis := TVisPropertiesList.Create(Core);
    try
      for i := 0 to 10 do
        Check(Assigned(Vis.New(FormatUtf8('Test%', [i]), FormatUtf8('Test%', [i]))));
      Names := Vis.GetNames;
      Check(Length(Names) = Length(Vis.fItems));
      for i := 0 to 10 do
        Check(Names[i] = Vis.fItems[i].Caption);
    finally
      FreeAndNil(Vis);
      Names := nil;
    end;
  finally
    FreeAndNil(Core);
  end;
end;

{$ENDIF}

end.

