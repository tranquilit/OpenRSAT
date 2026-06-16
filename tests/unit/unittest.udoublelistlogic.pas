unit unittest.udoublelistlogic;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.test,
  mormot.core.base,
  mormot.net.ldap,
  udoublelistlogic;

type
  TDoubleListLogicTest = class(TDoubleListLogic)
  public
    procedure GetAllResources; override;
    procedure SyncAttributeProperty(Option: TLdapAddOption); override;
  end;

  TUnitTestDoubleListLogic = class(TSynTestCase)
  published
    procedure AddToList_SingleItem;
    procedure AddToList_NilIgnored;
    procedure MoveItem_OutToIn;
    procedure MoveItem_InToOut;
    procedure GetNbElementsInLists;
    procedure GetItemAttrValue;
    procedure GetResultName;
    procedure GetValueFromAttribute_Nil;
    procedure GetValueFromAttribute_Valid;
  end;

implementation

procedure TDoubleListLogicTest.GetAllResources;
begin
end;

procedure TDoubleListLogicTest.SyncAttributeProperty(Option: TLdapAddOption);
begin
end;

procedure TUnitTestDoubleListLogic.AddToList_SingleItem;
var
  Logic: TDoubleListLogicTest;
  R: TLdapResult;
begin
  Logic := TDoubleListLogicTest.Create;
  try
    R := TLdapResult.Create;
    try
      R.FindOrAdd('name').Add('test');

      Logic.AddToList(R);

      Check(Length(Logic.OutResult) = 1);
      Check(Logic.OutResult[0].Find('name').GetReadable = 'test');
    finally
      R.Free;
    end;
  finally
    Logic.Free;
  end;
end;

procedure TUnitTestDoubleListLogic.AddToList_NilIgnored;
var
  Logic: TDoubleListLogicTest;
begin
  Logic := TDoubleListLogicTest.Create;
  try
    Logic.AddToList(nil);

    Check(Length(Logic.OutResult) = 0);
  finally
    Logic.Free;
  end;
end;

procedure TUnitTestDoubleListLogic.MoveItem_OutToIn;
var
  Logic: TDoubleListLogicTest;
  R: TLdapResult;
begin
  Logic := TDoubleListLogicTest.Create;
  try
    R := TLdapResult.Create;
    try
      R.FindOrAdd('name').Add('move');

      Logic.AddToList(R);
      Logic.MoveItem(msInResult, 0);

      Check(Length(Logic.OutResult) = 0);
      Check(Length(Logic.InResult) = 1);
    finally
      R.Free;
    end;
  finally
    Logic.Free;
  end;
end;

procedure TUnitTestDoubleListLogic.MoveItem_InToOut;
var
  Logic: TDoubleListLogicTest;
  R: TLdapResult;
begin
  Logic := TDoubleListLogicTest.Create;
  try
    R := TLdapResult.Create;
    try
      R.FindOrAdd('name').Add('move2');

      Logic.AddToList(R);
      Logic.MoveItem(msInResult, 0);
      Logic.MoveItem(msOutOfResult, 0);

      Check(Length(Logic.OutResult) = 1);
      Check(Length(Logic.InResult) = 0);
    finally
      R.Free;
    end;
  finally
    Logic.Free;
  end;
end;

procedure TUnitTestDoubleListLogic.GetNbElementsInLists;
var
  Logic: TDoubleListLogicTest;
  R: TLdapResult;
begin
  Logic := TDoubleListLogicTest.Create;
  try
    Check(Logic.GetNbElementsInLists = 0);

    R := TLdapResult.Create;
    try
      R.FindOrAdd('name').Add('x');

      Logic.AddToList(R);

      Check(Logic.GetNbElementsInLists = 1);
    finally
      R.Free;
    end;
  finally
    Logic.Free;
  end;
end;

procedure TUnitTestDoubleListLogic.GetItemAttrValue;
var
  Logic: TDoubleListLogicTest;
  Arr: TLdapResultArray;
  R: TLdapResult;
begin
  Logic := TDoubleListLogicTest.Create;
  try
    SetLength(Arr, 1);

    R := TLdapResult.Create;
    try
      R.FindOrAdd('name').Add('value');
      Arr[0] := R;

      Check(Logic.GetItemAttrValue(Arr, 0, 'name') = 'value');
    finally
      R.Free;
    end;
  finally
    Logic.Free;
  end;
end;

procedure TUnitTestDoubleListLogic.GetResultName;
var
  Logic: TDoubleListLogicTest;
  R: TLdapResult;
begin
  Logic := TDoubleListLogicTest.Create;
  try
    R := TLdapResult.Create;
    try
      R.FindOrAdd('name').Add('result');

      Check(Logic.GetResultName(R) = 'result');
    finally
      R.Free;
    end;
  finally
    Logic.Free;
  end;
end;

procedure TUnitTestDoubleListLogic.GetValueFromAttribute_Nil;
var
  Logic: TDoubleListLogicTest;
begin
  Logic := TDoubleListLogicTest.Create;
  try
    Check(Logic.GetValueFromAttribute(nil) = '');
  finally
    Logic.Free;
  end;
end;

procedure TUnitTestDoubleListLogic.GetValueFromAttribute_Valid;
var
  Logic: TDoubleListLogicTest;
  Attr: TLdapAttribute;
begin
  Logic := TDoubleListLogicTest.Create;
  try
    Attr := TLdapAttribute.Create;
    try
      Attr.Add('hello');

      Check(Logic.GetValueFromAttribute(Attr) = 'hello');
    finally
      Attr.Free;
    end;
  finally
    Logic.Free;
  end;
end;

end.
