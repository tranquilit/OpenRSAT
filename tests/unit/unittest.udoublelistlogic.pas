unit unittest.udoublelistlogic;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.test,
  mormot.core.base,
  mormot.core.os.security,
  mormot.net.ldap,
  udoublelistlogic,
  ucommon;

type
  { TUnitTestDoubleListLogic }
  TUnitTestDoubleListLogic = class(TSynTestCase)
  published
    procedure RemoveFromArray_ValidList;
    procedure RemoveFromArray_EmptyList;
  end;

implementation

procedure TUnitTestDoubleListLogic.RemoveFromArray_ValidList;
var
  ArrayTest: array of TLdapResult;
begin
  try
  finally
  end;
end;

procedure TUnitTestDoubleListLogic.RemoveFromArray_EmptyList;
begin
end;

end.

