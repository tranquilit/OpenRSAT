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
  private
    fLogic: TDoubleListLogic;
  end;

implementation

end.

