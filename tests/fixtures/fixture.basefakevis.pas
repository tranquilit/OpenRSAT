unit fixture.basefakevis;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils;

type

  { TFakeForm }

  TFakeForm = class(TObject, IUnknown)
  public
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): Hresult; virtual; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _AddRef: Longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release: Longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
  end;

implementation

{ TFakeForm }

function TFakeForm.QueryInterface(constref IID: TGUID; out Obj): Hresult; stdcall;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TFakeForm._AddRef: Longint; stdcall;
begin
  Result := -1;
end;

function TFakeForm._Release: Longint; stdcall;
begin
  Result := -1;
end;

end.

