unit uvisnewquery;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  StdCtrls,
  Buttons,
  ExtCtrls, ActnList,
  uinterfacecore;

type

  { TVisNewQuery }

  TVisNewQuery = class(TForm)
    Action_OK: TAction;
    Action_Cancel: TAction;
    Action_Browse: TAction;
    Action_DefineQuery: TAction;
    ActionList1: TActionList;
    BitBtn_Browse: TBitBtn;
    BitBtn_DefineQueryString: TBitBtn;
    BitBtn_Cancel: TBitBtn;
    BitBtn_OK: TBitBtn;
    CheckBox_IncludeSubcontainers: TCheckBox;
    Edit_Name: TEdit;
    Edit_Description: TEdit;
    Edit_QueryRoot: TEdit;
    Label_Name: TLabel;
    Label_Description: TLabel;
    Label_QueryRoot: TLabel;
    Label_QueryString: TLabel;
    Memo_QueryString: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure Action_OKExecute(Sender: TObject);
    procedure Action_OKUpdate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    fCore: ICore;
  public
    constructor Create(TheOwner: TComponent; Core: ICore); reintroduce;
  end;

implementation
uses
  mormot.net.ldap;
{$R *.lfm}

{ TVisNewQuery }

procedure TVisNewQuery.Action_OKExecute(Sender: TObject);
begin

end;

procedure TVisNewQuery.Action_OKUpdate(Sender: TObject);
begin
  Action_OK.Enabled := (Edit_Name.Text <> '') and (Memo_QueryString.Text <> '');
end;

procedure TVisNewQuery.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    27: Close;
  end;
end;

constructor TVisNewQuery.Create(TheOwner: TComponent; Core: ICore);
begin
  inherited Create(TheOwner);

  fCore := Core;

  Edit_QueryRoot.Text := DNToCN(fCore.LdapClient.DefaultDN);
end;

end.

