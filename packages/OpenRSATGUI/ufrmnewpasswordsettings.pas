unit ufrmnewpasswordsettings;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  ActnList,
  ComCtrls,
  ExtCtrls,
  Buttons, 
  StdCtrls,
  mormot.core.base,
  mormot.core.text,
  mormot.net.ldap,
  tis.ui.grid.core;

type

  { TFrmNewPasswordSettings }

  TFrmNewPasswordSettings = class(TFrame)
    Action_Add: TAction;
    Action_Remove: TAction;
    Action_OK: TAction;
    ActionList1: TActionList;
    BitBtn_Add: TBitBtn;
    BitBtn_Remove: TBitBtn;
    CheckBox_PwdComplexity: TCheckBox;
    CheckBox_PwdReversibleEncryption: TCheckBox;
    Edit_Name: TEdit;
    Edit_Precedence: TEdit;
    Edit_MinPwdLength: TEdit;
    Edit_PwdHistoryLength: TEdit;
    Edit_MinPwdAge: TEdit;
    Edit_MaxPwdAge: TEdit;
    Edit_LockoutThreshold: TEdit;
    Edit_LockoutObservationWindow: TEdit;
    Label_Name: TLabel;
    Label_LockoutObservationWindow: TLabel;
    Label_Precedence: TLabel;
    Label_MinPwdLength: TLabel;
    Label_PwdHistoryLength: TLabel;
    Label_PwdComplexity: TLabel;
    Label_PwdReversibleEncryption: TLabel;
    Label_MinPwdAge: TLabel;
    Label_MaxPwdAge: TLabel;
    Label_LockoutThreshold: TLabel;
    PageControl1: TPageControl;
    Panel_Actions: TPanel;
    Panel_LockoutThreshold: TPanel;
    Panel_LockoutObservationWindow: TPanel;
    Panel_Name: TPanel;
    Panel_Precedence: TPanel;
    Panel_MinPwdLength: TPanel;
    Panel_PwdHistoryLength: TPanel;
    Panel_PwdComplexity: TPanel;
    Panel_PwdReversibleEncryption: TPanel;
    Panel_MinPwdAge: TPanel;
    Panel_MaxPwdAge: TPanel;
    ScrollBox1: TScrollBox;
    TabSheet_Settings: TTabSheet;
    TabSheet_AppliesTo: TTabSheet;
    TisGrid_AppliesTo: TTisGrid;
  private
    fLdap: TLdapClient;
  public
    constructor Create(TheOwner: TComponent; ALdap: TLdapClient); reintroduce;
  end;

implementation
uses
  ucommon,
  uvisnewobject;

{$R *.lfm}

{ TFrmNewPasswordSettings }

constructor TFrmNewPasswordSettings.Create(TheOwner: TComponent;
  ALdap: TLdapClient);
var
  OwnerNewObject: TVisNewObject absolute TheOwner;
begin
  inherited Create(TheOwner);

  fLdap := ALdap;

  OwnerNewObject.Caption := rsNewObjectPasswordSettings;
  OwnerNewObject.Btn_Next.Action := Action_OK;
  OwnerNewObject.Btn_Next.Caption := rsNewObjectBtnOK;
  OwnerNewObject.Btn_Next.Default := True;
  OwnerNewObject.Btn_Back.Visible := False;
  OwnerNewObject.Image_Object.ImageIndex := -1;
end;

end.

