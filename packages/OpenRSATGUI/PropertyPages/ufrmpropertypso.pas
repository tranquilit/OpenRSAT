unit ufrmpropertypso;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  StdCtrls,
  ExtCtrls,
  Buttons,
  tis.ui.grid.core,
  uproperty,
  upropertyframe;

type

  { TFrmPropertyPSO }

  TFrmPropertyPSO = class(TPropertyFrame)
    BitBtn_Add: TBitBtn;
    BitBtn_Remove: TBitBtn;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    GroupBox_PasswordSettings: TGroupBox;
    GroupBox_AppliesTo: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    TisGrid_AppliesTo: TTisGrid;
  private

  public
    constructor Create(TheOwner: TComponent); override;
    procedure Update(Props: TProperty); override;

  end;

implementation

{$R *.lfm}

{ TFrmPropertyPSO }

constructor TFrmPropertyPSO.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Caption := 'PSO';
end;

procedure TFrmPropertyPSO.Update(Props: TProperty);
begin

end;

end.

