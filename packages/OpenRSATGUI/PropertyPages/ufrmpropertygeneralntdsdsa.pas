unit ufrmpropertygeneralntdsdsa;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  ExtCtrls,
  StdCtrls,
  Dialogs,
  mormot.core.base,
  mormot.core.log,
  mormot.net.ldap,
  ucommon,
  uhelpersui,
  uproperty,
  upropertyframe,
  ugeneralpropertyntdsdsa,
  udoublelistlogic,
  ulog;

type

  { TFrmPropertyGeneralNTDSDSA }

  TFrmPropertyGeneralNTDSDSA = class(TPropertyFrame)
    CheckBox_GlobalCatalog: TCheckBox;
    ComboBox_QueryPolicy: TComboBox;
    Edit_DNSAlias: TEdit;
    Edit_Description: TEdit;
    Edit_Name: TEdit;
    Image_Logo: TImage;
    Label_Info: TLabel;
    Label_DNSAlias: TLabel;
    Label_QueryPolicy: TLabel;
    Label_Description: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Shape1: TShape;
    procedure CheckBox_GlobalCatalogClick(Sender: TObject);
    procedure ComboBox_QueryPolicyChange(Sender: TObject);
    procedure Edit_DescriptionChange(Sender: TObject);
  private
    fLog: TSynLogClass;
    fLogic: TGeneralPropertyNTDSDSA;
    fInternalChange: Boolean;

    procedure LoadQueryPolicy;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Update(Props: TProperty); override;
  end;

implementation

{$R *.lfm}

procedure TFrmPropertyGeneralNTDSDSA.Edit_DescriptionChange(Sender: TObject);
begin
  fLogic.SetScalarProperty('description', Edit_Description.Text, aoReplaceValue);
end;

procedure TFrmPropertyGeneralNTDSDSA.LoadQueryPolicy;
var
  CurrentDN, Item: RawUtf8;
  n: Integer;
begin
  n := 0;
  CurrentDN := fLogic.GetNamebyDN(fLogic.GetPolicyDistinguishedName);
  for Item in ComboBox_QueryPolicy.Items do
  begin
    if Item = CurrentDN then
    begin
      ComboBox_QueryPolicy.ItemIndex := n;
      Exit;
    end;
    Inc(n);
  end;
end;

procedure TFrmPropertyGeneralNTDSDSA.ComboBox_QueryPolicyChange(Sender: TObject);
begin
  fLogic.SetScalarProperty('queryPolicyObject', fLogic.GetDNbyName(ComboBox_QueryPolicy.Text), aoReplaceValue);
end;

procedure TFrmPropertyGeneralNTDSDSA.CheckBox_GlobalCatalogClick(Sender: TObject);
var
  res: Integer;
begin
  if FInternalChange then
    Exit;

  if not CheckBox_GlobalCatalog.Checked then
    res := MessageDlg(rsNoGCDetected + LineEnding + LineEnding + rsWarningNoGC + LineEnding + LineEnding + rsConfirmRemovingGC, mtWarning, [mbYes, mbNo], 0);

  if res = mrNo then
  begin
    fInternalChange := True;
    CheckBox_GlobalCatalog.Checked := not CheckBox_GlobalCatalog.Checked;
    fInternalChange := False;
    Exit;
  end;
end;

constructor TFrmPropertyGeneralNTDSDSA.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fLog := TOpenRSATLog;
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, 'Create', Self);

  Caption := 'General';
end;

procedure TFrmPropertyGeneralNTDSDSA.Update(Props: TProperty);
var
  Policy: TLdapResult;
begin
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, 'Update', Self);

  fLogic := TGeneralPropertyNTDSDSA.Create(Props);
  fLogic.GetAllQueryPolicies;

  Edit_Name.Text := Props.name;
  Edit_Description.Text := Props.description;

  for Policy in fLogic.QueryPolicies do
  begin
    ComboBox_QueryPolicy.Items.Add(fLogic.GetAttributeName(Policy.Attributes));
  end;

  LoadQueryPolicy;
end;

end.

