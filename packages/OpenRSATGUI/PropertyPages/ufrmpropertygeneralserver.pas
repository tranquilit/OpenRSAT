unit ufrmpropertygeneralserver;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Dialogs,
  Controls,
  ExtCtrls,
  StdCtrls,
  mormot.core.base,
  mormot.core.log,
  uhelpersui,
  uproperty,
  upropertyframe,
  ugeneralpropertyserver,
  udoublelistlogic,
  ulog;

type

  { TFrmPropertyGeneralServer }

  TFrmPropertyGeneralServer = class(TPropertyFrame)
    Button_Add: TButton;
    Button_Remove: TButton;
    Edit_Description: TEdit;
    Edit_DCType: TEdit;
    Edit_Domain: TEdit;
    Edit_Computer: TEdit;
    Edit_Name: TEdit;
    Image_Logo: TImage;
    Label_TransportsAvailable: TLabel;
    Label_Transports: TLabel;
    Label_Description: TLabel;
    Label_DCType: TLabel;
    Label_Domain: TLabel;
    Label_Computer: TLabel;
    ListBox_TransportsAvailable: TListBox;
    ListBox_Transports: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Shape1: TShape;
    procedure Button_AddClick(Sender: TObject);
    procedure Button_RemoveClick(Sender: TObject);
    procedure Edit_DescriptionChange(Sender: TObject);
    procedure ListBox_TransportsAvailableSelectionChange(Sender: TObject; User: boolean);
    procedure ListBox_TransportsSelectionChange(Sender: TObject; User: boolean);
  private
    fLog: TSynLogClass;
    fLogic: TGeneralPropertyServer;

    procedure LoadListBox;
    procedure PrepareListBox;
    function GetDomain(Value: RawUtf8): RawUtf8;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(Props: TProperty); override;
  end;

implementation

uses
  mormot.net.ldap;

{$R *.lfm}

procedure TFrmPropertyGeneralServer.Button_AddClick(Sender: TObject);
var
  idx: Integer;
begin
  idx := ListBox_TransportsAvailable.ItemIndex;
  if idx <> -1 then
  begin
    fLogic.MoveItem(msInResult, idx);
    fLogic.SyncAttributeProperty;
    LoadListBox;
  end;
end;

procedure TFrmPropertyGeneralServer.Button_RemoveClick(Sender: TObject);
var
  idx: Integer;
begin
  idx := ListBox_Transports.ItemIndex;
  if idx <> -1 then
  begin
    fLogic.MoveItem(msOutOfResult, idx);
    fLogic.SyncAttributeProperty;
    LoadListBox;
  end;
end;

procedure TFrmPropertyGeneralServer.ListBox_TransportsAvailableSelectionChange(
  Sender: TObject; User: boolean);
begin
  Button_Add.Enabled := True;
  Button_Remove.Enabled := False;
end;

procedure TFrmPropertyGeneralServer.ListBox_TransportsSelectionChange(
  Sender: TObject; User: boolean);
begin
  Button_Remove.Enabled := True;
  Button_Add.Enabled := False;
end;

procedure TFrmPropertyGeneralServer.Edit_DescriptionChange(Sender: TObject);
begin
  fLogic.SetScalarProperty('description', Edit_Description.Text, aoReplaceValue);
end;

constructor TFrmPropertyGeneralServer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fLog := TOpenRSATLog;
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, 'Create', Self);

  Caption := 'General';
end;

destructor TFrmPropertyGeneralServer.Destroy;
begin
  FreeAndNil(fLogic);
  inherited Destroy;
end;

procedure TFrmPropertyGeneralServer.Update(Props: TProperty);
begin
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, 'Update', Self);

  fLogic := TGeneralPropertyServer.Create(Props);

  Edit_Name.Text := Props.name;
  Edit_Computer.Text := Props.CN;
  Edit_Description.Text := Props.description;
  Edit_Domain.Text := DNToCN(Props.LdapClient.DefaultDN);

  if fLogic.ServerIsGC then
    Edit_DCType.Text := 'Global Catalog'
  else
    Edit_DCType.Text := 'Domain Controller';

  fLogic.GetAllResources;
  PrepareListBox;
  LoadListBox;
end;

procedure TFrmPropertyGeneralServer.LoadListBox;
var
  r: TLdapResult;
begin
  ListBox_TransportsAvailable.Clear;
  for r in fLogic.OutResult do
    ListBox_TransportsAvailable.Items.Add(fLogic.GetResultName(r));

  ListBox_Transports.Clear;
  for r in fLogic.InResult do
    ListBox_Transports.Items.Add(fLogic.GetResultName(r));
end;

procedure TFrmPropertyGeneralServer.PrepareListBox;
var
  BridgeheadTransportList: TLdapAttribute;
  Transport: RawUtf8;
  n: Integer;
begin
  BridgeheadTransportList := fLogic.FindAttribute('bridgeheadTransportList');
  if not Assigned(BridgeheadTransportList) then
    exit;

  n := Length(fLogic.OutResult) - 1;
  while n >= 0 do
  begin
    for Transport in BridgeheadTransportList.GetAllReadable do
    begin
      if fLogic.GetValueFromAttribute(fLogic.FindAttribute('distinguishedName', fLogic.OutResult[n])) = Transport then
      begin
        fLogic.MoveItem(msInResult, n);
        break;
      end;
    end;

    Dec(n);
  end
end;

function TFrmPropertyGeneralServer.GetDomain(Value: RawUtf8): RawUtf8;
var
  p: Integer;
begin
  p := Pos('.', Value);
  if p = 0 then
    exit('');

  Result := Copy(Value, p + 1, Length(Value) - p);
end;

end.

