unit ufrmpropertygeneralsitelinkbridge;

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
  mormot.core.text,
  mormot.net.ldap,
  uhelpersui,
  uproperty,
  upropertyframe,
  ursatldapclient,
  udoublelistlogic,
  ugeneralpropertysitelinkbridge,
  ulog;

type
  { TFrmPropertyGeneralSiteLinkBridge }  
  TFrmPropertyGeneralSiteLinkBridge = class(TPropertyFrame)
    Button_Add: TButton;
    Button_Remove: TButton;
    Edit_Name: TEdit;
    Edit_Description: TEdit;
    Image_Logo: TImage;
    Label_NotInSiteLinkBridge: TLabel;
    Label_InSiteLinkBridge: TLabel;
    Label_Description: TLabel;
    ListBox_NotInSiteLinkBridge: TListBox;
    ListBox_InSiteLinkBridge: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Line_Header: TShape;
    procedure Button_AddClick(Sender: TObject);
    procedure Button_RemoveClick(Sender: TObject);
    procedure Edit_DescriptionChange(Sender: TObject);
  private
    fLog: TSynLogClass;
    fLogic: TGeneralPropertySiteLinkBridge;

    procedure LoadListBox;
    procedure PrepareListBox;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(Props: TProperty); override;
  end;

implementation

{$R *.lfm}

procedure TFrmPropertyGeneralSiteLinkBridge.Button_AddClick(Sender: TObject);
var
  idx: Integer;
begin
  idx := ListBox_NotInSiteLinkBridge.ItemIndex;
  if idx <> -1 then
  begin
    fLogic.MoveItem(msInResult, idx);
    fLogic.SyncAttributeProperty(aoReplaceValue);
    LoadListBox;
  end;
end;

procedure TFrmPropertyGeneralSiteLinkBridge.Button_RemoveClick(Sender: TObject);
var
  idx: Integer;
begin
  idx := ListBox_InSiteLinkBridge.ItemIndex;
  if idx <> -1 then
  begin
    fLogic.MoveItem(msOutOfResult, idx);
    fLogic.SyncAttributeProperty(aoReplaceValue);
    LoadListBox;
  end;
end;

procedure TFrmPropertyGeneralSiteLinkBridge.Edit_DescriptionChange(Sender: TObject);
begin
  fLogic.SetScalarProperty('description', Edit_Description.Text, aoReplaceValue);
end;

procedure TFrmPropertyGeneralSiteLinkBridge.LoadListBox;
var
  r: TLdapResult;
begin
  ListBox_NotInSiteLinkBridge.Clear;
  for r in fLogic.OutResult do
    ListBox_NotInSiteLinkBridge.Items.Add(fLogic.GetResultName(r));

  ListBox_InSiteLinkBridge.Clear;
  for r in fLogic.InResult do
    ListBox_InSiteLinkBridge.Items.Add(fLogic.GetResultName(r));
end;

procedure TFrmPropertyGeneralSiteLinkBridge.PrepareListBox;
var
  SiteList: TLdapAttribute;
  Site: RawUtf8;
  n: Integer;
begin
  SiteList := fLogic.FindAttribute('siteLinkList');
  if not Assigned(SiteList) then
    exit;

  n := Length(fLogic.OutResult) - 1;
  while n >= 0 do
  begin
    for Site in SiteList.GetAllReadable do
    begin
      if fLogic.GetValueFromAttribute(fLogic.FindAttribute('distinguishedName', fLogic.OutResult[n])) = Site then
      begin
        fLogic.MoveItem(msInResult, n);
        break;
      end;
    end;

    Dec(n);
  end
end;

constructor TFrmPropertyGeneralSiteLinkBridge.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fLog := TOpenRSATLog;
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, 'Create', Self);

  Caption := 'General';
end;

destructor TFrmPropertyGeneralSiteLinkBridge.Destroy;
begin
  FreeAndNil(fLogic);
  inherited Destroy;
end;

procedure TFrmPropertyGeneralSiteLinkBridge.Update(Props: TProperty);
begin
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, 'Update', Self);

  fLogic := TGeneralPropertySiteLinkBridge.Create(Props);

  Edit_Name.CaptionNoChange := fLogic.Props.name;
  Edit_Description.CaptionNoChange := fLogic.Props.description;

  fLogic.GetAllResources;
  PrepareListBox;
  LoadListBox;
end;

end.

