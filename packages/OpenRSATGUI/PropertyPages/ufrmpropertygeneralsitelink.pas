unit ufrmpropertygeneralsitelink;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  ExtCtrls,
  StdCtrls,
  Spin,
  Dialogs,
  mormot.core.base,
  mormot.core.log,
  mormot.core.text,
  mormot.net.ldap,
  uhelpersui,
  uproperty,
  ursatldapclient,
  upropertyframe,
  ugeneralpropertysitelink,
  udoublelistlogic,
  ulog;

type
  { TFrmPropertyGeneralSiteLink }
  TFrmPropertyGeneralSiteLink = class(TPropertyFrame)
    Button_Schedule: TButton;
    Button_Add: TButton;
    Button_Remove: TButton;
    Edit_Name: TEdit;
    Edit_Description: TEdit;
    Image_Logo: TImage;
    Label_Cost: TLabel;
    Label_Minutes: TLabel;
    Label_Replicate: TLabel;
    Label_NotInSiteLink: TLabel;
    Label_InSiteLink: TLabel;
    Label_Description: TLabel;
    ListBox_NotInSiteLink: TListBox;
    ListBox_InSiteLink: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Line_Header: TShape;
    SpinEdit_Replicate: TSpinEdit;
    SpinEdit_Cost: TSpinEdit;
    procedure Button_AddClick(Sender: TObject);
    procedure Button_RemoveClick(Sender: TObject);
    procedure Edit_DescriptionChange(Sender: TObject);
    procedure ListBox_InSiteLinkSelectionChange(Sender: TObject; User: boolean);
    procedure ListBox_NotInSiteLinkSelectionChange(Sender: TObject; User: boolean);
    procedure SpinEdit_CostChange(Sender: TObject);
    procedure SpinEdit_ReplicateChange(Sender: TObject);
  
  private
    fLog: TSynLogClass;
    fLogic: TGeneralPropertySiteLink;
    
    procedure LoadListBox;
    procedure PrepareListBox;
    
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(Props: TProperty); override;
  end;

implementation

{$R *.lfm}

procedure TFrmPropertyGeneralSiteLink.Button_AddClick(Sender: TObject);
var
  idx: Integer;
begin
  idx := ListBox_NotInSiteLink.ItemIndex;
  if idx <> -1 then
  begin
    fLogic.MoveItem(msInResult, idx);
    fLogic.SyncAttributeProperty;
    LoadListBox;
  end;
end;

procedure TFrmPropertyGeneralSiteLink.Button_RemoveClick(Sender: TObject);
var
  idx: Integer;
begin
  idx := ListBox_InSiteLink.ItemIndex;
  if idx <> -1 then
  begin
    fLogic.MoveItem(msOutOfResult, idx);
    fLogic.SyncAttributeProperty;
    LoadListBox;
  end;
end;

procedure TFrmPropertyGeneralSiteLink.Edit_DescriptionChange(Sender: TObject);
begin
  fLogic.SetScalarProperty('description', Edit_Description.Text, aoReplaceValue);
end;

procedure TFrmPropertyGeneralSiteLink.ListBox_InSiteLinkSelectionChange(Sender: TObject; User: boolean);
begin
  Button_Remove.Enabled := True;
  Button_Add.Enabled := False;
end;

procedure TFrmPropertyGeneralSiteLink.ListBox_NotInSiteLinkSelectionChange(Sender: TObject; User: boolean);
begin
  Button_Add.Enabled := True;
  Button_Remove.Enabled := False;
end;

procedure TFrmPropertyGeneralSiteLink.SpinEdit_CostChange(Sender: TObject);
begin
  fLogic.SetScalarProperty('cost', FloatToStr(SpinEdit_Cost.Value), aoReplaceValue);
end;

procedure TFrmPropertyGeneralSiteLink.SpinEdit_ReplicateChange(Sender: TObject);
begin
  fLogic.SetScalarProperty('replInterval', IntToStr(SpinEdit_Replicate.Value), aoReplaceValue);
end;

constructor TFrmPropertyGeneralSiteLink.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fLog := TOpenRSATLog;
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, 'Create', Self);

  Caption := 'General';
end;

destructor TFrmPropertyGeneralSiteLink.Destroy;
begin
  FreeAndNil(fLogic);
  inherited Destroy;
end;

procedure TFrmPropertyGeneralSiteLink.Update(Props: TProperty);
var
  Value: RawUtf8;
begin
  fLogic := TGeneralPropertySiteLink.Create(Props);

  Edit_Name.CaptionNoChange := Props.name;
  Edit_Description.CaptionNoChange := Props.description;

  Value := fLogic.GetValueFromAttribute(fLogic.FindAttribute('cost'));
  if Value <> '' then
    SpinEdit_Cost.Value := StrToFloat(Value)
  else
    SpinEdit_Cost.Value := 0;

  Value := fLogic.GetValueFromAttribute(fLogic.FindAttribute('replInterval'));
  if Value <> '' then
    SpinEdit_Replicate.Value := StrToInt(Value)
  else
    SpinEdit_Replicate.Value := 0;

  fLogic.GetAllResources;
  PrepareListBox;
  LoadListBox;
end;

procedure TFrmPropertyGeneralSiteLink.LoadListBox;
var
  r: TLdapResult;
begin
  ListBox_NotInSiteLink.Clear;
  for r in fLogic.OutResult do
    ListBox_NotInSiteLink.Items.Add(fLogic.GetResultName(r));

  ListBox_InSiteLink.Clear;
  for r in fLogic.InResult do
    ListBox_InSiteLink.Items.Add(fLogic.GetResultName(r));
end;

procedure TFrmPropertyGeneralSiteLink.PrepareListBox;
var
  SiteList: TLdapAttribute;
  Site: RawUtf8;
  n: Integer;
begin
  SiteList := fLogic.FindAttribute('siteList');
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

end.

