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
  mormot.core.base,
  mormot.core.log,
  mormot.net.ldap,
  uhelpersui,
  uproperty,
  ursatldapclient,
  upropertyframe;

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
    procedure ListBox_NotInSiteLinkSelectionChange(Sender: TObject;
      User: boolean);
    
type
  { LDAP Result }
  TLdapResultArray = array of TLdapResult;
  
procedure SpinEdit_CostChange(Sender: TObject);
procedure SpinEdit_ReplicateChange(Sender: TObject);
  private
    fLog: TSynLog;
    fLdap: TRsatLdapClient;
    fProperty: TProperty;

    fNotInSite, fInSite: TLdapResultArray;
    
  public
    constructor Create(TheOwner: TComponent); override;
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
    ListBox_InSiteLink.Items.Add(ListBox_NotInSiteLink.Items[idx]);
    ListBox_InSiteLink.ItemIndex := ListBox_InSiteLink.Items.Count - 1;
    ListBox_InSiteLink.SetFocus;
    ListBox_NotInSiteLink.Items.Delete(idx);
  end;
end;

procedure TFrmPropertyGeneralSiteLink.Button_RemoveClick(Sender: TObject);
var
  idx: Integer;
begin
  idx := ListBox_InSiteLink.ItemIndex;
  if idx <> -1 then
  begin
    ListBox_NotInSiteLink.Items.Add(ListBox_InSiteLink.Items[idx]);
    ListBox_NotInSiteLink.ItemIndex := ListBox_NotInSiteLink.Items.Count - 1;
    ListBox_NotInSiteLink.SetFocus;
    ListBox_InSiteLink.Items.Delete(idx);
  end;
end;

procedure TFrmPropertyGeneralSiteLink.Edit_DescriptionChange(Sender: TObject);
begin
  fProperty.Add('description', Edit_Description.Text);
end;

procedure TFrmPropertyGeneralSiteLink.ListBox_InSiteLinkSelectionChange(
  Sender: TObject; User: boolean);
begin
  Button_Remove.Enabled := True;
  Button_Add.Enabled := False;
end;

procedure TFrmPropertyGeneralSiteLink.ListBox_NotInSiteLinkSelectionChange(
  Sender: TObject; User: boolean);
begin
  Button_Add.Enabled := True;
  Button_Remove.Enabled := False;
end;

procedure TFrmPropertyGeneralSiteLink.SpinEdit_CostChange(Sender: TObject);
begin
  fProperty.Add('cost', FloatToStr(SpinEdit_Cost.Value));
end;

procedure TFrmPropertyGeneralSiteLink.SpinEdit_ReplicateChange(Sender: TObject);
begin
  fProperty.Add('replInterval', FloatToStr(SpinEdit_Replicate.Value));
end;

constructor TFrmPropertyGeneralSiteLink.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);

  Caption := 'General';
end;

procedure TFrmPropertyGeneralSiteLink.Update(Props: TProperty);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Update', Self);

  fProperty := Props;

  Edit_Name.CaptionNoChange := fProperty.name;
  Edit_Description.CaptionNoChange := fProperty.description;
  SpinEdit_Cost.Value := StrToFloat(fProperty.Attributes.Find('cost').GetReadable());
  SpinEdit_Replicate.Value := StrToFloat(fProperty.Attributes.Find('replInterval').GetReadable());
  
end;

end.

