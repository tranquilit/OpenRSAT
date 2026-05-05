unit ufrmpropertygeneralsitelink;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  ExtCtrls,
  StdCtrls, Spin,
  mormot.core.base,
  mormot.core.log,
  uproperty,
  upropertyframe;

type
  
  { TFrmPropertyGeneralSiteLink }

  TFrmPropertyGeneralSiteLink = class(TPropertyFrame)
    Button_Schedule: TButton;
    Button_Add: TButton;
    Button_Remove: TButton;
    Edit_Description: TEdit;
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
    SpinEdit_Replicate: TSpinEdit;
    SpinEdit_Cost: TSpinEdit;
    procedure Button_AddClick(Sender: TObject);
    procedure Button_RemoveClick(Sender: TObject);
    procedure ListBox_InSiteLinkSelectionChange(Sender: TObject; User: boolean);
    procedure ListBox_NotInSiteLinkSelectionChange(Sender: TObject; 
      User: boolean);
  private
    fLog: TSynLog;
    fProperty: TProperty;
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

constructor TFrmPropertyGeneralSiteLink.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);

  Caption := 'Site Link';
end;

procedure TFrmPropertyGeneralSiteLink.Update(Props: TProperty);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Update', Self);

  fProperty := Props;
end; 

end.

