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
  mormot.core.base,
  mormot.core.log,
  uhelpersui,
  uproperty,
  upropertyframe; 

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
  private
    fLog: TSynLog;
    fProperty: TProperty;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Update(Props: TProperty); override; 
  end;

implementation

{$R *.lfm}

constructor TFrmPropertyGeneralSiteLinkBridge.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);

  Caption := 'General';
end;

procedure TFrmPropertyGeneralSiteLinkBridge.Update(Props: TProperty);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Update', Self);

  fProperty := Props;
  
  Edit_Name.CaptionNoChange := fProperty.name;
  Edit_Description.CaptionNoChange := fProperty.description;
end; 

end.

