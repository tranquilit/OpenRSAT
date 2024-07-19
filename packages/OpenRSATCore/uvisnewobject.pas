unit uvisnewobject;

{$mode ObjFPC}{$H+}

interface

uses
  Buttons,
  Classes,
  ExtCtrls,
  Forms,
  StdCtrls,
  mormot.net.ldap,
  mormot.core.base,
  Controls,
  ucoredatamodule;

type

  TVisNewObjectType = (
    vnotNone,
    vnotUser,
    vnotGroup,
    vnotOrganizationalUnit,
    vnotComputer,
    vnotContact,
    vnotVolume,
    vnotSite,
    vnotSubnet
  );
  { TVisNewObject }

  TVisNewObject = class(TForm)
    Panel_Frame: TPanel;
    Panel_Header: TPanel;
    Edit_DN: TEdit;
    Image_Object: TImage;
    Label_DN: TLabel;
    Line_top: TShape;
    Line_bottom: TShape;
    Panel_Bottom: TPanel;
    Btn_Back: TBitBtn;
    Btn_Next: TBitBtn;
    Btn_Cancel: TBitBtn;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    fBaseDN: RawUtf8;
    Frame: TFrame;
    fNewObjectType: TVisNewObjectType;
    fObjectOU: RawUtf8;

    // Create the custom frame based on fNewObjectType.
    procedure ConstructFrame;
  public
    PageIdx, PageCount: Integer;
    Ldap: TLdapClient;

    CallBack: procedure of Object;
    constructor Create(TheOwner: TComponent; NewObjectType: TVisNewObjectType; OU, BaseDN: RawUtf8); reintroduce;
    property ObjectOU: RawUtf8 read fObjectOU write fObjectOU;
    property BaseDN: RawUtf8 read fBaseDN write fBaseDN;
  end;

implementation
uses
  Dialogs,
  SysUtils,
  mormot.core.text,
  ufrmnewcomputer,
  ufrmnewcontact,
  ufrmnewgroup,
  ufrmnewobject,
  ufrmnewou,
  ufrmnewsharedfolder,
  ufrmnewsite,
  ufrmnewsubnet,
  ufrmnewuser,
  ucommon;

{$R *.lfm}

{ TVisNewObject }

procedure TVisNewObject.FormShow(Sender: TObject);
begin
  ConstructFrame;

  UnifyButtonsWidth([Btn_Back, Btn_Next, Btn_Cancel]);
end;

procedure TVisNewObject.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    27: Close;
  end;
end;

procedure TVisNewObject.ConstructFrame;
begin
  case fNewObjectType of
    vnotNone: Frame := TFrmNewObject.Create(Self, Ldap);
    vnotComputer: Frame := TFrmNewComputer.Create(Self);
    vnotContact: Frame := TFrmNewContact.Create(Self);
    vnotGroup: Frame := TFrmNewGroup.Create(Self);
    vnotOrganizationalUnit: Frame := TFrmNewOU.Create(Self);
    vnotUser: Frame := TFrmNewUser.Create(Self);
    vnotVolume: Frame := TFrmNewSharedFolder.Create(Self);
    vnotSite: Frame := TFrmNewSite.Create(Self, Ldap);
    vnotSubnet: Frame := TFrmNewSubnet.Create(Self, Ldap);
    else
    begin
      Close;
    end;
  end;
  Frame.Parent := Panel_Frame;
  Frame.Align := alClient;
  if Assigned(CallBack) then
    CallBack;
end;

constructor TVisNewObject.Create(TheOwner: TComponent;
  NewObjectType: TVisNewObjectType; OU, BaseDN: RawUtf8);
begin
  inherited Create(TheOwner);

  fObjectOU := OU;
  fNewObjectType := NewObjectType;
  fBaseDN := BaseDN;

  Edit_DN.Text := DNToCN(ObjectOU);
  PageIdx := 0;
end;

end.

