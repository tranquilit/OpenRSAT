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
  ucoredatamodule,
  ursatldapclient;

type

  TVisNewObjectType = (
    vnotNone,
    vnotUser,
    vnotGroup,
    vnotInetOrgPerson,
    vnotMsDNSServerSettings,
    vnotMsDSKeyCredential,
    vnotMsDSResourcePropertyList,
    vnotMsDSShadowPrincipalContainer,
    vnotMsImagingPSPs,
    vnotOrganizationalUnit,
    vnotComputer,
    vnotContact,
    vnotVolume,
    vnotSite,
    vnotSubnet,
    vnotSharedFolder,
    vnotPrinter,
    vnotServer,
    vnotSiteLink,
    vnotSiteLinkBridge
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
    fFrame: TFrame;
    fNewObjectType: TVisNewObjectType;
    fObjectOU: RawUtf8;
    fLdap: TRsatLdapClient;

    // Create the custom frame based on fNewObjectType.
    procedure ConstructFrame;
    procedure ShowFrame;
  public
    PageIdx, PageCount: Integer;

    CallBack: procedure of Object;
    constructor Create(TheOwner: TComponent; NewObjectType: TVisNewObjectType; OU,
      BaseDN: RawUtf8; Ldap: TRsatLdapClient); reintroduce;
    property ObjectOU: RawUtf8 read fObjectOU write fObjectOU;
    property BaseDN: RawUtf8 read fBaseDN write fBaseDN;
    property Frame: TFrame read fFrame;
    property Ldap: TRsatLdapClient read fLdap;
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
  ufrmnewinetorgperson,
  ufrmnewmsdnsserversettings,
  ufrmnewmsdskeycredential,
  ufrmnewmsdsresourcepropertylist,
  ufrmnewmsdsshadowprincipalcontainer,
  ufrmnewmsimagingpsps,
  ufrmnewprinter,
  ufrmnewserver,
  ufrmnewsitelink,
  ufrmnewsitelinkbridge,
  ucommonui;

{$R *.lfm}

{ TVisNewObject }

procedure TVisNewObject.FormShow(Sender: TObject);
begin
  ShowFrame;

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
    vnotNone: fFrame := TFrmNewObject.Create(Self, Ldap);
    vnotComputer: fFrame := TFrmNewComputer.Create(Self);
    vnotContact: fFrame := TFrmNewContact.Create(Self);
    vnotGroup: fFrame := TFrmNewGroup.Create(Self);
    vnotInetOrgPerson: fFrame := TFrmNewInetOrgPerson.Create(Self);
    vnotMsDNSServerSettings: fFrame := TFrmNewMsDNSServerSettings.Create(Self);
    vnotMsDSKeyCredential: fFrame := TFrmNewMsDSKeyCredential.Create(Self);
    vnotMsDSResourcePropertyList: fFrame := TFrmNewMsDSResourcePropertyList.Create(Self);
    vnotMsDSShadowPrincipalContainer: fFrame := TFrmNewMsDSShadowPrincipalContainer.Create(Self);
    vnotMsImagingPSPs: fFrame := TFrmNewMsImagingPSPs.Create(Self);
    vnotOrganizationalUnit: fFrame := TFrmNewOU.Create(Self);
    vnotUser: fFrame := TFrmNewUser.Create(Self);
    vnotVolume: fFrame := TFrmNewSharedFolder.Create(Self);
    vnotSite: fFrame := TFrmNewSite.Create(Self, Ldap);
    vnotSubnet: fFrame := TFrmNewSubnet.Create(Self, Ldap);
    vnotSharedFolder: fFrame := TFrmNewSharedFolder.Create(Self);
    vnotPrinter: fFrame := TFrmNewPrinter.Create(Self);
    vnotServer: fFrame := TFrmNewServer.Create(Self, Ldap);
    vnotSiteLink: fFrame := TFrmNewSiteLink.Create(Self, Ldap, fObjectOU);
    vnotSiteLinkBridge: fFrame := TFrmNewSiteLinkBridge.Create(Self, Ldap, fObjectOU);
    else
    begin
      Close;
    end;
  end;
end;

procedure TVisNewObject.ShowFrame;
begin
  Frame.Parent := Panel_Frame;
  Frame.Align := alClient;
  if Assigned(CallBack) then
    CallBack;
end;

constructor TVisNewObject.Create(TheOwner: TComponent;
  NewObjectType: TVisNewObjectType; OU, BaseDN: RawUtf8; Ldap: TRsatLdapClient);
begin
  inherited Create(TheOwner);

  fObjectOU := OU;
  fNewObjectType := NewObjectType;
  fBaseDN := BaseDN;

  Edit_DN.Text := DNToCN(ObjectOU);
  PageIdx := 0;
  fLdap := Ldap;
  ConstructFrame;
end;

end.

