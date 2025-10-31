unit uvisproperties;

{$mode ObjFPC}{$H+}
{$modeSwitch advancedRecords}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
  ActnList,
  Buttons,
  Classes,
  ComCtrls,
  DateTimePicker,
  Dialogs,
  ExtCtrls,
  Forms,
  Menus,
  Graphics,
  StdCtrls,
  ShellCtrls,
  PairSplitter,
  IniPropStorage,
  SysUtils,
  tis.ui.grid.core,
  VirtualTrees,
  mormot.core.base,
  mormot.core.log,
  mormot.core.os.security,
  mormot.core.variants,
  mormot.crypt.x509,
  mormot.net.ldap,
  Controls,
  uinterfacecore,
  ucommon,
  ucoredatamodule;

type

  TAttributEditorFilter = (
    aefOnlyAvailableValues,
    aefOnlyWritableValues,
    aefMandatory,
    aefOptional,
    aefConstructed,
    aefBacklinks,
    aefSystemOnly
  );

  TAttributEditorFilters = set of TAttributEditorFilter;

  { TLdiff }

  TLdiff = record
    add:    TLdapAttributeList;
    update: TLdapAttributeList;
    delete: TLdapAttributeList;

    procedure Initialize();

    procedure Finalize();

    // Commit a change to be applied with a tag `add`, `update`, `delete`
    // Diff        : the The changelist
    // PAttributes : the attributes pool
    // n           : the attribute name
    // data        : the data to be commited
    // - If the data is nil or equal to the corresonding attribute in PAttributes
    //   then the commit is canceled
    // - If the data does not exist in PAttributes then it will be tagged with `add`
    // - If the data does exist in PAttributes then it will be tagged with `update`
    // - If the data is empty (not nil) it will be tagged with `delete`
    procedure Commit(Attributes: TLdapAttributeList; n: string;
      const data: TLdapAttribute);
    procedure Commit(Attributes: TLdapAttributeList; n: TLdapAttributeType;
      const data: TLdapAttribute);

    // Get the lastest change
    // Attributes : From TVisProperties
    // n          : The attribute name
    // - If Attributes is nil then will only search in TLdiff
    // - return nil if not found
    function Get(Attributes: TLdapAttributeList; n: string): TLdapAttribute;
    function Get(Attributes: TLdapAttributeList; n: TLdapAttributeType): TLdapAttribute;

    // Check if there are any changes
    function IsEmpty(): Boolean;

    procedure Clear();

    procedure Revert(n: String);
    procedure Revert(n: TLdapAttributeType);
  end;

  { TVisProperties }

  TVisProperties = class(TForm)
    Action_AttributesModify: TAction;
    Action_LAPSCopyPassword: TAction;
    Action_LAPSShowPassword: TAction;
    Action_LAPSExpireNow: TAction;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    BitBtn_AttributeModify: TBitBtn;
    BitBtn_usr_otherTelephone: TBitBtn;
    BitBtn_usr_url: TBitBtn;
    ComboBox_subnet_site: TComboBox;
    DateTimePicker1: TDateTimePicker;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit_subnet_prefix: TEdit;
    Edit_vlm_cn1: TEdit;
    Edit_subnet_cn: TEdit;
    Edit_vlm_description1: TEdit;
    Edit_subnet_description: TEdit;
    GroupBox1: TGroupBox;
    Image_vlm1: TImage;
    Image_subnet: TImage;
    IniPropStorage1: TIniPropStorage;
    Label1: TLabel;
    Label10: TLabel;
    Label_subnet_prefix: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label_BitlockerPassword: TLabel;
    Label_BitclockerDetails: TLabel;
    Label_vlm_description1: TLabel;
    Label_subnet_description: TLabel;
    Label_vlm_uNCName1: TLabel;
    Label_subnet_site: TLabel;
    Line_vlm_top1: TShape;
    Line_subnet_top: TShape;
    ListBox1: TListBox;
    Memo1: TMemo;
    PageControl: TPageControl;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel_vlm_content1: TPanel;
    Panel_subnet_content: TPanel;
    Panel_vlm_header1: TPanel;
    Panel_subnet_header: TPanel;
    Tab_SubnetGeneral: TTabSheet;
    Tab_SiteGeneral: TTabSheet;
    Tab_dnsRecords: TTabSheet;
    Tab_dnsProperties: TTabSheet;
    Tab_DNSZoneGeneral: TTabSheet;
    Tab_LAPS: TTabSheet;
    Tab_Bitlocker: TTabSheet;
      // Tab_DefaultGeneral
        Tab_DefaultGeneral: TTabSheet;
          Panel_dft_header: TPanel;
            Image_dft: TImage;
            Edit_dft_cn: TEdit;
          Line_dft_top: TShape;
          Panel_dft_content: TPanel;
            Label_dft_description: TLabel;
            Edit_dft_description: TEdit;
      // Tab_OUGeneral
        Tab_OUGeneral: TTabSheet;
          Panel_ogu_header: TPanel;
            Image_ogu: TImage;
            Edit_ogu_name: TEdit;
          Line_ogu_top: TShape;
          Panel_ogu_content: TPanel;
            Label_ogu_description: TLabel;
            Label_ogu_street: TLabel;
            Label_ogu_city: TLabel;
            Label_ogu_state: TLabel;
            Label_ogu_zip: TLabel;
            Label_ogu_country: TLabel;
            Edit_ogu_description: TEdit;
            Memo_ogu_street: TMemo;
            Edit_ogu_l: TEdit;
            Edit_ogu_st: TEdit;
            Edit_ogu_postalCode: TEdit;
            ComboBox_ogu_co: TComboBox;
      // Tab_VolumeGeneral
        Tab_VolumeGeneral: TTabSheet;
          Panel_vlm_header: TPanel;
            Image_vlm: TImage;
            Edit_vlm_cn: TEdit;
          Line_vlm_top: TShape;
          Panel_vlm_content: TPanel;
            Label_vlm_description: TLabel;
            Edit_vlm_description: TEdit;
            Label_vlm_uNCName: TLabel;
            Edit_vlm_uNCName: TEdit;
            BitBtn_vlm_keywords: TBitBtn;
      // Tab_UserGeneral
      Tab_UserGeneral: TTabSheet;
        Panel_usr_header: TPanel;
          Image_usr: TImage;
            Edit_usr_cn: TEdit;
        Line_usr_top: TShape;
        Panel_usr_top: TPanel;
          Label_usr_givenName: TLabel;
          Label_usr_initials: TLabel;
          Label_usr_sn: TLabel;
          Label_usr_displayName: TLabel;
          Label_usr_description: TLabel;
          Label_usr_desktopProfile: TLabel;
          Edit_usr_givenName: TEdit;
          Edit_usr_initials: TEdit;
          Edit_usr_sn: TEdit;
          Edit_usr_displayName: TEdit;
          Edit_usr_description: TEdit;
          Edit_usr_physicalDeliveryOfficeName: TEdit;
        Line_usr_bottom: TShape;
        Panel_usr_bottom: TPanel;
          Label_usr_telephoneNumber: TLabel;
          Label_usr_mail: TLabel;
          Label_usr_url: TLabel;
          Edit_usr_telephoneNumber: TEdit;
          Edit_usr_mail: TEdit;
          Edit_usr_wWWHomePage: TEdit;
      // Tab_GroupGeneral
      Tab_GroupGeneral: TTabSheet;
        Panel_grp_header: TPanel;
          Image_grp: TImage;
          Edit_grp_cn: TEdit;
        Line_grp_top: TShape;
        Panel_grp_top: TPanel;
          Label_grp_sAMAccountName: TLabel;
          Label_grp_description: TLabel;
          Label_grp_mail: TLabel;
          Edit_grp_sAMAccountName: TEdit;
          Edit_grp_description: TEdit;
          Edit_grp_mail: TEdit;
        RadioGroup_Scope: TRadioGroup;
          RadioBtn_Local: TRadioButton;
          RadioBtn_Global: TRadioButton;
          RadioBtn_Universal: TRadioButton;
        RadioGroup_Type: TRadioGroup;
          RadioBtn_Security: TRadioButton;
          RadioBtn_Distribution: TRadioButton;
        Label_grp_info: TLabel;
        Memo_grp_info: TMemo;
      // Tab_ComputerGeneral
      Tab_ComputerGeneral: TTabSheet;
        Panel_cmp_header: TPanel;
          Image_cmp: TImage;
          Edit_cmp_cn: TEdit;
        Line_cmp_top: TShape;
        Panel_cmp_top: TPanel;
          Label_cmp_sAMAccountName: TLabel;
          Label_cmp_dNSHostName: TLabel;
          Label_cmp_dcType: TLabel;
          Label_cmp_site: TLabel;
          Label_cmp_description: TLabel;
          Edit_cmp_description: TEdit;
          Edit_cmp_site: TEdit;
          Edit_cmp_dcType: TEdit;
          Edit_cmp_dNSHostName: TEdit;
          Edit_cmp_sAMAccountName: TEdit;
      // Tab_OperatingSystem
      Tab_OperatingSystem: TTabSheet;
        Label_ops_operatingSystem: TLabel;
        Label_ops_operatingSystemServicePack: TLabel;
        Label_ops_operatingSystemVersion: TLabel;
        Edit_ops_operatingSystem: TEdit;
        Edit_ops_operatingSystemServicePack: TEdit;
        Edit_ops_operatingSystemVersion: TEdit;
      // Tab_Address
      Tab_Address: TTabSheet;
        Label_adr_streetAddress: TLabel;
        Label_adr_postOfficeBox: TLabel;
        Label_adr_l: TLabel;
        Label_adr_st: TLabel;
        Label_adr_PostalCode: TLabel;
        Label_adr_CountryCode: TLabel;
        Memo_adr_streetAddress: TMemo;
        Edit_adr_postOfficeBox: TEdit;
        Edit_adr_l: TEdit;
        Edit_adr_st: TEdit;
        Edit_adr_PostalCode: TEdit;
        ComboBox_adr_CountryCode: TComboBox;
      // Account
      Tab_Account: TTabSheet;
        Panel_acc_LogonName: TPanel;
          Label_acc_UserLogonName: TLabel;
          Edit_acc_Name: TEdit;
          ComboBox_acc_Domain: TComboBox;
        Panel_acc_sAMAccountName: TPanel;
          Label_acc_UserSAMAccountName: TLabel;
          Edit_acc_SAMDomain: TEdit;
          Edit_acc_sAMAccountName: TEdit;
        Panel_accLlogonTime: TPanel;
          Btn_acc_LogonHours: TBitBtn;
          Btn_acc_LogOnTo: TBitBtn;
        CheckBox_acc_Unlock: TCheckBox;
        Label_acc_Options: TLabel;
        ScrollBox_acc_Options: TScrollBox;
          CheckBox_acc_opt_CannotChange: TCheckBox;
          CheckBox_acc_opt_MustChange: TCheckBox;
          CheckBox_acc_opt_Disabled: TCheckBox;
          CheckBox_acc_opt_KerberosAES128Encryption: TCheckBox;
          CheckBox_acc_opt_KerberosAES256Encryption: TCheckBox;
          CheckBox_acc_opt_KerberosDESEncryption: TCheckBox;
          CheckBox_acc_opt_NeverExpires: TCheckBox;
          CheckBox_acc_opt_NoKerberosPreauth: TCheckBox;
          CheckBox_acc_opt_ReversibleEncryption: TCheckBox;
          CheckBox_acc_opt_Sensitive: TCheckBox;
          CheckBox_acc_opt_SmartCard: TCheckBox;
        GroupBox_acc_Expires: TGroupBox;
          RadioButton_acc_Never: TRadioButton;
          RadioButton_acc_EndOf: TRadioButton;
          DateTimePicker_acc_Expires: TDateTimePicker;
      // Tab_Profile
      Tab_Profile: TTabSheet;
        GroupBox_pro_UserProfile: TGroupBox;
          Label_pro_profilePath: TLabel;
          Edit_pro_profilePath: TEdit;
          Label_pro_scriptPath: TLabel;
          Edit_pro_scriptPath: TEdit;
        GroupBox_pro_HomeFolder: TGroupBox;
          RadioButton_pro_homeDirectory: TRadioButton;
          Edit_pro_homeDirectory: TEdit;
          RadioButton_pro_ConnectTo: TRadioButton;
          ComboBox_pro_ConnectTo: TComboBox;
          Label_pro_ConnectTo: TLabel;
          Edit_pr0_homeDirectory: TEdit;
      // Tab Telephone
      Tab_Telephone: TTabSheet;
        GroupBox_tel_Numbers: TGroupBox;
          Label_tel_homePhone: TLabel;
          Label_tel_pager: TLabel;
          Label_tel_mobile: TLabel;
          Label_tel_facsimileTelephoneNumber: TLabel;
          Label_tel_ipPhone: TLabel;
          Edit_tel_homePhone: TEdit;
          Edit_tel_pager: TEdit;
          Edit_tel_mobile: TEdit;
          Edit_tel_facsimileTelephoneNumber: TEdit;
          Edit_tel_ipPhone: TEdit;
          BitBtn_tel_otherHomePhone: TBitBtn;
          BitBtn_tel_otherPager: TBitBtn;
          BitBtn_tel_otherMobile: TBitBtn;
          BitBtn_tel_otherFacsimileTelephoneNumber: TBitBtn;
          BitBtn_tel_otherIpPhone: TBitBtn;
        Label_tel_info: TLabel;
        Memo_tel_info: TMemo;
      // Tab_Organization
      Tab_Organization: TTabSheet;
        Label_org_title: TLabel;
        Label_org_department: TLabel;
        Label_org_company: TLabel;
        Edit_org_title: TEdit;
        Edit_org_department: TEdit;
        Edit_org_company: TEdit;
        GroupBox_org_manager: TGroupBox;
          Label_org_managerName: TLabel;
          Edit_org_manager: TEdit;
          BitBtn_org_change: TBitBtn;
          BitBtn_org_properties: TBitBtn;
          BitBtn_org_clear: TBitBtn;
        Label_org_directReports: TLabel;
        Timer_TisGridSearch: TTimer;
        TisGrid1: TTisGrid;
        TisGrid2: TTisGrid;
        TisGrid3: TTisGrid;
        TisGrid_org_directReports: TTisGrid;
      // Tab_PublishedCertificates
      Tab_PublishedCertificates: TTabSheet;
        Label_cer_listX509: TLabel;
        TisGrid_cer_listX509: TTisGrid;
        BitBtn_cer_view: TBitBtn;
        BitBtn_cer_add: TBitBtn;
        BitBtn_cer_remove: TBitBtn;
        BitBtn_cer_copy: TBitBtn;
      // Tab_Member
      Tab_Member: TTabSheet;
        Label_mem_title: TLabel;
        TisGrid_mem: TTisGrid;
        Panel_mem_bottom: TPanel;
          Btn_mem_delete: TBitBtn;
          Btn_mem_add: TBitBtn;
      // Tab_MemberOf
      Tab_MemberOf: TTabSheet;
        Label_mof_title: TLabel;
        List_mof: TTisGrid;
        Panel_mof_buttons: TPanel;
          Btn_mof_Add: TBitBtn;
          Btn_mof_Delete: TBitBtn;
        Line_mof: TShape;
        Panel_mof_primaryGroup: TPanel;
          Label_mof_primaryGroupKey: TLabel;
          Btn_mof_definePrimaryGroup: TBitBtn;
          Edit_mof_primaryGroup: TEdit;
          Text_mof_definePrimaryGroup: TLabel;
        Panel_mof_noPrimaryGroup: TPanel;
          Label_mof_noPrimaryGroup: TLabel;
      Tab_Location: TTabSheet;
        Image_Loc_earth: TImage;
        Line_loc: TShape;
        Label_loc_location: TLabel;
        Edit_loc_location: TEdit;
      // Tab_ManagedBy
      Tab_ManagedBy: TTabSheet;
        Panel_man_top: TPanel;
          Label_man_distinguishedName: TLabel;
          Label_man_physicalDeliveryOfficeName: TLabel;
          Label_man_streetAddress: TLabel;
          Label_man_l: TLabel;
          Label_man_st: TLabel;
          Label_man_co: TLabel;
          Edit_man_managedBy: TEdit;
            Btn_man_clear: TBitBtn;
            Btn_man_modify: TBitBtn;
            Btn_man_properties: TBitBtn;
          CheckBox_man: TCheckBox;
          Edit_man_physicalDeliveryOfficeName: TEdit;
          Edit_man_l: TEdit;
          Edit_man_st: TEdit;
          Edit_man_co: TEdit;
          Memo_man_streetAddress: TMemo;
        Line_man_top: TShape;
        Panel_man_bottom: TPanel;
          Label_man_telephoneNumber: TLabel;
          Label_man_facsimileTelephoneNumber: TLabel;
          Edit_man_telephoneNumber: TEdit;
          Edit_man_facsimileTelephoneNumber: TEdit;
      // Tab_Object
      Tab_Object: TTabSheet;
        Panel_obj_cn: TPanel;
          Label_obj_cn: TLabel;
          Edit_obj_cn: TEdit;
        Panel_obj_top: TPanel;
          Label_obj_objectClass: TLabel;
          Label_obj_whenCreated: TLabel;
          Label_obj_whenChanged: TLabel;
          Edit_obj_objectClass: TEdit;
          Edit_obj_whenCreated: TEdit;
          Edit_obj_whenChanged: TEdit;
        Label_obj_update: TLabel;
        Panel_obj_bottom: TPanel;
          Label_obj_uSNChanged: TLabel;
          Label_obj_uSNCreated: TLabel;
          Edit_obj_uSNChanged: TEdit;
          Edit_obj_uSNCreated: TEdit;
        CheckBox_obj_protect: TCheckBox;
      // Tab_Security
      Tab_Security: TTabSheet;
        TisGrid_SecurityListUser: TTisGrid;
        TisGrid_SecurityListRight: TTisGrid;
        BitBtn_AdvSecurity: TBitBtn;
        Panel_SecurityBottomButtons: TPanel;
          BitBtn_SecurityAddUser: TBitBtn;
          BitBtn_SecurityDeleteUser: TBitBtn;
      // Tab_Attributes
      Tab_Attributes: TTabSheet;
        Label_Attributes: TLabel;
        List_Attributes: TTisGrid;
        BitBtn_AttributesFilter: TBitBtn;
    // Bottom Panel ---------------------------------------------------------
    Panel_Bottom: TPanel;
      Btn_BottomApply: TBitBtn;
      Btn_BottomCancel: TBitBtn;
      Btn_BottomOK: TBitBtn;
    // Actions
    ActionList: TActionList;
      Action_AttributesOptional: TAction;
      Action_AttributesConstructed: TAction;
      Action_AttributesBacklinks: TAction;
      Action_AttributesSystemOnly: TAction;
      Action_AttributesMandatory: TAction;
      Action_AttributesShowWritable: TAction;
      Action_AttributesShowValues: TAction;
      Action_AttributesFilter: TAction;
      Action_CertificateView: TAction;
      Action_CertificateCopy: TAction;
      Action_CertificateRemove: TAction;
      Action_MemberOfAdd: TAction;
      Action_MemberOfDelete: TAction;
      Action_Apply: TAction;
      Action_OK: TAction;
      Action_Cancel: TAction;
      Action_MembersDelete: TAction;
      Action_MembersAdd: TAction;
      Action_ManagedByCheckBox: TAction;
      Action_ManagedByClear: TAction;
      Action_ManagedByProperties: TAction;
      Action_ManagedByChange: TAction;
      Action_OrganizationClear: TAction;
      Action_OrganizationProperties: TAction;
      Action_SecurityDeleteUser: TAction;
      Action_SecurityAddUser: TAction;
      Action_SecurityAdvanced: TAction;
    PopupMenu_AttributesFilter: TPopupMenu;
      MI_AF_ShowValues: TMenuItem;
      MI_AF_ShowAttribute: TMenuItem;
      Sep_AF_1: TMenuItem;
      MI_AF_ShowReadOnly: TMenuItem;
      MI_AF_ShowWritable: TMenuItem;
      MI_AF_Mandatory: TMenuItem;
      Sep_AF_2: TMenuItem;
      MI_AF_ShowOptional: TMenuItem;
      MI_AF_Constructed: TMenuItem;
      MI_AF_Backkinks: TMenuItem;
      MI_AF_SystemOnly: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    {$push}{$warn 5024 off}
    procedure acc_userPrincipalNameChange(Sender: TObject);
    procedure Action_AttributesBacklinksExecute(Sender: TObject);
    procedure Action_AttributesConstructedExecute(Sender: TObject);
    procedure Action_AttributesFilterExecute(Sender: TObject);
    procedure Action_AttributesMandatoryExecute(Sender: TObject);
    procedure Action_AttributesModifyExecute(Sender: TObject);
    procedure Action_AttributesModifyUpdate(Sender: TObject);
    procedure Action_AttributesOptionalExecute(Sender: TObject);
    procedure Action_AttributesShowValuesExecute(Sender: TObject);
    procedure Action_AttributesShowWritableExecute(Sender: TObject);
    procedure Action_AttributesSystemOnlyExecute(Sender: TObject);
    procedure Action_CertificateCopyExecute(Sender: TObject);
    procedure Action_CertificateCopyUpdate(Sender: TObject);
    procedure Action_CertificateRemoveExecute(Sender: TObject);
    procedure Action_CertificateRemoveUpdate(Sender: TObject);
    procedure Action_CertificateViewExecute(Sender: TObject);
    procedure Action_CertificateViewUpdate(Sender: TObject);
    procedure Action_LAPSCopyPasswordExecute(Sender: TObject);
    procedure Action_LAPSExpireNowExecute(Sender: TObject);
    procedure Action_LAPSShowPasswordExecute(Sender: TObject);
    procedure Action_ManagedByChangeExecute(Sender: TObject);
    procedure Action_ManagedByCheckBoxExecute(Sender: TObject);
    procedure Action_ManagedByCheckBoxUpdate(Sender: TObject);
    procedure Action_MembersAddExecute(Sender: TObject);
    procedure Action_MembersAddUpdate(Sender: TObject);
    procedure Action_ManagedByChangeUpdate(Sender: TObject);
    procedure Action_ManagedByClearExecute(Sender: TObject);
    procedure Action_ManagedByClearUpdate(Sender: TObject);
    procedure Action_MembersDeleteExecute(Sender: TObject);
    procedure Action_MembersDeleteUpdate(Sender: TObject);
    procedure Action_ManagedByPropertiesExecute(Sender: TObject);
    procedure Action_ManagedByPropertiesUpdate(Sender: TObject);
    procedure Action_MemberOfDeleteExecute(Sender: TObject);
    procedure Action_MemberOfDeleteUpdate(Sender: TObject);
    procedure Action_MemberOfAddExecute(Sender: TObject);
    procedure Action_OrganizationClearExecute(Sender: TObject);
    procedure Action_OrganizationClearUpdate(Sender: TObject);
    procedure Action_OrganizationPropertiesExecute(Sender: TObject);
    procedure Action_OrganizationPropertiesUpdate(Sender: TObject);
    procedure Action_SecurityAdvancedExecute(Sender: TObject);
    procedure Action_ApplyExecute(Sender: TObject);
    procedure Action_ApplyUpdate(Sender: TObject);
    procedure Action_CancelExecute(Sender: TObject);
    procedure Action_OKExecute(Sender: TObject);
    procedure Action_OKUpdate(Sender: TObject);
    procedure BitBtn_cer_addClick(Sender: TObject);
    procedure BitBtn_org_changeClick(Sender: TObject);
    procedure BitBtn_other_Click(Sender: TObject);
    procedure Btn_acc_LogonHoursClick(Sender: TObject);
    procedure Btn_acc_LogOnToClick(Sender: TObject);
    procedure CheckBox_acc_opt_Change(Sender: TObject);
    procedure CheckBox_acc_UnlockChange(Sender: TObject);
    procedure CheckBox_obj_protectChange(Sender: TObject);
    procedure ComboBox_adr_CountryCodeChange(Sender: TObject);
    procedure ComboBox_adr_CountryCodeDropDown(Sender: TObject);

    procedure ComboBox_pro_ConnectToChange(Sender: TObject);
    procedure ComboBox_subnet_siteChange(Sender: TObject);
    procedure DateTimePicker1Change(Sender: TObject);
    procedure DateTimePicker_acc_ExpiresChange(Sender: TObject);
    procedure Edit_Change(Sender: TObject);
    procedure Edit_pr0_homeDirectoryChange(Sender: TObject);
    procedure Edit_pro_homeDirectoryChange(Sender: TObject);
    procedure Edit_subnet_descriptionChange(Sender: TObject);
    procedure Edit_vlm_uNCNameChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    function FormHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean
      ): Boolean;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure List_AttributesDblClick(Sender: TObject);
    procedure List_AttributesDrawText(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      const CellText: String; const CellRect: TRect; var DefaultDraw: Boolean);
    procedure List_AttributesGetText(aSender: TBaseVirtualTree;
      aNode: PVirtualNode; const aCell: TDocVariantData; aColumn: TColumnIndex;
      aTextType: TVSTTextType; var aText: string);
    procedure List_AttributesKeyPress(Sender: TObject; var Key: char);
    procedure List_mofKeyPress(Sender: TObject; var Key: char);
    procedure PageControlChange(Sender: TObject);
    procedure Timer_TisGridSearchTimer(Sender: TObject);
    procedure TisGrid1Change(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TisGrid1KeyPress(Sender: TObject; var Key: char);
    procedure TisGrid2KeyPress(Sender: TObject; var Key: char);
    procedure TisGrid3KeyPress(Sender: TObject; var Key: char);
    procedure TisGrid_cer_listX509KeyPress(Sender: TObject; var Key: char);
    procedure TisGrid_memKeyPress(Sender: TObject; var Key: char);
    procedure TisGrid_org_directReportsDblClick(Sender: TObject);
    procedure List_mofDblClick(Sender: TObject);
    procedure List_mofGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure RadioButton_acc_EndOfChange(Sender: TObject);
    procedure RadioButton_acc_NeverChange(Sender: TObject);
    procedure RadioButton_pro_Change(Sender: TObject);
    procedure Tab_AttributesEnter(Sender: TObject);
    procedure TisGrid_org_directReportsKeyPress(Sender: TObject; var Key: char);
    procedure TisGrid_SecurityListRightGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure TisGrid_SecurityListRightGetText(aSender: TBaseVirtualTree;
      aNode: PVirtualNode; const aCell: TDocVariantData; aColumn: TColumnIndex;
      aTextType: TVSTTextType; var aText: string);
    procedure TisGrid_SecurityListRightKeyPress(Sender: TObject; var Key: char);
    procedure TisGrid_SecurityListUserFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure TisGrid_memDblClick(Sender: TObject);
    procedure TisGrid_memGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure RadioBtn_Change(Sender: TObject);
    procedure TisGrid_SecurityListUserKeyPress(Sender: TObject; var Key: char);
    {$pop}
  private
    fSearchWord: RawUtf8;

    IsLoading: Boolean;

    fCore: ICore;

    fSubnetInfo: TDocVariantData;
    procedure GetMemberOf(attrName: RawUtf8; Doc: PDocVariantData);
    function GetPrimaryGroupDN(primaryGroupID: Integer): String;
    function CertToDoc(s: RawByteString): TDocVariantData;
    procedure InitPanelLocation();
    procedure InitPanelOperatingSystem();
    procedure InitPanelOrganization();
    procedure InitPanelPublishedCertfifcates();
    procedure SecurityFillListUser();
    procedure SecurityFillRightGrid(sid, n: RawUtf8);
    procedure InitPanelDefaultGeneral();
    procedure InitPanelUserGeneral();
    procedure InitPanelgroupGeneral();
    procedure InitPanelComputerGeneral();
    procedure InitPanelOUGeneral();
    procedure InitPanelVolumeGeneral();
    procedure InitPanelSiteGeneral();
    procedure InitPanelSubnetGeneral();
    procedure InitPanelAddress();
    procedure InitPanelAccount();
    procedure InitPanelProfile();
    procedure InitPanelTelephone();
    procedure InitPanelMembers();
    procedure InitPanelMemberOf();
    procedure InitPanelManagedBy();
    procedure InitPanelObject();
    procedure InitPanelSecurity();
    procedure InitPanelAttributes();
    procedure InitPanelBitlocker();
    procedure InitPanelLaps();
    procedure InitPanelDnsProperties();
    procedure InitPanelDnsRecord();
    procedure UpdatePanelAttributes();
    procedure UpdatePanelAttributesWithSchema(options: TAttributEditorFilters);
    procedure UpdatePanelAttributesWithoutSchema(options: TAttributEditorFilters);

    procedure InitCountryCodes();
    procedure EditChange(AttributeName: RawUtf8; AValue: RawUtf8);

    procedure UpdateAttribute(Attribute: RawUtf8; value: TRawUtf8DynArray);
    function AddAttribute(Attribute: RawUtf8; value: RawUtf8): Integer;
    procedure RemoveAttribute(Attribute: RawUtf8; value: RawUtf8);
    procedure RemoveAttributeIndex(Attribute: RawUtf8; index: Integer);
    procedure DeleteAttribute(Attribute: RawUtf8);
    function GetAttribute(Attribute: RawUtf8): TRawUtf8DynArray;
    function GetAttributeAsRawUtf8(Attribute: RawUtf8): RawUtf8;
    function GetAttributeIndex(Attribute: RawUtf8; index: Integer; default: RawUtf8=''): RawUtf8;
  public
    Ldap: TLdapClient;
    CountryCodes: TDocVariantData;
    Attributes, ManagedByAttributes: TLdapAttributeList;
    LdapDiff: TLdiff;
    SecurityDescriptor: TSecurityDescriptor;
    DistinguishedName, PrimaryGroupDN, ObjClass: string;
    constructor Create(TheOwner: TComponent; ACore: ICore; _Ldap: TLdapClient; DN: RawUtf8); reintroduce;
    destructor Destroy(); override;
  end;

const
  ACCOUNTDISABLE             = $000002;
  PASSWD_CANT_CHANGE         = $000040;
  ENCRYPTED_TEXT_PWD_ALLOWED = $000080;
  DONT_EXPIRE_PASSWORD       = $010000;
  SMARTCARD_REQUIRED         = $040000;
  NOT_DELEGATED              = $100000;
  USE_DES_KEY_ONLY           = $200000;
  DONT_REQUIRE_PREAUTH       = $400000;
  AES128 = $08;
  AES256 = $10;

  /// standard long identifier of Certificate usage
  // - i.e. match OpenSSL PX509.ExtendedKeyUsage/KeyUsage text
  CEKU_FULLTEXT: array[TXExtendedKeyUsage] of RawUtf8 = (
    'None',                                       //xkuNone
    'Server Authentification',                    //xkuServerAuth
    'Client Authentification',                    //xkuClientAuth
    'Code Signing',                               //xkuCodeSigning
    'Email Protection',                           //xkuEmailProtection
    'Time Stamping',                              //xkuTimeStamping
    'Online Certificate Status Protocol Signing', //xkuOcspSigning
    'Ms Publisher');                              //xkuMsPublisher

  SELF_MEMBERSHIP_TEXT_GUID = '{bf9679c0-0de6-11d0-a285-00aa003049e2}';

  WELL_KNOWN_SID_NAMES: array[TWellKnownSid] of String = (
    rsWellKnownSidNull,                                            // wksNull,
    rsWellKnownSidWorld,                                           // wksWorld,
    rsWellKnownSidLocal,                                           // wksLocal,
    rsWellKnownSidConsoleLogon,                                    // wksConsoleLogon,
    rsWellKnownSidCreatorOwner,                                    // wksCreatorOwner,
    rsWellKnownSidCreatorGroup,                                    // wksCreatorGroup,
    rsWellKnownSidCreatorOwnerServer,                              // wksCreatorOwnerServer,
    rsWellKnownSidCreatorGroupServer,                              // wksCreatorGroupServer,
    rsWellKnownSidCreatorOwnerRights,                              // wksCreatorOwnerRights,
    rsWellKnownSidIntegrityUntrusted,                              // wksIntegrityUntrusted,
    rsWellKnownSidIntegrityLow,                                    // wksIntegrityLow,
    rsWellKnownSidIntegrityMedium,                                 // wksIntegrityMedium,
    rsWellKnownSidIntegrityMediumPlus,                             // wksIntegrityMediumPlus,
    rsWellKnownSidIntegrityHigh,                                   // wksIntegrityHigh,
    rsWellKnownSidIntegritySystem,                                 // wksIntegritySystem,
    rsWellKnownSidIntegrityProtectedProcess,                       // wksIntegrityProtectedProcess,
    rsWellKnownSidIntegritySecureProcess,                          // wksIntegritySecureProcess,
    rsWellKnownSidAuthenticationAuthorityAsserted,                 // wksAuthenticationAuthorityAsserted,
    rsWellKnownSidAuthenticationServiceAsserted,                   // wksAuthenticationServiceAsserted,
    rsWellKnownSidAuthenticationFreshKeyAuth,                      // wksAuthenticationFreshKeyAuth,
    rsWellKnownSidAuthenticationKeyTrust,                          // wksAuthenticationKeyTrust,
    rsWellKnownSidAuthenticationKeyPropertyMfa,                    // wksAuthenticationKeyPropertyMfa,
    rsWellKnownSidAuthenticationKeyPropertyAttestation,            // wksAuthenticationKeyPropertyAttestation,
    rsWellKnownSidNtAuthority,                                     // wksNtAuthority,
    rsWellKnownSidDialup,                                          // wksDialup,
    rsWellKnownSidNetwork,                                         // wksNetwork,
    rsWellKnownSidBatch,                                           // wksBatch,
    rsWellKnownSidInteractive,                                     // wksInteractive,
    rsWellKnownSidService,                                         // wksService,
    rsWellKnownSidAnonymous,                                       // wksAnonymous,
    rsWellKnownSidProxy,                                           // wksProxy,
    rsWellKnownSidEnterpriseControllers,                           // wksEnterpriseControllers,
    rsWellKnownSidSelf,                                            // wksSelf,
    rsWellKnownSidAuthenticatedUser,                               // wksAuthenticatedUser,
    rsWellKnownSidRestrictedCode,                                  // wksRestrictedCode,
    rsWellKnownSidTerminalServer,                                  // wksTerminalServer,
    rsWellKnownSidRemoteLogonId,                                   // wksRemoteLogonId,
    rsWellKnownSidThisOrganisation,                                // wksThisOrganisation,
    rsWellKnownSidIisUser,                                         // wksIisUser,
    rsWellKnownSidLocalSystem,                                     // wksLocalSystem,
    rsWellKnownSidLocalService,                                    // wksLocalService,
    rsWellKnownSidNetworkService,                                  // wksNetworkService,
    rsWellKnownSidLocalAccount,                                    // wksLocalAccount,
    rsWellKnownSidLocalAccountAndAdministrator,                    // wksLocalAccountAndAdministrator,
    rsWellKnownSidBuiltinDomain,                                   // wksBuiltinDomain,
    rsWellKnownSidBuiltinAdministrators,                           // wksBuiltinAdministrators,
    rsWellKnownSidBuiltinUsers,                                    // wksBuiltinUsers,
    rsWellKnownSidBuiltinGuests,                                   // wksBuiltinGuests,
    rsWellKnownSidBuiltinPowerUsers,                               // wksBuiltinPowerUsers,
    rsWellKnownSidBuiltinAccountOperators,                         // wksBuiltinAccountOperators,
    rsWellKnownSidBuiltinSystemOperators,                          // wksBuiltinSystemOperators,
    rsWellKnownSidBuiltinPrintOperators,                           // wksBuiltinPrintOperators,
    rsWellKnownSidBuiltinBackupOperators,                          // wksBuiltinBackupOperators,
    rsWellKnownSidBuiltinReplicator,                               // wksBuiltinReplicator,
    rsWellKnownSidBuiltinRasServers,                               // wksBuiltinRasServers,
    rsWellKnownSidBuiltinPreWindows2000CompatibleAccess,           // wksBuiltinPreWindows2000CompatibleAccess,
    rsWellKnownSidBuiltinRemoteDesktopUsers,                       // wksBuiltinRemoteDesktopUsers,
    rsWellKnownSidBuiltinNetworkConfigurationOperators,            // wksBuiltinNetworkConfigurationOperators,
    rsWellKnownSidBuiltinIncomingForestTrustBuilders,              // wksBuiltinIncomingForestTrustBuilders,
    rsWellKnownSidBuiltinPerfMonitoringUsers,                      // wksBuiltinPerfMonitoringUsers,
    rsWellKnownSidBuiltinPerfLoggingUsers,                         // wksBuiltinPerfLoggingUsers,
    rsWellKnownSidBuiltinAuthorizationAccess,                      // wksBuiltinAuthorizationAccess,
    rsWellKnownSidBuiltinTerminalServerLicenseServers,             // wksBuiltinTerminalServerLicenseServers,
    rsWellKnownSidBuiltinDcomUsers,                                // wksBuiltinDcomUsers,
    rsWellKnownSidBuiltinIUsers,                                   // wksBuiltinIUsers,
    rsWellKnownSidBuiltinCryptoOperators,                          // wksBuiltinCryptoOperators,
    rsWellKnownSidBuiltinUnknown,                                  // wksBuiltinUnknown,
    rsWellKnownSidBuiltinCacheablePrincipalsGroups,                // wksBuiltinCacheablePrincipalsGroups,
    rsWellKnownSidBuiltinNonCacheablePrincipalsGroups,             // wksBuiltinNonCacheablePrincipalsGroups,
    rsWellKnownSidBuiltinEventLogReadersGroup,                     // wksBuiltinEventLogReadersGroup,
    rsWellKnownSidBuiltinCertSvcDComAccessGroup,                   // wksBuiltinCertSvcDComAccessGroup,
    rsWellKnownSidBuiltinRdsRemoteAccessServers,                   // wksBuiltinRdsRemoteAccessServers,
    rsWellKnownSidBuiltinRdsEndpointServers,                       // wksBuiltinRdsEndpointServers,
    rsWellKnownSidBuiltinRdsManagementServers,                     // wksBuiltinRdsManagementServers,
    rsWellKnownSidBuiltinHyperVAdmins,                             // wksBuiltinHyperVAdmins,
    rsWellKnownSidBuiltinAccessControlAssistanceOperators,         // wksBuiltinAccessControlAssistanceOperators,
    rsWellKnownSidBuiltinRemoteManagementUsers,                    // wksBuiltinRemoteManagementUsers,
    rsWellKnownSidBuiltinDefaultSystemManagedGroup,                // wksBuiltinDefaultSystemManagedGroup,
    rsWellKnownSidBuiltinStorageReplicaAdmins,                     // wksBuiltinStorageReplicaAdmins,
    rsWellKnownSidBuiltinDeviceOwners,                             // wksBuiltinDeviceOwners,
    rsWellKnownSidBuiltinWriteRestrictedCode,                      // wksBuiltinWriteRestrictedCode,
    rsWellKnownSidBuiltinUserModeDriver,                           // wksBuiltinUserModeDriver,
    rsWellKnownSidCapabilityInternetClient,                        // wksCapabilityInternetClient,
    rsWellKnownSidCapabilityInternetClientServer,                  // wksCapabilityInternetClientServer,
    rsWellKnownSidCapabilityPrivateNetworkClientServer,            // wksCapabilityPrivateNetworkClientServer,
    rsWellKnownSidCapabilityPicturesLibrary,                       // wksCapabilityPicturesLibrary,
    rsWellKnownSidCapabilityVideosLibrary,                         // wksCapabilityVideosLibrary,
    rsWellKnownSidCapabilityMusicLibrary,                          // wksCapabilityMusicLibrary,
    rsWellKnownSidCapabilityDocumentsLibrary,                      // wksCapabilityDocumentsLibrary,
    rsWellKnownSidCapabilityEnterpriseAuthentication,              // wksCapabilityEnterpriseAuthentication,
    rsWellKnownSidCapabilitySharedUserCertificates,                // wksCapabilitySharedUserCertificates,
    rsWellKnownSidCapabilityRemovableStorage,                      // wksCapabilityRemovableStorage,
    rsWellKnownSidCapabilityAppointments,                          // wksCapabilityAppointments,
    rsWellKnownSidCapabilityContacts,                              // wksCapabilityContacts,
    rsWellKnownSidBuiltinAnyPackage,                               // wksBuiltinAnyPackage,
    rsWellKnownSidBuiltinAnyRestrictedPackage,                     // wksBuiltinAnyRestrictedPackage,
    rsWellKnownSidNtlmAuthentication,                              // wksNtlmAuthentication,
    rsWellKnownSidSChannelAuthentication,                          // wksSChannelAuthentication,
    rsWellKnownSidDigestAuthentication                             // wksDigestAuthentication,
  );

implementation
uses
  DateUtils,
  LCLIntf,
  LCLType,
  variants,
  mormot.core.data,
  mormot.core.text,
  mormot.core.os,
  mormot.crypt.secure,
  uOmniselect,
  uvispropertieslist,
  uvisadvancedsecurity,
  uvislogonhours,
  uvislogonworkstation,
  uvislistother,
  uvisattributeeditor,
  ufrmcore,
  ursatldapclient,
  udns;

{$R *.lfm}

{ LdapDiff }

procedure TLdiff.Initialize();
begin
  self.add := TLdapAttributeList.Create();
  self.update := TLdapAttributeList.Create();
  self.delete := TLdapAttributeList.Create();
end;

procedure TLdiff.Finalize();
begin
  FreeAndNil(self.add);
  FreeAndNil(self.update);
  FreeAndNil(self.delete);
end;

procedure TLdiff.Commit(Attributes: TLdapAttributeList; n: string;
  const data: TLdapAttribute);
var
  attr: TLdapAttribute;
begin
  self.Revert(n);

  if not Assigned(data) then
    Exit;

  attr := Attributes.Find(n);

  if Assigned(attr) then
  begin
    if AttributesEquals(attr, data) then
      Exit;
    if data.Count = 0 then
      self.delete.Add(n).Assign(attr)
    else
      self.update.Add(n).Assign(data);
  end
  else
    if data.Count <> 0 then
      self.add.Add(n).Assign(data);
end;

procedure TLdiff.Commit(Attributes: TLdapAttributeList; n: TLdapAttributeType;
  const data: TLdapAttribute);
var
  attr: TLdapAttribute;
begin
  self.Revert(n);

  if not Assigned(data) then
    Exit;

  attr := Attributes.Find(n);

  if Assigned(attr) then
  begin
    if AttributesEquals(attr, data) then
      Exit;
    if data.Count = 0 then
      self.delete.Add(n).Assign(attr)
    else
      self.update.Add(n).Assign(data);
  end
  else
    if data.Count <> 0 then
      self.add.Add(n).Assign(data);
end;

function TLdiff.Get(Attributes: TLdapAttributeList; n: string): TLdapAttribute;
begin
  result := self.add.Find(n);
  if Assigned(result) then
    Exit;

  result := self.update.Find(n);
  if Assigned(result) then
    Exit;

  result := self.delete.Find(n);
  if Assigned(result) then
    Exit;

  if Assigned(Attributes) then
  begin
    result := Attributes.Find(n);
    if Assigned(result) then
      Exit;
  end;

  result := nil
end;

function TLdiff.Get(Attributes: TLdapAttributeList; n: TLdapAttributeType): TLdapAttribute;
begin
  result := self.add.Find(n);
  if Assigned(result) then
    Exit;

  result := self.update.Find(n);
  if Assigned(result) then
    Exit;

  result := self.delete.Find(n);
  if Assigned(result) then
    Exit;

  if Assigned(Attributes) then
  begin
    result := Attributes.Find(n);
    if Assigned(result) then
      Exit;
  end;

  result := nil
end;

function TLdiff.IsEmpty(): Boolean;
begin
  result := (self.add.Count = 0) and (self.update.Count = 0) and (self.delete.Count = 0);
end;

procedure TLdiff.Clear();
begin
  self.add.Clear();
  self.update.Clear();
  self.delete.Clear();
end;

procedure TLdiff.Revert(n: String);
begin
  self.add.Delete((n));
  self.update.Delete((n));
  self.delete.Delete((n));
end;

procedure TLdiff.Revert(n: TLdapAttributeType);
begin
  self.add.Delete((n));
  self.update.Delete((n));
  self.delete.Delete((n));
end;

{ TVisProperties - private}

procedure TVisProperties.GetMemberOf(attrName: RawUtf8; Doc: PDocVariantData);
var
  at: PtrInt;
  i: Integer;
  FullCN, NameProp: String;
  arr: array of String;
  line: TDocVariantData;
begin
  if not Assigned(Doc) then
    Exit;
  Doc^.InitArray([]);

  at := Attributes.FindIndex(attrName);
  if at = -1 then
    Exit;

  for i := 0 to Attributes.Items[at].Count - 1 do
  begin
    FullCN := DNToCN(Attributes.Items[at].List[i]);
    arr := FullCN.Split(['/']);
    NameProp := arr[Length(arr) - 1];
    SetLength(FullCN, Length(FullCN) - (Length(NameProp) + 1));

    line.InitObject([
      'name', NameProp,
      'ADSF', FullCN,
      'distinguishedName', Attributes.Items[at].List[i]],
      [dvoCheckForDuplicatedNames]);
    Doc^.AddItem(line);
    line.Clear();
  end;
end;

function TVisProperties.GetPrimaryGroupDN(primaryGroupID: Integer): String;
var
  DomainSID, filter: RawUtf8;
  P: PUtf8Char;
  SID: TSid;
  res: TLdapResult;
begin
  result := '';
  if (primaryGroupID = -1) then
    Exit;
  // get DefaultDN SID
  res := Ldap.SearchObject(Ldap.DefaultDN(), '', ['objectSid']);
  if res = nil then
  begin
    ShowLdapSearchError(Ldap);
    Exit;
  end;
  res.CopyObjectSid(DomainSID);
  P := @DomainSID[1];
  TextToSid(P, SID);
  Inc(SID.SubAuthorityCount);
  SID.SubAuthority[SID.SubAuthorityCount - 1] := primaryGroupID;
  // get primary group distinguished name
  filter := FormatUtf8('(objectSid=%)', [LdapEscape(SidToText(@SID))]);
  res := Ldap.SearchObject(Ldap.DefaultDN(), filter, ['distinguishedName'], lssWholeSubtree);
  if res = nil then
  begin
    ShowLdapSearchError(Ldap);
    Exit;
  end;
  result := res.Attributes.GetByName('distinguishedName');
end;

// Panels

procedure TVisProperties.InitPanelDefaultGeneral();
begin
  Tab_DefaultGeneral.TabVisible := True;
  PageControl.ActivePage := Tab_DefaultGeneral;

  Edit_dft_cn.Text := GetAttributeIndex('name', 0);
  Edit_dft_description.Text := GetAttributeIndex('description', 0);
end;

procedure TVisProperties.InitPanelUserGeneral();
begin
  Tab_UserGeneral.TabVisible := True;
  PageControl.ActivePage := Tab_UserGeneral;

  Edit_usr_cn.Text                          := GetAttributeIndex('name', 0);
  Edit_usr_givenName.Text                   := GetAttributeIndex('givenName', 0);
  Edit_usr_initials.Text                    := GetAttributeIndex('initials', 0);
  Edit_usr_sn.Text                          := GetAttributeIndex('sn', 0);
  Edit_usr_displayName.Text                 := GetAttributeIndex('displayName', 0);
  Edit_usr_description.Text                 := GetAttributeIndex('description', 0);
  Edit_usr_physicalDeliveryOfficeName.Text  := GetAttributeIndex('physicalDeliveryOfficeName', 0);
  Edit_usr_telephoneNumber.Text             := GetAttributeIndex('telephoneNumber', 0);
  Edit_usr_mail.Text                        := GetAttributeIndex('mail', 0);
  Edit_usr_wWWHomePage.Text                 := GetAttributeIndex('wWWHomePage', 0);
end;

procedure TVisProperties.InitPanelgroupGeneral();
var
  i: Integer;
  GTArray: TGroupTypes;
begin
  Tab_GroupGeneral.TabVisible := True;
  PageControl.ActivePage := Tab_GroupGeneral;

  Edit_grp_cn.Text             := GetAttributeIndex('name', 0);
  Edit_grp_sAMAccountName.Text := GetAttributeIndex('sAMAccountName', 0);
  Edit_grp_description.Text    := GetAttributeIndex('description', 0);
  Edit_grp_mail.Text           := GetAttributeIndex('mail', 0);
  // https://learn.microsoft.com/en-us/windows/win32/adschema/a-grouptype#remarks
  i := String.ToInteger(GetAttributeIndex('groupType', 0));
  GTArray := GroupTypesFromInteger(i);
  RadioBtn_Local.Checked :=     (gtDomainLocal in GTArray);
  RadioBtn_Global.Checked :=    (gtGlobal in GTArray);
  RadioBtn_Universal.Checked := (gtUniversal in GTArray);
  RadioBtn_Security.Checked :=  (gtSecurity in GTArray); // security group or not
  RadioBtn_Distribution.Checked := not RadioBtn_Security.Checked;
  Memo_grp_info.Lines.Text := GetAttributeIndex('info', 0);
end;

procedure TVisProperties.InitPanelComputerGeneral();
var
  UAC: TUserAccountControls;
  SitesDN: RawUtf8;
  res: TLdapResult;
  pairs: TNameValueDNs;
  filter: String;
begin
  Tab_ComputerGeneral.TabVisible := True;
  PageControl.ActivePage := Tab_ComputerGeneral;

  Edit_cmp_cn.Text             := GetAttributeIndex('cn', 0);
  Edit_cmp_sAMAccountName.Text := GetAttributeIndex('sAMAccountName', 0);
  Edit_cmp_dNSHostName.Text    := GetAttributeIndex('dNSHostName', 0);

  UAC := UserAccountControlsFromText(Attributes.Get(atUserAccountControl));
  if uacWorkstationTrusted in UAC then
    Edit_cmp_dcType.Text := rsUACWorkstation
  else if uacServerTrusted in uac then
    Edit_cmp_dcType.Text := rsUACServer
  else
    Edit_cmp_dcType.Text := '';

  // Get all sites's servers
  SitesDN := FormatUtf8('CN=Sites,%', [Ldap.ConfigDN]);
  filter := '(serverReference=*)';
  Ldap.SearchBegin({VisMain.Storage.Options.SearchPageSize});
  try
    Ldap.SearchScope := lssWholeSubtree;
    if not Ldap.Search(SitesDN, False, filter, ['serverReference']) then
    begin
      ShowLdapSearchError(Ldap);
      Exit;
    end;
    // Find Server
    for res in Ldap.SearchResult.Items do
      if res.Attributes.GetByName('serverReference') = DistinguishedName then
      begin
        ParseDN(res.ObjectName, pairs);
        Edit_cmp_site.Text := pairs[2].Value;
        break;
      end;
  finally
    Ldap.SearchEnd;
  end;

  Edit_cmp_description.Text    := GetAttributeIndex('description', 0);
end;

procedure TVisProperties.InitPanelOUGeneral();
var
  s: String;
  doc: PDocVariantData;
  i: Integer;
begin
  Tab_OUGeneral.TabVisible := True;
  PageControl.ActivePage := Tab_OUGeneral;

  InitCountryCodes();
  Edit_ogu_name.Text := GetAttributeIndex('name', 0);
  Edit_ogu_description.Text := GetAttributeIndex('description', 0);
  Memo_ogu_street.Text := GetAttributeAsRawUtf8('street');
  Edit_ogu_l.Text := GetAttributeIndex('l', 0);
  Edit_ogu_st.Text := GetAttributeIndex('st', 0);
  Edit_ogu_postalCode.Text := GetAttributeIndex('postalCode', 0);
  ComboBox_ogu_co.ItemIndex := ComboBox_ogu_co.Items.Add(GetAttributeIndex('co', 0));

  s := ComboBox_ogu_co.Items[0];
  ComboBox_ogu_co.Clear();
  for doc in CountryCodes.Objects do
  begin
    i := ComboBox_ogu_co.Items.Add(doc^.S['name']);
    if s = doc^.S['name'] then
      ComboBox_ogu_co.ItemIndex := i;
  end;
end;

procedure TVisProperties.InitPanelVolumeGeneral();
begin
  Tab_VolumeGeneral.TabVisible := True;
  PageControl.ActivePage := Tab_VolumeGeneral;

  Edit_vlm_cn.Text := GetAttributeIndex('name', 0);
  Edit_vlm_description.Text := GetAttributeIndex('description', 0);
  Edit_vlm_uNCName.Text := GetAttributeIndex('uNCName', 0);
end;

procedure TVisProperties.InitPanelSiteGeneral();
var
  SearchResult: TLdapResult;
begin
  Tab_SiteGeneral.TabVisible := True;
  PageControl.ActivePage := Tab_SiteGeneral;

  Edit_vlm_cn1.Text := GetAttributeIndex('name', 0);
  Edit_vlm_description1.Text := GetAttributeIndex('description', 0);

  Ldap.SearchBegin();
  ListBox1.Items.BeginUpdate;
  try
    Ldap.SearchScope := lssSingleLevel;
    repeat
      if not Ldap.Search(Format('CN=Subnets,CN=Sites,%s', [Ldap.ConfigDN]), False, Format('(siteObject=%s)', [DistinguishedName]), ['cn']) then
      begin
        ShowLdapSearchError(Ldap);
        Exit;
      end;

      for SearchResult in Ldap.SearchResult.Items do
      begin
        if not Assigned(SearchResult) then
          continue;
        ListBox1.Items.Add(SearchResult.Find('cn').GetReadable());
      end;
    until Ldap.SearchCookie = '';
  finally
    ListBox1.Items.EndUpdate;
    Ldap.SearchEnd;
  end;
end;

procedure TVisProperties.InitPanelSubnetGeneral();
var
  SiteObject, SiteName, SiteDistinguishedName: RawUtf8;
  SearchResult: TLdapResult;
begin
  Tab_SubnetGeneral.TabVisible := True;

  fSubnetInfo.Init();
  Ldap.SearchBegin();
  ComboBox_subnet_site.Items.Clear;
  ComboBox_subnet_site.Items.BeginUpdate;
  try
    Ldap.SearchScope := lssWholeSubtree;

    repeat
      if not Ldap.Search(Format('CN=Sites,%s', [Ldap.ConfigDN]), False, '(objectClass=site)', ['name', 'distinguishedName']) then
      begin
        ShowLdapSearchError(Ldap);
        Exit;
      end;

      ComboBox_subnet_site.Items.Add('');
      for SearchResult in Ldap.SearchResult.Items do
      begin
        if not Assigned(SearchResult) then
          continue;
        SiteName := SearchResult.Find('name').GetReadable();
        SiteDistinguishedName := SearchResult.Find('distinguishedName').GetReadable();
        ComboBox_subnet_site.Items.Add(SiteName);
        fSubnetInfo.S[SiteName] := SiteDistinguishedName;
        fSubnetInfo.S[SiteDistinguishedName] := SiteName;
      end;
    until Ldap.SearchCookie = '';
  finally
    ComboBox_subnet_site.Items.EndUpdate;
    Ldap.SearchEnd;
  end;
  Edit_subnet_description.Text := GetAttributeIndex('description', 0);
  SiteObject := GetAttributeIndex('siteObject', 0);
  if not (SiteObject = '') then
    ComboBox_subnet_site.Text := fSubnetInfo.S[SiteObject];
  Edit_subnet_prefix.Text := GetAttributeIndex('name', 0);
end;

procedure TVisProperties.InitPanelOperatingSystem();
begin
  Tab_OperatingSystem.TabVisible := True;

  Edit_ops_operatingSystem.Text := GetAttributeIndex('operatingSystem', 0);
  Edit_ops_operatingSystemVersion.Text := GetAttributeIndex('operatingSystemVersion', 0);
  Edit_ops_operatingSystemServicePack.Text := GetAttributeIndex('operatingSystemServicePack', 0);
end;

procedure TVisProperties.InitPanelAddress();
begin
  Tab_Address.TabVisible := True;
  InitCountryCodes();
  Memo_adr_streetAddress.Text   := GetAttributeIndex('streetAddress', 0);
  Edit_adr_postOfficeBox.Text   := GetAttributeIndex('postOfficeBox', 0);
  Edit_adr_l.Text               := GetAttributeIndex('l', 0);
  Edit_adr_st.Text              := GetAttributeIndex('st', 0);
  Edit_adr_PostalCode.Text      := GetAttributeIndex('PostalCode', 0);
  ComboBox_adr_CountryCode.ItemIndex := ComboBox_adr_CountryCode.Items.Add(GetAttributeIndex('co', 0));
end;

procedure TVisProperties.InitPanelAccount();
var
  UPN, AE: String;
  UAC, msDS_SET, AceSelf, AceWorld: Integer;
  SecDesc: TSecurityDescriptor;
  SearchResult: TLdapResult;
  item: RawUtf8;
  UPNSplitted: TStringArray;
begin
  Tab_Account.TabVisible := True;

  // get userPrincipalName
  ComboBox_acc_Domain.Items.BeginUpdate;
  Ldap.SearchBegin();
  try
    Ldap.SearchScope := lssWholeSubtree;

    SearchResult := Ldap.SearchObject(FormatUtf8('CN=Partitions,%', [Ldap.ConfigDN]), '', ['uPNSuffixes']);
    if not Assigned(SearchResult) then
    begin
      ShowLdapSearchError(Ldap);
      Exit;
    end;

    ComboBox_acc_Domain.Items.Add('@' + DNToCN(Ldap.DefaultDN));
    if (Ldap.DefaultDN <> Ldap.RootDN) then
      ComboBox_acc_Domain.Items.Add('@' + DNToCN(Ldap.RootDN));
    for item in SearchResult.Find('uPNSuffixes').GetAllReadable do
      ComboBox_acc_Domain.Items.Add('@' + item);
  finally
    Ldap.SearchEnd;
    ComboBox_acc_Domain.Items.EndUpdate;
  end;

  UPN := GetAttributeIndex('userPrincipalName', 0, '@');
  UPNSplitted := UPN.Split('@');
  if (UPN <> '@') and (Length(UPNSplitted) = 2) then
  begin
    ComboBox_acc_Domain.ItemIndex := ComboBox_acc_Domain.Items.IndexOf('@' + UPNSplitted[1]);
    Edit_acc_Name.Text := UPNSplitted[0];
  end
  else
  begin
    ComboBox_acc_Domain.ItemIndex := -1;
    Edit_acc_Name.Text := '';
  end;

  // sAMAccountName
  Edit_acc_sAMAccountName.Text := GetAttributeIndex('sAMAccountName', 0);
  Edit_acc_SAMDomain.Text      := Ldap.NetbiosDN + '\';
  // 'Logon hours' & 'Log on to...' are handled in there OnClick
  CheckBox_acc_Unlock.Checked  := False;

  // Account Options
  // https://ldapwiki.com/wiki/Wiki.jsp?page=User-Account-Control%20Attribute
  UAC      := String(GetAttributeIndex('userAccountControl', 0, '0')).ToInteger();
  // https://ldapwiki.com/wiki/Wiki.jsp?page=MsDS-SupportedEncryptionTypes
  msDS_SET := String(GetAttributeIndex('msDS-SupportedEncryptionTypes', 0, '0')).ToInteger();
  CheckBox_acc_opt_MustChange.Checked   := GetAttributeIndex('pwdLastSet', 0, '0') = '0';
  CheckBox_acc_opt_CannotChange.Checked          := (UAC and PASSWD_CANT_CHANGE)         > 0; // readonly
  CheckBox_acc_opt_NeverExpires.Checked          := (UAC and DONT_EXPIRE_PASSWORD)       > 0;
  CheckBox_acc_opt_ReversibleEncryption.Checked  := (UAC and ENCRYPTED_TEXT_PWD_ALLOWED) > 0;
  CheckBox_acc_opt_Disabled.Checked              := (UAC and ACCOUNTDISABLE)             > 0;
  CheckBox_acc_opt_SmartCard.Checked             := (UAC and SMARTCARD_REQUIRED)         > 0;
  CheckBox_acc_opt_Sensitive.Checked             := (UAC and NOT_DELEGATED)              > 0;
  CheckBox_acc_opt_KerberosDESEncryption.Checked := (UAC and USE_DES_KEY_ONLY)           > 0;
  CheckBox_acc_opt_KerberosAES128Encryption.Checked := (msDS_SET and AES128) > 0;
  CheckBox_acc_opt_KerberosAES256Encryption.Checked := (msDS_SET and AES256) > 0;
  CheckBox_acc_opt_NoKerberosPreauth.Checked     := (UAC and DONT_REQUIRE_PREAUTH)       > 0;

  // accountExpires
  // 0 and 9223372036854775807 (0x7FFFFFFFFFFFFFFF) mean "never expire"
  // 0 will be used as default value
  AE := GetAttributeIndex('accountExpires', 0, '0');
  if (AE = '0') or (AE = '9223372036854775807') then
    DateTimePicker_acc_Expires.DateTime := Now()
  else
    DateTimePicker_acc_Expires.DateTime := MSTimeToDateTime(AE.ToInt64());
  RadioButton_acc_Never.Checked := (AE = '0') or (AE = '9223372036854775807');
  RadioButton_acc_EndOf.Checked := not RadioButton_acc_Never.Checked;
  DateTimePicker_acc_Expires.Enabled := RadioButton_acc_EndOf.Checked;

  // https://learn.microsoft.com/fr-fr/windows/win32/adsi/reading-user-cannot-change-password-ldap-provider
  if not CheckBox_acc_opt_CannotChange.Checked then // obtain it the OTHER way...
  begin
    // Get SecDesc
    if not SecDesc.FromBinary(Attributes.Find(atNTSecurityDescriptor).GetRaw()) then
    begin
      Dialogs.MessageDlg(rsTitleParsing, rsACEParsing, mtError, [mbOK], 0);
      Exit;
    end;

    AceSelf  := SecDescFindACE(@SecDesc,
      satObjectAccessDenied, KnownRawSid(wksSelf),
      [samControlAccess], @ATTR_UUID[kaUserChangePassword]);
    AceWorld := SecDescFindACE(@SecDesc,
      satObjectAccessDenied, KnownRawSid(wksWorld),
      [samControlAccess], @ATTR_UUID[kaUserChangePassword]);

    CheckBox_acc_opt_CannotChange.Checked := (AceSelf <> -1) and (AceWorld <> -1);
  end;
end;

procedure TVisProperties.InitPanelProfile();
begin
  Tab_Profile.TabVisible := True;

  Edit_pro_profilePath.Text   := GetAttributeIndex('profilePath', 0);
  Edit_pro_scriptPath.Text    := GetAttributeIndex('scriptPath', 0);
  ComboBox_pro_ConnectTo.ItemIndex  := ComboBox_pro_ConnectTo.Items.IndexOf(GetAttributeIndex('homeDrive', 0));
  RadioButton_pro_homeDirectory.Checked := ComboBox_pro_ConnectTo.ItemIndex = -1;
  Edit_pro_homeDirectory.Enabled        := ComboBox_pro_ConnectTo.ItemIndex = -1;
  RadioButton_pro_ConnectTo.Checked := ComboBox_pro_ConnectTo.ItemIndex <> -1;
  ComboBox_pro_ConnectTo.Enabled    := ComboBox_pro_ConnectTo.ItemIndex <> -1;
  Label_pro_ConnectTo.Enabled       := ComboBox_pro_ConnectTo.ItemIndex <> -1;
  Edit_pr0_homeDirectory.Enabled        := ComboBox_pro_ConnectTo.ItemIndex <> -1;
  if ComboBox_pro_ConnectTo.ItemIndex = -1 then
    Edit_pro_homeDirectory.Text := GetAttributeIndex('homeDirectory', 0)
  else
    Edit_pr0_homeDirectory.Text   := GetAttributeIndex('homeDirectory', 0);
end;

procedure TVisProperties.InitPanelTelephone();
begin
  Tab_Telephone.TabVisible := True;

  Edit_tel_homePhone.Text := GetAttributeIndex('homePhone', 0);
  Edit_tel_pager.Text := GetAttributeIndex('pager', 0);
  Edit_tel_mobile.Text := GetAttributeIndex('mobile', 0);
  Edit_tel_facsimileTelephoneNumber.Text := GetAttributeIndex('facsimileTelephoneNumber', 0);
  Edit_tel_ipPhone.Text := GetAttributeIndex('ipPhone', 0);
  Memo_tel_info.Text := GetAttributeIndex('info', 0);
end;

procedure TVisProperties.InitPanelOrganization();
var
  ManagerDN, report: RawUtf8;
  filter: RawUtf8 = '';
  doc, tmp: TDocVariantData;
  id: Integer;
  SearchResult: TLdapResult;
begin
  Tab_Organization.TabVisible := True;

  UnifyButtonsWidth([BitBtn_org_change, BitBtn_org_properties, BitBtn_org_clear]);

  Edit_org_title.Text      := GetAttributeIndex('title', 0);
  Edit_org_department.Text := GetAttributeIndex('department', 0);
  Edit_org_company.Text    := GetAttributeIndex('company', 0);

  ManagerDN := GetAttributeIndex('manager', 0);

  // Set filter
  if ManagerDN <> '' then
    filter := FormatUtf8('(distinguishedName=%)', [LdapEscape(ManagerDN)]);
  for report in GetAttribute('directReports') do
    filter += FormatUtf8('(distinguishedName=%)', [LdapEscape(report)]);

  if filter = '' then // if nothing to do
    Exit;
  filter := FormatUtf8('(|%)', [filter]);

  tmp.init();
  if ManagerDN <> '' then
  begin
    Ldap.SearchBegin();
    try
      Ldap.SearchScope := lssWholeSubtree;

      repeat
        if not Ldap.Search(Ldap.DefaultDN(), False, filter, ['name']) then
        begin
          ShowLdapSearchError(Ldap);
          Exit;
        end;

        for SearchResult in Ldap.SearchResult.Items do
        begin
          if not Assigned(SearchResult) then
            continue;
          tmp.AddValue('name', SearchResult.Find('name').GetReadable());
          if SearchResult.ObjectName = ManagerDN then
            Edit_org_manager.Text := tmp.S['name'];
          Doc.AddItem(tmp);
          tmp.Clear;
        end;
      until Ldap.SearchCookie = '';
    finally
      Ldap.SearchEnd;
    end;
  end;

  if ManagerDN <> '' then
  begin
    id := doc.SearchItemByProp('distinguishedName', ManagerDN, false);
    Edit_org_manager.Text := doc._[id]^.S['name'];
    doc.Delete(id);
  end;

  // Set data
  if Attributes.FindIndex('directReports') <> -1 then
  begin
    TisGrid_org_directReports.Data := doc;
    TisGrid_org_directReports.LoadData();
  end;
end;

procedure TVisProperties.InitPanelPublishedCertfifcates();
var
  Doc: TDocVariantData;
  cert: RawUtf8;
begin
  Tab_PublishedCertificates.TabVisible := True;

  // Buttons
  UnifyButtonsWidth([BitBtn_cer_add, BitBtn_cer_remove, BitBtn_cer_copy]);

  // Cert
  if Attributes.FindIndex('userCertificate') = -1 then
    Exit;
  Doc.InitArray([]);
  for cert in GetAttribute('userCertificate') do
    Doc.AddItem(CertToDoc(cert));
  TisGrid_cer_listX509.Data := Doc;
  TisGrid_cer_listX509.LoadData();
end;

procedure TVisProperties.InitPanelMembers();
begin
  Tab_Member.TabVisible := True;

  TisGrid_mem.BeginUpdate();
  try
    GetMemberOf('member', @TisGrid_mem.Data);
    TisGrid_mem.LoadData();
  finally
    TisGrid_mem.EndUpdate();
  end;
end;

procedure TVisProperties.InitPanelMemberOf();

  function GetPrimaryGroupName(PrimaryGroupDN: RawUtf8): RawUtf8;
  var
    Splitted, SplittedName: TStringArray;
  begin
    Result := '';

    Splitted := String(PrimaryGroupDN).Split(',');
    if not Assigned(Splitted) or (Length(Splitted) <= 0) then
      Exit;

    SplittedName := Splitted[0].Split('=');
    if not Assigned(SplittedName) or (Length(SplittedName) <> 2) then
      Exit;
    Result := SplittedName[1];
  end;

begin
  Tab_MemberOf.TabVisible := True;
  PrimaryGroupDN := GetPrimaryGroupDN(Utf8ToInteger(GetAttributeIndex('primaryGroupID', 0, '-1'), -1));
  if PrimaryGroupDN <> '' then
  begin
    Panel_mof_primaryGroup.Enabled := True;
    Panel_mof_noPrimaryGroup.Visible := False;
    Panel_mof_noPrimaryGroup.Enabled := False;
    Edit_mof_primaryGroup.Text := GetPrimaryGroupName(PrimaryGroupDN);
    AddAttribute('memberOf', PrimaryGroupDN);
  end
  else
  begin
    Panel_mof_noPrimaryGroup.Visible := True;
    Panel_mof_noPrimaryGroup.Enabled := True;
    Panel_mof_primaryGroup.Enabled := False;
    Edit_mof_primaryGroup.Text := '';
  end;
  List_mof.BeginUpdate();
  try
    GetMemberOf('memberOf', @List_mof.Data);
    List_mof.LoadData();
  finally
    List_mof.EndUpdate();
  end;
end;

procedure TVisProperties.InitPanelLocation();
begin
  Tab_Location.TabVisible := True;

  Edit_loc_location.Text := GetAttributeIndex('location', 0);
end;

procedure TVisProperties.InitPanelManagedBy();
var
  managedBy: String;
  res: TLdapResult;
  Sid: RawSid;
  SecDesc: TSecurityDescriptor;
  guid: TGuid;
begin
  Tab_ManagedBy.TabVisible := True;

  UnifyButtonsWidth([Btn_man_properties, Btn_man_modify, Btn_man_clear]);

  managedBy := GetAttributeIndex('managedBy', 0, '');
  if managedBy = '' then
    Exit;

  res := Ldap.SearchObject(managedBy, '', [
    'name',
    'distinguishedName',
    'physicalDeliveryOfficeName',
    'streetAddress',
    'l',
    'st',
    'co',
    'telephoneNumber',
    'facsimileTelephoneNumber',
    'objectClass',
    'objectSid'
  ]);
  if not Assigned(res) then
  begin
    ShowLdapSearchError(Ldap);
    Exit;
  end;
  Sid := res.Attributes.Find(atObjectSid).GetRaw();
  ManagedByAttributes := TLdapAttributeList(res.Attributes.Clone());

  Edit_man_managedBy.Text                  := DNToCN(ManagedByAttributes.Find('distinguishedName').GetRaw());
  Edit_man_physicalDeliveryOfficeName.Text := ManagedByAttributes.GetByName('physicalDeliveryOfficeName');
  Memo_man_streetAddress.Append(              ManagedByAttributes.GetByName('streetAddress'));
  Edit_man_l.Text                          := ManagedByAttributes.GetByName('l');
  Edit_man_st.Text                         := ManagedByAttributes.GetByName('st');
  Edit_man_co.Text                         := ManagedByAttributes.GetByName('co');
  Edit_man_telephoneNumber.Text            := ManagedByAttributes.GetByName('telephoneNumber');
  Edit_man_facsimileTelephoneNumber.Text   := ManagedByAttributes.GetByName('facsimileTelephoneNumber');

  // Manager can update membership list
  // Get user's NTSecurityDescriptor
  if not SecDesc.FromBinary(Attributes.Find(atNTSecurityDescriptor).GetRaw()) then
  begin
    Dialogs.MessageDlg(rsTitleParsing, rsACEParsing, mtError, [mbOK], 0);
    Exit;
  end;

  // Find ACE
  guid := RawUtf8ToGuid(SELF_MEMBERSHIP_TEXT_GUID);
  CheckBox_man.Checked := SecDescFindACE(@SecDesc,
    satObjectAccessAllowed, Sid,
    [samWriteProp], @guid) <> -1;
end;

procedure TVisProperties.InitPanelObject();
var
  SecDesc: TSecurityDescriptor;
  Sid: RawSid;
  FoundSelf, FoundParent: Boolean;
  data: TLdapAttribute;
begin
  Tab_Object.TabVisible := True;
  Edit_obj_cn.Text := DNToCN(DistinguishedName);
  Edit_obj_objectClass.Text   := GetAttributeIndex('objectClass', -1);
  Edit_obj_whenCreated.Text   := ISOToTimeFormat(GetAttributeIndex('whenCreated', 0));
  Edit_obj_whenChanged.Text   := ISOToTimeFormat(GetAttributeIndex('whenChanged', 0));
  Edit_obj_uSNChanged.Text    := GetAttributeIndex('uSNChanged', 0);
  Edit_obj_uSNCreated.Text    := GetAttributeIndex('uSNCreated', 0);

  Sid := KnownRawSid(wksWorld);
  // Self
  if not SecDesc.FromBinary(Attributes.Find(atNTSecurityDescriptor).GetRaw()) then
    Exit;
  FoundSelf := SecDescFindACE(@SecDesc,
    satAccessDenied, Sid,
    [samDelete, samDeleteTree], @ATTR_UUID[kaNull]) <> -1;

  // Parent
  if DistinguishedName = Ldap.DefaultDN() then
    FoundParent := True
  else
  begin
    data := Ldap.SearchObject(atNTSecurityDescriptor, GetParentDN(DistinguishedName), '');
    if not Assigned(data) then
    begin
      ShowLdapSearchError(Ldap);
      Exit;
    end;

    SecDesc.FromBinary(data.GetRaw());
    FoundParent := SecDescFindACE(@SecDesc,
      satAccessDenied, Sid,
      [samDeleteChild], @ATTR_UUID[kaNull]) <> -1;
  end;

  // Protect against accidental deletion
  CheckBox_obj_protect.Checked := FoundSelf and FoundParent;
end;

procedure TVisProperties.InitPanelSecurity();
begin
  if not SecurityDescriptor.FromBinary(Attributes.Find(atNTSecurityDescriptor).GetRaw()) then
    Exit;
  SecurityFillListUser();

  Tab_Security.TabVisible := True;
end;

procedure TVisProperties.InitPanelAttributes();
begin
  Tab_Attributes.TabVisible := True;
  UpdatePanelAttributes();
end;

procedure TVisProperties.InitPanelBitlocker();
var
  data: TDocVariantData;
  item: TLdapResult;
  cn, recoveryPassword, dNSHostName: RawUtf8;
  pageCount: Integer;
begin
  Tab_Bitlocker.TabVisible := True;
  pageCount := 0;
  data.init();
  ldap.SearchBegin({VisMain.Storage.Options.SearchPageSize});
  ldap.SearchScope := lssSingleLevel;
  try
    repeat
      if not Ldap.Search(DistinguishedName, False, 'objectClass=msFVE-RecoveryInformation', ['cn', 'msFVE-RecoveryPassword']) then
      begin
        ShowLdapSearchError(Ldap);
        Exit;
      end;
      TisGrid1.BeginUpdate;
      try
        for item in ldap.SearchResult.Items do
        begin
          cn := item.Find('cn').GetReadable();
          recoveryPassword := item.Find('msFVE-RecoveryPassword').GetReadable();
          dNSHostName := Attributes.Find('dNSHostName').GetReadable();
          data.AddValue('date', String(cn).Substring(0, 25));
          data.AddValue('passwordID', String(cn).Substring(26, 36));
          data.AddValue('recoveryPassword', recoveryPassword);
          data.AddValue('computer', dNSHostName);
          TisGrid1.Data.AddItem(data);
          data.Clear;
        end;
      finally
        TisGrid1.EndUpdate;
        TisGrid1.LoadData();
      end;
      Inc(pageCount);
    until (Ldap.SearchCookie = '') or (pageCount = {VisMain.Storage.Options.SearchPageNumber}1);
  finally
    ldap.SearchEnd;
  end;
end;

procedure TVisProperties.InitPanelLaps();
var
  data: TDocVariantData;
  res: TLdapResult;
  expirationTime, password: TLdapAttribute;
begin
  Tab_LAPS.TabVisible := True;

  res := Ldap.SearchObject(DistinguishedName, '', ['msLAPS-Password', 'ms-Mcs-AdmPwd', 'msLAPS-PasswordExpirationTime', 'ms-Mcs-AdmPwdExpirationTime']);
  if not Assigned(res) then
  begin
    ShowLdapSearchError(Ldap);
    Exit;
  end;
  expirationTime := res.Find('msLAPS-PasswordExpirationTime');
  if not Assigned(expirationTime) then
    expirationTime := res.Find('ms-Mcs-AdmPwdExpirationTime');
  if Assigned(expirationTime) then
    Edit1.Text := expirationTime.GetReadable();

  password := res.Find('msLAPS-Password');
  if Assigned(password) then
  begin
    data.InitJson(password.GetReadable());
    if data.Exists('n') then
      Edit2.Text := data.S['n'];
    if data.Exists('p') then
      Edit3.Text := data.S['p'];
  end else
  begin
    password := res.Find('ms-Mcs-AdmPwd');
    if Assigned(password) then
      Edit3.Text := password.GetReadable();
  end;


end;

procedure TVisProperties.InitPanelDnsProperties();
var
  dnsPropertyAttr: TLdapAttribute;
  i: Integer;
  DNSProperty: TDNSProperty;
  data: TDocVariantData;
begin
  Tab_dnsProperties.TabVisible := True;

  dnsPropertyAttr := Attributes.Find('dNSProperty');

  if not Assigned(dnsPropertyAttr) then
    Exit;

  data.init();
  TisGrid2.Clear;
  TisGrid2.BeginUpdate;
  try
    for i := 0 to dnsPropertyAttr.Count - 1 do
    begin
      if not DNSPropertyBytesToRecord(DNSProperty, PByteArray(dnsPropertyAttr.GetRaw(i))^) then
        Exit;
      data.AddOrUpdateValue('dataLength', DNSProperty.DataLength);
      data.AddOrUpdateValue('nameLength', DNSProperty.NameLength);
      data.AddOrUpdateValue('flag', DNSProperty.Flag);
      data.AddOrUpdateValue('version', DNSProperty.Version);
      data.AddOrUpdateValue('_id', DNSProperty.Id);
      data.AddOrUpdateValue('id', DnsPropertyIdToString(TDnsPropertyId(DNSProperty.Id)));
      data.AddOrUpdateValue('data', DNSPropertyDataToString(DNSProperty));
      data.AddOrUpdateValue('name', DNSProperty.Name);
      TisGrid2.Data.AddItem(data);
      data.clear;
    end;
  finally
    TisGrid2.EndUpdate;
    TisGrid2.LoadData();
  end;
end;

procedure TVisProperties.InitPanelDnsRecord();
var
  dnsRecordsAttr: TLdapAttribute;
  i: Integer;
  DNSRecord: TDNSRecord;
  data: TDocVariantData;
begin
  Tab_dnsRecords.TabVisible := True;

  dnsRecordsAttr := Attributes.Find('dnsRecord');
  if not Assigned(dnsRecordsAttr) then
    Exit;

  data.init();
  TisGrid3.Clear;
  TisGrid3.BeginUpdate;
  try
    for i := 0 to dnsRecordsAttr.Count - 1 do
    begin
      DNSRecordBytesToRecord(DNSRecord, PByteArray(dnsRecordsAttr.GetRaw(i))^);
      data.AddOrUpdateValue('dataLength', DNSRecord.DataLength);
      data.AddOrUpdateValue('type', DNSRecord.RecType);
      data.AddOrUpdateValue('version', DNSRecord.Version);
      data.AddOrUpdateValue('rank', DNSRecord.Rank);
      data.AddOrUpdateValue('flags', DNSRecord.Flags);
      data.AddOrUpdateValue('serial', DNSRecord.Serial);
      data.AddOrUpdateValue('ttlSeconds', DNSRecord.TtlSeconds);
      data.AddOrUpdateValue('reserved', DNSRecord.Reserved);
      data.AddOrUpdateValue('timestamp', DNSRecord.Timestamp);
      data.AddOrUpdateValue('data', DNSRecordDataToString(DNSRecord));
      TisGrid3.Data.AddItem(data);
      data.Clear;
    end;
  finally
    TisGrid3.EndUpdate;
    TisGrid3.LoadData();
  end;
end;

procedure TVisProperties.UpdatePanelAttributes();
var
  options: TAttributEditorFilters;
begin
  options := [];
  if MI_AF_ShowValues.Checked then
    options += [aefOnlyAvailableValues];
  if MI_AF_ShowWritable.Checked then
    options += [aefOnlyWritableValues];
  if MI_AF_Mandatory.Checked then
    options += [aefMandatory];
  if MI_AF_ShowOptional.Checked then
    options += [aefOptional];
  if MI_AF_Constructed.Checked then
    options += [aefConstructed];
  if MI_AF_Backkinks.Checked then
    options += [aefBacklinks];
  if MI_AF_SystemOnly.Checked then
    options += [aefSystemOnly];

  try
    UpdatePanelAttributesWithSchema(options);
  Except
    on E: Exception do
      UpdatePanelAttributesWithoutSchema(options);
  end;
end;

procedure TVisProperties.UpdatePanelAttributesWithSchema(
  options: TAttributEditorFilters);
var
  CategoryObjectAttributeList, ObjectAttributeList,
    objectClass, AuxiliaryObjectClassList, AttributesAlreadyAdded: TRawUtf8DynArray;
  CategoryObjectAttribute, CategoryObjectDN, ObjectAttribute,
    AuxiliaryObjectClass: RawUtf8;
  Attribute: TLdapAttribute;
  SearchResult: TLdapResult;
  data: TDocVariantData;

  function ArrayContains(Arr: TRawUtf8DynArray; Value: RawUtf8): Boolean;
  var
    v: RawUtf8;
  begin
    result := True;
    for v in Arr do
      if v = Value then
        Exit;
    result := False;
  end;

begin
  objectClass := Attributes.GetAll(atObjectClass);

  data.init();
  CategoryObjectAttributeList := [];
  AttributesAlreadyAdded := [];

  AuxiliaryObjectClassList := ['auxiliaryClass', 'systemAuxiliaryClass'];

  if aefMandatory in options then
    CategoryObjectAttributeList := Concat(CategoryObjectAttributeList, ['mustContain', 'systemMustContain']);
  if aefOptional in options then
    CategoryObjectAttributeList := Concat(CategoryObjectAttributeList, ['mayContain', 'systemMayContain']);

  List_Attributes.BeginUpdate();
  try
    List_Attributes.Clear();
    if Assigned(objectClass) and (Length(objectClass) > 0) then
    begin
      repeat
        ObjectAttributeList := [];
        //CategoryObjectDN := fCore.RsatStorage.GetSchemaObjectName('lDAPDisplayName', objectClass[0]);

        //if (CategoryObjectDN = '') then
        //  raise Exception.Create('Cannot retrieve "dn" of: ' + objectClass[0]);

        SearchResult := fCore.LdapClient.SearchObject(FormatUtf8('CN=Schema,%', [fCore.LdapClient.ConfigDN]), FormatUtf8('(lDAPDisplayName=%)', [LdapEscape(objectClass[0])]), Concat(AuxiliaryObjectClassList, CategoryObjectAttributeList), lssSingleLevel);

        for AuxiliaryObjectClass in AuxiliaryObjectClassList do
          objectClass := Concat(objectClass, SearchResult.Find(AuxiliaryObjectClass).GetAllReadable);// fCore.RsatStorage.Storage.GetAll(CategoryObjectDN, AuxiliaryObjectClass));

        for CategoryObjectAttribute in CategoryObjectAttributeList do
          ObjectAttributeList := Concat(ObjectAttributeList, SearchResult.Find(CategoryObjectAttribute).GetAllReadable);// fCore.RsatStorage.Storage.GetAll(CategoryObjectDN, CategoryObjectAttribute));

        for ObjectAttribute in ObjectAttributeList do
        begin
          if ArrayContains(AttributesAlreadyAdded, ObjectAttribute) then
            continue;
          Attribute := Attributes.Find(ObjectAttribute);
          data.AddOrUpdateValue('attribute', ObjectAttribute);
          if not Assigned(Attribute) and ((aefOnlyAvailableValues in options) or (aefOnlyWritableValues in options)) then
            continue;
          if Assigned(Attribute) then
            data.A_['value']^.AddItem(Attribute.GetVariant());
          AttributesAlreadyAdded := Concat(AttributesAlreadyAdded, [ObjectAttribute]);
          List_Attributes.Data.AddItem(data);
          data.clear;
        end;
        Delete(objectClass, 0, 1);
      until (Length(objectClass) <= 0);
    end;
  finally
    List_Attributes.EndUpdate();
    List_Attributes.LoadData();
  end;
end;

procedure TVisProperties.UpdatePanelAttributesWithoutSchema(
  options: TAttributEditorFilters);
var
  attribute: TLdapAttribute;
  data: TDocVariantData;
begin
  data.init();
  List_Attributes.BeginUpdate;
  try
    for attribute in Attributes.Items do
    begin
      if not Assigned(attribute) then
        continue;
      data.AddOrUpdateValue('attribute', attribute.AttributeName);
      data.AddOrUpdateValue('value', attribute.GetVariant());
      List_Attributes.Data.AddItem(data);
      data.Clear;
    end;
  finally
    List_Attributes.EndUpdate;
    List_Attributes.LoadData();
  end;
end;

procedure TVisProperties.InitCountryCodes();
var
  res: TResourceStream;
  s: RawUtf8 = '';
begin
  if not CountryCodes.IsVoid() then
    Exit;
  res := TResourceStream.Create(HInstance, 'COUNTRY_CODES_EN', RT_RCDATA);
  try
    SetLength(s, res.Size);
    res.Read(s[1], res.Size);
    CountryCodes.InitJson(s);
  finally
    FreeAndNil(res);
  end;
end;

procedure TVisProperties.EditChange(AttributeName: RawUtf8; AValue: RawUtf8);
var
  data: TLdapAttribute;
begin
  data := TLdapAttribute.Create(AttributeName, atUndefined);
  try
    if AValue <> '' then
      data.Add(AValue);
    LdapDiff.Commit(Attributes, AttributeName, data);
  finally
    FreeAndNil(data);
  end;
end;

{ TVisProperties - public}
// Form
constructor TVisProperties.Create(TheOwner: TComponent; ACore: ICore;
  _Ldap: TLdapClient; DN: RawUtf8);
var
  res: TLdapResult;
begin
  Inherited Create(TheOwner);

  fCore := ACore;
  Ldap := _Ldap;
  DistinguishedName := DN;
  UnifyButtonsWidth([Btn_BottomApply, Btn_BottomCancel, Btn_BottomOK]);

  IniPropStorage1.IniFileName := MakePath([GetAppConfigDir(false), 'RsatConsole.ini']);

  if not Assigned(Ldap) or
     not Assigned(fCore) or
     (DN = '') then
    Exit;
  // Fetch data
  Ldap.SearchRangeBegin;
  res := Ldap.SearchObject(DistinguishedName, '', ['*']);
  Ldap.SearchRangeEnd;
  if not Assigned(res) then
  begin
    ShowLdapSearchError(Ldap);
    Close();
    Exit;
  end;
  // Fill Attributes
  Attributes := TLdapAttributeList(res.Attributes.Clone());
  ManagedByAttributes := nil;
  // Init
  LdapDiff.Initialize();

  ObjClass := GetAttributeIndex('objectClass', -1);
  IsLoading := True;
  case ObjClass of
  'user':
  begin
    InitPanelUserGeneral();
    InitPanelAddress();
    InitPanelAccount();
    InitPanelProfile();
    InitPanelTelephone();
    InitPanelOrganization();
    InitPanelPublishedCertfifcates();
    InitPanelMemberOf();
    // Password Replication //do last
    InitPanelObject();
    InitPanelSecurity();
    // COM+ //do last
    InitPanelAttributes();
  end;
  'group':
  Begin
    InitPanelgroupGeneral();
    InitPanelMembers();
    InitPanelMemberOf();
    InitPanelManagedBy();
    InitPanelObject();
    InitPanelSecurity();
    InitPanelAttributes();
  end;
  'computer':
  begin
    InitPanelComputerGeneral();
    InitPanelOperatingSystem();
    InitPanelMemberOf();
    // Delegation
    // Password Replication //do last
    // LAPS
    InitPanelLaps();
    InitPanelLocation();
    InitPanelManagedBy();
    InitPanelObject();
    InitPanelSecurity();
    InitPanelAttributes();
    InitPanelBitlocker();
  end;
  'organizationalUnit':
  begin
    InitPanelOUGeneral();
    InitPanelManagedBy();
    InitPanelObject();
    InitPanelSecurity();
    // COM +
    InitPanelAttributes();
  end;
  'contact':
  begin
    InitPanelUserGeneral();
    InitPanelAddress();
    InitPanelTelephone();
    InitPanelOrganization();
    InitPanelMemberOf();
    InitPanelObject();
    InitPanelSecurity();
    InitPanelAttributes();
  end;
  'inetOrgPerson':
  begin
    InitPanelUserGeneral();
    InitPanelAddress();
    InitPanelAccount();
    InitPanelProfile();
    InitPanelTelephone();
    // Password Replication
    InitPanelObject();
    InitPanelSecurity();
    // COM+
    InitPanelAttributes();
    InitPanelOrganization();
    InitPanelPublishedCertfifcates();
    InitPanelMemberOf();
  end;
  'volume':
  begin
    InitPanelVolumeGeneral();
    InitPanelManagedBy();
    InitPanelObject();
    InitPanelSecurity();
    InitPanelAttributes();
  end;
  {$ifdef DEVMODE}
  'dnsZone':
  begin
    InitPanelDefaultGeneral();
    InitPanelObject();
    InitPanelSecurity();
    InitPanelAttributes();
    InitPanelDnsProperties();
  end;
  'dnsNode':
  begin
    InitPanelDefaultGeneral();
    InitPanelObject();
    InitPanelSecurity();
    InitPanelAttributes();
    InitPanelDnsRecord();
  end;
  {$endif}
  'site':
  begin
    InitPanelSiteGeneral();
    InitPanelLocation();
    InitPanelObject();
    InitPanelSecurity();
    InitPanelAttributes();
  end;
  'subnet':
  begin
    InitPanelSubnetGeneral();
    InitPanelLocation();
    InitPanelObject();
    InitPanelSecurity();
    InitPanelAttributes();
  end;
  else
    InitPanelDefaultGeneral();
    InitPanelObject();
    InitPanelSecurity();
    InitPanelAttributes();
  end;
  IsLoading := False;
end;

destructor TVisProperties.Destroy();
begin
  //if Assigned(VisMain.VisPropertiesList) and Assigned(Self) then
  //  VisMain.VisPropertiesList.Close(self);
  FreeAndNil(Attributes);
  if Assigned(ManagedByAttributes) then
    FreeAndNil(ManagedByAttributes);
  LdapDiff.Finalize();
  Inherited;
end;

procedure TVisProperties.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  fCore.CloseProperty(Self);
end;

procedure TVisProperties.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if (ModalResult = mrCancel) and not LdapDiff.IsEmpty() then
  begin
    CanClose := Dialogs.MessageDlg(rsWarning, rsUnsavedChangeQuit, mtWarning, mbYesNo, 0) = mrYes;
    Exit;
   end;
  CanClose := True;
end;

function TVisProperties.FormHelp(Command: Word; Data: PtrInt;
  var CallHelp: Boolean): Boolean;
begin
  result := CallHelp;
  ShowMessage('Help!');
end;

procedure TVisProperties.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    27: Close;
  end;
end;

procedure TVisProperties.FormShow(Sender: TObject);
begin
  MakeFullyVisible;
end;

procedure TVisProperties.List_AttributesDblClick(Sender: TObject);
begin
  Action_AttributesModify.Execute;
end;

// Manipulate Attributes
procedure TVisProperties.UpdateAttribute(Attribute: RawUtf8; value: TRawUtf8DynArray);
var
  attr: TLdapAttribute;
  s: RawUtf8;
begin
  Attributes.Delete(Attribute);

  attr := Attributes.Add(Attribute);
  for s in value do;
    attr.Add(s);
end;

function TVisProperties.AddAttribute(Attribute: RawUtf8; value: RawUtf8): Integer;
var
  attr: TLdapAttribute;
begin
  attr := Attributes.Find(Attribute);
  if not Assigned(attr) then
    attr := Attributes.Add(Attribute);

  attr.Add(value);
  result := attr.FindIndex(value);
end;

procedure TVisProperties.RemoveAttribute(Attribute: RawUtf8; value: RawUtf8);
var
  at: PtrInt;
  tmp, attr: TLdapAttribute;
  i: Integer;
begin
  at := Attributes.FindIndex(Attribute);
  if at = -1 then
    Exit;

  tmp := TLdapAttribute(Attributes.Items[at].Clone());
  try
    Attributes.Delete(Attribute);

    attr := Attributes.Add(Attribute);
    for i := 0 to tmp.Count - 1 do
      if tmp.List[i] = value then
        continue
      else
        attr.Add(tmp.List[i]);
  finally
    FreeAndNil(tmp);
  end;
end;

procedure TVisProperties.RemoveAttributeIndex(Attribute: RawUtf8; index: Integer);
var
  at: PtrInt;
  tmp, attr: TLdapAttribute;
  i: Integer;
begin
  at := Attributes.FindIndex(Attribute);
  if at = -1 then
    Exit;

  tmp := TLdapAttribute(Attributes.Items[at].Clone());
  try
    Attributes.Delete(Attribute);

    attr := Attributes.Add(Attribute);
    for i := 0 to tmp.Count - 1 do
      if i = index then
        continue
      else
        attr.Add(tmp.List[i]);
  finally
    FreeAndNil(tmp);
  end;
end;

procedure TVisProperties.DeleteAttribute(Attribute: RawUtf8);
begin
  Attributes.Delete(Attribute);
end;

function TVisProperties.GetAttribute(Attribute: RawUtf8): TRawUtf8DynArray;
var
  at: PtrInt;
begin
  at := Attributes.FindIndex(Attribute);
  if at = -1 then
    result := []
  else
    result := Attributes.Items[at].List;
end;

function TVisProperties.GetAttributeAsRawUtf8(Attribute: RawUtf8): RawUtf8;
var
  at: PtrInt;
begin
  result := '';

  at := Attributes.FindIndex(Attribute);
  if at = -1 then
    Exit;

  result := String.join('; ', Attributes.Items[at].List);
end;

function TVisProperties.GetAttributeIndex(Attribute: RawUtf8; index: Integer; default: RawUtf8 = ''): RawUtf8;
var
  at: PtrInt;
begin
  result := default;

  at := Attributes.FindIndex(Attribute);
  if at = -1 then
    Exit;

  if index < 0 then
    index := Attributes.Items[at].Count + index;
  if (index < 0) or (Index > Attributes.Items[at].Count - 1) then
    Exit;
  result := Attributes.Items[at].GetRaw(index);
end;

// Diff
procedure TVisProperties.Edit_Change(Sender: TObject);
var
  ModName, ModValue: String;
  data: TLdapAttribute;
begin
  ModName := (Sender as TComponent).Name;
  case ModName.Split('_')[0] of
    'Edit':
    begin
      ModName := Copy(ModName, 1 + Length('Edit_xxx_')); // usr, grp, cmp, obj, mem, ...
      ModValue := (Sender as TEdit).Text;
    end;
    'Memo':
    begin
      ModName := Copy(ModName, 1 + Length('Memo_xxx_')); // grp, sec, ...
      ModValue := (Sender as TMemo).Lines.Text;
    end;
  end;
  // Set Diff
  data := TLdapAttribute.Create(ModName, atUndefined);
  try
    if ModValue <> '' then
      data.Add(ModValue);
    LdapDiff.Commit(Attributes, ModName, data);
  finally
    FreeAndNil(data);
  end;
end;

// GroupGeneral
procedure TVisProperties.RadioBtn_Change(Sender: TObject);
var
  GTArray: TGroupTypes = [];
  data: TLdapAttribute;
begin
  if not (Sender as TRadioButton).Checked then
    Exit;

  // group left
  if RadioBtn_Local.Checked then
    GTArray := GTArray + [gtDomainLocal];
  if RadioBtn_Global.Checked then
    GTArray := GTArray + [gtGlobal];
  if RadioBtn_Universal.Checked then
    GTArray := GTArray + [gtUniversal];
  // group right
  if RadioBtn_Security.Checked then
    GTArray := GTArray + [gtSecurity];

  // Diff
  data := TLdapAttribute.Create('groupType', atGroupType);
  try
    data.Add(GroupTypesValue(GTArray).ToString());
    LdapDiff.Commit(Attributes, atGroupType, data);
  finally
    FreeAndNil(data);
  end;
end;

procedure TVisProperties.TisGrid_SecurityListUserKeyPress(Sender: TObject;
  var Key: char);
begin
  SearchInGrid(Timer_TisGridSearch, TisGrid_SecurityListUser, fSearchWord, Key);
end;

// VolumeGeneral
procedure TVisProperties.Edit_vlm_uNCNameChange(Sender: TObject);
begin
  if IsServerPath(Edit_vlm_uNCName.Text) then
  begin
    Edit_vlm_uNCName.Font.Color := clDefault;
    Edit_Change(Sender);
  end
  else
  begin
    Edit_vlm_uNCName.Font.Color := clRed;
    LdapDiff.Commit(Attributes, 'uNCName', nil);
  end;
end;

// Address
procedure TVisProperties.ComboBox_adr_CountryCodeDropDown(Sender: TObject);
Var
  s: String = '';
  doc: PDocVariantData;
  i: Integer;
begin
  if CountryCodes.Count = ComboBox_adr_CountryCode.Items.Count then
    Exit;
  s := ComboBox_adr_CountryCode.Items[0];
  ComboBox_adr_CountryCode.Clear();
  for doc in CountryCodes.Objects do
  begin
    i := ComboBox_adr_CountryCode.Items.Add(doc^.S['name']);
    if s = doc^.S['name'] then
      ComboBox_adr_CountryCode.ItemIndex := i;
  end;
end;

procedure TVisProperties.ComboBox_adr_CountryCodeChange(Sender: TObject);
var
  idx: Integer;
  ModName, ModValue: String;
  data: TLdapAttribute;
begin
  idx := (Sender as TComboBox).ItemIndex;
  // c
  ModName := 'c';
  ModValue := CountryCodes._[idx]^.S['alpha2'];
  data := TLdapAttribute.Create(ModName, atUndefined);
  try
    if ModValue <> '' then
      data.Add(ModValue);
    LdapDiff.Commit(Attributes, ModName, data);
  finally
    FreeAndNil(data);
  end;

  // countryCode
  ModName := 'countryCode';
  ModValue := CountryCodes._[idx]^.S['id'];
  data := TLdapAttribute.Create(ModName, atUndefined);
  try
    if ModValue <> '' then
      data.Add(ModValue);
    LdapDiff.Commit(Attributes, ModName, data);
  finally
    FreeAndNil(data);
  end;

  // co
  ModName := 'co';
  ModValue := CountryCodes._[idx]^.S['name'];
  data := TLdapAttribute.Create(ModName, atUndefined);
  try
    if ModValue <> '' then
      data.Add(ModValue);
    LdapDiff.Commit(Attributes, ModName, data);
  finally
    FreeAndNil(data);
  end;
end;

// Account
procedure TVisProperties.RadioButton_acc_NeverChange(Sender: TObject);
var
  data: TLdapAttribute;
begin
  if not RadioButton_acc_Never.Checked then
    Exit;
  DateTimePicker_acc_Expires.Enabled := False;

  data := TLdapAttribute.Create('accountExpires', atAccountExpires);
  try
    if (GetAttributeIndex('accountExpires', 0, '0') = '9223372036854775807') then // can be 0 or 9223372036854775807
      data.Add('9223372036854775807') // we prioritize canceling diff if possible
    else
      data.Add('0');
    LdapDiff.Commit(Attributes, atAccountExpires, data);
  finally
    FreeAndNil(data);
  end;
end;

procedure TVisProperties.RadioButton_acc_EndOfChange(Sender: TObject);
var
  data: TLdapAttribute;
begin
  if not RadioButton_acc_EndOf.Checked then
    Exit;
  DateTimePicker_acc_Expires.Enabled := True;

  data := TLdapAttribute.Create('accountExpires', atAccountExpires);
  try
    if MSTimeEqual(StrToInt64(GetAttributeIndex('accountExpires', 0, '0')),
      DateTimeToMSTime(DateTimePicker_acc_Expires.DateTime)) then
      data.Add(GetAttributeIndex('accountExpires', 0, '0'))
    else
      data.Add(DateTimeToMSTime(DateTimePicker_acc_Expires.DateTime).ToString());
    LdapDiff.Commit(Attributes, atAccountExpires, data);
  finally
    FreeAndNil(data);
  end;
end;

procedure TVisProperties.CheckBox_acc_opt_Change(Sender: TObject);
var
  UAC, msDS_SET: Integer;
  data: TLdapAttribute;
  SecDesc: TSecurityDescriptor;
  ModSecAceType: TSecAceType;
  AceSelf, AceWorld: PSecAce;
begin
  if IsLoading then
    Exit;

  // UAC
  data := LdapDiff.Get(Attributes, atUserAccountControl);
  if Assigned(data) then
    UAC := StrToInt(data.GetRaw(data.Count - 1))
  else
    UAC := 0;

  // msDS_SET
  data := LdapDiff.Get(Attributes, 'msDS-SupportedEncryptionTypes');
  if Assigned(data) then
    msDS_SET := StrToInt(data.GetRaw(data.Count - 1))
  else
    msDS_SET := 0;

  // switch
  case (Sender as TCheckBox).Name of
    'CheckBox_acc_opt_MustChange':
    begin
      if CheckBox_acc_opt_MustChange.Checked = {not xor}
        (GetAttributeIndex('pwdLastSet', 0, '0') = '0') then
      begin
        LdapDiff.Commit(Attributes, atPwdLastSet, nil);
        Exit;
      end;
      data := TLdapAttribute.Create('pwdLastSet', atPwdLastSet);
      try
        // https://ldapwiki.com/wiki/Wiki.jsp?page=Pwd-Last-Set%20attribute
        if CheckBox_acc_opt_MustChange.Checked then
          data.Add('0')
        else
          data.Add('-1'); // Current time
        LdapDiff.Commit(Attributes, atPwdLastSet, data);
        Exit;
      finally
        FreeAndNil(data);
      end;
    end;
    'CheckBox_acc_opt_CannotChange':
    begin
      // https://learn.microsoft.com/en-us/windows/win32/adsi/modifying-user-cannot-change-password-ldap-provider
      if not SecDesc.FromBinary(LdapDiff.Get(Attributes, atNTSecurityDescriptor).GetRaw()) then
      begin
        Dialogs.MessageDlg(rsTitleParsing, rsACEParsing, mtError, [mbOK], 0);
        Exit;
      end;

      // Get/Create current ACEs
      if not CheckBox_acc_opt_CannotChange.Checked then // not for current
        ModSecAceType := satObjectAccessDenied
      else
        ModSecAceType := satObjectAccessAllowed;
      AceSelf  := SecDescAddOrUpdateACE(@SecDesc, ATTR_UUID[kaUserChangePassword],
        KnownRawSid(wksSelf), ModSecAceType, [samControlAccess]);
      AceWorld := SecDescAddOrUpdateACE(@SecDesc, ATTR_UUID[kaUserChangePassword],
        KnownRawSid(wksWorld), ModSecAceType, [samControlAccess]);

      // Modify ACEs
      if CheckBox_acc_opt_CannotChange.Checked then
        ModSecAceType := satObjectAccessDenied
      else
        ModSecAceType := satObjectAccessAllowed;
      AceSelf^.AceType  := ModSecAceType;
      AceWorld^.AceType := ModSecAceType;

      // Order
      OrderAcl(Ldap, DistinguishedName, Ldap.DefaultDN(), @SecDesc.Dacl);

      // Ldiff
      data := TLdapAttribute.Create('nTSecurityDescriptor', atNTSecurityDescriptor);
      try
        data.Add(SecDesc.ToBinary());
        LdapDiff.Commit(Attributes, atNTSecurityDescriptor, data);
      finally
        FreeAndNil(data);
      end;
    end;
    'CheckBox_acc_opt_NeverExpires':
      UAC := UAC xor DONT_EXPIRE_PASSWORD;
    'CheckBox_acc_opt_ReversibleEncryption':
      UAC := UAC xor ENCRYPTED_TEXT_PWD_ALLOWED;
    'CheckBox_acc_opt_Disabled':
      UAC := UAC xor ACCOUNTDISABLE;
    'CheckBox_acc_opt_SmartCard':
      UAC := UAC xor SMARTCARD_REQUIRED;
    'CheckBox_acc_opt_Sensitive':
      UAC := UAC xor NOT_DELEGATED;
    'CheckBox_acc_opt_KerberosDESEncryption':
      UAC := UAC xor USE_DES_KEY_ONLY;
    'CheckBox_acc_opt_KerberosAES128Encryption':
      msDS_SET := msDS_SET xor AES128;
    'CheckBox_acc_opt_KerberosAES256Encryption':
      msDS_SET := msDS_SET xor AES256;
    'CheckBox_acc_opt_NoKerberosPreauth':
      UAC := UAC xor DONT_REQUIRE_PREAUTH;
  end;
  // UAC
  data := TLdapAttribute.Create('userAccountControl', atUserAccountControl);
  try
    data.Add(UAC.ToString());
    LdapDiff.Commit(Attributes, atUserAccountControl, data);
  finally
    FreeAndNil(data);
  end;

  // msDS_SET
  data := TLdapAttribute.Create('msDS-SupportedEncryptionTypes', atUndefined);
  try
    data.Add(msDS_SET.ToString());
    LdapDiff.Commit(Attributes, 'msDS-SupportedEncryptionTypes', data);
  finally
    FreeAndNil(data);
  end;
end;

procedure TVisProperties.DateTimePicker_acc_ExpiresChange(Sender: TObject);
var
  data: TLdapAttribute;
begin
  if not RadioButton_acc_EndOf.Checked then
    Exit;

  data := TLdapAttribute.Create('accountExpires', atAccountExpires);
  try
    if MSTimeEqual(StrToInt64(GetAttributeIndex('accountExpires', 0, '0')),
      DateTimeToMSTime(DateTimePicker_acc_Expires.DateTime)) then
      data.Add(GetAttributeIndex('accountExpires', 0, '0'))
    else
      data.Add(DateTimeToMSTime(DateTimePicker_acc_Expires.DateTime).ToString());
    LdapDiff.Commit(Attributes, atAccountExpires, data);
  finally
    FreeAndNil(data);
  end;
end;

procedure TVisProperties.acc_userPrincipalNameChange(Sender: TObject);
var
  data: TLdapAttribute;
begin
  data := TLdapAttribute.Create('userPrincipalName', atUserPrincipalName);
  try
    if Edit_acc_Name.Text <> '' then
      data.Add(Edit_acc_Name.Text + ComboBox_acc_Domain.Text);
    LdapDiff.Commit(Attributes, atUserPrincipalName, data);
  finally
    FreeAndNil(data);
  end;
end;

procedure TVisProperties.CheckBox_acc_UnlockChange(Sender: TObject);
var
  data: TLdapAttribute;
begin
  if Attributes.FindIndex('lockoutTime') <> -1 then
    UpdateAttribute('lockoutTime', ['1']); // I Cheat a bit here

  data := TLdapAttribute.Create('lockoutTime', atLockoutTime);
  try
    data.Add('0');
    if CheckBox_acc_Unlock.Checked then
      LdapDiff.Commit(Attributes, atLockoutTime, data)
    else
      LdapDiff.Commit(Attributes, atLockoutTime, nil);
  finally
    FreeAndNil(data);
  end;
end;

procedure TVisProperties.Btn_acc_LogonHoursClick(Sender: TObject);
var
  default: RawByteString = '';
  LogonHours: TVisLogonHours;
  Hours:   RawByteString = '';
  data: TLdapAttribute;
begin
  // Get Latest version
  SetLength(default, 21);  // 21bytes = (7days * 24hours) / 8bits
  FillByte(default[1], 21, $00);

  data := LdapDiff.Get(Attributes, 'LogonHours');
  if Assigned(data) then
    Hours := data.GetRaw()
  else
    Hours := default;

  // LogonHours Form
  LogonHours := TVisLogonHours.Create(self, @Hours);
  try
    if LogonHours.ShowModal() <> mrOK then
      Exit;
  finally
    FreeAndNil(LogonHours);
  end;

  // Diff
  data := TLdapAttribute.Create('logonHours', atUndefined);
  try
  if Hours <> default then
    data.Add(Hours);
  LdapDiff.Commit(Attributes, 'logonHours', data);
  finally
    FreeAndNil(data);
  end;
end;

procedure TVisProperties.Btn_acc_LogOnToClick(Sender: TObject);
var
  LogonWorkstation: TVisLogonWorkstation;
  Workstations: String = '';
  data: TLdapAttribute;
begin
  // Get Latest version
  data := LdapDiff.Get(Attributes, 'userWorkstations');
  if Assigned(data) then
    Workstations := data.GetRaw(0)
  else
    Workstations := '';

  // LogonWorkstation Form
  LogonWorkstation := TVisLogonWorkstation.Create(self, @Workstations);
  try
    if LogonWorkstation.ShowModal() <> mrOK then
      Exit;
  finally
    FreeAndNil(LogonWorkstation);
  end;

  // Diff
  data := TLdapAttribute.Create('userWorkstations', atUndefined);
  try
    if Workstations <> '' then
      data.Add(Workstations);
    LdapDiff.Commit(Attributes, 'userWorkstations', data);
  finally
    FreeAndNil(data);
  end;
end;

// Profile
procedure TVisProperties.Edit_pr0_homeDirectoryChange(Sender: TObject);
begin
  if IsServerPath(Edit_pr0_homeDirectory.Text) then
  begin
    Edit_pr0_homeDirectory.Font.Color := clDefault;
    Edit_Change(Sender);
  end
  else
  begin
    Edit_pr0_homeDirectory.Font.Color := clRed;
    LdapDiff.Commit(Attributes, 'homeDirectory', nil);
  end;
end;

procedure TVisProperties.Edit_pro_homeDirectoryChange(Sender: TObject);
begin
  if IsLocalPath(Edit_pro_homeDirectory.Text) then
  begin
    Edit_pro_homeDirectory.Font.Color := clDefault;
    Edit_Change(Sender);
  end
  else
  begin
    Edit_pro_homeDirectory.Font.Color := clRed;
    LdapDiff.Commit(Attributes, 'homeDirectory', nil);
  end;
end;

procedure TVisProperties.Edit_subnet_descriptionChange(Sender: TObject);
begin
  EditChange('description', Edit_subnet_description.Text);
end;

procedure TVisProperties.RadioButton_pro_Change(Sender: TObject);
var
  data: TLdapAttribute;
begin
  if IsLoading or not (Sender as TRadioButton).Checked then
    Exit;
  Edit_pro_homeDirectory.Enabled := RadioButton_pro_homeDirectory.Checked;
  ComboBox_pro_ConnectTo.Enabled := RadioButton_pro_ConnectTo.Checked;
  Label_pro_ConnectTo.Enabled    := RadioButton_pro_ConnectTo.Checked;
  Edit_pr0_homeDirectory.Enabled := RadioButton_pro_ConnectTo.Checked;

  // Clear data
  Edit_pro_homeDirectory.Text      := '';
  ComboBox_pro_ConnectTo.ItemIndex := -1;
  Edit_pr0_homeDirectory.Text      := '';

  // load prev data
  if RadioButton_pro_homeDirectory.Checked then
  begin
    if GetAttributeIndex('homeDrive', 0) = '' then
      Edit_pro_homeDirectory.Text := GetAttributeIndex('homeDirectory', 0);
  end
  else
  begin
    ComboBox_pro_ConnectTo.ItemIndex := ComboBox_pro_ConnectTo.Items.IndexOf(GetAttributeIndex('homeDrive', 0));
    if ComboBox_pro_ConnectTo.ItemIndex = -1 then
      ComboBox_pro_ConnectTo.ItemIndex := ComboBox_pro_ConnectTo.Items.Count -1;
    if GetAttributeIndex('homeDrive', 0) <> '' then
      Edit_pr0_homeDirectory.Text := GetAttributeIndex('homeDirectory', 0);
  end;

  // Ldiff
  data := TLdapAttribute.Create('homeDirectory', atUndefined);
  try
    if Edit_pro_homeDirectory.Text + Edit_pr0_homeDirectory.Text <> '' then
      data.Add(Edit_pro_homeDirectory.Text + Edit_pr0_homeDirectory.Text);
    LdapDiff.Commit(Attributes, 'homeDirectory', data);
  finally
    FreeAndNil(data);
  end;

  data := TLdapAttribute.Create('homeDrive', atUndefined);
  try
    if ComboBox_pro_ConnectTo.Text <> '' then
      data.Add(ComboBox_pro_ConnectTo.Text);
    LdapDiff.Commit(Attributes, 'homeDrive', data);
  finally
    FreeAndNil(data);
  end;
end;

procedure TVisProperties.ComboBox_pro_ConnectToChange(Sender: TObject);
var
  data: TLdapAttribute;
begin
  data := TLdapAttribute.Create('homeDrive', atUndefined);
  try
    if ComboBox_pro_ConnectTo.Text <> '' then
      data.Add(ComboBox_pro_ConnectTo.Text);
    LdapDiff.Commit(Attributes, 'homeDrive', data);
  finally
    FreeAndNil(data);
  end;
end;

procedure TVisProperties.ComboBox_subnet_siteChange(Sender: TObject);
var
  SiteName: String;
begin
  SiteName := ComboBox_subnet_site.Text;
  if (SiteName <> '') then
    EditChange('siteObject', fSubnetInfo.S[SiteName])
  else
    EditChange('siteObject', '');
end;

procedure TVisProperties.DateTimePicker1Change(Sender: TObject);
var
  attr, newAttr: TLdapAttribute;
  attrName: RawUtf8;
begin
  attrName := 'ms-Mcs-AdmPwdExpirationTime';
  attr := Attributes.Find(attrName);
  if not Assigned(attr) then
  begin
    attrName := 'msLAPS-PasswordExpirationTime';
    attr := Attributes.Find(attrName);
  end;
  if Assigned(attr) then
  begin
    newAttr := TLdapAttribute.Create(attrName, atUndefined);
    try
      newAttr.Add(DateTimeToStr(DateTimePicker1.DateTime));
      LdapDiff.Commit(Attributes, 'ms-Mcs-AdmPwdExpirationTime', newAttr);
    finally
      FreeAndNil(newAttr);
    end;
  end;
end;

// Telephone
procedure TVisProperties.BitBtn_other_Click(Sender: TObject);
var
  VisTelOther: TVisListOther;
  attrName: String;
  data: TLdapAttribute;
  tel: TDocVariantData;
  s: RawUtf8;
  MaxLength: Integer;
begin
  attrName := Copy((Sender as TBitBtn).Name, 1 + Length('BitBtn_xxx_'));
  case attrName of
    'url':
      MaxLength := 0;
    'keywords':
      MaxLength := 256;
    else
      MaxLength := 64;
  end;
  data := LdapDiff.Get(Attributes, attrName);
  if Assigned(data) then
    tel.InitArrayFrom(data.GetAllReadable(), [])
  else
    tel.InitArray([]);

  // VisTelOther
  VisTelOther := TVisListOther.Create(self, attrName, @tel, MaxLength);
  try
    if VisTelOther.ShowModal() <> mrOK then
      Exit;
  finally
    FreeAndNil(VisTelOther);
  end;

  // Diff
  data := TLdapAttribute.Create(attrName, atUndefined);
  try
    for s in tel.ToRawUtf8DynArray() do
      data.Add(s);
    LdapDiff.Commit(Attributes, attrName, data);
  finally
    FreeAndNil(data);
  end;
end;

// Organization
procedure TVisProperties.TisGrid_org_directReportsDblClick(Sender: TObject);
begin
  if not Assigned(TisGrid_org_directReports.FocusedRow) then
    Exit;
  fCore.OpenProperty(
    TisGrid_org_directReports.FocusedRow^.S['name'],
    TisGrid_org_directReports.FocusedRow^.S['distinguishedName']
  );
  //VisMain.VisPropertiesList.Open(
  //  TisGrid_org_directReports.FocusedRow^.S['name'],
  //  TisGrid_org_directReports.FocusedRow^.S['distinguishedName']);
end;

procedure TVisProperties.BitBtn_org_changeClick(Sender: TObject);
var
  Filter: RawUtf8;
  DNarr: TStringArray;
  Omniselect: TVisOmniselect;
  attr, data: TLdapAttribute;
begin
  // Set Filter
  Filter := FormatUtf8('(!(distinguishedName=%))', [LdapEscape(DistinguishedName)]); // Dont allow self

  // Omniselect
  DNarr := [''];
  Omniselect := TVisOmniselect.Create(self, Ldap, ['user'], Ldap.DefaultDN(), False, Filter);
  try
    Omniselect.Caption := rsTitleSelectNewManager;
    if Omniselect.ShowModal() <> mrOK then
      Exit;
    DNarr := Omniselect.SelectedObjects;
  finally
    FreeAndNil(Omniselect);
  end;

  // Set new data
  attr := Ldap.SearchObject(atName, DNarr[0], '');
  if not Assigned(attr) then
  begin
    ShowLdapSearchError(Ldap);
    Close();
    Exit;
  end;
  Edit_org_manager.Text := attr.GetRaw();

  // Diff
  data := TLdapAttribute.Create('manager', atUndefined);
  try
    data.Add(DNarr[0]);
    LdapDiff.Commit(Attributes, 'manager', data);
  finally
    FreeAndNil(data);
  end;
end;

procedure TVisProperties.Action_OrganizationPropertiesExecute(Sender: TObject);
var

  data: TLdapAttribute;
begin
  data := LdapDiff.Get(Attributes, 'manager');
  if Assigned(data) then
    fCore.OpenProperty(Edit_org_manager.Text, data.GetRaw(data.Count - 1));
    //VisMain.VisPropertiesList.Open(Edit_org_manager.Text, data.GetRaw(data.Count - 1));
end;

procedure TVisProperties.Action_OrganizationPropertiesUpdate(Sender: TObject);
begin
  Action_OrganizationProperties.Enabled := Edit_org_manager.Text <> '';
end;

procedure TVisProperties.Action_OrganizationClearExecute(Sender: TObject);
var
  data: TLdapAttribute;
begin
  Edit_org_manager.Text := '';

  data := TLdapAttribute.Create('manager', atUndefined);
  try
    LdapDiff.Commit(Attributes, 'manager', data);
  finally
    FreeAndNil(data);
  end;
end;

procedure TVisProperties.Action_OrganizationClearUpdate(Sender: TObject);
begin
  Action_OrganizationClear.Enabled := Edit_org_manager.Text <> '';
end;

// Published Certificates
function TVisProperties.CertToDoc(s: RawByteString): TDocVariantData;
var
  der: TCertDer;
  cert: TX509;
  purpose: array of String;
  ceku: TXExtendedKeyUsage;
begin
  // cert validation
  der := PemToDer(s);
  if not AsnDecChunk(der) then
    Exit;

  cert := TX509.Create();
  try
    // support PEM or DER input
    if not cert.LoadFromDer(der) or (cert.SignatureAlgorithm = xsaNone) then
      Exit;

    // Get purpose
    purpose := [];
    for ceku in cert.Signed.ExtendedKeyUsages do
    begin
      SetLength(purpose, Length(purpose) + 1);
      purpose[high(purpose)] := CEKU_FULLTEXT[ceku];
    end;

    // Set Data
    result.Init([dvoCheckForDuplicatedNames]);
    result.AddValue('certificate', s);
    result.AddValue('issuedTo', cert.Subject[xaCN]);
    result.AddValue('issuedBy', cert.Issuer[xacN]);
    result.AddValue('intendedPuposes', String.Join(', ',  purpose));
    result.AddValue('expirationDate', DateToStr(cert.NotAfter));
  finally
    FreeAndNil(cert);
  end;
end;

procedure TVisProperties.BitBtn_cer_addClick(Sender: TObject);
var
  FilePath: String;
  cert : RawByteString;
  tmp:TLdapAttribute;
  data: TLdapAttribute = nil;
begin
  if not OpenDialog.Execute() then
    Exit;

  // Get current
  tmp := LdapDiff.Get(Attributes, 'userCertificate');
  if Assigned(tmp) then
    data := TLdapAttribute(tmp.Clone())
  else
    data := TLdapAttribute.Create('userCertificate', atUndefined);
  try
    // Get data
    for FilePath in OpenDialog.Files do
      data.Add(StringFromFile(FilePath));

    // Fill cert grid
    TisGrid_cer_listX509.BeginUpdate();
    try
      TisGrid_cer_listX509.Data.Clear();
      for cert in data.GetAllReadable() do
        TisGrid_cer_listX509.Data.AddItem(CertToDoc(cert));
      TisGrid_cer_listX509.LoadData();
    finally
      TisGrid_cer_listX509.EndUpdate();
    end;

    // Diff
    LdapDiff.Commit(Attributes, 'userCertificate', data);
  finally
    FreeAndNil(data);
  end;
end;

procedure TVisProperties.Action_CertificateRemoveExecute(Sender: TObject);
var
  PDoc: PDocVariantData;
  data: TLdapAttribute;
begin
  TisGrid_cer_listX509.DeleteSelectedRows();

  data := TLdapAttribute.Create('userCertificate', atUndefined);
  try
    for PDoc in TisGrid_cer_listX509.Data.Objects do
      data.Add(PDoc^.U['certificate']);
    LdapDiff.Commit(Attributes, 'userCertificate', data);
  finally
    FreeAndNil(data);
  end;
end;

procedure TVisProperties.Action_CertificateRemoveUpdate(Sender: TObject);
begin
  Action_CertificateRemove.Enabled := Assigned(TisGrid_cer_listX509.FocusedRow);
end;

procedure TVisProperties.Action_CertificateCopyExecute(Sender: TObject);
var
  cert: RawByteString;
begin
  SaveDialog.FileName := TisGrid_cer_listX509.FocusedRow^.U['issuedTo'] + SaveDialog.DefaultExt;
  if not SaveDialog.Execute() then
    Exit;

  cert := TisGrid_cer_listX509.FocusedRow^.U['certificate'];
  FileFromString(cert, SaveDialog.FileName);
end;

procedure TVisProperties.Action_CertificateCopyUpdate(Sender: TObject);
begin
  Action_CertificateCopy.Enabled := Assigned(TisGrid_cer_listX509.FocusedRow);
end;

procedure TVisProperties.Action_CertificateViewExecute(Sender: TObject);
var
  TempFile: String;
  cert: RawUtf8;
begin
  TempFile := GetTempDir();
  if TempFile = '' then
    Exit;
  TempFile += TisGrid_cer_listX509.FocusedRow^.S['issuedTo'] + '.tmp.crt';

  cert := TisGrid_cer_listX509.FocusedRow^.U['certificate'];
  if not FileFromString(cert, TempFile) then
    Exit;
  OpenDocument(TempFile);
end;

procedure TVisProperties.Action_CertificateViewUpdate(Sender: TObject);
begin
  Action_CertificateView.Enabled := Assigned(TisGrid_cer_listX509.FocusedRow);
end;

procedure TVisProperties.Action_LAPSCopyPasswordExecute(Sender: TObject);
begin
  Edit3.CopyToClipboard;
end;

procedure TVisProperties.Action_LAPSExpireNowExecute(Sender: TObject);
begin
  DateTimePicker1.DateTime := Now;
  DateTimePicker1Change(Sender);
end;

procedure TVisProperties.Action_LAPSShowPasswordExecute(Sender: TObject);
begin
  if (Edit3.EchoMode = emPassword) then
  begin
    Edit3.EchoMode := emNormal;
    Action_LAPSShowPassword.Caption := 'Hide password';
  end
  else
  begin
    Edit3.EchoMode := emPassword;
    Action_LAPSShowPassword.Caption := 'Show password';
  end;
end;

// Member
procedure TVisProperties.TisGrid_memDblClick(Sender: TObject);
begin
  fCore.OpenProperty(TisGrid_mem.FocusedRow^.S['name'], TisGrid_mem.FocusedRow^.S['distinguishedName']);
  //VisMain.VisPropertiesList.Open(TisGrid_mem.FocusedRow^.S['name'], TisGrid_mem.FocusedRow^.S['distinguishedName']);
end;

procedure TVisProperties.TisGrid_memGetImageIndex(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
var
  grid: TTisGrid;
  ColumnName: String;
  //Data: PDocVariantData;
begin
  grid := Sender as TTisGrid;
  ColumnName := grid.FindColumnByIndex(Column).PropertyName;
  if ColumnName <> 'name' then
    Exit;

  //Data := grid.GetNodeAsPDocVariantData(Node, False);
  //if Data^.S['distinguishedName'] = PrimaryGroupDN then
  //  ImageIndex := Ord(Enum_Image.Im_Eye)
  //else
  //  ImageIndex := Ord(Enum_Image.Im_Group);
end;

procedure TVisProperties.Action_MembersAddExecute(Sender: TObject); // Member Add
  var
  Filter: RawUtf8;
  i: Integer;
  Omniselect: TVisOmniselect;
  DNarr: TStringArray;
  res: TLdapResult;
  Doc: TDocVariantData;
  item: PDocVariantData;
  data: TLdapAttribute;
begin
  // Set Filter
  Filter := '';
  if TisGrid_mem.Data.Count <> 0 then // Dont allow MemberOf's groups
  begin
    for i := 0 to TisGrid_mem.Data.Count - 1 do
      Filter += FormatUtf8('(distinguishedName=%)', [LdapEscape(TisGrid_mem.Data._[i]^.U['distinguishedName'])]);
    Filter := FormatUtf8('(!(|%))', [Filter]);
  end;
  // Omniselect
  DNarr := [];
  Omniselect := TVisOmniselect.Create(self, Ldap, ['group', 'user', 'computer', 'contact', 'Managed Service Accounts'], Ldap.DefaultDN(), True, Filter);
  try
    Omniselect.Caption := rsTitleSelectGroups;
    if Omniselect.ShowModal() <> mrOK then
      Exit;
    DNarr := Omniselect.SelectedObjects;
  finally
    FreeAndNil(Omniselect);
  end;

  // Get new members
  Filter := '';
  for i := 0 to Length(DNarr) - 1 do
    Filter += FormatUtf8('(distinguishedName=%)', [LdapEscape(DNarr[i])]);
  Filter := FormatUtf8('(|%)', [Filter]);
  Ldap.SearchBegin({VisMain.Storage.Options.SearchPageSize});
  try
    Ldap.SearchScope := lssWholeSubtree;
    repeat
      if not Ldap.Search([atName, atObjectClass, atDistinguishedName], Filter, Ldap.DefaultDN()) then // todo
      begin
        ShowLdapSearchError(Ldap);
        Exit;
      end;
      // Fill TisGrid_mem
      TisGrid_mem.BeginUpdate();
      try
        for res in Ldap.SearchResult.Items do
        begin
          //LdapResultToDocVariant(@res, @Doc, False, False);
          Doc.AddValue('name', res.Find('name').GetReadable());
          Doc.AddValue('distinguishedName', res.Find('distinguishedName').GetReadable());
          Doc.AddValue('objectClass', res.Find('objectClass').GetVariant());
          Doc.AddValue('ADSF', DNToCN(doc.S['distinguishedName']), false, 1);
          TisGrid_mem.Data.AddItem(Doc);
          Doc.Clear();
        end;
        TisGrid_mem.LoadData();
      finally
        TisGrid_mem.EndUpdate();
      end;
    until (Ldap.SearchCookie = '');
  finally
    Ldap.SearchEnd;
  end;

  // Diff
  data := TLdapAttribute.Create('member', atMember);
  try
    for item in TisGrid_mem.Data.Objects do
      data.Add(item^.S['distinguishedName']);
    LdapDiff.Commit(Attributes, atMember, data);
  finally
    FreeAndNil(data);
  end;
end;

procedure TVisProperties.Action_MembersAddUpdate(Sender: TObject);
begin
  { #todo -otprudhomme : Check permissions }
  Btn_mem_add.Enabled := True;
end;

procedure TVisProperties.Action_MembersDeleteExecute(Sender: TObject); // Member Delete
var
  item: PDocVariantData;
  data: TLdapAttribute;
begin
  TisGrid_mem.DeleteSelectedRows();

  // diff
  data := TLdapAttribute.Create('member', atMember);
  try
    for item in TisGrid_mem.Data.Objects do
      data.Add(item^.S['distinguishedName']);
    LdapDiff.Commit(Attributes, atMember, data);
  finally
    FreeAndNil(data);
  end;
end;

procedure TVisProperties.Action_MembersDeleteUpdate(Sender: TObject);
begin
  Btn_mem_delete.Enabled := (TisGrid_mem.Data.Count > 0);
end;

// MemberOf
procedure TVisProperties.List_mofGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  grid: TTisGrid;
  ColumnName: String;
  //Data: PDocVariantData;
begin
  grid := Sender as TTisGrid;
  ColumnName := grid.FindColumnByIndex(Column).PropertyName;
  if ColumnName <> 'name' then
    Exit;

  //Data := grid.GetNodeAsPDocVariantData(Node, False);
  //if Data^.S['distinguishedName'] = PrimaryGroupDN then
  //  ImageIndex := Ord(Enum_Image.Im_Eye)
  //else
  //  ImageIndex := Ord(Enum_Image.Im_Group);
end;

procedure TVisProperties.List_mofDblClick(Sender: TObject);
begin
  fCore.OpenProperty(List_mof.FocusedRow^.S['name'], List_mof.FocusedRow^.S['distinguishedName']);
  //VisMain.VisPropertiesList.Open(List_mof.FocusedRow^.S['name'], List_mof.FocusedRow^.S['distinguishedName']);
end;

procedure TVisProperties.Action_MemberOfAddExecute(Sender: TObject); // MemberOf Add
var
  Filter, ADistinguishedName: RawUtf8;
  i: Integer;
  Omniselect: TVisOmniselect;
  DNarr: TStringArray;
  Doc: TDocVariantData;
  tmp: TLdapAttribute;
  data: TLdapAttribute = nil;
  SearchResult: TLdapResult;
begin
  // Set filter to not allow MemberOf's groups
  Filter := '';
  if List_mof.Data.Count <> 0 then
  begin
    for i := 0 to List_mof.Data.Count - 1 do
      Filter += FormatUtf8('(distinguishedName=%)', [LdapEscape(List_mof.Data._[i]^.U['distinguishedName'])]);
    Filter := FormatUtf8('(!(|%))', [Filter]);
  end;

  // Select groups to add
  DNarr := [];
  Omniselect := TVisOmniselect.Create(self, Ldap, ['group'], Ldap.DefaultDN(), True, Filter);
  try
    Omniselect.Caption := rsTitleSelectGroups;
    if Omniselect.ShowModal() <> mrOK then
      Exit;
    DNarr := Omniselect.SelectedObjects;
    if (Length(DNarr) <= 0) then
      Exit;
  finally
    FreeAndNil(Omniselect);
  end;

  // Get new groups
  Filter := '';
  for i := 0 to Length(DNarr) - 1 do
    Filter += FormatUtf8('(distinguishedName=%)', [LdapEscape(DNarr[i])]);
  Filter := FormatUtf8('(|%)', [Filter]);

  // Get previous groups to Add
  tmp := LdapDiff.Get(nil, atMemberOf);
  if Assigned(tmp) then
    data := TLdapAttribute(tmp.Clone())
  else
    data := TLdapAttribute.Create('memberOf', atMemberOf);
  try
    Doc.init();
    // Fill List
    List_mof.BeginUpdate();
    Ldap.SearchBegin();
    try
      Ldap.SearchScope := lssWholeSubtree;

      repeat
        if not Ldap.Search(Ldap.DefaultDN(), False, Filter, ['name', 'objectClass', 'distinguishedName']) then
        begin
          ShowLdapSearchError(Ldap);
          Exit;
        end;

        for SearchResult in Ldap.SearchResult.Items do
        begin
          if not Assigned(SearchResult) then
            continue;
          ADistinguishedName := SearchResult.Find('distinguishedName').GetReadable();
          data.Add(ADistinguishedName);
          Doc.AddValue('ADSF', DNToCN(ADistinguishedName));
          Doc.AddValue('name', SearchResult.Find('name').GetReadable());
          Doc.AddValue('distinguishedName', ADistinguishedName);
          List_mof.Data.AddItem(Doc);
          Doc.Clear;
        end;
      until Ldap.SearchCookie = '';
    finally
      List_mof.EndUpdate();
      List_mof.LoadData();
      Ldap.SearchEnd;
    end;

    // diff
    LdapDiff.Commit(Attributes, atMemberOf, data);
  finally
    FreeAndNil(data);
  end;
end;

procedure TVisProperties.Action_MemberOfDeleteExecute(Sender: TObject); // MemberOf Delete
var
  DNToDelete: array of RawUtf8;
  DN, DnToFocus: RawUtf8;
  i: Integer;
  Row: PDocVariantData;
  tmp: TLdapAttribute;
  data: TLdapAttribute = nil;
  MSGstr: String;
  PNode: PVirtualNode;
begin
  MSGstr := FormatUtf8(rsDeleteMemberFromGroups, [GetAttributeIndex('cn', 0)]);
  if (List_mof.SelectedCount = 0)
  or (Dialogs.MessageDlg(rsTitleDeleteMember, MSGstr, mtWarning, mbYesNo, 0) = mrNo) then
    Exit;

  PNode := nil;

  // Get all DN to Delete
  DNToDelete := [];
  SetLength(DNToDelete, List_mof.SelectedCount);
  i := 0;
  for Row in List_mof.SelectedObjects do
  begin
    DN := Row^.U['distinguishedName'];
    if DN = PrimaryGroupDN then // Focus PrimaryGroup node
    begin
      Dialogs.MessageDlg(rsTitleDeleteMember, rsDeletePrimaryGroup, mtWarning, [mbOK], 0);
      Exit;
    end;
    DNToDelete[i] := DN;
    Inc(i);
  end;

  // Set focused List Group
  if PNode = nil then
  begin
    PNode := TTisGridGetNextUnselected(List_mof, List_mof.GetFirstSelected()); // Focus lower
    if PNode = nil then
      PNode := List_mof.GetPrevious(List_mof.GetFirstSelected()); // Focus upper
    if PNode = nil then
      PNode := List_mof.RootNode;
  end;
  if PNode <> List_mof.RootNode then
    DnToFocus := List_mof.GetNodeAsPDocVariantData(PNode)^.S['distinguishedName'];

  List_mof.BeginUpdate();
  try
    tmp := LdapDiff.Get(nil, atMemberOf);
    if Assigned(tmp) then
      data := TLdapAttribute(tmp.Clone())
    else
      data := TLdapAttribute.Create('memberOf', atMemberOf);
    try
      for DN in DNToDelete do
      begin
        data.Add(DN);
        // List
        List_mof.Data.Delete(List_mof.Data.SearchItemByProp('distinguishedName', DN, True));
      end;

      LdapDiff.Commit(Attributes, atMemberOf, data);
    finally
      FreeAndNil(data);
    end;

    // Update List
    List_mof.LoadData();

    // Focus new node
    TTisGridClearSelection(List_mof);
    if PNode <> List_mof.RootNode then
    begin
      List_mof.FocusedNode := List_mof.GetNodesBy('distinguishedName', DnToFocus)[0];
      List_mof.AddToSelection(List_mof.FocusedNode);
    end;
  finally

    List_mof.endUpdate();
  end;
end;

procedure TVisProperties.Action_MemberOfDeleteUpdate(Sender: TObject);
begin
  Action_MemberOfDelete.Enabled := List_mof.SelectedCount > 0;
end;

// Managed By
procedure TVisProperties.Action_ManagedByPropertiesExecute(Sender: TObject); // ManagedBy Properties
var
  data: TLdapAttribute;
begin
  data := LdapDiff.Get(Attributes, 'managedBy');
  if Assigned(data) then
    fCore.OpenProperty(ManagedByAttributes.Get(atName), data.GetRaw());
    //VisMain.VisPropertiesList.Open(ManagedByAttributes.Get(atName), data.GetRaw());
end;

procedure TVisProperties.Action_ManagedByPropertiesUpdate(Sender: TObject);
begin
  Action_ManagedByProperties.Enabled := Edit_man_managedBy.Text <> '';
end;

procedure TVisProperties.Action_ManagedByClearExecute(Sender: TObject); // ManagedBy Clear
var
  data: TLdapAttribute;
  Sid: RawSid;
  SecDesc: TSecurityDescriptor;
begin
  Sid := ManagedByAttributes.Find(atObjectSid).GetRaw();

  ManagedByAttributes.Clear();
  Edit_man_managedBy.Text                  := '';
  Edit_man_physicalDeliveryOfficeName.Text := '';
  Memo_man_streetAddress.Clear();
  Edit_man_l.Text                          := '';
  Edit_man_st.Text                         := '';
  Edit_man_co.Text                         := '';
  Edit_man_telephoneNumber.Text            := '';
  Edit_man_facsimileTelephoneNumber.Text   := '';

  data := TLdapAttribute.Create('managedBy', atUndefined);
  try
    LdapDiff.Commit(Attributes, 'managedBy', data);
  finally
    FreeAndNil(data);
  end;

  // Manager can update membership list
  if CheckBox_man.Checked then
  begin
    CheckBox_man.Checked := False;

    // Get SecDesc
    if not SecDesc.FromBinary(LdapDiff.Get(Attributes, atNTSecurityDescriptor).GetRaw()) then
    begin
      Dialogs.MessageDlg(rsTitleParsing, rsACEParsing, mtError, [mbOK], 0);
      Exit;
    end;

    // Delete ACE
    SecDescDeleteACE(@SecDesc, RawUtf8ToGuid(SELF_MEMBERSHIP_TEXT_GUID),
      Sid, satObjectAccessAllowed, [samWriteProp]);
    OrderAcl(Ldap, DistinguishedName, Ldap.DefaultDN(), @SecDesc.Dacl);

    // Diff
    data := TLdapAttribute.Create('nTSecurityDescriptor', atNTSecurityDescriptor);
    try
      data.Add(SecDesc.ToBinary());
      LdapDiff.Commit(Attributes, atNTSecurityDescriptor, data);
    finally
      FreeAndNil(data);
    end;
  end;
end;

procedure TVisProperties.Action_ManagedByClearUpdate(Sender: TObject);
begin
  Action_ManagedByClear.Enabled := Edit_man_managedBy.Text <> '';
  { #todo -otprudhomme : Check user permissions to clear the field managedBy }
end;

procedure TVisProperties.Action_ManagedByChangeExecute(Sender: TObject); // ManagedBy Change
var
  Filter: RawUtf8;
  Sid: RawSid;
  SecDesc: TSecurityDescriptor;
  DNarr: TStringArray;
  Omniselect: TVisOmniselect;
  data: TLdapAttribute;
  res: TLdapResult;
  guid: TGuid;

begin
  // Set Filter
  Filter := FormatUtf8('(!(distinguishedName=%))', [LdapEscape(DistinguishedName)]); // Dont allow self

  // Omniselect
  DNarr := [''];
  Omniselect := TVisOmniselect.Create(self, Ldap, ['user', 'group', 'contacts'], Ldap.DefaultDN(), False, Filter);
  try
    Omniselect.Caption := rsTitleSelectNewManager;
    if Omniselect.ShowModal() <> mrOK then
      Exit;
    DNarr := Omniselect.SelectedObjects;
  finally
    FreeAndNil(Omniselect);
  end;

  // Clear
  Action_ManagedByClear.Execute();

  // Diff
  data := TLdapAttribute.Create('managedBy', atUndefined);
  try
    data.Add(DNarr[0]);
    LdapDiff.Commit(Attributes, 'managedBy', data);
  finally
    FreeAndNil(data);
  end;

  // Manager can update membership list
  if GetAttributeIndex('managedBy', 0) = DNarr[0] then
  begin
    SecDesc.FromBinary(Attributes.Find(atNTSecurityDescriptor).GetRaw()); // Check original SecDesc

    guid := RawUtf8ToGuid(SELF_MEMBERSHIP_TEXT_GUID);
    Sid := ManagedByAttributes.Find(atObjectSid).GetRaw();
    CheckBox_man.Checked := SecDescFindACE(@SecDesc, satObjectAccessAllowed, Sid, [samWriteProp], @guid) <> -1;
    Action_ManagedByCheckBox.Execute(); // reset ACE
  end;

  // Set new data
  res := Ldap.SearchObject(DNarr[0], '', [
    'name',
    'distinguishedName',
    'physicalDeliveryOfficeName',
    'streetAddress',
    'l',
    'st',
    'co',
    'telephoneNumber',
    'facsimileTelephoneNumber',
    'objectClass',
    'objectSid'
  ]);
  if not Assigned(res) then
  begin
    ShowLdapSearchError(Ldap);
    Close();
    Exit;
  end;
  if Assigned(ManagedByAttributes) then
    FreeAndNil(ManagedByAttributes);
  ManagedByAttributes := TLdapAttributeList(res.Attributes.Clone());

  Edit_man_managedBy.Text                  := DNToCN(ManagedByAttributes.Find('distinguishedName').GetRaw());
  Edit_man_physicalDeliveryOfficeName.Text := ManagedByAttributes.GetByName('physicalDeliveryOfficeName');
  Memo_man_streetAddress.Append(              ManagedByAttributes.GetByName('streetAddress'));
  Edit_man_l.Text                          := ManagedByAttributes.GetByName('l');
  Edit_man_st.Text                         := ManagedByAttributes.GetByName('st');
  Edit_man_co.Text                         := ManagedByAttributes.GetByName('co');
  Edit_man_telephoneNumber.Text            := ManagedByAttributes.GetByName('telephoneNumber');
  Edit_man_facsimileTelephoneNumber.Text   := ManagedByAttributes.GetByName('facsimileTelephoneNumber');

  guid := RawUtf8ToGuid(SELF_MEMBERSHIP_TEXT_GUID);

  // Manager can update membership list
  // Get user's NTSecurityDescriptor
  SecDesc.FromBinary(Attributes.Find(atNTSecurityDescriptor).GetRaw());
  if GetAttributeIndex('managedBy', 0) = DNarr[0] then
  begin
    CheckBox_man.Checked := SecDescFindACE(@SecDesc,
      satObjectAccessAllowed, Sid,
      [samWriteProp], @guid) <> -1;
    Action_ManagedByCheckBox.Execute();
  end
  else
  begin
    CheckBox_man.Checked := False;

    // Delete ACE
    if not SecDescDeleteACE(@SecDesc, guid, Sid, satObjectAccessAllowed, [samWriteProp]) then
    begin
      Dialogs.MessageDlg(rsTitleNotFound, FormatUtf8(rsACENotFound, [RawSidToText(Sid)]), mtError, [mbOK], 0);
      Exit;
    end;
    OrderAcl(Ldap, DistinguishedName, Ldap.DefaultDN(), @SecDesc.Dacl);

    // Diff
    data := TLdapAttribute.Create('nTSecurityDescriptor', atNTSecurityDescriptor);
    try
      data.Add(SecDesc.ToBinary());
      LdapDiff.Commit(Attributes, atNTSecurityDescriptor, data);
    finally
      FreeAndNil(data);
    end;
  end;
end;

procedure TVisProperties.Action_ManagedByChangeUpdate(Sender: TObject);
begin
  Action_ManagedByChange.Enabled := True;
  { #todo -otprudhomme : Check user permissions to allow this Action_MembersAdd }
end;

procedure TVisProperties.Action_ManagedByCheckBoxExecute(Sender: TObject); // ManagedBy Check Box
var
  SecDesc: TSecurityDescriptor;
  data: TLdapAttribute;
  guid: TGuid;
  Sid: RawSid;
begin
  if IsLoading then
    Exit;

  // Get SecDesc
  if not SecDesc.FromBinary(LdapDiff.Get(Attributes, atNTSecurityDescriptor).GetRaw()) then
  begin
    Dialogs.MessageDlg(rsTitleParsing, rsACEParsing, mtError, [mbOK], 0);
    Exit;
  end;

  Sid  := ManagedByAttributes.Find(atObjectSid).GetRaw();
  guid := RawUtf8ToGuid(SELF_MEMBERSHIP_TEXT_GUID);

  if CheckBox_man.Checked then
  begin // Add ACE
    if not Assigned(SecDescAddACE(@SecDesc, guid, Sid, satObjectAccessAllowed, [samWriteProp])) then
    begin
      Dialogs.MessageDlg(rsTitleParsing, FormatUtf8(rsACECreateFailure, [DistinguishedName]), mtError, [mbOK], 0);
      Exit;
    end;
  end
  else
  begin // Delete ACE
    if not SecDescDeleteACE(@SecDesc, guid, Sid, satObjectAccessAllowed, [samWriteProp]) then
    begin
      Dialogs.MessageDlg(rsTitleNotFound, FormatUtf8(rsACENotFound, [RawSidToText(Sid)]), mtError, [mbOK], 0);
      Exit;
    end;
  end;

  OrderAcl(Ldap, DistinguishedName, Ldap.DefaultDN(), @SecDesc.Dacl);

  // LDiff
  data := TLdapAttribute.Create('nTSecurityDescriptor', atNTSecurityDescriptor);
  try
    data.Add(SecDesc.ToBinary());
    LdapDiff.Commit(Attributes, atNTSecurityDescriptor, data);
  finally
    FreeAndNil(data);
  end;
end;

procedure TVisProperties.Action_ManagedByCheckBoxUpdate(Sender: TObject);
begin
  Action_ManagedByCheckBox.Enabled := Edit_man_managedBy.Text <> '';
end;

// Object
procedure TVisProperties.CheckBox_obj_protectChange(Sender: TObject);
var
  SecDesc: TSecurityDescriptor;
  i: Integer;
  Sid: RawSid;
  data: TLdapAttribute;
begin
  if IsLoading then
    Exit;

  Sid := KnownRawSid(wksWorld);

  // Self
  if not SecDesc.FromBinary(LdapDiff.Get(Attributes, atNTSecurityDescriptor).GetRaw()) then
  begin
    Dialogs.MessageDlg(rsTitleParsing, rsACEParsing, mtError, [mbOK], 0);
    Exit;
  end;

  if CheckBox_obj_protect.Checked then
  begin // Add ACE
    if not Assigned(SecDescAddOrUpdateACE(@SecDesc, ATTR_UUID[kaNull], Sid, satAccessDenied, [samDelete, samDeleteTree])) then
    begin
      Dialogs.MessageDlg(rsTitleParsing, FormatUtf8(rsACECreateFailure, [DistinguishedName]), mtError, [mbOK], 0);
      Exit;
    end
  end
  else
  begin // Remove ACE
    i := SecDescFindACE(@SecDesc,
      satAccessDenied, Sid,
      [samDelete, samDeleteTree], @ATTR_UUID[kaNull]);
    if i = -1 then
    begin
      Dialogs.MessageDlg(rsTitleNotFound, FormatUtf8(rsACENotFound, [DistinguishedName]), mtError, [mbOK], 0);
      Exit;
    end;
    SecDesc.Dacl[i].Mask -= [samDelete, samDeleteTree];
  end;

  OrderAcl(Ldap, DistinguishedName, Ldap.DefaultDN(), @SecDesc.Dacl); // Order

  data := TLdapAttribute.Create('nTSecurityDescriptor', atNTSecurityDescriptor);
  try
    data.Add(SecDesc.ToBinary());
    LdapDiff.Commit(Attributes, atNTSecurityDescriptor, data);
  finally
    FreeAndNil(data);
  end;

  // Parent
  if DistinguishedName = Ldap.DefaultDN() then
    Exit;

  data := Ldap.SearchObject(atNTSecurityDescriptor, GetParentDN(DistinguishedName), '');
  if not Assigned(data) then
  begin
    ShowLdapSearchError(Ldap);
    CheckBox_obj_protect.Checked := not CheckBox_obj_protect.Checked;
    Exit;
  end;

  SecDesc.FromBinary(data.GetRaw());

  i := SecDescFindACE(@SecDesc,
    satAccessDenied, Sid,
    [samDeleteChild], @ATTR_UUID[kaNull]);
  if i <> -1 then
    Exit;
  if Dialogs.MessageDlg(
    rsConfirmation,
    rsACEpaadParent, mtConfirmation, mbYesNo, 0) <> mrYes then
    Exit;

  if not Assigned(SecDescAddOrUpdateACE(@SecDesc, ATTR_UUID[kaNull], Sid, satAccessDenied, [samDeleteChild])) then
  begin
    Dialogs.MessageDlg(rsTitleParsing, FormatUtf8(rsACECreateFailure, [DistinguishedName]), mtError, [mbOK], 0);
    Exit;
  end;

  data := TLdapAttribute.Create('nTSecurityDescriptor', atNTSecurityDescriptor);
  try
    data.Add(SecDesc.ToBinary());
    LdapDiff.Commit(Attributes, atNTSecurityDescriptor, data);
  finally
    FreeAndNil(data);
  end;
end;


// Security
procedure TVisProperties.Action_SecurityAdvancedExecute(Sender: TObject);
var
  vas: TVisAdvancedSecurity;
begin
  vas := TVisAdvancedSecurity.Create(self, Ldap, @SecurityDescriptor, DistinguishedName, Ldap.DefaultDN());
  try
    vas.Caption := FormatUtf8(rsTitleAdvancedSecurity, [GetAttributeIndex('name', 0)]);
    vas.ShowModal();
  finally
    FreeAndNil(vas);
  end;
  Attributes.Delete(atNTSecurityDescriptor);
  Attributes.Add(atNTSecurityDescriptor, SecurityDescriptor.ToBinary);
  SecurityFillListUser();
end;

procedure TVisProperties.TisGrid_SecurityListRightGetImageIndex(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
begin
  case TisGrid_SecurityListRight.FindColumnByIndex(Column).PropertyName of
    'allow':
    begin
      if (Sender as TTisGrid).GetNodeAsPDocVariantData(Node)^.B['allow'] then
        ImageIndex := Ord(ileValid)
      else
        ImageIndex := -1;
    end;
    'deny':
    begin
      if TisGrid_SecurityListRight.GetNodeAsPDocVariantData(Node)^.B['deny'] then
        ImageIndex := Ord(ileValid)
      else
        ImageIndex := -1;
    end;
  end;
end;

procedure TVisProperties.TisGrid_SecurityListRightGetText(
  aSender: TBaseVirtualTree; aNode: PVirtualNode; const aCell: TDocVariantData;
  aColumn: TColumnIndex; aTextType: TVSTTextType; var aText: string);
begin
  if (aSender as TTisGrid).FindColumnByIndex(aColumn).PropertyName <> 'name' then
    aText := '';
end;

procedure TVisProperties.TisGrid_SecurityListRightKeyPress(Sender: TObject;
  var Key: char);
begin
  SearchInGrid(Timer_TisGridSearch, TisGrid_SecurityListRight, fSearchWord, Key);
end;

procedure TVisProperties.TisGrid_SecurityListUserFocusChanged(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
var
  sid, n: RawUtf8;
  data: PDocVariantData;
begin
  data := TisGrid_SecurityListUser.GetNodeAsPDocVariantData(Node);
  if data <> nil then
  begin
    sid := data^.S['sid'];
    n := data^.S['name'];
    SecurityFillRightGrid(sid, n);
  end;
end;

procedure TVisProperties.SecurityFillListUser();
var
  namedSid: RawUtf8;
  newItem, sids: TDocVariantData;
  i, len: Integer;
  rawSidArr: array of RawSid;
  SearchResult: TLdapResult;
  acl: TSecAce;
  Filter: String;
begin
  newItem.Init([dvoCheckForDuplicatedNames]);

  TisGrid_SecurityListUser.BeginUpdate();
  try
    TisGrid_SecurityListUser.Clear();
    // Resolve Sids
    rawSidArr := [];
    len := Length(SecurityDescriptor.Dacl);
    for i := 0 to len - 1 do
      Insert(SecurityDescriptor.Dacl[i].Sid, rawSidArr, i);

    Filter := '(|';
    for acl in SecurityDescriptor.Dacl do
      Filter := FormatUtf8('%(objectSid=%)', [Filter, RawSidToText(acl.Sid)]);
    Filter := FormatUtf8('%)', [Filter]);
    sids.init();
    Ldap.SearchBegin();
    try
      Ldap.SearchScope := lssWholeSubtree;
      repeat
        if not Ldap.Search(Ldap.DefaultDN(), False, Filter, ['name', 'objectSid']) then
        begin
          ShowLdapSearchError(Ldap);
          Exit;
        end;

        for SearchResult in Ldap.SearchResult.Items do
        begin
          if not Assigned(SearchResult) then
            continue;
          sids.AddValue(SearchResult.Find('objectSid').GetReadable(), SearchResult.Find('name').GetReadable());
        end;
      until Ldap.SearchCookie = '';
    finally
      Ldap.SearchEnd;
    end;
    //sidArr := VisMain.Storage.SidCache.SidsNamesFromRawSids(rawSidArr);
    for i := 0 to len - 1 do
    begin
      namedSid := SecurityDescriptor.Dacl[i].SidText();
      if TisGrid_SecurityListUser.Data.SearchItemByProp('sid', namedSid, True) <> -1 then
        continue;
      newItem.AddValue('sid', namedSid);
      namedSid := RawSidToText(SecurityDescriptor.Dacl[i].Sid);
      if SidToKnown(namedSid) <> wksNull then
        newItem.AddValue('name', WELL_KNOWN_SID_NAMES[SidToKnown(namedSid)])
      else if sids.Exists(namedSid) then
        newItem.AddValue('name', sids.S[namedSid])
      else
        newItem.AddValue('name', namedSid);
      TisGrid_SecurityListUser.Data.AddItem(newItem);
      newItem.clear();
    end;
    TisGrid_SecurityListUser.LoadData();
  finally
    TisGrid_SecurityListUser.EndUpdate();
  end;
end;

procedure TVisProperties.SecurityFillRightGrid(sid, n: RawUtf8);
var
  secAccess: TSecAccess;
  AccessRow: TDocVariantData;
  ace: TSecAce;
  allow, deny: Boolean;
begin
  // header
  TisGrid_SecurityListRight.FindColumnByPropertyName('name').Text := FormatUtf8(rsTitlePermissionsFor, [n]);

  // data
  TisGrid_SecurityListRight.BeginUpdate();
  try
    TisGrid_SecurityListRight.Clear();
    // One Row for each SEC_ACCESS
    for secAccess in SEC_ACCESS do
    begin
      AccessRow.init([dvoCheckForDuplicatedNames]);
      AccessRow.AddValue('id', secAccess); // Is this Used ?
      AccessRow.AddValue('name', SEC_ACCESS_NAMES[secAccess]);
      allow := False;
      deny := False;
      for ace in SecurityDescriptor.Dacl do
      begin
        if (ace.SidText() <> sid) then        // look only for selected Ace
          continue;
        if not (secAccess in ace.mask) then // search the correct SEC_ACCESS for the row
          continue;
        case ace.AceType of
          satObjectAccessAllowed,
          satAccessAllowed:
            allow := True;
          satObjectAccessDenied,
          satAccessDenied:
            deny := True;
        end;
      end;
      AccessRow.AddValue('allow', allow);
      AccessRow.AddValue('deny', deny);
      TisGrid_SecurityListRight.Data.AddItem(AccessRow);
      AccessRow.Clear();
    end;
    TisGrid_SecurityListRight.LoadData();
  finally
    TisGrid_SecurityListRight.EndUpdate();
  end;
end;

// Attributes
procedure TVisProperties.Tab_AttributesEnter(Sender: TObject);
begin
  UpdatePanelAttributes();
end;

procedure TVisProperties.TisGrid_org_directReportsKeyPress(Sender: TObject;
  var Key: char);
begin
  SearchInGrid(Timer_TisGridSearch, TisGrid_org_directReports, fSearchWord, Key);
end;

procedure TVisProperties.List_AttributesDrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const CellText: String; const CellRect: TRect; var DefaultDraw: Boolean);
var
  NodeData: PDocVariantData;
  AttributeName: RawUtf8;
begin
  if Column <> 1 then
    Exit;

  NodeData := List_Attributes.GetNodeAsPDocVariantData(Node);
  if not Assigned(NodeData) then
    Exit;
  AttributeName := NodeData^.U['attribute'];

  if not Assigned(LdapDiff.Get(nil, AttributeName)) then // check if diff exists
    Exit;

  if CellText = GetAttributeAsRawUtf8(AttributeName) then
    TargetCanvas.Font.Style := [TFontStyle.fsItalic, TFontStyle.fsStrikeOut]
  else
    TargetCanvas.Font.Style := [TFontStyle.fsItalic, TFontStyle.fsBold];
end;

procedure TVisProperties.List_AttributesGetText(aSender: TBaseVirtualTree;
  aNode: PVirtualNode; const aCell: TDocVariantData; aColumn: TColumnIndex;
  aTextType: TVSTTextType; var aText: string);
var
  NodeData: PDocVariantData;
  data: TLdapAttribute;
  AttributeName: RawUtf8;
  arr: TRawUtf8DynArray;
  len: SizeInt;

  function ArrayJoin(sep: String; Values: TRawUtf8DynArray): String;
  var
    value: RawUtf8;
  begin
    result := '';
    for value in values do
      if result = '' then
        result := value
      else
        result := FormatUtf8('%%%', [result, sep, value]);
  end;

begin
  if aColumn <> 1 then
    Exit;

  NodeData := List_Attributes.GetNodeAsPDocVariantData(aNode);
  if not Assigned(NodeData) then
    Exit;
  AttributeName := NodeData^.U['attribute'];
  data := LdapDiff.Get(nil, AttributeName);

  if Assigned(data) then
    arr := data.GetAllReadable
  else
    arr := NodeData^.A['value']^.ToRawUtf8DynArray;

  len := Length(arr);
  if len > 10 then
    aText := arr[0] + '; range=0-' + IntToStr(len)
  else
    aText := ArrayJoin('; ', arr);
end;

procedure TVisProperties.List_AttributesKeyPress(Sender: TObject; var Key: char
  );
begin
  SearchInGrid(Timer_TisGridSearch, List_Attributes, fSearchWord, Key);
end;

procedure TVisProperties.List_mofKeyPress(Sender: TObject; var Key: char);
begin
  SearchInGrid(Timer_TisGridSearch, List_mof, fSearchWord, Key);
end;

procedure TVisProperties.PageControlChange(Sender: TObject);
begin
  TSynLog.Add.Log(sllDebug, FormatUtf8('TVisProperties: PageControlChange: %', [PageControl.ActivePage.Name]));
end;

procedure TVisProperties.Timer_TisGridSearchTimer(Sender: TObject);
begin
  Timer_TisGridSearch.Enabled := False;
end;

procedure TVisProperties.TisGrid1Change(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  data: PDocVariantData;
begin
  if not assigned(Node) then
    Exit;
  data := TisGrid1.GetNodeAsPDocVariantData(node);
  if not Assigned(data) then
    Exit;
  Memo1.Lines.Clear;
  Memo1.Lines.Add('Recovery password:');
  if data^.Exists('recoveryPassword') then
    Memo1.Lines.Add(data^.S['recoveryPassword']);
  if data^.Exists('computer') then
    Memo1.Lines.Add('Computer: ' + data^.S['computer']);
  if data^.Exists('date') then
    Memo1.Lines.Add('Date: ' + data^.S['date']);
  if data^.Exists('passwordID') then
    Memo1.Lines.Add('Password ID: ' + data^.S['passwordID']);
end;

procedure TVisProperties.TisGrid1KeyPress(Sender: TObject; var Key: char);
begin
  SearchInGrid(Timer_TisGridSearch, TisGrid1, fSearchWord, Key);
end;

procedure TVisProperties.TisGrid2KeyPress(Sender: TObject; var Key: char);
begin
  SearchInGrid(Timer_TisGridSearch, TisGrid2, fSearchWord, Key);
end;

procedure TVisProperties.TisGrid3KeyPress(Sender: TObject; var Key: char);
begin
  SearchInGrid(Timer_TisGridSearch, TisGrid3, fSearchWord, Key);
end;

procedure TVisProperties.TisGrid_cer_listX509KeyPress(Sender: TObject;
  var Key: char);
begin
  SearchInGrid(Timer_TisGridSearch, TisGrid_cer_listX509, fSearchWord, Key);
end;

procedure TVisProperties.TisGrid_memKeyPress(Sender: TObject; var Key: char);
begin
  SearchInGrid(Timer_TisGridSearch, TisGrid_mem, fSearchWord, Key);
end;

procedure TVisProperties.Action_AttributesBacklinksExecute(Sender: TObject);
begin
  Action_AttributesBacklinks.Checked := not Action_AttributesBacklinks.Checked;
  UpdatePanelAttributes();
end;

procedure TVisProperties.Action_AttributesConstructedExecute(Sender: TObject);
begin
  Action_AttributesConstructed.Checked := not Action_AttributesConstructed.Checked;
  UpdatePanelAttributes();
end;

procedure TVisProperties.Action_AttributesFilterExecute(Sender: TObject);
var
  p: TPoint;
begin
  p := Point(BitBtn_AttributesFilter.ClientRect.Left, BitBtn_AttributesFilter.ClientRect.Bottom);
  p := BitBtn_AttributesFilter.ControlToScreen(p);
  PopupMenu_AttributesFilter.PopUp(p.x, p.y);
end;

procedure TVisProperties.Action_AttributesMandatoryExecute(Sender: TObject);
begin
  Action_AttributesMandatory.Checked := not Action_AttributesMandatory.Checked;
  UpdatePanelAttributes();
end;

procedure TVisProperties.Action_AttributesModifyExecute(Sender: TObject);
var
  vis: TVisAttributeEditor;
  res: TLdapAttribute;
  value: RawUtf8;
  tempAttr: Boolean;
begin
  tempAttr := False;

  res := LdapDiff.Get(Attributes, List_Attributes.FocusedRow^.S['attribute']);
  if not Assigned(res) then
  begin
    tempAttr := True;
    res := TLdapAttribute.Create(List_Attributes.FocusedRow^.S['attribute'], atUndefined);
    for value in List_Attributes.FocusedRow^.A['value']^.ToRawUtf8DynArray do
      res.Add(value);
  end;
  vis := TVisAttributeEditor.Create(self, fCore, res, List_Attributes.FocusedRow^.S['attribute']);

  try
    if (vis.ShowModal = mrOK) then
      LdapDiff.Commit(Attributes, vis.Attr.AttributeName, vis.Attr);
  finally
    if (tempAttr) then
      FreeAndNil(res);
    FreeAndNil(vis);
  end;
end;

procedure TVisProperties.Action_AttributesModifyUpdate(Sender: TObject);
begin
  Action_AttributesModify.Enabled := Assigned(List_Attributes.FocusedRow);
end;

procedure TVisProperties.Action_AttributesOptionalExecute(Sender: TObject);
begin
  Action_AttributesOptional.Checked := not Action_AttributesOptional.Checked;
  UpdatePanelAttributes();
end;

procedure TVisProperties.Action_AttributesShowValuesExecute(Sender: TObject);
begin
  Action_AttributesShowValues.Checked := not Action_AttributesShowValues.Checked;
  UpdatePanelAttributes();
end;

procedure TVisProperties.Action_AttributesShowWritableExecute(Sender: TObject);
begin
  Action_AttributesShowWritable.Checked := not Action_AttributesShowWritable.Checked;
  UpdatePanelAttributes();
end;

procedure TVisProperties.Action_AttributesSystemOnlyExecute(Sender: TObject);
begin
  Action_AttributesSystemOnly.Checked := not Action_AttributesSystemOnly.Checked;
  UpdatePanelAttributes();
end;

// Action Buttons bottom
procedure TVisProperties.Action_CancelExecute(Sender: TObject); // Cancel
begin
  Close();
  Exit;
end;

procedure TVisProperties.Action_ApplyExecute(Sender: TObject); // Apply
  function ApplyMemberOf(): Boolean;
  var
    group: RawUtf8;
    res: TLdapResult;
    GroupAtt, AttDiff: TLdapAttribute;
    modif: TLdapModifyOp;

    function ArrayContains(const arr: array of string; const s: string): Boolean;
    var
      e: String;
    begin
      for e in arr do
      begin
        result := (e = s);
        if result then
          Exit;
      end;
    end;

  begin
    result := True;
    for group in LdapDiff.update.Find('memberOf').List do
    begin
      // Get group
      res := Ldap.SearchObject(group, '', ['member']);
      if not Assigned(res) then
      begin
        ShowLdapSearchError(Ldap);
        result := False;
        Exit;
      end;
      // Modify self to group
      GroupAtt := res.Attributes.Find(atMember);
      if Assigned(GroupAtt) and ArrayContains(GroupAtt.List, DistinguishedName) then
        modif := lmoDelete
      else
        modif := lmoAdd;
      AttDiff := TLdapAttribute.Create('member', atMember);
      try
        AttDiff.Add(DistinguishedName);
        if not Ldap.Modify(group, modif, AttDiff) then
        begin
          ShowLdapSearchError(Ldap);
          result := False;
          Exit;
        end;
      finally
        FreeAndNil(AttDiff);
      end;
    end;
  end;

  function ApplyDefault(item: TLdapAttribute; DiffType: TLdapModifyOp): Boolean;
  begin
    result := False;
    if not Ldap.Modify(DistinguishedName, DiffType, item) then
    begin
      ShowLdapModifyError(Ldap);
      Exit;
    end;
    result := True;
  end;

var
  res: TLdapResult;
  DiffType: TLdapModifyOp;
  diffList: TLdapAttributeList;
  i: Integer;
  SecDesc: TSecurityDescriptor;
begin
  // Modif Ldap
  Btn_BottomApply.Cursor := crHourGlass;
  try
    for DiffType in [lmoAdd, lmoReplace, lmoDelete] do
    begin
      case DiffType of
      lmoAdd:
        diffList := LdapDiff.add;
      lmoReplace:
        diffList := LdapDiff.update;
      lmoDelete:
        diffList := LdapDiff.delete;
      end;
      for i := 0 to diffList.Count - 1 do
        case diffList.Items[i].AttributeName of
        'memberOf':
          if not ApplyMemberOf() then
            Exit;
        'nTSecurityDescriptor':
        begin
          if not SecDesc.FromBinary(diffList.Find(atNTSecurityDescriptor).GetRaw()) then
          begin
            Dialogs.MessageDlg(rsTitleParsing, rsACEParsing, mtError, [mbOK], 0);
            Exit;
          end;
          OrderAcl(Ldap, DistinguishedName, Ldap.DefaultDN(), @SecDesc.Dacl);
          diffList.Add(atNTSecurityDescriptor, SecDesc.ToBinary(), aoReplaceValue);
          if not ApplyDefault(diffList.Items[i], DiffType) then
            Exit;
        end
        else
          if not ApplyDefault(diffList.Items[i], DiffType) then
            Exit;
        end;
    end;

    // Reset all
    LdapDiff.Clear();
    FreeAndNil(Attributes);
    res := Ldap.SearchObject(DistinguishedName, '', ['*']);
    if not Assigned(res) then
    begin
      ShowLdapSearchError(Ldap);
      Close();
      Exit;
    end;
    Attributes := TLdapAttributeList(res.Attributes.Clone());
  finally
    Btn_BottomApply.Cursor := crDefault;
  end;
end;

procedure TVisProperties.Action_ApplyUpdate(Sender: TObject);
begin
  Action_Apply.Enabled := not LdapDiff.IsEmpty();
end;

procedure TVisProperties.Action_OKExecute(Sender: TObject); // OK
begin
  Action_Apply.Execute();
  CheckBox_acc_Unlock.Checked := False;
  Close();
  Exit;
end;

procedure TVisProperties.Action_OKUpdate(Sender: TObject);
begin
  Action_OK.Enabled := True;
end;

end.
