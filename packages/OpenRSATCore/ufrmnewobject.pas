unit ufrmnewobject;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  StdCtrls,
  Buttons,
  ActnList,
  tis.ui.grid.core,
  mormot.net.ldap, mormot.core.variants, mormot.core.base;

type

  { TFrmNewObject }

  TFrmNewObject = class(TFrame)
    Action1: TAction;
    ActionList1: TActionList;
    BitBtn1: TBitBtn;
    ComboBox1: TComboBox;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    TisGrid1: TTisGrid;
    procedure ComboBox1Change(Sender: TObject);
  private
    fLdap: TLdapClient;
    LdapSchema: TDocVariantData;

    procedure Load;
    procedure ReloadGrid;
  public
    constructor Create(TheOwner: TComponent; ALdap: TLdapClient); reintroduce;
  end;

implementation

{$R *.lfm}

{ TFrmNewObject }

procedure TFrmNewObject.ComboBox1Change(Sender: TObject);
begin
  ReloadGrid;
end;

procedure TFrmNewObject.Load;
begin
  ComboBox1.SetFocus;
end;

procedure TFrmNewObject.ReloadGrid;
  procedure AddObjectAttribute(ObjectClass: RawUtf8);
  var
    SubClassOf, Aux: RawUtf8;
    NewItem: TDocVariantData;

    procedure InnerAddObjectAttribute(AttributeName: RawUtf8);
    var
      Attribute: RawUtf8;
    begin
      for Attribute in LdapSchema.O[ObjectClass]^.A[AttributeName]^.ToRawUtf8DynArray do
      begin
        NewItem.S['name'] := Attribute;
        TisGrid1.Data.AddItem(NewItem);
        NewItem.Clear;
      end;
    end;

  begin
    NewItem.Init();
    if not LdapSchema.Exists(ObjectClass) then
      Exit;
    //SubClassOf := LdapSchema.O[ObjectClass]^.A['subClassOf']^.ToRawUtf8DynArray[0];
    //if not (SubClassOf = ObjectClass) then
    //  AddObjectAttribute(SubClassOf);

    //for Aux in LdapSchema.O[ObjectClass]^.A['auxiliaryClass']^.ToRawUtf8DynArray do
    //  AddObjectAttribute(Aux);
    //
    //for Aux in LdapSchema.O[ObjectClass]^.A['systemAuxiliaryClass']^.ToRawUtf8DynArray do
    //  AddObjectAttribute(Aux);

    InnerAddObjectAttribute('mayContain');
    InnerAddObjectAttribute('systemMayContain');
    InnerAddObjectAttribute('mustContain');
    InnerAddObjectAttribute('systemMustContain');
  end;

begin
  TisGrid1.Clear;
  TisGrid1.BeginUpdate;
  try
    AddObjectAttribute(ComboBox1.Text);
  finally
    TisGrid1.EndUpdate;
    TisGrid1.LoadData;
  end;
end;

constructor TFrmNewObject.Create(TheOwner: TComponent; ALdap: TLdapClient);
var
  ObjectClassAttribute: TDocVariantData;
  Item: TLdapResult;
  Attribute: TLdapAttribute;
  i: Integer;
  ObjectClasses: TRawUtf8DynArray;
  DisplayName, LdapName: RawUtf8;

  procedure AddAttributes(AttributeName: RawUtf8);
  var
    value: RawUtf8;
  begin
    for value in LdapSchema.O[LdapName]^.A['mayContain']^.ToRawUtf8DynArray do
    begin
      ObjectClassAttribute.O_[LdapName]^.A_['mayContain']^.AddItem(value);
    end;
  end;

begin
  inherited Create(TheOwner);

  fLdap := ALdap;

  LdapSchema.Init();
  fLdap.SearchBegin();
  try
    fLdap.SearchScope := lssWholeSubtree;

    repeat
      if not fLdap.Search(Format('CN=Schema,%s', [fLdap.ConfigDN]), False, '', ['*']) then
        Exit;

      for Item in fLdap.SearchResult.Items do
      begin
        if not Assigned(Item) then
          continue;
        DisplayName := Item.Find('lDAPDisplayName').GetReadable();
        if DisplayName = '' then
          continue;
        for Attribute in Item.Attributes.Items do
        begin
          if not Assigned(Attribute) then
            continue;
          for i := 0 to Attribute.Count - 1 do
            LdapSchema.O_[DisplayName]^.A_[Attribute.AttributeName]^.AddItem(Attribute.GetReadable(i));
        end;
      end;
    until fLdap.SearchCookie = '';
  finally
    fLdap.SearchEnd;
  end;

  ObjectClassAttribute.Init();
  // todo: Create a docvariant that store available attribute for each of classSchema, so it only need to load this docvariant for the docvariantgrid
  ComboBox1.Clear;
  ComboBox1.Items.BeginUpdate;
  try
    for LdapName in LdapSchema.Names do
    begin
      if not LdapSchema.Exists(LdapName) then
        continue;

      //AddAttributes('mayContain');

      // mustContain, systemMayContain, systemMustContain
      // systemAuxiliaryClass, auxiliaryClass

      ObjectClasses := LdapSchema.O[LdapName]^.A['objectClass']^.ToRawUtf8DynArray;
      if ObjectClasses[High(ObjectClasses)] = 'classSchema' then
        ComboBox1.Items.Add(LdapSchema.O[LdapName]^.A['lDAPDisplayName']^.ToRawUtf8DynArray[0]);

    end;
  finally
    ComboBox1.Items.EndUpdate;
  end;
end;

end.

