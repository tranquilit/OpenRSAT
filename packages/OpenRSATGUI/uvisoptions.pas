unit uVisOptions;

{$mode ObjFPC}{$H+}

interface

uses
  Forms,
  ComCtrls,
  Buttons,
  ExtCtrls,
  StdCtrls,
  ActnList,
  PairSplitter,
  SysUtils,
  tis.ui.searchedit,
  Classes,
  mormot.core.log,
  mormot.net.ldap,
  Controls,
  ufrmrsatoptions,
  ufrmmodule,
  ufrmoption,
  uoption;

type

  { TVisOptions }

  TVisOptions = class(TForm)
    Action_Apply: TAction;
    Action_Cancel: TAction;
    Action_OK: TAction;
    ActionList1: TActionList;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    Panel_Bottom: TPanel;
    Btn_OK: TBitBtn;
    Btn_Cancel: TBitBtn;
    Btn_Apply: TButton;
    TisSearchEdit1: TTisSearchEdit;
    TreeView1: TTreeView;
    {$push}{$warn 5024 off}
    procedure Action_ApplyExecute(Sender: TObject);
    procedure Action_ApplyUpdate(Sender: TObject);
    procedure Action_OKExecute(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure TreeView1Changing(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
    {$pop}
  private
    fLog: TSynLog;

    fOptionFrames: Array of TFrameOption;

    procedure CreateOptionFrames;
  public
    constructor Create(TheOwner: TComponent); reintroduce;
    destructor Destroy; override;
  end;

implementation
uses
  Dialogs,
  mormot.core.text,
  mormot.core.base,
  ucoredatamodule,
  IniFiles,
  ufrmrsat;

{$R *.lfm}

{ TVisOptions }

procedure TVisOptions.FormShow(Sender: TObject);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'FormShow', Self);
end;

procedure TVisOptions.TreeView1Changing(Sender: TObject; Node: TTreeNode;
  var AllowChange: Boolean);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Change TreeView', Self);

  if Assigned(TreeView1.Selected) and Assigned(TreeView1.Selected.Data) then
    TFrameOption(TreeView1.Selected.Data).Visible := False
  else
    if Assigned(fLog) then
      fLog.Log(sllError, 'TreeView: No SelectedNode or no data assigned to SelectedNode', Self);

  if Assigned(Node) and Assigned(Node.Data) then
  begin
    TFrameOption(Node.Data).Visible := True;
    TFrameOption(Node.Data).Load;
  end
  else
    if Assigned(fLog) then
      fLog.Log(sllError, 'TreeView: No Node or no data assigned to node', Self);
end;

procedure TVisOptions.CreateOptionFrames;
var
  Frame: TFrameModule;

  function AddFrame(FrameOption: TFrameOption; Name: RawUtf8): TTreeNode;
  begin
    result := nil;
    if not Assigned(FrameOption) then
      Exit;
    result := TreeView1.Items.Add(nil, Name);
    result.Data := FrameOption;
    FrameOption.Align := alClient;
    FrameOption.Parent := PairSplitterSide2;
    FrameOption.Visible := False;
    Insert(FrameOption, fOptionFrames, 0);
  end;

begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create Option Frames');

  AddFrame(FrmRSAT.FrmRSATOptionClass.Create(Self, FrmRSAT.RsatOption), 'Global').Selected := True;

  for Frame in FrmRSAT.FrmModules.Items do
    if Assigned(Frame) and Assigned(Frame.FrmOptionClass) then
      AddFrame(Frame.FrmOptionClass.Create(Self, Frame.GetOption), Frame.GetModuleDisplayName);
end;

constructor TVisOptions.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);

  fOptionFrames := [];

  CreateOptionFrames;
end;

destructor TVisOptions.Destroy;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Destroy', Self);

  inherited Destroy;
end;

procedure TVisOptions.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    27:
    begin
      if Assigned(fLog) then
        fLog.Log(sllInfo, 'Escape pressed. Close page.', Self);
      Close;
    end;
  end;
end;

procedure TVisOptions.Action_ApplyUpdate(Sender: TObject);
var
  Change: Boolean;
  Frame: TFrameOption;
begin
  Change := False;
  for Frame in fOptionFrames do
  begin
    Change := Assigned(Frame) and Frame.OptionChanged;
    if Change then
      Break;
  end;

  Action_Apply.Enabled := Change;
end;

procedure TVisOptions.Action_OKExecute(Sender: TObject);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Execute', [Action_OK.Name], Self);

  Action_Apply.Execute;
  Close;
end;

procedure TVisOptions.Action_ApplyExecute(Sender: TObject);
var
  MessageResult: TModalResult;
  Frame: TFrameOption;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Execute', [Action_Apply.Name], Self);

  MessageResult := MessageDlg('Confirmation', 'Do you want to apply changes ?', mtConfirmation, mbYesNoCancel, 0);

  case MessageResult of
    mrYes:
    begin
      if Assigned(fLog) then
        fLog.Log(sllInfo, 'User confirm to apply Options modifications.', Self);

      for Frame in fOptionFrames do
      begin
        if Assigned(Frame) then
          Frame.Save;
      end;
    end;
    else
      if Assigned(fLog) then
        fLog.Log(sllInfo, 'User cancel to apply Options modifications.', Self);
  end;
end;

end.

