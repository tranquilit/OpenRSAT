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
  ufrmmodule;

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

    procedure LoadOptionPages;
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
  ufrmoption,
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

  if not Assigned(TreeView1.Selected) or not Assigned(TreeView1.Selected.Data) then
  begin
    if Assigned(fLog) then
      fLog.Log(sllError, 'TreeView: No SelectedNode or no data assigned to SelectedNode', Self);
    Exit;
  end;
  TFrameOption(TreeView1.Selected.Data).Visible := False;

  if not Assigned(Node) or not Assigned(Node.Data) then
  begin
    if Assigned(fLog) then
      fLog.Log(sllError, 'TreeView: No Node or no data assigned to node', Self);
    Exit;
  end;
  TFrameOption(Node.Data).Visible := True;
end;

procedure TVisOptions.LoadOptionPages;
var
  Node: TTreeNode;
  Frame: TFrameModule;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'LoadOptionPages', Self);

  FrmRSAT.FrmRSATOption.Load;
  Node := TreeView1.Items.Add(nil, 'Global');
  Node.Data := FrmRSAT.FrmRSATOption;

  TFrameOption(Node.Data).Align := alClient;
  TFrameOption(Node.Data).Parent := PairSplitterSide2;
  TFrameOption(Node.Data).Visible := True;
  Node.Selected := True;

  for Frame in FrmRSAT.FrmModules.Items do
  begin
    if not Assigned(Frame) then
      continue;
    Node := TreeView1.Items.Add(nil, Frame.Module.GetOption.OptionName);
    Node.Data := Frame.FrmOption;
    TFrameOption(Node.Data).Align := alClient;
    TFrameOption(Node.Data).Parent := PairSplitterSide2;
    TFrameOption(Node.Data).Visible := False;
  end;
end;

constructor TVisOptions.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);

  LoadOptionPages;
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
  Frame: TFrameModule;
begin
  Change := FrmRSAT.FrmRSATOption.OptionChanged;

  if not Change then
  begin
    for Frame in FrmRSAT.FrmModules.Items do
    begin
      if not Assigned(Frame) then
        Continue;
      Change := Frame.FrmOption.OptionChanged;
      if Change then
        Break;
    end;
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
  Frame: TFrameModule;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Execute', [Action_Apply.Name], Self);

  MessageResult := MessageDlg('Confirmation', 'Do you want to apply changes ?', mtConfirmation, mbYesNoCancel, 0);

  case MessageResult of
    mrYes:
    begin
      if Assigned(fLog) then
        fLog.Log(sllInfo, 'User confirm to apply Options modifications.', Self);

      FrmRSAT.FrmRSATOption.Save;

      for Frame in FrmRSAT.FrmModules.Items do
      begin
        if not Assigned(Frame) then
          Frame.FrmOption.Save;
      end;
    end;
    else
      if Assigned(fLog) then
        fLog.Log(sllInfo, 'User cancel to apply Options modifications.', Self);
  end;
end;

end.

