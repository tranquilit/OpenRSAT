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
  uinterfacemodule;

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

    fRsatOptions: TRsatOptions;

    procedure LoadOptionPages;
    procedure UnloadOptionPages;
  public
    constructor Create(TheOwner: TComponent; ARsatOptions: TRsatOptions); reintroduce;
    destructor Destroy; override;
  end;

implementation
uses
  Dialogs,
  mormot.core.text,
  mormot.core.base,
  ucoredatamodule,
  IniFiles;
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
  TOptions(TreeView1.Selected.Data).GetFrame.Visible := False;

  if not Assigned(Node) or not Assigned(Node.Data) then
  begin
    if Assigned(fLog) then
      fLog.Log(sllError, 'TreeView: No Node or no data assigned to node', Self);
    Exit;
  end;
  TOptions(Node.Data).GetFrame.Visible := True;
end;

procedure TVisOptions.LoadOptionPages;
var
  Node: TTreeNode;
  Option: TOptions;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'LoadOptionPages', Self);

  fRsatOptions.Load;
  Node := TreeView1.Items.Add(nil, 'Global');
  Node.Data := fRsatOptions;
  fRsatOptions.CreateFrame(Self);

  fRsatOptions.GetFrame.Align := alClient;
  fRsatOptions.GetFrame.Parent := PairSplitterSide2;
  fRsatOptions.GetFrame.Visible := True;
  Node.Selected := True;

  for Option in fRsatOptions.ModulesOptions do
  begin
    if not Assigned(Option) then
      continue;
    Node := TreeView1.Items.Add(nil, Option.OptionName);
    Node.Data := Option;
    Option.CreateFrame(Self);
    Option.GetFrame.Align := alClient;
    Option.GetFrame.Parent := PairSplitterSide2;
    Option.GetFrame.Visible := False;
  end;
end;

procedure TVisOptions.UnloadOptionPages;
var
  Option: TOptions;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'UnloadOptionPages', Self);

  fRsatOptions.DeleteFrame;
  for Option in fRsatOptions.ModulesOptions do
  begin
    if not Assigned(Option) then
      continue;
    Option.DeleteFrame;
  end;
end;

constructor TVisOptions.Create(TheOwner: TComponent; ARsatOptions: TRsatOptions
  );
begin
  inherited Create(TheOwner);

  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);

  fRsatOptions := ARsatOptions;

  LoadOptionPages;
end;

destructor TVisOptions.Destroy;
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Destoy', Self);

  UnloadOptionPages;

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
  Module: TOptions;
begin
  Change := fRsatOptions.Changed;

  if not Change then
  begin
    for Module in fRsatOptions.ModulesOptions do
    begin
      if not Assigned(Module) then
        Continue;
      Change := Module.Changed;
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
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, '% - Execute', [Action_Apply.Name], Self);

  MessageResult := MessageDlg('Confirmation', 'Do you want to apply changes ?', mtConfirmation, mbYesNoCancel, 0);

  case MessageResult of
    mrYes:
    begin
      if Assigned(fLog) then
        fLog.Log(sllInfo, 'User confirm to apply Options modifications.', Self);

      fRsatOptions.Save();
    end;
    else
      if Assigned(fLog) then
        fLog.Log(sllInfo, 'User cancel to apply Options modifications.', Self);
  end;
end;

end.

