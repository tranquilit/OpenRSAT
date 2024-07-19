unit utreeselectionhistory;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  ComCtrls;

type
  { TTreeSelectionHistory }

  TTreeSelectionHistory = class
  private
    fItems: Array of TTreeNode;
    fIndex: Integer;
    fCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Push(node: TTreeNode);
    procedure Clear;
    function Previous: TTreeNode;
    function Next: TTreeNode;
    function Current: TTreeNode;
    procedure Remove(idx: Integer; count: Integer = 1);
    procedure Remove(node: TTreeNode);
  end;

implementation

{ TTreeSelectionHistory }

constructor TTreeSelectionHistory.Create;
begin
  fItems := [];
  fIndex := 0;
  fCount := 0;
end;

destructor TTreeSelectionHistory.Destroy;
begin
  inherited Destroy;
end;

procedure TTreeSelectionHistory.Push(node: TTreeNode);
begin
  if (node = Current) then
    Exit;
  Remove(fIndex + 1, fCount - (fIndex + 1));
  fCount := Length(fItems);
  Insert(node, fItems, fCount);
  Inc(fCount);
  fIndex := fCount - 1;
end;

procedure TTreeSelectionHistory.Clear;
begin
  fItems := nil;
  fIndex := 0;
  fCount := 0;
end;

function TTreeSelectionHistory.Previous: TTreeNode;
begin
  result := nil;

  if not Assigned(fItems) or (fIndex <= 0) then
  begin
    fIndex := 0;
    Exit;
  end;
  Dec(fIndex);
  result := Current;
end;

function TTreeSelectionHistory.Next: TTreeNode;
begin
  result := nil;

  if not Assigned(fItems) or (fIndex >= fCount - 1) then
  begin
    fIndex := fCount - 1;
    Exit;
  end;
  Inc(fIndex);
  result := Current;
end;

function TTreeSelectionHistory.Current: TTreeNode;
begin
  result := nil;

  if not Assigned(fItems) then
    Exit;

  result := fItems[fIndex];
end;

procedure TTreeSelectionHistory.Remove(idx: Integer; count: Integer);
begin
  if (idx < 0) or (idx >= fCount) then
    Exit;

  Delete(fItems, idx, count);
  fCount := fCount - count;
  if fIndex >= fCount then
    fIndex := fCount - 1;
end;

procedure TTreeSelectionHistory.Remove(node: TTreeNode);
var
  i: Integer;
begin
  for i := 0 to fCount - 1 do
  begin
    if node = fItems[i] then
      Remove(i);
  end;
end;

end.

