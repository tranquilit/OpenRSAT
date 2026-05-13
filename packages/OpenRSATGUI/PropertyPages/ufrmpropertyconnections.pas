unit ufrmpropertyconnections;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  ExtCtrls,
  StdCtrls, tis.ui.grid.core,
  mormot.core.base,
  mormot.core.log,
  uhelpersui,
  uproperty,
  upropertyframe;  

type
  
  { TFrmPropertyConnections }

  TFrmPropertyConnections = class(TPropertyFrame)
    Label_To: TLabel;
    Label_From: TLabel;
    Panel1: TPanel;
    Grid_From: TTisGrid;
    Panel2: TPanel;
    Grid_To: TTisGrid;
  private
    fLog: TSynLog;
    fProperty: TProperty;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Update(Props: TProperty); override; 
  end;

implementation

{$R *.lfm}

constructor TFrmPropertyConnections.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fLog := TSynLog.Add;
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Create', Self);

  Caption := 'Connections';
end;

procedure TFrmPropertyConnections.Update(Props: TProperty);
begin
  if Assigned(fLog) then
    fLog.Log(sllTrace, 'Update', Self);

  fProperty := Props;
  
end; 

end.

