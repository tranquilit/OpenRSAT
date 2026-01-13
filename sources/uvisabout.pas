unit uvisabout;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ExtCtrls,
  StdCtrls,
  mormot.core.base;

type

  { TVisAbout }

  TVisAbout = class(TForm)
    Image_OpenRSATByTranquilIT: TImage;
    Label_Discord: TLabel;
    Label_GithubIssue: TLabel;
    Label_Documentation: TLabel;
    Label_WAPTPackage: TLabel;
    Label_TranquilIT: TLabel;
    Label_Github: TLabel;
    Label_Description: TLabel;
    Label_ApplicationInfo: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    procedure Label_DiscordClick(Sender: TObject);
    procedure Label_DiscordMouseEnter(Sender: TObject);
    procedure Label_DiscordMouseLeave(Sender: TObject);
    procedure Label_DocumentationMouseEnter(Sender: TObject);
    procedure Label_DocumentationMouseLeave(Sender: TObject);
    procedure Label_GithubIssueClick(Sender: TObject);
    procedure Label_GithubClick(Sender: TObject);
    procedure Label_GithubIssueMouseEnter(Sender: TObject);
    procedure Label_GithubIssueMouseLeave(Sender: TObject);
    procedure Label_GithubMouseEnter(Sender: TObject);
    procedure Label_GithubMouseLeave(Sender: TObject);
    procedure Label_TranquilITClick(Sender: TObject);
    procedure Label_TranquilITMouseEnter(Sender: TObject);
    procedure Label_TranquilITMouseLeave(Sender: TObject);
    procedure Label_WAPTPackageClick(Sender: TObject);
    procedure Label_WAPTPackageMouseEnter(Sender: TObject);
    procedure Label_WAPTPackageMouseLeave(Sender: TObject);
  private
    fVersion: RawUtf8;

    procedure SetVersion(AValue: RawUtf8);

    procedure UpdateApplicationInfo;

    procedure LinkHovered(Hovered: Boolean; Link: TLabel);
  public
    constructor Create(TheOwner: TComponent); override;

    property Version: RawUtf8 read fVersion write SetVersion;
  end;

implementation
uses
  lclintf,
  mormot.core.text;

{$R *.lfm}

{ TVisAbout }

procedure TVisAbout.Label_GithubClick(Sender: TObject);
begin
  OpenURL('https://github.com/tranquilit/OpenRSAT');
end;

procedure TVisAbout.Label_GithubIssueMouseEnter(Sender: TObject);
begin
  LinkHovered(True, Label_GithubIssue);
end;

procedure TVisAbout.Label_GithubIssueMouseLeave(Sender: TObject);
begin
  LinkHovered(False, Label_GithubIssue);
end;

procedure TVisAbout.Label_GithubMouseEnter(Sender: TObject);
begin
  LinkHovered(True, Label_Github);
end;

procedure TVisAbout.Label_GithubMouseLeave(Sender: TObject);
begin
  LinkHovered(False, Label_Github);
end;

procedure TVisAbout.Label_GithubIssueClick(Sender: TObject);
begin
  OpenURL('https://github.com/tranquilit/OpenRSAT/issues');
end;

procedure TVisAbout.Label_DiscordClick(Sender: TObject);
begin
  OpenURL('https://discord.com/invite/qj82uJTYnN');
end;

procedure TVisAbout.Label_DiscordMouseEnter(Sender: TObject);
begin
  LinkHovered(True, Label_Discord);
end;

procedure TVisAbout.Label_DiscordMouseLeave(Sender: TObject);
begin
  LinkHovered(False, Label_Discord);
end;

procedure TVisAbout.Label_DocumentationMouseEnter(Sender: TObject);
begin
  LinkHovered(True, Label_Documentation);
end;

procedure TVisAbout.Label_DocumentationMouseLeave(Sender: TObject);
begin
  LinkHovered(False, Label_Documentation);
end;

procedure TVisAbout.Label_TranquilITClick(Sender: TObject);
begin
  OpenURL('https://www.tranquil.it');
end;

procedure TVisAbout.Label_TranquilITMouseEnter(Sender: TObject);
begin
  LinkHovered(True, Label_TranquilIT);
end;

procedure TVisAbout.Label_TranquilITMouseLeave(Sender: TObject);
begin
  LinkHovered(False, Label_TranquilIT);
end;

procedure TVisAbout.Label_WAPTPackageClick(Sender: TObject);
begin
  OpenURL('https://wapt.tranquil.it/store/en/tis-openrsat');
end;

procedure TVisAbout.Label_WAPTPackageMouseEnter(Sender: TObject);
begin
  LinkHovered(True, Label_WAPTPackage);
end;

procedure TVisAbout.Label_WAPTPackageMouseLeave(Sender: TObject);
begin
  LinkHovered(False, Label_WAPTPackage);
end;

procedure TVisAbout.SetVersion(AValue: RawUtf8);
begin
  if fVersion = AValue then
    Exit;

  fVersion := AValue;
  UpdateApplicationInfo;
end;

procedure TVisAbout.UpdateApplicationInfo;
begin
  Label_ApplicationInfo.Caption := FormatUtf8('% - v%', [ApplicationName, Version]);
end;

procedure TVisAbout.LinkHovered(Hovered: Boolean; Link: TLabel);
begin
  Link.Font.Underline := Hovered;
  if Hovered then
  begin
    Link.Cursor := crHandPoint;
  end
  else
  begin
    Link.Cursor := crDefault;
  end;
end;

constructor TVisAbout.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  UpdateApplicationInfo;
end;

end.

