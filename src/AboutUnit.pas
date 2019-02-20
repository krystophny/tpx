unit AboutUnit;

interface

uses Messages, SysUtils, Classes, Graphics, Controls, Forms,
{$IFNDEF FPC}
  Windows, Variants, ShellAPI,
{$ELSE}
  LCLIntf, Buttons, LResources,
{$ENDIF}
  Dialogs, ExtCtrls, StdCtrls, SysBasic, PreView;

{$IFDEF VER140}
{$ELSE}
const
  SW_SHOW = 5;
{$ENDIF}

type
  TAboutForm = class(TForm)
    Panel1: TPanel;
    Image1: TImage;
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    Button2: TButton;
    WebAddress: TStaticText;
    Label3: TLabel;
    procedure Button2Click(Sender: TObject);
    procedure WebAddressClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutForm: TAboutForm;

implementation

{$IFDEF VER140}
{$R *.dfm}
{$ENDIF}

{$I tpx.inc}

procedure TAboutForm.Button2Click(Sender: TObject);
begin
  Memo1.Visible := not Memo1.Visible;
end;

procedure TAboutForm.WebAddressClick(Sender: TObject);
begin
{$IFDEF VER140}
  ShellExecute(Application.MainForm.Handle, 'open',
    PChar(WebAddress.Caption), nil, nil, SW_SHOW);
{$ELSE}
  OpenOrExec(HtmlViewerPath, PChar(WebAddress.Caption));
{$ENDIF}
end;

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  Label1.Caption := About_Copyright;
  Label2.Caption := About_Version;
  Label3.Caption := 'TpX format v.' + IntToStr(TpX_Format_Version);
  WebAddress.Caption := About_URL;
end;

initialization
{$IFDEF FPC}
{$I Propert.lrs}
{$ELSE}
{$R *.dfm}
{$ENDIF}
end.

