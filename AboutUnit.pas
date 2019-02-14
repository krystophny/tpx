unit AboutUnit;

{$IFNDEF VER140}
{$MODE Delphi}
{$ENDIF}

interface

uses Messages, SysUtils, Classes, Graphics, Controls, Forms,
{$IFDEF VER140}
  Windows, Variants, ShellAPI,
{$ELSE}
  LCLIntf, Buttons, LResources,
{$ENDIF}
  Dialogs, ExtCtrls, StdCtrls;

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
    procedure Button2Click(Sender: TObject);
    procedure WebAddressClick(Sender: TObject);
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
{$ENDIF}
end;

initialization
{$IFDEF VER140}
{$ELSE}
{$I Propert.lrs}
{$ENDIF}
end.
