unit AboutUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ShellAPI;

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

{$R *.dfm}

procedure TAboutForm.Button2Click(Sender: TObject);
begin
  Memo1.Visible := not Memo1.Visible;
end;

procedure TAboutForm.WebAddressClick(Sender: TObject);
begin
  ShellExecute(Application.MainForm.Handle, 'open',
    PChar(WebAddress.Caption), nil, nil, SW_SHOW);
end;

end.
