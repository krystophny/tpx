unit PrintEpsOpt;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics,
    Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Printers, Spin, PrintersDlgs;

type
  TPrintEpsOptForm = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Button1: TButton;
    Label1: TLabel;
    PrinterEdit: TEdit;
    PrinterSetupDialog1: TPrinterSetupDialog;
    UseOffsetBox: TCheckBox;
    SpinEdit1: TSpinEdit;
    Label2: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  PrintEpsOptForm: TPrintEpsOptForm;

implementation

{$R *.lfm}

procedure TPrintEpsOptForm.Button1Click(Sender: TObject);
var
  Device,
    Driver,
    Port: PChar;
  deviceMode: THandle;
  I: Integer;
begin
  //PrinterSetupDialog1.
  I := Printer.Printers.IndexOf(Trim(PrinterEdit.Text));
  if I >= 0 then Printer.PrinterIndex := I;
  if PrinterSetupDialog1.Execute then
  begin
    // Get name of selected printer
    GetMem(Device, cchDeviceName);
    GetMem(Driver, MAX_PATH);
    GetMem(Port, MAX_PATH);
    // TODO: not working on non-Windows
    // Printer.GetPrinter(Device, Driver, Port, deviceMode);
    PrinterEdit.Text := string(Device);
  end;
end;

end.
