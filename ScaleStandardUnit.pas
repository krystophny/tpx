unit ScaleStandardUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TScaleStandardForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Button1: TButton;
    Button2: TButton;
    ScalePhysical: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    ScaleStandardMaxWidth, ScaleStandardMaxHeight: Single;
  end;

var
  ScaleStandardForm: TScaleStandardForm;

implementation

uses CADSys4;

{$R *.dfm}

procedure TScaleStandardForm.FormShow(Sender: TObject);
begin
  Edit1.Text := Format('%.5g', [ScaleStandardMaxWidth]);
  Edit2.Text := Format('%.5g', [ScaleStandardMaxHeight]);
end;

procedure TScaleStandardForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if ModalResult = mrOK then
  begin
    ScaleStandardMaxWidth := StrToFloat(Edit1.Text);
    ScaleStandardMaxHeight := StrToFloat(Edit2.Text);
  end;
end;

procedure TScaleStandardForm.FormCreate(Sender: TObject);
begin
  ScaleStandardMaxWidth := 90;
  ScaleStandardMaxHeight := 50;
end;

end.

