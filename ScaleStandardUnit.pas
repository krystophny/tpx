unit ScaleStandardUnit;

{$IFNDEF VER140}
{$MODE Delphi}
{$ENDIF}

interface

uses Messages, SysUtils, Classes, Graphics, Controls, Forms,
{$IFDEF VER140}
  Windows, Variants,
{$ELSE}
  LCLIntf, LResources, Buttons,
{$ENDIF}
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
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    Label3: TLabel;
    Edit3: TEdit;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure RadioButton1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    ScaleStandardMaxWidth, ScaleStandardMaxHeight, PicScale:
      Single;
  end;

var
  ScaleStandardForm: TScaleStandardForm;

implementation

uses Drawings;

{$IFDEF VER140}
{$R *.dfm}
{$ENDIF}

procedure TScaleStandardForm.FormShow(Sender: TObject);
begin
  Edit1.Enabled := True;
  Edit2.Enabled := True;
  Edit3.Enabled := True;
  Edit1.Text := Format('%.5g', [ScaleStandardMaxWidth]);
  Edit2.Text := Format('%.5g', [ScaleStandardMaxHeight]);
  Edit3.Text := Format('%.5g', [PicScale]);
  RadioButton1Click(Self);
end;

procedure TScaleStandardForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if ModalResult = mrOK then
  begin
    if RadioButton1.Checked then
    begin
      ScaleStandardMaxWidth := StrToFloat(Trim(Edit1.Text));
      ScaleStandardMaxHeight := StrToFloat(Trim(Edit2.Text));
      PicScale := 0;
    end
    else
    begin
      PicScale := StrToFloat(Trim(Edit3.Text));
    end;
  end;
end;

procedure TScaleStandardForm.FormCreate(Sender: TObject);
begin
  ScaleStandardMaxWidth := 90;
  ScaleStandardMaxHeight := 50;
end;

procedure TScaleStandardForm.RadioButton1Click(Sender: TObject);
begin
  Edit1.Enabled := RadioButton1.Checked;
  Edit2.Enabled := RadioButton1.Checked;
  Edit3.Enabled := not RadioButton1.Checked;
end;

initialization
{$IFDEF VER140}
{$ELSE}
{$I Propert.lrs}
{$ENDIF}
end.
