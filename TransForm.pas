unit TransForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
  CADSys4, CS4BaseTypes;


type
  TTransfForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    LabeledEdit1: TLabeledEdit;
    LabeledEdit2: TLabeledEdit;
    LabeledEdit3: TLabeledEdit;
    LabeledEdit4: TLabeledEdit;
    LabeledEdit5: TLabeledEdit;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    LabeledEdit6: TLabeledEdit;
    LabeledEdit7: TLabeledEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose:
      Boolean);
    procedure FormClose(Sender: TObject; var Action:
      TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
  private
    { Private declarations }
    Edits: array[1..7] of TLabeledEdit;
    function CheckEdits: Boolean;
  public
    { Public declarations }
    CP: TPoint2D;
    R: TRect2D;
    T: TTransf2D;
  end;

var
  TransfForm: TTransfForm;

implementation

uses Geometry;

{$R *.dfm}

procedure TTransfForm.FormCreate(Sender: TObject);
begin
  Edits[1] := LabeledEdit1;
  Edits[2] := LabeledEdit2;
  Edits[3] := LabeledEdit3;
  Edits[4] := LabeledEdit4;
  Edits[5] := LabeledEdit5;
  Edits[6] := LabeledEdit6;
  Edits[7] := LabeledEdit7;
end;

function TTransfForm.CheckEdits: Boolean;
var
  I, J: Integer;
  A: TRealType;
begin
  for I := 1 to 7 do if Edits[I].Enabled then
    begin
      Val(Edits[I].Text, A, J);
      if J > 0 then
      begin
        Edits[I].SetFocus;
        Edits[I].SelectAll;
        Result := False;
        Exit;
      end;
    end;
  Result := True;
end;

procedure TTransfForm.FormCloseQuery(Sender: TObject; var
  CanClose: Boolean);
begin
  CanClose := CheckEdits;
end;

procedure TTransfForm.FormShow(Sender: TObject);
begin
  LabeledEdit1.Text := '0';
  LabeledEdit2.Text := '0';
  LabeledEdit3.Text := '0';
  LabeledEdit4.Text := '1';
  LabeledEdit5.Text := '1';
  LabeledEdit6.Text := FloatToStr(CP.X);
  LabeledEdit6.Enabled := RadioGroup2.ItemIndex = 5;
  LabeledEdit7.Text := FloatToStr(CP.Y);
  LabeledEdit7.Enabled := RadioGroup2.ItemIndex = 5;
  RadioGroup1.ItemIndex := 0;
end;

procedure TTransfForm.FormClose(Sender: TObject; var Action:
  TCloseAction);
var
  A, ShX, ShY, ScX, ScY: TRealType;
  RefP: TPoint2D;
begin
  if ModalResult <> mrOK then Exit;
  A := StrToFloat(LabeledEdit1.Text) / 180 * Pi;
  ShX := StrToFloat(LabeledEdit2.Text);
  ShY := StrToFloat(LabeledEdit3.Text);
  ScX := StrToFloat(LabeledEdit4.Text);
  ScY := StrToFloat(LabeledEdit5.Text);
  if RadioGroup1.ItemIndex = 1 then
  begin
    ScX := 1 / ScX;
    ScY := 1 / ScY;
  end;
  case RadioGroup2.ItemIndex of
    0: RefP := CP;
    1: RefP := R.FirstEdge;
    2: RefP := Point2D(R.Left, R.Top);
    3: RefP := Point2D(R.Right, R.Bottom);
    4: RefP := R.SecondEdge;
  else
    RefP := Point2D(StrToFloat(LabeledEdit6.Text),
      StrToFloat(LabeledEdit7.Text));
  end;
  if A <> 0 then T := RotateCenter2D(A, RefP)
  else T := IdentityTransf2D;
  T := MultiplyTransform2D(T, ScaleCenter2D(ScX, ScY, RefP));
  T := MultiplyTransform2D(T, Translate2D(ShX, ShY));
end;

procedure TTransfForm.RadioGroup2Click(Sender: TObject);
begin
  LabeledEdit6.Enabled := RadioGroup2.ItemIndex = 5;
  LabeledEdit7.Enabled := RadioGroup2.ItemIndex = 5;
end;

end.
