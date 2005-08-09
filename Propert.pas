unit Propert;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, StdCtrls,
  CS4Shapes, Spin, Buttons, Stars;

type
  TPropertiesForm = class(TForm)
    PropPages: TPageControl;
    LineSheet: TTabSheet;
    TextSheet: TTabSheet;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    LabeledEdit1: TLabeledEdit;
    LabeledEdit2: TLabeledEdit;
    Panel2: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    VoidSheet: TTabSheet;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    LabeledEdit3: TLabeledEdit;
    ComboBox3: TComboBox;
    ComboBox4: TComboBox;
    ComboBox5: TComboBox;
    Label3: TLabel;
    StarsSheet: TTabSheet;
    StarsFrame1: TStarsFrame;
    Button3: TButton;
    LabeledEdit4: TLabeledEdit;
    ComboBox6: TComboBox;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action:
      TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure ColorBox_DrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure ColorBox_Select(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    PPrimitive: TPrimitive2D;
  end;

var
  PropertiesForm: TPropertiesForm;

implementation

uses ColorEtc, Table, Geometry;

{$R *.dfm}

procedure TPropertiesForm.FormCreate(Sender: TObject);
begin
  MakeColorBox(ComboBox3);
  MakeColorBox(ComboBox4);
  MakeColorBox(ComboBox5);
end;

procedure TPropertiesForm.FormShow(Sender: TObject);
var
  I: Integer;
begin
  Caption := PPrimitive.Name;
  ComboBox1.ItemIndex := Ord(PPrimitive.LineStyle);
  ComboBox2.ItemIndex := Ord(PPrimitive.Hatching);
  ColorBoxSet(ComboBox3, PPrimitive.LineColor);
  ColorBoxSet(ComboBox4, PPrimitive.HatchColor);
  ColorBoxSet(ComboBox5, PPrimitive.FillColor);
  ComboBox6.Text := Format('%.5g', [PPrimitive.LineWidth]);
  //PropPages.TabHeight := 1;
  for I := 0 to PropPages.PageCount - 1 do
    PropPages.Pages[I].TabVisible := False;
  if PPrimitive is TText2D then
    with PPrimitive as TText2D do
    begin
      LabeledEdit1.Text := Text;
      LabeledEdit3.Text := TeXText;
      LabeledEdit2.Text := Format('%.5g', [Height]);
      LabeledEdit4.Text := Format('%.6g', [RadToDeg(Rot)]);
      RadioGroup1.ItemIndex := Ord(HJustification);
      RadioGroup2.ItemIndex := Ord(VJustification);
      PropPages.ActivePage := TextSheet;
    end
  else if PPrimitive is TLine2D then
    with PPrimitive as TLine2D do
    begin
      CheckBox1.Checked :=
        (PPrimitive as TLine2D).BeginArrowKind <> arrNone;
      if CheckBox1.Checked then
        SpinEdit1.Value := Ord(BeginArrowKind) - 1;
      CheckBox2.Checked :=
        (PPrimitive as TLine2D).EndArrowKind <> arrNone;
      if CheckBox2.Checked then
        SpinEdit2.Value := Ord(EndArrowKind) - 1;
      PropPages.ActivePage := LineSheet;
    end
  else if PPrimitive is TStar2D then
    with PPrimitive as TStar2D do
    begin
      StarsFrame1.ToolBar1.Buttons[Ord(StarKind)].Down := True;
      PropPages.ActivePage := StarsSheet;
    end
  else PropPages.ActivePage := VoidSheet;
  {for I := 0 to PropPages.PageCount - 1 do
    PropPages.Pages[I].TabVisible := False;}
      //I = PropPages.ActivePageIndex;
end;

procedure TPropertiesForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
  I: Integer;
begin
  if ModalResult <> mrOK then Exit;
  PPrimitive.LineStyle :=
    TLineStyle(ComboBox1.ItemIndex);
  PPrimitive.Hatching := THatching(ComboBox2.ItemIndex);
  PPrimitive.LineColor := ColorBoxGet(ComboBox3);
  PPrimitive.HatchColor := ColorBoxGet(ComboBox4);
  PPrimitive.FillColor := ColorBoxGet(ComboBox5);
  PPrimitive.LineWidth := StrToFloat(ComboBox6.Text);
  if PPrimitive is TText2D then
    with PPrimitive as TText2D do
    begin
      Text := LabeledEdit1.Text;
      TeXText := LabeledEdit3.Text;
      Height := StrToFloat(LabeledEdit2.Text);
      Rot := DegToRad(StrToFloat(LabeledEdit4.Text));
      HJustification := THJustification(RadioGroup1.ItemIndex);
      VJustification := TVJustification(RadioGroup2.ItemIndex);
    end
  else if PPrimitive is TLine2D then
    with PPrimitive as TLine2D do
    begin
      if CheckBox1.Checked then
        BeginArrowKind := TArrowKind(SpinEdit1.Value + 1)
      else BeginArrowKind := arrNone;
      if CheckBox2.Checked then
        EndArrowKind := TArrowKind(SpinEdit2.Value + 1)
      else EndArrowKind := arrNone;
    end
  else if PPrimitive is TStar2D then
    with PPrimitive as TStar2D do
    begin
      for I := 0 to StarsFrame1.ToolBar1.ButtonCount - 1 do
        if StarsFrame1.ToolBar1.Buttons[I].Down then
        begin
          StarKind := TStarKind(I);
          Break;
        end;
      PPrimitive.UpdateExtension(nil);
    end
      ;
end;

procedure TPropertiesForm.ColorBox_DrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  ColorBoxDrawItem(Control as TComboBox, Index, Rect, State);
end;

procedure TPropertiesForm.ColorBox_Select(Sender: TObject);
begin
  ColorBoxSelect(Sender as TComboBox);
end;

procedure TPropertiesForm.Button3Click(Sender: TObject);
begin
  TableForm.PPrimitive := PPrimitive;
  TableForm.ShowModal;
end;

end.
