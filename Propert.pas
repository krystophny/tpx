unit Propert;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, StdCtrls,
  CS4Shapes, Spin, Buttons, Stars, Arrows, ImgList;

type
  TPropertiesForm = class(TForm)
    PropPages: TPageControl;
    LineSheet: TTabSheet;
    TextSheet: TTabSheet;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
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
    LabeledEdit3: TLabeledEdit;
    ComboBox3: TComboBox;
    ComboBox4: TComboBox;
    ComboBox5: TComboBox;
    Label3: TLabel;
    StarsSheet: TTabSheet;
    Button3: TButton;
    LabeledEdit4: TLabeledEdit;
    ComboBox6: TComboBox;
    ArrowsFrame1: TArrowsFrame;
    SymbolsSheet: TTabSheet;
    ComboBox7: TComboBox;
    ImageList1: TImageList;
    LabeledEdit5: TLabeledEdit;
    LabeledEdit6: TLabeledEdit;
    StarsFrame1: TStarsFrame;
    Button4: TButton;
    FontDialog1: TFontDialog;
    FontCheckBox: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ColorBox_DrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure ColorBox_Select(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ComboBox7DrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Button4Click(Sender: TObject);
    procedure FontCheckBoxClick(Sender: TObject);
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
var I: Integer;
begin
  MakeColorBox(ComboBox3);
  MakeColorBox(ComboBox4);
  MakeColorBox(ComboBox5);
  for I := 0 to High(SymbolsIDs) do
    ComboBox7.Items.Add(SymbolsIDs[I]);
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
      FontCheckBox.Checked := Font.Name <> ' ';
      Button4.Enabled := FontCheckBox.Checked;
    end
  else if (PPrimitive is TLine2D) or (PPrimitive is TArc2D)
    or (PPrimitive is TPolyline2D) or (PPrimitive is TSmoothPath2D)
    or (PPrimitive is TBezierPath2D) then
    with PPrimitive as TPrimitive2D do
    begin
      ArrowsFrame1.ComboBox1.ItemIndex := Ord(BeginArrowKind);
      ArrowsFrame1.ComboBox2.ItemIndex := Ord(EndArrowKind);
      ArrowsFrame1.Edit1.Text := FloatToStr(ArrowSizeFactor);
      PropPages.ActivePage := LineSheet;
    end
  else if PPrimitive is TStar2D then
    with PPrimitive as TStar2D do
    begin
      StarsFrame1.ToolBar1.Buttons[Ord(StarKind)].Down := True;
      StarsFrame1.Edit1.Text := FloatToStr(StarSizeFactor);
      PropPages.ActivePage := StarsSheet;
    end
  else if PPrimitive is TSymbol2D then
    with PPrimitive as TSymbol2D do
    begin
      ComboBox7.ItemIndex := Ord(SymbolKind);
      LabeledEdit5.Text := Format('%.5g', [Diameter]);
      LabeledEdit6.Text := Format('%.6g', [RadToDeg(Rot)]);
      PropPages.ActivePage := SymbolsSheet;
    end
  else PropPages.ActivePage := VoidSheet;
  {for I := 0 to PropPages.PageCount - 1 do
    PropPages.Pages[I].TabVisible := False;}
      //I = PropPages.ActivePageIndex;
end;

procedure TPropertiesForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
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
      Rot := DegToRad(StrToRealType(LabeledEdit4.Text));
      HJustification := THJustification(RadioGroup1.ItemIndex);
      VJustification := TVJustification(RadioGroup2.ItemIndex);
      if not FontCheckBox.Checked then Font.Name := ' ';
    end
  else if (PPrimitive is TLine2D) or (PPrimitive is TArc2D)
    or (PPrimitive is TPolyline2D) or (PPrimitive is TSmoothPath2D)
    or (PPrimitive is TBezierPath2D) then
    with PPrimitive as TPrimitive2D do
    begin
      BeginArrowKind := TArrowKind(ArrowsFrame1.ComboBox1.ItemIndex);
      EndArrowKind := TArrowKind(ArrowsFrame1.ComboBox2.ItemIndex);
      ArrowSizeFactor := StrToRealType(ArrowsFrame1.Edit1.Text);
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
      StarSizeFactor := StrToRealType(StarsFrame1.Edit1.Text);
      PPrimitive.UpdateExtension(nil);
    end
  else if PPrimitive is TSymbol2D then
    with PPrimitive as TSymbol2D do
    begin
      SymbolKind := TSymbolKind(ComboBox7.ItemIndex);
      Diameter := StrToRealType(LabeledEdit5.Text);
      Rot := DegToRad(StrToRealType(LabeledEdit6.Text));
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

procedure TPropertiesForm.ComboBox7DrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  with (Control as TComboBox).Canvas do
  begin
    FillRect(Rect);
    TextOut(Rect.Left + 30, Rect.Top + 3, ComboBox7.Items[Index]);
    Rect.Right := Rect.Left + 26;
    Brush.Color := clWhite;
    FillRect(Rect);
    ImageList1.Draw((Control as TComboBox).Canvas,
      Rect.Left, Rect.Top, Index);
  end;
end;

procedure TPropertiesForm.Button4Click(Sender: TObject);
begin
  FontDialog1.Font.Assign((PPrimitive as TText2D).Font);
  if FontDialog1.Execute then
    (PPrimitive as TText2D).Font.Assign(FontDialog1.Font);
end;

procedure TPropertiesForm.FontCheckBoxClick(Sender: TObject);
begin
  Button4.Enabled := FontCheckBox.Checked;
end;

end.

