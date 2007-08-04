unit Propert;

{$IFNDEF VER140}
{$MODE Delphi}
{$ENDIF}

interface

uses Messages, SysUtils, Classes, Graphics,
  Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, StdCtrls,
{$IFDEF VER140}
  Windows, Variants,
{$ELSE}
  LResources, //LCLIntf,
{$ENDIF}
  Drawings, GObjBase, GObjects, Spin, Buttons, ImgList, ToolWin;

type
  TPropertiesForm = class(TForm)
    PropPages: TPageControl;
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
    SymbolsSheet: TTabSheet;
    ComboBox7: TComboBox;
    ImageList1: TImageList;
    LabeledEdit5: TLabeledEdit;
    LabeledEdit6: TLabeledEdit;
    Button4: TButton;
    FontDialog1: TFontDialog;
    FontCheckBox: TCheckBox;
    ArrowsPanel: TPanel;
    ArrImageList: TImageList;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    Label6: TLabel;
    Edit2: TEdit;
    StarsImageList: TImageList;
    RectSheet: TTabSheet;
    LabeledEdit7: TLabeledEdit;
    LabeledEdit8: TLabeledEdit;
    Panel3: TPanel;
    Label4: TLabel;
    ComboBox8: TComboBox;
    ComboBox9: TComboBox;
    Label5: TLabel;
    Edit1: TEdit;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ColorBox_DrawItem(Control: TWinControl; Index:
      Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure ColorBox_Select(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ComboBox7DrawItem(Control: TWinControl; Index:
      Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure FormCloseQuery(Sender: TObject; var CanClose:
      Boolean);
    procedure Button4Click(Sender: TObject);
    procedure FontCheckBoxClick(Sender: TObject);
    procedure ArrComboBoxDrawItem(Control: TWinControl; Index:
      Integer;
      Rect: TRect; State: TOwnerDrawState);
  private
    { Private declarations }
  public
    { Public declarations }
    PObject: TObject2D;
  end;

var
  PropertiesForm: TPropertiesForm;

implementation

uses ColorEtc, Table, Geometry, Devices;

{$IFDEF VER140}
{$R *.dfm}
{$ENDIF}

procedure TPropertiesForm.FormCreate(Sender: TObject);
var
  I: Integer;
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
  if not (PObject is TPrimitive2D) then Exit;
  Caption := (PObject as TPrimitive2D).Name;
  ComboBox1.ItemIndex := Ord((PObject as TPrimitive2D).LineStyle);
  ComboBox2.ItemIndex := Ord((PObject as TPrimitive2D).Hatching);
  ColorBoxSet(ComboBox3, (PObject as TPrimitive2D).LineColor);
  ColorBoxSet(ComboBox4, (PObject as TPrimitive2D).HatchColor);
  ColorBoxSet(ComboBox5, (PObject as TPrimitive2D).FillColor);
  ComboBox6.Text := Format('%.5g',
    [(PObject as TPrimitive2D).LineWidth]);
  //PropPages.TabHeight := 1;
{$IFDEF VER140}
  for I := 0 to PropPages.PageCount - 1 do
    PropPages.Pages[I].TabVisible := False;
{$ENDIF}
  Panel3.Visible := False;
  if PObject is TText2D then
    with PObject as TText2D do
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
  else if (PObject is TLine2D) or (PObject is TArc2D)
    or (PObject is TPolyline2D) or (PObject is TSmoothPath2D)
    or (PObject is TBezierPath2D) then
    with PObject as TPrimitive2D do
    begin
      ArrowsPanel.Visible := True;
      ComboBox8.ItemIndex := Ord(BeginArrowKind);
      ComboBox9.ItemIndex := Ord(EndArrowKind);
      Edit1.Text := FloatToStr(ArrowSizeFactor);
      Panel3.Visible := True;
      PropPages.ActivePage := VoidSheet;
    end
  else if PObject is TStar2D then
    with PObject as TStar2D do
    begin
      ToolBar1.Buttons[Ord(StarKind)].Down := True;
      Edit2.Text := FloatToStr(StarSizeFactor);
      PropPages.ActivePage := StarsSheet;
    end
  else if PObject is TSymbol2D then
    with PObject as TSymbol2D do
    begin
      ComboBox7.ItemIndex := Ord(SymbolKind);
      LabeledEdit5.Text := Format('%.5g', [Diameter]);
      LabeledEdit6.Text := Format('%.6g', [RadToDeg(Rot)]);
      PropPages.ActivePage := SymbolsSheet;
    end
  else if PObject is TRectangle2D then
    with PObject as TRectangle2D do
    begin
      LabeledEdit7.Text := Format('%.5g', [RX]);
      LabeledEdit8.Text := Format('%.5g', [RY]);
      PropPages.ActivePage := RectSheet;
    end
  else
    PropPages.ActivePage := VoidSheet;
end;

procedure TPropertiesForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  I: Integer;
begin
  if ModalResult <> mrOK then Exit;
  (PObject as TPrimitive2D).LineStyle :=
    TLineStyle(ComboBox1.ItemIndex);
  (PObject as TPrimitive2D).Hatching :=
    THatching(ComboBox2.ItemIndex);
  (PObject as TPrimitive2D).LineColor := ColorBoxGet(ComboBox3);
  (PObject as TPrimitive2D).HatchColor := ColorBoxGet(ComboBox4);
  (PObject as TPrimitive2D).FillColor := ColorBoxGet(ComboBox5);
  (PObject as TPrimitive2D).LineWidth :=
    StrToFloat(ComboBox6.Text);
  if PObject is TText2D then
    with PObject as TText2D do
    begin
      Text := LabeledEdit1.Text;
      TeXText := LabeledEdit3.Text;
      Height := StrToFloat(LabeledEdit2.Text);
      Rot := DegToRad(StrToRealType(LabeledEdit4.Text, 0));
      HJustification := THJustification(RadioGroup1.ItemIndex);
      VJustification := TVJustification(RadioGroup2.ItemIndex);
      if not FontCheckBox.Checked then Font.Name := ' ';
    end
  else if (PObject is TLine2D) or (PObject is TArc2D)
    or (PObject is TPolyline2D) or (PObject is TSmoothPath2D)
    or (PObject is TBezierPath2D) then
    with PObject as TPrimitive2D do
    begin
      BeginArrowKind := TArrowKind(ComboBox8.ItemIndex);
      EndArrowKind := TArrowKind(ComboBox9.ItemIndex);
      ArrowSizeFactor := StrToRealType(Edit1.Text, 1);
    end
  else if PObject is TStar2D then
    with PObject as TStar2D do
    begin
      for I := 0 to ToolBar1.ButtonCount - 1 do
        if ToolBar1.Buttons[I].Down then
        begin
          StarKind := TStarKind(I);
          Break;
        end;
      StarSizeFactor := StrToRealType(Edit2.Text, 0);
      PObject.UpdateExtension(nil);
    end
  else if PObject is TSymbol2D then
    with PObject as TSymbol2D do
    begin
      SymbolKind := TSymbolKind(ComboBox7.ItemIndex);
      Diameter := StrToRealType(LabeledEdit5.Text, 0);
      Rot := DegToRad(StrToRealType(LabeledEdit6.Text, 0));
    end
  else if PObject is TRectangle2D then
    with PObject as TRectangle2D do
    begin
      RX := StrToRealType(LabeledEdit7.Text, 0);
      RY := StrToRealType(LabeledEdit8.Text, 0);
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
  TableForm.PPrimitive := PObject as TPrimitive2D;
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
  FontDialog1.Font.Assign((PObject as TText2D).Font);
  if FontDialog1.Execute then
    (PObject as TText2D).Font.Assign(FontDialog1.Font);
end;

procedure TPropertiesForm.FontCheckBoxClick(Sender: TObject);
begin
  Button4.Enabled := FontCheckBox.Checked;
end;

procedure TPropertiesForm.ArrComboBoxDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  with (Control as TComboBox).Canvas do
  begin
    FillRect(Rect);
    TextOut(Rect.Left + 50, Rect.Top, ComboBox8.Items[Index]);
    Rect.Right := Rect.Left + 46;
    Brush.Color := clWhite;
    FillRect(Rect);
    ArrImageList.Draw((Control as TComboBox).Canvas, Rect.Left,
      Rect.Top + 2, Index);
  end;
end;

initialization
{$IFDEF VER140}
{$ELSE}
{$I Propert.lrs}
{$ENDIF}
end.

