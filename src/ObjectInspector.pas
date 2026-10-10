unit ObjectInspector;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Variants, Graphics, Controls, Forms, ExtCtrls,
  StdCtrls, ComCtrls, ImgList, Drawings, GObjects, GObjBase,
  PropertyEdits, PropertyEditCore, EditSessions, PropertyImages;

type
  TInspectorEpochProvider = function: QWord of object;
  TInspectorSessionEvent = procedure(Sender: TObject;
    Session: TObjectEditSession; RegisterSession: Boolean) of object;

  TInspectorFieldWidgets = record
    Row: TPanel;
    LabelText: TLabel;
    Editor: TWinControl;
    StateText: TLabel;
    ErrorText: TLabel;
  end;

  TObjectInspector = class(TPanel)
  private
    FImages: TPropertyImages;
    FHeader: TPanel;
    FSelectionLabel: TLabel;
    FCancelButton: TButton;
    FScroll: TScrollBox;
    FContent: TPanel;
    FEmptyLabel: TLabel;
    FCoordinateButton: TButton;
    FWidgets: array[TObjectPropertyField] of TInspectorFieldWidgets;
    FSectionBodies: array[0..5] of TPanel;
    FSectionButtons: array[0..5] of TButton;
    FSectionTitles: array[0..5] of string;
    FDrawing: TDrawing2D;
    FTargetIDs: TEditTargetIDs;
    FEpochProvider: TInspectorEpochProvider;
    FSession: TObjectEditSession;
    FSessionField: TObjectPropertyField;
    FHasSessionField: Boolean;
    FRefreshing: Integer;
    FHasSelection: Boolean;
    FOnEditCommitted: TNotifyEvent;
    FOnOpenCoordinates: TNotifyEvent;
    FOnSessionChanged: TInspectorSessionEvent;
    procedure BuildView;
    procedure AddSection(Index: Integer; const Title: string);
    procedure AddField(Section: Integer; Field: TObjectPropertyField;
      const Caption: string; IsCombo, IsColor, IsCheck: Boolean);
    procedure PopulateCombo(Field: TObjectPropertyField;
      Combo: TComboBox);
    procedure DrawColorItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure SectionClick(Sender: TObject);
    procedure FieldEnter(Sender: TObject);
    procedure FieldExit(Sender: TObject);
    procedure FieldChanged(Sender: TObject);
    procedure ColorSelect(Sender: TObject);
    procedure FieldKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CoordinatesClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    function FieldFromControl(Sender: TObject): TObjectPropertyField;
    function CurrentEpoch: QWord;
    function BuildPatch(Field: TObjectPropertyField;
      out Patch: TObjectPropertyPatch; out ErrorText: string): Boolean;
    function StartFieldSession(Field: TObjectPropertyField): Boolean;
    function CommitSession(out ErrorText: string): Boolean;
    procedure SetFieldError(Field: TObjectPropertyField;
      const ErrorText: string);
    procedure RefreshField(Field: TObjectPropertyField);
    procedure SetFieldValue(Field: TObjectPropertyField;
      const Value: Variant);
    procedure RefreshDefaults;
    function IsNumericField(Field: TObjectPropertyField): Boolean;
    function NumericRange(Field: TObjectPropertyField): TNumericEditRange;
    function DisplayValue(Field: TObjectPropertyField;
      const Value: Variant): string;
    procedure ReleaseSession;
    procedure RefreshCurrentSelection;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Bind(Drawing: TDrawing2D; const TargetIDs: TEditTargetIDs;
      const EpochProvider: TInspectorEpochProvider);
    procedure RefreshSelection(const TargetIDs: TEditTargetIDs;
      SelectedCount: Integer; HasCoordinateTable: Boolean = False);
    function PrepareForSelectionChange(out ErrorText: string): Boolean;
    procedure CancelActiveEdit;
    procedure FocusField(Field: TObjectPropertyField);
    procedure FocusObject(Obj: TObject2D);
    procedure DrawArrowComboItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure DrawStarComboItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    function EditorFor(Field: TObjectPropertyField): TWinControl;
    property OnEditCommitted: TNotifyEvent read FOnEditCommitted
      write FOnEditCommitted;
    property OnOpenCoordinates: TNotifyEvent read FOnOpenCoordinates
      write FOnOpenCoordinates;
    property OnSessionChanged: TInspectorSessionEvent
      read FOnSessionChanged write FOnSessionChanged;
    property HasActiveEdit: Boolean read FHasSessionField;
  end;

implementation

uses
  Math, Types, ColorEtc, Geometry
{$IFDEF FPC}
  , LCLType
{$ELSE}
  , Windows
{$ENDIF}
  ;

const
  SectionCount = 6;

function FieldName(Field: TObjectPropertyField): string;
begin
  case Field of
    opfLineStyle: Result := 'LineStyle';
    opfLineColor: Result := 'LineColor';
    opfLineWidth: Result := 'LineWidth';
    opfHatching: Result := 'Hatching';
    opfHatchColor: Result := 'HatchColor';
    opfFillColor: Result := 'FillColor';
    opfBeginArrowKind: Result := 'BeginArrow';
    opfEndArrowKind: Result := 'EndArrow';
    opfArrowSizeFactor: Result := 'ArrowSize';
    opfText: Result := 'Text';
    opfTeXText: Result := 'TeXSource';
    opfTextHeight: Result := 'TextHeight';
    opfFontName: Result := 'FontName';
    opfFontStyle: Result := 'FontStyle';
    opfHAlignment: Result := 'TextAlignment';
    opfTextRotation: Result := 'TextRotation';
    opfStarKind: Result := 'StarKind';
    opfStarSizeFactor: Result := 'StarSize';
    opfSymbolKind: Result := 'SymbolKind';
    opfSymbolDiameter: Result := 'SymbolDiameter';
    opfSymbolRotation: Result := 'SymbolRotation';
    opfRectangleRX: Result := 'RectangleRX';
    opfRectangleRY: Result := 'RectangleRY';
    opfBitmapLink: Result := 'BitmapLink';
    opfKeepAspectRatio: Result := 'KeepAspectRatio';
  end;
end;

function IsColorField(Field: TObjectPropertyField): Boolean;
begin
  Result := Field in [opfLineColor, opfHatchColor, opfFillColor];
end;

function SameTargetSet(const A, B: TEditTargetIDs): Boolean;
var
  I, J: Integer;
  Found: Boolean;
begin
  Result := False;
  if Length(A) <> Length(B) then Exit;
  for I := 0 to High(A) do
  begin
    Found := False;
    for J := 0 to High(B) do
      if A[I] = B[J] then
      begin
        Found := True;
        Break;
      end;
    if not Found then Exit;
  end;
  Result := True;
end;

constructor TObjectInspector.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BevelOuter := bvNone;
  BorderSpacing.Around := 0;
  Color := clBtnFace;
  TabStop := True;
  FImages := TPropertyImages.Create(Self);
  BuildView;
end;

destructor TObjectInspector.Destroy;
begin
  if FSession <> nil then
  begin
    FSession.Cancel(CurrentEpoch);
    ReleaseSession;
  end;
  inherited Destroy;
end;

procedure TObjectInspector.BuildView;
begin
  FHeader := TPanel.Create(Self);
  FHeader.Parent := Self;
  FHeader.Align := alTop;
  FHeader.Height := 38;
  FHeader.BevelOuter := bvLowered;

  FSelectionLabel := TLabel.Create(Self);
  FSelectionLabel.Name := 'InspectorSelectionState';
  FSelectionLabel.Parent := FHeader;
  FSelectionLabel.Align := alClient;
  FSelectionLabel.Layout := tlCenter;
  FSelectionLabel.AutoSize := False;
  FSelectionLabel.Caption := 'No selection';
  FSelectionLabel.BorderSpacing.Left := 8;

  FCancelButton := TButton.Create(Self);
  FCancelButton.Parent := FHeader;
  FCancelButton.Align := alRight;
  FCancelButton.Width := 72;
  FCancelButton.Caption := 'Cancel';
  FCancelButton.Visible := False;
  FCancelButton.OnClick := CancelButtonClick;

  FScroll := TScrollBox.Create(Self);
  FScroll.Parent := Self;
  FScroll.Align := alClient;
  FScroll.BorderSpacing.Around := 4;
  FScroll.AutoScroll := True;
  FScroll.HorzScrollBar.Visible := False;

  FContent := TPanel.Create(Self);
  FContent.Parent := FScroll;
  FContent.Align := alTop;
  FContent.BevelOuter := bvNone;
  FContent.AutoSize := True;

  AddSection(0, 'Line');
  AddField(0, opfLineStyle, 'Line style', True, False, False);
  AddField(0, opfLineColor, 'Line color', True, True, False);
  AddField(0, opfLineWidth, 'Width multiplier', False, False, False);
  AddField(0, opfBeginArrowKind, 'Start arrow', True, False, False);
  AddField(0, opfEndArrowKind, 'End arrow', True, False, False);
  AddField(0, opfArrowSizeFactor, 'Arrow size multiplier', False, False, False);

  AddSection(1, 'Fill and hatching');
  AddField(1, opfHatching, 'Hatching', True, False, False);
  AddField(1, opfHatchColor, 'Hatch color', True, True, False);
  AddField(1, opfFillColor, 'Fill color', True, True, False);

  AddSection(2, 'Text');
  AddField(2, opfText, 'Text', False, False, False);
  AddField(2, opfTeXText, 'TeX source', False, False, False);
  AddField(2, opfTextHeight, 'Text height (mm)', False, False, False);
  AddField(2, opfFontName, 'Font family', False, False, False);
  AddField(2, opfFontStyle, 'Font style', True, False, False);
  AddField(2, opfHAlignment, 'Horizontal alignment', True, False, False);
  AddField(2, opfTextRotation, 'Text angle (degrees)', False, False, False);

  AddSection(3, 'Stars and symbols');
  AddField(3, opfStarKind, 'Star shape', True, False, False);
  AddField(3, opfStarSizeFactor, 'Star size multiplier', False, False, False);
  AddField(3, opfSymbolKind, 'Symbol', True, False, False);
  AddField(3, opfSymbolDiameter, 'Symbol diameter (mm)', False, False, False);
  AddField(3, opfSymbolRotation, 'Symbol angle (degrees)', False, False, False);

  AddSection(4, 'Rounded corners');
  AddField(4, opfRectangleRX, 'Horizontal radius (mm)', False, False, False);
  AddField(4, opfRectangleRY, 'Vertical radius (mm)', False, False, False);

  AddSection(5, 'Bitmap');
  AddField(5, opfBitmapLink, 'Image link', False, False, False);
  AddField(5, opfKeepAspectRatio, 'Keep aspect ratio', False, False, True);

  FEmptyLabel := TLabel.Create(Self);
  FEmptyLabel.Parent := FContent;
  FEmptyLabel.Align := alTop;
  FEmptyLabel.AutoSize := False;
  FEmptyLabel.Height := 54;
  FEmptyLabel.WordWrap := True;
  FEmptyLabel.Caption := 'No object selected. New-object defaults remain available in the property toolbars.';
  FEmptyLabel.BorderSpacing.Around := 8;
  FEmptyLabel.Visible := False;

  FCoordinateButton := TButton.Create(Self);
  FCoordinateButton.Parent := FContent;
  FCoordinateButton.Align := alTop;
  FCoordinateButton.Height := 30;
  FCoordinateButton.Caption := 'Point coordinates…';
  FCoordinateButton.OnClick := CoordinatesClick;
  FCoordinateButton.Visible := False;
end;

procedure TObjectInspector.AddSection(Index: Integer; const Title: string);
begin
  FSectionTitles[Index] := Title;
  FSectionButtons[Index] := TButton.Create(Self);
  FSectionButtons[Index].Parent := FContent;
  FSectionButtons[Index].Align := alTop;
  FSectionButtons[Index].Height := 28;
  FSectionButtons[Index].Tag := Index;
  FSectionButtons[Index].Caption := '▾ ' + Title;
  FSectionButtons[Index].OnClick := SectionClick;

  FSectionBodies[Index] := TPanel.Create(Self);
  FSectionBodies[Index].Parent := FContent;
  FSectionBodies[Index].Align := alTop;
  FSectionBodies[Index].BevelOuter := bvNone;
  FSectionBodies[Index].AutoSize := False;
  FSectionBodies[Index].Height := 0;
  FSectionBodies[Index].Visible := Index < 3;
  if Index >= 3 then FSectionButtons[Index].Caption := '▸ ' + Title;
end;

procedure TObjectInspector.AddField(Section: Integer;
  Field: TObjectPropertyField; const Caption: string;
  IsCombo, IsColor, IsCheck: Boolean);
var
  Row: TPanel;
  LabelControl: TLabel;
  Control: TWinControl;
  IsText: Boolean;
begin
  IsText := Field in [opfText, opfTeXText];
  Row := TPanel.Create(Self);
  Row.Parent := FSectionBodies[Section];
  Row.Align := alTop;
  Row.BevelOuter := bvNone;
  if IsText then Row.Height := 112 else Row.Height := 64;
  Inc(FSectionBodies[Section].Height, Row.Height);

  LabelControl := TLabel.Create(Self);
  LabelControl.Parent := Row;
  LabelControl.SetBounds(4, 3, 170, 18);
  LabelControl.Caption := Caption;
  LabelControl.AutoSize := True;

  FWidgets[Field].StateText := TLabel.Create(Self);
  FWidgets[Field].StateText.Name := 'Inspector' + FieldName(Field) + 'State';
  FWidgets[Field].StateText.Parent := Row;
  FWidgets[Field].StateText.Anchors := [akTop, akRight];
  FWidgets[Field].StateText.SetBounds(172, 3, 100, 18);
  FWidgets[Field].StateText.Alignment := taRightJustify;
  FWidgets[Field].StateText.Caption := '';

  FWidgets[Field].ErrorText := TLabel.Create(Self);
  FWidgets[Field].ErrorText.Name := 'Inspector' + FieldName(Field) + 'Error';
  FWidgets[Field].ErrorText.Parent := Row;
  FWidgets[Field].ErrorText.SetBounds(4, Row.Height - 20, 270, 16);
  FWidgets[Field].ErrorText.AutoSize := False;
  FWidgets[Field].ErrorText.Font.Color := clRed;
  FWidgets[Field].ErrorText.Visible := False;

  if IsCheck then
  begin
    Control := TCheckBox.Create(Self);
    (Control as TCheckBox).Caption := Caption;
    LabelControl.Visible := False;
    FWidgets[Field].StateText.Visible := False;
    Control.SetBounds(4, 26, 260, 21);
  end
  else if IsCombo then
  begin
    Control := TComboBox.Create(Self);
    with Control as TComboBox do
    begin
      if IsColor then
      begin
        Style := csDropDown;
        PopulateCombo(Field, Control as TComboBox);
      end
      else if Field in [opfBeginArrowKind, opfEndArrowKind] then
      begin
        Style := csOwnerDrawFixed;
        OnDrawItem := DrawArrowComboItem;
        PopulateCombo(Field, Control as TComboBox);
      end
      else if Field = opfStarKind then
      begin
        Style := csOwnerDrawFixed;
        OnDrawItem := DrawStarComboItem;
        PopulateCombo(Field, Control as TComboBox);
      end
      else
      begin
        Style := csDropDownList;
        PopulateCombo(Field, Control as TComboBox);
      end;
      DropDownCount := 12;
    end;
    Control.SetBounds(4, 23, 264, 23);
  end
  else if IsText then
  begin
    Control := TMemo.Create(Self);
    Control.SetBounds(4, 23, 264, 66);
    (Control as TMemo).ScrollBars := ssVertical;
    (Control as TMemo).WordWrap := True;
    (Control as TMemo).OnChange := FieldChanged;
  end
  else
  begin
    Control := TEdit.Create(Self);
    Control.SetBounds(4, 23, 264, 23);
    (Control as TEdit).OnChange := FieldChanged;
  end;
  Control.Name := 'Inspector' + FieldName(Field) + 'Control';
  Control.Parent := Row;
  Control.Anchors := [akTop, akLeft, akRight];
  Control.Tag := Ord(Field);
  Control.TabOrder := 1;
  Control.OnEnter := FieldEnter;
  Control.OnExit := FieldExit;
  Control.OnKeyDown := FieldKeyDown;
  LabelControl.FocusControl := Control;
  if Control is TComboBox then
    (Control as TComboBox).OnChange := FieldChanged
  else if Control is TCheckBox then
    (Control as TCheckBox).OnChange := FieldChanged;

  FWidgets[Field].Row := Row;
  FWidgets[Field].LabelText := LabelControl;
  FWidgets[Field].Editor := Control;
end;

procedure TObjectInspector.PopulateCombo(Field: TObjectPropertyField;
  Combo: TComboBox);
var
  I: Integer;
  Style: TFontStyles;
  Caption: string;
begin
  Combo.Items.BeginUpdate;
  try
    case Field of
      opfLineColor, opfHatchColor, opfFillColor:
        begin
          Combo.Items.Add('Default');
          for I := 0 to TheHTMLColor.Count - 1 do
            Combo.Items.Add(TheHTMLColor[I]);
        end;
      opfLineStyle:
        begin
          Combo.Items.Add('None');
          Combo.Items.Add('Solid');
          Combo.Items.Add('Dotted');
          Combo.Items.Add('Dashed');
        end;
      opfHatching:
        begin
          Combo.Items.Add('None');
          Combo.Items.Add('Horizontal');
          Combo.Items.Add('Vertical');
          Combo.Items.Add('Forward diagonal');
          Combo.Items.Add('Backward diagonal');
          Combo.Items.Add('Cross');
          Combo.Items.Add('Diagonal cross');
        end;
      opfBeginArrowKind, opfEndArrowKind:
        for I := Low(ArrowsIDs) to High(ArrowsIDs) do
          Combo.Items.Add(string(ArrowsIDs[I]));
      opfHAlignment:
        begin
          Combo.Items.Add('Left');
          Combo.Items.Add('Center');
          Combo.Items.Add('Right');
        end;
      opfStarKind:
        for I := Low(StarsIDs) to High(StarsIDs) do
          Combo.Items.Add(string(StarsIDs[I]));
      opfSymbolKind:
        for I := Low(SymbolsIDs) to High(SymbolsIDs) do
          Combo.Items.Add(string(SymbolsIDs[I]));
      opfFontStyle:
        for I := 0 to 15 do
        begin
          Style := TFontStyles(Byte(I));
          Caption := '';
          if fsBold in Style then Caption := Caption + 'Bold';
          if fsItalic in Style then
          begin
            if Caption <> '' then Caption := Caption + ', ';
            Caption := Caption + 'Italic';
          end;
          if fsUnderline in Style then
          begin
            if Caption <> '' then Caption := Caption + ', ';
            Caption := Caption + 'Underline';
          end;
          if fsStrikeOut in Style then
          begin
            if Caption <> '' then Caption := Caption + ', ';
            Caption := Caption + 'Strikeout';
          end;
          if Caption = '' then Caption := 'Regular';
          Combo.Items.Add(Caption);
        end;
    end;
  finally
    Combo.Items.EndUpdate;
  end;
end;

procedure TObjectInspector.DrawColorItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  ColorBoxDrawItem(Control as TComboBox, Index, Rect, State);
end;

procedure TObjectInspector.DrawArrowComboItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  if not (Control is TComboBox) then Exit;
  with (Control as TComboBox).Canvas do
  begin
    FillRect(Rect);
    if (Index < 0) or (Index >= (Control as TComboBox).Items.Count) then Exit;
    TextOut(Rect.Left + 47, Rect.Top,
      (Control as TComboBox).Items[Index]);
    Rect.Right := Rect.Left + 46;
    Brush.Color := clWhite;
    FillRect(Rect);
    if Index < FImages.ArrImageList.Count then
      FImages.ArrImageList.Draw((Control as TComboBox).Canvas,
        Rect.Left, Rect.Top + 2, Index);
  end;
end;

procedure TObjectInspector.DrawStarComboItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  if not (Control is TComboBox) then Exit;
  with (Control as TComboBox).Canvas do
  begin
    FillRect(Rect);
    if (Index >= 0) and (Index < FImages.StarsImageList.Count) then
      FImages.StarsImageList.Draw((Control as TComboBox).Canvas,
        Rect.Left, Rect.Top + 1, Index);
    if (Index >= 0) and (Index < (Control as TComboBox).Items.Count) then
      TextOut(Rect.Left + 20, Rect.Top + 1,
        (Control as TComboBox).Items[Index]);
  end;
end;

procedure TObjectInspector.SectionClick(Sender: TObject);
var
  Index: Integer;
  Expanded: Boolean;
begin
  if not (Sender is TButton) then Exit;
  Index := (Sender as TButton).Tag;
  if (Index < 0) or (Index >= SectionCount) then Exit;
  Expanded := not FSectionBodies[Index].Visible;
  FSectionBodies[Index].Visible := Expanded;
  if Expanded then
    (Sender as TButton).Caption := '▾ ' + FSectionTitles[Index]
  else
    (Sender as TButton).Caption := '▸ ' + FSectionTitles[Index];
end;

procedure TObjectInspector.Bind(Drawing: TDrawing2D;
  const TargetIDs: TEditTargetIDs;
  const EpochProvider: TInspectorEpochProvider);
begin
  FDrawing := Drawing;
  FTargetIDs := Copy(TargetIDs);
  FEpochProvider := EpochProvider;
end;

function TObjectInspector.CurrentEpoch: QWord;
begin
  if Assigned(FEpochProvider) then Result := FEpochProvider()
  else Result := 0;
end;

function TObjectInspector.EditorFor(
  Field: TObjectPropertyField): TWinControl;
begin
  Result := FWidgets[Field].Editor;
end;

function TObjectInspector.IsNumericField(
  Field: TObjectPropertyField): Boolean;
begin
  Result := Field in [opfLineWidth, opfArrowSizeFactor, opfTextHeight,
    opfTextRotation, opfStarSizeFactor, opfSymbolDiameter,
    opfSymbolRotation, opfRectangleRX, opfRectangleRY];
end;

function TObjectInspector.NumericRange(
  Field: TObjectPropertyField): TNumericEditRange;
begin
  case Field of
    opfTextHeight: Result := nerPositive;
    opfTextRotation, opfSymbolRotation: Result := nerFinite;
  else
    Result := nerNonnegative;
  end;
end;

function TObjectInspector.DisplayValue(Field: TObjectPropertyField;
  const Value: Variant): string;
var
  Number: Double;
begin
  case Field of
    opfText: Result := DecodeTpXTextForEdit(AnsiString(VarToStr(Value)));
    opfLineColor, opfHatchColor, opfFillColor:
      if TColor(Integer(Value)) = clDefault then Result := 'Default'
      else Result := ColorToHtml(TColor(Integer(Value)));
    opfTextRotation, opfSymbolRotation:
      begin
        Number := RadToDeg(Double(Value));
        Result := FloatToStr(Number);
      end;
    opfLineWidth, opfArrowSizeFactor, opfTextHeight,
    opfStarSizeFactor, opfSymbolDiameter, opfRectangleRX,
    opfRectangleRY:
      Result := RealTypeToStr(TRealType(Value));
  else
    Result := VarToStr(Value);
  end;
end;

procedure TObjectInspector.SetFieldValue(Field: TObjectPropertyField;
  const Value: Variant);
var
  Editor: TWinControl;
  Index: Integer;
begin
  Editor := FWidgets[Field].Editor;
  if Editor = nil then Exit;
  if (Editor is TEdit) then
    (Editor as TEdit).Text := DisplayValue(Field, Value)
  else if Editor is TMemo then
    (Editor as TMemo).Text := DisplayValue(Field, Value)
  else if (Editor is TComboBox) then
  begin
    if IsColorField(Field) then
      (Editor as TComboBox).Text := DisplayValue(Field, Value)
    else
    begin
      Index := Integer(Value);
      if (Field = opfBeginArrowKind) or (Field = opfEndArrowKind) then
        if Index >= (Editor as TComboBox).Items.Count then Index := -1;
      (Editor as TComboBox).ItemIndex := Index;
    end;
  end
  else if Editor is TCheckBox then
    (Editor as TCheckBox).Checked := Boolean(Value);
end;

procedure TObjectInspector.RefreshField(Field: TObjectPropertyField);
var
  Value: TPropertyValue;
  Editor: TWinControl;
begin
  Editor := FWidgets[Field].Editor;
  if Editor = nil then Exit;
  if (FHasSessionField) and (FSessionField = Field) and
    ((FSession <> nil) or Editor.Focused) then Exit;
  Value := ReadObjectProperty(FDrawing, FTargetIDs, Field);
  case Value.State of
    pvsUnavailable:
      begin
        FWidgets[Field].StateText.Caption := 'Unavailable';
        Editor.Enabled := False;
        if Editor is TEdit then (Editor as TEdit).Text := ''
        else if Editor is TMemo then (Editor as TMemo).Clear
        else if Editor is TComboBox then (Editor as TComboBox).ItemIndex := -1
        else if Editor is TCheckBox then (Editor as TCheckBox).Checked := False;
      end;
    pvsMixed:
      begin
        FWidgets[Field].StateText.Caption := 'Mixed';
        Editor.Enabled := True;
        if Editor is TEdit then (Editor as TEdit).Text := ''
        else if Editor is TMemo then (Editor as TMemo).Clear
        else if Editor is TComboBox then (Editor as TComboBox).ItemIndex := -1
        else if Editor is TCheckBox then (Editor as TCheckBox).Checked := False;
      end;
    pvsUniform:
      begin
        FWidgets[Field].StateText.Caption := '';
        Editor.Enabled := True;
        SetFieldValue(Field, Value.Value);
      end;
  end;
  SetFieldError(Field, '');
end;

procedure TObjectInspector.RefreshDefaults;
var
  D: TDrawing2D;
  Field: TObjectPropertyField;
begin
  if FDrawing = nil then Exit;
  D := FDrawing;
  FEmptyLabel.Caption :=
    'No selection. These are new-object defaults; the property toolbars change them.';
  FEmptyLabel.Visible := True;
  for Field := Low(TObjectPropertyField) to High(TObjectPropertyField) do
  begin
    FWidgets[Field].StateText.Caption := 'Unavailable';
    FWidgets[Field].Editor.Enabled := False;
    SetFieldError(Field, '');
  end;
  FWidgets[opfLineStyle].StateText.Caption := '';
  FWidgets[opfLineColor].StateText.Caption := '';
  FWidgets[opfLineWidth].StateText.Caption := '';
  FWidgets[opfHatching].StateText.Caption := '';
  FWidgets[opfHatchColor].StateText.Caption := '';
  FWidgets[opfFillColor].StateText.Caption := '';
  FWidgets[opfBeginArrowKind].StateText.Caption := '';
  FWidgets[opfEndArrowKind].StateText.Caption := '';
  FWidgets[opfArrowSizeFactor].StateText.Caption := '';
  FWidgets[opfTextHeight].StateText.Caption := '';
  FWidgets[opfHAlignment].StateText.Caption := '';
  FWidgets[opfStarKind].StateText.Caption := '';
  FWidgets[opfStarSizeFactor].StateText.Caption := '';
  SetFieldValue(opfLineStyle, Ord(D.New_LineStyle));
  SetFieldValue(opfLineColor, Integer(D.New_LineColor));
  SetFieldValue(opfLineWidth, Double(D.New_LineWidth));
  SetFieldValue(opfHatching, Ord(D.New_Hatching));
  SetFieldValue(opfHatchColor, Integer(D.New_HatchColor));
  SetFieldValue(opfFillColor, Integer(D.New_FillColor));
  SetFieldValue(opfBeginArrowKind, D.New_Arr1);
  SetFieldValue(opfEndArrowKind, D.New_Arr2);
  SetFieldValue(opfArrowSizeFactor, Double(D.New_ArrSizeFactor));
  SetFieldValue(opfTextHeight, Double(D.New_FontHeight));
  SetFieldValue(opfHAlignment, D.New_HAlignment);
  SetFieldValue(opfStarKind, D.New_StarKind);
  SetFieldValue(opfStarSizeFactor, Double(D.New_StarSizeFactor));
  for Field in [opfLineStyle, opfLineColor, opfLineWidth,
    opfHatching, opfHatchColor, opfFillColor, opfBeginArrowKind,
    opfEndArrowKind, opfArrowSizeFactor, opfTextHeight,
    opfHAlignment, opfStarKind, opfStarSizeFactor] do
    FWidgets[Field].Editor.Enabled := False;
end;

procedure TObjectInspector.RefreshSelection(
  const TargetIDs: TEditTargetIDs; SelectedCount: Integer;
  HasCoordinateTable: Boolean);
var
  Field: TObjectPropertyField;
  ErrorText: string;
begin
  if (FSession <> nil) and not FSession.IsActive then ReleaseSession;
  if FHasSessionField and not SameTargetSet(FTargetIDs, TargetIDs) and
    not PrepareForSelectionChange(ErrorText) then
  begin
    FSelectionLabel.Caption := 'Finish or cancel the active property edit';
    Exit;
  end;
  FTargetIDs := Copy(TargetIDs);
  FRefreshing := FRefreshing + 1;
  try
    FHasSelection := Length(FTargetIDs) > 0;
    FEmptyLabel.Visible := not FHasSelection;
    FCoordinateButton.Visible := FHasSelection;
    FCoordinateButton.Enabled := HasCoordinateTable;
    if FHasSelection then
    begin
      FSelectionLabel.Caption := Format('%d object(s) selected', [SelectedCount]);
      for Field := Low(TObjectPropertyField) to High(TObjectPropertyField) do
        RefreshField(Field);
    end
    else
    begin
      FSelectionLabel.Caption := 'New-object defaults';
      RefreshDefaults;
    end;
  finally
    Dec(FRefreshing);
  end;
end;

procedure TObjectInspector.ReleaseSession;
var
  OldSession: TObjectEditSession;
begin
  OldSession := FSession;
  if OldSession = nil then Exit;
  FSession := nil;
  FHasSessionField := False;
  FCancelButton.Visible := False;
  if Assigned(FOnSessionChanged) then
    FOnSessionChanged(Self, OldSession, False);
  OldSession.Free;
end;

procedure TObjectInspector.RefreshCurrentSelection;
var
  Field: TObjectPropertyField;
begin
  if FDrawing = nil then Exit;
  FRefreshing := FRefreshing + 1;
  try
    if Length(FTargetIDs) = 0 then
      RefreshDefaults
    else
      for Field := Low(TObjectPropertyField) to High(TObjectPropertyField) do
        RefreshField(Field);
  finally
    Dec(FRefreshing);
  end;
end;

function TObjectInspector.FieldFromControl(
  Sender: TObject): TObjectPropertyField;
var
  Value: Integer;
begin
  Value := (Sender as TControl).Tag;
  if (Value < Ord(Low(TObjectPropertyField))) or
    (Value > Ord(High(TObjectPropertyField))) then
    Result := Low(TObjectPropertyField)
  else
    Result := TObjectPropertyField(Value);
end;

function TObjectInspector.BuildPatch(Field: TObjectPropertyField;
  out Patch: TObjectPropertyPatch; out ErrorText: string): Boolean;
var
  Editor: TWinControl;
  Combo: TComboBox;
  Check: TCheckBox;
  Raw: string;
  Number: Double;
  ColorValue: TColor;
begin
  Patch.Fields := [];
  ErrorText := '';
  Editor := FWidgets[Field].Editor;
  Include(Patch.Fields, Field);
  if IsNumericField(Field) then
  begin
    if Editor is TEdit then Raw := (Editor as TEdit).Text
    else Raw := '';
    if not TryParseNumericEdit(Raw, NumericRange(Field), Number,
      ErrorText) then Exit(False);
    if Field in [opfTextRotation, opfSymbolRotation] then
      Number := DegToRad(Number);
    case Field of
      opfLineWidth: Patch.LineWidth := TRealType(Number);
      opfArrowSizeFactor: Patch.ArrowSizeFactor := TRealType(Number);
      opfTextHeight: Patch.TextHeight := TRealType(Number);
      opfTextRotation: Patch.TextRotation := TRealType(Number);
      opfStarSizeFactor: Patch.StarSizeFactor := TRealType(Number);
      opfSymbolDiameter: Patch.SymbolDiameter := TRealType(Number);
      opfSymbolRotation: Patch.SymbolRotation := TRealType(Number);
      opfRectangleRX: Patch.RectangleRX := TRealType(Number);
      opfRectangleRY: Patch.RectangleRY := TRealType(Number);
    end;
    Exit(True);
  end;
  if Editor is TComboBox then
  begin
    Combo := Editor as TComboBox;
    if IsColorField(Field) then
    begin
      Raw := Trim(Combo.Text);
      if SameText(Raw, 'Default') then
        ColorValue := clDefault
      else
        ColorValue := HtmlToColor(Raw);
      if ColorValue = clNone then
      begin
        ErrorText := 'Enter a named color or #RRGGBB';
        Exit(False);
      end;
      case Field of
        opfLineColor: Patch.LineColor := ColorValue;
        opfHatchColor: Patch.HatchColor := ColorValue;
        opfFillColor: Patch.FillColor := ColorValue;
      end;
      Exit(True);
    end;
    if Combo.ItemIndex < 0 then
    begin
      ErrorText := 'Choose a value from the list';
      Exit(False);
    end;
    case Field of
      opfLineStyle: Patch.LineStyle := TLineStyle(Combo.ItemIndex);
      opfHatching: Patch.Hatching := THatching(Combo.ItemIndex);
      opfBeginArrowKind: Patch.BeginArrowKind := TArrowKind(Combo.ItemIndex);
      opfEndArrowKind: Patch.EndArrowKind := TArrowKind(Combo.ItemIndex);
      opfFontStyle: Patch.FontStyle := TFontStyles(Byte(Combo.ItemIndex));
      opfHAlignment: Patch.HAlignment := THAlignment(Combo.ItemIndex);
      opfStarKind: Patch.StarKind := TStarKind(Combo.ItemIndex);
      opfSymbolKind: Patch.SymbolKind := TSymbolKind(Combo.ItemIndex);
    end;
    Exit(True);
  end;
  if Editor is TCheckBox then
  begin
    Check := Editor as TCheckBox;
    Patch.KeepAspectRatio := Check.Checked;
    Exit(True);
  end;
  if Editor is TMemo then Raw := (Editor as TMemo).Text
  else if Editor is TEdit then Raw := (Editor as TEdit).Text
  else Raw := '';
  case Field of
    opfText: Patch.Text := EncodeEditTextForTpX(Raw);
    opfTeXText: Patch.TeXText := AnsiString(Raw);
    opfFontName: Patch.FontName := Raw;
    opfBitmapLink: Patch.BitmapLink := Raw;
  end;
  Result := True;
end;

function TObjectInspector.StartFieldSession(
  Field: TObjectPropertyField): Boolean;
var
  ErrorText: string;
begin
  Result := False;
  if not FHasSelection or (FDrawing = nil) then Exit;
  if (FSession <> nil) and not FSession.IsActive then ReleaseSession;
  if FSession <> nil then
  begin
    if FSession.IsActive and (FSessionField = Field) then Exit(True);
    if not CommitSession(ErrorText) then
    begin
      FSelectionLabel.Caption := 'Complete or cancel ' +
        FieldName(FSessionField) + ' before editing another field';
      Exit;
    end;
  end;
  FSession := TObjectEditSession.Create(FDrawing, CurrentEpoch,
    FTargetIDs);
  FSessionField := Field;
  FHasSessionField := True;
  FCancelButton.Visible := True;
  FSession.SetRawDraftPending(False);
  if Assigned(FOnSessionChanged) then
    FOnSessionChanged(Self, FSession, True);
  Result := True;
end;

function TObjectInspector.CommitSession(out ErrorText: string): Boolean;
var
  Field: TObjectPropertyField;
begin
  ErrorText := '';
  if FSession = nil then Exit(True);
  Field := FSessionField;
  Result := FSession.Commit(CurrentEpoch, ErrorText);
  if not Result then
  begin
    SetFieldError(Field, ErrorText);
    Exit;
  end;
  ReleaseSession;
  RefreshCurrentSelection;
  if Assigned(FOnEditCommitted) then FOnEditCommitted(Self);
end;

function TObjectInspector.PrepareForSelectionChange(
  out ErrorText: string): Boolean;
begin
  Result := CommitSession(ErrorText);
end;

procedure TObjectInspector.CancelActiveEdit;
begin
  if FSession = nil then Exit;
  FSession.Cancel(CurrentEpoch);
  ReleaseSession;
  RefreshCurrentSelection;
end;

procedure TObjectInspector.SetFieldError(Field: TObjectPropertyField;
  const ErrorText: string);
begin
  FWidgets[Field].ErrorText.Caption := ErrorText;
  FWidgets[Field].ErrorText.Visible := ErrorText <> '';
end;

procedure TObjectInspector.FieldEnter(Sender: TObject);
begin
  if FRefreshing > 0 then Exit;
  FSelectionLabel.Caption := 'Editing ' +
    FieldName(FieldFromControl(Sender));
end;

procedure TObjectInspector.FieldExit(Sender: TObject);
var
  Field: TObjectPropertyField;
  ErrorText: string;
begin
  if FRefreshing > 0 then Exit;
  Field := FieldFromControl(Sender);
  if FHasSessionField and (FSessionField = Field) then
    CommitSession(ErrorText);
end;

procedure TObjectInspector.FieldChanged(Sender: TObject);
var
  Field: TObjectPropertyField;
  Patch: TObjectPropertyPatch;
  ErrorText: string;
  Raw: string;
begin
  if FRefreshing > 0 then Exit;
  Field := FieldFromControl(Sender);
  if not StartFieldSession(Field) then
  begin
    if FHasSessionField and (FSessionField <> Field) then
      SetFieldError(Field, 'Finish or cancel the active edit first');
    Exit;
  end;
  if FWidgets[Field].Editor is TEdit then
    Raw := (FWidgets[Field].Editor as TEdit).Text
  else if FWidgets[Field].Editor is TMemo then
    Raw := (FWidgets[Field].Editor as TMemo).Text
  else if FWidgets[Field].Editor is TComboBox then
    Raw := (FWidgets[Field].Editor as TComboBox).Text
  else if FWidgets[Field].Editor is TCheckBox then
    Raw := BoolToStr((FWidgets[Field].Editor as TCheckBox).Checked, True)
  else
    Raw := '';
  if not FSession.TrySetRawDraft(Field, Raw, ErrorText) then
  begin
    FSession.SetRawDraftPendingForField(Field, True);
    SetFieldError(Field, ErrorText);
    Exit;
  end;
  if not BuildPatch(Field, Patch, ErrorText) then
  begin
    FSession.SetRawDraftPendingForField(Field, True);
    SetFieldError(Field, ErrorText);
    Exit;
  end;
  if not FSession.Preview(Patch, CurrentEpoch, ErrorText) then
  begin
    FSession.SetRawDraftPendingForField(Field, True);
    SetFieldError(Field, ErrorText);
    Exit;
  end;
  FSession.SetRawDraftPendingForField(Field, False);
  SetFieldError(Field, '');
  FSelectionLabel.Caption := Format('%d object(s) selected',
    [Length(FTargetIDs)]);
end;

procedure TObjectInspector.ColorSelect(Sender: TObject);
begin
  if FRefreshing > 0 then Exit;
  ColorBoxSelect(Sender as TComboBox);
  FieldChanged(Sender);
end;

procedure TObjectInspector.FieldKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  ErrorText: string;
  Field: TObjectPropertyField;
begin
  Field := FieldFromControl(Sender);
  if (Key = VK_ESCAPE) and FHasSessionField and
    (FSessionField = Field) then
  begin
    CancelActiveEdit;
    Key := 0;
  end
  else if (Key = VK_RETURN) and FHasSessionField and
    (FSessionField = Field) and
    ((not (FWidgets[Field].Editor is TMemo)) or
    (ssCtrl in Shift) or (ssMeta in Shift)) then
  begin
    CommitSession(ErrorText);
    Key := 0;
  end;
end;

procedure TObjectInspector.CoordinatesClick(Sender: TObject);
var
  ErrorText: string;
begin
  if not PrepareForSelectionChange(ErrorText) then
  begin
    FSelectionLabel.Caption := ErrorText;
    Exit;
  end;
  if Assigned(FOnOpenCoordinates) then FOnOpenCoordinates(Self);
end;

procedure TObjectInspector.CancelButtonClick(Sender: TObject);
begin
  CancelActiveEdit;
end;

procedure TObjectInspector.FocusField(Field: TObjectPropertyField);
var
  Section: Integer;
begin
  if Field in [opfLineStyle, opfLineColor, opfLineWidth,
    opfBeginArrowKind, opfEndArrowKind, opfArrowSizeFactor] then
    Section := 0
  else if Field in [opfHatching, opfHatchColor, opfFillColor] then
    Section := 1
  else if Field in [opfText, opfTeXText, opfTextHeight, opfFontName,
    opfFontStyle, opfHAlignment, opfTextRotation] then
    Section := 2
  else if Field in [opfStarKind, opfStarSizeFactor,
    opfSymbolKind, opfSymbolDiameter, opfSymbolRotation] then
    Section := 3
  else if Field in [opfRectangleRX, opfRectangleRY] then
    Section := 4
  else
    Section := 5;
  if not FSectionBodies[Section].Visible then
  begin
    FSectionBodies[Section].Visible := True;
    FSectionButtons[Section].Caption := '▾ ' + FSectionTitles[Section];
  end;
  if FWidgets[Field].Editor.CanFocus then
    FWidgets[Field].Editor.SetFocus;
end;

procedure TObjectInspector.FocusObject(Obj: TObject2D);
var
  IDs: TEditTargetIDs;
  ErrorText: string;
begin
  if (Obj = nil) or (FDrawing = nil) then Exit;
  if not PrepareForSelectionChange(ErrorText) then
  begin
    FSelectionLabel.Caption := ErrorText;
    Exit;
  end;
  FDrawing.SelectionClear;
  FDrawing.SelectionAdd(Obj);
  SetLength(IDs, 1);
  IDs[0] := Obj.ID;
  RefreshSelection(IDs, 1, Obj is TPrimitive2D);
end;



end.
