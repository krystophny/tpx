unit ObjectInspector;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Variants, Graphics, Controls, Forms, ExtCtrls,
  StdCtrls, ComCtrls, Buttons, ImgList, Drawings, GObjects, GObjBase,
  PropertyEdits, PropertyEditCore, EditSessions, PropertyImages, Devices,
  CompositionAwareControls;

type
  TInspectorEpochProvider = function: QWord of object;
  TInspectorSessionEvent = function(Sender: TObject;
    Session: TObjectEditSession; RegisterSession: Boolean): Boolean of object;
  TInspectorBeforeFieldInput = function(Sender: TObject;
    CurrentSession: TObjectEditSession): Boolean of object;
  TInspectorFieldRefreshGuard = function(Sender: TObject;
    Field: TObjectPropertyField; const TargetIDs: TEditTargetIDs): Boolean of object;

  TInspectorFieldWidgets = record
    Row: TPanel;
    LabelText: TLabel;
    HintText: TLabel;
    Editor: TWinControl;
    ErrorText: TLabel;
  end;

  TObjectInspector = class(TPanel)
  private
    { 3D size fields whose meaning differs across the selected bodies. }
    FSolidAmbiguous: TObjectPropertyFields;
    FImages: TPropertyImages;
    FHeader: TPanel;
    FSelectionLabel: TLabel;
    FCancelButton: TButton;
    FCloseButton: TSpeedButton;
    FScroll: TScrollBox;
    FContent: TPanel;
    FEmptyLabel: TLabel;
    FCoordinateButton: TButton;
    FMoreButton: TCheckBox;
    FShowMore: Boolean;
    FWidgets: array[TObjectPropertyField] of TInspectorFieldWidgets;
    FApplicable: array[TObjectPropertyField] of Boolean;
    FTitle: string;
    FDrawing: TDrawing2D;
    FTargetIDs: TEditTargetIDs;
    FEpochProvider: TInspectorEpochProvider;
    FSession: TObjectEditSession;
    FSessionField: TObjectPropertyField;
    FHasSessionField: Boolean;
    FCompositionTextChanged: set of TObjectPropertyField;
    FCompositionBaselineValid: set of TObjectPropertyField;
    FCompositionBaselineText: array[TObjectPropertyField] of string;
    FCompositionBaselineSelStart: array[TObjectPropertyField] of Integer;
    FCompositionBaselineSelLength: array[TObjectPropertyField] of Integer;
    FStableFieldTextValid: set of TObjectPropertyField;
    FStableFieldText: array[TObjectPropertyField] of string;
    FStableFieldSelStart: array[TObjectPropertyField] of Integer;
    FStableFieldSelLength: array[TObjectPropertyField] of Integer;
    FRefreshing: Integer;
    FRestoringFieldInput: Boolean;
    FHasSelection: Boolean;
    FOnEditCommitted: TNotifyEvent;
    FOnOpenCoordinates: TNotifyEvent;
    FOnSessionChanged: TInspectorSessionEvent;
    FOnBeforeFieldInput: TInspectorBeforeFieldInput;
    FOnShouldSkipFieldRefresh: TInspectorFieldRefreshGuard;
    FOnCloseRequest: TNotifyEvent;
    FOnShowMoreChanged: TNotifyEvent;
    procedure BuildView;
    function ContentWidth: Integer;
    procedure LayoutRows;
    procedure ContentResize(Sender: TObject);
    procedure AddField(Field: TObjectPropertyField;
      const Caption: string; IsCombo, IsColor, IsCheck: Boolean);
    procedure PopulateCombo(Field: TObjectPropertyField;
      Combo: TComboBox);
    procedure MoreClick(Sender: TObject);
    procedure CloseClick(Sender: TObject);
    procedure SetShowMore(Value: Boolean);
    procedure SetFieldState(Field: TObjectPropertyField; Mixed: Boolean;
      const Hint: string);
    function SelectionTitle(SelectedCount: Integer): string;
    procedure ShowEmptySelection;
    procedure FieldEnter(Sender: TObject);
    procedure FieldChanged(Sender: TObject);
    procedure CompositionChanged(Sender: TObject; Composing: Boolean);
    procedure CaptureStableField(Field: TObjectPropertyField);
    procedure CaptureStableSelection(Field: TObjectPropertyField);
    function EditorText(Field: TObjectPropertyField): string;
    procedure RestoreCompositionBaseline(Field: TObjectPropertyField);
    procedure FieldKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FieldKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FieldMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
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
    procedure RejectBlockedField(Field: TObjectPropertyField);
    procedure RefreshField(Field: TObjectPropertyField);
    procedure ClearFieldEditor(Field: TObjectPropertyField);
    procedure SetFieldValue(Field: TObjectPropertyField;
      const Value: Variant);
    function IsNumericField(Field: TObjectPropertyField): Boolean;
    function NumericRange(Field: TObjectPropertyField): TNumericEditRange;
    function DisplayValue(Field: TObjectPropertyField;
      const Value: Variant): string;
    procedure ReleaseSession;
    procedure RefreshCurrentSelection;
    procedure UpdateSolidCaptions;
    function EditorCompositionPending(Editor: TWinControl): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Bind(Drawing: TDrawing2D; const TargetIDs: TEditTargetIDs;
      const EpochProvider: TInspectorEpochProvider);
    procedure RefreshSelection(const TargetIDs: TEditTargetIDs;
      SelectedCount: Integer; HasCoordinateTable: Boolean = False);
    function AdoptRecoverySession(Session: TObjectEditSession): Boolean;
    function PrepareForSelectionChange(out ErrorText: string): Boolean;
    procedure CancelActiveEdit;
    procedure RefreshCompositionState;
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
    property OnBeforeFieldInput: TInspectorBeforeFieldInput
      read FOnBeforeFieldInput write FOnBeforeFieldInput;
    property OnShouldSkipFieldRefresh: TInspectorFieldRefreshGuard
      read FOnShouldSkipFieldRefresh write FOnShouldSkipFieldRefresh;
    property OnCloseRequest: TNotifyEvent read FOnCloseRequest
      write FOnCloseRequest;
    property OnShowMoreChanged: TNotifyEvent read FOnShowMoreChanged
      write FOnShowMoreChanged;
    property ShowMore: Boolean read FShowMore write SetShowMore;
    property HasActiveEdit: Boolean read FHasSessionField;
    function FieldShown(Field: TObjectPropertyField): Boolean;
  end;

function InspectorNumberText(const Value: Double): string;

implementation

uses
  Types, ColorEtc, Geometry, Solid3D
{$IFDEF FPC}
  , LCLType
{$ELSE}
  , Windows
{$ENDIF}
  ;

const
  { Object-specific properties come first; the style properties that the
    toolbars above the canvas also edit stay behind "More". }
  PrimaryFields: array[0..15] of TObjectPropertyField = (opfText,
    opfTeXText, opfTextHeight, opfTextRotation, opfSymbolKind,
    opfSymbolDiameter, opfSymbolRotation, opfRectangleRX, opfRectangleRY,
    opfBitmapLink, opfKeepAspectRatio, opfSolidSize1, opfSolidSize2,
    opfSolidSize3, opfSolidSize4, opfSolidSegments);
  MoreFields: array[0..16] of TObjectPropertyField = (opfLineStyle,
    opfLineColor, opfLineWidth, opfBeginArrowKind, opfEndArrowKind,
    opfArrowSizeFactor, opfHatching, opfHatchColor, opfFillColor,
    opfFontName, opfFontStyle, opfHAlignment, opfStarKind,
    opfStarSizeFactor, opfSolidAzimuth, opfSolidElevation,
    opfSolidPerspective);
  LabelWidth = 112;
  RowHeight = 30;
  MemoHeight = 50;

function InspectorNumberText(const Value: Double): string;
var
  Settings: TFormatSettings;
begin
  Settings := DefaultFormatSettings;
  Settings.DecimalSeparator := '.';
  { Values are stored in single precision; seven digits hide its noise. }
  Result := FloatToStrF(Value, ffGeneral, 7, 0, Settings);
end;

{ Changing a label's colour or style detaches it from the parent font; keep
  the size and face the rest of the panel uses. }
procedure StyleLabel(LabelControl: TLabel; Style: TFontStyles;
  Color: TColor);
begin
  LabelControl.Font.Assign(LabelControl.Parent.Font);
  LabelControl.Font.Style := Style;
  LabelControl.Font.Color := Color;
end;

function IsMoreField(Field: TObjectPropertyField): Boolean;
var I: Integer;
begin
  Result := False;
  for I := Low(MoreFields) to High(MoreFields) do
    if MoreFields[I] = Field then Exit(True);
end;

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
    opfSolidSize1: Result := 'SolidSize1';
    opfSolidSize2: Result := 'SolidSize2';
    opfSolidSize3: Result := 'SolidSize3';
    opfSolidSize4: Result := 'SolidSize4';
    opfSolidSegments: Result := 'SolidSegments';
    opfSolidAzimuth: Result := 'Azimuth';
    opfSolidElevation: Result := 'Elevation';
    opfSolidPerspective: Result := 'Perspective';
  end;
end;


function FontStylesFromMask(Mask: Integer): TFontStyles;
begin
  Result := [];
  if (Mask and 1) <> 0 then Include(Result, fsBold);
  if (Mask and 2) <> 0 then Include(Result, fsItalic);
  if (Mask and 4) <> 0 then Include(Result, fsUnderline);
  if (Mask and 8) <> 0 then Include(Result, fsStrikeOut);
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

function UTF8BytesToLCLText(const Value: TEditUtf8Text): string;
var
  Bytes: RawByteString;
begin
  Bytes := RawByteString(Value);
  SetCodePage(Bytes, DefaultSystemCodePage, False);
  Result := string(Bytes);
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

function TObjectInspector.ContentWidth: Integer;
begin
  Result := FContent.ClientWidth;
  if Result < 8 then Result := 264;
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
  FHeader.Height := 30;
  FHeader.BevelOuter := bvNone;

  FCloseButton := TSpeedButton.Create(Self);
  FCloseButton.Name := 'InspectorCloseButton';
  FCloseButton.Parent := FHeader;
  FCloseButton.Align := alRight;
  FCloseButton.Width := 26;
  FCloseButton.Flat := True;
  FCloseButton.Caption := '×';
  FCloseButton.Font.Size := 13;
  FCloseButton.Hint := 'Hide the inspector';
  FCloseButton.ShowHint := True;
  FCloseButton.OnClick := CloseClick;

  FCancelButton := TButton.Create(Self);
  FCancelButton.Parent := FHeader;
  FCancelButton.Align := alRight;
  FCancelButton.Width := 72;
  FCancelButton.Caption := 'Cancel';
  FCancelButton.Visible := False;
  FCancelButton.OnClick := CancelButtonClick;

  FSelectionLabel := TLabel.Create(Self);
  FSelectionLabel.Name := 'InspectorSelectionState';
  FSelectionLabel.Parent := FHeader;
  FSelectionLabel.Align := alClient;
  FSelectionLabel.Layout := tlCenter;
  FSelectionLabel.AutoSize := False;
  FSelectionLabel.Caption := 'No selection';
  FSelectionLabel.BorderSpacing.Left := 8;

  with TBevel.Create(Self) do
  begin
    Parent := Self;
    Align := alTop;
    Top := FHeader.Height;
    Height := 2;
    Shape := bsBottomLine;
  end;

  FScroll := TScrollBox.Create(Self);
  FScroll.Parent := Self;
  FScroll.Align := alClient;
  FScroll.BorderStyle := bsNone;
  FScroll.AutoScroll := True;
  FScroll.HorzScrollBar.Visible := False;

  FContent := TPanel.Create(Self);
  FContent.Parent := FScroll;
  FContent.Align := alTop;
  FContent.BevelOuter := bvNone;
  FContent.AutoSize := False;
  FContent.OnResize := ContentResize;

  { Creation order is the Tab order: object properties, then "More". }
  AddField(opfText, 'Text', False, False, False);
  AddField(opfTeXText, 'TeX source', False, False, False);
  AddField(opfTextHeight, 'Height (mm)', False, False, False);
  AddField(opfTextRotation, 'Angle (°)', False, False, False);
  AddField(opfSymbolKind, 'Symbol', True, False, False);
  AddField(opfSymbolDiameter, 'Diameter (mm)', False, False, False);
  AddField(opfSymbolRotation, 'Symbol angle (°)', False, False, False);
  AddField(opfRectangleRX, 'Corner rx (mm)', False, False, False);
  AddField(opfRectangleRY, 'Corner ry (mm)', False, False, False);
  AddField(opfBitmapLink, 'Image', False, False, False);
  AddField(opfKeepAspectRatio, 'Keep aspect ratio', False, False, True);
  { 3D size captions follow the selected body kind (UpdateSolidCaptions). }
  AddField(opfSolidSize1, 'Size 1', False, False, False);
  AddField(opfSolidSize2, 'Size 2', False, False, False);
  AddField(opfSolidSize3, 'Size 3', False, False, False);
  AddField(opfSolidSize4, 'Size 4', False, False, False);
  AddField(opfSolidSegments, 'Segments', False, False, False);

  FCoordinateButton := TButton.Create(Self);
  FCoordinateButton.Name := 'InspectorCoordinatesButton';
  FCoordinateButton.Parent := FContent;
  FCoordinateButton.Caption := 'Point coordinates…';
  FCoordinateButton.OnClick := CoordinatesClick;
  FCoordinateButton.Visible := False;

  FMoreButton := TCheckBox.Create(Self);
  FMoreButton.Name := 'InspectorMoreButton';
  FMoreButton.Parent := FContent;
  FMoreButton.Caption := 'More properties';
  FMoreButton.Hint := 'Line, fill and font properties, also on the toolbars';
  FMoreButton.ShowHint := True;
  FMoreButton.OnClick := MoreClick;

  AddField(opfLineStyle, 'Line style', True, False, False);
  AddField(opfLineColor, 'Line color', True, True, False);
  AddField(opfLineWidth, 'Line width', False, False, False);
  AddField(opfBeginArrowKind, 'Start arrow', True, False, False);
  AddField(opfEndArrowKind, 'End arrow', True, False, False);
  AddField(opfArrowSizeFactor, 'Arrow size', False, False, False);
  AddField(opfHatching, 'Hatching', True, False, False);
  AddField(opfHatchColor, 'Hatch color', True, True, False);
  AddField(opfFillColor, 'Fill color', True, True, False);
  AddField(opfFontName, 'Font', False, False, False);
  AddField(opfFontStyle, 'Font style', True, False, False);
  AddField(opfHAlignment, 'Alignment', True, False, False);
  AddField(opfStarKind, 'Star shape', True, False, False);
  AddField(opfStarSizeFactor, 'Star size', False, False, False);
  AddField(opfSolidAzimuth, 'Azimuth (°)', False, False, False);
  AddField(opfSolidElevation, 'Elevation (°)', False, False, False);
  AddField(opfSolidPerspective, 'Perspective', False, False, True);

  FEmptyLabel := TLabel.Create(Self);
  FEmptyLabel.Parent := FContent;
  FEmptyLabel.AutoSize := False;
  FEmptyLabel.WordWrap := True;
  FEmptyLabel.Caption :=
    'Select an object to edit it here. New objects use the toolbar settings.';
  StyleLabel(FSelectionLabel, [fsBold], clWindowText);
  FEmptyLabel.Visible := False;

  ShowEmptySelection;
end;

procedure TObjectInspector.ContentResize(Sender: TObject);
begin
  LayoutRows;
end;

procedure TObjectInspector.LayoutRows;
var
  W, Y: Integer;
  MoreCount: Integer;
  Field: TObjectPropertyField;
  procedure PlaceRow(F: TObjectPropertyField; Show: Boolean);
  var
    Row: TPanel;
    H, EditorTop: Integer;
    Editor: TWinControl;
  begin
    Row := FWidgets[F].Row;
    Row.Visible := Show;
    if not Show then Exit;
    Editor := FWidgets[F].Editor;
    if Editor is TMemo then
    begin
      FWidgets[F].LabelText.SetBounds(6, 4, W - 12, 18);
      Editor.SetBounds(6, 24, W - 12, MemoHeight);
      H := 24 + MemoHeight + 4;
    end
    else if Editor is TCheckBox then
    begin
      Editor.SetBounds(6, 4, W - 12, 22);
      H := RowHeight;
    end
    else
    begin
      EditorTop := (RowHeight - Editor.Height) div 2;
      if EditorTop < 1 then EditorTop := 1;
      FWidgets[F].LabelText.SetBounds(6, 0, LabelWidth - 6, RowHeight);
      Editor.SetBounds(LabelWidth + 4, EditorTop, W - LabelWidth - 10,
        Editor.Height);
      H := RowHeight;
    end;
    if FWidgets[F].ErrorText.Visible then
    begin
      FWidgets[F].ErrorText.SetBounds(6, H, W - 12, 16);
      Inc(H, 18);
    end;
    Row.SetBounds(0, Y, W, H);
    Inc(Y, H);
  end;
var
  I: Integer;
begin
  if FContent = nil then Exit;
  W := ContentWidth;
  Y := 4;
  FEmptyLabel.Visible := not FHasSelection;
  if not FHasSelection then
  begin
    for Field := Low(TObjectPropertyField) to High(TObjectPropertyField) do
      FWidgets[Field].Row.Visible := False;
    FCoordinateButton.Visible := False;
    FMoreButton.Visible := False;
    FEmptyLabel.SetBounds(8, Y, W - 16, 48);
    FContent.Height := Y + 52;
    Exit;
  end;
  for I := Low(PrimaryFields) to High(PrimaryFields) do
    PlaceRow(PrimaryFields[I], FApplicable[PrimaryFields[I]]);
  if FCoordinateButton.Visible then
  begin
    FCoordinateButton.SetBounds(6, Y + 3, W - 12, 26);
    Inc(Y, 32);
  end;
  MoreCount := 0;
  for I := Low(MoreFields) to High(MoreFields) do
    if FApplicable[MoreFields[I]] then Inc(MoreCount);
  FMoreButton.Visible := MoreCount > 0;
  if FMoreButton.Visible then
  begin
    FMoreButton.Checked := FShowMore;
    FMoreButton.SetBounds(6, Y + 4, W - 12, 22);
    Inc(Y, 30);
  end;
  for I := Low(MoreFields) to High(MoreFields) do
    PlaceRow(MoreFields[I], FShowMore and FApplicable[MoreFields[I]]);
  FContent.Height := Y + 6;
end;

procedure TObjectInspector.AddField(Field: TObjectPropertyField;
  const Caption: string; IsCombo, IsColor, IsCheck: Boolean);
var
  Row: TPanel;
  LabelControl: TLabel;
  Control: TWinControl;
  IsText: Boolean;
begin
  IsText := Field in [opfText, opfTeXText];
  Row := TPanel.Create(Self);
  Row.Parent := FContent;
  Row.BevelOuter := bvNone;
  Row.Caption := '';
  Row.Visible := False;
  Row.SetBounds(0, 0, ContentWidth, RowHeight);

  LabelControl := TLabel.Create(Self);
  LabelControl.Parent := Row;
  LabelControl.AutoSize := False;
  LabelControl.Layout := tlCenter;
  LabelControl.Caption := Caption;

  FWidgets[Field].HintText := nil;

  FWidgets[Field].ErrorText := TLabel.Create(Self);
  FWidgets[Field].ErrorText.Name := 'Inspector' + FieldName(Field) + 'Error';
  FWidgets[Field].ErrorText.Parent := Row;
  FWidgets[Field].ErrorText.AutoSize := False;
  StyleLabel(FWidgets[Field].ErrorText, [], clRed);
  FWidgets[Field].ErrorText.Visible := False;

  if IsCheck then
  begin
    Control := TCheckBox.Create(Self);
    (Control as TCheckBox).Caption := Caption;
    LabelControl.Visible := False;
  end
  else if IsCombo then
  begin
    Control := TComboBox.Create(Self);
    with Control as TComboBox do
    begin
      if IsColor then
        Style := csDropDown
      else if Field in [opfBeginArrowKind, opfEndArrowKind] then
      begin
        Style := csOwnerDrawFixed;
        OnDrawItem := DrawArrowComboItem;
      end
      else if Field = opfStarKind then
      begin
        Style := csOwnerDrawFixed;
        OnDrawItem := DrawStarComboItem;
      end
      else
        Style := csDropDownList;
      PopulateCombo(Field, Control as TComboBox);
      DropDownCount := 12;
    end;
  end
  else if IsText then
  begin
    Control := TCompositionAwareMemo.Create(Self);
    (Control as TMemo).ScrollBars := ssAutoVertical;
    (Control as TMemo).WordWrap := True;
    (Control as TMemo).OnChange := FieldChanged;
    (Control as TCompositionAwareMemo).OnCompositionChanged :=
      CompositionChanged;
  end
  else
  begin
    Control := TCompositionAwareEdit.Create(Self);
    (Control as TEdit).OnChange := FieldChanged;
    (Control as TCompositionAwareEdit).OnCompositionChanged :=
      CompositionChanged;
  end;
  // Naming an empty control would otherwise copy the name into its text.
  Control.ControlStyle := Control.ControlStyle - [csSetCaption];
  Control.Name := 'Inspector' + FieldName(Field) + 'Control';
  Control.Hint := Caption;
  Control.Parent := Row;
  Control.Anchors := [akTop, akLeft, akRight];
  Control.Tag := Ord(Field);
  Control.TabOrder := 0;
  Control.OnEnter := FieldEnter;
  Control.OnKeyDown := FieldKeyDown;
  Control.OnKeyUp := FieldKeyUp;
  if Control is TCompositionAwareEdit then
    (Control as TCompositionAwareEdit).OnMouseUp := FieldMouseUp
  else if Control is TCompositionAwareMemo then
    (Control as TCompositionAwareMemo).OnMouseUp := FieldMouseUp;
  LabelControl.FocusControl := Control;
  if Control is TComboBox then
    (Control as TComboBox).OnChange := FieldChanged
  else if Control is TCheckBox then
    (Control as TCheckBox).OnChange := FieldChanged;

  FWidgets[Field].Row := Row;
  FWidgets[Field].LabelText := LabelControl;
  FWidgets[Field].Editor := Control;
  FApplicable[Field] := False;
end;

procedure TObjectInspector.SetShowMore(Value: Boolean);
begin
  if FShowMore = Value then Exit;
  FShowMore := Value;
  LayoutRows;
  if Assigned(FOnShowMoreChanged) then FOnShowMoreChanged(Self);
end;

procedure TObjectInspector.MoreClick(Sender: TObject);
begin
  SetShowMore(FMoreButton.Checked);
end;

procedure TObjectInspector.CloseClick(Sender: TObject);
begin
  if Assigned(FOnCloseRequest) then FOnCloseRequest(Self);
end;

function TObjectInspector.FieldShown(Field: TObjectPropertyField): Boolean;
begin
  Result := FHasSelection and FApplicable[Field] and
    (FShowMore or not IsMoreField(Field));
end;

procedure TObjectInspector.SetFieldState(Field: TObjectPropertyField;
  Mixed: Boolean; const Hint: string);
var
  Editor: TWinControl;
begin
  if Mixed then StyleLabel(FWidgets[Field].LabelText, [fsItalic], clWindowText)
  else StyleLabel(FWidgets[Field].LabelText, [], clWindowText);
  Editor := FWidgets[Field].Editor;
  if Editor is TEdit then (Editor as TEdit).TextHint := Hint
  else if (Editor is TComboBox) and
    ((Editor as TComboBox).Style = csDropDown) then
    (Editor as TComboBox).TextHint := Hint;
  if Mixed then Editor.Hint := FWidgets[Field].LabelText.Caption + ' (mixed)'
  else Editor.Hint := FWidgets[Field].LabelText.Caption;
end;

function TObjectInspector.SelectionTitle(SelectedCount: Integer): string;
var
  Obj: TGraphicObject;
  Name: string;
  I: Integer;
begin
  if SelectedCount <> 1 then Exit(Format('%d objects', [SelectedCount]));
  Obj := nil;
  if (FDrawing <> nil) and (Length(FTargetIDs) = 1) then
    Obj := FDrawing.GetObject(FTargetIDs[0]);
  if Obj = nil then Exit('1 object');
  { Use the Insert menu's words where the class name differs. }
  if Obj is TClosedSmoothPath2D then Exit('Closed curve');
  if Obj is TSmoothPath2D then Exit('Curve');
  if Obj is TClosedBezierPath2D then Exit('Closed Bezier path');
  if Obj is TBezierPath2D then Exit('Bezier path');
  if Obj is TContainer2D then Exit('Group');
  if Obj is TSolid3D then
  begin
    if TSolid3D(Obj).BodyCount = 1 then
      Exit(SolidKindNames[TSolid3D(Obj).Bodies[0].Kind]);
    Exit('3D scene');
  end;
  Name := Obj.ClassName;
  if (Name <> '') and (Name[1] = 'T') then Delete(Name, 1, 1);
  if Copy(Name, Length(Name) - 1, 2) = '2D' then
    SetLength(Name, Length(Name) - 2);
  Result := '';
  for I := 1 to Length(Name) do
    if (I > 1) and (Name[I] in ['A'..'Z']) then
      Result := Result + ' ' + LowerCase(Name[I])
    else Result := Result + Name[I];
end;

procedure TObjectInspector.ShowEmptySelection;
var
  Field: TObjectPropertyField;
begin
  FHasSelection := False;
  for Field := Low(TObjectPropertyField) to High(TObjectPropertyField) do
  begin
    FApplicable[Field] := False;
    SetFieldError(Field, '');
  end;
  FTitle := 'No selection';
  FSelectionLabel.Caption := FTitle;
  LayoutRows;
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
          Style := FontStylesFromMask(I);
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
    opfSymbolRotation, opfRectangleRX, opfRectangleRY, opfSolidSize1,
    opfSolidSize2, opfSolidSize3, opfSolidSize4, opfSolidSegments,
    opfSolidAzimuth, opfSolidElevation];
end;

function TObjectInspector.NumericRange(
  Field: TObjectPropertyField): TNumericEditRange;
begin
  case Field of
    opfTextHeight: Result := nerPositive;
    opfTextRotation, opfSymbolRotation, opfSolidSize2, opfSolidSize4,
    opfSolidAzimuth, opfSolidElevation: Result := nerFinite;
    opfSolidSize1, opfSolidSize3, opfSolidSegments: Result := nerPositive;
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
    opfText: Result := UTF8BytesToLCLText(
      DecodeTpXTextForEdit(AnsiString(VarToStr(Value))));
    opfTeXText, opfBitmapLink:
      Result := UTF8BytesToLCLText(TEditUtf8Text(VarToStr(Value)));
    opfFontName:
      if VarToStr(Value) = ' ' then Result := ''
      else Result := UTF8BytesToLCLText(TEditUtf8Text(VarToStr(Value)));
    opfLineColor, opfHatchColor, opfFillColor:
      if TColor(Integer(Value)) = clDefault then Result := 'Default'
      else Result := ColorToHtml(TColor(Integer(Value)));
    opfTextRotation, opfSymbolRotation:
      begin
        Number := RadToDeg(Double(Value));
        Result := InspectorNumberText(Number);
      end;
    opfLineWidth, opfArrowSizeFactor, opfTextHeight,
    opfStarSizeFactor, opfSymbolDiameter, opfRectangleRX,
    opfRectangleRY:
      Result := InspectorNumberText(TRealType(Value));
    opfSolidSize1, opfSolidSize2, opfSolidSize3, opfSolidSize4,
    opfSolidAzimuth, opfSolidElevation:
      Result := InspectorNumberText(Double(Value));
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

procedure TObjectInspector.ClearFieldEditor(Field: TObjectPropertyField);
var
  Editor: TWinControl;
begin
  Editor := FWidgets[Field].Editor;
  if Editor is TEdit then (Editor as TEdit).Clear
  else if Editor is TMemo then (Editor as TMemo).Clear
  else if Editor is TComboBox then
  begin
    (Editor as TComboBox).ItemIndex := -1;
    (Editor as TComboBox).Text := '';
  end
  else if Editor is TCheckBox then
    (Editor as TCheckBox).Checked := False;
end;

procedure TObjectInspector.UpdateSolidCaptions;
var
  Field: TObjectPropertyField;
  Caption, Other: string;
  I: Integer;
  Obj: TGraphicObject;
begin
  FSolidAmbiguous := [];
  for Field := opfSolidSize1 to opfSolidSize4 do
  begin
    if Field in [opfSolidAzimuth..opfSolidPerspective] then Continue;
    { The kind's own word when every selected body agrees. }
    Caption := '';
    for I := 0 to High(FTargetIDs) do
    begin
      Obj := nil;
      if FDrawing <> nil then Obj := FDrawing.GetObject(FTargetIDs[I]);
      if not ((Obj is TObject2D) and
        SolidFieldCaption(TObject2D(Obj), Field, Other)) then Continue;
      if Caption = '' then Caption := Other
      else if Caption <> Other then
      begin
        Include(FSolidAmbiguous, Field);
        Break;
      end;
    end;
    if (Caption = '') or (Field in FSolidAmbiguous) then Continue;
    FWidgets[Field].LabelText.Caption := Caption;
    FWidgets[Field].Editor.Hint := Caption;
  end;
end;

procedure TObjectInspector.RefreshField(Field: TObjectPropertyField);
var
  Value: TPropertyValue;
  Editor: TWinControl;
begin
  Editor := FWidgets[Field].Editor;
  if Editor = nil then Exit;
  if Assigned(FOnShouldSkipFieldRefresh) and
    FOnShouldSkipFieldRefresh(Self, Field, FTargetIDs) then Exit;
  if (FHasSessionField) and (FSessionField = Field) and
    ((FSession <> nil) or Editor.Focused) then Exit;
  Value := ReadObjectProperty(FDrawing, FTargetIDs, Field);
  if Field in FSolidAmbiguous then Value.State := pvsUnavailable;
  FApplicable[Field] := Value.State <> pvsUnavailable;
  case Value.State of
    pvsUnavailable:
      begin
        SetFieldState(Field, False, '');
        ClearFieldEditor(Field);
      end;
    pvsMixed:
      begin
        Editor.Enabled := True;
        ClearFieldEditor(Field);
        SetFieldState(Field, True, 'mixed');
      end;
    pvsUniform:
      begin
        Editor.Enabled := True;
        SetFieldValue(Field, Value.Value);
        if (Field = opfFontName) and (VarToStr(Value.Value) = ' ') then
          SetFieldState(Field, False, 'inherited')
        else
          SetFieldState(Field, False, '');
      end;
  end;
  SetFieldError(Field, '');
  CaptureStableField(Field);
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
    if Length(FTargetIDs) > 0 then
    begin
      FHasSelection := True;
      FCoordinateButton.Visible := HasCoordinateTable;
      FTitle := SelectionTitle(SelectedCount);
      FSelectionLabel.Caption := FTitle;
      UpdateSolidCaptions;
      for Field := Low(TObjectPropertyField) to High(TObjectPropertyField) do
        RefreshField(Field);
      LayoutRows;
    end
    else
      ShowEmptySelection;
  finally
    Dec(FRefreshing);
  end;
end;

function TObjectInspector.AdoptRecoverySession(
  Session: TObjectEditSession): Boolean;
var
  Drafts: TRawEditDraftArray;
  Targets: TEditTargetIDs;
  Field: TObjectPropertyField;
  I: Integer;
  Editor: TWinControl;
  DraftText: string;
  Patch: TObjectPropertyPatch;
  ErrorText: string;
  Value: TPropertyValue;
  ItemIndex: Integer;
begin
  Result := False;
  if (Session = nil) or not Session.IsActive or (FDrawing = nil) then Exit;
  if (FSession <> nil) and FSession.IsActive then Exit;
  if FSession <> nil then ReleaseSession;
  Drafts := Session.CopyRawDrafts;
  if Length(Drafts) = 0 then Exit;
  Targets := Session.CopyTargetIDs;
  Field := Drafts[0].Field;
  Value := ReadObjectProperty(FDrawing, Targets, Field);
  if (Value.State = pvsUnavailable) and (Length(Targets) > 0) then Exit;

  FSession := Session;
  FSession.Presentation := espInspector;
  FSessionField := Field;
  FHasSessionField := True;
  FTargetIDs := Copy(Targets);
  FHasSelection := Length(FTargetIDs) > 0;
  FCoordinateButton.Visible := False;
  FCancelButton.Visible := True;
  FTitle := SelectionTitle(Length(FTargetIDs));
  FSelectionLabel.Caption := 'Recovered edit · ' + FTitle;

  FRefreshing := FRefreshing + 1;
  try
    for I := Ord(Low(TObjectPropertyField)) to
      Ord(High(TObjectPropertyField)) do
      RefreshField(TObjectPropertyField(I));
    if IsMoreField(Drafts[0].Field) then FShowMore := True;
    LayoutRows;
    for I := 0 to High(Drafts) do
    begin
      Field := Drafts[I].Field;
      Editor := FWidgets[Field].Editor;
      DraftText := UTF8BytesToLCLText(Drafts[I].Text);
      if Editor is TEdit then
        (Editor as TEdit).Text := DraftText
      else if Editor is TMemo then
        (Editor as TMemo).Text := DraftText
      else if Editor is TComboBox then
      begin
        if IsColorField(Field) then
          (Editor as TComboBox).Text := DraftText
        else
        begin
          ItemIndex := (Editor as TComboBox).Items.IndexOf(DraftText);
          (Editor as TComboBox).ItemIndex := ItemIndex;
        end;
      end
      else if Editor is TCheckBox then
        (Editor as TCheckBox).Checked := SameText(DraftText, 'True') or
          (DraftText = '1');
    end;
  finally
    Dec(FRefreshing);
  end;

  Field := Drafts[0].Field;
  if not BuildPatch(Field, Patch, ErrorText) then
    SetFieldError(Field, ErrorText)
  else
    SetFieldError(Field, '');
  FocusField(Field);
  Result := True;
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
  FSelectionLabel.Caption := FTitle;
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
      ShowEmptySelection
    else
    begin
      for Field := Low(TObjectPropertyField) to High(TObjectPropertyField) do
        RefreshField(Field);
      LayoutRows;
    end;
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
      opfLineWidth: Patch.LineWidth := Number;
      opfArrowSizeFactor: Patch.ArrowSizeFactor := Number;
      opfTextHeight: Patch.TextHeight := Number;
      opfTextRotation: Patch.TextRotation := Number;
      opfStarSizeFactor: Patch.StarSizeFactor := Number;
      opfSymbolDiameter: Patch.SymbolDiameter := Number;
      opfSymbolRotation: Patch.SymbolRotation := Number;
      opfRectangleRX: Patch.RectangleRX := Number;
      opfRectangleRY: Patch.RectangleRY := Number;
      opfSolidSize1: Patch.SolidSize1 := Number;
      opfSolidSize2: Patch.SolidSize2 := Number;
      opfSolidSize3: Patch.SolidSize3 := Number;
      opfSolidSize4: Patch.SolidSize4 := Number;
      opfSolidSegments: Patch.SolidSegments := Round(Number);
      opfSolidAzimuth: Patch.SolidAzimuth := Number;
      opfSolidElevation: Patch.SolidElevation := Number;
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
      opfFontStyle: Patch.FontStyle := FontStylesFromMask(Combo.ItemIndex);
      opfHAlignment: Patch.HAlignment := THAlignment(Combo.ItemIndex);
      opfStarKind: Patch.StarKind := TStarKind(Combo.ItemIndex);
      opfSymbolKind: Patch.SymbolKind := TSymbolKind(Combo.ItemIndex);
    end;
    Exit(True);
  end;
  if Editor is TCheckBox then
  begin
    Check := Editor as TCheckBox;
    if Field = opfSolidPerspective then Patch.SolidPerspective := Check.Checked
    else Patch.KeepAspectRatio := Check.Checked;
    Exit(True);
  end;
  if Editor is TCompositionAwareMemo then
    Raw := (Editor as TCompositionAwareMemo).EditText
  else if Editor is TMemo then Raw := (Editor as TMemo).Text
  else if Editor is TEdit then Raw := (Editor as TEdit).Text
  else Raw := '';
  case Field of
    opfText: Patch.Text := EncodeEditTextForTpX(
      LCLTextToUTF8Bytes(Raw));
    opfTeXText: Patch.TeXText := LCLTextToUTF8Bytes(Raw);
    opfFontName:
      if Raw = '' then Patch.FontName := TEditUtf8Text(' ')
      else Patch.FontName := LCLTextToUTF8Bytes(Raw);
    opfBitmapLink: Patch.BitmapLink := LCLTextToUTF8Bytes(Raw);
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
        FWidgets[FSessionField].Editor.Hint +
        ' before editing another field';
      Exit;
    end;
  end;
  FSession := TObjectEditSession.Create(FDrawing, CurrentEpoch,
    FTargetIDs);
  FSession.Presentation := espInspector;
  FSessionField := Field;
  FHasSessionField := True;
  FCancelButton.Visible := True;
  if Assigned(FOnSessionChanged) and
    not FOnSessionChanged(Self, FSession, True) then
  begin
    FSession.Cancel(CurrentEpoch);
    FSession.Free;
    FSession := nil;
    FHasSessionField := False;
    FCancelButton.Visible := False;
    Exit;
  end;
  Result := True;
end;

function TObjectInspector.CommitSession(out ErrorText: string): Boolean;
var
  Field: TObjectPropertyField;
begin
  ErrorText := '';
  if FSession = nil then Exit(True);
  RefreshCompositionState;
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

function TObjectInspector.EditorCompositionPending(
  Editor: TWinControl): Boolean;
begin
  if Editor is TCompositionAwareEdit then
    Exit((Editor as TCompositionAwareEdit).CompositionPending);
  if Editor is TCompositionAwareMemo then
    Exit((Editor as TCompositionAwareMemo).CompositionPending);
  Result := False;
end;

procedure TObjectInspector.RefreshCompositionState;
var
  Field: TObjectPropertyField;
  Editor: TWinControl;
begin
  for Field := Low(TObjectPropertyField) to High(TObjectPropertyField) do
  begin
    Editor := FWidgets[Field].Editor;
    if Editor is TCompositionAwareEdit then
      (Editor as TCompositionAwareEdit).RefreshCompositionState
    else if Editor is TCompositionAwareMemo then
      (Editor as TCompositionAwareMemo).RefreshCompositionState;
  end;
end;

procedure TObjectInspector.SetFieldError(Field: TObjectPropertyField;
  const ErrorText: string);
begin
  if (FWidgets[Field].ErrorText.Caption = ErrorText) and
    (FWidgets[Field].ErrorText.Visible = (ErrorText <> '')) then Exit;
  FWidgets[Field].ErrorText.Caption := ErrorText;
  FWidgets[Field].ErrorText.Visible := ErrorText <> '';
  if FWidgets[Field].Row.Visible then LayoutRows;
end;

procedure TObjectInspector.RejectBlockedField(Field: TObjectPropertyField);
var
  Value: TPropertyValue;
  ActiveField: TObjectPropertyField;
begin
  Value := ReadObjectProperty(FDrawing, FTargetIDs, Field);
  FRefreshing := FRefreshing + 1;
  try
    if Value.State = pvsUniform then SetFieldValue(Field, Value.Value)
    else ClearFieldEditor(Field);
  finally
    Dec(FRefreshing);
  end;
  SetFieldError(Field, '');
  if not FHasSessionField then Exit;
  ActiveField := FSessionField;
  if FWidgets[ActiveField].Editor.CanFocus then
    FWidgets[ActiveField].Editor.SetFocus;
  FSelectionLabel.Caption := 'Finish or cancel ' +
    FWidgets[ActiveField].Editor.Hint + ' before editing another field';
  if (FSession <> nil) and (FSession.LastError <> '') then
    SetFieldError(ActiveField, FSession.LastError);
end;

procedure TObjectInspector.FieldEnter(Sender: TObject);
var
  Field: TObjectPropertyField;
  ErrorText: string;
  ParentForm: TCustomForm;
begin
  if FRefreshing > 0 then Exit;
  Field := FieldFromControl(Sender);
  if (FSession <> nil) and FSession.IsActive and
    (Field <> FSessionField) then
  begin
    if FSession.HasRawDraftPending or FSession.CompositionPending then
    begin
      RejectBlockedField(Field);
      Exit;
    end;
    if (FWidgets[Field].Editor is TCustomEdit) or
      (FWidgets[Field].Editor is TCheckBox) then
      if not CommitSession(ErrorText) then
      begin
        RejectBlockedField(Field);
        Exit;
      end;
  end;
  if Assigned(FOnBeforeFieldInput) and not FRestoringFieldInput then
  begin
    if not FOnBeforeFieldInput(Self, FSession) then
    begin
      FSelectionLabel.Caption :=
        'Finish or cancel the active edit before editing a property';
      Exit;
    end;
    ParentForm := GetParentForm(Self);
    if (ParentForm <> nil) and
      (ParentForm.ActiveControl <> FWidgets[Field].Editor) and
      FWidgets[Field].Editor.CanFocus then
    begin
      FRestoringFieldInput := True;
      try
        FWidgets[Field].Editor.SetFocus;
      finally
        FRestoringFieldInput := False;
      end;
    end;
  end;
  FScroll.ScrollInView(FWidgets[Field].Editor);
  FSelectionLabel.Caption := FTitle;
end;

procedure TObjectInspector.FieldChanged(Sender: TObject);
var
  Field: TObjectPropertyField;
  Patch: TObjectPropertyPatch;
  CurrentValue: TPropertyValue;
  Combo: TComboBox;
  ErrorText: string;
  Raw: string;
  Composing: Boolean;
  ExpectedIndex: Integer;
  ComboIndex: Integer;
  ComboText: string;
  RestoreComboState: Boolean;
begin
  if FRefreshing > 0 then Exit;
  Field := FieldFromControl(Sender);
  RestoreComboState := False;
  if FWidgets[Field].Editor is TComboBox then
  begin
    Combo := FWidgets[Field].Editor as TComboBox;
    CurrentValue := ReadObjectProperty(FDrawing, FTargetIDs, Field);
    if CurrentValue.State = pvsMixed then
    begin
      if (Combo.ItemIndex = -1) and (Combo.Text = '') then Exit;
    end
    else if CurrentValue.State = pvsUniform then
    begin
      if IsColorField(Field) then
      begin
        if Combo.Text = DisplayValue(Field, CurrentValue.Value) then Exit;
      end
      else
      begin
        ExpectedIndex := Integer(CurrentValue.Value);
        if ((Field = opfBeginArrowKind) or (Field = opfEndArrowKind)) and
          (ExpectedIndex >= Combo.Items.Count) then ExpectedIndex := -1;
        if Combo.ItemIndex = ExpectedIndex then Exit;
      end;
    end;
    RestoreComboState := (FSession <> nil) and FSession.IsActive and
      FHasSessionField and (FSessionField <> Field);
    if RestoreComboState then
    begin
      { Committing the previous field refreshes controls before this patch. }
      ComboIndex := Combo.ItemIndex;
      ComboText := Combo.Text;
    end;
  end;
  if not StartFieldSession(Field) then
  begin
    if (FSession <> nil) and FSession.IsActive and
      (FSessionField <> Field) then
      RejectBlockedField(Field)
    else if FHasSessionField and (FSessionField <> Field) then
      SetFieldError(Field, 'Finish or cancel the active edit first');
    Exit;
  end;
  if RestoreComboState then
  begin
    Combo := FWidgets[Field].Editor as TComboBox;
    Inc(FRefreshing);
    try
      Combo.ItemIndex := ComboIndex;
      if Combo.Style.HasEditBox then
        Combo.Text := ComboText;
    finally
      Dec(FRefreshing);
    end;
  end;
  Composing := False;
  if FWidgets[Field].Editor is TCompositionAwareEdit then
  begin
    if (FWidgets[Field].Editor as TCompositionAwareEdit).
      SampleCompositionState then
    begin
      FSession.SetCompositionPending(True);
      Composing := True;
    end;
    if FSession.CompositionPending then
      Composing := True;
  end
  else if FWidgets[Field].Editor is TCompositionAwareMemo then
  begin
    if (FWidgets[Field].Editor as TCompositionAwareMemo).
      SampleCompositionState then
    begin
      FSession.SetCompositionPending(True);
      Composing := True;
    end;
    if FSession.CompositionPending then
      Composing := True;
  end;
  if Composing then Include(FCompositionTextChanged, Field)
  else CaptureStableField(Field);
  if FWidgets[Field].Editor is TEdit then
    Raw := (FWidgets[Field].Editor as TEdit).Text
  else if FWidgets[Field].Editor is TCompositionAwareMemo then
    Raw := (FWidgets[Field].Editor as TCompositionAwareMemo).EditText
  else if FWidgets[Field].Editor is TMemo then
    Raw := (FWidgets[Field].Editor as TMemo).Text
  else if FWidgets[Field].Editor is TComboBox then
    Raw := (FWidgets[Field].Editor as TComboBox).Text
  else if FWidgets[Field].Editor is TCheckBox then
    Raw := BoolToStr((FWidgets[Field].Editor as TCheckBox).Checked, True)
  else
    Raw := '';
  if not FSession.TrySetRawDraft(Field, LCLTextToUTF8Bytes(Raw),
    ErrorText) then
  begin
    FSession.SetRawDraftPendingForField(Field, True);
    SetFieldError(Field, ErrorText);
    Exit;
  end;
  if EditorCompositionPending(FWidgets[Field].Editor) or
    FSession.CompositionPending then
  begin
    SetFieldError(Field, '');
    if FWidgets[Field].Editor is TCompositionAwareEdit then
      (FWidgets[Field].Editor as TCompositionAwareEdit).
        RefreshCompositionState
    else if FWidgets[Field].Editor is TCompositionAwareMemo then
      (FWidgets[Field].Editor as TCompositionAwareMemo).
        RefreshCompositionState;
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
  FSelectionLabel.Caption := 'Editing ' + FWidgets[Field].LabelText.Caption;
end;

procedure TObjectInspector.CompositionChanged(Sender: TObject;
  Composing: Boolean);
var
  Field: TObjectPropertyField;
  HadTextChange: Boolean;
  CompositionCommitted: Boolean;
  CancelUncommitted: Boolean;
  Value: TPropertyValue;
begin
  Field := FieldFromControl(Sender);
  if Composing then
  begin
    if not StartFieldSession(Field) then
    begin
      if (FSession <> nil) and FSession.IsActive and
        (FSessionField <> Field) then
        RejectBlockedField(Field)
      else
        SetFieldError(Field, 'Finish or cancel the active property edit first');
      Exit;
    end;
    Exclude(FCompositionTextChanged, Field);
    Include(FCompositionBaselineValid, Field);
    if Field in FStableFieldTextValid then
    begin
      FCompositionBaselineText[Field] := FStableFieldText[Field];
      FCompositionBaselineSelStart[Field] := FStableFieldSelStart[Field];
      FCompositionBaselineSelLength[Field] := FStableFieldSelLength[Field];
    end
    else
    begin
      FCompositionBaselineText[Field] := '';
      FCompositionBaselineSelStart[Field] := 0;
      FCompositionBaselineSelLength[Field] := 0;
      if FDrawing <> nil then
      begin
        Value := ReadObjectProperty(FDrawing, FTargetIDs, Field);
        if Value.State = pvsUniform then
          FCompositionBaselineText[Field] :=
            DisplayValue(Field, Value.Value);
      end;
    end;
    if FSession <> nil then FSession.SetCompositionPending(True);
    Exit;
  end;
  if (FSession = nil) or not FSession.IsActive or
    (FSessionField <> Field) then Exit;
  HadTextChange := Field in FCompositionTextChanged;
  Exclude(FCompositionTextChanged, Field);
  if FWidgets[Field].Editor is TCompositionAwareEdit then
    CompositionCommitted :=
      (FWidgets[Field].Editor as TCompositionAwareEdit).
        CompositionCommitted
  else
    CompositionCommitted :=
      (FWidgets[Field].Editor as TCompositionAwareMemo).
        CompositionCommitted;
  {$IFDEF LCLCOCOA}
  CancelUncommitted := not CompositionCommitted;
  {$ELSE}
  CancelUncommitted := not CompositionCommitted and not HadTextChange and
    (EditorText(Field) = FCompositionBaselineText[Field]);
  {$ENDIF}
  FSession.SetCompositionPending(False);
  if not CancelUncommitted then
  begin
    Exclude(FCompositionBaselineValid, Field);
    FieldChanged(Sender);
  end
  else
  begin
    RestoreCompositionBaseline(Field);
    if HadTextChange then FieldChanged(Sender);
  end;
end;

function TObjectInspector.EditorText(Field: TObjectPropertyField): string;
var
  Editor: TWinControl;
begin
  Editor := FWidgets[Field].Editor;
  if Editor is TCustomEdit then Result := (Editor as TCustomEdit).Text
  else Result := '';
end;

procedure TObjectInspector.CaptureStableField(Field: TObjectPropertyField);
var
  Editor: TWinControl;
begin
  Editor := FWidgets[Field].Editor;
  if not (Editor is TCustomEdit) then Exit;
  FStableFieldText[Field] := (Editor as TCustomEdit).Text;
  FStableFieldSelStart[Field] := (Editor as TCustomEdit).SelStart;
  FStableFieldSelLength[Field] := (Editor as TCustomEdit).SelLength;
  Include(FStableFieldTextValid, Field);
end;

procedure TObjectInspector.CaptureStableSelection(
  Field: TObjectPropertyField);
var
  Editor: TWinControl;
begin
  Editor := FWidgets[Field].Editor;
  if Editor is TCompositionAwareEdit then
    (Editor as TCompositionAwareEdit).RefreshCompositionState
  else if Editor is TCompositionAwareMemo then
    (Editor as TCompositionAwareMemo).RefreshCompositionState;
  if not EditorCompositionPending(Editor) and
    ((FSession = nil) or not FSession.CompositionPending) then
    CaptureStableField(Field);
end;

procedure TObjectInspector.RestoreCompositionBaseline(
  Field: TObjectPropertyField);
begin
  if not (Field in FCompositionBaselineValid) then Exit;
  Exclude(FCompositionBaselineValid, Field);
  Inc(FRefreshing);
  try
    if FWidgets[Field].Editor is TEdit then
    begin
      (FWidgets[Field].Editor as TEdit).Text :=
        FCompositionBaselineText[Field];
      (FWidgets[Field].Editor as TEdit).SelStart :=
        FCompositionBaselineSelStart[Field];
      (FWidgets[Field].Editor as TEdit).SelLength :=
        FCompositionBaselineSelLength[Field];
    end
    else if FWidgets[Field].Editor is TCompositionAwareMemo then
    begin
      (FWidgets[Field].Editor as TCompositionAwareMemo).Text :=
        FCompositionBaselineText[Field];
      (FWidgets[Field].Editor as TCompositionAwareMemo).SelStart :=
        FCompositionBaselineSelStart[Field];
      (FWidgets[Field].Editor as TCompositionAwareMemo).SelLength :=
        FCompositionBaselineSelLength[Field];
    end;
  finally
    Dec(FRefreshing);
  end;
end;

procedure TObjectInspector.FieldKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  ErrorText: string;
  Field: TObjectPropertyField;
  ParentForm: TCustomForm;
  OriginControl: TWinControl;
begin
  Field := FieldFromControl(Sender);
  OriginControl := FWidgets[Field].Editor;
  CaptureStableSelection(Field);
  if EditorCompositionPending(FWidgets[Field].Editor) or
    ((FSession <> nil) and FSession.CompositionPending) then
  begin
    if Key = VK_TAB then Key := 0;
    Exit;
  end;
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
  end
  else if Key = VK_TAB then
  begin
    if FHasSessionField and (FSessionField = Field) and
      not CommitSession(ErrorText) then
    begin
      Key := 0;
      Exit;
    end;
    OriginControl.PerformTab(not (ssShift in Shift));
    ParentForm := GetParentForm(Self);
    if (ParentForm <> nil) and
      (ParentForm.ActiveControl <> nil) and
      (ParentForm.ActiveControl <> OriginControl) then
      Key := 0;
  end;
end;

procedure TObjectInspector.FieldKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if FRefreshing > 0 then Exit;
  CaptureStableSelection(FieldFromControl(Sender));
end;

procedure TObjectInspector.FieldMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FRefreshing > 0 then Exit;
  CaptureStableSelection(FieldFromControl(Sender));
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
begin
  if IsMoreField(Field) and not FShowMore then SetShowMore(True);
  LayoutRows;
  if FWidgets[Field].Editor.CanFocus then
  begin
    FScroll.ScrollInView(FWidgets[Field].Editor);
    FWidgets[Field].Editor.SetFocus;
  end;
end;

procedure TObjectInspector.FocusObject(Obj: TObject2D);
var
  IDs: TEditTargetIDs;
  Selected: TGraphicObject;
  Count: Integer;
  ErrorText: string;
  Field: TObjectPropertyField;
begin
  if (Obj = nil) or (FDrawing = nil) then Exit;
  if not PrepareForSelectionChange(ErrorText) then
  begin
    FSelectionLabel.Caption := ErrorText;
    Exit;
  end;
  if FDrawing.SelectedObjects.Find(Obj.ID) = nil then
  begin
    FDrawing.SelectionClear;
    FDrawing.SelectionAdd(Obj);
  end;
  SetLength(IDs, FDrawing.SelectedObjects.Count);
  Count := 0;
  Selected := FDrawing.SelectedObjects.FirstObj;
  while Selected <> nil do
  begin
    IDs[Count] := Selected.ID;
    Inc(Count);
    Selected := FDrawing.SelectedObjects.NextObj;
  end;
  RefreshSelection(IDs, Count, (Count = 1) and (Obj is TPrimitive2D));
  if Obj is TText2D then Field := opfText
  else if Obj is TStar2D then Field := opfStarKind
  else if Obj is TSymbol2D then Field := opfSymbolKind
  else if Obj is TBitmap2D then Field := opfBitmapLink
  else if Obj is TRectangle2D then Field := opfRectangleRX
  else if FShowMore then Field := opfLineStyle
  else Exit;
  FocusField(Field);
end;



end.
