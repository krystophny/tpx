unit PropertyEdits;

{$mode delphi}{$H+}

interface

uses Classes, Graphics, Geometry, Devices, Drawings, GObjects,
  PropertyEditCore;

type
  { LCL edit controls expose UTF-8 bytes in string values on every widgetset.
    Keep that byte contract explicit at the shared editing boundary; in
    particular, do not run these values through UTF8Encode on Windows. }
  TEditUtf8Text = type UTF8String;

  { Text is the exact stored TpX value. Use the conversion helpers below when
    presenting it in an editor; this keeps rollback and TeX text byte-exact. }
  TObjectPropertyField = (opfLineStyle, opfLineColor, opfLineWidth,
    opfHatching, opfHatchColor, opfFillColor, opfBeginArrowKind,
    opfEndArrowKind, opfArrowSizeFactor, opfText, opfTeXText,
    opfTextHeight, opfFontName, opfFontStyle, opfHAlignment,
    opfTextRotation, opfStarKind, opfStarSizeFactor, opfSymbolKind,
    opfSymbolDiameter, opfSymbolRotation, opfRectangleRX,
    opfRectangleRY, opfBitmapLink, opfKeepAspectRatio);
  TObjectPropertyFields = set of TObjectPropertyField;
  TEditTargetIDs = array of Integer;

  TObjectPropertyPatch = record
    Fields: TObjectPropertyFields;
    LineStyle: TLineStyle;
    LineColor: TColor;
    LineWidth: TRealType;
    Hatching: THatching;
    HatchColor: TColor;
    FillColor: TColor;
    BeginArrowKind: TArrowKind;
    EndArrowKind: TArrowKind;
    ArrowSizeFactor: TRealType;
    Text: AnsiString;
    TeXText: TEditUtf8Text;
    TextHeight: TRealType;
    FontName: TEditUtf8Text;
    FontStyle: TFontStyles;
    HAlignment: THAlignment;
    TextRotation: TRealType;
    StarKind: TStarKind;
    StarSizeFactor: TRealType;
    SymbolKind: TSymbolKind;
    SymbolDiameter: TRealType;
    SymbolRotation: TRealType;
    RectangleRX: TRealType;
    RectangleRY: TRealType;
    BitmapLink: TEditUtf8Text;
    KeepAspectRatio: Boolean;
  end;

function ExpandObjectEditTargetIDs(Drawing: TDrawing2D;
  const TargetIDs: array of Integer; out ErrorText: string): TEditTargetIDs;
function AssignObjectPropertyPatchValue(var Patch: TObjectPropertyPatch;
  Field: TObjectPropertyField; const Value: Variant;
  out ErrorText: string): Boolean;
function LCLTextToUTF8Bytes(const Value: string): TEditUtf8Text;
function DecodeTpXTextForEdit(const StorageText: AnsiString): TEditUtf8Text;
function EncodeEditTextForTpX(const DisplayText: TEditUtf8Text): AnsiString;

function ReadObjectProperty(Drawing: TDrawing2D;
  const TargetIDs: array of Integer;
  Field: TObjectPropertyField): TPropertyValue;
function ValidateObjectPropertyPatch(Drawing: TDrawing2D;
  const TargetIDs: array of Integer;
  const Patch: TObjectPropertyPatch; out ErrorText: string): Boolean;
function ApplyObjectPropertyPatch(Drawing: TDrawing2D;
  const TargetIDs: array of Integer;
  const Patch: TObjectPropertyPatch; out ErrorText: string;
  Repaint: Boolean = True): Boolean;
function CaptureObjectPropertyPatch(Drawing: TDrawing2D;
  const TargetIDs: array of Integer; Fields: TObjectPropertyFields;
  out Patch: TObjectPropertyPatch; out ErrorText: string): Boolean;

implementation

uses SysUtils, Math, Variants, GObjBase, Bitmaps;

procedure AddUnique(List: TList; Obj: TObject2D);
var I: Integer;
begin
  for I := 0 to List.Count - 1 do
    if TObject2D(List[I]).ID = Obj.ID then Exit;
  List.Add(Obj);
end;

procedure CollectLeaves(Obj: TObject2D; List: TList); forward;
function CollectTargets(Drawing: TDrawing2D;
  const TargetIDs: array of Integer; List: TList;
  out ErrorText: string): Boolean; forward;
function SupportsProperty(Obj: TObject2D;
  Field: TObjectPropertyField): Boolean; forward;
function ReadValue(Obj: TObject2D; Field: TObjectPropertyField;
  out Value: Variant): Boolean; forward;

function FindObjectInList(List: TGraphicObjList;
  ID: Integer): TObject2D;
var Obj: TGraphicObject;
begin
  Result := nil;
  Obj := List.FirstObj;
  while Obj <> nil do
  begin
    if Obj.ID = ID then Exit(Obj as TObject2D);
    if Obj is TGroup2D then
    begin
      Result := FindObjectInList((Obj as TGroup2D).Objects, ID);
      if Result <> nil then Exit;
    end;
    Obj := List.NextObj;
  end;
end;

function IsFiniteNonnegative(Value: TRealType): Boolean;
begin
  Result := not IsNan(Value) and not IsInfinite(Value) and (Value >= 0);
end;

function IsFinitePositive(Value: TRealType): Boolean;
begin
  Result := not IsNan(Value) and not IsInfinite(Value) and (Value > 0);
end;

function IsFiniteValue(Value: TRealType): Boolean;
begin
  Result := not IsNan(Value) and not IsInfinite(Value);
end;

function FontStylesMask(const Styles: TFontStyles): Byte;
begin
  Result := 0;
  Move(Styles, Result, SizeOf(Result));
end;

function FontStylesFromMask(Mask: Byte): TFontStyles;
begin
  Result := [];
  Move(Mask, Result, SizeOf(Mask));
end;

function LCLTextToUTF8Bytes(const Value: string): TEditUtf8Text;
var Bytes: RawByteString;
begin
  { A Lazarus string can carry UTF-8 bytes while tagged with the platform's
    default ANSI code page. Retag the existing bytes; never transcode them. }
  Bytes := RawByteString(Value);
  SetCodePage(Bytes, 65001, False);
  Result := TEditUtf8Text(Bytes);
end;

function DecodeTpXTextForEdit(const StorageText: AnsiString): TEditUtf8Text;
var I, Semi, Code: Integer; Part: string; WideResult: WideString;
  RawStorage: RawByteString;
  procedure AppendUtf8(const Value: TEditUtf8Text);
  begin
    WideResult := WideResult + UTF8Decode(Value);
  end;
  procedure AppendCodePoint(Value: Cardinal);
  begin
    if Value <= $FFFF then
      WideResult := WideResult + WideChar(Value)
    else
    begin
      Dec(Value, $10000);
      WideResult := WideResult + WideChar($D800 + (Value shr 10)) +
        WideChar($DC00 + (Value and $3FF));
    end;
  end;
begin
  RawStorage := RawByteString(StorageText);
  SetCodePage(RawStorage, 65001, False);
  WideResult := '';
  I := 1;
  while I <= Length(RawStorage) do
  begin
    if (RawStorage[I] = '&') and (I < Length(RawStorage)) and
      (RawStorage[I + 1] = '#') then
    begin
      Semi := I + 2;
      while (Semi <= Length(RawStorage)) and
        (RawStorage[Semi] <> ';') do Inc(Semi);
      if Semi <= Length(RawStorage) then
      begin
        Part := Copy(RawStorage, I + 2, Semi - I - 2);
        if (Part <> '') and (Part[1] = 'x') then
        begin
          Delete(Part, 1, 1);
          if TryStrToInt('$' + Part, Code) and
            (Code >= 0) and (Code <= $10FFFF) then
          begin
            AppendCodePoint(Code);
            I := Semi + 1;
            Continue;
          end;
        end
        else if TryStrToInt(Part, Code) and
          (Code >= 0) and (Code <= $10FFFF) then
        begin
          AppendCodePoint(Code);
          I := Semi + 1;
          Continue;
        end;
      end;
    end;
    Semi := I;
    while (Semi <= Length(RawStorage)) and
      not ((RawStorage[Semi] = '&') and (Semi < Length(RawStorage)) and
        (RawStorage[Semi + 1] = '#')) do Inc(Semi);
    if Semi = I then Inc(Semi);
    AppendUtf8(TEditUtf8Text(Copy(RawStorage, I, Semi - I)));
    I := Semi;
  end;
  Result := TEditUtf8Text(UTF8Encode(WideResult));
end;

function EncodeEditTextForTpX(const DisplayText: TEditUtf8Text): AnsiString;
var I, Count: Integer; B1, B2, B3, B4: Byte; CodePoint: Cardinal;
  procedure Append(const Value: string);
  begin
    Result := Result + AnsiString(Value);
  end;
  procedure AppendReference(const Value: Cardinal);
  begin
    Append('&#' + IntToStr(Value) + ';');
  end;
  function ContinuationByte(Index: Integer): Byte;
  begin
    if (Index > Length(DisplayText)) or
      ((Ord(DisplayText[Index]) and $C0) <> $80) then
      raise EConvertError.Create('Text is not valid UTF-8');
    Result := Ord(DisplayText[Index]);
  end;
begin
  Result := '';
  I := 1;
  while I <= Length(DisplayText) do
  begin
    B1 := Ord(DisplayText[I]);
    if B1 < $80 then
    begin
      if B1 = Ord('&') then AppendReference(B1)
      else if B1 < 32 then AppendReference(B1)
      else Result := Result + AnsiChar(B1);
      Inc(I);
      Continue;
    end;
    if (B1 >= $C2) and (B1 <= $DF) then
    begin
      Count := 2;
      B2 := ContinuationByte(I + 1);
      CodePoint := (Cardinal(B1 and $1F) shl 6) or (B2 and $3F);
    end
    else if (B1 >= $E0) and (B1 <= $EF) then
    begin
      Count := 3;
      B2 := ContinuationByte(I + 1);
      B3 := ContinuationByte(I + 2);
      if ((B1 = $E0) and (B2 < $A0)) or
        ((B1 = $ED) and (B2 >= $A0)) then
        raise EConvertError.Create('Text is not valid UTF-8');
      CodePoint := (Cardinal(B1 and $0F) shl 12) or
        (Cardinal(B2 and $3F) shl 6) or (B3 and $3F);
    end
    else if (B1 >= $F0) and (B1 <= $F4) then
    begin
      Count := 4;
      B2 := ContinuationByte(I + 1);
      B3 := ContinuationByte(I + 2);
      B4 := ContinuationByte(I + 3);
      if ((B1 = $F0) and (B2 < $90)) or
        ((B1 = $F4) and (B2 > $8F)) then
        raise EConvertError.Create('Text is not valid UTF-8');
      CodePoint := (Cardinal(B1 and $07) shl 18) or
        (Cardinal(B2 and $3F) shl 12) or
        (Cardinal(B3 and $3F) shl 6) or (B4 and $3F);
    end
    else
      raise EConvertError.Create('Text is not valid UTF-8');
    if CodePoint <= $FFFF then
      AppendReference(CodePoint)
    else
    begin
      Dec(CodePoint, $10000);
      AppendReference($D800 + (CodePoint shr 10));
      AppendReference($DC00 + (CodePoint and $3FF));
    end;
    Inc(I, Count);
  end;
end;

function ValidatePatchValues(const Patch: TObjectPropertyPatch;
  out ErrorText: string): Boolean;
begin
  Result := False;
  ErrorText := '';
  if (opfLineStyle in Patch.Fields) and
    ((Ord(Patch.LineStyle) < Ord(Low(TLineStyle))) or
    (Ord(Patch.LineStyle) > Ord(High(TLineStyle)))) then
    ErrorText := 'Invalid line style'
  else if (opfLineWidth in Patch.Fields) and
    not IsFiniteNonnegative(Patch.LineWidth) then
    ErrorText := 'Line width must be a finite nonnegative number'
  else if (opfHatching in Patch.Fields) and
    ((Ord(Patch.Hatching) < Ord(Low(THatching))) or
    (Ord(Patch.Hatching) > Ord(High(THatching)))) then
    ErrorText := 'Invalid hatching style'
  else if (opfBeginArrowKind in Patch.Fields) and
    ((Ord(Patch.BeginArrowKind) < Ord(Low(TArrowKind))) or
    (Ord(Patch.BeginArrowKind) > Ord(High(TArrowKind)))) then
    ErrorText := 'Invalid begin arrow'
  else if (opfEndArrowKind in Patch.Fields) and
    ((Ord(Patch.EndArrowKind) < Ord(Low(TArrowKind))) or
    (Ord(Patch.EndArrowKind) > Ord(High(TArrowKind)))) then
    ErrorText := 'Invalid end arrow'
  else if (opfArrowSizeFactor in Patch.Fields) and
    not IsFiniteNonnegative(Patch.ArrowSizeFactor) then
    ErrorText := 'Arrow size must be a finite nonnegative number'
  else if (opfTextHeight in Patch.Fields) and
    not IsFinitePositive(Patch.TextHeight) then
    ErrorText := 'Text height must be a finite positive number'
  else if (opfHAlignment in Patch.Fields) and
    ((Ord(Patch.HAlignment) < Ord(Low(THAlignment))) or
    (Ord(Patch.HAlignment) > Ord(High(THAlignment)))) then
    ErrorText := 'Invalid text alignment'
  else if (opfTextRotation in Patch.Fields) and
    not IsFiniteValue(Patch.TextRotation) then
    ErrorText := 'Text rotation must be finite'
  else if (opfStarKind in Patch.Fields) and
    ((Ord(Patch.StarKind) < Ord(Low(TStarKind))) or
    (Ord(Patch.StarKind) > Ord(High(TStarKind)))) then
    ErrorText := 'Invalid star style'
  else if (opfStarSizeFactor in Patch.Fields) and
    not IsFiniteNonnegative(Patch.StarSizeFactor) then
    ErrorText := 'Star size must be a finite nonnegative number'
  else if (opfSymbolKind in Patch.Fields) and
    ((Ord(Patch.SymbolKind) < Ord(Low(TSymbolKind))) or
    (Ord(Patch.SymbolKind) > Ord(High(TSymbolKind)))) then
    ErrorText := 'Invalid symbol style'
  else if (opfSymbolDiameter in Patch.Fields) and
    not IsFiniteNonnegative(Patch.SymbolDiameter) then
    ErrorText := 'Symbol diameter must be a finite nonnegative number'
  else if (opfSymbolRotation in Patch.Fields) and
    not IsFiniteValue(Patch.SymbolRotation) then
    ErrorText := 'Symbol rotation must be finite'
  else if (opfRectangleRX in Patch.Fields) and
    not IsFiniteNonnegative(Patch.RectangleRX) then
    ErrorText := 'Rectangle horizontal radius must be finite and nonnegative'
  else if (opfRectangleRY in Patch.Fields) and
    not IsFiniteNonnegative(Patch.RectangleRY) then
    ErrorText := 'Rectangle vertical radius must be finite and nonnegative'
  else
    Result := True;
end;

function ValidateObjectPropertyPatch(Drawing: TDrawing2D;
  const TargetIDs: array of Integer;
  const Patch: TObjectPropertyPatch; out ErrorText: string): Boolean;
var List: TList; I: Integer; Field: TObjectPropertyField;
begin
  Result := False;
  if not ValidatePatchValues(Patch, ErrorText) then Exit;
  if Patch.Fields = [] then Exit(True);
  List := TList.Create;
  try
    if not CollectTargets(Drawing, TargetIDs, List, ErrorText) then Exit;
    for I := 0 to List.Count - 1 do
      for Field := Low(TObjectPropertyField) to High(TObjectPropertyField) do
        if (Field in Patch.Fields) and
          not SupportsProperty(TObject2D(List[I]), Field) then
        begin
          ErrorText := 'A selected object does not support this property';
          Exit;
        end;
    Result := True;
  finally
    List.Free;
  end;
end;

procedure ApplyPatchToObject(Drawing: TDrawing2D; Obj: TObject2D;
  const Patch: TObjectPropertyPatch);
var Prim: TPrimitive2D; Txt: TText2D;
begin
  Prim := nil;
  if Obj is TPrimitive2D then Prim := Obj as TPrimitive2D;
  Txt := nil;
  if Obj is TText2D then Txt := Obj as TText2D;
  if opfLineStyle in Patch.Fields then Prim.LineStyle := Patch.LineStyle;
  if opfLineColor in Patch.Fields then Prim.LineColor := Patch.LineColor;
  if opfLineWidth in Patch.Fields then Prim.LineWidth := Patch.LineWidth;
  if opfHatching in Patch.Fields then Prim.Hatching := Patch.Hatching;
  if opfHatchColor in Patch.Fields then Prim.HatchColor := Patch.HatchColor;
  if opfFillColor in Patch.Fields then Prim.FillColor := Patch.FillColor;
  if opfBeginArrowKind in Patch.Fields then
    Prim.BeginArrowKind := Patch.BeginArrowKind;
  if opfEndArrowKind in Patch.Fields then
    Prim.EndArrowKind := Patch.EndArrowKind;
  if opfArrowSizeFactor in Patch.Fields then
    Prim.ArrowSizeFactor := Patch.ArrowSizeFactor;
  if opfText in Patch.Fields then Txt.Text := Patch.Text;
  if opfTeXText in Patch.Fields then
    Txt.TeXText := RawByteString(Patch.TeXText);
  if opfTextHeight in Patch.Fields then Txt.Height := Patch.TextHeight;
  if opfFontName in Patch.Fields then
    Txt.Font.Name := RawByteString(Patch.FontName);
  if opfFontStyle in Patch.Fields then Txt.Font.Style := Patch.FontStyle;
  if opfHAlignment in Patch.Fields then Txt.HAlignment := Patch.HAlignment;
  if opfTextRotation in Patch.Fields then Txt.Rot := Patch.TextRotation;
  if opfStarKind in Patch.Fields then
    (Obj as TStar2D).StarKind := Patch.StarKind;
  if opfStarSizeFactor in Patch.Fields then
    (Obj as TStar2D).StarSizeFactor := Patch.StarSizeFactor;
  if opfSymbolKind in Patch.Fields then
    (Obj as TSymbol2D).SymbolKind := Patch.SymbolKind;
  if opfSymbolDiameter in Patch.Fields then
    (Obj as TSymbol2D).Diameter := Patch.SymbolDiameter;
  if opfSymbolRotation in Patch.Fields then
    (Obj as TSymbol2D).Rot := Patch.SymbolRotation;
  if opfRectangleRX in Patch.Fields then
    (Obj as TRectangle2D).RX := Patch.RectangleRX;
  if opfRectangleRY in Patch.Fields then
    (Obj as TRectangle2D).RY := Patch.RectangleRY;
  if opfBitmapLink in Patch.Fields then
    (Obj as TBitmap2D).BitmapEntry := Drawing.RegisterBitmap(
      RawByteString(Patch.BitmapLink));
  if opfKeepAspectRatio in Patch.Fields then
    (Obj as TBitmap2D).KeepAspectRatio := Patch.KeepAspectRatio;
  Obj.UpdateExtension(nil);
end;

function ApplyObjectPropertyPatch(Drawing: TDrawing2D;
  const TargetIDs: array of Integer;
  const Patch: TObjectPropertyPatch; out ErrorText: string;
  Repaint: Boolean): Boolean;
type
  TTargetSnapshot = record
    ID: Integer;
    Patch: TObjectPropertyPatch;
  end;
var List: TList; I, AppliedCount: Integer; Snapshots: array of TTargetSnapshot;
  OneID: array[0..0] of Integer;
begin
  Result := False;
  AppliedCount := 0;
  Snapshots := nil;
  if not ValidateObjectPropertyPatch(Drawing, TargetIDs, Patch,
    ErrorText) then Exit;
  if Patch.Fields = [] then Exit(True);
  List := TList.Create;
  try
    try
      if not CollectTargets(Drawing, TargetIDs, List, ErrorText) then Exit;
      SetLength(Snapshots, List.Count);
      for I := 0 to List.Count - 1 do
      begin
        Snapshots[I].ID := TObject2D(List[I]).ID;
        OneID[0] := Snapshots[I].ID;
        if not CaptureObjectPropertyPatch(Drawing, OneID, Patch.Fields,
          Snapshots[I].Patch, ErrorText) then Exit;
      end;
      if opfBitmapLink in Patch.Fields then
        Drawing.RegisterBitmap(Patch.BitmapLink);
      AppliedCount := 0;
      for I := 0 to List.Count - 1 do
      begin
        AppliedCount := I + 1;
        ApplyPatchToObject(Drawing, TObject2D(List[I]), Patch);
      end;
      if Repaint then Drawing.RepaintViewports;
      Result := True;
    except
      on E: Exception do
      begin
        ErrorText := E.Message;
        for I := 0 to AppliedCount - 1 do
        begin
          try
            ApplyPatchToObject(Drawing, TObject2D(List[I]),
              Snapshots[I].Patch);
          except
            { Keep the original failure; valid snapshots normally restore
              through setters without raising. }
          end;
        end;
        if Repaint then Drawing.RepaintViewports;
        Result := False;
      end;
    end;
  finally
    List.Free;
  end;
end;

function AssignObjectPropertyPatchValue(var Patch: TObjectPropertyPatch;
  Field: TObjectPropertyField; const Value: Variant;
  out ErrorText: string): Boolean;
begin
  Result := False;
  ErrorText := '';
  Include(Patch.Fields, Field);
  case Field of
    opfLineStyle: Patch.LineStyle := TLineStyle(Integer(Value));
    opfLineColor: Patch.LineColor := TColor(Integer(Value));
    opfLineWidth: Patch.LineWidth := TRealType(Value);
    opfHatching: Patch.Hatching := THatching(Integer(Value));
    opfHatchColor: Patch.HatchColor := TColor(Integer(Value));
    opfFillColor: Patch.FillColor := TColor(Integer(Value));
    opfBeginArrowKind: Patch.BeginArrowKind := TArrowKind(Integer(Value));
    opfEndArrowKind: Patch.EndArrowKind := TArrowKind(Integer(Value));
    opfArrowSizeFactor: Patch.ArrowSizeFactor := TRealType(Value);
    opfText: Patch.Text := AnsiString(string(Value));
    opfTeXText: Patch.TeXText := LCLTextToUTF8Bytes(VarToStr(Value));
    opfTextHeight: Patch.TextHeight := TRealType(Value);
    opfFontName: Patch.FontName := LCLTextToUTF8Bytes(VarToStr(Value));
    opfFontStyle: Patch.FontStyle := FontStylesFromMask(Byte(Value));
    opfHAlignment: Patch.HAlignment := THAlignment(Integer(Value));
    opfTextRotation: Patch.TextRotation := TRealType(Value);
    opfStarKind: Patch.StarKind := TStarKind(Integer(Value));
    opfStarSizeFactor: Patch.StarSizeFactor := TRealType(Value);
    opfSymbolKind: Patch.SymbolKind := TSymbolKind(Integer(Value));
    opfSymbolDiameter: Patch.SymbolDiameter := TRealType(Value);
    opfSymbolRotation: Patch.SymbolRotation := TRealType(Value);
    opfRectangleRX: Patch.RectangleRX := TRealType(Value);
    opfRectangleRY: Patch.RectangleRY := TRealType(Value);
    opfBitmapLink: Patch.BitmapLink := LCLTextToUTF8Bytes(VarToStr(Value));
    opfKeepAspectRatio: Patch.KeepAspectRatio := Boolean(Value);
  end;
  Result := True;
end;

function ExpandObjectEditTargetIDs(Drawing: TDrawing2D;
  const TargetIDs: array of Integer; out ErrorText: string): TEditTargetIDs;
var List: TList; I: Integer;
begin
  Result := nil;
  List := TList.Create;
  try
    if not CollectTargets(Drawing, TargetIDs, List, ErrorText) then Exit;
    SetLength(Result, List.Count);
    for I := 0 to List.Count - 1 do
      Result[I] := TObject2D(List[I]).ID;
  finally
    List.Free;
  end;
end;

function CaptureObjectPropertyPatch(Drawing: TDrawing2D;
  const TargetIDs: array of Integer; Fields: TObjectPropertyFields;
  out Patch: TObjectPropertyPatch; out ErrorText: string): Boolean;
var Field: TObjectPropertyField; ReadValueResult: TPropertyValue;
begin
  Patch.Fields := [];
  Patch.Text := '';
  Patch.TeXText := '';
  Patch.FontName := '';
  Patch.BitmapLink := '';
  ErrorText := '';
  Result := False;
  for Field := Low(TObjectPropertyField) to High(TObjectPropertyField) do
    if Field in Fields then
    begin
      ReadValueResult := ReadObjectProperty(Drawing, TargetIDs, Field);
      if ReadValueResult.State <> pvsUniform then
      begin
        ErrorText := 'Cannot snapshot a mixed or unavailable property';
        Exit;
      end;
      if not AssignObjectPropertyPatchValue(Patch, Field,
        ReadValueResult.Value, ErrorText) then
        Exit;
    end;
  Result := True;
end;

procedure CollectLeaves(Obj: TObject2D; List: TList);
var Child: TGraphicObject;
begin
  if Obj is TGroup2D then
  begin
    Child := (Obj as TGroup2D).Objects.FirstObj;
    while Child <> nil do
    begin
      CollectLeaves(Child as TObject2D, List);
      Child := (Obj as TGroup2D).Objects.NextObj;
    end;
  end
  else
    AddUnique(List, Obj);
end;

function CollectTargets(Drawing: TDrawing2D;
  const TargetIDs: array of Integer; List: TList;
  out ErrorText: string): Boolean;
var I: Integer; Obj: TObject2D;
begin
  Result := False;
  ErrorText := '';
  if Drawing = nil then
  begin
    ErrorText := 'The drawing is no longer available';
    Exit;
  end;
  for I := 0 to High(TargetIDs) do
  begin
    Obj := FindObjectInList(Drawing.ObjectList, TargetIDs[I]);
    if Obj = nil then
    begin
      ErrorText := 'The selected object no longer exists';
      Exit;
    end;
    CollectLeaves(Obj, List);
  end;
  if List.Count = 0 then
  begin
    ErrorText := 'Select at least one editable object';
    Exit;
  end;
  Result := True;
end;

function SupportsProperty(Obj: TObject2D;
  Field: TObjectPropertyField): Boolean;
begin
  case Field of
    opfLineStyle, opfLineColor, opfLineWidth, opfHatching,
    opfHatchColor, opfFillColor, opfBeginArrowKind,
    opfEndArrowKind, opfArrowSizeFactor:
      Result := Obj is TPrimitive2D;
    opfText, opfTeXText, opfTextHeight, opfFontName,
    opfFontStyle, opfHAlignment, opfTextRotation:
      Result := Obj is TText2D;
    opfStarKind, opfStarSizeFactor: Result := Obj is TStar2D;
    opfSymbolKind, opfSymbolDiameter, opfSymbolRotation:
      Result := Obj is TSymbol2D;
    opfRectangleRX, opfRectangleRY: Result := Obj is TRectangle2D;
    opfBitmapLink, opfKeepAspectRatio: Result := Obj is TBitmap2D;
  end;
end;

function ReadValue(Obj: TObject2D; Field: TObjectPropertyField;
  out Value: Variant): Boolean;
var Prim: TPrimitive2D; Txt: TText2D;
begin
  Result := SupportsProperty(Obj, Field);
  if not Result then Exit;
  Prim := nil;
  if Obj is TPrimitive2D then Prim := Obj as TPrimitive2D;
  Txt := nil;
  if Obj is TText2D then Txt := Obj as TText2D;
  case Field of
    opfLineStyle: Value := Ord(Prim.LineStyle);
    opfLineColor: Value := Integer(Prim.LineColor);
    opfLineWidth: Value := Prim.LineWidth;
    opfHatching: Value := Ord(Prim.Hatching);
    opfHatchColor: Value := Integer(Prim.HatchColor);
    opfFillColor: Value := Integer(Prim.FillColor);
    opfBeginArrowKind: Value := Ord(Prim.BeginArrowKind);
    opfEndArrowKind: Value := Ord(Prim.EndArrowKind);
    opfArrowSizeFactor: Value := Prim.ArrowSizeFactor;
    opfText: Value := string(Txt.Text);
    opfTeXText: Value := string(Txt.TeXText);
    opfTextHeight: Value := Txt.Height;
    opfFontName: Value := Txt.Font.Name;
    opfFontStyle: Value := FontStylesMask(Txt.Font.Style);
    opfHAlignment: Value := Ord(Txt.HAlignment);
    opfTextRotation: Value := Txt.Rot;
    opfStarKind: Value := Ord((Obj as TStar2D).StarKind);
    opfStarSizeFactor: Value := (Obj as TStar2D).StarSizeFactor;
    opfSymbolKind: Value := Ord((Obj as TSymbol2D).SymbolKind);
    opfSymbolDiameter: Value := (Obj as TSymbol2D).Diameter;
    opfSymbolRotation: Value := (Obj as TSymbol2D).Rot;
    opfRectangleRX: Value := (Obj as TRectangle2D).RX;
    opfRectangleRY: Value := (Obj as TRectangle2D).RY;
    opfBitmapLink:
      if (Obj as TBitmap2D).BitmapEntry = nil then Value := ''
      else Value := (Obj as TBitmap2D).BitmapEntry.ImageLink;
    opfKeepAspectRatio: Value := (Obj as TBitmap2D).KeepAspectRatio;
  end;
end;

function ReadObjectProperty(Drawing: TDrawing2D;
  const TargetIDs: array of Integer;
  Field: TObjectPropertyField): TPropertyValue;
var List: TList; ErrorText: string; I: Integer; Value: Variant;
  Acc: TPropertyReadAccumulator;
begin
  Acc.SupportedCount := 0;
  Acc.UnavailableCount := 0;
  Acc.HasValue := False;
  Acc.Value := Unassigned;
  Acc.Mixed := False;
  List := TList.Create;
  try
    if CollectTargets(Drawing, TargetIDs, List, ErrorText) then
      for I := 0 to List.Count - 1 do
      begin
        Value := Unassigned;
        AddPropertyRead(Acc, ReadValue(TObject2D(List[I]), Field, Value),
          Value);
      end;
    Result := FinishPropertyRead(Acc);
  finally
    List.Free;
  end;
end;

end.
