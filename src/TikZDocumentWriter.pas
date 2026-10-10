unit TikZDocumentWriter;

{$mode objfpc}{$H+}

interface

uses DocumentFormats;

procedure RegisterTikZDocumentCodec;

implementation

uses Classes, SysUtils, Math, Graphics, Drawings, Geometry, GObjects,
  Devices, GObjBase, TikZLexer, TikZSyntax, TikZImport, TikZImportDrawing,
  TikZWriter, DocumentIO;

type
  TEditArray = array of TTikZBoundEdit;
  TPatchArray = array of TTikZSourcePatch;
  TSpanArray = array of TTikZSourceSpan;

function BytesOf(const S: RawByteString): TBytes;
var I: SizeInt;
begin
  Result := nil;
  SetLength(Result, Length(S));
  for I := 1 to Length(S) do Result[I - 1] := Ord(S[I]);
end;

function RawOf(const B: TBytes): RawByteString;
var I: SizeInt;
begin
  SetLength(Result, Length(B));
  for I := 0 to High(B) do Result[I + 1] := AnsiChar(B[I]);
end;

function SameSpan(const A, B: TTikZSourceSpan): Boolean; inline;
begin
  Result := (A.StartByte = B.StartByte) and (A.EndByte = B.EndByte);
end;

function NearValue(A, B: Extended): Boolean; inline;
begin
  Result := Abs(A - B) <= 0.000002 * Max(1.0, Max(Abs(A), Abs(B)));
end;

function SourceText(const Source: TBytes; const Span: TTikZSourceSpan): string;
var I, N: SizeInt;
begin
  Result := '';
  if (Span.StartByte < 0) or (Span.EndByte < Span.StartByte) or
    (Span.EndByte > Length(Source)) then Exit;
  N := Span.EndByte - Span.StartByte;
  SetLength(Result, N);
  for I := 0 to N - 1 do Result[I + 1] := AnsiChar(Source[Span.StartByte + I]);
end;

function AppendUnit(const Number, UnitName: string): RawByteString;
begin
  Result := RawByteString(Number + UnitName);
end;

function IsLineBreak(Value: Byte): Boolean; inline;
begin
  Result := Value in [Ord(#10), Ord(#13)];
end;

function EOLBytes(const Source: TBytes): RawByteString;
var I: SizeInt;
begin
  Result := RawByteString(LineEnding);
  for I := 0 to High(Source) do
  begin
    if Source[I] = Ord(#13) then
    begin
      if (I < High(Source)) and (Source[I + 1] = Ord(#10)) then
        Result := #13#10
      else Result := #13;
      Exit;
    end;
    if Source[I] = Ord(#10) then
    begin
      Result := #10;
      Exit;
    end;
  end;
end;

function SpanAlreadyPresent(const Spans: TSpanArray;
  const Span: TTikZSourceSpan): Boolean;
var I: SizeInt;
begin
  for I := 0 to High(Spans) do
    if SameSpan(Spans[I], Span) then Exit(True);
  Result := False;
end;

procedure AddSpan(var Spans: TSpanArray; const Span: TTikZSourceSpan);
var N: SizeInt;
begin
  N := Length(Spans);
  SetLength(Spans, N + 1);
  Spans[N] := Span;
end;

procedure AddBoundEdit(var Edits: TEditArray; BindingIndex,
  DependencyIndex: SizeInt; const Replacement: RawByteString;
  const LocalOverride: RawByteString = '';
  const LocalExpressionOverride: Boolean = False);
var N: SizeInt;
begin
  N := Length(Edits);
  SetLength(Edits, N + 1);
  Edits[N].BindingIndex := BindingIndex;
  Edits[N].DependencyIndex := DependencyIndex;
  Edits[N].Replacement := BytesOf(Replacement);
  Edits[N].LocalOverrideOption := BytesOf(LocalOverride);
  Edits[N].LocalExpressionOverride := LocalExpressionOverride;
end;

procedure AddPatch(var Patches: TPatchArray; const Span: TTikZSourceSpan;
  const Replacement: RawByteString);
var N: SizeInt;
begin
  N := Length(Patches);
  SetLength(Patches, N + 1);
  Patches[N].Span := Span;
  Patches[N].Replacement := BytesOf(Replacement);
end;

function ColorToRGB24(Value: TColor): LongWord;
var V: LongWord;
begin
  V := LongWord(ColorToRGB(Value));
  Result := ((V and $FF) shl 16) or (V and $FF00) or ((V shr 16) and $FF);
end;

function RGBColorName(Value: LongWord; out Name: string): Boolean;
begin
  Result := True;
  case Value and $FFFFFF of
    $000000: Name := 'black';
    $FFFFFF: Name := 'white';
    $FF0000: Name := 'red';
    $00FF00: Name := 'green';
    $0000FF: Name := 'blue';
    $808080: Name := 'gray';
    $00FFFF: Name := 'cyan';
    $FF00FF: Name := 'magenta';
    $FFFF00: Name := 'yellow';
  else
    Name := '';
    Result := False;
  end;
end;

function FinitePoint(const P: TPoint2D): Boolean; inline;
begin
  Result := (P.W <> 0) and not IsNan(P.X) and not IsNan(P.Y) and
    not IsInfinite(P.X) and not IsInfinite(P.Y);
end;

function SceneCommandPoint(const Obj: TTikZSceneObject; CommandIndex,
  PointSlot: SizeInt; out P: TTikZPoint): Boolean;
var C: TTikZPathCommand;
begin
  Result := False;
  if (CommandIndex < 0) or (CommandIndex >= Length(Obj.Commands)) then Exit;
  C := Obj.Commands[CommandIndex];
  case PointSlot of
    1:
      case C.Kind of
        tpcMove: P := C.P1;
        tpcLine: P := C.P1;
        tpcCubic: P := C.P1;
      else Exit;
      end;
    2:
      if C.Kind <> tpcCubic then Exit else P := C.P2;
    3:
      if C.Kind <> tpcCubic then Exit else P := C.P3;
  else Exit;
  end;
  Result := True;
end;

function NativePathPointIndex(const Obj: TTikZSceneObject; CommandIndex,
  PointSlot: SizeInt; out NativeIndex: SizeInt): Boolean;
var I, N: SizeInt; HasCubic: Boolean; C: TTikZPathCommand;
begin
  Result := False;
  HasCubic := False;
  for I := 0 to High(Obj.Commands) do
    if Obj.Commands[I].Kind = tpcCubic then HasCubic := True;
  N := 0;
  for I := 0 to CommandIndex do
  begin
    C := Obj.Commands[I];
    if I = CommandIndex then
    begin
      case C.Kind of
        tpcMove: if PointSlot = 1 then NativeIndex := 0 else Exit;
        tpcLine:
          if PointSlot <> 1 then Exit
          else if HasCubic then NativeIndex := N + 3
          else NativeIndex := N + 1;
        tpcCubic:
          if (PointSlot < 1) or (PointSlot > 3) then Exit
          else NativeIndex := N + PointSlot;
      else Exit;
      end;
      Result := True;
      Exit;
    end;
    case C.Kind of
      tpcMove: N := 0;
      tpcLine: if HasCubic then Inc(N, 3) else Inc(N, 1);
      tpcCubic: Inc(N, 3);
    end;
  end;
end;

function GetBoundPoint(Native: TPrimitive2D; SceneObject: TTikZSceneObject;
  CommandIndex, PointSlot, PointOrdinal: SizeInt; out P: TPoint2D;
  out Baseline: TTikZPoint): Boolean;
var
  Index: SizeInt;
  P0, P1, P2: TPoint2D;
  LeftX, RightX, BottomY, TopY, Angle: Extended;
  RX, RY, EllipseAngle: TRealType;
  FirstX, FirstY: Boolean;
  procedure Unrotate(var Q: TPoint2D);
  var X, Y, C, S: Extended;
  begin
    X := Q.X / Q.W - SceneObject.RotationCenter.X;
    Y := Q.Y / Q.W - SceneObject.RotationCenter.Y;
    C := Cos(SceneObject.Rotation); S := Sin(SceneObject.Rotation);
    Q := Point2D(SceneObject.RotationCenter.X + C * X + S * Y,
      SceneObject.RotationCenter.Y - S * X + C * Y);
  end;
begin
  Result := False;
  if (Native = nil) or (SceneObject = nil) then Exit;
  if SceneObject.Kind = tsoPath then
  begin
    if not NativePathPointIndex(SceneObject, CommandIndex, PointSlot, Index) or
      (Index < 0) or (Index >= Native.Points.Count) or
      not SceneCommandPoint(SceneObject, CommandIndex, PointSlot, Baseline) then Exit;
    P := Native.Points[Index];
  end
  else if SceneObject.Kind = tsoRectangle then
  begin
    if not (Native is TRectangle2D) or (Native.Points.Count < 3) or
      (PointOrdinal > 1) then Exit;
    P0 := Native.Points[0]; P1 := Native.Points[1]; P2 := Native.Points[2];
    Angle := ArcTan2(P1.Y / P1.W - P2.Y / P2.W,
      P1.X / P1.W - P2.X / P2.W);
    if not NearValue(Angle, SceneObject.Rotation) then Exit;
    Unrotate(P0); Unrotate(P1); Unrotate(P2);
    LeftX := P0.X; RightX := P1.X - P2.X + P0.X;
    BottomY := P0.Y; TopY := P2.Y;
    FirstX := SceneObject.SourceFirst.X <= SceneObject.SourceSecond.X;
    FirstY := SceneObject.SourceFirst.Y <= SceneObject.SourceSecond.Y;
    if PointOrdinal = 0 then
    begin
      if FirstX then P := Point2D(LeftX, P0.Y)
      else P := Point2D(RightX, P0.Y);
      if FirstY then P.Y := BottomY else P.Y := TopY;
      Baseline := SceneObject.SourceFirst;
    end
    else
    begin
      if FirstX then P := Point2D(RightX, P0.Y)
      else P := Point2D(LeftX, P0.Y);
      if FirstY then P.Y := TopY else P.Y := BottomY;
      Baseline := SceneObject.SourceSecond;
    end;
    Result := True;
  end
  else if SceneObject.Kind in [tsoText, tsoCircle, tsoEllipse, tsoImage] then
  begin
    if (CommandIndex <> 0) or (PointSlot <> 1) or
      (Native.Points.Count < 1) then Exit;
    if SceneObject.Kind = tsoEllipse then
    begin
      if not (Native is TEllipse2D) then Exit;
      TEllipse2D(Native).GetEllipseParams(P, RX, RY, EllipseAngle);
    end
    else P := Native.Points[0];
    Baseline := SceneObject.Center;
  end
  else if SceneObject.Kind in [tsoArc, tsoSector, tsoSegment] then Exit
  else Exit;
  if P.W = 0 then P.W := 1;
  Result := FinitePoint(P);
end;

function ParseSourceScalar(const Source: TBytes; const Span: TTikZSourceSpan;
  out Value: Extended): Boolean;
var S, NumberText: string; I: SizeInt; Settings: TFormatSettings;
begin
  S := Trim(SourceText(Source, Span));
  for I := 1 to Length(S) do
    if not (S[I] in ['0'..'9', '.', 'e', 'E', '+', '-']) then
    begin
      S := Copy(S, 1, I - 1);
      Break;
    end;
  NumberText := S;
  Settings := DefaultFormatSettings;
  Settings.DecimalSeparator := '.';
  Settings.ThousandSeparator := ',';
  Result := (NumberText <> '') and TryStrToFloat(NumberText, Value, Settings);
end;

function CoordinateNativeMM(const P: TPoint2D; Axis: TTikZDependencyKind): Extended;
begin
  if Axis = tdCoordinateX then Result := P.X / P.W else Result := P.Y / P.W;
end;

function UntransformScopePoint(const Transform: TTikZAffineTransform;
  const Point: TPoint2D; out X, Y: Extended): Boolean;
var DX, DY, Determinant: Extended;
begin
  Result := False;
  if (Point.W = 0) or IsNan(Point.W) or IsInfinite(Point.W) then Exit;
  Determinant := Transform.XX * Transform.YY - Transform.XY * Transform.YX;
  if IsNan(Determinant) or IsInfinite(Determinant) or
    (Abs(Determinant) < 1E-12) then Exit;
  DX := Point.X / Point.W - Transform.TX;
  DY := Point.Y / Point.W - Transform.TY;
  X := (Transform.YY * DX - Transform.XY * DY) / Determinant;
  Y := (Transform.XX * DY - Transform.YX * DX) / Determinant;
  Result := not IsNan(X) and not IsNan(Y) and
    not IsInfinite(X) and not IsInfinite(Y);
end;

procedure AddCoordinateEdits(Context: TTikZImportContext; BindingIndex: SizeInt;
  Native: TPrimitive2D; SceneObject: TTikZSceneObject; Source: TBytes;
  var Edits: TEditArray);
var
  I, J, PointOrdinal: SizeInt;
  Dep, XDep, YDep: TTikZPropertyDependency;
  P: TPoint2D;
  BaselinePoint: TTikZPoint;
  BaseX, BaseY, OriginalBaseX, OriginalBaseY: Extended;
  TargetX, TargetY, OldX, OldY: Extended;
  BaselineNative: TPoint2D;
  HasX, HasY: Boolean;
  XIndex, YIndex: SizeInt;
  NumberText: string;
  Binding: TTikZSourceBinding;

  procedure UndoSourceRotation(var X, Y: Extended);
  var DX, DY, C, S: Extended;
  begin
    if Abs(SceneObject.SourceRotation) < 1E-12 then Exit;
    DX := X - SceneObject.SourceRotationCenter.X;
    DY := Y - SceneObject.SourceRotationCenter.Y;
    C := Cos(SceneObject.SourceRotation);
    S := Sin(SceneObject.SourceRotation);
    X := SceneObject.SourceRotationCenter.X + C * DX + S * DY;
    Y := SceneObject.SourceRotationCenter.Y - S * DX + C * DY;
  end;

  procedure AddAxisEdit(const Dependency: TTikZPropertyDependency;
    DependencyIndex: SizeInt; Target, Baseline, CurrentBase,
    InitialBase: Extended);
  var OldValue, Number, PhysicalTolerance: Extended;
  begin
    if NearValue(Target, Baseline) and NearValue(CurrentBase, InitialBase) then Exit;
    if not (Dependency.DependencySource in [tdepLocalLiteral, tdepLocalStyle]) then
      raise EWriteError.Create('Unsupported TikZ coordinate dependency');
    if (Dependency.ValueScaleToMM = 0) or IsNan(Dependency.ValueScaleToMM) or
      IsInfinite(Dependency.ValueScaleToMM) then
      raise EWriteError.Create('TikZ coordinate has no invertible source scale');
    PhysicalTolerance := 0.000002 * Max(1.0, Max(Abs(Baseline), Abs(Target)));
    if Dependency.IsRelative then
      Number := (Target - CurrentBase) / Dependency.ValueScaleToMM
    else Number := Target / Dependency.ValueScaleToMM;
    if ParseSourceScalar(Source, Dependency.ValueSpan, OldValue) and
      (Abs((Number - OldValue) * Dependency.ValueScaleToMM) <= PhysicalTolerance) then Exit;
    NumberText := FormatTikZReal(Number, SizeOf(TRealType));
    AddBoundEdit(Edits, BindingIndex, DependencyIndex,
      AppendUnit(NumberText, Dependency.ValueUnit));
  end;

begin
  Binding := Context.BindingAt(BindingIndex);
  BaseX := 0; BaseY := 0;
  OriginalBaseX := 0; OriginalBaseY := 0;
  PointOrdinal := 0;
  I := 0;
  while I < Length(Binding.Dependencies) do
  begin
    Dep := Binding.Dependencies[I];
    if not (Dep.Kind in [tdCoordinateX, tdCoordinateY]) then
    begin
      Inc(I);
      Continue;
    end;
    XDep := Dep; YDep := Dep;
    XIndex := I; YIndex := I;
    HasX := Dep.Kind = tdCoordinateX;
    HasY := Dep.Kind = tdCoordinateY;
    J := I + 1;
    if J < Length(Binding.Dependencies) then
    begin
      if (Binding.Dependencies[J].CommandIndex = Dep.CommandIndex) and
        (Binding.Dependencies[J].PointSlot = Dep.PointSlot) then
      begin
        if Binding.Dependencies[J].Kind = tdCoordinateX then
        begin XDep := Binding.Dependencies[J]; XIndex := J; HasX := True; end
        else if Binding.Dependencies[J].Kind = tdCoordinateY then
        begin YDep := Binding.Dependencies[J]; YIndex := J; HasY := True; end;
      end;
    end;
    if not GetBoundPoint(Native, SceneObject, Dep.CommandIndex,
      Dep.PointSlot, PointOrdinal, P, BaselinePoint) then
      raise EWriteError.Create('TikZ object geometry no longer matches its source binding');
    XDep := Binding.Dependencies[XIndex];
    YDep := Binding.Dependencies[YIndex];
    if not UntransformScopePoint(XDep.ScopeTransform, P, TargetX, TargetY) then
      raise EWriteError.Create('TikZ coordinate has a non-invertible scope transform');
    if SceneObject.Kind = tsoRectangle then
    begin
      { Rectangle source corners are retained before the object's own rotate
        option. GetBoundPoint removes that rotation, and the scope inverse above
        returns the edited native corner to the original coordinate system. }
      OldX := BaselinePoint.X;
      OldY := BaselinePoint.Y;
    end
    else
    begin
      BaselineNative := Point2D(BaselinePoint.X, BaselinePoint.Y);
      if not UntransformScopePoint(XDep.ScopeTransform, BaselineNative,
        OldX, OldY) then
        raise EWriteError.Create('TikZ source point has a non-invertible scope transform');
      if SceneObject.Kind = tsoPath then
      begin
        UndoSourceRotation(TargetX, TargetY);
        UndoSourceRotation(OldX, OldY);
      end;
    end;
    if HasX then
    begin
      AddAxisEdit(XDep, XIndex, TargetX, OldX,
        BaseX, OriginalBaseX);
      if XDep.UpdatesRelativeBase then BaseX := TargetX;
      if XDep.UpdatesRelativeBase then OriginalBaseX := OldX;
    end;
    if HasY then
    begin
      AddAxisEdit(YDep, YIndex, TargetY, OldY,
        BaseY, OriginalBaseY);
      if YDep.UpdatesRelativeBase then BaseY := TargetY;
      if YDep.UpdatesRelativeBase then OriginalBaseY := OldY;
    end;
    if HasX and HasY then
    begin
      PointOrdinal := PointOrdinal + 1;
      I := I + 2;
    end
    else Inc(I);
  end;
end;

function FindDependency(const Binding: TTikZSourceBinding;
  Kind: TTikZDependencyKind): SizeInt;
var I: SizeInt;
begin
  Result := -1;
  for I := 0 to High(Binding.Dependencies) do
    if Binding.Dependencies[I].Kind = Kind then Exit(I);
end;

procedure CheckBoundPrimitiveType(const Primitive: TPrimitive2D;
  const SceneObject: TTikZSceneObject);
var I, ExpectedPoints: SizeInt; HasCubic, IsClosed: Boolean;
begin
  case SceneObject.Kind of
    tsoPath:
      begin
        HasCubic := False;
        IsClosed := False;
        ExpectedPoints := 0;
        for I := 0 to High(SceneObject.Commands) do
          case SceneObject.Commands[I].Kind of
            tpcCubic: HasCubic := True;
            tpcClose: IsClosed := True;
          end;
        for I := 0 to High(SceneObject.Commands) do
          case SceneObject.Commands[I].Kind of
            tpcMove: Inc(ExpectedPoints);
            tpcLine: if HasCubic then Inc(ExpectedPoints, 3)
              else Inc(ExpectedPoints);
            tpcCubic: Inc(ExpectedPoints, 3);
          end;
        if HasCubic then
        begin
          if IsClosed and not (Primitive is TClosedBezierPath2D) then
            raise EWriteError.Create('TikZ path was replaced by a different native path type');
          if not IsClosed and not (Primitive is TBezierPath2D) then
            raise EWriteError.Create('TikZ path was replaced by a different native path type');
        end
        else if IsClosed and not (Primitive is TPolygon2D) then
          raise EWriteError.Create('TikZ path was replaced by a different native path type')
        else if not IsClosed and not (Primitive is TPolyline2D) and
          not ((ExpectedPoints = 2) and (Primitive is TLine2D)) then
          raise EWriteError.Create('TikZ path was replaced by a different native path type');
        if Primitive.Points.Count <> ExpectedPoints then
          raise EWriteError.Create('TikZ path topology edits require a statement-level rewrite');
      end;
    tsoCircle:
      if not (Primitive is TCircle2D) then
        raise EWriteError.Create('TikZ circle was replaced by a different native object type');
    tsoEllipse:
      if not (Primitive is TEllipse2D) then
        raise EWriteError.Create('TikZ ellipse was replaced by a different native object type');
    tsoRectangle:
      if not (Primitive is TRectangle2D) then
        raise EWriteError.Create('TikZ rectangle was replaced by a different native object type');
    tsoArc, tsoSector, tsoSegment:
      if not (Primitive is TCircular2D) then
        raise EWriteError.Create('TikZ arc was replaced by a different native object type');
    tsoText:
      if not (Primitive is TText2D) then
        raise EWriteError.Create('TikZ node was replaced by a different native object type');
    tsoImage:
      if not (Primitive is TBitmap2D) then
        raise EWriteError.Create('TikZ image was replaced by a different native object type');
  end;
end;

function PropertyReplacement(const Dependency: TTikZPropertyDependency;
  const Value: RawByteString; const Key: string): TTikZBoundEdit;
begin
  Result.BindingIndex := -1;
  Result.DependencyIndex := -1;
  Result.Replacement := nil;
  Result.LocalOverrideOption := nil;
  Result.LocalExpressionOverride := False;
  Result.Replacement := BytesOf(Value);
  if Dependency.DependencySource in [tdepLocalLiteral, tdepLocalStyle] then
    Exit;
  if not Dependency.HasLocalOverride then
    raise EWriteError.Create('TikZ property depends on a shared or unsupported source value');
  Result.LocalOverrideOption := BytesOf(RawByteString(Key + '=' + string(Value)));
end;

procedure AddPropertyEdit(Context: TTikZImportContext; BindingIndex,
  DependencyIndex: SizeInt; const Value: RawByteString;
  const Key: string; var Edits: TEditArray);
var Binding: TTikZSourceBinding; Dep: TTikZPropertyDependency;
  E: TTikZBoundEdit; N, I: SizeInt;
begin
  Binding := Context.BindingAt(BindingIndex);
  if (DependencyIndex < 0) or
    (DependencyIndex >= Length(Binding.Dependencies)) then
    raise EWriteError.Create('A changed object property has no source binding');
  Dep := Binding.Dependencies[DependencyIndex];
  E := PropertyReplacement(Dep, Value, Key);
  E.BindingIndex := BindingIndex;
  E.DependencyIndex := DependencyIndex;
  { A single TikZ statement can materialize as multiple native subpaths. A
    shared style option cannot safely change for only one of those objects. }
  for I := 0 to Context.BindingCount - 1 do
    if (I <> BindingIndex) and SameSpan(Context.BindingAt(I).StatementSpan,
      Binding.StatementSpan) then
      raise EWriteError.Create('This style is shared by multiple paths in one TikZ statement');
  N := Length(Edits);
  SetLength(Edits, N + 1);
  Edits[N] := E;
end;

function TeXText(const Value: string): RawByteString;
var I: SizeInt;
begin
  Result := '';
  for I := 1 to Length(Value) do
    case Value[I] of
      '\': Result := Result + '\textbackslash{}';
      '{': Result := Result + '\{';
      '}': Result := Result + '\}';
      '%': Result := Result + '\%';
      '$': Result := Result + '\$';
      '&': Result := Result + '\&';
      '#': Result := Result + '\#';
      '_': Result := Result + '\_';
      '^': Result := Result + '\textasciicircum{}';
      '~': Result := Result + '\textasciitilde{}';
    else Result := Result + AnsiChar(Value[I]);
    end;
end;

function NativeTextBody(TextObject: TText2D): RawByteString;
begin
  if TextObject.TeXText <> '' then
    Result := RawByteString(TextObject.TeXText)
  else
    Result := TeXText(TextObject.Text);
end;

procedure AddNativeChanges(Context: TTikZImportContext; BindingIndex: SizeInt;
  Native: TPrimitive2D; Drawing: TDrawing2D; Source: TBytes;
  var Edits: TEditArray);
var
  Binding: TTikZSourceBinding;
  SceneObject: TTikZSceneObject;
  I, DepIndex: SizeInt;
  Dep: TTikZPropertyDependency;
  PhysicalWidth: Extended;
  Name, Value, Body: string;
  RGB: LongWord;
  BodyChanged: Boolean;
  TextObject: TText2D;
  Circular: TCircular2D;
  Ellipse: TEllipse2D;
  Bitmap: TBitmap2D;
  RadiusX, RadiusY, StartAngle, EndAngle, CurrentWidth,
    CurrentHeight: Extended;
  NewBaseline, SizeRatio, TextSizeBaseMM: Extended;
  CenterPoint: TPoint2D;
  NativeRadiusX, NativeRadiusY, NativeAngle: TRealType;
  procedure AddScalarEdit(Kind: TTikZDependencyKind; ScalarValue: Extended);
  var D: TTikZPropertyDependency; SourceValue, PhysicalTolerance: Extended;
  begin
    DepIndex := FindDependency(Binding, Kind);
    if DepIndex < 0 then
      raise EWriteError.Create('Changed geometry value has no TikZ source dependency');
    D := Binding.Dependencies[DepIndex];
    if D.DependencySource <> tdepLocalLiteral then
      raise EWriteError.Create('Changed geometry value depends on a shared TikZ expression');
    if (D.ValueScaleToMM = 0) or IsNan(D.ValueScaleToMM) or
      IsInfinite(D.ValueScaleToMM) then
      raise EWriteError.Create('Changed geometry value has no invertible TikZ unit');
    PhysicalTolerance := 0.000002 * Max(1.0, Abs(ScalarValue));
    if ParseSourceScalar(Source, D.ValueSpan, SourceValue) and
      (Abs((ScalarValue / D.ValueScaleToMM - SourceValue) * D.ValueScaleToMM) <=
      PhysicalTolerance) then Exit;
    Value := FormatTikZReal(ScalarValue / D.ValueScaleToMM,
      SizeOf(TRealType)) + D.ValueUnit;
    AddBoundEdit(Edits, BindingIndex, DepIndex, RawByteString(Value));
  end;
begin
  Binding := Context.BindingAt(BindingIndex);
  if (Binding.ObjectIndex < 0) or
    (Binding.ObjectIndex >= Context.Scene.ObjectCount) then
    raise EWriteError.Create('TikZ source binding has no scene object');
  SceneObject := Context.Scene.ObjectAt(Binding.ObjectIndex);
  CheckBoundPrimitiveType(Native, SceneObject);
  if Native.Hatching <> haNone then
    raise EWriteError.Create('Native hatching cannot be represented by this TikZ source codec');
  if (Native.BeginArrowKind <> arrNone) or (Native.EndArrowKind <> arrNone) then
    raise EWriteError.Create('Native arrow edits are not representable by this TikZ source codec');

  if SceneObject.Kind in [tsoPath, tsoRectangle, tsoText, tsoCircle,
    tsoEllipse, tsoImage] then
    AddCoordinateEdits(Context, BindingIndex, Native, SceneObject, Source, Edits);

  if SceneObject.Kind = tsoCircle then
  begin
    if not (Native is TCircle2D) then
      raise EWriteError.Create('Circle source binding was replaced by a different native type');
    RadiusX := PointDistance2D(Native.Points[0], Native.Points[1]);
    if not NearValue(RadiusX, SceneObject.RadiusX) then
      AddScalarEdit(tdRadius, RadiusX);
  end
  else if SceneObject.Kind = tsoEllipse then
  begin
    if not (Native is TEllipse2D) then
      raise EWriteError.Create('Ellipse source binding was replaced by a different native type');
    Ellipse := Native as TEllipse2D;
    Ellipse.GetEllipseParams(CenterPoint, NativeRadiusX, NativeRadiusY,
      NativeAngle);
    RadiusX := NativeRadiusX;
    RadiusY := NativeRadiusY;
    if not NearValue(RadiusX, SceneObject.RadiusX) then
      AddScalarEdit(tdRadius, RadiusX);
    if not NearValue(NativeAngle, SceneObject.Rotation) then
      AddScalarEdit(tdRotation, NativeAngle);
    { The second radius has its own dependency with PointSlot=2. }
    DepIndex := -1;
    for I := 0 to High(Binding.Dependencies) do
      if (Binding.Dependencies[I].Kind = tdRadius) and
        (Binding.Dependencies[I].PointSlot = 2) then DepIndex := I;
    if not NearValue(RadiusY, SceneObject.RadiusY) then
    begin
      if DepIndex < 0 then
        raise EWriteError.Create('Changed ellipse height has no TikZ dependency');
      Dep := Binding.Dependencies[DepIndex];
      if Dep.DependencySource <> tdepLocalLiteral then
        raise EWriteError.Create('Changed ellipse height depends on a shared expression');
      Value := FormatTikZReal(RadiusY / Dep.ValueScaleToMM,
        SizeOf(TRealType)) + Dep.ValueUnit;
      AddBoundEdit(Edits, BindingIndex, DepIndex, RawByteString(Value));
    end;
  end
  else if SceneObject.Kind in [tsoArc, tsoSector, tsoSegment] then
  begin
    if not (Native is TCircular2D) then
      raise EWriteError.Create('Arc source binding was replaced by a different native type');
    Circular := Native as TCircular2D;
    RadiusX := Circular.Radius;
    StartAngle := Circular.StartAngle - SceneObject.Rotation;
    EndAngle := Circular.EndAngle - SceneObject.Rotation;
    if not NearValue(RadiusX, SceneObject.RadiusX) then
      AddScalarEdit(tdRadius, RadiusX);
    if not NearValue(StartAngle, SceneObject.StartAngle - SceneObject.Rotation) then
    begin
      DepIndex := FindDependency(Binding, tdStartAngle);
      if DepIndex < 0 then raise EWriteError.Create('Arc start angle has no TikZ dependency');
      AddScalarEdit(tdStartAngle, StartAngle);
    end;
    if not NearValue(EndAngle, SceneObject.EndAngle - SceneObject.Rotation) then
    begin
      DepIndex := FindDependency(Binding, tdEndAngle);
      if DepIndex < 0 then raise EWriteError.Create('Arc end angle has no TikZ dependency');
      AddScalarEdit(tdEndAngle, EndAngle);
    end;
    if (Native.Points.Count > 0) and
      (not NearValue(Native.Points[0].X, SceneObject.Center.X) or
       not NearValue(Native.Points[0].Y, SceneObject.Center.Y)) then
      raise EWriteError.Create('Arc center edits require an unsupported source coordinate transform');
  end
  else if SceneObject.Kind = tsoImage then
  begin
    if not (Native is TBitmap2D) or (Native.Points.Count < 2) then
      raise EWriteError.Create('Image source binding was replaced by a different native type');
    Bitmap := Native as TBitmap2D;
    CurrentWidth := Bitmap.Points[2].X / Bitmap.Points[2].W -
      Bitmap.Points[0].X / Bitmap.Points[0].W;
    CurrentHeight := Bitmap.Points[2].Y / Bitmap.Points[2].W -
      Bitmap.Points[0].Y / Bitmap.Points[0].W;
    if not NearValue(CurrentWidth, SceneObject.WidthMM) then
      AddScalarEdit(tdWidth, CurrentWidth);
    if not NearValue(CurrentHeight, SceneObject.HeightMM) then
      AddScalarEdit(tdHeight, CurrentHeight);
  end;

  if (SceneObject.Kind <> tsoImage) and (SceneObject.Kind <> tsoText) then
  begin
  DepIndex := FindDependency(Binding, tdLineWidth);
  if Native.LineStyle <> liNone then
  begin
    if not SceneObject.StrokeEnabled then
      raise EWriteError.Create('Adding a stroke to an unpainted TikZ object is not supported');
    if Drawing.LineWidthBase <= 0 then
      raise EWriteError.Create('Drawing has no valid line-width base');
    PhysicalWidth := Native.LineWidth * Drawing.LineWidthBase;
    if not NearValue(PhysicalWidth, SceneObject.LineWidthMM) then
    begin
      if DepIndex < 0 then
        raise EWriteError.Create('Changed line width has no local TikZ dependency');
      Value := FormatTikZReal(PhysicalWidth, SizeOf(TRealType)) + 'mm';
      AddPropertyEdit(Context, BindingIndex, DepIndex, RawByteString(Value),
        'line width', Edits);
    end;
  end
  else if SceneObject.StrokeEnabled then
    raise EWriteError.Create('Removing a TikZ stroke is not supported');
  if not SceneObject.StrokeEnabled and (Native.LineStyle <> liNone) then
    raise EWriteError.Create('Adding a stroke to an unpainted TikZ object is not supported');

  if SceneObject.StrokeEnabled then
  begin
    RGB := ColorToRGB24(Native.LineColor);
    if RGB <> (SceneObject.StrokeRGB and $FFFFFF) then
    begin
      if not RGBColorName(RGB, Name) then
        raise EWriteError.Create('This edited stroke color has no safe local TikZ color name');
      DepIndex := FindDependency(Binding, tdStrokeColor);
      if DepIndex < 0 then
        raise EWriteError.Create('Changed stroke color has no TikZ source dependency');
      AddPropertyEdit(Context, BindingIndex, DepIndex, RawByteString(Name),
        'draw', Edits);
    end;
  end;

  if SceneObject.FillEnabled then
  begin
    if Native.FillColor = clDefault then
      raise EWriteError.Create('Removing a TikZ fill is not supported');
    RGB := ColorToRGB24(Native.FillColor);
    if RGB <> (SceneObject.FillRGB and $FFFFFF) then
    begin
      if not RGBColorName(RGB, Name) then
        raise EWriteError.Create('This edited fill color has no safe local TikZ color name');
      DepIndex := FindDependency(Binding, tdFillColor);
      if DepIndex < 0 then
        raise EWriteError.Create('Changed fill color has no TikZ source dependency');
      AddPropertyEdit(Context, BindingIndex, DepIndex, RawByteString(Name),
        'fill', Edits);
    end;
  end
  else if Native.FillColor <> clDefault then
    raise EWriteError.Create('Adding a fill to an unfilled TikZ object is not supported');

  if ((SceneObject.LineKind = tlSolid) and (Native.LineStyle <> liSolid)) or
    ((SceneObject.LineKind = tlDashed) and (Native.LineStyle <> liDashed)) or
    ((SceneObject.LineKind = tlDotted) and (Native.LineStyle <> liDotted)) then
    raise EWriteError.Create('Changing the line pattern is not supported by this source codec');
  end;

  if SceneObject.Kind = tsoText then
  begin
    if not (Native is TText2D) then
      raise EWriteError.Create('Text source binding was replaced by a different native object type');
    TextObject := Native as TText2D;
    if not NearValue(TextObject.Height, SceneObject.TextHeightMM) then
    begin
      if (TextObject.Height <= 0) or (SceneObject.TextHeightMM <= 0) or
        (SceneObject.TextBaselineMM <= 0) then
        raise EWriteError.Create('Changed text size has no positive source baseline');
      SizeRatio := TextObject.Height / SceneObject.TextHeightMM;
      NewBaseline := SceneObject.TextBaselineMM * SizeRatio;
      TextSizeBaseMM := Context.Scene.TextSizeMM;
      DepIndex := FindDependency(Binding, tdTextSize);
      if DepIndex < 0 then
        raise EWriteError.Create('Changed text height has no TikZ font-size dependency');
      Dep := Binding.Dependencies[DepIndex];
      if not Dep.HasLocalOverride then
        raise EWriteError.Create('TikZ font-size dependency has no object-local edit');
      if Dep.DependencySource = tdepSharedDefaultMacro then
      begin
        if (Dep.DependencyName = '') or (TextSizeBaseMM <= 0) then
          raise EWriteError.Create('Shared TikZ font-size macro has no local multiplier');
        Value := FormatTikZReal(TextObject.Height / TextSizeBaseMM,
          SizeOf(TRealType)) + '*' + Dep.DependencyName;
      end
      else if Dep.DependencySource = tdepLocalLiteral then
        Value := FormatTikZReal(TextObject.Height, SizeOf(TRealType)) + 'mm'
      else
        raise EWriteError.Create('TikZ font-size dependency cannot be edited locally');
      AddBoundEdit(Edits, BindingIndex, DepIndex, RawByteString(Value), '', True);

      DepIndex := FindDependency(Binding, tdTextBaseline);
      if DepIndex < 0 then
        raise EWriteError.Create('Changed text height has no TikZ baseline dependency');
      Dep := Binding.Dependencies[DepIndex];
      if not Dep.HasLocalOverride then
        raise EWriteError.Create('TikZ baseline dependency has no object-local edit');
      if Dep.DependencySource = tdepSharedDefaultMacro then
      begin
        if (Dep.DependencyName = '') or (TextSizeBaseMM <= 0) then
          raise EWriteError.Create('Shared TikZ baseline macro has no local multiplier');
        Value := FormatTikZReal(NewBaseline / TextSizeBaseMM,
          SizeOf(TRealType)) + '*' + Dep.DependencyName;
      end
      else if Dep.DependencySource = tdepLocalLiteral then
        Value := FormatTikZReal(NewBaseline, SizeOf(TRealType)) + 'mm'
      else
        raise EWriteError.Create('TikZ baseline dependency cannot be edited locally');
      AddBoundEdit(Edits, BindingIndex, DepIndex, RawByteString(Value), '', True);
    end;
    if not NearValue(TextObject.Rot, SceneObject.Rotation) then
    begin
      DepIndex := FindDependency(Binding, tdTextRotation);
      if DepIndex < 0 then
        raise EWriteError.Create('Changed text rotation has no TikZ source dependency');
      Dep := Binding.Dependencies[DepIndex];
      if Dep.ValueScaleToMM = 0 then
        raise EWriteError.Create('Text rotation has no invertible source unit');
      Value := FormatTikZReal(TextObject.Rot / Dep.ValueScaleToMM,
        SizeOf(TRealType)) + Dep.ValueUnit;
      AddPropertyEdit(Context, BindingIndex, DepIndex, RawByteString(Value),
        'rotate', Edits);
    end;
    RGB := ColorToRGB24(TextObject.LineColor);
    if RGB <> (SceneObject.StrokeRGB and $FFFFFF) then
    begin
      if not RGBColorName(RGB, Name) then
        raise EWriteError.Create('This edited text color has no safe local TikZ color name');
      DepIndex := FindDependency(Binding, tdTextColor);
      if DepIndex < 0 then
        raise EWriteError.Create('Changed text color has no TikZ source dependency');
      AddPropertyEdit(Context, BindingIndex, DepIndex, RawByteString(Name),
        'text', Edits);
    end;
    if TextObject.TeXText <> '' then
      BodyChanged := RawByteString(TextObject.TeXText) <>
        SceneObject.TextContent
    else
    begin
      BodyChanged := RawByteString(TextObject.Text) <>
        SceneObject.TextContent;
      if not BodyChanged then
        BodyChanged := (Pos('$', string(SceneObject.TextContent)) > 0) or
          (Pos('\', string(SceneObject.TextContent)) > 0);
    end;
    if BodyChanged then
    begin
      DepIndex := FindDependency(Binding, tdTextBody);
      if DepIndex < 0 then
        raise EWriteError.Create('Changed text body has no TikZ source dependency');
      Body := string(NativeTextBody(TextObject));
      AddBoundEdit(Edits, BindingIndex, DepIndex, RawByteString(Body));
    end;
  end;
end;

function ColorOption(Primitive: TPrimitive2D; Drawing: TDrawing2D;
  out Options: string): Boolean;
var Name: string; WidthMM: Extended;
begin
  Result := False;
  Options := '';
  if (Primitive.Hatching <> haNone) or
    (Primitive.BeginArrowKind <> arrNone) or
    (Primitive.EndArrowKind <> arrNone) then Exit;
  if Primitive is TText2D then
  begin
    if Primitive.FillColor <> clDefault then Exit;
    if Primitive.LineColor <> clDefault then
    begin
      if not RGBColorName(ColorToRGB24(Primitive.LineColor), Name) then Exit;
      Options := 'text=' + Name;
    end;
    Exit(True);
  end;
  if Primitive.LineStyle <> liNone then
  begin
    if Drawing.LineWidthBase <= 0 then Exit;
    WidthMM := Primitive.LineWidth * Drawing.LineWidthBase;
    Options := 'line width=' + FormatTikZReal(WidthMM, SizeOf(TRealType)) + 'mm';
    if Primitive.LineColor = clDefault then Name := 'black'
    else if not RGBColorName(ColorToRGB24(Primitive.LineColor), Name) then Exit;
    Options := Options + ', draw=' + Name;
    case Primitive.LineStyle of
      liDashed: Options := Options + ', dashed';
      liDotted: Options := Options + ', dotted';
      liSolid: ;
    else Exit;
    end;
  end;
  if Primitive.FillColor <> clDefault then
  begin
    if not RGBColorName(ColorToRGB24(Primitive.FillColor), Name) then Exit;
    if Options <> '' then Options := Options + ', ';
    Options := Options + 'fill=' + Name;
  end;
  Result := True;
end;

procedure AppendTikZOption(var Options: string; const Value: string);
begin
  if Options <> '' then Options := Options + ', ';
  Options := Options + Value;
end;

function PointText(const P: TPoint2D): string;
begin
  if not FinitePoint(P) then
    raise EWriteError.Create('New TikZ object has a non-finite coordinate');
  Result := '(' + FormatTikZReal(P.X / P.W, SizeOf(TRealType)) + 'mm,' +
    FormatTikZReal(P.Y / P.W, SizeOf(TRealType)) + 'mm)';
end;

function SerializeNewPrimitive(Primitive: TPrimitive2D;
  Drawing: TDrawing2D): RawByteString;
var
  Options, Body: string;
  I, N: SizeInt;
  A, B, C: TPoint2D;
  TextObject: TText2D;
  SizeRatio: Extended;
  Bezier: Boolean;
begin
  if not ColorOption(Primitive, Drawing, Options) then
    raise EWriteError.Create('This new object has a style the TikZ source codec cannot represent');
  if Primitive is TText2D then
  begin
    TextObject := Primitive as TText2D;
    if (Drawing.DefaultFontHeight <= 0) or IsNan(Drawing.DefaultFontHeight) or
      IsInfinite(Drawing.DefaultFontHeight) or (TextObject.Height <= 0) or
      IsNan(TextObject.Height) or IsInfinite(TextObject.Height) then
      raise EWriteError.Create('New TikZ text has no valid font size');
    SizeRatio := TextObject.Height / Drawing.DefaultFontHeight;
    Body := string(NativeTextBody(TextObject));
    if TextObject.HAlignment = ahLeft then
      AppendTikZOption(Options, 'anchor=base west')
    else if TextObject.HAlignment = ahRight then
      AppendTikZOption(Options, 'anchor=base east')
    else
      AppendTikZOption(Options, 'anchor=base');
    if not NearValue(TextObject.Rot, 0) then
      AppendTikZOption(Options, 'rotate=' + FormatTikZReal(
        TextObject.Rot * 180 / Pi, SizeOf(TRealType)));
    if Options <> '' then Options := '[' + Options + ']';
    Body := '\pgfmathsetlengthmacro{\tpxObjectFontSize}{' +
      FormatTikZReal(SizeRatio, SizeOf(TRealType)) + '*\tpxTextSize}' +
      '\pgfmathsetlengthmacro{\tpxObjectBaseline}{' +
      FormatTikZReal(SizeRatio * 1.2, SizeOf(TRealType)) +
      '*\tpxTextSize}\fontsize{\tpxObjectFontSize}' +
      '{\tpxObjectBaseline}\selectfont ' + Body;
    Exit(RawByteString('\node' + Options + ' at ' +
      PointText(Primitive.Points[0]) + ' {' + Body + '};'));
  end;
  if Primitive.LineStyle = liNone then
  begin
    if Primitive.FillColor <> clDefault then
      Result := RawByteString('\fill[' + Options + '] ')
    else Result := '\path ';
  end
  else Result := RawByteString('\draw[' + Options + '] ');
  if Primitive is TCircle2D then
  begin
    A := Primitive.Points[0]; B := Primitive.Points[1];
    Result := Result + RawByteString(PointText(A) + ' circle (' +
      FormatTikZReal(PointDistance2D(A, B), SizeOf(TRealType)) + 'mm);');
    Exit;
  end;
  if Primitive is TRectangle2D then
  begin
    A := Primitive.Points[0]; B := Primitive.Points[1]; C := Primitive.Points[2];
    if not (FinitePoint(A) and FinitePoint(B) and FinitePoint(C)) then
      raise EWriteError.Create('New TikZ rectangle has non-finite corners');
    A := Point2D(A.X / A.W, A.Y / A.W);
    B := Point2D(B.X / B.W, B.Y / B.W);
    C := Point2D(C.X / C.W, C.Y / C.W);
    if not (NearValue(A.X / A.W, C.X / C.W) and
      NearValue(B.Y / B.W, C.Y / C.W)) then
      raise EWriteError.Create('Rotated rectangles cannot be inserted into this TikZ source');
    Result := Result + RawByteString(PointText(A) + ' rectangle ' +
      PointText(B) + ';');
    Exit;
  end;
  if not (Primitive is TPolyline2D0) and
    not (Primitive is TBezierPath2D) and
    not (Primitive is TClosedBezierPath2D) and
    not (Primitive is TLine2D) then
    raise EWriteError.CreateFmt(
      'This new native object type has no TikZ source serializer (%s)',
      [Primitive.ClassName]);
  N := Primitive.Points.Count;
  if (N < 2) then
    raise EWriteError.Create('New TikZ paths require at least two control points');
  if Primitive is TLine2D then N := 2;
  Bezier := (Primitive is TBezierPath2D) or (Primitive is TClosedBezierPath2D);
  if Bezier and (((N - 1) mod 3) <> 0) then
    raise EWriteError.Create('New Bezier path has an incomplete control-point group');
  Result := Result + RawByteString(PointText(Primitive.Points[0]));
  I := 1;
  while I < N do
  begin
    if Bezier then
    begin
      A := Primitive.Points[I]; B := Primitive.Points[I + 1];
      C := Primitive.Points[I + 2];
      Result := Result + RawByteString(' .. controls ' + PointText(A) +
        ' and ' + PointText(B) + ' .. ' + PointText(C));
      Inc(I, 3);
    end
    else
    begin
      Result := Result + RawByteString(' -- ' + PointText(Primitive.Points[I]));
      Inc(I);
    end;
  end;
  if (Primitive is TPolygon2D) or (Primitive is TClosedBezierPath2D) then
    Result := Result + ' -- cycle';
  Result := Result + ';';
end;

function SerializeNewObject(Obj: TObject2D; Drawing: TDrawing2D): RawByteString;
begin
  if not (Obj is TPrimitive2D) then
    raise EWriteError.CreateFmt(
      'Only standalone primitive objects can be inserted into a TikZ picture (%s)',
      [Obj.ClassName]);
  Result := SerializeNewPrimitive(TPrimitive2D(Obj), Drawing);
end;

function SerializeNewDocument(Drawing: TDrawing2D;
  const FileName: TDocumentPath): RawByteString;
var
  Obj: TObject2D;
  IsTeXDocument: Boolean;
  EOL, FontDefault: RawByteString;
begin
  if Drawing = nil then
    raise EWriteError.Create('A new TikZ document has no drawing');
  IsTeXDocument := SameText(ExtractFileExt(string(FileName)), '.tex');
  EOL := RawByteString(LineEnding);
  if (Drawing.DefaultFontHeight <= 0) or IsNan(Drawing.DefaultFontHeight) or
    IsInfinite(Drawing.DefaultFontHeight) then
    raise EWriteError.Create('New TikZ document has no valid default text size');
  FontDefault := RawByteString('\providecommand{\tpxTextSize}{' +
    FormatTikZReal(Drawing.DefaultFontHeight * 72.27 / 25.4,
      SizeOf(TRealType)) + 'pt}');
  if IsTeXDocument then
    Result := '\documentclass{standalone}' + EOL +
      '\usepackage{tikz}' + EOL +
      FontDefault + EOL +
      '\begin{document}' + EOL +
      '\begin{tikzpicture}[x=1mm,y=1mm]' + EOL
  else
    Result := FontDefault + EOL +
      '\begin{tikzpicture}[x=1mm,y=1mm]' + EOL;
  Obj := Drawing.ObjectList.FirstObj as TObject2D;
  while Obj <> nil do
  begin
    Result := Result + SerializeNewObject(Obj, Drawing) + EOL;
    Obj := Drawing.ObjectList.NextObj as TObject2D;
  end;
  Result := Result + '\end{tikzpicture}' + EOL;
  if IsTeXDocument then Result := Result + '\end{document}' + EOL;
end;

function BuildSourceEdits(Drawing: TDrawing2D; Context: TTikZImportContext;
  const Source: TBytes; out Updated: TBytes): Boolean;
var
  Bindings: TTikZSourceBindings;
  Edits: TEditArray;
  PropertyPatches, Patches: TPatchArray;
  DeletedSpans: TSpanArray;
  Inserted: RawByteString;
  Obj: TObject2D;
  Primitive: TPrimitive2D;
  Binding, Other: TTikZSourceBinding;
  I, J, K, BindingIndex, PreviousIndex, EditsBefore: SizeInt;
  SeenNewObject, GeometryChanged: Boolean;
  Region: TTikZSourceSpan;
  Error: TTikZPatchError;
  EOL: RawByteString;
  Extent: TRect2D;
  Replacement: RawByteString;
  Syntax: TTikZSyntaxResult;
  Semantic: TTikZSemanticResult;
  ParseOptions: TTikZParseOptions;
  Candidate: TBytes;
  SortPatch: TTikZSourcePatch;
  function EditAffectsBounds(const Edit: TTikZBoundEdit): Boolean;
  var B: TTikZSourceBinding; Kind: TTikZDependencyKind;
  begin
    B := Context.BindingAt(Edit.BindingIndex);
    Kind := B.Dependencies[Edit.DependencyIndex].Kind;
    Result := Kind in [tdCoordinateX, tdCoordinateY, tdWidth, tdHeight,
      tdRadius, tdRotation, tdTextRotation, tdStartAngle, tdEndAngle,
      tdTextSize, tdLineWidth, tdTextBody];
  end;
begin
  Result := False;
  Bindings := nil;
  Edits := nil;
  PropertyPatches := nil;
  Patches := nil;
  DeletedSpans := nil;
  Candidate := nil;
  Updated := nil;
  if (Drawing = nil) or (Context = nil) or (Context.Document = nil) or
    (Context.Scene = nil) then
    raise EWriteError.Create('TikZ save-back requires its accepted document context');
  Region := Context.Document.Region.Span;
  SetLength(Bindings, Context.BindingCount);
  for I := 0 to Context.BindingCount - 1 do Bindings[I] := Context.BindingAt(I);
  PreviousIndex := -1;
  SeenNewObject := False;
  GeometryChanged := False;
  Inserted := '';
  EOL := EOLBytes(Source);

  Obj := Drawing.ObjectList.FirstObj as TObject2D;
  while Obj <> nil do
  begin
    BindingIndex := Context.FindBindingForObjectID(Obj.ID);
    if BindingIndex < 0 then
    begin
      SeenNewObject := True;
      GeometryChanged := True;
      if not SupportsTikZStructuralOperation(tsoInsertStatement) then
        raise EWriteError.Create('TikZ statement insertion is not supported');
      Replacement := SerializeNewObject(Obj, Drawing);
      if Inserted = '' then
      begin
        if (Context.Document.Region.ContentSpan.EndByte > 0) and
          (Context.Document.Region.ContentSpan.EndByte <= Length(Source)) and
          not IsLineBreak(Source[Context.Document.Region.ContentSpan.EndByte - 1]) then
          Inserted := EOL;
      end;
      Inserted := Inserted + Replacement + EOL;
    end
    else
    begin
      if SeenNewObject then
        raise EWriteError.Create('New TikZ objects can only be appended; current order would reorder source statements');
      Binding := Context.BindingAt(BindingIndex);
      if Binding.ObjectIndex < PreviousIndex then
        raise EWriteError.Create('Reordered TikZ objects cannot be saved without changing draw order');
      PreviousIndex := Binding.ObjectIndex;
      if not (Obj is TPrimitive2D) then
        raise EWriteError.Create('A bound TikZ object was replaced by a group');
      Primitive := TPrimitive2D(Obj);
      EditsBefore := Length(Edits);
      AddNativeChanges(Context, BindingIndex, Primitive, Drawing, Source, Edits);
      for K := EditsBefore to High(Edits) do
        if EditAffectsBounds(Edits[K]) then GeometryChanged := True;
    end;
    Obj := Drawing.ObjectList.NextObj as TObject2D;
  end;

  { One source command can create several native subpaths. Removing only some
    of them cannot be represented by deleting that command. }
  for I := 0 to Context.BindingCount - 1 do
  begin
    Binding := Context.BindingAt(I);
    if Binding.NativeObjectID < 0 then
      raise EWriteError.Create('TikZ source context contains an unbound native object');
    if Drawing.GetObject(Binding.NativeObjectID) <> nil then Continue;
    GeometryChanged := True;
    if not SupportsTikZStructuralOperation(tsoDeleteStatement) then
      raise EWriteError.Create('TikZ statement deletion is not supported');
    for J := 0 to Context.BindingCount - 1 do
    begin
      Other := Context.BindingAt(J);
      if SameSpan(Other.StatementSpan, Binding.StatementSpan) and
        (Drawing.GetObject(Other.NativeObjectID) <> nil) then
        raise EWriteError.Create('A TikZ statement producing multiple objects cannot be partially deleted');
    end;
    if not SpanAlreadyPresent(DeletedSpans, Binding.StatementSpan) then
    begin
      AddSpan(DeletedSpans, Binding.StatementSpan);
      AddPatch(Patches, Binding.StatementSpan, '');
    end;
  end;

  if not BuildTikZPropertyPatches(Source, Region, Bindings, Edits,
    PropertyPatches, Error) then
    raise EWriteError.CreateFmt('TikZ source patch could not be built (error %d)',
      [Ord(Error)]);
  for I := 0 to High(PropertyPatches) do
    AddPatch(Patches, PropertyPatches[I].Span,
      RawOf(PropertyPatches[I].Replacement));

  if Inserted <> '' then
  begin
    Binding.StatementSpan := Context.Document.Region.ContentSpan;
    Binding.StatementSpan.StartByte := Binding.StatementSpan.EndByte;
    AddPatch(Patches, Binding.StatementSpan, Inserted);
  end;

  if GeometryChanged and Context.Scene.HasOwnedBounds then
  begin
    Extent := Drawing.DrawingExtension;
    if IsNan(Extent.Left) or IsNan(Extent.Bottom) or IsNan(Extent.Right) or
      IsNan(Extent.Top) or IsInfinite(Extent.Left) or IsInfinite(Extent.Bottom) or
      IsInfinite(Extent.Right) or IsInfinite(Extent.Top) then
      raise EWriteError.Create('Updated drawing bounds are not finite');
    Replacement := '\path[line width=0mm] (' +
      FormatTikZReal(Extent.Left, SizeOf(TRealType)) + 'mm,' +
      FormatTikZReal(Extent.Bottom, SizeOf(TRealType)) + 'mm) rectangle +(' +
      FormatTikZReal(Extent.Right - Extent.Left, SizeOf(TRealType)) + 'mm,' +
      FormatTikZReal(Extent.Top - Extent.Bottom, SizeOf(TRealType)) + 'mm);';
    AddPatch(Patches, Context.Scene.OwnedBoundsSpan, Replacement);
  end;

  for I := 1 to High(Patches) do
  begin
    SortPatch := Patches[I];
    J := I - 1;
    while (J >= 0) and (Patches[J].Span.StartByte > SortPatch.Span.StartByte) do
    begin
      Patches[J + 1] := Patches[J];
      Dec(J);
    end;
    Patches[J + 1] := SortPatch;
  end;
  if not ApplyTikZSourcePatches(Source, Region, Patches, Candidate, Error) then
    raise EWriteError.CreateFmt('TikZ source edits overlap or leave the picture (error %d)',
      [Ord(Error)]);

  if Context.Document.Region.IsFragment then
    ParseOptions := DefaultTikZParseOptions(tikFragmentInput)
  else ParseOptions := DefaultTikZParseOptions(tikTexInput);
  Syntax := ParseTikZ(Candidate, ParseOptions);
  try
    if (Syntax.Outcome <> tpoAccepted) or not Syntax.Region.Found then
      raise EWriteError.Create('Edited TikZ source failed syntax validation');
    Semantic := EvaluateTikZ(Syntax, DefaultTikZImportOptions);
    try
      if Semantic.Outcome <> tsoAccepted then
        raise EWriteError.Create('Edited TikZ source failed semantic validation');
    finally
      Semantic.Free;
    end;
  finally
    Syntax.Free;
  end;
  Updated := Candidate;
  Result := True;
end;

function TikZCodecLoader(Candidate: TDrawing2D;
  const FileName: TDocumentPath; const SourceBytes: RawByteString;
  Diagnostics: TStrings; out CodecContext: TObject): Boolean;
begin
  Result := TikZDocumentLoader(Candidate, string(FileName), SourceBytes,
    Diagnostics, CodecContext);
end;

procedure RebindTikZDocumentContext(LiveDrawing, ParsedDrawing: TDrawing2D;
  CodecContext: TObject);
var
  Context: TTikZImportContext;
  LiveObject, ParsedObject: TObject2D;
  I: SizeInt;
  Binding: TTikZSourceBinding;
begin
  if not (CodecContext is TTikZImportContext) then
    raise EWriteError.Create('TikZ save-back could not rebind its parsed context');
  if (LiveDrawing = nil) or (ParsedDrawing = nil) then
    raise EWriteError.Create('TikZ save-back has no live or parsed drawing');
  Context := CodecContext as TTikZImportContext;
  if Context.BindingCount <> Context.Scene.ObjectCount then
    raise EWriteError.Create('TikZ save-back has incomplete source bindings');

  LiveObject := LiveDrawing.ObjectList.FirstObj as TObject2D;
  ParsedObject := ParsedDrawing.ObjectList.FirstObj as TObject2D;
  for I := 0 to Context.BindingCount - 1 do
  begin
    if (LiveObject = nil) or (ParsedObject = nil) then
      raise EWriteError.Create('TikZ save-back changed the bound object count');
    Binding := Context.BindingAt(I);
    if Binding.ObjectIndex <> I then
      raise EWriteError.Create('TikZ source bindings are not in object order');
    if ParsedDrawing.GetObject(Binding.NativeObjectID) <> ParsedObject then
      raise EWriteError.Create('TikZ parsed object order no longer matches its source bindings');
    Context.BindNativeObject(I, LiveObject.ID);
    LiveObject := LiveDrawing.ObjectList.NextObj as TObject2D;
    ParsedObject := ParsedDrawing.ObjectList.NextObj as TObject2D;
  end;
  if (LiveObject <> nil) or (ParsedObject <> nil) then
    raise EWriteError.Create('TikZ save-back changed the bound object count');
end;

function SaveTikZDocument(Drawing: TDrawing2D;
  const FileName: TDocumentPath; Destination: TStream;
  CodecContext: TObject): Boolean;
var
  Source, Updated: TBytes;
  Context: TTikZImportContext;
  NewDocument: RawByteString;
begin
  Result := False;
  if CodecContext = nil then
  begin
    NewDocument := SerializeNewDocument(Drawing, FileName);
    if Length(NewDocument) > 0 then
      Destination.WriteBuffer(NewDocument[1], Length(NewDocument));
    Exit(True);
  end;
  if not (CodecContext is TTikZImportContext) then
    raise EWriteError.Create('TikZ save-back context has an unsupported type');
  Context := CodecContext as TTikZImportContext;
  Source := Context.CopySource;
  if not BuildSourceEdits(Drawing, Context, Source, Updated) then Exit;
  if Length(Updated) > 0 then Destination.WriteBuffer(Updated[0], Length(Updated));
  Result := True;
end;

procedure RegisterTikZDocumentCodec;
var F: TDocumentFormat;
begin
  F.Id := 'tikz';
  F.DisplayName := 'TikZ source';
  F.Extensions := '.tikz;.tex';
  F.CanOpen := True;
  F.CanSaveBack := True;
  F.RoundTripProfile := 'Supported drawing semantics and original source text';
  F.RuntimeRequirements := '';
  RegisterDocumentCodec(F, @TikZCodecLoader, @SaveTikZDocument,
    @RebindTikZDocumentContext, dsvpPrivateStage);
end;

end.
