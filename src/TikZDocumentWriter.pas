unit TikZDocumentWriter;

{$mode objfpc}{$H+}

interface

uses DocumentFormats, Drawings;

procedure RegisterTikZDocumentCodec;
function GenerateTikZPictureFragment(Drawing: TDrawing2D): RawByteString;

implementation

uses Classes, SysUtils, Math, Graphics, Geometry, GObjects,
  Devices, GObjBase, TikZLexer, TikZSyntax, TikZImport, TikZImportDrawing,
  TikZFading,
  TikZWriter, DocumentIO, Pieces, Solid3D, TpXSaver;

type
  TEditArray = array of TTikZBoundEdit;
  TPatchArray = array of TTikZSourcePatch;
  TSpanArray = array of TTikZSourceSpan;
  TDefinitionArray = array of string;
  TStatementPart = (spWhole, spFill, spStroke);

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

{ Add a style patch. An insertion at the start of another patch is folded
  into that patch so both edits can apply to the same option position. }
procedure MergePatch(var Patches: TPatchArray; const Patch: TTikZSourcePatch);
var I: SizeInt;
begin
  for I := 0 to High(Patches) do
    if Patches[I].Span.StartByte = Patch.Span.StartByte then
    begin
      if Patch.Span.EndByte = Patch.Span.StartByte then
        Patches[I].Replacement := BytesOf(RawOf(Patch.Replacement) +
          RawOf(Patches[I].Replacement))
      else if Patches[I].Span.EndByte = Patches[I].Span.StartByte then
      begin
        Patches[I].Replacement := BytesOf(RawOf(Patches[I].Replacement) +
          RawOf(Patch.Replacement));
        Patches[I].Span := Patch.Span;
      end
      else
        raise EWriteError.Create('Two TikZ edits replace the same source option');
      Exit;
    end;
  AddPatch(Patches, Patch.Span, RawOf(Patch.Replacement));
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

{ Named colors stay readable; any other RGB value gets a local rgb color
  definition that the importer resolves back to the same 8-bit color. }
function TikZColor(Value: LongWord; var Definitions: TDefinitionArray): string;
var Definition: string; I: SizeInt;
  function Component(Shift: Integer): string;
  begin
    Result := FormatTikZReal(((Value shr Shift) and $FF) / 255, 4);
  end;
begin
  if RGBColorName(Value, Result) then Exit;
  Result := 'tpx' + IntToHex(Value and $FFFFFF, 6);
  Definition := '\definecolor{' + Result + '}{rgb}{' + Component(16) + ',' +
    Component(8) + ',' + Component(0) + '}';
  for I := 0 to High(Definitions) do
    if Definitions[I] = Definition then Exit;
  I := Length(Definitions);
  SetLength(Definitions, I + 1);
  Definitions[I] := Definition;
end;

function ArrowName(Kind: TArrowKind): string;
begin
  Result := string(ArrowsIDs[Ord(Kind)]);
end;

{ The TikZ arrow specification for native arrowheads, '' without arrows. }
function ArrowSpec(Primitive: TPrimitive2D; out Spec: string): string;
var BeginTip, EndTip: string;
begin
  Result := '';
  Spec := '';
  if (Primitive.BeginArrowKind = arrNone) and
    (Primitive.EndArrowKind = arrNone) then Exit;
  if not ArrowKindTikZTip(Primitive.BeginArrowKind, BeginTip) then
    Exit('start arrow ' + ArrowName(Primitive.BeginArrowKind) +
      ' has no TikZ arrow tip (supported: h40, h42, h44)');
  if not ArrowKindTikZTip(Primitive.EndArrowKind, EndTip) then
    Exit('end arrow ' + ArrowName(Primitive.EndArrowKind) +
      ' has no TikZ arrow tip (supported: h40, h42, h44)');
  if not NearValue(Primitive.ArrowSizeFactor, 1) then
    Exit('arrow size multiplier ' + FormatTikZReal(Primitive.ArrowSizeFactor,
      SizeOf(TRealType)) + ' has no TikZ source form (only 1 is supported)');
  if BeginTip = 'to' then BeginTip := '<';
  if EndTip = 'to' then EndTip := '>';
  Spec := BeginTip + '-' + EndTip;
end;

function LineStyleKeyword(Style: TLineStyle): string;
begin
  case Style of
    liSolid: Result := 'solid';
    liDashed: Result := 'dashed';
    liDotted: Result := 'dotted';
  else Result := '';
  end;
end;

{ Replace an object-local style keyword (line pattern or arrow tips) in
  place, or insert it at the start of the statement's option list. }
procedure AddStyleKeyword(Context: TTikZImportContext; BindingIndex: SizeInt;
  Kind: TTikZDependencyKind; const Keyword, PropertyName: string;
  const Source: TBytes; var Insertion: string; var Patches: TPatchArray);
var
  Binding: TTikZSourceBinding;
  Dep: TTikZPropertyDependency;
  Span: TTikZSourceSpan;
  I, DepIndex: SizeInt;
begin
  Binding := Context.BindingAt(BindingIndex);
  for I := 0 to Context.BindingCount - 1 do
    if (I <> BindingIndex) and SameSpan(Context.BindingAt(I).StatementSpan,
      Binding.StatementSpan) then
      raise EWriteError.Create('the ' + PropertyName +
        ' is shared by multiple paths in one TikZ statement');
  DepIndex := FindDependency(Binding, Kind);
  if DepIndex < 0 then
  begin
    if Insertion <> '' then Insertion := Insertion + ', ';
    Insertion := Insertion + Keyword;
    Exit;
  end;
  Dep := Binding.Dependencies[DepIndex];
  Span := Dep.PropertyKeySpan;
  if Dep.ValueSpan.EndByte > Span.EndByte then
    Span.EndByte := Dep.ValueSpan.EndByte;
  if (Span.StartByte < Binding.StatementSpan.StartByte) or
    (Span.EndByte > Binding.StatementSpan.EndByte) or
    (Span.EndByte <= Span.StartByte) or (Span.EndByte > Length(Source)) then
    raise EWriteError.Create('the ' + PropertyName +
      ' comes from an enclosing TikZ style and cannot be edited locally');
  AddPatch(Patches, Span, RawByteString(Keyword));
end;

{ Insert style keywords as the first options of a statement, creating the
  option list when the command has none. }
procedure AddStyleInsertion(Context: TTikZImportContext; BindingIndex: SizeInt;
  const Source: TBytes; const Insertion: string; var Patches: TPatchArray);
var
  Binding: TTikZSourceBinding;
  Span: TTikZSourceSpan;
  P, Q, Limit: SizeInt;
begin
  if Insertion = '' then Exit;
  Binding := Context.BindingAt(BindingIndex);
  P := Binding.StatementSpan.StartByte;
  Limit := Binding.StatementSpan.EndByte;
  if (P < 0) or (Limit > Length(Source)) or (P >= Limit) or
    (Source[P] <> Ord('\')) then
    raise EWriteError.Create('the TikZ statement has no command for a new style option');
  Inc(P);
  while (P < Limit) and (Chr(Source[P]) in ['A'..'Z', 'a'..'z']) do Inc(P);
  Q := P;
  while (Q < Limit) and (Source[Q] in [Ord(' '), 9, 10, 13]) do Inc(Q);
  Span := Binding.StatementSpan;
  if (Q < Limit) and (Source[Q] = Ord('[')) then
  begin
    Span.StartByte := Q + 1;
    Span.EndByte := Q + 1;
    AddPatch(Patches, Span, RawByteString(Insertion + ', '));
  end
  else
  begin
    Span.StartByte := P;
    Span.EndByte := P;
    AddPatch(Patches, Span, RawByteString('[' + Insertion + ']'));
  end;
end;

function NativeGradient(Native: TPrimitive2D): TTikZGradientKind;
begin
  if Native.FillColor = clDefault then Result := tgNone
  else
    case Native.FillGradient of
      fgLinear: Result := tgLinear;
      fgRadial: Result := tgRadial;
    else
      Result := tgNone;
    end;
end;

function NativeGradientRGB(Native: TPrimitive2D): LongWord;
begin
  if Native.GradientColor = clDefault then
    Result := ColorToRGB24(Native.FillColor)
  else Result := ColorToRGB24(Native.GradientColor);
end;

{ A gradient whose two ends differ in opacity: TikZ needs a fading for it,
  and the fading would also fade a stroke, which TpX therefore draws as a
  separate path (as T_TikZ_Device does). }
function FadedGradient(Primitive: TPrimitive2D): Boolean;
begin
  Result := (Primitive.FillColor <> clDefault) and
    (Primitive.FillGradient <> fgNone) and
    (Abs(ClampOpacity(Primitive.FillOpacity) -
    ClampOpacity(Primitive.GradientOpacity)) > 0.0005);
end;

function ShortNumber(Value: Extended; Digits: Integer = 6): string;
var
  Settings: TFormatSettings;
begin
  Settings := DefaultFormatSettings;
  Settings.DecimalSeparator := '.';
  Result := FloatToStrF(Value, ffGeneral, Digits, 0, Settings);
end;

{ True when Fading is the declaration the faded gradient of Primitive
  needs: same kind and the opacities TpX writes for it. }
function FadingFits(Primitive: TPrimitive2D;
  const Fading: TTikZFadingDeclaration): Boolean;
begin
  { Names spell out the stops, so the name decides. }
  Result := FadedGradient(Primitive) and
    (Fading.Radial = (Primitive.FillGradient = fgRadial)) and
    (Fading.Name = TikZFadingName(Fading.Radial,
    TikZFadingStopText(Primitive.FillOpacity),
    TikZFadingStopText(Primitive.GradientOpacity)));
end;

{ The fading declaration of a faded gradient, shared with the export. }
function FadingDeclarationText(Primitive: TPrimitive2D;
  out Name: string): string;
begin
  Result := TikZFadingDeclaration(Primitive.FillGradient = fgRadial,
    TikZFadingStopText(Primitive.FillOpacity),
    TikZFadingStopText(Primitive.GradientOpacity), Name);
end;

{ Shading keys of a gradient fill from Start to Finish, laid out as
  TPaintStyle describes. }
function ShadingText(Primitive: TPrimitive2D;
  const Start, Finish: string): string;
var
  Angle: Extended;
begin
  Angle := Primitive.GradientAngle - 360 * Floor(Primitive.GradientAngle / 360);
  if Primitive.FillGradient = fgRadial then
    Result := 'inner color=' + Start + ', outer color=' + Finish
  else if Abs(Angle) < 1E-9 then
    Result := 'left color=' + Start + ', right color=' + Finish
  else
    Result := 'bottom color=' + Start + ', top color=' + Finish +
      ', shading angle=' + FormatTikZReal(Angle - 90, SizeOf(TRealType));
end;

{ The fading keys of a faded gradient using the fading Name. }
function FadingOptionsText(Primitive: TPrimitive2D; const Name: string): string;
var
  Shift: TTikZPoint;
  Rotation, XScale, YScale: Double;
begin
  Result := 'path fading=' + Name;
  if Primitive.FillGradient = fgRadial then
    Exit(Result + ', fit fading=true');
  if not TikZLinearFadingOf(Primitive, Shift, Rotation, XScale, YScale) then
    raise EWriteError.Create('the faded gradient has no area to fill');
  Result := Result + ', fit fading=false, fading transform={shift={(' +
    ShortNumber(RoundTo(Shift.X, -4), 12) + 'mm,' +
    ShortNumber(RoundTo(Shift.Y, -4), 12) + 'mm)}, rotate=' +
    ShortNumber(Rotation) + ', xscale=' + ShortNumber(XScale) +
    ', yscale=' + ShortNumber(YScale) + '}';
end;

function SameGradient(Native: TPrimitive2D;
  SceneObject: TTikZSceneObject): Boolean;
begin
  Result := NativeGradient(Native) = SceneObject.Gradient;
  if not Result or (SceneObject.Gradient = tgNone) then Exit;
  Result := (NativeGradientRGB(Native) = (SceneObject.GradientRGB and $FFFFFF))
    and NearValue(ClampOpacity(Native.GradientOpacity),
    SceneObject.GradientOpacity) and
    ((SceneObject.Gradient = tgRadial) or
    NearValue(Native.GradientAngle - 360 * Floor(Native.GradientAngle / 360),
    SceneObject.GradientAngle));
end;

function OpacityOption(const Key: string; Value: Double): string;
begin
  { Opacities are stored in single precision; four digits keep the
    source short without changing the stored value. }
  Result := Key + '=' + OpacityText(Value);
end;

{ Edited opacities replace their own source option, or an 'opacity'
  option that set both, or are inserted as new options. }
procedure AddOpacityChanges(Context: TTikZImportContext;
  BindingIndex: SizeInt; Native: TPrimitive2D;
  SceneObject: TTikZSceneObject; const Source: TBytes;
  var Insertion: string; var Patches: TPatchArray;
  IncludeStroke, IncludeFill: Boolean; SharedDone: Boolean = False);
var
  Binding: TTikZSourceBinding;
  StrokeChanged, FillChanged, StrokeViaShared, FillViaShared: Boolean;
  Stroke, Fill: Double;
  Keyword: string;
begin
  Binding := Context.BindingAt(BindingIndex);
  StrokeChanged := IncludeStroke and SceneObject.StrokeEnabled and
    not NearValue(Native.LineOpacity, SceneObject.StrokeOpacity);
  FillChanged := IncludeFill and SceneObject.FillEnabled and
    not NearValue(Native.FillOpacity, SceneObject.FillOpacity);
  { A shared option already rewritten to the native stroke opacity. }
  if SharedDone and (FindDependency(Binding, tdStrokeOpacity) < 0) then
    StrokeChanged := False;
  if not (StrokeChanged or FillChanged) then Exit;
  if (Native.LineOpacity < 0) or (Native.LineOpacity > 1) or
    (Native.FillOpacity < 0) or (Native.FillOpacity > 1) then
    raise EWriteError.Create('Opacity must be between 0 and 1');
  StrokeViaShared := StrokeChanged and
    (FindDependency(Binding, tdStrokeOpacity) < 0) and
    (FindDependency(Binding, tdOpacity) >= 0);
  FillViaShared := FillChanged and
    (FindDependency(Binding, tdFillOpacity) < 0) and
    (FindDependency(Binding, tdOpacity) >= 0);
  if StrokeChanged and not StrokeViaShared then
    AddStyleKeyword(Context, BindingIndex, tdStrokeOpacity,
      OpacityOption('draw opacity', Native.LineOpacity), 'draw opacity',
      Source, Insertion, Patches);
  if FillChanged and not FillViaShared then
    AddStyleKeyword(Context, BindingIndex, tdFillOpacity,
      OpacityOption('fill opacity', Native.FillOpacity), 'fill opacity',
      Source, Insertion, Patches);
  if not (StrokeViaShared or FillViaShared) then Exit;
  if StrokeViaShared then Stroke := Native.LineOpacity
  else Stroke := SceneObject.StrokeOpacity;
  if FillViaShared then Fill := Native.FillOpacity
  else Fill := SceneObject.FillOpacity;
  if NearValue(Stroke, Fill) then
    Keyword := OpacityOption('opacity', Fill)
  else
    Keyword := OpacityOption('draw opacity', Stroke) + ', ' +
      OpacityOption('fill opacity', Fill);
  AddStyleKeyword(Context, BindingIndex, tdOpacity, Keyword, 'opacity',
    Source, Insertion, Patches);
end;

{ The bytes of an object-local option (key and value). }
function LocalOptionSpan(const Binding: TTikZSourceBinding;
  const Dep: TTikZPropertyDependency; const What: string): TTikZSourceSpan;
begin
  Result := Dep.PropertyKeySpan;
  if Dep.ValueSpan.EndByte > Result.EndByte then
    Result.EndByte := Dep.ValueSpan.EndByte;
  if (Result.StartByte < Binding.StatementSpan.StartByte) or
    (Result.EndByte > Binding.StatementSpan.EndByte) or
    (Result.EndByte <= Result.StartByte) then
    raise EWriteError.Create('the ' + What +
      ' comes from an enclosing TikZ style and cannot be edited locally');
end;

{ A shared 'opacity' option of a statement whose fill options are rewritten
  keeps only its stroke part, as 'draw opacity' with the native value; the
  fill options then carry the fill opacity. Without a stroke it goes with
  the fill options. Returns whether the option was rewritten here. }
function RewriteSharedOpacity(Context: TTikZImportContext;
  BindingIndex: SizeInt; Native: TPrimitive2D; Stroked: Boolean;
  var Patches: TPatchArray): Boolean;
var
  Binding: TTikZSourceBinding;
  DepIndex: SizeInt;
  Patch: TTikZSourcePatch;
begin
  Binding := Context.BindingAt(BindingIndex);
  DepIndex := FindDependency(Binding, tdOpacity);
  Result := (DepIndex >= 0) and Stroked;
  if not Result then Exit;
  Patch.Span := LocalOptionSpan(Binding, Binding.Dependencies[DepIndex],
    'opacity');
  Patch.Replacement := BytesOf(RawByteString(OpacityOption('draw opacity',
    ClampOpacity(Native.LineOpacity))));
  MergePatch(Patches, Patch);
end;

{ The source name for a colour of an edited fill: the name the statement
  already uses for that colour, or a safe standard name. }
function EditedFillColorName(RGB: LongWord; SceneObject: TTikZSceneObject;
  const Role: string): string;
begin
  if (RGB = (SceneObject.FillRGB and $FFFFFF)) and
    (SceneObject.FillColorName <> '') then Exit(SceneObject.FillColorName);
  if (SceneObject.Gradient <> tgNone) and
    (RGB = (SceneObject.GradientRGB and $FFFFFF)) and
    (SceneObject.GradientColorName <> '') then
    Exit(SceneObject.GradientColorName);
  if not RGBColorName(RGB, Result) then
    raise EWriteError.Create('This edited ' + Role +
      ' color has no safe local TikZ color name');
end;

{ The fill options of an edited native fill, reusing the statement's colour
  names. A faded gradient reuses the fading its statement already names, or
  an earlier declaration of the same fading, or declares it right before the
  statement. }
function EditedFillText(Context: TTikZImportContext; BindingIndex: SizeInt;
  Native: TPrimitive2D; SceneObject: TTikZSceneObject; const Source: TBytes;
  var Patches: TPatchArray): string;
var
  Binding: TTikZSourceBinding;
  Start, Finish, Name, Declaration: string;
  I, Found: SizeInt;
  Patch: TTikZSourcePatch;
begin
  Binding := Context.BindingAt(BindingIndex);
  Start := EditedFillColorName(ColorToRGB24(Native.FillColor), SceneObject,
    'fill');
  if Native.FillGradient = fgNone then
    Result := 'fill=' + Start
  else
  begin
    Finish := EditedFillColorName(NativeGradientRGB(Native), SceneObject,
      'gradient end');
    Result := ShadingText(Native, Start, Finish);
  end;
  if not FadedGradient(Native) then
  begin
    if not IsOpaque(Native.FillOpacity) then
      Result := Result + ', ' + OpacityOption('fill opacity',
        ClampOpacity(Native.FillOpacity));
    Exit;
  end;
  if (SceneObject.FadingDeclaration >= 0) and FadingFits(Native,
    Context.Scene.Fadings[SceneObject.FadingDeclaration]) then
    Name := SceneObject.FadingName
  else
  begin
    Declaration := FadingDeclarationText(Native, Name);
    Found := -1;
    for I := 0 to High(Context.Scene.Fadings) do
      if (Context.Scene.Fadings[I].Name = Name) and
        (Context.Scene.Fadings[I].Span.EndByte <=
        Binding.StatementSpan.StartByte) then Found := I;
    if (Found < 0) or not FadingFits(Native, Context.Scene.Fadings[Found]) then
    begin
      Patch.Span := Binding.StatementSpan;
      Patch.Span.EndByte := Patch.Span.StartByte;
      Patch.Replacement := BytesOf(RawByteString(Declaration) + EOLBytes(Source));
      MergePatch(Patches, Patch);
    end;
  end;
  Result := Result + ', ' + FadingOptionsText(Native, Name);
end;

{ Replaces the fill options of a statement (fill colour, fill opacity,
  shading and fading keys) by Text at the place of the first one, removing
  the others with their separators, or inserts Text when there are none. }
procedure ReplaceFillOptions(Context: TTikZImportContext;
  BindingIndex: SizeInt; const Source: TBytes; const Text: string;
  var Insertion: string; var Patches: TPatchArray;
  KeepTransform: Boolean = False; IncludeShared: Boolean = False);
var
  Binding: TTikZSourceBinding;
  Dep: TTikZPropertyDependency;
  Spans, ValueSpans: TSpanArray;
  Keys: array of string;
  Items: TStringList;
  Span, Merged: TTikZSourceSpan;
  I, J, P: SizeInt;
  Patch: TTikZSourcePatch;
  Key, Value, Old: string;
  A, B: Double;
  SameKeys: Boolean;
  Settings: TFormatSettings;
  function IsBlank(Value: Byte): Boolean;
  begin
    Result := Value in [Ord(' '), 9, 10, 13];
  end;
  function SplitItems(const Value: string): TStringList;
  var K, Depth, First: SizeInt;
  begin
    Result := TStringList.Create;
    Depth := 0;
    First := 1;
    for K := 1 to Length(Value) + 1 do
      if (K <= Length(Value)) and (Value[K] in ['{', '(']) then Inc(Depth)
      else if (K <= Length(Value)) and (Value[K] in ['}', ')']) then Dec(Depth)
      else if (K > Length(Value)) or ((Value[K] = ',') and (Depth = 0)) then
      begin
        Result.Add(Trim(Copy(Value, First, K - First)));
        First := K + 1;
      end;
  end;
begin
  Binding := Context.BindingAt(BindingIndex);
  Spans := nil;
  ValueSpans := nil;
  Keys := nil;
  for I := 0 to High(Binding.Dependencies) do
  begin
    Dep := Binding.Dependencies[I];
    if not ((Dep.Kind in [tdFillPaint, tdFillOpacity]) or
      (IncludeShared and (Dep.Kind = tdOpacity)) or
      ((Dep.Kind = tdFillColor) and
      (Dep.PropertyKeySpan.EndByte > Dep.PropertyKeySpan.StartByte))) then
      Continue;
    Span := LocalOptionSpan(Binding, Dep, 'fill');
    if SpanAlreadyPresent(Spans, Span) then Continue;
    AddSpan(Spans, Span);
    AddSpan(ValueSpans, Dep.ValueSpan);
    SetLength(Keys, Length(Keys) + 1);
    Keys[High(Keys)] := Trim(SourceText(Source, Dep.PropertyKeySpan));
  end;
  { With the same keys, only the values that changed are replaced. }
  if (Text <> '') and (Length(Spans) > 0) then
  begin
    Items := SplitItems(Text);
    try
      SameKeys := Items.Count = Length(Keys);
      for I := 0 to Items.Count - 1 do
      begin
        Key := Trim(Copy(Items[I], 1, Pos('=', Items[I]) - 1));
        J := 0;
        while (J <= High(Keys)) and (Keys[J] <> Key) do Inc(J);
        if J > High(Keys) then SameKeys := False;
      end;
      if SameKeys then
      begin
        Settings := DefaultFormatSettings;
        Settings.DecimalSeparator := '.';
        for I := 0 to Items.Count - 1 do
        begin
          P := Pos('=', Items[I]);
          Key := Trim(Copy(Items[I], 1, P - 1));
          Value := Trim(Copy(Items[I], P + 1, MaxInt));
          J := 0;
          while Keys[J] <> Key do Inc(J);
          Old := Trim(SourceText(Source, ValueSpans[J]));
          if Old = Value then Continue;
          if (Key = 'fading transform') and KeepTransform then Continue;
          if TryStrToFloat(Old, A, Settings) and
            TryStrToFloat(Value, B, Settings) and NearValue(A, B) then Continue;
          Patch.Span := ValueSpans[J];
          Patch.Replacement := BytesOf(RawByteString(Value));
          MergePatch(Patches, Patch);
        end;
        Exit;
      end;
    finally
      Items.Free;
    end;
  end;
  for I := 1 to High(Spans) do
  begin
    Span := Spans[I];
    J := I - 1;
    while (J >= 0) and (Spans[J].StartByte > Span.StartByte) do
    begin
      Spans[J + 1] := Spans[J];
      Dec(J);
    end;
    Spans[J + 1] := Span;
  end;
  if Length(Spans) = 0 then
  begin
    if Text <> '' then
    begin
      if Insertion <> '' then Insertion := Insertion + ', ';
      Insertion := Insertion + Text;
    end;
    Exit;
  end;
  J := 0;
  if Text <> '' then
  begin
    Patch.Span := Spans[0];
    Patch.Replacement := BytesOf(RawByteString(Text));
    MergePatch(Patches, Patch);
    J := 1;
  end;
  { Each removed option takes the separator after it, or the one before
    it when it ends the list; touching removals are merged. }
  Merged.StartByte := -1;
  Merged.EndByte := -1;
  for I := J to High(Spans) do
  begin
    Span := Spans[I];
    P := Span.EndByte;
    while (P < Binding.StatementSpan.EndByte) and IsBlank(Source[P]) do Inc(P);
    if (P < Binding.StatementSpan.EndByte) and (Source[P] = Ord(',')) then
    begin
      Inc(P);
      while (P < Binding.StatementSpan.EndByte) and IsBlank(Source[P]) do Inc(P);
      Span.EndByte := P;
    end
    else
    begin
      P := Span.StartByte;
      while (P > Binding.StatementSpan.StartByte) and IsBlank(Source[P - 1]) do
        Dec(P);
      if (P > Binding.StatementSpan.StartByte) and (Source[P - 1] = Ord(',')) then
        Span.StartByte := P - 1;
    end;
    if (Merged.StartByte >= 0) and (Span.StartByte <= Merged.EndByte) then
    begin
      if Span.EndByte > Merged.EndByte then Merged.EndByte := Span.EndByte;
      Continue;
    end;
    if Merged.StartByte >= 0 then
    begin
      Patch.Span := Merged;
      Patch.Replacement := nil;
      MergePatch(Patches, Patch);
    end;
    Merged := Span;
  end;
  if Merged.StartByte >= 0 then
  begin
    Patch.Span := Merged;
    Patch.Replacement := nil;
    MergePatch(Patches, Patch);
  end;
end;

{ Gradient, fill colour and fill opacity edits of a statement whose fill is
  or becomes a gradient. }
procedure AddFillPaintChanges(Context: TTikZImportContext;
  BindingIndex: SizeInt; Native: TPrimitive2D;
  SceneObject: TTikZSceneObject; const Source: TBytes;
  var Insertion: string; var Patches: TPatchArray;
  out SharedOpacityDone: Boolean);
var
  Changed, KeepTransform: Boolean;
  Shift: TTikZPoint;
  Rotation, XScale, YScale: Double;
begin
  Changed := not SameGradient(Native, SceneObject) or
    (ColorToRGB24(Native.FillColor) <> (SceneObject.FillRGB and $FFFFFF)) or
    not NearValue(ClampOpacity(Native.FillOpacity), SceneObject.FillOpacity) or
    (FadedGradient(Native) <> (SceneObject.FadingName <> ''));
  if not Changed and FadedGradient(Native) then
  begin
    if (SceneObject.FadingDeclaration < 0) or not FadingFits(Native,
      Context.Scene.Fadings[SceneObject.FadingDeclaration]) then
      Changed := True
    else if Native.FillGradient = fgLinear then
      { A geometry edit moves the box the fading is laid out on. }
      Changed := not TikZLinearFadingOf(Native, Shift, Rotation, XScale,
        YScale) or not SameTikZFadingTransform(SceneObject.FadingShift,
        SceneObject.FadingRotation, SceneObject.FadingXScale,
        SceneObject.FadingYScale, Shift, Rotation, XScale, YScale,
        Context.Scene.PictureScaleX);
  end;
  SharedOpacityDone := False;
  if not Changed then Exit;
  SharedOpacityDone := RewriteSharedOpacity(Context, BindingIndex, Native,
    SceneObject.StrokeEnabled, Patches);
  KeepTransform := (SceneObject.FadingName <> '') and
    (Native.FillGradient = fgLinear) and
    TikZLinearFadingOf(Native, Shift, Rotation, XScale, YScale) and
    SameTikZFadingTransform(SceneObject.FadingShift,
    SceneObject.FadingRotation, SceneObject.FadingXScale,
    SceneObject.FadingYScale, Shift, Rotation, XScale, YScale,
    Context.Scene.PictureScaleX);
  ReplaceFillOptions(Context, BindingIndex, Source,
    EditedFillText(Context, BindingIndex, Native, SceneObject, Source, Patches),
    Insertion, Patches, KeepTransform, not SharedOpacityDone);
end;

{ Part names the paint a statement carries for its native object: all of
  it, or only the fill or only the stroke when TpX writes the fill of a
  faded gradient and its stroke as two statements. ExtraOptions are
  inserted at the start of the statement's options. }
procedure AddNativeChanges(Context: TTikZImportContext; BindingIndex: SizeInt;
  Native: TPrimitive2D; Drawing: TDrawing2D; Source: TBytes;
  var Edits: TEditArray; var StylePatches: TPatchArray;
  Part: TStatementPart = spWhole; const ExtraOptions: string = '';
  SharedOpacityDone: Boolean = False);
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
  Insertion, Problem: string;
  Shared: Boolean;
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
  Insertion := ExtraOptions;
  DepIndex := FindDependency(Binding, tdLineWidth);
  if Part = spFill then
  else if Native.LineStyle <> liNone then
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
  if (Part <> spFill) and not SceneObject.StrokeEnabled and
    (Native.LineStyle <> liNone) then
    raise EWriteError.Create('Adding a stroke to an unpainted TikZ object is not supported');

  if (Part <> spFill) and SceneObject.StrokeEnabled then
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

  if Part = spStroke then
  else if (NativeGradient(Native) <> tgNone) or
    (SceneObject.Gradient <> tgNone) then
  begin
    if Native.FillColor = clDefault then
      raise EWriteError.Create('Removing a TikZ fill is not supported');
    AddFillPaintChanges(Context, BindingIndex, Native, SceneObject, Source,
      Insertion, StylePatches, Shared);
    SharedOpacityDone := SharedOpacityDone or Shared;
  end
  else if SceneObject.FillEnabled then
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

  AddOpacityChanges(Context, BindingIndex, Native, SceneObject, Source,
    Insertion, StylePatches, Part <> spFill, (Part = spWhole) and
    (NativeGradient(Native) = tgNone) and (SceneObject.Gradient = tgNone),
    SharedOpacityDone);
  if Part = spFill then
  else if SceneObject.StrokeEnabled and
    (((SceneObject.LineKind = tlSolid) and (Native.LineStyle <> liSolid)) or
    ((SceneObject.LineKind = tlDashed) and (Native.LineStyle <> liDashed)) or
    ((SceneObject.LineKind = tlDotted) and (Native.LineStyle <> liDotted))) then
  begin
    Name := LineStyleKeyword(Native.LineStyle);
    if Name = '' then
      raise EWriteError.Create('the line style has no TikZ source form');
    AddStyleKeyword(Context, BindingIndex, tdDashSize, Name, 'line style',
      Source, Insertion, StylePatches);
  end;
  Problem := ArrowSpec(Native, Value);
  if (Problem <> '') and (Part <> spFill) then raise EWriteError.Create(Problem);
  if (Part <> spFill) and (Native.BeginArrowKind <> TikZTipArrowKind(SceneObject.BeginArrow)) or
    (Native.EndArrowKind <> TikZTipArrowKind(SceneObject.EndArrow)) then
  begin
    if Value = '' then Value := '-';
    AddStyleKeyword(Context, BindingIndex, tdArrows, Value, 'arrow style',
      Source, Insertion, StylePatches);
  end;
  AddStyleInsertion(Context, BindingIndex, Source, Insertion, StylePatches);
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

{ Build the option list of a new statement. The result is '' on success or
  names the native property that has no TikZ source form. }
function NewObjectOptions(Primitive: TPrimitive2D; Drawing: TDrawing2D;
  var Definitions: TDefinitionArray; out Options, FadedFill: string): string;
var Spec, Start, Finish, Name, Declaration: string; WidthMM: Extended;
  I: SizeInt;
begin
  Result := '';
  Options := '';
  FadedFill := '';
  if Primitive.Hatching <> haNone then
    Exit('hatching has no TikZ source form');
  if Primitive is TText2D then
  begin
    if Primitive.FillColor <> clDefault then
      Exit('text fill color has no TikZ source form');
    if Primitive.LineColor <> clDefault then
      Options := 'text=' + TikZColor(ColorToRGB24(Primitive.LineColor),
        Definitions);
    Exit;
  end;
  Result := ArrowSpec(Primitive, Spec);
  if Result <> '' then Exit;
  if Primitive.LineStyle <> liNone then
  begin
    if Drawing.LineWidthBase <= 0 then
      Exit('line width has no valid drawing line-width base');
    WidthMM := Primitive.LineWidth * Drawing.LineWidthBase;
    Options := 'line width=' + FormatTikZReal(WidthMM, SizeOf(TRealType)) + 'mm';
    if Primitive.LineColor = clDefault then
      Options := Options + ', draw=black'
    else
      Options := Options + ', draw=' +
        TikZColor(ColorToRGB24(Primitive.LineColor), Definitions);
    case Primitive.LineStyle of
      liDashed, liDotted:
        Options := Options + ', ' + LineStyleKeyword(Primitive.LineStyle);
      liSolid: ;
    else Exit('line style has no TikZ source form');
    end;
    if Spec <> '' then Options := Options + ', ' + Spec;
  end
  else if Spec <> '' then
    Exit('arrows on an object without a stroke have no TikZ source form');
  if FadedGradient(Primitive) then
  begin
    { The fill becomes its own statement with a fading, declared once
      among the definitions; Options keep only the stroke. }
    Start := TikZColor(ColorToRGB24(Primitive.FillColor), Definitions);
    Finish := TikZColor(NativeGradientRGB(Primitive), Definitions);
    Declaration := FadingDeclarationText(Primitive, Name);
    I := 0;
    while (I <= High(Definitions)) and (Definitions[I] <> Declaration) do Inc(I);
    if I > High(Definitions) then
    begin
      SetLength(Definitions, I + 1);
      Definitions[I] := Declaration;
    end;
    FadedFill := ShadingText(Primitive, Start, Finish) + ', ' +
      FadingOptionsText(Primitive, Name);
  end
  else if (Primitive.FillColor <> clDefault) and
    (Primitive.FillGradient <> fgNone) then
  begin
    { TikZ axis and radial shadings, laid out as TPaintStyle describes. }
    if Options <> '' then Options := Options + ', ';
    Start := TikZColor(ColorToRGB24(Primitive.FillColor), Definitions);
    Finish := TikZColor(NativeGradientRGB(Primitive), Definitions);
    Options := Options + ShadingText(Primitive, Start, Finish);
  end
  else if Primitive.FillColor <> clDefault then
  begin
    if Options <> '' then Options := Options + ', ';
    Options := Options + 'fill=' +
      TikZColor(ColorToRGB24(Primitive.FillColor), Definitions);
  end;
  if (Primitive.LineStyle <> liNone) and (Primitive.LineOpacity <> 1) then
    Options := Options + ', ' +
      OpacityOption('draw opacity', ClampOpacity(Primitive.LineOpacity));
  if (Primitive.FillColor <> clDefault) and (Primitive.FillOpacity <> 1) and
    (FadedFill = '') then
    Options := Options + ', ' +
      OpacityOption('fill opacity', ClampOpacity(Primitive.FillOpacity));
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

function ObjectLabel(Obj: TObject2D; Index: SizeInt): string;
begin
  if Obj is TPrimitive2D then
    Result := Format('Object %d (%s)', [Index, TPrimitive2D(Obj).GetName])
  else Result := Format('Object %d (%s)', [Index, Obj.ClassName]);
end;

{ The path of a new primitive after its options, with the terminating
  semicolon. }
function NewPrimitiveGeometry(Primitive: TPrimitive2D): RawByteString; forward;

function SerializeNewPrimitive(Primitive: TPrimitive2D;
  Drawing: TDrawing2D; var Definitions: TDefinitionArray;
  const EOL: RawByteString): RawByteString;
var
  Options, FadedFill, Body, Problem: string;
  TextObject: TText2D;
  SizeRatio: Extended;
  Geometry: RawByteString;
begin
  Problem := NewObjectOptions(Primitive, Drawing, Definitions, Options,
    FadedFill);
  if Problem <> '' then raise EWriteError.Create(Problem);
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
  Geometry := NewPrimitiveGeometry(Primitive);
  if FadedFill <> '' then
  begin
    { As T_TikZ_Device writes it: the faded fill, then the stroke. }
    Result := RawByteString('\path[' + FadedFill + '] ') + Geometry;
    if Primitive.LineStyle <> liNone then
      Result := Result + EOL + RawByteString('\draw[' + Options + '] ') +
        Geometry;
    Exit;
  end;
  if Primitive.LineStyle = liNone then
  begin
    if Primitive.FillColor <> clDefault then
      Result := RawByteString('\fill[' + Options + '] ')
    else Result := '\path ';
  end
  else Result := RawByteString('\draw[' + Options + '] ');
  Result := Result + Geometry;
end;

function NewPrimitiveGeometry(Primitive: TPrimitive2D): RawByteString;
var
  I, N: SizeInt;
  A, B, C: TPoint2D;
  Bezier: Boolean;
  CX, CY, Radius, StartAngle, EndAngle: TRealType;
  ArcText: string;
begin
  Result := '';
  if Primitive is TCircular2D then
  begin
    TCircular2D(Primitive).GetArcParams(CX, CY, Radius, StartAngle, EndAngle);
    if IsNan(CX) or IsNan(CY) or IsNan(Radius) or IsInfinite(CX) or
      IsInfinite(CY) or IsInfinite(Radius) or (Radius <= 0) then
      raise EWriteError.Create('New TikZ arc has no finite center and radius');
    { Native arcs run counterclockwise from the start to the end angle. }
    if EndAngle <= StartAngle then EndAngle := EndAngle + 2 * Pi;
    A := Point2D(CX + Radius * Cos(StartAngle), CY + Radius * Sin(StartAngle));
    ArcText := ' arc (' + FormatTikZReal(StartAngle * 180 / Pi,
      SizeOf(TRealType)) + ':' + FormatTikZReal(EndAngle * 180 / Pi,
      SizeOf(TRealType)) + ':' + FormatTikZReal(Radius, SizeOf(TRealType)) +
      'mm)';
    if Primitive is TSector2D then
      Result := Result + RawByteString(PointText(Point2D(CX, CY)) + ' -- ' +
        PointText(A) + ArcText + ' -- cycle;')
    else if Primitive is TSegment2D then
      Result := Result + RawByteString(PointText(A) + ArcText + ' -- cycle;')
    else
      Result := Result + RawByteString(PointText(A) + ArcText + ';');
    Exit;
  end;
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

function SolidColorName(const Color, Default: TColor; const Id: string;
  Defs: TStrings): string;
var
  RGB: LongWord;
begin
  if Color = clDefault then RGB := ColorToRGB24(Default)
  else RGB := ColorToRGB24(Color);
  if RGBColorName(RGB, Result) then Exit;
  Result := Id;
  Defs.Add('\definecolor{' + Id + '}{rgb}{' +
    FormatTikZReal(((RGB shr 16) and $FF) / 255, SizeOf(Double)) + ',' +
    FormatTikZReal(((RGB shr 8) and $FF) / 255, SizeOf(Double)) + ',' +
    FormatTikZReal((RGB and $FF) / 255, SizeOf(Double)) + '}');
end;

// A 3D object as a block of plain TikZ paths (painter order) framed by
// comments that carry the editable object and a digest of the paths.
function SerializeNewSolid(Solid: TSolid3D; Drawing: TDrawing2D): RawByteString;
var
  Lines, Defs: TStringList;
  I, J: Integer;
  Piece: TPiece;
  LineName, FillName, HatchName, Options, Path: string;
  Segments, Loop: TPointsSet2D;
  Width: Extended;
  procedure AddPath(const Opt: string; const PP: TPointsSet2D;
    const Closed: Boolean; const Command: string);
  var
    K: Integer;
  begin
    Path := Command + '[' + Opt + '] ' + PointText(PP[0]);
    for K := 1 to PP.Count - 1 do Path := Path + ' -- ' + PointText(PP[K]);
    if Closed then Path := Path + ' -- cycle';
    Lines.Add(Path + ';');
  end;
begin
  Lines := TStringList.Create;
  Defs := TStringList.Create;
  Segments := TPointsSet2D.Create(0);
  Loop := TPointsSet2D.Create(0);
  try
    if Drawing.LineWidthBase <= 0 then
      raise EWriteError.Create('A TikZ 3D object needs a positive line width base');
    LineName := SolidColorName(Solid.LineColor, clBlack, 'tpxsolidline', Defs);
    FillName := SolidColorName(Solid.FillColor, clWhite, 'tpxsolidfill', Defs);
    HatchName := SolidColorName(Solid.HatchColor, clBlack, 'tpxsolidhatch', Defs);
    Lines.AddStrings(Defs);
    Width := Solid.LineWidth * Drawing.LineWidthBase;
    for I := 0 to Solid.Pieces.Count - 1 do
    begin
      Piece := Solid.Pieces[I];
      if (Piece = nil) or (Piece.Count < 2) then Continue;
      Loop.Clear;
      for J := 0 to Piece.Count - 1 do Loop.Add(Piece[J]);
      Options := '';
      if Piece.GetLineStyle(Solid) <> liNone then
      begin
        Options := 'line width=' + FormatTikZReal(Width, SizeOf(TRealType)) +
          'mm, draw=' + LineName;
        case Piece.GetLineStyle(Solid) of
          liDashed: Options := Options + ', dashed';
          liDotted: Options := Options + ', dotted';
        end;
      end;
      if (Piece.Fill = pfiDefault) and (Solid.FillColor <> clNone) then
      begin
        if Options <> '' then Options := Options + ', ';
        Options := Options + 'fill=' + FillName;
        if Piece.GetLineStyle(Solid) = liNone then
          AddPath(Options, Loop, Piece.Closed, '\fill')
        else AddPath(Options, Loop, Piece.Closed, '\draw');
      end
      else if Options <> '' then
        AddPath(Options, Loop, Piece.Closed, '\draw');
      if (Piece.Hatch = phaDefault) and Piece.Closed and
        (Solid.Hatching <> haNone) then
      begin
        Segments.Clear;
        HatchPolygon(Loop, Solid.Hatching, Drawing.HatchingStep, Segments);
        J := 0;
        while J + 1 < Segments.Count do
        begin
          Lines.Add('\draw[line width=' + FormatTikZReal(Width,
            SizeOf(TRealType)) + 'mm, draw=' + HatchName + '] ' +
            PointText(Segments[J]) + ' -- ' + PointText(Segments[J + 1]) + ';');
          Inc(J, 2);
        end;
      end;
    end;
    Result := RawByteString('% tpx-solid ' + SolidToTpXXml(Drawing, Solid, False, True) +
      LineEnding);
    for I := 0 to Lines.Count - 1 do
      Result := Result + RawByteString(Lines[I] + LineEnding);
    Result := Result + RawByteString('% tpx-solid-end ' +
      SolidGeometryDigest(Lines));
  finally
    Loop.Free;
    Segments.Free;
    Defs.Free;
    Lines.Free;
  end;
end;

function SerializeNewObject(Obj: TObject2D; Drawing: TDrawing2D;
  Index: SizeInt; var Definitions: TDefinitionArray;
  const EOL: RawByteString): RawByteString;
begin
  try
    if Obj is TSolid3D then
      Exit(SerializeNewSolid(TSolid3D(Obj), Drawing));
    if not (Obj is TPrimitive2D) then
      raise EWriteError.Create(
        'only standalone primitive objects can be inserted into a TikZ picture');
    Result := SerializeNewPrimitive(TPrimitive2D(Obj), Drawing, Definitions,
      EOL);
  except
    on E: EWriteError do
      raise EWriteError.Create(ObjectLabel(Obj, Index) + ': ' + E.Message);
  end;
end;

function SerializeNewDocument(Drawing: TDrawing2D;
  const FileName: TDocumentPath): RawByteString;
var
  Obj: TObject2D;
  IsTeXDocument: Boolean;
  EOL, FontDefault, Body: RawByteString;
  Definitions: TDefinitionArray;
  Index, I: SizeInt;
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
  Body := '';
  Definitions := nil;
  Index := 1;
  Obj := Drawing.ObjectList.FirstObj as TObject2D;
  while Obj <> nil do
  begin
    Body := Body + SerializeNewObject(Obj, Drawing, Index, Definitions, EOL) +
      EOL;
    Inc(Index);
    Obj := Drawing.ObjectList.NextObj as TObject2D;
  end;
  for I := 0 to High(Definitions) do
    Result := Result + RawByteString(Definitions[I]) + EOL;
  Result := Result + Body + '\end{tikzpicture}' + EOL;
  if IsTeXDocument then Result := Result + '\end{document}' + EOL;
end;

function GenerateTikZPictureFragment(Drawing: TDrawing2D): RawByteString;
var
  Obj: TObject2D;
begin
  if Drawing <> nil then
  begin
    Obj := Drawing.ObjectList.FirstObj as TObject2D;
    while Obj <> nil do
    begin
      if Obj is TBitmap2D then
        raise EWriteError.Create(
          'TikZ copy cannot embed bitmap assets; copy or export the linked image separately');
      Obj := Drawing.ObjectList.NextObj as TObject2D;
    end;
  end;
  Result := '% Generated from the current TpX drawing; this is regenerated TikZ.' +
    RawByteString(LineEnding) +
    '% Requires \usepackage{tikz}; TeX labels may need their own packages or macros.' +
    RawByteString(LineEnding) +
    SerializeNewDocument(Drawing, TDocumentPath('clipboard.tikz'));
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
  StylePatches: TPatchArray;
  Definitions: TDefinitionArray;
  ObjectIndex, DefinitionsBefore: SizeInt;
  SolidIndex: SizeInt;
  SolidRegion: TTikZSolidRegion;
  SceneObject: TTikZSceneObject;
  Referenced, Kept: Boolean;
  Live: TObject2D;
  function SpanText(const Bytes: TBytes; const Span: TTikZSourceSpan): RawByteString;
  var
    K: SizeInt;
  begin
    SetLength(Result, Span.EndByte - Span.StartByte);
    for K := 1 to Length(Result) do Result[K] := Char(Bytes[Span.StartByte + K - 1]);
  end;
  { A faded fill and the stroke TpX writes after it. While the gradient
    stays faded each statement takes its part of the edit; otherwise the
    fill statement goes and its fill joins the stroke statement. }
  procedure AddFadedPairChanges(FillIndex, StrokeIndex: SizeInt);
  var
    Fill, Stroke: TTikZSceneObject;
    FillText: string;
    procedure CheckName(const Name: string);
    begin
      if (Name <> '') and (Pos(',' + Name + ',', Stroke.ColorsSinceFill) > 0) and
        (Pos('=' + Name, FillText) > 0) then
        raise EWriteError.Create('the fill color ' + Name +
          ' is redefined before the stroke path it would join');
    end;
  begin
    Fill := Context.Scene.ObjectAt(FillIndex);
    Stroke := Context.Scene.ObjectAt(StrokeIndex);
    if FadedGradient(Primitive) then
    begin
      AddNativeChanges(Context, FillIndex, Primitive, Drawing, Source,
        Edits, StylePatches, spFill);
      AddNativeChanges(Context, StrokeIndex, Primitive, Drawing, Source,
        Edits, StylePatches, spStroke);
      Exit;
    end;
    if Primitive.FillColor = clDefault then
      raise EWriteError.Create('Removing a TikZ fill is not supported');
    FillText := EditedFillText(Context, FillIndex, Primitive, Fill, Source,
      StylePatches);
    CheckName(Fill.FillColorName);
    CheckName(Fill.GradientColorName);
    AddSpan(DeletedSpans, Context.BindingAt(FillIndex).StatementSpan);
    AddPatch(Patches, Context.BindingAt(FillIndex).StatementSpan, '');
    AddNativeChanges(Context, StrokeIndex, Primitive, Drawing, Source,
      Edits, StylePatches, spStroke, FillText);
  end;

  { A stroked statement whose gradient becomes faded: the fill moves to a
    new statement before it, as TpX writes such a gradient. }
  procedure SplitFadedFill(StatementIndex: SizeInt);
  var
    SceneObj: TTikZSceneObject;
    FillText, Ignored: string;
    Declarations: TPatchArray;
    Patch: TTikZSourcePatch;
    T: TTikZAffineTransform;
    P, Q: SizeInt;
    Command: string;
  begin
    SceneObj := Context.Scene.ObjectAt(StatementIndex);
    T := SceneObj.ScopeTransform;
    if (T.XX <> 1) or (T.XY <> 0) or (T.YX <> 0) or (T.YY <> 1) or
      (T.TX <> 0) or (T.TY <> 0) then
      raise EWriteError.Create(
        'a faded gradient inside a transformed TikZ scope is not supported');
    Declarations := nil;
    FillText := EditedFillText(Context, StatementIndex, Primitive, SceneObj,
      Source, Declarations);
    AddNativeChanges(Context, StatementIndex, Primitive, Drawing, Source,
      Edits, StylePatches, spStroke, '', RewriteSharedOpacity(Context,
      StatementIndex, Primitive, True, StylePatches));
    Ignored := '';
    ReplaceFillOptions(Context, StatementIndex, Source, '', Ignored,
      StylePatches);
    Patch.Span := Context.BindingAt(StatementIndex).StatementSpan;
    Patch.Span.EndByte := Patch.Span.StartByte;
    Patch.Replacement := BytesOf(RawByteString('\path[' + FillText + '] ') +
      NewPrimitiveGeometry(Primitive) + EOL);
    MergePatch(StylePatches, Patch);
    for P := 0 to High(Declarations) do MergePatch(StylePatches, Declarations[P]);
    { \fill and \filldraw fill by themselves. }
    P := Patch.Span.StartByte + 1;
    Q := P;
    while (Q < Length(Source)) and (Chr(Source[Q]) in ['A'..'Z', 'a'..'z']) do
      Inc(Q);
    Patch.Span.EndByte := Q;
    Command := SourceText(Source, Patch.Span);
    if (Command = '\fill') or (Command = '\filldraw') then
    begin
      Patch.Replacement := BytesOf('\draw');
      MergePatch(StylePatches, Patch);
    end;
  end;

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

  StylePatches := nil;
  Definitions := nil;
  ObjectIndex := 1;
  Obj := Drawing.ObjectList.FirstObj as TObject2D;
  while Obj <> nil do
  begin
    SolidIndex := Context.FindSolidRegionForObjectID(Obj.ID);
    if (Obj is TSolid3D) and (SolidIndex >= 0) then
    begin
      // A solid read from a '% tpx-solid' block rewrites only its block,
      // and leaves it byte-identical when nothing changed.
      SolidRegion := Context.SolidRegionAt(SolidIndex);
      if SeenNewObject then
        raise EWriteError.Create('New TikZ objects can only be appended; current order would reorder source statements');
      if SolidRegion.FirstBinding < PreviousIndex then
        raise EWriteError.Create('Reordered TikZ objects cannot be saved without changing draw order');
      PreviousIndex := SolidRegion.LastBinding;
      Replacement := SerializeNewSolid(TSolid3D(Obj), Drawing);
      if Replacement <> SpanText(Source, SolidRegion.Span) then
      begin
        GeometryChanged := True;
        AddPatch(Patches, SolidRegion.Span, Replacement);
      end;
      Inc(ObjectIndex);
      Obj := Drawing.ObjectList.NextObj as TObject2D;
      Continue;
    end;
    BindingIndex := Context.FindBindingForObjectID(Obj.ID);
    if BindingIndex < 0 then
    begin
      SeenNewObject := True;
      GeometryChanged := True;
      if not SupportsTikZStructuralOperation(tsoInsertStatement) then
        raise EWriteError.Create('TikZ statement insertion is not supported');
      DefinitionsBefore := Length(Definitions);
      Replacement := SerializeNewObject(Obj, Drawing, ObjectIndex, Definitions,
        EOL);
      for K := High(Definitions) downto DefinitionsBefore do
        Replacement := RawByteString(Definitions[K]) + EOL + Replacement;
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
      try
        SceneObject := Context.Scene.ObjectAt(Binding.ObjectIndex);
        if SceneObject.StrokeCompanion >= 0 then
        begin
          PreviousIndex := SceneObject.StrokeCompanion;
          AddFadedPairChanges(BindingIndex, SceneObject.StrokeCompanion);
        end
        else if FadedGradient(Primitive) and SceneObject.StrokeEnabled and
          (Primitive.LineStyle <> liNone) then
          SplitFadedFill(BindingIndex)
        else
          AddNativeChanges(Context, BindingIndex, Primitive, Drawing, Source,
            Edits, StylePatches);
      except
        on E: EWriteError do
          raise EWriteError.Create(ObjectLabel(Obj, ObjectIndex) + ': ' +
            E.Message);
      end;
      for K := EditsBefore to High(Edits) do
        if EditAffectsBounds(Edits[K]) then GeometryChanged := True;
    end;
    Inc(ObjectIndex);
    Obj := Drawing.ObjectList.NextObj as TObject2D;
  end;

  { One source command can create several native subpaths. Removing only some
    of them cannot be represented by deleting that command. }
  for I := 0 to Context.BindingCount - 1 do
  begin
    SolidIndex := Context.SolidRegionOfBinding(I);
    if SolidIndex >= 0 then
    begin
      // The statements of a solid block live and die with that solid.
      SolidRegion := Context.SolidRegionAt(SolidIndex);
      if (I = SolidRegion.FirstBinding) and
        (Drawing.GetObject(SolidRegion.NativeObjectID) = nil) then
      begin
        GeometryChanged := True;
        Region := SolidRegion.Span;
        if (Region.EndByte < Length(Source)) and (Source[Region.EndByte] = 13) then
          Inc(Region.EndByte);
        if (Region.EndByte < Length(Source)) and (Source[Region.EndByte] = 10) then
          Inc(Region.EndByte);
        AddPatch(Patches, Region, '');
        Region := Context.Document.Region.Span;
      end;
      Continue;
    end;
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

  { A fading declaration goes with the last statement that used it. }
  for I := 0 to High(Context.Scene.Fadings) do
  begin
    Referenced := False;
    Kept := False;
    for J := 0 to Context.BindingCount - 1 do
    begin
      Binding := Context.BindingAt(J);
      if Context.Scene.ObjectAt(Binding.ObjectIndex).FadingDeclaration <> I then
        Continue;
      Referenced := True;
      Live := Drawing.GetObject(Binding.NativeObjectID);
      if (Live is TPrimitive2D) and
        FadingFits(TPrimitive2D(Live), Context.Scene.Fadings[I]) and
        not SpanAlreadyPresent(DeletedSpans, Binding.StatementSpan) then
        Kept := True;
    end;
    if Referenced and not Kept then
      AddPatch(Patches, Context.Scene.Fadings[I].Span, '');
  end;

  if not BuildTikZPropertyPatches(Source, Region, Bindings, Edits,
    PropertyPatches, Error) then
    raise EWriteError.CreateFmt('TikZ source patch could not be built (error %d)',
      [Ord(Error)]);
  for I := 0 to High(PropertyPatches) do
    AddPatch(Patches, PropertyPatches[I].Span,
      RawOf(PropertyPatches[I].Replacement));
  for I := 0 to High(StylePatches) do
    MergePatch(Patches, StylePatches[I]);

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
  LiveObject, ParsedObject, LastLive, LastParsed: TObject2D;
  I, SolidIndex: SizeInt;
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
  LastLive := nil;
  LastParsed := nil;
  for I := 0 to Context.BindingCount - 1 do
  begin
    SolidIndex := Context.SolidRegionOfBinding(I);
    if SolidIndex >= 0 then
    begin
      // One live 3D object stands for all statements of its block.
      if I <> Context.SolidRegionAt(SolidIndex).FirstBinding then Continue;
      if not (LiveObject is TSolid3D) or not (ParsedObject is TSolid3D) or
        (Context.SolidRegionAt(SolidIndex).NativeObjectID <> ParsedObject.ID) then
        raise EWriteError.Create('TikZ parsed 3D object order no longer matches its source block');
      Context.SetSolidRegionObject(SolidIndex, LiveObject.ID);
      LiveObject := LiveDrawing.ObjectList.NextObj as TObject2D;
      ParsedObject := ParsedDrawing.ObjectList.NextObj as TObject2D;
      Continue;
    end;
    Binding := Context.BindingAt(I);
    if Binding.ObjectIndex <> I then
      raise EWriteError.Create('TikZ source bindings are not in object order');
    if (Context.Scene.ObjectAt(I).StrokeOf >= 0) and (LastLive <> nil) then
    begin
      // The stroke of a faded fill belongs to the object bound before it.
      if ParsedDrawing.GetObject(Binding.NativeObjectID) <> LastParsed then
        raise EWriteError.Create('TikZ parsed object order no longer matches its source bindings');
      Context.BindNativeObject(I, LastLive.ID);
      Continue;
    end;
    if (LiveObject = nil) or (ParsedObject = nil) then
      raise EWriteError.Create('TikZ save-back changed the bound object count');
    if ParsedDrawing.GetObject(Binding.NativeObjectID) <> ParsedObject then
      raise EWriteError.Create('TikZ parsed object order no longer matches its source bindings');
    Context.BindNativeObject(I, LiveObject.ID);
    LastLive := LiveObject;
    LastParsed := ParsedObject;
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
  Context := nil;
  if CodecContext <> nil then
  begin
    if not (CodecContext is TTikZImportContext) then
      raise EWriteError.Create('TikZ save-back context has an unsupported type');
    Context := CodecContext as TTikZImportContext;
    // Bare fragments need a picture envelope when saved as .tex.
    if Context.Document.Region.IsFragment and
      SameText(ExtractFileExt(string(FileName)), '.tex') then Context := nil;
  end;
  if Context = nil then
  begin
    NewDocument := SerializeNewDocument(Drawing, FileName);
    if Length(NewDocument) > 0 then
      Destination.WriteBuffer(NewDocument[1], Length(NewDocument));
    Exit(True);
  end;
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
