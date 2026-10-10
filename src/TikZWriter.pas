unit TikZWriter;

{$mode objfpc}{$H+}

interface

uses SysUtils, TikZLexer, TikZSyntax, TikZImport;

type
  TTikZSourcePatch = record
    Span: TTikZSourceSpan;
    Replacement: TBytes;
  end;
  TTikZSourcePatches = array of TTikZSourcePatch;

  TTikZPatchError = (
    tpeNone,
    tpeInvalidRegion,
    tpeSpanOutsideSource,
    tpePatchOutsideRegion,
    tpeUnsortedPatches,
    tpeOverlappingPatches,
    tpeOutputTooLarge,
    tpeUnsupportedDependency,
    tpeInvalidCandidate,
    tpeMissingPicture,
    tpeUnsupportedOperation,
    tpeInvalidBinding,
    tpeInvalidDependency
  );

  TTikZBoundEdit = record
    BindingIndex: SizeInt;
    DependencyIndex: SizeInt;
    Replacement: TBytes;
    LocalOverrideOption: TBytes;
    LocalExpressionOverride: Boolean;
  end;
  TTikZBoundEdits = array of TTikZBoundEdit;

  TTikZStructuralOperation = (tsoInsertStatement, tsoDeleteStatement,
    tsoReorderStatement, tsoGroupStatements);

{ Apply source-relative [start,end) patches inside EditableRegion. The source
  and all gaps between patches are copied byte-for-byte. Patches must be
  ordered by StartByte and may not overlap. An empty patch list is an exact
  byte copy. This routine does not infer whether a caller's edit is local or
  semantically safe; callers must build patches from supported source bindings
  and validate the completed candidate with TikZSyntax before replacing a file. }
function ApplyTikZSourcePatches(const Source: TBytes;
  const EditableRegion: TTikZSourceSpan;
  const Patches: array of TTikZSourcePatch;
  out Updated: TBytes; out Error: TTikZPatchError): Boolean;

{ Convert dependency-indexed edits into source patches. Local literals/styles
  replace their value span. A shared dependency is editable only when the
  importer proves a safe local replacement: either an object-local expression
  span (`LocalExpressionOverride`) or a statement-local option supplied in
  `LocalOverrideOption`. }
function BuildTikZPropertyPatches(const Source: TBytes;
  const EditableRegion: TTikZSourceSpan;
  const Bindings: TTikZSourceBindings;
  const Edits: array of TTikZBoundEdit;
  out Patches: TTikZSourcePatches;
  out Error: TTikZPatchError): Boolean;

{ Apply patches and require a structurally valid TikZ candidate. Unsupported
  but well-formed commands are retained as warnings; malformed syntax, a
  cancelled parse or a lost/ambiguous picture region rejects the candidate. }
function ApplyValidatedTikZSourcePatches(const Source: TBytes;
  const EditableRegion: TTikZSourceSpan;
  const Patches: array of TTikZSourcePatch;
  const InputKind: TTikZInputKind;
  out Updated: TBytes; out Error: TTikZPatchError): Boolean;

{ Context-based entry point for a model adapter. The context owns immutable
  source bytes and bindings; the caller supplies only changed local values. }
function ApplyTikZBoundEdits(Context: TTikZImportContext;
  const Edits: array of TTikZBoundEdit;
  const InputKind: TTikZInputKind;
  out Updated: TBytes; out Error: TTikZPatchError): Boolean;

{ Insertion and deletion are safe only as statement-local edits inside the
  selected picture. Reordering and grouping change draw order or style scope;
  reject them until a scope-aware transformation can prove equivalence. }
function SupportsTikZStructuralOperation(
  const Operation: TTikZStructuralOperation): Boolean;

{ Return enough significant digits for a value stored in a real type of the
  given size (4=Single, 8=Double, larger=Extended). The caller supplies the
  storage size of the actual scene type, avoiding a dependency on Geometry/LCL. }
function FormatTikZReal(const Value: Extended;
  const RealTypeSize: SizeInt): string;
{ Convert an evaluated coordinate to the scalar stored in its original
  TikZ unit. Relative coordinates use the caller's current evaluated base. }
function TikZCoordinateScalar(const ValueMM, RelativeBaseMM,
  ValueScaleToMM: Extended; const IsRelative: Boolean): Extended;
function FormatTikZCoordinate(const ValueMM, RelativeBaseMM,
  ValueScaleToMM: Extended; const IsRelative: Boolean;
  const RealTypeSize: SizeInt): string;

implementation

uses Math;

function ValidSpan(const Span: TTikZSourceSpan; SourceLength: SizeInt): Boolean;
begin
  Result := (Span.StartByte >= 0) and
    (Span.EndByte >= Span.StartByte) and
    (Span.EndByte <= SourceLength);
end;

procedure CopyBytes(const Source: TBytes; SourceStart, Count: SizeInt;
  var Destination: TBytes; DestinationStart: SizeInt);
begin
  if Count > 0 then
    Move(Source[SourceStart], Destination[DestinationStart], Count);
end;

function ApplyTikZSourcePatches(const Source: TBytes;
  const EditableRegion: TTikZSourceSpan;
  const Patches: array of TTikZSourcePatch;
  out Updated: TBytes; out Error: TTikZPatchError): Boolean;
var
  I, SourcePos, DestinationPos, Gap, PatchLength, ReplacementLength,
    NewLength: SizeInt;
  PreviousStart, PreviousEnd: SizeInt;
begin
  Updated := nil;
  Error := tpeNone;
  Result := False;

  if not ValidSpan(EditableRegion, Length(Source)) then
  begin
    Error := tpeInvalidRegion;
    Exit;
  end;

  NewLength := Length(Source);
  PreviousStart := -1;
  PreviousEnd := -1;
  for I := 0 to High(Patches) do
  begin
    if not ValidSpan(Patches[I].Span, Length(Source)) then
    begin
      Error := tpeSpanOutsideSource;
      Exit;
    end;
    if (Patches[I].Span.StartByte < EditableRegion.StartByte) or
      (Patches[I].Span.EndByte > EditableRegion.EndByte) then
    begin
      Error := tpePatchOutsideRegion;
      Exit;
    end;
    if I > 0 then
    begin
      if Patches[I].Span.StartByte < PreviousStart then
      begin
        Error := tpeUnsortedPatches;
        Exit;
      end;
      if (Patches[I].Span.StartByte = PreviousStart) or
        (Patches[I].Span.StartByte < PreviousEnd) then
      begin
        Error := tpeOverlappingPatches;
        Exit;
      end;
    end;
    PreviousStart := Patches[I].Span.StartByte;
    PreviousEnd := Patches[I].Span.EndByte;
    PatchLength := Patches[I].Span.EndByte - Patches[I].Span.StartByte;
    ReplacementLength := Length(Patches[I].Replacement);
    if ReplacementLength >= PatchLength then
    begin
      Gap := ReplacementLength - PatchLength;
      if Gap > High(SizeInt) - NewLength then
      begin
        Error := tpeOutputTooLarge;
        Exit;
      end;
      Inc(NewLength, Gap);
    end
    else
    begin
      Gap := PatchLength - ReplacementLength;
      Dec(NewLength, Gap);
    end;
  end;

  SetLength(Updated, SizeInt(NewLength));
  SourcePos := 0;
  DestinationPos := 0;
  for I := 0 to High(Patches) do
  begin
    Gap := Patches[I].Span.StartByte - SourcePos;
    CopyBytes(Source, SourcePos, Gap, Updated, DestinationPos);
    Inc(SourcePos, Gap);
    Inc(DestinationPos, Gap);
    PatchLength := Length(Patches[I].Replacement);
    CopyBytes(Patches[I].Replacement, 0, PatchLength, Updated,
      DestinationPos);
    Inc(DestinationPos, PatchLength);
    SourcePos := Patches[I].Span.EndByte;
  end;
  Gap := Length(Source) - SourcePos;
  CopyBytes(Source, SourcePos, Gap, Updated, DestinationPos);
  Result := True;
end;

function BuildTikZPropertyPatches(const Source: TBytes;
  const EditableRegion: TTikZSourceSpan;
  const Bindings: TTikZSourceBindings;
  const Edits: array of TTikZBoundEdit;
  out Patches: TTikZSourcePatches;
  out Error: TTikZPatchError): Boolean;
var
  I, J, BindingIndex, DependencyIndex, PatchCount, InsertAt,
    PreviousLength, OptionEnd: SizeInt;
  Binding: TTikZSourceBinding;
  Dependency: TTikZPropertyDependency;
  Trial: TBytes;
  Item: TTikZSourcePatch;
  Prefix: RawByteString;
  MergeFound: Boolean;

  function OptionInsertionPrefix(const OptionSpan: TTikZSourceSpan):
    RawByteString;
  var
    Position, LastSignificant, LineEnd, BackslashCount: SizeInt;
    Current: Byte;
  begin
    LastSignificant := -1;
    Position := OptionSpan.StartByte + 1;
    while Position < OptionSpan.EndByte - 1 do
    begin
      Current := Source[Position];
      if (Current = Ord(' ')) or (Current = Ord(#9)) or
        (Current = Ord(#10)) or (Current = Ord(#13)) then
      begin
        Inc(Position);
        Continue;
      end;
      if Current = Ord('%') then
      begin
        BackslashCount := 0;
        LineEnd := Position - 1;
        while (LineEnd >= OptionSpan.StartByte + 1) and
          (Source[LineEnd] = Ord('\')) do
        begin
          Inc(BackslashCount);
          Dec(LineEnd);
        end;
        if (BackslashCount mod 2) = 1 then
        begin
          LastSignificant := Position;
          Inc(Position);
          Continue;
        end;
        LineEnd := Position;
        while (LineEnd < OptionSpan.EndByte - 1) and
          not (Source[LineEnd] in [Ord(#10), Ord(#13)]) do Inc(LineEnd);
        Position := LineEnd;
        Continue;
      end;
      LastSignificant := Position;
      Inc(Position);
    end;
    if LastSignificant < 0 then Exit('');
    if Source[LastSignificant] = Ord(',') then Result := ' '
    else Result := ', ';
  end;

  function ComposeBytes(const PrefixText: RawByteString;
    const Value: TBytes): TBytes;
  begin
    Result := nil;
    SetLength(Result, Length(PrefixText) + Length(Value));
    if Length(PrefixText) > 0 then
      Move(PrefixText[1], Result[0], Length(PrefixText));
    if Length(Value) > 0 then
      Move(Value[0], Result[Length(PrefixText)], Length(Value));
  end;

begin
  Patches := nil;
  Error := tpeNone;
  Result := False;
  if not ValidSpan(EditableRegion, Length(Source)) then
  begin
    Error := tpeInvalidRegion;
    Exit;
  end;
  SetLength(Patches, Length(Edits));
  PatchCount := 0;
  for I := 0 to High(Edits) do
  begin
    BindingIndex := Edits[I].BindingIndex;
    DependencyIndex := Edits[I].DependencyIndex;
    if (BindingIndex < 0) or (BindingIndex >= Length(Bindings)) then
    begin
      Error := tpeInvalidBinding;
      Patches := nil;
      Exit;
    end;
    Binding := Bindings[BindingIndex];
    if not ValidSpan(Binding.StatementSpan, Length(Source)) or
      (Binding.StatementSpan.StartByte < EditableRegion.StartByte) or
      (Binding.StatementSpan.EndByte > EditableRegion.EndByte) or
      (DependencyIndex < 0) or
      (DependencyIndex >= Length(Binding.Dependencies)) then
    begin
      Error := tpeInvalidDependency;
      Patches := nil;
      Exit;
    end;
    Dependency := Binding.Dependencies[DependencyIndex];
    if Edits[I].LocalExpressionOverride then
    begin
      if not Dependency.HasLocalOverride or
        (Dependency.DependencySource = tdepUnsupported) or
        not ValidSpan(Dependency.ValueSpan, Length(Source)) or
        (Dependency.ValueSpan.StartByte < Binding.StatementSpan.StartByte) or
        (Dependency.ValueSpan.EndByte > Binding.StatementSpan.EndByte) then
      begin
        Error := tpeUnsupportedDependency;
        Patches := nil;
        Exit;
      end;
      Patches[PatchCount].Span := Dependency.ValueSpan;
      Patches[PatchCount].Replacement := Copy(Edits[I].Replacement);
      Inc(PatchCount);
      Continue;
    end;
    if Dependency.DependencySource in [tdepLocalLiteral, tdepLocalStyle] then
    begin
      if not ValidSpan(Dependency.ValueSpan, Length(Source)) or
        (Dependency.ValueSpan.StartByte < Binding.StatementSpan.StartByte) or
        (Dependency.ValueSpan.EndByte > Binding.StatementSpan.EndByte) then
      begin
        Error := tpePatchOutsideRegion;
        Patches := nil;
        Exit;
      end;
      Patches[PatchCount].Span := Dependency.ValueSpan;
      Patches[PatchCount].Replacement := Copy(Edits[I].Replacement);
      Inc(PatchCount);
    end;
    if not (Dependency.DependencySource in
      [tdepLocalLiteral, tdepLocalStyle]) then
    begin
      if not Dependency.HasLocalOverride or
        (Length(Edits[I].LocalOverrideOption) = 0) then
      begin
        Error := tpeUnsupportedDependency;
        Patches := nil;
        Exit;
      end;
      if not ValidSpan(Dependency.OptionListSpan, Length(Source)) or
        (Dependency.OptionListSpan.EndByte -
          Dependency.OptionListSpan.StartByte < 2) or
        (Dependency.OptionListSpan.StartByte < Binding.StatementSpan.StartByte) or
        (Dependency.OptionListSpan.EndByte > Binding.StatementSpan.EndByte) or
        (Source[Dependency.OptionListSpan.StartByte] <> Ord('[')) or
        (Source[Dependency.OptionListSpan.EndByte - 1] <> Ord(']')) then
      begin
        Error := tpeUnsupportedDependency;
        Patches := nil;
        Exit;
      end;
      InsertAt := Dependency.OptionListSpan.EndByte - 1;
      MergeFound := False;
      for J := 0 to PatchCount - 1 do
        if (Patches[J].Span.StartByte = InsertAt) and
          (Patches[J].Span.EndByte = InsertAt) then
        begin
          PreviousLength := Length(Patches[J].Replacement);
          OptionEnd := Length(Edits[I].LocalOverrideOption);
          SetLength(Patches[J].Replacement, PreviousLength + 2 + OptionEnd);
          Patches[J].Replacement[PreviousLength] := Ord(',');
          Patches[J].Replacement[PreviousLength + 1] := Ord(' ');
          Move(Edits[I].LocalOverrideOption[0],
            Patches[J].Replacement[PreviousLength + 2], OptionEnd);
          MergeFound := True;
          Break;
        end;
      if not MergeFound then
      begin
        Prefix := OptionInsertionPrefix(Dependency.OptionListSpan);
        Patches[PatchCount].Span.StartByte := InsertAt;
        Patches[PatchCount].Span.EndByte := InsertAt;
        Patches[PatchCount].Replacement := ComposeBytes(Prefix,
          Edits[I].LocalOverrideOption);
        Inc(PatchCount);
      end;
    end;
  end;
  SetLength(Patches, PatchCount);

  { Sort by source offset so selection order does not affect patch validity. }
  for I := 1 to High(Patches) do
  begin
    Item := Patches[I];
    J := I - 1;
    while (J >= 0) and (Patches[J].Span.StartByte > Item.Span.StartByte) do
    begin
      Patches[J + 1] := Patches[J];
      Dec(J);
    end;
    Patches[J + 1] := Item;
  end;

  if not ApplyTikZSourcePatches(Source, EditableRegion, Patches, Trial,
    Error) then
  begin
    Patches := nil;
    Exit;
  end;
  Result := True;
end;

function ApplyValidatedTikZSourcePatches(const Source: TBytes;
  const EditableRegion: TTikZSourceSpan;
  const Patches: array of TTikZSourcePatch;
  const InputKind: TTikZInputKind;
  out Updated: TBytes; out Error: TTikZPatchError): Boolean;
var
  Options: TTikZParseOptions;
  Parsed: TTikZSyntaxResult;
begin
  if not ApplyTikZSourcePatches(Source, EditableRegion, Patches,
    Updated, Error) then Exit(False);
  Options := DefaultTikZParseOptions(InputKind);
  Parsed := ParseTikZ(Updated, Options);
  try
    if Parsed.Outcome in [tpoInvalid, tpoCancelled] then
    begin
      Error := tpeInvalidCandidate;
      Updated := nil;
      Exit(False);
    end;
    if not Parsed.Region.Found then
    begin
      Error := tpeMissingPicture;
      Updated := nil;
      Exit(False);
    end;
    Error := tpeNone;
    Result := True;
  finally
    Parsed.Free;
  end;
end;

function ApplyTikZBoundEdits(Context: TTikZImportContext;
  const Edits: array of TTikZBoundEdit;
  const InputKind: TTikZInputKind;
  out Updated: TBytes; out Error: TTikZPatchError): Boolean;
var
  Source: TBytes;
  Bindings: TTikZSourceBindings;
  Patches: TTikZSourcePatches;
  I: SizeInt;
begin
  Updated := nil;
  if (Context = nil) or (Context.Document = nil) or
    (Context.Scene = nil) then
  begin
    Error := tpeInvalidBinding;
    Exit(False);
  end;
  Source := Context.CopySource;
  SetLength(Bindings, Context.BindingCount);
  for I := 0 to High(Bindings) do Bindings[I] := Context.BindingAt(I);
  if not BuildTikZPropertyPatches(Source, Context.Document.Region.Span,
    Bindings, Edits, Patches, Error) then Exit(False);
  Result := ApplyValidatedTikZSourcePatches(Source,
    Context.Document.Region.Span, Patches, InputKind, Updated, Error);
end;

function SupportsTikZStructuralOperation(
  const Operation: TTikZStructuralOperation): Boolean;
begin
  Result := Operation in [tsoInsertStatement, tsoDeleteStatement];
end;

function TikZSignificantDigits(RealTypeSize: SizeInt): Integer;
begin
  if RealTypeSize <= 4 then Result := 9
  else if RealTypeSize <= 8 then Result := 17
  else Result := 21;
end;

function FormatTikZReal(const Value: Extended;
  const RealTypeSize: SizeInt): string;
var
  Settings: TFormatSettings;
begin
  if IsNan(Value) or IsInfinite(Value) then
    raise EConvertError.Create('TikZ coordinates must be finite numbers');
  Settings := DefaultFormatSettings;
  Settings.DecimalSeparator := '.';
  Settings.ThousandSeparator := ',';
  Result := FloatToStrF(Value, ffGeneral,
    TikZSignificantDigits(RealTypeSize), 0, Settings);
end;

function TikZCoordinateScalar(const ValueMM, RelativeBaseMM,
  ValueScaleToMM: Extended; const IsRelative: Boolean): Extended;
begin
  if IsNan(ValueMM) or IsInfinite(ValueMM) or
    IsNan(ValueScaleToMM) or IsInfinite(ValueScaleToMM) or
    (ValueScaleToMM = 0) then
    raise EConvertError.Create('TikZ coordinate scale and values must be finite');
  if IsRelative then
  begin
    if IsNan(RelativeBaseMM) or IsInfinite(RelativeBaseMM) then
      raise EConvertError.Create('TikZ relative coordinate base must be finite');
    Result := (ValueMM - RelativeBaseMM) / ValueScaleToMM;
  end
  else Result := ValueMM / ValueScaleToMM;
end;

function FormatTikZCoordinate(const ValueMM, RelativeBaseMM,
  ValueScaleToMM: Extended; const IsRelative: Boolean;
  const RealTypeSize: SizeInt): string;
begin
  Result := FormatTikZReal(TikZCoordinateScalar(ValueMM, RelativeBaseMM,
    ValueScaleToMM, IsRelative), RealTypeSize);
end;

end.
