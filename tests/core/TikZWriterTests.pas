unit TikZWriterTests;

{$mode delphi}{$H+}

interface

implementation

uses SysUtils, Math, CoreTestSupport, TikZLexer, TikZSyntax, TikZImport,
  TikZWriter;

function BytesFromRaw(const Value: RawByteString): TBytes;
begin
  Result := nil;
  SetLength(Result, Length(Value));
  if Length(Value) > 0 then Move(Value[1], Result[0], Length(Value));
end;

function RawFromBytes(const Value: TBytes): RawByteString;
begin
  SetLength(Result, Length(Value));
  if Length(Value) > 0 then Move(Value[0], Result[1], Length(Value));
end;

function SpanOf(const Source, Needle: RawByteString;
  const SearchFrom: SizeInt = 0): TTikZSourceSpan;
var
  AtPosition: SizeInt;
begin
  AtPosition := Pos(Needle, Copy(Source, SearchFrom + 1, MaxInt));
  if (AtPosition = 0) or (Needle = '') then
    raise Exception.Create('TikZ test span not found: ' + string(Needle));
  Inc(AtPosition, SearchFrom);
  FillChar(Result, SizeOf(Result), 0);
  Result.StartByte := AtPosition - 1;
  Result.EndByte := Result.StartByte + Length(Needle);
end;

function BytePatch(const Span: TTikZSourceSpan;
  const Replacement: RawByteString): TTikZSourcePatch;
begin
  Result.Span := Span;
  Result.Replacement := BytesFromRaw(Replacement);
end;

const
  SourceText: RawByteString =
    #$EF#$BB#$BF'% user file ' + #$CE#$B2 + #13#10 +
    '\documentclass{standalone}' + #10 +
    '\begin{document}' + #13#10 +
    '% ignored text: \begin{tikzpicture}' + #13#10 +
    ' \begin{tikzpicture}[x=1cm]' + #13#10 +
    '  % keep this comment ' + #$E9 + #13#10 +
    '  \path[line width=0.75pt] (0.125,2.75) -- (3,4); % preserve ' +
      #$CE#$A9 + #13#10 +
    '  \node at (1,2) {left \% fake;};' + #13#10 +
    ' \end{tikzpicture}' + #13#10 +
    '\end{document}' + #10 + '% tail';
  ChangedText: RawByteString =
    #$EF#$BB#$BF'% user file ' + #$CE#$B2 + #13#10 +
    '\documentclass{standalone}' + #10 +
    '\begin{document}' + #13#10 +
    '% ignored text: \begin{tikzpicture}' + #13#10 +
    ' \begin{tikzpicture}[x=1cm]' + #13#10 +
    '  % keep this comment ' + #$E9 + #13#10 +
    '  \path[line width=1.25pt] (0.375,2.75) -- (3,4); % preserve ' +
      #$CE#$A9 + #13#10 +
    '  \node at (1,2) {right \% fake;};' + #13#10 +
    ' \end{tikzpicture}' + #13#10 +
    '\end{document}' + #10 + '% tail';
  SharedSourceText: RawByteString =
    '\begin{tikzpicture}' + #10 +
    '\path[line width=1*\tpxLineWidth] (0,0) -- (1,1);' + #10 +
    '\path[line width=1*\tpxLineWidth] (2,2) -- (3,3);' + #10 +
    '\end{tikzpicture}';

procedure TestSemanticCoordinateRoundTrip;
const
  SemanticSourceText: RawByteString =
    '\begin{tikzpicture}[x=1cm,y=0.25cm]' + #13#10 +
    '  % source-only fixture' + #13#10 +
    '  \draw (1,2) -- ++(0.5,0);' + #13#10 +
    '\end{tikzpicture}' + #13#10;
var
  Source, Updated: TBytes;
  Document, ReloadedDocument: TTikZSyntaxResult;
  Semantic, ReloadedSemantic: TTikZSemanticResult;
  Context: TTikZImportContext;
  SceneObject: TTikZSceneObject;
  Binding: TTikZSourceBinding;
  Edit: TTikZBoundEdit;
  DependencyIndex, I: SizeInt;
  Error: TTikZPatchError;
  UpdatedText: RawByteString;
begin
  Source := BytesFromRaw(SemanticSourceText);
  Document := ParseTikZ(Source,
    DefaultTikZParseOptions(tikTexInput));
  try
    CheckCore(Document.Outcome = tpoAccepted,
      'Metadata-free TikZ source did not parse');
    Semantic := EvaluateTikZ(Document, DefaultTikZImportOptions);
  finally
    Document.Free;
  end;
  try
    CheckCore((Semantic.Outcome = tsoAccepted) and
      (Semantic.Scene.ObjectCount = 1),
      'Metadata-free TikZ source did not materialize semantically');
    SceneObject := Semantic.Scene.ObjectAt(0);
    CheckCore((Length(SceneObject.Commands) = 2) and
      (Abs(SceneObject.Commands[1].P1.X - 15) < 1E-9),
      'The imported relative coordinate has the wrong physical position');
    Context := TTikZImportContext.Create(
      ParseTikZ(Source, DefaultTikZParseOptions(tikTexInput)),
      Semantic.DetachScene);
  finally
    Semantic.Free;
  end;
  try
    Context.BindNativeObject(0, 701);
    Binding := Context.BindingAt(Context.FindBindingForObjectID(701));
    DependencyIndex := -1;
    for I := 0 to High(Binding.Dependencies) do
      if (Binding.Dependencies[I].Kind = tdCoordinateX) and
        (Binding.Dependencies[I].CommandIndex = 1) and
        Binding.Dependencies[I].IsRelative then
      begin
        DependencyIndex := I;
        Break;
      end;
    CheckCore(DependencyIndex >= 0,
      'The relative coordinate has no source dependency binding');
    Edit.BindingIndex := Context.FindBindingForObjectID(701);
    Edit.DependencyIndex := DependencyIndex;
    Edit.LocalExpressionOverride := False;
    Edit.Replacement := BytesFromRaw(FormatTikZCoordinate(18, 10,
      Binding.Dependencies[DependencyIndex].ValueScaleToMM, True,
      SizeOf(Single)));
    CheckCore(ApplyTikZBoundEdits(Context, [Edit], tikTexInput, Updated,
      Error), 'A source-bound relative coordinate edit failed');
    UpdatedText := RawFromBytes(Updated);
    CheckCore(UpdatedText = StringReplace(SemanticSourceText,
      '++(0.5,0)', '++(0.8,0)', []),
      'A relative-coordinate edit changed unrelated source bytes');
  finally
    Context.Free;
  end;

  ReloadedDocument := ParseTikZ(Updated,
    DefaultTikZParseOptions(tikTexInput));
  try
    ReloadedSemantic := EvaluateTikZ(ReloadedDocument,
      DefaultTikZImportOptions);
  finally
    ReloadedDocument.Free;
  end;
  try
    CheckCore((ReloadedSemantic.Outcome = tsoAccepted) and
      (ReloadedSemantic.Scene.ObjectCount = 1),
      'The saved source could not be imported again');
    CheckCore(Abs(ReloadedSemantic.Scene.ObjectAt(0).Commands[1].P1.X -
      18) < 1E-9,
      'Reloaded geometry does not match the independently requested edit');
  finally
    ReloadedSemantic.Free;
  end;
end;

procedure TestSharedDefaultOverride;
var
  Source, Updated: TBytes;
  Region: TTikZSourceSpan;
  Bindings: TTikZSourceBindings;
  Edits: array[0..0] of TTikZBoundEdit;
  Patches: TTikZSourcePatches;
  Error: TTikZPatchError;
  Expected: RawByteString;
begin
  Source := BytesFromRaw(SharedSourceText);
  Region := SpanOf(SharedSourceText, '\begin{tikzpicture}');
  Region.EndByte := SpanOf(SharedSourceText, '\end{tikzpicture}',
    Region.StartByte).EndByte;
  SetLength(Bindings, 1);
  Bindings[0].StatementSpan := SpanOf(SharedSourceText,
    '\path[line width=1*\tpxLineWidth] (0,0) -- (1,1);');
  SetLength(Bindings[0].Dependencies, 1);
  Bindings[0].Dependencies[0].Kind := tdLineWidth;
  Bindings[0].Dependencies[0].DependencySource := tdepSharedDefaultMacro;
  Bindings[0].Dependencies[0].HasLocalOverride := True;
  Bindings[0].Dependencies[0].OptionListSpan := SpanOf(SharedSourceText,
    '[line width=1*\tpxLineWidth]');
  Edits[0].BindingIndex := 0;
  Edits[0].DependencyIndex := 0;
  Edits[0].LocalExpressionOverride := False;
  Edits[0].LocalOverrideOption := BytesFromRaw('line width=0.625mm');
  CheckCore(BuildTikZPropertyPatches(Source, Region, Bindings, Edits,
    Patches, Error), 'A safe local override for a shared default was rejected');
  CheckCore(ApplyValidatedTikZSourcePatches(Source, Region, Patches,
    tikTexInput, Updated, Error),
    'A shared default could not be overridden locally');
  Expected :=
    '\begin{tikzpicture}' + #10 +
    '\path[line width=1*\tpxLineWidth, line width=0.625mm] (0,0) -- (1,1);' + #10 +
    '\path[line width=1*\tpxLineWidth] (2,2) -- (3,3);' + #10 +
    '\end{tikzpicture}';
  CheckCore(RawFromBytes(Updated) = Expected,
    'A local override changed the shared default or another object');
end;

procedure TestLocalTextSizeExpressionPatch;
const
  TextSource: RawByteString =
    '\providecommand{\tpxTextSize}{10pt}' +
    '\begin{tikzpicture}' + #10 +
    '\node at (0,0) {\pgfmathsetlengthmacro{\tpxObjectFontSize}{1*\tpxTextSize}' +
      '\pgfmathsetlengthmacro{\tpxObjectBaseline}{1.2*\tpxTextSize}' +
      '\fontsize{\tpxObjectFontSize}{\tpxObjectBaseline}\selectfont a};' + #10 +
    '\node at (1,0) {\pgfmathsetlengthmacro{\tpxObjectFontSize}{1*\tpxTextSize}' +
      '\pgfmathsetlengthmacro{\tpxObjectBaseline}{1.2*\tpxTextSize}' +
      '\fontsize{\tpxObjectFontSize}{\tpxObjectBaseline}\selectfont b};' + #10 +
    '\end{tikzpicture}';
var
  Source, Updated: TBytes;
  Edits: array[0..1] of TTikZBoundEdit;
  Syntax, ReloadedSyntax: TTikZSyntaxResult;
  Semantic, ReloadedSemantic: TTikZSemanticResult;
  Context: TTikZImportContext;
  Binding: TTikZSourceBinding;
  FontIndex, BaselineIndex, I: SizeInt;
  BaseTextSize: Extended;
  Error: TTikZPatchError;
  Expected: RawByteString;
begin
  Source := BytesFromRaw(TextSource);
  Context := nil;
  Syntax := ParseTikZ(Source, DefaultTikZParseOptions(tikTexInput));
  Semantic := nil;
  ReloadedSyntax := nil;
  ReloadedSemantic := nil;
  try
    CheckCore(Syntax.Outcome = tpoAccepted,
      'Generated text-size fixture did not parse');
    Semantic := EvaluateTikZ(Syntax, DefaultTikZImportOptions);
    CheckCore((Semantic.Outcome = tsoAccepted) and
      (Semantic.Scene.ObjectCount = 2),
      'Generated text-size fixture did not import two nodes');
    Context := TTikZImportContext.Create(Syntax, Semantic.DetachScene);
    Syntax := nil;
    BaseTextSize := Context.Scene.TextSizeMM;
    Binding := Context.BindingAt(0);
    FontIndex := -1;
    BaselineIndex := -1;
    for I := 0 to High(Binding.Dependencies) do
      if Binding.Dependencies[I].Kind = tdTextSize then FontIndex := I
      else if Binding.Dependencies[I].Kind = tdTextBaseline then
        BaselineIndex := I;
    CheckCore((FontIndex >= 0) and (BaselineIndex >= 0),
      'Generated text-size dependencies lack a font or baseline span');
    Edits[0].BindingIndex := 0;
    Edits[0].DependencyIndex := FontIndex;
    Edits[0].Replacement := BytesFromRaw('1.5*\tpxTextSize');
    Edits[0].LocalExpressionOverride := True;
    Edits[1].BindingIndex := 0;
    Edits[1].DependencyIndex := BaselineIndex;
    Edits[1].Replacement := BytesFromRaw('1.8*\tpxTextSize');
    Edits[1].LocalExpressionOverride := True;
    CheckCore(ApplyTikZBoundEdits(Context, Edits, tikTexInput, Updated,
      Error), 'Local text-size expressions were not patchable');
  finally
    if Syntax <> nil then Syntax.Free;
    if Semantic <> nil then Semantic.Free;
    Context.Free;
  end;
  Expected := StringReplace(TextSource, '1*\tpxTextSize',
    '1.5*\tpxTextSize', []);
  Expected := StringReplace(Expected, '1.2*\tpxTextSize',
    '1.8*\tpxTextSize', []);
  CheckCore(RawFromBytes(Updated) = Expected,
    'Changing one text object modified another object or its shared default');
  ReloadedSyntax := ParseTikZ(Updated,
    DefaultTikZParseOptions(tikTexInput));
  try
    ReloadedSemantic := EvaluateTikZ(ReloadedSyntax,
      DefaultTikZImportOptions);
  finally
    ReloadedSyntax.Free;
  end;
  try
    CheckCore((ReloadedSemantic.Outcome = tsoAccepted) and
      (ReloadedSemantic.Scene.ObjectCount = 2),
      'Saved local text-size expressions could not be imported');
    CheckCore((Abs(ReloadedSemantic.Scene.ObjectAt(0).TextHeightMM -
      1.5 * BaseTextSize) < 0.00001) and
      (Abs(ReloadedSemantic.Scene.ObjectAt(0).TextBaselineMM -
      1.8 * BaseTextSize) < 0.00001),
      'Saved local font or baseline expression has the wrong physical size');
    CheckCore((Abs(ReloadedSemantic.Scene.ObjectAt(1).TextHeightMM -
      BaseTextSize) < 0.00001) and
      (Abs(ReloadedSemantic.Scene.ObjectAt(1).TextBaselineMM -
      1.2 * BaseTextSize) < 0.00001),
      'A local text-size edit changed a sibling node or the shared default');
  finally
    ReloadedSemantic.Free;
  end;
end;

procedure TestTikZWriter;
const
  Values: array[0..3] of Single =
    (0.1234567, -0.00003141593, 12.345678, 17.375);
  Steps: array[0..3] of Single =
    (0.00001, 0.000000001, 0.0001, 0.0001);
var
  Source, Output, NoChange, Inserted, Deleted, InvalidOutput: TBytes;
  Region, PointSpan, WidthSpan, TextSpan, InsertSpan, DeleteSpan:
    TTikZSourceSpan;
  Patches: TTikZSourcePatches;
  OnePatch: array[0..0] of TTikZSourcePatch;
  BadPatches: array[0..1] of TTikZSourcePatch;
  Bindings: TTikZSourceBindings;
  Edits: array[0..0] of TTikZBoundEdit;
  Scene: TTikZScene;
  SceneObject: TTikZSceneObject;
  Binding: TTikZSourceBinding;
  ParsedDoc: TTikZSyntaxResult;
  Context: TTikZImportContext;
  CycleContext: TTikZImportContext;
  CycleSemantic: TTikZSemanticResult;
  CycleBinding: TTikZSourceBinding;
  CycleEdit: TTikZBoundEdit;
  Error: TTikZPatchError;
  InsertExpected, DeleteExpected: RawByteString;
  I, J, Cycle, CycleBindingIndex, CycleDependencyIndex: Integer;
  CurrentValue: Single;
  OriginalValue, RecoveredValue, Tolerance: Extended;
  ExpectedX, ExpectedY, ActualX, ActualY, AngleRadians, CosAngle,
    SinAngle, SourceY: Extended;
  NextValue, OracleValue: Single;
  CycleInput, CycleOutput: TBytes;
  CycleDoc: TTikZSyntaxResult;
  CycleSource, CycleExpected, CyclePrefix, CycleSuffix: RawByteString;
  ValueBefore: TFormatSettings;
  NumericText: string;
begin
  TestSemanticCoordinateRoundTrip;
  TestSharedDefaultOverride;
  TestLocalTextSizeExpressionPatch;
  Source := BytesFromRaw(SourceText);
  Region := SpanOf(SourceText, '\begin{tikzpicture}[x=1cm]');
  Region.EndByte := SpanOf(SourceText, '\end{tikzpicture}',
    Region.StartByte).EndByte;

  CheckCore(ApplyTikZSourcePatches(Source, Region, [], NoChange, Error),
    'No-op TikZ save failed');
  CheckCore((Error = tpeNone) and (RawFromBytes(NoChange) = SourceText),
    'No-op TikZ save changed BOM, mixed line endings, Unicode or outer TeX');
  CheckCore(ApplyValidatedTikZSourcePatches(Source, Region, [], tikTexInput,
    NoChange, Error) and (RawFromBytes(NoChange) = SourceText),
    'A valid no-op candidate did not pass syntax validation unchanged');

  PointSpan := SpanOf(SourceText, '0.125');
  WidthSpan := SpanOf(SourceText, '0.75pt');
  TextSpan := SpanOf(SourceText, 'left');
  SetLength(Patches, 3);
  Patches[0] := BytePatch(WidthSpan, '1.25pt');
  Patches[1] := BytePatch(PointSpan, '0.375');
  Patches[2] := BytePatch(TextSpan, 'right');
  CheckCore(ApplyTikZSourcePatches(Source, Region, Patches, Output, Error),
    'A supported local edit plan was rejected');
  CheckCore((Error = tpeNone) and (RawFromBytes(Output) = ChangedText),
    'Local edits changed bytes outside the selected value spans');
  CheckCore(RawFromBytes(Source) = SourceText,
    'Applying a patch mutated the retained original source');

  SetLength(Bindings, 1);
  Bindings[0].StatementSpan := SpanOf(SourceText,
    '\path[line width=0.75pt] (0.125,2.75) -- (3,4);');
  SetLength(Bindings[0].Dependencies, 1);
  Bindings[0].Dependencies[0].Kind := tdCoordinateX;
  Bindings[0].Dependencies[0].DependencySource := tdepLocalLiteral;
  Bindings[0].Dependencies[0].ValueSpan := PointSpan;
  Bindings[0].Dependencies[0].OptionListSpan :=
    SpanOf(SourceText, '[line width=0.75pt]');
  Edits[0].BindingIndex := 0;
  Edits[0].DependencyIndex := 0;
  Edits[0].Replacement := BytesFromRaw('0.375');
  Edits[0].LocalExpressionOverride := False;
  CheckCore(BuildTikZPropertyPatches(Source, Region, Bindings, Edits,
    Patches, Error), 'A local literal dependency did not produce a patch');
  CheckCore(ApplyValidatedTikZSourcePatches(Source, Region, Patches,
    tikTexInput, Output, Error) and
    (RawFromBytes(Output) = StringReplace(SourceText, '0.125', '0.375', [])),
    'A dependency-indexed edit changed nonlocal source bytes');
  Bindings[0].Dependencies[0].DependencySource := tdepSharedDefaultMacro;
  CheckCore(not BuildTikZPropertyPatches(Source, Region, Bindings, Edits,
    Patches, Error) and (Error = tpeUnsupportedDependency),
    'An edit to a shared dependency was not rejected as unsafe');
  Bindings[0].Dependencies[0].HasLocalOverride := True;
  Edits[0].LocalOverrideOption := BytesFromRaw('line width=2pt');
  CheckCore(BuildTikZPropertyPatches(Source, Region, Bindings, Edits,
    Patches, Error), 'A dependency with a safe local override was rejected');
  CheckCore(ApplyValidatedTikZSourcePatches(Source, Region, Patches,
    tikTexInput, Output, Error) and
    (RawFromBytes(Output) = StringReplace(SourceText,
      '[line width=0.75pt]', '[line width=0.75pt, line width=2pt]', [])),
    'A dependency edit changed bytes beyond its local option list');

  Scene := TTikZScene.Create;
  SceneObject := TTikZSceneObject.Create;
  SceneObject.SourceSpan := Bindings[0].StatementSpan;
  Scene.AddObject(SceneObject);
  Binding := Bindings[0];
  Binding.ObjectIndex := 0;
  Binding.NativeObjectID := 17;
  Binding.Dependencies[0].Kind := tdCoordinateX;
  Binding.Dependencies[0].DependencySource := tdepLocalLiteral;
  Binding.Dependencies[0].ValueSpan := PointSpan;
  Scene.AddBinding(Binding);
  ParsedDoc := ParseTikZ(Source, DefaultTikZParseOptions(tikTexInput));
  Context := TTikZImportContext.Create(ParsedDoc, Scene);
  try
    CheckCore(Context.FindBindingForObjectID(17) = 0,
      'Source bindings did not resolve by native object identity');
    CheckCore(ApplyTikZBoundEdits(Context, [], tikTexInput, NoChange, Error) and
      (RawFromBytes(NoChange) = SourceText),
      'Context-based no-op save did not preserve exact source bytes');
    Edits[0].LocalOverrideOption := nil;
    Edits[0].Replacement := BytesFromRaw('0.375');
    CheckCore(ApplyTikZBoundEdits(Context, Edits, tikTexInput, Output, Error) and
      (RawFromBytes(Output) = StringReplace(SourceText, '0.125', '0.375', [])),
      'Context-based local edit did not produce the expected source');
    Context.BindNativeObject(0, 29);
    CheckCore((Context.FindBindingForObjectID(17) = -1) and
      (Context.FindBindingForObjectID(29) = 0),
      'Source binding did not follow its stable native object ID');
  finally
    Context.Free;
  end;

  OnePatch[0] := BytePatch(PointSpan, '(');
  CheckCore(not ApplyValidatedTikZSourcePatches(Source, Region, OnePatch,
    tikTexInput, InvalidOutput, Error) and
    (Error = tpeInvalidCandidate) and (Length(InvalidOutput) = 0),
    'Malformed candidate output was accepted for save');
  CheckCore(RawFromBytes(Source) = SourceText,
    'Rejected candidate validation changed retained original source');

  BadPatches[0] := BytePatch(PointSpan, '1');
  BadPatches[1] := BytePatch(SpanOf(SourceText, '125'), '9');
  CheckCore(not ApplyTikZSourcePatches(Source, Region, BadPatches, Output,
    Error) and (Error = tpeOverlappingPatches),
    'Overlapping source edits were accepted');
  CheckCore(RawFromBytes(Source) = SourceText,
    'Rejecting a patch plan changed the original source');

  InsertSpan := SpanOf(SourceText, '  \node');
  InsertSpan.EndByte := InsertSpan.StartByte;
  OnePatch[0] := BytePatch(InsertSpan,
    '  \draw (5,6) -- (7,8);' + #13#10);
  CheckCore(ApplyValidatedTikZSourcePatches(Source, Region, OnePatch,
    tikTexInput, Inserted, Error),
    'Statement insertion at a supplied local boundary failed');
  InsertExpected := Copy(SourceText, 1, InsertSpan.StartByte) +
    '  \draw (5,6) -- (7,8);' + #13#10 +
    Copy(SourceText, InsertSpan.StartByte + 1, MaxInt);
  CheckCore(RawFromBytes(Inserted) = InsertExpected,
    'Statement insertion changed source outside its insertion point');

  DeleteSpan := SpanOf(SourceText, '\node at (1,2) {left \% fake;};');
  OnePatch[0] := BytePatch(DeleteSpan, '');
  CheckCore(ApplyValidatedTikZSourcePatches(Source, Region, OnePatch,
    tikTexInput, Deleted, Error),
    'Statement deletion inside the selected picture failed');
  DeleteExpected := Copy(SourceText, 1, DeleteSpan.StartByte) +
    Copy(SourceText, DeleteSpan.EndByte + 1, MaxInt);
  CheckCore(RawFromBytes(Deleted) = DeleteExpected,
    'Statement deletion changed bytes outside the owned statement span');
  CheckCore(SupportsTikZStructuralOperation(tsoInsertStatement) and
    SupportsTikZStructuralOperation(tsoDeleteStatement) and
    not SupportsTikZStructuralOperation(tsoReorderStatement) and
    not SupportsTikZStructuralOperation(tsoGroupStatements),
    'Unsafe reorder/group structural operations were not rejected');

  ValueBefore := DefaultFormatSettings;
  try
    DefaultFormatSettings.DecimalSeparator := ',';
    CheckCore((Pos('.', FormatTikZReal(Values[0], SizeOf(Single))) > 0) and
      (Pos(',', FormatTikZReal(Values[0], SizeOf(Single))) = 0),
      'TikZ numeric output followed the active locale');
    CheckCore(FormatTikZReal(Values[3], SizeOf(Single)) = '17.375',
      'Fractional rotation was rounded to an integer');
    CheckCore(FormatTikZReal(Values[1], SizeOf(Single)) <> '0',
      'Small radius was rounded to zero');
    CheckCore(FormatTikZCoordinate(25, 15, 10, True, SizeOf(Single)) = '1',
      'A relative coordinate did not preserve its physical displacement');
    CheckCore(FormatTikZCoordinate(6.5, 0, 2.5, False,
      SizeOf(Single)) = '2.6',
      'A coordinate edit did not convert back to its explicit source unit');

    CyclePrefix := '\begin{tikzpicture}[x=3mm,y=0.25cm]' + #10 +
      '\begin{scope}[rotate=17.375]' + #10 + '\draw (';
    CycleSuffix := ',-0.00003141593) -- (12.345678,4.56789);' + #10 +
      '\end{scope}' + #10 +
      '\draw (0.125,0.25) circle (0.00003141593pt);' + #10 +
      '\end{tikzpicture}';
    SourceY := -0.00003141593 * 2.5;
    AngleRadians := 17.375 * Pi / 180;
    CosAngle := Cos(AngleRadians);
    SinAngle := Sin(AngleRadians);
    for I := Low(Values) to High(Values) do
    begin
      CurrentValue := Values[I];
      OracleValue := Values[I];
      for Cycle := 1 to 100 do
      begin
        NumericText := FormatTikZReal(CurrentValue, SizeOf(Single));
        CycleSource := CyclePrefix + NumericText + CycleSuffix;
        CycleInput := BytesFromRaw(CycleSource);
        CycleDoc := ParseTikZ(CycleInput,
          DefaultTikZParseOptions(tikTexInput));
        CycleSemantic := nil;
        CycleContext := nil;
        try
          CheckCore((CycleDoc.Outcome in [tpoAccepted, tpoUnsupported]) and
            CycleDoc.Region.Found,
            'A numeric cycle source did not parse as a picture');
          CycleSemantic := EvaluateTikZ(CycleDoc,
            DefaultTikZImportOptions);
          CheckCore((CycleSemantic.Outcome = tsoAccepted) and
            (CycleSemantic.Scene.ObjectCount = 2),
            'A numeric cycle source did not evaluate (outcome=' +
            IntToStr(Ord(CycleSemantic.Outcome)) + ', objects=' +
            IntToStr(CycleSemantic.Scene.ObjectCount) + ')');
          CycleContext := TTikZImportContext.Create(CycleDoc,
            CycleSemantic.DetachScene);
          CycleDoc := nil;
        finally
          if CycleDoc <> nil then CycleDoc.Free;
          if CycleSemantic <> nil then CycleSemantic.Free;
        end;

        CycleBindingIndex := 0;
        CycleBinding := CycleContext.BindingAt(CycleBindingIndex);
        CycleDependencyIndex := -1;
        for J := 0 to High(CycleBinding.Dependencies) do
          if (CycleBinding.Dependencies[J].Kind = tdCoordinateX) and
            (CycleBinding.Dependencies[J].CommandIndex = 0) and
            (CycleBinding.Dependencies[J].PointSlot = 1) then
          begin
            CycleDependencyIndex := J;
            Break;
          end;
        CheckCore(CycleDependencyIndex >= 0,
          'A numeric cycle lost its first coordinate source binding');
        NextValue := CurrentValue + Steps[I];
        OracleValue := OracleValue + Steps[I];
        CycleEdit.BindingIndex := CycleBindingIndex;
        CycleEdit.DependencyIndex := CycleDependencyIndex;
        CycleEdit.LocalOverrideOption := nil;
        CycleEdit.LocalExpressionOverride := False;
        CycleEdit.Replacement := BytesFromRaw(FormatTikZReal(NextValue,
          SizeOf(Single)) + CycleBinding.Dependencies[
          CycleDependencyIndex].ValueUnit);
        try
          CheckCore(ApplyTikZBoundEdits(CycleContext, [CycleEdit],
            tikTexInput, CycleOutput, Error),
            'A source-bound numeric edit failed to build');
        finally
          CycleContext.Free;
        end;
        CycleExpected := CyclePrefix +
          FormatTikZReal(NextValue, SizeOf(Single)) + CycleSuffix;
        CheckCore(RawFromBytes(CycleOutput) = CycleExpected,
          'A numeric edit changed untouched rotation, mixed-unit or radius literals');
        CycleDoc := ParseTikZ(CycleOutput,
          DefaultTikZParseOptions(tikTexInput));
        try
          CycleSemantic := EvaluateTikZ(CycleDoc,
            DefaultTikZImportOptions);
        finally
          CycleDoc.Free;
        end;
        try
          CheckCore((CycleSemantic.Outcome = tsoAccepted) and
            (CycleSemantic.Scene.ObjectCount = 2),
            'A saved source-bound numeric cycle could not be reloaded');
          ActualX := CycleSemantic.Scene.ObjectAt(0).Commands[0].P1.X;
          ActualY := CycleSemantic.Scene.ObjectAt(0).Commands[0].P1.Y;
          ExpectedX := CosAngle * (NextValue * 3) - SinAngle * SourceY;
          ExpectedY := SinAngle * (NextValue * 3) + CosAngle * SourceY;
          CheckCore((Abs(ActualX - ExpectedX) <= 0.00001) and
            (Abs(ActualY - ExpectedY) <= 0.00001),
            'Reloaded coordinates disagree with the independent affine oracle');
          CheckCore(Abs(CycleSemantic.Scene.ObjectAt(1).RadiusX -
            0.00003141593 * 25.4 / 72.27) <= 0.0000000001,
            'An untouched small mixed-unit radius changed after a cycle: ' +
            'kind=' + IntToStr(Ord(CycleSemantic.Scene.ObjectAt(1).Kind)) +
            ', radius=' + FloatToStr(CycleSemantic.Scene.ObjectAt(1).RadiusX));
          RecoveredValue := (CosAngle * ActualX + SinAngle * ActualY) / 3;
          CheckCore(Abs(RecoveredValue - OracleValue) <=
            0.00000002 * Max(1.0, Abs(OracleValue)),
            'A source-bound cycle did not preserve its numerical oracle value');
        finally
          CycleSemantic.Free;
        end;
        CurrentValue := RecoveredValue;
      end;
      OriginalValue := Values[I] + 100 * Steps[I];
      RecoveredValue := CurrentValue;
      if I = 0 then Tolerance := 0.000002
      else if I = 1 then Tolerance := 0.000000002
      else Tolerance := 0.0001;
      CheckCore(Abs(RecoveredValue - OriginalValue) <= Tolerance,
        'Numeric edits exceeded the 100-cycle precision tolerance');
      CheckCore(FormatTikZReal(CurrentValue, SizeOf(Single)) =
        FormatTikZReal(RecoveredValue, SizeOf(Single)),
        'Numeric text drifted after repeated save/reload cycles');
    end;
  finally
    DefaultFormatSettings := ValueBefore;
  end;
end;

initialization
  RegisterCoreTest('tikz-writer-fidelity', TestTikZWriter);

end.
