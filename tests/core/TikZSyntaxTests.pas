unit TikZSyntaxTests;

{$mode delphi}{$H+}{$codepage utf8}

interface

implementation

uses SysUtils, Classes, TikZLexer, TikZSyntax, CoreTestSupport;

function FixtureDirectory: string;
begin
  Result := GetEnvironmentVariable('TPX_CORE_FIXTURE_DIR');
  CheckCore(Result <> '', 'TPX_CORE_FIXTURE_DIR is not set');
  Result := IncludeTrailingPathDelimiter(Result) + 'tikz';
end;

function LoadFixture(const Name: string): TBytes;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(IncludeTrailingPathDelimiter(FixtureDirectory) +
    Name, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Result, Stream.Size);
    if Stream.Size > 0 then Stream.ReadBuffer(Result[0], Stream.Size);
  finally
    Stream.Free;
  end;
end;

function BytesOf(const Value: RawByteString): TBytes;
var
  I: SizeInt;
begin
  SetLength(Result, Length(Value));
  for I := 1 to Length(Value) do Result[I - 1] := Ord(Value[I]);
end;

function BytesText(const Value: TBytes): RawByteString;
var
  I: SizeInt;
begin
  SetLength(Result, Length(Value));
  for I := 0 to High(Value) do Result[I + 1] := AnsiChar(Value[I]);
end;

function SameBytes(const A, B: TBytes): Boolean;
var
  I: SizeInt;
begin
  Result := Length(A) = Length(B);
  if not Result then Exit;
  for I := 0 to High(A) do
    if A[I] <> B[I] then Exit(False);
end;

function HasDiagnostic(const Parsed: TTikZSyntaxResult;
  Code: TTikZDiagnosticCode): Boolean;
var
  I: SizeInt;
begin
  for I := 0 to Parsed.DiagnosticCount - 1 do
    if Parsed.DiagnosticAt(I).Code = Code then Exit(True);
  Result := False;
end;

function ParseBytes(const Source: TBytes; Kind: TTikZInputKind):
  TTikZSyntaxResult;
var
  Options: TTikZParseOptions;
begin
  Options := DefaultTikZParseOptions(Kind);
  Result := ParseTikZ(Source, Options);
end;

function EmptySpan(StartByte, EndByte: SizeInt): TTikZSourceSpan; forward;

procedure TestWriterFixtureAndByteRetention;
var
  Source, Replay: TBytes;
  Parsed: TTikZSyntaxResult;
  I, Semicolons, GroupsWithParents: SizeInt;
  Token: TTikZToken;
begin
  Source := LoadFixture('devtikz-emitted.tikz');
  Parsed := ParseBytes(Source, tikFragmentInput);
  try
    CheckCore(Parsed.Outcome = tpoAccepted,
      'DevTikZ writer fixture should be syntactically accepted');
    CheckCore(Parsed.Region.Found and not Parsed.Region.IsFragment,
      'writer wrapper should select its enclosed picture environment');
    CheckCore((Parsed.Region.Span.StartByte > 0) and
      (Parsed.Region.Span.EndByte < Length(Source)),
      'writer defaults and footer must remain outside the selected picture span');
    Replay := Parsed.CopySource;
    CheckCore(SameBytes(Source, Replay),
      'accepted writer source must replay byte for byte');
    Semicolons := 0;
    GroupsWithParents := 0;
    for I := 0 to Parsed.TokenCount - 1 do
    begin
      Token := Parsed.TokenAt(I);
      if Token.Kind = ttkSemicolon then Inc(Semicolons);
      if Token.ParentGroup >= 0 then Inc(GroupsWithParents);
    end;
    CheckCore(Semicolons = 5, 'writer path and node statements were not retained');
    CheckCore(GroupsWithParents > 20,
      'nested writer options and text groups were not linked');
  finally
    Parsed.Free;
  end;
end;

function NextRandom(var State: LongWord): LongWord;
begin
  State := State xor (State shl 13);
  State := State xor (State shr 17);
  State := State xor (State shl 5);
  Result := State;
end;

function ShrinkFailure(const Source: TBytes): TBytes; forward;

function FuzzFailureDescription(Seed, CaseNumber: LongWord;
  const Source: TBytes): string;
var
  Minimal: TBytes;
begin
  Minimal := ShrinkFailure(Source);
  Result := 'TikZ grammar fuzz mismatch; seed=' + IntToHex(Seed, 8) +
    ' case=' + IntToStr(CaseNumber) + ' source=' + BytesText(Source) +
    ' minimized=' + BytesText(Minimal);
end;

procedure TestEveryDocumentTokenBoundaryAndSeededGrammar;
const
  Seed: LongWord = $68C0FFEE;
  Coordinates: array[0..5] of string =
    ('0', '1', '-2', '.5', '1e-3', '2.25');
var
  Source, Prefix: TBytes;
  Parsed, Truncated: TTikZSyntaxResult;
  I, CutAt, DocumentEnd: SizeInt;
  State, CaseNumber, R1, R2, R3, R4: LongWord;
  TextSource: RawByteString;
begin
  Source := LoadFixture('handwritten.tex');
  Parsed := ParseBytes(Source, tikTexInput);
  try
    CheckCore(Parsed.Outcome = tpoAccepted,
      'truncation oracle must start from an accepted document');
    DocumentEnd := Pos('\end{document}', BytesText(Source));
    if DocumentEnd > 0 then Inc(DocumentEnd, Length('\end{document}') - 1);
    CheckCore(DocumentEnd > 0, 'document closing boundary is missing');
    for I := 0 to Parsed.TokenCount - 1 do
    begin
      CutAt := Parsed.TokenAt(I).Span.EndByte;
      if (CutAt = 0) or (CutAt >= DocumentEnd) then Break;
      Prefix := Copy(Source, 0, CutAt);
      Truncated := ParseBytes(Prefix, tikTexInput);
      try
        CheckCore(Truncated.Outcome <> tpoAccepted,
          'truncation at token boundary ' + IntToStr(CutAt) +
          ' was incorrectly accepted');
      finally
        Truncated.Free;
      end;
    end;
  finally
    Parsed.Free;
  end;

  State := Seed;
  for CaseNumber := 0 to 63 do
  begin
    R1 := NextRandom(State) mod Length(Coordinates);
    R2 := NextRandom(State) mod Length(Coordinates);
    R3 := NextRandom(State) mod Length(Coordinates);
    R4 := NextRandom(State) mod Length(Coordinates);
    TextSource := '% seeded fake delimiter: \begin{tikzpicture}; }' + #10 +
      '\begin{tikzpicture}[x=1mm,y=1mm]' + #10 +
      '\draw (' + Coordinates[R1] + ',' + Coordinates[R2] + ') -- (' +
      Coordinates[R3] + ',' + Coordinates[R4] + ');' + #10 +
      '\node at (0,0) {fraction \frac{1}{2}; café \% \{x\}};' + #10 +
      '\end{tikzpicture}';
    Source := BytesOf(TextSource);
    Parsed := ParseBytes(Source, tikFragmentInput);
    try
      if Parsed.Outcome <> tpoAccepted then
        CheckCore(False, FuzzFailureDescription(Seed, CaseNumber, Source));
      CheckCore(SameBytes(Source, Parsed.CopySource),
        'seeded grammar source was not retained unchanged');
    finally
      Parsed.Free;
    end;
  end;
end;

procedure TestDangerousCommandsRemainUnsupported;
const
  Cases: array[0..3] of string = (
    '\begin{tikzpicture}\path (0,0) -- (1,1);\write18{bad};\end{tikzpicture}',
    '\begin{tikzpicture}\path (0,0)--(1,1);\input{secret.tex};\end{tikzpicture}',
    '\begin{tikzpicture}\path (0,0)--(1,1);\catcode`\%=12;\end{tikzpicture}',
    '\begin{tikzpicture}\newcommand{\evil}{x}\path (0,0)--(1,1);\end{tikzpicture}');
var
  I: SizeInt;
  Source: TBytes;
  Parsed: TTikZSyntaxResult;
begin
  for I := Low(Cases) to High(Cases) do
  begin
    Source := BytesOf(Cases[I]);
    Parsed := ParseBytes(Source, tikFragmentInput);
    try
      CheckCore((Parsed.Outcome = tpoUnsupported) and
        HasDiagnostic(Parsed, tdcUnsupportedCommand),
        'dynamic TeX must be diagnosed without execution: ' + Cases[I]);
      CheckCore(SameBytes(Source, Parsed.CopySource),
        'unsupported input bytes must remain available for diagnostics');
    finally
      Parsed.Free;
    end;
  end;

  Source := BytesOf('\begin{tikzpicture}' +
    '\path[draw={\unresolvedStyle}] (0,0)--(1,1);' +
    '\end{tikzpicture}');
  Parsed := ParseBytes(Source, tikFragmentInput);
  try
    CheckCore((Parsed.Outcome = tpoUnsupported) and
      HasDiagnostic(Parsed, tdcUnsupportedCommand),
      'unresolved commands nested in TikZ option values must be diagnosed');
  finally
    Parsed.Free;
  end;
end;

function RemoveBytes(const Source: TBytes; StartAt, Count: SizeInt): TBytes;
var
  I, OutAt: SizeInt;
begin
  SetLength(Result, Length(Source) - Count);
  OutAt := 0;
  for I := 0 to Length(Source) - 1 do
    if (I < StartAt) or (I >= StartAt + Count) then
    begin
      Result[OutAt] := Source[I];
      Inc(OutAt);
    end;
end;

function IsAccepted(const Source: TBytes): Boolean;
var
  Parsed: TTikZSyntaxResult;
begin
  Parsed := ParseBytes(Source, tikFragmentInput);
  try
    Result := Parsed.Outcome = tpoAccepted;
  finally
    Parsed.Free;
  end;
end;

function ShrinkFailure(const Source: TBytes): TBytes;
var
  Candidate: TBytes;
  Chunk, StartAt, Count: SizeInt;
  Changed: Boolean;
begin
  Result := Copy(Source);
  Chunk := Length(Result) div 2;
  while Chunk > 0 do
  begin
    Changed := False;
    StartAt := 0;
    while StartAt < Length(Result) do
    begin
      Count := Chunk;
      if Count > Length(Result) - StartAt then Count := Length(Result) - StartAt;
      Candidate := RemoveBytes(Result, StartAt, Count);
      if not IsAccepted(Candidate) then
      begin
        Result := Candidate;
        Changed := True;
        Break;
      end;
      Inc(StartAt, Chunk);
    end;
    if not Changed then Chunk := Chunk div 2;
  end;
end;

procedure TestHandwrittenDocumentAndSpans;
var
  Source, Replay, Prefix, Suffix: TBytes;
  Parsed: TTikZSyntaxResult;
  I: SizeInt;
  Token: TTikZToken;
  NumberCount, DimensionCount, FractionCount: SizeInt;
begin
  Source := LoadFixture('handwritten.tex');
  Parsed := ParseBytes(Source, tikTexInput);
  try
    CheckCore(Parsed.Outcome = tpoAccepted,
      'hand-written declarative document should be accepted');
    CheckCore(Parsed.Region.Found and not Parsed.Region.IsFragment,
      '.tex input should select one picture environment');
    Replay := Parsed.CopySource;
    CheckCore(SameBytes(Source, Replay),
      'accepted document must retain all bytes outside and inside the picture');
    Prefix := Parsed.SourceSlice(EmptySpan(0, Parsed.Region.Span.StartByte));
    Suffix := Parsed.SourceSlice(EmptySpan(Parsed.Region.Span.EndByte,
      Length(Source)));
    CheckCore(Pos('fake environment in a comment', BytesText(Prefix)) > 0,
      'pre-picture comments must remain outside the selected region');
    CheckCore(Pos('After picture', BytesText(Suffix)) > 0,
      'post-picture comments must remain outside the selected region');
    NumberCount := 0;
    DimensionCount := 0;
    FractionCount := 0;
    for I := 0 to Parsed.TokenCount - 1 do
    begin
      Token := Parsed.TokenAt(I);
      if Token.Kind = ttkNumber then Inc(NumberCount);
      if Token.Kind = ttkDimension then Inc(DimensionCount);
      if Token.Kind = ttkControlWord then
      begin
        Replay := Parsed.SourceSlice(Token.Span);
        if BytesText(Replay) = '\frac' then Inc(FractionCount);
      end;
    end;
    CheckCore(NumberCount >= 4,
      'decimal and scientific coordinate numbers must be tokenized');
    CheckCore(DimensionCount >= 2, 'millimetre dimensions must be tokenized');
    CheckCore(FractionCount = 1, 'nested fraction text must remain a command token');
    CheckCore(Parsed.Region.ContentSpan.StartByte > Parsed.Region.Span.StartByte,
      'picture content span must exclude the environment header');
  finally
    Parsed.Free;
  end;

  Source := LoadFixture('independent-scene.tex');
  Parsed := ParseBytes(Source, tikTexInput);
  try
    CheckCore(Parsed.Outcome = tpoAccepted,
      'the metadata-free preview wrapper and literal color preamble should parse');
    CheckCore(Parsed.Region.Found and not Parsed.Region.IsFragment,
      'the preview wrapper must select its unique picture');
    Replay := Parsed.CopySource;
    CheckCore(SameBytes(Source, Replay),
      'preview wrappers and all outside-picture bytes must be retained');
  finally
    Parsed.Free;
  end;
end;

function EmptySpan(StartByte, EndByte: SizeInt): TTikZSourceSpan;
begin
  Result.StartByte := StartByte;
  Result.EndByte := EndByte;
  Result.StartLine := 1;
  Result.StartColumn := 1;
  Result.EndLine := 1;
  Result.EndColumn := 1;
end;

function CancelNow(Context: Pointer): Boolean;
begin
  Result := True;
end;

procedure TestAmbiguityEmptyPictureAndLimits;
var
  Source: TBytes;
  Parsed: TTikZSyntaxResult;
  Options: TTikZParseOptions;
  I: SizeInt;
begin
  Source := LoadFixture('empty-picture.tikz');
  Parsed := ParseBytes(Source, tikFragmentInput);
  try
    CheckCore(Parsed.Outcome = tpoAccepted,
      'an explicitly empty tikzpicture is a valid empty scene');
  finally
    Parsed.Free;
  end;

  Source := LoadFixture('multiple-pictures.tex');
  Parsed := ParseBytes(Source, tikTexInput);
  try
    CheckCore((Parsed.Outcome = tpoUnsupported) and
      HasDiagnostic(Parsed, tdcMultiplePictures) and not Parsed.Region.Found,
      'ambiguous .tex input must not select its first picture');
  finally
    Parsed.Free;
  end;

  Source := BytesOf('\begin{tikzpicture}\draw (0,0)--(1,1);' +
    '\end{tikzpicture} stray text');
  Parsed := ParseBytes(Source, tikFragmentInput);
  try
    CheckCore((Parsed.Outcome = tpoUnsupported) and
      HasDiagnostic(Parsed, tdcUnsupportedContext),
      'text outside a wrapped picture must not be silently ignored');
  finally
    Parsed.Free;
  end;

  Source := BytesOf('\begin{tikzpicture}{');
  Parsed := ParseBytes(Source, tikFragmentInput);
  try
    CheckCore((Parsed.Outcome = tpoInvalid) and
      HasDiagnostic(Parsed, tdcUnterminatedGroup),
      'an unterminated group must be invalid and source-located');
    CheckCore(SameBytes(Source, Parsed.CopySource),
      'invalid in-limit source must remain available for diagnosis');
  finally
    Parsed.Free;
  end;

  Source := BytesOf('[[[[[x]]]]]');
  Options := DefaultTikZParseOptions(tikFragmentInput);
  Options.LexerOptions.MaxNestingDepth := 4;
  Parsed := ParseTikZ(Source, Options);
  try
    CheckCore((Parsed.Outcome = tpoInvalid) and
      HasDiagnostic(Parsed, tdcNestingTooDeep),
      'nesting limit must fail before unbounded group allocation');
  finally
    Parsed.Free;
  end;

  Source := LoadFixture('devtikz-emitted.tikz');
  Options := DefaultTikZParseOptions(tikFragmentInput);
  Options.LexerOptions.MaxTokens := 8;
  Parsed := ParseTikZ(Source, Options);
  try
    CheckCore((Parsed.Outcome = tpoInvalid) and
      HasDiagnostic(Parsed, tdcTooManyTokens), 'token limit must be enforced');
  finally
    Parsed.Free;
  end;

  Options := DefaultTikZParseOptions(tikFragmentInput);
  Options.LexerOptions.MaxSourceBytes := Length(Source) - 1;
  Parsed := ParseTikZ(Source, Options);
  try
    CheckCore((Parsed.Outcome = tpoInvalid) and
      HasDiagnostic(Parsed, tdcSourceTooLarge), 'source-byte limit must be enforced');
    CheckCore(Length(Parsed.CopySource) = 0,
      'oversized data must not be copied into the parse result');
  finally
    Parsed.Free;
  end;

  Options := DefaultTikZParseOptions(tikFragmentInput);
  Options.LexerOptions.CancelCheck := @CancelNow;
  Parsed := ParseTikZ(BytesOf('\begin{tikzpicture}\end{tikzpicture}'), Options);
  try
    CheckCore((Parsed.Outcome = tpoCancelled) and
      HasDiagnostic(Parsed, tdcCancelled), 'caller cancellation must terminate parsing');
  finally
    Parsed.Free;
  end;

  Source := BytesOf(#$EF#$BB#$BF + '\begin{tikzpicture}\draw (0,0)--(1,1);' +
    '\end{tikzpicture}');
  Parsed := ParseBytes(Source, tikFragmentInput);
  try
    CheckCore(Parsed.Outcome = tpoAccepted,
      'a leading UTF-8 BOM must be accepted as retained trivia');
    CheckCore(SameBytes(Source, Parsed.CopySource),
      'UTF-8 BOM bytes must not be normalized away');
  finally
    Parsed.Free;
  end;

  Source := BytesOf('% CRLF spans' + #13#10 +
    '\begin{tikzpicture}' + #13#10 +
    '\draw (0,0)--(1e-3,2);' + #13#10 +
    '\end{tikzpicture}' + #13#10);
  Parsed := ParseBytes(Source, tikFragmentInput);
  try
    CheckCore(Parsed.Outcome = tpoAccepted, 'CRLF source should parse');
    I := 0;
    while (I < Parsed.TokenCount) and
      (BytesText(Parsed.SourceSlice(Parsed.TokenAt(I).Span)) <> '\draw') do Inc(I);
    CheckCore((I < Parsed.TokenCount) and
      (Parsed.TokenAt(I).Span.StartLine = 3) and
      (Parsed.TokenAt(I).Span.StartColumn = 1),
      'CRLF source spans must use one-based byte line/column positions');
  finally
    Parsed.Free;
  end;
end;


initialization
  RegisterCoreTest('tikz-writer-fixture-and-source-retention',
    TestWriterFixtureAndByteRetention);
  RegisterCoreTest('tikz-handwritten-document-and-spans',
    TestHandwrittenDocumentAndSpans);
  RegisterCoreTest('tikz-ambiguity-empty-and-bounded-input',
    TestAmbiguityEmptyPictureAndLimits);
  RegisterCoreTest('tikz-dangerous-commands-are-diagnosed',
    TestDangerousCommandsRemainUnsupported);
  RegisterCoreTest('tikz-truncation-and-seeded-grammar',
    TestEveryDocumentTokenBoundaryAndSeededGrammar);

end.
