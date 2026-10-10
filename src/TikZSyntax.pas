unit TikZSyntax;

{$mode objfpc}{$H+}

interface

uses SysUtils, TikZLexer;

type
  TTikZInputKind = (tikFragmentInput, tikTexInput);
  TTikZParseOutcome = (tpoAccepted, tpoUnsupported, tpoInvalid, tpoCancelled);

  TTikZPictureRegion = record
    Found: Boolean;
    IsFragment: Boolean;
    Span: TTikZSourceSpan;
    HeaderSpan: TTikZSourceSpan;
    ContentSpan: TTikZSourceSpan;
    BeginToken, EndToken: SizeInt;
    FirstContentToken, PastLastContentToken: SizeInt;
  end;

  TTikZParseOptions = record
    InputKind: TTikZInputKind;
    LexerOptions: TTikZLexerOptions;
  end;

  TTikZSyntaxResult = class
  private
    FSource: TBytes;
    FTokens: TTikZTokens;
    FGroups: TTikZGroups;
    FDiagnostics: TTikZDiagnostics;
    FDiagnosticCount, FDiagnosticCapacity: SizeInt;
    FOutcome: TTikZParseOutcome;
    FRegion: TTikZPictureRegion;
  public
    property Outcome: TTikZParseOutcome read FOutcome;
    property Region: TTikZPictureRegion read FRegion;
    function TokenCount: SizeInt;
    function TokenAt(Index: SizeInt): TTikZToken;
    function GroupCount: SizeInt;
    function GroupAt(Index: SizeInt): TTikZGroup;
    function DiagnosticCount: SizeInt;
    function DiagnosticAt(Index: SizeInt): TTikZDiagnostic;
    function CopySource: TBytes;
    function SourceSlice(const Span: TTikZSourceSpan): TBytes;
  end;

function DefaultTikZParseOptions(InputKind: TTikZInputKind): TTikZParseOptions;
function ParseTikZ(const Source: TBytes;
  const Options: TTikZParseOptions): TTikZSyntaxResult;

implementation

type
  TEnvironmentEntry = record
    Name: string;
    BeginToken: SizeInt;
  end;
  TEnvironmentStack = array of TEnvironmentEntry;
  TStringStack = array of string;

function EmptySpanAt(Offset, LineNo, ColumnNo: SizeInt): TTikZSourceSpan;
begin
  Result.StartByte := Offset;
  Result.EndByte := Offset;
  Result.StartLine := LineNo;
  Result.EndLine := LineNo;
  Result.StartColumn := ColumnNo;
  Result.EndColumn := ColumnNo;
end;

function SpanForBytes(const Source: TBytes; StartAt, EndAt: SizeInt): TTikZSourceSpan;
var
  I, LineNo, ColNo: SizeInt;
  PreviousCR: Boolean;
begin
  LineNo := 1;
  ColNo := 1;
  PreviousCR := False;
  for I := 0 to StartAt - 1 do
  begin
    if Source[I] = 13 then
    begin
      Inc(LineNo);
      ColNo := 1;
      PreviousCR := True;
    end
    else if Source[I] = 10 then
    begin
      if not PreviousCR then Inc(LineNo);
      ColNo := 1;
      PreviousCR := False;
    end
    else
    begin
      Inc(ColNo);
      PreviousCR := False;
    end;
  end;
  Result.StartByte := StartAt;
  Result.EndByte := EndAt;
  Result.StartLine := LineNo;
  Result.StartColumn := ColNo;
  for I := StartAt to EndAt - 1 do
  begin
    if Source[I] = 13 then
    begin
      Inc(LineNo);
      ColNo := 1;
      PreviousCR := True;
    end
    else if Source[I] = 10 then
    begin
      if not PreviousCR then Inc(LineNo);
      ColNo := 1;
      PreviousCR := False;
    end
    else
    begin
      Inc(ColNo);
      PreviousCR := False;
    end;
  end;
  Result.EndLine := LineNo;
  Result.EndColumn := ColNo;
end;

function BytesAsString(const Source: TBytes; StartAt, EndAt: SizeInt): string;
var
  I, Count: SizeInt;
begin
  if (StartAt < 0) or (EndAt < StartAt) or (EndAt > Length(Source)) then
    Exit('');
  Count := EndAt - StartAt;
  SetLength(Result, Count);
  for I := 1 to Count do Result[I] := Chr(Source[StartAt + I - 1]);
end;

function LowerAscii(const Value: string): string;
var
  I: SizeInt;
begin
  Result := Value;
  for I := 1 to Length(Result) do
    if (Result[I] >= 'A') and (Result[I] <= 'Z') then
      Result[I] := Chr(Ord(Result[I]) + Ord('a') - Ord('A'));
end;

function TrimAscii(const Value: string): string;
var
  FirstAt, LastAt: SizeInt;
begin
  FirstAt := 1;
  LastAt := Length(Value);
  while (FirstAt <= LastAt) and (Ord(Value[FirstAt]) <= 32) do Inc(FirstAt);
  while (LastAt >= FirstAt) and (Ord(Value[LastAt]) <= 32) do Dec(LastAt);
  Result := Copy(Value, FirstAt, LastAt - FirstAt + 1);
end;

function TokenText(const Source: TBytes; const Token: TTikZToken): string;
begin
  Result := BytesAsString(Source, Token.Span.StartByte, Token.Span.EndByte);
end;

function ControlName(const Source: TBytes; const Token: TTikZToken): string;
var
  Value: string;
begin
  Value := TokenText(Source, Token);
  if (Length(Value) > 0) and (Value[1] = '\') then Delete(Value, 1, 1);
  Result := Value;
end;

function IsTrivia(const Token: TTikZToken): Boolean; inline;
begin
  Result := (Token.Kind = ttkWhitespace) or (Token.Kind = ttkComment);
end;

function NextSignificant(ResultDoc: TTikZSyntaxResult; Index: SizeInt): SizeInt;
begin
  while (Index < ResultDoc.TokenCount) and
    IsTrivia(ResultDoc.TokenAt(Index)) do Inc(Index);
  Result := Index;
end;

procedure AddSyntaxDiagnostic(ResultDoc: TTikZSyntaxResult;
  Code: TTikZDiagnosticCode; const Span: TTikZSourceSpan;
  const Message: string; Severity: TTikZSeverity);
var
  N, NewCapacity: SizeInt;
begin
  N := ResultDoc.FDiagnosticCount;
  if N = ResultDoc.FDiagnosticCapacity then
  begin
    NewCapacity := ResultDoc.FDiagnosticCapacity;
    if NewCapacity = 0 then NewCapacity := 16
    else if NewCapacity > High(SizeInt) div 2 then
      NewCapacity := High(SizeInt)
    else NewCapacity := NewCapacity * 2;
    SetLength(ResultDoc.FDiagnostics, NewCapacity);
    ResultDoc.FDiagnosticCapacity := NewCapacity;
  end;
  Inc(ResultDoc.FDiagnosticCount);
  ResultDoc.FDiagnostics[N].Code := Code;
  ResultDoc.FDiagnostics[N].Severity := Severity;
  ResultDoc.FDiagnostics[N].Span := Span;
  ResultDoc.FDiagnostics[N].Message := Message;
  if Severity = tsError then ResultDoc.FOutcome := tpoInvalid
  else if ResultDoc.FOutcome = tpoAccepted then
    ResultDoc.FOutcome := tpoUnsupported;
end;

function GroupClose(ResultDoc: TTikZSyntaxResult; OpenToken: SizeInt): SizeInt;
var
  Token: TTikZToken;
begin
  if (OpenToken < 0) or (OpenToken >= ResultDoc.TokenCount) then Exit(-1);
  Token := ResultDoc.TokenAt(OpenToken);
  if (Token.Kind <> ttkOpenBrace) and (Token.Kind <> ttkOpenBracket) and
    (Token.Kind <> ttkOpenParen) then Exit(-1);
  Result := Token.MatchingToken;
end;

function ConsumeLiteralScalar(ResultDoc: TTikZSyntaxResult;
  var Cursor: SizeInt; AllowDimension: Boolean): Boolean;
var
  Token: TTikZToken;
  Text: string;
begin
  Result := False;
  Cursor := NextSignificant(ResultDoc, Cursor);
  if Cursor >= ResultDoc.TokenCount then Exit;
  Token := ResultDoc.TokenAt(Cursor);
  if Token.Kind = ttkOperator then
  begin
    Text := TokenText(ResultDoc.FSource, Token);
    if (Text <> '+') and (Text <> '-') then Exit;
    Cursor := NextSignificant(ResultDoc, Cursor + 1);
    if Cursor >= ResultDoc.TokenCount then Exit;
    Token := ResultDoc.TokenAt(Cursor);
  end;
  if (Token.Kind <> ttkNumber) and
    not (AllowDimension and (Token.Kind = ttkDimension)) then Exit;
  Inc(Cursor);
  Result := True;
end;

function ScopeShiftValueValid(ResultDoc: TTikZSyntaxResult;
  OpenBrace: SizeInt): Boolean;
var
  CloseBrace, OpenParen, CloseParen, Cursor: SizeInt;
begin
  Result := False;
  CloseBrace := GroupClose(ResultDoc, OpenBrace);
  if CloseBrace <= OpenBrace then Exit;
  OpenParen := NextSignificant(ResultDoc, OpenBrace + 1);
  if (OpenParen >= CloseBrace) or
    (ResultDoc.TokenAt(OpenParen).Kind <> ttkOpenParen) then Exit;
  CloseParen := GroupClose(ResultDoc, OpenParen);
  if (CloseParen <= OpenParen) or (CloseParen >= CloseBrace) then Exit;
  Cursor := NextSignificant(ResultDoc, OpenParen + 1);
  if (Cursor >= CloseParen) or
    not ConsumeLiteralScalar(ResultDoc, Cursor, True) then Exit;
  Cursor := NextSignificant(ResultDoc, Cursor);
  if (Cursor >= CloseParen) or
    (ResultDoc.TokenAt(Cursor).Kind <> ttkComma) then Exit;
  Cursor := NextSignificant(ResultDoc, Cursor + 1);
  if (Cursor >= CloseParen) or
    not ConsumeLiteralScalar(ResultDoc, Cursor, True) then Exit;
  Cursor := NextSignificant(ResultDoc, Cursor);
  if Cursor <> CloseParen then Exit;
  Result := NextSignificant(ResultDoc, CloseParen + 1) = CloseBrace;
end;

function ScopeTransformOptionsValid(ResultDoc: TTikZSyntaxResult;
  OpenBracket: SizeInt; CancelCheck: TTikZCancelCheck;
  CancelContext: Pointer; out BadToken: SizeInt;
  out WasCancelled: Boolean): Boolean;
var
  CloseBracket, Cursor, KeyToken, EqualsToken, ValueToken, ValueClose: SizeInt;
  Steps: SizeInt;
  Key: string;
begin
  Result := False;
  WasCancelled := False;
  BadToken := OpenBracket;
  CloseBracket := GroupClose(ResultDoc, OpenBracket);
  if CloseBracket <= OpenBracket then Exit;
  Cursor := NextSignificant(ResultDoc, OpenBracket + 1);
  if Cursor = CloseBracket then Exit(True);
  Steps := 0;
  while Cursor < CloseBracket do
  begin
    Inc(Steps);
    if (Steps mod 1024 = 0) and Assigned(CancelCheck) and
      CancelCheck(CancelContext) then
    begin
      WasCancelled := True;
      Exit;
    end;
    KeyToken := Cursor;
    BadToken := KeyToken;
    if ResultDoc.TokenAt(KeyToken).Kind <> ttkWord then Exit;
    Key := TokenText(ResultDoc.FSource, ResultDoc.TokenAt(KeyToken));
    EqualsToken := NextSignificant(ResultDoc, KeyToken + 1);
    BadToken := EqualsToken;
    if (EqualsToken >= CloseBracket) or
      (ResultDoc.TokenAt(EqualsToken).Kind <> ttkEquals) then Exit;
    ValueToken := NextSignificant(ResultDoc, EqualsToken + 1);
    BadToken := ValueToken;
    if ValueToken >= CloseBracket then Exit;
    if Key = 'shift' then
    begin
      if ResultDoc.TokenAt(ValueToken).Kind <> ttkOpenBrace then Exit;
      if not ScopeShiftValueValid(ResultDoc, ValueToken) then Exit;
      ValueClose := GroupClose(ResultDoc, ValueToken);
      Cursor := ValueClose + 1;
    end
    else if (Key = 'rotate') or (Key = 'scale') or (Key = 'xscale') or
      (Key = 'yscale') then
    begin
      Cursor := ValueToken;
      if not ConsumeLiteralScalar(ResultDoc, Cursor, False) then Exit;
    end
    else Exit;
    Cursor := NextSignificant(ResultDoc, Cursor);
    if Cursor = CloseBracket then Exit(True);
    BadToken := Cursor;
    if ResultDoc.TokenAt(Cursor).Kind <> ttkComma then Exit;
    Cursor := NextSignificant(ResultDoc, Cursor + 1);
    if Cursor >= CloseBracket then
    begin
      BadToken := Cursor - 1;
      Exit;
    end;
  end;
end;

function PlainGroupValue(ResultDoc: TTikZSyntaxResult; OpenToken: SizeInt): string;
var
  I, CloseToken: SizeInt;
  Token: TTikZToken;
begin
  Result := '';
  if (OpenToken < 0) or (OpenToken >= ResultDoc.TokenCount) then Exit;
  if ResultDoc.TokenAt(OpenToken).Kind <> ttkOpenBrace then Exit;
  CloseToken := GroupClose(ResultDoc, OpenToken);
  if CloseToken < 0 then Exit;
  for I := OpenToken + 1 to CloseToken - 1 do
  begin
    Token := ResultDoc.TokenAt(I);
    if IsTrivia(Token) then Continue;
    if (Token.Kind <> ttkWord) and (Token.Kind <> ttkNumber) then Exit('');
    Result := Result + TokenText(ResultDoc.FSource, Token);
  end;
end;

function RawGroupValue(ResultDoc: TTikZSyntaxResult; OpenToken: SizeInt): string;
var
  CloseToken: SizeInt;
  OpenItem, CloseItem: TTikZToken;
begin
  Result := '';
  CloseToken := GroupClose(ResultDoc, OpenToken);
  if CloseToken < 0 then Exit;
  OpenItem := ResultDoc.TokenAt(OpenToken);
  CloseItem := ResultDoc.TokenAt(CloseToken);
  Result := TrimAscii(BytesAsString(ResultDoc.FSource,
    OpenItem.Span.EndByte, CloseItem.Span.StartByte));
end;

function GroupHasOnlyDimension(ResultDoc: TTikZSyntaxResult;
  OpenToken: SizeInt; const RequiredUnit: string): Boolean;
var
  I, CloseToken, Count: SizeInt;
  Token: TTikZToken;
  Value: string;
begin
  Result := False;
  CloseToken := GroupClose(ResultDoc, OpenToken);
  if CloseToken < 0 then Exit;
  Count := 0;
  for I := OpenToken + 1 to CloseToken - 1 do
  begin
    Token := ResultDoc.TokenAt(I);
    if IsTrivia(Token) then Continue;
    if Token.Kind <> ttkDimension then Exit;
    Value := LowerAscii(TokenText(ResultDoc.FSource, Token));
    if Copy(Value, Length(Value) - Length(RequiredUnit) + 1,
      Length(RequiredUnit)) <> RequiredUnit then Exit;
    Inc(Count);
  end;
  Result := Count = 1;
end;

function IsSimpleIdentifier(const Value: string): Boolean;
var
  I: SizeInt;
begin
  Result := False;
  if (Value = '') or not
    (((Value[1] >= 'A') and (Value[1] <= 'Z')) or
     ((Value[1] >= 'a') and (Value[1] <= 'z'))) then Exit;
  for I := 2 to Length(Value) do
    if not (((Value[I] >= 'A') and (Value[I] <= 'Z')) or
      ((Value[I] >= 'a') and (Value[I] <= 'z')) or
      ((Value[I] >= '0') and (Value[I] <= '9')) or
      (Value[I] = '-') or (Value[I] = '_')) then Exit;
  Result := True;
end;

function MacroDefinitionValid(ResultDoc: TTikZSyntaxResult;
  CommandToken: SizeInt): Boolean;
var
  Arg, CloseToken, NameToken, ValueArg: SizeInt;
  MacroName, UnitName: string;
begin
  Result := False;
  Arg := NextSignificant(ResultDoc, CommandToken + 1);
  if (Arg >= ResultDoc.TokenCount) or
    (ResultDoc.TokenAt(Arg).Kind <> ttkOpenBrace) then Exit;
  CloseToken := GroupClose(ResultDoc, Arg);
  if CloseToken < 0 then Exit;
  NameToken := NextSignificant(ResultDoc, Arg + 1);
  if (NameToken >= CloseToken) or
    (ResultDoc.TokenAt(NameToken).Kind <> ttkControlWord) then Exit;
  if NextSignificant(ResultDoc, NameToken + 1) <> CloseToken then Exit;
  MacroName := ControlName(ResultDoc.FSource, ResultDoc.TokenAt(NameToken));
  if (MacroName = 'tpxLineWidth') or (MacroName = 'tpxDashSize') or
    (MacroName = 'tpxDotSize') then UnitName := 'mm'
  else if MacroName = 'tpxTextSize' then UnitName := 'pt'
  else Exit;
  ValueArg := NextSignificant(ResultDoc, CloseToken + 1);
  if (ValueArg >= ResultDoc.TokenCount) or
    (ResultDoc.TokenAt(ValueArg).Kind <> ttkOpenBrace) then Exit;
  Result := GroupHasOnlyDimension(ResultDoc, ValueArg, UnitName);
end;

function ColorDefinitionValid(ResultDoc: TTikZSyntaxResult;
  CommandToken: SizeInt): Boolean;
var
  I, Arg, CloseToken, ValueArg, NumberCount, CommaCount: SizeInt;
  NameValue, ModelValue: string;
  Token: TTikZToken;
  NumericValue: Extended;
  ParseCode: Integer;
begin
  Result := False;
  Arg := NextSignificant(ResultDoc, CommandToken + 1);
  if (Arg >= ResultDoc.TokenCount) or
    (ResultDoc.TokenAt(Arg).Kind <> ttkOpenBrace) then Exit;
  NameValue := RawGroupValue(ResultDoc, Arg);
  if (NameValue = '') or not IsSimpleIdentifier(NameValue) then Exit;
  CloseToken := GroupClose(ResultDoc, Arg);
  Arg := NextSignificant(ResultDoc, CloseToken + 1);
  ModelValue := PlainGroupValue(ResultDoc, Arg);
  if ModelValue <> 'rgb' then Exit;
  CloseToken := GroupClose(ResultDoc, Arg);
  ValueArg := NextSignificant(ResultDoc, CloseToken + 1);
  CloseToken := GroupClose(ResultDoc, ValueArg);
  if CloseToken < 0 then Exit;
  NumberCount := 0;
  CommaCount := 0;
  for I := ValueArg + 1 to CloseToken - 1 do
  begin
    Token := ResultDoc.TokenAt(I);
    if IsTrivia(Token) then Continue;
    if Token.Kind = ttkNumber then
    begin
      Val(TokenText(ResultDoc.FSource, Token), NumericValue, ParseCode);
      if (ParseCode <> 0) or (NumericValue < 0) or (NumericValue > 1) then Exit;
      Inc(NumberCount);
    end
    else if Token.Kind = ttkComma then Inc(CommaCount)
    else Exit;
  end;
  Result := (NumberCount = 3) and (CommaCount = 2);
end;

function IsSupportedPackageList(const Value: string): Boolean;
var
  Remaining, Item: string;
  CutAt: SizeInt;
  FoundPackage: Boolean;
begin
  Remaining := Value;
  Result := False;
  FoundPackage := False;
  if Remaining = '' then Exit;
  while Remaining <> '' do
  begin
    CutAt := Pos(',', Remaining);
    if CutAt = 0 then
    begin
      Item := TrimAscii(Remaining);
      Remaining := '';
    end
    else
    begin
      Item := TrimAscii(Copy(Remaining, 1, CutAt - 1));
      Delete(Remaining, 1, CutAt);
    end;
    Item := LowerAscii(Item);
    if Item = '' then Exit;
    if (Item <> 'tikz') and (Item <> 'pgf') and
      (Item <> 'xcolor') and (Item <> 'graphicx') and (Item <> 'preview') then
      Exit(False);
    FoundPackage := True;
  end;
  Result := FoundPackage;
end;

function HasBraceAncestor(ResultDoc: TTikZSyntaxResult;
  TokenIndex: SizeInt): Boolean;
var
  Parent: SizeInt;
  Group: TTikZGroup;
begin
  Parent := ResultDoc.TokenAt(TokenIndex).ParentGroup;
  while Parent >= 0 do
  begin
    Group := ResultDoc.GroupAt(Parent);
    if Group.Kind = tgBrace then Exit(True);
    Parent := Group.ParentGroup;
  end;
  Result := False;
end;

function HasBracketAncestor(ResultDoc: TTikZSyntaxResult;
  TokenIndex: SizeInt): Boolean;
var
  Parent: SizeInt;
  Group: TTikZGroup;
begin
  Parent := ResultDoc.TokenAt(TokenIndex).ParentGroup;
  while Parent >= 0 do
  begin
    Group := ResultDoc.GroupAt(Parent);
    if Group.Kind = tgBracket then Exit(True);
    Parent := Group.ParentGroup;
  end;
  Result := False;
end;

function IsDangerousCommand(const Name: string): Boolean;
begin
  Result := (Name = 'input') or (Name = 'include') or
    (Name = 'includeonly') or (Name = 'write') or (Name = 'read') or
    (Name = 'readline') or (Name = 'openout') or (Name = 'openin') or
    (Name = 'immediate') or (Name = 'catcode') or (Name = 'csname') or
    (Name = 'endcsname') or (Name = 'def') or (Name = 'gdef') or
    (Name = 'edef') or (Name = 'xdef') or (Name = 'let') or
    (Name = 'futurelet') or (Name = 'newcommand') or
    (Name = 'renewcommand') or (Name = 'declarerobustcommand') or
    (Name = 'newwrite') or (Name = 'newread') or (Name = 'closeout') or
    (Name = 'closein') or (Name = 'afterassignment') or
    (Name = 'aftergroup') or (Name = 'directlua') or (Name = 'luaexec') or
    (Name = 'primitive') or (Name = 'pdfprimitive') or
    (Name = 'verb') or (Name = 'verbatim') or
    (Name = 'loop') or (Name = 'repeat') or (Name = 'foreach') or
    (Name = 'tikzset') or (Name = 'tikzstyle') or
    (Name = 'usetikzlibrary') or (Name = 'pgfkeys') or
    (Name = 'pgfkeysalso') or (Name = 'pgfplotsset') or
    (Name = 'pgfplots') or (Name = 'write18') or (Name = 'shipout') or
    (Name = 'special') or (Name = 'globaldefs');
end;

function TTikZSyntaxResult.TokenCount: SizeInt;
begin Result := Length(FTokens); end;

function TTikZSyntaxResult.TokenAt(Index: SizeInt): TTikZToken;
begin Result := FTokens[Index]; end;

function TTikZSyntaxResult.GroupCount: SizeInt;
begin Result := Length(FGroups); end;

function TTikZSyntaxResult.GroupAt(Index: SizeInt): TTikZGroup;
begin Result := FGroups[Index]; end;

function TTikZSyntaxResult.DiagnosticCount: SizeInt;
begin Result := FDiagnosticCount; end;

function TTikZSyntaxResult.DiagnosticAt(Index: SizeInt): TTikZDiagnostic;
begin Result := FDiagnostics[Index]; end;

function TTikZSyntaxResult.CopySource: TBytes;
var
  I: SizeInt;
begin
  Result := nil;
  SetLength(Result, Length(FSource));
  for I := 0 to High(FSource) do Result[I] := FSource[I];
end;

function TTikZSyntaxResult.SourceSlice(
  const Span: TTikZSourceSpan): TBytes;
var
  I, Count: SizeInt;
begin
  Result := nil;
  Count := Span.EndByte - Span.StartByte;
  if (Span.StartByte < 0) or (Count < 0) or
    (Span.EndByte > Length(FSource)) then Count := 0;
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := FSource[Span.StartByte + I];
end;

function DefaultTikZParseOptions(InputKind: TTikZInputKind): TTikZParseOptions;
begin
  Result.InputKind := InputKind;
  Result.LexerOptions := DefaultTikZLexerOptions;
end;

function ParseTikZ(const Source: TBytes;
  const Options: TTikZParseOptions): TTikZSyntaxResult;
var
  Doc: TTikZSyntaxResult;
  LexData: TTikZLexResult;
  Environments: TEnvironmentStack;
  MacroGroups: TStringStack;
  Candidate: TTikZPictureRegion;
  CandidateCount, I, J, ArgToken, ArgClose, HeaderClose: SizeInt;
  ScopeOptionToken, ScopeBadToken: SizeInt;
  ScopeCancelled: Boolean;
  EnvironmentUsed, EnvironmentCapacity, MacroGroupUsed,
    MacroGroupCapacity: SizeInt;
  LastParserCheck: SizeInt;
  Token: TTikZToken;
  Name, EnvName: string;
  Span: TTikZSourceSpan;

  function CheckCancellation(AtToken: SizeInt): Boolean;
  begin
    Result := False;
    if Options.LexerOptions.CancelCheck = nil then Exit;
    if (AtToken <> 0) and (AtToken - LastParserCheck < 1024) then Exit;
    LastParserCheck := AtToken;
    if Options.LexerOptions.CancelCheck(Options.LexerOptions.CancelContext) then
    begin
      Span := SpanForBytes(Source, Length(Source), Length(Source));
      AddSyntaxDiagnostic(Doc, tdcCancelled, Span,
        'TikZ parsing was cancelled', tsError);
      Doc.FOutcome := tpoCancelled;
      Result := True;
    end;
  end;

  function InsideTikZEnvironment: Boolean;
  var
    E: SizeInt;
  begin
    for E := 0 to EnvironmentUsed - 1 do
      if Environments[E].Name = 'tikzpicture' then Exit(True);
    Result := False;
  end;

  function NextCapacity(Current, Limit: SizeInt): SizeInt;
  begin
    if Current = 0 then Result := 16
    else if Current > Limit div 2 then Result := Limit
    else Result := Current * 2;
    if Result > Limit then Result := Limit;
  end;

  procedure SetCandidate(BeginIndex, EndIndex, BeginArgClose,
    EndArgClose: SizeInt);
  var
    HeaderEndByte, ContentStartToken: SizeInt;
    BeginSpan, EndSpan: TTikZToken;
  begin
    BeginSpan := Doc.TokenAt(BeginIndex);
    EndSpan := Doc.TokenAt(EndIndex);
    HeaderClose := BeginArgClose;
    J := NextSignificant(Doc, BeginArgClose + 1);
    if (J < Doc.TokenCount) and
      (Doc.TokenAt(J).Kind = ttkOpenBracket) and
      (GroupClose(Doc, J) >= J) then
      HeaderClose := GroupClose(Doc, J);
    HeaderEndByte := Doc.TokenAt(HeaderClose).Span.EndByte;
    ContentStartToken := HeaderClose + 1;
    Candidate.Found := True;
    Candidate.IsFragment := False;
    Candidate.BeginToken := BeginIndex;
    Candidate.EndToken := EndIndex;
    Candidate.FirstContentToken := ContentStartToken;
    Candidate.PastLastContentToken := EndIndex;
    Candidate.Span := BeginSpan.Span;
    Candidate.Span.EndByte := Doc.TokenAt(EndArgClose).Span.EndByte;
    Candidate.Span.EndLine := Doc.TokenAt(EndArgClose).Span.EndLine;
    Candidate.Span.EndColumn := Doc.TokenAt(EndArgClose).Span.EndColumn;
    Candidate.HeaderSpan := BeginSpan.Span;
    Candidate.HeaderSpan.EndByte := HeaderEndByte;
    Candidate.HeaderSpan.EndLine := Doc.TokenAt(HeaderClose).Span.EndLine;
    Candidate.HeaderSpan.EndColumn := Doc.TokenAt(HeaderClose).Span.EndColumn;
    Candidate.ContentSpan := Doc.TokenAt(HeaderClose).Span;
    Candidate.ContentSpan.StartByte := HeaderEndByte;
    Candidate.ContentSpan.StartLine := Doc.TokenAt(HeaderClose).Span.EndLine;
    Candidate.ContentSpan.StartColumn := Doc.TokenAt(HeaderClose).Span.EndColumn;
    Candidate.ContentSpan.EndByte := EndSpan.Span.StartByte;
    Candidate.ContentSpan.EndLine := EndSpan.Span.StartLine;
    Candidate.ContentSpan.EndColumn := EndSpan.Span.StartColumn;
  end;

  function InSelectedRegion(TokenIndex: SizeInt): Boolean;
  var
    Item: TTikZToken;
  begin
    if not Doc.FRegion.Found then Exit(False);
    Item := Doc.TokenAt(TokenIndex);
    Result := (Item.Span.StartByte >= Doc.FRegion.ContentSpan.StartByte) and
      (Item.Span.StartByte < Doc.FRegion.ContentSpan.EndByte);
  end;

  procedure MarkUnsupported(TokenIndex: SizeInt; Code: TTikZDiagnosticCode;
    const Message: string);
  begin
    AddSyntaxDiagnostic(Doc, Code, Doc.TokenAt(TokenIndex).Span,
      Message, tsWarning);
  end;

  function DocumentClassValid(CommandToken: SizeInt): Boolean;
  var
    Arg: SizeInt;
    ClassName: string;
  begin
    Result := False;
    if Options.InputKind <> tikTexInput then Exit;
    Arg := NextSignificant(Doc, CommandToken + 1);
    if (Arg < Doc.TokenCount) and
      (Doc.TokenAt(Arg).Kind = ttkOpenBracket) then Exit;
    if (Arg >= Doc.TokenCount) or
      (Doc.TokenAt(Arg).Kind <> ttkOpenBrace) then Exit;
    ClassName := LowerAscii(PlainGroupValue(Doc, Arg));
    Result := (ClassName = 'article') or (ClassName = 'report') or
      (ClassName = 'book') or (ClassName = 'standalone') or
      (ClassName = 'minimal');
  end;

  function UsePackageValid(CommandToken: SizeInt): Boolean;
  var
    Arg, CloseToken: SizeInt;
    PackageName, PackageOptions: string;
    HasOptions: Boolean;
    I: SizeInt;
  begin
    Result := False;
    if Options.InputKind <> tikTexInput then Exit;
    Arg := NextSignificant(Doc, CommandToken + 1);
    if (Arg < Doc.TokenCount) and
      (Doc.TokenAt(Arg).Kind = ttkOpenBracket) then
    begin
      HasOptions := True;
      CloseToken := GroupClose(Doc, Arg);
      if CloseToken < Arg then Exit;
      PackageOptions := LowerAscii(RawGroupValue(Doc, Arg));
      for I := Length(PackageOptions) downto 1 do
        if Ord(PackageOptions[I]) <= 32 then Delete(PackageOptions, I, 1);
      Arg := NextSignificant(Doc, CloseToken + 1);
    end
    else
    begin
      HasOptions := False;
      PackageOptions := '';
    end;
    if (Arg >= Doc.TokenCount) or
      (Doc.TokenAt(Arg).Kind <> ttkOpenBrace) then Exit;
    PackageName := LowerAscii(RawGroupValue(Doc, Arg));
    if not IsSupportedPackageList(PackageName) then Exit;
    if HasOptions then
      Result := (PackageName = 'preview') and
        ((PackageOptions = 'active,tightpage') or
         (PackageOptions = 'tightpage,active'))
    else Result := True;
  end;

  function PreviewEnvironmentValid(CommandToken: SizeInt): Boolean;
  var
    Arg: SizeInt;
  begin
    Arg := NextSignificant(Doc, CommandToken + 1);
    Result := (Options.InputKind = tikTexInput) and
      (Arg < Doc.TokenCount) and
      (Doc.TokenAt(Arg).Kind = ttkOpenBrace) and
      (PlainGroupValue(Doc, Arg) = 'tikzpicture');
  end;

  function PreviewBorderValid(CommandToken: SizeInt): Boolean;
  var
    Target, ValueArg: SizeInt;
  begin
    Result := False;
    if Options.InputKind <> tikTexInput then Exit;
    Target := NextSignificant(Doc, CommandToken + 1);
    if (Target >= Doc.TokenCount) or
      (Doc.TokenAt(Target).Kind <> ttkControlWord) or
      (ControlName(Doc.FSource, Doc.TokenAt(Target)) <> 'PreviewBorder') then Exit;
    ValueArg := NextSignificant(Doc, Target + 1);
    Result := GroupHasOnlyDimension(Doc, ValueArg, 'pt');
  end;

  function UnicodeDeclarationValid(CommandToken: SizeInt): Boolean;
  var
    CodeArg, ReplacementArg: SizeInt;
    CodeValue, ReplacementValue: string;
    I: SizeInt;
  begin
    Result := False;
    if Options.InputKind <> tikTexInput then Exit;
    CodeArg := NextSignificant(Doc, CommandToken + 1);
    if (CodeArg >= Doc.TokenCount) or
      (Doc.TokenAt(CodeArg).Kind <> ttkOpenBrace) then Exit;
    CodeValue := PlainGroupValue(Doc, CodeArg);
    if (Length(CodeValue) <> 4) and (Length(CodeValue) <> 5) and
      (Length(CodeValue) <> 6) then Exit;
    for I := 1 to Length(CodeValue) do
      if not (((CodeValue[I] >= '0') and (CodeValue[I] <= '9')) or
        ((LowerAscii(CodeValue[I]) >= 'a') and
         (LowerAscii(CodeValue[I]) <= 'f'))) then Exit;
    CodeArg := GroupClose(Doc, CodeArg);
    ReplacementArg := NextSignificant(Doc, CodeArg + 1);
    if (ReplacementArg >= Doc.TokenCount) or
      (Doc.TokenAt(ReplacementArg).Kind <> ttkOpenBrace) then Exit;
    ReplacementValue := LowerAscii(RawGroupValue(Doc, ReplacementArg));
    Result := ReplacementValue = '\ensuremath{\beta}';
  end;

  function PreviewBorderTargetValid(TargetToken: SizeInt): Boolean;
  var
    Previous: SizeInt;
  begin
    Previous := TargetToken - 1;
    while (Previous >= 0) and IsTrivia(Doc.TokenAt(Previous)) do Dec(Previous);
    Result := (Previous >= 0) and
      (Doc.TokenAt(Previous).Kind = ttkControlWord) and
      (ControlName(Doc.FSource, Doc.TokenAt(Previous)) = 'setlength') and
      PreviewBorderValid(Previous);
  end;

  procedure CheckControls;
  var
    C: SizeInt;
    Item: TTikZToken;
    Cmd: string;
    Allowed, AlreadyReported: Boolean;
  begin
    for C := 0 to Doc.TokenCount - 1 do
    begin
      if CheckCancellation(C) then Exit;
      Item := Doc.TokenAt(C);
      if Item.Kind <> ttkControlWord then Continue;
      Cmd := ControlName(Doc.FSource, Item);
      AlreadyReported := False;
      if IsDangerousCommand(Cmd) then
      begin
        MarkUnsupported(C, tdcUnsupportedCommand,
          'Dynamic or side-effecting command is unsupported: \' + Cmd);
        AlreadyReported := True;
      end;
      if (Cmd = 'providecommand') and not MacroDefinitionValid(Doc, C) then
      begin
        MarkUnsupported(C, tdcUnsupportedDefinition,
          'Only the generated tpx length defaults may be defined');
        AlreadyReported := True;
      end;
      if (Cmd = 'definecolor') and not ColorDefinitionValid(Doc, C) then
      begin
        MarkUnsupported(C, tdcUnsupportedDefinition,
          'Only literal RGB color definitions are supported');
        AlreadyReported := True;
      end;
      if (Cmd = 'documentclass') and not DocumentClassValid(C) then
      begin
        MarkUnsupported(C, tdcUnsupportedContext,
          'Document class is supported only in a .tex wrapper');
        AlreadyReported := True;
      end;
      if (Cmd = 'usepackage') and not UsePackageValid(C) then
      begin
        MarkUnsupported(C, tdcUnsupportedContext,
          'Only supported packages and literal preview options are supported');
        AlreadyReported := True;
      end;
      if (Cmd = 'PreviewEnvironment') and
        not PreviewEnvironmentValid(C) then
      begin
        MarkUnsupported(C, tdcUnsupportedContext,
          'Only PreviewEnvironment{tikzpicture} is supported');
        AlreadyReported := True;
      end;
      if (Cmd = 'setlength') and not PreviewBorderValid(C) then
      begin
        MarkUnsupported(C, tdcUnsupportedContext,
          'Only the literal PreviewBorder point length is supported');
        AlreadyReported := True;
      end;
      if (Cmd = 'DeclareUnicodeCharacter') and
        not UnicodeDeclarationValid(C) then
      begin
        MarkUnsupported(C, tdcUnsupportedContext,
          'Only the supported literal beta Unicode declaration is accepted');
        AlreadyReported := True;
      end;
      if Item.ParentGroup < 0 then
      begin
        Allowed := False;
        if InSelectedRegion(C) then
          Allowed := (Cmd = 'path') or (Cmd = 'draw') or (Cmd = 'node') or
            (Cmd = 'fill') or (Cmd = 'filldraw') or
            (Cmd = 'begin') or (Cmd = 'end') or
            (Cmd = 'tpxLineWidth') or (Cmd = 'tpxDashSize') or
            (Cmd = 'tpxDotSize') or (Cmd = 'tpxTextSize')
        else
          Allowed := (Cmd = 'begin') or (Cmd = 'end') or
            (Cmd = 'begingroup') or (Cmd = 'endgroup') or
            (Cmd = 'beginpgfgraphicnamed') or
            (Cmd = 'endpgfgraphicnamed');
        if Cmd = 'providecommand' then
          Allowed := MacroDefinitionValid(Doc, C);
        if Cmd = 'definecolor' then
          Allowed := ColorDefinitionValid(Doc, C);
        if Cmd = 'documentclass' then
          Allowed := DocumentClassValid(C);
        if Cmd = 'usepackage' then
          Allowed := UsePackageValid(C);
        if Cmd = 'PreviewEnvironment' then
          Allowed := PreviewEnvironmentValid(C);
        if Cmd = 'setlength' then
          Allowed := PreviewBorderValid(C);
        if Cmd = 'PreviewBorder' then
          Allowed := PreviewBorderTargetValid(C);
        if Cmd = 'DeclareUnicodeCharacter' then
          Allowed := UnicodeDeclarationValid(C);
        if not Allowed and not AlreadyReported then
          MarkUnsupported(C, tdcUnsupportedCommand,
            'Unsupported top-level command: \' + Cmd);
      end
      else if InSelectedRegion(C) and HasBracketAncestor(Doc, C) then
      begin
        if (Cmd <> 'tpxLineWidth') and (Cmd <> 'tpxDashSize') and
          (Cmd <> 'tpxDotSize') and (Cmd <> 'tpxTextSize') then
          MarkUnsupported(C, tdcUnsupportedCommand,
            'Unsupported command in a TikZ option: \' + Cmd);
      end;
      if InSelectedRegion(C) and (Item.ParentGroup >= 0) and
        not HasBraceAncestor(Doc, C) and
        not HasBracketAncestor(Doc, C) and
        (Cmd <> 'tpxLineWidth') and (Cmd <> 'tpxDashSize') and
        (Cmd <> 'tpxDotSize') and (Cmd <> 'tpxTextSize') then
        MarkUnsupported(C, tdcUnsupportedCommand,
          'Unsupported command in a TikZ coordinate: \' + Cmd);
    end;
  end;

  procedure CheckStatements;
  var
    C, CloseToken, StatementCount, ArgToken, ArgCount,
      ScopeOptionsOpen: SizeInt;
    Item: TTikZToken;
    StatementOpen: Boolean;
    Cmd: string;
  begin
    StatementOpen := False;
    StatementCount := 0;
    C := Doc.FRegion.FirstContentToken;
    while C < Doc.FRegion.PastLastContentToken do
    begin
      if CheckCancellation(C) then Exit;
      Item := Doc.TokenAt(C);
      if IsTrivia(Item) then
      begin
        Inc(C);
        Continue;
      end;
      if (Item.ParentGroup < 0) and
        ((Item.Kind = ttkOpenBrace) or (Item.Kind = ttkOpenBracket) or
         (Item.Kind = ttkOpenParen)) then
      begin
        CloseToken := GroupClose(Doc, C);
        if CloseToken < C then
        begin
          AddSyntaxDiagnostic(Doc, tdcMalformedWrapper, Item.Span,
            'TikZ statement contains an unmatched group', tsError);
          Exit;
        end;
        if not StatementOpen then
          MarkUnsupported(C, tdcUnsupportedContext,
            'A drawing command must start each TikZ statement');
        C := CloseToken + 1;
        Continue;
      end;
      if (Item.Kind = ttkControlWord) and (Item.ParentGroup < 0) then
      begin
        Cmd := ControlName(Doc.FSource, Item);
        if (Cmd = 'begin') or (Cmd = 'end') then
        begin
          ArgToken := NextSignificant(Doc, C + 1);
          if (ArgToken < Doc.TokenCount) and
            (Doc.TokenAt(ArgToken).Kind = ttkOpenBrace) and
            (PlainGroupValue(Doc, ArgToken) = 'scope') then
          begin
            if StatementOpen then
              MarkUnsupported(C, tdcUnsupportedContext,
                'A scope boundary cannot split a drawing statement');
            CloseToken := GroupClose(Doc, ArgToken);
            C := CloseToken + 1;
            if Cmd = 'begin' then
            begin
              ScopeOptionsOpen := NextSignificant(Doc, C);
              if (ScopeOptionsOpen < Doc.TokenCount) and
                (Doc.TokenAt(ScopeOptionsOpen).Kind = ttkOpenBracket) then
                C := GroupClose(Doc, ScopeOptionsOpen) + 1;
            end;
            Continue;
          end;
        end;
        if Cmd = 'definecolor' then
        begin
          if StatementOpen then
            MarkUnsupported(C, tdcUnsupportedContext,
              'Color declarations must occur between drawing statements');
          ArgToken := NextSignificant(Doc, C + 1);
          ArgCount := 0;
          while (ArgCount < 3) and (ArgToken < Doc.TokenCount) and
            (Doc.TokenAt(ArgToken).Kind = ttkOpenBrace) do
          begin
            CloseToken := GroupClose(Doc, ArgToken);
            if CloseToken < ArgToken then Break;
            ArgToken := NextSignificant(Doc, CloseToken + 1);
            Inc(ArgCount);
          end;
          if ArgCount = 3 then C := ArgToken else Inc(C);
          Continue;
        end;
        if (Cmd = 'path') or (Cmd = 'draw') or (Cmd = 'node') or
          (Cmd = 'fill') or (Cmd = 'filldraw') then
        begin
          if not StatementOpen then
          begin
            StatementOpen := True;
            Inc(StatementCount);
          end;
          Inc(C);
          Continue;
        end;
      end;
      if (Item.Kind = ttkSemicolon) and (Item.ParentGroup < 0) then
      begin
        if not StatementOpen then
          MarkUnsupported(C, tdcUnsupportedContext,
            'TikZ statement terminator has no drawing command')
        else StatementOpen := False;
        Inc(C);
        Continue;
      end;
      if not StatementOpen then
        MarkUnsupported(C, tdcUnsupportedContext,
          'TikZ content outside a drawing statement is unsupported');
      Inc(C);
    end;
    if StatementOpen then
    begin
      Span := SpanForBytes(Source, Length(Source), Length(Source));
      AddSyntaxDiagnostic(Doc, tdcUnterminatedStatement, Span,
        'TikZ drawing statement is missing its terminating semicolon',
        tsError);
    end
    else if Doc.FRegion.IsFragment and (StatementCount = 0) then
      AddSyntaxDiagnostic(Doc, tdcNoPicture, Doc.FRegion.ContentSpan,
        'The fragment contains no drawing statement', tsWarning);
  end;

  procedure CheckOuterContext;
  var
    C, NextArg, CloseToken, OptionalArg, OptionalClose: SizeInt;
    Item: TTikZToken;
    Cmd: string;

    function AfterGroups(StartToken, Count: SizeInt): SizeInt;
    var
      A, N, CloseAt: SizeInt;
    begin
      A := StartToken;
      for N := 1 to Count do
      begin
        A := NextSignificant(Doc, A);
        if (A >= Doc.TokenCount) or
          (Doc.TokenAt(A).Kind <> ttkOpenBrace) then Exit(A);
        CloseAt := GroupClose(Doc, A);
        if CloseAt < A then Exit(A);
        A := CloseAt + 1;
      end;
      Result := A;
    end;

  begin
    if not Doc.FRegion.Found or Doc.FRegion.IsFragment then Exit;
    C := 0;
    while C < Doc.TokenCount do
    begin
      if CheckCancellation(C) then Exit;
      Item := Doc.TokenAt(C);
      if IsTrivia(Item) or InSelectedRegion(C) then
      begin
        Inc(C);
        Continue;
      end;
      if Item.ParentGroup >= 0 then
      begin
        Inc(C);
        Continue;
      end;
      if Item.Kind = ttkControlWord then
      begin
        Cmd := ControlName(Doc.FSource, Item);
        NextArg := NextSignificant(Doc, C + 1);
        if (Cmd = 'begin') or (Cmd = 'end') then
        begin
          CloseToken := GroupClose(Doc, NextArg);
          if CloseToken >= NextArg then C := CloseToken + 1
          else Inc(C);
          if (Cmd = 'begin') and (C < Doc.TokenCount) then
          begin
            OptionalArg := NextSignificant(Doc, C);
            if (OptionalArg < Doc.TokenCount) and
              (Doc.TokenAt(OptionalArg).Kind = ttkOpenBracket) then
            begin
              OptionalClose := GroupClose(Doc, OptionalArg);
              if OptionalClose >= OptionalArg then C := OptionalClose + 1;
            end;
          end;
          Continue;
        end;
        if (Cmd = 'documentclass') or (Cmd = 'usepackage') then
        begin
          OptionalArg := NextSignificant(Doc, NextArg);
          if (OptionalArg < Doc.TokenCount) and
            (Doc.TokenAt(OptionalArg).Kind = ttkOpenBracket) then
          begin
            OptionalClose := GroupClose(Doc, OptionalArg);
            if OptionalClose >= OptionalArg then
              OptionalArg := OptionalClose + 1;
          end
          else OptionalArg := NextArg;
          C := AfterGroups(OptionalArg, 1);
          Continue;
        end;
        if Cmd = 'providecommand' then
        begin
          C := AfterGroups(NextArg, 2);
          Continue;
        end;
        if (Cmd = 'definecolor') or (Cmd = 'DeclareUnicodeCharacter') then
        begin
          if Cmd = 'definecolor' then C := AfterGroups(NextArg, 3)
          else C := AfterGroups(NextArg, 2);
          Continue;
        end;
        if Cmd = 'PreviewEnvironment' then
        begin
          C := AfterGroups(NextArg, 1);
          Continue;
        end;
        if Cmd = 'setlength' then
        begin
          OptionalArg := NextSignificant(Doc, NextArg + 1);
          C := AfterGroups(OptionalArg, 1);
          Continue;
        end;
        if Cmd = 'beginpgfgraphicnamed' then
        begin
          CloseToken := GroupClose(Doc, NextArg);
          if CloseToken >= NextArg then C := CloseToken + 1 else Inc(C);
          Continue;
        end;
        Inc(C);
        Continue;
      end;
      if (Item.Kind = ttkOpenBrace) or (Item.Kind = ttkOpenBracket) or
        (Item.Kind = ttkOpenParen) then
      begin
        AddSyntaxDiagnostic(Doc, tdcUnsupportedContext, Item.Span,
          'Unattached group outside the selected picture is unsupported',
          tsWarning);
        CloseToken := GroupClose(Doc, C);
        if CloseToken >= C then C := CloseToken + 1 else Inc(C);
        Continue;
      end;
      AddSyntaxDiagnostic(Doc, tdcUnsupportedContext, Item.Span,
        'Non-command syntax outside the selected picture is unsupported',
        tsWarning);
      Inc(C);
    end;
  end;

begin
  Result := TTikZSyntaxResult.Create;
  Doc := Result;
  Result.FOutcome := tpoAccepted;
  Result.FSource := nil;
  Result.FTokens := nil;
  Result.FGroups := nil;
  Result.FDiagnostics := nil;
  Result.FDiagnosticCount := 0;
  Result.FDiagnosticCapacity := 0;
  Result.FRegion.Found := False;
  Result.FRegion.IsFragment := False;
  Result.FRegion.BeginToken := -1;
  Result.FRegion.EndToken := -1;
  Result.FRegion.FirstContentToken := -1;
  Result.FRegion.PastLastContentToken := -1;
  LastParserCheck := -1024;
  LexData := LexTikZ(Source, Options.LexerOptions);
  if (Options.LexerOptions.MaxSourceBytes > 0) and
    (Length(Source) <= Options.LexerOptions.MaxSourceBytes) then
  begin
    SetLength(Result.FSource, Length(Source));
    for I := 0 to High(Source) do Result.FSource[I] := Source[I];
  end;
  Result.FTokens := LexData.Tokens;
  Result.FGroups := LexData.Groups;
  Result.FDiagnostics := LexData.Diagnostics;
  Result.FDiagnosticCount := Length(LexData.Diagnostics);
  Result.FDiagnosticCapacity := Result.FDiagnosticCount;
  if LexData.Cancelled then
  begin
    Result.FOutcome := tpoCancelled;
    Exit;
  end;
  if LexData.Invalid then
  begin
    Result.FOutcome := tpoInvalid;
    Exit;
  end;
  LastParserCheck := -1024;
  if CheckCancellation(0) then Exit;
  Environments := nil;
  MacroGroups := nil;
  EnvironmentUsed := 0;
  EnvironmentCapacity := 0;
  MacroGroupUsed := 0;
  MacroGroupCapacity := 0;
  CandidateCount := 0;
  LastParserCheck := 0;
  I := 0;
  while I < Result.TokenCount do
  begin
    if CheckCancellation(I) then Exit;
    Token := Result.TokenAt(I);
    if IsTrivia(Token) then
    begin
      Inc(I);
      Continue;
    end;
    if (Token.Kind = ttkControlWord) and (Token.ParentGroup < 0) then
    begin
      Name := ControlName(Result.FSource, Token);
      if (Name = 'begin') or (Name = 'end') then
      begin
        ArgToken := NextSignificant(Result, I + 1);
        if (ArgToken >= Result.TokenCount) or
          (Result.TokenAt(ArgToken).Kind <> ttkOpenBrace) then
        begin
          AddSyntaxDiagnostic(Result, tdcMalformedWrapper, Token.Span,
            'Environment command requires a braced name', tsError);
          Exit;
        end;
        ArgClose := GroupClose(Result, ArgToken);
        EnvName := PlainGroupValue(Result, ArgToken);
        if (ArgClose < 0) or (EnvName = '') then
        begin
          AddSyntaxDiagnostic(Result, tdcMalformedWrapper, Token.Span,
            'Environment name is not a plain supported name', tsError);
          Exit;
        end;
        if Name = 'begin' then
        begin
          if (EnvName <> 'tikzpicture') and (EnvName <> 'document') and
            (EnvName <> 'center') and (EnvName <> 'figure') and
            (EnvName <> 'scope') then
            AddSyntaxDiagnostic(Result, tdcUnsupportedContext, Token.Span,
              'Unsupported environment: ' + EnvName, tsWarning);
          if (EnvName = 'scope') and not InsideTikZEnvironment then
            AddSyntaxDiagnostic(Result, tdcUnsupportedContext, Token.Span,
              'A scope environment is supported only inside tikzpicture',
              tsWarning);
          if InsideTikZEnvironment and (EnvName <> 'tikzpicture') and
            (EnvName <> 'scope') then
            AddSyntaxDiagnostic(Result, tdcUnsupportedContext, Token.Span,
              'Nested environments inside a picture are unsupported',
              tsWarning);
          if EnvName = 'scope' then
          begin
            ScopeOptionToken := NextSignificant(Result, ArgClose + 1);
            if (ScopeOptionToken < Result.TokenCount) and
              (Result.TokenAt(ScopeOptionToken).Kind = ttkOpenBracket) and
              not ScopeTransformOptionsValid(Result, ScopeOptionToken,
                Options.LexerOptions.CancelCheck,
                Options.LexerOptions.CancelContext, ScopeBadToken,
                ScopeCancelled) then
            begin
              if ScopeCancelled then
              begin
                AddSyntaxDiagnostic(Result, tdcCancelled, Token.Span,
                  'TikZ parsing was cancelled', tsError);
                Result.FOutcome := tpoCancelled;
                Exit;
              end;
              AddSyntaxDiagnostic(Result, tdcUnsupportedContext,
                Result.TokenAt(ScopeBadToken).Span,
                'Scope options must use literal shift, rotate, or scale values',
                tsWarning);
            end;
          end;
          if EnvironmentUsed >= Options.LexerOptions.MaxNestingDepth then
          begin
            AddSyntaxDiagnostic(Result, tdcNestingTooDeep, Token.Span,
              'TikZ environment nesting exceeds the configured limit', tsError);
            Exit;
          end;
          if EnvironmentUsed = EnvironmentCapacity then
          begin
            EnvironmentCapacity := NextCapacity(EnvironmentCapacity,
              Options.LexerOptions.MaxNestingDepth);
            SetLength(Environments, EnvironmentCapacity);
          end;
          Environments[EnvironmentUsed].Name := EnvName;
          Environments[EnvironmentUsed].BeginToken := I;
          Inc(EnvironmentUsed);
        end
        else
        begin
          if (EnvironmentUsed = 0) or
            (Environments[EnvironmentUsed - 1].Name <> EnvName) then
          begin
            AddSyntaxDiagnostic(Result, tdcUnmatchedEnvironment,
              Token.Span, 'Environment end does not match its open begin',
              tsError);
            Exit;
          end;
          if EnvName = 'tikzpicture' then
          begin
            Inc(CandidateCount);
            if CandidateCount = 1 then
              SetCandidate(Environments[EnvironmentUsed - 1].BeginToken,
                I, GroupClose(Result,
                  NextSignificant(Result,
                    Environments[EnvironmentUsed - 1].BeginToken + 1)),
                ArgClose);
            if CandidateCount = 2 then
            begin
              AddSyntaxDiagnostic(Result, tdcMultiplePictures,
                Result.TokenAt(I).Span,
                'More than one picture region is eligible for import',
                tsWarning);
              Exit;
            end;
          end;
          Dec(EnvironmentUsed);
        end;
        I := ArgClose + 1;
        Continue;
      end;
      if (Name = 'begingroup') or (Name = 'beginpgfgraphicnamed') then
      begin
        if MacroGroupUsed >= Options.LexerOptions.MaxNestingDepth then
        begin
          AddSyntaxDiagnostic(Result, tdcNestingTooDeep, Token.Span,
            'TikZ wrapper nesting exceeds the configured limit', tsError);
          Exit;
        end;
        if MacroGroupUsed = MacroGroupCapacity then
        begin
          MacroGroupCapacity := NextCapacity(MacroGroupCapacity,
            Options.LexerOptions.MaxNestingDepth);
          SetLength(MacroGroups, MacroGroupCapacity);
        end;
        MacroGroups[MacroGroupUsed] := Name;
        Inc(MacroGroupUsed);
      end
      else if (Name = 'endgroup') or (Name = 'endpgfgraphicnamed') then
      begin
        if (MacroGroupUsed = 0) or
          (((Name = 'endgroup') and
            (MacroGroups[MacroGroupUsed - 1] <> 'begingroup')) or
           ((Name = 'endpgfgraphicnamed') and
            (MacroGroups[MacroGroupUsed - 1] <> 'beginpgfgraphicnamed'))) then
        begin
          AddSyntaxDiagnostic(Result, tdcMalformedWrapper, Token.Span,
            'Wrapper end has no matching begin', tsError);
          Exit;
        end;
        Dec(MacroGroupUsed);
      end;
    end;
    if ((Token.Kind = ttkOpenBrace) or (Token.Kind = ttkOpenBracket) or
      (Token.Kind = ttkOpenParen)) and (Token.ParentGroup < 0) then
    begin
      J := GroupClose(Result, I);
      if J >= I then I := J + 1 else Inc(I);
    end
    else Inc(I);
  end;
  if EnvironmentUsed <> 0 then
  begin
    Token := Result.TokenAt(Environments[EnvironmentUsed - 1].BeginToken);
    AddSyntaxDiagnostic(Result, tdcUnmatchedEnvironment, Token.Span,
      'Environment has no matching end', tsError);
    Exit;
  end;
  if MacroGroupUsed <> 0 then
  begin
    AddSyntaxDiagnostic(Result, tdcMalformedWrapper,
      Result.TokenAt(Result.TokenCount - 1).Span,
      'Wrapper has no matching end', tsError);
    Exit;
  end;
  if CandidateCount > 1 then
  begin
    AddSyntaxDiagnostic(Result, tdcMultiplePictures,
      Result.TokenAt(0).Span,
      'More than one picture region is eligible for import', tsWarning);
    Exit;
  end;
  if CandidateCount = 1 then Result.FRegion := Candidate
  else if Options.InputKind = tikFragmentInput then
  begin
    Result.FRegion.Found := True;
    Result.FRegion.IsFragment := True;
    Result.FRegion.Span := SpanForBytes(Source, 0, Length(Source));
    Result.FRegion.HeaderSpan := EmptySpanAt(0, 1, 1);
    Result.FRegion.ContentSpan := Result.FRegion.Span;
    Result.FRegion.FirstContentToken := 0;
    Result.FRegion.PastLastContentToken := Result.TokenCount;
  end
  else
  begin
    Span := EmptySpanAt(0, 1, 1);
    AddSyntaxDiagnostic(Result, tdcNoPicture, Span,
      'No uniquely eligible tikzpicture environment was found', tsWarning);
    Exit;
  end;
  LastParserCheck := -1024;
  CheckControls;
  if Result.FOutcome = tpoCancelled then Exit;
  LastParserCheck := -1024;
  CheckOuterContext;
  if Result.FOutcome = tpoCancelled then Exit;
  LastParserCheck := -1024;
  CheckStatements;
end;

end.
