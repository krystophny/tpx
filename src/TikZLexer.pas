unit TikZLexer;

{$mode objfpc}{$H+}

interface

uses SysUtils;

type
  TTikZTokenKind = (
    ttkControlWord, ttkControlSymbol, ttkWhitespace, ttkComment,
    ttkWord, ttkNumber, ttkDimension, ttkOpenBrace, ttkCloseBrace,
    ttkOpenBracket, ttkCloseBracket, ttkOpenParen, ttkCloseParen,
    ttkSemicolon, ttkComma, ttkColon, ttkEquals, ttkOperator, ttkOther
  );

  TTikZGroupKind = (tgBrace, tgBracket, tgParen);

  { Byte offsets are zero-based and EndByte is exclusive. Line and column
    values are one-based byte positions; no Unicode decoding is performed. }
  TTikZSourceSpan = record
    StartByte, EndByte: SizeInt;
    StartLine, StartColumn: SizeInt;
    EndLine, EndColumn: SizeInt;
  end;

  TTikZToken = record
    Kind: TTikZTokenKind;
    Span: TTikZSourceSpan;
    MatchingToken: SizeInt;
    ParentGroup: SizeInt;
  end;
  TTikZTokens = array of TTikZToken;

  TTikZGroup = record
    Kind: TTikZGroupKind;
    OpenToken, CloseToken: SizeInt;
    ParentGroup: SizeInt;
    Span: TTikZSourceSpan;
    Depth: SizeInt;
  end;
  TTikZGroups = array of TTikZGroup;

  TTikZDiagnosticCode = (
    tdcSourceTooLarge, tdcTooManyTokens, tdcNestingTooDeep,
    tdcUnterminatedGroup, tdcUnexpectedCloser, tdcTrailingEscape,
    tdcCancelled, tdcNoPicture, tdcMultiplePictures,
    tdcUnmatchedEnvironment, tdcUnsupportedCommand,
    tdcUnsupportedContext, tdcUnsupportedDefinition,
    tdcMalformedWrapper, tdcUnterminatedStatement
  );

  TTikZSeverity = (tsWarning, tsError);

  TTikZDiagnostic = record
    Code: TTikZDiagnosticCode;
    Severity: TTikZSeverity;
    Span: TTikZSourceSpan;
    Message: string;
  end;
  TTikZDiagnostics = array of TTikZDiagnostic;

  TTikZCancelCheck = function(Context: Pointer): Boolean;

  TTikZLexerOptions = record
    MaxSourceBytes: SizeInt;
    MaxTokens: SizeInt;
    MaxNestingDepth: SizeInt;
    CancelCheck: TTikZCancelCheck;
    CancelContext: Pointer;
  end;

  TTikZLexResult = record
    Tokens: TTikZTokens;
    Groups: TTikZGroups;
    Diagnostics: TTikZDiagnostics;
    SourceBytes: SizeInt;
    Cancelled: Boolean;
    Invalid: Boolean;
  end;

function DefaultTikZLexerOptions: TTikZLexerOptions;
function LexTikZ(const Source: TBytes;
  const Options: TTikZLexerOptions): TTikZLexResult;

implementation

type
  TGroupStack = array of SizeInt;

function DefaultTikZLexerOptions: TTikZLexerOptions;
begin
  Result.MaxSourceBytes := 4 * 1024 * 1024;
  Result.MaxTokens := 500000;
  Result.MaxNestingDepth := 256;
  Result.CancelCheck := nil;
  Result.CancelContext := nil;
end;

function IsAlpha(B: Byte): Boolean; inline;
begin
  Result := ((B >= Ord('a')) and (B <= Ord('z'))) or
    ((B >= Ord('A')) and (B <= Ord('Z')));
end;

function IsDigit(B: Byte): Boolean; inline;
begin
  Result := (B >= Ord('0')) and (B <= Ord('9'));
end;

function IsSpace(B: Byte): Boolean; inline;
begin
  Result := (B = 9) or (B = 10) or (B = 11) or (B = 12) or
    (B = 13) or (B = 32);
end;

function IsUnit(const Source: TBytes; StartAt, Count: SizeInt): Boolean;
var
  U: string;
  I: SizeInt;
begin
  SetLength(U, Count);
  for I := 1 to Count do
  begin
    U[I] := Chr(Source[StartAt + I - 1]);
    if (U[I] >= 'A') and (U[I] <= 'Z') then
      U[I] := Chr(Ord(U[I]) + Ord('a') - Ord('A'));
  end;
  Result := (U = 'mm') or (U = 'cm') or (U = 'pt') or (U = 'bp') or
    (U = 'in') or (U = 'ex') or (U = 'em') or (U = 'sp') or
    (U = 'dd') or (U = 'pc') or (U = 'px');
end;

function PairKind(K: TTikZTokenKind; out G: TTikZGroupKind;
  out IsOpen: Boolean): Boolean;
begin
  Result := True;
  case K of
    ttkOpenBrace: begin G := tgBrace; IsOpen := True; end;
    ttkCloseBrace: begin G := tgBrace; IsOpen := False; end;
    ttkOpenBracket: begin G := tgBracket; IsOpen := True; end;
    ttkCloseBracket: begin G := tgBracket; IsOpen := False; end;
    ttkOpenParen: begin G := tgParen; IsOpen := True; end;
    ttkCloseParen: begin G := tgParen; IsOpen := False; end;
  else
    Result := False;
    G := tgBrace;
    IsOpen := False;
  end;
end;

function SpanAt(const Source: TBytes; StartAt, EndAt: SizeInt): TTikZSourceSpan;
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

procedure AddDiagnostic(var Data: TTikZLexResult; Code: TTikZDiagnosticCode;
  const Span: TTikZSourceSpan; const Message: string; Severity: TTikZSeverity);
var
  N: SizeInt;
begin
  N := Length(Data.Diagnostics);
  SetLength(Data.Diagnostics, N + 1);
  Data.Diagnostics[N].Code := Code;
  Data.Diagnostics[N].Severity := Severity;
  Data.Diagnostics[N].Span := Span;
  Data.Diagnostics[N].Message := Message;
  if Severity = tsError then Data.Invalid := True;
end;

function LexTikZ(const Source: TBytes;
  const Options: TTikZLexerOptions): TTikZLexResult;
var
  I, J, N, GIndex, Parent, GroupDepth, LineNo, ColNo: SizeInt;
  TokenUsed, TokenCapacity, GroupUsed, GroupCapacity, StackUsed,
    StackCapacity: SizeInt;
  LastCancel: SizeInt;
  PreviousCR: Boolean;
  Stack: TGroupStack;
  K: TTikZTokenKind;
  GK: TTikZGroupKind;
  StartLine, StartColumn: SizeInt;
  Span: TTikZSourceSpan;

  procedure AdvanceTo(NewPos: SizeInt);
  var
    P: SizeInt;
  begin
    while I < NewPos do
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
      Inc(I);
      P := I;
      if (Options.CancelCheck <> nil) and
        (P - LastCancel >= 4096) then
      begin
        LastCancel := P;
        if Options.CancelCheck(Options.CancelContext) then
        begin
          Result.Cancelled := True;
          Span.StartByte := P;
          Span.EndByte := P;
          Span.StartLine := LineNo;
          Span.EndLine := LineNo;
          Span.StartColumn := ColNo;
          Span.EndColumn := ColNo;
          AddDiagnostic(Result, tdcCancelled, Span,
            'TikZ parsing was cancelled', tsError);
          Exit;
        end;
      end;
    end;
  end;

  procedure AddToken(TokenKind: TTikZTokenKind; StartAt, EndAt: SizeInt);
  var
    TokenIndex, NewGroupIndex, ScanIndex, NewCapacity: SizeInt;
    TokenSpan: TTikZSourceSpan;
    GroupOpen: Boolean;
  begin
    if TokenUsed >= Options.MaxTokens then
    begin
      TokenSpan := SpanAt(Source, StartAt, EndAt);
      AddDiagnostic(Result, tdcTooManyTokens, TokenSpan,
        'TikZ source exceeds the token limit', tsError);
      Exit;
    end;
    if TokenUsed = TokenCapacity then
    begin
      NewCapacity := TokenCapacity;
      if NewCapacity = 0 then NewCapacity := 64
      else if NewCapacity > Options.MaxTokens div 2 then
        NewCapacity := Options.MaxTokens
      else NewCapacity := NewCapacity * 2;
      if NewCapacity > Options.MaxTokens then NewCapacity := Options.MaxTokens;
      SetLength(Result.Tokens, NewCapacity);
      TokenCapacity := NewCapacity;
    end;
    TokenIndex := TokenUsed;
    Inc(TokenUsed);
    TokenSpan.StartByte := StartAt;
    TokenSpan.EndByte := EndAt;
    TokenSpan.StartLine := StartLine;
    TokenSpan.StartColumn := StartColumn;
    TokenSpan.EndLine := LineNo;
    TokenSpan.EndColumn := ColNo;
    Result.Tokens[TokenIndex].Kind := TokenKind;
    Result.Tokens[TokenIndex].Span := TokenSpan;
    Result.Tokens[TokenIndex].MatchingToken := -1;
    if StackUsed = 0 then Parent := -1
    else Parent := Stack[StackUsed - 1];
    Result.Tokens[TokenIndex].ParentGroup := Parent;
    if not PairKind(TokenKind, GK, GroupOpen) then Exit;
    if GroupOpen then
    begin
      GroupDepth := StackUsed + 1;
      if GroupDepth > Options.MaxNestingDepth then
      begin
        AddDiagnostic(Result, tdcNestingTooDeep, TokenSpan,
          'TikZ source exceeds the group nesting limit', tsError);
        Exit;
      end;
      if GroupUsed = GroupCapacity then
      begin
        NewCapacity := GroupCapacity;
        if NewCapacity = 0 then NewCapacity := 64
        else if NewCapacity > Options.MaxTokens div 2 then
          NewCapacity := Options.MaxTokens
        else NewCapacity := NewCapacity * 2;
        if NewCapacity > Options.MaxTokens then NewCapacity := Options.MaxTokens;
        SetLength(Result.Groups, NewCapacity);
        GroupCapacity := NewCapacity;
      end;
      NewGroupIndex := GroupUsed;
      Inc(GroupUsed);
      Result.Groups[NewGroupIndex].Kind := GK;
      Result.Groups[NewGroupIndex].OpenToken := TokenIndex;
      Result.Groups[NewGroupIndex].CloseToken := -1;
      Result.Groups[NewGroupIndex].ParentGroup := Parent;
      Result.Groups[NewGroupIndex].Span := TokenSpan;
      Result.Groups[NewGroupIndex].Depth := GroupDepth;
      Stack[StackUsed] := NewGroupIndex;
      Inc(StackUsed);
    end
    else
    begin
      while (StackUsed > 0) and
        (Result.Groups[Stack[StackUsed - 1]].Kind <> GK) and
        (GK = tgBrace) do
        Dec(StackUsed);
      if (StackUsed = 0) or
        (Result.Groups[Stack[StackUsed - 1]].Kind <> GK) then
      begin
        GroupDepth := 0;
        for ScanIndex := StackUsed - 1 downto 0 do
          if Result.Groups[Stack[ScanIndex]].Kind = tgBrace then
          begin
            GroupDepth := 1;
            Break;
          end;
        if (GK <> tgBrace) and (GroupDepth <> 0) then Exit;
        AddDiagnostic(Result, tdcUnexpectedCloser, TokenSpan,
          'Unexpected closing delimiter', tsError);
        Exit;
      end;
      NewGroupIndex := Stack[StackUsed - 1];
      Dec(StackUsed);
      Result.Groups[NewGroupIndex].CloseToken := TokenIndex;
      Result.Groups[NewGroupIndex].Span.EndByte := EndAt;
      Result.Groups[NewGroupIndex].Span.EndLine := LineNo;
      Result.Groups[NewGroupIndex].Span.EndColumn := ColNo;
      Result.Tokens[TokenIndex].MatchingToken :=
        Result.Groups[NewGroupIndex].OpenToken;
      Result.Tokens[TokenIndex].ParentGroup :=
        Result.Groups[NewGroupIndex].ParentGroup;
      Result.Tokens[Result.Groups[NewGroupIndex].OpenToken].MatchingToken :=
        TokenIndex;
    end;
  end;

  procedure EmitAt(TokenKind: TTikZTokenKind; StartAt, EndAt: SizeInt);
  begin
    StartLine := LineNo;
    StartColumn := ColNo;
    AdvanceTo(EndAt);
    if not Result.Cancelled then AddToken(TokenKind, StartAt, EndAt);
  end;

  function IsBraceOpen(Index: SizeInt): Boolean;
  begin
    Result := (Index < Length(Source)) and (Source[Index] = Ord('{'));
  end;

var
  NumberEnd, UnitEnd, ExpEnd, VerbStart, VerbEnd, VerbDelimiter: SizeInt;
begin
  Result.Tokens := nil;
  Result.Groups := nil;
  Result.Diagnostics := nil;
  Result.Cancelled := False;
  Result.Invalid := False;
  Result.SourceBytes := Length(Source);
  if Options.MaxSourceBytes <= 0 then
  begin
    Result := Default(TTikZLexResult);
    Result.SourceBytes := Length(Source);
    AddDiagnostic(Result, tdcSourceTooLarge, SpanAt(Source, 0, 0),
      'Invalid TikZ source size limit', tsError);
    Exit;
  end;
  if Length(Source) > Options.MaxSourceBytes then
  begin
    AddDiagnostic(Result, tdcSourceTooLarge, SpanAt(Source, 0, 0),
      'TikZ source exceeds the byte limit', tsError);
    Exit;
  end;
  if Options.MaxTokens <= 0 then
  begin
    AddDiagnostic(Result, tdcTooManyTokens, SpanAt(Source, 0, 0),
      'Invalid TikZ token limit', tsError);
    Exit;
  end;
  if Options.MaxNestingDepth <= 0 then
  begin
    AddDiagnostic(Result, tdcNestingTooDeep, SpanAt(Source, 0, 0),
      'Invalid TikZ nesting limit', tsError);
    Exit;
  end;
  I := 0;
  LineNo := 1;
  ColNo := 1;
  LastCancel := 0;
  PreviousCR := False;
  StackUsed := 0;
  StackCapacity := Options.MaxNestingDepth;
  if StackCapacity > Options.MaxTokens then StackCapacity := Options.MaxTokens;
  if StackCapacity > Length(Source) then StackCapacity := Length(Source);
  SetLength(Stack, StackCapacity);
  TokenUsed := 0;
  TokenCapacity := 0;
  GroupUsed := 0;
  GroupCapacity := 0;
  while I < Length(Source) do
  begin
    if Result.Cancelled or Result.Invalid then Break;
    J := I;
    if (I = 0) and (Length(Source) >= 3) and
      (Source[0] = $EF) and (Source[1] = $BB) and (Source[2] = $BF) then
    begin
      J := 3;
      K := ttkWhitespace; { A leading UTF-8 BOM is retained as trivia. }
    end
    else if IsSpace(Source[I]) then
    begin
      Inc(J);
      while (J < Length(Source)) and IsSpace(Source[J]) do Inc(J);
      K := ttkWhitespace;
    end
    else if Source[I] = Ord('%') then
    begin
      Inc(J);
      while (J < Length(Source)) and
        (Source[J] <> 10) and (Source[J] <> 13) do Inc(J);
      K := ttkComment;
    end
    else if Source[I] = Ord('\') then
    begin
      Inc(J);
      if J = Length(Source) then
      begin
        EmitAt(ttkControlSymbol, I, J);
        if not Result.Cancelled and not Result.Invalid then
          AddDiagnostic(Result, tdcTrailingEscape,
            Result.Tokens[High(Result.Tokens)].Span,
            'A trailing backslash has no control symbol', tsError);
        Continue;
      end;
      if IsAlpha(Source[J]) then
      begin
        Inc(J);
        while (J < Length(Source)) and IsAlpha(Source[J]) do Inc(J);
        K := ttkControlWord;
      end
      else
      begin
        Inc(J);
        K := ttkControlSymbol;
      end;
      if (K = ttkControlWord) and (J - I = 5) and
        (Source[I + 1] = Ord('v')) and (Source[I + 2] = Ord('e')) and
        (Source[I + 3] = Ord('r')) and (Source[I + 4] = Ord('b')) then
      begin
        VerbStart := J;
        if (VerbStart < Length(Source)) and
          (Source[VerbStart] = Ord('*')) then Inc(VerbStart);
        if VerbStart < Length(Source) then
        begin
          VerbDelimiter := Source[VerbStart];
          VerbEnd := VerbStart + 1;
          while (VerbEnd < Length(Source)) and
            (Source[VerbEnd] <> VerbDelimiter) and
            (Source[VerbEnd] <> 10) and (Source[VerbEnd] <> 13) do
            Inc(VerbEnd);
          if (VerbEnd < Length(Source)) and
            (Source[VerbEnd] = VerbDelimiter) then Inc(VerbEnd);
          EmitAt(ttkControlWord, I, J);
          if VerbEnd > J then EmitAt(ttkOther, J, VerbEnd);
          Continue;
        end;
      end;
    end
    else if IsDigit(Source[I]) or
      ((Source[I] = Ord('.')) and (I + 1 < Length(Source)) and
       IsDigit(Source[I + 1])) then
    begin
      NumberEnd := I;
      if Source[NumberEnd] = Ord('.') then Inc(NumberEnd);
      while (NumberEnd < Length(Source)) and
        IsDigit(Source[NumberEnd]) do Inc(NumberEnd);
      if (NumberEnd < Length(Source)) and
        (Source[NumberEnd] = Ord('.')) then
      begin
        Inc(NumberEnd);
        while (NumberEnd < Length(Source)) and
          IsDigit(Source[NumberEnd]) do Inc(NumberEnd);
      end;
      ExpEnd := NumberEnd;
      if (ExpEnd < Length(Source)) and
        ((Source[ExpEnd] = Ord('e')) or (Source[ExpEnd] = Ord('E'))) then
      begin
        UnitEnd := ExpEnd + 1;
        if (UnitEnd < Length(Source)) and
          ((Source[UnitEnd] = Ord('+')) or (Source[UnitEnd] = Ord('-'))) then
          Inc(UnitEnd);
        if (UnitEnd < Length(Source)) and IsDigit(Source[UnitEnd]) then
        begin
          Inc(UnitEnd);
          while (UnitEnd < Length(Source)) and
            IsDigit(Source[UnitEnd]) do Inc(UnitEnd);
          NumberEnd := UnitEnd;
        end;
      end;
      UnitEnd := NumberEnd;
      while (UnitEnd < Length(Source)) and IsAlpha(Source[UnitEnd]) do
        Inc(UnitEnd);
      if (UnitEnd > NumberEnd) and IsUnit(Source, NumberEnd,
        UnitEnd - NumberEnd) then
      begin
        K := ttkDimension;
        J := UnitEnd;
      end
      else
      begin
        K := ttkNumber;
        J := NumberEnd;
      end;
    end
    else if IsAlpha(Source[I]) then
    begin
      Inc(J);
      while (J < Length(Source)) and IsAlpha(Source[J]) do Inc(J);
      K := ttkWord;
    end
    else
    begin
      Inc(J);
      case Source[I] of
        Ord('{'): K := ttkOpenBrace;
        Ord('}'): K := ttkCloseBrace;
        Ord('['): K := ttkOpenBracket;
        Ord(']'): K := ttkCloseBracket;
        Ord('('): K := ttkOpenParen;
        Ord(')'): K := ttkCloseParen;
        Ord(';'): K := ttkSemicolon;
        Ord(','): K := ttkComma;
        Ord(':'): K := ttkColon;
        Ord('='): K := ttkEquals;
        Ord('+'), Ord('-'), Ord('*'), Ord('/'), Ord('^'), Ord('_'),
        Ord('<'), Ord('>'), Ord('!'), Ord('|'), Ord('&'): K := ttkOperator;
      else
        K := ttkOther;
        if Source[I] >= 128 then
          while (J < Length(Source)) and (Source[J] >= 128) do Inc(J);
      end;
    end;
    EmitAt(K, I, J);
  end;
  if not Result.Cancelled and not Result.Invalid then
  begin
    for N := StackUsed - 1 downto 0 do
      if (Result.Groups[Stack[N]].Kind = tgBrace) or
        (Result.Groups[Stack[N]].ParentGroup < 0) then
      begin
        GIndex := Stack[N];
        Span := Result.Groups[GIndex].Span;
        Span.EndByte := Length(Source);
        Span.EndLine := LineNo;
        Span.EndColumn := ColNo;
        AddDiagnostic(Result, tdcUnterminatedGroup, Span,
          'TikZ source contains an unterminated group or option list',
          tsError);
        Break;
      end;
  end;
  SetLength(Result.Tokens, TokenUsed);
  SetLength(Result.Groups, GroupUsed);
end;

end.
