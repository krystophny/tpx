procedure CheckEditSessionOrder(Drawing: TDrawing2D;
  First, Second: TObject2D; const Description: string);
var Obj: TGraphicObject;
begin
  Obj := Drawing.ObjectList.FirstObj;
  Check(Obj = First, Description + ': first object changed');
  Obj := Drawing.ObjectList.NextObj;
  Check(Obj = Second, Description + ': second object changed');
end;

procedure SetFirstProvisionalPointCount(var State: RawByteString;
  PointCount: Longint);
var
  Stream: TMemoryStream;
  TargetCount, PathLength, ClassLength, ProvisionalCount,
    ObjectLength: LongWord;
  ClassIndex: Word;
  I: Integer;
  ObjectStart: Int64;
begin
  Stream := TMemoryStream.Create;
  try
    if Length(State) = 0 then
      raise EReadError.Create('Missing edit recovery fixture');
    Stream.WriteBuffer(State[1], Length(State));
    Stream.Position := 8 + 1;
    Stream.ReadBuffer(TargetCount, SizeOf(TargetCount));
    for I := 0 to Integer(TargetCount) - 1 do
    begin
      Stream.ReadBuffer(PathLength, SizeOf(PathLength));
      if Int64(PathLength) * SizeOf(LongWord) >
        Stream.Size - Stream.Position then
        raise EReadError.Create('Invalid edit recovery target fixture');
      Stream.Position := Stream.Position +
        Int64(PathLength) * SizeOf(LongWord);
      Stream.ReadBuffer(ClassLength, SizeOf(ClassLength));
      if ClassLength > Stream.Size - Stream.Position then
        raise EReadError.Create('Invalid edit recovery class fixture');
      Stream.Position := Stream.Position + ClassLength;
    end;
    Stream.ReadBuffer(ProvisionalCount, SizeOf(ProvisionalCount));
    if ProvisionalCount = 0 then
      raise EReadError.Create('Missing provisional recovery fixture');
    Stream.ReadBuffer(ClassIndex, SizeOf(ClassIndex));
    Stream.ReadBuffer(ObjectLength, SizeOf(ObjectLength));
    ObjectStart := Stream.Position;
    if (ObjectLength < 13) or
      (ObjectLength > Stream.Size - ObjectStart) then
      raise EReadError.Create('Invalid provisional recovery fixture');
    { TGraphicObject's stream header is nine bytes; the primitive point count
      follows it before the point records. }
    Move(PointCount, State[ObjectStart + 10], SizeOf(PointCount));
  finally
    Stream.Free;
  end;
end;

procedure TestEditSessionPreviewCancel;
var
  Drawing: TDrawing2D;
  First, Second, Provisional, Restored: TText2D;
  LineOne, LineTwo: TLine2D;
  Session: TObjectEditSession;
  Patch: TObjectPropertyPatch;
  PropertyValue: TPropertyValue;
  ErrorText, OriginalText: string;
  OriginalStoredText: AnsiString;
  FirstID, SecondID, HistoryCount, ProvisionalID, PromptCountBefore: Integer;
  LineOneID, LineTwoID: Integer;
  LineIDs: array[0..1] of Integer;
  Epoch: QWord;
  CanClose: Boolean;
begin
  Drawing := MainForm.TheDrawing;
  First := TText2D.CreateSpec(-1, Point2D(2, 3), 7, 'First');
  First.TeXText := '\alpha';
  Second := TText2D.CreateSpec(-1, Point2D(8, 3), 6, 'Second');
  Drawing.AddObject(-1, First);
  Drawing.AddObject(-1, Second);
  FirstID := First.ID;
  SecondID := Second.ID;
  OriginalStoredText := First.Text;
  Epoch := MainForm.CurrentDocumentEpoch;
  Drawing.History.Save;
  Drawing.History.SaveCheckSum;
  HistoryCount := Drawing.History.Count;
  Check(not Drawing.History.IsChanged,
    'test fixture must start at a clean history baseline');

  Session := TObjectEditSession.Create(Drawing, Epoch, [FirstID]);
  try
    Check(MainForm.RegisterEditSession(Session),
      'invalid-draft session did not register');
    Check(Session.TrySetRawDraft(opfTextHeight, '1e', ErrorText),
      'incomplete numeric input must remain available as a raw draft');
    Check(Session.HasUncommittedWork,
      'a changed raw draft must count as unsaved work');
    Check(not MainForm.TrySettleEditSession,
      'settlement must reject an incomplete numeric draft');
    PromptCountBefore := PromptCount;
    MainForm.UserEventExecute(MainForm.SaveDoc);
    Check(Session.IsActive and (PromptCount = PromptCountBefore),
      'Save must not publish or prompt while an incomplete draft is active');
    MainForm.FormCloseQuery(nil, CanClose);
    Check(not CanClose and Session.IsActive,
      'Close must remain blocked while an incomplete draft is active');
    Check((Drawing.GetObject(FirstID) = First) and (First.Height = 7),
      'an invalid raw draft must not mutate or retarget the object');
    Check(Session.Cancel(Epoch), 'invalid raw draft could not be canceled');
  finally
    MainForm.UnregisterEditSession(Session);
    Session.Free;
  end;

  Session := TObjectEditSession.Create(Drawing, Epoch, [FirstID]);
  try
    Check(MainForm.RegisterEditSession(Session),
      'composition session did not register');
    Patch.Fields := [opfTextHeight];
    Patch.TextHeight := 8;
    Check(Session.Preview(Patch, Epoch, ErrorText),
      'composition preview failed: ' + ErrorText);
    Session.SetCompositionPending(True);
    Check(not MainForm.TrySettleEditSession,
      'commit must wait until native text composition ends');
    Check((First.Height = 8) and Session.IsActive,
      'composition settlement must preserve its preview and live session');
    Session.SetCompositionPending(False);
    Check(Session.Cancel(Epoch),
      'composition preview could not be canceled after composition ended');
    Check(First.Height = 7,
      'composition cancellation did not restore the committed property');
  finally
    MainForm.UnregisterEditSession(Session);
    Session.Free;
  end;

  Session := TObjectEditSession.Create(Drawing, Epoch, [FirstID]);
  try
    Check(MainForm.RegisterEditSession(Session),
      'preview session did not register');
    Patch.Fields := [opfText];
    Patch.Text := EncodeEditTextForTpX(
      LCLTextToUTF8Bytes('Changed preview'));
    Patch.TeXText := '';
    Patch.FontName := '';
    Patch.BitmapLink := '';
    Check(Session.Preview(Patch, Epoch, ErrorText),
      'valid text preview failed: ' + ErrorText);
    Check(DecodeTpXTextForEdit(First.Text) = 'Changed preview',
      'the preview was not visible in the model');
    Check(First.TeXText = '\alpha',
      'editing Text must leave the independent TeXText field unchanged');
    Check(Drawing.History.Count = HistoryCount,
      'a reversible preview must not create a history operation');
    CheckEditSessionOrder(Drawing, First, Second,
      'preview must preserve object identity and order');
    Check(Session.Cancel(Epoch), 'preview could not be canceled');
    Check((First.Text = OriginalStoredText) and (First.TeXText = '\alpha'),
      'cancel must restore exact stored Text without changing TeXText');
    Check((Drawing.GetObject(FirstID) = First) and
      (Drawing.GetObject(SecondID) = Second),
      'cancel must preserve the original object instances and IDs');
    CheckEditSessionOrder(Drawing, First, Second,
      'cancel must preserve object order');
    Check((Drawing.History.Count = HistoryCount) and
      not Drawing.History.IsChanged,
      'cancel must create no completed history operation or dirty state');
  finally
    MainForm.UnregisterEditSession(Session);
    Session.Free;
  end;

  Session := TObjectEditSession.Create(Drawing, Epoch, []);
  try
    Check(MainForm.RegisterEditSession(Session),
      'provisional session did not register');
    Provisional := TText2D.CreateSpec(-1, Point2D(1, 1), 5, 'Temporary');
    ProvisionalID := Session.AddProvisional(Provisional);
    Check((ProvisionalID >= 0) and
      (Drawing.GetObject(ProvisionalID) = Provisional),
      'the session did not add and own its provisional object');
    Check(Session.Cancel(Epoch), 'provisional object could not be canceled');
    Check((Drawing.ObjectsCount = 2) and
      (Drawing.GetObject(FirstID) = First) and
      (Drawing.GetObject(SecondID) = Second),
      'canceling a provisional object must retain existing objects');
    CheckEditSessionOrder(Drawing, First, Second,
      'provisional cancellation must preserve object order');
  finally
    MainForm.UnregisterEditSession(Session);
    Session.Free;
  end;

  Session := TObjectEditSession.Create(Drawing, Epoch, [FirstID]);
  try
    Check(MainForm.RegisterEditSession(Session),
      'commit session did not register');
    Patch.Fields := [opfTextHeight];
    Patch.TextHeight := 9;
    Check(Session.Preview(Patch, Epoch, ErrorText),
      'valid numeric preview failed: ' + ErrorText);
    Check(Session.Commit(Epoch, ErrorText),
      'valid edit did not commit: ' + ErrorText);
    Check((Drawing.History.Count = HistoryCount + 1) and
      (Drawing.GetObject(FirstID) is TText2D),
      'a committed session must create one undoable history operation');
    Restored := Drawing.GetObject(FirstID) as TText2D;
    Check(Restored.Height = 9, 'commit did not retain the edited height');
    Drawing.History.Undo;
    Restored := Drawing.GetObject(FirstID) as TText2D;
    Check(Restored.Height = 7, 'undo did not restore the original height');
    Drawing.History.Redo;
    Restored := Drawing.GetObject(FirstID) as TText2D;
    Check(Restored.Height = 9, 'redo did not restore the committed height');
  finally
    MainForm.UnregisterEditSession(Session);
    Session.Free;
  end;
  Restored := Drawing.GetObject(FirstID) as TText2D;
  OriginalText := DecodeTpXTextForEdit(Restored.Text);
  Check(OriginalText = 'First', 'unrelated text changed during numeric edit');

  LineOne := TLine2D.CreateSpec(-1, Point2D(0, 0), Point2D(2, 1));
  LineOne.LineWidth := 1.125;
  LineOne.LineStyle := liDotted;
  LineOne.LineColor := clRed;
  LineTwo := TLine2D.CreateSpec(-1, Point2D(3, 0), Point2D(5, 1));
  LineTwo.LineWidth := 2.25;
  LineTwo.LineStyle := liDashed;
  LineTwo.LineColor := clBlue;
  Drawing.AddObject(-1, LineOne);
  Drawing.AddObject(-1, LineTwo);
  LineOneID := LineOne.ID;
  LineTwoID := LineTwo.ID;
  LineIDs[0] := LineOneID;
  LineIDs[1] := LineTwoID;
  HistoryCount := Drawing.History.Count;
  PropertyValue := ReadObjectProperty(Drawing, LineIDs, opfLineWidth);
  Check(PropertyValue.State = pvsMixed,
    'different supported line widths must read as mixed');
  Check(Drawing.History.Count = HistoryCount,
    'reading mixed values must not create a history operation');

  Session := TObjectEditSession.Create(Drawing, Epoch, LineIDs);
  try
    Check(MainForm.RegisterEditSession(Session),
      'multi-line session did not register');
    Patch.Fields := [opfLineWidth];
    Patch.LineWidth := 3.5;
    Check(Session.Preview(Patch, Epoch, ErrorText),
      'multi-line width preview failed: ' + ErrorText);
    Check((LineOne.LineColor = clRed) and (LineOne.LineStyle = liDotted) and
      (LineTwo.LineColor = clBlue) and (LineTwo.LineStyle = liDashed),
      'editing width must preserve each line color and style');
    Check(Drawing.History.Count = HistoryCount,
      'multi-target preview must not create an undo operation');
    Check(Session.Commit(Epoch, ErrorText),
      'multi-line width edit did not commit: ' + ErrorText);
    Check((LineOne.LineWidth = 3.5) and (LineTwo.LineWidth = 3.5) and
      (Drawing.History.Count = HistoryCount + 1),
      'one field edit must apply to all supported targets as one operation');
  finally
    MainForm.UnregisterEditSession(Session);
    Session.Free;
  end;

  Session := TObjectEditSession.Create(Drawing, Epoch,
    [FirstID, LineOneID]);
  try
    Check(MainForm.RegisterEditSession(Session),
      'invalid multi-target session did not register');
    Patch.Fields := [opfTextHeight];
    Patch.TextHeight := 20;
    PropertyValue := ReadObjectProperty(Drawing, [FirstID, LineOneID],
      opfTextHeight);
    Check(PropertyValue.State = pvsUnavailable,
      'a field unsupported by one target must read as unavailable');
    Restored := Drawing.GetObject(FirstID) as TText2D;
    Check(not Session.Preview(Patch, Epoch, ErrorText) and
      (Restored.Height = 9) and (LineOne.LineWidth = 3.5),
      'a mixed-capability patch must fail atomically without changing any target');
    Check(Session.Cancel(Epoch),
      'invalid multi-target session could not be canceled');
  finally
    MainForm.UnregisterEditSession(Session);
    Session.Free;
  end;
end;

procedure TestEditSessionRecoveryPayload;
var
  Drawing, Candidate: TDrawing2D;
  First, Second, RestoredFirst, RestoredSecond, Provisional: TText2D;
  Memo: TMemo;
  Session, RestoredSession, CorruptSession,
    CorruptedRestoredSession: TObjectEditSession;
  Patch: TObjectPropertyPatch;
  ModelBytes, AuxiliaryState, CorruptModel, CorruptAuxiliary: RawByteString;
  ExpectedUtf8, LclInputBytes: RawByteString;
  UnicodeEditText, DecodedEditText, DraftText, MemoRoundTrip: TEditUtf8Text;
  ErrorText, RecoveryRoot, RecoveryTempRoot: string;
  RecoveryRuntime: TAutoSaveRuntime; AssetContext: TRecoveryAssetContext;
  RecoveryPayload: RawByteString; UnresolvedLinks: LongWord;
  CanSaveBack: Boolean;
  CodecContext: TObject;
  Diagnostics: TStringList;
  TargetIDs: TEditTargetIDs;
  Epoch: QWord; MarkerIndex: Integer;
begin
  Drawing := MainForm.TheDrawing;
  First := TText2D.CreateSpec(-1, Point2D(4, 6), 7, 'Committed first');
  First.TeXText := '\\textbf{first}';
  Second := TText2D.CreateSpec(-1, Point2D(12, 6), 7, 'Committed sibling');
  Drawing.AddObject(-1, First);
  Drawing.AddObject(-1, Second);
  Drawing.History.Save;
  Drawing.History.SaveCheckSum;
  Session := TObjectEditSession.Create(Drawing,
    MainForm.CurrentDocumentEpoch, [First.ID]);
  Candidate := nil;
  CodecContext := nil;
  RestoredSession := nil;
  RecoveryRoot := IncludeTrailingPathDelimiter(GetTempDir) +
    'tpx-edit-session-payload-' + IntToStr(GetProcessID);
  RecoveryTempRoot := IncludeTrailingPathDelimiter(GetTempDir) +
    'tpx-edit-session-payload-tmp-' + IntToStr(GetProcessID);
  RecoveryRuntime := TAutoSaveRuntime.Create(RecoveryRoot);
  AssetContext := nil;
  Diagnostics := TStringList.Create;
  try
    Patch.Fields := [opfText];
    Patch.Text := EncodeEditTextForTpX(
      LCLTextToUTF8Bytes('Uncommitted first'));
    Patch.TeXText := '';
    Patch.FontName := '';
    Patch.BitmapLink := '';
    Check(Session.Preview(Patch, Session.DocumentEpoch, ErrorText),
      'recovery preview setup failed: ' + ErrorText);
    ExpectedUtf8 := RawByteString(#$CE#$B2#$F0#$9F#$98#$80'&#946; &');
    SetCodePage(ExpectedUtf8, 65001, False);
    LclInputBytes := ExpectedUtf8;
    SetCodePage(LclInputBytes, 1252, False);
    Memo := TMemo.Create(MainForm);
    try
      Memo.Parent := MainForm;
      Memo.SetBounds(0, 0, 1, 1);
      Memo.Visible := True;
      Check(PtrUInt(Memo.Handle) <> 0,
        'the UTF-8 input oracle did not create a native TMemo handle');
      Application.ProcessMessages;
      Memo.Text := string(LclInputBytes);
      UnicodeEditText := LCLTextToUTF8Bytes(Memo.Text);
      Check(Session.TrySetRawDraft(opfTeXText,
        LCLTextToUTF8Bytes(Memo.Text), ErrorText),
        'raw Unicode draft must preserve the native TMemo.Text bytes: ' +
        ErrorText);
      Memo.Visible := False;
    finally
      Memo.Free;
    end;
    Check((Length(UnicodeEditText) = Length(ExpectedUtf8)) and
      CompareMem(@UnicodeEditText[1], @ExpectedUtf8[1],
        Length(ExpectedUtf8)),
      'the LCL UTF-8 adapter must preserve bytes tagged with the ANSI code page');
    Patch.Text := EncodeEditTextForTpX(UnicodeEditText);
    Check(Session.Preview(Patch, Session.DocumentEpoch, ErrorText),
      'Unicode text preview setup failed: ' + ErrorText);
    DecodedEditText := DecodeTpXTextForEdit(Patch.Text);
    Check((Length(DecodedEditText) = Length(ExpectedUtf8)) and
      CompareMem(@DecodedEditText[1], @ExpectedUtf8[1], Length(ExpectedUtf8)),
      'TpX text encoding must preserve Unicode and literal entity-like text');
    Check(DecodeTpXTextForEdit('&#x1F600;') =
      LCLTextToUTF8Bytes('😀'),
      'hexadecimal supplementary references must decode as UTF-16 pairs');
    Check(Session.TrySetRawDraft(opfTextHeight, '1e', ErrorText),
      'raw numeric draft setup failed: ' + ErrorText);
    Session.Presentation := espInlineText;
    Session.SetCompositionPending(True);
    Check(not Session.BuildRecoveryData('', CorruptModel, CorruptAuxiliary,
      ErrorText) and (Pos('composition', LowerCase(ErrorText)) > 0),
      'recovery must defer while native composition contains uncommitted text');
    Session.SetCompositionPending(False);
    Check(Session.BuildRecoveryData('', ModelBytes, AuxiliaryState,
      ErrorText), 'could not build committed-scene recovery data: ' + ErrorText);
    Check((Length(ModelBytes) > 0) and (Length(AuxiliaryState) > 0),
      'recovery must carry a committed scene and a separate edit sidecar');
    Check((First.Text = Patch.Text) and (First.TeXText = '\\textbf{first}'),
      'recovery serialization must restore the live preview in place');
    ForceDirectories(RecoveryTempRoot);
    RecoveryPayload := RecoveryRuntime.BuildTpXRecoveryPayload(Drawing, '',
      UnresolvedLinks, Session);
    RecoveryRuntime.UnpackTpXRecoveryPayload(RecoveryPayload,
      TDocumentPath(UTF8String(RecoveryTempRoot)), ModelBytes,
      AuxiliaryState, AssetContext, UnresolvedLinks);
    Check((Length(ModelBytes) > 0) and (Length(AuxiliaryState) > 0),
      'the versioned AutoSave payload must preserve the separate edit sidecar');

    Candidate := LoadDocumentCandidate('tpx', 'recovered.tpx', ModelBytes,
      CanSaveBack, CodecContext, Diagnostics);
    Check(Candidate <> nil, 'the committed recovery model did not parse');
    RestoredFirst := Candidate.ObjectList.FirstObj as TText2D;
    RestoredSecond := Candidate.ObjectList.NextObj as TText2D;
    Check((Candidate.ObjectsCount = 2) and
      (RestoredFirst.Text = 'Committed first') and
      (RestoredSecond.Text = 'Committed sibling'),
      'the saved scene must contain committed labels, not preview text');
    Epoch := Session.DocumentEpoch + 1;
    Check(TObjectEditSession.RestoreRecoveryState(Candidate, Epoch,
      AuxiliaryState, RestoredSession, ErrorText),
      'the edit sidecar did not rebind to the recovered scene: ' + ErrorText);
    Check(RestoredSession.Presentation = espInlineText,
      'the edit sidecar did not preserve its owning presentation');
    TargetIDs := RestoredSession.CopyTargetIDs;
    Check(Length(TargetIDs) = 1,
      'the sidecar did not retain its one-object target');
    Check(TargetIDs[0] = RestoredFirst.ID,
      'structural recovery path selected the wrong object');
    RestoredFirst := Candidate.GetObject(TargetIDs[0]) as TText2D;
    DecodedEditText := DecodeTpXTextForEdit(RestoredFirst.Text);
    Check((Length(DecodedEditText) = Length(ExpectedUtf8)) and
      CompareMem(@DecodedEditText[1], @ExpectedUtf8[1],
        Length(ExpectedUtf8)) and
      (RestoredSecond.Text = 'Committed sibling'),
      'restoring a preview must bind to its target and leave the same-class sibling unchanged');
    Check(RestoredSession.HasRawDraft(opfTextHeight, DraftText) and
      (DraftText = '1e') and RestoredSession.HasRawDraftPending,
      'recovery must preserve the incomplete numeric draft as raw input');
    Check(RestoredSession.HasRawDraft(opfTeXText, DraftText) and
      (StringCodePage(DraftText) = 65001) and
      (Length(DraftText) = Length(ExpectedUtf8)) and
      CompareMem(@DraftText[1], @ExpectedUtf8[1], Length(ExpectedUtf8)),
      'recovery must preserve independent UTF-8 bytes and literal entity-like text');
    Memo := TMemo.Create(MainForm);
    try
      Memo.Parent := MainForm;
      Memo.SetBounds(0, 0, 1, 1);
      Memo.Visible := True;
      Check(PtrUInt(Memo.Handle) <> 0,
        'the restored UTF-8 oracle did not create a native TMemo handle');
      Application.ProcessMessages;
      Memo.Text := RawByteString(DraftText);
      MemoRoundTrip := LCLTextToUTF8Bytes(Memo.Text);
      Check((Length(MemoRoundTrip) = Length(ExpectedUtf8)) and
        CompareMem(@MemoRoundTrip[1], @ExpectedUtf8[1], Length(ExpectedUtf8)),
        'restoring a raw edit into TMemo must preserve Unicode UTF-8 bytes');
      Memo.Visible := False;
    finally
      Memo.Free;
    end;
    Check(EncodeEditTextForTpX(UnicodeEditText) =
      '&#946;&#55357;&#56832;&#38;#946; &#38;',
      'TpX text encoding must preserve Unicode and escape literal ampersands');
    Check(RestoredSession.Cancel(Epoch),
      'recovered edit could not be canceled');
    Check((RestoredFirst.Text = 'Committed first') and
      (RestoredSecond.Text = 'Committed sibling'),
      'canceling a recovered edit must restore the recovered committed scene');

    CorruptSession := TObjectEditSession.Create(Candidate, Epoch, []);
    try
      Provisional := TText2D.CreateSpec(-1,
        Point2D(1, 1), 5, 'RecoveryUniqueSentinel');
      CorruptSession.AddProvisional(Provisional);
      Check(CorruptSession.BuildRecoveryData('', CorruptModel,
        CorruptAuxiliary, ErrorText),
        'Could not build the provisional-object corruption fixture: ' +
          ErrorText);
      SetFirstProvisionalPointCount(CorruptAuxiliary, High(Longint));
      CorruptedRestoredSession := nil;
      Check(not TObjectEditSession.RestoreRecoveryState(Candidate, Epoch,
        CorruptAuxiliary, CorruptedRestoredSession, ErrorText),
        'A provisional point count exceeding its object stream must be rejected');
      Check((CorruptedRestoredSession = nil) and
        (Candidate.ObjectsCount = 3) and
        (Candidate.GetObject(Provisional.ID) = Provisional) and
        (RestoredFirst.Text = 'Committed first') and
        (RestoredSecond.Text = 'Committed sibling'),
        'A corrupt provisional point count changed the committed drawing');
      Check(CorruptSession.BuildRecoveryData('', CorruptModel,
        CorruptAuxiliary, ErrorText),
        'Could not reset the provisional-object corruption fixture: ' +
          ErrorText);
      MarkerIndex := Pos('RecoveryUniqueSentinel', string(CorruptAuxiliary));
      Check(MarkerIndex > 4,
        'The provisional-object fixture did not contain its unique text');
      CorruptAuxiliary[MarkerIndex - 4] := #$FF;
      CorruptAuxiliary[MarkerIndex - 3] := #$FF;
      CorruptAuxiliary[MarkerIndex - 2] := #$FF;
      CorruptAuxiliary[MarkerIndex - 1] := #$7F;
      CorruptedRestoredSession := nil;
      Check(not TObjectEditSession.RestoreRecoveryState(Candidate, Epoch,
        CorruptAuxiliary, CorruptedRestoredSession, ErrorText),
        'A corrupted provisional text length must be rejected');
      Check((CorruptedRestoredSession = nil) and
        (Candidate.ObjectsCount = 3) and
        (Candidate.GetObject(Provisional.ID) = Provisional) and
        (RestoredFirst.Text = 'Committed first') and
        (RestoredSecond.Text = 'Committed sibling'),
        Format('A corrupt provisional payload changed the committed drawing '
          + '(session nil=%s, count=%d, first=%s, sibling=%s)',
          [BoolToStr(CorruptedRestoredSession = nil, True),
           Candidate.ObjectsCount, RestoredFirst.Text, RestoredSecond.Text]));
      CorruptedRestoredSession.Free;
    finally
      if CorruptSession.IsActive then CorruptSession.Cancel(Epoch);
      CorruptSession.Free;
    end;
  finally
    if (RestoredSession <> nil) and RestoredSession.IsActive then
      RestoredSession.Cancel(Epoch);
    RestoredSession.Free;
    Session.Cancel(Session.DocumentEpoch);
    Session.Free;
    Candidate.Free;
    AssetContext.Free;
    RecoveryRuntime.Free;
    CodecContext.Free;
    Diagnostics.Free;
    RemoveDir(RecoveryRoot);
    RemoveDir(RecoveryTempRoot);
  end;
end;

function EditSessionRecoveryAuxiliaryLength(Runtime: TAutoSaveRuntime;
  const DocumentKey, TempRoot: string): Integer;
var Files: TStringList; Entry: TAutoSaveStoredRecord;
  ModelBytes, AuxiliaryState: RawByteString; AssetContext: TRecoveryAssetContext;
  UnresolvedLinks: LongWord; I: Integer;
begin
  Result := -1;
  Files := TStringList.Create;
  try
    Runtime.Store.ListDrafts(Files);
    for I := 0 to Files.Count - 1 do
      if Runtime.Store.LoadRecord(Files[I], Entry) and
        (Entry.DocumentKey = DocumentKey) then
      begin
        AssetContext := nil;
        try
          Runtime.UnpackTpXRecoveryPayload(Entry.Payload,
            TDocumentPath(UTF8String(TempRoot)), ModelBytes,
            AuxiliaryState, AssetContext, UnresolvedLinks);
          Result := Length(AuxiliaryState);
        finally
          AssetContext.Free;
        end;
        Exit;
      end;
  finally
    Files.Free;
  end;
end;

procedure TestEditSessionAutoSaveCancel;
const
  InitialSource: RawByteString =
    '%<TpX v="5" TeXFormat="none" PdfTeXFormat="none">' + LineEnding +
    '%<line x1="0" y1="0" x2="20" y2="10"/>' + LineEnding +
    '%</TpX>' + LineEnding;
var FileName, ErrorText, TempRoot: string; Line: TLine2D;
  Session: TObjectEditSession; StoreRuntime: TAutoSaveRuntime;
  ErrorText2: string; Deadline: QWord; AuxiliaryLength: Integer;
begin
  FileName := IncludeTrailingPathDelimiter(GetTempDir) +
    'tpx-edit-session-autosave-' + IntToStr(GetProcessID) + '.tpx';
  TempRoot := IncludeTrailingPathDelimiter(GetTempDir) +
    'tpx-edit-session-recovery-' + IntToStr(GetProcessID);
  WriteReloadSource(FileName, InitialSource);
  MainForm.Show;
  Application.ProcessMessages;
  AutoRefreshEnabled := True;
  MainForm.ApplyAutoReloadSettings;
  Check(BaseMode.OpenDocumentFromPath(FileName, ErrorText),
    'Could not open the AutoSave edit-session fixture: ' + ErrorText);
  MainForm.ApplyAutoSavePreference(True);
  Line := MainForm.TheDrawing.GetObject(0) as TLine2D;
  Line.Points[1] := Point2D(30, 10);
  MainForm.TheDrawing.History.Save;
  MainForm.NotifyDocumentEdited;
  Check(MainForm.TheDrawing.History.IsChanged,
    'AutoSave fixture must start with a committed dirty drawing');

  Session := TObjectEditSession.Create(MainForm.TheDrawing,
    MainForm.CurrentDocumentEpoch, [Line.ID]);
  StoreRuntime := TAutoSaveRuntime.Create(AutoSaveRecoveryRoot);
  try
    Check(MainForm.RegisterEditSession(Session),
      'AutoSave edit session did not register');
    Check(Session.TrySetRawDraft(opfLineWidth, '1e', ErrorText2),
      'Could not retain an incomplete raw property draft: ' + ErrorText2);
    Session.SetCompositionPending(True);
    ForceDirectories(TempRoot);
    Deadline := GetTickCount64 + 2000;
    AuxiliaryLength := -1;
    repeat
      Application.ProcessMessages;
      AuxiliaryLength := EditSessionRecoveryAuxiliaryLength(StoreRuntime,
        MainForm.CurrentAutoSaveKey, TempRoot);
      if AuxiliaryLength > 0 then Break;
      Sleep(10);
    until GetTickCount64 >= Deadline;
    Check(AuxiliaryLength < 0,
      'recovery must keep the last safe checkpoint during native composition');
    Session.SetCompositionPending(False);
    Deadline := GetTickCount64 + 2500;
    repeat
      Application.ProcessMessages;
      AuxiliaryLength := EditSessionRecoveryAuxiliaryLength(StoreRuntime,
        MainForm.CurrentAutoSaveKey, TempRoot);
      if AuxiliaryLength > 0 then Break;
      Sleep(10);
    until GetTickCount64 >= Deadline;
    Check(AuxiliaryLength > 0,
      'composition end did not rearm recovery for the active raw draft');
    Check(ReadDocumentSnapshot(FileName).SourceBytes = InitialSource,
      'AutoSave published a source scene while its raw edit session was active');

    Check(Session.Cancel(MainForm.CurrentDocumentEpoch),
      'The AutoSave raw-draft session could not be canceled');
    MainForm.UnregisterEditSession(Session);
    AuxiliaryLength := EditSessionRecoveryAuxiliaryLength(StoreRuntime,
      MainForm.CurrentAutoSaveKey, TempRoot);
    Check(AuxiliaryLength = 0,
      'Cancel left the stale raw draft recoverable after its immediate replacement');

    WaitForAutoSaveClean(FileName);
    Check(Abs(SavedSourceLineX2(FileName) - 30) < 0.0001,
      'Cancel did not resume AutoSave for the original committed dirty scene');
  finally
    if Session.IsActive then MainForm.UnregisterEditSession(Session);
    Session.Free;
    StoreRuntime.Free;
    MainForm.ApplyAutoSavePreference(False);
    DeleteFile(FileName);
    if DirectoryExists(TempRoot) then RemoveDir(TempRoot);
  end;
end;

procedure TestEditSessionRecoveryDisabled;
const
  InitialSource: RawByteString =
    '%<TpX v="5" TeXFormat="none" PdfTeXFormat="none">' + LineEnding +
    '%</TpX>' + LineEnding;
var FileName, RecoveryRoot, ErrorText: string; Drawing: TDrawing2D;
  TextObject: TText2D; Session: TObjectEditSession;
  Document: TDocumentSession; Runtime: TAutoSaveRuntime;
  Patch: TObjectPropertyPatch; Payload: RawByteString;
  UnresolvedLinks: LongWord; AuxiliaryLength: Integer;
begin
  FileName := IncludeTrailingPathDelimiter(GetTempDir) +
    'tpx-edit-session-recovery-off-' + IntToStr(GetProcessID) + '.tpx';
  RecoveryRoot := IncludeTrailingPathDelimiter(GetTempDir) +
    'tpx-edit-session-recovery-off-store-' + IntToStr(GetProcessID);
  WriteReloadSource(FileName, InitialSource);
  Drawing := MainForm.TheDrawing;
  TextObject := TText2D.CreateSpec(-1, Point2D(2, 3), 7, 'Recovery off');
  Drawing.AddObject(-1, TextObject);
  Drawing.History.Save;
  Drawing.History.SaveCheckSum;
  Document := TDocumentSession.Create;
  Runtime := TAutoSaveRuntime.Create(RecoveryRoot);
  Session := nil;
  try
    Document.AcceptSource(FileName, 'tpx', InitialSource,
      ReadDocumentRevision(FileName), True);
    Runtime.BindDocument(Document, Drawing, False, False, False,
      GetTickCount64, 'edit-session-recovery-off');
    Session := TObjectEditSession.Create(Drawing, 1, [TextObject.ID]);
    Patch.Fields := [opfTextHeight];
    Patch.TextHeight := 8;
    Check(Session.Preview(Patch, 1, ErrorText) and
      Session.Commit(1, ErrorText),
      'first property edit could not commit with recovery disabled: ' +
        ErrorText);
    Runtime.UpdateRecoveryAfterEditSessionEnd(Document, Drawing, True);
    AuxiliaryLength := EditSessionRecoveryAuxiliaryLength(Runtime,
      Runtime.DocumentKey, RecoveryRoot);
    Check(AuxiliaryLength < 0,
      'recovery disabled must not create its first draft after a property edit');
    Session.Free;
    Session := nil;

    Runtime.SetRecoveryEnabled(True, GetTickCount64);
    Session := TObjectEditSession.Create(Drawing, 1, [TextObject.ID]);
    Check(Session.TrySetRawDraft(opfTeXText,
      LCLTextToUTF8Bytes('retire this raw edit'), ErrorText),
      'could not create a raw draft for the recovery preference test');
    Payload := Runtime.BuildTpXRecoveryPayload(Drawing, Document.SourcePath,
      UnresolvedLinks, Session);
    Runtime.Store.SaveDraft(Runtime.DocumentKey, Document.SourcePath, 'tpx',
      'tpx-assets', Document.AcceptedRevision, Document.OriginalSourceBytes,
      Document.LocalRevision, Payload);
    Check(EditSessionRecoveryAuxiliaryLength(Runtime, Runtime.DocumentKey,
      RecoveryRoot) > 0,
      'the enabled-recovery fixture did not store its active raw draft');
    Runtime.SetRecoveryEnabled(False, GetTickCount64);
    Check(Session.Cancel(1), 'the raw edit for the recovery preference test failed to cancel');
    Runtime.UpdateRecoveryAfterEditSessionEnd(Document, Drawing, True);
    Check(EditSessionRecoveryAuxiliaryLength(Runtime, Runtime.DocumentKey,
      RecoveryRoot) = 0,
      'disabling recovery must retire an existing ended raw draft');
  finally
    if (Session <> nil) then
    begin
      if Session.IsActive then Session.Cancel(1);
      Session.Free;
    end;
    Runtime.Store.DeleteDraft(Runtime.DocumentKey);
    Runtime.Free;
    Document.Free;
    DeleteFile(FileName);
    RemoveDir(RecoveryRoot);
  end;
end;

procedure TestEditSessionDeferredExternalReload;
const
  InitialSource: RawByteString =
    '%<TpX v="5" TeXFormat="none" PdfTeXFormat="none">' + LineEnding +
    '%<line x1="0" y1="0" x2="20" y2="10"/>' + LineEnding +
    '%</TpX>' + LineEnding;
  ExternalSource: RawByteString =
    '%<TpX v="5" TeXFormat="none" PdfTeXFormat="none">' + LineEnding +
    '%<line x1="0" y1="0" x2="45" y2="10"/>' + LineEnding +
    '%</TpX>' + LineEnding;
var FileName, ErrorText: string; Line: TLine2D;
  Session: TObjectEditSession; Patch: TObjectPropertyPatch;
  Deadline: QWord; ErrorText2: string;
begin
  FileName := IncludeTrailingPathDelimiter(GetTempDir) +
    'tpx-edit-session-reload-' + IntToStr(GetProcessID) + '.tpx';
  WriteReloadSource(FileName, InitialSource);
  MainForm.Show;
  Application.ProcessMessages;
  AutoRefreshEnabled := True;
  MainForm.ApplyAutoReloadSettings;
  LoadReloadFixture(FileName);
  Line := MainForm.TheDrawing.GetObject(0) as TLine2D;
  Session := TObjectEditSession.Create(MainForm.TheDrawing,
    MainForm.CurrentDocumentEpoch, [Line.ID]);
  try
    Check(MainForm.RegisterEditSession(Session),
      'Reload edit session did not register');
    Patch.Fields := [opfLineWidth];
    Patch.LineWidth := 2;
    Check(Session.Preview(Patch, MainForm.CurrentDocumentEpoch, ErrorText2),
      'Reload-blocking preview failed: ' + ErrorText2);
    WriteReloadSource(FileName, ExternalSource);
    Deadline := GetTickCount64 + 6000;
    repeat
      Application.ProcessMessages;
      MainForm.ProcessPendingReload(GetTickCount64 + 1000);
      if MainForm.AutoReloadState.SettlePending then Break;
      Sleep(10);
    until GetTickCount64 >= Deadline;
    Check(MainForm.AutoReloadState.SettlePending and
      (Abs(ReloadLineX2 - 20) < 0.0001),
      'An external replacement changed the drawing during a live edit session');
    Check(Session.Cancel(MainForm.CurrentDocumentEpoch),
      'The reload-blocking preview could not be canceled');
    MainForm.UnregisterEditSession(Session);
    Deadline := GetTickCount64 + 6000;
    repeat
      Application.ProcessMessages;
      MainForm.ProcessPendingReload(GetTickCount64 + 1000);
      if Abs(ReloadLineX2 - 45) < 0.0001 then Break;
      Sleep(10);
    until GetTickCount64 >= Deadline;
    Check(Abs(ReloadLineX2 - 45) < 0.0001,
      'A source reload deferred by an edit session was not applied after cancel');
  finally
    if Session.IsActive then MainForm.UnregisterEditSession(Session);
    Session.Free;
    DeleteFile(FileName);
  end;
end;
