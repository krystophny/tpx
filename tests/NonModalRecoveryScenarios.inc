type
  TNonModalRecoveryObserver = class
  public
    Previous: TRecoveryEditSessionRestoredEvent;
    RestoredSession: TObjectEditSession;
    Calls: Integer;
    CallbackError: string;
    procedure RecoveryRestored(Sender: TObject;
      EditSession: TObjectEditSession);
end;

procedure TNonModalRecoveryObserver.RecoveryRestored(Sender: TObject;
  EditSession: TObjectEditSession);
begin
  Inc(Calls);
  try
    if Assigned(Previous) then Previous(Sender, EditSession);
  except
    on E: Exception do CallbackError := E.Message;
  end;
  RestoredSession := EditSession;
end;

function FindTextByLabel(Objects: TGraphicObjList;
  const LabelText: string): TText2D;
var Obj: TGraphicObject;
begin
  Result := nil;
  if Objects = nil then Exit;
  Obj := Objects.FirstObj;
  while Obj <> nil do
  begin
    if Obj is TGroup2D then
      Result := FindTextByLabel((Obj as TGroup2D).Objects, LabelText)
    else if (Obj is TText2D) and
      (UTF8ToSys(DecodeTpXTextForEdit((Obj as TText2D).Text)) = LabelText) then
      Result := Obj as TText2D;
    if Result <> nil then Exit;
    Obj := Objects.NextObj;
  end;
end;

function TextGroupDepth(Objects: TGraphicObjList;
  const LabelText: string): Integer;
var Obj: TGraphicObject; ChildDepth: Integer;
begin
  Result := -1;
  if Objects = nil then Exit;
  Obj := Objects.FirstObj;
  while Obj <> nil do
  begin
    if (Obj is TText2D) and
      (DecodeTpXTextForEdit((Obj as TText2D).Text) = LabelText) then
      Exit(0);
    if Obj is TGroup2D then
    begin
      ChildDepth := TextGroupDepth((Obj as TGroup2D).Objects, LabelText);
      if ChildDepth >= 0 then Exit(ChildDepth + 1);
    end;
    Obj := Objects.NextObj;
  end;
end;

procedure TestNonModalRecoveryCreate;
var SourcePath, ErrorText, ReadyPath: string;
  OldRuntimeID: Integer; Target: TText2D; Memo: TCompositionAwareMemo;
  Store: TAutoSaveStore; Files: TStringList; Entry: TAutoSaveStoredRecord;
  Snapshot: TDocumentSnapshot; Deadline: QWord; ReadyFile: TFileStream;
begin
  SourcePath := GetEnvironmentVariable('TPX_NONMODAL_RECOVERY_SOURCE');
  Check(SourcePath <> '', 'Non-modal recovery source path is missing');
  OldRuntimeID := StrToInt(GetEnvironmentVariable(
    'TPX_NONMODAL_RECOVERY_OLD_ID'));
  Snapshot := ReadDocumentSnapshot(SourcePath);
  MainForm.Show;
  Check(BaseMode.OpenDocumentFromPath(SourcePath, ErrorText),
    'Could not open nested recovery fixture: ' + ErrorText);
  Check((MainForm.TheDrawing.ObjectsCount = 2) and
    (FindTextByLabel(MainForm.TheDrawing.ObjectList, 'Fallback α') <> nil) and
    (FindTextByLabel(MainForm.TheDrawing.ObjectList, 'Other label Ω') <> nil),
    'Nested fixture did not load both distinguishable same-class labels');
  Check((TextGroupDepth(MainForm.TheDrawing.ObjectList, 'Fallback α') = 2) and
    (TextGroupDepth(MainForm.TheDrawing.ObjectList, 'Other label Ω') = 0),
    'The text targets did not load at their expected nested/top-level paths');
  Target := FindTextByLabel(MainForm.TheDrawing.ObjectList, 'Fallback α');
  Check(Target.ID <> OldRuntimeID,
    'The recovery fixture started with the reserved seed runtime ID');
  Target.ID := OldRuntimeID;
  MainForm.ApplyAutoSavePreference(True);
  Check(MainForm.BeginInlineTextEdit(Target),
    'Could not begin an inline edit on the nested label');
  Memo := MainForm.FindComponent('InlineTextMemo') as TCompositionAwareMemo;
  Check((Memo <> nil) and Memo.Visible,
    'Inline text editing did not create a visible canvas memo');
  Memo.Text := '\gamma_{j}';
  Check(MainForm.HasActiveEditSession,
    'Typing the draft unexpectedly ended the edit session');
  Check(Target.TeXText = '\beta_{i}',
    'Typing the draft changed the committed TeX source');
  Check(UTF8ToSys(DecodeTpXTextForEdit(Target.Text)) = 'Fallback α',
    'Typing the draft changed the committed plain-text label: ' +
    UTF8ToSys(DecodeTpXTextForEdit(Target.Text)));
  Store := TAutoSaveStore.Create(AutoSaveRecoveryRoot);
  Files := TStringList.Create;
  try
    Deadline := GetTickCount64 + 8000;
    repeat
      Application.ProcessMessages;
      Store.ListDrafts(Files);
      if Files.Count > 0 then Break;
      Sleep(10);
    until GetTickCount64 >= Deadline;
    Check((Files.Count = 1) and Store.LoadRecord(Files[0], Entry),
      'AutoSave did not persist exactly one pending text-edit recovery draft');
    Check((Entry.SourcePath = SourcePath) and (Entry.Payload <> '') and
      (Entry.BaseSourceBytes = Snapshot.SourceBytes),
      'Recovery draft lost its named source or accepted source bytes');
    Check((Entry.PayloadFormatId = 'tpx-assets') and
      (ReadDocumentSnapshot(SourcePath).SourceBytes = Snapshot.SourceBytes),
      'Recovery wrote the pending text draft into the source file');
    Check(MainForm.HasActiveEditSession and Memo.Visible and
      (Memo.EditText = '\gamma_{j}'),
      'The native inline buffer changed before process restart');
  finally
    Files.Free;
    Store.Free;
  end;
  { Keep the process alive so the driver can kill it without clean shutdown. }
  ReadyPath := GetEnvironmentVariable('TPX_NONMODAL_RECOVERY_READY_FILE');
  Check(ReadyPath <> '', 'Non-modal recovery ready-file path is missing');
  ReadyFile := TFileStream.Create(ReadyPath, fmCreate);
  ReadyFile.Free;
  while True do Sleep(1000);
end;

procedure TestNonModalRecoveryRestore;
var Driver: TRecoveryDialogDriver;
  Observer: TNonModalRecoveryObserver;
  DraftText: TEditUtf8Text; OldRuntimeID: Integer;
  Deadline: QWord; Target, Sibling: TText2D; IDs: TEditTargetIDs;
  Session: TObjectEditSession; Memo: TCompositionAwareMemo;
  LabelControl: TLabel;
  DoneButton: TButton; SavedRot, SavedSiblingRot: TRealType;
  Store: TAutoSaveStore; Files: TStringList; Entry: TAutoSaveStoredRecord;
  ActiveCaption, StatusSummary, SourcePath, ErrorText: string; I: Integer;
begin
  OldRuntimeID := StrToInt(GetEnvironmentVariable(
    'TPX_NONMODAL_RECOVERY_OLD_ID'));
  Observer := TNonModalRecoveryObserver.Create;
  Driver := TRecoveryDialogDriver.Create;
  Store := TAutoSaveStore.Create(AutoSaveRecoveryRoot);
  Files := TStringList.Create;
  try
    Store.ListDrafts(Files);
    Check((Files.Count = 1) and Store.LoadRecord(Files[0], Entry) and
      (Entry.PayloadFormatId = 'tpx-assets'),
      'The second process cannot read the persisted inline recovery payload');
    SourcePath := GetEnvironmentVariable('TPX_NONMODAL_RECOVERY_SOURCE');
    Check(BaseMode.OpenDocumentFromPath(SourcePath, ErrorText),
      'Could not load the live recovery fixture: ' + ErrorText);
  finally
    Files.Free;
    Store.Free;
  end;
  Driver.Timer := TTimer.Create(nil);
  Driver.Timer.Interval := 40;
  Driver.Timer.OnTimer := Driver.FindAndRecover;
  Driver.Timer.Enabled := True;
  Observer.Previous := MainForm.OnRecoveryEditSessionRestored;
  MainForm.OnRecoveryEditSessionRestored := Observer.RecoveryRestored;
  try
    MainForm.Show;
    Deadline := GetTickCount64 + 10000;
    repeat
      Application.ProcessMessages;
      if Driver.Clicked and (Observer.RestoredSession <> nil) then Break;
      Sleep(10);
    until GetTickCount64 >= Deadline;
    Check(Driver.Clicked,
      'Startup did not offer the persisted inline recovery draft');
    ActiveCaption := '';
    if Screen.ActiveForm <> nil then ActiveCaption := Screen.ActiveForm.Caption;
    StatusSummary := '';
    for I := 0 to MainForm.StatusBar1.Panels.Count - 1 do
      StatusSummary := StatusSummary + '[' +
        MainForm.StatusBar1.Panels[I].Text + ']';
    Check(Observer.RestoredSession <> nil,
      'Recovering the draft did not dispatch the restored edit session: ' +
      'calls=' + IntToStr(Observer.Calls) + ' callback=' +
      Observer.CallbackError + ' inline=' +
      BoolToStr(MainForm.InlineTextEditActive, True) + ' status=' +
      StatusSummary + ' activeform=' + ActiveCaption + ' objects=' +
      IntToStr(MainForm.TheDrawing.ObjectsCount));
    Session := Observer.RestoredSession;
    Check(MainForm.HasActiveEditSession and Session.IsActive and
      (Session.Presentation = espInlineText),
      'The restored inline edit is not registered with the main form');
    Target := FindTextByLabel(MainForm.TheDrawing.ObjectList, 'Fallback α');
    Sibling := FindTextByLabel(MainForm.TheDrawing.ObjectList, 'Other label Ω');
    Check((MainForm.TheDrawing.ObjectsCount = 2) and
      (Target <> nil) and (Sibling <> nil) and (Target <> Sibling) and
      (TextGroupDepth(MainForm.TheDrawing.ObjectList, 'Fallback α') = 2) and
      (TextGroupDepth(MainForm.TheDrawing.ObjectList, 'Other label Ω') = 0),
      'Recovery did not restore both labels in the committed scene');
    Check((UTF8ToSys(DecodeTpXTextForEdit(Target.Text)) = 'Fallback α') and
      (Target.TeXText = '\beta_{i}') and
      (Target.Points[0].X = 4.125) and (Target.Points[0].Y = 9.5) and
      (Target.Height = 2.25) and (Ord(Target.HAlignment) = 1) and
      (Abs(Target.Rot - 0.296705972839) < 0.000001),
      'The committed nested label changed while restoring its raw draft');
    Check((UTF8ToSys(DecodeTpXTextForEdit(Sibling.Text)) = 'Other label Ω') and
      (Sibling.TeXText = '') and (Sibling.Points[0].X = 14.25) and
      (Sibling.Points[0].Y = 8.75) and (Sibling.Height = 1.75) and
      (Abs(Sibling.Rot + 0.157079632679) < 0.000001),
      'The same-class sibling changed during target rebinding');
    IDs := Session.CopyTargetIDs;
    Check((Length(IDs) = 1) and (IDs[0] <> OldRuntimeID) and
      (Session.DocumentEpoch = MainForm.CurrentDocumentEpoch) and
      (FindTextByLabel(MainForm.TheDrawing.ObjectList, 'Fallback α').ID = IDs[0]),
      'The recovered session did not bind by nested structure to a fresh runtime ID');
    Check(Session.HasRawDraft(opfTeXText, DraftText) and
      Session.HasRawDraftPending and
      (DraftText = TEditUtf8Text('\gamma_{j}')),
      'Recovery lost the incomplete TeX source buffer');
    Memo := MainForm.FindComponent('InlineTextMemo') as TCompositionAwareMemo;
    LabelControl := MainForm.FindComponent('InlineTextFieldLabel') as TLabel;
    DoneButton := MainForm.FindComponent('InlineTextDoneButton') as TButton;
    Check((Memo <> nil) and Memo.Visible and
      (Memo.EditText = '\gamma_{j}') and (LabelControl <> nil) and
      (LabelControl.Caption = 'TeX source') and (DoneButton <> nil),
      'The recovered draft is not visible in the on-canvas TeX editor');
    SavedRot := Target.Rot;
    SavedSiblingRot := Sibling.Rot;
    DoneButton.Click;
    Target := FindTextByLabel(MainForm.TheDrawing.ObjectList, 'Fallback α');
    Sibling := FindTextByLabel(MainForm.TheDrawing.ObjectList, 'Other label Ω');
    Check((Target <> nil) and (Target.TeXText = '\gamma_{j}') and
      (UTF8ToSys(DecodeTpXTextForEdit(Target.Text)) = 'Fallback α') and
      (Target.Points[0].X = 4.125) and (Target.Points[0].Y = 9.5) and
      (Target.Height = 2.25) and (Ord(Target.HAlignment) = 1) and
      (Abs(Target.Rot - SavedRot) < 0.000001) and
      (Sibling <> nil) and (Sibling.TeXText = '') and
      (Sibling.Points[0].X = 14.25) and (Sibling.Points[0].Y = 8.75) and
      (Sibling.Height = 1.75) and
      (Abs(Sibling.Rot - SavedSiblingRot) < 0.000001),
      'Completing the recovered draft changed the wrong object or its anchor style');
    Check(MainForm.TheDrawing.History.CanUndo,
      'Completing the recovered draft did not create an undoable edit');
    MainForm.Undo.Execute;
    Target := FindTextByLabel(MainForm.TheDrawing.ObjectList, 'Fallback α');
    Sibling := FindTextByLabel(MainForm.TheDrawing.ObjectList, 'Other label Ω');
    Check((Target <> nil) and (Target.TeXText = '\beta_{i}') and
      (UTF8ToSys(DecodeTpXTextForEdit(Target.Text)) = 'Fallback α') and
      (Sibling <> nil) and (Sibling.TeXText = ''),
      'Undo after restart did not restore the committed scene and sibling');
    Check(not MainForm.TheDrawing.History.CanUndo,
      'The recovered text draft was split across more than one undo step');
    Check(not MainForm.HasActiveEditSession and
      (MainForm.FindComponent('InlineTextMemo') = nil),
      'Completing and undoing the restored draft left a stale editor session');
  finally
    MainForm.OnRecoveryEditSessionRestored := Observer.Previous;
    Driver.Free;
    Observer.Free;
  end;
end;
