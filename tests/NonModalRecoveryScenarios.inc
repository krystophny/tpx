type
  TNonModalRecoveryDialogDriver = class
  public
    Timer: TTimer;
    Clicked: Boolean;
    destructor Destroy; override;
    procedure FindAndRecover(Sender: TObject);
  end;

  TNonModalRecoveryObserver = class
  public
    Previous: TRecoveryEditSessionRestoredEvent;
    RestoredSession: TObjectEditSession;
    procedure RecoveryRestored(Sender: TObject;
      EditSession: TObjectEditSession);
  end;

function NonModalRecoveryRoot: string;
begin
  Result := IncludeTrailingPathDelimiter(GetAppConfigDir(False)) +
    'AutoSaveRecovery';
end;

destructor TNonModalRecoveryDialogDriver.Destroy;
begin
  Timer.Free;
  inherited Destroy;
end;

procedure TNonModalRecoveryDialogDriver.FindAndRecover(Sender: TObject);
var I, J: Integer; Dialog: TForm; Component: TComponent;
begin
  for I := 0 to Screen.FormCount - 1 do
  begin
    Dialog := Screen.Forms[I];
    if Dialog.Caption <> 'Recover a drawing' then Continue;
    for J := 0 to Dialog.ComponentCount - 1 do
    begin
      Component := Dialog.Components[J];
      if Component is TListBox then TListBox(Component).ItemIndex := 0;
      if (Component is TButton) and
        SameText(TButton(Component).Caption, 'Recover') then
      begin
        Timer.Enabled := False;
        Clicked := True;
        TButton(Component).Click;
        Exit;
      end;
    end;
  end;
end;

procedure TNonModalRecoveryObserver.RecoveryRestored(Sender: TObject;
  EditSession: TObjectEditSession);
begin
  if Assigned(Previous) then Previous(Sender, EditSession);
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
      (DecodeTpXTextForEdit((Obj as TText2D).Text) = LabelText) then
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
var SourcePath, ErrorText: string;
  OldRuntimeID: Integer; Target: TText2D; Memo: TMemo;
  Store: TAutoSaveStore; Files: TStringList; Entry: TAutoSaveStoredRecord;
  Snapshot: TDocumentSnapshot; Deadline: QWord;
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
  Memo := MainForm.FindComponent('InlineTextMemo') as TMemo;
  Check((Memo <> nil) and Memo.Visible,
    'Inline text editing did not create a visible canvas memo');
  Memo.Text := '\gamma_{j}';
  Check(MainForm.HasActiveEditSession and
    (Target.TeXText = '\beta_{i}') and
    (DecodeTpXTextForEdit(Target.Text) = 'Fallback α'),
    'Typing the draft changed the committed nested scene');
  Store := TAutoSaveStore.Create(NonModalRecoveryRoot);
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
      ReadDocumentSnapshot(SourcePath).SourceBytes = Snapshot.SourceBytes,
      'Recovery wrote the pending text draft into the source file');
    Check(MainForm.HasActiveEditSession and Memo.Visible and
      (Memo.Text = '\gamma_{j}'),
      'The native inline buffer changed before process restart');
  finally
    Files.Free;
    Store.Free;
  end;
end;

procedure TestNonModalRecoveryRestore;
var Driver: TNonModalRecoveryDialogDriver;
  Observer: TNonModalRecoveryObserver;
  SourcePath, DraftText: string; OldRuntimeID: Integer;
  Deadline: QWord; Target, Sibling: TText2D; IDs: TEditTargetIDs;
  Session: TObjectEditSession; Memo: TMemo; LabelControl: TLabel;
  DoneButton: TButton; SavedRot, SavedSiblingRot: TRealType;
begin
  SourcePath := GetEnvironmentVariable('TPX_NONMODAL_RECOVERY_SOURCE');
  OldRuntimeID := StrToInt(GetEnvironmentVariable(
    'TPX_NONMODAL_RECOVERY_OLD_ID'));
  Observer := TNonModalRecoveryObserver.Create;
  Driver := TNonModalRecoveryDialogDriver.Create;
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
    Check(Driver.Clicked and (Observer.RestoredSession <> nil),
      'Startup did not recover and reopen the inline text edit');
    Session := Observer.RestoredSession;
    Check(MainForm.HasActiveEditSession and Session.IsActive,
      'The restored inline edit is not registered with the main form');
    Target := FindTextByLabel(MainForm.TheDrawing.ObjectList, 'Fallback α');
    Sibling := FindTextByLabel(MainForm.TheDrawing.ObjectList, 'Other label Ω');
    Check((MainForm.TheDrawing.ObjectsCount = 2) and
      (Target <> nil) and (Sibling <> nil) and (Target <> Sibling) and
      (TextGroupDepth(MainForm.TheDrawing.ObjectList, 'Fallback α') = 2) and
      (TextGroupDepth(MainForm.TheDrawing.ObjectList, 'Other label Ω') = 0),
      'Recovery did not restore both labels in the committed scene');
    Check((DecodeTpXTextForEdit(Target.Text) = 'Fallback α') and
      (Target.TeXText = '\beta_{i}') and
      (Target.Points[0].X = 4.125) and (Target.Points[0].Y = 9.5) and
      (Target.Height = 2.25) and (Ord(Target.HAlignment) = 1),
      'The committed nested label changed while restoring its raw draft');
    Check((DecodeTpXTextForEdit(Sibling.Text) = 'Other label Ω') and
      (Sibling.TeXText = '') and (Sibling.Points[0].X = 14.25) and
      (Sibling.Points[0].Y = 8.75) and (Sibling.Height = 1.75),
      'The same-class sibling changed during target rebinding');
    IDs := Session.CopyTargetIDs;
    Check((Length(IDs) = 1) and (IDs[0] <> OldRuntimeID) and
      (Session.DocumentEpoch = MainForm.CurrentDocumentEpoch) and
      (FindTextByLabel(MainForm.TheDrawing.ObjectList, 'Fallback α').ID = IDs[0]),
      'The recovered session did not bind by nested structure to a fresh runtime ID');
    Check(Session.HasRawDraft(opfTeXText, DraftText) and
      Session.HasRawDraftPending and (DraftText = '\gamma_{j}'),
      'Recovery lost the incomplete TeX source buffer');
    Memo := MainForm.FindComponent('InlineTextMemo') as TMemo;
    LabelControl := MainForm.FindComponent('InlineTextFieldLabel') as TLabel;
    DoneButton := MainForm.FindComponent('InlineTextDoneButton') as TButton;
    Check((Memo <> nil) and Memo.Visible and
      (Memo.Text = '\gamma_{j}') and (LabelControl <> nil) and
      (LabelControl.Caption = 'TeX source') and (DoneButton <> nil),
      'The recovered draft is not visible in the on-canvas TeX editor');
    SavedRot := Target.Rot;
    SavedSiblingRot := Sibling.Rot;
    DoneButton.Click;
    Target := FindTextByLabel(MainForm.TheDrawing.ObjectList, 'Fallback α');
    Sibling := FindTextByLabel(MainForm.TheDrawing.ObjectList, 'Other label Ω');
    Check((Target <> nil) and (Target.TeXText = '\gamma_{j}') and
      (DecodeTpXTextForEdit(Target.Text) = 'Fallback α') and
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
      (DecodeTpXTextForEdit(Target.Text) = 'Fallback α') and
      (Sibling <> nil) and (Sibling.TeXText = ''),
      'Undo after restart did not restore the committed scene and sibling');
    Check(not MainForm.HasActiveEditSession and
      (MainForm.FindComponent('InlineTextMemo') = nil),
      'Completing and undoing the restored draft left a stale editor session');
  finally
    MainForm.OnRecoveryEditSessionRestored := Observer.Previous;
    Driver.Free;
    Observer.Free;
  end;
end;
