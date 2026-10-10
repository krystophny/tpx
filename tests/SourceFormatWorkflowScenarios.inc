function SourceWorkflowObjectAt(Drawing: TDrawing2D;
  const Index: Integer): TObject2D;
var
  I: Integer;
begin
  Result := Drawing.ObjectList.FirstObj as TObject2D;
  for I := 0 to Index - 1 do
    if Result <> nil then Result := Drawing.ObjectList.NextObj as TObject2D;
end;

function SourceWorkflowReadBytes(const FileName: string): RawByteString;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Result, Stream.Size);
    if Stream.Size > 0 then Stream.ReadBuffer(Result[1], Stream.Size);
  finally
    Stream.Free;
  end;
end;

procedure SourceWorkflowWriteBytes(const FileName: string;
  const Bytes: RawByteString);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    if Length(Bytes) > 0 then Stream.WriteBuffer(Bytes[1], Length(Bytes));
  finally
    Stream.Free;
  end;
end;

function SourceWorkflowFileCount(const Directory: string): Integer;
var
  Search: TSearchRec;
  Code: Integer;
begin
  Result := 0;
  Code := FindFirst(IncludeTrailingPathDelimiter(Directory) + '*',
    faAnyFile, Search);
  if Code = 0 then
  try
    repeat
      if (Search.Name <> '.') and (Search.Name <> '..') then Inc(Result);
      Code := FindNext(Search);
    until Code <> 0;
  finally
    FindClose(Search);
  end;
end;

procedure SourceWorkflowIdleObserver(const Phase: TDocumentIOPhase;
  const OwnedPath: TDocumentPath);
begin
  case Phase of
    dipPathExists:
      if SameDocumentPath(OwnedPath, SourceWorkflowIdleTarget) then
        Inc(SourceWorkflowIdlePathExistsCount);
    dipMetadataRead:
      if SameDocumentPath(OwnedPath, SourceWorkflowIdleTarget) then
        Inc(SourceWorkflowIdleMetadataCount);
    dipSnapshotRead:
      if SameDocumentPath(OwnedPath, SourceWorkflowIdleTarget) then
        Inc(SourceWorkflowIdleSnapshotCount);
    dipPrimaryFlush, dipAssetFlush, dipPrimaryReplace, dipAssetReplace:
      if SameDocumentPath(DocumentPathDirectory(OwnedPath),
          DocumentPathDirectory(SourceWorkflowIdleTarget)) then
        Inc(SourceWorkflowIdleWriteCount);
  end;
end;

procedure SourceWorkflowAssertIdle(const FileName: string);
const
  IdleDurationMS = 1200;
var
  Timer: TTimer;
  TimerObserver: TSourceWorkflowTimerObserver;
  TimerObservers: TList;
  PreviousObserver: TDocumentIOPhaseObserver;
  Deadline: QWord;
  I, TimerCount, TickCount: Integer;
begin
  MainForm.ProcessPendingReload(GetTickCount64 + 1000);
  Application.ProcessMessages;
  Check(not MainForm.AutoReloadState.SettlePending,
    'source refresh had not settled before idle measurement');
  PreviousObserver := DocumentIOPhaseObserver;
  Check(not Assigned(PreviousObserver),
    'another document I/O observer was active during idle measurement');
  TimerObservers := TList.Create;
  try
    for I := 0 to MainForm.ComponentCount - 1 do
      if MainForm.Components[I] is TTimer then
      begin
        Timer := MainForm.Components[I] as TTimer;
        Check(not Timer.Enabled,
          'a production source timer remained armed before idle measurement');
        TimerObserver := TSourceWorkflowTimerObserver.Create;
        TimerObserver.Timer := Timer;
        TimerObserver.Ticks := 0;
        TimerObserver.Previous := Timer.OnTimer;
        Timer.OnTimer := TimerObserver.TimerTick;
        TimerObservers.Add(TimerObserver);
      end;
    Check(TimerObservers.Count > 0,
      'no MainForm-owned production timers were observed');
    SourceWorkflowIdleTarget := TDocumentPath(FileName);
    SourceWorkflowIdlePathExistsCount := 0;
    SourceWorkflowIdleMetadataCount := 0;
    SourceWorkflowIdleSnapshotCount := 0;
    SourceWorkflowIdleWriteCount := 0;
    DocumentIOPhaseObserver := @SourceWorkflowIdleObserver;
    try
      Deadline := GetTickCount64 + IdleDurationMS;
      repeat
        Application.ProcessMessages;
        Sleep(5);
      until GetTickCount64 >= Deadline;
    finally
      DocumentIOPhaseObserver := PreviousObserver;
      for I := 0 to TimerObservers.Count - 1 do
      begin
        TimerObserver := TSourceWorkflowTimerObserver(TimerObservers[I]);
        TimerObserver.Timer.OnTimer := TimerObserver.Previous;
      end;
    end;
    Check((SourceWorkflowIdlePathExistsCount = 0) and
      (SourceWorkflowIdleMetadataCount = 0) and
      (SourceWorkflowIdleSnapshotCount = 0),
      Format('idle editor performed source checks: exists=%d metadata=%d snapshot=%d',
        [SourceWorkflowIdlePathExistsCount, SourceWorkflowIdleMetadataCount,
         SourceWorkflowIdleSnapshotCount]));
    Check(SourceWorkflowIdleWriteCount = 0,
      'idle editor performed a document source write');
    TickCount := 0;
    TimerCount := 0;
    for I := 0 to MainForm.ComponentCount - 1 do
      if MainForm.Components[I] is TTimer then
      begin
        Inc(TimerCount);
        Timer := MainForm.Components[I] as TTimer;
        Check(not Timer.Enabled,
          'a production source timer remained armed after idle');
      end;
    Check(TimerCount = TimerObservers.Count,
      'MainForm timer ownership changed during idle measurement');
    for I := 0 to TimerObservers.Count - 1 do
    begin
      TimerObserver := TSourceWorkflowTimerObserver(TimerObservers[I]);
      Inc(TickCount, TimerObserver.Ticks);
    end;
    Check(TickCount = 0,
      'a production source timer ticked during idle measurement');
    Check(not MainForm.AutoReloadState.SettlePending,
      'source settle state remained active after idle');
  finally
    DocumentIOPhaseObserver := PreviousObserver;
    for I := 0 to TimerObservers.Count - 1 do
    begin
      TimerObserver := TSourceWorkflowTimerObserver(TimerObservers[I]);
      TimerObserver.Timer.OnTimer := TimerObserver.Previous;
      TimerObserver.Free;
    end;
    TimerObservers.Free;
  end;
end;

procedure SourceWorkflowFaultObserver(const Phase: TDocumentIOPhase;
  const OwnedPath: TDocumentPath);
begin
  if (Phase <> SourceWorkflowFaultPhase) or
    not SameDocumentPath(OwnedPath, SourceWorkflowFaultTarget) then Exit;
  Inc(SourceWorkflowFaultCount);
  DocumentIOPhaseObserver := nil;
  if SourceWorkflowWriteExternalBytes then
    SourceWorkflowWriteBytes(string(OwnedPath), SourceWorkflowExternalBytes)
  else
    raise EWriteError.Create('source-acceptance injected write phase');
end;

function SourceWorkflowPhaseName(const Phase: TDocumentIOPhase): string;
begin
  Result := '';
  case Phase of
    dipPrimaryFlush: Result := 'primary-flush';
    dipAssetFlush: Result := 'asset-flush';
    dipPrimaryReplace: Result := 'primary-replace';
    dipAssetReplace: Result := 'asset-replace';
  end;
end;

procedure SourceWorkflowDragPoint(Primitive: TPrimitive2D;
  const PointIndex, DX, DY: Integer);
var
  StartPoint, EndPoint: TPoint;
begin
  StartPoint := Point2DToPoint(MainForm.LocalView.ViewportToScreen(
    Primitive.Points[PointIndex]));
  EndPoint := Point(StartPoint.X + DX, StartPoint.Y + DY);
  MainForm.EventManager.MouseDown(nil, mbLeft, [], StartPoint.X, StartPoint.Y);
  MainForm.EventManager.MouseMove(nil, [ssLeft], EndPoint.X, EndPoint.Y);
  MainForm.EventManager.MouseUp(nil, mbLeft, [], EndPoint.X, EndPoint.Y);
end;

procedure SourceWorkflowCheckNear(const Actual, Expected: TRealType;
  const LabelText: string; const Tolerance: TRealType = 0.0002);
begin
  Check(Abs(Actual - Expected) <= Tolerance,
    LabelText + ': got ' + FloatToStr(Actual) + ', expected ' +
    FloatToStr(Expected));
end;

procedure SourceWorkflowCheckConfigIsolation;
var
  ExpectedHome, ExpectedConfig, ActualConfig: TDocumentPath;

  function IsWithinRoot(const Path, Root: TDocumentPath): Boolean;
  begin
    Result := (Root <> '') and
      ((Path = Root) or
       (Pos(IncludeTrailingPathDelimiter(string(Root)),
         IncludeTrailingPathDelimiter(string(Path))) = 1));
  end;

begin
  ExpectedHome := NormalizeDocumentPath(TDocumentPath(
    GetEnvironmentVariable('TPX_TEST_HOME_ROOT')));
  ExpectedConfig := NormalizeDocumentPath(TDocumentPath(
    GetEnvironmentVariable('TPX_TEST_CONFIG_ROOT')));
  ActualConfig := NormalizeDocumentPath(TDocumentPath(GetAppConfigDir(False)));
  Check(IsWithinRoot(ActualConfig, ExpectedHome) or
    IsWithinRoot(ActualConfig, ExpectedConfig),
    'application preferences escaped the unittest temporary roots: ' +
      string(ActualConfig));
end;

procedure SourceWorkflowAssertTikZScene(const ExpectedEnd: TPoint2D);
var
  Drawing: TDrawing2D;
  Obj: TObject2D;
  Primitive: TPrimitive2D;
  Line: TPolyline2D;
  Cubic: TBezierPath2D;
  Rectangle: TRectangle2D;
  LabelText, Beta: string;
begin
  Drawing := MainForm.TheDrawing;
  Check(Drawing.ObjectsCount = 6, 'Metadata-free TikZ scene lost objects');
  Obj := SourceWorkflowObjectAt(Drawing, 0);
  Check(Obj is TPolyline2D, 'TikZ line did not materialize as TPolyline2D');
  Line := Obj as TPolyline2D;
  Check(Line.Points.Count = 2, 'TikZ line point count changed');
  SourceWorkflowCheckNear(Line.Points[0].X, 7.7712094, 'nested line start x');
  SourceWorkflowCheckNear(Line.Points[0].Y, 2.3657585, 'nested line start y');
  SourceWorkflowCheckNear(Line.Points[1].X, ExpectedEnd.X, 'nested line end x');
  SourceWorkflowCheckNear(Line.Points[1].Y, ExpectedEnd.Y, 'nested line end y');
  Check(Line.LineStyle = liDashed, 'TikZ line dash style was lost');
  SourceWorkflowCheckNear(Line.LineWidth * Drawing.LineWidthBase, 0.75,
    'TikZ line width in millimeters');
  Check(ColorToRGB(Line.LineColor) = RGBToColor(51, 102, 153),
    'TikZ line color was lost');

  Obj := SourceWorkflowObjectAt(Drawing, 1);
  Check(Obj is TBezierPath2D, 'TikZ cubic did not materialize as TBezierPath2D');
  Cubic := Obj as TBezierPath2D;
  Check(Cubic.Points.Count = 4, 'TikZ cubic control-point count changed');
  SourceWorkflowCheckNear(Cubic.Points[0].X, 3.6495858, 'cubic start x');
  SourceWorkflowCheckNear(Cubic.Points[0].Y, 2.4762960, 'cubic start y');
  SourceWorkflowCheckNear(Cubic.Points[1].X, 7.5547698, 'cubic control 1 x');
  SourceWorkflowCheckNear(Cubic.Points[1].Y, 3.3420545, 'cubic control 1 y');
  SourceWorkflowCheckNear(Cubic.Points[2].X, 7.3383302, 'cubic control 2 x');
  SourceWorkflowCheckNear(Cubic.Points[2].Y, 4.3183505, 'cubic control 2 y');
  SourceWorkflowCheckNear(Cubic.Points[3].X, 11.2435142, 'cubic end x');
  SourceWorkflowCheckNear(Cubic.Points[3].Y, 5.1841089, 'cubic end y');

  Obj := SourceWorkflowObjectAt(Drawing, 2);
  Check(Obj is TRectangle2D, 'TikZ rectangle did not materialize natively');
  Rectangle := Obj as TRectangle2D;
  SourceWorkflowCheckNear(Rectangle.Points[0].X, 10.25, 'rotated rectangle origin x');
  SourceWorkflowCheckNear(Rectangle.Points[0].Y, 5.5, 'rotated rectangle origin y');
  SourceWorkflowCheckNear(Rectangle.Points[1].X, 17.0785944, 'rotated rectangle corner x');
  SourceWorkflowCheckNear(Rectangle.Points[1].Y, 12.1092963, 'rotated rectangle corner y');
  SourceWorkflowCheckNear(Rectangle.Points[2].X, 8.9720004, 'rotated rectangle side x');
  SourceWorkflowCheckNear(Rectangle.Points[2].Y, 9.5532970, 'rotated rectangle side y');
  SourceWorkflowCheckNear(Rectangle.LineWidth * Drawing.LineWidthBase, 0.5,
    'TikZ rectangle line width in millimeters');
  Check(ColorToRGB(Rectangle.FillColor) = RGBToColor(221, 238, 255),
    'TikZ rectangle fill was lost');

  Obj := SourceWorkflowObjectAt(Drawing, 3);
  Check(Obj is TText2D, 'TikZ plain node did not materialize as TText2D');
  Primitive := Obj as TPrimitive2D;
  LabelText := 'Label ' + UTF8Encode(WideChar($03B2));
  Check((TText2D(Obj).Text = LabelText) and (TText2D(Obj).TeXText = ''),
    'Plain Unicode TikZ text was treated as preview TeX');
  Check(TText2D(Obj).HAlignment = ahLeft, 'TikZ west anchor changed');
  SourceWorkflowCheckNear(Primitive.Points[0].X, 1.25, 'TikZ label x');
  SourceWorkflowCheckNear(Primitive.Points[0].Y, 10.5, 'TikZ label y');
  SourceWorkflowCheckNear(TText2D(Obj).Height, 3.514598, 'TikZ label height');

  Obj := SourceWorkflowObjectAt(Drawing, 4);
  Check(Obj is TText2D, 'TikZ formula node did not materialize as TText2D');
  Beta := '$x^2$';
  Check((TText2D(Obj).TeXText = Beta) and
    (TText2D(Obj).HAlignment = ahRight),
    'TikZ formula body or east anchor changed');
  SourceWorkflowCheckNear(TText2D(Obj).Points[0].X, 10.25, 'TikZ formula x');
  SourceWorkflowCheckNear(TText2D(Obj).Points[0].Y, 14, 'TikZ formula y');

  Obj := SourceWorkflowObjectAt(Drawing, 5);
  Check(Obj is TBitmap2D, 'TikZ local image did not materialize as TBitmap2D');
  Primitive := Obj as TPrimitive2D;
  SourceWorkflowCheckNear(Primitive.Points[0].X, 12, 'TikZ image x');
  SourceWorkflowCheckNear(Primitive.Points[0].Y, 11, 'TikZ image y');
  SourceWorkflowCheckNear(Primitive.Points[2].X, 16, 'TikZ image right edge');
  SourceWorkflowCheckNear(Primitive.Points[2].Y, 14, 'TikZ image top edge');
end;

procedure TestSourceFormatTikZWorkflow;
const
  LineMarker = '\path[line width=0.75mm,draw=L,dash pattern=on 2mm off 1mm] (1,0) -- (';
  CycleDeltaX: TRealType = 0.0131;
  CycleDeltaY: TRealType = -0.0073;
var
  FileName, AfterEditName, AfterStructuralName, ErrorText: string;
  Mode: TTpXMode;
  Session: TDocumentSession;
  Line: TPolyline2D;
  Obj: TObject2D;
  BeforeBytes, AfterBytes, SecondSaveBytes: RawByteString;
  BeforeEnd, ExpectedEnd: TPoint2D;
  StartAt, EndAt, NewStartAt, NewEndAt, Cycle, CycleStartAt,
    CycleEndAt: SizeInt;
  CycleStartBytes, CycleEndBytes: RawByteString;
  Saved: Boolean;
begin
  SourceWorkflowCheckConfigIsolation;
  FileName := GetEnvironmentVariable('TPX_SOURCE_FILE');
  AfterEditName := GetEnvironmentVariable('TPX_SOURCE_AFTER_EDIT');
  AfterStructuralName := GetEnvironmentVariable('TPX_SOURCE_AFTER_STRUCTURAL');
  Check((FileName <> '') and (AfterEditName <> '') and
    (AfterStructuralName <> ''),
    'TikZ workflow file paths are not configured');
  Mode := MainForm.EventManager.Mode as TTpXMode;
  Check(Mode.OpenDocumentFromPath(TDocumentPath(FileName), ErrorText),
    'Production open rejected TikZ fixture: ' + ErrorText);
  Session := MainForm.EventManager.DocumentSession;
  Check((Session.SourceFormatId = 'tikz') and Session.CanSaveBack,
    'Accepted TikZ source was not attached to its save-back session');
  Check(not MainForm.TheDrawing.History.IsChanged,
    'A source-opened TikZ drawing started dirty');
  SourceWorkflowAssertTikZScene(Point2D(11.6763935, 3.2315169));

  BeforeBytes := SourceWorkflowReadBytes(FileName);
  StartAt := Pos(LineMarker, string(BeforeBytes));
  Check(StartAt > 0, 'Independent nested-scope line statement is missing');
  EndAt := StartAt + Pos(';', Copy(string(BeforeBytes), StartAt, MaxInt)) - 1;
  Line := SourceWorkflowObjectAt(MainForm.TheDrawing, 0) as TPolyline2D;
  BeforeEnd := Line.Points[1];
  MainForm.TheDrawing.SelectionAdd(Line);
  SourceWorkflowDragPoint(Line, 1, 14, -8);
  ExpectedEnd := Line.Points[1];
  Check((Abs(ExpectedEnd.X - BeforeEnd.X) > 0.05) and
    (Abs(ExpectedEnd.Y - BeforeEnd.Y) > 0.02),
    'Native point drag did not change only the selected endpoint');
  Check(MainForm.TheDrawing.History.IsChanged,
    'Native control-point drag did not dirty the drawing');

  Saved := Mode.SaveDocumentToPath(TDocumentPath(FileName), 'tikz', False,
    False, ErrorText);
  Check(Saved, 'Same-format TikZ save failed: ' + ErrorText);
  AfterBytes := SourceWorkflowReadBytes(FileName);
  NewStartAt := Pos(LineMarker, string(AfterBytes));
  Check(NewStartAt > 0, 'Edited TikZ statement was removed');
  NewEndAt := NewStartAt +
    Pos(';', Copy(string(AfterBytes), NewStartAt, MaxInt)) - 1;
  Check(NewEndAt > NewStartAt, 'Edited TikZ statement terminator is missing');
  Check(Copy(BeforeBytes, 1, StartAt - 1) =
    Copy(AfterBytes, 1, NewStartAt - 1),
    'TikZ edit changed bytes before its local statement');
  Check(Copy(BeforeBytes, EndAt + 1, MaxInt) =
    Copy(AfterBytes, NewEndAt + 1, MaxInt),
    'TikZ edit changed bytes after its local statement');
  Check(not MainForm.TheDrawing.History.IsChanged,
    'Successful source save did not accept the saved drawing state');
  Check(SameDocumentPath(Session.SourcePath, TDocumentPath(FileName)) and
    (Session.SourceFormatId = 'tikz'),
    'Same-format source save detached the document session');
  SourceWorkflowWriteBytes(AfterEditName, AfterBytes);

  Check(Mode.OpenDocumentFromPath(TDocumentPath(FileName), ErrorText),
    'Reopening edited TikZ source failed: ' + ErrorText);
  SourceWorkflowAssertTikZScene(ExpectedEnd);

  Obj := SourceWorkflowObjectAt(MainForm.TheDrawing, 5);
  Check(Obj is TBitmap2D, 'Expected local image before delete/append case');
  MainForm.TheDrawing.SelectionAdd(Obj);
  MainForm.UserEventExecute(MainForm.DeleteSelected);
  MainForm.EventManager.SendMessage(Msg_InsertLine, nil);
  MainForm.EventManager.MouseDown(nil, mbLeft, [], 80, 100);
  MainForm.EventManager.MouseUp(nil, mbLeft, [], 80, 100);
  MainForm.EventManager.MouseMove(nil, [], 180, 130);
  MainForm.EventManager.MouseDown(nil, mbLeft, [], 180, 130);
  MainForm.EventManager.MouseUp(nil, mbLeft, [], 180, 130);
  Check(MainForm.TheDrawing.ObjectsCount = 6,
    'Delete plus graphical line insertion produced the wrong object count');
  Saved := Mode.SaveDocumentToPath(TDocumentPath(FileName), 'tikz', False,
    False, ErrorText);
  Check(Saved, 'TikZ save after delete plus append failed: ' + ErrorText);
  AfterBytes := SourceWorkflowReadBytes(FileName);
  Check(Pos('includegraphics', string(AfterBytes)) = 0,
    'Deleted local image source statement remained after save');
  Check(not MainForm.TheDrawing.History.IsChanged,
    'Delete plus append save left the source drawing dirty');

  Check(Mode.OpenDocumentFromPath(TDocumentPath(FileName), ErrorText),
    'Reopening TikZ after delete plus append failed: ' + ErrorText);
  Check(MainForm.TheDrawing.ObjectsCount = 6,
    'Saved TikZ delete plus append changed scene object count');
  Obj := SourceWorkflowObjectAt(MainForm.TheDrawing, 5);
  Check(Obj is TPolyline2D,
    'Appended native line did not survive TikZ source reload');
  SecondSaveBytes := SourceWorkflowReadBytes(FileName);
  Saved := Mode.SaveDocumentToPath(TDocumentPath(FileName), 'tikz', False,
    False, ErrorText);
  Check(Saved, 'Second save after TikZ ID rebinding failed: ' + ErrorText);
  Check(SourceWorkflowReadBytes(FileName) = SecondSaveBytes,
    'Second no-op save changed source after delete/append rebinding');

  Line := SourceWorkflowObjectAt(MainForm.TheDrawing, 0) as TPolyline2D;
  ExpectedEnd := Line.Points[1];
  CycleStartBytes := SourceWorkflowReadBytes(FileName);
  CycleStartAt := Pos(LineMarker, string(CycleStartBytes));
  Check(CycleStartAt > 0, '100-cycle TikZ source lost its scoped line');
  CycleEndAt := CycleStartAt +
    Pos(';', Copy(string(CycleStartBytes), CycleStartAt, MaxInt)) - 1;
  Check(CycleEndAt > CycleStartAt,
    '100-cycle TikZ source line terminator is missing');
  for Cycle := 1 to 100 do
  begin
    ExpectedEnd.X := ExpectedEnd.X + CycleDeltaX;
    ExpectedEnd.Y := ExpectedEnd.Y + CycleDeltaY;
    Line.Points[1] := ExpectedEnd;
    Saved := Mode.SaveDocumentToPath(TDocumentPath(FileName), 'tikz', False,
      False, ErrorText);
    Check(Saved, 'Single-precision TikZ cycle save failed at ' +
      IntToStr(Cycle) + ': ' + ErrorText);
    Check(Mode.OpenDocumentFromPath(TDocumentPath(FileName), ErrorText),
      'Single-precision TikZ cycle reload failed at ' + IntToStr(Cycle) +
      ': ' + ErrorText);
    Line := SourceWorkflowObjectAt(MainForm.TheDrawing, 0) as TPolyline2D;
    SourceWorkflowCheckNear(Line.Points[1].X, ExpectedEnd.X,
      '100-cycle reloaded line x', 0.0003);
    SourceWorkflowCheckNear(Line.Points[1].Y, ExpectedEnd.Y,
      '100-cycle reloaded line y', 0.0003);
  end;
  CycleEndBytes := SourceWorkflowReadBytes(FileName);
  NewStartAt := Pos(LineMarker, string(CycleEndBytes));
  Check(NewStartAt > 0, '100-cycle save removed the scoped line');
  NewEndAt := NewStartAt +
    Pos(';', Copy(string(CycleEndBytes), NewStartAt, MaxInt)) - 1;
  Check((NewEndAt > NewStartAt) and
    (Copy(CycleStartBytes, 1, CycleStartAt - 1) =
      Copy(CycleEndBytes, 1, NewStartAt - 1)) and
    (Copy(CycleStartBytes, CycleEndAt + 1, MaxInt) =
      Copy(CycleEndBytes, NewEndAt + 1, MaxInt)),
    '100-cycle save changed source bytes outside the edited statement');
  SourceWorkflowWriteBytes(AfterStructuralName, CycleEndBytes);
end;

procedure TestSourceFormatSVGWorkflow;
var
  FileName, AfterEditName, ErrorText: string;
  Mode: TTpXMode;
  Session: TDocumentSession;
  Line: TPrimitive2D;
  Cubic: TBezierPath2D;
  TextObject: TText2D;
  Obj: TObject2D;
  AfterBytes: RawByteString;
  BeforeEnd, ExpectedEnd: TPoint2D;
  Saved: Boolean;
begin
  SourceWorkflowCheckConfigIsolation;
  FileName := GetEnvironmentVariable('TPX_SOURCE_FILE');
  AfterEditName := GetEnvironmentVariable('TPX_SOURCE_AFTER_EDIT');
  Check((FileName <> '') and (AfterEditName <> ''),
    'SVG workflow paths are not configured');
  Mode := MainForm.EventManager.Mode as TTpXMode;
  Check(Mode.OpenDocumentFromPath(TDocumentPath(FileName), ErrorText),
    'Production open rejected editable SVG fixture: ' + ErrorText);
  Session := MainForm.EventManager.DocumentSession;
  Check((Session.SourceFormatId = 'svg') and Session.CanSaveBack,
    'Accepted SVG source was not attached to its save-back session');
  Check(MainForm.TheDrawing.ObjectsCount = 5,
    'Strict SVG fixture did not materialize its five objects');
  Check(not MainForm.TheDrawing.History.IsChanged,
    'An SVG source-opened drawing started dirty');
  Check(MainForm.TheDrawing.PicScale = 1,
    'SVG physical/user-space scale was not retained by the native drawing');

  Obj := SourceWorkflowObjectAt(MainForm.TheDrawing, 0);
  Check((Obj is TLine2D) or (Obj is TPolyline2D),
    'SVG line did not materialize as a native line primitive');
  Line := Obj as TPrimitive2D;
  SourceWorkflowCheckNear(Line.Points[0].X, 1, 'SVG line start x');
  SourceWorkflowCheckNear(Line.Points[0].Y, 2, 'SVG y-up line start y');
  SourceWorkflowCheckNear(Line.Points[1].X, 7, 'SVG line end x');
  SourceWorkflowCheckNear(Line.Points[1].Y, 5, 'SVG y-up line end y');
  Check(Line.LineStyle = liSolid, 'SVG solid line style changed');
  SourceWorkflowCheckNear(Line.LineWidth, 0.5, 'SVG line width');
  Check(ColorToRGB(Line.LineColor) = RGBToColor(51, 102, 153),
    'SVG line color was lost');
  Obj := SourceWorkflowObjectAt(MainForm.TheDrawing, 1);
  Check(Obj is TBezierPath2D, 'SVG cubic did not materialize as TBezierPath2D');
  Cubic := Obj as TBezierPath2D;
  SourceWorkflowCheckNear(Cubic.Points[0].X, 2, 'SVG cubic start x');
  SourceWorkflowCheckNear(Cubic.Points[0].Y, 7, 'SVG y-up cubic start y');
  SourceWorkflowCheckNear(Cubic.Points[1].X, 3, 'SVG cubic control 1 x');
  SourceWorkflowCheckNear(Cubic.Points[1].Y, 8, 'SVG cubic control 1 y');
  SourceWorkflowCheckNear(Cubic.Points[2].X, 4, 'SVG cubic control 2 x');
  SourceWorkflowCheckNear(Cubic.Points[2].Y, 9, 'SVG cubic control 2 y');
  SourceWorkflowCheckNear(Cubic.Points[3].X, 5, 'SVG cubic end x');
  SourceWorkflowCheckNear(Cubic.Points[3].Y, 10, 'SVG cubic end y');
  Obj := SourceWorkflowObjectAt(MainForm.TheDrawing, 3);
  Check(Obj is TCircle2D, 'SVG circle did not materialize as TCircle2D');
  SourceWorkflowCheckNear(TCircle2D(Obj).Points[0].X, 17, 'SVG circle center x');
  SourceWorkflowCheckNear(TCircle2D(Obj).Points[0].Y, 5, 'SVG circle y-up center y');
  SourceWorkflowCheckNear(Sqrt(Sqr(TCircle2D(Obj).Points[1].X -
    TCircle2D(Obj).Points[0].X) + Sqr(TCircle2D(Obj).Points[1].Y -
    TCircle2D(Obj).Points[0].Y)), 1, 'SVG circle radius');
  Obj := SourceWorkflowObjectAt(MainForm.TheDrawing, 4);
  Check(Obj is TText2D, 'SVG label did not materialize as TText2D');
  TextObject := Obj as TText2D;
  Check((UTF8Encode(TextObject.WideText) = 'Label ' + #$CE + #$B2) and
    (TextObject.HAlignment = ahLeft), 'SVG Unicode text or anchor changed');
  SourceWorkflowCheckNear(TextObject.Points[0].X, 1, 'SVG label x');
  SourceWorkflowCheckNear(TextObject.Points[0].Y, 4, 'SVG y-up label y');

  BeforeEnd := Line.Points[1];
  MainForm.TheDrawing.SelectionAdd(Line);
  SourceWorkflowDragPoint(Line, 1, 14, -8);
  ExpectedEnd := Line.Points[1];
  Check((Abs(ExpectedEnd.X - BeforeEnd.X) > 0.05) and
    (Abs(ExpectedEnd.Y - BeforeEnd.Y) > 0.02),
    'Native SVG control-point drag did not change the selected endpoint');
  Saved := Mode.SaveDocumentToPath(TDocumentPath(FileName), 'svg', False,
    False, ErrorText);
  Check(Saved, 'Same-format SVG save failed: ' + ErrorText);
  AfterBytes := SourceWorkflowReadBytes(FileName);
  SourceWorkflowWriteBytes(AfterEditName, AfterBytes);
  Check(not MainForm.TheDrawing.History.IsChanged,
    'Successful SVG source save did not accept the edited state');
  Check(SameDocumentPath(Session.SourcePath, TDocumentPath(FileName)) and
    (Session.SourceFormatId = 'svg'),
    'Same-format SVG save detached the document session');
  Check(Mode.OpenDocumentFromPath(TDocumentPath(FileName), ErrorText),
    'Reopening edited SVG source failed: ' + ErrorText);
  Check(MainForm.TheDrawing.ObjectsCount = 5,
    'SVG save/reload changed the editable scene object count');
  Obj := SourceWorkflowObjectAt(MainForm.TheDrawing, 0);
  Check((Obj is TLine2D) or (Obj is TPolyline2D),
    'Reopened SVG line did not materialize as a native line primitive');
  Line := Obj as TPrimitive2D;
  SourceWorkflowCheckNear(Line.Points[0].X, 1, 'reloaded SVG line start x');
  SourceWorkflowCheckNear(Line.Points[0].Y, 2, 'reloaded SVG line start y');
  SourceWorkflowCheckNear(Line.Points[1].X, ExpectedEnd.X, 'reloaded SVG line end x');
  SourceWorkflowCheckNear(Line.Points[1].Y, ExpectedEnd.Y, 'reloaded SVG line end y');
end;

procedure TestSourceFormatExternalRefresh(const FormatId: string);
var
  FileName, ErrorText: string;
  Mode: TTpXMode;
  Session: TDocumentSession;
  OriginalBytes, ExternalBytes, SavedBytes: RawByteString;
  ExternalRevision: TDiskRevision;
  OldAutoRefresh: Boolean;
  Deadline: QWord;
  Obj: TObject2D;
  Line: TPrimitive2D;
  ExpectedExternal, ExpectedSaved: TPoint2D;
  Saved: Boolean;
  ExpectedObjectCount: Integer;
begin
  SourceWorkflowCheckConfigIsolation;
  FileName := GetEnvironmentVariable('TPX_SOURCE_FILE');
  Check(FileName <> '', 'cross-format refresh source path is not configured');
  Check((FormatId = 'tikz') or (FormatId = 'svg'),
    'cross-format refresh received an unknown format');
  OriginalBytes := SourceWorkflowReadBytes(FileName);
  if FormatId = 'svg' then
  begin
    ExternalBytes := StringReplace(string(OriginalBytes), 'x2="7"',
      'x2="9"', []);
    ExpectedExternal := Point2D(9, 5);
    ExpectedObjectCount := 5;
  end
  else
  begin
    ExternalBytes := StringReplace(string(OriginalBytes), '(2,0);',
      '(2.25,0);', []);
    ExpectedExternal := Point2D(12.6526895, 3.4479565);
    ExpectedObjectCount := 6;
  end;
  Check((ExternalBytes <> OriginalBytes) and (Length(ExternalBytes) > 0),
    'external source mutation did not match its independent fixture token');

  OldAutoRefresh := AutoRefreshEnabled;
  AutoRefreshEnabled := True;
  MainForm.Show;
  MainForm.SetFocus;
  Application.ProcessMessages;
  MainForm.ApplyAutoReloadSettings;
  MainForm.ClearDocumentWatchBinding;
  MainForm.EventManager.DocumentSession.Clear;
  Mode := MainForm.EventManager.Mode as TTpXMode;
  try
    Check(Mode.OpenDocumentFromPath(TDocumentPath(FileName), ErrorText),
      'production open rejected refresh fixture: ' + ErrorText);
    Session := MainForm.EventManager.DocumentSession;
    Check((Session.SourceFormatId = FormatId) and Session.CanSaveBack,
      'source format session was not eligible for same-format save');
    Check(not MainForm.AutoReloadState.WatchUnavailable,
      'production source open did not bind a usable native watch');
    Check(MainForm.TheDrawing.ObjectsCount = ExpectedObjectCount,
      'initial cross-format refresh scene has the wrong object count');

    SourceWorkflowWriteBytes(FileName, ExternalBytes);
    Deadline := GetTickCount64 + 7000;
    repeat
      Application.ProcessMessages;
      MainForm.ProcessPendingReload(GetTickCount64 + 1000);
      Obj := SourceWorkflowObjectAt(MainForm.TheDrawing, 0);
      Check((Obj is TPolyline2D) or (Obj is TLine2D),
        'external source refresh changed the first primitive type');
      Line := Obj as TPrimitive2D;
      if (Session.OriginalSourceBytes = ExternalBytes) and
        (Abs(Line.Points[1].X - ExpectedExternal.X) < 0.0003) and
        (Abs(Line.Points[1].Y - ExpectedExternal.Y) < 0.0003) then Break;
      Sleep(10);
    until GetTickCount64 >= Deadline;
    Check((Session.OriginalSourceBytes = ExternalBytes) and
      (Abs(Line.Points[1].X - ExpectedExternal.X) < 0.0003) and
      (Abs(Line.Points[1].Y - ExpectedExternal.Y) < 0.0003),
      Format('%s source event did not commit its new native geometry',
        [FormatId]));
    ExternalRevision := ReadDocumentRevision(FileName);
    Check(SameDiskRevision(Session.AcceptedRevision, ExternalRevision) and
      not MainForm.AutoReloadState.Conflict and
      not MainForm.AutoReloadState.InvalidExternal,
      'source refresh did not accept a clean external disk revision');
    Check(not MainForm.TheDrawing.History.IsChanged,
      'external source refresh made the drawing locally dirty');

    SourceWorkflowAssertIdle(FileName);

    MainForm.TheDrawing.SelectionAdd(Line);
    SourceWorkflowDragPoint(Line, 1, 12, -7);
    ExpectedSaved := Line.Points[1];
    Check(MainForm.TheDrawing.History.IsChanged,
      'graphic edit after source refresh did not dirty the drawing');
    Saved := Mode.SaveDocumentToPath(TDocumentPath(FileName), FormatId,
      False, False, ErrorText);
    Check(Saved, 'same-format save after external refresh failed: ' + ErrorText);
    SavedBytes := SourceWorkflowReadBytes(FileName);
    Check(SavedBytes <> ExternalBytes,
      'same-format graphic edit did not update external source bytes');
    Check(not MainForm.TheDrawing.History.IsChanged,
      'same-format save after refresh did not accept the graphic edit');
    Check(MainForm.TheDrawing.History.CanUndo,
      'same-format source save discarded the graphical edit from undo history');
    Check((Session.SourceFormatId = FormatId) and
      SameDocumentPath(Session.SourcePath, TDocumentPath(FileName)) and
      (Session.OriginalSourceBytes = SavedBytes),
      'same-format save after refresh detached its accepted source session');

    MainForm.Undo.Execute;
    Line := SourceWorkflowObjectAt(MainForm.TheDrawing, 0) as TPrimitive2D;
    Check(MainForm.TheDrawing.History.IsChanged,
      'undo after source save did not make the drawing dirty');
    SourceWorkflowCheckNear(Line.Points[1].X, ExpectedExternal.X,
      'undo after source save did not restore the refreshed x', 0.0003);
    SourceWorkflowCheckNear(Line.Points[1].Y, ExpectedExternal.Y,
      'undo after source save did not restore the refreshed y', 0.0003);
    Check(SourceWorkflowReadBytes(FileName) = SavedBytes,
      'undo after source save rewrote the published source');
    MainForm.Redo.Execute;
    Line := SourceWorkflowObjectAt(MainForm.TheDrawing, 0) as TPrimitive2D;
    Check(not MainForm.TheDrawing.History.IsChanged,
      'redo after source save did not restore the accepted state');
    SourceWorkflowCheckNear(Line.Points[1].X, ExpectedSaved.X,
      'redo after source save did not restore the saved x', 0.0003);
    SourceWorkflowCheckNear(Line.Points[1].Y, ExpectedSaved.Y,
      'redo after source save did not restore the saved y', 0.0003);
    Check(SourceWorkflowReadBytes(FileName) = SavedBytes,
      'redo after source save rewrote the published source');

    Check(Mode.OpenDocumentFromPath(TDocumentPath(FileName), ErrorText),
      'reopening cross-format source after refresh/save failed: ' + ErrorText);
    Obj := SourceWorkflowObjectAt(MainForm.TheDrawing, 0);
    Check((Obj is TPolyline2D) or (Obj is TLine2D),
      'saved source did not reload its edited native line');
    Line := Obj as TPrimitive2D;
    SourceWorkflowCheckNear(Line.Points[1].X, ExpectedSaved.X,
      'reloaded post-refresh line endpoint x', 0.0003);
    SourceWorkflowCheckNear(Line.Points[1].Y, ExpectedSaved.Y,
      'reloaded post-refresh line endpoint y', 0.0003);
  finally
    MainForm.ClearDocumentWatchBinding;
    AutoRefreshEnabled := OldAutoRefresh;
    MainForm.ApplyAutoReloadSettings;
  end;
end;

procedure TestSourceFormatRefreshSVG;
begin
  TestSourceFormatExternalRefresh('svg');
end;

procedure TestSourceFormatRefreshTikZ;
begin
  TestSourceFormatExternalRefresh('tikz');
end;

procedure SourceWorkflowWaitForSettlePending;
var Deadline: QWord;
begin
  Deadline := GetTickCount64 + 6000;
  repeat
    Application.ProcessMessages;
    MainForm.ProcessPendingReload(GetTickCount64 + 1000);
    if MainForm.AutoReloadState.SettlePending then Exit;
    Sleep(5);
  until GetTickCount64 >= Deadline;
  Check(False, 'filesystem helper event did not reach the deferred reload state');
end;

procedure TestSourceFormatBindingBurst(const FormatId: string);
const CycleCount = 8;
var
  BurstRoot, CycleDirectory, SourceFile, TargetFile, ErrorText, Extension: string;
  OriginalBytes, ExternalBytes, SavedBytes, OriginalAsset: RawByteString;
  Mode: TTpXMode;
  Session: TDocumentSession;
  Line: TPrimitive2D;
  BeforeEnd: TPoint2D;
  OldAutoRefresh: Boolean;
  OldGeneration, SaveAsGeneration: QWord;
  Deadline: QWord;
  I, ExpectedFiles: Integer;
  Saved, InteractionActive: Boolean;
begin
  SourceWorkflowCheckConfigIsolation;
  Check((FormatId = 'svg') or (FormatId = 'tikz'),
    'binding burst received an unsupported source format');
  BurstRoot := GetEnvironmentVariable('TPX_SOURCE_BURST_ROOT');
  Check((BurstRoot <> '') and DirectoryExists(BurstRoot),
    'source binding burst directory is not configured');
  OldAutoRefresh := AutoRefreshEnabled;
  if FormatId = 'svg' then Extension := 'svg' else Extension := 'tex';
  AutoRefreshEnabled := True;
  InteractionActive := False;
  MainForm.Show;
  MainForm.SetFocus;
  Application.ProcessMessages;
  MainForm.ApplyAutoReloadSettings;
  MainForm.ClearDocumentWatchBinding;
  MainForm.EventManager.DocumentSession.Clear;
  Mode := MainForm.EventManager.Mode as TTpXMode;
  try
    for I := 1 to CycleCount do
    begin
      CycleDirectory := IncludeTrailingPathDelimiter(BurstRoot) +
        'cycle-' + Format('%.2d', [I]);
      SourceFile := IncludeTrailingPathDelimiter(CycleDirectory) +
        'source.' + Extension;
      TargetFile := IncludeTrailingPathDelimiter(CycleDirectory) +
        'saved.' + Extension;
      Check(DirectoryExists(CycleDirectory) and FileExists(SourceFile),
        'source binding burst fixture is missing: ' + SourceFile);
      OriginalBytes := SourceWorkflowReadBytes(SourceFile);
      if FormatId = 'svg' then
      begin
        ExternalBytes := StringReplace(string(OriginalBytes), 'x2="7"',
          'x2="9"', []);
        ExpectedFiles := 2;
      end
      else
      begin
        OriginalAsset := SourceWorkflowReadBytes(
          IncludeTrailingPathDelimiter(CycleDirectory) + 'acceptance-image.png');
        ExternalBytes := StringReplace(string(OriginalBytes),
          '(2,0);', '(2.25,0);', []);
        ExpectedFiles := 3;
      end;
      Check((ExternalBytes <> OriginalBytes) and (Length(ExternalBytes) > 0),
        'could not mutate the independent source for a lifecycle cycle');
      Check(Mode.OpenDocumentFromPath(TDocumentPath(SourceFile), ErrorText),
        'source lifecycle open failed: ' + ErrorText);
      Session := MainForm.EventManager.DocumentSession;
      Check((Session.SourceFormatId = FormatId) and Session.CanSaveBack and
        SameDocumentPath(Session.SourcePath, TDocumentPath(SourceFile)),
        'source lifecycle open did not bind its format and path');
      Check(not MainForm.AutoReloadState.WatchUnavailable,
        'source lifecycle open did not bind a native watcher');
      Line := SourceWorkflowObjectAt(MainForm.TheDrawing, 0) as TPrimitive2D;
      BeforeEnd := Line.Points[1];
      OldGeneration := Session.WatchGeneration;

      { Let the real watcher enqueue a source event, but save to a new path
        while an active scene interaction defers the old generation's parse. }
      MainForm.BeginSceneInteraction;
      InteractionActive := True;
      SourceWorkflowWriteBytes(SourceFile, ExternalBytes);
      SourceWorkflowWaitForSettlePending;
      Check(Session.WatchGeneration = OldGeneration,
        'queued source event changed the active generation before acceptance');
      Saved := Mode.SaveDocumentToPath(TDocumentPath(TargetFile), FormatId,
        True, True, ErrorText);
      Check(Saved, 'Save As during a queued stale callback failed: ' + ErrorText);
      SavedBytes := SourceWorkflowReadBytes(TargetFile);
      Check((Session.SourcePath = TDocumentPath(TargetFile)) and
        (Session.OriginalSourceBytes = SavedBytes) and
        (Session.WatchGeneration > OldGeneration),
        'Save As did not accept a new source generation and exact bytes');
      SaveAsGeneration := Session.WatchGeneration;
      MainForm.EndSceneInteraction;
      InteractionActive := False;
      SourceWorkflowCheckNear(Line.Points[1].X, BeforeEnd.X,
        'pending old-source event changed the live endpoint x');
      SourceWorkflowCheckNear(Line.Points[1].Y, BeforeEnd.Y,
        'pending old-source event changed the live endpoint y');

      Deadline := GetTickCount64 + 700;
      repeat
        Application.ProcessMessages;
        MainForm.ProcessPendingReload(GetTickCount64);
        Sleep(5);
      until GetTickCount64 >= Deadline;
      Check((Session.SourcePath = TDocumentPath(TargetFile)) and
        (Session.WatchGeneration = SaveAsGeneration) and
        (Session.OriginalSourceBytes = SavedBytes) and
        not MainForm.AutoReloadState.Conflict and
        not MainForm.AutoReloadState.InvalidExternal and
        not MainForm.AutoReloadState.SettlePending,
        'a stale callback from the prior source crossed Save As');
      SourceWorkflowCheckNear(Line.Points[1].X, BeforeEnd.X,
        'stale callback changed the saved line endpoint x');
      SourceWorkflowCheckNear(Line.Points[1].Y, BeforeEnd.Y,
        'stale callback changed the saved line endpoint y');

      Check(Mode.OpenDocumentFromPath(TDocumentPath(TargetFile), ErrorText),
        'reopening the latest Save As target failed: ' + ErrorText);
      Session := MainForm.EventManager.DocumentSession;
      Line := SourceWorkflowObjectAt(MainForm.TheDrawing, 0) as TPrimitive2D;
      Check((Session.SourcePath = TDocumentPath(TargetFile)) and
        (Session.OriginalSourceBytes = SavedBytes) and
        (Session.WatchGeneration > SaveAsGeneration),
        'reopened target did not establish a fresh source binding');
      SourceWorkflowCheckNear(Line.Points[1].X, BeforeEnd.X,
        'Save As reload changed the local endpoint x');
      SourceWorkflowCheckNear(Line.Points[1].Y, BeforeEnd.Y,
        'Save As reload changed the local endpoint y');
      Check(SourceWorkflowFileCount(CycleDirectory) = ExpectedFiles,
        'repeated source binding left an unexpected stage, backup, or sidecar');
      if FormatId = 'tikz' then
        Check(SourceWorkflowReadBytes(
          IncludeTrailingPathDelimiter(CycleDirectory) + 'acceptance-image.png') =
          OriginalAsset,
          'repeated Save As changed the linked image bytes');

      MainForm.ClearDocumentWatchBinding;
      Session.Clear;
      Check(not MainForm.AutoReloadState.SettlePending,
        'closing a source lifecycle cycle left deferred work active');
      Application.ProcessMessages;
    end;
  finally
    if InteractionActive then MainForm.EndSceneInteraction;
    MainForm.ClearDocumentWatchBinding;
    AutoRefreshEnabled := OldAutoRefresh;
    MainForm.ApplyAutoReloadSettings;
  end;
end;

procedure TestSourceFormatBindingBurstSVG;
begin
  TestSourceFormatBindingBurst('svg');
end;

procedure TestSourceFormatBindingBurstTikZ;
begin
  TestSourceFormatBindingBurst('tikz');
end;

procedure TestSourceFormatWriteFailures;
var
  SourceFile, FaultRoot, CaseDirectory, Destination, AssetDestination,
    ErrorText, PhaseName: string;
  Mode: TTpXMode;
  Session: TDocumentSession;
  Context: TTikZImportContext;
  Line: TPolyline2D;
  Obj: TObject2D;
  OriginalBytes, OriginalAsset, AcceptedBytes, DestinationBytes, DestinationAsset,
    ExternalBytes: RawByteString;
  AcceptedRevision: TDiskRevision;
  AcceptedLocalRevision, AcceptedWatchGeneration: QWord;
  AcceptedCodecContext: TObject;
  AcceptedSourcePath: TDocumentPath;
  AcceptedRecoveryPath: TDocumentPath;
  AcceptedEnd: TPoint2D;
  AcceptedUndo, AcceptedRedo: Boolean;
  PreviousObserver: TDocumentIOPhaseObserver;
  PreviousExternalBytes: RawByteString;
  Phase: TDocumentIOPhase;
  Saved, OldWriteExternalBytes: Boolean;
  BindingIndex: Integer;
begin
  SourceWorkflowCheckConfigIsolation;
  SourceFile := GetEnvironmentVariable('TPX_SOURCE_FILE');
  FaultRoot := GetEnvironmentVariable('TPX_SOURCE_FAULT_ROOT');
  Check((SourceFile <> '') and (FaultRoot <> ''),
    'write-failure workflow paths are not configured');
  Check(not Assigned(DocumentIOPhaseObserver),
    'write-failure observer was already installed');
  OriginalBytes := SourceWorkflowReadBytes(SourceFile);
  OriginalAsset := SourceWorkflowReadBytes(
    ExtractFilePath(SourceFile) + 'acceptance-image.png');
  Mode := MainForm.EventManager.Mode as TTpXMode;
  Check(Mode.OpenDocumentFromPath(TDocumentPath(SourceFile), ErrorText),
    'Production open rejected write-failure TikZ source: ' + ErrorText);
  Session := MainForm.EventManager.DocumentSession;
  Check(Session.CodecContext is TTikZImportContext,
    'TikZ source did not retain its codec context');
  Context := Session.CodecContext as TTikZImportContext;
  Line := SourceWorkflowObjectAt(MainForm.TheDrawing, 0) as TPolyline2D;
  MainForm.TheDrawing.SelectionAdd(Line);
  SourceWorkflowDragPoint(Line, 1, 12, -7);
  AcceptedEnd := Line.Points[1];
  Check(MainForm.TheDrawing.History.IsChanged,
    'write-failure fixture edit did not create dirty history');

  AcceptedRevision := Session.AcceptedRevision;
  AcceptedBytes := Session.OriginalSourceBytes;
  AcceptedLocalRevision := Session.LocalRevision;
  AcceptedWatchGeneration := Session.WatchGeneration;
  AcceptedCodecContext := Session.CodecContext;
  AcceptedSourcePath := Session.SourcePath;
  AcceptedRecoveryPath := Session.RecoveryBackupPath;
  AcceptedUndo := MainForm.TheDrawing.History.CanUndo;
  AcceptedRedo := MainForm.TheDrawing.History.CanRedo;
  BindingIndex := Context.FindBindingForObjectID(Line.ID);
  Check(BindingIndex >= 0, 'edited TikZ line lost its source binding');
  PreviousObserver := DocumentIOPhaseObserver;
  OldWriteExternalBytes := SourceWorkflowWriteExternalBytes;
  PreviousExternalBytes := SourceWorkflowExternalBytes;
  try
    for Phase := dipPrimaryFlush to dipAssetReplace do
    begin
      PhaseName := SourceWorkflowPhaseName(Phase);
      CaseDirectory := IncludeTrailingPathDelimiter(FaultRoot) + PhaseName;
      Check(ForceDirectories(CaseDirectory),
        'could not create write-failure destination directory');
      Destination := IncludeTrailingPathDelimiter(CaseDirectory) + 'target.tex';
      AssetDestination := IncludeTrailingPathDelimiter(CaseDirectory) +
        'acceptance-image.png';
      DestinationBytes := 'existing source before ' + PhaseName;
      DestinationAsset := 'existing asset before ' + PhaseName;
      SourceWorkflowWriteBytes(Destination, DestinationBytes);
      SourceWorkflowWriteBytes(AssetDestination, DestinationAsset);

      SourceWorkflowFaultPhase := Phase;
      if (Phase = dipAssetFlush) or (Phase = dipAssetReplace) then
        SourceWorkflowFaultTarget := TDocumentPath(AssetDestination)
      else
        SourceWorkflowFaultTarget := TDocumentPath(Destination);
      SourceWorkflowFaultCount := 0;
      SourceWorkflowWriteExternalBytes := False;
      DocumentIOPhaseObserver := @SourceWorkflowFaultObserver;
      Saved := Mode.SaveDocumentToPath(TDocumentPath(Destination), 'tikz',
        True, True, ErrorText);
      DocumentIOPhaseObserver := nil;
      Check(not Saved,
        PhaseName + ' fault unexpectedly published the Save As transaction');
      Check((SourceWorkflowFaultCount = 1) and
        (Pos('source-acceptance injected write phase', ErrorText) > 0),
        PhaseName + ' did not reach its single targeted production seam: ' +
        ErrorText);
      Check(SourceWorkflowReadBytes(Destination) = DestinationBytes,
        PhaseName + ' fault changed the previous destination bytes');
      Check(SourceWorkflowReadBytes(AssetDestination) = DestinationAsset,
        PhaseName + ' fault changed the previous linked asset bytes');
      Check(SourceWorkflowFileCount(CaseDirectory) = 2,
        PhaseName + ' fault left a staging or rollback file behind');
      Check(SourceWorkflowReadBytes(SourceFile) = OriginalBytes,
        PhaseName + ' Save As fault changed the accepted source bytes');
      Check(SourceWorkflowReadBytes(
        ExtractFilePath(SourceFile) + 'acceptance-image.png') = OriginalAsset,
        PhaseName + ' Save As fault changed the accepted linked asset');
      Check((Session.CodecContext = AcceptedCodecContext) and
        (Session.SourcePath = AcceptedSourcePath) and
        (Session.SourceFormatId = 'tikz') and Session.CanSaveBack,
        PhaseName + ' fault changed accepted source identity or context');
      Check((Session.LocalRevision = AcceptedLocalRevision) and
        (Session.WatchGeneration = AcceptedWatchGeneration) and
        SameDiskRevision(Session.AcceptedRevision, AcceptedRevision) and
        (Session.OriginalSourceBytes = AcceptedBytes) and
        (Session.RecoveryBackupPath = AcceptedRecoveryPath),
        PhaseName + ' fault changed accepted session revisions or bytes');
      Check(MainForm.TheDrawing.History.IsChanged and
        (MainForm.TheDrawing.History.CanUndo = AcceptedUndo) and
        (MainForm.TheDrawing.History.CanRedo = AcceptedRedo),
        PhaseName + ' fault changed live dirty/undo state');
      SourceWorkflowCheckNear(Line.Points[1].X, AcceptedEnd.X,
        PhaseName + ' fault changed live line x');
      SourceWorkflowCheckNear(Line.Points[1].Y, AcceptedEnd.Y,
        PhaseName + ' fault changed live line y');
      Check(Context.FindBindingForObjectID(Line.ID) = BindingIndex,
        PhaseName + ' fault changed the live TikZ object binding');
    end;

    ExternalBytes := OriginalBytes + #10 + '% concurrent writer';
    SourceWorkflowFaultPhase := dipPrimaryFlush;
    SourceWorkflowFaultTarget := TDocumentPath(SourceFile);
    SourceWorkflowFaultCount := 0;
    SourceWorkflowWriteExternalBytes := True;
    SourceWorkflowExternalBytes := ExternalBytes;
    DocumentIOPhaseObserver := @SourceWorkflowFaultObserver;
    Saved := Mode.SaveDocumentToPath(TDocumentPath(SourceFile), 'tikz',
      False, False, ErrorText);
    DocumentIOPhaseObserver := nil;
    Check(not Saved and (SourceWorkflowFaultCount = 1),
      'a concurrent source writer was not rejected after serialization');
    Check(Pos('changed on disk', ErrorText) > 0,
      'concurrent source writer did not report the revision conflict: ' +
      ErrorText);
    Check(SourceWorkflowReadBytes(SourceFile) = ExternalBytes,
      'concurrent external source bytes were overwritten');
    Check(SourceWorkflowReadBytes(
      ExtractFilePath(SourceFile) + 'acceptance-image.png') = OriginalAsset,
      'source conflict changed the accepted linked asset');
    Check((Session.CodecContext = AcceptedCodecContext) and
      (Session.SourcePath = AcceptedSourcePath) and
      SameDiskRevision(Session.AcceptedRevision, AcceptedRevision) and
      (Session.OriginalSourceBytes = AcceptedBytes) and
      (Session.LocalRevision = AcceptedLocalRevision) and
      (Session.WatchGeneration = AcceptedWatchGeneration),
      'observed source conflict changed the accepted session');
    Check(MainForm.TheDrawing.History.IsChanged and
      (MainForm.TheDrawing.History.CanUndo = AcceptedUndo) and
      (MainForm.TheDrawing.History.CanRedo = AcceptedRedo),
      'observed source conflict changed dirty/undo state');
    SourceWorkflowCheckNear(Line.Points[1].X, AcceptedEnd.X,
      'source conflict changed live line x');
    SourceWorkflowCheckNear(Line.Points[1].Y, AcceptedEnd.Y,
      'source conflict changed live line y');
    Check(SourceWorkflowFileCount(ExtractFilePath(SourceFile)) = 2,
      'source conflict left a staged file beside the source');
  finally
    DocumentIOPhaseObserver := PreviousObserver;
    SourceWorkflowWriteExternalBytes := OldWriteExternalBytes;
    SourceWorkflowExternalBytes := PreviousExternalBytes;
  end;
end;
