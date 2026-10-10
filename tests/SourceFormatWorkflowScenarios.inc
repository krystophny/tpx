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
  SourceWorkflowCheckNear(Line.LineWidth, 0.75, 'TikZ line width');
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
  SourceWorkflowCheckNear(Rectangle.LineWidth, 0.5, 'TikZ rectangle line width');
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
  SourceWorkflowCheckNear(Primitive.Points[1].X, 16, 'TikZ image width');
  SourceWorkflowCheckNear(Primitive.Points[1].Y, 14, 'TikZ image height');
end;

procedure TestSourceFormatTikZWorkflow;
const
  LineMarker = '\path[line width=0.75mm,draw=L,dash pattern=on 2mm off 1mm] (1,0) -- (';
var
  FileName, AfterEditName, AfterStructuralName, ErrorText: string;
  Mode: TTpXMode;
  Session: TDocumentSession;
  Line: TPolyline2D;
  Obj: TObject2D;
  BeforeBytes, AfterBytes, SecondSaveBytes: RawByteString;
  BeforeEnd, ExpectedEnd: TPoint2D;
  StartAt, EndAt, NewStartAt, NewEndAt: SizeInt;
  Saved: Boolean;
begin
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
  SourceWorkflowWriteBytes(AfterStructuralName, SecondSaveBytes);
end;

procedure TestSourceFormatSVGWorkflow;
var
  FileName, AfterEditName, ErrorText: string;
  Mode: TTpXMode;
  Session: TDocumentSession;
  Line: TPolyline2D;
  Cubic: TBezierPath2D;
  TextObject: TText2D;
  Obj: TObject2D;
  AfterBytes: RawByteString;
  BeforeEnd, ExpectedEnd: TPoint2D;
  Saved: Boolean;
begin
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
  Check(Obj is TPolyline2D, 'SVG line did not materialize as TPolyline2D');
  Line := Obj as TPolyline2D;
  Check(Line.Points.Count = 2, 'SVG line point count changed');
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
  Check((TextObject.Text = 'Label ' + UTF8Encode(WideChar($03B2))) and
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
  Line := SourceWorkflowObjectAt(MainForm.TheDrawing, 0) as TPolyline2D;
  SourceWorkflowCheckNear(Line.Points[0].X, 1, 'reloaded SVG line start x');
  SourceWorkflowCheckNear(Line.Points[0].Y, 2, 'reloaded SVG line start y');
  SourceWorkflowCheckNear(Line.Points[1].X, ExpectedEnd.X, 'reloaded SVG line end x');
  SourceWorkflowCheckNear(Line.Points[1].Y, ExpectedEnd.Y, 'reloaded SVG line end y');
end;
