function TikZSaveObjectAt(Drawing: TDrawing2D; const Index: Integer): TObject2D;
var I: Integer;
begin
  Result := Drawing.ObjectList.FirstObj as TObject2D;
  for I := 0 to Index - 1 do
    if Result <> nil then Result := Drawing.ObjectList.NextObj as TObject2D;
end;

function TikZSaveReadBytes(const FileName: string): RawByteString;
var Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Result, Stream.Size);
    if Stream.Size > 0 then Stream.ReadBuffer(Result[1], Stream.Size);
  finally
    Stream.Free;
  end;
end;

procedure TikZSaveWriteBytes(const FileName: string; const Bytes: RawByteString);
var Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    if Length(Bytes) > 0 then Stream.WriteBuffer(Bytes[1], Length(Bytes));
  finally
    Stream.Free;
  end;
end;

procedure TikZSaveNear(const Actual, Expected: TRealType; const LabelText: string);
begin
  Check(Abs(Actual - Expected) < 0.00002,
    LabelText + ': got ' + FloatToStr(Actual) + ', expected ' +
    FloatToStr(Expected));
end;

procedure TikZSaveDragPoint(Primitive: TPrimitive2D;
  const PointIndex, DX, DY: Integer);
var StartPoint, EndPoint: TPoint;
begin
  StartPoint := Point2DToPoint(MainForm.LocalView.ViewportToScreen(
    Primitive.Points[PointIndex]));
  EndPoint := Point(StartPoint.X + DX, StartPoint.Y + DY);
  MainForm.EventManager.MouseDown(nil, mbLeft, [], StartPoint.X, StartPoint.Y);
  MainForm.EventManager.MouseMove(nil, [ssLeft], EndPoint.X, EndPoint.Y);
  MainForm.EventManager.MouseUp(nil, mbLeft, [], EndPoint.X, EndPoint.Y);
end;

procedure TestTikZSourceSave;
const
  LineMarker = '\path[line width=0.75mm,draw=L,dash pattern=on 2mm off 1mm] (1,0) -- (';
var
  FileName, RelocatedFileName, FreshFileName, RejectedFileName,
    FreshTeXFileName, ErrorText: string;
  Mode: TTpXMode;
  Session: TDocumentSession;
  Drawing: TDrawing2D;
  Line: TPolyline2D;
  TextObject: TText2D;
  Rectangle: TRectangle2D;
  Unsupported: TEllipse2D;
  Obj: TObject2D;
  BeforeBytes, AfterBytes, NoOpBytes, Sentinel: RawByteString;
  BeforeEnd, ExpectedEnd: TPoint2D;
  StartAt, EndAt, NewStartAt, NewEndAt: SizeInt;
  Saved: Boolean;
begin
  FileName := GetEnvironmentVariable('TPX_TIKZ_SOURCE_FILE');
  RelocatedFileName := GetEnvironmentVariable('TPX_TIKZ_RELOCATED_FILE');
  FreshFileName := GetEnvironmentVariable('TPX_TIKZ_FRESH_FILE');
  RejectedFileName := GetEnvironmentVariable('TPX_TIKZ_REJECTED_FILE');
  FreshTeXFileName := GetEnvironmentVariable('TPX_TIKZ_FRESH_TEX_FILE');
  Check((FileName <> '') and (RelocatedFileName <> '') and
    (FreshFileName <> '') and (RejectedFileName <> '') and
    (FreshTeXFileName <> ''), 'TikZ source-save paths are not configured');
  RegisterTikZDocumentCodec;
  Mode := MainForm.EventManager.Mode as TTpXMode;
  Check(Mode.OpenDocumentFromPath(TDocumentPath(FileName), ErrorText),
    'Production open rejected TikZ fixture: ' + ErrorText);
  Session := Mode.TpX_Manager.DocumentSession;
  Drawing := Mode.Drawing;
  Check((Session.SourceFormatId = 'tikz') and Session.CanSaveBack and
    (Drawing.ObjectsCount = 6),
    'Opened TikZ source lacks its save-back context or native objects');
  Line := TikZSaveObjectAt(Drawing, 0) as TPolyline2D;
  TikZSaveNear(Line.Points[1].X, 11.6763935, 'nested line endpoint x');
  TikZSaveNear(Line.Points[1].Y, 3.2315169, 'nested line endpoint y');

  BeforeBytes := TikZSaveReadBytes(FileName);
  StartAt := Pos(LineMarker, string(BeforeBytes));
  Check(StartAt > 0, 'Nested-scope line source statement is missing');
  EndAt := StartAt + Pos(';', Copy(string(BeforeBytes), StartAt, MaxInt)) - 1;
  Check(EndAt >= StartAt, 'Nested-scope line terminator is missing');
  BeforeEnd := Line.Points[1];
  Drawing.SelectionAdd(Line);
  TikZSaveDragPoint(Line, 1, 14, -8);
  ExpectedEnd := Line.Points[1];
  Check((Abs(ExpectedEnd.X - BeforeEnd.X) > 0.05) and
    (Abs(ExpectedEnd.Y - BeforeEnd.Y) > 0.02),
    'Native endpoint drag did not change the source-bound object');
  Saved := Mode.SaveDocumentToPath(TDocumentPath(FileName), 'tikz', False,
    False, ErrorText);
  Check(Saved, 'Same-format TikZ save failed: ' + ErrorText);
  AfterBytes := TikZSaveReadBytes(FileName);
  NewStartAt := Pos(LineMarker, string(AfterBytes));
  Check(NewStartAt > 0, 'Edited TikZ statement was removed');
  NewEndAt := NewStartAt +
    Pos(';', Copy(string(AfterBytes), NewStartAt, MaxInt)) - 1;
  Check((NewEndAt >= NewStartAt) and
    (Copy(BeforeBytes, 1, StartAt - 1) =
      Copy(AfterBytes, 1, NewStartAt - 1)) and
    (Copy(BeforeBytes, EndAt + 1, MaxInt) =
      Copy(AfterBytes, NewEndAt + 1, MaxInt)),
    'A local TikZ edit changed source bytes outside its drawing statement');
  Check(not Drawing.History.IsChanged,
    'Successful same-format TikZ save left the drawing dirty');

  Check(Mode.SaveDocumentToPath(TDocumentPath(RelocatedFileName), 'tikz',
    True, True, ErrorText), 'TikZ Save As with a local image failed: ' + ErrorText);
  Check(FileExists(RelocatedFileName) and
    FileExists(ExtractFilePath(RelocatedFileName) + 'acceptance-image.png'),
    'TikZ Save As did not publish the relative image beside the source');
  Check(Mode.OpenDocumentFromPath(TDocumentPath(RelocatedFileName), ErrorText),
    'Reopening relocated TikZ source failed: ' + ErrorText);
  Check(Mode.Drawing.ObjectsCount = 6,
    'Relocated TikZ source lost an object or local image');
  Line := TikZSaveObjectAt(Mode.Drawing, 0) as TPolyline2D;
  TikZSaveNear(Line.Points[1].X, ExpectedEnd.X, 'reloaded edited line x');
  TikZSaveNear(Line.Points[1].Y, ExpectedEnd.Y, 'reloaded edited line y');

  Obj := TikZSaveObjectAt(Mode.Drawing, 5);
  Check(Obj is TBitmap2D, 'Expected the local image before structural edits');
  Mode.Drawing.SelectionAdd(Obj);
  MainForm.UserEventExecute(MainForm.DeleteSelected);
  MainForm.EventManager.SendMessage(Msg_InsertLine, nil);
  MainForm.EventManager.MouseDown(nil, mbLeft, [], 80, 100);
  MainForm.EventManager.MouseUp(nil, mbLeft, [], 80, 100);
  MainForm.EventManager.MouseMove(nil, [], 180, 130);
  MainForm.EventManager.MouseDown(nil, mbLeft, [], 180, 130);
  MainForm.EventManager.MouseUp(nil, mbLeft, [], 180, 130);
  Check(Mode.Drawing.ObjectsCount = 6,
    'Deleting an image and drawing a line changed the object count');
  Check(Mode.SaveDocumentToPath(TDocumentPath(RelocatedFileName), 'tikz',
    False, False, ErrorText), 'TikZ delete-and-append save failed: ' + ErrorText);
  AfterBytes := TikZSaveReadBytes(RelocatedFileName);
  Check(Pos('includegraphics', string(AfterBytes)) = 0,
    'Deleted image source statement remained after save');
  Check(Mode.OpenDocumentFromPath(TDocumentPath(RelocatedFileName), ErrorText),
    'Reopening TikZ after structural edits failed: ' + ErrorText);
  Check((Mode.Drawing.ObjectsCount = 6) and
    (TikZSaveObjectAt(Mode.Drawing, 5) is TPolyline2D),
    'Appended native line did not survive source reload');
  NoOpBytes := TikZSaveReadBytes(RelocatedFileName);
  Check(Mode.SaveDocumentToPath(TDocumentPath(RelocatedFileName), 'tikz',
    False, False, ErrorText), 'No-op TikZ save after rebinding failed: ' + ErrorText);
  Check(TikZSaveReadBytes(RelocatedFileName) = NoOpBytes,
    'No-op save after object-ID rebinding changed TikZ source bytes');

  Drawing := Mode.Drawing;
  Session.Clear;
  Drawing.Clear;
  Drawing.LineWidthBase := 0.5;
  Line := TPolyline2D.CreateSpec(-1,
    [Point2D(1.125, 2.375), Point2D(5.625, 8.875)]);
  Line.LineStyle := liSolid;
  Line.LineColor := clBlack;
  Drawing.AddObject(-1, Line);
  Rectangle := TRectangle2D.Create(-1);
  Rectangle.Points[0] := Point2D(10.125, 12.375);
  Rectangle.Points[1] := Point2D(14.75, 18.625);
  Rectangle.Points[2] := Point2D(10.125, 18.625);
  Rectangle.LineStyle := liSolid;
  Rectangle.LineColor := clBlack;
  Rectangle.FillColor := clBlue;
  Drawing.AddObject(-1, Rectangle);
  TextObject := TText2D.CreateSpec(-1, Point2D(2.75, 3.125),
    Drawing.DefaultFontHeight, '$x^2$');
  TextObject.TeXText := '$x^2$';
  TextObject.Rot := 0.37;
  Drawing.AddObject(-1, TextObject);

  Sentinel := 'preserve unsupported Save As target';
  TikZSaveWriteBytes(RejectedFileName, Sentinel);
  Unsupported := TEllipse2D.Create(-1);
  Drawing.AddObject(-1, Unsupported);
  Check(not Mode.SaveDocumentToPath(TDocumentPath(RejectedFileName), 'tikz',
    True, True, ErrorText), 'Unsupported native object unexpectedly saved as TikZ');
  Check((TikZSaveReadBytes(RejectedFileName) = Sentinel) and
    (Session.SourceFormatId = ''),
    'Rejected fresh TikZ Save As changed its destination or source session');
  Drawing.DeleteObject(Unsupported.ID);
  Check(Drawing.ObjectsCount = 3,
    'Removing the unsupported ellipse changed the wrong native objects');
  Saved := Mode.SaveDocumentToPath(TDocumentPath(FreshFileName), 'tikz', True,
    True, ErrorText);
  Check(Saved, 'Fresh native drawing could not Save As TikZ: ' + ErrorText);
  Check(Mode.OpenDocumentFromPath(TDocumentPath(FreshFileName), ErrorText),
    'Fresh TikZ document could not reload: ' + ErrorText);
  Check(Mode.Drawing.ObjectsCount = 3,
    'Fresh TikZ Save As changed its three native objects');
  Line := TikZSaveObjectAt(Mode.Drawing, 0) as TPolyline2D;
  TikZSaveNear(Line.Points[0].X, 1.125, 'fresh line start x');
  TikZSaveNear(Line.Points[0].Y, 2.375, 'fresh line start y');
  TikZSaveNear(Line.Points[1].X, 5.625, 'fresh line end x');
  TikZSaveNear(Line.Points[1].Y, 8.875, 'fresh line end y');
  Rectangle := TikZSaveObjectAt(Mode.Drawing, 1) as TRectangle2D;
  TikZSaveNear(Abs(Rectangle.Points[1].X - Rectangle.Points[0].X),
    4.625, 'fresh rectangle width');
  TikZSaveNear(Abs(Rectangle.Points[1].Y - Rectangle.Points[0].Y),
    6.25, 'fresh rectangle height');
  TextObject := TikZSaveObjectAt(Mode.Drawing, 2) as TText2D;
  Check(TextObject.TeXText = '$x^2$',
    'Fresh TikZ save changed the raw TeX math body');
  TikZSaveNear(TextObject.Rot, 0.37, 'fresh text rotation');
  NoOpBytes := TikZSaveReadBytes(FreshFileName);
  Check(Mode.SaveDocumentToPath(TDocumentPath(FreshFileName), 'tikz', False,
    False, ErrorText), 'Fresh TikZ second save failed: ' + ErrorText);
  Check(TikZSaveReadBytes(FreshFileName) = NoOpBytes,
    'Fresh TikZ second save changed bytes without an edit');

  Session.Clear;
  Drawing := Mode.Drawing;
  Drawing.Clear;
  Line := TPolyline2D.CreateSpec(-1,
    [Point2D(0.625, 1.75), Point2D(3.375, 4.125)]);
  Line.LineStyle := liSolid;
  Drawing.AddObject(-1, Line);
  Check(Mode.SaveDocumentToPath(TDocumentPath(FreshTeXFileName), 'tikz',
    True, True, ErrorText), 'Fresh native drawing could not Save As TeX: ' + ErrorText);
  Check(Mode.OpenDocumentFromPath(TDocumentPath(FreshTeXFileName), ErrorText),
    'Fresh standalone TeX document could not reload: ' + ErrorText);
  Line := TikZSaveObjectAt(Mode.Drawing, 0) as TPolyline2D;
  TikZSaveNear(Line.Points[0].X, 0.625, 'fresh TeX line start x');
  TikZSaveNear(Line.Points[1].Y, 4.125, 'fresh TeX line end y');
end;
