procedure TestTikZImportNativeScene;
var
  FileName, FragmentFileName, RejectedFileName, ErrorText: string;
  Mode: TTpXMode;
  Context: TTikZImportContext;
  Binding: TTikZSourceBinding;
  Drawing: TDrawing2D;
  Primitive: TPrimitive2D;
  TextObject: TText2D;
  ImageObject: TBitmap2D;
  Point: TPoint2D;
  Angle: Double;
  I: Integer;
  OriginalObjectID: Integer;
  RejectedSource: RawByteString;
  RejectedStream: TFileStream;
  procedure Near(Actual, Expected: Double; const Description: string);
  begin
    Check(Abs(Actual - Expected) < 0.0001,
      Format('%s differs: expected %.9f, got %.9f',
        [Description, Expected, Actual]));
  end;
begin
  FileName := GetEnvironmentVariable('TPX_TIKZ_FIXTURE');
  Check((FileName <> '') and FileExists(FileName),
    'TPX_TIKZ_FIXTURE must name the copied independent TeX scene');
  Check(IsDocumentCodecRegistered('tikz'),
    'The TikZ loader was not registered at startup');
  Check(MainForm.EventManager.Mode is TTpXMode,
    'The normal drawing mode is not active');
  Mode := MainForm.EventManager.Mode as TTpXMode;
  Check(Mode.OpenDocumentFromPath(FileName, ErrorText),
    'Registered TeX document open failed: ' + ErrorText);

  Drawing := MainForm.TheDrawing;
  Check(Drawing.ObjectsCount = 6,
    'The independent TeX scene did not produce six native objects');
  Check(Drawing.GetObject(0) is TPolyline2D,
    'The first native object is not the styled line');
  Primitive := Drawing.GetObject(0) as TPrimitive2D;
  Check((Primitive.LineStyle = liDashed) and
    (Primitive.LineColor = RGBToColor(51, 102, 153)),
    'The imported line lost its dash or RGB stroke');
  Near(Primitive.LineWidth * Drawing.LineWidthBase, 0.75,
    'line width (mm)');
  Point := Primitive.Points[0];
  Near(Point.X, 7.7712094323, 'transformed line start x (mm)');
  Near(Point.Y, 2.3657584558, 'transformed line start y (mm)');
  Point := Primitive.Points[1];
  Near(Point.X, 11.6763934607, 'transformed line end x (mm)');
  Near(Point.Y, 3.2315169115, 'transformed line end y (mm)');

  Check(Drawing.GetObject(1) is TBezierPath2D,
    'The second native object is not a cubic Bezier');
  Primitive := Drawing.GetObject(1) as TPrimitive2D;
  Point := Primitive.Points[1];
  Near(Point.X, 7.5547698183, 'transformed cubic control 1 x (mm)');
  Near(Point.Y, 3.3420544629, 'transformed cubic control 1 y (mm)');
  Point := Primitive.Points[3];
  Near(Point.X, 11.2435142329, 'transformed cubic end x (mm)');
  Near(Point.Y, 5.1841089257, 'transformed cubic end y (mm)');

  Check(Drawing.GetObject(2) is TRectangle2D,
    'The third native object is not a rectangle');
  Primitive := Drawing.GetObject(2) as TPrimitive2D;
  Angle := DegToRad(17.5);
  Check(Primitive.FillColor = RGBToColor(221, 238, 255),
    'The rotated rectangle lost its fill RGB');
  Near(Primitive.LineWidth * Drawing.LineWidthBase, 0.5,
    'rectangle outline width (mm)');
  Point := Primitive.Points[0];
  Near(Point.X, 10.25, 'rectangle first corner x (mm)');
  Near(Point.Y, 5.5, 'rectangle first corner y (mm)');
  Point := Primitive.Points[1];
  Near(Point.X, 10.25 + 8.5 * Cos(Angle) - 4.25 * Sin(Angle),
    'rectangle rotated corner x (mm)');
  Near(Point.Y, 5.5 + 8.5 * Sin(Angle) + 4.25 * Cos(Angle),
    'rectangle rotated corner y (mm)');

  Check(Drawing.GetObject(3) is TText2D,
    'The fourth native object is not text');
  TextObject := Drawing.GetObject(3) as TText2D;
  Check((TextObject.Text = 'Label β') and (TextObject.TeXText = '') and
    (TextObject.HAlignment = ahLeft),
    'Plain UTF-8 text or its west anchor changed during materialization');
  Check(Drawing.GetObject(4) is TText2D,
    'The fifth native object is not text');
  TextObject := Drawing.GetObject(4) as TText2D;
  Check((TextObject.Text = '$x^2$') and (TextObject.TeXText = '$x^2$') and
    (TextObject.HAlignment = ahRight),
    'The TeX text body or east anchor changed during materialization');

  Check(Drawing.GetObject(5) is TBitmap2D,
    'The sixth native object is not the local image');
  ImageObject := Drawing.GetObject(5) as TBitmap2D;
  Check((ImageObject.BitmapEntry <> nil) and
    (ImageObject.BitmapEntry.GetOnlyName = 'acceptance-image') and
    (ImageObject.BitmapEntry.ImageLink = 'acceptance-image.png') and
    SameFileName(ImageObject.BitmapEntry.GetFullLink,
      ExpandFileName(ExtractFilePath(FileName) + 'acceptance-image.png')),
    'The permitted local image was not linked');
  Near(ImageObject.Points[0].X, 12, 'image x (mm)');
  Near(ImageObject.Points[0].Y, 11, 'image y (mm)');
  Near(ImageObject.Points[2].X, 16, 'image width (mm)');
  Near(ImageObject.Points[2].Y, 14, 'image height (mm)');

  Context := Mode.TpX_Manager.DocumentSession.CodecContext as
    TTikZImportContext;
  Check(Context.BindingCount = 6,
    'The accepted source context did not retain all object bindings');
  for I := 0 to Context.BindingCount - 1 do
  begin
    Binding := Context.BindingAt(I);
    Check((Binding.ObjectIndex = I) and
      (Binding.NativeObjectID = Drawing.GetObject(I).ID),
      Format('Native ID binding %d no longer matches drawing order', [I]));
  end;

  OriginalObjectID := Drawing.GetObject(0).ID;
  RejectedFileName := ChangeFileExt(FileName, '.unsupported.tex');
  RejectedSource := '\begin{tikzpicture}' +
    '\draw (0,0)--(1,1);' +
    '\node at (0,0){\includegraphics[width=2mm,height=2mm]' +
    '{images/missing.png}};' +
    '\end{tikzpicture}';
  RejectedStream := TFileStream.Create(RejectedFileName, fmCreate);
  try
    RejectedStream.WriteBuffer(RejectedSource[1], Length(RejectedSource));
  finally
    RejectedStream.Free;
  end;
  Check(not Mode.OpenDocumentFromPath(RejectedFileName, ErrorText),
    'An unsupported nested asset reference must not be accepted as partial');
  Check((Pos('1:', ErrorText) > 0) and
    (Pos('permitted existing local asset', ErrorText) > 0),
    'Rejected image reference should include a source location and reason');
  Check((Drawing.ObjectsCount = 6) and
    (Drawing.GetObject(0).ID = OriginalObjectID),
    'A failed TikZ import changed the already-open drawing');

  FragmentFileName := GetEnvironmentVariable('TPX_TIKZ_FRAGMENT_FIXTURE');
  Check((FragmentFileName <> '') and FileExists(FragmentFileName),
    'TPX_TIKZ_FRAGMENT_FIXTURE must name a bare .tikz fragment');
  Check(Mode.OpenDocumentFromPath(FragmentFileName, ErrorText),
    'Registered bare-fragment open failed: ' + ErrorText);
  Check((MainForm.TheDrawing.ObjectsCount = 1) and
    (MainForm.TheDrawing.GetObject(0) is TPolyline2D),
    'A bare .tikz path did not open as one editable native polyline');
  Primitive := MainForm.TheDrawing.GetObject(0) as TPrimitive2D;
  Near(Primitive.Points[1].X, 20, 'bare-fragment end x (mm)');
  Near(Primitive.Points[1].Y, 10, 'bare-fragment end y (mm)');
end;
