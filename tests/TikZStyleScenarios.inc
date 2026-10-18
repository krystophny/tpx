function TikZStyleObject(Drawing: TDrawing2D; Index: Integer): TPrimitive2D;
begin
  Result := TikZSaveObjectAt(Drawing, Index) as TPrimitive2D;
end;

function TikZStyleSave(Mode: TTpXMode; const FileName: string;
  SaveAs: Boolean): string;
begin
  Result := '';
  if not Mode.SaveDocumentToPath(TDocumentPath(FileName), 'tikz', SaveAs,
    SaveAs, Result) and (Result = '') then
    Result := 'save failed without a message';
end;

procedure TikZStyleOpen(Mode: TTpXMode; const FileName: string);
var ErrorText: string;
begin
  Check(Mode.OpenDocumentFromPath(TDocumentPath(FileName), ErrorText),
    'Reopening ' + FileName + ' failed: ' + ErrorText);
end;

function TikZStyleDark(Frame: TBitmap; X, Y: Integer): Boolean;
var C: TColor;
begin
  C := ColorToRGB(Frame.Canvas.Pixels[X, Y]);
  Result := (Red(C) < 96) and (Green(C) < 96) and (Blue(C) < 96);
end;

{ Scan the center row of a rendered horizontal stroke. Dashes are the dark
  runs; a hollow column is dark beside the center but light on it, which is
  how an inverted thin overlay inside a wide stroke shows up. }
procedure TikZStyleScanStroke(Frame: TBitmap; Y, X0, X1: Integer;
  out DarkColumns, Runs, HollowColumns: Integer);
var X: Integer; Dark, WasDark: Boolean;
begin
  DarkColumns := 0;
  Runs := 0;
  HollowColumns := 0;
  WasDark := False;
  for X := X0 to X1 do
  begin
    Dark := TikZStyleDark(Frame, X, Y);
    if Dark then Inc(DarkColumns);
    if Dark and not WasDark then Inc(Runs);
    if not Dark and TikZStyleDark(Frame, X, Y - 2) and
      TikZStyleDark(Frame, X, Y + 2) then Inc(HollowColumns);
    WasDark := Dark;
  end;
end;

procedure TikZStyleRender(View: TViewport2D; Frame: TBitmap;
  Primitive: TPrimitive2D; Style: TLineStyle; Y, X0, X1: Integer;
  out DarkColumns, Runs, HollowColumns: Integer);
begin
  Primitive.LineStyle := Style;
  View.Repaint;
  PumpEvents;
  View.RenderToCanvas(Frame.Canvas);
  TikZStyleScanStroke(Frame, Y, X0, X1, DarkColumns, Runs, HollowColumns);
end;

procedure TestTikZStyleRoundTrip;
var
  Directory, SolidFile, StyleFile, ExampleFile, ErrorText, ExpectedLabel: string;
  Mode: TTpXMode;
  Drawing: TDrawing2D;
  Line: TLine2D;
  Primitive: TPrimitive2D;
  Frame: TBitmap;
  View: TViewport2D;
  Item, ExampleMenu: TMenuItem;
  P0, P1: TPoint2D;
  Count, I, Y, X0, X1, Dark, Runs, Hollow: Integer;
  Style: TLineStyle;
  Styles: array of TLineStyle;
  Colors: array of TColor;
  Fills: array of TColor;
  BeginArrows, EndArrows: array of TArrowKind;
  Classes: array of string;
  Radii, StartAngles, EndAngles: array of TRealType;
  NoOpBytes, Bytes: RawByteString;
begin
  Directory := GetEnvironmentVariable('TPX_TIKZ_STYLE_DIR');
  Check(Directory <> '', 'TikZ style round-trip directory is not configured');
  Directory := IncludeTrailingPathDelimiter(Directory);
  SolidFile := Directory + 'simple_line.tikz';
  StyleFile := Directory + 'dashed_line.tikz';
  ExampleFile := Directory + 'double_slit.tikz';
  RegisterTikZDocumentCodec;
  Mode := MainForm.EventManager.Mode as TTpXMode;

  { A solid line saved by TpX must reopen as one solid stroke. }
  Mode.TpX_Manager.DocumentSession.Clear;
  Drawing := Mode.Drawing;
  Drawing.Clear;
  Line := TLine2D.CreateSpec(-1, Point2D(14.4, 46.2), Point2D(27.5, 46.2));
  Line.LineStyle := liSolid;
  Line.LineWidth := 0.3 / Drawing.LineWidthBase;
  Drawing.AddObject(-1, Line);
  ErrorText := TikZStyleSave(Mode, SolidFile, True);
  Check(ErrorText = '', 'Solid line could not Save As TikZ: ' + ErrorText);
  TikZStyleOpen(Mode, SolidFile);
  Drawing := Mode.Drawing;
  Check(Drawing.ObjectsCount = 1, 'Reopened solid line changed its object count');
  Primitive := TikZStyleObject(Drawing, 0);
  Check(Primitive.LineStyle = liSolid, Format(
    'Reopened solid TikZ line has line style %d', [Ord(Primitive.LineStyle)]));
  TikZSaveNear(Primitive.LineWidth * Drawing.LineWidthBase, 0.3,
    'reopened solid line width (mm)');
  View := MainForm.LocalView;
  Frame := TBitmap.Create;
  try
    MainForm.SetBounds(20, 20, 900, 700);
    MainForm.Show;
    PumpEvents;
    View.ShowGrid := False;
    View.ShowCrossHair := False;
    View.VisualRect := Rect2D(12, 40, 30, 52);
    Frame.SetSize(View.ClientWidth, View.ClientHeight);
    View.RenderToCanvas(Frame.Canvas);
    P0 := View.ViewportToScreen(Primitive.Points[0]);
    P1 := View.ViewportToScreen(Primitive.Points[1]);
    Y := Round(P0.Y);
    X0 := Round(Min(P0.X, P1.X)) + 6;
    X1 := Round(Max(P0.X, P1.X)) - 6;
    Check((X1 - X0 > 40) and (Y > 2) and (Y < Frame.Height - 3),
      Format('Solid line is not visible in the rendered viewport: ' +
      'x0=%d x1=%d y=%d frame=%dx%d', [X0, X1, Y, Frame.Width, Frame.Height]));
    TikZStyleScanStroke(Frame, Y, X0, X1, Dark, Runs, Hollow);
    Check((Dark = X1 - X0 + 1) and (Hollow = 0), Format(
      'Reopened solid line renders with gaps: %d of %d columns dark',
      [Dark, X1 - X0 + 1]));
    { Wide dashed and dotted strokes must show real gaps without the former
      inverted one-pixel overlay that drew them as hollow beads. }
    for Style in [liDashed, liDotted] do
    begin
      TikZStyleRender(View, Frame, Primitive, Style, Y, X0, X1, Dark, Runs,
        Hollow);
      Check((Runs >= 3) and (Dark < X1 - X0 + 1) and (Hollow = 0), Format(
        'Wide line style %d renders %d dashes, %d dark and %d hollow columns',
        [Ord(Style), Runs, Dark, Hollow]));
    end;
    TikZStyleRender(View, Frame, Primitive, liSolid, Y, X0, X1, Dark, Runs,
      Hollow);
    Check((Dark = X1 - X0 + 1) and (Hollow = 0),
      'A solid line renders with gaps after drawing patterned strokes');
  finally
    Frame.Free;
  end;

  { Dashed and dotted lines on a fresh drawing save as TikZ, reopen with the
    same style, and accept line-pattern edits as source-preserving saves. }
  Mode.TpX_Manager.DocumentSession.Clear;
  Drawing := Mode.Drawing;
  Drawing.Clear;
  Line := TLine2D.CreateSpec(-1, Point2D(0, 0), Point2D(20, 0));
  Line.LineStyle := liDashed;
  Drawing.AddObject(-1, Line);
  Line := TLine2D.CreateSpec(-1, Point2D(0, 5), Point2D(20, 5));
  Line.LineStyle := liDotted;
  Drawing.AddObject(-1, Line);
  Line := TLine2D.CreateSpec(-1, Point2D(0, 10), Point2D(20, 10));
  Line.LineStyle := liSolid;
  Drawing.AddObject(-1, Line);
  ErrorText := TikZStyleSave(Mode, StyleFile, True);
  Check(ErrorText = '', 'Dashed/dotted lines could not Save As TikZ: ' + ErrorText);
  TikZStyleOpen(Mode, StyleFile);
  Drawing := Mode.Drawing;
  Check((Drawing.ObjectsCount = 3) and
    (TikZStyleObject(Drawing, 0).LineStyle = liDashed) and
    (TikZStyleObject(Drawing, 1).LineStyle = liDotted) and
    (TikZStyleObject(Drawing, 2).LineStyle = liSolid),
    'Fresh dashed/dotted/solid lines did not reopen with their styles');
  NoOpBytes := TikZSaveReadBytes(StyleFile);
  ErrorText := TikZStyleSave(Mode, StyleFile, False);
  Check(ErrorText = '', 'No-op save of styled TikZ lines failed: ' + ErrorText);
  Check(TikZSaveReadBytes(StyleFile) = NoOpBytes,
    'No-op save of styled TikZ lines changed the source');
  TikZStyleObject(Drawing, 0).LineStyle := liDotted;
  TikZStyleObject(Drawing, 1).LineStyle := liSolid;
  TikZStyleObject(Drawing, 2).LineStyle := liDashed;
  ErrorText := TikZStyleSave(Mode, StyleFile, False);
  Check(ErrorText = '', 'Changing TikZ line patterns could not be saved: ' +
    ErrorText);
  TikZStyleOpen(Mode, StyleFile);
  Drawing := Mode.Drawing;
  Check((Drawing.ObjectsCount = 3) and
    (TikZStyleObject(Drawing, 0).LineStyle = liDotted) and
    (TikZStyleObject(Drawing, 1).LineStyle = liSolid) and
    (TikZStyleObject(Drawing, 2).LineStyle = liDashed),
    'Edited TikZ line patterns did not reopen with their new styles');
  TikZStyleObject(Drawing, 0).LineStyle := liDashed;
  ErrorText := TikZStyleSave(Mode, StyleFile, False);
  Check(ErrorText = '', 'Second line-pattern edit could not be saved: ' +
    ErrorText);
  TikZStyleOpen(Mode, StyleFile);
  Check(TikZStyleObject(Mode.Drawing, 0).LineStyle = liDashed,
    'Second line-pattern edit did not reopen');

  { The bundled double-slit example saves as TikZ and reopens with the same
    objects, line styles, colors and arrowheads. }
  ExampleMenu := FindMenuCaption(MainForm.MainMenu1.Items, 'Open Example');
  Check(ExampleMenu <> nil, 'File menu has no Open Example submenu');
  Item := FindMenuCaption(ExampleMenu, 'Double-slit interference');
  Check(Item <> nil, 'Open Example is missing the double-slit drawing');
  Item.Click;
  Drawing := MainForm.TheDrawing;
  Count := Drawing.ObjectsCount;
  SetLength(Styles, Count);
  SetLength(Colors, Count);
  SetLength(Fills, Count);
  SetLength(BeginArrows, Count);
  SetLength(EndArrows, Count);
  SetLength(Classes, Count);
  SetLength(Radii, Count);
  SetLength(StartAngles, Count);
  SetLength(EndAngles, Count);
  for I := 0 to Count - 1 do
  begin
    Primitive := TikZStyleObject(Drawing, I);
    Styles[I] := Primitive.LineStyle;
    Colors[I] := Primitive.LineColor;
    Fills[I] := Primitive.FillColor;
    BeginArrows[I] := Primitive.BeginArrowKind;
    EndArrows[I] := Primitive.EndArrowKind;
    Classes[I] := Primitive.ClassName;
    if Primitive is TCircular2D then
    begin
      Radii[I] := TCircular2D(Primitive).Radius;
      StartAngles[I] := TCircular2D(Primitive).StartAngle;
      EndAngles[I] := TCircular2D(Primitive).EndAngle;
    end;
  end;
  ErrorText := TikZStyleSave(Mode, ExampleFile, True);
  Check(ErrorText = '', 'Double-slit example could not Save As TikZ: ' +
    ErrorText);
  TikZStyleOpen(Mode, ExampleFile);
  Drawing := Mode.Drawing;
  Check(Drawing.ObjectsCount = Count, Format(
    'Double-slit TikZ reopened with %d objects; expected %d',
    [Drawing.ObjectsCount, Count]));
  for I := 0 to Count - 1 do
  begin
    Primitive := TikZStyleObject(Drawing, I);
    { The importer materializes every two-point TikZ path as a polyline. }
    Check((Primitive.ClassName = Classes[I]) or
      ((Classes[I] = 'TLine2D') and (Primitive is TPolyline2D) and
      (Primitive.Points.Count = 2)), Format(
      'Double-slit object %d reopened as %s instead of %s',
      [I + 1, Primitive.ClassName, Classes[I]]));
    if Primitive is TText2D then Continue;
    Check(Primitive.LineStyle = Styles[I], Format(
      'Double-slit object %d reopened with line style %d instead of %d',
      [I + 1, Ord(Primitive.LineStyle), Ord(Styles[I])]));
    Check(ColorToRGB(Primitive.LineColor) = ColorToRGB(Colors[I]), Format(
      'Double-slit object %d reopened with stroke color %x instead of %x',
      [I + 1, ColorToRGB(Primitive.LineColor), ColorToRGB(Colors[I])]));
    Check((Primitive.FillColor = clDefault) = (Fills[I] = clDefault),
      Format('Double-slit object %d changed its fill state', [I + 1]));
    if Fills[I] <> clDefault then
      Check(ColorToRGB(Primitive.FillColor) = ColorToRGB(Fills[I]), Format(
        'Double-slit object %d reopened with fill color %x instead of %x',
        [I + 1, ColorToRGB(Primitive.FillColor), ColorToRGB(Fills[I])]));
    if Primitive is TCircular2D then
    begin
      TikZSaveNear(TCircular2D(Primitive).Radius, Radii[I],
        Format('double-slit arc %d radius', [I + 1]));
      TikZSaveNear(TCircular2D(Primitive).StartAngle, StartAngles[I],
        Format('double-slit arc %d start angle', [I + 1]));
      TikZSaveNear(TCircular2D(Primitive).EndAngle, EndAngles[I],
        Format('double-slit arc %d end angle', [I + 1]));
    end;
    Check((Primitive.BeginArrowKind = BeginArrows[I]) and
      (Primitive.EndArrowKind = EndArrows[I]), Format(
      'Double-slit object %d changed its arrowheads', [I + 1]));
  end;
  NoOpBytes := TikZSaveReadBytes(ExampleFile);
  ErrorText := TikZStyleSave(Mode, ExampleFile, False);
  Check(ErrorText = '', 'No-op save of the double-slit TikZ failed: ' +
    ErrorText);
  Check(TikZSaveReadBytes(ExampleFile) = NoOpBytes,
    'No-op save of the double-slit TikZ changed the source');

  { A style that has no TikZ source form names the object and property. }
  Primitive := nil;
  for I := 0 to Drawing.ObjectsCount - 1 do
    if (TikZStyleObject(Drawing, I) is TPolyline2D) and
      (TikZStyleObject(Drawing, I).EndArrowKind <> arrNone) then
    begin
      Primitive := TikZStyleObject(Drawing, I);
      Break;
    end;
  Check(Primitive <> nil, 'Double-slit TikZ has no arrowed line');
  Primitive.EndArrowKind := arrT10;
  Bytes := TikZSaveReadBytes(ExampleFile);
  ErrorText := TikZStyleSave(Mode, ExampleFile, False);
  ExpectedLabel := Format('object %d', [I + 1]);
  Check((ErrorText <> '') and (Pos(ExpectedLabel, LowerCase(ErrorText)) > 0) and
    (Pos('end arrow', LowerCase(ErrorText)) > 0),
    'Unrepresentable arrow message does not name the object and property: ' +
    ErrorText);
  Check(TikZSaveReadBytes(ExampleFile) = Bytes,
    'A rejected TikZ save changed the source file');
end;
