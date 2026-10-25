{ Fill and stroke opacity: inspector editing with undo, translucent screen
  rendering, and local TikZ source patches. }

function PaintStyleSave(Mode: TTpXMode; const FileName: string;
  SaveAs: Boolean): string;
begin
  Result := '';
  if not Mode.SaveDocumentToPath(TDocumentPath(FileName), 'tikz', SaveAs,
    SaveAs, Result) and (Result = '') then
    Result := 'save failed without a message';
end;

function PaintStylePixel(Frame: TBitmap; View: TViewport2D;
  const P: TPoint2D): TColor;
var S: TPoint2D;
begin
  S := View.ViewportToScreen(P);
  Result := ColorToRGB(Frame.Canvas.Pixels[Round(S.X), Round(S.Y)]);
end;

procedure CheckPaintChannel(const Actual, Expected: Integer;
  const LabelText: string);
begin
  Check(Abs(Actual - Expected) <= 8, Format('%s is %d, expected about %d',
    [LabelText, Actual, Expected]));
end;

procedure TestPaintOpacity;
var
  Drawing: TDrawing2D;
  Mode: TTpXMode;
  Band: TPolygon2D;
  Line: TLine2D;
  Prim: TPrimitive2D;
  View: TViewport2D;
  Frame: TBitmap;
  C: TColor;
  Edit: TEdit;
  ErrorText, Directory, FileName: string;
  Bytes, Expected: RawByteString;
  At: SizeInt;
begin
  SetLiveTeXEnabled(False);
  RegisterTikZDocumentCodec;
  Mode := MainForm.EventManager.Mode as TTpXMode;
  Mode.TpX_Manager.DocumentSession.Clear;
  MainForm.SetBounds(20, 20, 900, 700);
  MainForm.Show;
  MainForm.ShowInspector;
  PumpEvents;
  Drawing := Mode.Drawing;
  Drawing.Clear;

  { A black line under a yellow band at half fill opacity. }
  Line := TLine2D.CreateSpec(-1, Point2D(10, 20), Point2D(50, 20));
  Line.LineWidth := 4;
  Line.LineColor := clBlack;
  Drawing.AddObject(-1, Line);
  Band := TPolygon2D.CreateSpec(-1, [Point2D(5, 15), Point2D(55, 15),
    Point2D(55, 25), Point2D(5, 25)]);
  Band.LineStyle := liNone;
  Band.FillColor := clYellow;
  Drawing.AddObject(-1, Band);

  Drawing.SelectionClear;
  Drawing.SelectionAdd(Band);
  MainForm.RefreshInspectorSelection;
  MainForm.Inspector.ShowMore := True;
  MainForm.Inspector.FocusField(opfFillOpacity);
  Edit := MainForm.Inspector.EditorFor(opfFillOpacity) as TEdit;
  Check(Edit.Text = '1', 'An opaque fill shows opacity ' + Edit.Text);
  Edit.Text := '1.5';
  Check(not MainForm.Inspector.PrepareForSelectionChange(ErrorText),
    'An opacity above 1 was accepted');
  Check(Band.FillOpacity = 1, 'An invalid opacity draft changed the object');
  MainForm.Inspector.CancelActiveEdit;
  MainForm.Inspector.FocusField(opfFillOpacity);
  Edit.Text := '0.5';
  Check(MainForm.Inspector.PrepareForSelectionChange(ErrorText),
    'Fill opacity edit failed: ' + ErrorText);
  Band := TikZSaveObjectAt(Drawing, 1) as TPolygon2D;
  Check(Abs(Band.FillOpacity - 0.5) < 1E-6,
    'Inspector did not set the fill opacity');
  Drawing.History.Undo;
  Prim := TikZSaveObjectAt(Drawing, 1) as TPrimitive2D;
  Check(Prim.FillOpacity = 1, 'Undo did not restore the opaque fill');
  Drawing.History.Redo;
  Band := TikZSaveObjectAt(Drawing, 1) as TPolygon2D;
  Check(Abs(Band.FillOpacity - 0.5) < 1E-6,
    'Redo did not reapply the fill opacity');

  View := MainForm.LocalView;
  Frame := TBitmap.Create;
  try
    View.ShowGrid := False;
    View.ShowCrossHair := False;
    View.VisualRect := Rect2D(0, 0, 60, 40);
    View.Repaint;
    PumpEvents;
    Frame.SetSize(View.ClientWidth, View.ClientHeight);
    View.RenderToCanvas(Frame.Canvas);
    { Half yellow over white and over black. }
    C := PaintStylePixel(Frame, View, Point2D(30, 24));
    CheckPaintChannel(Red(C), 255, 'band over paper red');
    CheckPaintChannel(Green(C), 255, 'band over paper green');
    CheckPaintChannel(Blue(C), 128, 'band over paper blue');
    C := PaintStylePixel(Frame, View, Point2D(30, 20));
    CheckPaintChannel(Red(C), 128, 'band over line red');
    CheckPaintChannel(Green(C), 128, 'band over line green');
    CheckPaintChannel(Blue(C), 0, 'band over line blue');
    { A quarter-opaque stroke over paper. }
    Line := TikZSaveObjectAt(Drawing, 0) as TLine2D;
    Line.LineOpacity := 0.25;
    Band.FillOpacity := 0;
    View.Repaint;
    PumpEvents;
    View.RenderToCanvas(Frame.Canvas);
    C := PaintStylePixel(Frame, View, Point2D(30, 20));
    CheckPaintChannel(Red(C), 191, 'quarter stroke red');
    CheckPaintChannel(Blue(C), 191, 'quarter stroke blue');
    Line.LineOpacity := 1;
    Band.FillOpacity := 0.5;
  finally
    Frame.Free;
  end;

  { TikZ: a new document writes the opacity options; edits patch the
    existing option or insert a new one, leaving other bytes alone. }
  Directory := GetEnvironmentVariable('TPX_PAINT_STYLE_DIR');
  Check(Directory <> '', 'Paint style directory is not configured');
  FileName := IncludeTrailingPathDelimiter(Directory) + 'opacity.tikz';
  ErrorText := PaintStyleSave(Mode, FileName, True);
  Check(ErrorText = '', 'Save As TikZ failed: ' + ErrorText);
  Bytes := TikZSaveReadBytes(FileName);
  Check(Pos('fill opacity=0.5', Bytes) > 0,
    'New TikZ statement lost the fill opacity: ' + Bytes);
  Check(Pos('draw opacity', Bytes) = 0,
    'An opaque stroke wrote a draw opacity');
  Check(Mode.OpenDocumentFromPath(TDocumentPath(FileName), ErrorText),
    'Reopening the TikZ document failed: ' + ErrorText);
  Drawing := Mode.Drawing;
  { TikZ reopens the shapes as paths, so address them as primitives. }
  Prim := TikZSaveObjectAt(Drawing, 1) as TPrimitive2D;
  Check(Abs(Prim.FillOpacity - 0.5) < 1E-6,
    'Reopened TikZ band lost its fill opacity');
  Prim.FillOpacity := 0.25;
  Prim := TikZSaveObjectAt(Drawing, 0) as TPrimitive2D;
  Check(Prim.LineOpacity = 1, 'Reopened TikZ line is not opaque');
  Prim.LineOpacity := 0.75;
  ErrorText := PaintStyleSave(Mode, FileName, False);
  Check(ErrorText = '', 'Saving edited opacities failed: ' + ErrorText);
  Expected := Bytes;
  At := Pos('fill opacity=0.5', Expected);
  Delete(Expected, At, Length('fill opacity=0.5'));
  Insert('fill opacity=0.25', Expected, At);
  Bytes := TikZSaveReadBytes(FileName);
  At := Pos('draw opacity=0.75, ', Bytes);
  Check(At > 0, 'The new draw opacity was not inserted: ' + Bytes);
  Delete(Bytes, At, Length('draw opacity=0.75, '));
  Check(Bytes = Expected,
    'Opacity edits changed TikZ bytes outside their options: ' + Bytes);
end;

{ Gradient fills: inspector editing with undo and the canvas rendering of a
  radial fade and a linear blend. }
procedure TestPaintGradient;
var
  Drawing: TDrawing2D;
  Mode: TTpXMode;
  Disc: TCircle2D;
  Band: TPolygon2D;
  Prim: TPrimitive2D;
  View: TViewport2D;
  Frame: TBitmap;
  C: TColor;
  Combo: TComboBox;
  Edit: TEdit;
  ErrorText: string;
begin
  SetLiveTeXEnabled(False);
  Mode := MainForm.EventManager.Mode as TTpXMode;
  Mode.TpX_Manager.DocumentSession.Clear;
  MainForm.SetBounds(20, 20, 900, 700);
  MainForm.Show;
  MainForm.ShowInspector;
  PumpEvents;
  Drawing := Mode.Drawing;
  Drawing.Clear;
  Disc := TCircle2D.Create(-1);
  Disc.Points[0] := Point2D(15, 20);
  Disc.Points[1] := Point2D(15, 30);
  Disc.LineStyle := liNone;
  Disc.FillColor := clBlack;
  Drawing.AddObject(-1, Disc);
  Band := TPolygon2D.CreateSpec(-1, [Point2D(30, 15), Point2D(50, 15),
    Point2D(50, 25), Point2D(30, 25)]);
  Band.LineStyle := liNone;
  Band.FillColor := clRed;
  Band.FillGradient := fgLinear;
  Band.GradientColor := clBlue;
  Drawing.AddObject(-1, Band);

  Drawing.SelectionClear;
  Drawing.SelectionAdd(Disc);
  MainForm.RefreshInspectorSelection;
  MainForm.Inspector.ShowMore := True;
  MainForm.Inspector.FocusField(opfFillGradient);
  Combo := MainForm.Inspector.EditorFor(opfFillGradient) as TComboBox;
  Check(Combo.Text = 'Solid', 'A solid fill shows gradient ' + Combo.Text);
  Combo.ItemIndex := Ord(fgRadial);
  if Assigned(Combo.OnChange) then Combo.OnChange(Combo);
  Check(MainForm.Inspector.PrepareForSelectionChange(ErrorText),
    'Gradient kind edit failed: ' + ErrorText);
  MainForm.Inspector.FocusField(opfGradientOpacity);
  Edit := MainForm.Inspector.EditorFor(opfGradientOpacity) as TEdit;
  Edit.Text := '0';
  Check(MainForm.Inspector.PrepareForSelectionChange(ErrorText),
    'Gradient end opacity edit failed: ' + ErrorText);
  Prim := TikZSaveObjectAt(Drawing, 0) as TPrimitive2D;
  Check((Prim.FillGradient = fgRadial) and (Prim.GradientOpacity = 0),
    'Inspector did not set the radial fade');
  Drawing.History.Undo;
  Prim := TikZSaveObjectAt(Drawing, 0) as TPrimitive2D;
  Check((Prim.FillGradient = fgRadial) and (Prim.GradientOpacity = 1),
    'Undo did not restore only the end opacity');
  Drawing.History.Undo;
  Prim := TikZSaveObjectAt(Drawing, 0) as TPrimitive2D;
  Check(Prim.FillGradient = fgNone, 'Undo did not restore the solid fill');
  Drawing.History.Redo;
  Drawing.History.Redo;
  Prim := TikZSaveObjectAt(Drawing, 0) as TPrimitive2D;
  Check((Prim.FillGradient = fgRadial) and (Prim.GradientOpacity = 0),
    'Redo did not reapply the radial fade');

  View := MainForm.LocalView;
  Frame := TBitmap.Create;
  try
    View.ShowGrid := False;
    View.ShowCrossHair := False;
    View.VisualRect := Rect2D(0, 0, 60, 40);
    View.Repaint;
    PumpEvents;
    Frame.SetSize(View.ClientWidth, View.ClientHeight);
    View.RenderToCanvas(Frame.Canvas);
    { Opaque black core, half way grey, nearly clear rim. }
    C := PaintStylePixel(Frame, View, Point2D(15, 20));
    CheckPaintChannel(Red(C), 0, 'disc centre');
    C := PaintStylePixel(Frame, View, Point2D(20, 20));
    CheckPaintChannel(Red(C), 128, 'disc half radius');
    C := PaintStylePixel(Frame, View, Point2D(24.6, 20));
    Check(Red(C) > 225, Format('disc rim is %d, expected nearly white',
      [Red(C)]));
    { Red at the left end, blue at the right end, purple in the middle. }
    C := PaintStylePixel(Frame, View, Point2D(30.3, 20));
    Check((Red(C) > 240) and (Blue(C) < 16), 'linear start is not red');
    C := PaintStylePixel(Frame, View, Point2D(49.7, 20));
    Check((Blue(C) > 240) and (Red(C) < 16), 'linear end is not blue');
    C := PaintStylePixel(Frame, View, Point2D(40, 20));
    CheckPaintChannel(Red(C), 128, 'linear middle red');
    CheckPaintChannel(Blue(C), 128, 'linear middle blue');
  finally
    Frame.Free;
  end;
end;

{ A translucent stroke and its arrowhead composite once, as one layer, so
  the head over the stroke has the colour of the stroke alone. }
procedure TestPaintArrowOpacity;
var
  Drawing: TDrawing2D;
  Mode: TTpXMode;
  Line: TLine2D;
  View: TViewport2D;
  Frame: TBitmap;
  Stroke, Overlap, Head: TColor;
begin
  SetLiveTeXEnabled(False);
  Mode := MainForm.EventManager.Mode as TTpXMode;
  Mode.TpX_Manager.DocumentSession.Clear;
  MainForm.SetBounds(20, 20, 900, 700);
  MainForm.Show;
  PumpEvents;
  Drawing := Mode.Drawing;
  Drawing.Clear;
  { The head spans x = 41.6..50 and is 2.1 wide each side at its base;
    the stroke is 0.6 wide each side. }
  Line := TLine2D.CreateSpec(-1, Point2D(10, 20), Point2D(50, 20));
  Line.LineWidth := 4;
  Line.LineColor := clRed;
  Line.LineOpacity := 0.5;
  Line.EndArrowKind := arrH44;
  Line.ArrowSizeFactor := 3;
  Drawing.AddObject(-1, Line);

  View := MainForm.LocalView;
  Frame := TBitmap.Create;
  try
    View.ShowGrid := False;
    View.ShowCrossHair := False;
    View.VisualRect := Rect2D(0, 0, 60, 40);
    View.Repaint;
    PumpEvents;
    Frame.SetSize(View.ClientWidth, View.ClientHeight);
    View.RenderToCanvas(Frame.Canvas);
    Stroke := PaintStylePixel(Frame, View, Point2D(30, 20));
    Overlap := PaintStylePixel(Frame, View, Point2D(46, 20));
    Head := PaintStylePixel(Frame, View, Point2D(44, 21.1));
    CheckPaintChannel(Red(Stroke), 255, 'half red stroke red');
    CheckPaintChannel(Green(Stroke), 128, 'half red stroke green');
    CheckPaintChannel(Blue(Stroke), 128, 'half red stroke blue');
    Check(Overlap = Stroke, Format(
      'arrowhead over the stroke is %.6x, the stroke alone %.6x',
      [Overlap, Stroke]));
    Check(Head = Stroke, Format(
      'arrowhead beside the stroke is %.6x, the stroke alone %.6x',
      [Head, Stroke]));
  finally
    Frame.Free;
  end;
end;

{ A TikZ file with TpX's own fadings (the export of four gradients whose
  ends differ in opacity) reopens with one object per gradient, saves
  unchanged as a no-op, and patches gradient edits locally. }
procedure TestTikZFadingPatches;
const
  OldDeclaration = '\pgfinterruptpicture\pgfdeclarehorizontalshading' +
    '{tpxfadeL1x0p2s}{100bp}{gray(0bp)=(1);gray(25bp)=(1);' +
    'gray(75bp)=(0.2);gray(100bp)=(0.2)}\pgfdeclarefading{tpxfadeL1x0p2}' +
    '{\pgfuseshading{tpxfadeL1x0p2s}}\endpgfinterruptpicture%';
  NewDeclaration = '\pgfinterruptpicture\pgfdeclarehorizontalshading' +
    '{tpxfadeL1x0p5s}{100bp}{gray(0bp)=(1);gray(25bp)=(1);' +
    'gray(75bp)=(0.5);gray(100bp)=(0.5)}\pgfdeclarefading{tpxfadeL1x0p5}' +
    '{\pgfuseshading{tpxfadeL1x0p5s}}\endpgfinterruptpicture%';
  HalfDeclaration = '\pgfinterruptpicture\pgfdeclarehorizontalshading' +
    '{tpxfadeL0p5x1s}{100bp}{gray(0bp)=(0.5);gray(25bp)=(0.5);' +
    'gray(75bp)=(1);gray(100bp)=(1)}\pgfdeclarefading{tpxfadeL0p5x1}' +
    '{\pgfuseshading{tpxfadeL0p5x1s}}\endpgfinterruptpicture%';
  HalfFill = '\path[left color=F, right color=G, path fading=tpxfadeL0p5x1, ' +
    'fit fading=false, fading transform={shift={(15,25)}, rotate=0, ' +
    'xscale=1.70079, yscale=1.79279}] (0.00,20.00) rectangle +(30.00,10.00);';
  HalfStroke = '\path[line width=1*\tpxLineWidth, draw=L] (0.00,20.00) ' +
    'rectangle +(30.00,10.00);';
var
  Mode: TTpXMode;
  Directory, FileName, ErrorText: string;
  Original, Bytes, Expected, Eol: RawByteString;
  Prim: TPrimitive2D;
  procedure Reopen;
  begin
    Check(Mode.OpenDocumentFromPath(TDocumentPath(FileName), ErrorText),
      'Opening the faded TikZ file failed: ' + ErrorText);
    Check(Mode.Drawing.ObjectsCount = 4,
      Format('The faded TikZ file opened as %d objects',
      [Mode.Drawing.ObjectsCount]));
  end;
  function Obj(Index: Integer): TPrimitive2D;
  begin
    Result := TikZSaveObjectAt(Mode.Drawing, Index) as TPrimitive2D;
  end;
  procedure SaveAndCheckNoOp(const What: string);
  begin
    ErrorText := PaintStyleSave(Mode, FileName, False);
    Check(ErrorText = '', 'Saving ' + What + ' failed: ' + ErrorText);
    Bytes := TikZSaveReadBytes(FileName);
    Reopen;
    ErrorText := PaintStyleSave(Mode, FileName, False);
    Check(ErrorText = '', 'Saving reopened ' + What + ' failed: ' + ErrorText);
    Check(TikZSaveReadBytes(FileName) = Bytes,
      'Saving reopened ' + What + ' unchanged was not a no-op');
  end;
begin
  SetLiveTeXEnabled(False);
  Mode := MainForm.EventManager.Mode as TTpXMode;
  Mode.TpX_Manager.DocumentSession.Clear;
  Directory := GetEnvironmentVariable('TPX_PAINT_STYLE_DIR');
  Check(Directory <> '', 'Paint style directory is not configured');
  FileName := IncludeTrailingPathDelimiter(Directory) + 'faded.tikz';
  Original := TikZSaveReadBytes(GetEnvironmentVariable('TPX_TIKZ_FADING_FIXTURE'));
  Check(Pos(OldDeclaration, Original) > 0, 'The fading fixture is not TpX output');
  { TpX writes CRLF line breaks; a checkout may have turned them into LF. }
  if Pos(#13#10, Original) > 0 then Eol := #13#10 else Eol := #10;
  TikZSaveWriteBytes(FileName, Original);
  Reopen;
  Prim := Obj(0);
  Check((Prim.FillGradient = fgLinear) and (Prim.LineStyle = liSolid) and
    (ColorToRGB(Prim.LineColor) = ColorToRGB(clLime)) and
    (Abs(Prim.GradientAngle - 30) < 1E-6) and (Prim.FillOpacity = 1) and
    (Abs(Prim.GradientOpacity - 0.2) < 1E-6),
    'The stroked faded rectangle did not reopen as one object');
  Prim := Obj(1);
  Check((Prim.FillGradient = fgRadial) and (Prim.LineStyle = liNone) and
    (Prim.GradientOpacity = 0), 'The radial fade did not reopen');
  ErrorText := PaintStyleSave(Mode, FileName, False);
  Check(ErrorText = '', 'Saving the unchanged faded file failed: ' + ErrorText);
  Check(TikZSaveReadBytes(FileName) = Original,
    'Saving the unchanged faded file changed its bytes');

  { End colour and end opacity: the fading is replaced by a new one and
    only the changed option values are patched. }
  Prim := Obj(0);
  Prim.GradientColor := clRed;
  Prim.GradientOpacity := 0.5;
  ErrorText := PaintStyleSave(Mode, FileName, False);
  Check(ErrorText = '', 'Saving a faded gradient edit failed: ' + ErrorText);
  Expected := StringReplace(Original, OldDeclaration, NewDeclaration, []);
  Expected := StringReplace(Expected,
    'top color=G, shading angle=-60, path fading=tpxfadeL1x0p2',
    'top color=red, shading angle=-60, path fading=tpxfadeL1x0p5', []);
  Bytes := TikZSaveReadBytes(FileName);
  Check(Bytes = Expected, 'A faded gradient edit was not local: ' + Bytes);
  Reopen;
  Prim := Obj(0);
  Check((ColorToRGB(Prim.GradientColor) = ColorToRGB(clRed)) and
    (Abs(Prim.GradientOpacity - 0.5) < 1E-6) and (Prim.LineStyle = liSolid),
    'The faded gradient edit did not reopen');

  { Equal end opacities: the fill joins its stroke statement and the
    fading declaration goes. }
  TikZSaveWriteBytes(FileName, Original);
  Reopen;
  Obj(2).GradientOpacity := 0.5;
  ErrorText := PaintStyleSave(Mode, FileName, False);
  Check(ErrorText = '', 'Saving an unfaded gradient failed: ' + ErrorText);
  Expected := StringReplace(Original, HalfDeclaration + Eol, '', []);
  Expected := StringReplace(Expected, HalfFill, '', []);
  Expected := StringReplace(Expected, HalfStroke, '\path[left color=F, ' +
    'right color=G, fill opacity=0.5, line width=1*\tpxLineWidth, draw=L] ' +
    '(0.00,20.00) rectangle +(30.00,10.00);', []);
  Bytes := TikZSaveReadBytes(FileName);
  Check(Bytes = Expected, 'Unfading a gradient was not local: ' + Bytes);
  SaveAndCheckNoOp('an unfaded gradient');
  Prim := Obj(2);
  Check((Prim.FillGradient = fgLinear) and (Prim.LineStyle = liSolid) and
    (Abs(Prim.FillOpacity - 0.5) < 1E-6) and
    (Abs(Prim.GradientOpacity - 0.5) < 1E-6),
    'The unfaded gradient did not reopen as one stroked object');

  { Different end opacities again: the fill moves to a new statement before
    the stroke, with its fading declared. }
  Prim.GradientOpacity := 1;
  ErrorText := PaintStyleSave(Mode, FileName, False);
  Check(ErrorText = '', 'Saving a newly faded gradient failed: ' + ErrorText);
  Bytes := TikZSaveReadBytes(FileName);
  Check(Pos(HalfDeclaration + Eol + '\path[left color=F, right color=G, ' +
    'path fading=tpxfadeL0p5x1, fit fading=false, fading transform=' +
    '{shift={(15mm,25mm)}, rotate=0, xscale=1.70079, yscale=1.79279}] ' +
    '(0mm,20mm) rectangle (30mm,30mm);' + Eol + HalfStroke, Bytes) > 0,
    'A newly faded gradient was not split from its stroke: ' + Bytes);
  SaveAndCheckNoOp('a newly faded gradient');
  Prim := Obj(2);
  Check((Prim.FillGradient = fgLinear) and (Prim.LineStyle = liSolid) and
    (Abs(Prim.FillOpacity - 0.5) < 1E-6) and (Prim.GradientOpacity = 1),
    'The newly faded gradient did not reopen as one object');

  { Geometry and kind edits rewrite the fading placement. }
  Prim.Transform(Translate2D(5, 0));
  Obj(1).FillGradient := fgLinear;
  SaveAndCheckNoOp('moved and changed fades');
  Check(Pos('fading transform={shift={(20mm,25mm)}', Bytes) > 0,
    'Moving a faded rectangle did not move its fading: ' + Bytes);
  Prim := Obj(2);
  Check(Abs(Prim.Points[0].X - 5) < 1E-4, 'The faded rectangle did not move');
  Prim := Obj(1);
  Check((Prim.FillGradient = fgLinear) and (Prim.GradientOpacity = 0) and
    (Prim.FillOpacity = 1), 'A radial fade did not become a linear one');
end;

{ Fading names spell out the opacities they were written with, so editing
  one of two fades whose end opacities differ only in the fourth decimal
  leaves the other alone; a shared opacity option takes gradient edits. }
procedure TestTikZFadingNamesAndSharedOpacity;
const
  Shared = '\begin{tikzpicture}[x=1mm,y=1mm]' + LineEnding +
    '\draw[draw=red, inner color=blue, outer color=white, opacity=0.5] ' +
    '(0,0) circle (5);' + LineEnding +
    '\path[left color=red, right color=blue, opacity=0.4] (20,0) ' +
    'rectangle (30,10);' + LineEnding +
    '\end{tikzpicture}' + LineEnding;
var
  Mode: TTpXMode;
  Drawing: TDrawing2D;
  Directory, FileName, ErrorText: string;
  Bytes, Before, Second: RawByteString;
  Disc: TCircle2D;
  I, At: Integer;
  procedure Open(Count: Integer);
  begin
    Check(Mode.OpenDocumentFromPath(TDocumentPath(FileName), ErrorText),
      'Opening ' + FileName + ' failed: ' + ErrorText);
    Check(Mode.Drawing.ObjectsCount = Count, Format('%s opened as %d objects',
      [FileName, Mode.Drawing.ObjectsCount]));
  end;
  function Obj(Index: Integer): TPrimitive2D;
  begin
    Result := TikZSaveObjectAt(Mode.Drawing, Index) as TPrimitive2D;
  end;
  procedure Save(const What: string);
  begin
    ErrorText := PaintStyleSave(Mode, FileName, False);
    Check(ErrorText = '', 'Saving ' + What + ' failed: ' + ErrorText);
    Bytes := TikZSaveReadBytes(FileName);
  end;
begin
  SetLiveTeXEnabled(False);
  Mode := MainForm.EventManager.Mode as TTpXMode;
  Mode.TpX_Manager.DocumentSession.Clear;
  Directory := GetEnvironmentVariable('TPX_PAINT_STYLE_DIR');
  Check(Directory <> '', 'Paint style directory is not configured');

  FileName := IncludeTrailingPathDelimiter(Directory) + 'close.tikz';
  Drawing := Mode.Drawing;
  Drawing.Clear;
  for I := 0 to 1 do
  begin
    Disc := TCircle2D.Create(-1);
    Disc.Points[0] := Point2D(10 + 20 * I, 10);
    Disc.Points[1] := Point2D(15 + 20 * I, 10);
    Disc.LineStyle := liNone;
    Disc.FillColor := clBlack;
    Disc.FillGradient := fgRadial;
    Disc.GradientOpacity := 0.2501 + 0.0003 * I;
    Drawing.AddObject(-1, Disc);
  end;
  ErrorText := PaintStyleSave(Mode, FileName, True);
  Check(ErrorText = '', 'Save As TikZ failed: ' + ErrorText);
  Bytes := TikZSaveReadBytes(FileName);
  Check((Pos('{tpxfadeR1x0p2501}', Bytes) > 0) and
    (Pos('{tpxfadeR1x0p2504}', Bytes) > 0),
    'Close end opacities did not get their own fadings: ' + Bytes);
  Open(2);
  Check(Abs(Obj(1).GradientOpacity - 0.2504) < 1E-6,
    'The second fade lost its end opacity');
  At := Pos('\path[inner color=black, outer color=black, path fading=' +
    'tpxfadeR1x0p2504', Bytes);
  Check(At > 0, 'The second fade statement is missing: ' + Bytes);
  Second := Copy(Bytes, At, MaxInt);
  Obj(0).GradientOpacity := 0.2502;
  Save('a close end opacity');
  Check(Copy(Bytes, Length(Bytes) - Length(Second) + 1, MaxInt) = Second,
    'Editing the first fade changed the second statement: ' + Bytes);
  Check((Pos('{tpxfadeR1x0p2501}', Bytes) = 0) and
    (Pos('{tpxfadeR1x0p2502}', Bytes) > 0) and
    (Pos('{tpxfadeR1x0p2504}', Bytes) > 0),
    'The edited fade did not get its own fading: ' + Bytes);
  Open(2);
  Check((Abs(Obj(0).GradientOpacity - 0.2502) < 1E-6) and
    (Abs(Obj(1).GradientOpacity - 0.2504) < 1E-6),
    'Editing one of two close fades changed the other');

  { Shared opacity: the stroke keeps it as draw opacity, the fill options
    carry the fill opacity or the fading. }
  FileName := IncludeTrailingPathDelimiter(Directory) + 'shared.tikz';
  TikZSaveWriteBytes(FileName, Shared);
  Open(2);
  Obj(0).GradientColor := clYellow;
  Obj(1).GradientOpacity := 0;
  Save('gradient edits under a shared opacity');
  Before := Bytes;
  Check(Pos('\draw[draw=red, inner color=blue, outer color=yellow, ' +
    'fill opacity=0.5, draw opacity=0.5] (0,0) circle (5);', Bytes) > 0,
    'The shared opacity of a stroked gradient was not split: ' + Bytes);
  Check((Pos('opacity=0.4', Bytes) = 0) and
    (Pos('{tpxfadeL0p4x0}', Bytes) > 0),
    'The shared opacity of a newly faded gradient was not replaced: ' + Bytes);
  Open(2);
  Check((ColorToRGB(Obj(0).GradientColor) = ColorToRGB(clYellow)) and
    (Abs(Obj(0).FillOpacity - 0.5) < 1E-6) and
    (Abs(Obj(0).LineOpacity - 0.5) < 1E-6),
    'The stroked gradient under a shared opacity did not reopen');
  Check((Abs(Obj(1).FillOpacity - 0.4) < 1E-6) and (Obj(1).GradientOpacity = 0),
    'The faded gradient under a shared opacity did not reopen');
  Save('the reopened shared opacity file');
  Check(Bytes = Before, 'Saving the reopened file unchanged was not a no-op');
end;
