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
