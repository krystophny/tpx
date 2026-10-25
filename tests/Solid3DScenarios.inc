const
  Solid3DSceneTpX =
    '%<TpX v="5" TeXFormat="tikz" PdfTeXFormat="tikz" TeXFigure="none" PicScale="1" Border="2">'#10 +
    '%  <group><solid x1="20" y1="80" x2="45" y2="80" az="-60" el="25" lw="0.50" lc="#8c8c8c" fill="#ffffff"><body kind="box" w="1" d="1" h="1"/></solid></group>'#10 +
    '%  <group><solid x1="60" y1="80" x2="85" y2="80" az="-60" el="25" lw="0.50" lc="#8c8c8c" fill="#ffffff"><body kind="cylinder" r="0.42" h="1" segments="24"/></solid></group>'#10 +
    '%  <group><solid x1="100" y1="80" x2="125" y2="80" az="-60" el="25" lw="0.50" lc="#8c8c8c" fill="#ffffff"><body kind="sphere" r="0.5" segments="24" rings="12"/></solid></group>'#10 +
    '%  <group><solid x1="140" y1="80" x2="165" y2="80" az="-60" el="25" lw="0.50" lc="#8c8c8c" fill="#ffffff"><body kind="cone" r="0.48" h="1.05" segments="24"/></solid></group>'#10 +
    '%  <group><solid x1="185" y1="80" x2="215" y2="80" az="-60" el="25" lw="0.50" lc="#8c8c8c" fill="#ffffff"><body kind="torus" r1="0.62" r2="0.24" segments="36" rings="16"/></solid></group>'#10 +
    '%  <group><solid x1="70" y1="30" x2="72" y2="30" az="-62" el="24" lw="0.50" ha="3" lc="#8c8c8c" hc="#8c8c8c" fill="#ffffff">'#10 +
    '%    <body kind="revolve" angle="260" start="-12" segments="40">'#10 +
    '%      <bezier closed="1">12,0 12,5.5 14.5,8.5 17,8.5 20.5,8.5 22.5,4 22.5,0 22.5,-4 20.5,-8.5 17,-8.5 14.5,-8.5 12,-5.5 12,0</bezier>'#10 +
    '%      <ellipse x="17.6" y="0" dx="5.2" dy="9"/>'#10 +
    '%    </body>'#10 +
    '%    <body kind="revolve" angle="296" start="-30" segments="40">'#10 +
    '%      <ellipse x="17.6" y="0" dx="5.2" dy="9"/>'#10 +
    '%    </body>'#10 +
    '%  </solid></group>'#10 +
    '%  <group><solid x1="160" y1="25" x2="161" y2="25" az="-35" el="28" lw="0.60" lc="#666666" fill="#ffffff">'#10 +
    '%    <body kind="extrude" depth="8"><polygon>0,0 18,0 18,5 6,5 6,14 0,14</polygon></body>'#10 +
    '%  </solid></group>'#10 +
    '%</TpX>'#10;

function Solid3DLoad(const FormatId, FileName: string; const Bytes: RawByteString;
  out Context: TObject): TDrawing2D;
var
  CanSaveBack: Boolean;
  Diagnostics: TStringList;
begin
  Diagnostics := TStringList.Create;
  try
    Result := LoadDocumentCandidate(FormatId, TDocumentPath(FileName), Bytes,
      CanSaveBack, Context, Diagnostics);
    Check(Result <> nil, FileName + ' did not load: ' + Diagnostics.Text);
    Check(CanSaveBack, FileName + ' with 3D objects is not editable: ' +
      Diagnostics.Text);
  finally
    Diagnostics.Free;
  end;
end;

function Solid3DCount(const Drawing: TDrawing2D): Integer;
var
  Obj: TGraphicObject;
begin
  Result := 0;
  Obj := Drawing.ObjectList.FirstObj;
  while Obj <> nil do
  begin
    if Obj is TSolid3D then Inc(Result);
    Obj := Drawing.ObjectList.NextObj;
  end;
end;

procedure CheckSameSolid(const A, B: TSolid3D; const What: string;
  const ShiftX, ShiftY, BaseA, BaseB: Double);
var
  I, J, K: Integer;
  BA, BB: TSolidBody;
  PA, PB: TPiece;
  Tol: Double;
begin
  Check(B <> nil, What + ': 3D object missing');
  Check((Abs(A.Azimuth - B.Azimuth) < 1E-6) and
    (Abs(A.Elevation - B.Elevation) < 1E-6) and
    (A.Perspective = B.Perspective) and (A.Hidden = B.Hidden),
    What + ': camera or style changed');
  Check((A.LineColor = B.LineColor) and (A.FillColor = B.FillColor) and
    (A.Hatching = B.Hatching) and
    // TpX stores line width multipliers with two decimals.
    (Abs(A.LineWidth * BaseA - B.LineWidth * BaseB) <=
    0.01 * A.LineWidth * BaseA + 1E-6),
    What + Format(': line, fill or hatching changed (lc %x/%x fill %x/%x ha %d/%d lw %g*%g vs %g*%g)',
    [A.LineColor, B.LineColor, A.FillColor, B.FillColor, Ord(A.Hatching),
    Ord(B.Hatching), A.LineWidth, BaseA, B.LineWidth, BaseB]));
  Check(A.BodyCount = B.BodyCount, What + ': body count changed');
  for I := 0 to A.BodyCount - 1 do
  begin
    BA := A.Bodies[I];
    BB := B.Bodies[I];
    Check((BA.Kind = BB.Kind) and (BA.SegU = BB.SegU) and (BA.SegV = BB.SegV),
      What + ': body kind or segments changed');
    Check((Abs(BA.Size1 - BB.Size1) < 1E-6) and
      (Abs(BA.Size2 - BB.Size2) < 1E-6) and (Abs(BA.Size3 - BB.Size3) < 1E-6),
      What + ': body parameters changed');
    for K := 0 to 11 do
      Check(Abs(BA.M[K] - BB.M[K]) < 1E-6, What + ': body placement changed');
    Check(BA.Profiles.Count = BB.Profiles.Count, What + ': profile count changed');
  end;
  Tol := 1E-3 * Max(1, A.BoundingBox.Right - A.BoundingBox.Left);
  Check(A.Pieces.Count = B.Pieces.Count, What + Format(
    ': rendered face count changed (%d vs %d)', [A.Pieces.Count, B.Pieces.Count]));
  for I := 0 to A.Pieces.Count - 1 do
  begin
    PA := A.Pieces[I];
    PB := B.Pieces[I];
    Check(PA.Count = PB.Count, What + ': rendered face shape changed');
    for J := 0 to PA.Count - 1 do
      Check((Abs(PA[J].X + ShiftX - PB[J].X) < Tol) and
        (Abs(PA[J].Y + ShiftY - PB[J].Y) < Tol),
        What + Format(': rendered geometry moved (face %d point %d: %g,%g vs %g,%g; ' +
        'origin %g,%g vs %g,%g)', [I, J, PA[J].X, PA[J].Y, PB[J].X, PB[J].Y,
        A.Points[0].X, A.Points[0].Y, B.Points[0].X, B.Points[0].Y]));
  end;
end;

procedure CheckSameSolids(const A, B: TDrawing2D; const What: string;
  const AllowShift: Boolean = False);
var
  OA, OB: TGraphicObject;
  ShiftX, ShiftY: Double;
  First: Boolean;
begin
  // SVG keeps the picture, not TpX's absolute coordinates: one common
  // translation of the whole drawing is allowed there.
  ShiftX := 0;
  ShiftY := 0;
  First := True;
  Check(Solid3DCount(A) = Solid3DCount(B), What + Format(
    ': 3D object count changed (%d vs %d)', [Solid3DCount(A), Solid3DCount(B)]));
  Check(A.ObjectsCount = B.ObjectsCount, What + ': object count changed');
  OA := A.ObjectList.FirstObj;
  OB := B.ObjectList.FirstObj;
  while (OA <> nil) and (OB <> nil) do
  begin
    if OA is TSolid3D then
    begin
      Check(OB is TSolid3D, What + ': a 3D object became ' + OB.ClassName);
      if AllowShift and First and (TSolid3D(OA).Pieces.Count > 0) and
        (TSolid3D(OB).Pieces.Count > 0) then
      begin
        ShiftX := TSolid3D(OB).Pieces[0][0].X - TSolid3D(OA).Pieces[0][0].X;
        ShiftY := TSolid3D(OB).Pieces[0][0].Y - TSolid3D(OA).Pieces[0][0].Y;
      end;
      First := False;
      CheckSameSolid(TSolid3D(OA), TSolid3D(OB), What, ShiftX, ShiftY,
        A.LineWidthBase, B.LineWidthBase);
    end;
    OA := A.ObjectList.NextObj;
    OB := B.ObjectList.NextObj;
  end;
end;

procedure TestSolid3DFormats;
const
  Formats: array[0..2] of string = ('tpx', 'svg', 'tikz');
  Names: array[0..2] of string = ('scene.TpX', 'scene.svg', 'scene.tikz');
var
  Scene, Reloaded, Again, Mid: TDrawing2D;
  Ctx, Ctx2, MidCtx: TObject;
  Bytes, Bytes2, Tampered: RawByteString;
  I, P: Integer;
  Snapshot: TMemoryStream;
begin
  Scene := Solid3DLoad('tpx', 'scene.TpX', Solid3DSceneTpX, Ctx);
  try
    Check(Solid3DCount(Scene) = 7, 'TpX reader did not restore the 3D objects');
    for I := 0 to High(Formats) do
    begin
      Bytes := SerializeDocumentToBytes(Formats[I], Scene, TDocumentPath(Names[I]));
      Reloaded := Solid3DLoad(Formats[I], Names[I], Bytes, Ctx2);
      try
        CheckSameSolids(Scene, Reloaded, Formats[I] + ' round trip', I = 1);
        Bytes2 := SerializeDocumentToBytes(Formats[I], Reloaded,
          TDocumentPath(Names[I]), Ctx2);
        Check(Bytes2 = Bytes, Formats[I] + ' save -> open -> save is not byte-stable');
        // An undo snapshot (object stream) keeps the saved numbers too.
        Snapshot := TMemoryStream.Create;
        try
          Reloaded.SaveObjectsToStream(Snapshot);
          Reloaded.DeleteAllObjects;
          Snapshot.Position := 0;
          Reloaded.LoadObjectsFromStream(Snapshot);
        finally
          Snapshot.Free;
        end;
        if I = 1 then
          Check(SerializeDocumentToBytes(Formats[I], Reloaded,
            TDocumentPath(Names[I]), Ctx2) = Bytes,
            Formats[I] + ' save after an undo snapshot is not byte-stable');
        Again := Solid3DLoad(Formats[I], Names[I], Bytes2, MidCtx);
        try
          CheckSameSolids(Scene, Again, Formats[I] + ' second round trip', I = 1);
        finally
          Again.Free;
          MidCtx.Free;
        end;
      finally
        Reloaded.Free;
        Ctx2.Free;
      end;
    end;
    // Cross-format chains keep the solids.
    for I := 1 to 2 do
    begin
      Bytes := SerializeDocumentToBytes(Formats[I], Scene, TDocumentPath(Names[I]));
      Mid := Solid3DLoad(Formats[I], Names[I], Bytes, MidCtx);
      try
        Bytes2 := SerializeDocumentToBytes('tpx', Mid, 'chain.TpX');
        Reloaded := Solid3DLoad('tpx', 'chain.TpX', Bytes2, Ctx2);
        try
          CheckSameSolids(Scene, Reloaded, 'TpX -> ' + Formats[I] + ' -> TpX',
            I = 1);
        finally
          Reloaded.Free;
          Ctx2.Free;
        end;
      finally
        Mid.Free;
        MidCtx.Free;
      end;
    end;
    // Copy as TikZ carries the 3D objects and pastes back as them.
    Bytes := GenerateTikZPictureFragment(Scene);
    Check(Pos('% tpx-solid ', string(Bytes)) > 0,
      'Copy as TikZ dropped the 3D object records');
    if GetEnvironmentVariable('TPX_TIKZ_FRAGMENT') <> '' then
      with TFileStream.Create(GetEnvironmentVariable('TPX_TIKZ_FRAGMENT'), fmCreate) do
      try
        WriteBuffer(Bytes[1], Length(Bytes));
      finally
        Free;
      end;
    Reloaded := Solid3DLoad('tikz', 'clipboard.tikz', Bytes, Ctx2);
    try
      CheckSameSolids(Scene, Reloaded, 'Copy as TikZ');
    finally
      Reloaded.Free;
      Ctx2.Free;
    end;
    // Faces edited outside TpX stay as the edited plain paths.
    Bytes := SerializeDocumentToBytes('svg', Scene, 'scene.svg');
    P := Pos('<polygon', string(Bytes));
    P := PosEx('points="', string(Bytes), P) + Length('points="');
    Tampered := Copy(Bytes, 1, P - 1) + '1' + Copy(Bytes, P, MaxInt);
    Reloaded := Solid3DLoad('svg', 'tampered.svg', Tampered, Ctx2);
    try
      Check(Solid3DCount(Reloaded) = 6,
        'an SVG 3D object with edited faces was not kept as plain paths');
    finally
      Reloaded.Free;
      Ctx2.Free;
    end;
    Bytes := SerializeDocumentToBytes('tikz', Scene, 'scene.tikz');
    P := Pos('% tpx-solid ', string(Bytes));
    P := PosEx(' -- (', string(Bytes), P) + Length(' -- (');
    Tampered := Copy(Bytes, 1, P - 1) + '1' + Copy(Bytes, P, MaxInt);
    Reloaded := Solid3DLoad('tikz', 'tampered.tikz', Tampered, Ctx2);
    try
      Check(Solid3DCount(Reloaded) = 6,
        'a TikZ 3D object with edited paths was not kept as plain paths');
    finally
      Reloaded.Free;
      Ctx2.Free;
    end;
  finally
    Scene.Free;
    Ctx.Free;
  end;
  WriteLn('PASS solid3d-formats');
end;

function Solid3DReadFile(const FileName: string): RawByteString;
var
  F: TFileStream;
begin
  F := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Result, F.Size);
    if F.Size > 0 then F.ReadBuffer(Result[1], F.Size);
  finally
    F.Free;
  end;
end;

procedure TestSolid3DOpenSave;
const
  Exts: array[0..2] of string = ('.TpX', '.svg', '.tikz');
  Formats: array[0..2] of string = ('tpx', 'svg', 'tikz');
var
  Mode: TTpXMode;
  Base, Path, Path2, ErrorText: string;
  Reference, Ctx: TObject;
  Expected: TDrawing2D;
  I: Integer;
  First, Second: RawByteString;
  Solid: TSolid3D;
  Obj: TGraphicObject;
begin
  Mode := TTpXMode(MainForm.EventManager.Mode);
  Base := GetTempFileName(GetTempDir, 'tpx-solid-');
  DeleteFile(Base);
  Expected := Solid3DLoad('tpx', 'scene.TpX', Solid3DSceneTpX, Reference);
  try
    Path := Base + '.TpX';
    with TFileStream.Create(Path, fmCreate) do
    try
      WriteBuffer(Solid3DSceneTpX[1], Length(Solid3DSceneTpX));
    finally
      Free;
    end;
    // File -> Open, then Save As through every editable format and back.
    for I := 0 to High(Formats) do
    begin
      ErrorText := '';
      Check(Mode.OpenDocumentFromPath(TDocumentPath(Path), ErrorText),
        'opening ' + Path + ' failed: ' + ErrorText);
      CheckSameSolids(Expected, Mode.Drawing, 'open ' + ExtractFileExt(Path), True);
      Path2 := Base + '-as' + Exts[I];
      Check(Mode.SaveDocumentToPath(TDocumentPath(Path2), Formats[I], True, True,
        ErrorText), 'Save As ' + Formats[I] + ' failed: ' + ErrorText);
      First := Solid3DReadFile(Path2);
      Check(Mode.OpenDocumentFromPath(TDocumentPath(Path2), ErrorText),
        'reopening ' + Path2 + ' failed: ' + ErrorText);
      CheckSameSolids(Expected, Mode.Drawing, 'reopen ' + Exts[I], True);
      Check(Mode.SaveDocumentToPath(TDocumentPath(Path2), Formats[I], False, False,
        ErrorText), 'Save ' + Formats[I] + ' failed: ' + ErrorText);
      Second := Solid3DReadFile(Path2);
      Check(First = Second, 'Save of an unchanged ' + Formats[I] +
        ' document with 3D objects changed its bytes');
      // Edit the tokamak scene, save in place and reopen.
      Obj := Mode.Drawing.ObjectList.FirstObj;
      Solid := nil;
      while Obj <> nil do
      begin
        if (Obj is TSolid3D) and (TSolid3D(Obj).BodyCount = 2) then
          Solid := TSolid3D(Obj);
        Obj := Mode.Drawing.ObjectList.NextObj;
      end;
      Check(Solid <> nil, 'reopened ' + Formats[I] + ' lost the two-body scene');
      Solid.Bodies[0].Size1 := 200;
      Solid.SetView(-40, 30);
      Mode.Drawing.NotifyChanged;
      Check(Mode.SaveDocumentToPath(TDocumentPath(Path2), Formats[I], False, False,
        ErrorText), 'Save of an edited ' + Formats[I] + ' 3D object failed: ' +
        ErrorText);
      Check(Mode.OpenDocumentFromPath(TDocumentPath(Path2), ErrorText),
        'reopening the edited ' + Path2 + ' failed: ' + ErrorText);
      Check(Solid3DCount(Mode.Drawing) = 7, 'edited ' + Formats[I] +
        ' document lost 3D objects');
      Obj := Mode.Drawing.ObjectList.FirstObj;
      Solid := nil;
      while Obj <> nil do
      begin
        if (Obj is TSolid3D) and (TSolid3D(Obj).BodyCount = 2) then
          Solid := TSolid3D(Obj);
        Obj := Mode.Drawing.ObjectList.NextObj;
      end;
      Check((Solid <> nil) and (Abs(Solid.Bodies[0].Size1 - 200) < 1E-6) and
        (Abs(Solid.Azimuth + 40) < 1E-6), 'edited ' + Formats[I] +
        ' 3D parameters were not saved');
      DeleteFile(Path2);
      DeleteFile(ChangeFileExt(Path2, '.tpx-recovery'));
    end;
  finally
    Expected.Free;
    Reference.Free;
    DeleteFile(Path);
    DeleteFile(ChangeFileExt(Path, '.tpx-recovery'));
  end;
  WriteLn('PASS solid3d-open-save');
end;

procedure TestSolid3DEditing;
var
  Drawing: TDrawing2D;
  Profile, Hole, Axis: TPrimitive2D;
  Solid, Merged: TSolid3D;
  Obj: TGraphicObject;
  Count, I: Integer;
  procedure CheckRevolveAxis(const DX, DY: Double; const Reverse: Boolean);
  var
    P: array[0..3] of TPoint2D;
    Axis0, Axis1, Swap: TPoint2D;
    J, FormatIndex: Integer;
    R: TRect2D;
    Restored: TPolygon2D;
    Bytes: RawByteString;
    Reloaded: TDrawing2D;
    Context: TObject;
  const
    Formats: array[0..2] of string = ('tpx', 'svg', 'tikz');
    Names: array[0..2] of string = ('axis.TpX', 'axis.svg', 'axis.tikz');
  begin
    Drawing.DeleteAllObjects;
    // Independently specified rectangle: radius 5..10, axial height -5..5,
    // around the line through (20,30) in direction (DX,DY).
    P[0] := Point2D(20 + 5 * DY - 5 * DX, 30 - 5 * DX - 5 * DY);
    P[1] := Point2D(20 + 10 * DY - 5 * DX, 30 - 10 * DX - 5 * DY);
    P[2] := Point2D(20 + 10 * DY + 5 * DX, 30 - 10 * DX + 5 * DY);
    P[3] := Point2D(20 + 5 * DY + 5 * DX, 30 - 5 * DX + 5 * DY);
    Profile := TPolygon2D.CreateSpec(-1, P);
    Axis0 := Point2D(20 - 20 * DX, 30 - 20 * DY);
    Axis1 := Point2D(20 + 20 * DX, 30 + 20 * DY);
    if Reverse then
    begin
      Swap := Axis0; Axis0 := Axis1; Axis1 := Swap;
    end;
    Axis := TLine2D.CreateSpec(-1, Axis0, Axis1);
    Drawing.AddObject(-1, Profile);
    Drawing.AddObject(-1, Axis);
    Drawing.SelectAll;
    Check(SolidFromSelected(Drawing, skRevolve), 'Revolve rejected a valid axis');
    Solid := Drawing.ObjectList.FirstObj as TSolid3D;
    R := (Solid.Bodies[0].Profiles.FirstObj as TObject2D).BoundingBox;
    Check((Abs(R.Left - 5) < 1E-5) and (Abs(R.Right - 10) < 1E-5) and
      (Abs(R.Bottom + 5) < 1E-5) and (Abs(R.Top - 5) < 1E-5),
      Format('Revolve axis %g,%g: radius %g..%g height %g..%g',
        [DX, DY, R.Left, R.Right, R.Bottom, R.Top]));
    Check(Solid.Pieces.Count > 0, 'Revolve about a selected axis rendered nothing');
    for FormatIndex := 0 to High(Formats) do
    begin
      Bytes := SerializeDocumentToBytes(Formats[FormatIndex], Drawing,
        TDocumentPath(Names[FormatIndex]));
      Reloaded := Solid3DLoad(Formats[FormatIndex], Names[FormatIndex], Bytes, Context);
      try
        CheckSameSolids(Drawing, Reloaded, 'angled-axis ' + Formats[FormatIndex],
          FormatIndex = 1);
        Check(SerializeDocumentToBytes(Formats[FormatIndex], Reloaded,
          TDocumentPath(Names[FormatIndex]), Context) = Bytes,
          'angled-axis ' + Formats[FormatIndex] + ' unchanged save changed bytes');
      finally
        Reloaded.Free;
        Context.Free;
      end;
    end;
    Drawing.SelectAll;
    UngroupSelected(Drawing);
    Restored := Drawing.ObjectList.FirstObj as TPolygon2D;
    for J := 0 to 3 do
      Check((Abs(Restored.Points[J].X - P[J].X) < 1E-5) and
        (Abs(Restored.Points[J].Y - P[J].Y) < 1E-5),
        'Ungroup did not restore the original profile after an angled revolution');
  end;
begin
  Drawing := MainForm.TheDrawing;
  Drawing.TeXCenterFigure := True;
  CheckRevolveAxis(0, 1, False);
  CheckRevolveAxis(1, 0, False);
  CheckRevolveAxis(0.6, 0.8, False);
  CheckRevolveAxis(0.6, 0.8, True);
  Drawing.DeleteAllObjects;
  Profile := TPolygon2D.CreateSpec(-1, [Point2D(0,0), Point2D(10,0),
    Point2D(10,10), Point2D(0,10)]);
  Axis := TLine2D.CreateSpec(-1, Point2D(20,0), Point2D(20,10));
  Drawing.AddObject(-1, Profile);
  Drawing.AddObject(-1, Axis);
  Drawing.SelectAll;
  Check(SolidFromSelected(Drawing, skExtrude), 'Extrude rejected a closed profile');
  Check((Drawing.ObjectsCount = 2) and (Drawing.GetObject(Axis.ID) = Axis),
    'Extrude deleted an unrelated selected open shape');
  Drawing.History.Undo;
  Check(Drawing.ObjectsCount = 2, 'Undo did not restore the extrusion selection');

  Drawing.DeleteAllObjects;
  // Insert each primitive the way the tool palette does.
  for I := 0 to 4 do
  begin
    MainForm.EventManager.SendMessage(Msg_InsertBox + I, nil);
    Check(MainForm.EventManager.Mode is TInsertSizedGrObjMode,
      'a 3D insert tool did not start the sized insert mode');
    TInsertSizedGrObjMode(MainForm.EventManager.Mode).PointSelected(
      Point2D(10 + 40 * I, 10));
    TInsertSizedGrObjMode(MainForm.EventManager.Mode).PointSelected(
      Point2D(40 + 40 * I, 40));
  end;
  Check(Solid3DCount(Drawing) = 5, 'the five 3D insert tools did not add objects');
  Obj := Drawing.ObjectList.FirstObj;
  while Obj <> nil do
  begin
    Solid := Obj as TSolid3D;
    Check(Solid.Placed and (Solid.BoundingBox.Right - Solid.BoundingBox.Left > 5),
      'an inserted 3D object was not fitted into its box');
    Check(Solid.FillColor = clWhite, 'inserted 3D faces are not opaque white');
    Obj := Drawing.ObjectList.NextObj;
  end;
  // Revolve two closed shapes about a selected axis line.
  Drawing.DeleteAllObjects;
  Profile := TPolygon2D.CreateSpec(-1, [Point2D(30, -10), Point2D(45, -10),
    Point2D(50, 0), Point2D(45, 10), Point2D(30, 10)]);
  Hole := TPolygon2D.CreateSpec(-1, [Point2D(36, -3), Point2D(42, -3),
    Point2D(42, 3), Point2D(36, 3)]);
  Axis := TLine2D.CreateSpec(-1, Point2D(10, -20), Point2D(10, 20));
  Drawing.AddObject(-1, Profile);
  Drawing.AddObject(-1, Hole);
  Drawing.AddObject(-1, Axis);
  Drawing.SelectAll;
  MainForm.EventManager.SendMessage(Msg_Revolve, nil);
  Check((Drawing.ObjectsCount = 1) and (Drawing.ObjectList.FirstObj is TSolid3D),
    'Revolve did not replace the profiles and the axis by one 3D object');
  Solid := Drawing.ObjectList.FirstObj as TSolid3D;
  Check((Solid.Bodies[0].Kind = skRevolve) and
    (Solid.Bodies[0].Profiles.Count = 2) and
    (Abs(Solid.Bodies[0].Size1 - 270) < 1E-6),
    'Revolve did not keep both profiles with the cut-away default');
  // Undo restores the profiles.
  Drawing.History.Undo;
  Check(Drawing.ObjectsCount = 3, 'Undo did not restore the revolved profiles');
  Drawing.History.Redo;
  Check(Solid3DCount(Drawing) = 1, 'Redo did not restore the revolution');
  // Standard views and orbit.
  Drawing.SelectAll;
  MainForm.EventManager.SendMessage(Msg_View3DTop, nil);
  Solid := Drawing.ObjectList.FirstObj as TSolid3D;
  Check((Abs(Solid.Elevation - 90) < 1E-6), '3D view Top did not look from above');
  MainForm.EventManager.SendMessage(Msg_View3DIsometric, nil);
  Check(Abs(Solid.Elevation - Solid_DefaultElevation) < 1E-6,
    '3D view Isometric did not restore the default camera');
  MainForm.EventManager.SendMessage(Msg_StartRotate, nil);
  Check(MainForm.EventManager.Mode is TOrbitMode,
    'Start rotate on a 3D selection did not orbit');
  MainForm.EventManager.Mode.OnMouseMove(nil, [], 100, 100);
  MainForm.EventManager.Mode.OnMouseUp(nil, mbLeft, [], 140, 120);
  Solid := Drawing.ObjectList.FirstObj as TSolid3D;
  Check((Abs(Solid.Azimuth - (Solid_DefaultAzimuth - 20)) < 1E-6) and
    (Abs(Solid.Elevation - (Solid_DefaultElevation + 10)) < 1E-6),
    'orbit did not turn the camera by the dragged angles');
  // Group joins solids into one scene; Ungroup splits it and returns profiles.
  MainForm.EventManager.SendMessage(Msg_InsertTorus, nil);
  TInsertSizedGrObjMode(MainForm.EventManager.Mode).PointSelected(Point2D(80, 0));
  TInsertSizedGrObjMode(MainForm.EventManager.Mode).PointSelected(Point2D(110, 30));
  Drawing.SelectAll;
  MainForm.EventManager.SendMessage(Msg_Group, nil);
  Check((Drawing.ObjectsCount = 1) and
    (TSolid3D(Drawing.ObjectList.FirstObj).BodyCount = 2),
    'Group did not join two 3D objects into one scene');
  Merged := Drawing.ObjectList.FirstObj as TSolid3D;
  Count := Merged.Pieces.Count;
  Check(Count > 0, 'a merged 3D scene renders nothing');
  // Perspective must render the scene once, not once per projected point.
  Drawing.SelectAll;
  MainForm.EventManager.SendMessage(Msg_View3DPerspective, nil);
  Merged := Drawing.ObjectList.FirstObj as TSolid3D;
  Check(Merged.Perspective and (Merged.Pieces.Count > 0),
    '3D view Perspective did not render the scene in perspective');
  Drawing.SelectAll;
  MainForm.EventManager.SendMessage(Msg_Ungroup, nil);
  Check(Solid3DCount(Drawing) = 2, 'Ungroup did not split the 3D scene');
  Drawing.SelectionClear;
  Obj := Drawing.ObjectList.FirstObj;
  Drawing.SelectionAdd(Obj);
  MainForm.EventManager.SendMessage(Msg_Ungroup, nil);
  Check((Solid3DCount(Drawing) = 1) and (Drawing.ObjectsCount = 3),
    'Ungroup of a revolution did not return its two profiles');
  Drawing.DeleteAllObjects;
  WriteLn('PASS solid3d-editing');
end;

procedure TestSolid3DInsertFit;
var
  Drawing: TDrawing2D;
  Mode: TInsertSizedGrObjMode;
  Solid: TSolid3D;
  Kind: TSolidKind;
  Persp: Boolean;
  A, B: TPoint2D;
  Preview, Final: TRect2D;
  W, H, Tol: Double;
  What: string;
  function SameBox(const R, S: TRect2D): Boolean;
  begin
    Result := (Abs(R.Left - S.Left) <= Tol) and (Abs(R.Right - S.Right) <= Tol) and
      (Abs(R.Bottom - S.Bottom) <= Tol) and (Abs(R.Top - S.Top) <= Tol);
  end;
begin
  Drawing := MainForm.TheDrawing;
  Drawing.DeleteAllObjects;
  // Drag with the mouse handlers of the insert tool: the preview while
  // dragging and the placed object must both be fitted into the dragged box.
  for Persp := False to True do
    for Kind := skBox to skTorus do
    begin
      What := SolidKindNames[Kind] + IfThen(Persp, ' (perspective)', '');
      MainForm.EventManager.SendMessage(Msg_InsertBox + Ord(Kind) - Ord(skBox), nil);
      Check(MainForm.EventManager.Mode is TInsertSizedGrObjMode,
        What + ': the insert tool did not start the sized insert mode');
      Mode := TInsertSizedGrObjMode(MainForm.EventManager.Mode);
      Solid := Mode.Obj as TSolid3D;
      Solid.Perspective := Persp;
      Mode.OnMouseDown(nil, mbLeft, [], 60, 50);
      Mode.OnMouseMove(nil, [], 160, 150);
      Mode.OnMouseMove(nil, [], 260, 200);
      A := MainForm.LocalView.GetSnappedPoint(
        MainForm.LocalView.ScreenToViewport(Point2D(60, 50)));
      B := MainForm.LocalView.GetSnappedPoint(
        MainForm.LocalView.ScreenToViewport(Point2D(260, 200)));
      W := Abs(B.X - A.X);
      H := Abs(B.Y - A.Y);
      Tol := 1E-6 * Max(W, H);
      Check((W > 1) and (H > 1), What + ': the drag did not span a box');
      Preview := Solid.BoundingBox;
      Mode.OnMouseUp(nil, mbLeft, [], 260, 200);
      Check(MainForm.EventManager.Mode <> Mode,
        What + ': releasing the mouse did not finish the insertion');
      Check(Drawing.ObjectList.LastObj = Solid,
        What + ': the dragged object was not added');
      Final := Solid.BoundingBox;
      Check(Solid.Placed and SameBox(Final, Preview),
        What + ': the placed object differs from the drag preview');
      Check((Final.Left >= Min(A.X, B.X) - Tol) and
        (Final.Right <= Max(A.X, B.X) + Tol) and
        (Final.Bottom >= Min(A.Y, B.Y) - Tol) and
        (Final.Top <= Max(A.Y, B.Y) + Tol),
        What + ': the placed object leaves the dragged box');
      Check((Abs((Final.Left + Final.Right) - (A.X + B.X)) <= 2 * Tol) and
        (Abs((Final.Bottom + Final.Top) - (A.Y + B.Y)) <= 2 * Tol),
        What + ': the placed object is not centred in the dragged box');
      Check((Abs((Final.Right - Final.Left) - W) <= Tol) or
        (Abs((Final.Top - Final.Bottom) - H) <= Tol),
        What + ': the placed object does not fill the dragged box');
      // A redraw keeps the placement.
      Solid.UpdateExtension(Solid);
      Check(SameBox(Solid.BoundingBox, Final),
        What + ': the placed object changed size on redraw');
    end;
  Drawing.DeleteAllObjects;
  WriteLn('PASS solid3d-insert-fit');
end;

procedure TestSolid3DInspector;
var
  Drawing: TDrawing2D;
  Torus: TSolid3D;
  ErrorText, FileName: string;
  Edit: TEdit;
  Box: TCheckBox;
  Origin: TPoint2D;
  Scale, Angle: Double;
  Pieces0: Integer;
  Saved: RawByteString;
  Ctx: TObject;
  Reloaded: TDrawing2D;
  procedure ChangeNumber(Field: TObjectPropertyField; const Value: string);
  begin
    MainForm.Inspector.FocusField(Field);
    Edit := MainForm.Inspector.EditorFor(Field) as TEdit;
    Edit.Text := Value;
    Check(MainForm.Inspector.PrepareForSelectionChange(ErrorText),
      'Inspector 3D edit failed: ' + ErrorText);
  end;
begin
  SetLiveTeXEnabled(False);
  MainForm.Show;
  MainForm.ShowInspector;
  Application.ProcessMessages;
  Drawing := MainForm.TheDrawing;
  Drawing.DeleteAllObjects;
  MainForm.EventManager.SendMessage(Msg_InsertTorus, nil);
  TInsertSizedGrObjMode(MainForm.EventManager.Mode).PointSelected(Point2D(10, 10));
  TInsertSizedGrObjMode(MainForm.EventManager.Mode).PointSelected(Point2D(70, 50));
  Torus := Drawing.ObjectList.FirstObj as TSolid3D;
  Drawing.SelectionClear;
  Drawing.SelectionAdd(Torus);
  MainForm.RefreshInspectorSelection;
  Check(MainForm.Inspector.FieldShown(opfSolidSize1) and
    MainForm.Inspector.FieldShown(opfSolidSize2) and
    not MainForm.Inspector.FieldShown(opfSolidSize3) and
    MainForm.Inspector.FieldShown(opfSolidSegments),
    'Inspector does not show the torus radii and segments');
  Check(not MainForm.Inspector.FieldShown(opfBeginArrowKind),
    'Inspector offers arrows for a 3D object');
  Torus.GetPlacement(Origin, Scale, Angle);
  Pieces0 := Torus.Pieces.Count;
  ChangeNumber(opfSolidSize1, '30');
  Torus := Drawing.ObjectList.FirstObj as TSolid3D;
  Check(Abs(Torus.Bodies[0].Size1 * Scale - 30) < 1E-3,
    'Inspector did not set the torus radius in drawing mm');
  ChangeNumber(opfSolidSegments, '12');
  Torus := Drawing.ObjectList.FirstObj as TSolid3D;
  Check((Torus.Bodies[0].SegU = 12) and (Torus.Pieces.Count < Pieces0),
    'Inspector segment edit did not re-render the torus');
  MainForm.Inspector.ShowMore := True;
  Check(MainForm.Inspector.FieldShown(opfSolidAzimuth) and
    MainForm.Inspector.FieldShown(opfSolidPerspective),
    'More properties does not show the 3D camera');
  ChangeNumber(opfSolidAzimuth, '-30');
  MainForm.Inspector.FocusField(opfSolidPerspective);
  Box := MainForm.Inspector.EditorFor(opfSolidPerspective) as TCheckBox;
  Box.Checked := True;
  if Assigned(Box.OnChange) then Box.OnChange(Box);
  Check(MainForm.Inspector.PrepareForSelectionChange(ErrorText), ErrorText);
  Torus := Drawing.ObjectList.FirstObj as TSolid3D;
  Check((Abs(Torus.Azimuth + 30) < 1E-6) and Torus.Perspective,
    'Inspector camera edits did not reach the 3D object');
  // Undo steps back through the Inspector edits.
  Drawing.History.Undo;
  Torus := Drawing.ObjectList.FirstObj as TSolid3D;
  Check(not Torus.Perspective, 'Undo did not revert the Inspector 3D edit');
  Drawing.History.Redo;
  FileName := GetTempFileName(GetTempDir, 'tpx-solid-inspector-') + '.TpX';
  try
    Check(StoreToFile_TpX(Drawing, FileName, False), 'saving the edited torus failed');
    Saved := Solid3DReadFile(FileName);
    Check((Pos('kind="torus"', string(Saved)) > 0) and
      (Pos('segments="12"', string(Saved)) > 0) and
      (Pos('az="-30"', string(Saved)) > 0) and (Pos('persp="1"', string(Saved)) > 0),
      'the saved file does not carry the Inspector 3D edits');
    Reloaded := Solid3DLoad('tpx', FileName, Saved, Ctx);
    try
      Torus := Reloaded.ObjectList.FirstObj as TSolid3D;
      Torus.GetPlacement(Origin, Scale, Angle);
      Check(Abs(Torus.Bodies[0].Size1 * Scale - 30) < 1E-3,
        'the reopened torus lost its Inspector radius');
    finally
      Reloaded.Free;
      Ctx.Free;
    end;
  finally
    DeleteFile(FileName);
  end;
  // A box beside the torus: sizes mean different things, so they hide.
  Drawing.AddObject(-1, TSolid3D.Create(-1));
  TSolid3D(Drawing.ObjectList.LastObj).AddBody(skBox);
  TSolid3D(Drawing.ObjectList.LastObj).FinishFirstDraw;
  Drawing.SelectAll;
  MainForm.RefreshInspectorSelection;
  Check(not MainForm.Inspector.FieldShown(opfSolidSize1) and
    MainForm.Inspector.FieldShown(opfSolidAzimuth),
    'Inspector shows ambiguous 3D sizes for a box beside a torus');
  Drawing.SelectionClear;
  Drawing.DeleteAllObjects;
  WriteLn('PASS solid3d-inspector');
end;
