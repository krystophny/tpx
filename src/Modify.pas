unit Modify;

// This module contains various routines for modification of
// a drawing and its objects

interface

uses
  Geometry, Graphics, Drawings, GObjBase, GObjects, Solid3D;

type
  TMvBackwardForward = (mv_Forward, mv_Backward,
    mv_ToFront, mv_ToBack);
  TObjectsAlignment = (align_Left, align_HCenter, align_Right,
    align_Bottom, align_VCenter, align_Top);

procedure BackwardForward(const ADrawing: TDrawing2D;
  const Mv: TMvBackwardForward);
procedure AlignSelected(const ADrawing: TDrawing2D;
  const Alignment: TObjectsAlignment);
procedure DuplicateSelected(const ADrawing: TDrawing2D;
  const Shift: TVector2D);
procedure GroupSelected(const ADrawing: TDrawing2D);
procedure UngroupSelected(const ADrawing: TDrawing2D);
// 3D objects
procedure ApplySolidDefaults(const Solid: TSolid3D);
function SolidFromSelected(const ADrawing: TDrawing2D;
  const Kind: TSolidKind): Boolean;
function AllSelectedAreSolids(const ADrawing: TDrawing2D): Boolean;
procedure SetSelectedSolidsView(const ADrawing: TDrawing2D;
  const View: TSolidView);
procedure ToggleSelectedSolidsPerspective(const ADrawing: TDrawing2D);
function MergeSolids(const Solids: TGraphicObjList): TSolid3D;
procedure CompoundFromSelected(const ADrawing: TDrawing2D);
procedure UncompoundSelected(const ADrawing: TDrawing2D);
function ScaleStandard(const ADrawing: TDrawing2D;
  const ScaleStandardMaxWidth,
  ScaleStandardMaxHeight: TRealType): TTransf2D;
procedure ScalePhysical(const ADrawing: TDrawing2D;
  const S: TRealType; const DoNotify: Boolean);
function CanConvertSelected(const ADrawing: TDrawing2D;
  const DestClass: TPrimitive2DClass): Boolean;
procedure ConvertSelected(const ADrawing: TDrawing2D;
  const DestClass: TPrimitive2DClass);
procedure SimplifyPoly(const ADrawing: TDrawing2D;
  const Aperture: TRealType);
procedure SimplifyBezierPaths(const ADrawing: TDrawing2D;
  const Aperture: TRealType);
procedure ConnectPaths(const ADrawing: TDrawing2D);
procedure SelectedReversePoints(const ADrawing: TDrawing2D);
procedure MakeGrayscale(const ADrawing: TDrawing2D);
procedure BreakPath(const ADrawing: TDrawing2D;
  const Obj0: TPrimitive2D;
  const P: TPoint2D; const Aperture, Precision: TRealType);
procedure DeleteSmallObjects(const ADrawing: TDrawing2D;
  const D: TRealType);

type
  // Type for procedures which change properties of a group of graphical objects
  TChangeProc = procedure(
    const Obj: TGraphicObject; PData: Pointer);

    // Change properties of a group of graphical objects
procedure ChangeObjects(const Objects: TGraphicObjList;
  const ChangeProc: TChangeProc; const PData: Pointer);
procedure ChangeSelected(const ADrawing: TDrawing2D;
  const ChangeProc: TChangeProc; const PData: Pointer);
procedure ChangeLineStyle(
  const Obj: TGraphicObject; PData: Pointer);
procedure ChangeLineColor(
  const Obj: TGraphicObject; PData: Pointer);
procedure ChangeHatching(
  const Obj: TGraphicObject; PData: Pointer);
procedure ChangeHatchColor(
  const Obj: TGraphicObject; PData: Pointer);
procedure ChangeFillColor(
  const Obj: TGraphicObject; PData: Pointer);
procedure ChangeLineWidth(
  const Obj: TGraphicObject; PData: Pointer);
procedure ChangeBeginArrowKind(
  const Obj: TGraphicObject; PData: Pointer);
procedure ChangeEndArrowKind(
  const Obj: TGraphicObject; PData: Pointer);
procedure ChangeArrowSizeFactor(
  const Obj: TGraphicObject; PData: Pointer);

type
  TChangePropertiesKind = (chpLS, chpLC, chpLW,
    chpHa, chpHC, chpFC, chpArr1, chpArr2, chpArrS,
    chpFH, chpHJ, chpSK, chpSS);
  TChangePropertiesSet = set of TChangePropertiesKind;

procedure ChangeSelectedProperties(const ADrawing: TDrawing2D;
  const ChangePropertiesSet: TChangePropertiesSet);

implementation

uses Math, ColorEtc, Devices, SysUtils;

procedure BackwardForward(const ADrawing: TDrawing2D;
  const Mv: TMvBackwardForward);
var
  Position: TGraphicObject;
  CurrentObj: TGraphicObject;
  MoveMore: Boolean;
begin
  if ADrawing.SelectedObjects.Count = 0 then Exit;
  MoveMore := False;
  case Mv of
    mv_Forward:
      begin
        ADrawing.ObjectList.FindObjByID(
          ADrawing.SelectedObjects.LastObj.ID);
        Position := ADrawing.ObjectList.NextObj;
        if Position = nil then
          Position := ADrawing.ObjectList.LastObj;
        MoveMore := True;
      end;
    mv_Backward:
      begin
        ADrawing.ObjectList.FindObjByID(
          ADrawing.SelectedObjects.LastObj.ID);
        Position := ADrawing.ObjectList.PrevObj;
        if Position = nil then
          Position := ADrawing.ObjectList.FirstObj;
      end;
    mv_ToFront:
      begin
        MoveMore := True;
        Position := ADrawing.ObjectList.LastObj;
      end;
    mv_ToBack: Position := ADrawing.ObjectList.FirstObj;
  end;
  CurrentObj := ADrawing.SelectedObjects.LastObj;
  while CurrentObj <> nil do
  begin
    if CurrentObj.ID <> Position.ID then
    begin
      ADrawing.MoveObject(CurrentObj.ID,
        Position.ID);
      if MoveMore then
        ADrawing.MoveObject(Position.ID,
          CurrentObj.ID);
    end;
    CurrentObj := ADrawing.SelectedObjects.PrevObj;
  end;
end;

procedure AlignSelected(const ADrawing: TDrawing2D;
  const Alignment: TObjectsAlignment);
var
  Obj: TObject2D;
  R: TRect2D;
  A, TX, TY: TRealType;
begin
  if ADrawing.SelectedObjects.Count = 0 then Exit;
  ADrawing.SelectedObjects.Lock;
  try
    R := GetExtension0(ADrawing, ADrawing.SelectedObjects);
    case Alignment of
      align_Left: A := R.Left;
      align_HCenter: A := (R.Left + R.Right) / 2;
      align_Right: A := R.Right;
      align_Bottom: A := R.Bottom;
      align_VCenter: A := (R.Bottom + R.Top) / 2;
      align_Top: A := R.Top;
    end;
    TX := 0;
    TY := 0;
    Obj := ADrawing.SelectedObjects.FirstObj as TObject2D;
    while Obj <> nil do
    begin
      R := Obj.BoundingBox;
      case Alignment of
        align_Left: TX := A - R.Left;
        align_HCenter: TX := A - (R.Left + R.Right) / 2;
        align_Right: TX := A - R.Right;
        align_Bottom: TY := A - R.Bottom;
        align_VCenter: TY := A - (R.Bottom + R.Top) / 2;
        align_Top: TY := A - R.Top;
      end;
      Obj.TransForm(Translate2D(TX, TY));
      Obj := ADrawing.SelectedObjects.NextObj as TObject2D;
    end;
  finally
    ADrawing.SelectedObjects.Unlock;
    ADrawing.NotifyChanged;
  end;
end;

procedure DuplicateSelected(const ADrawing: TDrawing2D;
  const Shift: TVector2D);
var
  Obj, NewObj: TGraphicObject;
  NewObjs: TGraphicObjList;
  T: TTransf2D;
  OnChangeDrawing0: TOnChangeDrawing;
begin
  NewObjs := TGraphicObjList.Create;
  NewObjs.FreeOnDelete := False;
  OnChangeDrawing0 := ADrawing.OnChangeDrawing;
  ADrawing.OnChangeDrawing := nil;
  try
    ADrawing.SelectedObjects.Lock;
    try
      Obj := ADrawing.SelectedObjects.FirstObj;
      while Obj <> nil do
      begin
        NewObj :=
          TpXFindClassByName(Obj.ClassName).CreateDupe(Obj);
        NewObjs.Add(NewObj);
        Obj := ADrawing.SelectedObjects.NextObj;
      end;
      T := Translate2D(Shift.X * 5, Shift.Y * 5);
      Obj := ADrawing.SelectedObjects.FirstObj;
      while Obj <> nil do
      begin
        (Obj as TObject2D).TransForm(T);
        Obj := ADrawing.SelectedObjects.NextObj;
      end;
    finally
      ADrawing.SelectedObjects.Unlock;
    end;
    ADrawing.AddList(NewObjs);
    ADrawing.SelectionClear;
    ADrawing.SelectionAddList(NewObjs);
  finally
    ADrawing.OnChangeDrawing := OnChangeDrawing0;
    NewObjs.Free;
  end;
end;

procedure ReplaceSelectedBy(const ADrawing: TDrawing2D;
  const NewObj: TObject2D);
var
  Obj: TGraphicObject;
  OnChangeDrawing0: TOnChangeDrawing;
begin
  OnChangeDrawing0 := ADrawing.OnChangeDrawing;
  ADrawing.OnChangeDrawing := nil;
  try
    Obj := ADrawing.SelectedObjects.Pop;
    ADrawing.DeleteSelected;
    ADrawing.AddObject(-1, NewObj);
    ADrawing.MoveObject(NewObj.ID, Obj.ID);
    ADrawing.DeleteObject(Obj.ID);
    ADrawing.SelectionAdd(NewObj);
  finally
    ADrawing.OnChangeDrawing := OnChangeDrawing0;
  end;
  ADrawing.NotifyChanged;
end;

function AllSelectedAreSolids(const ADrawing: TDrawing2D): Boolean;
var
  Obj: TGraphicObject;
begin
  Result := ADrawing.SelectedObjects.Count > 0;
  Obj := ADrawing.SelectedObjects.FirstObj;
  while Obj <> nil do
  begin
    if not (Obj is TSolid3D) then
    begin
      Result := False;
      Exit;
    end;
    Obj := ADrawing.SelectedObjects.NextObj;
  end;
end;

procedure ApplySolidDefaults(const Solid: TSolid3D);
begin
  // Faces must be opaque for hidden surfaces; untouched default lines
  // become the light grey edges of the clean 3D style.
  if (Solid.FillColor = clDefault) or (Solid.FillColor = clNone) then
    Solid.FillColor := clWhite;
  if Solid.LineColor = clDefault then
  begin
    Solid.LineColor := Solid_DefaultLineColor;
    if Solid.LineWidth = 1 then Solid.LineWidth := Solid_DefaultLineWidth;
  end;
end;

function SolidFromSelected(const ADrawing: TDrawing2D;
  const Kind: TSolidKind): Boolean;
var
  Shapes: TGraphicObjList;
  Obj: TGraphicObject;
  AxisStart, AxisEnd: TPoint2D;
  Axis: TGraphicObject;
  HasAxis: Boolean;
  R: TRect2D;
  First: Boolean;
  Solid: TSolid3D;
  Body: TSolidBody;
begin
  Result := False;
  Shapes := TGraphicObjList.Create;
  Shapes.FreeOnDelete := False;
  try
    HasAxis := False;
    Axis := nil;
    First := True;
    Obj := ADrawing.SelectedObjects.FirstObj;
    while Obj <> nil do
    begin
      if IsClosedProfile(Obj) then
      begin
        Shapes.Add(Obj);
        if First then R := (Obj as TObject2D).BoundingBox
        else R := BoxOutBox2D(R, (Obj as TObject2D).BoundingBox);
        First := False;
      end
      else if (Obj is TLine2D) and (Kind = skRevolve) and not HasAxis then
      begin
        // A selected straight line is the axis of the revolution.
        HasAxis := True;
        AxisStart := TLine2D(Obj).Points[0];
        AxisEnd := TLine2D(Obj).Points[1];
        if IsSamePoint2D(AxisStart, AxisEnd) then Exit;
        Axis := Obj;
      end;
      Obj := ADrawing.SelectedObjects.NextObj;
    end;
    if Shapes.Count = 0 then Exit;
    if not HasAxis then
    begin
      AxisStart := Point2D(R.Left, R.Bottom);
      AxisEnd := Point2D(R.Left, R.Top);
      if IsSamePoint2D(AxisStart, AxisEnd) then AxisEnd.Y := AxisStart.Y + 1;
    end;
    Solid := CreateSolidFromProfiles(Shapes, Kind, AxisStart, AxisEnd);
    ApplySolidDefaults(Solid);
    if Kind = skRevolve then
    begin
      // Leave a quarter open toward the viewer, like a cut-away drawing.
      Body := Solid.Bodies[0];
      Body.Size1 := 270;
      Body.Size2 := Solid.Azimuth + 45;
    end;
    Solid.UpdateExtension(Solid);
    // Replace only the profiles and the chosen axis; leave other selected
    // drawing objects intact.
    ADrawing.SelectionClear;
    ADrawing.SelectionAddList(Shapes);
    if Axis <> nil then ADrawing.SelectionAdd(Axis);
    ReplaceSelectedBy(ADrawing, Solid);
    Result := True;
  finally
    Shapes.Free;
  end;
end;

// Rotation part of the camera: rows are the screen right, up and view axis.
procedure ViewRows(const Solid: TSolid3D; out M: TMat34);
var
  Az, El: Double;
begin
  Az := Solid.Azimuth * Pi / 180;
  El := Solid.Elevation * Pi / 180;
  M := IdentityMat34;
  M[0] := -Sin(Az); M[1] := Cos(Az); M[2] := 0;
  M[4] := -Sin(El) * Cos(Az); M[5] := -Sin(El) * Sin(Az); M[6] := Cos(El);
  M[8] := Cos(El) * Cos(Az); M[9] := Cos(El) * Sin(Az); M[10] := Sin(El);
end;

function TransposeRot(const M: TMat34): TMat34;
begin
  Result := IdentityMat34;
  Result[0] := M[0]; Result[1] := M[4]; Result[2] := M[8];
  Result[4] := M[1]; Result[5] := M[5]; Result[6] := M[9];
  Result[8] := M[2]; Result[9] := M[6]; Result[10] := M[10];
end;

function MergeSolids(const Solids: TGraphicObjList): TSolid3D;
var
  Obj: TGraphicObject;
  First, Solid: TSolid3D;
  V1, VI, T, RZ: TMat34;
  O1, OI: TPoint2D;
  S1, A1, SI, AI, K: Double;
  Offset: TVec3;
  I: Integer;
  Body: TSolidBody;
begin
  First := Solids.FirstObj as TSolid3D;
  Result := TSolid3D.Create(-1);
  Result.Assign(First);
  First.GetPlacement(O1, S1, A1);
  ViewRows(First, V1);
  Obj := Solids.NextObj;
  while Obj <> nil do
  begin
    Solid := Obj as TSolid3D;
    Solid.GetPlacement(OI, SI, AI);
    ViewRows(Solid, VI);
    // Keep each body's picture: map its scene into the first camera with
    // the relative scale, in-plane rotation and offset of its placement.
    K := SI / S1;
    RZ := IdentityMat34;
    RZ[0] := Cos(AI - A1) * K; RZ[1] := -Sin(AI - A1) * K;
    RZ[4] := Sin(AI - A1) * K; RZ[5] := Cos(AI - A1) * K;
    RZ[10] := K;
    T := MulMat34(TransposeRot(V1), MulMat34(RZ, VI));
    Offset := First.UnprojectOffset(Point2D(OI.X - O1.X, OI.Y - O1.Y));
    T[3] := Offset.X;
    T[7] := Offset.Y;
    T[11] := Offset.Z;
    for I := 0 to Solid.BodyCount - 1 do
    begin
      Body := TSolidBody.Create(Solid.Bodies[I].Kind);
      Body.Assign(Solid.Bodies[I]);
      Body.M := MulMat34(T, Body.M);
      Result.AdoptBody(Body);
    end;
    Obj := Solids.NextObj;
  end;
  Result.UpdateExtension(Result);
end;

procedure SetSelectedSolidsView(const ADrawing: TDrawing2D;
  const View: TSolidView);
var
  Obj: TGraphicObject;
begin
  Obj := ADrawing.SelectedObjects.FirstObj;
  while Obj <> nil do
  begin
    if Obj is TSolid3D then TSolid3D(Obj).ApplyView(View);
    Obj := ADrawing.SelectedObjects.NextObj;
  end;
  ADrawing.NotifyChanged;
end;

procedure ToggleSelectedSolidsPerspective(const ADrawing: TDrawing2D);
var
  Obj: TGraphicObject;
begin
  Obj := ADrawing.SelectedObjects.FirstObj;
  while Obj <> nil do
  begin
    if Obj is TSolid3D then
    begin
      TSolid3D(Obj).Perspective := not TSolid3D(Obj).Perspective;
      TSolid3D(Obj).UpdateExtension(Obj);
    end;
    Obj := ADrawing.SelectedObjects.NextObj;
  end;
  ADrawing.NotifyChanged;
end;

procedure GroupSelected(const ADrawing: TDrawing2D);
var
  Group: TGroup2D;
  Obj: TGraphicObject;
  OnChangeDrawing0: TOnChangeDrawing;
begin
  if ADrawing.SelectedObjects.Count <= 1 then Exit;
  if AllSelectedAreSolids(ADrawing) then
  begin
    // Grouped 3D objects become one scene with a shared camera.
    ReplaceSelectedBy(ADrawing, MergeSolids(ADrawing.SelectedObjects));
    Exit;
  end;
  OnChangeDrawing0 := ADrawing.OnChangeDrawing;
  ADrawing.OnChangeDrawing := nil;
  try
    Group := TGroup2D.Create(-1);
    try
      Obj := ADrawing.SelectedObjects.FirstObj;
      while Obj <> nil do
      begin
        Group.Objects.Add(TpXFindClassByName(
          Obj.ClassName).CreateDupe(Obj));
        Obj := ADrawing.SelectedObjects.NextObj;
      end;
      Obj := ADrawing.SelectedObjects.Pop;
      ADrawing.DeleteSelected;
      ADrawing.AddObject(-1, Group);
      ADrawing.MoveObject(Group.ID, Obj.ID);
      ADrawing.DeleteObject(Obj.ID);
      ADrawing.SelectionAdd(Group);
    except
      Group.Free;
    end;
  finally
    ADrawing.OnChangeDrawing := OnChangeDrawing0;
  end;
  ADrawing.NotifyChanged;
end;

// A scene splits into one object per body; a single extruded or revolved
// body returns its profiles to 2D editing.
function SplitSolid(const ADrawing: TDrawing2D; const Solid: TSolid3D;
  const Added: TGraphicObjList): Boolean;
var
  Parts: TGraphicObjList;
  Part: TSolid3D;
  Body: TSolidBody;
  Obj: TGraphicObject;
  Copy: TPrimitive2D;
  Origin: TPoint2D;
  Scale, Angle: Double;
  I: Integer;
  T: TTransf2D;
begin
  Result := False;
  Parts := TGraphicObjList.Create;
  Parts.FreeOnDelete := False;
  try
    if Solid.BodyCount > 1 then
      for I := 0 to Solid.BodyCount - 1 do
      begin
        Part := TSolid3D.Create(-1);
        Part.Assign(Solid);
        while Part.BodyCount > 0 do Part.ExtractBody(0).Free;
        Body := TSolidBody.Create(Solid.Bodies[I].Kind);
        Body.Assign(Solid.Bodies[I]);
        Part.AdoptBody(Body);
        Part.UpdateExtension(Part);
        Parts.Add(Part);
      end
    else if (Solid.BodyCount = 1) and
      (Solid.Bodies[0].Kind in [skExtrude, skRevolve]) then
    begin
      Solid.GetPlacement(Origin, Scale, Angle);
      T := MultiplyTransform2D(Scale2D(Scale, Scale),
        MultiplyTransform2D(Rotate2D(Angle), Translate2D(Origin.X, Origin.Y)));
      Obj := Solid.Bodies[0].Profiles.FirstObj;
      while Obj <> nil do
      begin
        Copy := TPrimitive2D(TpXFindClassByName(Obj.ClassName).Create(-1));
        Copy.Assign(Obj);
        Copy.Transform(T);
        Parts.Add(Copy);
        Obj := Solid.Bodies[0].Profiles.NextObj;
      end;
    end
    else Exit;
    ADrawing.SelectedObjects.RemoveCurrent;
    ADrawing.InsertList(Solid.ID, Parts);
    Added.AddFromList(Parts);
    ADrawing.DeleteObject(Solid.ID);
    Result := True;
  except
    Parts.FreeOnDelete := True;
    raise;
  end;
  Parts.Free;
end;

procedure UngroupSelected(const ADrawing: TDrawing2D);
var
  Group: TGroup2D;
  LastObj: TGraphicObject;
  OnChangeDrawing0: TOnChangeDrawing;
  TmpLst: TGraphicObjList;
begin
  OnChangeDrawing0 := ADrawing.OnChangeDrawing;
  ADrawing.OnChangeDrawing := nil;
  TmpLst := TGraphicObjList.Create;
  TmpLst.FreeOnDelete := False;
  try
    ADrawing.SelectedObjects.Lock;
    try
      ADrawing.SelectedObjects.FirstObj;
      while ADrawing.SelectedObjects.CurrentObj <> nil do
      begin
        if (ADrawing.SelectedObjects.CurrentObj is TSolid3D) and
          SplitSolid(ADrawing, ADrawing.SelectedObjects.CurrentObj as TSolid3D,
          TmpLst) then
          // SplitSolid removed the solid from the selection.
        else if ADrawing.SelectedObjects.CurrentObj is TGroup2D then
        begin
          Group := ADrawing.SelectedObjects.CurrentObj as TGroup2D;
          ADrawing.SelectedObjects.RemoveCurrent;
          ADrawing.InsertList(Group.ID, Group.Objects);
          TmpLst.AddFromList(Group.Objects);
          Group.Objects.FreeOnDelete := False;
          Group.Objects.Clear;
          ADrawing.DeleteObject(Group.ID);
        end
        else
          ADrawing.SelectedObjects.NextObj;
      end;
    finally
      ADrawing.SelectedObjects.Unlock;
    end;
    ADrawing.SelectionAddList(TmpLst);
  finally
    TmpLst.Free;
    ADrawing.OnChangeDrawing := OnChangeDrawing0;
  end;
  ADrawing.NotifyChanged;
end;

procedure CompoundFromSelected(const ADrawing: TDrawing2D);
var
  Compound: TCompound2D;
  OnChangeDrawing0: TOnChangeDrawing;
  Obj: TGraphicObject;
  procedure ProcessList(ObjList: TGraphicObjList);
  var
    I: Integer;
    Obj: TGraphicObject;
    Prim: TPrimitive2D;
    Group: TGroup2D;
  begin
    Obj := ObjList.FirstObj;
    while Obj <> nil do
    begin
      if Obj is TPrimitive2D then
      begin
        Prim := Obj as TPrimitive2D;
        Prim.Pieces.Clear;
        Prim.FillPieces;
        for I := 0 to Prim.Pieces.Count - 1 do
          Compound.AddPiece(Prim.Pieces[I]);
      end
      else if Obj is TGroup2D then
      begin
        Group := Obj as TGroup2D;
        // Recursion
        ProcessList(Group.Objects);
      end;
      Obj := ObjList.NextObj;
    end;
  end;
begin
  if ADrawing.SelectedObjects.Count < 1 then Exit;
  OnChangeDrawing0 := ADrawing.OnChangeDrawing;
  ADrawing.OnChangeDrawing := nil;
  try
    Compound := TCompound2D.Create(-1);
    Compound.Points.DisableEvents := True;
    try
      Compound.AssignProperties(
        ADrawing.SelectedObjects.Peek);
      Compound.Points.Clear;
      ProcessList(ADrawing.SelectedObjects);
      Obj := ADrawing.SelectedObjects.Pop;
      ADrawing.DeleteSelected;
      ADrawing.AddObject(-1, Compound);
      ADrawing.MoveObject(Compound.ID, Obj.ID);
      ADrawing.DeleteObject(Obj.ID);
      ADrawing.SelectionAdd(Compound);
    except
      Compound.Free;
    end;
    Compound.Points.DisableEvents := False;
  finally
    ADrawing.OnChangeDrawing := OnChangeDrawing0;
  end;
  ADrawing.NotifyChanged;
end;

procedure UncompoundSelected(const ADrawing: TDrawing2D);
var
  Compound: TCompound2D;
  LastObj: TGraphicObject;
  OnChangeDrawing0: TOnChangeDrawing;
  SelLst, CmpdLst: TGraphicObjList;
begin
  OnChangeDrawing0 := ADrawing.OnChangeDrawing;
  ADrawing.OnChangeDrawing := nil;
  SelLst := TGraphicObjList.Create;
  SelLst.FreeOnDelete := False;
  CmpdLst := TGraphicObjList.Create;
  CmpdLst.FreeOnDelete := False;
  try
    ADrawing.SelectedObjects.Lock;
    try
      ADrawing.SelectedObjects.FirstObj;
      while ADrawing.SelectedObjects.CurrentObj <> nil do
      begin
        if ADrawing.SelectedObjects.CurrentObj is TCompound2D then
        begin
          Compound := ADrawing.SelectedObjects.CurrentObj as
            TCompound2D;
          ADrawing.SelectedObjects.RemoveCurrent;
          UncompoundCompound(Compound, CmpdLst);
          ADrawing.InsertList(Compound.ID, CmpdLst);
          SelLst.AddFromList(CmpdLst);
          ADrawing.DeleteObject(Compound.ID);
        end
        else
          ADrawing.SelectedObjects.NextObj;
      end;
    finally
      ADrawing.SelectedObjects.Unlock;
    end;
    ADrawing.SelectionAddList(SelLst);
  finally
    SelLst.Free;
    CmpdLst.Free;
    ADrawing.OnChangeDrawing := OnChangeDrawing0;
  end;
  ADrawing.NotifyChanged;
end;

function ScaleStandard(const ADrawing: TDrawing2D;
  const ScaleStandardMaxWidth,
  ScaleStandardMaxHeight: TRealType): TTransf2D;
var
  Rect: TRect2D;
  ScaleX, ScaleY, Scale: TRealType;
begin
  Rect := ADrawing.DrawingExtension;
  if Rect.Top <= Rect.Bottom then Exit;
  if Rect.Right <= Rect.Left then Exit;
  if ADrawing.PicScale <= 0 then Exit;
  Result := Translate2D(-Rect.Left, -Rect.Bottom);
  ScaleX := ScaleStandardMaxWidth / (Rect.Right - Rect.Left);
  ScaleY := ScaleStandardMaxHeight / (Rect.Top - Rect.Bottom);
  Scale := Min(ScaleX, ScaleY) / ADrawing.PicScale;
  Result := MultiplyTransform2D(Result, Scale2D(Scale, Scale));
  ADrawing.ObjectList.TransForm(Result);
end;

procedure ScalePhysical(const ADrawing: TDrawing2D;
  const S: TRealType; const DoNotify: Boolean);
begin
//  fDrawing2D.PicScale := fDrawing2D.PicScale * Magnif;
  ADrawing.Border := ADrawing.Border * S;
  if not ScaleLineWidth then
    ADrawing.LineWidthBase := ADrawing.LineWidthBase * S;
  ADrawing.HatchingStep := ADrawing.HatchingStep * S;
  ADrawing.DottedSize := ADrawing.DottedSize * S;
  ADrawing.DashSize := ADrawing.DashSize * S;
  if DoNotify then
    ADrawing.History.SetPropertiesChanged;
end;

function CanConvertSelected(const ADrawing: TDrawing2D;
  const DestClass: TPrimitive2DClass): Boolean;
var
  Obj: TGraphicObject;
begin
  Result := False;
  if ADrawing.SelectedObjects.Count = 0 then Exit;
  if DestClass = TBitmap2D then
  begin
    Obj := ADrawing.SelectedObjects.FirstObj;
    while Obj <> nil do
    begin
      if not (Obj is TBitmap2D) then Exit;
      if not Assigned(TBitmap2D(Obj).BitmapEntry) then Exit;
      Obj := ADrawing.SelectedObjects.NextObj;
    end;
  end;
  Result := True;
end;

procedure ConvertSelected(const ADrawing: TDrawing2D;
  const DestClass: TPrimitive2DClass);
var
  Current, NewObj: TGraphicObject;
  NewObjs: TGraphicObjList;
begin
  if not CanConvertSelected(ADrawing, DestClass) then
    raise Exception.Create('Cannot convert the selection to a bitmap without an image resource');
  NewObjs := TGraphicObjList.Create;
  NewObjs.FreeOnDelete := False;
  try
    ADrawing.SelectedObjects.Lock;
    ADrawing.ObjectList.Lock;
    try
      Current := ADrawing.SelectedObjects.FirstObj;
      while Current <> nil do
      begin
        NewObj := DestClass.Create(Current.ID);
        NewObj.Assign(Current);
        ADrawing.SelectedObjects.ReplaceDeleteCurrent(NewObj);
        ADrawing.ObjectList.FindObjByID(Current.ID);
        ADrawing.ObjectList.ReplaceDeleteCurrent(NewObj);
        NewObjs.Add(NewObj);
          //NewObj.ID := Current.ID;
        Current := ADrawing.SelectedObjects.NextObj;
      end;
    finally
      ADrawing.SelectedObjects.Unlock;
      ADrawing.ObjectList.Unlock;
    end;
  finally
    ADrawing.SelectionClear;
    ADrawing.SelectionAddList(NewObjs);
    NewObjs.Free;
    ADrawing.NotifyChanged;
  end;
end;

procedure SimplifyPoly(const ADrawing: TDrawing2D;
  const Aperture: TRealType);
var
  Current, NewObj: TGraphicObject;
  NewObjs: TGraphicObjList;
begin
  NewObjs := TGraphicObjList.Create;
  NewObjs.FreeOnDelete := False;
  try
    ADrawing.SelectedObjects.Lock;
    ADrawing.ObjectList.Lock;
    try
      Current := ADrawing.SelectedObjects.FirstObj;
      while Current <> nil do
      begin
        if Current is TPolyline2D0 then
        begin
          if Current is TPolyline2D then
            NewObj := TPolyline2D.Create(Current.ID)
          else
            NewObj := TPolygon2D.Create(Current.ID);
          NewObj.Assign(Current);
          Geometry.SimplifyPoly((Current as TPrimitive2D).Points,
            (NewObj as TPrimitive2D).Points,
            Aperture, Current is TPolygon2D);
          ADrawing.SelectedObjects.ReplaceDeleteCurrent(NewObj);
          ADrawing.ObjectList.FindObjByID(Current.ID);
          ADrawing.ObjectList.ReplaceDeleteCurrent(NewObj);
          NewObjs.Add(NewObj);
        end;
          //NewObj.ID := Current.ID;
        Current := ADrawing.SelectedObjects.NextObj;
      end;
    finally
      ADrawing.SelectedObjects.Unlock;
      ADrawing.ObjectList.Unlock;
    end;
  finally
    ADrawing.SelectionClear;
    ADrawing.SelectionAddList(NewObjs);
    NewObjs.Free;
    ADrawing.NotifyChanged;
  end;
end;

procedure SimplifyBezierPaths(const ADrawing: TDrawing2D;
  const Aperture: TRealType);
var
  Current: TGraphicObject;
  NewObjs: TGraphicObjList;
begin
  NewObjs := TGraphicObjList.Create;
  NewObjs.FreeOnDelete := False;
  try
    ADrawing.SelectedObjects.Lock;
    try
      Current := ADrawing.SelectedObjects.FirstObj;
      while Current <> nil do
      begin
        if Current is TBezierPath2D0 then
        begin
          Geometry.SimplifyBezierPath(
            (Current as TPrimitive2D).Points,
            Aperture, Current is TClosedBezierPath2D);
          NewObjs.Add(Current);
        end;
        Current := ADrawing.SelectedObjects.NextObj;
      end;
    finally
      ADrawing.SelectedObjects.Unlock;
    end;
  finally
    ADrawing.SelectionClear;
    ADrawing.SelectionAddList(NewObjs);
    NewObjs.Free;
    ADrawing.NotifyChanged;
  end;
end;

procedure ConnectPaths(const ADrawing: TDrawing2D);
var
  Prim, Prim2: TPrimitive2D;
  TempList: TGraphicObjList;
  OnChangeDrawing: TOnChangeDrawing;
  TmpPP, PPP: TPointsSet2D;
  IsLinearObj, IsBezierObj: Boolean;
  procedure CheckLengths;
  var
    L00, L01, L10, L11: TRealType;
  begin
    L00 := PointDistance2D(PPP[0], TmpPP[0]);
    L01 := PointDistance2D(PPP[0], TmpPP[TmpPP.Count - 1]);
    L10 := PointDistance2D(PPP[PPP.Count - 1], TmpPP[0]);
    L11 := PointDistance2D(PPP[PPP.Count - 1],
      TmpPP[TmpPP.Count - 1]);
    if Min(L10, L11) > Min(L00, L01) then PPP.ReversePoints;
    if Min(L00, L10) > Min(L01, L11) then TmpPP.ReversePoints;
  end;
  procedure ProcessObj(const Obj: TGraphicObject);
  begin
    IsLinearObj := (Obj is TLine2D) or (Obj is TPolyline2D);
    IsBezierObj := (Obj is TBezierPath2D)
      or (Obj is TSmoothPath2D) or (Obj is TArc2D);
    if not (IsLinearObj or IsBezierObj) then Exit;
    if (Obj as TPrimitive2D).Points.Count = 0 then Exit;
    if Prim = nil then // Initiate path
    begin
      if IsLinearObj then
      begin
        Prim := TPolyline2D.Create(-1);
        Prim.Assign(Obj);
        ADrawing.AddObject(-1, Prim);
        ADrawing.MoveObject(Prim.ID, Obj.ID);
        ADrawing.DeleteObject(Obj.ID);
      end
      else if IsBezierObj then
      begin
        Prim := TBezierPath2D.Create(Obj.ID);
        Prim.Assign(Obj);
        if not (Obj is TBezierPath2D) then
          (Obj as TPrimitive2D).BezierPoints(Prim.Points);
        ADrawing.AddObject(-1, Prim);
        ADrawing.MoveObject(Prim.ID, Obj.ID);
        ADrawing.DeleteObject(Obj.ID);
      end;
      Exit;
    end;
    // Change path type if necessary
    if (Prim is TPolyline2D) and IsBezierObj then
    begin
      Prim2 := TBezierPath2D.Create(-1);
      Prim2.Assign(Prim);
      ADrawing.AddObject(-1, Prim2);
      ADrawing.MoveObject(Prim2.ID, Prim.ID);
      ADrawing.DeleteObject(Prim.ID);
      Prim := Prim2;
    end;
    // Add to path
    TmpPP.Clear;
    PPP := Prim.Points;
    if Prim is TBezierPath2D then
    begin
      (Obj as TPrimitive2D).BezierPoints(TmpPP);
      CheckLengths;
      if IsSamePoint2D(PPP[PPP.Count - 1], TmpPP[0])
        then TmpPP.Delete(0)
      else
        PPP.AddPoints([
          MixPoint(PPP[PPP.Count - 1], TmpPP[0], 0.25),
            MixPoint(PPP[PPP.Count - 1], TmpPP[0], 0.75)]);
      PPP.AppendPoints(TmpPP);
      ADrawing.DeleteObject(Obj.ID);
    end
    else
    begin
      TmpPP.AppendPoints((Obj as TPrimitive2D).Points);
      CheckLengths;
      if IsSamePoint2D(PPP[PPP.Count - 1], TmpPP[0])
        then TmpPP.Delete(0);
      PPP.AppendPoints(TmpPP);
      ADrawing.DeleteObject(Obj.ID);
    end;
  end;
begin
  if ADrawing.SelectedObjects.Count = 0 then Exit;
  OnChangeDrawing := ADrawing.OnChangeDrawing;
  ADrawing.OnChangeDrawing := nil;
  TmpPP := TPointsSet2D.Create(0);
  try
    TempList := TGraphicObjList.Create;
    TempList.FreeOnDelete := False;
    TempList.AddFromList(ADrawing.SelectedObjects);
    ADrawing.SelectionClear;
    TempList.Lock;
    try
      Prim := nil;
      TempList.FirstObj;
      while TempList.CurrentObj <> nil do
      begin
        ProcessObj(TempList.CurrentObj);
        TempList.NextObj;
      end;
      ADrawing.SelectionClear;
    finally
      TempList.Unlock;
      TempList.Free;
      TmpPP.Free;
    end;
    if Assigned(Prim) then
      ADrawing.SelectionAdd(Prim);
  finally
    ADrawing.OnChangeDrawing := OnChangeDrawing;
    ADrawing.NotifyChanged;
  end;
end;

procedure SelectedReversePoints(const ADrawing: TDrawing2D);
var
  Obj: TGraphicObject;
begin
  if ADrawing.SelectedObjects.Count = 0 then Exit;
  Obj := ADrawing.SelectedObjects.FirstObj;
  while Obj <> nil do
  begin
    if Obj is TPrimitive2D then
      (Obj as TPrimitive2D).ReversePoints;
    Obj := ADrawing.SelectedObjects.NextObj;
  end;
  ADrawing.NotifyChanged;
end;

procedure MakeGrayscale(const ADrawing: TDrawing2D);
var
  Objects: TGraphicObjList;
  procedure Convert(const Objects: TGraphicObjList);
  var
    Obj: TGraphicObject;
  begin
    Obj := Objects.FirstObj;
    while Obj <> nil do
    begin
      if Obj is TPrimitive2D then
        with Obj as TPrimitive2D do
        begin
          if LineColor <> clDefault then
            LineColor := GrayScale(LineColor);
          if HatchColor <> clDefault then
            HatchColor := GrayScale(HatchColor);
          if FillColor <> clDefault then
            FillColor := GrayScale(FillColor);
        end
      else if Obj is TGroup2D then
        Convert((Obj as TGroup2D).Objects);
      Obj := Objects.NextObj;
    end;
  end;
begin
  if ADrawing.SelectedObjects.Count = 0 then
    Objects := ADrawing.ObjectList
  else
    Objects := ADrawing.SelectedObjects;
  Objects.Lock;
  try
    Convert(Objects);
  finally
    Objects.Unlock;
    ADrawing.NotifyChanged;
  end;
end;

procedure BreakPath(const ADrawing: TDrawing2D;
  const Obj0: TPrimitive2D;
  const P: TPoint2D; const Aperture, Precision: TRealType);
var
  Obj1, Obj2: TPrimitive2D;
  OnChangeDrawing: TOnChangeDrawing;
begin
  Obj0.BreakPath(P, Aperture, Precision, Obj1, Obj2);
  if Obj1 = nil then
  begin
    Obj1 := Obj2;
    Obj2 := nil;
  end;
  if Obj1 = nil then Exit;
  OnChangeDrawing := ADrawing.OnChangeDrawing;
  ADrawing.OnChangeDrawing := nil;
  try
    ADrawing.SelectionRemove(Obj0);
    ADrawing.AddObject(-1, Obj1);
    ADrawing.MoveObject(Obj1.ID, Obj0.ID);
    ADrawing.DeleteObject(Obj0.ID);
    ADrawing.SelectionAdd(Obj1);
    if Obj2 <> nil then
    begin
      ADrawing.InsertObject(-1, Obj1.ID, Obj2);
      ADrawing.SelectionAdd(Obj2);
    end;
  finally
    ADrawing.OnChangeDrawing := OnChangeDrawing;
    ADrawing.NotifyChanged;
  end;
end;

procedure DeleteSmallObjects(const ADrawing: TDrawing2D;
  const D: TRealType);
var
  Current: TGraphicObject;
  R: TRect2D;
  LeftObjs: TGraphicObjList;
  OnChangeDrawing0: TOnChangeDrawing;
  N: Integer;
begin
  if D <= 0 then Exit;
  LeftObjs := TGraphicObjList.Create;
  LeftObjs.FreeOnDelete := False;
  OnChangeDrawing0 := ADrawing.OnChangeDrawing;
  ADrawing.OnChangeDrawing := nil;
  N := 0;
  try
    ADrawing.SelectedObjects.Lock;
    try
      Current := ADrawing.SelectedObjects.FirstObj;
      while Current <> nil do
      begin
        if Current is TObject2D then
        begin
          R := (Current as TObject2D).BoundingBox;
          if ((R.Right - R.Left) < D)
            and ((R.Top - R.Bottom) < D) then
          begin
            Inc(N);
            ADrawing.DeleteObject(Current.ID)
          end
          else LeftObjs.Add(Current);
        end
        else LeftObjs.Add(Current);
        Current := ADrawing.SelectedObjects.NextObj;
      end;
    finally
      ADrawing.SelectedObjects.Unlock;
    end;
  finally
    if N > 0 then
    begin
      ADrawing.SelectedObjects.Clear;
      ADrawing.SelectionAddList(LeftObjs);
    end;
    LeftObjs.Free;
    ADrawing.OnChangeDrawing := OnChangeDrawing0;
    if N > 0 then
      ADrawing.NotifyChanged;
  end;
end;

// ============ ChangeObjects procedures

procedure ChangeObjects(const Objects: TGraphicObjList;
  const ChangeProc: TChangeProc; const PData: Pointer);
  procedure ProcessList(const Objects: TGraphicObjList);
  var
    Obj: TGraphicObject;
  begin
    Obj := Objects.FirstObj;
    while Assigned(Obj) do
    begin
      if Obj is TGroup2D
        then ProcessList((Obj as TGroup2D).Objects)
      else ChangeProc(Obj, PData);
      Obj := Objects.NextObj;
    end;
  end;
begin
  Objects.Lock;
  try
    ProcessList(Objects);
  finally
    Objects.Unlock;
  end;
end;

procedure ChangeSelected(const ADrawing: TDrawing2D;
  const ChangeProc: TChangeProc; const PData: Pointer);
begin
  ChangeObjects(ADrawing.SelectedObjects, ChangeProc, PData);
      //Obj.UpdateExtension(Self);
  ADrawing.NotifyChanged;
end;

procedure UpdateExtension(const Obj: TGraphicObject; PData:
  Pointer);
begin
  if Obj is TPrimitive2D then
    (Obj as TPrimitive2D).UpdateExtension(nil);
end;

procedure ChangeLineStyle(const Obj: TGraphicObject; PData:
  Pointer);
begin
  if Obj is TPrimitive2D then
    (Obj as TPrimitive2D).LineStyle := TLineStyle(PData^);
end;

procedure ChangeLineColor(const Obj: TGraphicObject;
  PData: Pointer);
begin
  if Obj is TPrimitive2D then
    (Obj as TPrimitive2D).LineColor := TColor(PData^);
end;

procedure ChangeHatching(const Obj: TGraphicObject; PData:
  Pointer);
begin
  if Obj is TPrimitive2D then
    (Obj as TPrimitive2D).Hatching := THatching(PData^);
end;


procedure ChangeHatchColor(const Obj: TGraphicObject;
  PData: Pointer);
begin
  if Obj is TPrimitive2D then
    (Obj as TPrimitive2D).HatchColor := TColor(PData^);
end;

procedure ChangeFillColor(const Obj: TGraphicObject;
  PData: Pointer);
begin
  if Obj is TPrimitive2D then
    (Obj as TPrimitive2D).FillColor := TColor(PData^);
end;

procedure ChangeLineWidth(const Obj: TGraphicObject;
  PData: Pointer);
begin
  if Obj is TPrimitive2D then
    (Obj as TPrimitive2D).LineWidth := TRealType(PData^);
end;

procedure ChangeBeginArrowKind(const Obj: TGraphicObject;
  PData: Pointer);
begin
  if Obj is TPrimitive2D then
    (Obj as TPrimitive2D).BeginArrowKind := TArrowKind(PData^);
end;

procedure ChangeEndArrowKind(const Obj: TGraphicObject;
  PData: Pointer);
begin
  if Obj is TPrimitive2D then
    (Obj as TPrimitive2D).EndArrowKind := TArrowKind(PData^);
end;

procedure ChangeArrowSizeFactor(
  const Obj: TGraphicObject; PData: Pointer);
begin
  if Obj is TPrimitive2D then
    (Obj as TPrimitive2D).ArrowSizeFactor := TRealType(PData^);
end;

procedure ChangeFontHeight(const Obj: TGraphicObject;
  PData: Pointer);
begin
  if Obj is TText2D then
    (Obj as TText2D).Height := TRealType(PData^);
end;

procedure ChangeHAlignment(const Obj: TGraphicObject;
  PData: Pointer);
begin
  if Obj is TText2D then
    (Obj as TText2D).HAlignment := THAlignment(PData^);
end;

procedure ChangeStarKind(const Obj: TGraphicObject;
  PData: Pointer);
begin
  if Obj is TStar2D then
    (Obj as TStar2D).StarKind := TStarKind(PData^);
end;

procedure ChangeStarSizeFactor(const Obj: TGraphicObject;
  PData: Pointer);
begin
  if Obj is TStar2D then
    (Obj as TStar2D).StarSizeFactor := TRealType(PData^);
end;

procedure ChangeSelectedProperties(const ADrawing: TDrawing2D;
  const ChangePropertiesSet: TChangePropertiesSet);
begin
  if chpLS in ChangePropertiesSet then
    ChangeObjects(ADrawing.SelectedObjects,
      ChangeLineStyle, @ADrawing.New_LineStyle);
  if chpLC in ChangePropertiesSet then
    ChangeObjects(ADrawing.SelectedObjects,
      ChangeLineColor, @ADrawing.New_LineColor);
  if chpLW in ChangePropertiesSet then
    ChangeObjects(ADrawing.SelectedObjects,
      ChangeLineWidth, @ADrawing.New_LineWidth);
  if chpHa in ChangePropertiesSet then
    ChangeObjects(ADrawing.SelectedObjects,
      ChangeHatching, @ADrawing.New_Hatching);
  if chpHC in ChangePropertiesSet then
    ChangeObjects(ADrawing.SelectedObjects,
      ChangeHatchColor, @ADrawing.New_HatchColor);
  if chpFC in ChangePropertiesSet then
    ChangeObjects(ADrawing.SelectedObjects,
      ChangeFillColor, @ADrawing.New_FillColor);
  if chpArr1 in ChangePropertiesSet then
    ChangeObjects(ADrawing.SelectedObjects,
      ChangeBeginArrowKind, @ADrawing.New_Arr1);
  if chpArr2 in ChangePropertiesSet then
    ChangeObjects(ADrawing.SelectedObjects,
      ChangeEndArrowKind, @ADrawing.New_Arr2);
  if chpArrS in ChangePropertiesSet then
    ChangeObjects(ADrawing.SelectedObjects,
      ChangeArrowSizeFactor, @ADrawing.New_ArrSizeFactor);
  if chpFH in ChangePropertiesSet then
    ChangeObjects(ADrawing.SelectedObjects,
      ChangeFontHeight, @ADrawing.New_FontHeight);
  if chpHJ in ChangePropertiesSet then
    ChangeObjects(ADrawing.SelectedObjects,
      ChangeHAlignment, @ADrawing.New_HAlignment);
  if chpSK in ChangePropertiesSet then
    ChangeObjects(ADrawing.SelectedObjects,
      ChangeStarKind, @ADrawing.New_StarKind);
  if chpSS in ChangePropertiesSet then
    ChangeObjects(ADrawing.SelectedObjects,
      ChangeStarSizeFactor, @ADrawing.New_StarSizeFactor);
  ChangeObjects(ADrawing.SelectedObjects,
    UpdateExtension, nil);
  ADrawing.NotifyChanged;
end;

end.

