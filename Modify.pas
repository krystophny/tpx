unit Modify;

// This module contains various routines for modification of
// a drawing and its objects

interface

uses
  Geometry, Graphics, Drawings, GObjBase, GObjects;

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
function ScaleStandard(const ADrawing: TDrawing2D;
  const ScaleStandardMaxWidth,
  ScaleStandardMaxHeight: TRealType;
  const DoNotify: Boolean): TTransf2D;
procedure ScalePhysical(const ADrawing: TDrawing2D;
  const S: TRealType; const DoNotify: Boolean);
procedure ConvertSelected(const ADrawing: TDrawing2D;
  const DestClass: TPrimitive2DClass);
procedure SimplifyPoly(const ADrawing: TDrawing2D;
  const Aperture: TRealType);
procedure ConnectPaths(const ADrawing: TDrawing2D);
procedure SelectedReversePoints(const ADrawing: TDrawing2D);
procedure MakeGrayscale(const ADrawing: TDrawing2D);
procedure BreakPath(const ADrawing: TDrawing2D;
  const Obj0: TPrimitive2D;
  const P: TPoint2D; const Aperture, Precision: TRealType);

implementation

uses Math, ColorEtc;

procedure BackwardForward(const ADrawing: TDrawing2D;
  const Mv: TMvBackwardForward);
var
  TmpIter: TGraphicObjIterator;
  TmpIter2: TGraphicObjIterator;
  Position: TGraphicObject;
  Current: TGraphicObject;
  MoveMore: Boolean;
begin
  if ADrawing.SelectedObjects.Count = 0 then Exit;
  MoveMore := False;
  TmpIter := ADrawing.SelectedObjects.GetIterator;
  try
    TmpIter2 :=
      ADrawing.ObjectsIterator;
    try
      case Mv of
        mv_Forward:
          begin
            TmpIter2.Search(TmpIter.Last.ID);
            Position := TmpIter2.Next;
            if Position = nil then
              Position := TmpIter2.Last;
            MoveMore := True;
          end;
        mv_Backward:
          begin
            TmpIter2.Search(TmpIter.Last.ID);
            Position := TmpIter2.Prev;
            if Position = nil then
              Position := TmpIter2.First;
          end;
        mv_ToFront:
          begin
            MoveMore := True;
            Position := TmpIter2.Last
          end;
        mv_ToBack: Position := TmpIter2.First;
      end;
    finally
      TmpIter2.Free;
    end;
    Current := TmpIter.Last;
    while Current <> nil do
    begin
      if Current.ID <> Position.ID then
      begin
        ADrawing.MoveObject(Current.ID,
          Position.ID);
        if MoveMore then
          ADrawing.MoveObject(Position.ID,
            Current.ID);
      end;
      Current := TmpIter.Prev;
    end;
  finally
    TmpIter.Free;
  end;
end;

procedure AlignSelected(const ADrawing: TDrawing2D;
  const Alignment: TObjectsAlignment);
var
  TmpIter: TGraphicObjIterator;
  Obj: TObject2D;
  R: TRect2D;
  A, TX, TY: TRealType;
begin
  if ADrawing.SelectedObjects.Count = 0 then Exit;
  TmpIter := ADrawing.SelectedObjects.GetIterator;
  try
    R := GetExtension0(ADrawing, TmpIter);
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
    Obj := TmpIter.First as TObject2D;
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
      Obj := TmpIter.Next as TObject2D;
    end;
  finally
    TmpIter.Free;
    ADrawing.NotifyChanged;
  end;
end;

procedure DuplicateSelected(const ADrawing: TDrawing2D;
  const Shift: TVector2D);
var
  ExIter: TExclusiveGraphicObjIterator;
  Current, NewObj: TGraphicObject;
  NewObjs: TGraphicObjList;
  T: TTransf2D;
  OnChangeDrawing0: TOnChangeDrawing;
begin
  NewObjs := TGraphicObjList.Create;
  NewObjs.FreeOnClear := False;
  OnChangeDrawing0 := ADrawing.OnChangeDrawing;
  ADrawing.OnChangeDrawing := nil;
  try
    ExIter := ADrawing.SelectedObjects.GetExclusiveIterator;
    try
      Current := nil;
      while ExIter.GetNext(Current) do
      begin
        NewObj :=
          TpXFindClassByName(Current.ClassName).CreateDupe(Current);
        NewObjs.Add(NewObj);
      end;
    finally
      ExIter.Free;
    end;
    T := Translate2D(Shift.X * 5, Shift.Y * 5);
    ExIter := NewObjs.GetExclusiveIterator;
    try
      Current := nil;
      while ExIter.GetNext(Current) do
        (Current as TObject2D).TransForm(T);
    finally
      ExIter.Free;
    end;
    ADrawing.AddList(NewObjs);
    ADrawing.SelectionClear;
    ADrawing.SelectionAddList(NewObjs);
  finally
    ADrawing.OnChangeDrawing := OnChangeDrawing0;
    NewObjs.Free;
  end;
end;

procedure GroupSelected(const ADrawing: TDrawing2D);
var
  Group: TContainer2D;
  Iter: TGraphicObjIterator;
  Obj: TGraphicObject;
  ExIter: TExclusiveGraphicObjIterator;
  OnChangeDrawing0: TOnChangeDrawing;
begin
  if ADrawing.SelectedObjects.Count <= 1 then Exit;
  OnChangeDrawing0 := ADrawing.OnChangeDrawing;
  ADrawing.OnChangeDrawing := nil;
  try
    Group := TContainer2D.Create(-1);
    try
      ExIter := ADrawing.SelectedObjects.GetExclusiveIterator;
      try
        Obj := nil;
        while ExIter.GetNext(Obj) do
          Group.Objects.Add(
            TpXFindClassByName(Obj.ClassName).CreateDupe(Obj));
      finally
        ExIter.Free;
      end;
//    Group.Objects.AddFromList(ADrawing.SelectedObjects);
      Iter := ADrawing.SelectedObjects.GetIterator;
      Obj := Iter.Last;
      Iter.Free;
      ADrawing.SelectionRemove(Obj);
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

procedure UngroupSelected(const ADrawing: TDrawing2D);
begin

end;

function ScaleStandard(const ADrawing: TDrawing2D;
  const ScaleStandardMaxWidth,
  ScaleStandardMaxHeight: TRealType;
  const DoNotify: Boolean): TTransf2D;
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
  ADrawing.TransformObjects([-1], Result, DoNotify);
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

procedure ConvertSelected(const ADrawing: TDrawing2D;
  const DestClass: TPrimitive2DClass);
var
  ExIter, ExIter2: TExclusiveGraphicObjIterator;
  Current, NewObj: TGraphicObject;
  NewObjs: TGraphicObjList;
begin
  NewObjs := TGraphicObjList.Create;
  NewObjs.FreeOnClear := False;
  try
    ExIter :=
      ADrawing.SelectedObjects.GetExclusiveIterator;
    ExIter2 :=
      ADrawing.ObjectsExclusiveIterator;
    try
      Current := nil;
      while ExIter.GetNext(Current) do
      begin
        NewObj := DestClass.Create(Current.ID);
        NewObj.Assign(Current);
        ExIter.ReplaceDeleteCurrent(NewObj);
        ExIter2.Search(Current.ID);
        ExIter2.ReplaceDeleteCurrent(NewObj);
        NewObjs.Add(NewObj);
          //NewObj.ID := Current.ID;
      end;
    finally
      ExIter.Free;
      ExIter2.Free;
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
  ExIter, ExIter2: TExclusiveGraphicObjIterator;
  Current, NewObj: TGraphicObject;
  NewObjs: TGraphicObjList;
begin
  NewObjs := TGraphicObjList.Create;
  NewObjs.FreeOnClear := False;
  try
    ExIter :=
      ADrawing.SelectedObjects.GetExclusiveIterator;
    ExIter2 :=
      ADrawing.ObjectsExclusiveIterator;
    try
      Current := nil;
      while ExIter.GetNext(Current) do
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
          ExIter.ReplaceDeleteCurrent(NewObj);
          ExIter2.Search(Current.ID);
          ExIter2.ReplaceDeleteCurrent(NewObj);
          NewObjs.Add(NewObj);
        end;
          //NewObj.ID := Current.ID;
      end;
    finally
      ExIter.Free;
      ExIter2.Free;
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
  Obj: TGraphicObject;
  Prim, Prim2: TPrimitive2D;
  Iter: TGraphicObjIterator;
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
begin
  if ADrawing.SelectedObjects.Count = 0 then Exit;
  OnChangeDrawing := ADrawing.OnChangeDrawing;
  ADrawing.OnChangeDrawing := nil;
  TmpPP := TPointsSet2D.Create(0);
  try
    TempList := TGraphicObjList.Create;
    TempList.FreeOnClear := False;
    TempList.AddFromList(ADrawing.SelectedObjects);
    ADrawing.SelectionClear;
    Iter := TempList.GetExclusiveIterator;
    try
      Prim := nil;
      Obj := nil;
      while Iter.GetNext(Obj) do
      begin
        IsLinearObj := (Obj is TLine2D) or (Obj is TPolyline2D);
        IsBezierObj := (Obj is TBezierPath2D) or (Obj is
          TSmoothPath2D)
          or (Obj is TArc2D);
        if not (IsLinearObj or IsBezierObj) then Continue;
        if (Obj as TPrimitive2D).Points.Count = 0 then Continue;
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
          Continue;
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
          TmpPP.Delete(0);
          PPP.AppendPoints(TmpPP);
          ADrawing.DeleteObject(Obj.ID);
        end
        else
        begin
          TmpPP.AppendPoints((Obj as TPrimitive2D).Points);
          if (TmpPP.Count > 0) and IsSamePoint2D(
            PPP[PPP.Count - 1], TmpPP[0])
            then TmpPP.Delete(0);
          CheckLengths;
          PPP.AppendPoints(TmpPP);
          ADrawing.DeleteObject(Obj.ID);
        end;
      end;
      ADrawing.SelectionClear;
    finally
      Iter.Free;
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
  Iter: TGraphicObjIterator;
begin
  if ADrawing.SelectedObjects.Count = 0 then Exit;
  Iter := ADrawing.SelectedObjects.GetIterator;
  try
    Obj := nil;
    while Iter.GetNext(Obj) do
    begin
      if Obj is TPrimitive2D then
        (Obj as TPrimitive2D).ReversePoints;
    end;
  finally
    Iter.Free;
    ADrawing.NotifyChanged;
  end;
end;

procedure MakeGrayscale(const ADrawing: TDrawing2D);
var
  Obj: TGraphicObject;
  Iter: TGraphicObjIterator;
begin
  if ADrawing.SelectedObjects.Count = 0 then
    Iter := ADrawing.ObjectsIterator
  else
    Iter := ADrawing.SelectedObjects.GetIterator;
  try
    Obj := nil;
    while Iter.GetNext(Obj) do
      if Obj is TPrimitive2D then
        with Obj as TPrimitive2D do
        begin
          if LineColor <> clDefault then
            LineColor := GrayScale(LineColor);
          if HatchColor <> clDefault then
            HatchColor := GrayScale(HatchColor);
          if FillColor <> clDefault then
            FillColor := GrayScale(FillColor);
        end;
  finally
    Iter.Free;
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

end.

