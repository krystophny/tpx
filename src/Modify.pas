unit Modify;

// This module contains various routines for modification of
// a drawing and its objects

interface

uses
{$IFDEF FPC}
Graphics,
{$ELSE}
FMX.Graphics, System.UITypes,
{$ENDIF}
  Geometry, Drawings, GObjBase, GObjects;

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
procedure CompoundFromSelected(const ADrawing: TDrawing2D);
procedure UncompoundSelected(const ADrawing: TDrawing2D);
function ScaleStandard(const ADrawing: TDrawing2D;
  const ScaleStandardMaxWidth,
  ScaleStandardMaxHeight: TRealType): TTransf2D;
procedure ScalePhysical(const ADrawing: TDrawing2D;
  const S: TRealType; const DoNotify: Boolean);
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

uses Math, ColorEtc, Devices;

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

procedure GroupSelected(const ADrawing: TDrawing2D);
var
  Group: TGroup2D;
  Obj: TGraphicObject;
  OnChangeDrawing0: TOnChangeDrawing;
begin
  if ADrawing.SelectedObjects.Count <= 1 then Exit;
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
        if ADrawing.SelectedObjects.CurrentObj is TGroup2D then
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

procedure ConvertSelected(const ADrawing: TDrawing2D;
  const DestClass: TPrimitive2DClass);
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
        {$IFDEF FPC}
          if LineColor <> clDefault then
            LineColor := GrayScale(LineColor);
          if HatchColor <> clDefault then
            HatchColor := GrayScale(HatchColor);
          if FillColor <> clDefault then
            FillColor := GrayScale(FillColor);
        {$ELSE}
          if LineColor <> TColorRec.SysDefault then
            LineColor := GrayScale(LineColor);
          if HatchColor <> TColorRec.SysDefault then
            HatchColor := GrayScale(HatchColor);
          if FillColor <> TColorRec.SysDefault then
            FillColor := GrayScale(FillColor);
        {$ENDIF}
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

