unit Draw;

interface

uses Types, Classes, Windows, Graphics, Geometry;

type
{: This class defines a decorative pen, that is a special Window pen that
   can have any pattern. The pattern is defined by a string of bits like
   '1110001100', in which a one rapresent a colored pixel and a zero
   rapresent a transparent pixel. By using this pen the redrawing of the
   image will be slower, so use it only where necessary. Because a decorative
   pen is associated to a layer you can manage it better.

   <B=Note>: The decorative pen use LineDDA functions to draw the lines and
   can only be used for lines and polylines (so no ellipses are drawed using
   the pattern).
}
  TDecorativePen = class(TObject)
  private
    fPStyle: TBits;
    fCnv: TCanvas;
    fCurBit: Word;
    fStartPt, fEndPt, fLastPt: TPoint;

    procedure SetBit(const IDX: Word; const B: Boolean);
    function GetBit(const IDX: Word): Boolean;
    function GetMaxBit: Word;
    procedure CallLineDDA;
  public
    constructor Create;
    destructor Destroy; override;
    {: This method is used to make deep copy of the object by obtaining state
       informations from another.

       <I=Obj> is the object from which copy the needed informations.
    }
    procedure Assign(Source: TObject);
    {: Move the pen for the canvas <I=Cnv> to the position <I=X,Y>.
    }
    procedure MoveTo(Cnv: TCanvas; X, Y: Integer);
    {: Move the pen for the canvas <I=Cnv> to the position <I=X,Y>.
       This will not reset the current pattern bit. It is useful
       when you are drawing a shapes made by segment.
    }
    procedure MoveToNotReset(Cnv: TCanvas; X, Y: Integer);
    {: Draw a line using the current pattern and color to the position <I=X,Y>.
    }
    procedure LineTo(Cnv: TCanvas; X, Y: Integer);
    {: Draw a polyline using the current pattern and color.
       <I=Pts> are the points of the polyline and <I=NPts> is the number of points.
    }
    procedure Polyline(Cnv: TCanvas; Pts: Pointer; NPts:
      Integer);
    {: Specify the pattern for lines.
       The pattern is defined by a string of bits like
       '1110001100', in which a one rapresent a colored pixel and a zero
       rapresent a transparent pixel. By using this pen the redrawing of the
       image will be slower, so use it only where necessary. Because a decorative
       pen is associated to a layer you can manage it better.
    }
    procedure SetPenStyle(const SString: string);
    {: Contains the patter for lines.
       The pattern is defined by a string of bits like
       '1110001100', in which a one rapresent a colored pixel and a zero
       rapresent a transparent pixel. By using this pen the redrawing of the
       image will be slower, so use it only where necessary. Because a decorative
       pen is associated to a layer you can manage it better.
    }
    property PenStyle[const IDX: Word]: Boolean read GetBit write
    SetBit;
    {: Contains the pattern length.
    }
    property PatternLenght: Word read GetMaxBit;
  end;

{: Defines an object that contains a DecorativePen and a Canvas.
   By using the decorative pen methods you can draw patterned lines,
   and by using the Canvas property you can draw using the
   canvas.

   See also <See Class=TDecorativePen> for details.
}
  TDecorativeCanvas = class(TObject)
  private
    fDecorativePen: TDecorativePen;
    fCanvas: TCanvas;
  public
    constructor Create(ACanvas: TCanvas);
    destructor Destroy; override;

    {: This method is a shortcut to the MoveTo method of the
       owned decorative pen. I suggest to use it in all of your
       shapes to draw lines.
    }
    procedure MoveTo(X, Y: Integer);
    {: This method is a shortcut to the LineTo method of the
       owned decorative pen. I suggest to use it in all of your
       shapes to draw lines.
    }
    procedure LineTo(X, Y: Integer);
    {: This method is a shortcut to the Polyline method of the
       owned decorative pen. I suggest to use it in all of your
       shapes to draw polylines.
    }
    procedure Polyline(Points: Pointer; NPts: Integer);
    {: Contains the decorative pen.
    }
    property DecorativePen: TDecorativePen read fDecorativePen;
    {: Contains the underling convas used to draw.
    }
    property Canvas: TCanvas read fCanvas write fCanvas;
  end;

  {: This type defines the procedure used by a <See Class=TCADViewport>
   controll to clear the canvas before drawing on it.

   <I=Sender> is the viewport, <I=Cnv> canvas to be cleared,
   <I=ARect> is the rectangle of the viewplane that will be drawed onto
   the canvas and <I=BackCol> is the background color to be used to clear
   the canvas.

   By default the canvas is simply cleared with the background color. This
   procedure is useful to draw a raster image onto which you want to
   do readlining.
}
  TClearCanvas = procedure(Sender: TObject; Cnv: TCanvas; const
    ARect: TRect2D; const BackCol: TColor) of object;

  { 2D functions. }

  {: Use this function to initialize a 2D point variable.

     This function returns always a cartesing point (W=1).

     Parameters:

     <LI=<I=X> is the X coordinate of the point.>
     <LI=<I=Y> is the Y coordinate of the point.>
  }

  { Drawing functions
    Vect deve essere di tipo PVectPoints2D, Count è il numero di punti. }
procedure Draw2DSubSetAsPolygon(const Vect: Pointer; Count:
  Integer;
  const Cnv: TDecorativeCanvas;
  const Clip, Extent: TRect2D;
  const S: TTransf2D;
  const StartIdx, EndIdx: Integer);
procedure Draw2DSubSetAsPolyline(const Vect: Pointer; Count:
  Integer;
  const Cnv: TDecorativeCanvas;
  const Clip, Extent: TRect2D;
  const S: TTransf2D;
  const StartIdx, EndIdx: Integer;
  const ToBeClosed: Boolean);
  {: This is an utility procedure useful when you want to mark a control point of
     a graphic object.

     It draws a small square of size <I=Wdt> centered at <I=X,Y> on the
     canvas <I=Cnv>. All the parameters are refered to window's screen
     coordinates. <I=Wdt> is half of the side of the square.
  }
procedure DrawPlaceHolder(const Cnv: TDecorativeCanvas; const X,
  Y, Wdt: Integer);
procedure DrawRoundPlaceHolder(const Cnv: TDecorativeCanvas;
  const X, Y, Wdt: Integer);
  {: This function can be used to draw a rectangle onto a
     canvas.

     The rectangle being drawed is first transformed by <I=MT>,
     then it is clipped within the specified region and
     then mapped on the screen with the matrix <I=S>.

     Parameters:

     <LI=<I=Cnv> is the canvas on which the rectangle is drawed.>
     <LI=<I=Box> is the rectangle to be drawed.>
     <LI=<I=Clip> is the clipping rectangle.>
     <LI=<I=MT> is the transform matrix to be applied to Box.>
     <LI=<I=S> is the mapping matrix used to map the clipped Box to the screen.>
  }
procedure DrawRect2DAsPolyline(const Cnv: TDecorativeCanvas;
  const Box, Clip: TRect2D; const MT, S: TTransf2D);
  {: This function can be used to draw a box onto a canvas.

     The box being drawed is first transformed by
     <I=MT>, then it is clipped within the specified region and
     then mapped on the screen with the matrix <I=S>.

     The rectangle is filled with the canvas brush.

     Parameters:

     <LI=<I=Cnv> is the canvas on which the rectangle is drawed.>
     <LI=<I=Box> is the rectangle to be drawed.>
     <LI=<I=Clip> is the clipping rectangle.>
     <LI=<I=MT> is the transform matrix to be applied to Box>
     <LI=<I=S> is the mapping matrix used to map the clipped Boxto the screen.>
  }
procedure DrawRect2DAsPolygon(const Cnv: TDecorativeCanvas; const
  Box, Clip: TRect2D; const MT, S: TTransf2D);
  {: This function can be used to draw a line onto a canvas.

     The line being drawed is clipped within the specified
     region and then mapped on the screen with the matrix <I=S>.

     Parameters:

     <LI=<I=Cnv> is the canvas on which the line is drawed.>
     <LI=<I=P1> is the first point of the line.>
     <LI=<I=P2> is the second point of the line.>
     <LI=<I=Clip>. This is the clipping rectangle.>
     <LI=<I=S> is the mapping matrix used to map the clipped line to the screen.>
  }
function DrawLine2D(const Cnv: TDecorativeCanvas; P1, P2:
  TPoint2D; const Clip: TRect2D; const S: TTransf2D): Boolean;
  {: This function can be used to draw a bounding box onto
     a canvas.

     The box being drawed is first transformed by <I=MT>.
     This transformation keep the bounding box nature of
     the rectangle, so the transformed box has the edges
     parallel to the axis of the world.

     After that it is mapped onto the screen and clipped
     against the clipping region.

     Parameters:

     <LI=<I=Cnv> is the canvas on which the rectangle is drawed.>
     <LI=<I=Box> is the rectangle to be drawed.>
     <LI=<I=Clip> is the clipping rectangle.>
     <LI=<I=MT> is the transform matrix to be applied to <I=Box>.>
     <LI=<I=S> is the mapping matrix used to map the clipped
      <I=Box> to the screen.>
  }
procedure DrawBoundingBox2D(const Cnv: TDecorativeCanvas; Box,
  Clip: TRect2D; const S: TTransf2D);


    {: Draw the entire set of the points in the set onto a Canvas as
       a closed polygon filled with the current brush of the Canvas.

       <I=Cnv> is the destination canvas on which the set will
       be drawed <See Class=TDecorativeCanvas@TDecorativeCanvas>; <I=Clip> is the clipping rectangle in canvas
       coordinates used to clip the output; <I=Extent> is the
       extension of the set as returned from <See Property=TPointsSet2D@Extension>;
       <I=S> is the viewport transformation as returned from
       <See Property=TCADViewport@ViewportToScreenTransform>.

       The set will be drawed as a closed polyline (it will be
       closed automatically by the method) with the outline drawed
       with the current pen and the interior filled with the
       current brush of the canvas.

       <B=Note 1>: The <I=Clip> parameter is of type <See Type=TRect2D>
       but it is referred to convas coordinates that are of type <I=TRect>.
       For this you have to use the <See Function=RectToRect2D> function
       to pass the <I=ClipRect> property of the canvas.

       <B=Note2>: The clipping region of the canvas have to be the
       entire canvas extension (that is no user defined clipping region
       must be is set).
    }
procedure DrawAsPolygon(PP: TPointsSet2D; const Cnv: TDecorativeCanvas; const
  Clip, Extent: TRect2D; const S: TTransf2D);
    //TSY:
procedure DrawAsPolygonOutline(PP: TPointsSet2D; const Cnv: TDecorativeCanvas;
  const
  Clip, Extent: TRect2D; const S: TTransf2D);
    {: Draw the entire set of the points in the set onto a Canvas
       as a polyline.

       <I=Cnv> is the destination canvas on which the set will
       be drawed <See Class=TDecorativeCanvas@TDecorativeCanvas>; <I=Clip> is the clipping rectangle in canvas
       coordinates used to clip the output; <I=Extent> is the
       extension of the set as returned from <See Property=TPointsSet2D@Extension>;
       <I=S> is the viewport transformation as returned from
       <See Property=TCADViewport@ViewportToScreenTransform>.

       The set will be drawed as a polyline (not closed by the
       method) with the outline drawed with the current pen of
       the canvas and not filled.

       <B=Note 1>: The <I=Clip> parameter is of type <See Type=TRect2D>
       but it is referred to convas coordinates that are of type <I=TRect>.
       For this you have to use the <See Function=RectToRect2D> function
       to pass the <I=ClipRect> property of the canvas.

       <B=Note 2>: The clipping region of the canvas have to be the
       entire canvas extension (that is no user defined clipping region
       must be is set).
    }
procedure DrawAsPolyline(PP: TPointsSet2D; const Cnv: TDecorativeCanvas; const
  Clip, Extent: TRect2D; const S: TTransf2D);
    {: Draw a subset of points onto a Canvas as a closed
       polygon filled with the current brush of the Canvas.

       <I=Cnv> is the destination canvas on which the set will
       be drawed <See Class=TDecorativeCanvas@TDecorativeCanvas>; <I=Clip> is the clipping rectangle in canvas
       coordinates used to clip the output; <I=Extent> is the
       extension of the set as returned from <See Property=TPointsSet2D@Extension>;
       <I=S> is the viewport transformation as returned from
       <See Property=TCADViewport@ViewportToScreenTransform>.
       <I=StartIdx> is the index of first point of the set to be
       drawed; <I=EndIdx> is the index of last point of the
       set to be drawed. <I=EndIdx> must be greater than
       <I=StartIdx>.

       The set will be drawed as a closed polyline (it will be
       closed automatically by the method) from the point at
       <I=StartIdx> and the last point at <I=EndIdx>, with the
       outline drawed with the current pen and the interior filled
       with the current brush of the canvas.

       <B=Note 1>: The <I=Clip> parameter is of type <See Type=TRect2D>
       but it is referred to convas coordinates that are of type <I=TRect>.
       For this you have to use the <See Function=RectToRect2D> function
       to pass the <I=ClipRect> property of the canvas.

       <B=Note2>: The clipping region of the canvas have to be the
       entire canvas extension (that is no user defined clipping region
       must be is set).
    }
procedure DrawSubsetAsPolygon(PP: TPointsSet2D; const Cnv: TDecorativeCanvas;
  const Clip, Extent: TRect2D; const S: TTransf2D; const
  StartIdx, EndIdx: Integer);
    {: Draw a subset of the points in the set onto a Canvas as a polyline.

       <I=Cnv> is the destination canvas on which the set will
       be drawed <See Class=TDecorativeCanvas@TDecorativeCanvas>; <I=Clip> is the clipping rectangle in canvas
       coordinates used to clip the output; <I=Extent> is the
       extension of the set as returned from <See Property=TPointsSet2D@Extension>;
       <I=S> is the viewport transformation as returned from
       <See Property=TCADViewport@ViewportToScreenTransform>.
       <I=StartIdx> is the index of first point of the set to be
       drawed; <I=EndIdx> is the index of last point of the
       set to be drawed. <I=EndIdx> must be greater than
       <I=StartIdx>.

       The set will be drawed as a polyline (not closed by the
       method) from the point at <I=StartIdx> and the last
       point at <I=EndIdx>, with the outline drawed with
       the current pen of the canvas and not filled.

       <B=Note 1>: The <I=Clip> parameter is of type <See Type=TRect2D>
       but it is referred to convas coordinates that are of type <I=TRect>.
       For this you have to use the <See Function=RectToRect2D> function
       to pass the <I=ClipRect> property of the canvas.

       <B=Note2>: The clipping region of the canvas have to be the
       entire canvas extension (that is no user defined clipping region
       must be is set).
    }
procedure DrawSubsetAsPolyline(PP: TPointsSet2D; const Cnv: TDecorativeCanvas;
  const Clip, Extent: TRect2D; const S: TTransf2D; const
  StartIdx, EndIdx: Integer; const ToBeClosed: Boolean);

implementation

{ TDecorativeCanvas }

constructor TDecorativeCanvas.Create(ACanvas: TCanvas);
begin
  inherited Create;
  fCanvas := ACanvas;
  fDecorativePen := TDecorativePen.Create;
end;

destructor TDecorativeCanvas.Destroy;
begin
  fDecorativePen.Free;
  inherited Destroy;
end;

procedure TDecorativeCanvas.MoveTo(X, Y: Integer);
begin
  fDecorativePen.MoveTo(fCanvas, X, Y);
end;

procedure TDecorativeCanvas.LineTo(X, Y: Integer);
begin
  fDecorativePen.LineTo(fCanvas, X, Y);
end;

procedure TDecorativeCanvas.Polyline(Points: Pointer; NPts:
  Integer);
begin
  fDecorativePen.Polyline(fCanvas, Points, NPts);
end;

{ TDecorativePen }

procedure LineDDAMethod1(X, Y: Integer; lpData: Pointer);
  stdcall;
var
  NextBit: Integer;
begin
  with TDecorativePen(lpData) do
  begin
    NextBit := (fCurBit + Abs(X - fLastPt.X)) mod GetMaxBit;
    fLastPt := Point(X, Y);
    if (fCurBit < GetMaxBit) and
      (fPStyle[fCurBit] and
      not fPStyle[NextBit]) then
      fCnv.Polyline([Point(fStartPt.X, fStartPt.Y), Point(X, Y)])
    else if not fPStyle[fCurBit] then
      fStartPt := Point(X, Y);
    if (X = fEndPt.X - 1) or (X = fEndPt.X + 1) then
      fCnv.Polyline([Point(fStartPt.X, fStartPt.Y), Point(X,
          Y)]);
    fCurBit := NextBit;
  end;
end;

procedure LineDDAMethod2(X, Y: Integer; lpData: Pointer);
  stdcall;
var
  NextBit: Integer;
begin
  with TDecorativePen(lpData) do
  begin
    NextBit := (fCurBit + Abs(Y - fLastPt.Y)) mod GetMaxBit;
    fLastPt := Point(X, Y);
    if (fCurBit < GetMaxBit) and
      (fPStyle[fCurBit] and not fPStyle[NextBit]) then
      fCnv.Polyline([Point(fStartPt.X, fStartPt.Y), Point(X, Y)])
    else if not fPStyle[fCurBit] then
      fStartPt := Point(X, Y);
    if (Y = fEndPt.Y - 1) or (Y = fEndPt.Y + 1) then
      fCnv.Polyline([Point(fStartPt.X, fStartPt.Y), Point(X,
          Y)]);
    fCurBit := NextBit;
  end
end;

procedure TDecorativePen.SetBit(const IDX: Word; const B:
  Boolean);
begin
  fPStyle[IDX] := B;
end;

function TDecorativePen.GetBit(const IDX: Word): Boolean;
begin
  Result := fPStyle[IDX];
end;

function TDecorativePen.GetMaxBit: Word;
begin
  Result := fPStyle.Size;
end;

procedure TDecorativePen.CallLineDDA;
begin
  if (Abs(fEndPt.X - fStartPt.X) > Abs(fEndPt.Y - fStartPt.Y))
    then
    LineDDA(fStartPt.X, fStartPt.Y, fEndPt.X, fEndPt.Y,
      @LineDDAMethod1, Integer(Self))
  else
    LineDDA(fStartPt.X, fStartPt.Y, fEndPt.X, fEndPt.Y,
      @LineDDAMethod2, Integer(Self));
end;

constructor TDecorativePen.Create;
begin
  inherited;

  fPStyle := TBits.Create;
end;

destructor TDecorativePen.Destroy;
begin
  fPStyle.Free;
  inherited;
end;

procedure TDecorativePen.Assign(Source: TObject);
var
  Cont: Integer;
begin
  if (Source = Self) then
    Exit;
  if Source is TDecorativePen then
  begin
    fPStyle.Size := 0;
    for Cont := 0 to TDecorativePen(Source).fPStyle.Size - 1 do
      fPStyle[Cont] := TDecorativePen(Source).fPStyle[Cont];
  end;
end;

procedure TDecorativePen.MoveTo(Cnv: TCanvas; X, Y: Integer);
begin
  if (fPStyle.Size > 0) then
  begin
    fStartPt := Point(X, Y);
    fEndPt := Point(X, Y);
    fLastPt := fStartPt;
    fCurBit := 0;
  end
  else
    Cnv.MoveTo(X, Y);
end;

procedure TDecorativePen.MoveToNotReset(Cnv: TCanvas; X, Y:
  Integer);
begin
  if (fPStyle.Size > 0) then
  begin
    fStartPt := Point(X, Y);
    fEndPt := Point(X, Y);
    fLastPt := fStartPt;
  end
  else
    Cnv.MoveTo(X, Y);
end;

procedure TDecorativePen.LineTo(Cnv: TCanvas; X, Y: Integer);
begin
  if (fPStyle.Size > 0) then
  begin
    fEndPt := Point(X, Y);
    fCnv := Cnv;
    CallLineDDA;
    fStartPt := Point(X, Y);
    fLastPt := fStartPt;
  end
  else
    Cnv.LineTo(X, Y);
end;

procedure TDecorativePen.Polyline(Cnv: TCanvas; Pts: Pointer;
  NPts: Integer);
type
  TPoints = array[0..0] of TPoint;
var
  Cont: Integer;
  TmpPts: ^TPoints;
begin
  if NPts <= 1 then
    Exit;
  TmpPts := Pts;
  if (fPStyle.Size > 0) then
  begin
    fCnv := Cnv;
    fCurBit := 0;
    fLastPt := TmpPts^[0];
    for Cont := 0 to NPts - 2 do
    begin
      if Cont > 0 then
        fStartPt := fLastPt
      else
        fStartPt := TmpPts^[Cont];
      fEndPt := TmpPts^[Cont + 1];
      CallLineDDA;
    end;
  end
  else
    Windows.Polyline(Cnv.Handle, TmpPts^, NPts);
end;

procedure TDecorativePen.SetPenStyle(const SString: string);
var
  Cont: Integer;
begin
  fPStyle.Size := Length(SString);
  for Cont := 1 to fPStyle.Size do
    if SString[Cont] = '1' then
      SetBit(Cont - 1, True)
    else
      SetBit(Cont - 1, False);
end;

// Drawing functions ~~~~~~~~~~~~~~~~~~~~~~~~

procedure Draw2DSubSetAsPolygon(const Vect: Pointer; Count:
  Integer;
  const Cnv: TDecorativeCanvas;
  const Clip, Extent: TRect2D;
  const S: TTransf2D;
  const StartIdx, EndIdx: Integer);
type
  TPoints = array[0..0] of TPoint;
var
  VisPoints, VisPoints1, Cont: Integer;
  TmpPt1, TmpPt2: TPoint2D;
  TmpPts, FirstClipPts: ^TPoints;
  ClipRes: TClipResult;
  TmpClip: TRect2D;
begin
  if (Count = 0) or (EndIdx > Count) or (StartIdx > EndIdx) then
    Exit;
  GetMem(TmpPts, Count * 3 * SizeOf(TPoint));
  GetMem(FirstClipPts, Count * 3 * SizeOf(TPoint));
  try
    VisPoints := 0;
    VisPoints1 := 0;
    if IsBoxAllInBox2D(TransformRect2D(Extent, S), Clip) then
      for Cont := StartIdx to EndIdx do
      begin
        TmpPt1 := TransformPoint2D(PVectPoints2D(Vect)^[Cont],
          S);
        TmpPts^[VisPoints] := Point2DToPoint(TmpPt1);
        Inc(VisPoints);
      end
    else
    begin
      TmpClip := ReorderRect2D(Clip);
      for Cont := StartIdx to EndIdx do
      begin
        TmpPt1 := TransformPoint2D(PVectPoints2D(Vect)^[Cont],
          S);
        if Cont < EndIdx then
          TmpPt2 := TransformPoint2D(PVectPoints2D(Vect)^[Cont +
            1], S)
        else
          TmpPt2 :=
            TransformPoint2D(PVectPoints2D(Vect)^[StartIdx], S);
        ClipRes := _ClipLineLeftRight2D(TmpClip, TmpPt1,
          TmpPt2);
        if not (ccNotVisible in ClipRes) then
        begin
          FirstClipPts^[VisPoints1] := Point2DToPoint(TmpPt1);
          Inc(VisPoints1);
        end;
        if ccSecond in ClipRes then
        begin
          FirstClipPts^[VisPoints1] := Point2DToPoint(TmpPt2);
          Inc(VisPoints1);
        end;
      end;
      FirstClipPts^[VisPoints1] := FirstClipPts^[0];
      Inc(VisPoints1);
      VisPoints := 0;
      for Cont := 0 to VisPoints1 - 2 do
      begin
        TmpPt1 := PointToPoint2d(FirstClipPts^[Cont]);
        TmpPt2 := PointToPoint2d(FirstClipPts^[Cont + 1]);
        ClipRes := _ClipLineUpBottom2D(TmpClip, TmpPt1, TmpPt2);
        if not (ccNotVisible in ClipRes) then
        begin
          TmpPts^[VisPoints] := Point2DToPoint(TmpPt1);
          Inc(VisPoints);
        end;
        if ccSecond in ClipRes then
        begin
          TmpPts^[VisPoints] := Point2DToPoint(TmpPt2);
          Inc(VisPoints);
        end;
      end;
    end;
    if (VisPoints > 0) then
      Polygon(Cnv.Canvas.Handle, TmpPts^, VisPoints);
  finally
    FreeMem(TmpPts, Count * 3 * SizeOf(TPoint));
    FreeMem(FirstClipPts, Count * 3 * SizeOf(TPoint));
  end;
end;

procedure Draw2DSubSetAsPolyline(const Vect: Pointer; Count:
  Integer;
  const Cnv: TDecorativeCanvas;
  const Clip, Extent: TRect2D;
  const S: TTransf2D;
  const StartIdx, EndIdx: Integer;
  const ToBeClosed: Boolean);
type
  TPoints = array[0..0] of TPoint;
var
  VisPoints, Cont, AllocatedMem: Integer;
  TmpPt1, TmpPt2: TPoint2D;
  TmpPts: ^TPoints;
  ClipRes: TClipResult;
  TmpClip: TRect2D;
begin
  if (Count = 0) or (EndIdx > Count) or (StartIdx > EndIdx) then
    Exit;
  if ToBeClosed then
    AllocatedMem := (Count + 1) * SizeOf(TPoint)
  else
    AllocatedMem := Count * SizeOf(TPoint);
  GetMem(TmpPts, AllocatedMem);
  try
    VisPoints := 0;
    if IsBoxAllInBox2D(TransformRect2D(Extent, S), Clip) then
    begin
      for Cont := StartIdx to EndIdx do
      begin
        TmpPt1 := TransformPoint2D(PVectPoints2D(Vect)^[Cont],
          S);
        TmpPts^[VisPoints] := Point2DToPoint(TmpPt1);
        Inc(VisPoints);
      end;
      if ToBeClosed then
      begin
        TmpPt1 :=
          TransformPoint2D(PVectPoints2D(Vect)^[StartIdx], S);
        TmpPts^[VisPoints] := Point2DToPoint(TmpPt1);
        Inc(VisPoints);
      end;
      ClipRes := [ccVisible];
    end
    else if ToBeClosed then
    begin
      TmpClip := ReorderRect2D(Clip);
      for Cont := StartIdx + 1 to EndIdx + 1 do
      begin
        TmpPt1 := TransformPoint2D(PVectPoints2D(Vect)^[Cont -
          1], S);
        if Cont > EndIdx then
          TmpPt2 :=
            TransformPoint2D(PVectPoints2D(Vect)^[StartIdx], S)
        else
          TmpPt2 := TransformPoint2D(PVectPoints2D(Vect)^[Cont],
            S);
        ClipRes := _ClipLine2D(TmpClip, TmpPt1, TmpPt2);
        if not (ccNotVisible in ClipRes) then
        begin
          TmpPts^[VisPoints] := Point2DToPoint(TmpPt1);
          Inc(VisPoints);
        end;
        if ccSecond in ClipRes then
        begin
          TmpPts^[VisPoints] := Point2DToPoint(TmpPt2);
          Inc(VisPoints);
          Cnv.Polyline(TmpPts, VisPoints);
          VisPoints := 0;
        end;
      end;
      if not (ccNotVisible in ClipRes) then
      begin
        TmpPts^[VisPoints] := Point2DToPoint(TmpPt2);
        Inc(VisPoints);
      end;
    end
    else
    begin
      TmpClip := ReorderRect2D(Clip);
      for Cont := StartIdx + 1 to EndIdx do
      begin
        TmpPt1 := TransformPoint2D(PVectPoints2D(Vect)^[Cont -
          1], S);
        TmpPt2 := TransformPoint2D(PVectPoints2D(Vect)^[Cont],
          S);
        ClipRes := _ClipLine2D(TmpClip, TmpPt1, TmpPt2);
        if not (ccNotVisible in ClipRes) then
        begin
          TmpPts^[VisPoints] := Point2DToPoint(TmpPt1);
          Inc(VisPoints);
        end;
        if ccSecond in ClipRes then
        begin
          TmpPts^[VisPoints] := Point2DToPoint(TmpPt2);
          Inc(VisPoints);
          Cnv.Polyline(TmpPts, VisPoints);
          VisPoints := 0;
        end;
      end;
      if not (ccNotVisible in ClipRes) then
      begin
        TmpPts^[VisPoints] := Point2DToPoint(TmpPt2);
        Inc(VisPoints);
      end;
    end;
    if (VisPoints > 0) then
      Cnv.Polyline(TmpPts, VisPoints);
  finally
    FreeMem(TmpPts, AllocatedMem);
  end;
end;

procedure DrawPlaceHolder(const Cnv: TDecorativeCanvas; const X,
  Y, Wdt: Integer);
var
  TmpWdt: Integer;
begin
  with Cnv.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := clSilver;
    Pen.Style := psSolid;
    TmpWdt := Wdt div 2;
    Windows.Rectangle(Handle, X - TmpWdt, Y - TmpWdt, X +
      TmpWdt, Y + TmpWdt);
  end;
end;

procedure DrawRoundPlaceHolder(const Cnv: TDecorativeCanvas;
  const X, Y, Wdt: Integer);
var
  TmpWdt: Integer;
begin
  with Cnv.Canvas do
  begin
    Brush.Style := bsSolid;
    //Brush.Color := clSilver;
    Brush.Color := clSkyBlue;
    Pen.Style := psSolid;
    TmpWdt := Wdt div 2;
    Windows.Ellipse(Handle, X - TmpWdt, Y - TmpWdt, X + TmpWdt, Y
      + TmpWdt);
  end;
end;

procedure DrawRect2DAsPolyline(const Cnv: TDecorativeCanvas;
  const Box, Clip: TRect2D; const MT, S: TTransf2D);
type
  TPoints = array[0..0] of TPoint;
var
  VisPoints, Cont: Integer;
  TmpPt1, TmpPt2: TPoint2D;
  TmpPts: ^TPoints;
  BoxPts: ^TVectPoints2D;
  ClipRes: TClipResult;
  TmpClip: TRect2D;
begin
  GetMem(TmpPts, 5 * SizeOf(TPoint));
  GetMem(BoxPts, 5 * SizeOf(TPoint2D));
  try
    VisPoints := 0;
    Cont := 0;
    BoxPts^[Cont] := TransformPoint2D(Box.FirstEdge, MT);
    BoxPts^[Cont + 1] := TransformPoint2D(Point2D(Box.Left,
      Box.Top), MT);
    BoxPts^[Cont + 2] := TransformPoint2D(Box.SecondEdge, MT);
    BoxPts^[Cont + 3] := TransformPoint2D(Point2D(Box.Right,
      Box.Bottom), MT);
    BoxPts^[Cont + 4] := TransformPoint2D(Box.FirstEdge, MT);
    if IsBoxAllInBox2D(TransformRect2D(Box, S), Clip) then
      for Cont := 0 to 4 do
      begin
        TmpPt1 := TransformPoint2D(PVectPoints2D(BoxPts)^[Cont],
          S);
        TmpPts^[VisPoints] := Point2DToPoint(TmpPt1);
        Inc(VisPoints);
        ClipRes := [ccVisible];
      end
    else
    begin
      TmpClip := ReorderRect2D(Clip);
      for Cont := 1 to 4 do
      begin
        TmpPt1 := TransformPoint2D(PVectPoints2D(BoxPts)^[Cont -
          1], S);
        TmpPt2 := TransformPoint2D(PVectPoints2D(BoxPts)^[Cont],
          S);
        ;
        ClipRes := _ClipLine2D(TmpClip, TmpPt1, TmpPt2);
        if not (ccNotVisible in ClipRes) then
        begin
          TmpPts^[VisPoints] := Point2DToPoint(TmpPt1);
          Inc(VisPoints);
        end;
        if ccSecond in ClipRes then
        begin
          TmpPts^[VisPoints] := Point2DToPoint(TmpPt2);
          Inc(VisPoints);
          Cnv.Polyline(TmpPts, VisPoints);
          VisPoints := 0;
        end;
      end;
      if not (ccNotVisible in ClipRes) then
      begin
        TmpPts^[VisPoints] := Point2DToPoint(TmpPt2);
        Inc(VisPoints);
      end;
    end;
    if (VisPoints > 0) then
      Cnv.Polyline(TmpPts, VisPoints)
  finally
    FreeMem(BoxPts, 5 * SizeOf(TPoint2D));
    FreeMem(TmpPts, 5 * SizeOf(TPoint));
  end;
end;

procedure DrawRect2DAsPolygon(const Cnv: TDecorativeCanvas; const
  Box, Clip: TRect2D; const MT, S: TTransf2D);
type
  TPoints = array[0..0] of TPoint;
var
  VisPoints, VisPoints1, Cont: Integer;
  TmpPt1, TmpPt2: TPoint2D;
  TmpPts, FirstClipPts: ^TPoints;
  BoxPts: ^TVectPoints2D;
  ClipRes: TClipResult;
  TmpClip: TRect2D;
begin
  GetMem(TmpPts, 15 * SizeOf(TPoint));
  GetMem(FirstClipPts, 15 * SizeOf(TPoint));
  GetMem(BoxPts, 5 * SizeOf(TPoint2D));
  try
    Cont := 0;
    BoxPts^[Cont] := TransformPoint2D(Box.FirstEdge, MT);
    BoxPts^[Cont + 1] := TransformPoint2D(Point2D(Box.Left,
      Box.Top), MT);
    BoxPts^[Cont + 2] := TransformPoint2D(Box.SecondEdge, MT);
    BoxPts^[Cont + 3] := TransformPoint2D(Point2D(Box.Right,
      Box.Bottom), MT);
    BoxPts^[Cont + 4] := TransformPoint2D(Box.FirstEdge, MT);
    VisPoints := 0;
    VisPoints1 := 0;
    if IsBoxAllInBox2D(TransformRect2D(Box, S), Clip) then
      for Cont := 0 to 4 do
      begin
        TmpPt1 := TransformPoint2D(PVectPoints2D(BoxPts)^[Cont],
          S);
        TmpPts^[VisPoints] := Point2DToPoint(TmpPt1);
        Inc(VisPoints);
      end
    else
    begin
      TmpClip := ReorderRect2D(Clip);
      for Cont := 0 to 4 do
      begin
        TmpPt1 := TransformPoint2D(PVectPoints2D(BoxPts)^[Cont],
          S);
        if Cont < 4 then
          TmpPt2 := TransformPoint2D(PVectPoints2D(BoxPts)^[Cont
            + 1], S)
        else
          TmpPt2 := TransformPoint2D(PVectPoints2D(BoxPts)^[0],
            S);
        ClipRes := _ClipLineLeftRight2D(TmpClip, TmpPt1,
          TmpPt2);
        if not (ccNotVisible in ClipRes) then
        begin
          FirstClipPts^[VisPoints1] := Point2DToPoint(TmpPt1);
          Inc(VisPoints1);
        end;
        if ccSecond in ClipRes then
        begin
          FirstClipPts^[VisPoints1] := Point2DToPoint(TmpPt2);
          Inc(VisPoints1);
        end;
      end;
      FirstClipPts^[VisPoints1].X := FirstClipPts^[0].X;
      FirstClipPts^[VisPoints1].Y := FirstClipPts^[0].Y;
      Inc(VisPoints1);
      VisPoints := 0;
      for Cont := 0 to VisPoints1 - 2 do
      begin
        TmpPt1 := PointToPoint2d(FirstClipPts^[Cont]);
        TmpPt2 := PointToPoint2d(FirstClipPts^[Cont + 1]);
        ClipRes := _ClipLineUpBottom2D(TmpClip, TmpPt1, TmpPt2);
        if not (ccNotVisible in ClipRes) then
        begin
          TmpPts^[VisPoints] := Point2DToPoint(TmpPt1);
          Inc(VisPoints);
        end;
        if ccSecond in ClipRes then
        begin
          TmpPts^[VisPoints] := Point2DToPoint(TmpPt2);
          Inc(VisPoints);
        end;
      end;
    end;
    if (VisPoints > 0) then
      Polygon(Cnv.Canvas.Handle, TmpPts^, VisPoints);
  finally
    FreeMem(BoxPts, 5 * SizeOf(TPoint2D));
    FreeMem(TmpPts, 15 * SizeOf(TPoint));
    FreeMem(FirstClipPts, 15 * SizeOf(TPoint));
  end;
end;

function DrawLine2D(const Cnv: TDecorativeCanvas; P1, P2:
  TPoint2D; const Clip: TRect2D; const S: TTransf2D): Boolean;
var
  TmpPt1, TmpPt2: TPoint;
  TmpPt12D, TmpPt22D: TPoint2D;
  ClipRes: TClipResult;
  TmpBox: TRect2D;
  TmpClip: TRect2D;
begin
  TmpBox.FirstEdge := P1;
  TmpBox.SecondEdge := P2;
  TmpBox := ReorderRect2D(TmpBox);
  if IsBoxAllInBox2D(TransformRect2D(TmpBox, S), Clip) then
  begin
    Result := True;
    TmpPt1 := Point2DToPoint(TransformPoint2D(P1, S));
    TmpPt2 := Point2DToPoint(TransformPoint2D(P2, S));
    Cnv.MoveTo(TmpPt1.X, TmpPt1.Y);
    Cnv.LineTo(TmpPt2.X, TmpPt2.Y);
  end
  else
  begin
    TmpClip := ReorderRect2D(Clip);
    TmpPt12D := TransformPoint2D(P1, S);
    TmpPt22D := TransformPoint2D(P2, S);
    ;
    ClipRes := _ClipLine2D(TmpClip, TmpPt12D, TmpPt22D);
    Result := not (ccNotVisible in ClipRes);
    if Result then
    begin
      TmpPt1 := Point2DToPoint(TmpPt12D);
      TmpPt2 := Point2DToPoint(TmpPt22D);
      Cnv.MoveTo(TmpPt1.X, TmpPt1.Y);
      Cnv.LineTo(TmpPt2.X, TmpPt2.Y);
    end;
  end;
end;

procedure DrawBoundingBox2D(const Cnv: TDecorativeCanvas; Box,
  Clip: TRect2D; const S: TTransf2D);
begin
  DrawRect2DAsPolyline(Cnv, Box, Clip, IdentityTransf2D, S)
end;


procedure DrawAsPolygon(PP: TPointsSet2D;
const Cnv:  TDecorativeCanvas;
  const Clip, Extent: TRect2D;
  const S: TTransf2D);
var
  PenStyle0: TPenStyle;
begin
  //TSY: remove line drawing
  PenStyle0 := Cnv.Canvas.Pen.Style;
  Cnv.Canvas.Pen.Style := psClear;
  Draw2DSubSetAsPolygon(PP.PointsPointer, PP.Count, Cnv, Clip, Extent, S,
    0, PP.Count - 1);
  Cnv.Canvas.Pen.Style := PenStyle0;
end;

//TSY: added

procedure DrawAsPolygonOutline(PP: TPointsSet2D;
const Cnv:  TDecorativeCanvas;
  const Clip, Extent: TRect2D; const S: TTransf2D);
var
  BrushStyle0: TBrushStyle;
  BrushColor0: TColor;
begin
  BrushStyle0 := Cnv.Canvas.Brush.Style;
  BrushColor0 := Cnv.Canvas.Brush.Color;
  Cnv.Canvas.Brush.Style := bsClear;
  Draw2DSubSetAsPolygon(PP.PointsPointer, PP.Count, Cnv, Clip, Extent, S,
    0, PP.Count - 1);
  Cnv.Canvas.Brush.Style := BrushStyle0;
  Cnv.Canvas.Brush.Color := BrushColor0;
end;

procedure DrawAsPolyline(PP: TPointsSet2D;
const Cnv:  TDecorativeCanvas;
  const Clip, Extent: TRect2D;
  const S: TTransf2D);
begin
  Draw2DSubSetAsPolyline(PP.PointsPointer, PP.Count, Cnv, Clip, Extent, S,
    0, PP.Count - 1, False);
end;

procedure DrawSubsetAsPolygon(PP: TPointsSet2D;
const Cnv:  TDecorativeCanvas;
  const Clip, Extent: TRect2D;
  const S: TTransf2D;
  const StartIdx, EndIdx: Integer);
begin
  Draw2DSubSetAsPolygon(PP.PointsPointer, PP.Count, Cnv, Clip, Extent, S,
    StartIdx, EndIdx);
end;

procedure DrawSubsetAsPolyline(PP: TPointsSet2D;
const Cnv:  TDecorativeCanvas;
  const Clip, Extent: TRect2D;
  const S: TTransf2D;
  const StartIdx, EndIdx: Integer;
  const ToBeClosed: Boolean);
begin
  Draw2DSubSetAsPolyline(PP.PointsPointer, PP.Count, Cnv, Clip, Extent, S,
    StartIdx, EndIdx, ToBeClosed);
end;

end.

