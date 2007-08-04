unit Geometry;

{$IFNDEF VER140}
{$MODE Delphi}
{$ENDIF}

interface

uses Types, MiscUtils;

type
{: To let the library to use any floating point precision value
   in defining the coordinates of points and so on, all
   floating point number used by the library is of this type.

   At the moment the precision is set to Single but you don't
   rely on this assumption when you create new kind of shapes
   classes or use the library.

   <B=Note>: I don't think that this type will change due to storage
   and speed efficency.
}
  TRealType = Single;
//  TRealType = Double;

// The maximum value for coordinates.
const
  MaxRealType = 3.4E+38; // MaxSingle;

//TSY:
function StrToRealType(const St: string;
  const DefaultVal: TRealType): TRealType;

type

{: This type is the result information of a clipping method. The
   clipping functions are used internally by the library and you
   don't need to use them directly.

   The tags have the following meanings:

   <LI=<I=ccFirst> the clipping function has modified the first point of the segment>
   <LI=<I=ccSecond> the clipping function has modified the second point of the segment>
   <LI=<I=ccNotVisible> the segment to be clipped is not visible>
   <LI=<I=ccVisible> the segment to be clipped is fully visible>
}
  TClipCode = (ccFirst, ccSecond, ccNotVisible, ccVisible);
{: This type is a set of <See Type=TClipCode> tags. A clipping function may
   return such a set.
}
  TClipResult = set of TClipCode;
{: This type define the point position against view frustum.
}
  TOutPos = (Left, Bottom, Right, Top, neareye, fareye);
{: This type define the point position against view frustum.
}
  TOutCode = set of TOutPos;

{: This type defines a 2D point in homogeneous coordinates.

   All point informations in the library are in homogeneous
   coordinates that is they have a third coordinate W. This
   coordinate may be treated as divisor coefficient for the X and Y
   coordinates.

   A 2D point in the euclidean space (the normally used point) can
   be obtained by dividing each X and Y coordinates by W:

   <Code=
     Xe := X / W;<BR>
     Ye := Y / W;<BR>.
   >

   A point to be valid must have at least one of its coordinates
   not zero. If a point has W = 0 it is called a point at infinitum
   and it is not allowed in the library. Normally this kind of
   points is used to rapresent a direction but in the library
   the <See Type=TVector2D> type is used instead.
}
  TPoint2D = record
    X, Y, W: TRealType;
  end;

{: This type defines a 2D vector or direction.

   Use this type when you need to defines directions in the
   2D space. In the case of 2D application this type may be
   used in defining parametric segments or to specify
   ortogonal segments.
}
  TVector2D = record
    X, Y: TRealType;
  end;

{: This type defines a 2D axis aligned rectangle.

   The rectangle is specified by two of its corners, the
   lower-left ones and the upper-right ones.

   This type is useful to defines bounding boxes of shapes.
}
  TRect2D = record
    case Byte of
      0: (Left, Bottom, W1, Right, Top, W2: TRealType);
      1: (FirstEdge, SecondEdge: TPoint2D);
  end;

{: This type defines a 2D transformation for homogeneous points
   and vectors.

   The convention used by the library is that a matrix premultiply
   a point, that is:

   <Code=TP = M * T>

   where <I=TP> and <I=T> are points and <I=M> is a matrix.

   The matrix is specified by columns, that is <I=M[2, 1]> is
   the element at row 2 and column 1 and <I=M[1, 2]> is the
   element at row 1 anc column 2.
}
  TTransf2D = array[1..3, 1..3] of TRealType;

  {: Vector of 2D points. }
  TVectPoints2D = array[0..0] of TPoint2D;
  {: Pointer to vector of 2D points. }
  PVectPoints2D = ^TVectPoints2D;

  // A type for winding rule - non-zero or even/odd
  TWinding = (wn_NonZero, wn_EvenOdd);

const
  TWOPI = 6.28318530717959;
  Sqrt2 = 1.414213562373; // sqrt(2)
  ISqrt2 = 0.707106781186547; // 1/sqrt(2)

// Constants used with the picking functions.
  PICK_NOOBJECT = -200;
  PICK_INBBOX = -100;
  PICK_ONINOBJECT = -1;
  PICK_ONOBJECT = -2;
  PICK_INOBJECT = -3;

// Identity matrix for 2D transformation.
  IdentityTransf2D: TTransf2D = ((1.0, 0.0, 0.0),
    (0.0, 1.0, 0.0), (0.0, 0.0, 1.0));
// Null matrix for 2D transformation.
  NullTransf2D: TTransf2D = ((0.0, 0.0, 0.0),
    (0.0, 0.0, 0.0), (0.0, 0.0, 1.0));

  {: This function returns the angle in radiants that
     corresponds to <I=A> in degrees.
  }
function DegToRad(const A: TRealType): TRealType;
  {: This function returns the angle in degree that
     corresponds to <I=A> in angles.

     The resulting angle is in the range <I=0-360°>.
  }
function RadToDeg(const A: TRealType): TRealType;
  {: Use this function to initialize a 2D point variable.

     This function returns always a cartesing point (W=1).

     Parameters:

     <LI=<I=X> is the X coordinate of the point.>
     <LI=<I=Y> is the Y coordinate of the point.>
  }

function Point2D(const X, Y: TRealType): TPoint2D;
  {: Use this function to initialize a 2D rectangle variable.

     This function returns always a cartesian rectangle (with W=1).

     Parameters:

     <LI=<I=Left> is the left X coordinate of the rectangle.>
     <LI=<I=Bottom> is the bottom Y coordinate of the rectangle.>
     <LI=<I=Right> is the right X coordinate of the rectangle.>
     <LI=<I=Top> is the top Y coordinate of the rectangle.>
  }
function Rect2D(const Left, Bottom, Right, Top: TRealType):
  TRect2D;
  // Create a Vector
function V2D(const X, Y: TRealType): TVector2D;
  {: Use this function to create a versor.

     A versor is vector whose length is 1. This function compute
     this kind of vector by dividing the vector by its length.
     If the lenght is zero an exception will be raised.

     Parameters:

     <LI=<I=X> is the X coordinate of the versor.>
     <LI=<I=Y> is the Y coordinate of the versor.>
  }
function Versor2D(const X, Y: TRealType): TVector2D;
function ScaleVector2D(const V: TVector2D;
  const S: TRealType): TVector2D;
  {: This function normalizes a vector.

     Normalizing a Vector means that the vector coordinates
     are divided by the length of the vector. If the lenght is
     zero an exception will be raised.

     Parameters:

     <LI=<I=V> is the vector being normalized.>
  }
function NormalizeVector2D(const V: TVector2D): TVector2D;
  {: This function returns a versor that represent the direction from
     the point <I=PFrom> to the point <I=PTo>.

     The function computes the vector beetwen the two points
     and then normalizes it.

     Parameters:

     <LI=<I=PFrom> is the starting point of the direction.>
     <LI=<I=PTo> is the ending point of the direction.>
  }
function Direction2D(const PFrom, PTo: TPoint2D): TVector2D;
  {: This function returns the vector from the point
     <I=PFrom> to the point <I=PTo>.

     A vector represents the difference of the coordinates
     from the two points.

     The points are made cartesian before the creation of the vector.
  }
function Vector2D(PFrom, PTo: TPoint2D): TVector2D;
  {: This function returns the modulus of the vector, that
     is its length.

     This value is also called the <I=Euclidian Norm> of the vector.

     Parameters:

     <LI=<I=V> is the versor on which the function is applied.>
  }
function VectorLength2D(const V: TVector2D): TRealType;
function VectorAdd2D(const V1, V2: TVector2D): TVector2D;
function VectorSubtract2D(const V1, V2: TVector2D): TVector2D;

function VectorAngle2(const X, Y: TRealType): TRealType;
function VectorAngle(const V: TVector2D): TRealType;
function TwoPointsAngle(P0, P1: TPoint2D): TRealType;
function TwoVectorsAngle2(X1, Y1, X2, Y2: TRealType): TRealType;
function PolarVector(const Radius, Angle: TRealType): TVector2D;

//TSY:
function ShiftPoint(const P: TPoint2D; const V: TVector2D):
  TPoint2D;
function ShiftPointScale(const P: TPoint2D; const V: TVector2D;
  const S: TRealType): TPoint2D;

  {: This function returns <B=True> if two points are equal
     and <B=False> otherwise.

     All the coordinates (also W) are tested.

     Parameters:

     <LI=<I=P1> is the first point.>
     <LI=<I=P2> is the second point.>
  }
function IsSamePoint2D(P1, P2: TPoint2D): Boolean;
  {: This function returns <B=True> if two vectors are equal
     and <I=False> otherwise.

     All the coordinates (also W) are tested.

     Parameters:

     <LI=<I=V1> is the first vector.>
     <LI=<I=V2> is the second vector.>
  }
function IsSameVector2D(const V1, V2: TVector2D): Boolean;
  {: This function returns <B=True> if two transform matrices
     are equal and <B=False> otherwise.

     The function use optimizations if the two matrices are
     cartesian matrices.

     Parameters:

     <LI=<I=T1> is the first matrix.>
     <LI=<I=T2> is the second matrix.>
  }
function IsSameTransform2D(const T1, T2: TTransf2D): Boolean;
  {: This function returns <B=True> if a transform matrix is
     a cartesian matrix, that is a matrix with the third column
     equal to [0, 0, 1].

     Parameters:

     <LI=<I=T> is the matrix being tested.>
  }
function IsCartesianTransform2D(const T: TTransf2D): Boolean;
  {: This function returns the cartesian point equivalent
     to <I=P>.

     A cartesian point has the W coordinate equals to 1.
     To convert the point all the coordinates are divided by W.

     When a point is made cartesian, there is no way to obtain
     the original point back.

     Parameters:

     <LI=<I=P> is the point being transformed.>
  }
function CartesianPoint2D(const P: TPoint2D): TPoint2D;
  {: This function returns the cartesian rectangle equivalent
     to <I=R>.

     A cartesian rectangle has the W coordinate of its corners
     equals to 1. To convert the rectangle all the coordinates
     of the two corners are divided by their W.

     When a rectangle is made cartesian, there is no way to
     obtain the original rectangle back.

     Parameters:

     <LI=<I=R> is the rectangle being transformed.>
  }
function CartesianRect2D(const R: TRect2D): TRect2D;
  {: This function returns an ordered rectangle in which the
     first point is the left-bottom one and the second point
     is the right-top one.

     The returned rectangle is always cartesian.

     Parameters:

     <LI=<I=R> is the rectangle being ordered.>
  }
function ReorderRect2D(const R: TRect2D): TRect2D;
  {: This function transforms a point into a point in
     which the coordinates are integers. To do the
     transformation the function <I=Round> is used.

     The resulting point is specified in screen coordinate
     system.

     Parameters:

     <LI=<I=P2D> is the point being transformed.>
  }
function Point2DToPoint(const P2D: TPoint2D): TPoint;
//TSY:
  {: This function transforms a point with integer
     coordinates into a point with real coordinates.

     <I=P> is considered as specified in screen
     coordinate system.

     Parameters:

     <LI=<I=P> is the point being transformed.>
  }
function PointToPoint2D(const P: TPoint): TPoint2D;
  {: This function transforms a rectangle with integer
     coordinates into a rectangle with real coordinates.

     <I=R> is considered as specified in screen
     coordinate system.

     Parameters:

     <LI=<I=R> is the rectangle being transformed.>
  }
function RectToRect2D(const R: TRect): TRect2D;
  {: This function transforms a rectangle with real coordinates
     into a rectangle with integer coordinates.
     To do the transformation the function <I=Round> is used.

     The resulting rectangle is specified in screen coordinate
     system.

     Parameters:

     <LI=<I=R> is the rectangle being transformed.>
  }
function Rect2DToRect(R: TRect2D): TRect;
//Center of rectangle
function BoxCenter(const R: TRect2D): TPoint2D;
function BoxBottomRight(const R: TRect2D): TPoint2D;
function BoxTopLeft(const R: TRect2D): TPoint2D;
  {: This function enlarges a rectangle by the specified
     percentual.

     The resulting rectangle is always cartesian.

     Parameters:

     <LI=<I=R> is the box being enlarged.>
     <LI=<I=Perc> is the percentual by which enlarge the box.
     A value of 0.5 means the the rectangle is doubled (50%).>
  }
function EnlargeBoxPerc2D(R: TRect2D; const Perc: TRealType):
  TRect2D;
  {: This function enlarges a rectangle by the specified
     delta in all directions.

     The resulting rectangle is always cartesian.

     Parameters:

     <LI=<I=R> is the box being enlarged.>
     <LI=<I=Delta> is the amount by which enlarge the box.
     A value of 0.1 means the the rectangle is enlarged by
     0.1 along the X-Y directions.
  }
function EnlargeBoxDelta2D(R: TRect2D; const Delta: TRealType):
  TRect2D;
  {: This function multiplies two affine matrix. It
     returns the resulting matrix.

     The order of multiplication is important for matrixes.

     This function may be useful to concatenate transformation
     matrixes.

     Parameters:

     <LI=<I=M1> is the first matrix.>
     <LI=<I=M2> is the second matrix.>
  }
function MultiplyTransform2D(const M1, M2: TTransf2D):
  TTransf2D;
  {: This function returns the invers of a matrix.

     The matrix must be non singular (that is its determinant
     is non zero).

     This function may be useful to obtain the inverse of
     a transformation.

     Parameters:
     <LI=<I=M> is the matrix to be inverted.>
  }
function InvertTransform2D(const M1: TTransf2D): TTransf2D;
  {: This function transforms a points by using a
     transformation matrix.
     Parameters:
     <LI=<I=P> is the point to be transformed.>
     <LI=<I=T> is the transformation matrix.>
  }
function TransformPoint2D(const P: TPoint2D; const T:
  TTransf2D): TPoint2D;
  {: This function transforms a vector by using a transformation
     matrix.
     Parameters:
     <LI=<I=V> is the vector to be transformed.>
     <LI=<I=T> is the transformation matrix.>
  }
function TransformVector2D(const V: TVector2D; const T:
  TTransf2D): TVector2D;
  {: This function transforms a rectangle by using a
     transformation matrix. The first corner and the second corner
     of the rectangle are transformed.
     Parameters:
     <LI=<I=R> is the rectangle to be transformed.>
     <LI=<I=T> is the transformation matrix.>
  }
function TransformRect2D(const R: TRect2D; const T: TTransf2D):
  TRect2D;
  {: This function transforms a bounding box by using a
     transformation matrix. The resulting rectangle is a the
     bounding box that fully contains the passed rectangle.
     Parameters:
     <LI=<I=R> is the rectangle to be transformed.>
     <LI=<I=T> is the transformation matrix.>
  }
function TransformBoundingBox2D(const Box: TRect2D; const
  Transf: TTransf2D): TRect2D;
  {Form transform matrix
    | A  B  0 |
    | C  D  0 |
    | E  F  1 |  }
function Transform2D(const A, B, C, D, E, F: TRealType): TTransf2D;

  {: This function returns a transformation matrix that
     correspond to a 2D translation.

     Parameters:

     <LI=<I=Tx> is the translation among the X axis.>
     <LI=<I=Ty> is the translation among the Y axis.>
  }
function Translate2D(const TX, TY: TRealType): TTransf2D;

  {: This function returns a transformation matrix that
     correspond to a 2D rotation.

     The center of rotation is in (0, 0). To rotate along a
     point (px, py), concatenate the following matrixes:
     <Code=
       Translate(-px, -py) -> Rotate2D(A) -> Translate(px, py)
     >
     Parameters:

     <LI=<I=R> is the angle of rotation in radiants.>
  }
function Rotate2D(const R: TRealType): TTransf2D;

//TSY: rotate over point (CX,CY), R radians
function RotateCenter2D(const R: TRealType;
  const C: TPoint2D): TTransf2D;

  {: This dunction returns a transformation matrix that
     correspond to a 2D scaling.

     Parameters:

     <LI=<I=Sx> is the scaling among the X axis.>
     <LI=<I=Sy> is the scaling among the Y axis.>
  }
function Scale2D(const Sx, Sy: TRealType): TTransf2D;
//TSY: scale with central point (CX,CY)
function ScaleCenter2D(const Sx, Sy: TRealType; const C:
  TPoint2D): TTransf2D;

//TSY: flip over a line given by the ortogonal vector from (0,0)
function Flip2D(const P: TPoint2D; const V: TVector2D): TTransf2D;

//TSY: Skew (shear) transform with respect to reference point C.
// AH, AV are angles in radiants
function Skew2D(const AH, AV: TRealType;
  const C: TPoint2D): TTransf2D;

  {: This function creates a matrix transform that maps
     the points in the W window, in points in the V window.

     The function mantains the aspect ratio desired (if non zero).

     Parameters:

     <LI=<I=W> is the source rectangle>
     <LI=<I=V> is the destionation rectangle>
     <LI=<I=Aspect> is the aspect ratio, that is <I=XW/YW>.>
  }
function GetVisualTransform2D(var W: TRect2D; const V: TRect;
  Aspect: TRealType): TTransf2D;
  {: This procedure may be used to apply an ortogonal
     constraint to a point.

     The parameter <I=CurrPt> is modified so that the
     ray beetwen <I=LastPt> and <I=CurrPt> is parallel
     to the axis <I=X> or <I=Y>. The new <I=CurrPt> will
     be on that ray, and the ray will be parallel to the
     axis that lead to the greter lenght on the ray.

     Parameters:

     <LI=<I=LastPt> is the reference point>
     <LI=<I=CurrPt> is the point to be constrained.>
  }
procedure MakeOrto2D(LastPt: TPoint2D; var CurrPt: TPoint2D);
{Rounding direction towards the multiples of 45deg}
procedure Make45_2D_V(var V: TVector2D);
procedure Make45_2D(LastPt: TPoint2D; var CurrPt: TPoint2D);
  {: This function returns the dot product of two vectors.
     The resulting value is given by <I=S=a.X*b.X+a.Y*b.Y>.

     If the vectors are versors, then the result value is
     the coseno of the angle beetwen the versors.

     Parameters:

     <LI=<I=A> is the first vector>
     <LI=<I=B> is the second vector>
  }
function DotProduct2D(const A, B: TVector2D): TRealType;
  {: This function returns the vector that is
     perpendicular to the given one.

     Parameters:

     <LI=<I=V> is the reference vector>
  }
function Perpendicular2D(const V: TVector2D): TVector2D;
  {: This function returns the vector that rapresent
     the opposite of the given one.

     The result will be <I=(-V.X, -V.Y)>.

     Parameters:

     <LI=<I=V> is the reference vector>
  }
function Reflect2D(const V: TVector2D): TVector2D;
function MixPoint(const P1, P2: TPoint2D; Mix: TRealType):
  TPoint2D;
function MidPoint(const P1, P2: TPoint2D): TPoint2D;
function IsotropicScale(const T: TTransf2D): TRealType;
  {: This function clips a segment against a rectangular
     clipping region.

     The function returns the clipping result code
     and change <I=Pt1> and <I=Pt2> on the base of the clipping.

     The resulting value can be one of the following:

     <LI=<I=[ccNotVisible]> if the segment is not visible
      in the clipping region.>
     <LI=<I=[ccVisible]> if the segment is fully contained
      in the clipping region.>
     <LI=<I=[ccFirstEdge]> if the first point of segment
      was clipped.>
     <LI=<I=[ccSecondEdge]> if the second point of segment
      was clipped.>
     <LI=<I=[ccFirstEdge, ccSecondEdge]> if the bothe point
      of segment were clipped.>

     Parameters:

     <LI=<I=Clip> is the clipping region.>
     <LI=<I=Pt1> is the first point of the segment.>
     <LI=<I=Pt2> is the second point of the segment.>
  }
  //TSY:
function _ClipLine2D(const Clip: TRect2D; var Pt1, Pt2:
  TPoint2D): TClipResult;
function _ClipLineLeftRight2D(const Clip: TRect2D; var Pt1, Pt2:
  TPoint2D): TClipResult;
function _ClipLineUpBottom2D(const Clip: TRect2D; var Pt1, Pt2:
  TPoint2D): TClipResult;
function ClipLine2D(Clip: TRect2D; var Pt1, Pt2: TPoint2D):
  TClipResult;
  {: This function clips a segment against the left and right
     edges of a rectangular clipping region.

     The function returns the clipping result code and change
     <I=Pt1> and <I=Pt2> on the base of the clipping.

     The resulting value can be one of the following:

     <LI=<I=[ccNotVisible]> if the segment is not visible
      in the clipping region.>
     <LI=<I=[ccVisible]> if the segment is fully contained in
      the clipping region.>
     <LI=<I=[ccFirstEdge]> if the first point of segment
      was clipped.>
     <LI=<I=[ccSecondEdge]> if the second point of segment was
      clipped.>
     <LI=<I=[ccFirstEdge, ccSecondEdge]> if both the point
      of segment were clipped.>

     Parameters:

     <LI=<I=Clip> is the clipping region.>
     <LI=<I=Pt1> is the first point of the segment.>
     <LI=<I=Pt2> is the second point of the segment.>
  }
function ClipLineLeftRight2D(Clip: TRect2D; var Pt1, Pt2:
  TPoint2D): TClipResult;
  {: This function clips a segment against the upper and
     bottom edges of a rectangular clipping region.

     The function returns the clipping result code and
     change <I=Pt1> and <I=Pt2> on the base of the clipping.

     The resulting value can be one of the following:

     <LI=<I=[ccNotVisible]> if the segment is not visible in
      the clipping region.>
     <LI=<I=[ccVisible]> if the segment is fully contained in
      the clipping region.>
     <LI=<I=[ccFirstEdge]> if the first point of segment was
      clipped.>
     <LI=<I=[ccSecondEdge]> if the second point of segment
      was clipped.>
     <LI=<I=[ccFirstEdge, ccSecondEdge]> if both the point
      of segment were clipped.>

     Parameters:

     <LI=<I=Clip> is the clipping region.>
     <LI=<I=Pt1> is the first point of the segment.>
     <LI=<I=Pt2> is the second point of the segment.>
  }
function ClipLineUpBottom2D(Clip: TRect2D; var Pt1, Pt2:
  TPoint2D): TClipResult;
  {: This function returns the distance beetwen two points.

     The resulting value is always positive.

     Parameters:

     <LI=<I=P1> is the first point>
     <LI=<I=P2> is the second point>
  }
function PointDistance2D(const P1, P2: TPoint2D): TRealType;
  {: This function returns <B=True> if the point P is in the axis aligned
     box with the side equal to 2*Aperture and centered at the point <I=RP>.
     So a point may be considered near also if its distance from RP is
     greater than Aperture.

     Parameters:

     <LI=<I=RP> is the reference point>
     <LI=<I=P> is the testing point>
     <LI=<I=Aperture> is the half side of reference box>
     <LI=<I=Dist> will contains the distance of <I=P>
      from <I=RP>. If the function returns <B=False>,
      <I=Dist> will be set to <See const=MaxCoord>.
  }
function NearPoint2D(RP, P: TPoint2D; const Aperture: TRealType;
  var Dist: TRealType): Boolean;

// projection of point to a linear segment
function PointSegmentProj2D(const P, P0, P1: TPoint2D): TPoint2D;
function PointSegmentDistance2D(const P, P1, P2: TPoint2D):
  TRealType;
  {: This function returns a position code that represent the
    position of a point respect to the given rectangle.

    The result value is one of the following:

    <LI=<I=PICK_NOOBJECT> if the point <I=P> isn't near to
     the rectangle>
    <LI=<I=PICK_ONOBJECT> if the point <I=P> is near to the
     passed rectangle by a distance less than <I=Aperture>.>
    <LI=<I=PICK_INOBJECT> if the point is inside the rectangle.>

    Parameters:

    <LI=<I=Box> is the axis alligned rectangle>
    <LI=<I=P> is the testing point>
    <LI=<I=Dist> will contains the real distance of <I=P> from
     the box. If the function returns
     <I=PICK_NOOBJECT>, <I=Dist> will be set to <See const=MaxCoord>.>
    <LI=<I=Aperture> is the reference distance>
    <LI=<I=T> is an optional transformation matrix. The
     box is transformed by this matrix before the testing.>
  }
function IsPointOnRect2D(const Box: TRect2D; P: TPoint2D; var
  Dist: TRealType;
  const Aperture: TRealType; const T: TTransf2D): Integer;

  {: This function returns <B=True> if the testing point is
     inside the reference rectangle.

     The function works on the assumption that the parameters
     are cartesian points (W = 1.0).

     Parameters:

     <LI=<I=Box> is the axis alligned rectangle>
     <LI=<I=P> is the testing point>
  }
function IsPointInCartesianBox2D(const P: TPoint2D; const Box:
  TRect2D): Boolean;

  {: This function returns the smallest axis alligned rectangle
     that contains the given point and the given rectangle.

     The function works on both ordinary and non ordinary
     points, for the function makes the points ordinary (W=1.0)
     before do its job.

     Parameters:

     <LI=<I=Pt> is the point>
     <LI=<I=Box> is the axis alligned rectangle>
  }
function PointOutBox2D(P: TPoint2D; Box: TRect2D): TRect2D;

  {: This function returns <B=True> if Box1 is completely or
     partially contained in Box2.

    The function works on both ordinary and non ordinary
    points, for the function makes the points ordinary
    (W=1.0) before do its job.

    Parameters:

    <LI=<I=Box1> is the first axis alligned rectangle>
    <LI=<I=Box2> is the second axis alligned rectangle>
  }
function IsBoxInBox2D(Box1, Box2: TRect2D): Boolean;
  {: This function returns <B=True> if <I=Box2> is fully
     contained into <I=Box2>.

     The function works on both ordinary and non ordinary
     points, for the function makes the points ordinary
     (W=1.0) before do its job.

     Parameters:

     <LI=<I=Box1> is the first axis alligned rectangle>
     <LI=<I=Box2> is the second axis alligned rectangle>
  }
function IsBoxAllInBox2D(Box1, Box2: TRect2D): Boolean;
  {: This function returns <B=True> if <I=Box2> is fully
     contained into <I=Box2>.

     The function works on the assumption that the parameters
     are ordinary points (W = 1.0).

     Parameters:

     <LI=<I=Box1> is the first axis alligned rectangle>
     <LI=<I=Box2> is the second axis alligned rectangle>
  }
function IsBoxAllInCartesianBox2D(const Box1, Box2: TRect2D):
  Boolean;

  {: This function returns the smallest axis alligned rectangle
     that contains the given rectangles.

     The function works on both ordinary and non ordinary
     points, for the function makes the points ordinary (W=1.0)
     before do its job.

     Parameters:

     <LI=<I=Box1> is the first axis alligned rectangle>
     <LI=<I=Box2> is the second axis alligned rectangle>
  }
function BoxOutBox2D0(const Box1, Box2: TRect2D): TRect2D;
// The same function with reordering of bounds
function BoxOutBox2D(Box1, Box2: TRect2D): TRect2D;

function BezierBasic(
  const A0, A1, A2, A3, U: TRealType): TRealType;
function BezierPoint(const P0, P1, P2, P3: TPoint2D;
  const U: TRealType): TPoint2D;
function BezierDerivative(const P0, P1, P2, P3: TPoint2D;
  const U: TRealType): TVector2D;
procedure SmallArcBezier(CP: TPoint2D; R, SA, EA: TRealType;
  var P0, P1, P2, P3: TPoint2D);
{ Position at Bezier curve which is closest to P }
function ClosestBezierPoint(
  const P, P0, P1, P2, P3: TPoint2D;
  const Precision: TRealType): TRealType;
procedure BreakBezier(var P0, P1, P2, P3, P4, P5, P6: TPoint2D;
  const U: TRealType);
procedure BreakBezierMid(var P0, P1, P2, P3, P4, P5, P6: TPoint2D);
procedure RectangleCalcPoints(P0, P1, P2: TPoint2D;
  var P3, P4: TPoint2D; var A: TRealType);

type

{: This type defines a message handler function that is called
   when a <See Class=TPointsSet2D> object is changed.

   It has as arguments the vector that has called the handler.
}
  TOnChangePointsSet = procedure(Sender: TObject) of object;
{: This class defines a set of points that can be drawed as
   a polyline or filled polygon.

   The points in the set are single precision points (<See Class=TPoint2D>)
   that are clipped against a 2D axis aligned rectangle before drawed
   on a Canvas.

   The class has also picking capabilities as well as a method
   to apply a trasform to all the points.

   The vector can grow when new points are added.

   This class is very useful in defining new shapes to be used with
   the library (see also <See Class=TPrimitive2D>, <See Class=TCurve2D>).
}
  TPointsSet2D = class(TObject)
  protected
    fPoints: Pointer;
    fCapacity, fCount: Integer;
    fGrowEnabled: Boolean;
    fTag: Integer;

    procedure PutProp(Index: Integer; const Item: TPoint2D);
      virtual;
    {: This method is called whenever a point is requested from the set.

       <I=Index> is the index of the point to be extracted (the set
       is like an array so the inserted points are referred to by a
       numeric index value from 0 to Count - 1).

       The method must return a point or raise a 'out of bound'
       exception.
    }
    function Get(Index: Integer): TPoint2D; virtual;
    {: This method is called whenever a point in the set is to be replaced.

       <I=PutIndex> is the index of point where to store
       <I=Item>; <I=ItemIndex> is the index of <I=Item> if
       it is already present in the list (so it will be replaced by
       <I=Item>); <I=Item> is the modified point.

       <B=Note>: You may want to redefine this method if you want
       to customize the set by adding new informations to all the
       points. For instance if you want to add a type specifier to
       all the points you have to override this method and use the
       specified indexes to manage the extra infos. For these
       remember that:

       <LI=<I=PutIndex> is equal to <I=ItemIndex> if
            a point is being replaced with a new one.>
       <LI=<I=ItemIndex>=<I=PutIndex> - 1 if the point
            at <I=PutIndex> is being replaced with the previous
            point in the set (this happens when <See Method=TPointsSet2D@Insert> is
            called)>
       <LI=<I=ItemIndex>=<I=PutIndex> + 1 if the point
            at <I=PutIndex> is being replaced with the next
            point in the set (this happens when <See Method=TPointsSet2D@Delete> is
            called).>
    }
    procedure Put(PutIndex, ItemIndex: Integer; const Item:
      TPoint2D); virtual;
  public
    {: This method create a new instance of the class setting its capacity
       to <I=_Capacity>.

       <I=_Capacity> is the initial number of points that can be stored
       in the set. If <See Property=TPointsSet2D@GrowingEnabled> is <B=True>
       you can add more that <I=_Capacity> points to the set, otherwise
       a 'out of bound' exception will be raised if
       you try to do so.

       Setting <I=_Capacity> to the real number of points in the set
       will speed up the insertion of the point in the set.
    }
    constructor Create(const _Capacity: Integer); virtual;
    {: Free the memory used by the instance of the class.

       Remember to call the <I=Free> method of the class when done
       with the class or memory leak will result.
    }
    destructor Destroy; override;
    {: Clear the set.

       <B=Note> that the memory used by the set is not freed for
       optimization. Only the <See Property=TPointsSet2D@Count> is reset to zero.
    }
    procedure Clear;
    procedure Expand(const NewCapacity: Integer);
    {: Copy a subset of the points from another set. The
       points are copied in the respective positions, so
       the second point of S is copied on the second point
       of Self.

       <I=S> is the source set from which the points will be
       copied; <I=StIdx> and <I=EndIdx> are the start and end
       index of the points in <I=S> to be copied respectively.

       After the copy the <See Property=TPointsSet2D@OnChange> event will
       be fired.

       <B=Note>: If <See Property=TPointsSet2D@GrowingEnabled> is
       <B=True> the size of the vector may grow if there is no
       space for the points, otherwise only the points that sit into
       the vect are copied and a 'out of bound'
       exception will be raised if you try to do so.
    }
    procedure Copy(const S: TPointsSet2D; const StIdx, EndIdx:
      Integer); virtual;
    {: Add an item at the end of the set.

       <I=Item> is the point to be added. The point is added
       after any other point.

       After the copy the <See Property=TPointsSet2D@OnChange> event will
       be fired.

       <B=Note>: If <See Property=TPointsSet2D@GrowingEnabled> is
       <B=True> the size of the vector may grow if there is no
       space for the point, otherwise a 'out of bound'
       exception will be raised if you try to do so.
    }
    procedure Add(const Item: TPoint2D);
    {: Add a set all the points of a set to the current one.

       <I=Items> is the set from which copy the points. The
       points will be added in the same order the have in <I=Items>
       at the end of the current set.

       After the copy the <See Property=TPointsSet2D@OnChange> event will
       be fired.

       <B=Note>: If <See Property=TPointsSet2D@GrowingEnabled> is
       <B=True> the size of the vector may grow if there is no
       space for the points, otherwise only the points that sit into
       the vect are copied and a 'out of bound'
       exception will be raised if you try to do so.
    }
    procedure AddPoints(const Items: array of TPoint2D); virtual;
    procedure AppendPoints(PP: TPointsSet2D); virtual;
    {: Transform all the points in the set which a transformation matrix.
       After the copy the <See Property=TPointsSet2D@OnChange> event will
       be fired.
    }
    procedure TransformPoints(const T: TTransf2D); virtual;
    procedure ReversePoints; virtual;
    {: Remove a point from the set.

       <I=Index> is the index value of the point to be removed.
       After the copy the <See Property=TPointsSet2D@OnChange> event will
       be fired.

       <B=Note 1>: Removing a point from a set is a time consuming
       operation (or better it may be so if the set is a very large
       one).

       <B=Note 2>: If the <I=Index> is not in the vector a
       'out of bound' exception will be raised.
    }
    procedure Delete(const Index: Integer); virtual;
    {: Insert a point into the set.

       <I=Index> is the index value at which the point will be
       inserted. All the points in the set from the one next of
       <I=Index> position to the end will be moved by one
       to make space for the new point. <I=Item> is the new point to be
       inserted.

       <B=Note 1>: Insert a point into a set is a time consuming
       operation (or better it may be so if the set is a very large
       one).

       <B=Note 2>: If <See Property=TPointsSet2D@GrowingEnabled> is
       <B=True> the size of the vector may grow if there is no
       space for the point, otherwise a 'out of bound'
       exception will be raised if you try to do so.
    }
    procedure Insert(const Index: Integer; const Item: TPoint2D);
      virtual;
    {: This property contains the bounding box of the set, that is
       the smaller axis-alligned rectangle that fully contains the
       points in the set.

       <B=Note>: Because this method compute the bounding box every
       time it is called you may want to store it in a temporary
       variable if you want to use it in different part of your
       function (obviously if you don't change the set between
       uses of the bounding box).
    }
    procedure GetBoundingBox0(var Rect: TRect2D); virtual;
    function GetBoundingBox: TRect2D; virtual;
// Is given point P lies on a polyline defined by the points?
    function IsPointOnPolylineStroke(
      const P: TPoint2D; const Closed: Boolean;
      var Distance: TRealType;
      out Pos: Integer): Boolean; virtual;
// Is given point P lies inside a polygon defined by the points?
    function IsPointInside(const P: TPoint2D;
      const Precision: TRealType): Boolean; virtual;
    {: This property contains the points in the set in an array-like
       fashion.

       <I=Index> is the index of the point to be accessed and it
       is zero-based (that is the points in the set have
       indexes 0,1,2,...).

       <B=Note 1>: This is a default property so you can drop the
       name of the property, for instance you may use:

       <Code=AVect[2]>

       instead of:

       <Code=AVect.Points[2]>

       <B=Note 2>: If <I=Index> is greater than
       <See Property=TPointsSet2D@Count> a 'out of bound' exception
       will be raised.
    }
    property Points[Index: Integer]: TPoint2D read Get write
    PutProp; default;
    {: This property contains the number of points in the set.

       You may use this property to iterate the points in the set.

       <B=Note>: This property is always less than <See Property=TPointsSet2D@Capacity>.
    }
    property Count: Integer read fCount;
    {: This property contains the maximun number of points
       that can be added or inserted into the set without growing
       it (if possible).
    }
    property Capacity: Integer read fCapacity;
    {: If this property is set to <B=True> then the set can grow
       if you add points when <See Property=TPointsSet2D@Count> is equal to
       <See Property=TPointsSet2D@Capacity>, otherwise a 'out of bound'
       exception will be raised if you try to do so.
    }
    property GrowingEnabled: Boolean read fGrowEnabled write
      fGrowEnabled;
    {: This property contains the points reference, that is a pointer
       to the underling set of points.
    }
    property PointsReference: Pointer read fPoints;
    {: The Tag property for user's information.

       It is not saved.
    }
    property Tag: Integer read fTag write fTag;
    property PointsPointer: Pointer read fPoints;
  end;

{
   An extension of TPointsSet2D.
   When new points are added, the class can call a user defined
   function whenever this occour.
}
  TEPointsSet2D = class(TPointsSet2D)
  protected
    fDisableEvents: Boolean;
    fOnChange: TOnChangePointsSet;
    procedure PutProp(Index: Integer; const Item: TPoint2D);
      override;
    procedure SetDisableEvents(B: Boolean);
    procedure CallOnChange;
  public
    constructor Create(const _Capacity: Integer); override;
    procedure AddPoints(const Items: array of TPoint2D); override;
    procedure AppendPoints(PP: TPointsSet2D); override;
    procedure Delete(const Index: Integer); override;
    procedure Insert(const Index: Integer; const Item: TPoint2D);
      override;
    procedure TransformPoints(const T: TTransf2D); override;
    procedure Copy(const S: TPointsSet2D; const StIdx, EndIdx:
      Integer); override;
    {: EVENTS}
    {: This event is fired whenever the set is changed.

       In the <I=Sender> argument of the event handler the changed
       set is passed.

       <B=Note>: If you change the vector in the event handler no
       new event will be fired.
    }
    {: If this property is set to <B=True> then the
       <See Property=TPointsSet2D@OnChange> event will not be fired when
       the set change.
    }
    property DisableEvents: Boolean read fDisableEvents write
      SetDisableEvents;
    property OnChange: TOnChangePointsSet read fOnChange write
      fOnChange;
  end;

  TCircularKind = (ci_Arc, ci_Sector, ci_Segment);

  TArcDirection = (ad_Clockwise, ad_CounterClockwise);

// Distance from point P to polyline given by points PP
// T is an optional transformation matrix.
// If Closed is True the polyline will be closed (if it is not already closed)
// Pos gives position of a point on a polyline (which segment)
function PointPolylineDistance(
  const PP: TPointsSet2D; P: TPoint2D;
  const T: TTransf2D; const Closed: Boolean;
  out Pos: Integer): TRealType;
  // Is a given point inside a given polygon?
function IsPointInPolygon(
  const PP: TPointsSet2D; P: TPoint2D;
  const T: TTransf2D; const WRule: TWinding): Boolean;
function IsPointOnBezierStroke(
  const PP: TPointsSet2D; const P: TPoint2D;
  const Closed: Boolean; const Precision: TRealType;
  var Distance: TRealType;
  out Pos: Integer): Boolean;
function IsPointInCircular(
  const P, CP: TPoint2D; const R, SA, EA: TRealType;
  const Kind: TCircularKind): Boolean;
function IsPointOnCircular(
  const P, CP: TPoint2D; const R, SA, EA: TRealType;
  const Kind: TCircularKind;
  const Precision: TRealType; var Distance: TRealType): Boolean;
function IsPointInEllipse(
  const P, CP: TPoint2D;
  const RX, RY, ARot: TRealType): Boolean;
function IsPointOnEllipse(
  const P, CP: TPoint2D;
  const RX, RY, ARot, Precision: TRealType): Boolean;
function IsPointInBezierShape(
  const PP: TPointsSet2D; P: TPoint2D;
  const Closed: Boolean; const T: TTransf2D;
  const WRule: TWinding;
  const Precision: TRealType): Boolean;

// Bounding box for an array of points
function PointsBoundingBox(
  PointsArray: array of TPoint2D): TRect2D;
procedure BezierSegmentBoundingBox(
  const P0, P1, P2, P3: TPoint2D;
  var Rect: TRect2D);
procedure BezierPathBoundingBox(
  const PP: TPointsSet2D;
  const Closed: Boolean;
  var Rect: TRect2D);
// Bounding box for a circle
function CirceBoundingBox(
  const CP: TPoint2D; const R: TRealType): TRect2D;
// Bounding box for a circular object (arc, sector or segment)
function CircularBoundingBox(
  const CP: TPoint2D; const R: TRealType;
  SA, EA: TRealType; const Kind: TCircularKind): TRect2D;
// Bounding box for a rectangle
function RectangleBoundingBox(
  const P: TPoint2D;
  const W, H, ARot: TRealType): TRect2D;
// Bounding box for an ellipse
function EllipseBoundingBox(
  const CP: TPoint2D;
  const RX, RY, ARot: TRealType): TRect2D;

// Bezier path approximation for an ellipse, 8 arcs
procedure EllipseBezierPoints8(CP: TPoint2D;
  RX, RY, ARot: TRealType; PP: TPointsSet2D);
// Bezier path approximation for a cicualar object (arc, sector or segment)
procedure CircularBezierPoints(CP: TPoint2D; R, SA, EA: TRealType;
  PP: TPointsSet2D; const Kind: TCircularKind);
// Bezier path for a rounded rectangle
procedure RoundRectBezierPoints(P: TPoint2D;
  W, H, RX, RY, ARot: TRealType;
  PP: TPointsSet2D);
// Polygon approximation for a circle
procedure LinearizeCircle(PP: TPointsSet2D;
  const CP: TPoint2D; const R: TRealType;
  const CurvePrecision: Integer);
// Polygon approximation for a cicualar object
procedure LinearizeCircular(PP: TPointsSet2D;
  const CP: TPoint2D; const R, SA, EA: TRealType;
  const Kind: TCircularKind;
  const CurvePrecision: Integer);
procedure RectanglePoints(const P: TPoint2D;
  const W, H, ARot: TRealType;
  out P0, P1, P2, P3: TPoint2D);
procedure LinearizeRectangle(PP: TPointsSet2D;
  const P: TPoint2D; const W, H, ARot: TRealType);
procedure LinearizeEllipse(PP: TPointsSet2D;
  const CP: TPoint2D; const RX, RY, ARot: TRealType;
  const CurvePrecision: Integer);
procedure LinearizeBezier_X(const BezierPP: TPointsSet2D;
  Precision: Double; const Closed: Boolean;
  const LinPP: TPointsSet2D);
procedure LinearizeBezier(const BezierPP: TPointsSet2D;
  const Precision: Integer; const Closed: Boolean;
  const LinPP: TPointsSet2D);
procedure LinPolyToBezier(const LinPP: TPointsSet2D;
  const Closed: Boolean; const BezierPP: TPointsSet2D);
procedure GetHobbyBezier(var PP: TPointsSet2D;
  const Points: TPointsSet2D);
procedure GetClosedHobbyBezier(var PP: TPointsSet2D;
  const Points: TPointsSet2D);
procedure DeleteBezierPointSmoothly(
  const P0, P1, P2, P3, P4, P5, P6: TPoint2D;
  var Q1, Q2: TPoint2D);

procedure SimplifyPoly(PP: TPointsSet2D; OutPP: TPointsSet2D;
  const Delta: TRealType; const Closed: Boolean);
procedure SimplifyPolyInPlace(PP: TPointsSet2D;
  const Delta: TRealType; const Closed: Boolean);

function SVG_Path_HasCurves(const PathData: string): Boolean;

type

  T_SVG_Path_ClosePathMethod = procedure(const X, Y: TRealType) of
    object;
  T_SVG_Path_LineToMethod = procedure(const X, Y: TRealType) of
    object;
  T_SVG_Path_BezierToMethod =
    procedure(const X1, Y1, X2, Y2, X3, Y3: TRealType) of object;

  TSetOfChar = set of Char;

  T_SVG_Path_Parser = class
  private
    fParser: TSimpleParser;
    OnClosePath: T_SVG_Path_ClosePathMethod;
    OnMoveTo, OnLineTo: T_SVG_Path_LineToMethod;
    OnBezierTo: T_SVG_Path_BezierToMethod;
  public
    AllAsBezier: Boolean;
    constructor Create(const OnClosePath:
      T_SVG_Path_ClosePathMethod;
      const OnMoveTo, OnLineTo: T_SVG_Path_LineToMethod;
      const OnBezierTo: T_SVG_Path_BezierToMethod);
    destructor Destroy; override;
    procedure Parse(const Source: string);
  end;

function SVG_Transform_Parse(const Source: string): TTransf2D;

type
  THatchingDirection = array[1..4] of Single;

  TFillRule = (fr_Winding, fr_Alternate);

const
  HatchingDirections: array[0..6] of
  THatchingDirection = ((0, 0, 0, 0), (0, 1, 0, 0),
    (1, 0, 0, 0), (1, 1, 0, 0), (1, -1, 0, 0), (1, 0, 0, 1),
    (1, 1, 1, -1));

procedure CalculateHatching(const P: TPointsSet2D;
  const DX, DY: TRealType; Step: TRealType;
  const Lines: TPointsSet2D; const FillRule: TFillRule);

implementation
uses SysUtils, Math, SysBasic;

function StrToRealType(const St: string;
  const DefaultVal: TRealType): TRealType;
var
  J: Integer;
begin
  Val(St, Result, J);
  if J > 0 then Result := DefaultVal;
end;

// 2D Clipping functions.

{ This function returns the position code of P against Clip.
  P and Clip must be cartesian point, Clip must be ordered.
}

function _PositionCode2D(const Clip: TRect2D; const P:
  TPoint2D): TOutCode;
begin
  Result := [];
  if P.X < Clip.Left then
    Result := [Left]
  else if P.X > Clip.Right then
    Result := [Right];
  if P.Y < Clip.Bottom then
    Result := Result + [Bottom]
  else if P.Y > Clip.Top then
    Result := Result + [Top];
end;

{ This function is used to implement the Liang-Barsky clipping method. }

function _ClipPt(const Denom, Num: Extended; var tE, TL:
  Extended): Boolean;
var
  T: Extended;
begin
  Result := False;
  if Denom > 0 then
  begin
    T := Num / Denom;
    if T > TL then
      Exit
    else if T > tE then
      tE := T;
  end
  else if Denom < 0 then
  begin
    T := Num / Denom;
    if T < tE then
      Exit
    else if T < TL then
      TL := T;
  end
  else if Num > 0 then
    Exit;
  Result := True;
end;

{ Implement the Liang-Barsky algoritm. }

function _ClipLine2D(const Clip: TRect2D; var Pt1, Pt2:
  TPoint2D): TClipResult;
var
  DX, DY, tE, TL: Extended;
begin
  Pt1 := CartesianPoint2D(Pt1);
  Pt2 := CartesianPoint2D(Pt2);
  DX := Pt2.X - Pt1.X;
  DY := Pt2.Y - Pt1.Y;
  Result := [ccNotVisible];
  if (DX = 0) and (DY = 0) and IsPointInCartesianBox2D(Pt1, Clip)
    then
  begin
    Result := [ccVisible];
    Exit;
  end;
  tE := 0.0;
  TL := 1.0;
  // 0.9 in 1.
  if _ClipPt(DX, Clip.Left - Pt1.X, tE, TL) then
    if _ClipPt(-DX, Pt1.X - Clip.Right, tE, TL) then
      if _ClipPt(DY, Clip.Bottom - Pt1.Y, tE, TL) then
        if _ClipPt(-DY, Pt1.Y - Clip.Top, tE, TL) then
        begin
          Result := [];
          if TL < 1 then
          begin
            Pt2.X := Pt1.X + TL * DX;
            Pt2.Y := Pt1.Y + TL * DY;
            Result := [ccSecond];
          end;
          if tE > 0 then
          begin
            Pt1.X := Pt1.X + tE * DX;
            Pt1.Y := Pt1.Y + tE * DY;
            Result := Result + [ccFirst];
          end;
          if Result = [] then
            Result := [ccVisible];
        end;
end;

{ Implement the Liang-Barsky algoritm. }

function _ClipLineLeftRight2D(const Clip: TRect2D; var Pt1, Pt2:
  TPoint2D): TClipResult;
var
  DX, DY, tE, TL: Extended;
begin
  Pt1 := CartesianPoint2D(Pt1);
  Pt2 := CartesianPoint2D(Pt2);
  DX := Pt2.X - Pt1.X;
  DY := Pt2.Y - Pt1.Y;
  Result := [ccNotVisible];
  if (DX = 0) and (DY = 0) and IsPointInCartesianBox2D(Pt1, Clip)
    then
  begin
    Result := [ccVisible];
    Exit;
  end;
  tE := 0.0;
  TL := 1.0;
  { 0.9 in 1. }
  if _ClipPt(DX, Clip.Left - Pt1.X, tE, TL) then
    if _ClipPt(-DX, Pt1.X - Clip.Right, tE, TL) then
    begin
      Result := [];
      if TL < 1 then
      begin
        Pt2.X := Pt1.X + TL * DX;
        Pt2.Y := Pt1.Y + TL * DY;
        Result := [ccSecond];
      end;
      if tE > 0 then
      begin
        Pt1.X := Pt1.X + tE * DX;
        Pt1.Y := Pt1.Y + tE * DY;
        Result := Result + [ccFirst];
      end;
      if Result = [] then
        Result := [ccVisible];
    end;
end;

{ Implement the Liang-Barsky algoritm. }

function _ClipLineUpBottom2D(const Clip: TRect2D; var Pt1, Pt2:
  TPoint2D): TClipResult;
var
  DX, DY, tE, TL: Extended;
begin
  Pt1 := CartesianPoint2D(Pt1);
  Pt2 := CartesianPoint2D(Pt2);
  DX := Pt2.X - Pt1.X;
  DY := Pt2.Y - Pt1.Y;
  Result := [ccNotVisible];
  if (DX = 0) and (DY = 0) and IsPointInCartesianBox2D(Pt1, Clip)
    then
  begin
    Result := [ccVisible];
    Exit;
  end;
  tE := 0.0;
  TL := 1.0;
  { 0.9 in 1. }
  if _ClipPt(DY, Clip.Bottom - Pt1.Y, tE, TL) then
    if _ClipPt(-DY, Pt1.Y - Clip.Top, tE, TL) then
    begin
      Result := [];
      if TL < 1 then
      begin
        Pt2.X := Pt1.X + TL * DX;
        Pt2.Y := Pt1.Y + TL * DY;
        Result := [ccSecond];
      end;
      if tE > 0 then
      begin
        Pt1.X := Pt1.X + tE * DX;
        Pt1.Y := Pt1.Y + tE * DY;
        Result := Result + [ccFirst];
      end;
      if Result = [] then
        Result := [ccVisible];
    end;
end;

{ General functions }

function DegToRad(const A: TRealType): TRealType;
begin
  Result := A * Pi / 180.0;
end;

function RadToDeg(const A: TRealType): TRealType;
begin
  Result := A * 180.0 / Pi;
end;

{ 2D functions }

function IsSamePoint2D(P1, P2: TPoint2D): Boolean;
begin
  if (P1.W <> P2.W) then
  begin
    P1 := CartesianPoint2D(P1);
    P2 := CartesianPoint2D(P2);
  end;
  Result := (P1.X = P2.X) and (P1.Y = P2.Y);
end;

function IsSameVector2D(const V1, V2: TVector2D): Boolean;
begin
  Result := (V1.X = V2.X) and (V1.Y = V2.Y);
end;

function IsSameTransform2D(const T1, T2: TTransf2D): Boolean;
begin
  Result := (T1[1, 1] = T2[1, 1])
    and (T1[1, 2] = T2[1, 2])
    and (T1[1, 3] = T2[1, 3])
    and (T1[2, 1] = T2[2, 1])
    and (T1[2, 2] = T2[2, 2])
    and (T1[2, 3] = T2[2, 3])
    and (T1[3, 1] = T2[3, 1])
    and (T1[3, 2] = T2[3, 2])
    and (T1[3, 3] = T2[3, 3]);
end;

function IsCartesianTransform2D(const T: TTransf2D): Boolean;
begin
  Result := (T[1, 3] = 0.0) and (T[2, 3] = 0.0) and (T[3, 3] =
    1.0);
end;

function Point2D(const X, Y: TRealType): TPoint2D;
begin
  Result.X := X;
  Result.Y := Y;
  Result.W := 1.0;
end;

function VectorLength2D(const V: TVector2D): TRealType;
begin
  Result := Hypot(V.X, V.Y); //Sqrt(V.X * V.X + V.Y * V.Y);
end;

function VectorAdd2D(const V1, V2: TVector2D): TVector2D;
begin
  Result.X := V1.X + V2.X;
  Result.Y := V1.Y + V2.Y;
end;

function VectorSubtract2D(const V1, V2: TVector2D): TVector2D;
begin
  Result.X := V1.X - V2.X;
  Result.Y := V1.Y - V2.Y;
end;

function VectorAngle2(const X, Y: TRealType): TRealType;
begin
  Result := ArcTan2(Y, X);
  if Result < 0 then Result := Result + 2 * Pi;
end;

function VectorAngle(const V: TVector2D): TRealType;
begin
  Result := VectorAngle2(V.X, V.Y);
end;

function TwoPointsAngle(P0, P1: TPoint2D): TRealType;
begin
  P0 := CartesianPoint2D(P0);
  P1 := CartesianPoint2D(P1);
  Result := VectorAngle2(P1.X - P0.X, P1.Y - P0.Y);
  //if (Result < 1E-9) or (Result > 2 * Pi - 1E-9) then    Result := 0;
end;

function TwoVectorsAngle2(X1, Y1, X2, Y2: TRealType): TRealType;
var
  A1, A2: TRealType;
begin
  A1 := ArcTan2(Y1, X1);
  A2 := ArcTan2(Y2, X2);
  Result := A2 - A1;
  if Result < 0 then Result := Result + 2 * Pi;
end;

function PolarVector(const Radius, Angle: TRealType): TVector2D;
begin
  Result.X := Cos(Angle) * Radius;
  Result.Y := Sin(Angle) * Radius;
end;

function ShiftPoint(const P: TPoint2D; const V: TVector2D):
  TPoint2D;
begin
  Result := Point2D(P.X + V.X, P.Y + V.Y);
end;

function ShiftPointScale(const P: TPoint2D; const V: TVector2D;
  const S: TRealType): TPoint2D;
begin
  Result := Point2D(P.X + V.X * S, P.Y + V.Y * S);
end;

function V2D(const X, Y: TRealType): TVector2D;
begin
  Result.X := X;
  Result.Y := Y;
end;

function Versor2D(const X, Y: TRealType): TVector2D;
begin
  Result.X := X;
  Result.Y := Y;
  Result := NormalizeVector2D(Result);
end;

function ScaleVector2D(const V: TVector2D;
  const S: TRealType): TVector2D;
begin
  Result.X := V.X * S;
  Result.Y := V.Y * S;
end;

function NormalizeVector2D(const V: TVector2D): TVector2D;
var
  Modul: TRealType;
begin
  Modul := VectorLength2D(V);
  Result := V;
  if (Modul <> 1.0) and (Modul <> 0.0) then
  begin
    Modul := 1.0 / Modul;
    Result.X := V.X * Modul;
    Result.Y := V.Y * Modul;
  end;
end;

function Vector2D(PFrom, PTo: TPoint2D): TVector2D;
begin
  if PFrom.W <> PTo.W then
  begin
    PFrom := CartesianPoint2D(PFrom);
    PTo := CartesianPoint2D(PTo);
  end;
  Result.X := PTo.X - PFrom.X;
  Result.Y := PTo.Y - PFrom.Y;
end;

function Direction2D(const PFrom, PTo: TPoint2D): TVector2D;
begin
  Result := NormalizeVector2D(Vector2D(PFrom, PTo));
end;

function ReorderRect2D(const R: TRect2D): TRect2D;
begin
  Result := CartesianRect2D(R);
  if R.Left > R.Right then
  begin
    Result.Left := R.Right;
    Result.Right := R.Left;
  end;
  if R.Bottom > R.Top then
  begin
    Result.Bottom := R.Top;
    Result.Top := R.Bottom;
  end;
end;

function Rect2D(const Left, Bottom, Right, Top: TRealType):
  TRect2D;
begin
  Result.Left := Left;
  Result.Right := Right;
  Result.W1 := 1.0;
  Result.Bottom := Bottom;
  Result.Top := Top;
  Result.W2 := 1.0;
end;

function CartesianPoint2D(const P: TPoint2D): TPoint2D;
begin
  if (P.W <> 1.0) and (P.W <> 0.0) then
  begin
    Result.X := P.X / P.W;
    Result.Y := P.Y / P.W;
    Result.W := 1.0;
  end
  else
    Result := P;
end;

function CartesianRect2D(const R: TRect2D): TRect2D;
begin
  Result.FirstEdge := CartesianPoint2D(R.FirstEdge);
  Result.SecondEdge := CartesianPoint2D(R.SecondEdge);
end;

function Point2DToPoint(const P2D: TPoint2D): TPoint;
begin
  if (P2D.W <> 1.0) and (P2D.W <> 0.0) then
  begin
    Result.X := Round(P2D.X / P2D.W);
    Result.Y := Round(P2D.Y / P2D.W);
  end
  else
  begin
    Result.X := Round(P2D.X);
    Result.Y := Round(P2D.Y);
  end
end;

function PointToPoint2D(const P: TPoint): TPoint2D;
begin
  Result.X := P.X;
  Result.Y := P.Y;
  Result.W := 1.0;
end;

function RectToRect2D(const R: TRect): TRect2D;
begin
  Result := Rect2D(R.Left, R.Top, R.Right, R.Bottom);
end;

function Rect2DToRect(R: TRect2D): TRect;
begin
  R := ReorderRect2D(R);
  Result.Left := Round(R.Left);
  Result.Right := Round(R.Right);
  Result.Top := Round(R.Bottom);
  Result.Bottom := Round(R.Top);
end;

function BoxCenter(const R: TRect2D): TPoint2D;
begin
  Result := MidPoint(R.FirstEdge, R.SecondEdge);
end;

function BoxBottomRight(const R: TRect2D): TPoint2D;
begin
  Result := Point2D(R.Right, R.Bottom);
end;

function BoxTopLeft(const R: TRect2D): TPoint2D;
begin
  Result := Point2D(R.Left, R.Top);
end;

function EnlargeBoxDelta2D(R: TRect2D; const Delta: TRealType):
  TRect2D;
begin
  R := CartesianRect2D(R);
  Result.Left := R.Left - Delta;
  Result.Right := R.Right + Delta;
  Result.W1 := 1.0;
  Result.Bottom := R.Bottom - Delta;
  Result.Top := R.Top + Delta;
  Result.W2 := 1.0;
end;

function EnlargeBoxPerc2D(R: TRect2D; const Perc: TRealType):
  TRect2D;
var
  Marg: TRealType;
begin
  R := CartesianRect2D(R);
  Marg := Abs(R.Right - R.Left) * Perc;
  Result.Left := R.Left - Marg;
  Result.Right := R.Right + Marg;
  Marg := Abs(R.Top - R.Bottom) * Perc;
  Result.Bottom := R.Bottom - Marg;
  Result.Top := R.Top + Marg;
  Result.W1 := 1.0;
  Result.W2 := 1.0;
end;

function GetVisualTransform2D(var W: TRect2D; const V: TRect;
  Aspect: TRealType): TTransf2D;
var
  TmpAsp: TRealType;
begin
  {
    | Sx                     0                          0 |
    | 0                      Sy                         0 |
    | V.Left-(Sx*W.Left)+0.5 V.Bottom-(Sy*W.Bottom)+0.5 1 |

    Sx=(V.Right-V.Left-1.0)/(W.Right-W.Left)
    Sy=(V.Top-V.Bottom-1.0)/(W.Top-W.Bottom)
    Se pero' AspectRatio=Sx/Sy > 0 allora Sy=Sx/AspectRatio.
  }
  try
    if (Aspect > 0.0) and (V.Bottom > V.Top) and (V.Right > V.Left)
      then
    begin
      TmpAsp :=
        (V.Right - V.Left) / (V.Bottom - V.Top) * Aspect;
      if (W.Top - W.Bottom) > (W.Right - W.Left) / TmpAsp then
        W.Right := W.Left + (W.Top - W.Bottom) * TmpAsp
      else
        W.Top := W.Bottom + (W.Right - W.Left) / TmpAsp;
    end;
    if W.Right <> W.Left then
      Result[1, 1] := (V.Right - V.Left) / (W.Right - W.Left);
    Result[1, 2] := 0.0;
    Result[1, 3] := 0.0;
    Result[2, 1] := 0.0;
    if W.Top > W.Bottom then
      Result[2, 2] := (V.Top - V.Bottom) / (W.Top - W.Bottom);
    Result[2, 3] := 0.0;
    Result[3, 1] := V.Left - Result[1, 1] * W.Left - 0.5;
    Result[3, 2] := V.Bottom - Result[2, 2] * W.Bottom - 0.5;
    Result[3, 3] := 1.0;
  except
    on EZeroDivide do Result := IdentityTransf2D;
  end;
end;

function MultiplyTransform2D(const M1, M2: TTransf2D):
  TTransf2D;
var
  B1, B2: Boolean;
begin
  B1 := (M1[1, 3] = 0.0) and (M1[2, 3] = 0.0) and (M1[3, 3] =
    1.0);
  B2 := (M2[1, 3] = 0.0) and (M2[2, 3] = 0.0) and (M2[3, 3] =
    1.0);
  if B1 and B2 then
  begin
    Result[1, 1] := M1[1, 1] * M2[1, 1] + M1[1, 2] * M2[2, 1];
    Result[1, 2] := M1[1, 1] * M2[1, 2] + M1[1, 2] * M2[2, 2];
    Result[1, 3] := 0.0;

    Result[2, 1] := M1[2, 1] * M2[1, 1] + M1[2, 2] * M2[2, 1];
    Result[2, 2] := M1[2, 1] * M2[1, 2] + M1[2, 2] * M2[2, 2];
    Result[2, 3] := 0.0;

    Result[3, 1] := M1[3, 1] * M2[1, 1] + M1[3, 2] * M2[2, 1] +
      M2[3, 1];
    Result[3, 2] := M1[3, 1] * M2[1, 2] + M1[3, 2] * M2[2, 2] +
      M2[3, 2];
    Result[3, 3] := 1.0;
  end
  else if B1 and (not B2) then
  begin
    Result[1, 1] := M1[1, 1] * M2[1, 1] + M1[1, 2] * M2[2, 1];
    Result[1, 2] := M1[1, 1] * M2[1, 2] + M1[1, 2] * M2[2, 2];
    Result[1, 3] := M1[1, 1] * M2[1, 3] + M1[1, 2] * M2[2, 3];

    Result[2, 1] := M1[2, 1] * M2[1, 1] + M1[2, 2] * M2[2, 1];
    Result[2, 2] := M1[2, 1] * M2[1, 2] + M1[2, 2] * M2[2, 2];
    Result[2, 3] := M1[2, 1] * M2[1, 3] + M1[2, 2] * M2[2, 3];

    Result[3, 1] := M1[3, 1] * M2[1, 1] + M1[3, 2] * M2[2, 1] +
      M2[3, 1];
    Result[3, 2] := M1[3, 1] * M2[1, 2] + M1[3, 2] * M2[2, 2] +
      M2[3, 2];
    Result[3, 3] := M1[3, 1] * M2[1, 3] + M1[3, 2] * M2[2, 3] +
      M2[3, 3];
  end
  else if (not B1) and B2 then
  begin
    Result[1, 1] := M1[1, 1] * M2[1, 1] + M1[1, 2] * M2[2, 1] +
      M1[1, 3] * M2[3, 1];
    Result[1, 2] := M1[1, 1] * M2[1, 2] + M1[1, 2] * M2[2, 2] +
      M1[1, 3] * M2[3, 2];
    Result[1, 3] := M1[1, 3];

    Result[2, 1] := M1[2, 1] * M2[1, 1] + M1[2, 2] * M2[2, 1] +
      M1[2, 3] * M2[3, 1];
    Result[2, 2] := M1[2, 1] * M2[1, 2] + M1[2, 2] * M2[2, 2] +
      M1[2, 3] * M2[3, 2];
    Result[2, 3] := M1[2, 3];

    Result[3, 1] := M1[3, 1] * M2[1, 1] + M1[3, 2] * M2[2, 1] +
      M1[3, 3] * M2[3, 1];
    Result[3, 2] := M1[3, 1] * M2[1, 2] + M1[3, 2] * M2[2, 2] +
      M1[3, 3] * M2[3, 2];
    Result[3, 3] := M1[3, 3];
  end
  else
  begin
    Result[1, 1] := M1[1, 1] * M2[1, 1] + M1[1, 2] * M2[2, 1] +
      M1[1, 3] * M2[3, 1];
    Result[1, 2] := M1[1, 1] * M2[1, 2] + M1[1, 2] * M2[2, 2] +
      M1[1, 3] * M2[3, 2];
    Result[1, 3] := M1[1, 1] * M2[1, 3] + M1[1, 2] * M2[2, 3] +
      M1[1, 3] * M2[3, 3];

    Result[2, 1] := M1[2, 1] * M2[1, 1] + M1[2, 2] * M2[2, 1] +
      M1[2, 3] * M2[3, 1];
    Result[2, 2] := M1[2, 1] * M2[1, 2] + M1[2, 2] * M2[2, 2] +
      M1[2, 3] * M2[3, 2];
    Result[2, 3] := M1[2, 1] * M2[1, 3] + M1[2, 2] * M2[2, 3] +
      M1[2, 3] * M2[3, 3];

    Result[3, 1] := M1[3, 1] * M2[1, 1] + M1[3, 2] * M2[2, 1] +
      M1[3, 3] * M2[3, 1];
    Result[3, 2] := M1[3, 1] * M2[1, 2] + M1[3, 2] * M2[2, 2] +
      M1[3, 3] * M2[3, 2];
    Result[3, 3] := M1[3, 1] * M2[1, 3] + M1[3, 2] * M2[2, 3] +
      M1[3, 3] * M2[3, 3];
  end;
end;

{
  a11 a12
  a21 a22
}

function _Det2(a11, a21, a12, a22: TRealType): TRealType;
begin
  Result := a11 * a22 - a12 * a21;
end;

function _Det3_2D(const M: TTransf2D): TRealType;
begin
  Result := M[1, 1] * M[2, 2] * M[3, 3] +
    M[1, 3] * M[2, 1] * M[3, 2] +
    M[1, 2] * M[2, 3] * M[3, 1] -
    M[1, 3] * M[2, 2] * M[3, 1] -
    M[1, 1] * M[3, 2] * M[2, 3] -
    M[1, 2] * M[2, 1] * M[3, 3];
end;

function _Adjoint3(const M: TTransf2D): TTransf2D;
var
  A1, A2, A3, B1, B2, B3, C1, C2, C3: TRealType;
begin
  A1 := M[1, 1];
  A2 := M[2, 1];
  A3 := M[3, 1];
  B1 := M[1, 2];
  B2 := M[2, 2];
  B3 := M[3, 2];
  C1 := M[1, 3];
  C2 := M[2, 3];
  C3 := M[3, 3];

  Result[1, 1] := _Det2(B2, B3, C2, C3);
  Result[2, 1] := -_Det2(A2, A3, C2, C3);
  Result[3, 1] := _Det2(A2, A3, B2, B3);

  Result[1, 2] := -_Det2(B1, B3, C1, C3);
  Result[2, 2] := _Det2(A1, A3, C1, C3);
  Result[3, 2] := -_Det2(A1, A3, B1, B3);

  Result[1, 3] := _Det2(B1, B2, C1, C2);
  Result[2, 3] := -_Det2(A1, A2, C1, C2);
  Result[3, 3] := _Det2(A1, A2, B1, B2);
end;

function InvertTransform2D(const M1: TTransf2D): TTransf2D;
var
  Divisor: TRealType;
begin
  if (M1[1, 3] = 0.0) and (M1[2, 3] = 0.0) and (M1[3, 3] = 1.0)
    then
  begin // Cartesian
    if (M1[1, 1] * M1[2, 2] - M1[2, 1] * M1[1, 2]) = 0.0 then
    begin
      Result := NullTransf2D;
      Exit;
    end;
    Divisor := 1.0 / (M1[1, 1] * M1[2, 2] - M1[2, 1] * M1[1,
      2]);
    Result[1, 1] := M1[2, 2] * Divisor;
    Result[1, 2] := -M1[1, 2] * Divisor;
    Result[1, 3] := 0.0;
    Result[2, 1] := -M1[2, 1] * Divisor;
    Result[2, 2] := M1[1, 1] * Divisor;
    Result[2, 3] := 0.0;
    Result[3, 1] := (M1[2, 1] * M1[3, 2] - M1[2, 2] * M1[3, 1])
      * Divisor;
    Result[3, 2] := (M1[1, 2] * M1[3, 1] - M1[1, 1] * M1[3, 2])
      * Divisor;
    Result[3, 3] := 1.0;
  end
  else
  begin
    Divisor := _Det3_2D(M1);
    if Divisor = 0 then
    begin
      Result := NullTransf2D;
      Exit;
    end;
    Result := _Adjoint3(M1);

    Result[1, 1] := Result[1, 1] / Divisor;
    Result[1, 2] := Result[1, 2] / Divisor;
    Result[1, 3] := Result[1, 3] / Divisor;
    Result[2, 1] := Result[2, 1] / Divisor;
    Result[2, 2] := Result[2, 2] / Divisor;
    Result[2, 3] := Result[2, 3] / Divisor;
    Result[3, 1] := Result[3, 1] / Divisor;
    Result[3, 2] := Result[3, 2] / Divisor;
    Result[3, 3] := Result[3, 3] / Divisor;
  end;
end;

function TransformPoint2D(const P: TPoint2D; const T:
  TTransf2D): TPoint2D;
begin
  Result.X := P.X * T[1, 1] + P.Y * T[2, 1] + P.W * T[3, 1];
  Result.Y := P.X * T[1, 2] + P.Y * T[2, 2] + P.W * T[3, 2];
  Result.W := P.X * T[1, 3] + P.Y * T[2, 3] + P.W * T[3, 3];
end;

function TransformVector2D(const V: TVector2D; const T:
  TTransf2D): TVector2D;
begin
  Result.X := V.X * T[1, 1] + V.Y * T[2, 1];
  Result.Y := V.X * T[1, 2] + V.Y * T[2, 2];
end;

function TransformRect2D(const R: TRect2D; const T: TTransf2D):
  TRect2D;
begin
  Result.FirstEdge := TransformPoint2D(R.FirstEdge, T);
  Result.SecondEdge := TransformPoint2D(R.SecondEdge, T);
end;

function TransformBoundingBox2D(const Box: TRect2D; const
  Transf: TTransf2D): TRect2D;
var
  Box1, Box2: TRect2D;
begin
  Box1 := TransformRect2D(Box, Transf);
  Box2 := TransformRect2D(Rect2D(Box.Left, Box.Top, Box.Right,
    Box.Bottom), Transf);
  Result := BoxOutBox2D(Box1, Box2);
end;

function Transform2D(const A, B, C, D, E, F: TRealType): TTransf2D;
begin
  { | A  B  0 |
    | C  D  0 |
    | E  F  1 |  }
  Result[1, 1] := A;
  Result[1, 2] := B;
  Result[1, 3] := 0;
  Result[2, 1] := C;
  Result[2, 2] := D;
  Result[2, 3] := 0;
  Result[3, 1] := E;
  Result[3, 2] := F;
  Result[3, 3] := 1;
end;

function Translate2D(const TX, TY: TRealType): TTransf2D;
begin
  {
    | 1  0  0 |
    | 0  1  0 |
    | Tx Ty 1 |
  }
  Result := IdentityTransf2D;
  Result[3, 1] := TX;
  Result[3, 2] := TY;
end;

function Rotate2D(const R: TRealType): TTransf2D;
begin
  {
    | cos(R)  sin(R)   0 |
    | -sin(R) cos(R)   0 |
    | 0       0        1 |
  }
  Result := IdentityTransf2D;
  Result[1, 1] := Cos(R);
  Result[1, 2] := Sin(R);
  Result[2, 1] := -Result[1, 2];
  Result[2, 2] := Result[1, 1];
end;

//TSY: rotate over point (CX,CY), R radians

function RotateCenter2D(const R: TRealType;
  const C: TPoint2D): TTransf2D;
var
  CosR, SinR: TRealType;
begin
  {
    | cosR                 sinR                 0 |
    | -sinR                cosR                 0 |
    | CX*(1-cosR)+CY*sinR  CY*(1-cosR)-CX*sinR  1 |
  }
  Result := IdentityTransf2D;
  CosR := Cos(R);
  SinR := Sin(R);
  Result[1, 1] := CosR;
  Result[1, 2] := SinR;
  Result[2, 1] := -SinR;
  Result[2, 2] := CosR;
  Result[3, 1] := C.X * (1 - CosR) + C.Y * SinR;
  Result[3, 2] := C.Y * (1 - CosR) - C.X * SinR;
end;

function Scale2D(const Sx, Sy: TRealType): TTransf2D;
begin
  {
    | Sx  0  0 |
    | 0   Sy 0 |
    | 0   0  1 |
  }
  Result := IdentityTransf2D;
  Result[1, 1] := Sx;
  Result[2, 2] := Sy;
end;

//TSY: scale with central point (CX,CY)

function ScaleCenter2D(const Sx, Sy: TRealType;
  const C: TPoint2D): TTransf2D;
begin
  {
    | Sx       0         0 |
    | 0        Sy        0 |
    | CX(1-Sx) CY(1-Sy)  1 |
  }
  Result := IdentityTransf2D;
  Result[1, 1] := Sx;
  Result[2, 2] := Sy;
  Result[3, 1] := C.X * (1 - Sx);
  Result[3, 2] := C.Y * (1 - Sy);
end;

//TSY: flip over a line
// The line is given by a (cartesian) point on it and a normal
// (an ortogonal vector)

function Flip2D(const P: TPoint2D; const V: TVector2D): TTransf2D;
var
  V2, A, B, C: TRealType;
begin
  {      V2 = VX^2+VY^2
  A = 2 * VX^2 / V2, B = 2 * VX*VY / V2, C = 2 * VY^2 / V2
    | 1-A        -B         0 |
    | -B         1-C        0 |
    | A*PX+B*PY  B*PX+C*PY  1 |
  }
  Result := IdentityTransf2D;
  V2 := Sqr(V.X) + Sqr(V.Y);
  if V2 = 0 then Exit;
  A := 2 * Sqr(V.X) / V2;
  B := 2 * V.X * V.Y / V2;
  C := 2 * Sqr(V.Y) / V2;
  Result[1, 1] := 1 - A;
  Result[2, 1] := B;
  Result[3, 1] := A * P.X + B * P.Y;
  Result[1, 2] := -B;
  Result[2, 2] := 1 - C;
  Result[3, 2] := B * P.X + C * P.Y;
end;

function Skew2D(const AH, AV: TRealType;
  const C: TPoint2D): TTransf2D;
var
  CotanH, CotanV: TRealType;
begin
  {
    | 1           CotanV      0 |
    | CotanH      1           0 |
    | -CX*CotanH  -CY*CotanV  1 |
  }
  Result := IdentityTransf2D;
  CotanH := Tan(AH);
  //if CotanH <> 0 then CotanH := 1 / CotanH;
  CotanV := Tan(AV);
  //if CotanV <> 0 then CotanV := 1 / CotanV;
  Result[1, 2] := CotanV;
  Result[2, 1] := CotanH;
  Result[3, 1] := -C.X * CotanH;
  Result[3, 2] := -C.Y * CotanV;
end;


function IsBoxInBox2D(Box1, Box2: TRect2D): Boolean;
var
  FCode, SCode: TOutCode;
begin
  Box1 := ReorderRect2D(Box1);
  Box2 := ReorderRect2D(Box2);
  FCode := _PositionCode2D(Box2, Box1.FirstEdge);
  SCode := _PositionCode2D(Box2, Box1.SecondEdge);
  Result := (FCode * SCode) = [];
end;

function IsPointInCartesianBox2D(const P: TPoint2D; const Box:
  TRect2D): Boolean;
begin
  Result := _PositionCode2D(Box, P) = [];
end;

function IsBoxAllInBox2D(Box1, Box2: TRect2D): Boolean;
var
  FCode, SCode: TOutCode;
begin
  Box1 := CartesianRect2D(Box1);
  Box2 := CartesianRect2D(Box2);
  FCode := _PositionCode2D(Box2, Box1.FirstEdge);
  SCode := _PositionCode2D(Box2, Box1.SecondEdge);
  Result := (FCode = []) and (SCode = []);
end;

function IsBoxAllInCartesianBox2D(const Box1, Box2: TRect2D):
  Boolean;
var
  FCode, SCode: TOutCode;
begin
  FCode := _PositionCode2D(Box2, Box1.FirstEdge);
  SCode := _PositionCode2D(Box2, Box1.SecondEdge);
  Result := (FCode = []) and (SCode = []);
end;

function BoxOutBox2D0(const Box1, Box2: TRect2D): TRect2D;
begin
  Result := Box1;
  if Box2.Left < Box1.Left then
    Result.Left := Box2.Left;
  if Box2.Right > Box1.Right then
    Result.Right := Box2.Right;
  if Box2.Bottom < Box1.Bottom then
    Result.Bottom := Box2.Bottom;
  if Box2.Top > Box1.Top then
    Result.Top := Box2.Top;
end;

function BoxOutBox2D(Box1, Box2: TRect2D): TRect2D;
begin
  Box1 := ReorderRect2D(Box1);
  Box2 := ReorderRect2D(Box2);
  Result := BoxOutBox2D0(Box1, Box2);
end;

procedure MakeOrto2D(LastPt: TPoint2D; var CurrPt: TPoint2D);
var
  DeltaPt: TVector2D;
begin
  LastPt := CartesianPoint2D(LastPt);
  CurrPt := CartesianPoint2D(CurrPt);
  DeltaPt := Vector2D(LastPt, CurrPt);
  if Abs(DeltaPt.X) > Abs(DeltaPt.Y) then
    CurrPt.Y := LastPt.Y
  else
    CurrPt.X := LastPt.X;
end;

procedure Make45_2D_V(var V: TVector2D);
var
  D: TVector2D;
  L, LMax: TRealType;
  I: Integer;
const
  DD: array[0..7] of TVector2D = (
    (X: 1; Y: 0), (X: ISqrt2; Y: ISqrt2),
    (X: 0; Y: 1), (X: - ISqrt2; Y: ISqrt2),
    (X: - 1; Y: 0), (X: - ISqrt2; Y: - ISqrt2),
    (X: 0; Y: - 1), (X: ISqrt2; Y: - ISqrt2));
begin
  LMax := V.X;
  D := DD[0];
  for I := 1 to 7 do
  begin
    L := DotProduct2D(DD[I], V);
    if L > LMax then
    begin
      LMax := L;
      D := DD[I];
    end;
  end;
  V := ScaleVector2D(D, LMax);
end;

procedure Make45_2D(LastPt: TPoint2D; var CurrPt: TPoint2D);
var
  DeltaPt: TVector2D;
begin
  LastPt := CartesianPoint2D(LastPt);
  CurrPt := CartesianPoint2D(CurrPt);
  DeltaPt := Vector2D(LastPt, CurrPt);
  Make45_2D_V(DeltaPt);
  CurrPt := ShiftPoint(LastPt, DeltaPt);
end;

function DotProduct2D(const A, B: TVector2D): TRealType;
begin
  Result := A.X * B.X + A.Y * B.Y;
end;

function Perpendicular2D(const V: TVector2D): TVector2D;
begin
  Result.X := -V.Y;
  Result.Y := V.X;
end;

function Reflect2D(const V: TVector2D): TVector2D;
begin
  Result.X := -V.X;
  Result.Y := -V.Y;
end;

function MixPoint(const P1, P2: TPoint2D; Mix: TRealType):
  TPoint2D;
begin
  Result := Point2D(P1.X + (P2.X - P1.X) * Mix,
    P1.Y + (P2.Y - P1.Y) * Mix);
end;

function MidPoint(const P1, P2: TPoint2D): TPoint2D;
begin
  Result := Point2D((P1.X + P2.X) / 2,
    (P1.Y + P2.Y) / 2);
end;

function IsotropicScale(const T: TTransf2D): TRealType;
begin
  Result := Sqrt((Sqr(T[1, 1]) + Sqr(T[2, 1]) +
    Sqr(T[2, 2]) + Sqr(T[1, 2])) / 2);
end;

function PointDistance2D(const P1, P2: TPoint2D): TRealType;
begin
  Result := VectorLength2D(Vector2D(P1, P2));
end;

function PointSegmentProj2D(const P, P0, P1: TPoint2D): TPoint2D;
var
  V, W: TVector2D;
  VW, VV: TRealType;
begin
  V := Vector2D(P0, P1);
  W := Vector2D(P0, P);
  VW := DotProduct2D(V, W);
  if VW <= 0 then
  begin
    Result := P0;
    Exit;
  end;
  VV := DotProduct2D(V, V);
  if VV <= VW then
    Result := P1
  else
    Result := ShiftPointScale(P0, V, VW / VV);
end;

function PointSegmentDistance2D(const P, P1, P2: TPoint2D):
  TRealType;
begin
  Result := PointDistance2D(P, PointSegmentProj2D(P, P1, P2))
end;

function IsPointOnRect2D(const Box: TRect2D; P: TPoint2D; var
  Dist: TRealType;
  const Aperture: TRealType; const T: TTransf2D): Integer;
var
  TmpDist: TRealType;
  TmpPt1, TmpPt2: TPoint2D;
begin
  Dist := MaxRealType;
  Result := PICK_NOOBJECT;
  P := CartesianPoint2D(P);
  TmpPt1 := CartesianPoint2D(TransformPoint2D(Box.FirstEdge,
    T));
  TmpPt2 := CartesianPoint2D(TransformPoint2D(Point2D(Box.Left,
    Box.Top), T));
  TmpDist := PointSegmentDistance2D(P, TmpPt1, TmpPt2);
  if TmpDist <= Aperture then
  begin
    Result := PICK_ONOBJECT;
    Dist := TmpDist;
    Exit;
  end;
  TmpPt1 := TmpPt2;
  TmpPt2 := CartesianPoint2D(TransformPoint2D(
    Box.SecondEdge, T));
  TmpDist := PointSegmentDistance2D(P, TmpPt1, TmpPt2);
  if TmpDist <= Aperture then
  begin
    Result := PICK_ONOBJECT;
    Dist := TmpDist;
    Exit;
  end;
  TmpPt1 := TmpPt2;
  TmpPt2 := CartesianPoint2D(TransformPoint2D(
    Point2D(Box.Right, Box.Bottom), T));
  TmpDist := PointSegmentDistance2D(P, TmpPt1, TmpPt2);
  if TmpDist <= Aperture then
  begin
    Result := PICK_ONOBJECT;
    Dist := TmpDist;
    Exit;
  end;
  TmpPt1 := TmpPt2;
  TmpPt2 := CartesianPoint2D(TransformPoint2D(
    Box.FirstEdge, T));
  TmpDist := PointSegmentDistance2D(P, TmpPt1, TmpPt2);
  if TmpDist <= Aperture then
  begin
    Result := PICK_ONOBJECT;
    Dist := TmpDist;
    Exit;
  end;
end;

function PointOutBox2D(P: TPoint2D; Box: TRect2D): TRect2D;
begin
  P := CartesianPoint2D(P);
  Result := Box;
  if P.X > Result.Right then
    Result.Right := P.X;
  if P.X < Result.Left then
    Result.Left := P.X;
  if P.Y > Result.Top then
    Result.Top := P.Y;
  if P.Y < Result.Bottom then
    Result.Bottom := P.Y;
end;

function NearPoint2D(RP, P: TPoint2D; const Aperture: TRealType;
  var Dist: TRealType): Boolean;
var
  TmpBox: TRect2D;
begin
  RP := CartesianPoint2D(RP);
  P := CartesianPoint2D(P);
  TmpBox.FirstEdge := Point2D(RP.X - Aperture, RP.Y - Aperture);
  TmpBox.SecondEdge := Point2D(RP.X + Aperture, RP.Y +
    Aperture);
  Result := _PositionCode2D(TmpBox, P) = [];
  if Result then
    Dist := Sqrt(Power(P.X - RP.X, 2) + Power(P.Y - RP.Y, 2))
  else
    Dist := MaxRealType;
end;

{ 2D clipping functions. }

{ Use the Liang-Barsky algoritm. }

function ClipLine2D(Clip: TRect2D; var Pt1, Pt2: TPoint2D):
  TClipResult;
var
  DX, DY, tE, TL: Extended;
begin
  Result := [ccNotVisible];
  Clip := ReorderRect2D(Clip);
  Pt1 := CartesianPoint2D(Pt1);
  Pt2 := CartesianPoint2D(Pt2);
  DX := Pt2.X - Pt1.X;
  DY := Pt2.Y - Pt1.Y;
  if (DX = 0) and (DY = 0) and IsPointInCartesianBox2D(Pt1, Clip)
    then
  begin
    Result := [ccVisible];
    Exit;
  end;
  tE := 0.0;
  TL := 1.0;
  { 0.9 in 1. }
  if _ClipPt(DX, Clip.Left - Pt1.X, tE, TL) then
    if _ClipPt(-DX, Pt1.X - Clip.Right, tE, TL) then
      if _ClipPt(DY, Clip.Bottom - Pt1.Y, tE, TL) then
        if _ClipPt(-DY, Pt1.Y - Clip.Top, tE, TL) then
        begin
          Result := [];
          if TL < 1 then
          begin
            Pt2.X := Pt1.X + TL * DX;
            Pt2.Y := Pt1.Y + TL * DY;
            Result := [ccSecond];
          end;
          if tE > 0 then
          begin
            Pt1.X := Pt1.X + tE * DX;
            Pt1.Y := Pt1.Y + tE * DY;
            Result := Result + [ccFirst];
          end;
          if Result = [] then
            Result := [ccVisible];
        end;
end;

{ Use the Liang-Barsky algoritm. }

function ClipLineLeftRight2D(Clip: TRect2D; var Pt1, Pt2:
  TPoint2D): TClipResult;
var
  DX, DY, tE, TL: Extended;
begin
  Clip := ReorderRect2D(Clip);
  Pt1 := CartesianPoint2D(Pt1);
  Pt2 := CartesianPoint2D(Pt2);
  DX := Pt2.X - Pt1.X;
  DY := Pt2.Y - Pt1.Y;
  Result := [ccNotVisible];
  if (DX = 0) and (DY = 0) and IsPointInCartesianBox2D(Pt1, Clip)
    then
  begin
    Result := [ccVisible];
    Exit;
  end;
  tE := 0.0;
  TL := 1.0;
  { 0.9 in 1. }
  if _ClipPt(DX, Clip.Left - Pt1.X, tE, TL) then
    if _ClipPt(-DX, Pt1.X - Clip.Right, tE, TL) then
    begin
      Result := [];
      if TL < 1 then
      begin
        Pt2.X := Pt1.X + TL * DX;
        Pt2.Y := Pt1.Y + TL * DY;
        Result := [ccSecond];
      end;
      if tE > 0 then
      begin
        Pt1.X := Pt1.X + tE * DX;
        Pt1.Y := Pt1.Y + tE * DY;
        Result := Result + [ccFirst];
      end;
      if Result = [] then
        Result := [ccVisible];
    end;
end;

function ClipLineUpBottom2D(Clip: TRect2D; var Pt1, Pt2:
  TPoint2D): TClipResult;
var
  DX, DY, tE, TL: Extended;
begin
  Clip := ReorderRect2D(Clip);
  Pt1 := CartesianPoint2D(Pt1);
  Pt2 := CartesianPoint2D(Pt2);
  DX := Pt2.X - Pt1.X;
  DY := Pt2.Y - Pt1.Y;
  Result := [ccNotVisible];
  if (DX = 0) and (DY = 0) and IsPointInCartesianBox2D(Pt1, Clip)
    then
  begin
    Result := [ccVisible];
    Exit;
  end;
  tE := 0.0;
  TL := 1.0;
  { 0.9 in 1. }
  if _ClipPt(DY, Clip.Bottom - Pt1.Y, tE, TL) then
    if _ClipPt(-DY, Pt1.Y - Clip.Top, tE, TL) then
    begin
      Result := [];
      if TL < 1 then
      begin
        Pt2.X := Pt1.X + TL * DX;
        Pt2.Y := Pt1.Y + TL * DY;
        Result := [ccSecond];
      end;
      if tE > 0 then
      begin
        Pt1.X := Pt1.X + tE * DX;
        Pt1.Y := Pt1.Y + tE * DY;
        Result := Result + [ccFirst];
      end;
      if Result = [] then
        Result := [ccVisible];
    end;
end;

function PointPolylineDistance(
  const PP: TPointsSet2D; P: TPoint2D;
  const T: TTransf2D; const Closed: Boolean;
  out Pos: Integer): TRealType;
var
  D: TRealType;
  P0, P1: TPoint2D;
  I, I0: Integer;
  PVect: PVectPoints2D;
begin
  PVect := PP.PointsReference;
  Result := MaxRealType;
  Pos := -2;
  if PP.Count = 0 then
    Exit;
  P := CartesianPoint2D(P);
  if Closed then
  begin
    I0 := 0;
    P0 := CartesianPoint2D(TransformPoint2D(
      PVect^[PP.Count - 1], T));
  end
  else
  begin
    I0 := 1;
    P0 := CartesianPoint2D(TransformPoint2D(
      PVect^[0], T));
  end;
  for I := I0 to PP.Count - 1 do
  begin
    P1 := CartesianPoint2D(TransformPoint2D(
      PVect^[I], T));
    D := PointSegmentDistance2D(P, P0, P1);
    if D <= Result then
    begin
      Result := D;
      Pos := I - 1;
    end;
    P0 := P1;
  end;
  if Pos = -1 then Pos := PP.Count - 1;
end;

// Copyright 2001, softSurfer (www.softsurfer.com)
// This code may be freely used and modified for any purpose
// providing that this copyright notice is included with it.
// SoftSurfer makes no warranty for this code, and cannot be held
// liable for any real or imagined damage resulting from its use.
// Users of this code must verify correctness for their application.

// isLeft(): tests if a point is Left|On|Right of an infinite line.
//    Input:  three points P0, P1, and P
//    Return: >0 for P left of the line through P0 and P1
//            =0 for P on the line
//            <0 for P right of the line
//    See: the January 2001 Algorithm "Area of 2D and 3D Triangles and Polygons"

function _isLeft(const P, P0, P1: TPoint2D): TRealType;
begin
  Result := (P1.X - P0.X) * (P.Y - P0.Y)
    - (P.X - P0.X) * (P1.Y - P0.Y);
end;

function CrossingInc(const P, P0, P1: TPoint2D): Integer;
begin
  Result := 0;
  if (P0.Y <= P.Y) then // start y <= P.y
  begin
    if (P1.Y > P.Y) then // an upward crossing
      if (_isLeft(P, P0, P1) > 0) // P left of edge
        then Result := 1 // have a valid up intersect
  end
  else // start y > P.y (no test needed)
  begin
    if (P1.Y <= P.Y) then // a downward crossing
      if (_isLeft(P, P0, P1) < 0) // P right of edge
        then Result := -1; // have a valid down intersect
  end;
end;

function IsPointInPolygon(
  const PP: TPointsSet2D; P: TPoint2D;
  const T: TTransf2D; const WRule: TWinding): Boolean;
var
  I, CrossingNumber: Integer;
  P0, P1: TPoint2D;
  PVect: PVectPoints2D;
begin
  PVect := PP.PointsReference;
  Result := False;
  if PP.Count = 0 then Exit;
  CrossingNumber := 0;
  P := CartesianPoint2D(P);
  P0 := CartesianPoint2D(TransformPoint2D(
    PVect^[PP.Count - 1], T));
  for I := 0 to PP.Count - 1 do
  begin
    P1 := CartesianPoint2D(TransformPoint2D(
      PVect^[I], T));
    Inc(CrossingNumber, CrossingInc(P, P0, P1));
    P0 := P1;
  end;
  // non-zero winding rule or even/odd
  case WRule of
    wn_NonZero: if CrossingNumber <> 0 then Result := True;
    wn_EvenOdd: if Odd(CrossingNumber) then Result := True;
  end;
end;

function IsPointInCircular(
  const P, CP: TPoint2D; const R, SA, EA: TRealType;
  const Kind: TCircularKind): Boolean;
var
  SA_, EA_, A: TRealType;
begin
  Result := False;
  if PointDistance2D(P, CP) > R then Exit;
  if Kind = ci_Sector then
  begin
    SA_ := SA - Floor(SA / (2 * Pi)) * 2 * Pi;
    EA_ := EA - Floor(EA / (2 * Pi)) * 2 * Pi;
    if EA_ < SA_ then EA_ := EA_ + 2 * Pi;
    A := VectorAngle(Vector2D(CP, P));
    if ((SA_ <= A) and (A <= EA_)) or
      ((SA_ <= A + 2 * Pi) and (A + 2 * Pi <= EA_))
      then Result := True;
  end
  else // Kind = ci_Arc or Kind = ci_Segment
  begin
    if _isLeft(P,
      ShiftPoint(CP, PolarVector(R, EA)),
      ShiftPoint(CP, PolarVector(R, SA))) >= 0
      then Result := True;
  end;
end;

function IsPointOnCircular(
  const P, CP: TPoint2D; const R, SA, EA: TRealType;
  const Kind: TCircularKind;
  const Precision: TRealType; var Distance: TRealType): Boolean;
var
  SA_, EA_, A, D: TRealType;
  P1, P2: TPoint2D;
begin
  Result := False;
  D := Abs(PointDistance2D(P, CP) - R);
  if D <= Distance then
  begin
    SA_ := SA - Floor(SA / (2 * Pi)) * 2 * Pi;
    EA_ := EA - Floor(EA / (2 * Pi)) * 2 * Pi;
    if EA_ < SA_ then EA_ := EA_ + 2 * Pi;
    A := VectorAngle(Vector2D(CP, P));
    if ((SA_ <= A) and (A <= EA_)) or
      ((SA_ <= A + 2 * Pi) and (A + 2 * Pi <= EA_)) then
    begin
      Distance := D;
      Result := True;
    end;
  end;
  P1 := ShiftPoint(CP, PolarVector(R, SA));
  P2 := ShiftPoint(CP, PolarVector(R, EA));
  case Kind of
    ci_Arc:
      D := Min(PointDistance2D(P, P1),
        PointDistance2D(P, P2));
    ci_Sector:
      D := Min(PointSegmentDistance2D(P, CP, P1),
        PointSegmentDistance2D(P, CP, P2));
    ci_Segment:
      D := PointSegmentDistance2D(P, P1, P2);
  end;
  if D <= Distance then
  begin
    Distance := D;
    Result := True;
  end;
end;

function IsPointInEllipse(
  const P, CP: TPoint2D;
  const RX, RY, ARot: TRealType): Boolean;
var
  V: TVector2D;
  PA: TPoint2D;
  A, C: TRealType;
begin
  V := TransformVector2D(Vector2D(CP, P),
    Rotate2D(-ARot));
  if RX >= RY then
  begin
    A := RX;
    C := Sqrt(Sqr(RX) - Sqr(RY));
    PA := Point2D(V.X, V.Y);
  end
  else
  begin
    A := RY;
    C := Sqrt(Sqr(RY) - Sqr(RX));
    PA := Point2D(V.Y, V.X);
  end;
  Result := PointDistance2D(PA, Point2D(C, 0)) +
    PointDistance2D(PA, Point2D(-C, 0)) <= 2 * A;
end;

function IsPointOnEllipse(
  const P, CP: TPoint2D;
  const RX, RY, ARot, Precision: TRealType): Boolean;
var
  V: TVector2D;
  PA: TPoint2D;
  A, B, C, D: TRealType;
begin
// Approximate method
  V := TransformVector2D(Vector2D(CP, P),
    Rotate2D(-ARot));
  if RX >= RY then
  begin
    A := RX;
    B := RY;
    PA := Point2D(V.X, V.Y);
  end
  else
  begin
    A := RY;
    B := RX;
    PA := Point2D(V.Y, V.X);
  end;
  C := Sqrt(Sqr(A) - Sqr(B));
  D := (PointDistance2D(PA, Point2D(C, 0)) +
    PointDistance2D(PA, Point2D(-C, 0))) / 2;
  if D < C then D := C;
  D := B - Sqrt(Sqr(D) - Sqr(C));
  Result := Abs(D) <= Precision;
end;

function SubdivBezierCrossingInc(
  const P: TPoint2D;
  P0, P1, P2, P3: TPoint2D;
  const Precision: TRealType): Integer;
var
  Rect: TRect2D;
  P4, P5, P6: TPoint2D;
begin
  Result := 0;
  Rect := PointsBoundingBox([P0, P1, P2, P3]);
  if not IsPointInCartesianBox2D(P, Rect) then
  begin
    Result := CrossingInc(P, P0, P3);
    Exit;
  end;
  if ((Rect.Right - Rect.Left) <= Precision)
    and ((Rect.Top - Rect.Bottom) <= Precision)
    then Exit;
  // Use subdivision
  BreakBezierMid(P0, P1, P2, P3, P4, P5, P6);
  Inc(Result, SubdivBezierCrossingInc(P, P0, P1, P2, P3,
    Precision));
  Inc(Result, SubdivBezierCrossingInc(P, P3, P4, P5, P6,
    Precision));
end;

function IsPointInBezierShape(
  const PP: TPointsSet2D; P: TPoint2D;
  const Closed: Boolean; const T: TTransf2D;
  const WRule: TWinding;
  const Precision: TRealType): Boolean;
var
  I, N, CrossingNumber: Integer;
  P0, P3: TPoint2D;
  PVect: PVectPoints2D;
begin
  PVect := PP.PointsReference;
  Result := False;
  if PP.Count < 3 then Exit;
  CrossingNumber := 0;
  P := CartesianPoint2D(P);
  P0 := CartesianPoint2D(TransformPoint2D(
    PVect^[PP.Count - 1], T));
  N := PP.Count div 3;
  P0 :=
    CartesianPoint2D(TransformPoint2D(PP[0], T));
  for I := 1 to N do
  begin
    if Closed and (I = N) then
      P3 := CartesianPoint2D(TransformPoint2D(PP[0], T))
    else
      P3 := CartesianPoint2D(TransformPoint2D(PP[I * 3], T));
    Inc(CrossingNumber,
      SubdivBezierCrossingInc(P, P0,
      CartesianPoint2D(TransformPoint2D(PP[I * 3 - 2], T)),
      CartesianPoint2D(TransformPoint2D(PP[I * 3 - 1], T)),
      P3, Precision));
    P0 := P3;
  end;
  if N * 3 + 1 = PP.Count then
    Inc(CrossingNumber,
      CrossingInc(P,
      CartesianPoint2D(TransformPoint2D(PP[PP.Count - 1], T)),
      CartesianPoint2D(TransformPoint2D(PP[0], T))));
  // non-zero winding rule or even/odd
  case WRule of
    wn_NonZero: if CrossingNumber <> 0 then Result := True;
    wn_EvenOdd: if Odd(CrossingNumber) then Result := True;
  end;
end;


procedure PointsBoundingBox0(
  const PVect: PVectPoints2D; const Count: Integer;
  var Rect: TRect2D);
var
  I: Integer;
  P: TPoint2D;
begin
  if Count = 0 then
    Exit;
  for I := 0 to Count - 1 do
  begin
    P := CartesianPoint2D(PVect^[I]);
    if P.X > Rect.Right then
      Rect.Right := P.X;
    if P.X < Rect.Left then
      Rect.Left := P.X;
    if P.Y > Rect.Top then
      Rect.Top := P.Y;
    if P.Y < Rect.Bottom then
      Rect.Bottom := P.Y;
  end;
end;

function PointsBoundingBox(
  PointsArray: array of TPoint2D): TRect2D;
var
  I: Integer;
  P: TPoint2D;
begin
  if Length(PointsArray) = 0 then
  begin
    Result := Rect2D(0, 0, 0, 0);
    Exit;
  end;
  P := CartesianPoint2D(PointsArray[0]);
  Result.FirstEdge := P;
  Result.SecondEdge := P;
  for I := 1 to Length(PointsArray) - 1 do
  begin
    P := CartesianPoint2D(PointsArray[I]);
    if P.X > Result.Right then
      Result.Right := P.X;
    if P.X < Result.Left then
      Result.Left := P.X;
    if P.Y > Result.Top then
      Result.Top := P.Y;
    if P.Y < Result.Bottom then
      Result.Bottom := P.Y;
  end;
end;

procedure BezierSegmentBoundingBox(
  const P0, P1, P2, P3: TPoint2D;
  var Rect: TRect2D);
  procedure CheckPoint(const U: TRealType);
  begin
    if U <= 0 then Exit;
    if U >= 1 then Exit;
    Rect := PointOutBox2D(BezierPoint(
      P0, P1, P2, P3, U), Rect);
  end;
  procedure CheckRoots(const A0, A1, A2, A3: TRealType);
  var
    A, B2, C, D: TRealType;
  begin
    // Find roots of quadratic equation
    A := A3 - A0 - 3 * (A2 - A1);
    B2 := A0 + A2 - 2 * A1;
    C := A1 - A0;
    if (B2 = 0) and (C = 0) then Exit;
    if Abs(A) / (Abs(B2) + Abs(C)) < 1E-10 then
    begin
      if C = 0 then Exit;
      if Abs(B2) / Abs(C) > 1E-10 then
        CheckPoint(-C / B2 / 2); // if linear
      Exit;
    end;
    D := Sqr(B2) - A * C;
    if D < 0 then Exit;
    D := Sqrt(D) / A;
    B2 := -B2 / A;
    CheckPoint(B2 - D);
    if D <> 0 then CheckPoint(B2 + D);
  end;
begin
  Rect := PointOutBox2D(P0, Rect);
  Rect := PointOutBox2D(P3, Rect);
  CheckRoots(P0.X, P1.X, P2.X, P3.X);
  CheckRoots(P0.Y, P1.Y, P2.Y, P3.Y);
end;

procedure BezierPathBoundingBox(
  const PP: TPointsSet2D;
  const Closed: Boolean;
  var Rect: TRect2D);
var
  P0, P3: TPoint2D;
  I, N: Integer;
begin
  if PP.Count = 0 then Exit;
  if PP.Count < 3 then
  begin
    Rect := PointOutBox2D(PP[0], Rect);
    Exit;
  end;
  N := PP.Count div 3;
  P0 := PP[0];
  for I := 1 to N do
  begin
    if Closed and (I = N) then P3 := PP[0]
    else P3 := PP[I * 3];
    BezierSegmentBoundingBox(
      P0, PP[I * 3 - 2], PP[I * 3 - 1], P3, Rect);
    P0 := P3;
  end;
end;

function CirceBoundingBox(
  const CP: TPoint2D; const R: TRealType): TRect2D;
begin
  Result := Rect2D(CP.X - R, CP.Y - R, CP.X + R, CP.Y + R);
end;

function CircularBoundingBox(
  const CP: TPoint2D; const R: TRealType;
  SA, EA: TRealType; const Kind: TCircularKind): TRect2D;
var
  I: Integer;
  A: TRealType;
begin
  Result.FirstEdge :=
    ShiftPoint(CP, PolarVector(R, SA));
  Result.SecondEdge :=
    ShiftPoint(CP, PolarVector(R, EA));
  Result := ReorderRect2D(Result);
  if Kind <> ci_Arc then
    Result := PointOutBox2D(CP, Result);
  SA := SA - Floor(SA / (2 * Pi)) * 2 * Pi;
  EA := EA - Floor(EA / (2 * Pi)) * 2 * Pi;
  if EA < SA then EA := EA + 2 * Pi;
  for I := 0 to 8 do
  begin
    A := I * Pi / 2;
    if (SA <= A) and (A < EA) then
    begin
      Result := PointOutBox2D(
        ShiftPoint(CP, PolarVector(R, A)), Result);
    end;
  end;
end;

function RectangleBoundingBox(
  const P: TPoint2D;
  const W, H, ARot: TRealType): TRect2D;
var
  P0, P1, P2, P3: TPoint2D;
begin
  RectanglePoints(P, W, H, ARot, P0, P1, P2, P3);
  Result := PointsBoundingBox([P0, P1, P2, P3]);
end;

function EllipseBoundingBox(
  const CP: TPoint2D;
  const RX, RY, ARot: TRealType): TRect2D;
var
  C, S, RR1, RR2, AA: TRealType;
begin
  if (RX = 0) and (RY = 0) then
  begin
    Result.FirstEdge := CP;
    Result.SecondEdge := CP;
    Exit;
  end;
  C := Cos(ARot);
  S := Sin(ARot);
  RR1 := Sqrt(Sqr(RX * C) + Sqr(RY * S));
  if RR1 = 0 then RR1 := 1;
  RR2 := Sqrt(Sqr(RX * S) + Sqr(RY * C));
  if RR2 = 0 then RR2 := 1;
  AA := C * S * (Sqr(RY) - Sqr(RX));
  Result := PointsBoundingBox([
    ShiftPoint(CP, V2D(RR1, AA / RR1)),
      ShiftPoint(CP, V2D(-RR1, -AA / RR1)),
      ShiftPoint(CP, V2D(AA / RR2, RR2)),
      ShiftPoint(CP, V2D(-AA / RR2, -RR2))]);
end;

function BezierBasic(
  const A0, A1, A2, A3, U: TRealType): TRealType;
begin
  Result :=
    Sqr(1 - U) * (A0 * (1 - U) + A1 * 3 * U)
    + Sqr(U) * (A2 * 3 * (1 - U) + A3 * U);
end;

function BezierPoint(const P0, P1, P2, P3: TPoint2D;
  const U: TRealType): TPoint2D;
begin
  Result := Point2D(
    BezierBasic(P0.X, P1.X, P2.X, P3.X, U),
    BezierBasic(P0.Y, P1.Y, P2.Y, P3.Y, U));
end;

function BezierDerivative(const P0, P1, P2, P3: TPoint2D;
  const U: TRealType): TVector2D;
begin
  Result := V2D(
    3 * ((1 - U) * (P0.X * (U - 1) + P1.X * (1 - 3 * U))
    + U * (P2.X * (2 - 3 * U) + P3.X * U)),
    3 * ((1 - U) * (P0.Y * (U - 1) + P1.Y * (1 - 3 * U))
    + U * (P2.Y * (2 - 3 * U) + P3.Y * U)));
end;

function QBezierPoint(const P0, P1, P2: TPoint2D;
  const U: TRealType): TPoint2D;
begin
  Result := Point2D(
    (1 - U) * (P0.X * (1 - U) + P1.X * 2 * U) + Sqr(U) * P2.X,
    (1 - U) * (P0.Y * (1 - U) + P1.Y * 2 * U) + Sqr(U) * P2.Y);
end;

procedure SmallArcBezier(CP: TPoint2D; R, SA, EA: TRealType;
  var P0, P1, P2, P3: TPoint2D);
var
  AA, BB: TRealType;
  S: TPoint2D;
begin // cubic Bezier approximation to a small circular arc
  BB := Cos((EA - SA) / 2);
  if BB = 0 then BB := 1;
  BB := 1 / BB;
  AA := 4 / (3 * (BB + 1)); // approximately 0.65
  P0 := Point2D(CP.X + R * Cos(SA), CP.Y + R * Sin(SA));
  P3 := Point2D(CP.X + R * Cos(EA), CP.Y + R * Sin(EA));
  S := MidPoint(P0, P3);
  S := MixPoint(CP, S, Sqr(BB));
  P1 := MixPoint(P0, S, AA);
  P2 := MixPoint(P3, S, AA);
end;

procedure SmallEllipticArcBezier(CP: TPoint2D; RX, RY, Rot, SA,
  EA:
  TRealType;
  var P0, P1, P2, P3: TPoint2D);
var
  AA, BB: TRealType;
  S: TPoint2D;
  T: TTransf2D;
//CP is the center of the ellipse
//RX and RY are the radii of the ellipse (also known as its semi-major and semi-minor axes)
//Rot is the angle from the x-axis of the current coordinate system to the x-axis of the ellipse
//SA is the start angle of the elliptical arc prior to the stretch and rotate operations
//EA is the end angle of the elliptical arc prior to the stretch and rotate operations
begin // cubic Bezier approximation to a small elliptic arc
  BB := Cos((EA - SA) / 2);
  if BB = 0 then BB := 1;
  BB := 1 / BB;
  AA := 4 / (3 * (BB + 1)); // approximately 0.65
  P0 := Point2D(RX * Cos(SA), RY * Sin(SA));
  P3 := Point2D(RX * Cos(EA), RY * Sin(EA));
  S := MidPoint(P0, P3);
  S := MixPoint(Point2D(0, 0), S, Sqr(BB));
  P1 := MixPoint(P0, S, AA);
  P2 := MixPoint(P3, S, AA);
  T := MultiplyTransform2D(Rotate2D(Rot), Translate2D(CP.X,
    CP.Y));
  P0 := TransformPoint2D(P0, T);
  P1 := TransformPoint2D(P1, T);
  P2 := TransformPoint2D(P2, T);
  P3 := TransformPoint2D(P3, T);
end;

const
  Ell_Cnst = 0.265206;

procedure EllipseBezierPoints8(CP: TPoint2D;
  RX, RY, ARot: TRealType; PP: TPointsSet2D);
var
  P0, P1, P2: TPoint2D;
  T: TTransf2D;
  I: Integer;
  procedure Rot45(var P: TPoint2D);
  begin
    P := Point2D(
      (P.X - P.Y) * ISqrt2, (P.X + P.Y) * ISqrt2);
  end;
begin
  //Ellipse approximation by cubic Bezier path
  //Precision 3E-6!
  PP.Clear;
  PP.Expand(25);
  PP.Add(Point2D(1, 0));
  P0 := Point2D(1, Ell_Cnst);
  PP.Add(P0);
  P1 := Point2D(
    (1 + Ell_Cnst) * ISqrt2, (1 - Ell_Cnst) * ISqrt2);
  PP.Add(P1);
  P2 := Point2D(ISqrt2, ISqrt2);
  PP.Add(P2);
  for I := 1 to 7 do
  begin
    Rot45(P0);
    PP.Add(P0);
    Rot45(P1);
    PP.Add(P1);
    Rot45(P2);
    PP.Add(P2);
  end;
  T := Translate2D(CP.X, CP.Y);
  if ARot <> 0 then
    T := MultiplyTransform2D(Rotate2D(ARot), T);
  T := MultiplyTransform2D(Scale2D(RX, RY), T);
  for I := 0 to 24 do
    PP[I] := TransformPoint2D(PP[I], T);
end;

procedure CircularBezierPoints(CP: TPoint2D; R, SA, EA:
  TRealType;
  PP: TPointsSet2D; const Kind: TCircularKind);
var
  Angle, Delta, AA, BB: TRealType;
  I, NArcs: Integer;
  S, P0, P3: TPoint2D;
begin // Bezier path approximation to a circular arc
  Angle := EA - SA - Floor((EA - SA) / (2 * Pi)) * (2 * Pi);
  NArcs := Ceil(Angle * 4 / Pi);
  PP.Clear;
  if Kind = ci_Sector then
    PP.Expand(NArcs * 3 + 4)
  else
    PP.Expand(NArcs * 3 + 1);
  //if NArcs = 0 then Exit;
  if NArcs > 0 then Delta := Angle / NArcs;
  BB := 1 / Cos(Delta / 2);
  AA := 4 / (3 * (BB + 1)); // approximately 0.65
  P0 := Point2D(CP.X + R * Cos(SA), CP.Y + R * Sin(SA));
  PP[0] := P0;
  for I := 0 to NArcs - 1 do
  begin
    P3 := Point2D(CP.X + R * Cos(SA + Delta * (I + 1)),
      CP.Y + R * Sin(SA + Delta * (I + 1)));
    S := MidPoint(P0, P3);
    S := MixPoint(CP, S, Sqr(BB));
    PP[I * 3 + 1] := MixPoint(P0, S, AA);
    PP[I * 3 + 2] := MixPoint(P3, S, AA);
    PP[I * 3 + 3] := P3;
    P0 := P3;
  end;
  if Kind = ci_Sector then
  begin
    PP[NArcs * 3 + 1] := MixPoint(PP[NArcs * 3], CP, 0.25);
    PP[NArcs * 3 + 2] := MixPoint(PP[NArcs * 3], CP, 0.75);
    PP[NArcs * 3 + 3] := CP;
  end;
end;

procedure RoundRectBezierPoints(P: TPoint2D;
  W, H, RX, RY, ARot: TRealType;
  PP: TPointsSet2D);
var
  TmpPP: TPointsSet2D;
begin
  PP.Expand(24);
  TmpPP := TPointsSet2D.Create(12);
  try
    TmpPP.Add(Point2D(P.X, P.Y + RY));
    TmpPP.Add(Point2D(P.X, P.Y));
    TmpPP.Add(Point2D(P.X + RX, P.Y));
    TmpPP.Add(Point2D(P.X + W - RX, P.Y));
    TmpPP.Add(Point2D(P.X + W, P.Y));
    TmpPP.Add(Point2D(P.X + W, P.Y + RY));
    TmpPP.Add(Point2D(P.X + W, P.Y + H - RY));
    TmpPP.Add(Point2D(P.X + W, P.Y + H));
    TmpPP.Add(Point2D(P.X + W - RX, P.Y + H));
    TmpPP.Add(Point2D(P.X + RX, P.Y + H));
    TmpPP.Add(Point2D(P.X, P.Y + H));
    TmpPP.Add(Point2D(P.X, P.Y + H - RY));
    TmpPP.TransformPoints(RotateCenter2D(ARot, TmpPP[1]));
    PP.Add(TmpPP[0]);
    PP.Add(MixPoint(TmpPP[0], TmpPP[1], 0.5567));
    PP.Add(MixPoint(TmpPP[2], TmpPP[1], 0.5567));
    PP.Add(TmpPP[2]);
    PP.Add(MidPoint(TmpPP[2], TmpPP[3]));
    PP.Add(MidPoint(TmpPP[2], TmpPP[3]));
    PP.Add(TmpPP[3]);
    PP.Add(MixPoint(TmpPP[3], TmpPP[4], 0.5567));
    PP.Add(MixPoint(TmpPP[5], TmpPP[4], 0.5567));
    PP.Add(TmpPP[5]);
    PP.Add(MidPoint(TmpPP[5], TmpPP[6]));
    PP.Add(MidPoint(TmpPP[5], TmpPP[6]));
    PP.Add(TmpPP[6]);
    PP.Add(MixPoint(TmpPP[6], TmpPP[7], 0.5567));
    PP.Add(MixPoint(TmpPP[8], TmpPP[7], 0.5567));
    PP.Add(TmpPP[8]);
    PP.Add(MidPoint(TmpPP[8], TmpPP[9]));
    PP.Add(MidPoint(TmpPP[8], TmpPP[9]));
    PP.Add(TmpPP[9]);
    PP.Add(MixPoint(TmpPP[9], TmpPP[10], 0.5567));
    PP.Add(MixPoint(TmpPP[11], TmpPP[10], 0.5567));
    PP.Add(TmpPP[11]);
    PP.Add(MidPoint(TmpPP[11], TmpPP[0]));
    PP.Add(MidPoint(TmpPP[11], TmpPP[0]));
  finally
    TmpPP.Free;
  end;
end;

function SubdivClosestBezierPoint0(const P: TPoint2D;
  P0, P1, P2, P3: TPoint2D;
  const Precision: TRealType;
  var MinDist: TRealType): TRealType;
var
  P4, P5, P6: TPoint2D;
  V0: TVector2D;
  D0, D3, D_Min, D_Max, U: TRealType;
begin
  V0 := Vector2D(P, P0);
  D0 := VectorLength2D(V0);
  if D0 < MinDist then
  begin
    MinDist := D0;
    Result := 0;
  end;
  D3 := PointDistance2D(P, P3);
  if D3 < MinDist then
  begin
    MinDist := D3;
    Result := 1;
  end;
  if MinDist < Precision then Exit;
  V0 := NormalizeVector2D(V0);
  D_Min := Min(Min(D0,
    DotProduct2D(V0, Vector2D(P, P1))),
    Min(DotProduct2D(V0, Vector2D(P, P2)),
    DotProduct2D(V0, Vector2D(P, P3))));
  // D_Min is a lower estimate for the distance
  if D_Min > MinDist then Exit;
  D_Max := Max(Max(D0,
    DotProduct2D(V0, Vector2D(P, P1))),
    Max(DotProduct2D(V0, Vector2D(P, P2)),
    DotProduct2D(V0, Vector2D(P, P3))));
  if D_Max - D_Min < Precision then
  begin
    // Change direction to perpendicular one
    V0 := Perpendicular2D(V0);
    D_Min := Min(Min(0,
      DotProduct2D(V0, Vector2D(P, P1))),
      Min(DotProduct2D(V0, Vector2D(P, P2)),
      DotProduct2D(V0, Vector2D(P, P3))));
    D_Max := Max(Max(0,
      DotProduct2D(V0, Vector2D(P, P1))),
      Max(DotProduct2D(V0, Vector2D(P, P2)),
      DotProduct2D(V0, Vector2D(P, P3))));
    // If curve segment is small then stop
    if D_Max - D_Min < Precision then Exit;
  end;
  // Use subdivision
  BreakBezierMid(P0, P1, P2, P3, P4, P5, P6);
  if D0 <= D3 then
  begin
    D0 := MinDist;
    U := SubdivClosestBezierPoint0(P, P0, P1, P2, P3,
      Precision, MinDist) / 2;
    if MinDist < D0 then Result := U;
    if MinDist < Precision then Exit;
    D0 := MinDist;
    U := SubdivClosestBezierPoint0(P, P3, P4, P5, P6,
      Precision, MinDist) / 2 + 0.5;
    if MinDist < D0 then Result := U;
    Exit;
  end;
  D0 := MinDist;
  U := SubdivClosestBezierPoint0(P, P3, P4, P5, P6,
    Precision, MinDist) / 2 + 0.5;
  if MinDist < D0 then Result := U;
  if MinDist < Precision then Exit;
  D0 := MinDist;
  U := SubdivClosestBezierPoint0(P, P0, P1, P2, P3,
    Precision, MinDist) / 2;
  if MinDist < D0 then Result := U;
end;

function SubdivClosestBezierPoint(
  const P, P0, P1, P2, P3:
  TPoint2D; const Precision: TRealType): TRealType;
// Finds a point on Bezier curve which is nearest to a given point P.
// Uses a variant of Recursive Subdivision algorithm
var
  MinDist: TRealType;
begin
  MinDist := MaxRealType;
  Result :=
    SubdivClosestBezierPoint0(P, P0, P1, P2, P3,
    Precision, MinDist);
end;

function ClosestBezierPoint(
  const P, P0, P1, P2, P3: TPoint2D;
  const Precision: TRealType): TRealType;
begin
  Result := SubdivClosestBezierPoint(P, P0, P1, P2, P3,
    Precision);
end;

function IsPointOnBezierStroke(
  const PP: TPointsSet2D; const P: TPoint2D;
  const Closed: Boolean; const Precision: TRealType;
  var Distance: TRealType;
  out Pos: Integer): Boolean;
var
  MinDist: TRealType;
  P0, P3: TPoint2D;
  I, N: Integer;
begin
  Result := False;
  if PP.Count = 0 then Exit;
  if PP.Count < 3 then
  begin
    Result := PointDistance2D(P, PP[0]) < Distance;
    Pos := 1;
    if Result then
      Distance := PointDistance2D(P, PP[0]);
    Exit;
  end;
  N := PP.Count div 3;
  P0 := PP[0];
  MinDist := Distance;
  for I := 1 to N do
  begin
    if Closed and (I = N) then P3 := PP[0]
    else P3 := PP[I * 3];
    SubdivClosestBezierPoint0(
      P, P0, PP[I * 3 - 2], PP[I * 3 - 1], P3,
      Precision, MinDist);
    if MinDist < Distance then
    begin
      Distance := MinDist;
      Result := True;
      Pos := I - 1;
    end;
    P0 := P3;
  end;
end;

procedure BreakBezier(var P0, P1, P2, P3, P4, P5, P6: TPoint2D;
  const U: TRealType);
// Bezier curve subdivision at parameter U
begin
  P6 := P3;
  P5 := MixPoint(P2, P3, U);
  P4 := QBezierPoint(P1, P2, P3, U);
  P3 := BezierPoint(P0, P1, P2, P3, U);
  P2 := QBezierPoint(P0, P1, P2, U);
  P1 := MixPoint(P0, P1, U);
end;

procedure BreakBezierMid(var P0, P1, P2, P3, P4, P5, P6:
  TPoint2D);
// Bezier curve subdivision at parameter 0.5
begin
  P6 := P3;
  P5 := MidPoint(P2, P3);
  P3 := MidPoint(P1, P2);
  P1 := MidPoint(P0, P1);
  P2 := MidPoint(P3, P1);
  P4 := MidPoint(P3, P5);
  P3 := MidPoint(P2, P4);
end;

procedure RectangleCalcPoints(P0, P1, P2: TPoint2D;
  var P3, P4: TPoint2D; var A: TRealType);
begin
  A := (Sqr(P2.X - P0.X) + Sqr(P2.Y - P0.Y));
  if A = 0 then
  begin
    A := 1;
    P2 := Point2D(P0.X, P1.Y);
  end
  else
    A := ((P1.X - P0.X) * (P2.X - P0.X)
      + (P1.Y - P0.Y) * (P2.Y - P0.Y)) / A;
  P3 := MixPoint(P0, P2, A);
  P4 := ShiftPoint(P0, Vector2D(P3, P1));
end;

procedure LinearizeCircle(PP: TPointsSet2D;
  const CP: TPoint2D; const R: TRealType;
  const CurvePrecision: Integer);
var
  I: Integer;
  Delta, CurrAngle: TRealType;
begin
  Delta := TWOPI / CurvePrecision;
  CurrAngle := 0;
  PP.Clear;
  PP.Expand(CurvePrecision);
  for I := 0 to CurvePrecision - 1 do
  begin
    PP[I] := Point2D(CP.X + R * Cos(CurrAngle),
      CP.Y + R * Sin(CurrAngle));
    CurrAngle := CurrAngle + Delta;
  end;
end;

procedure LinearizeCircular(PP: TPointsSet2D;
  const CP: TPoint2D; const R, SA, EA: TRealType;
  const Kind: TCircularKind;
  const CurvePrecision: Integer);
var
  I: Integer;
  Delta, CurrAngle: TRealType;
begin
  PP.Clear;
  if Kind = ci_Sector then
    PP.Expand(CurvePrecision + 1)
  else
    PP.Expand(CurvePrecision);
  if SA < EA then
    Delta := (EA - SA) / (CurvePrecision - 1)
  else
    Delta := (TWOPI - SA + EA) / (CurvePrecision - 1);
  CurrAngle := SA;
  for I := 0 to CurvePrecision - 1 do
  begin
    PP[I] := Point2D(CP.X + R * Cos(CurrAngle),
      CP.Y + R * Sin(CurrAngle));
    CurrAngle := CurrAngle + Delta
  end;
  if Kind = ci_Sector then
    PP[CurvePrecision] := CP;
end;

procedure RectanglePoints(const P: TPoint2D;
  const W, H, ARot: TRealType;
  out P0, P1, P2, P3: TPoint2D);
var
  DH, DV: TVector2D;
begin
  DH := PolarVector(W, ARot);
  DV := PolarVector(H, ARot + Pi / 2);
  P0 := P;
  P1 := ShiftPoint(P, DH);
  P2 := ShiftPoint(ShiftPoint(P, DH), DV);
  P3 := ShiftPoint(P, DV);
end;

procedure LinearizeRectangle(PP: TPointsSet2D;
  const P: TPoint2D; const W, H, ARot: TRealType);
var
  P0, P1, P2, P3: TPoint2D;
begin
  PP.Expand(4);
  RectanglePoints(P, W, H, ARot, P0, P1, P2, P3);
  PP[0] := P0;
  PP[1] := P1;
  PP[2] := P2;
  PP[3] := P3;
end;

procedure LinearizeEllipse(PP: TPointsSet2D;
  const CP: TPoint2D; const RX, RY, ARot: TRealType;
  const CurvePrecision: Integer);
var
  I: Integer;
  Delta, CurrAngle: TRealType;
  T: TTransf2D;
begin
  Delta := TWOPI / CurvePrecision;
  CurrAngle := 0;
  PP.Clear;
  PP.Expand(CurvePrecision);
  if ARot <> 0 then
    T := RotateCenter2D(ARot, CP);
  for I := 0 to CurvePrecision - 1 do
  begin
    PP[I] := Point2D(CP.X + RX * Cos(CurrAngle),
      CP.Y + RY * Sin(CurrAngle));
    if ARot <> 0 then PP[I] := TransformPoint2D(PP[I], T);
    CurrAngle := CurrAngle + Delta;
  end;
end;

procedure LinearizeBezier_X(const BezierPP: TPointsSet2D;
  Precision: Double; const Closed: Boolean;
  const LinPP: TPointsSet2D);
// Recursive subdivision algorithm
var
  I: Integer;
  function IsApproxLine(const P0, P1, P2, P3: TPoint2D): Boolean;
  begin
// function PointDistance2D(const P1, P2: TPoint2D): TRealType;
    Result :=
      (PointDistance2D(P1, MidPoint(P0, P2)) < Precision) and
      (PointDistance2D(P2, MidPoint(P1, P3)) < Precision);
  end;
  procedure AddArc(const P0, P1, P2, P3: TPoint2D);
  var
    Q1, Q2, Q3, Q4, Q5: TPoint2D;
  begin
    if IsApproxLine(P0, P1, P2, P3) then
    begin
      LinPP.Add(P0);
      Exit;
    end;
    Q1 := MidPoint(P0, P1);
    Q3 := MidPoint(P1, P2);
    Q5 := MidPoint(P2, P3);
    Q2 := MidPoint(Q1, Q3);
    Q4 := MidPoint(Q3, Q5);
    Q3 := MidPoint(Q2, Q4);
    AddArc(P0, Q1, Q2, Q3);
    AddArc(Q3, Q4, Q5, P3);
  end;
begin
  if BezierPP.Count = 0 then Exit;
  Precision := Precision / 3;
  LinPP.Expand(BezierPP.Count);
  LinPP.Clear;
  for I := 0 to BezierPP.Count div 3 - 1 do
    AddArc(BezierPP[I * 3], BezierPP[I * 3 + 1],
      BezierPP[I * 3 + 2], BezierPP[I * 3 + 3]);
  if Closed or (BezierPP.Count = 1) then
    LinPP.Add(BezierPP[0])
  else
    LinPP.Add(BezierPP[BezierPP.Count - 1]);
end;

procedure LinearizeBezier(const BezierPP: TPointsSet2D;
  const Precision: Integer; const Closed: Boolean;
  const LinPP: TPointsSet2D);
// Direct evaluation algorithm
var
  I: Integer;
  procedure AddBezierArc(const PP: TPointsSet2D;
    const Precision: Integer;
    const P0, P1, P2, P3: TPoint2D);
  var
    I: Integer;
  begin
    for I := 0 to Precision - 1 do
      PP.Add(BezierPoint(P0, P1, P2, P3, I / Precision));
  end;
begin
  if BezierPP.Count = 0 then Exit;
  LinPP.Clear;
  LinPP.Expand(Precision * (BezierPP.Count div 3));
  for I := 0 to BezierPP.Count div 3 - 1 do
    AddBezierArc(LinPP, Precision,
      BezierPP[I * 3], BezierPP[I * 3 + 1],
      BezierPP[I * 3 + 2], BezierPP[I * 3 + 3]);
  if Closed or (BezierPP.Count = 1) then
    LinPP.Add(BezierPP[0])
  else
    LinPP.Add(BezierPP[BezierPP.Count - 1]);
end;

procedure LinPolyToBezier(const LinPP: TPointsSet2D;
  const Closed: Boolean; const BezierPP: TPointsSet2D);
var
  I: Integer;
begin
  if LinPP.Count = 0 then Exit;
  BezierPP.Clear;
  if LinPP.Count = 1 then
  begin
    BezierPP.Add(LinPP[0]);
    Exit;
  end;
  BezierPP.Expand((LinPP.Count - 1 + Byte(Closed)) * 3 + 1);
  BezierPP.Add(LinPP[0]);
  for I := 0 to LinPP.Count - 2 do
    if not IsSamePoint2D(LinPP[I], LinPP[I + 1]) then
      BezierPP.AddPoints([MixPoint(LinPP[I], LinPP[I + 1], 0.25),
        MixPoint(LinPP[I], LinPP[I + 1], 0.75),
          LinPP[I + 1]]);
  if Closed then
  begin
    if not IsSamePoint2D(LinPP[LinPP.Count - 1], LinPP[0]) then
      BezierPP.AddPoints([MixPoint(LinPP[LinPP.Count - 1],
          LinPP[0], 0.25),
        MixPoint(LinPP[LinPP.Count - 1], LinPP[0], 0.75),
          LinPP[0]]);
  end;
end;

// =====================================================================
// TPointsSet2D
// =====================================================================

procedure TPointsSet2D.Expand(const NewCapacity: Integer);
var
  I: Integer;
begin
  if NewCapacity <= fCapacity then
    Exit;
  ReAllocMem(fPoints, NewCapacity * SizeOf(TPoint2D));
  for I := fCapacity to NewCapacity - 1 do
    with PVectPoints2D(fPoints)^[I] do
    begin
      X := 0.0;
      Y := 0.0;
      W := 1.0;
    end;
  fCapacity := NewCapacity;
end;

constructor TPointsSet2D.Create(const _Capacity: Integer);
begin
  inherited Create;

  fCount := 0;
  fCapacity := _Capacity;
  GetMem(fPoints, fCapacity * SizeOf(TPoint2D));
  fGrowEnabled := True;
  fTag := 0;
end;

destructor TPointsSet2D.Destroy;
begin
  FreeMem(fPoints, fCapacity * SizeOf(TPoint2D));
  inherited Destroy;
end;

function TPointsSet2D.Get(Index: Integer): TPoint2D;
begin
  if Index < fCount then
    Result := PVectPoints2D(fPoints)^[Index]
  else
    raise
      Exception.Create('TPointsSet2D.Get: Index out of bound');
end;

procedure TPointsSet2D.GetBoundingBox0(var Rect: TRect2D);
begin
  PointsBoundingBox0(
    PVectPoints2D(fPoints), fCount, Rect);
end;

function TPointsSet2D.GetBoundingBox: TRect2D;
begin
  Result := Rect2D(
    MaxRealType, MaxRealType, -MaxRealType, -MaxRealType);
  GetBoundingBox0(Result);
end;

procedure TPointsSet2D.Put(PutIndex, ItemIndex: Integer;
  const Item: TPoint2D);
begin
  if PutIndex < fCapacity then
  begin
    PVectPoints2D(fPoints)^[PutIndex] := Item;
    if PutIndex >= fCount then
      fCount := PutIndex + 1;
  end
  else if fGrowEnabled then
  begin
    Expand(MaxIntValue([PutIndex + 1, fCapacity * 2 + 1]));
    PVectPoints2D(fPoints)^[PutIndex] := Item;
    Inc(fCount);
  end
  else
    raise
      Exception.Create('TPointsSet2D.Put: Vector out of bound');
end;

procedure TPointsSet2D.PutProp(Index: Integer;
  const Item: TPoint2D);
begin
  Put(Index, Index, Item);
end;

procedure TPointsSet2D.Add(const Item: TPoint2D);
begin
  PutProp(fCount, Item);
end;

procedure TPointsSet2D.AddPoints(const Items: array of
  TPoint2D);
var
  I: Integer;
begin
  if (not fGrowEnabled) and (High(Items) - Low(Items) >
    fCapacity) then
    raise
      Exception.Create('TPointsSet2D.AddPoints: Vector out of bound');
  Expand(fCapacity + High(Items) - Low(Items) + 1);
  for I := Low(Items) to High(Items) do
    Put(fCount, fCount, Items[I]);
end;

procedure TPointsSet2D.AppendPoints(PP: TPointsSet2D);
var
  I: Integer;
begin
  if (not fGrowEnabled) and (fCount + PP.Count > fCapacity) then
    raise
      Exception.Create('TPointsSet2D.AppendPoints: Vector out of bound');
  Expand(fCount + PP.Count);
  for I := 0 to PP.Count - 1 do
    Put(fCount, fCount, PP[I]);
end;

procedure TPointsSet2D.Clear;
begin
  fCount := 0;
end;

procedure TPointsSet2D.Delete(const Index: Integer);
var
  I: Integer;
begin
  if Index < fCount then
  begin
    for I := Index to fCount - 2 do
      Put(I, I + 1, PVectPoints2D(fPoints)^[I + 1]);
    Dec(fCount);
  end
  else
    raise
      Exception.Create('TPointsSet2D.Delete: Vector out of bound');
end;

procedure TPointsSet2D.Insert(const Index: Integer;
  const Item: TPoint2D);
var
  I: Integer;
begin
  if Index >= fCount then
    raise
      Exception.Create('TPointsSet2D.Insert: Vector out of bound');
  Put(fCount, fCount, Item);
  for I := fCount - 1 downto Index + 1 do
    Put(I, I - 1, PVectPoints2D(fPoints)^[I - 1]);
  Put(Index, Index, Item);
end;

procedure TPointsSet2D.TransformPoints(const T: TTransf2D);
var
  I: Integer;
  P: TPoint2D;
begin
  for I := 0 to fCount - 1 do
  begin
    P := TransformPoint2D(PVectPoints2D(fPoints)^[I], T);
    PVectPoints2D(fPoints)^[I] := P;
  end;
end;

procedure TPointsSet2D.ReversePoints;
var
  I: Integer;
  P: TPoint2D;
begin
  for I := 0 to fCount div 2 - 1 do
  begin
    P := PVectPoints2D(fPoints)^[I];
    PVectPoints2D(fPoints)^[I]
      := PVectPoints2D(fPoints)^[fCount - I - 1];
    PVectPoints2D(fPoints)^[fCount - I - 1] := P;
  end;
end;

procedure TPointsSet2D.Copy(const S: TPointsSet2D; const StIdx,
  EndIdx: Integer);
var
  I: Integer;
begin
  try
    Expand(EndIdx + 1);
    for I := StIdx to EndIdx do
      Put(I, I, S[I]);
  except
  end;
end;

function TPointsSet2D.IsPointOnPolylineStroke(
  const P: TPoint2D; const Closed: Boolean;
  var Distance: TRealType;
  out Pos: Integer): Boolean;
var
  D: TRealType;
begin
  D := PointPolylineDistance(
    Self, P, IdentityTransf2D, Closed, Pos);
  Result := D < Distance;
  if Result then Distance := D;
end;

function TPointsSet2D.IsPointInside(const P: TPoint2D;
  const Precision: TRealType): Boolean;
begin
  Result := IsPointInPolygon(
    Self, P, IdentityTransf2D, wn_NonZero);
end;

// =====================================================================
// TEPointsSet2D
// =====================================================================

constructor TEPointsSet2D.Create(const _Capacity: Integer);
begin
  inherited Create(_Capacity);
  fDisableEvents := False;
end;

procedure TEPointsSet2D.SetDisableEvents(B: Boolean);
begin
  fDisableEvents := B;
end;

procedure TEPointsSet2D.CallOnChange;
begin
  // Call onChange only if the events are enabled.
  if (not fDisableEvents) and Assigned(fOnChange) then
  begin
    fDisableEvents := True;
    try
      fOnChange(Self);
    finally
      fDisableEvents := False;
    end;
  end;
end;

procedure TEPointsSet2D.PutProp(Index: Integer;
  const Item: TPoint2D);
begin
  Put(Index, Index, Item);
  CallOnChange;
end;

procedure TEPointsSet2D.AddPoints(const Items: array of
  TPoint2D);
begin
  inherited AddPoints(Items);
  CallOnChange;
end;

procedure TEPointsSet2D.AppendPoints(PP: TPointsSet2D);
begin
  inherited AppendPoints(PP);
  CallOnChange;
end;

procedure TEPointsSet2D.Delete(const Index: Integer);
begin
  inherited Delete(Index);
  CallOnChange;
end;

procedure TEPointsSet2D.Insert(const Index: Integer;
  const Item: TPoint2D);
begin
  inherited Insert(Index, Item);
  CallOnChange;
end;

procedure TEPointsSet2D.TransformPoints(const T: TTransf2D);
begin
  inherited TransformPoints(T);
  CallOnChange;
end;

procedure TEPointsSet2D.Copy(const S: TPointsSet2D; const StIdx,
  EndIdx:
  Integer);
begin
  inherited Copy(S, StIdx, EndIdx);
  CallOnChange;
end;

     {*---- Hobby spline functions ----*}

function GetHobbyBezierAngle(const P0, P1, Q0, Q1: TPoint2D):
  TRealType;
begin
  Result :=
    ArcTan2(Q1.Y - Q0.Y, Q1.X - Q0.X)
    - ArcTan2(P1.Y - P0.Y, P1.X - P0.X);
  if Result >= Pi then
    Result := Result - 2 * Pi
  else if Result < -Pi then
    Result := Result + 2 * Pi;
end;

procedure GetHobbyBezierConstants(const Theta, Phi: TRealType;
  var Alph, Rho, Sigma: TRealType);
const
  C0 = 0.381966011250105; //(3 - Sqrt(5)) / 2;
  Sqrt2 = 1.4142135623731;
begin
  Alph := Sqrt2 * (Sin(Theta) - Sin(Phi) / 16)
    * (Sin(Phi) - Sin(Theta) / 16)
    * (Cos(Theta) - Cos(Phi));
  Rho := (2 + Alph) /
    (1 + (1 - C0) * Cos(Theta) + C0 * Cos(Phi));
  Sigma := (2 - Alph) /
    (1 + (1 - C0) * Cos(Phi) + C0 * Cos(Theta));
end;

procedure MakeHobbyBezierTransform(var P0, P1: TPoint2D;
  const Theta, Phi, Alph, Rho, Sigma: TRealType);
var
  TT: TTransf2D;
begin
  TT := IdentityTransf2D;
  TT[1, 1] := P1.X - P0.X;
  TT[2, 1] := P0.Y - P1.Y;
  TT[3, 1] := P0.X;
  TT[1, 2] := P1.Y - P0.Y;
  TT[2, 2] := P1.X - P0.X;
  TT[3, 2] := P0.Y;
  P0 :=
    TransformPoint2D(Point2D(Rho / 3 * Cos(Theta),
    Rho / 3 * Sin(Theta)), TT);
  P1 :=
    TransformPoint2D(Point2D(1 - Sigma / 3 * Cos(Phi),
    Sigma / 3 * Sin(Phi)), TT);
end;

procedure GetHobbyBezier(var PP: TPointsSet2D;
  const Points: TPointsSet2D);
var
  I, N: Integer;
  Len, A, B, C, D, S, T, Theta, Phi, Psi: array of TRealType;
  Gamm, Rho, Sigma, Alph: TRealType;
  P0, P1: TPoint2D;
begin
  N := Points.Count - 1;
  if N = 0 then
  begin
    PP[0] := Points[0];
    Exit;
  end;
  if N = 1 then
  begin
    PP[0] := Points[0];
    PP[1] := MixPoint(Points[0], Points[1], 0.25);
    PP[2] := MixPoint(Points[0], Points[1], 0.75);
    PP[3] := Points[1];
    Exit;
  end;
  SetLength(Len, N);
  for I := 0 to N - 1 do
    Len[I] := PointDistance2D(Points[I + 1], Points[I]);
  SetLength(Psi, N);
  for I := 1 to N - 1 do
    Psi[I] := GetHobbyBezierAngle(Points[I - 1],
      Points[I], Points[I], Points[I + 1]);
  SetLength(A, N - 1);
  SetLength(B, N - 1);
  SetLength(C, N - 1);
  SetLength(D, N - 1);
  SetLength(S, N);
  SetLength(T, N);
  if N > 2 then
  begin
    if Len[1] > 1E-20 * Len[0] then
      C[0] := Len[0] / Len[1]
    else
      C[0] := 1;
    S[1] := 1 + 2 * C[0];
    T[1] := Psi[1] + Psi[2] * C[0];
    A[N - 2] := 1;
    if Len[N - 1] > 0 then
      B[N - 2] := 2 + Len[N - 2] / Len[N - 1]
    else
      B[N - 2] := 3;
    C[N - 2] := 0;
    D[N - 2] := 2 * Psi[N - 1];
  end
  else
  begin
    if Len[1] > 1E-20 * Len[0] then
      S[1] := 1 + Len[0] / Len[1]
    else
      S[1] := 2;
    T[1] := Psi[1];
  end;
  for I := 1 to N - 3 do
  begin
    A[I] := 1;
    if Len[I + 1] > 0 then
      C[I] := Len[I] / Len[I + 1]
    else
      C[I] := 1;
    B[I] := 2 * (1 + C[I]);
    D[I] := 2 * Psi[I + 1] + Psi[I + 2] * C[I];
  end;
  for I := 1 to N - 2 do
  begin
    Gamm := A[I] / S[I];
    S[I + 1] := B[I] - Gamm * C[I - 1];
    T[I + 1] := D[I] - Gamm * T[I];
  end;
  SetLength(Theta, N);
  SetLength(Phi, N + 1);
  Theta[N - 1] := -T[N - 1] / S[N - 1];
  for I := N - 2 downto 1 do
    if S[I] <> 0 then
      Theta[I] := -(T[I] + C[I - 1] * Theta[I + 1]) / S[I]
    else
      Theta[I] := 0;
  Phi[N] := Theta[N - 1];
  for I := 1 to N - 1 do
    Phi[I] := -Psi[I] - Theta[I];
  Theta[0] := Phi[1];
  for I := 0 to N - 1 do
  begin
    GetHobbyBezierConstants(Theta[I], Phi[I + 1], Alph, Rho,
      Sigma);
    if I = 0 then PP[0] := Points[0];
    P0 := Points[I];
    P1 := Points[I + 1];
    MakeHobbyBezierTransform(P0, P1,
      Theta[I], Phi[I + 1], Alph, Rho, Sigma);
    PP[I * 3 + 1] := P0;
    PP[I * 3 + 2] := P1;
    PP[I * 3 + 3] := Points[I + 1];
  end;
end;

procedure GetClosedHobbyBezier(var PP: TPointsSet2D;
  const Points: TPointsSet2D);
var
  I, N: Integer;
  Len, A, B, C, D, K, L, M, P, Q, R, Theta, Phi, Psi
    : array of TRealType;
  Alph, Bet, Rho, Sigma: TRealType;
  P0, P1: TPoint2D;
  function GetPoint(I: Integer): TPoint2D;
  begin
    if I < N then
      Result := Points[I]
    else
      Result := Points[I - N];
  end;
begin
  N := Points.Count;
  if N = 1 then
  begin
    PP[0] := Points[0];
    Exit;
  end;
  if N = 2 then
  begin
    PP[0] := Points[0];
    PP[1] := MixPoint(Points[0], Points[1], 0.25);
    PP[2] := MixPoint(Points[0], Points[1], 0.75);
    PP[3] := Points[1];
    PP[4] := MixPoint(Points[0], Points[1], 0.75);
    PP[5] := MixPoint(Points[0], Points[1], 0.25);
    PP[6] := Points[0];
    Exit;
  end;
  SetLength(Len, N + 2);
  for I := 0 to N - 1 do
    Len[I] := PointDistance2D(GetPoint(I + 1), Points[I]);
  Len[N] := Len[0];
  Len[N + 1] := Len[1];
  SetLength(Psi, N + 2);
  for I := 1 to N do
    Psi[I] := GetHobbyBezierAngle(GetPoint(I - 1),
      GetPoint(I), GetPoint(I), GetPoint(I + 1));
  Psi[0] := Psi[N];
  Psi[N + 1] := Psi[1];
  SetLength(A, N);
  SetLength(B, N);
  SetLength(C, N);
  SetLength(D, N);
  SetLength(K, N - 1);
  SetLength(L, N - 1);
  SetLength(M, N - 1);
  SetLength(P, N - 1);
  SetLength(Q, N - 1);
  SetLength(R, N - 1);
  for I := 0 to N - 1 do
  begin
    A[I] := 1;
    if Len[I + 1] > 0 then
      C[I] := Len[I] / Len[I + 1]
    else
      C[I] := 1;
    B[I] := 2 * (1 + C[I]);
    D[I] := 2 * Psi[I + 1] + Psi[I + 2] * C[I];
  end;
  K[0] := A[0];
  L[0] := B[0];
  M[0] := D[0];
  P[0] := B[N - 1];
  Q[0] := C[N - 1];
  R[0] := D[N - 1];
  for I := 0 to N - 3 do
  begin
    Alph := A[I + 1] / L[I];
    K[I + 1] := -Alph * K[I];
    L[I + 1] := B[I + 1] - Alph * C[I];
    M[I + 1] := D[I + 1] - Alph * M[I];
    Bet := Q[I] / L[I];
    P[I + 1] := P[I] - Bet * K[I];
    Q[I + 1] := -Bet * C[I];
    R[I + 1] := R[I] - Bet * M[I];
  end;
  SetLength(Theta, N);
  SetLength(Phi, N + 1);
  Theta[0] :=
    (M[N - 2] * (Q[N - 2] + A[N - 1]) - L[N - 2] * R[N - 2]) /
    (P[N - 2] * L[N - 2]
    - (Q[N - 2] + A[N - 1]) * (K[N - 2] + C[N - 2]));
  Theta[N - 1] :=
    -(M[N - 2] + (K[N - 2] + C[N - 2]) * Theta[0]) / L[N - 2];
  {for I := 0 to N - 3 do
    if Q[I] = 0 then Theta[I + 1] := 0
    else Theta[I + 1] :=
      -(R[I] + P[I] * Theta[0] + A[N - 1] * Theta[N - 1])
        / Q[I];}//unstable formula
  for I := N - 3 downto 0 do
    if L[I] = 0 then
      Theta[I + 1] := 0
    else
      Theta[I + 1] :=
        -(M[I] + K[I] * Theta[0] + C[I] * Theta[I + 2])
        / L[I];
  for I := 0 to N - 1 do
    Phi[I] := -Psi[I] - Theta[I];
  Phi[N] := Phi[0];
  for I := 0 to N - 1 do
  begin
    GetHobbyBezierConstants(Theta[I], Phi[I + 1], Alph, Rho,
      Sigma);
    if I = 0 then PP[0] := Points[0];
    P0 := Points[I];
    P1 := GetPoint(I + 1);
    MakeHobbyBezierTransform(P0, P1,
      Theta[I], Phi[I + 1], Alph, Rho, Sigma);
    PP[I * 3 + 1] := P0;
    PP[I * 3 + 2] := P1;
    PP[I * 3 + 3] := GetPoint(I + 1);
  end;
end;

procedure DeleteBezierPointSmoothly(
  const P0, P1, P2, P3, P4, P5, P6: TPoint2D;
  var Q1, Q2: TPoint2D);
var
  Alph, Rho, Sigma, Theta, Phi: TRealType;
begin
  Theta := Pi - GetHobbyBezierAngle(P0, P1, P6, P0);
  Phi := GetHobbyBezierAngle(P5, P6, P0, P6);
  GetHobbyBezierConstants(Theta, Phi, Alph, Rho, Sigma);
  Q1 := P0;
  Q2 := P6;
  MakeHobbyBezierTransform(Q1, Q2,
    Theta, Phi, Alph, Rho, Sigma);
end;

     {*---- T_SVG_Path_Parser ----*}

function SVG_Path_HasCurves(const PathData: string): Boolean;
const
  CurvedChars = 'CSQTAcsqta';
var
  I: Integer;
begin
  Result := False;
  for I := 1 to Length(PathData) do
    if Pos(PathData[I], CurvedChars) > 0 then
    begin
      Result := True;
      Exit;
    end;
end;

constructor T_SVG_Path_Parser.Create(
  const OnClosePath: T_SVG_Path_ClosePathMethod;
  const OnMoveTo, OnLineTo: T_SVG_Path_LineToMethod;
  const OnBezierTo: T_SVG_Path_BezierToMethod);
begin
  inherited Create;
  Self.OnClosePath := OnClosePath;
  Self.OnMoveTo := OnMoveTo;
  Self.OnLineTo := OnLineTo;
  Self.OnBezierTo := OnBezierTo;
  AllAsBezier := False;
  fParser := TSimpleParser.Create;
end;

destructor T_SVG_Path_Parser.Destroy;
begin
  fParser.Free;
  inherited Destroy;
end;

procedure T_SVG_Path_Parser.Parse(const Source: string);
var
  CurrPathCharIndex: Integer;
  Data: array[1..7] of TRealType;
  PInit, PCurr: TPoint2D;
  VCurr: TVector2D;
const
  PathChars = { } 'MLHVCSQTAZmlhvcsqtaz';
  PathCharsNext = 'LLHVCSQTA llhvcsqta ';
  NumArg: array[1..20] of Integer =
  (2, 2, 1, 1, 6, 4, 4, 2, 7, 0, 2, 2, 1, 1, 6, 4, 4, 2, 7, 0);
  procedure TakeSpace;
  begin
    fParser.SkipAnyMult([#32, ',', #13, #10, #9]);
  end;
  function GetNum: TRealType;
  begin
    Result := StrToRealType(fParser.GetNumStr, 0);
  end;
  procedure GetPathCommand;
  var
    Index, J: Integer;
    procedure MoveTo(X, Y: TRealType);
    begin
      OnMoveTo(X, Y);
      PCurr := Point2D(X, Y);
      PInit := PCurr;
    end;
    procedure LineTo(X, Y: TRealType);
    begin
      if not AllAsBezier then
        OnLineTo(X, Y)
      else
        OnBezierTo(
          (2 * PCurr.X + X) / 3, (2 * PCurr.Y + Y) / 3,
          (PCurr.X + 2 * X) / 3, (PCurr.Y + 2 * Y) / 3, X, Y);
      PCurr := Point2D(X, Y);
    end;
    procedure BezierTo(X1, Y1, X2, Y2, X3, Y3: TRealType);
    begin
      OnBezierTo(X1, Y1, X2, Y2, X3, Y3);
      VCurr.X := X3 - X2;
      VCurr.Y := Y3 - Y2;
      PCurr := Point2D(X3, Y3);
    end;
    procedure QBezierTo(X1, Y1, X2, Y2: TRealType);
    begin
      OnBezierTo((PCurr.X + 2 * X1) / 3, (PCurr.Y + 2 * Y1) /
        3,
        (2 * X1 + X2) / 3, (2 * Y1 + Y2) / 3, X2, Y2);
      // Model quadratic Bezier curve as cubic one
      VCurr.X := X2 - X1;
      VCurr.Y := Y2 - Y1;
      PCurr := Point2D(X2, Y2);
    end;
    procedure EllipticArcEndpointToCenter(
      P1, P2: TPoint2D; RX, RY, Rot: TRealType; FA, FS:
      Boolean;
      var CP: TPoint2D; var SA, EA: TRealType);
{ Based on SVG specification. "F.6 Elliptical arc implementation notes"
    For most situations, there are actually 4 different arcs.
    If large-arc-flag is '1' (<>0), then one of
  the two larger arc sweeps will be chosen; otherwise, if
  large-arc-flag is '0', one of the smaller arc sweeps will be
  chosen
    If sweep-flag is '1' (<>0), then the arc will be drawn in a
  "positive-angle" direction. A value of 0
  causes the arc to be drawn in a "negative-angle" direction}
    var
      V1, VC: TVector2D;
      Root: TRealType;
    begin
      if (RX = 0) or (RY = 0) or IsSamePoint2D(P1, P2) then
      begin
        CP := MidPoint(P1, P2);
        SA := 0;
        EA := 2 * Pi;
        Exit;
      end;
      RX := Abs(RX);
      RY := Abs(RY);
      V1 := Vector2D(P2, P1);
      V1 := ScaleVector2D(V1, 0.5);
      V1 := TransformVector2D(V1, Rotate2D(-Rot));
      Root := Sqr(V1.X) / Sqr(RX) + Sqr(V1.Y) / Sqr(RY);
      if Root > 1 then //If radii are incorrect
      begin
        RX := RX * Sqrt(Root);
        RY := RY * Sqrt(Root);
        Root := 1;
      end;
      VC.X := RX * V1.Y / RY;
      VC.Y := -RY * V1.X / RX;
      Root := Sqrt(1 / Root - 1);
      if FA = FS then Root := -Root;
      VC := ScaleVector2D(VC, Root);
      CP := MidPoint(P1, P2);
      CP := ShiftPoint(CP, TransformVector2D(VC,
        Rotate2D(Rot)));
      SA := VectorAngle2((V1.X - VC.X) / RX, (V1.Y - VC.Y) /
        RY);
      EA := VectorAngle2((-V1.X - VC.X) / RX, (-V1.Y - VC.Y) /
        RY);
      if (not FS) and (EA > SA) then
        EA := EA - 2 * Pi
      else if FS and (EA < SA) then
        EA := EA + 2 * Pi;
    end;

    procedure ArcTo(RX, RY, Rot, FA, FS, X, Y: TRealType);
    //rx ry x-axis-rotation large-arc-flag sweep-flag x y
    var
      CP, P0, P1, P2, P3: TPoint2D;
      SA, EA, DA: TRealType;
      I, N: Integer;
    begin
      if not AllAsBezier then
        OnLineTo(X, Y)
      else
      begin
        Rot := DegToRad(Rot);
        EllipticArcEndpointToCenter(
          PCurr, Point2D(X, Y), RX, RY, Rot, FA <> 0, FS <> 0,
          CP,
          SA, EA);
        N := Ceil(2 * Abs(EA - SA) / Pi);
        if N > 0 then DA := (EA - SA) / N;
        for I := 0 to N - 1 do
        begin
          SmallEllipticArcBezier(CP, RX, RY, Rot,
            SA + DA * I, SA + DA * (I + 1), P0, P1, P2, P3);
          OnBezierTo(P1.X, P1.Y, P2.X, P2.Y, P3.X, P3.Y);
        end;
      end;
      PCurr := Point2D(X, Y);
    end;
    procedure Closepath;
    begin
      if not AllAsBezier then
        OnClosePath(PInit.X, PInit.Y)
      else
      begin
        if not IsSamePoint2D(PInit, PCurr) then
          OnBezierTo((2 * PCurr.X + PInit.X) / 3,
            (2 * PCurr.Y + PInit.Y) / 3,
            (PCurr.X + 2 * PInit.X) / 3,
            (PCurr.Y + 2 * PInit.Y) / 3,
            PInit.X, PInit.Y);
        OnClosePath(PInit.X, PInit.Y)
      end;
      PCurr := PInit;
    end;
    //PInit, PCurr
  begin
    TakeSpace;
    if fParser.Finished then Exit;
    Index := Pos(fParser.ViewCh, PathChars);
    if Index > 0 then
    begin
      fParser.GetCh;
      CurrPathCharIndex := Pos(PathCharsNext[Index],
        PathChars);
    end
    else
      Index := CurrPathCharIndex;
    for J := 1 to NumArg[Index] do
    begin
      TakeSpace;
      Data[J] := GetNum;
    end;
    case Index of //  'MLHVCSQTAZmlhvcsqtaz';
      1 {M}: MoveTo(Data[1], Data[2]);
      2 {L}: LineTo(Data[1], Data[2]);
      3 {H}: LineTo(Data[1], PCurr.Y);
      4 {V}: LineTo(PCurr.X, Data[1]);
      5 {C}: BezierTo(Data[1], Data[2], Data[3], Data[4],
          Data[5],
          Data[6]);
      6 {S}: BezierTo(PCurr.X + VCurr.X, PCurr.Y + VCurr.Y,
          Data[1], Data[2], Data[3], Data[4]);
      7 {Q}: QBezierTo(Data[1], Data[2], Data[3], Data[4]);
      8 {T}: QBezierTo(PCurr.X + VCurr.X, PCurr.Y + VCurr.Y,
          Data[1], Data[2]);
      9 {A}: ArcTo(Data[1], Data[2], Data[3], Data[4], Data[5],
          Data[6],
          Data[7]);
      10, 20 {Z,z}: Closepath;
      11 {m}: MoveTo(PCurr.X + Data[1], PCurr.Y + Data[2]);
      12 {l}: LineTo(PCurr.X + Data[1], PCurr.Y + Data[2]);
      13 {h}: LineTo(PCurr.X + Data[1], PCurr.Y);
      14 {v}: LineTo(PCurr.X, PCurr.Y + Data[1]);
      15 {c}: BezierTo(PCurr.X + Data[1], PCurr.Y + Data[2],
          PCurr.X + Data[3], PCurr.Y + Data[4],
          PCurr.X + Data[5], PCurr.Y + Data[6]);
      16 {s}: BezierTo(PCurr.X + VCurr.X, PCurr.Y + VCurr.Y,
          PCurr.X + Data[1], PCurr.Y + Data[2],
          PCurr.X + Data[3], PCurr.Y + Data[4]);
      17 {q}: QBezierTo(PCurr.X + Data[1], PCurr.Y + Data[2],
          PCurr.X + Data[3], PCurr.Y + Data[4]);
      18 {t}: QBezierTo(PCurr.X + VCurr.X, PCurr.Y + VCurr.Y,
          PCurr.X + Data[1], PCurr.Y + Data[2]);
      19 {a}: ArcTo(Data[1], Data[2], Data[3], Data[4],
          Data[5],
          PCurr.X + Data[6], PCurr.Y + Data[7]);
    end;
  end;
begin
  fParser.SetSource(@Source);
  CurrPathCharIndex := 2;
  PCurr := Point2D(0, 0);
  repeat GetPathCommand;
  until fParser.Finished;
end;

     {*---- SVG_Transform_Parse ----*}

function SVG_Transform_Parse(const Source: string): TTransf2D;
var
  fParser: TSimpleParser;
  procedure TakeSpace;
  begin
    fParser.SkipAnyMult([#32, ',', #13, #10, #9]);
  end;
  function GetTransform: TTransf2D;
  var
    Index, NNum: Integer;
    Data: array[1..7] of TRealType;
    Num: TRealType;
    ID: string;
  begin
    Result := IdentityTransf2D;
    TakeSpace;
    ID := fParser.GetAnyMult(['a'..'x', 'X', 'Y']);
    Index :=
      CSV_Find('matrix,translate,scale,rotate,skewX,skewY',
      ID);
    if Index = 0 then
    begin
      fParser.SetFinished;
      Exit;
    end;
    TakeSpace;
    if fParser.GetCh <> '(' then
    begin
      fParser.SetFinished;
      Exit;
    end;
    NNum := 0;
    repeat
      TakeSpace;
      Num := StrToRealType(fParser.GetNumStr, 8934567);
      if (Index = 4) and (NNum = 0) then
      begin
        TakeSpace;
        if fParser.GetAnyMult(['r', 'a', 'd']) = 'rad' then
        begin
          //fParser.Take(3);
          Num := RadToDeg(Num);
        end;
      end;
      if Num <> 8934567 then
      begin
        Inc(NNum);
        Data[NNum] := Num;
      end;
    until Num = 8934567;
    TakeSpace;
    if fParser.GetCh <> ')' then
    begin
      fParser.SetFinished;
      Exit;
    end;
    case Index of
      1: //matrix
        begin
          Result[1, 1] := Data[1];
          Result[1, 2] := Data[2];
          Result[2, 1] := Data[3];
          Result[2, 2] := Data[4];
          Result[3, 1] := Data[5];
          Result[3, 2] := Data[6];
        end;
      2: //translate
        begin
          if NNum = 1 then
            Result := Translate2D(Data[1], Data[1])
          else
            Result := Translate2D(Data[1], Data[2]);
        end;
      3: //scale
        begin
          if NNum = 1 then
            Result := Scale2D(Data[1], Data[1])
          else
            Result := Scale2D(Data[1], Data[2]);
        end;
      4: //rotate
        begin
          if NNum = 1 then
            Result := Rotate2D(DegToRad(Data[1]))
          else
            Result := RotateCenter2D(DegToRad(Data[1]),
              Point2D(Data[2], Data[3]));
        end;
      5: //skewX
        Result := Skew2D(DegToRad(Data[1]), 0, Point2D(0, 0));
      6: //skewY
        Result := Skew2D(0, DegToRad(Data[1]), Point2D(0, 0));
    end;
  end;
begin
  fParser := TSimpleParser.Create;
  try
    fParser.SetSource(@Source);
    Result := IdentityTransf2D;
    repeat Result := MultiplyTransform2D(GetTransform, Result);
    until fParser.Finished;
  finally
    fParser.Free;
  end;
end;

procedure SimplifyPoly(PP: TPointsSet2D; OutPP: TPointsSet2D;
  const Delta: TRealType; const Closed: Boolean);
var
  V, D, V1, V2, B1, B2, C, C1, C2: TVector2D;
  I, N: Integer;
  P0: TPoint2D;
  A: TRealType;
  function GetP(const I: Integer): TPoint2D;
  begin
    if I < PP.Count then
      Result := PP[I]
    else
      Result := PP[I - PP.Count]
  end;
  function Coord(const V, B1, B2: TVector2D): TVector2D;
  // Find coordinates of V in the coordinate system formed by B1 and B2
  var
    VB1, VB2, B12, B11, B22: TRealType;
  begin
    VB1 := DotProduct2D(V, B1);
    VB2 := DotProduct2D(V, B2);
    B12 := DotProduct2D(B1, B2);
    B11 := DotProduct2D(B1, B1);
    B22 := DotProduct2D(B2, B2);
    Result.X := B11 * B22 - Sqr(B12);
    if Result.X <> 0 then
      Result.X := (VB1 * B22 - VB2 * B12) / Result.X;
    if B22 <> 0 then
      Result.Y := (VB2 - Result.X * B12) / B22;
  end;
  procedure GetBase;
  begin
    repeat
      if I >= N then Exit;
      V := Vector2D(P0, GetP(I));
      A := VectorLength2D(V);
      Inc(I);
    until A > 0;
    D.X := -V.Y * Delta / A;
    D.Y := V.X * Delta / A;
    B1 := VectorAdd2D(V, D);
    B2 := VectorSubtract2D(V, D);
  end;
  procedure CheckNext;
  begin
    repeat
      if I >= N then Exit;
      V := Vector2D(P0, GetP(I));
      A := VectorLength2D(V);
      if A <= 0 then
      begin
        Inc(I);
        Continue;
      end;
      C := Coord(V, B1, B2);
      if (C.X < 0) or (C.Y < 0) then Exit;
      D.X := -V.Y * Delta / A;
      D.Y := V.X * Delta / A;
      V1 := VectorAdd2D(V, D);
      V2 := VectorSubtract2D(V, D);
      C1 := Coord(V1, B1, B2);
      C2 := Coord(V2, B1, B2);
      if C1.Y > 0 then B1 := V1;
      if C2.X > 0 then B2 := V2;
      Inc(I);
    until False;
  end;
begin
  OutPP.Clear;
  OutPP.Expand(PP.Count);
  if PP.Count < 3 then
  begin
    OutPP.Copy(PP, 0, PP.Count - 1);
    Exit;
  end;
  P0 := GetP(0);
  I := 1;
  if Closed then
    N := PP.Count + 1
  else
    N := PP.Count;
  while I < N do
  begin
    OutPP.Add(P0);
    GetBase;
    CheckNext;
    if I <= N then P0 := GetP(I - 1);
  end;
  if not Closed then OutPP.Add(P0);
end;

procedure SimplifyPolyInPlace(PP: TPointsSet2D;
  const Delta: TRealType; const Closed: Boolean);
var
  TmpPP: TPointsSet2D;
begin
  TmpPP := TPointsSet2D.Create(PP.Count);
  try
    TmpPP.Copy(PP, 0, PP.Count - 1);
    SimplifyPoly(TmpPP, PP, Delta, Closed);
  finally
    TmpPP.Free;
  end;
end;

procedure CalculateHatching(const P: TPointsSet2D;
  const DX, DY: TRealType; Step: TRealType;
  const Lines: TPointsSet2D; const FillRule: TFillRule);
var
  PP: TPointsSet2D;
  PrevP, CurrP, IntersP: TPoint2D;
  PrevV, CurrV, A: TRealType;
  MinValue, MaxValue, Value0, C: TRealType;
  I, J, K: Integer;
  Sgns: array of Boolean;
  TmpSgn: Boolean;
  SgnCount: Integer;
  function GetValue(P: TPoint2D): TRealType;
  begin
    Result := P.X * DX + P.Y * DY;
  end;
begin
  if P.Count < 3 then Exit;
  if Step <= 0 then Exit;
  Step := Step * Sqrt(Sqr(DX) + Sqr(DY));
  PP := TPointsSet2D.Create(P.Count);
  MinValue := MaxRealType;
  MaxValue := -MaxRealType;
  for I := 0 to P.Count - 1 do
  begin
    CurrV := GetValue(P[I]);
    if CurrV < MinValue then MinValue := CurrV;
    if CurrV > MaxValue then MaxValue := CurrV;
  end;
  // Collect intersection points in PP:
  Value0 := (Floor(MinValue / Step) + 1) * Step;
  SetLength(Sgns, P.Count);
  for J := 0 to
    Ceil(MaxValue / Step) - Floor(MinValue / Step) - 2 do
  begin
    PP.Clear;
    C := Value0 + J * Step;
    CurrP := P[P.Count - 1];
    CurrV := GetValue(CurrP);
    for I := 0 to P.Count - 1 do
    begin
      PrevP := CurrP;
      PrevV := CurrV;
      CurrP := P[I];
      CurrV := GetValue(CurrP);
      if ((CurrV > C) and (PrevV > C)) or
        ((CurrV < C) and (PrevV < C))
        or (CurrV = PrevV) then Continue;
      A := (C - PrevV) / (CurrV - PrevV);
      IntersP := Point2D(A * CurrP.X + (1 - A) * PrevP.X,
        A * CurrP.Y + (1 - A) * PrevP.Y);
      PP.Add(IntersP);
      Sgns[PP.Count - 1] := CurrV > PrevV;
    end;
    // Sort PP:
    for I := 0 to PP.Count - 1 do
    begin
      IntersP := PP[I];
      TmpSgn := Sgns[I];
      for K := I + 1 to PP.Count - 1 do
        if - DY * IntersP.X + DX * IntersP.Y
          > -DY * PP[K].X + DX * PP[K].Y then
        begin
          PP[I] := PP[K];
          PP[K] := IntersP;
          IntersP := PP[I];
          Sgns[I] := Sgns[K];
          Sgns[K] := TmpSgn;
          TmpSgn := Sgns[I];
        end;
    end;
    if FillRule = fr_Winding then
    begin
      SgnCount := 0;
      for K := 0 to (PP.Count div 2) * 2 - 1 do
      begin
        if SgnCount = 0 then Lines.Add(PP[K]);
        if Sgns[K] then
          Inc(SgnCount)
        else
          Dec(SgnCount);
        if SgnCount = 0 then Lines.Add(PP[K]);
      end;
    end
    else //FillRule = fr_Alternate
      for K := 0 to (PP.Count div 2) * 2 - 1 do
        Lines.Add(PP[K]);
  end;
  PP.Free;
end;

end.

