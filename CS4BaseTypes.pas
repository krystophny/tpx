{: This help file explain all the base types defined in
   the CADSys 4.0 library for both the 2D and 3D use.

   These types are defined in the CS4BaseTypes unit file
   that you must include in the <B=uses> clause in all of your units
   that use CADSys.
}
unit CS4BaseTypes;

interface

uses Classes, Windows, Clipbrd;

//TSY:
procedure PutStreamToClipboard0(Format: Word; Stream: TStream;
  Size: Longint);
procedure PutStreamToClipboard(Format: Word; Stream: TStream;
  Size: Longint);
procedure GetStreamFromClipboard(Format: Word; Stream: TStream);
procedure GetStreamFromClipboardAsText(Stream: TStream);

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
//TSY:
function StrToRealType(St: string): TRealType;

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
   lower-left ones and the upper-right ones. Consider that
   the origin of the coordinates of the library is different
   from the Windows' one. Use <See Function=Rect2DToRect> and
   <See Function=RectToRect2D> functions to convert from the
   two coordinate systems.

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

const
  TWOPI = 2 * Pi;
  Sqrt2 = 1.414213562373;

  {: Constant used with the picking functions.

     See also <See Method=TCADViewport2D@PickObject> and
     <See Method=TCADViewport3D@PickObject>.
  }
  PICK_NOOBJECT = -200;
  {: Constant used with the picking functions.

     See also <See Method=TCADViewport2D@PickObject> and
     <See Method=TCADViewport3D@PickObject>.
  }
  PICK_INBBOX = -100;
  {: Constant used with the picking functions.

     See also <See Method=TCADViewport2D@PickObject> and
     <See Method=TCADViewport3D@PickObject>.
  }
  PICK_ONOBJECT = -1;
  {: Constant used with the picking functions.

     See also <See Method=TCADViewport2D@PickObject> and
     <See Method=TCADViewport3D@PickObject>.
  }
  PICK_INOBJECT = -2;
  {: This is the identity matrix for 2D transformation.
  }
  IdentityTransf2D: TTransf2D = ((1.0, 0.0, 0.0), (0.0, 1.0,
    0.0),
    (0.0, 0.0, 1.0));
  {: This is the null matrix for 2D transformation.
  }
  NullTransf2D: TTransf2D = ((0.0, 0.0, 0.0), (0.0, 0.0, 0.0),
    (0.0, 0.0, 1.0));
  {: This is the minimum value for coordinates.
  }
  MinCoord = -1.0E8;
  {: This is the maximun value for coordinates.
  }
  MaxCoord = 1.0E8;


implementation

function StrToRealType(St: string): TRealType;
var
  J: Integer;
begin
  Val(St, Result, J);
end;

procedure PutStreamToClipboard0(Format: Word; Stream: TStream;
  Size: Longint);
var
  Len: Longint;
  Buffer: Pointer;
  Data: THandle;
begin
  Clipboard.Open;
  try
    Len := Stream.Size - Stream.Position;
    if Len > Size then Len := Size;
    Data := GlobalAlloc(gmem_Moveable or GMEM_DDESHARE
      {HeapAllocFlags}, Len);
    try
      if Data <> 0 then
      begin
        Buffer := GlobalLock(Data);
        try
          Stream.Read(Buffer^, Len);
          SetClipboardData(Format, Data);
        finally
          GlobalUnlock(Data);
        end;
      end;
    except
      GlobalFree(Data);
      raise;
    end;
  finally
    Clipboard.Close;
  end;
end;

procedure PutStreamToClipboard(Format: Word; Stream: TStream;
  Size: Longint);
begin
  Clipboard.Open;
  try
    PutStreamToClipboard0(Format, Stream, Size);
  finally
    Clipboard.Close;
  end;
end;

procedure GetStreamFromClipboard(Format: Word; Stream: TStream);
var
  Buffer: Pointer;
  Data: THandle;
begin
  Clipboard.Open;
  try
    Data := GetClipboardData(Format);
    if Data = 0 then Exit;
    Buffer := GlobalLock(Data);
    try
      Stream.Write(Buffer^, GlobalSize(Data));
    finally
      GlobalUnlock(Data);
    end;
  finally
    Clipboard.Close;
  end;
end;

procedure GetStreamFromClipboardAsText(Stream: TStream);
var
  Len: Integer;
  Str: string;
begin
  //GetStreamFromClipboard(CF_TEXT, Stream);
  Clipboard.Open;
  try
    if not Clipboard.HasFormat(CF_TEXT) then Exit;
    Str := Clipboard.AsText;
    Len := Length(Str);
    if Len > 0 then Stream.Write(Str[1], Len);
  finally
    Clipboard.Close;
  end;
end;

end.
