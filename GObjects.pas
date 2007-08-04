unit GObjects;

// This unit comprises classes which represent drawing objects.

{$IFNDEF VER140}
{$MODE Delphi}
{$ENDIF}

interface

uses Types, SysUtils, Classes,
{$IFDEF VER140}
  Windows, WinBasic
{$ELSE}
  LCLIntf, LCLType, LMessages, LCLProc, LazBasic
{$ENDIF}
  , Graphics, Drawings, Geometry, Contnrs,
  Pieces, Devices, GObjBase;

type

  {: This type defines the <I=saving mode> used by the <I=outline> primitive
     shapes.

     An <I=outline> primitive shape is an entity that is drawed as
     a connected set of points (<I=profile points>) which displacement
     is controlled by a set of <I=control points>. The points used
     to draw the entity may be keept in memory or recomputed from
     the control points whenever they are needed to draw the
     shape. In the former a lot of memory may be used but the
     drawing is faster, in the latter the drawing of the shape
     may take a quite longer time but the memory is used
     efficently.

     This type is used to specify beetwen the two modes:

     <LI=<I=stSpace> forces an <I=outline> to recompute the
      shape's points whenever it must be drawed.>
     <LI=<I=stTime> tell to the <I=outline> to store the
      shape's points and compute them only when the control
      points are changed.>
  }
  TPrimitiveSavingType = (stSpace, stTime);

  {: This type specifies how to draw an arc segment:

     <LI=in the <I=adClockwise> mode the arc segment is
      drawed clockwise>.
     <LI=in the <I=adCounterClockwise> mode the arc segment is
      drawed counter clockwise>.
  }

  TArrowKind = (arrNone, arrH40, arrH41, arrH42, arrH43, arrH44,
    arrH45, arrH46, arrH47, arrH48, arrT40, arrT43, arrT44, arrT45,
    arrH20, arrH21, arrH22, arrH23, arrH24, arrT20, arrT21, arrT22,
    arrT23,
    arrHR10, arrHR11, arrHR12, arrTR10, arrH10, arrH11, arrH12,
    arrH12C, arrT10,
    arrR0, arrR10, arrR11, arrR12, arrR20, arrR20C, arrR21, arrR33,
    arrTS10, arrTS11, arrTS12, arrHS10, arrHS12,
    arrTS20, arrTS21, arrTS23, arrHS20, arrHS23,
    arrO, arrOC, arrQQ);

const
  ArrowsIDs: array[0..52] of string[8] =
  ('none', 'h40', 'h41', 'h42', 'h43', 'h44',
    'h45', 'h46', 'h47', 'h48', 't40', 't43', 't44', 't45',
    'h20', 'h21', 'h22', 'h23', 'h24', 't20', 't21', 't22', 't23',
    'hr10', 'hr11', 'hr12', 'tr10', 'h10', 'h11', 'h12', 'h12c',
    't10',
    'r0', 'r10', 'r11', 'r12', 'r20', 'r20c', 'r21', 'r33',
    'ts10', 'ts11', 'ts12', 'hs10', 'hs12',
    'ts20', 'ts21', 'ts23', 'hs20', 'hs23',
    'o', 'oc', 'qq');

type

  {: This is the class reference type for the
     <See Class=TPrimitive2D> shape class.
  }
  TPrimitive2DClass = class of TPrimitive2D;

  {: This class defines a <I=2D primitive>.

     A primitive shape is an entity which shape is controlled
     by a set of <I=control points>. This class stores and
     handles this set of point allowing the developer to
     focus on the way to draw the shape from these control
     points.

     This is the right class from which derive your own
     entity classes.

     <B=Warning>: This is an abstract class that cannot be used
     directly.

     <B=Note>: The control points are always in the object model
     coordinate system !
  }
  TPrimitive2D = class(TObject2D)
  protected
    fPoints: TEPointsSet2D;
    fHatching: THatching;
    fLineStyle: TLineStyle;
    fLineWidth: TRealType;
    fLineColor, fHatchColor, fFillColor: TColor;
    fCanDeletePoints: Boolean;
    fDrawPathBezier: Boolean;
    fOwnsInterior: Boolean;
    fBeginArrowKind: TArrowKind;
    fEndArrowKind: TArrowKind;
    fArrowSizeFactor: TRealType;
    function GetArrowSize: TRealType;
    procedure _UpdateExtension; override;
    {: This method allows you to change the type of the set
       of points used to store the <I=control points> of the
       primitive.

       When the entity is created this method is called to
       create the set of <I=control points> that defines
       the shape of the entity.

       By default a <See Class=TPointsSet2D> instance is created
       to store a maximum of <I=Size> points.

       You may want to override this property to create
       a special set of points that is able to store more
       information than a simple point. For instance you can
       derive a new set from <See Class=TPointsSet2D> that for
       every point store the kind of the point. This is the
       first step to create a <I=path shape> that draw arc
       segment as well as straight lines.
    }
    function CreateVect(const Size: Integer): TEPointsSet2D;
      dynamic;
  public
    Pieces: TArrayOfPiece;
    FirstDrawPoint: Integer;
    {: This is the constructor of the class.

       The constructor need the identifier of the new graphic object.
       This <See Property=TGraphicObject@ID> will be used to
       identify the object in the <See Class=TDrawing2D>.

       If the object is added with the method <See Method=TDrawing@AddObject>
       and with the first parameter set to a number equal or greater that
       0, the <I=ID> given here will be overriden.

       If you derives from this class remember to call the
       inherited method. In this case pass the desired number
       of control points and set <See Property=TPointsSet2D@GrowingEnabled>
       of <See Property=TPrimitive2D@Points> to the desired value.

       Parameters:

       <LI=<I=ID> is the ID of the object.>
       <LI=<I=NPts> is the number of control points that the
        primitive can store without growing the vector.>
    }
    class function GetName: string; virtual; abstract;
    constructor Create(ID: Integer); override;
    constructor CreateSpec(ID: Integer; NPts: Integer;
      CurvePrec: Word);
    procedure WhenCreated;
    procedure FinishFirstDraw; virtual;
    destructor Destroy; override;
    constructor CreateFromStream(const Stream: TStream); override;
    procedure Assign(const Obj: TGraphicObject); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure FillPieces; virtual;
    //TSY:
    procedure BezierPoints(PP: TPointsSet2D); virtual;
    procedure DeviceDrawPiece(Piece: TPiece;
      const Transf: TTransf2D; const Dvc: TDevice;
      const ClipRect2D: TRect2D);
    procedure DeviceDrawPieces(const VT: TTransf2D;
      const Dvc: TDevice; const ClipRect2D: TRect2D);
    procedure DrawControlPoints(const VT: TTransf2D;
      const ClipRect2D: TRect2D; const Width: Integer); override;
    procedure DrawControlPoints0(const VT: TTransf2D;
      const ClipRect2D: TRect2D; const Width: Integer); virtual;
    procedure DeviceDraw(Transf: TTransf2D;
      const Dvc: TDevice; const ClipRect2D: TRect2D); override;
    function OnMe(P: TPoint2D; Aperture: TRealType;
      var Distance: TRealType): Integer; override;
    procedure TransForm(const T: TTransf2D); override;
    procedure ReversePoints; virtual;
    function DeleteControlPoint0(I: Integer): Integer; virtual;
    procedure DeleteControlPoint(const I: Integer); virtual;
    //Add control point at Pos
    function InsertControlPoint0(const Pos: Integer;
      P: TPoint2D; Precision: TRealType): Integer; virtual;
    function InsertControlPoint(P: TPoint2D;
      Aperture, Precision: TRealType): Boolean; virtual;
    procedure MoveControlPoint0(const Pos: Integer;
      P: TPoint2D; Shift: TShiftState); virtual;
    procedure BreakPath(P: TPoint2D;
      Aperture, Precision: TRealType; var Obj1, Obj2:
      TPrimitive2D);
      virtual;
    property Points: TEPointsSet2D read fPoints write fPoints;
    property OwnsInterior: Boolean read fOwnsInterior;
    property Hatching: THatching read fHatching write fHatching;
    property LineStyle: TLineStyle read fLineStyle
      write fLineStyle;
    property LineWidth: TRealType read fLineWidth
      write fLineWidth;
    property LineColor: TColor read fLineColor
      write fLineColor;
    property HatchColor: TColor read fHatchColor
      write fHatchColor;
    property FillColor: TColor read fFillColor
      write fFillColor;
    property BeginArrowKind: TArrowKind read fBeginArrowKind write
      fBeginArrowKind;
    property EndArrowKind: TArrowKind read fEndArrowKind write
      fEndArrowKind;
    property ArrowSizeFactor: TRealType read fArrowSizeFactor write
      fArrowSizeFactor;
    property ArrowSize: TRealType read GetArrowSize;
    property Name: string read GetName;
    property CanDeletePoints: Boolean read fCanDeletePoints;
  end;

  {: This class defines a 2D line segment.

     The entity has two <I=control points> that are the extremes of
     the segment.
  }
  TLine2D = class(TPrimitive2D)
  protected
  public
    class function GetName: string; override;
    {: This constructor creates a new 2D line segment.

       Parameters:

       <LI=<I=ID> is the object identifier.>
       <LI=<I=P1> is the starting point of the segment.>
       <LI=<I=P2> is the ending point of the segment.>
    }
    constructor Create(ID: Integer); override;
    constructor CreateSpec(ID: Integer; const P1, P2: TPoint2D);
    procedure WhenCreated;
    procedure Assign(const Obj: TGraphicObject); override;
    constructor CreateFromStream(const Stream: TStream); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure FillPieces; override;
    procedure ReversePoints; override;
    procedure BreakPath(P: TPoint2D;
      Aperture, Precision: TRealType; var Obj1, Obj2:
      TPrimitive2D);
      override;
  end;

  {: This class defines a 2D polyline.

     A polyline is obtained by connecting the <I=profile points> (in
     this case are the same as the <I=control points>) with straight
     line segments.
  }
  TPolyline2D0 = class(TPrimitive2D)
  public
    {: This constructor creates a new 2D polyline.

       Parameters:

       <LI=<I=ID> is the object identifier.>
       <LI=<I=Pts> is an array that contains the <I=control points> of
        the polyline. If you want to create a pointless polyline
        (because you want to add the points after the construction)
        you must pass an array of only one point (an empty array is
        not allowed by Delphi) and delete the point after the
        construction phase by using the method of
        <See Property=TPrimitive2D@Points>.>
    }
    constructor Create(ID: Integer); override;
    constructor CreateSpec(ID: Integer; const Pts: array of
      TPoint2D);
    procedure WhenCreated;
    procedure FillPieces; override;
    procedure Assign(const Obj: TGraphicObject); override;
    procedure ReversePoints; override;
  end;

  TPolyline2D = class(TPolyline2D0)
  protected
  public
    class function GetName: string; override;
    procedure FillPieces; override;
    procedure BreakPath(P: TPoint2D;
      Aperture, Precision: TRealType; var Obj1, Obj2:
      TPrimitive2D);
      override;
  end;

  {: This class defines a 2D polygon.

     A polygon is obtained by connecting the <I=profile points> (
     in this case they are the same as the <I=profile points>)
     with straight segments.
  }
  TPolygon2D = class(TPolyline2D0)
  protected
  public
    class function GetName: string; override;
    constructor Create(ID: Integer); override;
    constructor CreateSpec(ID: Integer; const Pts: array of
      TPoint2D);
    procedure WhenCreated;
    procedure FillPieces; override;
    procedure Assign(const Obj: TGraphicObject); override;
    procedure BreakPath(P: TPoint2D;
      Aperture, Precision: TRealType; var Obj1, Obj2:
      TPrimitive2D);
      override;
  end;


  TStarKind = (starCircle, starSquare, starDiamond, starTriUp,
    starTriDown,
    starPenta, starStar4, starStar5, starStar6, starCross,
    starDCross,
    starFlower5, starFlower4, starStar4Arc, starMaltese);

const
  StarsIDs: array[0..14] of string[8] =
  ('circle', 'square', 'diamond', 'triup', 'tridown',
    'penta', 'star4', 'star5', 'star6', 'cross', 'dcross',
    'flower5', 'flower4', 'star4arc', 'maltese');

type

  TStar2D = class(TPrimitive2D)
  protected
    fStarKind: TStarKind;
    fStarSizeFactor: TRealType;
  public
    class function GetName: string; override;
    constructor Create(ID: Integer); override;
    constructor CreateSpec(ID: Integer; const P: TPoint2D);
    procedure WhenCreated;
    procedure Assign(const Obj: TGraphicObject); override;
    constructor CreateFromStream(const Stream: TStream); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure FillPieces; override;
    property StarKind: TStarKind read fStarKind write fStarKind;
    property StarSizeFactor: TRealType read fStarSizeFactor write
      fStarSizeFactor;
  end;

  TBox2D0 = class(TPrimitive2D)
  protected
  public
    {: This constructor creates a new 2D frame.

       Parameters:

       <LI=<I=ID> is the object identifier.>
       <LI=<I=P1> is the bottom-left corner of the frame.>
       <LI=<I=P2> is the upper-right corner of the frame.>
       //TSY: P3 is angle setting point
    }
    constructor Create(ID: Integer); override;
    procedure WhenCreated;
    procedure Assign(const Obj: TGraphicObject); override;
    procedure PolyPoints(var PP: TPointsSet2D; T:
      TTransf2D);
    procedure MoveControlPoint0(const Pos: Integer;
      P: TPoint2D; Shift: TShiftState); override;
  end;

  {: This class defines a 2D rectangle.

     The entity has two <I=control points> that are the corner
     points of the rectangle and one point to control the rotation angle.
  }
  TRectangle2D = class(TBox2D0)
  protected
    fRX, fRY: TRealType;
  public
    class function GetName: string; override;
    constructor Create(ID: Integer); override;
    procedure WhenCreated;
    procedure GetRectangleParameters(P0, P1, P2: TPoint2D;
      var P: TPoint2D; var ARot, W, H: TRealType;
      const Eps: TRealType);
    procedure Assign(const Obj: TGraphicObject); override;
    constructor CreateFromStream(const Stream: TStream); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure FillPieces; override;
    property RX: TRealType read fRX write fRX;
    property RY: TRealType read fRY write fRY;
  end;

  {: This class defines a 2D ellipse.
     The ellipse is defined by the box that contains it.
  }
  TEllipse2D = class(TBox2D0)
  protected
  public
    class function GetName: string; override;
    constructor Create(ID: Integer); override;
    procedure WhenCreated;
    procedure FillPieces; override;
    procedure GetEllipseParams(var CP: TPoint2D;
      var RX, RY, ARot: TRealType);
    procedure Assign(const Obj: TGraphicObject); override;
  end;

  TCircle2D = class(TPrimitive2D)
  public
    class function GetName: string; override;
    constructor Create(ID: Integer); override;
    procedure WhenCreated;
    procedure Assign(const Obj: TGraphicObject); override;
    procedure FillPieces; override;
    procedure BreakPath(P: TPoint2D;
      Aperture, Precision: TRealType; var Obj1, Obj2:
      TPrimitive2D);
      override;
  end;

  {: This class defines an arc of a 2D circle.

     The arc is defined by the center, radius,
     and the starting and ending angles of the arc.
  }
  TCircular2D = class(TPrimitive2D)
  protected
    procedure SetRadius(R: TRealType);
    function GetRadius: TRealType;
    procedure SetStartAngle(A: TRealType);
    function GetStartAngle: TRealType;
    procedure SetEndAngle(A: TRealType);
    function GetEndAngle: TRealType;
  public
    procedure GetArcParams(var CX, CY, R, SA, EA:
      TRealType);
    constructor Create(ID: Integer); override;
    constructor CreateSpec(ID: Integer; const CP: TPoint2D; R, SA,
      EA: TRealType);
    procedure WhenCreated;
    procedure FinishFirstDraw; override;
    procedure Assign(const Obj: TGraphicObject); override;
    constructor CreateFromStream(const Stream: TStream); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure FillPieces; override;
    procedure ReversePoints; override;
    procedure MoveControlPoint0(const Pos: Integer; P: TPoint2D;
      Shift: TShiftState); override;
    property Radius: TRealType read GetRadius write
      SetRadius;
    property StartAngle: TRealType read GetStartAngle write
      SetStartAngle;
    property EndAngle: TRealType read GetEndAngle write
      SetEndAngle;
  end;

  TArc2D = class(TCircular2D)
  protected
  public
    class function GetName: string; override;
    procedure BreakPath(P: TPoint2D;
      Aperture, Precision: TRealType; var Obj1, Obj2:
      TPrimitive2D);
      override;
  end;

  TSector2D = class(TCircular2D)
  protected
  public
    class function GetName: string; override;
    constructor Create(ID: Integer); override;
    constructor CreateSpec(ID: Integer; const CP: TPoint2D; R, SA,
      EA: TRealType);
    procedure WhenCreated;
    procedure BreakPath(P: TPoint2D;
      Aperture, Precision: TRealType; var Obj1, Obj2:
      TPrimitive2D);
      override;
  end;

  TSegment2D = class(TCircular2D)
  protected
  public
    class function GetName: string; override;
    constructor Create(ID: Integer); override;
    constructor CreateSpec(ID: Integer; const CP: TPoint2D; R, SA,
      EA: TRealType);
    procedure WhenCreated;
    procedure BreakPath(P: TPoint2D;
      Aperture, Precision: TRealType; var Obj1, Obj2:
      TPrimitive2D);
      override;
  end;

  {: Base class for TBezierPath2D and TSmoothPath2D
  }
  TBezierPrimitive2D = class(TPrimitive2D)
  public
    procedure FillPieces; override;
    procedure ReversePoints; override;
  end;

  // Plain cubic Bezier path

  TBezierPath2D0 = class(TBezierPrimitive2D)
  public
    constructor Create(ID: Integer); override;
    constructor CreateSpec(ID: Integer; const Pts: array of
      TPoint2D);
    procedure WhenCreated;
    procedure Assign(const Obj: TGraphicObject); override;
    procedure BezierPoints(PP: TPointsSet2D); override;
    procedure DrawControlPoints0(const VT: TTransf2D;
      const ClipRect2D: TRect2D; const Width: Integer); override;
  end;

  TBezierPath2D = class(TBezierPath2D0)
  protected
  public
    class function GetName: string; override;
    procedure FinishFirstDraw; override;
    function DeleteControlPoint0(I: Integer): Integer; override;
    function InsertControlPoint0(const Pos: Integer;
      P: TPoint2D; Precision: TRealType): Integer; override;
    procedure MoveControlPoint0(const Pos: Integer; P: TPoint2D;
      Shift: TShiftState); override;
    procedure BreakPath(P: TPoint2D;
      Aperture, Precision: TRealType; var Obj1, Obj2:
      TPrimitive2D);
      override;
  end;

  TClosedBezierPath2D = class(TBezierPath2D0)
  protected
  public
    class function GetName: string; override;
    constructor Create(ID: Integer); override;
    constructor CreateSpec(ID: Integer; const Pts: array of
      TPoint2D);
    procedure WhenCreated;
    procedure FinishFirstDraw; override;
    procedure BezierPoints(PP: TPointsSet2D);
      override;
    function DeleteControlPoint0(I: Integer): Integer; override;
    function InsertControlPoint0(const Pos: Integer;
      P: TPoint2D; Precision: TRealType): Integer; override;
    procedure MoveControlPoint0(const Pos: Integer; P: TPoint2D;
      Shift: TShiftState); override;
    procedure BreakPath(P: TPoint2D;
      Aperture, Precision: TRealType; var Obj1, Obj2:
      TPrimitive2D);
      override;
  end;

  {: Smooth cubic Bezier path - Hobby spline
  }
  TSmoothPath2D0 = class(TBezierPrimitive2D)
  protected
    //function FillProfile: TRect2D; override;
  public
    constructor Create(ID: Integer); override;
    constructor CreateSpec(ID: Integer; const Pts: array of
      TPoint2D);
    procedure WhenCreated;
    procedure Assign(const Obj: TGraphicObject); override;
    procedure BezierPoints(PP: TPointsSet2D); override;
    function InsertControlPoint0(const Pos: Integer;
      P: TPoint2D; Precision: TRealType): Integer; override;
  end;

  TSmoothPath2D = class(TSmoothPath2D0)
  protected
  public
    class function GetName: string; override;
    procedure BreakPath(P: TPoint2D;
      Aperture, Precision: TRealType; var Obj1, Obj2:
      TPrimitive2D);
      override;
  end;

  TClosedSmoothPath2D = class(TSmoothPath2D0)
  protected
  public
    class function GetName: string; override;
    constructor Create(ID: Integer); override;
    constructor CreateSpec(ID: Integer; const Pts: array of
      TPoint2D);
    procedure WhenCreated;
    procedure BezierPoints(PP: TPointsSet2D); override;
    procedure BreakPath(P: TPoint2D;
      Aperture, Precision: TRealType; var Obj1, Obj2:
      TPrimitive2D);
      override;
  end;

  {: This class defines a 2D text object
  }
  TText2D = class(TPrimitive2D)
  protected
    fText: AnsiString;
    fTeXText: AnsiString;
    fHeight: TRealType;
    fRot: TRealType;
    fHJustification: THJustification;
    fVJustification: TVJustification;
    procedure ResetJustification;
    procedure SetHJustification(J: THJustification);
    procedure SetVJustification(J: TVJustification);
    function GetWideText: Widestring;
    function GetFaceName: string;
    function GetStyle: TFontStyles;
    function GetCharset: TFontCharSet;
  public
    Font: TFont; // Win font
    class function GetName: string; override;
    {: Create a new text entity in the rectangle <I=Rect1>, with
       the given <I=Height> and <I=Text>.

       Points[0] will be the left-bottom corner of the text
       bounding box (also used as clipping box for the text)
       and Points[1] the right-up corner.

       The <I=width> of the text is set to the width of <I=Rect1>.
       If you don’t know the dimension of the Text on screen, set
       the <See Property=TText2D@AutoSize> property to <B=True>.
       The first time the text will be drawed the bounding box will
       be adjusted automatically.

       The rectangle will be drawed in the current brush and pen
       if <See Property=TText2D@DrawBox> property is <B=True>.
    }
    constructor Create(ID: Integer); override;
    constructor CreateSpec(ID: Integer; P: TPoint2D; Height:
      TRealType; Txt: AnsiString);
    procedure WhenCreated;
    constructor CreateFromStream(const Stream: TStream); override;
    destructor Destroy; override;
    procedure Assign(const Obj: TGraphicObject); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure FillPieces; override;
    procedure TransForm(const T: TTransf2D); override;
    {: This property contains the heigth of the text in world
       units.
       This is different from the Heigth of font used to render
       the text object.
    }
    property Height: TRealType read fHeight write fHeight;
    property Rot: TRealType read fRot write fRot;
    {: This property contains the text string used by the
       text entity.

       You may include more than one line of text simply
       adding <Code=#10#13> beetwen lines.
    }
    property Text: AnsiString read fText write fText;
    property TeXText: AnsiString read fTeXText write fTeXText;
    property WideText: Widestring read GetWideText;
    property FaceName: string read GetFaceName;
    property Style: TFontStyles read GetStyle;
    property Charset: TFontCharSet read GetCharset;
    {: This property contains the <I=clipping flags> used
       by drawing the text with the <I=DrawText> API function.

       By default the are setted to <I=DT_NOCLIP>.
    }
    property HJustification: THJustification read
      fHJustification write SetHJustification;
    property VJustification: TVJustification read
      fVJustification write SetVJustification;
  end;

  TSymbolKind = (symProcess, symDecision, symInputOutput,
    symPreparation, symPunchCard, symManualOperation, symKeyboard,
    symPunchTape, symDocument,
    symDocuments, symDisplay, symTerminal, symKeying,
    symAlternateProcess,
    symOnlineStorage, symMagneticDrum, symMagneticTape,
    symHoarrow1, symHoarrow1v,
    symHoarrow2, symHoarrow3, symHoarrow4, symStar5,
    symDiamond8, symBaloon1, symBaloon2,
    symCloud1, symSplash1, symSnowFlake1);

const
  SymbolsIDs: array[0..28] of string[15] =
  ('process', 'decision', 'input-output',
    'preparation', 'punch-card', 'manual-op', 'keyboard',
    'punch-tape', 'document',
    'documents', 'display', 'terminal', 'keying', 'alt-process',
    'online-storage', 'magnetic-drum', 'magnetic-tape',
    'hoarrow1', 'hoarrow1v',
    'hoarrow2', 'hoarrow3', 'hoarrow4', 'star5',
    'diamond8', 'baloon1', 'baloon2',
    'cloud1', 'splash1', 'snowflake1');

type

  TSymbol2D = class(TPrimitive2D)
  protected
    fDiameter: TRealType;
    fRot: TRealType;
    fHJustification: THJustification;
    fVJustification: TVJustification;
    fSymbolKind: TSymbolKind;
    procedure SetHJustification(J: THJustification);
    procedure SetVJustification(J: TVJustification);
  public
    class function GetName: string; override;
    constructor Create(ID: Integer); override;
    constructor CreateSpec(ID: Integer; P: TPoint2D;
      Diameter: TRealType);
    procedure WhenCreated;
    constructor CreateFromStream(const Stream: TStream); override;
    procedure Assign(const Obj: TGraphicObject); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure FillPieces; override;
    {function OnMe(P: TPoint2D; Aperture: TRealType; var
      Distance: TRealType): Integer; override;}
    procedure TransForm(const T: TTransf2D); override;
    property Diameter: TRealType read fDiameter write fDiameter;
    property Rot: TRealType read fRot write fRot;
    property SymbolKind: TSymbolKind read
      fSymbolKind write fSymbolKind;
    property HJustification: THJustification read
      fHJustification write SetHJustification;
    property VJustification: TVJustification read
      fVJustification write SetVJustification;
  end;


  {: This class rapresents a scalable raster bitmap.

     This object is useful when you want a bitmap with world
     dimension that is scaled when you zoom in a portion of the
     drawing.

  }
  TBitmap2D = class(TPrimitive2D)
  protected
    fBitmap: TBitmap;
    fScaleFactor: TRealType;
    fAspectRatio: TRealType;
    fCopyMode: TCopyMode;

    procedure SetScaleFactor(SF: TRealType);
    procedure SetAspectRatio(AR: TRealType);
  public
    class function GetName: string; override;
    {: This constructor creates a new bitmap object.

       <I=Bmp> is the bitmap to be drawed and it will be freed
        by the object.

       <I=P1> and <I=P2> are the corner points of the bitmap
       in world coordinates (and the bitmap will be stretched
       to fit in).

       <B=Note>: The bitmap cannot be rotated !
    }
    constructor Create(ID: Integer); override;
    constructor CreateSpec(ID: Integer; const P1, P2: TPoint2D;
      BMP:
      TBitmap);
    procedure WhenCreated;
    destructor Destroy; override;
    procedure Assign(const Obj: TGraphicObject); override;
    constructor CreateFromStream(const Stream: TStream); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure DeviceDraw(Transf: TTransf2D;
      const Dvc: TDevice; const ClipRect2D: TRect2D); override;
    {: This property contains the bitmap to be drawed.

       It will be freed by the object.
    }
    property Bitmap: TBitmap read fBitmap;
    {: This property may contain the scale factor to be used for the
       bitmap.

       If you set this property the bounding box is recomputed and
       the bitmap is not stretched. The first Points will remain
       in the position specified but the second point will be
       repositioned using the ScaleFactor.

       The value rapresent how many drawing unit aorrespond to
       one pixel in the image. So an image of 50x50 pixels with
       a ScaleFactor of 2.0 will be 100x100 drawing units large.
       How many application units (cm, inch, etc) is a drawing
       unit is left to you.

       Setting it to zero (the default) means that no scale is
       needed.
    }
    property ScaleFactor: TRealType read fScaleFactor write
      SetScaleFactor;
    {: This property may contains the aspect ratio (Width/Heigth) to
       be used for the bitmap.

       This property is used only if ScaleFactor is not 0.0

       Setting it to zero (the default) means that no aspect ratio is
       needed.
    }
    property AspectRatio: TRealType read fAspectRatio write
      SetAspectRatio;
    {: This property contains the CopyMode used to copy the
       bitmap.
    }
    property CopyMode: TCopyMode read fCopyMode write fCopyMode;
  end;

procedure GetEllipseParams0(const P0, P1, P2: TPoint2D;
  var P3, P4: TPoint2D;
  var CP: TPoint2D; var RX, RY, ARot: TRealType);
function GetLineStyleString(const LineStyle: TLineStyle): string;

const
  {: This constats rapresent the maximum number of vectorial fonts
     that may be used in the library.
  }
  MAX_REGISTERED_FONTS = 512;

const
  GraphicObjectClasses: array[1..16] of TGraphicObjectClass
  = (TLine2D, TCircle2D, TRectangle2D, TEllipse2D,
    TArc2D, TSector2D, TSegment2D,
    TPolyline2D, TPolygon2D, TSmoothPath2D, TClosedSmoothPath2D,
    TBezierPath2D, TClosedBezierPath2D,
    TText2D, //TBitmap2D,
    TStar2D, TSymbol2D);

implementation

uses Math, Dialogs;

function GetLineStyleString(const LineStyle: TLineStyle): string;
begin
  case LineStyle of
    liNone: Result := 'none';
    liSolid: Result := 'solid';
    liDotted: Result := 'dot';
    liDashed: Result := 'dash';
  end;
end;

// =====================================================================
// TPrimitive2D
// =====================================================================

procedure TPrimitive2D._UpdateExtension;
begin
   { Change the extension. }
  Pieces.Clear;
  FillPieces;
  if Pieces.Count = 0 then
    WritableBBox := Rect2D(0, 0, 0, 0)
  else
    WritableBBox := Pieces.GetBoundingBox;
end;

function TPrimitive2D.CreateVect(const Size: Integer):
  TEPointsSet2D;
begin
  Result := TEPointsSet2D.Create(Size);
end;

constructor TPrimitive2D.Create(ID: Integer);
begin
  inherited Create(ID);
  { Create the internal vector. }
  fPoints := CreateVect(10);
  WhenCreated;
end;

constructor TPrimitive2D.CreateSpec(ID: Integer; NPts: Integer;
  CurvePrec: Word);
begin
  inherited Create(ID);
  { Create the internal vector. }
  fPoints := CreateVect(NPts);
  WhenCreated;
end;

procedure TPrimitive2D.WhenCreated;
begin
  if fPoints <> nil then
    fPoints.OnChange := UpdateExtension;
  fHatching := haNone;
  fLineStyle := liSolid;
  fLineWidth := 1;
  fLineColor := clDefault;
  fHatchColor := clDefault;
  fFillColor := clDefault;
  fCanDeletePoints := False;
  fDrawPathBezier := True;
  Pieces := TArrayOfPiece.Create(1);
  FirstDrawPoint := -1;
  fOwnsInterior := True;
  fArrowSizeFactor := 1;
end;

procedure TPrimitive2D.FinishFirstDraw;
begin
  FirstDrawPoint := -2;
end;

destructor TPrimitive2D.Destroy;
begin
  fPoints.Free;
  Pieces.Free;
  inherited Destroy;
end;

procedure TPrimitive2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then Exit;
  inherited Assign(Obj);
  if not Assigned(Pieces) then
    Pieces := TArrayOfPiece.Create(1);
  if (Obj is TPrimitive2D) then
  begin // Per default non aggiunge i punti.
    if not Assigned(fPoints) then
    begin
      fPoints := CreateVect(0);
      fPoints.GrowingEnabled := True;
      fPoints.OnChange := UpdateExtension;
    end;
    fPoints.Clear;
    fHatching := (Obj as TPrimitive2D).fHatching;
    fLineStyle := (Obj as TPrimitive2D).fLineStyle;
    fLineWidth := (Obj as TPrimitive2D).fLineWidth;
    fLineColor := (Obj as TPrimitive2D).fLineColor;
    fHatchColor := (Obj as TPrimitive2D).fHatchColor;
    fFillColor := (Obj as TPrimitive2D).fFillColor;
    fCanDeletePoints := (Obj as TPrimitive2D).fCanDeletePoints;
    fDrawPathBezier := (Obj as TPrimitive2D).fDrawPathBezier;
    FirstDrawPoint := -1;
    fOwnsInterior := (Obj as TPrimitive2D).fOwnsInterior;
    fBeginArrowKind := (Obj as TPrimitive2D).fBeginArrowKind;
    fEndArrowKind := (Obj as TPrimitive2D).fEndArrowKind;
    fArrowSizeFactor := (Obj as TPrimitive2D).fArrowSizeFactor;
    //WhenCreated;
  end;
end;

constructor TPrimitive2D.CreateFromStream(const Stream: TStream);
var
  TmpN: Longint;
  Cont: Integer;
  TmpPt: TPoint2D;
  TmpBoolean: Boolean;
begin
  { Load the standard properties }
  inherited;
  WhenCreated;
  with Stream do
  begin
    Read(TmpN, SizeOf(TmpN));
    fPoints := CreateVect(TmpN);
     { Read all the points. }
    for Cont := 0 to TmpN - 1 do
    begin
      Read(TmpPt, SizeOf(TmpPt));
      fPoints.Points[Cont] := TmpPt;
    end;
    Read(TmpBoolean, SizeOf(TmpBoolean));
    fPoints.GrowingEnabled := TmpBoolean;
    Read(fHatching, SizeOf(fHatching));
    Read(fLineStyle, SizeOf(fLineStyle));
    Read(fLineWidth, SizeOf(fLineWidth));
    Read(fLineColor, SizeOf(fLineColor));
    Read(fHatchColor, SizeOf(fHatchColor));
    Read(fFillColor, SizeOf(fFillColor));
    Read(fCanDeletePoints, SizeOf(fCanDeletePoints));
    Read(fDrawPathBezier, SizeOf(fDrawPathBezier));
    Read(fOwnsInterior, SizeOf(fOwnsInterior));
    Read(fBeginArrowKind, SizeOf(fBeginArrowKind));
    Read(fEndArrowKind, SizeOf(fEndArrowKind));
    Read(fArrowSizeFactor, SizeOf(fArrowSizeFactor));
  end;
  fPoints.OnChange := UpdateExtension;
end;

procedure TPrimitive2D.SaveToStream(const Stream: TStream);
var
  TmpN: Longint;
  Cont: Integer;
  TmpPt: TPoint2D;
  TmpBoolean: Boolean;
begin
  { Save the standard properties }
  inherited SaveToStream(Stream);
  with Stream do
  begin
    TmpN := fPoints.Count;
    Write(TmpN, SizeOf(TmpN));
     { Write all points. }
    for Cont := 0 to TmpN - 1 do
    begin
      TmpPt := fPoints.Points[Cont];
      Write(TmpPt, SizeOf(TmpPt));
    end;
    TmpBoolean := fPoints.GrowingEnabled;
    Write(TmpBoolean, SizeOf(TmpBoolean));
    Write(fHatching, SizeOf(fHatching));
    Write(fLineStyle, SizeOf(fLineStyle));
    Write(fLineWidth, SizeOf(fLineWidth));
    Write(fLineColor, SizeOf(fLineColor));
    Write(fHatchColor, SizeOf(fHatchColor));
    Write(fFillColor, SizeOf(fFillColor));
    Write(fCanDeletePoints, SizeOf(fCanDeletePoints));
    Write(fDrawPathBezier, SizeOf(fDrawPathBezier));
    Write(fOwnsInterior, SizeOf(fOwnsInterior));
    Write(fBeginArrowKind, SizeOf(fBeginArrowKind));
    Write(fEndArrowKind, SizeOf(fEndArrowKind));
    Write(fArrowSizeFactor, SizeOf(fArrowSizeFactor));
  end;
end;

function TPrimitive2D.GetArrowSize: TRealType;
begin
  if ParentDrawing is TDrawing2D then
    Result := (ParentDrawing as TDrawing2D).ArrowsSize
  else
    Result := 1;
  Result := Abs(Result * fArrowSizeFactor);
end;

procedure TPrimitive2D.FillPieces;
begin

end;

procedure TPrimitive2D.BezierPoints(PP: TPointsSet2D);
var
  P: TPoint2D;
begin
  if Pieces.Count = 0 then
    LinPolyToBezier(fPoints, True, PP)
  else if Pieces[0] is TLinPath then
    LinPolyToBezier(Pieces[0], (Pieces[0] as TLinPath).Closed, PP)
  else
  begin
    Pieces[0].BezierPoints(PP);
    if PP.Count > 0 then
    begin
      P := PP[PP.Count - 1];
      if Pieces[0].Closed and
        not IsSamePoint2D(PP[0], P) then
      begin
        P := MidPoint(PP[0], P);
        PP.AddPoints([P, P, PP[0]]);
      end;
    end;
  end;
end;

procedure TPrimitive2D.DeviceDrawPiece(Piece: TPiece;
  const Transf: TTransf2D; const Dvc: TDevice;
  const ClipRect2D: TRect2D);
begin
  Dvc.PieceCh(Piece, Piece.GetLineColor(Self),
    Piece.GetHatchColor(Self),
    Piece.GetFillColor(Self), Piece.GetLineStyle(Self),
    Piece.GetLineWidth(Self), Piece.GetHatching(Self),
    Transf);
end;

procedure TPrimitive2D.DeviceDrawPieces(const VT: TTransf2D;
  const Dvc: TDevice; const ClipRect2D: TRect2D);
var
  I: Integer;
begin
  for I := 0 to Pieces.Count - 1 do
    if Pieces[I] <> nil then
      DeviceDrawPiece(Pieces[I], VT, Dvc, ClipRect2D);
end;

procedure TPrimitive2D.DrawControlPoints(const VT: TTransf2D;
  const ClipRect2D: TRect2D; const Width: Integer);
begin
  inherited DrawControlPoints(VT, ClipRect2D, Width);
  DrawControlPoints0(VT, ClipRect2D, Width);
end;

procedure TPrimitive2D.DrawControlPoints0(const VT: TTransf2D;
  const ClipRect2D: TRect2D; const Width: Integer);
var
  I: Integer;
begin
  for I := 0 to Points.Count - 1 do
    (OwnerDrawing as TDrawing2D).OnControlPoint(Points[I], VT,
      ClipRect2D);
end;

procedure TPrimitive2D.DeviceDraw(Transf: TTransf2D;
  const Dvc: TDevice; const ClipRect2D: TRect2D);
begin
  DeviceDrawPieces(Transf, Dvc, ClipRect2D);
end;

function TPrimitive2D.OnMe(P: TPoint2D; Aperture: TRealType;
  var Distance: TRealType): Integer;
var
  I: Integer;
  ResDist: TRealType;
  Pos: Integer;
  function _OnMe: Integer;
  begin
    Result := PICK_NOOBJECT;
    if Pieces.IsPointInside(Self, P, Aperture)
      then Result := PICK_INOBJECT;
    Distance := Aperture;
    if Pieces.IsPointOnStroke(
      Self, P, Aperture, Distance, Pos) then
      if Result = PICK_NOOBJECT
        then Result := PICK_ONOBJECT
      else Result := PICK_ONINOBJECT;
  end;
begin
  Result := inherited OnMe(P, Aperture, Distance);
  for I := 0 to Points.Count - 1 do
    if NearPoint2D(P, Points[I], Aperture, ResDist)
      and
      (ResDist <= Distance) then
    begin
      Result := I;
      Distance := ResDist;
    end;
  if Result = PICK_INBBOX then
  begin
    Result := _OnMe;
  end;
  if (Result >= PICK_ONOBJECT) and
    (fLineStyle <> liNone) and (Distance <= Self.fLineWidth / 2)
    then
    Distance := 0;
end;

procedure TPrimitive2D.TransForm(const T: TTransf2D);
begin
  fPoints.TransformPoints(T);
  if ScaleLineWidth then
    LineWidth := LineWidth * IsotropicScale(T);
end;

procedure TPrimitive2D.ReversePoints;
begin

end;

function TPrimitive2D.DeleteControlPoint0(I: Integer): Integer;
begin
  Result := 0;
  if (I < 0) or (I > fPoints.Count - 1) then Exit;
  fPoints.Delete(I);
  Inc(Result);
end;

procedure TPrimitive2D.DeleteControlPoint(const I: Integer);
begin
  if not fCanDeletePoints then Exit;
  if DeleteControlPoint0(I) > 0 then
    (OwnerDrawing as TDrawing2D).NotifyChanged;
end;

function TPrimitive2D.InsertControlPoint0(const Pos: Integer;
  P: TPoint2D; Precision: TRealType): Integer;
begin
  Result := 0;
  if (Pos < 0) or (Pos >= fPoints.Count) then Exit;
  if Pos < fPoints.Count - 1 then
  begin
    P := PointSegmentProj2D(P, fPoints[Pos], fPoints[Pos + 1]);
    fPoints.Insert(Pos + 1, P);
  end
  else
  begin
    P := PointSegmentProj2D(P, fPoints[fPoints.Count - 1],
      fPoints[0]);
    fPoints.Add(P);
  end;
  Inc(Result);
end;

function TPrimitive2D.InsertControlPoint(P: TPoint2D;
  Aperture, Precision: TRealType): Boolean;
var
  I: Integer;
  Distance: TRealType;
begin
  Result := False;
  if not fCanDeletePoints then Exit;
  Distance := Aperture;
  if not Pieces.IsPointOnStroke(
    Self, P, Precision, Distance, I) then Exit;
  Result := InsertControlPoint0(I, P, Precision) > 0;
  if Result then (OwnerDrawing as TDrawing2D).NotifyChanged;
end;

procedure TPrimitive2D.MoveControlPoint0(const Pos: Integer;
  P: TPoint2D; Shift: TShiftState);
begin
  fPoints[Pos] := P;
end;

procedure TPrimitive2D.BreakPath(P: TPoint2D;
  Aperture, Precision: TRealType; var Obj1, Obj2: TPrimitive2D);
begin
  Obj1 := nil;
  Obj2 := nil;
end;

// =====================================================================
// TLine2D
// =====================================================================

type

  TSimplePoint2D = array[1..2] of TRealType;
  TSimplePointArr2D = array[0..0] of TSimplePoint2D;
  PSimplePointArr2D = ^TSimplePointArr2D;
  //TSimplePointArr2D = array of TSimplePoint2D;

const
  ArrH40Arr: array[1..5] of TSimplePoint2D
  = ((0, 0), (4, -1), (0, 0), (4, 1), (0, 0));
  ArrH41Arr: array[1..5] of TSimplePoint2D
  = ((0, 0), (4, -1), (1, 0), (4, 1), (0, 0));
  ArrH42Arr: array[1..5] of TSimplePoint2D
  = ((0, 0), (4, -1), (2, 0), (4, 1), (0, 0));
  ArrH43Arr: array[1..5] of TSimplePoint2D
  = ((0, 0), (4, -1), (3, 0), (4, 1), (0, 0));
  ArrH44Arr: array[1..4] of TSimplePoint2D
  = ((0, 0), (4, -1), (4, 1), (0, 0));
  ArrH45Arr: array[1..5] of TSimplePoint2D
  = ((0, 0), (4, -1), (5, 0), (4, 1), (0, 0));
  ArrH46Arr: array[1..5] of TSimplePoint2D
  = ((0, 0), (4, -1), (6, 0), (4, 1), (0, 0));
  ArrH47Arr: array[1..5] of TSimplePoint2D
  = ((0, 0), (4, -1), (7, 0), (4, 1), (0, 0));
  ArrH48Arr: array[1..5] of TSimplePoint2D
  = ((0, 0), (4, -1), (8, 0), (4, 1), (0, 0));
  ArrT40Arr: array[1..5] of TSimplePoint2D
  = ((0, 0), (-4, -1), (0, 0), (-4, 1), (0, 0));
  ArrT43Arr: array[1..5] of TSimplePoint2D
  = ((0, 0), (-1, -1), (3, 0), (-1, 1), (0, 0));
  ArrT44Arr: array[1..4] of TSimplePoint2D
  = ((0, -1), (4, 0), (0, 1), (0, -1));
  ArrT45Arr: array[1..5] of TSimplePoint2D
  = ((0, 0), (1, -1), (5, 0), (1, 1), (0, 0));
  ArrH20Arr: array[1..5] of TSimplePoint2D
  = ((0, 0), (2, -1), (0, 0), (2, 1), (0, 0));
  ArrH21Arr: array[1..5] of TSimplePoint2D
  = ((0, 0), (2, -1), (1, 0), (2, 1), (0, 0));
  ArrH22Arr: array[1..4] of TSimplePoint2D
  = ((0, 0), (2, -1), (2, 1), (0, 0));
  ArrH23Arr: array[1..5] of TSimplePoint2D
  = ((0, 0), (2, -1), (3, 0), (2, 1), (0, 0));
  ArrH24Arr: array[1..5] of TSimplePoint2D
  = ((0, 0), (2, -1), (4, 0), (2, 1), (0, 0));
  ArrT20Arr: array[1..5] of TSimplePoint2D
  = ((0, 0), (-2, -1), (0, 0), (-2, 1), (0, 0));
  ArrT21Arr: array[1..5] of TSimplePoint2D
  = ((0, 0), (-1, -1), (1, 0), (-1, 1), (0, 0));
  ArrT22Arr: array[1..4] of TSimplePoint2D
  = ((0, -1), (2, 0), (0, 1), (0, -1));
  ArrT23Arr: array[1..5] of TSimplePoint2D
  = ((0, 0), (1, -1), (3, 0), (1, 1), (0, 0));
  ArrHR10Arr: array[1..5] of TSimplePoint2D
  = ((0, 0), (1.73, -1), (0, 0), (1.73, 1), (0, 0));
  ArrHR11Arr: array[1..4] of TSimplePoint2D
  = ((0, 0), (1.73, -1), (1.73, 1), (0, 0));
  ArrHR12Arr: array[1..5] of TSimplePoint2D
  = ((0, 0), (1.73, -1), (3.46, 0), (1.73, 1), (0, 0));
  ArrTR10Arr: array[1..5] of TSimplePoint2D
  = ((0, 0), (-1.73, -1), (0, 0), (-1.73, 1), (0, 0));
  ArrH10Arr: array[1..5] of TSimplePoint2D
  = ((0, 0), (1, -1), (0, 0), (1, 1), (0, 0));
  ArrH11Arr: array[1..4] of TSimplePoint2D
  = ((0, 0), (1, -1), (1, 1), (0, 0));
  ArrH12Arr: array[1..5] of TSimplePoint2D
  = ((0, 0), (1, -1), (2, 0), (1, 1), (0, 0));
  ArrH12CArr: array[1..5] of TSimplePoint2D
  = ((-1, 0), (0, -1), (1, 0), (0, 1), (-1, 0));
  ArrT10Arr: array[1..5] of TSimplePoint2D
  = ((0, 0), (-1, -1), (0, 0), (-1, 1), (0, 0));
  ArrR0Arr: array[1..2] of TSimplePoint2D
  = ((0, -1), (0, 1));
  ArrR10Arr: array[1..5] of TSimplePoint2D
  = ((0, -1), (1, -1), (1, 1), (0, 1), (0, -1));
  ArrR11Arr: array[1..7] of TSimplePoint2D
  = ((0, -1), (1, -1), (1, 1), (0, 1), (1, 1), (1, -1), (0, -1));
  ArrR12Arr: array[1..9] of TSimplePoint2D
  = ((0, -1), (0, 0), (1, 0), (1, -1), (1, 1), (1, 0), (0, 0), (0,
    1), (0, -1));
  ArrR20Arr: array[1..5] of TSimplePoint2D
  = ((0, -1), (2, -1), (2, 1), (0, 1), (0, -1));
  ArrR20CArr: array[1..5] of TSimplePoint2D
  = ((-1, -1), (1, -1), (1, 1), (-1, 1), (-1, -1));
  ArrR21Arr: array[1..7] of TSimplePoint2D
  = ((0, -1), (2, -1), (2, 1), (0, 1), (2, 1), (2, -1), (0, -1));
  ArrR33Arr: array[1..15] of TSimplePoint2D
  = ((0, -1), (0, 0), (1, 0), (1, -1), (1, 0),
    (2, 0), (2, -1), (2, 1), (2, 0),
    (1, 0), (1, 1), (1, 0), (0, 0), (0, 1), (0, -1));
  ArrTS10Arr: array[1..7] of TSimplePoint2D
  = ((0, 0), (-1, -1), (0, -1), (1, 0), (0, 1), (-1, 1), (0, 0));
  ArrTS11Arr: array[1..9] of TSimplePoint2D
  = ((1, -1), (1, -1), (2, 0), (1, 1), (0, 1),
    (1, 1), (2, 0), (1, -1), (0, -1));
  ArrTS12Arr: array[1..11] of TSimplePoint2D
  = ((0, 0), (-1, -1), (0, 0), (1, 0), (0, -1), (1, 0),
    (0, 1), (1, 0), (0, 0), (-1, 1), (0, 0));
  ArrHS10Arr: array[1..7] of TSimplePoint2D
  = ((0, 0), (1, -1), (2, -1), (1, 0), (2, 1), (1, 1), (0, 0));
  ArrHS12Arr: array[1..13] of TSimplePoint2D
  = ((0, 0), (1, -1), (0.1, -0.1), (0.1, 0), (1, 0), (2, -1), (1,
    0),
    (2, 1), (1, 0), (0.1, 0), (0.1, 0.1), (1, 1), (0, 0));
  ArrTS20Arr: array[1..7] of TSimplePoint2D
  = ((0, 0), (-1, -1), (1, -1), (2, 0), (1, 1), (-1, 1), (0, 0));
  ArrTS21Arr: array[1..9] of TSimplePoint2D
  = ((-1, -1), (1, -1), (2, 0), (1, 1), (-1, 1),
    (1, 1), (2, 0), (1, -1), (-1, -1));
  ArrTS23Arr: array[1..17] of TSimplePoint2D
  = ((0, 0), (-1, -1), (0, 0), (1, 0), (0, -1), (1, 0),
    (2, 0), (1, -1), (2, 0), (1, 1), (2, 0),
    (1, 0), (0, 1), (1, 0), (0, 0), (-1, 1), (0, 0));
  ArrHS20Arr: array[1..7] of TSimplePoint2D
  = ((0, 0), (1, -1), (3, -1), (2, 0), (3, 1), (1, 1), (0, 0));
  ArrHS23Arr: array[1..19] of TSimplePoint2D
  = ((0, 0), (1, -1), (0.1, -0.1), (0.1, 0), (1, 0), (2, -1),
    (1, 0), (2, 0), (3, -1), (2, 0), (3, 1), (2, 0),
    (1, 0), (2, 1), (1, 0), (0.1, 0), (0.1, 0.1), (1, 1), (0, 0));

class function TLine2D.GetName: string;
begin
  Result := 'Line';
end;

constructor TLine2D.Create(ID: Integer);
begin
  inherited CreateSpec(ID, 2, 50);
  WhenCreated;
end;

constructor TLine2D.CreateSpec(ID: Integer; const P1, P2:
  TPoint2D);
begin
  inherited CreateSpec(ID, 2, 50);
  fPoints.DisableEvents := True;
  try
    fPoints.Add(P1);
    fPoints.Add(P2);
  finally
    fPoints.DisableEvents := False;
    UpdateExtension(Self);
  end;
  WhenCreated;
end;

procedure TLine2D.WhenCreated;
begin
  fDrawPathBezier := False;
  fPoints.GrowingEnabled := False;
end;

procedure TLine2D.Assign(const Obj: TGraphicObject);
var
  RX, RY, ARot: TRealType;
  CP: TPoint2D;
begin
  if (Obj = Self) then Exit;
  inherited Assign(Obj);
  if Obj is TPrimitive2D then
  begin
    fPoints.DisableEvents := True;
    if Obj is TEllipse2D then
    begin
      CP.W := 1;
      (Obj as TEllipse2D).GetEllipseParams(
        CP, RX, RY, ARot);
      if RY > RX then
      begin
        ARot := ARot + Pi / 2;
        RX := RY;
      end;
      fPoints.Clear;
      fPoints.AddPoints([
        ShiftPoint(CP, PolarVector(RX, ARot)),
          ShiftPoint(CP, PolarVector(-RX, ARot))]);
    end
    else if (Obj as TPrimitive2D).fPoints.Count > 1 then
    begin
      //fPoints.Copy((Obj as TPrimitive2D).fPoints, 0, 1)
      fPoints.Copy((Obj as TPrimitive2D).fPoints, 0, 0);
      fPoints[1] :=
        (Obj as TPrimitive2D).fPoints[(Obj as
        TPrimitive2D).fPoints.Count - 1];
    end
    else if (Obj as TPrimitive2D).fPoints.Count = 1 then
    begin
      fPoints.Copy((Obj as TPrimitive2D).fPoints, 0, 0);
      fPoints[1] := Point2D(fPoints[0].X, fPoints[0].Y + 1);
    end;
    fPoints.DisableEvents := False;
    fPoints.GrowingEnabled := False;
  end;
  UpdateExtension(Self);
end;

constructor TLine2D.CreateFromStream(const Stream: TStream);
begin
  inherited;
  WhenCreated;
  with Stream do
  begin
  end;
end;

procedure TLine2D.SaveToStream(const Stream: TStream);
begin
  inherited SaveToStream(Stream);
  with Stream do
  begin
  end;
end;

type
  ArrowProc = procedure(const Arr: array of TSimplePoint2D;
    var Data: Pointer);

function GetArrowArray(const Kind: TArrowKind;
  const AProc: ArrowProc; var Data: Pointer): Boolean;
begin
  Result := True;
  case Kind of
    arrH40: AProc(ArrH40Arr, Data);
    arrH41: AProc(ArrH41Arr, Data);
    arrH42: AProc(ArrH42Arr, Data);
    arrH43: AProc(ArrH43Arr, Data);
    arrH44: AProc(ArrH44Arr, Data);
    arrH45: AProc(ArrH45Arr, Data);
    arrH46: AProc(ArrH46Arr, Data);
    arrH47: AProc(ArrH47Arr, Data);
    arrH48: AProc(ArrH48Arr, Data);
    arrT40: AProc(ArrT40Arr, Data);
    arrT43: AProc(ArrT43Arr, Data);
    arrT44: AProc(ArrT44Arr, Data);
    arrT45: AProc(ArrT45Arr, Data);
    arrH20: AProc(ArrH20Arr, Data);
    arrH21: AProc(ArrH21Arr, Data);
    arrH22: AProc(ArrH22Arr, Data);
    arrH23: AProc(ArrH23Arr, Data);
    arrH24: AProc(ArrH24Arr, Data);
    arrT20: AProc(ArrT20Arr, Data);
    arrT21: AProc(ArrT21Arr, Data);
    arrT22: AProc(ArrT22Arr, Data);
    arrT23: AProc(ArrT23Arr, Data);
    arrHR10: AProc(ArrHR10Arr, Data);
    arrHR11: AProc(ArrHR11Arr, Data);
    arrHR12: AProc(ArrHR12Arr, Data);
    arrTR10: AProc(ArrTR10Arr, Data);
    arrH10: AProc(ArrH10Arr, Data);
    arrH11: AProc(ArrH11Arr, Data);
    arrH12: AProc(ArrH12Arr, Data);
    arrH12C: AProc(ArrH12CArr, Data);
    arrT10: AProc(ArrT10Arr, Data);
    arrR0: AProc(ArrR0Arr, Data);
    arrR10: AProc(ArrR10Arr, Data);
    arrR11: AProc(ArrR11Arr, Data);
    arrR12: AProc(ArrR12Arr, Data);
    arrR20: AProc(ArrR20Arr, Data);
    arrR20C: AProc(ArrR20CArr, Data);
    arrR21: AProc(ArrR21Arr, Data);
    arrR33: AProc(ArrR33Arr, Data);
    arrTS10: AProc(ArrTS10Arr, Data);
    arrTS11: AProc(ArrTS11Arr, Data);
    arrTS12: AProc(ArrTS12Arr, Data);
    arrHS10: AProc(ArrHS10Arr, Data);
    arrHS12: AProc(ArrHS12Arr, Data);
    arrTS20: AProc(ArrTS20Arr, Data);
    arrTS21: AProc(ArrTS21Arr, Data);
    arrTS23: AProc(ArrTS23Arr, Data);
    arrHS20: AProc(ArrHS20Arr, Data);
    arrHS23: AProc(ArrHS23Arr, Data);
  else
    Result := False;
  end;
end;

procedure FillLinArrow(const Arr: array of TSimplePoint2D;
  var Data: Pointer);
var
  APiece: TPiece;
  I: Integer;
begin
  APiece := TLinPath.Create(Length(Arr));
  Data := APiece;
  for I := 0 to Length(Arr) - 1 do
    APiece.Add(Point2D(Arr[I][1], Arr[I][2]));
end;

procedure MeasureLinArrow0(
  const Arr: array of TSimplePoint2D;
  var Data: Pointer);
var
  I: Integer;
begin
  TRealType(Data^) := 0;
  for I := 0 to Length(Arr) - 1 do
    if Arr[I][1] > TRealType(Data^)
      then TRealType(Data^) := Arr[I][1];
  if TRealType(Data^) = 0 then
    TRealType(Data^) := 0.1;
end;

function MeasureArrow(const Kind: TArrowKind): TRealType;
var
  P: Pointer;
begin
  Result := 0;
  P := @Result;
  if not GetArrowArray(Kind, MeasureLinArrow0, P) then
    case Kind of
      arrQQ: Result := 3;
      arrOC: Result := 1;
    else //arrO
      Result := 2;
    end;
end;

procedure FillArrow(const Kind: TArrowKind;
  const P0, P1: TPoint2D; const ArrowSize: TRealType;
  Pieces: TArrayOfPiece);
var
  APiece: TPiece;
  P: Pointer;
  V: TVector2D;
  T: TTransf2D;
begin
  if Kind = arrNone then Exit;
  V := Direction2D(P0, P1);
  if GetArrowArray(Kind, FillLinArrow, P) then
  begin
    APiece := P;
    APiece.Closed :=
      IsSamePoint2D(APiece[0], APiece[APiece.Count - 1]);
  end
  else
    case Kind of
      arrQQ: APiece := Pieces.AddFromSvgPath(
          'M 0 0 L 1 -0.5 L 1.5 -1.5 L 2 -0.5 L 3 0 L 2 0.5 L 1.5 1.5 L 1 0.5 Z');
      arrOC:
        begin
        //APiece := TLinPath.Create(31);
        //LinearizeCircle(APiece, Point2D(0, 0), 1 {radius}, 30);
          APiece := TCirclePiece.Create(1);
          APiece.Add(Point2D(0, 0));
          (APiece as TCirclePiece).R := 1;
        end;
    else //arrO
      begin
      //APiece := TLinPath.Create(31);
      //LinearizeCircle(APiece, Point2D(ArrowSize, 0), 1, 30);
        APiece := TCirclePiece.Create(1);
        APiece.Add(Point2D(1, 0));
        (APiece as TCirclePiece).R := 1;
      end;
    end;
  T := IdentityTransf2D;
  T[1, 1] := V.X * ArrowSize;
  T[1, 2] := V.Y * ArrowSize;
  T[2, 1] := -V.Y * ArrowSize;
  T[2, 2] := V.X * ArrowSize;
  T[3, 1] := P0.X;
  T[3, 2] := P0.Y;
  if not (Kind = arrQQ) then
    Pieces.Add(APiece);
  APiece.TransformPoints(T);
  APiece.Line := pliSolidDefault;
  APiece.Fill := pfiLineAsDefault;
  APiece.Hatch := phaNone;
end;

procedure TLine2D.FillPieces;
var
  P0, P1: TPoint2D;
  APiece: TLinPath;
begin
  P0 := fPoints[0];
  P1 := fPoints[1];
  APiece := TLinPath.Create(2);
  Pieces.Add(APiece);
  APiece.Add(P0);
  APiece.Add(P1);
  APiece.Fill := pfiNone;
  if PointDistance2D(P0, P1) = 0 then Exit;
  FillArrow(fBeginArrowKind, P0, P1, ArrowSize, Pieces);
  FillArrow(fEndArrowKind, P1, P0, ArrowSize, Pieces);
end;

procedure TLine2D.ReversePoints;
begin
  fPoints.ReversePoints;
  UpdateExtension(Self);
end;

procedure TLine2D.BreakPath(P: TPoint2D;
  Aperture, Precision: TRealType; var Obj1, Obj2: TPrimitive2D);
var
  PNew: TPoint2D;
begin
  Obj1 := nil;
  Obj2 := nil;
  PNew := PointSegmentProj2D(P, fPoints[0], fPoints[1]);
  if PointDistance2D(P, PNew) > Aperture then Exit;
  if IsSamePoint2D(PNew, fPoints[0]) then Exit;
  if IsSamePoint2D(PNew, fPoints[1]) then Exit;
  if Aperture = 0 then Exit;
  Obj1 := TLine2D.Create(-1);
  Obj1.Assign(Self);
  Obj1.fPoints[1] := PNew;
  Obj2 := TLine2D.Create(-1);
  Obj2.Assign(Self);
  Obj2.fPoints[0] := PNew;
end;

// =====================================================================
// TPolyline2D0
// =====================================================================

constructor TPolyline2D0.Create(ID: Integer);
begin
  inherited Create(ID);
  WhenCreated;
end;

constructor TPolyline2D0.CreateSpec(ID: Integer; const Pts: array
  of
  TPoint2D);
begin
  inherited CreateSpec(ID, High(Pts) - Low(Pts) + 1, 50);
  fPoints.AddPoints(Pts);
  WhenCreated;
end;

procedure TPolyline2D0.WhenCreated;
begin
  fOwnsInterior := False;
  fCanDeletePoints := True;
  fDrawPathBezier := False;
end;

procedure TPolyline2D0.FillPieces;
var
  APath: TLinPath;
begin
  APath := TLinPath.Create(fPoints.Count);
  Pieces.Add(APath);
  APath.Line := pliDefault;
  APath.Fill := pfiDefault;
  APath.Hatch := phaDefault;
  APath.Closed := False;
  APath.Copy(fPoints, 0, fPoints.Count - 1);
end;

procedure TPolyline2D0.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then Exit;
  inherited Assign(Obj);
  if Obj is TPrimitive2D then
  begin
    fPoints.DisableEvents := True;
    fPoints.Clear;
    if (Obj is TPrimitive2D) and not (Obj is TPolyline2D0)
      and ((Obj as TPrimitive2D).Pieces.Count > 0) then
    begin
      (Obj as TPrimitive2D).Pieces[0].Linearize(fPoints);
      if ((Obj as TPrimitive2D).Pieces[0].Closed)
        and (fPoints.Count > 0)
        and not IsSamePoint2D(
        fPoints[0], fPoints[fPoints.Count - 1])
        then fPoints.Add(fPoints[0]);
    end
    else
      fPoints.Copy((Obj as TPrimitive2D).fPoints, 0,
        (Obj as TPrimitive2D).fPoints.Count - 1);
    fPoints.DisableEvents := False;
    fPoints.GrowingEnabled := True;
  end;
  WhenCreated;
  UpdateExtension(Self);
end;

procedure TPolyline2D0.ReversePoints;
begin
  fPoints.ReversePoints;
  UpdateExtension(Self);
end;

// =====================================================================
// TPolyline2D
// =====================================================================

class function TPolyline2D.GetName: string;
begin
  Result := 'Polyline';
end;

procedure TPolyline2D.FillPieces;
begin
  inherited FillPieces;
  if fPoints.Count < 2 then Exit;
  FillArrow(fBeginArrowKind, fPoints[0], fPoints[1], ArrowSize,
    Pieces);
  FillArrow(fEndArrowKind,
    fPoints[fPoints.Count - 1], fPoints[fPoints.Count - 2],
    ArrowSize, Pieces);
end;

procedure TPolyline2D.BreakPath(P: TPoint2D;
  Aperture, Precision: TRealType; var Obj1, Obj2: TPrimitive2D);
var
  I, J: Integer;
begin
  Obj1 := nil;
  Obj2 := nil;
  if not fPoints.IsPointOnPolylineStroke(
    P, False, Aperture, I) then Exit;
  if I < 0 then Exit;
  if Aperture = 0 then
    if I = 0 then
      Exit
    else
      Dec(I)
  else if not InsertControlPoint0(I, P, Precision) > 0 then
    Exit;
  Obj1 := TPolyline2D.Create(-1);
  Obj1.Assign(Self);
  Obj1.fPoints.Clear;
  Obj1.fPoints.Copy(fPoints, 0, I + 1);
  Obj2 := TPolyline2D.Create(-1);
  Obj2.Assign(Self);
  Obj2.fPoints.Clear;
  for J := I + 1 to fPoints.Count - 1 do
    Obj2.fPoints.Add(fPoints[J]);
end;

// =====================================================================
// TPolygon2D
// =====================================================================

class function TPolygon2D.GetName: string;
begin
  Result := 'Polygon';
end;

constructor TPolygon2D.Create(ID: Integer);
begin
  inherited Create(ID);
  WhenCreated;
end;

constructor TPolygon2D.CreateSpec(ID: Integer; const Pts: array
  of
  TPoint2D);
begin
  inherited CreateSpec(ID, Pts);
  WhenCreated;
end;

procedure TPolygon2D.WhenCreated;
begin
end;

procedure TPolygon2D.FillPieces;
begin
  inherited FillPieces;
  Pieces[0].Closed := True;
end;

procedure TPolygon2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then Exit;
  inherited Assign(Obj);
  WhenCreated;
end;

procedure TPolygon2D.BreakPath(P: TPoint2D;
  Aperture, Precision: TRealType; var Obj1, Obj2: TPrimitive2D);
var
  I, J: Integer;
begin
  Obj1 := nil;
  Obj2 := nil;
  if not fPoints.IsPointOnPolylineStroke(
    P, True, Aperture, I) then Exit;
  if I < 0 then Exit;
  if Aperture = 0 then
    Dec(I)
  else if not InsertControlPoint0(I, P, Precision) > 0 then
    Exit;
  Obj1 := TPolyline2D.Create(-1);
  Obj1.Assign(Self);
  Obj1.fPoints.Clear;
  for J := I + 1 to fPoints.Count - 1 do
    Obj1.fPoints.Add(fPoints[J]);
  for J := 0 to I + 1 do
    Obj1.fPoints.Add(fPoints[J]);
  Obj2 := nil;
end;

// =====================================================================
// TStar2D
// =====================================================================

const
{starCircle, starSquare, starDiamond, starTriUp, starTriDown,
    starPenta, starStar4, starStar5, starStar6, starCross, starDCross,
    starFlower5, starFlower4, starStar4Arc, starMaltese}
  StarSquareArr: array[1..4] of TSimplePoint2D
  = ((1, 1), (-1, 1), (-1, -1), (1, -1));
  StarDiamondArr: array[1..4] of TSimplePoint2D
  = ((1, 0), (0, 1), (-1, 0), (0, -1));
  StarTriUpArr: array[1..3] of TSimplePoint2D
  = ((1, -0.58), (0, 1.15), (-1, -0.58));
  StarTriDownArr: array[1..3] of TSimplePoint2D
  = ((-1, 0.58), (0, -1.15), (1, 0.58));
  StarPentaArr: array[1..5] of TSimplePoint2D
  = ((0.95, 0.318), (0, 1), (-0.95, 0.318), (-0.59, -0.81), (0.59,
    -0.81));
  StarCrossArr: array[1..12] of TSimplePoint2D
  = ((3, 1), (1, 1), (1, 3), (-1, 3),
    (-1, 1), (-3, 1), (-3, -1), (-1, -1),
    (-1, -3), (1, -3), (1, -1), (3, -1));
  StarDCrossArr: array[1..12] of TSimplePoint2D
  = ((1, 0), (3, 2), (2, 3), (0, 1),
    (-2, 3), (-3, 2), (-1, 0), (-3, -2),
    (-2, -3), (0, -1), (2, -3), (3, -2));
  StarStar4Arr: array[1..8] of TSimplePoint2D
  = ((3, 0), (1, 1), (0, 3), (-1, 1),
    (-3, 0), (-1, -1), (0, -3), (1, -1));
  StarStar5Arr: array[1..10] of TSimplePoint2D
  = ((0.95, 0.31), (0.22, 0.31), (0, 1), (-0.22, 0.31), (-0.95,
    0.31),
    (-0.36, -0.12), (-0.59, -0.81), (0, -0.38), (0.59, -0.81),
    (0.36, -0.12));
  StarStar6Arr: array[1..12] of TSimplePoint2D
  = ((1, 0), (1.5, 0.87), (0.5, 0.87), (0, 1.73),
    (-0.5, 0.87), (-1.5, 0.87), (-1, 0), (-1.5, -0.87),
    (-0.5, -0.87), (0, -1.73), (0.5, -0.87), (1.5, -0.87));

class function TStar2D.GetName: string;
begin
  Result := 'Star';
end;

procedure TStar2D.FillPieces;
var
  StarSize: TRealType;
  APiece: TPiece;
  function FillSimplePoly(
    const PArr: PSimplePointArr2D; const Count: Integer;
    const R: TRealType): TLinPath;
  var
    I: Integer;
  begin
    Result := TLinPath.Create(Count);
    for I := 0 to Pred(Count) do
      Result.Add(Point2D(PArr^[I][1] * R, PArr^[I][2] * R));
    Pieces.Add(Result);
    Result.Closed := True;
  end;
begin
  if ParentDrawing is TDrawing2D then
    StarSize := (ParentDrawing as TDrawing2D).StarsSize
  else
    StarSize := 1;
  StarSize := StarSize * StarSizeFactor;
  case StarKind of
    starSquare: APiece := FillSimplePoly(@StarSquareArr,
        Length(StarSquareArr), StarSize * 0.9);
    starDiamond: APiece := FillSimplePoly(@StarDiamondArr,
        Length(StarDiamondArr), StarSize * 1.2);
    starTriUp: APiece := FillSimplePoly(@StarTriUpArr,
        Length(StarTriUpArr), StarSize * 1.1);
    starTriDown: APiece := FillSimplePoly(@StarTriDownArr,
        Length(StarTriDownArr), StarSize * 1.1);
    starPenta: APiece := FillSimplePoly(@StarPentaArr,
        Length(StarPentaArr), StarSize * 1.1);
    starStar4: APiece := FillSimplePoly(@StarStar4Arr,
        Length(StarStar4Arr), StarSize * 0.5);
    starStar5: APiece := FillSimplePoly(@StarStar5Arr,
        Length(StarStar5Arr), StarSize * 1.6);
    starStar6: APiece := FillSimplePoly(@StarStar6Arr,
        Length(StarStar6Arr), StarSize * 0.8);
    starCross: APiece := FillSimplePoly(@StarCrossArr,
        Length(StarCrossArr), StarSize * 0.4);
    starDCross: APiece := FillSimplePoly(@StarDCrossArr,
        Length(StarDCrossArr), StarSize * 0.4);
    starFlower5: APiece := Pieces.AddFromSvgPathT(
        'M -0.19 0.16 C -0.16 0.19 -0.13 0.21 -0.1 0.23 C -0.35 0.71 -0.24 0.97 0 0.97 C 0.24 0.97 0.35 0.71 0.1 0.23 C 0.13 0.21 0.16 0.19 0.19 0.16 C 0.56 0.55 0.85 0.53 0.92 0.3 C 1 0.07 0.78 -0.11 0.25 -0.02 ' +
        'C 0.24 -0.06 0.23 -0.1 0.21 -0.13 C 0.7 -0.37 0.76 -0.64 0.57 -0.78 C 0.38 -0.92 0.13 -0.77 0.06 -0.24 C 0.02 -0.25 -0.02 -0.25 -0.06 -0.24 C -0.13 -0.77 -0.38 -0.92 -0.57 -0.78 C -0.76 -0.64 -0.7 -0.37 -0.21 -0.13 ' +
        'C -0.23 -0.1 -0.24 -0.06 -0.25 -0.02 C -0.78 -0.11 -1 0.07 -0.92 0.3 C -0.85 0.53 -0.56 0.55 -0.19 0.16 Z',
        Scale2D(1.5 * StarSize, 1.5 * StarSize));
    starFlower4: APiece := Pieces.AddFromSvgPathT(
        'M 5 5 C 5 32 36 34 36 65 C 36 84 19 100 0 100 C -19 100 -36 84 -36 65 C -36 34 -5 32 -5 5 C -32 5 -34 36 -65 36 C -84 36 -100 19 -100 0 C -100 -19 -84 -36 -65 -36 C -34 -36 -32 -5 -5 -5 C -5 -32 -36 -34 -36 -65 ' +
        'C -36 -84 -19 -100 0 -100 C 19 -100 36 -84 36 -65 C 36 -34 5 -32 5 -5 C 32 -5 34 -36 65 -36 C 84 -36 100 -19 100 0 C 100 19 84 36 65 36 C 34 36 32 5 5 5 Z',
        Scale2D(StarSize * 0.013, StarSize * 0.013));
    starStar4Arc: APiece := Pieces.AddFromSvgPathT(
        'M -100 0 C -50 15 -15 50 0 100 C 15 50 50 15 100 0 C 50 -15 15 -50 0 -100 C -15 -50 -50 -15 -100 0 Z',
        Scale2D(StarSize * 0.014, StarSize * 0.014));
    starMaltese: APiece := Pieces.AddFromSvgPathT(
        'M 100 67 C 79 31 44 4 3 3 C 4 44 31 79 67 100 C 22 100 -22 100 -67 100 C -31 79 -4 44 -3 3 C -44 4 -79 31 -100 67 C -100 22 -100 -22 -100 -67 C -79 -31 -44 -4 -3 -3 C -4 -44 -31 -79 -67 -100 C -22 -100 22 -100 67 -100 ' +
        'C 31 -79 4 -44 3 -3 C 44 -4 79 -31 100 -67 C 100 -22 100 22 100 67 Z',
        Scale2D(StarSize * 0.0115, StarSize * 0.0115));
  else //starCircle
    begin
{      APiece := TLinPath.Create(30);
      Pieces.Add(APiece);
      LinearizeCircle(APiece,
        Point2D(0, 0), StarSize, 30);}
      APiece := TCirclePiece.Create(1);
      APiece.Add(Point2D(0, 0));
      (APiece as TCirclePiece).R := StarSize;
      Pieces.Add(APiece);
    end;
  end;
  APiece.TransformPoints(Translate2D(fPoints[0].X,
    fPoints[0].Y));
  APiece.Line := pliSolidDefault;
  APiece.Fill := pfiLineAsDefault;
  APiece.Hatch := phaNone;
end;

constructor TStar2D.Create(ID: Integer);
begin
  inherited CreateSpec(ID, 1, 50);
  WhenCreated;
end;

constructor TStar2D.CreateSpec(ID: Integer; const P: TPoint2D);
begin
  inherited CreateSpec(ID, 1, 50);
  fPoints.DisableEvents := True;
  try
    fPoints.Add(P);
  finally
    fPoints.DisableEvents := False;
    UpdateExtension(Self);
  end;
  WhenCreated;
end;

procedure TStar2D.WhenCreated;
begin
  fStarKind := starCircle;
  fStarSizeFactor := 1;
  fPoints.GrowingEnabled := False;
end;

procedure TStar2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then Exit;
  inherited Assign(Obj);
  if (Obj is TPrimitive2D) and not (Obj is TStar2D) then
  begin
    fPoints[0] := BoxCenter((Obj as TPrimitive2D).BoundingBox);
  end;
  WhenCreated;
  if (Obj is TStar2D) then
  begin
    if (Obj as TPrimitive2D).fPoints.Count > 0 then
      fPoints.Copy((Obj as TPrimitive2D).fPoints, 0, 0);
    fStarKind := TStar2D(Obj).fStarKind;
    fStarSizeFactor := TStar2D(Obj).fStarSizeFactor;
  end;
  UpdateExtension(Self);
end;

constructor TStar2D.CreateFromStream(const Stream: TStream);
begin
  inherited;
  with Stream do
  begin
    Read(fStarKind, SizeOf(fStarKind));
    Read(fStarSizeFactor, SizeOf(fStarSizeFactor));
  end;
end;

procedure TStar2D.SaveToStream(const Stream: TStream);
begin
  inherited SaveToStream(Stream);
  with Stream do
  begin
    Write(fStarKind, SizeOf(fStarKind));
    Write(fStarSizeFactor, SizeOf(fStarSizeFactor));
  end;
end;

// =====================================================================
// TBox2D0
// =====================================================================

function RectScaleTransform(A, C, D: TPoint2D): TTransf2D;
//A: (1, 1)
//C  (0.5, 0.5) center
//D: (1, 0)
var
  B: TPoint2D;
  DXI, DYI: TRealType;
  VX, VY: TVector2D;
begin
  Result := IdentityTransf2D;
  B := MixPoint(C, A, -1);
  VX := Vector2D(B, D);
  VY := Vector2D(D, A);
  DXI := VectorLength2D(VX);
  DYI := VectorLength2D(VY);
  if DXI <> 0 then DXI := 1 / DXI;
  if DYI <> 0 then DYI := 1 / DYI;
  VX := ScaleVector2D(VX, DXI);
  VY := ScaleVector2D(VY, DYI);
  Result[1, 1] := VX.X * DXI;
  Result[1, 2] := VY.X * DYI;
  Result[2, 1] := VX.Y * DXI;
  Result[2, 2] := VY.Y * DYI;
  Result[3, 1] := -(B.X * VX.X + B.Y * VX.Y) * DXI;
  Result[3, 2] := -(B.X * VY.X + B.Y * VY.Y) * DYI;
end;

procedure TBox2D0.PolyPoints(var PP: TPointsSet2D;
  T: TTransf2D);
var
  A: TRealType;
  P0, P1, P2, P3, P4: TPoint2D;
begin
  PP.Clear;
  PP.Expand(4);
  P0 := fPoints[0];
  P1 := fPoints[1];
  P2 := fPoints[2];
  if (P2.X = 0) and (P2.Y = 0)
    and ((P1.X <> 0) or (P1.Y <> 0)) then
  begin
    P2 := Point2D(P0.X, P0.Y + 1);
    fPoints[2] := P2;
  end;
  RectangleCalcPoints(P0, P1, P2, P3, P4, A);
  PP.Add(TransformPoint2D(P0, T));
  PP.Add(TransformPoint2D(P3, T));
  PP.Add(TransformPoint2D(P1, T));
  PP.Add(TransformPoint2D(P4, T));
end;

constructor TBox2D0.Create(ID: Integer);
begin
  inherited CreateSpec(ID, 3, 150);
  if fPoints <> nil then fPoints.DisableEvents := True;
  try
    fPoints.Add(Point2D(0, 0));
    fPoints.Add(Point2D(0, 0));
    fPoints.Add(Point2D(0, 0));
  finally
    if fPoints <> nil then fPoints.DisableEvents := False;
    UpdateExtension(Self);
  end;
  WhenCreated;
end;

procedure TBox2D0.WhenCreated;
begin
  fPoints.GrowingEnabled := False;
end;

procedure TBox2D0.Assign(const Obj: TGraphicObject);
var
  R: TRect2D;
begin
  if (Obj = Self) then Exit;
  inherited Assign(Obj);
  if Obj is TBox2D0
    and ((Obj as TPrimitive2D).fPoints.Count > 2) then
    fPoints.Copy((Obj as TPrimitive2D).fPoints, 0, 2)
  else if Obj is TPrimitive2D then
  begin
    //R := (Obj as TPrimitive2D).fPoints.Extension;
    R := (Obj as TPrimitive2D).BoundingBox;
    fPoints.Clear;
    fPoints.AddPoints([R.FirstEdge, R.SecondEdge,
      Point2D(R.Right, R.Bottom)]);
  end;
  WhenCreated;
end;

procedure TBox2D0.MoveControlPoint0(const Pos: Integer;
  P: TPoint2D; Shift: TShiftState);
var
  T: TTransf2D;
  CP: TPoint2D;
begin
  CP := MidPoint(fPoints[0], fPoints[1]);
  case Pos of
    0: T := RectScaleTransform(fPoints[0], CP, fPoints[2]);
    1: T := RectScaleTransform(fPoints[1], CP, fPoints[2]);
  else //2:
    T := RectScaleTransform(fPoints[2], CP, fPoints[0]);
  end;
  fPoints.TransformPoints(T);
  P := TransformPoint2D(P, T);
  fPoints.TransformPoints(Scale2D(P.X, P.Y));
  fPoints.TransformPoints(InvertTransform2D(T));
  //fPoints[Pos] := P;
end;

// =====================================================================
// TRectangle2D
// =====================================================================

class function TRectangle2D.GetName: string;
begin
  Result := 'Rectangle';
end;

procedure TRectangle2D.GetRectangleParameters(P0, P1, P2:
  TPoint2D;
  var P: TPoint2D; var ARot, W, H: TRealType;
  const Eps: TRealType);
var
  A: TRealType;
  P3, P4: TPoint2D;
  R: TRect2D;
begin
  RectangleCalcPoints(P0, P1, P2, P3, P4, A);
  if (A <> 1) and (A <> 0) then
  begin
    fPoints.DisableEvents := True;
    try
      fPoints[2] := P3;
    finally
      fPoints.DisableEvents := False;
    end;
  end;
  A := (P4.X - P0.X) * (P3.Y - P0.Y) -
    (P4.Y - P0.Y) * (P3.X - P0.X);
  if A > 0 then
  begin
    W := PointDistance2D(P0, P4);
    H := PointDistance2D(P0, P3);
    ARot := TwoPointsAngle(P0, P4);
    P := P0;
  end
  else
  begin
    W := PointDistance2D(P0, P3);
    H := PointDistance2D(P0, P4);
    ARot := TwoPointsAngle(P0, P3);
    P := P0;
  end;
  if Abs(ARot / Pi * 2 - Round(ARot / Pi * 2)) < Eps then
  begin
    R.Left := Min(Min(P0.X, P1.X), P3.X);
    R.Bottom := Min(Min(P0.Y, P1.Y), P3.Y);
    R.Right := Max(Max(P0.X, P1.X), P3.X);
    R.Top := Max(Max(P0.Y, P1.Y), P3.Y);
    R.W1 := 1;
    R.W2 := 1;
    P := R.FirstEdge;
    W := R.Right - R.Left;
    H := R.Top - R.Bottom;
    ARot := 0;
  end;
end;

procedure TRectangle2D.FillPieces;
var
  APath: TPiece;
  P0, P1, P2, P: TPoint2D;
  RXX, RYY, ARot, H, W: TRealType;
begin
  if RX = 0 then
    APath := TRectanglePiece.Create(1)
  else
    APath := TBezierPath.Create(25);
  Pieces.Add(APath);
  APath.Closed := True;
  APath.Line := pliDefault;
  APath.Fill := pfiDefault;
  APath.Hatch := phaDefault;
  P0 := fPoints[0];
  P1 := fPoints[1];
  P2 := fPoints[2];
  GetRectangleParameters(P0, P1, P2, P,
    ARot, W, H, 1E-10);
  if RX = 0 then
  begin
    (APath as TRectanglePiece).ARot := ARot;
    (APath as TRectanglePiece).H := H;
    (APath as TRectanglePiece).W := W;
    APath.Add(P);
  end
  else
  begin
    RXX := RX;
    RYY := RY;
    if RY = 0 then RYY := RXX;
    if RXX > W / 2 then RXX := W / 2;
    if RXX < 0 then RXX := 0;
    if RYY > H / 2 then RYY := H / 2;
    if RYY < 0 then RYY := 0;
    RoundRectBezierPoints(P, W, H, RXX, RYY, ARot, APath);
    APath.Add(APath[0]);
  end;
end;

constructor TRectangle2D.Create(ID: Integer);
begin
  inherited Create(ID);
  WhenCreated;
end;

procedure TRectangle2D.WhenCreated;
begin
  fDrawPathBezier := False;
  fRX := 0;
  fRY := 0;
end;

procedure TRectangle2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then Exit;
  if fPoints <> nil then fPoints.DisableEvents := True;
  try
    inherited Assign(Obj);
    WhenCreated;
  finally
    if fPoints <> nil then fPoints.DisableEvents := False;
  end;
  if Obj is TRectangle2D then
  begin
    fRX := (Obj as TRectangle2D).fRX;
    fRY := (Obj as TRectangle2D).fRY;
  end;
  UpdateExtension(Self);
end;

constructor TRectangle2D.CreateFromStream(const Stream: TStream);
begin
  inherited CreateFromStream(Stream);
  with Stream do
  begin
    Read(fRX, SizeOf(fRX));
    Read(fRY, SizeOf(fRY));
  end;
end;

procedure TRectangle2D.SaveToStream(const Stream: TStream);
begin
  inherited SaveToStream(Stream);
  with Stream do
  begin
    Write(fRX, SizeOf(fRX));
    Write(fRY, SizeOf(fRY));
  end;
end;

// =====================================================================
// TEllipse2D
// =====================================================================

class function TEllipse2D.GetName: string;
begin
  Result := 'Ellipse';
end;

procedure TEllipse2D.FillPieces;
var
  APath: TEllipsePiece;
  CP: TPoint2D;
begin
  APath := TEllipsePiece.Create(1);
  Pieces.Add(APath);
  APath.Line := pliDefault;
  APath.Fill := pfiDefault;
  APath.Hatch := phaDefault;
  APath.Closed := True;
  GetEllipseParams(CP, APath.RX, APath.RY, APath.ARot);
  APath.Add(CP);
end;

constructor TEllipse2D.Create(ID: Integer);
begin
  inherited Create(ID);
  WhenCreated;
end;

procedure TEllipse2D.WhenCreated;
begin
  fDrawPathBezier := True;
end;

procedure TEllipse2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then Exit;
  if fPoints <> nil then fPoints.DisableEvents := True;
  try
    inherited Assign(Obj);
    WhenCreated;
  finally
    if fPoints <> nil then fPoints.DisableEvents := False;
  end;
  UpdateExtension(Self);
end;

function GetEllipseParams00(const P0, P1: TPoint2D;
  P2: TPoint2D; var P3, P4: TPoint2D;
  var CP: TPoint2D; var RX, RY, ARot: TRealType): Boolean;
var
  A: TRealType;
begin
  //Result is true if P2 needs to be updated with P3
  Result := False;
  if IsSamePoint2D(P2, P0)
    and not IsSamePoint2D(P1, P0) then
  {if IsSamePoint2D(P2, Point2D(0,0))
    and not IsSamePoint2D(P1, Point2D(0,0)) then}
  begin
    P2 := Point2D(P0.X, P0.Y + 1);
    Result := True;
  end;
  RectangleCalcPoints(P0, P1, P2, P3, P4, A);
  if (A <> 1) and (A <> 0) then
    Result := True;
  CP := MidPoint(P0, P1);
  RX := PointDistance2D(P0, P4) / 2.0;
  RY := PointDistance2D(P0, P3) / 2.0;
  ARot := TwoPointsAngle(P0, P4);
end;

procedure GetEllipseParams0(const P0, P1, P2: TPoint2D;
  var P3, P4: TPoint2D;
  var CP: TPoint2D; var RX, RY, ARot: TRealType);
begin
  GetEllipseParams00(P0, P1, P2, P3, P4,
    CP, RX, RY, ARot);
end;

procedure TEllipse2D.GetEllipseParams(
  var CP: TPoint2D; var RX, RY, ARot: TRealType);
var
  P0, P1, P2, P3, P4: TPoint2D;
begin
  P0 := fPoints[0];
  P1 := fPoints[1];
  P2 := fPoints[2];
  if GetEllipseParams00(P0, P1, P2, P3, P4,
    CP, RX, RY, ARot) then
  begin
    fPoints.DisableEvents := True;
    try
      fPoints[2] := P3;
    finally
      fPoints.DisableEvents := False;
    end;
  end;
end;

// =====================================================================
// TCircle2D
// =====================================================================

class function TCircle2D.GetName: string;
begin
  Result := 'Circle';
end;

procedure TCircle2D.FillPieces;
var
  APath: TCirclePiece;
begin
  APath := TCirclePiece.Create(1);
  Pieces.Add(APath);
  APath.Line := pliDefault;
  APath.Fill := pfiDefault;
  APath.Hatch := phaDefault;
  APath.Closed := True;
  APath.Add(fPoints[0]);
  APath.R := PointDistance2D(fPoints[0], fPoints[1]);
end;

constructor TCircle2D.Create(ID: Integer);
begin
  inherited CreateSpec(ID, 2, 150);
  fPoints.DisableEvents := True;
  try
    fPoints.Add(Point2D(0, 0));
    fPoints.Add(Point2D(0, 0));
  finally
    fPoints.DisableEvents := False;
    UpdateExtension(Self);
  end;
  WhenCreated;
end;

procedure TCircle2D.WhenCreated;
begin
  fPoints.GrowingEnabled := False;
end;

procedure TCircle2D.Assign(const Obj: TGraphicObject);
var
  R: TRect2D;
begin
  if (Obj = Self) then Exit;
  inherited Assign(Obj);
  if Obj is TPrimitive2D then
  begin
    if (Obj is TCircle2D) or (Obj is TCircular2D) then
      fPoints.Copy((Obj as TPrimitive2D).fPoints, 0, 1)
    else
    begin
      R := (Obj as TPrimitive2D).BoundingBox;
      fPoints.Clear;
      fPoints.AddPoints([BoxCenter(R), R.FirstEdge]);
      if (Obj is TEllipse2D) or (Obj is TStar2D)
        or (Obj is TBezierPath2D0) or (Obj is TSmoothPath2D0)
        or (Obj is TPolyline2D0)
        then
        fPoints[1] := MixPoint(fPoints[0], fPoints[1], 1 /
          Sqrt(2));
    end;
  end;
  WhenCreated;
  UpdateExtension(Self);
end;

procedure TCircle2D.BreakPath(P: TPoint2D;
  Aperture, Precision: TRealType; var Obj1, Obj2: TPrimitive2D);
var
  A, Distance: TRealType;
  I: Integer;
begin
  Obj1 := nil;
  Obj2 := nil;
  Distance := Aperture;
  if not Pieces.IsPointOnStroke(
    Self, P, Precision, Distance, I) then Exit;
  A := TwoPointsAngle(fPoints[0], P);
  Obj1 := TArc2D.Create(-1);
  Obj1.Assign(Self);
  (Obj1 as TArc2D).SetStartAngle(A);
  (Obj1 as TArc2D).SetEndAngle(A + 1.999999 * Pi);
end;

// =====================================================================
// TCircular2D
// =====================================================================

procedure TCircular2D.FillPieces;
var
  APath: TCircularPiece;
  P0, P1, P2: TPoint2D;
  procedure FillArcArrows;
  var
    Delta: TRealType;
    function GetDelta(const A: TRealType): TRealType;
    begin
      if A > 1 then Result := Pi / 3
      else if A > 0 then Result := ArcCos(1 - Sqr(A) / 2);
    end;
  begin
    // First arrow-head
    Delta := GetDelta(ArrowSize *
      MeasureArrow(fBeginArrowKind) / APath.R);
    FillArrow(fBeginArrowKind,
      ShiftPoint(P0, PolarVector(APath.R, APath.SA)),
      ShiftPoint(P0, PolarVector(APath.R, APath.SA + Delta)),
      ArrowSize, Pieces);
    // Second arrow-head
    Delta := GetDelta(ArrowSize *
      MeasureArrow(fEndArrowKind) / APath.R);
    if Delta > 1 then Delta := Pi / 3
    else Delta := ArcCos(1 - Sqr(Delta) / 2);
    FillArrow(fEndArrowKind,
      ShiftPoint(P0, PolarVector(APath.R, APath.EA)),
      ShiftPoint(P0, PolarVector(APath.R, APath.EA - Delta)),
      ArrowSize, Pieces);
  end;
begin
  APath := TCircularPiece.Create(1);
  Pieces.Add(APath);
  APath.Line := pliDefault;
  APath.Fill := pfiDefault;
  APath.Hatch := phaDefault;
  APath.Closed := not (Self is TArc2D);
  P0 := fPoints[0];
  P1 := fPoints[1];
  P2 := fPoints[2];
  APath.Add(P0);
  APath.R := PointDistance2D(P0, P1);
  APath.SA := ArcTan2(P1.Y - P0.Y, P1.X - P0.X);
  APath.EA := ArcTan2(P2.Y - P0.Y, P2.X - P0.X);
  if (Self is TArc2D) then
  begin
    APath.Kind := ci_Arc;
    if (fBeginArrowKind <> arrNone) or
      (fEndArrowKind <> arrNone) then FillArcArrows;
  end
  else if (Self is TSector2D) then
    APath.Kind := ci_Sector
  else if (Self is TSegment2D) then
    APath.Kind := ci_Segment;
end;

procedure TCircular2D.GetArcParams(var CX, CY, R, SA, EA:
  TRealType);
var
  P0, P1, P2: TPoint2D;
begin
  P0 := fPoints[0];
  P1 := fPoints[1];
  P2 := fPoints[2];
  CX := P0.X;
  CY := P0.Y;
  R := PointDistance2D(P0, P1);
  SA := ArcTan2(P1.Y - CY, P1.X - CX);
  EA := ArcTan2(P2.Y - CY, P2.X - CX);
end;

procedure TCircular2D.SetRadius(R: TRealType);
var
  CX, CY, R0, SA, EA: TRealType;
begin
  GetArcParams(CX, CY, R0, SA, EA);
  fPoints[1] := Point2D(CX + R * Cos(SA), CY + R * Sin(SA));
  fPoints[2] := Point2D(CX + R * Cos(EA), CY + R * Sin(EA));
end;

function TCircular2D.GetRadius: TRealType;
begin
  Result := PointDistance2D(fPoints[0], fPoints[1]);
end;

procedure TCircular2D.SetStartAngle(A: TRealType);
var
  CX, CY, R, SA, EA: TRealType;
begin
  GetArcParams(CX, CY, R, SA, EA);
  fPoints[1] := Point2D(CX + R * Cos(A), CY + R * Sin(A));
end;

function TCircular2D.GetStartAngle: TRealType;
var
  CX, CY, R, EA: TRealType;
begin
  GetArcParams(CX, CY, R, Result, EA);
end;

procedure TCircular2D.SetEndAngle(A: TRealType);
var
  CX, CY, R, SA, EA: TRealType;
begin
  GetArcParams(CX, CY, R, SA, EA);
  fPoints[2] := Point2D(CX + R * Cos(A), CY + R * Sin(A));
end;

function TCircular2D.GetEndAngle: TRealType;
var
  CX, CY, R, SA: TRealType;
begin
  GetArcParams(CX, CY, R, SA, Result);
end;

constructor TCircular2D.Create(ID: Integer);
begin
  inherited CreateSpec(ID, 3, 150);
  WhenCreated;
end;

constructor TCircular2D.CreateSpec(ID: Integer; const CP:
  TPoint2D;
  R, SA, EA: TRealType);
begin
  inherited CreateSpec(ID, 3, 150);
  fPoints.DisableEvents := True;
  try
    fPoints.Add(CP);
    fPoints.Add(Point2D(CP.X + R * Cos(SA),
      CP.Y + R * Sin(SA)));
    fPoints.Add(Point2D(CP.X + R * Cos(EA),
      CP.Y + R * Sin(EA)));
  finally
    fPoints.DisableEvents := False;
    UpdateExtension(Self);
  end;
  WhenCreated;
end;

procedure TCircular2D.WhenCreated;
begin
  fPoints.GrowingEnabled := False;
  fOwnsInterior := False;
end;

procedure TCircular2D.FinishFirstDraw;
var
  CX, CY, R, SA, EA: TRealType;
begin
  inherited FinishFirstDraw;
  GetArcParams(CX, CY, R, SA, EA);
  fPoints[2] :=
    Point2D(CX + R * Cos(EA), CY + R * Sin(EA));
end;

procedure TCircular2D.Assign(const Obj: TGraphicObject);
var
  R: TRect2D;
begin
  if (Obj = Self) then Exit;
  inherited Assign(Obj);
  fPoints.DisableEvents := True;
  try
    if Obj is TPrimitive2D then
    begin
      if Obj is TCircle2D then
      begin
        fPoints.Clear;
        fPoints.AddPoints([
          (Obj as TPrimitive2D).fPoints[0],
            (Obj as TPrimitive2D).fPoints[1],
            (Obj as TPrimitive2D).fPoints[1]]);
        StartAngle := 0;
        EndAngle := 2 * Pi - 1E-5;
      end
      else if (Obj as TPrimitive2D).fPoints.Count > 2 then
        fPoints.Copy((Obj as TPrimitive2D).fPoints, 0, 2)
      else if (Obj as TPrimitive2D).fPoints.Count = 2 then
      begin
        fPoints.Clear;
        fPoints.AddPoints([
          MidPoint((Obj as TPrimitive2D).fPoints[0],
            (Obj as TPrimitive2D).fPoints[1]),
            (Obj as TPrimitive2D).fPoints[0],
            (Obj as TPrimitive2D).fPoints[1]]);
      end
      else
      begin
        R := (Obj as TPrimitive2D).BoundingBox;
        fPoints.Clear;
        fPoints.AddPoints([BoxCenter(R), R.FirstEdge,
          R.SecondEdge]);
      end;
      fPoints.GrowingEnabled := False;
    end;
    if Obj is TCircular2D then
    begin
      StartAngle := (Obj as TCircular2D).StartAngle;
      EndAngle := (Obj as TCircular2D).EndAngle;
    end;
    WhenCreated;
  finally
    fPoints.DisableEvents := False;
  end;
  UpdateExtension(Self);
end;

constructor TCircular2D.CreateFromStream(const Stream: TStream);
begin
  { Load the standard properties }
  inherited;
end;

procedure TCircular2D.SaveToStream(const Stream: TStream);
begin
  { Save the standard properties }
  inherited SaveToStream(Stream);
end;

procedure TCircular2D.ReversePoints;
var
  CX, CY, R, SA, EA: TRealType;
begin
  GetArcParams(CX, CY, R, SA, EA);
  fPoints[1] :=
    Point2D(CX + R * Cos(EA), CY + R * Sin(EA));
  fPoints[2] :=
    Point2D(CX + R * Cos(SA), CY + R * Sin(SA));
  UpdateExtension(Self);
end;


procedure TCircular2D.MoveControlPoint0(const Pos: Integer;
  P: TPoint2D; Shift: TShiftState);
var
  CX, CY, R, SA, EA: TRealType;
begin
  if Pos = 2 then
  begin
    fPoints[2] := P;
    GetArcParams(CX, CY, R, SA, EA);
    R := PointDistance2D(fPoints[0], P);
    fPoints[1] :=
      Point2D(CX + R * Cos(SA), CY + R * Sin(SA));
  end
  else
  begin
    fPoints[Pos] := P;
    GetArcParams(CX, CY, R, SA, EA);
    fPoints[2] :=
      Point2D(CX + R * Cos(EA), CY + R * Sin(EA));
  end;
end;

// =====================================================================
// TArc2D
// =====================================================================

class function TArc2D.GetName: string;
begin
  Result := 'Arc';
end;

procedure TArc2D.BreakPath(P: TPoint2D;
  Aperture, Precision: TRealType; var Obj1, Obj2: TPrimitive2D);
var
  A, Distance: TRealType;
  I: Integer;
begin
  Obj1 := nil;
  Obj2 := nil;
  Distance := Aperture;
  if not Pieces.IsPointOnStroke(
    Self, P, Precision, Distance, I) then Exit;
//  if Aperture = 0 then Exit;
  A := TwoPointsAngle(fPoints[0], P);
  Obj1 := TArc2D.Create(-1);
  Obj1.Assign(Self);
  (Obj1 as TArc2D).SetEndAngle(A);
  Obj2 := TArc2D.Create(-1);
  Obj2.Assign(Self);
  (Obj2 as TArc2D).SetStartAngle(A);
end;

// =====================================================================
// TSector2D
// =====================================================================

class function TSector2D.GetName: string;
begin
  Result := 'Sector';
end;

constructor TSector2D.Create(ID: Integer);
begin
  inherited Create(ID);
  WhenCreated;
end;

constructor TSector2D.CreateSpec(ID: Integer; const CP: TPoint2D;
  R, SA, EA: TRealType);
begin
  inherited CreateSpec(ID, CP, R, SA, EA);
  WhenCreated;
end;

procedure TSector2D.WhenCreated;
begin
  fOwnsInterior := True;
end;

procedure TSector2D.BreakPath(P: TPoint2D;
  Aperture, Precision: TRealType; var Obj1, Obj2: TPrimitive2D);
var
  A, Distance: TRealType;
  I: Integer;
begin
  Obj1 := nil;
  Obj2 := nil;
  Distance := Aperture;
  if not Pieces.IsPointOnStroke(
    Self, P, Precision, Distance, I) then Exit;
//  if Aperture = 0 then Exit;
  A := TwoPointsAngle(fPoints[0], P);
  Obj1 := TSector2D.Create(-1);
  Obj1.Assign(Self);
  (Obj1 as TSector2D).SetEndAngle(A);
  Obj2 := TSector2D.Create(-1);
  Obj2.Assign(Self);
  (Obj2 as TSector2D).SetStartAngle(A);
end;

// =====================================================================
// TSegment2D
// =====================================================================

class function TSegment2D.GetName: string;
begin
  Result := 'Segment';
end;

constructor TSegment2D.Create(ID: Integer);
begin
  inherited Create(ID);
  WhenCreated;
end;

constructor TSegment2D.CreateSpec(ID: Integer; const CP:
  TPoint2D;
  R, SA, EA: TRealType);
begin
  inherited CreateSpec(ID, CP, R, SA, EA);
  WhenCreated;
end;

procedure TSegment2D.WhenCreated;
begin
  fOwnsInterior := True;
end;

procedure TSegment2D.BreakPath(P: TPoint2D;
  Aperture, Precision: TRealType; var Obj1, Obj2: TPrimitive2D);
var
  A, Distance: TRealType;
  I: Integer;
begin
  Obj1 := nil;
  Obj2 := nil;
  Distance := Aperture;
  if not Pieces.IsPointOnStroke(
    Self, P, Precision, Distance, I) then Exit;
//  if Aperture = 0 then Exit;
  A := TwoPointsAngle(fPoints[0], P);
  Obj1 := TSegment2D.Create(-1);
  Obj1.Assign(Self);
  (Obj1 as TSegment2D).SetEndAngle(A);
  Obj2 := TSegment2D.Create(-1);
  Obj2.Assign(Self);
  (Obj2 as TSegment2D).SetStartAngle(A);
end;

// =====================================================================
// TBezierPrimitive2D
// =====================================================================

procedure TBezierPrimitive2D.FillPieces;
var
  APath: TBezierPath;
  procedure FillBezierArrows;
  var
    P0, P1: TPoint2D;
    function GetSecondPoint(const Kind: TArrowKind;
      const P0, P1, P2, P3: TPoint2D): TPoint2D;
    var
      R, U, DP: TRealType;
      V: TVector2D;
    begin
//      Result := P1; // primitive approximation
      // First approximation:
      if ArrowSize = 0 then Exit;
      R := ArrowSize * MeasureArrow(Kind);
      V := Vector2D(P0, P1);
      U := VectorLength2D(V);
      if U > 0 then U := R / (3 * U)
      else U := 0.1;
      Result := BezierPoint(P0, P1, P2, P3, U);
      // Second approximation (Newton algorithm step):
      V := Vector2D(P0, Result);
      DP := DotProduct2D(
        BezierDerivative(P0, P1, P2, P3, U), V);
      if Abs(DP) < Sqr(R) * 1E-10 then Exit;
      U := U + (Sqr(R) - VectorLength2D(V)) / (2 * DP);
      Result := BezierPoint(P0, P1, P2, P3, U);
    end;
  begin
    if fPoints.Count < 2 then Exit;
    if fPoints.Count < 4 then
    begin
      FillArrow(fBeginArrowKind,
        Pieces.Item[0][0], Pieces.Item[0][1],
        ArrowSize, Pieces);
      FillArrow(fEndArrowKind,
        Pieces.Item[0][Pieces.Item[0].Count - 1],
        Pieces.Item[0][Pieces.Item[0].Count - 2],
        ArrowSize, Pieces);
      Exit;
    end;
    // First arrow-head
    P0 := Pieces.Item[0][0];
    P1 := GetSecondPoint(fBeginArrowKind,
      P0, Pieces.Item[0][1],
      Pieces.Item[0][2], Pieces.Item[0][3]);
    FillArrow(fBeginArrowKind, P0, P1, ArrowSize, Pieces);
    // Second arrow-head
    P0 := Pieces.Item[0][Pieces.Item[0].Count - 1];
    P1 := GetSecondPoint(fEndArrowKind,
      P0, Pieces.Item[0][Pieces.Item[0].Count - 2],
      Pieces.Item[0][Pieces.Item[0].Count - 3],
      Pieces.Item[0][Pieces.Item[0].Count - 4]);
    FillArrow(fEndArrowKind, P0, P1, ArrowSize, Pieces);
  end;
begin
  APath := TBezierPath.Create(0);
  Pieces.Add(APath);
  APath.Line := pliDefault;
  APath.Fill := pfiDefault;
  APath.Hatch := phaDefault;
  BezierPoints(APath);
  APath.Closed := (Self is TClosedSmoothPath2D)
    or (Self is TClosedBezierPath2D);
  if not APath.Closed and
    ((fBeginArrowKind <> arrNone) or
    (fEndArrowKind <> arrNone)) then FillBezierArrows;
end;

procedure TBezierPrimitive2D.ReversePoints;
begin
  fPoints.ReversePoints;
  UpdateExtension(Self);
end;

// =====================================================================
// TBezierPath2D0
// =====================================================================

procedure AddBezierArc(const PP: TPointsSet2D;
  const CurvePrecision: Integer;
  const P0, P1, P2, P3: TPoint2D);
var
  I: Integer;
begin
  //GetBezierArc(P0, P1, P2, P3, SmoothingFactor, Q1, Q2);
  for I := 0 to CurvePrecision - 1 do
    PP.Add(BezierPoint(P0, P1, P2, P3, I / CurvePrecision));
end;

constructor TBezierPath2D0.Create(ID: Integer);
begin
  inherited Create(ID);
  WhenCreated;
  //FirstDrawPoint := 0;
end;

constructor TBezierPath2D0.CreateSpec(ID: Integer;
  const Pts: array of TPoint2D);
begin
  inherited CreateSpec(ID, High(Pts) - Low(Pts) + 1, 20);
  fPoints.AddPoints(Pts);
  WhenCreated;
end;

procedure TBezierPath2D0.WhenCreated;
begin
  fPoints.GrowingEnabled := True;
  fOwnsInterior := False;
  fCanDeletePoints := True;
  FirstDrawPoint := -1;
end;

procedure TBezierPath2D0.BezierPoints(PP: TPointsSet2D);
var
  I: Integer;
begin
  if fPoints.Count < 1 then Exit;
  PP.Clear;
  if (FirstDrawPoint >= 0) and (fPoints.Count >= 3) then
  begin
    PP.Expand(fPoints.Count * 3 - 2);
    GetHobbyBezier(PP, fPoints);
    Exit;
  end;
  PP.Expand(((fPoints.Count + 1) div 3) * 3 + 1);
  PP[0] := fPoints[0];
  PP[((fPoints.Count + 1) div 3) * 3] := fPoints[0];
  for I := 1 to (fPoints.Count - 1) div 3 do
  begin
    PP[3 * I - 2] := fPoints[3 * I - 2];
    PP[3 * I - 1] := fPoints[3 * I - 1];
    PP[3 * I] := fPoints[3 * I];
  end;
  case fPoints.Count - ((fPoints.Count - 1) div 3) * 3 of
    2:
      begin
        PP[PP.Count - 3] := fPoints[fPoints.Count - 2];
        PP[PP.Count - 2] := fPoints[fPoints.Count - 1];
        PP[PP.Count - 1] := fPoints[fPoints.Count - 1];
      end;
    3:
      begin
        PP[PP.Count - 3] := fPoints[fPoints.Count - 2];
        PP[PP.Count - 2] := fPoints[fPoints.Count - 2];
        PP[PP.Count - 1] := fPoints[fPoints.Count - 1];
      end;
  end;
end;

procedure TBezierPath2D0.Assign(const Obj: TGraphicObject);
var
  PP: TPointsSet2D;
begin
  if (Obj = Self) then Exit;
  inherited Assign(Obj);
  if Obj is TPrimitive2D then
  begin
    fPoints.DisableEvents := True;
    if not (Obj is TBezierPath2D0) then
    begin
      PP := TPointsSet2D.Create(0);
      try
        (Obj as TPrimitive2D).BezierPoints(PP);
        if Self is TClosedBezierPath2D then
          if IsSamePoint2D(PP[0], PP[PP.Count - 1])
            then
            fPoints.Copy(PP, 0, PP.Count - 2)
          else
          begin
            fPoints.Copy(PP, 0, PP.Count - 1);
            fPoints.Add(MixPoint(PP[0], PP[PP.Count - 1],
              0.75));
            fPoints.Add(MixPoint(PP[0], PP[PP.Count - 1],
              0.25));
          end
        else
          fPoints.Copy(PP, 0, PP.Count - 1);
      finally
        PP.Free;
      end;
    end
    else
    begin
      PP := TBezierPath2D0(Obj).fPoints;
      if ((Self is TClosedBezierPath2D) and
        (Obj is TClosedBezierPath2D))
        or ((Self is TBezierPath2D)
        and (Obj is TBezierPath2D)) then
        fPoints.Copy(PP, 0, PP.Count - 1)
      else if Self is TClosedBezierPath2D then
      begin
        if IsSamePoint2D(PP[0], PP[PP.Count - 1]) then
          fPoints.Copy(PP, 0, PP.Count - 2)
        else
        begin
          fPoints.Copy(PP, 0, PP.Count - 1);
          fPoints.Add(MixPoint(PP[0], PP[PP.Count - 1], 0.75));
          fPoints.Add(MixPoint(PP[0], PP[PP.Count - 1], 0.25));
        end;
      end
      else
      begin
        fPoints.Copy(PP, 0, PP.Count - 1);
        fPoints.Add(PP[0]);
      end;
    end;
    fPoints.DisableEvents := False;
    fPoints.GrowingEnabled := True;
    fOwnsInterior := False;
    fCanDeletePoints := True;
    fDrawPathBezier := True;
  end;
  UpdateExtension(Self);
end;

procedure TBezierPath2D0.DrawControlPoints0(const VT: TTransf2D;
  const ClipRect2D: TRect2D; const Width: Integer);
var
  TmpPt2: TPoint2D;
  I: Integer;
begin
  for I := 0 to fPoints.Count - 1 do
    if (I mod 3) <> 1 then
    begin
      if (Self is TBezierPath2D) and (I = fPoints.Count - 1) then
        Break;
      if I = fPoints.Count - 1 then TmpPt2 := fPoints[0]
      else TmpPt2 := fPoints[I + 1];
      (OwnerDrawing as TDrawing2D).OnControlLine(
        fPoints[I], TmpPt2, VT, ClipRect2D);
    end;
  for I := 0 to fPoints.Count - 1 do
  begin
    if ((I mod 3) = 0) {or (I = fPoints.Count - 1)} then
      (OwnerDrawing as TDrawing2D).OnControlPoint(
        fPoints[I], VT, ClipRect2D)
    else
      (OwnerDrawing as TDrawing2D).OnControlPoint2(
        fPoints[I], VT, ClipRect2D);
  end;
end;

// =====================================================================
// TBezierPath2D
// =====================================================================

class function TBezierPath2D.GetName: string;
begin
  Result := 'Bezier path';
end;

procedure TBezierPath2D.FinishFirstDraw;
var
  PP: TPointsSet2D;
begin
  inherited FinishFirstDraw;
  PP := TPointsSet2D.Create(fPoints.Count * 3 - 2);
  try
    GetHobbyBezier(PP, fPoints);
    fPoints.Clear;
    fPoints.DisableEvents := True;
    fPoints.Copy(PP, 0, PP.Count - 1);
    fPoints.DisableEvents := False;
  finally
    PP.Free;
  end;
  //FirstDrawPoint := 0;
  //    (OwnerDrawing as TDrawing2D).NotifyChanged;
  UpdateExtension(Self);
end;

function TBezierPath2D.DeleteControlPoint0(I: Integer): Integer;
var
  Q1, Q2: TPoint2D;
  DoSmooth: Boolean;
begin
  Result := 0;
  DoSmooth := False;
  if (I < 0) or (I > fPoints.Count - 1) then Exit;
  if I = 0 then
    I := 1
  else if I = fPoints.Count - 1 then
    case I mod 3 of
      0: Dec(I);
      1: Inc(I);
    end
  else if I mod 3 <> 0 then
    Exit
  else
  begin
    DoSmooth := True;
    DeleteBezierPointSmoothly(
      fPoints[I - 3], fPoints[I - 2], fPoints[I - 1], fPoints[I],
      fPoints[I + 1], fPoints[I + 2], fPoints[I + 3], Q1, Q2);
  end;
  if I + 1 <= fPoints.Count - 1 then
  begin
    fPoints.Delete(I + 1);
    Inc(Result);
  end;
  if I <= fPoints.Count - 1 then
  begin
    fPoints.Delete(I);
    Inc(Result);
  end;
  if I - 1 >= 0 then
  begin
    fPoints.Delete(I - 1);
    Inc(Result);
  end;
  if DoSmooth then
  begin
    fPoints[I - 2] := Q1;
    fPoints[I - 1] := Q2;
  end;
end;

function TBezierPath2D.InsertControlPoint0(const Pos: Integer;
  P: TPoint2D; Precision: TRealType): Integer;
var
  P0, P1, P2, P3, P4, P5, P6: TPoint2D;
  UStar: TRealType;
begin
  Result := 0;
  if (Pos < 0) or (3 * Pos >= fPoints.Count - 3) then Exit;
  P0 := fPoints[3 * Pos];
  P1 := fPoints[3 * Pos + 1];
  P2 := fPoints[3 * Pos + 2];
  P3 := fPoints[3 * Pos + 3];
  UStar := ClosestBezierPoint(P, P0, P1, P2, P3, Precision);
  BreakBezier(P0, P1, P2, P3, P4, P5, P6, UStar);
  fPoints[3 * Pos + 1] := P1;
  fPoints[3 * Pos + 2] := P5;
  fPoints.Insert(3 * Pos + 2, P4);
  fPoints.Insert(3 * Pos + 2, P3);
  fPoints.Insert(3 * Pos + 2, P2);
  Inc(Result, 3);
end;

procedure TurnBezierHandle(const PP: TPointsSet2D;
  const Pos, PosC, PosM: Integer);
var
  V: TVector2D;
  R: TRealType;
begin
  V := Direction2D(PP[Pos], PP[PosC]);
  R := PointDistance2D(PP[PosM], PP[PosC]);
  PP[PosM] := ShiftPoint(PP[PosC], ScaleVector2D(V, R));
end;

procedure TBezierPath2D.MoveControlPoint0(const Pos: Integer;
  P: TPoint2D; Shift: TShiftState);
var
  V: TVector2D;
  PosC, PosM: Integer;
begin
  if Pos mod 3 <> 0 then
    if not (SSAlt in Shift) xor SmoothBezierNodes then
    begin
      fPoints[Pos] := P;
      Exit;
    end
    else
    begin
      fPoints[Pos] := P;
      case Pos mod 3 of
        1:
          begin
            PosM := Pos - 2;
            if PosM < 0 then Exit;
            PosC := Pos - 1;
          end;
        2:
          begin
            PosM := Pos + 2;
            if PosM > fPoints.Count - 1 then Exit;
            PosC := Pos + 1;
          end;
      end;
      TurnBezierHandle(fPoints, Pos, PosC, PosM);
      Exit;
    end;
  V := Vector2D(fPoints[Pos], P);
  if Pos - 1 >= 0
    then fPoints[Pos - 1] := ShiftPoint(fPoints[Pos - 1], V);
  if Pos + 1 <= fPoints.Count - 1
    then fPoints[Pos + 1] := ShiftPoint(fPoints[Pos + 1], V);
  fPoints[Pos] := P;
end;

procedure TBezierPath2D.BreakPath(P: TPoint2D;
  Aperture, Precision: TRealType; var Obj1, Obj2: TPrimitive2D);
var
  I, J: Integer;
  Distance: TRealType;
begin
  Obj1 := nil;
  Obj2 := nil;
  Distance := Aperture;
  if not Pieces.IsPointOnStroke(
    Self, P, Precision, Distance, I) then Exit;
  if Aperture = 0 then
    if I = 0 then
      Exit
    else
      Dec(I)
  else if not InsertControlPoint0(I, P, Precision) > 0 then
    Exit;
  Obj1 := TBezierPath2D.Create(-1);
  Obj1.Assign(Self);
  Obj1.fPoints.Clear;
  Obj1.fPoints.Copy(fPoints, 0, (I + 1) * 3);
  Obj2 := TBezierPath2D.Create(-1);
  Obj2.Assign(Self);
  Obj2.fPoints.Clear;
  for J := (I + 1) * 3 to fPoints.Count - 1 do
    Obj2.fPoints.Add(fPoints[J]);
end;

// =====================================================================
// TClosedBezierPath2D
// =====================================================================

class function TClosedBezierPath2D.GetName: string;
begin
  Result := 'Closed Bezier path';
end;

constructor TClosedBezierPath2D.Create(ID: Integer);
begin
  inherited Create(ID);
  WhenCreated;
end;

constructor TClosedBezierPath2D.CreateSpec(ID: Integer; const
  Pts:
  array of TPoint2D);
begin
  inherited CreateSpec(ID, Pts);
  WhenCreated;
end;

procedure TClosedBezierPath2D.WhenCreated;
begin
end;

procedure TClosedBezierPath2D.FinishFirstDraw;
var
  PP: TPointsSet2D;
begin
  inherited FinishFirstDraw;
  PP := TPointsSet2D.Create(fPoints.Count * 3);
  try
    GetClosedHobbyBezier(PP, fPoints);
    fPoints.Clear;
    fPoints.DisableEvents := True;
    fPoints.Copy(PP, 0, PP.Count - 2);
    fPoints.DisableEvents := False;
  finally
    PP.Free;
  end;
  //FirstDrawPoint := 0;
  //    (OwnerDrawing as TDrawing2D).NotifyChanged;
  UpdateExtension(Self);
end;

procedure TClosedBezierPath2D.BezierPoints(PP: TPointsSet2D);
var
  I: Integer;
begin
  if fPoints.Count < 2 then Exit;
  PP.Clear;
  if (FirstDrawPoint >= 0) and (fPoints.Count >= 3) then
  begin
    PP.Expand(fPoints.Count * 3);
    GetClosedHobbyBezier(PP, fPoints);
    Exit;
  end;
  PP.Expand(((fPoints.Count + 2) div 3) * 3 + 1);
  PP[((fPoints.Count + 2) div 3) * 3] := fPoints[0];
  for I := 0 to (fPoints.Count) div 3 - 1 do
  begin
    PP[3 * I] := fPoints[3 * I];
    PP[3 * I + 1] := fPoints[3 * I + 1];
    PP[3 * I + 2] := fPoints[3 * I + 2];
  end;
  case fPoints.Count mod 3 of
    1:
      begin
        PP[PP.Count - 4] := fPoints[fPoints.Count - 1];
        PP[PP.Count - 3] := fPoints[fPoints.Count - 1];
        PP[PP.Count - 2] := fPoints[0];
      end;
    2:
      begin
        PP[PP.Count - 4] := fPoints[fPoints.Count - 2];
        PP[PP.Count - 3] := fPoints[fPoints.Count - 1];
        PP[PP.Count - 2] := fPoints[fPoints.Count - 1];
      end;
  end;
end;

function TClosedBezierPath2D.DeleteControlPoint0(I: Integer):
  Integer;
var
  TmpPt, Q1, Q2: TPoint2D;
  //DoSmooth: Boolean;
  function GetPoint(I: Integer): TPoint2D;
  begin
    if I < 0 then I := I + fPoints.Count;
    if I > fPoints.Count - 1 then I := I - fPoints.Count;
    Result := fPoints[I];
  end;
begin
  Result := 0;
  //DoSmooth := False;
  if (I < 0) or (I > fPoints.Count - 1) then Exit;
  if I mod 3 <> 0 then Exit;
  if I = fPoints.Count - 1 then
  begin
    fPoints.Delete(I);
    Inc(Result);
    //if I > 0 then fPoints.Delete(I - 1);
    Exit;
  end;
  DeleteBezierPointSmoothly(
    GetPoint(I - 3), GetPoint(I - 2), GetPoint(I - 1),
    GetPoint(I),
    GetPoint(I + 1), GetPoint(I + 2), GetPoint(I + 3), Q1, Q2);
  if I = 0 then
  begin
    if fPoints.Count mod 3 = 1 then
    begin
      fPoints.Delete(0);
      Inc(Result);
      if fPoints.Count < 1 then Exit;
      TmpPt := fPoints[0];
      fPoints.Delete(0);
      fPoints.Add(TmpPt);
      TmpPt := fPoints[0];
      fPoints.Delete(0);
      fPoints.Add(TmpPt);
      Exit;
    end;
    if fPoints.Count mod 3 = 0 then
      fPoints.Delete(fPoints.Count - 1);
    fPoints.Delete(1);
    Inc(Result);
    fPoints.Delete(0);
    Inc(Result);
    if fPoints.Count < 1 then Exit;
    TmpPt := fPoints[0];
    fPoints.Delete(0);
    fPoints.Add(TmpPt);
    fPoints[fPoints.Count - 2] := Q1;
    fPoints[fPoints.Count - 1] := Q2;
    Exit;
  end;
  fPoints.Delete(I + 1);
  Inc(Result);
  fPoints.Delete(I);
  Inc(Result);
  fPoints.Delete(I - 1);
  Inc(Result);
  //if DoSmooth then
  begin
    fPoints[I - 2] := Q1;
    fPoints[I - 1] := Q2;
  end;
end;

function TClosedBezierPath2D.InsertControlPoint0(const Pos:
  Integer; P: TPoint2D; Precision: TRealType): Integer;
var
  P0, P1, P2, P3, P4, P5, P6: TPoint2D;
  UStar: TRealType;
begin
  Result := 0;
  if (Pos < 0) or (Pos + 1 > fPoints.Count div 3) then Exit;
  P0 := fPoints[3 * Pos];
  P1 := fPoints[3 * Pos + 1];
  P2 := fPoints[3 * Pos + 2];
  if 3 * Pos <= fPoints.Count - 4 then
    P3 := fPoints[3 * Pos + 3]
  else
    P3 := fPoints[0];
  UStar := ClosestBezierPoint(P, P0, P1, P2, P3, Precision);
  BreakBezier(P0, P1, P2, P3, P4, P5, P6, UStar);
  fPoints[3 * Pos + 1] := P1;
  fPoints[3 * Pos + 2] := P5;
  fPoints.Insert(3 * Pos + 2, P4);
  fPoints.Insert(3 * Pos + 2, P3);
  fPoints.Insert(3 * Pos + 2, P2);
  Inc(Result, 3);
end;

procedure TClosedBezierPath2D.MoveControlPoint0(const Pos:
  Integer;
  P: TPoint2D; Shift: TShiftState);
var
  V: TVector2D;
  PosC, PosM: Integer;
begin
  if Pos mod 3 <> 0 then
    if not (SSAlt in Shift) xor SmoothBezierNodes then
    begin
      fPoints[Pos] := P;
      Exit;
    end
    else
    begin
      fPoints[Pos] := P;
      case Pos mod 3 of
        1:
          begin
            PosM := Pos - 2;
            if PosM < 0 then
              if fPoints.Count mod 3 = 0
                then
                PosM := fPoints.Count - 1
              else
                Exit;
            PosC := Pos - 1;
          end;
        2:
          begin
            PosM := Pos + 2;
            if PosM > fPoints.Count - 1 then
              if fPoints.Count mod 3 = 0 then
              begin
                PosM := 1;
                PosC := 0;
              end
              else
                Exit
            else
              PosC := Pos + 1;
          end;
      end;
      TurnBezierHandle(fPoints, Pos, PosC, PosM);
      Exit;
    end;
  if Pos mod 3 <> 0 then
  begin
    fPoints[Pos] := P;
    Exit;
  end;
  V := Vector2D(fPoints[Pos], P);
  if Pos - 1 >= 0
    then fPoints[Pos - 1] := ShiftPoint(fPoints[Pos - 1], V);
  if Pos + 1 <= fPoints.Count - 1
    then fPoints[Pos + 1] := ShiftPoint(fPoints[Pos + 1], V);
  if (Pos = 0) and (fPoints.Count mod 3 = 0)
    then
    fPoints[fPoints.Count - 1]
      := ShiftPoint(fPoints[fPoints.Count - 1], V);
  fPoints[Pos] := P;
end;

procedure TClosedBezierPath2D.BreakPath(P: TPoint2D;
  Aperture, Precision: TRealType; var Obj1, Obj2: TPrimitive2D);
var
  I, J: Integer;
  Distance: TRealType;
begin
  Obj1 := nil;
  Obj2 := nil;
  Distance := Aperture;
  if not Pieces.IsPointOnStroke(
    Self, P, Precision, Distance, I) then Exit;
  if Aperture = 0 then
    Dec(I)
  else if not InsertControlPoint0(I, P, Precision) > 0 then
    Exit;
  Obj1 := TBezierPath2D.Create(-1);
  Obj1.Assign(Self);
  Obj1.fPoints.Clear;
  for J := (I + 1) * 3 to fPoints.Count - 1 do
    Obj1.fPoints.Add(fPoints[J]);
  for J := 0 to (I + 1) * 3 do
    Obj1.fPoints.Add(fPoints[J]);
  Obj2 := nil;
end;

// =====================================================================
// TSmoothPath2D
// =====================================================================

constructor TSmoothPath2D0.Create(ID: Integer);
begin
  inherited Create(ID);
  WhenCreated;
end;

constructor TSmoothPath2D0.CreateSpec(ID: Integer;
  const Pts: array of TPoint2D);
begin
  inherited CreateSpec(ID, High(Pts) - Low(Pts) + 1, 20);
  fPoints.AddPoints(Pts);
  WhenCreated;
end;

procedure TSmoothPath2D0.WhenCreated;
begin
  fPoints.GrowingEnabled := True;
  fOwnsInterior := False;
  fCanDeletePoints := True;
end;

procedure TSmoothPath2D0.BezierPoints(PP: TPointsSet2D);
begin
  if fPoints.Count < 2 then Exit;
  PP.Clear;
  PP.Expand(fPoints.Count * 3 - 2);
  if fPoints.Count = 2 then
  begin
    PP[0] := fPoints[0];
    PP[1] := fPoints[0];
    PP[2] := fPoints[1];
    PP[3] := fPoints[1];
  end
  else
    GetHobbyBezier(PP, fPoints);
end;

procedure TSmoothPath2D0.Assign(const Obj: TGraphicObject);
var
  I, N: Integer;
  PP: TPointsSet2D;
  P, P1: TPoint2D;
  A, SA, EA: TRealType;
begin
  if (Obj = Self) then Exit;
  inherited Assign(Obj);
  if Obj is TPrimitive2D then
  begin
    fPoints.DisableEvents := True;
    if Obj is TSmoothPath2D0 then
    begin
      fPoints.Copy((Obj as TPrimitive2D).fPoints, 0,
        (Obj as TPrimitive2D).fPoints.Count - 1);
    end
    else if Obj is TCircle2D then
    begin
      P := (Obj as TCircle2D).fPoints[0];
      A := PointDistance2D(P, (Obj as TCircle2D).fPoints[1]);
      PP := TPointsSet2D.Create(5);
      for I := 0 to 4 - Byte(Self is TClosedSmoothPath2D) do
        PP.Add(Point2D(P.X + A * Cos(I * Pi / 2),
          P.Y + A * Sin(I * Pi / 2)));
      fPoints.Copy(PP, 0, PP.Count - 1);
      PP.Free;
    end
    else if Obj is TCircular2D then
    begin
      (Obj as TCircular2D).GetArcParams(P.X, P.Y, A, SA, EA);
      while SA > EA do
        EA := EA + 2 * Pi;
      P.W := 1;
      PP := TPointsSet2D.Create(5);
      N := Round(Abs(EA - SA) / Pi * 3);
      if N < 2 then N := 2;
      if not (Obj is TArc2D) then
      begin
        if N < 3 then N := 3;
        if Obj is TSegment2D then
          P1 := MidPoint(Point2D(P.X + A * Cos(SA), P.Y + A *
            Sin(SA)),
            Point2D(P.X + A * Cos(EA), P.Y + A * Sin(EA)))
        else
          P1 := P;
        PP.Add(P1);
      end;
      for I := 0 to N do
        PP.Add(Point2D(P.X + A * Cos(SA + I * (EA - SA) / N),
          P.Y + A * Sin(SA + I * (EA - SA) / N)));
      if not (Obj is TArc2D) and (Self is TSmoothPath2D) then
        PP.Add(P1);
      fPoints.Copy(PP, 0, PP.Count - 1);
      PP.Free;
    end
    {else if Obj is TStar2D then
    begin
      if (Obj as TStar2D).Pieces.Count > 0 then
      begin
        PP := (Obj as TStar2D).Pieces[0];
        if Self.IsClosed and
          IsSamePoint2D(PP[0], PP[PP.Count - 1]) then
          fPoints.Copy(PP, 0, PP.Count - 2)
        else
          fPoints.Copy(PP, 0, PP.Count - 1)
      end;
    end}
    else if (Obj as TPrimitive2D).fDrawPathBezier
      or (Obj is TRectangle2D)
      or (Obj is TStar2D) or (Obj is TSymbol2D) then
    begin
      fPoints.Clear;
      PP := TPointsSet2D.Create(0);
      try
        (Obj as TPrimitive2D).BezierPoints(PP);
        if (PP.Count > 0) and
          not (IsSamePoint2D(PP[0], PP[PP.Count - 1]) and
          (Self is TClosedSmoothPath2D))
          then fPoints.Add(PP[0]);
        for I := 0 to Pred((PP.Count - 1) div 3) do
        begin
          fPoints.AddPoints([
            BezierPoint(PP[3 * I], PP[3 * I + 1], PP[3 * I + 2],
              PP[3 * I + 3], 0.25),
              BezierPoint(PP[3 * I], PP[3 * I + 1], PP[3 * I +
              2],
                PP[3 * I + 3], 0.75),
              PP[3 * I + 3]]);
        end;
      finally
        PP.Free;
      end;
    end
    else if Obj is TPolyline2D0 then
    begin
      fPoints.Clear;
      PP := (Obj as TPolyline2D0).fPoints;
      for I := 0 to PP.Count - 2 do
      begin
        fPoints.AddPoints([MixPoint(PP[I], PP[I + 1], 0.25),
          MixPoint(PP[I], PP[I + 1], 0.75)]);
      end;
      if Obj is TPolygon2D then
      begin
        fPoints.AddPoints([MixPoint(PP[PP.Count - 1], PP[0],
            0.25),
          MixPoint(PP[PP.Count - 1], PP[0], 0.75)]);
      end;
    end
    else
    begin
      //BeginUseProfile;
      try
        {fPoints.Copy((Obj as TPrimitive2D).Profile.Item[0], 0,
          (Obj as TPrimitive2D).Profile.Item[0].Count - 1)}
        fPoints.Copy((Obj as TPrimitive2D).fPoints, 0,
          (Obj as TPrimitive2D).fPoints.Count - 1)
      finally
        //EndUseProfile;
      end;
      (Obj as TPrimitive2D).fDrawPathBezier := True;
    end;
    fPoints.DisableEvents := False;
    fPoints.GrowingEnabled := True;
    fOwnsInterior := False;
    fCanDeletePoints := True;
    fDrawPathBezier := True;
  end;
  UpdateExtension(Self);
end;

function TSmoothPath2D0.InsertControlPoint0(const Pos: Integer;
  P: TPoint2D; Precision: TRealType): Integer;
var
  PPP: TPointsSet2D;
  P0, P1, P2, P3: TPoint2D;
  U: TRealType;
begin
  Result := 0;
  if (Pos < 0) or (Pos >= fPoints.Count) then Exit;
  PPP := Pieces[0];
  P0 := PPP[Pos * 3];
  P1 := PPP[Pos * 3 + 1];
  P2 := PPP[Pos * 3 + 2];
  P3 := PPP[Pos * 3 + 3];
  U := ClosestBezierPoint(P, P0, P1, P2, P3, Precision);
  P := BezierPoint(P0, P1, P2, P3, U);
  if Pos < fPoints.Count - 1 then
    fPoints.Insert(Pos + 1, P)
  else
    fPoints.Add(P);
  Inc(Result);
end;

// =====================================================================
// TSmoothPath2D
// =====================================================================

class function TSmoothPath2D.GetName: string;
begin
  Result := 'Curve';
end;

procedure TSmoothPath2D.BreakPath(P: TPoint2D;
  Aperture, Precision: TRealType; var Obj1, Obj2: TPrimitive2D);
var
  I, J: Integer;
  Distance: TRealType;
begin
  Obj1 := nil;
  Obj2 := nil;
  Distance := Aperture;
  if not Pieces.IsPointOnStroke(
    Self, P, Precision, Distance, I) then Exit;
  if not InsertControlPoint0(I, P, Precision) > 0 then Exit;
  Obj1 := TSmoothPath2D.Create(-1);
  Obj1.Assign(Self);
  Obj1.fPoints.Clear;
  Obj1.fPoints.Copy(fPoints, 0, I + 1);
  Obj2 := TSmoothPath2D.Create(-1);
  Obj2.Assign(Self);
  Obj2.fPoints.Clear;
  for J := I + 1 to fPoints.Count - 1 do
    Obj2.fPoints.Add(fPoints[J]);
end;

// =====================================================================
// TClosedSmoothPath2D
// =====================================================================

class function TClosedSmoothPath2D.GetName: string;
begin
  Result := 'Closed curve';
end;

constructor TClosedSmoothPath2D.Create(ID: Integer);
begin
  inherited Create(ID);
  WhenCreated;
end;

constructor TClosedSmoothPath2D.CreateSpec(ID: Integer; const
  Pts:
  array of TPoint2D);
begin
  inherited CreateSpec(ID, Pts);
  WhenCreated;
end;

procedure TClosedSmoothPath2D.WhenCreated;
begin
end;

procedure TClosedSmoothPath2D.BezierPoints(PP: TPointsSet2D);
begin
  if fPoints.Count < 2 then Exit;
  PP.Clear;
  PP.Expand(fPoints.Count * 3 + 1);
  if fPoints.Count = 2 then
  begin
    PP[0] := fPoints[0];
    PP[1] := fPoints[0];
    PP[2] := fPoints[1];
    PP[3] := fPoints[1];
    PP[4] := fPoints[1];
    PP[5] := fPoints[0];
    PP[6] := fPoints[0];
  end
  else
    GetClosedHobbyBezier(PP, fPoints);
end;

procedure TClosedSmoothPath2D.BreakPath(P: TPoint2D;
  Aperture, Precision: TRealType; var Obj1, Obj2: TPrimitive2D);
var
  I, J: Integer;
  Distance: TRealType;
begin
  Obj1 := nil;
  Obj2 := nil;
  Distance := Aperture;
  if not Pieces.IsPointOnStroke(
    Self, P, Precision, Distance, I) then Exit;
  if not InsertControlPoint0(I, P, Precision) > 0 then Exit;
  Obj1 := TSmoothPath2D.Create(-1);
  Obj1.Assign(Self);
  Obj1.fPoints.Clear;
  for J := I + 1 to fPoints.Count - 1 do
    Obj1.fPoints.Add(fPoints[J]);
  for J := 0 to I + 1 do
    Obj1.fPoints.Add(fPoints[J]);
  Obj2 := nil;
end;

// =====================================================================
// TText2D
// =====================================================================

class function TText2D.GetName: string;
begin
  Result := 'Text';
end;

procedure TText2D.ResetJustification;
begin
  UpdateExtension(Self);
end;

procedure TText2D.SetHJustification(J: THJustification);
begin
  fHJustification := J;
  ResetJustification;
end;

procedure TText2D.SetVJustification(J: TVJustification);
begin
  fVJustification := J;
  ResetJustification;
end;

constructor TText2D.Create(ID: Integer);
begin
  inherited CreateSpec(ID, 2, 0);
  fPoints.DisableEvents := True;
  try
    fHeight := DefaultFontHeight_Default;
    fText := '';
    WritableBBox := Rect2D(0, 0, 1, 1);
    fPoints.Add(Point2D(0, 0));
  finally
    fPoints.DisableEvents := False;
    UpdateExtension(Self);
  end;
  WhenCreated;
end;

constructor TText2D.CreateSpec(ID: Integer; P: TPoint2D; Height:
  TRealType; Txt: AnsiString);
begin
  inherited CreateSpec(ID, 2, 0);
  fPoints.DisableEvents := True;
  try
    fHeight := Height;
    fText := Txt;
    WritableBBox := Rect2D(P.X, P.Y, P.X + 1, P.Y + 1);
    fPoints.Add(P);
  finally
    fPoints.DisableEvents := False;
    UpdateExtension(Self);
  end;
  WhenCreated;
  if (Pos('$', Txt) > 0) or (Pos('\', Txt) > 0)
    then
    if Length(Txt) > 1 then fTeXText := Txt;
end;

procedure TText2D.WhenCreated;
begin
  fTeXText := '';
  fRot := 0;
  Font := TFont.Create;
  Font.Name := ' ';
  fPoints.GrowingEnabled := False;
end;

destructor TText2D.Destroy;
begin
  Font.Free;
  inherited Destroy;
end;

function TText2D.GetWideText: Widestring;
var
  St, StCode: string;
  I: Integer;
begin
  St := fText;
  Result := '';
  I := Pos('&#', St);
  while I > 0 do
  begin
    Result := Result + Copy(St, 1, I - 1);
    Delete(St, 1, I + 1);
    I := Pos(';', St);
    if I = 0 then I := Length(St) + 1;
    StCode := Copy(St, 1, I - 1);
    if (StCode <> '') and (StCode[1] = 'x') then
      StCode[1] := '$';
    Result := Result + Widechar(StrToInt(StCode));
    Delete(St, 1, I);
    I := Pos('&#', St);
  end;
  Result := Result + St;
end;

function TText2D.GetFaceName: string;
begin
  if Assigned(Font) and (Font.Name <> ' ') then
    Result := Font.Name
  else if Assigned(ParentDrawing)
    and ((ParentDrawing as TDrawing2D).FontName <> '') then
    Result := (ParentDrawing as TDrawing2D).FontName
  else if FontName_Default <> '' then
    Result := FontName_Default
  else
    Result := 'Times New Roman';
end;

function TText2D.GetStyle: TFontStyles;
begin
  if Assigned(Font) and (Font.Name <> ' ') then
    Result := Font.Style
  else
    Result := [];
end;

function TText2D.GetCharset: TFontCharSet;
begin
  if Assigned(Font) and (Font.Name <> ' ') then
    Result := Font.Charset
  else
    Result := DEFAULT_CHARSET;
end;

constructor TText2D.CreateFromStream(const Stream: TStream);
var
  TmpInt: Integer;
  TmpSt: string;
begin
  { Load the standard properties }
  inherited;
  with Stream do
  begin
    Read(TmpInt, SizeOf(TmpInt));
    SetString(fText, nil, TmpInt);
    Read(Pointer(fText)^, TmpInt);
    Read(TmpInt, SizeOf(TmpInt));
    SetString(fTeXText, nil, TmpInt);
    Read(Pointer(fTeXText)^, TmpInt);
    Read(fHeight, SizeOf(fHeight));
    Read(fRot, SizeOf(fRot));
    Read(fHJustification, SizeOf(fHJustification));
    Read(fVJustification, SizeOf(fVJustification));
    Font := TFont.Create;
    Read(TmpInt, SizeOf(TmpInt));
    SetString(TmpSt, nil, TmpInt);
    Read(Pointer(TmpSt)^, TmpInt);
    Font.Name := TmpSt;
    Read(TmpInt, SizeOf(TmpInt));
    if TmpInt = 1 then Font.Style := Font.Style + [fsItalic];
    Read(TmpInt, SizeOf(TmpInt));
    if TmpInt = 1 then Font.Style := Font.Style + [fsBold];
    Read(TmpInt, SizeOf(TmpInt));
    Font.Charset := TmpInt;
  end;
end;

procedure TText2D.SaveToStream(const Stream: TStream);
var
  TmpInt: Integer;
  TmpSt: string;
begin
  { Save the standard properties }
  inherited SaveToStream(Stream);
  with Stream do
  begin
    TmpInt := Length(fText);
    Write(TmpInt, SizeOf(TmpInt));
    Write(Pointer(fText)^, TmpInt);
    TmpInt := Length(fTeXText);
    Write(TmpInt, SizeOf(TmpInt));
    Write(Pointer(fTeXText)^, TmpInt);
    Write(fHeight, SizeOf(fHeight));
    Write(fRot, SizeOf(fRot));
    Write(fHJustification, SizeOf(fHJustification));
    Write(fVJustification, SizeOf(fVJustification));
    TmpInt := Length(Font.Name);
    Write(TmpInt, SizeOf(TmpInt));
    TmpSt := Font.Name;
    Write(Pointer(TmpSt)^, TmpInt);
    TmpInt := Integer(fsItalic in Font.Style);
    Write(TmpInt, SizeOf(TmpInt));
    TmpInt := Integer(fsBold in Font.Style);
    Write(TmpInt, SizeOf(TmpInt));
    TmpInt := Font.Charset;
    Write(TmpInt, SizeOf(TmpInt));
  end;
end;

procedure TText2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then Exit;
  inherited Assign(Obj);
  if Obj is TPrimitive2D then
  begin
    if (Obj as TPrimitive2D).fPoints.Count > 0 then
      fPoints.Copy((Obj as TPrimitive2D).fPoints, 0, 0);
    fPoints.GrowingEnabled := False;
    fText := 'text';
  end;
  if Obj is TText2D then
  begin
    fText := (Obj as TText2D).Text;
    fTeXText := (Obj as TText2D).TeXText;
    fHeight := (Obj as TText2D).Height;
    fRot := (Obj as TText2D).Rot;
    if not Assigned(Font) then
      Font := TFont.Create;
    Font.Assign(TText2D(Obj).Font);
    fHJustification := (Obj as TText2D).fHJustification;
    fVJustification := (Obj as TText2D).fVJustification;
  end;
  UpdateExtension(Self);
end;

procedure TText2D.FillPieces;
var
  APiece: TTextPiece;
begin
  APiece := TTextPiece.Create(1);
  Pieces.Add(APiece);
  APiece.Add(fPoints[0]);
  APiece.Fill := pfiNone;
  APiece.ARot := Rot;
  APiece.WideText := GetWideText;
  APiece.TeXText := TeXText;
  APiece.Height := Height;
  APiece.HJustification := HJustification;
  APiece.VJustification := VJustification;
  APiece.FaceName := GetFaceName;
  APiece.Style := GetStyle;
  APiece.Charset := GetCharset;
  APiece.Closed := True;
end;

procedure TText2D.TransForm(const T: TTransf2D);
var
  V: TVector2D;
begin
  fPoints[0] := TransformPoint2D(fPoints[0], T);
  V := PolarVector(fHeight, fRot);
  if ScaleText then V := TransformVector2D(V, T);
  if RotateText then fRot := VectorAngle(V);
  fHeight := VectorLength2D(V);
  UpdateExtension(Self);
end;

// =====================================================================
// TSymbol2D
// =====================================================================

class function TSymbol2D.GetName: string;
begin
  Result := 'Graphical symbol';
end;

const
  DecisionArr: array[1..5] of TSimplePoint2D
  = ((0.5, 1), (0, 0.5), (0.5, 0), (1, 0.5), (0.5, 1));

  Baloon1Arr: array[1..8] of TSimplePoint2D
  = ((0, 1), (1, 1), (1, 0.33), (0.4, 0.33),
    (0.13, 0.05), (0.2, 0.33), (0, 0.33), (0, 1));

procedure TSymbol2D.SetHJustification(J: THJustification);
begin
  fHJustification := J;
  UpdateExtension(Self);
end;

procedure TSymbol2D.SetVJustification(J: TVJustification);
begin
  fVJustification := J;
  UpdateExtension(Self);
end;

procedure TSymbol2D.FillPieces;
var
  P0: TPoint2D;
  HShift, VShift: TRealType;
  procedure FillPiece(const PArr: PSimplePointArr2D;
    const Count: Integer);
  var
    APiece: TPiece;
    I: Integer;
  begin
    APiece := TLinPath.Create(30);
    Pieces.Add(APiece);
    for I := 0 to Pred(Count) do
      APiece.Add(Point2D(PArr^[I][1], PArr^[I][2]));
    APiece.Closed := True;
  end;
  procedure SetPiecesAttr;
  var
    APiece: TPiece;
    I: Integer;
  begin
    for I := 0 to Pieces.Count - 1 do
    begin
      APiece := Pieces[I];
      APiece.Line := pliSolidDefault;
      APiece.Fill := pfiDefault;
      APiece.Hatch := phaNone;
    end;
  end;
begin
  Pieces.Clear;
  case SymbolKind of
    symProcess: Pieces.AddFromSvgPathT(
        'M -1000 -668 L -1000 668 L 1000 668 L 1000 -668 Z',
        SVG_Transform_Parse('scale(0.001 0.001)'));
    symInputOutput: Pieces.AddFromSvgPathT(
        'M 0.17 1 L 1 1 L 0.83 0 L 0 0 Z',
        SVG_Transform_Parse('scale(2 1.336) translate(-0.5, -0.5)'));
    symPreparation: Pieces.AddFromSvgPathT(
        'M 0.17 1 L 0.83 1 L 1 0.5 L 0.83 0 L 0.17 0 L 0 0.5 Z',
        SVG_Transform_Parse('scale(2 1.336) translate(-0.5, -0.5)'));
    symPunchCard: Pieces.AddFromSvgPathT(
        'M 0.1 1 L 1 1 L 1 0 L 0 0 L 0 0.8 Z',
        SVG_Transform_Parse('scale(2 1) translate(-0.5, -0.5)'));
    symManualOperation: Pieces.AddFromSvgPathT(
        'M 0 1 L 1 1 L 0.75 0 L 0.25 0 Z',
        SVG_Transform_Parse('scale(2 1.336) translate(-0.5, -0.5)'));
    symKeyboard: Pieces.AddFromSvgPathT(
        'M 0 0.7 L 1 1 L 1 0 L 0 0 Z',
        SVG_Transform_Parse('scale(2 1) translate(-0.5, -0.5)'));
    symPunchTape: Pieces.AddFromSvgPathT(
        'M 0 0.91 V 0.18 C 0 0.13 0.03 0.08 0.05 0.06 C 0.12 0.01 0.2 0 0.28 0.02 C 0.44 0.05 0.6 0.18 0.76 0.18 ' +
        'C 0.84 0.18 0.93 0.15 1 0.09 V 0.82 C 1 0.87 0.97 0.92 0.95 0.94 C 0.88 0.99 0.8 1 0.72 0.98 C 0.56 0.95 0.4 0.82 0.24 0.82 C 0.16 0.82 0.07 0.85 0 0.91 Z',
        SVG_Transform_Parse('scale(2 1.05) translate(-0.5, -0.5)'));
    symDocument: Pieces.AddFromSvgPathT(
        'M 0 1 V 0.1 C 0.07 0.02 0.16 0 0.25 0 C 0.5 0 0.65 0.27 0.88 0.27 C 0.92 0.27 0.96 0.26 1 0.25 V 1 H 0 Z',
        SVG_Transform_Parse('scale(2 1.336) translate(-0.5, -0.5)'));
    symDocuments:
      begin
        Pieces.AddFromSvgPathT('M 0.96 0.34 L 1 0.33 L 1 1 L 0.08 1 L 0.08 0.945',
          SVG_Transform_Parse('scale(2.174 1.501) translate(-0.46, -0.445)'));
        Pieces.AddFromSvgPathT('M 0.92 0.29 L 0.96 0.28 L 0.96 0.945 L 0.04 0.945 L 0.04 0.89',
          SVG_Transform_Parse('scale(2.174 1.501) translate(-0.46, -0.445)'));
        Pieces.AddFromSvgPathT(
          'M 0	0.89 C 0 0.62 0 0.13 0 0.09 C 0.06 0.01 0.15 0 0.23 0 C 0.46 0 0.6 0.24 0.81 0.24 C 0.85 0.24 0.88 0.24 0.92 0.23 C 0.92 0.32 0.92 0.79 0.92 0.89 C 0.72 0.89 0.13 0.89 0	0.89 Z',
          SVG_Transform_Parse('scale(2.174 1.501) translate(-0.46, -0.445)'));
      end;
    symDisplay: Pieces.AddFromSvgPathT(
        'M 0.02 0.59 C 0.01 0.56 0 0.54 0 0.5 C 0 0.46 0.01 0.44 0.02 0.41 C 0.06 0.34 0.1 0.27 0.15 0.2 ' +
        'C 0.2 0.12 0.26 0.05 0.33 0 H 0.87 C 0.91 0.06 0.93 0.13 0.95 0.21 C 1 0.4 1 0.6 0.95 0.79 C 0.93 0.87 0.91 0.94 0.87 1 ' +
        'H 0.33 C 0.26 0.95 0.2 0.88 0.15 0.8 C 0.1 0.73 0.06 0.66 0.02 0.59 Z',
        SVG_Transform_Parse('scale(2 1.336) translate(-0.5, -0.5)'));
    symTerminal: Pieces.AddFromSvgPathT(
        'M 0.21 0  H 0.79 C 1 0 1 1 0.79 1 H 0.21 C 0 1 0 0 0.21 0 Z',
        SVG_Transform_Parse('scale(2.2 0.73) translate(-0.5, -0.5)'));
    symKeying: Pieces.AddFromSvgPathT(
        'M 0.1 1 C 0.9 1 0.9 1 0.9 1 C 0.96 1 1 0.74 1 0.5 C 1 0.25 0.96 0 0.9 0 C 0.1 0 0.1 0 0.1 0 C 0.05 0 0 0.25 0 0.5 C 0 0.74 0.05 1 0.1 1 Z',
        SVG_Transform_Parse('scale(2.4 1.336) translate(-0.5, -0.5)'));
    symOnlineStorage: Pieces.AddFromSvgPathT(
        'M 0.11 1 C 1 1 1 1 1 1 C 0.94 1 0.89 0.75 0.89 0.5 C 0.89 0.25 0.94 0 1 0 C 0.11 0 0.11 0 0.11 0 C 0.06 0 0 0.25 0 0.5 C 0 0.75 0.06 1 0.11 1 Z',
        SVG_Transform_Parse('scale(2 1.336) translate(-0.5, -0.5)'));
    symAlternateProcess: Pieces.AddFromSvgPathT(
        'M 0.05 1 C 0.02 1 0 0.95 0 0.88 V 0.13 C 0 0.06 0.02 0 0.05 0 H 0.95 C 0.98 0 1 0.06 1 0.13 V 0.88 C 1 0.95 0.98 1 0.95 1 H 0.05 Z',
        SVG_Transform_Parse('scale(2 1.336) translate(-0.5, -0.5)'));
    symMagneticDrum:
      begin
        Pieces.AddFromSvgPathT(
          'M 0.1 1 C 0.9 1 0.9 1 0.9 1 C 0.95 1 1 0.76 1 0.5 C 1 0.26 0.95 0 0.9 0 C 0.1 0 0.1 0 0.1 0 C 0.05 0 0 0.26 0 0.5 C 0 0.76 0.05 1 0.1 1 Z',
          SVG_Transform_Parse('scale(2.4 1.336) translate(-0.5, -0.5)'));
        Pieces.AddFromSvgPathT(
          'M 0.9 1 C 0.85 1 0.8 0.76 0.8 0.5 C 0.8 0.26 0.85 0 0.9 0',
          SVG_Transform_Parse('scale(2.4 1.336) translate(-0.5, -0.5)'));
      end;
    symMagneticTape: Pieces.AddFromSvgPathT(
        'M 1000 0 C 1000 552 552 1000 0 1000 C -552 1000 -1000 552 -1000 0 C -1000 -552 -552 -999 0 -1000 H 1000 V -902 H 432 C 768 -741 1000 -398 1000 0 Z',
        //'M 0.87 0.16 C 0.95 0.25 1 0.37 1 0.5 C 1 0.78 0.78 1 0.5 1 C 0.22 1 0 0.78 0 0.5 C 0 0.23 0.22 0 0.49 0 C 0.64 0 0.83 0 0.97 0 C 0.97 0.05 0.97 0.11 0.97 0.16 C 0.94 0.16 0.9 0.16 0.87 0.16',
        SVG_Transform_Parse('scale(0.00068 0.00068) translate(-0.5, -0.5)'));
    symHoarrow1: Pieces.AddFromSvgPathT(
        'M 1 0.5 L 0.8 1 L 0.8 0.75 L 0 0.75 L 0 0.25 L 0.8 0.25 L 0.8 0 L 1 0.5 Z',
        SVG_Transform_Parse('scale(2 0.8) translate(-0.5, -0.5)'));
    symHoarrow1v: Pieces.AddFromSvgPathT(
        'M 1 0.5 L 0.8 1 L 0.8 0.75 L 0 0.75 L 0.12 0.5 L 0 0.25 L 0.8 0.25 L 0.8 0 L 1 0.5 Z',
        SVG_Transform_Parse('scale(2 0.8) translate(-0.5, -0.5)'));
    symHoarrow2: Pieces.AddFromSvgPathT(
        'M 0 0.5 L 0.2 0 L 0.2 0.25 L 0.8 0.25 L 0.8 0 L 1 0.5 L 0.8 1 L 0.8 0.75 L 0.2 0.75 L 0.2 1 L 0 0.5 Z',
        SVG_Transform_Parse('scale(2 0.8) translate(-0.5, -0.5)'));
    symHoarrow3: Pieces.AddFromSvgPathT(
        'M 0.5 1 L 0.3 0.8 L 0.4 0.8 L 0.4 0.6 L 0.2 0.6 L 0.2 0.7 L 0 0.5 L 0.2 0.3 L 0.2 0.4 L 0.8 0.4 L 0.8 0.3 L 1 0.5 L 0.8 0.7 L 0.8 0.6 L 0.6 0.6 L 0.6 0.8 L 0.7 0.8 L 0.5 1 Z',
        SVG_Transform_Parse('scale(2) translate(-0.5, -0.5)'));
    symHoarrow4: Pieces.AddFromSvgPathT(
        'M 0.5 1 L 0.3 0.8 L 0.4 0.8 L 0.4 0.6 L 0.2 0.6 L 0.2 0.7 L 0 0.5 L 0.2 0.3 L 0.2 0.4 L 0.4 0.4 L 0.4 0.2 L 0.3 0.2 L 0.5 0 L 0.7 0.2 L 0.6 0.2 L 0.6 0.4 L 0.8 0.4 L 0.8 0.3 L 1 0.5 L 0.8 0.7 L 0.8 0.6 L 0.6 0.6 L 0.6 0.8 L 0.7 0.8 L 0.5 1 Z',
        SVG_Transform_Parse('scale(2) translate(-0.5, -0.5)'));
    symStar5: Pieces.AddFromSvgPathT(
        'M 951 309 L 363 -118 L 588 -809 L 0 -382 L -588 -809 L -363 -118 L -951 309 L -225 309 L 0 1000 L 225 309 Z',
        SVG_Transform_Parse('scale(0.001)'));
    symDiamond8: Pieces.AddFromSvgPathT(
        'M -1000 0 L -707 429 L -5 712 L 717 429 L 1000 0 L 717 -429 L -5 -712 L -707 -429 Z',
        SVG_Transform_Parse('scale(0.001)'));
    symBaloon1:
      begin
        FillPiece(@Baloon1Arr, Length(Baloon1Arr));
        Pieces.TransForm(Translate2D(-0.5, -0.665));
        Pieces.TransForm(Scale2D(2, 2));
      end;
    symBaloon2: Pieces.AddFromSvgPathT(
        'M 0.93 0.28 C 0.98 0.32 1 0.4 1 0.46 V 0.82 C 1 0.92 0.93 1 0.84 1 H 0.16 C 0.07 1 0 0.92 0 0.82 ' +
        'V 0.46 C 0 0.36 0.07 0.28 0.16 0.28 H 0.73 L 0.66 0 L 0.93 0.28 Z',
        SVG_Transform_Parse('scale(2) translate(-0.5, -0.64)'));
    symCloud1: Pieces.AddFromSvgPathT(
        'M 258 505 C 423 632 795 468 747 266 C 1050 266 1075 -82 854 -116 C 990 -270 643 -661 303 -504 C 341 -722 -281 -717 -296 -494 ' +
        'C -493 -616 -930 -483 -789 -250 C -963 -279 -1142 258 -828 298 C -918 483 -674 662 -525 462 C -530 712 236 735 258 505 Z',
        SVG_Transform_Parse('scale(0.001)'));
    symSplash1: Pieces.AddFromSvgPathT(
        'M 0.5 0.73 L 0.39 0.89 L 0.34 0.71 L 0.02 0.89 L 0.21 0.65 L 0 0.6 L 0.17 0.45 L 0.01 0.32 L 0.26 0.35 L 0.22 0.18 ' +
        'L 0.36 0.28 L 0.39 0 L 0.49 0.31 L 0.61 0.09 L 0.65 0.33 L 0.84 0.16 L 0.78 0.4 L 1 0.38 L 0.81 0.52 L 0.98 0.62 L 0.77 0.66 L 0.85 0.79 L 0.66 0.75 L 0.67 1 Z',
        SVG_Transform_Parse('scale(2) translate(-0.5, -0.5)'));
    symSnowFlake1: Pieces.AddFromSvgPathT(
        'M 0.45 1 L 0.55 1 L 0.55 0.86 L 0.69 0.93 L 0.69 0.84 L 0.55 0.76 L 0.55 0.58 L 0.73 0.67 L 0.72 0.81 L 0.81 0.86 L 0.82 0.72 ' +
        'L 0.95 0.79 L 1 0.71 L 0.86 0.64 L 1 0.57 L 0.91 0.52 L 0.77 0.59 L 0.6 0.5 L 0.77 0.41 L 0.91 0.48 L 1 0.43 L 0.86 0.36 L 1 0.29 ' +
        'L 0.95 0.21 L 0.82 0.28 L 0.81 0.14 L 0.72 0.19 L 0.73 0.32 L 0.55 0.42 L 0.55 0.24 L 0.69 0.16 L 0.69 0.07 L 0.55 0.14 L 0.55 0 ' +
        'L 0.45 0 L 0.45 0.14 L 0.31 0.07 L 0.31 0.16 L 0.45 0.23 L 0.45 0.42 L 0.27 0.32 L 0.28 0.19 L 0.19 0.14 L 0.18 0.28 L 0.05 0.21 L 0 0.29 ' +
        'L 0.14 0.36 L 0 0.43 L 0.09 0.48 L 0.23 0.41 L 0.4 0.5 L 0.23 0.59 L 0.09 0.52 L 0 0.57 L 0.14 0.64 L 0 0.71 L 0.05 0.79 L 0.18 0.72 ' +
        'L 0.19 0.86 L 0.28 0.81 L 0.27 0.67 L 0.45 0.58 L 0.45 0.76 L 0.31 0.83 L 0.31 0.93 L 0.45 0.86 L 0.45 1 Z',
        SVG_Transform_Parse('scale(1.825 2) translate(-0.5, -0.5)'));
  else //symDecision
    begin
      FillPiece(@DecisionArr, Length(DecisionArr));
      Pieces.TransForm(Translate2D(-0.5, -0.5));
      Pieces.TransForm(Scale2D(2, 1.336));
    end;
  end;
  SetPiecesAttr;
  P0 := fPoints[0];
  case fHJustification of
    jhLeft: HShift := P0.X - fDiameter / 2;
    jhCenter: HShift := P0.X;
    jhRight: HShift := P0.X + fDiameter;
  end;
  case fVJustification of
    jvBottom: VShift := P0.Y - fDiameter / 2;
    jvCenter: VShift := P0.Y;
    jvTop: VShift := P0.Y + fDiameter / 2;
  end;
  Pieces.TransForm(Scale2D(fDiameter / 2, fDiameter / 2));
  Pieces.TransForm(Rotate2D(fRot));
  Pieces.TransForm(Translate2D(HShift, VShift));
end;

constructor TSymbol2D.Create(ID: Integer);
begin
  inherited CreateSpec(ID, 2, 150);
  fPoints.DisableEvents := True;
  try
    fDiameter := DefaultSymbolSize_Default;
    WritableBBox := Rect2D(0, 0, 1, 1);
    fPoints.Add(Point2D(0, 0));
  finally
    fPoints.DisableEvents := False;
    UpdateExtension(Self);
  end;
  WhenCreated;
end;

constructor TSymbol2D.CreateSpec(ID: Integer; P: TPoint2D;
  Diameter: TRealType);
begin
  inherited CreateSpec(ID, 2, 150);
  fPoints.DisableEvents := True;
  try
    fDiameter := Diameter;
    WritableBBox := Rect2D(P.X, P.Y, P.X + 1, P.Y + 1);
    fPoints.Add(P);
  finally
    fPoints.DisableEvents := False;
    UpdateExtension(Self);
  end;
  WhenCreated;
end;

procedure TSymbol2D.WhenCreated;
begin
  SymbolKind := symDecision;
  LineStyle := liSolid;
  LineWidth := 1;
  fRot := 0;
  fPoints.GrowingEnabled := False;
  fHJustification := jhCenter;
  fVJustification := jvCenter;
end;

procedure TSymbol2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then Exit;
  inherited Assign(Obj);
  if (Obj is TPrimitive2D) then
    if not (Obj is TSymbol2D) then
    begin
      fPoints[0] := BoxCenter((Obj as TPrimitive2D).BoundingBox);
      SymbolKind := symDecision;
      WhenCreated;
    end
    else // Obj is TSymbol2D)
    begin
      if (Obj as TPrimitive2D).fPoints.Count > 0 then
        fPoints.Copy((Obj as TPrimitive2D).fPoints, 0, 0);
      fSymbolKind := (Obj as TSymbol2D).fSymbolKind;
      fDiameter := (Obj as TSymbol2D).fDiameter;
      fRot := (Obj as TSymbol2D).fRot;
      fHJustification := (Obj as TSymbol2D).fHJustification;
      fVJustification := (Obj as TSymbol2D).fVJustification;
      fPoints.GrowingEnabled := False;
    end;
  UpdateExtension(Self);
end;

constructor TSymbol2D.CreateFromStream(const Stream: TStream);
begin
  inherited;
  with Stream do
  begin
    Read(fSymbolKind, SizeOf(fSymbolKind));
    Read(fDiameter, SizeOf(fDiameter));
    Read(fRot, SizeOf(fRot));
    Read(fHJustification, SizeOf(fHJustification));
    Read(fVJustification, SizeOf(fVJustification));
  end;
end;

procedure TSymbol2D.SaveToStream(const Stream: TStream);
begin
  inherited SaveToStream(Stream);
  with Stream do
  begin
    Write(fSymbolKind, SizeOf(fSymbolKind));
    Write(fDiameter, SizeOf(fDiameter));
    Write(fRot, SizeOf(fRot));
    Write(fHJustification, SizeOf(fHJustification));
    Write(fVJustification, SizeOf(fVJustification));
  end;
end;

procedure TSymbol2D.TransForm(const T: TTransf2D);
var
  V: TVector2D;
begin
  fPoints[0] := TransformPoint2D(fPoints[0], T);
  V := PolarVector(fDiameter, fRot);
  V := TransformVector2D(V, T);
  if RotateSymbols then fRot := VectorAngle(V);
  fDiameter := VectorLength2D(V);
  if ScaleLineWidth then
    LineWidth := LineWidth * IsotropicScale(T);
  UpdateExtension(Self);
end;

// =====================================================================
// TBitmap2D
// =====================================================================

class function TBitmap2D.GetName: string;
begin
  Result := 'Bitmap';
end;

procedure TBitmap2D.SetScaleFactor(SF: TRealType);
var
  TmpPt: TPoint2D;
begin
  if (fScaleFactor <> SF) then
  begin
    fScaleFactor := SF;
    if (fScaleFactor <> 0.0) then
    begin
      if (fAspectRatio <> 0.0) then
        TmpPt.X := fPoints[0].X + fBitmap.Height * fScaleFactor
          /
          fAspectRatio
      else
        TmpPt.X := fPoints[0].X + fBitmap.Width * fScaleFactor;
      TmpPt.Y := fPoints[0].Y + fBitmap.Height * fScaleFactor;
      TmpPt.W := 1.0;
      fPoints[1] := TmpPt;
    end;
  end;
end;

procedure TBitmap2D.SetAspectRatio(AR: TRealType);
var
  TmpPt: TPoint2D;
begin
  if (fAspectRatio <> AR) then
  begin
    fAspectRatio := AR;
    if (fScaleFactor <> 0.0) then
    begin
      if (fAspectRatio <> 0.0) then
        TmpPt.X := fPoints[0].X + fBitmap.Height * fScaleFactor
          /
          fAspectRatio
      else
        TmpPt.X := fPoints[0].X + fBitmap.Width * fScaleFactor;
      TmpPt.Y := fPoints[0].Y + fBitmap.Height * fScaleFactor;
      TmpPt.W := 1.0;
      fPoints[1] := TmpPt;
    end;
  end;
end;

constructor TBitmap2D.Create(ID: Integer);
begin
  inherited CreateSpec(ID, 2, 0);
  fPoints.DisableEvents := True;
  try
    fBitmap := TBitmap.Create;
    fPoints.Add(Point2D(0, 0));
    fPoints.Add(Point2D(0, 0));
  finally
    fPoints.DisableEvents := False;
    UpdateExtension(Self);
  end;
  WhenCreated;
end;

constructor TBitmap2D.CreateSpec(ID: Integer; const P1, P2:
  TPoint2D; BMP: TBitmap);
begin
  inherited CreateSpec(ID, 2, 0);
  fPoints.DisableEvents := True;
  try
    fBitmap := TBitmap.Create;
    fBitmap.Assign(BMP);
    fPoints.Add(P1);
    fPoints.Add(P2);
  finally
    fPoints.DisableEvents := False;
    UpdateExtension(Self);
  end;
  WhenCreated;
end;

procedure TBitmap2D.WhenCreated;
begin
  fScaleFactor := 0.0;
  fAspectRatio := 1.0;
  fCopyMode := cmSrcCopy;
  fPoints.GrowingEnabled := False;
end;

procedure TBitmap2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then Exit;
  inherited;
  if (Obj is TBitmap2D) or (Obj is TRectangle2D) then
  begin
    fScaleFactor := TBitmap2D(Obj).ScaleFactor;
    fAspectRatio := TBitmap2D(Obj).AspectRatio;
    fCopyMode := TBitmap2D(Obj).CopyMode;
    if Obj is TBitmap2D then
      fBitmap.Assign(TBitmap2D(Obj).fBitmap);
    fPoints.Copy((Obj as TPrimitive2D).fPoints, 0, 1);
    fPoints.GrowingEnabled := True;
  end;
end;

destructor TBitmap2D.Destroy;
begin
  fBitmap.Free;
  inherited Destroy;
end;

constructor TBitmap2D.CreateFromStream(const Stream: TStream);
begin
  { Load the standard properties }
  inherited;
  fBitmap := TBitmap.Create;
  fBitmap.LoadFromStream(Stream);
  Stream.Read(fScaleFactor, SizeOf(fScaleFactor));
  Stream.Read(fAspectRatio, SizeOf(fAspectRatio));
  Stream.Read(fCopyMode, SizeOf(fCopyMode));
end;

procedure TBitmap2D.SaveToStream(const Stream: TStream);
begin
  { Save the standard properties }
  inherited SaveToStream(Stream);
  fBitmap.SaveToStream(Stream);
  Stream.Write(fScaleFactor, SizeOf(fScaleFactor));
  Stream.Write(fAspectRatio, SizeOf(fAspectRatio));
  Stream.Write(fCopyMode, SizeOf(fCopyMode));
end;

{procedure TBitmap2D.Draw(VT: TTransf2D; const Cnv:
  TCanvas; const ClipRect2D: TRect2D; const
  DrawMode:
  Integer);
var
  TmpPt1, TmpPt2: TPoint2D;
  TmpTransf: TTransf2D;
  TmpRect: TRect;
  OldMode: TCopyMode;
begin
  TmpTransf := VT;
  TmpPt1 := TransformPoint2D(fPoints[0], TmpTransf);
  TmpPt2 := TransformPoint2D(fPoints[1], TmpTransf);
  TmpRect := Rect2DToRect(Rect2D(TmpPt1.X, TmpPt1.Y, TmpPt2.X,
    TmpPt2.Y));
  OldMode := Cnv.CopyMode;
  Cnv.CopyMode := fCopyMode;
  Cnv.StretchDraw(TmpRect, fBitmap);
  Cnv.CopyMode := OldMode;
end;}

procedure TBitmap2D.DeviceDraw(Transf: TTransf2D;
  const Dvc: TDevice; const ClipRect2D: TRect2D);
begin
end;

initialization
finalization
end.

