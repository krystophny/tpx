{: This help file explain all the entities classes defined in
   the CADSys 4.0 library for both the 2D and 3D use.

   These classes are defined in the CS4Shapes unit file
   that you must include in the <B=uses> clause in all of your units
   that access the types mentioned here.
}
unit CS4Shapes;

interface

uses SysUtils, Classes, Windows, Graphics,
  WinBasic, CADSys4, Geometry, Draw, Contnrs;

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
  TArcDirection = (adClockwise, adCounterClockwise);

  {: This handler can be used to modify a primitive by dragging its
     control points.

     See also <See Class=TObject2DHandler>.
  }
  TPrimitive2DHandler = class(TObject2DHandler)
  public
    //TSY: Moved to TPrimitive2D:
    procedure DrawControlPoints(const Sender: TObject2D; const
      VT: TTransf2D; const Cnv: TDecorativeCanvas; const Width:
      Integer); override;
    function OnMe(const Sender: TObject2D; PT: TPoint2D;
      Aperture: TRealType; var Distance: TRealType): Integer;
      override;
  end;

  TLineStyle = (liNone, liSolid, liDotted, liDashed);

  THatching = (haNone, haHorizontal, haVertical,
    haFDiagonal, haBDiagonal, haCross, haDiagCross);

  TArrowKind = (arrNone, arrH40, arrH41, arrH42, arrH43, arrH44,
    arrH45, arrH46, arrH47, arrH48, arrT40, arrT43, arrT44, arrT45,
    arrH20, arrH21, arrH22, arrH23, arrH24, arrT20, arrT21, arrT22, arrT23,
    arrHR10, arrHR11, arrHR12, arrTR10, arrH10, arrH11, arrH12, arrH12C, arrT10,
    arrR0, arrR10, arrR11, arrR12, arrR20, arrR20C, arrR21, arrR33,
    arrTS10, arrTS11, arrTS12, arrHS10, arrHS12,
    arrTS20, arrTS21, arrTS23, arrHS20, arrHS23,
    arrO, arrOC, arrQQ);

const
  ArrowsIDs: array[0..52] of string[8] =
  ('none', 'h40', 'h41', 'h42', 'h43', 'h44',
    'h45', 'h46', 'h47', 'h48', 't40', 't43', 't44', 't45',
    'h20', 'h21', 'h22', 'h23', 'h24', 't20', 't21', 't22', 't23',
    'hr10', 'hr11', 'hr12', 'tr10', 'h10', 'h11', 'h12', 'h12c', 't10',
    'r0', 'r10', 'r11', 'r12', 'r20', 'r20c', 'r21', 'r33',
    'ts10', 'ts11', 'ts12', 'hs10', 'hs12',
    'ts20', 'ts21', 'ts23', 'hs20', 'hs23',
    'o', 'oc', 'qq');

type

  {: This is the class reference type for the
     <See Class=TPrimitive2D> shape class.
  }
  TPrimitive2DClass = class of TPrimitive2D;

  TPrimitive2D = class;

  TPieceLine = (pliDefault, pliNone, pliSolidDefault, pliFillAsDefault,
    pliFill);
  TPieceFill = (pfiDefault, pfiNone, pfiLineAsDefault, pfiLine);
  TPieceHatch = (phaDefault, phaNone);

  // TSY: New class for future. Needed for drawing compound shapes
  TPiece = class(TDrawPointsSet)
  public
    Line: TPieceLine;
    Fill: TPieceFill;
    Hatch: TPieceHatch;
    constructor Create(const _Capacity: Integer); override;
    procedure FillProfile(const Profile: TProfile;
      const Obj: TPrimitive2D); virtual;
    function GetLineStyle(Obj: TPrimitive2D): TLineStyle;
    function GetLineWidth(Obj: TPrimitive2D): TRealType;
    function GetLineColor(Obj: TPrimitive2D): TColor;
    function GetHatching(Obj: TPrimitive2D): THatching;
    function GetFillColor(Obj: TPrimitive2D): TColor;
  end;

  // polyline/polygon
  TLinPath = class(TPiece)
  end;

  // polybezier
  TBezierPath = class(TPiece)
  public
    procedure FillProfile(const Profile: TProfile;
      const Obj: TPrimitive2D); override;
  end;

  TPiece_CreateFromSvg = class(TPointsSet2D)
  private
    fIsBezier: Boolean;
    fPiece: TPiece;
    procedure PathOnClose(const X, Y: TRealType);
    procedure PathOnMoveTo(const X, Y: TRealType);
    procedure PathOnLineTo(const X, Y: TRealType);
    procedure PathOnBezierTo(const X1, Y1, X2, Y2, X3, Y3: TRealType);
  public
    function CreateFromSvgPath(const SvgPath: string): TPiece;
  end;

  TArrayOfPiece = class(TProfile)
  protected
    function GetItem(I: Integer): TPiece;
  public
    //procedure Add(Piece: TPiece);
    function AddFromSvgPath(const SvgPath: string): TPiece;
    function AddFromSvgPathT(const SvgPath: string;
      const T: TTransf2D): TPiece;
    function FillProfile(const Profile: TProfile;
      const Obj: TPrimitive2D): TRect2D;
    //procedure GetExtension0(var R: TRect2D; const FirstPass: Boolean);
    //function GetExtension: TRect2D;
    property Item[I: Integer]: TPiece read GetItem; default;
  end;

  TDrawPathProc = procedure(const Canvas: TCanvas;
    const IsClosed: Boolean; const Pnts: array of TPoint) of object;

  {: This class defines a <I=2D primitive>.
     /TSY: Merged with TOutline2D and TCurve2D./

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

     (<I=Profile>) is a 2D polyline in which the points that define
     the shape are not the same as the <I=control points> of the entity.
     (The control points only control the shape of the curve.)

     The points used to draw the entity ("profile") may be keept in memory or
     recomputed from the control points whenever they are needed to
     draw the shape. In the former a lot of memory may be used but the
     drawing is faster, in the latter the drawing of the shape
     may take a quite longer time but the memory is used
     efficently.

     For this class the <See property=TCurve2D@SavingType> property
     defines the mode of saving used by the entity.

     Before using the <I=profile points> you must call the
     <See Method=TPrimitive2D@BeginUseProfile> and after the
     using you must call the
     <See Method=TPrimitive2D@EndUseProfile> method.

     You have to put the code that defines the <I=profile points>
     from the <I=control points> in the
     <See Method=TCurve2D@FillProfile> method.

     <B=Note>: The control points and the profile points are
     always in the object model coordinate system !
  }
  TPrimitive2D = class(TObject2D)
  private
    fPoints: TEPointsSet2D;
    fHatching: THatching;
    fLineStyle: TLineStyle;
    fLineWidth: TRealType;
    fLineColor, fHatchColor, fFillColor: TColor;
    fCanDeletePoints: Boolean;
    fDrawPathBezier: Boolean;
    fIsClosed: Boolean;
    fOwnsInterior: Boolean;
    fProfile: TProfile;
    fSavingType: TPrimitiveSavingType;
    fCurvePrecision: Word;
    fCountReference: Integer;
    fBeginArrowKind: TArrowKind;
    fEndArrowKind: TArrowKind;
    fArrowSizeFactor: TRealType;

    procedure SetCurvePrecision(N: Word);
    procedure SetPrimitiveSavingType(S: TPrimitiveSavingType);
    procedure InitProfile;
    procedure FreeProfile;
    function GetArrowSize: TRealType;
  protected
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
    {: This method is called when the <I=profile points>
       are needed.

       It must returns the set of the <I=profile points>
       created by the class.

       <B=Warning>: You don't have to delete the set of points
       returned by the method.
    }
    function GetProfile: TProfile; virtual;
    function FillProfile: TRect2D; dynamic;
    {: This method returns the number of <I=profile points>
       that is equal (for an <I=outline>) to the number
       of <I=control points>.
    }
    function GetNPts: Integer; virtual;
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
    constructor CreateFromStream(const Stream: TStream; const
      Version: TCADVersion); override;
    procedure Assign(const Obj: TGraphicObject); override;
    procedure SaveToStream(const Stream: TStream); override;
    {: This property contains the set of <I=control points> used
       to define the shape of the entity.

       See the introduction of <See Class=TPrimitive2D> for details.
    }
    {: This method returns <B=True> if the set of <I=profile points>
       is closed.

       The fact that the set is closed influences the way in which
       it is drawed and picked.
    }
    procedure BeginUseProfile; dynamic;
    {: This method finalizes the <I=profile points> when you
       finish to use them.

       You must call this method when you no longer need to use
       the set of <I=profile points>. This allow the library
       to eventually saves the memory used by the entity.

       It is better to call this method in a <Code=try-finally>
       block with this method in the finally part.

       <B=Note>: This method must be called after the
       <See Method=TOutline2D@BeginUseProfile>.
    }
    procedure EndUseProfile; dynamic;
    {: This method is called whenever the <I=profile points>
       must be computed.

       You must redefine this method to fill the set of
       <I=control points> by using the actual set of
       <I=control points>. In defining this method you have
       to call the inherited method passing it the
       right number of <I=profile points> used by the
       entity as the <I=N> parameter.

       In the method you may use the
       <See Property=TPrimitive2D@ProfilePoints> to add the
       points to the set of <I=profile points>.

       <B=Warning>: Don't call
       <See Method=TPrimitive2D@BeginUseProfile> nor
       <See Method=TPrimitive2D@EndUseProfile> in this method.
       Also don't access the <See Property=TPrimitive2D@NumberOfProfilePts>
       property but use the number of points that you have
       computed.
    }
    procedure NewProfileItem(const NPnt: Integer);
    procedure FillPieces; virtual;
    //TSY:
    procedure BezierPoints(PP: TPointsSet2D); virtual;
    procedure DrawPoints(Points2D: TPointsSet2D;
      NoHatching, Connected: Boolean;
      VT: TTransf2D; const Cnv:
      TDecorativeCanvas; const ClipRect2D: TRect2D; const
      DrawMode: Integer);
    procedure DrawPolyline(const Canvas: TCanvas;
      const IsClosed: Boolean; const Pnts: array of TPoint);
    procedure DrawBezierPath(const Canvas: TCanvas;
      const IsClosed: Boolean; const Pnts: array of TPoint);
    procedure DrawPath(const Canvas: TCanvas;
      const IsClosed: Boolean; const Pnts: array of TPoint);
      virtual;
    procedure DrawNative(const VT: TTransf2D; const Cnv:
      TDecorativeCanvas; const ClipRect2D: TRect2D; const
      DrawMode: Integer; const IsClosed: Boolean;
      const Pnts: array of TPoint); virtual;
    procedure DrawPiece(Piece: TPiece;
      const VT: TTransf2D; const Cnv:
      TDecorativeCanvas; const ClipRect2D: TRect2D; const
      DrawMode: Integer);
    procedure DrawPieces(const VT: TTransf2D; const Cnv:
      TDecorativeCanvas; const ClipRect2D: TRect2D; const
      DrawMode: Integer);
    procedure DrawControlPoints0(const VT: TTransf2D;
      const Cnv: TDecorativeCanvas; const Width:
      Integer); virtual;
    procedure Draw(VT: TTransf2D; const Cnv:
      TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode:
      Integer); override;
    function OnMe(PT: TPoint2D; Aperture: TRealType;
      var Distance: TRealType): Integer; override;
    function OnProfile(PT: TPoint2D; Aperture: TRealType):
      Integer; virtual;
    procedure Transform(const T: TTransf2D); override;
    function DeleteControlPoint0(I: Integer): Integer; virtual;
    procedure DeleteControlPoint(const I: Integer); virtual;
    //Add control point at Pos
    function InsertControlPoint0(const Pos: Integer; P:
      TPoint2D): Integer; virtual;
    procedure InsertControlPoint(const Pos: Integer; P:
      TPoint2D); virtual;
    procedure MoveControlPoint0(const Pos: Integer;
      P: TPoint2D; Shift: TShiftState); virtual;
    property Points: TEPointsSet2D read fPoints write fPoints;
    {: This is the set of <I=profile points> that is used to
       draw the entity.

       See the class description of <See Class=TOutline2D>.
    }
    property Profile: TProfile read GetProfile;
    {: This property is <B=True> when the shape of the
       entity must be considere as closed.
    }
    {: This property may be used in the
       <See Method=TCurve2D@FillProfile> as a parameters
       to control the precision (ie the number of <I=profile points>)
       used to draw the curve profile.

       By default it is 50.
    }
    property CurvePrecision: Word read fCurvePrecision write
      SetCurvePrecision;
    {: This property specify the saving mode used by the
       curve.

       By default it is set to <I=stTime>.

       See also <See Type=TPrimitiveSavingType>.
    }
    property SavingType: TPrimitiveSavingType read fSavingType
      write SetPrimitiveSavingType;
    property IsClosed: Boolean read fIsClosed;
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
  end;

  {: This class defines a 2D line segment.

     The entity has two <I=control points> that are the extremes of
     the segment.
  }
  TLine2D = class(TPrimitive2D)
  private
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
    constructor CreateFromStream(const Stream: TStream; const
      Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure FillPieces; override;
    procedure BezierPoints(PP: TPointsSet2D); override;
    procedure Draw(VT: TTransf2D; const Cnv:
      TDecorativeCanvas; const ClipRect2D: TRect2D; const
      DrawMode: Integer); override;
    function OnMe(PT: TPoint2D; Aperture: TRealType; var
      Distance: TRealType): Integer; override;
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
    procedure Draw(VT: TTransf2D; const Cnv:
      TDecorativeCanvas; const ClipRect2D: TRect2D; const
      DrawMode: Integer); override;
  end;

  TPolyline2D = class(TPolyline2D0)
  private
  public
    class function GetName: string; override;
    procedure FillPieces; override;
  end;

  {: This class defines a 2D polygon.

     A polygon is obtained by connecting the <I=profile points> (
     in this case they are the same as the <I=profile points>)
     with straight segments and filling the shape with the current
     brush of the Canvas.
  }
  TPolygon2D = class(TPolyline2D0)
  private
  public
    class function GetName: string; override;
    constructor Create(ID: Integer); override;
    constructor CreateSpec(ID: Integer; const Pts: array of
      TPoint2D);
    procedure WhenCreated;
    procedure FillPieces; override;
    procedure Assign(const Obj: TGraphicObject); override;
  end;


  TStarKind = (starCircle, starSquare, starDiamond, starTriUp, starTriDown,
    starPenta, starStar4, starStar5, starStar6, starCross, starDCross,
    starFlower5, starFlower4, starStar4Arc, starMaltese);

const
  StarsIDs: array[0..14] of string[8] =
  ('circle', 'square', 'diamond', 'triup', 'tridown',
    'penta', 'star4', 'star5', 'star6', 'cross', 'dcross',
    'flower5', 'flower4', 'star4arc', 'maltese');

type

  TStar2D = class(TPrimitive2D)
  private
    fStarKind: TStarKind;
    fStarSizeFactor: TRealType;
  protected
  public
    class function GetName: string; override;
    constructor Create(ID: Integer); override;
    constructor CreateSpec(ID: Integer; const P: TPoint2D);
    procedure WhenCreated;
    procedure Assign(const Obj: TGraphicObject); override;
    constructor CreateFromStream(const Stream: TStream; const
      Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure FillPieces; override;
    procedure Draw(VT: TTransf2D; const Cnv:
      TDecorativeCanvas; const ClipRect2D: TRect2D; const
      DrawMode: Integer); override;
    property StarKind: TStarKind read fStarKind write fStarKind;
    property StarSizeFactor: TRealType read fStarSizeFactor write
      fStarSizeFactor;
  end;

  TBox2D0 = class(TPrimitive2D)
  protected
    function FillProfile: TRect2D; override;
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
     points of the rectangle.
  }
  TRectangle2D = class(TBox2D0)
  private
  public
    class function GetName: string; override;
    constructor Create(ID: Integer); override;
    procedure WhenCreated;
    procedure Assign(const Obj: TGraphicObject); override;
    procedure Draw(VT: TTransf2D; const Cnv:
      TDecorativeCanvas; const ClipRect2D: TRect2D; const
      DrawMode: Integer); override;
  end;

  {: This class defines a 2D ellipse.
     The ellipse is defined by the box that contains it.
  }
  TEllipse2D = class(TBox2D0)
  private
  protected
    function FillProfile: TRect2D; override;
  public
    class function GetName: string; override;
    constructor Create(ID: Integer); override;
    procedure WhenCreated;
    procedure GetEllipseParams(var CX, CY, RX, RY, ARot:
      TRealType);
    procedure BezierPoints(PP: TPointsSet2D); override;
    procedure Draw(VT: TTransf2D; const Cnv:
      TDecorativeCanvas; const ClipRect2D: TRect2D; const
      DrawMode: Integer); override;
    procedure Assign(const Obj: TGraphicObject); override;
  end;

  TCircle2D = class(TPrimitive2D)
  private
  protected
    function FillProfile: TRect2D; override;
  public
    class function GetName: string; override;
    constructor Create(ID: Integer); override;
    procedure WhenCreated;
    procedure Assign(const Obj: TGraphicObject); override;
    procedure BezierPoints(PP: TPointsSet2D); override;
    procedure DrawPath(const Canvas: TCanvas;
      const IsClosed: Boolean; const Pnts: array of TPoint);
      override;
    procedure Draw(VT: TTransf2D; const Cnv:
      TDecorativeCanvas; const ClipRect2D: TRect2D; const
      DrawMode: Integer); override;
  end;

  {: This class defines an arc of a 2D circle.

     The arc is defined by the center, radius,
     and the starting and ending angles of the arc.
  }
  TCircular2D = class(TPrimitive2D)
  private
    fRadius, FStartAngle, FEndAngle: TRealType;
    procedure SetRadius(R: TRealType);
    procedure SetStartAngle(A: TRealType);
    procedure SetEndAngle(A: TRealType);
  protected
    function FillProfile: TRect2D; override;
  public
    procedure GetArcParams(var CX, CY, R, SA, EA:
      TRealType);
    constructor Create(ID: Integer); override;
    constructor CreateSpec(ID: Integer; const CP: TPoint2D; R, SA,
      EA: TRealType);
    procedure WhenCreated;
    procedure FinishFirstDraw; override;
    procedure Assign(const Obj: TGraphicObject); override;
    constructor CreateFromStream(const Stream: TStream; const
      Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure BezierPoints(PP: TPointsSet2D); override;
    procedure DrawPath(const Canvas: TCanvas;
      const IsClosed: Boolean; const Pnts: array of TPoint);
      override;
    procedure Draw(VT: TTransf2D; const Cnv:
      TDecorativeCanvas; const ClipRect2D: TRect2D; const
      DrawMode: Integer); override;
    procedure MoveControlPoint0(const Pos: Integer; P: TPoint2D;
      Shift: TShiftState); override;
    property Radius: TRealType read fRadius write
      SetRadius;
    property StartAngle: TRealType read FStartAngle write
      SetStartAngle;
    property EndAngle: TRealType read FEndAngle write
      SetEndAngle;
  end;

  TArc2D = class(TCircular2D)
  private
  public
    class function GetName: string; override;
  end;

  TSector2D = class(TCircular2D)
  private
  public
    class function GetName: string; override;
    constructor Create(ID: Integer); override;
    constructor CreateSpec(ID: Integer; const CP: TPoint2D; R, SA,
      EA: TRealType);
    procedure WhenCreated;
  end;

  TSegment2D = class(TCircular2D)
  private
  public
    class function GetName: string; override;
    constructor Create(ID: Integer); override;
    constructor CreateSpec(ID: Integer; const CP: TPoint2D; R, SA,
      EA: TRealType);
    procedure WhenCreated;
  end;

  {: Base class for TBezierPath2D and TSmoothPath2D
  }
  TBezierPrimitive2D = class(TPrimitive2D)
  public
    procedure FillPieces; override;
    procedure Draw(VT: TTransf2D; const Cnv:
      TDecorativeCanvas; const ClipRect2D: TRect2D; const
      DrawMode: Integer); override;
    function OnProfile(PT: TPoint2D; Aperture: TRealType):
      Integer; override;
  end;

  // Plain cubic Bezier path

  TBezierPath2D0 = class(TBezierPrimitive2D)
  public
    constructor Create(ID: Integer); override;
    constructor CreateSpec(ID: Integer; const Pts: array of
      TPoint2D);
    procedure WhenCreated;
    constructor CreateFromStream(const Stream:
      TStream; const Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure Assign(const Obj: TGraphicObject); override;
    procedure BezierPoints(PP: TPointsSet2D); override;
    procedure DrawControlPoints0(const VT: TTransf2D;
      const Cnv: TDecorativeCanvas; const Width:
      Integer); override;
  end;

  TBezierPath2D = class(TBezierPath2D0)
  private
  public
    class function GetName: string; override;
    procedure FinishFirstDraw; override;
    function DeleteControlPoint0(I: Integer): Integer; override;
    function InsertControlPoint0(const Pos: Integer;
      P: TPoint2D): Integer; override;
    procedure MoveControlPoint0(const Pos: Integer; P: TPoint2D;
      Shift: TShiftState); override;
  end;

  TClosedBezierPath2D = class(TBezierPath2D0)
  private
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
    function InsertControlPoint0(const Pos: Integer; P:
      TPoint2D): Integer; override;
    procedure MoveControlPoint0(const Pos: Integer; P: TPoint2D;
      Shift: TShiftState); override;
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
    constructor CreateFromStream(const Stream:
      TStream; const Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure Assign(const Obj: TGraphicObject); override;
    procedure BezierPoints(PP: TPointsSet2D); override;
  end;

  TSmoothPath2D = class(TSmoothPath2D0)
  private
  public
    class function GetName: string; override;
  end;

  TClosedSmoothPath2D = class(TSmoothPath2D0)
  private
  public
    class function GetName: string; override;
    constructor Create(ID: Integer); override;
    constructor CreateSpec(ID: Integer; const Pts: array of
      TPoint2D);
    procedure WhenCreated;
    procedure BezierPoints(PP: TPointsSet2D);
      override;
  end;

  {: This type defines the horizontal justification mode for a
     2D/3D vectorial text:

     <LI=<I=jhLeft> means left justification.>
     <LI=<I=jhRight> means right justification.>
     <LI=<I=jhCenter> means center justification.>
  }
  THJustification = (jhLeft, jhCenter, jhRight);
  {: This type defines the vertical justification mode for a
     2D/3D vectorial text:

     <LI=<I=jvTop> means top justification.>
     <LI=<I=jvBottom> means bottom justification.>
     <LI=<I=jvCenter> means center justification.>
  }
  TVJustification = (jvBaseline, jvBottom, jvCenter, jvTop);

  {: This class defines a 2D text object that uses the Windows
     font for drawing.

     I created a new text object that is more suitable for a
     2D/3D CAD programs. By the fact that any TTF may be
     converted into the new font format, I discourage the use
     of this object that is keept in the library only for
     backward compatibility.

     See <See Class=TVectFont> for information on the
     new type of Text object.

     <B=Note>: The new text object is not able to fill the
      interior of characters. If you need this capability you
      still have to use this kind of Text object.
  }
  TText2D = class(TPrimitive2D)
  private
    fText: AnsiString;
    fTeXText: AnsiString;
    fHeight: TRealType;
    fRot: TRealType;
    //TSY:
    fHJustification: THJustification;
    fVJustification: TVJustification;
    fExtFont: TExtendedFont;
    fDrawBox, fRecalcBox: Boolean;
    fClippingFlags: Integer; // Win32s DrawText flags.
    procedure ResetJustification;
    procedure SetHJustification(J: THJustification);
    procedure SetVJustification(J: TVJustification);
    function GetWideText: WideString;
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
    constructor CreateFromStream(const Stream: TStream; const
      Version: TCADVersion); override;
    destructor Destroy; override;
    procedure Assign(const Obj: TGraphicObject); override;
    procedure SaveToStream(const Stream: TStream); override;
    function GetExtension0: TRect2D;
    function GetExtension: TRect2D;
    procedure _UpdateExtension; override;
    function FillProfile: TRect2D; override;
    procedure Draw(VT: TTransf2D; const Cnv:
      TDecorativeCanvas; const ClipRect2D: TRect2D; const
      DrawMode: Integer); override;
    procedure Transform(const T: TTransf2D); override;
    {: This property contains the heigth of the text in world
       units.
       This is different from the Heigth of font used to render
       the text object.
    }
    property Height: TRealType read fHeight write fHeight;
    property Rot: TRealType read fRot write fRot;
    {: This property contains the <See Class=TExtendedFont>
       instance used to render the text.

       Use it to change the font.
    }
    property LogFont: TExtendedFont read fExtFont;
    {: If this property is set to <B=True> the text box is
       drawed below the text. Otherwise only the text is drawed.

       By default it is <B=False>.
    }
    property DrawBox: Boolean read fDrawBox write fDrawBox;
    {: If this property is <B=True>, the bounding box is changed
       when the object is drawed so it contains the whole text.

       By default it is <B=False>.
    }
    property AutoSize: Boolean read fRecalcBox write fRecalcBox;
    {: This property contains the text string used by the
       text entity.

       You may include more than one line of text simply
       adding <Code=#10#13> beetwen lines.
    }
    property Text: AnsiString read fText write fText;
    property TeXText: AnsiString read fTeXText write fTeXText;
    property WideText: WideString read GetWideText;
    {: This property contains the <I=clipping flags> used
       by drawing the text with the <I=DrawText> API function.

       By default the are setted to <I=DT_NOCLIP>.
    }
    property ClippingFlags: Integer read fClippingFlags write
      fClippingFlags;
    property HJustification: THJustification read
      fHJustification write SetHJustification;
    property VJustification: TVJustification read
      fVJustification write SetVJustification;
  end;

  TSymbolKind = (symProcess, symDecision, symInputOutput,
    symPreparation, symPunchCard, symManualOperation, symKeyboard,
    symPunchTape, symDocument,
    symDocuments, symDisplay, symTerminal, symKeying, symAlternateProcess,
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
  private
    fDiameter: TRealType;
    fRot: TRealType;
    fHJustification: THJustification;
    fVJustification: TVJustification;
    fSymbolKind: TSymbolKind;
    procedure SetHJustification(J: THJustification);
    procedure SetVJustification(J: TVJustification);
  protected
  public
    class function GetName: string; override;
    constructor Create(ID: Integer); override;
    constructor CreateSpec(ID: Integer; P: TPoint2D;
      Diameter: TRealType);
    procedure WhenCreated;
    constructor CreateFromStream(const Stream: TStream; const
      Version: TCADVersion); override;
    procedure Assign(const Obj: TGraphicObject); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure FillPieces; override;
    procedure Draw(VT: TTransf2D; const Cnv:
      TDecorativeCanvas; const ClipRect2D: TRect2D; const
      DrawMode: Integer); override;
    {function OnMe(PT: TPoint2D; Aperture: TRealType; var
      Distance: TRealType): Integer; override;}
    procedure Transform(const T: TTransf2D); override;
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
     drawing. For instance this can be useful for GIS
     applications. However this object isn’t fast and sometimes
     the bitmap is not drawed. Maybe the problem is the Windows
     95 bitmap support. I will add faster and reliable
     raster capability to the library as soon as I have time.

     You can however use a thirdy part library to enhance this
     object by using this class as a blueprint for your
     specific bitmap entity.
  }
  TBitmap2D = class(TPrimitive2D)
  private
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
    constructor CreateSpec(ID: Integer; const P1, P2: TPoint2D; BMP:
      TBitmap);
    procedure WhenCreated;
    destructor Destroy; override;
    procedure Assign(const Obj: TGraphicObject); override;
    constructor CreateFromStream(const Stream: TStream; const
      Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure Draw(VT: TTransf2D; const Cnv:
      TDecorativeCanvas; const ClipRect2D: TRect2D; const
      DrawMode: Integer); override;
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

  {: This class defines a 2D/3D vectorial char as a set of
     polylines.

     The number of polylines that can be used to define a char
     is limited by the size given at the moment of creation.

     In the char definition the polylines must be defined in the
     unitary square that ranges from X:0,1 and Y:0,1, so that
     the char may be scaled with a scale transformation.

     When the char is drawed inside a string, the real dimension
     of the char is used (the font is a proportional one).

     <B=Note>: The vectorial character may be created with
     conversion from Windows True Type Font with a
     separate utility given with the library.
  }
  TVectChar = class(TObject)
  private
    fSubVects: TIndexedObjectList;
    fExtension: TRect2D;

    function GetVect(IDX: Integer): TPointsSet2D;
    function GetVectCount: Integer;
  public
    {: This constructor creates an new instance of the vectorial char.

       Parameters:

       <LI=<I=NSubVect> is the number of polylines that defines
        the char.>
    }
    constructor Create(NSubVect: Integer);
    destructor Destroy; override;
    {: This method create a new instance of a char definition by
       retrieves its datas from a stream.

       Parameters:

       <LI=<I=Stream> is the stream that contains the char
        definition (previously saved with
        <See Method=TVectChar@SaveToStream>).>
    }
    constructor CreateFromStream(const Stream: TStream; const
      Version: TCADVersion);
    {: This method saves a char definition into a stream.

       Parameters:

       <LI=<I=Stream> is the stream into which save the char
        definition (that can be retrived with
        <See Method=TVectCharCreateFromStream>).>
    }
    procedure SaveToStream(const Stream: TStream);
    {: This method is used computes the real dimension of the
       char.

       This method <B=MUST> be called when the definition of
       the char is finished (that is when you finish to
       create the polylines of the char).
    }
    procedure UpdateExtension(Sender: TObject);
    {: This property contains the set of polylines that defines
       the char.

       The polylines are stored in instances of
       <See Class=TPointsSet2D>.

       <I=Idx> is the index of the polyline in the set.
    }
    property Vectors[IDX: Integer]: TPointsSet2D read GetVect;
    {: This property contains the number of polylines that
       defines the char.

       This property cannot be changed.
    }
    property VectorCount: Integer read GetVectCount;
    {: This property contains the bounding box of the char.

       This value is updated by calling
       <See Method=TVectChar@UpdateExtension>. You must call
       that method before use this property.
    }
    property Extension: TRect2D read fExtension;
  end;

  {: This class defines a 2D vectorial font typeface.

     The typeface is made up of 256 instances of
     <See Class=TVectChar> (so no unicode may be used). The font
     may be stored and retrived from a font file.

     The easy way to define a font typeface is by using the
     <I=True Type Font> converter that create it from a TTF font.

     The chars are indexed by their <I=ASCII> value. If a char
     is not defined, it will not be drawed and an underline will
     be shown.

     The space and carriage returns are handled automatically,
     but you can change their shape.

     Any font used by the application is registered in the
     system with an <I=Index>, that is used to save and retrieve
     the font from the disk and associate the correct font to
     the texts.

     If you change the indexes among different drawings, you
     could load the wrong file. In this case you will notice that
     the text is drawed with a different font. If the index
     doesn't correspond to any font and no default font is
     defined an exception will be raised and the drawing will not
     be loaded.

     See <See Function=CADSysSetDefaultFont> and the other
     registration functions for details.
  }
  TVectFont = class(TObject)
  private
    fVects: TIndexedObjectList;

    function GetChar(Ch: Char): TVectChar;
  public
    constructor Create;
    destructor Destroy; override;
    {: This method creates an instance of the font and retrieves
       its definition from a Stream.

       Parameters:

       <LI=<I=Stream> is the stream from which retrieve the
        font definition.>
    }
    constructor CreateFromStream(const Stream: TStream; const
      Version: TCADVersion);
    {: This method saves an instance of the font into a Stream.

       Parameters:

       <LI=<I=Stream> is the stream to which saves the font
        definition.>
    }
    procedure SaveToStream(const Stream: TStream);
    {: This method draws a char of the font on a canvas using a
       transform mapping from a 2D viewing system.

       Parameters:

       <LI=<I=Ch> is the index (ASCII) of the char to be drawed.>
       <LI=<I=DrawPoint> is the 2D point in world coordinates at
        which draw the char (it is the botton-left corner of the
        bounding box of the char). The point is changed by the
        method into the position for the next char on the same
        text line.>
       <LI=<I=H> is the size of the char in world units. This value
        is used to scale the char definition.>
       <LI=<I=ICS> is the space between two chars (in normalized
        units). For example a value of 0.2 means a space of
        20% of H.>
       <LI=<I=VT> is the mapping transform from world to screen.
        It may be obtained from the
        <See Property=TCADViewport@ViewportToScreenTransform>
        property.>
       <LI=<I=Cnv> is the canvas on which draw the char.>
    }
    procedure DrawChar2D(Ch: Char; var DrawPoint: TPoint2D; const
      H, ICS: TRealType; const VT: TTransf2D; Cnv:
      TDecorativeCanvas);
    {: This method returns the bounding box of a vectorial text string
       when the current font is used to draw it.

       Parameters:

       <LI=<I=Str> is the string.>
       <LI=<I=H> is the size of the char in world units.>
       <LI=<I=InterChar> is the space beetwen two chars
        (in normalized units). For example a value of 0.2 means
        a space of 20% of H.>
       <LI=<I=InterLine> is the space beetwen two lines of the
        text (in normalizaed units). For example a value of 0.2
        means a space of 20% of H.>
    }
    function GetTextExtension(Str: AnsiString; H, InterChar,
      InterLine: TRealType): TRect2D;
    {: This method creates a new char definition and returns the
       <See Class=TVectChar> that rapresents it.

       You must use the returned value to define the char.

       If the char is already present it will be deleted.

       Parameters:

       <LI=<I=Ch> is the characted to be defined.>
       <LI=<I=N> is the number of polylines used to draw the char.>
    }
    function CreateChar(Ch: Char; N: Integer): TVectChar;
    {: This property contains the set of chars of the font.

       The characters are indexed by their <I=ASCII> code.
    }
    property Chars[Ch: Char]: TVectChar read GetChar;
  end;


  {: This class defines the 2D vectorial text.

     The text may be multilines and justified. It uses an
     instance of <See Class=TVectFont> to extract the typeface
     to be used.
  }
  TJustifiedVectText2D = class(TPrimitive2D)
  private
    fVectFont: TVectFont;
    fText: AnsiString;
    fHJustification: THJustification;
    fVJustification: TVJustification;
    fBasePoint: TPoint2D;
    fHeight, fCharSpace, fInterLine: TRealType;
    fDrawBox: Boolean;

    procedure SetHeight(H: TRealType);
    procedure SetCharSpace(S: TRealType);
    procedure SetInterLine(S: TRealType);
    procedure SetText(T: string);
    function GetTextExtension: TRect2D;
    procedure DrawText(const VT: TTransf2D; const Cnv:
      TDecorativeCanvas; const DrawMode: Integer);
  protected
    procedure _UpdateExtension; override;
  public
    class function GetName: string; override;
    {: This constructor creates a new instance of the class.

       Parameters:

       <LI=<I=ID> is identifier that univocally identify the
        object in the CAD. By means of the method used to add the
        object to the CAD, the <I=ID> of the object might be different
        from the one supplied here. See <See Method=TDrawing@AddObject>
        for details.>
       <LI=<I=FontVect> is the font typeface. Use
        <See Function=CADSysFindFontByIndex> and
        <See Function=CADSysFindFontIndex> for details.>
       <LI=<I=TextBox> is the rectangle used to justify the text.
        The string is drawed from the upper-left corner of this
        box.>
       <LI=<I=Height> is the size of the font in world units.>
       <LI=<I=Txt> is the text to be drawed.>
    }
    constructor Create(ID: Integer); override;
    constructor CreateSpec(ID: Integer; FontVect: TVectFont;
      TextBox: TRect2D; Height: TRealType; Txt: AnsiString);
    procedure WhenCreated;
    constructor CreateFromStream(const Stream: TStream; const
      Version: TCADVersion); override;
    procedure Assign(const Obj: TGraphicObject); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure Draw(VT: TTransf2D; const Cnv:
      TDecorativeCanvas; const ClipRect2D: TRect2D; const
      DrawMode: Integer); override;
    function OnMe(PT: TPoint2D; Aperture: TRealType; var
      Distance: TRealType): Integer; override;
    {: This property contains the size of the font in world unit.
    }
    property Height: TRealType read fHeight write SetHeight;
    {: This property contains the spacing beetwen chars in
       normalized unit.

       For example a value of 0.2 means a space of 20% of the
       <See Property=TJustifiedVectText2D@Height>.
    }
    property CharSpace: TRealType read fCharSpace write
      SetCharSpace;
    {: This property contains the spacing beetwen lines of the
       text.

       For example a value of 0.2 means a space of 20% of the
       <See Property=TJustifiedVectText2D@Height>.
    }
    property InterLine: TRealType read fInterLine write
      SetInterLine;
    {: This property contains the instance of the font that is
       used to draw the text.
    }
    property VectFont: TVectFont read fVectFont write fVectFont;
    {: If this property is <B=True>, a frame is drawed around
       the text.
    }
    property DrawBox: Boolean read fDrawBox write fDrawBox;
    {: This property contains the text to be drawed.
    }
    property Text: AnsiString read fText write SetText;
    {: This property specifies the horizontal justification.
    }
    property HorizontalJust: THJustification read fHJustification
      write fHJustification;
    {: This property specifies the vertical justification.
    }
    property VerticalJust: TVJustification read fVJustification
      write fVJustification;
    //TSY:
    property Extension: TRect2D read GetTextExtension;
  end;

  {: This procedure sets the default font.
  }
procedure CADSysSetDefaultFont(const Font: TVectFont);
  {: This function returns the default font.
  }
function CADSysGetDefaultFont: TVectFont;
  {: This function initializes the list of registered fonts.
  }
procedure CADSysInitFontList;
  {: This function clears the list of registered fonts.
  }
procedure CADSysClearFontList;
  {: This function returns the index of a font.

     If the font is not registered an exception will be raised.

     Parameters:

     <LI=<I=Font> is the font to be searched.>
  }
function CADSysFindFontIndex(const Font: TVectFont): Word;
  {: This function returns the font that was registered with
     the specified index.

     If the index is not used and a default font is defined,
     the function returns the default font. Otherwise an
     exception will be raised.

     Parameters:

     <LI=<I=Index> is the index of the font to be searched for.>
  }
function CADSysFindFontByIndex(Index: Word): TVectFont;
  {: This function register a font by retriving it from
     a file.

     If the index of registration is already in use an
     exception will be raised.

     Parameters:

     <LI=<I=Index> is the registration index.>
     <LI=<I=FileName> is name of the file that contains the
      font.>

     <B=Note>: There are <I=MAX_REGISTERED_FONTS> slots in the
     registration list.
  }
procedure CADSysRegisterFontFromFile(Index: Word; const
  FileName: string);
  {: This function register a font.

     If the index of registration is already in use an
     exception will be raised.

     Parameters:

     <LI=<I=Index> is the registration index.>
     <LI=<I=Font> is the font to be registered.>

     <B=Note>: There are <I=MAX_REGISTERED_FONTS> slots in the
     registration list.
  }
procedure CADSysRegisterFont(Index: Word; const Font:
  TVectFont);
  {: This function clear the registration of a font.

     Parameters:

     <LI=<I=Index> is the registration index.>
  }
procedure CADSysUnregisterFont(Index: Word);

procedure RectangleCalcPoints(P0, P1, P2: TPoint2D;
  var P3, P4: TPoint2D; var A: TRealType);
procedure GetEllipseParams0(const P0, P1, P2: TPoint2D;
  var P3, P4: TPoint2D;
  var CX, CY, RX, RY, ARot: TRealType);
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

var
  VectFonts2DRegistered: array[0..MAX_REGISTERED_FONTS] of
  TVectFont;
  _NullChar: TVectChar;
  _DefaultFont: TVectFont;
  _DefaultHandler2D: TPrimitive2DHandler;

// =====================================================================
// TPrimitive2DHandler
// =====================================================================

procedure TPrimitive2DHandler.DrawControlPoints(const Sender:
  TObject2D; const VT: TTransf2D; const Cnv: TDecorativeCanvas;
  const Width: Integer);
begin
  if Sender is TPrimitive2D then
    TPrimitive2D(Sender).DrawControlPoints0(VT, Cnv, Width);
end;

function TPrimitive2DHandler.OnMe(const Sender: TObject2D; PT:
  TPoint2D; Aperture: TRealType; var Distance: TRealType):
  Integer;
var
  Cont: Integer;
  ResDist: TRealType;
begin
  Result := PICK_NOOBJECT;
  if Sender is TPrimitive2D then
    with TPrimitive2D(Sender) do
      for Cont := 0 to Points.Count - 1 do
        if NearPoint2D(PT, Points[Cont], Aperture, ResDist)
          and
          (ResDist <= Distance) then
        begin
          Result := Cont;
          Distance := ResDist;
        end;
end;

// =====================================================================
// TPiece
// =====================================================================

constructor TPiece.Create(const _Capacity: Integer);
begin
  inherited Create(_Capacity);
  Line := pliDefault;
  Fill := pfiDefault;
  Hatch := phaDefault;
end;

procedure TPiece.FillProfile(const Profile: TProfile;
  const Obj: TPrimitive2D);
begin
  Profile.NewItem(Count);
  Profile.LastSet.Copy(Self, 0, Count - 1);
  Profile.LastSet.Closed := Closed; //Obj.IsClosed;
    //(Count > 1) and IsSamePoint2D(Points[0], Points[Count - 1]);
  Profile.LastSet.Filled := GetFillColor(Obj) <> clDefault;
  Profile.LastSet.Hatched := GetHatching(Obj) <> haNone;
end;

function TPiece.GetLineStyle(Obj: TPrimitive2D): TLineStyle;
begin
  case Line of
    pliDefault: Result := Obj.LineStyle;
    pliNone: Result := liNone;
    pliSolidDefault:
      if Obj.LineStyle in [liNone] then Result := liNone
      else Result := liSolid;
    pliFillAsDefault:
      Result := liSolid;
    pliFill:
      if Obj.FillColor = clDefault then Result := liNone
      else Result := liSolid;
  end;
end;

function TPiece.GetLineWidth(Obj: TPrimitive2D): TRealType;
begin
  Result := Obj.LineWidth;
  {case Line of
    pliDefault, pliSolidDefault: Result := Obj.LineWidth;
    pliNone: Result := clRed;
    pliFillAsDefault:
      if Obj.LineColor <> clDefault then Result := Obj.LineColor
      else
        if Obj.FillColor <> clDefault then Result := Obj.FillColor
        else Result := clBlack;
    pliFill: Result := Obj.FillColor;
  end;}
end;

function TPiece.GetLineColor(Obj: TPrimitive2D): TColor;
begin
  case Line of
    pliDefault, pliSolidDefault: Result := Obj.LineColor;
    pliNone: Result := clRed;
    pliFillAsDefault:
      if Obj.LineColor <> clDefault then Result := Obj.LineColor
      else
        if Obj.FillColor <> clDefault then Result := Obj.FillColor
        else Result := clBlack;
    pliFill: Result := Obj.FillColor;
  end;
end;

function TPiece.GetHatching(Obj: TPrimitive2D): THatching;
begin
  case Hatch of
    phaDefault: Result := Obj.Hatching;
    phaNone: Result := haNone;
  end;
end;

function TPiece.GetFillColor(Obj: TPrimitive2D): TColor;
begin
  case Fill of
    pfiDefault: Result := Obj.FillColor;
    pfiNone: Result := clDefault;
    pfiLine:
      if Obj.LineColor <> clDefault then Result := Obj.LineColor
      else Result := clBlack;
    pfiLineAsDefault:
      if Obj.FillColor <> clDefault then Result := Obj.FillColor
      else
        if Obj.LineColor <> clDefault then Result := Obj.LineColor
        else Result := clBlack;
  end;
end;

// =====================================================================
// TBezierPath
// =====================================================================

procedure TBezierPath.FillProfile(const Profile: TProfile;
  const Obj: TPrimitive2D);
begin
  Profile.NewItem(0);
  LinearizeBezier(Self, BezierPrecision, Closed, Profile.LastSet);
  Profile.LastSet.Closed := Closed;
  Profile.LastSet.Filled := GetFillColor(Obj) <> clDefault;
  Profile.LastSet.Hatched := GetHatching(Obj) <> haNone;
end;

     {*---- TPiece_CreateFromSvg ----*}

procedure TPiece_CreateFromSvg.PathOnClose(const X, Y: TRealType);
begin
  //fPiece.Add(Point2D(X, Y));
  //fPiece.Fill :=
  fPiece.Closed := True;
end;

procedure TPiece_CreateFromSvg.PathOnMoveTo(const X, Y: TRealType);
begin
  fPiece.Add(Point2D(X, Y));
end;

procedure TPiece_CreateFromSvg.PathOnLineTo(const X, Y: TRealType);
begin
  fPiece.Add(Point2D(X, Y));
end;

procedure TPiece_CreateFromSvg.PathOnBezierTo(
  const X1, Y1, X2, Y2, X3, Y3: TRealType);
begin
  fPiece.Add(Point2D(X1, Y1));
  fPiece.Add(Point2D(X2, Y2));
  fPiece.Add(Point2D(X3, Y3));
end;

function TPiece_CreateFromSvg.CreateFromSvgPath(const SvgPath: string): TPiece;
var
  PathParser: T_SVG_Path_Parser;
begin
  fIsBezier := SVG_Path_HasCurves(SvgPath);
  if fIsBezier then Result := TBezierPath.Create(0)
  else Result := TLinPath.Create(0);
  fPiece := Result;
  PathParser := T_SVG_Path_Parser.Create(
    PathOnClose, PathOnMoveTo, PathOnLineTo, PathOnBezierTo);
  try
    PathParser.AllAsBezier := fIsBezier;
    PathParser.Parse(SvgPath);
  finally
    PathParser.Free;
  end;
end;

// =====================================================================
// TArrayOfPiece
// =====================================================================

{procedure TArrayOfPiece.Add(Piece: TPiece);
begin
  inherited Add(Piece);
end;}

function TArrayOfPiece.AddFromSvgPath(const SvgPath: string): TPiece;
var
  FromSvg: TPiece_CreateFromSvg;
begin
  FromSvg := TPiece_CreateFromSvg.Create(20);
  try
    Result := FromSvg.CreateFromSvgPath(SvgPath);
    Add(Result);
  finally
    FromSvg.Free;
  end;
end;

function TArrayOfPiece.AddFromSvgPathT(const SvgPath: string;
  const T: TTransf2D): TPiece;
begin
  Result := AddFromSvgPath(SvgPath);
  Result.TransformPoints(T);
end;

function TArrayOfPiece.GetItem(I: Integer): TPiece;
begin
  Result := inherited GetItem(I) as TPiece;
end;

function TArrayOfPiece.FillProfile(const Profile: TProfile;
  const Obj: TPrimitive2D): TRect2D;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    GetItem(I).FillProfile(Profile, Obj);
  Result := Profile.GetExtension;
end;

{procedure TArrayOfPiece.GetExtension0(var R: TRect2D; const FirstPass: Boolean);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Pieces[I].GetExtension0(R, FirstPass and (I = 0));
end;}

{function TArrayOfPiece.GetExtension: TRect2D;
begin
  Result := Rect2D(0, 0, 0, 0);
  GetExtension0(Result, True);
end;}

// =====================================================================
// TPrimitive2D
// =====================================================================

procedure TPrimitive2D._UpdateExtension;
begin
   { Change the extension. }
//    WritableBox := fPoints.Extension;
  if not Assigned(fPoints) or (fPoints.Count = 0) then
  begin
    WritableBox := Rect2D(0, 0, 0, 0);
    Exit;
  end;
  case fSavingType of
    stSpace:
      begin
        InitProfile;
        WritableBox := FillProfile;
        FreeProfile;
      end;
    stTime:
      begin
        FreeProfile;
        InitProfile;
        WritableBox := FillProfile;
      end;
  end;
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
  fCurvePrecision := 150;

  WhenCreated;
end;

constructor TPrimitive2D.CreateSpec(ID: Integer; NPts: Integer;
  CurvePrec: Word);
begin
  inherited Create(ID);
  { Create the internal vector. }
  fPoints := CreateVect(NPts);
  fCurvePrecision := CurvePrec;

  WhenCreated;
end;

procedure TPrimitive2D.WhenCreated;
begin
  if fPoints <> nil then
    fPoints.OnChange := UpdateExtension;
  fCurvePrecision := 1;
  fHatching := haNone;
  fLineStyle := liSolid;
  fLineWidth := 1;
  fLineColor := clDefault;
  fHatchColor := clDefault;
  fFillColor := clDefault;
  SetSharedHandler(_DefaultHandler2D);
  fCanDeletePoints := False;
  fDrawPathBezier := True;
  Pieces := TArrayOfPiece.Create(1);
  FirstDrawPoint := -1;
  fIsClosed := False;
  fOwnsInterior := True;
  fProfile := nil;
  fCountReference := 0;
  fSavingType := stTime;
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
  fCountReference := 0;
  FreeProfile;
  inherited Destroy;
end;

procedure TPrimitive2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
    Exit;
  inherited Assign(Obj);
  fSavingType := stTime;
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
    fIsClosed := (Obj as TPrimitive2D).fIsClosed;
    fOwnsInterior := (Obj as TPrimitive2D).fOwnsInterior;
    fCurvePrecision := (Obj as TPrimitive2D).fCurvePrecision;
    fSavingType := (Obj as TPrimitive2D).fSavingType;
    fBeginArrowKind := (Obj as TPrimitive2D).fBeginArrowKind;
    fEndArrowKind := (Obj as TPrimitive2D).fEndArrowKind;
    fArrowSizeFactor := (Obj as TPrimitive2D).fArrowSizeFactor;
    //WhenCreated;
  end;
end;

constructor TPrimitive2D.CreateFromStream(const Stream: TStream;
  const Version: TCADVersion);
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
    Read(fIsClosed, SizeOf(fIsClosed));
    Read(fOwnsInterior, SizeOf(fOwnsInterior));
    Read(fCurvePrecision, SizeOf(fCurvePrecision));
    Read(fSavingType, SizeOf(fSavingType));
    fCountReference := 0;
    Read(fBeginArrowKind, SizeOf(fBeginArrowKind));
    Read(fEndArrowKind, SizeOf(fEndArrowKind));
    Read(fArrowSizeFactor, SizeOf(fArrowSizeFactor));
  end;
  fPoints.OnChange := UpdateExtension;
  SetSharedHandler(_DefaultHandler2D);
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
    Write(fIsClosed, SizeOf(fIsClosed));
    Write(fOwnsInterior, SizeOf(fOwnsInterior));
    Write(fCurvePrecision, SizeOf(fCurvePrecision));
    Write(fSavingType, SizeOf(fSavingType));
    Write(fBeginArrowKind, SizeOf(fBeginArrowKind));
    Write(fEndArrowKind, SizeOf(fEndArrowKind));
    Write(fArrowSizeFactor, SizeOf(fArrowSizeFactor));
  end;
end;

procedure TPrimitive2D.SetPrimitiveSavingType(S:
  TPrimitiveSavingType);
begin
  if S <> fSavingType then
  begin
    fSavingType := S;
    UpdateExtension(Self);
  end;
end;

procedure TPrimitive2D.SetCurvePrecision(N: Word);
begin
  if fCurvePrecision <> N then
    fCurvePrecision := N;
end;

function TPrimitive2D.GetArrowSize: TRealType;
begin
  if OwnerCAD is TDrawing2D then
    Result := (OwnerCAD as TDrawing2D).ArrowsSize
  else Result := 1;
  Result := Result * fArrowSizeFactor;
end;

function TPrimitive2D.FillProfile: TRect2D;
begin
  {if Pieces.Count = 0 then
    Result := Rect2D(0, 0, 0, 0)
  else}
  begin
    Pieces.Clear;
    FillPieces;
    Result := Pieces.FillProfile(Profile, Self);
  end;
end;

procedure TPrimitive2D.NewProfileItem(const NPnt: Integer);
begin
  Profile.NewItem(NPnt);
  Profile.Item[0].Closed := fIsClosed;
  Profile.Item[0].Hatched := fHatching <> haNone;
  Profile.Item[0].Filled := fFillColor <> clDefault;
end;

procedure TPrimitive2D.InitProfile;
begin
  if not Assigned(fProfile) then
    fProfile := TProfile.Create(0)
  else
    fProfile.Clear;
  Inc(fCountReference);
{      if fOwnsInterior or (fHatching <> haNone)
        or (fFillColor <> clDefault) then}
end;

procedure TPrimitive2D.FreeProfile;
begin
  Dec(fCountReference);
  if fCountReference <= 0 then
  begin
    FreeAndNil(fProfile);
    fCountReference := 0;
  end;
end;

function TPrimitive2D.GetProfile: TProfile;
begin
  if not Assigned(fProfile) then
    raise
      ECADSysException.Create('TPrimitive2D: Call BeginUseProfile before accessing the curve points.');
  Result := fProfile;
end;

function TPrimitive2D.GetNPts: Integer;
begin
  if not Assigned(fProfile) then
    raise
      ECADSysException.Create('TPrimitive2D: Call BeginUseProfile before accessing the curve points.');
  Result := fProfile.Count;
end;

procedure TPrimitive2D.BeginUseProfile;
begin
  if (fSavingType = stSpace) or not Assigned(fProfile) then
  begin
    InitProfile;
    WritableBox := FillProfile;
  end;
end;

procedure TPrimitive2D.EndUseProfile;
begin
  if fSavingType = stSpace then
    FreeProfile;
end;

procedure TPrimitive2D.FillPieces;
begin

end;

{procedure TPrimitive2D.BezierPoints(PP: TPointsSet2D; T: TTransf2D);
var
  R: TRect2D;
  BL, BR, TL, TR: TPoint2D;
begin
  R := CartesianRect2D(Box);
  BL := R.FirstEdge;
  BR := BoxBottomRight(R);
  TL := BoxTopLeft(R);
  TR := R.SecondEdge;
  PP.Free;
  PP := TPointsSet2D.Create(13);
  PP.Add(BL);
  PP.Add(MixPoint(BL, TL, 0.25));
  PP.Add(MixPoint(BL, TL, 0.75));
  PP.Add(TL);
  PP.Add(MixPoint(TL, TR, 0.25));
  PP.Add(MixPoint(TL, TR, 0.75));
  PP.Add(TR);
  PP.Add(MixPoint(TR, BR, 0.25));
  PP.Add(MixPoint(TR, BR, 0.75));
  PP.Add(BR);
  PP.Add(MixPoint(BR, BL, 0.25));
  PP.Add(MixPoint(BR, BL, 0.75));
  PP.Add(BL);
end;}

procedure TPrimitive2D.BezierPoints(PP: TPointsSet2D);
begin
  if Pieces.Count = 0 then
    LinPolyToBezier(fPoints, IsClosed, PP)
  else
    if Pieces[0] is TLinPath then
      LinPolyToBezier(Pieces[0], Pieces[0].Closed, PP)
    else if Pieces[0] is TBezierPath then
    begin
      PP.Clear;
      PP.Copy(Pieces[0], 0, Pieces[0].Count - 1);
    end;
end;

procedure TPrimitive2D.DrawPoints(Points2D: TPointsSet2D;
  NoHatching, Connected: Boolean;
  VT: TTransf2D; const Cnv:
  TDecorativeCanvas; const ClipRect2D: TRect2D; const
  DrawMode: Integer);
begin
  if not NoHatching then
    if (DrawMode and DRAWMODE_OutlineOnly = 0)
      and (Hatching <> haNone) then
    begin
      DrawAsPolygon(Points2D, Cnv,
        RectToRect2D(Cnv.Canvas.ClipRect), Box, VT);
    end;
  if (DrawMode and DRAWMODE_OutlineOnly <> 0)
    or (LineStyle <> liNone) then
    if Connected then
      DrawAsPolygonOutline(Points2D, Cnv,
        RectToRect2D(Cnv.Canvas.ClipRect), Box, VT)
    else
      DrawAsPolyline(Points2D, Cnv,
        RectToRect2D(Cnv.Canvas.ClipRect), Box, VT);
end;

procedure TPrimitive2D.DrawPolyline(const Canvas: TCanvas;
  const IsClosed: Boolean; const Pnts: array of TPoint);
begin
  if High(Pnts) < 1 then Exit;
  BeginPath(Canvas.Handle);
  //if IsClosed then Canvas.Polygon(Pnts)  else Canvas.PolyLine(Pnts);
  MoveToEx(Canvas.Handle, Pnts[0].X, Pnts[0].Y, nil);
  PolyLineTo(Canvas.Handle, Pnts[1], High(Pnts));
  if IsClosed then CloseFigure(Canvas.Handle);
  EndPath(Canvas.Handle);
end;

procedure TPrimitive2D.DrawBezierPath(const Canvas: TCanvas;
  const IsClosed: Boolean; const Pnts: array of TPoint);
begin
  if High(Pnts) < 1 then Exit;
  BeginPath(Canvas.Handle);
  MoveToEx(Canvas.Handle, Pnts[0].X, Pnts[0].Y, nil);
  PolyBezierTo(Canvas.Handle, Pnts[1], High(Pnts));
  if IsClosed then CloseFigure(Canvas.Handle);
  EndPath(Canvas.Handle);
end;

procedure TPrimitive2D.DrawPath(const Canvas: TCanvas;
  const IsClosed: Boolean; const Pnts: array of TPoint);
begin
  if fDrawPathBezier then
    DrawBezierPath(Canvas, IsClosed, Pnts)
  else DrawPolyline(Canvas, IsClosed, Pnts);
end;

procedure DrawNative0(const VT: TTransf2D;
  const Cnv: TDecorativeCanvas;
  const PathProc: TDrawPathProc;
  const ClipRect2D: TRect2D;
  const DrawMode: Integer; const IsClosed: Boolean;
  const Pnts: array of TPoint;
  const LineStyle: TLineStyle;
  const Line_Width: TRealType;
  const Hatching: THatching;
  const LineColor, FillColor, HatchColor: TColor;
  const LineWidthBase, PixelSize, UnitLength, Scale, MiterLimit: TRealType);
var
  IsMetafile: Boolean;
  BrushStyle0: TBrushStyle;
  BrushColor0: TColor;
  PenStyle, PenStyle0: TPenStyle;
  PenColor0: TColor;
  LOGBRUSH: tagLOGBRUSH;
  LOGPEN: tagLOGPEN;
  //MiterLimit0: Single;
  procedure SetPen(PenStyle: TPenStyle;
    PenColor: TColor);
  begin
    with LOGBRUSH do
    begin
      lbStyle := BS_SOLID;
      lbColor := PenColor;
      lbHatch := 0;
    end;
    Cnv.Canvas.Pen.Handle := ExtCreatePen(
      PS_GEOMETRIC or Ord(PenStyle) {PS_Solid} or PS_EndCap_Flat or
      PS_Join_Miter,
      Cnv.Canvas.Pen.Width, LOGBRUSH, 0, nil);
    {with LOGPEN do
    begin
      lopnStyle := Ord(PenStyle);// or PS_JOIN_MITER
      lopnWidth := Point(Cnv.Canvas.Pen.Width, 0);
      lopnColor := PenColor;
    end;
    Cnv.Canvas.Pen.Handle := CreatePenIndirect(LOGPEN);}
  end;
begin
  IsMetafile := Cnv.Canvas is TMetaFileCanvas;
  //Windows GDI is a wonder!
  if (DrawMode and DRAWMODE_OutlineOnly <> 0) then
  begin
    BrushStyle0 := Cnv.Canvas.Brush.Style;
    BrushColor0 := Cnv.Canvas.Brush.Color;
    Cnv.Canvas.Brush.Style := bsClear;
    PathProc(Cnv.Canvas, IsClosed, Pnts);
    StrokePath(Cnv.Canvas.Handle);
    Cnv.Canvas.Brush.Style := BrushStyle0;
    Cnv.Canvas.Brush.Color := BrushColor0;
    Exit;
  end;
  PenStyle0 := Cnv.Canvas.Pen.Style;
  PenColor0 := Cnv.Canvas.Pen.Color;
  SetPolyFillMode(Cnv.Canvas.Handle, WINDING);
   // winding aka nonzero fill rule
  with LOGBRUSH do
  begin
    if Hatching <> haNone then lbStyle := BS_HATCHED
    else if FillColor = clDefault then lbStyle := BS_HOLLOW
    else lbStyle := BS_SOLID;
    if Hatching <> haNone then
      if HatchColor = clDefault then lbColor := clGray
      else lbColor := HatchColor
    else if FillColor = clDefault then lbColor := clNone
    else lbColor := FillColor;
    lbHatch := Ord(Hatching) - 1;
  end;
  Cnv.Canvas.Brush.Handle := CreateBrushIndirect(LOGBRUSH);
  //If brush style is BS_HATCHED then BkColor is used to fill background!
  if FillColor = clDefault then
  begin
    SetBkColor(Cnv.Canvas.Handle, {not ColorToRGB(LOGBRUSH.lbColor)} clNone);
    SetBkMode(Cnv.Canvas, False);
  end
  else
  begin
    SetBkColor(Cnv.Canvas.Handle, FillColor);
    SetBkMode(Cnv.Canvas, True);
  end;
  PathProc(Cnv.Canvas, IsClosed, Pnts);
  if LineStyle = liNone then SetPen(psClear, clRed)
  else
  begin
    SetMiterLimit(Cnv.Canvas.Handle, MiterLimit, nil);
    {GetMiterLimit(Cnv.Canvas.Handle, MiterLimit0);
    MiterLimit0 := MiterLimit + MiterLimit0 * 0;}
    case LineStyle of
      liSolid:
        PenStyle := psSolid;
      liDotted:
        PenStyle := psDot;
      liDashed:
        PenStyle := psDash;
    end;
    if IsMetafile or (Scale <= 0) then Cnv.Canvas.Pen.Width :=
      Max(Round(LineWidthBase * Line_Width / UnitLength), 1)
    else
      Cnv.Canvas.Pen.Width :=
        Max(Round(LineWidthBase * Line_Width / Scale / PixelSize),
        1 {Cnv.Canvas.Pen.Width});
    if LineColor = clDefault
      then SetPen(PenStyle, PenColor0)
    else SetPen(PenStyle, LineColor);
  end;
  if LineStyle <> liNone then
    if (Hatching <> haNone) or (FillColor <> clDefault) then
      if not IsClosed then
      begin
        //StrokeAndFillPath is not suitable for opened figures,
        // because it closes all such figures!
        FillPath(Cnv.Canvas.Handle);
        PathProc(Cnv.Canvas, IsClosed, Pnts);
        StrokePath(Cnv.Canvas.Handle);
      end
      else
        StrokeAndFillPath(Cnv.Canvas.Handle)
    else
      StrokePath(Cnv.Canvas.Handle)
  else
    FillPath(Cnv.Canvas.Handle);
  Cnv.Canvas.Pen.Style := PenStyle0;
  Cnv.Canvas.Pen.Color := PenColor0;
end;

procedure TPrimitive2D.DrawNative(const VT: TTransf2D; const Cnv:
  TDecorativeCanvas; const ClipRect2D: TRect2D;
  const DrawMode: Integer; const IsClosed: Boolean;
  const Pnts: array of TPoint);
var
  LineWidthBase, PixelSize, UnitLength, Scale, MiterLimit: TRealType;
begin
  if OwnerCAD = nil then
  begin
    LineWidthBase := 1;
    PixelSize := 1;
    UnitLength := 1;
    Scale := 1;
  end
  else
  begin
    LineWidthBase := (OwnerCAD as TDrawing2D).LineWidthBase;
    PixelSize := OwnerCAD.Viewports[0].GetPixelAperture.X;
    UnitLength := (OwnerCAD as TDrawing2D).PicUnitLength;
    Scale := (OwnerCAD as TDrawing2D).PicScale;
    MiterLimit := (OwnerCAD as TDrawing2D).MiterLimit;
  end;
  DrawNative0(VT, Cnv, DrawPath, ClipRect2D, DrawMode, IsClosed, Pnts,
    fLineStyle, fLineWidth, fHatching, fLineColor, fFillColor, fHatchColor,
    LineWidthBase, PixelSize, UnitLength, Scale, MiterLimit);
end;

procedure TPrimitive2D.DrawPiece(Piece: TPiece;
  const VT: TTransf2D; const Cnv:
  TDecorativeCanvas; const ClipRect2D: TRect2D; const
  DrawMode: Integer);
var
  PathProc: TDrawPathProc;
  Pnts: array of TPoint;
  I: Integer;
  LineWidthBase, PixelSize, UnitLength, Scale, MiterLimit: TRealType;
begin
  //if OwnerCAD = nil then Exit;
  if OwnerCAD = nil then
  begin
    LineWidthBase := 1;
    PixelSize := 1;
    UnitLength := 1;
    Scale := 1;
  end
  else
  begin
    LineWidthBase := (OwnerCAD as TDrawing2D).LineWidthBase;
    PixelSize := OwnerCAD.Viewports[0].GetPixelAperture.X;
    UnitLength := (OwnerCAD as TDrawing2D).PicUnitLength;
    Scale := (OwnerCAD as TDrawing2D).PicScale;
    MiterLimit := (OwnerCAD as TDrawing2D).MiterLimit;
  end;
  if Piece.Count <= 1 then Exit;
  //IsClosed := IsSamePoint2D(Piece[0], Piece[Piece.Count - 1]);
  if Piece is TLinPath then PathProc := DrawPolyline
  else //if Piece is TBezierPath
    PathProc := DrawBezierPath;
  SetLength(Pnts, Piece.Count);
  for I := 0 to Piece.Count - 1 do
    Pnts[I] := Point2DToPoint(TransformPoint2D(Piece[I], VT));
  DrawNative0(VT, Cnv, PathProc, ClipRect2D, DrawMode, Piece.Closed, Pnts,
    Piece.GetLineStyle(Self), Piece.GetLineWidth(Self), Piece.GetHatching(Self),
    Piece.GetLineColor(Self), Piece.GetFillColor(Self), fHatchColor,
    LineWidthBase, PixelSize, UnitLength, Scale, MiterLimit);
end;

procedure TPrimitive2D.DrawPieces(const VT: TTransf2D; const Cnv:
  TDecorativeCanvas; const ClipRect2D: TRect2D; const
  DrawMode: Integer);
var
  I: Integer;
begin
  for I := 0 to Pieces.Count - 1 do
    if Pieces[I] <> nil then
      DrawPiece(Pieces[I], VT, Cnv, ClipRect2D, DrawMode);
end;

procedure TPrimitive2D.DrawControlPoints0(const VT: TTransf2D;
  const Cnv: TDecorativeCanvas; const Width:
  Integer);
var
  TmpPt: TPoint2D;
  Cont: Integer;
begin
  for Cont := 0 to Points.Count - 1 do
  begin
    TmpPt := TransformPoint2D(Points[Cont], VT);
    DrawPlaceHolder(Cnv, Round(TmpPt.X), Round(TmpPt.Y),
      Width);
  end;
end;

procedure TPrimitive2D.Draw(VT: TTransf2D; const Cnv:
  TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode:
  Integer);
var
  I: Integer;
begin
  BeginUseProfile;
  try
    for I := 0 to fProfile.Count - 1 do
    begin
      DrawPoints(fProfile.Item[I], not fProfile.Item[I].Hatched,
        fProfile.Item[I].Closed,
        VT, Cnv, ClipRect2D, DrawMode);

    end;
  finally
    EndUseProfile;
  end;
end;

function TPrimitive2D.OnMe(PT: TPoint2D; Aperture: TRealType;
  var Distance: TRealType): Integer;
begin
  BeginUseProfile;
  try
    Result := inherited OnMe(PT, Aperture, Distance);
    if Result = PICK_INBBOX then
    begin
      Result := fProfile.OnMe(PT, Aperture, Distance);
    end;
  finally
    EndUseProfile;
  end;
  if (Result >= PICK_ONOBJECT) and
    (fLineStyle <> liNone) and (Distance <= Self.fLineWidth / 2) then
    Distance := 0;
end;

function TPrimitive2D.OnProfile(PT: TPoint2D; Aperture:
  TRealType): Integer;
begin
  BeginUseProfile;
  try
    Result := Profile.OnProfile(PT, Aperture);
  finally
    EndUseProfile;
  end;
end;

procedure TPrimitive2D.Transform(const T: TTransf2D);
begin
  fPoints.TransformPoints(T);
  if ScaleLineWidth then
    LineWidth := LineWidth * IsotropicScale(T);
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
    if OwnerCAD is TDrawing2D then
      (OwnerCAD as TDrawing2D).NotifyChanged;
end;

function TPrimitive2D.InsertControlPoint0(const Pos: Integer; P:
  TPoint2D): Integer;
begin
  Result := 0;
  if (Pos < 0) or (Pos > fPoints.Count) then Exit;
  if Pos < fPoints.Count then fPoints.Insert(Pos, P)
  else fPoints.Add(P);
  Inc(Result);
end;

procedure TPrimitive2D.InsertControlPoint(const Pos: Integer; P:
  TPoint2D);
begin
  if not fCanDeletePoints then Exit;
  if InsertControlPoint0(Pos, P) > 0 then
    if OwnerCAD is TDrawing2D then
      (OwnerCAD as TDrawing2D).NotifyChanged;
end;

procedure TPrimitive2D.MoveControlPoint0(const Pos: Integer;
  P: TPoint2D; Shift: TShiftState);
begin
  fPoints[Pos] := P;
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
  = ((0, -1), (0, 0), (1, 0), (1, -1), (1, 1), (1, 0), (0, 0), (0, 1), (0, -1));
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
  = ((0, 0), (1, -1), (0.1, -0.1), (0.1, 0), (1, 0), (2, -1), (1, 0),
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
  = ((0, 0), (1, -1), (0.1, -0.1), (0.1, 0), (1, 0), (2, -1), (1, 0),
    (2, 0), (3, -1), (2, 0), (3, 1), (2, 0),
    (1, 0), (2, 1), (1, 0), (0.1, 0), (0.1, 0.1), (1, 1), (0, 0));

procedure FillCircleProfilePoints(PP: TPointsSet2D;
  const CP: TPoint2D; const R: TRealType; const CurvePrecision: Word);
var
  Cont: Integer;
  Delta, CurrAngle: TRealType;
begin
  Delta := TWOPI / CurvePrecision;
  CurrAngle := 0;
  PP.Clear;
  for Cont := 0 to CurvePrecision do
  begin
    PP.Add(
      Point2D(CP.X + R * Cos(CurrAngle),
      CP.Y + R * Sin(CurrAngle)));
    CurrAngle := CurrAngle + Delta;
  end;
end;

class function TLine2D.GetName: string;
begin
  Result := 'Line';
end;

constructor TLine2D.Create(ID: Integer);
begin
  inherited CreateSpec(ID, 2, 50);
  WhenCreated;
end;

constructor TLine2D.CreateSpec(ID: Integer; const P1, P2: TPoint2D);
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
begin
  if (Obj = Self) then
    Exit;
  inherited Assign(Obj);
  if Obj is TPrimitive2D then
  begin
    fPoints.DisableEvents := True;
    if (Obj as TPrimitive2D).fPoints.Count > 1 then
      fPoints.Copy((Obj as TPrimitive2D).fPoints, 0, 1)
    else
      if (Obj as TPrimitive2D).fPoints.Count = 1 then
      begin
        fPoints.Copy((Obj as TPrimitive2D).fPoints, 0, 0);
        fPoints[1] := Point2D(fPoints[0].X, fPoints[0].Y + 1);
      end;
    fPoints.DisableEvents := False;
    fPoints.GrowingEnabled := False;
  end;
  UpdateExtension(Self);
end;

constructor TLine2D.CreateFromStream(const Stream: TStream;
  const Version: TCADVersion);
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

procedure FillArrow(const Kind: TArrowKind;
  const P0, P1: TPoint2D; const ArrowSize: TRealType;
  Pieces: TArrayOfPiece);
var
  APiece: TPiece;
  V: TVector2D;
  T: TTransf2D;
  procedure Fill(const Arr: array of TSimplePoint2D);
  var
    I: Integer;
  begin
    APiece := TLinPath.Create(Length(Arr));
    for I := 0 to Length(Arr) - 1 do
      APiece.Add(Point2D(Arr[I][1], Arr[I][2]));
  end;
begin
  if Kind = arrNone then Exit;
  V := Direction2D(P0, P1);
  case Kind of
    arrH40: Fill(ArrH40Arr);
    arrH41: Fill(ArrH41Arr);
    arrH42: Fill(ArrH42Arr);
    arrH43: Fill(ArrH43Arr);
    arrH44: Fill(ArrH44Arr);
    arrH45: Fill(ArrH45Arr);
    arrH46: Fill(ArrH46Arr);
    arrH47: Fill(ArrH47Arr);
    arrH48: Fill(ArrH48Arr);
    arrT40: Fill(ArrT40Arr);
    arrT43: Fill(ArrT43Arr);
    arrT44: Fill(ArrT44Arr);
    arrT45: Fill(ArrT45Arr);
    arrH20: Fill(ArrH20Arr);
    arrH21: Fill(ArrH21Arr);
    arrH22: Fill(ArrH22Arr);
    arrH23: Fill(ArrH23Arr);
    arrH24: Fill(ArrH24Arr);
    arrT20: Fill(ArrT20Arr);
    arrT21: Fill(ArrT21Arr);
    arrT22: Fill(ArrT22Arr);
    arrT23: Fill(ArrT23Arr);
    arrHR10: Fill(ArrHR10Arr);
    arrHR11: Fill(ArrHR11Arr);
    arrHR12: Fill(ArrHR12Arr);
    arrTR10: Fill(ArrTR10Arr);
    arrH10: Fill(ArrH10Arr);
    arrH11: Fill(ArrH11Arr);
    arrH12: Fill(ArrH12Arr);
    arrH12C: Fill(ArrH12CArr);
    arrT10: Fill(ArrT10Arr);
    arrR0: Fill(ArrR0Arr);
    arrR10: Fill(ArrR10Arr);
    arrR11: Fill(ArrR11Arr);
    arrR12: Fill(ArrR12Arr);
    arrR20: Fill(ArrR20Arr);
    arrR20C: Fill(ArrR20CArr);
    arrR21: Fill(ArrR21Arr);
    arrR33: Fill(ArrR33Arr);
    arrTS10: Fill(ArrTS10Arr);
    arrTS11: Fill(ArrTS11Arr);
    arrTS12: Fill(ArrTS12Arr);
    arrHS10: Fill(ArrHS10Arr);
    arrHS12: Fill(ArrHS12Arr);
    arrTS20: Fill(ArrTS20Arr);
    arrTS21: Fill(ArrTS21Arr);
    arrTS23: Fill(ArrTS23Arr);
    arrHS20: Fill(ArrHS20Arr);
    arrHS23: Fill(ArrHS23Arr);
    arrQQ: APiece := Pieces.AddFromSvgPath(
        'M 0 0 L 1 -0.5 L 1.5 -1.5 L 2 -0.5 L 3 0 L 2 0.5 L 1.5 1.5 L 1 0.5 Z');
    arrOC:
      begin
        APiece := TLinPath.Create(31);
        FillCircleProfilePoints(
          APiece, Point2D(0, 0), 1 {radius}, 30);
      end;
  else //arrO
    begin
      APiece := TLinPath.Create(31);
      FillCircleProfilePoints(
        APiece, Point2D(ArrowSize, 0), 1, 30);
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
  APiece.Closed := True;
end;

procedure TLine2D.FillPieces;
var
  P0, P1: TPoint2D;
begin
  P0 := fPoints[0];
  P1 := fPoints[1];
  Pieces.Add(TLinPath.Create(2));
  Pieces.AddPointToLast(P0);
  Pieces.AddPointToLast(P1);
  if PointDistance2D(P0, P1) = 0 then Exit;
  FillArrow(BeginArrowKind, P0, P1, ArrowSize, Pieces);
  FillArrow(EndArrowKind, P1, P0, ArrowSize, Pieces);
end;

procedure TLine2D.BezierPoints(PP: TPointsSet2D);
begin
  PP.Clear;
  PP.Expand(4);
  PP.AddPoints(
    [fPoints[0],
    MixPoint(fPoints[0], fPoints[1], 0.25),
      MixPoint(fPoints[0], fPoints[1], 0.75),
      fPoints[1]]);
end;

procedure TLine2D.Draw(VT: TTransf2D; const Cnv:
  TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode:
  Integer);
begin
  DrawPieces(VT, Cnv, ClipRect2D, DrawMode);
end;

function TLine2D.OnMe(PT: TPoint2D; Aperture: TRealType;
  var Distance: TRealType): Integer;
var
  TmpDist: TRealType;
begin
  Result := inherited OnMe(PT, Aperture, Distance);
  if Result = PICK_INBBOX then
  begin
    Result := MaxIntValue([PICK_INBBOX,
      IsPointOnPolyLine2D(fPoints.PointsReference, fPoints.Count,
        PT, TmpDist, Aperture, IdentityTransf2D, False)]);
    Distance := MinValue([Aperture, TmpDist]);
  end;
end;

// =====================================================================
// TPolyline2D0
// =====================================================================

constructor TPolyline2D0.Create(ID: Integer);
begin
  inherited Create(ID);
  WhenCreated;
end;

constructor TPolyline2D0.CreateSpec(ID: Integer; const Pts: array of
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
  fIsClosed := False;
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
  //APath.Closed := False;
  APath.Copy(fPoints, 0, fPoints.Count - 1);
end;

procedure TPolyline2D0.Assign(const Obj: TGraphicObject);
var
  I: Integer;
begin
  if (Obj = Self) then
    Exit;
  inherited Assign(Obj);
  if Obj is TPrimitive2D then
  begin
    {if Obj is TStar2D then
    begin
      if (Obj as TStar2D).Pieces.Count > 0 then
        fPoints.Copy((Obj as TStar2D).Pieces[0], 0,
          (Obj as TStar2D).Pieces[0].Count - 1)
    end
    else}
    fPoints.DisableEvents := True;
    if (Obj is TPrimitive2D) and not (Obj is TPolyline2D0) then
    begin
      (Obj as TPrimitive2D).BeginUseProfile;
      try
        if (Obj as TPrimitive2D).Profile.Count > 0 then
          fPoints.Copy((Obj as TPrimitive2D).Profile.Item[0], 0,
            (Obj as TPrimitive2D).Profile.Item[0].Count - 1)
      finally
        (Obj as TPrimitive2D).EndUseProfile;
      end;
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

procedure TPolyline2D0.Draw(VT: TTransf2D; const Cnv:
  TDecorativeCanvas; const ClipRect2D: TRect2D; const
  DrawMode: Integer);
begin
  DrawPieces(VT, Cnv, ClipRect2D, DrawMode);
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
  FillArrow(BeginArrowKind, fPoints[0], fPoints[1], ArrowSize, Pieces);
  FillArrow(EndArrowKind,
    fPoints[fPoints.Count - 1], fPoints[fPoints.Count - 2],
    ArrowSize, Pieces);
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

constructor TPolygon2D.CreateSpec(ID: Integer; const Pts: array of
  TPoint2D);
begin
  inherited CreateSpec(ID, Pts);
  WhenCreated;
end;

procedure TPolygon2D.WhenCreated;
begin
  fIsClosed := True;
end;

procedure TPolygon2D.FillPieces;
begin
  inherited FillPieces;
  Pieces[0].Closed := True;
end;

procedure TPolygon2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
    Exit;
  inherited Assign(Obj);
  WhenCreated;
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
  = ((0.95, 0.318), (0, 1), (-0.95, 0.318), (-0.59, -0.81), (0.59, -0.81));
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
  = ((0.95, 0.31), (0.22, 0.31), (0, 1), (-0.22, 0.31), (-0.95, 0.31),
    (-0.36, -0.12), (-0.59, -0.81), (0, -0.38), (0.59, -0.81), (0.36, -0.12));
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
    const R: TRealType): TPiece;
  var
    I: Integer;
  begin
    Result := TLinPath.Create(Count);
    for I := 0 to Pred(Count) do
      Result.Add(Point2D(PArr^[I][1] * R, PArr^[I][2] * R));
    Pieces.Add(Result);
  end;
begin
  if OwnerCAD is TDrawing2D then
    StarSize := (OwnerCAD as TDrawing2D).StarsSize
  else StarSize := 1;
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
      APiece := TLinPath.Create(30);
      Pieces.Add(APiece);
      FillCircleProfilePoints(APiece,
        Point2D(0, 0), StarSize, 30);
    end;
  end;
  APiece.TransformPoints(Translate2D(fPoints[0].X, fPoints[0].Y));
  APiece.Line := pliSolidDefault;
  APiece.Fill := pfiLineAsDefault;
  APiece.Hatch := phaNone;
  APiece.Closed := True;
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
  fLineStyle := liSolid;
  fLineWidth := 1;
  fPoints.GrowingEnabled := False;
end;

procedure TStar2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
    Exit;
  inherited Assign(Obj);
  if (Obj is TPrimitive2D) and not (Obj is TStar2D) then
  begin
    fPoints[0] := BoxCenter((Obj as TPrimitive2D).Box);
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

constructor TStar2D.CreateFromStream(const Stream: TStream;
  const Version: TCADVersion);
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

procedure TStar2D.Draw(VT: TTransf2D; const Cnv:
  TDecorativeCanvas; const ClipRect2D: TRect2D; const
  DrawMode: Integer);
begin
  DrawPieces(VT, Cnv, ClipRect2D, DrawMode);
end;

// =====================================================================
// TBox2D0
// =====================================================================

procedure RectangleCalcPoints(P0, P1, P2: TPoint2D;
  var P3, P4: TPoint2D; var A: TRealType);
begin
  A := (Sqr(P2.X - P0.X) + Sqr(P2.Y - P0.Y));
  if A = 0 then
  begin
    A := 1;
    P2 := Point2D(P0.X, P1.Y);
  end
  else A := ((P1.X - P0.X) * (P2.X - P0.X)
      + (P1.Y - P0.Y) * (P2.Y - P0.Y)) / A;
  P3 := MixPoint(P0, P2, A);
  P4 := ShiftPoint(P0, Vector2D(P3, P1));
end;

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

function TBox2D0.FillProfile: TRect2D;
var
  A: TRealType;
  P0, P1, P2, P3, P4: TPoint2D;
begin
  if CurvePrecision = 0 then
  begin
    Result := Rect2D(0, 0, 0, 0);
    Exit;
  end;
  NewProfileItem(4);
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
  if (A <> 1) and (A <> 0) then fPoints[2] := P3;
  Profile.AddPointToLast(P0);
  Profile.AddPointToLast(P3);
  Profile.AddPointToLast(P1);
  Profile.AddPointToLast(P4);
  Result := Profile.GetExtension;
end;

constructor TBox2D0.Create(ID: Integer);
begin
  inherited CreateSpec(ID, 3, 150);
  fPoints.DisableEvents := True;
  try
    fPoints.Add(Point2D(0, 0));
    fPoints.Add(Point2D(0, 0));
    fPoints.Add(Point2D(0, 0));
  finally
    fPoints.DisableEvents := False;
    UpdateExtension(Self);
  end;
  WhenCreated;
end;

procedure TBox2D0.WhenCreated;
begin
  fPoints.GrowingEnabled := False;
  fIsClosed := True;
end;

procedure TBox2D0.Assign(const Obj: TGraphicObject);
var
  R: TRect2D;
begin
  if (Obj = Self) then
    Exit;
  inherited Assign(Obj);
  if Obj is TBox2D0
    and ((Obj as TPrimitive2D).fPoints.Count > 2) then
    fPoints.Copy((Obj as TPrimitive2D).fPoints, 0, 2)
  else
    if Obj is TPrimitive2D then
    begin
    //R := (Obj as TPrimitive2D).fPoints.Extension;
      R := (Obj as TPrimitive2D).Box;
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

constructor TRectangle2D.Create(ID: Integer);
begin
  inherited Create(ID);
  WhenCreated;
end;

procedure TRectangle2D.WhenCreated;
begin
  fDrawPathBezier := False;
end;

procedure TRectangle2D.Assign(const Obj: TGraphicObject);
begin
  fPoints.DisableEvents := True;
  try
    inherited Assign(Obj);
    WhenCreated;
  finally
    fPoints.DisableEvents := False;
  end;
  UpdateExtension(Self);
end;

procedure TRectangle2D.Draw(VT: TTransf2D; const Cnv:
  TDecorativeCanvas; const ClipRect2D: TRect2D; const
  DrawMode: Integer);
var
  Pnts: array of TPoint;
  I: Integer;
begin
  {if not (Cnv.Canvas is TMetaFileCanvas) then
  begin
    inherited;
    Exit;
  end;}
  BeginUseProfile;
  try
    SetLength(Pnts, Profile.Item[0].Count);
    for I := 0 to Profile.Item[0].Count - 1 do
      Pnts[I] := Point2DToPoint(
        TransformPoint2D(Profile.Item[0][I], VT));
  finally
    EndUseProfile;
  end;
  DrawNative(VT, Cnv, ClipRect2D, DrawMode, IsClosed, Pnts);
end;


// =====================================================================
// TEllipse2D
// =====================================================================

class function TEllipse2D.GetName: string;
begin
  Result := 'Ellipse';
end;

constructor TEllipse2D.Create(ID: Integer);
begin
  inherited Create(ID);
  WhenCreated;
end;

procedure TEllipse2D.WhenCreated;
begin
  fCurvePrecision := 150;
  fDrawPathBezier := True;
end;

procedure TEllipse2D.Assign(const Obj: TGraphicObject);
begin
  fPoints.DisableEvents := True;
  try
    inherited Assign(Obj);
    WhenCreated;
  finally
    fPoints.DisableEvents := False;
  end;
  UpdateExtension(Self);
end;

function GetEllipseParams00(const P0, P1: TPoint2D;
  P2: TPoint2D; var P3, P4: TPoint2D;
  var CX, CY, RX, RY, ARot: TRealType): Boolean;
var
  A: TRealType;
begin
  //Result is true if P2 needs to be updated by P3
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
  RX := PointDistance2D(P1, P3) / 2.0;
  RY := PointDistance2D(P0, P3) / 2.0;
  CX := (P1.X + P0.X) / 2.0;
  CY := (P1.Y + P0.Y) / 2.0;
  ARot := Pi / 2 + 2 * Pi - TwoPointsAngle(P0, P2);
end;

procedure GetEllipseParams0(const P0, P1, P2: TPoint2D;
  var P3, P4: TPoint2D;
  var CX, CY, RX, RY, ARot: TRealType);
var
  A: TRealType;
begin
  GetEllipseParams00(P0, P1, P2, P3, P4,
    CX, CY, RX, RY, ARot);
end;

procedure TEllipse2D.GetEllipseParams(
  var CX, CY, RX, RY, ARot: TRealType);
var
  P2, P3, P4: TPoint2D;
begin
  P2 := fPoints[2];
  if GetEllipseParams00(fPoints[0], fPoints[1], P2, P3, P4,
    CX, CY, RX, RY, ARot)
    then fPoints[2] := P3;
end;

function TEllipse2D.FillProfile: TRect2D;
var
  Cont: Integer;
  CP, P: TPoint2D;
  Delta, CurrAngle, RX, RY, ARot: TRealType;
begin
  GetEllipseParams(CP.X, CP.Y, RX, RY, ARot);
  Delta := TWOPI / CurvePrecision;
  CurrAngle := 0;
  NewProfileItem(CurvePrecision + 1);
  for Cont := 1 to CurvePrecision do
  begin
    P := Point2D(CP.X + RX * Cos(CurrAngle),
      CP.Y - RY * Sin(CurrAngle));
    P := TransformPoint2D(P, RotateCenter2D(-ARot, CP));
    Profile.AddPointToLast(P);
    CurrAngle := CurrAngle + Delta;
  end;
  Result := Profile.GetExtension;
end;

const
  Ell_Cnst = 0.265206;
  ISqrt2 = 0.707106781186547; // 1/sqrt(2)

procedure EllipseBezierPoints(CP: TPoint2D;
  RX, RY, ARot: TRealType; PP: TPointsSet2D;
  T: TTransf2D);
var
  P0, P1, P2, Q0, Q1, Q2, Q3: TPoint2D;
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
  T := MultiplyTransform2D(Translate2D(CP.X, CP.Y), T);
  if ARot <> 0 then
    T := MultiplyTransform2D(Rotate2D(-ARot), T);
  T := MultiplyTransform2D(Scale2D(RX, RY), T);
  for I := 0 to 24 do
    PP[I] := TransformPoint2D(PP[I], T);
end;

procedure TEllipse2D.BezierPoints(PP: TPointsSet2D);
var
  CP: TPoint2D;
  RX, RY, ARot: TRealType;
begin
  GetEllipseParams(CP.X, CP.Y, RX, RY, ARot);
  EllipseBezierPoints(CP, RX, RY, ARot, PP, IdentityTransf2D);
end;

procedure TEllipse2D.Draw(VT: TTransf2D; const Cnv:
  TDecorativeCanvas; const ClipRect2D: TRect2D; const
  DrawMode: Integer);
var
  PP: TPointsSet2D;
  Pnts: array of TPoint;
  I: Integer;
begin
  //inherited;
  {if not (Cnv.Canvas is TMetaFileCanvas) then
  begin
    inherited;
    Exit;
  end;}
  PP := TPointsSet2D.Create(0);
  try
    BezierPoints(PP);
    PP.TransformPoints(VT);
    SetLength(Pnts, PP.Count);
    for I := 0 to PP.Count - 1 do
      Pnts[I] := Point2DToPoint(PP[I]);
  finally
    PP.Free;
  end;
  DrawNative(VT, Cnv, ClipRect2D, DrawMode, IsClosed, Pnts);
end;

// =====================================================================
// TCircle2D
// =====================================================================

class function TCircle2D.GetName: string;
begin
  Result := 'Circle';
end;

function TCircle2D.FillProfile: TRect2D;
var
  R: TRealType;
  CP: TPoint2D;
begin
  if CurvePrecision = 0 then
  begin
    Result := Rect2D(0, 0, 0, 0);
    Exit;
  end;
  CP := fPoints[0];
  R := PointDistance2D(CP, fPoints[1]);
  NewProfileItem(CurvePrecision + 1);
  FillCircleProfilePoints(Profile.Item[0],
    CP, R, CurvePrecision);
  Result := Profile.GetExtension;
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
  fIsClosed := True;
  fCurvePrecision := 150;
end;

procedure TCircle2D.Assign(const Obj: TGraphicObject);
var
  R: TRect2D;
begin
  if (Obj = Self) then
    Exit;
  inherited Assign(Obj);
  if Obj is TPrimitive2D then
  begin
    if (Obj is TCircle2D) or (Obj is TCircular2D) then
      fPoints.Copy((Obj as TPrimitive2D).fPoints, 0, 1)
    else
    begin
      R := (Obj as TPrimitive2D).Box;
      fPoints.Clear;
      fPoints.AddPoints([BoxCenter(R), R.FirstEdge]);
      if (Obj is TEllipse2D) or (Obj is TStar2D)
        or (Obj is TBezierPath2D0) or (Obj is TSmoothPath2D0)
        or (Obj is TPolyline2D0)
        then
        fPoints[1] := MixPoint(fPoints[0], fPoints[1], 1 / Sqrt(2));
    end;
  end;
  WhenCreated;
  UpdateExtension(Self);
end;

procedure TCircle2D.BezierPoints(PP: TPointsSet2D);
var
  R: TRealType;
  CP: TPoint2D;
begin
  CP := fPoints[0];
  R := PointDistance2D(CP, fPoints[1]);
  EllipseBezierPoints(CP, R, R, 0, PP, IdentityTransf2D);
end;

procedure TCircle2D.DrawPath(const Canvas: TCanvas;
  const IsClosed: Boolean; const Pnts: array of TPoint);
begin
  {Ellipse(Canvas.Handle, Pnts[0].X, Pnts[0].Y,
    Pnts[1].X, Pnts[1].Y);}
  DrawBezierPath(Canvas, IsClosed, Pnts);
end;

procedure TCircle2D.Draw(VT: TTransf2D; const Cnv:
  TDecorativeCanvas; const ClipRect2D: TRect2D; const
  DrawMode: Integer);
var
  PP: TPointsSet2D;
  Pnts: array of TPoint;
  I: Integer;
begin
  PP := TPointsSet2D.Create(0);
  try
    BezierPoints(PP);
    PP.TransformPoints(VT);
    SetLength(Pnts, PP.Count);
    for I := 0 to PP.Count - 1 do
      Pnts[I] := Point2DToPoint(PP[I]);
  finally
    PP.Free;
  end;
  DrawNative(VT, Cnv, ClipRect2D, DrawMode, IsClosed, Pnts);
end;

// =====================================================================
// TCircular2D
// =====================================================================

procedure ArcBezier(CP: TPoint2D; R, SA, EA: TRealType;
  PP: TPointsSet2D);
var
  Angle, Delta, AA, BB: TRealType;
  I, NArcs: Integer;
  S, P0, P3: TPoint2D;
begin // Bezier path approximation to a circular arc
  Angle := EA - SA - Floor((EA - SA) / (2 * Pi)) * (2 * Pi);
  NArcs := Ceil(Angle * 4 / Pi);
  PP.Clear;
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
begin
  fRadius := R;
end;

procedure TCircular2D.SetStartAngle(A: TRealType);
var
  CX, CY, R, SA, EA: TRealType;
begin
  if FStartAngle <> A then
  begin
    FStartAngle := A;
    GetArcParams(CX, CY, R, SA, EA);
    fPoints[1] := Point2D(CX + R * Cos(A), CY + R * Sin(A));
  end;
end;

procedure TCircular2D.SetEndAngle(A: TRealType);
var
  CX, CY, R, SA, EA: TRealType;
begin
  if FEndAngle <> A then
  begin
    FEndAngle := A;
    GetArcParams(CX, CY, R, SA, EA);
    fPoints[2] := Point2D(CX + R * Cos(A), CY + R * Sin(A));
  end;
end;

function TCircular2D.FillProfile: TRect2D;
  procedure GetArcPoints(PP: TPointsSet2D;
    NPts: Integer);
  var
    Cont: Integer;
    CX, CY, R: TRealType;
    Delta, CurrAngle: TRealType;
  begin
    if NPts < 2 then NPts := 150;
// Angles are in radiants
    GetArcParams(CX, CY, R, FStartAngle, FEndAngle);
  // Calcola il delta angolare tra due punti
    if FStartAngle < FEndAngle then
      Delta := (FEndAngle - FStartAngle) / (NPts - 1)
    else
      Delta := (TWOPI - FStartAngle + FEndAngle) / (NPts - 1);
  // Crea il vettore curvilineo.
  // Popola il vettore curvilineo.
    CurrAngle := FStartAngle;
    for Cont := 1 to NPts do
    begin
      PP.Add(Point2D(CX + R * Cos(CurrAngle),
        CY + R * Sin(CurrAngle)));
      CurrAngle := CurrAngle + Delta
    end;
  end;
begin
  if CurvePrecision = 0 then
  begin
    Result := Rect2D(0, 0, 0, 0);
    Exit;
  end;
  if Self is TSector2D then NewProfileItem(CurvePrecision + 1)
  else NewProfileItem(CurvePrecision);
  GetArcPoints(Profile.Item[0], CurvePrecision);
  if Self is TSector2D then Profile.AddPointToLast(fPoints[0]);
  Result := Profile.GetExtension;
end;

constructor TCircular2D.Create(ID: Integer);
begin
  inherited CreateSpec(ID, 3, 150);
  WhenCreated;
end;

constructor TCircular2D.CreateSpec(ID: Integer; const CP: TPoint2D;
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
    FStartAngle := SA;
    FEndAngle := EA;
    fRadius := R;
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
  fCurvePrecision := 150;
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
  if (Obj = Self) then
    Exit;
  inherited Assign(Obj);
  fPoints.DisableEvents := True;
  try
    if Obj is TPrimitive2D then
    begin
      if Obj is TCircle2D then
      begin
        fPoints.Clear;
        fPoints.AddPoints([(Obj as TPrimitive2D).fPoints[0],
          (Obj as TPrimitive2D).fPoints[1], (Obj as TPrimitive2D).fPoints[1]]);
        StartAngle := 0;
        EndAngle := 2 * Pi - 1E-5;
      end
      else if (Obj as TPrimitive2D).fPoints.Count > 2 then
        fPoints.Copy((Obj as TPrimitive2D).fPoints, 0, 2)
      else
      begin
        R := (Obj as TPrimitive2D).Box;
        fPoints.Clear;
        fPoints.AddPoints([BoxCenter(R), R.FirstEdge, R.SecondEdge]);
      end;
      fPoints.GrowingEnabled := False;
    end;
    if Obj is TCircular2D then
    begin
      StartAngle := (Obj as TCircular2D).StartAngle;
      EndAngle := (Obj as TCircular2D).EndAngle;
    end;
    WhenCreated;
    fIsClosed := not (Self is TArc2D);
  finally
    fPoints.DisableEvents := False;
  end;
  UpdateExtension(Self);
end;

constructor TCircular2D.CreateFromStream(const Stream: TStream;
  const
  Version: TCADVersion);
begin
  { Load the standard properties }
  inherited;
end;

procedure TCircular2D.SaveToStream(const Stream: TStream);
begin
  { Save the standard properties }
  inherited SaveToStream(Stream);
end;

procedure TCircular2D.BezierPoints(PP: TPointsSet2D);
var
  CP: TPoint2D;
  R, SA, EA: TRealType;
  I: Integer;
begin
  GetArcParams(CP.X, CP.Y, R, SA, EA);
  ArcBezier(CP, R, SA, EA, PP);
  {if Self is TSegment2D then
    PP.AddPoints([fPoints[2], fPoints[1], fPoints[1]]);}
  if Self is TSector2D then
  begin
    PP.AddPoints(
      [MixPoint(PP[PP.Count - 1], fPoints[0], 0.25),
      MixPoint(PP[PP.Count - 1], fPoints[0], 0.75),
        fPoints[0]]);
  end;
end;

procedure TCircular2D.DrawPath(const Canvas: TCanvas;
  const IsClosed: Boolean; const Pnts: array of TPoint);
begin
  DrawBezierPath(Canvas, IsClosed, Pnts);
  Exit;
  {if Self is TSector2D then
  begin
    Pie(Canvas.Handle,
      Pnts[0].X, Pnts[0].Y, Pnts[1].X, Pnts[1].Y,
      Pnts[2].X, Pnts[2].Y, Pnts[3].X, Pnts[3].Y);
    Exit;
  end;}
  BeginPath(Canvas.Handle);
  MoveToEx(Canvas.Handle, Pnts[2].X, Pnts[2].Y, nil);
  ArcTo(Canvas.Handle,
    Pnts[0].X, Pnts[0].Y, Pnts[1].X, Pnts[1].Y,
    Pnts[2].X, Pnts[2].Y, Pnts[3].X, Pnts[3].Y);
  if not (Self is TArc2D) then
  begin
    if Self is TSector2D then
      LineTo(Canvas.Handle, Pnts[4].X, Pnts[4].Y);
    //LineTo(Canvas.Handle, Pnts[2].X, Pnts[2].Y);
  end;
  if not (Self is TArc2D) then
    CloseFigure(Canvas.Handle);
  EndPath(Canvas.Handle);
end;

procedure TCircular2D.Draw(VT: TTransf2D; const Cnv:
  TDecorativeCanvas; const ClipRect2D: TRect2D; const
  DrawMode: Integer);
var
  PP: TPointsSet2D;
  Pnts: array of TPoint;
  I: Integer;
  CX, CY, R, SA, EA: TRealType;
begin
  if FirstDrawPoint = 2 then
  begin
    GetArcParams(CX, CY, R, SA, EA);
    R := PointDistance2D(fPoints[0], fPoints[2]);
    fPoints[1] :=
      Point2D(CX + R * Cos(SA), CY + R * Sin(SA));
  end;
  PP := TPointsSet2D.Create(0);
  try
    BezierPoints(PP);
    PP.TransformPoints(VT);
    if PP.Count > 0 then
    begin
      SetLength(Pnts, PP.Count);
      for I := 0 to PP.Count - 1 do
        Pnts[I] := Point2DToPoint(PP[I]);
      DrawNative(VT, Cnv, ClipRect2D, DrawMode, IsClosed, Pnts);
    end;
  finally
    PP.Free;
  end;
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
  fIsClosed := True;
  fOwnsInterior := True;
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
  fIsClosed := True;
  fOwnsInterior := True;
end;

// =====================================================================
// TBezierPrimitive2D
// =====================================================================

procedure TBezierPrimitive2D.FillPieces;
var
  APath: TBezierPath;
begin
  APath := TBezierPath.Create(0);
  Pieces.Add(APath);
  APath.Line := pliDefault;
  APath.Fill := pfiDefault;
  APath.Hatch := phaDefault;
  BezierPoints(APath);

  Pieces.Item[0].Closed := fIsClosed;

  if fIsClosed then Exit;
  if fPoints.Count < 2 then Exit;
  FillArrow(BeginArrowKind,
    Pieces.Item[0][0], Pieces.Item[0][1],
    ArrowSize, Pieces);
  FillArrow(EndArrowKind,
    Pieces.Item[0][Pieces.Item[0].Count - 1],
    Pieces.Item[0][Pieces.Item[0].Count - 2],
    ArrowSize, Pieces);
end;

procedure TBezierPrimitive2D.Draw(VT: TTransf2D; const Cnv:
  TDecorativeCanvas; const ClipRect2D: TRect2D; const
  DrawMode: Integer);
begin
  DrawPieces(VT, Cnv, ClipRect2D, DrawMode);
end;

function TBezierPrimitive2D.OnProfile(PT: TPoint2D; Aperture:
  TRealType): Integer;
begin
  Result := inherited OnProfile(PT, Aperture);
  Result := Result div BezierPrecision;
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
  if fPoints.Count < 2 then Exit;
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

constructor TBezierPath2D0.CreateFromStream(const Stream:
  TStream; const Version: TCADVersion);
begin
  { Load the standard properties }
  inherited;
  with Stream do
   { Load the particular properties. }
    ;
//  FirstDrawPoint := 0;
end;

procedure TBezierPath2D0.SaveToStream(const Stream: TStream);
begin
  { Save the standard properties }
  inherited SaveToStream(Stream);
  with Stream do
    ;
end;

procedure TBezierPath2D0.Assign(const Obj: TGraphicObject);
var
  PP: TPointsSet2D;
begin
  if (Obj = Self) then
    Exit;
  inherited Assign(Obj);
  fIsClosed := Self is TClosedBezierPath2D;
  if Obj is TPrimitive2D then
  begin
    fPoints.DisableEvents := True;
    if {(Obj is TPrimitive2D) and} not (Obj is TBezierPath2D0) then
    begin
      //BeginUseProfile;
      try
        PP := TPointsSet2D.Create(0);
        try
          (Obj as TPrimitive2D).BezierPoints(PP);
          if fIsClosed then
            if IsSamePoint2D(PP[0], PP[PP.Count - 1])
              then fPoints.Copy(PP, 0, PP.Count - 2)
            else
            begin
              fPoints.Copy(PP, 0, PP.Count - 1);
              fPoints.Add(MixPoint(PP[0], PP[PP.Count - 1], 0.75));
              fPoints.Add(MixPoint(PP[0], PP[PP.Count - 1], 0.25));
            end
          else fPoints.Copy(PP, 0, PP.Count - 1);
        finally
          PP.Free;
        end;
      finally
        //EndUseProfile;
      end;
      (Obj as TPrimitive2D).fDrawPathBezier := True;
    end
    else
    begin
      PP := TBezierPath2D0(Obj).fPoints;
      if TBezierPath2D0(Obj).IsClosed = fIsClosed then
        fPoints.Copy(PP, 0, PP.Count - 1)
      else if fIsClosed then
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
{  fPoints.DisableEvents := True;
  try
    WhenCreated;
  finally
    fPoints.DisableEvents := False;
  end;
    UpdateExtension(Self);}
end;

procedure TBezierPath2D0.DrawControlPoints0(const VT: TTransf2D;
  const Cnv: TDecorativeCanvas; const Width:
  Integer);
var
  TmpPt, TmpPt2: TPoint2D;
  Cont: Integer;
begin
  for Cont := 0 to fPoints.Count - 1 do
    if (Cont mod 3) <> 1 then
    begin
      if not IsClosed and (Cont = fPoints.Count - 1) then Break;
      TmpPt := TransformPoint2D(fPoints[Cont], VT);
      if Cont = fPoints.Count - 1 then
        TmpPt2 := TransformPoint2D(fPoints[0], VT)
      else
        TmpPt2 := TransformPoint2D(fPoints[Cont + 1], VT);
      Windows.MoveToEx(Cnv.Canvas.Handle, Round(TmpPt.X), Round(TmpPt.Y), nil);
      Windows.LineTo(Cnv.Canvas.Handle, Round(TmpPt2.X), Round(TmpPt2.Y));
    end;
  for Cont := 0 to fPoints.Count - 1 do
  begin
    TmpPt := TransformPoint2D(fPoints[Cont], VT);
    if ((Cont mod 3) = 0) {or (Cont = fPoints.Count - 1)} then
      DrawPlaceHolder(Cnv, Round(TmpPt.X), Round(TmpPt.Y),
        Width)
    else
      DrawRoundPlaceHolder(Cnv, Round(TmpPt.X), Round(TmpPt.Y),
        Width);
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
  {if OwnerCAD is TDrawing2D then
    (OwnerCAD as TDrawing2D).NotifyChanged;}
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
  if I = 0 then I := 1
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

function TBezierPath2D.InsertControlPoint0(const Pos: Integer; P:
  TPoint2D): Integer;
var
  P0, P1, P2, P3, P4, P5, P6: TPoint2D;
  UStar: TRealType;
begin
  Result := 0;
  if (Pos <= 0) or (3 * Pos >= fPoints.Count) then Exit;
  P0 := fPoints[3 * Pos - 3];
  P1 := fPoints[3 * Pos - 2];
  P2 := fPoints[3 * Pos - 1];
  P3 := fPoints[3 * Pos];
  UStar := ClosestBesier(P, P0, P1, P2, P3);
  BreakBezier(P0, P1, P2, P3, P4, P5, P6, UStar);
  fPoints[3 * Pos - 2] := P1;
  fPoints[3 * Pos - 1] := P5;
  fPoints.Insert(3 * Pos - 1, P4);
  fPoints.Insert(3 * Pos - 1, P3);
  fPoints.Insert(3 * Pos - 1, P2);
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

constructor TClosedBezierPath2D.CreateSpec(ID: Integer; const Pts:
  array of TPoint2D);
begin
  inherited CreateSpec(ID, Pts);
  WhenCreated;
end;

procedure TClosedBezierPath2D.WhenCreated;
begin
  fIsClosed := True;
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
  {if OwnerCAD is TDrawing2D then
    (OwnerCAD as TDrawing2D).NotifyChanged;}
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

function TClosedBezierPath2D.DeleteControlPoint0(I: Integer): Integer;
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
    GetPoint(I - 3), GetPoint(I - 2), GetPoint(I - 1), GetPoint(I),
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
    if fPoints.Count mod 3 = 0 then fPoints.Delete(fPoints.Count - 1);
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

function TClosedBezierPath2D.InsertControlPoint0(const Pos: Integer; P:
  TPoint2D): Integer;
var
  P0, P1, P2, P3, P4, P5, P6: TPoint2D;
  UStar: TRealType;
begin
  Result := 0;
  if (Pos <= 0) or (Pos > fPoints.Count div 3) then Exit;
  P0 := fPoints[3 * Pos - 3];
  P1 := fPoints[3 * Pos - 2];
  P2 := fPoints[3 * Pos - 1];
  if 3 * Pos <= fPoints.Count - 1 then P3 := fPoints[3 * Pos]
  else P3 := fPoints[0];
  UStar := ClosestBesier(P, P0, P1, P2, P3);
  BreakBezier(P0, P1, P2, P3, P4, P5, P6, UStar);
  fPoints[3 * Pos - 2] := P1;
  fPoints[3 * Pos - 1] := P5;
  fPoints.Insert(3 * Pos - 1, P4);
  fPoints.Insert(3 * Pos - 1, P3);
  fPoints.Insert(3 * Pos - 1, P2);
  Inc(Result, 3);
end;

procedure TClosedBezierPath2D.MoveControlPoint0(const Pos: Integer;
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
                then PosM := fPoints.Count - 1
              else Exit;
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
              else Exit
            else PosC := Pos + 1;
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
    then fPoints[fPoints.Count - 1]
    := ShiftPoint(fPoints[fPoints.Count - 1], V);
  fPoints[Pos] := P;
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
var
  I: Integer;
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
  else GetHobbyBezier(PP, fPoints);
end;

constructor TSmoothPath2D0.CreateFromStream(const Stream:
  TStream; const Version: TCADVersion);
begin
  { Load the standard properties }
  inherited;
  with Stream do
   { Load the particular properties. }
    ;
end;

procedure TSmoothPath2D0.SaveToStream(const Stream: TStream);
begin
  { Save the standard properties }
  inherited SaveToStream(Stream);
  with Stream do
    ;
end;

procedure TSmoothPath2D0.Assign(const Obj: TGraphicObject);
var
  I, N: Integer;
  PP: TPointsSet2D;
  P, P1: TPoint2D;
  A, SA, EA: TRealType;
begin
  if (Obj = Self) then
    Exit;
  inherited Assign(Obj);
  fIsClosed := Self is TClosedSmoothPath2D;
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
      while SA > EA do EA := EA + 2 * Pi;
      P.W := 1;
      PP := TPointsSet2D.Create(5);
      N := Round(Abs(EA - SA) / Pi * 3);
      if N < 2 then N := 2;
      if not (Obj is TArc2D) then
      begin
        if N < 3 then N := 3;
        if Obj is TSegment2D then
          P1 := MidPoint(Point2D(P.X + A * Cos(SA), P.Y + A * Sin(SA)),
            Point2D(P.X + A * Cos(EA), P.Y + A * Sin(EA)))
        else P1 := P;
        PP.Add(P1);
      end;
      for I := 0 to N do
        PP.Add(Point2D(P.X + A * Cos(SA + I * (EA - SA) / N),
          P.Y + A * Sin(SA + I * (EA - SA) / N)));
      if not (Obj is TArc2D) and not fIsClosed then PP.Add(P1);
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
        //BeginUseProfile;
        try
          (Obj as TPrimitive2D).BezierPoints(PP);
        finally
          //EndUseProfile;
        end;
        if (PP.Count > 0) and
          not (IsSamePoint2D(PP[0], PP[PP.Count - 1]) and fIsClosed)
          then fPoints.Add(PP[0]);
        for I := 0 to Pred((PP.Count - 1) div 3) do
        begin
          fPoints.AddPoints([
            BezierPoint(PP[3 * I], PP[3 * I + 1], PP[3 * I + 2],
              PP[3 * I + 3], 0.25),
              BezierPoint(PP[3 * I], PP[3 * I + 1], PP[3 * I + 2],
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
      if (Obj as TPolyline2D0).IsClosed then
      begin
        fPoints.AddPoints([MixPoint(PP[PP.Count - 1], PP[0], 0.25),
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

// =====================================================================
// TSmoothPath2D
// =====================================================================

class function TSmoothPath2D.GetName: string;
begin
  Result := 'Curve';
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

constructor TClosedSmoothPath2D.CreateSpec(ID: Integer; const Pts:
  array of TPoint2D);
begin
  inherited CreateSpec(ID, Pts);
  WhenCreated;
end;

procedure TClosedSmoothPath2D.WhenCreated;
begin
  fIsClosed := True;
end;

procedure TClosedSmoothPath2D.BezierPoints(PP: TPointsSet2D);
var
  I: Integer;
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
  else GetClosedHobbyBezier(PP, fPoints);
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
  case fHJustification of
    jhLeft: fClippingFlags := 0;
    jhCenter: fClippingFlags := 1;
    jhRight: fClippingFlags := 2;
  end;
  case fVJustification of
    jvTop: fClippingFlags := fClippingFlags + 8;
    jvCenter: fClippingFlags := fClippingFlags + 4;
    //jvBottom: fClippingFlags := fClippingFlags+0;
    jvBaseline: fClippingFlags := fClippingFlags + 24; //??
  end;
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
    WritableBox := Rect2D(0, 0, 1, 1);
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
    WritableBox := Rect2D(P.X, P.Y, P.X + 1, P.Y + 1);
    fPoints.Add(P);
  finally
    fPoints.DisableEvents := False;
    UpdateExtension(Self);
  end;
  WhenCreated;
  if (Pos('$', Txt) > 0) or (Pos('\', Txt) > 0)
    then if Length(Txt) > 1 then fTeXText := Txt;
end;

procedure TText2D.WhenCreated;
begin
  fTeXText := '';
  fRot := 0;
  fRecalcBox := False;
  fDrawBox := False;
  fExtFont := TExtendedFont.Create;
  fExtFont.FaceName := '';
  fClippingFlags := 0;
  Font := TFont.Create;
  Font.Name := ' ';
  fPoints.GrowingEnabled := False;
end;

destructor TText2D.Destroy;
begin
  fExtFont.Free;
  Font.Free;
  inherited Destroy;
end;

function TText2D.GetWideText: WideString;
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
    if (StCode <> '') and (StCode[1] = 'x') then StCode[1] := '$';
    Result := Result + WideChar(StrToInt(StCode));
    Delete(St, 1, I);
    I := Pos('&#', St);
  end;
  Result := Result + St;
end;

function TText2D.GetExtension0: TRect2D;
var
  X1, Y1, X2, Y2: TRealType;
  H, W, VShift: TRealType;
  Text_Metric: tagTEXTMETRIC;
  I: Integer;
  function GetDimension(var H, W: TRealType): Boolean;
  var
    BMP: TBitmap;
    S: TSize;
    TmpH: Integer;
    NoName: Boolean;
    AExtFont: TExtendedFont;
  begin
    Result := False;
    if not Assigned(OwnerCAD) then Exit;
    if OwnerCAD.ViewportsCount > 0 then
    begin
      fExtFont.Canvas := (Self.OwnerCAD.Viewports[0] as
        TCADViewport2D).OnScreenCanvas.Canvas;
      AExtFont := fExtFont;
      NoName := AExtFont.FaceName = '';
    end
    else
    begin
      BMP := TBitmap.Create;
      AExtFont := TExtendedFont.Create;
      AExtFont.Canvas := BMP.Canvas;
      NoName := True;
    end;
    if (Font <> nil) and (Font.Name <> ' ') then
    begin
      AExtFont.FaceName := Font.Name;
      AExtFont.Italic := Byte(fsItalic in Font.Style);
      if fsBold in Font.Style then AExtFont.Weight := FW_BOLD;
      AExtFont.Charset := Font.Charset;
      NoName := False;
    end;
    TmpH := 100;
    AExtFont.Height := -TmpH;
    if NoName then
      if Assigned(OwnerCAD)
        and ((OwnerCAD as TDrawing2D).FontName <> '')
        then AExtFont.FaceName := (OwnerCAD as TDrawing2D).FontName
      else if FontName_Default <> '' then
        AExtFont.FaceName := FontName_Default
      else AExtFont.FaceName := 'Times New Roman';
    {Windows.GetTextExtentPoint32W(Cnv.Canvas.Handle, PWideChar(fText),
      Length(fText), S);}
    GetTextMetrics(AExtFont.Canvas.Handle, Text_Metric);
    I := Text_Metric.tmInternalLeading;
    case fVJustification of
      jvTop: VShift := 1;
      jvCenter: VShift := 0.5;
      jvBottom: VShift := 0;
      jvBaseline: VShift := Text_Metric.tmDescent / TmpH;
    end;
    //tmAscent tmDescent
    Windows.GetTextExtentPoint32W(AExtFont.Canvas.Handle, PWideChar(GetWideText), Length(GetWideText), S);
    W := S.CX / TmpH * fHeight;
    //H := S.CY / TmpH * fHeight;
    H := fHeight;
    //H := H + I * 0;
    Result := True;
    if OwnerCAD.ViewportsCount = 0 then
    begin
      AExtFont.Free;
      BMP.Free;
    end
    else
      if NoName then AExtFont.FaceName := '';
  end;
begin
  if not GetDimension(H, W) then
  begin
    H := fHeight;
    W := fHeight * 2 / 3 * Length(GetWideText);
    VShift := 0;
  end;
  case fHJustification of
    jhLeft: X1 := fPoints[0].X;
    jhCenter: X1 := fPoints[0].X - W / 2;
    jhRight: X1 := fPoints[0].X - W;
  end;
  X2 := X1 + W;
  Y1 := fPoints[0].Y - VShift * H;
  Y2 := Y1 + H;
  Result := Rect2D(X1, Y1, X2, Y2);
end;

function TText2D.GetExtension: TRect2D;
begin
  Result := TransformBoundingBox2D(GetExtension0,
    RotateCenter2D(fRot, fPoints[0]));
end;

procedure TText2D._UpdateExtension;
begin
  inherited;
  WritableBox := GetExtension;
end;

function TText2D.FillProfile: TRect2D;
begin
  Result := GetExtension0;
  NewProfileItem(4);
  Profile.AddPointToLast(Point2D(Result.Left, Result.Bottom));
  Profile.AddPointToLast(Point2D(Result.Right, Result.Bottom));
  Profile.AddPointToLast(Point2D(Result.Right, Result.Top));
  Profile.AddPointToLast(Point2D(Result.Left, Result.Top));
  Profile.LastSet.Closed := True;
  Profile.LastSet.Filled := True;
  Profile.LastSet.TransformPoints(RotateCenter2D(fRot, fPoints[0]));
  Result := Profile.GetExtension;
end;

procedure TText2D.Draw(VT: TTransf2D; const Cnv:
  TDecorativeCanvas; const ClipRect2D: TRect2D; const
  DrawMode: Integer);
var
  TmpBox: TRect2D;
  TmpHeight: TRealType;
  TmpRect: TRect;
  Pnt: TPoint;
  D: TVector2D;
  TmpTransf: TTransf2D;
  NoName: Boolean;
  AlignFlags: Word;
  Text_Metric: tagTEXTMETRIC;
begin
  NoName := fExtFont.FaceName = '';
  if NoName then
    if Assigned(OwnerCAD)
      and ((OwnerCAD as TDrawing2D).FontName <> '')
      then fExtFont.FaceName := (OwnerCAD as TDrawing2D).FontName
    else if FontName_Default <> '' then
      fExtFont.FaceName := FontName_Default
    else fExtFont.FaceName := 'Times New Roman';
  Cnv.Canvas.Brush.Style := bsClear;
  { Find the correct size. }
  TmpBox.FirstEdge := Point2D(0, 0);
  TmpBox.SecondEdge := Point2D(0, fHeight);
  TmpBox := TransformRect2D(TmpBox, VT);
  TmpHeight := PointDistance2D(TmpBox.FirstEdge,
    TmpBox.SecondEdge);
  if TmpHeight <= 0 then
    Exit;
  { Build up the DrawText rect. }
  //TmpRect := Rect2DToRect(TransformRect2D(Box, VT));
  TmpRect := Rect2DToRect(TransformBoundingBox2D(Box, VT));
  fExtFont.Canvas := Cnv.Canvas;
  if fLineColor <> clDefault then
    fExtFont.Canvas.Font.Color := fLineColor
  else fExtFont.Canvas.Font.Color := clBlack;
  try
    SetGraphicsMode(Cnv.Canvas.Handle, GM_ADVANCED); //move somewhere
    //Negative Height to set height without internal leading
    fExtFont.Height := -1000;
    GetTextMetrics(Cnv.Canvas.Handle, Text_Metric);
    //SetTextMetrics(Cnv.Canvas.Handle, Text_Metric);
    fExtFont.Height := -Round(TmpHeight);
    //GetTextMetrics(Cnv.Canvas.Handle, Text_Metric);
    fExtFont.LogFont.lfEscapement := Round(Self.fRot / Pi * 1800);
    fExtFont.LogFont.lfOrientation := fExtFont.LogFont.lfEscapement;
    D.X := 0;
    case VJustification of
      jvBottom: D.Y := 0;
      jvCenter: D.Y := 0.5 * TmpHeight;
      jvTop: D.Y := -Text_Metric.tmInternalLeading * TmpHeight / 1000;
      jvBaseline: D.Y := 0;
    end;
    {case VJustification of
      jvBottom: D.Y := 0 * TmpHeight - Text_Metric.tmDescent * TmpHeight / 1000;
      jvCenter: D.Y := 0.5 * TmpHeight - Text_Metric.tmDescent * TmpHeight /
        1000;
      jvTop: D.Y := 1 * TmpHeight - Text_Metric.tmDescent * TmpHeight / 1000;
      jvBaseline: D.Y := 0;
    end;}
    if (Cnv.Canvas.Pen.Mode <> pmNot) then
    begin
      Pnt := Point2DToPoint(TransformPoint2D(fPoints[0], VT));
      if fRot <> 0 then D := TransformVector2D(D, Rotate2D(fRot));
      //D := TransformVector2D(D, Scale2D(Height, Height));
      //P := ShiftPoint(P, D);
      Pnt.X := Pnt.X - Round(D.X);
      Pnt.Y := Pnt.Y + Round(D.Y);
      AlignFlags := 0;
      case fHJustification of
        jhLeft: AlignFlags := AlignFlags + TA_LEFT;
        jhCenter: AlignFlags := AlignFlags + TA_CENTER;
        jhRight: AlignFlags := AlignFlags + TA_RIGHT;
      end;
      case fVJustification of
        jvTop: AlignFlags := AlignFlags + TA_TOP;
        jvCenter: AlignFlags := AlignFlags + TA_BOTTOM;
        jvBottom: AlignFlags := AlignFlags + TA_BOTTOM;
        jvBaseline: AlignFlags := AlignFlags + TA_BASELINE;
      end;
      //AlignFlags := AlignFlags + TA_BASELINE;
      Windows.SetTextAlign(Cnv.Canvas.Handle, AlignFlags);
      Windows.TextOutW(Cnv.Canvas.Handle, Pnt.X, Pnt.Y,
        PWideChar(GetWideText), Length(GetWideText));
      {DrawRect2DAsPolyline(Cnv, GetExtension0,
        RectToRect2D(Cnv.Canvas.ClipRect),
        RotateCenter2D(fRot, fPoints[0]), VT);}
    end;
    if fDrawBox or (Cnv.Canvas.Pen.Mode = pmNot) then
      DrawRect2DAsPolyline(Cnv, GetExtension0,
        RectToRect2D(Cnv.Canvas.ClipRect),
        RotateCenter2D(fRot, fPoints[0]), VT);
  finally
    fExtFont.Canvas := nil;
  end;
  if NoName then fExtFont.FaceName := '';
end;

constructor TText2D.CreateFromStream(const Stream: TStream;
  const Version: TCADVersion);
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
    fExtFont := TExtendedFont.Create;
    fExtFont.LoadFromStream(Stream);
    Read(fClippingFlags, SizeOf(fClippingFlags));
    Read(fDrawBox, SizeOf(fDrawBox));
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
    fExtFont.SaveToStream(Stream);
    Write(fClippingFlags, SizeOf(fClippingFlags));
    Write(fDrawBox, SizeOf(fDrawBox));
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
  if (Obj = Self) then
    Exit;
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
    fDrawBox := (Obj as TText2D).DrawBox;
    fRecalcBox := (Obj as TText2D).fRecalcBox;
    fClippingFlags := (Obj as TText2D).ClippingFlags;
    if not Assigned(fExtFont) then
      fExtFont := TExtendedFont.Create;
    fExtFont.Assign(TText2D(Obj).fExtFont);
    if not Assigned(Font) then
      Font := TFont.Create;
    Font.Assign(TText2D(Obj).Font);
    fHJustification := (Obj as TText2D).fHJustification;
    fVJustification := (Obj as TText2D).fVJustification;
  end;
  UpdateExtension(Self);
end;

procedure TText2D.Transform(const T: TTransf2D);
var
  V: TVector2D;
begin
  fPoints[0] := TransformPoint2D(fPoints[0], T);
  V := PolarVector(fHeight, fRot);
  V := TransformVector2D(V, T);
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
  T: TTransf2D;
  P0, P3, P4: TPoint2D;
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
        Pieces.Transform(Translate2D(-0.5, -0.665));
        Pieces.Transform(Scale2D(2, 2));
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
      Pieces.Transform(Translate2D(-0.5, -0.5));
      Pieces.Transform(Scale2D(2, 1.336));
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
  Pieces.Transform(Scale2D(fDiameter / 2, fDiameter / 2));
  Pieces.Transform(Rotate2D(fRot));
  Pieces.Transform(Translate2D(HShift, VShift));
end;

constructor TSymbol2D.Create(ID: Integer);
begin
  inherited CreateSpec(ID, 2, 150);
  fPoints.DisableEvents := True;
  try
    fDiameter := DefaultSymbolSize_Default;
    WritableBox := Rect2D(0, 0, 1, 1);
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
    WritableBox := Rect2D(P.X, P.Y, P.X + 1, P.Y + 1);
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
  if (Obj is TPrimitive2D) and not (Obj is TSymbol2D) then
  begin
    fPoints[0] := BoxCenter((Obj as TPrimitive2D).Box);
    SymbolKind := symDecision;
    WhenCreated;
  end;
  if (Obj is TSymbol2D) then
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

constructor TSymbol2D.CreateFromStream(const Stream: TStream;
  const Version: TCADVersion);
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

procedure TSymbol2D.Draw(VT: TTransf2D; const Cnv:
  TDecorativeCanvas; const ClipRect2D: TRect2D; const
  DrawMode: Integer);
begin
  DrawPieces(VT, Cnv, ClipRect2D, DrawMode);
end;

procedure TSymbol2D.Transform(const T: TTransf2D);
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
  fAspectRatio := 0.0;
  fCopyMode := cmSrcCopy;
  fPoints.GrowingEnabled := False;
end;

procedure TBitmap2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
    Exit;
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

constructor TBitmap2D.CreateFromStream(const Stream: TStream;
  const Version: TCADVersion);
begin
  { Load the standard properties }
  inherited;
  fBitmap := TBitmap.Create;
  fBitmap.LoadFromStream(Stream);
  if (Version >= 'CAD422') then
  begin
    Stream.Read(fScaleFactor, SizeOf(fScaleFactor));
    Stream.Read(fAspectRatio, SizeOf(fAspectRatio));
    Stream.Read(fCopyMode, SizeOf(fCopyMode));
  end;
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

procedure TBitmap2D.Draw(VT: TTransf2D; const Cnv:
  TDecorativeCanvas; const ClipRect2D: TRect2D; const
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
  OldMode := Cnv.Canvas.CopyMode;
  Cnv.Canvas.CopyMode := fCopyMode;
  Cnv.Canvas.StretchDraw(TmpRect, fBitmap);
  Cnv.Canvas.CopyMode := OldMode;
end;

// =====================================================================
// TVectFont
// =====================================================================

function TVectChar.GetVect(IDX: Integer): TPointsSet2D;
begin
  Result := TPointsSet2D(fSubVects[IDX]);
end;

function TVectChar.GetVectCount: Integer;
begin
  Result := fSubVects.NumberOfObjects;
end;

constructor TVectChar.Create(NSubVect: Integer);
var
  Cont: Integer;
begin
  inherited Create;

  fSubVects := TIndexedObjectList.Create(NSubVect);
  fSubVects.FreeOnClear := True;
  for Cont := 0 to NSubVect - 1 do
    fSubVects.Objects[Cont] := TPointsSet2D.Create(0);
end;

destructor TVectChar.Destroy;
begin
  fSubVects.Free;

  inherited;
end;

procedure TVectChar.UpdateExtension;
var
  Cont: Integer;
begin
  fExtension := Rect2D(0.0, 0.0, 0.0, 0.0);
  for Cont := 0 to fSubVects.NumberOfObjects - 1 do
    fExtension := BoxOutBox2D(fExtension,
      TPointsSet2D(fSubVects.Objects[Cont]).Extension);
end;

constructor TVectChar.CreateFromStream(const Stream: TStream;
  const Version: TCADVersion);
var
  TmpInt, Cont: Integer;
  TmpN: Longint;
  TmpPt: TPoint2D;
begin
  inherited Create;
  if (Version >= 'CAD4  ') then
    with Stream do
    begin
      Read(TmpInt, SizeOf(TmpInt));
      fSubVects := TIndexedObjectList.Create(TmpInt);
      // Lettura vettori.
      for Cont := 0 to fSubVects.NumberOfObjects - 1 do
      begin
        Read(TmpN, SizeOf(TmpN));
        fSubVects.Objects[Cont] :=
          TPointsSet2D.Create(TmpN);
        while TmpN > 0 do
        begin
          Read(TmpPt, SizeOf(TmpPt));
          TPointsSet2D(fSubVects.Objects[Cont]).Add(TmpPt);
          Dec(TmpN);
        end;
      end;
      UpdateExtension(Self);
    end;
end;

procedure TVectChar.SaveToStream(const Stream: TStream);
var
  TmpInt, Cont: Integer;
  TmpN: Longint;
  TmpPt: TPoint2D;
begin
  with Stream do
  begin
    TmpInt := fSubVects.NumberOfObjects;
    Write(TmpInt, SizeOf(TmpInt));
     // Scrittura vettori.
    for Cont := 0 to fSubVects.NumberOfObjects - 1 do
    begin
      TmpN := TPointsSet2D(fSubVects.Objects[Cont]).Count;
      Write(TmpN, SizeOf(TmpN));
      while TmpN > 0 do
      begin
        TmpPt :=
          TPointsSet2D(fSubVects.Objects[Cont]).Points[TmpN - 1];
        Write(TmpPt, SizeOf(TmpPt));
        Dec(TmpN);
      end;
    end;
  end;
end;

// =====================================================================
// TVectFont
// =====================================================================

function TVectFont.GetChar(Ch: Char): TVectChar;
begin
  Result := TVectChar(fVects[Ord(Ch)]);
end;

constructor TVectFont.Create;
begin
  inherited;

  // Al massimo ci sono 256 caratteri.
  fVects := TIndexedObjectList.Create(256);
  fVects.FreeOnClear := True;
end;

destructor TVectFont.Destroy;
begin
  fVects.Free;

  inherited;
end;

constructor TVectFont.CreateFromStream(const Stream: TStream;
  const Version: TCADVersion);
var
  TmpInt: Integer;
begin
  inherited Create;
  with Stream do
  begin
    fVects := TIndexedObjectList.Create(256);
     // Lettura caratteri.
    while True do
    begin
        // Lettura numero carattere.
      Read(TmpInt, SizeOf(TmpInt));
      if TmpInt = -1 then
        Break; // Fine.
      fVects[TmpInt] := TVectChar.CreateFromStream(Stream,
        Version);
    end;
  end;
end;

procedure TVectFont.SaveToStream(const Stream: TStream);
var
  TmpInt: Integer;
begin
  with Stream do
  begin
     // Scrittura caratteri.
    for TmpInt := 0 to fVects.NumberOfObjects - 1 do
      if fVects[TmpInt] <> nil then
      begin
         // Scrittura numero carattere.
        Write(TmpInt, SizeOf(TmpInt));
        TVectChar(fVects[TmpInt]).SaveToStream(Stream);
      end;
     // Fine
    TmpInt := -1;
    Write(TmpInt, SizeOf(TmpInt));
  end;
end;

function TVectFont.CreateChar(Ch: Char; N: Integer):
  TVectChar;
begin
  if fVects[Ord(Ch)] <> nil then
    fVects[Ord(Ch)].Free;
  Result := TVectChar.Create(N);
  fVects[Ord(Ch)] := Result;
end;

procedure TVectFont.DrawChar2D(Ch: Char; var DrawPoint:
  TPoint2D; const H, ICS: TRealType; const VT: TTransf2D; Cnv:
  TDecorativeCanvas);
var
  Cont: Integer;
  TmpTransf: TTransf2D;
  TmpExt: TRect2D;
  TmpCh: TVectChar;
begin
  TmpCh := nil;
  if (Ch = ' ') then
  begin
    DrawPoint.X := DrawPoint.X + (0.4 + ICS) * H;
    Exit;
  end
  else if fVects[Ord(Ch)] <> nil then
    TmpCh := GetChar(Ch)
  else if (Ch <> #13) then
    TmpCh := _NullChar;
  if TmpCh <> nil then
  begin
    TmpTransf := IdentityTransf2D;
    TmpTransf[1, 1] := H;
    TmpTransf[2, 2] := H;
    TmpTransf[3, 1] := DrawPoint.X;
    TmpTransf[3, 2] := DrawPoint.Y;
    TmpTransf[3, 3] := DrawPoint.W;
    with TmpCh do
    begin
      TmpExt := Extension;
      for Cont := 0 to VectorCount - 1 do
        DrawAsPolyline(Vectors[Cont], Cnv,
          RectToRect2D(Cnv.Canvas.ClipRect), TmpExt,
          MultiplyTransform2D(TmpTransf, VT));
      DrawPoint.X := DrawPoint.X + (TmpExt.Right + ICS) * H;
    end;
  end;
end;

function TVectFont.GetTextExtension(Str: AnsiString; H,
  InterChar, InterLine: TRealType): TRect2D;
var
  Cont: Integer;
  RowLen, RowHeight, MaxRowLen: TRealType;
begin
  // Per i caratteri con la gambetta (come g) parto con Bottom = 1.0
  Result := Rect2D(0.0, 1.0, 0.0, H);
  MaxRowLen := 0.0;
  RowLen := 0.0;
  RowHeight := 0.0;
  for Cont := 1 to Length(Str) do
  begin
    if fVects[Ord(Str[Cont])] <> nil then
      with GetChar(Str[Cont]).Extension do
      begin
        RowLen := RowLen + (Right + InterChar);
         // Bottom contiene l'eventuale gambetta.
        RowHeight := MaxValue([RowHeight, 1.0 - Bottom]);
      end
    else if Str[Cont] = ' ' then
      // Space.
      RowLen := RowLen + (0.5 + InterChar)
    else if Str[Cont] <> #13 then
      // Carattere nullo.
      with _NullChar.Extension do
        RowLen := RowLen + (Right + InterChar);
    if Str[Cont] = #13 then
    begin
        // New line. L'altezza è 1.3 per via delle gambette.
      MaxRowLen := MaxValue([MaxRowLen, RowLen - InterChar]);
      Result.Bottom := Result.Bottom - (InterLine +
        RowHeight);
      RowLen := 0.0;
      RowHeight := 0.0;
    end;
  end;
  MaxRowLen := MaxValue([MaxRowLen, RowLen - InterChar]);
  Result.Left := 0.0;
  Result.Bottom := (Result.Bottom - RowHeight) * H;
  Result.Right := MaxRowLen * H;
end;

// =====================================================================
// Registration functions
// =====================================================================

function CADSysFindFontIndex(const Font: TVectFont): Word;
var
  Cont: Integer;
begin
  for Cont := 0 to MAX_REGISTERED_FONTS do
    if Assigned(VectFonts2DRegistered[Cont]) and
      (VectFonts2DRegistered[Cont] = Font) then
    begin
      Result := Cont;
      Exit;
    end;
  raise
    ECADObjClassNotFound.Create('CADSysFindFontIndex: Font not found');
end;

function CADSysFindFontByIndex(Index: Word): TVectFont;
begin
  if not Assigned(VectFonts2DRegistered[Index]) then
  begin
    if Assigned(_DefaultFont) then
      Result := _DefaultFont
    else
      raise
        ECADObjClassNotFound.Create('CADSysFindFontByIndex: Font not registered');
  end
  else
    Result := VectFonts2DRegistered[Index];
end;

procedure CADSysRegisterFont(Index: Word; const Font:
  TVectFont);
begin
  if Index > MAX_REGISTERED_FONTS then
    raise
      Exception.Create('CADSysRegisterFont: Out of bound registration index');
  if Assigned(VectFonts2DRegistered[Index]) then
    raise
      ECADObjClassNotFound.Create('CADSysRegisterFont: Font index already allocated');
  VectFonts2DRegistered[Index] := Font;
end;

procedure CADSysUnregisterFont(Index: Word);
begin
  if Index > MAX_REGISTERED_FONTS then
    raise
      Exception.Create('CADSysUnregisterFont: Out of bound registration index');
  if Assigned(VectFonts2DRegistered[Index]) then
  begin
    VectFonts2DRegistered[Index].Free;
    VectFonts2DRegistered[Index] := nil;
  end;
end;

procedure CADSysRegisterFontFromFile(Index: Word; const
  FileName: string);
var
  TmpStream: TFileStream;
begin
  if Index > MAX_REGISTERED_FONTS then
    raise
      Exception.Create('CADSysRegisterFontFromFile: Out of bound registration index');
  if Assigned(VectFonts2DRegistered[Index]) then
    raise
      ECADObjClassNotFound.Create('CADSysRegisterFontFromFile: Font index already allocated');
  TmpStream := TFileStream.Create(FileName, fmOpenRead);
  try
    VectFonts2DRegistered[Index] :=
      TVectFont.CreateFromStream(TmpStream, CADSysVersion);
  finally
    TmpStream.Free;
  end;
end;

procedure CADSysInitFontList;
var
  Cont: Word;
begin
  for Cont := 0 to MAX_REGISTERED_FONTS do
    VectFonts2DRegistered[Cont] := nil;
end;

procedure CADSysClearFontList;
var
  Cont: Word;
begin
  for Cont := 0 to MAX_REGISTERED_FONTS do
    if Assigned(VectFonts2DRegistered[Cont]) then
      VectFonts2DRegistered[Cont].Free;
end;

function CADSysGetDefaultFont: TVectFont;
begin
  Result := _DefaultFont;
end;

procedure CADSysSetDefaultFont(const Font: TVectFont);
begin
  _DefaultFont := Font;
end;

// =====================================================================
// TJustifiedVectText2D
// =====================================================================

class function TJustifiedVectText2D.GetName: string;
begin
  Result := 'JustifiedVectText';
end;

procedure TJustifiedVectText2D.SetHeight(H: TRealType);
begin
  fHeight := H;
  UpdateExtension(Self);
end;

procedure TJustifiedVectText2D.SetCharSpace(S: TRealType);
begin
  fCharSpace := S;
  UpdateExtension(Self);
end;

procedure TJustifiedVectText2D.SetInterLine(S: TRealType);
begin
  fInterLine := S;
  UpdateExtension(Self);
end;

procedure TJustifiedVectText2D.SetText(T: string);
begin
  fText := T;
  UpdateExtension(Self);
end;

function TJustifiedVectText2D.GetTextExtension: TRect2D;
var
  TmpRect: TRect2D;
  CurrRect: TRect2D;
  DX, TX, TY: TRealType;
begin
  if Assigned(fVectFont) then
    TmpRect := fVectFont.GetTextExtension(fText, fHeight,
      fCharSpace, fInterLine)
  else
    TmpRect := Rect2D(0, 0, 0, 0);
  CurrRect.FirstEdge := fPoints[0];
  CurrRect.SecondEdge := fPoints[1];
  CurrRect := ReorderRect2D(CurrRect);
  fBasePoint := Point2D(CurrRect.Left, CurrRect.Top -
    TmpRect.Top);
  DX := TmpRect.Right - TmpRect.Left;
  case fHJustification of
    jhLeft: TX := CurrRect.Left;
    jhRight: TX := CurrRect.Right - DX;
    jhCenter: TX := (CurrRect.Left + CurrRect.Right - DX) /
      2.0;
  else
    TX := CurrRect.Left;
  end;
  case fVJustification of
    jvTop: TY := CurrRect.Top - TmpRect.Top;
    jvBottom: TY := CurrRect.Bottom - TmpRect.Bottom;
    jvCenter: TY := (CurrRect.Top + CurrRect.Bottom) / 2.0;
  else
    TY := CurrRect.Top - TmpRect.Top;
  end;
  Result := TransformRect2D(TmpRect, Translate2D(TX, TY));
end;

procedure TJustifiedVectText2D.DrawText(const VT: TTransf2D;
  const Cnv: TDecorativeCanvas; const DrawMode: Integer);
  procedure DrawTextLine(BasePt: TPoint2D; Str: string;
    ObjTransf: TTransf2D);
  var
    Cont: Integer;
  begin
    for Cont := 1 to Length(Str) do
     // Disegno il carattere.
      fVectFont.DrawChar2D(Str[Cont], BasePt, fHeight,
        fCharSpace, ObjTransf, Cnv);
  end;
var
  TmpTransf: TTransf2D;
  TmpStr, TmpRow: string;
  CurrBasePt: TPoint2D;
  TmpPos: Integer;
  TmpText: TRect2D;
begin
  if not Assigned(fVectFont) then
    Exit;
  // sposto il testo, applico la trasformazione oggetto al testo e trasformo nel viewport.
  TmpTransf := VT;
  try
    TmpText := GetTextExtension;
    if fDrawBox then
      DrawRect2DAsPolyline(Cnv, TmpText,
        RectToRect2D(Cnv.Canvas.ClipRect), IdentityTransf2D,
        VT);
    if DrawMode and DRAWMODE_VECTTEXTONLYBOX =
      DRAWMODE_VECTTEXTONLYBOX then
      Exit;
    CurrBasePt.X := TmpText.Left;
    CurrBasePt.Y := TmpText.Top - fHeight;
    CurrBasePt.W := 1.0;
    // Estraggo le righe.
    TmpStr := fText;
    TmpPos := Pos(#13, TmpStr);
    while TmpPos > 0 do
    begin
      TmpRow := Copy(TmpStr, 1, TmpPos - 1);
      Delete(TmpStr, 1, TmpPos);
      if TmpStr[1] = #10 then
        Delete(TmpStr, 1, 1);
      TmpPos := Pos(#13, TmpStr);
       // Draw the string.
      TmpText := fVectFont.GetTextExtension(TmpRow, fHeight,
        fCharSpace, fInterLine);
      DrawTextLine(CurrBasePt, TmpRow, TmpTransf);
      CurrBasePt.Y := CurrBasePt.Y - (TmpText.Top -
        TmpText.Bottom) - fHeight * fInterLine;
    end;
    // Draw the string.
    DrawTextLine(CurrBasePt, TmpStr, TmpTransf);
  finally
  end;
end;

constructor TJustifiedVectText2D.Create(ID: Integer);
begin
  inherited CreateSpec(ID, 2, 150);
  fPoints.DisableEvents := True;
  try
    fPoints.Add(Point2D(0, 0));
    fPoints.Add(Point2D(0, 0));
  finally
    fPoints.DisableEvents := False;
  end;

  fHeight := DefaultFontHeight_Default;
  fText := '';
  fVectFont := nil;
  WhenCreated;

  UpdateExtension(Self);
end;

constructor TJustifiedVectText2D.CreateSpec(ID: Integer; FontVect:
  TVectFont; TextBox: TRect2D; Height: TRealType; Txt:
  AnsiString);
begin
  inherited CreateSpec(ID, 2, 150);
  fPoints.DisableEvents := True;
  try
    fPoints.Add(TextBox.FirstEdge);
    fPoints.Add(TextBox.SecondEdge);
  finally
    fPoints.DisableEvents := False;
  end;

  fHeight := Height;
  fText := Txt;
  fVectFont := FontVect;
  WhenCreated;

  UpdateExtension(Self);
end;

procedure TJustifiedVectText2D.WhenCreated;
begin
  fCharSpace := 0.1;
  fInterLine := 0.02;
  fDrawBox := False;
  fHJustification := jhLeft;
  fVJustification := jvTop;
end;

constructor TJustifiedVectText2D.CreateFromStream(const
  Stream:
  TStream; const Version: TCADVersion);
var
  TmpInt: Integer;
begin
  { Load the standard properties }
  inherited;
  with Stream do
  begin
    Read(TmpInt, SizeOf(TmpInt));
    SetString(fText, nil, TmpInt);
    Read(Pointer(fText)^, TmpInt);
     // Lettura indice font.
    Read(TmpInt, SizeOf(TmpInt));
    try
      fVectFont := CADSysFindFontByIndex(TmpInt);
    except
      on ECADObjClassNotFound do
      begin
        ShowMessage('Font class not found. Font not assigned');
        fVectFont := nil;
      end;
    end;
    Read(fHJustification, SizeOf(fHJustification));
    Read(fVJustification, SizeOf(fVJustification));
    Read(fDrawBox, SizeOf(fDrawBox));
    Read(fHeight, SizeOf(fHeight));
    Read(fInterLine, SizeOf(fInterLine));
    Read(fCharSpace, SizeOf(fCharSpace));
  end;
end;

procedure TJustifiedVectText2D.Assign(const Obj:
  TGraphicObject);
begin
  if (Obj = Self) then
    Exit;
  inherited Assign(Obj);
  if (Obj is TJustifiedVectText2D) or (Obj is TText2D) then
  begin
    if not Assigned(fPoints) then
    begin
      fPoints := CreateVect(2);
      fPoints.GrowingEnabled := False;
      fPoints.OnChange := UpdateExtension;
    end;
    if Obj is TJustifiedVectText2D then
    begin
      fText := (Obj as TJustifiedVectText2D).Text;
      fHeight := (Obj as TJustifiedVectText2D).Height;
      fInterLine := (Obj as TJustifiedVectText2D).InterLine;
      fCharSpace := (Obj as TJustifiedVectText2D).CharSpace;
      fDrawBox := (Obj as TJustifiedVectText2D).DrawBox;
      fVectFont := (Obj as TJustifiedVectText2D).fVectFont;
      fHJustification := (Obj as
        TJustifiedVectText2D).fHJustification;
      fVJustification := (Obj as
        TJustifiedVectText2D).fVJustification;
    end
    else if Obj is TText2D then
    begin
      fText := (Obj as TText2D).Text;
      fHeight := (Obj as TText2D).Height;
      fDrawBox := (Obj as TText2D).DrawBox;
    end;
    fPoints.Clear;
    fPoints.Copy((Obj as TPrimitive2D).fPoints, 0, 1);
  end;
end;

procedure TJustifiedVectText2D.SaveToStream(const Stream:
  TStream);
var
  TmpInt: Integer;
begin
  { Save the standard properties }
  inherited;
  with Stream do
  begin
    TmpInt := Length(fText);
    Write(TmpInt, SizeOf(TmpInt));
    Write(Pointer(fText)^, TmpInt);
     // Scrittura indice font.
    TmpInt := CADSysFindFontIndex(fVectFont);
    Write(TmpInt, SizeOf(TmpInt));
    Write(fHJustification, SizeOf(fHJustification));
    Write(fVJustification, SizeOf(fVJustification));
    Write(fDrawBox, SizeOf(fDrawBox));
    Write(fHeight, SizeOf(fHeight));
    Write(fInterLine, SizeOf(fInterLine));
    Write(fCharSpace, SizeOf(fCharSpace));
  end;
end;

procedure TJustifiedVectText2D.Draw(VT: TTransf2D; const
  Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const
  DrawMode: Integer);
begin
  DrawText(VT, Cnv, DrawMode);
end;

function TJustifiedVectText2D.OnMe(PT: TPoint2D; Aperture:
  TRealType; var Distance: TRealType): Integer;
var
  TmpVect: TPointsSet2D;
  TmpDist: TRealType;
  TmpBox: TRect2D;
begin
  Result := inherited OnMe(PT, Aperture, Distance);
  if (Result = PICK_INBBOX) then
  begin
    TmpBox := GetTextExtension;
    TmpVect := TPointsSet2D.Create(4);
    try
      TmpVect.Add(TmpBox.FirstEdge);
      TmpVect.Add(Point2D(TmpBox.Left, TmpBox.Top));
      TmpVect.Add(TmpBox.SecondEdge);
      TmpVect.Add(Point2D(TmpBox.Right, TmpBox.Bottom));
      Result := MaxIntValue([PICK_INBBOX,
        IsPointInPolygon2D(TmpVect.PointsReference,
          TmpVect.Count,
          PT, TmpDist, Aperture, IdentityTransf2D)]);
      Distance := MinValue([Aperture, TmpDist]);
    finally
      TmpVect.Free;
    end;
  end;
end;

procedure TJustifiedVectText2D._UpdateExtension;
begin
  inherited;
  WritableBox := GetTextExtension;
end;

initialization

  _DefaultHandler2D := TPrimitive2DHandler.Create(nil);

  // Vectorial fonts
  CADSysInitFontList;

  _NullChar := TVectChar.Create(1);
  _NullChar.Vectors[0].Add(Point2D(0.0, 0.0));
  _NullChar.Vectors[0].Add(Point2D(0.8, 0.0));
  _NullChar.UpdateExtension(nil);
  _DefaultFont := nil;
finalization
  CADSysClearFontList;
  _NullChar.Free;
  _DefaultHandler2D.Free;
end.

