{: This help file explain all the entities classes defined in
   the CADSys 4.0 library for both the 2D and 3D use.

   These classes are defined in the CS4Shapes unit file
   that you must include in the <B=uses> clause in all of your units
   that access the types mentioned here.
}
unit CS4Shapes;

interface

uses SysUtils, Classes, Windows, Graphics,
  CADSys4, CS4BaseTypes;

type
  {: This type defines the type used to specify the name of
     a <I=font type face> (like Times New Roman).
  }
  TFaceName = string[LF_FACESIZE];
  {: This class encapsulates the interface for the GDI font of
     Windows as defined by <Code=TLOGFONT> structure.

     This font is used by the library for the <See Class=TText2D>
     shape class. The use of it is somewhat difficult in the
     context of a 2D or 3D drawing so it is better to use
     the vectorial text shape <See Class=TJustifiedVectText2D> and
     <See Class=TJustifiedVectText3D>.
  }
  TExtendedFont = class(TObject)
  private
    LogFont: TLOGFONT;
    FHandle: HFONT;
    fCanvas: TCanvas;
    procedure SetNewValue;
    procedure SetCanvas(Cnv: TCanvas);
    procedure SetHeight(Value: Word);
    function GetHeight: Word;
    procedure SetWidth(Value: Word);
    function GetWidth: Word;
    procedure SetEscapement(Value: Word);
    function GetEscapement: Word;
    procedure SetWeight(Value: Word);
    function GetWeight: Word;
    procedure SetItalic(Value: Byte);
    function GetItalic: Byte;
    procedure SetUnderline(Value: Byte);
    function GetUnderline: Byte;
    procedure SetStrikeOut(Value: Byte);
    function GetStrikeOut: Byte;
    procedure SetCharSet(Value: Byte);
    function GetCharSet: Byte;
    procedure SetOutPrecision(Value: Byte);
    function GetOutPrecision: Byte;
    procedure SetClipPrecision(Value: Byte);
    function GetClipPrecision: Byte;
    procedure SetQuality(Value: Byte);
    function GetQuality: Byte;
    procedure SetPicthAndFamily(Value: Byte);
    function GetPicthAndFamily: Byte;
    procedure SetFaceName(Value: TFaceName);
    function GetFaceName: TFaceName;
  public
    {: This is the constructor that creates an instance of a
       font.

       When a new font is created it is set using the
       <I=DEFAULT_GUI_FONT> as defined in Windows specifications.
    }
    constructor Create;
    {: This destructor frees the font informations.

       It also detaches the font form a <I=Canvas>, if the font is
       currently in use by it.
    }
    destructor Destroy; override;
    {: This method assign the font data by using another font
       class as a prototype.

       Parameters:

       <LI=<I=Obj> is the font being used as a prototype.>
    }
    procedure Assign(Obj: TExtendedFont);
    {: This method saves the font informations into a stream.

       Parameters:

       <LI=<I=Strm> is the stream on which save the font structure.>
    }
    procedure SaveToStream(Strm: TStream);
    {: This method retrieves the font informations from a stream.

       Parameters:

       <LI=<I=Strm> is the stream from which retrieve the font
        structure.>
    }
    procedure LoadFromStream(Strm: TStream);
    {: This property attaches the font to a Canvas.

       If you want to use the font on a Canvas you must use this
       property. After you have setted this propery, to detach
       the font from the Canvas assign <B=nil> to this property.
    }
    property Canvas: TCanvas read fCanvas write SetCanvas;
    {: This property contains the handle for the
       <Code=TLOGFONT> structure.
    }
    property Handle: HFONT read FHandle;
    {: This property specifies the <I=lfHeight> field of <Code=TLOGFONT>.
    }
    property Height: Word read GetHeight write SetHeight;
    {: This property specifies the <I=lfWidth> field of <Code=TLOGFONT>.
    }
    property Width: Word read GetWidth write SetWidth;
    {: This property specifies the <I=lfEscapement> field of
       <Code=TLOGFONT>.
    }
    property Escapement: Word read GetEscapement write
      SetEscapement;
    {: This property specifies the <I=lfWeight> field of
       <Code=TLOGFONT>.
    }
    property Weight: Word read GetWeight write SetWeight;
    {: This property specifies the <I=lfItalic> field of
       <Code=TLOGFONT>.
    }
    property Italic: Byte read GetItalic write SetItalic;
    {: This property specifies the <I=lfUnderline> field of
       <Code=TLOGFONT>.
    }
    property Underline: Byte read GetUnderline write
      SetUnderline;
    {: This property specifies the <I=lfStrikeOut> field of
       <Code=TLOGFONT>.
    }
    property StrikeOut: Byte read GetStrikeOut write
      SetStrikeOut;
    {: This property specifies the <I=lfCharSet> field of
       <Code=TLOGFONT>.
    }
    property Charset: Byte read GetCharSet write SetCharSet;
    {: This property specifies the <I=lfOutPrecision> field of
       <Code=TLOGFONT>.
    }
    property OutPrecision: Byte read GetOutPrecision write
      SetOutPrecision;
    {: This property specifies the <I=lfClipPrecision> field of
       <Code=TLOGFONT>.
    }
    property ClipPrecision: Byte read GetClipPrecision write
      SetClipPrecision;
    {: This property specifies the <I=lfQuality> field of
       <Code=TLOGFONT>.
    }
    property Quality: Byte read GetQuality write SetQuality;
    {: This property specifies the <I=lfPitchAndFamily> field of
       <Code=TLOGFONT>.
    }
    property PicthAndFamily: Byte read GetPicthAndFamily write
      SetPicthAndFamily;
    {: This property specify the <I=lfFaceName> field of
       <Code=TLOGFONT>.
    }
    property FaceName: TFaceName read GetFaceName write
      SetFaceName;
  end;

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

  {: This is the class reference type for the
     <See Class=TPrimitive2D> shape class.
  }
  TPrimitive2DClass = class of TPrimitive2D;

  {: This handler can be used to modify a primitive by dragging its
     control points.

     See also <See Class=TObject2DHandler>.
  }
  TPrimitive2DHandler = class(TObject2DHandler)
  public
    procedure DrawControlPoints(const Sender: TObject2D; const
      VT: TTransf2D; const Cnv: TDecorativeCanvas; const Width:
      Integer); override;
    function OnMe(const Sender: TObject2D; PT: TPoint2D;
      Aperture: TRealType; var Distance: TRealType): Integer;
      override;
  end;

  TLineKind = (liNone, liThick, liThin, liDotted, liDashed);

  THatching = (haNone, haHorizontal, haVertical,
    haFDiagonal, haBDiagonal, haCross, haDiagCross);

  {: This class defines a <I=2D primitive>.

     A primitive shape is an entity which shape is controlled
     by a set of <I=control points>. This class stores and
     handles this set of point allowing the developer to
     focus on the way to draw the shape from these control
     points.

     This is the right class from which derive your own
     entity classes.

     See also <See Class=TOutline2D> and <See Class=TCurve2D>.

     <B=Warning>: This is an abstract class that cannot be used
     directly.

     <B=Note>: The control points are always in the object model
     coordinate system !
  }

  TPrimitive2D = class;

  TPieceLine = (pliDefault, pliNone, pliSolidDefault, pliFillAsDefault,
    pliFill);
  TPieceFill = (pfiDefault, pfiNone, pfiLineAsDefault, pfiLine);
  TPieceHatch = (phaDefault, phaNone);

  // TSY: New class for future. Needed for drawing compound shapes
  TPiece = class(TPointsSet2D)
    Line: TPieceLine;
    Fill: TPieceFill;
    Hatch: TPieceHatch;
    constructor Create(const _Capacity: Word); override;
    function GetLineKind(Obj: TPrimitive2D): TLineKind;
    function GetLineColor(Obj: TPrimitive2D): TColor;
    function GetHatching(Obj: TPrimitive2D): THatching;
    function GetFillColor(Obj: TPrimitive2D): TColor;
  end;

  // polyline/polygon
  TPath = class(TPiece)
  end;

  // polybezier
  TBezierPath = class(TPiece)
  end;

  TArrayOfPiece = class
  private
    Pieces: array of TPiece;
    function GetItem(I: Integer): TPiece;
  public
    Count: Integer;
    constructor Create(Capacity: Integer);
    destructor Destroy; override;
    procedure Clear;
    procedure Add(Piece: TPiece);
    procedure GetExtension0(var R: TRect2D; const FirstPass: Boolean);
    function GetExtension: TRect2D;
    property Item[I: Integer]: TPiece read GetItem; default;
  end;

  TDrawPathProc = procedure(const Canvas: TCanvas;
    const IsClosed: Boolean; const Pnts: array of TPoint) of object;

  TPrimitive2D = class(TObject2D)
  private
    fPoints: TPointsSet2D;
    fHatching: THatching;
    fLineKind: TLineKind;
    fLineColor, fHatchColor, fFillColor: TColor;
    fCanDeletePoints: Boolean;
    fDrawPathBezier: Boolean;
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
    function CreateVect(const Size: Integer): TPointsSet2D;
      dynamic;
  public
    Pieces: TArrayOfPiece;
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
    constructor Create(ID: Longint; NPts: Integer);
    procedure WhenCreated;
    destructor Destroy; override;
    constructor CreateFromStream(const Stream: TStream; const
      Version: TCADVersion); override;
    procedure Assign(const Obj: TGraphicObject); override;
    procedure SaveToStream(const Stream: TStream); override;
    {: This property contains the set of <I=control points> used
       to define the shape of the entity.

       See the introduction of <See Class=TPrimitive2D> for details.
    }
    //TSY:
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
    procedure TransForm(const T: TTransf2D); override;
    procedure DeleteControlPoint0(const I: Integer);
    procedure DeleteControlPoint(const I: Integer); virtual;
    //Add control point at Pos
    procedure InsertControlPoint0(const Pos: Integer; P:
      TPoint2D);
    procedure InsertControlPoint(const Pos: Integer; P:
      TPoint2D);
      virtual;
    property Points: TPointsSet2D read fPoints write fPoints;
    property Hatching: THatching read fHatching write fHatching;
    property LineKind: TLineKind read fLineKind
      write fLineKind;
    property LineColor: TColor read fLineColor
      write fLineColor;
    property HatchColor: TColor read fHatchColor
      write fHatchColor;
    property FillColor: TColor read fFillColor
      write fFillColor;
    property Name: string read GetName;
  end;

//TSY:
  TArrowData = record
    W, L1, L2: TRealType;
  end;

  TArrowKind = (arrNone, arrL0, arrL1, arrL2, arrL3, arrL4,
    arrL5, arrL6, arrL7, arrL8);

  {: This class defines a 2D line segment.

     The entity has two <I=control points> that are the extremes of
     the segment.
  }
  TLine2D = class(TPrimitive2D)
  private
  public
  //TSY:
    BeginArrowKind: TArrowKind;
    EndArrowKind: TArrowKind;
    class function GetName: string; override;
    {: This constructor creates a new 2D line segment.

       Parameters:

       <LI=<I=ID> is the object identifier.>
       <LI=<I=P1> is the starting point of the segment.>
       <LI=<I=P2> is the ending point of the segment.>
    }
    constructor Create(ID: Longint; const P1, P2: TPoint2D);
    procedure WhenCreated;
    procedure Assign(const Obj: TGraphicObject); override;
    constructor CreateFromStream(const Stream: TStream; const
      Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;
    function FillPieces: TRect2D;
    procedure Draw(VT: TTransf2D; const Cnv:
      TDecorativeCanvas; const ClipRect2D: TRect2D; const
      DrawMode: Integer); override;
    function OnMe(PT: TPoint2D; Aperture: TRealType; var
      Distance: TRealType): Integer; override;
  protected
    procedure _UpdateExtension; override;
  end;

  {: This class defines a <I=2D outline>.

     An <I=outline> primitive shape is an entity that is drawed as
     a connected set of points (<I=profile points>) which displacement
     is controlled by a set of <I=control points>. The points used
     to draw the entity may be keept in memory or recomputed from
     the control points whenever they are needed to draw the
     shape. In the former a lot of memory may be used but the
     drawing is faster, in the latter the drawing of the shape
     may take a quite longer time but the memory is used
     efficently.

     For an <I=outline> the control points are the same as
     the <I=profile points>.

     Before using the <I=profile points> you must call the
     <See Method=TOutline2D@BeginUseProfilePoints> and after the
     using you must call the
     <See Method=TOutline2D@EndUseProfilePoints> method.

     See also <See Class=TCurve2D>.

     <B=Note>: The control points are always in the object model
     coordinate system !
  }
  TOutline2D = class(TPrimitive2D)
  private
    fIsClosed: Boolean;
    fOwnsInterior: Boolean;
  protected
    {: This method is called when the <I=profile points>
       are needed.

       It must returns the set of the <I=profile points>
       created by the class.

       <B=Warning>: You don't have to delete the set of points
       returned by the method.
    }
    function GetProfilePoints: TPointsSet2D; virtual; abstract;
    {: This method returns the number of <I=profile points>
       that is equal (for an <I=outline>) to the number
       of <I=control points>.
    }
    function GetNPts: Integer; virtual; abstract;
    {: This method returns <B=True> if the set of <I=profile points>
       is closed.

       The fact that the set is closed influences the way in which
       it is drawed and picked.
    }
  public
    constructor Create(ID: Longint; NPts: Integer);
    procedure WhenCreated;
    {: This method initializes the profile points vector for using.

       You must call this method before any use of the methods that
       work on the set of <I=profile points>.

       It is better to call this method in a <Code=try-finally>
       block with <See Method=TOutline2D@EndUseProfilePoints>
       in the finally part.
    }
    procedure Assign(const Obj: TGraphicObject); override;
    constructor CreateFromStream(const Stream: TStream; const
      Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure BeginUseProfilePoints; dynamic;
    {: This method finalizes the <I=profile points> when you
       finish to use them.

       You must call this method when you no longer need to use
       the set of <I=profile points>. This allow the library
       to eventually saves the memory used by the entity.

       It is better to call this method in a <Code=try-finally>
       block with this method in the finally part.

       <B=Note>: This method must be called after the
       <See Method=TOutline2D@BeginUseProfilePoints>.
    }
    procedure EndUseProfilePoints; dynamic;
    procedure Draw(VT: TTransf2D; const Cnv:
      TDecorativeCanvas; const ClipRect2D: TRect2D; const
      DrawMode: Integer); override;
    function OnMe(PT: TPoint2D; Aperture: TRealType; var
      Distance: TRealType): Integer; override;
    function OnProfile(PT: TPoint2D; Aperture: TRealType):
      Integer; virtual;
    {: This is the set of <I=profile points> that is used to
       draw the entity.

       See the class description of <See Class=TOutline2D>.
    }
    property ProfilePoints: TPointsSet2D read GetProfilePoints;
    {: This property contains the size of the set of
       <I=profile points> that is used to draw the entity.

       See the class description of <See Class=TOutline2D>.
    }
    property NumberOfProfilePts: Integer read GetNPts;
    {: This property is <B=True> when the shape of the
       entity must be considere as closed.
    }
    property IsClosed: Boolean read fIsClosed;
    property OwnsInterior: Boolean read fOwnsInterior;
  end;

  {: This class defines a 2D polyline.

     A polyline is obtained by connecting the <I=profile points> (in
     this case are the same as the <I=control points>) with straight
     line segments.
  }
  TPolyline2D0 = class(TOutline2D)
  protected
    function GetProfilePoints: TPointsSet2D; override;
    function GetNPts: Integer; override;
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
    constructor Create(ID: Longint; const Pts: array of
      TPoint2D);
    procedure WhenCreated;
    procedure Assign(const Obj: TGraphicObject); override;
    procedure Draw(VT: TTransf2D; const Cnv:
      TDecorativeCanvas; const ClipRect2D: TRect2D; const
      DrawMode: Integer); override;
  end;

  TPolyline2D = class(TPolyline2D0)
  private
  public
    class function GetName: string; override;
  end;

  {: This class defines a 2D polygon.

     A polygon is obtained by connecting the <I=profile points> (
     in this case they are the same as the <I=profile points>)
     with straight segments and filling the shape with the current
     brush of the Canvas.
  }
  TPolygon2D = class(TPolyline2D)
  private
  public
    class function GetName: string; override;
    constructor Create(ID: Longint; const Pts: array of
      TPoint2D);
    procedure WhenCreated;
    procedure Assign(const Obj: TGraphicObject); override;
  end;

  {: This class defines a 2D curve.

     A <I=curve> primitive shape is an entity that is drawed as
     a connected set of points (<I=profile points>) which displacement
     is controlled by a set of <I=control points>. The points used
     to draw the entity may be keept in memory or recomputed from
     the control points whenever they are needed to draw the
     shape. In the former a lot of memory may be used but the
     drawing is faster, in the latter the drawing of the shape
     may take a quite longer time but the memory is used
     efficently.

     A curve is a 2D polyline in which the points that define
     the shape (<I=profile points>)are not the same as the
     <I=control points> of the entity. In this case the control
     points only control the shape of the curve.

     For this class the <See property=TCurve2D@SavingType> property
     defines the mode of saving used by the entity.

     Before using the <I=profile points> you must call the
     <See Method=TOutline2D@BeginUseProfilePoints> and after the
     using you must call the
     <See Method=TOutline2D@EndUseProfilePoints> method.

     You have to put the code that defines the <I=profile points>
     from the <I=control points> in the
     <See Method=TCurve2D@PopulateCurvePoints> method.

     See also <See Class=TOutline2D>.

     <B=Note>: The control points and the profile points are
     always in the object model coordinate system !
  }
  TCurve2D = class(TOutline2D)
  private
    fSavingType: TPrimitiveSavingType;
    fCurvePrecision: Word;
    fCurvePoints: TPointsSet2D;
    fCountReference: Integer;

    procedure SetCurvePrecision(N: Word);
    procedure SetPrimitiveSavingType(S: TPrimitiveSavingType);
    procedure FreeCurvePoints;
  protected
    procedure _UpdateExtension; override;
    {: This method is called whenever the <I=profile points>
       must be computed.

       You must redefine this method to fill the set of
       <I=control points> by using the actual set of
       <I=control points>. In defining this method you have
       to call the inherited method passing it the
       right number of <I=profile points> used by the
       entity as the <I=N> parameter.

       In the method you may use the
       <See Property=TOutline2D@ProfilePoints> to add the
       points to the set of <I=profile points>.

       <B=Warning>: Don't call
       <See Method=TOutline2D@BeginUseProfilePoints> nor
       <See Method=TOutline2D@EndUseProfilePoints> in this method.
       Also don't access the <See Property=TOutline2D@NumberOfProfilePts>
       property but use the number of points that you have
       computed.
    }
    function PopulateCurvePoints(N: Word): TRect2D; dynamic;
    function GetProfilePoints: TPointsSet2D; override;
    function GetNPts: Integer; override;
  public
    constructor Create(ID: Longint; NPts: Integer; CurvePrec:
      Word);
    procedure WhenCreated;
    destructor Destroy; override;
    procedure Assign(const Obj: TGraphicObject); override;
    constructor CreateFromStream(const Stream: TStream; const
      Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure BeginUseProfilePoints; override;
    procedure EndUseProfilePoints; override;
    {: This property may be used in the
       <See Method=TCurve2D@PopulateCurvePoints> as a parameters
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
  end;

  TStarKind = (starCircle, starSquare, starDiamond, starTriUp, starTriDown,
    starPenta, starStar4, starStar5, starStar6, starCross,
    starDCross, starAster5, starStar4Arc);

const
  StarsIDs: array[0..10] of string[8] =
  ('circle', 'square', 'diamond', 'triup', 'tridown', 'penta',
    'star4', 'star5', 'star6', 'cross', 'dcross');

type

  TStar2D = class(TPrimitive2D)
  private
  protected
    function FillPieces: TRect2D;
    procedure _UpdateExtension; override;
  public
    StarKind: TStarKind;
    class function GetName: string; override;
    constructor Create(ID: Longint; const P: TPoint2D);
    procedure WhenCreated;
    procedure Assign(const Obj: TGraphicObject); override;
    constructor CreateFromStream(const Stream: TStream; const
      Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure Draw(VT: TTransf2D; const Cnv:
      TDecorativeCanvas; const ClipRect2D: TRect2D; const
      DrawMode: Integer); override;
  end;

  TBox2D0 = class(TCurve2D)
  protected
    function PopulateCurvePoints(N: Word): TRect2D; override;
  public
    {: This constructor creates a new 2D frame.

       Parameters:

       <LI=<I=ID> is the object identifier.>
       <LI=<I=P1> is the bottom-left corner of the frame.>
       <LI=<I=P2> is the upper-right corner of the frame.>
       //TSY: P3 is angle setting point
    }
    constructor Create(ID: Longint);
    procedure WhenCreated;
    procedure Assign(const Obj: TGraphicObject); override;
    procedure PolyPoints(var PP: TPointsSet2D; T:
      TTransf2D);
  end;

  {: This class defines a 2D rectangle.

     The entity has two <I=control points> that are the corner
     points of the rectangle.
  }
  TRectangle2D = class(TBox2D0)
  private
  public
    class function GetName: string; override;
    constructor Create(ID: Longint);
    procedure WhenCreated;
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
    function PopulateCurvePoints(N: Word): TRect2D; override;
  public
    class function GetName: string; override;
    procedure GetEllipseParams(var CX, CY, RX, RY, ARot:
      TRealType);
    procedure BezierPoints(var PP: TPointsSet2D; T: TTransf2D);
    procedure Draw(VT: TTransf2D; const Cnv:
      TDecorativeCanvas; const ClipRect2D: TRect2D; const
      DrawMode: Integer); override;
  end;

  TCircle2D = class(TCurve2D)
  private
  protected
    function PopulateCurvePoints(N: Word): TRect2D; override;
  public
    class function GetName: string; override;
    constructor Create(ID: Longint);
    procedure WhenCreated;
    procedure Assign(const Obj: TGraphicObject); override;
    procedure BezierPoints(var PP: TPointsSet2D;
      T: TTransf2D);
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
  TCircular2D = class(TCurve2D)
  private
    fRadius, FStartAngle, FEndAngle: TRealType;
    procedure SetRadius(R: TRealType);
    procedure SetStartAngle(A: TRealType);
    procedure SetEndAngle(A: TRealType);
  protected
    function PopulateCurvePoints(N: Word): TRect2D; override;
  public
    procedure GetArcPoints(PP: TPointsSet2D; NPts: Word);
    procedure GetArcParams(var CX, CY, R, SA, EA:
      TRealType);
    constructor Create(ID: Longint; const CP: TPoint2D; R, SA,
      EA: TRealType);
    procedure WhenCreated;
    procedure Assign(const Obj: TGraphicObject); override;
    constructor CreateFromStream(const Stream: TStream; const
      Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure BezierPoints(var PP: TPointsSet2D;
      T: TTransf2D);
    procedure DrawPath(const Canvas: TCanvas;
      const IsClosed: Boolean; const Pnts: array of TPoint);
      override;
    procedure Draw(VT: TTransf2D; const Cnv:
      TDecorativeCanvas; const ClipRect2D: TRect2D; const
      DrawMode: Integer); override;
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
    constructor Create(ID: Longint; const CP: TPoint2D; R, SA,
      EA: TRealType);
    procedure WhenCreated;
  end;

  TSegment2D = class(TCircular2D)
  private
  public
    class function GetName: string; override;
    constructor Create(ID: Longint; const CP: TPoint2D; R, SA,
      EA: TRealType);
    procedure WhenCreated;
  end;

  TSpline2D0 = class(TCurve2D)
  private
    fOrder: Byte;
  public
    procedure Assign(const Obj: TGraphicObject); override;
    constructor CreateFromStream(const Stream: TStream; const
      Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;
    function GetPoint(I: Integer): TPoint2D; virtual;
    procedure BezierPoints(var PP: TPointsSet2D; T: TTransf2D);
      virtual; abstract;
    procedure Draw(VT: TTransf2D; const Cnv:
      TDecorativeCanvas; const ClipRect2D: TRect2D; const
      DrawMode: Integer); override;
    property Order: Byte read fOrder write fOrder;
  end;

  {: This class defines a 2D B-Spline curve.

     The B-Spline is defined by its control points.
     The order of the spline is 3 but you can change it.
  }
  TBSpline2D0 = class(TSpline2D0)
  protected
    function PopulateCurvePoints(N: Word): TRect2D; override;
  public
    {: This constructor creates a new 2D spline.

       Parameters:

       <LI=<I=ID> is the object identifier.>
       <LI=<I=Pts> is an array that contains the control points
        of the spline. If you want to create a pointless spline
        (because you want to add the points after the construction)
        you must pass an array of only one point (an empty array is
        not allowed by Delphi) and delete the point after the
        construction.>
    }
    constructor Create(ID: Longint; const Pts: array of
      TPoint2D);
    procedure WhenCreated;
    {: This property contains the order of the spline.

       By default it is three (cubic? spline).
    }
    function OnProfile(PT: TPoint2D; Aperture: TRealType):
      Integer; override;
  end;

  TBSpline2D = class(TBSpline2D0)
  private
  public
    class function GetName: string; override;
    function GetPoint(I: Integer): TPoint2D; override;
    procedure BezierPoints(var PP: TPointsSet2D; T: TTransf2D);
      override;
  end;

  TCubicBSpline2D = class(TBSpline2D0)
  private
  public
    class function GetName: string; override;
    constructor Create(ID: Longint; const Pts: array of
      TPoint2D);
    procedure WhenCreated;
    function GetPoint(I: Integer): TPoint2D; override;
    procedure BezierPoints(var PP: TPointsSet2D; T: TTransf2D);
      override;
  end;

  TClosedBSpline2D0 = class(TSpline2D0)
  protected
    function PopulateCurvePoints(N: Word): TRect2D; override;
  public
    constructor Create(ID: Longint; const Pts: array of
      TPoint2D);
    procedure WhenCreated;
    function OnProfile(PT: TPoint2D; Aperture: TRealType):
      Integer; override;
    procedure BezierPoints(var PP: TPointsSet2D; T: TTransf2D);
      override;
  end;

  TClosedBSpline2D = class(TClosedBSpline2D0)
  private
  public
    class function GetName: string; override;
  end;

  TClosedCubicBSpline2D = class(TClosedBSpline2D0)
  private
  public
    class function GetName: string; override;
    constructor Create(ID: Longint; const Pts: array of
      TPoint2D);
    procedure WhenCreated;
    procedure BezierPoints(var PP: TPointsSet2D; T: TTransf2D);
      override;
  end;

  {: Smooth cubic Bezier path - Hobby spline
  }
  TSmoothPath2D0 = class(TCurve2D)
  protected
    function PopulateCurvePoints(N: Word): TRect2D; override;
  public
    constructor Create(ID: Longint; const Pts: array of
      TPoint2D);
    procedure WhenCreated;
    constructor CreateFromStream(const Stream:
      TStream; const Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure Assign(const Obj: TGraphicObject); override;
    procedure BezierPoints(var PP: TPointsSet2D; T: TTransf2D);
      virtual;
    procedure Draw(VT: TTransf2D; const Cnv:
      TDecorativeCanvas; const ClipRect2D: TRect2D; const
      DrawMode: Integer); override;
    function OnProfile(PT: TPoint2D; Aperture: TRealType):
      Integer; override;
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
    constructor Create(ID: Longint; const Pts: array of
      TPoint2D);
    procedure WhenCreated;
    procedure BezierPoints(var PP: TPointsSet2D; T: TTransf2D);
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
  TVJustification = (jvBottom, jvCenter, jvTop);

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
    //TSY:
    fHJustification: THJustification;
    fVJustification: TVJustification;
    fExtFont: TExtendedFont;
    fDrawBox, fRecalcBox: Boolean;
    fClippingFlags: Integer; // Win32s DrawText flags.
    procedure ResetJustification;
    procedure SetHJustification(J: THJustification);
    procedure SetVJustification(J: TVJustification);
  public
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
    constructor Create(ID: Longint; P: TPoint2D; Height:
      TRealType; Txt: AnsiString);
    procedure WhenCreated;
    constructor CreateFromStream(const Stream: TStream; const
      Version: TCADVersion); override;
    destructor Destroy; override;
    procedure Assign(const Obj: TGraphicObject); override;
    procedure SaveToStream(const Stream: TStream); override;
    function GetExtension: TRect2D;
    procedure _UpdateExtension; override;
    procedure Draw(VT: TTransf2D; const Cnv:
      TDecorativeCanvas; const ClipRect2D: TRect2D; const
      DrawMode: Integer); override;
    function OnMe(PT: TPoint2D; Aperture: TRealType; var
      Distance: TRealType): Integer; override;
    {: This property contains the heigth of the text in world
       units.

       This is different from the Heigth of font used to render
       the text object.
    }
    procedure TransForm(const T: TTransf2D); override;
    property Height: TRealType read fHeight write fHeight;
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
    constructor Create(ID: Longint; const P1, P2: TPoint2D; BMP:
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
    {: This property may contains the scale factor to be used for the
       bitmap.

       If you set this property the bounding box is recomputed and
       the bitmap is not stretched. The first Points will remain
       in the position specified but the second point will be
       repositioned using the ScaleFactor.

       The value rapresent how many drawing unit correspond to
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
    constructor Create(ID: Longint; FontVect: TVectFont;
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
function CalcRotationAngle(P0, P1: TPoint2D): Double;
function BezierPoint(const P0, P1, P2, P3: TPoint2D;
  const U: TRealType): TPoint2D;

const
  {: This constats rapresent the maximum number of vectorial fonts
     that may be used in the library.
  }
  MAX_REGISTERED_FONTS = 512;

implementation

uses Math, Dialogs;

var
  VectFonts2DRegistered: array[0..MAX_REGISTERED_FONTS] of
  TVectFont;
  _NullChar: TVectChar;
  _DefaultFont: TVectFont;
  _DefaultHandler2D: TPrimitive2DHandler;

// =====================================================================
// TExtendedFont
// =====================================================================

procedure TExtendedFont.SetHeight(Value: Word);
begin
  LogFont.lfHeight := Value;
  SetNewValue;
end;

function TExtendedFont.GetHeight: Word;
begin
  Result := LogFont.lfHeight;
end;

procedure TExtendedFont.SetWidth(Value: Word);
begin
  LogFont.lfWidth := Value;
  SetNewValue;
end;

function TExtendedFont.GetWidth: Word;
begin
  Result := LogFont.lfWidth;
end;

procedure TExtendedFont.SetEscapement(Value: Word);
begin
  LogFont.lfEscapement := Value;
  SetNewValue;
end;

function TExtendedFont.GetEscapement: Word;
begin
  Result := LogFont.lfEscapement;
end;

procedure TExtendedFont.SetWeight(Value: Word);
begin
  LogFont.lfWeight := Value;
  SetNewValue;
end;

function TExtendedFont.GetWeight: Word;
begin
  Result := LogFont.lfWeight;
end;

procedure TExtendedFont.SetItalic(Value: Byte);
begin
  LogFont.lfItalic := Value;
  SetNewValue;
end;

function TExtendedFont.GetItalic: Byte;
begin
  Result := LogFont.lfItalic;
end;

procedure TExtendedFont.SetUnderline(Value: Byte);
begin
  LogFont.lfUnderline := Value;
  SetNewValue;
end;

function TExtendedFont.GetUnderline: Byte;
begin
  Result := LogFont.lfUnderline;
end;

procedure TExtendedFont.SetStrikeOut(Value: Byte);
begin
  LogFont.lfStrikeOut := Value;
  SetNewValue;
end;

function TExtendedFont.GetStrikeOut: Byte;
begin
  Result := LogFont.lfStrikeOut;
end;

procedure TExtendedFont.SetCharSet(Value: Byte);
begin
  LogFont.lfCharSet := Value;
  SetNewValue;
end;

function TExtendedFont.GetCharSet: Byte;
begin
  Result := LogFont.lfCharSet;
end;

procedure TExtendedFont.SetOutPrecision(Value: Byte);
begin
  LogFont.lfOutPrecision := Value;
  SetNewValue;
end;

function TExtendedFont.GetOutPrecision: Byte;
begin
  Result := LogFont.lfOutPrecision;
end;

procedure TExtendedFont.SetClipPrecision(Value: Byte);
begin
  LogFont.lfClipPrecision := Value;
  SetNewValue;
end;

function TExtendedFont.GetClipPrecision: Byte;
begin
  Result := LogFont.lfClipPrecision;
end;

procedure TExtendedFont.SetQuality(Value: Byte);
begin
  LogFont.lfQuality := Value;
  SetNewValue;
end;

function TExtendedFont.GetQuality: Byte;
begin
  Result := LogFont.lfQuality;
end;

procedure TExtendedFont.SetPicthAndFamily(Value: Byte);
begin
  LogFont.lfPitchAndFamily := Value;
  SetNewValue;
end;

function TExtendedFont.GetPicthAndFamily: Byte;
begin
  Result := LogFont.lfPitchAndFamily;
end;

procedure TExtendedFont.SetFaceName(Value: TFaceName);
var
  Cont: Byte;
begin
  for Cont := 1 to Length(Value) do
    LogFont.lfFaceName[Cont - 1] := Value[Cont];
  LogFont.lfFaceName[Length(Value)] := #0;
  SetNewValue;
end;

function TExtendedFont.GetFaceName: TFaceName;
begin
  Result := LogFont.lfFaceName;
end;

procedure TExtendedFont.SetNewValue;
var
  TmpHandle: HFONT;
begin
  TmpHandle := CreateFontIndirect(LogFont);
  if Assigned(fCanvas) then
    SelectObject(fCanvas.Handle, TmpHandle);
  DeleteObject(FHandle);
  FHandle := TmpHandle;
end;

procedure TExtendedFont.SetCanvas(Cnv: TCanvas);
begin
  if Assigned(fCanvas) then
    SelectObject(fCanvas.Handle, fCanvas.Font.Handle);
  fCanvas := Cnv;
  if Assigned(fCanvas) then
    SelectObject(fCanvas.Handle, FHandle);
end;

constructor TExtendedFont.Create;
begin
  inherited Create;
  GetObject(GetStockObject(DEFAULT_GUI_FONT), SizeOf(LogFont),
    @LogFont);
  LogFont.lfFaceName := 'Small Font';
  FHandle := CreateFontIndirect(LogFont);
end;

procedure TExtendedFont.Assign(Obj: TExtendedFont);
begin
  if Obj = Self then
    Exit;
  LogFont := TExtendedFont(Obj).LogFont;
  SetNewValue;
end;

destructor TExtendedFont.Destroy;
begin
  if Assigned(fCanvas) then
    SelectObject(fCanvas.Handle, fCanvas.Font.Handle);
  DeleteObject(FHandle);
  inherited Destroy;
end;

procedure TExtendedFont.SaveToStream(Strm: TStream);
begin
  with Strm do
    Write(LogFont, SizeOf(LogFont));
end;

procedure TExtendedFont.LoadFromStream(Strm: TStream);
begin
  with Strm do
  begin
    Read(LogFont, SizeOf(LogFont));
    SetNewValue;
  end;
end;

// =====================================================================
// TPrimitive2DHandler
// =====================================================================

procedure TPrimitive2DHandler.DrawControlPoints(const Sender:
  TObject2D; const VT: TTransf2D; const Cnv: TDecorativeCanvas;
  const Width: Integer);
var
  TmpPt: TPoint2D;
  Cont: Integer;
begin
  if Sender is TPrimitive2D then
    with TPrimitive2D(Sender) do
      for Cont := 0 to Points.Count - 1 do
      begin
        TmpPt := TransformPoint2D(Points[Cont], VT);
        DrawPlaceHolder(Cnv, Round(TmpPt.X), Round(TmpPt.Y),
          Width);
      end;
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

constructor TPiece.Create(const _Capacity: Word);
begin
  inherited Create(_Capacity);
  Line := pliDefault;
  Fill := pfiDefault;
  Hatch := phaDefault;
end;

function TPiece.GetLineKind(Obj: TPrimitive2D): TLineKind;
begin
  case Line of
    pliDefault: Result := Obj.LineKind;
    pliNone: Result := liNone;
    pliSolidDefault:
      if Obj.LineKind in [liThin, liDashed] then Result := liThin
      else if Obj.LineKind in [liNone] then Result := liNone
      else Result := liThick;
    pliFillAsDefault:
      if Obj.LineKind in [liThin, liDashed] then Result := liThin
      else Result := liThick;
    pliFill:
      if Obj.FillColor = clDefault then Result := liNone
      else
        if Obj.LineKind in [liThin, liDashed] then Result := liThin
        else Result := liThick;
  end;
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
// TArrayOfPiece
// =====================================================================

constructor TArrayOfPiece.Create(Capacity: Integer);
var
  I: Integer;
begin
  inherited Create;
  SetLength(Pieces, Capacity);
  Count := 0;
  for I := 0 to Capacity - 1 do Pieces[I] := nil;
end;

destructor TArrayOfPiece.Destroy;
begin
  inherited Destroy;
end;

procedure TArrayOfPiece.Clear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do FreeAndNil(Pieces[I]);
  Count := 0;
end;

procedure TArrayOfPiece.Add(Piece: TPiece);
begin
  Pieces[Count] := Piece;
  Inc(Count);
end;

function TArrayOfPiece.GetItem(I: Integer): TPiece;
begin
  Result := Pieces[I];
end;

procedure TArrayOfPiece.GetExtension0(var R: TRect2D; const FirstPass: Boolean);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Pieces[I].GetExtension0(R, FirstPass and (I = 0));
end;

function TArrayOfPiece.GetExtension: TRect2D;
begin
  Result := Rect2D(0, 0, 0, 0);
  GetExtension0(Result, True);
end;

// =====================================================================
// TPrimitive2D
// =====================================================================

procedure TPrimitive2D._UpdateExtension;
begin
  if not Assigned(fPoints) or (fPoints.Count = 0) then
    WritableBox := Rect2D(0, 0, 0, 0)
  else
   { Change the extension. }
    WritableBox := fPoints.Extension;
end;

function TPrimitive2D.CreateVect(const Size: Integer):
  TPointsSet2D;
begin
  Result := TPointsSet2D.Create(Size);
end;

constructor TPrimitive2D.Create(ID: Longint; NPts: Integer);
begin
  inherited Create(ID);
  { Create the internal vector. }
  fPoints := CreateVect(NPts);

  WhenCreated;
  {
  fHatching := haNone;
  fLineKind := liThin;
  fLineColor := clDefault;
  fHatchColor := clDefault;
  fFillColor := clDefault;
  SetSharedHandler(_DefaultHandler2D);
  fCanDeletePoints := False;
  fDrawPathBezier := True;}
end;

procedure TPrimitive2D.WhenCreated;
begin
  if fPoints <> nil then
    fPoints.OnChange := UpdateExtension;
  fHatching := haNone;
  fLineKind := liThin;
  fLineColor := clDefault;
  fHatchColor := clDefault;
  fFillColor := clDefault;
  SetSharedHandler(_DefaultHandler2D);
  fCanDeletePoints := False;
  fDrawPathBezier := True;
  Pieces := nil;
end;

destructor TPrimitive2D.Destroy;
begin
  fPoints.Free;
  Pieces.Free;
  inherited Destroy;
end;

procedure TPrimitive2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
    Exit;
  inherited Assign(Obj);
  if (Obj is TPrimitive2D) then
  begin // Per default non aggiunge i punti.
    if not Assigned(fPoints) then
    begin
      fPoints := CreateVect(0);
      fPoints.GrowingEnabled := True;
      fPoints.OnChange := UpdateExtension;
    end;
    fPoints.Clear;
    fHatching := TPrimitive2D(Obj).fHatching;
    fLineKind := TPrimitive2D(Obj).fLineKind;
    fLineColor := TPrimitive2D(Obj).fLineColor;
    fHatchColor := TPrimitive2D(Obj).fHatchColor;
    fFillColor := TPrimitive2D(Obj).fFillColor;
    fCanDeletePoints := TPrimitive2D(Obj).fCanDeletePoints;
    fDrawPathBezier := TPrimitive2D(Obj).fDrawPathBezier;
  end;
end;

constructor TPrimitive2D.CreateFromStream(const Stream: TStream;
  const Version: TCADVersion);
var
  TmpWord: Word;
  Cont: Integer;
  TmpPt: TPoint2D;
  TmpBoolean: Boolean;
begin
  { Load the standard properties }
  inherited;
  WhenCreated;
  with Stream do
  begin
    Read(TmpWord, SizeOf(TmpWord));
    fPoints := CreateVect(TmpWord);
     { Read all the points. }
    for Cont := 0 to TmpWord - 1 do
    begin
      Read(TmpPt, SizeOf(TmpPt));
      fPoints.Points[Cont] := TmpPt;
    end;
    Read(TmpBoolean, SizeOf(TmpBoolean));
    fPoints.GrowingEnabled := TmpBoolean;
    Read(fHatching, SizeOf(fHatching));
    Read(fLineKind, SizeOf(fLineKind));
    Read(fLineColor, SizeOf(fLineColor));
    Read(fHatchColor, SizeOf(fHatchColor));
    Read(fFillColor, SizeOf(fFillColor));
    Read(fCanDeletePoints, SizeOf(fCanDeletePoints));
    Read(fDrawPathBezier, SizeOf(fDrawPathBezier));
  end;
  fPoints.OnChange := UpdateExtension;
  SetSharedHandler(_DefaultHandler2D);
end;

procedure TPrimitive2D.SaveToStream(const Stream: TStream);
var
  TmpWord: Word;
  Cont: Integer;
  TmpPt: TPoint2D;
  TmpBoolean: Boolean;
begin
  { Save the standard properties }
  inherited SaveToStream(Stream);
  with Stream do
  begin
    TmpWord := fPoints.Count;
    Write(TmpWord, SizeOf(TmpWord));
     { Write all points. }
    for Cont := 0 to TmpWord - 1 do
    begin
      TmpPt := fPoints.Points[Cont];
      Write(TmpPt, SizeOf(TmpPt));
    end;
    TmpBoolean := fPoints.GrowingEnabled;
    Write(TmpBoolean, SizeOf(TmpBoolean));
    Write(fHatching, SizeOf(fHatching));
    Write(fLineKind, SizeOf(fLineKind));
    Write(fLineColor, SizeOf(fLineColor));
    Write(fHatchColor, SizeOf(fHatchColor));
    Write(fFillColor, SizeOf(fFillColor));
    Write(fCanDeletePoints, SizeOf(fCanDeletePoints));
    Write(fDrawPathBezier, SizeOf(fDrawPathBezier));
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
      Points2D.DrawAsPolygon(Cnv,
        RectToRect2D(Cnv.Canvas.ClipRect), Box, VT);
    end;
  if (DrawMode and DRAWMODE_OutlineOnly <> 0)
    or (LineKind <> liNone) then
    if Connected then
      Points2D.DrawAsPolygonOutline(Cnv,
        RectToRect2D(Cnv.Canvas.ClipRect), Box, VT)
    else
      Points2D.DrawAsPolyline(Cnv,
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
  const LineKind: TLineKind;
  const Hatching: THatching;
  const LineColor, FillColor, HatchColor: TColor;
  const LineWidth, PixelSize, UnitLength, Scale: TRealType);
var
  IsMetafile: Boolean;
  BrushStyle0: TBrushStyle;
  BrushColor0: TColor;
  PenStyle, PenStyle0: TPenStyle;
  PenColor0: TColor;
  LOGBRUSH: tagLOGBRUSH;
  LOGPEN: tagLOGPEN;
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
      PS_Geometric or Ord(PenStyle) {PS_Solid} or PS_EndCap_Flat or
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
    SetBkMode(Cnv.Canvas.Handle, TRANSPARENT);
  end
  else
  begin
    SetBkColor(Cnv.Canvas.Handle, FillColor);
    SetBkMode(Cnv.Canvas.Handle, OPAQUE);
  end;
  PathProc(Cnv.Canvas, IsClosed, Pnts);
  if LineKind = liNone then SetPen(psClear, clRed)
  else
  begin
    SetMiterLimit(Cnv.Canvas.Handle, 10, nil);
    case LineKind of
      liThin:
        begin
          Cnv.Canvas.Pen.Width := 1;
          PenStyle := psSolid;
        end;
      liThick:
        begin
          Cnv.Canvas.Pen.Width := 2;
          PenStyle := psSolid;
        end;
      liDotted:
        begin
          Cnv.Canvas.Pen.Width := 2;
          PenStyle := psDot;
        end;
      liDashed:
        begin
          Cnv.Canvas.Pen.Width := 1;
          PenStyle := psDash;
        end;
    end;
    if IsMetafile then Cnv.Canvas.Pen.Width :=
      Max(Round(LineWidth * Cnv.Canvas.Pen.Width / UnitLength), 1)
    else Cnv.Canvas.Pen.Width :=
      Max(Round(LineWidth * Cnv.Canvas.Pen.Width / Scale / PixelSize),
        1 {Cnv.Canvas.Pen.Width});
    if LineColor = clDefault
      then SetPen(PenStyle, PenColor0)
    else SetPen(PenStyle, LineColor);
  end;
  if LineKind <> liNone then
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
  LineWidth, PixelSize, UnitLength, Scale: TRealType;
begin
  if OwnerCAD = nil then
  begin
    LineWidth := 1;
    PixelSize := 1;
    UnitLength := 1;
    Scale := 1;
  end
  else
  begin
    LineWidth := (OwnerCAD as TDrawing2D).LineWidth;
    PixelSize := OwnerCAD.Viewports[0].GetPixelAperture.X;
    UnitLength := (OwnerCAD as TDrawing2D).PicUnitLength;
    Scale := (OwnerCAD as TDrawing2D).PicScale;
  end;
  DrawNative0(VT, Cnv, DrawPath, ClipRect2D, DrawMode, IsClosed, Pnts,
    fLineKind, fHatching, fLineColor, fFillColor, fHatchColor,
    LineWidth, PixelSize, UnitLength, Scale);
end;

procedure TPrimitive2D.DrawPiece(Piece: TPiece;
  const VT: TTransf2D; const Cnv:
  TDecorativeCanvas; const ClipRect2D: TRect2D; const
  DrawMode: Integer);
var
  PathProc: TDrawPathProc;
  IsClosed: Boolean;
  Pnts: array of TPoint;
  I: Integer;
  LineWidth, PixelSize, UnitLength, Scale: TRealType;
begin
  //if OwnerCAD = nil then Exit;
  if OwnerCAD = nil then
  begin
    LineWidth := 1;
    PixelSize := 1;
    UnitLength := 1;
    Scale := 1;
  end
  else
  begin
    LineWidth := (OwnerCAD as TDrawing2D).LineWidth;
    PixelSize := OwnerCAD.Viewports[0].GetPixelAperture.X;
    UnitLength := (OwnerCAD as TDrawing2D).PicUnitLength;
    Scale := (OwnerCAD as TDrawing2D).PicScale;
  end;
  if Piece.Count <= 1 then Exit;
  IsClosed := IsSamePoint2D(Piece[0], Piece[Piece.Count - 1]);
  if Piece is TPath then PathProc := DrawPolyline
  else //if Piece is TBezierPath
    PathProc := DrawBezierPath;
  SetLength(Pnts, Piece.Count);
  for I := 0 to Piece.Count - 1 do
    Pnts[I] := Point2DToPoint(TransformPoint2D(Piece[I], VT));
  DrawNative0(VT, Cnv, PathProc, ClipRect2D, DrawMode, IsClosed, Pnts,
    Piece.GetLineKind(Self), Piece.GetHatching(Self),
    Piece.GetLineColor(Self), Piece.GetFillColor(Self), fHatchColor,
    LineWidth, PixelSize, UnitLength, Scale);
end;

procedure TPrimitive2D.DrawPieces(const VT: TTransf2D; const Cnv:
  TDecorativeCanvas; const ClipRect2D: TRect2D; const
  DrawMode: Integer);
var
  I: Integer;
begin
  for I := 0 to Pieces.Count - 1 do
    if Pieces.Pieces[I] <> nil then
      DrawPiece(Pieces.Pieces[I], VT, Cnv, ClipRect2D, DrawMode);
end;

procedure TPrimitive2D.TransForm(const T: TTransf2D);
begin
  fPoints.TransformPoints(T);
end;

procedure TPrimitive2D.DeleteControlPoint0(const I: Integer);
begin
  if (I < 0) or (I > fPoints.Count - 1) then Exit;
  fPoints.Delete(I);
  if OwnerCAD is TDrawing2D then
    (OwnerCAD as TDrawing2D).NotifyChanged;
end;

procedure TPrimitive2D.DeleteControlPoint(const I: Integer);
begin
  if fCanDeletePoints then DeleteControlPoint0(I);
end;

procedure TPrimitive2D.InsertControlPoint0(const Pos: Integer;
  P: TPoint2D);
begin
  if (Pos < 0) or (Pos > fPoints.Count) then Exit;
  if Pos < fPoints.Count then fPoints.Insert(Pos, P)
  else fPoints.Add(P);
  if OwnerCAD is TDrawing2D then
    (OwnerCAD as TDrawing2D).NotifyChanged;
end;

procedure TPrimitive2D.InsertControlPoint(const Pos: Integer; P:
  TPoint2D);
begin
  if fCanDeletePoints then InsertControlPoint0(Pos, P);
end;

// =====================================================================
// TLine2D
// =====================================================================

const
  ArrowKindes: array[0..9, 1..3] of TRealType = (
    (0, 0, 4), (1, 0, 4), (1, 1, 4), (1, 2, 4), (1, 3, 4),
    (1, 4, 4), (1, 5, 4), (1, 6, 4), (1, 7, 4),
    (1, 8, 4));

class function TLine2D.GetName: string;
begin
  Result := 'Line';
end;

constructor TLine2D.Create(ID: Longint; const P1, P2: TPoint2D);
begin
  inherited Create(ID, 2);
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
    if TPrimitive2D(Obj).fPoints.Count > 1 then
      fPoints.Copy(TPrimitive2D(Obj).fPoints, 0, 1);
    fPoints.GrowingEnabled := False;
  end;
  if (Obj is TLine2D) then
  begin
    BeginArrowKind := (Obj as TLine2D).BeginArrowKind;
    EndArrowKind := (Obj as TLine2D).EndArrowKind;
  end;
end;

constructor TLine2D.CreateFromStream(const Stream: TStream;
  const Version: TCADVersion);
begin
  inherited;
  WhenCreated;
  with Stream do
  begin
    Read(BeginArrowKind, SizeOf(BeginArrowKind));
    Read(EndArrowKind, SizeOf(EndArrowKind));
  end;
end;

procedure TLine2D.SaveToStream(const Stream: TStream);
begin
  inherited SaveToStream(Stream);
  with Stream do
  begin
    Write(BeginArrowKind, SizeOf(BeginArrowKind));
    Write(EndArrowKind, SizeOf(EndArrowKind));
  end;
end;

function TLine2D.FillPieces: TRect2D;
var
  L: TRealType;
  P0, P1: TPoint2D;
  BeginArrow: TArrowData;
  EndArrow: TArrowData;
  ArrowSize: TRealType;
  function GetArrowData(Kind: TArrowKind): TArrowData;
  begin
    Result.W := ArrowKindes[Ord(Kind), 1];
    Result.L1 := ArrowKindes[Ord(Kind), 2];
    Result.L2 := ArrowKindes[Ord(Kind), 3];
  end;
  procedure Add(const IPiece: Integer; const X, Y: TRealType);
  begin
    Pieces.Pieces[IPiece].Add(Point2D(X, Y));
  end;
  procedure AddArrow(W, L1, L2, X1, Y1, X2, Y2: TRealType);
  var
    XC, YC, WY, WX, L2Y, L2X: TRealType;
    IPiece: Integer;
    APath: TPath;
  begin
    IPiece := Pieces.Count;
    APath := TPath.Create(5);
    Pieces.Add(APath);
    APath.Line := pliSolidDefault;
    APath.Fill := pfiLineAsDefault;
    APath.Hatch := phaNone;
    L1 := L1 * ArrowSize;
    L2 := L2 * ArrowSize;
    W := W * ArrowSize;
    XC := L1 / L * (X2 - X1) + X1;
    YC := L1 / L * (Y2 - Y1) + Y1;
    WX := W * (X2 - X1);
    WY := W * (Y2 - Y1);
    L2X := L2 * (X2 - X1);
    L2Y := L2 * (Y2 - Y1);
    Pieces.Pieces[0].Add(Point2D(XC, YC));
    Add(IPiece, XC, YC);
    Add(IPiece, (L2X - WY) / L + X1, (L2Y + WX) / L + Y1);
    Add(IPiece, X1, Y1);
    Add(IPiece, (L2X + WY) / L + X1, (L2Y - WX) / L + Y1);
    Add(IPiece, XC, YC);
  end;
begin
  if Pieces = nil then Pieces := TArrayOfPiece.Create(3)
  else Pieces.Clear;
  Result := Rect2D(0, 0, 0, 0);
  if OwnerCAD is TDrawing2D then
    ArrowSize := (OwnerCAD as TDrawing2D).ArrowsSize
  else ArrowSize := 1;
  BeginArrow := GetArrowData(BeginArrowKind);
  EndArrow := GetArrowData(EndArrowKind);
  L := Sqrt(Sqr(Points[0].X - Points[1].X)
    + Sqr(Points[0].Y - Points[1].Y));
  if L = 0 then Exit;
  Pieces.Add(TPath.Create(2));
  P0 := fPoints[0];
  P1 := fPoints[1];
  with BeginArrow do
  begin
    if W = 0 then Pieces.Pieces[0].Add(P0)
    else
      AddArrow(W, L1, L2, P0.X, P0.Y, P1.X, P1.Y);
  end;
  with EndArrow do
  begin
    if W = 0 then Pieces.Pieces[0].Add(P1)
    else
      AddArrow(W, L1, L2, P1.X, P1.Y, P0.X, P0.Y);
  end;
  Result := Pieces.GetExtension;
end;

procedure TLine2D.Draw(VT: TTransf2D; const Cnv:
  TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode:
  Integer);
begin
  FillPieces;
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

procedure TLine2D._UpdateExtension;
begin
  if not Assigned(fPoints) or (fPoints.Count = 0) then
    WritableBox := Rect2D(0, 0, 0, 0)
  else if (BeginArrowKind <> arrNone) or (EndArrowKind <> arrNone) then
    WritableBox := FillPieces
  else WritableBox := fPoints.Extension;
end;


// =====================================================================
// TOutline2D
// =====================================================================

constructor TOutline2D.Create(ID: Longint; NPts: Integer);
begin
  inherited Create(ID, NPts);
  WhenCreated;
end;

procedure TOutline2D.WhenCreated;
begin
  fIsClosed := False;
  fOwnsInterior := True;
end;

procedure TOutline2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
    Exit;
  inherited Assign(Obj);
  if (Obj is TOutline2D) then
  begin
    fIsClosed := (Obj as TOutline2D).fIsClosed;
    fOwnsInterior := (Obj as TOutline2D).fOwnsInterior;
  end;
end;

constructor TOutline2D.CreateFromStream(const Stream: TStream;
  const Version: TCADVersion);
begin
  inherited;
  with Stream do
  begin
    Read(fIsClosed, SizeOf(fIsClosed));
    Read(fOwnsInterior, SizeOf(fOwnsInterior));
  end;
end;

procedure TOutline2D.SaveToStream(const Stream: TStream);
begin
  inherited SaveToStream(Stream);
  with Stream do
  begin
    Write(fIsClosed, SizeOf(fIsClosed));
    Write(fOwnsInterior, SizeOf(fOwnsInterior));
  end;
end;

procedure TOutline2D.BeginUseProfilePoints;
begin
  // Due to the fact that ControlPoints are equal to ProfilePoints
  // there is no need to do initialization here.
end;

procedure TOutline2D.EndUseProfilePoints;
begin
  // Due to the fact that ControlPoints are equal to ProfilePoints
  // there is no need to do finalization here.
end;

procedure TOutline2D.Draw(VT: TTransf2D; const Cnv:
  TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode:
  Integer);
begin
  BeginUseProfilePoints;
  try
    if Assigned(ProfilePoints) then
      DrawPoints(ProfilePoints, False, fIsClosed,
        VT, Cnv, ClipRect2D, DrawMode);
  finally
    EndUseProfilePoints;
  end;
end;

function TOutline2D.OnMe(PT: TPoint2D; Aperture: TRealType;
  var Distance: TRealType): Integer;
var
  TmpDist: TRealType;
begin
  Result := inherited OnMe(PT, Aperture, Distance);
  if Result <> PICK_INBBOX then Exit;
  BeginUseProfilePoints;
  try
    if Assigned(ProfilePoints) then
    begin
      if fOwnsInterior or (fHatching <> haNone)
        or (fFillColor <> clDefault) then
        Result := MaxIntValue([PICK_INBBOX,
          IsPointInPolygon2D(ProfilePoints.PointsReference,
            ProfilePoints.Count,
            PT, TmpDist, Aperture, IdentityTransf2D)])
      else
        Result := MaxIntValue([PICK_INBBOX,
          IsPointOnPolyLine2D(ProfilePoints.PointsReference,
            ProfilePoints.Count,
            PT, TmpDist, Aperture, IdentityTransf2D,
            fIsClosed)]);
    end;
    Distance := MinValue([Aperture, TmpDist]);
  finally
    EndUseProfilePoints;
  end;
end;

function TOutline2D.OnProfile(PT: TPoint2D; Aperture:
  TRealType): Integer;
begin
  BeginUseProfilePoints;
  try
    Result := FindPointPolylinePosition(
      ProfilePoints.PointsReference,
      ProfilePoints.Count, PT, Aperture,
      fIsClosed);
  finally
    EndUseProfilePoints;
  end;
end;

// =====================================================================
// TPolyline2D0
// =====================================================================

constructor TPolyline2D0.Create(ID: Longint; const Pts: array of
  TPoint2D);
begin
  inherited Create(ID, High(Pts) - Low(Pts) + 1);
  fPoints.AddPoints(Pts);
  WhenCreated;
end;

procedure TPolyline2D0.WhenCreated;
begin
  fOwnsInterior := False;
  fCanDeletePoints := True;
  fDrawPathBezier := False;
end;

procedure TPolyline2D0.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
    Exit;
  inherited Assign(Obj);
  if Obj is TPrimitive2D then
  begin
    if (Obj is TOutline2D) and not (Obj is TPolyline2D0) then
    begin
      BeginUseProfilePoints;
      try
        fPoints.Copy(TOutline2D(Obj).ProfilePoints, 0,
          TOutline2D(Obj).ProfilePoints.Count - 1)
      finally
        EndUseProfilePoints;
      end;
    end
    else
      fPoints.Copy(TPrimitive2D(Obj).fPoints, 0,
        TPrimitive2D(Obj).fPoints.Count - 1);
    fPoints.GrowingEnabled := True;
  end;
  fOwnsInterior := False;
  fCanDeletePoints := True;
end;

function TPolyline2D0.GetProfilePoints: TPointsSet2D;
begin
  Result := fPoints;
end;

function TPolyline2D0.GetNPts: Integer;
begin
  Result := fPoints.Count;
end;

procedure TPolyline2D0.Draw(VT: TTransf2D; const Cnv:
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
  BeginUseProfilePoints;
  try
    SetLength(Pnts, ProfilePoints.Count);
    for I := 0 to ProfilePoints.Count - 1 do
      Pnts[I] := Point2DToPoint(
        TransformPoint2D(ProfilePoints[I], VT));
  finally
    EndUseProfilePoints;
  end;
  DrawNative(VT, Cnv, ClipRect2D, DrawMode, IsClosed, Pnts);
end;

// =====================================================================
// TPolygon2D
// =====================================================================

class function TPolyline2D.GetName: string;
begin
  Result := 'Polyline';
end;

// =====================================================================
// TPolygon2D
// =====================================================================

class function TPolygon2D.GetName: string;
begin
  Result := 'Polygon';
end;

constructor TPolygon2D.Create(ID: Longint; const Pts: array of
  TPoint2D);
begin
  inherited Create(ID, Pts);
  WhenCreated;
end;

procedure TPolygon2D.WhenCreated;
begin
  fIsClosed := True;
end;

procedure TPolygon2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
    Exit;
  inherited Assign(Obj);
  fIsClosed := True;
end;

// =====================================================================
// TCurve2D
// =====================================================================

procedure TCurve2D.SetPrimitiveSavingType(S:
  TPrimitiveSavingType);
begin
  if S <> fSavingType then
  begin
    fSavingType := S;
    UpdateExtension(Self);
  end;
end;

procedure TCurve2D.SetCurvePrecision(N: Word);
begin
  if fCurvePrecision <> N then
    fCurvePrecision := N;
end;

function TCurve2D.PopulateCurvePoints(N: Word): TRect2D;
begin
  if not Assigned(fCurvePoints) then
    fCurvePoints := TPointsSet2D.Create(N)
  else
    fCurvePoints.Clear;
  Inc(fCountReference);
  fCurvePoints.GrowingEnabled := True;
  Result := Rect2D(0, 0, 0, 0);
end;

procedure TCurve2D.FreeCurvePoints;
begin
  Dec(fCountReference);
  if fCountReference <= 0 then
  begin
    fCurvePoints.Free;
    fCurvePoints := nil;
    fCountReference := 0;
  end;
end;

constructor TCurve2D.Create(ID: Longint; NPts: Integer;
  CurvePrec: Word);
begin
  inherited Create(ID, NPts);
  fCurvePrecision := CurvePrec;
  WhenCreated;
end;

procedure TCurve2D.WhenCreated;
begin
  fCurvePoints := nil;
  fCountReference := 0;
  fSavingType := stTime;
end;

destructor TCurve2D.Destroy;
begin
  fCountReference := 0;
  FreeCurvePoints;
  inherited;
end;

procedure TCurve2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
    Exit;
  inherited;
  fSavingType := stTime;
  if Obj is TCurve2D then
  begin
    CurvePrecision := TCurve2D(Obj).fCurvePrecision;
    SavingType := TCurve2D(Obj).fSavingType;
  end;
end;

constructor TCurve2D.CreateFromStream(const Stream: TStream;
  const Version: TCADVersion);
begin
  inherited;
  with Stream do
  begin
    Read(fCurvePrecision, SizeOf(fCurvePrecision));
    Read(fSavingType, SizeOf(fSavingType));
    fCountReference := 0;
  end;
end;

procedure TCurve2D.SaveToStream(const Stream: TStream);
begin
  inherited;
  with Stream do
  begin
    Write(fCurvePrecision, SizeOf(fCurvePrecision));
    Write(fSavingType, SizeOf(fSavingType));
  end;
end;

procedure TCurve2D._UpdateExtension;
begin
  if not Assigned(fPoints) or (fPoints.Count = 0) then
    Exit;
  case fSavingType of
    stSpace:
      begin
        WritableBox := PopulateCurvePoints(0);
        FreeCurvePoints;
      end;
    stTime:
      begin
        FreeCurvePoints;
        WritableBox := PopulateCurvePoints(0);
      end;
  end;
end;

function TCurve2D.GetProfilePoints: TPointsSet2D;
begin
  if not Assigned(fCurvePoints) then
    raise
      ECADSysException.Create('TCurve2D: Call BeginUseProfilePoints before accessing the curve points.');
  Result := fCurvePoints;
end;

function TCurve2D.GetNPts: Integer;
begin
  if not Assigned(fCurvePoints) then
    raise
      ECADSysException.Create('TCurve2D: Call BeginUseProfilePoints before accessing the curve points.');
  Result := fCurvePoints.Count;
end;

procedure TCurve2D.BeginUseProfilePoints;
begin
  if fSavingType = stSpace then
    WritableBox := PopulateCurvePoints(0);
end;

procedure TCurve2D.EndUseProfilePoints;
begin
  if fSavingType = stSpace then
    FreeCurvePoints;
end;

// =====================================================================
// TStar2D
// =====================================================================

type

  TSimplePoint2D = array[1..2] of TRealType;
  TSimplePointArr2D = array[0..0] of TSimplePoint2D;
  PSimplePointArr2D = ^TSimplePointArr2D;
  //TSimplePointArr2D = array of TSimplePoint2D;

const
{starCircle, starSquare, starDiamond, starTriUp, starTriDown,
    starPenta, starStar4, starStar5, starStar6, starAster5, starCross,
    starDCross, starStar4Arc}
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

procedure FillStarPoly(PP: TPointsSet2D;
  const PArr: PSimplePointArr2D; const Count: Integer;
  const CP: TPoint2D; const R: TRealType);
var
  I: Integer;
begin
  for I := 0 to Pred(Count) do
    PP.Add(Point2D(CP.X + PArr^[I][1] * R, CP.Y + PArr^[I][2] * R));
end;

procedure FillCircleProfilePoints(PP: TPointsSet2D;
  const CP: TPoint2D; const R: TRealType; const CurvePrecision: Word);
var
  Cont: Integer;
  Delta, CurrAngle: TRealType;
begin
  Delta := TWOPI / CurvePrecision;
  CurrAngle := 0;
  PP.Clear;
  for Cont := 1 to CurvePrecision do
  begin
    PP.Add(
      Point2D(CP.X + R * Cos(CurrAngle),
      CP.Y + R * Sin(CurrAngle)));
    CurrAngle := CurrAngle + Delta;
  end;
end;

class function TStar2D.GetName: string;
begin
  Result := 'Star';
end;

function TStar2D.FillPieces: TRect2D;
var
  StarsSize: TRealType;
  APath: TPath;
begin
  Result := Rect2D(0, 0, 0, 0);
  if OwnerCAD is TDrawing2D then
    StarsSize := (OwnerCAD as TDrawing2D).StarsSize
  else StarsSize := 1;
  if Pieces = nil then
    Pieces := TArrayOfPiece.Create(1)
  else Pieces.Clear;
  APath := TPath.Create(30);
  Pieces.Add(APath);
  //APath.Line := pliFillAsDefault;
  APath.Line := pliSolidDefault;
  APath.Fill := pfiLineAsDefault;
  APath.Hatch := phaNone;
  case StarKind of
    starSquare: FillStarPoly(APath, @StarSquareArr,
        Length(StarSquareArr), fPoints[0], StarsSize * 0.9);
    starDiamond: FillStarPoly(APath, @StarDiamondArr,
        Length(StarDiamondArr), fPoints[0], StarsSize * 1.2);
    starTriUp: FillStarPoly(APath, @StarTriUpArr,
        Length(StarTriUpArr), fPoints[0], StarsSize * 1.1);
    starTriDown: FillStarPoly(APath, @StarTriDownArr,
        Length(StarTriDownArr), fPoints[0], StarsSize * 1.1);
    starPenta: FillStarPoly(APath, @StarPentaArr,
        Length(StarPentaArr), fPoints[0], StarsSize * 1.1);
    starStar4: FillStarPoly(APath, @StarStar4Arr,
        Length(StarStar4Arr), fPoints[0], StarsSize * 0.5);
    starStar5: FillStarPoly(APath, @StarStar5Arr,
        Length(StarStar5Arr), fPoints[0], StarsSize * 1.6);
    starStar6: FillStarPoly(APath, @StarStar6Arr,
        Length(StarStar6Arr), fPoints[0], StarsSize * 0.8);
    starCross: FillStarPoly(APath, @StarCrossArr,
        Length(StarCrossArr), fPoints[0], StarsSize * 0.4);
    starDCross: FillStarPoly(APath, @StarDCrossArr,
        Length(StarDCrossArr), fPoints[0], StarsSize * 0.4);
  else //starCircle, starAster5, starStar4Arc
    FillCircleProfilePoints(APath,
      fPoints[0], StarsSize, 30);
  end;
  APath.Add(APath[0]);
  Result := APath.Extension;
end;

procedure TStar2D._UpdateExtension;
begin
  if not Assigned(fPoints) or (fPoints.Count = 0) then
    WritableBox := Rect2D(0, 0, 0, 0)
  else WritableBox := FillPieces;
end;

constructor TStar2D.Create(ID: Longint; const P: TPoint2D);
begin
  inherited Create(ID, 50);
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
  StarKind := starCircle;
  LineKind := liThin;
  fPoints.GrowingEnabled := False;
end;

procedure TStar2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
    Exit;
  inherited Assign(Obj);
  if Obj is TPrimitive2D then
  begin
    if TPrimitive2D(Obj).fPoints.Count > 0 then
      fPoints.Copy(TPrimitive2D(Obj).fPoints, 0, 0);
    fPoints.GrowingEnabled := False;
  end;
  StarKind := starCircle;
  if (Obj is TStar2D) then
    StarKind := TStar2D(Obj).StarKind;
end;

constructor TStar2D.CreateFromStream(const Stream: TStream;
  const Version: TCADVersion);
begin
  inherited;
  with Stream do
  begin
    Read(StarKind, SizeOf(StarKind));
  end;
end;

procedure TStar2D.SaveToStream(const Stream: TStream);
begin
  inherited SaveToStream(Stream);
  with Stream do
  begin
    Write(StarKind, SizeOf(StarKind));
  end;
end;

procedure TStar2D.Draw(VT: TTransf2D; const Cnv:
  TDecorativeCanvas; const ClipRect2D: TRect2D; const
  DrawMode: Integer);
begin
  FillPieces;
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
  P3 := Point2D(A * P2.X + (1 - A) * P0.X,
    A * P2.Y + (1 - A) * P0.Y);
  P4 := Point2D(P0.X + P1.X - P3.X,
    P0.Y + P1.Y - P3.Y);
end;

function CalcRotationAngle(P0, P1: TPoint2D): Double;
begin
  Result := ArcTan2(P1.X - P0.X, P1.Y - P0.Y);
  if Result < 0 then Result := Result + Pi;
  if (Result < 1E-9) or (Result > Pi - 1E-9) then
    Result := 0;
end;

procedure TBox2D0.PolyPoints(var PP: TPointsSet2D;
  T: TTransf2D);
var
  A: TRealType;
  P0, P1, P2, P3, P4: TPoint2D;
begin
  PP.Free;
  PP := TPointsSet2D.Create(4);
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

function TBox2D0.PopulateCurvePoints(N: Word): TRect2D;
var
  A: TRealType;
  P0, P1, P2, P3, P4: TPoint2D;
begin
  if CurvePrecision = 0 then
  begin
    Result := Rect2D(0, 0, 0, 0);
    Exit;
  end;

  Result := inherited PopulateCurvePoints(CurvePrecision);

  if Self is TEllipse2D then Exit;

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
  if (A <> 1) and (A <> 0) then
    fPoints[2] := P3;
  ProfilePoints.Clear;
  ProfilePoints.Add(P0);
  ProfilePoints.Add(P3);
  ProfilePoints.Add(P1);
  ProfilePoints.Add(P4);
  Result := ProfilePoints.Extension;
end;

constructor TBox2D0.Create(ID: Longint);
begin
  inherited Create(ID, 3, 150);
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
begin
  if (Obj = Self) then
    Exit;
  inherited Assign(Obj);
  fIsClosed := True;
  if Obj is TPrimitive2D then
  begin
    if TPrimitive2D(Obj).fPoints.Count > 2 then
      fPoints.Copy(TPrimitive2D(Obj).fPoints, 0, 2);
    fPoints.GrowingEnabled := False;
  end;
end;

// =====================================================================
// TRectangle2D
// =====================================================================

class function TRectangle2D.GetName: string;
begin
  Result := 'Rectangle';
end;

constructor TRectangle2D.Create(ID: Longint);
begin
  inherited Create(ID);
  WhenCreated;
end;

procedure TRectangle2D.WhenCreated;
begin
  fDrawPathBezier := False;
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
  BeginUseProfilePoints;
  try
    SetLength(Pnts, ProfilePoints.Count);
    for I := 0 to ProfilePoints.Count - 1 do
      Pnts[I] := Point2DToPoint(
        TransformPoint2D(ProfilePoints[I], VT));
  finally
    EndUseProfilePoints;
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

procedure TEllipse2D.GetEllipseParams(
  var CX, CY, RX, RY, ARot: TRealType);
var
  P0, P1, P2, P3, P4: TPoint2D;
  A: TRealType;
begin
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
  if (A <> 1) and (A <> 0) then
    fPoints[2] := P3;
  RX := PointDistance2D(P1, P3) / 2.0;
  RY := PointDistance2D(P0, P3) / 2.0;
  CX := (P1.X + P0.X) / 2.0;
  CY := (P1.Y + P0.Y) / 2.0;
  ARot := CalcRotationAngle(P0, P2);
end;

function TEllipse2D.PopulateCurvePoints(N: Word): TRect2D;
var
  Cont: Integer;
  CP, P: TPoint2D;
  Delta, CurrAngle, RX, RY, ARot: TRealType;
begin
  Result := inherited PopulateCurvePoints(CurvePrecision + 1);

  GetEllipseParams(CP.X, CP.Y, RX, RY, ARot);
  Delta := TWOPI / CurvePrecision;
  CurrAngle := 0;
  ProfilePoints.Clear;
  for Cont := 1 to CurvePrecision do
  begin
    P := Point2D(CP.X + RX * Cos(CurrAngle),
      CP.Y - RY * Sin(CurrAngle));
    P := TransformPoint2D(P, RotateCenter2D(-ARot, CP));
    ProfilePoints.Add(P);
    CurrAngle := CurrAngle + Delta;
  end;
  Result := ProfilePoints.Extension;
end;

const
  Ell_Cnst = 0.265206;
  ISqrt2 = 0.707106781186547; // 1/sqrt(2)

procedure EllipseBezierPoints(CP: TPoint2D;
  RX, RY, ARot: TRealType; var PP: TPointsSet2D;
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
  PP.Free;
  PP := TPointsSet2D.Create(25);
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

procedure TEllipse2D.BezierPoints(var PP: TPointsSet2D;
  T: TTransf2D);
var
  CP: TPoint2D;
  RX, RY, ARot: TRealType;
begin
  GetEllipseParams(CP.X, CP.Y, RX, RY, ARot);
  EllipseBezierPoints(CP, RX, RY, ARot, PP, T);
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
  PP := nil;
  try
    BezierPoints(PP, VT);
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

function TCircle2D.PopulateCurvePoints(N: Word): TRect2D;
var
  R: TRealType;
  CP: TPoint2D;
begin
  if CurvePrecision = 0 then
  begin
    Result := Rect2D(0, 0, 0, 0);
    Exit;
  end;
  Result := inherited PopulateCurvePoints(CurvePrecision + 1);
  CP := fPoints[0];
  R := PointDistance2D(CP, fPoints[1]);
  FillCircleProfilePoints(ProfilePoints,
    CP, R, CurvePrecision);
  Result := ProfilePoints.Extension;
end;

constructor TCircle2D.Create(ID: Longint);
begin
  inherited Create(ID, 2, 150);
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
end;

procedure TCircle2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
    Exit;
  inherited Assign(Obj);
  if Obj is TPrimitive2D then
  begin
    if TPrimitive2D(Obj).fPoints.Count > 1 then
      fPoints.Copy(TPrimitive2D(Obj).fPoints, 0, 1);
    fPoints.GrowingEnabled := False;
  end;
end;

procedure TCircle2D.BezierPoints(var PP: TPointsSet2D;
  T: TTransf2D);
var
  R: TRealType;
  CP: TPoint2D;
begin
  CP := fPoints[0];
  R := PointDistance2D(CP, fPoints[1]);
  EllipseBezierPoints(CP, R, R, 0, PP, T);
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
{var
  P0, P1: TPoint;
  R: TRealType;
  CP: TPoint2D;}
var
  PP: TPointsSet2D;
  Pnts: array of TPoint;
  I: Integer;
begin
  {if not (Cnv.Canvas is TMetaFileCanvas) then
  begin
    inherited;
    Exit;
  end;}
  {CP := TransformPoint2D(fPoints[0], VT);
  R := PointDistance2D(CP, TransformPoint2D(fPoints[1], VT));
  P0 := Point2DToPoint(Point2D(CP.X - R, CP.Y - R));
  P1 := Point2DToPoint(Point2D(CP.X + R, CP.Y + R));
  DrawNative(VT, Cnv, ClipRect2D, DrawMode,
    IsClosed, [P0, P1]);}
  PP := nil;
  try
    BezierPoints(PP, VT);
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

procedure SmallArcBezier(CP: TPoint2D; R, SA, EA: TRealType;
  var P0, P1, P2, P3: TPoint2D);
var
  AA, BB: TRealType;
  S: TPoint2D;
begin // cubic Bezier approximation to a small circular arc
  BB := 1 / Cos((EA - SA) / 2);
  AA := 4 / (3 * (BB + 1)); // approximately 0.65
  P0 := Point2D(CP.X + R * Cos(SA), CP.Y + R * Sin(SA));
  P3 := Point2D(CP.X + R * Cos(EA), CP.Y + R * Sin(EA));
  S := MidPoint(P0, P3);
  S := MixPoint(CP, S, Sqr(BB));
  P1 := MixPoint(P0, S, AA);
  P2 := MixPoint(P3, S, AA);
end;

procedure ArcBezier(CP: TPoint2D; R, SA, EA: TRealType;
  var PP: TPointsSet2D);
var
  Angle, Delta, AA, BB: TRealType;
  I, NArcs: Integer;
  S, P0, P3: TPoint2D;
begin // Bezier path approximation to a circular arc
  Angle := EA - SA - Floor((EA - SA) / (2 * Pi)) * (2 * Pi);
  NArcs := Ceil(Angle * 10 / Pi);
  if NArcs = 0 then Exit;
  Delta := Angle / NArcs;
  PP.Free;
  PP := TPointsSet2D.Create(NArcs * 3 + 1);
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

procedure TCircular2D.GetArcPoints(PP: TPointsSet2D; NPts:
  Word);
var
  Cont: Integer;
  CX, CY, R: TRealType;
  Delta, CurrAngle: TRealType;
begin
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
    ProfilePoints.Add(Point2D(CX + R * Cos(CurrAngle),
      CY + R * Sin(CurrAngle)));
    CurrAngle := CurrAngle + Delta
  end;
end;

function TCircular2D.PopulateCurvePoints(N: Word): TRect2D;
begin
  if CurvePrecision = 0 then
  begin
    Result := Rect2D(0, 0, 0, 0);
    Exit;
  end;
  if Self is TSector2D then
    Result := inherited PopulateCurvePoints(CurvePrecision + 1)
  else
    Result := inherited PopulateCurvePoints(CurvePrecision);
  GetArcPoints(ProfilePoints, CurvePrecision);
  if Self is TSector2D then ProfilePoints.Add(fPoints[0]);
  Result := ProfilePoints.Extension;
end;

{ Angles are in radiants. }

constructor TCircular2D.Create(ID: Longint; const CP: TPoint2D;
  R, SA, EA: TRealType);
begin
  inherited Create(ID, 3, 150);
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
end;

procedure TCircular2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
    Exit;
  inherited Assign(Obj);
  fOwnsInterior := False;
  if Obj is TPrimitive2D then
  begin
    if TPrimitive2D(Obj).fPoints.Count > 2 then
      fPoints.Copy(TPrimitive2D(Obj).fPoints, 0, 2);
    fPoints.GrowingEnabled := False;
  end;
  if Obj is TCircular2D then
  begin
    FStartAngle := (Obj as TCircular2D).StartAngle;
    FEndAngle := (Obj as TCircular2D).EndAngle;
  end;
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

procedure TCircular2D.BezierPoints(var PP: TPointsSet2D;
  T: TTransf2D);
var
  CP: TPoint2D;
  R, SA, EA: TRealType;
  I: Integer;
begin
  GetArcParams(CP.X, CP.Y, R, SA, EA);
  ArcBezier(CP, R, SA, EA, PP);
  if PP = nil then Exit;
  {if Self is TSegment2D then
    PP.AddPoints([fPoints[2], fPoints[1], fPoints[1]]);}
  if Self is TSector2D then
  begin
    PP.AddPoints([PP[PP.Count - 1]{fPoints[2]}, fPoints[0], fPoints[0]{,
      fPoints[0], fPoints[1], fPoints[1]}]);
  end;
  for I := 0 to PP.Count - 1 do
    PP[I] := TransformPoint2D(PP[I], T);
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
{  CP: TPoint2D;
  PC, P0, P1, P2, P3: TPoint;
  R: TRealType;}
begin
  {if not (Cnv.Canvas is TMetaFileCanvas) then
  begin
    inherited;
    Exit;
  end;}
  {CP := TransformPoint2D(fPoints[0], VT);
  R := PointDistance2D(CP, TransformPoint2D(fPoints[1], VT));
  P0 := Point2DToPoint(Point2D(CP.X - R, CP.Y - R));
  P1 := Point2DToPoint(Point2D(CP.X + R, CP.Y + R));
  P2 := Point2DToPoint(
    TransformPoint2D(fPoints[1], VT));
  P3 := Point2DToPoint(TransformPoint2D(fPoints[2], VT));
  if Self is TSector2D then PC := Point2DToPoint(CP);
  DrawNative(VT, Cnv, ClipRect2D, DrawMode,
    IsClosed, [P0, P1, P2, P3, PC]);}
  PP := nil;
  try
    BezierPoints(PP, VT);
    if PP <> nil then
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

constructor TSector2D.Create(ID: Longint; const CP: TPoint2D;
  R, SA, EA: TRealType);
begin
  inherited Create(ID, CP, R, SA, EA);
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

constructor TSegment2D.Create(ID: Longint; const CP:
  TPoint2D;
  R, SA, EA: TRealType);
begin
  inherited Create(ID, CP, R, SA, EA);
  WhenCreated;
end;

procedure TSegment2D.WhenCreated;
begin
  fIsClosed := True;
  fOwnsInterior := True;
end;

// =====================================================================
// TSpline2D0
// =====================================================================

function TSpline2D0.GetPoint(I: Integer): TPoint2D;
var
  N: Integer;
begin
  N := fPoints.Count;
  I := I - Floor(I / N) * N;
  Result := fPoints[I];
end;

constructor TSpline2D0.CreateFromStream(const Stream: TStream;
  const Version: TCADVersion);
begin
  { Load the standard properties }
  inherited;
  with Stream do
   { Load the particular properties. }
    Read(fOrder, SizeOf(fOrder));
end;

procedure TSpline2D0.SaveToStream(const Stream: TStream);
begin
  { Save the standard properties }
  inherited SaveToStream(Stream);
  with Stream do
    Write(fOrder, SizeOf(fOrder));
end;

procedure TSpline2D0.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
    Exit;
  inherited Assign(Obj);
  if (Obj is TSpline2D0) then
  begin
    fOrder := TSpline2D0(Obj).fOrder;
  end;
  if Obj is TPrimitive2D then
  begin
    if (Obj is TOutline2D) and not (Obj is TSpline2D0) then
    begin
      BeginUseProfilePoints;
      try
        fPoints.Copy(TOutline2D(Obj).ProfilePoints, 0,
          TOutline2D(Obj).ProfilePoints.Count - 1)
      finally
        EndUseProfilePoints;
      end;
    end
    else
      fPoints.Copy(TPrimitive2D(Obj).fPoints, 0,
        TPrimitive2D(Obj).fPoints.Count - 1);
    fPoints.GrowingEnabled := True;
  end;
end;

{procedure TSpline2D0.DrawPath(const Canvas: TCanvas;
  const Pnts: array of TPoint);
begin
  if High(Pnts) < 1 then Exit;
  BeginPath(Canvas.Handle);
  MoveToEx(Canvas.Handle, Pnts[0].X, Pnts[0].Y, nil);
  PolyBezierTo(Canvas.Handle, Pnts[1], High(Pnts));
  if fIsClosed then CloseFigure(Canvas.Handle);
  EndPath(Canvas.Handle);
end;}

procedure TSpline2D0.Draw(VT: TTransf2D; const Cnv:
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
  if fPoints.Count < fOrder then
  begin
    inherited;
    Exit;
  end;
  PP := nil;
  try
    BezierPoints(PP, VT);
    SetLength(Pnts, PP.Count);
    for I := 0 to PP.Count - 1 do
      Pnts[I] := Point2DToPoint(PP[I]);
  finally
    PP.Free;
  end;
  DrawNative(VT, Cnv, ClipRect2D, DrawMode, IsClosed, Pnts);
end;

// =====================================================================
// TBSpline2D0
// =====================================================================

function Knot(I, K, N: Integer): Integer;
begin
  if I < K then
    Result := 0
  else if I > N then
    Result := N - K + 2
  else
    Result := I - K + 1;
end;

function NBlend(I, K, OK, N: Integer; U: TRealType):
  TRealType;
var
  T: Integer;
begin
  if K = 1 then
  begin
    Result := 0;
    if (Knot(I, OK, N) <= U) and (U < Knot(I + 1, OK, N)) then
      Result := 1;
  end
  else
  begin
    Result := 0;
    T := Knot(I + K - 1, OK, N) - Knot(I, OK, N);
    if T <> 0 then
      Result := (U - Knot(I, OK, N)) *
        NBlend(I, K - 1, OK, N, U) / T;
    T := Knot(I + K, OK, N) - Knot(I + 1, OK, N);
    if T <> 0 then
      Result := Result + (Knot(I + K, OK, N) - U) *
        NBlend(I + 1, K - 1, OK, N, U) / T;
  end;
end;

function BSpline2D(U: TRealType; N, K: Integer; const Points:
  TPointsSet2D): TPoint2D;
var
  I: Integer;
  B: TRealType;
  TmpPt: TPoint2D;
begin
  Result := Point2D(0.0, 0.0);
  for I := 0 to N do
  begin
    B := NBlend(I, K, K, N, U);
    TmpPt := Points[I];
    Result.X := Result.X + TmpPt.X * B;
    Result.Y := Result.Y + TmpPt.Y * B;
  end;
end;

function TBSpline2D0.PopulateCurvePoints(N: Word): TRect2D;
var
  Cont: Integer;
begin
  if CurvePrecision = 0 then
  begin
    Result := Rect2D(0, 0, 0, 0);
    Exit;
  end;
  if fPoints.Count < fOrder then
  begin
    Result := inherited PopulateCurvePoints(fPoints.Count);
    ProfilePoints.Copy(fPoints, 0, fPoints.Count - 1);
  end
  else
  begin
    Result := inherited PopulateCurvePoints(
      CurvePrecision * Pred(fPoints.Count) + 1);
    for Cont := 0 to
      CurvePrecision * Pred(fPoints.Count) - 1 do
      ProfilePoints.Add(BSpline2D(Cont /
        (CurvePrecision * Pred(fPoints.Count)) *
        (fPoints.Count - fOrder + 1),
        fPoints.Count - 1, fOrder, fPoints));
    ProfilePoints.Add(fPoints[fPoints.Count - 1]);
  end;
  Result := ProfilePoints.Extension;
end;

constructor TBSpline2D0.Create(ID: Longint; const Pts: array of
  TPoint2D);
begin
  inherited Create(ID, High(Pts) - Low(Pts) + 1, 20);
  fPoints.AddPoints(Pts);
  WhenCreated;
end;

procedure TBSpline2D0.WhenCreated;
begin
  fOrder := 3; { The default spline is cubic. (quadratic?) }
  fPoints.GrowingEnabled := True;
  fOwnsInterior := False;
  fCanDeletePoints := True;
end;

function TBSpline2D0.OnProfile(PT: TPoint2D; Aperture:
  TRealType): Integer;
begin
  Result := inherited OnProfile(PT, Aperture);
  Result := Result div CurvePrecision;
end;

// =====================================================================
// TBSpline2D
// =====================================================================

class function TBSpline2D.GetName: string;
begin
  Result := 'Quadratic spline';
end;

function TBSpline2D.GetPoint(I: Integer): TPoint2D;
begin
  if I < 0 then I := 0;
  if I > Pred(fPoints.Count) then I := Pred(fPoints.Count);
  if I = 0 then
    Result := MixPoint(fPoints[1], fPoints[0], 2)
  else if I = Pred(fPoints.Count) then
    Result := MixPoint(fPoints[Pred(Pred(fPoints.Count))],
      fPoints[Pred(fPoints.Count)], 2)
  else Result := fPoints[I];
end;

procedure TBSpline2D.BezierPoints(var PP: TPointsSet2D; T:
  TTransf2D);
var
  P0, P1, P2, Q0, Q1, Q2, Q3: TPoint2D;
  I: Integer;
begin
  PP.Free;
  PP := TPointsSet2D.Create(1 + fPoints.Count * 3);
  for I := 0 to fPoints.Count - 3 do
  begin
    P0 := TransformPoint2D(GetPoint(I), T);
    P1 := TransformPoint2D(GetPoint(I + 1), T);
    P2 := TransformPoint2D(GetPoint(I + 2), T);
    Q0 := MidPoint(P0, P1);
    Q1 := MixPoint(P0, P1, 5 / 6);
    Q2 := MixPoint(P2, P1, 5 / 6);
    Q3 := MidPoint(P2, P1);
    if I = 0 then PP.Add(Q0);
    PP.Add(Q1);
    PP.Add(Q2);
    PP.Add(Q3);
  end;
end;

// =====================================================================
// TCubicBSpline2D
// =====================================================================

class function TCubicBSpline2D.GetName: string;
begin
  Result := 'Cubic spline';
end;

constructor TCubicBSpline2D.Create(ID: Longint;
  const Pts: array of TPoint2D);
begin
  inherited Create(ID, Pts);
  WhenCreated;
end;

procedure TCubicBSpline2D.WhenCreated;
begin
  fOrder := 4;
end;

function TCubicBSpline2D.GetPoint(I: Integer): TPoint2D;
begin
  if I < 0 then I := 0;
  if I > Pred(fPoints.Count) then I := Pred(fPoints.Count);
  if I = 0 then
  begin
    Result := MixPoint(fPoints[1], fPoints[0], 2);
    Result := MixPoint(GetPoint(1), Result, 3);
  end
  else if I = 1 then
  begin
    if fPoints.Count = 4 then
      Result := MixPoint(fPoints[2], fPoints[1], 2)
    else
      Result := MixPoint(fPoints[2], fPoints[1], 3 / 2);
  end
  else if I = fPoints.Count - 2 then
  begin
    if fPoints.Count = 4 then
      Result := MixPoint(fPoints[fPoints.Count - 3],
        fPoints[fPoints.Count - 2], 2)
    else
      Result := MixPoint(fPoints[fPoints.Count - 3],
        fPoints[fPoints.Count - 2], 3 / 2);
  end
  else if I = Pred(fPoints.Count) then
  begin
    Result := MixPoint(fPoints[fPoints.Count - 2],
      fPoints[Pred(fPoints.Count)], 2);
    Result := MixPoint(GetPoint(fPoints.Count - 2), Result, 3);
  end
  else Result := fPoints[I];
end;

procedure TCubicBSpline2D.BezierPoints(var PP:
  TPointsSet2D; T: TTransf2D);
var
  P0, P1, P2, P3, Q0, Q1, Q2, Q3, R1, R2: TPoint2D;
  I: Integer;
begin
  PP.Free;
  PP := TPointsSet2D.Create(1 + fPoints.Count * 3);
  for I := 0 to fPoints.Count - 4 do
  begin
    P0 := TransformPoint2D(GetPoint(I), T);
    P1 := TransformPoint2D(GetPoint(I + 1), T);
    P2 := TransformPoint2D(GetPoint(I + 2), T);
    P3 := TransformPoint2D(GetPoint(I + 3), T);
    Q0 := MixPoint(P0, P1, 2 / 3);
    Q1 := MixPoint(P2, P1, 2 / 3);
    Q2 := MixPoint(P1, P2, 2 / 3);
    Q3 := MixPoint(P3, P2, 2 / 3);
    R1 := MidPoint(Q0, Q1);
    R2 := MidPoint(Q2, Q3);
    if I = 0 then PP.Add(R1);
    PP.Add(Q1);
    PP.Add(Q2);
    PP.Add(R2);
  end;
end;

// =====================================================================
// TClosedBSpline2D
// =====================================================================

function BKernel(K: Integer; Z: TRealType): TRealType;
var
  FR: TRealType;
begin
  if (Z < 0) or (Z >= K) then
  begin
    Result := 0;
    Exit;
  end;
  if K = 1 then
  begin
    Result := 1;
    Exit;
  end;
  Result := (Z * BKernel(K - 1, Z) +
    (K - Z) * BKernel(K - 1, Z - 1)) / (K - 1);
end;

function ClosedBSpline2D(U: TRealType; N, K: Integer; const
  Points: TPointsSet2D): TPoint2D;
var
  I, J: Integer;
  B: TRealType;
  TmpPt: TPoint2D;
begin
  Result := Point2D(0.0, 0.0);
  for I := Ceil(U - K / 2) to Ceil(U + K / 2) - 1 do
  begin
    B := BKernel(K, K / 2 + I - U);
    J := I - Floor(I / (N + 1)) * (N + 1);
    TmpPt := Points[J];
    Result.X := Result.X + TmpPt.X * B;
    Result.Y := Result.Y + TmpPt.Y * B;
  end;
end;

function TClosedBSpline2D0.PopulateCurvePoints(N: Word):
  TRect2D;
var
  Cont: Integer;
begin
  if CurvePrecision = 0 then
  begin
    Result := Rect2D(0, 0, 0, 0);
    Exit;
  end;
  if Points.Count < fOrder then
  begin
    Result := inherited PopulateCurvePoints(Points.Count);
    ProfilePoints.Copy(Points, 0, Points.Count - 1);
  end
  else
  begin
    Result := inherited PopulateCurvePoints(
      CurvePrecision * Points.Count);
    for Cont := 0 to CurvePrecision * Points.Count - 1 do
      ProfilePoints.Add(ClosedBSpline2D(Cont /
        (CurvePrecision * Points.Count) *
        (Points.Count),
        Points.Count - 1, fOrder, Points));
  end;
  Result := ProfilePoints.Extension;
end;

constructor TClosedBSpline2D0.Create(ID: Longint; const Pts:
  array
  of TPoint2D);
begin
  inherited Create(ID, High(Pts) - Low(Pts) + 1, 20);
  Points.AddPoints(Pts);
  WhenCreated;
end;

procedure TClosedBSpline2D0.WhenCreated;
begin
  fOrder := 3; { The default spline is cubic. (quadratic?) }
  Points.GrowingEnabled := True;
  fIsClosed := True;
  fOwnsInterior := False;
  fCanDeletePoints := True;
end;

function TClosedBSpline2D0.OnProfile(PT: TPoint2D; Aperture:
  TRealType): Integer;
begin
  Result := inherited OnProfile(PT, Aperture);
  Result := Result div CurvePrecision;
end;

procedure TClosedBSpline2D0.BezierPoints(var PP: TPointsSet2D;
  T: TTransf2D);
var
  P0, P1, P2, Q0, Q1, Q2, Q3: TPoint2D;
  I: Integer;
begin
  PP.Free;
  PP := TPointsSet2D.Create(1 + fPoints.Count * 3);
  for I := 0 to fPoints.Count - 1 do
  begin
    P0 := TransformPoint2D(GetPoint(I), T);
    P1 := TransformPoint2D(GetPoint(I + 1), T);
    P2 := TransformPoint2D(GetPoint(I + 2), T);
    Q0 := MidPoint(P0, P1);
    Q1 := MixPoint(P0, P1, 5 / 6);
    Q2 := MixPoint(P2, P1, 5 / 6);
    Q3 := MidPoint(P2, P1);
    if I = 0 then PP.Add(Q0);
    PP.Add(Q1);
    PP.Add(Q2);
    PP.Add(Q3);
  end;
end;

// =====================================================================
// TClosedBSpline2D
// =====================================================================

class function TClosedBSpline2D.GetName: string;
begin
  Result := 'Closed quadratic spline';
end;

// =====================================================================
// TClosedCubicBSpline2D
// =====================================================================

class function TClosedCubicBSpline2D.GetName: string;
begin
  Result := 'Closed cubic spline';
end;

constructor TClosedCubicBSpline2D.Create(ID: Longint;
  const Pts: array of TPoint2D);
begin
  inherited Create(ID, Pts);
  WhenCreated;
end;

procedure TClosedCubicBSpline2D.WhenCreated;
begin
  fOrder := 4;
end;

procedure TClosedCubicBSpline2D.BezierPoints(var PP:
  TPointsSet2D;
  T: TTransf2D);
var
  P0, P1, P2, P3, Q0, Q1, Q2, Q3, R1, R2: TPoint2D;
  I: Integer;
begin
  PP.Free;
  PP := TPointsSet2D.Create(1 + fPoints.Count * 3);
  for I := 0 to fPoints.Count - 1 do
  begin
    P0 := TransformPoint2D(GetPoint(I), T);
    P1 := TransformPoint2D(GetPoint(I + 1), T);
    P2 := TransformPoint2D(GetPoint(I + 2), T);
    P3 := TransformPoint2D(GetPoint(I + 3), T);
    Q0 := MixPoint(P0, P1, 2 / 3);
    Q1 := MixPoint(P2, P1, 2 / 3);
    Q2 := MixPoint(P1, P2, 2 / 3);
    Q3 := MixPoint(P3, P2, 2 / 3);
    R1 := MidPoint(Q0, Q1);
    R2 := MidPoint(Q2, Q3);
    if I = 0 then PP.Add(R1);
    PP.Add(Q1);
    PP.Add(Q2);
    PP.Add(R2);
  end;
end;

// =====================================================================
// TSmoothPath2D
// =====================================================================

function BezierPoint(const P0, P1, P2, P3: TPoint2D;
  const U: TRealType): TPoint2D;
begin
  Result := Point2D(
    Sqr(1 - U) * (P0.X * (1 - U) + P1.X * 3 * U)
    + Sqr(U) * (P2.X * 3 * (1 - U) + P3.X * U),
    Sqr(1 - U) * (P0.Y * (1 - U) + P1.Y * 3 * U)
    + Sqr(U) * (P2.Y * 3 * (1 - U) + P3.Y * U));
end;

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

function TSmoothPath2D0.PopulateCurvePoints(N: Word): TRect2D;
var
  I: Integer;
  PP: TPointsSet2D;
begin
  if CurvePrecision = 0 then
  begin
    Result := Rect2D(0, 0, 0, 0);
    Exit;
  end;
  if fPoints.Count < 2 then
  begin
    Result := inherited PopulateCurvePoints(fPoints.Count);
    ProfilePoints.Copy(fPoints, 0, fPoints.Count - 1);
  end
  else
  begin
    Result := inherited PopulateCurvePoints(
      CurvePrecision * fPoints.Count + 1);
    PP := nil;
    try
      BezierPoints(PP, IdentityTransf2D);
      for I := 0 to fPoints.Count - 2 + Byte(fIsClosed) do
        AddBezierArc(ProfilePoints, CurvePrecision,
          PP[I * 3], PP[I * 3 + 1],
          PP[I * 3 + 2], PP[I * 3 + 3]);
    finally
      PP.Free;
    end;
    if fIsClosed then
      ProfilePoints.Add(fPoints[0])
    else
      ProfilePoints.Add(fPoints[fPoints.Count - 1]);
  end;
  Result := ProfilePoints.Extension;
end;

constructor TSmoothPath2D0.Create(ID: Longint;
  const Pts: array of TPoint2D);
begin
  inherited Create(ID, High(Pts) - Low(Pts) + 1, 20);
  fPoints.AddPoints(Pts);
  WhenCreated;
end;

procedure TSmoothPath2D0.WhenCreated;
begin
  fPoints.GrowingEnabled := True;
  fOwnsInterior := False;
  fCanDeletePoints := True;
end;

procedure GetHobbyBezier(var PP: TPointsSet2D;
  const Points: TPointsSet2D);
var
  I, N: Integer;
  Len, A, B, C, D, S, T, Theta, Phi, Psi: array of TRealType;
  Gamm, Rho, Sigma, Alph, C0: TRealType;
  P1: TPoint2D;
  TT: TTransf2D;
begin
  N := Points.Count - 1;
  SetLength(Len, N);
  for I := 0 to N - 1 do
    Len[I] := PointDistance2D(Points[I + 1], Points[I]);
  SetLength(Psi, N);
  for I := 1 to N - 1 do
  begin
    Psi[I] :=
      ArcTan2(Points[I + 1].Y - Points[I].Y,
      Points[I + 1].X - Points[I].X)
      - ArcTan2(Points[I].Y - Points[I - 1].Y,
      Points[I].X - Points[I - 1].X);
    if Psi[I] >= Pi then Psi[I] := Psi[I] - 2 * Pi
    else if Psi[I] < -Pi then Psi[I] := Psi[I] + 2 * Pi;
  end;
  SetLength(A, N - 1);
  SetLength(B, N - 1);
  SetLength(C, N - 1);
  SetLength(D, N - 1);
  SetLength(S, N);
  SetLength(T, N);
  if N > 2 then
  begin
    if Len[1] > 0 then
      C[0] := Len[0] / Len[1] else C[0] := 1;
    S[1] := 1 + 2 * C[0];
    T[1] := Psi[1] + Psi[2] * C[0];
    A[N - 2] := 1;
    if Len[N - 1] > 0 then
      B[N - 2] := 2 + Len[N - 2] / Len[N - 1] else
      B[N - 2] := 3;
    C[N - 2] := 0;
    D[N - 2] := 2 * Psi[N - 1];
  end
  else
  begin
    if Len[1] > 0 then
      S[1] := 1 + Len[0] / Len[1] else S[1] := 2;
    T[1] := Psi[1];
  end;
  for I := 1 to N - 3 do
  begin
    A[I] := 1;
    if Len[I + 1] > 0 then
      C[I] := Len[I] / Len[I + 1] else C[I] := 1;
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
    Theta[I] := -(T[I] + C[I - 1] * Theta[I + 1]) / S[I];
  Phi[N] := Theta[N - 1];
  for I := 1 to N - 1 do Phi[I] := -Psi[I] - Theta[I];
  Theta[0] := Phi[1];
  TT := IdentityTransf2D;
  for I := 0 to N - 1 do
  begin
    Alph := Sqrt(2) * (Sin(Theta[I]) - Sin(Phi[I + 1]) / 16)
      * (Sin(Phi[I + 1]) - Sin(Theta[I]) / 16)
      * (Cos(Theta[I]) - Cos(Phi[I + 1]));
    C0 := (3 - Sqrt(5)) / 2;
    Rho := (2 + Alph) /
      (1 + (1 - C0) * Cos(Theta[I]) + C0 * Cos(Phi[I + 1]));
    Sigma := (2 - Alph) /
      (1 + (1 - C0) * Cos(Phi[I + 1]) + C0 * Cos(Theta[I]));
    TT[1, 1] := Points[I + 1].X - Points[I].X;
    TT[2, 1] := Points[I].Y - Points[I + 1].Y;
    TT[3, 1] := Points[I].X;
    TT[1, 2] := Points[I + 1].Y - Points[I].Y;
    TT[2, 2] := Points[I + 1].X - Points[I].X;
    TT[3, 2] := Points[I].Y;
    if I = 0 then PP[0] := Points[0];
    PP[I * 3 + 1] :=
      TransformPoint2D(Point2D(Rho / 3 * Cos(Theta[I]),
      Rho / 3 * Sin(Theta[I])), TT);
    PP[I * 3 + 2] :=
      TransformPoint2D(Point2D(1 - Sigma / 3 * Cos(Phi[I + 1]),
      Sigma / 3 * Sin(Phi[I + 1])), TT);
    PP[I * 3 + 3] := Points[I + 1];
  end;
end;

procedure TSmoothPath2D0.BezierPoints(var PP: TPointsSet2D; T:
  TTransf2D);
var
  I: Integer;
begin
  PP.Free;
  PP := TPointsSet2D.Create(fPoints.Count * 3 - 2);
  if fPoints.Count = 2 then
  begin
    PP[0] := fPoints[0];
    PP[1] := fPoints[0];
    PP[2] := fPoints[1];
    PP[3] := fPoints[1];
  end
  else GetHobbyBezier(PP, fPoints);
  for I := 0 to PP.Count - 1 do
    PP[I] := TransformPoint2D(PP[I], T);
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
begin
  if (Obj = Self) then
    Exit;
  inherited Assign(Obj);
  if Obj is TPrimitive2D then
  begin
    if (Obj is TOutline2D) and not (Obj is TSmoothPath2D0) then
    begin
      BeginUseProfilePoints;
      try
        {fPoints.Copy(TOutline2D(Obj).ProfilePoints, 0,
          TOutline2D(Obj).ProfilePoints.Count - 1)}
        fPoints.Copy(TOutline2D(Obj).fPoints, 0,
          TOutline2D(Obj).fPoints.Count - 1)
      finally
        EndUseProfilePoints;
      end;
    end
    else
      fPoints.Copy(TPrimitive2D(Obj).fPoints, 0,
        TPrimitive2D(Obj).fPoints.Count - 1);
    fPoints.GrowingEnabled := True;
    fOwnsInterior := False;
    fCanDeletePoints := True;
  end;
end;

procedure TSmoothPath2D0.Draw(VT: TTransf2D; const Cnv:
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
  if fPoints.Count < 2 then
  begin
    inherited;
    Exit;
  end;
  PP := nil;
  try
    BezierPoints(PP, VT);
    SetLength(Pnts, PP.Count);
    for I := 0 to PP.Count - 1 do
      Pnts[I] := Point2DToPoint(PP[I]);
  finally
    PP.Free;
  end;
  DrawNative(VT, Cnv, ClipRect2D, DrawMode, IsClosed, Pnts);
end;

function TSmoothPath2D0.OnProfile(PT: TPoint2D; Aperture:
  TRealType): Integer;
begin

  Result := inherited OnProfile(PT, Aperture);
  Result := Result div CurvePrecision;
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

constructor TClosedSmoothPath2D.Create(ID: Longint; const Pts:
  array of TPoint2D);
begin
  inherited Create(ID, Pts);
  WhenCreated;
end;

procedure TClosedSmoothPath2D.WhenCreated;
begin
  fIsClosed := True;
end;

procedure GetClosedHobbyBezier(var PP: TPointsSet2D;
  const Points: TPointsSet2D);
var
  I, N: Integer;
  Len, A, B, C, D, K, L, M, P, Q, R, Theta, Phi, Psi
    : array of TRealType;
  Alph, Bet, Rho, Sigma, C0: TRealType;
  P1: TPoint2D;
  TT: TTransf2D;
  function GetPoint(I: Integer): TPoint2D;
  begin
    if I < N then Result := Points[I]
    else Result := Points[I - N];
  end;
begin
  N := Points.Count;
  SetLength(Len, N + 2);
  for I := 0 to N - 1 do
    Len[I] := PointDistance2D(GetPoint(I + 1), Points[I]);
  Len[N] := Len[0];
  Len[N + 1] := Len[1];
  SetLength(Psi, N + 2);
  for I := 1 to N do
  begin
    Psi[I] :=
      ArcTan2(GetPoint(I + 1).Y - GetPoint(I).Y,
      GetPoint(I + 1).X - GetPoint(I).X)
      - ArcTan2(GetPoint(I).Y - GetPoint(I - 1).Y,
      GetPoint(I).X - GetPoint(I - 1).X);
    if Psi[I] >= Pi then Psi[I] := Psi[I] - 2 * Pi
    else if Psi[I] < -Pi then Psi[I] := Psi[I] + 2 * Pi;
  end;
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
      C[I] := Len[I] / Len[I + 1] else C[I] := 1;
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
    if L[I] = 0 then Theta[I + 1] := 0
    else Theta[I + 1] :=
      -(M[I] + K[I] * Theta[0] + C[I] * Theta[I + 2])
        / L[I];
  for I := 0 to N - 1 do Phi[I] := -Psi[I] - Theta[I];
  Phi[N] := Phi[0];
  TT := IdentityTransf2D;
  for I := 0 to N - 1 do
  begin
    Alph := Sqrt(2) * (Sin(Theta[I]) - Sin(Phi[I + 1]) / 16)
      * (Sin(Phi[I + 1]) - Sin(Theta[I]) / 16)
      * (Cos(Theta[I]) - Cos(Phi[I + 1]));
    C0 := (3 - Sqrt(5)) / 2;
    Rho := (2 + Alph) /
      (1 + (1 - C0) * Cos(Theta[I]) + C0 * Cos(Phi[I + 1]));
    Sigma := (2 - Alph) /
      (1 + (1 - C0) * Cos(Phi[I + 1]) + C0 * Cos(Theta[I]));
    TT[1, 1] := GetPoint(I + 1).X - Points[I].X;
    TT[2, 1] := Points[I].Y - GetPoint(I + 1).Y;
    TT[3, 1] := Points[I].X;
    TT[1, 2] := GetPoint(I + 1).Y - Points[I].Y;
    TT[2, 2] := GetPoint(I + 1).X - Points[I].X;
    TT[3, 2] := Points[I].Y;
    if I = 0 then PP[0] := Points[0];
    PP[I * 3 + 1] :=
      TransformPoint2D(Point2D(Rho / 3 * Cos(Theta[I]),
      Rho / 3 * Sin(Theta[I])), TT);
    PP[I * 3 + 2] :=
      TransformPoint2D(
      Point2D(1 - Sigma / 3 * Cos(Phi[I + 1]),
      Sigma / 3 * Sin(Phi[I + 1])), TT);
    PP[I * 3 + 3] := GetPoint(I + 1);
  end;
end;

procedure TClosedSmoothPath2D.BezierPoints(var PP:
  TPointsSet2D; T:
  TTransf2D);
var
  I: Integer;
begin
  PP.Free;
  PP := TPointsSet2D.Create(fPoints.Count * 3 + 1);
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
  for I := 0 to PP.Count - 1 do
    PP[I] := TransformPoint2D(PP[I], T);
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

constructor TText2D.Create(ID: Longint; P: TPoint2D; Height:
  TRealType; Txt: AnsiString);
begin
  inherited Create(ID, 2);
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
end;

procedure TText2D.WhenCreated;
begin
  fTeXText := '';
  fRecalcBox := False;
  fDrawBox := False;
  fExtFont := TExtendedFont.Create;
  fExtFont.FaceName := 'Times New Roman';
  fClippingFlags := 0;
  fPoints.GrowingEnabled := False;
end;

destructor TText2D.Destroy;
begin
  fExtFont.Free;
  inherited Destroy;
end;

function TText2D.GetExtension: TRect2D;
var
  X1, Y1, X2, Y2: TRealType;
  H, W: TRealType;
  function GetDimension(var H, W: TRealType): Boolean;
  var
    Cnv: TDecorativeCanvas;
    S: TSize;
    TmpH: Integer;
  begin
    Result := False;
    if not Assigned(OwnerCAD) then Exit;
    if OwnerCAD.ViewportsCount = 0 then Exit;
    Cnv := (Self.OwnerCAD.Viewports[0] as
      TCADViewport2D).OnScreenCanvas;
    {if fHeight > 7.5 then TmpH := Round(fHeight)
    else}
    TmpH := 100;
    fExtFont.Height := TmpH;
    fExtFont.Canvas := Cnv.Canvas;
    S := Cnv.Canvas.TextExtent(fText);
    W := S.CX / TmpH * fHeight;
    H := S.CY / TmpH * fHeight;
    Result := True;
  end;
begin
  if not GetDimension(H, W) then
  begin
    H := fHeight;
    W := fHeight * 2 / 3 * Length(fText);
  end;
  case fHJustification of
    jhLeft: X1 := fPoints[0].X;
    jhCenter: X1 := fPoints[0].X - W / 2;
    jhRight: X1 := fPoints[0].X - W;
  end;
  X2 := X1 + W;
  case fVJustification of
    jvTop: Y1 := fPoints[0].Y - H;
    jvCenter: Y1 := fPoints[0].Y - H / 2;
    jvBottom: Y1 := fPoints[0].Y;
  end;
  Y2 := Y1 + H;
  Result := Rect2D(X1, Y1, X2, Y2);
end;

procedure TText2D._UpdateExtension;
begin
  inherited;
  WritableBox := GetExtension;
end;

procedure TText2D.Draw(VT: TTransf2D; const Cnv:
  TDecorativeCanvas; const ClipRect2D: TRect2D; const
  DrawMode: Integer);
var
  TmpBox: TRect2D;
  TmpHeight: Integer;
  TmpRect: TRect;
  TmpTransf: TTransf2D;
begin
  Cnv.Canvas.Brush.Style := bsClear;
  { Find the correct size. }
  TmpBox.FirstEdge := Point2D(0, 0);
  TmpBox.SecondEdge := Point2D(0, fHeight);
  TmpBox := TransformRect2D(TmpBox, VT);
  TmpHeight := Round(PointDistance2D(TmpBox.FirstEdge,
    TmpBox.SecondEdge));
  if TmpHeight <= 0 then
    Exit;
  { Build up the DrawText rect. }
  TmpRect := Rect2DToRect(TransformRect2D(Box, VT));
  fExtFont.Canvas := Cnv.Canvas;
  if fLineColor <> clDefault then
    fExtFont.Canvas.Font.Color := fLineColor
  else fExtFont.Canvas.Font.Color := clBlack;
  try
    fExtFont.Height := TmpHeight;
    Windows.DrawText(Cnv.Canvas.Handle, PChar(fText),
      -1, TmpRect, fClippingFlags + DT_CALCRECT +
      DT_NOPREFIX);
    if (Cnv.Canvas.Pen.Mode <> pmNot) then
    begin
      Windows.DrawText(Cnv.Canvas.Handle, PChar(fText),
        -1, TmpRect, fClippingFlags + DT_NOPREFIX);
    end;
    if fDrawBox or (Cnv.Canvas.Pen.Mode = pmNot) then
      {Windows.DrawText(Cnv.Canvas.Handle, PChar(fText),
        Length(fText), TmpRect, fClippingFlags + DT_NOPREFIX);}
      DrawRect2DAsPolyline(Cnv, Box,
        RectToRect2D(Cnv.Canvas.ClipRect),
        IdentityTransf2D, VT);
  finally
    fExtFont.Canvas := nil;
  end;
end;

function TText2D.OnMe(PT: TPoint2D; Aperture: TRealType; var
  Distance: TRealType): Integer;
begin
  Result := inherited OnMe(PT, Aperture, Distance);
  if Result = PICK_INBBOX then
    Result := PICK_INOBJECT;
end;

constructor TText2D.CreateFromStream(const Stream: TStream;
  const
  Version: TCADVersion);
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
    Read(TmpInt, SizeOf(TmpInt));
    SetString(fTeXText, nil, TmpInt);
    Read(Pointer(fTeXText)^, TmpInt);
    fExtFont := TExtendedFont.Create;
    fExtFont.LoadFromStream(Stream);
    Read(fClippingFlags, SizeOf(fClippingFlags));
    Read(fDrawBox, SizeOf(fDrawBox));
    Read(fHeight, SizeOf(fHeight));
    Read(fHJustification, SizeOf(fHJustification));
    Read(fVJustification, SizeOf(fVJustification));
  end;
end;

procedure TText2D.SaveToStream(const Stream: TStream);
var
  TmpInt: Integer;
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
    Write(fHJustification, SizeOf(fHJustification));
    Write(fVJustification, SizeOf(fVJustification));
  end;
end;

procedure TText2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
    Exit;
  inherited Assign(Obj);
  if Obj is TPrimitive2D then
  begin
    if TPrimitive2D(Obj).fPoints.Count > 0 then
      fPoints.Copy(TPrimitive2D(Obj).fPoints, 0, 0);
    fPoints.GrowingEnabled := False;
  end;
  if Obj is TText2D then
  begin
    fText := (Obj as TText2D).Text;
    fTeXText := (Obj as TText2D).TeXText;
    fHeight := (Obj as TText2D).Height;
    fDrawBox := (Obj as TText2D).DrawBox;
    fRecalcBox := (Obj as TText2D).fRecalcBox;
    fClippingFlags := (Obj as TText2D).ClippingFlags;
    if not Assigned(fExtFont) then
      fExtFont := TExtendedFont.Create;
    fExtFont.Assign(TText2D(Obj).fExtFont);
    fHJustification := (Obj as TText2D).fHJustification;
    fVJustification := (Obj as TText2D).fVJustification;
  end;
end;

procedure TText2D.TransForm(const T: TTransf2D);
var
  P0, P1: TPoint2D;
begin
  P0 := fPoints[0];
  P1 := P0;
  P1.Y := P1.Y + fHeight;
  P0 := TransformPoint2D(P0, T);
  P1 := TransformPoint2D(P1, T);
  fHeight := PointDistance2D(P0, P1);
  //fHeight := RoundTo(fHeight, -2);
  fPoints[0] := P0;
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

constructor TBitmap2D.Create(ID: Longint; const P1, P2:
  TPoint2D; BMP: TBitmap);
begin
  inherited Create(ID, 2);
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
    fPoints.Copy(TPrimitive2D(Obj).fPoints, 0, 1);
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
  TmpWord: Word;
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
        Read(TmpWord, SizeOf(TmpWord));
        fSubVects.Objects[Cont] :=
          TPointsSet2D.Create(TmpWord);
        while TmpWord > 0 do
        begin
          Read(TmpPt, SizeOf(TmpPt));
          TPointsSet2D(fSubVects.Objects[Cont]).Add(TmpPt);
          Dec(TmpWord);
        end;
      end;
      UpdateExtension(Self);
    end;
end;

procedure TVectChar.SaveToStream(const Stream: TStream);
var
  TmpInt, Cont: Integer;
  TmpWord: Word;
  TmpPt: TPoint2D;
begin
  with Stream do
  begin
    TmpInt := fSubVects.NumberOfObjects;
    Write(TmpInt, SizeOf(TmpInt));
     // Scrittura vettori.
    for Cont := 0 to fSubVects.NumberOfObjects - 1 do
    begin
      TmpWord := TPointsSet2D(fSubVects.Objects[Cont]).Count;
      Write(TmpWord, SizeOf(TmpWord));
      while TmpWord > 0 do
      begin
        TmpPt :=
          TPointsSet2D(fSubVects.Objects[Cont]).Points[TmpWord
          -
          1];
        Write(TmpPt, SizeOf(TmpPt));
        Dec(TmpWord);
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
        Vectors[Cont].DrawAsPolyline(Cnv,
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
      ECADOutOfBound.Create('CADSysRegisterFont: Out of bound registration index');
  if Assigned(VectFonts2DRegistered[Index]) then
    raise
      ECADObjClassNotFound.Create('CADSysRegisterFont: Font index already allocated');
  VectFonts2DRegistered[Index] := Font;
end;

procedure CADSysUnregisterFont(Index: Word);
begin
  if Index > MAX_REGISTERED_FONTS then
    raise
      ECADOutOfBound.Create('CADSysUnregisterFont: Out of bound registration index');
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
      ECADOutOfBound.Create('CADSysRegisterFontFromFile: Out of bound registration index');
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

constructor TJustifiedVectText2D.Create(ID: Longint; FontVect:
  TVectFont; TextBox: TRect2D; Height: TRealType; Txt:
  AnsiString);
begin
  inherited Create(ID, 2);
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
    fPoints.Copy(TPrimitive2D(Obj).fPoints, 0, 1);
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

