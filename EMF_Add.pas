unit EMF_Add;

interface

uses Types, Classes, Forms, Windows, Contnrs, SysUtils, Graphics;

const

  EMF_Records: array[1..97] of string[50] =
  ('HEADER', 'POLYBEZIER', 'POLYGON', 'POLYLINE',
    'POLYBEZIERTO', 'POLYLINETO', 'POLYPOLYLINE',
    'POLYPOLYGON', 'SETWINDOWEXTEX', 'SETWINDOWORGEX',
    'SETVIEWPORTEXTEX', 'SETVIEWPORTORGEX',
    'SETBRUSHORGEX', 'EOF', 'SETPIXELV', 'SETMAPPERFLAGS',
    'SETMAPMODE', 'SETBKMODE', 'SETPOLYFILLMODE',
    'SETROP2', 'SETSTRETCHBLTMODE', 'SETTEXTALIGN',
    'SETCOLORADJUSTMENT', 'SETTEXTCOLOR', 'SETBKCOLOR',
    'OFFSETCLIPRGN', 'MOVETOEX', 'SETMETARGN',
    'EXCLUDECLIPRECT', 'INTERSECTCLIPRECT',
    'SCALEVIEWPORTEXTEX', 'SCALEWINDOWEXTEX', 'SAVEDC',
    'RESTOREDC', 'SETWORLDTRANSFORM',
    'MODIFYWORLDTRANSFORM', 'SELECTOBJECT', 'CREATEPEN',
    'CREATEBRUSHINDIRECT', 'DELETEOBJECT', 'ANGLEARC',
    'ELLIPSE', 'RECTANGLE', 'ROUNDRECT', 'ARC', 'CHORD',
    'PIE', 'SELECTPALETTE', 'CREATEPALETTE',
    'SETPALETTEENTRIES', 'RESIZEPALETTE', 'REALIZEPALETTE',
    'EXTFLOODFILL', 'LINETO', 'ARCTO', 'POLYDRAW',
    'SETARCDIRECTION', 'SETMITERLIMIT', 'BEGINPATH',
    'ENDPATH', 'CLOSEFIGURE', 'FILLPATH',
    'STROKEANDFILLPATH', 'STROKEPATH', 'FLATTENPATH',
    'WIDENPATH', 'SELECTCLIPPATH', 'ABORTPATH', 'Qwerty',
    'GDICOMMENT', 'FILLRGN', 'FRAMERGN', 'INVERTRGN',
    'PAINTRGN', 'EXTSELECTCLIPRGN', 'BITBLT', 'STRETCHBLT',
    'MASKBLT', 'PLGBLT', 'SETDIBITSTODEVICE',
    'STRETCHDIBITS', 'EXTCREATEFONTINDIRECTW',
    'EXTTEXTOUTA', 'EXTTEXTOUTW', 'POLYBEZIER16',
    'POLYGON16', 'POLYLINE16', 'POLYBEZIERTO16',
    'POLYLINETO16', 'POLYPOLYLINE16', 'POLYPOLYGON16',
    'POLYDRAW16', 'CREATEMONOBRUSH',
    'CREATEDIBPATTERNBRUSHPT', 'EXTCREATEPEN',
    'POLYTEXTOUTA', 'POLYTEXTOUTW');

type

  TEMF_Record = class
    EMR_Info0: TEMR;
    procedure ReadFromStream(AStream: TStream); virtual;
      abstract;
  end;

  TEMF_RecordClass = class of TEMF_Record;

  TEMF_Skip = class(TEMF_Record)
    EMR_Info: TEMR;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_PolyGen = class(TEMF_Record)
    PointsArray: array of TPoint;
  end;

  TEMF_Poly32 = class(TEMF_PolyGen)
    EMR_Info: TEMRPolyline;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_PolyBezier320 = class(TEMF_Poly32)
  end;

  TEMF_PolyBezier32 = class(TEMF_PolyBezier320)
  end;

  TEMF_Polygon32 = class(TEMF_Poly32)
  end;

  TEMF_Polyline32 = class(TEMF_Poly32)
  end;

  TEMF_PolyBezierTo32 = class(TEMF_PolyBezier320)
  end;

  TEMF_PolylineTo32 = class(TEMF_Poly32)
  end;

  TEMF_PolyPolyline0 = class(TEMF_Record)
    EMR_Info: TEMRPolyPolyline;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_PolyPolyline = class(TEMF_PolyPolyline0)
  end;

  TEMF_PolyPolygon = class(TEMF_PolyPolyline0)
  end;

  TEMF_SetExtEx = class(TEMF_Record)
    EMR_Info: TEMRSetViewportExtEx;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_SetWindowExtEx = class(TEMF_SetExtEx)
  end;

  TEMF_SetOrgEx = class(TEMF_Record)
    EMR_Info: TEMRSetViewportOrgEx;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_SetWindowOrgEx = class(TEMF_SetOrgEx)
  end;

  TEMF_SetViewportExtEx = class(TEMF_SetExtEx)
  end;

  TEMF_SetViewportOrgEx = class(TEMF_SetOrgEx)
  end;

  TEMF_SetBrushOrgEx = class(TEMF_SetOrgEx)
  end;

  TEMF_EOF = class(TEMF_Skip)
  end;

  TEMF_SetPixelV = class(TEMF_Record)
    EMR_Info: TEMRSetPixelV;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_SetMapperFlags = class(TEMF_Record)
    EMR_Info: TEMRSetMapperFlags;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_Mode = class(TEMF_Record)
    EMR_Info: TEMRSelectClipPath;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_SetMapMode = class(TEMF_Mode)
  end;

  TEMF_SetBkMode = class(TEMF_Mode)
  end;

  TEMF_SetPolyFillMode = class(TEMF_Mode)
  end;

  TEMF_SetRop2 = class(TEMF_Mode)
  end;

  TEMF_SetStretchBltMode = class(TEMF_Mode)
  end;

  TEMF_SetTextAlign = class(TEMF_Mode)
  end;

  TEMF_SetColorAdjustment = class(TEMF_Record)
    EMR_Info: TEMRSetColorAdjustment;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_SetTextColor0 = class(TEMF_Record)
    EMR_Info: TEMRSetTextColor;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_SetTextColor = class(TEMF_SetTextColor0)
  end;

  TEMF_SetBkColor = class(TEMF_SetTextColor0)
  end;

  TEMF_OffsetClipRgn = class(TEMF_Record)
    EMR_Info: TEMROffsetClipRgn;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_LineTo0 = class(TEMF_Record)
    EMR_Info: TEMRLineTo;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_MoveToEx = class(TEMF_LineTo0)
  end;

  TEMF_SetMetaRgn = class(TEMF_Skip)
  end;

  TEMF_ClipRect = class(TEMF_Record)
    EMR_Info: TEMRExcludeClipRect;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_ExcludeClipRect = class(TEMF_ClipRect)
  end;

  TEMF_IntersectClipRect = class(TEMF_ClipRect)
  end;

  TEMF_ScaleExtEx = class(TEMF_Record)
    EMR_Info: TEMRScaleViewportExtEx;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_ScaleViewportExtEx = class(TEMF_ScaleExtEx)
  end;

  TEMF_ScaleWindowExtEx = class(TEMF_ScaleExtEx)
  end;

  TEMF_SaveDC = class(TEMF_Skip)
  end;

  TEMF_RestoreDC = class(TEMF_Record)
    EMR_Info: TEMRRestoreDC;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_SetWorldTransform = class(TEMF_Record)
    EMR_Info: TEMRSetWorldTransform;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_ModifyWorldTransform = class(TEMF_Record)
    EMR_Info: TEMRModifyWorldTransform;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_ObjectHandle = class(TEMF_Record)
    EMR_Info: TEMRSelectObject;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_SelectObject = class(TEMF_ObjectHandle)
  end;

  TEMF_CreatePen = class(TEMF_Record)
    EMR_Info: TEMRCreatePen;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_CreateBrushIndirect = class(TEMF_Record)
    EMR_Info: TEMRCreateBrushIndirect;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_DeleteObject = class(TEMF_ObjectHandle)
  end;

  TEMF_AngleArc = class(TEMF_Record)
    EMR_Info: TEMRAngleArc;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_Rect0 = class(TEMF_Record)
    EMR_Info: TEMREllipse;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_Ellipse = class(TEMF_Rect0)
  end;

  TEMF_Rectangle = class(TEMF_Rect0)
  end;

  TEMF_RoundRect = class(TEMF_Record)
    EMR_Info: TEMRRoundRect;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_Arc0 = class(TEMF_Record)
    EMR_Info: TEMRArc;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_Arc = class(TEMF_Arc0)
  end;

  TEMF_Chord = class(TEMF_Arc0)
  end;

  TEMF_Pie = class(TEMF_Arc0)
  end;

  TEMF_SelectPalette = class(TEMF_Record)
    EMR_Info: TEMRSelectPalette;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_CreatePalette = class(TEMF_Record)
    EMR_Info: TEMRCreatePalette;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_SetPaletteEntries = class(TEMF_Record)
    EMR_Info: TEMRSetPaletteEntries;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_ResizePalette = class(TEMF_Record)
    EMR_Info: TEMRResizePalette;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_RealizePalette = class(TEMF_Skip)
  end;

  TEMF_ExtFloodFill = class(TEMF_Record)
    EMR_Info: TEMRExtFloodFill;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_LineTo = class(TEMF_LineTo0)
  end;

  TEMF_ArcTo = class(TEMF_Arc0)
  end;

  TEMF_PolyDraw = class(TEMF_Record)
    EMR_Info: TEMRPolyDraw;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_SetArcDirection = class(TEMF_Record)
    EMR_Info: TEMRSetArcDirection;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMRSetMiterLimitQQ = packed record
    emr: TEMR;
    eMiterLimit: Longword;
    //a bug in Windows GDI! eMiterLimit is actually DWORD(Longword) instead of FLOAT(Single)
  end;

  TEMF_SetMiterLimit = class(TEMF_Record)
    EMR_Info: TEMRSetMiterLimitQQ;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_BeginPath = class(TEMF_Skip)
    HasStroke, HasFill, HasClip, UseBezier: Boolean;
    N: Integer;
  end;

  TEMF_EndPath = class(TEMF_Skip)
  end;

  TEMF_CloseFigure = class(TEMF_Skip)
  end;

  TEMF_PathBounds = class(TEMF_Record)
    EMR_Info: TEMRFillPath;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_FillPath = class(TEMF_PathBounds)
  end;

  TEMF_StrokeAndFillPath = class(TEMF_PathBounds)
  end;

  TEMF_StrokePath = class(TEMF_PathBounds)
  end;

  TEMF_FlattenPath = class(TEMF_Skip)
  end;

  TEMF_WidenPath = class(TEMF_Skip)
  end;

  TEMF_SelectClipPath = class(TEMF_PathBounds)
    EMR_Info: TEMRSelectClipPath;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_AbortPath = class(TEMF_Skip)
  end;

  TEMF_GDIComment = class(TEMF_Record)
    EMR_Info: TEMRGDIComment;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_FillRgn = class(TEMF_Record)
    EMR_Info: TEMRFillRgn;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_FrameRgn = class(TEMF_Record)
    EMR_Info: TEMRFrameRgn;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_InvertRgn = class(TEMF_Record)
    EMR_Info: TEMRInvertRgn;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_PaintRgn = class(TEMF_Record)
    EMR_Info: TEMRInvertRgn;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_ExtSelectClipRgn = class(TEMF_Record)
    EMR_Info: TEMRExtSelectClipRgn;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_BitBlt = class(TEMF_Record)
    EMR_Info: TEMRBitBlt;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_StretchBlt = class(TEMF_Record)
    EMR_Info: TEMRStretchBlt;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_MaskBlt = class(TEMF_Record)
    EMR_Info: TEMRMaskBlt;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_PLGBlt = class(TEMF_Record)
    EMR_Info: TEMRPLGBlt;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_SetDIBitsToDevice = class(TEMF_Record)
    EMR_Info: TEMRSetDIBitsToDevice;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_StretchDIBits = class(TEMF_Record)
    EMR_Info: TEMRStretchDIBits;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_ExtCreateFontIndirectW = class(TEMF_Record)
    EMR_Info: TEMRExtCreateFontIndirect;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_ExtTextOut = class(TEMF_Record)
    EMR_Info: TEMRExtTextOut;
    Str: string;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_ExtTextOutA = class(TEMF_ExtTextOut)
  end;

  TEMF_ExtTextOutW = class(TEMF_ExtTextOut)
  end;

  TEMF_Poly16 = class(TEMF_PolyGen)
    EMR_Info: TEMRPolyline16;
//    PointsArray16: array of TSmallPoint;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_PolyBezier160 = class(TEMF_Poly16)
  end;

  TEMF_PolyBezier16 = class(TEMF_PolyBezier160)
  end;

  TEMF_Polygon16 = class(TEMF_Poly16)
  end;

  TEMF_Polyline16 = class(TEMF_Poly16)
  end;

  TEMF_PolyBezierTo16 = class(TEMF_PolyBezier160)
  end;

  TEMF_PolylineTo16 = class(TEMF_Poly16)
  end;

  T_PointsArray = class
    PointsArray: array of TPoint;
  end;

  TEMF_PolyPoly16 = class(TEMF_Record)
    EMR_Info: TEMRPolyPolyline16;
    PolyList: TObjectList;
    constructor Create;
    destructor Destroy; override;
    procedure ReadFromStream(AStream: TStream); override;
    function GetLen(iPoly: Integer): Integer;
    function GetPnt(iPoly: Integer; I: Integer): TPoint;
  end;

  TEMF_PolyPolyline16 = class(TEMF_PolyPoly16)
  end;

  TEMF_PolyPolygon16 = class(TEMF_PolyPoly16)
  end;

  TEMF_PolyDraw16 = class(TEMF_Record)
    EMR_Info: TEMRPolyDraw16;
    PointsArray: array of TPoint;
    abTypes: array of Byte;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_CreateMonoBrush = class(TEMF_Record)
    EMR_Info: TEMRCreateMonoBrush;
    BMP: Graphics.TBitmap;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_CreateDIBPatternBrushPt = class(TEMF_Record)
    EMR_Info: TEMRCreateDIBPatternBrushPt;
    BMP: Graphics.TBitmap;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_ExtCreatePen = class(TEMF_Record)
    EMR_Info: TEMRExtCreatePen;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_PolyTextOutA = class(TEMF_Record)
    EMR_Info: TEMRPolyTextOut;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TEMF_PolyTextOutW = class(TEMF_Record)
    EMR_Info: TEMRPolyTextOut;
    procedure ReadFromStream(AStream: TStream); override;
  end;

  TPolyLineArr = array[1..5] of Integer;

  T_EMFHeader = packed record
    RecordType: DWORD; // Record type
    RecordSize: DWORD; // Size of the record in bytes
    BoundsLeft: Longint; // Left inclusive bounds
    BoundsTop: Longint; // Top inclusive bounds * /
    BoundsRight: Longint; // Right inclusive bounds * /
    BoundsBottom: Longint; // Bottom inclusive bounds * /
    FrameLeft: Longint;
      // Left side of inclusive Picture frame * /
    FrameTop: Longint;
          // Top side of inclusive Picture frame * /
    FrameRight: Longint;
      // Right side of inclusive Picture frame * /
    FrameBottom: Longint;
      // Bottom side of inclusive Picture frame * /
    Signature: DWORD; // Signature ID(always 0 x464D4520) * /
    Version: DWORD; // Version of the metafile * /
    Size: DWORD; // Size of the metafile in bytes * /
    NumOfRecords: DWORD;
          // Number of records in the metafile * /
    NumOfHandles: Word;
      // Number of handles in the Handle table * /
    Reserved: Word; // not used(always 0) * /
    SizeOfDescrip: DWORD;
      // Size of description string in WORDs * /
    OffsOfDescrip: DWORD;
      // offset of description string in metafile * /
    NumPalEntries: DWORD;
          // Number of Color palette entries * /
    WidthDevPixels: Longint;
      // Width of reference Device in Pixels * /
    HeightDevPixels: Longint;
      // Height of reference Device in Pixels * /
    WidthDevMM: Longint;
      // Width of reference Device in millimeters * /
    HeightDevMM: Longint;
      // Height of reference Device in millimeters * /
  end;


  TStretchDIBits = packed record
    rclBounds: TRect;
    xDest: Longint;
          // x-coord of destination upper-left corner
    yDest: Longint;
          // y-coord of destination upper-left corner
    xSrc: Longint; // x-coord of source upper-left corner
    ySrc: Longint; // y-coord of source upper-left corner
    cxSrc: Longint;
    cySrc: Longint;
    offBmiSrc: DWORD;
    cbBmiSrc: DWORD;
    offBitsSrc: DWORD;
    cbBitsSrc: DWORD;
    iUsageSrc: DWORD; // usage options
    dwRop: DWORD; // raster operation code
    cxDest: Longint;
    cyDest: Longint;
  end;

//The XFORM structure specifies a world-space to page-space transformation.
  XFORM = packed record
    eM11: Single; //FLOAT
    eM12: Single;
    eM21: Single;
    eM22: Single;
    eDx: Single;
    eDy: Single;
  end;

  TBitBlt = packed record
    rclBounds: TRect;
    xDest: Longint;
          // x-coord of destination upper-left corner
    yDest: Longint;
          // y-coord of destination upper-left corner
    cxDest: Longint; // width of destination rectangle
    cyDest: Longint; // height of destination rectangle
    dwRop: DWORD; // raster operation code
    xSrc: Longint;
          // x-coordinate of source upper-left corner
    ySrc: Longint;
          // y-coordinate of source upper-left corner
    xformSrc: XFORM;
    crBkColorSrc: DWORD; //COLORREF
    iUsageSrc: DWORD;
    offBmiSrc: DWORD;
    cbBmiSrc: DWORD;
    offBitsSrc: DWORD;
    cbBitsSrc: DWORD;
  end;

  TPolyPoly = packed record
    rclBounds: TRect; //Bounding rectangle, in device units.
    nPolys: DWORD; //Number of polys.
    cpts: DWORD; //Total Number of Points in All polys.
  end;

{
The EMRPOLYPOLYLINE16 and EMRPOLYPOLYGON16 structures contain
members for the PolyPolyline and PolyPolygon enhanced metafile records.

  RECTL   rclBounds; Bounding rectangle, in device units.
  DWORD   nPolys; Number of polys.
  DWORD   cpts; Total number of points in all polys.
  DWORD   aPolyCounts[1]; Array of point counts for each poly.
  POINTS  apts[1]; Array of 16-bit points, in logical units.
  }


const
  EMF_RecordsClasses: array[1..97] of TEMF_RecordClass =

  (nil, TEMF_PolyBezier32, TEMF_Polygon32, TEMF_Polyline32,
    TEMF_PolyBezierTo32, TEMF_PolylineTo32, TEMF_PolyPolyline,
    TEMF_PolyPolygon, TEMF_SetWindowExtEx,
    TEMF_SetWindowOrgEx,
    TEMF_SetViewportExtEx, TEMF_SetViewportOrgEx,
    TEMF_SetBrushOrgEx, TEMF_EOF, TEMF_SetPixelV,
    TEMF_SetMapperFlags,
    TEMF_SetMapMode, TEMF_SetBkMode, TEMF_SetPolyFillMode,
    TEMF_SetRop2, TEMF_SetStretchBltMode, TEMF_SetTextAlign,
    TEMF_SetColorAdjustment, TEMF_SetTextColor,
    TEMF_SetBkColor,
    TEMF_OffsetClipRgn, TEMF_MoveToEx, TEMF_SetMetaRgn,
    TEMF_ExcludeClipRect, TEMF_IntersectClipRect,
    TEMF_ScaleViewportExtEx, TEMF_ScaleWindowExtEx,
    TEMF_SaveDC,
    TEMF_RestoreDC, TEMF_SetWorldTransform,
    TEMF_ModifyWorldTransform, TEMF_SelectObject,
    TEMF_CreatePen,
    TEMF_CreateBrushIndirect, TEMF_DeleteObject,
    TEMF_AngleArc,
    TEMF_Ellipse, TEMF_Rectangle, TEMF_RoundRect, TEMF_Arc,
    TEMF_Chord,
    TEMF_Pie, TEMF_SelectPalette, TEMF_CreatePalette,
    TEMF_SetPaletteEntries, TEMF_ResizePalette,
    TEMF_RealizePalette,
    TEMF_ExtFloodFill, TEMF_LineTo, TEMF_ArcTo,
    TEMF_PolyDraw,
    TEMF_SetArcDirection, TEMF_SetMiterLimit,
    TEMF_BeginPath,
    TEMF_EndPath, TEMF_CloseFigure, TEMF_FillPath,
    TEMF_StrokeAndFillPath, TEMF_StrokePath,
    TEMF_FlattenPath,
    TEMF_WidenPath, TEMF_SelectClipPath, TEMF_AbortPath,
    nil,
    TEMF_GDIComment, TEMF_FillRgn, TEMF_FrameRgn,
    TEMF_InvertRgn,
    TEMF_PaintRgn, TEMF_ExtSelectClipRgn, TEMF_BitBlt,
    TEMF_StretchBlt,
    TEMF_MaskBlt, TEMF_PLGBlt, TEMF_SetDIBitsToDevice,
    TEMF_StretchDIBits, TEMF_ExtCreateFontIndirectW,
    TEMF_ExtTextOutA, TEMF_ExtTextOutW, TEMF_PolyBezier16,
    TEMF_Polygon16, TEMF_Polyline16, TEMF_PolyBezierTo16,
    TEMF_PolylineTo16, TEMF_PolyPolyline16,
    TEMF_PolyPolygon16,
    TEMF_PolyDraw16, TEMF_CreateMonoBrush,
    TEMF_CreateDIBPatternBrushPt, TEMF_ExtCreatePen,
    TEMF_PolyTextOutA, TEMF_PolyTextOutW);

implementation
{typedef struct _EnhancedMetaHeader

    DWORD RecordType;       /* Record type */
    DWORD RecordSize;       /* Size of the record in bytes */
    LONG  BoundsLeft;       /* Left inclusive bounds */
    LONG  BoundsRight;      /* Right inclusive bounds */
    LONG  BoundsTop;        /* Top inclusive bounds */
    LONG  BoundsBottom;     /* Bottom inclusive bounds */
    LONG  FrameLeft;        /* Left side of inclusive picture frame */
    LONG  FrameRight;       /* Right side of inclusive picture frame */
    LONG  FrameTop;         /* Top side of inclusive picture frame */
    LONG  FrameBottom;      /* Bottom side of inclusive picture frame */
    DWORD Signature;        /* Signature ID (always 0x464D4520) */
    DWORD Version;          /* Version of the metafile */
    DWORD Size;             /* Size of the metafile in bytes */
    DWORD NumOfRecords;     /* Number of records in the metafile */
    WORD  NumOfHandles;     /* Number of handles in the handle table */
    WORD  Reserved;         /* Not used (always 0) */
    DWORD SizeOfDescrip;    /* Size of description string in WORDs */
    DWORD OffsOfDescrip;    /* Offset of description string in metafile */
    DWORD NumPalEntries;    /* Number of color palette entries */
    LONG  WidthDevPixels;   /* Width of reference device in pixels */
    LONG  HeightDevPixels;  /* Height of reference device in pixels */
    LONG  WidthDevMM;       /* Width of reference device in millimeters */
    LONG  HeightDevMM;      /* Height of reference device in millimeters */
 ENHANCEDMETAHEADER;}

{typedef struct _EnhancedMetaRecord
    DWORD Function;      /* Function number (defined in WINGDI.H) */
    DWORD Size;          /* Total size of the record in WORDs */
    DWORD Parameters[];   /* Parameter values passed to GDI function */
 EMFRECORD}

    {A('Record type ' + IntToStr(fHeader.RecordType));
    A('Size of the record in bytes ' +
      IntToStr(fHeader.RecordSize));
    A('Left inclusive bounds ' + IntToStr(fHeader.BoundsLeft));
    A('Right inclusive bounds ' +
      IntToStr(fHeader.BoundsRight));
    A('Top inclusive bounds ' + IntToStr(fHeader.BoundsTop));
    A('Bottom inclusive bounds ' +
      IntToStr(fHeader.BoundsBottom));
    A('Left side of inclusive Picture frame ' +
      IntToStr(fHeader.FrameLeft));
    A('Right side of inclusive Picture frame ' +
      IntToStr(fHeader.FrameRight));
    A('Top side of inclusive Picture frame ' +
      IntToStr(fHeader.FrameTop));
    A('Bottom side of inclusive Picture frame ' +
      IntToStr(fHeader.FrameBottom));
    A('Signature ID(always 0 x464D4520) ' +
      IntToStr(fHeader.Signature) + ' Hex:' +
      IntToHex(fHeader.Signature, 8));
    A('Version of the metafile ' + IntToStr(fHeader.Version) +
      ' Hex:' + IntToHex(fHeader.Version, 8));
    A('Size of the metafile in bytes ' +
      IntToStr(fHeader.Size));
    A('Number of records in the metafile ' +
      IntToStr(fHeader.NumOfRecords));
    A('Number of handles in the Handle table ' +
      IntToStr(fHeader.NumOfHandles));
    A('not used(always 0) ' + IntToStr(fHeader.Reserved));
    A('Size of description string in WORDs ' +
      IntToStr(fHeader.SizeOfDescrip));
    A('offset of description string in metafile ' +
      IntToStr(fHeader.OffsOfDescrip));
    A('Number of Color palette entries ' +
      IntToStr(fHeader.NumPalEntries));
    A('Width of reference Device in Pixels ' +
      IntToStr(fHeader.WidthDevPixels));
    A('Height of reference Device in Pixels ' +
      IntToStr(fHeader.HeightDevPixels));
    A('Width of reference Device in millimeters ' +
      IntToStr(fHeader.WidthDevMM));
    A('Height of reference Device in millimeters ' +
      IntToStr(fHeader.HeightDevMM));}

{
The SetROP2 function sets the current foreground mix mode.
GDI uses the foreground mix mode to combine pens and interiors
of filled objects with the colors already on the screen.
The foreground mix mode defines how colors from the brush or
pen and the colors in the existing image are to be combined.

int SetROP2(
  HDC hdc,         // handle to DC
  int fnDrawMode   // drawing mode
);
Parameters
hdc
[in] Handle to the device context.
fnDrawMode
[in] Specifies the mix mode. This parameter can be one of the following values. Mix mode Description
R2_BLACK Pixel is always 0.
R2_COPYPEN Pixel is the pen color.
R2_MASKNOTPEN Pixel is a combination of the colors common to both the screen and the inverse of the pen.
R2_MASKPEN Pixel is a combination of the colors common to both the pen and the screen.
R2_MASKPENNOT Pixel is a combination of the colors common to both the pen and the inverse of the screen.
R2_MERGENOTPEN Pixel is a combination of the screen color and the inverse of the pen color.
R2_MERGEPEN Pixel is a combination of the pen color and the screen color.
R2_MERGEPENNOT Pixel is a combination of the pen color and the inverse of the screen color.
R2_NOP Pixel remains unchanged.
R2_NOT Pixel is the inverse of the screen color.
R2_NOTCOPYPEN Pixel is the inverse of the pen color.
R2_NOTMASKPEN Pixel is the inverse of the R2_MASKPEN color.
R2_NOTMERGEPEN Pixel is the inverse of the R2_MERGEPEN color.
R2_NOTXORPEN Pixel is the inverse of the R2_XORPEN color.
R2_WHITE Pixel is always 1.
R2_XORPEN Pixel is a combination of the colors in the pen and in the screen, but not in both.
}

procedure TEMF_Skip.ReadFromStream(AStream: TStream);
begin
  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
  AStream.Seek(EMR_Info.nSize - SizeOf(EMR_Info),
    soFromCurrent);
end;

procedure TEMF_Poly32.ReadFromStream(AStream: TStream);
var
  I: Longword;
begin
  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
  AStream.Seek(-SizeOf(EMR_Info.aptl), soFromCurrent);
  SetLength(PointsArray, EMR_Info.cptl);
  for I := 0 to EMR_Info.cptl - 1 do
    AStream.ReadBuffer(PointsArray[I], SizeOf(TPoint));
end;

procedure TEMF_PolyPolyline0.ReadFromStream(AStream: TStream);
begin

  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
  AStream.Seek(-SizeOf(EMR_Info.aPolyCounts)
    - SizeOf(EMR_Info.aptl), soFromCurrent);
end;

procedure TEMF_SetExtEx.ReadFromStream(AStream: TStream);
begin

  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
end;

procedure TEMF_SetOrgEx.ReadFromStream(AStream: TStream);
begin

  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
end;

procedure TEMF_SetPixelV.ReadFromStream(AStream: TStream);
begin

  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
end;

procedure TEMF_SetMapperFlags.ReadFromStream(AStream: TStream);
begin

  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
end;

procedure TEMF_Mode.ReadFromStream(AStream: TStream);
begin

  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
end;

procedure TEMF_SetColorAdjustment.ReadFromStream(AStream:
  TStream);
begin

  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
end;

procedure TEMF_SetTextColor0.ReadFromStream(AStream: TStream);
begin

  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
end;

procedure TEMF_OffsetClipRgn.ReadFromStream(AStream: TStream);
begin

  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
end;

procedure TEMF_LineTo0.ReadFromStream(AStream: TStream);
begin

  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
end;

procedure TEMF_ClipRect.ReadFromStream(AStream: TStream);
begin

  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
end;

procedure TEMF_ScaleExtEx.ReadFromStream(AStream: TStream);
begin

  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
end;

procedure TEMF_RestoreDC.ReadFromStream(AStream: TStream);
begin

  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
end;

procedure TEMF_SetWorldTransform.ReadFromStream(AStream:
  TStream);
begin

  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
end;

procedure TEMF_ModifyWorldTransform.ReadFromStream(AStream:
  TStream);
begin

  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
end;

procedure TEMF_ObjectHandle.ReadFromStream(AStream:
  TStream);
begin

  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
end;

procedure TEMF_CreatePen.ReadFromStream(AStream:
  TStream);
begin

  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
end;

procedure TEMF_CreateBrushIndirect.ReadFromStream(AStream:
  TStream);
begin

  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
end;

procedure TEMF_AngleArc.ReadFromStream(AStream:
  TStream);
begin

  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
end;

procedure TEMF_Rect0.ReadFromStream(AStream:
  TStream);
begin

  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
end;

procedure TEMF_RoundRect.ReadFromStream(AStream:
  TStream);
begin

  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
end;

procedure TEMF_Arc0.ReadFromStream(AStream:
  TStream);
begin

  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
end;

procedure TEMF_SelectPalette.ReadFromStream(AStream:
  TStream);
begin

  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
end;

procedure TEMF_CreatePalette.ReadFromStream(AStream:
  TStream);
begin

  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
end;

procedure TEMF_SetPaletteEntries.ReadFromStream(AStream:
  TStream);
begin

  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
end;

procedure TEMF_ResizePalette.ReadFromStream(AStream:
  TStream);
begin

  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
end;

procedure TEMF_ExtFloodFill.ReadFromStream(AStream:
  TStream);
begin

  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
end;

procedure TEMF_PolyDraw.ReadFromStream(AStream:
  TStream);
begin

  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
  AStream.Seek(-SizeOf(EMR_Info.aptl)
    - SizeOf(EMR_Info.abTypes), soFromCurrent);
end;

procedure TEMF_SetArcDirection.ReadFromStream(AStream:
  TStream);
begin

  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
end;

procedure TEMF_SetMiterLimit.ReadFromStream(AStream:
  TStream);
{var
  BB: array[1..4] of Byte;
  BB2: array[1..4] of Byte;
  I: Integer;
  St: string;}
begin
  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
  //EMR_Info.eMiterLimit := 7;
  //Move(EMR_Info.eMiterLimit, BB, SizeOf(BB));
  //for I := 1 to 4 do BB2[5 - I] := BB[I];
  //Move(BB2, EMR_Info.eMiterLimit, SizeOf(BB));
  //St := '';
  //for I := 1 to 4 do St := St + Format('%x', [BB[I]]);
  //Application.MessageBox(PChar(St), '');
  //Application.Tag := Round(EMR_Info.eMiterLimit);
//  unsigned 32-bit *2 + 32
end;

procedure TEMF_PathBounds.ReadFromStream(AStream:
  TStream);
begin

  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
end;

procedure TEMF_SelectClipPath.ReadFromStream(AStream:
  TStream);
begin

  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
end;

procedure TEMF_GDIComment.ReadFromStream(AStream:
  TStream);
begin

  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
  AStream.Seek(-SizeOf(EMR_Info.Data), soFromCurrent);
end;

procedure TEMF_FillRgn.ReadFromStream(AStream:
  TStream);
begin

  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
  AStream.Seek(-SizeOf(EMR_Info.RgnData), soFromCurrent);
end;

procedure TEMF_FrameRgn.ReadFromStream(AStream:
  TStream);
begin

  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
  AStream.Seek(-SizeOf(EMR_Info.RgnData), soFromCurrent);
end;

procedure TEMF_InvertRgn.ReadFromStream(AStream:
  TStream);
begin

  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
  AStream.Seek(-SizeOf(EMR_Info.RgnData), soFromCurrent);
end;

procedure TEMF_PaintRgn.ReadFromStream(AStream:
  TStream);
begin

  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
  AStream.Seek(-SizeOf(EMR_Info.RgnData), soFromCurrent);
end;

procedure TEMF_ExtSelectClipRgn.ReadFromStream(AStream:
  TStream);
begin

  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
  AStream.Seek(-SizeOf(EMR_Info.RgnData), soFromCurrent);
end;

procedure TEMF_BitBlt.ReadFromStream(AStream:
  TStream);
begin

  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
end;

procedure TEMF_StretchBlt.ReadFromStream(AStream:
  TStream);
begin

  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
end;

procedure TEMF_MaskBlt.ReadFromStream(AStream:
  TStream);
begin

  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
end;

procedure TEMF_PLGBlt.ReadFromStream(AStream:
  TStream);
begin

  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
end;

procedure TEMF_SetDIBitsToDevice.ReadFromStream(AStream:
  TStream);
begin

  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
end;

procedure TEMF_StretchDIBits.ReadFromStream(AStream:
  TStream);
begin

  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
end;

procedure TEMF_ExtCreateFontIndirectW.ReadFromStream(AStream:
  TStream);
var
  APosition: Int64;
begin

  APosition := AStream.Position;
  AStream.ReadBuffer(EMR_Info.emr, SizeOf(EMR_Info.emr));
  AStream.ReadBuffer(EMR_Info.ihFont, SizeOf(EMR_Info.ihFont));
  AStream.ReadBuffer(EMR_Info.elfw, SizeOf(EMR_Info.elfw));
  AStream.Position := APosition;
  AStream.Seek(EMR_Info.emr.nSize, soFromCurrent);
end;

procedure TEMF_ExtTextOut.ReadFromStream(AStream:
  TStream);
var
  WStr: Widestring;
  Ch: Char;
  WCh: Widechar;
  I: Integer;
begin
  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
  AStream.Position :=
    AStream.Position - SizeOf(EMR_Info)
    + EMR_Info.emrtext.offString;
  if Self is TEMF_ExtTextOutW then
  begin
    SetLength(WStr, EMR_Info.emrtext.NChars);
    AStream.ReadBuffer(WStr[1], EMR_Info.emrtext.NChars * 2);
    //Str := WStr;
    Str := '';
    for I := 1 to Length(WStr) do
    begin
      Ch := string(WStr[I])[1];
      WCh := Widestring(Ch)[1];
      if WCh <> WStr[I]
        then Str := Str + Format('&#x%x;', [Ord(WStr[I])])
      else Str := Str + Ch;
    end;
  end
  else
  begin
    SetLength(Str, EMR_Info.emrtext.NChars);
    AStream.ReadBuffer(Str[1], EMR_Info.emrtext.NChars);
  end;
end;

procedure TEMF_Poly16.ReadFromStream(AStream: TStream);
var
  I: Longword;
  P: TSmallPoint;
begin
  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
  AStream.Seek(-SizeOf(EMR_Info.apts), soFromCurrent);
  SetLength(PointsArray, EMR_Info.cpts);
  for I := 0 to EMR_Info.cpts - 1 do
  begin
    AStream.ReadBuffer(P, SizeOf(TSmallPoint));
    PointsArray[I].X := P.X;
    PointsArray[I].Y := P.Y;
  end;
end;

constructor TEMF_PolyPoly16.Create;
begin
  inherited Create;
  PolyList := TObjectList.Create;
  PolyList.OwnsObjects := True;
end;

destructor TEMF_PolyPoly16.Destroy;
begin
  PolyList.Free;
  inherited Destroy;
end;

procedure TEMF_PolyPoly16.ReadFromStream(AStream: TStream);
var
  iPoly, I: Longword;
  Len: DWORD;
  Poly: T_PointsArray;
  P: TSmallPoint;
begin

  if PolyList = nil then
    PolyList := TObjectList.Create;
  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
  AStream.Seek(-SizeOf(EMR_Info.aPolyCounts)
    - SizeOf(EMR_Info.apts), soFromCurrent);
  if EMR_Info.nPolys = 0 then Exit;
  for iPoly := 0 to EMR_Info.nPolys - 1 do
  begin
    AStream.ReadBuffer(Len, SizeOf(Len));
    Poly := T_PointsArray.Create;
    PolyList.Add(Poly);
    SetLength(Poly.PointsArray, Len);
  end;
  for iPoly := 0 to EMR_Info.nPolys - 1 do
  begin
    Poly := PolyList[iPoly] as T_PointsArray;
    for I := 0 to High(Poly.PointsArray) do
    begin
      AStream.ReadBuffer(P,
        SizeOf(TSmallPoint));
      Poly.PointsArray[I].X := P.X;
      Poly.PointsArray[I].Y := P.Y;
    end;
  end;
end;

function TEMF_PolyPoly16.GetLen(iPoly: Integer): Integer;
begin
  Result := High((PolyList[iPoly] as
    T_PointsArray).PointsArray) + 1;
end;

function TEMF_PolyPoly16.GetPnt(iPoly: Integer; I: Integer):
  TPoint;
begin
  Result := (PolyList[iPoly] as
    T_PointsArray).PointsArray[I];
end;

{  TEMF_PolyPoly16 = class(TEMF_Record)
    T_PointsArray16 = class
      PolyList: TObjectList;
      }

procedure TEMF_PolyDraw16.ReadFromStream(AStream: TStream);
var
  I: Longword;
  S: TMemoryStream;
  St: string;
  P: TSmallPoint;
begin
  AStream.ReadBuffer(EMR_Info.emr, SizeOf(EMR_Info.emr));
  AStream.ReadBuffer(EMR_Info.rclBounds, SizeOf(EMR_Info.rclBounds));
  AStream.ReadBuffer(EMR_Info.cpts, SizeOf(EMR_Info.cpts));
  {AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
  AStream.Seek(-SizeOf(EMR_Info.apts)
    - SizeOf(EMR_Info.abTypes), soFromCurrent);}
  SetLength(PointsArray, EMR_Info.cpts);
  SetLength(abTypes, EMR_Info.cpts);
  for I := 0 to EMR_Info.cpts - 1 do
  begin
    AStream.ReadBuffer(P, SizeOf(TSmallPoint));
    PointsArray[I].X := P.X;
    PointsArray[I].Y := P.Y;
  end;
  for I := 0 to EMR_Info.cpts - 1 do
    AStream.ReadBuffer(abTypes[I], SizeOf(Byte));
  {S := TMemoryStream.Create;
  for I := 0 to EMR_Info.cpts - 1 do
  begin
    St := Format('%d %d %d'#13#10,
      [PointsArray16[I].X, PointsArray16[I].Y, abTypes[I]]);
    S.Write(St[1], Length(St));
  end;
  S.SaveToFile('C:\WRK\-.txt');
  S.Free;}
end;

procedure TEMF_CreateMonoBrush.ReadFromStream(AStream: TStream);
var
  bmiHeader: TBitmapInfoHeader;
  bmi: TBitmapInfo;
  PBits: Pointer;
  h_DC: HDC;
  hBmp: Longint;
  Position0: Longint;
begin
  Position0 := AStream.Position;
  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
  if not Assigned(BMP) then BMP := Graphics.TBitmap.Create;
  AStream.Position := Position0 + EMR_Info.offBmi;
  AStream.ReadBuffer(bmiHeader, SizeOf(bmiHeader));
  bmi.bmiHeader := bmiHeader;
  //Application.MessageBox(PChar(IntToStr(bmiHeader.biBitCount)), '');
  h_DC := GetWindowDC(0);
  GetMem(PBits, EMR_Info.cbBits);
  AStream.Position := Position0 + EMR_Info.offBits;
  AStream.ReadBuffer(PBits^, EMR_Info.cbBits);
  hBmp := CreateDIBSection(h_DC, bmi, EMR_Info.iUsage, PBits, 0, 0);
  //Application.MessageBox(PChar(IntToStr(EMR_Info.cbBits)), '');
  BMP.Handle := hBmp;
  //Application.MessageBox(PChar(IntToStr(BMP.Canvas.Pixels[0,0])), '');
  //Application.MessageBox(PChar(Format('%d %d', [BMP.Width, BMP.Height])), '');
  ReleaseDC(0, h_DC);
end;

procedure TEMF_CreateDIBPatternBrushPt.ReadFromStream(AStream:
  TStream);
var
  bmiHeader: TBitmapInfoHeader;
  bmi: TBitmapInfo;
  PBits: Pointer;
  h_DC: HDC;
  hBmp: Longint;
  Position0: Longint;
begin
  Position0 := AStream.Position;
  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
  if not Assigned(BMP) then BMP := Graphics.TBitmap.Create;
  AStream.Position := Position0 + EMR_Info.offBmi;
  AStream.ReadBuffer(bmiHeader, SizeOf(bmiHeader));
  bmi.bmiHeader := bmiHeader;
  //Application.MessageBox(PChar(IntToStr(bmiHeader.biBitCount)), '');
  h_DC := GetWindowDC(0);
  GetMem(PBits, EMR_Info.cbBits);
  AStream.Position := Position0 + EMR_Info.offBits;
  AStream.ReadBuffer(PBits^, EMR_Info.cbBits);
  hBmp := CreateDIBSection(h_DC, bmi, EMR_Info.iUsage, PBits, 0, 0);
  //Application.MessageBox(PChar(IntToStr(EMR_Info.cbBits)), '');
  BMP.Handle := hBmp;
  //Application.MessageBox(PChar(IntToStr(BMP.Canvas.Pixels[0,0])), '');
  //Application.MessageBox(PChar(Format('%d %d', [BMP.Width, BMP.Height])), '');
  ReleaseDC(0, h_DC);
end;

procedure TEMF_ExtCreatePen.ReadFromStream(AStream: TStream);
begin

  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
  //AStream.Seek(-SizeOf(EMR_Info.elp.elpStyleEntry),    soFromCurrent);
end;

procedure TEMF_PolyTextOutA.ReadFromStream(AStream: TStream);
begin

  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
  AStream.Seek(-SizeOf(EMR_Info.aemrtext), soFromCurrent);
end;

procedure TEMF_PolyTextOutW.ReadFromStream(AStream: TStream);
begin

  AStream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
  AStream.Seek(-SizeOf(EMR_Info.aemrtext), soFromCurrent);
end;

end.

