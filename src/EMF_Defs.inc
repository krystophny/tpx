const
  LF_FACESIZE = 32;
  LF_FULLFACESIZE = 64;
  ELF_VENDOR_SIZE = 4;

  { Stock Logical Objects }
  WHITE_BRUSH = 0;
  LTGRAY_BRUSH = 1;
  GRAY_BRUSH = 2;
  DKGRAY_BRUSH = 3;
  BLACK_BRUSH = 4;
  NULL_BRUSH = 5;
  HOLLOW_BRUSH = NULL_BRUSH;
  WHITE_PEN = 6;
  BLACK_PEN = 7;
  NULL_PEN = 8;
  OEM_FIXED_FONT = 10;
  ANSI_FIXED_FONT = 11;
  ANSI_VAR_FONT = 12;
  SYSTEM_FONT = 13;
  DEVICE_DEFAULT_FONT = 14;
  DEFAULT_PALETTE = 15;
  SYSTEM_FIXED_FONT = $10;
  DEFAULT_GUI_FONT = 17;
  DC_BRUSH = 18;
  DC_PEN = 19;
  STOCK_LAST = 19;

  { PolyDraw and GetPath point types }
  PT_CLOSEFIGURE = 1;
  PT_LINETO = 2;
  PT_BEZIERTO = 4;
  PT_MOVETO = 6;

  ETO_OPAQUE = 2;

  { Background Modes }
  TRANSPARENT = 1;
  OPAQUE = 2;
  BKMODE_LAST = 2;

  { Graphics Modes }
  GM_COMPATIBLE = 1;
  GM_ADVANCED = 2;
  GM_LAST = 2;

  { Mapping Modes }
  MM_TEXT = 1;
  MM_LOMETRIC = 2;
  MM_HIMETRIC = 3;
  MM_LOENGLISH = 4;
  MM_HIENGLISH = 5;
  MM_TWIPS = 6;
  MM_ISOTROPIC = 7;
  MM_ANISOTROPIC = 8;

  { Brush Styles }
  BS_SOLID                = 0;
  BS_NULL                 = 1;
  BS_HOLLOW               = BS_NULL;
  BS_HATCHED              = 2;
  BS_PATTERN              = 3;
  BS_INDEXED              = 4;
  BS_DIBPATTERN           = 5;
  BS_DIBPATTERNPT         = 6;
  BS_PATTERN8X8           = 7;
  BS_DIBPATTERN8X8        = 8;
  BS_MONOPATTERN          = 9;

  { Hatch Styles }
  HS_HORIZONTAL = 0;       { ----- }
  HS_VERTICAL   = 1;       { ||||| }
  HS_FDIAGONAL  = 2;       { ///// }
  HS_BDIAGONAL  = 3;       { \\\\\ }
  HS_CROSS      = 4;       { +++++ }
  HS_DIAGCROSS  = 5;       { xxxxx }

  { Pen Styles }
  PS_SOLID       = 0;
  PS_DASH        = 1;      { ------- }
  PS_DOT         = 2;      { ....... }
  PS_DASHDOT     = 3;      { _._._._ }
  PS_DASHDOTDOT  = 4;      { _.._.._ }
  PS_NULL = 5;
  PS_INSIDEFRAME = 6;
  PS_USERSTYLE = 7;
  PS_ALTERNATE = 8;
  PS_STYLE_MASK = 15;

  PS_COSMETIC = 0;
  PS_GEOMETRIC = $10000;
  PS_TYPE_MASK = $F0000;

  { Text Alignment Options }
  TA_NOUPDATECP = 0;
  TA_UPDATECP = 1;
  TA_LEFT = 0;
  TA_RIGHT = 2;
  TA_CENTER = 6;
  TA_TOP = 0;
  TA_BOTTOM = 8;
  TA_BASELINE = 24;
  TA_RTLREADING = $100;

  { xform stuff }
  MWT_IDENTITY = 1;
  MWT_LEFTMULTIPLY = 2;
  MWT_RIGHTMULTIPLY = 3;

  FW_BOLD = 700;

  type

  UINT = LongWord;
  ULONG = Cardinal;
  LONG = Longint;
  SHORT = Smallint;

  HDC = type LongWord;

  POINTL = TPoint;
  POINT = TPoint;
  POINTS = TSmallPoint;
  RECTL = TRect;
  COLORREF = DWORD;
  TCHAR = AnsiChar;
  BCHAR = AnsiChar;
  WCHAR = WideChar;

     LOGFONT = record
          lfHeight : LONG;
          lfWidth : LONG;
          lfEscapement : LONG;
          lfOrientation : LONG;
          lfWeight : LONG;
          lfItalic : BYTE;
          lfUnderline : BYTE;
          lfStrikeOut : BYTE;
          lfCharSet : BYTE;
          lfOutPrecision : BYTE;
          lfClipPrecision : BYTE;
          lfQuality : BYTE;
          lfPitchAndFamily : BYTE;
          lfFaceName : array[0..(LF_FACESIZE)-1] of TCHAR;
       end;
     LPLOGFONT = ^LOGFONT;
     TLOGFONT = LOGFONT;
     TLOGFONTA = LOGFONT;
     PLOGFONT = ^LOGFONT;
     PLOGFONTA = PLOGFONT;

     LOGFONTW = record
       lfHeight: LONG;
       lfWidth: LONG;
       lfEscapement: LONG;
       lfOrientation: LONG;
       lfWeight: LONG;
       lfItalic: BYTE;
       lfUnderline: BYTE;
       lfStrikeOut: BYTE;
       lfCharSet: BYTE;
       lfOutPrecision: BYTE;
       lfClipPrecision: BYTE;
       lfQuality: BYTE;
       lfPitchAndFamily: BYTE;
       lfFaceName: array [0..LF_FACESIZE - 1] of WCHAR;
     end;
     LPLOGFONTW = ^LOGFONTW;
     NPLOGFONTW = ^LOGFONTW;
     TLogFontW = LOGFONTW;
     PLogFontW = ^TLogFontW;

     COLORADJUSTMENT = record
          caSize : WORD;
          caFlags : WORD;
          caIlluminantIndex : WORD;
          caRedGamma : WORD;
          caGreenGamma : WORD;
          caBlueGamma : WORD;
          caReferenceBlack : WORD;
          caReferenceWhite : WORD;
          caContrast : SHORT;
          caBrightness : SHORT;
          caColorfulness : SHORT;
          caRedGreenTint : SHORT;
       end;
     LPCOLORADJUSTMENT = ^COLORADJUSTMENT;
     tagCOLORADJUSTMENT = COLORADJUSTMENT;
     TCOLORADJUSTMENT = COLORADJUSTMENT;
     PCOLORADJUSTMENT = ^COLORADJUSTMENT;

     COLORMAP = record
          from : COLORREF;
          _to : COLORREF;
       end;
     LPCOLORMAP = ^COLORMAP;
     _COLORMAP = COLORMAP;
     TCOLORMAP = COLORMAP;
     PCOLORMAP = ^COLORMAP;

     RGBQUAD = record
          rgbBlue : BYTE;
          rgbGreen : BYTE;
          rgbRed : BYTE;
          rgbReserved : BYTE;
       end;
     tagRGBQUAD = RGBQUAD;
     TRGBQUAD = RGBQUAD;
     PRGBQUAD = ^RGBQUAD;

     BITMAPINFOHEADER = record
          biSize : DWORD;
          biWidth : LONG;
          biHeight : LONG;
          biPlanes : WORD;
          biBitCount : WORD;
          biCompression : DWORD;
          biSizeImage : DWORD;
          biXPelsPerMeter : LONG;
          biYPelsPerMeter : LONG;
          biClrUsed : DWORD;
          biClrImportant : DWORD;
       end;
     LPBITMAPINFOHEADER = ^BITMAPINFOHEADER;
     TBITMAPINFOHEADER = BITMAPINFOHEADER;
     PBITMAPINFOHEADER = ^BITMAPINFOHEADER;

     BITMAPINFO = record
          bmiHeader : BITMAPINFOHEADER;
          bmiColors : array[0..0] of RGBQUAD;
       end;
     LPBITMAPINFO = ^BITMAPINFO;
     PBITMAPINFO = ^BITMAPINFO;
     TBITMAPINFO = BITMAPINFO;

     EMR = record
          iType : DWORD;
          nSize : DWORD;
       end;
     tagEMR = EMR;
     TEMR = EMR;
     PEMR = ^EMR;

     EMRANGLEARC = record
          emr : EMR;
          ptlCenter : POINTL;
          nRadius : DWORD;
          eStartAngle : Single;
          eSweepAngle : Single;
       end;
     tagEMRANGLEARC = EMRANGLEARC;
     TEMRANGLEARC = EMRANGLEARC;
     PEMRANGLEARC = ^EMRANGLEARC;

     EMRARC = record
          emr : EMR;
          rclBox : RECTL;
          ptlStart : POINTL;
          ptlEnd : POINTL;
       end;
     tagEMRARC = EMRARC;
     TEMRARC = EMRARC;
     PEMRARC = ^EMRARC;

     EMRARCTO = EMRARC;
     TEMRARCTO = EMRARC;
     PEMRARCTO = ^EMRARC;

     EMRCHORD = EMRARC;
     TEMRCHORD = EMRARC;
     PEMRCHORD = ^EMRARC;

     EMRPIE = EMRARC;
     TEMRPIE = EMRARC;
     PEMRPIE = ^EMRARC;

     XFORM = record
          eM11 : Single;
          eM12 : Single;
          eM21 : Single;
          eM22 : Single;
          eDx : Single;
          eDy : Single;
       end;
     LPXFORM = ^XFORM;
     _XFORM = XFORM;
     TXFORM = XFORM;
     PXFORM = ^XFORM;

     EMRBITBLT = record
          emr : EMR;
          rclBounds : RECTL;
          xDest : LONG;
          yDest : LONG;
          cxDest : LONG;
          cyDest : LONG;
          dwRop : DWORD;
          xSrc : LONG;
          ySrc : LONG;
          xformSrc : XFORM;
          crBkColorSrc : COLORREF;
          iUsageSrc : DWORD;
          offBmiSrc : DWORD;
          offBitsSrc : DWORD;
          cbBitsSrc : DWORD;
       end;
     tagEMRBITBLT = EMRBITBLT;
     TEMRBITBLT = EMRBITBLT;
     PEMRBITBLT = ^EMRBITBLT;

     LOGBRUSH = record
          lbStyle : UINT;
          lbColor : COLORREF;
          lbHatch : LONG;
       end;
     tagLOGBRUSH = LOGBRUSH;
     TLOGBRUSH = LOGBRUSH;
     PLOGBRUSH = ^LOGBRUSH;

     EMRCREATEBRUSHINDIRECT = record
          emr : EMR;
          ihBrush : DWORD;
          lb : LOGBRUSH;
       end;
     tagEMRCREATEBRUSHINDIRECT = EMRCREATEBRUSHINDIRECT;
     TEMRCREATEBRUSHINDIRECT = EMRCREATEBRUSHINDIRECT;
     PEMRCREATEBRUSHINDIRECT = ^EMRCREATEBRUSHINDIRECT;

     LCSCSTYPE = LONG;

     LCSGAMUTMATCH = LONG;

     EMRCREATEDIBPATTERNBRUSHPT = record
          emr : EMR;
          ihBrush : DWORD;
          iUsage : DWORD;
          offBmi : DWORD;
          cbBmi : DWORD;
          offBits : DWORD;
          cbBits : DWORD;
       end;
     tagEMRCREATEDIBPATTERNBRUSHPT = EMRCREATEDIBPATTERNBRUSHPT;
     TEMRCREATEDIBPATTERNBRUSHPT = EMRCREATEDIBPATTERNBRUSHPT;
     PEMRCREATEDIBPATTERNBRUSHPT = EMRCREATEDIBPATTERNBRUSHPT;

     EMRCREATEMONOBRUSH = record
          emr : EMR;
          ihBrush : DWORD;
          iUsage : DWORD;
          offBmi : DWORD;
          cbBmi : DWORD;
          offBits : DWORD;
          cbBits : DWORD;
       end;
     tagEMRCREATEMONOBRUSH = EMRCREATEMONOBRUSH;
     TEMRCREATEMONOBRUSH = EMRCREATEMONOBRUSH;
     PEMRCREATEMONOBRUSH = ^EMRCREATEMONOBRUSH;

     PALETTEENTRY = record
          peRed : BYTE;
          peGreen : BYTE;
          peBlue : BYTE;
          peFlags : BYTE;
       end;
     LPPALETTEENTRY = ^PALETTEENTRY;
     tagPALETTEENTRY = PALETTEENTRY;
     TPALETTEENTRY = PALETTEENTRY;
     PPALETTEENTRY = ^PALETTEENTRY;

     LOGPALETTE = record
          palVersion : WORD;
          palNumEntries : WORD;
          palPalEntry : array[0..0] of PALETTEENTRY;
       end;
     LPLOGPALETTE = ^LOGPALETTE;
     tagLOGPALETTE = LOGPALETTE;
     TLOGPALETTE = LOGPALETTE;
     PLOGPALETTE = ^LOGPALETTE;

     EMRCREATEPALETTE = record
          emr : EMR;
          ihPal : DWORD;
          lgpl : LOGPALETTE;
       end;
     tagEMRCREATEPALETTE = EMRCREATEPALETTE;
     TEMRCREATEPALETTE = EMRCREATEPALETTE;
     PEMRCREATEPALETTE = ^EMRCREATEPALETTE;

     LOGPEN = record
          lopnStyle : UINT;
          lopnWidth : POINT;
          lopnColor : COLORREF;
       end;
     tagLOGPEN = LOGPEN;
     TLOGPEN = LOGPEN;
     PLOGPEN = ^LOGPEN;

     EMRCREATEPEN = record
          emr : EMR;
          ihPen : DWORD;
          lopn : LOGPEN;
       end;
     tagEMRCREATEPEN = EMRCREATEPEN;
     TEMRCREATEPEN = EMRCREATEPEN;
     PEMRCREATEPEN = ^EMRCREATEPEN;

     EMRELLIPSE = record
          emr : EMR;
          rclBox : RECTL;
       end;
     tagEMRELLIPSE = EMRELLIPSE;
     TEMRELLIPSE = EMRELLIPSE;
     PEMRELLIPSE = ^EMRELLIPSE;

     EMRRECTANGLE = EMRELLIPSE;
     TEMRRECTANGLE = EMRELLIPSE;
     PEMRRECTANGLE = ^EMRELLIPSE;

     EMREOF = record
          emr : EMR;
          nPalEntries : DWORD;
          offPalEntries : DWORD;
          nSizeLast : DWORD;
       end;
     tagEMREOF = EMREOF;
     TEMREOF = EMREOF;
     PEMREOF = ^EMREOF;

     EMREXCLUDECLIPRECT = record
          emr : EMR;
          rclClip : RECTL;
       end;
     tagEMREXCLUDECLIPRECT = EMREXCLUDECLIPRECT;
     TEMREXCLUDECLIPRECT = EMREXCLUDECLIPRECT;
     PEMREXCLUDECLIPRECT = ^EMREXCLUDECLIPRECT;

     EMRINTERSECTCLIPRECT = EMREXCLUDECLIPRECT;
     TEMRINTERSECTCLIPRECT = EMREXCLUDECLIPRECT;
     PEMRINTERSECTCLIPRECT = ^EMREXCLUDECLIPRECT;

     PANOSE = record
          bFamilyType : BYTE;
          bSerifStyle : BYTE;
          bWeight : BYTE;
          bProportion : BYTE;
          bContrast : BYTE;
          bStrokeVariation : BYTE;
          bArmStyle : BYTE;
          bLetterform : BYTE;
          bMidline : BYTE;
          bXHeight : BYTE;
       end;
     tagPANOSE = PANOSE;
     TPANOSE = PANOSE;
     PPANOSE = ^PANOSE;

     EXTLOGFONT = record
          elfLogFont : LOGFONTW;
          elfFullName : array[0..(LF_FULLFACESIZE)-1] of BCHAR;
          elfStyle : array[0..(LF_FACESIZE)-1] of BCHAR;
          elfVersion : DWORD;
          elfStyleSize : DWORD;
          elfMatch : DWORD;
          elfReserved : DWORD;
          elfVendorId : array[0..(ELF_VENDOR_SIZE)-1] of BYTE;
          elfCulture : DWORD;
          elfPanose : PANOSE;
       end;
     tagEXTLOGFONT = EXTLOGFONT;
     TEXTLOGFONT = EXTLOGFONT;
     PEXTLOGFONT = ^EXTLOGFONT;

     EMREXTCREATEFONTINDIRECTW = record
          emr : EMR;
          ihFont : DWORD;
          elfw : EXTLOGFONT;
       end;
     tagEMREXTCREATEFONTINDIRECTW = EMREXTCREATEFONTINDIRECTW;
     TEMREXTCREATEFONTINDIRECTW = EMREXTCREATEFONTINDIRECTW;
     PEMREXTCREATEFONTINDIRECTW = ^EMREXTCREATEFONTINDIRECTW;


     EXTLOGPEN = record
          elpPenStyle : UINT;
          elpWidth : UINT;
          elpBrushStyle : UINT;
          elpColor : COLORREF;
          elpHatch : LONG;
          elpNumEntries : DWORD;
          elpStyleEntry : array[0..0] of DWORD;
       end;
     tagEXTLOGPEN = EXTLOGPEN;
     TEXTLOGPEN = EXTLOGPEN;
     PEXTLOGPEN = ^EXTLOGPEN;

     EMREXTCREATEPEN = record
          emr : EMR;
          ihPen : DWORD;
          offBmi : DWORD;
          cbBmi : DWORD;
          offBits : DWORD;
          cbBits : DWORD;
          elp : EXTLOGPEN;
       end;
     tagEMREXTCREATEPEN = EMREXTCREATEPEN;
     TEMREXTCREATEPEN = EMREXTCREATEPEN;
     PEMREXTCREATEPEN = ^EMREXTCREATEPEN;

     EMREXTFLOODFILL = record
          emr : EMR;
          ptlStart : POINTL;
          crColor : COLORREF;
          iMode : DWORD;
       end;
     tagEMREXTFLOODFILL = EMREXTFLOODFILL;
     TEMREXTFLOODFILL = EMREXTFLOODFILL;
     PEMREXTFLOODFILL = ^EMREXTFLOODFILL;

     EMREXTSELECTCLIPRGN = record
          emr : EMR;
          cbRgnData : DWORD;
          iMode : DWORD;
          RgnData : array[0..0] of BYTE;
       end;
     tagEMREXTSELECTCLIPRGN = EMREXTSELECTCLIPRGN;
     TEMREXTSELECTCLIPRGN = EMREXTSELECTCLIPRGN;
     PEMREXTSELECTCLIPRGN = ^EMREXTSELECTCLIPRGN;

     EMRTEXT = record
          ptlReference : POINTL;
          nChars : DWORD;
          offString : DWORD;
          fOptions : DWORD;
          rcl : RECTL;
          offDx : DWORD;
       end;
     tagEMRTEXT = EMRTEXT;
     TEMRTEXT = EMRTEXT;
     PEMRTEXT = ^EMRTEXT;

     EMREXTTEXTOUTA = record
          emr : EMR;
          rclBounds : RECTL;
          iGraphicsMode : DWORD;
          exScale : Single;
          eyScale : Single;
          emrtext : EMRTEXT;
       end;
     tagEMREXTTEXTOUTA = EMREXTTEXTOUTA;
     TEMREXTTEXTOUTA = EMREXTTEXTOUTA;
     PEMREXTTEXTOUTA = ^EMREXTTEXTOUTA;

     EMREXTTEXTOUTW = EMREXTTEXTOUTA;
     TEMREXTTEXTOUTW = EMREXTTEXTOUTA;
     PEMREXTTEXTOUTW = ^EMREXTTEXTOUTA;

     EMRFILLPATH = record
          emr : EMR;
          rclBounds : RECTL;
       end;
     tagEMRFILLPATH = EMRFILLPATH;
     TEMRFILLPATH = EMRFILLPATH;
     PEMRFILLPATH = ^EMRFILLPATH;

     EMRSTROKEANDFILLPATH = EMRFILLPATH;
     TEMRSTROKEANDFILLPATH = EMRFILLPATH;
     PEMRSTROKEANDFILLPATH = ^EMRFILLPATH;

     EMRSTROKEPATH = EMRFILLPATH;
     TEMRSTROKEPATH = EMRFILLPATH;
     PEMRSTROKEPATH = ^EMRFILLPATH;

     EMRFILLRGN = record
          emr : EMR;
          rclBounds : RECTL;
          cbRgnData : DWORD;
          ihBrush : DWORD;
          RgnData : array[0..0] of BYTE;
       end;
     tagEMRFILLRGN = EMRFILLRGN;
     TEMRFILLRGN = EMRFILLRGN;
     PEMRFILLRGN = ^EMRFILLRGN;

     EMRFORMAT = record
          dSignature : DWORD;
          nVersion : DWORD;
          cbData : DWORD;
          offData : DWORD;
       end;
     tagEMRFORMAT = EMRFORMAT;
     TEMRFORMAT = EMRFORMAT;
     PEMRFORMAT = ^EMRFORMAT;

     SIZE = record
          cx : LONG;
          cy : LONG;
       end;
     LPSIZE = ^SIZE;
     tagSIZE = SIZE;
     TSIZE = SIZE;
     PSIZE = ^SIZE;

     SIZEL = SIZE;
     TSIZEL = SIZE;
     PSIZEL = ^SIZE;
     LPSIZEL = ^SIZE;

     EMRFRAMERGN = record
          emr : EMR;
          rclBounds : RECTL;
          cbRgnData : DWORD;
          ihBrush : DWORD;
          szlStroke : SIZEL;
          RgnData : array[0..0] of BYTE;
       end;
     tagEMRFRAMERGN = EMRFRAMERGN;
     TEMRFRAMERGN = EMRFRAMERGN;
     PEMRFRAMERGN = ^EMRFRAMERGN;

     EMRGDICOMMENT = record
          emr : EMR;
          cbData : DWORD;
          Data : array[0..0] of BYTE;
       end;
     tagEMRGDICOMMENT = EMRGDICOMMENT;
     TEMRGDICOMMENT = EMRGDICOMMENT;
     PEMRGDICOMMENT = ^EMRGDICOMMENT;

     EMRINVERTRGN = record
          emr : EMR;
          rclBounds : RECTL;
          cbRgnData : DWORD;
          RgnData : array[0..0] of BYTE;
       end;
     tagEMRINVERTRGN = EMRINVERTRGN;
     TEMRINVERTRGN = EMRINVERTRGN;
     PEMRINVERTRGN = ^EMRINVERTRGN;

     EMRPAINTRGN = EMRINVERTRGN;
     TEMRPAINTRGN = EMRINVERTRGN;
     PEMRPAINTRGN = ^EMRINVERTRGN;

     EMRLINETO = record
          emr : EMR;
          ptl : POINTL;
       end;
     tagEMRLINETO = EMRLINETO;
     TEMRLINETO = EMRLINETO;
     PEMRLINETO = ^EMRLINETO;

     EMRMOVETOEX = EMRLINETO;
     TEMRMOVETOEX = EMRLINETO;
     PEMRMOVETOEX = ^EMRLINETO;

     EMRMASKBLT = record
          emr : EMR;
          rclBounds : RECTL;
          xDest : LONG;
          yDest : LONG;
          cxDest : LONG;
          cyDest : LONG;
          dwRop : DWORD;
          xSrc : LONG;
          ySrc : LONG;
          xformSrc : XFORM;
          crBkColorSrc : COLORREF;
          iUsageSrc : DWORD;
          offBmiSrc : DWORD;
          cbBmiSrc : DWORD;
          offBitsSrc : DWORD;
          cbBitsSrc : DWORD;
          xMask : LONG;
          yMask : LONG;
          iUsageMask : DWORD;
          offBmiMask : DWORD;
          cbBmiMask : DWORD;
          offBitsMask : DWORD;
          cbBitsMask : DWORD;
       end;
     tagEMRMASKBLT = EMRMASKBLT;
     TEMRMASKBLT = EMRMASKBLT;
     PEMRMASKBLT = ^EMRMASKBLT;

     EMRMODIFYWORLDTRANSFORM = record
          emr : EMR;
          xform : XFORM;
          iMode : DWORD;
       end;
     tagEMRMODIFYWORLDTRANSFORM = EMRMODIFYWORLDTRANSFORM;
     TEMRMODIFYWORLDTRANSFORM = EMRMODIFYWORLDTRANSFORM;
     PEMRMODIFYWORLDTRANSFORM = EMRMODIFYWORLDTRANSFORM;

     EMROFFSETCLIPRGN = record
          emr : EMR;
          ptlOffset : POINTL;
       end;
     tagEMROFFSETCLIPRGN = EMROFFSETCLIPRGN;
     TEMROFFSETCLIPRGN = EMROFFSETCLIPRGN;
     PEMROFFSETCLIPRGN = ^EMROFFSETCLIPRGN;

     EMRPLGBLT = record
          emr : EMR;
          rclBounds : RECTL;
          aptlDest : array[0..2] of POINTL;
          xSrc : LONG;
          ySrc : LONG;
          cxSrc : LONG;
          cySrc : LONG;
          xformSrc : XFORM;
          crBkColorSrc : COLORREF;
          iUsageSrc : DWORD;
          offBmiSrc : DWORD;
          cbBmiSrc : DWORD;
          offBitsSrc : DWORD;
          cbBitsSrc : DWORD;
          xMask : LONG;
          yMask : LONG;
          iUsageMask : DWORD;
          offBmiMask : DWORD;
          cbBmiMask : DWORD;
          offBitsMask : DWORD;
          cbBitsMask : DWORD;
       end;
     tagEMRPLGBLT = EMRPLGBLT;
     TEMRPLGBLT = EMRPLGBLT;
     PEMRPLGBLT = ^EMRPLGBLT;

     EMRPOLYDRAW = record
          emr : EMR;
          rclBounds : RECTL;
          cptl : DWORD;
          aptl : array[0..0] of POINTL;
          abTypes : array[0..0] of BYTE;
       end;
     tagEMRPOLYDRAW = EMRPOLYDRAW;
     TEMRPOLYDRAW = EMRPOLYDRAW;
     PEMRPOLYDRAW = ^EMRPOLYDRAW;

     EMRPOLYDRAW16 = record
          emr : EMR;
          rclBounds : RECTL;
          cpts : DWORD;
          apts : array[0..0] of POINTS;
          abTypes : array[0..0] of BYTE;
       end;
     tagEMRPOLYDRAW16 = EMRPOLYDRAW16;
     TEMRPOLYDRAW16 = EMRPOLYDRAW16;
     PEMRPOLYDRAW16 = ^EMRPOLYDRAW16;

     EMRPOLYLINE = record
          emr : EMR;
          rclBounds : RECTL;
          cptl : DWORD;
          aptl : array[0..0] of POINTL;
       end;
     tagEMRPOLYLINE = EMRPOLYLINE;
     TEMRPOLYLINE = EMRPOLYLINE;
     PEMRPOLYLINE = ^EMRPOLYLINE;

     EMRPOLYBEZIER = EMRPOLYLINE;
     TEMRPOLYBEZIER = EMRPOLYLINE;
     PEMRPOLYBEZIER = ^EMRPOLYLINE;

     EMRPOLYGON = EMRPOLYLINE;
     TEMRPOLYGON = EMRPOLYLINE;
     PEMRPOLYGON = ^EMRPOLYLINE;

     EMRPOLYBEZIERTO = EMRPOLYLINE;
     TEMRPOLYBEZIERTO = EMRPOLYLINE;
     PEMRPOLYBEZIERTO = ^EMRPOLYLINE;

     EMRPOLYLINETO = EMRPOLYLINE;
     TEMRPOLYLINETO = EMRPOLYLINE;
     PEMRPOLYLINETO = ^EMRPOLYLINE;

     EMRPOLYLINE16 = record
          emr : EMR;
          rclBounds : RECTL;
          cpts : DWORD;
          apts : array[0..0] of POINTL;
       end;
     tagEMRPOLYLINE16 = EMRPOLYLINE16;
     TEMRPOLYLINE16 = EMRPOLYLINE16;
     PEMRPOLYLINE16 = ^EMRPOLYLINE16;

     EMRPOLYBEZIER16 = EMRPOLYLINE16;
     TEMRPOLYBEZIER16 = EMRPOLYLINE16;
     PEMRPOLYBEZIER16 = ^EMRPOLYLINE16;

     EMRPOLYGON16 = EMRPOLYLINE16;
     TEMRPOLYGON16 = EMRPOLYLINE16;
     PEMRPOLYGON16 = ^EMRPOLYLINE16;

     EMRPOLYBEZIERTO16 = EMRPOLYLINE16;
     TEMRPOLYBEZIERTO16 = EMRPOLYLINE16;
     PEMRPOLYBEZIERTO16 = ^EMRPOLYLINE16;

     EMRPOLYLINETO16 = EMRPOLYLINE16;
     TEMRPOLYLINETO16 = EMRPOLYLINE16;
     PEMRPOLYLINETO16 = ^EMRPOLYLINE16;

     EMRPOLYPOLYLINE = record
          emr : EMR;
          rclBounds : RECTL;
          nPolys : DWORD;
          cptl : DWORD;
          aPolyCounts : array[0..0] of DWORD;
          aptl : array[0..0] of POINTL;
       end;
     tagEMRPOLYPOLYLINE = EMRPOLYPOLYLINE;
     TEMRPOLYPOLYLINE = EMRPOLYPOLYLINE;
     PEMRPOLYPOLYLINE = ^EMRPOLYPOLYLINE;

     EMRPOLYPOLYGON = EMRPOLYPOLYLINE;
     TEMRPOLYPOLYGON = EMRPOLYPOLYLINE;
     PEMRPOLYPOLYGON = ^EMRPOLYPOLYLINE;

     EMRPOLYPOLYLINE16 = record
          emr : EMR;
          rclBounds : RECTL;
          nPolys : DWORD;
          cpts : DWORD;
          aPolyCounts : array[0..0] of DWORD;
          apts : array[0..0] of POINTS;
       end;
     tagEMRPOLYPOLYLINE16 = EMRPOLYPOLYLINE16;
     TEMRPOLYPOLYLINE16 = EMRPOLYPOLYLINE16;
     PEMRPOLYPOLYLINE16 = ^EMRPOLYPOLYLINE16;

     EMRPOLYPOLYGON16 = EMRPOLYPOLYLINE16;
     TEMRPOLYPOLYGON16 = EMRPOLYPOLYLINE16;
     PEMRPOLYPOLYGON16 = ^EMRPOLYPOLYLINE16;

     EMRPOLYTEXTOUTA = record
          emr : EMR;
          rclBounds : RECTL;
          iGraphicsMode : DWORD;
          exScale : Single;
          eyScale : Single;
          cStrings : LONG;
          aemrtext : array[0..0] of EMRTEXT;
       end;
     tagEMRPOLYTEXTOUTA = EMRPOLYTEXTOUTA;
     TEMRPOLYTEXTOUTA = EMRPOLYTEXTOUTA;
     PEMRPOLYTEXTOUTA = ^EMRPOLYTEXTOUTA;

     EMRPOLYTEXTOUTW = EMRPOLYTEXTOUTA;
     TEMRPOLYTEXTOUTW = EMRPOLYTEXTOUTA;
     PEMRPOLYTEXTOUTW = ^EMRPOLYTEXTOUTA;

     EMRRESIZEPALETTE = record
          emr : EMR;
          ihPal : DWORD;
          cEntries : DWORD;
       end;
     tagEMRRESIZEPALETTE = EMRRESIZEPALETTE;
     TEMRRESIZEPALETTE = EMRRESIZEPALETTE;
     PEMRRESIZEPALETTE = ^EMRRESIZEPALETTE;

     EMRRESTOREDC = record
          emr : EMR;
          iRelative : LONG;
       end;
     tagEMRRESTOREDC = EMRRESTOREDC;
     TEMRRESTOREDC = EMRRESTOREDC;
     PEMRRESTOREDC = ^EMRRESTOREDC;

     EMRROUNDRECT = record
          emr : EMR;
          rclBox : RECTL;
          szlCorner : SIZEL;
       end;
     tagEMRROUNDRECT = EMRROUNDRECT;
     TEMRROUNDRECT = EMRROUNDRECT;
     PEMRROUNDRECT = ^EMRROUNDRECT;

     EMRSCALEVIEWPORTEXTEX = record
          emr : EMR;
          xNum : LONG;
          xDenom : LONG;
          yNum : LONG;
          yDenom : LONG;
       end;
     tagEMRSCALEVIEWPORTEXTEX = EMRSCALEVIEWPORTEXTEX;
     TEMRSCALEVIEWPORTEXTEX = EMRSCALEVIEWPORTEXTEX;
     PEMRSCALEVIEWPORTEXTEX = ^EMRSCALEVIEWPORTEXTEX;

     EMRSCALEWINDOWEXTEX = EMRSCALEVIEWPORTEXTEX;
     TEMRSCALEWINDOWEXTEX = EMRSCALEVIEWPORTEXTEX;
     PEMRSCALEWINDOWEXTEX = ^EMRSCALEVIEWPORTEXTEX;

     EMRSELECTCOLORSPACE = record
          emr : EMR;
          ihCS : DWORD;
       end;
     tagEMRSELECTCOLORSPACE = EMRSELECTCOLORSPACE;
     TEMRSELECTCOLORSPACE = EMRSELECTCOLORSPACE;
     PEMRSELECTCOLORSPACE = ^EMRSELECTCOLORSPACE;

     EMRDELETECOLORSPACE = EMRSELECTCOLORSPACE;
     TEMRDELETECOLORSPACE = EMRSELECTCOLORSPACE;
     PEMRDELETECOLORSPACE = ^EMRSELECTCOLORSPACE;

     EMRSELECTOBJECT = record
          emr : EMR;
          ihObject : DWORD;
       end;
     tagEMRSELECTOBJECT = EMRSELECTOBJECT;
     TEMRSELECTOBJECT = EMRSELECTOBJECT;
     PEMRSELECTOBJECT = ^EMRSELECTOBJECT;

     EMRDELETEOBJECT = EMRSELECTOBJECT;
     TEMRDELETEOBJECT = EMRSELECTOBJECT;
     PEMRDELETEOBJECT = ^EMRSELECTOBJECT;

     EMRSELECTPALETTE = record
          emr : EMR;
          ihPal : DWORD;
       end;
     tagEMRSELECTPALETTE = EMRSELECTPALETTE;
     TEMRSELECTPALETTE = EMRSELECTPALETTE;
     PEMRSELECTPALETTE = ^EMRSELECTPALETTE;

     EMRSETARCDIRECTION = record
          emr : EMR;
          iArcDirection : DWORD;
       end;
     tagEMRSETARCDIRECTION = EMRSETARCDIRECTION;
     TEMRSETARCDIRECTION = EMRSETARCDIRECTION;
     PEMRSETARCDIRECTION = ^EMRSETARCDIRECTION;

     EMRSETBKCOLOR = record
          emr : EMR;
          crColor : COLORREF;
       end;
     tagEMRSETTEXTCOLOR = EMRSETBKCOLOR;
     TEMRSETBKCOLOR = EMRSETBKCOLOR;
     PEMRSETBKCOLOR = ^EMRSETBKCOLOR;

     EMRSETTEXTCOLOR = EMRSETBKCOLOR;
     TEMRSETTEXTCOLOR = EMRSETBKCOLOR;
     PEMRSETTEXTCOLOR = ^EMRSETBKCOLOR;

     EMRSETCOLORADJUSTMENT = record
          emr : EMR;
          ColorAdjustment : COLORADJUSTMENT;
       end;
     tagEMRSETCOLORADJUSTMENT = EMRSETCOLORADJUSTMENT;
     TEMRSETCOLORADJUSTMENT = EMRSETCOLORADJUSTMENT;
     PEMRSETCOLORADJUSTMENT = ^EMRSETCOLORADJUSTMENT;

     EMRSETDIBITSTODEVICE = record
          emr : EMR;
          rclBounds : RECTL;
          xDest : LONG;
          yDest : LONG;
          xSrc : LONG;
          ySrc : LONG;
          cxSrc : LONG;
          cySrc : LONG;
          offBmiSrc : DWORD;
          cbBmiSrc : DWORD;
          offBitsSrc : DWORD;
          cbBitsSrc : DWORD;
          iUsageSrc : DWORD;
          iStartScan : DWORD;
          cScans : DWORD;
       end;
     tagEMRSETDIBITSTODEVICE = EMRSETDIBITSTODEVICE;
     TEMRSETDIBITSTODEVICE = EMRSETDIBITSTODEVICE;
     PEMRSETDIBITSTODEVICE = ^EMRSETDIBITSTODEVICE;

     EMRSETMAPPERFLAGS = record
          emr : EMR;
          dwFlags : DWORD;
       end;
     tagEMRSETMAPPERFLAGS = EMRSETMAPPERFLAGS;
     TEMRSETMAPPERFLAGS = EMRSETMAPPERFLAGS;
     PEMRSETMAPPERFLAGS = ^EMRSETMAPPERFLAGS;

     EMRSETMITERLIMIT = record
          emr : EMR;
          eMiterLimit : Single;
       end;
     tagEMRSETMITERLIMIT = EMRSETMITERLIMIT;
     TEMRSETMITERLIMIT = EMRSETMITERLIMIT;
     PEMRSETMITERLIMIT = ^EMRSETMITERLIMIT;

     EMRSETPALETTEENTRIES = record
          emr : EMR;
          ihPal : DWORD;
          iStart : DWORD;
          cEntries : DWORD;
          aPalEntries : array[0..0] of PALETTEENTRY;
       end;
     tagEMRSETPALETTEENTRIES = EMRSETPALETTEENTRIES;
     TEMRSETPALETTEENTRIES = EMRSETPALETTEENTRIES;
     PEMRSETPALETTEENTRIES = ^EMRSETPALETTEENTRIES;

     EMRSETPIXELV = record
          emr : EMR;
          ptlPixel : POINTL;
          crColor : COLORREF;
       end;
     tagEMRSETPIXELV = EMRSETPIXELV;
     TEMRSETPIXELV = EMRSETPIXELV;
     PEMRSETPIXELV = ^EMRSETPIXELV;

     EMRSETVIEWPORTEXTEX = record
          emr : EMR;
          szlExtent : SIZEL;
       end;
     tagEMRSETVIEWPORTEXTEX = EMRSETVIEWPORTEXTEX;
     TEMRSETVIEWPORTEXTEX = EMRSETVIEWPORTEXTEX;
     PEMRSETVIEWPORTEXTEX = ^EMRSETVIEWPORTEXTEX;

     EMRSETWINDOWEXTEX = EMRSETVIEWPORTEXTEX;
     TEMRSETWINDOWEXTEX = EMRSETVIEWPORTEXTEX;
     PEMRSETWINDOWEXTEX = ^EMRSETVIEWPORTEXTEX;

     EMRSETVIEWPORTORGEX = record
          emr : EMR;
          ptlOrigin : POINTL;
       end;
     tagEMRSETVIEWPORTORGEX = EMRSETVIEWPORTORGEX;
     TEMRSETVIEWPORTORGEX = EMRSETVIEWPORTORGEX;
     PEMRSETVIEWPORTORGEX = ^EMRSETVIEWPORTORGEX;

     EMRSETWINDOWORGEX = EMRSETVIEWPORTORGEX;
     TEMRSETWINDOWORGEX = EMRSETVIEWPORTORGEX;
     PEMRSETWINDOWORGEX = ^EMRSETVIEWPORTORGEX;

     EMRSETBRUSHORGEX = EMRSETVIEWPORTORGEX;
     TEMRSETBRUSHORGEX = EMRSETVIEWPORTORGEX;
     PEMRSETBRUSHORGEX = ^EMRSETVIEWPORTORGEX;

     EMRSETWORLDTRANSFORM = record
          emr : EMR;
          xform : XFORM;
       end;
     tagEMRSETWORLDTRANSFORM = EMRSETWORLDTRANSFORM;
     TEMRSETWORLDTRANSFORM = EMRSETWORLDTRANSFORM;
     PEMRSETWORLDTRANSFORM = ^EMRSETWORLDTRANSFORM;

     EMRSTRETCHBLT = record
          emr : EMR;
          rclBounds : RECTL;
          xDest : LONG;
          yDest : LONG;
          cxDest : LONG;
          cyDest : LONG;
          dwRop : DWORD;
          xSrc : LONG;
          ySrc : LONG;
          xformSrc : XFORM;
          crBkColorSrc : COLORREF;
          iUsageSrc : DWORD;
          offBmiSrc : DWORD;
          cbBmiSrc : DWORD;
          offBitsSrc : DWORD;
          cbBitsSrc : DWORD;
          cxSrc : LONG;
          cySrc : LONG;
       end;
     tagEMRSTRETCHBLT = EMRSTRETCHBLT;
     TEMRSTRETCHBLT = EMRSTRETCHBLT;
     PEMRSTRETCHBLT = ^EMRSTRETCHBLT;

     EMRSTRETCHDIBITS = record
          emr : EMR;
          rclBounds : RECTL;
          xDest : LONG;
          yDest : LONG;
          xSrc : LONG;
          ySrc : LONG;
          cxSrc : LONG;
          cySrc : LONG;
          offBmiSrc : DWORD;
          cbBmiSrc : DWORD;
          offBitsSrc : DWORD;
          cbBitsSrc : DWORD;
          iUsageSrc : DWORD;
          dwRop : DWORD;
          cxDest : LONG;
          cyDest : LONG;
       end;
     tagEMRSTRETCHDIBITS = EMRSTRETCHDIBITS;
     TEMRSTRETCHDIBITS = EMRSTRETCHDIBITS;
     PEMRSTRETCHDIBITS = ^EMRSTRETCHDIBITS;

     EMRABORTPATH = record
          emr : EMR;
       end;
     TEMRABORTPATH = EMRABORTPATH;
     PEMRABORTPATH = ^EMRABORTPATH;

     tagABORTPATH = EMRABORTPATH;
     TABORTPATH = EMRABORTPATH;

     EMRBEGINPATH = EMRABORTPATH;
     TEMRBEGINPATH = EMRABORTPATH;
     PEMRBEGINPATH = ^EMRABORTPATH;

     EMRENDPATH = EMRABORTPATH;
     TEMRENDPATH = EMRABORTPATH;
     PEMRENDPATH = ^EMRABORTPATH;

     EMRCLOSEFIGURE = EMRABORTPATH;
     TEMRCLOSEFIGURE = EMRABORTPATH;
     PEMRCLOSEFIGURE = ^EMRABORTPATH;

     EMRFLATTENPATH = EMRABORTPATH;
     TEMRFLATTENPATH = EMRABORTPATH;
     PEMRFLATTENPATH = ^EMRABORTPATH;

     EMRWIDENPATH = EMRABORTPATH;
     TEMRWIDENPATH = EMRABORTPATH;
     PEMRWIDENPATH = ^EMRABORTPATH;

     EMRSETMETARGN = EMRABORTPATH;
     TEMRSETMETARGN = EMRABORTPATH;
     PEMRSETMETARGN = ^EMRABORTPATH;

     EMRSAVEDC = EMRABORTPATH;
     TEMRSAVEDC = EMRABORTPATH;
     PEMRSAVEDC = ^EMRABORTPATH;

     EMRREALIZEPALETTE = EMRABORTPATH;
     TEMRREALIZEPALETTE = EMRABORTPATH;
     PEMRREALIZEPALETTE = ^EMRABORTPATH;

     EMRSELECTCLIPPATH = record
          emr : EMR;
          iMode : DWORD;
       end;
     tagEMRSELECTCLIPPATH = EMRSELECTCLIPPATH;
     TEMRSELECTCLIPPATH = EMRSELECTCLIPPATH;
     PEMRSELECTCLIPPATH = ^EMRSELECTCLIPPATH;

     EMRSETBKMODE = EMRSELECTCLIPPATH;
     TEMRSETBKMODE = EMRSELECTCLIPPATH;
     PEMRSETBKMODE = ^EMRSELECTCLIPPATH;

     EMRSETMAPMODE = EMRSELECTCLIPPATH;
     TEMRSETMAPMODE = EMRSELECTCLIPPATH;
     PEMRSETMAPMODE = ^EMRSELECTCLIPPATH;

     EMRSETPOLYFILLMODE = EMRSELECTCLIPPATH;
     TEMRSETPOLYFILLMODE = EMRSELECTCLIPPATH;
     PEMRSETPOLYFILLMODE = ^EMRSELECTCLIPPATH;

     EMRSETROP2 = EMRSELECTCLIPPATH;
     TEMRSETROP2 = EMRSELECTCLIPPATH;
     PEMRSETROP2 = ^EMRSELECTCLIPPATH;

     EMRSETSTRETCHBLTMODE = EMRSELECTCLIPPATH;
     TEMRSETSTRETCHBLTMODE = EMRSELECTCLIPPATH;
     PEMRSETSTRETCHBLTMODE = ^EMRSELECTCLIPPATH;

     EMRSETTEXTALIGN = EMRSELECTCLIPPATH;
     TEMRSETTEXTALIGN = EMRSELECTCLIPPATH;
     PEMRSETTEXTALIGN = ^EMRSELECTCLIPPATH;

     EMRENABLEICM = EMRSELECTCLIPPATH;
     TEMRENABLEICM = EMRSELECTCLIPPATH;
     PEMRENABLEICM = ^EMRSELECTCLIPPATH;


