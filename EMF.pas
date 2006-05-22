unit EMF;

interface

uses Types, SysUtils, Classes, Windows, Graphics, ComCtrls,
  Contnrs, Variants, XUtils, XXmlDom, EMF_Add, Math, StrUtils, Forms,
  Dialogs;

//Integer	–2147483648..2147483647	signed 32-bit
//Cardinal	0..4294967295	unsigned 32-bit
//Shortint	–128..127	signed 8-bit
//Smallint	–32768..32767	signed 16-bit
//Longint	–2147483648..2147483647	signed 32-bit
//Int64	–2^63..2^63–1	signed 64-bit
//Byte	0..255	unsigned 8-bit
//Word	0..65535	unsigned 16-bit
//Longword	0..4294967295	unsigned 32-bit
  //DWORD = Longword; (double word) is used in Windows programming to represent a four-byte integer.
  //Word	0..65535	unsigned 16-bit Longword	0..4294967295	unsigned 32-bit
  //ULong    Int64
  //Long = Long Int (C++) Long modifier 2 x bytes
  //FLOAT = Single  (32-bit)
  //UInt = Longword = DWORD

{$I Symbol.inc}

type

  TMetaRecordInfo = packed record
    Fu: DWORD; // Function number (defined in WINGDI.H) */
    Size: DWORD;
      // Total size of the record in WORDs ? (bytes) */
  end;

  T_EMF_Structure = class(TObjectList)
    Header: T_EMFHeader;
    pLogStrings: TStrings;
    constructor Create;
    procedure LoadFromStream(const Stream: TStream);
    procedure PreparePath;
  end;

  T_EMF_Loader = class
    EMF_Struct: T_EMF_Structure;
    IsOld: Boolean;
    ExtOldX, ExtOldY: Integer;
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromStreamEnh(const Stream: TStream);
    procedure LoadFromMF(const MF: TMetaFile);
    procedure LoadFromStream(const Stream: TStream;
      const IsOld0: Boolean);
    procedure LoadFromFile(const FileName: string);
    procedure FillXML(XML: TXMLDDocument);
  end;

  TFontObject = class
    Font: TLogFontW;
  end;

  TPenObject = class
    Pen: TLogPen;
  end;

  TExtPenObject = class
    ExtPen: TExtLogPen;
  end;

  TBrushObject = class
    Brush: TLogBrush;
  end;

implementation

uses Input, CADSys4, CS4Shapes, ColorEtc, Geometry, SysBasic;


constructor T_EMF_Loader.Create;
begin
  inherited Create;
  EMF_Struct := T_EMF_Structure.Create;
end;

destructor T_EMF_Loader.Destroy;
begin
  EMF_Struct.Free;
  inherited Destroy;
end;

procedure T_EMF_Loader.LoadFromStreamEnh(const Stream: TStream);
begin
  Stream.Position := 0;
  EMF_Struct.LoadFromStream(Stream);
  EMF_Struct.PreparePath;
end;

procedure T_EMF_Loader.LoadFromMF(const MF: TMetaFile);
var
  MemStream: TMemoryStream;
begin
  MemStream := TMemoryStream.Create;
  try
    {Application.MessageBox(PChar(Format('%d %d %d %d',
    [MF.Width, MF.Height,MF.MMWidth, MF.MMHeight])),'',0);}
    if not MF.Enhanced then
    begin
      ExtOldX := MF.Width;
      ExtOldY := MF.Height;
      MF.Enhanced := True;
      IsOld := True;
    end
    else IsOld := False;
//      MF.SaveToFile('C:\WRK\Delphi\TpX\Drawings\-.emf');
    MF.SaveToStream(MemStream);
    //LoadFromStream(MemStream, False);
    LoadFromStreamEnh(MemStream);
  finally
    MemStream.Free;
  end;
end;

procedure T_EMF_Loader.LoadFromStream(const Stream: TStream;
  const IsOld0: Boolean);
var
  MF: TMetaFile;
begin
  Stream.Position := 0;
  if not IsOld then
    LoadFromStreamEnh(Stream)
  else
  begin
    MF := TMetaFile.Create;
    try
      MF.Enhanced := False;
      MF.LoadFromStream(Stream);
      LoadFromMF(MF);
    finally
      MF.Free;
    end;
  end;
  IsOld := IsOld0;
end;

procedure T_EMF_Loader.LoadFromFile(const FileName: string);
var
  Stream: TFileStream;
  MF: TMetaFile;
begin
  if LowerCase(ExtractFileExt(FileName)) = '.emf' then
  begin
    Stream := TFileStream.Create(FileName, fmOpenRead);
    try
      LoadFromStream(Stream, False);
    finally
      Stream.Free;
    end;
  end
  else
  begin
    MF := TMetaFile.Create;
    try
      MF.LoadFromFile(FileName);
      LoadFromMF(MF);
    finally
      MF.Free;
    end;
  end;
end;


function WideStringToString(WSt: Widestring): string;
var
  I, N: Integer;
  WSt2: Widestring;
  St: string;

  function Uni(N: Integer): string;
  begin
    if (N >= 61472) and (N <= 61566)
      then Result := '$' + Symbols1[N - 61471] + '$'
    else if (N >= 61601) and (N <= 61682)
      then Result := '$' + Symbols2[N - 61600] + '$'
    else Result := 'U' + IntToStr(N);
  end;
begin
  Result := '';
  for I := 1 to Length(WSt) do
  begin
    N := Ord(WSt[I]);
    if N < 256 then Result := Result + Chr(N)
    else
    begin
      St := WSt[I];
      WSt2 := St[1];
      if WSt2 = WSt[I] then Result := Result + St
      else Result := Result + Uni(N);
    end;
  end;
end;


     {*---- T_EMF_Structure ----*}

constructor T_EMF_Structure.Create;
begin
  inherited Create;
  OwnsObjects := True;
  pLogStrings := nil;
end;

procedure T_EMF_Structure.LoadFromStream(const Stream:
  TStream);
var
  RecIndex, I: Integer;
  EMR_Info: TEMR;
  ARecord: TEMF_Record;
  LastBeginPath: Integer;
  UseBezier: Boolean;
  APosition: Int64;
begin
  if pLogStrings <> nil then pLogStrings.Clear;
  Stream.ReadBuffer(Header, SizeOf(Header));
  Stream.Seek(Header.RecordSize, soFromBeginning);
  LastBeginPath := -1;
  UseBezier := False;
  for RecIndex := 1 to Header.NumOfRecords - 1 do
  begin
    APosition := Stream.Position;
    Stream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
    if pLogStrings <> nil then
      pLogStrings.Add(EMF_Records[EMR_Info.iType]);
    Stream.Seek(-SizeOf(EMR_Info), soFromCurrent);
    if (EMR_Info.iType > 0) and (EMR_Info.iType <= 97) then
      ARecord := EMF_RecordsClasses[EMR_Info.iType].Create
    else ARecord := TEMF_Skip.Create;
    Add(ARecord);
    ARecord.EMR_Info0 := EMR_Info;
    ARecord.ReadFromStream(Stream);
    Stream.Position := APosition;
    Stream.Seek(EMR_Info.nSize, soFromCurrent);
    if ARecord is TEMF_BeginPath then
    begin
      (ARecord as TEMF_BeginPath).HasStroke := False;
      (ARecord as TEMF_BeginPath).HasFill := False;
      (ARecord as TEMF_BeginPath).HasClip := False;
      LastBeginPath := RecIndex - 1;
      (ARecord as TEMF_BeginPath).UseBezier := False;
      UseBezier := False;
    end
    else if LastBeginPath >= 0 then
    begin
      if ARecord is TEMF_PathBounds then
      begin
        if ARecord is TEMF_StrokePath then
          (Self[LastBeginPath]
            as TEMF_BeginPath).HasStroke := True
        else if ARecord is TEMF_FillPath then
          (Self[LastBeginPath]
            as TEMF_BeginPath).HasFill := True
        else if ARecord is TEMF_StrokeAndFillPath then
        begin
          (Self[LastBeginPath]
            as TEMF_BeginPath).HasStroke := True;
          (Self[LastBeginPath]
            as TEMF_BeginPath).HasFill := True;
        end
        else if ARecord is TEMF_SelectClipPath then
          (Self[LastBeginPath]
            as TEMF_BeginPath).HasClip := True;
        (Self[LastBeginPath]
          as TEMF_BeginPath).UseBezier := UseBezier;
      end
      else if (ARecord is TEMF_PolyBezierTo32)
        or (ARecord is TEMF_PolyBezierTo16)
        or (ARecord is TEMF_ArcTo) then
      begin
        UseBezier := True;
      end
      else if (ARecord is TEMF_PolyDraw16) then
      begin
        UseBezier := False;
        for I := 0 to (ARecord as TEMF_PolyDraw16).EMR_Info.cpts - 1 do
          if (ARecord as TEMF_PolyDraw16).abTypes[I] and 6 = PT_BEZIERTO
            then
          begin
            UseBezier := True;
            Break;
          end;
      end;
    end;
  end;
end;

procedure T_EMF_Structure.PreparePath;
var
  RecIndex, I: Integer;
  ARecord: TEMF_Record;
  LastBeginPath, N: Integer;
  NPointsMult: Longword;
begin
  LastBeginPath := -1;
  N := 0;
  for RecIndex := 1 to Count - 1 do
  //for RecIndex := 1 to Header.NumOfRecords - 2 do
  begin
    //MainForm.StatusBar1.Panels[0].Text := IntToStr(RecIndex) + ' ';
    ARecord := Self[RecIndex] as TEMF_Record;
    if ARecord is TEMF_BeginPath then
    begin
      LastBeginPath := RecIndex;
      (ARecord as TEMF_BeginPath).N := 0;
      N := 0;
      if (ARecord as TEMF_BeginPath).UseBezier
        then NPointsMult := 3 else NPointsMult := 1;
    end;
    if LastBeginPath >= 0 then
    begin
      if ARecord is TEMF_EndPath then
        (Self[LastBeginPath] as TEMF_BeginPath).N := N
      else if ARecord is TEMF_PolylineTo32 then
        Inc(N, (ARecord as TEMF_PolylineTo32).EMR_Info.cptl * NPointsMult)
      else if ARecord is TEMF_PolylineTo16 then
        Inc(N, (ARecord as TEMF_PolylineTo16).EMR_Info.cpts * NPointsMult)
      else if ARecord is TEMF_LineTo then
        Inc(N, NPointsMult)
      else if ARecord is TEMF_MoveToEx then
        Inc(N, 2 * NPointsMult)
      else if ARecord is TEMF_CloseFigure then
        Inc(N, NPointsMult)
      else if ARecord is TEMF_PolyBezierTo32 then
        Inc(N, (ARecord as TEMF_PolyBezierTo32).EMR_Info.cptl * 3)
      else if ARecord is TEMF_PolyBezierTo16 then
        Inc(N, (ARecord as TEMF_PolyBezierTo16).EMR_Info.cpts * 3)
      else if ARecord is TEMF_ArcTo then
        Inc(N, 12 * NPointsMult)
      else if (ARecord is TEMF_PolyDraw16) then
      begin
        for I := 0 to (ARecord as TEMF_PolyDraw16).EMR_Info.cpts - 1 do
        begin
          Inc(N, 1);
          case (ARecord as TEMF_PolyDraw16).abTypes[I] and 6 of
            PT_LINETO: Inc(N, NPointsMult);
            PT_BEZIERTO: Inc(N, 1);
            PT_MOVETO: Inc(N, 2 * NPointsMult);
          else Inc(N, NPointsMult);
          end;
          if (ARecord as TEMF_PolyDraw16).abTypes[I] and 1 > 0 then
            Inc(N, NPointsMult);
        end;
      end;
    end;
  end;
end;

type

  T_DC_Info = class //Device Context information
    IsPenExt: Boolean;
    Pen: TLogPen;
    ExtPen: TExtLogPen;
    Brush: TLogBrush;
    BkMode: Longint;
    MapMode: Longint;
    TextAlignment: DWORD;
    BkColor: TColor;
    TextColor: TColor;
    ViewportOrigin, WindowOrigin: TPoint;
    ViewportExtent, WindowExtent: TSize;
    XScale, YScale: TRealType;
    WorldTr: TTransf2D;
  end;
{The SaveDC function saves the current state of the specified
device context (DC) by copying data describing selected objects
and graphic modes (such as the bitmap, brush, palette, font,
pen, region, drawing mode, and mapping mode) to a context stack.
Background mode	GetBkMode
Drawing mode	GetROP2
Mapping mode	GetMapMode
Polygon-fill mode	GetPolyFillMode
Stretching mode	GetStretchBltMode}

     {*---- T_EMF_Loader ----*}

procedure T_EMF_Loader.FillXML(XML: TXMLDDocument);
var
  RecIndex, I, J: Integer;
  ARecord: TEMF_Record;
  CurrViewportOrigin, CurrWindowOrigin: TPoint;
  CurrViewportExtent, CurrWindowExtent: TSize;
  MMScale, CurrXScale, CurrYScale: TRealType;
  A, R: TRealType;
  CPoint: TPoint;
        //ptlOrigin: TPoint;
    //szlExtent: TSize;
  NChars: DWORD;
  CurrTextAlignment: DWORD;
  St, AString: string;
  XMLNode: TXMLDElement;
  CurrFontHandle, CurrPenHandle, CurrBrushHandle: Longword;
  CurrFontHeight: TRealType;
  CurrFont: TLogFontW;
  IsCurrPenExt: Boolean;
  CurrPen: TLogPen;
  CurrExtPen: TExtLogPen;
  CurrBrush: TLogBrush;
  CurrBkMode: Longint;
  CurrMapMode: Longint;
  CurrBkColor: TColor;
  CurrTextColor: TColor;
  BB: Double;
  CurrXY, MoveToXY: TPoint2D;
  Handles, DC_InfoList: TObjectList;
  HasPath, HasStroke, HasFill, HasClip, UseBezier: Boolean;
  PathArr: array of TPoint2D;
  PathArrPos: Integer;
  BezArr: array[0..3] of TPoint2D;
  P: TPoint2D;
  V: TVector2D;
  BezMod: Byte;
  FirstMoveTo: Boolean;
  XY: TPoint2D;
  CurrWorldTr, TempTr: TTransf2D;
  TmpObj: TObject;
  LineToList: TList;
const
  BezFract: array[0..8] of TRealType =
  (0.01, 0.03, 0.12, 0.31, 0.5, 0.69, 0.88, 0.97, 0.99);
  function XFormToTransf2D(XFORM: TXForm): TTransf2D;
  begin
    with XFORM do
    begin
      Result[1, 1] := eM11;
      Result[1, 2] := eM12;
      Result[1, 3] := 0;
      Result[2, 1] := eM21;
      Result[2, 2] := eM22;
      Result[2, 3] := 0;
      Result[3, 1] := eDx;
      Result[3, 2] := eDy;
      Result[3, 3] := 1;
    end;
  end;
  procedure AddPathPoint0(P: TPoint2D);
  begin
    PathArr[PathArrPos] := P;
    Inc(PathArrPos);
  end;
  procedure AddPathPoint(P: TPoint2D);
  var
    P0: TPoint2D;
  begin
    if UseBezier then
    begin
      P0 := PathArr[PathArrPos - 1];
      PathArr[PathArrPos] := MixPoint(P0, P, 0.3);
      PathArr[PathArrPos + 1] := MixPoint(P0, P, 0.7);
      PathArr[PathArrPos + 2] := P;
      Inc(PathArrPos, 3);
    end
    else AddPathPoint0(P);
  end;
  procedure SetXYScale;
  begin
    case CurrMapMode of
      MM_TEXT: //device pixel
        begin
          CurrXScale := MMScale;
          CurrYScale := MMScale;
        end;
      MM_LOMETRIC: //0.1 millimeter
        begin
          CurrXScale := 0.1;
          CurrYScale := 0.1;
        end;
      MM_HIMETRIC: //0.01 millimeter
        begin
          CurrXScale := 0.01;
          CurrYScale := 0.01;
        end;
      MM_LOENGLISH: //0.01 inch
        begin
          CurrXScale := 0.254;
          CurrYScale := 0.254;
        end;
      MM_HIENGLISH: //0.001 inch
        begin
          CurrXScale := 0.0254;
          CurrYScale := 0.0254;
        end;
      MM_TWIPS: //one twentieth of a printer's point (1/1440 inch, also called a "twip")
        begin
          CurrXScale := 0.017639;
          CurrYScale := 0.017639;
        end;
    else //MM_ISOTROPIC, MM_ANISOTROPIC:
      begin
        if (CurrViewportExtent.CX = 0) or (CurrWindowExtent.CX = 0)
          then CurrXScale := MMScale
        else
          CurrXScale := CurrViewportExtent.CX / CurrWindowExtent.CX * MMScale;
        if (CurrViewportExtent.CY = 0) or (CurrWindowExtent.CY = 0)
          then CurrYScale := MMScale
        else
          CurrYScale := CurrViewportExtent.CY / CurrWindowExtent.CY * MMScale;
      end;
    end;
  end;
  function GetXY0(P: TPoint2D): TPoint2D;
  begin
    XY := TransformPoint2D(P, CurrWorldTr);
    case CurrMapMode of
      MM_TEXT:
        begin
          XY := Point2D(XY.X * MMScale, BB * MMScale - XY.Y * MMScale);
        end;
      MM_ISOTROPIC, MM_ANISOTROPIC:
        begin
          XY := Point2D(XY.X - CurrWindowOrigin.X, XY.Y - CurrWindowOrigin.Y);
          XY := Point2D((CurrViewportOrigin.X * MMScale +
            XY.X * CurrXScale),
            BB * MMScale - (CurrViewportOrigin.Y * MMScale +
            XY.Y * CurrYScale));
        end;
    else
      begin
        XY := Point2D(XY.X * CurrXScale, XY.Y * CurrYScale);
      end;
    end;
    Result := XY;
  end;

  function GetXY(X, Y: TRealType): TPoint2D; overload;
  begin
    Result := GetXY0(Point2D(X, Y));
  end;

  function GetXY(P: TPoint2D): TPoint2D; overload;
  begin
    Result := GetXY0(P);
  end;

  procedure SetHandle(Handle: Longword; Obj: TObject);
  begin
    if (Handle <> 123456789) and (Handle <= Handles.Count) then
      Handles[Handle - 1] := Obj;
  end;

  function GetHandle(Handle: Longword): TObject;
  begin
    if (Handle <> 123456789) and (Handle <= Handles.Count) then
      Result := Handles[Handle - 1]
    else if Handle > 2147483647 then
      Result := Handles[EMF_Struct.Header.NumOfHandles + Handle - 2147483648]
    else Result := nil;
  end;

  function CurrPenWidth: TRealType;
  begin
    if IsCurrPenExt then
    begin
      if (CurrExtPen.elpPenStyle and PS_TYPE_MASK) = PS_GEOMETRIC then
        Result := CurrExtPen.elpWidth * CurrXScale / MMScale
      else Result := CurrExtPen.elpWidth;
    end
    else
    begin
      Result := CurrPen.lopnWidth.X * CurrXScale / MMScale;
    end;
    Result := Result * IsotropicScale(CurrWorldTr);
    if not IsCurrPenExt and (Result < 0.25 { / MMScale})
      and (CurrPen.lopnWidth.X = 1) then
      Result := 0.25 { / MMScale};
    if Result = 0 then Result := 0.25 { / MMScale};
    {if Abs(1 - Round(Result / 0.05) / (Result / 0.05)) < 0.005 then
    begin
      Result := Round(Result / 0.05) * 0.05;
    end;}
  end;

  function CurrLineStyle: TLineStyle;
  var
    CurrPenStyle: Longint;
  begin
    if (HasPath and not HasStroke)
      or (CurrPenHandle = 123456789) then
    begin
      Result := liNone;
      Exit;
    end;
    if IsCurrPenExt then
      CurrPenStyle := CurrExtPen.elpPenStyle and PS_STYLE_MASK
    else
      CurrPenStyle := CurrPen.lopnStyle;
    case CurrPenStyle of
      PS_SOLID: Result := liSolid;
      PS_DASH: Result := liDashed;
      PS_DOT: Result := liDotted;
      PS_DASHDOT: Result := liDashed;
      PS_DASHDOTDOT: Result := liDashed;
      PS_NULL: Result := liNone;
      PS_INSIDEFRAME: Result := liSolid; //??
    else Result := liSolid;
    end;
  end;

  function CurrPenColor: TColor;
  begin
    if IsCurrPenExt then
      Result := CurrExtPen.elpColor
    else
      Result := CurrPen.lopnColor;
  end;

  function CurrHatching: THatching;
  begin
    if (HasPath and not HasFill)
      or (CurrBrushHandle = 123456789) then
    begin
      Result := haNone;
      Exit;
    end;
    case CurrBrush.lbStyle of
      BS_SOLID:
        {if CurrBrush.lbColor <> clWhite then
          Result := haDiagCross else}
        Result := haNone;
          //$02000000  //clNone = TColor($1FFFFFFF);  //clDefault = TColor($20000000);
      BS_NULL: Result := haNone;
      BS_PATTERN: Result := haNone;
      BS_HATCHED:
        case CurrBrush.lbHatch of
          HS_HORIZONTAL: Result := haHorizontal; { ----- }
          HS_VERTICAL: Result := haVertical; { ||||| }
          HS_FDIAGONAL: Result := haFDiagonal; { ///// }
          HS_BDIAGONAL: Result := haBDiagonal; { \\\\\ }
          HS_CROSS: Result := haCross; { +++++ }
          HS_DIAGCROSS: Result := haDiagCross; { xxxxx }
        else Result := haCross;
        end;
    else Result := haCross;
    end;
  end;

  function CurrHatchColor: TColor;
  begin
    if (HasPath and not HasFill)
      or (CurrBrushHandle = 123456789) then
    begin
      Result := clNone;
      Exit;
    end;
    case CurrBrush.lbStyle of
      BS_SOLID, BS_NULL: Result := clNone;
    else
      Result := CurrBrush.lbColor;
    end;
  end;

  function CurrBrushColor: TColor;
  begin
    Result := clNone;
    if (HasPath and not HasFill)
      or (CurrBrushHandle = 123456789) then Exit;
    if CurrBrush.lbStyle = BS_SOLID then
      Result := CurrBrush.lbColor
    else //if CurrBrush.lbStyle = BS_HATCHED then
      if CurrBkMode = OPAQUE then Result := CurrBkColor;
  end;

  procedure SaveDC_Info;
  var
    Info: T_DC_Info;
  begin
    Info := T_DC_Info.Create;
    Info.IsPenExt := IsCurrPenExt;
    if IsCurrPenExt then Info.ExtPen := CurrExtPen
    else Info.Pen := CurrPen;
    Info.Brush := CurrBrush;
    Info.BkMode := CurrBkMode;
    Info.MapMode := CurrMapMode;
    Info.TextAlignment := CurrTextAlignment;
    Info.BkColor := CurrBkColor;
    Info.TextColor := CurrTextColor;
    Info.ViewportOrigin := CurrViewportOrigin;
    Info.WindowOrigin := CurrWindowOrigin;
    Info.ViewportExtent := CurrViewportExtent;
    Info.WindowExtent := CurrWindowExtent;
    Info.XScale := CurrXScale;
    Info.YScale := CurrYScale;
    Info.WorldTr := CurrWorldTr;
    DC_InfoList.Add(Info);
  end;

  procedure RestoreDC_Info;
  var
    Info: T_DC_Info;
  begin
    if DC_InfoList.Count < 1 then Exit;
    Info := DC_InfoList[DC_InfoList.Count - 1] as T_DC_Info;
    IsCurrPenExt := Info.IsPenExt;
    if IsCurrPenExt then CurrExtPen := Info.ExtPen
    else CurrPen := Info.Pen;
    CurrBrush := Info.Brush;
    CurrBkMode := Info.BkMode;
    CurrMapMode := Info.MapMode;
    CurrTextAlignment := Info.TextAlignment;
    CurrBkColor := Info.BkColor;
    CurrTextColor := Info.TextColor;
    CurrViewportOrigin := Info.ViewportOrigin;
    CurrWindowOrigin := Info.WindowOrigin;
    CurrViewportExtent := Info.ViewportExtent;
    CurrWindowExtent := Info.WindowExtent;
    CurrWorldTr := Info.WorldTr;
    CurrXScale := Info.XScale;
    CurrYScale := Info.YScale;
//    SetXYScale;
    DC_InfoList.Delete(DC_InfoList.Count - 1);
    {CurrPenHandle := 0;
    CurrBrushHandle := 0;}
  end;

  procedure WritePrimitiveAttr(LineStyle: TLineStyle;
    LineWidth: TRealType;
    LineColor: TColor; Hatching: THatching;
    HatchColor: TColor;
    const BrushColor: TColor; const Pattern: Boolean;
    XMLNode: TXMLDElement);
  begin
    if {(LineStyle = liNone) and }(Hatching = haNone)
      and (BrushColor = clNone) then
    begin
      //LineStyle := liDashed;
      //LineStyle := liNone;
      //if LineColor = clWhite then
      //LineColor := clLtGray;
      //LineWidth := 0;
      if Pattern {CurrBrush.lbStyle = BS_PATTERN} then
      begin
        Hatching := haDiagCross;
        HatchColor := clLtGray;
      end;
    end;
    // then
    if LineStyle <> liSolid then
      XMLNode.AttributeValue['li'] := GetLineStyleString(LineStyle);
    if Hatching <> haNone then
      XMLNode.AttributeValue['ha'] := Hatching;
    if (Hatching <> haNone) and (HatchColor <> clNone) and (HatchColor <>
      clBlack) then
      XMLNode.AttributeValue['hc'] := ColorToHtml(HatchColor);
    if (LineColor <> clBlack) and (LineStyle <> liNone) and (HatchColor <>
      clBlack) then
      XMLNode.AttributeValue['lc'] := ColorToHtml(LineColor);
    if BrushColor <> clNone then
      XMLNode.AttributeValue['fill'] := ColorToHtml(BrushColor);
    if (LineWidth <> 1) and (LineStyle <> liNone) then
      XMLNode.AttributeValue['lw'] := Format('%.5g', [LineWidth]);
  end;

  procedure AddPathAttributes;
  begin
    if ARecord is TEMF_FillPath then
      WritePrimitiveAttr(liNone, clDefault, 1,
        CurrHatching, CurrHatchColor, CurrBrushColor,
        CurrBrush.lbStyle = BS_PATTERN, XMLNode)
    else if ARecord is TEMF_StrokeAndFillPath then
      WritePrimitiveAttr(CurrLineStyle, CurrPenWidth, CurrPenColor,
        CurrHatching, CurrHatchColor, CurrBrushColor,
        CurrBrush.lbStyle = BS_PATTERN, XMLNode)
    else if ARecord is TEMF_StrokePath then
      WritePrimitiveAttr(CurrLineStyle, CurrPenWidth, CurrPenColor,
        haNone, clNone, clNone,
        CurrBrush.lbStyle = BS_PATTERN, XMLNode)
    else if ARecord is TEMF_SelectClipPath then
      WritePrimitiveAttr(liDashed, CurrPenWidth, clLtGray,
        haNone, clNone, clNone, False, XMLNode);
  end;

  procedure AddLineTo(X, Y: Integer);
  var
    P: PPoint;
  begin
    New(P);
    P.X := X;
    P.Y := Y;
    LineToList.Add(P);
    CurrXY := PointToPoint2D(P^);
  end;

  procedure AddPoint2D(P: TPoint2D);
  begin
    St := St + FloatToStr(P.X) + ',' + FloatToStr(P.Y) + ' ';
  end;

  procedure NodeAddXY(const XS, YS: string; P: TPoint2D);
  begin
    XMLNode.AttributeValue[XS] := P.X;
    XMLNode.AttributeValue[YS] := P.Y;
  end;

  function AInfoRec: TXMLDElement;
  begin
    Result := XML.DocumentElement.AddElement(
      //'rec' + IntToStr(RecIndex) +
      //'fu' +      IntToStr(ARecInfo.Fu));
      AnsiReplaceStr(ARecord.ClassName, 'TEMF_', ''));
  end;

  procedure AddLineTolist;
  var
    I: Integer;
    IsPolygon: Boolean;
  begin
    if LineToList.Count = 0 then Exit;
    if LineToList.Count > 2 then
    begin
      St := '';
      IsPolygon := (PPoint(LineToList[0]).X =
        PPoint(LineToList[LineToList.Count - 1]).X)
        and (PPoint(LineToList[0]).Y =
        PPoint(LineToList[LineToList.Count - 1]).Y);
      if IsPolygon then
        XMLNode := XML.DocumentElement.AddElement('polygon')
      else
        XMLNode := XML.DocumentElement.AddElement('polyline');
      for I := 0 to LineToList.Count - 1 - Byte(IsPolygon) do
        AddPoint2D(GetXY(
          PPoint(LineToList[I]).X,
          PPoint(LineToList[I]).Y));
      XMLNode.Text := St;
    end
    else
    begin
      XMLNode := XML.DocumentElement.AddElement('line');
      NodeAddXY('x1', 'y1', GetXY(
        PPoint(LineToList[0]).X,
        PPoint(LineToList[0]).Y));
      NodeAddXY('x2', 'y2', GetXY(
        PPoint(LineToList[1]).X,
        PPoint(LineToList[1]).Y));
    end;
    WritePrimitiveAttr(CurrLineStyle, CurrPenWidth, CurrPenColor,
      haNone, clNone, clNone, False, XMLNode);
    for I := 0 to LineToList.Count - 1 do
      Dispose(PPoint(LineToList[I]));
    LineToList.Clear;
  end;

  function AddItem(const ID: string): TXMLDElement;
  begin
    AddLineTolist;
    Result := XML.DocumentElement.AddElement(ID);
  end;

  procedure RoundRect(const EMR_Info: TEMRRoundRect);
  var
    PP: TPointsSet2D;
    RX, RY: TRealType;
    I: Integer;
  begin
//    (ARecord as TEMF_RoundRect).EMR_Info.szlCorner.CX
    PP := TPointsSet2D.Create(12);
    try
      with EMR_Info do
      begin
        RX := szlCorner.CX / 2;
        RY := szlCorner.CY / 2;
        PP.Add(GetXY(rclBox.Left, rclBox.Top + RY));
        PP.Add(GetXY(rclBox.Left, rclBox.Top));
        PP.Add(GetXY(rclBox.Left + RX, rclBox.Top));
        PP.Add(GetXY(rclBox.Right - RX, rclBox.Top));
        PP.Add(GetXY(rclBox.Right, rclBox.Top));
        PP.Add(GetXY(rclBox.Right, rclBox.Top + RY));
        PP.Add(GetXY(rclBox.Right, rclBox.Bottom - RY));
        PP.Add(GetXY(rclBox.Right, rclBox.Bottom));
        PP.Add(GetXY(rclBox.Right - RX, rclBox.Bottom));
        PP.Add(GetXY(rclBox.Left + RX, rclBox.Bottom));
        PP.Add(GetXY(rclBox.Left, rclBox.Bottom));
        PP.Add(GetXY(rclBox.Left, rclBox.Bottom - RY));
      end;
      XMLNode := AddItem('bezier');
      //XMLNode := AddItem('polygon');
      XMLNode.AttributeValue['closed'] := '1';
      St := '';
      AddPoint2D(PP[0]);
      AddPoint2D(MixPoint(PP[0], PP[1], 0.5567));
      AddPoint2D(MixPoint(PP[2], PP[1], 0.5567));
      AddPoint2D(PP[2]);
      AddPoint2D(MidPoint(PP[2], PP[3]));
      AddPoint2D(MidPoint(PP[2], PP[3]));
      AddPoint2D(PP[3]);
      AddPoint2D(MixPoint(PP[3], PP[4], 0.5567));
      AddPoint2D(MixPoint(PP[5], PP[4], 0.5567));
      AddPoint2D(PP[5]);
      AddPoint2D(MidPoint(PP[5], PP[6]));
      AddPoint2D(MidPoint(PP[5], PP[6]));
      AddPoint2D(PP[6]);
      AddPoint2D(MixPoint(PP[6], PP[7], 0.5567));
      AddPoint2D(MixPoint(PP[8], PP[7], 0.5567));
      AddPoint2D(PP[8]);
      AddPoint2D(MidPoint(PP[8], PP[9]));
      AddPoint2D(MidPoint(PP[8], PP[9]));
      AddPoint2D(PP[9]);
      AddPoint2D(MixPoint(PP[9], PP[10], 0.5567));
      AddPoint2D(MixPoint(PP[11], PP[10], 0.5567));
      AddPoint2D(PP[11]);
      AddPoint2D(MidPoint(PP[11], PP[0]));
      AddPoint2D(MidPoint(PP[11], PP[0]));
      //for I := 0 to 11 do AddPoint2D(PP[I]);
      XMLNode.Text := St;
    finally
      PP.Free;
    end;
  end;

  procedure ArcToPath(const EMR_Info: TEMRArc);
  var
    CP, P0, P1, P2, P3: TPoint2D;
    RX, RY, A1, A2, SA, EA: TRealType;
    I: Integer;
    procedure AddP(const P: TPoint2D);
    begin
      AddPathPoint0(GetXY(Point2D(CP.X + P.X * RX, CP.Y + P.Y * RY)));
    end;
  begin
    with EMR_Info do
    begin
      CP := Point2D((rclBox.Left + rclBox.Right) / 2,
        (rclBox.Top + rclBox.Bottom) / 2);
      RX := Abs(rclBox.Right - rclBox.Left) / 2;
      RY := Abs(rclBox.Bottom - rclBox.Top) / 2;
      A1 := ArcTan2((ptlStart.Y - CP.Y) * RX / RY, ptlStart.X - CP.X);
      A2 := ArcTan2((ptlEnd.Y - CP.Y) * RX / RY, ptlEnd.X - CP.X);
      if A1 < A2 then A1 := A1 + 2 * Pi;
      for I := 0 to 3 do
      begin
        SA := A1 + (A2 - A1) / 4 * I;
        EA := A1 + (A2 - A1) / 4 * (I + 1);
        SmallArcBezier(Point2D(0, 0), 1, SA, EA, P0, P1, P2, P3);
        AddP(P1);
        AddP(P2);
        AddP(P3);
      end;
      CurrXY := Point2D(CP.X + P3.X * RX, CP.Y + P3.Y * RY);
    end;
  end;
var
  Text_Metric: tagTEXTMETRIC;
  VShift: Integer;
  procedure GetFontMetric(const LogFont: TLogFontW);
  var
    h_DC: HDC;
    h_FONT: HFONT;
  begin
    h_DC := GetWindowDC(0);
    h_FONT := CreateFontIndirectW(LogFont);
    SelectObject(h_DC, h_FONT);
    GetTextMetrics(h_DC, Text_Metric);
    DeleteObject(h_FONT);
    ReleaseDC(0, h_DC);
  end;
begin
  if XML = nil then Exit;
  XML.LoadXML('<TpX/>');
  LineToList := TList.Create;
  LineToList.Capacity := 10;
  Handles := TObjectList.Create;
  Handles.OwnsObjects := True;
  DC_InfoList := TObjectList.Create;
  DC_InfoList.OwnsObjects := True;
  Handles.Capacity := EMF_Struct.Header.NumOfHandles + STOCK_LAST + 1;
  for I := 1 to EMF_Struct.Header.NumOfHandles + STOCK_LAST + 1 do
    Handles.Add(nil);
{ WHITE_BRUSH = 0; LTGRAY_BRUSH = 1; GRAY_BRUSH = 2; DKGRAY_BRUSH = 3;
 BLACK_BRUSH = 4; NULL_BRUSH = 5;  WHITE_PEN = 6;  BLACK_PEN = 7;  NULL_PEN = 8;
  OEM_FIXED_FONT = 10;  ANSI_FIXED_FONT = 11;  ANSI_VAR_FONT = 12;
  SYSTEM_FONT = 13;  DEVICE_DEFAULT_FONT = 14;  DEFAULT_PALETTE = 15;
  SYSTEM_FIXED_FONT = $10;  DEFAULT_GUI_FONT = 17;
  DC_BRUSH = 18;  DC_PEN = 19;  STOCK_LAST = 19;  2147483647}
//  Initializing stock objects:
  TmpObj := TBrushObject.Create;
  (TmpObj as TBrushObject).Brush.lbColor := clWhite;
  SetHandle(EMF_Struct.Header.NumOfHandles + WHITE_BRUSH + 1, TmpObj);
  TmpObj := TBrushObject.Create;
  (TmpObj as TBrushObject).Brush.lbColor := clLtGray;
  SetHandle(EMF_Struct.Header.NumOfHandles + LTGRAY_BRUSH + 1, TmpObj);
  TmpObj := TBrushObject.Create;
  (TmpObj as TBrushObject).Brush.lbColor := clGray;
  SetHandle(EMF_Struct.Header.NumOfHandles + GRAY_BRUSH + 1, TmpObj);
  TmpObj := TBrushObject.Create;
  (TmpObj as TBrushObject).Brush.lbColor := clDkGray;
  SetHandle(EMF_Struct.Header.NumOfHandles + DKGRAY_BRUSH + 1, TmpObj);
  TmpObj := TBrushObject.Create;
  (TmpObj as TBrushObject).Brush.lbColor := clBlack;
  SetHandle(EMF_Struct.Header.NumOfHandles + BLACK_BRUSH + 1, TmpObj);
  TmpObj := TBrushObject.Create;
  (TmpObj as TBrushObject).Brush.lbColor := clNone;
  SetHandle(EMF_Struct.Header.NumOfHandles + NULL_BRUSH + 1, TmpObj);
  TmpObj := TBrushObject.Create;
  (TmpObj as TBrushObject).Brush.lbColor := clWhite;
  SetHandle(EMF_Struct.Header.NumOfHandles + DC_BRUSH + 1, TmpObj);
  TmpObj := TExtPenObject.Create;
  (TmpObj as TExtPenObject).ExtPen.elpColor := clWhite;
  SetHandle(EMF_Struct.Header.NumOfHandles + WHITE_PEN + 1, TmpObj);
  TmpObj := TExtPenObject.Create;
  (TmpObj as TExtPenObject).ExtPen.elpColor := clBlack;
  SetHandle(EMF_Struct.Header.NumOfHandles + BLACK_PEN + 1, TmpObj);
  TmpObj := TExtPenObject.Create;
  (TmpObj as TExtPenObject).ExtPen.elpColor := clNone;
  (TmpObj as TExtPenObject).ExtPen.elpPenStyle := PS_NULL;
  SetHandle(EMF_Struct.Header.NumOfHandles + NULL_PEN + 1, TmpObj);
  TmpObj := TExtPenObject.Create;
  (TmpObj as TExtPenObject).ExtPen.elpColor := clBlack;
  SetHandle(EMF_Struct.Header.NumOfHandles + DC_PEN + 1, TmpObj);
{Background color	Background color setting from Windows Control Panel (typically, white).
Background mode	OPAQUE.
Bitmap	None.
Brush	WHITE_BRUSH.
Brush origin	(0,0).
Clipping region	Entire window or client area with the update region clipped, as appropriate. Child and pop-up windows in the client area may also be clipped.
Palette	DEFAULT_PALETTE.
Current pen position	(0,0).
Device origin	Upper left corner of the window or the client area.
Drawing mode	R2_COPYPEN.
Font	SYSTEM_FONT (SYSTEM_FIXED_FONT for applications written to run with Windows versions 3.0 and earlier).
Intercharacter spacing	0.
Mapping mode	MM_TEXT.
Pen	BLACK_PEN.
Polygon-fill mode	ALTERNATE.
Stretch mode	BLACKONWHITE.
Text color	Text color setting from Control Panel (typically, black).
Viewport extent	(1,1).
Viewport origin	(0,0).
Window extent	(1,1).
Window origin	(0,0).}
  CurrPenHandle := 2147483648 + BLACK_PEN;
  CurrBrushHandle := 2147483648 + WHITE_BRUSH;
  CurrBrush.lbColor := clWhite;
  CurrBkColor := clWhite;
  CurrBkMode := OPAQUE;
  CurrMapMode := MM_TEXT;
  CurrPen.lopnColor := clBlack;
  CurrTextColor := clBlack;
  with XML.DocumentElement do
  begin
    AttributeValue['v'] := 2;
    with EMF_Struct.Header do
    begin
      AttributeValue['l'] := BoundsLeft;
      AttributeValue['t'] := BoundsTop;
      AttributeValue['r'] := BoundsRight;
      AttributeValue['b'] := BoundsBottom;
      AttributeValue['WidthDevPixels'] := WidthDevPixels;
      AttributeValue['HeightDevPixels'] := HeightDevPixels;
      AttributeValue['WidthDevMM'] := WidthDevMM;
      AttributeValue['HeightDevMM'] := HeightDevMM;
      AttributeValue['l'] := BoundsLeft;
      AttributeValue['t'] := BoundsTop;
      AttributeValue['PicUnitLength'] :=
        PicUnitLength_Default;
      MMScale := WidthDevMM / WidthDevPixels;
      AttributeValue['PicScale'] := 1;
      AttributeValue['LineWidth'] := MMScale;
      //ShowMessage(FloatToStr(Scale));
    end;
  end;
  CurrViewportOrigin := Point(0, 0);
  CurrViewportExtent.CX := 1;
  CurrViewportExtent.CY := 1;
  CurrWindowOrigin := CurrViewportOrigin;
  CurrWindowExtent := CurrViewportExtent;
  SetXYScale;
  BB := EMF_Struct.Header.BoundsBottom;
  CurrWorldTr := IdentityTransf2D;
  CurrXY := Point2D(0, 0);
  HasPath := False;
  for RecIndex := 0 to EMF_Struct.Count - 1 do
  begin
    if RecIndex mod 100 = 0 then
      ShowProgress(RecIndex / EMF_Struct.Count);
    ARecord := EMF_Struct[RecIndex] as TEMF_Record;
    if (ARecord is TEMF_SetExtEx) then
    begin
      XMLNode := AInfoRec;
      with (ARecord as TEMF_SetExtEx).EMR_Info do
      begin
        XMLNode.AttributeValue['cx'] := szlExtent.CX;
        XMLNode.AttributeValue['cy'] := szlExtent.CY;
        if ARecord is TEMF_SetViewportExtEx then
        begin
          if IsOld then
          begin
            CurrViewportExtent.CX := ExtOldX;
            CurrViewportExtent.CY := ExtOldY;
          end
          else CurrViewportExtent := szlExtent
        end
        else CurrWindowExtent := szlExtent;
        SetXYScale;
      end;
    end
    else if (ARecord is TEMF_SetOrgEx) then
    begin
      XMLNode := AInfoRec;
      with (ARecord as TEMF_SetOrgEx).EMR_Info do
      begin
        XMLNode.AttributeValue['x'] := ptlOrigin.X;
        XMLNode.AttributeValue['y'] := ptlOrigin.Y;
        if ARecord is TEMF_SetViewportOrgEx
          then CurrViewportOrigin := ptlOrigin
        else if ARecord is TEMF_SetWindowOrgEx
          then CurrWindowOrigin := ptlOrigin;
      end;
      SetXYScale;
    end
    else if (ARecord is TEMF_ScaleExtEx) then
    begin
      XMLNode := AInfoRec;
      with (ARecord as TEMF_ScaleExtEx).EMR_Info do
      begin
        XMLNode.AttributeValue['xNum'] := xNum;
        XMLNode.AttributeValue['xDenom'] := xDenom;
        XMLNode.AttributeValue['yNum'] := yNum;
        XMLNode.AttributeValue['yDenom'] := yDenom;
        if ARecord is TEMF_ScaleViewportExtEx then
        begin
          CurrViewportExtent.CX :=
            (CurrViewportExtent.CX * xNum) div xDenom;
          CurrViewportExtent.CY :=
            (CurrViewportExtent.CY * yNum) div yDenom;
        end
        else
        begin
          CurrWindowExtent.CX :=
            (CurrWindowExtent.CX * xNum) div xDenom;
          CurrWindowExtent.CY :=
            (CurrWindowExtent.CY * yNum) div yDenom;
        end;
      end;
      SetXYScale;
    end
    else if (ARecord is TEMF_BeginPath) then
    begin
      HasPath := True;
      HasStroke := (ARecord as TEMF_BeginPath).HasStroke;
      HasFill := (ARecord as TEMF_BeginPath).HasFill;
      HasClip := (ARecord as TEMF_BeginPath).HasClip;
      SetLength(PathArr, (ARecord as TEMF_BeginPath).N + 1);
      PathArrPos := 0;
      UseBezier := (ARecord as TEMF_BeginPath).UseBezier;
    end
    else if (ARecord is TEMF_EndPath) then
      HasPath := False
    else if (ARecord is TEMF_RoundRect) then
    begin
      with (ARecord as TEMF_RoundRect).EMR_Info do
      begin
        {XMLNode := AddItem('rect');
        NodeAddXY('x1', 'y1',
          GetXY(rclBox.Left, rclBox.Top));
        NodeAddXY('x2', 'y2',
          GetXY(rclBox.Right, rclBox.Bottom));
              }
        RoundRect((ARecord as TEMF_RoundRect).EMR_Info);
      end;
      WritePrimitiveAttr(CurrLineStyle, CurrPenWidth, CurrPenColor,
        CurrHatching, CurrHatchColor, CurrBrushColor,
        CurrBrush.lbStyle = BS_PATTERN, XMLNode);
    end
    else if (ARecord is TEMF_Rect0) then
    begin
      if ARecord is TEMF_Ellipse then
        with (ARecord as TEMF_Ellipse).EMR_Info do
        begin
          XMLNode := AddItem('ellipse');
          NodeAddXY('x', 'y',
            GetXY((rclBox.Left + rclBox.Right) / 2,
            (rclBox.Top + rclBox.Bottom) / 2));
          XMLNode.AttributeValue['dx']
            := Abs(rclBox.Right - rclBox.Left) * CurrXScale;
          XMLNode.AttributeValue['dy']
            := Abs(rclBox.Bottom - rclBox.Top) * CurrXScale;
        end
      else
        {if (CurrLineKind <> liNone)
          or (CurrHatching <> haNone) then}
        with (ARecord as TEMF_Rectangle).EMR_Info do
        begin
          XMLNode := AddItem('rect');
          NodeAddXY('x1', 'y1',
            GetXY(rclBox.Left, rclBox.Top));
          NodeAddXY('x2', 'y2',
            GetXY(rclBox.Right, rclBox.Bottom));
        end;
      WritePrimitiveAttr(CurrLineStyle, CurrPenWidth, CurrPenColor,
        CurrHatching, CurrHatchColor, CurrBrushColor,
        CurrBrush.lbStyle = BS_PATTERN, XMLNode);
    end
    else if HasPath and (ARecord is TEMF_ArcTo) then
    begin
      ArcToPath((ARecord as TEMF_ArcTo).EMR_Info);
    end
    else if (ARecord is TEMF_Arc0) then
    begin
      if (ARecord is TEMF_Arc) or (ARecord is TEMF_ArcTo)
        then XMLNode := AddItem('arc')
      else if (ARecord is TEMF_Pie)
        then XMLNode := AddItem('sector')
      else XMLNode := AddItem('segment');
      with (ARecord as TEMF_Arc0).EMR_Info do
      begin
        //rclBox: TRect; Inclusive-inclusive bounding rectangle
        CPoint.X := (rclBox.Left + rclBox.Right) div 2;
        CPoint.Y := (rclBox.Top + rclBox.Bottom) div 2;
        NodeAddXY('x', 'y', GetXY(CPoint.X, CPoint.Y));
        XMLNode.AttributeValue['d'] :=
          ((rclBox.Right - rclBox.Left) * CurrXScale
          + (rclBox.Bottom - rclBox.Top) * CurrYScale) / 2;
        XMLNode.AttributeValue['a1'] :=
          ArcTan2(CPoint.Y - ptlStart.Y, ptlStart.X - CPoint.X);
        XMLNode.AttributeValue['a2'] :=
          ArcTan2(CPoint.Y - ptlEnd.Y, ptlEnd.X - CPoint.X);
        if (ARecord is TEMF_ArcTo) then
        begin
          CurrXY := Point2D(ptlEnd.X, ptlEnd.Y);
        end;
      end;
      WritePrimitiveAttr(CurrLineStyle, CurrPenWidth, CurrPenColor,
        CurrHatching, CurrHatchColor, CurrBrushColor,
        CurrBrush.lbStyle = BS_PATTERN, XMLNode);
    end
    else if ARecord is TEMF_StretchDIBits then
    begin
      XMLNode := AddItem('rect');
      with (ARecord as TEMF_StretchDIBits).EMR_Info do
      begin
        NodeAddXY('x1', 'y1',
          GetXY(rclBounds.Left, rclBounds.Top));
        NodeAddXY('x2', 'y2',
          GetXY(rclBounds.Right, rclBounds.Bottom));
      end;
      WritePrimitiveAttr(liNone, 1, clNone,
        haNone, clNone, CurrBrushColor, True, XMLNode);
    end
    else if ARecord is TEMF_BitBlt then
    begin
      XMLNode := AddItem('rect');
      with (ARecord as TEMF_BitBlt).EMR_Info do
      begin
        XMLNode.AttributeValue['dwRop'] := IntToHex(dwRop, 8);
            //IntToStr(dwRop);
        NodeAddXY('x1', 'y1', GetXY(xDest, yDest));
        NodeAddXY('x2', 'y2',
          GetXY(xDest + cxDest, yDest + cyDest));
        WritePrimitiveAttr(liNone, 1, clNone,
          haNone, clNone, CurrBrushColor, True, XMLNode);
      end;
    end
    else if HasPath and ((ARecord is TEMF_PolyBezierTo16)
      or (ARecord is TEMF_PolyBezierTo32)) then
    begin
      with ARecord as TEMF_PolyGen do
        for I := 0 to High(PointsArray) do
          AddPathPoint0(GetXY(PointsArray[I].X,
            PointsArray[I].Y));
    end
    else if (ARecord is TEMF_PolyBezier160)
      or (ARecord is TEMF_PolyBezier320) then
    begin
      XMLNode := AddItem('bezier');
  //Bez0,Bez1,Bez2,Bez3:TPoint2D;
  //BezMod:Byte;
      with ARecord as TEMF_PolyGen do
      begin
        BezMod := 0;
        St := '';
        for I := 0 to High(PointsArray) do
        begin
          BezArr[BezMod] := GetXY(PointsArray[I].X,
            PointsArray[I].Y);
          if BezMod = 3 then
          begin
            if I < 5 then AddPoint2D(BezArr[0]);
            if not (IsSamePoint2D(BezArr[0], BezArr[3])
              and IsSamePoint2D(BezArr[0], BezArr[1])) then
            begin
              //for J := 0 to 8 do
              //  AddPoint2D(BezierPoint(BezArr[0], BezArr[1],
              //    BezArr[2], BezArr[3], {0.03 + J / 8 * 0.97} BezFract[J]));
                //for J := 0 to 8 do
                //  AddPoint2D(BezierPoint(BezArr[0], BezArr[1],
                //    BezArr[2], BezArr[3], {0.03 + J / 8 * 0.97} BezFract[J]));
              AddPoint2D(BezArr[1]);
              AddPoint2D(BezArr[2]);
              AddPoint2D(BezArr[3]);
              BezArr[0] := BezArr[3];
            end;
          end;
          //St := St + FloatToStr() + ',' + FloatToStr();
          if I < High(PointsArray) then St := St + ' '
          else
            if (ARecord is TEMF_PolyBezierTo16)
              or (ARecord is TEMF_PolyBezierTo32) then
            begin
              CurrXY := Point2D(PointsArray[I].X,
                PointsArray[I].Y);
            end;
          BezMod := Succ(BezMod mod 3);
        end;
      end;
      WritePrimitiveAttr(CurrLineStyle, CurrPenWidth, CurrPenColor,
        CurrHatching, CurrHatchColor, CurrBrushColor,
        CurrBrush.lbStyle = BS_PATTERN, XMLNode);
{        WritePrimitiveAttr(CurrLineStyle, CurrPenWidth, CurrPenColor,
          haNone, clNone, clNone,
          CurrBrush.lbStyle = BS_PATTERN, XMLNode);}
      XMLNode.Text := XMLNode.Text + St;
    end
    else if HasPath and (ARecord is TEMF_PolylineTo16) then
    begin
      with ARecord as TEMF_PolylineTo16 do
        for I := 0 to High(PointsArray) do
          AddPathPoint(GetXY(PointsArray[I].X,
            PointsArray[I].Y));
    end
    else if (ARecord is TEMF_PolyGen) then
    begin
      {if (CurrLineKind <> liNone)
        or (CurrHatching <> haNone) then}
      begin
        if (ARecord is TEMF_Polygon16)
          or (ARecord is TEMF_Polygon32) then
          XMLNode := AddItem('polygon')
        else if (ARecord is TEMF_Polyline16)
          or (ARecord is TEMF_Polyline32) then
          XMLNode := AddItem('polyline')
        else if (ARecord is TEMF_PolylineTo16)
          or (ARecord is TEMF_PolylineTo32) then
          XMLNode := AddItem('polyline');
        with ARecord as TEMF_PolyGen do
        begin
          St := '';
          for I := 0 to High(PointsArray) do
          begin
            AddPoint2D(
              GetXY(PointsArray[I].X, PointsArray[I].Y));
            if I < High(PointsArray) then St := St + ' '
            else
              if ARecord is TEMF_PolylineTo16 then
                CurrXY := Point2D(PointsArray[I].X, PointsArray[I].Y);
          end;
        end;
        if (ARecord is TEMF_Polygon16) or (ARecord is TEMF_Polygon32) then
          WritePrimitiveAttr(CurrLineStyle, CurrPenWidth, CurrPenColor,
            CurrHatching, CurrHatchColor, CurrBrushColor,
            CurrBrush.lbStyle = BS_PATTERN, XMLNode)
        else WritePrimitiveAttr(CurrLineStyle, CurrPenWidth, CurrPenColor,
            haNone, clNone, clNone,
            CurrBrush.lbStyle = BS_PATTERN, XMLNode);
        XMLNode.Text := St;
      end;
    end
    else if (ARecord is TEMF_PolyPoly16) then
    begin
      {if (CurrLineKind <> liNone)
        or (CurrHatching <> haNone) then}
      with (ARecord as TEMF_PolyPoly16) do
        for J := 0 to PolyList.Count - 1 do
        begin
          if ARecord is TEMF_PolyPolygon16 then
            XMLNode := AddItem('polygon')
          else if ARecord is TEMF_PolyPolyline16 then
            XMLNode := AddItem('polyline');
          St := '';
          for I := 0 to GetLen(J) - 1 do
            AddPoint2D(
              GetXY(GetPnt(J, I).X, GetPnt(J, I).Y));
          if ARecord is TEMF_PolyPolygon16 then
            WritePrimitiveAttr(CurrLineStyle, CurrPenWidth, CurrPenColor,
              CurrHatching, CurrHatchColor, CurrBrushColor,
              CurrBrush.lbStyle = BS_PATTERN, XMLNode)
          else WritePrimitiveAttr(CurrLineStyle, CurrPenWidth, CurrPenColor,
              haNone, clNone, clNone,
              CurrBrush.lbStyle = BS_PATTERN, XMLNode);
          XMLNode.Text := St;
        end;

      {if (CurrLineKind <> liNone)
        or (CurrHatching <> haNone) then
      begin
      end;}
    end
    else if HasPath and (ARecord is TEMF_PolyDraw16) then
    begin
      with ARecord as TEMF_PolyDraw16 do
      begin
        AddPathPoint0(Point2D(-1234, -1234));
        //AddPathPoint0(GetXY(MoveToXY));
        for I := 0 to EMR_Info.cpts - 1 do
        begin
          case abTypes[I] and 6 of
            PT_LINETO: AddPathPoint(GetXY(
                PointsArray[I].X, PointsArray[I].Y));
            PT_BEZIERTO:
              AddPathPoint0(GetXY(
                PointsArray[I].X, PointsArray[I].Y));
            PT_MOVETO:
              begin
                AddPathPoint0(Point2D(-1234, -1234));
                AddPathPoint0(GetXY(
                  PointsArray[I].X, PointsArray[I].Y));
                MoveToXY := Point2D(
                  PointsArray[I].X, PointsArray[I].Y);
              end;
          else
            {AddPathPoint(GetXY(
              PointsArray[I].X, PointsArray[I].Y));}
          end;
          //if (ARecord as TEMF_PolyDraw16).abTypes[I] and 1 > 0 then ;
        end;
        CurrXY := Point2D(
          PointsArray[I].X, PointsArray[I].Y)
      end;
    end
    else if HasPath and (ARecord is TEMF_MoveToEx) then
    begin
      with (ARecord as TEMF_MoveToEx).EMR_Info do
      begin
        AddPathPoint0(Point2D(-1234, -1234));
        AddPathPoint0(GetXY(ptl.X, ptl.Y));
        MoveToXY := Point2D(ptl.X, ptl.Y);
      end;
    end
    else if ARecord is TEMF_MoveToEx then
    begin
      with (ARecord as TEMF_MoveToEx).EMR_Info do
        if (CurrXY.X <> ptl.X) or (CurrXY.Y <> ptl.Y)
          // or (MoveToXY.X <> ptl.X) or (MoveToXY.Y <> ptl.Y)
        then
        begin
          XMLNode := AddItem('moveto');
          NodeAddXY('x', 'y', GetXY(ptl.X, ptl.Y));
          CurrXY := Point2D(ptl.X, ptl.Y);
          MoveToXY := CurrXY;
        end;
    end
    else if HasPath and (ARecord is TEMF_CloseFigure) then
    begin
      AddPathPoint(GetXY(MoveToXY));
    end
    else if ARecord is TEMF_CloseFigure then
    begin
      XMLNode := AddItem('line');
      NodeAddXY('x1', 'y1', GetXY(CurrXY));
      NodeAddXY('x2', 'y2', GetXY(MoveToXY));
      WritePrimitiveAttr(CurrLineStyle, CurrPenWidth, CurrPenColor,
        CurrHatching, CurrHatchColor, CurrBrushColor,
        CurrBrush.lbStyle = BS_PATTERN, XMLNode);
      XMLNode :=
        AddItem('CLOSEFIGURE');
    end
    else if HasPath and (ARecord is TEMF_LineTo) then
    begin
      with (ARecord as TEMF_LineTo).EMR_Info do
        AddPathPoint(GetXY(ptl.X, ptl.Y));
    end
    else if ARecord is TEMF_LineTo then
    begin
      //XMLNode := AddItem('line');
      with (ARecord as TEMF_LineTo).EMR_Info do
      begin
        {NodeAddXY('x1', 'y1', GetXY(CurrXY));
        NodeAddXY('x2', 'y2', GetXY(ptl.X, ptl.Y));}
        if LineToList.Count = 0 then
          AddLineTo(Round(CurrXY.X), Round(CurrXY.Y));
        AddLineTo(ptl.X, ptl.Y);
      end;
    end
    else if (ARecord is TEMF_ObjectHandle) then
    begin
      XMLNode := AInfoRec;
      AddLineTolist;
      with (ARecord as TEMF_ObjectHandle).EMR_Info do
      begin
        //ihObject: DWORD;    Object handle index
        XMLNode.AttributeValue['Handle'] := IntToStr(ihObject);
        if (ARecord is TEMF_SelectObject) then
        begin
          TmpObj := GetHandle(ihObject);
          if TmpObj is TFontObject then
          begin
            CurrFontHandle := ihObject;
            CurrFont := (TmpObj as TFontObject).Font
          end
          else if TmpObj is TPenObject then
          begin
            CurrPenHandle := ihObject;
            CurrPen := (TmpObj as TPenObject).Pen;
            IsCurrPenExt := False;
          end
          else if TmpObj is TExtPenObject then
          begin
            CurrPenHandle := ihObject;
            CurrExtPen := (TmpObj as TExtPenObject).ExtPen;
            IsCurrPenExt := True;
          end
          else if TmpObj is TBrushObject then
          begin
            CurrBrushHandle := ihObject;
            CurrBrush := (TmpObj as TBrushObject).Brush;
          end;
        end
        else if (ARecord is TEMF_DeleteObject) then
        begin
        //Handles.Delete(ihObject);
          if CurrFontHandle = ihObject then CurrFontHandle := 123456789
          else if CurrPenHandle = ihObject then
            CurrPenHandle := 123456789
          else if CurrBrushHandle = ihObject then
            CurrBrushHandle := 123456789;
          SetHandle(ihObject, nil);
        end;
      end
    end
    else if (ARecord is TEMF_SaveDC) then
    begin
      XMLNode := AInfoRec;
      SaveDC_Info;
    end
    else if (ARecord is TEMF_RestoreDC) then
    begin
      XMLNode := AInfoRec;
      XMLNode.AttributeValue['iRelative']
        := (ARecord as TEMF_RestoreDC).EMR_Info.iRelative;
      RestoreDC_Info;
    end
    else if (ARecord is TEMF_ExtCreateFontIndirectW) then
    begin
      XMLNode := AddItem('font');
      with (ARecord as TEMF_ExtCreateFontIndirectW).EMR_Info do
      begin
        XMLNode.AttributeValue['Handle'] := ihFont;
        TmpObj := TFontObject.Create;
        SetHandle(ihFont, TmpObj);
        (TmpObj as TFontObject).Font := elfw.elfLogFont;
        with XMLNode, elfw.elfLogFont do
        begin
          AttributeValue['Height'] := lfHeight;
          AttributeValue['Width'] := lfWidth;
          AttributeValue['Escapement'] := lfEscapement;
          AttributeValue['Orientation'] := lfOrientation;
          AttributeValue['Weight'] := lfWeight;
          AttributeValue['Italic'] := lfItalic;
          AttributeValue['Underline'] := lfUnderline;
          AttributeValue['StrikeOut'] := lfStrikeOut;
          AttributeValue['CharSet'] := lfCharSet;
          AttributeValue['OutPrecision'] := lfOutPrecision;
          AttributeValue['ClipPrecision'] := lfClipPrecision;
          AttributeValue['Quality'] := lfQuality;
          AttributeValue['PitchAndFamily'] := lfPitchAndFamily;
        end;
      end
    end
    else if (ARecord is TEMF_SetMiterLimit) then
    begin
      with (ARecord as TEMF_SetMiterLimit).EMR_Info do
      begin
        XMLNode := AddItem('MiterLimit');
        XMLNode.AttributeValue['MiterLimit'] := eMiterLimit;
        if eMiterLimit < 1 then eMiterLimit := 1;
        XML.DocumentElement.AttributeValue['MiterLimit'] := eMiterLimit;
      end;
    end
    else if (ARecord is TEMF_CreatePen) then
    begin
      XMLNode := AddItem('Pen');
      with (ARecord as TEMF_CreatePen).EMR_Info do
      begin
        XMLNode.AttributeValue['Handle'] := ihPen;
        TmpObj := TPenObject.Create;
        SetHandle(ihPen, TmpObj);
        (TmpObj as TPenObject).Pen := lopn;
        with XMLNode, lopn do
        begin
          AttributeValue['Style'] := lopnStyle;
          AttributeValue['WidthX'] := lopnWidth.X;
          AttributeValue['WidthY'] := lopnWidth.Y;
          AttributeValue['Color'] := ColorToString(lopnColor);
        end;
      end
    end
    else if (ARecord is TEMF_ExtCreatePen) then
    begin
      XMLNode := AddItem('ExtPen');
      with (ARecord as TEMF_ExtCreatePen).EMR_Info do
      begin
        XMLNode.AttributeValue['Handle'] := ihPen;
        TmpObj := TExtPenObject.Create;
        SetHandle(ihPen, TmpObj);
        (TmpObj as TExtPenObject).ExtPen := elp;
        with XMLNode, elp do
        begin
          AttributeValue['offBmi'] := offBmi;
          AttributeValue['cbBmi'] := cbBmi;
          AttributeValue['offBits'] := offBits;
          AttributeValue['cbBits'] := cbBits;
          AttributeValue['Style'] := elpPenStyle;
          AttributeValue['Width'] := elpWidth;
          AttributeValue['BrushStyle'] := elpBrushStyle;
          AttributeValue['Color'] := ColorToString(elpColor);
          AttributeValue['Hatch'] := elpHatch;
        //elpStyleEntry: array[0..0] of DWORD;??
        end;
      end
    end
    else if (ARecord is TEMF_CreateBrushIndirect) then
    begin
      XMLNode := AddItem('Brush');
      with (ARecord as TEMF_CreateBrushIndirect).EMR_Info do
      begin
        TmpObj := TBrushObject.Create;
        SetHandle(ihBrush, TmpObj);
        (TmpObj as TBrushObject).Brush := LB;
        with XMLNode, LB do
        begin
          AttributeValue['Handle'] := ihBrush;
          AttributeValue['Style'] := lbStyle;
          AttributeValue['Color'] := ColorToString(lbColor);
          AttributeValue['Hatch'] := lbHatch;
        end;
      end
    end
    else if (ARecord is TEMF_CreateDIBPatternBrushPt) then
    begin
      XMLNode := AddItem('DIBPatternBrushPt');
      with (ARecord as TEMF_CreateDIBPatternBrushPt).EMR_Info do
      begin
        TmpObj := TBrushObject.Create;
        SetHandle(ihBrush, TmpObj);
        (TmpObj as TBrushObject).Brush.lbStyle := BS_PATTERN;
        (TmpObj as TBrushObject).Brush.lbColor := clNone;
        //MainForm.Image1.Picture.Assign((ARecord as TEMF_CreateDIBPatternBrushPt).BMP);
        //(TmpObj as TBrushObject).Brush.lbColor :=          (ARecord as TEMF_CreateDIBPatternBrushPt).BMP.Canvas.Pixels[1, 1];
        XMLNode.AttributeValue['Handle'] := ihBrush;
      end
    end
    else if (ARecord is TEMF_CreateMonoBrush) then
    begin
      XMLNode := AddItem('MonoBrush');
      with (ARecord as TEMF_CreateMonoBrush).EMR_Info do
      begin
        TmpObj := TBrushObject.Create;
        SetHandle(ihBrush, TmpObj);
        //MainForm.Image1.Picture.Assign((ARecord as TEMF_CreateMonoBrush).BMP);
        (TmpObj as TBrushObject).Brush.lbStyle := BS_PATTERN;
        (TmpObj as TBrushObject).Brush.lbColor := clNone; //clSkyBlue;
        XMLNode.AttributeValue['Handle'] := ihBrush;
      end
    end
    else if (ARecord is TEMF_SetBkMode) then
    begin
      XMLNode := AInfoRec;
      with (ARecord as TEMF_SetBkMode).EMR_Info do
      begin
        case iMode of
          0: XMLNode.AttributeValue['Mode'] := 'OPAQUE';
        //OPAQUE Background is filled with the current background color before the text, hatched brush, or pen is drawn.
          1: XMLNode.AttributeValue['Mode'] := 'TRANSPARENT';
        //TRANSPARENT Background remains untouched.
        else
          XMLNode.AttributeValue['Mode'] := iMode;
        end;
        CurrBkMode := iMode;
      end;
    end
    else if (ARecord is TEMF_SetMapMode) then
    begin
      XMLNode := AInfoRec;
      with (ARecord as TEMF_SetMapMode).EMR_Info do
      begin
        case iMode of
          MM_TEXT: XMLNode.AttributeValue['Mode'] := 'MM_TEXT';
          MM_LOMETRIC: XMLNode.AttributeValue['Mode'] := 'MM_LOMETRIC';
          MM_HIMETRIC: XMLNode.AttributeValue['Mode'] := 'MM_HIMETRIC';
          MM_LOENGLISH: XMLNode.AttributeValue['Mode'] := 'MM_LOENGLISH';
          MM_HIENGLISH: XMLNode.AttributeValue['Mode'] := 'MM_HIENGLISH';
          MM_TWIPS: XMLNode.AttributeValue['Mode'] := 'MM_TWIPS';
          MM_ISOTROPIC: XMLNode.AttributeValue['Mode'] := 'MM_ISOTROPIC';
          MM_ANISOTROPIC: XMLNode.AttributeValue['Mode'] :=
            'MM_ANISOTROPIC';
        else
          XMLNode.AttributeValue['Mode'] := iMode;
        end;
        CurrMapMode := iMode;
        SetXYScale;
      end;
    end
    else if (ARecord is TEMF_SetBkColor) then
    begin
      XMLNode := AInfoRec;
      with (ARecord as TEMF_SetBkColor).EMR_Info do
      begin
        XMLNode.AttributeValue['Color'] :=
          ColorToString(crColor);
        CurrBkColor := crColor;
      end;
    end
    else if (ARecord is TEMF_SetTextColor) then
    begin
      XMLNode := AddItem('TextColor');
      CurrTextColor := (ARecord as
        TEMF_SetTextColor).EMR_Info.crColor;
      XMLNode.AttributeValue['Color'] := ColorToString(CurrTextColor);
    end
    else if (ARecord is TEMF_SetTextAlign) then
    begin
      XMLNode := AddItem('TextAlign');
      CurrTextAlignment := (ARecord as
        TEMF_SetTextAlign).EMR_Info.iMode;
      XMLNode.AttributeValue['Align'] := CurrTextAlignment;
        //??DT_LEFT = 0; DT_CENTER = 1;   DT_RIGHT = 2;
        //??DT_TOP = 0;  DT_VCENTER = 4;  DT_BOTTOM = 8;
  //TA_LEFT = 0;  TA_RIGHT = 2;  TA_CENTER = 6;
  //TA_TOP = 0;  TA_BOTTOM = 8;  TA_BASELINE = 24;
    end
    else if (ARecord is TEMF_ExtTextOut) then
    begin
      with (ARecord as TEMF_ExtTextOut).EMR_Info do
      begin
        if CurrBkMode = OPAQUE then
        begin
          XMLNode := AddItem('rect');
          //TempTr := CurrWorldTr;
          if iGraphicsMode = GM_COMPATIBLE then ;
            //CurrWorldTr := IdentityTransf2D;
            //CurrWorldTr := Scale2D(100 / exScale * MMScale,
            //  100 / eyScale * MMScale);
          NodeAddXY('x1', 'y1',
            GetXY(rclBounds.Left, rclBounds.Top));
          NodeAddXY('x2', 'y2',
            GetXY(rclBounds.Right, rclBounds.Bottom));
          WritePrimitiveAttr(liNone, 1, clNone,
            haNone, clNone, CurrBkColor,
            CurrBrush.lbStyle = BS_PATTERN, XMLNode);
          //CurrWorldTr := TempTr;
        end;
        if (emrtext.FOptions and 2) = ETO_OPAQUE then
        begin
          XMLNode := AddItem('rect');
          //TempTr := CurrWorldTr;
          //if iGraphicsMode = GM_COMPATIBLE then ;
            //CurrWorldTr := IdentityTransf2D;
            //CurrWorldTr := Scale2D(100 / exScale * MMScale,
            //  100 / eyScale * MMScale);
          NodeAddXY('x1', 'y1',
            GetXY(emrtext.rcl.Left, emrtext.rcl.Top));
          NodeAddXY('x2', 'y2',
            GetXY(emrtext.rcl.Right, emrtext.rcl.Bottom));
          WritePrimitiveAttr(liNone, 1, clNone,
            haNone, clNone, CurrBkColor,
            CurrBrush.lbStyle = BS_PATTERN, XMLNode);
          //CurrWorldTr := TempTr;
        end;
      //NodeAddXY('x', 'y', Point2D(CurrX + rclBox.Left, CurrY - rclBox.Bottom);
        GetFontMetric(CurrFont);
        XMLNode := AddItem('text');
        XMLNode.AttributeValue['emrtext.FOptions'] := emrtext.FOptions;
        XMLNode.AttributeValue['emrtext.rcl.Left'] := emrtext.rcl.Left;
        XMLNode.AttributeValue['emrtext.rcl.Top'] := emrtext.rcl.Top;
        XMLNode.AttributeValue['emrtext.rcl.Right'] := emrtext.rcl.Right;
        XMLNode.AttributeValue['emrtext.rcl.Bottom'] := emrtext.rcl.Bottom;
        //ETO_OPAQUE
        XMLNode.AttributeValue['RectLeft'] := rclBounds.Left;
        XMLNode.AttributeValue['RectTop'] := rclBounds.Top;
        XMLNode.AttributeValue['RectRight'] := rclBounds.Right;
        XMLNode.AttributeValue['RectBottom'] :=
          rclBounds.Bottom;
        XMLNode.AttributeValue['iGraphicsMode'] :=
          iGraphicsMode;
        XMLNode.AttributeValue['exScale'] := exScale;
        XMLNode.AttributeValue['w']
          := (rclBounds.Right - rclBounds.Left) { * ASingle};
        XMLNode.AttributeValue['eyScale'] := eyScale;
        XMLNode.AttributeValue['h0'] :=
          rclBounds.Bottom - rclBounds.Top;
        XMLNode.AttributeValue['height'] :=
          CurrFont.lfHeight;
        A := CurrFont.lfOrientation / 1800 * Pi;
        if iGraphicsMode = GM_COMPATIBLE then
        begin
          if (CurrFont.lfOrientation = 0) and
            (CurrFont.lfEscapement <> 0) then
            A := CurrFont.lfEscapement / 1800 * Pi;
        end
        else
        begin
          A := CurrFont.lfEscapement / 1800 * Pi;
          V.X := Cos(A); V.Y := Sin(A);
          V := TransformVector2D(V, CurrWorldTr);
          A := ArcTan2(-V.Y, V.X);
        end;
        if A <> 0 then XMLNode.AttributeValue['rot'] := A;
        if CurrFont.lfHeight < 0 then
          CurrFontHeight := Abs(CurrFont.lfHeight)
        else CurrFontHeight :=
          CurrFont.lfHeight - Text_Metric.tmInternalLeading;
            //CurrFont.lfHeight / 1.2;
        if CurrFontHeight = 0 then CurrFontHeight := 12;
        //device units!
        XMLNode.AttributeValue['h'] := //Round()
          Abs(CurrFontHeight * CurrWorldTr[2, 2] * CurrYScale);
          //100 / exScale * MMScale *
//        := (rclBox.Bottom - rclBox.Top){ * ASingle};
        CPoint := emrtext.ptlReference;
        {if (CPoint.X = 0) and (CPoint.Y = 0) then
        begin
          CPoint.X := rclBounds.Left;
          CPoint.Y := rclBounds.Top;
        end;}
        XMLNode.AttributeValue['reference_point'] :=
          IntToStr(CPoint.X) + ',' + IntToStr(CPoint.Y);
        if (CurrTextAlignment and 24) = TA_TOP then
          VShift := Text_Metric.tmInternalLeading
        else VShift := 0;
        if (CurrTextAlignment and 1) = TA_UPDATECP then
        //Use current position
          GetXY(CurrXY.X, CurrXY.Y + VShift)
        else
          GetXY(CPoint.X, CPoint.Y + VShift);
        if (CurrTextAlignment and 24) = TA_BASELINE then
          {NodeAddXY('x', 'y', GetXY(CPoint.X, CPoint.Y
            + CurrFontHeight * 0.35))}
        begin
          //GetXY(CPoint.X, CPoint.Y);
          NodeAddXY('x', 'y',
            Point2D(XY.X, XY.Y {+ Abs(CurrFontHeight * CurrYScale
            * 0.35 * CurrWorldTr[2, 2])}));
        end
              //an approximate rule-of-thumb correction when text is aligned at baseline
        else //GetXY(CPoint.X, CPoint.Y)
          NodeAddXY('x', 'y', XY);
        XMLNode.AttributeValue['nChars'] := emrtext.NChars;
        XMLNode.AttributeValue['Offset_to_string']
          := emrtext.offString;
        //if Trim((ARecord as TEMF_ExtTextOut).Str) <> '' then
        XMLNode.AttributeValue['t']
          := (ARecord as TEMF_ExtTextOut).Str;
        //else XMLNode.AttributeValue['t']
          //:= '<?' + (ARecord as TEMF_ExtTextOut).Str + '?>';
  //TA_LEFT = 0;  TA_RIGHT = 2;  TA_CENTER = 6;
  //TA_TOP = 0;  TA_BOTTOM = 8;  TA_BASELINE = 24;
        case CurrTextAlignment and 6 of
          TA_RIGHT: XMLNode.AttributeValue['jh'] := 'r';
          TA_CENTER: XMLNode.AttributeValue['jh'] := 'c';
        else XMLNode.AttributeValue['jh'] := 'l';
        end;
        case CurrTextAlignment and 24 of
          //TA_BASELINE: XMLNode.AttributeValue['jv'] := 'c';
          TA_BASELINE: ;
          TA_BOTTOM: XMLNode.AttributeValue['jv'] := 'b';
        else XMLNode.AttributeValue['jv'] := 't';
        end;
        if CurrTextColor <> clBlack then
          XMLNode.AttributeValue['lc'] := ColorToHtml(CurrTextColor);
{EMR_EXTTEXTOUTA
  RECTL   rclBounds; //Bounding rectangle, in device units.
  DWORD   iGraphicsMode; //Current graphics mode. This member can be either the GM_COMPATIBLE or GM_ADVANCED value.
  FLOAT   exScale; //X-scaling factor from page units to .01mm units if the graphics mode is the GM_COMPATIBLE value.
  FLOAT   eyScale; //Y-scaling factor from page units to .01mm units if the graphics mode is the GM_COMPATIBLE value.
  EMRTEXT emrtext; //EMRTEXT structure, which is followed by the string and the intercharacter spacing array    }
{EMRTEXT
  POINTL ptlReference; // Logical coordinates of the reference point used to position the string.
  DWORD  nChars; // Number of characters in string.
  DWORD  offString; // Offset to string.
  DWORD  fOptions; // Value specifying how to use the application-defined rectangle. This member can be a combination of the ETO_CLIPPED and ETO_OPAQUE values.
  RECTL  rcl; // Optional clipping and/or opaquing rectangle, in logical units.
  DWORD  offDx; // Offset to intercharacter spacing array.  };
        with XMLNode.AddElement('font') do
        begin
          St := '';
          for I := 0 to LF_FACESIZE do
          begin
            if CurrFont.lfFaceName[I] = #0 then Break;
            St := St + CurrFont.lfFaceName[I];
          end;
          AttributeValue['face'] := Trim(St);
          if CurrFont.lfWeight >= FW_BOLD then AttributeValue['bf'] := 1;
          if CurrFont.lfItalic = 1 then AttributeValue['it'] := 1;
          AttributeValue['charset'] := CurrFont.lfCharSet;
        end;
    {lfHeight: Longint;
    lfWidth: Longint;
    lfEscapement: Longint;
    lfOrientation: Longint;
    lfWeight: Longint;
    lfItalic: Byte;
    lfUnderline: Byte;
    lfStrikeOut: Byte;
    lfCharSet: Byte;
    lfOutPrecision: Byte;
    lfClipPrecision: Byte;
    lfQuality: Byte;
    lfPitchAndFamily: Byte;
    lfFaceName: array[0..LF_FACESIZE - 1] of WideChar;}
      end
    end
    else if (ARecord is TEMF_SetWorldTransform) then
    begin
      CurrWorldTr := XFormToTransf2D((ARecord as
        TEMF_SetWorldTransform).EMR_Info.XFORM);
    end
    else if (ARecord is TEMF_ModifyWorldTransform) then
    begin
      TempTr := XFormToTransf2D((ARecord
        as TEMF_ModifyWorldTransform).EMR_Info.XFORM);
      case (ARecord as
        TEMF_ModifyWorldTransform).EMR_Info.iMode of
        MWT_IDENTITY: CurrWorldTr := IdentityTransf2D;
        MWT_LEFTMULTIPLY:
          CurrWorldTr := MultiplyTransform2D(TempTr, CurrWorldTr);
        MWT_RIGHTMULTIPLY:
          CurrWorldTr := MultiplyTransform2D(CurrWorldTr, TempTr);
      end;
    end
    else if (ARecord is TEMF_PathBounds) then
    begin
      FirstMoveTo := True;
      PathArr[PathArrPos] := Point2D(-1234, -1234);
      if not UseBezier then
      begin
        for I := 0 to PathArrPos do
        begin
          if PathArr[I].X = -1234 then
          begin
            if not FirstMoveTo then
            begin
              XMLNode.Text := St;
              AddPathAttributes;
              if IsSamePoint2D(CurrXY, PathArr[I - 1])
                then XMLNode.SetTagName('polygon')
                ; //XMLNode.BaseName := 'polygon';
            end;
            if I < PathArrPos then
            begin
              CurrXY := PathArr[I + 1];
              XMLNode := AddItem('polyline');
              //AddPoint2D(CurrXY);
            end;
            FirstMoveTo := False;
            St := '';
          end
          else
          begin
            St := St +
              Format('%.5g,%.5g', [PathArr[I].X, PathArr[I].Y]);
            if I < PathArrPos then St := St + ' ';
          end;
        end;
      end
      else
      begin
        for I := 0 to PathArrPos do
        begin
          if PathArr[I].X = -1234 then
          begin
            if not FirstMoveTo then
            begin
              XMLNode.Text := St;
              AddPathAttributes;
              if IsSamePoint2D(CurrXY, PathArr[I - 1])
                then XMLNode.AttributeValue['closed'] := '1';
            end;
            if I < PathArrPos then
            begin
              XMLNode := AddItem('bezier');
              CurrXY := PathArr[I + 1];
            end;
            FirstMoveTo := False;
            St := '';
            BezMod := 0;
          end
          else
          begin
            BezArr[BezMod] := PathArr[I];
            if BezMod = 0 then AddPoint2D(PathArr[I]);
            if BezMod = 3 then
            begin
              if not (IsSamePoint2D(BezArr[0], BezArr[3])
                and IsSamePoint2D(BezArr[0], BezArr[1])) then
              begin
                //for J := 0 to 8 do
                //  AddPoint2D(BezierPoint(BezArr[0], BezArr[1],
                //    BezArr[2], BezArr[3], {0.03 + J / 8 * 0.97} BezFract[J]));
                AddPoint2D(BezArr[1]);
                AddPoint2D(BezArr[2]);
                AddPoint2D(BezArr[3]);
                BezArr[0] := BezArr[3];
              end;
            end;
            BezMod := Succ(BezMod mod 3);
          end;
        end;
      end;
    end
    else
    begin
      XMLNode := AInfoRec;
    end;
  end;
  AddLineTolist;
  LineToList.Free;
  Handles.Free;
  DC_InfoList.Free;
end;

end.

