unit Devices;

{$IFNDEF VER140}
{$MODE Delphi}
{$ENDIF}

interface

uses SysUtils, Classes, Graphics, Geometry;

{$I tpx.inc}

const

  DRAWMODE_OutlineOnly = 64;

type

  TLineStyle = (liNone, liSolid, liDotted, liDashed);

  THatching = (haNone, haHorizontal, haVertical,
    haFDiagonal, haBDiagonal, haCross, haDiagCross);

  { Text horizontal alignment }
  THAlignment = (ahLeft, ahCenter, ahRight);

  { Text vertical alignment }
  TVAlignment = (jvBaseline, jvBottom, jvCenter, jvTop);

  TPathProc = procedure(PP: TPointsSet2D;
    const LineColor, HatchColor, FillColor: TColor;
    const LineStyle: TLineStyle; const LineWidth: TRealType;
    const Hatching: THatching; const Transf: TTransf2D;
    const Closed: Boolean) of object;

  TRectProc = procedure(const P: TPoint2D; const W, H: TRealType;
    const LineColor, HatchColor, FillColor: TColor;
    const LineStyle: TLineStyle; const LineWidth: TRealType;
    const Hatching: THatching) of object;

  TRotRectProc = procedure(const P: TPoint2D;
    const W, H, ARot: TRealType;
    const LineColor, HatchColor, FillColor: TColor;
    const LineStyle: TLineStyle; const LineWidth: TRealType;
    const Hatching: THatching) of object;

  TEllipseProc = procedure(const CP: TPoint2D;
    const RX, RY: TRealType;
    const LineColor, HatchColor, FillColor: TColor;
    const LineStyle: TLineStyle; const LineWidth: TRealType;
    const Hatching: THatching) of object;

  TRotEllipseProc = procedure(const CP: TPoint2D;
    const RX, RY, ARot: TRealType;
    const LineColor, HatchColor, FillColor: TColor;
    const LineStyle: TLineStyle; const LineWidth: TRealType;
    const Hatching: THatching) of object;

  TCircleProc = procedure(const CP: TPoint2D; const R: TRealType;
    const LineColor, HatchColor, FillColor: TColor;
    const LineStyle: TLineStyle; const LineWidth: TRealType;
    const Hatching: THatching) of object;

  TCircularProc = procedure(const CP: TPoint2D; R, SA, EA:
    TRealType;
    const LineColor, HatchColor, FillColor: TColor;
    const LineStyle: TLineStyle; const LineWidth: TRealType;
    const Hatching: THatching; const Kind: TCircularKind) of
    object;

  TTextProc = procedure(P: TPoint2D; H: TRealType;
    WideText: WideString; TeXText: AnsiString;
    const HAlignment: THAlignment;
    const LineColor: TColor;
    const AFaceName: AnsiString;
    const Charset: TFontCharSet; const Style: TFontStyles) of
    object;

  TRotTextProc = procedure(P: TPoint2D; H, ARot: TRealType;
    WideText: WideString; TeXText: AnsiString;
    const HAlignment: THAlignment;
    const LineColor: TColor;
    const AFaceName: AnsiString;
    const Charset: TFontCharSet; const Style: TFontStyles) of
    object;

  TBitmapProc = procedure(P: TPoint2D; W, H: TRealType;
    const KeepAspectRatio: Boolean;
    BitmapEntry: TObject) of object;

  TGroupProc = procedure of object;

  TGenPathProc = procedure(const GP: TGenericPath;
    const LineColor, HatchColor, FillColor: TColor;
    const LineStyle: TLineStyle; const LineWidth: TRealType;
    const Hatching: THatching;
    const Transf: TTransf2D) of object;

{ ===============================================================
 TDevice is a basic class for output devices, which are used to
  render graphical objects.
 Use it to produce new export devices
  (for example, TSvgDevice in DevSVG)
 It is also used for screen devices (see TCanvasDevice)
 ===============================================================}

  TDevice = class(TObject)
  protected
    fT: TTransf2D;
    fTScale: TRealType;
    fFactorMM: TRealType;
    fHasBezier, fHasClosedBezier,
      fHasArc, fHasSector, fHasSegment,
      fHasNativeHatching, fDisjointFill,
      fTextAsRect: Boolean;
    fHatchingStep, fHatchingLineWidth: TRealType;
    fLineWidthBase, fDottedSize, fDashSize, fMiterLimit,
      fApproximationPrecision: TRealType;
    procedure SetTransform(const Transf: TTransf2D);
    function MultTransf(const Transf: TTransf2D): TTransf2D;
    procedure HatchPath(
      const IsBezier: Boolean; PP: TPointsSet2D;
      const LineColor, HatchColor, FillColor: TColor;
      const LineStyle: TLineStyle; const LineWidth: TRealType;
      const Hatching: THatching; const Transf: TTransf2D;
      const Closed: Boolean); virtual;
    procedure HatchingLine(P0, P1: TPoint2D;
      const LineColor: TColor; const LineStyle: TLineStyle;
      const LineWidth: TRealType); virtual;
    procedure WriteHatchingLines(const Lines: TPointsSet2D;
      const HatchColor: TColor; const LineWidth: TRealType);
      virtual;
    procedure Poly(PP: TPointsSet2D;
      const LineColor, HatchColor, FillColor: TColor;
      const LineStyle: TLineStyle; const LineWidth: TRealType;
      const Hatching: THatching; const Transf: TTransf2D;
      const Closed: Boolean);
      virtual; abstract;
  public
    OnCircle: TCircleProc;
    OnEllipse: TEllipseProc;
    OnBezier: TPathProc;
    OnRotEllipse: TRotEllipseProc;
    OnRect: TRectProc;
    OnRotRect: TRotRectProc;
    OnCircular: TCircularProc;
    OnText: TTextProc;
    OnRotText: TRotTextProc;
    OnBitmap: TBitmapProc;
    OnStartGroup, OnFinishGroup: TGroupProc;
    OnGenPath: TGenPathProc;
    constructor Create;
{ Polyline/polygon and polybezier methods (Poly, Bezier)
   must do their transforms themselves!
  (This allows to use the points from the object without duplicating them)}
    procedure PiecePath(Piece: TObject;
      const LineColor, HatchColor, FillColor: TColor;
      const LineStyle: TLineStyle; const LineWidth: TRealType;
      const Hatching: THatching; const Transf: TTransf2D;
      const Closed: Boolean); virtual;
    procedure PieceCh(Piece: TObject;
      const LineColor, HatchColor, FillColor: TColor;
      const LineStyle: TLineStyle; const LineWidth: TRealType;
      const Hatching: THatching; const Transf: TTransf2D); virtual;
    procedure PolyCh(PP: TPointsSet2D;
      const LineColor, HatchColor, FillColor: TColor;
      const LineStyle: TLineStyle; const LineWidth: TRealType;
      const Hatching: THatching; const Transf: TTransf2D;
      const Closed: Boolean); virtual;
    procedure BezierCh(PP: TPointsSet2D;
      const LineColor, HatchColor, FillColor: TColor;
      const LineStyle: TLineStyle; const LineWidth: TRealType;
      const Hatching: THatching; const Transf: TTransf2D;
      const Closed: Boolean); virtual;
    procedure RectCh(const P: TPoint2D;
      const W, H, ARot: TRealType;
      const LineColor, HatchColor, FillColor: TColor;
      const LineStyle: TLineStyle; const LineWidth: TRealType;
      const Hatching: THatching; const Transf: TTransf2D); virtual;
    procedure EllipseCh(const CP: TPoint2D;
      const RX, RY, ARot: TRealType;
      const LineColor, HatchColor, FillColor: TColor;
      const LineStyle: TLineStyle; const LineWidth: TRealType;
      const Hatching: THatching; const Transf: TTransf2D); virtual;
    procedure CircleCh(const CP: TPoint2D; const R: TRealType;
      const LineColor, HatchColor, FillColor: TColor;
      const LineStyle: TLineStyle; const LineWidth: TRealType;
      const Hatching: THatching; const Transf: TTransf2D); virtual;
    procedure CircularCh(const CP: TPoint2D; R, SA, EA: TRealType;
      const LineColor, HatchColor, FillColor: TColor;
      const LineStyle: TLineStyle; const LineWidth: TRealType;
      const Hatching: THatching; const Kind: TCircularKind;
      const Transf: TTransf2D);
      virtual;
    procedure TextCh(const P: TPoint2D; H, ARot: TRealType;
      WideText: WideString; TeXText: AnsiString;
      const HAlignment: THAlignment;
      const LineColor: TColor;
      const AFaceName: AnsiString;
      const Charset: TFontCharSet; const Style: TFontStyles;
      const Transf: TTransf2D);
      virtual;
    procedure BitmapCh(const P: TPoint2D;
      const W, H: TRealType; const KeepAspectRatio: Boolean;
      BitmapEntry: TObject; const Transf: TTransf2D); virtual;
    procedure WriteHatching(
      const PP: TPointsSet2D; const Hatching: THatching;
      HatchColor: TColor; const LineWidth: TRealType;
      Step: TRealType; const Transf: TTransf2D); virtual;
    procedure WriteHeader(ExtRect: TRect2D); virtual;
    procedure WriteFooter; virtual;
    property T: TTransf2D read fT write SetTransform;
    property TScale: TRealType read fTScale;
    property FactorMM: TRealType read fFactorMM write fFactorMM;
    property HasBezier: Boolean read fHasBezier;
    property HasClosedBezier: Boolean read fHasClosedBezier;
    property HasArc: Boolean read fHasArc;
    property HasSector: Boolean read fHasSector;
    property HasSegment: Boolean read fHasSegment;
    property HasNativeHatching: Boolean read fHasNativeHatching;
    property DisjointFill: Boolean read fDisjointFill;
    property TextAsRect: Boolean read fTextAsRect;
    property HatchingStep: TRealType write fHatchingStep;
    property HatchingLineWidth: TRealType write fHatchingLineWidth;
    property LineWidthBase: TRealType write fLineWidthBase;
    property ApproximationPrecision: TRealType
      write fApproximationPrecision;
    property DottedSize: TRealType write fDottedSize;
    property DashSize: TRealType write fDashSize;
    property MiterLimit: TRealType write fMiterLimit;
  end;

  TStreamDevice = class(TDevice)
  protected
    fStream: TStream;
    procedure SetStream(AStream: TStream); virtual;
    procedure WriteStreamPoint0(const X, Y: TRealType); virtual;
  public
    procedure WriteStream(Value: Variant);
    procedure WriteLnStream(Value: Variant);
    procedure WriteStreamPoint(P: TPoint2D); virtual;
    procedure WriteStreamPointT(P: TPoint2D;
      const Transf: TTransf2D); virtual;
    property Stream: TStream read fStream write SetStream;
  end;

  TFileDevice = class(TStreamDevice)
  protected
    fFileName: string;
    fIncludePath: string;
  public
    property FileName: string read fFileName write fFileName;
    property IncludePath: string read fIncludePath write
      fIncludePath;
  end;

function FF_N(const X: TRealType; const Prec: Integer): string;

implementation

uses Drawings, GObjects, SysBasic, PreView, Pieces;

function FF_N(const X: TRealType; const Prec: Integer): string;
begin
  Result := Format('%.' + IntToStr(Prec) + 'f', [X]);
end;

// =====================================================================
// TDevice
// =====================================================================

constructor TDevice.Create;
begin
  inherited Create;
  fT := IdentityTransf2D;
  fDisjointFill := False;
//  OnBitmap := nil;
end;

procedure TDevice.SetTransform(const Transf: TTransf2D);
begin
  fT := Transf;
  fTScale := IsotropicScale(Transf);
  if fTScale = 0 then fTScale := 1;
end;

function TDevice.MultTransf(const Transf: TTransf2D): TTransf2D;
begin
  Result := MultiplyTransform2D(Transf, fT);
end;

procedure TDevice.PiecePath(Piece: TObject;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching; const Transf: TTransf2D;
  const Closed: Boolean);
var
  APiece: TPiece;
  P: TPoint2D;
  Size: TVector2D;
begin
  APiece := Piece as TPiece;
  if APiece is TPolyLinPiece then
    Poly(APiece, LineColor, HatchColor, FillColor,
      LineStyle, LineWidth, Hatching, Transf, Closed)
  else if APiece is TBezierPiece then
    OnBezier(APiece, LineColor, HatchColor, FillColor,
      LineStyle, LineWidth, Hatching, Transf, Closed)
  else if APiece is TCirclePiece then
    CircleCh(APiece[0], (APiece as TCirclePiece).R,
      LineColor, HatchColor, FillColor,
      LineStyle, LineWidth, Hatching, Transf)
  else if APiece is TCircularPiece then
    CircularCh(APiece[0], (APiece as TCircularPiece).R,
      (APiece as TCircularPiece).SA, (APiece as TCircularPiece).EA,
      LineColor, HatchColor, FillColor,
      LineStyle, LineWidth, Hatching,
      (APiece as TCircularPiece).Kind, Transf)
  else if APiece is TRectanglePiece then
    RectCh(APiece[0], (APiece as TRectanglePiece).W,
      (APiece as TRectanglePiece).H,
      (APiece as TRectanglePiece).ARot,
      LineColor, HatchColor, FillColor,
      LineStyle, LineWidth, Hatching, Transf)
  else if APiece is TEllipsePiece then
    EllipseCh(APiece[0], (APiece as TEllipsePiece).RX,
      (APiece as TEllipsePiece).RY, (APiece as TEllipsePiece).ARot,
      LineColor, HatchColor, FillColor,
      LineStyle, LineWidth, Hatching, Transf)
  else if APiece is TTextPiece then
    if not fTextAsRect then
    begin
      TextCh(APiece[0],
        (APiece as TTextPiece).Height,
        (APiece as TTextPiece).ARot,
        (APiece as TTextPiece).WideText,
        (APiece as TTextPiece).TeXText,
        (APiece as TTextPiece).HAlignment,
        LineColor,
        (APiece as TTextPiece).FaceName,
        (APiece as TTextPiece).Charset,
        (APiece as TTextPiece).Style, Transf);
    end
    else
    begin
      (APiece as TTextPiece).MeasureTextRectangle(P, Size);
      RectCh(P, Size.X, Size.Y, (APiece as TTextPiece).ARot,
        clBlue, clNone, clCream, liSolid, 1, haNone, Transf);
    end
  else if APiece is TBitmapPiece then
  begin
    BitmapCh(APiece[0],
      (APiece as TBitmapPiece).Width,
      (APiece as TBitmapPiece).Height,
      (APiece as TBitmapPiece).KeepAspectRatio,
      (APiece as TBitmapPiece).BitmapEntry,
      Transf);
  end
  else if APiece is TGenPathPiece then
  begin
    if Assigned(OnGenPath) then
      OnGenPath((APiece as TGenPathPiece).Path,
        LineColor, HatchColor, FillColor,
        LineStyle, LineWidth, Hatching, Transf);
  end
  else
  begin
    (APiece as TTextPiece).MeasureTextRectangle(P, Size);
    RectCh(P, Size.X, Size.Y, (APiece as TTextPiece).ARot,
      clBlack, clNone, clNone, liSolid, 1, haNone, Transf);
  end;
  ;
end;

procedure TDevice.PieceCh(Piece: TObject;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching; const Transf: TTransf2D);
var
  APiece: TPiece;
  procedure MakeLinear;
  var
    LinPath: TPolyLinPiece;
  begin
    LinPath := TPolyLinPiece.Create(0);
    try
      APiece.Linearize(LinPath, fApproximationPrecision);
      LinPath.Assign(APiece);
      PieceCh(LinPath, LineColor, HatchColor, FillColor,
        LineStyle, LineWidth, Hatching, Transf);
    finally
      LinPath.Free;
    end;
  end;
var
  TmpHatching: THatching;
  procedure LinearizeHatch;
  var
    LinPP: TPointsSet2D;
  begin
    LinPP := TPointsSet2D.Create(0);
    try
      APiece.Linearize(LinPP, fApproximationPrecision);
      WriteHatching(LinPP, Hatching, HatchColor,
        fHatchingLineWidth, fHatchingStep, Transf);
    finally
      LinPP.Free;
    end;
  end;
begin
  APiece := Piece as TPiece;
  if not ((APiece is TTextPiece) or (APiece is TBitmapPiece)
    ) then //    or (APiece is TGenPathPiece)
  begin
    if (APiece.Count < 1) and
      not (APiece is TGenPathPiece) then Exit;
    if not (APiece is TPolyLinPiece) then
      if (not Assigned(OnBezier)
        or (not APiece.Closed and not fHasBezier)
        or (APiece.Closed and not fHasClosedBezier))
        and not (APiece is TGenPathPiece) then
      begin
        MakeLinear;
        Exit;
      end;
    if ((Hatching = haNone) or fHasNativeHatching)
      and not fDisjointFill then
    begin
      PiecePath(APiece, LineColor, HatchColor, FillColor,
        LineStyle, LineWidth, Hatching, Transf, APiece.Closed);
      Exit;
    end;
    if not fHasNativeHatching then
      TmpHatching := haNone
    else
      TmpHatching := Hatching;
    if FillColor <> clDefault then
      PiecePath(APiece, LineColor, HatchColor, FillColor,
        liNone, LineWidth, TmpHatching, Transf, True);
    if (Hatching <> haNone) and not fHasNativeHatching then
      if APiece is TPolyLinPiece then
        WriteHatching(APiece, Hatching, HatchColor,
          fHatchingLineWidth, fHatchingStep, Transf) // * fFactorMM
      else
        LinearizeHatch;
    if LineStyle = liNone then Exit;
  end;
  PiecePath(APiece, LineColor, HatchColor, clDefault,
    LineStyle, LineWidth, TmpHatching, Transf, APiece.Closed);
end;

procedure TDevice.PolyCh(PP: TPointsSet2D;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching; const Transf: TTransf2D;
  const Closed: Boolean);
begin
  if PP.Count < 1 then Exit;
  if ((Hatching = haNone) or fHasNativeHatching)
    and not fDisjointFill then
    Poly(PP, LineColor, HatchColor, FillColor,
      LineStyle, LineWidth, Hatching, Transf, Closed)
  else
    HatchPath(False, PP, LineColor, HatchColor, FillColor,
      LineStyle, LineWidth, Hatching, Transf, Closed);
end;

procedure TDevice.BezierCh(PP: TPointsSet2D;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching; const Transf: TTransf2D;
  const Closed: Boolean);
  procedure WriteAsLinear;
  var
    LinPP: TPointsSet2D;
  begin
    LinPP := TPointsSet2D.Create(0);
    try
      LinearizeBezier(PP, fApproximationPrecision, Closed, LinPP);
      PolyCh(LinPP, LineColor, HatchColor, FillColor,
        LineStyle, LineWidth, Hatching, Transf,
        Closed);
    finally
      LinPP.Free;
    end;
  end;
begin
  if PP.Count < 1 then Exit;
  if Assigned(OnBezier) then
    if (not Closed and fHasBezier)
      or (Closed and fHasClosedBezier) then
    begin
      if ((Hatching = haNone) or fHasNativeHatching)
        and not fDisjointFill then
        OnBezier(PP, LineColor, HatchColor, FillColor,
          LineStyle, LineWidth, Hatching, Transf, Closed)
      else
        HatchPath(True, PP, LineColor, HatchColor, FillColor,
          LineStyle, LineWidth, Hatching, Transf, Closed);
      Exit;
    end;
  WriteAsLinear;
end;

procedure TDevice.HatchPath(
  const IsBezier: Boolean; PP: TPointsSet2D;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching; const Transf: TTransf2D;
  const Closed: Boolean);
var
  PathProc: TPathProc;
  TmpHatching: THatching;
  procedure HatchBezier;
  var
    LinPP: TPointsSet2D;
  begin
    LinPP := TPointsSet2D.Create(0);
    try
      LinearizeBezier(PP, fApproximationPrecision, Closed, LinPP);
      WriteHatching(LinPP, Hatching, HatchColor,
        fHatchingLineWidth, fHatchingStep, Transf);
    finally
      LinPP.Free;
    end;
  end;
begin
  if IsBezier then
    PathProc := OnBezier
  else
    PathProc := Poly;
  if not fHasNativeHatching then
    TmpHatching := haNone
  else
    TmpHatching := Hatching;
  if FillColor <> clDefault then
    PathProc(PP, LineColor, HatchColor, FillColor,
      liNone, LineWidth, TmpHatching, Transf, True);
  if (Hatching <> haNone) and not fHasNativeHatching then
    if IsBezier then
      HatchBezier
    else
      WriteHatching(PP, Hatching, HatchColor,
        fHatchingLineWidth, fHatchingStep, Transf); // * fFactorMM
  if LineStyle <> liNone then
    PathProc(PP, LineColor, HatchColor, clDefault,
      LineStyle, LineWidth, TmpHatching, Transf, Closed);
end;

procedure TDevice.RectCh(const P: TPoint2D;
  const W, H, ARot: TRealType;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching; const Transf: TTransf2D);
  procedure WriteAsLinear;
  var
    LinPP: TPointsSet2D;
  begin
    LinPP := TPointsSet2D.Create(4);
    try
      LinearizeRectangle(LinPP, P, W, H, ARot);
      PolyCh(LinPP, LineColor, HatchColor, FillColor,
        LineStyle, LineWidth, Hatching, Transf, True);
    finally
      LinPP.Free;
    end;
  end;
begin
  if Assigned(OnRect) and (ARot = 0) then
  begin
    OnRect(TransformPoint2D(P, MultTransf(Transf)),
      W * fTScale, H * fTScale,
      LineColor, HatchColor, FillColor,
      LineStyle, LineWidth, Hatching);
    Exit;
  end
  else if Assigned(OnRotRect) then
  begin
    OnRotRect(TransformPoint2D(P, MultTransf(Transf)),
      W * fTScale, H * fTScale, ARot,
      LineColor, HatchColor, FillColor,
      LineStyle, LineWidth, Hatching);
    Exit;
  end;
  WriteAsLinear;
end;

procedure TDevice.EllipseCh(const CP: TPoint2D;
  const RX, RY, ARot: TRealType;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching; const Transf: TTransf2D);
  procedure WriteAsBezier;
  var
    PP: TPointsSet2D;
  begin
    PP := TPointsSet2D.Create(0);
    try
      EllipseBezierPoints8(CP, RX, RY, ARot, PP);
      BezierCh(PP,
        LineColor, HatchColor, FillColor,
        LineStyle, LineWidth, Hatching, Transf, True);
    finally
      PP.Free;
    end;
  end;
begin
  if Assigned(OnEllipse) and (ARot = 0) then
  begin
    OnEllipse(TransformPoint2D(CP, MultTransf(Transf)),
      RX * fTScale, RY * fTScale,
      LineColor, HatchColor, FillColor,
      LineStyle, LineWidth, Hatching);
    Exit;
  end
  else if Assigned(OnRotEllipse) then
  begin
    OnRotEllipse(TransformPoint2D(CP, MultTransf(Transf)),
      RX * fTScale, RY * fTScale, ARot,
      LineColor, HatchColor, FillColor,
      LineStyle, LineWidth, Hatching);
    Exit;
  end;
  WriteAsBezier;
end;

procedure TDevice.CircleCh(const CP: TPoint2D; const R: TRealType;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching; const Transf: TTransf2D);
  procedure WriteAsBezier;
  var
    PP: TPointsSet2D;
  begin
    PP := TPointsSet2D.Create(0);
    try
      EllipseBezierPoints8(CP, R, R, 0, PP);
      BezierCh(PP,
        LineColor, HatchColor, FillColor,
        LineStyle, LineWidth, Hatching, Transf, True);
    finally
      PP.Free;
    end;
  end;
begin
  if Assigned(OnCircle) then
  begin
    OnCircle(
      TransformPoint2D(CP, MultTransf(Transf)), R * fTScale,
      LineColor, HatchColor, FillColor,
      LineStyle, LineWidth, Hatching);
    Exit;
  end;
  WriteAsBezier;
end;

procedure TDevice.CircularCh(const CP: TPoint2D; R, SA, EA:
  TRealType;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching; const Kind: TCircularKind;
  const Transf: TTransf2D);
  procedure WriteAsBezier;
  var
    PP: TPointsSet2D;
  begin
    PP := TPointsSet2D.Create(0);
    try
      CircularBezierPoints(CP, R, SA, EA, PP, Kind);
      BezierCh(PP,
        LineColor, HatchColor, FillColor,
        LineStyle, LineWidth, Hatching, Transf, Kind <> ci_Arc);
    finally
      PP.Free;
    end;
  end;
begin
  if Assigned(OnCircular) then
    if ((Kind = ci_Arc) and HasArc)
      or ((Kind = ci_Sector) and HasSector)
      or ((Kind = ci_Segment) and HasSegment) then
    begin
      OnCircular(TransformPoint2D(CP, MultTransf(Transf)),
        R * fTScale, SA, EA,
        LineColor, HatchColor, FillColor,
        LineStyle, LineWidth, Hatching, Kind);
      Exit;
    end;
  WriteAsBezier;
end;

procedure TDevice.TextCh(
  const P: TPoint2D; H, ARot: TRealType;
  WideText: WideString; TeXText: AnsiString;
  const HAlignment: THAlignment;
  const LineColor: TColor;
  const AFaceName: AnsiString;
  const Charset: TFontCharSet; const Style: TFontStyles;
  const Transf: TTransf2D);
begin
  if (Assigned(OnText) and (ARot = 0))
    or not Assigned(OnRotText) then
  begin
    OnText(TransformPoint2D(P, MultTransf(Transf)),
      H * fTScale, WideText, TeXText,
      HAlignment,
      LineColor, AFaceName, Charset, Style);
    Exit;
  end;
  OnRotText(TransformPoint2D(P, MultTransf(Transf)),
    H * fTScale, ARot, WideText, TeXText,
    HAlignment, 
    LineColor, AFaceName, Charset, Style);
end;

procedure TDevice.BitmapCh(const P: TPoint2D;
  const W, H: TRealType; const KeepAspectRatio: Boolean;
  BitmapEntry: TObject; const Transf: TTransf2D);
begin
  if not Assigned(OnBitmap) then
  begin
    RectCh(P, W, H, 0,
      clBlack, clNone, clNone, liSolid, 1, haNone, Transf);
    Exit;
  end;
  OnBitmap(TransformPoint2D(P, MultTransf(Transf)),
    W * fTScale, H * fTScale, KeepAspectRatio, BitmapEntry);
end;

procedure TDevice.HatchingLine(P0, P1: TPoint2D;
  const LineColor: TColor; const LineStyle: TLineStyle;
  const LineWidth: TRealType);
begin

end;

procedure TDevice.WriteHatchingLines(const Lines: TPointsSet2D;
  const HatchColor: TColor; const LineWidth: TRealType);
var
  I: Integer;
begin
  for I := 0 to Lines.Count div 2 - 1 do
  begin
    HatchingLine(Lines[I * 2], Lines[I * 2 + 1],
      HatchColor, liSolid, LineWidth);
  end;
end;

procedure TDevice.WriteHatching(
  const PP: TPointsSet2D; const Hatching: THatching;
  HatchColor: TColor; const LineWidth: TRealType;
  Step: TRealType; const Transf: TTransf2D);
var
  Lines: TPointsSet2D;
  DX, DY: TRealType;
  procedure WriteHatching0;
  begin
    if (DX = 0) and (DY = 0) then Exit;
    CalculateHatching(PP, DX, DY,
      Step * fFactorMM / fTScale, Lines, fr_Winding);
    if HatchColor = clDefault then HatchColor := clBlack;
    Lines.TransformPoints(Transf);
    WriteHatchingLines(Lines, HatchColor, LineWidth);
  end;
begin
  if Hatching = haNone then Exit;
  Lines := TPointsSet2D.Create(10);
  DX := HatchingDirections[Ord(Hatching)][1];
  DY := HatchingDirections[Ord(Hatching)][2];
  WriteHatching0;
  DX := HatchingDirections[Ord(Hatching)][3];
  DY := HatchingDirections[Ord(Hatching)][4];
  Lines.Clear;
  WriteHatching0;
  Lines.Free;
end;

procedure TDevice.WriteHeader(ExtRect: TRect2D);
begin

end;

procedure TDevice.WriteFooter;
begin

end;

// =====================================================================
// TStreamDevice
// =====================================================================

procedure TStreamDevice.SetStream(AStream: TStream);
begin
  fStream := AStream;
end;

procedure TStreamDevice.WriteStream(Value: Variant);
var
  ValueSt: string;
begin
  ValueSt := Value;
  if ValueSt = '' then Exit;
  fStream.Write(ValueSt[1], Length(ValueSt));
end;

procedure TStreamDevice.WriteLnStream(Value: Variant);
var
  ValueSt: string;
begin
  ValueSt := Value;
  ValueSt := ValueSt + EOL;
  fStream.Write(ValueSt[1], Length(ValueSt));
end;

procedure TStreamDevice.WriteStreamPoint0(const X, Y: TRealType);
begin
  WriteStream(Format('(%.2f,%.2f)', [X, Y]));
end;

procedure TStreamDevice.WriteStreamPoint(P: TPoint2D);
begin
  WriteStreamPoint0(P.X, P.Y);
end;

procedure TStreamDevice.WriteStreamPointT(P: TPoint2D;
  const Transf: TTransf2D);
begin
  P := TransformPoint2D(P, MultTransf(Transf));
  WriteStreamPoint0(P.X, P.Y);
end;

end.

