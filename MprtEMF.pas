unit MprtEMF;

{$IFNDEF VER140}
{$MODE Delphi}
{$ENDIF}

interface

uses Types, SysUtils, Classes, Graphics, ComCtrls,
  Dialogs, Contnrs, Math, StrUtils, Forms,
{$IFNDEF VER140}
  LCLIntf,
{$ELSE}
  Windows,
{$ENDIF}
  EMF_Add, Devices, Geometry, GObjects, Drawings, Input;

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

type

  TMetaRecordInfo = packed record
    Fu: DWORD; // Function number (defined in WINGDI.H) */
    Size: DWORD;
      // Total size of the record in WORDs ? (bytes) */
  end;

  // A class representing metafile
  T_EMF_Structure = class(TObjectList)
    Header: T_EMFHeader;
    pLogStrings: TStrings; // pointer to a log
    constructor Create;
    procedure LoadFromStream(const Stream: TStream);
    procedure PreparePath;
  end;

  TRecordProc = procedure(const ARecord: TEMF_Record)
    of object;

  T_EMF_Import = class(T_Import)
  protected
    PathArr: array of TPoint2D;
    PathArrPos: Integer;
    HasPath, HasStroke, HasFill, HasClip, UseBezier: Boolean;
    CurrTextAlignment: DWORD;
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
    MMScale, CurrXScale, CurrYScale: TRealType;
    CurrViewportOrigin, CurrWindowOrigin: TPoint;
    CurrViewportExtent, CurrWindowExtent: TSize;
    CurrWorldTr: TTransf2D;
    Handles, DC_InfoList: TObjectList;
    LineToPP: TPointsSet2D;
    RecordProcs, RecordProcs_Path: array[1..97] of TRecordProc;
    procedure SetHandle(Handle: Longword; Obj: TObject);
    function GetHandle(Handle: Longword): TObject;
    procedure SetXYScale;
    procedure AddPathPoint0(P: TPoint2D);
    procedure AddPathPoint(P: TPoint2D);
    function GetXY0(P: TPoint2D): TPoint2D;
    function GetXY(X, Y: TRealType): TPoint2D; overload;
    function GetXY(P: TPoint2D): TPoint2D; overload;
    function CurrPenWidth: TRealType;
    function CurrLineStyle: TLineStyle;
    function CurrPenColor: TColor;
    function CurrHatching: THatching;
    function CurrHatchColor: TColor;
    function CurrBrushColor: TColor;
    procedure SaveDC_Info;
    procedure RestoreDC_Info;
    procedure AddPathAttributes(
      const ARecord: TEMF_Record;
      const Prim: TPrimitive2D);
    procedure WriteRecordToLog(const ARecord: TEMF_Record);
    procedure WriteToLog(const aName: string;
      const aValue: Variant);
    function Add_LineToPP: TPrimitive2D;
//    function AddPrimitive(
//      const Obj: TPrimitive2D): TPrimitive2D; override;
    procedure Rect0_Execute(const ARecord: TEMF_Record);
    procedure RoundRect_Execute(const ARecord: TEMF_Record);
    procedure Arc0_Execute(const ARecord: TEMF_Record);
    procedure StretchDIBits_Execute(const ARecord: TEMF_Record);
    procedure BitBlt_Execute(const ARecord: TEMF_Record);
    procedure PolyBezier_Execute(const ARecord: TEMF_Record);
    procedure Poly_Execute(const ARecord: TEMF_Record);
    procedure PolyPoly_Execute(const ARecord: TEMF_Record);
    procedure ExtTextOut_Execute(const ARecord: TEMF_Record);
    procedure PathBounds_Poly(const ARecord: TEMF_Record);
    procedure PathBounds_Bezier(const ARecord: TEMF_Record);
    procedure PathBounds_Execute(const ARecord: TEMF_Record);
    procedure SetExtEx_Execute(const ARecord: TEMF_Record);
    procedure SetOrgEx_Execute(const ARecord: TEMF_Record);
    procedure ScaleExtEx_Execute(const ARecord: TEMF_Record);
    procedure BeginPath_Execute(const ARecord: TEMF_Record);
    procedure EndPath_Execute(const ARecord: TEMF_Record);
    procedure MoveToEx_Execute(const ARecord: TEMF_Record);
    procedure LineTo_Execute(const ARecord: TEMF_Record);
    procedure ObjectHandle_Execute(const ARecord: TEMF_Record);
    procedure SaveDC_Execute(const ARecord: TEMF_Record);
    procedure RestoreDC_Execute(const ARecord: TEMF_Record);
    procedure ExtCreateFontIndirectW_Execute(
      const ARecord: TEMF_Record);
    procedure SetMiterLimit_Execute(const ARecord: TEMF_Record);
    procedure CreatePen_Execute(const ARecord: TEMF_Record);
    procedure ExtCreatePen_Execute(const ARecord: TEMF_Record);
    procedure CreateBrushIndirect_Execute(
      const ARecord: TEMF_Record);
    procedure CreateDIBPatternBrushPt_Execute(
      const ARecord: TEMF_Record);
    procedure CreateMonoBrush_Execute(
      const ARecord: TEMF_Record);
    procedure SetBkMode_Execute(const ARecord: TEMF_Record);
    procedure SetMapMode_Execute(const ARecord: TEMF_Record);
    procedure SetBkColor_Execute(const ARecord: TEMF_Record);
    procedure SetTextColor_Execute(const ARecord: TEMF_Record);
    procedure SetTextAlign_Execute(const ARecord: TEMF_Record);
    procedure SetWorldTransform_Execute(
      const ARecord: TEMF_Record);
    procedure ModifyWorldTransform_Execute(
      const ARecord: TEMF_Record);
    procedure ArcTo_Path_Execute(const ARecord: TEMF_Record);
    procedure PolyBezierTo_Path_Execute(
      const ARecord: TEMF_Record);
    procedure PolylineTo16_Path_Execute(
      const ARecord: TEMF_Record);
    procedure PolyDraw16_Path_Execute(
      const ARecord: TEMF_Record);
    procedure MoveToEx_Path_Execute(const ARecord: TEMF_Record);
    procedure CloseFigure_Path_Execute(const ARecord: TEMF_Record);
    procedure LineTo_Path_Execute(const ARecord: TEMF_Record);
  public
    EMF_Struct: T_EMF_Structure;
    IsOld: Boolean;
    ExtOldX, ExtOldY: Integer;
    pLogStrings: TStrings; // pointer to a log
    constructor Create(Drawing: TDrawing2D);
    destructor Destroy; override;
    procedure LoadFromStreamEnh(const Stream: TStream);
{$IFDEF VER140}
    procedure LoadFromMF(const MF: TMetaFile);
{$ENDIF}
    procedure LoadFromStream(const Stream: TStream;
      const IsOld0: Boolean);
    procedure LoadFromFile(const FileName: string);
    procedure ParseEmfRecord(const Index: Integer);
    procedure ParseEmf;
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

uses ColorEtc, SysBasic;


{function WideStringToString(WSt: Widestring): string;
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
end;}

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
  if Assigned(pLogStrings) then pLogStrings.Clear;
  Stream.ReadBuffer(Header, SizeOf(Header));
  Stream.Seek(Header.RecordSize, soFromBeginning);
  LastBeginPath := -1;
  UseBezier := False;
  for RecIndex := 1 to Header.NumOfRecords - 1 do
  begin
    APosition := Stream.Position;
    Stream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
    if Assigned(pLogStrings) then
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
        for I := 0 to (ARecord as TEMF_PolyDraw16).EMR_Info.cpts - 1
          do
          if (ARecord as TEMF_PolyDraw16).abTypes[I] and 6 =
            PT_BEZIERTO
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
        Inc(N, (ARecord as TEMF_PolylineTo32).EMR_Info.cptl *
          NPointsMult)
      else if ARecord is TEMF_PolylineTo16 then
        Inc(N, (ARecord as TEMF_PolylineTo16).EMR_Info.cpts *
          NPointsMult)
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
        for I := 0 to (ARecord as TEMF_PolyDraw16).EMR_Info.cpts - 1
          do
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

     {*---- T_EMF_Import ----*}

constructor T_EMF_Import.Create(Drawing: TDrawing2D);
var
  I: Integer;
begin
  inherited Create(Drawing);
  EMF_Struct := T_EMF_Structure.Create;
  pLogStrings := nil;
  for I := 1 to 97 do RecordProcs[I] := nil;
  RecordProcs[2] := PolyBezier_Execute; // 'POLYBEZIER'
  RecordProcs[3] := Poly_Execute; // 'POLYGON'
  RecordProcs[4] := Poly_Execute; // 'POLYLINE'
  RecordProcs[5] := PolyBezier_Execute; // 'POLYBEZIERTO'
  RecordProcs[6] := Poly_Execute; // 'POLYLINETO'
  RecordProcs[7] := PolyPoly_Execute; // 'POLYPOLYLINE'
  RecordProcs[8] := PolyPoly_Execute; // 'POLYPOLYGON'
  RecordProcs[9] := SetExtEx_Execute; // 'SETWINDOWEXTEX'
  RecordProcs[10] := SetOrgEx_Execute; // 'SETWINDOWORGEX'
  RecordProcs[11] := SetExtEx_Execute; // 'SETVIEWPORTEXTEX'
  RecordProcs[12] := SetOrgEx_Execute; // 'SETVIEWPORTORGEX'
  RecordProcs[17] := SetMapMode_Execute;
  RecordProcs[18] := SetBkMode_Execute;
  RecordProcs[22] := SetTextAlign_Execute;
  RecordProcs[24] := SetTextColor_Execute;
  RecordProcs[25] := SetBkColor_Execute;
  RecordProcs[27] := MoveToEx_Execute;
  RecordProcs[31] := ScaleExtEx_Execute; // 'SCALEVIEWPORTEXTEX'
  RecordProcs[32] := ScaleExtEx_Execute; // 'SCALEWINDOWEXTEX'
  RecordProcs[33] := SaveDC_Execute;
  RecordProcs[34] := RestoreDC_Execute;
  RecordProcs[35] := SetWorldTransform_Execute;
  RecordProcs[36] := ModifyWorldTransform_Execute;
  RecordProcs[37] := ObjectHandle_Execute; // 'SELECTOBJECT'
  RecordProcs[38] := CreatePen_Execute;
  RecordProcs[39] := CreateBrushIndirect_Execute;
  RecordProcs[40] := ObjectHandle_Execute; // 'DELETEOBJECT'
  RecordProcs[42] := Rect0_Execute; // 'ELLIPSE'
  RecordProcs[43] := Rect0_Execute; // 'RECTANGLE'
  RecordProcs[44] := RoundRect_Execute;
  RecordProcs[45] := Arc0_Execute; // 'ARC'
  RecordProcs[46] := Arc0_Execute; // 'CHORD'
  RecordProcs[47] := Arc0_Execute; // 'PIE'
  RecordProcs[54] := LineTo_Execute;
  RecordProcs[55] := Arc0_Execute; // 'ARCTO'
  RecordProcs[58] := SetMiterLimit_Execute;
  RecordProcs[59] := BeginPath_Execute; // 'BEGINPATH'
  RecordProcs[60] := EndPath_Execute; // 'ENDPATH'
  RecordProcs[62] := PathBounds_Execute; // 'FILLPATH'
  RecordProcs[63] := PathBounds_Execute; // 'STROKEANDFILLPATH'
  RecordProcs[64] := PathBounds_Execute; // 'STROKEPATH'
  RecordProcs[67] := PathBounds_Execute; // 'SELECTCLIPPATH'
  RecordProcs[76] := BitBlt_Execute;
  RecordProcs[81] := StretchDIBits_Execute;
  RecordProcs[82] := ExtCreateFontIndirectW_Execute;
  RecordProcs[83] := ExtTextOut_Execute; // 'EXTTEXTOUTA'
  RecordProcs[84] := ExtTextOut_Execute; // 'EXTTEXTOUTW'
  RecordProcs[85] := PolyBezier_Execute; // 'POLYBEZIER16'
  RecordProcs[86] := Poly_Execute; // 'POLYGON16'
  RecordProcs[87] := Poly_Execute; // 'POLYLINE16'
  RecordProcs[88] := PolyBezier_Execute; // 'POLYBEZIERTO16'
  RecordProcs[89] := Poly_Execute; // 'POLYLINETO16'
  RecordProcs[90] := PolyPoly_Execute; // 'POLYPOLYLINE16'
  RecordProcs[91] := PolyPoly_Execute; // 'POLYPOLYGON16'
  RecordProcs[95] := ExtCreatePen_Execute;
  RecordProcs[94] := CreateDIBPatternBrushPt_Execute;
  RecordProcs[93] := CreateMonoBrush_Execute;
  for I := 1 to 97 do RecordProcs_Path[I] := nil;
  RecordProcs_Path[5] :=
    PolyBezierTo_Path_Execute; // 'POLYBEZIERTO'
  RecordProcs_Path[27] := MoveToEx_Path_Execute;
  RecordProcs_Path[54] := LineTo_Path_Execute;
  RecordProcs_Path[55] := ArcTo_Path_Execute;
  RecordProcs_Path[61] := CloseFigure_Path_Execute;
  RecordProcs_Path[88] :=
    PolyBezierTo_Path_Execute; // 'POLYBEZIERTO16'
  RecordProcs_Path[89] := PolylineTo16_Path_Execute;
  RecordProcs_Path[92] := PolyDraw16_Path_Execute;
end;

destructor T_EMF_Import.Destroy;
begin
  EMF_Struct.Free;
  inherited Destroy;
end;

procedure T_EMF_Import.LoadFromStreamEnh(const Stream: TStream);
begin
  Stream.Position := 0;
  EMF_Struct.LoadFromStream(Stream);
  EMF_Struct.PreparePath;
end;

{$IFDEF VER140}

procedure T_EMF_Import.LoadFromMF(const MF: TMetaFile);
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
{$ENDIF}

procedure T_EMF_Import.LoadFromStream(const Stream: TStream;
  const IsOld0: Boolean);
{$IFDEF VER140}
var
  MF: TMetaFile;
{$ENDIF}
begin
  Stream.Position := 0;
{$IFDEF VER140}
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
{$ELSE}
  LoadFromStreamEnh(Stream);
{$ENDIF}
  IsOld := IsOld0;
end;

procedure T_EMF_Import.LoadFromFile(const FileName: string);
var
  Stream: TFileStream;
{$IFDEF VER140}
  MF: TMetaFile;
{$ENDIF}
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
{$IFDEF VER140}
    MF := TMetaFile.Create;
    try
      MF.LoadFromFile(FileName);
      LoadFromMF(MF);
    finally
      MF.Free;
    end;
{$ENDIF}
  end;
end;

procedure T_EMF_Import.AddPathPoint0(P: TPoint2D);
begin
  PathArr[PathArrPos] := P;
  Inc(PathArrPos);
end;

procedure T_EMF_Import.AddPathPoint(P: TPoint2D);
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

procedure T_EMF_Import.SetXYScale;
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
        CurrXScale := CurrViewportExtent.CX / CurrWindowExtent.CX
          * MMScale;
      if (CurrViewportExtent.CY = 0) or (CurrWindowExtent.CY = 0)
        then CurrYScale := MMScale
      else
        CurrYScale := CurrViewportExtent.CY / CurrWindowExtent.CY
          * MMScale;
    end;
  end;
end;

function T_EMF_Import.GetXY0(P: TPoint2D): TPoint2D;
begin
  Result := TransformPoint2D(P, CurrWorldTr);
  case CurrMapMode of
    MM_TEXT:
      begin
        Result := Point2D(Result.X * MMScale,
          BB * MMScale - Result.Y * MMScale);
      end;
    MM_ISOTROPIC, MM_ANISOTROPIC:
      begin
        Result := Point2D(Result.X - CurrWindowOrigin.X,
          Result.Y - CurrWindowOrigin.Y);
        Result := Point2D(
          CurrViewportOrigin.X * MMScale + Result.X * CurrXScale,
          BB * MMScale - (CurrViewportOrigin.Y * MMScale +
          Result.Y * CurrYScale));
      end;
  else
    begin
      Result := Point2D(Result.X * CurrXScale,
        Result.Y * CurrYScale);
    end;
  end;
end;

function T_EMF_Import.GetXY(X, Y: TRealType): TPoint2D;
begin
  Result := GetXY0(Point2D(X, Y));
end;

function T_EMF_Import.GetXY(P: TPoint2D): TPoint2D;
begin
  Result := GetXY0(P);
end;

procedure T_EMF_Import.SetHandle(Handle: Longword; Obj: TObject);
begin
  if (Handle <> 123456789) and (Handle <= Handles.Count) then
    Handles[Handle - 1] := Obj;
end;

function T_EMF_Import.GetHandle(Handle: Longword): TObject;
begin
  if (Handle <> 123456789) and (Handle <= Handles.Count) then
    Result := Handles[Handle - 1]
  else if Handle > 2147483647 then
    Result := Handles[EMF_Struct.Header.NumOfHandles + Handle -
      2147483648]
  else Result := nil;
end;

function T_EMF_Import.CurrPenWidth: TRealType;
begin
  if IsCurrPenExt then
  begin
    if (CurrExtPen.elpPenStyle and PS_TYPE_MASK) = PS_GEOMETRIC
      then
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

function T_EMF_Import.CurrLineStyle: TLineStyle;
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
    PS_INSIDEFRAME: Result := liSolid;
  else Result := liSolid;
  end;
end;

function T_EMF_Import.CurrPenColor: TColor;
begin
  if IsCurrPenExt then
    Result := CurrExtPen.elpColor
  else
    Result := CurrPen.lopnColor;
end;

function T_EMF_Import.CurrHatching: THatching;
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

function T_EMF_Import.CurrHatchColor: TColor;
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

function T_EMF_Import.CurrBrushColor: TColor;
begin
  Result := clNone;
  if (HasPath and not HasFill)
    or (CurrBrushHandle = 123456789) then Exit;
  if CurrBrush.lbStyle = BS_SOLID then
    Result := CurrBrush.lbColor
  else //if CurrBrush.lbStyle = BS_HATCHED then
    if CurrBkMode = OPAQUE then Result := CurrBkColor;
end;

procedure T_EMF_Import.SaveDC_Info;
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

procedure T_EMF_Import.RestoreDC_Info;
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
  Prim: TPrimitive2D);
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
    Prim.LineStyle := LineStyle;
  if Hatching <> haNone then
    Prim.Hatching := Hatching;
  if (Hatching <> haNone) and (HatchColor <> clNone) and
    (HatchColor <> clBlack) then
    Prim.HatchColor := HatchColor;
  if (LineColor <> clBlack) and (LineStyle <> liNone) and
    (HatchColor <> clBlack) then
    Prim.LineColor := LineColor;
  if BrushColor <> clNone then
    Prim.FillColor := BrushColor;
  if (LineWidth <> 1) and (LineStyle <> liNone) then
    Prim.LineWidth := LineWidth;
end;

procedure T_EMF_Import.AddPathAttributes(
  const ARecord: TEMF_Record;
  const Prim: TPrimitive2D);
begin
  if ARecord is TEMF_FillPath then
    WritePrimitiveAttr(liNone, clDefault, 1,
      CurrHatching, CurrHatchColor, CurrBrushColor,
      CurrBrush.lbStyle = BS_PATTERN, Prim)
  else if ARecord is TEMF_StrokeAndFillPath then
    WritePrimitiveAttr(CurrLineStyle, CurrPenWidth, CurrPenColor,
      CurrHatching, CurrHatchColor, CurrBrushColor,
      CurrBrush.lbStyle = BS_PATTERN, Prim)
  else if ARecord is TEMF_StrokePath then
    WritePrimitiveAttr(CurrLineStyle, CurrPenWidth, CurrPenColor,
      haNone, clNone, clNone,
      CurrBrush.lbStyle = BS_PATTERN, Prim)
  else if ARecord is TEMF_SelectClipPath then
    WritePrimitiveAttr(liDashed, CurrPenWidth, clLtGray,
      haNone, clNone, clNone, False, Prim);
end;

procedure T_EMF_Import.WriteRecordToLog(const ARecord:
  TEMF_Record);
begin
  if Assigned(pLogStrings) then
  begin
    pLogStrings.Add('');
    pLogStrings.Add('>>' +
      AnsiReplaceStr(ARecord.ClassName, 'TEMF_', ''));
  end;
end;

procedure T_EMF_Import.WriteToLog(const aName: string;
  const aValue: Variant);
begin
  if Assigned(pLogStrings) then
    pLogStrings.Add('  ' +
      aName + ' = ' + string(aValue));
end;

function T_EMF_Import.Add_LineToPP: TPrimitive2D;
var
  I: Integer;
  IsPolygon: Boolean;
begin
  if LineToPP.Count = 0 then Exit;
  if LineToPP.Count > 2 then
  begin
    IsPolygon := IsSamePoint2D(LineToPP[0],
      LineToPP[LineToPP.Count - 1]);
    if IsPolygon then Result := AddPolygon
    else Result := AddPolyline;
    Result.Points.Expand(LineToPP.Count - Byte(IsPolygon));
    for I := 0 to LineToPP.Count - Byte(IsPolygon) - 1 do
      Result.Points.Add(GetXY(LineToPP[I]));
  end
  else
  begin
    Result := AddLine(GetXY(LineToPP[0]), GetXY(LineToPP[1]));
  end;
  WritePrimitiveAttr(CurrLineStyle, CurrPenWidth, CurrPenColor,
    haNone, clNone, clNone, False, Result);
  LineToPP.Clear;
end;

{function T_EMF_Import.AddPrimitive(
  const Obj: TPrimitive2D): TPrimitive2D;
begin
  Add_LineToPP;
  inherited AddPrimitive(Obj);
end;}

procedure T_EMF_Import.Rect0_Execute(
  const ARecord: TEMF_Record);
begin
  if ARecord is TEMF_Ellipse then
    with (ARecord as TEMF_Ellipse).EMR_Info do
      AddEllipse(GetXY((rclBox.Left + rclBox.Right) / 2,
        (rclBox.Top + rclBox.Bottom) / 2),
        Abs(rclBox.Right - rclBox.Left) * CurrXScale / 2,
        Abs(rclBox.Bottom - rclBox.Top) * CurrXScale / 2)
  else
    with (ARecord as TEMF_Rectangle).EMR_Info do
      AddRect(GetXY(rclBox.Left, rclBox.Top),
        GetXY(rclBox.Right, rclBox.Bottom));
  WritePrimitiveAttr(CurrLineStyle, CurrPenWidth, CurrPenColor,
    CurrHatching, CurrHatchColor, CurrBrushColor,
    CurrBrush.lbStyle = BS_PATTERN, fCurrPrimitive);
end;

procedure T_EMF_Import.RoundRect_Execute(
  const ARecord: TEMF_Record);
var
  P1, P2, P3: TPoint2D;
  V1, V2: TVector2D;
begin
  with (ARecord as TEMF_RoundRect).EMR_Info.rclBox,
    (ARecord as TEMF_RoundRect).EMR_Info.szlCorner do
  begin
    P1 := GetXY(Left, Bottom);
    P2 := GetXY(Right, Top);
    P3 := GetXY(Left + CX / 2, Bottom - CY / 2);
  end;
  V1 := Vector2D(P1, P2);
  V2 := Vector2D(P1, P3);
  AddRoundRect(P1, V1.X, V1.Y, V2.X, V2.Y);
  WritePrimitiveAttr(CurrLineStyle, CurrPenWidth, CurrPenColor,
    CurrHatching, CurrHatchColor, CurrBrushColor,
    CurrBrush.lbStyle = BS_PATTERN, fCurrPrimitive);
end;

var
{$IFDEF VER140}
  Text_Metric: tagTEXTMETRIC;
{$ENDIF}
  VShift: Integer;

procedure T_EMF_Import.Arc0_Execute(
  const ARecord: TEMF_Record);
var
  Kind: TCircularKind;
  ICX, ICY: Integer;
begin
  if (ARecord is TEMF_Arc) or (ARecord is TEMF_ArcTo)
    then Kind := ci_Arc
  else if (ARecord is TEMF_Pie)
    then Kind := ci_Sector
  else Kind := ci_Segment;
  with (ARecord as TEMF_Arc0).EMR_Info do
  begin
    //rclBox: TRect; Inclusive-inclusive bounding rectangle
    ICX := (rclBox.Left + rclBox.Right) div 2;
    ICY := (rclBox.Top + rclBox.Bottom) div 2;
    AddCircular(GetXY(ICX, ICY),
      ((rclBox.Right - rclBox.Left) * CurrXScale
      + (rclBox.Bottom - rclBox.Top) * CurrYScale) / 4,
      ArcTan2(ICY - ptlStart.Y, ptlStart.X - ICX),
      ArcTan2(ICY - ptlEnd.Y, ptlEnd.X - ICX), Kind);
    if (ARecord is TEMF_ArcTo) then
    begin
      CurrXY := Point2D(ptlEnd.X, ptlEnd.Y);
    end;
  end;
  WritePrimitiveAttr(CurrLineStyle, CurrPenWidth, CurrPenColor,
    CurrHatching, CurrHatchColor, CurrBrushColor,
    CurrBrush.lbStyle = BS_PATTERN, fCurrPrimitive);
end;

procedure T_EMF_Import.StretchDIBits_Execute(
  const ARecord: TEMF_Record);
begin
  with (ARecord as TEMF_StretchDIBits).EMR_Info do
    AddRect(GetXY(rclBounds.Left, rclBounds.Top),
      GetXY(rclBounds.Right, rclBounds.Bottom));
  WritePrimitiveAttr(liNone, 1, clNone,
    haNone, clNone, CurrBrushColor, True, fCurrPrimitive);
end;

procedure T_EMF_Import.BitBlt_Execute(
  const ARecord: TEMF_Record);
begin
  with (ARecord as TEMF_BitBlt).EMR_Info do
  begin
    WriteToLog('dwRop', IntToHex(dwRop, 8));
    AddRect(GetXY(xDest, yDest),
      GetXY(xDest + cxDest, yDest + cyDest));
  end;
  WritePrimitiveAttr(liNone, 1, clNone,
    haNone, clNone, CurrBrushColor, True, fCurrPrimitive);
end;

procedure T_EMF_Import.PolyBezier_Execute(
  const ARecord: TEMF_Record);
var
  BezMod: Byte;
  BezArr: array[0..3] of TPoint2D;
  I: Integer;
  PP: TPointsSet2D;
begin
  AddBezier; //? Closed
  PP := fCurrPrimitive.Points;
  with ARecord as TEMF_PolyGen do
  begin
    BezMod := 0;
    for I := 0 to High(PointsArray) do
    begin
      BezArr[BezMod] := GetXY(PointsArray[I].X,
        PointsArray[I].Y);
      if BezMod = 3 then
      begin
        if I < 5 then PP.Add(BezArr[0]);
        if not (IsSamePoint2D(BezArr[0], BezArr[3])
          and IsSamePoint2D(BezArr[0], BezArr[1])) then
        begin
          PP.Add(BezArr[1]);
          PP.Add(BezArr[2]);
          PP.Add(BezArr[3]);
          BezArr[0] := BezArr[3];
        end;
      end;
      if I >= High(PointsArray) then
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
    CurrBrush.lbStyle = BS_PATTERN, fCurrPrimitive);
end;

procedure T_EMF_Import.Poly_Execute(
  const ARecord: TEMF_Record);
var
  I: Integer;
  PP: TPointsSet2D;
begin
  if (ARecord is TEMF_Polygon16)
    or (ARecord is TEMF_Polygon32)
    then AddPolygon
  else if (ARecord is TEMF_Polyline16)
    or (ARecord is TEMF_Polyline32)
    or (ARecord is TEMF_PolylineTo16)
    or (ARecord is TEMF_PolylineTo32)
    then AddPolyline;
  PP := fCurrPrimitive.Points;
  with ARecord as TEMF_PolyGen do
  begin
    for I := 0 to High(PointsArray) do
    begin
      PP.Add(
        GetXY(PointsArray[I].X, PointsArray[I].Y));
      if I >= High(PointsArray) then
        if ARecord is TEMF_PolylineTo16 then
          CurrXY := Point2D(PointsArray[I].X,
            PointsArray[I].Y);
    end;
    if (ARecord is TEMF_Polygon16)
      or (ARecord is TEMF_Polygon32) then
      WritePrimitiveAttr(CurrLineStyle, CurrPenWidth,
        CurrPenColor,
        CurrHatching, CurrHatchColor, CurrBrushColor,
        CurrBrush.lbStyle = BS_PATTERN, fCurrPrimitive)
    else
      WritePrimitiveAttr(CurrLineStyle, CurrPenWidth,
        CurrPenColor,
        haNone, clNone, clNone,
        CurrBrush.lbStyle = BS_PATTERN, fCurrPrimitive);
  end;
end;

procedure T_EMF_Import.PolyPoly_Execute(
  const ARecord: TEMF_Record);
var
  I, J: Integer;
  PP: TPointsSet2D;
begin
  with (ARecord as TEMF_PolyPoly16) do
    for J := 0 to PolyList.Count - 1 do
    begin
      if ARecord is TEMF_PolyPolygon16 then
        AddPolygon
      else if ARecord is TEMF_PolyPolyline16 then
        AddPolyline;
      PP := fCurrPrimitive.Points;
      for I := 0 to GetLen(J) - 1 do
        PP.Add(
          GetXY(GetPnt(J, I).X, GetPnt(J, I).Y));
      if ARecord is TEMF_PolyPolygon16 then
        WritePrimitiveAttr(CurrLineStyle, CurrPenWidth,
          CurrPenColor,
          CurrHatching, CurrHatchColor, CurrBrushColor,
          CurrBrush.lbStyle = BS_PATTERN, fCurrPrimitive)
      else
        WritePrimitiveAttr(CurrLineStyle, CurrPenWidth,
          CurrPenColor,
          haNone, clNone, clNone,
          CurrBrush.lbStyle = BS_PATTERN, fCurrPrimitive);
    end;
end;

{$IFDEF VER140}

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
{$ENDIF}

procedure T_EMF_Import.ExtTextOut_Execute(
  const ARecord: TEMF_Record);
var
  ARot, H: TRealType;
  V: TVector2D;
  CPoint: TPoint;
  P: TPoint2D;
  I: Integer;
  Prim: TText2D;
  St: string;
begin
  with (ARecord as TEMF_ExtTextOut).EMR_Info do
  begin
    if CurrBkMode = OPAQUE then
    begin
      AddRect(GetXY(rclBounds.Left, rclBounds.Top),
        GetXY(rclBounds.Right, rclBounds.Bottom));
          //TempTr := CurrWorldTr;
      if iGraphicsMode = GM_COMPATIBLE then ;
      WritePrimitiveAttr(liNone, 1, clNone,
        haNone, clNone, CurrBkColor,
        CurrBrush.lbStyle = BS_PATTERN, fCurrPrimitive);
    end;
    if (emrtext.FOptions and 2) = ETO_OPAQUE then
    begin
      AddRect(GetXY(emrtext.rcl.Left, emrtext.rcl.Top),
        GetXY(emrtext.rcl.Right, emrtext.rcl.Bottom));
      WritePrimitiveAttr(liNone, 1, clNone,
        haNone, clNone, CurrBkColor,
        CurrBrush.lbStyle = BS_PATTERN, fCurrPrimitive);
    end;
      //NodeAddXY('x', 'y', Point2D(CurrX + rclBox.Left, CurrY - rclBox.Bottom, Prim);
{$IFDEF VER140}
    GetFontMetric(CurrFont);
{$ENDIF}
    WriteToLog('emrtext.FOptions', emrtext.FOptions);
    WriteToLog('emrtext.rcl.Left', emrtext.rcl.Left);
    WriteToLog('emrtext.rcl.Top', emrtext.rcl.Top);
    WriteToLog('emrtext.rcl.Right', emrtext.rcl.Right);
    WriteToLog('emrtext.rcl.Bottom', emrtext.rcl.Bottom);
        //ETO_OPAQUE
    WriteToLog('RectLeft', rclBounds.Left);
    WriteToLog('RectTop', rclBounds.Top);
    WriteToLog('RectRight', rclBounds.Right);
    WriteToLog('RectBottom', rclBounds.Bottom);
    WriteToLog('iGraphicsMode', iGraphicsMode);
    WriteToLog('exScale', exScale);
    WriteToLog('w',
      rclBounds.Right - rclBounds.Left) { * ASingle};
    WriteToLog('eyScale', eyScale);
    WriteToLog('h0', rclBounds.Bottom - rclBounds.Top);
    WriteToLog('height', CurrFont.lfHeight);
    ARot := CurrFont.lfOrientation / 1800 * Pi;
    if iGraphicsMode = GM_COMPATIBLE then
    begin
      if (CurrFont.lfOrientation = 0) and
        (CurrFont.lfEscapement <> 0) then
        ARot := CurrFont.lfEscapement / 1800 * Pi;
    end
    else
    begin
      ARot := CurrFont.lfEscapement / 1800 * Pi;
      V.X := Cos(ARot); V.Y := Sin(ARot);
      V := TransformVector2D(V, CurrWorldTr);
      ARot := ArcTan2(-V.Y, V.X);
    end;
    if CurrFont.lfHeight < 0 then
      CurrFontHeight := Abs(CurrFont.lfHeight)
    else CurrFontHeight :=
{$IFDEF VER140}
      CurrFont.lfHeight - Text_Metric.tmInternalLeading;
{$ELSE}
      CurrFont.lfHeight;
{$ENDIF}
            //CurrFont.lfHeight / 1.2;
    if CurrFontHeight = 0 then CurrFontHeight := 12;
        //device units!
    H := Abs(CurrFontHeight * CurrWorldTr[2, 2] * CurrYScale);
          //100 / exScale * MMScale *
//        := (rclBox.Bottom - rclBox.Top){ * ASingle};
    CPoint := emrtext.ptlReference;
        {if (CPoint.X = 0) and (CPoint.Y = 0) then
        begin
          CPoint.X := rclBounds.Left;
          CPoint.Y := rclBounds.Top;
        end;}
    WriteToLog('reference_point',
      IntToStr(CPoint.X) + ',' + IntToStr(CPoint.Y));
    if (CurrTextAlignment and 24) = TA_TOP then
{$IFDEF VER140}
      VShift := Text_Metric.tmInternalLeading
{$ELSE}
      VShift := 0
{$ENDIF}
    else VShift := 0;
    if (CurrTextAlignment and 1) = TA_UPDATECP then
        //Use current position
      P := GetXY(CurrXY.X, CurrXY.Y + VShift)
    else
      P := GetXY(CPoint.X, CPoint.Y + VShift);
    if (CurrTextAlignment and 24) = TA_BASELINE then
    begin
    //? an approximate rule-of-thumb correction when text is aligned at baseline
    //P.Y := P.Y + Abs(CurrFontHeight * CurrYScale * 0.35 * CurrWorldTr[2, 2])
    end;
    WriteToLog('nChars', emrtext.NChars);
    WriteToLog('Offset_to_string', emrtext.offString);
        //if Trim((ARecord as TEMF_ExtTextOut).Str) <> '' then
    AddText(P, H, (ARecord as TEMF_ExtTextOut).Str);
    Prim := fCurrPrimitive as TText2D;
    Prim.Rot := ARot;
  //TA_LEFT = 0;  TA_RIGHT = 2;  TA_CENTER = 6;
  //TA_TOP = 0;  TA_BOTTOM = 8;  TA_BASELINE = 24;
    case CurrTextAlignment and 6 of
      TA_RIGHT: Prim.HJustification := jhRight;
      TA_CENTER: Prim.HJustification := jhCenter;
    else Prim.HJustification := jhLeft;
    end;
    case CurrTextAlignment and 24 of
          //TA_BASELINE: Prim.VJustification := jvCenter;
      TA_BASELINE: ;
      TA_BOTTOM: Prim.VJustification := jvBottom;
    else Prim.VJustification := jvTop;
    end;
    if CurrTextColor <> clBlack then
      Prim.LineColor := CurrTextColor;
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
    St := '';
    for I := 0 to LF_FACESIZE do
    begin
      if CurrFont.lfFaceName[I] = #0 then Break;
      St := St + CurrFont.lfFaceName[I];
    end;
    Prim.Font.Name := Trim(St);
    if CurrFont.lfWeight >= FW_BOLD then
      Prim.Font.Style := Prim.Font.Style + [fsBold];
    if CurrFont.lfItalic = 1 then
      Prim.Font.Style := Prim.Font.Style + [fsItalic];
    Prim.Font.Charset := CurrFont.lfCharSet;
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
end;

procedure T_EMF_Import.PathBounds_Poly(
  const ARecord: TEMF_Record);
var
  PP: TPointsSet2D;
  FirstMoveTo: Boolean;
  I: Integer;
begin
  FirstMoveTo := True;
  PathArr[PathArrPos] := Point2D(-1234, -1234);
  PP := TPointsSet2D.Create(PathArrPos);
  try
    for I := 0 to PathArrPos do
    begin
      if PathArr[I].X <> -1234
        then PP.Add(PathArr[I])
      else
      begin
        if not FirstMoveTo and (PP.Count > 0) then
        begin
          if IsSamePoint2D(CurrXY, PathArr[I - 1])
            then AddPolygon
          else AddPolyline;
          if I < PathArrPos then CurrXY := PathArr[I + 1];
          fCurrPrimitive.Points.AppendPoints(PP);
          PP.Clear;
          AddPathAttributes(ARecord, fCurrPrimitive);
        end;
        FirstMoveTo := False;
      end;
    end;
  finally
    PP.Free;
  end;
end;

procedure T_EMF_Import.PathBounds_Bezier(
  const ARecord: TEMF_Record);
var
  PP: TPointsSet2D;
  FirstMoveTo: Boolean;
  I: Integer;
  BezMod: Byte;
  BezArr: array[0..3] of TPoint2D;
begin
  FirstMoveTo := True;
  PathArr[PathArrPos] := Point2D(-1234, -1234);
  PP := TPointsSet2D.Create(PathArrPos);
  try
    for I := 0 to PathArrPos do
    begin
      if PathArr[I].X <> -1234 then
      begin
        BezArr[BezMod] := PathArr[I];
        if BezMod = 0 then PP.Add(PathArr[I]);
        if BezMod = 3 then
        begin
          if not (IsSamePoint2D(BezArr[0], BezArr[3])
            and IsSamePoint2D(BezArr[0], BezArr[1])) then
          begin
                //for J := 0 to 8 do
                //  PP.Add(BezierPoint(BezArr[0], BezArr[1],
                //    BezArr[2], BezArr[3], {0.03 + J / 8 * 0.97} BezFract[J]));
            PP.Add(BezArr[1]);
            PP.Add(BezArr[2]);
            PP.Add(BezArr[3]);
            BezArr[0] := BezArr[3];
          end;
        end;
        BezMod := Succ(BezMod mod 3);
      end
      else
      begin
        if not FirstMoveTo and (PP.Count > 0) then
        begin
          if IsSamePoint2D(CurrXY, PathArr[I - 1])
            then AddClosedBezier
          else AddBezier;
          if I < PathArrPos then CurrXY := PathArr[I + 1];
          fCurrPrimitive.Points.AppendPoints(PP);
          PP.Clear;
          AddPathAttributes(ARecord, fCurrPrimitive);
        end;
        FirstMoveTo := False;
        BezMod := 0;
      end;
    end;
  finally
    PP.Free;
  end;
end;

procedure T_EMF_Import.PathBounds_Execute(
  const ARecord: TEMF_Record);
begin
  if not UseBezier then PathBounds_Poly(ARecord)
  else PathBounds_Bezier(ARecord);
end;

procedure T_EMF_Import.SetExtEx_Execute(
  const ARecord: TEMF_Record);
begin
  with (ARecord as TEMF_SetExtEx).EMR_Info do
  begin
    WriteToLog('cx', szlExtent.CX);
    WriteToLog('cy', szlExtent.CY);
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
end;

procedure T_EMF_Import.SetOrgEx_Execute(
  const ARecord: TEMF_Record);
begin
  with (ARecord as TEMF_SetOrgEx).EMR_Info do
  begin
    WriteToLog('x', ptlOrigin.X);
    WriteToLog('y', ptlOrigin.Y);
    if ARecord is TEMF_SetViewportOrgEx
      then CurrViewportOrigin := ptlOrigin
    else if ARecord is TEMF_SetWindowOrgEx
      then CurrWindowOrigin := ptlOrigin;
  end;
  SetXYScale;
end;

procedure T_EMF_Import.ScaleExtEx_Execute(
  const ARecord: TEMF_Record);
begin
  with (ARecord as TEMF_ScaleExtEx).EMR_Info do
  begin
    WriteToLog('xNum', xNum);
    WriteToLog('xDenom', xDenom);
    WriteToLog('yNum', yNum);
    WriteToLog('yDenom', yDenom);
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
end;

procedure T_EMF_Import.BeginPath_Execute(
  const ARecord: TEMF_Record);
begin
  HasPath := True;
  HasStroke := (ARecord as TEMF_BeginPath).HasStroke;
  HasFill := (ARecord as TEMF_BeginPath).HasFill;
  HasClip := (ARecord as TEMF_BeginPath).HasClip;
  SetLength(PathArr, (ARecord as TEMF_BeginPath).N + 1);
  PathArrPos := 0;
  UseBezier := (ARecord as TEMF_BeginPath).UseBezier;
end;

procedure T_EMF_Import.EndPath_Execute(
  const ARecord: TEMF_Record);
begin
  HasPath := False
end;

procedure T_EMF_Import.MoveToEx_Execute(
  const ARecord: TEMF_Record);
begin
  with (ARecord as TEMF_MoveToEx).EMR_Info do
    if (CurrXY.X <> ptl.X) or (CurrXY.Y <> ptl.Y)
          // or (MoveToXY.X <> ptl.X) or (MoveToXY.Y <> ptl.Y)
    then
    begin
      WriteToLog('ptl.X', ptl.X);
      WriteToLog('ptl.Y', ptl.Y);
      CurrXY := PointToPoint2D(ptl);
      MoveToXY := CurrXY;
    end;
end;

procedure T_EMF_Import.LineTo_Execute(
  const ARecord: TEMF_Record);
begin
  with (ARecord as TEMF_LineTo).EMR_Info do
  begin
    if LineToPP.Count = 0 then
      LineToPP.Add(CurrXY);
    CurrXY := PointToPoint2D(ptl);
    LineToPP.Add(CurrXY);
  end;
end;

procedure T_EMF_Import.ObjectHandle_Execute(
  const ARecord: TEMF_Record);
var
  TmpObj: TObject;
begin
//  Add_LineToPP;
  with (ARecord as TEMF_ObjectHandle).EMR_Info do
  begin
        //ihObject: DWORD;    Object handle index
    WriteToLog('Handle', IntToStr(ihObject));
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
      if CurrFontHandle = ihObject then
        CurrFontHandle := 123456789
      else if CurrPenHandle = ihObject then
        CurrPenHandle := 123456789
      else if CurrBrushHandle = ihObject then
        CurrBrushHandle := 123456789;
      SetHandle(ihObject, nil);
    end;
  end
end;

procedure T_EMF_Import.SaveDC_Execute(
  const ARecord: TEMF_Record);
begin
  SaveDC_Info;
end;

procedure T_EMF_Import.RestoreDC_Execute(
  const ARecord: TEMF_Record);
begin
  WriteToLog('iRelative',
    (ARecord as TEMF_RestoreDC).EMR_Info.iRelative);
  RestoreDC_Info;
end;

procedure T_EMF_Import.ExtCreateFontIndirectW_Execute(
  const ARecord: TEMF_Record);
var
  TmpObj: TFontObject;
begin
  with (ARecord as TEMF_ExtCreateFontIndirectW).EMR_Info do
  begin
    WriteToLog('Handle', ihFont);
    TmpObj := TFontObject.Create;
    SetHandle(ihFont, TmpObj);
    (TmpObj as TFontObject).Font := elfw.elfLogFont;
    with elfw.elfLogFont do
    begin
      WriteToLog('Height', lfHeight);
      WriteToLog('Width', lfWidth);
      WriteToLog('Escapement', lfEscapement);
      WriteToLog('Orientation', lfOrientation);
      WriteToLog('Weight', lfWeight);
      WriteToLog('Italic', lfItalic);
      WriteToLog('Underline', lfUnderline);
      WriteToLog('StrikeOut', lfStrikeOut);
      WriteToLog('CharSet', lfCharSet);
      WriteToLog('OutPrecision', lfOutPrecision);
      WriteToLog('ClipPrecision', lfClipPrecision);
      WriteToLog('Quality', lfQuality);
      WriteToLog('PitchAndFamily', lfPitchAndFamily);
    end;
  end
end;

procedure T_EMF_Import.SetMiterLimit_Execute(
  const ARecord: TEMF_Record);
begin
  with (ARecord as TEMF_SetMiterLimit).EMR_Info do
  begin
    WriteToLog('MiterLimit', eMiterLimit);
    if eMiterLimit < 1 then eMiterLimit := 1;
    fDrawing2D.MiterLimit := eMiterLimit;
  end;
end;

procedure T_EMF_Import.CreatePen_Execute(
  const ARecord: TEMF_Record);
var
  TmpObj: TPenObject;
begin
  with (ARecord as TEMF_CreatePen).EMR_Info do
  begin
    WriteToLog('Handle', ihPen);
    TmpObj := TPenObject.Create;
    SetHandle(ihPen, TmpObj);
    (TmpObj as TPenObject).Pen := lopn;
    with lopn do
    begin
      WriteToLog('Style', lopnStyle);
      WriteToLog('WidthX', lopnWidth.X);
      WriteToLog('WidthY', lopnWidth.Y);
      WriteToLog('Color', ColorToString(lopnColor));
    end;
  end
end;

procedure T_EMF_Import.ExtCreatePen_Execute(
  const ARecord: TEMF_Record);
var
  TmpObj: TExtPenObject;
begin
  with (ARecord as TEMF_ExtCreatePen).EMR_Info do
  begin
    WriteToLog('Handle', ihPen);
    TmpObj := TExtPenObject.Create;
    SetHandle(ihPen, TmpObj);
    (TmpObj as TExtPenObject).ExtPen := elp;
    with elp do
    begin
      WriteToLog('offBmi', offBmi);
      WriteToLog('cbBmi', cbBmi);
      WriteToLog('offBits', offBits);
      WriteToLog('cbBits', cbBits);
      WriteToLog('Style', elpPenStyle);
      WriteToLog('Width', elpWidth);
      WriteToLog('BrushStyle', elpBrushStyle);
      WriteToLog('Color', ColorToString(elpColor));
      WriteToLog('Hatch', elpHatch);
        //elpStyleEntry: array[0..0] of DWORD;
    end;
  end
end;

procedure T_EMF_Import.CreateBrushIndirect_Execute(
  const ARecord: TEMF_Record);
var
  TmpObj: TBrushObject;
begin
  with (ARecord as TEMF_CreateBrushIndirect).EMR_Info do
  begin
    TmpObj := TBrushObject.Create;
    SetHandle(ihBrush, TmpObj);
    (TmpObj as TBrushObject).Brush := LB;
    with LB do
    begin
      WriteToLog('Handle', ihBrush);
      WriteToLog('Style', lbStyle);
      WriteToLog('Color', ColorToString(lbColor));
      WriteToLog('Hatch', lbHatch);
    end;
  end
end;

procedure T_EMF_Import.CreateDIBPatternBrushPt_Execute(
  const ARecord: TEMF_Record);
var
  TmpObj: TBrushObject;
begin
  with (ARecord as TEMF_CreateDIBPatternBrushPt).EMR_Info do
  begin
    TmpObj := TBrushObject.Create;
    SetHandle(ihBrush, TmpObj);
    (TmpObj as TBrushObject).Brush.lbStyle := BS_PATTERN;
    (TmpObj as TBrushObject).Brush.lbColor := clNone;
        //MainForm.Image1.Picture.Assign((ARecord as TEMF_CreateDIBPatternBrushPt).BMP);
        //(TmpObj as TBrushObject).Brush.lbColor :=          (ARecord as TEMF_CreateDIBPatternBrushPt).BMP.Canvas.Pixels[1, 1];
    WriteToLog('Handle', ihBrush);
  end
end;

procedure T_EMF_Import.CreateMonoBrush_Execute(
  const ARecord: TEMF_Record);
var
  TmpObj: TBrushObject;
begin
  with (ARecord as TEMF_CreateMonoBrush).EMR_Info do
  begin
    TmpObj := TBrushObject.Create;
    SetHandle(ihBrush, TmpObj);
        //MainForm.Image1.Picture.Assign((ARecord as TEMF_CreateMonoBrush).BMP);
    (TmpObj as TBrushObject).Brush.lbStyle := BS_PATTERN;
    (TmpObj as TBrushObject).Brush.lbColor := clNone;
          //clSkyBlue;
    WriteToLog('Handle', ihBrush);
  end
end;

procedure T_EMF_Import.SetBkMode_Execute(
  const ARecord: TEMF_Record);
begin
  with (ARecord as TEMF_SetBkMode).EMR_Info do
  begin
    case iMode of
      0: WriteToLog('Mode', 'OPAQUE');
        //OPAQUE Background is filled with the current background color before the text, hatched brush, or pen is drawn.
      1: WriteToLog('Handle', 'TRANSPARENT');
        //TRANSPARENT Background remains untouched.
    else
      WriteToLog('Handle', iMode);
    end;
    CurrBkMode := iMode;
  end;
end;

procedure T_EMF_Import.SetMapMode_Execute(
  const ARecord: TEMF_Record);
const
  MapModes: array[MM_TEXT..MM_ANISOTROPIC] of string =
  ('MM_TEXT', 'MM_LOMETRIC', 'MM_HIMETRIC',
    'MM_LOENGLISH', 'MM_HIENGLISH', 'MM_TWIPS',
    'MM_ISOTROPIC', 'MM_ANISOTROPIC');
begin
  with (ARecord as TEMF_SetMapMode).EMR_Info do
  begin
    if iMode <= MM_ANISOTROPIC then
      WriteToLog('Handle', MapModes[iMode])
    else WriteToLog('Handle', iMode);
    CurrMapMode := iMode;
    SetXYScale;
  end;
end;

procedure T_EMF_Import.SetBkColor_Execute(
  const ARecord: TEMF_Record);
begin
  with (ARecord as TEMF_SetBkColor).EMR_Info do
  begin
    WriteToLog('Color', ColorToString(crColor));
    CurrBkColor := crColor;
  end;
end;

procedure T_EMF_Import.SetTextColor_Execute(
  const ARecord: TEMF_Record);
begin
  CurrTextColor := (ARecord as
    TEMF_SetTextColor).EMR_Info.crColor;
  WriteToLog('Color', ColorToString(CurrTextColor));
end;

procedure T_EMF_Import.SetTextAlign_Execute(
  const ARecord: TEMF_Record);
begin
  WriteToLog('Align', CurrTextAlignment);
  CurrTextAlignment := (ARecord as
    TEMF_SetTextAlign).EMR_Info.iMode;
end;

procedure T_EMF_Import.SetWorldTransform_Execute(
  const ARecord: TEMF_Record);
begin
  CurrWorldTr := XFormToTransf2D((ARecord as
    TEMF_SetWorldTransform).EMR_Info.XFORM);
end;

procedure T_EMF_Import.ModifyWorldTransform_Execute(
  const ARecord: TEMF_Record);
var
  TempTr: TTransf2D;
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
end;

procedure T_EMF_Import.ArcTo_Path_Execute(
  const ARecord: TEMF_Record);
var
  CP, P0, P1, P2, P3: TPoint2D;
  RX, RY, A1, A2, SA, EA: TRealType;
  I: Integer;
  procedure AddP(const P: TPoint2D);
  begin
    AddPathPoint0(
      GetXY(Point2D(CP.X + P.X * RX, CP.Y + P.Y * RY)));
  end;
begin
  with (ARecord as TEMF_ArcTo).EMR_Info do
  begin
    CP := Point2D((rclBox.Left + rclBox.Right) / 2,
      (rclBox.Top + rclBox.Bottom) / 2);
    RX := Abs(rclBox.Right - rclBox.Left) / 2;
    RY := Abs(rclBox.Bottom - rclBox.Top) / 2;
    A1 := ArcTan2((ptlStart.Y - CP.Y) * RX / RY, ptlStart.X -
      CP.X);
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

procedure T_EMF_Import.PolyBezierTo_Path_Execute(
  const ARecord: TEMF_Record);
var
  I: Integer;
begin
  with ARecord as TEMF_PolyGen do
    for I := 0 to High(PointsArray) do
      AddPathPoint0(GetXY(PointsArray[I].X,
        PointsArray[I].Y));
end;

procedure T_EMF_Import.PolylineTo16_Path_Execute(
  const ARecord: TEMF_Record);
var
  I: Integer;
begin
  with ARecord as TEMF_PolylineTo16 do
    for I := 0 to High(PointsArray) do
      AddPathPoint(GetXY(PointsArray[I].X,
        PointsArray[I].Y));
end;

procedure T_EMF_Import.PolyDraw16_Path_Execute(
  const ARecord: TEMF_Record);
var
  I: Integer;
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
end;

procedure T_EMF_Import.MoveToEx_Path_Execute(
  const ARecord: TEMF_Record);
begin
  with (ARecord as TEMF_MoveToEx).EMR_Info do
  begin
    AddPathPoint0(Point2D(-1234, -1234));
    AddPathPoint0(GetXY(ptl.X, ptl.Y));
    MoveToXY := Point2D(ptl.X, ptl.Y);
  end;
end;

procedure T_EMF_Import.CloseFigure_Path_Execute(
  const ARecord: TEMF_Record);
begin
  AddPathPoint(GetXY(MoveToXY));
end;

procedure T_EMF_Import.LineTo_Path_Execute(
  const ARecord: TEMF_Record);
begin
  with (ARecord as TEMF_LineTo).EMR_Info do
    AddPathPoint(GetXY(ptl.X, ptl.Y));
end;

procedure T_EMF_Import.ParseEmfRecord(const Index: Integer);
var
  ARecord: TEMF_Record;
  iType: Byte;
begin
  if Index mod 100 = 0 then
    ShowProgress(Index / EMF_Struct.Count);
  ARecord := EMF_Struct[Index] as TEMF_Record;
  WriteRecordToLog(ARecord);
  iType := ARecord.EMR_Info0.iType;
  if HasPath and (iType <= 97) and
    Assigned(RecordProcs_Path[iType]) then
    RecordProcs_Path[iType](ARecord)
  else if (iType <= 97) and
    Assigned(RecordProcs[iType]) then
  begin
    if iType in [2..8, 14, 27, 34, 37, 41..48, 55, 56,
      59, 85..92] //58 SETMITERLIMIT,
      then Add_LineToPP;
    RecordProcs[iType](ARecord);
  end;
end;

procedure T_EMF_Import.ParseEmf;
var
  RecIndex, I: Integer;
  TmpObj: TObject;
begin
  if Assigned(pLogStrings) then pLogStrings.Clear;
  LineToPP := TPointsSet2D.Create(10);
  Handles := TObjectList.Create;
  Handles.OwnsObjects := True;
  DC_InfoList := TObjectList.Create;
  DC_InfoList.OwnsObjects := True;
  Handles.Capacity :=
    EMF_Struct.Header.NumOfHandles + STOCK_LAST + 1;
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
  SetHandle(EMF_Struct.Header.NumOfHandles + WHITE_BRUSH + 1,
    TmpObj);
  TmpObj := TBrushObject.Create;
  (TmpObj as TBrushObject).Brush.lbColor := clLtGray;
  SetHandle(EMF_Struct.Header.NumOfHandles + LTGRAY_BRUSH + 1,
    TmpObj);
  TmpObj := TBrushObject.Create;
  (TmpObj as TBrushObject).Brush.lbColor := clGray;
  SetHandle(EMF_Struct.Header.NumOfHandles + GRAY_BRUSH + 1,
    TmpObj);
  TmpObj := TBrushObject.Create;
  (TmpObj as TBrushObject).Brush.lbColor := clDkGray;
  SetHandle(EMF_Struct.Header.NumOfHandles + DKGRAY_BRUSH + 1,
    TmpObj);
  TmpObj := TBrushObject.Create;
  (TmpObj as TBrushObject).Brush.lbColor := clBlack;
  SetHandle(EMF_Struct.Header.NumOfHandles + BLACK_BRUSH + 1,
    TmpObj);
  TmpObj := TBrushObject.Create;
  (TmpObj as TBrushObject).Brush.lbColor := clNone;
  SetHandle(EMF_Struct.Header.NumOfHandles + NULL_BRUSH + 1,
    TmpObj);
  TmpObj := TBrushObject.Create;
  (TmpObj as TBrushObject).Brush.lbColor := clWhite;
  SetHandle(EMF_Struct.Header.NumOfHandles + DC_BRUSH + 1, TmpObj);
  TmpObj := TExtPenObject.Create;
  (TmpObj as TExtPenObject).ExtPen.elpColor := clWhite;
  SetHandle(EMF_Struct.Header.NumOfHandles + WHITE_PEN + 1,
    TmpObj);
  TmpObj := TExtPenObject.Create;
  (TmpObj as TExtPenObject).ExtPen.elpColor := clBlack;
  SetHandle(EMF_Struct.Header.NumOfHandles + BLACK_PEN + 1,
    TmpObj);
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
  with EMF_Struct.Header do
  begin
    WriteToLog('l', BoundsLeft);
    WriteToLog('t', BoundsTop);
    WriteToLog('r', BoundsRight);
    WriteToLog('b', BoundsBottom);
    WriteToLog('WidthDevPixels', WidthDevPixels);
    WriteToLog('HeightDevPixels', HeightDevPixels);
    WriteToLog('WidthDevMM', WidthDevMM);
    WriteToLog('HeightDevMM', HeightDevMM);
    fDrawing2D.PicUnitLength := PicUnitLength_Default;
    MMScale := WidthDevMM / WidthDevPixels;
    fDrawing2D.PicScale := 1;
    fDrawing2D.LineWidthBase := MMScale;
      //ShowMessage(FloatToStr(Scale));
  end;
  CurrViewportOrigin.X := 0;
  CurrViewportOrigin.Y := 0;
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
    ParseEmfRecord(RecIndex);
  Add_LineToPP;
  LineToPP.Free;
  Handles.Free;
  DC_InfoList.Free;
  fDrawing2D.AddList(fLst);
  fLst.Clear;
end;

end.

