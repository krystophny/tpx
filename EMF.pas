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
  end;

  T_EMF_Loader = class
    EMF_Struct: T_EMF_Structure;
    constructor Create;
    destructor Destroy; override;
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

uses MainUnit, InOut, CADSys4, CS4BaseTypes, CS4Shapes, ColorEtc;


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


procedure T_EMF_Loader.LoadFromFile(const FileName: string);
var
  Stream: TStream;
  //Stream: TMemoryStream; TFileStream;
  MF: TMetaFile;
begin
  if LowerCase(ExtractFileExt(FileName)) = '.emf' then
  begin
    Stream := TFileStream.Create(FileName, fmOpenRead);
    try
      EMF_Struct.LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  end
  else
  begin
    MF := TMetaFile.Create;
    Stream := TMemoryStream.Create;
    try
      MF.LoadFromFile(FileName);
      MF.Enhanced := True;
      MF.SaveToStream(Stream);
      Stream.Position := 0;
      EMF_Struct.LoadFromStream(Stream);
    finally
      Stream.Free;
      MF.Free;
    end;
  end;
end;


function WideStringToString(WSt: WideString): string;
var
  I, N: Integer;
  WSt2: WideString;
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
  RecIndex: Integer;
  EMR_Info: TEMR;
  ARecord: TEMF_Record;
  LastBeginPath: Integer;
var
  APosition: Int64;
begin
  if pLogStrings <> nil then pLogStrings.Clear;
  Stream.ReadBuffer(Header, SizeOf(Header));
  Stream.Seek(Header.RecordSize, soFromBeginning);
  LastBeginPath := -1;
  for RecIndex := 1 to Header.NumOfRecords - 1 do
  begin
    APosition := Stream.Position;
    Stream.ReadBuffer(EMR_Info, SizeOf(EMR_Info));
    if pLogStrings <> nil then
      pLogStrings.Add(EMF_Records[EMR_Info.iType]);
    Stream.Seek(-SizeOf(EMR_Info), soFromCurrent);
    if (EMR_Info.iType < 1) or (EMR_Info.iType > 97) then
    begin
        //ShowMessage(IntToStr(EMR_Info.iType));
      Stream.Position := APosition;
      Stream.Seek(EMR_Info.nSize, soFromCurrent);
      Continue;
    end;
    ARecord := EMF_RecordsClasses[EMR_Info.iType].Create;
    Add(ARecord);
    ARecord.ReadFromStream(Stream);
    Stream.Position := APosition;
    Stream.Seek(EMR_Info.nSize, soFromCurrent);
    if ARecord is TEMF_BeginPath then
    begin
      (ARecord as TEMF_BeginPath).HasStroke := False;
      (ARecord as TEMF_BeginPath).HasFill := False;
      LastBeginPath := RecIndex - 1
    end
    else if (ARecord is TEMF_PathBounds)
      and (LastBeginPath >= 0) then
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
  end;

  TPathPrevKind = (ppk_None, ppk_LineTo, ppk_PolylineTo, ppk_PolybezierTo);

     {*---- T_EMF_Loader ----*}

procedure T_EMF_Loader.FillXML(XML: TXMLDDocument);
var
  RecIndex, I, J, SizeX, SizeY: Integer;
  ARecord: TEMF_Record;
  ViewportOrigin, WindowOrigin: TPoint;
  ViewportExtent, WindowExtent: TSize;
  CPoint: TPoint;
        //ptlOrigin: TPoint;
    //szlExtent: TSize;
  NChars, TextAlignment: DWORD;
  St, AString: string;
  XMLNode: TXMLDElement;
  PathPrevXMLNode: TXMLDElement; //inside path shows previous element
  PathPrevKind: TPathPrevKind; //kind of previous path element
  PathPrevIndex: Integer; //index of previous path element
  CurrFontHandle, CurrPenHandle, CurrBrushHandle: Integer;
  CurrFontHeight: Longint;
  CurrFont: TLogFontW;
  IsCurrPenExt: Boolean;
  CurrPen: TLogPen;
  CurrExtPen: TExtLogPen;
  CurrBrush: TLogBrush;
  BkMode: Longint;
  BkColor: TColor;
  BB, CurrX, CurrY, MoveToX, MoveToY: Double;
  Handles, DC_InfoList: TObjectList;
  HasPath, HasStroke, HasFill: Boolean;
  BezArr: array[0..3] of TPoint2D;
  P: TPoint2D;
  BezMod: Byte;
  Scale: TRealType;
const
  BezFract: array[0..8] of TRealType =
  (0.01, 0.03, 0.12, 0.31, 0.5, 0.69, 0.88, 0.97, 0.99);
  procedure SetHandle(Handle: Integer; Obj: TObject);
  begin
    if (Handle >= 1) and (Handle <= Handles.Count) then
      Handles[Handle - 1] := Obj;
  end;
  function GetHandle(Handle: Integer): TObject;
  begin
    if (Handle >= 1) and (Handle <= Handles.Count) then
      Result := Handles[Handle - 1]
    else Result := nil;
  end;
  function CurrPenWidth: Longint;
  begin
    if IsCurrPenExt then
      Result := CurrExtPen.elpWidth
    else
      Result := Max(CurrPen.lopnWidth.X, CurrPen.lopnWidth.Y);
  end;
  function CurrPenStyle: Longint;
  begin
    if IsCurrPenExt then
      Result := CurrExtPen.elpPenStyle
    else
      Result := CurrPen.lopnStyle;
  end;
  function CurrLineKind: TLineKind;
  begin
    if (HasPath and not HasStroke)
      or (CurrPenHandle < 0) then
    begin
      Result := liNone;
      Exit;
    end;
    case CurrPenStyle of
      PS_SOLID: Result := liThin;
      PS_DASH: Result := liDashed;
      PS_DOT: Result := liDotted;
      PS_DASHDOT: Result := liDashed;
      PS_DASHDOTDOT: Result := liDashed;
      PS_NULL: Result := liNone;
      PS_INSIDEFRAME: Result := liThick; //??
    else Result := liThick;
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
      or (CurrBrushHandle < 0) then
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
      BS_PATTERN: Result := haDiagCross;
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
  function CurrBrushColor: TColor;
  begin
    Result := clNone;
    if (HasPath and not HasFill)
      or (CurrBrushHandle < 0) then Exit;
    if CurrBrush.lbStyle = BS_SOLID then
      Result := CurrBrush.lbColor
    else //if CurrBrush.lbStyle = BS_HATCHED then
      if BkMode = OPAQUE then Result := BkColor;
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
    DC_InfoList.Delete(DC_InfoList.Count - 1);
    {CurrPenHandle := 0;
    CurrBrushHandle := 0;}
  end;
  procedure WritePrimitiveAttr(LineKind: TLineKind;
    LineColor: TColor; Hatching: THatching;
    BrushColor: TColor;
    XMLNode: TXMLDElement);
  begin
    if (LineKind = liNone) and (Hatching = haNone)
      and (BrushColor = clNone) then
    begin
      LineKind := liDashed;
      if LineColor = clWhite then LineColor := clSilver;
    end;
    // then
    if LineKind <> liThick then
      XMLNode.AttributeValue['li'] := LineKind;
    if Hatching <> haNone then
      XMLNode.AttributeValue['ha'] := Hatching;
    if LineColor <> clBlack then
      XMLNode.AttributeValue['lc'] := ColorToHtml(LineColor);
    if BrushColor <> clNone then
      XMLNode.AttributeValue['fill'] := ColorToHtml(BrushColor);
  end;
  function AInfoRec: TXMLDElement;
  begin
    Result := XML.DocumentElement.AddElement(
      //'rec' + IntToStr(RecIndex) +
      //'fu' +      IntToStr(ARecInfo.Fu));
      AnsiReplaceStr(ARecord.ClassName, 'TEMF_', ''));
  end;
  function AddItem(const ID: string): TXMLDElement;
  begin
    Result := XML.DocumentElement.AddElement(ID);
  end;
  function GetXScale: Single;
  begin
    if (ViewportExtent.CX = 0) or (WindowExtent.CX = 0)
      then Result := Scale
    else Result := ViewportExtent.CX / WindowExtent.CX * Scale;
  end;
  function GetYScale: Single;
  begin
    if (ViewportExtent.CY = 0) or (WindowExtent.CY = 0)
      then Result := Scale
    else Result := ViewportExtent.CY / WindowExtent.CY * Scale;
  end;
  function GetX(X: Single): Single;
  begin
    Result := (ViewportOrigin.X * Scale +
      (X - WindowOrigin.X) * GetXScale);
  end;
  function GetY(Y: Single): Single;
  begin
    Result := BB * Scale - (ViewportOrigin.Y * Scale +
      (Y - WindowOrigin.Y) * GetYScale);
  end;
  procedure AddPoint2D(P: TPoint2D);
  begin
    St := St + FloatToStr(P.X) + ',' + FloatToStr(P.Y) + ' ';
  end;
begin
  if XML = nil then Exit;
  XML.LoadXML('<TpX/>');
  Handles := TObjectList.Create;
  Handles.OwnsObjects := True;
  DC_InfoList := TObjectList.Create;
  DC_InfoList.OwnsObjects := True;
  Handles.Capacity := EMF_Struct.Header.NumOfHandles;
  CurrPenHandle := -1;
  CurrBrushHandle := -1;
  CurrBrush.lbColor := clWhite;
  BkColor := clWhite;
  BkMode := TRANSPARENT;
  CurrPen.lopnColor := clWhite;
  for I := 1 to EMF_Struct.Header.NumOfHandles do
    Handles.Add(nil);
  with XML.DocumentElement do
  begin
    AttributeValue['v'] := 1;
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
      Scale := WidthDevMM / WidthDevPixels;
      AttributeValue['PicScale'] := 1;
      AttributeValue['LineWidth'] := Scale;
      //ShowMessage(FloatToStr(Scale));
    end;
  end;
  ViewportOrigin := Point(0, 0);
  ViewportExtent.CX := 0;
  ViewportExtent.CY := 0;
  WindowOrigin := ViewportOrigin;
  WindowExtent := ViewportExtent;
  BB := EMF_Struct.Header.BoundsBottom;
  CurrX := GetX(0);
  CurrY := GetY(0);
  HasPath := False;
  for RecIndex := 0 to EMF_Struct.Count - 1 do
  begin
    if RecIndex mod 100 = 0 then
    begin
      MainForm.ProgressBar1.Position :=
        Round(RecIndex / EMF_Struct.Count * 100);
      Application.ProcessMessages;
    end;
    ARecord := EMF_Struct[RecIndex] as TEMF_Record;
    if (ARecord is TEMF_SetExtEx) then
    begin
      XMLNode := AInfoRec;
      with (ARecord as TEMF_SetExtEx).EMR_Info do
      begin
        XMLNode.AttributeValue['cx'] := szlExtent.CX;
        XMLNode.AttributeValue['cy'] := szlExtent.CY;
        if ARecord is TEMF_SetViewportExtEx
          then ViewportExtent := szlExtent
        else WindowExtent := szlExtent;
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
          then ViewportOrigin := ptlOrigin
        else if ARecord is TEMF_SetWindowOrgEx
          then WindowOrigin := ptlOrigin;
      end
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
          ViewportExtent.CX :=
            (ViewportExtent.CX * xNum) div xDenom;
          ViewportExtent.CY :=
            (ViewportExtent.CY * yNum) div yDenom;
        end
        else
        begin
          WindowExtent.CX :=
            (WindowExtent.CX * xNum) div xDenom;
          WindowExtent.CY :=
            (WindowExtent.CY * yNum) div yDenom;
        end
      end;
    end
    else if (ARecord is TEMF_BeginPath) then
    begin
      HasPath := True;
      HasStroke := (ARecord as TEMF_BeginPath).HasStroke;
      HasFill := (ARecord as TEMF_BeginPath).HasFill;
      PathPrevKind := ppk_None;
    end
    else if (ARecord is TEMF_EndPath) then
      HasPath := False
    else if (ARecord is TEMF_Rect0) then
    begin
      if ARecord is TEMF_Ellipse then
        with (ARecord as TEMF_Ellipse).EMR_Info do
        begin
          XMLNode := AddItem('ellipse');
          XMLNode.AttributeValue['x']
            := GetX((rclBox.Left + rclBox.Right) / 2);
          XMLNode.AttributeValue['y']
            := GetY((rclBox.Top + rclBox.Bottom) / 2);
          XMLNode.AttributeValue['dx']
            := Abs(rclBox.Right - rclBox.Left) * GetXScale;
          XMLNode.AttributeValue['dy']
            := Abs(rclBox.Bottom - rclBox.Top) * GetXScale;
        end
      else
        {if (CurrLineKind <> liNone)
          or (CurrHatching <> haNone) then}
        with (ARecord as TEMF_Rectangle).EMR_Info do
        begin
          XMLNode := AddItem('rect');
          XMLNode.AttributeValue['x1'] := GetX(rclBox.Left);
          XMLNode.AttributeValue['y1'] := GetY(rclBox.Top);
          XMLNode.AttributeValue['x2'] := GetX(rclBox.Right);
          XMLNode.AttributeValue['y2'] := GetY(rclBox.Bottom);
        end;
      WritePrimitiveAttr(CurrLineKind, CurrPenColor, CurrHatching,
        CurrBrushColor, XMLNode);
    end
    // else if EMR_ROUNDRECT	44
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
        XMLNode.AttributeValue['x'] := GetX(CPoint.X);
        XMLNode.AttributeValue['y'] := GetY(CPoint.Y);
        XMLNode.AttributeValue['d'] :=
          ((rclBox.Right - rclBox.Left) * GetXScale
          + (rclBox.Bottom - rclBox.Top) * GetYScale) / 2;
        XMLNode.AttributeValue['a1'] :=
          ArcTan2(CPoint.Y - ptlStart.Y, ptlStart.X - CPoint.X);
        XMLNode.AttributeValue['a2'] :=
          ArcTan2(CPoint.Y - ptlEnd.Y, ptlEnd.X - CPoint.X);
        if (ARecord is TEMF_ArcTo) then
        begin
          CurrX := GetX(ptlEnd.X);
          CurrY := GetY(ptlEnd.Y);
        end;
      end;
      WritePrimitiveAttr(CurrLineKind, CurrPenColor, CurrHatching,
        CurrBrushColor, XMLNode);
    end
    else if ARecord is TEMF_StretchDIBits then
    begin
      XMLNode := AddItem('rect');
      with (ARecord as TEMF_StretchDIBits).EMR_Info do
      begin
        XMLNode.AttributeValue['x1'] := GetX(rclBounds.Left);
        XMLNode.AttributeValue['y1'] := GetY(rclBounds.Top);
        XMLNode.AttributeValue['x2'] := GetX(rclBounds.Right);
        XMLNode.AttributeValue['y2'] := GetY(rclBounds.Bottom);
      end;
      WritePrimitiveAttr(CurrLineKind, CurrPenColor, haNone,
        CurrBrushColor, XMLNode);
    end
    else if ARecord is TEMF_BitBlt then
    begin
      XMLNode := AddItem('rect');
      with (ARecord as TEMF_BitBlt).EMR_Info do
      begin
        XMLNode.AttributeValue['x1'] := GetX(xDest);
        XMLNode.AttributeValue['y1'] := GetY(yDest);
        XMLNode.AttributeValue['x2'] := GetX(xDest + cxDest);
        XMLNode.AttributeValue['y2'] := GetY(yDest + cyDest);
      end;
      WritePrimitiveAttr(CurrLineKind, CurrPenColor, haNone,
        CurrBrushColor, XMLNode);
    end
    else if (ARecord is TEMF_PolyBezier160) then
    begin
      if HasPath and (RecIndex = PathPrevIndex + 1)
        and (PathPrevKind = ppk_PolybezierTo) then
      begin
        XMLNode := PathPrevXMLNode;
      end
      else XMLNode := AddItem('curve');
    //TPathPrevKind = (ppk_None, ppk_LineTo, ppk_PolylineTo, ppk_PolybezierTo);
  //Bez0,Bez1,Bez2,Bez3:TPoint2D;
  //BezMod:Byte;
      with ARecord as TEMF_PolyBezier160 do
      begin
        if ARecord is TEMF_PolyBezierTo16 then
        begin
          BezArr[0] := Point2D(CurrX, CurrY);
          BezMod := 1;
          PathPrevIndex := RecIndex;
          PathPrevKind := ppk_PolybezierTo;
          PathPrevXMLNode := XMLNode;
        end
        else BezMod := 0;
        St := '';
        for I := 0 to High(PointsArray16) do
        begin
          BezArr[BezMod] := Point2D(GetX(PointsArray16[I].X),
            GetY(PointsArray16[I].Y));
          if BezMod = 3 then
          begin
            if I < 5 then AddPoint2D(BezArr[0]);
            for J := 0 to 8 do
              AddPoint2D(BezierPoint(BezArr[0], BezArr[1],
                BezArr[2], BezArr[3], {0.03 + J / 8 * 0.97} BezFract[J]));
            AddPoint2D(BezArr[3]);
            BezArr[0] := BezArr[3];
          end;
          //St := St + FloatToStr() + ',' + FloatToStr();
          if I < High(PointsArray16) then St := St + ' '
          else
            if ARecord is TEMF_PolyBezierTo16 then
            begin
              CurrX := GetX(PointsArray16[I].X);
              CurrY := GetY(PointsArray16[I].Y);
            end;
          BezMod := Succ(BezMod mod 3);
        end;
      end;
      WritePrimitiveAttr(CurrLineKind, CurrPenColor, CurrHatching,
        CurrBrushColor, XMLNode);
      XMLNode.Text := XMLNode.Text + St;
    end
    else if (ARecord is TEMF_Poly16) then
    begin
      {if (CurrLineKind <> liNone)
        or (CurrHatching <> haNone) then}
      begin
        if HasPath and (RecIndex = PathPrevIndex + 1)
        //and (PathPrevKind = ppk_PolybezierTo)
        then
        begin
          XMLNode := PathPrevXMLNode;
        end
        else
          if ARecord is TEMF_Polygon16 then
            XMLNode := AddItem('polygon')
          else if ARecord is TEMF_Polyline16 then
            XMLNode := AddItem('polyline')
          else if ARecord is TEMF_PolylineTo16 then
            XMLNode := AddItem('polyline');
        with ARecord as TEMF_Poly16 do
        begin
          if ARecord is TEMF_PolylineTo16 then
          begin
            if RecIndex = PathPrevIndex + 1 then St := ''
            else St := FloatToStr(CurrX) + ',' +
              FloatToStr(CurrY) + ' ';
            PathPrevIndex := RecIndex;
            PathPrevKind := ppk_PolylineTo;
            PathPrevXMLNode := XMLNode;
          end
          else St := '';
          for I := 0 to High(PointsArray16) do
          begin
            St := St +
              FloatToStr(GetX(PointsArray16[I].X)) + ','
              + FloatToStr(GetY(PointsArray16[I].Y));
            if I < High(PointsArray16) then St := St + ' '
            else
              if ARecord is TEMF_PolylineTo16 then
              begin
                CurrX := GetX(PointsArray16[I].X);
                CurrY := GetY(PointsArray16[I].Y);
              end;
          end;
        end;
        WritePrimitiveAttr(CurrLineKind, CurrPenColor, CurrHatching,
          CurrBrushColor, XMLNode);
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
          begin
            St := St +
              FloatToStr(GetX(GetPnt(J, I).X)) + ','
              + FloatToStr(GetY(GetPnt(J, I).Y));
            if I < GetLen(J) - 1 then St := St + ' ';
          end;
          WritePrimitiveAttr(CurrLineKind, CurrPenColor, CurrHatching,
            CurrBrushColor, XMLNode);
          XMLNode.Text := St;
        end;

      {if (CurrLineKind <> liNone)
        or (CurrHatching <> haNone) then
      begin
      end;}
    end
    else if ARecord is TEMF_MoveToEx then
    begin
      XMLNode := AddItem('moveto');
      with (ARecord as TEMF_MoveToEx).EMR_Info do
      begin
        XMLNode.AttributeValue['x'] := GetX(ptl.X);
        XMLNode.AttributeValue['y'] := GetY(ptl.Y);
        CurrX := GetX(ptl.X);
        CurrY := GetY(ptl.Y);
        MoveToX := CurrX;
        MoveToY := CurrY;
      end;
    end
    else if ARecord is TEMF_CloseFigure then
    begin
      XMLNode := AddItem('line');
      XMLNode.AttributeValue['x1'] := CurrX;
      XMLNode.AttributeValue['y1'] := CurrY;
      XMLNode.AttributeValue['x2'] := MoveToX;
      XMLNode.AttributeValue['y2'] := MoveToY;
      WritePrimitiveAttr(CurrLineKind, CurrPenColor, CurrHatching,
        CurrBrushColor, XMLNode);
      XMLNode :=
        AddItem('CLOSEFIGURE');
    end
    else if ARecord is TEMF_LineTo then
    begin
      if HasPath and (RecIndex = PathPrevIndex + 1) and (PathPrevKind <>
        ppk_None) then
        if (PathPrevKind in [ppk_PolybezierTo, ppk_PolylineTo]) then
        begin //continue path
          XMLNode := PathPrevXMLNode;
          with (ARecord as TEMF_LineTo).EMR_Info do
            XMLNode.Text := XMLNode.Text +
              FloatToStr(GetX(ptl.X)) + ',' + FloatToStr(GetY(ptl.Y)) + ' ';
          PathPrevIndex := RecIndex;
        end
        else //ppk_LineTo
        begin
          with (ARecord as TEMF_LineTo).EMR_Info do
            St :=
              PathPrevXMLNode.AttributeValueSt['x1'] + ','
              + PathPrevXMLNode.AttributeValueSt['y1'] + ' '
              + PathPrevXMLNode.AttributeValueSt['x2'] + ','
              + PathPrevXMLNode.AttributeValueSt['y2'] + ' '
              + FloatToStr(GetX(ptl.X)) + ',' + FloatToStr(GetY(ptl.Y)) + ' ';
          XML.DocumentElement.RemoveChild(PathPrevXMLNode);
          XMLNode := AddItem('polyline');
          XMLNode.Text := St;
          with (ARecord as TEMF_LineTo).EMR_Info do
            XMLNode.Text := XMLNode.Text +
              FloatToStr(GetX(ptl.X)) + ',' + FloatToStr(GetY(ptl.Y)) + ' ';
          PathPrevXMLNode := XMLNode;
          PathPrevIndex := RecIndex;
          PathPrevKind := ppk_PolylineTo;
        end
      else
      begin
        XMLNode := AddItem('line');
        with (ARecord as TEMF_LineTo).EMR_Info do
        begin
          XMLNode.AttributeValue['x1'] := CurrX;
          XMLNode.AttributeValue['y1'] := CurrY;
          XMLNode.AttributeValue['x2'] := GetX(ptl.X);
          XMLNode.AttributeValue['y2'] := GetY(ptl.Y);
          WritePrimitiveAttr(CurrLineKind, CurrPenColor, CurrHatching,
            CurrBrushColor, XMLNode);
          CurrX := GetX(ptl.X);
          CurrY := GetY(ptl.Y);
          PathPrevXMLNode := XMLNode;
          PathPrevIndex := RecIndex;
          PathPrevKind := ppk_LineTo;
        end;
      end;
    end
    else if (ARecord is TEMF_ObjectHandle) then
    begin
      XMLNode := AInfoRec;
      with (ARecord as TEMF_ObjectHandle).EMR_Info do
      begin
        //ihObject: DWORD;    Object handle index
        XMLNode.AttributeValue['Handle'] := ihObject;
        if (ARecord is TEMF_SelectObject) then
        begin
          if GetHandle(ihObject) is TFontObject then
          begin
            CurrFontHandle := ihObject;
            CurrFont := (GetHandle(ihObject) as
              TFontObject).Font
          end
          else if GetHandle(ihObject) is TPenObject then
          begin
            CurrPenHandle := ihObject;
            CurrPen := (GetHandle(ihObject) as TPenObject).Pen;
            IsCurrPenExt := False;
          end
          else if GetHandle(ihObject) is TExtPenObject then
          begin
            CurrPenHandle := ihObject;
            CurrExtPen := (GetHandle(ihObject) as
              TExtPenObject).ExtPen;
            IsCurrPenExt := True;
          end
          else if GetHandle(ihObject) is TBrushObject then
          begin
            CurrBrushHandle := ihObject;
            CurrBrush := (GetHandle(ihObject) as
              TBrushObject).Brush;
          end;
        end
        else if (ARecord is TEMF_DeleteObject) then
        begin
        //Handles.Delete(ihObject);
          if CurrFontHandle = ihObject then CurrFontHandle := -1
          else if CurrPenHandle = ihObject then
            CurrPenHandle := -1
          else if CurrBrushHandle = ihObject then
            CurrBrushHandle := -1;
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
        SetHandle(ihFont, TFontObject.Create);
        (GetHandle(ihFont) as TFontObject).Font :=
          elfw.elfLogFont;
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
    else if (ARecord is TEMF_CreatePen) then
    begin
      XMLNode := AddItem('Pen');
      with (ARecord as TEMF_CreatePen).EMR_Info do
      begin
        XMLNode.AttributeValue['Handle'] := ihPen;
        SetHandle(ihPen, TPenObject.Create);
        (GetHandle(ihPen) as TPenObject).Pen := lopn;
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
        SetHandle(ihPen, TExtPenObject.Create);
        (GetHandle(ihPen) as TExtPenObject).ExtPen := elp;
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
        SetHandle(ihBrush, TBrushObject.Create);
        (GetHandle(ihBrush) as TBrushObject).Brush := LB;
        with XMLNode, LB do
        begin
          AttributeValue['Handle'] := ihBrush;
          AttributeValue['Style'] := lbStyle;
          AttributeValue['Color'] := ColorToString(lbColor);
          AttributeValue['Hatch'] := lbHatch;
        end;
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
        BkMode := iMode;
      end;
    end
    else if (ARecord is TEMF_SetBkColor) then
    begin
      XMLNode := AInfoRec;
      with (ARecord as TEMF_SetBkColor).EMR_Info do
      begin
        XMLNode.AttributeValue['Color'] :=
          ColorToString(crColor);
        BkColor := crColor;
      end;
    end
    else if (ARecord is TEMF_SetTextColor) then
    begin
      XMLNode := AddItem('TextColor');
      XMLNode.AttributeValue['Color'] :=
        ColorToString((ARecord as
        TEMF_SetTextColor).EMR_Info.crColor);
    end
    else if (ARecord is TEMF_SetTextAlign) then
    begin
      XMLNode := AddItem('TextAlign');
      TextAlignment := (ARecord as
        TEMF_SetTextAlign).EMR_Info.iMode;
      XMLNode.AttributeValue['Align'] := TextAlignment;
        //??DT_LEFT = 0; DT_CENTER = 1;   DT_RIGHT = 2;
        //??DT_TOP = 0;  DT_VCENTER = 4;  DT_BOTTOM = 8;
  //TA_LEFT = 0;  TA_RIGHT = 2;  TA_CENTER = 6;
  //TA_TOP = 0;  TA_BOTTOM = 8;  TA_BASELINE = 24;
    end
    else if (ARecord is TEMF_ExtTextOut) then
    begin
      XMLNode := AddItem('text');
      with (ARecord as TEMF_ExtTextOut).EMR_Info do
      begin
      //XMLNode.AttributeValue['x'] := CurrX + rclBox.Left;
      //XMLNode.AttributeValue['y'] := CurrY - rclBox.Bottom;
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
        CurrFontHeight := Abs(CurrFont.lfHeight);
        if CurrFontHeight = 0 then CurrFontHeight := 12;
        XMLNode.AttributeValue['h'] := //Round()
          Abs(CurrFontHeight * GetYScale);
//        := (rclBox.Bottom - rclBox.Top){ * ASingle};
        CPoint := emrtext.ptlReference;
        if (CPoint.X = 0) and (CPoint.Y = 0) then
        begin
          CPoint.X := rclBounds.Left;
          CPoint.Y := rclBounds.Top;
        end;
        XMLNode.AttributeValue['reference_point'] :=
          IntToStr(CPoint.X) + ',' + IntToStr(CPoint.Y);
        XMLNode.AttributeValue['x'] := GetX(CPoint.X);
        if (TextAlignment and TA_BASELINE) <> 0 then
          XMLNode.AttributeValue['y'] := GetY(CPoint.Y
            - CurrFontHeight * 0.35)
              //an approximate rule-of-thumb correction when text is aligned at baseline
        else
          XMLNode.AttributeValue['y'] := GetY(CPoint.Y);
        XMLNode.AttributeValue['nChars'] := emrtext.NChars;
        XMLNode.AttributeValue['Offset_to_string']
          := emrtext.offString;
        XMLNode.AttributeValue['t']
          := (ARecord as TEMF_ExtTextOut).Str;
      (*//DT_LEFT = 0; DT_CENTER = 1;   DT_RIGHT = 2;
      {if (TextAlignment and DT_CENTER) <> 0 then
        XMLNode.AttributeValue['jh'] := 'c'
      else } if (TextAlignment and DT_RIGHT) <> 0 then
          XMLNode.AttributeValue['jh'] := 'r';
      //else XMLNode.AttributeValue['jh'] := 'l';
      //DT_TOP = 0;  DT_VCENTER = 4;  DT_BOTTOM = 8;
        if (TextAlignment and DT_VCENTER) <> 0 then
          XMLNode.AttributeValue['jv'] := 'c'
        else if (TextAlignment and DT_BOTTOM) <> 0 then
          XMLNode.AttributeValue['jv'] := 't'
        else XMLNode.AttributeValue['jv'] := 'b';*)
        if (TextAlignment and TA_RIGHT) <> 0 then
          XMLNode.AttributeValue['jh'] := 'r'
        else if (TextAlignment and TA_CENTER) <> 0 then
          XMLNode.AttributeValue['jh'] := 'c'
        else XMLNode.AttributeValue['jh'] := 'l';
        if (TextAlignment and TA_BASELINE) <> 0 then
          XMLNode.AttributeValue['jv'] := 'c'
        else if (TextAlignment and TA_BOTTOM) <> 0 then
          XMLNode.AttributeValue['jv'] := 'b'
        else XMLNode.AttributeValue['jv'] := 't';
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
      end
    end
    else if (ARecord is TEMF_PathBounds) then
    begin
      if {HasPath and (RecIndex <= PathPrevIndex + 2) and}(PathPrevKind <>
        ppk_None) then
      begin
        if ARecord is TEMF_FillPath then
          WritePrimitiveAttr(liNone, clDefault, CurrHatching,
            CurrBrushColor, PathPrevXMLNode)
        else if ARecord is TEMF_StrokeAndFillPath then
          WritePrimitiveAttr(CurrLineKind, CurrPenColor, CurrHatching,
            CurrBrushColor, PathPrevXMLNode)
        else if ARecord is TEMF_StrokePath then
          WritePrimitiveAttr(CurrLineKind, CurrPenColor, haNone,
            clNone, PathPrevXMLNode);
      end
      else XMLNode := AInfoRec;
    end
    else
    begin
      XMLNode := AInfoRec;
    end;
  end;
  Handles.Free;
  DC_InfoList.Free;
end;

end.

