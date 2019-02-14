unit MprtSVG;

// SVG import module

interface

uses Types, SysUtils, Classes, Contnrs, Graphics, StrUtils,
  Drawings, GObjects,
{$IFDEF VER140}
  WinBasic,
{$ELSE}
  LazBasic,
{$ENDIF}
  PrimSAX, Geometry, Devices, Input;

type

  T_SVG_Import_State = class
    LineColor: TColor;
    LineWidth: TRealType;
    LineStyle: TLineStyle;
    Hatching: THatching;
    HatchColor: TColor;
    FillColor: TColor;
    T: TTransf2D;
    FontHeight: TRealType;
    FontFamily: string;
    FontStyle: TFontStyles;
    HJustification: THJustification;
    TextPos: TPoint2D;
    Text: string;
    procedure Inherit(const State: T_SVG_Import_State);
    procedure UpdateLC(const LineColor: TColor);
    procedure UpdateLW(const LineWidth: TRealType);
    procedure UpdateLI(const LineStyle: TLineStyle);
    procedure UpdateHA(const Hatching: THatching);
    procedure UpdateHC(const HatchColor: TColor);
    procedure UpdateFill(const FillColor: TColor);
    procedure UpdateT(const T: TTransf2D);
  end;

  T_SVG_Import = class(T_Import)
  private
    fSAX: TPrimSAX;
    fPathParser: T_SVG_Path_Parser;
    fPathPoints: TPointsSet2D;
    fPathAttributes: TAttributes;
    fStack: TObjectStack;
    fStyleAttributes: TAttributes;
    fScale: TRealType; //(user units measured in mm)
    viewBox_X, viewBox_Y, viewBox_W, viewBox_H, W_MM, H_MM:
    TRealType;
    fState: T_SVG_Import_State;
    fInStyle: Boolean;
    fCSS: TAttributes;
    fEntities: TAttributes;
    procedure PathOnClose(const X, Y: TRealType);
    procedure PathOnMoveTo(const X, Y: TRealType);
    procedure PathOnLineTo(const X, Y: TRealType);
    procedure PathOnBezierTo(const X1, Y1, X2, Y2, X3, Y3:
      TRealType);
    procedure StartElement(var E: TPrimElement);
    procedure SAX_Comment(const Comment: string);
    //function TransfPoint(P: TPoint2D): TPoint2D;
    function NewState: T_SVG_Import_State;
    procedure EndStateProc(var E: TPrimElement);
    procedure StyleStartProc(var E: TPrimElement);
    procedure StyleEndProc(var E: TPrimElement);
    procedure CDATAProc(const CDATA: string);
    procedure Entities_Proc(const DOCTYPE: string;
      const Attributes: TAttributes);
    function ReplaceEntities(const St: string): string;
    procedure SVGProc(var E: TPrimElement);
    procedure DescProc(var E: TPrimElement);
    procedure TitleProc(var E: TPrimElement);
    procedure StartGroupProc(var E: TPrimElement);
    procedure PathProc(var E: TPrimElement);
    procedure TextStartProc(var E: TPrimElement);
    procedure TextEndProc(var E: TPrimElement);
    procedure TSpanStartProc(var E: TPrimElement);
    procedure TSpanEndProc(var E: TPrimElement);
    procedure textPathStartProc(var E: TPrimElement);
    procedure textPathEndProc(var E: TPrimElement);
    procedure LineProc(var E: TPrimElement);
    procedure RectProc(var E: TPrimElement);
    procedure CircleProc(var E: TPrimElement);
    procedure EllipseProc(var E: TPrimElement);
    procedure PolygonProc(var E: TPrimElement);
    procedure PolylineProc(var E: TPrimElement);
    procedure ParseStyleAttributes(St: string);
    function StoreAttributes(
      const Attributes: TAttributes;
      const Tag: string): T_SVG_Import_State;
    procedure SetAttributes;
    procedure Correct_LineWidth;
  public
    constructor Create(Drawing: TDrawing2D);
    destructor Destroy; override;
    procedure ParseFromStream(const Stream: TStream);
    procedure ParseFromFile(const FileName: string);
  end;

implementation

uses MiscUtils, ColorEtc, gzio;

{ --================ T_SVG_Import_State ==================-- }

procedure T_SVG_Import_State.Inherit(const State:
  T_SVG_Import_State);
begin
  LineColor := State.LineColor;
  LineWidth := State.LineWidth;
  LineStyle := State.LineStyle;
  Hatching := State.Hatching;
  HatchColor := State.HatchColor;
  FillColor := State.FillColor;
  T := State.T;
  FontHeight := State.FontHeight;
  FontFamily := State.FontFamily;
  FontStyle := State.FontStyle;
  HJustification := State.HJustification;
  TextPos := State.TextPos;
  Text := State.Text;
end;

procedure T_SVG_Import_State.UpdateLC(const LineColor: TColor);
begin
  Self.LineColor := LineColor;
end;

procedure T_SVG_Import_State.UpdateLW(const LineWidth: TRealType);
begin
  Self.LineWidth := LineWidth;
end;

procedure T_SVG_Import_State.UpdateLI(const LineStyle: TLineStyle);
begin
  Self.LineStyle := LineStyle;
end;

procedure T_SVG_Import_State.UpdateHA(const Hatching: THatching);
begin
  Self.Hatching := Hatching;
end;

procedure T_SVG_Import_State.UpdateHC(const HatchColor: TColor);
begin
  Self.HatchColor := HatchColor;
end;

procedure T_SVG_Import_State.UpdateFill(const FillColor: TColor);
begin
  Self.FillColor := FillColor;
end;

procedure T_SVG_Import_State.UpdateT(const T: TTransf2D);
begin
  Self.T := MultiplyTransform2D(T, Self.T);
end;

{ --================ T_SVG_Import ==================-- }

constructor T_SVG_Import.Create(Drawing: TDrawing2D);
  procedure AddProc(const Tag: string;
    const StartProc, EndProc: TPrimElementMethod);
  begin
    fSAX.AddProc(Tag, StartProc, EndProc);
    fSAX.AddProc('svg:' + Tag, StartProc, EndProc);
  end;
begin
  inherited Create(Drawing);
  fSAX := TPrimSAX.Create(
    StartElement, nil, SAX_Comment);
  AddProc('svg', SVGProc, nil);
  AddProc('desc', nil, DescProc);
  AddProc('title', nil, TitleProc);
  AddProc('g', StartGroupProc, EndStateProc);
  AddProc('a', StartGroupProc, EndStateProc);
  AddProc('path', PathProc, EndStateProc);
  AddProc('text', TextStartProc, TextEndProc);
  AddProc('tspan', TSpanStartProc, TSpanEndProc);
  AddProc('textPath', textPathStartProc, textPathEndProc);
  AddProc('line', LineProc, EndStateProc);
  AddProc('rect', RectProc, EndStateProc);
  AddProc('circle', CircleProc, EndStateProc);
  AddProc('ellipse', EllipseProc, EndStateProc);
  AddProc('polygon', PolygonProc, EndStateProc);
  AddProc('polyline', PolylineProc, EndStateProc);
  AddProc('style', StyleStartProc, StyleEndProc);
  fSAX.OnCDATA := CDATAProc;
  fSAX.OnDOCTYPE := Entities_Proc;
  fPathParser := T_SVG_Path_Parser.Create(
    PathOnClose, PathOnMoveTo, PathOnLineTo, PathOnBezierTo);
  fPathPoints := TPointsSet2D.Create(0);
  fStack := TObjectStack.Create;
  fStyleAttributes := TAttributes.Create;
  fStyleAttributes.Delimiter := ';';
  fCSS := TAttributes.Create;
  fEntities := TAttributes.Create;
end;

destructor T_SVG_Import.Destroy;
begin
  fSAX.Free;
  fPathParser.Free;
  fStack.Free;
  fStyleAttributes.Free;
  fCSS.Free;
  fEntities.Free;
  inherited Destroy;
end;

{function T_SVG_Import.TransfPoint(P: TPoint2D): TPoint2D;
begin
  Result := Point2D(P.X, P.Y);
end;}

procedure T_SVG_Import.PathOnClose(const X, Y: TRealType);
begin
  if fPathPoints.Count <= 1 then
  begin
    fPathPoints.Clear;
    Exit;
  end;
  if fPathParser.AllAsBezier then
  begin
    AddClosedBezier;
    fCurrPrimitive.Points.Copy(fPathPoints,
      0, fPathPoints.Count - 2);
  end
  else
  begin
    AddPolygon;
    if IsSamePoint2D(
      fPathPoints[fPathPoints.Count - 1], Point2D(X, Y)) then
      fCurrPrimitive.Points.Copy(fPathPoints,
        0, fPathPoints.Count - 2)
    else
      fCurrPrimitive.Points.Copy(fPathPoints,
        0, fPathPoints.Count - 1);
  end;
  fPathPoints.Clear;
  SetAttributes;
end;

procedure T_SVG_Import.PathOnMoveTo(const X, Y: TRealType);
begin
  if fPathPoints.Count <= 1 then
  begin
    fPathPoints.Clear;
    fPathPoints.Add(Point2D(X, Y));
    Exit;
  end;
  if fPathParser.AllAsBezier then AddBezier
  else AddPolyline;
  fCurrPrimitive.Points.Copy(fPathPoints, 0,
    fPathPoints.Count - 1);
  SetAttributes;
  fPathPoints.Clear;
  fPathPoints.Add(Point2D(X, Y));
end;

procedure T_SVG_Import.PathOnLineTo(const X, Y: TRealType);
begin
  fPathPoints.Add(Point2D(X, Y));
end;

procedure T_SVG_Import.PathOnBezierTo(
  const X1, Y1, X2, Y2, X3, Y3: TRealType);
begin
  fPathPoints.Add(Point2D(X1, Y1));
  fPathPoints.Add(Point2D(X2, Y2));
  fPathPoints.Add(Point2D(X3, Y3));
end;

procedure T_SVG_Import.StartElement(var E: TPrimElement);
begin
end;

function GetRealTypeAttr(Value: Variant; const Default: TRealType;
  const Scale: TRealType): TRealType;
//Scale --- user unit = Scale mm
var
  St, Units, NumSt: string;
  Len, Index: Integer;
const Data: array[0..9] of Single =
  (1, 3.5, 3.5, 1, 0.352777777777778, 4.23333333333333, 10, 1,
    25.4, 0.01);
begin
  St := Trim(string(Value));
  if St = '' then
  begin
    Result := Default;
    Exit;
  end;
//The list of unit identifiers in SVG matches the list of unit identifiers in CSS:
// em, ex, px, pt, pc, cm, mm, in and percentages
  Len := Length(St);
  if St[Len] = '%' then
  begin
    Units := '%';
    NumSt := Trim(LeftStr(St, Len - 1));
  end
  else if Len > 1 then
  begin
    Units := RightStr(St, 2);
    NumSt := Trim(LeftStr(St, Len - 2));
  end;
  Index := CSV_Find('em,ex,px,pt,pc,cm,mm,in,%', Units);
  if Index = 0 then NumSt := St;
  if Pos(' ', NumSt) > 0 then
    NumSt := LeftStr(NumSt, Pos(' ', NumSt) - 1);
  if Pos(',', NumSt) > 0 then
    NumSt := LeftStr(NumSt, Pos(',', NumSt) - 1);
  Result := StrToFloat(NumSt);
  Result := Data[Index] * Result;
  if Index in [1, 2, 4, 5, 6, 7, 8] then Result := Result / Scale;
//em = 3.5/Scale (assume)
//ex = 3.5/Scale (assume)
//px = 1
//pt = 0.352777777777778/Scale
//pc = 4.23333333333333/Scale
//cm = 10/Scale
//mm = 1/Scale
//in = 25.4/Scale
//% = 0.01

{  For example, suppose that the user agent can determine from its environment that "1px" corresponds to "0.2822222mm" (i.e., 90dpi). Then, for all processing of SVG content:
"1pt" equals "1.25px" (and therefore 1.25 user units)
"1pc" equals "15px" (and therefore 15 user units)
"1mm" would be "3.543307px" (3.543307 user units)
"1cm" equals "35.43307px" (and therefore 35.43307 user units)
"1in" equals "90px" (and therefore 90 user units)}
//pc = 15/1.25 pt=3/0.25 pt=12pt
//pt=1.25/3.543307 mm=0.352777786401235mm=25.4/72mm=0.352777777777778mm
//pc=15/3.543307 mm = 4.23333343681482mm=25.4/6mm=4.23333333333333mm
//in=90/3.543307 mm = 25.4mm
//in=90/1.25 pt =72pt
end;

function T_SVG_Import.NewState: T_SVG_Import_State;
var
  OldState: T_SVG_Import_State;
begin
  if fStack.Count > 0 then
    OldState := fStack.Peek as T_SVG_Import_State
  else OldState := nil;
  Result := T_SVG_Import_State.Create;
  fStack.Push(Result);
  if Assigned(OldState) then Result.Inherit(OldState);
end;

procedure T_SVG_Import.EndStateProc(var E: TPrimElement);
begin
  fStack.Pop.Free;
end;

procedure ParseViewBox(VB: string; var X, Y, W, H: TRealType);
var
  List: TStringList;
begin
  X := 0;
  Y := 0;
  W := 100;
  H := 100;
  VB := AnsiReplaceStr(VB, ',', ' ');
  VB := AnsiReplaceStr(VB, '#10', ' ');
  VB := AnsiReplaceStr(VB, '#13', ' ');
  VB := AnsiReplaceStr(VB, '   ', ' ');
  VB := AnsiReplaceStr(VB, '  ', ' ');
  VB := AnsiReplaceStr(VB, '  ', ' ');
  List := TStringList.Create;
  try
    List.Delimiter := ' ';
    List.DelimitedText := VB;
    if List.Count < 4 then Exit;
{       E.Attributes, 'width', viewBox_W, Default_px) * Default_px;}
    X := GetRealTypeAttr(List[0], 0, 1);
    Y := GetRealTypeAttr(List[1], 0, 1);
    W := GetRealTypeAttr(List[2], 0, 1);
    H := GetRealTypeAttr(List[3], 0, 1);
  finally
    List.Free;
  end;
end;

procedure T_SVG_Import.StyleStartProc(var E: TPrimElement);
begin
  fInStyle := True;
end;

procedure T_SVG_Import.StyleEndProc(var E: TPrimElement);
begin
  fInStyle := False;
end;

procedure ParseCSS(const CDATA: string; CSS: TAttributes);
var
  fParser: TSimpleParser;
  Name, Value: string;
  procedure TakeSpace;
  begin
    fParser.SkipAnyMult([#32, #13, #10, #9]);
  end;
  procedure GetItem;
  begin
    Name := '';
    Value := '';
    Name := fParser.GetAnyMult([#9..#255] - ['{', ']']);
    if fParser.ViewCh = '{' then
    begin
      fParser.GetCh;
      Value := fParser.GetAnyMult([#9..#255] - ['}', ']']);
    end;
    if fParser.ViewCh = '}' then fParser.GetCh;
    Name := Trim(Name);
    Value := Trim(Value);
  end;
begin
  CSS.Clear;
  fParser := TSimpleParser.Create;
  try
    fParser.SetSource(@CDATA);
    repeat
      GetItem;
      if Name <> '' then
      begin
        CSS.Add(Name + '=' + Value);
      end;
    until fParser.Finished;
  finally
    fParser.Free;
  end;
end;

procedure T_SVG_Import.CDATAProc(const CDATA: string);
begin
  if fInStyle then
  begin
    ParseCSS(CDATA, fCSS);
    //SysBasic.MessageBoxInfo(fCSS.Text);
  end;
end;

procedure T_SVG_Import.Entities_Proc(const DOCTYPE: string;
  const Attributes: TAttributes);
begin
  fEntities.Assign(Attributes);
end;

function T_SVG_Import.ReplaceEntities(const St: string): string;
var
  fParser: TSimpleParser;
  Entity: string;
  //WCh:WideChar;
  Ch: Char;
  function GetEntity: string;
  var
    I, J: Integer;
  begin
    Result := '';
    if fParser.ViewCh <> '&' then Exit;
    fParser.GetCh;
    Result := fParser.GetAnyMult([#9..#255] - [';']);
    if fParser.ViewCh <> ';' then
    begin
      Result := '&' + Result;
      Exit;
    end;
    fParser.GetCh;
    if (Result <> '') and (Result[1] = '#') then
    begin
      Val(System.Copy(Result, 2, Length(Result)), I, J);
      if J = 0 then
      begin
        //WCh := Chr(I);
        //Result := Chr(I);
        Ch := Chr(I);
        if Ord(Ch) = I then Result := Ch else
          Result := '&' + Result + ';';
        Exit;
      end;
    end;
    if fEntities.IndexOfName(Result) > 0 then
      Result := fEntities.Values[Result]
  end;
begin
  Result := '';
  fParser := TSimpleParser.Create;
  try
    fParser.SetSource(@St);
    repeat
      Result := Result + fParser.GetAnyMult([#9..#255] - ['&']);
      Entity := GetEntity;
      Result := Result + Entity;
    until fParser.Finished;
  finally
    fParser.Free;
  end;
end;

procedure T_SVG_Import.SVGProc(var E: TPrimElement);
const
  Default_Size = 150; // Assume that "screen size" is approx. 150mm
  Default_px = 0.2; // Assume that pixel (px) is 0.2mm
begin
  if E.Attributes.IndexOfName('viewBox') >= 0 then
    ParseViewBox(E.Attributes.Values['viewBox'],
      viewBox_X, viewBox_Y, viewBox_W, viewBox_H)
  else
  begin
    viewBox_X := 0; viewBox_Y := 0; viewBox_W := 0; viewBox_H := 0;
  end;
  if E.Attributes.IndexOfName('width') >= 0 then
  begin
    W_MM := GetRealTypeAttr(
      E.Attributes.Values['width'], viewBox_W, Default_px) *
      Default_px;
    if Abs(W_MM / Default_px - 1) < 1E-4 then
      W_MM := Default_Size;
  end
  else W_MM := Default_Size;
  if E.Attributes.IndexOfName('height') >= 0 then
  begin
    H_MM := GetRealTypeAttr(
      E.Attributes.Values['height'], viewBox_H, Default_px) *
      Default_px;
    if Abs(H_MM / Default_px - 1) < 1E-4 then
      H_MM := Default_Size;
  end
  else H_MM := Default_Size;
  if (viewBox_W > 0) and (viewBox_H > 0) then
    fScale := Sqrt(W_MM / viewBox_W * H_MM / viewBox_H)
  else if viewBox_W > 0 then fScale := W_MM / viewBox_W
  else if viewBox_H > 0 then fScale := H_MM / viewBox_H
  else fScale := Default_px;
  fDrawing2D.PicScale := fScale;
  fDrawing2D.LineWidthBase := fScale;
  //width="8in" height="8in" viewBox="0 0 1 1"
  fState := NewState;
  fState.LineColor := Graphics.clDefault;
  fState.LineWidth := 1;
  fState.LineStyle := liNone; //liSolid;
  fState.Hatching := haNone;
  fState.HatchColor := Graphics.clDefault;
  fState.FillColor := clBlack; //clSilver;Graphics.clDefault;
  fState.T := IdentityTransf2D;
  fState.FontHeight := 10;
  fState.FontFamily := '';
  fState.FontStyle := [];
  fState.HJustification := jhLeft;
  fState.TextPos := Point2D(0, 0);
  fState.Text := '';
  fInStyle := False;
  StoreAttributes(E.Attributes, 'svg');
end;

procedure T_SVG_Import.DescProc(var E: TPrimElement);
begin
  fDrawing2D.Comment := ReplaceEntities(E.Text);
end;

procedure T_SVG_Import.TitleProc(var E: TPrimElement);
begin
  fDrawing2D.Caption := ReplaceEntities(E.Text);
end;

procedure T_SVG_Import.StartGroupProc(var E: TPrimElement);
begin
  StoreAttributes(E.Attributes, 'g');
end;

procedure T_SVG_Import.PathProc(var E: TPrimElement);
var
  PathData: string;
begin
  if E.Attributes.IndexOfName('d') < 0 then Exit;
  StoreAttributes(E.Attributes, 'path');
  PathData := E.Attributes.Values['d'];
  fPathParser.AllAsBezier := SVG_Path_HasCurves(PathData);
  fPathAttributes := E.Attributes;
  fPathParser.Parse(PathData);
  if fPathPoints.Count <= 1 then
  begin
    fPathPoints.Clear;
    Exit;
  end;
  if fPathParser.AllAsBezier then AddBezier
  else AddPolyline;
  fCurrPrimitive.Points.Copy(fPathPoints, 0,
    fPathPoints.Count - 1);
  fPathPoints.Clear;
  SetAttributes;
end;

procedure T_SVG_Import.TextStartProc(var E: TPrimElement);
begin
  fState := StoreAttributes(E.Attributes, 'text');
  fState.TextPos := Point2D(
    GetRealTypeAttr(E.Attributes.Values['x'], 0, fScale)
    + GetRealTypeAttr(E.Attributes.Values['dx'], 0, fScale),
    GetRealTypeAttr(E.Attributes.Values['y'], 0, fScale)
    + GetRealTypeAttr(E.Attributes.Values['dy'], 0, fScale));
end;

procedure T_SVG_Import.TextEndProc(var E: TPrimElement);
begin
  fState := fStack.Peek as T_SVG_Import_State;
  fState.Text :=
    fState.Text + ReplaceEntities(Trim(E.Text));
  if fState.Text = '' then
  begin
    EndStateProc(E);
    Exit;
  end;
  AddText(fState.TextPos.X, fState.TextPos.Y,
    fState.FontHeight, fState.Text);
  (fCurrPrimitive as TText2D).HJustification :=
    fState.HJustification;
  if HasFontFamily(fState.FontFamily) then
  begin
    (fCurrPrimitive as TText2D).Font.Name := fState.FontFamily;
    (fCurrPrimitive as TText2D).Font.Style := fState.FontStyle;
  end;
  SetAttributes;
  fCurrPrimitive.LineColor := fCurrPrimitive.FillColor;
  fCurrPrimitive.FillColor := Graphics.clNone;
  EndStateProc(E);
end;

procedure T_SVG_Import.TSpanStartProc(var E: TPrimElement);
begin
  fState := StoreAttributes(E.Attributes, 'tspan');
  fState.TextPos := Point2D(
    GetRealTypeAttr(
    E.Attributes.Values['x'], fState.TextPos.X, fScale)
    + GetRealTypeAttr(E.Attributes.Values['dx'], 0, fScale),
    GetRealTypeAttr(
    E.Attributes.Values['y'], fState.TextPos.Y, fScale)
    + GetRealTypeAttr(E.Attributes.Values['dy'], 0, fScale));
end;

procedure T_SVG_Import.TSpanEndProc(var E: TPrimElement);
begin
  fState := fStack.Peek as T_SVG_Import_State;
  fState.Text :=
    fState.Text + ReplaceEntities(Trim(E.Text));
  if fState.Text = '' then
  begin
    EndStateProc(E);
    Exit;
  end;
  AddText(fState.TextPos.X, fState.TextPos.Y,
    fState.FontHeight, fState.Text);
  (fCurrPrimitive as TText2D).HJustification :=
    fState.HJustification;
  if HasFontFamily(fState.FontFamily) then
  begin
    (fCurrPrimitive as TText2D).Font.Name := fState.FontFamily;
    (fCurrPrimitive as TText2D).Font.Style := fState.FontStyle;
  end;
  SetAttributes;
  fCurrPrimitive.LineColor := fCurrPrimitive.FillColor;
  fCurrPrimitive.FillColor := Graphics.clNone;
  EndStateProc(E);
end;

procedure T_SVG_Import.textPathStartProc(var E: TPrimElement);
begin
end;

procedure T_SVG_Import.textPathEndProc(var E: TPrimElement);
begin
  fState := fStack.Peek as T_SVG_Import_State;
  fState.Text :=
    fState.Text + ReplaceEntities(Trim(E.Text));
  EndStateProc(E);
end;

procedure T_SVG_Import.LineProc(var E: TPrimElement);
var
  X1, Y1, X2, Y2: TRealType;
begin
  StoreAttributes(E.Attributes, 'line');
  X1 := GetRealTypeAttr(E.Attributes.Values['x1'], 0, fScale);
  Y1 := GetRealTypeAttr(E.Attributes.Values['y1'], 0, fScale);
  X2 := GetRealTypeAttr(E.Attributes.Values['x2'], 0, fScale);
  Y2 := GetRealTypeAttr(E.Attributes.Values['y2'], 0, fScale);
  Self.fCurrPrimitive := AddLine(X1, Y1, X2, Y2);
  SetAttributes;
end;

procedure T_SVG_Import.RectProc(var E: TPrimElement);
var
  X, Y, W, H, RX, RY, S: TRealType;
begin
  fState := StoreAttributes(E.Attributes, 'rect');
  X := GetRealTypeAttr(E.Attributes.Values['x'], 0, fScale);
  Y := GetRealTypeAttr(E.Attributes.Values['y'], 0, fScale);
  RX := GetRealTypeAttr(E.Attributes.Values['rx'], 0, fScale);
  RY := GetRealTypeAttr(E.Attributes.Values['ry'], RX, fScale);
  W := GetRealTypeAttr(E.Attributes.Values['width'], 0, fScale);
  H := GetRealTypeAttr(E.Attributes.Values['height'], 0, fScale);
  S := Abs(fState.T[1, 1]) + Abs(fState.T[1, 2]) +
    Abs(fState.T[2, 1]) + Abs(fState.T[2, 2]);
  S := Abs(fState.T[1, 1] * fState.T[2, 1]
    - fState.T[1, 2] * fState.T[2, 2]) / Sqr(S);
  if S > 1E-10 {skewed} then
  begin
    if RY > 0 then
      AddRoundRectAsBezier(X, Y, W, H, RX, RY)
    else
      AddRectAsPolygon(X, Y, W, H);
  end
  else
  begin
    if RY > 0 then
      AddRoundRect(X, Y, W, H, RX, RY)
    else
      AddRect(X, Y, W, H);
  end;
  SetAttributes;
end;

procedure T_SVG_Import.CircleProc(var E: TPrimElement);
var
  S: TRealType;
begin
  fState := StoreAttributes(E.Attributes, 'circle');
  S := Abs(fState.T[1, 1]) + Abs(fState.T[1, 2]) +
    Abs(fState.T[2, 1]) + Abs(fState.T[2, 2]);
  S := (Sqr(fState.T[1, 1]) - Sqr(fState.T[2, 1])
    + Sqr(fState.T[1, 2]) - Sqr(fState.T[2, 2])) / Sqr(S);
  if S > 1E-10 {anisotropic transform} then
    Self.fCurrPrimitive := AddEllipseAsBezier(
      GetRealTypeAttr(E.Attributes.Values['cx'], 0, fScale),
      GetRealTypeAttr(E.Attributes.Values['cy'], 0, fScale),
      GetRealTypeAttr(E.Attributes.Values['r'], 0, fScale),
      GetRealTypeAttr(E.Attributes.Values['r'], 0, fScale))
  else
    Self.fCurrPrimitive := AddCircle(
      GetRealTypeAttr(E.Attributes.Values['cx'], 0, fScale),
      GetRealTypeAttr(E.Attributes.Values['cy'], 0, fScale),
      GetRealTypeAttr(E.Attributes.Values['r'], 0, fScale));
  SetAttributes;
end;

procedure T_SVG_Import.EllipseProc(var E: TPrimElement);
var
  S: TRealType;
begin
  fState := StoreAttributes(E.Attributes, 'ellipse');
  S := Abs(fState.T[1, 1]) + Abs(fState.T[1, 2]) +
    Abs(fState.T[2, 1]) + Abs(fState.T[2, 2]);
  S := Abs(fState.T[1, 1] * fState.T[2, 1]
    - fState.T[1, 2] * fState.T[2, 2]) / Sqr(S);
  if S > 1E-10 {skewed} then
    Self.fCurrPrimitive := AddEllipseAsBezier(
      GetRealTypeAttr(E.Attributes.Values['cx'], 0, fScale),
      GetRealTypeAttr(E.Attributes.Values['cy'], 0, fScale),
      GetRealTypeAttr(E.Attributes.Values['rx'], 0, fScale),
      GetRealTypeAttr(E.Attributes.Values['ry'], 0, fScale))
  else
    Self.fCurrPrimitive := AddEllipse(
      GetRealTypeAttr(E.Attributes.Values['cx'], 0, fScale),
      GetRealTypeAttr(E.Attributes.Values['cy'], 0, fScale),
      GetRealTypeAttr(E.Attributes.Values['rx'], 0, fScale),
      GetRealTypeAttr(E.Attributes.Values['ry'], 0, fScale));
  SetAttributes;
end;

procedure T_SVG_Import.PolygonProc(var E: TPrimElement);
begin
  if E.Attributes.IndexOfName('points') < 0 then Exit;
  StoreAttributes(E.Attributes, 'polygon');
  AddPolygon;
  FillPoints(fCurrPrimitive, E.Attributes.Values['points'],
    fCurrPrimitive.Points);
  SetAttributes;
end;

procedure T_SVG_Import.PolylineProc(var E: TPrimElement);
begin
  StoreAttributes(E.Attributes, 'polyline');
  if E.Attributes.IndexOfName('points') < 0 then Exit;
  Self.fCurrPrimitive := AddPolyline;
  FillPoints(fCurrPrimitive, E.Attributes.Values['points'],
    fCurrPrimitive.Points);
  SetAttributes;
end;

procedure T_SVG_Import.SAX_Comment(const Comment: string);
begin
end;

procedure T_SVG_Import.ParseStyleAttributes(St: string);
var
  fParser: TSimpleParser;
  Name, Value: string;
  procedure TakeSpace;
  begin
    fParser.SkipAnyMult([#32, #13, #10, #9]);
  end;
  procedure GetAttr;
  begin
    Name := '';
    Value := '';
    Name := fParser.GetAnyMult([#9..#255] - [';', ':']);
    if fParser.ViewCh = ':' then
    begin
      fParser.GetCh;
      Value := fParser.GetAnyMult([#9..#255] - [';']);
    end;
    if fParser.ViewCh = ';' then fParser.GetCh;
    Name := Trim(Name);
    Value := AnsiDequotedStr(AnsiDequotedStr(Trim(Value), '"'),
      '''');
  end;
begin
  fParser := TSimpleParser.Create;
  try
    fParser.SetSource(@St);
    fStyleAttributes.Clear;
    repeat
      GetAttr;
      if Name <> '' then
      begin
        fStyleAttributes.Add(Name + '=' + Value);
      end;
    until fParser.Finished;
  finally
    fParser.Free;
  end;
end;

function SVG_Color(ClStr: string): TColor;
var
  R, G, B: Integer;
  function GetByte(St: string): Byte;
  var
    Mult: Single;
  begin
    Mult := 1;
    if Pos('%', St) > 0 then
    begin
      Mult := 2.55;
      St := Trim(AnsiReplaceStr(St, '%', ''));
    end;
    try
      Result := Round(StrToFloat(St) * Mult);
    except
      Result := 0;
    end;
  end;
begin
  Result := clSilver;
  ClStr := Trim(ClStr);
  if ClStr = '' then
    Exit;
  if Pos('url(', ClStr) > 0 then
    Exit;
  if Pos('rgb(', ClStr) > 0 then
  begin
    ClStr := AnsiReplaceStr(ClStr, 'rgb', '');
    ClStr := AnsiReplaceStr(ClStr, '(', '');
    ClStr := AnsiReplaceStr(ClStr, ')', '');
    R := GetByte(CSV_Item(ClStr, 1));
    G := GetByte(CSV_Item(ClStr, 2));
    B := GetByte(CSV_Item(ClStr, 3));
    Result := B shl 16 + G shl 8 + R;
    Exit;
  end;
  if (ClStr[1] = '#') and (Length(ClStr) = 4) then
    ClStr := '#' + ClStr[2] + ClStr[2] + ClStr[3] + ClStr[3]
      + ClStr[4] + ClStr[4];
  try
    Result := HtmlToColor(ClStr);
  finally
  end;
end;

function T_SVG_Import.StoreAttributes(
  const Attributes: TAttributes;
  const Tag: string): T_SVG_Import_State;
var
  St, CurrentColor: string;
  ScaleFactor: TRealType;
  AClass, Classes, CSS_Style: string;
  I: Integer;
begin
  Result := NewState;
  if Attributes.IndexOfName('class') >= 0 then
  begin
    CSS_Style := '';
    Classes :=
      AnsiReplaceStr(Attributes.Values['class'], ' ', ',');
    Classes := AnsiReplaceStr(Classes, ',,,', ',');
    Classes := AnsiReplaceStr(Classes, ',,', ',');
    I := 1;
    repeat
      AClass := CSV_Item(Classes, I);
      if AClass <> '' then
      begin
        St := fCSS.Values['.' + AClass];
        if St = '' then
          St := fCSS.Values[Tag + '.' + AClass];
        CSS_Style := CSS_Style + St;
      end;
      Inc(I);
    until AClass = '';
    if CSS_Style = '' then CSS_Style := fCSS.Values[Tag];
    Attributes.Add('style=' + CSS_Style);
  end;
  if Attributes.IndexOfName('style') >= 0 then
  begin
    ParseStyleAttributes(ReplaceEntities(Attributes.Values['style']));
    Attributes.AddStrings(fStyleAttributes);
    Attributes.Delete(Attributes.IndexOfName('style'));
  end;
  if Attributes.IndexOfName('color') >= 0 then
    CurrentColor := Attributes.Values['color']
  else CurrentColor := 'red';
  if Attributes.IndexOfName('fill') >= 0 then
  begin
    if Attributes.Values['fill'] = 'currentColor' then
      Attributes.Values['fill'] := CurrentColor;
    if Attributes.Values['fill'] = 'none' then
    begin
      Result.UpdateHA(haNone);
      Result.UpdateFill(Graphics.clDefault)
    end
    else if Attributes.Values['fill'] = 'currentColor' then
    begin
      Result.UpdateHA(haNone);
      Result.UpdateFill(clLtGray)
    end
    else if Pos('url(', Attributes.Values['fill']) > 0 then
    begin
      Result.UpdateHA(haDiagCross);
      Result.UpdateFill(Graphics.clDefault);
    end
    else
    begin
      Result.UpdateHA(haNone);
      Result.UpdateFill(SVG_Color(Attributes.Values['fill']));
    end;
  end;
  if Attributes.IndexOfName('stroke') >= 0 then
  begin
    if Attributes.Values['stroke'] = 'currentColor' then
      Attributes.Values['stroke'] := CurrentColor;
    if Attributes.Values['stroke'] = 'none'
      then Result.UpdateLI(liNone)
    else if Pos('url(', Attributes.Values['stroke']) > 0 then
    begin
      Result.UpdateLI(liDashed);
      Result.UpdateLC(clLtGray);
    end
    else
    begin
      Result.UpdateLI(liSolid);
      Result.UpdateLC(SVG_Color(Attributes.Values['stroke']));
    end;
    if Attributes.IndexOfName('stroke-dasharray') >= 0 then
      if Attributes.Values['stroke-dasharray'] <> 'none' then
        Result.UpdateLI(liDashed);
  end;
  //if Attributes.IndexOfName('stroke-width') >= 0 then
  begin
    St := Attributes.Values['transform'];
    if St <> '' then
      Result.UpdateT(SVG_Transform_Parse(St));
  end;
  ScaleFactor := IsotropicScale(Result.T);
  Result.UpdateLW(GetRealTypeAttr(
    Attributes.Values['stroke-width'], 1, fScale)
    * ScaleFactor);
  if Attributes.IndexOfName('font-size') >= 0 then
    Result.FontHeight :=
      GetRealTypeAttr(Attributes.Values['font-size'], 3.5278,
      fScale);
  if Attributes.IndexOfName('font-family') >= 0 then
    Result.FontFamily :=
      CSV_Item(Attributes.Values['font-family'], 1);
  if Attributes.IndexOfName('font-weight') >= 0 then
    if Attributes.Values['font-weight'] = 'bold' then
      fState.FontStyle := fState.FontStyle + [fsBold];
  if Attributes.IndexOfName('font-style') >= 0 then
    if Attributes.Values['font-style'] = 'italic' then
      fState.FontStyle := fState.FontStyle + [fsItalic];
  if Attributes.IndexOfName('text-anchor') >= 0 then
    Result.HJustification := THJustification(CSV_Find(
      'start,middle,end',
      Attributes.Values['text-anchor']) - 1);
end;

procedure T_SVG_Import.SetAttributes;
begin
  if fCurrPrimitive = nil then Exit;
  fState := fStack.Peek as T_SVG_Import_State;
  LC(fState.LineColor);
  LW(fState.LineWidth);
  LI(fState.LineStyle);
  Ha(fState.Hatching);
  Fill(fState.FillColor);
  if not IsSameTransform2D(fState.T, IdentityTransf2D) then
    fCurrPrimitive.TransForm(fState.T);
end;

procedure T_SVG_Import.Correct_LineWidth;
//Correct libplot line width for SVG
var
  Tmp: TObject2D;
  Prim: TPrimitive2D;
  TmpIter: TExclusiveGraphicObjIterator;
  MinLW, LW: TRealType;
begin
  TmpIter := fLst.GetExclusiveIterator;
  MinLW := 0;
  try
    Tmp := TmpIter.First as TObject2D;
    while Tmp <> nil do
    begin
      if (Tmp is TPrimitive2D) then
      begin
        Prim := Tmp as TPrimitive2D;
        if not (Prim is TText2D)
          and (Prim.LineStyle <> liNone) then
        begin
          LW := Prim.LineWidth;
          if (LW > 0) and ((LW < MinLW) or (MinLW = 0))
            then MinLW := LW;
        end;
      end;
      Tmp := TmpIter.Next as TObject2D;
    end;
  finally
    TmpIter.Free;
  end;
  if (MinLW > 0) and ((MinLW < 0.1) or (MinLW > 10)) then
  begin
    Scale_LineWidth(1 / MinLW);
    fDrawing2D.LineWidthBase := fDrawing2D.LineWidthBase * MinLW;
  end;
end;

procedure T_SVG_Import.ParseFromStream(const Stream: TStream);
var
  T: TTransf2D;
  OnChangeDrawing0: TOnChangeDrawing;
  ScaleLineWidth0: Boolean;
  Ext: TRect2D;
  SX, SY: TRealType;
  procedure DeleteLibplotRect;
  var
    TmpObj: TGraphicObject;
  begin
    if fLst.Count = 0 then Exit;
    TmpObj := fLst.Find(0);
    if not (TmpObj is TRectangle2D) then Exit;
    if (TmpObj as TRectangle2D).Points.Count < 3 then Exit;
    if not IsSamePoint2D((TmpObj as TRectangle2D).Points[0],
      Point2D(0, 0))
      then Exit;
    if not IsSamePoint2D((TmpObj as TRectangle2D).Points[1],
      Point2D(1, 1))
      then Exit;
    fLst.Delete(0);
  end;
begin
  OnChangeDrawing0 := fDrawing2D.OnChangeDrawing;
  fDrawing2D.OnChangeDrawing := nil;
  ScaleLineWidth0 := ScaleLineWidth;
  ScaleLineWidth := False;
  try
    fSAX.ParseStream(Stream);
    DeleteLibplotRect;
    T := MultiplyTransform2D(
      Flip2D(Point2D(0, 0), V2D(0, 1)),
      Translate2D(0, viewBox_Y + viewBox_H));
    if Abs(fDrawing2D.PicScale / 203.2 - 1) < 1E-4 then
    begin
      T := MultiplyTransform2D(T, Scale2D(203.2, 203.2));
      fDrawing2D.PicScale := 1;
    end;
    Correct_LineWidth;
    fLst.TransForm(T);
    fDrawing2D.AddList(fLst);
    if (viewBox_W = 0) and (viewBox_H = 0) then
    begin
      Ext := fDrawing2D.DrawingExtension;
      SX := W_MM / (Ext.Right - Ext.Left);
      SY := H_MM / (Ext.Top - Ext.Bottom);
      if SX > SY then SX := SY;
      fDrawing2D.PicScale := SX;
    end;
  finally
    fDrawing2D.OnChangeDrawing := OnChangeDrawing0;
    ScaleLineWidth := ScaleLineWidth0;
  end;
  fDrawing2D.NotifyChanged;
end;

procedure DecompressGzFile(const FileName: string;
  const OutStream: TStream);
const
  BufferSize = 4096;
var
  Len: Integer;
  zfile: gzFile;
  Buffer: array[0..BufferSize - 1] of Byte;
begin
  zfile := gzopen(FileName, 'r');
  if (zfile = nil) then Exit; //Error
  try
    while True do begin
      Len := gzread(zfile, @Buffer, BufferSize);
      //if (Len < 0) then Len := Len div (Len * 0); //Error
      if (Len = 0) then Break;
      OutStream.WriteBuffer(Buffer, Len);
    end;
  finally
    gzclose(zfile);
  end;
  OutStream.Seek(0, soFromBeginning);
end;

procedure T_SVG_Import.ParseFromFile(const FileName: string);
var
  fStream: TFileStream;
  TmpStream: TMemoryStream;
begin
  if LowerCase(ExtractFileExt(FileName)) <> '.svgz' then
  begin
    fStream := TFileStream.Create(FileName, fmOpenRead);
    try
      ParseFromStream(fStream);
    finally
      fStream.Free;
    end;
    Exit;
  end;
//  Exit;
  TmpStream := TMemoryStream.Create;
  try
    DecompressGzFile(FileName, TmpStream);
    ParseFromStream(TmpStream);
  finally
    TmpStream.Free;
  end;
end;

end.

