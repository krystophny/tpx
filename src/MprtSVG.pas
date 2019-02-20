unit MprtSVG;

{$MODE Delphi}

// SVG import module

interface

uses Types, SysUtils, Classes, Contnrs, Graphics, StrUtils,
  Drawings, GObjBase, GObjects,
{$IFNDEF FPC}
  WinBasic,
{$ELSE}
  LazBasic,
{$ENDIF}
  PrimSAX, Geometry, Devices, Input;

type

  T_SVG_Import_State = class
    Scale: TRealType; //(user units measured in mm)
    viewBox_X, viewBox_Y, viewBox_W, viewBox_H,
      W_MM, H_MM: TRealType;
    IsInDefs: Boolean;
    IsInStyle: Boolean;
    ID: string;
    LineColor: TColor;
    LineWidth: TRealType;
    LineStyle: TLineStyle;
    Hatching: THatching;
    HatchColor: TColor;
    FillColor: TColor;
    FillOpacity: TRealType;
    T: TTransf2D;
    FontHeight: TRealType;
    FontFamily: string;
    FontStyle: TFontStyles;
    HAlignment: THAlignment;
    TextPos: TPoint2D;
    Text: string;
    GradientColor: TColor;
    GradientStopOffset: TRealType;
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
    fPathAttributes: TAttributes;
    fStack: TObjectStack;
    fStyleAttributes: TAttributes;
    fCurrState: T_SVG_Import_State;
    fCSS: TAttributes;
    fEntities: TAttributes;
    fNamedObjects: TStringList;
    fInvisibleList: TGraphicObjList;
    fGradientColors: TStringList;
    procedure StartElement(var E: TPrimElement);
    procedure SAX_Comment(const Comment: string);
    //function TransfPoint(P: TPoint2D): TPoint2D;
    procedure CreateNewState;
    procedure EndStateProc(var E: TPrimElement);
    procedure DefsStartProc(var E: TPrimElement);
    procedure DefsEndProc(var E: TPrimElement);
    procedure UseProc(var E: TPrimElement);
    procedure StyleStartProc(var E: TPrimElement);
    procedure StyleEndProc(var E: TPrimElement);
    procedure GradientStartProc(var E: TPrimElement);
    procedure GradientEndProc(var E: TPrimElement);
    procedure GradientStopProc(var E: TPrimElement);
    procedure CDATAProc(const CDATA: string);
    procedure Entities_Proc(const DOCTYPE: string;
      const Attributes: TAttributes);
    function ReplaceEntities(const St: string): string;
    procedure SVGProc(var E: TPrimElement);
    procedure DescProc(var E: TPrimElement);
    procedure TitleProc(var E: TPrimElement);
    procedure StartGroupProc(var E: TPrimElement);
    procedure EndGroupProc(var E: TPrimElement);
    procedure StartSymbolProc(var E: TPrimElement);
    procedure EndSymbolProc(var E: TPrimElement);
    procedure StartClipPathProc(var E: TPrimElement);
    procedure EndClipPathProc(var E: TPrimElement);
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
    procedure StoreAttributes(
      const Attributes: TAttributes; const Tag: string);
    procedure SetAttributes;
    procedure Correct_LineWidth;
  public
    constructor Create(Drawing: TDrawing2D);
    destructor Destroy; override;
    procedure ParseFromStream(const Stream: TStream);
    procedure ParseFromFile(const FileName: string);
  end;

implementation

uses MiscUtils, SysBasic, ColorEtc, gzio, Modify;

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
  except
  end;
//  if Result = clNone then MessageBoxError(ClStr);
end;

{ --================ T_SVG_Import_State ==================-- }

procedure T_SVG_Import_State.Inherit(const State:
  T_SVG_Import_State);
begin
  Scale := State.Scale;
  viewBox_X := State.viewBox_X;
  viewBox_Y := State.viewBox_Y;
  viewBox_W := State.viewBox_W;
  viewBox_H := State.viewBox_H;
  W_MM := State.W_MM;
  H_MM := State.H_MM;
  IsInDefs := State.IsInDefs;
  IsInStyle := State.IsInStyle;
  if State.LineColor <> clNone then LineColor := State.LineColor;
  if State.LineWidth > 0 then LineWidth := State.LineWidth;
  LineStyle := State.LineStyle;
  Hatching := State.Hatching;
  if State.HatchColor <> clNone then
    HatchColor := State.HatchColor;
  if State.FillColor <> clNone then FillColor := State.FillColor;
  FillOpacity := State.FillOpacity;
  T := State.T;
  if State.FontHeight > 0 then FontHeight := State.FontHeight;
  FontFamily := State.FontFamily;
  FontStyle := State.FontStyle;
  HAlignment := State.HAlignment;
  TextPos := State.TextPos;
  Text := State.Text;
end;

procedure T_SVG_Import_State.UpdateLC(const LineColor: TColor);
begin
  if LineColor <> clNone then Self.LineColor := LineColor;
end;

procedure T_SVG_Import_State.UpdateLW(const LineWidth: TRealType);
begin
  if LineWidth > 0 then Self.LineWidth := LineWidth;
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
  if HatchColor <> clNone then Self.HatchColor := HatchColor;
end;

procedure T_SVG_Import_State.UpdateFill(const FillColor: TColor);
begin
  if FillColor <> clNone then Self.FillColor := FillColor;
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
  AddProc('defs', DefsStartProc, DefsEndProc);
  AddProc('use', nil, UseProc);
  AddProc('style', StyleStartProc, StyleEndProc);
  AddProc('linearGradient', GradientStartProc, GradientEndProc);
  AddProc('radialGradient', GradientStartProc, GradientEndProc);
  AddProc('stop', GradientStopProc, nil);
  AddProc('desc', nil, DescProc);
  AddProc('title', nil, TitleProc);
  AddProc('g', StartGroupProc, EndGroupProc);
  AddProc('a', StartGroupProc, EndGroupProc);
  AddProc('symbol', StartSymbolProc, EndSymbolProc);
  AddProc('clipPath', StartClipPathProc, EndClipPathProc);
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
  fSAX.OnCDATA := CDATAProc;
  fSAX.OnDOCTYPE := Entities_Proc;
  fPathParser := T_SVG_Path_Parser.Create(
    nil, nil, nil, nil);
  fStack := TObjectStack.Create;
  fStyleAttributes := TAttributes.Create;
  fStyleAttributes.Delimiter := ';';
  fCSS := TAttributes.Create;
  fEntities := TAttributes.Create;
  fNamedObjects := TStringList.Create;
  fInvisibleList := TGraphicObjList.Create;
  fGradientColors := TStringList.Create;
end;

destructor T_SVG_Import.Destroy;
begin
  fSAX.Free;
  fPathParser.Free;
  fStack.Free;
  fStyleAttributes.Free;
  fCSS.Free;
  fEntities.Free;
  fNamedObjects.Free;
  fInvisibleList.Free;
  fGradientColors.Free;
  inherited Destroy;
end;

{function T_SVG_Import.TransfPoint(P: TPoint2D): TPoint2D;
begin
  Result := Point2D(P.X, P.Y);
end;}

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
  if Index in [1, 2, 4, 5, 6, 7, 8] then Result := Result / Scale
  else if (Index = 9) and (Default <> 0)
    then Result := Result * Default;
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

procedure T_SVG_Import.CreateNewState;
var
  OldState: T_SVG_Import_State;
begin
  if fStack.Count > 0 then
    OldState := fStack.Peek as T_SVG_Import_State
  else OldState := nil;
  fCurrState := T_SVG_Import_State.Create;
  fCurrState.IsInDefs := False;
  fCurrState.IsInStyle := False;
  fCurrState.ID := '';
  fCurrState.LineColor := Graphics.clNone; //Graphics.clDefault;
  fCurrState.LineWidth := -1; //1
  fCurrState.LineStyle := liSolid; //liNone;
  fCurrState.Hatching := haNone;
  fCurrState.HatchColor := Graphics.clDefault; // Graphics.clNone;
  fCurrState.FillColor := Graphics.clNone; // Graphics.clBlack;
  fCurrState.FillOpacity := 1;
    //clBlack; //clSilver;Graphics.clDefault;
  fCurrState.T := IdentityTransf2D;
  fCurrState.FontHeight := 10;
  fCurrState.FontFamily := '';
  fCurrState.FontStyle := [];
  fCurrState.HAlignment := ahLeft;
  fCurrState.TextPos := Point2D(0, 0);
  fCurrState.Text := '';
  fStack.Push(fCurrState);
  if Assigned(OldState) then fCurrState.Inherit(OldState);
end;

procedure T_SVG_Import.EndStateProc(var E: TPrimElement);
begin
  fStack.Pop.Free;
  fCurrState := fStack.Peek as T_SVG_Import_State;
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
    X := GetRealTypeAttr(List[0], 0, 1);
    Y := GetRealTypeAttr(List[1], 0, 1);
    W := GetRealTypeAttr(List[2], 0, 1);
    H := GetRealTypeAttr(List[3], 0, 1);
  finally
    List.Free;
  end;
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
  if fCurrState.IsInStyle then
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
  StoreAttributes(E.Attributes, 'svg');
  if E.Attributes.IndexOfName('viewBox') >= 0 then
    ParseViewBox(E.Attributes.Values['viewBox'],
      fCurrState.viewBox_X, fCurrState.viewBox_Y,
      fCurrState.viewBox_W, fCurrState.viewBox_H)
  else
  begin
    fCurrState.viewBox_X := 0; fCurrState.viewBox_Y := 0;
    fCurrState.viewBox_W := 0; fCurrState.viewBox_H := 0;
  end;
  if E.Attributes.IndexOfName('width') >= 0 then
  begin
    fCurrState.W_MM := GetRealTypeAttr(
      E.Attributes.Values['width'], fCurrState.viewBox_W,
      Default_px) * Default_px;
    if Abs(fCurrState.W_MM / Default_px - 1) < 1E-4 then
      fCurrState.W_MM := Default_Size;
  end
  else fCurrState.W_MM := Default_Size;
  if E.Attributes.IndexOfName('height') >= 0 then
  begin
    fCurrState.H_MM := GetRealTypeAttr(
      E.Attributes.Values['height'], fCurrState.viewBox_H,
      Default_px) * Default_px;
    if Abs(fCurrState.H_MM / Default_px - 1) < 1E-4 then
      fCurrState.H_MM := Default_Size;
  end
  else fCurrState.H_MM := Default_Size;
  if (fCurrState.viewBox_W > 0) and (fCurrState.viewBox_H > 0) then
    fCurrState.Scale
      := Sqrt(fCurrState.W_MM / fCurrState.viewBox_W *
      fCurrState.H_MM / fCurrState.viewBox_H)
  else if fCurrState.viewBox_W > 0
    then fCurrState.Scale := fCurrState.W_MM / fCurrState.viewBox_W
  else if fCurrState.viewBox_H > 0
    then fCurrState.Scale := fCurrState.H_MM / fCurrState.viewBox_H
  else fCurrState.Scale := Default_px;
  fDrawing2D.PicScale := fCurrState.Scale;
  fDrawing2D.LineWidthBase := fCurrState.Scale;
  //width="8in" height="8in" viewBox="0 0 1 1"
end;

procedure T_SVG_Import.DefsStartProc(var E: TPrimElement);
begin
  fCurrState.IsInDefs := True;
  NewCurrentList(fInvisibleList);
end;

procedure T_SVG_Import.DefsEndProc(var E: TPrimElement);
begin
  fCurrState.IsInDefs := False;
  RestoreList;
end;

procedure ChangeUndefinedLineColor(const Obj: TGraphicObject;
  PData: Pointer);
begin
  if Obj is TPrimitive2D then
    if (Obj as TPrimitive2D).LineColor = clNone then
      (Obj as TPrimitive2D).LineColor := TColor(PData^);
end;

procedure ChangeUndefinedLineWidth(const Obj: TGraphicObject;
  PData: Pointer);
begin
  if Obj is TPrimitive2D then
    if (Obj as TPrimitive2D).LineWidth < 0 then
      (Obj as TPrimitive2D).LineWidth := -TRealType(PData^)
        * (Obj as TPrimitive2D).LineWidth
    else if (Obj as TPrimitive2D).LineWidth = 0 then
      (Obj as TPrimitive2D).LineWidth := TRealType(PData^);
end;

procedure ChangeUndefinedFillColor(const Obj: TGraphicObject;
  PData: Pointer);
begin
  if Obj is TPrimitive2D then
    if (Obj as TPrimitive2D).FillColor = clNone then
      (Obj as TPrimitive2D).FillColor := TColor(PData^);
end;

procedure T_SVG_Import.UseProc(var E: TPrimElement);
var
  ID: string;
  I: Integer;
  X, Y: TRealType;
  procedure LC_Use(const Cl: TColor; Obj: TObject2D = nil);
  begin
    if Cl = clNone then Exit;
    if GetPrim(Obj) then
    begin
      ChangeUndefinedLineColor(Obj, @Cl);
      Exit;
    end;
    if GetGroup(Obj) then
    begin
      ChangeObjects(
        (Obj as TGroup2D).Objects, ChangeUndefinedLineColor, @Cl);
      Exit;
    end;
  end;
  procedure LW_Use(
    const W: TRealType; Obj: TObject2D = nil);
  begin
    if W <= 0 then Exit;
    if GetPrim(Obj) then
    begin
      ChangeUndefinedLineWidth(Obj, @W);
      Exit;
    end;
    if GetGroup(Obj) then
    begin
      ChangeObjects(
        (Obj as TGroup2D).Objects, ChangeUndefinedLineWidth, @W);
      Exit;
    end;
  end;
  procedure Fill_Use(
    const Cl: TColor; Obj: TObject2D = nil);
  begin
    if Cl = clNone then Exit;
    if GetPrim(Obj) then
    begin
      ChangeUndefinedFillColor(Obj, @Cl);
      Exit;
    end;
    if GetGroup(Obj) then
    begin
      ChangeObjects(
        (Obj as TGroup2D).Objects, ChangeUndefinedFillColor, @Cl);
      Exit;
    end;
  end;
begin
  if E.Attributes.IndexOfName('xlink:href') >= 0
    then ID := E.Attributes.Values['xlink:href']
  else Exit;
  Delete(ID, 1, 1);
  I := fNamedObjects.IndexOf(ID);
  if I < 0 then Exit;
  fCurrObj := fNamedObjects.Objects[I] as TObject2D;
  fCurrObj := TpXFindClassByName(
    fCurrObj.ClassName).CreateDupe(fCurrObj) as TObject2D;
  AddObj(fCurrObj);
  if E.Attributes.IndexOfName('x') >= 0 then
  begin
    X := StrToFloat(E.Attributes.Values['x']);
    if E.Attributes.IndexOfName('y') >= 0 then
      Y := StrToFloat(E.Attributes.Values['y'])
    else Y := 0;
    fCurrObj.Transform(Translate2D(X, Y));
  end;
  StoreAttributes(E.Attributes, 'use');
  //SetAttributes;
  if fCurrObj = nil then Exit;
  if fCurrState.ID <> '' then
    fNamedObjects.AddObject(fCurrState.ID, fCurrObj);
  LC_Use(fCurrState.LineColor);
  LW_Use(fCurrState.LineWidth);
  if fCurrState.LineColor <> clNone
    then LI(fCurrState.LineStyle)
  else LI(liNone);
  Ha(fCurrState.Hatching);
  Fill_Use(fCurrState.FillColor);
  if not IsSameTransform2D(fCurrState.T, IdentityTransf2D) then
    fCurrObj.Transform(fCurrState.T);
  EndStateProc(E);
end;

procedure T_SVG_Import.StyleStartProc(var E: TPrimElement);
begin
  fCurrState.IsInStyle := True;
end;

procedure T_SVG_Import.StyleEndProc(var E: TPrimElement);
begin
  fCurrState.IsInStyle := False;
end;

procedure T_SVG_Import.GradientStartProc(var E: TPrimElement);
begin
  StoreAttributes(E.Attributes, '');
  fCurrState.GradientStopOffset := 0;
  fCurrState.GradientColor := clRed;
end;

procedure T_SVG_Import.GradientEndProc(var E: TPrimElement);
var
  LinkID: string;
begin
  if E.Attributes.IndexOfName('xlink:href') >= 0 then
  begin
    LinkID := E.Attributes.Values['xlink:href'];
    Delete(LinkID, 1, 1);
    if fGradientColors.IndexOfName(LinkID) >= 0 then
      fCurrState.GradientColor
        := HtmlToColor(fGradientColors.Values[LinkID])
    else fCurrState.GradientColor := clRed;
  end;
  if E.Attributes.IndexOfName('id') >= 0 then
  begin
    fGradientColors.Values[E.Attributes.Values['id']]
      := ColorToHtml(fCurrState.GradientColor);
  end;
  EndStateProc(E);
end;

procedure T_SVG_Import.GradientStopProc(var E: TPrimElement);
var
  Offset: TRealType;
  Color: TColor;
begin
  if fCurrState.GradientStopOffset = -1 then Exit;
  if E.Attributes.IndexOfName('style') >= 0 then
  begin
    ParseStyleAttributes(ReplaceEntities(E.Attributes.Values['style']));
    E.Attributes.AddStrings(fStyleAttributes);
    E.Attributes.Delete(E.Attributes.IndexOfName('style'));
  end;
  if E.Attributes.IndexOfName('offset') >= 0 then
    Offset :=
      GetRealTypeAttr(E.Attributes.Values['offset'], 0, 1)
  else Exit;
  if E.Attributes.IndexOfName('stop-color') >= 0 then
  begin
    if (E.Attributes.Values['stop-color'] = 'currentColor')
      or (E.Attributes.Values['stop-color'] = 'none')
      or (Pos('url(', E.Attributes.Values['stop-color']) > 0)
      then Exit;
    Color := SVG_Color(E.Attributes.Values['stop-color']);
  end;
  if Offset < 0.5 then
  begin
    fCurrState.GradientStopOffset := Offset;
    fCurrState.GradientColor := Color;
    Exit;
  end;
  // An approximate fill color to use instead of a gradient
  if Offset > 0.5 then
    fCurrState.GradientColor :=
      MixColors(fCurrState.GradientColor, Color,
      (Offset - 0.5) / (Offset - fCurrState.GradientStopOffset))
  else fCurrState.GradientColor := Color;
  fCurrState.GradientStopOffset := -1;
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
  StartGroup;
end;

procedure T_SVG_Import.EndGroupProc(var E: TPrimElement);
begin
  FinishGroup;
  if (fCurrState.ID <> '') and Assigned(fCurrObj) then
    fNamedObjects.AddObject(fCurrState.ID, fCurrObj);
  EndStateProc(E);
end;

procedure T_SVG_Import.StartSymbolProc(var E: TPrimElement);
begin
  StoreAttributes(E.Attributes, 'symbol');
  NewCurrentList(fInvisibleList);
  StartGroup;
end;

procedure T_SVG_Import.EndSymbolProc(var E: TPrimElement);
begin
  FinishGroup;
  if (fCurrState.ID <> '') and Assigned(fCurrObj) then
    fNamedObjects.AddObject(fCurrState.ID, fCurrObj);
  RestoreList;
  EndStateProc(E);
end;

procedure T_SVG_Import.StartClipPathProc(var E: TPrimElement);
begin
  CreateNewState;
  NewCurrentList(fInvisibleList);
end;

procedure T_SVG_Import.EndClipPathProc(var E: TPrimElement);
begin
  RestoreList;
  EndStateProc(E);
end;

procedure T_SVG_Import.PathProc(var E: TPrimElement);
var
  PathData: string;
  Compound: TCompound2D;
  Group: TGroup2D;
  TmpID: string;
begin
  StoreAttributes(E.Attributes, 'path');
  if E.Attributes.IndexOfName('d') < 0 then Exit;
  PathData := E.Attributes.Values['d'];
  fPathAttributes := E.Attributes;
  Compound := AddCompound as TCompound2D;
  Compound.Points.DisableEvents := True;
  fPathParser.FillGenPath(PathData, Compound.GenPath);
  Compound.Points.DisableEvents := False;
  TmpID := fCurrState.ID;
  fCurrState.ID := '';
  SetAttributes;
  fCurrState.ID := TmpID;
  Group := TGroup2D.Create(-1);
  fCurrentList.Pop;
  UncompoundCompound(Compound, Group.Objects);
  Compound.Free;
  if Group.Objects.Count = 1 then
  begin
    Group.Objects.FreeOnDelete := False;
    AddObj(Group.Objects.Peek as TObject2D);
    Group.Free;
  end
  else
  begin
    AddObj(Group);
  end;
  if fCurrState.ID <> '' then
    fNamedObjects.AddObject(fCurrState.ID, fCurrObj);
{
  TmpObjList: TGraphicObjList;
  //if N = 1 then
  begin
    fCurrentList.Pop;
    TmpObjList := TGraphicObjList.Create;
    TmpObjList.FreeOnDelete := False;
    try
      UncompoundCompound(Compound, TmpObjList);
      fCurrentList.AddFromList(TmpObjList);
    finally
      TmpObjList.Free;
    end;
    fCurrObj := fCurrentList.Peek as TObject2D;
    Compound.Free;
  end;}
end;

procedure T_SVG_Import.TextStartProc(var E: TPrimElement);
begin
  StoreAttributes(E.Attributes, 'text');
  fCurrState.TextPos := Point2D(
    GetRealTypeAttr(
    E.Attributes.Values['x'], 0, fCurrState.Scale)
    + GetRealTypeAttr(
    E.Attributes.Values['dx'], 0, fCurrState.Scale),
    GetRealTypeAttr(
    E.Attributes.Values['y'], 0, fCurrState.Scale)
    + GetRealTypeAttr(
    E.Attributes.Values['dy'], 0, fCurrState.Scale));
end;

procedure T_SVG_Import.TextEndProc(var E: TPrimElement);
begin
  fCurrState.Text :=
    fCurrState.Text + ReplaceEntities(Trim(E.Text));
  if fCurrState.Text = '' then
  begin
    EndStateProc(E);
    Exit;
  end;
  AddText(fCurrState.TextPos.X, fCurrState.TextPos.Y,
    fCurrState.FontHeight, fCurrState.Text);
  (fCurrObj as TText2D).HAlignment :=
    fCurrState.HAlignment;
  if HasFontFamily(fCurrState.FontFamily) then
  begin
    (fCurrObj as TText2D).Font.Name := fCurrState.FontFamily;
    (fCurrObj as TText2D).Font.style := fCurrState.FontStyle;
  end;
  SetAttributes;
  (fCurrObj as TPrimitive2D).LineColor := (fCurrObj as
    TPrimitive2D).FillColor;
  (fCurrObj as TPrimitive2D).FillColor := Graphics.clNone;
  EndStateProc(E);
end;

procedure T_SVG_Import.TSpanStartProc(var E: TPrimElement);
begin
  StoreAttributes(E.Attributes, 'tspan');
  fCurrState.TextPos := Point2D(
    GetRealTypeAttr(
    E.Attributes.Values['x'], fCurrState.TextPos.X,
    fCurrState.Scale)
    + GetRealTypeAttr(E.Attributes.Values['dx'], 0,
    fCurrState.Scale),
    GetRealTypeAttr(
    E.Attributes.Values['y'], fCurrState.TextPos.Y,
    fCurrState.Scale)
    + GetRealTypeAttr(E.Attributes.Values['dy'], 0,
    fCurrState.Scale));
end;

procedure T_SVG_Import.TSpanEndProc(var E: TPrimElement);
begin
  fCurrState.Text :=
    fCurrState.Text + ReplaceEntities(Trim(E.Text));
  if fCurrState.Text = '' then
  begin
    EndStateProc(E);
    Exit;
  end;
  AddText(fCurrState.TextPos.X, fCurrState.TextPos.Y,
    fCurrState.FontHeight, fCurrState.Text);
  (fCurrObj as TText2D).HAlignment :=
    fCurrState.HAlignment;
  if HasFontFamily(fCurrState.FontFamily) then
  begin
    (fCurrObj as TText2D).Font.Name := fCurrState.FontFamily;
    (fCurrObj as TText2D).Font.style := fCurrState.FontStyle;
  end;
  SetAttributes;
  (fCurrObj as TPrimitive2D).LineColor := (fCurrObj as
    TPrimitive2D).FillColor;
  (fCurrObj as TPrimitive2D).FillColor := Graphics.clNone;
  EndStateProc(E);
end;

procedure T_SVG_Import.textPathStartProc(var E: TPrimElement);
begin
  CreateNewState;
end;

procedure T_SVG_Import.textPathEndProc(var E: TPrimElement);
begin
  fCurrState.Text :=
    fCurrState.Text + ReplaceEntities(Trim(E.Text));
  EndStateProc(E);
end;

procedure T_SVG_Import.LineProc(var E: TPrimElement);
var
  X1, Y1, X2, Y2: TRealType;
begin
  StoreAttributes(E.Attributes, 'line');
  X1 := GetRealTypeAttr(
    E.Attributes.Values['x1'], 0, fCurrState.Scale);
  Y1 := GetRealTypeAttr(
    E.Attributes.Values['y1'], 0, fCurrState.Scale);
  X2 := GetRealTypeAttr(
    E.Attributes.Values['x2'], 0, fCurrState.Scale);
  Y2 := GetRealTypeAttr(
    E.Attributes.Values['y2'], 0, fCurrState.Scale);
  AddLine(X1, Y1, X2, Y2);
  SetAttributes;
end;

procedure T_SVG_Import.RectProc(var E: TPrimElement);
var
  X, Y, W, H, RX, RY, S: TRealType;
begin
  StoreAttributes(E.Attributes, 'rect');
  X := GetRealTypeAttr(
    E.Attributes.Values['x'], 0, fCurrState.Scale);
  Y := GetRealTypeAttr(
    E.Attributes.Values['y'], 0, fCurrState.Scale);
  RX := GetRealTypeAttr(
    E.Attributes.Values['rx'], 0, fCurrState.Scale);
  RY := GetRealTypeAttr(
    E.Attributes.Values['ry'], RX, fCurrState.Scale);
  W := GetRealTypeAttr(
    E.Attributes.Values['width'], fCurrState.viewBox_W,
    fCurrState.Scale);
  H := GetRealTypeAttr(
    E.Attributes.Values['height'], fCurrState.viewBox_H,
    fCurrState.Scale);
  S := Abs(fCurrState.T[1, 1]) + Abs(fCurrState.T[1, 2]) +
    Abs(fCurrState.T[2, 1]) + Abs(fCurrState.T[2, 2]);
  S := Abs(fCurrState.T[1, 1] * fCurrState.T[2, 1]
    - fCurrState.T[1, 2] * fCurrState.T[2, 2]) / Sqr(S);
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
  StoreAttributes(E.Attributes, 'circle');
  S := Abs(fCurrState.T[1, 1]) + Abs(fCurrState.T[1, 2]) +
    Abs(fCurrState.T[2, 1]) + Abs(fCurrState.T[2, 2]);
  S := (Sqr(fCurrState.T[1, 1]) - Sqr(fCurrState.T[2, 1])
    + Sqr(fCurrState.T[1, 2]) - Sqr(fCurrState.T[2, 2])) / Sqr(S);
  if S > 1E-10 {anisotropic transform} then
    AddEllipseAsBezier(
      GetRealTypeAttr(
      E.Attributes.Values['cx'], 0, fCurrState.Scale),
      GetRealTypeAttr(
      E.Attributes.Values['cy'], 0, fCurrState.Scale),
      GetRealTypeAttr(
      E.Attributes.Values['r'], 0, fCurrState.Scale),
      GetRealTypeAttr(
      E.Attributes.Values['r'], 0, fCurrState.Scale))
  else
    AddCircle(
      GetRealTypeAttr(
      E.Attributes.Values['cx'], 0, fCurrState.Scale),
      GetRealTypeAttr(
      E.Attributes.Values['cy'], 0, fCurrState.Scale),
      GetRealTypeAttr(
      E.Attributes.Values['r'], 0, fCurrState.Scale));
  SetAttributes;
end;

procedure T_SVG_Import.EllipseProc(var E: TPrimElement);
var
  S: TRealType;
begin
  StoreAttributes(E.Attributes, 'ellipse');
  S := Abs(fCurrState.T[1, 1]) + Abs(fCurrState.T[1, 2]) +
    Abs(fCurrState.T[2, 1]) + Abs(fCurrState.T[2, 2]);
  S := Abs(fCurrState.T[1, 1] * fCurrState.T[2, 1]
    - fCurrState.T[1, 2] * fCurrState.T[2, 2]) / Sqr(S);
  if S > 1E-10 {skewed} then
    AddEllipseAsBezier(
      GetRealTypeAttr(
      E.Attributes.Values['cx'], 0, fCurrState.Scale),
      GetRealTypeAttr(
      E.Attributes.Values['cy'], 0, fCurrState.Scale),
      GetRealTypeAttr(
      E.Attributes.Values['rx'], 0, fCurrState.Scale),
      GetRealTypeAttr(
      E.Attributes.Values['ry'], 0, fCurrState.Scale))
  else
    AddEllipse(
      GetRealTypeAttr(
      E.Attributes.Values['cx'], 0, fCurrState.Scale),
      GetRealTypeAttr(
      E.Attributes.Values['cy'], 0, fCurrState.Scale),
      GetRealTypeAttr(
      E.Attributes.Values['rx'], 0, fCurrState.Scale),
      GetRealTypeAttr(
      E.Attributes.Values['ry'], 0, fCurrState.Scale));
  SetAttributes;
end;

procedure T_SVG_Import.PolygonProc(var E: TPrimElement);
begin
  if E.Attributes.IndexOfName('points') < 0 then Exit;
  StoreAttributes(E.Attributes, 'polygon');
  AddPolygon;
  FillPoints(fCurrObj as TPrimitive2D,
    E.Attributes.Values['points'],
    (fCurrObj as TPrimitive2D).Points);
  SetAttributes;
end;

procedure T_SVG_Import.PolylineProc(var E: TPrimElement);
begin
  StoreAttributes(E.Attributes, 'polyline');
  if E.Attributes.IndexOfName('points') < 0 then Exit;
  AddPolyline;
  FillPoints(fCurrObj as TPrimitive2D,
    E.Attributes.Values['points'],
    (fCurrObj as TPrimitive2D).Points);
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

procedure T_SVG_Import.StoreAttributes(
  const Attributes: TAttributes; const Tag: string);
var
  St, CurrentColor: string;
  ScaleFactor, A: TRealType;
  AClass, Classes, CSS_Style: string;
  I: Integer;
begin
  CreateNewState;
  if Attributes.IndexOfName('id') >= 0 then
    fCurrState.ID := Attributes.Values['id']
  else fCurrState.ID := '';
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
        St := Trim(St);
        if St <> '' then
          if St[Length(St)] <> ';' then St := St + ';';
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
      fCurrState.UpdateHA(haNone);
      fCurrState.UpdateFill(Graphics.clDefault)
    end
    else if Attributes.Values['fill'] = 'currentColor' then
    begin
      fCurrState.UpdateHA(haNone);
      fCurrState.UpdateFill(clLtGray)
    end
    else if Pos('url(', Attributes.Values['fill']) > 0 then
    begin
      St := Trim(Attributes.Values['fill']);
      Delete(St, 1, 4);
      Delete(St, Length(St), 1);
      St := Trim(St);
      Delete(St, 1, 1);
      if fGradientColors.IndexOfName(St) >= 0 then
        fCurrState.UpdateFill(HtmlToColor(fGradientColors.Values[St]))
      else
      begin
        fCurrState.UpdateHA(haDiagCross);
        fCurrState.UpdateFill(Graphics.clDefault);
      end;
    end
    else
    begin
      fCurrState.UpdateHA(haNone);
      fCurrState.UpdateFill(SVG_Color(Attributes.Values['fill']));
    end;
  end;
  if Attributes.IndexOfName('fill-opacity') >= 0 then
    fCurrState.FillOpacity :=
      GetRealTypeAttr(Attributes.Values['fill-opacity'], 1, 1);
  if fCurrState.FillOpacity < 0.1
    then fCurrState.UpdateFill(clDefault);
  if Attributes.IndexOfName('stroke') >= 0 then
  begin
    if Attributes.Values['stroke'] = 'currentColor' then
      Attributes.Values['stroke'] := CurrentColor;
    if Attributes.Values['stroke'] = 'none'
      then fCurrState.UpdateLI(liNone)
    else if Pos('url(', Attributes.Values['stroke']) > 0 then
    begin
      fCurrState.UpdateLI(liDashed);
      fCurrState.UpdateLC(clLtGray);
    end
    else
    begin
      fCurrState.UpdateLI(liSolid);
      fCurrState.UpdateLC(SVG_Color(Attributes.Values['stroke']));
    end;
    if Attributes.IndexOfName('stroke-dasharray') >= 0 then // ??
      if Attributes.Values['stroke-dasharray'] <> 'none' then
        fCurrState.UpdateLI(liDashed);
  end;
  if Attributes.IndexOfName('transform') >= 0 then
  begin
    St := Attributes.Values['transform'];
    if St <> '' then
      fCurrState.UpdateT(SVG_Transform_Parse(Trim(St)));
  end;
  ScaleFactor := IsotropicScale(fCurrState.T);
  if Attributes.IndexOfName('stroke-width') >= 0 then
    fCurrState.UpdateLW(GetRealTypeAttr(
      Attributes.Values['stroke-width'], 1, fCurrState.Scale)
      * ScaleFactor);
  if Attributes.IndexOfName('font-size') >= 0 then
    fCurrState.FontHeight :=
      GetRealTypeAttr(Attributes.Values['font-size'], 3.5278,
      fCurrState.Scale);
  if Attributes.IndexOfName('font-family') >= 0 then
    fCurrState.FontFamily :=
      CSV_Item(Attributes.Values['font-family'], 1);
  if Attributes.IndexOfName('font-weight') >= 0 then
    if Attributes.Values['font-weight'] = 'bold' then
      fCurrState.FontStyle := fCurrState.FontStyle + [fsBold];
  if Attributes.IndexOfName('font-style') >= 0 then
    if Attributes.Values['font-style'] = 'italic' then
      fCurrState.FontStyle := fCurrState.FontStyle + [fsItalic];
  if Attributes.IndexOfName('text-anchor') >= 0 then
    fCurrState.HAlignment :=
      THAlignment(CSV_Find('start,middle,end',
      Attributes.Values['text-anchor']) - 1);
end;

procedure T_SVG_Import.SetAttributes;
begin
  if fCurrObj = nil then Exit;
  if fCurrState.ID <> '' then
    fNamedObjects.AddObject(fCurrState.ID, fCurrObj);
  LC(fCurrState.LineColor);
  if fCurrState.LineWidth > 0 then
    LW(fCurrState.LineWidth)
  else LW(-IsotropicScale(fCurrState.T)); // * fCurrState.Scale
  if fCurrState.LineColor <> clNone
    then LI(fCurrState.LineStyle)
  else LI(liNone);
  Ha(fCurrState.Hatching);
  Fill(fCurrState.FillColor);
  if not IsSameTransform2D(fCurrState.T, IdentityTransf2D) then
    fCurrObj.Transform(fCurrState.T);
end;

procedure T_SVG_Import.Correct_LineWidth;
//Correct libplot line width for SVG
var
  Obj: TGraphicObject;
  Prim: TPrimitive2D;
  MinLW, LW: TRealType;
  procedure CorrectList(const Lst: TGraphicObjList);
  begin
    Obj := Lst.FirstObj;
    while Obj <> nil do
    begin
      if (Obj is TPrimitive2D) then
      begin
        Prim := Obj as TPrimitive2D;
        if not (Prim is TText2D)
          and (Prim.LineStyle <> liNone) then
        begin
          LW := Prim.LineWidth;
          if (LW > 0) and ((LW < MinLW) or (MinLW = 0))
            then MinLW := LW;
        end;
      end
      else if (Obj is TGroup2D) then
        CorrectList((Obj as TGroup2D).Objects);
      Obj := Lst.NextObj;
    end;
  end;
begin
  MinLW := 0;
  CorrectList(fMainList);
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
    if fMainList.Count = 0 then Exit;
    TmpObj := fMainList.Find(0);
    if not (TmpObj is TRectangle2D) then Exit;
    if (TmpObj as TRectangle2D).Points.Count < 3 then Exit;
    if not IsSamePoint2D((TmpObj as TRectangle2D).Points[0],
      Point2D(0, 0))
      then Exit;
    if not IsSamePoint2D((TmpObj as TRectangle2D).Points[1],
      Point2D(1, 1))
      then Exit;
    fMainList.Delete(0);
  end;
  procedure BreakTopGroup;
  var
    Group: TGroup2D;
  begin
    Group := fMainList.Pop as TGroup2D;
    fMainList.AddFromList(Group.Objects);
    Group.Objects.FreeOnDelete := False;
    Group.Free;
  end;
begin
  fDrawing2D.Clear;
  OnChangeDrawing0 := fDrawing2D.OnChangeDrawing;
  fDrawing2D.OnChangeDrawing := nil;
  ScaleLineWidth0 := ScaleLineWidth;
  ScaleLineWidth := False;
  try
    fSAX.ParseStream(Stream);
    DeleteLibplotRect;
    T := MultiplyTransform2D(
      Flip2D(Point2D(0, 0), V2D(0, 1)),
      Translate2D(0, fCurrState.viewBox_Y + fCurrState.viewBox_H));
    if Abs(fDrawing2D.PicScale / 203.2 - 1) < 1E-4 then
    begin
      T := MultiplyTransform2D(T, Scale2D(203.2, 203.2));
      fDrawing2D.PicScale := 1;
    end;
    Correct_LineWidth;
    while fMainList.Peek is TGroup2D do BreakTopGroup;
    fMainList.Transform(T);
    fDrawing2D.AddList(fMainList);
    if (fCurrState.viewBox_W = 0) and (fCurrState.viewBox_H = 0)
      then
    begin
      Ext := fDrawing2D.DrawingExtension;
      if (Ext.Right > Ext.Left) and (Ext.Top > Ext.Bottom) then
      begin
        SX := fCurrState.W_MM / (Ext.Right - Ext.Left);
        SY := fCurrState.H_MM / (Ext.Top - Ext.Bottom);
        if SX > SY then SX := SY;
      end
      else SX := 1;
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
//      if (Len < 0) then Len := Len div (Len * 0); //Error
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

