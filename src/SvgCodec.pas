unit SvgCodec;

{$MODE Delphi}
{$H+}

interface

uses Classes, SysUtils, Geometry, Drawings, GObjBase, GObjects,
  DocumentFormats;

type
  ESvgCodec = class(Exception);
  TSvgDocumentProfile = (sdpEditable, sdpImportOnly);

// The caller supplies a detached drawing. On failure it must discard it.
procedure LoadSvgFromStream(const Stream: TStream; const Candidate: TDrawing2D;
  const CompressedSource: Boolean; out Profile: TSvgDocumentProfile;
  out Diagnostics: string);
procedure LoadSvgzFile(const FileName: string; const Candidate: TDrawing2D;
  out Diagnostics: string);
function LoadSvgDocument(Candidate: TDrawing2D; const FileName: TDocumentPath;
  const SourceBytes: RawByteString; Diagnostics: TStrings): Boolean;
procedure SaveSvgToStream(const Drawing: TDrawing2D;
  const Destination: TStream);
procedure RegisterSvgDocumentCodec;

implementation

uses Math, PrimSAX, MprtSVG, Output, gzio, DocumentIO;

const
  MaxSvgInputBytes = 4 * 1024 * 1024;
  MaxSvgExpandedBytes = 16 * 1024 * 1024;
  MaxSvgDepth = 64;
  MaxSvgElements = 100000;
  MaxPathCommands = 100000;
  MaxSvgIssues = 64;

type
  TSvgViewport = record
    WidthMM, HeightMM: TRealType;
    ViewBoxX, ViewBoxY, ViewBoxWidth, ViewBoxHeight: TRealType;
  end;

  TSvgProfileScan = class
  private
    fDepth: Integer;
    fElements: Integer;
    fRootSeen: Boolean;
    fRootClosed: Boolean;
    fGeneratedCommentSeen: Boolean;
    fTitleCount: Integer;
    fDescCount: Integer;
    fTextPreserve: Boolean;
    fIssueOverflow: Boolean;
    fPhysicalWidth: TRealType;
    fPhysicalHeight: TRealType;
    fViewBoxX: TRealType;
    fViewBoxY: TRealType;
    fViewBoxWidth: TRealType;
    fViewBoxHeight: TRealType;
    fDashArrayPresent: Boolean;
    fDashArrayValid: Boolean;
    fDashSizeUser: TRealType;
    fTransformScaleStack: array[0..MaxSvgDepth] of TRealType;
    fNames: TStringList;
    fIssues: TStringList;
    procedure AddIssue(const Issue: string);
    procedure CheckAttributes(const Tag: string; const Attributes: TAttributes);
    procedure CheckStyle(const Tag, Value: string);
    procedure CheckDashArray(const Tag, Value: string);
    procedure CheckRoot(const Attributes: TAttributes);
    procedure CheckPhysicalLength(const Name, Value: string);
    procedure CheckGeometryLength(const Name, Value: string;
      PositiveOnly: Boolean);
    procedure CheckViewBox(const Value: string);
    procedure ValidateTransform(const Value: string;
      out UniformScale: TRealType);
    procedure ValidatePath(const Value: string);
    procedure StartElement(const Name: string; const Attributes: TAttributes);
    procedure EndElement(const Name: string);
    procedure Text(const Value: string);
    procedure Comment(const Value: string);
    procedure CDATA(const Value: string);
    procedure Doctype(const Name: string; const Attributes: TAttributes);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Scan(const Stream: TStream; out Editable: Boolean;
      out Diagnostics: string);
    procedure GetViewport(out Viewport: TSvgViewport);
    procedure GetDashArray(out Present: Boolean; out SizeUser: TRealType);
  end;

  TSvgCodecContext = class(TObject)
  public
    Viewport: TSvgViewport;
  end;

function ValueIn(const Value: string; const Values: array of string): Boolean;
var
  I: Integer;
begin
  for I := Low(Values) to High(Values) do
    if Value = Values[I] then Exit(True);
  Result := False;
end;

function ParseSvgNumber(const Value, Context: string): TRealType;
var
  Settings: TFormatSettings;
begin
  Settings := DefaultFormatSettings;
  Settings.DecimalSeparator := '.';
  try
    Result := StrToFloat(Value, Settings);
  except
    raise ESvgCodec.Create('Invalid numeric value for ' + Context + ': ' + Value);
  end;
  if IsNan(Result) or IsInfinite(Result) then
    raise ESvgCodec.Create('Non-finite numeric value for ' + Context);
  if Abs(Result) > 1E12 then
    raise ESvgCodec.Create('Numeric value exceeds the supported range for ' + Context);
end;

function InlineStyleValue(const Style, PropertyName: string): string;
var
  Declarations: TStringList;
  I, Sep: Integer;
begin
  Result := '';
  Declarations := TStringList.Create;
  try
    Declarations.Delimiter := ';';
    Declarations.StrictDelimiter := True;
    Declarations.DelimitedText := Style;
    for I := 0 to Declarations.Count - 1 do
    begin
      Sep := Pos(':', Declarations[I]);
      if (Sep > 0) and
        (LowerCase(Trim(Copy(Declarations[I], 1, Sep - 1))) =
          PropertyName) then
      begin
        Result := Trim(Copy(Declarations[I], Sep + 1, MaxInt));
        Exit;
      end;
    end;
  finally
    Declarations.Free;
  end;
end;

procedure ValidatePointList(const Value, Context: string);
var
  Parts: TStringList;
  S: string;
  I, MinParts: Integer;
begin
  Parts := TStringList.Create;
  try
    S := Value;
    ExtractStrings([',', ' ', #9, #10, #13], [], PChar(S), Parts);
    if Context = 'polygon' then MinParts := 6 else MinParts := 4;
    if (Parts.Count < MinParts) or Odd(Parts.Count) then
      raise ESvgCodec.Create('SVG ' + Context + ' points need x/y pairs');
    if Parts.Count > MaxPathCommands * 2 then
      raise ESvgCodec.Create('SVG ' + Context + ' exceeds the point limit');
    for I := 0 to Parts.Count - 1 do
      ParseSvgNumber(Parts[I], Context + ' points');
  finally
    Parts.Free;
  end;
end;

function LocalSvgName(const Name: string): string;
begin
  if Copy(Name, 1, 4) = 'svg:' then
    Result := Copy(Name, 5, MaxInt)
  else
    Result := Name;
end;

function IsSvgName(const Name: string): Boolean;
begin
  if Copy(Name, 1, 4) = 'svg:' then
    Result := Pos(':', Copy(Name, 5, MaxInt)) = 0
  else
    Result := (Name <> '') and (Pos(':', Name) = 0);
end;

function IsSimpleColor(const Value: string): Boolean;
var
  S: string;
  I: Integer;
begin
  S := LowerCase(Trim(Value));
  if ValueIn(S, ['none', 'black', 'white', 'red', 'green', 'blue',
    'yellow', 'gray', 'grey', 'silver', 'maroon', 'olive', 'lime',
    'aqua', 'teal', 'navy', 'fuchsia', 'purple', 'orange']) then
    Exit(True);
  if (Length(S) <> 4) and (Length(S) <> 7) then Exit(False);
  if S[1] <> '#' then Exit(False);
  for I := 2 to Length(S) do
    if not (S[I] in ['0'..'9', 'a'..'f']) then Exit(False);
  Result := True;
end;

constructor TSvgProfileScan.Create;
begin
  inherited Create;
  fNames := TStringList.Create;
  fIssues := TStringList.Create;
  fDashArrayValid := True;
end;

destructor TSvgProfileScan.Destroy;
begin
  fNames.Free;
  fIssues.Free;
  inherited Destroy;
end;

procedure TSvgProfileScan.AddIssue(const Issue: string);
begin
  if (Issue = '') or (fIssues.IndexOf(Issue) >= 0) then Exit;
  if fIssues.Count >= MaxSvgIssues then
    fIssueOverflow := True
  else
    fIssues.Add(Issue);
end;

procedure TSvgProfileScan.CheckStyle(const Tag, Value: string);
var
  Declarations: TStringList;
  I, Sep: Integer;
  Name, V: string;
  TransformScale: TRealType;
begin
  Declarations := TStringList.Create;
  try
    Declarations.Delimiter := ';';
    Declarations.StrictDelimiter := True;
    Declarations.DelimitedText := Value;
    for I := 0 to Declarations.Count - 1 do
    begin
      if Trim(Declarations[I]) = '' then Continue;
      Sep := Pos(':', Declarations[I]);
      if Sep = 0 then
      begin
        AddIssue('malformed inline style');
        Continue;
      end;
      Name := LowerCase(Trim(Copy(Declarations[I], 1, Sep - 1)));
      V := LowerCase(Trim(Copy(Declarations[I], Sep + 1, MaxInt)));
      if not ValueIn(Name, ['fill', 'stroke', 'stroke-width',
        'stroke-dasharray', 'font-family', 'font-size', 'font-weight',
        'font-style', 'text-anchor', 'transform']) then
        AddIssue('unsupported style property ' + Name);
      if (Name = 'fill') or (Name = 'stroke') then
      begin
        if Pos('url(', V) > 0 then AddIssue('resource paint in style')
        else if not IsSimpleColor(V) then
          AddIssue('unsupported color in style ' + Name);
      end;
      if Pos('url(', V) > 0 then AddIssue('resource URL in style');
      if Name = 'transform' then
      begin
        ValidateTransform(Copy(Declarations[I], Sep + 1, MaxInt),
          TransformScale);
      end;
      if Name = 'stroke-dasharray' then CheckDashArray(Tag, V);
      if (Name = 'font-weight') and not ValueIn(V, ['normal', 'bold']) then
        AddIssue('unsupported font-weight ' + V);
      if (Name = 'font-style') and not ValueIn(V, ['normal', 'italic']) then
        AddIssue('unsupported font-style ' + V);
      if (Name = 'font-family') and (Pos(',', V) > 0) then
        AddIssue('font-family fallback list is not retained');
      if (Name = 'text-anchor') and
        not ValueIn(V, ['start', 'middle', 'end']) then
        AddIssue('unsupported text-anchor ' + V);
      if (Name = 'stroke-width') or (Name = 'font-size') then
        CheckGeometryLength(Name, Copy(Declarations[I], Sep + 1, MaxInt),
          Name = 'font-size');
    end;
  finally
    Declarations.Free;
  end;
end;

procedure TSvgProfileScan.CheckDashArray(const Tag, Value: string);
var
  Parts: TStringList;
  S, NumberText, Units: string;
  PartIndex, CharIndex: Integer;
  Number, Factor, FirstNumber: TRealType;
  HasPhysicalUnit: Boolean;
begin
  S := LowerCase(Trim(Value));
  if S = 'none' then Exit;
  if not ValueIn(Tag, ['line', 'rect', 'circle', 'ellipse', 'polygon',
    'polyline', 'path']) then
  begin
    AddIssue('inherited stroke-dasharray cannot be preserved');
    fDashArrayValid := False;
    Exit;
  end;
  Parts := TStringList.Create;
  try
    ExtractStrings([',', ' ', #9, #10, #13], [], PChar(S), Parts);
    if Parts.Count <> 2 then
    begin
      AddIssue('custom stroke-dasharray is not a two-to-one pattern');
      fDashArrayValid := False;
      Exit;
    end;
    for PartIndex := 0 to Parts.Count - 1 do
    begin
      NumberText := Trim(Parts[PartIndex]);
      Units := '';
      CharIndex := Length(NumberText);
      while (CharIndex > 0) and
        (NumberText[CharIndex] in ['a'..'z', 'A'..'Z', '%']) do
        Dec(CharIndex);
      if CharIndex < Length(NumberText) then
      begin
        Units := LowerCase(Copy(NumberText, CharIndex + 1, MaxInt));
        NumberText := Trim(Copy(NumberText, 1, CharIndex));
      end;
      HasPhysicalUnit := Units <> '';
      Number := ParseSvgNumber(NumberText, 'stroke-dasharray');
      if Number <= 0 then
      begin
        AddIssue('stroke-dasharray entries must be positive');
        fDashArrayValid := False;
        Exit;
      end;
      if Units = '' then Factor := 1
      else if Units = 'px' then Factor := 25.4 / 96
      else if Units = 'mm' then Factor := 1
      else if Units = 'cm' then Factor := 10
      else if Units = 'in' then Factor := 25.4
      else if Units = 'pt' then Factor := 25.4 / 72
      else if Units = 'pc' then Factor := 25.4 / 6
      else
      begin
        AddIssue('stroke-dasharray uses unsupported units ' + Units);
        fDashArrayValid := False;
        Exit;
      end;
      if HasPhysicalUnit then
      begin
        if (fPhysicalWidth <= 0) or (fViewBoxWidth <= 0) then
        begin
          AddIssue('physical stroke-dasharray units need a valid root scale');
          fDashArrayValid := False;
          Exit;
        end;
        Number := Number * Factor / (fPhysicalWidth / fViewBoxWidth);
      end;
      if IsNan(Number) or IsInfinite(Number) or (Number > 1E12) then
        raise ESvgCodec.Create('SVG dash length exceeds the supported range');
      if PartIndex = 0 then FirstNumber := Number
      else if PartIndex = 1 then
      begin
        if Abs(FirstNumber - 2 * Number) > 1E-7 *
          Max(1, Max(FirstNumber, Number)) then
        begin
          AddIssue('custom stroke-dasharray is not a two-to-one pattern');
          fDashArrayValid := False;
          Exit;
        end;
        Number := Number * fTransformScaleStack[fDepth];
        if IsNan(Number) or IsInfinite(Number) or (Number > 1E12) then
          raise ESvgCodec.Create('Transformed SVG dash length exceeds the supported range');
        if fDashArrayPresent and
          (Abs(fDashSizeUser - Number) > 1E-7 *
            Max(1, Max(fDashSizeUser, Number))) then
        begin
          AddIssue('stroke-dasharray lengths vary after transforms');
          fDashArrayValid := False;
          Exit;
        end;
        fDashArrayPresent := True;
        fDashSizeUser := Number;
      end;
    end;
  finally
    Parts.Free;
  end;
end;

procedure TSvgProfileScan.CheckRoot(const Attributes: TAttributes);
var
  I: Integer;
  Name, Value: string;
begin
  if Attributes.IndexOfName('viewBox') < 0 then
    AddIssue('root viewBox is required for editable SVG');
  if Attributes.IndexOfName('width') < 0 then
    AddIssue('root width is required for editable SVG');
  if Attributes.IndexOfName('height') < 0 then
    AddIssue('root height is required for editable SVG');
  if Attributes.IndexOfName('xmlns') < 0 then
    AddIssue('SVG namespace is missing');
  if Attributes.IndexOfName('preserveAspectRatio') > -1 then
  begin
    Value := Trim(Attributes.Values['preserveAspectRatio']);
    if (Value <> '') and (Value <> 'xMidYMid meet') then
      AddIssue('non-default preserveAspectRatio');
  end;
  for I := 0 to Attributes.Count - 1 do
  begin
    Name := Attributes.Names[I];
    Value := Attributes.ValueFromIndex[I];
    if Name = 'viewBox' then
      CheckViewBox(Value)
    else if (Name = 'width') or (Name = 'height') then
      CheckPhysicalLength(Name, Value)
    else if (Name = 'xmlns') and (Value <> 'http://www.w3.org/2000/svg') then
      AddIssue('unexpected SVG namespace')
    else if (Name = 'xmlns:xlink') and
      (Value <> 'http://www.w3.org/1999/xlink') then
      AddIssue('unexpected xlink namespace')
    else if (Name = 'xmlns:svg') and
      (Value <> 'http://www.w3.org/2000/svg') then
      AddIssue('unexpected svg prefix namespace')
    else if Name = 'fill-rule' then
      if Value <> 'nonzero' then AddIssue('unsupported fill-rule ' + Value)
    else if Name = 'stroke-miterlimit' then
      if Value <> '10' then AddIssue('non-default stroke-miterlimit')
    else if Name = 'style' then CheckStyle('svg', Value)
    else if not ValueIn(Name, ['xmlns', 'xmlns:xlink', 'xmlns:svg', 'version', 'viewBox',
      'width', 'height', 'preserveAspectRatio', 'fill-rule',
      'stroke-miterlimit', 'style']) then
      AddIssue('unsupported svg attribute ' + Name);
  end;
  if (Attributes.IndexOfName('viewBox') >= 0) and
    (Attributes.IndexOfName('width') >= 0) and
    (Attributes.IndexOfName('height') >= 0) and
    (fPhysicalWidth > 0) and (fPhysicalHeight > 0) and
    (fViewBoxWidth > 0) and (fViewBoxHeight > 0) then
    if Abs(fPhysicalWidth / fViewBoxWidth -
      fPhysicalHeight / fViewBoxHeight) >
      1E-5 * Max(fPhysicalWidth / fViewBoxWidth,
        fPhysicalHeight / fViewBoxHeight) then
      AddIssue('root dimensions do not match viewBox aspect ratio');
end;

procedure TSvgProfileScan.CheckPhysicalLength(const Name, Value: string);
var
  NumberText, Units: string;
  I: Integer;
  Number, Factor: TRealType;
begin
  NumberText := Trim(Value);
  Units := '';
  for I := Length(NumberText) downto 1 do
    if not (NumberText[I] in ['a'..'z', 'A'..'Z', '%']) then Break;
  if I < Length(NumberText) then
  begin
    Units := LowerCase(Copy(NumberText, I + 1, MaxInt));
    NumberText := Trim(Copy(NumberText, 1, I));
  end;
  Number := ParseSvgNumber(NumberText, Name);
  if Number <= 0 then
    raise ESvgCodec.Create('SVG root ' + Name + ' must be positive');
  if (Units = '') or (Units = 'px') then Factor := 25.4 / 96
  else if Units = 'mm' then Factor := 1
  else if Units = 'cm' then Factor := 10
  else if Units = 'in' then Factor := 25.4
  else if Units = 'pt' then Factor := 25.4 / 72
  else if Units = 'pc' then Factor := 25.4 / 6
  else
  begin
    AddIssue('root ' + Name + ' uses unsupported units ' + Units);
    Exit;
  end;
  if Name = 'width' then fPhysicalWidth := Number * Factor
  else fPhysicalHeight := Number * Factor;
end;

procedure TSvgProfileScan.CheckGeometryLength(const Name, Value: string;
  PositiveOnly: Boolean);
var
  NumberText, Units: string;
  I: Integer;
  Number: TRealType;
begin
  NumberText := Trim(Value);
  Units := '';
  for I := Length(NumberText) downto 1 do
    if not (NumberText[I] in ['a'..'z', 'A'..'Z', '%']) then Break;
  if I < Length(NumberText) then
  begin
    Units := LowerCase(Copy(NumberText, I + 1, MaxInt));
    NumberText := Trim(Copy(NumberText, 1, I));
    if not ValueIn(Units, ['px', 'mm', 'cm', 'in', 'pt', 'pc']) then
      AddIssue(Name + ' uses unsupported units ' + Units);
  end;
  Number := ParseSvgNumber(NumberText, Name);
  if (PositiveOnly or ValueIn(Name, ['rx', 'ry', 'stroke-width'])) and
    (Number < 0) then
    raise ESvgCodec.Create('SVG ' + Name + ' must be non-negative');
  if (Number = 0) and ValueIn(Name,
    ['r', 'rx', 'ry', 'width', 'height', 'font-size', 'stroke-width']) then
    AddIssue('zero ' + Name + ' is not represented by the TpX model');
end;

procedure TSvgProfileScan.CheckViewBox(const Value: string);
var
  Parts: TStringList;
  S: string;
  I: Integer;
begin
  Parts := TStringList.Create;
  try
    S := Value;
    ExtractStrings([',', ' ', #9, #10, #13], [], PChar(S), Parts);
    if Parts.Count <> 4 then
      raise ESvgCodec.Create('SVG viewBox must contain four numbers');
    for I := 0 to Parts.Count - 1 do
      ParseSvgNumber(Parts[I], 'viewBox');
    fViewBoxX := ParseSvgNumber(Parts[0], 'viewBox x');
    fViewBoxY := ParseSvgNumber(Parts[1], 'viewBox y');
    fViewBoxWidth := ParseSvgNumber(Parts[2], 'viewBox width');
    fViewBoxHeight := ParseSvgNumber(Parts[3], 'viewBox height');
    if (fViewBoxWidth <= 0) or (fViewBoxHeight <= 0) then
      raise ESvgCodec.Create('SVG viewBox width and height must be positive');
  finally
    Parts.Free;
  end;
end;

procedure TSvgProfileScan.GetViewport(out Viewport: TSvgViewport);
begin
  Viewport.WidthMM := fPhysicalWidth;
  Viewport.HeightMM := fPhysicalHeight;
  Viewport.ViewBoxX := fViewBoxX;
  Viewport.ViewBoxY := fViewBoxY;
  Viewport.ViewBoxWidth := fViewBoxWidth;
  Viewport.ViewBoxHeight := fViewBoxHeight;
end;

procedure TSvgProfileScan.GetDashArray(out Present: Boolean;
  out SizeUser: TRealType);
begin
  Present := fDashArrayPresent and fDashArrayValid;
  SizeUser := fDashSizeUser;
end;

procedure TSvgProfileScan.CheckAttributes(const Tag: string;
  const Attributes: TAttributes);
var
  I: Integer;
  Name, Value: string;
  Allowed: string;
  PositiveOnly: Boolean;
  TransformScale: TRealType;
begin
  if Tag = 'svg' then
  begin
    CheckRoot(Attributes);
    Exit;
  end;
  Allowed := 'id,transform,style,fill,stroke,stroke-width,' +
    'stroke-dasharray,font-family,font-size,font-weight,font-style,' +
    'text-anchor,fill-rule,stroke-miterlimit,color';
  if Tag = 'line' then
    Allowed := Allowed + ',x1,y1,x2,y2'
  else if Tag = 'rect' then
    Allowed := Allowed + ',x,y,width,height,rx,ry'
  else if Tag = 'circle' then
    Allowed := Allowed + ',cx,cy,r'
  else if Tag = 'ellipse' then
    Allowed := Allowed + ',cx,cy,rx,ry'
  else if Tag = 'polygon' then
    Allowed := Allowed + ',points'
  else if Tag = 'polyline' then
    Allowed := Allowed + ',points'
  else if Tag = 'path' then
    Allowed := Allowed + ',d'
  else if Tag = 'text' then
    Allowed := Allowed + ',x,y,xml:space'
  else if (Tag = 'g') or (Tag = 'title') or (Tag = 'desc') or
    (Tag = 'defs') then
    Allowed := 'id,transform,style,fill,stroke,stroke-width,' +
      'stroke-dasharray,font-family,font-size,font-weight,font-style,' +
      'text-anchor,fill-rule,stroke-miterlimit,color';
  if ValueIn(Tag, ['title', 'desc', 'defs']) then Allowed := '';
  for I := 0 to Attributes.Count - 1 do
  begin
    Name := Attributes.Names[I];
    Value := Attributes.ValueFromIndex[I];
    if (Name = 'style') then CheckStyle(Tag, Value)
    else if Name = 'transform' then ValidateTransform(Value, TransformScale)
    else if (Name = 'fill') or (Name = 'stroke') then
    begin
      if Pos('url(', LowerCase(Value)) > 0 then
        AddIssue('resource paint in ' + Name);
      if not IsSimpleColor(Value) and
        (Pos('url(', LowerCase(Value)) = 0) then
        AddIssue('unsupported color in ' + Name);
    end
    else if (Name = 'fill-opacity') or (Name = 'stroke-opacity') or
      (Name = 'opacity') then AddIssue('unsupported opacity attribute ' + Name)
    else if (Name = 'class') then AddIssue('CSS class inheritance')
    else if (Name = 'href') or (Name = 'xlink:href') then
      AddIssue('SVG reference ' + Name)
    else if Pos('on', LowerCase(Name)) = 1 then
      raise ESvgCodec.Create('SVG event attributes are not accepted: ' + Name)
    else if Pos(',' + Name + ',', ',' + Allowed + ',') = 0 then
      AddIssue('unsupported ' + Tag + ' attribute ' + Name);
  end;
  if (Attributes.IndexOfName('id') >= 0) and
    (Attributes.Values['id'] <> '') then
    AddIssue('element IDs are not retained by the TpX model');
  if Tag = 'text' then
    fTextPreserve := Attributes.Values['xml:space'] = 'preserve';
  if (Attributes.IndexOfName('font-family') >= 0) and
    (Pos(',', Attributes.Values['font-family']) > 0) then
    AddIssue('font-family fallback list is not retained');
  if (Tag = 'text') and (Attributes.IndexOfName('font-family') < 0) and
    (Pos('font-family', LowerCase(Attributes.Values['style'])) = 0) then
    AddIssue('text uses an inherited or default font family');
  if (Attributes.IndexOfName('font-weight') >= 0) and
    not ValueIn(LowerCase(Attributes.Values['font-weight']), ['normal', 'bold']) then
    AddIssue('unsupported font-weight ' + Attributes.Values['font-weight']);
  if (Attributes.IndexOfName('font-style') >= 0) and
    not ValueIn(LowerCase(Attributes.Values['font-style']), ['normal', 'italic']) then
    AddIssue('unsupported font-style ' + Attributes.Values['font-style']);
  for I := 0 to Attributes.Count - 1 do
  begin
    Name := Attributes.Names[I];
    if ValueIn(Name, ['x', 'y', 'x1', 'y1', 'x2', 'y2', 'cx', 'cy', 'r',
      'rx', 'ry', 'width', 'height', 'font-size', 'stroke-width']) then
    begin
      PositiveOnly := ValueIn(Name, ['r', 'rx', 'ry', 'width', 'height',
        'font-size']);
      if ValueIn(Name, ['rx', 'ry']) then PositiveOnly := False;
      CheckGeometryLength(Name, Attributes.ValueFromIndex[I], PositiveOnly);
    end;
  end;
  if (Tag = 'path') and (Attributes.IndexOfName('d') >= 0) then
    ValidatePath(Attributes.Values['d']);
  if Attributes.IndexOfName('stroke-dasharray') >= 0 then
  begin
    if InlineStyleValue(Attributes.Values['style'], 'stroke-dasharray') <> '' then
      AddIssue('competing stroke-dasharray declarations are import-only')
    else
      CheckDashArray(Tag, Attributes.Values['stroke-dasharray']);
  end;
  if (Tag = 'path') and (Attributes.IndexOfName('d') < 0) then
    raise ESvgCodec.Create('SVG path has no d attribute');
  if (Tag = 'rect') and ((Attributes.IndexOfName('width') < 0) or
    (Attributes.IndexOfName('height') < 0)) then
    raise ESvgCodec.Create('SVG rect requires width and height');
  if (Tag = 'circle') and (Attributes.IndexOfName('r') < 0) then
    raise ESvgCodec.Create('SVG circle requires r');
  if (Tag = 'ellipse') and ((Attributes.IndexOfName('rx') < 0) or
    (Attributes.IndexOfName('ry') < 0)) then
    raise ESvgCodec.Create('SVG ellipse requires rx and ry');
  if (Tag = 'polygon') or (Tag = 'polyline') then
    if Attributes.IndexOfName('points') < 0 then
      raise ESvgCodec.Create('SVG ' + Tag + ' requires points');
  if (Tag = 'polygon') or (Tag = 'polyline') then
    ValidatePointList(Attributes.Values['points'], Tag);
  if (Attributes.IndexOfName('fill-rule') >= 0) and
    (Attributes.Values['fill-rule'] <> 'nonzero') then
    AddIssue('unsupported fill-rule ' + Attributes.Values['fill-rule']);
  if (Attributes.IndexOfName('stroke-miterlimit') >= 0) and
    (Attributes.Values['stroke-miterlimit'] <> '10') then
    AddIssue('non-default stroke-miterlimit');
  if (Attributes.IndexOfName('text-anchor') >= 0) and
    not ValueIn(Attributes.Values['text-anchor'], ['start', 'middle', 'end']) then
    raise ESvgCodec.Create('Unsupported text-anchor value');
  if (Attributes.IndexOfName('color') >= 0) or
    (Pos('currentColor', Attributes.Text) > 0) then
    AddIssue('currentColor inheritance is not retained');
end;

procedure TSvgProfileScan.ValidateTransform(const Value: string;
  out UniformScale: TRealType);
var
  I, J, K, Count: Integer;
  Name, Args, Token: string;
  Parts: TStringList;
  A, B, C, D, SX, SY, Tolerance, Determinant: TRealType;
  LocalScale: TRealType;
begin
  UniformScale := 1;
  I := 1;
  while I <= Length(Value) do
  begin
    while (I <= Length(Value)) and (Value[I] in [' ', #9, #10, #13, ',']) do
      Inc(I);
    if I > Length(Value) then Break;
    J := I;
    while (I <= Length(Value)) and (Value[I] in ['A'..'Z', 'a'..'z']) do
      Inc(I);
    if I = J then raise ESvgCodec.Create('Malformed SVG transform');
    Name := Copy(Value, J, I - J);
    while (I <= Length(Value)) and (Value[I] in [' ', #9, #10, #13]) do
      Inc(I);
    if (I > Length(Value)) or (Value[I] <> '(') then
      raise ESvgCodec.Create('Malformed SVG transform ' + Name);
    Inc(I);
    J := I;
    while (I <= Length(Value)) and (Value[I] <> ')') do Inc(I);
    if I > Length(Value) then
      raise ESvgCodec.Create('Unclosed SVG transform ' + Name);
    Args := Copy(Value, J, I - J);
    Inc(I);
    Parts := TStringList.Create;
    try
      ExtractStrings([',', ' ', #9, #10, #13], [], PChar(Args), Parts);
      Count := Parts.Count;
      for K := 0 to Count - 1 do
        ParseSvgNumber(Parts[K], 'transform');
      LocalScale := 1;
      if Name = 'matrix' then
      begin
        if Count <> 6 then raise ESvgCodec.Create('matrix() needs six values');
        A := ParseSvgNumber(Parts[0], 'matrix');
        B := ParseSvgNumber(Parts[1], 'matrix');
        C := ParseSvgNumber(Parts[2], 'matrix');
        D := ParseSvgNumber(Parts[3], 'matrix');
        SX := Sqrt(Sqr(A) + Sqr(B));
        SY := Sqrt(Sqr(C) + Sqr(D));
        LocalScale := SX;
        Determinant := A * D - B * C;
        Tolerance := 1E-9 * Max(1, Max(SX, SY));
        if (SX <= Tolerance) or (SY <= Tolerance) or
          (Abs(SX - SY) > Tolerance) or
          (Abs(A * C + B * D) > Tolerance * Max(SX, SY)) or
          (Determinant < 0) then
          AddIssue('non-uniform or skewed matrix transform is import-only');
      end
      else if Name = 'translate' then
      begin
        if not (Count in [1, 2]) then
          raise ESvgCodec.Create('translate() needs one or two values');
      end
      else if Name = 'scale' then
      begin
        if not (Count in [1, 2]) then
          raise ESvgCodec.Create('scale() needs one or two values');
        if (Count = 2) and
          (Abs(ParseSvgNumber(Parts[0], 'scale') -
            ParseSvgNumber(Parts[1], 'scale')) > 1E-9 *
            Max(1, Max(Abs(ParseSvgNumber(Parts[0], 'scale')),
              Abs(ParseSvgNumber(Parts[1], 'scale'))))) then
          AddIssue('non-uniform scale is import-only');
        LocalScale := Abs(ParseSvgNumber(Parts[0], 'scale'));
        if LocalScale <= 1E-9 then
          AddIssue('singular scale transform is import-only');
        if (Count = 1) and
          (ParseSvgNumber(Parts[0], 'scale') < 0) then
          AddIssue('reflected scale transform is import-only');
        if (Count = 2) and
          (ParseSvgNumber(Parts[0], 'scale') *
            ParseSvgNumber(Parts[1], 'scale') < 0) then
          AddIssue('reflected scale transform is import-only');
      end
      else if Name = 'rotate' then
      begin
        if not (Count in [1, 3]) then
          raise ESvgCodec.Create('rotate() needs one or three values');
      end
      else if ValueIn(Name, ['skewX', 'skewY']) then
      begin
        if Count <> 1 then raise ESvgCodec.Create(Name + '() needs one value');
        AddIssue('skew transforms are import-only');
      end
      else raise ESvgCodec.Create('Unsupported SVG transform ' + Name);
      UniformScale := UniformScale * LocalScale;
      if IsNan(UniformScale) or IsInfinite(UniformScale) or
        (UniformScale > 1E12) then
        raise ESvgCodec.Create('SVG transform scale exceeds the supported range');
    finally
      Parts.Free;
    end;
  end;
  if Trim(Value) = '' then
    raise ESvgCodec.Create('Empty SVG transform');
end;

procedure TSvgProfileScan.ValidatePath(const Value: string);
const
  CommandChars = 'MmLlHhVvCcSsQqTtAaZz';
var
  I, J, ArgCount, ArgIndex, GroupCount: Integer;
  C, Effective: Char;
  Token: string;
  Number: TRealType;
  HadSeparator, HadComma: Boolean;
  FirstCommand, SawMove, SawArguments: Boolean;

  procedure SkipSpace;
  begin
    while (I <= Length(Value)) and (Value[I] in [#9, #10, #13, ' ']) do Inc(I);
  end;

  function ReadNumber: TRealType;
  var
    Start, Digits: Integer;
  begin
    Start := I;
    if (I <= Length(Value)) and (Value[I] in ['+', '-']) then Inc(I);
    Digits := 0;
    while (I <= Length(Value)) and (Value[I] in ['0'..'9']) do
    begin
      Inc(I);
      Inc(Digits);
    end;
    if (I <= Length(Value)) and (Value[I] = '.') then
    begin
      Inc(I);
      while (I <= Length(Value)) and (Value[I] in ['0'..'9']) do
      begin
        Inc(I);
        Inc(Digits);
      end;
    end;
    if Digits = 0 then raise ESvgCodec.Create('Malformed SVG path number');
    if (I <= Length(Value)) and (Value[I] in ['e', 'E']) then
    begin
      Inc(I);
      if (I <= Length(Value)) and (Value[I] in ['+', '-']) then Inc(I);
      Digits := 0;
      while (I <= Length(Value)) and (Value[I] in ['0'..'9']) do
      begin
        Inc(I);
        Inc(Digits);
      end;
      if Digits = 0 then raise ESvgCodec.Create('Malformed SVG path exponent');
    end;
    Token := Copy(Value, Start, I - Start);
    Result := ParseSvgNumber(Token, 'path');
    if Abs(Result) > 1E9 then
      raise ESvgCodec.Create('SVG path coordinate exceeds the supported range');
  end;

begin
  if Length(Value) > MaxSvgInputBytes then
    raise ESvgCodec.Create('SVG path data exceeds the supported size');
  I := 1;
  C := #0;
  GroupCount := 0;
  FirstCommand := True;
  SawMove := False;
  SawArguments := False;
  while I <= Length(Value) do
  begin
    SkipSpace;
    if I > Length(Value) then Break;
    if Pos(Value[I], CommandChars) > 0 then
    begin
      C := Value[I];
      Inc(I);
      GroupCount := 0;
      if FirstCommand and not (C in ['M', 'm']) then
        raise ESvgCodec.Create('SVG path must begin with moveto');
      if C in ['M', 'm'] then SawMove := True;
      if C in ['Z', 'z'] then
      begin
        if not SawMove then
          raise ESvgCodec.Create('SVG closepath appears before moveto');
        C := #0;
        Continue;
      end;
    end
    else if C = #0 then
      raise ESvgCodec.Create('SVG path data must begin with a command');
    Effective := UpCase(C);
    case Effective of
      'M', 'L', 'T': ArgCount := 2;
      'H', 'V': ArgCount := 1;
      'C': ArgCount := 6;
      'S', 'Q': ArgCount := 4;
      'A': ArgCount := 7;
    else
      raise ESvgCodec.Create('Unsupported SVG path command');
    end;
    for ArgIndex := 1 to ArgCount do
    begin
      SkipSpace;
      if (I <= Length(Value)) and (Value[I] = ',') then
      begin
        Inc(I);
        SkipSpace;
        HadComma := True;
      end
      else HadComma := False;
      if (I > Length(Value)) or (Pos(Value[I], CommandChars) > 0) then
        raise ESvgCodec.Create('Incomplete SVG path command ' + C);
      Number := ReadNumber;
      if (Effective = 'A') and (ArgIndex in [1, 2]) and (Number < 0) then
        raise ESvgCodec.Create('SVG arc radii must be non-negative');
      if (Effective = 'A') and ((ArgIndex = 4) or (ArgIndex = 5)) and
        (Number <> 0) and (Number <> 1) then
        raise ESvgCodec.Create('SVG arc flags must be 0 or 1');
      if I <= Length(Value) then
      begin
        HadSeparator := Value[I] in [#9, #10, #13, ' ', ','];
        if HadComma and not HadSeparator and
          not (Value[I] in ['+', '-']) then
          raise ESvgCodec.Create('Malformed SVG path separator');
        if not HadSeparator and not (Value[I] in ['+', '-', '.']) and
          (Pos(Value[I], CommandChars) = 0) then
          raise ESvgCodec.Create('Malformed SVG path data near ' + Copy(Value, I, 12));
      end;
    end;
    Inc(GroupCount);
    SawArguments := True;
    FirstCommand := False;
    if Effective = 'A' then
      AddIssue('elliptic path arcs are approximated by cubic Bezier curves');
    if C = 'M' then C := 'L'
    else if C = 'm' then C := 'l';
    if GroupCount > MaxPathCommands then
      raise ESvgCodec.Create('SVG path exceeds the command limit');
  end;
  if not SawArguments then
    raise ESvgCodec.Create('SVG path data is empty');
end;

procedure ValidateXmlEntities(const Stream: TMemoryStream); forward;

procedure TSvgProfileScan.Scan(const Stream: TStream; out Editable: Boolean;
  out Diagnostics: string);
var
  Parser: TStitchSAX;
  SavedPosition: Int64;
begin
  SavedPosition := Stream.Position;
  try
    ValidateXmlEntities(Stream as TMemoryStream);
    Stream.Position := 0;
    Parser := TStitchSAX.Create(StartElement, EndElement, Text, Comment);
    try
      Parser.OnCDATA := CDATA;
      Parser.OnDOCTYPE := Doctype;
      Parser.ParseStream(Stream);
    finally
      Parser.Free;
    end;
    if not fRootSeen or not fRootClosed then
      raise ESvgCodec.Create('SVG document has no complete root element');
  except
    on E: EStitchSAX do
      raise ESvgCodec.Create('Malformed SVG XML: ' + E.Message);
  end;
  Editable := fIssues.Count = 0;
  Diagnostics := '';
  if not Editable then
  begin
    Diagnostics := 'SVG opened import-only. ';
    if fIssues.Count > 0 then
    begin
      Diagnostics := Diagnostics + fIssues[0];
      if fIssues.Count > 1 then
        Diagnostics := Diagnostics + ' (and ' + IntToStr(fIssues.Count - 1) +
          ' more unsupported feature(s))';
    if fIssueOverflow then
      Diagnostics := Diagnostics + '; additional feature details omitted';
    end;
  end;
  Stream.Position := SavedPosition;
end;

function ReadBounded(const Stream: TStream; const Maximum: Int64;
  const Description: string): TMemoryStream;
var
  Buffer: array[0..16383] of Byte;
  Count: LongInt;
  Total: Int64;
begin
  Result := TMemoryStream.Create;
  try
    Total := 0;
    repeat
      Count := Stream.Read(Buffer, SizeOf(Buffer));
      if Count <= 0 then Break;
      Inc(Total, Count);
      if Total > Maximum then
        raise ESvgCodec.Create(Description + ' exceeds the size limit');
      Result.WriteBuffer(Buffer, Count);
    until False;
    Result.Position := 0;
  except
    Result.Free;
    raise;
  end;
end;

procedure ValidateXmlEntities(const Stream: TMemoryStream);
var
  Source, Entity: string;
  I, J, K, EncodingPos, QuotePos, Code, EndRef: Integer;
  Encoding, XmlDecl: string;
  Quote: Char;
begin
  SetLength(Source, Stream.Size);
  if Stream.Size > 0 then
  begin
    Stream.Position := 0;
    Stream.ReadBuffer(Source[1], Stream.Size);
  end;
  if Pos(#0, Source) > 0 then
    raise ESvgCodec.Create('SVG XML must use UTF-8 encoding');
  I := 1;
  while I <= Length(Source) do
  begin
    Code := Ord(Source[I]);
    if Code < $80 then
    begin
      if not ((Code = 9) or (Code = 10) or (Code = 13) or (Code >= 32)) then
        raise ESvgCodec.Create('SVG XML contains a forbidden control character');
      Inc(I);
      Continue;
    end;
    if Code in [$C2..$DF] then
    begin
      if (I + 1 > Length(Source)) or
        (Ord(Source[I + 1]) < $80) or (Ord(Source[I + 1]) > $BF) then
        raise ESvgCodec.Create('SVG XML contains invalid UTF-8');
      Code := ((Code and $1F) shl 6) or (Ord(Source[I + 1]) and $3F);
      Inc(I, 2);
    end
    else if Code in [$E0..$EF] then
    begin
      if (I + 2 > Length(Source)) or
        (Ord(Source[I + 1]) < $80) or (Ord(Source[I + 1]) > $BF) or
        (Ord(Source[I + 2]) < $80) or (Ord(Source[I + 2]) > $BF) then
        raise ESvgCodec.Create('SVG XML contains invalid UTF-8');
      if ((Code = $E0) and (Ord(Source[I + 1]) < $A0)) or
        ((Code = $ED) and (Ord(Source[I + 1]) >= $A0)) then
        raise ESvgCodec.Create('SVG XML contains invalid UTF-8');
      Code := ((Code and $0F) shl 12) or
        ((Ord(Source[I + 1]) and $3F) shl 6) or
        (Ord(Source[I + 2]) and $3F);
      Inc(I, 3);
    end
    else if Code in [$F0..$F4] then
    begin
      if (I + 3 > Length(Source)) or
        (Ord(Source[I + 1]) < $80) or (Ord(Source[I + 1]) > $BF) or
        (Ord(Source[I + 2]) < $80) or (Ord(Source[I + 2]) > $BF) or
        (Ord(Source[I + 3]) < $80) or (Ord(Source[I + 3]) > $BF) then
        raise ESvgCodec.Create('SVG XML contains invalid UTF-8');
      if ((Code = $F0) and (Ord(Source[I + 1]) < $90)) or
        ((Code = $F4) and (Ord(Source[I + 1]) > $8F)) then
        raise ESvgCodec.Create('SVG XML contains invalid UTF-8');
      Code := ((Code and $07) shl 18) or
        ((Ord(Source[I + 1]) and $3F) shl 12) or
        ((Ord(Source[I + 2]) and $3F) shl 6) or
        (Ord(Source[I + 3]) and $3F);
      Inc(I, 4);
    end
    else raise ESvgCodec.Create('SVG XML contains invalid UTF-8');
    if not (((Code >= $20) and (Code <= $D7FF)) or
      ((Code >= $E000) and (Code <= $FFFD)) or
      ((Code >= $10000) and (Code <= $10FFFF))) then
      raise ESvgCodec.Create('SVG XML contains a forbidden Unicode character');
  end;
  I := 1;
  while I <= Length(Source) do
  begin
    if Copy(Source, I, 4) = '<!--' then
    begin
      J := Pos('-->', Copy(Source, I + 4, MaxInt));
      if J = 0 then Exit;
      Inc(I, 4 + J + 2);
      Continue;
    end;
    if Copy(Source, I, 9) = '<![CDATA[' then
    begin
      J := Pos(']]>', Copy(Source, I + 9, MaxInt));
      if J = 0 then Exit;
      Inc(I, 9 + J + 2);
      Continue;
    end;
    if Copy(Source, I, 2) = '<?' then
    begin
      J := Pos('?>', Copy(Source, I + 2, MaxInt));
      if J = 0 then Exit;
      if ((I = 1) or ((I = 4) and
        (Copy(Source, 1, 3) = #$EF#$BB#$BF))) and
        (Copy(Source, I, 5) = '<?xml') then
      begin
        XmlDecl := Copy(Source, I, J + 3);
        EncodingPos := Pos('encoding', LowerCase(XmlDecl));
        if EncodingPos > 0 then
        begin
          K := EncodingPos + Length('encoding');
          while (K <= Length(XmlDecl)) and
            (XmlDecl[K] in [' ', #9, #10, #13]) do Inc(K);
          if (K <= Length(XmlDecl)) and (XmlDecl[K] = '=') then
          begin
            Inc(K);
            while (K <= Length(XmlDecl)) and
              (XmlDecl[K] in [' ', #9, #10, #13]) do Inc(K);
            if (K > Length(XmlDecl)) or
              not (XmlDecl[K] in ['"', '''']) then
              raise ESvgCodec.Create('Malformed XML encoding declaration');
            Quote := XmlDecl[K];
            QuotePos := K + 1;
            K := QuotePos;
            while (K <= Length(XmlDecl)) and (XmlDecl[K] <> Quote) do Inc(K);
            if K > Length(XmlDecl) then
              raise ESvgCodec.Create('Malformed XML encoding declaration');
            Encoding := LowerCase(Copy(XmlDecl, QuotePos, K - QuotePos));
            if not ValueIn(Encoding, ['utf-8', 'utf8']) then
              raise ESvgCodec.Create('SVG XML must use UTF-8 encoding');
          end;
        end;
      end;
      Inc(I, 2 + J + 1);
      Continue;
    end;
    if Copy(Source, I, 9) = '<!DOCTYPE' then
      raise ESvgCodec.Create('DOCTYPE and entity declarations are not accepted');
    if Source[I] <> '&' then
    begin
      Inc(I);
      Continue;
    end;
    J := I + 1;
    while (J <= Length(Source)) and (Source[J] <> ';') and
      not (Source[J] in ['<', #9, #10, #13, ' ']) do Inc(J);
    if (J > Length(Source)) or (Source[J] <> ';') then
      raise ESvgCodec.Create('Malformed XML entity reference');
    EndRef := J;
    Entity := Copy(Source, I + 1, J - I - 1);
    if not ValueIn(Entity, ['amp', 'lt', 'gt', 'quot', 'apos']) then
    begin
      if (Length(Entity) < 2) or (Entity[1] <> '#') then
        raise ESvgCodec.Create('Undeclared XML entity reference: &' + Entity + ';');
      if (Length(Entity) > 2) and (Entity[2] in ['x', 'X']) then
      begin
        for J := 3 to Length(Entity) do
          if not (Entity[J] in ['0'..'9', 'a'..'f', 'A'..'F']) then
            raise ESvgCodec.Create('Malformed numeric XML entity reference');
        if not TryStrToInt('$' + Copy(Entity, 3, MaxInt), Code) then
          raise ESvgCodec.Create('Malformed numeric XML entity reference');
      end
      else
      begin
        for J := 2 to Length(Entity) do
          if not (Entity[J] in ['0'..'9']) then
            raise ESvgCodec.Create('Malformed numeric XML entity reference');
        if not TryStrToInt(Copy(Entity, 2, MaxInt), Code) then
          raise ESvgCodec.Create('Malformed numeric XML entity reference');
      end;
      if not ((Code in [9, 10, 13]) or ((Code >= 32) and
        (Code <= $10FFFF) and not ((Code >= $D800) and (Code <= $DFFF)))) then
        raise ESvgCodec.Create('Invalid Unicode code point in XML entity reference');
    end;
    I := EndRef + 1;
  end;
  Stream.Position := 0;
end;

procedure AppendDiagnostic(const Diagnostics: TStrings; const Value: string);
begin
  if (Diagnostics <> nil) and (Value <> '') then Diagnostics.Add(Value);
end;

procedure LoadSvgFromStreamInternal(const Stream: TStream;
  const Candidate: TDrawing2D;
  const CompressedSource: Boolean; out Profile: TSvgDocumentProfile;
  out Diagnostics: string; out Viewport: TSvgViewport);
var
  Data: TMemoryStream;
  Scanner: TSvgProfileScan;
  Editable: Boolean;
  Importer: T_SVG_Import;
  HasDashArray: Boolean;
  DashSizeUser: TRealType;
begin
  if (Stream = nil) or (Candidate = nil) then
    raise ESvgCodec.Create('SVG input stream and candidate drawing are required');
  Stream.Position := 0;
  if CompressedSource then
    Data := ReadBounded(Stream, MaxSvgExpandedBytes, 'Expanded SVGZ input')
  else
    Data := ReadBounded(Stream, MaxSvgInputBytes, 'SVG input');
  try
    Scanner := TSvgProfileScan.Create;
    try
      Scanner.Scan(Data, Editable, Diagnostics);
      Scanner.GetViewport(Viewport);
      Scanner.GetDashArray(HasDashArray, DashSizeUser);
    finally
      Scanner.Free;
    end;
    try
      Importer := T_SVG_Import.Create(Candidate);
      try
        Importer.IgnoreUseReferences := True;
        Data.Position := 0;
        Importer.ParseFromStream(Data);
      finally
        Importer.Free;
      end;
      if HasDashArray then
        Candidate.DashSize := DashSizeUser * Candidate.PicScale;
    except
      on E: ESvgCodec do raise;
      on E: Exception do
        raise ESvgCodec.Create('SVG import failed: ' + E.Message);
    end;
    if (Candidate.ObjectsCount = 0) and not Editable then
      raise ESvgCodec.Create('SVG contains unsupported content but no geometry that TpX can import');
    Profile := sdpEditable;
    if not Editable or CompressedSource then Profile := sdpImportOnly;
    if CompressedSource then
    begin
      if Diagnostics <> '' then Diagnostics := Diagnostics + '; ';
      Diagnostics := Diagnostics + 'SVGZ is import-only';
    end;
  finally
    Data.Free;
  end;
end;

procedure LoadSvgFromStream(const Stream: TStream; const Candidate: TDrawing2D;
  const CompressedSource: Boolean; out Profile: TSvgDocumentProfile;
  out Diagnostics: string);
var
  Viewport: TSvgViewport;
begin
  LoadSvgFromStreamInternal(Stream, Candidate, CompressedSource, Profile,
    Diagnostics, Viewport);
end;

procedure DecompressSvgzFile(const FileName: string; const OutStream: TStream);
const
  BufferSize = 16384;
  GzipStatusOK = 0;
  GzipStatusStreamEnd = 1;
var
  Input: TFileStream;
  ZFile: gzFile;
  Buffer: array[0..BufferSize - 1] of Byte;
  Len: Integer;
  Total: Int64;
  ErrorCode: SmallInt;
  CloseCode: Integer;
  ErrorText: string;
begin
  Input := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    if Input.Size > MaxSvgInputBytes then
      raise ESvgCodec.Create('SVGZ input exceeds the 4 MiB compressed size limit');
  finally
    Input.Free;
  end;
  ZFile := gzopen(PChar(FileName), 'rb');
  if ZFile = nil then raise ESvgCodec.Create('Can not open SVGZ stream');
  CloseCode := 0;
  try
    Total := 0;
    repeat
      Len := gzread(ZFile, @Buffer, BufferSize);
      if Len < 0 then
      begin
        ErrorText := gzerror(ZFile, ErrorCode);
        if ErrorText = '' then ErrorText := 'decompression error';
        raise ESvgCodec.Create('Invalid SVGZ stream: ' + ErrorText);
      end;
      if Len = 0 then Break;
      Inc(Total, Len);
      if Total > MaxSvgExpandedBytes then
        raise ESvgCodec.Create('SVGZ expands beyond the 16 MiB limit');
      OutStream.WriteBuffer(Buffer, Len);
    until False;
    ErrorText := gzerror(ZFile, ErrorCode);
    // zlib reports Z_STREAM_END through gzerror after a successful read has
    // returned zero. Only other non-zero states indicate a corrupt stream.
    if (ErrorCode <> GzipStatusOK) and
      (ErrorCode <> GzipStatusStreamEnd) then
    begin
      if ErrorText = '' then ErrorText := 'decompression error';
      raise ESvgCodec.Create('Invalid SVGZ stream: ' + ErrorText);
    end;
  finally
    CloseCode := gzclose(ZFile);
  end;
  if CloseCode <> 0 then
    raise ESvgCodec.Create('Invalid or truncated SVGZ stream');
  OutStream.Position := 0;
end;

procedure LoadSvgzFile(const FileName: string; const Candidate: TDrawing2D;
  out Diagnostics: string);
var
  Data: TMemoryStream;
  Profile: TSvgDocumentProfile;
begin
  Data := TMemoryStream.Create;
  try
    DecompressSvgzFile(FileName, Data);
    Data.Position := 0;
    LoadSvgFromStream(Data, Candidate, True, Profile, Diagnostics);
  finally
    Data.Free;
  end;
end;

procedure LoadSvgzBytes(const Bytes: RawByteString; const Candidate: TDrawing2D;
  Diagnostics: TStrings);
var
  TempName: string;
  FileStream: TFileStream;
  Detail: string;
begin
  if Length(Bytes) > MaxSvgInputBytes then
    raise ESvgCodec.Create('SVGZ input exceeds the 4 MiB compressed size limit');
  TempName := GetTempFileName(GetTempDir(False), 'tpx-svgz-');
  try
    FileStream := TFileStream.Create(TempName, fmCreate);
    try
      if Length(Bytes) > 0 then FileStream.WriteBuffer(Bytes[1], Length(Bytes));
    finally
      FileStream.Free;
    end;
    LoadSvgzFile(TempName, Candidate, Detail);
    AppendDiagnostic(Diagnostics, Detail);
  finally
    DeleteFile(TempName);
  end;
end;

function LoadSvgDocumentWithContext(Candidate: TDrawing2D;
  const FileName: TDocumentPath; const SourceBytes: RawByteString;
  Diagnostics: TStrings; out CodecContext: TObject): Boolean;
var
  Data: TMemoryStream;
  Profile: TSvgDocumentProfile;
  Detail: string;
  Viewport: TSvgViewport;
begin
  CodecContext := nil;
  if Candidate = nil then raise ESvgCodec.Create('SVG candidate drawing is required');
  if SameText(ExtractFileExt(FileName), '.svgz') then
  begin
    LoadSvgzBytes(SourceBytes, Candidate, Diagnostics);
    Exit(False);
  end;
  if Length(SourceBytes) = 0 then
    raise ESvgCodec.Create('SVG source snapshot is empty');
  if Length(SourceBytes) > MaxSvgInputBytes then
    raise ESvgCodec.Create('SVG input exceeds the 4 MiB size limit');
  Data := TMemoryStream.Create;
  try
    Data.WriteBuffer(SourceBytes[1], Length(SourceBytes));
    Data.Position := 0;
    LoadSvgFromStreamInternal(Data, Candidate, False, Profile, Detail,
      Viewport);
    AppendDiagnostic(Diagnostics, Detail);
    Result := Profile = sdpEditable;
    if Result then
    begin
      CodecContext := TSvgCodecContext.Create;
      TSvgCodecContext(CodecContext).Viewport := Viewport;
    end;
  finally
    Data.Free;
  end;
end;

function LoadSvgDocument(Candidate: TDrawing2D; const FileName: TDocumentPath;
  const SourceBytes: RawByteString; Diagnostics: TStrings): Boolean;
var
  CodecContext: TObject;
begin
  Result := LoadSvgDocumentWithContext(Candidate, FileName, SourceBytes,
    Diagnostics, CodecContext);
  CodecContext.Free;
end;

function SvgListCanBeSaved(const Objects: TGraphicObjList;
  out Unsupported: string): Boolean; forward;

function SvgObjectCanBeSaved(const Obj: TObject2D;
  out Unsupported: string): Boolean;
begin
  if Obj is TBitmap2D then
  begin
    Unsupported := 'bitmap objects require linked assets';
    Exit(False);
  end;
  if Obj is TGroup2D then
    Exit(SvgListCanBeSaved(TGroup2D(Obj).Objects, Unsupported));
  Result := (Obj is TLine2D) or (Obj is TEllipse2D) or
    (Obj is TRectangle2D) or (Obj is TCircle2D) or
    (Obj is TCircular2D) or (Obj is TSmoothPath2D0) or
    (Obj is TBezierPath2D0) or (Obj is TStar2D) or
    (Obj is TSymbol2D) or (Obj is TPolyline2D0) or
    (Obj is TText2D) or (Obj is TCompound2D);
  if not Result then
    Unsupported := Obj.ClassName + ' objects are not supported by the SVG saver';
end;

function SvgListCanBeSaved(const Objects: TGraphicObjList;
  out Unsupported: string): Boolean;
var
  Obj: TGraphicObject;
begin
  Result := True;
  if Objects = nil then Exit;
  Obj := Objects.FirstObj;
  while Obj <> nil do
  begin
    if not (Obj is TObject2D) then
    begin
      Unsupported := Obj.ClassName + ' objects are not SVG drawable objects';
      Exit(False);
    end;
    if not SvgObjectCanBeSaved(TObject2D(Obj), Unsupported) then
      Exit(False);
    Obj := Objects.NextObj;
  end;
end;

procedure SaveSvgToStreamWithContext(const Drawing: TDrawing2D;
  const Destination: TStream; CodecContext: TObject); forward;

procedure SaveSvgToStream(const Drawing: TDrawing2D;
  const Destination: TStream);
begin
  SaveSvgToStreamWithContext(Drawing, Destination, nil);
end;

procedure SaveSvgToStreamWithContext(const Drawing: TDrawing2D;
  const Destination: TStream; CodecContext: TObject);
var
  Saver: T_SVG_Export;
  PicScale0: TRealType;
  Unsupported: string;
begin
  if (Drawing = nil) or (Destination = nil) then
    raise ESvgCodec.Create('SVG drawing and destination stream are required');
  if not SvgListCanBeSaved(Drawing.ObjectList, Unsupported) then
    raise ESvgCodec.Create('Cannot save SVG source: ' + Unsupported);
  PicScale0 := Drawing.PicScale;
  Saver := T_SVG_Export.Create(Drawing);
  try
    if CodecContext is TSvgCodecContext then
      Saver.SetSourceViewport(
        TSvgCodecContext(CodecContext).Viewport.WidthMM,
        TSvgCodecContext(CodecContext).Viewport.HeightMM,
        TSvgCodecContext(CodecContext).Viewport.ViewBoxX,
        TSvgCodecContext(CodecContext).Viewport.ViewBoxY,
        TSvgCodecContext(CodecContext).Viewport.ViewBoxWidth,
        TSvgCodecContext(CodecContext).Viewport.ViewBoxHeight);
    Saver.WriteToTpX(Destination, '');
  finally
    Drawing.PicScale := PicScale0;
    Saver.Free;
  end;
end;

function LoadSvgCodec(Candidate: TDrawing2D; const FileName: TDocumentPath;
  const SourceBytes: RawByteString; Diagnostics: TStrings;
  out CodecContext: TObject): Boolean;
begin
  Result := LoadSvgDocumentWithContext(Candidate, FileName, SourceBytes,
    Diagnostics, CodecContext);
end;

function SaveSvgCodec(Drawing: TDrawing2D; const FileName: TDocumentPath;
  Destination: TStream; CodecContext: TObject): Boolean;
begin
  SaveSvgToStreamWithContext(Drawing, Destination, CodecContext);
  Result := True;
end;

procedure RegisterSvgDocumentCodec;
var
  Format: TDocumentFormat;
begin
  Format.Id := 'svg';
  Format.DisplayName := 'Scalable Vector Graphics';
  Format.Extensions := '.svg';
  Format.CanOpen := True;
  Format.CanSaveBack := True;
  Format.RoundTripProfile :=
    'Validated primitive profile; unsupported SVG content is import-only';
  Format.RuntimeRequirements := '';
  RegisterDocumentCodec(Format, @LoadSvgCodec, @SaveSvgCodec);

  Format.Id := 'svgz';
  Format.DisplayName := 'Compressed Scalable Vector Graphics';
  Format.Extensions := '.svgz';
  Format.CanSaveBack := False;
  Format.RoundTripProfile := 'Bounded decompression; import-only';
  RegisterDocumentCodec(Format, @LoadSvgCodec, nil);
end;

procedure TSvgProfileScan.StartElement(const Name: string;
  const Attributes: TAttributes);
var
  Tag, Parent, TransformValue, StyleTransform: string;
  ParentScale, LocalScale, EffectiveScale: TRealType;
begin
  if Name = '?xml' then Exit;
  if (Name <> '') and (Name[1] = '?') then
  begin
    AddIssue('XML processing instruction is not retained');
    Exit;
  end;
  Tag := LocalSvgName(Name);
  if not IsSvgName(Name) then
    raise ESvgCodec.Create('Unsupported XML namespace prefix: ' + Name);
  if fDepth >= MaxSvgDepth then
    raise ESvgCodec.Create('SVG nesting exceeds the 64 element limit');
  Inc(fElements);
  if fElements > MaxSvgElements then
    raise ESvgCodec.Create('SVG has more than 100000 elements');
  if fNames.Count > 0 then Parent := fNames[fNames.Count - 1]
  else Parent := '';
  if not fRootSeen then
  begin
    if Tag <> 'svg' then raise ESvgCodec.Create('The root element is not svg');
    fRootSeen := True;
  end
  else if fRootClosed then
    raise ESvgCodec.Create('SVG contains content after the root element');
  if (Tag = 'svg') and (fDepth > 0) then
    AddIssue('nested SVG viewports are not retained');
  if (Tag = 'script') then
    raise ESvgCodec.Create('SVG script elements are not accepted');
  if not ValueIn(Tag, ['svg', 'g', 'defs', 'title', 'desc', 'line', 'rect',
    'circle', 'ellipse', 'polygon', 'polyline', 'path', 'text', 'tspan',
    'textPath', 'use', 'style', 'linearGradient', 'radialGradient', 'stop',
    'clipPath', 'a', 'symbol', 'image', 'filter', 'mask', 'marker',
    'foreignObject']) then
    AddIssue('unsupported SVG element ' + Tag);
  if ValueIn(Tag, ['tspan', 'textPath']) then
    AddIssue(Tag + ' text layout is not retained by the TpX model');
  if ValueIn(Tag, ['linearGradient', 'radialGradient', 'filter', 'clipPath',
    'mask', 'marker', 'style', 'use', 'symbol', 'image', 'foreignObject', 'a']) then
    AddIssue('unsupported SVG feature ' + Tag);
  if LocalSvgName(Parent) = 'defs' then
    AddIssue('defined SVG content is not retained');
  if (LocalSvgName(Parent) = 'defs') and (Tag = 'defs') then
    AddIssue('nested defs');
  if (Tag = 'defs') and (LocalSvgName(Parent) <> 'svg') then
    AddIssue('defs outside the root element');
  TransformValue := Attributes.Values['transform'];
  StyleTransform := InlineStyleValue(Attributes.Values['style'], 'transform');
  if (TransformValue <> '') and (StyleTransform <> '') then
    AddIssue('competing transform declarations are import-only')
  else if TransformValue = '' then
    TransformValue := StyleTransform;
  LocalScale := 1;
  if TransformValue <> '' then
    ValidateTransform(TransformValue, LocalScale);
  if fDepth = 0 then ParentScale := 1
  else ParentScale := fTransformScaleStack[fDepth - 1];
  EffectiveScale := ParentScale * LocalScale;
  if IsNan(EffectiveScale) or IsInfinite(EffectiveScale) or
    (EffectiveScale <= 0) or (EffectiveScale > 1E12) then
    raise ESvgCodec.Create('SVG cumulative transform scale exceeds the supported range');
  fTransformScaleStack[fDepth] := EffectiveScale;
  if Tag = 'title' then
  begin
    Inc(fTitleCount);
    if fTitleCount > 1 then AddIssue('multiple SVG titles are not retained');
  end
  else if Tag = 'desc' then
  begin
    Inc(fDescCount);
    if fDescCount > 1 then
      AddIssue('multiple SVG descriptions are not retained');
  end;
  if ValueIn(LocalSvgName(Parent), ['title', 'desc']) and (Tag <> '') then
    raise ESvgCodec.Create('Nested markup in SVG metadata is not accepted');
  CheckAttributes(Tag, Attributes);
  fNames.Add(Name);
  Inc(fDepth);
end;

procedure TSvgProfileScan.EndElement(const Name: string);
begin
  if Name = '?xml' then Exit;
  if (Name <> '') and (Name[1] = '?') then Exit;
  if (fDepth <= 0) or (fNames[fNames.Count - 1] <> Name) then
    raise ESvgCodec.Create('Mismatched SVG element end: ' + Name);
  fNames.Delete(fNames.Count - 1);
  Dec(fDepth);
  if fDepth = 0 then fRootClosed := True;
end;

procedure TSvgProfileScan.Text(const Value: string);
var
  Tag: string;
begin
  if (fDepth = 0) and (Trim(Value) <> '') then
    raise ESvgCodec.Create('Text appears outside the SVG root element');
  if fDepth > 0 then
  begin
    Tag := LocalSvgName(fNames[fNames.Count - 1]);
    if (Tag = 'text') and not fTextPreserve and
      (Trim(Value) <> Value) then
      AddIssue('leading or trailing text whitespace is not retained');
    if (Trim(Value) <> '') and
      not ValueIn(Tag, ['title', 'desc', 'text', 'tspan']) then
      AddIssue('non-visual text outside metadata/text is not retained');
  end;
end;

procedure TSvgProfileScan.Comment(const Value: string);
begin
  if (Trim(Value) = 'Created by TpX drawing tool') and
    not fGeneratedCommentSeen then
    fGeneratedCommentSeen := True
  else
    AddIssue('source XML comment is not retained by the SVG exporter');
end;

procedure TSvgProfileScan.CDATA(const Value: string);
begin
  if Trim(Value) <> '' then AddIssue('CDATA content is not retained');
end;

procedure TSvgProfileScan.Doctype(const Name: string;
  const Attributes: TAttributes);
begin
  raise ESvgCodec.Create('DOCTYPE and entity declarations are not accepted');
end;

initialization
  RegisterSvgDocumentCodec;

end.
