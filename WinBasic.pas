unit WinBasic;

interface

uses Windows, Graphics, Classes;

type

  {: This type defines the type used to specify the name of
     a <I=font type face> (like Times New Roman).
  }
  TFaceName = string[LF_FACESIZE];
  {: This class encapsulates the interface for the GDI font of
     Windows as defined by <Code=TLOGFONT> structure.

     This font is used by the library for the <See Class=TText2D>
     shape class.
  }
  TExtendedFont = class(TObject)
  private
    FHandle: HFONT;
    fCanvas: TCanvas;
    procedure SetNewValue;
    procedure SetCanvas(Cnv: TCanvas);
    procedure SetHeight(Value: Integer);
    function GetHeight: Integer;
    procedure SetWidth(Value: Word);
    function GetWidth: Word;
    procedure SetAngle(Value: Word);
    function GetAngle: Word;
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
{$IFNDEF VER140}
    LogFont: TLOGFONT;
{$ELSE}
    LogFont: tagLOGFONTA;
{$ENDIF}
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
    property Height: Integer read GetHeight write SetHeight;
    {: This property specifies the <I=lfWidth> field of <Code=TLOGFONT>.
    }
    property Width: Word read GetWidth write SetWidth;
    {: This property specifies the <I=lfEscapement> field of
       <Code=TLOGFONT>.
    }
    property Angle: Word read GetAngle write
      SetAngle;
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

  TEnhMetaHeader = Windows.TEnhMetaHeader;

function GetLongPath(const ShortName: string): string;
function GetLongPathName(lpszShortName: LPCTSTR;
  lpszLongName: LPTSTR; cchBuffer: DWORD): DWORD; stdcall;
function HasFontFamily(const FontName: string): Boolean;
procedure SetBkMode(Canvas: TCanvas; const OPAQUE: Boolean);
procedure GetCanvasResolution(Canvas: TCanvas; var LogHeight,
  LogWidth: Double);
procedure ChangeCanvasResolution(Canvas: TCanvas; const WX, WY, VX,
  VY: Integer);
// Resolution in points per mm
procedure GetMetaFileResolution(Metafile: TMetaFile; var PpMM_X,
  PpMM_Y: Double);
function SetWindowLong(HWnd: LongWord; dwNewLong: Longint):
  Longint;

implementation

function GetLongPathName; external kernel32 name 'GetLongPathNameA';

function GetLongPath(const ShortName: string): string;
begin
  SetLength(Result, MAX_PATH + 1);
  SetLength(Result, GetLongPathName(PChar(ShortName), PChar(Result),
MAX_PATH));
end;

procedure Set_FaceName(
  var LogFont: TLOGFONT; const Value: TFaceName);
var
  I: Byte;
begin
  for I := 1 to Length(Value) do
    LogFont.lfFaceName[I - 1] := Value[I];
  LogFont.lfFaceName[Length(Value)] := #0;
end;

function EnumFontsProc(var LogFont: TLOGFONT;
  var TextMetric: TTextMetric;
  FontType: Integer; Data: Pointer): Integer; stdcall;
begin
  //LogFont.lfFaceName;
  Boolean(Data^) := True;
  Result := 0;
end;

function HasFontFamily(const FontName: string): Boolean;
var
  DC: HDC;
  LFont: TLOGFONT;
begin
  Result := False;
  if FontName = '' then Exit;
  FillChar(LFont, sizeof(LFont), 0);
  LFont.lfCharset := DEFAULT_CHARSET;
  Set_FaceName(LFont, FontName);
  try
    DC := GetDC(0);
    EnumFontFamiliesEx(DC, LFont, @EnumFontsProc,
      Longint(@Result), 0);
  finally
    ReleaseDC(0, DC);
  end;
end;

// =====================================================================
// TExtendedFont
// =====================================================================

procedure TExtendedFont.SetHeight(Value: Integer);
begin
  LogFont.lfHeight := Value;
  SetNewValue;
end;

function TExtendedFont.GetHeight: Integer;
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

procedure TExtendedFont.SetAngle(Value: Word);
begin
  LogFont.lfEscapement := Value;
  LogFont.lfOrientation := Value;
  SetNewValue;
end;

function TExtendedFont.GetAngle: Word;
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
  LogFont.lfCharset := Value;
  SetNewValue;
end;

function TExtendedFont.GetCharSet: Byte;
begin
  Result := LogFont.lfCharset;
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
  GetObject(GetStockObject(DEFAULT_GUI_FONT), sizeof(LogFont),
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
    Write(LogFont, sizeof(LogFont));
end;

procedure TExtendedFont.LoadFromStream(Strm: TStream);
begin
  with Strm do
  begin
    Read(LogFont, sizeof(LogFont));
    SetNewValue;
  end;
end;

procedure SetBkMode(Canvas: TCanvas; const OPAQUE: Boolean);
begin
  if not OPAQUE then
    Windows.SetBkMode(Canvas.Handle, Windows.TRANSPARENT)
  else
    Windows.SetBkMode(Canvas.Handle, Windows.OPAQUE);
end;

  // Questa è la dimensione di un pixel in mm.

procedure GetCanvasResolution(Canvas: TCanvas; var LogHeight,
  LogWidth: Double);
begin
  LogHeight := GetDeviceCaps(Canvas.Handle, VERTSIZE) /
    GetDeviceCaps(Canvas.Handle, VERTRES);
  LogWidth := GetDeviceCaps(Canvas.Handle, HORZSIZE) /
    GetDeviceCaps(Canvas.Handle, HORZRES);
end;

procedure ChangeCanvasResolution(Canvas: TCanvas; const WX, WY, VX,
  VY: Integer);
begin
  SetMapMode(Canvas.Handle, MM_ISOTROPIC);
  SetWindowExtEx(Canvas.Handle, WX, WY, nil);
  SetViewportExtEx(Canvas.Handle, VX, VY, nil);
end;

procedure GetMetaFileResolution(Metafile: TMetaFile; var PpMM_X,
  PpMM_Y: Double);
var EMFHeader: TEnhMetaHeader;
begin
  GetEnhMetaFileHeader(Metafile.Handle, sizeof(EMFHeader),
    @EMFHeader);
  PpMM_X := EMFHeader.szlDevice.CX / EMFHeader.szlMillimeters.CX;
  PpMM_Y := EMFHeader.szlDevice.CY / EMFHeader.szlMillimeters.CY;
end;

function SetWindowLong(HWnd: LongWord; dwNewLong: Longint):
  Longint;
begin
  Result := Windows.SetWindowLong(HWnd, gwl_wndProc, dwNewLong);
end;

end.

