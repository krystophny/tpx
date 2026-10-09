unit WebTeXPreview;

{$MODE Delphi}

interface

uses Classes, Graphics, Geometry, Devices;

var OnPreviewChanged: TNotifyEvent;

function DrawWebTeX(const Canvas: TCanvas; const ObjectKey: PtrUInt;
  const P: TPoint2D; const H, Rotation: Double; const Source: string;
  const HAlign: THAlignment; const VAlign: TVAlignment;
  const Color: TColor): Boolean;
procedure SetWebTeXEnabled(Value: Boolean);
procedure ForgetWebTeXObject(ObjectKey: PtrUInt);
procedure BrowserTeXReady;

implementation

uses Types, GraphType, IntfGraphics, LazCanvas;

function BrowserDrawTeX(Data: Pointer; Width, Height, Key: LongInt;
  X, Y, Size, Rotation: Double; Source: PChar; Count, HAlign, VAlign,
  Color, Left, Top, Right, Bottom: LongInt): LongInt;
  external 'tpx' name 'tex_draw';
procedure BrowserEnableTeX(Value: LongInt); external 'tpx' name 'tex_enable';
procedure BrowserForgetTeX(Key: PtrUInt); external 'tpx' name 'tex_forget';

function DrawWebTeX(const Canvas: TCanvas; const ObjectKey: PtrUInt;
  const P: TPoint2D; const H, Rotation: Double; const Source: string;
  const HAlign: THAlignment; const VAlign: TVAlignment;
  const Color: TColor): Boolean;
var
  Surface: TLazCanvas;
  Raw: TRawImage;
  Clip: TRect;
  RGB: LongInt;
begin
  Result := False;
  if (Source = '') or (H <= 0) then Exit;
  Surface := TLazCanvas(Canvas.Handle);
  TLazIntfImage(Surface.Image).GetRawImage(Raw);
  Clip := Bounds(0, 0, Surface.Width, Surface.Height);
  if Surface.Clipping then Clip := Surface.ClipRect;
  if Color = clDefault then RGB := 0 else RGB := ColorToRGB(Color);
  RGB := ((RGB and $FF) shl 16) or (RGB and $FF00) or ((RGB shr 16) and $FF);
  Result := BrowserDrawTeX(Raw.Data, Surface.Width, Surface.Height, ObjectKey,
    P.X + Surface.WindowOrg.X + Surface.BaseWindowOrg.X,
    P.Y + Surface.WindowOrg.Y + Surface.BaseWindowOrg.Y,
    H, Rotation, PChar(Source), Length(Source), Ord(HAlign), Ord(VAlign), RGB,
    Clip.Left, Clip.Top, Clip.Right, Clip.Bottom) <> 0;
end;

procedure SetWebTeXEnabled(Value: Boolean);
begin
  BrowserEnableTeX(Ord(Value));
end;

procedure BrowserTeXReady;
begin
  if Assigned(OnPreviewChanged) then OnPreviewChanged(nil);
end;

procedure ForgetWebTeXObject(ObjectKey: PtrUInt);
begin
  BrowserForgetTeX(ObjectKey);
end;

exports BrowserTeXReady name 'tpx_tex_ready';

end.
