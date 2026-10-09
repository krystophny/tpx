unit LiveTeX;

{$mode Delphi}

interface

uses Classes, Graphics, Geometry, Devices;

var
  LiveTeXEnabled: Boolean = True;
  LiveTeXStatus: string = '';

procedure InitializeLiveTeX(const Changed: TNotifyEvent);
procedure ShutdownLiveTeX;
procedure SetLiveTeXEnabled(Value: Boolean);
procedure ForgetLiveTeXObject(ObjectKey: PtrUInt);
function LiveTeXCompilationCount: Integer;
function LiveTeXPending: Boolean;
function DrawLiveTeX(const Canvas: TCanvas; const ObjectKey: PtrUInt;
  const P: TPoint2D; const H, Rotation: Double; const Source: string;
  const HAlign: THAlignment; const VAlign: TVAlignment;
  const Color: TColor): Boolean;

implementation

{$IFDEF CPUWASM32}
uses WebTeXPreview;

procedure InitializeLiveTeX(const Changed: TNotifyEvent);
begin
  OnPreviewChanged := Changed;
end;

procedure ShutdownLiveTeX;
begin
  OnPreviewChanged := nil;
  SetWebTeXEnabled(False);
end;

procedure SetLiveTeXEnabled(Value: Boolean);
begin
  LiveTeXEnabled := Value;
  SetWebTeXEnabled(Value);
end;

procedure ForgetLiveTeXObject(ObjectKey: PtrUInt);
begin
  ForgetWebTeXObject(ObjectKey);
end;

function LiveTeXCompilationCount: Integer;
begin
  Result := 0;
end;

function LiveTeXPending: Boolean;
begin
  Result := False;
end;

function DrawLiveTeX(const Canvas: TCanvas; const ObjectKey: PtrUInt;
  const P: TPoint2D; const H, Rotation: Double; const Source: string;
  const HAlign: THAlignment; const VAlign: TVAlignment;
  const Color: TColor): Boolean;
begin
  Result := LiveTeXEnabled and (Source <> '') and
    DrawWebTeX(Canvas, ObjectKey, P, H, Rotation, Source, HAlign, VAlign, Color);
end;
{$ELSE}

uses SysUtils, Math, Types, Forms, ExtCtrls, Contnrs, MD5,
  IntfGraphics, FPImage, FileUtil, LazFileUtils,
  SysBasic, Preview, TeXPreviewWorker;

type
  TPreviewEntry = class
    Key, Source, Preamble, LatexCommand, Error: string;
    Image: TPortableNetworkGraphic;
    RasterDPI, WantedDPI, Version: Integer;
    WidthPt, HeightPt, DepthPt: Double;
    Pending, Running: Boolean;
    destructor Destroy; override;
  end;

  TPreviewObject = class
    Current, LastGood: TPreviewEntry;
    Rotated: TPortableNetworkGraphic;
    Image: TPortableNetworkGraphic;
    Angle: Double;
    Version: Integer;
    OffsetX, OffsetY: Integer;
    destructor Destroy; override;
  end;

  TTeXPreviewManager = class
  private
    FEntries, FObjects: TStringList;
    FTimer: TTimer;
    FWorker: TTeXPreviewWorker;
    FChanged: TNotifyEvent;
    FDirectory, FPreamble: string;
    FPreambleAge: LongInt;
    FClosing: Boolean;
    FRuns: Integer;
    procedure Schedule(Sender: TObject);
    procedure WorkerTerminated(Sender: TObject);
    procedure CollectResults(Data: PtrInt);
    procedure ReadPreamble;
    function IsUsed(Entry: TPreviewEntry): Boolean;
  public
    constructor Create(const Changed: TNotifyEvent);
    destructor Destroy; override;
    procedure Enable(Value: Boolean);
    function Draw(const Canvas: TCanvas; ObjectKey: PtrUInt;
      const P: TPoint2D; H, Rotation: Double; const Source: string;
      HAlign: THAlignment; VAlign: TVAlignment; Color: TColor): Boolean;
  end;

var
  Manager: TTeXPreviewManager;

destructor TPreviewEntry.Destroy;
begin
  Image.Free;
  inherited;
end;

destructor TPreviewObject.Destroy;
begin
  Rotated.Free;
  inherited;
end;

constructor TTeXPreviewManager.Create(const Changed: TNotifyEvent);
var
  ID: TGUID;
begin
  inherited Create;
  FChanged := Changed;
  FEntries := TStringList.Create;
  FEntries.Sorted := True;
  FEntries.OwnsObjects := True;
  FObjects := TStringList.Create;
  FObjects.Sorted := True;
  FObjects.OwnsObjects := True;
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.Interval := 180;
  FTimer.OnTimer := Schedule;
  CreateGUID(ID);
  FDirectory := IncludeTrailingPathDelimiter(GetAppConfigDir(False)) +
    'preview-cache' + PathDelim + GUIDToString(ID);
  FPreambleAge := -2;
  ReadPreamble;
end;

destructor TTeXPreviewManager.Destroy;
begin
  FClosing := True;
  FTimer.Free;
  if Assigned(FWorker) then begin
    FWorker.OnTerminate := nil;
    FWorker.Terminate;
    FWorker.WaitFor;
    FWorker.Free;
  end;
  Application.RemoveAsyncCalls(Self);
  FObjects.Free;
  FEntries.Free;
  if DirectoryExists(FDirectory) then DeleteDirectory(FDirectory, False);
  inherited;
end;

procedure TTeXPreviewManager.ReadPreamble;
var
  Name: string;
  Age: LongInt;
  Lines: TStringList;
begin
  Name := TpXTemplatePath('preview.tex.inc');
  Age := FileAge(Name);
  if Age = FPreambleAge then Exit;
  FPreambleAge := Age;
  Lines := TStringList.Create;
  try
    if FileExists(Name) then Lines.LoadFromFile(Name)
    else Lines.Add('\documentclass[10pt]{article}');
    FPreamble := Lines.Text;
  finally
    Lines.Free;
  end;
end;

function TTeXPreviewManager.IsUsed(Entry: TPreviewEntry): Boolean;
var
  I: Integer;
begin
  for I := 0 to FObjects.Count - 1 do
    if TPreviewObject(FObjects.Objects[I]).Current = Entry then Exit(True);
  Result := False;
end;

procedure TTeXPreviewManager.Schedule(Sender: TObject);
var
  Inputs: TTeXPreviewInputs;
  Entry: TPreviewEntry;
  I, N: Integer;
begin
  FTimer.Enabled := False;
  if FClosing or not LiveTeXEnabled or Assigned(FWorker) then Exit;
  SetLength(Inputs, FEntries.Count);
  N := 0;
  for I := 0 to FEntries.Count - 1 do begin
    Entry := TPreviewEntry(FEntries.Objects[I]);
    if Entry.Pending and IsUsed(Entry) then begin
      Inputs[N].Key := Entry.Key;
      Inputs[N].Source := Entry.Source;
      Inputs[N].Preamble := Entry.Preamble;
      Inputs[N].LatexCommand := Entry.LatexCommand;
      Inputs[N].RasterDPI := Entry.WantedDPI;
      Entry.Pending := False;
      Entry.Running := True;
      Inc(N);
    end;
  end;
  SetLength(Inputs, N);
  if N = 0 then Exit;
  FWorker := TTeXPreviewWorker.Create(Inputs, FDirectory, WorkerTerminated);
  FWorker.Start;
end;

procedure TTeXPreviewManager.WorkerTerminated(Sender: TObject);
begin
  if not FClosing then Application.QueueAsyncCall(CollectResults, 0);
end;

procedure TTeXPreviewManager.CollectResults(Data: PtrInt);
var
  I, J: Integer;
  Entry: TPreviewEntry;
  PNG: TPortableNetworkGraphic;
begin
  if FClosing or not Assigned(FWorker) then Exit;
  Inc(FRuns, FWorker.Runs);
  for I := 0 to High(FWorker.Results) do begin
    J := FEntries.IndexOf(FWorker.Results[I].Key);
    if J < 0 then Continue;
    Entry := TPreviewEntry(FEntries.Objects[J]);
    Entry.Running := False;
    Entry.Error := FWorker.Results[I].Error;
    if (Entry.Error = '') and FileExists(FWorker.Results[I].PNGFile) then begin
      PNG := TPortableNetworkGraphic.Create;
      try
        PNG.LoadFromFile(FWorker.Results[I].PNGFile);
        FreeAndNil(Entry.Image);
        Entry.Image := PNG;
        Inc(Entry.Version);
        PNG := nil;
        Entry.RasterDPI := FWorker.Results[I].RasterDPI;
        Entry.WidthPt := FWorker.Results[I].WidthPt;
        Entry.HeightPt := FWorker.Results[I].HeightPt;
        Entry.DepthPt := FWorker.Results[I].DepthPt;
      except
        on E: Exception do Entry.Error := E.Message;
      end;
      PNG.Free;
    end;
    if (Entry.Error <> '') and IsUsed(Entry) then
      LiveTeXStatus := 'LaTeX preview: ' + Copy(Entry.Error, 1, 180);
  end;
  FreeAndNil(FWorker);
  if LiveTeXEnabled then begin
    if Assigned(FChanged) then FChanged(Self);
    Schedule(nil);
  end;
end;

procedure TTeXPreviewManager.Enable(Value: Boolean);
var
  I: Integer;
begin
  FTimer.Enabled := False;
  if not Value then begin
    if Assigned(FWorker) then FWorker.Terminate;
  end else begin
    for I := 0 to FEntries.Count - 1 do
      with TPreviewEntry(FEntries.Objects[I]) do
        if Error <> '' then begin Error := ''; Pending := True; end;
  end;
  LiveTeXStatus := '';
end;

procedure RotatePreview(Obj: TPreviewObject; Image: TPortableNetworkGraphic;
  Angle: Double; Version: Integer);
var
  Source, Target: TLazIntfImage;
  X, Y, SX, SY, W, H: Integer;
  C, S: Double;
  Corners: array[0..3] of TPoint;
  Transparent: TFPColor;
begin
  if (Obj.Image = Image) and (Obj.Angle = Angle) and (Obj.Version = Version) and Assigned(Obj.Rotated) then Exit;
  FreeAndNil(Obj.Rotated);
  Obj.Image := Image;
  Obj.Angle := Angle;
  Obj.Version := Version;
  C := Cos(Angle);
  S := Sin(Angle);
  W := Image.Width;
  H := Image.Height;
  Corners[0] := Point(0, 0);
  Corners[1] := Point(Round(W*C), Round(-W*S));
  Corners[2] := Point(Round(H*S), Round(H*C));
  Corners[3] := Point(Corners[1].X+Corners[2].X, Corners[1].Y+Corners[2].Y);
  Obj.OffsetX := Min(Min(0,Corners[1].X),Min(Corners[2].X,Corners[3].X));
  Obj.OffsetY := Min(Min(0,Corners[1].Y),Min(Corners[2].Y,Corners[3].Y));
  W := Max(Max(0,Corners[1].X),Max(Corners[2].X,Corners[3].X))-Obj.OffsetX+1;
  H := Max(Max(0,Corners[1].Y),Max(Corners[2].Y,Corners[3].Y))-Obj.OffsetY+1;
  Source := Image.CreateIntfImage;
  Target := TLazIntfImage.Create(0,0);
  try
    Target.DataDescription := Source.DataDescription;
    Target.SetSize(W,H);
    Transparent := colTransparent;
    for Y := 0 to H-1 do
      for X := 0 to W-1 do begin
        SX := Round((X+Obj.OffsetX)*C-(Y+Obj.OffsetY)*S);
        SY := Round((X+Obj.OffsetX)*S+(Y+Obj.OffsetY)*C);
        if (SX>=0) and (SY>=0) and (SX<Source.Width) and (SY<Source.Height) then
          Target.Colors[X,Y] := Source.Colors[SX,SY]
        else Target.Colors[X,Y] := Transparent;
      end;
    Obj.Rotated := TPortableNetworkGraphic.Create;
    Obj.Rotated.LoadFromIntfImage(Target);
  finally
    Source.Free;
    Target.Free;
  end;
end;

function TTeXPreviewManager.Draw(const Canvas: TCanvas; ObjectKey: PtrUInt;
  const P: TPoint2D; H, Rotation: Double; const Source: string;
  HAlign: THAlignment; VAlign: TVAlignment; Color: TColor): Boolean;
var
  Key, Text: string;
  I, DPI: Integer;
  Entry: TPreviewEntry;
  Obj: TPreviewObject;
  RGB: LongInt;
  W, A, D, X, Y, Scale, C, S: Double;
  R: TRect;
  Image: TPortableNetworkGraphic;
begin
  Result := False;
  if not LiveTeXEnabled or (Source='') or (H<=0) then Exit;
  ReadPreamble;
  RGB := ColorToRGB(Color);
  if Color = clDefault then RGB := 0;
  Text := Format('{\fontsize{10}{12}\selectfont\color[RGB]{%d,%d,%d}%s}',
    [RGB and 255,(RGB shr 8) and 255,(RGB shr 16) and 255,Source]);
  Key := MD5Print(MD5String(FPreamble+#0+LatexPath+#0+Text));
  I := FEntries.IndexOf(Key);
  if I<0 then begin
    Entry := TPreviewEntry.Create;
    Entry.Key := Key;
    Entry.Source := Text;
    Entry.Preamble := FPreamble;
    Entry.LatexCommand := LatexPath;
    Entry.Pending := True;
    I := FEntries.AddObject(Key, Entry);
  end;
  Entry := TPreviewEntry(FEntries.Objects[I]);
  DPI := 288;
  while (DPI < H*72/10*2) and (DPI<2304) do DPI := DPI*2;
  Entry.WantedDPI := Max(Entry.WantedDPI,DPI);
  if (Entry.RasterDPI<DPI) and (Entry.Error='') and not Entry.Running then Entry.Pending := True;
  Key := IntToHex(ObjectKey,SizeOf(ObjectKey)*2);
  I := FObjects.IndexOf(Key);
  if I<0 then I := FObjects.AddObject(Key,TPreviewObject.Create);
  Obj := TPreviewObject(FObjects.Objects[I]);
  if Obj.Current<>Entry then begin
    Obj.Current := Entry;
    if Entry.Pending then FTimer.Enabled := False;
  end;
  if Entry.Pending and not Assigned(FWorker) then FTimer.Enabled := True;
  if Assigned(Entry.Image) then Obj.LastGood := Entry;
  Entry := Obj.LastGood;
  if not Assigned(Entry) or not Assigned(Entry.Image) then Exit;
  W := Entry.WidthPt*H/10;
  A := Entry.HeightPt*H/10;
  D := Entry.DepthPt*H/10;
  X := 0;
  case HAlign of
    ahCenter: X := -W/2;
    ahRight: X := -W;
  end;
  Y := -A;
  case VAlign of
    jvBottom: Y := -A-D;
    jvCenter: Y := -(A+D)/2;
    jvTop: Y := 0;
  end;
  C := Cos(Rotation);
  S := Sin(Rotation);
  Image := Entry.Image;
  Scale := H/10*72.27/Entry.RasterDPI;
  if Abs(Rotation)>0.00001 then begin
    RotatePreview(Obj,Image,Rotation,Entry.Version);
    Image := Obj.Rotated;
    R.Left := Round(P.X+X*C+Y*S+Obj.OffsetX*Scale);
    R.Top := Round(P.Y-X*S+Y*C+Obj.OffsetY*Scale);
  end else begin
    R.Left := Round(P.X+X);
    R.Top := Round(P.Y+Y);
  end;
  R.Right := R.Left+Max(1,Round(Image.Width*Scale));
  R.Bottom := R.Top+Max(1,Round(Image.Height*Scale));
  Canvas.StretchDraw(R,Image);
  Result := True;
end;

procedure InitializeLiveTeX(const Changed: TNotifyEvent);
begin
  ShutdownLiveTeX;
  Manager := TTeXPreviewManager.Create(Changed);
end;

procedure ShutdownLiveTeX;
begin
  FreeAndNil(Manager);
end;

procedure SetLiveTeXEnabled(Value: Boolean);
begin
  LiveTeXEnabled := Value;
  if Assigned(Manager) then Manager.Enable(Value);
end;

procedure ForgetLiveTeXObject(ObjectKey: PtrUInt);
var
  I: Integer;
begin
  if not Assigned(Manager) then Exit;
  I := Manager.FObjects.IndexOf(IntToHex(ObjectKey,SizeOf(ObjectKey)*2));
  if I>=0 then Manager.FObjects.Delete(I);
end;

function LiveTeXCompilationCount: Integer;
begin
  Result := 0;
  if Assigned(Manager) then Result := Manager.FRuns;
end;

function LiveTeXPending: Boolean;
begin
  Result := Assigned(Manager) and
    (Assigned(Manager.FWorker) or Manager.FTimer.Enabled);
end;

function DrawLiveTeX(const Canvas: TCanvas; const ObjectKey: PtrUInt;
  const P: TPoint2D; const H, Rotation: Double; const Source: string;
  const HAlign: THAlignment; const VAlign: TVAlignment;
  const Color: TColor): Boolean;
begin
  Result := Assigned(Manager) and LiveTeXEnabled and
    Manager.Draw(Canvas,ObjectKey,P,H,Rotation,Source,HAlign,VAlign,Color);
end;

finalization
  ShutdownLiveTeX;
{$ENDIF}

end.
