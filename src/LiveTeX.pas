unit LiveTeX;

{$mode Delphi}

interface

uses Classes, Graphics, Geometry, Devices, Types, Drawings;

const
  LiveTeXTrustStatus =
    'TeX preview blocked; approve this file in View to render.';

var
  LiveTeXEnabled: Boolean = True;
  LiveTeXStatus: string = '';
  LiveTeXDocumentTrusted: Boolean = True;
  LiveTeXDocumentGeneration: QWord = 1;

procedure InitializeLiveTeX(const Changed: TNotifyEvent);
procedure ConfigureLiveTeX(const Drawing: TDrawing2D);
procedure ShutdownLiveTeX;
procedure BeginLiveTeXDocument(Trusted: Boolean);
procedure SetLiveTeXDocumentTrusted(Trusted: Boolean);
procedure SetLiveTeXEnabled(Value: Boolean);
procedure FlushLiveTeX;
procedure ForgetLiveTeXObject(ObjectKey: PtrUInt);
function LiveTeXCompilationCount: Integer;
function LiveTeXPending: Boolean;
function TakeLiveTeXDamage(out R: TRect): Boolean;
function DrawLiveTeX(const Canvas: TCanvas; const ObjectKey: PtrUInt;
  const P: TPoint2D; const H, Rotation: Double; const Source: string;
  const HAlign: THAlignment; const VAlign: TVAlignment;
  const Color: TColor; FallbackRadius: Double = 0): Boolean;

implementation

{$IFDEF CPUWASM32}
uses WebTeXPreview;

procedure InitializeLiveTeX(const Changed: TNotifyEvent);
begin
  OnPreviewChanged := Changed;
end;

procedure ConfigureLiveTeX(const Drawing: TDrawing2D);
begin
end;

procedure ShutdownLiveTeX;
begin
  OnPreviewChanged := nil;
  SetWebTeXEnabled(False);
end;

procedure BeginLiveTeXDocument(Trusted: Boolean);
begin
  Inc(LiveTeXDocumentGeneration);
  LiveTeXDocumentTrusted := Trusted;
  if not Trusted then LiveTeXStatus := LiveTeXTrustStatus
  else LiveTeXStatus := '';
  if Assigned(OnPreviewChanged) then OnPreviewChanged(nil);
end;

procedure SetLiveTeXDocumentTrusted(Trusted: Boolean);
begin
  if LiveTeXDocumentTrusted = Trusted then Exit;
  BeginLiveTeXDocument(Trusted);
end;

procedure SetLiveTeXEnabled(Value: Boolean);
begin
  LiveTeXEnabled := Value;
  SetWebTeXEnabled(Value);
  if not LiveTeXDocumentTrusted then LiveTeXStatus := LiveTeXTrustStatus
  else LiveTeXStatus := '';
  if Assigned(OnPreviewChanged) then OnPreviewChanged(nil);
end;

procedure FlushLiveTeX;
begin
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

function TakeLiveTeXDamage(out R: TRect): Boolean;
begin
  R := Rect(0,0,0,0);
  Result := False;
end;

function DrawLiveTeX(const Canvas: TCanvas; const ObjectKey: PtrUInt;
  const P: TPoint2D; const H, Rotation: Double; const Source: string;
  const HAlign: THAlignment; const VAlign: TVAlignment;
  const Color: TColor; FallbackRadius: Double): Boolean;
begin
  Result := LiveTeXEnabled and LiveTeXDocumentTrusted and (Source <> '') and
    DrawWebTeX(Canvas, ObjectKey, P, H, Rotation, Source, HAlign, VAlign, Color);
end;
{$ELSE}

uses SysUtils, Math, Forms, ExtCtrls, MD5, FileUtil, LazFileUtils,
  SysBasic, Preview, TeXPreviewWorker, TeXPreviewCache;

type
  TPreviewEntry = class
    Key, ContentKey, Source, Preamble, LatexCommand, Error, ImageFile: string;
    Image: TPortableNetworkGraphic;
    RasterDPI, WantedDPI, OffsetX, OffsetY: Integer;
    Rotation: Double;
    WidthPt, HeightPt, DepthPt: Double;
    Pending, Running: Boolean;
    destructor Destroy; override;
  end;

  TPreviewObject = class
    Current, LastGood: TPreviewEntry;
    P: TPoint2D;
    H: Double;
    HAlign: THAlignment;
    VAlign: TVAlignment;
    Bounds: TRect;
  end;

  TTeXPreviewManager = class
  private
    FEntries, FObjects: TStringList;
    FTimer: TTimer;
    FWorker: TTeXPreviewWorker;
    FChanged: TNotifyEvent;
    FDirectory, FPreamble, FPackages: string;
    FFigurePrologue, FFigureEpilogue, FPicturePrologue, FPictureEpilogue: string;
    FTeXFormat, FTeXFigure: Integer;
    FCache: TTeXPreviewCache;
    FPreambleAge: LongInt;
    FClosing, FWorkerCancelled: Boolean;
    FDocumentGeneration, FWorkerDocumentGeneration: QWord;
    FRuns: Integer;
    FDamage: TRect;
    FHasDamage: Boolean;
    procedure AddDamage(const R: TRect);
    function Bounds(Obj: TPreviewObject; Entry: TPreviewEntry): TRect;
    procedure Prune;
    procedure Schedule(Sender: TObject);
    procedure WorkerTerminated(Sender: TObject);
    procedure CollectResults(Data: PtrInt);
    procedure ReadPreamble;
    function IsUsed(Entry: TPreviewEntry): Boolean;
    procedure CancelUnusedWorker;
    procedure InvalidateDocument;
  public
    constructor Create(const Changed: TNotifyEvent);
    destructor Destroy; override;
    procedure Enable(Value: Boolean);
    function Draw(const Canvas: TCanvas; ObjectKey: PtrUInt;
      const P: TPoint2D; H, Rotation: Double; const Source: string;
      HAlign: THAlignment; VAlign: TVAlignment; Color: TColor;
      FallbackRadius: Double): Boolean;
  end;

var
  Manager: TTeXPreviewManager;

destructor TPreviewEntry.Destroy;
begin
  Image.Free;
  inherited;
end;

constructor TTeXPreviewManager.Create(const Changed: TNotifyEvent);
begin
  inherited Create;
  FChanged := Changed;
  FDocumentGeneration := LiveTeXDocumentGeneration;
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
  FCache := TTeXPreviewCache.Create(
    IncludeTrailingPathDelimiter(GetAppConfigDir(False))+'preview-cache');
  FDirectory := FCache.Directory;
  FPreambleAge := -2;
  FTeXFormat := -1;
  FTeXFigure := -1;
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
  FCache.Free;
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
    FPreamble := Lines.Text+FPackages;
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

procedure TTeXPreviewManager.CancelUnusedWorker;
var
  I: Integer;
  Entry: TPreviewEntry;
begin
  if not Assigned(FWorker) or FWorkerCancelled then Exit;
  for I := 0 to FEntries.Count-1 do begin
    Entry := TPreviewEntry(FEntries.Objects[I]);
    if Entry.Running and IsUsed(Entry) then Exit;
  end;
  FWorkerCancelled := True;
  FWorker.Terminate;
end;

procedure TTeXPreviewManager.InvalidateDocument;
begin
  FDocumentGeneration := LiveTeXDocumentGeneration;
  FTimer.Enabled := False;
  FObjects.Clear;
  FHasDamage := False;
  CancelUnusedWorker;
  if Assigned(FChanged) then FChanged(Self);
end;

procedure TTeXPreviewManager.Schedule(Sender: TObject);
var
  Inputs: TTeXPreviewInputs;
  Entry: TPreviewEntry;
  I, N: Integer;
begin
  FTimer.Enabled := False;
  if FClosing or not LiveTeXEnabled or not LiveTeXDocumentTrusted or
    Assigned(FWorker) then Exit;
  SetLength(Inputs, FEntries.Count);
  N := 0;
  for I := 0 to FEntries.Count - 1 do begin
    Entry := TPreviewEntry(FEntries.Objects[I]);
    if Entry.Pending and IsUsed(Entry) then begin
      Inputs[N].Key := Entry.Key;
      Inputs[N].ContentKey := Entry.ContentKey;
      Inputs[N].Rotation := Entry.Rotation;
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
  FWorkerDocumentGeneration := FDocumentGeneration;
  FWorker := TTeXPreviewWorker.Create(Inputs, FDirectory, WorkerTerminated);
  FWorker.Start;
end;

procedure TTeXPreviewManager.WorkerTerminated(Sender: TObject);
begin
  if not FClosing then Application.QueueAsyncCall(CollectResults, 0);
end;

procedure TTeXPreviewManager.CollectResults(Data: PtrInt);
var
  I, J, K: Integer;
  Entry: TPreviewEntry;
  Obj: TPreviewObject;
  PNG: TPortableNetworkGraphic;
  PreviousStatus: string;
begin
  if FClosing or not Assigned(FWorker) then Exit;
  if FWorkerDocumentGeneration <> FDocumentGeneration then begin
    for I := 0 to High(FWorker.Results) do begin
      J := FEntries.IndexOf(FWorker.Results[I].Key);
      if J < 0 then Continue;
      Entry := TPreviewEntry(FEntries.Objects[J]);
      Entry.Running := False;
      Entry.Error := '';
      Entry.Pending := LiveTeXDocumentTrusted and IsUsed(Entry);
    end;
    FreeAndNil(FWorker);
    FWorkerCancelled := False;
    if LiveTeXEnabled and LiveTeXDocumentTrusted then Schedule(nil);
    Exit;
  end;
  PreviousStatus := LiveTeXStatus;
  Inc(FRuns, FWorker.Runs);
  for I := 0 to High(FWorker.Results) do begin
    J := FEntries.IndexOf(FWorker.Results[I].Key);
    if J < 0 then Continue;
    Entry := TPreviewEntry(FEntries.Objects[J]);
    Entry.Running := False;
    if FWorkerCancelled then begin
      Entry.Error := '';
      Entry.Pending := LiveTeXDocumentTrusted and IsUsed(Entry);
      Continue;
    end;
    Entry.Error := FWorker.Results[I].Error;
    if (Entry.Error = '') and FileExists(FWorker.Results[I].PNGFile) then begin
      PNG := TPortableNetworkGraphic.Create;
      try
        PNG.LoadFromFile(FWorker.Results[I].PNGFile);
        FreeAndNil(Entry.Image);
        Entry.Image := PNG;
        Entry.ImageFile := FWorker.Results[I].PNGFile;
        PNG := nil;
        Entry.RasterDPI := FWorker.Results[I].RasterDPI;
        Entry.OffsetX := FWorker.Results[I].OffsetX;
        Entry.OffsetY := FWorker.Results[I].OffsetY;
        Entry.WidthPt := FWorker.Results[I].WidthPt;
        Entry.HeightPt := FWorker.Results[I].HeightPt;
        Entry.DepthPt := FWorker.Results[I].DepthPt;
      except
        on E: Exception do Entry.Error := E.Message;
      end;
      PNG.Free;
      if Assigned(Entry.Image) then
        for K := 0 to FObjects.Count-1 do begin
          Obj := TPreviewObject(FObjects.Objects[K]);
          if Obj.Current = Entry then begin
            AddDamage(Obj.Bounds);
            AddDamage(Bounds(Obj,Entry));
          end;
        end;
    end;

  end;
  FreeAndNil(FWorker);
  FWorkerCancelled := False;
  LiveTeXStatus := '';
  for I := 0 to FEntries.Count-1 do begin
    Entry := TPreviewEntry(FEntries.Objects[I]);
    if (Entry.Error<>'') and IsUsed(Entry) then
      LiveTeXStatus := 'LaTeX preview: '+Copy(Entry.Error,1,180);
  end;
  Prune;
  if LiveTeXEnabled then begin
    if Assigned(FChanged) and (FHasDamage or (PreviousStatus<>LiveTeXStatus)) then
      FChanged(Self);
    Schedule(nil);
  end;
end;

procedure TTeXPreviewManager.Enable(Value: Boolean);
var
  I: Integer;
begin
  FTimer.Enabled := False;
  if not Value then begin
    if Assigned(FWorker) then begin
      FWorkerCancelled := True;
      FWorker.Terminate;
    end;
  end else begin
    for I := 0 to FEntries.Count - 1 do
      with TPreviewEntry(FEntries.Objects[I]) do
        if Error <> '' then begin Error := ''; Pending := True; end;
  end;
  LiveTeXStatus := '';
  if not LiveTeXDocumentTrusted then LiveTeXStatus := LiveTeXTrustStatus;
  if Value and LiveTeXDocumentTrusted then Schedule(nil);
end;

procedure TTeXPreviewManager.AddDamage(const R: TRect);
begin
  if not FHasDamage then FDamage := R
  else UnionRect(FDamage,FDamage,R);
  FHasDamage := True;
end;

procedure TTeXPreviewManager.Prune;
var
  I,J: Integer;
  Entry: TPreviewEntry;
  Used: Boolean;
  Content, Images, Jobs, Data: TStringList;
  Search: TSearchRec;
  Base, Name: string;
begin
  I := FEntries.Count-1;
  while (I>=0) and (FEntries.Count>256) do begin
    Entry := TPreviewEntry(FEntries.Objects[I]);
    Used := False;
    for J := 0 to FObjects.Count-1 do
      with TPreviewObject(FObjects.Objects[J]) do
        if (Current=Entry) or (LastGood=Entry) then Used := True;
    if not Used then FEntries.Delete(I);
    Dec(I);
  end;
  { A job DVI may back several labels. Keep it only while a retained content
    manifest refers to it; expired variants and failed job files have no owner. }
  Content := TStringList.Create;
  Images := TStringList.Create;
  Jobs := TStringList.Create;
  Data := TStringList.Create;
  try
    Content.Sorted := True;
    Content.Duplicates := dupIgnore;
    Images.Sorted := True;
    Images.Duplicates := dupIgnore;
    Jobs.Sorted := True;
    Jobs.Duplicates := dupIgnore;
    for I := 0 to FEntries.Count-1 do begin
      Entry := TPreviewEntry(FEntries.Objects[I]);
      Content.Add(MD5Print(MD5String(Entry.ContentKey)));
      if Entry.ImageFile<>'' then Images.Add(ExtractFileName(Entry.ImageFile));
    end;
    if FindFirst(FDirectory+PathDelim+'*',faAnyFile,Search)=0 then begin
      repeat
        if Search.Attr and faDirectory<>0 then Continue;
        Name := Search.Name;
        Base := Copy(Name,1,32);
        if (Content.IndexOf(Base)<0) or
          ((Pos('-r',Name)>0) and (Images.IndexOf(Name)<0)) then
          DeleteFile(FDirectory+PathDelim+Name)
        else if ExtractFileExt(Name)='.cache' then begin
          Data.LoadFromFile(FDirectory+PathDelim+Name);
          if Data.Count>0 then
            Jobs.Add(ExcludeTrailingPathDelimiter(ExtractFilePath(Data[0])));
        end;
      until FindNext(Search)<>0;
      FindClose(Search);
    end;
    if FindFirst(FDirectory+PathDelim+'job-*',faDirectory,Search)=0 then begin
      repeat
        if (Search.Attr and faDirectory<>0) and
          (Jobs.IndexOf(Search.Name)<0) then
          DeleteDirectory(FDirectory+PathDelim+Search.Name,False);
      until FindNext(Search)<>0;
      FindClose(Search);
    end;
  finally
    Data.Free;
    Jobs.Free;
    Images.Free;
    Content.Free;
  end;
end;

function TTeXPreviewManager.Bounds(Obj: TPreviewObject;
  Entry: TPreviewEntry): TRect;
var
  W,A,D,X,Y,Scale,C,S: Double;
begin
  W := Entry.WidthPt*Obj.H/10;
  A := Entry.HeightPt*Obj.H/10;
  D := Entry.DepthPt*Obj.H/10;
  X := 0;
  case Obj.HAlign of
    ahCenter: X := -W/2;
    ahRight: X := -W;
  end;
  Y := -A;
  case Obj.VAlign of
    jvBottom: Y := -A-D;
    jvCenter: Y := -(A+D)/2;
    jvTop: Y := 0;
  end;
  C := Cos(Entry.Rotation);
  S := Sin(Entry.Rotation);
  Scale := Obj.H/10*72.27/Entry.RasterDPI;
  Result.Left := Round(Obj.P.X+X*C+Y*S+Entry.OffsetX*Scale);
  Result.Top := Round(Obj.P.Y-X*S+Y*C+Entry.OffsetY*Scale);
  Result.Right := Result.Left+Max(1,Round(Entry.Image.Width*Scale));
  Result.Bottom := Result.Top+Max(1,Round(Entry.Image.Height*Scale));
end;

function TTeXPreviewManager.Draw(const Canvas: TCanvas; ObjectKey: PtrUInt;
  const P: TPoint2D; H, Rotation: Double; const Source: string;
  HAlign: THAlignment; VAlign: TVAlignment; Color: TColor;
      FallbackRadius: Double): Boolean;
var
  Key, ContentKey, Text: string;
  I, DPI: Integer;
  Entry: TPreviewEntry;
  Obj: TPreviewObject;
  RGB: LongInt;
  Radius: Integer;
begin
  Result := False;
  if not LiveTeXEnabled or not LiveTeXDocumentTrusted or (H<=0) then Exit;
  if Source='' then begin
    ForgetLiveTeXObject(ObjectKey);
    Exit;
  end;
  ReadPreamble;
  RGB := ColorToRGB(Color);
  if Color = clDefault then RGB := 0;
  Text := Format('{\fontsize{10}{12}\selectfont\color[RGB]{%d,%d,%d}%s}',
    [RGB and 255,(RGB shr 8) and 255,(RGB shr 16) and 255,Source+'%'+LineEnding]);
  { Match the export's hook order, inside the worker's per-label hbox group.
    Local definitions and balanced hook pairs must not leak to other labels. }
  if FPicturePrologue<>'' then Text := FPicturePrologue+'%'+LineEnding+Text;
  if FFigurePrologue<>'' then Text := FFigurePrologue+'%'+LineEnding+Text;
  if FPictureEpilogue<>'' then Text := Text+'%'+LineEnding+FPictureEpilogue;
  if FFigureEpilogue<>'' then Text := Text+'%'+LineEnding+FFigureEpilogue;
  Text := Text+'%'+LineEnding;
  ContentKey := MD5Print(MD5String(FPreamble+#0+LatexPath+#0+Text));
  Rotation := Rotation-Floor(Rotation/(2*Pi))*2*Pi;
  Key := MD5Print(MD5String(ContentKey+#0+FloatToStr(Rotation)));
  I := FEntries.IndexOf(Key);
  if I<0 then begin
    Entry := TPreviewEntry.Create;
    Entry.Key := Key;
    Entry.ContentKey := ContentKey;
    Entry.Rotation := Rotation;
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
  Obj.P := P;
  Obj.H := H;
  Obj.HAlign := HAlign;
  Obj.VAlign := VAlign;
  if Obj.Current<>Entry then begin
    Obj.Current := Entry;
    CancelUnusedWorker;
    if Entry.Pending then FTimer.Enabled := False;
  end;
  if Entry.Pending and not Assigned(FWorker) then FTimer.Enabled := True;
  if Assigned(Entry.Image) then Obj.LastGood := Entry;
  Entry := Obj.LastGood;
  if not Assigned(Entry) or not Assigned(Entry.Image) then begin
    { Until first typesetting, include the source glyphs in the damaged area.
      The canvas font is prepared by the caller before reaching this hook. }
    Radius := Max(1,Ceil(Max(H*2,FallbackRadius)));
    Obj.Bounds := Rect(Floor(P.X-Radius),Floor(P.Y-Radius),
      Ceil(P.X+Radius),Ceil(P.Y+Radius));
    Exit;
  end;
  Obj.Bounds := Bounds(Obj,Entry);
  Canvas.StretchDraw(Obj.Bounds,Entry.Image);
  Result := True;
end;

procedure InitializeLiveTeX(const Changed: TNotifyEvent);
begin
  ShutdownLiveTeX;
  try
    Manager := TTeXPreviewManager.Create(Changed);
  except
    on E: Exception do LiveTeXStatus := 'LaTeX preview: '+E.Message;
  end;
end;

procedure ConfigureLiveTeX(const Drawing: TDrawing2D);
var
  Packages: TStringList;
begin
  if not Assigned(Manager) or not Assigned(Drawing) then Exit;
  if (Manager.FTeXFormat=Ord(Drawing.TeXFormat)) and
    (Manager.FTeXFigure=Ord(Drawing.TeXFigure)) and
    (Manager.FFigurePrologue=Drawing.TeXFigurePrologue) and
    (Manager.FFigureEpilogue=Drawing.TeXFigureEpilogue) and
    (Manager.FPicturePrologue=Drawing.TeXPicPrologue) and
    (Manager.FPictureEpilogue=Drawing.TeXPicEpilogue) then Exit;
  Packages := TStringList.Create;
  try
    AddTeXPreviewPackages(Packages,Drawing,ltxview_Dvi);
    Manager.FPackages := Packages.Text;
    Manager.FTeXFormat := Ord(Drawing.TeXFormat);
    Manager.FTeXFigure := Ord(Drawing.TeXFigure);
    Manager.FFigurePrologue := Drawing.TeXFigurePrologue;
    Manager.FFigureEpilogue := Drawing.TeXFigureEpilogue;
    Manager.FPicturePrologue := Drawing.TeXPicPrologue;
    Manager.FPictureEpilogue := Drawing.TeXPicEpilogue;
    Manager.FPreambleAge := -2;
  finally
    Packages.Free;
  end;
end;

procedure ShutdownLiveTeX;
begin
  FreeAndNil(Manager);
end;

procedure BeginLiveTeXDocument(Trusted: Boolean);
begin
  Inc(LiveTeXDocumentGeneration);
  LiveTeXDocumentTrusted := Trusted;
  if not Trusted then LiveTeXStatus := LiveTeXTrustStatus
  else LiveTeXStatus := '';
  if Assigned(Manager) then Manager.InvalidateDocument;
end;

procedure SetLiveTeXDocumentTrusted(Trusted: Boolean);
begin
  if LiveTeXDocumentTrusted = Trusted then Exit;
  BeginLiveTeXDocument(Trusted);
end;

procedure SetLiveTeXEnabled(Value: Boolean);
begin
  LiveTeXEnabled := Value;
  if not LiveTeXDocumentTrusted then LiveTeXStatus := LiveTeXTrustStatus
  else LiveTeXStatus := '';
  if Assigned(Manager) then Manager.Enable(Value);
end;

procedure FlushLiveTeX;
begin
  if Assigned(Manager) then Manager.Schedule(nil);
end;

procedure ForgetLiveTeXObject(ObjectKey: PtrUInt);
var
  I: Integer;
begin
  if not Assigned(Manager) then Exit;
  I := Manager.FObjects.IndexOf(IntToHex(ObjectKey,SizeOf(ObjectKey)*2));
  if I>=0 then begin
    Manager.FObjects.Delete(I);
    Manager.CancelUnusedWorker;
  end;
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

function TakeLiveTeXDamage(out R: TRect): Boolean;
begin
  Result := Assigned(Manager) and Manager.FHasDamage;
  if Result then begin
    R := Manager.FDamage;
    InflateRect(R,2,2);
    Manager.FHasDamage := False;
  end else R := Rect(0,0,0,0);
end;

function DrawLiveTeX(const Canvas: TCanvas; const ObjectKey: PtrUInt;
  const P: TPoint2D; const H, Rotation: Double; const Source: string;
  const HAlign: THAlignment; const VAlign: TVAlignment;
  const Color: TColor; FallbackRadius: Double): Boolean;
begin
  Result := Assigned(Manager) and LiveTeXEnabled and
    LiveTeXDocumentTrusted and
    Manager.Draw(Canvas,ObjectKey,P,H,Rotation,Source,HAlign,VAlign,Color,FallbackRadius);
end;

finalization
  ShutdownLiveTeX;
{$ENDIF}

end.
