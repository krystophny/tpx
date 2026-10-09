unit TeXPreviewWorker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TTeXPreviewInput = record
    Key, Source, Preamble, LatexCommand: string;
    RasterDPI: Integer;
  end;
  TTeXPreviewInputs = array of TTeXPreviewInput;

  TTeXPreviewOutput = record
    Key, PNGFile, SVGFile, Error: string;
    WidthPt, HeightPt, DepthPt: Double;
    RasterDPI: Integer;
  end;
  TTeXPreviewOutputs = array of TTeXPreviewOutput;

  { Owns no UI objects. Start explicitly; read Results only after completion.
    The caller owns the cache directory and removes it after joining workers. }
  TTeXPreviewWorker = class(TThread)
  private
    FInputs: TTeXPreviewInputs;
    FDirectory: string;
    FResults: TTeXPreviewOutputs;
    FRuns: Integer;
    function RunTool(const Executable, WorkDirectory: string;
      const Arguments: array of string; out Log: string): Boolean;
    function CacheBase(Index: Integer): string;
    function LoadCached(Index: Integer): Boolean;
    procedure Rasterize(Index: Integer; const DVIFile: string; Page: Integer);
    procedure CompileBatch(const Indices: array of Integer);
  protected
    procedure Execute; override;
  public
    constructor Create(const Inputs: TTeXPreviewInputs;
      const CacheDirectory: string; Callback: TNotifyEvent);
    property Results: TTeXPreviewOutputs read FResults;
    property Directory: string read FDirectory;
    property Runs: Integer read FRuns;
  end;

implementation

uses
  Process, MD5;

const
  ToolTimeoutMS = 15000;
  MaxLogLength = 65536;

function ToolName(const Name, LatexCommand: string): string;
var
  Candidate, Executable: string;
begin
  Executable := Name;
  {$IFDEF MSWINDOWS}
  Executable := Executable + '.exe';
  {$ENDIF}
  Candidate := ExtractFilePath(LatexCommand) + Executable;
  if (ExtractFilePath(LatexCommand) <> '') and FileExists(Candidate) then
    Exit(Candidate);
  Result := Executable;
end;

function ErrorSummary(const Log: string): string;
var
  Lines: TStringList;
  I: Integer;
begin
  Result := Trim(Log);
  Lines := TStringList.Create;
  try
    Lines.Text := Log;
    for I := 0 to Lines.Count - 1 do
      if ((Length(Lines[I]) > 0) and (Lines[I][1] = '!')) or
         (Pos('preview.tex:', Lines[I]) > 0) then
      begin
        Result := Trim(Lines[I]);
        Break;
      end;
  finally
    Lines.Free;
  end;
  if Length(Result) > 240 then Result := Copy(Result, 1, 240) + '...';
  if Result = '' then Result := 'LaTeX preview did not produce output';
end;

function PointValue(const Text: string): Double;
var
  Format: TFormatSettings;
  Value: string;
begin
  Format := DefaultFormatSettings;
  Format.DecimalSeparator := '.';
  Value := Trim(Text);
  if Copy(Value, Length(Value) - 1, 2) = 'pt' then
    Delete(Value, Length(Value) - 1, 2);
  if not TryStrToFloat(Value, Result, Format) then
    raise Exception.Create('Invalid LaTeX preview dimensions');
end;

constructor TTeXPreviewWorker.Create(const Inputs: TTeXPreviewInputs;
  const CacheDirectory: string; Callback: TNotifyEvent);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FInputs := Copy(Inputs);
  FDirectory := IncludeTrailingPathDelimiter(ExpandFileName(CacheDirectory));
  SetLength(FResults, Length(Inputs));
  OnTerminate := Callback;
end;

function TTeXPreviewWorker.RunTool(const Executable, WorkDirectory: string;
  const Arguments: array of string; out Log: string): Boolean;
var
  Child: TProcess;
  I, Count: Integer;
  Buffer: array[0..4095] of Byte;
  Chunk: string;
  Started: QWord;
  TimedOut: Boolean;
  procedure DrainOutput;
  begin
    while Child.Output.NumBytesAvailable > 0 do
    begin
      Count := Child.Output.Read(Buffer, SizeOf(Buffer));
      if Count <= 0 then Break;
      SetString(Chunk, PChar(@Buffer[0]), Count);
      Log := Log + Chunk;
      if Length(Log) > MaxLogLength then
        Delete(Log, 1, Length(Log) - MaxLogLength);
    end;
  end;
begin
  Result := False;
  Log := '';
  if Terminated then Exit;
  Child := TProcess.Create(nil);
  try
    try
      Child.Executable := Executable;
      Child.CurrentDirectory := WorkDirectory;
      Child.Options := [poUsePipes, poStderrToOutPut, poNoConsole];
      for I := Low(Arguments) to High(Arguments) do
        Child.Parameters.Add(Arguments[I]);
      Child.Execute;
      Child.CloseInput;
      Started := GetTickCount64;
      TimedOut := False;
      while Child.Running do
      begin
        DrainOutput;
        TimedOut := GetTickCount64 - Started >= ToolTimeoutMS;
        if Terminated or TimedOut then
        begin
          Child.Terminate(1);
          Child.WaitOnExit(1000);
          Break;
        end;
        Sleep(10);
      end;
      DrainOutput;
      if Terminated then Log := 'Preview cancelled'
      else if TimedOut then Log := ExtractFileName(Executable) + ' timed out'
      else Result := Child.ExitStatus = 0;
    except
      on E: Exception do Log := E.Message;
    end;
  finally
    Child.Free;
  end;
end;

function TTeXPreviewWorker.CacheBase(Index: Integer): string;
begin
  { The caller's content key deliberately excludes display DPI. }
  Result := FDirectory + MD5Print(MD5String(FInputs[Index].Key));
end;

procedure TTeXPreviewWorker.Rasterize(Index: Integer;
  const DVIFile: string; Page: Integer);
var
  Log, Base, SVGPath, PNGPath: string;
  DPI: Integer;
begin
  if Terminated then Exit;
  Base := CacheBase(Index);
  SVGPath := Base + '.svg';
  DPI := FInputs[Index].RasterDPI;
  if DPI < 96 then DPI := 96;
  if DPI > 2400 then DPI := 2400;
  FResults[Index].RasterDPI := DPI;
  PNGPath := Base + '-' + IntToStr(DPI) + '.png';
  if not FileExists(SVGPath) then
    RunTool(ToolName('dvisvgm', FInputs[Index].LatexCommand),
      ExtractFilePath(DVIFile), ['--no-fonts', '--exact',
      '--page=' + IntToStr(Page), '--output=' + SVGPath, DVIFile], Log);
  if FileExists(SVGPath) then FResults[Index].SVGFile := SVGPath;
  { DVI remains a reusable vector source when no SVG rasterizer is installed.
    Zooming only repeats this cheap conversion; it never repeats LaTeX. }
  if not FileExists(PNGPath) then
    if not RunTool(ToolName('dvipng', FInputs[Index].LatexCommand),
      ExtractFilePath(DVIFile), ['-q', '-T', 'tight', '-bg', 'Transparent',
      '-D', IntToStr(DPI), '-p', IntToStr(Page), '-l', IntToStr(Page),
      '-o', PNGPath, DVIFile], Log) then
    begin
      DeleteFile(PNGPath);
      FResults[Index].Error := 'dvipng: ' + ErrorSummary(Log);
      Exit;
    end;
  if FileExists(PNGPath) then FResults[Index].PNGFile := PNGPath
  else FResults[Index].Error := 'dvipng did not produce a preview image';
end;

function TTeXPreviewWorker.LoadCached(Index: Integer): Boolean;
var
  Data: TStringList;
  FileName, DVIFile: string;
  Page: Integer;
begin
  Result := False;
  FileName := CacheBase(Index) + '.cache';
  if not FileExists(FileName) then Exit;
  Data := TStringList.Create;
  try
    try
      Data.LoadFromFile(FileName);
      if Data.Count <> 5 then Exit;
      DVIFile := FDirectory + Data[0];
      if not FileExists(DVIFile) then Exit;
      if not TryStrToInt(Data[1], Page) or (Page < 1) then Exit;
      FResults[Index].WidthPt := PointValue(Data[2]);
      FResults[Index].HeightPt := PointValue(Data[3]);
      FResults[Index].DepthPt := PointValue(Data[4]);
      Rasterize(Index, DVIFile, Page);
      Result := True;
    except
      { An incomplete cache entry is a miss. }
    end;
  finally
    Data.Free;
  end;
end;

procedure TTeXPreviewWorker.CompileBatch(const Indices: array of Integer);
var
  Source, Metrics, Values, Cache: TStringList;
  ID: TGuid;
  BatchName, BatchDirectory, DVIFile, Log: string;
  I, J, Index, Line, Page, DPI: Integer;
  DPIs: array of Integer;
  AlreadyRendered: Boolean;
  RenderedFile: string;
begin
  if (Length(Indices) = 0) or Terminated then Exit;
  CreateGUID(ID);
  BatchName := 'job-' + GUIDToString(ID);
  BatchDirectory := FDirectory + BatchName + PathDelim;
  ForceDirectories(BatchDirectory);
  Source := TStringList.Create;
  Metrics := TStringList.Create;
  Values := TStringList.Create;
  Cache := TStringList.Create;
  try
    Source.Text := FInputs[Indices[0]].Preamble;
    if Pos('\documentclass', Source.Text) = 0 then
      Source.Insert(0, '\documentclass[10pt]{article}');
    Source.Add('\usepackage{color}');
    Source.Add('\usepackage[active,tightpage]{preview}');
    Source.Add('\setlength{\PreviewBorder}{0pt}');
    Source.Add('\newbox\tpxpreviewbox');
    Source.Add('\newwrite\tpxpreviewmetrics');
    Source.Add('\begin{document}');
    Source.Add('\immediate\openout\tpxpreviewmetrics=metrics.txt');
    for I := 0 to High(Indices) do
    begin
      Source.Add('\setbox\tpxpreviewbox=\hbox{' + FInputs[Indices[I]].Source + '}');
      Source.Add('\immediate\write\tpxpreviewmetrics{' + IntToStr(I + 1) +
        '|\the\wd\tpxpreviewbox|\the\ht\tpxpreviewbox|\the\dp\tpxpreviewbox}');
      Source.Add('\begin{preview}\copy\tpxpreviewbox\end{preview}');
    end;
    Source.Add('\immediate\closeout\tpxpreviewmetrics');
    Source.Add('\end{document}');
    Source.SaveToFile(BatchDirectory + 'preview.tex');
    Inc(FRuns);
    if not RunTool(FInputs[Indices[0]].LatexCommand, BatchDirectory,
      ['-no-shell-escape', '-interaction=nonstopmode', '-halt-on-error',
       '-file-line-error', 'preview.tex'], Log) then
    begin
      for I := 0 to High(Indices) do
        FResults[Indices[I]].Error := 'LaTeX: ' + ErrorSummary(Log);
      Exit;
    end;
    DVIFile := BatchDirectory + 'preview.dvi';
    if not FileExists(DVIFile) or
       not FileExists(BatchDirectory + 'metrics.txt') then
    begin
      for I := 0 to High(Indices) do
        FResults[Indices[I]].Error := 'LaTeX did not produce DVI preview output';
      Exit;
    end;
    { Render all pages in each tool invocation: a drawing with many labels
      must not launch a new converter process for every object. }
    if RunTool(ToolName('dvisvgm', FInputs[Indices[0]].LatexCommand),
      BatchDirectory, ['--no-fonts', '--exact', '--page=1-',
      '--output=preview-%p.svg', DVIFile], Log) then
      for I := 0 to High(Indices) do
      begin
        RenderedFile := BatchDirectory + 'preview-' + IntToStr(I + 1) + '.svg';
        if FileExists(RenderedFile) then
          RenameFile(RenderedFile, CacheBase(Indices[I]) + '.svg');
      end;
    SetLength(DPIs, 0);
    for I := 0 to High(Indices) do
    begin
      if Terminated then Exit;
      DPI := FInputs[Indices[I]].RasterDPI;
      if DPI < 96 then DPI := 96;
      if DPI > 2400 then DPI := 2400;
      AlreadyRendered := False;
      for J := 0 to High(DPIs) do
        if DPIs[J] = DPI then AlreadyRendered := True;
      if AlreadyRendered then Continue;
      SetLength(DPIs, Length(DPIs) + 1);
      DPIs[High(DPIs)] := DPI;
      if RunTool(ToolName('dvipng', FInputs[Indices[0]].LatexCommand),
        BatchDirectory, ['-q', '-T', 'tight', '-bg', 'Transparent',
        '-D', IntToStr(DPI), '-o', 'preview-%d-' + IntToStr(DPI) + '.png',
        DVIFile], Log) then
        for J := 0 to High(Indices) do
        begin
          RenderedFile := BatchDirectory + 'preview-' + IntToStr(J + 1) +
            '-' + IntToStr(DPI) + '.png';
          if FileExists(RenderedFile) then
            RenameFile(RenderedFile, CacheBase(Indices[J]) +
              '-' + IntToStr(DPI) + '.png');
        end;
    end;
    Metrics.LoadFromFile(BatchDirectory + 'metrics.txt');
    Values.StrictDelimiter := True;
    Values.Delimiter := '|';
    for I := 0 to High(Indices) do
    begin
      if Terminated then Exit;
      Index := Indices[I];
      FResults[Index].Error := 'LaTeX did not return preview dimensions';
      for Line := 0 to Metrics.Count - 1 do
      begin
        Values.DelimitedText := Metrics[Line];
        if (Values.Count <> 4) or
           not TryStrToInt(Values[0], Page) or (Page <> I + 1) then Continue;
        FResults[Index].WidthPt := PointValue(Values[1]);
        FResults[Index].HeightPt := PointValue(Values[2]);
        FResults[Index].DepthPt := PointValue(Values[3]);
        FResults[Index].Error := '';
        Cache.Clear;
        Cache.Add(BatchName + PathDelim + 'preview.dvi');
        Cache.Add(IntToStr(Page));
        Cache.Add(Values[1]);
        Cache.Add(Values[2]);
        Cache.Add(Values[3]);
        Cache.SaveToFile(CacheBase(Index) + '.cache');
        Rasterize(Index, DVIFile, Page);
        Break;
      end;
    end;
  finally
    Cache.Free;
    Values.Free;
    Metrics.Free;
    Source.Free;
  end;
end;

procedure TTeXPreviewWorker.Execute;
var
  Handled: array of Boolean;
  Batch: array of Integer;
  I, J, K, Count: Integer;
  Duplicate: Boolean;
begin
  try
    if not ForceDirectories(FDirectory) then
      raise Exception.Create('Cannot create LaTeX preview cache directory');
    SetLength(Handled, Length(FInputs));
    for I := 0 to High(FInputs) do
    begin
      FResults[I].Key := FInputs[I].Key;
      FResults[I].RasterDPI := FInputs[I].RasterDPI;
    end;
    for I := 0 to High(FInputs) do
    begin
      if Terminated then Exit;
      if Handled[I] then Continue;
      Handled[I] := True;
      if LoadCached(I) then Continue;
      SetLength(Batch, Length(FInputs) - I);
      Batch[0] := I;
      Count := 1;
      for J := I + 1 to High(FInputs) do
        if not Handled[J] and
           (FInputs[J].Preamble = FInputs[I].Preamble) and
           (FInputs[J].LatexCommand = FInputs[I].LatexCommand) then
        begin
          Duplicate := False;
          for K := 0 to Count - 1 do
            if FInputs[Batch[K]].Key = FInputs[J].Key then Duplicate := True;
          if Duplicate then Continue;
          Handled[J] := True;
          if LoadCached(J) then Continue;
          Batch[Count] := J;
          Inc(Count);
        end;
      SetLength(Batch, Count);
      CompileBatch(Batch);
    end;
  except
    on E: Exception do
      for I := 0 to High(FResults) do
        if FResults[I].PNGFile = '' then
          FResults[I].Error := ErrorSummary(E.Message);
  end;
end;

end.
