unit PreView;

{$IFNDEF VER140}
{$MODE Delphi}
{$ENDIF}

interface
uses
  SysUtils, StrUtils, Forms, Classes, Drawings, Graphics;

type
  TLaTeXPreviewKind = (ltxview_Dvi, ltxview_Pdf, ltxview_PS);

procedure Run_LaTeX_Temp(const Drawing: TDrawing2D; PreviewKind:
  TLaTeXPreviewKind; var TempDvi: string; const Hide: Boolean;
  const DvipsFixBB: Boolean);
procedure Preview_LaTeX(const Drawing: TDrawing2D;
  PreviewKind: TLaTeXPreviewKind);
procedure Preview_Picture(const Drawing: TDrawing2D;
  PreviewKind: ExportFormatKind);
procedure GhostScriptConvert(const FileName: string;
  const Keys: string; const OutFileName: string);
function GetPostScriptPreview(const FileName: string): TBitmap;
procedure View_Source(const Drawing: TDrawing2D);

var
  LatexPath: string = 'latex.exe';
  PdfLatexPath: string = 'pdflatex.exe';
  DviPsPath: string = 'dvips.exe';
  //Ps2PdfPath: string = 'dvips.exe';
  DviViewerPath: string = '';
  PdfViewerPath: string = '';
  TextViewerPath: string = '';
  HtmlViewerPath: string = '';
  PSViewerPath: string = '';
  SvgViewerPath: string = '';
  PngViewerPath: string = '';
  BmpViewerPath: string = '';
  GhostscriptPath: string = 'gswin32c.exe';
    // or mgs.exe from MikTeX

implementation

uses Output,
{$IFDEF VER140}
  ShellAPI,
{$ELSE}
{$ENDIF}
  SysBasic, MiscUtils;

procedure WritePreamble(const List: TStringList);
begin
  List.Add('\documentclass[a4paper,10pt]{article}');
  List.Add('% comment unused packages below');
  List.Add('\usepackage{color}');
  List.Add('%\pdfoutput=0 % uncomment this to run pdfLaTeX in DVI mode');
  List.Add('\usepackage{ifpdf}');
  List.Add('\ifpdf %if using pdfLaTeX in PDF mode');
  List.Add('  \usepackage[pdftex]{graphicx}');
  List.Add('  \DeclareGraphicsExtensions{.pdf,.png,.mps}');
  List.Add('  \usepackage{pgf}');
  List.Add('  \usepackage{tikz}');
  List.Add('\else %if using LaTeX or pdfLaTeX in DVI mode');
  List.Add('  \usepackage{graphicx}');
  List.Add('  \DeclareGraphicsExtensions{.eps,.bmp}');
  List.Add('  \DeclareGraphicsRule{.emf}{bmp}{}{}% declare EMF filename extension');
  List.Add('  \DeclareGraphicsRule{.png}{bmp}{}{}% declare PNG filename extension');
  List.Add('  \usepackage{pgf}');
  List.Add('  \usepackage{tikz}');
  List.Add('  \usepackage{pstricks}%variant: \usepackage{pst-all}');
  List.Add('\fi');
  List.Add('\usepackage{epic,bez123}');
  List.Add('\usepackage{floatflt}% package for floatingfigure environment');
  List.Add('\usepackage{wrapfig}% package for wrapfigure environment');
end;

procedure WriteTempTeXFile(const FileName, TpXName: string;
  const PreviewKind: TLaTeXPreviewKind);
var
  IncludeFile: string;
  List: TStringList;
begin
  IncludeFile := ExtractFilePath(Application.ExeName) +
    'preview.tex.inc';
  List := TStringList.Create;
  try
    if FileExists(IncludeFile) then List.LoadFromFile(IncludeFile)
    else
    begin
      WritePreamble(List);
      List.SaveToFile(IncludeFile);
    end;
    if PreviewKind = ltxview_Pdf then
    List.Text := AnsiReplaceStr(
        List.Text, '\pdfoutput=0', '\pdfoutput=1');
    List.Add('\begin{document}');
    //List.Add('\hrule height 1ex');
    List.Add('\thispagestyle{empty}');
    List.Add('\ ');
      //Without this preview does not work for pgf inside figure
    List.Add('');
    List.Add('\input{' + TpXName + '}%');
    List.Add('');
    List.Add('\ ');
    List.Add('\end{document}');
    List.SaveToFile(FileName);
  finally
    List.Free;
  end;
end;

procedure Run_LaTeX_Temp(const Drawing: TDrawing2D; PreviewKind:
  TLaTeXPreviewKind; var TempDvi: string; const Hide: Boolean;
  const DvipsFixBB: Boolean);
var
  TempDir, TempIncludePath, TempTpX, TempTeX, TempTeXLog: string;
  Res: Boolean;
  LatexCompPath: string;
  TeXFormat0: TeXFormatKind;
  PdfTeXFormat0: PdfTeXFormatKind;
const
  TempTpX0 = '(tpx)TpX.TpX';
begin
  TempDir := GetTempDir;
  TempTpX := TempDir + TempTpX0;
  TempTeX := TempDir + '(doc)TpX.tex';
  TempTeXLog := ChangeFileExt(TempTeX, '.log');
  TryDeleteFile(TempTpX);
  TryDeleteFile(TempTeX);
  //StringReplace(TpXName, '\', '/', [rfReplaceAll])
  case PreviewKind of
    ltxview_Dvi, ltxview_PS: TempDvi := TempDir + '(doc)TpX.dvi';
    ltxview_Pdf: TempDvi := TempDir + '(doc)TpX.pdf';
  end;
  TryDeleteFile(TempDvi);
  // Set IncludePath to empty string for preview
  TempIncludePath := Drawing.IncludePath;
  Drawing.IncludePath := '';
  TeXFormat0 := Drawing.TeXFormat;
  PdfTeXFormat0 := Drawing.PdfTeXFormat;
  case PreviewKind of
    ltxview_Dvi, ltxview_PS: Drawing.PdfTeXFormat := pdftex_none;
    ltxview_Pdf: Drawing.TeXFormat := tex_none;
  end;
  try
    StoreToFile_TpX(Drawing, TempTpX, DvipsFixBB);
  except
    Drawing.IncludePath := TempIncludePath;
    Drawing.TeXFormat := TeXFormat0;
    Drawing.PdfTeXFormat := PdfTeXFormat0;
    Exit;
  end;
  Drawing.IncludePath := TempIncludePath;
  Drawing.TeXFormat := TeXFormat0;
  Drawing.PdfTeXFormat := PdfTeXFormat0;
  if not FileExists(TempTpX) then
  begin
    MessageBoxError('Temporary TpX file not created');
    Exit;
  end;
  WriteTempTeXFile(TempTeX, TempTpX0, PreviewKind);
  if not FileExists(TempTeX) then
  begin
    MessageBoxError('Temporary TeX file not created');
    TryDeleteFile(TempTpX);
    Exit;
  end;
  case PreviewKind of
    ltxview_Dvi, ltxview_PS: LatexCompPath := LatexPath;
    ltxview_Pdf: LatexCompPath := PdfLatexPath;
  end;

  {if not FileExists(ExpandFileName(LatexCompPath)) then
  begin
    MessageBoxError('LaTeX (PDFLaTeX) path not found'+ExpandFileName(LatexCompPath);
    Exit;
  end;}
  try
    TryDeleteFile(TempTeXLog);
    Res := FileExec(Format('%s "%s" -quiet',
      [LatexCompPath, TempTeX]), '', '',
      {IncludeTrailingPathDelimiter(ExtractFilePath(FileName))} TempDir,
      Hide, True);
    if not FileExists(TempDvi) then
    begin
      if not FileExists(TempTeXLog) then
        MessageBoxError('DVI (PDF) file not created')
      else if MessageBoxErrorAsk(
        'DVI (PDF) file not created. Do you want to see log file?')
        then
      begin
        OpenOrExec(TextViewerPath, TempTeXLog);
        Res := False;
      end;
      Exit;
    end;
  finally
    TryDeleteFile(TempTpX);
    TryDeleteFile(TempTeX);
    //TryDeleteFile(TempDvi);
  end;
end;

procedure Preview_LaTeX(const Drawing: TDrawing2D; PreviewKind:
  TLaTeXPreviewKind);
var
  TempDvi, ViewerPath: string;
begin
  Run_LaTeX_Temp(Drawing, PreviewKind, TempDvi, False, False);
  if PreviewKind = ltxview_PS then
  begin
    TryDeleteFile(ChangeFileExt(TempDvi, '.ps'));
    FileExec(Format('%s "%s"', // -E
      [DviPsPath, ExtractFileName(TempDvi)]), '', '',
      {IncludeTrailingPathDelimiter(ExtractFilePath(FileName))} GetTempDir,
      True, True);
    TempDvi := ChangeFileExt(TempDvi, '.ps');
    if not FileExists(TempDvi) then
    begin
        //if not FileExists(TempTeXLog) then
      MessageBoxError('PS file not created');
        {else if MessageBoxError(
          'PS file not created. Do you want to see log file?',
          'Error', MB_OKCANCEL) = idOK then
        begin
          OpenOrExec(TextViewerPath, TempTeXLog);
          Res := False;
        end;}
      Exit;
    end;
  end;
  case PreviewKind of
    ltxview_Dvi: ViewerPath := DviViewerPath;
    ltxview_PS: ViewerPath := PSViewerPath;
    ltxview_Pdf: ViewerPath := PdfViewerPath;
  end;
  OpenOrExec(ViewerPath, TempDvi);
end;

procedure Preview_Picture(const Drawing: TDrawing2D;
  PreviewKind: ExportFormatKind);
var
  TmpFileName, Ext: string;
begin
  TmpFileName := GetTempDir + '(img)TpX';
  Ext := CSV_Item(ExportDefaultExt, Ord(PreviewKind) + 1);
  if Ext = '' then Exit;
  TmpFileName := TmpFileName + '.' + Ext;
  TryDeleteFile(TmpFileName);
  case PreviewKind of
    export_SVG:
      StoreToFile_Saver(Drawing,
        TmpFileName, T_SVG_Export);
    export_EMF:
      StoreToFile_EMF(Drawing, TmpFileName);
    export_EPS:
      StoreToFile_Saver(Drawing,
        TmpFileName, T_PostScript_Export);
{$IFDEF VER140}
    export_PNG:
      StoreToFile_Saver(Drawing,
        TmpFileName, T_PNG_Export);
    export_BMP:
      StoreToFile_Saver(Drawing,
        TmpFileName, T_BMP_Export);
{$ENDIF}
    export_PDF:
      StoreToFile_Saver(Drawing,
        TmpFileName, T_PDF_Export);
    {export_MP:
      StoreToFile_Saver(Drawing,
        TmpFileName, T_MetaPost_Export);
    export_MPS:
      StoreToFile_MPS(Drawing, TmpFileName);
    export_EpsToPdf:
      StoreToFile_EpsToPdf(Drawing, TmpFileName, False);}
  else
    //DoSaveDrawing(TmpFileName);
  end;
  if not FileExists(TmpFileName) then
  begin
    MessageBoxError(Format('Output file % not created', [Ext]));
    Exit;
  end;
  if PreviewKind in [export_EPS]
    then OpenOrExec(PSViewerPath, TmpFileName)
  else if PreviewKind in [export_PDF]
    then OpenOrExec(PdfViewerPath, TmpFileName)
  else if PreviewKind in [export_SVG]
    then OpenOrExec(SvgViewerPath, TmpFileName)
  else if PreviewKind in [export_PNG]
    then OpenOrExec(PngViewerPath, TmpFileName)
  else if PreviewKind in [export_BMP]
    then OpenOrExec(BmpViewerPath, TmpFileName)
  else OpenOrExec('', TmpFileName);
end;

procedure GhostScriptConvert(const FileName: string;
  const Keys: string; const OutFileName: string);
begin
  if not FileExists(GhostscriptPath) then Exit;
  if not FileExists(FileName) then Exit;
  FileExec(Format(
    '%s -dSAFER -dBATCH -dNOPAUSE -dEPSCrop -q %s -sOutputFile="%s" "%s"',
    [GhostscriptPath, Keys, OutFileName, FileName]), '', '',
    GetTempDir, True, True);
end;

function GetPostScriptPreview(const FileName: string): TBitmap;
var
  TmpBmp: string;
begin
  Result := nil;
  TmpBmp := GetTempDir + '(img)bmp.bmp';
  GhostScriptConvert(FileName, '-r72 -sDEVICE=bmp256', TmpBmp);
  if not FileExists(TmpBmp) then Exit;
  Result := TBitmap.Create;
  Result.LoadFromFile(TmpBmp);
  TryDeleteFile(TmpBmp);
end;

procedure View_Source(const Drawing: TDrawing2D);
var
  TempDir, TempTpX, TempIncludePath: string;
const
  TempTpX0 = '(tpx)TpX.TpX';
begin
  if Drawing.FileName <> Drawing_NewFileName then
    OpenOrExec(TextViewerPath, Drawing.FileName)
  else
  begin
    TempDir := GetTempDir;
    TempTpX := TempDir + TempTpX0;
    TryDeleteFile(TempTpX);
  // Set IncludePath to empty string for preview
    TempIncludePath := Drawing.IncludePath;
    Drawing.IncludePath := '';
    try
      StoreToFile_TpX(Drawing, TempTpX, False);
    except
      Drawing.IncludePath := TempIncludePath;
      Exit;
    end;
    Drawing.IncludePath := TempIncludePath;
    if not FileExists(TempTpX) then
    begin
      MessageBoxError('Temporary TpX file not created');
      Exit;
    end;
    OpenOrExec(TextViewerPath, TempTpX);
  end;
end;

procedure WritePreambleForHelp;
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    WritePreamble(List);
    if OutHelp then // Save preamble for inclusion into help
      List.SaveToFile(ExtractFilePath(ParamStr(0))
        + '\Help\preamble.inc');
  finally
    List.Free;
  end;
end;


initialization
  WritePreambleForHelp;
end.

