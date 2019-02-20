unit PreView;

interface
uses
  SysUtils, StrUtils, Classes, Forms, Dialogs, Controls,
  Drawings, Graphics;

type
  TLaTeXPreviewKind = (ltxview_Dvi, ltxview_Pdf, ltxview_PS);

function Run_LaTeX_Temp(const Drawing: TDrawing2D;
  const PreviewKind: TLaTeXPreviewKind;
  var TempDvi: string; const Hide: Boolean;
  const DvipsFixBB: Boolean): Boolean;
procedure Preview_LaTeX(const Drawing: TDrawing2D;
  PreviewKind: TLaTeXPreviewKind);
procedure Preview_Picture(const Drawing: TDrawing2D;
  PreviewKind: ExportFormatKind);
procedure GhostScriptConvert(const FileName: string;
  const Keys: string; const OutFileName: string);
function GetPostScriptPreview(const FileName: string): TBitmap;
procedure View_Source(const Drawing: TDrawing2D);
procedure StoreToFile_PreviewSource(
  const Drawing: TDrawing2D; const FileName: string;
  const PreviewKind: TLaTeXPreviewKind);

var
{$IFNDEF LINUX}
  LatexPath: string = 'latex.exe';
  PdfLatexPath: string = 'pdflatex.exe';
  DviPsPath: string = 'dvips.exe';
{$ELSE}
  LatexPath: string = 'latex';
  PdfLatexPath: string = 'pdflatex';
  DviPsPath: string = 'dvips';
{$ENDIF}
  DviViewerPath: string = '';
  PdfViewerPath: string = '';
  TextViewerPath: string = '';
  HtmlViewerPath: string = '';
  PSViewerPath: string = '';
  SvgViewerPath: string = '';
  PngViewerPath: string = '';
  BmpViewerPath: string = '';
{$IFNDEF LINUX}
  GhostscriptPath: string = 'gswin32c.exe';
{$ELSE}
  GhostscriptPath: string = 'gs'; //??
{$ENDIF}
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
  List.Add('%\pdfoutput=0 % uncomment this to run pdfLaTeX in DVI mode');
end;

procedure WriteTempTeXFile(const FileName, TpXName: string;
  const Drawing: TDrawing2D;
  const PreviewKind: TLaTeXPreviewKind;
  const PgfDvipsFixBB: Boolean);
var
  IncludeFile: string;
  List: TStringList;
begin
  IncludeFile := ExtractFilePath(Application.ExeName) +
    'preview.tex.inc';
  List := TStringList.Create;
  //List.Insert();'%TpX%'
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
    List.Add('\usepackage{ifpdf}');
    List.Add('\usepackage{color}');
    if PreviewKind = ltxview_Pdf then
    begin
      List.Add('\usepackage[pdftex]{graphicx}');
      List.Add('\DeclareGraphicsExtensions{.pdf,.png,.jpg,.jpeg,.mps}');
    end
    else
    begin
      List.Add('\usepackage{graphicx}');
      List.Add('\DeclareGraphicsExtensions{.eps,.bmp}');
    end;
    if PreviewKind = ltxview_Pdf then
    begin
      case Drawing.PdfTeXFormat of
        pdftex_tex: List.Add('\usepackage{epic,bez123}');
        pdftex_pgf: List.Add('\usepackage{pgf}');
        pdftex_tikz: List.Add('\usepackage{tikz}');
      end;
    end
    else
    begin
      case Drawing.TeXFormat of
        tex_tex: List.Add('\usepackage{epic,bez123}');
        tex_pgf: List.Add('\usepackage{pgf}');
        tex_tikz: List.Add('\usepackage{tikz}');
        tex_pstricks: List.Add('\usepackage{pstricks}');
        tex_emf:
          List.Add('\DeclareGraphicsRule{.emf}{bmp}{}{}% declare EMF filename extension');
        tex_png:
          List.Add('\DeclareGraphicsRule{.png}{bmp}{}{}% declare PNG filename extension');
      end;
    end;
    case Drawing.TeXFigure of
      fig_floating: List.Add('\usepackage{floatflt}');
      fig_wrap: List.Add('\usepackage{wrapfig}');
    end;
    if PgfDvipsFixBB then List.Add('\pgfrealjobname{dummy}');
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

function WritePreviewSource(const Drawing: TDrawing2D;
  const PreviewKind: TLaTeXPreviewKind;
  const DvipsFixBB: Boolean;
  const TeXFileName, TpXFileName: string): Boolean;
var
  IncludePath0, FileName0: string;
  TeXFormat0: TeXFormatKind;
  PdfTeXFormat0: PdfTeXFormatKind;
begin
  Result := False;
  TryDeleteFile(TpXFileName);
  TryDeleteFile(TeXFileName);
  //StringReplace(TpXName, '\', '/', [rfReplaceAll])
  // Set IncludePath to empty string for preview
  IncludePath0 := Drawing.IncludePath;
  FileName0 := Drawing.FileName;
  Drawing.IncludePath := '';
  TeXFormat0 := Drawing.TeXFormat;
  PdfTeXFormat0 := Drawing.PdfTeXFormat;
  case PreviewKind of
    ltxview_Dvi, ltxview_PS: Drawing.PdfTeXFormat := pdftex_none;
    ltxview_Pdf: Drawing.TeXFormat := tex_none;
  end;
  try
    Drawing.FileName := TpXFileName;
    Result := StoreToFile_TpX(Drawing, TpXFileName, DvipsFixBB);
  except
    Drawing.IncludePath := IncludePath0;
    Drawing.TeXFormat := TeXFormat0;
    Drawing.PdfTeXFormat := PdfTeXFormat0;
    Drawing.FileName := FileName0;
    Exit;
  end;
  if not Result then Exit;
  Result := False;
  Drawing.IncludePath := IncludePath0;
  Drawing.TeXFormat := TeXFormat0;
  Drawing.PdfTeXFormat := PdfTeXFormat0;
  Drawing.FileName := FileName0;
  if not FileExists(TpXFileName) then
  begin
    MessageBoxError('Preview/temporary TpX file not created');
    Exit;
  end;
  WriteTempTeXFile(TeXFileName, ExtractFileName(TpXFileName),
    Drawing, PreviewKind,
    (Drawing.TeXFormat in [tex_pgf, tex_tikz]) and DvipsFixBB);
  if not FileExists(TeXFileName) then
  begin
    MessageBoxError('Preview/temporary TeX file not created');
    TryDeleteFile(TpXFileName);
    Exit;
  end;
  Result := True;
end;

function Run_LaTeX_Temp(const Drawing: TDrawing2D;
  const PreviewKind: TLaTeXPreviewKind; var TempDvi: string;
  const Hide: Boolean; const DvipsFixBB: Boolean): Boolean;
var
  TempDir, TempTpX, TempTeX, TempTeXLog: string;
  LatexCompPath: string;
const
  TempTpX0 = '(tpx)TpX.TpX';
begin
  Result := False;
  TempDir := GetTempDir;
  TempTpX := TempDir + TempTpX0;
  TempTeX := TempDir + '(doc)TpX.tex';
  if not WritePreviewSource(
    Drawing, PreviewKind, DvipsFixBB, TempTeX, TempTpX) then Exit;
  case PreviewKind of
    ltxview_Dvi, ltxview_PS: LatexCompPath := LatexPath;
    ltxview_Pdf: LatexCompPath := PdfLatexPath;
  end;
  if not CheckFilePath(LatexCompPath, 'LaTeX/PdfLaTeX') then Exit;
  case PreviewKind of
    ltxview_Dvi, ltxview_PS: TempDvi := TempDir + '(doc)TpX.dvi';
    ltxview_Pdf: TempDvi := TempDir + '(doc)TpX.pdf';
  end;
  TryDeleteFile(TempDvi);
  TempTeXLog := ChangeFileExt(TempTeX, '.log');
  TryDeleteFile(TempTeXLog);
  try
    Result := FileExec(Format('%s -interaction=batchmode "%s"',
      [PrepareFilePath(LatexCompPath), TempTeX]), '', '',
      {IncludeTrailingPathDelimiter(ExtractFilePath(FileName))} TempDir,
      Hide, True);
    if not Result or not FileExists(TempDvi) then
    begin
      Result := False;
      if not FileExists(TempTeXLog) then
        MessageBoxError('DVI (PDF) file not created')
      else if MessageBoxErrorAsk(
        'DVI (PDF) file not created. Do you want to see log file?')
        then
      begin
        OpenOrExec(TextViewerPath, TempTeXLog);
      end;
      Exit;
    end;
  finally
    TryDeleteFile(TempTpX);
    TryDeleteFile(TempTeX);
    //TryDeleteFile(TempDvi);
  end;
  Result := True;
end;

procedure Preview_LaTeX(const Drawing: TDrawing2D; PreviewKind:
  TLaTeXPreviewKind);
var
  TempDvi, ViewerPath: string;
begin
  if not Run_LaTeX_Temp(Drawing, PreviewKind, TempDvi, False, False)
    then Exit;
  if PreviewKind = ltxview_PS then
  begin
    if not CheckFilePath(DviPsPath, 'DviPs') then Exit;
    TryDeleteFile(ChangeFileExt(TempDvi, '.ps'));
    FileExec(Format('%s "%s"', 
      [PrepareFilePath(DviPsPath), ExtractFileName(TempDvi)]), '',
      '',
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
  if ViewerPath <> '' then
    if not CheckFilePath(ViewerPath, 'Viewer') then Exit;
  OpenOrExec(ViewerPath, TempDvi);
end;

procedure Preview_Picture(const Drawing: TDrawing2D;
  PreviewKind: ExportFormatKind);
var
  TmpFileName, Ext, ViewerPath: string;
begin
  TmpFileName := GetTempDir + '(img)TpX';
  Ext := CSV_Item(ExportDefaultExt, Ord(PreviewKind) + 1);
  if Ext = '' then Exit;
  TmpFileName := TmpFileName + '.' + Ext;
  TryDeleteFile(TmpFileName);
  case PreviewKind of
    export_SVG:
      StoreToFile_Saver(Drawing, TmpFileName, T_SVG_Export);
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
  if PreviewKind in [export_EPS] then
    ViewerPath := PSViewerPath
  else if PreviewKind in [export_PDF] then
    ViewerPath := PdfViewerPath
  else if PreviewKind in [export_SVG] then
    ViewerPath := SvgViewerPath
  else if PreviewKind in [export_PNG] then
    ViewerPath := PngViewerPath
  else if PreviewKind in [export_BMP] then
    ViewerPath := BmpViewerPath
  else ViewerPath := '';
  if ViewerPath <> '' then
    if not CheckFilePath(ViewerPath, 'Viewer') then Exit;
  OpenOrExec(ViewerPath, TmpFileName);
end;

procedure GhostScriptConvert(const FileName: string;
  const Keys: string; const OutFileName: string);
begin
  if not CheckFilePath(GhostscriptPath, 'Ghostscript') then Exit;
  if not FileExists(FileName) then Exit;
  FileExec(Format(
    '%s -dSAFER -dBATCH -dNOPAUSE -dEPSCrop -q %s -sOutputFile="%s" "%s"',
    [PrepareFilePath(GhostscriptPath),
    Keys, OutFileName, FileName]), '', '',
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
  TempDir, TempTpX, IncludePath0: string;
  DlgResult: Word;
  ShowCurrent: Boolean;
const
  TempTpX0 = '(tpx)TpX.TpX';
begin
  ShowCurrent := Drawing.FileName = Drawing_NewFileName;
  if (not ShowCurrent) and Drawing.History.IsChanged then
  begin
    DlgResult := MessageDlg(
      'Drawing has changed. Do you want to see the current variant?',
      mtWarning, [mbYes, mbNo, mbCancel], 0);
    if DlgResult = mrCancel then Exit;
    ShowCurrent := DlgResult = mrYes;
  end;
  if not ShowCurrent then
  begin
    if not CheckFilePath(TextViewerPath, 'TextViewerPath') then
      Exit;
    OpenOrExec(TextViewerPath, Drawing.FileName);
    Exit;
  end;
  TempDir := GetTempDir;
  TempTpX := TempDir + TempTpX0;
  TryDeleteFile(TempTpX);
  // Set IncludePath to empty string for preview
  IncludePath0 := Drawing.IncludePath;
  Drawing.IncludePath := '';
  try
    StoreToFile_TpX(Drawing, TempTpX, False);
  except
    Drawing.IncludePath := IncludePath0;
    Exit;
  end;
  Drawing.IncludePath := IncludePath0;
  if not FileExists(TempTpX) then
  begin
    MessageBoxError('Temporary TpX file not created');
    Exit;
  end;
  if not CheckFilePath(TextViewerPath, 'TextViewerPath') then
    Exit;
  OpenOrExec(TextViewerPath, TempTpX);
end;

procedure StoreToFile_PreviewSource(
  const Drawing: TDrawing2D; const FileName: string;
  const PreviewKind: TLaTeXPreviewKind);
var
  TpXFileName: string;
begin
  TpXFileName := ExtractFilePath(FileName)
    + ChangeFileExt(ExtractFileName(FileName), '') + '(TpX).TpX';
  WritePreviewSource(
    Drawing, PreviewKind, False, FileName, TpXFileName);
end;

procedure WritePreambleForHelp;
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    WritePreamble(List);
//    if OutHelp then // Save preamble for inclusion into help
//      List.SaveToFile(ExtractFilePath(ParamStr(0))
//        + '\Help\preamble.inc');
  finally
    List.Free;
  end;
end;


initialization
  WritePreambleForHelp;
end.

