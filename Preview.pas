unit PreView;

interface
uses
  SysUtils, Forms, Windows, Classes, CADSys4;

type
  TLaTeXPreviewKind = (ltxview_Dvi, ltxview_PDF, ltxview_PS);
  TPicturePreviewKind =
    (pview_EPS, pview_PDF, pview_EMF, pview_BMP, pview_PNG, pview_SVG);

procedure Preview_LaTeX(const Drawing: TDrawing2D;
  PreviewKind: TLaTeXPreviewKind);
procedure Preview_Picture(const Drawing: TDrawing2D;
  PreviewKind: TPicturePreviewKind);

var
  LatexPath: string = 'latex.exe';
  PdfLatexPath: string = 'pdflatex.exe';
  DviPsPath: string = 'dvips.exe';
  //Ps2PdfPath: string = 'dvips.exe';
  DviViewerPath: string = '';
  PdfViewerPath: string = '';
  TextViewerPath: string = '';
  PSViewerPath: string = '';

implementation

uses InOut, ShellAPI;

procedure WriteTempTeXFile(const FileName, TpXName: string);
var
  IncludeFile: string;
  List: TStringList;
begin
  IncludeFile := ExtractFilePath(Application.ExeName) + 'preview.tex.inc';
  List := TStringList.Create;
  try
    if FileExists(IncludeFile) then List.LoadFromFile(IncludeFile)
    else
    begin
      List.Add('\documentclass[a4paper,10pt]{article}');
      List.Add('\usepackage{color}');
      List.Add('%\pdfoutput=0 % uncomment this to run PDFTeX in TeX mode');
      List.Add('\usepackage{ifpdf}');
      List.Add('\ifx\pdftexversion\undefined %if using TeX');
      List.Add('  \usepackage{graphicx}');
      List.Add('\else %if using PDFTeX');
      List.Add('  \usepackage[pdftex]{graphicx}');
      List.Add('\fi');
      List.Add('\ifpdf %if using PDFTeX in PDF mode');
      List.Add('  \DeclareGraphicsExtensions{.pdf,.png,.mps}');
      List.Add('  \usepackage{pgf}');
      List.Add('\else %if using TeX or PDFTeX in TeX mode');
      List.Add('  \usepackage{graphicx}');
      List.Add('  \DeclareGraphicsExtensions{.eps,.bmp}');
      List.Add('  \DeclareGraphicsRule{.emf}{bmp}{}{}% declare EMF filename extension');
      List.Add('  \DeclareGraphicsRule{.png}{bmp}{}{}% declare PNG filename extension');
      List.Add('  \usepackage{pgf}');
      List.Add('  \usepackage{pstricks}%variant: \usepackage{pst-all}');
      List.Add('\fi');
      List.Add('\usepackage{epic,bez123}');
      List.Add('\usepackage{floatflt}% package for floatingfigure environment');
      List.Add('\usepackage{wrapfig}% package for wrapfigure environment');
      List.SaveToFile(IncludeFile);
    end;
    List.Add('\begin{document}');
    //List.Add('\hrule height 1ex');
    List.Add('\thispagestyle{empty}');
    List.Add('\ ');
    List.Add('');
    List.Add('\input{' + TpXName + '}');
    List.Add('');
    List.Add('\ ');
    List.Add('\end{document}');
    List.SaveToFile(FileName);
  finally
    List.Free;
  end;
end;

procedure Preview_LaTeX(const Drawing: TDrawing2D; PreviewKind:
  TLaTeXPreviewKind);
var
  TempDir, TempIncludePath, TempTpX, TempTeX, TempDvi, TempTeXLog: string;
  Res: Boolean;
  LatexCompPath, ViewerPath: string;
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
    ltxview_PDF: TempDvi := TempDir + '(doc)TpX.pdf';
  end;
  TryDeleteFile(TempDvi);
  // Set IncludePath to empty string for preview
  TempIncludePath := Drawing.IncludePath;
  Drawing.IncludePath := '';
  try
    StoreToFile_TpX(Drawing, TempTpX);
  except
    Drawing.IncludePath := TempIncludePath;
    Exit;
  end;
  Drawing.IncludePath := TempIncludePath;
  if not FileExists(TempTpX) then
  begin
    Application.MessageBox('Temporary TpX file not created',
      'Error', MB_OK);
    Exit;
  end;
  WriteTempTeXFile(TempTeX, TempTpX0);
  if not FileExists(TempTeX) then
  begin
    Application.MessageBox('Temporary TeX file not created',
      'Error', MB_OK);
    TryDeleteFile(TempTpX);
    Exit;
  end;
  case PreviewKind of
    ltxview_Dvi, ltxview_PS: LatexCompPath := LatexPath;
    ltxview_PDF: LatexCompPath := PdfLatexPath;
  end;

  {if not FileExists(ExpandFileName(LatexCompPath)) then
  begin
    Application.MessageBox(PChar('LaTeX (PDFLaTeX) path not found'+ExpandFileName(LatexCompPath)),
      'Error', MB_OK);
    Exit;
  end;}
  try
    TryDeleteFile(TempTeXLog);
    Res := FileExec(Format('"%s" "%s"',
      [LatexCompPath, TempTeX]), '', '',
      {IncludeTrailingPathDelimiter(ExtractFilePath(FileName))} TempDir,
      False, True);
    if not FileExists(TempDvi) then
    begin
      if not FileExists(TempTeXLog) then
        Application.MessageBox('DVI (PDF) file not created',
          'Error', MB_OK)
      else if Application.MessageBox(
        'DVI (PDF) file not created. Do you want to see log file?',
        'Error', MB_OKCANCEL) = idOK then
      begin
        OpenOrExec(TextViewerPath, TempTeXLog);
        Res := False;
      end;
      Exit;
    end;
    if PreviewKind = ltxview_PS then
    begin
      TryDeleteFile(ChangeFileExt(TempDvi, '.ps'));
      Res := FileExec(Format('"%s" "%s"', // -E
        [DviPsPath, ExtractFileName(TempDvi)]), '', '',
      {IncludeTrailingPathDelimiter(ExtractFilePath(FileName))} TempDir,
        False, True);
      TempDvi := ChangeFileExt(TempDvi, '.ps');
      if not FileExists(TempDvi) then
      begin
        //if not FileExists(TempTeXLog) then
        Application.MessageBox('PS file not created',
          'Error', MB_OK);
        {else if Application.MessageBox(
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
      ltxview_PDF: ViewerPath := PdfViewerPath;
    end;
    OpenOrExec(ViewerPath, TempDvi);
  finally
    TryDeleteFile(TempTpX);
    TryDeleteFile(TempTeX);
    //TryDeleteFile(TempDvi);
  end;
end;

procedure Preview_Picture(const Drawing: TDrawing2D;
  PreviewKind: TPicturePreviewKind);
var
  TmpFileName, Ext: string;
begin
  TmpFileName := GetTempDir + '(img)TpX';
  case PreviewKind of
    pview_SVG: Ext := 'svg';
    pview_EMF: Ext := 'emf';
    pview_EPS: Ext := 'eps';
    pview_PNG: Ext := 'png';
    pview_BMP: Ext := 'bmp';
    pview_PDF: Ext := 'pdf';
    {pview_MP:    pview_MPS:    pview_EpsToPdf:}
  else
    Exit;
  end;
  TmpFileName := TmpFileName + '.' + Ext;
  TryDeleteFile(TmpFileName);
  case PreviewKind of
    pview_SVG:
      StoreToFile_Saver(Drawing,
        TmpFileName, T_SVG_Export);
    pview_EMF:
      Drawing.SaveToFile_EMF(TmpFileName);
    pview_EPS:
      StoreToFile_Saver(Drawing,
        TmpFileName, T_PostScript_Export);
    pview_PNG:
      StoreToFile_Saver(Drawing,
        TmpFileName, T_PNG_Export);
    pview_BMP:
      StoreToFile_Saver(Drawing,
        TmpFileName, T_Bitmap_Export);
    pview_PDF:
      StoreToFile_Saver(Drawing,
        TmpFileName, T_PDF_Export);
    {pview_MP:
      StoreToFile_Saver(Drawing,
        TmpFileName, T_MetaPost_Export);
    pview_MPS:
      StoreToFile_MPS(Drawing, TmpFileName);
    pview_EpsToPdf:
      StoreToFile_EpsToPdf(Drawing, TmpFileName, False);}
  else
    //DoSaveDrawing(TmpFileName);
  end;
  if not FileExists(TmpFileName) then
  begin
    Application.MessageBox(
      PChar(Format('Output file % not created', [Ext])),
      'Error', MB_OK);
    Exit;
  end;
  if PreviewKind in [pview_EPS]
    then OpenOrExec(PSViewerPath, TmpFileName)
  else OpenOrExec('', TmpFileName);
end;

end.

