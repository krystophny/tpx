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
      List.Add('\ifx\pdftexversion\undefined %if using TeX');
      List.Add('  \usepackage{graphicx}');
      List.Add('  \usepackage{pstricks}%variant: \usepackage{pst-all}');
      List.Add('  \DeclareGraphicsExtensions{.eps,.bmp}');
      List.Add('  \DeclareGraphicsRule{.emf}{bmp}{}{}% declare EMF filename extension');
      List.Add('  \DeclareGraphicsRule{.png}{bmp}{}{}% declare PNG filename extension');
      List.Add('\else %if using PDFTeX');
      List.Add('  \usepackage[pdftex]{graphicx}');
      List.Add('  \DeclareGraphicsExtensions{.pdf,.png,.mps}');
      List.Add('\fi');
      List.Add('\usepackage{epic,bez123}');
      List.Add('\usepackage{floatflt}% package for floatingfigure environment');
      List.Add('\usepackage{wrapfig}% package for wrapfigure environment');
      List.SaveToFile(IncludeFile);
    end;
    List.Add('\begin{document}');
    List.Add('\input{' + TpXName + '}');
    List.Add('\end{document}');
    List.SaveToFile(FileName);
  finally
    List.Free;
  end;
end;

procedure Preview_LaTeX(const Drawing: TDrawing2D; PreviewKind:
  TLaTeXPreviewKind);
var
  TempDir, TempIncludePath, TempTpX, TempTeX, TempDvi: string;
  Res: Boolean;
  LatexCompPath, ViewerPath: string;
const
  TempTpX0 = '(tmp)TpX.TpX';
begin
  TempDir := GetTempDir;
  TempTpX := TempDir + TempTpX0;
  TempTeX := TempDir + '(tmp)TpX-.tex';
  //StringReplace(TpXName, '\', '/', [rfReplaceAll])
  case PreviewKind of
    ltxview_Dvi, ltxview_PS: TempDvi := TempDir + '(tmp)TpX-.dvi';
    ltxview_PDF: TempDvi := TempDir + '(tmp)TpX-.pdf';
  end;
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
    Res := FileExec(Format('"%s" "%s"',
      [LatexCompPath, TempTeX]), '', '',
      {IncludeTrailingPathDelimiter(ExtractFilePath(FileName))} TempDir,
      False, True);
    if FileExists(TempDvi) then
    begin
      case PreviewKind of
        ltxview_Dvi: ViewerPath := DviViewerPath;
        ltxview_PS: ViewerPath := PSViewerPath;
        ltxview_PDF: ViewerPath := PdfViewerPath;
      end;
      if ViewerPath = '' then
        //WinExec(PChar(TempDvi), 0)
{   H := } ShellExecute(Application.Handle,
          PChar('open'),
          PChar(TempDvi),
          nil {PChar(Parameters)}, nil {PChar(Directory)}, SW_SHOW)
      else
        Res := FileExec(Format('"%s" "%s"',
          [ViewerPath, TempDvi]), '', '',
      {IncludeTrailingPathDelimiter(ExtractFilePath(FileName))}'',
          False, False);
    end
    else
      Application.MessageBox('DVI (PDF) file not created',
        'Error', MB_OK);
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
  TmpFileName := GetTempDir + '(tmp)TpX-';
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
  ShellExecute(Application.Handle,
    PChar('open'),
    PChar(TmpFileName),
    nil {PChar(Parameters)}, nil {PChar(Directory)}, SW_SHOW)
end;

end.

