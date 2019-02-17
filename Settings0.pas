unit Settings0;

{$IFNDEF VER140}
{$MODE Delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, Options0, MainUnit;

procedure Initialize;
procedure LoadSettings_Ex(MainForm: TMainForm);
procedure LoadSettings;
procedure SaveSettings;

var
  SettingsList: TOptionsList;

implementation

uses Output, Input, Drawings, Preview, SysBasic, Bitmaps
{$IFDEF VER140}
  , EMF_Unit, Modes
{$ENDIF}
  ;

const
  IniFileName0 = 'TpX.ini';

var
  IniFileName: string = IniFileName0;

procedure LoadSettings;
var
  I: Integer;
  Strings: TStringList;
  St: string;
begin
  if not FileExists(IniFileName) then Exit;
  Strings := TStringList.Create;
  try
    Strings.LoadFromFile(IniFileName);
    for I := 0 to SettingsList.Count - 1 do
    begin
      St := Strings.Values[(SettingsList[I] as TOptionData).Key];
      // Temporary fix against saving of bad settings
      if (SettingsList[I] as TOptionData).Key = 'LineWidth_Default'
        then
      begin
        if StrToFloat(St) <= 0
          then St := FloatToStr(LineWidthBase_Default);
      end;
      if St <> '' then
        (SettingsList[I] as TOptionData).AsString := St;
    end;
  finally
    Strings.Free;
  end;
end;

function FillSettings(Strings: TStrings): Boolean;
var
  Data: TOptionData;
  I: Integer;
begin
  Result := False;
  Strings.Clear;
  for I := 0 to SettingsList.Count - 1 do
  begin
    Data := SettingsList.Items[I] as TOptionData;
    // Temporary fix against saving of bad settings
    if Data.Key = 'LineWidth_Default' then
      if StrToFloat(Data.AsString) <= 0 then
        Break
      else
        Result := True;
    Strings.Add(Data.Key + '=' + Data.AsString);
    //Strings.Values[Data.Key] := Data.AsString;
    //Application.MessageBox(PChar(IntToStr(Strings.Count) + ' >>' + Data.AsString + '<<'), nil);
  end;
  if not Result then
    MessageBoxError('Bad settings were not saved to ini file');
end;

procedure SaveSettings;
var
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    if FillSettings(Strings)
      then Strings.SaveToFile(IniFileName);
  finally
    Strings.Free;
  end;
  if not OutHelp then Exit;
  // Save drawing options hints for help
  Strings := TStringList.Create;
  try
    Strings.Text := SettingsList.HintsText;
    Strings.SaveToFile(ExtractFilePath(ParamStr(0))
      + '\Help\program_settings.inc');
  finally
    Strings.Free;
  end;
end;

procedure Initialize;
begin
  IniFileName := IncludeTrailingPathDelimiter(
    ExtractFilePath(ParamStr(0))) + IniFileName0;
  SettingsList := TOptionsList.Create;

  SettingsList.AddRealType('PicScale_Default',
    @PicScale_Default, 'Default value of PicScale');
  SettingsList.AddRealType('Border_Default',
    @Border_Default, 'Default value of Border');
  SettingsList.AddChoice('TeXFormat_Default',
    @TeXFormat_Default, TeXFormat_Choice,
    'Default value of TeXFormat');
  SettingsList.AddChoice('PdfTeXFormat_Default',
    @PdfTeXFormat_Default, PdfTeXFormat_Choice,
    'Default value of PdfTeXFormat');
  SettingsList.AddRealType('BitmapRes_Default',
    @BitmapRes_Default, 'Default value of BitmapRes');
  SettingsList.AddRealType('PicMagnif_Default',
    @PicMagnif_Default, 'Default value of PicMagnif');
  SettingsList.AddString('IncludePath_Default',
    @(IncludePath_Default), 'Default value of IncludePath');
  SettingsList.AddRealType('LineWidth_Default',
    @LineWidthBase_Default, 'Default value of LineWidth');
  SettingsList.AddRealType('ArrowsSize_Default',
    @ArrowsSize_Default, 'Default value of ArrowsSize');
  SettingsList.AddRealType('StarsSize_Default',
    @StarsSize_Default, 'Default value of StarsSize');
  SettingsList.AddRealType('HatchingStep_Default',
    @HatchingStep_Default, 'Default value of HatchingStep');
  SettingsList.AddRealType('HatchingLineWidth_Default',
    @HatchingLineWidth_Default,
    'Default value of HatchingLineWidth');
  SettingsList.AddRealType('DottedSize_Default',
    @DottedSize_Default, 'Default value of DottedSize');
  SettingsList.AddRealType('DashSize_Default',
    @DashSize_Default, 'Default value of DashSize');
  SettingsList.AddRealType('DefaultFontHeight_Default',
    @DefaultFontHeight_Default,
    'Default value of DefaultFontHeight');
  SettingsList.AddFontName('FontName_Default',
    @FontName_Default, 'Default value of FontName');
  SettingsList.AddRealType('DefaultSymbolSize_Default',
    @DefaultSymbolSize_Default,
    'Default value of DefaultSymbolSize');
  SettingsList.AddRealType('ApproximationPrecision_Default',
    @ApproximationPrecision_Default,
    'Default value of ApproximationPrecision');
  SettingsList.AddBoolean('TeXCenterFigure_Default',
    @TeXCenterFigure_Default,
    'Default value of TeXCenterFigure');
  SettingsList.AddChoice('TeXFigure_Default',
    @TeXFigure_Default, TeXFigure_Choice,
    'Default value of TeXFigure');
  SettingsList.AddBoolean('FontSizeInTeX_Default',
    @(FontSizeInTeX_Default),
    'Default value of FontSizeInTeX');
  SettingsList.AddBoolean('MetaPostTeXText_Default',
    @(MetaPostTeXText_Default),
    'Default value of MetaPostTeXText');

  {SettingsList.AddBoolean('ScalePhysicalUnits',
    @(MainForm.ScalePhysical.Checked),
    'Scale physical units when transforming the whole picture');}
  SettingsList.AddFilePath('LatexPath', @(LatexPath),
    '*.exe|*.exe', 'Path to LaTeX (latex.exe)');
  SettingsList.AddFilePath('PdfLatexPath', @(PdfLatexPath),
    '*.exe|*.exe', 'Path to PDFLaTeX (pdflatex.exe)');
  SettingsList.AddFilePath('DviPsPath', @(DviPsPath),
    '*.exe|*.exe', 'Path to DVIPS (dvips.exe)');
  SettingsList.AddFilePath('DviViewerPath', @(DviViewerPath),
    '*.exe|*.exe',
    'Path to DVI viewer (e.g. yap.exe). Leave this blank to use the default viewer');
  SettingsList.AddFilePath('PdfViewerPath', @(PdfViewerPath),
    '*.exe|*.exe',
    'Path to PDF viewer (e.g. acrobat.exe). Leave this blank to use the default viewer');
  SettingsList.AddFilePath('PSViewerPath', @(PSViewerPath),
    '*.exe|*.exe',
    'Path to PostScript viewer (e.g. gsview32.exe). Leave this blank to use the default viewer');
  SettingsList.AddFilePath('SvgViewerPath', @(SvgViewerPath),
    '*.exe|*.exe',
    'Path to SVG viewer (e.g. iexplore.exe). Leave this blank to use the default viewer');
  SettingsList.AddFilePath('PngViewerPath', @(PngViewerPath),
    '*.exe|*.exe',
    'Path to PNG viewer. Leave this blank to use the default viewer');
  SettingsList.AddFilePath('BmpViewerPath', @(BmpViewerPath),
    '*.exe|*.exe',
    'Path to BMP viewer. Leave this blank to use the default viewer');
  SettingsList.AddFilePath('TextViewerPath', @(TextViewerPath),
    '*.exe|*.exe',
    'Path to text viewer (e.g. notepad.exe). Leave this blank to use the default viewer');
{$IFDEF VER140}
  SettingsList.AddString('PostscriptPrinter', @(PostscriptPrinter),
    'Postscript printer to create EPS files');
  SettingsList.AddBoolean('PostscriptPrinterUseOffset',
    @(PostscriptPrinterUseOffset),
    'Use offset when creating EPS files');
{$ELSE}
  SettingsList.AddFilePath('HtmlViewerPath', @(HtmlViewerPath),
    '*.exe|*.exe',
    'Path to HTML viewer. Leave this blank to use the default viewer');
{$ENDIF}

  SettingsList.AddFilePath('MetaPostPath', @(MetaPostPath),
    '*.exe|*.exe',
    'Path to MetaPost program (mpost.exe or mp.exe). Needed for exporting to MetaPost EPS (.mps)');
  SettingsList.AddFilePath('Font_pfb_Path', @(Font_pfb_Path),
    'Type 1 fonts (*.pfb)|*.pfb',
    'Path to Type 1 font (.pfb). Needed for embedding font into EPS');
  SettingsList.AddFilePath('PsToEditPath', @(PsToEditPath),
    '*.exe|*.exe',
    'Path to PsToEdit program (pstoedit.exe).' +
    ' Needed for converting EPS to EMF or SVG (used for EPS import)');
  SettingsList.AddString('PsToEditFormat', @(PsToEditFormat),
    'Format for PsToEdit program for converting EPS to EMF or SVG (used for EPS import).' +
    ' Format can be emf or one of the better implementations (wemf, wemfc, wemfnss)' +
    ' which are available in registered version of PsToEdit.' +
    ' For SVG set plot-svg (free version) or svg (registered version)');
  SettingsList.AddFilePath('GhostscriptPath', @(GhostscriptPath),
    '*.exe|*.exe',
    'Path to Ghostscript program (gswin32c.exe).' +
    ' Needed for EPS to PDF conversion, custom export (latexcustom)' +
    ' and previewing PS and PDF files in Open dialog');
  SettingsList.AddString('GhostscriptCustomKeys',
    @(GhostscriptCustomKeys),
    'Ghostscript command line options used for custom export (latexcustom).' +
    ' Example: -r300 -sDEVICE=png256.' +
    ' List of possible devices taken literally from Ghostscript follows:' + EOL
    +
    ' bbox bit bitcmyk bitrgb bj10e bj200 bjc600 bjc800 bmp16 bmp16m bmp256' +
    ' bmp32b bmpgray bmpmono bmpsep1 bmpsep8 cdeskjet cdj550 cdjcolor cdjmono' +
    ' declj250 deskjet devicen display djet500 djet500c eps9high eps9mid epson' +
    ' epsonc epswrite ibmpro ijs jetp3852 jpeg jpeggray laserjet lbp8 lj250' +
    ' ljet2p ljet3 ljet3d ljet4 ljet4d ljetplus m8510 mswindll mswinpr2 necp6' +
    ' nullpage pbm pbmraw pcx16 pcx24b pcx256 pcxcmyk pcxgray pcxmono pdfwrite' +
    ' pgm pgmraw pgnm pgnmraw pj pjxl pjxl300 pkmraw png16 png16m png256' +
    ' pngalpha pnggray pngmono pnm pnmraw ppm ppmraw psdcmyk psdrgb psmono' +
    ' pswrite pxlcolor pxlmono r4081 spotcmyk st800 stcolor t4693d2 t4693d4' +
    ' t4693d8 tek4696 tiff12nc tiff24nc tiff32nc tiffcrle tiffg3 tiffg32d' +
    ' tiffg4 tiffgray tifflzw tiffpack tiffsep uniprint');
  SettingsList.AddFilePath('Bitmap2EpsPath', @(Bitmap2EpsPath),
    '*.exe|*.exe',
    'Path to a program (like sam2p or bmeps)' +
    ' which converts bitmaps to EPS files.' +
    ' (It is used for including bitmaps into output graphics).');
end;

procedure LoadSettings_Ex(MainForm: TMainForm);
begin
  SettingsList.AddStringList('RecentFiles',
    MainForm.EventManager.RecentFiles, 'List of recent files');
{$IFDEF VER140}
  SettingsList.Add(
    TTpXExtAssocOption.Create('ExtAssoc', nil,
    'Associate ".TpX" extension with TpX'));
{$ENDIF}
  SettingsList.AddBoolean('ShowGrid',
    @(MainForm.LocalView.ShowGrid), 'Show grid');
  SettingsList.AddBoolean('GridOnTop',
    @(MainForm.LocalView.GridOnTop), 'Grid on top');
  SettingsList.AddBoolean('ShowCrossHair',
    @(MainForm.LocalView.ShowCrossHair), 'Show crosshair');
  SettingsList.AddBoolean('ShowRulers',
    @(MainForm.LocalView.ShowRulers), 'Show rulers');
  SettingsList.AddBoolean('ShowScrollBars',
    @(MainForm.ShowScrollBars.Checked), 'Show scroll bars');
//  SettingsList.AddBoolean('ShowPropertiesToolbar1',
//    @(MainForm.ShowPropertiesToolbar1.Checked),
//      'Show the first properties toolbar');
//  SettingsList.AddBoolean('ShowPropertiesToolbar2',
//    @(MainForm.ShowPropertiesToolbar2.Checked),
//      'Show the second properties toolbar');
  SettingsList.AddBoolean('AreaSelectInside',
    @(AreaSelectInside), 'Area select inside only');
  SettingsList.AddBoolean('UseSnap',
    @(UseSnap), 'Snap to grid');
  SettingsList.AddBoolean('UseAngularSnap',
    @(UseAngularSnap), 'Angular snap (45 degrees)');
  SettingsList.AddInteger('Mainform.Left',
    @(MainForm.FormPos_Left), 1, 2000,
    'Main window left side position');
  SettingsList.AddInteger('Mainform.Top',
    @(MainForm.FormPos_Top), 1, 2000,
    'Main window top side position');
  SettingsList.AddInteger('Mainform.Width',
    @(MainForm.FormPos_Width), 1, 2000, 'Main window width');
  SettingsList.AddInteger('Mainform.Height',
    @(MainForm.FormPos_Height), 1, 2000, 'Main window height');
  SettingsList.AddBoolean('Mainform.Maximized',
    @(MainForm.FormPos_Maximized), 'Main window state');

  LoadSettings;
end;

initialization
  Settings0.Initialize;
finalization
  SettingsList.Free;
end.

