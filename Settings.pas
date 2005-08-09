unit Settings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, Grids, ValEdit, Options0, StdCtrls,
  ExtCtrls,
  ComCtrls;

type
  TSettingsForm = class(TForm)
    VLE: TValueListEditor;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
    FontDialog1: TFontDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose:
      Boolean);
    procedure VLESelectCell(Sender: TObject; ACol, ARow:
      Integer;
      var CanSelect: Boolean);
    procedure VLEEditButtonClick(Sender: TObject);
    procedure VLEGetPickList(Sender: TObject; const KeyName:
      string;
      Values: TStrings);
  private
    { Private declarations }
  public
    { Public declarations }
    SettingsList: TOptionsList;
    procedure FillSettings;
  end;

var
  SettingsForm: TSettingsForm;

implementation

uses InOut, MainUnit, CADSys4, PreView, EMF_Unit;

{$R *.dfm}

const
  IniFileName0 = 'TpX.ini';

var
  IniFileName: string = IniFileName0;

procedure TSettingsForm.FormCreate(Sender: TObject);
var
  I, Row: Integer;
begin
  IniFileName := IncludeTrailingPathDelimiter(
    ExtractFilePath(ParamStr(0))) + IniFileName0;
  SettingsList := TOptionsList.Create;

  SettingsList.AddRealType('PicScale_Default',
    @PicScale_Default, 'Picture scale (mm per unit)');
  SettingsList.AddRealType('Border_Default',
    @Border_Default, 'Picture border (mm)');
  SettingsList.AddChoice('TeXFormat_Default',
    @TeXFormat_Default, TeXFormat_Choice,
    'Format for including picture in TeX');
  SettingsList.AddChoice('PdfTeXFormat_Default',
    @PdfTeXFormat_Default, PdfTeXFormat_Choice,
    'Format for including picture in PdfTeX');
  SettingsList.AddRealType('PicUnitLength_Default',
    @PicUnitLength_Default, 'Picture unit length (mm)');
  SettingsList.AddRealType('PicMagnif_Default',
    @PicMagnif_Default,
    'Picture phisical size magnification factor');
  SettingsList.AddString('IncludePath_Default',
    @(IncludePath_Default),
    'Path to add before \includegraphics file name (like mypictures/)');

  SettingsList.AddRealType('LineWidth_Default',
    @LineWidthBase_Default, 'Thin line width (mm)');
  SettingsList.AddRealType('ArrowsSize_Default',
    @ArrowsSize_Default, 'Arrows size');
  SettingsList.AddRealType('StarsSize_Default',
    @StarsSize_Default, 'Stars size');
  SettingsList.AddRealType('HatchingStep_Default',
    @HatchingStep_Default, 'Hatching step (mm)');
  SettingsList.AddRealType('HatchingLineWidth_Default',
    @HatchingLineWidth_Default, 'Hatching line width (fraction of LineWidth)');
  SettingsList.AddRealType('DottedSize_Default',
    @DottedSize_Default, 'Dotted line size (mm)');
  SettingsList.AddRealType('DashSize_Default',
    @DashSize_Default, 'Dashed line size (mm)');
  SettingsList.AddRealType('DefaultFontHeight_Default',
    @DefaultFontHeight_Default, 'Default font height');
  SettingsList.AddFontName('FontName_Default',
    @FontName_Default, 'Default font');
  {SettingsList.AddBoolean('ScalePhysicalUnits',
    @(MainForm.ScalePhysical.Checked),
    'Scale physical units when transforming the whole picture');}

  SettingsList.AddRealType('TeXMinLine_Default',
    @TeXMinLine_Default, 'TeX minimum line length');
  SettingsList.AddBoolean('TeXCenterFigure_Default',
    @TeXCenterFigure_Default, 'Center TeX figure');
  SettingsList.AddChoice('TeXFigure_Default',
    @TeXFigure_Default, TeXFigure_Choice,
    'TeX figure environment:' + EOL +
    'none - no figure' + EOL +
    'figure - standard {figure} environment' + EOL +
    'floatingfigure - {floatingfigure} from floatflt package'
    + EOL +
    'wrapfigure - {wrapfigure} from wrapfig package');
  SettingsList.AddBoolean('MetaPostTeXText_Default',
    @(MetaPostTeXText_Default),
    'Use TeX text in MetaPost files');

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
  SettingsList.AddFilePath('TextViewerPath', @(TextViewerPath),
    '*.exe|*.exe',
    'Path to text viewer (e.g. notepad.exe). Leave this blank to use the default viewer');
  SettingsList.AddString('PostscriptPrinter', @(PostscriptPrinter),
    'Postscript printer to create EPS files');
  SettingsList.AddBoolean('PostscriptPrinterUseOffset',
    @(PostscriptPrinterUseOffset),
    'Use offset when creating EPS files');


  SettingsList.AddFilePath('MetaPostPath', @(MetaPostPath),
    '*.exe|*.exe',
    'Path to MetaPost program (mpost.exe or mp.exe). Needed for exporting to MetaPost EPS (.mps)');
  SettingsList.AddFilePath('EpsToPdfPath', @(EpsToPdfPath),
    '*.exe|*.exe',
    'Path to EpsToPdf program (epstopdf.exe). Needed for convering EPS to PDF');
  SettingsList.AddFilePath('Font_pfb_Path', @(Font_pfb_Path),
    'Type 1 fonts (*.pfb)|*.pfb',
    'Path to Type 1 font (.pfb). Needed for embedding font into EPS');
  SettingsList.AddFilePath('PsToEditPath', @(PsToEditPath),
    '*.exe|*.exe',
    'Path to PsToEdit program (pstoedit.exe). Needed for convering EPS to EMF (used for EPS import)');
  SettingsList.AddString('PsToEditFormat', @(PsToEditFormat),
    'Format for PsToEdit program for convering EPS to EMF (used for EPS import).' +
    ' Format can be emf or one of the better implementations (wemf, wemfc, wemfnss)' +
    ' which are available in registered version of PsToEdit');

  SettingsList.AddStringList('RecentFiles', MainForm.RecentFiles, '');
  SettingsList.Add(
    TTpXExtAssocOption.Create('ExtAssoc', nil,
    'Associate ".TpX" extention with TpX'));
  SettingsList.AddBoolean('ShowGrid',
    @(MainForm.LocalView.ShowGrid), 'Show grid');
  //SettingsList.AddInteger('GridDeltaX',    @(MainForm.LocalView.GridDeltaX), 1, 2000,    'Grid delta, X axis');
  //SettingsList.AddInteger('GridDeltaY',    @(MainForm.LocalView.GridDeltaY), 1, 2000,    'Grid delta, Y axis');
  SettingsList.AddBoolean('ShowRulers',
    @(MainForm.LocalView.ShowRulers), 'Show rulers');
  SettingsList.AddBoolean('ShowScrollBars',
    @(MainForm.ShowScrollBars.Checked), 'Show scroll bars');
  SettingsList.AddBoolean('AreaSelectInside',
    @(MainForm.AreaSelectInside.Checked),
    'Area select inside only');
  SettingsList.AddInteger('Mainform.Width',
    @(MainForm.Width), 1, 2000, '');
  SettingsList.AddInteger('Mainform.Height',
    @(MainForm.Height), 1, 2000, '');

  if not FileExists(IniFileName) then Exit;
  VLE.Strings.LoadFromFile(IniFileName);
  for I := 0 to SettingsList.Count - 1 do
    if VLE.FindRow((SettingsList[I] as TOptionData).Key, Row)
      then
      (SettingsList[I] as TOptionData).AsString
        := VLE.Cells[1, Row]

end;

procedure TSettingsForm.FormDestroy(Sender: TObject);
begin
  FillSettings;
  VLE.Strings.SaveToFile(IniFileName);
  SettingsList.Free;
end;

procedure TSettingsForm.FillSettings;
var
  Data: TOptionData;
  I: Integer;
begin
  VLE.Strings.Clear;
  for I := 0 to SettingsList.Count - 1 do
  begin
    Data := SettingsList.Items[I] as TOptionData;
    VLE.InsertRow(Data.Key, Data.AsString, True);
  end;
end;

procedure TSettingsForm.FormShow(Sender: TObject);
var
  Data: TOptionData;
  I: Integer;
begin
  FillSettings;
  for I := 0 to SettingsList.Count - 1 do
  begin
    Data := SettingsList.Items[I] as TOptionData;
    if Data.GetMask <> '' then
      VLE.ItemProps[I].EditMask := Data.GetMask;
    if Data is TFilePathOption then
      VLE.ItemProps[I].EditStyle := esEllipsis;
    if Data is TFontNameOption then
      VLE.ItemProps[I].EditStyle := esEllipsis;
    if Data is TChoiceOption then
    begin
      VLE.ItemProps[I].EditStyle := esPickList;
      VLE.ItemProps[I].ReadOnly := True;
    end;
  end;
end;

procedure TSettingsForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  I: Integer;
begin
  if ModalResult <> mrOK then Exit;
  for I := 0 to SettingsList.Count - 1 do
  try
    (SettingsList[I] as TOptionData).AsString :=
      VLE.Cells[1, I + 1];
  except
    VLE.SetFocus;
    VLE.Row := I + 1;
    CanClose := False;
    Exit;
  end;
end;

procedure TSettingsForm.VLESelectCell(Sender: TObject; ACol,
  ARow: Integer;
  var CanSelect: Boolean);
begin
  Memo1.Text :=
    (SettingsList.Items[ARow - 1] as TOptionData).Hint;
  CanSelect := True;
end;

procedure TSettingsForm.VLEEditButtonClick(Sender: TObject);
begin
  if VLE.Row < 1 then Exit;
  if SettingsList[VLE.Row - 1] is TFilePathOption then
    with SettingsList[VLE.Row - 1] as TFilePathOption do
    begin
      OpenDialog1.FileName := VLE.Cells[1, VLE.Row];
      OpenDialog1.Filter := Filter;
      if not OpenDialog1.Execute then Exit;
      VLE.Cells[1, VLE.Row] := OpenDialog1.FileName;
    end
  else if SettingsList[VLE.Row - 1] is TFontNameOption then
    with SettingsList[VLE.Row - 1] as TFontNameOption do
    begin
      FontDialog1.Font.Name := VLE.Cells[1, VLE.Row];
      if not FontDialog1.Execute then Exit;
      VLE.Cells[1, VLE.Row] := FontDialog1.Font.Name;
    end;
end;

procedure TSettingsForm.VLEGetPickList(Sender: TObject;
  const KeyName: string; Values: TStrings);
begin
  if VLE.Row < 1 then Exit;
  if SettingsList[VLE.Row - 1] is TChoiceOption then
  begin
    with SettingsList[VLE.Row - 1] as TChoiceOption do
      Values.AddStrings(Choices);
  end;
end;

end.

