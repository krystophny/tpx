unit Options;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, Grids, ValEdit, Options0, StdCtrls,
  ExtCtrls,
  ComCtrls;

type
  TOptionsForm = class(TForm)
    VLE: TValueListEditor;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose:
      Boolean);
    procedure VLESelectCell(Sender: TObject; ACol, ARow:
      Integer;
      var CanSelect: Boolean);
    procedure VLEGetPickList(Sender: TObject; const KeyName:
      string;
      Values: TStrings);
  private
    { Private declarations }
  public
    { Public declarations }
    OptionsList: TOptionsList;
  end;

var
  OptionsForm: TOptionsForm;

implementation

uses InOut, MainUnit, CADSys4;

{$R *.dfm}

procedure TOptionsForm.FormCreate(Sender: TObject);
begin
  OptionsList := TOptionsList.Create;
  with MainForm.TheDrawing do
  begin
    OptionsList.AddString('Caption', @(Caption), 'Caption');
    OptionsList.AddString('Comment', @(Comment), 'Comment');
    OptionsList.AddString('Label', @(FigLabel), 'Label');
    OptionsList.AddRealType('PicScale', @PicScale,
      'Picture scale (mm per unit)');
    OptionsList.AddRealType('Border', @Border,
      'Picture border (mm)');
    OptionsList.AddChoice('TeXFormat',
      @TeXFormat, TeXFormat_Choice,
      'Format for including picture in TeX');
    OptionsList.AddChoice('PdfTeXFormat',
      @PdfTeXFormat, PdfTeXFormat_Choice,
      'Format for including picture in PdfTeX');
    OptionsList.AddRealType('PicUnitLength',
      @PicUnitLength, 'Picture unit length (mm)');
    OptionsList.AddRealType('PicMagnif',
      @PicMagnif,
      'Picture phisical size magnification factor. Use PicScale to represent picture space coordinates in mm. Use PicMagnif to change the meaning of mm for quick rescaling of the picture');
    OptionsList.AddString('IncludePath',
      @(IncludePath),
      'Path to add before \includegraphics file name (like mypictures/)');
      
    OptionsList.AddRealType('LineWidth', @LineWidth,
      'Thin line width (mm)');
    OptionsList.AddRealType('ArrowsSize',
      @(ArrowsSize), 'Arrows size');
    OptionsList.AddRealType('StarsSize',
      @(StarsSize), 'Stars size');
    OptionsList.AddRealType('HatchingStep',
      @HatchingStep, 'Hatching step (mm)');
    OptionsList.AddRealType('DottedSize', @DottedSize,
      'Dotted line size (mm)');
    OptionsList.AddRealType('DashSize', @DashSize,
      'Dashed line size (mm)');
    OptionsList.AddRealType('DefaultFontHeight',
      @(DefaultFontHeight), 'Default font height');
      
    OptionsList.AddRealType('TeXMinLine',
      @TeXMinLine, 'TeX minimum line length');
    OptionsList.AddBoolean('TeXCenterFigure',
      @TeXCenterFigure, 'Center TeX figure');
    OptionsList.AddChoice('TeXFigure',
      @TeXFigure, TeXFigure_Choice,
      'TeX figure environment:' + EOL +
      'none - no figure' + EOL +
      'figure - standard {figure} environment' + EOL +
      'floatingfigure - {floatingfigure} from floatflt package'
      + EOL +
      'wrapfigure - {wrapfigure} from wrapfig package');
    OptionsList.AddString('TeXFigurePlacement',
      @(TeXFigurePlacement),
      'The optional argument [placement] determines where LaTeX will try to place your figure. There are four places where LaTeX can possibly put a float:' + EOL
      + 'h (Here) - at the position in the text where the figure environment appears'
      + EOL
      + 't (Top) - at the top of a text page' + EOL
      + 'b (Bottom) - at the bottom of a text page' + EOL +
      'p (Page of floats) - on a separate float page, which is a page containing no text, only floats'
      + EOL +
      'Putting ! as the first argument in the square brackets will encourage LATEX to do what you say, even if the result''s sub-optimal.'
      + EOL +
      ' Example: htbp' + EOL + EOL +
      'For wrapfigure placement is one of  r, l, i, o, R, L, I, O,  for right, left,  inside, outside, (here / FLOAT)'
      + EOL + EOL +
      'The floatingfigure placement option may be either one of the following: r, l, p, or v. The options all overrule any present package option which may be in effect.  The options have the following functions:'
      + EOL +
      'r  Forces the current floating figure to be typset to the right in a paragraph'
      + EOL +
      'l Forces the current floating figure to be typset to the left in a paragraph'
      + EOL +
      'p Forces the current floating figure to be typset to the right in a paragraph if the pagenumber is odd, and to the left if even'
      + EOL +
      'v Applies the package option to the current figure, and if no package option is specified, it forces the current floating figure to be typset to the right in a paragraph if the pagenumber is odd, and to the left if even'
      );
    OptionsList.AddString('TeXFigurePrologue',
      @(TeXFigurePrologue), 'Text to put before float');
    OptionsList.AddString('TeXFigureEpilogue',
      @(TeXFigureEpilogue), 'Text to put after float');
    OptionsList.AddString('TeXPicPrologue',
      @(TeXPicPrologue),
      'Text to put before picture/includegraphics');
    OptionsList.AddString('TeXPicEpilogue',
      @(TeXPicEpilogue),
      'Text to put after picture/includegraphics');

    OptionsList.AddBoolean('MetaPostTeXText',
      @(MetaPostTeXText), 'Use TeX text in MetaPost files');
  end;
end;

procedure TOptionsForm.FormDestroy(Sender: TObject);
begin
  OptionsList.Free;
end;

procedure TOptionsForm.FormShow(Sender: TObject);
var
  Data: TOptionData;
  I: Integer;
begin
  VLE.Strings.Clear;
  for I := 0 to OptionsList.Count - 1 do
  begin
    Data := OptionsList.Items[I] as TOptionData;
    VLE.InsertRow(Data.Key, Data.AsString, True);
    if Data.GetMask <> '' then
      VLE.ItemProps[I].EditMask := Data.GetMask;
    if Data is TChoiceOption then
    begin
      VLE.ItemProps[I].EditStyle := esPickList;
      VLE.ItemProps[I].ReadOnly := True;
    end;
  end;
end;

procedure TOptionsForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  I: Integer;
begin
  if ModalResult <> mrOK then Exit;
  for I := 0 to OptionsList.Count - 1 do
  try
    (OptionsList[I] as TOptionData).AsString :=
      VLE.Cells[1, I + 1];
  except
    VLE.SetFocus;
    VLE.Row := I + 1;
    CanClose := False;
    Exit;
  end;
end;

procedure TOptionsForm.VLESelectCell(Sender: TObject; ACol,
  ARow: Integer;
  var CanSelect: Boolean);
begin
  Memo1.Text :=
    (OptionsList[ARow - 1] as TOptionData).Hint;
  CanSelect := True;
end;

procedure TOptionsForm.VLEGetPickList(Sender: TObject;
  const KeyName: string; Values: TStrings);
begin
  if VLE.Row < 1 then Exit;
  if OptionsList[VLE.Row - 1] is TChoiceOption then
  begin
    with OptionsList[VLE.Row - 1] as TChoiceOption do
      Values.AddStrings(Choices);
  end;
end;

end.

