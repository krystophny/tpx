program TpX;

uses
  Forms,
  Settings0 in 'src\Settings0.pas',
  MainUnit in 'src\MainUnit.pas' {MainForm},
  Output in 'src\Output.pas',
  MprtEMF in 'src\MprtEMF.pas',
  EMF_Add in 'src\EMF_Add.pas',
  Propert in 'src\Propert.pas' {PropertiesForm},
  Options0 in 'src\Options0.pas',
  GObjects in 'src\GObjects.pas',
  Drawings in 'src\Drawings.pas',
  Options in 'src\Options.pas' {OptionsForm},
  AboutUnit in 'src\AboutUnit.pas' {AboutForm},
  TransForm in 'src\TransForm.pas' {TransfForm},
  ColorEtc in 'src\ColorEtc.pas',
  Preview in 'src\Preview.pas',
  PdfFonts in 'Lib\PowerPdf\PdfFonts.pas',
  Table in 'src\Table.pas' {TableForm},
  EMF_Unit in 'src\EMF_Unit.pas' {EMF_Form},
  Geometry in 'src\Geometry.pas',
  PrintEpsOpt in 'src\PrintEpsOpt.pas' {PrintEpsOptForm},
  ScaleStandardUnit in 'src\ScaleStandardUnit.pas' {ScaleStandardForm},
  DevCanvas in 'src\DevCanvas.pas',
  Input in 'src\Input.pas',
  ClpbrdOp in 'src\ClpbrdOp.pas',
  WinBasic in 'src\WinBasic.pas',
  SysBasic in 'src\SysBasic.pas',
  MiscUtils in 'src\MiscUtils.pas',
  PrimSAX in 'src\PrimSAX.pas',
  Numeric in 'src\Numeric.pas',
  Manage in 'src\Manage.pas',
  Modes in 'src\Modes.pas',
  ViewPort in 'src\ViewPort.pas',
  Pieces in 'src\Pieces.pas',
  Devices in 'src\Devices.pas',
  XmlOut in 'src\XmlOut.pas',
  DevSVG in 'src\DevSVG.pas',
  DevTikZ in 'src\DevTikZ.pas',
  DevPGF in 'src\DevPGF.pas',
  DevTeXPc in 'src\DevTeXPc.pas',
  TpXSaver in 'src\TpXSaver.pas',
  DevPS in 'src\DevPS.pas',
  DevPDF in 'src\DevPDF.pas',
  DevPSTr in 'src\DevPSTr.pas',
  DevMP in 'src\DevMP.pas',
  Modify in 'src\Modify.pas',
  GObjBase in 'src\GObjBase.pas',
  Bitmaps in 'src\Bitmaps.pas',
  InfoForm in 'src\InfoForm.pas' {InfoBox};

{$R *.RES}

begin
  if not CheckCommandLine then Exit;
  Application.Title := 'TpX';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TPropertiesForm, PropertiesForm);
  Application.CreateForm(TOptionsForm, OptionsForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.CreateForm(TTransfForm, TransfForm);
  Application.CreateForm(TTableForm, TableForm);
  Application.CreateForm(TEMF_Form, EMF_Form);
  Application.CreateForm(TPrintEpsOptForm, PrintEpsOptForm);
  Application.CreateForm(TScaleStandardForm, ScaleStandardForm);
  Application.CreateForm(TInfoBox, InfoBox);
  try
    Application.Run;
  finally
  end;
end.

