program TpX;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {MainForm},
  Output in 'Output.pas',
  MprtEMF in 'MprtEMF.pas',
  EMF_Add in 'EMF_Add.pas',
  Propert in 'Propert.pas' {PropertiesForm},
  Options0 in 'Options0.pas',
  GObjects in 'GObjects.pas',
  Drawings in 'Drawings.pas',
  Options in 'Options.pas' {OptionsForm},
  AboutUnit in 'AboutUnit.pas' {AboutForm},
  TransForm in 'TransForm.pas' {TransfForm},
  ColorEtc in 'ColorEtc.pas',
  PreView in 'Preview.pas',
  PdfFonts in 'Lib\PowerPdf\PdfFonts.pas',
  Table in 'Table.pas' {TableForm},
  EMF_Unit in 'EMF_Unit.pas' {EMF_Form},
  Geometry in 'Geometry.pas',
  PrintEpsOpt in 'PrintEpsOpt.pas' {PrintEpsOptForm},
  ScaleStandardUnit in 'ScaleStandardUnit.pas' {ScaleStandardForm},
  DevCanvas in 'DevCanvas.pas',
  Input in 'Input.pas',
  ClpbrdOp in 'ClpbrdOp.pas',
  WinBasic in 'WinBasic.pas',
  SysBasic in 'SysBasic.pas',
  Gr32Add in 'Gr32Add.pas',
  MiscUtils in 'MiscUtils.pas',
  PrimSAX in 'PrimSAX.pas',
  Numeric in 'Numeric.pas',
  Settings0 in 'Settings0.pas',
  Manage in 'Manage.pas',
  Modes in 'Modes.pas',
  ViewPort in 'ViewPort.pas',
  Pieces in 'Pieces.pas',
  Devices in 'Devices.pas',
  XmlOut in 'XmlOut.pas',
  DevSVG in 'DevSVG.pas',
  DevTikZ in 'DevTikZ.pas',
  DevPGF in 'DevPGF.pas',
  DevTeXPc in 'DevTeXPc.pas',
  TpXSaver in 'TpXSaver.pas',
  DevPS in 'DevPS.pas',
  DevPDF in 'DevPDF.pas',
  DevPSTr in 'DevPSTr.pas',
  DevMP in 'DevMP.pas',
  DevGr32 in 'DevGr32.pas',
  MprtSVG in 'MprtSVG.pas',
  Modify in 'Modify.pas',
  GObjBase in 'GObjBase.pas';

{$R *.RES}

begin
  Application.Initialize;
  if not CheckCommandLine then Exit;
  Application.Title := 'TpX';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TPropertiesForm, PropertiesForm);
  Application.CreateForm(TOptionsForm, OptionsForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.CreateForm(TTransfForm, TransfForm);
  Application.CreateForm(TTableForm, TableForm);
  Application.CreateForm(TEMF_Form, EMF_Form);
  Application.CreateForm(TPrintEpsOptForm, PrintEpsOptForm);
  Application.CreateForm(TScaleStandardForm, ScaleStandardForm);
  Application.Run;
end.

