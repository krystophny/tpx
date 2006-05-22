program TpX;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {MainForm},
  Output in 'Output.pas',
  EMF in 'EMF.pas',
  EMF_Add in 'EMF_Add.pas',
  Propert in 'Propert.pas' {PropertiesForm},
  Settings in 'Settings.pas' {SettingsForm},
  Options0 in 'Options0.pas',
  CS4Shapes in 'CS4Shapes.pas',
  CS4Tasks in 'CS4Tasks.pas',
  CADSys4 in 'CADSys4.pas',
  Options in 'Options.pas' {OptionsForm},
  AboutUnit in 'AboutUnit.pas' {AboutForm},
  TransForm in 'TransForm.pas' {TransfForm},
  ColorEtc in 'ColorEtc.pas',
  PreView in 'Preview.pas',
  PdfFonts in 'Lib\PowerPdf\PdfFonts.pas',
  Stars in 'Stars.pas' {StarsFrame: TFrame},
  Table in 'Table.pas' {TableForm},
  EMF_Unit in 'EMF_Unit.pas' {EMF_Form},
  Geometry in 'Geometry.pas',
  PrintEpsOpt in 'PrintEpsOpt.pas' {PrintEpsOptForm},
  ScaleStandardUnit in 'ScaleStandardUnit.pas' {ScaleStandardForm},
  Draw in 'Draw.pas',
  Arrows in 'Arrows.pas' {ArrowsFrame: TFrame},
  Input in 'Input.pas',
  ClpbrdOp in 'ClpbrdOp.pas',
  WinBasic in 'WinBasic.pas',
  SysBasic in 'SysBasic.pas',
  Gr32Add in 'Gr32Add.pas',
  MiscUtils in 'MiscUtils.pas',
  PrimSAX in 'PrimSAX.pas';

{$R *.RES}

begin
  if not CheckCommandLine then Exit;
  Application.Initialize;
  Application.Title := 'TpX';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TPropertiesForm, PropertiesForm);
  Application.CreateForm(TSettingsForm, SettingsForm);
  Application.CreateForm(TOptionsForm, OptionsForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.CreateForm(TTransfForm, TransfForm);
  Application.CreateForm(TTableForm, TableForm);
  Application.CreateForm(TEMF_Form, EMF_Form);
  Application.CreateForm(TPrintEpsOptForm, PrintEpsOptForm);
  Application.CreateForm(TScaleStandardForm, ScaleStandardForm);
  Application.Run;
end.

