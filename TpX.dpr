program TpX;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {MainForm},
  InOut in 'InOut.pas',
  EMF in 'EMF.pas',
  EMF_Add in 'EMF_Add.pas',
  Propert in 'Propert.pas' {PropertiesForm},
  Settings in 'Settings.pas' {SettingsForm},
  Options0 in 'Options0.pas',
  CS4BaseTypes in 'CS4BaseTypes.pas',
  CS4Shapes in 'CS4Shapes.pas',
  CS4Tasks in 'CS4Tasks.pas',
  CADSys4 in 'CADSys4.pas',
  Options in 'Options.pas' {OptionsForm},
  AboutUnit in 'AboutUnit.pas' {AboutForm},
  TransForm in 'TransForm.pas' {TransfForm},
  ColorEtc in 'ColorEtc.pas',
  PreView in 'Preview.pas',
  PdfFonts in 'Lib\PowerPdf\PdfFonts.pas',
  Stars in 'Stars.pas' {StarsFrame: TFrame};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'TpX';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TPropertiesForm, PropertiesForm);
  Application.CreateForm(TSettingsForm, SettingsForm);
  Application.CreateForm(TOptionsForm, OptionsForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.CreateForm(TTransfForm, TransfForm);
  Application.Run;
end.

