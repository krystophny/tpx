program TpX;

{$MODE Delphi}

uses
  Forms, Interfaces,
  Settings0 in 'Settings0.pas', // Initialize settings first!
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
  Geometry in 'Geometry.pas',
  PrintEpsOpt in 'PrintEpsOpt.pas' {PrintEpsOptForm},
  ScaleStandardUnit in 'ScaleStandardUnit.pas' {ScaleStandardForm},
  DevCanvas in 'DevCanvas.pas',
  Input in 'Input.pas',
  ClpbrdOp in 'ClpbrdOp.pas',
  {$IFDEF WINDOWS}            
  EMF_Unit in 'EMF_Unit.pas' {EMF_Form},
  WinBasic in 'WinBasic.pas',
  Gr32Add in 'Gr32Add.pas',   
  DevGr32 in 'DevGr32.pas',
  {$ELSE}
  SysBasic in 'SysBasic.pas',
  {$ENDIF}
  MiscUtils in 'MiscUtils.pas',
  PrimSAX in 'PrimSAX.pas',
  Numeric in 'Numeric.pas',
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
  MprtSVG in 'MprtSVG.pas',
  Modify in 'Modify.pas',
  GObjBase in 'GObjBase.pas',
  Bitmaps in 'Bitmaps.pas',
  InfoForm in 'InfoForm.pas' {InfoBox};

{$R *.res}
{$R Options.lfm} 
{$R AboutUnit.lfm}  
{$IFDEF WINDOWS}
  {$R EMF_Unit.lfm}  
  {$R PrintEpsOpt.lfm}  
  {$R Table.lfm}  
  {$R InfoForm.lfm}
{$ENDIF}
{$R MainUnit.lfm}
{$R TransForm.lfm}
{$R Table.lfm}
{$R Propert.lfm}
{$R ScaleStandardUnit.lfm}


begin
  if not CheckCommandLine then Exit;
  Application.Title := 'TpX';          
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TPropertiesForm, PropertiesForm);
  Application.CreateForm(TOptionsForm, OptionsForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.CreateForm(TTransfForm, TransfForm);
  Application.CreateForm(TTableForm, TableForm);  
  {$IFDEF WINDOWS}
  Application.CreateForm(TEMF_Form, EMF_Form);
  {$ENDIF}
  Application.CreateForm(TPrintEpsOptForm, PrintEpsOptForm);
  Application.CreateForm(TScaleStandardForm, ScaleStandardForm);
  Application.CreateForm(TInfoBox, InfoBox);
  try
    Application.Run;
  finally
  end;
end.

