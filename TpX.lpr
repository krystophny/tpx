program TpX;

{$MODE Delphi}

uses
  Forms, Interfaces,
  Settings0 in 'src\Settings0.pas', // Initialize settings first!
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
  PreView in 'src\Preview.pas',
  PdfFonts in 'src\lib\PowerPdf\PdfFonts.pas',
  Table in 'src\Table.pas' {TableForm},
  Geometry in 'src\Geometry.pas',
  PrintEpsOpt in 'src\PrintEpsOpt.pas' {PrintEpsOptForm},
  ScaleStandardUnit in 'src\ScaleStandardUnit.pas' {ScaleStandardForm},
  DevCanvas in 'src\DevCanvas.pas',
  Input in 'src\Input.pas',
  ClpbrdOp in 'src\ClpbrdOp.pas',
  {$IFDEF DELPHI}
  WinBasic in 'src\WinBasic.pas',
  EMF_Unit in 'src\EMF_Unit.pas' {EMF_Form},
  Gr32Add in 'src\Gr32Add.pas',   
  DevGr32 in 'src\DevGr32.pas',
  {$ELSE}
  SysBasic in 'src\SysBasic.pas',
  {$ENDIF}
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
  MprtSVG in 'src\MprtSVG.pas',
  Modify in 'src\Modify.pas',
  GObjBase in 'src\GObjBase.pas',
  Bitmaps in 'src\Bitmaps.pas',
  InfoForm in 'src\InfoForm.pas' {InfoBox};

{$R TpX.res}
{$R src\Options.lfm}
{$R src\AboutUnit.lfm}
{$IFDEF DELPHI}
  {$R src\EMF_Unit.lfm}
  {$R src\PrintEpsOpt.lfm}
  {$R src\Table.lfm}
  {$R src\InfoForm.lfm}
{$ENDIF}
{$R src\MainUnit.lfm}
{$R src\TransForm.lfm}
{$R src\Table.lfm}
{$R src\Propert.lfm}
{$R src\ScaleStandardUnit.lfm}


begin
  if not CheckCommandLine then Exit;
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TPropertiesForm, PropertiesForm);
  Application.CreateForm(TOptionsForm, OptionsForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.CreateForm(TTransfForm, TransfForm);
  Application.CreateForm(TTableForm, TableForm);  
  {$IFDEF DELPHI}
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

