program TpX;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainUnit in 'src\fmx\MainUnit.pas' {MainForm},
  Modify in 'src\Modify.pas',
  Geometry in 'src\Geometry.pas',
  MiscUtils in 'src\MiscUtils.pas',
  SysBasic in 'src\SysBasic.pas',
  Numeric in 'src\Numeric.pas',
  Drawings in 'src\Drawings.pas',
  GObjBase in 'src\GObjBase.pas',
  Devices in 'src\Devices.pas',
  Options0 in 'src\Options0.pas',
  Pieces in 'src\Pieces.pas',
  GObjects in 'src\GObjects.pas',
  ColorEtc in 'src\ColorEtc.pas',
  ViewPort in 'src\fmx\ViewPort.pas',
  Manage in 'src\Manage.pas',
  Input in 'src\Input.pas',
  Output in 'src\Output.pas',
  DevSVG in 'src\DevSVG.pas',
  DevTeXPc in 'src\DevTeXPc.pas',
  DevTikZ in 'src\DevTikZ.pas',
  DevPGF in 'src\DevPGF.pas',
  DevPS in 'src\DevPS.pas',
  TpXSaver in 'src\TpXSaver.pas',
  XmlOut in 'src\XmlOut.pas',
  Settings0 in 'src\Settings0.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
