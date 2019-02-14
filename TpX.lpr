program TpX;

{$MODE Delphi}

uses
  Forms, Interfaces,
  MainUnit in 'MainUnit.pas' {MainForm},
  Output in 'Output.pas',
  EMF_Add in 'EMF_Add.pas',
  Settings0 in 'Settings0.pas',
  Options0 in 'Options0.pas',
  AboutUnit in 'AboutUnit.pas' {AboutForm},
  TransForm in 'TransForm.pas' {TransfForm},
  ColorEtc in 'ColorEtc.pas',
  PreView in 'Preview.pas',
  Table in 'Table.pas' {TableForm},
  Geometry in 'Geometry.pas',
  ScaleStandardUnit in 'ScaleStandardUnit.pas' {ScaleStandardForm},
  Input in 'Input.pas',
  ClpbrdOp in 'ClpbrdOp.pas',
  SysBasic in 'SysBasic.pas',
  MiscUtils in 'MiscUtils.pas',
  PrimSAX in 'PrimSAX.pas', LazBasic, Propert, Options, ViewPort, Drawings,
  GObjects, Manage, Modes, Devices, DevSVG, DevTikZ, Numeric,
  Pieces, XmlOut, DevCanvas, DevMP, DevPDF, DevPGF, DevPS, DevPSTr,
  DevTeXPc, Modify, MprtEMF, MprtSVG, TpXSaver;

{R *.RES}

begin
  Application.Initialize;
  if not CheckCommandLine then Exit;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TOptionsForm, OptionsForm);
  Application.CreateForm(TPropertiesForm, PropertiesForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.CreateForm(TTransfForm, TransfForm);
  Application.CreateForm(TTableForm, TableForm);
  Application.CreateForm(TScaleStandardForm, ScaleStandardForm);
  Application.Run;
end.

