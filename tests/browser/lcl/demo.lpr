{ SPDX-License-Identifier: MIT
  Entry point of the CustomDrawn WASI regression probe (see browser.py). }
{$ifdef CPUWASM32}library demo;{$else}program demo;{$endif}
{$mode objfpc}{$H+}
uses Interfaces, Forms, DemoMain;
begin
  Application.Initialize;
  Application.CreateForm(TDemoForm, DemoForm);
  DemoForm.Show;
  Application.Run;
end.
