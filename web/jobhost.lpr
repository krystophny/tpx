{ SPDX-License-Identifier: MIT }
program jobhost;
{$mode objfpc}
uses JS, WebAssembly, WasiEnv, JOB_Browser;
var Environment: TPas2JSWASIEnvironment;
    Bridge: TJSObjectBridge;
    JobImports, HostObject: TJSObject;
procedure Connect(Instance: TJSWebAssemblyInstance);
begin Environment.Instance := Instance; Environment.SetExports(TWASIExports(Instance.exports_)); end;
begin
  Environment := TPas2JSWASIEnvironment.Create;
  Bridge := TJSObjectBridge.Create(Environment);
  JobImports := TJSObject.New;
  Bridge.FillImportObject(JobImports);
  asm
    window.jobHost = {
      imports: $mod.JobImports,
      register: function(object) { return $mod.Bridge.RegisterLocalObject(object); },
      release: function(id) { return $mod.Bridge.ReleaseObject(id); },
      live: function() { return $mod.Bridge.FLocalObjects.filter(x => x != null).length; },
      slots: function() { return $mod.Bridge.FLocalObjects.length; }
    };
    $mod.HostObject = window.jobHost;
  end;
  HostObject['connect'] := @Connect;
end.
