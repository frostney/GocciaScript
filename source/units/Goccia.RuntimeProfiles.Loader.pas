unit Goccia.RuntimeProfiles.Loader;

{$I Goccia.inc}

interface

uses
  Goccia.Runtime;

{ ATestingModule registers the `goccia:test` module namespace without
  injecting any testing global. Every host that attaches this profile gets the
  importable API; only GocciaTestRunner adds the globals on top, and it passes
  False here so its own TGocciaTestingLibraryRuntimeExtension owns the single
  `goccia:test` registration. }
procedure ApplyLoaderRuntimeProfile(const ARuntime: TGocciaRuntimeCore;
  const ATestingModule: Boolean = True);

implementation

uses
  Goccia.RuntimeExtensions.AsyncHooks,
  Goccia.RuntimeExtensions.Console,
  Goccia.RuntimeExtensions.CSV,
  Goccia.RuntimeExtensions.Fetch,
  Goccia.RuntimeExtensions.JSON5,
  Goccia.RuntimeExtensions.JSONL,
  Goccia.RuntimeExtensions.Performance,
  Goccia.RuntimeExtensions.Semver,
  Goccia.RuntimeExtensions.TestingLibrary,
  Goccia.RuntimeExtensions.TextAssets,
  Goccia.RuntimeExtensions.TextEncoding,
  Goccia.RuntimeExtensions.TOML,
  Goccia.RuntimeExtensions.TSV,
  Goccia.RuntimeExtensions.URL,
  Goccia.RuntimeExtensions.YAML;

procedure ApplyLoaderRuntimeProfile(const ARuntime: TGocciaRuntimeCore;
  const ATestingModule: Boolean);
begin
  ARuntime.Install(TGocciaConsoleRuntimeExtension.Create);
  ARuntime.Engine.BuiltinGlobals.RegisterUtilityRuntimeGlobals;
  ARuntime.Install(TGocciaCSVRuntimeExtension.Create);
  ARuntime.Install(TGocciaJSON5RuntimeExtension.Create);
  ARuntime.Install(TGocciaJSONLRuntimeExtension.Create);
  ARuntime.Install(TGocciaTOMLRuntimeExtension.Create);
  ARuntime.Install(TGocciaTSVRuntimeExtension.Create);
  ARuntime.Install(TGocciaYAMLRuntimeExtension.Create);
  ARuntime.Install(TGocciaSemverRuntimeExtension.Create);
  ARuntime.Install(TGocciaTextAssetsRuntimeExtension.Create);
  ARuntime.Install(TGocciaPerformanceRuntimeExtension.Create);
  ARuntime.Install(TGocciaTextEncodingRuntimeExtension.Create);
  ARuntime.Install(TGocciaURLRuntimeExtension.Create);
  ARuntime.Install(TGocciaFetchRuntimeExtension.Create);
  { Pure context bookkeeping — no I/O, no clock, no way to observe anything
    the running program did not already have. It carries no capability, so it
    is on by default wherever the loader profile is. }
  ARuntime.Install(TGocciaAsyncHooksRuntimeExtension.Create);
  if ATestingModule then
    ARuntime.Install(TGocciaTestingLibraryRuntimeExtension.CreateModuleOnly);
end;

end.
