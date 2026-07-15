unit Goccia.RuntimeProfiles.Loader;

{$I Goccia.inc}

interface

uses
  Goccia.Runtime;

procedure ApplyLoaderRuntimeProfile(const ARuntime: TGocciaRuntimeCore);

implementation

uses
  Goccia.RuntimeExtensions.Console,
  Goccia.RuntimeExtensions.CSV,
  Goccia.RuntimeExtensions.Fetch,
  Goccia.RuntimeExtensions.JSON5,
  Goccia.RuntimeExtensions.JSONL,
  Goccia.RuntimeExtensions.Performance,
  Goccia.RuntimeExtensions.Semver,
  Goccia.RuntimeExtensions.TextAssets,
  Goccia.RuntimeExtensions.TextEncoding,
  Goccia.RuntimeExtensions.TOML,
  Goccia.RuntimeExtensions.TSV,
  Goccia.RuntimeExtensions.URL,
  Goccia.RuntimeExtensions.YAML;

procedure ApplyLoaderRuntimeProfile(const ARuntime: TGocciaRuntimeCore);
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
end;

end.
