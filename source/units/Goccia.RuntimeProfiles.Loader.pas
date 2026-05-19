unit Goccia.RuntimeProfiles.Loader;

{$I Goccia.inc}

interface

uses
  Goccia.Runtime;

type
  TGocciaLoaderRuntimeProfile = class(TGocciaRuntimeProfile)
  public
    class procedure Apply(const ARuntime: TGocciaRuntimeCore); override;
  end;

implementation

uses
  Goccia.RuntimeExtensions.Console,
  Goccia.RuntimeExtensions.CSV,
  Goccia.RuntimeExtensions.Fetch,
  Goccia.RuntimeExtensions.HostGlobals,
  Goccia.RuntimeExtensions.JSON5,
  Goccia.RuntimeExtensions.JSONL,
  Goccia.RuntimeExtensions.Performance,
  Goccia.RuntimeExtensions.SemVer,
  Goccia.RuntimeExtensions.TextAssets,
  Goccia.RuntimeExtensions.TextEncoding,
  Goccia.RuntimeExtensions.TOML,
  Goccia.RuntimeExtensions.TSV,
  Goccia.RuntimeExtensions.URL,
  Goccia.RuntimeExtensions.YAML;

class procedure TGocciaLoaderRuntimeProfile.Apply(
  const ARuntime: TGocciaRuntimeCore);
begin
  ARuntime.Install(TGocciaConsoleRuntimeExtension.Create);
  ARuntime.Install(TGocciaHostGlobalsRuntimeExtension.Create);
  ARuntime.Install(TGocciaCSVRuntimeExtension.Create);
  ARuntime.Install(TGocciaJSON5RuntimeExtension.Create);
  ARuntime.Install(TGocciaJSONLRuntimeExtension.Create);
  ARuntime.Install(TGocciaTOMLRuntimeExtension.Create);
  ARuntime.Install(TGocciaTSVRuntimeExtension.Create);
  ARuntime.Install(TGocciaYAMLRuntimeExtension.Create);
  ARuntime.Install(TGocciaTextAssetsRuntimeExtension.Create);
  ARuntime.Install(TGocciaPerformanceRuntimeExtension.Create);
  ARuntime.Install(TGocciaTextEncodingRuntimeExtension.Create);
  ARuntime.Install(TGocciaURLRuntimeExtension.Create);
  ARuntime.Install(TGocciaFetchRuntimeExtension.Create);
  ARuntime.Install(TGocciaSemVerRuntimeExtension.Create);
end;

end.
