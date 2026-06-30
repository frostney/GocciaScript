unit Goccia.RuntimeProfiles.Loader;

{$I Goccia.inc}

interface

uses
  Goccia.Runtime;

procedure ApplyLoaderRuntimeProfile(const ARuntime: TGocciaRuntimeCore);

implementation

uses
  Goccia.Builtins.Semver,
  Goccia.Constants.PropertyNames,
  Goccia.RuntimeExtensions.Console,
  Goccia.RuntimeExtensions.CSV,
  Goccia.RuntimeExtensions.Fetch,
  Goccia.RuntimeExtensions.JSON5,
  Goccia.RuntimeExtensions.JSONL,
  Goccia.RuntimeExtensions.Performance,
  Goccia.RuntimeExtensions.TextAssets,
  Goccia.RuntimeExtensions.TextEncoding,
  Goccia.RuntimeExtensions.TOML,
  Goccia.RuntimeExtensions.TSV,
  Goccia.RuntimeExtensions.URL,
  Goccia.RuntimeExtensions.YAML,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.Primitives;

// Plain factory wrapper so the SemVer namespace can be installed as a lazy
// property; CreateSemverNamespace returns TGocciaObjectValue, which is not
// procedural-type-compatible with TGocciaLazyPlainFactory directly.
function SemverNamespaceFactory: TGocciaValue;
begin
  Result := CreateSemverNamespace;
end;

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
  ARuntime.Install(TGocciaTextAssetsRuntimeExtension.Create);
  ARuntime.Install(TGocciaPerformanceRuntimeExtension.Create);
  ARuntime.Install(TGocciaTextEncodingRuntimeExtension.Create);
  ARuntime.Install(TGocciaURLRuntimeExtension.Create);
  ARuntime.Install(TGocciaFetchRuntimeExtension.Create);
  if Assigned(ARuntime.Engine.GocciaGlobal) then
    // Defer building the SemVer namespace until Goccia.semver is first touched.
    // Enumerable to match the eager data property it replaces.
    ARuntime.Engine.DefineLazyObjectProperty(ARuntime.Engine.GocciaGlobal,
      SEMVER_NAMESPACE_PROPERTY, SemverNamespaceFactory,
      [pfEnumerable, pfWritable, pfConfigurable]);
end;

end.
