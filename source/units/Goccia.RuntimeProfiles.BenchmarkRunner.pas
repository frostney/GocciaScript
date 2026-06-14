unit Goccia.RuntimeProfiles.BenchmarkRunner;

{$I Goccia.inc}

interface

uses
  Goccia.Runtime;

procedure ApplyBenchmarkRunnerRuntimeProfile(const ARuntime: TGocciaRuntimeCore);

implementation

uses
  Goccia.RuntimeExtensions.Benchmark,
  Goccia.RuntimeProfiles.Loader;

procedure ApplyBenchmarkRunnerRuntimeProfile(const ARuntime: TGocciaRuntimeCore);
begin
  ApplyLoaderRuntimeProfile(ARuntime);
  ARuntime.Install(TGocciaBenchmarkRuntimeExtension.Create);
end;

end.
